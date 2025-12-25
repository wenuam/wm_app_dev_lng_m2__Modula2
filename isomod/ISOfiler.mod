(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE ISOfiler;
(* This is not an ISO defined module *)
(* It is a ADW Software helper module *)

<*/NOPACK*>

(* provides the functions that the ISO file modules need, common code. *)
(* StreamFile, SeqFile and RndFile use this for their core functions. *)
(* a layer between ISO and FileFunc *)

<*/VALIDVER:PROTECT*>
(*<*/VERSION:PROTECT*>*)
(* procedures in this module will protect from multiple access in *)
(* a multi threaded environment *)
(* this is generally not very useful, since two successive calls can *)
(* be interrupted in the middle, thus corrupting the output. *)
(* call sequences usually are protected by user code *)

<*/VALIDVER:FetchHandle*>
(*<*/VERSION:FetchHandle*>*)
(* Using this version tag will allow this system to function *)
(* if the user changes their own standard handles for redirection *)
(* by using operating system calls during program operation, instead *)
(* of the ISO defined way of redirection *)
(* Only WIN32 allows the values of the standard handles to change. *)

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADDADR, CAST, ADRCARD;

FROM IOLink IMPORT
    DeviceTablePtr, RAISEdevException;

FROM IOConsts IMPORT
    ReadResults;

FROM IOChan IMPORT
    ChanId, ChanExceptions;

FROM ChanConsts IMPORT
    OpenResults, FlagSet, text, raw, read, write;

FROM Strings IMPORT
    Assign;

FROM ExStorage IMPORT
    AllocateEx, DeallocateEx, GetDefaultHeap;

FROM FileFunc IMPORT
    File, FileSpecString, OpenFile, CreateFile,
    CloseFile, TruncateFile,
    AccessModes, EOL, CommonFileErrors,
    FakeFileOpen, SetFilePos, GetFilePos, MoveFilePos, FileLength,
    SetFileBuffer, RemoveFileBuffer, FlushBuffers,
    ReadChar, WriteChar, ReadBlock, WriteBlock,
    TranslateFileError;
IMPORT FileFunc;

FROM StdHandles IMPORT
    IsConsole, StdInputHandle, StdOutputHandle, StdErrorHandle;

%IF PROTECT %THEN

FROM Threads IMPORT
    CriticalSection, CreateCriticalSection, CloseCriticalSection,
    EnterCriticalSection, LeaveCriticalSection;

%END

CONST
    DefaultBufferSize   = 4 * 1024;

TYPE
    FileInfoPointer     = POINTER TO FileInfo;
    FileInfo =
        RECORD
        buffer          : POINTER TO ARRAY [0..0] OF BYTE;
        bufSize         : CARDINAL;
        len             : CARDINAL;
        %IF PROTECT %THEN
        critic          : CriticalSection;
        %END
        f               : File;
        typ             : FileTypes;
        lookCh          : CHAR;
        looked          : BOOLEAN;
        canPosition     : BOOLEAN;
        name            : FileSpecString; (* short allocated *)
        END;

PROCEDURE ALLOCATE(VAR OUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    AllocateEx(addr, amount, GetDefaultHeap());
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    DeallocateEx(addr, amount, GetDefaultHeap());
END DEALLOCATE;

PROCEDURE FetchHandle(f : FileInfoPointer);
BEGIN
    CASE f^.typ OF
    FileStdIn:
        f^.f.handle := StdInputHandle();
    |
    FileStdOut:
        f^.f.handle := StdOutputHandle();
    |
    FileStdError:
        f^.f.handle := StdErrorHandle();
    ELSE
    END;
END FetchHandle;

PROCEDURE HandleDiskError(errorNum : CARDINAL; p : DeviceTablePtr);
BEGIN
    p^.errNum := errorNum;

    %IF PROTECT %THEN
        LeaveCriticalSection(f^.critic);
    %END

    RAISEdevException(p^.cid, p^.did, hardDeviceError, "HARD-DEVICE-ERROR");
END HandleDiskError;

PROCEDURE BadReadPermission(p : DeviceTablePtr);
BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      notAvailable,
                      "Read service not available on this channel");
END BadReadPermission;

PROCEDURE BadWritePermission(p : DeviceTablePtr);
BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      notAvailable,
                      "Write service not available on this channel");
END BadWritePermission;

PROCEDURE BadSkip(p : DeviceTablePtr);
BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      skipAtEnd,
                      "Skip attempted at end of Input");
END BadSkip;

PROCEDURE FetchChar(p : DeviceTablePtr) : CHAR;
VAR
    f   : FileInfoPointer;
    res : ReadResults;
    ch  : CHAR;
BEGIN
    f := p^.cd;
    IF NOT f^.looked THEN
        f^.lookCh := ReadChar(f^.f);
        IF f^.f.status = 0 THEN
            f^.looked := TRUE;
        ELSE
            IF TranslateFileError(f^.f) <> FileErrBrokenPipe THEN
                HandleDiskError(f^.f.status, p);
            ELSE
                (* we will let a broken pipe read signify endOfInput *)
                ch := '';
            END;
        END;
    END;
    ch := f^.lookCh;

    IF ch = EOL THEN
        res := endOfLine;
    ELSIF (ch = '') OR (f^.f.eof) THEN
        res := endOfInput
    ELSE
        res := allRight;
    END;
    p^.result := res;

    RETURN ch;
END FetchChar;

PROCEDURE Look(p : DeviceTablePtr; VAR ch : CHAR; VAR res : ReadResults);
VAR
    f   : FileInfoPointer;
BEGIN
    IF read <= p^.flags THEN
        f := p^.cd;
        %IF PROTECT %THEN
            EnterCriticalSection(f^.critic);
        %END

        %IF FetchHandle %THEN
            FetchHandle(f);
        %END
        ch := FetchChar(p);
        res := p^.result;

        %IF PROTECT %THEN
            LeaveCriticalSection(f^.critic);
        %END
    ELSE
        BadReadPermission(p);
    END;
END Look;

PROCEDURE EatChar(p : DeviceTablePtr);
VAR
    f   : FileInfoPointer;
    ch  : CHAR;
BEGIN
    p^.result := allRight;

    f := p^.cd;
    IF f^.looked THEN
        f^.looked := FALSE;
    ELSE
        ch := ReadChar(f^.f);
        IF f^.f.status <> 0 THEN
            IF TranslateFileError(f^.f) <> FileErrBrokenPipe THEN
                HandleDiskError(f^.f.status, p);
            ELSE
                p^.result := endOfInput;
            END;
        ELSIF (ch = '') OR (f^.f.eof) THEN
            p^.result := endOfInput;
        END;
    END;
END EatChar;

PROCEDURE Skip(p : DeviceTablePtr);
VAR
    f   : FileInfoPointer;
BEGIN
    IF read <= p^.flags THEN
        f := p^.cd;
        %IF PROTECT %THEN
            EnterCriticalSection(f^.critic);
        %END

        %IF FetchHandle %THEN
            FetchHandle(f);
        %END
        EatChar(p);

        %IF PROTECT %THEN
            LeaveCriticalSection(f^.critic);
        %END

        IF p^.result = endOfInput THEN
            BadSkip(p);
        END;
    ELSE
        BadReadPermission(p);
    END;
END Skip;

PROCEDURE SkipLook(p : DeviceTablePtr; VAR ch : CHAR; VAR res : ReadResults);
VAR
    f   : FileInfoPointer;
BEGIN
    IF read <= p^.flags THEN
        f := p^.cd;
        %IF PROTECT %THEN
            EnterCriticalSection(f^.critic);
        %END

        %IF FetchHandle %THEN
            FetchHandle(f);
        %END

        ch := '';
        EatChar(p);
        res := p^.result;
        IF res = endOfInput THEN
            %IF PROTECT %THEN
                LeaveCriticalSection(f^.critic);
            %END
            BadSkip(p);
        END;

        ch := FetchChar(p);
        res := p^.result;
    ELSE
        BadReadPermission(p);
    END;
END SkipLook;

PROCEDURE LnWrite(p : DeviceTablePtr);
VAR
    f   : FileInfoPointer;
BEGIN
    IF write <= p^.flags THEN
        f := p^.cd;
        %IF PROTECT %THEN
            EnterCriticalSection(f^.critic);
        %END

        %IF FetchHandle %THEN
            FetchHandle(f);
        %END
        WriteChar(f^.f, EOL);
        IF f^.f.status <> 0 THEN
            HandleDiskError(f^.f.status, p);
        END;

        f^.looked := FALSE;

        %IF PROTECT %THEN
            LeaveCriticalSection(f^.critic);
        %END
    ELSE
        BadWritePermission(p);
    END;
END LnWrite;

PROCEDURE TextRead(p : DeviceTablePtr;
                   addr : ADDRESS;
                   c : CARDINAL;
                   VAR d : CARDINAL);
VAR
    ch  : CHAR;
    ptr : POINTER TO CHAR;
    f   : FileInfoPointer;
BEGIN
    IF read <= p^.flags THEN
        d := 0;
        p^.result := allRight;
        IF c <> 0 THEN
            f := p^.cd;
            %IF PROTECT %THEN
                EnterCriticalSection(f^.critic);
            %END

            %IF FetchHandle %THEN
                FetchHandle(f);
            %END

            ptr := addr;
            ch := FetchChar(p);
            WHILE (c > 0) AND (p^.result = allRight) DO
                DEC(c);
                INC(d);
                ptr^ := ch;
                ptr := ADDADR(ptr, SIZE(CHAR));

                EatChar(p);
                IF p^.result = endOfInput THEN
                    %IF PROTECT %THEN
                        LeaveCriticalSection(f^.critic);
                    %END
                    BadSkip(p);
                END;

                IF c > 0 THEN
                    ch := FetchChar(p);
                END;
            END;

            (* if we got something, then allRight *)

            IF d > 0 THEN
                p^.result := allRight;
            END;

            %IF PROTECT %THEN
                LeaveCriticalSection(f^.critic);
            %END
        END;
    ELSE
        BadReadPermission(p);
    END;
END TextRead;

PROCEDURE TextWrite(p : DeviceTablePtr; addr : ADDRESS; c : CARDINAL);
VAR
    f   : FileInfoPointer;
BEGIN
    IF write <= p^.flags THEN
        f := p^.cd;
        %IF PROTECT %THEN
            EnterCriticalSection(f^.critic);
        %END

        %IF FetchHandle %THEN
            FetchHandle(f);
        %END
        WriteBlock(f^.f, addr, c * SIZE(CHAR));
        IF f^.f.status <> 0 THEN
            HandleDiskError(f^.f.status, p);
        END;

        f^.looked := FALSE;

        %IF PROTECT %THEN
            LeaveCriticalSection(f^.critic);
        %END
    ELSE
        BadWritePermission(p);
    END;
END TextWrite;

PROCEDURE RawRead(p : DeviceTablePtr;
                  addr : ADDRESS;
                  c : CARDINAL;
                  VAR d : CARDINAL);
VAR
    f           : FileInfoPointer;
BEGIN
    IF read <= p^.flags THEN
        d := 0;
        p^.result := allRight;
        IF c <> 0 THEN
            f := p^.cd;
            %IF PROTECT %THEN
                EnterCriticalSection(f^.critic);
            %END

            %IF FetchHandle %THEN
                FetchHandle(f);
            %END

            IF f^.looked THEN
                f^.looked := FALSE;

                (* if SIZE(CHAR) > 1 the raw read can be 1 *)

                IF f^.canPosition THEN
                    MoveFilePos(f^.f, -SIZE(CHAR));
                ELSE
                    IF c < SIZE(CHAR) THEN
                        (* we cannot really handle this so generate an error *)
                        (* generate a disk error by calling move *)

                        MoveFilePos(f^.f, -SIZE(CHAR));
                        IF f^.f.status <> 0 THEN
                            HandleDiskError(f^.f.status, p);
                        END;
                        (* the OS said it did the move, who am I to argue *)
                    ELSE
                        addr^:CHAR := f^.lookCh;
                        addr := ADDADR(addr, SIZE(CHAR));
                        d := SIZE(CHAR);
                        c := c - SIZE(CHAR);
                    END;
                END;
            END;

            IF c <> 0 THEN
                ReadBlock(f^.f, addr, c);
                d := d + f^.f.count;
            END;

            IF f^.f.status <> 0 THEN
                IF TranslateFileError(f^.f) <> FileErrBrokenPipe THEN
                    HandleDiskError(f^.f.status, p);
                ELSE
                    p^.result := endOfInput;
                END;
            ELSIF d = c THEN
                p^.result := allRight;
            ELSIF d <> 0 THEN
                p^.result := wrongFormat;
            ELSE
                p^.result := endOfInput;
            END;

            %IF PROTECT %THEN
                LeaveCriticalSection(f^.critic);
            %END
        END;
    ELSE
        BadReadPermission(p);
    END;
END RawRead;

PROCEDURE RawWrite(p : DeviceTablePtr; addr : ADDRESS; c : CARDINAL);
VAR
    f   : FileInfoPointer;
BEGIN
    IF write <= p^.flags THEN
        f := p^.cd;
        %IF PROTECT %THEN
            EnterCriticalSection(f^.critic);
        %END

        %IF FetchHandle %THEN
            FetchHandle(f);
        %END
        WriteBlock(f^.f, addr, c);
        IF f^.f.status <> 0 THEN
            HandleDiskError(f^.f.status, p);
        END;

        f^.looked := FALSE;

        %IF PROTECT %THEN
            LeaveCriticalSection(f^.critic);
        %END
    ELSE
        BadWritePermission(p);
    END;
END RawWrite;

PROCEDURE GetName(p : DeviceTablePtr; VAR str : ARRAY OF CHAR);
VAR
    f   : FileInfoPointer;
BEGIN
    f := p^.cd;
    %IF PROTECT %THEN
        EnterCriticalSection(f^.critic);
    %END

    Assign(f^.name, str);

    %IF PROTECT %THEN
        LeaveCriticalSection(f^.critic);
    %END
END GetName;

PROCEDURE Flush(p : DeviceTablePtr);
VAR
    f   : FileInfoPointer;
BEGIN
    f := p^.cd;
    %IF PROTECT %THEN
        EnterCriticalSection(f^.critic);
    %END

    IF f^.buffer <> NIL THEN
        %IF FetchHandle %THEN
            FetchHandle(f);
        %END

        FlushBuffers(f^.f, FALSE);
        IF f^.f.status <> 0 THEN
            HandleDiskError(f^.f.status, p);
        END;
    END;

    f^.looked := FALSE;

    %IF PROTECT %THEN
        LeaveCriticalSection(f^.critic);
    %END
END Flush;

PROCEDURE Reset(p : DeviceTablePtr);
VAR
    f   : FileInfoPointer;
BEGIN
    f := p^.cd;
    %IF PROTECT %THEN
        EnterCriticalSection(f^.critic);
    %END

    %IF FetchHandle %THEN
        FetchHandle(f);
    %END

    IF f^.buffer <> NIL THEN
        FlushBuffers(f^.f, FALSE);
        IF f^.f.status <> 0 THEN
            HandleDiskError(f^.f.status, p);
        END;
    END;

    SetFilePos(f^.f, 0);
    IF f^.f.status <> 0 THEN
        HandleDiskError(f^.f.status, p);
    END;

    f^.looked := FALSE;

    %IF PROTECT %THEN
        LeaveCriticalSection(f^.critic);
    %END
END Reset;

PROCEDURE Free(p : DeviceTablePtr);
VAR
    f   : FileInfoPointer;
BEGIN
    f := p^.cd;
    %IF PROTECT %THEN
        EnterCriticalSection(f^.critic);
    %END

    %IF FetchHandle %THEN
        FetchHandle(f);
    %END

    IF f^.buffer <> NIL THEN
        FlushBuffers(f^.f, FALSE);
        IF f^.f.status <> 0 THEN
            HandleDiskError(f^.f.status, p);
        END;
    END;

    CloseFile(f^.f);
    IF f^.f.status <> 0 THEN
        HandleDiskError(f^.f.status, p);
    END;

    IF f^.buffer <> NIL THEN
        DEALLOCATE(f^.buffer, f^.bufSize);
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f^.critic);
        CloseCriticalSection(f^.critic);
    %END

    DEALLOCATE(p^.cd, SIZE(f^) - SIZE(f^.name) + f^.len + 1);
    p^.cd := NIL;
END Free;

PROCEDURE FileErrToOpenRes(f : File) : OpenResults;
BEGIN
    CASE TranslateFileError(f) OF
    FileErrSuccess:
        RETURN opened;
    |
    FileErrFileNotFound, FileErrPathNotFound:
        RETURN noSuchFile;
    |
    FileErrNoHandles:
        RETURN tooManyOpen;
    |
    FileErrAccessDenied, FileErrSharingOrLock:
        RETURN wrongPermissions;
    ELSE
        RETURN otherProblem;
    END;
END FileErrToOpenRes;

PROCEDURE Open(p : DeviceTablePtr;
               name : ARRAY OF CHAR;
               readWrite : BOOLEAN) : OpenResults;
VAR
    mode        : AccessModes;
    f           : FileInfoPointer;
    file        : File;
    l           : CARDINAL;
    res         : OpenResults;
BEGIN
    mode := ReadOnlyDenyWrite;
    IF readWrite THEN
        mode := ReadWriteDenyAll;
    END;

    OpenFile(file, name, mode);
    res := FileErrToOpenRes(file);
    IF res = opened THEN
        l := LENGTH(name);
        ALLOCATE(f, SIZE(f^) - SIZE(f^.name) + l + 1);
        Assign(name, f^.name);
        f^.len := l;
        f^.looked := FALSE;
        f^.buffer := NIL;
        f^.f := file;
        f^.typ := FileNormal;

        f^.canPosition := FileFunc.FileType(file) = FileFunc.FileTypeDisk;

        p^.cd := f;

        RETURN res;
    END;
    RETURN res;
END Open;

PROCEDURE FakeOpen(p : DeviceTablePtr; typ : FileTypes);
VAR
    f           : FileInfoPointer;
    l           : CARDINAL;
    name        : ARRAY [0..31] OF CHAR;
    mode        : AccessModes;
BEGIN
    CASE typ OF
    FileStdIn:
        name := "$$StdIn";
        mode := ReadOnlyDenyWrite;
    |
    FileStdOut:
        name := "$$StdOut";
        mode := WriteOnlyDenyAll;
    |
    FileStdError:
        name := "$$StdError";
        mode := WriteOnlyDenyAll;
    ELSE
        p^.cd := NIL;
        RETURN;
    END;

    l := LENGTH(name);
    ALLOCATE(f, SIZE(f^) - SIZE(f^.name) + l + 1);
    Assign(name, f^.name);
    f^.len := l;
    f^.looked := FALSE;
    f^.buffer := NIL;

    f^.typ := typ;
    f^.canPosition := FALSE;
    FetchHandle(f);

    FakeFileOpen(f^.f, f^.f.handle, mode);

    p^.cd := f;
END FakeOpen;

PROCEDURE Create(p : DeviceTablePtr; name : ARRAY OF CHAR) : OpenResults;
VAR
    f           : FileInfoPointer;
    file        : File;
    l           : CARDINAL;
    res         : OpenResults;
BEGIN
    CreateFile(file, name);
    res := FileErrToOpenRes(file);
    IF res = opened THEN
        l := LENGTH(name);
        ALLOCATE(f, SIZE(f^) - SIZE(f^.name) + l + 1);
        Assign(name, f^.name);
        f^.len := l;
        f^.looked := FALSE;
        f^.buffer := NIL;
        f^.f := file;
        f^.typ := FileNormal;

        f^.canPosition := FileFunc.FileType(file) = FileFunc.FileTypeDisk;

        p^.cd := f;
        RETURN res;
    END;
    RETURN res;
END Create;

PROCEDURE Close(p : DeviceTablePtr);
VAR
    f   : FileInfoPointer;
BEGIN
    f := p^.cd;

    %IF FetchHandle %THEN
        FetchHandle(f);
    %END

    IF f^.buffer <> NIL THEN
        FlushBuffers(f^.f, FALSE);
        IF f^.f.status <> 0 THEN
            HandleDiskError(f^.f.status, p);
        END;
    END;

    CloseFile(f^.f);

    IF f^.buffer <> NIL THEN
        DEALLOCATE(f^.buffer, f^.bufSize);
    END;

    DEALLOCATE(p^.cd, SIZE(f^) - SIZE(f^.name) + f^.len + 1);
    p^.cd := NIL;
END Close;

PROCEDURE AssignProcs(p : DeviceTablePtr;
                      buffer : BOOLEAN;
                      reset : BOOLEAN;
                      flags : FlagSet);
VAR
    f   : FileInfoPointer;
BEGIN
    %IF PROTECT %THEN
        CreateCriticalSection(f^.critic);
    %END

    p^.doFlush := Flush;
    p^.doFree := Free;
    p^.doGetName := GetName;
    p^.doLnWrite := LnWrite;

    p^.doLook := Look;
    p^.doSkip := Skip;
    p^.doSkipLook := SkipLook;

    IF reset THEN
        p^.doReset := Reset;
    END;

    IF text <= flags THEN
        p^.doTextWrite := TextWrite;
        p^.doTextRead := TextRead;
    END;

    IF raw <= flags THEN
        p^.doRawWrite := RawWrite;
        p^.doRawRead := RawRead;
    END;

    IF buffer THEN
        f := p^.cd;
        %IF FetchHandle %THEN
            FetchHandle(f);
        %END
        SetDeviceBuffer(p^.cid, DefaultBufferSize);
    END;
END AssignProcs;

PROCEDURE Truncate(p : DeviceTablePtr);
VAR
    f   : FileInfoPointer;
BEGIN
    f := p^.cd;
    %IF PROTECT %THEN
        EnterCriticalSection(f^.critic);
    %END

    %IF FetchHandle %THEN
        FetchHandle(f);
    %END
    TruncateFile(f^.f);
    IF f^.f.status <> 0 THEN
        HandleDiskError(f^.f.status, p);
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f^.critic);
    %END
END Truncate;

PROCEDURE SetPos(p : DeviceTablePtr; pos : CARDINAL32);
VAR
    f   : FileInfoPointer;
BEGIN
    f := p^.cd;
    %IF PROTECT %THEN
        EnterCriticalSection(f^.critic);
    %END

    f^.looked := FALSE;

    %IF FetchHandle %THEN
        FetchHandle(f);
    %END
    SetFilePos(f^.f, pos);
    IF f^.f.status <> 0 THEN
        HandleDiskError(f^.f.status, p);
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f^.critic);
    %END
END SetPos;

PROCEDURE GetPos(p : DeviceTablePtr) : CARDINAL32;
VAR
    f   : FileInfoPointer;
    pos : CARDINAL32;
BEGIN
    f := p^.cd;
    %IF PROTECT %THEN
        EnterCriticalSection(f^.critic);
    %END

    %IF FetchHandle %THEN
        FetchHandle(f);
    %END
    pos := GetFilePos(f^.f);
    IF f^.f.status <> 0 THEN
        HandleDiskError(f^.f.status, p);
    END;

    IF f^.looked THEN
        pos := pos - SIZE(CHAR);
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f^.critic);
    %END

    RETURN pos;
END GetPos;

PROCEDURE EndPos(p : DeviceTablePtr) : CARDINAL32;
VAR
    f   : FileInfoPointer;
    pos : CARDINAL32;
BEGIN
    f := p^.cd;
    %IF PROTECT %THEN
        EnterCriticalSection(f^.critic);
    %END

    %IF FetchHandle %THEN
        FetchHandle(f);
    %END
    pos := FileLength(f^.f);
    IF f^.f.status <> 0 THEN
        HandleDiskError(f^.f.status, p);
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f^.critic);
    %END
    RETURN pos;
END EndPos;

PROCEDURE SetDeviceBuffer(cid : ChanId; amount : CARDINAL);
VAR
    p   : DeviceTablePtr;
    f   : FileInfoPointer;
BEGIN
    p := CAST(DeviceTablePtr, cid);
    f := p^.cd;

    Flush(p);

    IF f^.buffer <> NIL THEN
        RemoveFileBuffer(f^.f);
        DEALLOCATE(f^.buffer, f^.bufSize);
    END;

    IF amount <> 0 THEN
        IF NOT IsConsole(f^.f.handle) THEN
            IF amount > 32*1024 THEN
                amount := 32*1024;
            ELSIF amount < 512 THEN
                amount := 512;
            END;
            ALLOCATE(f^.buffer, amount);
            IF f^.buffer <> NIL THEN
                f^.bufSize := amount;
                SetFileBuffer(f^.f, f^.buffer^[0..amount-1]);
            END;
        END;
    END;
END SetDeviceBuffer;

END ISOfiler.
