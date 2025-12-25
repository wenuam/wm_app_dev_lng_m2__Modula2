(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE FileFunc;
<*/NOWARN:F*>

<*/VALIDVER:PROTECT*>
(*<*/VERSION:PROTECT*>*)

FROM SYSTEM IMPORT
    BYTE, ADR, ADDRESS, ADRCARD, CAST, ADRINT;

%IF Unicode %THEN
FROM SYSTEM IMPORT
    WORD;
%END

FROM SysClock IMPORT
    DateTime;

FROM Strings IMPORT
    Concat, Append, Delete, Equal;

FROM ExStrings IMPORT
    AppendCharCond, AppendChar, AppendNum, GetNextItem, EqualI;

FROM Environment IMPORT
    GetSymbol;

FROM MemUtils IMPORT
    MoveMem;

FROM WIN32 IMPORT
    HANDLE, HINSTANCE, DWORD, BOOL, LPTSTR, ULARGE_INTEGER,
    SECURITY_ATTRIBUTES, INVALID_HANDLE_VALUE, OVERLAPPED,
    FILE_ATTRIBUTE_NORMAL, GENERIC_READ, GENERIC_WRITE,
    FILE_SHARE_READ, FILE_SHARE_WRITE, OPEN_EXISTING,
    CREATE_ALWAYS,
    CloseHandle, GetLastError, ReadFile, WriteFile,
    SetEndOfFile, GetFullPathName, GetDiskFreeSpace,
    LockFileEx, UnlockFileEx, LOCKFILE_EXCLUSIVE_LOCK, LOCKFILE_FAIL_IMMEDIATELY,
    (*
    SECURITY_DESCRIPTOR, SECURITY_DESCRIPTOR_REVISION,
    InitializeSecurityDescriptor, SetSecurityDescriptorDacl,
    *)
    FILE_BEGIN, FILE_CURRENT, FILE_END, SetFilePointerEx,
    FILE_ATTRIBUTE_ARCHIVE, FILE_ATTRIBUTE_DIRECTORY,
    FILE_ATTRIBUTE_HIDDEN, FILE_ATTRIBUTE_READONLY,
    FILE_ATTRIBUTE_SYSTEM, FILE_ATTRIBUTE_COMPRESSED,
    FILE_ATTRIBUTE_TEMPORARY, FILE_ATTRIBUTE_ENCRYPTED,
    FILE_ATTRIBUTE_OFFLINE,
    SetFileAttributes, GetFileAttributes,
    FILE_FLAG_WRITE_THROUGH,
    FILE_FLAG_RANDOM_ACCESS, FILE_FLAG_SEQUENTIAL_SCAN,
    FILETIME, SYSTEMTIME, SetFileTime,
    FileTimeToLocalFileTime, LocalFileTimeToFileTime,
    FileTimeToSystemTime, SystemTimeToFileTime,
    TIME_ZONE_INFORMATION, GetTimeZoneInformation,
    TIME_ZONE_ID_DAYLIGHT, TIME_ZONE_ID_UNKNOWN,
    FindFirstFile, FindNextFile, WIN32_FIND_DATA,
    MoveFile, FlushFileBuffers, GetTempPath,
    CreateDirectory, RemoveDirectory, GetCurrentDirectory, SetCurrentDirectory,
    FILE_TYPE_DISK, FILE_TYPE_CHAR, FILE_TYPE_PIPE, GetFileType,
    DRIVE_REMOVABLE, DRIVE_REMOTE, DRIVE_CDROM, DRIVE_FIXED, DRIVE_RAMDISK,
    GetModuleHandle, GetProcAddress, GetCurrentThreadId;
IMPORT WIN32;

IMPORT WINERROR;

FROM WINX IMPORT
    NULL_HANDLE,
    NIL_FILETIME, NIL_SECURITY_ATTRIBUTES;

%IF PROTECT %THEN
FROM Threads IMPORT
    CreateCriticalSection, CloseCriticalSection,
    EnterCriticalSection, LeaveCriticalSection;
%END

PROCEDURE GetRootDirectory(VAR spec : ARRAY OF CHAR);
VAR
    parts       : FileNameParts;
BEGIN
    ConstructFileName(spec, "FOO.FOO", spec);
    ExpandFileSpec(spec);
    ParseFileName(spec, parts);

    IF parts.drive[0] <> '' THEN
        spec := parts.drive;
    ELSE
        spec := "";
    END;
    AppendCharCond('\', spec);
END GetRootDirectory;

PROCEDURE GetTimeZone(VAR dt : DateTime);
VAR
    zone        : TIME_ZONE_INFORMATION;
    retVal      : CARDINAL32;
BEGIN
    dt.zone := 0;
    dt.summerTimeFlag := FALSE;

    retVal := GetTimeZoneInformation(zone);
    IF retVal <> 0FFFFFFFFh THEN
        dt.zone := zone.Bias;

        IF retVal <> TIME_ZONE_ID_UNKNOWN THEN
            dt.summerTimeFlag := retVal = TIME_ZONE_ID_DAYLIGHT;
            IF dt.summerTimeFlag THEN
                dt.zone := dt.zone + zone.DaylightBias;
            END;
        END;
    END;
END GetTimeZone;

PROCEDURE OpenFileEx(VAR OUT f : File;
                     spec : ARRAY OF CHAR;
                     mode : AccessModes;
                     useInfo : FileUseInfoSet);
VAR
<*/PUSH/NOCHECK:U*>
    access      : DWORD;
    share       : DWORD;
<*/POP*>
    flags       : DWORD;
    secAttr     : SECURITY_ATTRIBUTES;
    (*secDesc     : SECURITY_DESCRIPTOR;*)
    tempSpec    : FileSpecString;
BEGIN
    f.eof := FALSE;
    f.count := 0;
    f.buffered := FALSE;
    f.buffer := NIL;
    f.userData := NIL;
    f.bp := 0;
    f.be := 0;
    f.size := 0;
    f.start := 0;
    f.end := 0;
    f.peeked := FALSE;
    f.valid := FALSE;
    f.dirty := FALSE;
    f.canPosition := TRUE;
    f.mode := mode;

    (*
    InitializeSecurityDescriptor(secDesc, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(secDesc, TRUE, NIL, FALSE);
    *)
    secAttr.nLength := SIZE(secAttr);
    (*secAttr.lpSecurityDescriptor := ADR(secDesc);*)
    secAttr.lpSecurityDescriptor := NIL;
    secAttr.bInheritHandle := FALSE;

    CASE mode OF
    ReadOnlyDenyNone:
        access := GENERIC_READ;
        share := FILE_SHARE_READ BOR FILE_SHARE_WRITE;
    |
    ReadOnlyDenyWrite:
        access := GENERIC_READ;
        share := FILE_SHARE_READ;
    |
    ReadOnlyDenyAll:
        access := GENERIC_READ;
        share := 0;
    |
    ReadWriteDenyNone:
        access := GENERIC_READ BOR GENERIC_WRITE;
        share := FILE_SHARE_READ BOR FILE_SHARE_WRITE;
    |
    ReadWriteDenyWrite:
        access := GENERIC_READ BOR GENERIC_WRITE;
        share := FILE_SHARE_READ;
    |
    ReadWriteDenyAll:
        access := GENERIC_READ BOR GENERIC_WRITE;
        share := 0;
    |
    WriteOnlyDenyNone:
        access := GENERIC_WRITE;
        share := FILE_SHARE_READ BOR FILE_SHARE_WRITE;
    |
    WriteOnlyDenyAll:
        access := GENERIC_WRITE;
        share := 0;
    END;

    flags := FILE_ATTRIBUTE_NORMAL;
    IF SequentialAccess IN useInfo THEN
        flags := flags BOR FILE_FLAG_SEQUENTIAL_SCAN;
    END;
    IF RandomAccess IN useInfo THEN
        flags := flags BOR FILE_FLAG_RANDOM_ACCESS;
    END;
    IF WriteThrough IN useInfo THEN
        flags := flags BOR FILE_FLAG_WRITE_THROUGH;
    END;
    IF TemporaryFile IN useInfo THEN
        flags := flags BOR FILE_ATTRIBUTE_TEMPORARY;
    END;

    tempSpec := spec;
    tempSpec[HIGH(tempSpec)] := '';
    f.handle := CAST(ADRINT, WIN32.CreateFile(tempSpec,
                                                access,
                                                share,
                                                secAttr,
                                                OPEN_EXISTING,
                                                flags,
                                                NULL_HANDLE));

    IF f.handle <> InvalidHandle THEN
        f.status := 0;

        f.canPosition := GetFileType(CAST(HANDLE, f.handle)) = FILE_TYPE_DISK;

        %IF PROTECT %THEN
            CreateCriticalSection(f.critic);
        %END
    ELSE
        f.handle := InvalidHandle;
        f.status := GetLastError();
    END;
END OpenFileEx;

PROCEDURE OpenFile(VAR OUT f : File;
                   spec : ARRAY OF CHAR;
                   mode : AccessModes);
BEGIN
    OpenFileEx(f, spec, mode, FileUseInfoSet{});
END OpenFile;

PROCEDURE CreateFileEx(VAR OUT f : File;
                       spec : ARRAY OF CHAR;
                       useInfo : FileUseInfoSet);
VAR
    secAttr     : SECURITY_ATTRIBUTES;
    (*secDesc     : SECURITY_DESCRIPTOR;*)
    tempSpec    : FileSpecString;
    flags       : DWORD;
BEGIN
    f.eof := TRUE;
    f.count := 0;
    f.buffered := FALSE;
    f.buffer := NIL;
    f.userData := NIL;
    f.bp := 0;
    f.be := 0;
    f.size := 0;
    f.start := 0;
    f.end := 0;
    f.valid := FALSE;
    f.dirty := FALSE;
    f.peeked := FALSE;
    f.mode := ReadWriteDenyAll;
    f.canPosition := TRUE;

    (* provide full access *)

    (*
    InitializeSecurityDescriptor(secDesc, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(secDesc, TRUE, NIL, FALSE);
    *)
    secAttr.nLength := SIZE(secAttr);
    (*secAttr.lpSecurityDescriptor := ADR(secDesc);*)
    secAttr.lpSecurityDescriptor := NIL;
    secAttr.bInheritHandle := FALSE;

    flags := FILE_ATTRIBUTE_NORMAL;
    IF SequentialAccess IN useInfo THEN
        flags := flags BOR FILE_FLAG_SEQUENTIAL_SCAN;
    END;
    IF RandomAccess IN useInfo THEN
        flags := flags BOR FILE_FLAG_RANDOM_ACCESS;
    END;
    IF WriteThrough IN useInfo THEN
        flags := flags BOR FILE_FLAG_WRITE_THROUGH;
    END;
    IF TemporaryFile IN useInfo THEN
        flags := flags BOR FILE_ATTRIBUTE_TEMPORARY;
    END;

    tempSpec := spec;
    tempSpec[HIGH(tempSpec)] := '';
    f.handle := CAST(ADRINT, WIN32.CreateFile(tempSpec,
                                                GENERIC_READ
                                                    BOR GENERIC_WRITE,
                                                0,
                                                secAttr,
                                                CREATE_ALWAYS,
                                                flags,
                                                NULL_HANDLE));

    IF f.handle <> InvalidHandle THEN
        SetEndOfFile(CAST(HANDLE, f.handle));
        f.status := 0;

        f.canPosition := GetFileType(CAST(HANDLE, f.handle)) = FILE_TYPE_DISK;

        %IF PROTECT %THEN
            CreateCriticalSection(f.critic);
        %END
    ELSE
        f.handle := InvalidHandle;
        f.status := GetLastError();
    END;
END CreateFileEx;

PROCEDURE CreateFile(VAR OUT f : File; spec : ARRAY OF CHAR);
BEGIN
    CreateFileEx(f, spec, FileUseInfoSet{});
END CreateFile;

PROCEDURE GetTempFileDirectory(VAR OUT spec : ARRAY OF CHAR);
VAR
    result      : CARDINAL;
BEGIN
    result := GetTempPath(HIGH(spec)+1, spec);
    IF (result = 0) OR (result > (HIGH(spec)+1)) THEN
        spec := ".\";
    END;
END GetTempFileDirectory;

PROCEDURE MakeTempFileName(VAR INOUT spec : ARRAY OF CHAR);
VAR
    tempName    : FileSpecString;
    tempSpec    : FileSpecString;
    prefix      : ARRAY [0..15] OF CHAR;
    i           : CARDINAL;
BEGIN
    tempSpec := spec;
    IF tempSpec[0] = '' THEN
        GetTempFileDirectory(tempSpec);
    ELSE
        AppendCharCond('\', tempSpec);
    END;
    Append(".TMP", tempSpec);

    (* we want multiple threads to be able to get unique file names. *)

    (* because of the existance of old file servers(Novell) we should? *)
    (* make sure the filename is no longer than 8 characters *)

    prefix := "~";
    AppendNum(GetCurrentThreadId() REM 10000, prefix);

    i := 0;
    REPEAT
        tempName := prefix;
        AppendChar('-', tempName);
        AppendNum(i, tempName);
        INC(i);
        ConstructFileName(tempName, tempSpec, tempName);
    UNTIL NOT FileExists(tempName);

    spec := tempName;
END MakeTempFileName;

PROCEDURE CreateTempFile(VAR OUT f : File; VAR INOUT spec : ARRAY OF CHAR);
BEGIN
    CreateTempFileEx(f, spec, FileUseInfoSet{});
END CreateTempFile;

PROCEDURE CreateTempFileEx(VAR OUT f : File;
                           VAR INOUT spec : ARRAY OF CHAR;
                           useInfo : FileUseInfoSet);
BEGIN
    MakeTempFileName(spec);
    CreateFileEx(f, spec, useInfo+FileUseInfoSet{TemporaryFile});
END CreateTempFileEx;

PROCEDURE OpenCreateFile(VAR OUT f : File;
                         spec : ARRAY OF CHAR;
                         mode : AccessModes);
BEGIN
    OpenCreateFileEx(f, spec, mode, FileUseInfoSet{});
END OpenCreateFile;

PROCEDURE OpenCreateFileEx(VAR OUT f : File;
                           spec : ARRAY OF CHAR;
                           mode : AccessModes;
                           useInfo : FileUseInfoSet);
BEGIN
    OpenFileEx(f, spec, mode, useInfo);
    IF f.status <> 0 THEN
        CreateFileEx(f, spec, useInfo);
    END;
END OpenCreateFileEx;

PROCEDURE FakeFileOpen(VAR OUT f : File; handle : ADRINT; mode : AccessModes);
BEGIN
    f.handle := handle;
    f.mode := mode;

    f.status := 0;
    f.eof := FALSE;
    f.count := 0;
    f.buffered := FALSE;
    f.buffer := NIL;
    f.userData := NIL;
    f.bp := 0;
    f.be := 0;
    f.size := 0;
    f.start := 0;
    f.end := 0;
    f.valid := FALSE;
    f.dirty := FALSE;
    f.peeked := FALSE;
    f.canPosition := FileType(f) = FileTypeDisk;
END FakeFileOpen;

PROCEDURE CloseFile(VAR INOUT f : File);
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    IF f.buffered THEN
        IF NOT FlushFile(f) THEN
        END;
    END;

    IF CloseHandle(CAST(HANDLE, f.handle)) THEN
        f.status := 0;
        f.handle := InvalidHandle;

        %IF PROTECT %THEN
            LeaveCriticalSection(f.critic);
            CloseCriticalSection(f.critic);
        %END
    ELSE
        %IF PROTECT %THEN
            LeaveCriticalSection(f.critic);
        %END
        f.status := GetLastError();
    END;
END CloseFile;

PROCEDURE FileType(VAR INOUT f : File) : FileTypes;
VAR
    typ         : FileTypes;
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    CASE GetFileType(CAST(HANDLE, f.handle)) OF
    FILE_TYPE_DISK:
        typ := FileTypeDisk;
    |
    FILE_TYPE_CHAR:
        typ := FileTypeChar;
    |
    FILE_TYPE_PIPE:
        typ := FileTypePipe;
    ELSE
        typ := FileTypeUnknown;
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
    RETURN typ;
END FileType;

PROCEDURE SetFileBuffer(VAR INOUT f : File; VAR OUT buf : ARRAY OF BYTE);
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    IF NOT f.buffered THEN
        f.status := 0;
        f.bp := 0;
        f.be := 0;
        f.size := HIGH(buf)+1;
        f.buffer := ADR(buf);
        f.valid := FALSE;
        f.dirty := FALSE;
        f.peeked := FALSE;
        IF f.canPosition THEN
            f.start := GetFilePos(f);
        ELSE
            f.start := 0;
        END;
        f.end := f.start;
        f.buffered := TRUE;
    ELSE
        f.status := 0FFFFFFFFh;
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
END SetFileBuffer;

PROCEDURE RemoveFileBuffer(VAR INOUT f : File);
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    f.status := 0;

    IF f.buffered THEN
        IF NOT FlushFile(f) THEN
            %IF PROTECT %THEN
                LeaveCriticalSection(f.critic);
            %END
            RETURN;
        END;

        f.bp := 0;
        f.be := 0;
        f.size := 0;
        f.buffer := NIL;
        f.start := 0;
        f.end := 0;
        f.valid := FALSE;
        f.dirty := FALSE;
        f.buffered := FALSE;
        f.peeked := FALSE;
    ELSE
        f.status := 0FFFFFFFFh;
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
END RemoveFileBuffer;

PROCEDURE FlushBuffers(VAR INOUT f : File; flushOS : BOOLEAN);
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    f.status := 0;

    IF f.buffered AND f.dirty THEN
        IF FlushFile(f) THEN
            (* advance the position *)
            IF f.canPosition THEN
                INC(f.start, f.be);
            ELSE
                f.start := 0;
            END;
            f.end := f.start;
            f.bp := 0;
            f.be := 0;
            f.valid := FALSE;
        ELSE
            f.status := GetLastError();
            %IF PROTECT %THEN
                LeaveCriticalSection(f.critic);
            %END
            RETURN;
        END;
    END;

    IF flushOS THEN
        IF FlushFileBuffers(CAST(HANDLE, f.handle)) THEN
            %IF PROTECT %THEN
                LeaveCriticalSection(f.critic);
            %END
            RETURN;
        ELSE
            f.status := GetLastError();
        END;
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
END FlushBuffers;

PROCEDURE PosOp(VAR INOUT f : File; typ : CARDINAL; val : INTEGER64) : CARDINAL64;
VAR
    res         : INTEGER64;
BEGIN
    IF SetFilePointerEx(CAST(HANDLE, f.handle), val, ADR(res), typ) THEN
        f.status := 0;
    ELSE
        f.status := GetLastError();
    END;
    RETURN res;
END PosOp;

PROCEDURE ReadWrite(VAR INOUT f : File;
                    buf : ADDRESS;
                    size : CARDINAL;
                    read : BOOLEAN);
VAR
    done        : BOOL;
BEGIN
    IF read THEN
        done := ReadFile(CAST(HANDLE, f.handle), buf, size, f.count, NIL);
    ELSE
        done := WriteFile(CAST(HANDLE, f.handle), buf, size, f.count, NIL);
    END;

    IF NOT done THEN
        f.status := GetLastError();
    ELSE
        f.status := 0;

        IF read AND (f.count < size) THEN
            f.eof := TRUE;
        END;
    END;
END ReadWrite;

PROCEDURE FlushFile(VAR INOUT f : File) : BOOLEAN;
BEGIN
    f.status := 0;

    IF f.dirty THEN
        IF f.canPosition THEN
            PosOp(f, FILE_BEGIN, f.start);
            IF f.status <> 0 THEN
                RETURN FALSE;
            END;
        END;

        ReadWrite(f, f.buffer, f.be, FALSE);
        IF (f.status <> 0) OR (f.count <> f.be) THEN
            f.status := GetLastError();
            RETURN FALSE;
        END;

        f.dirty := FALSE;
    END;

    RETURN TRUE;
END FlushFile;

PROCEDURE FillBuffer(VAR INOUT f : File) : BOOLEAN;
BEGIN
    f.status := 0;

    IF f.eof THEN
        RETURN FALSE;
    END;

    IF NOT FlushFile(f) THEN
        RETURN FALSE;
    END;

    IF f.canPosition THEN
        PosOp(f, FILE_BEGIN, f.end);
        IF f.status <> 0 THEN
            f.status := GetLastError();
            RETURN FALSE;
        END;
    END;

    ReadWrite(f, f.buffer, f.size, TRUE);
    IF f.status <> 0 THEN
        f.status := GetLastError();
        RETURN FALSE;
    ELSIF f.count = 0 THEN
        f.bp := 0;
        f.be := 0;
        IF f.canPosition THEN
            f.start := PosOp(f, FILE_CURRENT, 0);
        ELSE
            f.start := 0;
        END;
        f.end := f.start;
        f.eof := TRUE;
        f.dirty := FALSE;
        f.valid := FALSE;
        RETURN FALSE;
    END;

    f.eof := FALSE;

    IF f.canPosition THEN
        f.start := f.end;
        INC(f.end, f.count);
    ELSE
        f.start := 0;
        f.end := f.count;
    END;
    f.be := f.count;
    f.bp := 0;
    f.valid := TRUE;
    f.dirty := FALSE;
    RETURN TRUE;
END FillBuffer;

PROCEDURE ReadBlock(VAR INOUT f : File; buf : ADDRESS; size : CARDINAL);
VAR
    i           : CARDINAL;
    left        : CARDINAL;
    this        : CARDINAL;
    data        : POINTER TO ARRAY [0..0] OF BYTE;
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    IF (f.mode >= ReadOnlyDenyNone) AND (f.mode <= ReadWriteDenyAll) THEN
        data := buf;
        f.status := 0;

        IF size = 0 THEN
            f.count := 0;
        ELSIF f.peeked THEN
            f.peeked := FALSE;
            IF size >= SIZE(CHAR) THEN
                data^[0]:CHAR := f.peekedChar;
                size := size - SIZE(CHAR);
                f.count := 0;
                IF size <> 0 THEN
                    ReadBlock(f, ADR(data^[SIZE(CHAR)]), size);
                END;
                f.count := f.count + SIZE(CHAR);
            ELSE
                (* we cannot really deal with this, just give an error *)
                f.status := 30;
            END;
        ELSIF f.buffered THEN
            IF f.bp+size <= f.be THEN
                f.count := size;

                CASE size OF
                1:
                    data^[0] := f.buffer^[f.bp];
                    INC(f.bp);
                |
                2:
                    data^[0] := f.buffer^[f.bp];
                    data^[1] := f.buffer^[f.bp+1];
                    f.bp := f.bp + 2;
                |
                4:
                    data^[0] := f.buffer^[f.bp];
                    data^[1] := f.buffer^[f.bp+1];
                    data^[2] := f.buffer^[f.bp+2];
                    data^[3] := f.buffer^[f.bp+3];
                    f.bp := f.bp + 4;
                ELSE
                    MoveMem(data, ADR(f.buffer^[f.bp]), size);
                    f.bp := f.bp + size;
                END;
            ELSE
                left := size;
                i := 0;
                REPEAT

                    (* if buffer is empty, get some stuff *)

                    IF f.bp = f.be THEN
                        f.bp := 0;
                        IF NOT FillBuffer(f) THEN
                            BREAK;
                        END;
                    END;

                    (* See how much of the record is in the buffer *)

                    this := f.be - f.bp;
                    IF this > left THEN
                        this := left;
                    END;

                    (* Get the portion in this buffer *)

                    IF this <> 0 THEN
                        MoveMem(ADR(data^[i]), ADR(f.buffer^[f.bp]), this);
                        i := i + this;
                        f.bp := f.bp + this;
                        left := left - this;
                    END;
                UNTIL left = 0;
                f.count := i;
            END;
        ELSE
            ReadWrite(f, data, size, TRUE)
        END;
    ELSE
        f.status := WINERROR.ERROR_ACCESS_DENIED;
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
END ReadBlock;

PROCEDURE WriteBlock(VAR INOUT f : File; buf : ADDRESS; size : CARDINAL);
VAR
    i           : CARDINAL;
    left        : CARDINAL;
    this        : CARDINAL;
    data        : POINTER TO ARRAY [0..0] OF BYTE;
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    IF (f.mode >= ReadWriteDenyNone) AND (f.mode <= WriteOnlyDenyAll) THEN
        data := buf;
        f.peeked := FALSE;
        f.status := 0;

        IF size = 0 THEN
            f.count := 0;
        ELSIF f.buffered THEN
            IF f.bp+size <= f.size THEN
                CASE size OF
                1:
                    f.buffer^[f.bp] := data^[0];
                    INC(f.bp);
                |
                2:
                    f.buffer^[f.bp] := data^[0];
                    f.buffer^[f.bp+1] := data^[1];
                    f.bp := f.bp + 2;
                |
                4:
                    f.buffer^[f.bp] := data^[0];
                    f.buffer^[f.bp+1] := data^[1];
                    f.buffer^[f.bp+2] := data^[2];
                    f.buffer^[f.bp+3] := data^[3];
                    f.bp := f.bp + 4;
                ELSE
                    MoveMem(ADR(f.buffer^[f.bp]), data, size);
                    f.bp := f.bp + size;
                END;

                f.count := size;
                IF f.bp > f.be THEN
                    f.be := f.bp;
                END;
                f.valid := TRUE;
                f.dirty := TRUE;
                f.end := f.start;
                INC(f.end, f.be);
            ELSE
                left := size;
                i := 0;
                REPEAT

                    (* if buffer is full, flush it *)

                    IF f.bp = f.size THEN
                        f.bp := 0;
                        IF FlushFile(f) THEN
                            (* advance the position *)
                            IF f.canPosition THEN
                                INC(f.start, f.be);
                            ELSE
                                f.start := 0;
                            END;
                            f.end := f.start;
                            f.bp := 0;
                            f.be := 0;
                            f.valid := FALSE;
                        ELSE
                            BREAK;
                        END;
                    END;

                    (* See how much of the record is in the buffer *)

                    this := f.size - f.bp;
                    IF this > left THEN
                        this := left;
                    END;

                    (* Put the portion in this buffer *)

                    IF this <> 0 THEN
                        MoveMem(ADR(f.buffer^[f.bp]), ADR(data^[i]), this);
                        i := i + this;
                        f.bp := f.bp + this;
                        IF f.bp > f.be THEN
                            f.be := f.bp;
                        END;
                        left := left - this;
                        f.dirty := TRUE;
                        f.valid := TRUE;
                    END;
                UNTIL left = 0;
                f.count := i;
                f.end := f.start;
                INC(f.end, f.be);
            END;
        ELSE
            ReadWrite(f, data, size, FALSE)
        END;
    ELSE
        f.status := WINERROR.ERROR_ACCESS_DENIED;
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
END WriteBlock;

PROCEDURE ReadChar(VAR INOUT f : File) : CHAR;
VAR
    ch  : CHAR;

    PROCEDURE getAChar() : CHAR;
    VAR
        <*/PUSH/NOCHECK:U*>
        ch      : CHAR;
        <*/POP*>
    BEGIN
        IF f.bp+SIZE(CHAR) <= f.be THEN
            %IF Ascii %THEN
                ch := f.buffer^[f.bp]:CHAR;
            %ELSE
                ch:WORD[0] := f.buffer^[f.bp]:CARDINAL8;
                ch:WORD[1] := f.buffer^[f.bp+1]:CARDINAL8;
            %END
            f.bp := f.bp + SIZE(CHAR);
        ELSE
            ReadBlock(f, ADR(ch), SIZE(ch));
            IF (f.status <> 0) OR (f.count <> SIZE(ch)) THEN
                ch := '';
            END;
        END;
        RETURN ch;
    END getAChar;

BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    IF (f.mode >= ReadOnlyDenyNone) AND (f.mode <= ReadWriteDenyAll) THEN

        IF f.buffered THEN
            f.status := 0;

            IF f.peeked THEN
                f.peeked := FALSE;
                ch := f.peekedChar;
            ELSE
                ch := getAChar();
            END;

            IF ch = CHR(13) THEN
                ch := getAChar();
                IF ch <> CHR(10) THEN
                    IF NOT f.eof THEN
                        IF f.canPosition THEN
                            MoveFilePos(f, -SIZE(ch));
                        ELSE
                            f.peeked := TRUE;
                            f.peekedChar := ch;
                        END;
                    END;
                    f.status := 0;
                    f.eof := FALSE;
                END;
                ch := EOL;
            ELSIF ch = CHR(10) THEN
                ch := EOL;
            END;
        ELSE
            ch := '';
            IF f.peeked THEN
                f.peeked := FALSE;
                ch := f.peekedChar;
                f.status := 0;
                f.count := SIZE(ch);
            ELSE
                ReadWrite(f, ADR(ch), SIZE(ch), TRUE);
            END;
            IF (f.status = 0) AND (f.count = SIZE(ch)) THEN
                IF ch = CHR(13) THEN
                    ReadWrite(f, ADR(ch), SIZE(ch), TRUE);
                    IF (f.status = 0) AND (f.count = SIZE(ch)) THEN
                        IF ch <> CHR(10) THEN
                            IF f.canPosition THEN
                                PosOp(f, FILE_CURRENT, -SIZE(ch));
                            ELSE
                                f.peeked := TRUE;
                                f.peekedChar := ch;
                            END;
                        END;
                    ELSE
                        f.status := 0;
                    END;
                    f.eof := FALSE;
                    ch := EOL;
                ELSIF ch = CHR(10) THEN
                    ch := EOL;
                END;
            ELSE
                ch := '';
            END;
        END;
    ELSE
        f.status := WINERROR.ERROR_ACCESS_DENIED;
        ch := '';
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
    RETURN ch;
END ReadChar;

PROCEDURE WriteChar(VAR INOUT f : File; ch : CHAR);
CONST
    crlf : ARRAY [0..1] OF CHAR = {CHR(13), CHR(10)};
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    IF (f.mode >= ReadWriteDenyNone) AND (f.mode <= WriteOnlyDenyAll) THEN
        f.peeked := FALSE;

        IF f.buffered THEN
            f.status := 0;

            IF ch = EOL THEN
                WriteBlock(f, ADR(crlf), SIZE(crlf));
            ELSE
                IF f.bp+SIZE(CHAR) <= f.size THEN
                    %IF Ascii %THEN
                        f.buffer^[f.bp]:CHAR := ch;
                    %ELSE
                        f.buffer^[f.bp]:CARDINAL8 := ch:WORD[0];
                        f.buffer^[f.bp+1]:CARDINAL8 := ch:WORD[1];
                    %END
                    f.bp := f.bp + SIZE(CHAR);

                    IF f.bp > f.be THEN
                        f.be := f.bp;
                    END;
                    f.valid := TRUE;
                    f.dirty := TRUE;
                    f.end := f.start;
                    INC(f.end, f.be);
                ELSE
                    WriteBlock(f, ADR(ch), SIZE(ch));
                END;
            END;
        ELSE
            IF ch = EOL THEN
                ReadWrite(f, ADR(crlf), SIZE(crlf), FALSE);
            ELSE
                ReadWrite(f, ADR(ch), SIZE(ch), FALSE);
            END;
        END;
    ELSE
        f.status := WINERROR.ERROR_ACCESS_DENIED;
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
END WriteChar;

PROCEDURE PeekChar(VAR INOUT f : File) : CHAR;
VAR
    <*/PUSH/NOCHECK:U*>
    ch          : CHAR;
    <*/POP*>
    stat        : CARDINAL;
    eof         : BOOLEAN;
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    IF (f.mode >= ReadOnlyDenyNone) AND (f.mode <= ReadWriteDenyAll) THEN
        f.status := 0;

        IF f.peeked THEN
            ch := f.peekedChar;
        ELSIF f.buffered THEN
            IF f.bp+SIZE(CHAR) <= f.be THEN
                %IF Ascii %THEN
                    ch := f.buffer^[f.bp]:CHAR;
                %ELSE
                    ch:WORD[0] := f.buffer^[f.bp]:CARDINAL8;
                    ch:WORD[1] := f.buffer^[f.bp+1]:CARDINAL8;
                %END
            ELSE
                ReadBlock(f, ADR(ch), SIZE(ch));
                IF f.canPosition THEN
                    stat := f.status;
                    eof := f.eof;
                    MoveFilePos(f, -SIZE(CHAR));
                    f.status := stat;
                    f.eof := eof;
                ELSE
                    f.peeked := TRUE;
                    f.peekedChar := ch;
                END;
            END;
        ELSE
            ch := '';
            ReadWrite(f, ADR(ch), SIZE(ch), TRUE);
            IF (f.status = 0) AND (f.count = SIZE(ch)) THEN
                f.eof := FALSE;
                IF f.canPosition THEN
                    PosOp(f, FILE_CURRENT, -SIZE(ch));
                ELSE
                    f.peeked := TRUE;
                    f.peekedChar := ch;
                END;
            ELSE
                ch := '';
            END;
        END;

        IF (ch = CHR(13)) OR (ch = CHR(10)) THEN
            ch := EOL;
        END;
    ELSE
        f.status := WINERROR.ERROR_ACCESS_DENIED;
        ch := '';
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
    RETURN ch;
END PeekChar;

PROCEDURE ReadLine(VAR INOUT f : File; VAR OUT str : ARRAY OF CHAR) : CARDINAL;
VAR
    i           : ADRCARD;
    highStr     : ADRCARD;
    ch          : CHAR;
BEGIN
    highStr := HIGH(str);
    i := 0;
    LOOP
        IF i <= highStr THEN
            ch := ReadChar(f);
            IF (f.status = 0) AND (NOT f.eof) THEN
                IF ch <> EOL THEN
                    str[i] := ch;
                    INC(i);
                ELSE
                    EXIT;
                END;
            ELSE
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;
    IF i <= highStr THEN
        str[i] := '';
    END;
    RETURN i;
END ReadLine;

PROCEDURE WriteLine(VAR INOUT f : File; str : ARRAY OF CHAR);
BEGIN
    IF str[0] <> '' THEN
        WriteBlock(f, ADR(str), LENGTH(str)*SIZE(CHAR));
    END;
    WriteChar(f, EOL);
END WriteLine;

PROCEDURE LockFileRegion(VAR INOUT f : File; start, length : CARDINAL64; lockType : FileLockTypes; wait : BOOLEAN);
CONST
    winLock     : ARRAY FileLockTypes OF CARDINAL = {0, LOCKFILE_EXCLUSIVE_LOCK};
    winWait     : ARRAY BOOLEAN OF CARDINAL = {LOCKFILE_FAIL_IMMEDIATELY, 0};
VAR
    overlap     : OVERLAPPED;
BEGIN
    IF length = 0 THEN
        length := MAX(CARDINAL64);
    END;
    f.status := 0;
    overlap.hEvent := NIL;
    overlap.Offset := start REM 100000000H;
    overlap.OffsetHigh := start / 100000000H;
    IF NOT LockFileEx(CAST(HANDLE, f.handle),
                      winLock[lockType] BOR winWait[wait],
                      0,(*reserved*)
                      length REM 100000000H, length / 100000000H, ADR(overlap))
    THEN
        f.status := GetLastError();
    END;
END LockFileRegion;

PROCEDURE UnlockFileRegion(VAR INOUT f : File; start, length : CARDINAL64);
VAR
    overlap     : OVERLAPPED;
BEGIN
    IF length = 0 THEN
        length := MAX(CARDINAL);
    END;
    f.status := 0;
    overlap.hEvent := NIL;
    overlap.Offset := start REM 100000000H;
    overlap.OffsetHigh := start / 100000000H;
    IF NOT UnlockFileEx(CAST(HANDLE, f.handle), 0(*reserved*), length REM 100000000H, length / 100000000H, ADR(overlap)) THEN
        f.status := GetLastError();
    END;
END UnlockFileRegion;

PROCEDURE SetFilePos(VAR INOUT f : File; pos : CARDINAL64);
VAR
    temp        : INTEGER;
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    f.peeked := FALSE;

    IF f.buffered THEN
        f.status := 0;

        temp := INT(pos) - INT(f.start);
        IF f.valid AND (temp >= 0) AND (temp <= INT(f.be)) THEN
            f.bp := temp;
            f.eof := FALSE;
        ELSE
            FlushFile(f);
            f.start := pos;
            f.end := pos;
            f.bp := 0;
            f.be := 0;
            f.eof := FALSE;
            f.valid := FALSE;
            f.dirty := FALSE;
        END;
    ELSE
        PosOp(f, FILE_BEGIN, pos);
        f.eof := FALSE;
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
END SetFilePos;

PROCEDURE GetFilePos(VAR INOUT f : File) : CARDINAL64;
VAR
    pos         : CARDINAL64;
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    IF f.buffered THEN
        f.status := 0;

        pos := f.start + VAL(CARDINAL64,f.bp);
    ELSE
        pos := PosOp(f, FILE_CURRENT, 0);
    END;

    IF f.peeked THEN
        pos := pos - SIZE(CHAR);
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
    RETURN pos;
END GetFilePos;

PROCEDURE MoveFilePos(VAR INOUT f : File; pos : INTEGER64);
VAR
    temp        : CARDINAL64;
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    f.peeked := FALSE;

    IF f.buffered THEN
        f.status := 0;

        IF pos < 0 THEN
            temp := ABS(pos);
            IF temp <= VAL(CARDINAL64,f.bp) THEN
                f.bp := f.bp - VAL(CARDINAL,temp);
                f.eof := FALSE;
                RETURN;
            ELSE
                temp := f.start + VAL(CARDINAL64,f.bp) - temp;
            END;
        ELSE
            temp := VAL(CARDINAL64,f.bp) + VAL(CARDINAL64,pos);
            IF temp <= VAL(CARDINAL64,f.be) THEN
                f.bp := temp;
                f.eof := FALSE;
                RETURN;
            ELSE
                temp := f.start + temp;
            END;
        END;

        FlushFile(f);
        f.start := temp;
        f.end := temp;
        f.bp := 0;
        f.be := 0;
        f.eof := FALSE;
        f.valid := FALSE;
        f.dirty := FALSE;
    ELSE
        PosOp(f, FILE_CURRENT, pos);
        f.eof := FALSE;
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
END MoveFilePos;

PROCEDURE TruncateFile(VAR INOUT f : File);
VAR
    pos         : CARDINAL64;
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    pos := GetFilePos(f);

    IF f.buffered THEN
        IF NOT FlushFile(f) THEN
            %IF PROTECT %THEN
                LeaveCriticalSection(f.critic);
            %END
            RETURN;
        END;
        PosOp(f, FILE_BEGIN, pos);
    END;

    SetFilePos(f, pos);

    f.status := 0;
    IF NOT SetEndOfFile(CAST(HANDLE, f.handle)) THEN
        f.status := GetLastError();
    END;

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
END TruncateFile;

PROCEDURE FileLength(VAR INOUT f : File) : CARDINAL64;
VAR
    l, save     : CARDINAL64;
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(f.critic);
    %END

    save := GetFilePos(f);

    IF f.buffered THEN
        IF NOT FlushFile(f) THEN
            %IF PROTECT %THEN
                LeaveCriticalSection(f.critic);
            %END
            RETURN 0;
        END;
    END;

    (* get end *)

    l := PosOp(f, FILE_END, 0);

    SetFilePos(f, save);

    %IF PROTECT %THEN
        LeaveCriticalSection(f.critic);
    %END
    RETURN l;
END FileLength;

PROCEDURE GetFileSizes(name : ARRAY OF CHAR; VAR actual, alloc : CARDINAL64);
VAR
    entry               : SearchEntry;
    sectorsPerCluster   : CARDINAL32;
    bytesPerSector      : CARDINAL32;
    freeClusters        : CARDINAL32;
    granularity         : CARDINAL32;
    result              : CARDINAL32;
    temp                : FileSpecString;
BEGIN
    actual := 0;
    alloc := 0;
    IF FindFirst(name, AllAttributes, MustHaveNormalFile, entry) THEN
        FindClose(entry);

        actual := entry.size;

        temp := name;
        GetRootDirectory(temp);

        temp[HIGH(temp)] := '';
        IF GetDiskFreeSpace(temp,
                            sectorsPerCluster,
                            bytesPerSector,
                            freeClusters,
                            granularity)
        THEN
            granularity := sectorsPerCluster * bytesPerSector;
            result := entry.size / VAL(CARDINAL64,granularity);
            IF (entry.size REM VAL(CARDINAL64,granularity)) <> 0 THEN
                INC(result);
            END;
            alloc := result * granularity;
        END;
    END;
END GetFileSizes;

PROCEDURE TranslateFileError(f : File) : CommonFileErrors;
BEGIN
    CASE f.status OF
    WINERROR.ERROR_SUCCESS:
        RETURN FileErrSuccess;
    |
    WINERROR.ERROR_FILE_NOT_FOUND:
        RETURN FileErrFileNotFound;
    |
    WINERROR.ERROR_PATH_NOT_FOUND:
        RETURN FileErrPathNotFound;
    |
    WINERROR.ERROR_TOO_MANY_OPEN_FILES:
        RETURN FileErrNoHandles;
    |
    WINERROR.ERROR_ACCESS_DENIED:
        RETURN FileErrAccessDenied;
    |
    WINERROR.ERROR_INVALID_HANDLE:
        RETURN FileErrInvalidHandle;
    |
    WINERROR.ERROR_NOT_READY:
        RETURN FileErrNotReady;
    |
    WINERROR.ERROR_WRITE_PROTECT:
        RETURN FileErrWriteProtect;
    |
    WINERROR.ERROR_SHARING_VIOLATION, WINERROR.ERROR_LOCK_VIOLATION:
        RETURN FileErrSharingOrLock;
    |
    WINERROR.ERROR_HANDLE_DISK_FULL, WINERROR.ERROR_DISK_FULL:
        RETURN FileErrDiskFull;
    |
    WINERROR.ERROR_BROKEN_PIPE:
        RETURN FileErrBrokenPipe;
    ELSE
        RETURN FileErrUnknown;
    END;
END TranslateFileError;

PROCEDURE GetM2Attributes(attrib : CARDINAL) : FileAttributeSet;
VAR
    attr        : FileAttributeSet;
BEGIN
    attr := FileAttributeSet{};

    IF (attrib BAND FILE_ATTRIBUTE_ARCHIVE) <> 0 THEN
        INCL(attr, Archive);
    END;
    IF (attrib BAND FILE_ATTRIBUTE_READONLY) <> 0 THEN
        INCL(attr, ReadOnly);
    END;
    IF (attrib BAND FILE_ATTRIBUTE_HIDDEN) <> 0 THEN
        INCL(attr, Hidden);
    END;
    IF (attrib BAND FILE_ATTRIBUTE_SYSTEM) <> 0 THEN
        INCL(attr, System);
    END;
    IF (attrib BAND FILE_ATTRIBUTE_COMPRESSED) <> 0 THEN
        INCL(attr, Compressed);
    END;
    IF (attrib BAND FILE_ATTRIBUTE_TEMPORARY) <> 0 THEN
        INCL(attr, Temporary);
    END;
    IF (attrib BAND FILE_ATTRIBUTE_ENCRYPTED) <> 0 THEN
        INCL(attr, Encrypted);
    END;
    IF (attrib BAND FILE_ATTRIBUTE_OFFLINE) <> 0 THEN
        INCL(attr, Offline);
    END;
    IF (attrib BAND FILE_ATTRIBUTE_DIRECTORY) <> 0 THEN
        INCL(attr, Directory);
    ELSE
        INCL(attr, NormalFile);
    END;

    RETURN attr;
END GetM2Attributes;

PROCEDURE GetWin32Attributes(attr : FileAttributeSet) : DWORD;
VAR
    NTattr      : DWORD;
BEGIN
    NTattr := 0;
    IF Archive IN attr THEN
        NTattr := NTattr + FILE_ATTRIBUTE_ARCHIVE;
    END;
    IF Hidden IN attr THEN
        NTattr := NTattr + FILE_ATTRIBUTE_HIDDEN;
    END;
    IF ReadOnly IN attr THEN
        NTattr := NTattr + FILE_ATTRIBUTE_READONLY;
    END;
    IF System IN attr THEN
        NTattr := NTattr + FILE_ATTRIBUTE_SYSTEM;
    END;
    IF Compressed IN attr THEN
        NTattr := NTattr + FILE_ATTRIBUTE_COMPRESSED;
    END;
    IF Temporary IN attr THEN
        NTattr := NTattr + FILE_ATTRIBUTE_TEMPORARY;
    END;
    IF Encrypted IN attr THEN
        NTattr := NTattr + FILE_ATTRIBUTE_ENCRYPTED;
    END;
    IF Offline IN attr THEN
        NTattr := NTattr + FILE_ATTRIBUTE_OFFLINE;
    END;
    IF Directory IN attr THEN
        NTattr := NTattr + FILE_ATTRIBUTE_DIRECTORY;
    END;
    RETURN NTattr;
END GetWin32Attributes;

PROCEDURE GetFileAttr(name : ARRAY OF CHAR;
                      VAR OUT attr : FileAttributeSet) : BOOLEAN;
VAR
    res         : DWORD;
    spec        : FileSpecString;
BEGIN
    spec := name;
    spec[HIGH(spec)] := '';
    res := GetFileAttributes(spec);

    IF res <> 0FFFFFFFFh THEN
        attr := GetM2Attributes(res);
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END;
END GetFileAttr;

PROCEDURE SetFileAttr(name : ARRAY OF CHAR; attr : FileAttributeSet) : BOOLEAN;
VAR
    NTattr      : DWORD;
    res         : BOOL;
    spec        : FileSpecString;
BEGIN
    NTattr := GetWin32Attributes(attr);

    spec := name;
    spec[HIGH(spec)] := '';
    res := SetFileAttributes(spec, NTattr);
    IF res THEN
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END;
END SetFileAttr;

PROCEDURE GetFileDateTime(spec : ARRAY OF CHAR; VAR OUT dt : DateTime);
VAR
    e   : SearchEntry;
BEGIN
    dt.year := 0;

    IF FindFirst(spec, AllAttributes, MustHaveNothing, e) THEN
        dt := e.dt;
        FindClose(e);
    END;
END GetFileDateTime;

PROCEDURE GetFileDateTimeUTC(spec : ARRAY OF CHAR; VAR OUT dt : DateTime);
VAR
    e   : SearchEntry;
BEGIN
    dt.year := 0;

    IF FindFirst(spec, AllAttributes, MustHaveNothing, e) THEN
        dt := e.dtUTC;
        FindClose(e);
    END;
END GetFileDateTimeUTC;

PROCEDURE GetFileDateTimes(spec : ARRAY OF CHAR;
                           VAR OUT create, access, modify : DateTime;
                           utc : BOOLEAN);
VAR
    h           : HANDLE;
    NTsearch    : WIN32_FIND_DATA;

    PROCEDURE doTime(win32 : FILETIME; VAR OUT dt : DateTime);
    VAR
        st      : SYSTEMTIME;
        ft      : FILETIME;
    BEGIN
        IF utc THEN
            FileTimeToSystemTime(win32, st);
        ELSE
            FileTimeToLocalFileTime(win32, ft);
            FileTimeToSystemTime(ft, st);
        END;

        dt.year := st.wYear;
        dt.month := st.wMonth;
        dt.day := st.wDay;
        dt.hour := st.wHour;
        dt.minute := st.wMinute;
        dt.second := st.wSecond;
        dt.fractions := st.wMilliseconds;
        GetTimeZone(dt);
    END doTime;

BEGIN
    create.year := 0;
    access.year := 0;
    modify.year := 0;

    h := FindFirstFile(spec, NTsearch);
    IF h <> NIL THEN
        WIN32.FindClose(h);

        doTime(NTsearch.ftCreationTime, create);
        doTime(NTsearch.ftLastAccessTime, access);
        doTime(NTsearch.ftLastWriteTime, modify);
    END;
END GetFileDateTimes;

PROCEDURE SetFileDateTime(spec : ARRAY OF CHAR; dt : DateTime) : BOOLEAN;
VAR
    st          : SYSTEMTIME;
    ft          : FILETIME;
    last        : FILETIME;
    f           : File;
    success     : BOOLEAN;
BEGIN
    st.wYear := dt.year;
    st.wMonth := dt.month;
    st.wDay := dt.day;
    st.wHour := dt.hour;
    st.wMinute := dt.minute;
    st.wSecond := dt.second;
    st.wMilliseconds := dt.fractions;
    SystemTimeToFileTime(st, ft);

    LocalFileTimeToFileTime(ft, last);

    success := FALSE;
    OpenFile(f, spec, WriteOnlyDenyNone);
    IF f.status = 0 THEN
        IF NOT SetFileTime(CAST(HANDLE, f.handle),
                           NIL_FILETIME,(*created*)
                           last,(*accessed*)
                           last)(*written*)
        THEN
            f.status := GetLastError();
        END;
        success := f.status = 0;
        CloseFile(f);
    END;
    RETURN success;
END SetFileDateTime;

PROCEDURE SetFileDateTimeUTC(spec : ARRAY OF CHAR; dt : DateTime) : BOOLEAN;
VAR
    st          : SYSTEMTIME;
    ft          : FILETIME;
    f           : File;
    success     : BOOLEAN;
BEGIN
    st.wYear := dt.year;
    st.wMonth := dt.month;
    st.wDay := dt.day;
    st.wHour := dt.hour;
    st.wMinute := dt.minute;
    st.wSecond := dt.second;
    st.wMilliseconds := dt.fractions;
    SystemTimeToFileTime(st, ft);

    success := FALSE;
    OpenFile(f, spec, WriteOnlyDenyNone);
    IF f.status = 0 THEN
        IF NOT SetFileTime(CAST(HANDLE, f.handle),
                           NIL_FILETIME,(*created*)
                           ft,(*accessed*)
                           ft)(*written*)
        THEN
            f.status := GetLastError();
        END;
        success := f.status = 0;
        CloseFile(f);
    END;
    RETURN success;
END SetFileDateTimeUTC;

PROCEDURE SetFileDateTimes(spec : ARRAY OF CHAR;
                           create, access, modify : DateTime;
                           utc : BOOLEAN) : BOOLEAN;
VAR
    ftCreate,
    ftAccess,
    ftModify    : FILETIME;
    pCreate,
    pAccess,
    pModify     : POINTER TO FILETIME;
    f           : File;
    success     : BOOLEAN;

    PROCEDURE convert(dt : DateTime) : FILETIME;
    VAR
        ft      : FILETIME;
        st      : SYSTEMTIME;
    BEGIN
        st.wYear := dt.year;
        st.wMonth := dt.month;
        st.wDay := dt.day;
        st.wHour := dt.hour;
        st.wMinute := dt.minute;
        st.wSecond := dt.second;
        st.wMilliseconds := dt.fractions;
        SystemTimeToFileTime(st, ft);

        IF NOT utc THEN
            LocalFileTimeToFileTime(ft, ft);
        END;
        RETURN ft;
    END convert;

BEGIN
    success := FALSE;
    OpenFile(f, spec, WriteOnlyDenyNone);
    IF f.status = 0 THEN
        pCreate := NIL;
        pAccess := NIL;
        pModify := NIL;
        IF create.year <> 0 THEN
            ftCreate := convert(create);
            pCreate := ADR(ftCreate);
        END;
        IF access.year <> 0 THEN
            ftAccess := convert(access);
            pAccess := ADR(ftAccess);
        END;
        IF modify.year <> 0 THEN
            ftModify := convert(modify);
            pModify := ADR(ftModify);
            IF pAccess = NIL THEN
                pAccess := pModify;
            END;
        END;

        IF NOT SetFileTime(CAST(HANDLE, f.handle), pCreate^, pAccess^, pModify^) THEN
            f.status := GetLastError();
        END;
        success := f.status = 0;
        CloseFile(f);
    END;
    RETURN success;
END SetFileDateTimes;

PROCEDURE RenameFile(fromFile, toFile : ARRAY OF CHAR) : BOOLEAN;
VAR
    specF       : FileSpecString;
    specT       : FileSpecString;
BEGIN
    specF := fromFile;
    specF[HIGH(specF)] := '';
    specT := toFile;
    specT[HIGH(specT)] := '';
    IF MoveFile(specF, specT) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END RenameFile;

PROCEDURE DeleteFile(name : ARRAY OF CHAR) : BOOLEAN;
VAR
    spec        : FileSpecString;
BEGIN
    spec := name;
    spec[HIGH(spec)] := '';
    IF WIN32.DeleteFile(spec) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END DeleteFile;

PROCEDURE FileExists(name : ARRAY OF CHAR) : BOOLEAN;
VAR
    attr        : FileAttributeSet;
BEGIN
    IF GetFileAttr(name, attr) THEN
        RETURN NormalFile IN attr;
    END;
    RETURN FALSE;
END FileExists;

PROCEDURE CopyFile(source, dest : ARRAY OF CHAR) : BOOLEAN;
VAR
    srcName, dstName    : FileSpecString;
BEGIN
    srcName := source;
    srcName[HIGH(srcName)] := '';
    dstName := dest;
    dstName[HIGH(dstName)] := '';

    (* handles NTFS streams *)
    RETURN WIN32.CopyFile(srcName, dstName, FALSE);
END CopyFile;

PROCEDURE SetHandleCount(num : CARDINAL);
BEGIN
    WIN32.SetHandleCount(num);
END SetHandleCount;

PROCEDURE GetNextDir(list : ARRAY OF CHAR;
                     sepChars : ARRAY OF CHAR;
                     VAR INOUT i : CARDINAL;
                     VAR OUT item : ARRAY OF CHAR) : BOOLEAN;
VAR
    l   : ADRCARD;
BEGIN
    IF GetNextItem(list, i, item, sepChars) THEN
        l := LENGTH(item);
        IF (item[l-1] <> ':') AND
           (
            (item[l-1] <> '\') OR
            (item[l-1] <> '/')
           )
        THEN
            Append('\', item);
        END;
        RETURN TRUE;
    END;
    RETURN FALSE;
END GetNextDir;

PROCEDURE ParseFileName(path : ARRAY OF CHAR; VAR OUT parts : FileNameParts);
VAR
    i, l, s, e  : ADRINT;
BEGIN
    parts.drive := "";
    parts.path := "";
    parts.name := "";
    parts.extension := "";

    IF path[0] = '' THEN
        RETURN;
    END;

    i := 0;
    l := LENGTH(path);

    WHILE (i < l) AND (path[i] = ' ') DO
        INC(i);
    END;

    (* get the drive component *)

    s := i;
    WHILE (i < l) AND (path[i] <> ':') DO
        INC(i);
    END;
    IF i < l THEN
        parts.drive := path[s..i];
        INC(i);
    ELSE
        i := s;

        (* now check for a UNC server name *)

        IF ((path[i] = '\') OR (path[i] = '/')) AND
           (i+1 < l) AND
           ((path[i+1] = '\') OR (path[i+1] = '/'))
        THEN
            i := i + 2;
            WHILE (i < l) AND ((path[i] <> '\') AND (path[i] <> '/')) DO
                INC(i);
            END;
            INC(i);

            (* we have passed the server name *)
            (* now pass the share device name *)

            WHILE (i < l) AND ((path[i] <> '\') AND (path[i] <> '/')) DO
                INC(i);
            END;
            DEC(i);

            IF i < l THEN
                parts.drive := path[s..i];
                INC(i);
            END;
        END;
    END;

    (* get the path component *)

    s := i;
    e := i-1;
    WHILE i < l DO
        IF (path[i] = '\') OR (path[i] = '/') THEN
            e := i;
        END;
        INC(i);
    END;
    IF e >= s THEN
        parts.path := path[s..e];
        i := e+1;
    ELSE
        i := s;
    END;

    (* get the name component *)

    s := i;
    e := i-1;
    WHILE i < l DO
        IF path[i] = '.' THEN
            e := i;
        END;
        INC(i);
    END;
    IF e > s THEN
        parts.name := path[s..e-1];
        i := e;
    ELSIF (e < s) AND (s < l) THEN
        parts.name := path[s..i-1];
    ELSE
        i := s;
    END;

    (* get the extension component *)

    IF i < l THEN
        IF l-1 > i THEN
            parts.extension := path[i..l-1];
        ELSE
            (* extensions are not a single . *)
            Append(".", parts.name);
        END;
    END;
END ParseFileName;

PROCEDURE ParseFileNameEx(path : ARRAY OF CHAR;
                          VAR OUT parts : FileNameParts;
                          list : ARRAY OF CHAR;
                          sepChars : ARRAY OF CHAR);
VAR
    item        : NameString;
    i           : CARDINAL;
BEGIN
    ParseFileName(path, parts);
    IF parts.extension[0] <> '' THEN
        i := 0;
        WHILE GetNextItem(list, i, item, sepChars) DO
            IF EqualI(item, parts.extension[1..HIGH(parts.extension)]) THEN
                RETURN;
            END;
        END;
        Append(parts.extension, parts.name);
        parts.extension := "";
    END;
END ParseFileNameEx;

PROCEDURE AssembleParts(parts : FileNameParts; VAR OUT name : ARRAY OF CHAR);
BEGIN
    Concat(parts.drive, parts.path, name);
    Append(parts.name, name);
    Append(parts.extension, name);
END AssembleParts;

PROCEDURE ConstructFileName(pri, def : ARRAY OF CHAR;
                            VAR OUT res : ARRAY OF CHAR);
VAR
    priParts    : FileNameParts;
    defParts    : FileNameParts;
    i           : ADRCARD;
    highRes     : ADRCARD;

    PROCEDURE copy(part : ARRAY OF CHAR);
    VAR
        j               : ADRCARD;
        highPart        : ADRCARD;
        highRes         : ADRCARD;
        c               : CHAR;
    BEGIN
        highPart := HIGH(part);
        highRes := HIGH(res);
        FOR j := 0 TO highPart DO
            c := part[j];
            IF c = '' THEN
                RETURN;
            END;
            IF i <= highRes THEN
                res[i] := c;
                i := i + 1;
            END;
        END;
    END copy;

BEGIN
    ParseFileName(pri, priParts);
    ParseFileName(def, defParts);

    i := 0;

    IF priParts.drive[0] <> '' THEN
        copy(priParts.drive);
    ELSE
        copy(defParts.drive);
    END;

    IF priParts.path[0] <> '' THEN
        copy(priParts.path);
    ELSE
        copy(defParts.path);
    END;

    IF priParts.name[0] <> '' THEN
        copy(priParts.name);
    ELSE
        copy(defParts.name);
    END;

    IF priParts.extension[0] <> '' THEN
        copy(priParts.extension);
    ELSE
        copy(defParts.extension);
    END;

    highRes := HIGH(res);
    IF i <= highRes THEN
        res[i] := '';
    END;
END ConstructFileName;

PROCEDURE ConstructFileNameEx(pri, def : ARRAY OF CHAR;
                              VAR OUT res : ARRAY OF CHAR;
                              list : ARRAY OF CHAR;
                              sepChars : ARRAY OF CHAR);
VAR
    priParts    : FileNameParts;
    defParts    : FileNameParts;
    i           : ADRCARD;
    highRes     : ADRCARD;

    PROCEDURE copy(part : ARRAY OF CHAR);
    VAR
        j               : ADRCARD;
        highPart        : ADRCARD;
        highRes         : ADRCARD;
        c               : CHAR;
    BEGIN
        highPart := HIGH(part);
        highRes := HIGH(res);
        FOR j := 0 TO highPart DO
            c := part[j];
            IF c = '' THEN
                RETURN;
            END;
            IF i <= highRes THEN
                res[i] := c;
                i := i + 1;
            END;
        END;
    END copy;

BEGIN
    ParseFileNameEx(pri, priParts, list, sepChars);
    ParseFileNameEx(def, defParts, list, sepChars);

    i := 0;

    IF priParts.drive[0] <> '' THEN
        copy(priParts.drive);
    ELSE
        copy(defParts.drive);
    END;

    IF priParts.path[0] <> '' THEN
        copy(priParts.path);
    ELSE
        copy(defParts.path);
    END;

    IF priParts.name[0] <> '' THEN
        copy(priParts.name);
    ELSE
        copy(defParts.name);
    END;

    IF priParts.extension[0] <> '' THEN
        copy(priParts.extension);
    ELSE
        copy(defParts.extension);
    END;

    highRes := HIGH(res);
    IF i <= highRes THEN
        res[i] := '';
    END;
END ConstructFileNameEx;

PROCEDURE FindInPathList(fileName, pathList : ARRAY OF CHAR;
                         sepChars : ARRAY OF CHAR;
                         VAR OUT result : ARRAY OF CHAR;
                         searchCurrent : BOOLEAN) : BOOLEAN;
VAR
    i           : CARDINAL;
    workStr     : FileSpecString;
BEGIN
    IF pathList[0] = '' THEN
        searchCurrent := TRUE;
    END;

    IF searchCurrent AND FileExists(fileName) THEN
        result := fileName;
        RETURN TRUE;
    END;

    i := 0;
    WHILE GetNextDir(pathList, sepChars, i, workStr) DO
        Append(fileName, workStr);
        IF FileExists(workStr) THEN
            result := workStr;
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END FindInPathList;

PROCEDURE FindInOSPathList(fileName : ARRAY OF CHAR;
                           VAR OUT result : ARRAY OF CHAR) : BOOLEAN;
VAR
    path        : ARRAY [0..1023] OF CHAR;
BEGIN
    IF GetSymbol("PATH", path) THEN
        IF FindInPathList(fileName, path, ";", result, TRUE) THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END FindInOSPathList;

PROCEDURE FileSpecIsAbsolute (spec : ARRAY OF CHAR) : BOOLEAN;
BEGIN
	CASE spec[0] OF
	| '/', '\'			 : RETURN TRUE;
	| "A".."Z", "a".."z" : RETURN spec[1] = ":";
	ELSE				   RETURN FALSE;
	END;
END FileSpecIsAbsolute;

PROCEDURE ExpandFileSpec(VAR INOUT spec : ARRAY OF CHAR);
VAR
    result      : FileSpecString;
    addr        : LPTSTR;
    (*i, j, k, l  : CARDINAL;*)
BEGIN
    result := "";
    spec[HIGH(spec)] := '';
    GetFullPathName(spec, HIGH(result)+1, result, addr);
	(* Simplify paths with \\?\ or \\?\UNC\ *)
	IF (spec[0] = "\") & (spec[1] = "\") & (spec[2] = "?") THEN
		IF (CAP(spec[4]) = "U") & (CAP(spec[5]) = "N") & (CAP(spec[6]) = "C") THEN
			Delete (spec, 2, 6);
		ELSIF spec[5] = ":" THEN
			Delete (spec, 0, 4);
		ELSE
			Delete (spec, 2, 2);
		END;
	END;
(*
    (* now strip the . and .. directory names from the result *)

    l := LENGTH(result);
    i := 0;
    LOOP
        WHILE (i < l) AND (result[i] <> '\') AND (result[i] <> '/') DO
            INC(i);
        END;

        IF i < l THEN
            j := i;
            INC(j);
            IF (j < l) AND (result[j] = '.') THEN
                INC(j);
                IF (j < l) AND (result[j] = '.') THEN
                    INC(j);

                    k := i;
                    DEC(k);
                    WHILE (k > 0) AND
                          (result[k] <> '\') AND
                          (result[k] <> '/')
                    DO
                        DEC(k);
                    END;
                    IF k <= 1 THEN
                        k := 2;
                    END;
                    i := k;
                END;

                Delete(result, i, j-i);
                l := l - (j-i);
            ELSE
                INC(i);
            END;
        ELSE
            EXIT;
        END;
    END;
*)
    spec := result;
END ExpandFileSpec;

PROCEDURE MakeFileSpecRelative (BasePath : ARRAY OF CHAR; VAR INOUT spec : ARRAY OF CHAR) : BOOLEAN;
VAR
    Base, Rel	: FileSpecString;
    i, j, k		: CARDINAL;
BEGIN
	Base := BasePath;
	ExpandFileSpec (Base);
	Rel := spec;
	ExpandFileSpec (Rel);
	IF (Base[0] = "\") & (Base[1] = "\") THEN
		(* First case : both specifications start with a server name *)
		IF (Rel[0] = "\") OR (Rel[1] # "\") THEN RETURN FALSE END;
		i := 2;
		j := 2;
		LOOP (* compare server names *)
			IF (Base[i] = 0C) OR (CAP(Base[i]) # CAP(Rel[i])) THEN RETURN FALSE END;
			IF Base[i] = "\" THEN EXIT END;
			INC (i);
		END;
	ELSIF Base[1] = ":" THEN
		(* Second case : both specifications start with a device *)
		IF (CAP(Base[0]) # CAP(Rel[0])) OR (Rel[1] # ":") OR (Base[2] # "\") OR (Rel[2] # "\") THEN RETURN FALSE END;
		i := 3;
	ELSE
		RETURN FALSE;
	END;
	(* Skip equal directories *)
	j := i; (* Position after the last \ *)
	WHILE (CAP(Base[i]) = CAP(Rel[i])) & (Base[i] # 0C) DO
		IF Base[i] = "\" THEN j := i+1 END;
		INC (i);
	END;
	i := j;
	(* Count remaining directories in Base *)
	k := 0;
	WHILE Base[j] # 0C DO
		IF Base[j] = "\" THEN INC (k) END;
		INC (j);
	END;
	(* Insert starting ..\ *)
	j := 0;
	WHILE k > 0 DO
		spec[j] := ".";
		INC (j);
		spec[j] := ".";
		INC (j);
		spec[j] := "\";
		INC (j);
		DEC (k);
	END;
	(* Copy remaining letters from Rel *)
	WHILE Rel[i] # 0C DO
		spec[j] := Rel[i];
		INC (i);
		INC (j);
	END;
	spec[j] := 0C;
	RETURN TRUE;
END MakeFileSpecRelative;

PROCEDURE SetEntry(NTsearch : WIN32_FIND_DATA; VAR OUT entry : SearchEntry);
VAR
    st          : SYSTEMTIME;
    ft          : FILETIME;
BEGIN
    entry.name := NTsearch.cFileName;
    entry.size := VAL(CARDINAL64,NTsearch.nFileSizeLow) + VAL(CARDINAL64,NTsearch.nFileSizeHigh) * 100000000H;

    entry.attribute := GetM2Attributes(NTsearch.dwFileAttributes);

    FileTimeToSystemTime(NTsearch.ftLastWriteTime, st);
    entry.dtUTC.year := st.wYear;
    entry.dtUTC.month := st.wMonth;
    entry.dtUTC.day := st.wDay;
    entry.dtUTC.hour := st.wHour;
    entry.dtUTC.minute := st.wMinute;
    entry.dtUTC.second := st.wSecond;
    entry.dtUTC.fractions := st.wMilliseconds;
    GetTimeZone(entry.dtUTC);

    FileTimeToLocalFileTime(NTsearch.ftLastWriteTime, ft);
    FileTimeToSystemTime(ft, st);

    entry.dt.year := st.wYear;
    entry.dt.month := st.wMonth;
    entry.dt.day := st.wDay;
    entry.dt.hour := st.wHour;
    entry.dt.minute := st.wMinute;
    entry.dt.second := st.wSecond;
    entry.dt.fractions := st.wMilliseconds;
    GetTimeZone(entry.dt);

    (* don't know if Win32 uses these, save them anyway *)

    entry.reserved0 := NTsearch.dwReserved0;
    entry.reserved1 := NTsearch.dwReserved1;
END SetEntry;

PROCEDURE FindFirst(path : ARRAY OF CHAR;
                    mayHave : FileAttributeSet;
                    mustHave : FileAttributeSet;
                    VAR OUT entry : SearchEntry) : BOOLEAN;
VAR
    NTsearch    : WIN32_FIND_DATA;
    spec        : FileSpecString;
    found       : BOOLEAN;
BEGIN
    entry.mayHave := mayHave + mustHave;
    IF NOT (Directory IN mayHave) THEN
        INCL(entry.mayHave, NormalFile);
    END;
    entry.mustHave := mustHave;

    IF (
        (mustHave * FileAttributeSet{NormalFile, Directory})
         =
         FileAttributeSet{NormalFile, Directory}
       )
    THEN
        RETURN FALSE;
    END;

    spec := path;
    spec[HIGH(spec)] := '';

    entry.findHandle := FindFirstFile(spec, NTsearch);
    IF entry.findHandle <> INVALID_HANDLE_VALUE THEN

        %IF PROTECT %THEN
            CreateCriticalSection(entry.critic);
        %END

        SetEntry(NTsearch, entry);

        IF (NOT Equal(entry.name, ".")) AND (NOT Equal(entry.name, "..")) THEN
            IF entry.attribute >= entry.mustHave THEN
                IF entry.attribute <= entry.mayHave THEN
                    RETURN TRUE;
                END;
            END;
        END;

        found := FindNext(entry);
        IF NOT found THEN
            FindClose(entry);
        END;
        RETURN found;
    END;
    RETURN FALSE;
END FindFirst;

PROCEDURE FindNext(VAR INOUT entry : SearchEntry) : BOOLEAN;
VAR
    NTsearch    : WIN32_FIND_DATA;
    found       : BOOLEAN;
BEGIN
    %IF PROTECT %THEN
        EnterCriticalSection(entry.critic);
    %END

    (* restore the unknown fields *)

    NTsearch.dwReserved0 := entry.reserved0;
    NTsearch.dwReserved1 := entry.reserved1;

    found := FALSE;
    WHILE (NOT found) AND FindNextFile(entry.findHandle, NTsearch) DO

        SetEntry(NTsearch, entry);

        IF (NOT Equal(entry.name, ".")) AND (NOT Equal(entry.name, "..")) THEN
            IF entry.attribute >= entry.mustHave THEN
                IF entry.attribute <= entry.mayHave THEN
                    found := TRUE;
                END;
            END;
        END;
    END;

    (* don't know if NT uses these, save them anyway *)

    entry.reserved0 := NTsearch.dwReserved0;
    entry.reserved1 := NTsearch.dwReserved1;

    %IF PROTECT %THEN
        LeaveCriticalSection(entry.critic);
    %END
    RETURN found;
END FindNext;

PROCEDURE FindClose(VAR INOUT entry : SearchEntry);
BEGIN
    %IF PROTECT %THEN
        CloseCriticalSection(entry.critic);

    %END
    WIN32.FindClose(entry.findHandle);
END FindClose;

PROCEDURE MakeDir(name : ARRAY OF CHAR) : BOOLEAN;
VAR
    spec        : FileSpecString;
    l           : ADRCARD;
BEGIN
    spec := name;
    spec[HIGH(spec)] := '';
    l := LENGTH(spec);
    IF (spec[l-1] = '\') OR (spec[l-1] = '/') THEN
        spec[l-1] := '';
    END;

    IF CreateDirectory(spec, NIL_SECURITY_ATTRIBUTES) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END MakeDir;

PROCEDURE CreateDirTree(fileSpec : ARRAY OF CHAR) : BOOLEAN;
VAR
    i           : ADRCARD;
    l           : ADRCARD;
    parts       : FileNameParts;
    newPath     : FileSpecString;
BEGIN
    IF fileSpec[0] <> '' THEN
        ParseFileName(fileSpec, parts);

        l := LENGTH(parts.path);
        i := 1;
        WHILE i < l DO
            IF (parts.path[i] = '\') OR (parts.path[i] = '/') THEN
                Concat(parts.drive, parts.path[0..i-1], newPath);
                IF NOT DirExists(newPath) THEN
                    IF NOT MakeDir(newPath) THEN
                        RETURN FALSE;
                    END;
                END;
            END;
            INC(i);
        END;
    END;
    RETURN TRUE;
END CreateDirTree;

PROCEDURE DeleteDir(name : ARRAY OF CHAR) : BOOLEAN;
VAR
    spec        : FileSpecString;
    l           : ADRCARD;
BEGIN
    spec := name;
    spec[HIGH(spec)] := '';
    IF spec[0] <> '' THEN
        l := LENGTH(spec);
        IF (spec[l-1] = '\') OR (spec[l-1] = '/') THEN
            spec[l-1] := '';
        END;
        IF RemoveDirectory(spec) THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END DeleteDir;

PROCEDURE DirExists(name : ARRAY OF CHAR) : BOOLEAN;
VAR
    e           : SearchEntry;
    lName       : FileSpecString;
    l           : ADRCARD;
BEGIN
    lName := name;
    lName[HIGH(lName)] := '';
    l := LENGTH(lName);
    IF l <> 0 THEN
        IF (lName[l-1] = '\') OR (lName[l-1] = '/') THEN
            lName[l-1] := '';
        END;
        IF FindFirst(lName, AllAttributes, MustHaveDirectory, e) THEN
            FindClose(e);
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
(*
VAR
    attr        : FileAttributeSet;
    lName       : FileSpecString;
    l           : CARDINAL;
BEGIN
    lName := name;
    lName[HIGH(lName)] := '';
    l := LENGTH(lName);
    IF l <> 0 THEN
        IF (lName[l-1] = '\') OR (lName[l-1] = '/') THEN
            lName[l-1] := '';
        END;
        IF GetFileAttr(name, attr) THEN
            RETURN Directory IN attr;
        END;
    END;
    RETURN FALSE;
    *)
END DirExists;

PROCEDURE RenameDir(fromDir, toDir : ARRAY OF CHAR) : BOOLEAN;
VAR
    specF       : FileSpecString;
    specT       : FileSpecString;
    l           : ADRCARD;
BEGIN
    specF := fromDir;
    specF[HIGH(specF)] := '';
    l := LENGTH(specF);
    IF (specF[l-1] = '\') OR (specF[l-1] = '/') THEN
        specF[l-1] := '';
    END;

    specT := toDir;
    specT[HIGH(specT)] := '';
    l := LENGTH(specT);
    IF (specT[l-1] = '\') OR (specT[l-1] = '/') THEN
        specT[l-1] := '';
    END;

    IF MoveFile(specF, specT) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END RenameDir;

PROCEDURE GetDefaultPath(VAR OUT path : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    IF GetCurrentDirectory(HIGH(path)+1, path) <> 0 THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END GetDefaultPath;

PROCEDURE SetDefaultPath(path : ARRAY OF CHAR) : BOOLEAN;
VAR
    spec        : FileSpecString;
BEGIN
    spec := path;
    spec[HIGH(spec)] := '';
    IF SetCurrentDirectory(spec) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END SetDefaultPath;

PROCEDURE GetPath(VAR spec : ARRAY OF CHAR);
VAR
    parts       : FileNameParts;
BEGIN
    ConstructFileName(spec, "foo.foo", spec);
    ExpandFileSpec(spec);
    ParseFileName(spec, parts);
    parts.name := "";
    parts.extension := "";
    AssembleParts(parts, spec);
END GetPath;

PROCEDURE GetExSize(spec : ARRAY OF CHAR; VAR OUT avail : LONGCARD) : BOOLEAN;
TYPE
    spaceExProc = PROCEDURE(NOHIGH ARRAY OF CHAR,
                            VAR ULARGE_INTEGER,
                            VAR ULARGE_INTEGER,
                            VAR ULARGE_INTEGER) : BOOL [WINDOWS];
VAR
    lib                 : HINSTANCE;
    addr                : ADDRESS;
    getSpace            : spaceExProc;
    availToCaller       : ULARGE_INTEGER;
    totalAvail          : ULARGE_INTEGER;
    totalSpace          : ULARGE_INTEGER;
    temp                : FileSpecString;
BEGIN
    temp := spec;
    temp[HIGH(temp)] := '';
    GetPath(temp);

    lib := CAST(HINSTANCE, GetModuleHandle("kernel32.dll"));
    IF SIZE(CHAR) = 2 THEN
        addr := CAST(ADDRESS, GetProcAddress(lib, "GetDiskFreeSpaceExW"));
    ELSE
        addr := CAST(ADDRESS, GetProcAddress(lib, "GetDiskFreeSpaceExA"));
    END;

    IF addr <> NIL THEN
        getSpace := CAST(spaceExProc, addr);
        IF getSpace(spec, availToCaller, totalSpace, totalAvail) THEN
            avail := availToCaller;
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END GetExSize;

PROCEDURE GetDeviceFreeSpace(spec : ARRAY OF CHAR) : CARDINAL32;
VAR
    result      : LONGINT;
    gran        : CARDINAL;
BEGIN
    result := GetDeviceFreeSpaceEx(spec, gran);
    IF result > MAX(CARDINAL32) THEN
        result := MAX(CARDINAL32);
    END;
    RETURN result;
END GetDeviceFreeSpace;

PROCEDURE GetDeviceFreeSpaceEx(spec : ARRAY OF CHAR;
                           VAR OUT allocGranularity : CARDINAL32) : LONGCARD;
VAR
    sectorsPerCluster   : CARDINAL32;
    bytesPerSector      : CARDINAL32;
    freeClusters        : CARDINAL32;
    totalClusters       : CARDINAL32;
    result              : LONGCARD;
    ok                  : BOOLEAN;
    temp                : FileSpecString;
BEGIN
    allocGranularity := 0;
    result := 0;

    temp := spec;
    temp[HIGH(temp)] := '';
    GetRootDirectory(temp);

    ok := GetDiskFreeSpace(temp,
                           sectorsPerCluster,
                           bytesPerSector,
                           freeClusters,
                           totalClusters);
    IF ok THEN
        allocGranularity := sectorsPerCluster * bytesPerSector;
    END;

    IF NOT GetExSize(spec, result) THEN
        IF ok THEN
            result := VAL(LONGCARD, sectorsPerCluster) *
                      VAL(LONGCARD, bytesPerSector) *
                      VAL(LONGCARD, freeClusters);
        END;
    END;
    RETURN result;
END GetDeviceFreeSpaceEx;

PROCEDURE GetDeviceType(spec : ARRAY OF CHAR) : DeviceTypes;
VAR
    temp        : FileSpecString;
BEGIN
    temp := spec;
    temp[HIGH(temp)] := '';
    GetRootDirectory(temp);

    CASE WIN32.GetDriveType(temp) OF
    DRIVE_FIXED:
        RETURN DeviceFixedDisk;
    |
    DRIVE_RAMDISK:
        RETURN DeviceRamdisk;
    |
    DRIVE_REMOTE:
        RETURN DeviceRemote;
    |
    DRIVE_REMOVABLE, DRIVE_CDROM:
        RETURN DeviceRemovable;
    ELSE
        RETURN DeviceUnknown;
    END;
END GetDeviceType;

END FileFunc.
