(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE NamedPipes;
<*/NOWARN:F*>

FROM SYSTEM IMPORT
    ADDRESS, ADR;

FROM Strings IMPORT
    Concat, Append;

FROM ExStorage IMPORT
    ALLOCATE, DEALLOCATE, DeallocateEx, GetHeap, HeapInfoPointer;

FROM FileFunc IMPORT
    FileSpecString;

FROM WIN32 IMPORT
    DWORD, HANDLE, SECURITY_ATTRIBUTES, SECURITY_DESCRIPTOR,
    INVALID_HANDLE_VALUE, SECURITY_DESCRIPTOR_REVISION,
    GENERIC_READ, GENERIC_WRITE, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,
    PIPE_TYPE_BYTE, PIPE_TYPE_MESSAGE,
    PIPE_WAIT, PIPE_READMODE_BYTE, PIPE_READMODE_MESSAGE, PIPE_ACCESS_DUPLEX,
    PIPE_UNLIMITED_INSTANCES, NMPWAIT_USE_DEFAULT_WAIT,
    InitializeSecurityDescriptor, SetSecurityDescriptorDacl,
    CreateNamedPipe, CloseHandle, GetLastError, ConnectNamedPipe,
    FlushFileBuffers, DisconnectNamedPipe, WaitNamedPipe,
    CreateFile, WriteFile, ReadFile, SetNamedPipeHandleState, CallNamedPipe;
IMPORT WIN32;

IMPORT WINERROR, WINX;

TYPE
    NamedPipe   = POINTER TO NamedPipeRecord;
    NamedPipeRecord =
        RECORD
        handle          : HANDLE;
        heap            : HeapInfoPointer;
        error           : CARDINAL;
        numInstances    : CARDINAL;
        readMode        : NamedPipeReadMode;
        typ             : NamedPipeType;
        server          : BOOLEAN;
        END;

PROCEDURE TranslateError(error : CARDINAL) : NamedPipeStatus;
BEGIN
    CASE error OF
    0,
    WINERROR.ERROR_PIPE_CONNECTED:
        RETURN NpSuccess;
    |
    WINERROR.ERROR_FILE_NOT_FOUND:
        RETURN NpPipeNotFound;
    |
    WINERROR.ERROR_MORE_DATA:
        RETURN NpMoreData;
    |
    WINERROR.ERROR_PIPE_BUSY:
        RETURN NpPipeBusy;
    |
    WINERROR.ERROR_BROKEN_PIPE:
        RETURN NpBrokenPipe;
    |
    WINERROR.ERROR_ACCESS_DENIED,
    WINERROR.ERROR_SHARING_VIOLATION:
        RETURN NpAccessDenied;
    ELSE
        RETURN NpError;
    END;
END TranslateError;

PROCEDURE ServerCreateNamedPipe(VAR OUT np : NamedPipe;
                                pipeName : ARRAY OF CHAR;
                                typ : NamedPipeType;
                                readMode : NamedPipeReadMode;
                                numInstances : CARDINAL;
                                bufSize : CARDINAL) : BOOLEAN;
VAR
    tempSpec    : FileSpecString;
    winType     : DWORD;
    h           : HANDLE;

    secAttr     : SECURITY_ATTRIBUTES;
    secDesc     : SECURITY_DESCRIPTOR;
BEGIN
    (* provide full access to this pipe *)

    InitializeSecurityDescriptor(secDesc, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(secDesc, TRUE, NIL, FALSE);

    secAttr.nLength := SIZE(secAttr);
    secAttr.lpSecurityDescriptor := ADR(secDesc);
    secAttr.bInheritHandle := FALSE;

    Concat("\\.\PIPE\", pipeName, tempSpec);

    CASE typ OF
    NpBytePipe:
        winType := PIPE_TYPE_BYTE;
    |
    NpMessagePipe:
        winType := PIPE_TYPE_MESSAGE;
    END;
    CASE readMode OF
    NpReadBytes:
        winType := winType BOR PIPE_READMODE_BYTE;
    |
    NpReadMessages:
        winType := winType BOR PIPE_READMODE_MESSAGE;
    END;

    winType := winType BOR PIPE_WAIT;

    IF numInstances = 0 THEN
        RETURN FALSE;
    ELSIF numInstances > PIPE_UNLIMITED_INSTANCES THEN
        numInstances := PIPE_UNLIMITED_INSTANCES;
    END;

    h := CreateNamedPipe(tempSpec,
                         PIPE_ACCESS_DUPLEX,
                         winType,
                         numInstances,
                         bufSize,
                         bufSize,
                         0,             (* default client timeout *)
                         secAttr);

    IF h <> INVALID_HANDLE_VALUE THEN
        NEW(np);
        np^.handle := h;
        np^.server := TRUE;
        np^.typ := typ;
        np^.readMode := readMode;
        np^.numInstances := numInstances;
        np^.error := 0;
        np^.heap := GetHeap();
        RETURN TRUE;
    END;
    RETURN FALSE;
END ServerCreateNamedPipe;

PROCEDURE ServerCloseNamedPipe(VAR INOUT np : NamedPipe);
BEGIN
    CloseHandle(np^.handle);
    DeallocateEx(np, SIZE(np^), np^.heap);
    np := NIL;
END ServerCloseNamedPipe;

PROCEDURE ServerConnectNamedPipe(np : NamedPipe) : NamedPipeStatus;
BEGIN
    IF ConnectNamedPipe(np^.handle, NIL) THEN
        np^.error := 0;
        RETURN NpSuccess;
    ELSE
        np^.error := GetLastError();
        RETURN TranslateError(np^.error);
    END;
END ServerConnectNamedPipe;

PROCEDURE ServerDisconnectNamedPipe(np : NamedPipe) : BOOLEAN;
BEGIN
    IF NOT FlushFileBuffers(np^.handle) THEN
        np^.error := GetLastError();
        RETURN FALSE;
    END;
    IF NOT DisconnectNamedPipe(np^.handle) THEN
        np^.error := GetLastError();
        RETURN FALSE;
    END;
    np^.error := 0;
    RETURN TRUE;
END ServerDisconnectNamedPipe;

PROCEDURE ClientOpenNamedPipe(VAR OUT np : NamedPipe;
                              serverName : ARRAY OF CHAR;
                              pipeName : ARRAY OF CHAR;
                              readMode : NamedPipeReadMode
                             ) : NamedPipeStatus;
VAR
    h           : HANDLE;
    winMode     : DWORD;
    tempSpec    : FileSpecString;
    secAttr     : SECURITY_ATTRIBUTES;
    error       : CARDINAL;
BEGIN
    secAttr.nLength := SIZE(secAttr);
    secAttr.lpSecurityDescriptor := NIL;
    secAttr.bInheritHandle := FALSE;

    Concat(serverName, "\PIPE\", tempSpec);
    Append(pipeName, tempSpec);

    h := CreateFile(tempSpec,
                    GENERIC_READ BOR GENERIC_WRITE,
                    0,
                    secAttr,
                    OPEN_EXISTING,
                    FILE_ATTRIBUTE_NORMAL,
                    NIL);
    error := GetLastError();

    IF h <> INVALID_HANDLE_VALUE THEN
        NEW(np);
        np^.handle := h;
        np^.server := FALSE;
        np^.readMode := readMode;
        np^.error := 0;
        np^.heap := GetHeap();

        CASE readMode OF
        NpReadBytes:
            winMode := PIPE_READMODE_BYTE;
        |
        NpReadMessages:
            winMode := PIPE_READMODE_MESSAGE;
        END;
        winMode := winMode BOR PIPE_WAIT;

        IF SetNamedPipeHandleState(np^.handle,
                                   winMode,
                                   WINX.NIL_DWORD,
                                   WINX.NIL_DWORD)
        THEN
            RETURN NpSuccess;
        END;
        error := GetLastError();
        CloseHandle(np^.handle);
        DISPOSE(np);
    END;
    RETURN TranslateError(error);
END ClientOpenNamedPipe;

PROCEDURE ClientCloseNamedPipe(VAR INOUT np : NamedPipe);
BEGIN
    CloseHandle(np^.handle);
    DeallocateEx(np, SIZE(np^), np^.heap);
    np := NIL;
END ClientCloseNamedPipe;

PROCEDURE ClientWaitForNamedPipe(serverName : ARRAY OF CHAR;
                                 pipeName : ARRAY OF CHAR;
                                 timeout : CARDINAL) : NamedPipeStatus;
VAR
    tempSpec    : FileSpecString;
BEGIN
    Concat(serverName, "\PIPE\", tempSpec);
    Append(pipeName, tempSpec);

    IF WaitNamedPipe(tempSpec, timeout) THEN
        RETURN NpSuccess;
    ELSE
        RETURN TranslateError(GetLastError());
    END;
END ClientWaitForNamedPipe;

PROCEDURE ClientCallNamedPipe(serverName : ARRAY OF CHAR;
                              pipeName : ARRAY OF CHAR;
                              writeData : ADDRESS;
                              sizeWrite : CARDINAL;
                              readData : ADDRESS;
                              sizeRead : CARDINAL;
                              VAR OUT numRead : CARDINAL
                             ) : NamedPipeStatus;
VAR
    tempSpec    : FileSpecString;
BEGIN
    Concat(serverName, "\PIPE\", tempSpec);
    Append(pipeName, tempSpec);

    IF CallNamedPipe(tempSpec,
                     readData,
                     sizeRead,
                     writeData,
                     sizeWrite,
                     numRead,
                     NMPWAIT_USE_DEFAULT_WAIT)
    THEN
        RETURN NpSuccess;
    ELSE
        RETURN TranslateError(GetLastError())
    END;
END ClientCallNamedPipe;

PROCEDURE WriteNamedPipe(np : NamedPipe;
                         data : ADDRESS;
                         size : CARDINAL;
                         VAR OUT numWritten : CARDINAL
                        ) : NamedPipeStatus;
BEGIN
    IF WriteFile(np^.handle, data, size, numWritten, NIL) THEN
        np^.error := 0;
        RETURN NpSuccess;
    ELSE
        np^.error := GetLastError();
        RETURN TranslateError(np^.error);
    END;
END WriteNamedPipe;

PROCEDURE ReadNamedPipe(np :  NamedPipe;
                        data : ADDRESS;
                        size : CARDINAL;
                        VAR OUT numRead : CARDINAL
                       ) : NamedPipeStatus;
BEGIN
    IF ReadFile(np^.handle, data, size, numRead, NIL) THEN
        np^.error := 0;
        RETURN NpSuccess;
    ELSE
        np^.error := GetLastError();
        RETURN TranslateError(np^.error);
    END;
END ReadNamedPipe;

PROCEDURE TransactNamedPipe(np : NamedPipe;
                            writeData : ADDRESS;
                            sizeWrite : CARDINAL;
                            readData : ADDRESS;
                            sizeRead : CARDINAL;
                            VAR OUT numRead : CARDINAL
                           ) : NamedPipeStatus;
BEGIN
    IF WIN32.TransactNamedPipe(np^.handle,
                               readData,
                               sizeRead,
                               writeData,
                               sizeWrite,
                               numRead,
                               NIL)
    THEN
        np^.error := 0;
        RETURN NpSuccess;
    ELSE
        np^.error := GetLastError();
        RETURN TranslateError(np^.error);
    END;
END TransactNamedPipe;

PROCEDURE PeekNamedPipe(np :  NamedPipe;
                        data : ADDRESS;
                        size : CARDINAL;
                        VAR OUT numRead : CARDINAL
                       ) : NamedPipeStatus;
VAR
    totalAvail  : DWORD;
    messageLeft : DWORD;
BEGIN
    IF WIN32.PeekNamedPipe(np^.handle,
                           data,
                           size,
                           numRead,
                           totalAvail,
                           messageLeft)
    THEN
        np^.error := 0;
        IF (messageLeft <> 0) OR
           (
            (numRead <> size) AND (messageLeft <> 0)
           )
        THEN
            RETURN NpMoreData;
        END;
        RETURN NpSuccess;
    ELSE
        np^.error := GetLastError();
        RETURN TranslateError(np^.error);
    END;
END PeekNamedPipe;

PROCEDURE NamedPipeIsEmpty(np : NamedPipe) : BOOLEAN;
VAR
    totalAvail  : DWORD;
    dummy1      : DWORD;
    dummy2      : DWORD;
BEGIN
    IF WIN32.PeekNamedPipe(np^.handle,
                           NIL,
                           0,
                           dummy1,
                           totalAvail,
                           dummy2)
    THEN
        np^.error := 0;
        RETURN totalAvail = 0;
    END;
    np^.error := GetLastError();
    RETURN TRUE;
END NamedPipeIsEmpty;

PROCEDURE GetOSError(np : NamedPipe) : CARDINAL;
BEGIN
    RETURN np^.error;
END GetOSError;

END NamedPipes.
