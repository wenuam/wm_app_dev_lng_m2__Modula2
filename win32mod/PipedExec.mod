(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE PipedExec;
<*/NOWARN:F*>

FROM SYSTEM IMPORT
    CAST, LOC;

FROM Strings IMPORT
    Append;

FROM FileFunc IMPORT
    FileSpecString, File, InvalidHandle, FakeFileOpen, AccessModes,
    ExpandFileSpec;

FROM ElapsedTime IMPORT
    StartTimeEx, GetTimeEx;

FROM WIN32 IMPORT
    HANDLE, SECURITY_ATTRIBUTES, DUPLICATE_SAME_ACCESS,
    CreatePipe, DuplicateHandle, GetCurrentProcess,
    PROCESS_INFORMATION, STARTUPINFO, BOOL,
    NORMAL_PRIORITY_CLASS, STILL_ACTIVE,
    CREATE_NEW_PROCESS_GROUP, DETACHED_PROCESS,
    STARTF_USESTDHANDLES, STARTF_USESHOWWINDOW,
    CreateProcess, GetExitCodeProcess, TerminateProcess, GetLastError, Sleep,
    HeapAlloc, HeapFree, GetProcessHeap;
IMPORT WIN32;

FROM WINUSER IMPORT
    SW_HIDE, SW_SHOWDEFAULT, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED;

FROM WINX IMPORT
    NULL_HANDLE, NIL_STR, NIL_SECURITY_ATTRIBUTES;

TYPE
    PipedExecHandle = POINTER TO LOC;

PROCEDURE GetPipeHandles(VAR read, write : HANDLE;
                         bufSize : CARDINAL) : BOOLEAN;
VAR
    secAttr             : SECURITY_ATTRIBUTES;
BEGIN
    secAttr.nLength := SIZE(secAttr);
    secAttr.bInheritHandle := TRUE;
    secAttr.lpSecurityDescriptor := NIL;

    IF CreatePipe(read, write, secAttr, bufSize) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END GetPipeHandles;

PROCEDURE DupHandle(h : HANDLE; inherit : BOOLEAN) : HANDLE;
VAR
    newH        : HANDLE;
BEGIN
    IF DuplicateHandle(GetCurrentProcess(), h,
                       GetCurrentProcess(), newH,
                       0, inherit, DUPLICATE_SAME_ACCESS)
    THEN
        RETURN newH;
    END;
    RETURN NULL_HANDLE;
END DupHandle;

PROCEDURE CloseHandle(h : HANDLE);
BEGIN
    IF h <> NULL_HANDLE THEN
        WIN32.CloseHandle(h);
    END;
END CloseHandle;

PROCEDURE PipedExec(VAR OUT execHandle : PipedExecHandle;
                    program, command, defaultPath : ARRAY OF CHAR;
                    VAR OUT childIn : File;
                    inBufferSize : CARDINAL;
                    VAR OUT childOut : File;
                    outBufferSize : CARDINAL;
                    flags : ExecFlagSet) : BOOLEAN;
BEGIN
    childIn.handle := InvalidHandle;
    childOut.handle := InvalidHandle;
    RETURN PipedExecEx(execHandle,
                       program, command, defaultPath,
                       childIn, inBufferSize,
                       childOut, outBufferSize,
                       flags);
END PipedExec;

PROCEDURE PipedExecEx(VAR OUT execHandle : PipedExecHandle;
                      program, command, defaultPath : ARRAY OF CHAR;
                      VAR INOUT childIn : File;
                      inBufferSize : CARDINAL;
                      VAR INOUT childOut : File;
                      outBufferSize : CARDINAL;
                      flags : ExecFlagSet) : BOOLEAN;
VAR
    inRead              : HANDLE;
    inWrite             : HANDLE;
    outRead             : HANDLE;
    outWrite            : HANDLE;
    outError            : HANDLE;
    dummy               : HANDLE;

    len                 : CARDINAL;
    params              : POINTER TO ARRAY [0..0] OF CHAR;
    progbuf             : FileSpecString;
    path                : FileSpecString;
    info                : PROCESS_INFORMATION;
    startup             : STARTUPINFO;
    stat                : BOOL;
    errorNumber         : CARDINAL;
    attribs             : CARDINAL;
BEGIN
    errorNumber := 0;
    execHandle := NIL;

    IF childIn.handle = InvalidHandle THEN
        IF GetPipeHandles(inRead, inWrite, inBufferSize) THEN
            dummy := inWrite;
            inWrite := DupHandle(inWrite, FALSE);
            IF inWrite <> NULL_HANDLE THEN
                CloseHandle(dummy);
            ELSE
                CloseHandle(inRead);
                CloseHandle(dummy);
                RETURN FALSE;
            END;
        ELSE
            RETURN FALSE;
        END;
    ELSE
        IF childIn.mode = ReadOnlyDenyNone THEN
            inRead := DupHandle(CAST(HANDLE, childIn.handle), TRUE);
            inWrite := NULL_HANDLE;
        ELSE
            RETURN FALSE;
        END;
    END;

    IF childOut.handle = InvalidHandle THEN
        IF NOT GetPipeHandles(outRead, outWrite, outBufferSize) THEN
            CloseHandle(inRead);
            CloseHandle(inWrite);
            RETURN FALSE;
        END;
    ELSE
        IF childOut.mode = WriteOnlyDenyNone THEN
            outWrite := DupHandle(CAST(HANDLE, childOut.handle), TRUE);
            outRead := NULL_HANDLE;
        ELSE
            CloseHandle(inRead);
            CloseHandle(inWrite);
            RETURN FALSE;
        END;
    END;

    outError := DupHandle(outWrite, TRUE);

    progbuf := program;
    progbuf[HIGH(progbuf)] := '';
    ExpandFileSpec(progbuf);

    path := defaultPath;
    path[HIGH(path)] := '';

    len := LENGTH(progbuf) + LENGTH(command) + (2+1+1);

    params := HeapAlloc(GetProcessHeap(), 0, len*SIZE(CHAR));

    (* surround the program with quotes in case it contains spaces *)

    params^[0] := '"';
    params^[1] := '';
    Append(progbuf, params^[0..len]);
    Append('" ', params^[0..len]);
    Append(command, params^[0..len]);

    startup.cb := SIZE(startup);
    startup.lpReserved := NIL;
    startup.lpDesktop := NIL;
    startup.lpTitle := NIL;
    startup.cbReserved2 := 0;
    startup.lpReserved2 := NIL;
    startup.dwFlags := STARTF_USESTDHANDLES BOR STARTF_USESHOWWINDOW;
    startup.hStdInput := inRead;
    startup.hStdOutput := outWrite;
    startup.hStdError := outError;

    startup.wShowWindow := SW_SHOWDEFAULT;
    IF ExecMinimized IN flags THEN
        startup.wShowWindow := SW_SHOWMINIMIZED;
    END;
    IF ExecMaximized IN flags THEN
        startup.wShowWindow := SW_SHOWMAXIMIZED;
    END;
    IF ExecHidden IN flags THEN
        startup.wShowWindow := SW_HIDE;
    END;

    attribs := NORMAL_PRIORITY_CLASS BOR CREATE_NEW_PROCESS_GROUP;
    IF ExecDetached IN flags THEN
        attribs := attribs BOR DETACHED_PROCESS;
    END;

    info.hProcess := NULL_HANDLE;

    IF path[0] = '' THEN
        stat := CreateProcess(progbuf,
                              params^,
                              NIL_SECURITY_ATTRIBUTES,(* process security *)
                              NIL_SECURITY_ATTRIBUTES,(* thread 0 security *)
                              TRUE,         (* inherit handles *)
                              attribs,
                              NIL,          (* copy our environment *)
                              NIL_STR,      (* use our default dir *)
                              startup,
                              info);
    ELSE
        stat := CreateProcess(progbuf,
                              params^,
                              NIL_SECURITY_ATTRIBUTES,(* process security *)
                              NIL_SECURITY_ATTRIBUTES,(* thread 0 security *)
                              TRUE,         (* inherit handles *)
                              attribs,
                              NIL,          (* copy our environment *)
                              path,
                              startup,
                              info);
    END;
    errorNumber := GetLastError();

    HeapFree(GetProcessHeap(), 0, params);

    IF stat THEN
        (* do not need our copies of these any more *)

        CloseHandle(inRead);
        CloseHandle(outWrite);
        CloseHandle(outError);

        IF childIn.handle = InvalidHandle THEN
            FakeFileOpen(childIn, CAST(CARDINAL, inWrite), WriteOnlyDenyNone);
        END;
        IF childOut.handle = InvalidHandle THEN
            FakeFileOpen(childOut, CAST(CARDINAL, outRead), ReadOnlyDenyNone);
        END;

        (* don't need this *)

        CloseHandle(info.hThread);

        (* save this to get the program exit code later *)

        execHandle := CAST(PipedExecHandle, info.hProcess);
        RETURN TRUE;
    (*
    ELSE
        progbuf := "Error code ";
        ExStrings.AppendNum(errorNumber, progbuf);
        WINUSER.MessageBox(WINX.NULL_HWND,
                           progbuf, "CreateProcess <> TRUE",
                           0);
                           *)
    END;

    CloseHandle(inRead);
    CloseHandle(inWrite);
    CloseHandle(outRead);
    CloseHandle(outWrite);
    CloseHandle(outError);
    RETURN FALSE;
END PipedExecEx;

PROCEDURE GetExitCode(execHandle : PipedExecHandle; timeout : INTEGER) : CARDINAL;
VAR
    code        : CARDINAL;
    stTime,
    curTime     : INTEGER;
BEGIN
    code := MAX(CARDINAL);
    stTime := StartTimeEx();
    IF execHandle <> NIL THEN
        LOOP
            GetExitCodeProcess(CAST(HANDLE, execHandle), code);
            IF code = STILL_ACTIVE THEN
                code := MAX(CARDINAL);
            END;

            curTime := GetTimeEx(stTime);
            IF (timeout >= 0) AND (curTime >= timeout) THEN
                EXIT;
            END;
            curTime := (timeout-curTime) / 4;
            IF curTime > 250 THEN
                curTime := 250;
            END;
            Sleep(curTime);
        END;
    END;
    RETURN code;
END GetExitCode;

PROCEDURE ClosePipedExec(VAR INOUT execHandle : PipedExecHandle);
BEGIN
    IF GetExitCode(execHandle, 0) = MAX(CARDINAL) THEN
        TerminateProcess(CAST(HANDLE, execHandle), 255);
    END;
    CloseHandle(CAST(HANDLE, execHandle));
    execHandle := NIL;
END ClosePipedExec;

END PipedExec.
