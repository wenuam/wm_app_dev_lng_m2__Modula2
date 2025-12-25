(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE RunProg;
<*/NOWARN:F*>

FROM WIN32 IMPORT
    CreateProcess, PROCESS_INFORMATION, GetLastError,
    NORMAL_PRIORITY_CLASS, IDLE_PRIORITY_CLASS, HIGH_PRIORITY_CLASS,
    CREATE_NEW_PROCESS_GROUP, DETACHED_PROCESS,
    CloseHandle, GetExitCodeProcess, STARTUPINFO, STARTF_USESHOWWINDOW,
    WaitForSingleObject, STILL_ACTIVE, TerminateProcess,
    HeapAlloc, HeapFree, GetProcessHeap;

FROM WINUSER IMPORT
    SW_SHOWDEFAULT, SW_SHOWMINNOACTIVE, SW_SHOWMAXIMIZED, SW_HIDE;

FROM WINX IMPORT
    NULL_HANDLE, NIL_STR, NIL_SECURITY_ATTRIBUTES;

FROM FileFunc IMPORT
    FileSpecString, ExpandFileSpec;

FROM Environment IMPORT
    GetSymbol;

FROM Strings IMPORT
    Append, Concat;

TYPE
    ExecHandle = POINTER TO ExecRec;
    ExecRec =
        RECORD
        info    : PROCESS_INFORMATION;
        END;

PROCEDURE DoIt(name, command, defaultPath : ARRAY OF CHAR;
               flags : ExecFlagSet;
               VAR OUT info : PROCESS_INFORMATION) : BOOLEAN;
VAR
    progBuf             : FileSpecString;
    params              : POINTER TO ARRAY [0..0] OF CHAR;
    len                 : CARDINAL;
    path                : FileSpecString;
    startup             : STARTUPINFO;
    attribs             : CARDINAL;
    ok                  : BOOLEAN;
BEGIN
    progBuf := name;
    progBuf[HIGH(progBuf)] := '';
    ExpandFileSpec(progBuf);

    path := defaultPath;
    path[HIGH(path)] := '';

    len := LENGTH(progBuf) + LENGTH(command) + (2+1+1);

    params := HeapAlloc(GetProcessHeap(), 0, len*SIZE(CHAR));

    (* surround the program with quotes in case it contains spaces *)

    params^[0] := '"';
    params^[1] := '';
    Append(progBuf, params^[0..len]);
    Append('" ', params^[0..len]);
    Append(command, params^[0..len]);

    startup.cb := SIZE(startup);
    startup.lpReserved := NIL;
    startup.lpDesktop := NIL;
    startup.lpTitle := NIL;
    startup.dwFlags := STARTF_USESHOWWINDOW;
    startup.dwX := 0;
    startup.dwY := 0;
    startup.dwXSize := 0;
    startup.dwYSize := 0;
    startup.cbReserved2 := 0;
    startup.lpReserved2 := NIL;

    startup.wShowWindow := SW_SHOWDEFAULT;
    IF ExecMinimized IN flags THEN
        startup.wShowWindow := SW_SHOWMINNOACTIVE;
    END;
    IF ExecMaximized IN flags THEN
        startup.wShowWindow := SW_SHOWMAXIMIZED;
    END;
    IF ExecHidden IN flags THEN
        startup.wShowWindow := SW_HIDE;
    END;

    attribs := CREATE_NEW_PROCESS_GROUP;
    IF ExecHighPriority IN flags THEN
        attribs := attribs BOR HIGH_PRIORITY_CLASS;
    ELSIF ExecNormalPriority IN flags THEN
        attribs := attribs BOR NORMAL_PRIORITY_CLASS;
    ELSIF ExecIdlePriority IN flags THEN
        attribs := attribs BOR IDLE_PRIORITY_CLASS;
    ELSE
        attribs := attribs BOR NORMAL_PRIORITY_CLASS;
    END;
    IF ExecDetached IN flags THEN
        attribs := attribs BOR DETACHED_PROCESS;
    END;

    info.hProcess := NULL_HANDLE;

    IF defaultPath[0] = '' THEN
        ok := CreateProcess(progBuf,   (* program name *)
                            params^,(* command line *)
                            NIL_SECURITY_ATTRIBUTES,(* process security *)
                            NIL_SECURITY_ATTRIBUTES,(* thread 0 security *)
                            TRUE,              (* Inherit handles *)
                            attribs,
                            NIL,               (* copy our environment *)
                            NIL_STR,   (* use our directory *)
                            startup,
                            info);
    ELSE
        ok := CreateProcess(progBuf,   (* program name *)
                            params^,(* command line *)
                            NIL_SECURITY_ATTRIBUTES,(* process security *)
                            NIL_SECURITY_ATTRIBUTES,(* thread 0 security *)
                            TRUE,              (* Inherit handles *)
                            attribs,
                            NIL,               (* copy our environment *)
                            path,
                            startup,
                            info);
    END;

    HeapFree(GetProcessHeap(), 0, params);
    RETURN ok;
END DoIt;

PROCEDURE RunProgram(name, command, defaultPath : ARRAY OF CHAR;
                     flags : ExecFlagSet;
                     VAR OUT status : CARDINAL) : BOOLEAN;
VAR
    info                : PROCESS_INFORMATION;
BEGIN
    IF DoIt(name, command, defaultPath, flags, info) THEN
        IF ExecAsync IN flags THEN
            GetExitCodeProcess(info.hProcess, status);
            CloseHandle(info.hThread);
            CloseHandle(info.hProcess);
        ELSE
            WaitForSingleObject(info.hProcess, 0FFFFFFFFh);
            GetExitCodeProcess(info.hProcess, status);
            CloseHandle(info.hThread);
            CloseHandle(info.hProcess);
        END;
        RETURN TRUE;
    END;
    status := GetLastError();
    RETURN FALSE;
END RunProgram;

PROCEDURE RunProgramEx(name, command, defaultPath : ARRAY OF CHAR;
                       flags : ExecFlagSet;
                       VAR OUT handle : ExecHandle) : BOOLEAN;
VAR
    info                : PROCESS_INFORMATION;
BEGIN
    IF (ExecAsync IN flags) AND
       DoIt(name, command, defaultPath, flags, info)
    THEN
        handle := HeapAlloc(GetProcessHeap(), 0, SIZE(handle^));
        handle^.info := info;
        RETURN TRUE;
    END;
    handle := NIL;
    RETURN FALSE;
END RunProgramEx;

PROCEDURE GetProgramExitStatus(handle : ExecHandle) : CARDINAL;
VAR
    code        : CARDINAL;
BEGIN
    code := MAX(CARDINAL);
    IF handle <> NIL THEN
        GetExitCodeProcess(handle^.info.hProcess, code);
        IF code = STILL_ACTIVE THEN
            code := MAX(CARDINAL);
        END;
    END;
    RETURN code;
END GetProgramExitStatus;

PROCEDURE TerminateProgram(VAR INOUT handle : ExecHandle);
BEGIN
    IF handle <> NIL THEN
        IF GetProgramExitStatus(handle) = MAX(CARDINAL) THEN
            TerminateProcess(handle^.info.hProcess, 0);
        END;

        CloseHandle(handle^.info.hThread);
        CloseHandle(handle^.info.hProcess);
        HeapFree(GetProcessHeap(), 0, handle);
    END;
END TerminateProgram;

PROCEDURE PerformCommand(com : ARRAY OF CHAR;
                         flags : ExecFlagSet;
                         VAR OUT status : CARDINAL) : BOOLEAN;
VAR
    comspec     : ARRAY [0..255] OF CHAR;
    command     : ARRAY [0..511] OF CHAR;
BEGIN
    IF NOT GetSymbol("COMSPEC", comspec) THEN
        comspec := "CMD.EXE";
    END;
    Concat("/C ", com, command);
    RETURN RunProgram(comspec, command, "", flags, status);
END PerformCommand;

END RunProg.
