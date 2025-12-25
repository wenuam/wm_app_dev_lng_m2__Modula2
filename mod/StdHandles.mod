(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE StdHandles;
<*/NOWARN:F*>

%IF Windows %THEN

FROM SYSTEM IMPORT
    CAST, ADRCARD;

FROM WIN32 IMPORT
    STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE, GetStdHandle,
    GetFileType, FILE_TYPE_CHAR, HANDLE, INVALID_HANDLE_VALUE;

FROM WINX IMPORT
    NULL_HANDLE;

%ELSIF UNIX %THEN

FROM SYSTEM IMPORT
    ADRCARD;

FROM UNIX IMPORT
    STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO,
    stat_t, fstat, S_ISCHR;

%END

PROCEDURE HaveStdInput() : BOOLEAN;
%IF Windows %THEN
VAR
    h   : HANDLE;
%END
BEGIN
%IF Windows %THEN
    h := GetStdHandle(STD_INPUT_HANDLE);
    RETURN (h <> INVALID_HANDLE_VALUE) AND (h <> NULL_HANDLE);
%ELSE
    RETURN TRUE;
%END
END HaveStdInput;

PROCEDURE StdInputHandle() : ADRCARD;
BEGIN
    %IF Windows %THEN
        RETURN CAST(ADRCARD, GetStdHandle(STD_INPUT_HANDLE));
    %ELSIF UNIX %THEN
        RETURN STDIN_FILENO;
    %ELSE
        fix me
    %END
END StdInputHandle;

PROCEDURE HaveStdOutput() : BOOLEAN;
%IF Windows %THEN
VAR
    h   : HANDLE;
%END
BEGIN
%IF Windows %THEN
    h := GetStdHandle(STD_OUTPUT_HANDLE);
    RETURN (h <> INVALID_HANDLE_VALUE) AND (h <> NULL_HANDLE);
%ELSE
    RETURN TRUE;
%END
END HaveStdOutput;

PROCEDURE StdOutputHandle() : ADRCARD;
BEGIN
    %IF Windows %THEN
        RETURN CAST(ADRCARD, GetStdHandle(STD_OUTPUT_HANDLE));
    %ELSIF UNIX %THEN
        RETURN STDOUT_FILENO;
    %ELSE
        fix me
    %END
END StdOutputHandle;

PROCEDURE HaveStdError() : BOOLEAN;
%IF Windows %THEN
VAR
    h   : HANDLE;
%END
BEGIN
%IF Windows %THEN
    h := GetStdHandle(STD_ERROR_HANDLE);
    RETURN (h <> INVALID_HANDLE_VALUE) AND (h <> NULL_HANDLE);
%ELSE
    RETURN TRUE;
%END
END HaveStdError;

PROCEDURE StdErrorHandle() : ADRCARD;
BEGIN
    %IF Windows %THEN
        RETURN CAST(ADRCARD, GetStdHandle(STD_ERROR_HANDLE));
    %ELSIF UNIX %THEN
        RETURN STDERR_FILENO;
    %ELSE
        fix me
    %END
END StdErrorHandle;

%IF Windows %THEN

PROCEDURE IsConsole(h : ADRCARD) : BOOLEAN;
BEGIN
    RETURN GetFileType(CAST(HANDLE, h)) = FILE_TYPE_CHAR;
END IsConsole;

%ELSIF UNIX %THEN

PROCEDURE IsConsole(h : ADRCARD) : BOOLEAN;
VAR
    info        : stat_t;
BEGIN
    IF fstat(h, info) >= 0 THEN
        RETURN S_ISCHR(info.st_mode);
    END;
    RETURN FALSE;
END IsConsole;

%END

END StdHandles.
