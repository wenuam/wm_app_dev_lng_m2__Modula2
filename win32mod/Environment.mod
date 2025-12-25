(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE Environment;

FROM SYSTEM IMPORT
    ADRCARD;

FROM WIN32 IMPORT
    LPTSTR,
    GetEnvironmentVariable, GetModuleFileName,
    HeapAlloc, HeapFree, GetProcessHeap;
IMPORT WIN32;

FROM WINX IMPORT
    NULL_HANDLE;

PROCEDURE GetSymbol(name : ARRAY OF CHAR;
                    VAR OUT result : ARRAY OF CHAR) : BOOLEAN;
VAR
    lName       : ARRAY [0..127] OF CHAR;
    dummy       : ARRAY [0..15] OF CHAR;
    count       : CARDINAL;
    ptr         : POINTER TO ARRAY [0..0] OF CHAR;
BEGIN
    lName := name;
    lName[HIGH(lName)] := '';

    (* first check for existence without changing result *)

    dummy := "";
    count := GetEnvironmentVariable(lName, dummy, HIGH(dummy) + 1);
    IF count <> 0 THEN
        IF count <= HIGH(result)+1 THEN
            (* it exsits, now copy to result *)
            count := GetEnvironmentVariable(lName, result, HIGH(result) + 1);
            RETURN TRUE;
        ELSE
            (* need to simulate our defined API  *)
            (* GetEnvironmentVaraible does not copy anything unless *)
            (* it all fits *)
            ptr := HeapAlloc(GetProcessHeap(), 0, (count+1)*SIZE(CHAR));
            count := GetEnvironmentVariable(lName, ptr^, count+1);
            result := ptr^[0..HIGH(result)];
            IF NOT HeapFree(GetProcessHeap(), 0, ptr) THEN
            END;
        END;
    END;
    RETURN FALSE;
END GetSymbol;

PROCEDURE GetCommandLine(VAR OUT commandLine : ARRAY OF CHAR);
VAR
    p           : LPTSTR;
    i           : ADRCARD;
    j           : ADRCARD;
    highComline : ADRCARD;
BEGIN
    p := WIN32.GetCommandLine();
    i := 0;

    commandLine[0] := '';

    (* skip leading *)

    WHILE (p^[i] <> '') AND (p^[i] = ' ') DO
        INC(i);
    END;

    IF p^[i] = '"' THEN
        (* the program filespec may have quotes because of *)
        (* spaces somewhere in the filespec *)

        INC(i);
        WHILE (p^[i] <> '') AND (p^[i] <> '"') DO
            INC(i);
        END;
        IF p^[i] = '"' THEN
            INC(i);
        END;
    ELSE
        WHILE (p^[i] <> '') AND (p^[i] <> ' ') DO
            INC(i);
        END;
    END;

    WHILE (p^[i] <> '') AND (p^[i] = ' ') DO
        INC(i);
    END;

    j := 0;
    highComline := HIGH(commandLine);
    WHILE (p^[i] <> '') AND (j <= highComline) DO
        commandLine[j] := p^[i];
        INC(i);
        INC(j);
    END;

    IF j <= highComline THEN
        commandLine[j] := '';
    END;
END GetCommandLine;

(* Get the name of the currently executing program *)

PROCEDURE GetProgramName(VAR OUT name : ARRAY OF CHAR);
BEGIN
    IF GetModuleFileName(NULL_HANDLE, name, SIZE(name)/SIZE(CHAR)) = 0 THEN
        name := "";
    END;
END GetProgramName;

END Environment.
