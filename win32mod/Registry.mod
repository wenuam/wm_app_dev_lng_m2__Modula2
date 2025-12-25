(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE Registry;
<*/NOWARN:F*>

FROM SYSTEM IMPORT
    ADRCARD, LOC;

FROM Strings IMPORT
    Concat, Append, Equal;

FROM ExStrings IMPORT
    AppendCharCond;

FROM Conversions IMPORT
    IntToStr, CardToStr, StrToCard, StrToInt;

FROM FileFunc IMPORT
    FileSpecString, FileNameParts, ParseFileName;

FROM ExStorage IMPORT
    ALLOCATE, DEALLOCATE;

FROM WIN32 IMPORT
    DWORD, FILETIME, REG_DWORD,
    KEY_READ, KEY_WRITE, REG_SZ, REG_BINARY,
    REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS;

FROM WINREG IMPORT
    HKEY, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE,
    RegOpenKeyEx, RegQueryValueEx, RegSetValueEx, RegCloseKey, RegCreateKeyEx,
    RegDeleteValue, RegEnumValue, RegEnumKeyEx, RegDeleteKey;

FROM WINERROR IMPORT
    ERROR_NO_MORE_ITEMS;

FROM WINX IMPORT
    NIL_DWORD, NIL_STR, NIL_SECURITY_ATTRIBUTES;

CONST
    RegBaseTable : ARRAY RegistryBase OF HKEY =
        {HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE};
VAR
    RegBase     : HKEY;
    PathString  : ARRAY [0..255] OF CHAR;

PROCEDURE CheckRegTree(base : HKEY; name : ARRAY OF CHAR) : BOOLEAN;
VAR
    res         : DWORD;
    key         : HKEY;
    ok          : BOOLEAN;
BEGIN
    ok := TRUE;
    IF RegOpenKeyEx(base,
                    name,
                    0,
                    KEY_WRITE,
                    key) <> 0
    THEN
        ok := RegCreateKeyEx(base,
                             name,
                             0,
                             NIL_STR,
                             REG_OPTION_NON_VOLATILE,
                             KEY_ALL_ACCESS,
                             NIL_SECURITY_ATTRIBUTES,
                             key,
                             res) = 0;
    END;
    IF ok THEN
        RegCloseKey(key);
    END;
    RETURN ok;
END CheckRegTree;

PROCEDURE InitRegistry(base : RegistryBase;
                       pathStr : ARRAY OF CHAR;
                       create : BOOLEAN) : BOOLEAN;
VAR
    key         : HKEY;
BEGIN
    RegBase := RegBaseTable[base];
    PathString := pathStr;

    IF create THEN
        RETURN CheckRegTree(RegBase, PathString);
    ELSE
        IF RegOpenKeyEx(RegBase, PathString, 0, KEY_READ, key) = 0 THEN
            RegCloseKey(key);
            RETURN TRUE;
        END;
        RETURN FALSE;
    END;
END InitRegistry;

PROCEDURE ReadRegString(keyName : ARRAY OF CHAR;
                        VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
VAR
    key         : HKEY;
    ok          : BOOLEAN;
    typ         : CARDINAL;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(RegBase,
                    PathString,
                    0,
                    KEY_READ,
                    key) = 0
    THEN
        count := SIZE(str);
        ok := RegQueryValueEx(key,
                              keyName,
                              NIL_DWORD,
                              typ,
                              str,
                              count) = 0;
        RegCloseKey(key);
        RETURN ok AND (typ = REG_SZ);
    END;
    RETURN FALSE;
END ReadRegString;

PROCEDURE WriteRegString(keyName : ARRAY OF CHAR;
                         str : ARRAY OF CHAR) : BOOLEAN;
VAR
    key         : HKEY;
    ok          : BOOLEAN;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(RegBase,
                    PathString,
                    0,
                    KEY_WRITE,
                    key) = 0
    THEN
        count := (LENGTH(str) + 1) * SIZE(CHAR);
        ok := RegSetValueEx(key,
                            keyName,
                            0,
                            REG_SZ,
                            str,
                            count
                            ) = 0;

        RegCloseKey(key);
        RETURN ok;
    END;
    RETURN FALSE;
END WriteRegString;

PROCEDURE ReadRegInt(keyName : ARRAY OF CHAR;
                     VAR OUT val : INTEGER) : BOOLEAN;
VAR
    str         : ARRAY [0..15] OF CHAR;
BEGIN
    IF ReadRegString(keyName, str) THEN
        StrToInt(str, val);
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END;
END ReadRegInt;

PROCEDURE WriteRegInt(keyName : ARRAY OF CHAR;
                         val : INTEGER) : BOOLEAN;
VAR
    str         : ARRAY [0..15] OF CHAR;
BEGIN
    IF IntToStr(val, str) THEN
        IF WriteRegString(keyName, str) THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END WriteRegInt;

PROCEDURE ReadRegCard(keyName : ARRAY OF CHAR;
                      VAR OUT val : CARDINAL) : BOOLEAN;
VAR
    str         : ARRAY [0..20] OF CHAR;
BEGIN
    IF ReadRegString(keyName, str) THEN
        RETURN StrToCard(str, val);
    END;
    RETURN FALSE;
END ReadRegCard;

PROCEDURE WriteRegCard(keyName : ARRAY OF CHAR;
                          val : CARDINAL) : BOOLEAN;
VAR
    str         : ARRAY [0..15] OF CHAR;
BEGIN
    IF CardToStr(val, str) THEN
        IF WriteRegString(keyName, str) THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END WriteRegCard;

PROCEDURE ReadRegBool(keyName : ARRAY OF CHAR;
                      VAR OUT val : BOOLEAN) : BOOLEAN;
VAR
    str         : ARRAY [0..15] OF CHAR;
BEGIN
    IF ReadRegString(keyName, str) THEN
        val := Equal(str, "1");
        RETURN TRUE;
    END;
    RETURN FALSE;
END ReadRegBool;

PROCEDURE WriteRegBool(keyName : ARRAY OF CHAR;
                       val : BOOLEAN) : BOOLEAN;
VAR
    str         : ARRAY [0..15] OF CHAR;
BEGIN
    IF val THEN
        str := "1";
    ELSE
        str := "0";
    END;

    IF WriteRegString(keyName, str) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END WriteRegBool;

PROCEDURE ReadRegBin(keyName : ARRAY OF CHAR;
                     VAR OUT bin : ARRAY OF LOC) : BOOLEAN;
VAR
    key         : HKEY;
    ok          : BOOLEAN;
    typ         : CARDINAL;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(RegBase,
                    PathString,
                    0,
                    KEY_READ,
                    key) = 0
    THEN
        count := SIZE(bin);
        ok := RegQueryValueEx(key,
                              keyName,
                              NIL_DWORD,
                              typ,
                              bin,
                              count) = 0;
        RegCloseKey(key);
        <*/PUSH/NOWARN:U*>
        RETURN ok AND (count = ORD(SIZE(bin))) AND (typ = REG_BINARY);
        <*/POP*>
    END;
    RETURN FALSE;
END ReadRegBin;

PROCEDURE WriteRegBin(keyName : ARRAY OF CHAR;
                      bin : ARRAY OF LOC) : BOOLEAN;
VAR
    key         : HKEY;
    ok          : BOOLEAN;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(RegBase,
                    PathString,
                    0,
                    KEY_WRITE,
                    key) = 0
    THEN
        count := SIZE(bin);
        ok := RegSetValueEx(key,
                            keyName,
                            0,
                            REG_BINARY,
                            bin,
                            count
                            ) = 0;

        RegCloseKey(key);
        RETURN ok;
    END;
    RETURN FALSE;
END WriteRegBin;

PROCEDURE ReadRegDWord(keyName : ARRAY OF CHAR;
                        VAR OUT val : DWORD) : BOOLEAN;
VAR
    key         : HKEY;
    ok          : BOOLEAN;
    typ         : CARDINAL;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(RegBase,
                    PathString,
                    0,
                    KEY_READ,
                    key) = 0
    THEN
        count := SIZE(val);
        ok := RegQueryValueEx(key,
                              keyName,
                              NIL_DWORD,
                              typ,
                              val,
                              count) = 0;
        RegCloseKey(key);
        RETURN ok AND (typ = REG_DWORD);
    END;
    RETURN FALSE;
END ReadRegDWord;

PROCEDURE WriteRegDWord(keyName : ARRAY OF CHAR;
                         val : DWORD) : BOOLEAN;
VAR
    key         : HKEY;
    ok          : BOOLEAN;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(RegBase,
                    PathString,
                    0,
                    KEY_WRITE,
                    key) = 0
    THEN
        count := SIZE(val);
        ok := RegSetValueEx(key,
                            keyName,
                            0,
                            REG_DWORD,
                            val,
                            count
                            ) = 0;

        RegCloseKey(key);
        RETURN ok;
    END;
    RETURN FALSE;
END WriteRegDWord;

PROCEDURE RemoveRegItem(keyName : ARRAY OF CHAR);
VAR
    key         : HKEY;
BEGIN
    IF RegOpenKeyEx(RegBase,
                    PathString,
                    0,
                    KEY_WRITE,
                    key) = 0
    THEN
        RegDeleteValue(key, keyName);
        RegCloseKey(key);
    END;
END RemoveRegItem;

PROCEDURE RemoveRegDir(base : RegistryBase; keyName : ARRAY OF CHAR);

    PROCEDURE deleteKey(base : HKEY; path, keyName : ARRAY OF CHAR);
    TYPE
        listPtr = POINTER TO listRec;
        listRec =
            RECORD
            next        : listPtr;
            name        : ARRAY [0..63] OF CHAR;
            END;
    VAR
        temp    : ARRAY [0..255] OF CHAR;
        root    : HKEY;
        count   : CARDINAL;
        i       : CARDINAL;
        res     : CARDINAL;
        ft      : FILETIME;
        first,
        last,
        ptr     : listPtr;
    BEGIN
        temp := path;
        AppendCharCond('\', temp);
        Append(keyName, temp);
        IF RegOpenKeyEx(base, temp, 0, KEY_ALL_ACCESS, root) = 0 THEN
            (* enumerate all subdirectories *)
            (* we have to finish enumerating before be recurse *)
            (* otherwise it does not work *)
            (* operating system limitation? *)

            first := NIL;
            last := NIL;
            i := 0;
            REPEAT
                temp := "";
                count := HIGH(temp)+1;
                res := RegEnumKeyEx(root,
                                    i,
                                    temp, count,
                                    NIL_DWORD,
                                    NIL_STR,
                                    NIL_DWORD,
                                    ft
                                    );
                IF res = 0 THEN
                    NEW(ptr);
                    ptr^.next := NIL;
                    ptr^.name := temp;

                    IF first <> NIL THEN
                        last^.next := ptr;
                    ELSE
                        first := ptr;
                    END;
                    last := ptr;
                    INC(i);
                ELSIF res <> ERROR_NO_MORE_ITEMS THEN
                    res := ERROR_NO_MORE_ITEMS;
                END;
            UNTIL res = ERROR_NO_MORE_ITEMS;
            RegCloseKey(root);

            temp := path;
            AppendCharCond('\', temp);
            Append(keyName, temp);
            WHILE first <> NIL DO
                last := first^.next;

                deleteKey(base, temp, first^.name);

                DISPOSE(first);
                first := last;
            END;
        END;


        IF RegOpenKeyEx(base, path, 0, KEY_ALL_ACCESS, root) = 0 THEN
            RegDeleteKey(root, keyName);
            RegCloseKey(root);
        END;
    END deleteKey;

VAR
    parts       : FileNameParts;
    temp        : FileSpecString;
    l           : ADRCARD;
BEGIN
    ParseFileName(keyName, parts);
    IF parts.name[0] = '' THEN
        l := LENGTH(parts.path);
        IF (l > 0) AND (parts.path[l-1] = '\') THEN
            temp := parts.path;
            temp[l-1] := '';
            ParseFileName(temp, parts);
        END;
    END;
    IF parts.name[0] <> '' THEN
        deleteKey(RegBaseTable[base], parts.path, parts.name);
    END;
END RemoveRegDir;

PROCEDURE EnumRegItems(call : EnumCallBack);
VAR
    key         : HKEY;
    keyName     : ARRAY [0..127] OF CHAR;
    i           : CARDINAL;
    count       : DWORD;
    typ         : DWORD;
BEGIN
    IF RegOpenKeyEx(RegBase,
                    PathString,
                    0,
                    KEY_READ,
                    key) = 0
    THEN
        i := 0;
        LOOP
            keyName := "";
            count := HIGH(keyName)+1;
            IF RegEnumValue(key,
                            i,
                            keyName, count,
                            NIL_DWORD,
                            typ,
                            NIL_DWORD,
                            NIL_DWORD
                            ) = 0
            THEN
                call(keyName);
                INC(i);
            ELSE
                EXIT;
            END;
        END;
        RegCloseKey(key);
    END;
END EnumRegItems;

PROCEDURE RegisterProgramPath(program, searchPath : ARRAY OF CHAR);
VAR
    keyName     : FileSpecString;
    parts       : FileNameParts;
    key         : HKEY;
    count       : CARDINAL;
    ok          : BOOLEAN;
BEGIN
    ParseFileName(program, parts);

    Concat("Software\Microsoft\Windows\CurrentVersion\App Paths\",
            parts.name,
            keyName);
    Append(parts.extension, keyName);
    CheckRegTree(HKEY_LOCAL_MACHINE, keyName);

    IF RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                    keyName,
                    0,
                    KEY_WRITE,
                    key) = 0
    THEN
        count := (LENGTH(program) + 1) * SIZE(CHAR);
        ok := RegSetValueEx(key,
                            "",
                            0,
                            REG_SZ,
                            program,
                            count
                            ) = 0;

        IF searchPath[0] <> '' THEN
            count := (LENGTH(searchPath) + 1) * SIZE(CHAR);
            ok := RegSetValueEx(key,
                                "Path",
                                0,
                                REG_SZ,
                                searchPath,
                                count
                                ) = 0;
        END;

        RegCloseKey(key);
    END;
END RegisterProgramPath;

PROCEDURE RegisterFileExtAppName(ext : ARRAY OF CHAR;
                                 appName : ARRAY OF CHAR;
                                 description : ARRAY OF CHAR);
VAR
    root        : HKEY;
    key         : HKEY;
    res         : DWORD;
    ok          : BOOLEAN;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                    "Software\Classes",
                    0,
                    KEY_WRITE,
                    root) = 0
    THEN
        RegCreateKeyEx(root,
                       ext,
                       0,
                       NIL_STR,
                       REG_OPTION_NON_VOLATILE,
                       KEY_ALL_ACCESS,
                       NIL_SECURITY_ATTRIBUTES,
                       key,
                       res);

        count := (LENGTH(appName) + 1) * SIZE(CHAR);
        ok := RegSetValueEx(key,
                            "",
                            0,
                            REG_SZ,
                            appName,
                            count
                            ) = 0;
        RegCloseKey(key);

        RegCreateKeyEx(root,
                       appName,
                       0,
                       NIL_STR,
                       REG_OPTION_NON_VOLATILE,
                       KEY_ALL_ACCESS,
                       NIL_SECURITY_ATTRIBUTES,
                       key,
                       res);

        count := (LENGTH(description) + 1) * SIZE(CHAR);
        ok := RegSetValueEx(key,
                            "",
                            0,
                            REG_SZ,
                            description,
                            count
                            ) = 0;
        RegCloseKey(key);

        RegCloseKey(root);
    END;
END RegisterFileExtAppName;

PROCEDURE RegisterFileIcon(appName : ARRAY OF CHAR;
                           exeName : ARRAY OF CHAR;
                           icon : INTEGER);
VAR
    keyName     : ARRAY [0..127] OF CHAR;
    keyStr      : ARRAY [0..300] OF CHAR;
    numStr      : ARRAY [0..15] OF CHAR;
    key         : HKEY;
    ok          : BOOLEAN;
    count       : CARDINAL;
BEGIN
    Concat("Software\Classes\", appName, keyName);
    Append("\DefaultIcon", keyName);
    CheckRegTree(HKEY_LOCAL_MACHINE, keyName);

    keyStr := exeName;
    Append(',', keyStr);
    IntToStr(icon, numStr);
    Append(numStr, keyStr);

    IF RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                    keyName,
                    0,
                    KEY_WRITE,
                    key) = 0
    THEN
        count := (LENGTH(keyStr) + 1) * SIZE(CHAR);
        ok := RegSetValueEx(key,
                            "",
                            0,
                            REG_SZ,
                            keyStr,
                            count
                            ) = 0;
        RegCloseKey(key);
    END;
END RegisterFileIcon;

PROCEDURE RegisterFileOpen(appName : ARRAY OF CHAR;
                           command : ARRAY OF CHAR);
VAR
    keyName     : ARRAY [0..127] OF CHAR;
    key         : HKEY;
    count       : CARDINAL;
    ok          : BOOLEAN;
BEGIN
    Concat("Software\Classes\", appName, keyName);
    Append("\Shell\Open\Command", keyName);
    CheckRegTree(HKEY_LOCAL_MACHINE, keyName);

    IF RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                    keyName,
                    0,
                    KEY_WRITE,
                    key) = 0
    THEN
        count := (LENGTH(command) + 1) * SIZE(CHAR);
        ok := RegSetValueEx(key,
                            "",
                            0,
                            REG_SZ,
                            command,
                            count
                            ) = 0;
        RegCloseKey(key);
    END;
END RegisterFileOpen;

PROCEDURE RegisterFilePrint(appName : ARRAY OF CHAR;
                            command : ARRAY OF CHAR);
VAR
    keyName     : ARRAY [0..127] OF CHAR;
    key         : HKEY;
    count       : CARDINAL;
    ok          : BOOLEAN;
BEGIN
    Concat("Software\Classes\", appName, keyName);
    Append("\Shell\Print\Command", keyName);
    CheckRegTree(HKEY_LOCAL_MACHINE, keyName);

    IF RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                    keyName,
                    0,
                    KEY_WRITE,
                    key) = 0
    THEN
        count := (LENGTH(command) + 1) * SIZE(CHAR);
        ok := RegSetValueEx(key,
                            "",
                            0,
                            REG_SZ,
                            command,
                            count
                            ) = 0;
        RegCloseKey(key);
    END;
END RegisterFilePrint;

PROCEDURE RegisterFilePrintTo(appName : ARRAY OF CHAR;
                              command : ARRAY OF CHAR);
VAR
    keyName     : ARRAY [0..127] OF CHAR;
    key         : HKEY;
    count       : CARDINAL;
    ok          : BOOLEAN;
BEGIN
    Concat("Software\Classes\", appName, keyName);
    Append("\Shell\PrintTo\Command", keyName);
    CheckRegTree(HKEY_LOCAL_MACHINE, keyName);

    IF RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                    keyName,
                    0,
                    KEY_WRITE,
                    key) = 0
    THEN
        count := (LENGTH(command) + 1) * SIZE(CHAR);
        ok := RegSetValueEx(key,
                            "",
                            0,
                            REG_SZ,
                            command,
                            count
                            ) = 0;
        RegCloseKey(key);
    END;
END RegisterFilePrintTo;

END Registry.
