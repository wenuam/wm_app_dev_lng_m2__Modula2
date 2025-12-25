(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE ConfigSettings;
<*/NOWARN:F*>

FROM SYSTEM IMPORT
    LOC;

FROM Strings IMPORT
    Concat, Append, Equal;

FROM ExStrings IMPORT
    AppendChar;

FROM ExStorage IMPORT
    ALLOCATE, DEALLOCATE;

FROM Conversions IMPORT
    IntToStr, CardToStr, StrToCard, StrToInt;

FROM WIN32 IMPORT
    DWORD,
    KEY_READ, KEY_WRITE, REG_SZ, REG_BINARY,
    REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS;

FROM WINREG IMPORT
    HKEY, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE,
    RegOpenKeyEx, RegQueryValueEx, RegSetValueEx, RegCloseKey, RegCreateKeyEx,
    RegDeleteValue, RegEnumValue;

FROM WINX IMPORT
    NIL_DWORD, NIL_STR, NIL_SECURITY_ATTRIBUTES;

FROM WINERROR IMPORT
    ERROR_MORE_DATA;

CONST
    RegBaseTable : ARRAY BOOLEAN OF HKEY = {HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER};
VAR
    RegBase     : HKEY;
    PathString  : ARRAY [0..255] OF CHAR;
    Open        : BOOLEAN = FALSE;

PROCEDURE CheckRegTree(base : HKEY; name : ARRAY OF CHAR) : CfgOpenStatus;
VAR
    res         : DWORD;
    key         : HKEY;
    stat        : CfgOpenStatus;
BEGIN
    stat := CfgFailed;

    IF RegOpenKeyEx(base, name, 0, KEY_WRITE, key) = 0 THEN
        stat := CfgOpened;
        RegCloseKey(key);
    ELSE
        IF RegCreateKeyEx(base,
                          name,
                          0,
                          NIL_STR,
                          REG_OPTION_NON_VOLATILE,
                          KEY_ALL_ACCESS,
                          NIL_SECURITY_ATTRIBUTES,
                          key,
                          res) = 0
        THEN
            stat := CfgCreated;
            RegCloseKey(key);
        END;
    END;
    RETURN stat;
END CheckRegTree;

PROCEDURE OpenConfig(companyName, appName : ARRAY OF CHAR;
                     user : BOOLEAN) : CfgOpenStatus;
VAR
    stat        : CfgOpenStatus;
BEGIN
    stat := CfgFailed;
    IF NOT Open THEN
        RegBase := RegBaseTable[user];

        Concat("Software\", companyName, PathString);
        AppendChar('\', PathString);
        Append(appName, PathString);

        stat := CheckRegTree(RegBase, PathString);
        IF stat <> CfgFailed THEN
            Open := TRUE;
        END;
    END;
    RETURN stat;
END OpenConfig;

PROCEDURE CloseConfig;
BEGIN
    Open := FALSE;
END CloseConfig;

PROCEDURE ReadCfgString(keyName : ARRAY OF CHAR; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
VAR
    key         : HKEY;
    ok          : BOOLEAN;
    typ         : CARDINAL;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(RegBase, PathString, 0, KEY_READ, key) = 0
    THEN
        count := SIZE(str);
        ok := RegQueryValueEx(key, keyName, NIL_DWORD, typ, str, count) = 0;
        RegCloseKey(key);
        RETURN ok AND (typ = REG_SZ);
    END;
    RETURN FALSE;
END ReadCfgString;

PROCEDURE WriteCfgString(keyName : ARRAY OF CHAR; str : ARRAY OF CHAR) : BOOLEAN;
VAR
    key         : HKEY;
    ok          : BOOLEAN;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(RegBase, PathString, 0, KEY_WRITE, key) = 0 THEN
        count := (LENGTH(str) + 1) * SIZE(CHAR);
        ok := RegSetValueEx(key, keyName, 0, REG_SZ, str, count) = 0;

        RegCloseKey(key);
        RETURN ok;
    END;
    RETURN FALSE;
END WriteCfgString;

PROCEDURE ReadCfgInt(keyName : ARRAY OF CHAR; VAR OUT val : INTEGER) : BOOLEAN;
VAR
    str         : ARRAY [0..15] OF CHAR;
BEGIN
    IF ReadCfgString(keyName, str) THEN
        RETURN StrToInt(str, val);
    END;
    RETURN FALSE;
END ReadCfgInt;

PROCEDURE WriteCfgInt(keyName : ARRAY OF CHAR; val : INTEGER) : BOOLEAN;
VAR
    str         : ARRAY [0..15] OF CHAR;
BEGIN
    IF IntToStr(val, str) THEN
        IF WriteCfgString(keyName, str) THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END WriteCfgInt;

PROCEDURE ReadCfgCard(keyName : ARRAY OF CHAR; VAR OUT val : CARDINAL) : BOOLEAN;
VAR
    str         : ARRAY [0..15] OF CHAR;
BEGIN
    IF ReadCfgString(keyName, str) THEN
        RETURN StrToCard(str, val);
    END;
    RETURN FALSE;
END ReadCfgCard;

PROCEDURE WriteCfgCard(keyName : ARRAY OF CHAR; val : CARDINAL) : BOOLEAN;
VAR
    str         : ARRAY [0..15] OF CHAR;
BEGIN
    IF CardToStr(val, str) THEN
        IF WriteCfgString(keyName, str) THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END WriteCfgCard;

PROCEDURE ReadCfgBool(keyName : ARRAY OF CHAR; VAR OUT val : BOOLEAN) : BOOLEAN;
VAR
    str         : ARRAY [0..15] OF CHAR;
BEGIN
    IF ReadCfgString(keyName, str) THEN
        val := Equal(str, "1");
        RETURN TRUE;
    END;
    RETURN FALSE;
END ReadCfgBool;

PROCEDURE WriteCfgBool(keyName : ARRAY OF CHAR; val : BOOLEAN) : BOOLEAN;
TYPE
    string      = ARRAY [0..1] OF CHAR;
CONST
    str         : ARRAY BOOLEAN OF string = {"0", "1"};
BEGIN
    IF WriteCfgString(keyName, str[val]) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END WriteCfgBool;

PROCEDURE ReadCfgBin(keyName : ARRAY OF CHAR; VAR OUT bin : ARRAY OF LOC) : BOOLEAN;
VAR
    key         : HKEY;
    ok          : BOOLEAN;
    typ         : CARDINAL;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(RegBase, PathString, 0, KEY_READ, key) = 0 THEN
        count := 1;
        IF RegQueryValueEx(key, keyName, NIL_DWORD, typ, bin, count) = ERROR_MORE_DATA THEN
            <*/PUSH/NOWARN:U*>
            IF count <> ORD(SIZE(bin)) THEN
            <*/POP*>
                RegCloseKey(key);
                RETURN FALSE;
            END;
        END;

        count := SIZE(bin);
        ok := RegQueryValueEx(key, keyName, NIL_DWORD, typ, bin, count) = 0;
        RegCloseKey(key);
        <*/PUSH/NOWARN:U*>
        RETURN ok AND (count = ORD(SIZE(bin))) AND (typ = REG_BINARY);
        <*/POP*>
    END;
    RETURN FALSE;
END ReadCfgBin;

PROCEDURE WriteCfgBin(keyName : ARRAY OF CHAR; bin : ARRAY OF LOC) : BOOLEAN;
VAR
    key         : HKEY;
    ok          : BOOLEAN;
    count       : CARDINAL;
BEGIN
    IF RegOpenKeyEx(RegBase, PathString, 0, KEY_WRITE, key) = 0 THEN
        count := SIZE(bin);
        ok := RegSetValueEx(key, keyName, 0, REG_BINARY, bin, count) = 0;
        RegCloseKey(key);
        RETURN ok;
    END;
    RETURN FALSE;
END WriteCfgBin;

PROCEDURE RemoveCfgItem(keyName : ARRAY OF CHAR);
VAR
    key         : HKEY;
BEGIN
    IF RegOpenKeyEx(RegBase, PathString, 0, KEY_WRITE, key) = 0 THEN
        RegDeleteValue(key, keyName);
        RegCloseKey(key);
    END;
END RemoveCfgItem;

PROCEDURE RemoveAllCfgItems;
TYPE
    nameListPtr = POINTER TO nameListRec;
    nameListRec =
        RECORD
        next    : nameListPtr;
        name    : ARRAY [0..127] OF CHAR;
        END;
VAR
    key         : HKEY;
    keyName     : ARRAY [0..127] OF CHAR;
    i           : CARDINAL;
    count       : DWORD;
    typ         : DWORD;
    first       : nameListPtr;
    ptr         : nameListPtr;
BEGIN
    IF RegOpenKeyEx(RegBase, PathString, 0, KEY_READ, key) = 0 THEN
        first := NIL;
        i := 0;
        LOOP
            keyName := "";
            count := HIGH(keyName)+1;
            IF RegEnumValue(key, i, keyName, count, NIL_DWORD, typ, NIL_DWORD, NIL_DWORD) = 0 THEN
                NEW(ptr);
                ptr^.next := first;
                first := ptr;
                ptr^.name := keyName;

                INC(i);
            ELSE
                EXIT;
            END;
        END;

        RegCloseKey(key);

        IF RegOpenKeyEx(RegBase, PathString, 0, KEY_WRITE, key) = 0 THEN
            WHILE first <> NIL DO
                ptr := first^.next;
                RegDeleteValue(key, first^.name);
                DISPOSE(first);
                first := ptr;
            END;
            RegCloseKey(key);
        ELSE
            WHILE first <> NIL DO
                ptr := first^.next;
                DISPOSE(first);
                first := ptr;
            END;
        END;
    END;
END RemoveAllCfgItems;

PROCEDURE EnumCfgItems(call : EnumCallBack);
VAR
    key         : HKEY;
    keyName     : ARRAY [0..127] OF CHAR;
    i           : CARDINAL;
    count       : DWORD;
    typ         : DWORD;
BEGIN
    IF RegOpenKeyEx(RegBase, PathString, 0, KEY_READ, key) = 0 THEN
        i := 0;
        LOOP
            keyName := "";
            count := HIGH(keyName)+1;
            IF RegEnumValue(key, i, keyName, count, NIL_DWORD, typ, NIL_DWORD, NIL_DWORD) = 0 THEN
                call(keyName);
                INC(i);
            ELSE
                EXIT;
            END;
        END;
        RegCloseKey(key);
    END;
END EnumCfgItems;

END ConfigSettings.
