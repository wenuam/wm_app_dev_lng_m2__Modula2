(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                           All rights reserved.                          *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE TextFileFunc;

FROM SYSTEM IMPORT
    ADRCARD, ADR, IsThread;

FROM FileFunc IMPORT
    File, EOL, WriteBlock, WriteChar, ReadChar;

FROM Conversions IMPORT
    LongToStr, StrToLong, LongBaseToStr, StrBaseToLong;

FROM LongStr IMPORT
    RealToFixed, RealToFloat, RealToEng, StrToReal, ConvResults;

IMPORT ASCII;

PROCEDURE WriteString(VAR INOUT f : File; str : ARRAY OF CHAR);
BEGIN
    IF str[0] <> '' THEN
        WriteBlock(f, ADR(str), LENGTH(str)*SIZE(CHAR));
    END;
END WriteString;

PROCEDURE WriteStringLn(VAR INOUT f : File; str : ARRAY OF CHAR);
BEGIN
    WriteString(f, str);
    WriteLn(f);
END WriteStringLn;

PROCEDURE WriteField(VAR INOUT f : File;
                     str : ARRAY OF CHAR;
                     fieldLen : INTEGER);
VAR
    delta       : INTEGER;
    i           : INTEGER;
BEGIN
    delta := ABS(fieldLen) - INT(LENGTH(str));

    IF (fieldLen <> 0) AND (delta < 0) AND ChopField THEN
        FOR i := 1 TO ABS(fieldLen) DO
            WriteChar(f, ErrorChar);
        END;
        f.status := MAX(CARDINAL);
    ELSE
        IF (delta > 0) AND (fieldLen > 0) THEN
            FOR i := 1 TO delta DO
                WriteChar(f, PadChar);
                IF f.status <> 0 THEN
                    RETURN;
                END;
            END;
        END;

        WriteBlock(f, ADR(str), LENGTH(str)*SIZE(CHAR));
        IF f.status <> 0 THEN
            RETURN;
        END;

        IF (delta > 0) AND (fieldLen < 0) THEN
            FOR i := 1 TO delta DO
                WriteChar(f, PadChar);
                IF f.status <> 0 THEN
                    RETURN;
                END;
            END;
        END;
    END;
END WriteField;

PROCEDURE WriteNumber(VAR INOUT f : File;
                      num : LONGINT;
                      fieldLen : INTEGER);
VAR
    str         : ARRAY [0..31] OF CHAR;
BEGIN
    IF LongToStr(num, str) THEN
        WriteField(f, str, fieldLen);
    ELSE
        f.status := MAX(CARDINAL);
    END;
END WriteNumber;

PROCEDURE WriteNumberBase(VAR INOUT f : File;
                          num : LONGCARD;
                          base : CARDINAL;
                          fieldLen : INTEGER);
VAR
    str         : ARRAY [0..63] OF CHAR;
BEGIN
    IF LongBaseToStr(num, base, str) THEN
        WriteField(f, str, fieldLen);
    ELSE
        f.status := MAX(CARDINAL);
    END;
END WriteNumberBase;

PROCEDURE WriteReal(VAR INOUT f : File;
                    num : LONGREAL;
                    sigFigs : CARDINAL;
                    eng : BOOLEAN;
                    fieldLen : INTEGER);
VAR
    str         : ARRAY [0..79] OF CHAR;
BEGIN
    IF eng THEN
        RealToEng(num, sigFigs, str);
    ELSE
        RealToFloat(num, sigFigs, str);
    END;
    WriteField(f, str, fieldLen);
END WriteReal;

PROCEDURE WriteRealFixed(VAR INOUT f : File;
                         num : LONGREAL;
                         place : CARDINAL;
                         fieldLen : INTEGER);
VAR
    str         : ARRAY [0..79] OF CHAR;
BEGIN
    RealToFixed(num, place, str);
    WriteField(f, str, fieldLen);
END WriteRealFixed;

PROCEDURE WriteBoolean(VAR INOUT f : File; bool : BOOLEAN; fieldLen : INTEGER);
CONST
    value       : ARRAY BOOLEAN OF ARRAY [0..7] OF CHAR =
        {"FALSE", "TRUE"};
BEGIN
    WriteField(f, value[bool], fieldLen);
END WriteBoolean;

PROCEDURE WriteLn(VAR INOUT f : File);
BEGIN
    WriteChar(f, EOL);
END WriteLn;

PROCEDURE ReadString(VAR INOUT f : File; VAR OUT str : ARRAY OF CHAR);
VAR
    i           : ADRCARD;
    highStr     : ADRCARD;
    ch          : CHAR;
BEGIN
    i := 0;
    highStr := HIGH(str);
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
END ReadString;

PROCEDURE ReadField(VAR INOUT f : File; VAR OUT str : ARRAY OF CHAR);
VAR
    ch          : CHAR;
    i           : ADRCARD;
    highStr     : ADRCARD;
BEGIN
    LOOP
        ch := ReadChar(f);
        IF (f.status = 0) AND (NOT f.eof) THEN
            IF NOT (ch IN Separators) THEN
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;

    i := 0;
    highStr := HIGH(str);
    IF (f.status = 0) AND (NOT f.eof) THEN
        str[0] := ch;
        INC(i);
        LOOP
            IF i <= highStr THEN
                ch := ReadChar(f);
                IF (f.status = 0) AND (NOT f.eof) THEN
                    IF NOT (ch IN Separators) THEN
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
    END;
    IF i <= highStr THEN
        str[i] := '';
    END;
END ReadField;

PROCEDURE ReadNumber(VAR INOUT f : File) : LONGINT;
VAR
    str         : ARRAY [0..127] OF CHAR;
    num         : LONGINT;
BEGIN
    ReadField(f, str);
    IF f.status = 0 THEN
        IF StrToLong(str, num) THEN
            RETURN num;
        ELSE
            f.status := MAX(CARDINAL);
        END;
    END;
    RETURN 0;
END ReadNumber;

PROCEDURE ReadNumberValidate(VAR INOUT f : File;
                             min, max : LONGINT;
                             VAR OUT inRange : BOOLEAN) : LONGINT;
VAR
    str         : ARRAY [0..127] OF CHAR;
    num         : LONGINT;
BEGIN
    ReadField(f, str);
    IF f.status = 0 THEN
        IF StrToLong(str, num) THEN
            inRange := (num >= min) AND (num <= max);
            RETURN num;
        END;
        f.status := MAX(CARDINAL);
    END;
    inRange := FALSE;
    RETURN 0;
END ReadNumberValidate;

PROCEDURE ReadNumberBase(VAR INOUT f : File; base : CARDINAL) : LONGCARD;
VAR
    str         : ARRAY [0..127] OF CHAR;
    num         : LONGCARD;
BEGIN
    ReadField(f, str);
    IF f.status = 0 THEN
        IF StrBaseToLong(str, base, num) THEN
            RETURN num;
        ELSE
            f.status := MAX(CARDINAL);
        END;
    END;
    RETURN 0;
END ReadNumberBase;

PROCEDURE ReadReal(VAR INOUT f : File) : LONGREAL;
VAR
    str         : ARRAY [0..127] OF CHAR;
    num         : LONGREAL;
    res         : ConvResults;
BEGIN
    ReadField(f, str);
    IF f.status = 0 THEN
        StrToReal(str, num, res);
        IF res = strAllRight THEN
            RETURN num;
        ELSE
            f.status := MAX(CARDINAL);
        END;
    END;
    RETURN 0.0;
END ReadReal;

PROCEDURE ReadBoolean(VAR INOUT f : File) : BOOLEAN;
VAR
    str         : ARRAY [0..127] OF CHAR;
BEGIN
    ReadField(f, str);
    IF f.status = 0 THEN
        RETURN (CAP(str[0]) = 'T') AND
               (CAP(str[1]) = 'R') AND
               (CAP(str[2]) = 'U') AND
               (CAP(str[3]) = 'E') AND
               (str[4] = '');
    END;
    RETURN FALSE;
END ReadBoolean;

PROCEDURE ReadLn(VAR INOUT f : File);
VAR
    ch  : CHAR;
BEGIN
    LOOP
        ch := ReadChar(f);
        IF (f.status = 0) AND (f.count = SIZE(ch)) THEN
            IF ch = EOL THEN
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;
END ReadLn;

BEGIN
    IF NOT IsThread THEN
        PadChar := ' ';
        ErrorChar := '?';
        ChopField := FALSE;
        Separators := SeparatorSet{' ', EOL,
                                   ASCII.ht, ASCII.vt,
                                   ASCII.ff};
    END;
END TextFileFunc.
