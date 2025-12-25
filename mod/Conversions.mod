(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE Conversions;

FROM SYSTEM IMPORT
    ADRCARD, ADRINT;

CONST
    Digits : ARRAY [0..15] OF CHAR = {"0123456789ABCDEF"};

PROCEDURE StringToInt(str : ARRAY OF CHAR;
                      VAR INOUT pos : CARDINAL;
                      VAR OUT num : INTEGER;
                      VAR OUT done : BOOLEAN);
CONST
    max         = MAX(INTEGER) / 10;
VAR
    ch          : CHAR;
    neg         : BOOLEAN;
    first       : BOOLEAN;
    d           : INTEGER;
    lnum        : INTEGER;
    last        : INTEGER;
BEGIN
    done := TRUE;
    first := TRUE;
    lnum := 0;
    neg := FALSE;

    WHILE ((pos <= HIGH(str)) AND (str[pos] = ' ')) DO
        INC(pos);
    END;

    last := MAX(INTEGER) REM 10;
    IF pos <= HIGH(str) THEN
        ch := str[pos];
        IF ch = '+' THEN
            INC(pos);
        ELSIF ch = '-' THEN
            INC(pos);
            neg := TRUE;
            last := ABS(MIN(INTEGER) REM 10);
        END;
    END;

    LOOP
        IF pos > HIGH(str) THEN
            done := NOT first;
            EXIT;
        END;

        ch := str[pos];
        IF (ch < '0') OR (ch > '9') THEN
            done := NOT first;
            EXIT;
        END;

        first := FALSE;
        d := INT(ch) - INT('0');
        IF (lnum > max) OR
           (
            (lnum = max) AND
            (d > last)
           )
        THEN
            done := FALSE;
            EXIT;
        END;
        lnum := (lnum * 10) + d;
        INC(pos);
    END;

    num := lnum;
    IF neg THEN
        num := -num;
    END;
END StringToInt;

PROCEDURE StrToInt(buf : ARRAY OF CHAR; VAR OUT num: INTEGER) : BOOLEAN;
VAR
    i   : CARDINAL;
    ok  : BOOLEAN;
BEGIN
    i := 0;
    StringToInt(buf, i, num, ok);
    RETURN ok AND (i = LENGTH(buf));
END StrToInt;

PROCEDURE IntToString(num : ADRINT;
                      size : CARDINAL;
                      VAR OUT str : ARRAY OF CHAR;
                      VAR INOUT posInOut : CARDINAL;
                      VAR OUT done : BOOLEAN);
VAR
    neg         : BOOLEAN;
    i           : ADRINT;
    j           : ADRINT;
    pos         : ADRCARD;
    highStr     : ADRCARD;
    buf         : ARRAY [0..31] OF CHAR;
BEGIN
    pos := posInOut;
    highStr := HIGH(str);
    neg := (num < 0);

    i := 0;
    REPEAT
        buf[i] := Digits[ABS(num REM 10)];
        num := num / 10;
        INC(i);
    UNTIL num = 0;

    DEC(i);

    <*/PUSH/NOWARN:U*>
    FOR j := i+VAL(ADRINT, INT(neg))+2 TO VAL(ADRINT, size) DO
    <*/POP*>
        IF pos <= highStr THEN
            str[pos] := ' ';
            INC(pos);
        END;
    END;

    IF neg THEN
        IF pos <= highStr THEN
            str[pos] := '-';
            INC(pos);
        END;
    END;

    WHILE (i >= 0) AND (pos <= highStr) DO
        str[pos] := buf[i];
        DEC(i);
        INC(pos);
    END;

    done := (i < 0);

    IF pos <= highStr THEN
        str[pos] := '';
    END;
    posInOut := pos;
END IntToString;

PROCEDURE IntToStr(num : ADRINT; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
VAR
    ok  : BOOLEAN;
    i   : CARDINAL;
BEGIN
    i := 0;
    IntToString(num, 0, str, i, ok);
    RETURN ok;
END IntToStr;

PROCEDURE StringToCard(str : ARRAY OF CHAR;
                       VAR INOUT posInOut : CARDINAL;
                       VAR OUT num : CARDINAL;
                       VAR OUT done : BOOLEAN);
CONST
    max         = MAX(CARDINAL) / 10;
    last        = MAX(CARDINAL) REM 10;
VAR
    ch          : CHAR;
    first       : BOOLEAN;
    lnum        : CARDINAL;
    d           : CARDINAL;
    pos         : ADRCARD;
    highStr     : ADRCARD;
BEGIN
    pos := posInOut;
    highStr := HIGH(str);
    done := TRUE;
    first := TRUE;
    lnum := 0;

    WHILE ((pos <= highStr) AND (str[pos] = ' ')) DO
        INC(pos);
    END;

    IF pos <= highStr THEN
        IF str[pos] = '+' THEN
            INC(pos);
        END;
    END;

    LOOP
        IF pos > highStr THEN
            done := NOT first;
            EXIT;
        END;

        ch := str[pos];
        IF (ch < '0') OR (ch > '9') THEN
            done := NOT first;
            EXIT;
        END;

        first := FALSE;
        d := ORD(ch) - ORD('0');
        IF (
            (lnum > max)
           )
           OR
           (
            (lnum = max) AND (d > last)
           )
        THEN
            done := FALSE;
            EXIT;
        END;
        lnum := (lnum * 10) + d;
        INC(pos);
    END;
    posInOut := pos;
    num := lnum;
END StringToCard;

PROCEDURE StrToCard(buf : ARRAY OF CHAR; VAR OUT num : CARDINAL) : BOOLEAN;
VAR
    i   : CARDINAL;
    ok  : BOOLEAN;
BEGIN
    i := 0;
    StringToCard(buf, i, num, ok);
    RETURN ok AND (i = LENGTH(buf));
END StrToCard;

PROCEDURE CardToString(num : ADRCARD;
                       size : CARDINAL;
                       VAR OUT str : ARRAY OF CHAR;
                       VAR INOUT posInOut : CARDINAL;
                       VAR OUT done : BOOLEAN);
VAR
    i           : ADRINT;
    j           : ADRINT;
    pos         : ADRCARD;
    highStr     : ADRCARD;
    buf         : ARRAY [0..31] OF CHAR;
BEGIN
    pos := posInOut;
    highStr := HIGH(str);
    i := 0;
    REPEAT
        buf[i] := Digits[num REM 10];
        num := num / 10;
        INC(i);
    UNTIL num = 0;

    DEC(i);

    FOR j := i+2 TO VAL(ADRINT, size) DO
        IF pos <= highStr THEN
            str[pos] := ' ';
            INC(pos);
        END;
    END;

    WHILE (i >= 0) AND (pos <= highStr) DO
        str[pos] := buf[i];
        DEC(i);
        INC(pos);
    END;

    done := (i < 0);

    IF pos <= highStr THEN
        str[pos] := '';
    END;
    posInOut := pos;
END CardToString;

PROCEDURE CardToStr(num : ADRCARD; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
VAR
    ok  : BOOLEAN;
    i   : CARDINAL;
BEGIN
    i := 0;
    CardToString(num, 0, str, i, ok);
    RETURN ok;
END CardToStr;

PROCEDURE StrBaseToCard(str : ARRAY OF CHAR;
                        base : CARDINAL;
                        VAR OUT num : CARDINAL) : BOOLEAN;
VAR
    ch          : CHAR;
    i           : ADRCARD;
    highStr     : ADRCARD;
    t           : CARDINAL;
    max         : CARDINAL;
    lbase       : CARDINAL;
    last        : CARDINAL;
    done        : BOOLEAN;
    lnum        : CARDINAL;
BEGIN
    highStr := HIGH(str);
    lbase := base;
    max := MAX(CARDINAL) / lbase;
    last := MAX(CARDINAL) REM lbase;

    i := 0;
    lnum := 0;
    done := FALSE;

    WHILE ((i <= highStr) AND (str[i] = ' ')) DO
        INC(i);
    END;

    WHILE i <= highStr DO
        IF str[i] = '' THEN
            num := lnum;
            RETURN done;
        END;

        ch := CAP(str[i]);
        IF (ch >= 'A') AND (ch <= 'F') THEN
            t := 10 + (ORD(ch)-ORD('A'));
        ELSIF (ch >= '0') AND (ch <= '9') THEN
            t := ORD(ch) - ORD('0');
        ELSE
            t := 100;
        END;

        IF t < base THEN
            IF (lnum > max) OR ((lnum = max) AND (t > last)) THEN
                num := lnum;
                RETURN FALSE;
            END;

            <*/PUSH/NOWARN:U*>
            lnum := (lnum * lbase) + ORD(t);
            <*/POP*>
            done := TRUE;
            INC(i);
        ELSE
            num := lnum;
            RETURN FALSE;
        END;
    END;
    num := lnum;
    RETURN done;
END StrBaseToCard;

PROCEDURE CardBaseToStr(num : ADRCARD;
                        base : CARDINAL;
                        VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
VAR
    i           : ADRCARD;
    j           : ADRCARD;
    highStr     : ADRCARD;
    lbase       : ADRCARD;
    buf         : ARRAY [0..79] OF CHAR;
BEGIN
    IF (base >= 2) AND (base <= 16) THEN
        highStr := HIGH(str);
        lbase := base;
        i := 0;
        REPEAT
            buf[i] := Digits[num REM lbase];
            num := num / lbase;
            INC(i);
        UNTIL num = 0;

        DEC(i);

        IF i <= highStr THEN
            IF i < highStr THEN
                str[i+1] := '';
            END;

            FOR j := 0 TO i DO;
                str[i-j] := buf[j];
            END;

            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END CardBaseToStr;

PROCEDURE StringToLong(str : ARRAY OF CHAR;
                       VAR INOUT posInOut : CARDINAL;
                       VAR OUT num : LONGINT;
                       VAR OUT done : BOOLEAN);
CONST
    max         = MAX(LONGINT) / 10;
VAR
    ch          : CHAR;
    neg         : BOOLEAN;
    first       : BOOLEAN;
    d           : INTEGER;
    lnum        : LONGINT;
    last        : INTEGER;
    pos         : ADRCARD;
    highStr     : ADRCARD;
BEGIN
    pos := posInOut;
    highStr := HIGH(str);
    done := TRUE;
    first := TRUE;
    lnum := 0;
    neg := FALSE;

    WHILE ((pos <= highStr) AND (str[pos] = ' ')) DO
        INC(pos);
    END;

    last := INT( MAX(LONGINT) REM 10 );
    IF pos <= highStr THEN
        ch := str[pos];
        IF ch = '+' THEN
            INC(pos);
        ELSIF ch = '-' THEN
            INC(pos);
            neg := TRUE;
            last := ABS(INT( MIN(LONGINT) REM 10 ));
        END;
    END;

    LOOP
        IF pos > highStr THEN
            done := NOT first;
            EXIT;
        END;

        ch := str[pos];
        IF (ch < '0') OR (ch > '9') THEN
            done := NOT first;
            EXIT;
        END;

        first := FALSE;
        d := INT(ch) - INT('0');
        IF (lnum > max) OR
           (
            (lnum = max) AND
            (d > last)
           )
        THEN
            done := FALSE;
            EXIT;
        END;
        lnum := (lnum * 10) + VAL(LONGINT, d);
        INC(pos);
    END;

    num := lnum;
    IF neg THEN
        num := -num;
    END;
    posInOut := pos;
END StringToLong;

PROCEDURE StrToLong(str : ARRAY OF CHAR; VAR OUT num : LONGINT) : BOOLEAN;
VAR
    i   : CARDINAL;
    ok  : BOOLEAN;
BEGIN
    i := 0;
    StringToLong(str, i, num, ok);
    RETURN ok AND (i = LENGTH(str));
END StrToLong;

PROCEDURE LongToString(num : LONGINT;
                       size : CARDINAL;
                       VAR OUT str : ARRAY OF CHAR;
                       VAR INOUT posInOut : CARDINAL;
                       VAR OUT done : BOOLEAN);
VAR
    neg         : BOOLEAN;
    i           : ADRINT;
    j           : ADRINT;
    pos         : ADRCARD;
    highStr     : ADRCARD;
    buf         : ARRAY [0..31] OF CHAR;
BEGIN
    pos := posInOut;
    highStr := HIGH(str);
    neg := (num < 0);

    i := 0;
    REPEAT
        buf[i] := Digits[ABS(INT(num REM 10))];
        num := num / 10;
        INC(i);
    UNTIL num = 0;

    DEC(i);

    <*/PUSH/NOWARN:U*>
    FOR j := i+VAL(ADRINT, INT(neg))+2 TO VAL(ADRINT, size) DO
    <*/POP*>
        IF pos <= highStr THEN
            str[pos] := ' ';
            INC(pos);
        END;
    END;

    IF neg THEN
        IF pos <= highStr THEN
            str[pos] := '-';
            INC(pos);
        END;
    END;

    WHILE (i >= 0) AND (pos <= highStr) DO
        str[pos] := buf[i];
        DEC(i);
        INC(pos);
    END;

    done := (i < 0);

    IF pos <= highStr THEN
        str[pos] := '';
    END;
    posInOut := pos;
END LongToString;

PROCEDURE LongToStr(num : LONGINT; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
VAR
    ok  : BOOLEAN;
    i   : CARDINAL;
BEGIN
    i := 0;
    LongToString(num, 0, str, i, ok);
    RETURN ok;
END LongToStr;

PROCEDURE StrBaseToLong(str : ARRAY OF CHAR;
                        base : CARDINAL;
                        VAR OUT num : LONGCARD) : BOOLEAN;
VAR
    ch          : CHAR;
    i           : ADRCARD;
    highStr     : ADRCARD;
    t           : CARDINAL;
    last        : CARDINAL;
    done        : BOOLEAN;
    max         : LONGCARD;
    lnum        : LONGCARD;
    lbase       : LONGCARD;
BEGIN
    highStr := HIGH(str);
    lbase := base;

    max := MAX(LONGCARD) / lbase;
    last := MAX(LONGCARD) REM lbase;

    i := 0;
    lnum := 0;
    done := FALSE;

    WHILE ((i <= highStr) AND (str[i] = ' ')) DO
        INC(i);
    END;

    LOOP
        IF (i <= highStr) AND (str[i] <> '') THEN

            ch := CAP(str[i]);
            IF (ch >= 'A') AND (ch <= 'F') THEN
                t := 10 + (ORD(ch)-ORD('A'));
            ELSIF (ch >= '0') AND (ch <= '9') THEN
                t := ORD(ch) - ORD('0');
            ELSE
                t := 100;
            END;

            IF t < base THEN
                IF (lnum > max) OR ((lnum = max) AND (t > last)) THEN
                    num := lnum;
                    done := FALSE;
                    EXIT;
                END;

                lnum := (lnum * lbase) + VAL(LONGCARD, t);
                done := TRUE;
                INC(i);
            ELSE
                done := FALSE;
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;
    num := lnum;
    RETURN done;
END StrBaseToLong;

PROCEDURE LongBaseToStr(num : LONGCARD;
                        base : CARDINAL;
                        VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
VAR
    i           : ADRCARD;
    j           : ADRCARD;
    highStr     : ADRCARD;
    lbase       : LONGCARD;
    buf         : ARRAY [0..79] OF CHAR;
    neg         : BOOLEAN;
BEGIN
    IF (base >= 2) AND (base <= 16) THEN
        neg := num < 0;

        lbase := base;
        i := 0;
        REPEAT
            buf[i] := Digits[ABS(INT(num REM lbase))];
            num := num / lbase;
            INC(i);
        UNTIL num = 0;

        IF neg THEN
            buf[i] := '-';
            INC(i);
        END;

        DEC(i);

        highStr := HIGH(str);
        IF i <= highStr THEN
            IF i < highStr THEN
                str[i+1] := '';
            END;

            FOR j := 0 TO i DO;
                str[i-j] := buf[j];
            END;

            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END LongBaseToStr;

END Conversions.
