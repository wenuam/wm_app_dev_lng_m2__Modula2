(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE FormatString;
<*/INLINE:N*>

FROM SYSTEM IMPORT
    ADDRESS, ADDADR, VA_LIST, VA_START, VA_ARG, ADRCARD, CAST;

FROM Strings IMPORT
    Delete, Insert, Append;

FROM Conversions IMPORT
    CardToStr, IntToStr, LongToStr, CardBaseToStr, LongBaseToStr;

PROCEDURE FormatString(formatStr : ARRAY OF CHAR;
                       VAR OUT destStr : ARRAY OF CHAR) : BOOLEAN;
VAR
    args        : VA_LIST;
BEGIN
    VA_START(args);
    RETURN FormatStringEx(formatStr, destStr, args);
END FormatString;

PROCEDURE FormatStringEx(formatStr : ARRAY OF CHAR;
                         VAR OUT destStr : ARRAY OF CHAR;
                         VAR INOUT args : VA_LIST) : BOOLEAN;
TYPE
    tPtrCh              = POINTER TO CHAR;
VAR
    i                   : ADRCARD;
    st                  : ADRCARD;
    l                   : ADRCARD;
    width               : ADRCARD;
    leftJust            : BOOLEAN;
    ok                  : BOOLEAN;
    dataType            : CHAR;
    padChar             : CHAR;
    c8                  : CARDINAL8;
    text                : ARRAY [0..255] OF CHAR;

    PROCEDURE insertText;
    VAR
        textLen         : ADRCARD;
        highDest        : ADRCARD;
    BEGIN
        textLen := LENGTH(text);

        WHILE textLen < width DO
            IF leftJust THEN
                Append(padChar, text);
            ELSE
                Insert(padChar, 0, text);
            END;
            INC(textLen);
        END;

        Insert(text, i, destStr);
        i := i + textLen;
        l := l + textLen;

        (* did we just blow the capacity of destStr *)

        highDest := HIGH(destStr);
        IF l-1 > highDest THEN
            ok := FALSE;
        END;
    END insertText;

    PROCEDURE getHexNum(VAR OUT hexNum : CARDINAL) : BOOLEAN;
    VAR
        ch      : CHAR;
        num     : CARDINAL;
        gotOne  : BOOLEAN;
    BEGIN
        gotOne := FALSE;
        num := 0;
        WHILE (i < l) AND
              (
               (
                (destStr[i] >= '0') AND
                (destStr[i] <= '9')
               )
               OR
               (
                (CAP(destStr[i]) >= 'A') AND
                (CAP(destStr[i]) <= 'F')
               )
              )
        DO
            gotOne := TRUE;
            ch := CAP(destStr[i]);
            Delete(destStr, i, 1);
            DEC(l);

            num := num * 16;
            IF (ch >= '0') AND (ch <= '9') THEN
                num := num + (ORD(ch) - ORD('0'));
            ELSE
                num := num + (ORD(ch) - (ORD('A') + 10));
            END;
        END;

        IF gotOne AND (num >= ORD(MIN(CHAR))) AND (num <= ORD(MAX(CHAR))) THEN
            hexNum := num;
            RETURN TRUE;
        END;
        RETURN FALSE;
    END getHexNum;

    PROCEDURE getControlChar() : BOOLEAN;
    VAR
        ctrlChar        : CHAR;
        hexNum          : CARDINAL;
    BEGIN
        ctrlChar := destStr[i+1];
        Delete(destStr, i, 2);
        l := l - 2;

        CASE CAP(ctrlChar) OF
        'N':
            %IF %NOT UNIX %THEN
                Insert(CHR(13), i, destStr);
                INC(i);
                INC(l);
            %END
            Insert(CHR(10), i, destStr);
            INC(l);
        |
        'T':
            Insert(CHR(9), i, destStr);
            INC(l);
        |
        'V':
            Insert(CHR(11), i, destStr);
            INC(l);
        |
        'F':
            Insert(CHR(12), i, destStr);
            INC(l);
        |
        'X':
            IF getHexNum(hexNum) THEN
                Insert(CHR(hexNum), i, destStr);
                INC(l);
            ELSE
                RETURN FALSE;
            END;
        ELSE
            Insert(ctrlChar, i, destStr);
            INC(l);
        END;

        RETURN TRUE;
    END getControlChar;

    PROCEDURE getStr(ptrCh : tPtrCh; VAR OUT text : ARRAY OF CHAR) : BOOLEAN;
    VAR
        i               : ADRCARD;
        highText        : ADRCARD;
    BEGIN
        i := 0;
        highText := HIGH(text);
        WHILE ptrCh^ <> '' DO
            IF i < highText THEN
                text[i] := ptrCh^;
                INC(i);
            END;
            ptrCh := ADDADR(ptrCh, SIZE(CHAR));
        END;
        text[i] := '';
        RETURN TRUE;
    EXCEPT
        RETURN FALSE;
    END getStr;

    PROCEDURE boolToStr(b : BOOLEAN; VAR OUT str : ARRAY OF CHAR);
    BEGIN
        IF b THEN
            str := "TRUE";
        ELSE
            str := "FALSE";
        END;
    END boolToStr;

    PROCEDURE addressToStr(a : ADDRESS; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
    CONST
        digits  = SIZE(ADRCARD)*2;
    VAR
        l       : CARDINAL;
    BEGIN
        IF LongBaseToStr(CAST(ADRCARD, a), 16, str) THEN
            l := LENGTH(str);
            WHILE l < digits DO
                INC(l);
                Insert('0', 0, str);
            END;
            RETURN TRUE;
        END;
        RETURN FALSE;
    END addressToStr;

BEGIN
    (* doing the code this way allows the formatStr and destStr *)
    (* to be the same string *)
    (* copying from one to the other on the fly will not work *)
    (* unless a copy of the formatStr is made *)

    destStr := formatStr;

    (* could be faster, but it would be more complex *)

    ok := TRUE;
    l := LENGTH(destStr);
    i := 0;
    LOOP
        IF (i >= l) OR NOT ok THEN
            EXIT;
        ELSIF destStr[i] = '\' THEN
            IF i+1 < l THEN
                IF destStr[i+1] = '\' THEN
                    Delete(destStr, i, 1);
                    INC(i);
                    DEC(l);
                ELSE
                    IF NOT getControlChar() THEN
                        ok := FALSE;
                        EXIT;
                    END;
                END;
            ELSE
                ok := FALSE;
                EXIT;
            END;
        ELSIF destStr[i] <> '%' THEN
            INC(i);
        ELSIF (i+1 < l) AND (destStr[i+1] = '%') THEN
            Delete(destStr, i, 1);
            INC(i);
            DEC(l);
        ELSE
            st := i;
            INC(i);

            IF i >= l THEN
                ok := FALSE;
                EXIT;
            END;

            leftJust := FALSE;
            IF destStr[i] = '-' THEN
                leftJust := TRUE;
                INC(i);
            END;

            IF i >= l THEN
                ok := FALSE;
                EXIT;
            END;

            padChar := ' ';
            IF destStr[i] = "'" THEN
                INC(i);

                IF i >= l THEN
                    ok := FALSE;
                    EXIT;
                END;

                padChar := destStr[i];
                INC(i);
            END;

            IF i >= l THEN
                ok := FALSE;
                EXIT;
            END;

            width := 0;
            IF destStr[i] = '*' THEN
                INC(i);

                IF i >= l THEN
                    ok := FALSE;
                    EXIT;
                END;

                width := VA_ARG(args, CARDINAL);
            ELSE
                WHILE (i < l) AND (destStr[i] >= '0') AND (destStr[i] <= '9') DO
                    <*/PUSH/NOWARN:U*>
                    width := (width * 10) + VAL(ADRCARD, (ORD(destStr[i]) - ORD('0')));
                    <*/POP*>
                    INC(i);
                END;
            END;

            IF (i >= l) OR (width > 256) THEN
                ok := FALSE;
                EXIT;
            END;

            dataType := CAP(destStr[i]);

            Delete(destStr, st, i-st+1);
            l := l - (i-st+1);

            i := st;

            CASE dataType OF
            'C':
                IF CardToStr(VA_ARG(args, CARDINAL), text) THEN
                    insertText;
                END;
            |
            'I':
                IF IntToStr(VA_ARG(args, INTEGER), text) THEN
                    insertText;
                END;
            |
            'L':
                IF LongToStr(VA_ARG(args, LONGINT), text) THEN
                    insertText;
                END;
            |
            'U':
                IF LongBaseToStr(VA_ARG(args, LONGCARD), 10, text) THEN
                    insertText;
                END;
            |
            'H':
                IF CardBaseToStr(VA_ARG(args, CARDINAL), 16, text) THEN
                    insertText;
                END;
            |
            'X':
                IF LongBaseToStr(VA_ARG(args, LONGCARD), 16, text) THEN
                    insertText;
                END;
            |
            'B':
                c8 := VA_ARG(args, ADRCARD);
                boolToStr(c8 <> 0, text);
                insertText;
            |
            'A':
                IF addressToStr(VA_ARG(args, ADDRESS), text) THEN
                    insertText;
                END;
            |
            'S':
                IF getStr(VA_ARG(args, ADDRESS), text) THEN
                    insertText;
                END;
            ELSE
                ok := FALSE;
                EXIT;
            END;
        END;
    END;
    RETURN ok;
END FormatStringEx;

END FormatString.
