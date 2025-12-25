IMPLEMENTATION MODULE WholeStr;

(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman

          Implementation © 1993
                by R. Sutcliffe
       (Portions coded by G. Tischer)
        Trinity Western University
7600 Glover Rd., Langley, BC Canada V3A 6H4
         e-mail: rsutc@twu.ca

    Stony Brook compiler port Implementation ported
    from above implementation source, those
        portions copyright (c) 1994-2004
        by ADW Software
    procedures are marked as modified
=========================================== *)

  (* Whole-number/string conversions *)

(* the string form of a signed whole number is
     ["+" | "-"], decimal digit, {decimal digit}
*)

FROM SYSTEM IMPORT
    ADRINT, ADRCARD;

FROM CharClass IMPORT
    IsNumeric;

TYPE
    DigitsArray = ARRAY [0..9] OF CHAR;
CONST
    Digits      = DigitsArray{"0123456789"};

PROCEDURE StrToInt(str : ARRAY OF CHAR;
                   VAR int : INTEGER;
                   VAR res : ConvResults);
(* modified by Stony brook *)
  (* Ignores any leading spaces in str. If the subsequent characters in
     str are in the format of a signed whole number, assigns a corresponding
     value to int. Assigns a value indicating the format of str to res.  *)
CONST
    maxDiv10    = MAX(INTEGER) / 10;
VAR
    i           : ADRCARD;
    sign        : INTEGER;
    strLen      : ADRCARD;
    lastDig     : CHAR;
BEGIN
    i := 0;
    int := 0;
    sign := 1;
    res := strAllRight;

    strLen := LENGTH(str);

    WHILE (i < strLen) AND (str[i] = ' ') DO
        INC(i);
    END;

    (* if the string is empty then exit*)

    IF i = strLen THEN
        res := strEmpty;
        RETURN;
    END;

    lastDig := CHR( (MAX(INTEGER) REM 10) + INT('0') );
    IF i < strLen THEN
        IF str[i] = "-" THEN
            sign := -1;
            INC(i);
            lastDig := CHR( ABS(MIN(INTEGER) REM 10) + INT('0') );
        ELSIF str[i] = "+" THEN
            INC(i);
        END;
    END;

    WHILE (i < strLen) AND
          IsNumeric(str[i]) AND
          (res <> strOutOfRange)
    DO
        IF (int < maxDiv10) OR
           (
            (int = maxDiv10) AND (str[i] <= lastDig)
           )
        THEN
            int := int * 10;
            int := int + (INT(str[i]) - INT('0'));
            INC(i);
            res := strAllRight;
        ELSE
            res := strOutOfRange;
        END;
    END;

    IF (res = strAllRight) AND (i <> strLen) THEN
        res := strWrongFormat;
    END;

    IF res = strAllRight THEN
        int := int * sign;
    ELSE
        int := 31415;
    END;
END StrToInt;

PROCEDURE IntToStr(int : INTEGER; VAR str : ARRAY OF CHAR);
(* modified by Stony brook *)
  (* Converts the value of int to string form and copies the possibly
     truncated result to str. *)
VAR
    neg         : BOOLEAN;
    i           : ADRINT;
    pos         : ADRCARD;
    highStr     : ADRCARD;
    buf         : ARRAY [0..31] OF CHAR;
BEGIN
    neg := (int < 0);

    i := 0;
    REPEAT
        buf[i] := Digits[ABS(int REM 10)];
        int := int / 10;
        INC(i);
    UNTIL int = 0;

    DEC(i);

    pos := 0;
    IF neg THEN
        str[0] := '-';
        pos := 1;
    END;

    highStr := HIGH(str);
    WHILE (i >= 0) AND (pos <= highStr) DO
        str[pos] := buf[i];
        DEC(i);
        INC(pos);
    END;

    IF pos <= highStr THEN
        str[pos] := 0C;
    END;
END IntToStr;

PROCEDURE StrToCard(str : ARRAY OF CHAR;
                    VAR card : CARDINAL;
                    VAR res : ConvResults);
(* modified by Stony brook *)

    (* Ignores any leading spaces in str. If the subsequent characters in
     str are in the format of an unsigned whole number, assigns a
     corresponding value to card. Assigns a value indicating the format
     of str to res.  *)
CONST
    maxDiv10    = MAX(CARDINAL) / 10;
    lastDig     = CHR( (MAX(CARDINAL) REM 10) + ORD('0') );
VAR
    i           : ADRCARD;
    strLen      : ADRCARD;
BEGIN
    i := 0;
    card := 0;
    res := strAllRight;

    strLen := LENGTH(str);

    WHILE (i < strLen) AND (str[i] = ' ') DO
        INC(i);
    END;

    (* if the string is empty then exit*)

    IF i = strLen THEN
        res := strEmpty;
        RETURN;
    END;

    WHILE (i < strLen) AND
          IsNumeric(str[i]) AND
          (res <> strOutOfRange)
    DO
        IF (card < maxDiv10) OR
           (
            (card = maxDiv10) AND (str[i] <= lastDig)
           )
        THEN
            card := card * 10;
            card := card + (ORD(str[i]) - ORD('0'));
            INC(i);
            res := strAllRight;
        ELSE
            res := strOutOfRange;
        END;
    END;

    IF (res = strAllRight) AND (i <> strLen) THEN
        res := strWrongFormat;
    END;

    IF res <> strAllRight THEN
        card := 31415;
    END;
END StrToCard;

PROCEDURE CardToStr(card : CARDINAL; VAR str : ARRAY OF CHAR);
(* modified by Stony brook *)
  (* Converts the value of card to string form and copies the possibly
     truncated result to str. *)
VAR
    i           : ADRINT;
    pos         : ADRCARD;
    highStr     : ADRCARD;
    buf         : ARRAY [0..31] OF CHAR;
BEGIN
    i := 0;
    REPEAT
        buf[i] := Digits[card REM 10];
        card := card / 10;
        INC(i);
    UNTIL card = 0;

    DEC(i);
    pos := 0;

    highStr := HIGH(str);
    WHILE (i >= 0) AND (pos <= highStr) DO
        str[pos] := buf[i];
        DEC(i);
        INC(pos);
    END;

    IF pos <= highStr THEN
        str[pos] := 0C;
    END;
END CardToStr;

END WholeStr.
