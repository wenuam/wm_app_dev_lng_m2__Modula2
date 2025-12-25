(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE LWholeStr;

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

PROCEDURE StrToLongInt(str : ARRAY OF CHAR;
                       VAR int : LONGINT;
                       VAR res : ConvResults);
  (* Ignores any leading spaces in str. If the subsequent characters in
     str are in the format of a signed whole number, assigns a corresponding
     value to int. Assigns a value indicating the format of str to res.  *)
CONST
    maxDiv10    = MAX(LONGINT) / 10;
VAR
    i           : ADRCARD;
    sign        : LONGINT;
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

    lastDig := CHR( (MAX(LONGINT) REM 10) + INT('0') );
    IF i < strLen THEN
        IF str[i] = "-" THEN
            sign := -1;
            INC(i);
            lastDig := CHR( ABS(MIN(LONGINT) REM 10) + INT('0') );
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
            int := int + VAL(LONGINT, (INT(str[i]) - INT('0')));
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
END StrToLongInt;

PROCEDURE LongIntToStr(int : LONGINT; VAR str : ARRAY OF CHAR);
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
END LongIntToStr;

PROCEDURE StrToLongCard(str : ARRAY OF CHAR;
                        VAR card : LONGCARD;
                        VAR res : ConvResults);
    (* Ignores any leading spaces in str. If the subsequent characters in
     str are in the format of an unsigned whole number, assigns a
     corresponding value to card. Assigns a value indicating the format
     of str to res.  *)
CONST
    (* cannot do constant divides with values > MAX(LONGINT) without 128 bit internal type *)
    (* maxDiv10 was computed by Dividing MAX(LONGINT) by 10 *)
    maxDiv10    = 1999999999999999h;
    lastDig     = '5';
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
            card := card + VAL(LONGCARD, ORD(str[i]) - ORD('0'));
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
END StrToLongCard;

PROCEDURE LongCardToStr(card : LONGCARD; VAR str : ARRAY OF CHAR);
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
        str[pos] := '';
    END;
END LongCardToStr;

END LWholeStr.
