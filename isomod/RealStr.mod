IMPLEMENTATION MODULE RealStr;


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

  (* REAL/string conversions *)

(* the string form of a signed fixed-point real number is
     ["+" | "-"], decimal digit, {decimal digit}, [".", {decimal digit}]
*)

(* the string form of a signed floating-point real number is
     signed fixed-point real number, "E", ["+" | "-"], decimal digit, {decimal digit}
*)

FROM SYSTEM IMPORT
    ADRCARD, CAST;

FROM CharClass IMPORT
    IsNumeric;

FROM SpecialReals IMPORT
	IsFinite, IsPositiveInfinity, IsNegativeInfinity, IsQNaN;

CONST
    MaxRoundArray       = 7;
    MaxPowerArray       = 5;

TYPE
    RoundArray  = ARRAY [0..MaxRoundArray] OF REAL;
    PowerArray  = ARRAY [0..MaxPowerArray] OF REAL;

CONST
    (* the small extra value is due to the imperfect nature of IEEE numbers *)
    RoundValue  = RoundArray{
                             0.5,
                             5.0E-2 + 1.0E-7,
                             5.0E-3 + 1.0E-7,
                             5.0E-4 + 1.0E-7,
                             5.0E-5 + 1.0E-7,
                             5.0E-6 + 1.0E-7,
                             5.0E-7,
                             5.0E-8
                            };

    Powers = PowerArray{
                        1.0E1,
                        1.0E2,
                        1.0E4,
                        1.0E8,
                        1.0E16,
                        1.0E32
                       };

PROCEDURE StrToReal(str : ARRAY OF CHAR;
                    VAR real : REAL;
                    VAR res : ConvResults);
(* modified by Stony brook *)
  (* Ignores any leading spaces in str.
     If the subsequent characters in str are in the format of a signed real
     number, assigns a corresponding value to real.
     Assigns a value indicating the format of str to res.
  *)
VAR
    i, j                : ADRCARD;
    exp                 : CARDINAL;
    pow2                : CARDINAL;
    sign, frac          : REAL;
    fracPlace           : REAL;
    strLen              : ADRCARD;
    neg                 : BOOLEAN;
BEGIN
    i := 0;
    real := 0.0;
    sign := 1.0;
    res := strAllRight;

    strLen := LENGTH(str);

    (* skip over spaces *)

    WHILE (i < strLen) AND (str[i] = ' ') DO
        INC(i);
    END;

    (* if the string is empty then exit*)

    IF i = strLen THEN
        res := strEmpty;
        RETURN;
    END;

    (* get the sign, if there is one *)

    IF i < strLen THEN
        IF str[i] = "-" THEN
            sign := -1.0;
            INC(i);
        ELSIF str[i] = "+" THEN
            INC(i);
        END;
    END;

    (* get numbers *)

    WHILE (i < strLen) AND IsNumeric(str[i]) DO
        real := (real * 10.0) + FLOAT(INT(str[i]) - INT('0'));
        INC(i);
    END;

    (* if there is a fractional part *)

    IF (i < strLen) AND (str[i] = ".") THEN
        INC(i);
        fracPlace := 10.0;

        WHILE (i < strLen) AND IsNumeric(str[i]) DO
            frac := FLOAT(INT(str[i]) - INT('0')) / fracPlace;
            fracPlace := fracPlace * 10.0;
            real := real + frac;
            INC(i);
        END;
    END;

    real := real * sign;

    (* if an exponent *)

    IF (i < strLen) AND ((str[i] = "E") OR (str[i] = "e")) THEN
        INC(i);
        exp := 0;
        neg := FALSE;

        (* if the exponent is negative *)

        IF (i < strLen) AND (str[i] = "-") THEN
            neg := TRUE;
            INC(i);
        ELSIF (i < strLen) AND (str[i] = "+") THEN
            INC(i);
        END;

        (* get numbers *)

        WHILE (i < strLen) AND IsNumeric(str[i]) DO
            exp := (exp * 10) + (ORD(str[i]) - ORD('0'));
            INC(i);
        END;

        pow2 := 32;
        FOR j := MaxPowerArray TO 0 BY -1 DO
            IF exp >= pow2 THEN
                IF NOT neg THEN
                    real := real * Powers[j];
                ELSE
                    real := real / Powers[j];
                END;
                exp := exp - pow2;
            END;
            pow2 := pow2 / 2;
        END;
    END;

    (* Check for a valid resulting number *)

    IF Infinite(real) THEN
        res := strOutOfRange;
    END;

    IF (res = strAllRight) AND (i <> strLen) THEN
        res := strWrongFormat;
    END;

    IF res <> strAllRight THEN
        real := 3.141592654;
    END;
END StrToReal;

PROCEDURE AddToString(ch : CHAR; VAR i : ADRCARD; VAR s : ARRAY OF CHAR);
VAR
    high        : ADRCARD;
BEGIN
    high := HIGH(s);
    IF i <= high THEN
        s[i] := ch;
        INC(i);
    END;
END AddToString;

PROCEDURE WriteZero(VAR s : ARRAY OF CHAR; sigFigs : CARDINAL);
VAR
    i           : ADRCARD;
    highS       : ADRCARD;
    digits      : CARDINAL;
BEGIN
    i := 0;
    AddToString("0", i, s);
    AddToString(".", i, s);
    digits := 1;
    highS := HIGH(s);
    WHILE (i <= highS) AND (digits < sigFigs) DO
        s[i] := "0";
        INC(i);
        INC(digits);
    END;
    AddToString("E", i, s);
    AddToString("+", i, s);
    AddToString("0", i, s);
    AddToString("0", i, s);
    AddToString('', i, s);
END WriteZero;

PROCEDURE WriteExponent(VAR s : ARRAY OF CHAR;
                        VAR i : ADRCARD;
                        exp : INTEGER);
BEGIN
    AddToString("E", i, s);
    IF exp < 0 THEN
        AddToString("-", i, s);
    ELSE
        AddToString("+", i, s);
    END;
    exp := ABS(exp);

    AddToString(CHR((exp / 10) + INT('0')), i, s);
    AddToString(CHR((exp REM 10) + INT('0')), i, s);
END WriteExponent;

PROCEDURE Infinite(R : REAL) : BOOLEAN;
(* check for infinity/NaN *)
BEGIN
    RETURN (CAST(CARDINAL32, R) BAND 7F800000h) = 7F800000h;
END Infinite;

PROCEDURE RealToFloat(real : REAL;
                      sigFigs : ADRCARD;
                      VAR str : ARRAY OF CHAR);
(* modified by Stony brook *)
  (* Converts the value of real to floating-point string form, with sigFigs
     significant figures, and copies the possibly truncated result to str.
  *)
VAR
    i, digits   : ADRCARD;
    j           : ADRCARD;
    exp         : INTEGER;
    pow2        : CARDINAL;
BEGIN
    i := 0;
    exp := 0;
    digits := 0;

	(* if the nuber is some special value *)

    IF ~ IsFinite(real) THEN
		IF IsPositiveInfinity(real) THEN
		    str := "Infinity";
		ELSIF IsNegativeInfinity(real) THEN
		    str := "-Infinity";
		ELSIF IsQNaN(real) THEN
		    str := "Quiet NaN";
		ELSE
		    str := "Signaling NaN";
		END;
        RETURN;
    END;

    (* if the number is zero, exit *)

    IF real = 0.0 THEN
        WriteZero(str, sigFigs);
        RETURN;
    END;

    IF sigFigs = 0 THEN
        sigFigs := 6;
    END;

    (* if negative *)

    IF real < 0.0 THEN
        AddToString("-", i, str);
        real := -real;
    END;

    (* force the number between 0 and 10 *)

    IF real >= 10.0 THEN
        pow2 := 32;
        FOR j := MaxPowerArray TO 0 BY -1 DO
            IF real > Powers[j] THEN
                real := real / Powers[j];
                exp := exp + INT(pow2);
            END;
            pow2 := pow2 / 2;
        END;

    ELSIF real < 1.0 THEN
        pow2 := 32;
        FOR j := MaxPowerArray TO 0 BY -1 DO
            IF real <= (1.0 / Powers[j]) THEN
                real := real * Powers[j];
                exp := exp - INT(pow2);
            END;
            pow2 := pow2 / 2;
        END;
        IF real < 1.0 THEN
            real := real * 10.0;
            DEC(exp)
        END;
    END;

    (* round off *)

    IF sigFigs-1 <= MaxRoundArray THEN
        real := real + RoundValue[sigFigs-1];
    END;
    IF real >= 10.0 THEN
        real := real / 10.0;
        INC(exp);
    END;

    (* first digit *)

    AddToString(CHR(TRUNC(real) + ORD('0')), i, str);
    real := real - FLOAT(TRUNC(real));
    real := real * 10.0;
    digits := 1;

    (* write fractional part *)

    IF sigFigs > 1 THEN
        AddToString('.', i, str);

        WHILE digits < sigFigs DO
            AddToString(CHR(TRUNC(real) + ORD('0')), i, str);
            real := real - FLOAT(TRUNC(real));
            real := real * 10.0;
            INC(digits);
        END;
    END;

    WriteExponent(str, i, exp);

    (* terminate the string *)

    AddToString('', i, str);
END RealToFloat;

PROCEDURE RealToEng(real : REAL;
                    sigFigs : ADRCARD;
                    VAR str : ARRAY OF CHAR);
(* modified by Stony brook *)
  (* Converts the value of real to floating-point string form, with sigFigs
     significant figures, and copies the possibly truncated result to str.
     The number is scaled with one to three digits in the whole number part
     and with an exponent that is a multiple of three. *)
VAR
    i, j, digits        : ADRCARD;
    exp                 : INTEGER;
    pow2                : CARDINAL;
BEGIN
    i := 0;
    exp := 0;
    digits := 0;

    (* if the number is zero, exit *)

    IF real = 0.0 THEN
        WriteZero(str, sigFigs);
        RETURN;
    ELSIF Infinite(real) THEN
        str := "Infinite";
        RETURN;
    END;

    IF sigFigs = 0 THEN
        sigFigs := 6;
    END;

    (* if negative *)

    IF real < 0.0 THEN
        AddToString("-", i, str);
        real := -real;
    END;

    (* force the number between 0 and 999 *)

    IF real >= 1000.0 THEN
        REPEAT
            IF real >= 1.0E7 THEN
                real := real / 1.0E4;
                exp := exp + 4;
            ELSIF real >= 1.0E5 THEN
                real := real / 1.0E2;
                exp := exp + 2;
            ELSE
                real := real / 10.0;
                INC(exp);
            END;
        UNTIL (real < 1000.0) AND ((exp REM 3) = 0);

    ELSIF real < 1.0 THEN
        pow2 := 32;
        FOR j := MaxPowerArray TO 0 BY -1 DO
            IF real <= (1.0 / Powers[j]) THEN
                real := real * Powers[j];
                exp := exp - INT(pow2);
            END;
            pow2 := pow2 / 2;
        END;
        IF real < 1.0 THEN
            real := real * 10.0;
            DEC(exp)
        END;

        (* now greater than one. get the exponent correct *)

        WHILE (exp REM 3) <> 0 DO
            real := real * 10.0;
            DEC(exp)
        END;
    END;

    (* back number up to less than 10 *)

    digits := 1;
    WHILE real >= 10.0 DO
        real := real / 10.0;
        INC(digits);
    END;

    (* round off *)

    IF sigFigs-1 <= MaxRoundArray THEN
        real := real + RoundValue[sigFigs-1];
    END;
    IF real >= 10.0 THEN
        real := real / 10.0;
        INC(digits);
    END;

    (* write numbers *)

    FOR j := 1 TO digits DO
        IF j <= sigFigs THEN
            AddToString(CHR(TRUNC(real) + ORD('0')), i, str);
        ELSE
            AddToString("0", i, str);
        END;
        real := real - FLOAT(TRUNC(real));
        real := real * 10.0;
    END;

    (* write fractional part *)

    IF sigFigs > digits THEN
        AddToString('.', i, str);

        WHILE digits < sigFigs DO
            AddToString(CHR(TRUNC(real) + ORD('0')), i, str);
            real := real - FLOAT(TRUNC(real));
            real := real * 10.0;
            INC(digits);
        END;
    END;

    WriteExponent(str, i, exp);

    (* terminate the string *)

    AddToString('', i, str);
END RealToEng;

PROCEDURE RealToFixed(real : REAL;
                      place : INTEGER;
                      VAR str : ARRAY OF CHAR);
(* modified by Stony brook *)
  (* Converts the value of real to fixed-point string form, rounded to the
     given place relative to the decimal point, and copies the possibly
     truncated result to str. *)
VAR
    i, digits   : ADRCARD;
    j           : INTEGER;
    round       : CARDINAL;

    PROCEDURE WriteZero(VAR s : ARRAY OF CHAR);
    VAR
        highS   : ADRCARD;
    BEGIN
        AddToString("0", i, s);
        IF place > 0 THEN
            AddToString(".", i, s);
        END;
        digits := 0;
        highS := HIGH(s);
        WHILE (i <= highS) AND (INT(digits) < place) DO
            s[i] := "0";
            INC(i);
            INC(digits);
        END;
        AddToString('', i, s);
    END WriteZero;

BEGIN
    i := 0;
    digits := 0;

    (* if number is zero, exit *)

    IF real = 0.0 THEN
        WriteZero(str);
        RETURN;
    ELSIF Infinite(real) THEN
        str := "Infinite";
        RETURN;
    END;

    (* if negative *)

    IF real < 0.0 THEN
        AddToString("-", i, str);
        real := -real;
    END;

    (* force real to less than 10 *)

    round := place;
    digits := 1;
    WHILE real >= 10.0 DO
        IF real >= 1.0E4 THEN
            real := real / 1.0E4;
            digits := digits + 4;
            round := round + 4;
        ELSIF real >= 1.0E2 THEN
            real := real / 1.0E2;
            digits := digits + 2;
            round := round + 2;
        ELSE
            real := real / 10.0;
            INC(digits);
            round := round + 1;
        END;
    END;

    (* round off *)

    IF round < 0 THEN
        round := 0;
    ELSIF round > MaxRoundArray THEN
        round := MaxRoundArray;
    END;
    real := real + RoundValue[round];
    IF real >= 10.0 THEN
        real := real / 10.0;
        INC(digits);
    END;

    (* write digits *)

    FOR j := 1 TO INT(digits) DO
        IF j < 8 THEN
            AddToString(CHR(TRUNC(real) + ORD('0')), i, str);
        ELSE
            AddToString("0", i, str);
        END;
        real := real - FLOAT(TRUNC(real));
        real := real * 10.0;
    END;

    (* write fractional part *)

    IF place >= 0 THEN
        place := place + INT(digits);
        AddToString(".", i, str);

        WHILE INT(digits) < place DO
            IF digits < 8 THEN
                AddToString(CHR(TRUNC(real) + ORD('0')), i, str);
            ELSE
                AddToString("0", i, str);
            END;
            real := real - FLOAT(TRUNC(real));
            real := real * 10.0;
            INC(digits);
        END;
    END;

    (* terminate the string *)

    AddToString('', i, str);
END RealToFixed;

PROCEDURE RealToStr(real : REAL; VAR str : ARRAY OF CHAR);
  (* Converts the value of real as RealToFixed if the sign and magnitude
     can be shown within the capacity of str, or otherwise as RealToFloat,
     and copies the possibly truncated result to str. The number of places
     or significant digits depend on the capacity of str. *)
BEGIN
    IF (ABS(real) > 999999.0) OR
       ((ABS(real) < 0.000001) AND (ABS(real) > 0.0))
    THEN
        RealToFloat(real, 8, str);
    ELSE
        RealToFixed(real, 8, str);
    END;
END RealToStr;

END RealStr.
