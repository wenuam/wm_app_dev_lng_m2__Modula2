(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE RConversions;


FROM SYSTEM IMPORT
    ADRCARD;

FROM CharClass IMPORT
    IsNumeric;

FROM LongStr IMPORT
    RealToFloat, RealToFixed;

FROM SpecialReals IMPORT
	Infinity, MinusInfinity, QNaN, SNaN, IsFinite;


CONST
    MaxPowerArray       = 8;

TYPE
    PowerArray  = ARRAY [0..MaxPowerArray] OF LONGREAL;

CONST
    Powers = PowerArray{
                        1.0E1,
                        1.0E2,
                        1.0E4,
                        1.0E8,
                        1.0E16,
                        1.0E32,
                        1.0E64,
                        1.0E128,
                        1.0E256
                       };

(* Convert a real number to a string using exponentional notation with *)
(* one digit in front of the decimal point and the specified number of *)
(* digits.  digits should be between 1 and 17 *)

PROCEDURE RealToString(R : LONGREAL;
                       digits : CARDINAL;
                       VAR OUT S : ARRAY OF CHAR;
                       VAR INOUT I : CARDINAL;
                       VAR OUT Done : BOOLEAN);
VAR
    temp        : ARRAY [0..63] OF CHAR;
    j, l        : ADRCARD;

    PROCEDURE PutChar(c : CHAR);
    BEGIN
        IF I <= HIGH(S) THEN
            S[I] := c;
            INC(I);
        ELSIF c <> '' THEN
            Done := FALSE;
        END;
    END PutChar;

BEGIN
    Done := TRUE;
    IF digits > 17 THEN
        digits := 17;
    END;

    RealToFloat(R, digits, temp);
	IF temp[0] # '-' THEN
		PutChar(' ');
	END;
    l := LENGTH(temp);
    IF l <> 0 THEN
        FOR j := 0 TO l-1 DO
            PutChar(temp[j]);
        END;
    END;

    IF I <= HIGH(S) THEN
        S[I] := '';
    END;
END RealToString;

(* Convert a LONGREAL number to a string using fixed point format *)
(* digits is the total number of digits to convert, before is  the *)
(* number of digits before the decimal point.  0 <= before <= digits *)

PROCEDURE RealToStringFixed(R : LONGREAL;
                            digits, before : CARDINAL;
                            VAR OUT S : ARRAY OF CHAR;
                            VAR INOUT I : CARDINAL;
                            VAR OUT Done : BOOLEAN);
CONST
    inf         = 'infinity';
VAR
    temp        : ARRAY [0..63] OF CHAR;
    j, l        : ADRCARD;

    PROCEDURE PutChar(c : CHAR);
    BEGIN
        IF I <= HIGH(S) THEN
            S[I] := c;
            INC(I);
        ELSIF c <> '' THEN
            Done := FALSE;
        END;
    END PutChar;

BEGIN
    Done := TRUE;
    IF R < 0.0 THEN
        PutChar('-');
        R := -R;
    END;

    IF ~ IsFinite(R) THEN
        FOR j := 0 TO HIGH(inf) DO
            PutChar(inf[j]);
        END;
        RETURN;
    END;

    (* Check the range of the params *)

    IF digits > 16 THEN
        digits := 16;
    END;
    IF before > digits THEN
        before := digits;
    END;

    RealToFixed(R, digits-before, temp);
    l := LENGTH(temp);
    IF l <> 0 THEN
        FOR j := 0 TO l-1 DO
            PutChar(temp[j]);
        END;
    END;

    IF I <= HIGH(S) THEN
        S[I] := '';
    END;
END RealToStringFixed;

(* Convert a string to a longreal number *)

PROCEDURE StringToReal(S : ARRAY OF CHAR;
                       VAR INOUT I : CARDINAL;
                       VAR OUT R : LONGREAL;
                       VAR OUT Done : BOOLEAN);

VAR
    j                   : ADRCARD;
    exp                 : CARDINAL;
    pow2                : CARDINAL;
    sign, frac          : LONGREAL;
    fracPlace           : LONGREAL;
    real                : LONGREAL;
    ch                  : CHAR;
    hitend              : BOOLEAN;
    gotOne              : BOOLEAN;
    neg                 : BOOLEAN;

    PROCEDURE GetChar;
    BEGIN
        IF (I <= HIGH(S)) AND (S[I] <> '') THEN
            ch := S[I];
            INC(I);
        ELSE
            ch := '';
            hitend := TRUE;
        END;
    END GetChar;

	PROCEDURE GetInfinityKeyword;
	CONST
		Key = "INFINITY";
	VAR
		I : CARDINAL = 1;
	BEGIN
		LOOP
			IF Key[I] = 0C THEN RETURN END;
			GetChar;
			IF Key[I] # CAP(ch) THEN EXIT END;
			INC (I);
		END;
		Done := I >= 3;
		R := Infinity;
	END GetInfinityKeyword;

	PROCEDURE GetKeyword (Key : ARRAY OF CHAR);
	VAR
		I : CARDINAL = 1;
	BEGIN
		LOOP
			IF Key[I] = 0C THEN RETURN END;
			GetChar;
			IF Key[I] # CAP(ch) THEN EXIT END;
			INC (I);
		END;
		Done := FALSE;
	END GetKeyword;

BEGIN
    real := 0.0;
    sign := 1.0;
    hitend := FALSE;
    gotOne := FALSE;
    Done := TRUE;

    (* scan away leading blanks *)

    GetChar;
    WHILE ch = ' ' DO
        GetChar;
    END;

	(* check for special values *)

    CASE CAP(ch) OF
	| "I" :
		GetInfinityKeyword;
	| "N" :
		GetKeyword ("NAN");
		IF Done THEN R := QNaN END;
		RETURN;
	| "S" :
		GetKeyword ("SNAN");
		IF Done THEN R := SNaN END;
		RETURN;
	| "Q" :
		GetKeyword ("QNAN");
		IF Done THEN R := QNaN END;
		RETURN;
	ELSE
	END;

    (* check for a sign *)

    IF ch = '-' THEN
        GetChar;
        sign := -1.0;
    ELSIF ch = '+' THEN
        GetChar;
    END;

	(* check signed infinity *)

	IF CAP(ch) = "I" THEN
		GetInfinityKeyword;
		IF Done & (sign < 0.) THEN
			R := MinusInfinity;
		END;
		RETURN;
	END;

    (* scan away leading zeros *)

    WHILE ch = '0' DO
        gotOne := TRUE;
        GetChar;
    END;

    (* get numbers *)

    WHILE (NOT hitend) AND IsNumeric(ch) DO
        gotOne := TRUE;
        real := (real * 10.0) + LFLOAT(INT(ch) - INT('0'));
        GetChar;
    END;

    IF NOT gotOne THEN
        Done := FALSE;
        RETURN;
    END;

    (* if there is a fractional part *)

    IF (NOT hitend) AND (ch = ".") THEN
        GetChar;
        fracPlace := 10.0;

        WHILE (NOT hitend) AND IsNumeric(ch) DO
            frac := LFLOAT(INT(ch) - INT('0')) / fracPlace;
            fracPlace := fracPlace * 10.0;
            real := real + frac;
            GetChar;
        END;
    END;

    real := real * sign;

    (* if an exponent *)

    IF (NOT hitend) AND ((ch = "E") OR (ch = "e")) THEN
        GetChar;
        exp := 0;
        neg := FALSE;

        (* if the exponent is negative *)

        IF (NOT hitend) AND (ch = "-") THEN
            neg := TRUE;
            GetChar;
        ELSIF (NOT hitend) AND (ch = "+") THEN
            GetChar;
        END;

        (* get numbers *)

        WHILE (NOT hitend) AND IsNumeric(ch) DO
            exp := (exp * 10) + (ORD(ch) - ORD('0'));
            GetChar;
        END;

        pow2 := 256;
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

    R := real;

    (* check for a valid number *)

    IF ~ IsFinite(R) THEN
        Done := FALSE;
    END;

    (* If we did not run off the end of the string, backup to point *)
    (* to the character which stopped our scan *)

    IF NOT hitend THEN
        I := I - 1;
    END;
END StringToReal;

END RConversions.
