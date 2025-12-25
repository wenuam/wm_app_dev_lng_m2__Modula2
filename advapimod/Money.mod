(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 2009                                  *)
(*                        by ADW Software                                  *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE Money;
<*/OPT:T*>

FROM SYSTEM IMPORT
    ADRCARD, ASSERT;

FROM Conversions IMPORT
    LongToString, StringToLong, StringToCard;

<*/VALIDVER:Trivial*>
(*<*/VER:Trivial*>*)

%IF Trivial %THEN

(* these work but limit the numeric range of the operands *)

PROCEDURE Mul(a, b : Money) : Money;
BEGIN
    RETURN ((a * b) + (Scale/2)) / Scale;
END Mul;

PROCEDURE Div(a, b : Money) : Money;
BEGIN
    RETURN (a * Scale) / b;
END Div;

%ELSE

TYPE
    Digit       = CARDINAL32;
    Digit2      = CARDINAL64;
    ResultArray = ARRAY [0..3] OF Digit;

    ASSERT(SIZE(Digit) = (SIZE(Money) / 2));
    ASSERT(SIZE(Digit2) = (SIZE(Digit) * 2));
    ASSERT(SIZE(ResultArray) = (SIZE(Money) * 2));

CONST
    DigitBits   = SIZE(Digit) * 8;
    (*DigitMask   = (1 SHL DigitBits) - 1;*)

PROCEDURE MulPrim(a, b : Money; VAR OUT r : ResultArray);
VAR
    a1, b1      : ARRAY [0..1] OF Digit;
    ai, bi, ri  : ADRCARD;
    temp        : Digit2;
    carry       : Digit;
BEGIN
    FOR ri := 0 TO HIGH(r) DO
        r[ri] := 0;
    END;

    <*/PUSH/NOCHECK:A*>
    a1[0] := a;
    <*/POP*>
    a1[1] := a SHR DigitBits;

    <*/PUSH/NOCHECK:A*>
    b1[0] := b;
    <*/POP*>
    b1[1] := b SHR DigitBits;

    FOR ai := 0 TO HIGH(a1) DO
        FOR bi := 0 TO HIGH(b1) DO
            ri := ai + bi;
            temp := (VAL(Digit2, a1[ai]) * VAL(Digit2, b1[bi])) + VAL(Digit2, r[ri]);
            <*/PUSH/NOCHECK:A*>
            r[ri] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            IF carry <> 0 THEN
                REPEAT
                    INC(ri);
                    temp := VAL(Digit2, r[ri]) + VAL(Digit2, carry);
                    <*/PUSH/NOCHECK:A*>
                    r[ri] := temp;
                    <*/POP*>
                    carry := temp SHR DigitBits;
                UNTIL carry = 0;(*not possible for ri to get out of range*)
            END;
        END;
    END;
END MulPrim;

PROCEDURE DivPrim(VAR INOUT a : ResultArray; b : Money) : Money;
VAR
    a0, a1,
    a2, a3,
    b0, b1      : Digit;
    i           : CARDINAL;
    borrow      : Digit;
BEGIN
    (* get these into simple locals. lets them become register variables *)

    <*/PUSH/NOCHECK:A*>
    b0 := b;
    <*/POP*>
    b1 := b SHR DigitBits;

    a0 := a[0];
    a1 := a[1];
    a2 := a[2];
    a3 := a[3];

    (* shift-subtract algorithm *)

    FOR i := 1 TO 64 DO
        a3 := (a3 SHL 1) BOR (a2 SHR (DigitBits-1));
        a2 := (a2 SHL 1) BOR (a1 SHR (DigitBits-1));
        a1 := (a1 SHL 1) BOR (a0 SHR (DigitBits-1));
        a0 := (a0 SHL 1);

        IF (a3 > b1) OR ((a3 = b1) AND (a2 >= b0)) THEN
            borrow := ORD(a2 < b0);
            <*/PUSH/NOCHECK:O*>
            a2 := a2 - b0;
            a3 := a3 - b1 - borrow;
            <*/POP*>

            INC(a0);
        END;
    END;

    RETURN (VAL(Money, a1) SHL DigitBits) BOR VAL(Money, a0);
END DivPrim;

PROCEDURE Mul(a, b : Money) : Money;
VAR
    r           : ResultArray;
    result      : Money;
    neg         : BOOLEAN;
    carry       : Digit;
    temp        : Digit2;
    ri          : ADRCARD;
BEGIN
    neg := FALSE;
    IF a < 0 THEN
        a := -a;
        neg := TRUE;
    END;
    IF b < 0 THEN
        b := -b;
        neg := NOT neg;
    END;

    MulPrim(a, b, r);

    (* intermediate result has double the decimal places.
       round this to our last place before we scale down.
    *)
    temp := VAL(Digit2, r[0]) + (Scale / 2);
    <*/PUSH/NOCHECK:A*>
    r[0] := temp;
    <*/POP*>
    carry := temp SHR DigitBits;
    IF carry <> 0 THEN
        ri := 0;
        REPEAT
            INC(ri);
            temp := VAL(Digit2, r[ri]) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            r[ri] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;
        UNTIL carry = 0;
    END;

    (* scale down *)

    result := DivPrim(r, Scale);

    IF NOT neg THEN
        RETURN result;
    END;
    RETURN -result;
END Mul;

PROCEDURE Div(a, b : Money) : Money;
VAR
    r           : ResultArray;
    result      : Money;
    neg         : BOOLEAN;
BEGIN
    neg := FALSE;
    IF a < 0 THEN
        a := -a;
        neg := TRUE;
    END;
    IF b < 0 THEN
        b := -b;
        neg := NOT neg;
    END;

    (* you lose precision if you multiply by scale after the divide,
       so we multiply the dividend by scale before the divide.
    *)
    MulPrim(a, Scale, r);
    result := DivPrim(r, b);

    IF NOT neg THEN
        RETURN result;
    END;
    RETURN -result;
END Div;

%END

PROCEDURE MulInt(a : Money; intVal : INTEGER) : Money;
BEGIN
    RETURN Mul(a, IntToMoney(intVal));
END MulInt;

PROCEDURE DivInt(a : Money; intVal : INTEGER) : Money;
BEGIN
    RETURN Div(a, IntToMoney(intVal));
END DivInt;

PROCEDURE Percent(m : Money; fract : Fraction) : Money;
BEGIN
    RETURN Mul(m, MakeFraction(fract));
END Percent;

PROCEDURE MoneyToString(num : Money;
                        places : CARDINAL;
                        VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
CONST
    round       : ARRAY [0..DecPlaces] OF CARDINAL =
                        {Half, Half/10, Half/100, Half/1000, 0};

    chop        : ARRAY [1..DecPlaces] OF CARDINAL = {1000, 100, 10, 1};

VAR
    i, j        : CARDINAL;
    ok          : BOOLEAN;
    place       : CARDINAL;
BEGIN
    IF places > DecPlaces THEN
        places := DecPlaces;
    END;

    num := num + MakeFraction(round[places]);

    i := 0;
    LongToString(MoneyToInt64(num), 0, str, i, ok);
    IF ok AND (i <= HIGH(str)) THEN
        IF places > 0 THEN
            str[i] := '.';
            INC(i);

            place := GetFraction(num);
            place := place / chop[places];

            IF i+places-1 <= HIGH(str) THEN
                FOR j := places-1 TO 0 BY -1 DO
                    str[i+j] := CHR((place REM 10) + ORD('0'));
                    place := place / 10;
                END;
                i := i + places;
            ELSE
                ok := FALSE;
            END;
        END;

        IF i <= HIGH(str) THEN
            str[i] := '';
        END;
    END;
    RETURN ok;
END MoneyToString;

PROCEDURE StringToMoney(str : ARRAY OF CHAR; VAR OUT num : Money) : BOOLEAN;
VAR
    i           : CARDINAL;
    ok          : BOOLEAN;
    whole       : INTEGER64;
    fract       : CARDINAL;
BEGIN
    fract := 0;
    whole := 0;
    i := 0;

    StringToLong(str, i, whole, ok);
    IF ok THEN
        IF (i <= HIGH(str)) AND (str[i] = '.') THEN
            INC(i);
            StringToCard(str, i, fract, ok);
            IF ok THEN
                IF fract >= Scale THEN
                    WHILE fract >= Scale*10 DO
                        fract := fract / 10;
                    END;

                    (* at this point fract has one digit too many.
                       round and get fract into the proper range.
                    *)
                    fract := (fract+5) / 10;

                    IF fract >= Scale THEN
                        (* if the rounding made it too big, then increment
                           the whole part and set the fractional part to zero.
                        *)
                        INC(whole);
                        fract := 0;
                    END;
                END;
            END;
        END;
    END;

    num := MakeMoney(whole, fract);

    RETURN ok AND ((i > HIGH(str)) OR (str[i] = ''));
END StringToMoney;

END Money.
