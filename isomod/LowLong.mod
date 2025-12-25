(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE LowLong;

FROM SYSTEM IMPORT
    SHIFT, CAST, UNREFERENCED_PARAMETER, IsThread;

FROM EXCEPTIONS IMPORT
    ExceptionSource, AllocateSource, RaiseRTL, IsCurrentSource;

(* this module uses set arithmetic to do bit operations *)
(* very ugly, but ISO portable *)
(* the set operations are done 32-bit wide. *)

TYPE
    SET32       = PACKEDSET OF [0..31];

    (* use this to get at the various pieces of an IEEE number *)
    (* IEEE 754, 854 64-bit number *)

    (*
    Bits 0-51 are the significand. The highest bit of the actual
        significand is always one and is not stored. Thus we have a 53-bit
        effective significand.
        Number between 1-2.

    Bits 52-62 are the exponent, biased by 1023.
        2**exponent
        twos complement once the bias is removed.
        The reason for the bias is that relational comparisons can be
        done as though the number were a 64-bit unsigned integer.
    Bit 63 is the sign. Set = negative.
    *)

    (*
        A 64-bit IEEE number is determined by.
        if 0 < exp < 2047
            x = (-1**sign) * 2**(exp-1023) * (1.sig)
        if exp = 0 and sig <> 0
            x = (-1**sign) * 2**(-1022) * (0.sig)
        if exp = 0 and sig = 0
            x = (-1**sign) * 0
        if exp = 2047 and sig = 0
            x = (-1**sign) * Infinity
        if exp = 2047 and sig <> 0
            x = NaN, Not a Number
    *)

    <*/PUSH/PACK/NOWARN:A*>
    Split       =
        RECORD
        CASE : CARDINAL OF
        0:
            r           : LONGREAL;
        |
        1:
            dws         : ARRAY [0..1] OF SET32;
        |
        2:
            dw          : ARRAY [0..1] OF CARDINAL32;
        ELSE
        END;
        END;
    <*/POP*>

CONST
    Bias        = 1023;

    %IF LittleEndian %THEN
        LS      = 0;(*least significant*)
        MS      = 1;(* most *)
    %ELSE
        LS      = 1;
        MS      = 0;
    %END


VAR
    LowSrc      : ExceptionSource;

PROCEDURE exponent(x : LONGREAL) : INTEGER;
(* returns the exponent value of x *)
VAR
    sp  : Split;
BEGIN
    IF x <> 0.0 THEN
        sp.r := x;
        RETURN CAST(INTEGER, SHIFT(sp.dws[MS] * SET32{20..30}, -20)) - Bias;
    ELSE
        RaiseRTL(LowSrc, 0, "EXPONENT-EXCEPTION");
    END;
END exponent;

PROCEDURE fraction(x : LONGREAL) : LONGREAL;
(* returns the significand (or significant part) of x *)
VAR
    sp  : Split;
    sp1 : Split;
BEGIN
    sp.r := x;

    (* clear out the exponent field *)

    sp.dws[MS] := sp.dws[MS] * SET32{0..19, 31};

    (* get an exponent of 2**0 *)

    sp1.dw[MS] := Bias;

    (* shift it into position *)

    sp1.dws[MS] := SHIFT(sp1.dws[MS], 20);

    (* or it into the result *)

    sp.dws[MS] := sp.dws[MS] + sp1.dws[MS];

    RETURN sp.r;
END fraction;

PROCEDURE sign(x : LONGREAL) : LONGREAL;
(* returns the signum of x *)
BEGIN
    IF x < 0.0 THEN
        RETURN -1.0;
    ELSE
        RETURN 1.0;
    END;
END sign;

PROCEDURE succ(x : LONGREAL) : LONGREAL;
(* returns the next value of the type LONGREAL greater than x *)
BEGIN
    RETURN x+ulp(x);
END succ;

PROCEDURE ulp(x : LONGREAL) : LONGREAL;
(* returns the value of a unit in the last place of x *)
VAR
    sp  : Split;
    sp1 : Split;
    exp : INTEGER;
BEGIN
    sp.r := x;

    exp := CAST(INTEGER, SHIFT(sp.dws[MS] * SET32{20..30}, -20)) - Bias;

    (* zap the significand. this leaves 1.0 x 2**exp *)
    (* also dump the exponent as well *)

    sp.dw[LS] := 0;
    sp.dws[MS] := sp.dws[MS] * SET32{31};

    (* subtract from the exponent the number of significand places *)

    exp := exp - 52;
    IF exp < expoMin THEN
        exp := expoMin;
    END;
    exp := exp + Bias;

    (* shift it into position *)

    sp1.dw[MS] := exp;
    sp1.dws[MS] := SHIFT(sp1.dws[MS], 20);

    (* or it into the result *)

    sp.dws[MS] := sp.dws[MS] + sp1.dws[MS];

    RETURN sp.r;
END ulp;

PROCEDURE pred(x : LONGREAL) : LONGREAL;
(* returns the previous value of the type LONGREAL less than x *)
BEGIN
    RETURN x-ulp(x);
END pred;

PROCEDURE intpart(x : LONGREAL) : LONGREAL;
(* returns the integer parts of x *)
VAR
    sp  : Split;
    exp : INTEGER;
    neg : BOOLEAN;
BEGIN
    neg := FALSE;
    IF x < 0.0 THEN
        neg := TRUE;
        x := -x;
    END;
    sp.r := x;

    exp := CAST(INTEGER, SHIFT(sp.dws[MS] * SET32{20..30}, -20));
    IF exp = 0 THEN
        (* the assumed bit is assumed 0 in this case *)
        (* therefore the integer part is 0.0 *)

        sp.r := 0.0;
    ELSIF exp < 2047 THEN
        (* zap the significand. this leaves 1.0 x 2**exp *)

        sp.dw[LS] := 0;
        sp.dws[MS] := sp.dws[MS] * SET32{20..31};
    END;

    IF neg THEN
        sp.r := -sp.r;
    END;
    RETURN sp.r;
END intpart;

PROCEDURE fractpart(x : LONGREAL) : LONGREAL;
(* returns the fractional parts of x *)
BEGIN
    RETURN x-intpart(x);
END fractpart;

PROCEDURE scale(x : LONGREAL; n : INTEGER) : LONGREAL;
(* returns the value of (x * radix ** n) *)
VAR
    sp  : Split;
    sp1 : Split;
    exp : INTEGER;
BEGIN
    sp.r := x;
    exp := CAST(INTEGER, SHIFT(sp.dws[MS] * SET32{20..30}, -20));
    exp := exp + n;
    IF (0 < exp) AND (exp < 2047) THEN
        sp.r := x;

        (* clear out the exponent field *)

        sp.dws[MS] := sp.dws[MS] * SET32{0..19, 31};

        (* set the exponent *)

        sp1.dw[MS] := exp;

        (* shift it into position *)

        sp1.dws[MS] := SHIFT(sp1.dws[MS], 20);

        (* or in into the result *)

        sp.dws[MS] := sp.dws[MS] + sp1.dws[MS];
        RETURN sp.r;
    ELSE
        RaiseRTL(LowSrc, 0, "SCALE-EXCEPTION");
    END;
END scale;

PROCEDURE trunc(x : LONGREAL; n : INTEGER) : LONGREAL;
(* returns the value of the first n places of x *)
VAR
    sp  : Split;
    sp1 : Split;
    i   : INTEGER;
BEGIN
    sp.r := x;

    (* setup a mask, and then take bits out of the mask *)

    sp1.dws[LS] := SET32{0..31};
    sp1.dws[MS] := SET32{0..31};

    FOR i := 0 TO (52-n)-1 DO
        EXCL(sp1.dws[i / 32], ORD(i));
    END;

    (* bitwise and with the mask *)

    sp.dws[LS] := sp.dws[LS] * sp1.dws[LS];
    sp.dws[MS] := sp.dws[MS] * sp1.dws[MS];

    RETURN sp.r;
END trunc;

PROCEDURE round(x : LONGREAL; n : INTEGER) : LONGREAL;
(* returns the value of x rounded to the first n places *)
VAR
    sp  : Split;
    sp1 : Split;
    i   : INTEGER;
BEGIN
    sp.r := x;

    (* setup a mask, and then take bits out of the mask *)

    sp1.dws[LS] := SET32{0..31};
    sp1.dws[MS] := SET32{0..31};

    FOR i := 0 TO (52-n)-1 DO
        EXCL(sp1.dws[i / 32], ORD(i));
    END;

    (* bitwise and with the mask *)

    sp.dws[LS] := sp.dws[LS] * sp1.dws[LS];
    sp.dws[MS] := sp.dws[MS] * sp1.dws[MS];

    RETURN sp.r;
END round;

PROCEDURE synthesize(expart : INTEGER; frapart : LONGREAL) : LONGREAL;
(* returns a value of the type LONGREAL constructed from the given
   expart and frapart
*)
VAR
    sp  : Split;
    sp1 : Split;
    exp : INTEGER;
BEGIN
    sp.r := frapart;
    exp := CAST(INTEGER, SHIFT(sp.dws[MS] * SET32{20..30}, -20));
    exp := exp + expart;
    IF (0 < exp) AND (exp < 2047) THEN
        sp.r := frapart;

        (* clear out the exponent field *)

        sp.dws[MS] := sp.dws[MS] * SET32{0..19, 31};

        (* set the exponent *)

        sp1.dw[MS] := exp;

        (* shift it into position *)

        sp1.dws[MS] := SHIFT(sp1.dws[MS], 20);

        (* or in into the result *)

        sp.dws[MS] := sp.dws[MS] + sp1.dws[MS];
        RETURN sp.r;
    ELSE
        RaiseRTL(LowSrc, 0, "SYNTHESIZE-EXCEPTION");
    END;
END synthesize;

PROCEDURE setMode(m : Modes);
(* sets status flags approiate to the underlying implementation of the
   type LONGREAL
*)
BEGIN
    UNREFERENCED_PARAMETER(m);
END setMode;

PROCEDURE currentMode() : Modes;
(* returns the current status flags in the form set by setMode *)
BEGIN
    RETURN Modes{};
END currentMode;

PROCEDURE IsLowException() : BOOLEAN;
(* returns TRUE if the current coroutine is in the exception state
   because of the raising of an exception in a routine from this module;
   otherwise returns FALSE
*)
BEGIN
    RETURN IsCurrentSource(LowSrc);
END IsLowException;

BEGIN
    (* in case this module is part of a WIN32 DLL, that inits per thread *)

    IF NOT IsThread THEN
        AllocateSource(LowSrc);
    END;
END LowLong.
