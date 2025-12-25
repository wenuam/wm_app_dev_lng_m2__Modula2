(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE LowReal;

FROM SYSTEM IMPORT
    SHIFT, CAST, UNREFERENCED_PARAMETER, IsThread;

FROM EXCEPTIONS IMPORT
    ExceptionSource, AllocateSource, RaiseRTL, IsCurrentSource;


(* this module uses set arithmetic to do bit operations *)
(* very ugly, but ISO portable *)

TYPE
    SET32       = PACKEDSET OF [0..31];

    (* use this to get at the various pieces of an IEEE number *)
    (* IEEE 754, 854 32-bit number *)

    (*
    Bits 0-22 are the significand. The highest bit of the actual
        significand is always one and is not stored. Thus we have a 24-bit
        effective significand.
        Number between 1-2.

    Bits 23-30 are the exponent, biased by 127.
        2**exponent
        twos complement once the bias is removed.
        The reason for the bias is that relational comparisons can be
        done as though the number were a 32bit unsigned integer.
    Bit 31 is the sign. Set = negative.
    *)

    (*
        A 32-bit IEEE number is determined by.
        if 0 < exp < 255
            x = (-1**sign) * 2**(exp-127) * (1.sig)
        if exp = 0 and sig <> 0
            x = (-1**sign) * 2**(-126) * (0.sig)
        if exp = 0 and sig = 0
            x = (-1**sign) * 0
        if exp = 255 and sig = 0
            x = (-1**sign) * Infinity
        if exp = 255 and sig <> 0
            x = NaN, Not a Number
    *)

    <*/PUSH/PACK/NOWARN:A*>
    Split       =
        RECORD
        CASE : CARDINAL OF
        0:
            r           : REAL;
        |
        1:
            dws         : SET32;
        |
        6:
            dw          : CARDINAL32; (* 32bit numeric *)
        ELSE
        END;
        END;
    <*/POP*>

CONST
    Bias        = 127;

VAR
    LowSrc      : ExceptionSource;

PROCEDURE exponent(x : REAL) : INTEGER;
(* returns the exponent value of x *)
VAR
    sp  : Split;
BEGIN
    IF x <> 0.0 THEN
        sp.r := x;
        RETURN CAST(INTEGER, SHIFT(sp.dws * SET32{23..30}, -23)) - Bias;
    ELSE
        RaiseRTL(LowSrc, 0, 'EXPONENT-EXCEPTION');
    END;
END exponent;

PROCEDURE fraction(x : REAL) : REAL;
(* returns the significand (or significant part) of x *)
VAR
    sp  : Split;
    sp1 : Split;
BEGIN
    sp.r := x;

    (* clear out the exponent field *)

    sp.dws := sp.dws * SET32{0..22, 31};

    (* get an exponent of 2**0 *)

    sp1.dw := Bias;

    (* shift it into position *)

    sp1.dws := SHIFT(sp1.dws, 23);

    (* or it into the result *)

    sp.dws := sp.dws + sp1.dws;

    RETURN sp.r;
END fraction;

PROCEDURE sign(x : REAL) : REAL;
(* returns the signum of x *)
BEGIN
    IF x < 0.0 THEN
        RETURN -1.0;
    ELSE
        RETURN 1.0;
    END;
END sign;

PROCEDURE succ(x : REAL) : REAL;
(* returns the next value of the type REAL greater than x *)
BEGIN
    RETURN x+ulp(x);
END succ;

PROCEDURE ulp(x : REAL) : REAL;
(* returns the value of a unit in the last place of x *)
VAR
    sp  : Split;
    sp1 : Split;
    exp : INTEGER;
BEGIN
    sp.r := x;

    exp := CAST(INTEGER, SHIFT(sp.dws * SET32{23..30}, -23)) - Bias;

    (* zap the significand. this leaves 1.0 x 2**exp *)
    (* also dump the exponent as well *)

    sp.dws := sp.dws * SET32{31};

    (* subtract from the exponent the number of significand places *)

    exp := exp - 23;
    IF exp < expoMin THEN
        exp := expoMin;
    END;
    exp := exp + Bias;

    (* shift it into position *)

    sp1.dw := exp;
    sp1.dws := SHIFT(sp1.dws, 23);

    (* or it into the result *)

    sp.dws := sp.dws + sp1.dws;

    RETURN sp.r;
END ulp;

PROCEDURE pred(x : REAL) : REAL;
(* returns the previous value of the type REAL less than x *)
BEGIN
    RETURN x-ulp(x);
END pred;

PROCEDURE intpart(x : REAL) : REAL;
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

    exp := CAST(INTEGER, SHIFT(sp.dws * SET32{23..30}, -23));
    IF exp = 0 THEN
        (* the assumed bit is assumed 0 in this case *)
        (* therefore the integer part is 0.0 *)

        sp.r := 0.0;
    ELSIF exp < 255 THEN
        (* zap the significand. this leaves 1.0 x 2**exp *)

        sp.dws := sp.dws * SET32{23..31};
    END;

    IF neg THEN
        sp.r := -sp.r;
    END;
    RETURN sp.r;
END intpart;

PROCEDURE fractpart(x : REAL) : REAL;
(* returns the fractional parts of x *)
BEGIN
    RETURN x-intpart(x);
END fractpart;

PROCEDURE scale(x : REAL; n : INTEGER) : REAL;
(* returns the value of (x * radix ** n) *)
VAR
    sp  : Split;
    sp1 : Split;
    exp : INTEGER;
BEGIN
    sp.r := x;
    exp := CAST(INTEGER, SHIFT(sp.dws * SET32{23..30}, -23));
    exp := exp + n;
    IF (0 < exp) AND (exp < 255) THEN
        sp.r := x;

        (* clear out the exponent field *)

        sp.dws := sp.dws * SET32{0..22, 31};

        (* set the exponent *)

        sp1.dw := exp;

        (* shift it into position *)

        sp1.dws := SHIFT(sp1.dws, 23);

        (* or in into the result *)

        sp.dws := sp.dws + sp1.dws;
        RETURN sp.r;
    ELSE
        RaiseRTL(LowSrc, 0, 'SCALE-EXCEPTION');
    END;
END scale;

PROCEDURE trunc(x : REAL; n : INTEGER) : REAL;
(* returns the value of the first n places of x *)
VAR
    sp  : Split;
    dws : SET32;
    i   : INTEGER;
BEGIN
    sp.r := x;

    (* setup a mask, and then take bits out of the mask *)

    dws := SET32{0..31};

    FOR i := 0 TO (23-n)-1 DO
        EXCL(dws, ORD(i));
    END;

    (* bitwise and with the mask *)

    sp.dws := sp.dws * dws;

    RETURN sp.r;
END trunc;

PROCEDURE round(x : REAL; n : INTEGER) : REAL;
(* returns the value of x rounded to the first n places *)
VAR
    sp  : Split;
    dws : SET32;
    i   : INTEGER;
BEGIN
    sp.r := x;

    (* setup a mask, and then take bits out of the mask *)

    dws := SET32{0..31};

    FOR i := 0 TO (23-n)-1 DO
        EXCL(dws, ORD(i));
    END;

    (* bitwise and with the mask *)

    sp.dws := sp.dws * dws;

    RETURN sp.r;
END round;

PROCEDURE synthesize(expart : INTEGER; frapart : REAL) : REAL;
(* returns a value of the type REAL constructed from the given
   expart and frapart
*)
VAR
    sp  : Split;
    sp1 : Split;
    exp : INTEGER;
BEGIN
    sp.r := frapart;
    exp := CAST(INTEGER, SHIFT(sp.dws * SET32{23..30}, -23));
    exp := exp + expart;
    IF (0 < exp) AND (exp < 255) THEN
        sp.r := frapart;

        (* clear out the exponent field *)

        sp.dws := sp.dws * SET32{0..22, 31};

        (* set the exponent *)

        sp1.dw := exp;

        (* shift it into position *)

        sp1.dws := SHIFT(sp1.dws, 23);

        (* or in into the result *)

        sp.dws := sp.dws + sp1.dws;
        RETURN sp.r;
    ELSE
        RaiseRTL(LowSrc, 0, 'SCALE-EXCEPTION');
    END;
END synthesize;

PROCEDURE setMode(m : Modes);
(* sets status flags approiate to the underlying implementation of the
   type REAL
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
END LowReal.
