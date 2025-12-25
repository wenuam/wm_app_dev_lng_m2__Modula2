IMPLEMENTATION MODULE LongMath;
<*/OPTIMIZE:T*>

(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman

    Stony Brook compiler port Implementation
        copyright (c) 1994-2004
        by ADW Software
=========================================== *)

%IF %NOT IA32 %THEN
FROM SYSTEM IMPORT
	ADRINT, CAST %IF %NOT IA32 %AND %NOT AMD64 %THEN, IsThread %END;
%END

%IF IA32 %OR AMD64 %THEN

FROM SpecialReals IMPORT
	NaN;

%ELSE

FROM EXCEPTIONS IMPORT
    ExceptionSource, RaiseRTL, AllocateSource, IsCurrentSource;

CONST
    NEGATIVE_SQRT_ARG           = 0;
    NONPOSITIVE_LN_ARG          = 1;
    (*
    TAN_OVERFLOW                = 2;
    *)
    ARCSIN_ARG_MAGNITUDE        = 3;
    ARCCOS_ARG_MAGNITUDE        = 4;

    EXP_OVERFLOW                = 5;
    NONPOSITIVE_POWER_ARG       = 6;

VAR
    mathSrc     : ExceptionSource;

%END

%IF InlineFpp %THEN
PROCEDURE sqrt(x : LONGREAL) : LONGREAL [FppPrim, Invariant];
%ELSE
PROCEDURE sqrt(x : LONGREAL) : LONGREAL [Invariant];
%END
BEGIN
	%IF IA32 %OR AMD64 %THEN
    	RETURN SQRT(x);
	%ELSE
	    IF x >= 0.0 THEN
    	    RETURN SQRT(x);
    	ELSE
        	RaiseRTL(mathSrc, NEGATIVE_SQRT_ARG, "NEGATIVE-SQRT-ARG");
	    END;
	%END
END sqrt;

%IF %NOT IA32 %THEN

PROCEDURE poly(p : ARRAY OF LONGREAL; x : LONGREAL) : LONGREAL;
VAR
    i   : CARDINAL;
    s   : LONGREAL;
BEGIN
    s := p[HIGH(p)];
    FOR i := HIGH(p)-1 TO 0 BY -1 DO
        s := s * x + p[i];
    END;
    RETURN s;
END poly;

TYPE
    words       = ARRAY [0..3] OF INTEGER16;

%END

CONST
    piover4     = pi / 4.0;
    piover2     = pi / 2.0;
%IF IA32 %THEN
    zero        = 0.0;
%ELSE
	%IF LittleEndian %THEN
		ExpWord     = 3;
	%ELSE
		ExpWord     = 0;
	%END
    ln2         = 0.69314718055994530941723;
    sqrt2       = 1.41421356237309504880169;
%END

%IF IA32 %THEN

PROCEDURE fexp(x : LONGREAL) : LONGREAL; ASSEMBLER;
%IF Bits32 %THEN
ASM
        FLD     x
        FLD1
        FLDL2E
        FMUL    ST, ST(2)
        FST     ST(2)
        FPREM
        F2XM1
        FADDP   ST(1), ST
        FSCALE
        FSTP    ST(1)
%ELSE
    fix me
%END
END fexp;

%ELSE

CONST
    (* 18.08 digits *)
    expp : ARRAY [0..2] OF LONGREAL =
        {
        +0.1513906799054338915894328E+4,
        +0.20202065651286927227886E+2,
        +0.23093347753750233624E-1
        };

    expq : ARRAY [0..2] OF LONGREAL =
        {
         +0.4368211662727558498496814E+4,
         +0.233184211427481623790295E+3,
         +0.1E+1
        };

%END

%IF InlineFpp %AND IA32 %THEN
PROCEDURE exp(x : LONGREAL) : LONGREAL [FppPrim, Invariant];
%ELSE
PROCEDURE exp(x : LONGREAL) : LONGREAL [Invariant];
%END
VAR
	%IF IA32 %THEN
	    lr                  : LONGREAL;
	%ELSE
		px, qx, expx, xsq   : LONGREAL;
		pow                 : INTEGER32;
		ts                  : INTEGER32;
		int16               : INTEGER16;
		half                : BOOLEAN;
    %END
BEGIN
    %IF IA32 %THEN
        lr := fexp(x);
		RETURN lr;
	%ELSE

		expx := x / ln2;
		pow := VAL(INTEGER32, expx);
		expx := expx - VAL(LONGREAL, pow);
		IF expx < 0.0 THEN
			expx := expx + 1.0;
			pow := pow - 1;
		END;

		half := FALSE;
		IF expx >= 0.5 THEN
			expx := expx - 0.5;
			half := TRUE;
		END;

		xsq := expx * expx;
		px := expx * poly(expp, xsq);
		qx := poly(expq, xsq);
		expx := (qx + px) / (qx - px);
		IF half THEN
			expx := expx * sqrt2;
		END;

		ts := ((expx:words[ExpWord] SHL 1) SHR 5) - 3FEh;
		ts := ts + pow;
		IF ts < -1022 THEN
			RETURN 0.0;
		ELSIF ts > 1023 THEN
			%IF AMD64 %THEN
			expx := MAX(LONGREAL);
			RETURN expx + 1.; (* provoke overflow exception *)
			%ELSE
				RaiseRTL(mathSrc, EXP_OVERFLOW, "EXP-OVERFLOW");
			%END
		ELSE
			int16 := ts;
			int16 := ((int16+3FEh) SHL 4) BAND 7FF0h;
			expx:words[ExpWord] :=
				(expx:words[ExpWord] BAND CAST(INTEGER16, 800Fh)) BOR int16;
			RETURN expx;
		END;
	%END
END exp;

%IF IA32 %THEN

PROCEDURE fln(x : LONGREAL) : LONGREAL; ASSEMBLER;
ASM
        FLDLN2                          (* get log base e of 2 *)
        FLD     x
        FYL2X                           (* get the log of it *)
END fln;

%ELSE

CONST
    (* 16.65 digits *)
    lnp : ARRAY [0..2] OF LONGREAL =
        {
         -0.90174691662040536328986E+2,
         +0.934639006428585382474E+2,
         -0.18327870372215593212E+2
        };

    lnq : ARRAY [0..3] OF LONGREAL =
        {
         -0.45087345831020305748486E+2,
         +0.61761065598471302843E+2,
         -0.20733487895513939345E+2,
         +1.0E+0
        };

%END

%IF InlineFpp %AND IA32 %THEN
PROCEDURE ln(x : LONGREAL) : LONGREAL [FppPrim, Invariant];
%ELSE
PROCEDURE ln(x : LONGREAL) : LONGREAL [Invariant];
%END
%IF %NOT IA32 %THEN
VAR
    pow         : INTEGER16;
    z, zsq, lnz : LONGREAL;
%END
BEGIN
    %IF IA32 %THEN
        RETURN fln(x);
	%ELSE
	    IF x > 0.0 THEN

			lnz := x;

			(* get the power of 2 and reduce TO a number between 1/2 and 1 *)

			pow := ((lnz:words[ExpWord] SHL 1) SHR 5) - 3FEh;
			lnz:words[ExpWord] :=
				(lnz:words[ExpWord] BAND CAST(INTEGER16, 800Fh)) BOR 3FE0h;

			(* now reduce TO a number between 1/SQRT(2) TO SQRT(2) *)

			lnz := lnz * sqrt2;

			(* calculate ln(z) where z = (x-1) / (x+1) via polynomial *)

			z := (lnz - 1.0) / (lnz + 1.0);
			zsq := z * z;
			lnz  := (z * poly(lnp, zsq) / poly(lnq, zsq));

			(* ADD IN the log OF the power - 1/2 FOR the SQRT(2) factor *)

			RETURN (ln2 * (VAL(LONGREAL, pow) - 0.5)) + lnz;
		ELSE
			%IF AMD64 %THEN
				RETURN NaN;
			%ELSE
				RaiseRTL(mathSrc, NONPOSITIVE_LN_ARG, "NONPOSITIVE-LN-ARG");
			%END
		END;
	%END
END ln;

%IF IA32 %THEN

PROCEDURE fsin(x : LONGREAL) : LONGREAL; ASSEMBLER;
ASM
        FLD     x
        FSIN
END fsin;

%ELSE

CONST
    (* 17.48 digits *)
    sinpoly : ARRAY [0..6] OF LONGREAL =
        {
         +0.78539816339744830714E+0,
         -0.80745512188280530192E-1,
         +0.2490394570188736117E-2,
         -0.36576204158455695E-4,
         +0.313361621661904E-6,
         -0.1757149292755E-8,
         +0.6877100349E-11
        };

CONST
    (* 16.25 digits *)
    cospoly : ARRAY [0..6] OF LONGREAL =
        {
         +0.99999999999999994429E+0,
         -0.30842513753403722987E+0,
         +0.158543442437345682E-1,
         -0.32599188645404001E-3,
         +0.359085912336036E-5,
         -0.2460945716614E-7,
         +0.11363812697E-9
        };

%END

%IF InlineFpp %AND IA32 %THEN
PROCEDURE sin(x : LONGREAL) : LONGREAL [FppPrim, Invariant];
%ELSE
PROCEDURE sin(x : LONGREAL) : LONGREAL [Invariant];
%END
%IF %NOT IA32 %THEN
TYPE
    octant = [0..7];
VAR
    n   : LONGINT;
    oct : octant;
    sx  : LONGREAL;
    neg : BOOLEAN;
CONST
    which : ARRAY octant OF (dosin, docos) =
            {dosin, docos, docos, dosin, dosin, docos, docos, dosin};
%END
BEGIN
    %IF IA32 %THEN
        RETURN fsin(x);
    %ELSE

		sx := x;

		neg := FALSE;
		IF sx < 0.0 THEN
			sx := -sx;
			neg := TRUE;
		END;
		sx := sx / piover4;
		n := VAL(LONGINT, sx);
		sx := sx - VAL(LONGREAL, n);
		oct := ORD(n) REM 8;

		IF ODD(oct) THEN
			sx := 1.0 - sx;
		END;

		IF which[oct] = dosin THEN
			sx := sx * poly(sinpoly, sx*sx);
		ELSE
			sx := poly(cospoly, sx*sx);
		END;

		IF (oct >= 4) <> neg THEN
			RETURN -sx;
		ELSE
			RETURN sx;
		END;
	%END
END sin;

%IF IA32 %THEN
PROCEDURE fcos(x : LONGREAL) : LONGREAL; ASSEMBLER;
ASM
        FLD     x
        FCOS
END fcos;
%END

%IF InlineFpp %AND IA32 %THEN
PROCEDURE cos(x : LONGREAL) : LONGREAL [FppPrim, Invariant];
%ELSE
PROCEDURE cos(x : LONGREAL) : LONGREAL [Invariant];
%END
%IF %NOT IA32 %THEN
TYPE
    octant      = [0..7];
VAR
    n           : LONGINT;
    oct         : octant;
    cx          : LONGREAL;
CONST
    which : ARRAY octant OF (dosin, docos) =
        {docos, dosin, dosin, docos, docos, dosin, dosin, docos};
%END
BEGIN
    %IF IA32 %THEN
        RETURN fcos(x);
    %ELSE

		cx := ABS(x / piover4);
		n := VAL(LONGINT, cx);
		cx := cx - VAL(LONGREAL, n);
		oct := ORD(n) REM 8;

		IF ODD(oct) THEN
			cx := 1.0 - cx;
		END;

		IF which[oct] = dosin THEN
			cx := cx * poly(sinpoly, cx*cx);
		ELSE
			cx := poly(cospoly, cx*cx);
		END;

		IF (oct >= 2) AND (oct <= 5) THEN
			RETURN -cx;
		ELSE
			RETURN cx;
		END;
	%END
END cos;

%IF IA32 %THEN

PROCEDURE ftan(x : LONGREAL) : LONGREAL; ASSEMBLER;
ASM
        FLD     x
        FSINCOS
        FDIVP
END ftan;

%ELSE

CONST
    (* 19.74 digits *)
    tanp : ARRAY [0..4] OF LONGREAL =
        {
         -0.1306820264754825668269611177E+5,
         +0.1055970901714953193602353981E+4,
         -0.1550685653483266376941705728E+2,
         +0.3422554387241003435328470489E-1,
         +0.3386638642677172096076369E-4
        };

    tanq : ARRAY [0..3] OF LONGREAL =
        {
         -0.1663895238947119001851464661E+5,
         +0.4765751362916483698926655581E+4,
         -0.1555033164031709966900124574E+3,
         +1.0E+0
        };

%END

%IF InlineFpp %AND IA32 %THEN
PROCEDURE tan(x : LONGREAL) : LONGREAL [FppPrim, Invariant];
%ELSE
PROCEDURE tan(x : LONGREAL) : LONGREAL [Invariant];
%END
%IF %NOT IA32 %THEN
TYPE
    quadrant = [0..3];
VAR
    n   : LONGINT;
    q   : quadrant;
    xsq : LONGREAL;
    tx  : LONGREAL;
%END
BEGIN
    %IF IA32 %THEN
        RETURN ftan(x);
    %ELSE

		tx := x;
		tx := tx / piover4;
		n := VAL(LONGINT, tx);
		tx := tx - VAL(LONGREAL, n);

		q := ORD(n) REM 4;

		IF ODD(q) THEN
			tx := 1.0 - tx;
		END;

		xsq := tx * tx;
		tx := tx * poly(tanp, xsq) / poly(tanq, xsq);

		IF (q = 1) OR (q = 2) THEN
			tx := 1.0 / tx;
		END;

		IF q >= 2 THEN
			RETURN -tx;
		ELSE
			RETURN tx;
		END;
	%END
END tan;

PROCEDURE arcsin(x : LONGREAL) : LONGREAL [Invariant];
BEGIN
    IF x = -1.0 THEN
        RETURN -piover2;
    ELSIF x = 1.0 THEN
        RETURN piover2;
    ELSIF (x < -1.0) OR (x > 1.0) THEN
		%IF IA32 %OR AMD64 %THEN
        	RETURN NaN;
		%ELSE
	        RaiseRTL(mathSrc, ARCSIN_ARG_MAGNITUDE, "ARCSIN-ARG-MAGNITUDE");
		%END
    END;
    RETURN arctan(x / sqrt(1.0 - x*x));
END arcsin;

PROCEDURE arccos(x : LONGREAL) : LONGREAL [Invariant];
BEGIN
    IF x = -1.0 THEN
        RETURN 2.0 * piover2;
    ELSIF x = 1.0 THEN
        RETURN 0.0;
    ELSIF (x < -1.0) OR (x > 1.0) THEN
		%IF IA32 %OR AMD64 %THEN
        	RETURN NaN;
		%ELSE
	        RaiseRTL(mathSrc, ARCCOS_ARG_MAGNITUDE, "ARCCOS-ARG-MAGNITUDE");
		%END
    END;
    RETURN piover2 - arctan(x / sqrt(1.0 - x*x));
END arccos;

%IF IA32 %THEN

PROCEDURE farctan(x : LONGREAL) : LONGREAL; ASSEMBLER;
ASM
        FLD     x
        FCOM    zero
        FSTSW       AX
        MOV     EDX, EAX

        FLD1                            (* get a one *)

        FXCH                            (* get the arg *)

        FABS                            (* get abs value of arg *)

        FCOM                            (* compare to 1 *)
        FSTSW   AX
        SAHF

        JB      @LESS                   (* handle by less, greater or equal *)
        JA      @GREATER

    (* arctan(1) = pi/4 *)
        FCOMPP                          (* remove two stack items *)
        FLD     piover4
        JMP     @CHECKNEG

@LESS:
        FXCH                            (* less, we must exchange *)
        FPATAN
        JMP     @POP_IT

@GREATER:
        FPATAN
        FLD     piover2
        FSUBR
@POP_IT:

@CHECKNEG:
        TEST    DH, 1                   (* or maybe the arg was neg? *)
        JZ      @AT_NO_NEG_RES
        FCHS                            (* negate the result *)
@AT_NO_NEG_RES:
END farctan;

%ELSE

CONST
    artannover16 : ARRAY [0..16] OF LONGREAL =
        {
        +0.0,
        +0.6241880999595734847397911E-1,
        +0.1243549945467614350313548E+0,
        +0.1853479499956947648860259E+0,
        +0.2449786631268641541720824E+0,
        +0.3028848683749714055605560E+0,
        +0.3587706702705722203959200E+0,
        +0.4124104415973873068997912E+0,
        +0.4636476090008061162142562E+0,
        +0.5123894603107377066666010E+0,
        +0.5585993153435624359715082E+0,
        +0.6022873461349641816821226E+0,
        +0.6435011087932843868028092E+0,
        +0.6823165548747480782564299E+0,
        +0.7188299996216245054170141E+0,
        +0.7531512809621943895247393E+0,
        +0.7853981633974483096156608E+0
        };

CONST
    (* 16.52 digits *)
    atp : ARRAY [0..5] OF LONGREAL =
        {
         +0.999999999999999969557E+0,
         -0.33333333333310718E+0,
         +0.19999999972769276E+0,
         -0.142857022886077E+0,
         +0.1110871947826E+0,
         -0.8870580341E-1
        };

%END

%IF InlineFpp %AND IA32 %THEN
PROCEDURE arctan(x : LONGREAL) : LONGREAL [FppPrim, Invariant];
%ELSE
PROCEDURE arctan(x : LONGREAL) : LONGREAL [Invariant];
%END
%IF %NOT IA32 %THEN
VAR
    neg, inv    : BOOLEAN;
    i           : ADRINT;
    r, atx      : LONGREAL;
%END
BEGIN
    %IF IA32 %THEN
        RETURN farctan(x);
    %ELSE

		neg := FALSE;
		inv := FALSE;
		atx := x;

		(* handle negatives by atan(-x) = -atan(x) *)

		IF atx < 0.0 THEN
			atx := -atx;
			neg := TRUE;
		END;

		(* and numbers > 1 by atan(x) = PI/2 - atan(1/x) *)

		IF atx > 1.0 THEN
			atx := 1.0 / atx;
			inv := TRUE;
		END;

		(* x is now between 0 and 1 *)

		(* determine which 1/16 of this range it is in *)

		i := VAL(INTEGER, atx * 16.0);

		(* r is the nearest value that we have the arctan for *)

		r := VAL(LONGREAL, i) / 16.0;

		(* and use the formula atan(x) = atan(r) + atan((x-r)/(1+xr)) *)

		atx := (atx - r) / (1.0 + (atx * r));
		atx := artannover16[i] + (atx * poly(atp, atx*atx));

		(* handle the symmetry *)

		IF inv THEN
			atx := piover2 - atx;
		END;

		IF neg THEN
			RETURN -atx;
		ELSE
			RETURN atx;
		END;
	%END
END arctan;

PROCEDURE power(base, exponent : LONGREAL) : LONGREAL [Invariant];
BEGIN
    IF base > 0.0 THEN
        RETURN (exp(exponent * ln(base)));
    END;
	%IF IA32 %OR AMD64 %THEN
    	RETURN NaN;
	%ELSE
	    RaiseRTL(mathSrc, NONPOSITIVE_POWER_ARG, "NONPOSITIVE-POWER-ARG");
    	RETURN 0.0;
	%END
END power;

%IF IA32 %OR AMD64 %THEN
<*/NOWARN:I*>
%ELSE
PROCEDURE round(x : LONGREAL) : INTEGER [Invariant];
BEGIN
    IF x >= 0.0 THEN
        RETURN VAL(INTEGER, (x + 0.5));
    ELSE
        RETURN VAL(INTEGER, (x - 0.5));
    END;
END round;
%END

PROCEDURE IsRMathException() : BOOLEAN;
BEGIN
	%IF IA32 %OR AMD64 %THEN
    	RETURN FALSE;
	%ELSE
	    RETURN IsCurrentSource(mathSrc);
	%END
END IsRMathException;

%IF IA32 %OR AMD64 %THEN
%ELSE
BEGIN
    IF NOT IsThread THEN
        AllocateSource(mathSrc);
    END;
%END
END LongMath.
