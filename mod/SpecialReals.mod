IMPLEMENTATION MODULE SpecialReals;

(* Special real values													   	 *)

(* A. Mutylin                                                       March 2017 *)

FROM SYSTEM IMPORT CAST;

CONST
    ExponentMask = 7FF0000000000000H;
    FractionMask = 000FFFFFFFFFFFFFH;
	QuietMask	 = 0008000000000000H;

PROCEDURE IsFinite (R : LONGREAL) : BOOLEAN;
BEGIN
    RETURN CAST(CARDINAL64,R) BAND ExponentMask # ExponentMask;
END IsFinite;

PROCEDURE IsNaN (R : LONGREAL) : BOOLEAN;
VAR
    A : CARDINAL64;
BEGIN
    A := CAST(CARDINAL64,R);
    RETURN (A BAND ExponentMask = ExponentMask) & (A BAND FractionMask # 0);
END IsNaN;

PROCEDURE IsQNaN (R : LONGREAL) : BOOLEAN;
VAR
    A : CARDINAL64;
BEGIN
    A := CAST(CARDINAL64,R);
    RETURN (A BAND ExponentMask = ExponentMask) & (A BAND QuietMask # 0);
END IsQNaN;

PROCEDURE IsSNaN (R : LONGREAL) : BOOLEAN;
VAR
    A : CARDINAL64;
BEGIN
    A := CAST(CARDINAL64,R);
    RETURN (A BAND ExponentMask = ExponentMask) & (A BAND FractionMask # 0) & (A BAND QuietMask = 0);
END IsSNaN;

PROCEDURE IsInfinity (R : LONGREAL) : BOOLEAN;
VAR
    A : CARDINAL64;
BEGIN
    A := CAST(CARDINAL64,R);
    RETURN (A BAND ExponentMask = ExponentMask) & (A BAND FractionMask = 0);
END IsInfinity;

PROCEDURE IsPositiveInfinity (R : LONGREAL) : BOOLEAN;
BEGIN
    RETURN IsInfinity(R) & (R>0.);
END IsPositiveInfinity;

PROCEDURE IsNegativeInfinity (R : LONGREAL) : BOOLEAN;
BEGIN
    RETURN IsInfinity(R) & (R<0.);
END IsNegativeInfinity;

PROCEDURE IsMinusZero (R : LONGREAL) : BOOLEAN;
BEGIN
    RETURN CAST(INTEGER64,R) = 8000000000000000H;
END IsMinusZero;

END SpecialReals.
