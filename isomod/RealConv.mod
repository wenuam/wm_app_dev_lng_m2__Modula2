IMPLEMENTATION MODULE RealConv;

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

FROM SYSTEM IMPORT
    ADRCARD, IsThread;

FROM EXCEPTIONS IMPORT
    ExceptionSource, AllocateSource, IsCurrentSource, RaiseRTL;

FROM CharClass IMPORT
    IsWhiteSpace, IsNumeric;

IMPORT ConvTypes;

VAR
    RealConvSrc : ExceptionSource;

PROCEDURE WE(inputCh : CHAR;
             VAR chClass : ConvTypes.ScanClass;
             VAR nextState : ConvTypes.ScanState);
BEGIN
    IF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := WE;
    ELSE
        chClass := ConvTypes.terminator;
    END;
END WE;

PROCEDURE SE(inputCh : CHAR;
             VAR chClass : ConvTypes.ScanClass;
             VAR nextState : ConvTypes.ScanState);
BEGIN
    IF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := WE;
    ELSE
        chClass := ConvTypes.invalid;
        nextState := SE;
    END;
END SE;

PROCEDURE E(inputCh : CHAR;
            VAR chClass : ConvTypes.ScanClass;
            VAR nextState : ConvTypes.ScanState);
BEGIN
    IF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := WE;
    ELSIF (inputCh = '-') OR (inputCh = '+') THEN
        chClass := ConvTypes.valid;
        nextState := SE;
    ELSE
        chClass := ConvTypes.invalid;
        nextState := E;
    END;
END E;

PROCEDURE F(inputCh : CHAR;
            VAR chClass : ConvTypes.ScanClass;
            VAR nextState : ConvTypes.ScanState);
BEGIN
    IF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := F;
    ELSIF (inputCh = 'E') OR (inputCh = 'e') THEN
        chClass := ConvTypes.valid;
        nextState := E;
    ELSE
        chClass := ConvTypes.terminator;
    END;
END F;

PROCEDURE P(inputCh : CHAR;
            VAR chClass : ConvTypes.ScanClass;
            VAR nextState : ConvTypes.ScanState);
BEGIN
    IF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := P;
    ELSIF inputCh = '.' THEN
        chClass := ConvTypes.valid;
        nextState := F;
    ELSIF (inputCh = 'E') OR (inputCh = 'e') THEN
        chClass := ConvTypes.valid;
        nextState := E;
    ELSE
        chClass := ConvTypes.terminator;
    END;
END P;

PROCEDURE RS(inputCh : CHAR;
             VAR chClass : ConvTypes.ScanClass;
             VAR nextState : ConvTypes.ScanState);
BEGIN
    IF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := P;
    ELSE
        chClass := ConvTypes.invalid;
        nextState := RS;
    END;
END RS;

PROCEDURE ScanReal(inputCh : CHAR;
                   VAR chClass : ConvTypes.ScanClass;
                   VAR nextState : ConvTypes.ScanState);
(* Represents the start state of a finite state scanner for real
   numbers - assigns class of inputCh to chClass and a procedure
   representing the next state to nextState.
*)
BEGIN
    IF IsWhiteSpace(inputCh) THEN
        chClass := ConvTypes.padding;
        nextState := ScanReal;
    ELSIF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := P;
    ELSIF (inputCh = '-') OR (inputCh = '+') THEN
        chClass := ConvTypes.valid;
        nextState := RS;
    ELSE
        chClass := ConvTypes.invalid;
        nextState := ScanReal;
    END;
END ScanReal;

CONST
    MaxPowerArray       = 5;

TYPE
    PowerArray  = ARRAY [0..MaxPowerArray] OF REAL;

CONST
    Powers = PowerArray{
                        1.0E1,
                        1.0E2,
                        1.0E4,
                        1.0E8,
                        1.0E16,
                        1.0E32
                       };

PROCEDURE doFormat(str : ARRAY OF CHAR; VAR real : REAL) : ConvResults;
VAR
    gotOne      : BOOLEAN;
    mode        : (whole, fraction, exponent);
    i           : ADRCARD;
    j           : ADRCARD;
    l           : ADRCARD;
    state       : ConvTypes.ScanState;
    class       : ConvTypes.ScanClass;
    fracPlace   : REAL;
    frac        : REAL;
    msign       : REAL;
    exp         : INTEGER;
    pow2        : INTEGER;
    eneg        : BOOLEAN;
BEGIN
    l := LENGTH(str);

    IF l <> 0 THEN
        gotOne := FALSE;
        mode := whole;
        fracPlace := 10.0;
        real := 0.0;
        msign := 1.0;
        eneg := FALSE;
        exp := 0;
        state := ScanReal;
        i := 0;
        LOOP
            state(str[i], class, state);
            CASE class OF
            ConvTypes.invalid:
                RETURN strWrongFormat;
            |
            ConvTypes.terminator:
                EXIT;
            |
            ConvTypes.valid:
                IF str[i] = '+' THEN
                    IF mode = whole THEN
                        msign := +1.0;
                    ELSE
                        eneg := FALSE;
                    END;
                ELSIF str[i] = '-' THEN
                    IF mode = whole THEN
                        msign := -1.0;
                    ELSE
                        eneg := TRUE;
                    END;
                ELSIF str[i] = '.' THEN
                    mode := fraction;
                ELSIF (str[i] = 'E') OR (str[i] = 'e') THEN
                    mode := exponent;
                ELSE
                    CASE mode OF
                    whole:
                        gotOne := TRUE;
                        IF real < 3.38E37 THEN
                            real := (real * 10.0) + FLOAT(ORD(str[i]) - ORD('0'));
                        ELSE
                            RETURN strOutOfRange;
                        END;
                    |
                    fraction:

                        frac := FLOAT(ORD(str[i]) - ORD('0')) / fracPlace;
                        fracPlace := fracPlace * 10.0;
                        real := real + frac;
                    |
                    exponent:
                        IF (exp < 3) OR
                           (
                            (exp = 3) AND ((ORD(str[i]) - ORD('0')) <= 8)
                           )
                        THEN
                            exp := (exp * 10) + (INT(str[i]) - INT('0'));
                        ELSE
                            RETURN strOutOfRange;
                        END;
                    END;
                END;
            |
            ConvTypes.padding:
            END;

            INC(i);
            IF i = l THEN
                EXIT;
            END;
        END;

        IF gotOne THEN
            real := real * msign;

            IF exp <> 0 THEN

                IF (exp < 39) OR
                   (
                    (exp = 39) AND (real <= 0.338)
                   )
                THEN
                    pow2 := 32;
                    FOR j := MaxPowerArray TO 0 BY -1 DO
                        IF exp >= pow2 THEN
                            IF NOT eneg THEN
                                real := real * Powers[j];
                            ELSE
                                real := real / Powers[j];
                            END;
                            exp := exp - pow2;
                        END;
                        pow2 := pow2 / 2;
                    END;
                ELSE
                    RETURN strOutOfRange;
                END;
            END;

            RETURN strAllRight;
        END;
    END;
    RETURN strEmpty;
END doFormat;

PROCEDURE FormatReal(str : ARRAY OF CHAR) : ConvResults;
(* Returns the format of the string value for conversion to REAL *)
VAR
    real        : REAL;
BEGIN
    RETURN doFormat(str, real);
END FormatReal;

PROCEDURE ValueReal(str : ARRAY OF CHAR) : REAL;
(* Returns the value corresponding to the real number string
   value str if str is well-formed; otherwise raises the RealConv exception.
*)
VAR
    real        : REAL;
BEGIN
    IF doFormat(str, real) = strAllRight THEN
        RETURN real;
    END;
    RaiseRTL(RealConvSrc, 0, '');
END ValueReal;

PROCEDURE LengthFloatReal(real : REAL; sigFigs : CARDINAL) : CARDINAL;
(* Returns the number of characters in the floating-point
   string representation of real with sigFigs significant figures
*)
VAR
    symbols             : CARDINAL;
BEGIN
    IF sigFigs = 0 THEN
        sigFigs := 1;
    END;

    symbols := 0;

    (* the minus sign *)

    IF real < 0.0 THEN
        INC(symbols);
    END;

    (* the decimal point *)

    IF sigFigs > 1 THEN
        INC(symbols);
    END;

    (* 4 = E+00 *)

    RETURN symbols + sigFigs + 4;
END LengthFloatReal;

PROCEDURE LengthEngReal(real : REAL; sigFigs : CARDINAL) : CARDINAL;
(* Returns the number of characters in the floating-point engineering
   string representation of real with sigFigs significant figures
*)
VAR
    symbols     : CARDINAL;
    left        : CARDINAL;
    tenths      : REAL;
    exp         : INTEGER;
    i           : CARDINAL;
BEGIN
    IF sigFigs = 0 THEN
        sigFigs := 1;
    END;

    symbols := 0;

    (* if negative *)

    IF real < 0.0 THEN
        INC(symbols);
        real := ABS(real);
    END;

    (* force the number between 0 and 999 *)

    exp := 0;
    IF real < 1.0 THEN
        REPEAT
            real := real * 10.0;
            DEC(exp);
        UNTIL (real >= 1.0) AND (LFLOAT(exp / 3) = (LFLOAT(exp) / 3.0));

    ELSIF (real >= 1000.0) OR (LFLOAT(exp / 3) <> (LFLOAT(exp) / 3.0)) THEN
        REPEAT
            real := real / 10.0;
            INC(exp);
        UNTIL (real < 1000.0) AND (LFLOAT(exp / 3) = (LFLOAT(exp) / 3.0));
    END;

    (* get rid of small errors *)

    tenths := 0.5;
    FOR i := 1 TO sigFigs DO
        tenths := tenths / 10.0;
    END;
    real := real + tenths;

    IF real >= 1000.0 THEN
        real := real / 10.0;
        INC(exp);
    END;

    (* back number up to less than 10 *)

    left := 1;
    WHILE real >= 10.0 DO
        real := real / 10.0;
        INC(left);
    END;

    (* 4 = E+00 *)

    IF sigFigs > left THEN
        (* decimal point *)

        INC(symbols);

        RETURN symbols + sigFigs + 4;
    ELSE
        RETURN symbols + left + 4;
    END;
END LengthEngReal;

PROCEDURE LengthFixedReal(real : REAL; place : INTEGER) : CARDINAL;
(* Returns the number of characters in the fixed-point
   string representation of real rounded to the given place relative to
   the decimal point
*)
VAR
    i           : INTEGER;
    left        : CARDINAL;
    symbols     : CARDINAL;
    tenths      : REAL;
BEGIN
    symbols := 0;

    IF real < 0.0 THEN
        INC(symbols);
        real := ABS(real);
    END;

    (* get rid of small errors *)

    tenths := 0.5;
    FOR i := 1 TO place DO
        tenths := tenths / 10.0;
    END;
    real := real + tenths;

    (* force real to less than 10 *)

    left := 1;
    WHILE real >= 10.0 DO
        real := real / 10.0;
        INC(left);
    END;

    IF place > 0 THEN
        (* deminal point *)

        INC(symbols);

        RETURN symbols + left + ORD(place);
    ELSE
        RETURN symbols + left;
    END;
END LengthFixedReal;

PROCEDURE IsRConvException() : BOOLEAN;
(* Returns TRUE if the current coroutine is in the exceptional execution
   state because of the raising of an exception in a routine from this
   module; otherwise returns FALSE
*)
BEGIN
    RETURN IsCurrentSource(RealConvSrc);
END IsRConvException;

BEGIN
    (* in case this module is part of a WIN32 DLL, that inits per thread *)

    IF NOT IsThread THEN
        AllocateSource(RealConvSrc);
    END;
END RealConv.
