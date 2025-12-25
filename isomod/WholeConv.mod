IMPLEMENTATION MODULE WholeConv;

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
    ADRCARD;

FROM EXCEPTIONS IMPORT
    ExceptionSource, AllocateSource, IsCurrentSource, RaiseRTL;

FROM CharClass IMPORT
    IsWhiteSpace, IsNumeric;

IMPORT ConvTypes;

VAR
    WholeConvSrc        : ExceptionSource;

PROCEDURE W(inputCh : CHAR;
            VAR chClass : ConvTypes.ScanClass;
            VAR nextState : ConvTypes.ScanState);
BEGIN
    IF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := W;
    ELSE
        chClass := ConvTypes.terminator;
    END;
END W;

PROCEDURE S(inputCh : CHAR;
            VAR chClass : ConvTypes.ScanClass;
            VAR nextState : ConvTypes.ScanState);
BEGIN
    IF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := W;
    ELSE
        chClass := ConvTypes.invalid;
        nextState := S;
    END;
END S;

PROCEDURE ScanInt(inputCh : CHAR;
                  VAR chClass : ConvTypes.ScanClass;
                  VAR nextState : ConvTypes.ScanState);
(* Represents the start state of a finite state scanner for signed whole
   numbers - assigns class of inputCh to chClass and a procedure
   representing the next state to nextState.
*)
BEGIN
    IF IsWhiteSpace(inputCh) THEN
        chClass := ConvTypes.padding;
        nextState := ScanInt;
    ELSIF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := W;
    ELSIF (inputCh = '-') OR (inputCh = '+') THEN
        chClass := ConvTypes.valid;
        nextState := S;
    ELSE
        chClass := ConvTypes.invalid;
        nextState := ScanInt;
    END;
END ScanInt;

PROCEDURE doIntFmt(str : ARRAY OF CHAR; VAR int : INTEGER) : ConvResults;
CONST
    maxDiv10    = MAX(INTEGER) / 10;
    lastDig     = CHR( (MAX(INTEGER) REM 10) + INT('0') );
VAR
    gotOne      : BOOLEAN;
    neg         : BOOLEAN;
    i           : ADRCARD;
    l           : ADRCARD;
    state       : ConvTypes.ScanState;
    class       : ConvTypes.ScanClass;
BEGIN
    l := LENGTH(str);

    IF l <> 0 THEN
        gotOne := FALSE;
        neg := FALSE;
        int := 0;
        state := ScanInt;
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
                IF str[i] = '-' THEN
                    neg := TRUE;
                ELSIF str[i] <> '+' THEN
                    gotOne := TRUE;
                    IF (int < maxDiv10) OR
                       (
                        (int = maxDiv10) AND (str[i] <= lastDig)
                       )
                    THEN
                        int := int * 10;
                        int := int + (INT(str[i]) - INT('0'));
                    ELSE
                        RETURN strOutOfRange;
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
            RETURN strAllRight;
        END;
    END;
    RETURN strEmpty;
END doIntFmt;

PROCEDURE FormatInt(str : ARRAY OF CHAR) : ConvResults;
(* Returns the format of the string value for conversion to INTEGER *)
VAR
    int : INTEGER;
BEGIN
    RETURN doIntFmt(str, int);
END FormatInt;

PROCEDURE ValueInt(str : ARRAY OF CHAR) : INTEGER;
(* Returns the value corresponding to the signed whole number string
   value str if str is well-formed; otherwise raises the WholeConv exception.
*)
VAR
    int : INTEGER;
BEGIN
    IF doIntFmt(str, int) = strAllRight THEN
        RETURN int;
    END;
    RaiseRTL(WholeConvSrc, 0, '');
END ValueInt;

PROCEDURE LengthInt(int : INTEGER) : CARDINAL;
(* Returns the number of characters in the string representation of int *)
VAR
    digits      : CARDINAL;
BEGIN
    digits := 0;
    IF int <= 0 THEN
        INC(digits);
        int := ABS(int);
    END;

    WHILE int <> 0 DO
        int := int / 10;
        INC(digits);
    END;

    RETURN digits;
END LengthInt;

PROCEDURE ScanCard(inputCh : CHAR;
                   VAR chClass : ConvTypes.ScanClass;
                   VAR nextState : ConvTypes.ScanState);
(* Represents the start state of a finite state scanner for signed whole
   numbers - assigns class of inputCh to chClass and a procedure
   representing the next state to nextState.
*)
BEGIN
    IF IsWhiteSpace(inputCh) THEN
        chClass := ConvTypes.padding;
        nextState := ScanCard;
    ELSIF IsNumeric(inputCh) THEN
        chClass := ConvTypes.valid;
        nextState := W;
    ELSE
        chClass := ConvTypes.invalid;
        nextState := ScanCard;
    END;
END ScanCard;

PROCEDURE doCardFmt(str : ARRAY OF CHAR; VAR card : CARDINAL) : ConvResults;
CONST
    maxDiv10    = MAX(CARDINAL) / 10;
    lastDig     = CHR( (MAX(CARDINAL) REM 10) + ORD('0') );
VAR
    gotOne      : BOOLEAN;
    i           : ADRCARD;
    l           : ADRCARD;
    state       : ConvTypes.ScanState;
    class       : ConvTypes.ScanClass;
BEGIN
    l := LENGTH(str);

    IF l <> 0 THEN
        gotOne := FALSE;
        card := 0;
        state := ScanCard;
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
                gotOne := TRUE;
                IF (card < maxDiv10) OR
                   (
                    (card = maxDiv10) AND (str[i] <= lastDig)
                   )
                THEN
                    card := card * 10;
                    card := card + (ORD(str[i]) - ORD('0'));
                ELSE
                    RETURN strOutOfRange;
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
            RETURN strAllRight;
        END;
    END;
    RETURN strEmpty;
END doCardFmt;

PROCEDURE FormatCard(str : ARRAY OF CHAR) : ConvResults;
(* Returns the format of the string value for conversion to CARDINAL *)
VAR
    card        : CARDINAL;
BEGIN
    RETURN doCardFmt(str, card);
END FormatCard;

PROCEDURE ValueCard(str : ARRAY OF CHAR) : CARDINAL;
(* Returns the value corresponding to the signed whole number string
   value str if str is well-formed; otherwise raises the WholeConv exception.
*)
VAR
    card        : CARDINAL;
BEGIN
    IF doCardFmt(str, card) = strAllRight THEN
        RETURN card;
    END;
    RaiseRTL(WholeConvSrc, 0, '');
END ValueCard;

PROCEDURE LengthCard(card : CARDINAL) : CARDINAL;
(* Returns the number of characters in the string representation of card *)
VAR
    digits      : CARDINAL;
BEGIN
    digits := 0;
    IF card = 0 THEN
        INC(digits);
    END;

    WHILE card <> 0 DO
        card := card / 10;
        INC(digits);
    END;

    RETURN digits;
END LengthCard;

PROCEDURE IsWholeConvException() : BOOLEAN;
(* Returns TRUE if the current coroutine is in the exceptional execution
   state because of the raising of an exception in a routine from this
   module; otherwise returns FALSE
*)
BEGIN
    RETURN IsCurrentSource(WholeConvSrc);
END IsWholeConvException;

BEGIN
    AllocateSource(WholeConvSrc);
END WholeConv.
