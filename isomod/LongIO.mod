IMPLEMENTATION MODULE LongIO;

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

  (* Input and output of real numbers in decimal text form over specified
channels. The read result is of the type IOConsts.ReadResults. *)

FROM SYSTEM IMPORT
    ADRCARD;

IMPORT IOChan;

FROM LongMath IMPORT
    ln;

FROM LongStr IMPORT
    ConvResults, StrToReal, RealToFloat, RealToEng, RealToFixed;

FROM TextIO IMPORT
    WriteString, WriteChar;

FROM IOConsts IMPORT
    ReadResults;

FROM IOChan IMPORT
    SetReadResult;

FROM CharClass IMPORT
    IsWhiteSpace, IsNumeric;

TYPE
  String = ARRAY [0..63] OF CHAR;

  (* The text form of a signed fixed-point real number is
       ["+" | "-"], decimal digit, {decimal digit},
       [".", {decimal digit}]

     The text form of a signed floating-point real number is
       signed fixed-point real number,
       "E", ["+" | "-"], decimal digit, {decimal digit}
  *)

(*===============================*)

(* local procedures *)

PROCEDURE SetResult(cid : IOChan.ChanId; res : ConvResults);
BEGIN
    CASE res OF
    strAllRight:
        SetReadResult (cid, allRight);
    |
    strOutOfRange:
        SetReadResult (cid, outOfRange);
    |
    strWrongFormat:
        SetReadResult (cid, wrongFormat);
    |
    strEmpty:
        SetReadResult (cid, endOfLine);
    END;
END SetResult;

(*===============================*)

PROCEDURE ReadRealNumber(cid : IOChan.ChanId;
                         VAR str : ARRAY OF CHAR) : ReadResults;
(* new procedure by Stony Brook *)
VAR
    i           : ADRCARD;
    highStr     : ADRCARD;
    ch          : CHAR;
    gotDig      : BOOLEAN;
    gotChar     : BOOLEAN;
    res         : ReadResults;

    PROCEDURE allowPlusMinus;
    BEGIN
        IF res = allRight THEN
            IF (ch = '-') OR (ch = '+') THEN
                gotChar := TRUE;
                str[i] := ch;
                INC(i);
                IOChan.SkipLook(cid, ch, res);
            END;
        END;
    END allowPlusMinus;

    PROCEDURE readDigits;
    VAR
        highStr : ADRCARD;
    BEGIN
        highStr := HIGH(str);
        WHILE (res = allRight) AND (i <= highStr) AND IsNumeric(ch) DO
            gotDig := TRUE;
            str[i] := ch;
            INC(i);
            IOChan.SkipLook(cid, ch, res);
        END;
    END readDigits;

BEGIN
    str := "";

    (* skip leading spaces *)

    IOChan.Look(cid, ch, res);
    WHILE (res = allRight) AND IsWhiteSpace(ch) DO
        IOChan.SkipLook(cid, ch, res);
    END;

    i := 0;
    gotDig := FALSE;
    gotChar := FALSE;

    allowPlusMinus;
    readDigits;
    IF NOT gotDig THEN
        IF gotChar OR ((res <> endOfLine) AND (res <> endOfInput)) THEN
            res := wrongFormat;
        END;
    END;

    IF (res = allRight) AND (ch = '.') THEN
        str[i] := ch;
        INC(i);
        IOChan.SkipLook(cid, ch, res);

        readDigits;
    END;

    IF (res = allRight) AND ((ch = 'E') OR (ch = 'e')) THEN
        str[i] := ch;
        INC(i);
        IOChan.SkipLook(cid, ch, res);

        allowPlusMinus;
        gotDig := FALSE;
        readDigits;
        IF NOT gotDig THEN
            res := wrongFormat;
        END;
    END;

    highStr := HIGH(str);
    IF res <> wrongFormat THEN
        IF gotChar AND ((i-1) > highStr) THEN
            (* eat the remaining numbers *)

            WHILE (res = allRight) AND IsNumeric(ch) DO
                IOChan.SkipLook(cid, ch, res);
            END;

            res := outOfRange;
        ELSIF (res = endOfLine) OR (res = endOfInput) THEN
            res := allRight;
        END;
    END;

    IF i <= highStr THEN
        str[i] := '';
    END;

    SetReadResult(cid, res);
    RETURN res;
END ReadRealNumber;

PROCEDURE ReadReal (cid: IOChan.ChanId; VAR real: LONGREAL);
(* modified by Stony Brook *)

  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of a signed fixed or floating point number.
     The value of this number is assigned to real. The read result is set
     to the value allRight, outOfRange, wrongFormat, endOfLine, or
     endOfInput. *)
VAR
    temp        : String;
    res         : ConvResults;
BEGIN
    IF ReadRealNumber(cid, temp) = allRight THEN
        StrToReal(temp, real, res);
        SetResult(cid, res);
    END;
END ReadReal;

PROCEDURE WriteFloat(cid : IOChan.ChanId;
                     real : LONGREAL;
                     sigFigs : CARDINAL;
                     width : CARDINAL);
  (* Writes the value of real to cid in floating-point text form,
     with sigFigs significant figures, in a field of the given minimum width.
  *)
VAR
    temp : String;
    i    : CARDINAL;
BEGIN
    IF width = 0 THEN
        WriteChar(cid, ' ');
    END;

    RealToFloat(real, sigFigs, temp); (* convert LONGREAL number to a string *)
    IF LENGTH(temp) < width THEN
        (* write spaces *)
        FOR i := 1 TO width - LENGTH(temp) DO
            WriteString(cid, " ");
        END;
    END;
    WriteString(cid, temp); (* write the string *)
END WriteFloat;

PROCEDURE WriteEng(cid : IOChan.ChanId;
                   real : LONGREAL;
                   sigFigs : CARDINAL;
                   width : CARDINAL);
  (* As for WriteFloat, except that the number is scaled with one to three
     digits in the whole number part, and with an exponent that is a
     multiple of three. *)
VAR
    temp : String;
    i    : CARDINAL;
BEGIN
    IF width = 0 THEN
        WriteChar(cid, ' ');
    END;

    RealToEng(real, sigFigs, temp); (* convert LONGREAL to a string *)
    IF LENGTH(temp) < width THEN
        (* write spaces *)
        FOR i := 1 TO width - LENGTH(temp) DO
            WriteString(cid, " ");
        END;
    END;
    WriteString(cid, temp); (* write the string *)
END WriteEng;

PROCEDURE WriteFixed(cid : IOChan.ChanId;
                     real : LONGREAL;
                     place : INTEGER;
                     width : CARDINAL);
  (* Writes the value of real to cid in fixed-point text form, rounded to
     the given place relative to the decimal point, in a field of the given
     minimum width. *)
VAR
    temp : String;
    i    : CARDINAL;
BEGIN
    IF width = 0 THEN
        WriteChar(cid, ' ');
    END;

    RealToFixed(real, place, temp); (* convert LONGREAL to fixed *)
    IF LENGTH(temp) < width THEN
        (* write spaces *)
        FOR i := 1 TO width - LENGTH(temp )DO
            WriteString(cid, " ");
        END;
    END;
    WriteString(cid, temp); (* write the string *)
END WriteFixed;

PROCEDURE WriteReal(cid : IOChan.ChanId;
                    real : LONGREAL;
                    width : CARDINAL);
  (* Writes the value of real to cid, as WriteFixed if the sign and
     magnitude can be shown in the given width, or otherwise as WriteFloat.
     The number of places or significant digits depends on the given width.
  *)
VAR
    temp        : String;
    i, num      : CARDINAL;
BEGIN
    IF width = 0 THEN
        WriteFloat(cid, real, 0, 0);
        RETURN;
    END;

    IF ABS(real) >= 1.0 THEN (* get the number of digits in the whole part *)
        num := TRUNC((ln(ABS(real)) / ln(10.0)));
    ELSE
        num := width;
    END;

    IF real < 0.0 THEN (* if the number is negative *)
        INC(num);
    END;

    IF width >= num + 2 THEN (* if the number fits *)

        (* convert to fixed format *)

        RealToFixed(real, width - 2 - num, temp);
    ELSE
        IF (
            (real >= 0.0) AND (width > 6)
           )
           OR
           (
            (real < 0.0) AND (width > 7)
           )
        THEN
            (* convert to floating point *)

            IF real >= 0.0 THEN
                RealToFloat(real, width - 6, temp);
            ELSE
                RealToFloat(real, width - 7, temp);
            END;
        ELSE
            RealToFloat(real, width, temp);
        END;
    END;

    IF width = 0 THEN
        WriteChar(cid, ' ');
    END;

    IF LENGTH(temp) < width THEN (* write spaces *)
        FOR i := 1 TO width - LENGTH(temp) DO
            WriteString(cid, " ");
        END;
    END;
    WriteString(cid, temp); (* write the string *)
END WriteReal;

END LongIO.
