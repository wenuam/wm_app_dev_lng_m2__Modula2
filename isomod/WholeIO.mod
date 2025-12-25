IMPLEMENTATION MODULE WholeIO;

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

  (* Input and output of whole numbers in decimal text form over specified
channels. The read result is of the type IOConsts.ReadResults.
  *)

FROM SYSTEM IMPORT
    ADRCARD;

IMPORT IOChan;

FROM WholeStr IMPORT
    ConvResults, StrToInt, IntToStr, StrToCard, CardToStr;

FROM IOConsts IMPORT
    ReadResults;

FROM IOChan IMPORT
    SetReadResult;

FROM TextIO IMPORT
    WriteString, WriteChar;

FROM CharClass IMPORT
    IsNumeric;

TYPE
  String = ARRAY [0..63] OF CHAR;

(*===============================*)

  (* The text form of a signed whole number is
       ["+" | "-"], decimal digit, {decimal digit}

     The text form of an unsigned whole number is
       decimal digit, {decimal digit}
  *)

PROCEDURE SetResult(cid: IOChan.ChanId; res : ConvResults);
BEGIN
    CASE res OF
    strAllRight:
        SetReadResult(cid, allRight);
    |
    strOutOfRange:
        SetReadResult(cid, outOfRange);
    |
    strWrongFormat:
        SetReadResult(cid, wrongFormat);
    |
    strEmpty:
        SetReadResult (cid, endOfLine);
    END;
END SetResult;

(*===============================*)


PROCEDURE ReadNumber(cid : IOChan.ChanId;
                     VAR str : ARRAY OF CHAR;
                     sign : BOOLEAN) : ReadResults;
(* new procedure by Stony Brook *)
VAR
    i           : ADRCARD;
    highStr     : ADRCARD;
    ch          : CHAR;
    gotDig      : BOOLEAN;
    gotChar     : BOOLEAN;
    res         : ReadResults;
BEGIN
    str[0] := 0C;

    (* skip leading spaces *)

    IOChan.Look(cid, ch, res);
    WHILE (res = allRight) AND (ch = ' ') DO
        IOChan.SkipLook(cid, ch, res);
    END;

    i := 0;
    gotChar := FALSE;
    gotDig := FALSE;

    IF res = allRight THEN

        (* check for a plus or minus *)

        IF (ch = '-') OR (ch = '+') THEN
            gotChar :=  TRUE;
            IF sign THEN
                str[i] := ch;
                INC(i);
                IOChan.SkipLook(cid, ch, res);
            ELSE
                res := wrongFormat;
            END;
        END;
    END;

    (* read numeric characters *)

    highStr := HIGH(str);
    WHILE (res = allRight) AND (i <= highStr) AND IsNumeric(ch) DO
        gotDig := TRUE;
        str[i] := ch;
        INC(i);
        IOChan.SkipLook(cid, ch, res);
    END;

    IF NOT gotDig THEN
        IF gotChar OR ((res <> endOfLine) AND (res <> endOfInput)) THEN
            res := wrongFormat;
        END;
    ELSIF (i-1) > highStr THEN
        (* eat the remaining numbers *)

        WHILE (res = allRight) AND IsNumeric(ch) DO
            IOChan.SkipLook(cid, ch, res);
        END;

        res := outOfRange;
    ELSIF (res = endOfLine) OR (res = endOfInput) THEN
        res := allRight;
    END;

    IF i <= highStr THEN
        str[i] := '';
    END;

    SetReadResult(cid, res);
    RETURN res;
END ReadNumber;

PROCEDURE ReadInt(cid : IOChan.ChanId; VAR int : INTEGER);
(* modified by Stony Brook *)

  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of a signed whole number.  The value of this number
     is assigned to int.  The read result is set to the value allRight,
     outOfRange, wrongFormat, endOfLine, or endOfInput. *)
VAR
    temp        : String;
    res         : ConvResults;
BEGIN
    IF ReadNumber(cid, temp, TRUE) = allRight THEN
        StrToInt(temp, int, res);
        SetResult(cid, res);
    END;
END ReadInt;

PROCEDURE WriteInt(cid : IOChan.ChanId; int : INTEGER; width : CARDINAL);
  (* Writes the value of int to cid in text form, in a field of the given
     minimum width. A width of zero(0) is special and means only a single
     space character will be output *)
VAR
    temp        : String;
    count       : CARDINAL;
BEGIN
    IF width = 0 THEN
        WriteChar(cid, ' ');
    END;

    IntToStr(int, temp); (* convert the INTEGER to a string *)

    IF LENGTH(temp) < width THEN
        (* write spaces *)

        FOR count := 1 TO width - LENGTH(temp) DO
            WriteString(cid, " ");
        END;
    END;

    WriteString(cid, temp); (* write the string *)
END WriteInt;

PROCEDURE ReadCard(cid : IOChan.ChanId; VAR card : CARDINAL);
(* modified by Stony Brook *)

  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of an unsigned whole number.  The value of this number
     is assigned to card.  The read result is set to the value allRight,
     outOfRange, wrongFormat, endOfLine, or endOfInput. *)
VAR
    temp        : String;
    res         : ConvResults;
BEGIN
    IF ReadNumber(cid, temp, FALSE) = allRight THEN
        StrToCard(temp, card, res);
        SetResult(cid, res);
    END;
END ReadCard;

PROCEDURE WriteCard(cid : IOChan.ChanId; card : CARDINAL; width : CARDINAL);
  (* Writes the value of card to cid in text form, in a field of the given
     minimum width.  A width of zero(0) is special and means only a single
     space character will be output *)
VAR
    temp        : String;
    count       : CARDINAL;
BEGIN
    IF width = 0 THEN
        WriteChar(cid, ' ');
    END;

    CardToStr(card, temp); (* convert the CARDINAL to a string *)

    IF LENGTH(temp) < width THEN
        (* write spaces *)

        FOR count := 1 TO width - LENGTH(temp) DO
            WriteString(cid, " ");
        END;
    END;

    WriteString(cid, temp); (* write the string *)
END WriteCard;

END WholeIO.
