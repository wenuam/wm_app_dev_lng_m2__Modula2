(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE LWholeIO;

  (* Input and output of whole numbers in decimal text form over specified
channels. The read result is of the type IOConsts.ReadResults.
  *)

FROM SYSTEM IMPORT
    ADRCARD;

IMPORT IOChan;

FROM LWholeStr IMPORT
    ConvResults, StrToLongInt, LongIntToStr, StrToLongCard, LongCardToStr;

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

PROCEDURE ReadNumber(cid : IOChan.ChanId;
                     VAR str : ARRAY OF CHAR;
                     sign : BOOLEAN) : ReadResults;
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

PROCEDURE ReadLongInt(cid : IOChan.ChanId; VAR int : LONGINT);
  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of a signed whole number.  The value of this number
     is assigned to int.  The read result is set to the value allRight,
     outOfRange, wrongFormat, endOfLine, or endOfInput. *)
VAR
    temp        : String;
    res         : ConvResults;
BEGIN
    IF ReadNumber(cid, temp, TRUE) = allRight THEN
        StrToLongInt(temp, int, res);
        SetResult(cid, res);
    END;
END ReadLongInt;

PROCEDURE WriteLongInt(cid : IOChan.ChanId; int : LONGINT; width : CARDINAL);
  (* Writes the value of int to cid in text form, in a field of the given
     minimum width.A width of zero(0) is special and means only a single
     space character will be output *)
VAR
    temp        : String;
    count       : CARDINAL;
BEGIN
    IF width = 0 THEN
        WriteChar(cid, ' ');
    END;

    LongIntToStr(int, temp); (* convert the LONGINT to a string *)

    IF LENGTH(temp) < width THEN
        (* write spaces *)

        FOR count := 1 TO width - LENGTH(temp) DO
            WriteString(cid, " ");
        END;
    END;

    WriteString(cid, temp); (* write the string *)
END WriteLongInt;

PROCEDURE ReadLongCard(cid : IOChan.ChanId; VAR card : LONGCARD);
  (* Skips leading spaces, and removes any remaining characters from cid
     that form part of an unsigned whole number.  The value of this number
     is assigned to card.  The read result is set to the value allRight,
     outOfRange, wrongFormat, endOfLine, or endOfInput. *)
VAR
    temp        : String;
    res         : ConvResults;
BEGIN
    IF ReadNumber(cid, temp, FALSE) = allRight THEN
        StrToLongCard(temp, card, res);
        SetResult(cid, res);
    END;
END ReadLongCard;

PROCEDURE WriteLongCard(cid : IOChan.ChanId; card : LONGCARD; width : CARDINAL);
  (* Writes the value of card to cid in text form, in a field of the given
     minimum width.A width of zero(0) is special and means only a single
     space character will be output *)
VAR
    temp        : String;
    count       : CARDINAL;
BEGIN
    IF width = 0 THEN
        WriteChar(cid, ' ');
    END;

    LongCardToStr(card, temp); (* convert the LONGCARD to a string *)

    IF LENGTH(temp) < width THEN
        (* write spaces *)

        FOR count := 1 TO width - LENGTH(temp) DO
            WriteString(cid, " ");
        END;
    END;

    WriteString(cid, temp); (* write the string *)
END WriteLongCard;

END LWholeIO.
