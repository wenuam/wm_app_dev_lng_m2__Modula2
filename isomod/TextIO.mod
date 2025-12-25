IMPLEMENTATION MODULE TextIO;

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

  (* Input and output of character and string types over specified channels.
     The read result is of the type IOConsts.ReadResults.
  *)

IMPORT IOChan, IOConsts, SYSTEM;

  (* The following procedures do not read past line marks *)

PROCEDURE ReadChar(cid : IOChan.ChanId; VAR ch : CHAR);
(* Modified by Stony Brook *)
  (* If possible, removes a character from the input stream cid and
     assigns the corresponding value to ch.  The read result is set to
     the value allRight, endOfLine, or endOfInput.
  *)
VAR
    res         : IOConsts.ReadResults;
BEGIN
    IOChan.Look(cid, ch, res);
    IF res = IOConsts.allRight THEN
        IOChan.Skip(cid);
    END;
END ReadChar;

PROCEDURE ReadRestLine(cid : IOChan.ChanId; VAR s : ARRAY OF CHAR);
(* Modified by Stony Brook *)
  (* Removes any remaining characters from the input stream cid before
     the next line mark, copying to s as many as can be accommodated as
     a string value. The read result is set to the value allRight,
     outOfRange, endOfLine, or endOfInput.  *)
VAR
    count       : CARDINAL;
    ch          : CHAR;
    res         : IOConsts.ReadResults;
BEGIN
    s[0] := '';
    count := 0;
    IOChan.Look(cid, ch, res);
    WHILE res = IOConsts.allRight DO
        IF count <= HIGH(s) THEN
            s[count] := ch;
        END;
        INC(count);
        IOChan.SkipLook(cid, ch, res);
    END;

    IF count <> 0 THEN
        IF count <= HIGH(s)+1 THEN
            IF count < HIGH(s)+1 THEN
                s[count] := '';
            END;
            IOChan.SetReadResult(cid, IOConsts.allRight)
        ELSE
            IOChan.SetReadResult(cid, IOConsts.outOfRange)
        END;
    END;
END ReadRestLine;

PROCEDURE ReadString(cid : IOChan.ChanId; VAR s : ARRAY OF CHAR);
  (* Removes only those characters from the input stream cid before the
     next line mark that can be accommodated in s as a string value, and
     copies them to s.  The read result is set to the value allRight,
     endOfLine, or endOfInput.  *)
VAR
    numToRead, numRead : CARDINAL;
BEGIN
    s[0] := '';
    numToRead := HIGH(s) + 1;
    IOChan.TextRead(cid, SYSTEM.ADR(s), numToRead, numRead);
    IF numRead < numToRead THEN
        s[numRead] := '';
    END;
    IF numRead > 0 THEN
        (* else already set to endOfLine or endOfInput *)

        IOChan.SetReadResult(cid, IOConsts.allRight)
    END;
END ReadString;

PROCEDURE ReadToken(cid : IOChan.ChanId; VAR s : ARRAY OF CHAR);
(* Modified by Stony Brook *)
  (* Skips leading spaces, and then removes characters from the input
     stream cid before the next space or line mark, copying to s as
     many as can be accommodated as a string value. The read result is
     set to the value allRight, outOfRange, endOfLine, or end OfInput. *)
VAR
    count       : CARDINAL;
    ch          : CHAR;
    res         : IOConsts.ReadResults;
BEGIN
    IOChan.Look(cid, ch, res);
    WHILE (ch = " ") AND (res = IOConsts.allRight) DO
        IOChan.SkipLook(cid, ch, res);
    END;

    s[0] := '';
    count := 0;
    WHILE (ch <> " ") AND (res = IOConsts.allRight) DO
        IF count <= HIGH(s) THEN
            s[count] := ch;
        END;
        INC(count);
        IOChan.SkipLook(cid, ch, res);
    END;

    IF count <> 0 THEN
        IF count <= HIGH(s)+1 THEN
            IF count < HIGH(s)+1 THEN
                s[count] := '';
            END;
            IOChan.SetReadResult(cid, IOConsts.allRight)
        ELSE
            IOChan.SetReadResult(cid, IOConsts.outOfRange)
        END;
    END;
END ReadToken;

  (* The following procedure reads past the next line mark *)

PROCEDURE SkipLine(cid : IOChan.ChanId);
  (* Removes successive items from the input stream cid up to and
     including the next line  mark, or until the end of input is reached.
     The read result is set to the value allRight, or endOfInput.  *)
VAR
    res : IOConsts.ReadResults;
    ch  : CHAR;
BEGIN
    IOChan.Look(cid, ch, res);
    WHILE res = IOConsts.allRight DO
        IOChan.SkipLook(cid, ch, res);
    END;

    IF res = IOConsts.endOfLine THEN
        IOChan.Skip(cid);
        IOChan.SetReadResult(cid, IOConsts.allRight);
    END;
END SkipLine;

  (* Output procedures *)

PROCEDURE WriteChar(cid : IOChan.ChanId; ch : CHAR);
  (* Writes the value of ch to the output stream cid. *)
BEGIN
    IOChan.TextWrite(cid, SYSTEM.ADR(ch), 1)
END WriteChar;

PROCEDURE WriteLn(cid : IOChan.ChanId);
  (* Writes a line mark to the output stream cid. *)
BEGIN
    IOChan.WriteLn(cid)
END WriteLn;

PROCEDURE WriteString(cid : IOChan.ChanId; s : ARRAY OF CHAR);
  (* Writes the string value in s to the output stream cid. *)
BEGIN
    IOChan.TextWrite(cid, SYSTEM.ADR(s), LENGTH(s));
END WriteString;

END TextIO.
