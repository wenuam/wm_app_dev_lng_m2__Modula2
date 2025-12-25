IMPLEMENTATION MODULE STextIO;

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

   (* Input and output of character and string types over default channels. The read result
    is of the type IOConsts.ReadResults. *)

IMPORT TextIO;

FROM StdChans IMPORT
    InChan, OutChan;

  (* The following procedures do not read past line marks *)

PROCEDURE ReadChar(VAR ch : CHAR);
  (* If possible, removes a character from the default input stream,
     and assigns the corresponding value to ch.  The read result is set
     to allRight, endOfLine or endOfInput. *)
BEGIN
    TextIO.ReadChar(InChan(), ch);
END ReadChar;

PROCEDURE ReadRestLine(VAR s : ARRAY OF CHAR);
  (* Removes any remaining characters from the default input stream before
     the next line mark, copying to s as many as can be accommodated as a
     string value.  The read result is set to the value allRight,
     outOfRange, endOfLine, or endOfInput. *)
BEGIN
    TextIO.ReadRestLine(InChan(), s);
END ReadRestLine;

PROCEDURE ReadString(VAR s : ARRAY OF CHAR);
  (* Removes only those characters from the default input stream before the
     next line mark that can be accommodated in s as a string value, and
     copies them to s.  The read result is set to the value allRight,
     endOfLine, or endOfInput. *)
BEGIN
    TextIO.ReadString(InChan(), s);
END ReadString;

PROCEDURE ReadToken(VAR s : ARRAY OF CHAR);
  (* Skips leading spaces, and then removes characters from the default
     input stream before the next space or line mark, copying to s as
     many as can be accommodated as a string value.  The read result is
     set to the value allRight, outOfRange, endOfLine, or endOfInput. *)
BEGIN
    TextIO.ReadToken(InChan(), s);
END ReadToken;

  (* The following procedure reads past the next line mark *)

PROCEDURE SkipLine;
  (* Removes successive items from the default input stream up to and
     including the next line mark or until the end of input is reached.
     The read result is set to the value allRight, or endOfInput. *)
BEGIN
    TextIO.SkipLine(InChan());
END SkipLine;

  (* Output procedures *)

PROCEDURE WriteChar(ch : CHAR);
  (* Writes the value of ch to the default output stream. *)
BEGIN
    TextIO.WriteChar(OutChan(), ch);
END WriteChar;

PROCEDURE WriteLn;
  (* Writes a line mark to the default output stream. *)
BEGIN
    TextIO.WriteLn(OutChan());
END WriteLn;

PROCEDURE WriteString(s : ARRAY OF CHAR);
  (* Writes the string value of s to the default output stream. *)
BEGIN
    TextIO.WriteString(OutChan(), s);
END WriteString;

END STextIO.
