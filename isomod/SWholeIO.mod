IMPLEMENTATION MODULE SWholeIO;

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

(* Input and output of whole numbers in decimal text form over default
   channels. The read result is of the type IOConsts.ReadResults. *)

(* The text form of a signed whole number is
   ["+" | "-"], decimal digit, {decimal digit}

   The text form of an unsigned whole number is
   decimal digit, {decimal digit}
*)

IMPORT WholeIO, StdChans;

PROCEDURE ReadInt(VAR int : INTEGER);
(* Skips leading spaces, and removes any remaining characters from the
   default input channel that form part of a signed whole number.  The
   value of this number is assigned to int. The read result is set to
   the value allRight, outOfRange, wrongFormat, endOfLine, or endOfInput. *)
BEGIN
    WholeIO.ReadInt(StdChans.InChan(), int);
END ReadInt;

PROCEDURE WriteInt(int : INTEGER; width : CARDINAL);
(* Writes the value of int to the default output channel in text form,
 in a field of the given minimum width.A width of zero(0) is special
 and means only a single space character will be output *)
BEGIN
    WholeIO.WriteInt(StdChans.OutChan(), int, width);
END WriteInt;

PROCEDURE ReadCard(VAR card : CARDINAL);
  (* Skips leading spaces, and removes any remaining characters from the
     default input channel that form part of an unsigned whole number.
     The value of this  number is assigned to card.  The read result is
     set to the value allRight, outOfRange, wrongFormat, endOfLine,
     or endOfInput. *)
BEGIN
    WholeIO.ReadCard(StdChans.InChan(), card);
END ReadCard;

PROCEDURE WriteCard(card : CARDINAL; width : CARDINAL);
(* Writes the value of card to the default output channel in text form,
 in a field of the given minimum width.A width of zero(0) is special
 and means only a single space character will be output *)
BEGIN
    WholeIO.WriteCard(StdChans.OutChan(), card, width);
END WriteCard;

END SWholeIO.
