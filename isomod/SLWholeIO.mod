(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE SLWholeIO;
(* Input and output of whole numbers in decimal text form over default
   channels. The read result is of the type IOConsts.ReadResults. *)

(* The text form of a signed whole number is
   ["+" | "-"], decimal digit, {decimal digit}

   The text form of an unsigned whole number is
   decimal digit, {decimal digit}
*)

IMPORT LWholeIO, StdChans;

PROCEDURE ReadLongInt(VAR int : LONGINT);
(* Skips leading spaces, and removes any remaining characters from the
   default input channel that form part of a signed whole number.  The
   value of this number is assigned to int. The read result is set to
   the value allRight, outOfRange, wrongFormat, endOfLine, or endOfInput. *)
BEGIN
    LWholeIO.ReadLongInt(StdChans.InChan(), int);
END ReadLongInt;

PROCEDURE WriteLongInt(int : LONGINT; width : CARDINAL);
(* Writes the value of int to the default output channel in text form,
 in a field of the given minimum width.A width of zero(0) is special
 and means only a single space character will be output *)
BEGIN
    LWholeIO.WriteLongInt(StdChans.OutChan(), int, width);
END WriteLongInt;

PROCEDURE ReadLongCard(VAR card : LONGCARD);
  (* Skips leading spaces, and removes any remaining characters from the
     default input channel that form part of an unsigned whole number.
     The value of this  number is assigned to card.  The read result is
     set to the value allRight, outOfRange, wrongFormat, endOfLine,
     or endOfInput. *)
BEGIN
    LWholeIO.ReadLongCard(StdChans.InChan(), card);
END ReadLongCard;

PROCEDURE WriteLongCard(card : LONGCARD; width : CARDINAL);
(* Writes the value of card to the default output channel in text form,
 in a field of the given minimum width.A width of zero(0) is special
 and means only a single space character will be output *)
BEGIN
    LWholeIO.WriteLongCard(StdChans.OutChan(), card, width);
END WriteLongCard;

END SLWholeIO.
