IMPLEMENTATION MODULE SLongIO;

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

  (* Input and output of real numbers in decimal text form over default
     channels.  The read result is of the type IOConsts.ReadResults.
  *)

  (* The text form of a signed fixed-point real number is
       ["+" | "-"], decimal digit, {decimal digit},
       [".", {decimal digit}]

     The text form of a signed floating-point real number is
       signed fixed-point real number,
       "E", ["+" | "-"], decimal digit, {decimal digit}
  *)


IMPORT StdChans, LongIO;

PROCEDURE ReadReal(VAR real : LONGREAL);
  (* Skips leading spaces, and removes any remaining characters from
     the default input channel that form part of a signed fixed or
     floating point number. The value of this number is assigned to real.
     The read result is set to the value allRight, outOfRange, wrongFormat,
     endOfLine, or endOfInput.
  *)
BEGIN
    LongIO.ReadReal(StdChans.InChan(), real);
END ReadReal;

PROCEDURE WriteFloat(real : LONGREAL; sigFigs : CARDINAL; width : CARDINAL);
  (* Writes the value of real to the default output channel in
     floating-point text form, with sigFigs significant figures, in a
     field of the given minimum width. *)
BEGIN
    LongIO.WriteFloat(StdChans.OutChan(), real, sigFigs, width);
END WriteFloat;

PROCEDURE WriteEng(real : LONGREAL; sigFigs : CARDINAL; width : CARDINAL);
  (* As for WriteFloat, except that the number is scaled with one to three
     digits in the whole number part, and with an exponent that is a
     multiple of three. *)
BEGIN
    LongIO.WriteEng(StdChans.OutChan(), real, sigFigs, width);
END WriteEng;

PROCEDURE WriteFixed(real : LONGREAL; place, width : CARDINAL);
  (* Writes the value of real to the default output channel in
     fixed-point text form, rounded to the given place relative to the
     decimal point, in a field of the given minimum width. *)
BEGIN
    LongIO.WriteFixed(StdChans.OutChan(), real, place, width);
END WriteFixed;

PROCEDURE WriteReal(real : LONGREAL; width : CARDINAL);
  (* Writes the value of real to the default output channel, as WriteFixed
     if the sign and magnitude can be shown in the given width, or otherwise
     as WriteFloat.  The number of places or significant digits depends on
     the given width. *)
BEGIN
    LongIO.WriteReal(StdChans.OutChan(), real, width);
END WriteReal;

END SLongIO.
