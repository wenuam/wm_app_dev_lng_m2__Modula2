IMPLEMENTATION MODULE RawIO;

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
        portions copyright (c) 1994
        by ADW Software
    procedures are marked as modified
=========================================== *)

(* Reading and writing data over specified channels using raw operations, that is,
   with no conversion or interpretation.
   The read result is of the type IOConsts.ReadResults.
*)

IMPORT IOChan, SYSTEM;

PROCEDURE Read(cid : IOChan.ChanId; VAR to : ARRAY OF SYSTEM.LOC);
  (* Reads storage units from cid, and assigns them to successive
     components of to. The read result is set to the value allRight,
     wrongFormat, or endOfInput. *)
VAR
    numRead : CARDINAL;
BEGIN
    IOChan.RawRead(cid, SYSTEM.ADR(to), SIZE(to), numRead);
END Read;

PROCEDURE Write(cid : IOChan.ChanId; from : ARRAY OF SYSTEM.LOC);
  (* Writes storage units to cid from successive components of from. *)
BEGIN
    IOChan.RawWrite(cid, SYSTEM.ADR(from), SIZE(from));
END Write;

END RawIO.


