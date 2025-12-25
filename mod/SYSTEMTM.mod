(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE SYSTEMTM;
<*/NOOPT:X*>

PROCEDURE IsTerminating() : BOOLEAN;
BEGIN
    RETURN Terminating;
END IsTerminating;

PROCEDURE HasHalted() : BOOLEAN;
BEGIN
    RETURN HaltCalled;
END HasHalted;

PROCEDURE CallProc(theProc : TermProcType);
BEGIN
    theProc;

EXCEPT
    (* trap and handle all exceptions *)
    RETURN;
END CallProc;

PROCEDURE RunTerm;
VAR
    theProc             : TermProcType;
    saveTermProc        : TermProcType;
    saveTerm            : BOOLEAN;
BEGIN
    (* we save this because of thread DLL termination, if selected *)

    saveTermProc := TermProc;
    saveTerm := Terminating;

    Terminating := TRUE;
    WHILE TermProc <> NILPROC DO
        theProc := TermProc;
        TermProc := NILPROC;

        (* the called termination proc sets its saved termination *)
        (* proc to TermProc, thus continuing the loop of term procs. *)
        (* this is how the module finally sections operate due to *)
        (* ISO rules about exception handling in finally sections. *)

        (* users installing term procs using haltproc can simply call *)
        (* the old handler returned by haltproc, or they can mimic *)
        (* the finally behavior by calling haltproc with the saved old term proc *)
        (* before returning from their term proc. *)

        CallProc(theProc);
    END;

    Terminating := saveTerm;
    TermProc := saveTermProc;

(*EXCEPT don't do this. loop can only fail if TermProc data was clobbered.
    saveTermProc := TermProc;
    saveTerm := Terminating;
    RETURN*)
END RunTerm;

END SYSTEMTM.
