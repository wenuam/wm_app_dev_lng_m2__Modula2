(***************************************************************************)
(*                                                                         *)
(*                       Copyright (C) 2009                                *)
(*                         by ADW Software                                 *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE SYSTEMCO;
<*/NOOPT:X*>
<*/NOWARN:I*>

FROM SYSTEM IMPORT
    ADDRESS, UNREFERENCED_PARAMETER, MAKEADR;

FROM SYSTEMEX IMPORT
    RaiseM2Exception, coException;

PROCEDURE NEWCOROUTINE(p : PROC;
                       WS   : ADDRESS;
                       WS_SIZE : CARDINAL;
                       VAR CR : COROUTINE;
                       initProt : PROTECTION);
BEGIN
    UNREFERENCED_PARAMETER(p);
    UNREFERENCED_PARAMETER(WS);
    UNREFERENCED_PARAMETER(WS_SIZE);
    UNREFERENCED_PARAMETER(CR);
    UNREFERENCED_PARAMETER(initProt);
    RaiseM2Exception(coException, "NEWCOROUTINE is not implemented");
END NEWCOROUTINE;

PROCEDURE TRANSFER(VAR OLDP : COROUTINE; NEWP : COROUTINE);
BEGIN
    UNREFERENCED_PARAMETER(OLDP);
    UNREFERENCED_PARAMETER(NEWP);
    RaiseM2Exception(coException, "TRANSFER is not implemented");
END TRANSFER;

PROCEDURE CURRENT() : COROUTINE;
BEGIN
    RETURN CurrentCo;
END CURRENT;

PROCEDURE PROT() : PROTECTION;
BEGIN
    RETURN CurProt;
END PROT;

PROCEDURE COROUTINEDONE(cr : COROUTINE);
BEGIN
    UNREFERENCED_PARAMETER(cr);
END COROUTINEDONE;

PROCEDURE SETPROT(p : PROTECTION) : PROTECTION [PASS(AX), ALTERS(AX)];
ASSEMBLER;
ASM
        MOV     EAX, p

        %IF PICCode %THEN
        MOV     RDX, GOT CurProt
        XCHG    EAX, [RDX+GOTOFFS CurProt]
        %ELSE
        XCHG    EAX, CurProt
        %END
END SETPROT;

PROCEDURE RESETPROT(p : PROTECTION) [ALTERS()];
BEGIN
    CurProt := p;
END RESETPROT;

PROCEDURE InitSystemCo;
BEGIN
    CurProt := PROTECTION{};
    CurrentCo := MAKEADR(0);
    NextCo := MAKEADR(1);
END InitSystemCo;

END SYSTEMCO.
