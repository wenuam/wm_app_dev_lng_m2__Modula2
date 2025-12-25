IMPLEMENTATION MODULE IOLink;

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

<*/NOWARN*>

  (* Types and procedures for the standard implementation of channels *)

IMPORT IOChan, IOConsts, ChanConsts;
IMPORT SYSTEM;
IMPORT EXCEPTIONS;

FROM ExStorage IMPORT
    AllocateEx, DeallocateEx, GetDefaultHeap;

TYPE
    DeviceId = POINTER TO SYSTEM.LOC;
    (* Values of this type are used to identify new device modules,
       and are normally obtained by them during their initialization. *)

VAR
    NextDevice  : DeviceId = SYSTEM.MAKEADR(0);
    ExSource    : EXCEPTIONS.ExceptionSource;

PROCEDURE ALLOCATE(VAR OUT addr : SYSTEM.ADDRESS; amount : SYSTEM.ADRCARD);
BEGIN
    AllocateEx(addr, amount, GetDefaultHeap());
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR INOUT addr : SYSTEM.ADDRESS; amount : SYSTEM.ADRCARD);
BEGIN
    DeallocateEx(addr, amount, GetDefaultHeap());
END DEALLOCATE;

PROCEDURE AllocateDeviceId(VAR did : DeviceId);
(* modified by Stony Brook *)

  (* Allocates a unique value of type DeviceId, and assigns this value to
     did. *)
BEGIN
    did := NextDevice;
    NextDevice := SYSTEM.ADDADR(NextDevice, 1);
END AllocateDeviceId;

(* ===============================*)

PROCEDURE RaiseWrongDevice;
BEGIN
    EXCEPTIONS.RaiseRTL(ExSource, ORD(IOChan.wrongDevice), "Wrong Device");
END RaiseWrongDevice;

(* ===============================*)

(* next section contains the default procedures for MakeChan *)

PROCEDURE DefLookProc(p : DeviceTablePtr;
                      VAR ch : CHAR;
                      VAR res : IOConsts.ReadResults);
BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      IOChan.notAvailable,
                      "Read service not available on this channel");
END DefLookProc;

PROCEDURE DefSkipProc(p : DeviceTablePtr);
BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      IOChan.notAvailable,
                      "Read service not available on this channel");
END DefSkipProc;

PROCEDURE DefSkipLookProc(p : DeviceTablePtr;
                          VAR ch : CHAR;
                          VAR res : IOConsts.ReadResults);
BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      IOChan.notAvailable,
                      "Read service not available on this channel");
END DefSkipLookProc;

PROCEDURE DefWriteLnProc(p : DeviceTablePtr);
BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      IOChan.notAvailable,
                      "Write service not available on this channel");
END DefWriteLnProc;

PROCEDURE DefTextReadProc(p : DeviceTablePtr;
                          adr: SYSTEM.ADDRESS;
                          c : CARDINAL;
                          VAR d: CARDINAL);
BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      IOChan.notAvailable,
                      "Text read service not available on this channel");
END DefTextReadProc;

PROCEDURE DefTextWriteProc(p : DeviceTablePtr;
                           adr : SYSTEM.ADDRESS;
                           c : CARDINAL);
BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      IOChan.notAvailable,
                      "Text write service not available on this channel");
END DefTextWriteProc;

PROCEDURE DefRawReadProc(p : DeviceTablePtr;
                         adr : SYSTEM.ADDRESS;
                         c : CARDINAL;
                         VAR d : CARDINAL);
BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      IOChan.notAvailable,
                      "Raw read service not available on this channel");
END DefRawReadProc;

PROCEDURE DefRawWriteProc(p : DeviceTablePtr;
                          adr : SYSTEM.ADDRESS;
                          c : CARDINAL);

BEGIN
    RAISEdevException(p^.cid,
                      p^.did,
                      IOChan.notAvailable,
                      "Raw write service not available on this channel");
END DefRawWriteProc;

PROCEDURE DefGetNameProc(p : DeviceTablePtr; VAR str : ARRAY OF CHAR);
BEGIN
    str := "";
END DefGetNameProc;

PROCEDURE DefResetProc(p : DeviceTablePtr);
BEGIN
END DefResetProc;

PROCEDURE DefFlushProc(p : DeviceTablePtr);
BEGIN
END DefFlushProc;

PROCEDURE DefFreeProc(p : DeviceTablePtr);
BEGIN
END DefFreeProc;

(*========================================*)

PROCEDURE MakeChan(did : DeviceId; VAR cid : IOChan.ChanId);
  (* Attempts to make a new channel for the device module identified by did.
     If no more channels can be made, the identity of the invalid channel
     is assigned to cid.
     Otherwise, the identity of a new channel is assigned to cid.  *)
VAR
    p : DeviceTablePtr;
BEGIN
    NEW(p);

    (* set up fields to defaults *)

    p^.cd := NIL;
    p^.did := did;
    p^.cid := SYSTEM.CAST(IOChan.ChanId, p);
    p^.result := IOConsts.notKnown;
    p^.errNum := 0;
    p^.flags := ChanConsts.FlagSet{};
    p^.doLook := DefLookProc;
    p^.doSkip := DefSkipProc;
    p^.doSkipLook := DefSkipLookProc;
    p^.doLnWrite := DefWriteLnProc;
    p^.doTextRead := DefTextReadProc;
    p^.doTextWrite := DefTextWriteProc;
    p^.doRawRead := DefRawReadProc;
    p^.doRawWrite := DefRawWriteProc;
    p^.doGetName := DefGetNameProc;
    p^.doReset := DefResetProc;
    p^.doFlush := DefFlushProc;
    p^.doFree := DefFreeProc;

    cid := SYSTEM.CAST(IOChan.ChanId, p);
END MakeChan;

PROCEDURE UnMakeChan(did : DeviceId; VAR cid : IOChan.ChanId);
  (* If the device module identified by did is not the module that made
     the channel identified by cid, the exception wrongDevice is raised;
     otherwise the channel is deallocated, and the value identifying the
     invalid channel is assigned to cid. *)
VAR
    p : DeviceTablePtr;
BEGIN
    p := SYSTEM.CAST(DeviceTablePtr, cid);
    IF p^.did = did THEN
        DISPOSE(p);
        cid := IOChan.InvalidChan();
    ELSE
        RaiseWrongDevice;
    END;
END UnMakeChan;

  (* The pointer to the device table for a channel is obtained using the following  procedure: *)

PROCEDURE DeviceTablePtrValue(cid : IOChan.ChanId;
                              did : DeviceId) : DeviceTablePtr;
  (* If the device module identified by did is not the module that made the
     channel identified by cid, the exception wrongDevice is raised;
     otherwise returns a pointer to the device table for the channel.  *)
VAR
    p : DeviceTablePtr;
BEGIN
    p := SYSTEM.CAST(DeviceTablePtr, cid);
    IF p^.did = did THEN
        RETURN p;
    ELSE
        RaiseWrongDevice;
        RETURN NIL;
    END;
END DeviceTablePtrValue;

PROCEDURE IsDevice(cid : IOChan.ChanId; did : DeviceId) : BOOLEAN;
  (* Tests if the device module identified by did is the module that made
     the channel dentified by cid. *)
VAR
    p : DeviceTablePtr;
BEGIN
    p := SYSTEM.CAST(DeviceTablePtr, cid);
    IF p^.did = did THEN
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END;
END IsDevice;

PROCEDURE RAISEdevException(cid : IOChan.ChanId;
                            did : DeviceId;
                            x : DevExceptionRange;
                            s : ARRAY OF CHAR);
  (* If the device module identified by did is not the module that made
     the channel identified by cid, the exception wrongDevice is raised;
     otherwise the given exception is raised, and the string value in s
     is included in the exception message. *)
VAR
    p           : DeviceTablePtr;
BEGIN
    p := SYSTEM.CAST(DeviceTablePtr, cid);
    IF p^.did <> did THEN
        RaiseWrongDevice;
    ELSE
        EXCEPTIONS.RaiseRTL(ExSource, ORD(x), s);
    END;
END RAISEdevException;

PROCEDURE IsIOException() : BOOLEAN;
  (* If the current coroutine is in the exceptional execution state
     because of the raising of an exception from ChanExceptions;
     otherwise returns FALSE. *)
BEGIN
    IF EXCEPTIONS.IsExceptionalExecution() THEN
        RETURN EXCEPTIONS.IsCurrentSource(ExSource);
    END;
    RETURN FALSE;
END IsIOException;

PROCEDURE IOException() : IOChan.ChanExceptions;
  (* Returns TRUE if the current coroutine is in the exceptional execution
     state because of the raising of an exception from ChanExceptions,
     returns the corresponding enumeration value, and otherwise raises an
     exception. *)
BEGIN
    RETURN VAL(IOChan.ChanExceptions, EXCEPTIONS.CurrentNumber(ExSource));
END IOException;

PROCEDURE Init;
VAR
    cid : IOChan.ChanId;
BEGIN
    EXCEPTIONS.AllocateSource(ExSource);
    cid := IOChan.InvalidChan();(* allocates invalid chan *)
END Init;

PROCEDURE Term;
VAR
    cid : IOChan.ChanId;
    p   : DeviceTablePtr;
BEGIN
    cid := IOChan.InvalidChan();
    p := SYSTEM.CAST(DeviceTablePtr, cid);
    UnMakeChan(p^.did, cid);
END Term;

BEGIN
    <*/INITORDERBEFORE:IOChan*>
    (* in case this module is part of a WIN32 DLL, that inits per thread *)

    IF NOT SYSTEM.IsThread THEN
        Init;
    END;

FINALLY
    IF NOT SYSTEM.IsThread THEN
        Term;
    END;
END IOLink.
