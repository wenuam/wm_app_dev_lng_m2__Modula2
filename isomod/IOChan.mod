IMPLEMENTATION MODULE IOChan;


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

 (* Types and procedures forming the interface to channels for
device-independent data transfer modules *)

IMPORT IOConsts, ChanConsts, SYSTEM, IOLink;


TYPE
    ChanId = POINTER TO SYSTEM.LOC;
    (* Values of this type are used to identify channels *)

  (* There is one pre-defined value identifying an invalid channel on
     which no data transfer operations are available.  It may be used
     to initialize variables of type ChanId. *)

VAR
    invalidDevice       : IOLink.DeviceId;
    invalidChan         : ChanId;

PROCEDURE InvalidGetNameProc(p : IOLink.DeviceTablePtr;
                             VAR str : ARRAY OF CHAR);
BEGIN
    str := "Invalid Channel";
END InvalidGetNameProc;

PROCEDURE InvalidChan() : ChanId;
  (* Returns the value identifying the invalid channel. *)
VAR
    p           : IOLink.DeviceTablePtr;
BEGIN
    IF invalidChan = NIL THEN
        IOLink.AllocateDeviceId(invalidDevice);
        IOLink.MakeChan(invalidDevice, invalidChan);
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, invalidChan);
        p^.doGetName := InvalidGetNameProc;
    END;
    RETURN invalidChan;
END InvalidChan;

 (* For each of the following operations, if the device supports the
    operation on the channel, the behaviour of the procedure conforms with
    the description below.  The full behaviour is defined for each device
    module.  If the device does not support the operation on the channel,
    the behaviour of the procedure is to raise the exception notAvailable. *)

  (* Text operations - these perform any required translation between the
     internal and external representation of text. *)

PROCEDURE Look(cid : ChanId; VAR ch : CHAR; VAR res : IOConsts.ReadResults);
  (* If there is a character as the next item in the input stream cid,
     assigns its value to ch without removing it from the stream; otherwise
     the value of ch is not defined.  res (and the stored read result) are
     set to the value allRight, endOfLine, or endOfInput.
  *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    IF cid <> NIL THEN
        did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
        did^.doLook(did, ch, res);
    ELSE
    END;
END Look;

PROCEDURE Skip(cid : ChanId);
    (* If the input stream cid has ended, the exception skipAtEnd is raised;
       otherwise the next character or line mark in cid is removed, and the
       stored read result is set to the value allRight. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.doSkip(did);
END Skip;

PROCEDURE SkipLook(cid : ChanId; VAR ch : CHAR; VAR res : IOConsts.ReadResults);
  (* If the input stream cid has ended, the exception skipAtEnd is raised;
     otherwise the next character or line mark in cid is removed.  If there
     is a character as the next item in cid stream, assigns its value to ch
     without removing it from the stream.
     Otherwise, the value of ch is not defined.  res (and the stored read
     result) are set to the value allRight, endOfLine, or endOfInput. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.doSkipLook(did, ch, res);
END SkipLook;

PROCEDURE WriteLn(cid : ChanId);
  (* Writes a line mark over the channel cid. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.doLnWrite(did);
END WriteLn;

PROCEDURE TextRead(cid : ChanId;
                   to : SYSTEM.ADDRESS;
                   maxChars : CARDINAL;
                   VAR charsRead: CARDINAL);
  (* Reads at most maxChars characters from the current line in cid, and
     assigns corresponding values to successive components of an
     ARRAY OF CHAR variable for which the address of the first component is
     to. The number of characters read is assigned to charsRead. The stored
     read result is set to allRight, endOfLine, or endOfInput. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.doTextRead(did, to, maxChars, charsRead);
END TextRead;

PROCEDURE TextWrite(cid : ChanId;
                    from : SYSTEM.ADDRESS;
                    charsToWrite : CARDINAL);
  (* Writes a number of characters given by the value of charsToWrite,
     from successive components of an ARRAY OF CHAR variable for which
     the address of the first component is from, to the channel cid. *)

  (* Direct raw operations  - these do not effect translation between the
     internal and external representation of data *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.doTextWrite(did, from, charsToWrite);
END TextWrite;

  (* Direct raw operations  - these do not effect translation between the
internal and external representation of data
  *)

PROCEDURE RawRead(cid : ChanId;
                  to : SYSTEM.ADDRESS;
                  maxLocs : CARDINAL;
                  VAR locsRead : CARDINAL);
  (* Reads at most maxLocs items from cid, and assigns corresponding values
     to successive components of an ARRAY OF LOC variable for which the
     address of the first component is to. The number of characters read is
     assigned to charsRead. The stored read result is set to the value
     allRight, or endOfInput. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.doRawRead(did, to, maxLocs, locsRead);
END RawRead;

PROCEDURE RawWrite(cid : ChanId;
                   from : SYSTEM.ADDRESS;
                   locsToWrite : CARDINAL);
  (* Writes a number of items given by the value of charsToWrite, from
     successive components of an ARRAY OF LOC variable for which the
     address of the first component is from, to the channel cid. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.doRawWrite(did, from, locsToWrite);
END RawWrite;

  (* Common operations *)

PROCEDURE GetName(cid : ChanId; VAR s : ARRAY OF CHAR);
  (* Copies to s a name associated with the channel cid, possibly truncated
     (depending on the capacity of s).  *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.doGetName(did, s);
END GetName;

PROCEDURE Reset(cid : ChanId);
  (* Resets the channel cid to a state defined by the device module. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.doReset(did);
END Reset;

PROCEDURE Flush(cid : ChanId);
  (* Flushes any data buffered by the device module out to the channel cid. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.doFlush(did);
END Flush;

  (* Access to read results *)

PROCEDURE SetReadResult(cid : ChanId; res : IOConsts.ReadResults);
  (* Sets the read result value for the channel cid to the value res. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    did^.result := res;
END SetReadResult;

PROCEDURE ReadResult(cid : ChanId) : IOConsts.ReadResults;
  (* Returns the stored read result value for the channel cid. (This is
     initially the value notKnown). *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    RETURN did^.result;
END ReadResult;

  (* Users can discover which flags actually apply to a channel *)

PROCEDURE CurrentFlags(cid : ChanId) : ChanConsts.FlagSet;
  (* Returns the set of flags that currently apply to the channel cid. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    RETURN did^.flags;
END CurrentFlags;

  (* The following exceptions are defined for this module and its clients *)

(* TYPE
  ChanExceptions =
    (wrongDevice,      (* device specific operation on wrong device *)
     notAvailable,     (* operation attempted that is not available on that channel *)
     skipAtEnd,        (* attempt to skip data from a stream that has ended *)
     softDeviceError,  (* device specific recoverable error *)
     hardDeviceError,  (* device specific non-recoverable error *)
     textParseError,   (* input data does not correspond to a character or linemark - optional detection *)
     notAChannel       (* given value does not identify a channel - optional detection *)
    );
*)


PROCEDURE IsChanException() : BOOLEAN;
  (* Returns TRUE if the current coroutine is in the exceptional execution
     state because of the raising of an exception from ChanExceptions;
     otherwise returns FALSE. *)
BEGIN
    RETURN IOLink.IsIOException();
END IsChanException;

PROCEDURE ChanException() : ChanExceptions;
  (* If the current coroutine is in the exceptional execution state
     because of the raising of an exception from ChanExceptions,
     returns the corresponding enumeration value, and otherwise raises
     an exception.
  *)
BEGIN
    RETURN IOLink.IOException();
END ChanException;

(* When a device procedure detects a device error, it raises the
 exception softDeviceError or hardDeviceError.  If these exceptions
 are handled, the following facilities may be used to discover an
 implementation-defined error number for the channel.
*)

(* TYPE
  DeviceErrNum = INTEGER;
*)

PROCEDURE DeviceError(cid : ChanId) : DeviceErrNum;
  (* If a device error exception has been raised for the channel cid,
     returns the error number stored by the device module. *)
VAR
    did : IOLink.DeviceTablePtr;
BEGIN
    did := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
    RETURN did^.errNum;
END DeviceError;

BEGIN
    invalidChan := NIL;
END IOChan.
