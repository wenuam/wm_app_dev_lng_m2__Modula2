IMPLEMENTATION MODULE RndFile;

  (* Random access files *)


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

IMPORT IOLink, IOChan, SYSTEM, EXCEPTIONS;

IMPORT ISOfiler;

(*
TYPE
  ChanId = IOChan.ChanId;
  FlagSet = ChanConsts.FlagSet;
  OpenResults = ChanConsts.OpenResults;

  (* Accepted singleton values of FlagSet *)

CONST
  read = FlagSet{ChanConsts.readFlag};   (* input operations are requested/available *)
  write = FlagSet{ChanConsts.writeFlag}; (* output operations are requested/available *)
  old = FlagSet{ChanConsts.oldFlag};     (* a file may/must/did exist before the channel is opened *)
  text = FlagSet{ChanConsts.textFlag};   (* text operations are requested/available *)
  raw = FlagSet{ChanConsts.rawFlag};     (* raw operations are requested/available *)
*)


VAR
    rndFiledid  : IOLink.DeviceId;
    RndExSrc    : EXCEPTIONS.ExceptionSource;

PROCEDURE WrongDeviceEx;
BEGIN
    IOLink.RAISEdevException(IOChan.InvalidChan(),
                             rndFiledid,
                             IOChan.notAvailable,
                             "Not a RndFile device");
END WrongDeviceEx;

(* common open stuff in following *)

PROCEDURE Open(VAR cid : ChanId;
               name : ARRAY OF CHAR;
               flags : FlagSet;
               create : BOOLEAN;
               VAR res : OpenResults);
(* modified by Stony Brook *)

VAR
    p   : IOLink.DeviceTablePtr;
BEGIN
    IOLink.MakeChan(rndFiledid, cid);
    IF cid <> IOChan.InvalidChan() THEN

        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);

        p^.flags := flags;

        res := ISOfiler.Open(p, name, write <= flags);

        IF res = opened THEN
            IF NOT (old <= flags) THEN
                ISOfiler.Close(p);
                res := fileExists;
            END;
        ELSIF res = noSuchFile THEN
            IF create THEN
                res := ISOfiler.Create(p, name);
            END;
        END;

        IF res = opened THEN
            ISOfiler.AssignProcs(p, FALSE, TRUE, flags);
        ELSE
            IOLink.UnMakeChan(rndFiledid, cid);
            cid := IOChan.InvalidChan();
        END;
    ELSE
        res := outOfChans;
    END;
END Open;

  (* ================================== *)

PROCEDURE OpenOld(VAR cid : ChanId;
                  name : ARRAY OF CHAR;
                  flags : FlagSet;
                  VAR res : OpenResults);
(* modified by Stony Brook *)

  (* Attempts to obtain and open a channel connected to a stored random
     access file of the given name. The old flag is implied; without the
     write flag, read is implied; without the text flag, raw is implied.
     If successful, assigns to cid the identity of the opened channel,
     assigns the value opened to res, and sets the read/write position to
     the start of the file. If a channel cannot be opened as required, the
     value of res indicates the reason, and cid identifies the invalid
     channel.
  *)
BEGIN
    flags := flags + old;

    IF NOT (write <= flags) THEN
        flags := flags + read;
    END;

    IF NOT (text <= flags) THEN
        flags := flags + raw;
    END;

    Open(cid, name, flags, FALSE, res);
END OpenOld;

PROCEDURE OpenClean(VAR cid : ChanId;
                    name : ARRAY OF CHAR;
                    flags : FlagSet;
                    VAR res : OpenResults);
(* modified by Stony Brook *)

  (* Attempts to obtain and open a channel connected to a stored random
     access file of the given name. The write flag is implied;
     without the text flag, raw is implied.
     If successful, assigns to cid the identity of the opened channel,
     assigns the value opened to res, and truncates the file to zero length.
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel.
  *)
VAR
    p   : IOLink.DeviceTablePtr;
BEGIN
    flags := flags + write;

    IF NOT (text <= flags) THEN
        flags := flags + raw;
    END;

    Open(cid, name, flags, TRUE, res);

    IF res = opened THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
        ISOfiler.Truncate(p);
    END;
END OpenClean;

PROCEDURE IsRndFile(cid : ChanId) : BOOLEAN;
  (* Tests if the channel identified by cid is open to a random access file.
  *)
BEGIN
    RETURN IOLink.IsDevice(cid, rndFiledid);
END IsRndFile;

PROCEDURE IsRndFileException() : BOOLEAN;
(* modified by Stony Brook *)

  (* Returns TRUE if the current coroutine is in the exceptional execution
     state because of the raising of a RndFile exception;
     otherwise returns FALSE.
  *)
BEGIN
    RETURN EXCEPTIONS.IsCurrentSource(RndExSrc);
END IsRndFileException;

(* CONST
  FilePosSize = 4;

TYPE
  FilePos = ARRAY [1 .. FilePosSize] OF SYSTEM.LOC;
 *)

PROCEDURE StartPos(cid : ChanId) : FilePos;
(* modified by Stony Brook *)
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the position of
     the start of the file.
  *)
VAR
    pos : CARDINAL32;
BEGIN
    pos := 0;
    IF NOT IsRndFile(cid) THEN
        WrongDeviceEx;
    END;
    RETURN SYSTEM.CAST(FilePos, pos);
END StartPos;

PROCEDURE CurrentPos(cid : ChanId) : FilePos;
(* modified by Stony Brook *)
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the position of
     the current read/write position.
  *)
VAR
    p   : IOLink.DeviceTablePtr;
    pos : CARDINAL32;
BEGIN
    IF IsRndFile(cid) THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);

        pos := ISOfiler.GetPos(p);
    ELSE
        WrongDeviceEx;
        pos := 0;
    END;
    RETURN SYSTEM.CAST(FilePos, pos);
END CurrentPos;

PROCEDURE EndPos(cid : ChanId) : FilePos;
(* modified by Stony Brook *)
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the first
     position after which there have been no writes.
  *)
VAR
    p   : IOLink.DeviceTablePtr;
    pos : CARDINAL32;
BEGIN
    IF IsRndFile(cid) THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);

        pos := ISOfiler.EndPos(p);
    ELSE
        WrongDeviceEx;
        pos := 0;
    END;
    RETURN SYSTEM.CAST(FilePos, pos);
END EndPos;

PROCEDURE NewPos(cid : ChanId;
                 chunks : INTEGER;
                 chunkSize : CARDINAL;
                 from : FilePos) : FilePos;
(* modified by Stony Brook *)
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise returns the
     position (chunks * chunkSize) relative to the position given by from,
     or raises the exception posRange if the required position cannot be
     represented as a value of type FilePos.
  *)
VAR
    p           : IOLink.DeviceTablePtr;
    pos         : CARDINAL32;
    endP        : CARDINAL32;
    offset      : CARDINAL32;
    neg         : BOOLEAN;

    PROCEDURE badPosException;
    BEGIN
        EXCEPTIONS.RaiseRTL(RndExSrc, 0, "NewPos outside valid file range");
    END badPosException;

BEGIN
    IF NOT IsRndFile(cid) THEN
        WrongDeviceEx;
    END;

    neg := FALSE;
    IF chunks < 0 THEN
        neg := TRUE;
        chunks := -chunks;
    END;

    p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);

    pos := SYSTEM.CAST(CARDINAL32, from);
    <*/PUSH/NOWARN:U*>
    offset := VAL(CARDINAL32, chunks) * VAL(CARDINAL32, chunkSize);
    <*/POP*>

    IF neg THEN
        IF offset > pos THEN
            badPosException;
        END;
        pos := pos - offset;
    ELSE
        endP := ISOfiler.EndPos(p);

        pos := pos + offset;

        IF pos > endP THEN
            badPosException;
        END;
    END;

    RETURN SYSTEM.CAST(FilePos, pos);
END NewPos;

PROCEDURE SetPos(cid : ChanId; pos : FilePos);
(* modified by Stony Brook *)
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise sets the read/write
     position to the value given by pos.
  *)
VAR
    p           : IOLink.DeviceTablePtr;
BEGIN
    IF IsRndFile(cid) THEN
        p := SYSTEM.CAST (IOLink.DeviceTablePtr, cid);

        ISOfiler.SetPos(p, SYSTEM.CAST(CARDINAL32, pos));
    ELSE
        WrongDeviceEx;
    END;
END SetPos;

PROCEDURE Close(VAR cid : ChanId);
  (* If the channel identified by cid is not open to a random access file,
     the exception wrongDevice is raised; otherwise closes the channel, and
     assigns the value identifying the invalid channel to cid.
  *)
VAR
    p : IOLink.DeviceTablePtr;
BEGIN
    IF IsRndFile(cid) THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
        p^.doFree(p);
        IOLink.UnMakeChan(rndFiledid, cid);
        cid := IOChan.InvalidChan();
    ELSE
        WrongDeviceEx;
    END;
END Close;

BEGIN
    (* in case this module is part of a WIN32 DLL, that inits per thread *)

    IF NOT SYSTEM.IsThread THEN
        (* modified by Stony Brook *)

        IOLink.AllocateDeviceId(rndFiledid);
        EXCEPTIONS.AllocateSource(RndExSrc);
    END;
END RndFile.
