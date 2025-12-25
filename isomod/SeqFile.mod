IMPLEMENTATION MODULE SeqFile;

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

  (* Rewindable sequential files *)

IMPORT IOLink, IOChan, SYSTEM;

IMPORT ISOfiler;

(*
TYPE
  ChanId = IOChan.ChanId;
  FlagSet = ChanConsts.FlagSet;
  OpenResults = ChanConsts.OpenResults;
*)

  (* Accepted singleton values of FlagSet *)

(* CONST
  read = FlagSet{ChanConsts.readFlag};   (* input operations are requested/available *)
  write = FlagSet{ChanConsts.writeFlag}; (* output operations are requested/available *)
  old = FlagSet{ChanConsts.oldFlag};     (* a file may/must/did exist before the channel is opened *)
  text = FlagSet{ChanConsts.textFlag};   (* text operations are requested/available *)
  raw = FlagSet{ChanConsts.rawFlag};     (* raw operations are requested/available *)
*)

VAR
    seqFiledid : IOLink.DeviceId;

PROCEDURE WrongDeviceEx;
BEGIN
    IOLink.RAISEdevException(IOChan.InvalidChan(),
                             seqFiledid,
                             IOChan.notAvailable,
                             "Not SeqFile device");
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
    IOLink.MakeChan(seqFiledid, cid);
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
            ISOfiler.AssignProcs(p, TRUE, TRUE, flags);
        ELSE
            IOLink.UnMakeChan(seqFiledid, cid);
            cid := IOChan.InvalidChan();
        END;
    ELSE
        res := outOfChans;
    END;
END Open;


  (* ================================== *)

PROCEDURE OpenWrite(VAR cid : ChanId;
                    name : ARRAY OF CHAR;
                    flags : FlagSet;
                    VAR res : OpenResults);
(* modified by Stony Brook *)

  (* Attempts to obtain and open a channel connected to a stored rewindable
     file of the given name. The write flag is implied; without the raw flag,
     text is implied. If successful, assigns to cid the identity of the
     opened channel, assigns the value opened to res, and selects output mode,
     with the write position at the start of the file
     (i.e. the file is of zero length).
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel.
  *)
VAR
    p   : IOLink.DeviceTablePtr;
BEGIN
    flags := flags + write;

    IF NOT (raw <= flags) THEN
        flags := flags + text;
    END;

    Open(cid, name, flags, TRUE, res);

    IF res = opened THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
        ISOfiler.Truncate(p);
    END;
END OpenWrite;

PROCEDURE OpenAppend(VAR cid : ChanId;
                     name : ARRAY OF CHAR;
                     flags : FlagSet;
                     VAR res : OpenResults);
(* modified by Stony Brook *)

  (* Attempts to obtain and open a channel connected to a stored rewindable
     file of the given name.
     The write and old flags are implied; without the raw flag, text is
     implied. If successful, assigns to cid the identity of the opened
     channel, assigns the value opened to res, and selects output mode,
     with the write position corresponding to the length of the file.
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel.
  *)
VAR
    p   : IOLink.DeviceTablePtr;
BEGIN
    flags := flags + write + old;

    IF NOT (raw <= flags) THEN
        flags := flags + text;
    END;

    Open(cid, name, flags, TRUE, res);

    IF res = opened THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
        ISOfiler.SetPos(p, ISOfiler.EndPos(p));
    END;
END OpenAppend;

PROCEDURE OpenRead(VAR cid : ChanId;
                   name : ARRAY OF CHAR;
                   flags : FlagSet;
                   VAR res : OpenResults);
(* modified by Stony Brook *)

  (* Attempts to obtain and open a channel connected to a stored rewindable
     file of the given name. The read and old flags are implied; without the
     raw flag, text is implied.If successful, assigns to cid the identity
     of the opened channel, assigns the value opened to res, and selects
     input mode, with the read position corresponding to the start of the
     file. If a channel cannot be opened as required, the value of res
     indicates the reason, and cid identifies the invalid channel.
  *)
BEGIN
    flags := flags + read + old;

    IF NOT (raw <= flags) THEN
        flags := flags + text;
    END;

    Open(cid, name, flags, FALSE, res);
END OpenRead;

PROCEDURE IsSeqFile(cid : ChanId) : BOOLEAN;
  (* Tests if the channel identified by cid is open to a rewindable
     sequential file. *)
BEGIN
    RETURN IOLink.IsDevice(cid, seqFiledid);
END IsSeqFile;


PROCEDURE Reread(cid : ChanId);
  (* If the channel identified by cid is not open to a rewindable sequential
     file, the exception wrongDevice is raised; otherwise attempts to set
     the read position to the start of the file, and to select input mode.
     If the operation cannot be performed (perhaps because of insufficient
     permissions) neither input mode nor output mode is selected.
  *)
VAR
    p : IOLink.DeviceTablePtr;
BEGIN
    IF IsSeqFile(cid) THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
        IOChan.Reset(cid);
        p^.flags := p^.flags-write+read;
    ELSE
        WrongDeviceEx;
    END;
END Reread;

PROCEDURE Rewrite(cid : ChanId);
  (* If the channel identified by cid is not open to a rewindable sequential
     file, the exception wrongDevice is raised; otherwise, attempts to
     truncate the file to zero length, and to select output mode.
     If the operation cannot be performed (perhaps because of insufficient
     permissions) neither input mode nor output mode is selected.
  *)
VAR
    p : IOLink.DeviceTablePtr;
BEGIN
    IF IsSeqFile(cid) THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
        IOChan.Reset(cid);
        p^.flags := p^.flags-read+write;
    ELSE
        WrongDeviceEx;
    END;
END Rewrite;


PROCEDURE Close(VAR cid : ChanId);
  (* If the channel identified by cid is not open to a rewindable sequential
     file, the exception wrongDevice is raised; otherwise closes the channel,
     and assigns the value identifying the invalid channel to cid.
  *)
VAR
    p : IOLink.DeviceTablePtr;
BEGIN
    IF IsSeqFile(cid) THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
        p^.doFree(p);
        IOLink.UnMakeChan(seqFiledid, cid);
        cid := IOChan.InvalidChan();
    ELSE
        WrongDeviceEx;
    END;
END Close;

BEGIN
    (* in case this module is part of a WIN32 DLL, that inits per thread *)

    IF NOT SYSTEM.IsThread THEN
        IOLink.AllocateDeviceId(seqFiledid);
    END;
END SeqFile.
