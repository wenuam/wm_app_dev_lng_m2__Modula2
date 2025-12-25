IMPLEMENTATION MODULE StreamFile;

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

  (* Independent sequential data streams *)

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
  streamFiledid : IOLink.DeviceId;


PROCEDURE WrongDeviceEx;
BEGIN
    IOLink.RAISEdevException(IOChan.InvalidChan(),
                             streamFiledid,
                             IOChan.notAvailable,
                             "Not a StreamFile device");
END WrongDeviceEx;

(* =========================================================== *)

PROCEDURE Open(VAR cid : ChanId;
               name : ARRAY OF CHAR;
               flags : FlagSet;
               VAR res : OpenResults);
(* modified by Stony Brook *)

  (* Attempts to obtain and open a channel connected to a sequential
     stream of the given name. The read flag implies old; without the raw
     flag, text is implied. If successful, assigns to cid the identity of
     the opened channel, and assigns the value opened to res. If a channel
     cannot be opened as required, the value of res indicates the reason,
     and cid identifies the invalid channel.   *)
VAR
    p   : IOLink.DeviceTablePtr;
BEGIN
    IOLink.MakeChan(streamFiledid, cid);
    IF cid <> IOChan.InvalidChan() THEN

        (* read implies old *)

        IF read <= flags THEN
            flags := flags + old;
        END;

        (* without raw, text is implied *)

        IF NOT (raw <= flags) THEN
            flags := flags + text;
        END;

        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
        p^.flags := flags;

        res := ISOfiler.Open(p, name, write <= flags);

        IF res = opened THEN
            IF NOT (old <= flags) THEN
                ISOfiler.Close(p);
                res := fileExists;
            END;
        ELSIF res = noSuchFile THEN
            (* only create when *only* write is present *)

            IF (write <= flags) AND NOT (read <= flags) THEN
                res := ISOfiler.Create(p, name);
            END;
        END;

        IF res = opened THEN
            ISOfiler.AssignProcs(p, TRUE, FALSE, flags);
        ELSE
            IOLink.UnMakeChan(streamFiledid, cid);
            cid := IOChan.InvalidChan();
        END;
    ELSE
        res := outOfChans;
    END;
END Open;

  (* ================================== *)

PROCEDURE IsStreamFile(cid : ChanId) : BOOLEAN;
  (* Tests if the channel identified by cid is open to a sequential stream. *)
BEGIN
    RETURN IOLink.IsDevice(cid, streamFiledid);
END IsStreamFile;

PROCEDURE Close(VAR cid : ChanId);
  (* If the channel identified by cid is not open to a sequential stream,
     the exception wrongDevice is raised; otherwise closes the channel, and
     assigns the value identifying the invalid channel to cid.   *)
VAR
    p : IOLink.DeviceTablePtr;
BEGIN
    IF IsStreamFile(cid) THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);
        p^.doFree(p);
        IOLink.UnMakeChan(streamFiledid, cid);
        cid := IOChan.InvalidChan();
    ELSE
        WrongDeviceEx;
    END;
END Close;

BEGIN
    (* in case this module is part of a WIN32 DLL, that inits per thread *)

    IF NOT SYSTEM.IsThread THEN
        IOLink.AllocateDeviceId(streamFiledid);
    END;
END StreamFile.
