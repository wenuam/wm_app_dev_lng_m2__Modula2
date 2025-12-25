IMPLEMENTATION MODULE StdChans;

<*/VALIDVER:StdChanToTerminal,AutoOpenStd*>
(*<*/VERSION:AutoOpenStd*>*)

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

   (* Access to standard and default channels *)

IMPORT IOChan, IOLink, IOConsts, SYSTEM;

%IF Windows %AND AutoOpenStd %THEN
FROM WINCON IMPORT
    AllocConsole;
%END

%IF StdChanToTerminal %THEN
IMPORT TermFile;
%ELSE
IMPORT ChanConsts, ISOfiler, StdHandles;
%END

(*
TYPE
  ChanId = IOChan.ChanId;
*)
    (* Values of this type are used to identify channels *)

  (* The following functions return the standard channel values.
     These channels cannot be closed.
  *)

VAR
    StdChanDid          : IOLink.DeviceId;

    CurInChan           : ChanId;
    CurOutChan          : ChanId;
    CurErrChan          : ChanId;

    TheNullChan         : ChanId;

    StdInChanId         : ChanId;
    StdOutChanId        : ChanId;
    StdErrChanId        : ChanId;

PROCEDURE StdInChan() : ChanId;
  (* Returns the identity of the implementation-defined standard source for
     program input. *)
BEGIN
    RETURN StdInChanId;
END StdInChan;

PROCEDURE StdOutChan() : ChanId;
  (* Returns the identity of the implementation-defined standard source for
     program output. *)
BEGIN
    RETURN StdOutChanId;
END StdOutChan;

PROCEDURE StdErrChan() : ChanId;
  (* Returns the identity of the implementation-defined standard destination for
     program error messages. *)
BEGIN
    RETURN StdErrChanId;
END StdErrChan;

PROCEDURE NullChan() : ChanId;
  (* Returns the identity of a channel open to the null device. *)
BEGIN
    RETURN TheNullChan;
END NullChan;

(* The following functions return the default channel values *)

PROCEDURE InChan() : ChanId;
  (* Returns the identity of the current default input channel. *)
BEGIN
    RETURN CurInChan;
END InChan;

PROCEDURE OutChan() : ChanId;
  (* Returns the identity of the current default output channel. *)
BEGIN
    RETURN CurOutChan;
END OutChan;

PROCEDURE ErrChan() : ChanId;
  (* Returns the identity of the current default error message channel. *)
BEGIN
    RETURN CurErrChan;
END ErrChan;

(* The following procedures allow for redirection of the default channels *)

PROCEDURE SetInChan(cid : ChanId);
  (* Sets the current default input channel to that identified by cid. *)
BEGIN
    CurInChan := cid;
END SetInChan;

PROCEDURE SetOutChan(cid : ChanId);
  (* Sets the current default output channel to that identified by cid. *)
BEGIN
    CurOutChan := cid;
END SetOutChan;

PROCEDURE SetErrChan(cid : ChanId);
  (* Sets the current default error channel to that identified by cid. *)
BEGIN
    CurErrChan := cid;
END SetErrChan;

(* ===============================*)

(* next section contains the empty procedures for NullChan *)

PROCEDURE NullLookProc(p : IOLink.DeviceTablePtr;
                       VAR ch : CHAR;
                       VAR res : IOConsts.ReadResults);
BEGIN
    p^.result := IOConsts.endOfInput;
    res := p^.result;
    ch := CHR(0);
END NullLookProc;

PROCEDURE NullSkipProc(p : IOLink.DeviceTablePtr);
BEGIN
    p^.result := IOConsts.endOfInput;
END NullSkipProc;

PROCEDURE NullSkipLookProc(p : IOLink.DeviceTablePtr;
                           VAR ch : CHAR;
                           VAR res : IOConsts.ReadResults);
BEGIN
    NullLookProc(p, ch, res);
END NullSkipLookProc;

PROCEDURE NullWriteLnProc(p : IOLink.DeviceTablePtr);
BEGIN
    SYSTEM.UNREFERENCED_PARAMETER(p);
END NullWriteLnProc;

PROCEDURE NullTextReadProc(p : IOLink.DeviceTablePtr;
                           adr : SYSTEM.ADDRESS;
                           c : CARDINAL;
                           VAR d : CARDINAL);
BEGIN
    SYSTEM.UNREFERENCED_PARAMETER(adr);
    SYSTEM.UNREFERENCED_PARAMETER(c);

    p^.result := IOConsts.endOfInput;
    d := 0;
END NullTextReadProc;

PROCEDURE NullTextWriteProc(p : IOLink.DeviceTablePtr;
                            adr : SYSTEM.ADDRESS;
                            c : CARDINAL);
BEGIN
    SYSTEM.UNREFERENCED_PARAMETER(p);
    SYSTEM.UNREFERENCED_PARAMETER(adr);
    SYSTEM.UNREFERENCED_PARAMETER(c);
END NullTextWriteProc;

PROCEDURE NullRawReadProc(p : IOLink.DeviceTablePtr;
                          adr : SYSTEM.ADDRESS;
                          c : CARDINAL;
                          VAR d : CARDINAL);
BEGIN
    SYSTEM.UNREFERENCED_PARAMETER(adr);
    SYSTEM.UNREFERENCED_PARAMETER(c);
    p^.result := IOConsts.endOfInput;
    d := 0;
END NullRawReadProc;

PROCEDURE NullRawWriteProc(p : IOLink.DeviceTablePtr;
                           adr : SYSTEM.ADDRESS;
                           c : CARDINAL);

BEGIN
    SYSTEM.UNREFERENCED_PARAMETER(p);
    SYSTEM.UNREFERENCED_PARAMETER(adr);
    SYSTEM.UNREFERENCED_PARAMETER(c);
END NullRawWriteProc;

PROCEDURE NullGetNameProc(p : IOLink.DeviceTablePtr;
                          VAR str : ARRAY OF CHAR);
BEGIN
    SYSTEM.UNREFERENCED_PARAMETER(p);
    str := "Null Channel";
END NullGetNameProc;

(*========================================*)

%IF AutoOpenStd %AND Windows %THEN

(* next section contains the procedures for non-console programs. *)
(* if a console call is made then a console will be automatically *)
(* opened at that time. *)
(* this is here as a convenience. doing this sidesteps some of the *)
(* operating system. *)

VAR
    ConsoleAllocated    : BOOLEAN = FALSE;

PROCEDURE DoOpen(p : IOLink.DeviceTablePtr);
BEGIN
    IF NOT ConsoleAllocated THEN
        ConsoleAllocated := AllocConsole();

        IF ChanConsts.readFlag IN p^.flags THEN
            ISOfiler.FakeOpen(p, ISOfiler.FileStdIn);
            ISOfiler.AssignProcs(p, TRUE, FALSE, p^.flags);
        ELSE
            ISOfiler.FakeOpen(p, ISOfiler.FileStdOut);
            ISOfiler.AssignProcs(p, TRUE, FALSE, p^.flags);
        END;
    END;
END DoOpen;

PROCEDURE AutoLookProc(p : IOLink.DeviceTablePtr;
                       VAR ch : CHAR;
                       VAR res : IOConsts.ReadResults);
BEGIN
    DoOpen(p);
    p^.doLookU(p, ch, res);
END AutoLookProc;

PROCEDURE AutoLookProcU(p : IOLink.DeviceTablePtr;
                       VAR ch : UCHAR;
                       VAR res : IOConsts.ReadResults);
BEGIN
    DoOpen(p);
    p^.doLookU(p, ch, res);
END AutoLookProcU;

PROCEDURE AutoSkipProc(p : IOLink.DeviceTablePtr);
BEGIN
    DoOpen(p);
    p^.doSkip(p);
END AutoSkipProc;

PROCEDURE AutoSkipLookProc(p : IOLink.DeviceTablePtr;
                           VAR ch : CHAR;
                           VAR res : IOConsts.ReadResults);
BEGIN
    DoOpen(p);
    p^.doSkipLook(p, ch, res);
END AutoSkipLookProc;

PROCEDURE AutoSkipLookProcU(p : IOLink.DeviceTablePtr;
                           VAR ch : UCHAR;
                           VAR res : IOConsts.ReadResults);
BEGIN
    DoOpen(p);
    p^.doSkipLookU(p, ch, res);
END AutoSkipLookProcU;

PROCEDURE AutoWriteLnProc(p : IOLink.DeviceTablePtr);
BEGIN
    DoOpen(p);
    p^.doLnWrite(p);
END AutoWriteLnProc;

PROCEDURE AutoWriteLnProcU(p : IOLink.DeviceTablePtr);
BEGIN
    DoOpen(p);
    p^.doLnWriteU(p);
END AutoWriteLnProcU;

PROCEDURE AutoTextReadProc(p : IOLink.DeviceTablePtr;
                           adr : SYSTEM.ADDRESS;
                           c : CARDINAL;
                           VAR d : CARDINAL);
BEGIN
    DoOpen(p);
    p^.doTextRead(p, adr, c, d);
END AutoTextReadProc;

PROCEDURE AutoTextReadProcU(p : IOLink.DeviceTablePtr;
                           adr : SYSTEM.ADDRESS;
                           c : CARDINAL;
                           VAR d : CARDINAL);
BEGIN
    DoOpen(p);
    p^.doTextReadU(p, adr, c, d);
END AutoTextReadProcU;

PROCEDURE AutoTextWriteProc(p : IOLink.DeviceTablePtr;
                            adr : SYSTEM.ADDRESS;
                            c : CARDINAL);
BEGIN
    DoOpen(p);
    p^.doTextWrite(p, adr, c);
END AutoTextWriteProc;

PROCEDURE AutoTextWriteProcU(p : IOLink.DeviceTablePtr;
                            adr : SYSTEM.ADDRESS;
                            c : CARDINAL);
BEGIN
    DoOpen(p);
    p^.doTextWriteU(p, adr, c);
END AutoTextWriteProcU;

PROCEDURE AutoRawReadProc(p : IOLink.DeviceTablePtr;
                          adr : SYSTEM.ADDRESS;
                          c : CARDINAL;
                          VAR d : CARDINAL);
BEGIN
    DoOpen(p);
    p^.doRawRead(p, adr, c, d);
END AutoRawReadProc;

PROCEDURE AutoRawWriteProc(p : IOLink.DeviceTablePtr;
                           adr : SYSTEM.ADDRESS;
                           c : CARDINAL);

BEGIN
    DoOpen(p);
    p^.doRawWrite(p, adr, c);
END AutoRawWriteProc;

PROCEDURE AutoGetNameProc(p : IOLink.DeviceTablePtr;
                          VAR str : ARRAY OF CHAR);
BEGIN
    SYSTEM.UNREFERENCED_PARAMETER(p);
    str := "Auto Channel";
END AutoGetNameProc;

PROCEDURE AutoGetNameProcU(p : IOLink.DeviceTablePtr;
                          VAR str : ARRAY OF UCHAR);
BEGIN
    SYSTEM.UNREFERENCED_PARAMETER(p);
    str := "Auto Channel";
END AutoGetNameProcU;

%END

PROCEDURE InitStd;
VAR
    p           : IOLink.DeviceTablePtr;
    invalid     : ChanId;
    %IF StdChanToTerminal %THEN
    res         : TermFile.OpenResults;
    %END
BEGIN
    invalid := IOChan.InvalidChan();

    IOLink.AllocateDeviceId(StdChanDid);

    IOLink.MakeChan(StdChanDid, TheNullChan);
    IF TheNullChan <> invalid THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, TheNullChan);
        (* reset, flush and free are all do nothings *)
        p^.doLook := NullLookProc;
        p^.doSkip := NullSkipProc;
        p^.doSkipLook := NullSkipLookProc;
        p^.doTextRead := NullTextReadProc;
        p^.doRawRead := NullRawReadProc;
        p^.doLnWrite := NullWriteLnProc;
        p^.doTextWrite := NullTextWriteProc;
        p^.doRawWrite := NullRawWriteProc;
        p^.doGetName := NullGetNameProc;
    END;

    %IF StdChanToTerminal %THEN
        TermFile.Open(StdInChanId,
                      TermFile.read+TermFile.raw+TermFile.text,
                      res);
        IF res <> TermFile.opened THEN
            StdInChanId := invalid;
        END;

        TermFile.Open(StdOutChanId,
                      TermFile.write+TermFile.raw+TermFile.text,
                      res);
        IF res <> TermFile.opened THEN
            StdOutChanId := invalid;
        END;

        StdErrChanId := StdOutChanId;
        (*
        TermFile.Open(StdErrChanId, write+raw+text, res);
        IF res <> TermFile.opened THEN
            StdErrChanId := invalid;
        END;
        *)
    %ELSE
        IF StdHandles.HaveStdInput() THEN
            IOLink.MakeChan(StdChanDid, StdInChanId);
            IF StdInChanId <> invalid THEN
                p := SYSTEM.CAST(IOLink.DeviceTablePtr, StdInChanId);

                p^.flags := ChanConsts.read+ChanConsts.text+ChanConsts.raw;

                ISOfiler.FakeOpen(p, ISOfiler.FileStdIn);
                ISOfiler.AssignProcs(p, TRUE, FALSE, p^.flags);
            END;
        ELSE
            %IF AutoOpenStd %AND Windows %THEN
                IOLink.MakeChan(StdChanDid, StdInChanId);
                IF StdInChanId <> invalid THEN
                    p := SYSTEM.CAST(IOLink.DeviceTablePtr, StdInChanId);
                    p^.flags := ChanConsts.read+ChanConsts.text+ChanConsts.raw;
                    p^.doLook := AutoLookProc;
                    p^.doLookU := AutoLookProcU;
                    p^.doSkip := AutoSkipProc;
                    p^.doSkipU := AutoSkipProcU;
                    p^.doSkipLook := AutoSkipLookProc;
                    p^.doSkipLookU := AutoSkipLookProcU;
                    p^.doTextRead := AutoTextReadProc;
                    p^.doTextReadU := AutoTextReadProcU;
                    p^.doRawRead := AutoRawReadProc;
                    p^.doLnWrite := AutoWriteLnProc;
                    p^.doLnWriteU := AutoWriteLnProcU;
                    p^.doTextWrite := AutoTextWriteProc;
                    p^.doTextWriteU := AutoTextWriteProcU;
                    p^.doRawWrite := AutoRawWriteProc;
                    p^.doGetName := AutoGetNameProc;
                    p^.doGetNameU := AutoGetNameProcU;
                END;
            %ELSE
                StdInChanId := TheNullChan;
            %END
        END;

        IF StdHandles.HaveStdOutput() THEN
            IOLink.MakeChan(StdChanDid, StdOutChanId);
            IF StdOutChanId <> invalid THEN
                p := SYSTEM.CAST(IOLink.DeviceTablePtr, StdOutChanId);

                p^.flags := ChanConsts.write+ChanConsts.text+ChanConsts.raw;

                ISOfiler.FakeOpen(p, ISOfiler.FileStdOut);
                ISOfiler.AssignProcs(p, TRUE, FALSE, p^.flags);
            END;
        ELSE
            %IF AutoOpenStd %AND Windows %THEN
                IOLink.MakeChan(StdChanDid, StdOutChanId);
                IF StdOutChanId <> invalid THEN
                    p := SYSTEM.CAST(IOLink.DeviceTablePtr, StdOutChanId);
                    p^.flags := ChanConsts.write+ChanConsts.text+ChanConsts.raw;
                    p^.doLook := AutoLookProc;
                    p^.doLookU := AutoLookProcU;
                    p^.doSkip := AutoSkipProc;
                    p^.doSkipU := AutoSkipProcU;
                    p^.doSkipLook := AutoSkipLookProc;
                    p^.doSkipLookU := AutoSkipLookProcU;
                    p^.doTextRead := AutoTextReadProc;
                    p^.doTextReadU := AutoTextReadProcU;
                    p^.doRawRead := AutoRawReadProc;
                    p^.doLnWrite := AutoWriteLnProc;
                    p^.doLnWriteU := AutoWriteLnProcU;
                    p^.doTextWrite := AutoTextWriteProc;
                    p^.doTextWriteU := AutoTextWriteProcU;
                    p^.doRawWrite := AutoRawWriteProc;
                    p^.doGetName := AutoGetNameProc;
                    p^.doGetNameU := AutoGetNameProcU;
                END;
            %ELSE
                StdOutChanId := TheNullChan;
            %END
        END;

        IF StdHandles.HaveStdError() THEN
            IOLink.MakeChan(StdChanDid, StdErrChanId);
            IF StdErrChanId <> invalid THEN
                p := SYSTEM.CAST(IOLink.DeviceTablePtr, StdErrChanId);

                p^.flags := ChanConsts.write+ChanConsts.text+ChanConsts.raw;

                ISOfiler.FakeOpen(p, ISOfiler.FileStdError);
                ISOfiler.AssignProcs(p, TRUE, FALSE, p^.flags);
            END;
        ELSE
            StdErrChanId := TheNullChan;
        END;

    %END

    (* set the current default channels to the standard channels *)

    CurInChan := StdInChanId;
    CurOutChan := StdOutChanId;
    CurErrChan := StdErrChanId;
END InitStd;

PROCEDURE TermStd;
VAR
    invalid     : ChanId;
BEGIN
    (* flush these guys because they will be buffered if redirected *)

    IOChan.Flush(StdOutChanId);
    IOChan.Flush(StdErrChanId);

    invalid := IOChan.InvalidChan();

    %IF StdChanToTerminal %THEN
        IF StdOutChanId <> invalid THEN
            TermFile.Close(StdOutChanId);
        END;
        IF StdInChanId <> invalid THEN
            TermFile.Close(StdInChanId);
        END;
    %ELSE
        IF (StdErrChanId <> StdOutChanId) AND
           (StdErrChanId <> TheNullChan) AND
           (StdErrChanId <> invalid)
        THEN
            IOLink.UnMakeChan(StdChanDid, StdErrChanId);
        END;
        IF (StdOutChanId <> TheNullChan) AND (StdOutChanId <> invalid) THEN
            IOLink.UnMakeChan(StdChanDid, StdOutChanId);
        END;
        IF (StdInChanId <> TheNullChan) AND (StdInChanId <> invalid) THEN
            IOLink.UnMakeChan(StdChanDid, StdInChanId);
        END;

        IF TheNullChan <> invalid THEN
            IOLink.UnMakeChan(StdChanDid, TheNullChan);
        END;
    %END
END TermStd;

BEGIN
    (* in case this module is part of a WIN32 DLL, that inits per thread *)

    IF NOT SYSTEM.IsThread THEN
        InitStd;
    END;

FINALLY
    IF NOT SYSTEM.IsThread THEN
        TermStd;
    END;
END StdChans.
