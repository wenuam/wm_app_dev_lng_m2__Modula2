IMPLEMENTATION MODULE TermFile;

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

  (* Access to the terminal device *)

  (* Channels opened by this module are connected to a single terminal
     device; typed characters are distributed between channels according
     to the sequence of read requests. *)

IMPORT IOConsts, IOChan, ChanConsts, IOLink, SYSTEM, CharBuffer, Terminal;

(* TYPE
  ChanId = IOChan.ChanId;
  FlagSet = ChanConsts.FlagSet;
  OpenResults = ChanConsts.OpenResults;


  (* Accepted singleton values of FlagSet *)

CONST
  read = FlagSet{ChanConsts.readFlag};   (* input operations are
requested/available *)
  write = FlagSet{ChanConsts.writeFlag}; (* output operations are
requested/available *)
  text = FlagSet{ChanConsts.textFlag};   (* text operations are
requested/available *)
  raw = FlagSet{ChanConsts.rawFlag};     (* raw operations are
requested/available *)
  echo = FlagSet{ChanConsts.echoFlag};   (* echoing by interactive device on
reading of
                                            characters from input stream
requested/applies
                                         *)
*)

CONST
    EolChar             = CHR(13);

VAR
    termFiledid                 : IOLink.DeviceId;
    buf                         : CharBuffer.Buffer;
    chansInSingleCharMode       : CARDINAL;

(* *************************** *)
(* Here are the Terminal procedures *)

PROCEDURE CollectInBuf;
(* modified by Stony Brook *)
VAR
    chr : CHAR;
BEGIN
    REPEAT
        IF NOT CharBuffer.Full(buf) THEN
            Terminal.Read(chr);
            IF chr <> '' THEN
                IF chr = CHR(8) THEN (* backspace *)
                    CharBuffer.Erase(buf)
                ELSE
                    CharBuffer.Enter(buf,chr);
                END;

                IF (* (chr <> EolChar) AND *) (chansInSingleCharMode = 0)
                (* decision: echo the cr when read *)
                THEN
                    Terminal.Write (chr);
                END;
            END;
        ELSE
            RETURN;
        END;
    UNTIL chr = EolChar;
END CollectInBuf;

PROCEDURE TermLookProc(p : IOLink.DeviceTablePtr;
                       VAR ch : CHAR;
                       VAR res : IOConsts.ReadResults);
(* modified by Stony Brook *)
VAR
    chr : CHAR;
BEGIN
    IF CharBuffer.Empty(buf) THEN
        IF ChanConsts.echo <= p^.flags THEN
            Terminal.Read(chr);
            IF chr <> '' THEN
                Terminal.Write(chr); (* echo *)
                CharBuffer.Enter(buf, chr);
            END;
        ELSE (* line mode and echo is there as they are collected *)
            CollectInBuf;
        END;
    END;

    IF NOT CharBuffer.Empty(buf) THEN
        CharBuffer.Look(buf, ch);
        IF ch = EolChar THEN
            res := IOConsts.endOfLine;
        ELSE
            res := IOConsts.allRight;
        END;
    ELSE
        res := IOConsts.endOfInput;
    END;
    p^.result := res;
END TermLookProc;

PROCEDURE TermSkipProc(p : IOLink.DeviceTablePtr);
VAR
    chr : CHAR;
BEGIN
    IF NOT CharBuffer.Empty(buf) THEN
        CharBuffer.Skip(buf);
    ELSE
        Terminal.Read(chr);
        IF chr = '' THEN
            p^.result := IOConsts.endOfInput;
            IOLink.RAISEdevException(p^.cid,
                                     p^.did,
                                     IOChan.skipAtEnd,
                                     "Skip attempted at end of Input");
        END;
    END;
    p^.result := IOConsts.allRight;  (* if got this far *)
END TermSkipProc;

PROCEDURE TermSkipLookProc(p : IOLink.DeviceTablePtr;
                           VAR ch : CHAR;
                           VAR res : IOConsts.ReadResults);
BEGIN
    TermSkipProc(p);
    TermLookProc(p, ch, res);
END TermSkipLookProc;

PROCEDURE TermWriteLnProc(p : IOLink.DeviceTablePtr);
BEGIN
    Terminal.WriteLn;
END TermWriteLnProc;

PROCEDURE TermTextReadProc(p : IOLink.DeviceTablePtr;
                           adr : SYSTEM.ADDRESS;
                           c : CARDINAL;
                           VAR d : CARDINAL);
(* modified by Stony Brook *)
VAR
    ch  : CHAR;
    ptr : POINTER TO CHAR;
    res : IOConsts.ReadResults;
BEGIN
    d := 0;
    ptr := adr;
    TermLookProc (p, ch, res);
    WHILE (c > 0) AND (res = IOConsts.allRight) DO
        (*
        IF ChanConsts.echo <= p^.flags THEN
            (* single character mode do echo here *)
            Terminal.Write (ch);
        END;
        *)
        DEC(c);
        INC(d);
        ptr^ := ch;
        ptr := SYSTEM.ADDADR(ptr, SIZE(CHAR));

        IF c > 0 THEN
            TermSkipLookProc(p, ch, res);
        ELSE
            TermSkipProc(p);
        END;
    END;
END TermTextReadProc;

PROCEDURE TermTextWriteProc(p : IOLink.DeviceTablePtr;
                            adr : SYSTEM.ADDRESS;
                            c : CARDINAL);
(* Modified by Stony Brook *)
VAR
    ch  : CHAR;
    ptr : POINTER TO ARRAY [0..0] OF CHAR;
BEGIN
    IF c <> 0 THEN
        ptr := adr;
        Terminal.WriteString(ptr^[0..c-1]);
        (*
        WHILE c > 0 DO
            ch := ptr^;
            Terminal.Write(ch);
            ptr := SYSTEM.ADDADR(ptr, SIZE(CHAR));
            DEC(c);
        END;
        *)
    END;
END TermTextWriteProc;

PROCEDURE TermRawReadProc(p : IOLink.DeviceTablePtr;
                          adr : SYSTEM.ADDRESS;
                          c : CARDINAL;
                          VAR d : CARDINAL);
BEGIN
    TermTextReadProc(p, adr, c, d)  (* raw == text *)
END TermRawReadProc;

PROCEDURE TermRawWriteProc(p : IOLink.DeviceTablePtr;
                           adr : SYSTEM.ADDRESS;
                           c : CARDINAL);

BEGIN
    TermTextWriteProc(p, adr, c)  (* raw == text *)
END TermRawWriteProc;

PROCEDURE TermGetNameProc(p : IOLink.DeviceTablePtr;
                          VAR str : ARRAY OF CHAR);
BEGIN
    str := "Terminal";
END TermGetNameProc;

PROCEDURE TermResetProc(p : IOLink.DeviceTablePtr);
BEGIN
    Terminal.Reset;
END TermResetProc;

(* *************************** *)

PROCEDURE WrongDeviceEx;
BEGIN
    (* force a wrong device exception *)

    IOLink.RAISEdevException(IOChan.InvalidChan(),
                             termFiledid,
                             IOChan.notAvailable,
                             "Not a TermFile device");
END WrongDeviceEx;

PROCEDURE Open(VAR cid : ChanId;
               flags : FlagSet;
               VAR res : OpenResults);
  (* Attempts to obtain and open a channel connected to the terminal.
     Without the raw flag, text is implied.
     Without the echo flag, line mode is requested, otherwise single
     character mode is requested.
     If successful, assigns to cid the identity of the opened channel, and
     assigns the value opened to res.
     If a channel cannot be opened as required, the value of res indicates
     the reason, and cid identifies the invalid channel. *)
VAR
    p : IOLink.DeviceTablePtr;
BEGIN
    IOLink.MakeChan(termFiledid, cid);

    IF cid <> IOChan.InvalidChan() THEN
        res := opened;
        p := SYSTEM.CAST (IOLink.DeviceTablePtr, cid);
        p^.flags := flags;

        IF ChanConsts.echo <= p^.flags THEN
            INC(chansInSingleCharMode);
        END;

        IF ChanConsts.read <= flags THEN
            p^.doLook := TermLookProc;
            p^.doSkip := TermSkipProc;
            p^.doSkipLook := TermSkipLookProc;

            IF (ChanConsts.text <= flags) OR
                (NOT (ChanConsts.raw <= flags))
            THEN
                p^.doTextRead := TermTextReadProc;
            END;

            IF ChanConsts.raw <= flags THEN
                p^.doRawRead := TermRawReadProc;
            END;
        END;

        IF ChanConsts.write <= flags THEN
            p^.doLnWrite := TermWriteLnProc;

            IF (ChanConsts.text <= flags) OR
               (NOT (ChanConsts.raw <= flags))
            THEN
                p^.doTextWrite := TermTextWriteProc;
            END;

            IF ChanConsts.raw <= flags THEN
                p^.doRawWrite := TermRawWriteProc;
            END;
        END;

        p^.doGetName := TermGetNameProc;
        p^.doReset := TermResetProc;

        (* reset, flush and free are all do nothings *)
    ELSE
        res := outOfChans;
    END;
END Open;

PROCEDURE IsTermFile(cid : ChanId) : BOOLEAN;
  (* Tests if the channel identified by cid is open to the terminal. *)
BEGIN
    RETURN IOLink.IsDevice(cid, termFiledid);
END IsTermFile;


PROCEDURE Close(VAR cid : ChanId);
  (* If the channel identified by cid is not open to the terminal,
     the exception wrongDevice is raised; otherwise closes the channel
     and assigns the value identifying the invalid channel to cid. *)
VAR
    p : IOLink.DeviceTablePtr;
BEGIN
    IF IsTermFile(cid) THEN
        p := SYSTEM.CAST(IOLink.DeviceTablePtr, cid);

        IF ChanConsts.echo <= p^.flags THEN
            DEC(chansInSingleCharMode);
        END;

        IOLink.UnMakeChan(termFiledid, cid);
        cid := IOChan.InvalidChan();
    ELSE
        WrongDeviceEx;
    END
END Close;

PROCEDURE Init;
BEGIN
    IOLink.AllocateDeviceId(termFiledid);
    CharBuffer.Init(buf);
    chansInSingleCharMode := 0;
END Init;

PROCEDURE Term;
BEGIN
    CharBuffer.Destroy(buf);
END Term;

BEGIN
    (* in case this module is part of a WIN32 DLL, that inits per thread *)

    IF NOT SYSTEM.IsThread THEN
        Init;
    END;

FINALLY
    IF NOT SYSTEM.IsThread THEN
        Term;
    END;
END TermFile.
