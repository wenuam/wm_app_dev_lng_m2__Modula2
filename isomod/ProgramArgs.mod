IMPLEMENTATION MODULE ProgramArgs;

(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman

    Stony Brook compiler port Implementation
        copyright (c) 1994-2004
        by ADW Software
=========================================== *)

FROM SYSTEM IMPORT
    ADDRESS, ADDADR, CAST, UNREFERENCED_PARAMETER, IsThread;

FROM Strings IMPORT
    Assign;

FROM Environment IMPORT
    GetCommandLine;

FROM CharClass IMPORT
    IsWhiteSpace;

IMPORT IOLink, IOChan, IOConsts, ChanConsts;

VAR
    CommandLine : ARRAY [0..2*1024] OF CHAR;
    Bp          : CARDINAL;
    Be          : CARDINAL;

    argDid      : IOLink.DeviceId;
    argChanId   : ChanId;

PROCEDURE ArgChan() : ChanId;
(* returns the value that identifies a channel for reading program arguments *)
BEGIN
    RETURN argChanId;
END ArgChan;

PROCEDURE SkipSpace;
BEGIN
    WHILE (Bp < Be) AND IsWhiteSpace(CommandLine[Bp]) DO
        INC(Bp);
    END;
END SkipSpace;

 (* the procedures for I/O *)

PROCEDURE ArgLookProc(p : IOLink.DeviceTablePtr;
                      VAR ch : CHAR;
                      VAR res : IOConsts.ReadResults);
BEGIN
    IF ChanConsts.read <= p^.flags THEN
        IF Bp < Be THEN
            ch := CommandLine[Bp];
            res := IOConsts.allRight;
        ELSE
            res := IOConsts.endOfInput
        END;
    ELSE
        res := IOConsts.endOfInput;
        ch := '';
    END;
    p^.result := res;
END ArgLookProc;

PROCEDURE ArgSkipProc(p : IOLink.DeviceTablePtr);
BEGIN
    IF ChanConsts.read <= p^.flags THEN
        IF Bp < Be THEN
            INC(Bp);
        ELSE
            p^.result := IOConsts.endOfInput;
            IOLink.RAISEdevException(p^.cid,
                                     p^.did,
                                     IOChan.skipAtEnd,
                                     "Skip attempted at end of Input");
        END;
    ELSE
        p^.result := IOConsts.endOfInput;
    END;
END ArgSkipProc;

PROCEDURE ArgSkipLookProc(p : IOLink.DeviceTablePtr;
                          VAR ch : CHAR;
                          VAR res : IOConsts.ReadResults);
BEGIN
    ArgSkipProc(p);
    ArgLookProc(p, ch, res);
END ArgSkipLookProc;

PROCEDURE ArgTextReadProc(p : IOLink.DeviceTablePtr;
                          adr : ADDRESS;
                          c : CARDINAL;
                          VAR d : CARDINAL);

VAR
    ch          : CHAR;
    ptr         : POINTER TO CHAR;
    res         : IOConsts.ReadResults;
BEGIN
    ptr := adr;
    d := 0;
    ArgLookProc(p, ch, res);
    WHILE (c > 0) AND (res = IOConsts.allRight) DO
        DEC(c);
        INC(d);
        ptr^ := ch;
        ptr := ADDADR(ptr, SIZE(CHAR));
        ArgSkipLookProc(p, ch, res);
    END;
END ArgTextReadProc;

PROCEDURE ArgResetProc(p : IOLink.DeviceTablePtr);
BEGIN
    UNREFERENCED_PARAMETER(p);
    Bp := 0;
END ArgResetProc;

PROCEDURE ArgGetNameProc(p : IOLink.DeviceTablePtr; VAR str: ARRAY OF CHAR);
BEGIN
    UNREFERENCED_PARAMETER(p);
    Assign("ProgramArguments", str);
END ArgGetNameProc;

PROCEDURE InitCommandLine;
BEGIN
    GetCommandLine(CommandLine);
    Bp := 0;
    Be := LENGTH(CommandLine);
    SkipSpace;
END InitCommandLine;

PROCEDURE IsArgPresent() : BOOLEAN;
(* tests if there is a current argument to read from; If not,
   read <= IOChan.CurrentFlags() will be FALSE, and attempting to read
   from the argument channel will raise the exception notAvailable
*)

VAR
    p   : IOLink.DeviceTablePtr;
BEGIN
    IF Bp = MAX(Bp) THEN
        InitCommandLine;
    END;
    SkipSpace;

    p := CAST(IOLink.DeviceTablePtr, argChanId);
    IF Bp < Be THEN
        p^.flags := ChanConsts.read;
    ELSE
        p^.flags := ChanConsts.FlagSet{};
    END;

    RETURN Bp < Be;
END IsArgPresent;

PROCEDURE NextArg();
(* if there is another argument, causes subsequent input from the
   argument device to come from the start of the next argument. Otherwise
   there is no argument to read from, and a call to IsArgPresent will
   return FALSE
*)

VAR
    p   : IOLink.DeviceTablePtr;
BEGIN
    IF Bp = MAX(Bp) THEN
        InitCommandLine;
    END;
    SkipSpace;

    p := CAST(IOLink.DeviceTablePtr, argChanId);
    IF Bp < Be THEN
        p^.flags := ChanConsts.read;
    ELSE
        p^.flags := ChanConsts.FlagSet{};
    END;
END NextArg;

PROCEDURE Init;
VAR
    p   : IOLink.DeviceTablePtr;
BEGIN
    Bp := MAX(Bp);

    IOLink.AllocateDeviceId(argDid);

    IOLink.MakeChan(argDid, argChanId);
    IF argChanId <> IOChan.InvalidChan() THEN
        p := CAST(IOLink.DeviceTablePtr, argChanId);
        IF Bp < Be THEN
            p^.flags := ChanConsts.read;
        ELSE
            p^.flags := ChanConsts.FlagSet{};
        END;
        p^.cd := NIL;
        p^.doReset := ArgResetProc;
        p^.doGetName := ArgGetNameProc;

        p^.doLook := ArgLookProc;
        p^.doSkip := ArgSkipProc;
        p^.doSkipLook := ArgSkipLookProc;
        p^.doTextRead := ArgTextReadProc;
    END;
END Init;

PROCEDURE Term;
BEGIN
    IF argChanId <> IOChan.InvalidChan() THEN
        IOLink.UnMakeChan(argDid, argChanId);
    END;
END Term;

BEGIN
    (* in case this module is part of a WIN32 DLL, that inits per thread *)

    IF NOT IsThread THEN
        Init;
    END;

FINALLY
    IF NOT IsThread THEN
        Term;
    END;
END ProgramArgs.
