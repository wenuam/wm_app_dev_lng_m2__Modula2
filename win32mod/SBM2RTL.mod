(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 1999-2001                          *)
(*                      by Stony Brook Software                            *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
MODULE sbm2rtl;
<*/DLL*>

(* these modules maintain a state in globals and therefore their *)
(* behavior will change when linked via DLL. *)

IMPORT ConfigSettings, ElapsedTime, ExStorage, FormatDT, IOLink,
       ProgramArgs, RandomNumbers,
       RealRandomNumbers, Registry, StdChans, Terminal, Threads,
       WinShell, DlgShell, BasicDialogs, TextWindows,
       VLI(*prime and random APIs*);

(* these modules do not maintain a state, however they use modules that *)
(* do maintain a state and therefore change behavior indirectly. *)

IMPORT SIOResult, SLongIO, SLWholeIO, SRawIO, SRealIO, STextIO, SWholeIO, TermFile,
       RndFile, SeqFile, StreamFile;

(* these modules do not maintain a state and therefore do not change *)
(* behavior. *)

IMPORT BitVectors,
       CharClass, Conversions,
       Environment, ExStrings, FileFunc, FileMap, FormatString,
       GenCRC, IOChan, IOResult,
       LongConv, LongIO, LongStr, LowLong, LowReal, LWholeIO,
       LWholeStr, MemShare, MemUtils, NamedPipes, PipedExec, Pipes,
       RawIO, RConversions, RealConv, RealIO, RealStr, RunProg,
       SortLib, StdHandles, Strings, Socket, SysClock,
       TextFileFunc, TextIO, TimeFunc,
       WholeConv, WholeIO, WholeStr,
       MD5, SHA1, SHA256, SHA384, SHA512,
       AES, DES, Blowfish, AreSee4, RSA;

FROM SYSTEM IMPORT
    CAST;
FROM WIN32 IMPORT
    GetModuleHandle, HINSTANCE;
FROM WINX IMPORT
    Instance, NIL_STR;

EXPORTS MODULE ConfigSettings,
        MODULE ElapsedTime,
        MODULE ExStorage,
        MODULE FormatDT,
        MODULE IOLink,
        MODULE ProgramArgs,
        MODULE RandomNumbers,
        MODULE RealRandomNumbers,
        MODULE Registry,
        MODULE StdChans,
        MODULE Terminal,
        MODULE Threads,
        MODULE WinShell,
        MODULE DlgShell,
        MODULE BasicDialogs,
        MODULE TextWindows,
        MODULE VLI;

EXPORTS MODULE SIOResult,
        MODULE SLongIO,
        MODULE SLWholeIO,
        MODULE SRawIO,
        MODULE SRealIO,
        MODULE STextIO,
        MODULE SWholeIO,
        MODULE TermFile,
        MODULE RndFile,
        MODULE SeqFile,
        MODULE StreamFile;

EXPORTS MODULE BitVectors,
        MODULE CharClass,
        MODULE Conversions,
        MODULE Environment,
        MODULE ExStrings,
        MODULE FileFunc,
        MODULE FileMap,
        MODULE FormatString,
        MODULE GenCRC,
        MODULE IOChan,
        MODULE IOResult,
        MODULE LongConv,
        MODULE LongIO,
        (*MODULE LongMath,*)
        MODULE LongStr,
        MODULE LowLong,
        MODULE LowReal,
        MODULE LWholeIO,
        MODULE LWholeStr,
        MODULE MemShare,
        MODULE MemUtils,
        MODULE NamedPipes,
        MODULE PipedExec,
        MODULE Pipes,
        MODULE RawIO,
        MODULE RConversions,
        MODULE RealConv,
        MODULE RealIO,
        (*MODULE RealMath,*)
        MODULE RealStr,
        MODULE RunProg,
        MODULE SortLib,
        MODULE StdHandles,
        MODULE Strings,
        MODULE Socket,
        MODULE SysClock,
        MODULE TextFileFunc,
        MODULE TextIO,
        MODULE TimeFunc,
        MODULE WholeConv,
        MODULE WholeIO,
        MODULE WholeStr,
        MODULE MD5,
        MODULE SHA1,
        MODULE SHA256,
        MODULE SHA384,
        MODULE SHA512,
        MODULE AES,
        MODULE DES,
        MODULE Blowfish,
        MODULE AreSee4,
        MODULE RSA;

BEGIN
    (* we will have this DLL impersonate the main process *)
    (* while none of the RTL currently cares about this handle, *)
    (* this impersonation will make the rtl modules act the same if linked *)
    (* statically or not *)

    Instance := CAST(HINSTANCE, GetModuleHandle(NIL_STR));
    RETURN TRUE;
END sbm2rtl.
