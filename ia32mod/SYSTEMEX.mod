(***************************************************************************)
(*                                                                         *)
(*                       Copyright (C) 2009                                *)
(*                         by ADW Software                                 *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE SYSTEMEX;
<*/NOOPT:X*>
<*/NOWARN:F*>

<*/VALIDVER:Linux*>
<*/VALIDVER:DebugStr*>

(*<*/VER:DebugStr*>*)

FROM SYSTEM IMPORT
    %IF UNIX %THEN
    DLL,
    %END
    %IF Linux %THEN
    BYTE,
    %END
    %IF PICCode %THEN
    OFFS,
    %END
    ADR, ADRCARD, ADRINT, ADDRESS, ADDADR, SUBADR, MAKEADR, CAST, SYSHALT, UNREFERENCED_PARAMETER;

%IF %NOT (Windows %OR UNIX) %THEN
IMPORT COROUTINES;
%END

%IF Windows %THEN

FROM WIN32 IMPORT
    %IF Bits64 %THEN
    RtlLookupFunctionEntry, RtlVirtualUnwind, RtlRestoreContext, RtlCaptureContext,
    UNWIND_HISTORY_TABLE, PRUNTIME_FUNCTION, UNW_FLAG_NHANDLER, UNW_FLAG_EHANDLER,
    UNWIND_INFO, PUNWIND_INFO, UNWIND_CODE,
    %END
    %IF DebugStr %THEN
    GetStdHandle, STD_OUTPUT_HANDLE,
    %END
    EXCEPTION_DISPOSITION, EXCEPTION_NONCONTINUABLE,
    EXCEPTION_UNWIND, EXCEPTION_NESTED_CALL, EXCEPTION_STACK_INVALID,
    EXCEPTION_ACCESS_VIOLATION, EXCEPTION_PRIV_INSTRUCTION,
    EXCEPTION_INT_DIVIDE_BY_ZERO, EXCEPTION_FLT_DIVIDE_BY_ZERO,
    EXCEPTION_FLT_DENORMAL_OPERAND, EXCEPTION_FLT_OVERFLOW,
    EXCEPTION_FLT_UNDERFLOW, EXCEPTION_FLT_INEXACT_RESULT,
    EXCEPTION_STACK_OVERFLOW, STATUS_NO_MEMORY,
    PEXCEPTION_RECORD, PCONTEXT, PDISPATCHER_CONTEXT,
    RaiseException, ULONG_PTR,
    EXCEPTION_POINTERS, UnhandledExceptionFilter,
    MEMORY_BASIC_INFORMATION, VirtualQuery, MEM_COMMIT, PAGE_GUARD,
    GetProcessHeap, HeapAlloc, HeapFree, HeapReAlloc,
    GetCurrentThreadId,
    CRITICAL_SECTION, InitializeCriticalSection, DeleteCriticalSection,
    EnterCriticalSection, LeaveCriticalSection,
    CloseHandle, HANDLE, GetCurrentProcessId,
    SuspendThread, ResumeThread, OpenThread,
    CONTEXT, CONTEXT_FULL, GetThreadContext, THREAD_ALL_ACCESS;

FROM TOOLHELP32 IMPORT
    PROCESSENTRY32, CreateToolhelp32Snapshot, Process32First, Process32Next, THREADENTRY32, Thread32First,
    Thread32Next, TH32CS_SNAPTHREAD, TH32CS_SNAPPROCESS, TH32CS_SNAPMODULE,
    MODULEENTRY32, Module32First, Module32Next;

FROM WINUSER IMPORT
    MessageBox, MB_OK, MB_ICONSTOP, MB_SETFOREGROUND, MB_TASKMODAL;

FROM WINX IMPORT
    NULL_HWND;

%ELSIF UNIX %THEN

FROM UNIX IMPORT
    write, STDERR_FILENO,
    SIGSEGV, SIGBUS, SIGILL, SIGFPE,
    SA_SIGINFO, SIG_UNBLOCK,
    sigaction_t, siginfo_t, sigset_t, ucontext_t, gregids_t, gregset_t,
    sigaction, sigemptyset, sigaddset, malloc, realloc, free;

%IF MultiThreadLib %THEN
FROM PTHREADS IMPORT
    pthread_self,
    pthread_mutex_t, pthread_mutexattr_t, PTHREAD_MUTEX_RECURSIVE,
    pthread_mutexattr_init, pthread_mutexattr_settype,
    pthread_mutexattr_destroy,
    pthread_mutex_init, pthread_mutex_destroy, pthread_mutex_lock, pthread_mutex_unlock,
    pthread_sigmask;
%ELSE
FROM UNIX IMPORT
    sigprocmask;
%END

%END

FROM MemUtils IMPORT
    ZeroMem;

FROM FileFunc IMPORT
    %IF UNIX %THEN
    OpenFileEx, AccessModes, ReadBlock, FileUseInfo, FileUseInfoSet, SetFilePos,
    %END
    %IF DebugStr %THEN
    FakeFileOpen,
        %IF %NOT UNIX %THEN
        AccessModes,
        %END
    %END
    File, FileSpecString, CreateFile, WriteBlock, CloseFile, FileNameParts, ParseFileName, AssembleParts;

FROM Environment IMPORT
    GetProgramName;

%IF Windows %OR Linux %THEN
FROM Strings IMPORT
    %IF Linux %AND (%NOT Bits64) %THEN
    Delete,
    %END
    Equal;
%END

%IF Linux %THEN
FROM Conversions IMPORT
    %IF Bits32 %THEN
    StrBaseToCard;
    %ELSE
    StrBaseToLong;
    %END
%END

CONST
    NoSource    = CAST(ExceptionSource, 0);

%IF Windows %THEN
    %IF Bits32 %THEN
        ADWExceptionSignature  = 12348765h;
    %END
%ELSE
    ADWExceptionSignature  = 0;
%END

TYPE
    <*/PUSH/PACK/NOWARN:A*>
    StatePtr    = POINTER TO State;
    State       =
        RECORD
			src     : ExceptionSource;
			addr    : ADDRESS;
			checking: ADDRESS;
			num     : ExceptionNumber;
			mess    : ARRAY [0..255] OF CHAR;
        END;

    FramePtr    = POINTER TO Frame;
    Frame =
        RECORD
			%IF Windows %AND Bits32 %THEN
				prevFrame   : FramePtr;
				filterProc  : ADDRESS;
				signature   : ADRCARD;
				handler     : ADDRESS;
				state       : StatePtr;
				curBP       : ADRCARD;
			%ELSIF Windows %AND Bits64 %THEN
				handler     : ADDRESS;
				state       : StatePtr;
			%ELSE
				oldBP       : ADDRESS;
				signature   : ADRCARD; (* 0 *)
				handler     : ADDRESS;
				state       : StatePtr;
				curSP       : ADDRESS;
			%END
        END;
    <*/POP*>

    %IF Windows %AND Bits64 %THEN
    tPtrC64             = POINTER TO CARDINAL64;

    SCOPE_TABLE_REC_M2 =
        RECORD
            beginAddress        : CARDINAL32;
            endAddress          : CARDINAL32;
            jumpTarget          : CARDINAL32;
            stateOffset         : INTEGER32;
        END;

    SCOPE_TABLE_SIG_M2 =
        RECORD
			CASE : BOOLEAN OF
			TRUE:
				str     : ARRAY [0..7] OF ACHAR;
			|
			FALSE:
				dq      : CARDINAL64;
			END;
        END;

    SCOPE_TABLE_M2 =
		RECORD
			sig             : SCOPE_TABLE_SIG_M2;
			count           : CARDINAL32;(* always high bit set *)
			table           : ARRAY [0..0] OF SCOPE_TABLE_REC_M2;(*variable length *)
		END;
    PSCOPE_TABLE_M2     = POINTER TO SCOPE_TABLE_M2;
    %END

    %IF Bits32 %THEN
        IntRegisters = (SegGs, SegFs, SegEs, SegDs,
                        rEdi, rEsi, rEbx, rEdx, rEcx, rEax, rEbp,
                        rEip, SegCs, EFlags, rEsp, SegSs);

        tThreadInfo = RECORD
            ThreadId        : CARDINAL32;
            IntegerRegisters: ARRAY IntRegisters OF CARDINAL32;
    END;
    %ELSIF Bits64 %THEN
        IntRegisters = (rR8, rR9, rR10, rR11, rR12, rR13, rR14, rR15,
                        rRdi, rRsi, rRbx, rRdx, rRcx, rRax, rRbp,
                        rRip, EFlags, rRsp);

        tThreadInfo = RECORD
            ThreadId        : CARDINAL64;
            IntegerRegisters: ARRAY IntRegisters OF CARDINAL64;
        END;
    %END

    tPmdMemoryInfoItem = RECORD
        BlockStartAddress       : POINTER TO ARRAY[0..0] OF CHAR;
        BlockSize               : ADRCARD;
        Attributes              : ADRCARD; (* need to create something *)
        DiskLoc                 : ADRCARD;
    END;

VAR
    UserUnhandledProc           : PROC;

    PMD                         : PROCEDURE(BOOLEAN);
    PMDEnable                   : BOOLEAN;
    SuppressRegisterSave        : BOOLEAN;
    DebuggerAttach              : AttachDebuggerOpt;
    PMDFileWriteLocation        : ADRCARD;
    GlobalThreadInfo            : tThreadInfo;

    SafeSavedSP                 : ADRCARD;
    PMDVersionStr               : ARRAY [0..63] OF CHAR;
%IF Windows %THEN
    PMDSem                      : CRITICAL_SECTION;
%ELSIF UNIX %THEN
    %IF MultiThreadLib %THEN
    PMDSem                      : pthread_mutex_t;
    %END
    Old_SIGSEGV,
    Old_SIGBUS,
    Old_SIGILL,
    Old_SIGFPE                  : sigaction_t;
    AccessViolationsTrapped     : BOOLEAN;
%END


%IF Windows %THEN
CONST
    OurWindowsExceptionCode     = 0E0123456h;
    NumExceptParams             = 6;
%IF Bits64 %THEN
    ADWSig = SCOPE_TABLE_SIG_M2 {TRUE, "ADW Soft"};
%END
%END

%IF Linux %THEN
%IF Bits32 %THEN
    PROCEDURE StrBaseToNum = StrBaseToCard;
%ELSIF Bits64 %THEN
    PROCEDURE StrBaseToNum = StrBaseToLong;
%END
%END

%IF DebugStr %THEN
PROCEDURE Print(str : ARRAY OF ACHAR; eol : BOOLEAN);
CONST
    CrLf : ARRAY [0..1] OF ACHAR = {ACHR(13), ACHR(10)};
VAR
    len         : CARDINAL;
    f           : File;
BEGIN
    len := 0;
    WHILE (len <= HIGH(str)) AND (str[len] <> '') DO
        INC(len);
    END;

    FakeFileOpen(f, CAST(ADRCARD, GetStdHandle(STD_OUTPUT_HANDLE)), WriteOnlyDenyAll);
    WriteBlock(f, ADR(str), len);
    IF eol THEN
        WriteBlock(f, ADR(CrLf), SIZE(CrLf));
    END;
END Print;

PROCEDURE PrintHex(label : ARRAY OF CHAR; num : ADRCARD; s : CARDINAL);
VAR
    str         : ARRAY [0..31] OF CHAR;

    PROCEDURE convertHex(num : ADRCARD; s : ADRCARD; VAR OUT str : ARRAY OF ACHAR);
    CONST
        HexDig      : ARRAY [0..15] OF ACHAR = {"0123456789ABCDEF"};
    VAR
        i   : ADRCARD;
    BEGIN
        FOR i := s-1 TO 0 BY -1 DO
            str[i] := HexDig[num REM 16];
            num := num / 16;
        END;
        str[s] := '';
    END convertHex;
BEGIN
    convertHex(num, s, str);
    Print(label, FALSE);
    Print(str, TRUE);
END PrintHex;

PROCEDURE PrintCard(label : ARRAY OF CHAR; num : ADRCARD);
VAR
    str         : ARRAY [0..31] OF CHAR;
BEGIN
    ConvertDec(num, str);
    Print(label, FALSE);
    Print(str, TRUE);
END PrintCard;
%END

%IF UNIX %THEN

PROCEDURE ErrorWrite(str : ARRAY OF ACHAR);
VAR
    len         : ADRCARD;
    highStr     : ADRCARD;
BEGIN
    len := 0;
    highStr := HIGH(str);
    WHILE (len <= highStr) AND (str[len] <> '') DO
        INC(len);
    END;

    (*??? check for X/windows and do a message box. *)
    (* if so then with what toolkit? *)
    write(STDERR_FILENO, str, len);
END ErrorWrite;

PROCEDURE ErrorWriteLn;
CONST
    CrLf : ARRAY [0..1] OF ACHAR = {ACHR(13), ACHR(10)};
BEGIN
    (*??? check for X/windows and do a message box. *)
    (* if so then with what toolkit? *)
    write(STDERR_FILENO, CrLf, 2);
END ErrorWriteLn;

%END

PROCEDURE ErrorMessage(str1, str2 : ARRAY OF CHAR);
BEGIN
    %IF Windows %THEN
        MessageBox(NULL_HWND, str2, str1, MB_OK BOR MB_ICONSTOP BOR MB_SETFOREGROUND BOR MB_TASKMODAL);
    %ELSIF UNIX %THEN
        ErrorWriteLn;
        ErrorWrite(str1);
        ErrorWriteLn;
        ErrorWrite(str2);
        ErrorWriteLn;
    %END
END ErrorMessage;

%IF Bits64 %THEN
(*
  This procedure gets the return address of the current procedure.  The procedure requires a frame.
  The Return Address is at the location pointed to by RBP
*)
PROCEDURE GetReturnAddr() : ADDRESS; PUREASM;
ASM
        lea    rax, [rbp+8]
        ret
END GetReturnAddr;
%END

PROCEDURE ConvertDec(num : CARDINAL; VAR OUT str : ARRAY OF CHAR);
VAR
    d           : CARDINAL;
    i           : ADRCARD;
    highStr     : ADRCARD;
BEGIN
    d := 1000000000;
    WHILE (d > num) AND (d > 1) DO
        d := d / 10;
    END;

    i := 0;
    highStr := HIGH(str);
    REPEAT
        IF i <= highStr THEN
            str[i] := ACHR(((num / d) REM 10) + ORD('0'));
            INC(i);
        END;
        d := d / 10;
    UNTIL d = 0;

    IF i <= highStr THEN
        str[i] := '';
    END;
END ConvertDec;

PROCEDURE ConvertHex(num : ADRCARD; VAR OUT str : ARRAY OF CHAR);
CONST
    HexDig      : ARRAY [0..15] OF ACHAR = {"0123456789ABCDEF"};
    digits      = SIZE(ADRCARD)*2;
VAR
    i   : ADRCARD;
BEGIN
    FOR i := digits-1 TO 0 BY -1 DO
        str[i] := HexDig[num REM 16];
        num := num / 16;
    END;
    str[digits] := '';
END ConvertHex;

PROCEDURE Append(source : ARRAY OF CHAR; VAR INOUT destination : ARRAY OF CHAR);
VAR
    i                   : ADRCARD;
    j                   : ADRCARD;
    highSource          : ADRCARD;
    highDestination     : ADRCARD;
BEGIN
    highSource := HIGH(source);
    highDestination := HIGH(destination);
    i := 0;
    WHILE (i <= highDestination) AND (destination[i] <> '') DO
        INC(i);
    END;

    j := 0;
    WHILE (j <= highSource) AND (source[j] <> '') AND (i <= highDestination) DO
        destination[i] := source[j];
        INC(i);
        INC(j);
    END;

    IF i <= highDestination THEN
        destination[i] := '';
    END;
END Append;

PROCEDURE Terminate;
BEGIN
    PMD(FALSE);
    SYSHALT(255);
END Terminate;

PROCEDURE ReadCodeByte(offs : ADDRESS) : CARDINAL8 [INLINE];
TYPE
    bytePtr     = POINTER TO CARDINAL8;
BEGIN
    RETURN CAST(bytePtr, offs)^;
END ReadCodeByte;

PROCEDURE ReadCodeWord(offs : ADDRESS) : CARDINAL16 [INLINE];
TYPE
    wordPtr     = POINTER TO CARDINAL16;
BEGIN
    RETURN CAST(wordPtr, offs)^;
END ReadCodeWord;

PROCEDURE ReadCodeDword(offs : ADDRESS) : CARDINAL32 [INLINE];
TYPE
    dwordPtr    = POINTER TO CARDINAL32;
BEGIN
    RETURN CAST(dwordPtr, offs)^;
END ReadCodeDword;

PROCEDURE GetCheckingInfo(checking : ADDRESS; VAR OUT line : CARDINAL; VAR OUT name : ARRAY OF CHAR);
VAR
    i           : ADRCARD;
    highName    : ADRCARD;
    db          : CARDINAL8;
    nameAddr    : ADDRESS;
BEGIN
    line := 0;
    name := "";
    IF checking <> NIL THEN
        line := ReadCodeWord(checking);
        checking := ADDADR(checking, 2);

        (* get the offset of the module name *)

        <*/PUSH/NOCHECK:O/NOWARN:U*>
        nameAddr := ADDADR(checking, VAL(ADRINT, CAST(INTEGER32, ReadCodeDword(checking))));
        <*/POP*>

        (* read the module name *)

        name := "";
        i := 0;
        highName := HIGH(name);
        REPEAT
            db := ReadCodeByte(nameAddr);
            nameAddr  := ADDADR(nameAddr, 1);
            IF i <= highName THEN
                name[i] := ACHR(db);
                INC(i);
            END;
        UNTIL (db = 0) OR (i > highName);
    END;
END GetCheckingInfo;

PROCEDURE IsValidSource(src : ExceptionSource) : BOOLEAN;
VAR
	N : ADRCARD;
BEGIN
	N := CAST (ADRCARD,src);
    RETURN (N > 0) & (N < CAST(ADRCARD, NextSource));
END IsValidSource;

PROCEDURE Unhandled(src : ExceptionSource;
                    num : ExceptionNumber;
                    mess : ARRAY OF CHAR;
                    addr : ADDRESS;
                    checking : ADDRESS) [NEVERRETURNS];
VAR
    str         : ARRAY [0..255] OF CHAR;
    temp        : ARRAY [0..63] OF CHAR;
    line        : CARDINAL;
BEGIN
    UNREFERENCED_PARAMETER(num);

    UserUnhandledProc;

    IF IsValidSource(src) THEN
        str := "at ";
        IF checking <> NIL THEN
            GetCheckingInfo(checking, line, temp);

            Append(temp, str);
            Append(".", str);
            ConvertDec(line, temp);
        ELSE
            ConvertHex(CAST(ADRCARD, addr), temp);
        END;
        Append(temp, str);
        Append(" -> ", str);
        Append(mess, str);

        ErrorMessage("Unhandled exception", str);
    END;

    Terminate;
END Unhandled;

%IF Bits32 %OR %NOT Windows %THEN
PROCEDURE ClearFpuState() [ALTERS()]; ASSEMBLER;
TYPE
    fpuEnv32bit =
    RECORD
        cw      : CARDINAL16;
        res1    : CARDINAL16;
        sw      : CARDINAL16;
        res2    : CARDINAL16;
        tw      : CARDINAL16;
        res3    : CARDINAL16;

        dontCare: ARRAY [0..15] OF CARDINAL8;
    END;

VAR
    state       : fpuEnv32bit;
ASM
        (* we want to clear the fpu stack of values *)
        (* and set the fpu stack pointer to zero *)

        FSTSW   AX
        TEST    AX, 0011100011111111b
        JZ      @exit
        FSTENV  BYTE PTR state
        MOV     state.sw, 0
        MOV     state.tw, 0FFFFh
        FLDENV  BYTE PTR state
    @exit:
END ClearFpuState;
%END

%IF Bits32 %THEN
PROCEDURE ExecuteHandler(exf : FramePtr) [PASS(AX),ALTERS(AX),NEVERRETURNS]; PUREASM;
ASM
        CALL    ClearFpuState

    %IF Windows %THEN
        MOV     ESP, exf
        MOV     DWORD PTR FS:[0], ESP
        MOV     EAX, [ESP].Frame.handler
        MOV     EBP, [ESP].Frame.curBP
    %ELSE
        MOV     EBP, exf
        MOV     ESP, [EBP].Frame.curSP
        MOV     EAX, [EBP].Frame.handler
    %END
        (*
          Look for a CALL UnhandledException,
          if not execute handler directly,
          otherwise set EBP and ESP to safe locations
        *)
        CMP     BYTE PTR PMDEnable, 0
        JE      @DoHandler
        CMP     BYTE PTR [EAX], 0E8h
        JNE     @DoHandler
        PUSH    EDX
        MOV     EDX, [EAX+1]
        LEA     EDX, [EDX+EAX+5]
        CMP     EDX, OFFSET UnhandledException
        POP     EDX
        JNE     @DoHandler
        MOV     ESP, SafeSavedSP
        PUSH    EBP
    @DoHandler:
        JMP     EAX
END ExecuteHandler;
%ELSIF Bits64 %AND %NOT Windows %THEN
PROCEDURE ExecuteHandler(exf : FramePtr) [PASS(AX),ALTERS(AX,DX),NEVERRETURNS]; PUREASM;
ASM
        CALL    ClearFpuState

    %IF Windows %THEN
        MOV     RBP, exf
        MOV     RSP, [RBP].Frame.SP
        MOV     RAX, [RBP].Frame.handler
        MOV     RBP, [RBP].Frame.BP
    %ELSE
        MOV     RBP, exf
        MOV     RSP, [RBP].Frame.curSP
        MOV     RAX, [RBP].Frame.handler
    %END
        (*
          Look for a CALL UnhandledException,
          if not execute handler directly,
          otherwise set EBP and ESP to safe locations
        *)
        %IF PICCode %THEN
        MOV     RDX, GOT PMDEnable
        CMP     BYTE PTR [RDX+GOTOFFS PMDEnable], 0
        %ELSE
        CMP     BYTE PTR PMDEnable, 0
        %END
        JE      @DoHandler
        CMP     BYTE PTR [RAX], 0E8h
        JNE     @DoHandler
        MOV     EDX, [RAX+1]
        MOVSXD  RDX, EDX
        LEA     RDX, [RDX+RAX+5]
        %IF PICCode %THEN
        CMP     RDX, GOT UnhandledException
        %ELSE
        CMP     RDX, OFFSET UnhandledException
        %END
        JNE     @DoHandler
        %IF PICCode %THEN
        MOV     RDX, GOT SafeSavedSP
        MOV     RSP, [RDX+GOTOFFS SafeSavedSP]
        %ELSE
        MOV     RSP, SafeSavedSP
        %END
        PUSH    RBP
    @DoHandler:
        JMP     RAX
END ExecuteHandler;
%END

%IF Windows %AND Bits64 %THEN
PROCEDURE GetScope(imageBase : CARDINAL64; rFunc : PRUNTIME_FUNCTION) : PSCOPE_TABLE_M2;
VAR
    scope       : CARDINAL64;
    info        : PUNWIND_INFO;
BEGIN
    IF rFunc <> NIL THEN
        scope := imageBase + VAL(CARDINAL64, rFunc^.UnwindData);
        info := MAKEADR(scope);
        IF (info^.Flags BAND UNW_FLAG_EHANDLER) <> 0 THEN
            scope := scope + SIZE(UNWIND_INFO) + (ORD64(info^.CountOfCodes) * SIZE(UNWIND_CODE));
            IF ODD(info^.CountOfCodes) THEN
                scope := scope + SIZE(UNWIND_CODE);
            END;
            scope := scope + 4;(*handle address*)
            RETURN MAKEADR(scope);
        END;
    END;
    RETURN NIL;
END GetScope;

PROCEDURE GetBp() : ADRCARD [ALTERS(AX)]; PUREASM;
ASM
    MOV         RAX, RBP
    RET
END GetBp;

PROCEDURE GetPrevBp() : ADRCARD [ALTERS(AX)]; PUREASM;
ASM
    MOV         RAX, [RBP]
    RET
END GetPrevBp;

PROCEDURE GetFramePtr (VAR Context : CONTEXT) : CARDINAL64 [INLINE];
BEGIN
	RETURN Context.R13; (* register should be the same as M2oper.AuxFP in the compiler sources *)
END GetFramePtr;


PROCEDURE FindExFrame (active : BOOLEAN; skipPast : CARDINAL64; VAR OUT frame : Frame) : BOOLEAN;
VAR
    rFunc               : PRUNTIME_FUNCTION;
    history             : UNWIND_HISTORY_TABLE;
    imageBase           : CARDINAL64;
    handlerData         : ADDRESS;
    establisherFrame    : CARDINAL64;
    context             : CONTEXT;
    handler             : ADDRESS;
    scope               : PSCOPE_TABLE_M2;
    state               : StatePtr;
	First				: BOOLEAN = TRUE;
BEGIN
    ZeroMem(ADR(history), SIZE(history));
    history.Unwind := TRUE;

    RtlCaptureContext(context);
    rFunc := RtlLookupFunctionEntry(context.Rip, imageBase, ADR(history));
    LOOP
        IF rFunc <> NIL THEN
            handlerData := NIL;
            handler := CAST(ADDRESS, RtlVirtualUnwind(UNW_FLAG_NHANDLER,
                                                      imageBase,
                                                      context.Rip,
                                                      rFunc,
                                                      context,
                                                      handlerData,
                                                      establisherFrame,
                                                      NIL
                                                      ));

        ELSE
            handler := NIL;
            context.Rip := CAST(tPtrC64, context.Rsp)^;
            context.Rsp := context.Rsp + 8;
        END;

        IF (context.Rbp = 0) OR (context.Rip = 0) THEN
            RETURN FALSE
        END;

        rFunc := RtlLookupFunctionEntry (context.Rip-1, imageBase, ADR(history));
		(* -1 is necessary for an extremal case where CALL instruction is the last in the procedure *)
        IF (context.Rbp > skipPast) & (rFunc # NIL) THEN
			scope := GetScope(imageBase, rFunc);
			IF scope <> NIL THEN
				IF scope^.sig.dq = ADWSig.dq THEN
					state := MAKEADR (GetFramePtr(context) + CAST (CARDINAL64, INT64(scope^.table[0].stateOffset)));

					frame.state := state;
					frame.handler := MAKEADR (imageBase + ORD64(scope^.table[0].jumpTarget));
					IF ~ active OR ~ First OR (state^.src = NIL) THEN
						RETURN TRUE;
					END;
				END;
				First := FALSE;
			END;
        END;
    END; (* LOOP *)
END FindExFrame;
%END

PROCEDURE GetFrameStart() : ADDRESS; ASSEMBLER;
ASM
%IF IA32 %THEN
    %IF Windows %THEN
    MOV         EAX, DWORD PTR FS:[0]
    %ELSE
    MOV         EAX, EBP
    %END
%ELSE
    MOV         RAX, RBP
%END
END GetFrameStart;

PROCEDURE GetActiveFrame(VAR OUT frame : Frame) : FramePtr;
VAR
    ptr         : FramePtr;
BEGIN
    SuppressRegisterSave := TRUE;
    ptr := GetFrameStart();
    %IF Windows %AND Bits32 %THEN
        UNREFERENCED_PARAMETER(frame);
        (* need to start loop by getting last frame since we installed an exception handler *)
        LOOP
            ptr := ptr^.prevFrame;
            IF (ptr <> NIL) AND (ptr <> CAST(FramePtr, MAKEADR(0FFFFFFFFh))) THEN
                IF ptr^.signature = ADWExceptionSignature THEN
                    IF ptr^.handler <> NIL THEN
                        EXIT;
                    END;
                END;
            ELSE
                ptr := NIL;
                EXIT;
            END;
        END;
    %ELSIF Windows %AND Bits64 %THEN
        ptr := NIL;
        IF FindExFrame (TRUE, GetPrevBp(), frame) THEN
            ptr := ADR(frame);
        END;
    %ELSE
        UNREFERENCED_PARAMETER(frame);
        (* need to start loop by getting last frame since we installed an exception handler *)
        LOOP
            (* get the old BP *)
            (* if NIL we have walked the whole stack - EXIT *)
            (* look for a frame signature *)
            (* do we have an active frame *)
            ptr := ptr^.oldBP;
            IF ptr <> NIL THEN
                IF ptr^.signature = ADWExceptionSignature THEN
                    IF ptr^.handler <> NIL THEN
                        EXIT;
                    END;
                END;
            ELSE
                EXIT;
            END;
        END;
    %END
    SuppressRegisterSave := FALSE;
    RETURN ptr;
EXCEPT
    SuppressRegisterSave := FALSE;
    RETURN NIL;
END GetActiveFrame;

%IF UNIX %THEN
PROCEDURE GetActiveFrameEx(startBp : ADDRESS) : FramePtr;
VAR
    ptr         : FramePtr;
BEGIN
    SuppressRegisterSave := TRUE;
    ptr := startBp;
    LOOP
        IF ptr <> NIL THEN
            IF (ptr^.signature = ADWExceptionSignature) AND (ptr^.handler <> NIL) THEN
                SuppressRegisterSave := FALSE;
                RETURN ptr;
            END;
            ptr := ptr^.oldBP;
        ELSE
            EXIT
        END;
    END;
    SuppressRegisterSave := FALSE;
    RETURN NIL;
EXCEPT
    SuppressRegisterSave := FALSE;
    RETURN NIL;
END GetActiveFrameEx;
%END

%IF %NOT (Windows %AND Bits64) %THEN
PROCEDURE GetCurrentFrame() : FramePtr;
VAR
    ptr                 : FramePtr;
BEGIN
    SuppressRegisterSave := TRUE;
    ptr := GetFrameStart();
    %IF Windows %AND Bits32 %THEN
        IF ptr <> NIL THEN
            (* need to start loop by getting last frame since we installed an exception handler *)
            LOOP
                ptr := ptr^.prevFrame;
                IF ptr = CAST(FramePtr, MAKEADR(0FFFFFFFFh)) THEN
                    ptr := NIL;
                    EXIT;
                ELSIF (ptr = NIL) OR (ptr^.signature = ADWExceptionSignature) THEN
                    EXIT;
                END;
            END;
        END;
    %ELSE
        (* need to start loop by getting last frame since we installed an exception handler *)
        LOOP
            ptr := ptr^.oldBP;
            IF (ptr = NIL) OR (ptr^.signature = ADWExceptionSignature) THEN
                EXIT;
            END;
        END;
    %END
    SuppressRegisterSave := FALSE;
    RETURN ptr;
EXCEPT
    SuppressRegisterSave := FALSE;
    RETURN NIL;
END GetCurrentFrame;
%END

PROCEDURE GetCurrentExStatePtr() : StatePtr;
VAR
%IF Windows %AND Bits64 %THEN
    frame       : Frame;
%ELSE
    frame       : FramePtr;
%END
BEGIN
    %IF Windows %AND Bits64 %THEN
        IF FindExFrame (FALSE, GetPrevBp(), frame) THEN
            RETURN frame.state;
        END;
    %ELSE
        frame := GetCurrentFrame();
        IF frame <> NIL THEN
            RETURN frame^.state;
        END;
    %END
    RETURN NIL;
END GetCurrentExStatePtr;

PROCEDURE AllocateSource(VAR OUT src : ExceptionSource);
BEGIN
    src := NextSource;
    NextSource := ADDADR(NextSource, 1);
END AllocateSource;

%IF Windows %THEN
PROCEDURE RaiseWindows(src : ExceptionSource;
                       num : ExceptionNumber;
                       mess : ARRAY OF CHAR;
                       addr : ADDRESS;
                       checking : ADDRESS) [NEVERRETURNS];
VAR
    exceptParams : ARRAY [0..NumExceptParams-1] OF ULONG_PTR;
BEGIN
    exceptParams[0] := CAST(ADRCARD, src);
    exceptParams[1] := num;
    exceptParams[2] := CAST(ADRCARD, ADR(mess));
    exceptParams[3] := LENGTH(mess);
    exceptParams[4] := CAST(ADRCARD, addr);
    exceptParams[5] := CAST(ADRCARD, checking);
    RaiseException(OurWindowsExceptionCode, EXCEPTION_NONCONTINUABLE, NumExceptParams, exceptParams);
END RaiseWindows;
%END

PROCEDURE Raise(src : ExceptionSource; num : ExceptionNumber; mess : ARRAY OF CHAR) [NEVERRETURNS];
VAR
    pa          : POINTER TO ADDRESS;
    addr        : ADDRESS;
    exf         : FramePtr;
    frame       : Frame;
BEGIN
	IF ~ IsValidSource (src) THEN
		RaiseM2Exception (exException, "ILLEGAL-EXCEPTION-SOURCE");
	END;
    %IF IA32 %THEN
        pa := SUBADR(ADR(num), 12);
    %ELSIF AMD64 %THEN
        pa := GetReturnAddr();
    %ELSE
        fix me
    %END

    (* fetch the return address of this proc *)
    (* and adjust for the call *)

    addr := SUBADR(pa^, 5);

    IF PMDEnable AND (NOT SuppressRegisterSave) THEN
        SaveRegs;
    END;

    exf := GetActiveFrame(frame);
    IF exf <> NIL THEN
        %IF Windows %THEN
            RaiseWindows(src, num, mess, addr, SUBADR(exf^.handler, 6));
        %ELSE
            exf^.state^.src := src;
            exf^.state^.num := num;
            exf^.state^.mess := mess;
            exf^.state^.addr := addr;
            exf^.state^.checking := SUBADR(exf^.handler, 6);
            ExecuteHandler(exf);
        %END
    ELSE
        Unhandled(src, num, mess, addr, NIL);
    END;
END Raise;

PROCEDURE RAISE (src : ExceptionSource; num : ExceptionNumber; mess : ARRAY OF CHAR) [NEVERRETURNS]; PUREASM;
ASM
		JMP		Raise;
END RAISE;

PROCEDURE RaiseRTL (src : ExceptionSource; num : ExceptionNumber; mess : ARRAY OF CHAR) [NEVERRETURNS]; PUREASM;
ASM
		JMP		Raise;
END RaiseRTL;


PROCEDURE CurrentNumber(src : ExceptionSource) : ExceptionNumber [FRAME];
VAR
    state       : StatePtr;
BEGIN
    state := GetCurrentExStatePtr();
    IF state = NIL THEN
        RaiseM2Exception(exException, "NO-CURRENT-EXCEPTION");
        RETURN 0;
    ELSIF state^.src <> src THEN
        RaiseM2Exception(exException, "NOT-EXCEPTION-SOURCE");
        RETURN 0;
    ELSE
        RETURN state^.num;
    END;
END CurrentNumber;

PROCEDURE GetMessage(VAR OUT text : ARRAY OF CHAR) [FRAME];
VAR
    state       : StatePtr;
BEGIN
    text := "";
    state := GetCurrentExStatePtr();
    IF state <> NIL THEN
        text := state^.mess;
    END;
END GetMessage;

PROCEDURE IsCurrentSource(src : ExceptionSource) : BOOLEAN [FRAME];
VAR
    state       : StatePtr;
BEGIN
    state := GetCurrentExStatePtr();
    IF state <> NIL THEN
        RETURN state^.src = src;
    END;
    RETURN FALSE;
END IsCurrentSource;

PROCEDURE IsExceptionalExecution() : BOOLEAN [FRAME];
VAR
    state       : StatePtr;
BEGIN
    state := GetCurrentExStatePtr();
    IF state <> NIL THEN
        RETURN state^.src <> NoSource;
    END;
    RETURN FALSE;
END IsExceptionalExecution;

%IF Windows %AND Bits32 %THEN
PROCEDURE UnlinkCurrentFrame(exf : FramePtr) [PASS(AX)]; ASSEMBLER;
ASM
        MOV     EAX, exf
        MOV     EAX, [EAX].Frame.prevFrame;
        MOV     DWORD PTR FS:[0], EAX
END UnlinkCurrentFrame;
%END

%IF Windows %AND Bits64 %THEN
PROCEDURE HackReraiseReturnAddress() [INLINE];
VAR
    retAddr     : POINTER TO ARRAY [0..1] OF ADDRESS;
BEGIN
    retAddr := MAKEADR(GetBp());(* get RERAISE's bp *)
    retAddr^[1] := SUBADR(retAddr^[1], 5);
END HackReraiseReturnAddress;
%END

PROCEDURE RERAISE(state_in : ADDRESS) [FRAME, NEVERRETURNS];
VAR
    newExf      : FramePtr;
    state       : StatePtr;
    frame       : Frame;
%IF Windows %AND Bits32 %THEN
    curExf      : FramePtr;
%END
BEGIN
    state := state_in;

    %IF Windows %AND Bits64 %THEN
        (* RERAISE never returns and it is called as the last instruction of a procedure
           so it's return address is not in the address range of the procedure where it was called.
           this makes the Windows unwind code return the wrong rFunc data point to us.
           it returns the next function in memory.
           subtracting the call instruction bytes gets the return address in the correct range.
           simple workaround for the windows unwind since this proc never returns.
        *)
        HackReraiseReturnAddress;
    %END

    newExf := GetActiveFrame(frame);
    IF (newExf <> NIL) AND (newExf^.state <> state) THEN
        %IF Windows %THEN
            %IF Bits32 %THEN
                curExf := GetCurrentFrame();
                UnlinkCurrentFrame(curExf);
            %END
            RaiseWindows(state^.src,
                         state^.num,
                         state^.mess,
                         state^.addr,
                         state^.checking);
        %ELSE
            newExf^.state^.src := state^.src;
            newExf^.state^.num := state^.num;
            newExf^.state^.mess := state^.mess;
            newExf^.state^.addr := state^.addr;
            newExf^.state^.checking := state^.checking;
            ExecuteHandler(newExf);
        %END
    END;

    IF (state <> NIL) AND IsValidSource(state^.src) THEN
        Unhandled(state^.src,
                  state^.num,
                  state^.mess,
                  state^.addr,
                  state^.checking);
    END;
    Terminate;
END RERAISE;

PROCEDURE UnhandledException ["EXCEPTIONS_UNHANDLEDEXCEPTION"]() [FRAME, NEVERRETURNS];
VAR
    state       : StatePtr;
BEGIN
    state := GetCurrentExStatePtr();
    IF state <> NIL THEN
        IF CAST(ADRCARD, state^.src) < CAST(ADRCARD, NextSource) THEN
            Unhandled(state^.src,
                      state^.num,
                      state^.mess,
                      state^.addr,
                      state^.checking);
        END;
    END;

    Terminate;
END UnhandledException;

PROCEDURE EXCEPTADR ["EXCEPTIONS_EXCEPTADR"] () : ADDRESS [FRAME];
VAR
    state       : StatePtr;
BEGIN
    state := GetCurrentExStatePtr();
    IF (state <> NIL) AND (state^.src <> NoSource) THEN
        RETURN state^.addr;
    END;
    RETURN NIL;
END EXCEPTADR;

PROCEDURE EXCEPT_INFO ["EXCEPTIONS_EXCEPT_INFO"]
            (VAR OUT addr : ADDRESS;
             VAR OUT line : CARDINAL;
             VAR OUT name : ARRAY OF CHAR) [FRAME];
VAR
    state       : StatePtr;
BEGIN
    addr := NIL;
    line := 0;
    name := "";
    state := GetCurrentExStatePtr();
    IF state <> NIL THEN
        IF state^.src <> NoSource THEN
            addr := state^.addr;
            GetCheckingInfo(state^.checking, line, name);
        END;
    END;
END EXCEPT_INFO;

PROCEDURE SetUnhandledExceptionProc ["EXCEPTIONS_SetUnhandledExceptionProc"] (proc : PROC);
BEGIN
    UserUnhandledProc := proc;
END SetUnhandledExceptionProc;

PROCEDURE AttachDebugger ["EXCEPTIONS_AttachDebugger"] (opt : AttachDebuggerOpt);
BEGIN
    DebuggerAttach := opt;
END AttachDebugger;

%IF Windows %THEN

%IF Bits32 %THEN

PROCEDURE Win32ExceptFilter ["EXCEPTIONS_Win32ExceptFilter"]
                (info_in, frame_in, context_in, dispatch_in : ADDRESS) : CARDINAL
                    [RightToLeft, LEAVES];
CONST
    ignore              = EXCEPTION_UNWIND BOR EXCEPTION_NESTED_CALL BOR EXCEPTION_STACK_INVALID;
VAR
    exf         : FramePtr;
    pStr        : POINTER TO ARRAY [0..0] OF CHAR;
    strLen      : CARDINAL;
    info     	: PEXCEPTION_RECORD;
    exPtrs      : EXCEPTION_POINTERS;
    pRegs       : PCONTEXT;
    dispatch    : PDISPATCHER_CONTEXT;
BEGIN
    dispatch := dispatch_in;

    info := info_in;
    IF ((info^.ExceptionCode BAND 0C0000000h) <> 0) AND(* errors only *)
       ((info^.ExceptionFlags BAND ignore) = 0)
    THEN
        exf := frame_in;
        IF exf^.handler <> NIL THEN
            IF (info^.ExceptionCode = OurWindowsExceptionCode) AND
               (info^.NumberParameters = NumExceptParams)
            THEN
                IF DebuggerAttach = AttachAll THEN
                    exPtrs.ExceptionRecord := info_in;
                    exPtrs.ContextRecord := context_in;
                    RETURN UnhandledExceptionFilter(exPtrs);
                END;

                exf^.state^.src := MAKEADR(info^.ExceptionInformation[0]);
                exf^.state^.num := info^.ExceptionInformation[1];
                pStr := CAST(ADDRESS, info^.ExceptionInformation[2]);
                strLen := info^.ExceptionInformation[3];
				IF strLen # 0 THEN
                	exf^.state^.mess := pStr^[0..strLen-1];
				END;
				IF strLen <= 255 THEN
					exf^.state^.mess[strLen] := 0C;
				END;
                exf^.state^.addr := CAST(ADDRESS, info^.ExceptionInformation[4]);
                exf^.state^.checking := CAST(ADDRESS, info^.ExceptionInformation[5]);
            ELSE
                IF DebuggerAttach >= AttachExternal THEN
                    exPtrs.ExceptionRecord := info_in;
                    exPtrs.ContextRecord := context_in;
                    RETURN UnhandledExceptionFilter(exPtrs);
                END;

                IF PMDEnable AND (NOT SuppressRegisterSave) THEN
                    pRegs := context_in;
                    AssignThreadContextToThreadInfo(GlobalThreadInfo:tThreadInfo,
                                                    pRegs^,
                                                    GetCurrentThreadId());
                END;

                exf^.state^.src := M2Except;
                exf^.state^.num := ORD(sysException);
                exf^.state^.addr := info^.ExceptionAddress;
                exf^.state^.checking := SUBADR(exf^.handler, 6);

                CASE info^.ExceptionCode OF
                EXCEPTION_ACCESS_VIOLATION:
                    exf^.state^.mess := "ACCESS_VIOLATION";
                |
                EXCEPTION_PRIV_INSTRUCTION:
                    exf^.state^.mess := "ILLEGAL_INSTRUCTION";
                |
                EXCEPTION_STACK_OVERFLOW:
                    exf^.state^.mess := "STACK_OVERFLOW";
                |
                EXCEPTION_INT_DIVIDE_BY_ZERO,
                EXCEPTION_FLT_DIVIDE_BY_ZERO:
                    exf^.state^.mess := "DIVIDE_BY_ZERO";
                |
                EXCEPTION_FLT_DENORMAL_OPERAND:
                    exf^.state^.mess := "FLOAT_DENORMAL";
                |
                EXCEPTION_FLT_OVERFLOW:
                    exf^.state^.mess := "FLOAT_OVERFLOW";
                |
                EXCEPTION_FLT_UNDERFLOW:
                    exf^.state^.mess := "FLOAT_UNDERFLOW";
                |
                EXCEPTION_FLT_INEXACT_RESULT:
                    exf^.state^.mess := "FLOAT_INEXACT";
                |
                STATUS_NO_MEMORY:
                    exf^.state^.mess := "NO_MEMORY";
                ELSE
                    ConvertHex(info^.ExceptionCode, exf^.state^.mess);
                END;
            END;

            ExecuteHandler(exf);
        END;
    END;

    RETURN ORD(ExceptionContinueSearch);
END Win32ExceptFilter;

%ELSE

PROCEDURE WinTableExecHandler ["SYSTEMEX_Debug_WinTableExecHandler"]
                (pRegs : PCONTEXT; handler : CARDINAL64) [NEVERRETURNS];
(* this procedure exists for the debugger to be able to set a break on.
   it uses this procedure to easily identify the address of the exception handler to be able
   to set a break on that location.
*)
BEGIN
    pRegs^.Rip := handler;
    RtlRestoreContext(pRegs^, NIL);
    (* never get here *)
END WinTableExecHandler;


PROCEDURE WinExceptTableHandler ["EXCEPTIONS_WinExceptTableHandler"]
                (info_in, frame_in, context_in, dispatch_in : ADDRESS) : CARDINAL;
CONST
    ignore              = EXCEPTION_UNWIND BOR EXCEPTION_NESTED_CALL BOR EXCEPTION_STACK_INVALID;
VAR
    pStr                : POINTER TO ARRAY [0..0] OF CHAR;
    strLen              : CARDINAL;
    info                : PEXCEPTION_RECORD;
    dispatch            : PDISPATCHER_CONTEXT;
    scope               : PSCOPE_TABLE_M2;
    beginA,
    endA                : CARDINAL64;
    exPtrs              : EXCEPTION_POINTERS;
    pRegs               : PCONTEXT;
    state               : StatePtr;
    handlerFrame        : CARDINAL64;
    handler             : CARDINAL64;
    rFunc               : PRUNTIME_FUNCTION;
    history             : UNWIND_HISTORY_TABLE;
    imageBase           : CARDINAL64;
    handlerData         : ADDRESS;
    establisherFrame    : CARDINAL64;
BEGIN
    info := info_in;
    dispatch := dispatch_in;
    scope := dispatch^.HandlerData;
    pRegs := context_in;
    handlerFrame := CAST(ADRCARD, frame_in);


    IF ((info^.ExceptionCode BAND 0C0000000h) <> 0) AND(*errors only*)
       ((info^.ExceptionFlags BAND ignore) = 0) AND
       (scope <> NIL) AND
       (scope^.sig.dq = ADWSig.dq)(*valid M2 scope*)
    THEN
        beginA := dispatch^.ImageBase + ORD64(scope^.table[0].beginAddress);
        endA := dispatch^.ImageBase + ORD64(scope^.table[0].endAddress);
        handler := dispatch^.ImageBase + ORD64(scope^.table[0].jumpTarget);
        state := ADDADR (frame_in, INT64(scope^.table[0].stateOffset));


        IF (dispatch^.ControlPc >= beginA) AND (dispatch^.ControlPc <= endA) THEN


            IF (info^.ExceptionCode = OurWindowsExceptionCode) AND
               (info^.NumberParameters = NumExceptParams)
            THEN
                IF DebuggerAttach = AttachAll THEN
                    exPtrs.ExceptionRecord := info_in;
                    exPtrs.ContextRecord := context_in;
                    RETURN UnhandledExceptionFilter(exPtrs);
                END;


                state^.src := MAKEADR(info^.ExceptionInformation[0]);
                state^.num := info^.ExceptionInformation[1];
                pStr := CAST(ADDRESS, info^.ExceptionInformation[2]);
                strLen := info^.ExceptionInformation[3];
				IF strLen # 0 THEN
                	state^.mess := pStr^[0..strLen-1];
				END;
				IF strLen < 256 THEN
					state^.mess[strLen] := 0C;
				END;
                state^.addr := CAST(ADDRESS, info^.ExceptionInformation[4]);
                state^.checking := CAST(ADDRESS, info^.ExceptionInformation[5]);
            ELSE
                IF DebuggerAttach >= AttachExternal THEN
                    exPtrs.ExceptionRecord := info_in;
                    exPtrs.ContextRecord := context_in;
                    RETURN UnhandledExceptionFilter(exPtrs);
                END;


                IF PMDEnable AND (NOT SuppressRegisterSave) THEN
                    AssignThreadContextToThreadInfo(GlobalThreadInfo:tThreadInfo,
                                                    pRegs^,
                                                    GetCurrentThreadId());
                END;


                state^.src := M2Except;
                state^.num := ORD(sysException);
                state^.addr := info^.ExceptionAddress;
                state^.checking := MAKEADR(handler - 6);


                CASE info^.ExceptionCode OF
                EXCEPTION_ACCESS_VIOLATION:
                    state^.mess := "ACCESS_VIOLATION";
                |
                EXCEPTION_PRIV_INSTRUCTION:
                    state^.mess := "ILLEGAL_INSTRUCTION";
                |
                EXCEPTION_STACK_OVERFLOW:
                    state^.mess := "STACK_OVERFLOW";
                |
                EXCEPTION_INT_DIVIDE_BY_ZERO,
                EXCEPTION_FLT_DIVIDE_BY_ZERO:
                    state^.mess := "DIVIDE_BY_ZERO";
                |
                EXCEPTION_FLT_DENORMAL_OPERAND:
                    state^.mess := "FLOAT_DENORMAL";
                |
                EXCEPTION_FLT_OVERFLOW:
                    state^.mess := "FLOAT_OVERFLOW";
                |
                EXCEPTION_FLT_UNDERFLOW:
                    state^.mess := "FLOAT_UNDERFLOW";
                |
                EXCEPTION_FLT_INEXACT_RESULT:
                    state^.mess := "FLOAT_INEXACT";
                |
                STATUS_NO_MEMORY:
                    state^.mess := "NO_MEMORY";
                ELSE
                    ConvertHex(info^.ExceptionCode, state^.mess);
                END;
            END;


            (* unwind from the exception context to the exception handler context *)


            history := dispatch^.HistoryTable^;


            WHILE (GetFramePtr(pRegs^) <> handlerFrame) AND (pRegs^.Rip <> 0) DO
                rFunc := RtlLookupFunctionEntry(pRegs^.Rip, imageBase, ADR(history));
                IF rFunc <> NIL THEN
                    RtlVirtualUnwind(UNW_FLAG_NHANDLER,
                                     imageBase,
                                     pRegs^.Rip,
                                     rFunc,
                                     pRegs^,
                                     handlerData,
                                     establisherFrame,
                                     NIL
                                     );
                ELSE
                    pRegs^.Rip := CAST(tPtrC64, pRegs^.Rsp)^;
                    pRegs^.Rsp := pRegs^.Rsp + 8;
                END;
            END;
            IF GetFramePtr(pRegs^) = handlerFrame THEN
                WinTableExecHandler(pRegs, handler);
                (* never get here *)
            END;
        END;
    END;

    RETURN ORD(ExceptionContinueSearch);
END WinExceptTableHandler;
%END

%END

PROCEDURE SetSafeSavedSP(); ASSEMBLER;
ASM
    %IF Bits32 %THEN
    MOV EAX, EBP           (* use EBP to set SafeSavedSP.  It's relative to SP. *)
                           (*  SunOS changes ESP on entry to HandleSignal and that can be problematic *)
    SUB EAX, 100h
    MOV SafeSavedSP, EAX
    %ELSIF Bits64 %THEN
    MOV RAX, RBP           (* use EBP to set SafeSavedSP.  It's relative to SP. *)
                           (*  SunOS changes ESP on entry to HandleSignal and that can be problematic *)
    SUB RAX, 100h
    %IF PICCode %THEN
    MOV RDX, GOT SafeSavedSP
    MOV QWORD PTR [RDX+GOTOFFS SafeSavedSP], RAX
    %ELSE
    MOV SafeSavedSP, RAX
    %END
    %END
END SetSafeSavedSP;

PROCEDURE WriteDumpFile(VAR INOUT f : File; buf : ADDRESS; size : ADRCARD);
BEGIN
    WriteBlock(f, buf, size);
    PMDFileWriteLocation := PMDFileWriteLocation + size;
END WriteDumpFile;

%IF WINDOWS %THEN
PROCEDURE AssignThreadContextToThreadInfo(VAR OUT threadInfo : tThreadInfo;
                                          threadContext : CONTEXT;
                                          threadId : CARDINAL);
BEGIN
    threadInfo.ThreadId := threadId;

    %IF IA32 %THEN
        threadInfo.IntegerRegisters[SegGs] := threadContext.SegGs;
        threadInfo.IntegerRegisters[SegFs] := threadContext.SegFs;
        threadInfo.IntegerRegisters[SegEs] := threadContext.SegEs;
        threadInfo.IntegerRegisters[SegDs] := threadContext.SegDs;
        threadInfo.IntegerRegisters[rEdi] := threadContext.Edi;
        threadInfo.IntegerRegisters[rEsi] := threadContext.Esi;
        threadInfo.IntegerRegisters[rEbx] := threadContext.Ebx;
        threadInfo.IntegerRegisters[rEdx] := threadContext.Edx;
        threadInfo.IntegerRegisters[rEcx] := threadContext.Ecx;
        threadInfo.IntegerRegisters[rEax] := threadContext.Eax;
        threadInfo.IntegerRegisters[rEbp] := threadContext.Ebp;
        threadInfo.IntegerRegisters[rEip] := threadContext.Eip;
        threadInfo.IntegerRegisters[SegCs] := threadContext.SegCs;
        threadInfo.IntegerRegisters[EFlags] := threadContext.EFlags;
        threadInfo.IntegerRegisters[rEsp] := threadContext.Esp;
        threadInfo.IntegerRegisters[SegSs] := threadContext.SegSs;
    %ELSIF AMD64 %THEN
        threadInfo.IntegerRegisters[rR8] := threadContext.R8;
        threadInfo.IntegerRegisters[rR9] := threadContext.R9;
        threadInfo.IntegerRegisters[rR10] := threadContext.R10;
        threadInfo.IntegerRegisters[rR11] := threadContext.R11;
        threadInfo.IntegerRegisters[rR12] := threadContext.R12;
        threadInfo.IntegerRegisters[rR13] := threadContext.R13;
        threadInfo.IntegerRegisters[rR14] := threadContext.R14;
        threadInfo.IntegerRegisters[rR15] := threadContext.R15;

        threadInfo.IntegerRegisters[rRdi] := threadContext.Rdi;
        threadInfo.IntegerRegisters[rRsi] := threadContext.Rsi;
        threadInfo.IntegerRegisters[rRbx] := threadContext.Rbx;
        threadInfo.IntegerRegisters[rRdx] := threadContext.Rdx;
        threadInfo.IntegerRegisters[rRcx] := threadContext.Rcx;
        threadInfo.IntegerRegisters[rRax] := threadContext.Rax;
        threadInfo.IntegerRegisters[rRbp] := threadContext.Rbp;
        threadInfo.IntegerRegisters[rRip] := threadContext.Rip;
        threadInfo.IntegerRegisters[EFlags] := threadContext.EFlags;
        threadInfo.IntegerRegisters[rRsp] := threadContext.Rsp;
    %ELSE
        fix me
    %END
    SetSafeSavedSP;
END AssignThreadContextToThreadInfo;
%ELSIF UNIX %THEN
(*
PROCEDURE WriteRegs(threadInfo : tThreadInfo);
    PROCEDURE WriteReg(val : CARDINAL; str : ARRAY OF CHAR);
    VAR
        s : ARRAY [0..31] OF CHAR;
    BEGIN
        ErrorWrite(str);
        ConvertHex(val, s);
        ErrorWrite(s);
        ErrorWriteLn;
    END WriteReg;
BEGIN
    ErrorWriteLn;
    ErrorWrite("********REGISTER DUMP*************");
    ErrorWriteLn;
    WriteReg(threadInfo.IntegerRegisters[SegGs], "GS = ");
    WriteReg(threadInfo.IntegerRegisters[SegFs], "FS = ");
    WriteReg(threadInfo.IntegerRegisters[SegEs], "ES = ");
    WriteReg(threadInfo.IntegerRegisters[SegDs], "DS = ");
    WriteReg(threadInfo.IntegerRegisters[rEdi], "EDI = ");
    WriteReg(threadInfo.IntegerRegisters[rEsi], "ESI = ");
    WriteReg(threadInfo.IntegerRegisters[rEbx], "EBX = ");
    WriteReg(threadInfo.IntegerRegisters[rEdx], "EDX = ");
    WriteReg(threadInfo.IntegerRegisters[rEcx], "ECX = ");
    WriteReg(threadInfo.IntegerRegisters[rEax], "EAX = ");
    WriteReg(threadInfo.IntegerRegisters[rEbp], "EBP = ");
    WriteReg(threadInfo.IntegerRegisters[rEip], "IP = ");
    WriteReg(threadInfo.IntegerRegisters[SegCs], "CS = ");
    WriteReg(threadInfo.IntegerRegisters[rEsp], "SP = ");
    WriteReg(threadInfo.IntegerRegisters[SegSs], "SS = ");
END WriteRegs;
*)
PROCEDURE AssignThreadContextToThreadInfo(VAR OUT threadInfo : tThreadInfo;
                                          regs : gregset_t;
                                          threadId : ADRCARD);
BEGIN
    %IF Bits32 %THEN
    threadInfo.ThreadId := threadId;
    threadInfo.IntegerRegisters[SegGs] := regs[REG_GS];
    threadInfo.IntegerRegisters[SegFs] := regs[REG_FS];
    threadInfo.IntegerRegisters[SegEs] := regs[REG_ES];
    threadInfo.IntegerRegisters[SegDs] := regs[REG_DS];
    threadInfo.IntegerRegisters[rEdi] := regs[REG_EDI];
    threadInfo.IntegerRegisters[rEsi] := regs[REG_ESI];
    threadInfo.IntegerRegisters[rEbx] := regs[REG_EBX];
    threadInfo.IntegerRegisters[rEdx] := regs[REG_EDX];
    threadInfo.IntegerRegisters[rEcx] := regs[REG_ECX];
    threadInfo.IntegerRegisters[rEax] := regs[REG_EAX];
    threadInfo.IntegerRegisters[rEbp] := regs[REG_EBP];
    threadInfo.IntegerRegisters[rEip] := regs[REG_EIP];
    threadInfo.IntegerRegisters[SegCs] := regs[REG_CS];
    threadInfo.IntegerRegisters[EFlags] := regs[REG_EFL];
    threadInfo.IntegerRegisters[rEsp] := regs[REG_ESP];
    threadInfo.IntegerRegisters[SegSs] := regs[REG_SS];
    SetSafeSavedSP;
    %ELSIF Bits64 %THEN
    threadInfo.ThreadId := threadId;
    threadInfo.IntegerRegisters[rR8] := regs[REG_R8];
    threadInfo.IntegerRegisters[rR9] := regs[REG_R9];
    threadInfo.IntegerRegisters[rR10] := regs[REG_R10];
    threadInfo.IntegerRegisters[rR11] := regs[REG_R11];
    threadInfo.IntegerRegisters[rR12] := regs[REG_R12];
    threadInfo.IntegerRegisters[rR13] := regs[REG_R13];
    threadInfo.IntegerRegisters[rR14] := regs[REG_R14];
    threadInfo.IntegerRegisters[rR15] := regs[REG_R15];
    threadInfo.IntegerRegisters[rRdi] := regs[REG_RDI];
    threadInfo.IntegerRegisters[rRsi] := regs[REG_RSI];
    threadInfo.IntegerRegisters[rRbx] := regs[REG_RBX];
    threadInfo.IntegerRegisters[rRdx] := regs[REG_RDX];
    threadInfo.IntegerRegisters[rRcx] := regs[REG_RCX];
    threadInfo.IntegerRegisters[rRax] := regs[REG_RAX];
    threadInfo.IntegerRegisters[rRbp] := regs[REG_RBP];
    threadInfo.IntegerRegisters[rRip] := regs[REG_RIP];
    threadInfo.IntegerRegisters[EFlags] := regs[REG_EFL];
    threadInfo.IntegerRegisters[rRsp] := regs[REG_RSP];
    SetSafeSavedSP;
    %ELSE
        fix me
    %END
END AssignThreadContextToThreadInfo;
%END

%IF Windows %THEN
PROCEDURE DumpPmdInfo(VAR INOUT f : File; resumeThreads : BOOLEAN);
TYPE
    tThreadId = CARDINAL;
    tModuleInfo = RECORD
        LoadAddress : CARDINAL32;
        LoadSize    : CARDINAL32;
        NameLength  : CARDINAL32;
        Name        : ARRAY [0..1023] OF CHAR (* short allocated, use NameLength to read *)
    END;
VAR
    snap                : HANDLE;
    <*/PUSH/NOCHECK:U*>
    info                : PROCESSENTRY32;
    tInfo               : THREADENTRY32;
    mInfo               : MODULEENTRY32;
    threadContext       : CONTEXT;
    <*/POP*>
    threadHandle        : HANDLE;
    ourProcessId        : CARDINAL;
    currentThreadId     : CARDINAL;
    runningThreads      : POINTER TO ARRAY [0..0] OF tThreadId;
    maxRunningThreads   : INTEGER;
    numThreads          : INTEGER;
    numModules          : INTEGER;
    i                   : INTEGER;
    sizeToBeWritten     : CARDINAL;
    moduleInfo          : tModuleInfo;
    threadInfo          : tThreadInfo;
    processedOurProcess : BOOLEAN;
    address             : ADDRESS;
    lpBuffer            : MEMORY_BASIC_INFORMATION;
    numMemBlocks        : INTEGER32;
    memoryInfo          : POINTER TO ARRAY [0..0] OF tPmdMemoryInfoItem;
    maxMemoryInfoItems  : INTEGER;
    blockLoc            : ADRCARD;
    programName         : FileSpecString;
    heap                : HANDLE;
BEGIN
    heap := GetProcessHeap();

    maxRunningThreads := 256;
    runningThreads := HeapAlloc(heap, 0, maxRunningThreads * SIZE(tThreadId));

    maxMemoryInfoItems := 256;
    memoryInfo := HeapAlloc(heap, 0, maxMemoryInfoItems * SIZE(tPmdMemoryInfoItem));

    numThreads := 0;
    numModules := 0;
    processedOurProcess := FALSE;
    ourProcessId := GetCurrentProcessId();

    snap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD BOR TH32CS_SNAPPROCESS BOR TH32CS_SNAPMODULE, 0);
    ZeroMem(ADR(info), SIZE(info));
    info.dwSize := SIZE(info);
    GetProgramName(programName);

    IF Process32First(snap, info) THEN
        REPEAT
            IF info.th32ProcessID = ourProcessId THEN
                processedOurProcess := TRUE;
                ZeroMem(ADR(mInfo), SIZE(mInfo));
                mInfo.dwSize := SIZE(mInfo);
                IF Module32First(snap, mInfo) THEN
                    REPEAT
                        IF NOT Equal(mInfo.szExePath, programName) THEN
                            INC(numModules);
                        END;
                    UNTIL NOT Module32Next(snap, mInfo);

                   (*
                   NumModules - DWORD
                   ARRAY [0..NumModules-1] OF RECORD
                        LoadAddress : DWORD
                        LoadSize    : DWORD
                        NameLength  : DWORD
                        Name        : ARRAY [0..NameLength] OF CHAR (* short allocated,
                                                                       use NameLength to read *)
                   END
                   *)

                    WriteDumpFile(f, ADR(numModules), SIZE(numModules));
                    IF Module32First(snap, mInfo) THEN
                        REPEAT
                            IF NOT Equal(mInfo.szExePath, programName) THEN
                                moduleInfo.LoadAddress := CAST(ADRCARD, mInfo.modBaseAddr);
                                moduleInfo.LoadSize := mInfo.modBaseSize;
                                moduleInfo.NameLength := LENGTH(mInfo.szExePath);
                                IF moduleInfo.NameLength >= HIGH(moduleInfo.Name) THEN
                                    moduleInfo.NameLength := HIGH(moduleInfo.Name);
                                END;
                                moduleInfo.Name[0..moduleInfo.NameLength] := mInfo.szExePath;
                                sizeToBeWritten :=
                                    SIZE(moduleInfo) - SIZE(moduleInfo.Name) + moduleInfo.NameLength + 1;
                                WriteDumpFile(f, ADR(moduleInfo), sizeToBeWritten);
                            END;
                        UNTIL NOT Module32Next(snap, mInfo);
                    END;
                ELSE
                    WriteDumpFile(f, ADR(numModules), SIZE(numModules));
                END;

                ZeroMem(ADR(tInfo), SIZE(tInfo));
                tInfo.dwSize := SIZE(tInfo);
                currentThreadId := GetCurrentThreadId();
                IF Thread32First(snap, tInfo) THEN
                    REPEAT
                        IF tInfo.th32OwnerProcessID = ourProcessId THEN
                            IF tInfo.th32ThreadID <> currentThreadId THEN
                                IF numThreads >= maxRunningThreads THEN
                                    maxRunningThreads := maxRunningThreads + 256;
                                    runningThreads := HeapReAlloc(heap, 0,
                                                                  runningThreads,
                                                                  maxRunningThreads * SIZE(tThreadId));
                                END;
                                runningThreads^[numThreads] := tInfo.th32ThreadID;
                                INC(numThreads);
                            END;
                        END;
                    UNTIL NOT Thread32Next(snap, tInfo);

                   (*
                   NumThreads  : DWORD
                   ARRAY [0..NumThreads-1] OF RECORD
                       ThreadInfo
                   END
                   *)

                    WriteDumpFile(f, ADR(numThreads), SIZE(numThreads));
                    FOR i := 0 TO numThreads-1 DO
                        threadHandle := OpenThread(THREAD_ALL_ACCESS, FALSE, runningThreads^[i]);
                        SuspendThread(threadHandle);
                        ZeroMem(ADR(threadContext), SIZE(threadContext));
                        threadContext.ContextFlags := CONTEXT_FULL;
                        IF GetThreadContext(threadHandle, threadContext) THEN
                            AssignThreadContextToThreadInfo(threadInfo, threadContext, runningThreads^[i]);
                            WriteDumpFile(f, ADR(threadInfo), SIZE(threadInfo));
                        END;
                        CloseHandle(threadHandle);
                    END;
                    BREAK;
                ELSE
                    WriteDumpFile(f, ADR(numThreads), SIZE(numThreads));
                END;
            END;
            info.szExeFile := "";
            info.dwSize := SIZE(info);
        UNTIL NOT Process32Next(snap, info);
    END;
    CloseHandle(snap);

    IF NOT processedOurProcess THEN
        WriteDumpFile(f, ADR(numModules), SIZE(numModules));
        WriteDumpFile(f, ADR(numThreads), SIZE(numThreads));
    END;

   (*
   --- Memory Information ---
       NumBlocks        : DWORD;
       ARRAY [0..NumBlocks-1] OF
           {
            MemBlockAddress         : DWORD
            MemBlockSize            : DWORD
            MemBlockAttributes      : DWORD
            DiskLocOfBlock          : DWORD
           }
       MemDumpBlock0
       MemDumpBlock1
       ...
       MemDumpBlock.NumBlocks-1
   *)

    numMemBlocks := 0;
    address := MAKEADR(0);
    WHILE VirtualQuery(address, lpBuffer, SIZE(lpBuffer)) <> 0 DO
        IF ((lpBuffer.State BAND MEM_COMMIT) <> 0) AND ((lpBuffer.Protect BAND PAGE_GUARD) = 0) THEN
            IF numMemBlocks >= maxMemoryInfoItems THEN
                maxMemoryInfoItems := maxMemoryInfoItems + 256;
                memoryInfo := HeapReAlloc(heap, 0,
                                          memoryInfo, maxMemoryInfoItems * SIZE(tPmdMemoryInfoItem));
            END;
            memoryInfo^[numMemBlocks].BlockStartAddress := address;
            memoryInfo^[numMemBlocks].BlockSize := lpBuffer.RegionSize;
            memoryInfo^[numMemBlocks].Attributes := 0;
            INC(numMemBlocks);
        END;
        address := ADDADR(address, lpBuffer.RegionSize);
    END;
    WriteDumpFile(f, ADR(numMemBlocks), SIZE(numMemBlocks));
    IF numMemBlocks <> 0 THEN
        blockLoc := PMDFileWriteLocation +
                        (VAL(ADRCARD, SIZE(tPmdMemoryInfoItem)) * VAL(ADRCARD, numMemBlocks));
        FOR i := 0 TO numMemBlocks-1 DO
            memoryInfo^[i].DiskLoc := blockLoc;
            blockLoc := blockLoc + memoryInfo^[i].BlockSize;
        END;
        WriteDumpFile(f, ADR(memoryInfo^), SIZE(tPmdMemoryInfoItem) * numMemBlocks);
        FOR i := 0 TO numMemBlocks-1 DO
            WriteDumpFile(f, memoryInfo^[i].BlockStartAddress, memoryInfo^[i].BlockSize);
        END;
    END;
    CloseFile(f);

    HeapFree(heap, 0, memoryInfo);

    IF resumeThreads THEN
        FOR i := 0 TO numThreads-1 DO
            threadHandle := OpenThread(THREAD_ALL_ACCESS, FALSE, runningThreads^[i]);
            ResumeThread(threadHandle);
            CloseHandle(threadHandle);
        END;
    END;

    HeapFree(heap, 0, runningThreads);
END DumpPmdInfo;
%END

%IF UNIX %THEN
PROCEDURE DumpMemory(VAR memFile, dumpFile : File;  buf : ADDRESS; size : ADRCARD);
VAR
    <*/PUSH/NOCHECK:U*>
    localbuffer : ARRAY [0..4095] OF CHAR;
    <*/POP*>
BEGIN
    WHILE size <> 0 DO
        SetFilePos(memFile, CAST(ADRCARD, buf));
        IF size <= SIZE(localbuffer) THEN
            ReadBlock(memFile, ADR(localbuffer), size);
            WriteBlock(dumpFile, ADR(localbuffer), size);
            PMDFileWriteLocation := PMDFileWriteLocation + size;
            size := 0;
        ELSE
            ReadBlock(memFile, ADR(localbuffer), SIZE(localbuffer));
            WriteBlock(dumpFile, ADR(localbuffer), SIZE(localbuffer));
            PMDFileWriteLocation := PMDFileWriteLocation + SIZE(localbuffer);
            buf := ADDADR(buf, SIZE(localbuffer));
            size := size - SIZE(localbuffer);
        END;
    END;
END DumpMemory;
%END

%IF Linux %THEN
PROCEDURE DumpPmdInfo(VAR f : File; resumeThreads : BOOLEAN);
CONST
    cr = CHR(13);
    lf = CHR(10);
VAR
    mapsFile            : File;
    memFile             : File;
    start,
    end,
    devMajor,
    devMinor            : ADRCARD;
    offset              : ADRCARD;
    inode               : ADRCARD;
    name                : FileSpecString;
    attribs             : ARRAY [0..3] OF CHAR;
    numSharedObjects    : CARDINAL;
    soLoadAddr,
    soLoadSize          : ADRCARD;
    soName              : FileSpecString;
    moduleInfoIndex     : CARDINAL;
    numThreads          : CARDINAL32;
    moduleInfoAllocSize : CARDINAL;
    moduleInfo          : POINTER TO ARRAY [0..0] OF BYTE;

    numMemBlocks        : INTEGER32;
    memoryInfo          : POINTER TO ARRAY [0..0] OF tPmdMemoryInfoItem;
    maxMemoryInfoItems  : INTEGER;
    blockLoc            : ADRCARD;
    i                   : INTEGER;
    endOfLine           : BOOLEAN;

    PROCEDURE WriteSOBuffer(data : ARRAY OF BYTE);
    VAR
        size : CARDINAL;
    BEGIN
        size := HIGH(data) + 1;
        IF moduleInfoIndex + size >= moduleInfoAllocSize THEN
            moduleInfoAllocSize := moduleInfoAllocSize + (16*1024);
            realloc(moduleInfo, moduleInfoAllocSize);
        END;
        moduleInfo^[moduleInfoIndex..moduleInfoIndex+size-1] := data[0..size-1];
        moduleInfoIndex := moduleInfoIndex + size;
    END WriteSOBuffer;

    PROCEDURE AddSharedObject(name : ARRAY OF CHAR; start, end : ADRCARD);
    VAR
        soNameLength: CARDINAL32;
    BEGIN
        IF numSharedObjects <> 0 THEN
            IF (start = (soLoadAddr + soLoadSize)) AND Equal(soName, name) THEN
                soLoadSize := end - soLoadAddr;
                RETURN;
            END;
            WriteSOBuffer(soLoadAddr);
            WriteSOBuffer(soLoadSize);

            soNameLength := LENGTH(soName);
            WriteSOBuffer(soNameLength);
            WriteSOBuffer(soName[0..soNameLength]);
        END;
        IF start = MAX(CARDINAL) THEN
            RETURN;
        END;
        INC(numSharedObjects);
        soLoadAddr := start;
        soLoadSize := end-start;
        soName := name;
    END AddSharedObject;

    PROCEDURE GetNum(VAR num : ADRCARD; base : CARDINAL; allowLeadingCrLf : BOOLEAN) : BOOLEAN;
    VAR
        str     : ARRAY [0..63] OF CHAR;
        <*/PUSH/NOCHECK:U*>
        ch      : CHAR;
        <*/POP*>
        i       : CARDINAL;
    BEGIN
        num := 0;
        IF endOfLine THEN
            RETURN FALSE;
        END;
        LOOP
            ReadBlock(mapsFile, ADR(ch), SIZE(ch));
            IF (mapsFile.status <> 0) OR (mapsFile.count = 0) THEN
                RETURN FALSE;
            END;
            IF (ch = cr) OR (ch = lf) THEN
                (* allow parsing a cr or lf if at beginning of line - return error if at beginning of line *)
                IF NOT allowLeadingCrLf THEN
                    endOfLine := TRUE;
                    RETURN FALSE;
                END;
            ELSIF (ch <> " ") AND (ch <> CHR(9)) THEN
                EXIT;
            END;
        END;
        ch := CAP(ch);
        IF (ch < "0") OR (ch > "9") THEN
            IF NOT ((base = 16) AND (ch >= "A") AND (ch <= "F")) THEN
                RETURN FALSE;
            END;
        END;
        i := 0;
        LOOP
            str[i] := ch;
            INC(i);
            str[i] := 0C;
            ReadBlock(mapsFile, ADR(ch), SIZE(ch));
            IF (mapsFile.status <> 0) OR (mapsFile.count = 0) THEN
                endOfLine := TRUE;
                EXIT;
            END;
            ch := CAP(ch);
            IF (ch < "0") OR (ch > "9") THEN
                IF NOT ((base = 16) AND (ch >= "A") AND (ch <= "F")) THEN
                    endOfLine := (ch = cr) OR (ch = lf);
                    EXIT;
                END;
            END;
        END;
        IF base = 16 THEN
            %IF %NOT Bits64 %THEN
            IF LENGTH(str) > 8 THEN
                (* If 64bit entity, delete the leading digits, we can only deal with 32 bit addresses at this time *)
                Delete(str, 0, LENGTH(str)-8);
            END;
            %END
            RETURN StrBaseToNum(str, 16, num);
        ELSE
            RETURN StrBaseToNum(str, 10, num);
        END;
    END GetNum;

    PROCEDURE GetAttribs(VAR attribs : ARRAY OF CHAR) : BOOLEAN;
    VAR
        <*/PUSH/NOCHECK:U*>
        ch      : CHAR;
        <*/POP*>
        i       : CARDINAL;
    BEGIN
        IF endOfLine THEN
            RETURN FALSE;
        END;
        LOOP
            ReadBlock(mapsFile, ADR(ch), SIZE(ch));
            IF (mapsFile.status <> 0) OR (mapsFile.count = 0) THEN
                RETURN FALSE;
            END;
            IF (ch = cr) OR (ch = lf) THEN
                endOfLine := TRUE;
                RETURN FALSE;
            ELSIF (ch <> " ") AND (ch <> CHR(9)) THEN
                EXIT;
            END;
        END;
        attribs[0] := CAP(ch);
        FOR i := 1 TO 3 DO
            ReadBlock(mapsFile, ADR(ch), SIZE(ch));
            IF (mapsFile.status <> 0) OR (mapsFile.count = 0) THEN
                RETURN FALSE;
            END;
            IF (ch = cr) OR (ch = lf) THEN
                endOfLine := TRUE;
                RETURN FALSE;
            END;
            attribs[i] := CAP(ch);
        END;
        RETURN TRUE;
    END GetAttribs;

    PROCEDURE ReadSharedObjectName(VAR name : ARRAY OF CHAR) : BOOLEAN;
    CONST
        cr = CHR(13);
        lf = CHR(10);
    VAR
        i       : CARDINAL;
        <*/PUSH/NOCHECK:U*>
        ch      : CHAR;
        <*/POP*>
    BEGIN
        IF endOfLine THEN
            RETURN FALSE;
        END;
        (* Eat spaces and cr/lf *)
        LOOP
            ReadBlock(mapsFile, ADR(ch), SIZE(ch));
            IF (mapsFile.status <> 0) OR (mapsFile.count = 0) THEN
                RETURN FALSE;
            END;
            IF (ch = cr) OR (ch = lf) THEN
                RETURN FALSE;
            ELSIF (ch <> " ") AND (ch <> CHR(9)) THEN
                EXIT;
            END;
        END;

        i := 0;
        LOOP
            name[i] := ch;
            INC(i);
            ReadBlock(mapsFile, ADR(ch), SIZE(ch));
            IF (mapsFile.status <> 0) OR (mapsFile.count = 0) THEN
                EXIT;
            ELSIF (ch = cr) OR (ch = lf) THEN
                EXIT;
            END;
        END;
        name[i] := 0C;
        RETURN TRUE;
    END ReadSharedObjectName;

BEGIN
    UNREFERENCED_PARAMETER(resumeThreads);
    numMemBlocks := 0;
    numSharedObjects := 0;
    moduleInfoIndex := 0;
    numThreads := 0;

    maxMemoryInfoItems := 256;
    memoryInfo := malloc(maxMemoryInfoItems * SIZE(tPmdMemoryInfoItem));
    moduleInfoAllocSize := 1024*16;
    moduleInfo := malloc(moduleInfoAllocSize);

    OpenFileEx(mapsFile, "/proc/self/maps", ReadOnlyDenyNone, FileUseInfoSet{RandomAccess});
(*
    ReadBlock(mapsFile, ADR(moduleInfo^[0]), moduleInfoAllocSize);
    WriteDumpFile(f, ADR(moduleInfo^[0]), mapsFile.count);
    CloseFile(mapsFile);
    RETURN;
*)
    LOOP
        endOfLine := FALSE;
        IF GetNum(start, 16, TRUE) AND
           GetNum(end, 16, FALSE) AND
           GetAttribs(attribs) AND
           GetNum(offset, 16, FALSE) AND
           GetNum(devMajor, 16, FALSE) AND
           GetNum(devMinor, 16, FALSE) AND
           GetNum(inode, 10, FALSE)
        THEN
            IF ReadSharedObjectName(name) THEN
                AddSharedObject(name, start, end);
            END;
            IF attribs[0] = "R" THEN
                IF numMemBlocks >= maxMemoryInfoItems THEN
                    maxMemoryInfoItems := maxMemoryInfoItems + 256;
                    realloc(memoryInfo, maxMemoryInfoItems * SIZE(tPmdMemoryInfoItem));
                END;
                memoryInfo^[numMemBlocks].BlockStartAddress := CAST(ADDRESS, start);
                memoryInfo^[numMemBlocks].BlockSize := end-start;
                memoryInfo^[numMemBlocks].Attributes := 0;
                INC(numMemBlocks);
            END;
        ELSE
            EXIT;
        END;
    END;
    CloseFile(mapsFile);

    (* Output Shared Object information *)
    AddSharedObject("", MAX(CARDINAL), 0); (* flush buffer *)
    WriteDumpFile(f, ADR(numSharedObjects), SIZE(numSharedObjects));
    IF numSharedObjects <> 0 THEN
        WriteDumpFile(f, ADR(moduleInfo^[0]), moduleInfoIndex);
    END;
    (* Output Thread Information *)
    WriteDumpFile(f, ADR(numThreads), SIZE(numThreads)); (* No thread info *)

    WriteDumpFile(f, ADR(numMemBlocks), SIZE(numMemBlocks));

    OpenFileEx(memFile, "/proc/self/mem", ReadOnlyDenyNone, FileUseInfoSet{RandomAccess});
    IF numMemBlocks <> 0 THEN
        blockLoc := PMDFileWriteLocation + (SIZE(tPmdMemoryInfoItem) * VAL(ADRCARD, numMemBlocks));
        FOR i := 0 TO numMemBlocks-1 DO
            memoryInfo^[i].DiskLoc := blockLoc;
            blockLoc := blockLoc + memoryInfo^[i].BlockSize;
        END;
        WriteDumpFile(f, ADR(memoryInfo^), SIZE(tPmdMemoryInfoItem) * numMemBlocks);
        FOR i := 0 TO numMemBlocks-1 DO
            (*
              Need to use the /proc/mem method to write memory blocks because linux HUNG when attempting
              to access/write the memory blocks directly.
            *)
            DumpMemory(memFile, f, memoryInfo^[i].BlockStartAddress, memoryInfo^[i].BlockSize);
        END;
    END;
    CloseFile(memFile);
    CloseFile(f);

    free(moduleInfo);
    free(memoryInfo);
END DumpPmdInfo;
%END

PROCEDURE DoPMD(resumeThreads : BOOLEAN);
CONST
    %IF Bits32 %THEN
    Processor = 1;
    %ELSIF Bits64 %THEN
    Processor = 4;
    %END
    CurrentVersion = 1;
VAR
    f           : File;
    name        : FileSpecString;
    parts       : FileNameParts;
    %IF Windows %OR UNIX %THEN
    traceId     : ADRCARD;
    %ELSE
    traceId     : pthread_t;
    %END
    sig         : ARRAY [0..11] OF CHAR;
    processorId : CARDINAL8;
    version     : CARDINAL8;
    pmdOS       : CARDINAL8;
BEGIN
    %IF Windows %THEN
        traceId := GetCurrentThreadId();
        EnterCriticalSection(PMDSem);
    %ELSIF UNIX %THEN
        %IF MultiThreadLib %THEN
            traceId := pthread_self();
            pthread_mutex_lock(PMDSem);
        %ELSE
            traceId := 0;
        %END
    %ELSE
        traceId := COROUTINES.CURRENT();
    %END

    (*
       Signature - "ADW PMD"
       --- ID ---
       ProcessorId : DWORD (* 1 = IA32, 2 = SPARC32, 3 = PPC32 4=AMD64, 5=SPARC64 6=PPC64*)
       Version     : DWORD (* CurrentVersion = 1 *)
       OS          : DWORD (* 1 = WIN32, 2 = LINUX, 3 = SUNOS, 4 = AIX *)
       --- Current Location Info ---
           RegisterDump
       --- Module Load Information ---
           NumModules - DWORD
           ARRAY [0..NumModules-1] OF RECORD
                LoadAddress : DWORD
                LoadSize    : DWORD
                NameLength  : DWORD
                Name        : ARRAY [0..NameLength] OF CHAR (* short allocated, use NameLength to read *)
           END
       --- Process Information ---
           NumThreads  : DWORD
           ARRAY [0..NumThreads-1] OF RECORD
               ThreadInfo
           END
       --- Memory Information ---
           NumBlocks        : DWORD;
           ARRAY [0..NumBlocks-1] OF RECORD
               MemBlockAddress         : DWORD
               MemBlockSize            : DWORD
               MemBlockAttributes      : DWORD
               DiskLocOfBlock          : DWORD
           END
           MemDumpBlock0
           MemDumpBlock1
           ...
           MemDumpBlockN-1
    *)

    IF traceId = GlobalThreadInfo.ThreadId THEN
        PMDFileWriteLocation := 0;
        GetProgramName(name);
        ParseFileName(name, parts);
        IF PMDVersionStr[0] <> "" THEN
%IF %NOT UNICODE %THEN
            Append("-", parts.name);
            Append(PMDVersionStr, parts.name);
%END
        END;
        parts.extension := ".pmd";
        AssembleParts(parts, name);
        CreateFile(f, name);
        IF f.status = 0 THEN
            sig := "ADW PMD";
            WriteDumpFile(f, ADR(sig), SIZE(sig));
            processorId := Processor;
            version := CurrentVersion;
            %IF Windows %THEN
            pmdOS := 1;
            %ELSIF Linux %THEN
            pmdOS := 2;
            %ELSE
            fixme
            %END
            WriteDumpFile(f, ADR(processorId), SIZE(processorId));
            WriteDumpFile(f, ADR(version), SIZE(version));
            WriteDumpFile(f, ADR(pmdOS), SIZE(pmdOS));
            (*--- curent Location Info ---*)
            WriteDumpFile(f, ADR(GlobalThreadInfo), SIZE(GlobalThreadInfo));
            DumpPmdInfo(f, resumeThreads);
        END;
    END;

    %IF Windows %THEN
        LeaveCriticalSection(PMDSem);
    %ELSIF UNIX %THEN
        %IF MultiThreadLib %THEN
            pthread_mutex_unlock(PMDSem);
        %END
    %END
END DoPMD;

PROCEDURE NullPMD(resumeThreads : BOOLEAN);
BEGIN
    UNREFERENCED_PARAMETER(resumeThreads);
END NullPMD;

PROCEDURE EnableCallTrace(version : ARRAY OF CHAR);
BEGIN
    PMDVersionStr := version;
    PMD := DoPMD;
    PMDEnable := TRUE;
END EnableCallTrace;

PROCEDURE OutputCallTrace;
BEGIN
    PMD(TRUE);
END OutputCallTrace;

PROCEDURE OutputUserPMD;
BEGIN
    SaveRegs;
    PMD(TRUE);
END OutputUserPMD;

%IF UNIX %THEN

PROCEDURE TryPropagate(sig : INTEGER; info : siginfo_t; context : ucontext_t; oldAct : sigaction_t);

    PROCEDURE valid(addr : ADDRESS) : BOOLEAN;
    VAR
        db      : CARDINAL8;
    BEGIN
        db := addr^;
        RETURN TRUE;
    EXCEPT
        RETURN FALSE;
    END valid;

BEGIN
    IF (SA_SIGINFO BAND oldAct.sa_flags) <> 0 THEN
        IF valid(CAST(ADDRESS, oldAct.sa_sigaction)) THEN
            oldAct.sa_sigaction(sig, info, context);
        END;
    ELSE
        IF valid(CAST(ADDRESS, oldAct.sa_handler)) THEN
            oldAct.sa_handler(sig);
        END;
    END;

EXCEPT
    RETURN;
END TryPropagate;

PROCEDURE HandleSignal(sig : INTEGER; info : siginfo_t; context : ucontext_t) [OSCall, EXPORT, PROPAGATEEXCEPTION];
CONST
    %IF Bits32 %THEN
    IPReg = REG_EIP;
    BPReg = REG_EBP;
    %ELSIF Bits64 %THEN
    IPReg = REG_RIP;
    BPReg = REG_RBP;
    %END
VAR
    mask        : sigset_t;
    oldMask     : sigset_t;
    exf         : FramePtr;
    str         : ARRAY [0..63] OF CHAR;
    addr        : ADDRESS;
BEGIN
    sigemptyset(mask);
    sigaddset(mask, sig);
    %IF MultiThreadLib %THEN
        pthread_sigmask(SIG_UNBLOCK, mask, oldMask);
    %ELSE
        sigprocmask(SIG_UNBLOCK, mask, oldMask);
    %END

    addr := NIL;
    IF ADR(info) <> NIL THEN
        addr := info.si_addr;(*always is NIL, why?*)
        IF addr = NIL THEN
            addr := MAKEADR(context.uc_mcontext.gregs[IPReg]);
        END;
    END;

    IF PMDEnable AND (NOT SuppressRegisterSave) THEN
        AssignThreadContextToThreadInfo(GlobalThreadInfo, context.uc_mcontext.gregs, pthread_self());
    END;

    CASE sig OF
    SIGSEGV:
        str := "ACCESS_VIOLATION";
    |
    SIGBUS:
        str := "ALIGNMENT_VIOLATION";
    |
    SIGILL:
        str := "ILLEGAL_INSTRUCTION";
    |
    SIGFPE:
        str := "FLOAT_EXCEPTION";
    ELSE
        ConvertDec(sig, str);
    END;
    (*
    WriteRegs(GlobalThreadInfo);
    *)
    exf := GetActiveFrameEx(MAKEADR(context.uc_mcontext.gregs[BPReg]));
    IF exf <> NIL THEN
        exf^.state^.src := M2Except;
        exf^.state^.num := ORD(sysException);
        exf^.state^.mess := str;
        exf^.state^.addr := addr;
        exf^.state^.checking := SUBADR(exf^.handler, 6);
        ExecuteHandler(exf);
    ELSE
        IF (exf = NIL) AND DLL AND AccessViolationsTrapped THEN
            (* try to provide some way for a DLL and program to both handle
               these signals with their own handlers.
               this can only happen if the DLL is M2 and the program is something else.
               The M2 DLL must have called TrapAccessViolations.
            *)

            CASE sig OF
            SIGSEGV:
                TryPropagate(sig, info, context, Old_SIGSEGV);
            |
            SIGBUS:
                TryPropagate(sig, info, context, Old_SIGBUS);
            |
            SIGILL:
                TryPropagate(sig, info, context, Old_SIGILL);
            |
            SIGFPE:
                TryPropagate(sig, info, context, Old_SIGFPE);
            ELSE
            END;
        END;

        Unhandled(M2Except, ORD(sysException), str, addr, NIL);
    END;
END HandleSignal;

PROCEDURE TrapAccessViolations;
VAR
    action      : sigaction_t;
BEGIN
    IF NOT AccessViolationsTrapped THEN
        AccessViolationsTrapped := TRUE;

        action.sa_flags := SA_SIGINFO;
        action.sa_sigaction := HandleSignal;
        sigemptyset(action.sa_mask);
        sigaction(SIGSEGV, action, Old_SIGSEGV);

        action.sa_flags := SA_SIGINFO;
        action.sa_sigaction := HandleSignal;
        sigemptyset(action.sa_mask);
        sigaction(SIGBUS, action, Old_SIGBUS);

        action.sa_flags := SA_SIGINFO;
        action.sa_sigaction := HandleSignal;
        sigemptyset(action.sa_mask);
        sigaction(SIGILL, action, Old_SIGILL);

        action.sa_flags := SA_SIGINFO;
        action.sa_sigaction := HandleSignal;
        sigemptyset(action.sa_mask);
        sigaction(SIGFPE, action, Old_SIGFPE);
    END;
END TrapAccessViolations;

%END

PROCEDURE DoNothingUnhandled;
BEGIN
END DoNothingUnhandled;

PROCEDURE InitSystemEx;
%IF UNIX %AND MultiThreadLib %THEN
VAR
    attr        : pthread_mutexattr_t;
%END
BEGIN
    NextSource := MAKEADR(1);
    AllocateSource(M2Except);
    AllocateSource(M2OOExcept);

    UserUnhandledProc := DoNothingUnhandled;
    DebuggerAttach := DoNotAttach;
    ZeroMem(ADR(GlobalThreadInfo), SIZE(GlobalThreadInfo));

    %IF Windows %THEN
        InitializeCriticalSection(PMDSem);
        GlobalThreadInfo.ThreadId := GetCurrentThreadId();
    %ELSIF UNIX %THEN
        %IF MultiThreadLib %THEN
            pthread_mutexattr_init(attr);
            pthread_mutexattr_settype(attr, PTHREAD_MUTEX_RECURSIVE);
            pthread_mutex_init(PMDSem, attr);
            pthread_mutexattr_destroy(attr);

            GlobalThreadInfo.ThreadId := pthread_self();
        %ELSE
            GlobalThreadInfo.ThreadId := 0;
        %END
    %ELSE
        GlobalThreadInfo.ThreadId := COROUTINES.CURRENT();
    %END
    PMD := NullPMD;
    PMDEnable := FALSE;
    SuppressRegisterSave := FALSE;
    PMDVersionStr[0] := "";

    %IF UNIX %THEN
        AccessViolationsTrapped := FALSE;
        IF NOT DLL THEN
            TrapAccessViolations;
        END;
    %END
END InitSystemEx;

PROCEDURE DeinitSystemEx;
BEGIN
%IF UNIX %THEN
    %IF MultiThreadLib %THEN
        pthread_mutex_destroy(PMDSem);
    %END
%ELSIF WINDOWS %THEN
    DeleteCriticalSection(PMDSem);
%END
END DeinitSystemEx;

(* implementation of M2EXCEPTION *)

PROCEDURE M2Exception() : M2Exceptions;
BEGIN
    RETURN VAL(M2Exceptions, CurrentNumber(M2Except));
END M2Exception;

PROCEDURE IsM2Exception() : BOOLEAN;
BEGIN
    RETURN IsCurrentSource(M2Except);
END IsM2Exception;

PROCEDURE RaiseM2Exception(ex : M2Exceptions; mess : ARRAY OF CHAR) [NEVERRETURNS];
BEGIN
    Raise(M2Except, ORD(ex), mess);
END RaiseM2Exception;

(* runtime checking procedures *)

PROCEDURE RaiseCheck(src : ExceptionSource; ra : ADDRESS; num : ExceptionNumber; mess : ARRAY OF CHAR);
VAR
    pa          : POINTER TO ADDRESS;
    addr        : ADDRESS;
    checking    : ADDRESS;
    exf         : FramePtr;
    frame       : Frame;
    dw          : CARDINAL16;
BEGIN
    (* point to the return address of the error function *)
    (*
      AMD64 passes the return address in ra.
      IA32 needs to adjust by 4 since ADR of a phantom variable is taken and the RETURN address is
      4 bytes below the variable.
    *)
    %IF IA32 %THEN
        pa := SUBADR(ra, 4);
	%ELSE
	    <*/PUSH/NOWARN:G*>
    	ASM
			AND RSP, -16
		END;
	    <*/POP*>
	    pa := ra;
    %END

    addr := pa^;

    checking := addr;
    dw := ReadCodeWord(checking);
    IF dw = 06EBh THEN
        checking := ADDADR(checking, 2);
    END;

    (* adjust for the call *)

    addr := SUBADR(addr, 5);
    IF IsValidSource(src) THEN
        exf := GetActiveFrame(frame);
        IF exf <> NIL THEN
            %IF Windows %THEN
                RaiseWindows(src, num, mess, addr, checking);
            %ELSE
                exf^.state^.src := src;
                exf^.state^.num := num;
                exf^.state^.mess := mess;
                exf^.state^.addr := addr;
                exf^.state^.checking := checking;
                ExecuteHandler(exf);
            %END
        ELSE
            Terminate;
        END;
    ELSE
        Unhandled(src, num, mess, addr, checking);
    END;
END RaiseCheck;

PROCEDURE SaveRegs() [ALTERS()]; PUREASM;
%IF PICCode %THEN
CONST
   IntRegOffset = OFFS(tThreadInfo.IntegerRegisters);
   ThreadIdOffset = OFFS(tThreadInfo.ThreadId);
%END
ASM
    %IF Bits32 %THEN
    PUSH    EDX
    PUSH    EAX

    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[SegGs*4], GS
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[SegFs*4], FS
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[SegEs*4], ES
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[SegDs*4], DS
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[rEdi*4], EDI
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[rEsi*4], ESI
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[rEbx*4], EBX
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[rEdx*4], EDX
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[rEcx*4], ECX
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[rEax*4], EAX
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[SegCs*4], CS
    LAHF
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[EFlags*4], EAX
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[rEsp*4], ESP
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[SegSs*4], SS

    LEA     EAX, [EBP+8-4]   (* Get ADDRESS of Return address of calling proc *)
    MOV     EDX, [EAX]
    SUB     EDX, 5           (* adjust for the call *)
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[rEip*4], EDX

    SUB     EAX, 4           (* Get ADDRESS of Calling proc's EBP *)
    MOV     EDX, [EAX]
    MOV     DWORD PTR GlobalThreadInfo.IntegerRegisters[rEbp*4], EDX

    %IF Windows %THEN
    CALL    GetCurrentThreadId
    %ELSIF UNIX %THEN
    CALL    pthread_self
    %END
    MOV     DWORD PTR GlobalThreadInfo.ThreadId, EAX

    CALL    SetSafeSavedSP

    POP     EAX
    POP     EDX
    RET
    %ELSIF Bits64 %THEN
    PUSH    RBX
    PUSH    RDX
    PUSH    RAX

    %IF PICCode %THEN
    MOV     RDX, GOT GlobalThreadInfo
    ADD     RDX, IntRegOffset + GOTOFFS GlobalThreadInfo
    %ELSE
    MOV     RDX, OFFSET GlobalThreadInfo.IntegerRegisters
    %END
    MOV     QWORD PTR [RDX + rRdi*8], RDI
    MOV     QWORD PTR [RDX + rRsi*8], RSI
    MOV     QWORD PTR [RDX + rRbx*8], RBX
    MOV     QWORD PTR [RDX + rRax*8], RAX
    MOV     RAX, [RSP+8]      (* fetch the real RDX from stack *)
    MOV     QWORD PTR [RDX + rRdx*8], RAX
    MOV     QWORD PTR [RDX + rRcx*8], RCX

    MOV     QWORD PTR [RDX + rR8*8], R8
    MOV     QWORD PTR [RDX + rR9*8], R9
    MOV     QWORD PTR [RDX + rR10*8], R10
    MOV     QWORD PTR [RDX + rR11*8], R11
    MOV     QWORD PTR [RDX + rR12*8], R12
    MOV     QWORD PTR [RDX + rR14*8], R14
    MOV     QWORD PTR [RDX + rR15*8], R15
	%IF Windows %THEN
    MOV     RAX, [R13]	(* Saved by calling procedure R13 *)
    MOV     QWORD PTR [RDX + rR13*8], RAX
	%ELSE
    MOV     QWORD PTR [RDX + rR13*8], R13
	%END

    LAHF
    MOV     QWORD PTR [RDX + EFlags*8], RAX
    MOV     QWORD PTR [RDX + rRsp*8], RSP

    LEA     RAX, [RBP+16-8]   (* Get ADDRESS of Return address of calling proc *)
    MOV     RBX, [RAX]
    SUB     RBX, 5           (* adjust for the call *)
    MOV     QWORD PTR [RDX + rRip*8], RBX

    SUB     RAX, 8           (* Get ADDRESS of Calling proc's EBP *)
    MOV     RBX, [RAX]
    MOV     QWORD PTR [RDX + rRbp*8], RBX

    %IF Windows %THEN
    CALL    GetCurrentThreadId
    %ELSIF UNIX %THEN
    CALL    pthread_self
    %END
    %IF PICCode %THEN
    MOV     RDX, GOT GlobalThreadInfo
    ADD     RDX, ThreadIdOffset + GOTOFFS GlobalThreadInfo
    MOV     QWORD PTR [RDX], RAX
    %ELSE
    MOV     QWORD PTR GlobalThreadInfo.ThreadId, RAX
    %END

    CALL    SetSafeSavedSP

    POP     RAX
    POP     RDX
    POP     RBX
    RET
    %END
END SaveRegs;

%IF IA32 %THEN
PROCEDURE RANGEERROR(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    SaveRegs;
    RaiseCheck(M2Except, ADR(dummy), ORD(rangeException), "ASSIGN-RANGE");
END RANGEERROR;

PROCEDURE INDEXERROR(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    SaveRegs;
    RaiseCheck(M2Except, ADR(dummy), ORD(indexException), "INDEX-RANGE");
END INDEXERROR;

PROCEDURE OVERFLOW(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    SaveRegs;
    RaiseCheck(M2Except, ADR(dummy), ORD(wholeValueException), "WHOLE-OVERFLOW");
END OVERFLOW;

PROCEDURE NILPOINTER(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    SaveRegs;
    RaiseCheck(M2Except, ADR(dummy), ORD(invalidLocation), "NIL-DEREFERENCE");
END NILPOINTER;

PROCEDURE EMPTYCLASS(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    SaveRegs;
    RaiseCheck(M2OOExcept, ADR(dummy), ORD(emptyException), "EMPTY-CLASS-REFERENCE");
END EMPTYCLASS;

PROCEDURE NILPROCCALLED() [FRAME];
BEGIN
    RaiseM2Exception(invalidLocation, "NIL-PROCEDURE-CALLED");
END NILPROCCALLED;

PROCEDURE WRONGVARIANT(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    SaveRegs;
    RaiseCheck(M2Except, ADR(dummy), ORD(invalidLocation), "INVALID-VARIANT");
END WRONGVARIANT;

PROCEDURE DIVZERO(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
VAR
    sel : CARDINAL;
BEGIN
    <*/PUSH/NOWARN:G*>
    ASM
        MOVZX   EAX, AL
        MOV     sel, EAX
    END;
    <*/POP*>
    SaveRegs;
    CASE sel OF
    0:
        RaiseCheck(M2Except, ADR(dummy), ORD(wholeDivException), "WHOLE-ZERO-DIVISION");
    |
    1:
        RaiseCheck(M2Except, ADR(dummy), ORD(wholeDivException), "WHOLE-ZERO-REMAINDER");
    |
    2:
        RaiseCheck(M2Except, ADR(dummy), ORD(wholeDivException), "WHOLE-NONPOS-DIV");
    |
    3:
        RaiseCheck(M2Except, ADR(dummy), ORD(wholeDivException), "WHOLE-NONPOS-MOD");
    ELSE
    END;
END DIVZERO;

PROCEDURE CHECKREAL_X87(dummy : CARDINAL) [ALTERS(AX), LEAVES]; PUREASM;
CONST
    message     : ARRAY OF CHAR = {"ASSIGN-RANGE"};
    messHigh    = HIGH(message);
ASM
        SUB     ESP, 4
        FST     DWORD PTR [ESP]
        MOV     EAX, [ESP]
        ADD     ESP, 4
        AND     EAX, 7F800000h
        CMP     EAX, 7F800000h
        JE      @bad
        RET
    @bad:
        PUSH    EBP
        MOV     EBP, ESP
        CALL    SaveRegs
        POP     EBP

        PUSH    M2Except

        LEA     EAX, [ESP+8]
        PUSH    EAX

        MOV     EAX, realValueException
        PUSH    EAX

        MOV     EAX, messHigh
        PUSH    EAX
        MOV     EAX, OFFSET message
        PUSH    EAX

        CALL    RaiseCheck
END CHECKREAL_X87;

PROCEDURE CHECKLONGREAL_X87(dummy : CARDINAL) [ALTERS(AX), LEAVES]; PUREASM;
CONST
    message     : ARRAY OF CHAR = {"ASSIGN-RANGE"};
    messHigh    = HIGH(message);
ASM
        SUB     ESP, 8
        FST     QWORD PTR [ESP]
        MOV     EAX, DWORD PTR [ESP+4]
        ADD     ESP, 8
        AND     EAX, 7FF00000h
        CMP     EAX, 7FF00000h
        JE      @bad
        RET
    @bad:
        PUSH    EBP
        MOV     EBP, ESP
        CALL    SaveRegs
        POP     EBP

        PUSH    M2Except

        LEA     EAX, [ESP+8]
        PUSH    EAX

        MOV     EAX, realValueException
        PUSH    EAX

        MOV     EAX, messHigh
        PUSH    EAX
        MOV     EAX, OFFSET message
        PUSH    EAX

        CALL    RaiseCheck
END CHECKLONGREAL_X87;

PROCEDURE CASERANGE(dummy : CARDINAL) [ALTERS(AX,DX), LEAVES]; ASSEMBLER;
CONST
    message     : ARRAY OF CHAR = {"CASE-RANGE"};
    messHigh    = HIGH(message);
ASM
        MOV     EAX, M2Except
        PUSH    EAX

        MOV     EAX, caseSelectException
        PUSH    EAX

        MOV     EAX, messHigh
        PUSH    EAX
        MOV     EAX, OFFSET message
        PUSH    EAX

        (* Now push this procedures return address so that the error *)
        (* address will point to the actual code location of the error *)

        PUSH    DWORD PTR [EBP]+4

        JMP     Raise
END CASERANGE;

PROCEDURE GUARDEXCEPTION(dummy : CARDINAL) [ALTERS(AX,DX), LEAVES]; ASSEMBLER;
CONST
    message     : ARRAY OF CHAR = {"GUARD-EXCEPTION"};
    messHigh    = HIGH(message);
ASM
        MOV     EAX, M2OOExcept
        PUSH    EAX

        MOV     EAX, guardException
        PUSH    EAX

        MOV     EAX, messHigh
        PUSH    EAX
        MOV     EAX, OFFSET message
        PUSH    EAX

        (* Now push this procedures return address so that the error *)
        (* address will point to the actual code location of the error *)

        PUSH    DWORD PTR [EBP]+4

        JMP     Raise
END GUARDEXCEPTION;

%ELSIF AMD64 %THEN

PROCEDURE RANGEERROR(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    UNREFERENCED_PARAMETER(dummy);
    SaveRegs;
    RaiseCheck(M2Except, GetReturnAddr(), ORD(rangeException), "ASSIGN-RANGE");
END RANGEERROR;

PROCEDURE INDEXERROR(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    UNREFERENCED_PARAMETER(dummy);
    SaveRegs;
    RaiseCheck(M2Except, GetReturnAddr(), ORD(indexException), "INDEX-RANGE");
END INDEXERROR;

PROCEDURE OVERFLOW(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    UNREFERENCED_PARAMETER(dummy);
    SaveRegs;
    RaiseCheck(M2Except, GetReturnAddr(), ORD(wholeValueException), "WHOLE-OVERFLOW");
END OVERFLOW;

PROCEDURE NILPOINTER(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    UNREFERENCED_PARAMETER(dummy);
    SaveRegs;
    RaiseCheck(M2Except, GetReturnAddr(), ORD(invalidLocation), "NIL-DEREFERENCE");
END NILPOINTER;

PROCEDURE EMPTYCLASS(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    UNREFERENCED_PARAMETER(dummy);
    SaveRegs;
    RaiseCheck(M2OOExcept, GetReturnAddr(), ORD(emptyException), "EMPTY-CLASS-REFERENCE");
END EMPTYCLASS;

PROCEDURE NILPROCCALLED() [FRAME];
BEGIN
    RaiseM2Exception(invalidLocation, "NIL-PROCEDURE-CALLED");
END NILPROCCALLED;

PROCEDURE WRONGVARIANT(dummy : CARDINAL) [LEAVES, ALTERS(AX,DX)];
BEGIN
    UNREFERENCED_PARAMETER(dummy);
    SaveRegs;
    RaiseCheck(M2Except, GetReturnAddr(), ORD(invalidLocation), "INVALID-VARIANT");
END WRONGVARIANT;

PROCEDURE DIVZERO(dummy : CARDINAL);
VAR
    sel : CARDINAL;
BEGIN
    UNREFERENCED_PARAMETER(dummy);
    <*/PUSH/NOWARN:G*>
    ASM
        MOVZX   EAX, AL
        MOV     sel, EAX
    END;
    <*/POP*>
    SaveRegs;
    CASE sel OF
    0:
        RaiseCheck(M2Except, GetReturnAddr(), ORD(wholeDivException), "WHOLE-ZERO-DIVISION");
    |
    1:
        RaiseCheck(M2Except, GetReturnAddr(), ORD(wholeDivException), "WHOLE-ZERO-REMAINDER");
    |
    2:
        RaiseCheck(M2Except, GetReturnAddr(), ORD(wholeDivException), "WHOLE-NONPOS-DIV");
    |
    3:
        RaiseCheck(M2Except, GetReturnAddr(), ORD(wholeDivException), "WHOLE-NONPOS-MOD");
    ELSE
    END;
END DIVZERO;

PROCEDURE CHECKREAL_SSE(dummy : CARDINAL) [ALTERS(AX), LEAVES]; PUREASM;
CONST
    message     : ARRAY OF CHAR = {"ASSIGN-RANGE"};
    messHigh    = HIGH(message);
ASM
        MOVD    EAX, XMM0
        AND     EAX, 7F800000h
        CMP     EAX, 7F800000h
        JE      @bad
        RET
    @bad:
        PUSH    RBP
        MOV     RBP, RSP

    %IF Windows %THEN
		PUSH	R13
		MOV		R13, RSP
        CALL    SaveRegs
		POP		R13
        POP     RBP
		%IF PICCode %THEN
		MOV		RCX, GOT M2Except
		MOV		RCX, [RCX+GOTOFFS M2Except]
		%ELSE
        MOV     RCX, M2Except
		%END
        MOV     RDX, RSP       (* RSP points to the return address of this procedure *)
        MOV     R8D, realValueException
		%IF PICCode %THEN
		MOV		R9, GOT message
		ADD		R9, GOTOFFS message
		%ELSE
        MOV     R9, OFFSET message
		%END
        MOV     EAX, messHigh
        MOV     [RSP+40], RAX
    %ELSE
        CALL    SaveRegs
        POP     RBP
        MOV     RDI, GOT M2Except
        MOV     RDI, [RDI+GOTOFFS M2Except]
        MOV     RSI, RSP       (* RSP points to the return address of this procedure *)
        MOV     EDX, realValueException
        MOV     RCX, GOT message
        ADD     RCX, GOTOFFS message
        MOV     R8D, messHigh
    %END
        CALL    RaiseCheck
END CHECKREAL_SSE;

PROCEDURE CHECKLONGREAL_SSE(dummy : CARDINAL) [ALTERS(AX), LEAVES]; PUREASM;
CONST
    message     : ARRAY OF CHAR = {"ASSIGN-RANGE"};
    messHigh    = HIGH(message);
ASM
        MOVQ    RAX, XMM0
        SHR     RAX, 32
        AND     EAX, 7FF00000h
        CMP     EAX, 7FF00000h
        JE      @bad
        RET
    @bad:
        PUSH    RBP
        MOV     RBP, RSP

    %IF Windows %THEN
		PUSH	R13
		MOV		R13, RSP
        CALL    SaveRegs
		POP		R13
        POP     RBP
		%IF PICCode %THEN
		MOV		RCX, GOT M2Except
		MOV		RCX, [RCX+GOTOFFS M2Except]
		%ELSE
        MOV     RCX, M2Except
		%END
        MOV     RDX, RSP       (* RSP points to the return address of this procedure *)
        MOV     R8D, realValueException
		%IF PICCode %THEN
		MOV		R9, GOT message
		ADD		R9, GOTOFFS message
		%ELSE
        MOV     R9, OFFSET message
		%END
        MOV     EAX, messHigh
        MOV     [RSP+40], RAX
    %ELSE
        CALL    SaveRegs
        POP     RBP
        MOV     RDI, GOT M2Except
        MOV     RDI, [RDI+GOTOFFS M2Except]
        MOV     RSI, RSP       (* RSP points to the return address of this procedure *)
        MOV     EDX, realValueException
        MOV     RCX, GOT message
        ADD     RCX, GOTOFFS message
        MOV     R8D, messHigh
    %END
        CALL    RaiseCheck
END CHECKLONGREAL_SSE;

PROCEDURE CASERANGE(dummy : CARDINAL)
%IF Windows %THEN [ALTERS(AX,CX,DX,R8,R9), LEAVES] %ELSE [ALTERS(AX,CX,DX,SI,DI), LEAVES] %END;
PUREASM;
CONST
    message     : ARRAY OF CHAR = {"CASE-RANGE"};
    messHigh    = HIGH(message);
ASM
	%IF Windows %THEN
		%IF PICCode %THEN
        MOV     RCX, GOT M2Except
		MOV		RCX, [RCX+GOTOFFS M2Except]
        MOV     EDX, caseSelectException
        MOV     R8, GOT message
		ADD		R8, GOTOFFS message
		%ELSE
        MOV     RCX, M2Except
        MOV     EDX, caseSelectException
        MOV     R8, OFFSET message
		%END
        MOV     R9D, messHigh
	%ELSE
        MOV     RDI, GOT M2Except
        MOV     RDI, [RDI+GOTOFFS M2Except]
        MOV     ESI, caseSelectException
        MOV     RDX, GOT message
        ADD     RDX, GOTOFFS message
        MOV     ECX, messHigh
	%END
        JMP     Raise
END CASERANGE;

PROCEDURE GUARDEXCEPTION(dummy : CARDINAL)
%IF Windows %THEN [ALTERS(AX,CX,DX,R8,R9), LEAVES] %ELSE [ALTERS(AX,CX,DX,SI,DI), LEAVES] %END;
PUREASM;
CONST
    message     : ARRAY OF CHAR = {"GUARD-EXCEPTION"};
    messHigh    = HIGH(message);
ASM
	%IF Windows %THEN
		%IF PICCode %THEN
        MOV     RCX, GOT M2OOExcept
		MOV		RCX, [RCX+GOTOFFS M2OOExcept]
        MOV     EDX, caseSelectException
        MOV     R8, GOT message
		ADD		R8, GOTOFFS message
		%ELSE
        MOV     RCX, M2OOExcept
        MOV     EDX, caseSelectException
        MOV     R8, OFFSET message
		%END
        MOV     R9D, messHigh
	%ELSE
        MOV     RDI, GOT M2OOExcept
        MOV     RDI, [RDI+GOTOFFS M2OOExcept]
        MOV     ESI, caseSelectException
        MOV     RDX, GOT message
        ADD     RDX, GOTOFFS message
        MOV     ECX, messHigh
	%END
        JMP     Raise
END GUARDEXCEPTION;

%END

PROCEDURE M2OOException ["M2OOEXCEPTION_M2OOException"] () : M2OOExceptions;
BEGIN
    RETURN VAL(M2OOExceptions, CurrentNumber(M2OOExcept));
END M2OOException;

PROCEDURE IsM2OOException ["M2OOEXCEPTION_IsM2OOException"] () : BOOLEAN;
BEGIN
    RETURN IsCurrentSource(M2OOExcept);
END IsM2OOException;

PROCEDURE RaiseM2OOException ["M2OOEXCEPTION_RaiseM2OOException"] (ex : M2OOExceptions; mess : ARRAY OF CHAR) [NEVERRETURNS];
BEGIN
    Raise(M2OOExcept, ORD(ex), mess);
END RaiseM2OOException;

END SYSTEMEX.
