(***************************************************************************)
(*                                                                         *)
(*                       Copyright (C) 2009                                *)
(*                         by ADW Software                                 *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE SYSTEMM;
<*/NOOPT:X/OPT:T/INLINE:N*>

FROM SYSTEM IMPORT
    ADDRESS, BYTE, CAST, ADR, ADDADR, SUBADR, ADRCARD, FUNC, UNREFERENCED_PARAMETER, SWAPENDIAN, MAKEADR;

FROM SYSTEMEX IMPORT
    AllocateSource, ExceptionSource, IsCurrentSource, RaiseRTL,
    RaiseM2OOException, M2OOExceptions, RaiseM2Exception, M2Exceptions,
    InitSystemEx, DeinitSystemEx, NextSource;

FROM SYSTEMCO IMPORT
    InitSystemCo;

FROM SYSTEMTM IMPORT
    HaltCalled, Terminating, RunTerm, TermProc, TermProcType;

IMPORT MemUtils;

%IF WINDOWS %THEN

FROM WIN32 IMPORT
    HINSTANCE, ULONG,
	GetCommandLine, GetEnvironmentStrings, GetModuleHandleA, ExitProcess, SetThreadStackGuarantee,
    SetErrorMode, GetSystemInfo, FatalAppExit, SYSTEM_INFO,
    DLL_PROCESS_ATTACH, DLL_PROCESS_DETACH,
    DLL_THREAD_ATTACH, DLL_THREAD_DETACH,
    SEM_NOOPENFILEERRORBOX, SEM_FAILCRITICALERRORS,
    SEM_NOALIGNMENTFAULTEXCEPT;

FROM WINUSER IMPORT
    SW_SHOWDEFAULT, CharUpperBuffA, CharUpperW;

FROM WINX IMPORT
    Instance, PrevInstance, CmdShow, CmdLine,
    NIL_ASTR;

%ELSIF UNIX %THEN

FROM SYSTEM IMPORT FIXME;

FROM UNIX IMPORT
    sysconf, _SC_NPROCESSORS_ONLN, _SC_NPROCESSORS_CONF, exit, toupper, LC_ALL, setlocale,
    sigaction_t, sigaction,
    sigset_t, sigemptyset, sigaddset,
    SIGALRM, SIGPROF, SIGUSR1, SIGUSR2, SIGINT, SIGURG, SIGPIPE, SIGPOLL, SIGXFSZ, SIGWINCH,
    SIGXCPU, SIGRTMIN, SIGRTMAX,
    SIG_BLOCK, sigprocmask;

FROM UNIX_MAIN IMPORT
    main;
%END

%IF LINUX %THEN
FROM UNIX_MAIN IMPORT
    __libc_start_main;
%END

TYPE
    SetElement          = ADRCARD;
CONST
    SetElementSize      = SIZE(SetElement);
    SetElementBits      = SetElementSize * 8;

VAR
    SystemInitialized   : BOOLEAN = FALSE;

    AssertSrc           : ExceptionSource;

PROCEDURE TerminalTermProc;
BEGIN
    TermProc := NILPROC;
END TerminalTermProc;

%IF UNIX %THEN
PROCEDURE DoNothingSignalHandler(sig : INTEGER) [Cdecl, EXPORT];
BEGIN
    UNREFERENCED_PARAMETER(sig);
END DoNothingSignalHandler;

PROCEDURE DefaultSignalActions;
CONST
    block     : ARRAY [0..10] OF INTEGER =
        {SIGALRM, SIGPROF, SIGUSR1, SIGUSR2, SIGINT, SIGURG, SIGPIPE, SIGPOLL, SIGXFSZ, SIGXCPU, SIGWINCH};
VAR
    mask,
    oldMask     : sigset_t;
    i           : ADRCARD;
    sig         : INTEGER;
    action      : sigaction_t;
    null        [MAKEADR(0)] : sigaction_t;
BEGIN
    IF NOT DLL THEN
        (* eat the infamous pipe broken signal. you still get a error result from
           read/write.
           this method is fork friendly.
        *)
        action.sa_flags := 0;
        action.sa_handler := DoNothingSignalHandler;
        FUNC sigemptyset(action.sa_mask);
        FUNC sigaction(SIGPIPE, action, null);

        (* block some signals.
           the threads module should do the same for new threads.
        *)

        FUNC sigemptyset(mask);
        FOR i := 0 TO HIGH(block) DO
            FUNC sigaddset(mask, block[i]);
        END;
        FOR sig := SIGRTMIN() TO SIGRTMAX() DO
            FUNC sigaddset(mask, sig);
        END;
        FUNC sigprocmask(SIG_BLOCK, mask, oldMask);
    END;
END DefaultSignalActions;
%END

PROCEDURE GoodCPU () : BOOLEAN [Alters(AX,BX,CX,DX)]; PUREASM;
ASM
	(* First check if CPUID instruction is available *)
	%IF IA32 %THEN
		PUSHFD					(* Save EFLAGS *)
		POP		EAX
		MOV		ECX, EAX
		XOR		EAX, 200000H    (* Toggle ID flag *)
		PUSH	EAX
		POPFD
		PUSHFD
		POP		EAX
	%ELSE
		PUSHFQ					(* Save RFLAGS *)
		POP		RAX
		MOV		RCX, RAX
		XOR		RAX, 200000H    (* Toggle ID flag *)
		PUSH	RAX
		POPFQ
		PUSHFQ
		POP		RAX
	%END
	XOR		EAX, ECX
	JE		@ret
	XOR		EAX, EAX
	CPUID
	%IF AMD64 %AND PICCode %THEN
        MOV     RAX, GOT CPUVendorString
		MOV		[RAX + GOTOFFS CPUVendorString    ], EBX
		MOV		[RAX + GOTOFFS CPUVendorString + 4], EDX
		MOV		[RAX + GOTOFFS CPUVendorString + 8], ECX
	%ELSE
		MOV		dword ptr [CPUVendorString  ], EBX
		MOV		dword ptr [CPUVendorString+4], EDX
		MOV		dword ptr [CPUVendorString+8], ECX
	%END
	MOV		EAX, 1
	CPUID
	MOV		ECX, EAX
	SHR		ECX, 8
	AND		ECX, 0FH			(* Base Family *)
	MOV		EBX, EAX
	SHR		EBX, 20
	AND		EBX, 0FFH			(* Extended Family *)
	ADD		ECX, EBX
	%IF AMD64 %AND PICCode %THEN
        MOV     RBX, GOT CPUFamily
		MOV		[RBX + GOTOFFS CPUFamily], CL
	%ELSE
		MOV		CPUFamily, CL
	%END
	MOV		ECX, EAX
	SHR		ECX, 4
	AND		ECX, 0FH			(* Base Model *)
	MOV		EBX, EAX
	SHR		EBX, 12
	AND		EBX, 0F0H			(* Extended Model *)
	OR		ECX, EBX
	%IF AMD64 %AND PICCode %THEN
        MOV     RBX, GOT CPUModel
		MOV		[RBX + GOTOFFS CPUModel], CL
	%ELSE
		MOV		CPUModel, CL
	%END
	AND		EAX, 0FH
	%IF AMD64 %AND PICCode %THEN
        MOV     RBX, GOT CPUStepping
		MOV		[RBX + GOTOFFS CPUStepping], AL
	%ELSE
		MOV		CPUStepping, AL
	%END
	MOV		EAX, EDX			(* FPU presence bit in 0th bit *)
	%IF IA32 %THEN
		TEST	EAX, 4000000H	(* SSE2 presence bit *)
		JZ		@l1
		MOV		EDX, offset MemoryFence
		JMP		@com
@l1 :	TEST	EAX, 2000000H	(* SSE presence bit *)
		JZ		@l2
		MOV		EDX, offset OldMemoryFence
		JMP		@com
@l2 :	MOV		EDX, offset DummyMemoryFence
@com :	MOV		MFENCE, EDX
	%ELSE
		SHR		EAX, 26			(* shift SSE2 presence bit to 0th bit *)
	%END
	AND		AL, 1
@ret :
	RET
END GoodCPU;

PROCEDURE InitM2System;
VAR
%IF WINDOWS %THEN
    info        : SYSTEM_INFO;
%ELSIF UNIX %THEN
    count       : INTEGER;
%END
BEGIN
	(* Check if the CPU is good enough for ADW Modula-2 compiler *)
	IF ~ GoodCPU () THEN
		%IF WINDOWS %THEN
			IF DLL THEN
				FatalAppExit (0, "Called DLL cannot work on this computer, CPU is inappropriate.");
			ELSE
				FatalAppExit (0, "This application cannot work on this computer, CPU is inappropriate.");
			END;
	    %ELSIF UNIX %THEN
			FIXME ("Issue message for UNIX too");
    	    exit(254);
	    %ELSE
    	    fix me
    	%END
	END;
	(* Check known vendors *)
	IF MemUtils.CompMem (ADR(CPUVendorString), ADR("GenuineIntel"A), 12) = 12 THEN
		CPUVendor := Intel;
	ELSIF MemUtils.CompMem (ADR(CPUVendorString), ADR("AuthenticAMD"A), 12) = 12 THEN
		CPUVendor := AMD;
	END;
	MemUtils.Init;
    CPU_HYPERTHREAD := FALSE;

    %IF WINDOWS %THEN
        GetSystemInfo(info);
        CPUCOUNT := info.dwNumberOfProcessors;
    %ELSIF UNIX %THEN
        DefaultSignalActions;

        CPUCOUNT := 0;
        count := sysconf(_SC_NPROCESSORS_ONLN);
        IF count > 0 THEN
            CPUCOUNT := count;
        ELSE
            count := sysconf(_SC_NPROCESSORS_CONF);
            IF count > 0 THEN
                CPUCOUNT := count;
            END;
        END;
    %ELSE
        CPUCOUNT := 1;
    %END

    (* init the termination variables *)

    EXITCODE := 0;
    HaltCalled := FALSE;
    Terminating := FALSE;
    TermProc := TerminalTermProc;

    InitCapTable;

    (* see if the system has already been initialized *)
    (* this is only necessary when the RTL is used as a DLL *)
    (* and multiple M2 startup's exist. *)
    (* such as one DLL and a main program, both M2. *)

    IF NOT GetRtlInit() THEN
        (* init the coroutine system *)

        InitSystemCo;

        (* init the exception system *)

        InitSystemEx;
        AllocateSource(AssertSrc);

        (* user exceptions start at a given number. lets the debugger identify
           "system" exceptions that are raised.
        *)
        NextSource := MAKEADR(10);
    END;

    SystemInitialized := TRUE;
END InitM2System;

PROCEDURE DeinitM2System;
BEGIN
    DeinitSystemEx;
END DeinitM2System;

%IF UNIX %THEN
%IF IA32 %THEN
PROCEDURE _start ["_start"] (); PUREASM;
ASM
        (* ELF 386 ABI states
          [ESP]                 = argc
          [ESP+4]               = argv[0]
          ...
          [ESP+(4*argc)]        = NULL
          [ESP+(4*(argc+1)]     = envp[0]

          EDX - contains function pointer to register with atexit
        *)
        XOR     EBP, EBP

        MOV     EBX, ESP

        MOV     EAX, [EBX]
        MOV     CMD_COUNT, EAX
        LEA     EAX, [EBX+4]
        MOV     CMD, EAX

        LEA     EAX, [EBX+4]
        MOV     ECX, CMD_COUNT
        INC     ECX
        SHL     ECX, 2
        ADD     EAX, ECX
        MOV     ENV, EAX

        AND     ESP, -8

    %IF LINUX %THEN
        PUSH    ESP             (* stack end *)
        PUSH    EDX             (* passed rtl fini *)
        PUSH    NIL             (* fini *)
        PUSH    NIL             (* init *)
        PUSH    CMD             (* argv *)
        PUSH    CMD_COUNT       (* argc *)
        PUSH    OFFSET main     (* main program, libc will call us *)
        CALL    __libc_start_main
    %ELSE
        fix me
    %END

        (* crash if this somehow returns *)
        XOR     EAX, EAX
        MOV     EAX, [EAX]
END _start;
%ELSIF AMD64 %THEN
PROCEDURE _start ["_start"] (); PUREASM;
ASM
        (* ELF SYSTEM V AMD64 ABI states

          [RSP]                 = argc
          [RSP+8]               = argv[0]
          ...
          [RSP+(8*argc)]        = NULL
          [RSP+(8*(argc+1)]     = envp[0]

          RDX - contains function pointer to register with atexit
        *)
        XOR     RBP, RBP

        MOV     RBX, RSP

        MOV     EAX, [RBX]
        %IF PICCode %THEN
        MOV     R8, GOT CMD_COUNT
        MOV     DWORD PTR [R8+GOTOFFS CMD_COUNT], EAX
        %ELSE
        MOV     CMD_COUNT, EAX
        %END
        LEA     RAX, [RBX+8]
        %IF PICCode %THEN
        MOV     R9, GOT CMD
        MOV     QWORD PTR [R9+GOTOFFS CMD], RAX
        %ELSE
        MOV     CMD, RAX
        %END

        LEA     RAX, [RBX+8]
        %IF PICCode %THEN
        MOV     ECX, DWORD PTR [R8]
        %ELSE
        MOV     ECX, CMD_COUNT
        %END
        INC     ECX
        SHL     ECX, 3
        ADD     RAX, RCX
        %IF PICCode %THEN
        MOV     R10, GOT ENV
        MOV     QWORD PTR [R10+GOTOFFS ENV], RAX
        %ELSE
        MOV     ENV, RAX
        %END

        AND     RSP, -16

        %IF LINUX %THEN
        MOV     [RSP], RSP              (* stack end *)
        MOV     R9, RDX                 (* passed rtl fini *)
        MOV     R8, NIL                 (* fini *)
        MOV     RCX, NIL                (* init *)
        %IF PICCode %THEN
        MOV     RDX, GOT CMD
        MOV     RDX, [RDX+GOTOFFS CMD]  (* argv *)
        MOV     RSI, GOT CMD_COUNT
        MOV     ESI, [RSI+GOTOFFS CMD_COUNT](* argc *)
        MOV     RDI, GOT main
        %ELSE
        MOV     RDX, CMD                (* argv *)
        MOV     ESI, CMD_COUNT          (* argc *)
        MOV     RDI, OFFSET main        (* main program, libc will call us *)
        %END
        CALL    __libc_start_main
        %ELSE
            fix me
        %END
END _start;
%END
%END

%IF UNIX %THEN

PROCEDURE _fini ["_fini"] ();
BEGIN
    (* DLL termination *)
    Terminating := TRUE;
    EXITCODE := 0;
    RunTerm;
    DeinitM2System;
END _fini;

%END

PROCEDURE Start;
%IF WINDOWS %THEN
VAR
	Size : ULONG;
%END
BEGIN
    IsThread := FALSE;
    IsInit := TRUE;
    DLL := FALSE;

    %IF WINDOWS %THEN
        CMD := GetCommandLine();
        CmdLine := CMD;

        ENV := GetEnvironmentStrings();

        Instance := CAST(HINSTANCE, GetModuleHandleA(NIL_ASTR));
        PrevInstance := NIL;
        CmdShow := SW_SHOWDEFAULT;

        FUNC SetErrorMode (SEM_NOOPENFILEERRORBOX BOR SEM_FAILCRITICALERRORS BOR SEM_NOALIGNMENTFAULTEXCEPT);
		Size := 8*1024;
		FUNC SetThreadStackGuarantee (Size);
    %ELSIF UNIX %THEN
        (* handled in _start *)
    %ELSE
        fix me
    %END

    InitM2System;
END Start;

%IF WINDOWS %OR UNIX %THEN

%IF WINDOWS %THEN

PROCEDURE GetInst() : ADDRESS; PUREASM;
ASM
    %IF Bits32 %THEN
    MOV         EAX, [EBP]      (* get BP of main program *)
    MOV         EAX, [EAX+4+4](* startDLLret + BP save *)
    %ELSE
    MOV         RAX, RCX
    %END
    RET
END GetInst;

PROCEDURE GetReason() : CARDINAL32; PUREASM;
ASM
    %IF Bits32 %THEN
    MOV         EAX, [EBP]      (* get BP of main program *)
    MOV         EAX, [EAX+4+4+4](* startDLLret + BP save + hinst *)
    %ELSE
    MOV         RAX, RDX
    %END
    RET
END GetReason;

%END

PROCEDURE Start_DLL() [FRAME];
%IF WINDOWS %THEN
VAR
    reason      : CARDINAL;
    inst        : ADDRESS;
%END
BEGIN
    DLL := TRUE;

    %IF WINDOWS %THEN
        reason := GetReason();
        inst := GetInst();

        CASE reason OF
        DLL_PROCESS_ATTACH:
            IsThread := FALSE;
            IsInit := TRUE;

            CMD := GetCommandLine();
            CmdLine := CMD;

            ENV := GetEnvironmentStrings();

            Instance := CAST(HINSTANCE, inst);
            PrevInstance := NIL;
            CmdShow := SW_SHOWDEFAULT;

            InitM2System;
        |
        DLL_PROCESS_DETACH:
            IsThread := FALSE;
            IsInit := FALSE;
            DeinitM2System;
        |
        DLL_THREAD_ATTACH:
            IsThread := TRUE;
            IsInit := TRUE;
        |
        DLL_THREAD_DETACH:
            IsThread := TRUE;
            IsInit := FALSE;
        ELSE
        END

    %ELSIF UNIX %THEN
        IsThread := FALSE;
        IsInit := TRUE;
        CMD := NIL;
        ENV := NIL;
        CMD_COUNT := 0;
        InitM2System;

    %ELSE
        fix me
    %END
END Start_DLL;
%END

PROCEDURE Start_Cmain(retAddr : ADDRESS;
                      argc : CARDINAL;
                      argv : ADDRESS) [RightToLeft, LEAVES];
BEGIN
    IsInit := TRUE;
    IsThread := FALSE;
    DLL := FALSE;
    UNREFERENCED_PARAMETER(retAddr);

    %IF WINDOWS %THEN
        UNREFERENCED_PARAMETER(argc);
        UNREFERENCED_PARAMETER(argv);

        CMD := GetCommandLine();
        CmdLine := CMD;

        ENV := GetEnvironmentStrings();

        Instance := CAST(HINSTANCE, GetModuleHandleA(NIL_ASTR));
        PrevInstance := NIL;
        CmdShow := SW_SHOWDEFAULT;

        FUNC SetErrorMode(SEM_NOOPENFILEERRORBOX BOR
                          SEM_FAILCRITICALERRORS BOR
                          SEM_NOALIGNMENTFAULTEXCEPT);

    %ELSIF UNIX %THEN
        CMD_COUNT := argc;
        CMD := argv;
        ENV := NIL;
    %ELSE
        fix me
    %END

    InitM2System;
END Start_Cmain;

PROCEDURE HALTPROC(halter : TermProcType) : TermProcType;
VAR
    old :  TermProcType;
BEGIN
    old := TermProc;
    TermProc := halter;
    RETURN old;
END HALTPROC;

PROCEDURE SYSHALT(exitVal : CARDINAL);
BEGIN
    Terminating := FALSE;
    EXITCODE := exitVal;
    RunTerm;
    DeinitM2System;


    %IF WINDOWS %THEN
        ExitProcess(exitVal);
    %ELSIF UNIX %THEN
        exit(exitVal);
    %ELSE
        fix me
    %END

EXCEPT
  %IF WINDOWS %THEN
      ExitProcess(exitVal);
  %ELSIF UNIX %THEN
      exit(exitVal);
  %ELSE
      fix me
  %END
END SYSHALT;

PROCEDURE USERHALT(exitVal : CARDINAL);
BEGIN
    HaltCalled := TRUE;
    SYSHALT(exitVal);
END USERHALT;

%IF WINDOWS %THEN
PROCEDURE EmptyThread ["SYSTEMM_EmptyThread"] (param : ADDRESS) : CARDINAL32 [Windows];
BEGIN
	UNREFERENCED_PARAMETER (param);
	RETURN 0;
END EmptyThread;
%END

PROCEDURE REPEATBLOCK(addr : ADDRESS; size, count : CARDINAL)
                                    %IF IA32 %THEN
                                    [ALTERS(AX,DX)];
                                    %ELSE
                                    [LeftToRight];
                                    %END
VAR
    ptrS, ptrD  : POINTER TO BYTE;
    amount      : CARDINAL;
BEGIN
    ptrS := addr;
    ptrD := ADDADR(ptrS, size);
    LOOP
        IF count <> 0 THEN
            DEC(count);

            amount := size;
            LOOP
                IF amount <> 0 THEN
                    DEC(amount);
                    ptrD^ := ptrS^;
                    ptrD := ADDADR(ptrD, 1);
                    ptrS := ADDADR(ptrS, 1);

                    IF amount <> 0 THEN
                        DEC(amount);
                        ptrD^ := ptrS^;
                        ptrD := ADDADR(ptrD, 1);
                        ptrS := ADDADR(ptrS, 1);
                    ELSE
                        EXIT;
                    END;
                ELSE
                    EXIT;
                END;
            END;
        ELSE
            EXIT;
        END;
    END;
END REPEATBLOCK;

%IF IA32 %THEN

PROCEDURE LENGTH (str : ARRAY OF ACHAR) : CARDINAL [Pass(DX,DI),Alters(AX,CX,DX,DI),Returns(DX),Invariant]; PUREASM;
ASM
        CLD
        MOV     ECX, EDX
        INC     ECX
        XOR     AL, AL
        REPNE SCASB
        JE      @zero
        INC     EDX
        RET
@zero:  SUB     EDX, ECX
        RET
END LENGTH;

PROCEDURE ULENGTH (str : ARRAY OF UCHAR) : CARDINAL [Pass(DX,DI),Alters(AX,CX,DX,DI),Returns(DX),Invariant]; PUREASM;
ASM
        CLD
        MOV     ECX, EDX
        INC     ECX
        XOR     AX, AX
        REPNE SCASW
        JE      @zero
        INC     EDX
        RET
@zero:  SUB     EDX, ECX
		RET
END ULENGTH;

%ELSE

PROCEDURE LENGTH (str : ARRAY OF ACHAR) : CARDINAL [Pass(DI,DX),Alters(AX,CX,DX,DI),Returns(DX),Invariant]; PUREASM;
ASM
        CLD
        MOV     RCX, RDX
        INC     RCX
        XOR     AL, AL
        REPNE SCASB
        JE      @zero
        INC     RDX
        RET
@zero:  SUB     RDX, RCX
        RET
END LENGTH;

PROCEDURE ULENGTH (str : ARRAY OF UCHAR) : CARDINAL [Pass(DI,DX),Alters(AX,CX,DX,DI),Returns(DX),Invariant]; PUREASM;
ASM
        CLD
        MOV     RCX, RDX
        INC     RCX
        XOR     AX, AX
        REPNE SCASW
        JE      @zero
        INC     RDX
        RET
@zero:  SUB     RDX, RCX
        RET
END ULENGTH;

%END

VAR
    CapTable    : ARRAY [MIN(ACHAR)..MAX(ACHAR)] OF ACHAR;

PROCEDURE InitCapTable;

    PROCEDURE upcase(ch : ACHAR) : ACHAR;
    BEGIN
        %IF Windows %THEN
            IF CharUpperBuffA(ch, 1) <> 1 THEN
            END;
            RETURN ch;
        %ELSIF UNIX %THEN
            RETURN ACHR(toupper(ORD(ch)));
        %ELSE
            IF (ch >= 'a') AND (ch <= 'z') THEN
                DEC(ch, ORD('a')-ORD('A'));
            END;
            RETURN ch;
        %END
    END upcase;

VAR
    ch  : ACHAR;
BEGIN
%IF UNIX %THEN
    FUNC setlocale(LC_ALL, "");
%END

    FOR ch := MIN(ACHAR) TO MAX(ACHAR) DO
        CapTable[ch] := upcase(ch);
    END;
END InitCapTable;

PROCEDURE CAP(ch : ACHAR) : ACHAR [Invariant];
BEGIN
    RETURN CapTable[ch];
END CAP;

PROCEDURE UCAP(ch : UCHAR) : UCHAR [Invariant];
BEGIN
    %IF Windows %THEN
        RETURN CharUpperW(ORD(ch));
    %ELSE
		IF ch <= UCHR(127) THEN
			RETURN CapTable[VAL(ACHAR,ch)];
		ELSE
        	RETURN ch;
		END;
    %END
END UCAP;

PROCEDURE SETOR(SET1, SET2, RESULTSET : ADDRESS; LEN : CARDINAL)
                                        %IF IA32 %THEN
                                        [ALTERS(AX)];
                                        %ELSE
                                        [LeftToRight];
                                        %END
VAR
    ptrS1, ptrS2, ptrD  : POINTER TO ARRAY [0..3] OF SetElement;
    count               : CARDINAL;
BEGIN
    count := LEN;
    ptrD := RESULTSET;
    ptrS1 := SET1;
    ptrS2 := SET2;
    LOOP
        IF count >= 4 THEN
            count := count - 4;
            ptrD^[0] := ptrS1^[0] BOR ptrS2^[0];
            ptrD^[1] := ptrS1^[1] BOR ptrS2^[1];
            ptrD^[2] := ptrS1^[2] BOR ptrS2^[2];
            ptrD^[3] := ptrS1^[3] BOR ptrS2^[3];
            ptrS1 := ADDADR(ptrS1, 4*SetElementSize);
            ptrS2 := ADDADR(ptrS2, 4*SetElementSize);
            ptrD := ADDADR(ptrD, 4*SetElementSize);
            IF count = 0 THEN
                EXIT;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            ptrD^[0] := ptrS1^[0] BOR ptrS2^[0];
            ptrD^[1] := ptrS1^[1] BOR ptrS2^[1];
            ptrS1 := ADDADR(ptrS1, 2*SetElementSize);
            ptrS2 := ADDADR(ptrS2, 2*SetElementSize);
            ptrD := ADDADR(ptrD, 2*SetElementSize);
            IF count = 0 THEN
                EXIT;
            END;
        ELSE
            ptrD^[0] := ptrS1^[0] BOR ptrS2^[0];
            EXIT;
        END;
    END;
END SETOR;

PROCEDURE SETAND(SET1, SET2, RESULTSET : ADDRESS; LEN : CARDINAL)
                                        %IF IA32 %THEN
                                        [ALTERS(AX)];
                                        %ELSE
                                        [LeftToRight];
                                        %END
VAR
    ptrS1, ptrS2, ptrD  : POINTER TO ARRAY [0..3] OF SetElement;
    count               : CARDINAL;
BEGIN
    count := LEN;
    ptrD := RESULTSET;
    ptrS1 := SET1;
    ptrS2 := SET2;
    LOOP
        IF count >= 4 THEN
            count := count - 4;
            ptrD^[0] := ptrS1^[0] BAND ptrS2^[0];
            ptrD^[1] := ptrS1^[1] BAND ptrS2^[1];
            ptrD^[2] := ptrS1^[2] BAND ptrS2^[2];
            ptrD^[3] := ptrS1^[3] BAND ptrS2^[3];
            ptrS1 := ADDADR(ptrS1, 4*SetElementSize);
            ptrS2 := ADDADR(ptrS2, 4*SetElementSize);
            ptrD := ADDADR(ptrD, 4*SetElementSize);
            IF count = 0 THEN
                EXIT;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            ptrD^[0] := ptrS1^[0] BAND ptrS2^[0];
            ptrD^[1] := ptrS1^[1] BAND ptrS2^[1];
            ptrS1 := ADDADR(ptrS1, 2*SetElementSize);
            ptrS2 := ADDADR(ptrS2, 2*SetElementSize);
            ptrD := ADDADR(ptrD, 2*SetElementSize);
            IF count = 0 THEN
                EXIT;
            END;
        ELSE
            ptrD^[0] := ptrS1^[0] BAND ptrS2^[0];
            EXIT;
        END;
    END;
END SETAND;

PROCEDURE SETXOR(SET1, SET2, RESULTSET : ADDRESS; LEN : CARDINAL)
                                        %IF IA32 %THEN
                                        [ALTERS(AX)];
                                        %ELSE
                                        [LeftToRight];
                                        %END
VAR
    ptrS1, ptrS2, ptrD  : POINTER TO ARRAY [0..3] OF SetElement;
    count               : CARDINAL;
BEGIN
    count := LEN;
    ptrD := RESULTSET;
    ptrS1 := SET1;
    ptrS2 := SET2;
    LOOP
        IF count >= 4 THEN
            count := count - 4;
            ptrD^[0] := ptrS1^[0] BXOR ptrS2^[0];
            ptrD^[1] := ptrS1^[1] BXOR ptrS2^[1];
            ptrD^[2] := ptrS1^[2] BXOR ptrS2^[2];
            ptrD^[3] := ptrS1^[3] BXOR ptrS2^[3];
            ptrS1 := ADDADR(ptrS1, 4*SetElementSize);
            ptrS2 := ADDADR(ptrS2, 4*SetElementSize);
            ptrD := ADDADR(ptrD, 4*SetElementSize);
            IF count = 0 THEN
                EXIT;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            ptrD^[0] := ptrS1^[0] BXOR ptrS2^[0];
            ptrD^[1] := ptrS1^[1] BXOR ptrS2^[1];
            ptrS1 := ADDADR(ptrS1, 2*SetElementSize);
            ptrS2 := ADDADR(ptrS2, 2*SetElementSize);
            ptrD := ADDADR(ptrD, 2*SetElementSize);
            IF count = 0 THEN
                EXIT;
            END;
        ELSE
            ptrD^[0] := ptrS1^[0] BXOR ptrS2^[0];
            EXIT;
        END;
    END;
END SETXOR;

PROCEDURE SETANDNOT(SET1, SET2, RESULTSET : ADDRESS; LEN : CARDINAL)
                                        %IF IA32 %THEN
                                        [ALTERS(AX)];
                                        %ELSE
                                        [LeftToRight];
                                        %END
VAR
    ptrS1, ptrS2, ptrD  : POINTER TO ARRAY [0..3] OF SetElement;
    count               : CARDINAL;
BEGIN
    count := LEN;
    ptrD := RESULTSET;
    ptrS1 := SET1;
    ptrS2 := SET2;
    LOOP
        IF count >= 4 THEN
            count := count - 4;
            ptrD^[0] := ptrS1^[0] BAND (BNOT ptrS2^[0]);
            ptrD^[1] := ptrS1^[1] BAND (BNOT ptrS2^[1]);
            ptrD^[2] := ptrS1^[2] BAND (BNOT ptrS2^[2]);
            ptrD^[3] := ptrS1^[3] BAND (BNOT ptrS2^[3]);
            ptrS1 := ADDADR(ptrS1, 4*SetElementSize);
            ptrS2 := ADDADR(ptrS2, 4*SetElementSize);
            ptrD := ADDADR(ptrD, 4*SetElementSize);
            IF count = 0 THEN
                EXIT;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            ptrD^[0] := ptrS1^[0] BAND (BNOT ptrS2^[0]);
            ptrD^[1] := ptrS1^[1] BAND (BNOT ptrS2^[1]);
            ptrS1 := ADDADR(ptrS1, 2*SetElementSize);
            ptrS2 := ADDADR(ptrS2, 2*SetElementSize);
            ptrD := ADDADR(ptrD, 2*SetElementSize);
            IF count = 0 THEN
                EXIT;
            END;
        ELSE
            ptrD^[0] := ptrS1^[0] BAND (BNOT ptrS2^[0]);
            EXIT;
        END;
    END;
END SETANDNOT;

PROCEDURE SETGEQ(LSET, RSET : ADDRESS; LEN : CARDINAL) : BOOLEAN
                                        %IF IA32 %THEN
                                        [ALTERS(AX)];
                                        %ELSE
                                        [LeftToRight];
                                        %END
VAR
    ptrL, ptrR  : POINTER TO ARRAY [0..3] OF SetElement;
    count       : CARDINAL;
BEGIN
    (* due to how the compiler handles branches *)
    (* this call should return an inverse boolean result *)

    ptrL := LSET;
    ptrR := RSET;
    count := LEN;
    LOOP
        IF count >= 4 THEN
            count := count - 4;
            IF ((ptrR^[0] BAND (BNOT ptrL^[0])) = 0) AND
               ((ptrR^[1] BAND (BNOT ptrL^[1])) = 0) AND
               ((ptrR^[2] BAND (BNOT ptrL^[2])) = 0) AND
               ((ptrR^[3] BAND (BNOT ptrL^[3])) = 0)
            THEN
                ptrL := ADDADR(ptrL, 4*SetElementSize);
                ptrR := ADDADR(ptrR, 4*SetElementSize);
                IF count = 0 THEN
                    RETURN FALSE;
                END;
            ELSE
                RETURN TRUE;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            IF ((ptrR^[0] BAND (BNOT ptrL^[0])) = 0) AND
               ((ptrR^[1] BAND (BNOT ptrL^[1])) = 0)
            THEN
                ptrL := ADDADR(ptrL, 2*SetElementSize);
                ptrR := ADDADR(ptrR, 2*SetElementSize);
                IF count = 0 THEN
                    RETURN FALSE;
                END;
            ELSE
                RETURN TRUE;
            END;
        ELSE
            RETURN (ptrR^[0] BAND (BNOT ptrL^[0])) <> 0;
        END;
    END;
END SETGEQ;

PROCEDURE SETEQL(LSET, RSET : ADDRESS; LEN : CARDINAL) : BOOLEAN
                                        %IF IA32 %THEN
                                        [ALTERS(AX)];
                                        %ELSE
                                        [LeftToRight];
                                        %END
VAR
    ptrL, ptrR  : POINTER TO ARRAY [0..3] OF SetElement;
    count       : CARDINAL;
BEGIN
    (* due to how the compiler handles branches *)
    (* this call should return an inverse boolean result *)

    ptrL := LSET;
    ptrR := RSET;
    count := LEN;
    LOOP
        IF count >= 4 THEN
            count := count - 4;
            IF (ptrL^[0] = ptrR^[0]) AND
               (ptrL^[1] = ptrR^[1]) AND
               (ptrL^[2] = ptrR^[2]) AND
               (ptrL^[3] = ptrR^[3])
            THEN
                ptrL := ADDADR(ptrL, 4*SetElementSize);
                ptrR := ADDADR(ptrR, 4*SetElementSize);
                IF count = 0 THEN
                    RETURN FALSE;
                END;
            ELSE
                RETURN TRUE;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            IF (ptrL^[0] = ptrR^[0]) AND (ptrL^[1] = ptrR^[1]) THEN
                ptrL := ADDADR(ptrL, 2*SetElementSize);
                ptrR := ADDADR(ptrR, 2*SetElementSize);
                IF count = 0 THEN
                    RETURN FALSE;
                END;
            ELSE
                RETURN TRUE;
            END;
        ELSE
            RETURN ptrL^[0] <> ptrR^[0];
        END;
    END;
END SETEQL;

PROCEDURE SETSHIFT(SRC : ADDRESS; shift : INTEGER; RESULT : ADDRESS; LenIn : CARDINAL)
                                        %IF IA32 %THEN
                                        [ALTERS(AX)];
                                        %ELSE
                                        [LeftToRight];
                                        %END
VAR
    ptr, ptrS   : POINTER TO ARRAY [0..0] OF SetElement;
    bits        : CARDINAL;
    words       : ADRCARD;
    i, j        : ADRCARD;
    LEN         : ADRCARD;
    temp        : SetElement;
    count       : CARDINAL;
BEGIN
    (* copy the source to result if they differ *)

    LEN := LenIn;
    IF SRC <> RESULT THEN
        count := LEN;
        ptrS := SRC;
        ptr := RESULT;
        LOOP
            IF count >= 4 THEN
                count := count - 4;
                ptr^[0] := ptrS^[0];
                ptr^[1] := ptrS^[1];
                ptr^[2] := ptrS^[2];
                ptr^[3] := ptrS^[3];
                ptr := ADDADR(ptr, 4*SetElementSize);
                ptrS := ADDADR(ptrS, 4*SetElementSize);
                IF count = 0 THEN
                    EXIT;
                END;
            ELSIF count >= 2 THEN
                count := count - 2;
                ptr^[0] := ptrS^[0];
                ptr^[1] := ptrS^[1];
                ptr := ADDADR(ptr, 2*SetElementSize);
                ptrS := ADDADR(ptrS, 2*SetElementSize);
                IF count = 0 THEN
                    EXIT;
                END;
            ELSE
                ptr^[0] := ptrS^[0];
                EXIT;
            END;
        END;
    END;

    ptr := RESULT;

    IF shift > 0 THEN
        (* shift words *)

        words := ORD(shift) / SetElementBits;
        IF words <> 0 THEN
            IF words < LEN THEN
                i := LEN - words;
                j := i + words;
                REPEAT
                    DEC(i);
                    DEC(j);
                    ptr^[j] := ptr^[i];
                UNTIL i = 0;
            ELSE
                words := LEN;
            END;
            FOR i := 0 TO words-1 DO
                ptr^[i] := 0;
            END;
        END;

        (* shift bits *)

        bits := ORD(shift) REM SetElementBits;
        IF bits <> 0 THEN
            temp := SetElementBits - bits;
            IF LEN > 1 THEN
                j := LEN - 2;
                FOR i := LEN-1 TO 1 BY -1 DO
                    ptr^[i] := (ptr^[i] SHL bits) BOR (ptr^[j] SHR temp);
                    DEC(j);
                END;
            END;
            ptr^[0] := ptr^[0] SHL bits;
        END;

    ELSIF shift < 0 THEN
        shift := -shift;

        (* shift words *)

        words := ORD(shift) / SetElementBits;
        IF words <> 0 THEN
            IF words < LEN THEN
                j := words;
                FOR i := 0 TO LEN-words-1 DO
                    ptr^[i] := ptr^[j];
                    INC(j);
                END;
            ELSE
                words := LEN;
            END;
            FOR i := LEN-words TO LEN-1 DO
                ptr^[i] := 0;
            END;
        END;

        (* shift bits *)

        bits := ORD(shift) REM SetElementBits;
        IF bits <> 0 THEN
            IF LEN > 1 THEN
                temp := SetElementBits - bits;
                j := 1;
                FOR i := 0 TO LEN-2 DO
                    ptr^[i] := (ptr^[i] SHR bits) BOR (ptr^[j] SHL temp);
                    INC(j);
                END;
            END;
            ptr^[LEN-1] := ptr^[LEN-1] SHR bits;
        END;
    END;
END SETSHIFT;

PROCEDURE SETROTATE(SRC : ADDRESS; shift : INTEGER; RESULT : ADDRESS; LEN : CARDINAL)
                                        %IF IA32 %THEN
                                        [ALTERS(AX)];
                                        %ELSE
                                        [LeftToRight];
                                        %END
VAR
    ptrC, ptrCs : POINTER TO ARRAY [0..1] OF SetElement;
    count       : CARDINAL;
    numShift    : CARDINAL;
    carry, bit  : SetElement;
BEGIN
    (* copy the source to result if they differ *)

    IF SRC <> RESULT THEN
        ptrCs := SRC;
        ptrC := RESULT;
        count := LEN;
        LOOP
            IF count <> 0 THEN
                DEC(count);
                ptrC^[0] := ptrCs^[0];
                ptrC := ADDADR(ptrC, SetElementSize);
                ptrCs := ADDADR(ptrCs, SetElementSize);
            ELSE
                EXIT;
            END;
        END;
    END;

    IF shift > 0 THEN
        (* rotate whole words left *)

        numShift := shift / SetElementBits;
        LOOP
            IF numShift <> 0 THEN
                DEC(numShift);

                count := LEN-1;
                ptrC := RESULT;
                ptrC := ADDADR(RESULT, (LEN-2)*SetElementSize);
                carry := ptrC^[1];
                LOOP
                    IF count <> 0 THEN
                        DEC(count);
                        ptrC^[1] := ptrC^[0];
                        ptrC := SUBADR(ptrC, SetElementSize);
                    ELSE
                        EXIT;
                    END;
                END;
                ptrC := RESULT;
                ptrC^[0] := carry;
            ELSE
                EXIT;
            END;
        END;

        (* rotate bits left *)

        numShift := shift REM SetElementBits;
        LOOP
            IF numShift <> 0 THEN
                DEC(numShift);

                count := LEN;
                ptrC := ADDADR(RESULT, (LEN-1)*SetElementSize);
                carry := ptrC^[0] SHR (SetElementBits-1);
                ptrC := RESULT;
                LOOP
                    IF count <> 0 THEN
                        DEC(count);
                        bit := ptrC^[0] SHR (SetElementBits-1);
                        ptrC^[0] := (ptrC^[0] SHL 1) BOR carry;
                        ptrC := ADDADR(ptrC, SetElementSize);
                        carry := bit;
                    ELSE
                        EXIT;
                    END;
                END;
            ELSE
                EXIT;
            END;
        END;

    ELSIF shift < 0 THEN
        shift := -shift;

        (* shift whole words right *)

        numShift := shift / SetElementBits;
        LOOP
            IF numShift <> 0 THEN
                DEC(numShift);

                count := LEN-1;
                ptrC := RESULT;
                carry := ptrC^[0];
                LOOP
                    IF count <> 0 THEN
                        DEC(count);
                        ptrC^[0] := ptrC^[1];
                        ptrC := ADDADR(ptrC, SetElementSize);
                    ELSE
                        EXIT;
                    END;
                END;
                ptrC := ADDADR(RESULT, (LEN-1)*SetElementSize);
                ptrC^[0] := carry;
            ELSE
                EXIT;
            END;
        END;

        (* rotate bits right *)

        numShift := shift REM SetElementBits;
        LOOP
            IF numShift <> 0 THEN
                DEC(numShift);

                count := LEN;
                ptrC := RESULT;
                carry := (ptrC^[0] BAND 1) SHL (SetElementBits-1);
                ptrC := ADDADR(RESULT, (LEN-1)*SetElementSize);
                LOOP
                    IF count <> 0 THEN
                        DEC(count);
                        bit := ptrC^[0] BAND 1;
                        ptrC^[0] := (ptrC^[0] SHR 1) BOR carry;
                        ptrC := SUBADR(ptrC, SetElementSize);
                        carry := bit SHL (SetElementBits-1);
                    ELSE
                        EXIT;
                    END;
                END;
            ELSE
                EXIT;
            END;
        END;
    END;
END SETROTATE;

PROCEDURE REGSETBITS(BIT1, BIT2 : CARDINAL) : ADRCARD
                                        %IF IA32 %THEN
                                        [ALTERS(AX)];
                                        %ELSIF AMD64 %THEN
                                        [LeftToRight];
                                        %END
CONST
    maxSet      = (SIZE(ADRCARD)*8)-1;
TYPE
    adrCardSet  = PACKEDSET OF [0..maxSet];
VAR
    set : adrCardSet;
    i   : CARDINAL;
BEGIN
    set := adrCardSet{};
    IF BIT1 <= BIT2 THEN
        IF (BIT1 <= maxSet) AND (BIT2 <= maxSet) THEN
            FOR i := BIT1 TO BIT2 DO
                INCL(set, i);
            END;
        ELSE
            RaiseM2Exception(rangeException, "Runtime SET Bits range, value out of range");
        END;
    ELSE
        RaiseM2Exception(rangeException, "Runtime SET Bits range, lower value > upper");
    END;
    RETURN CAST(ADRCARD, set);;
END REGSETBITS;

PROCEDURE SETBITS(BIT1, BIT2 : CARDINAL; RESULT : ADDRESS; LEN : CARDINAL)
                                        %IF IA32 %THEN
                                        [ALTERS(AX)];
                                        %ELSE
                                        [LeftToRight];
                                        %END
VAR
    ptrC        : POINTER TO SetElement;
    count       : CARDINAL;
    numZ        : CARDINAL;
    temp        : SetElement;
    numBits     : CARDINAL;
BEGIN
    ptrC := RESULT;
    numBits := BIT2-BIT1+1;
    numZ := BIT1 REM SetElementBits;

    (* number of words to fill with zeros *)

    count := BIT1 / SetElementBits;
    WHILE count <> 0 DO
        DEC(LEN);
        DEC(count);
        ptrC^ := 0;
        ptrC := ADDADR(ptrC, SetElementSize);
    END;

    IF numZ+numBits <= SetElementBits THEN
        (* the 1's fit within a single word *)

        <*/PUSH/NOWARN:U*>
        temp := VAL(SetElement, MAX(SetElement)) SHL numZ;(* the low zero bits *)
        <*/POP*>
        temp := temp SHL (SetElementBits-numBits-numZ) SHR (SetElementBits-numBits-numZ);
        ptrC^ := temp;
        ptrC := ADDADR(ptrC, SetElementSize);
        DEC(LEN);
    ELSE
        <*/PUSH/NOWARN:U*>
        ptrC^ := VAL(SetElement, MAX(SetElement)) SHL numZ;
        <*/POP*>
        ptrC := ADDADR(ptrC, SetElementSize);
        numBits := numBits - (SetElementBits-numZ);
        DEC(LEN);

        (* fill the whole word 1's *)

        WHILE numBits >= SetElementBits DO
            numBits := numBits - SetElementBits;
            DEC(LEN);
            ptrC^ := MAX(SetElement);
            ptrC := ADDADR(ptrC, SetElementSize);
        END;

        (* now handle the last partial word 1's *)

        IF numBits <> 0 THEN
        <*/PUSH/NOWARN:U*>
            temp := BNOT (VAL(SetElement, MAX(SetElement)) SHL numBits);
        <*/POP*>

            DEC(LEN);
            ptrC^ := temp;
            ptrC := ADDADR(ptrC, SetElementSize);
        END;
    END;

    (* fill the remaining zeros *)

    WHILE LEN <> 0 DO
        DEC(LEN);
        ptrC^ := 0;
        ptrC := ADDADR(ptrC, SetElementSize);
    END;
END SETBITS;

PROCEDURE ASSERT(strAdr : ADDRESS; lineNum : CARDINAL) [LeftToRight];
TYPE
    tptc        = POINTER TO ACHAR;
VAR
    str         : ARRAY [0..63] OF CHAR;
    i           : ADRCARD;
    d           : CARDINAL;
    ptc         : tptc;

    PROCEDURE put(ch : ACHAR);
    BEGIN
        IF i <= HIGH(str) THEN
            str[i] := ch;
            INC(i);
        END;
    END put;

BEGIN
    str := "ASSERT failure at ";
    i := 18;
    ptc := strAdr;
    WHILE ptc^ <> '' DO
        put(ptc^);
        ptc := ADDADR(ptc, SIZE(ACHAR));
    END;

    put('.');

    d := 100000000;
    WHILE (d > lineNum) AND (d > 1) DO
        d := d / 10;
    END;

    REPEAT
        put( ACHR(((lineNum / d) REM 10) + ORD('0')) );
        d := d / 10;
    UNTIL d = 0;
    put(0C);

    RaiseRTL(AssertSrc, 0, str);
END ASSERT;

PROCEDURE ISASSERT() : BOOLEAN;
BEGIN
    RETURN IsCurrentSource(AssertSrc);
END ISASSERT;

PROCEDURE SWAPARRAY(addr : ADDRESS; count : CARDINAL; elementSize : CARDINAL) [LeftToRight];
TYPE
    variant =
        RECORD
        CASE : CARDINAL OF
        0: c2 : CARDINAL16;|
        1: c4 : CARDINAL32;|
        2: c8 : CARDINAL64;
        ELSE
        END;
        END;
VAR
    ptr         : POINTER TO variant;
BEGIN
    ptr := addr;
    REPEAT
        DEC(count);

        IF elementSize = 4 THEN
            SWAPENDIAN(ptr^.c4);
        ELSIF elementSize = 2 THEN
            SWAPENDIAN(ptr^.c2);
        ELSIF elementSize = 8 THEN
            SWAPENDIAN(ptr^.c8);
        END;

        ptr := ADDADR(ptr, elementSize);
    UNTIL count = 0;
END SWAPARRAY;

TYPE
    tPtrV       = POINTER TO VmtRec;
    tPtrO       = POINTER TO tPtrV;
    ConstructorProc = PROCEDURE(tPtrO);

    (* the vmt pointer in the object points beyond all of the below *)
    (* overhead fields *)
    <*/PUSH/PACK*>
    VmtRec =
        RECORD
			size                    : INTEGER;
			negSize                 : INTEGER;
			parentVmt               : tPtrV;
			staticDataSize          : INTEGER;
			hiddenDataSize          : INTEGER;
			hiddenPointerOffset     : INTEGER;
			%IF Bits64 %THEN
				pad                 : INTEGER;
			%END
			constructor             : ConstructorProc;
			destructor              : ConstructorProc;
        END;
    <*/POP*>
CONST
    VmtOverhead         = SIZE(VmtRec);
    %IF Bits32 %THEN
    	ClassDataAlign      = 4;
    %ELSIF Bits64 %THEN
    	ClassDataAlign      = 8; (*???RSG*)
    %ELSE
        fix me
    %END

PROCEDURE GetActualVmt(object : ADDRESS) : ADDRESS [INLINE];
BEGIN
    RETURN SUBADR(CAST(tPtrO, object)^, VmtOverhead);
END GetActualVmt;

PROCEDURE INITCLASS(vmt : ADDRESS) [LeftToRight];

    PROCEDURE compute(ptrV : tPtrV) : INTEGER;
    (* here we compute the dynamic size of the object *)
    (* by this I mean including any possible hidden object data *)
    (* fields declared inside the implementation portion of a module *)
    (* we cannot know this size at compile time *)
    VAR
        total   : INTEGER;
    BEGIN
        IF ptrV^.parentVmt <> NIL THEN
            total := compute(ptrV^.parentVmt);
        ELSE
            total := 0;
        END;

        ptrV^.staticDataSize := (ptrV^.staticDataSize + (ClassDataAlign-1)) BAND (-ClassDataAlign);
        ptrV^.hiddenDataSize := (ptrV^.hiddenDataSize + (ClassDataAlign-1)) BAND (-ClassDataAlign);

        total := total + ptrV^.hiddenDataSize;
        ptrV^.size := ptrV^.staticDataSize + total;
        ptrV^.negSize := -ptrV^.size;

        RETURN total;
    END compute;

VAR
    ptrV        : tPtrV;
BEGIN
    ptrV := vmt;
    IF ptrV^.size = 0 THEN
        FUNC compute(ptrV);
    END;
END INITCLASS;

PROCEDURE CONSTRUCTCLASS(object, vmt : ADDRESS) [LeftToRight];
TYPE
    tPtrA       = POINTER TO ADDRESS;
    tPtrI       = POINTER TO INTEGER;
VAR
    staticSize  : INTEGER;

    PROCEDURE callConstructorChain(ptrV : tPtrV) : INTEGER;
    VAR
        ptrI    : tPtrI;
        ptrO    : tPtrO;
        offset  : INTEGER;
    BEGIN
        (* parent constructors are handled before we do anything *)

        IF ptrV^.parentVmt <> NIL THEN
            offset := callConstructorChain(ptrV^.parentVmt);
        ELSE
            offset := staticSize;
        END;

        (* now init our special fields *)

        IF ptrV^.hiddenPointerOffset <> -1 THEN
            ptrI := ADDADR(object, ptrV^.hiddenPointerOffset);
            ptrI^ := offset;
        END;

        IF CAST(ADDRESS, ptrV^.constructor) <> NIL THEN
            ptrO := CAST(tPtrO, object);
            ptrO^ := ADDADR(ptrV, VmtOverhead);
            ptrV^.constructor(ptrO);
        END;

        RETURN offset + ptrV^.hiddenDataSize;
    END callConstructorChain;

BEGIN
    (* call the constructor chain *)
    (* the proper vmt for each class will be stuffed into the *)
    (* vmt field of the object before calling the constructor *)

    staticSize := vmt:tPtrV^.staticDataSize;
    FUNC callConstructorChain(vmt);

    (* init the vmt field in the object *)

    object:tPtrA^ := ADDADR(vmt, VmtOverhead);
END CONSTRUCTCLASS;

PROCEDURE CONSTRUCTCLASSALL(VAR OUT object : ADDRESS; vmt : ADDRESS; allocProc : AllocDeallocProc) [LeftToRight];
BEGIN
    INITCLASS(vmt);

    allocProc (object, CAST(tPtrV, vmt)^.size);

    CONSTRUCTCLASS(object, vmt);
END CONSTRUCTCLASSALL;

PROCEDURE DESTRUCTCLASS(object : ADDRESS) [LeftToRight];
VAR
    ptrV        : tPtrV;
BEGIN
    (* destructors are called in reverse order of construction *)

    ptrV := GetActualVmt(object);
    WHILE ptrV <> NIL DO
        IF CAST(ADDRESS, ptrV^.destructor) <> NIL THEN
            ptrV^.destructor(object);
        END;

        ptrV := ptrV^.parentVmt;
    END;
END DESTRUCTCLASS;

PROCEDURE DESTRUCTCLASSALL(VAR INOUT object : ADDRESS; deallocProc : AllocDeallocProc) [LeftToRight];
VAR
    ptrV        : tPtrV;
BEGIN
    DESTRUCTCLASS(object);
    ptrV := GetActualVmt(object);
    deallocProc (object, ptrV^.size);
END DESTRUCTCLASSALL;

PROCEDURE CLONEOBJECT(destObj, srcObj : ADDRESS) [LeftToRight];
TYPE
    bytes       = ARRAY [0..0] OF BYTE;
VAR
    amount      : CARDINAL;
    ptrV        : tPtrV;
BEGIN
    ptrV := GetActualVmt(srcObj);
    amount := ptrV^.size;
    destObj^:bytes[0..amount-1] := srcObj^:bytes[0..amount-1];
END CLONEOBJECT;

PROCEDURE CLONEOBJECTALL(VAR OUT destObj : ADDRESS; srcObj : ADDRESS; allocProc : AllocDeallocProc) [LeftToRight];
VAR
    ptrV        : tPtrV;
BEGIN
    IF srcObj <> NIL THEN
        ptrV := GetActualVmt(srcObj);

        allocProc (destObj, ptrV^.size);

        CLONEOBJECT(destObj, srcObj);
    ELSE
        destObj := NIL;
    END;
END CLONEOBJECTALL;

PROCEDURE IsMember(vmtL, vmtR : tPtrV) : BOOLEAN;
(* is the left a decendant of the right *)
BEGIN
    LOOP
        IF vmtL <> NIL THEN
            IF vmtL <> vmtR THEN
                vmtL := vmtL^.parentVmt;
            ELSE
                RETURN TRUE;
            END;
        ELSE
            EXIT;
        END;
    END;
    RETURN FALSE;
END IsMember;

PROCEDURE ISMEMBERL(object, vmt : ADDRESS) : BOOLEAN [LeftToRight];
BEGIN
    IF object <> NIL THEN
        RETURN IsMember(GetActualVmt(object), vmt);
    END;
    RETURN FALSE;
END ISMEMBERL;

PROCEDURE ISMEMBERR(vmt, object : ADDRESS) : BOOLEAN [LeftToRight];
BEGIN
    IF object <> NIL THEN
        RETURN IsMember(vmt, GetActualVmt(object));
    END;
    RETURN FALSE;
END ISMEMBERR;

PROCEDURE ISMEMBERLR(objectL, objectR : ADDRESS) : BOOLEAN [LeftToRight];
BEGIN
    IF (objectL <> NIL) AND (objectR <> NIL) THEN
        RETURN IsMember(GetActualVmt(objectL), GetActualVmt(objectR));
    END;
    RETURN FALSE;
END ISMEMBERLR;

PROCEDURE AbstractMethod() [FRAME];
BEGIN
    RaiseM2OOException(abstractException, "ABSTRACT-METHOD-CALLED");
END AbstractMethod;

PROCEDURE SLVM ["SB_SYSTEM@SLVM_EXCLUDED"] () [FRAME];
BEGIN
    RaiseM2OOException(excludedMethodException, "EXCLUDED-METHOD-CALLED");
END SLVM;

PROCEDURE OutputDebugMessage(str : ARRAY OF CHAR);
BEGIN
    (* this does nothing. the debugger sets a breakpoint on this procedure
       and knows what the parameter structure is so it can read the string
       and display it in the debugger message window.
    *)
    UNREFERENCED_PARAMETER(str);
END OutputDebugMessage;

%IF IA32 %THEN

PROCEDURE DummyMemoryFence; PUREASM; (* to use on CPUs before SSE *)
ASM
		RET
END DummyMemoryFence;

PROCEDURE OldMemoryFence; PUREASM; (* to use on CPUs with SSE before SSE2 *)
ASM
    	SFENCE
		RET
END OldMemoryFence;

PROCEDURE MemoryFence; PUREASM;
ASM
    	MFENCE
		RET
END MemoryFence;

%END

(*
 * GUARDBRANCH procedure is called to execute GUARD statement.
 *
 * It searches through so-called GUARD transfer table and execute jump to the appropriate location.
 *
 * A GUARD transfer table contains cells corresponding to all cases of the statement and one final cell.
 * Each non-final cell contains an address of selector class' VMT and a transfer to code.
 * A final cell contains zero value and a tranfer to go in case when no selector matches.
 *
 * Structure of a table cell depends on type of generated code:
 *
 * For 32-bit code a cell contains a VMT address and an address to jump to.
 * For 64-bit position independent code a cell contains
 *	a 32-bit offset from the start of the cell to a double word containing VMT address (GOT) and
 *	a 5-byte JMP instruction.
 * For 64-bit not position independent code a cell contains a 64-bit VMT address and a 64-bit address to jump to.
 *)

%IF IA32 %THEN

PROCEDURE GUARDBRANCH(object, table : ADDRESS) [PASS(DX, AX), ALTERS(DX, AX)]; PUREASM;
ASM
        PUSH    ESI
        PUSH    EDI

        TEST    EDX, EDX        (* check for a null object *)
        JZ      @FindElse       (* this goes to the else *)
        MOV     ESI, [EDX]      (* indirect to get the source vmt *)
        SUB     ESI, VmtOverhead(* adjust the object vmt to actual vmt *)

    @OuterLoopTop:
        MOV     EDI, CS:[EAX]   (* get the next item to compare *)
        TEST    EDI, EDI        (* end of table? *)
        JZ      @Done
        MOV     EDX, ESI        (* get the source vmt *)
    @InnerLoopTop:
        CMP     EDX, EDI        (* are the vmt's the same *)
        JE      @Done
        MOV     EDX, [EDX].VmtRec.parentVmt
        TEST    EDX, EDX        (* last vmt in chain? *)
        JNZ     @InnerLoopTop
        ADD     EAX, 8          (* next table item *)
        JMP     @OuterLoopTop   (* try again *)
    @Done:
        POP     EDI
        POP     ESI
        POP     EDX             (* get rid of the return address *)
        PUSH    DWORD PTR CS:[EAX+4](* put a new one back *)
        RET
    @FindElseTop:
        ADD     EAX, 8
    @FindElse:
        MOV     EDI, CS:[EAX]
        TEST    EDI, EDI
        JNZ     @FindElseTop
        JMP     @Done
END GUARDBRANCH;

%ELSE

PROCEDURE GUARDBRANCH (object, table : ADDRESS) [PASS(DX,AX), ALTERS(DX,AX)]; PUREASM;
ASM
	%IF Windows %THEN
        MOV		[RSP+ 8], RSI
        MOV		[RSP+16], RDI
    %ELSE
        PUSH	RSI
        PUSH	RDI
	%END

        TEST    RDX, RDX        (* check for a null object *)
        JZ      @FindElse       (* this goes to the else *)
        MOV		RSI, [RDX]      (* indirect to get the source vmt *)
        SUB     RSI, VmtOverhead(* adjust the object vmt to actual vmt *)

    @OuterLoopTop:
	%IF PICCode %THEN
		MOVSXD	RDI, [RAX]		(* get a relative address of GOT VMT *)
		TEST	EDI, EDI		(* end of table? *)
		JZ		@Done
		MOV		RDI, [RAX+RDI]	(* VMT address *)
	%ELSE
		MOV		RDI, [RAX]		(* get the next item to compare *)
		TEST	RDI, RDI		(* end of table? *)
		JZ		@Done
	%END
        MOV     RDX, RSI        (* get the source vmt *)
    @InnerLoopTop:
        CMP     RDX, RDI        (* are the vmt's the same *)
        JE      @Done
        MOV     RDX, [RDX].VmtRec.parentVmt
        TEST    RDX, RDX        (* last vmt in chain? *)
        JNZ     @InnerLoopTop
	%IF PICCode %THEN
        ADD     RAX, 9        	(* next table item *)
	%ELSE
        ADD     RAX, 16        	(* next table item *)
	%END
        JMP     @OuterLoopTop   (* try again *)

    @Done:
	%IF Windows %THEN
        MOV     RSI, [RSP+ 8]	(* restore RSI and RDI content *)
        MOV     RDI, [RSP+16]
	%ELSE
        POP     RDI				(* restore RSI and RDI content *)
        POP     RSI
	%END
	%IF PICCode %THEN
		ADD		RAX, 4			(* address of JMP instruction *)
	%ELSE
		MOV		RAX, [RAX+8]	(* address to jump to *)
	%END
		MOV		[RSP], RAX		(* replace the return address *)
        RET

    @FindElseTop:
	%IF PICCode %THEN
        ADD     RAX, 9
    @FindElse:
        CMP		DWORD PTR [RAX], 0
	%ELSE
        ADD     RAX, 16
    @FindElse:
        CMP		QWORD PTR [RAX], 0
	%END
        JNZ     @FindElseTop
        JMP     @Done
END GUARDBRANCH;

%END

PROCEDURE GetRtlInit() : BOOLEAN;
BEGIN
    RETURN SystemInitialized;
END GetRtlInit;

END SYSTEMM.
