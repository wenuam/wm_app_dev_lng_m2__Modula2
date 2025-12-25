(***************************************************************************)
(*                                                                         *)
(*                           Copyright (C) 2009                            *)
(*                             by ADW Software                             *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(* Extended and partially rewritten on Assembler by A. Mutylin October 2015*)
(***************************************************************************)
IMPLEMENTATION MODULE MemUtils;
<*/OPTIMIZE:TN/INLINE:N*>

<*/NOHIGH*>(* none of these calls care about this, so why pass usused data *)

FROM SYSTEM IMPORT
    ADDRESS, ADRCARD, BYTE, WORD, DWORD %IF AMD64 %THEN , QWORD %END
	%IF IA32 %OR AMD64 %THEN , CPUVendorType, CPUVendor, CPUFamily %END;

%IF IA32 %OR AMD64 %THEN

PROCEDURE SupportedERMSB () : BOOLEAN [Alters(AX,BX,CX,DX),Returns(BX)]; PUREASM;
ASM
		XOR		EAX, EAX
		CPUID
		XOR		EBX, EBX
		CMP		EAX, 7
		JB		@end
        MOV     EAX, 7
        XOR     ECX, ECX
        CPUID
        AND     EBX, 200H
        SHR		EBX, 9
@end:   RET
END SupportedERMSB;

PROCEDURE FillMemBYTE (dest : ADDRESS; numBytes : ADRCARD; db : BYTE) %IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX)] %END;
PUREASM;
ASM
        CLD
        REP STOSB
        RET
END FillMemBYTE;

PROCEDURE FillMemWORD(dest : ADDRESS; numWords : ADRCARD; dw : WORD) %IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX)] %END;
PUREASM;
ASM
        CLD
        REP STOSW
		RET
END FillMemWORD;

PROCEDURE FillMemDWORD(dest : ADDRESS; numDwords : ADRCARD; dd : DWORD) %IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX)] %END;
PUREASM;
ASM
        CLD
        REP STOSD
		RET
END FillMemDWORD;

%IF IA32 %THEN

PROCEDURE ZeroMemTrad (dest : ADDRESS; numBytes : CARDINAL32) [Pass(DI,CX),Alters(DI,CX,AX),PropagateException,Export]; PUREASM;
ASM
        XOR     EAX, EAX
        CLD
        PUSH	ECX
        SHR     ECX, 2
        REP STOSD
        POP     ECX
        AND     ECX, 3
		REP STOSB
		RET
END ZeroMemTrad;

PROCEDURE ZeroMemERMSB (dest : ADDRESS; numBytes : CARDINAL32) %IF %NOT DLL %THEN [Pass(DI,CX),Alters(DI,CX,AX)] %END; PUREASM;
ASM
        XOR     EAX, EAX
        CLD
		REP STOSB
		RET
END ZeroMemERMSB;

PROCEDURE ScanMemBYTE (dest : ADDRESS; numBytes : CARDINAL32; db : BYTE) : CARDINAL32
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,AX)] %END;
PUREASM;
ASM
        PUSH    ECX
        CLD
        REPNE SCASB
        POP     EAX
        JNE     @end
        SUB     EAX, ECX
        JZ      @end		(* The only sense of this check is numBytes = 0 *)
        DEC     EAX
@end:   RET
END ScanMemBYTE;

PROCEDURE ScanMemNeBYTE (dest : ADDRESS; numBytes : CARDINAL32; db : BYTE) : CARDINAL32
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,AX)] %END;
PUREASM;
ASM
        PUSH    ECX
        CLD
        REPE SCASB
        POP     EAX
        JE      @end
        SUB     EAX, ECX
        JZ      @end		(* The only sense of this check is numBytes = 0 *)
        DEC     EAX
@end:   RET
END ScanMemNeBYTE;

PROCEDURE ScanMemWORD (dest : ADDRESS; numWords : CARDINAL32; dw : WORD) : CARDINAL32
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,AX)] %END;
PUREASM;
ASM
        PUSH    ECX
        CLD
        REPNE SCASW
        POP     EAX
        JNE     @end
        SUB     EAX, ECX
        JZ      @end		(* The only sense of this check is numWords = 0 *)
        DEC     EAX
@end:   RET
END ScanMemWORD;

PROCEDURE ScanMemNeWORD (dest : ADDRESS; numWords : CARDINAL32; dw : WORD) : CARDINAL32
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,AX)] %END;
PUREASM;
ASM
        PUSH    ECX
        CLD
        REPE SCASW
        POP     EAX
        JE      @end
        SUB     EAX, ECX
        JZ      @end		(* The only sense of this check is numWords = 0 *)
        DEC     EAX
@end:   RET
END ScanMemNeWORD;

PROCEDURE ScanMemDWORD(dest : ADDRESS; numDwords : CARDINAL32; dd : DWORD) : CARDINAL32
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,AX)] %END;
PUREASM;
ASM
        PUSH    ECX
        CLD
        REPNE SCASD
        POP     EAX
        JNE     @end
        SUB     EAX, ECX
        JZ      @end		(* The only sense of this check is numDwords = 0 *)
        DEC     EAX
@end:   RET
END ScanMemDWORD;

PROCEDURE ScanMemNeDWORD(dest : ADDRESS; numDwords : CARDINAL32; dd : DWORD) : CARDINAL32
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,AX)] %END;
PUREASM;
ASM
        PUSH    ECX
        CLD
        REPE SCASD
        POP     EAX
        JE      @end
        SUB     EAX, ECX
        JZ      @end		(* The only sense of this check is numDwords = 0 *)
        DEC     EAX
@end:   RET
END ScanMemNeDWORD;

PROCEDURE CompMem (dest, src : ADDRESS; numBytes : CARDINAL32) : CARDINAL32
%IF %NOT DLL %THEN [Pass(DI,SI,CX),Alters(DI,SI,CX,AX)] %END;
PUREASM;
ASM
        PUSH	ECX
		CLD
		REPE CMPSB
		POP		EAX
		JE		@end
		SUB		EAX, ECX
        JZ      @end		(* The only sense of this check is numBytes = 0 *)
		DEC		EAX
@end:	RET
END CompMem;

PROCEDURE MoveMemForwardTrad (dest, src : ADDRESS; numBytes : CARDINAL32)
[Pass(DI,SI,CX),Alters(DI,SI,CX),PropagateException,Export];
PUREASM;
ASM
        CLD
		PUSH	ECX
        SHR     ECX, 2
        REP MOVSD
        POP     ECX
        AND     ECX, 3
		REP MOVSB
		RET
END MoveMemForwardTrad;

PROCEDURE MoveMemForwardERMSB (dest, src : ADDRESS; numBytes : CARDINAL32)
%IF %NOT DLL %THEN [Pass(DI,SI,CX),Alters(DI,SI,CX)] %END;
PUREASM;
ASM
        CLD
		REP MOVSB
		RET
END MoveMemForwardERMSB;

PROCEDURE MoveMemBackward (dest, src : ADDRESS; numBytes : CARDINAL32)
%IF %NOT DLL %THEN [Pass(DI,SI,CX),Alters(DI,SI,CX,AX)] %END;
PUREASM;
ASM
        STD
        LEA     ESI, [ESI+ECX-4]
        LEA     EDI, [EDI+ECX-4]
        MOV     EAX, ECX
        SHR     ECX, 2
        REP MOVSD
        MOV     ECX, EAX
        AND     ECX, 3
        ADD     EDI, 3
        ADD     ESI, 3
        REP MOVSB
        CLD
		RET
END MoveMemBackward;

PROCEDURE MoveMemTrad (dest, src : ADDRESS; amount : CARDINAL32) [Pass(DI,SI,CX),Alters(DI,SI,CX,AX),PropagateException,Export];
PUREASM;
ASM
        CMP     EDI, ESI
        JBE     @MoveForward
        MOV     EAX, ECX
        ADD     EAX, ESI
        CMP     EAX, EDI                (* check for overlap *)
        JBE     @MoveForward
        STD
        LEA     ESI, [ESI+ECX-4]
        LEA     EDI, [EDI+ECX-4]
        MOV     EAX, ECX
        SHR     ECX, 2
        REP MOVSD
        MOV     ECX, EAX
        AND     ECX, 3
        ADD     EDI, 3
        ADD     ESI, 3
        REP MOVSB
        CLD
        RET
@MoveForward:
        CLD
        MOV     EAX, ECX
        SHR     ECX, 2
        REP MOVSD
        MOV     ECX, EAX
        AND     ECX, 3
		REP MOVSB
		RET
END MoveMemTrad;

PROCEDURE MoveMemERMSB (dest, src : ADDRESS; amount : CARDINAL32) %IF %NOT DLL %THEN [Pass(DI,SI,CX),Alters(DI,SI,CX,AX)] %END;
PUREASM;
ASM
        CMP     EDI, ESI
        JBE     @MoveForward
        MOV     EAX, ECX
        ADD     EAX, ESI
        CMP     EAX, EDI                (* check for overlap *)
        JBE     @MoveForward
        STD
        LEA     ESI, [ESI+ECX-4]
        LEA     EDI, [EDI+ECX-4]
        MOV     EAX, ECX
        SHR     ECX, 2
        REP MOVSD
        MOV     ECX, EAX
        AND     ECX, 3
        ADD     EDI, 3
        ADD     ESI, 3
        REP MOVSB
        CLD
        RET
@MoveForward:
        CLD
		REP MOVSB
		RET
END MoveMemERMSB;

%ELSE (* %IF IA32 *)

PROCEDURE ZeroMemTrad (dest : ADDRESS; numBytes : CARDINAL64) [Pass(DI,CX),Alters(DI,CX,AX),PropagateException,Export]; PUREASM;
ASM
        XOR     EAX, EAX
        CLD
%IF Windows %THEN
		MOV		[RSP+8], ECX
%ELSE
        PUSH	RCX
%END
        SHR     RCX, 3
        REP STOSQ
%IF Windows %THEN
		MOV		ECX, [RSP+8]
%ELSE
        POP     RCX
%END
        AND     ECX, 7
		REP STOSB
		RET
END ZeroMemTrad;

PROCEDURE ZeroMemERMSB (dest : ADDRESS; numBytes : CARDINAL64) %IF %NOT DLL %THEN [Pass(DI,CX),Alters(DI,CX,AX)] %END; PUREASM;
ASM
        XOR     EAX, EAX
        CLD
		REP STOSB
		RET
END ZeroMemERMSB;

PROCEDURE FillMemQWORD (dest : ADDRESS; numQwords : CARDINAL64; dq : DWORD)
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX)] %END;
PUREASM;
ASM
        CLD
        REP STOSQ
		RET
END FillMemQWORD;

PROCEDURE ScanMemBYTE (dest : ADDRESS; numBytes : CARDINAL64; db : BYTE) : CARDINAL64
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,DX),Returns(DX)] %END;
PUREASM;
ASM
        MOV		RDX, RCX
        CLD
        REPNE SCASB
        JNE     @end
        SUB     RDX, RCX
        JZ      @end		(* The only sense of this check is numBytes = 0 *)
        DEC     RDX
@end:   RET
END ScanMemBYTE;

PROCEDURE ScanMemNeBYTE (dest : ADDRESS; numBytes : CARDINAL64; db : BYTE) : CARDINAL64
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,DX),Returns(DX)] %END;
PUREASM;
ASM
        MOV		RDX, RCX
        CLD
        REPE SCASB
        JE      @end
        SUB     RDX, RCX
        JZ      @end		(* The only sense of this check is numBytes = 0 *)
        DEC     RDX
@end:   RET
END ScanMemNeBYTE;

PROCEDURE ScanMemWORD (dest : ADDRESS; numWords : CARDINAL64; dw : WORD) : CARDINAL64
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,DX),Returns(DX)] %END;
PUREASM;
ASM
        MOV		RDX, RCX
        CLD
        REPNE SCASW
        JNE     @end
        SUB     RDX, RCX
        JZ      @end		(* The only sense of this check is numWords = 0 *)
        DEC     RDX
@end:   RET
END ScanMemWORD;

PROCEDURE ScanMemNeWORD (dest : ADDRESS; numWords : CARDINAL64; dw : WORD) : CARDINAL64
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,DX),Returns(DX)] %END;
PUREASM;
ASM
        MOV		RDX, RCX
        CLD
        REPE SCASW
        JE      @end
        SUB     RDX, RCX
        JZ      @end		(* The only sense of this check is numWords = 0 *)
        DEC     RDX
@end:   RET
END ScanMemNeWORD;

PROCEDURE ScanMemDWORD (dest : ADDRESS; numDwords : CARDINAL64; dd : DWORD) : CARDINAL64
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,DX),Returns(DX)] %END;
PUREASM;
ASM
        MOV		RDX, RCX
        CLD
        REPNE SCASD
        JNE     @end
        SUB     RDX, RCX
        JZ      @end		(* The only sense of this check is numDwords = 0 *)
        DEC     RDX
@end:   RET
END ScanMemDWORD;

PROCEDURE ScanMemNeDWORD (dest : ADDRESS; numDwords : CARDINAL64; dd : DWORD) : CARDINAL64
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,DX),Returns(DX)] %END;
PUREASM;
ASM
        MOV		RDX, RCX
        CLD
        REPE SCASD
        JE      @end
        SUB     RDX, RCX
        JZ      @end		(* The only sense of this check is numDwords = 0 *)
        DEC     RDX
@end:   RET
END ScanMemNeDWORD;

PROCEDURE ScanMemQWORD (dest : ADDRESS; numQwords : CARDINAL64; dq : QWORD) : CARDINAL64
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,DX),Returns(DX)] %END;
PUREASM;
ASM
        MOV		RDX, RCX
        CLD
        REPNE SCASQ
        JNE     @end
        SUB     RDX, RCX
        JZ      @end		(* The only sense of this check is numQwords = 0 *)
        DEC     RDX
@end:   RET
END ScanMemQWORD;

PROCEDURE ScanMemNeQWORD (dest : ADDRESS; numQwords : CARDINAL64; dq : QWORD) : CARDINAL64
%IF %NOT DLL %THEN [Pass(DI,CX,AX),Alters(DI,CX,DX),Returns(DX)] %END;
PUREASM;
ASM
        MOV		RDX, RCX
        CLD
        REPE SCASQ
        JE      @end
        SUB     RDX, RCX
        JZ      @end		(* The only sense of this check is numQwords = 0 *)
        DEC     RDX
@end:   RET
END ScanMemNeQWORD;

PROCEDURE CompMem (dest, src : ADDRESS; numBytes : CARDINAL64) : CARDINAL64
%IF %NOT DLL %THEN [Pass(DI,SI,CX),Alters(DI,SI,CX,AX)] %END;
PUREASM;
ASM
        PUSH	RCX
		CLD
		REPE CMPSB
		POP		RAX
		JE		@end
		SUB		RAX, RCX
        JZ      @end		(* The only sense of this check is numBytes = 0 *)
		DEC		RAX
@end:	RET
END CompMem;

PROCEDURE MoveMemForwardTrad (dest, src : ADDRESS; numBytes : CARDINAL64)
[Pass(DI,SI,CX),Alters(DI,SI,CX),PropagateException,Export];
PUREASM;
ASM
        CLD
%IF Windows %THEN
		MOV		[RSP+8], ECX
%ELSE
		PUSH	RCX
%END
        SHR     RCX, 3
        REP MOVSQ
%IF Windows %THEN
		MOV		ECX, [RSP+8]
%ELSE
        POP     RCX
%END
        AND     ECX, 7
		REP MOVSB
		RET
END MoveMemForwardTrad;

PROCEDURE MoveMemForwardERMSB (dest, src : ADDRESS; numBytes : CARDINAL64)
%IF %NOT DLL %THEN [Pass(DI,SI,CX),Alters(DI,SI,CX)] %END;
PUREASM;
ASM
        CLD
		REP MOVSB
		RET
END MoveMemForwardERMSB;

PROCEDURE MoveMemBackward (dest, src : ADDRESS; numBytes : CARDINAL64)
%IF %NOT DLL %THEN [Pass(DI,SI,CX),Alters(DI,SI,CX,AX)] %END;
PUREASM;
ASM
        STD
        LEA     RSI, [RSI+RCX-8]
        LEA     RDI, [RDI+RCX-8]
        MOV     EAX, ECX
        SHR     RCX, 3
        REP MOVSQ
        MOV     ECX, EAX
        AND     ECX, 7
        ADD     RDI, 7
        ADD     RSI, 7
        REP MOVSB
        CLD
		RET
END MoveMemBackward;

PROCEDURE MoveMemTrad (dest, src : ADDRESS; amount : CARDINAL64) [Pass(DI,SI,CX),Alters(DI,SI,CX,AX),PropagateException,Export];
PUREASM;
ASM
        CMP     RDI, RSI
        JBE     @MoveForward
        MOV     RAX, RCX
        ADD     RAX, RSI
        CMP     RAX, RDI                (* check for overlap *)
        JBE     @MoveForward
        STD
        LEA     RSI, [RSI+RCX-8]
        LEA     RDI, [RDI+RCX-8]
        MOV     EAX, ECX
        SHR     RCX, 3
        REP MOVSQ
        MOV     ECX, EAX
        AND     ECX, 7
        ADD     RDI, 7
        ADD     RSI, 7
        REP MOVSB
        CLD
        RET
@MoveForward:
        CLD
		MOV		EAX, ECX
        SHR     RCX, 3
        REP MOVSQ
		MOV		ECX, EAX
        AND     ECX, 7
		REP MOVSB
		RET
END MoveMemTrad;

PROCEDURE MoveMemERMSB (dest, src : ADDRESS; amount : CARDINAL64) %IF %NOT DLL %THEN [Pass(DI,SI,CX),Alters(DI,SI,CX,AX)] %END;
PUREASM;
ASM
        CMP     RDI, RSI
        JBE     @MoveForward
        MOV     RAX, RCX
        ADD     RAX, RSI
        CMP     RAX, RDI                (* check for overlap *)
        JBE     @MoveForward
        STD
        LEA     RSI, [RSI+RCX-8]
        LEA     RDI, [RDI+RCX-8]
        MOV     EAX, ECX
        SHR     RCX, 3
        REP MOVSQ
        MOV     ECX, EAX
        AND     ECX, 7
        ADD     RDI, 7
        ADD     RSI, 7
        REP MOVSB
        CLD
        RET
@MoveForward:
        CLD
		REP MOVSB
		RET
END MoveMemERMSB;

%END (* %IF IA32 *)

PROCEDURE Init;
BEGIN
    IF (CPUVendor = Intel) & ~ SupportedERMSB() OR (CPUVendor = AMD) & (CPUFamily < 16H) THEN
		ZeroMem := ZeroMemTrad;
		MoveMemForward := MoveMemForwardTrad;
		MoveMem := MoveMemTrad;
	END;
END Init;

%ELSE (* %IF IA32 %OR AMD64 *)

FROM SYSTEM IMPORT
    ADDADR, SUBADR, CAST, ASSERT, UNREFERENCED_PARAMETER;

TYPE
%IF Bits32 %THEN
    MoveType    = CARDINAL32;
%ELSE
    MoveType    = CARDINAL64;
%END
    <*/PUSH/PACK*>
    MoveVariant =
        RECORD
			CASE : CARDINAL OF
			0: b : BYTE;|
			1: w : WORD;|
			2: d : DWORD;|
			3: ba : ARRAY [0..SIZE(MoveType)-1] OF BYTE;|
			4: wa : ARRAY [0..(SIZE(MoveType)/2)-1] OF WORD;|
			5: m : MoveType;
			ELSE
			END;
        END;
    <*/POP*>

    MovePointer = POINTER TO ARRAY [0..3] OF MoveVariant;

    ASSERT(SIZE(MoveVariant) = SIZE(MoveType));

CONST
    MoveSize    = SIZE(MoveType);

PROCEDURE FillMemBYTE(dest : ADDRESS; numBytes : CARDINAL; db : BYTE);
VAR
    ptr         : MovePointer;
    outVal      : MoveType;
BEGIN
    ptr := dest;
    LOOP
        IF (numBytes <> 0) AND ((CAST(ADRCARD, ptr) REM MoveSize) <> 0) THEN
            DEC(numBytes);
            ptr^[0].b := db;
            ptr := ADDADR(ptr, 1);
        ELSE
            EXIT;
        END;
    END;

    (* the address is now aligned *)

    outVal := ORD(db);
    outVal := outVal BOR (outVal SHL 8);
    outVal := outVal BOR (outVal SHL 16);
    %IF Bits64 %THEN
        outVal := outVal BOR (outVal SHL 32);
    %END
    LOOP
        IF numBytes >= MoveSize*4 THEN
            numBytes := numBytes - (MoveSize*4);
            ptr^[0].m := outVal;
            ptr^[1].m := outVal;
            ptr^[2].m := outVal;
            ptr^[3].m := outVal;
            ptr := ADDADR(ptr, MoveSize*4);
            IF numBytes < MoveSize THEN
                EXIT;
            END;

        ELSIF numBytes >= MoveSize THEN
            numBytes := numBytes - MoveSize;
            ptr^[0].m := outVal;
            ptr := ADDADR(ptr, MoveSize);
                IF numBytes < MoveSize THEN
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;

    LOOP
        IF numBytes <> 0 THEN
            DEC(numBytes);
            ptr^[0].b := db;
            ptr := ADDADR(ptr, 1);

            IF numBytes <> 0 THEN
                DEC(numBytes);
                ptr^[0].b := db;
                ptr := ADDADR(ptr, 1);
                IF numBytes = 0 THEN
                    EXIT;
                END;
            ELSE
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;
END FillMemBYTE;

PROCEDURE FillMemWORD(dest : ADDRESS; numWords : CARDINAL; dw : WORD);
VAR
    ptr         : MovePointer;
    outVal      : MoveType;
BEGIN
    ptr := dest;
    LOOP
        IF (numWords <> 0) AND ((CAST(ADRCARD, ptr) REM MoveSize) <> 0) THEN
            DEC(numWords);
            ptr^[0].w := dw;
            ptr := ADDADR(ptr, 2);
        ELSE
            EXIT;
        END;
    END;

    (* the address is now aligned *)

    outVal := ORD(dw);
    outVal := outVal BOR (outVal SHL 16);
    %IF Bits64 %THEN
        outVal := outVal BOR (outVal SHL 32);
    %END
    LOOP
        IF numWords >= (MoveSize/2*4) THEN
            numWords := numWords - (MoveSize/2*4);
            ptr^[0].m := outVal;
            ptr^[1].m := outVal;
            ptr^[2].m := outVal;
            ptr^[3].m := outVal;
            ptr := ADDADR(ptr, MoveSize*4);
            IF numWords < (MoveSize/2) THEN
                EXIT;
            END;
        ELSIF numWords >= (MoveSize/2) THEN
            numWords := numWords - (MoveSize/2);
            ptr^[0].m := outVal;
            ptr := ADDADR(ptr, MoveSize);
            IF numWords < (MoveSize/2) THEN
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;

    LOOP
        IF numWords <> 0 THEN
            DEC(numWords);
            ptr^[0].w := dw;
            ptr := ADDADR(ptr, 2);
            IF numWords = 0 THEN
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;
END FillMemWORD;

PROCEDURE FillMemDWORD(dest : ADDRESS; numDwords : CARDINAL; dd : DWORD);
VAR
    ptr         : MovePointer;
    outVal      : MoveType;
BEGIN
    ptr := dest;
    LOOP
        IF (numDwords <> 0) AND ((CAST(ADRCARD, ptr) REM MoveSize) <> 0) THEN
            DEC(numDwords);
            ptr^[0].d := dd;
            ptr := ADDADR(ptr, 4);
        ELSE
            EXIT;
        END;
    END;

    (* the address is now aligned *)

    %IF Bits32 %THEN
        outVal := dd;
    %ELSE
        outVal := VAL(CARDINAL, dd);
        outVal := outVal BOR (outVal SHL 32);
    %END

    LOOP
        IF numDwords >= (MoveSize/4*4) THEN
            numDwords := numDwords - (MoveSize/4*4);
            ptr^[0].m := outVal;
            ptr^[1].m := outVal;
            ptr^[2].m := outVal;
            ptr^[3].m := outVal;
            ptr := ADDADR(ptr, MoveSize*4);
            IF numDwords < (MoveSize/4) THEN
                EXIT;
            END;
        ELSIF numDwords >= (MoveSize/4) THEN
            numDwords := numDwords - (MoveSize/4);
            ptr^[0].m := outVal;
            ptr := ADDADR(ptr, MoveSize);
            IF numDwords < (MoveSize/4) THEN
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;

    LOOP
        IF numDwords <> 0 THEN
            DEC(numDwords);
            ptr^[0].d := dd;
            ptr := ADDADR(ptr, 4);
            IF numDwords = 0 THEN
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;
END FillMemDWORD;

PROCEDURE ZeroMem (dest : ADDRESS; numBytes : CARDINAL);
BEGIN
    UNREFERENCED_PARAMETER (dest);
    UNREFERENCED_PARAMETER (numBytes);
    (* TO BE WRITTEN *)
END ZeroMem;

PROCEDURE ScanMemBYTE(dest : ADDRESS; numBytes : CARDINAL; db : BYTE) : CARDINAL;
VAR
    ptr         : POINTER TO ARRAY [0..3] OF BYTE;
    i           : CARDINAL;
BEGIN
    ptr := dest;
    i := 0;
    LOOP
        IF numBytes >= 4 THEN
            numBytes := numBytes - 4;

            IF ptr^[0] <> db THEN
                IF ptr^[1] <> db THEN
                    IF ptr^[2] <> db THEN
                        IF ptr^[3] <> db THEN
                            ptr := ADDADR(ptr, 4);
                            i := i + 4;
                        ELSE
                            RETURN i + 3;
                        END;
                    ELSE
                        RETURN i + 2;
                    END;
                ELSE
                    RETURN i + 1;
                END;
            ELSE
                RETURN i;
            END;

        ELSIF (numBytes > 0) AND (ptr^[0] <> db) THEN
            DEC(numBytes);
            INC(i);
            ptr := ADDADR(ptr, 1);
        ELSE
            RETURN i;
        END;
    END;
END ScanMemBYTE;

PROCEDURE ScanMemNeBYTE(dest : ADDRESS; numBytes : CARDINAL; db : BYTE) : CARDINAL;
VAR
    ptr         : POINTER TO ARRAY [0..3] OF BYTE;
    i           : CARDINAL;
BEGIN
    ptr := dest;
    i := 0;
    LOOP
        IF numBytes >= 4 THEN
            numBytes := numBytes - 4;

            IF ptr^[0] = db THEN
                IF ptr^[1] = db THEN
                    IF ptr^[2] = db THEN
                        IF ptr^[3] = db THEN
                            ptr := ADDADR(ptr, 4);
                            i := i + 4;
                        ELSE
                            RETURN i + 3;
                        END;
                    ELSE
                        RETURN i + 2;
                    END;
                ELSE
                    RETURN i + 1;
                END;
            ELSE
                RETURN i;
            END;

        ELSIF (numBytes > 0) AND (ptr^[0] = db) THEN
            DEC(numBytes);
            INC(i);
            ptr := ADDADR(ptr, 1);
        ELSE
            RETURN i;
        END;
    END;
END ScanMemNeBYTE;

PROCEDURE ScanMemWORD(dest : ADDRESS; numWords : CARDINAL; dw : WORD) : CARDINAL;
VAR
    ptr         : POINTER TO ARRAY [0..3] OF WORD;
    i           : CARDINAL;
BEGIN
    ptr := dest;
    i := 0;
    LOOP
        IF numWords >= 4 THEN
            numWords := numWords - 4;

            IF ptr^[0] <> dw THEN
                IF ptr^[1] <> dw THEN
                    IF ptr^[2] <> dw THEN
                        IF ptr^[3] <> dw THEN
                            ptr := ADDADR(ptr, 2*4);
                            i := i + 4;
                        ELSE
                            RETURN i + 3;
                        END;
                    ELSE
                        RETURN i + 2;
                    END;
                ELSE
                    RETURN i + 1;
                END;
            ELSE
                RETURN i;
            END;

        ELSIF (numWords > 0) AND (ptr^[0] <> dw) THEN
            DEC(numWords);
            INC(i);
            ptr := ADDADR(ptr, 2);
        ELSE
            RETURN i;
        END;
    END;
END ScanMemWORD;

PROCEDURE ScanMemNeWORD(dest : ADDRESS; numWords : CARDINAL; dw : WORD) : CARDINAL;
VAR
    ptr         : POINTER TO ARRAY [0..3] OF WORD;
    i           : CARDINAL;
BEGIN
    ptr := dest;
    i := 0;
    LOOP
        IF numWords >= 4 THEN
            numWords := numWords - 4;

            IF ptr^[0] = dw THEN
                IF ptr^[1] = dw THEN
                    IF ptr^[2] = dw THEN
                        IF ptr^[3] = dw THEN
                            ptr := ADDADR(ptr, 2*4);
                            i := i + 4;
                        ELSE
                            RETURN i + 3;
                        END;
                    ELSE
                        RETURN i + 2;
                    END;
                ELSE
                    RETURN i + 1;
                END;
            ELSE
                RETURN i;
            END;

        ELSIF (numWords > 0) AND (ptr^[0] = dw) THEN
            DEC(numWords);
            INC(i);
            ptr := ADDADR(ptr, 2);
        ELSE
            RETURN i;
        END;
    END;
END ScanMemNeWORD;

PROCEDURE ScanMemDWORD(dest : ADDRESS; numDwords : CARDINAL; dd : DWORD) : CARDINAL;
VAR
    ptr         : POINTER TO ARRAY [0..3] OF DWORD;
    i           : CARDINAL;
BEGIN
    ptr := dest;
    i := 0;
    LOOP
        IF numDwords >= 4 THEN
            numDwords := numDwords - 4;

            IF ptr^[0] <> dd THEN
                IF ptr^[1] <> dd THEN
                    IF ptr^[2] <> dd THEN
                        IF ptr^[3] <> dd THEN
                            ptr := ADDADR(ptr, 4*4);
                            i := i + 4;
                        ELSE
                            RETURN i + 3;
                        END;
                    ELSE
                        RETURN i + 2;
                    END;
                ELSE
                    RETURN i + 1;
                END;
            ELSE
                RETURN i;
            END;

        ELSIF (numDwords > 0) AND (ptr^[0] <> dd) THEN
            DEC(numDwords);
            INC(i);
            ptr := ADDADR(ptr, 4);
        ELSE
            RETURN i;
        END;
    END;
END ScanMemDWORD;

PROCEDURE ScanMemNeDWORD(dest : ADDRESS; numDwords : CARDINAL; dd : DWORD) : CARDINAL;
VAR
    ptr         : POINTER TO ARRAY [0..3] OF DWORD;
    i           : CARDINAL;
BEGIN
    ptr := dest;
    i := 0;
    LOOP
        IF numDwords >= 4 THEN
            numDwords := numDwords - 4;

            IF ptr^[0] = dd THEN
                IF ptr^[1] = dd THEN
                    IF ptr^[2] = dd THEN
                        IF ptr^[3] = dd THEN
                            ptr := ADDADR(ptr, 4*4);
                            i := i + 4;
                        ELSE
                            RETURN i + 3;
                        END;
                    ELSE
                        RETURN i + 2;
                    END;
                ELSE
                    RETURN i + 1;
                END;
            ELSE
                RETURN i;
            END;

        ELSIF (numDwords > 0) AND (ptr^[0] = dd) THEN
            DEC(numDwords);
            INC(i);
            ptr := ADDADR(ptr, 4);
        ELSE
            RETURN i;
        END;
    END;
END ScanMemNeDWORD;

PROCEDURE MoveAF(ptrD, ptrS : MovePointer; amount : CARDINAL) [INLINE];
BEGIN
    LOOP
        IF amount >= MoveSize*4 THEN
            amount := amount - MoveSize*4;
            ptrD^[0].m := ptrS^[0].m;
            ptrD^[1].m := ptrS^[1].m;
            ptrD^[2].m := ptrS^[2].m;
            ptrD^[3].m := ptrS^[3].m;
            ptrS := ADDADR(ptrS, MoveSize*4);
            ptrD := ADDADR(ptrD, MoveSize*4);
            IF amount < MoveSize THEN
                EXIT;
            END;

        ELSIF amount >= MoveSize*2 THEN
            amount := amount - MoveSize*2;
            ptrD^[0].m := ptrS^[0].m;
            ptrD^[1].m := ptrS^[1].m;
            ptrS := ADDADR(ptrS, MoveSize*2);
            ptrD := ADDADR(ptrD, MoveSize*2);
            IF amount < MoveSize THEN
                EXIT;
            END;

        ELSIF amount >= MoveSize THEN
            amount := amount - MoveSize;
            ptrD^[0].m := ptrS^[0].m;
            ptrS := ADDADR(ptrS, MoveSize);
            ptrD := ADDADR(ptrD, MoveSize);
            IF amount < MoveSize THEN
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;

    IF amount > 0 THEN
        LOOP
            IF amount >= 3 THEN
                amount := amount - 3;
                ptrD^[0].ba[0] := ptrS^[0].ba[0];
                ptrD^[0].ba[1] := ptrS^[0].ba[1];
                ptrD^[0].ba[2] := ptrS^[0].ba[2];
                ptrS := ADDADR(ptrS, 3);
                ptrD := ADDADR(ptrD, 3);
                IF amount = 0 THEN
                    EXIT;
                END;
            ELSIF amount >= 2 THEN
                amount := amount - 2;
                ptrD^[0].ba[0] := ptrS^[0].ba[0];
                ptrD^[0].ba[1] := ptrS^[0].ba[1];
                ptrS := ADDADR(ptrS, 2);
                ptrD := ADDADR(ptrD, 2);
                IF amount = 0 THEN
                    EXIT;
                END;
            ELSE
                ptrD^[0].ba[0] := ptrS^[0].ba[0];
                EXIT;
            END;
        END;
    END;
END MoveAF;

PROCEDURE MoveAB(ptrD, ptrS : MovePointer; amount : CARDINAL) [INLINE];
BEGIN
    ptrD := ADDADR(ptrD, amount-1);
    ptrS := ADDADR(ptrS, amount-1);
    LOOP
        IF (amount REM MoveSize) <> 0 THEN
            DEC(amount);
            ptrD^[0].b := ptrS^[0].b;
            ptrS := SUBADR(ptrS, 1);
            ptrD := SUBADR(ptrD, 1);
        ELSE
            EXIT;
        END;
    END;

    ptrD := SUBADR(ptrD, (MoveSize*3)+(MoveSize-1));
    ptrS := SUBADR(ptrS, (MoveSize*3)+(MoveSize-1));
    LOOP
        IF amount >= MoveSize*4 THEN
            amount := amount - MoveSize*4;
            ptrD^[3].m := ptrS^[3].m;
            ptrD^[2].m := ptrS^[2].m;
            ptrD^[1].m := ptrS^[1].m;
            ptrD^[0].m := ptrS^[0].m;
            ptrS := SUBADR(ptrS, MoveSize*4);
            ptrD := SUBADR(ptrD, MoveSize*4);
            IF amount < MoveSize THEN
                EXIT;
            END;

        ELSIF amount >= MoveSize*2 THEN
            amount := amount - MoveSize*2;
            ptrD^[3].m := ptrS^[3].m;
            ptrD^[2].m := ptrS^[2].m;
            ptrS := SUBADR(ptrS, MoveSize*2);
            ptrD := SUBADR(ptrD, MoveSize*2);
            IF amount < MoveSize THEN
                EXIT;
            END;

        ELSIF amount >= MoveSize THEN
            amount := amount - MoveSize;
            ptrD^[3].m := ptrS^[3].m;
            ptrS := SUBADR(ptrS, MoveSize);
            ptrD := SUBADR(ptrD, MoveSize);
            IF amount < MoveSize THEN
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;
END MoveAB;

PROCEDURE MoveUF(ptrD, ptrS : MovePointer; amount : CARDINAL) [INLINE];
BEGIN
    (* move bytes until dest is aligned *)
    LOOP
        IF (amount <> 0) AND ((CAST(ADRCARD, ptrD) REM MoveSize) <> 0) THEN
            DEC(amount);
            ptrD^[0].b := ptrS^[0].b;
            ptrS := ADDADR(ptrS, 1);
            ptrD := ADDADR(ptrD, 1);
        ELSE
            EXIT;
        END;
    END;

    (* dest is now aligned *)

    IF (CAST(ADRCARD, ptrS) REM MoveSize) = 0 THEN
        (* great, source is also aligned. can happen with two byte aligned data *)
        LOOP
            IF amount >= MoveSize*4 THEN
                amount := amount - MoveSize*4;
                ptrD^[0].m := ptrS^[0].m;
                ptrD^[1].m := ptrS^[1].m;
                ptrD^[2].m := ptrS^[2].m;
                ptrD^[3].m := ptrS^[3].m;
                ptrS := ADDADR(ptrS, MoveSize*4);
                ptrD := ADDADR(ptrD, MoveSize*4);
                IF amount < MoveSize THEN
                    EXIT;
                END;

            ELSIF amount >= MoveSize*2 THEN
                amount := amount - MoveSize*2;
                ptrD^[0].m := ptrS^[0].m;
                ptrD^[1].m := ptrS^[1].m;
                ptrS := ADDADR(ptrS, MoveSize*2);
                ptrD := ADDADR(ptrD, MoveSize*2);
                IF amount < MoveSize THEN
                    EXIT;
                END;

            ELSIF amount >= MoveSize THEN
                amount := amount - MoveSize;
                ptrD^[0].m := ptrS^[0].m;
                ptrS := ADDADR(ptrS, MoveSize);
                ptrD := ADDADR(ptrD, MoveSize);
                IF amount < MoveSize THEN
                    EXIT;
                END;
            ELSE
                EXIT;
            END;
        END;

    ELSIF (CAST(ADRCARD, ptrS) REM 2) = 0 THEN
        LOOP
            IF amount >= MoveSize THEN
                amount := amount - MoveSize;

                %IF LittleEndian %THEN
                    %IF Bits32 %THEN
                        ptrD^[0].m := (VAL(MoveType, ptrS^[0].wa[0])       ) BOR
                                      (VAL(MoveType, ptrS^[0].wa[1]) SHL 16);
                    %ELSE
                        ptrD^[0].m := (VAL(MoveType, ptrS^[0].wa[0])       ) BOR
                                      (VAL(MoveType, ptrS^[0].wa[1]) SHL 16) BOR
                                      (VAL(MoveType, ptrS^[0].wa[2]) SHL 32) BOR
                                      (VAL(MoveType, ptrS^[0].wa[3]) SHL 48);
                    %END
                %ELSE
                    %IF Bits32 %THEN
                        ptrD^[0].m := (VAL(MoveType, ptrS^[0].wa[0]) SHL 16) BOR
                                      (VAL(MoveType, ptrS^[0].wa[1])       );
                    %ELSE
                        ptrD^[0].m := (VAL(MoveType, ptrS^[0].wa[0]) SHL 48) BOR
                                      (VAL(MoveType, ptrS^[0].wa[1]) SHL 32) BOR
                                      (VAL(MoveType, ptrS^[0].wa[2]) SHL 16) BOR
                                      (VAL(MoveType, ptrS^[0].wa[3])       );
                    %END
                %END

                ptrS := ADDADR(ptrS, MoveSize);
                ptrD := ADDADR(ptrD, MoveSize);
            ELSE
                EXIT;
            END;
        END;

    ELSE
        LOOP
            IF amount >= MoveSize THEN
                amount := amount - MoveSize;

                %IF LittleEndian %THEN
                    %IF Bits32 %THEN
                        ptrD^[0].m := (VAL(MoveType, ptrS^[0].ba[0])       ) BOR
                                      (VAL(MoveType, ptrS^[0].ba[1]) SHL  8) BOR
                                      (VAL(MoveType, ptrS^[0].ba[2]) SHL 16) BOR
                                      (VAL(MoveType, ptrS^[0].ba[3]) SHL 24);
                    %ELSE
                        ptrD^[0].m := (VAL(MoveType, ptrS^[0].ba[0])       ) BOR
                                      (VAL(MoveType, ptrS^[0].ba[1]) SHL  8) BOR
                                      (VAL(MoveType, ptrS^[0].ba[2]) SHL 16) BOR
                                      (VAL(MoveType, ptrS^[0].ba[3]) SHL 24) BOR
                                      (VAL(MoveType, ptrS^[0].ba[4]) SHL 32) BOR
                                      (VAL(MoveType, ptrS^[0].ba[5]) SHL 40) BOR
                                      (VAL(MoveType, ptrS^[0].ba[6]) SHL 48) BOR
                                      (VAL(MoveType, ptrS^[0].ba[7]) SHL 56);
                    %END
                %ELSE
                    %IF Bits32 %THEN
                        ptrD^[0].m := (VAL(MoveType, ptrS^[0].ba[0]) SHL 24) BOR
                                      (VAL(MoveType, ptrS^[0].ba[1]) SHL 16) BOR
                                      (VAL(MoveType, ptrS^[0].ba[2]) SHL  8) BOR
                                      (VAL(MoveType, ptrS^[0].ba[3])       );
                    %ELSE
                        ptrD^[0].m := (VAL(MoveType, ptrS^[0].ba[0]) SHL 56) BOR
                                      (VAL(MoveType, ptrS^[0].ba[1]) SHL 48) BOR
                                      (VAL(MoveType, ptrS^[0].ba[2]) SHL 40) BOR
                                      (VAL(MoveType, ptrS^[0].ba[3]) SHL 32) BOR
                                      (VAL(MoveType, ptrS^[0].ba[4]) SHL 24) BOR
                                      (VAL(MoveType, ptrS^[0].ba[5]) SHL 16) BOR
                                      (VAL(MoveType, ptrS^[0].ba[6]) SHL  8) BOR
                                      (VAL(MoveType, ptrS^[0].ba[7])       );
                    %END
                %END

                ptrS := ADDADR(ptrS, MoveSize);
                ptrD := ADDADR(ptrD, MoveSize);
            ELSE
                EXIT;
            END;
        END;
    END;

    IF amount > 0 THEN
        LOOP
            IF amount >= 3 THEN
                amount := amount - 3;
                ptrD^[0].ba[0] := ptrS^[0].ba[0];
                ptrD^[0].ba[1] := ptrS^[0].ba[1];
                ptrD^[0].ba[2] := ptrS^[0].ba[2];
                ptrS := ADDADR(ptrS, 3);
                ptrD := ADDADR(ptrD, 3);
                IF amount = 0 THEN
                    EXIT;
                END;
            ELSIF amount >= 2 THEN
                amount := amount - 2;
                ptrD^[0].ba[0] := ptrS^[0].ba[0];
                ptrD^[0].ba[1] := ptrS^[0].ba[1];
                ptrS := ADDADR(ptrS, 2);
                ptrD := ADDADR(ptrD, 2);
                IF amount = 0 THEN
                    EXIT;
                END;
            ELSE
                ptrD^[0].ba[0] := ptrS^[0].ba[0];
                EXIT;
            END;
        END;
    END;
END MoveUF;

PROCEDURE MoveUB(ptrD, ptrS : MovePointer; amount : CARDINAL) [INLINE];
BEGIN
    ptrD := ADDADR(ptrD, amount);
    ptrS := ADDADR(ptrS, amount);

    IF (CAST(ADRCARD, ptrD) REM MoveSize) <> 0 THEN
        LOOP
            ptrS := SUBADR(ptrS, 1);
            ptrD := SUBADR(ptrD, 1);

            IF amount <> 0 THEN
                DEC(amount);
                ptrD^[0].b := ptrS^[0].b;

                IF (CAST(ADRCARD, ptrD) REM MoveSize) = 0 THEN
                    EXIT;
                END;
            ELSE
                EXIT;
            END;
        END;
    END;

    (* dest is now aligned *)

    ptrS := SUBADR(ptrS, MoveSize);
    ptrD := SUBADR(ptrD, MoveSize);
    LOOP
        IF amount >= MoveSize THEN
            amount := amount - MoveSize;

            %IF LittleEndian %THEN
                %IF Bits32 %THEN
                    ptrD^[0].m := (VAL(MoveType, ptrS^[0].ba[3]) SHL 24) BOR
                                  (VAL(MoveType, ptrS^[0].ba[2]) SHL 16) BOR
                                  (VAL(MoveType, ptrS^[0].ba[1]) SHL  8) BOR
                                  (VAL(MoveType, ptrS^[0].ba[0])       );
                %ELSE
                    ptrD^[0].m := (VAL(MoveType, ptrS^[0].ba[7]) SHL 56) BOR
                                  (VAL(MoveType, ptrS^[0].ba[6]) SHL 48) BOR
                                  (VAL(MoveType, ptrS^[0].ba[5]) SHL 40) BOR
                                  (VAL(MoveType, ptrS^[0].ba[4]) SHL 32) BOR
                                  (VAL(MoveType, ptrS^[0].ba[3]) SHL 24) BOR
                                  (VAL(MoveType, ptrS^[0].ba[2]) SHL 16) BOR
                                  (VAL(MoveType, ptrS^[0].ba[1]) SHL  8) BOR
                                  (VAL(MoveType, ptrS^[0].ba[0])       );
                %END
            %ELSE
                %IF Bits32 %THEN
                    ptrD^[0].m := (VAL(MoveType, ptrS^[0].ba[3])       ) BOR
                                  (VAL(MoveType, ptrS^[0].ba[2]) SHL  8) BOR
                                  (VAL(MoveType, ptrS^[0].ba[1]) SHL 16) BOR
                                  (VAL(MoveType, ptrS^[0].ba[0]) SHL 24);
                %ELSE
                    ptrD^[0].m := (VAL(MoveType, ptrS^[0].ba[7])       ) BOR
                                  (VAL(MoveType, ptrS^[0].ba[6]) SHL  8) BOR
                                  (VAL(MoveType, ptrS^[0].ba[5]) SHL 16) BOR
                                  (VAL(MoveType, ptrS^[0].ba[4]) SHL 24) BOR
                                  (VAL(MoveType, ptrS^[0].ba[3]) SHL 32) BOR
                                  (VAL(MoveType, ptrS^[0].ba[2]) SHL 40) BOR
                                  (VAL(MoveType, ptrS^[0].ba[1]) SHL 48) BOR
                                  (VAL(MoveType, ptrS^[0].ba[0]) SHL 56);
                %END
            %END

            ptrS := SUBADR(ptrS, MoveSize);
            ptrD := SUBADR(ptrD, MoveSize);
        ELSE
            EXIT;
        END;
    END;

    LOOP
        IF amount <> 0 THEN
            DEC(amount);
            ptrD^[0].ba[MoveSize-1] := ptrS^[0].ba[MoveSize-1];
            ptrS := SUBADR(ptrS, 1);
            ptrD := SUBADR(ptrD, 1);
            IF amount = 0 THEN
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;
END MoveUB;
(*
PROCEDURE MoveMemForward(dest, src : ADDRESS; numBytes : CARDINAL);
BEGIN
    UNREFERENCED_PARAMETER (dest);
    UNREFERENCED_PARAMETER (src);
    UNREFERENCED_PARAMETER (numBytes);
    (* TO BE WRITTEN *)
END MoveMemForward;

PROCEDURE MoveMemBack(dest, src : ADDRESS; numBytes : CARDINAL);
BEGIN
    UNREFERENCED_PARAMETER (dest);
    UNREFERENCED_PARAMETER (src);
    UNREFERENCED_PARAMETER (numBytes);
    (* TO BE WRITTEN *)
END MoveMemBack;
*)
PROCEDURE MoveMem(dest, src : ADDRESS; amount : CARDINAL);
VAR
    addrD, addrS        : ADRCARD;
    tamount             : ADRCARD;
BEGIN
    addrD := CAST(ADRCARD, dest);
    addrS := CAST(ADRCARD, src);

    tamount := amount;
    IF (addrD <= addrS) OR ((addrS+tamount) <= addrD) THEN
        (* move forward *)

        IF ((addrD REM MoveSize) = 0) AND ((addrS REM MoveSize) = 0) THEN
            MoveAF(dest, src, amount);
        ELSE
            MoveUF(dest, src, amount);
        END;
    ELSE
        (* move backwards *)

        IF ((addrD REM MoveSize) = 0) AND ((addrS REM MoveSize) = 0) THEN
            MoveAB(dest, src, amount);
        ELSE
            MoveUB(dest, src, amount);
        END;
    END;
END MoveMem;

%END

END MemUtils.
