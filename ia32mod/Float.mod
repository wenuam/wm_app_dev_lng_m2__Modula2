IMPLEMENTATION MODULE Float;

(* Some primitive real constants and operations for x86-x64 CPUs *)

(* A. Mutylin                                                       January 2017 *)

FROM SYSTEM IMPORT CAST;

IMPORT EXCEPTIONS, M2EXCEPTION;

(* Floating point processor control register operations. *)

CONST
	DefaultMXCSR = 1F80H;

%IF %NOT PICCode %THEN
VAR
	RoundMXCSR : CARDINAL32 = DefaultMXCSR;
%END

PROCEDURE GetPrecision87 () : Precision [Alters(AX)]; PUREASM;
ASM
	%IF IA32 %THEN
		PUSH	AX
		FNSTCW	word ptr [ESP]
		POP		AX
		MOV		AL, AH
	%ELSIF Windows %THEN
		FNSTCW	word ptr [RSP+8]
		MOV		AL, [RSP+9]
	%ELSE
		PUSH	AX
		FNSTCW	word ptr [RSP]
		POP		AX
		MOV		AL, AH
	%END
	AND		AX, 3
	RET
END GetPrecision87;

PROCEDURE GetRounding87 () : RoundingMode [Alters(AX)]; PUREASM;
ASM
	%IF IA32 %THEN
		PUSH	AX
		FNSTCW	word ptr [ESP]
		POP		AX
		SHR		AX, 10
	%ELSIF Windows %THEN
		FNSTCW	word ptr [RSP+8]
		MOV		AL, [RSP+9]
		SHR		AL, 2
	%ELSE
		PUSH	AX
		FNSTCW	word ptr [RSP]
		POP		AX
		SHR		AX, 10
	%END
	AND		AX, 3
	RET
END GetRounding87;

PROCEDURE GetRoundingSSE () : RoundingMode [Alters(AX)]; PUREASM;
ASM
	%IF IA32 %THEN
		PUSH	EAX
		STMXCSR	dword ptr [ESP]
		POP		EAX
		SHR		AX, 13
	%ELSIF Windows %THEN
		STMXCSR	dword ptr [RSP+8]
		MOV		AL, [RSP+9]
		SHR		AL, 5
	%ELSE
		PUSH	RAX
		STMXCSR	dword ptr [RSP]
		POP		RAX
		SHR		AX, 13
	%END
	AND		AX, 3
	RET
END GetRoundingSSE;

PROCEDURE GetFlushToZeroSSE () : BOOLEAN [Alters(AX)]; PUREASM;
ASM
	%IF IA32 %THEN
		PUSH	EAX
		STMXCSR	dword ptr [ESP]
		POP		EAX
		SHR		AX, 15
	%ELSIF Windows %THEN
		STMXCSR	dword ptr [RSP+8]
		MOV		AL, [RSP+9]
		SHR		AL, 7
	%ELSE
		PUSH	RAX
		STMXCSR	dword ptr [RSP]
		POP		RAX
		SHR		AX, 15
	%END
	RET
END GetFlushToZeroSSE;

PROCEDURE GetDenormalsAreZerosSSE () : BOOLEAN [Alters(AX)]; PUREASM;
ASM
	%IF IA32 %THEN
		PUSH	EAX
		STMXCSR	dword ptr [ESP]
		POP		EAX
		MOV		AL, AH
	%ELSIF Windows %THEN
		STMXCSR	dword ptr [RSP+8]
		MOV		AL, [RSP+9]
	%ELSE
		PUSH	RAX
		STMXCSR	dword ptr [RSP]
		POP		RAX
		MOV		AL, AH
	%END
	AND		AX, 1
	RET
END GetDenormalsAreZerosSSE;

PROCEDURE SetPrecision87 (P : Precision) [Pass(BX),Alters()]; PUREASM;
ASM
	%IF IA32 %THEN
		PUSH	AX
		FNSTCW	word ptr [ESP]
		AND		byte ptr [ESP+1], 0FCH
		OR		byte ptr [ESP+1], BL
		FLDCW	word ptr [ESP]
		ADD		ESP, 2
	%ELSIF Windows %THEN
		FNSTCW	word ptr [RSP+8]
		AND		byte ptr [RSP+9], 0FCH
		OR		byte ptr [RSP+9], BL
		FLDCW	word ptr [RSP+8]
	%ELSE
		PUSH	AX
		FNSTCW	word ptr [RSP]
		AND		byte ptr [RSP+1], 0FCH
		OR		byte ptr [RSP+1], BL
		FLDCW	word ptr [RSP]
		ADD		RSP, 2
	%END
	RET
END SetPrecision87;

PROCEDURE SetRounding87 (RM : RoundingMode) [Pass(BX),Alters(BX)]; PUREASM;
ASM
	SHL		BL, 2
	%IF IA32 %THEN
		PUSH	BX
		FNSTCW	word ptr [ESP]
		AND		byte ptr [ESP+1], 0F3H
		OR		byte ptr [ESP+1], BL
		FLDCW	word ptr [ESP]
		POP		BX
	%ELSIF Windows %THEN
		FNSTCW	word ptr [RSP+8]
		AND		byte ptr [RSP+9], 0F3H
		OR		byte ptr [RSP+9], BL
		FLDCW	word ptr [RSP+8]
	%ELSE
		PUSH	BX
		FNSTCW	word ptr [RSP]
		AND		byte ptr [RSP+1], 0F3H
		OR		byte ptr [RSP+1], BL
		FLDCW	word ptr [RSP]
		POP		BX
	%END
	RET
END SetRounding87;

PROCEDURE SetRoundingSSE (RM : RoundingMode) [Pass(BX),Alters(BX)]; PUREASM;
ASM
	SHL		BL, 5
	%IF IA32 %THEN
		PUSH	EBX
		STMXCSR	dword ptr [ESP]
		AND		byte ptr [ESP+1], 9FH
		OR		byte ptr [ESP+1], BL
		LDMXCSR	dword ptr [ESP]
		POP		EBX
	%ELSIF Windows %THEN
		STMXCSR	dword ptr [RSP+8]
		AND		byte ptr [RSP+9], 9FH
		OR		byte ptr [RSP+9], BL
		LDMXCSR	dword ptr [RSP+8]
	%ELSE
		PUSH	RBX
		STMXCSR	dword ptr [RSP]
		AND		byte ptr [RSP+1], 9FH
		OR		byte ptr [RSP+1], BL
		LDMXCSR	dword ptr [RSP]
		POP		RBX
	%END
	RET
END SetRoundingSSE;

%IF IA32 %THEN

PROCEDURE SetRoundingBothInt (RM : RoundingMode) [Pass(BX),Alters(BX)]; PUREASM;
ASM
	SHL		BL, 2
	PUSH	EBX
	FNSTCW	word ptr [ESP]
	AND		byte ptr [ESP+1], 0F3H
	OR		byte ptr [ESP+1], BL
	FLDCW	word ptr [ESP]
	SHL		BL, 3
	STMXCSR	dword ptr [ESP]
	AND		byte ptr [ESP+1], 9FH
	OR		byte ptr [ESP+1], BL
	LDMXCSR	dword ptr [ESP]
	POP		EBX
	RET
END SetRoundingBothInt;

%ELSIF Windows %THEN

PROCEDURE SetRoundingBoth (RM : RoundingMode) [Pass(BX),Alters(BX)]; PUREASM;
ASM
	SHL		BL, 2
	FNSTCW	word ptr [RSP+8]
	AND		byte ptr [RSP+9], 0F3H
	OR		byte ptr [RSP+9], BL
	FLDCW	word ptr [RSP+8]
	SHL		BL, 3
	STMXCSR	dword ptr [RSP+8]
	AND		byte ptr [RSP+9], 9FH
	OR		byte ptr [RSP+9], BL
	LDMXCSR	dword ptr [RSP+8]
	RET
END SetRoundingBoth;

%ELSE

PROCEDURE SetRoundingBoth (RM : RoundingMode) [Pass(BX),Alters(BX)]; PUREASM;
ASM
	SHL		BL, 2
	PUSH	RBX
	FNSTCW	word ptr [RSP]
	AND		byte ptr [RSP+1], 0F3H
	OR		byte ptr [RSP+1], BL
	FLDCW	word ptr [RSP]
	SHL		BL, 3
	STMXCSR	dword ptr [RSP]
	AND		byte ptr [RSP+1], 9FH
	OR		byte ptr [RSP+1], BL
	LDMXCSR	dword ptr [RSP]
	POP		RBX
	RET
END SetRoundingBoth;

%END

PROCEDURE SetFlushToZeroSSE (Bit : BOOLEAN) [Pass(BX),Alters(BX)]; PUREASM;
ASM
	SHL		BL, 7
	%IF IA32 %THEN
		PUSH	EBX
		STMXCSR	dword ptr [ESP]
		AND		byte ptr [ESP+1], 7FH
		OR		byte ptr [ESP+1], BL
		LDMXCSR	dword ptr [ESP]
		POP		EBX
	%ELSIF Windows %THEN
		STMXCSR	dword ptr [RSP+8]
		AND		byte ptr [RSP+9], 7FH
		OR		byte ptr [RSP+9], BL
		LDMXCSR	dword ptr [RSP+8]
	%ELSE
		PUSH	RBX
		STMXCSR	dword ptr [RSP]
		AND		byte ptr [RSP+1], 7FH
		OR		byte ptr [RSP+1], BL
		LDMXCSR	dword ptr [RSP]
		POP		RBX
	%END
	RET
END SetFlushToZeroSSE;

PROCEDURE SetDenormalsAreZerosSSE (Bit : BOOLEAN) [Pass(BX),Alters(BX)]; PUREASM;
ASM
	SHL		BL, 6
	%IF IA32 %THEN
		PUSH	EBX
		STMXCSR	dword ptr [ESP]
		AND		byte ptr [ESP], 0BFH
		OR		byte ptr [ESP], BL
		LDMXCSR	dword ptr [ESP]
		POP		EBX
	%ELSIF Windows %THEN
		STMXCSR	dword ptr [RSP+8]
		AND		byte ptr [RSP+8], 0BFH
		OR		byte ptr [RSP+8], BL
		LDMXCSR	dword ptr [RSP+8]
	%ELSE
		PUSH	RBX
		STMXCSR	dword ptr [RSP]
		AND		byte ptr [RSP], 0BFH
		OR		byte ptr [RSP], BL
		LDMXCSR	dword ptr [RSP]
		POP		RBX
	%END
	RET
END SetDenormalsAreZerosSSE;

PROCEDURE Init87() [Alters()]; PUREASM;
ASM
	FINIT
	RET
END Init87;

PROCEDURE InitSSE() [Alters()]; PUREASM;
ASM
	%IF PICCode %THEN
		%IF Windows %THEN
			MOV		dword ptr [RSP+8], DefaultMXCSR
			LDMXCSR	[RSP+8]
		%ELSE
			PUSH	dword ptr DefaultMXCSR
			LDMXCSR	[RSP]
			ADD		RSP, 4
		%END
	%ELSE
		LDMXCSR RoundMXCSR
	%END
		RET
END InitSSE;

%IF IA32 %THEN

PROCEDURE InitBothInt() [Alters()]; PUREASM;
ASM
	FINIT
	LDMXCSR RoundMXCSR
	RET
END InitBothInt;

%ELSE

PROCEDURE InitBoth() [Alters()]; PUREASM;
ASM
	FINIT
	%IF PICCode %THEN
		%IF Windows %THEN
			MOV		[RSP+8], dword ptr DefaultMXCSR
			LDMXCSR	[RSP+8]
		%ELSE
			PUSH	dword ptr DefaultMXCSR
			LDMXCSR	[RSP]
			ADD		RSP, 4
		%END
	%ELSE
		LDMXCSR RoundMXCSR
	%END
	RET
END InitBoth;

%END

(* Integer part operations *)

%IF IA32 %THEN

PROCEDURE Round (X : LONGREAL) : LONGREAL [Alters()]; PUREASM;
ASM
	FLD 	qword ptr [ESP+4]
	FRNDINT
	RET		8
END Round;

PROCEDURE Round32 (X : REAL) : REAL [Alters()]; PUREASM;
ASM
	FLD		dword ptr [ESP+4]
	FRNDINT
	RET		4
END Round32;

PROCEDURE Nearest (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	FLD 	qword ptr [ESP+4]
	FNSTCW  word ptr [ESP+6]
	FNSTCW  word ptr [ESP+4]
	AND		byte ptr [ESP+5], 0F3H
	FLDCW	word ptr [ESP+4]
	FRNDINT
	FLDCW	word ptr [ESP+6]
	RET		8
END Nearest;

PROCEDURE Nearest32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	FLD		dword ptr [ESP+4]
	FNSTCW  word ptr [ESP+6]
	FNSTCW  word ptr [ESP+4]
	AND		byte ptr [ESP+5], 0F3H
	FLDCW	word ptr [ESP+4]
	FRNDINT
	FLDCW	word ptr [ESP+6]
	RET		4
END Nearest32;

PROCEDURE Trunc (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	FLD 	qword ptr [ESP+4]
	FNSTCW  word ptr [ESP+6]
	FNSTCW  word ptr [ESP+4]
	OR		byte ptr [ESP+5], 0CH
	FLDCW	word ptr [ESP+4]
	FRNDINT
	FLDCW	word ptr [ESP+6]
	RET		8
END Trunc;

PROCEDURE Trunc32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	FLD		dword ptr [ESP+4]
	FNSTCW  word ptr [ESP+6]
	FNSTCW  word ptr [ESP+4]
	OR		byte ptr [ESP+5], 0CH
	FLDCW	word ptr [ESP+4]
	FRNDINT
	FLDCW	word ptr [ESP+6]
	RET		4
END Trunc32;

PROCEDURE Floor (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	FLD 	qword ptr [ESP+4]
	FNSTCW  word ptr [ESP+6]
	FNSTCW  word ptr [ESP+4]
	AND		byte ptr [ESP+5], 0F7H
	OR		byte ptr [ESP+5], 4
	FLDCW	word ptr [ESP+4]
	FRNDINT
	FLDCW	word ptr [ESP+6]
	RET		8
END Floor;

PROCEDURE Floor32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	FLD		dword ptr [ESP+4]
	FNSTCW  word ptr [ESP+6]
	FNSTCW  word ptr [ESP+4]
	AND		byte ptr [ESP+5], 0F7H
	OR		byte ptr [ESP+5], 4
	FLDCW	word ptr [ESP+4]
	FRNDINT
	FLDCW	word ptr [ESP+6]
	RET		4
END Floor32;

PROCEDURE Ceiling (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	FLD 	qword ptr [ESP+4]
	FNSTCW  word ptr [ESP+6]
	FNSTCW  word ptr [ESP+4]
	AND		byte ptr [ESP+5], 0FBH
	OR		byte ptr [ESP+5], 8
	FLDCW	word ptr [ESP+4]
	FRNDINT
	FLDCW	word ptr [ESP+6]
	RET		8
END Ceiling;

PROCEDURE Ceiling32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	FLD		dword ptr [ESP+4]
	FNSTCW  word ptr [ESP+6]
	FNSTCW  word ptr [ESP+4]
	AND		byte ptr [ESP+5], 0FBH
	OR		byte ptr [ESP+5], 8
	FLDCW	word ptr [ESP+4]
	FRNDINT
	FLDCW	word ptr [ESP+6]
	RET		4
END Ceiling32;

%ELSE

PROCEDURE SSE4Round (X : LONGREAL) : LONGREAL [Alters()]; PUREASM;
ASM
	ROUNDSD XMM0, XMM0, 4
	RET
END SSE4Round;

PROCEDURE SSE4Round32 (X : REAL) : REAL [Alters()]; PUREASM;
ASM
	ROUNDSS	XMM0, XMM0, 4
	RET
END SSE4Round32;

PROCEDURE SSE4Nearest (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	ROUNDSD XMM0, XMM0, 0
	RET
END SSE4Nearest;

PROCEDURE SSE4Nearest32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	ROUNDSS XMM0, XMM0, 0
	RET
END SSE4Nearest32;

PROCEDURE SSE4Trunc (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	ROUNDSD XMM0, XMM0, 3
	RET
END SSE4Trunc;

PROCEDURE SSE4Trunc32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	ROUNDSS XMM0, XMM0, 3
	RET
END SSE4Trunc32;

PROCEDURE SSE4Floor (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	ROUNDSD XMM0, XMM0, 1
	RET
END SSE4Floor;

PROCEDURE SSE4Floor32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	ROUNDSS XMM0, XMM0, 1
	RET
END SSE4Floor32;

PROCEDURE SSE4Ceiling (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	ROUNDSD XMM0, XMM0, 2
	RET
END SSE4Ceiling;

PROCEDURE SSE4Ceiling32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	ROUNDSS XMM0, XMM0, 2
	RET
END SSE4Ceiling32;

(* Same procedures for CPU without SSE4 *)

PROCEDURE SSE2Round (X : LONGREAL) : LONGREAL [Alters()]; PUREASM;
ASM
	%IF Windows %THEN	(* save RAX *)
		MOV		qword ptr [RSP+8], RAX
	%ELSE
		PUSH	RAX
	%END
		CVTSD2SI RAX, XMM0
		CMP		RAX,
		JE		@end (* number is too too big, XMM0 content should not be changed *)
		CVTSI2SD XMM0, RAX
	%IF Windows %THEN	(* restore RAX *)
@end:	MOV		RAX, qword ptr [RSP+8]
	%ELSE
@end:	POP		RAX
	%END
		RET
END SSE2Round;

PROCEDURE SSE2Round32 (X : REAL) : REAL [Alters()]; PUREASM;
ASM
	%IF Windows %THEN	(* save RAX *)
		MOV		qword ptr [RSP+8], RAX
	%ELSE
		PUSH	RAX
	%END
		CVTSS2SI EAX, XMM0
		CMP		EAX, 80000000H
		JE		@end (* number is too too big, XMM0 content should not be changed *)
		CVTSI2SS XMM0, EAX
	%IF Windows %THEN	(* restore RAX *)
@end:	MOV		RAX, qword ptr [RSP+8]
	%ELSE
@end:	POP		RAX
	%END
		RET
END SSE2Round32;

PROCEDURE SSE2Nearest (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	%IF Windows %THEN
		MOV		qword ptr [RSP+8], RAX	(* save RAX *)
		STMXCSR	[RSP+16]
		STMXCSR	[RSP+20]
		AND		byte ptr [RSP+17], 9FH
		LDMXCSR	[RSP+16]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+20]
	%ELSE
		PUSH	RAX		(* save RAX *)
		PUSH 	RAX		(* make room in the stack *)
		STMXCSR	[RSP]
		STMXCSR	[RSP+4]
		AND		byte ptr [RSP+1], 9FH
		LDMXCSR	[RSP]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+4]
	%END
		DEC		RAX
		INC		RAX
		JO		@end (* number is too too big, XMM0 content should not be changed *)
		CVTSI2SD XMM0, RAX
	%IF Windows %THEN
@end:	MOV		RAX, qword ptr [RSP+8]	(* restore RAX *)
	%ELSE
@end:	POP		RAX	(* eliminate unnecessary room in the stack *)
		POP		RAX	(* restore RAX *)
	%END
		RET
END SSE2Nearest;

PROCEDURE SSE2Nearest32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	%IF Windows %THEN
		MOV		qword ptr [RSP+8], RAX	(* save RAX *)
		STMXCSR	[RSP+16]
		STMXCSR	[RSP+20]
		AND		byte ptr [RSP+17], 9FH
		LDMXCSR	[RSP+16]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+20]
	%ELSE
		PUSH	RAX		(* save RAX *)
		PUSH 	RAX		(* make room in the stack *)
		STMXCSR	[RSP]
		STMXCSR	[RSP+4]
		AND		byte ptr [RSP+1], 9FH
		LDMXCSR	[RSP]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+4]
	%END
		CMP		EAX, 80000000H
		JE		@end (* number is too too big, XMM0 content should not be changed *)
		CVTSI2SS XMM0, EAX
	%IF Windows %THEN
@end:	MOV		RAX, qword ptr [RSP+8]	(* restore RAX *)
	%ELSE
@end:	POP		RAX	(* eliminate unnecessary room in the stack *)
		POP		RAX	(* restore RAX *)
	%END
		RET
END SSE2Nearest32;

PROCEDURE SSE2Trunc (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	%IF Windows %THEN	(* save RAX *)
		MOV		qword ptr [RSP+8], RAX
	%ELSE
		PUSH	RAX
	%END
		CVTTSD2SI RAX, XMM0
		DEC		RAX
		INC		RAX
		JO		@end (* number is too too big, XMM0 content should not be changed *)
		CVTSI2SD XMM0, RAX
	%IF Windows %THEN	(* restore RAX *)
@end:	MOV		RAX, qword ptr [RSP+8]
	%ELSE
@end:	POP		RAX
	%END
		RET
END SSE2Trunc;

PROCEDURE SSE2Trunc32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	%IF Windows %THEN	(* save RAX *)
		MOV		qword ptr [RSP+8], RAX
	%ELSE
		PUSH	RAX
	%END
		CVTTSS2SI EAX, XMM0
		CMP		EAX, 80000000H
		JE		@end (* number is too too big, XMM0 content should not be changed *)
		CVTSI2SS XMM0, EAX
	%IF Windows %THEN	(* restore RAX *)
@end:	MOV		RAX, qword ptr [RSP+8]
	%ELSE
@end:	POP		RAX
	%END
		RET
END SSE2Trunc32;

PROCEDURE SSE2Floor (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	%IF Windows %THEN
		MOV		qword ptr [RSP+8], RAX	(* save RAX *)
		STMXCSR	[RSP+16]
		STMXCSR	[RSP+20]
		AND		byte ptr [RSP+17], 0BFH
		OR		byte ptr [RSP+17], 20H
		LDMXCSR	[RSP+16]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+20]
	%ELSE
		PUSH	RAX		(* save RAX *)
		PUSH 	RAX		(* make room in the stack *)
		STMXCSR	[RSP]
		STMXCSR	[RSP+4]
		AND		byte ptr [RSP+1], 0BFH
		OR		byte ptr [RSP+1], 20H
		LDMXCSR	[RSP]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+4]
	%END
		DEC		RAX
		INC		RAX
		JO		@end (* number is too too big, XMM0 content should not be changed *)
		CVTSI2SD XMM0, RAX
	%IF Windows %THEN
@end:	MOV		RAX, qword ptr [RSP+8]	(* restore RAX *)
	%ELSE
@end:	POP		RAX	(* eliminate unnecessary room in the stack *)
		POP		RAX	(* restore RAX *)
	%END
		RET
END SSE2Floor;

PROCEDURE SSE2Floor32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	%IF Windows %THEN
		MOV		qword ptr [RSP+8], RAX	(* save RAX *)
		STMXCSR	[RSP+16]
		STMXCSR	[RSP+20]
		AND		byte ptr [RSP+17], 0BFH
		OR		byte ptr [RSP+17], 20H
		LDMXCSR	[RSP+16]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+20]
	%ELSE
		PUSH	RAX		(* save RAX *)
		PUSH 	RAX		(* make room in the stack *)
		STMXCSR	[RSP]
		STMXCSR	[RSP+4]
		AND		byte ptr [RSP+1], 0BFH
		OR		byte ptr [RSP+1], 20H
		LDMXCSR	[RSP]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+4]
	%END
		CMP		EAX, 80000000H
		JE		@end (* number is too too big, XMM0 content should not be changed *)
		CVTSI2SS XMM0, EAX
	%IF Windows %THEN
@end:	MOV		RAX, qword ptr [RSP+8]	(* restore RAX *)
	%ELSE
@end:	POP		RAX	(* eliminate unnecessary room in the stack *)
		POP		RAX	(* restore RAX *)
	%END
		RET
END SSE2Floor32;

PROCEDURE SSE2Ceiling (X : LONGREAL) : LONGREAL [Invariant, Alters()]; PUREASM;
ASM
	%IF Windows %THEN
		MOV		qword ptr [RSP+8], RAX	(* save RAX *)
		STMXCSR	[RSP+16]
		STMXCSR	[RSP+20]
		AND		byte ptr [RSP+17], 0DFH
		OR		byte ptr [RSP+17], 40H
		LDMXCSR	[RSP+16]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+20]
	%ELSE
		PUSH	RAX		(* save RAX *)
		PUSH 	RAX		(* make room in the stack *)
		STMXCSR	[RSP]
		STMXCSR	[RSP+4]
		AND		byte ptr [RSP+1], 0DFH
		OR		byte ptr [RSP+1], 40H
		LDMXCSR	[RSP]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+4]
	%END
		DEC		RAX
		INC		RAX
		JO		@end (* number is too too big, XMM0 content should not be changed *)
		CVTSI2SD XMM0, RAX
	%IF Windows %THEN
@end:	MOV		RAX, qword ptr [RSP+8]	(* restore RAX *)
	%ELSE
@end:	POP		RAX	(* eliminate unnecessary room in the stack *)
		POP		RAX	(* restore RAX *)
	%END
		RET
END SSE2Ceiling;

PROCEDURE SSE2Ceiling32 (X : REAL) : REAL [Invariant, Alters()]; PUREASM;
ASM
	%IF Windows %THEN
		MOV		qword ptr [RSP+8], RAX	(* save RAX *)
		STMXCSR	[RSP+16]
		STMXCSR	[RSP+20]
		AND		byte ptr [RSP+17], 0DFH
		OR		byte ptr [RSP+17], 40H
		LDMXCSR	[RSP+16]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+20]
	%ELSE
		PUSH	RAX		(* save RAX *)
		PUSH 	RAX		(* make room in the stack *)
		STMXCSR	[RSP]
		STMXCSR	[RSP+4]
		AND		byte ptr [RSP+1], 0DFH
		OR		byte ptr [RSP+1], 40H
		LDMXCSR	[RSP]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+4]
	%END
		CMP		EAX, 80000000H
		JE		@end (* number is too too big, XMM0 content should not be changed *)
		CVTSI2SS XMM0, EAX
	%IF Windows %THEN
@end:	MOV		RAX, qword ptr [RSP+8]	(* restore RAX *)
	%ELSE
@end:	POP		RAX	(* eliminate unnecessary room in the stack *)
		POP		RAX	(* restore RAX *)
	%END
		RET
END SSE2Ceiling32;

%END

(* Real to integer convert operations *)

CONST
	ConvErrorMessage = "Real to Integer Overflow";
	ConvErrorMessageLen = LENGTH(ConvErrorMessage);

VAR
	ConvErrorMessageVar : ARRAY [0..ConvErrorMessageLen] OF CHAR = ConvErrorMessage;

%IF IA32 %THEN

PROCEDURE RoundToInt64 (X : LONGREAL) : INTEGER64 [Alters(AX,DX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		SUB		ESP, 8
		FISTP	qword ptr [ESP]
		POP		EAX
		POP		EDX
		CMP		EDX, 80000000H
		JNE		@good
		CMP		EAX, 0
		JNE		@good
		(* Check on special case : source is exactly -2**63 *)
		CMP		dword ptr [ESP+8], 0C003E000H
		JNE		Error8
		CMP		dword ptr [ESP+4], 0
		JNE		Error8
@good :	RET		8
END RoundToInt64;

PROCEDURE RoundToInt32 (X : LONGREAL) : INTEGER32 [Alters(AX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		FISTP	qword ptr [ESP+4]
		MOV		EAX, dword ptr [ESP+4]
		CMP		EAX, 0
		JS		@neg
		CMP		dword ptr [ESP+8], 0
		JNE		Error8
		RET		8
@neg :	CMP		dword ptr [ESP+8], -1
		JNE		Error8
		RET		8
END RoundToInt32;

PROCEDURE RoundToInt16 (X : LONGREAL) : INTEGER16 [Alters(AX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		FISTP	dword ptr [ESP+4]
		MOV		EAX, dword ptr [ESP+4]
		CMP		EAX, -8000H
		JL		Error8
		CMP		EAX, 8000H
		JGE		Error8
		RET		8
END RoundToInt16;

PROCEDURE Round32ToInt64 (X : REAL) : INTEGER64 [Alters(AX,DX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		SUB 	ESP, 8
		FISTP	qword ptr [ESP]
		POP		EAX
		POP		EDX
		CMP		EDX, 80000000H
		JNE		@good
		CMP		EAX, 0
		JNE		@good
		(* Check on special case : source is exactly -2**63 *)
		CMP		dword ptr [ESP+4], 0DF000000H
		JNE		Error4
@good :	RET		4
END Round32ToInt64;

PROCEDURE Round32ToInt32 (X : REAL) : INTEGER32 [Alters(AX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		PUSH	EAX		(* Make a room in the stack *)
		FISTP	dword ptr [ESP]
		POP		EAX
		CMP		EAX, 80000000H
		JNE		@good
		(* Check on special case : source is exactly -2**31 *)
		CMP		dword ptr [ESP+4], 0CF000000H
		JNE		Error4
@good :	RET		4
END Round32ToInt32;

PROCEDURE Round32ToInt16 (X : REAL) : INTEGER16 [Alters(AX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		FISTP	dword ptr [ESP+4]
		MOV		EAX, dword ptr [ESP+4]
		CMP		EAX, -8000H
		JL		Error4
		CMP		EAX, 8000H
		JGE		Error4
		RET		4
END Round32ToInt16;

PROCEDURE NearestToInt64 (X : LONGREAL) : INTEGER64 [Invariant, Alters(AX,DX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 12
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0F3H
		FLDCW	word ptr [ESP+8]
		FISTP	qword ptr [ESP]
		POP		EAX
		POP		EDX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EDX, 80000000H
		JNE		@good
		CMP		EAX, 0
		JNE		@good
		(* Check on special case : source is exactly -2**63 *)
		CMP		dword ptr [ESP+8], 0C3E00000H
		JNE		Error8
		CMP		dword ptr [ESP+4], 0
		JNE		Error8
@good :	RET		8
END NearestToInt64;

PROCEDURE NearestToInt32 (X : LONGREAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		PUSH	EAX		(* make room in the stack *)
		FNSTCW	word ptr [ESP+2]
		FNSTCW	word ptr [ESP]
		AND		byte ptr [ESP+1], 0F3H
		FLDCW	word ptr [ESP]
		FISTP	qword ptr [ESP+8]
		FLDCW	word ptr [ESP+2]
		POP		EAX
		MOV		EAX, [ESP+4]
		CMP		EAX, 0
		JS		@neg
		CMP		dword ptr [ESP+8], 0
		JNE		Error8
		RET		8
@neg :	CMP		dword ptr [ESP+8], -1
		JNE		Error8
		RET		8
END NearestToInt32;

PROCEDURE NearestToInt16 (X : LONGREAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0F3H
		FLDCW	word ptr [ESP+8]
		FISTP	dword ptr [ESP+4]
		FLDCW	word ptr [ESP+10]
		MOV		EAX, [ESP+4]
		CMP		EAX, -8000H
		JL		Error8
		CMP		EAX, 8000H
		JGE		Error8
		RET		8
END NearestToInt16;

PROCEDURE Nearest32ToInt64 (X : REAL) : INTEGER64 [Invariant, Alters(AX,DX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 12
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0F3H
		FLDCW	word ptr [ESP+8]
		FISTP	qword ptr [ESP]
		POP		EAX
		POP		EDX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EDX, 80000000H
		JNE		@good
		CMP		EAX, 0
		JNE		@good
		(* Check on special case : source is exactly -2**63 *)
		CMP		dword ptr [ESP+4], 0DF000000H
		JNE		Error4
@good :	RET		4
END Nearest32ToInt64;

PROCEDURE Nearest32ToInt32 (X : REAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 8
		FNSTCW	word ptr [ESP+6]
		FNSTCW	word ptr [ESP+4]
		AND		byte ptr [ESP+5], 0F3H
		FLDCW	word ptr [ESP+4]
		FISTP	dword ptr [ESP]
		POP		EAX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EAX, 80000000H
		JNE		@good
		(* Check on special case : source is exactly -2**31 *)
		CMP		dword ptr [ESP+4], 0CF000000H
		JNE		Error4
@good :	RET		4
END Nearest32ToInt32;

PROCEDURE Nearest32ToInt16 (X : REAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		PUSH	EAX		(* make room in the stack *)
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0F3H
		FLDCW	word ptr [ESP+8]
		FISTP	dword ptr [ESP]
		FLDCW	word ptr [ESP+10]
		POP		EAX
		CMP		EAX, -8000H
		JL		Error4
		CMP		EAX, 8000H
		JGE		Error4
		RET		4
END Nearest32ToInt16;

PROCEDURE TruncToInt64 (X : LONGREAL) : INTEGER64 [Invariant, Alters(AX,DX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 12
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		OR		byte ptr [ESP+9], 0CH
		FLDCW	word ptr [ESP+8]
		FISTP	qword ptr [ESP]
		POP		EAX
		POP		EDX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EDX, 80000000H
		JNE		@good
		CMP		EAX, 0
		JNE		@good
		(* Check on special case : source is exactly -2**63 *)
		CMP		dword ptr [ESP+8], 0C3E00000H
		JNE		Error8
		CMP		dword ptr [ESP+4], 0
		JNE		Error8
@good :	RET		8
END TruncToInt64;

PROCEDURE TruncToInt32 (X : LONGREAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		PUSH	EAX		(* make room in the stack *)
		FNSTCW	word ptr [ESP+2]
		FNSTCW	word ptr [ESP]
		OR		byte ptr [ESP+1], 0CH
		FLDCW	word ptr [ESP]
		FISTP	qword ptr [ESP+8]
		FLDCW	word ptr [ESP+2]
		POP		EAX
		MOV		EAX, [ESP+4]
		CMP		EAX, 0
		JS		@neg
		CMP		dword ptr [ESP+8], 0
		JNE		Error8
		RET		8
@neg :	CMP		dword ptr [ESP+8], -1
		JNE		Error8
		RET		8
END TruncToInt32;

PROCEDURE TruncToInt16 (X : LONGREAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		OR		byte ptr [ESP+9], 0CH
		FLDCW	word ptr [ESP+8]
		FISTP	dword ptr [ESP+4]
		FLDCW	word ptr [ESP+10]
		MOV		EAX, [ESP+4]
		CMP		EAX, -8000H
		JL		Error8
		CMP		EAX, 8000H
		JGE		Error8
		RET		8
END TruncToInt16;

PROCEDURE Trunc32ToInt64 (X : REAL) : INTEGER64 [Invariant, Alters(AX,DX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 12
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		OR		byte ptr [ESP+9], 0CH
		FLDCW	word ptr [ESP+8]
		FISTP	qword ptr [ESP]
		POP		EAX
		POP		EDX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EDX, 80000000H
		JNE		@good
		CMP		EAX, 0
		JNE		@good
		(* Check on special case : source is exactly -2**63 *)
		CMP		dword ptr [ESP+4], 0DF000000H
		JNE		Error4
@good :	RET		4
END Trunc32ToInt64;

PROCEDURE Trunc32ToInt32 (X : REAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 8
		FNSTCW	word ptr [ESP+6]
		FNSTCW	word ptr [ESP+4]
		OR		byte ptr [ESP+5], 0CH
		FLDCW	word ptr [ESP+4]
		FISTP	dword ptr [ESP]
		POP		EAX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EAX, 80000000H
		JNE		@good
		(* Check on special case : source is exactly -2**31 *)
		CMP		dword ptr [ESP+4], 0CF000000H
		JNE		Error4
@good :	RET		4
END Trunc32ToInt32;

PROCEDURE Trunc32ToInt16 (X : REAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		PUSH	EAX		(* make room in the stack *)
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		OR		byte ptr [ESP+9], 0CH
		FLDCW	word ptr [ESP+8]
		FISTP	dword ptr [ESP]
		FLDCW	word ptr [ESP+10]
		POP		EAX
		CMP		EAX, -8000H
		JL		Error4
		CMP		EAX, 8000H
		JGE		Error4
		RET		4
END Trunc32ToInt16;

PROCEDURE FloorToInt64 (X : LONGREAL) : INTEGER64 [Invariant, Alters(AX,DX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 12
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0F7H
		OR		byte ptr [ESP+9], 4
		FLDCW	word ptr [ESP+8]
		FISTP	qword ptr [ESP]
		POP		EAX
		POP		EDX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EDX, 80000000H
		JNE		@good
		CMP		EAX, 0
		JNE		@good
		(* Check on special case : source is exactly -2**63 *)
		CMP		dword ptr [ESP+8], 0C3E00000H
		JNE		Error8
		CMP		dword ptr [ESP+4], 0
		JNE		Error8
@good :	RET		8
END FloorToInt64;

PROCEDURE FloorToInt32 (X : LONGREAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		PUSH	EAX		(* make room in the stack *)
		FNSTCW	word ptr [ESP+2]
		FNSTCW	word ptr [ESP]
		AND		byte ptr [ESP+1], 0F7H
		OR		byte ptr [ESP+1], 4
		FLDCW	word ptr [ESP]
		FISTP	qword ptr [ESP+8]
		FLDCW	word ptr [ESP+2]
		POP		EAX
		MOV		EAX, [ESP+4]
		CMP		EAX, 0
		JS		@neg
		CMP		dword ptr [ESP+8], 0
		JNE		Error8
		RET		8
@neg :	CMP		dword ptr [ESP+8], -1
		JNE		Error8
		RET		8
END FloorToInt32;

PROCEDURE FloorToInt16 (X : LONGREAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0F7H
		OR		byte ptr [ESP+9], 4
		FLDCW	word ptr [ESP+8]
		FISTP	dword ptr [ESP+4]
		FLDCW	word ptr [ESP+10]
		MOV		EAX, [ESP+4]
		CMP		EAX, -8000H
		JL		Error8
		CMP		EAX, 8000H
		JGE		Error8
		RET		8
END FloorToInt16;

PROCEDURE Floor32ToInt64 (X : REAL) : INTEGER64 [Invariant, Alters(AX,DX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 12
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0F7H
		OR		byte ptr [ESP+9], 4
		FLDCW	word ptr [ESP+8]
		FISTP	qword ptr [ESP]
		POP		EAX
		POP		EDX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EDX, 80000000H
		JNE		@good
		CMP		EAX, 0
		JNE		@good
		(* Check on special case : source is exactly -2**63 *)
		CMP		dword ptr [ESP+4], 0DF000000H
		JNE		Error4
@good :	RET		4
END Floor32ToInt64;

PROCEDURE Floor32ToInt32 (X : REAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 8
		FNSTCW	word ptr [ESP+6]
		FNSTCW	word ptr [ESP+4]
		AND		byte ptr [ESP+5], 0F7H
		OR		byte ptr [ESP+5], 4
		FLDCW	word ptr [ESP+4]
		FISTP	dword ptr [ESP]
		POP		EAX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EAX, 80000000H
		JNE		@good
		(* Check on special case : source is exactly -2**31 *)
		CMP		dword ptr [ESP+4], 0CF000000H
		JNE		Error4
@good :	RET		4
END Floor32ToInt32;

PROCEDURE Floor32ToInt16 (X : REAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		PUSH	EAX		(* make room in the stack *)
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0F7H
		OR		byte ptr [ESP+9], 4
		FLDCW	word ptr [ESP+8]
		FISTP	dword ptr [ESP]
		FLDCW	word ptr [ESP+10]
		POP		EAX
		CMP		EAX, -8000H
		JL		Error4
		CMP		EAX, 8000H
		JGE		Error4
		RET		4
END Floor32ToInt16;

PROCEDURE CeilingToInt64 (X : LONGREAL) : INTEGER64 [Invariant, Alters(AX,DX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 12
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0FBH
		OR		byte ptr [ESP+9], 8
		FLDCW	word ptr [ESP+8]
		FISTP	qword ptr [ESP]
		POP		EAX
		POP		EDX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EDX, 80000000H
		JNE		@good
		CMP		EAX, 0
		JNE		@good
		(* Check on special case : source is exactly -2**63 *)
		CMP		dword ptr [ESP+8], 0C3E00000H
		JNE		Error8
		CMP		dword ptr [ESP+4], 0
		JNE		Error8
@good :	RET		8
END CeilingToInt64;

PROCEDURE CeilingToInt32 (X : LONGREAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		PUSH	EAX		(* make room in the stack *)
		FNSTCW	word ptr [ESP+2]
		FNSTCW	word ptr [ESP]
		AND		byte ptr [ESP+1], 0FBH
		OR		byte ptr [ESP+1], 8
		FLDCW	word ptr [ESP]
		FISTP	qword ptr [ESP+8]
		FLDCW	word ptr [ESP+2]
		POP		EAX
		MOV		EAX, [ESP+4]
		CMP		EAX, 0
		JS		@neg
		CMP		dword ptr [ESP+8], 0
		JNE		Error8
		RET		8
@neg :	CMP		dword ptr [ESP+8], -1
		JNE		Error8
		RET		8
END CeilingToInt32;

PROCEDURE CeilingToInt16 (X : LONGREAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		qword ptr [ESP+4]
		(* Set rounding mode *)
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0FBH
		OR		byte ptr [ESP+9], 8
		FLDCW	word ptr [ESP+8]
		FISTP	dword ptr [ESP+4]
		FLDCW	word ptr [ESP+10]
		MOV		EAX, [ESP+4]
		CMP		EAX, -8000H
		JL		Error8
		CMP		EAX, 8000H
		JGE		Error8
		RET		8
END CeilingToInt16;

PROCEDURE Ceiling32ToInt64 (X : REAL) : INTEGER64 [Invariant, Alters(AX,DX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 12
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0FBH
		OR		byte ptr [ESP+9], 8
		FLDCW	word ptr [ESP+8]
		FISTP	qword ptr [ESP]
		POP		EAX
		POP		EDX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EDX, 80000000H
		JNE		@good
		CMP		EAX, 0
		JNE		@good
		(* Check on special case : source is exactly -2**63 *)
		CMP		dword ptr [ESP+4], 0DF000000H
		JNE		Error4
@good :	RET		4
END Ceiling32ToInt64;

PROCEDURE Ceiling32ToInt32 (X : REAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		SUB		ESP, 8
		FNSTCW	word ptr [ESP+6]
		FNSTCW	word ptr [ESP+4]
		AND		byte ptr [ESP+5], 0FBH
		OR		byte ptr [ESP+5], 8
		FLDCW	word ptr [ESP+4]
		FISTP	dword ptr [ESP]
		POP		EAX
		FLDCW	word ptr [ESP+2]
		ADD		ESP, 4
		CMP		EAX, 80000000H
		JNE		@good
		(* Check on special case : source is exactly -2**31 *)
		CMP		dword ptr [ESP+4], 0CF000000H
		JNE		Error4
@good :	RET		4
END Ceiling32ToInt32;

PROCEDURE Ceiling32ToInt16 (X : REAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
		FLD		dword ptr [ESP+4]
		(* Set rounding mode *)
		PUSH	EAX		(* make room in the stack *)
		FNSTCW	word ptr [ESP+10]
		FNSTCW	word ptr [ESP+8]
		AND		byte ptr [ESP+9], 0FBH
		OR		byte ptr [ESP+9], 8
		FLDCW	word ptr [ESP+8]
		FISTP	dword ptr [ESP]
		FLDCW	word ptr [ESP+10]
		POP		EAX
		CMP		EAX, -8000H
		JL		Error4
		CMP		EAX, 8000H
		JGE		Error4
		RET		4
END Ceiling32ToInt16;

PROCEDURE Error8; PUREASM;
ASM
		POP		EAX		(* get return address *)
		MOV		dword ptr [ESP+4], M2EXCEPTION.wholeValueException
		MOV		dword ptr [ESP], ConvErrorMessageLen
		PUSH	offset ConvErrorMessageVar
		PUSH	EAX		(* restore return address *)
		JMP		M2EXCEPTION.RaiseM2Exception
END Error8;

PROCEDURE Error4; PUREASM;
ASM
		POP		EAX		(* get return address *)
		MOV		dword ptr [ESP], M2EXCEPTION.wholeValueException
		PUSH	ConvErrorMessageLen
		PUSH	offset ConvErrorMessageVar
		PUSH	EAX		(* restore return address *)
		JMP		M2EXCEPTION.RaiseM2Exception
END Error4;

%ELSE

PROCEDURE RoundToInt64 (X : LONGREAL) : INTEGER64 [Alters(AX)]; PUREASM;
ASM
		CVTSD2SI RAX, XMM0
		DEC		RAX
		INC		RAX
		JNO		@good
		(* Check on special case : source is exactly -2**63 *)
	%IF Windows %THEN
		MOVSD	qword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+12], 0C3E00000H
		JNE		Error
		CMP		dword ptr [RSP+8], 0
		JNE		Error
	%ELSE
		PUSH	RAX
		MOVSD	qword ptr [RSP], XMM0
		CMP		dword ptr [RSP+4], 0C3E00000H
		JNE		@bad0
		CMP		dword ptr [RSP], 0
		JE		@good0
@bad0 :	POP		RAX
		JMP		Error
@good0:	ADD		RSP, 8
	%END
@good :	RET
END RoundToInt64;

PROCEDURE RoundToInt32 (X : LONGREAL) : INTEGER32 [Alters(AX)]; PUREASM;
ASM
		CVTSD2SI RAX, XMM0
		CMP		RAX, -80000000H
		JL		Error
		CMP		RAX, 7FFFFFFFH
		JG		Error
		RET
END RoundToInt32;

PROCEDURE RoundToInt16 (X : LONGREAL) : INTEGER16 [Alters(AX)]; PUREASM;
ASM
		CVTSD2SI EAX, XMM0
		CMP		EAX, -8000H
		JL		Error
		CMP		EAX, 7FFFH
		JG		Error
		RET
END RoundToInt16;

PROCEDURE Round32ToInt64 (X : REAL) : INTEGER64 [Alters(AX)]; PUREASM;
ASM
		CVTSS2SI RAX, XMM0
		DEC		RAX
		INC		RAX
		JNO		@good
		(* Check on special case : source is exactly -2**63 *)
	%IF Windows %THEN
		MOVSS	dword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+8], 0DF000000H
		JNE		Error
	%ELSE
		PUSH	RAX
		MOVSS	dword ptr [RSP], XMM0
		CMP		dword ptr [RSP], 0DF000000H
		JE		@good0
		POP		RAX
		JMP		Error
@good0:	ADD		RSP, 8
	%END
@good :	RET
END Round32ToInt64;

PROCEDURE Round32ToInt32 (X : REAL) : INTEGER32 [Alters(AX)]; PUREASM;
ASM
		CVTSS2SI RAX, XMM0
		CMP		RAX, -80000000H
		JL		Error
		CMP		RAX, 7FFFFFFFH
		JG		Error
		RET
END Round32ToInt32;

PROCEDURE Round32ToInt16 (X : REAL) : INTEGER16 [Alters(AX)]; PUREASM;
ASM
		CVTSS2SI EAX, XMM0
		CMP		EAX, -8000H
		JL		Error
		CMP		EAX, 7FFFH
		JG		Error
		RET
END Round32ToInt16;

PROCEDURE NearestToInt64 (X : LONGREAL) : INTEGER64 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 9FH
		LDMXCSR	[RSP+8]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+12]
		DEC		RAX
		INC		RAX
		JNO		@good
		(* Check on special case : source is exactly -2**63 *)
		MOVSD	qword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+12], 0C3E00000H
		JNE		Error
		CMP		dword ptr [RSP+8], 0
		JNE		Error
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 9FH
		LDMXCSR	[RSP]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		DEC		RAX
		INC		RAX
		JNO		@good0
		(* Check on special case : source is exactly -2**63 *)
		MOVSD	qword ptr [RSP], XMM0
		CMP		dword ptr [RSP+4], 0C3E00000H
		JNE		@bad0
		CMP		dword ptr [RSP], 0
		JE		@good0
@bad0 :	POP		RAX
		JMP		Error
@good0:	ADD		RSP, 8
	%END
@good :	RET
END NearestToInt64;

PROCEDURE NearestToInt32 (X : LONGREAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 9FH
		LDMXCSR	[RSP+8]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 9FH
		LDMXCSR	[RSP]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		RAX, -80000000H
		JL		Error
		CMP		RAX, 7FFFFFFFH
		JG		Error
		RET
END NearestToInt32;

PROCEDURE NearestToInt16 (X : LONGREAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 9FH
		LDMXCSR	[RSP+8]
		CVTSD2SI EAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 9FH
		LDMXCSR	[RSP]
		CVTSD2SI EAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		EAX, -8000H
		JL		Error
		CMP		EAX, 7FFFH
		JG		Error
		RET
END NearestToInt16;

PROCEDURE Nearest32ToInt64 (X : REAL) : INTEGER64 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 9FH
		LDMXCSR	[RSP+8]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+12]
		DEC		RAX
		INC		RAX
		JNO		@good
		(* Check on special case : source is exactly -2**63 *)
		MOVSS	dword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+8], 0DF000000H
		JNE		Error
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 9FH
		LDMXCSR	[RSP]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		DEC		RAX
		INC		RAX
		JNO		@good0
		(* Check on special case : source is exactly -2**63 *)
		MOVSS	dword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+8], 0DF000000H
		JE		@good0
		POP		RAX
		JMP		Error
@good0:	ADD		RSP, 8
	%END
@good :	RET
END Nearest32ToInt64;

PROCEDURE Nearest32ToInt32 (X : REAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 9FH
		LDMXCSR	[RSP+8]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 9FH
		LDMXCSR	[RSP]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		RAX, -80000000H
		JL		Error
		CMP		RAX, 7FFFFFFFH
		JG		Error
		RET
END Nearest32ToInt32;

PROCEDURE Nearest32ToInt16 (X : REAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 9FH
		LDMXCSR	[RSP+8]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 9FH
		LDMXCSR	[RSP]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		EAX, -8000H
		JL		Error
		CMP		EAX, 7FFFH
		JG		Error
		RET
END Nearest32ToInt16;

PROCEDURE TruncToInt64 (X : LONGREAL) : INTEGER64 [Invariant, Alters(AX)]; PUREASM;
ASM
		CVTTSD2SI RAX, XMM0
		DEC		RAX
		INC		RAX
		JNO		@good
		(* Check on special case : source is exactly -2**63 *)
	%IF Windows %THEN
		MOVSD	qword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+12], 0C3E00000H
		JNE		Error
		CMP		dword ptr [RSP+8], 0
		JNE		Error
	%ELSE
		PUSH	RAX
		MOVSD	qword ptr [RSP], XMM0
		CMP		dword ptr [RSP+4], 0C3E00000H
		JNE		@bad0
		CMP		dword ptr [RSP], 0
		JE		@good0
@bad0 :	POP		RAX
		JMP		Error
@good0:	ADD		RSP, 8
	%END
@good :	RET
END TruncToInt64;

PROCEDURE TruncToInt32 (X : LONGREAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
		CVTTSD2SI RAX, XMM0
		CMP		RAX, -80000000H
		JL		Error
		CMP		RAX, 7FFFFFFFH
		JG		Error
		RET
END TruncToInt32;

PROCEDURE TruncToInt16 (X : LONGREAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
		CVTTSD2SI EAX, XMM0
		CMP		EAX, -8000H
		JL		Error
		CMP		EAX, 7FFFH
		JG		Error
		RET
END TruncToInt16;

PROCEDURE Trunc32ToInt64 (X : REAL) : INTEGER64 [Invariant, Alters(AX)]; PUREASM;
ASM
		CVTTSS2SI RAX, XMM0
		DEC		RAX
		INC		RAX
		JNO		@good
		(* Check on special case : source is exactly -2**63 *)
	%IF Windows %THEN
		MOVSS	dword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+8], 0DF000000H
		JNE		Error
	%ELSE
		PUSH	RAX
		MOVSS	dword ptr [RSP], XMM0
		CMP		dword ptr [RSP], 0DF000000H
		JE		@good0
		POP		RAX
		JMP		Error
@good0:	ADD		RSP, 8
	%END
@good :	RET
END Trunc32ToInt64;

PROCEDURE Trunc32ToInt32 (X : REAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
		CVTTSS2SI RAX, XMM0
		CMP		RAX, -80000000H
		JL		Error
		CMP		RAX, 7FFFFFFFH
		JG		Error
		RET
END Trunc32ToInt32;

PROCEDURE Trunc32ToInt16 (X : REAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
		CVTTSS2SI EAX, XMM0
		CMP		EAX, -8000H
		JL		Error
		CMP		EAX, 7FFFH
		JG		Error
		RET
END Trunc32ToInt16;

PROCEDURE FloorToInt64 (X : LONGREAL) : INTEGER64 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0BFH
		OR		byte ptr [RSP+9], 20H
		LDMXCSR	[RSP+8]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+12]
		DEC		RAX
		INC		RAX
		JNO		@good
		(* Check on special case : source is exactly -2**63 *)
		MOVSD	qword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+12], 0C3E00000H
		JNE		Error
		CMP		dword ptr [RSP+8], 0
		JNE		Error
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0BFH
		OR		byte ptr [RSP+1], 20H
		LDMXCSR	[RSP]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		DEC		RAX
		INC		RAX
		JNO		@good0
		(* Check on special case : source is exactly -2**63 *)
		MOVSD	qword ptr [RSP], XMM0
		CMP		dword ptr [RSP+4], 0C3E00000H
		JNE		@bad0
		CMP		dword ptr [RSP], 0
		JE		@good0
@bad0 :	POP		RAX
		JMP		Error
@good0:	ADD		RSP, 8
	%END
@good :	RET
END FloorToInt64;

PROCEDURE FloorToInt32 (X : LONGREAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0BFH
		OR		byte ptr [RSP+9], 20H
		LDMXCSR	[RSP+8]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0BFH
		OR		byte ptr [RSP+1], 20H
		LDMXCSR	[RSP]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		RAX, -80000000H
		JL		Error
		CMP		RAX, 7FFFFFFFH
		JG		Error
		RET
END FloorToInt32;

PROCEDURE FloorToInt16 (X : LONGREAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0BFH
		OR		byte ptr [RSP+9], 20H
		LDMXCSR	[RSP+8]
		CVTSD2SI EAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0BFH
		OR		byte ptr [RSP+1], 20H
		LDMXCSR	[RSP]
		CVTSD2SI EAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		EAX, -8000H
		JL		Error
		CMP		EAX, 7FFFH
		JG		Error
		RET
END FloorToInt16;

PROCEDURE Floor32ToInt64 (X : REAL) : INTEGER64 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0BFH
		OR		byte ptr [RSP+9], 20H
		LDMXCSR	[RSP+8]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+12]
		DEC		RAX
		INC		RAX
		JNO		@good
		(* Check on special case : source is exactly -2**63 *)
		MOVSS	dword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+8], 0DF000000H
		JNE		Error
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0BFH
		OR		byte ptr [RSP+1], 20H
		LDMXCSR	[RSP]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		DEC		RAX
		INC		RAX
		JNO		@good0
		(* Check on special case : source is exactly -2**63 *)
		MOVSS	dword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+8], 0DF000000H
		JE		@good0
		POP		RAX
		JMP		Error
@good0:	ADD		RSP, 8
	%END
@good :	RET
END Floor32ToInt64;

PROCEDURE Floor32ToInt32 (X : REAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0BFH
		OR		byte ptr [RSP+9], 20H
		LDMXCSR	[RSP+8]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0BFH
		OR		byte ptr [RSP+1], 20H
		LDMXCSR	[RSP]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		RAX, -80000000H
		JL		Error
		CMP		RAX, 7FFFFFFFH
		JG		Error
		RET
END Floor32ToInt32;

PROCEDURE Floor32ToInt16 (X : REAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0BFH
		OR		byte ptr [RSP+9], 20H
		LDMXCSR	[RSP+8]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0BFH
		OR		byte ptr [RSP+1], 20H
		LDMXCSR	[RSP]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		EAX, -8000H
		JL		Error
		CMP		EAX, 7FFFH
		JG		Error
		RET
END Floor32ToInt16;

PROCEDURE CeilingToInt64 (X : LONGREAL) : INTEGER64 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0DFH
		OR		byte ptr [RSP+9], 40H
		LDMXCSR	[RSP+8]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+12]
		DEC		RAX
		INC		RAX
		JNO		@good
		(* Check on special case : source is exactly -2**63 *)
		MOVSD	qword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+12], 0C3E00000H
		JNE		Error
		CMP		dword ptr [RSP+8], 0
		JNE		Error
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0DFH
		OR		byte ptr [RSP+1], 40H
		LDMXCSR	[RSP]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		DEC		RAX
		INC		RAX
		JNO		@good0
		(* Check on special case : source is exactly -2**63 *)
		MOVSD	qword ptr [RSP], XMM0
		CMP		dword ptr [RSP+4], 0C3E00000H
		JNE		@bad0
		CMP		dword ptr [RSP], 0
		JE		@good0
@bad0 :	POP		RAX
		JMP		Error
@good0:	ADD		RSP, 8
	%END
@good :	RET
END CeilingToInt64;

PROCEDURE CeilingToInt32 (X : LONGREAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0DFH
		OR		byte ptr [RSP+9], 40H
		LDMXCSR	[RSP+8]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0DFH
		OR		byte ptr [RSP+1], 40H
		LDMXCSR	[RSP]
		CVTSD2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		RAX, -80000000H
		JL		Error
		CMP		RAX, 7FFFFFFFH
		JG		Error
		RET
END CeilingToInt32;

PROCEDURE CeilingToInt16 (X : LONGREAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0DFH
		OR		byte ptr [RSP+9], 40H
		LDMXCSR	[RSP+8]
		CVTSD2SI EAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0DFH
		OR		byte ptr [RSP+1], 40H
		LDMXCSR	[RSP]
		CVTSD2SI EAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		EAX, -8000H
		JL		Error
		CMP		EAX, 7FFFH
		JG		Error
		RET
END CeilingToInt16;

PROCEDURE Ceiling32ToInt64 (X : REAL) : INTEGER64 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0DFH
		OR		byte ptr [RSP+9], 40H
		LDMXCSR	[RSP+8]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+12]
		DEC		RAX
		INC		RAX
		JNO		@good
		(* Check on special case : source is exactly -2**63 *)
		MOVSS	dword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+8], 0DF000000H
		JNE		Error
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0DFH
		OR		byte ptr [RSP+1], 40H
		LDMXCSR	[RSP]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		DEC		RAX
		INC		RAX
		JNO		@good0
		(* Check on special case : source is exactly -2**63 *)
		MOVSS	dword ptr [RSP+8], XMM0
		CMP		dword ptr [RSP+8], 0DF000000H
		JE		@good0
		POP		RAX
		JMP		Error
@good0:	ADD		RSP, 8
	%END
@good :	RET
END Ceiling32ToInt64;

PROCEDURE Ceiling32ToInt32 (X : REAL) : INTEGER32 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0DFH
		OR		byte ptr [RSP+9], 40H
		LDMXCSR	[RSP+8]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0DFH
		OR		byte ptr [RSP+1], 40H
		LDMXCSR	[RSP]
		CVTSS2SI RAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		RAX, -80000000H
		JL		Error
		CMP		RAX, 7FFFFFFFH
		JG		Error
		RET
END Ceiling32ToInt32;

PROCEDURE Ceiling32ToInt16 (X : REAL) : INTEGER16 [Invariant, Alters(AX)]; PUREASM;
ASM
	%IF Windows %THEN
		STMXCSR	[RSP+12]
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+9], 0DFH
		OR		byte ptr [RSP+9], 40H
		LDMXCSR	[RSP+8]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+12]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP+4]
		STMXCSR	[RSP]
		AND		byte ptr [RSP+1], 0DFH
		OR		byte ptr [RSP+1], 40H
		LDMXCSR	[RSP]
		CVTSS2SI EAX, XMM0
		LDMXCSR	[RSP+4]
		ADD		RSP, 8
	%END
		CMP		EAX, -8000H
		JL		Error
		CMP		EAX, 7FFFH
		JG		Error
		RET
END Ceiling32ToInt16;

PROCEDURE Error; PUREASM;
ASM
	%IF Windows %THEN
		MOV		CL, M2EXCEPTION.wholeValueException
		%IF PICCode %THEN
			MOV		RDX, got ConvErrorMessageVar
			ADD		RDX, gotoffs ConvErrorMessageVar
		%ELSE
			LEA		RDX, ConvErrorMessageVar
		%END
		MOV		R8D, ConvErrorMessageLen
	%ELSE
		MOV		DIL, M2EXCEPTION.wholeValueException
		MOV		RSI, got ConvErrorMessageVar
		ADD		RSI, gotoffs ConvErrorMessageVar
		MOV		EDX, ConvErrorMessageLen
	%END
	JMP		M2EXCEPTION.RaiseM2Exception
END Error;

%END

(* Exception handling means *)

CONST
	FPExceptionText = "Floating point exception";
	FPExceptionTextLen = LENGTH(FPExceptionText);

VAR
    FPM2Exception : EXCEPTIONS.ExceptionSource;
	FPExceptionTextVar : ARRAY [0..FPExceptionTextLen] OF CHAR = FPExceptionText;

PROCEDURE ClearExceptions87() [Alters()]; PUREASM;
ASM
	FNCLEX
	RET
END ClearExceptions87;

PROCEDURE ClearExceptionsSSE() [Alters()]; PUREASM;
ASM
	%IF IA32 %THEN
		PUSH	EAX
		STMXCSR	[ESP]
		AND		byte ptr [ESP], 0C0H
		LDMXCSR	[ESP]
		POP		EAX
	%ELSIF Windows %THEN
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+8], 0C0H
		LDMXCSR	[RSP+8]
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP]
		AND		byte ptr [RSP], 0C0H
		LDMXCSR	[RSP]
		POP		RAX
	%END
	RET
END ClearExceptionsSSE;

%IF IA32 %THEN

PROCEDURE ClearExceptionsBothInt() [Alters()]; PUREASM;
ASM
	FNCLEX
	PUSH	EAX
	STMXCSR	[ESP]
	AND		byte ptr [ESP], 0C0H
	LDMXCSR	[ESP]
	POP		EAX
	RET
END ClearExceptionsBothInt;

%ELSIF Windows %THEN

PROCEDURE ClearExceptionsBoth() [Alters()]; PUREASM;
ASM
	FNCLEX
	STMXCSR	[RSP+8]
	AND		byte ptr [RSP+8], 0C0H
	LDMXCSR	[RSP+8]
	RET
END ClearExceptionsBoth;

%ELSE

PROCEDURE ClearExceptionsBoth() [Alters()]; PUREASM;
ASM
	FNCLEX
	PUSH	RAX
	STMXCSR	[RSP]
	AND		byte ptr [RSP], 0C0H
	LDMXCSR	[RSP]
	POP		RAX
	RET
END ClearExceptionsBoth;

%END

PROCEDURE GetExceptions87 () : FPExceptions [Alters(AX)]; PUREASM;
ASM
	FNSTSW	AX
	AND		AL, 03FH
	RET
END GetExceptions87;

PROCEDURE GetExceptionsSSE () : FPExceptions [Alters(AX)]; PUREASM;
ASM
	%IF IA32 %THEN
		PUSH	EAX
		STMXCSR	[ESP]
		POP		EAX
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP]
		POP		RAX
	%END
	AND		AL, 03FH
	RET
END GetExceptionsSSE;

%IF IA32 %THEN

PROCEDURE GetExceptionsBothInt () : FPExceptions [Alters(AX)]; PUREASM;
ASM
	FNSTSW	AX
	PUSH	EAX
	STMXCSR	[ESP]
	OR		AL, [ESP]
	ADD		ESP, 4
	AND		AL, 03FH
	RET
END GetExceptionsBothInt;

%ELSE

PROCEDURE GetExceptionsBoth () : FPExceptions [Alters(AX)]; PUREASM;
ASM
	FNSTSW	AX
	PUSH	RAX
	STMXCSR	[RSP]
	OR		AL, [RSP]
	ADD		RSP, 8
	AND		AL, 03FH
	RET
END GetExceptionsBoth;

%END

PROCEDURE CheckException87 (Exceptions : FPExceptions) [Pass(BX),Alters(AX)]; PUREASM;
ASM
	FNSTSW	AX
	AND		AL, BL
	JNZ		@raise
	RET
@raise :
	%IF IA32 %THEN
		MOVZX	EAX, AL
		PUSH	EAX					(* Exception code *)
		PUSH	dword ptr FPExceptionTextLen
		PUSH	offset FPExceptionTextVar
		PUSH	dword ptr [ESP+12]	(* Move the return address to stack top *)
		MOV		EAX, FPM2Exception
		MOV		[ESP+16], EAX
	%ELSE
		%IF Windows %THEN
			%IF PICCode %THEN
				MOV     RCX, got FPM2Exception
				MOV		RCX, gotoffs FPM2Exception [RCX]
			%ELSE
				MOV		RCX, FPM2Exception
			%END
			MOVZX 	EDX, AL				(* Exception code *)
			%IF PICCode %THEN
				MOV     R8, got FPExceptionTextVar
				ADD     R8, gotoffs FPExceptionTextVar
			%ELSE
				MOV		R8, offset FPExceptionTextVar
			%END
			MOV		R9D, FPExceptionTextLen
		%ELSE
			MOV     RDI, got FPM2Exception
			MOV		RDI, gotoffs FPM2Exception [RDI]
			MOVZX 	ESI, AL				(* Exception code *)
			MOV     RDX, got FPExceptionTextVar
			ADD     RDX, gotoffs FPExceptionTextVar
			MOV		ECX, FPExceptionTextLen
		%END
	%END
	JMP		EXCEPTIONS.RaiseRTL
END CheckException87;

PROCEDURE CheckExceptionSSE (Exceptions : FPExceptions) [Pass(BX),Alters()]; PUREASM;
ASM
	%IF IA32 %THEN
		PUSH	EAX
		STMXCSR	[ESP]
		AND		byte ptr [ESP], BL
		JNZ		@raise
		POP		EAX
	%ELSIF Windows %THEN
		STMXCSR	[RSP+8]
		AND		byte ptr [RSP+8], BL
		JNZ		@raise
	%ELSE
		PUSH	RAX
		STMXCSR	[RSP]
		AND		byte ptr [RSP], BL
		JNZ		@raise
		POP		RAX
	%END
	RET
@raise :
	%IF IA32 %THEN
		MOVZX	EAX, byte ptr [ESP]
		MOV		[ESP], EAX			(* Exception code *)
		PUSH	dword ptr FPExceptionTextLen
		PUSH	offset FPExceptionTextVar
		PUSH	dword ptr [ESP+12]	(* Move the return address to stack top *)
		MOV		EAX, FPM2Exception
		MOV		[ESP+16], EAX
	%ELSE
		%IF Windows %THEN
			%IF PICCode %THEN
				MOV     RCX, got FPM2Exception
				MOV		RCX, gotoffs FPM2Exception [RCX]
			%ELSE
				MOV		RCX, FPM2Exception
			%END
			MOVZX	EDX, byte ptr [RSP+8](* Exception code *)
			%IF PICCode %THEN
				MOV     R8, got FPExceptionTextVar
				ADD		R8, gotoffs FPExceptionTextVar
			%ELSE
				MOV		R8, offset FPExceptionTextVar
			%END
			MOV		R9D, FPExceptionTextLen
		%ELSE
			MOV     RDI, got FPM2Exception
			MOV		RDI, gotoffs FPM2Exception [RDI]
			POP		RSI					(* Exception code *)
			AND		RSI, 0FFH
			MOV     RDX, got FPExceptionTextVar
			ADD		RDX, gotoffs FPExceptionTextVar
			MOV		ECX, FPExceptionTextLen
		%END
	%END
	JMP		EXCEPTIONS.RaiseRTL
END CheckExceptionSSE;

%IF IA32 %THEN

PROCEDURE CheckExceptionBothInt (Exceptions : FPExceptions) [Pass(BX),Alters(AX)]; PUREASM;
ASM
	FNSTSW	AX
	PUSH	EAX (* Move exception code to the stack *)
	STMXCSR	[ESP]
	OR		AL, [ESP]
	AND		AL, BL
	JNZ		@raise
	POP		EAX
	RET
@raise :
	MOVZX	EAX, AL
	MOV		[ESP], EAX			(* Exception code *)
	PUSH	dword ptr FPExceptionTextLen
	PUSH	offset FPExceptionTextVar
	PUSH	dword ptr [ESP+12]	(* Move the return address to stack top *)
	MOV		EAX, FPM2Exception
	MOV		[ESP+16], EAX
	JMP		EXCEPTIONS.RaiseRTL
END CheckExceptionBothInt;

%ELSE

PROCEDURE CheckExceptionBoth (Exceptions : FPExceptions) [Pass(BX),Alters(AX)]; PUREASM;
ASM
	FNSTSW	AX
	STMXCSR	[RSP+8]
	OR		AL, [RSP+8]
	AND		AL, BL
	JNZ		@raise
	RET
@raise :
	%IF Windows %THEN
		%IF PICCode %THEN
			MOV     RCX, got FPM2Exception
			MOV		RCX, gotoffs FPM2Exception [RCX]
		%ELSE
			MOV		RCX, FPM2Exception
		%END
		MOVZX	EDX, AL				(* Exception code *)
		%IF PICCode %THEN
			MOV     R8, got FPExceptionTextVar
			ADD		R8, gotoffs FPExceptionTextVar
		%ELSE
			MOV		R8, offset FPExceptionTextVar
		%END
		MOV		R9D, FPExceptionTextLen
	%ELSE
		MOV     RDI, got FPM2Exception
		MOV		RDI, gotoffs FPM2Exception [RDI]
		MOVZX	ESI, AL				(* Exception code *)
		MOV     RDX, got FPExceptionTextVar
		ADD		RDX, gotoffs FPExceptionTextVar
		MOV		ECX, FPExceptionTextLen
	%END
	JMP		EXCEPTIONS.RaiseRTL
END CheckExceptionBoth;

%END

PROCEDURE RaisedFPExceptions () : FPExceptions;
BEGIN
	IF EXCEPTIONS.IsCurrentSource(FPM2Exception) THEN
		RETURN CAST(FPExceptions,EXCEPTIONS.CurrentNumber(FPM2Exception));
	ELSE
		RETURN NoFPException;
	END;
END RaisedFPExceptions;

%IF IA32 %THEN

PROCEDURE SSEPresent() : BOOLEAN [Alters(AX,BX,CX,DX),Returns(DX)]; PUREASM;
ASM
	MOV		EAX, 1
	CPUID
	SHR		EDX, 25			(* shift SSE presence bit to 0th bit *)
	AND		DL, 1
	RET
END SSEPresent;

%ELSE

PROCEDURE SSE4p1Present() : BOOLEAN [Alters(AX,BX,CX,DX),Returns(CX)]; PUREASM;
ASM
	MOV		EAX, 1
	CPUID
	SHR		ECX, 19			(* shift SSE4.1 presence bit to 0th bit *)
	AND		CL, 1
	RET
END SSE4p1Present;

%END


BEGIN
	%IF IA32 %THEN
		IF SSEPresent() THEN
			SetRoundingBoth     := SetRoundingBothInt    ;
			InitBoth		    := InitBothInt           ;
			ClearExceptionsBoth := ClearExceptionsBothInt;
			GetExceptionsBoth   := GetExceptionsBothInt  ;
			CheckExceptionBoth  := CheckExceptionBothInt ;
		END;
	%ELSE
		IF SSE4p1Present() THEN
			Round	  := SSE4Round	  ;
			Nearest	  := SSE4Nearest  ;
			Trunc	  := SSE4Trunc	  ;
			Floor	  := SSE4Floor	  ;
			Ceiling	  := SSE4Ceiling  ;
			Round32	  := SSE4Round32  ;
			Nearest32 := SSE4Nearest32;
			Trunc32	  := SSE4Trunc32  ;
			Floor32	  := SSE4Floor32  ;
			Ceiling32 := SSE4Ceiling32;
		ELSE
			Round	  := SSE2Round	  ;
			Nearest	  := SSE2Nearest  ;
			Trunc	  := SSE2Trunc	  ;
			Floor	  := SSE2Floor	  ;
			Ceiling	  := SSE2Ceiling  ;
			Round32	  := SSE2Round32  ;
			Nearest32 := SSE2Nearest32;
			Trunc32	  := SSE2Trunc32  ;
			Floor32	  := SSE2Floor32  ;
			Ceiling32 := SSE2Ceiling32;
		END;
	%END
    EXCEPTIONS.AllocateSource (FPM2Exception);
END Float.
