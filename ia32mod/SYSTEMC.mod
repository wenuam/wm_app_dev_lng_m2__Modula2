(***************************************************************************)
(*                                                                         *)
(*                       Copyright (C) 2009                                *)
(*                         by ADW Software                                 *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE SYSTEMC;
<*/NOOPT:X*>

FROM SYSTEM IMPORT
    %IF AMD64 %THEN
    ADRCARD, CAST, VA_LIST, ADDADR,
    %END
    ADDRESS;

FROM SYSTEMEX IMPORT
    RaiseM2Exception, M2Exceptions;

%IF Windows %OR UNIX %THEN
PROCEDURE STACKGROWTH(pages : CARDINAL) [ALTERS()]; PUREASM;
(* because of Linux it seems we must have ESP subtracted before we access a page
   otherwise the stack growth fails on large stack allocations.
   if seems the Linux kernel determines that an ESP access below ESP in the guard page
   if assumed to be a "real" access violation and not a reason to grow the stack.
*)
ASM
%IF Bits32 %THEN
        PUSH    EAX
        PUSH    ECX
        MOV     EAX, ESP

        MOV     ECX, [ESP+12]
    @LoopTop:
        SUB     ESP, 4*1024
        TEST    byte ptr [ESP], 255
        LOOP	@LoopTop

        MOV     ESP, EAX
        POP     ECX
        POP     EAX
        RET     4
%ELSIF Bits64 %THEN
(* Note : This passes the parameter pages on the stack.  Non-standard *)
        PUSH    RAX
        PUSH    RCX
        MOV     RAX, RSP

        MOV     ECX, [RSP + 3*8(*AddressSize*)]
    @LoopTop:
        SUB     RSP, 4*1024
        TEST    byte ptr [RSP], 255
        LOOP    @LoopTop

        MOV     RSP, RAX
        POP     RCX
        POP     RAX
        RET     8
%ELSE
    fix me
%END
END STACKGROWTH;
%END

PROCEDURE MOVE(source, dest : ADDRESS; count : CARDINAL) [PASS(SI, DI, AX), ALTERS(AX, CX, SI, DI)]; PUREASM;
ASM
%IF Bits32 %THEN
        CMP     EDI, ESI
        JBE     @MoveForward
        MOV     EAX, ECX
        ADD     EAX, ESI
        CMP     EAX, EDI                (* check for overlap *)
        JBE     @MoveForward

@MoveBackward:
        STD
        ADD     ESI, ECX                (*adjust source pointer to end*)
        ADD     EDI, ECX

        SUB     EDI, 4
        SUB     ESI, 4

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

%ELSIF Bits64 %THEN
        (* Count passed in RCX *)
        (* dest passed in RDI *)
        (* source passed in RSI *)

        CMP     RDI, RSI
        JBE     @MoveForward

        MOV     RAX, RCX
        ADD     RAX, RSI
        CMP     RAX, RDI                (* check for overlap *)
        JBE     @MoveForward

@MoveBackward:
        STD
        ADD     RSI, RCX                (*adjust source pointer to end*)
        ADD     RDI, RCX

        SUB     RDI, 8
        SUB     RSI, 8

        MOV     RAX, RCX
        SHR     RCX, 3
        REP MOVSQ
        MOV     RCX, RAX
        AND     RCX, 7
        ADD     RDI, 7
        ADD     RSI, 7
        REP MOVSB
		CLD
        RET

@MoveForward:
        CLD
        REP MOVSB
		RET

%ELSE
    fix me
%END
END MOVE;

PROCEDURE MOVEBYTES(SOURCE : ADDRESS; SOURCE_LEN : CARDINAL; DEST : ADDRESS; DEST_LEN : CARDINAL)
                    [PASS(SI,CX,DI,AX), ALTERS(AX,CX,SI,DI %IF AMD64 %THEN  ,DX %END)]; PUREASM;
ASM
%IF Bits32 %THEN
        CMP     ECX, EAX                (*check dest length*)
        JB      @SOURCE_SHORTER         (*skip if source is shorter*)
        MOV     ECX, EAX                (*use the destination length*)
        XOR     EAX, EAX                (*and there is no fill*)
        JMP     @DO_MOVE                (*go an do it*)

@SOURCE_SHORTER:
        SUB     EAX, ECX                (*get bytes to fill*)

@DO_MOVE:
        CMP     EDI, ESI                (*check for dest addr > source*)
        JBE     @MOVE_FORWARD           (*skip if not*)
        ADD     ESI, ECX
        CMP     ESI, EDI                (* check for overlap *)
        JA      @MOVE_BACKWARD
        SUB     ESI, ECX

@MOVE_FORWARD:
        CLD
        REP MOVSB
		MOV		ECX, EAX
		XOR		EAX, EAX
		REP STOSB
        RET

@MOVE_BACKWARD: (* move bytes backwards because of overlap.*)
		STD
		ADD     EDI, ECX                (*adjust destination pointer to end*)
		ADD     EDI, EAX
		DEC		EDI
		DEC		ESI
		PUSH	ECX
		MOV		ECX, EAX
		XOR		EAX, EAX
		REP STOSB
		POP		ECX
		REP MOVSB
		CLD
        RET

%ELSIF Bits64 %THEN
        (*
        SOURCE is passed in RSI
        DEST is passed IN RDI
        SOURCE_LEN is passed in RCX
        DEST_LEN is passed in RAX
        *)
        CMP     RCX, RAX                (*check dest length*)
        JB      @SOURCE_SHORTER         (*skip if source is shorter*)
        MOV     RCX, RAX                (*use the destination length*)
        XOR     RAX, RAX                (*and there is no fill*)
        JMP     @DO_MOVE                (*go an do it*)

@SOURCE_SHORTER:
        SUB     RAX, RCX                (*get bytes to fill*)

@DO_MOVE:
        CMP     RDI, RSI                (*check for dest addr > source*)
        JBE     @MOVE_FORWARD           (*skip if not*)
        ADD     RSI, RCX
        CMP     RSI, RDI                (* check for overlap *)
        JA      @MOVE_BACKWARD
        SUB     RSI, RCX

@MOVE_FORWARD:
        CLD
        REP MOVSB
		MOV		RCX, RAX
		XOR		RAX, RAX
		REP STOSB
        RET

@MOVE_BACKWARD: (* move bytes backwards because of overlap.*)

		STD
		ADD     RDI, RCX                (*adjust destination pointer to end*)
		ADD     RDI, RAX
		DEC		RDI
		DEC		RSI
		MOV		RDX, RCX				(* save RCX *)
		MOV		RCX, RAX
		XOR		AL, AL
		REP STOSB
		MOV		RCX, RDX				(* restore RCX *)
        REP MOVSB
		CLD
        RET

%ELSE
    fix me
%END
END MOVEBYTES;

PROCEDURE COPYPAR3(source, dest : ADDRESS; amount : CARDINAL) [LeftToRight]; PUREASM;
ASM
%IF Bits32 %THEN
(*
 AX is the SS relative offset of the copy
 DX is the SS relative offset of the source address
 CX is the size
*)

        PUSH    ESI
        PUSH    EDI

        MOV     ESI, [EDX]      (* get the source pointer*)
        MOV     [EDX], EAX      (* place new address in param location*)

        MOV     EDI, EAX
        CLD
        REP     MOVSB

        POP     EDI
        POP     ESI

        RET
%ELSIF Bits64 %THEN
(*
 AX is the SS relative offset of the copy
 DX is the SS relative offset of the source address
 CX is the size
*)

        PUSH    RSI
        PUSH    RDI

        MOV     RSI, [RDX]      (* get the source pointer*)
        MOV     [RDX], RAX      (* place new address in param location*)

        MOV     RDI, RAX
        CLD
        REP     MOVSB

        POP     RDI
        POP     RSI

        RET
%ELSE
    fix me
%END
END COPYPAR3;

PROCEDURE COPYOPENPARAM; PUREASM;
(*
  CX = bytes to copy
  DX = BP offset of pointer
  IF Windows AND Bits64 THEN AX = StackDepth (size of paramaters save area)
*)
ASM
%IF Bits32 %THEN
    %IF Windows %OR UNIX %THEN
        CMP     ECX, 4096
        JB      @Ok
        MOV     EAX, ECX
        SHR     EAX, 12
        PUSH    EAX
        CALL    STACKGROWTH
    @Ok:
    %END

(* get the return address*)

        POP     EAX

(* allocate the space on the stack*)
(* round the address down to a modulus of 4 *)

        SUB     ESP, ECX
        AND     SP, 0FFFCH

(* get a pointer to the address in the parameter list*)

        ADD     EDX, EBP

(* preserve si and di*)

        PUSH    ESI
        PUSH    EDI

(* get address of the current stack top in DI*)

        MOV     EDI, ESP

(*adjust DI because of our register saves*)
        ADD     EDI, 8

(* load the address of the parameter*)

        MOV     ESI, [EDX]

(* move the data *)

        CLD
        REP     MOVSB

(* restore SI and DI*)
        POP     EDI
        POP     ESI

(* replace address in the parameter list*)

        MOV     [EDX], ESP

(* re-push our return address*)

        PUSH    EAX
        RET
%ELSIF Bits64 %THEN

    %IF Windows %OR UNIX %THEN

        CMP     RCX, 4096
        JB      @Ok
		PUSH	RAX
        MOV     RAX, RCX
        SHR     RAX, 12
        PUSH    RAX
        CALL    STACKGROWTH
		POP		RAX
    @Ok:

(* get a pointer to the address in the parameter list*)

        ADD     RDX, RBP

(* save stack depth *)

        PUSH	RAX

(* calculate size to add to the stack (round up to a modulo 16) *)

		LEA		RAX, [RCX+15]
		AND		AL, 0F0H

(* allocate the space on the stack*)

        SUB     RSP, RAX

(* preserve si and di *)

        MOV		[RSP], RSI
        PUSH    RDI

(* copy return address to other location *)

		MOV		RDI, [RSP+RAX+16]
		MOV		[RSP+16], RDI

(* get stack depth into RAX *)

		MOV		RAX, [RSP+RAX+8]

(* get address of destination in DI *)

        LEA     RDI, [RSP+RAX+24]

(* load the address of the parameter*)

        MOV     RSI, [RDX]

(* replace address in the parameter list*)

        MOV     [RDX], RDI

(* move the data *)

        CLD
        REP     MOVSB

(* restore SI and DI and return *)

        POP     RDI
        POP     RSI
		RET

	%ELSE (* WINDOWS *)

(* get the return address*)

        POP     RAX

(* allocate the space on the stack*)
(* round the address down to a modulus of 4 *)

        SUB     RSP, RCX
        AND     SP, 0FFF0H

(* get a pointer to the address in the parameter list*)

        ADD     RDX, RBP

(* preserve si and di*)

        PUSH    RSI
        PUSH    RDI

(* get address of the current stack top in DI*)

        MOV     RDI, RSP

(*adjust DI because of our register saves*)
        ADD     RDI, 16

(* load the address of the parameter*)

        MOV     RSI, [RDX]

(* move the data *)

        CLD
        REP     MOVSB

(* restore SI and DI*)
        POP     RDI
        POP     RSI

(* replace address in the parameter list*)

        MOV     [RDX], RSP

(* re-push our return address*)

        PUSH    RAX
        RET

	%END

%ELSE
    fix me
%END
END COPYOPENPARAM;

PROCEDURE LMUL(RIGHT, LEFT : LONGINT) : LONGINT
                            %IF IA32 %THEN
                            [ALTERS(AX, DX)];
                            %ELSE
                            [LeftToRight];
                            %END
                            PUREASM;
%IF Bits32 %THEN
(*
   ESP          = return addr
   ESP+4        = Left Low
   ESP+8        = Left high
   ESP+12       = Right Low
   ESP+16       = Right high
*)
ASM
        MOV     EAX, [ESP+8]
        OR      EAX, [ESP+16]
        JNZ     @FullMul
        MOV     EAX, [ESP+4]
        MUL     DWORD PTR [ESP+12]
        RET     16
    @FullMul:
(*
   ESP          = saved ECX
   ESP+4        = return addr
   ESP+8        = Left Low
   ESP+12       = Left high
   ESP+16       = Right Low
   ESP+20       = Right high
*)
        PUSH    ECX

        MOV     EAX, [ESP+12]           (* multiply left high by right low *)
        MUL     DWORD PTR [ESP+16]
        MOV     ECX, EAX

        MOV     EAX, [ESP+20]           (* multiply right high by left low *)
        MUL     DWORD PTR [ESP+8]
        ADD     ECX, EAX

        MOV     EAX, DWORD PTR [ESP+8]
        MUL     DWORD PTR [ESP+16]      (* multiply low words *)
        ADD     EDX, ECX

        POP     ECX
        RET     16
%ELSIF Bits64 %THEN
(*
   RDI          = Left Low
   RSI          = Left high
   RDX          = Right Low
   RCX          = Right high
*)
ASM
        MOV     RAX, RSI
        OR      RAX, RCX
        JNZ     @FullMul
        MOV     RAX, RDI
        MUL     RDX
        RET     32
    @FullMul:
(*
   RDI          = Left Low
   RSI          = Left high
   R10          = Right Low
   RCX          = Right high
*)
        MOV     R10, RDX

        MOV     RAX, RSI                (* multiply left high by right low *)
        MUL     R10
        MOV     R9, RAX

        MOV     RAX, RCX                (* multiply right high by left low *)
        MUL     RDI
        ADD     R9, RAX

        MOV     RAX, RDI
        MUL     R10                     (* multiply low words *)
        ADD     RDX, R9

        RET
%ELSE
    fix me
%END
END LMUL;
(*
PROCEDURE LMUL(RIGHT, LEFT : LONGINT) : LONGINT [ALTERS(AX, DX)]; PUREASM;
ASM
        PUSH    EBP                     (* set up stack frame*)
        MOV     EBP, ESP
        PUSH    EBX
        PUSH    ECX

        MOV     EAX, DWORD PTR LEFT     (* multiply two low words*)
        MUL     DWORD PTR RIGHT
        MOV     EBX, EAX
        MOV     ECX, EDX

        MOV     EAX, DWORD PTR LEFT+4   (* multiply high by low*)
        MUL     DWORD PTR RIGHT
        ADD     ECX, EAX

        MOV     EAX, DWORD PTR RIGHT+4
        MUL     DWORD PTR LEFT          (* multiply low by high*)
        ADD     ECX, EAX

        MOV     EAX, EBX                (* get the full result*)
        MOV     EDX, ECX

        POP     ECX
        POP     EBX
        POP     EBP
        RET     16
END LMUL;
*)

PROCEDURE Overflow;
BEGIN
    RaiseM2Exception(wholeValueException, "WHOLE-OVERFLOW");
END Overflow;

%IF Bits32 %THEN
PROCEDURE LMULCHECK(RIGHT, LEFT : LONGINT) : LONGINT [ALTERS(AX, DX)]; PUREASM;
VAR
    neg : BOOLEAN;
ASM
        PUSH    EBP                     (* set up stack frame*)
        MOV     EBP, ESP
        PUSH    EAX                     (* space for neg *)
        PUSH    EBX
        PUSH    ECX
        PUSH    EDI
        PUSH    ESI


        MOV     DL, 0

        MOV     ESI, DWORD PTR LEFT
        MOV     EDI, DWORD PTR LEFT+4
        CMP     EDI, 0
        JGE     @LEFTNOTNEG
        MOV     DL, 1
        NEG     ESI
        ADC     EDI, 0
        NEG     EDI
@LEFTNOTNEG:

        MOV     EAX, DWORD PTR RIGHT
        CMP     DWORD PTR RIGHT+4, 0
        JGE     @RIGHTNOTNEG
        MOV     ECX, DWORD PTR RIGHT+4
        NEG     EAX
        ADC     ECX, 0
        XOR     DL, 1
        NEG     ECX
        MOV     DWORD PTR RIGHT+4, ECX
@RIGHTNOTNEG:
        MOV     neg, DL
        MOV     ECX, EAX                (* save right *)


        MUL     ESI                     (* multiply two low words*)
        MOV     EBX, EAX
        MOV     EAX, ECX                (* get right ready to MUL *)
        MOV     ECX, EDX

        OR      EDI, EDI                (* multiply high by low*)
        JZ      @LMOP32BIT
        MUL     EDI
        ADD     ECX, EAX
        ADC     EDX, 0
        JNZ     @RunError

@LMOP32BIT:
        MOV     EAX, DWORD PTR RIGHT+4
        OR      EAX, EAX
        JZ      @RMOP32BIT
        MUL     ESI                     (* multiply low by high*)
        ADD     ECX, EAX
        ADC     EDX, 0
        JNZ     @RunError

@RMOP32BIT:
        CMP     ECX, 0
        JL      @RunError

        CMP     EDI, 0
        JE      @RMOP32BIT2
        CMP     DWORD PTR RIGHT+4, 0
        JNE     @RunError

@RMOP32BIT2:
        CMP     neg, FALSE
        JZ      @UseResult
        NEG     EBX
        ADC     ECX, 0
        NEG     ECX

@UseResult:

        POP     ESI
        POP     EDI

        MOV     EAX, EBX                (* get the full result*)
        MOV     EDX, ECX

        POP     ECX
        POP     EBX

        POP     EBP                     (* get neg off stack *)
        POP     EBP
        RET     16
@RunError:
        CALL    Overflow
END LMULCHECK;
%ELSIF Bits64 %THEN
PROCEDURE LMULCHECK(R, L : DoubleInt) : DoubleInt [LeftToRight]; PUREASM;
(*
     Left = {SI:DI}
     Right = {CX:DX} ====> {R9:R8}
     neg : BOOLEAN;  (* R11L *)
*)
CONST
    LEFT        = 16;
    RIGHT       = 32;
ASM
        MOV     R9, RCX
        MOV     R8, RDX
        MOV     R11L, 0             (* neg := FALSE *)

        CMP     RSI, 0
        JGE     @LEFTNOTNEG
        MOV     R11L, 1
        NEG     RDI
        ADC     RSI, 0
        NEG     RSI
@LEFTNOTNEG:

        MOV     RAX, R8
        CMP     R9, 0
        JGE     @RIGHTNOTNEG
        MOV     RCX, R9
        NEG     RAX
        ADC     RCX, 0
        XOR     R11L, 1
        NEG     RCX
        MOV     R9, RCX
@RIGHTNOTNEG:
        MOV     RCX, RAX                (* save right *)


        MUL     RDI                     (* multiply two low words*)
        MOV     R10, RAX
        MOV     RAX, RCX                (* get right ready to MUL *)
        MOV     RCX, RDX

        OR      RSI, RSI                (* multiply high by low*)
        JZ      @LMOP64BIT
        MUL     RSI
        ADD     RCX, RAX
        ADC     RDX, 0
        JNZ     @RunError

@LMOP64BIT:
        MOV     RAX, R9
        OR      RAX, RAX
        JZ      @RMOP64BIT
        MUL     RDI                     (* multiply low by high*)
        ADD     RCX, RAX
        ADC     RDX, 0
        JNZ     @RunError

@RMOP64BIT:
        CMP     RCX, 0
        JL      @RunError

        CMP     RSI, 0
        JE      @RMOP64BIT2
        CMP     QWORD PTR R9, 0
        JNE     @RunError

@RMOP64BIT2:
        CMP     R11L, FALSE
        JZ      @UseResult
        NEG     R10
        ADC     RCX, 0
        NEG     RCX

@UseResult:

        MOV     RAX, R10                (* get the full result*)
        MOV     RDX, RCX

        RET
@RunError:
        CALL    Overflow
END LMULCHECK;
%ELSE
    fix me
%END

%IF Bits32 %THEN
PROCEDURE DIVIDE(RIGHT, LEFT : LONGINT); PUREASM;
ASM
        PUSH    EDI
        MOV     ECX, EAX
        XOR     EDX, EDX
        XOR     EAX, EAX
        MOV     EDI, 32                 (* get loop count*)
@DIVLOOP:
        SHL     EAX, 1                  (* shift the dividend left one bit*)
        RCL     EBX, 1
        RCL     ECX, 1
        RCL     EDX, 1
        SUB     ECX, DWORD PTR RIGHT    (* subtract the divisor*)
        SBB     EDX, DWORD PTR RIGHT+4
        JAE     @GENBIT                 (* if positive, output a result bit*)
        ADD     ECX, DWORD PTR RIGHT    (* no, add it back*)
        ADC     EDX, DWORD PTR RIGHT+4
        JMP     @NOBIT
@GENBIT:
        INC     EAX                     (* set a bit in the result*)
@NOBIT:
        DEC     EDI                     (* loop 32 times*)
        JNE     @DIVLOOP
        CMP     ESI, 1                  (* check for negative result*)
        JNE     @NONEGRESULT
        NEG     EAX
        ADC     EBX, 0
        NEG     EBX
        NEG     ECX
        ADC     EDX, 0
        NEG     EDX
@NONEGRESULT:
        POP     EDI                     (* restore regs*)
        RET
END DIVIDE;
(*
%ELSIF Bits64 %THEN
PROCEDURE DIVIDE(R, L : LONGINT); PUREASM;
(*
     Left = {RAX:RBX}
     Right = {R9:R8}
*)
ASM
        MOV     RCX, RAX
        XOR     RDX, RDX
        XOR     RAX, RAX
        MOV     RDI, 64                 (* get loop count*)
@DIVLOOP:
        SHL     RAX, 1                  (* shift the dividend left one bit*)
        RCL     RBX, 1
        RCL     RCX, 1
        RCL     RDX, 1
        SUB     RCX, R8                 (* subtract the divisor*)
        SBB     RDX, R9
        JAE     @GENBIT                 (* if positive, output a result bit*)
        ADD     RCX, R8                 (* no, add it back*)
        ADC     RDX, R9
        JMP     @NOBIT
@GENBIT:
        INC     RAX                     (* set a bit in the result*)
@NOBIT:
        DEC     RDI                     (* loop 64 times*)
        JNE     @DIVLOOP
        CMP     RSI, 1                  (* check for negative result*)
        JNE     @NONEGRESULT
        NEG     RAX
        ADC     RBX, 0
        NEG     RBX
        NEG     RCX
        ADC     RDX, 0
        NEG     RDX
@NONEGRESULT:
        RET
END DIVIDE;
%ELSE
    fix me
*)
%END

%IF Bits32 %THEN
PROCEDURE LDIVIS(RIGHT, LEFT : LONGINT) : LONGINT [ALTERS(AX, DX)]; PUREASM;
ASM
        PUSH    EBP
        MOV     EBP, ESP

        PUSH    EBX
        PUSH    ESI
        PUSH    ECX

        XOR     ESI, ESI

        MOV     EAX, DWORD PTR LEFT+4         (* get high word of dividend*)
        MOV     EBX, DWORD PTR LEFT           (* get low word of dividend*)
        CMP     EAX, 0
        JGE     @DIVPOSDIVIDEND
        NEG     EBX
        ADC     EAX, 0
        NEG     EAX
        INC     ESI
@DIVPOSDIVIDEND:


        MOV     EDX, DWORD PTR RIGHT+4        (* get the high word of divisor*)
        MOV     ECX, DWORD PTR RIGHT
        OR      EDX, EDX
        JZ      @POS32DIVISOR
        JNS     @DO64BITDIV
        INC     ESI
        NEG     ECX
        ADC     EDX, 0
        NEG     EDX
        JZ      @POS32DIVISOR
        MOV     DWORD PTR RIGHT+4, EDX
        MOV     DWORD PTR RIGHT, ECX
        JMP     @DO64BITDIV

@POS32DIVISOR:
        CMP     EAX, ECX
        JAE     @Do2Divs
        MOV     EDX, EAX
        MOV     EAX, EBX
        DIV     ECX
        XOR     EDX, EDX
        CMP     ESI, 1
        JNE     @LONGDIVRET
        NEG     EAX
        ADC     EDX, 0
        NEG     EDX
        JMP     @LONGDIVRET

    @Do2Divs:
        XOR     EDX, EDX
        DIV     ECX
        XCHG    EAX, EBX
        DIV     ECX
        MOV     EDX, EBX
        CMP     ESI, 1
        JNE     @LONGDIVRET
        NEG     EAX
        ADC     EDX, 0
        NEG     EDX
        JMP     @LONGDIVRET

@DO64BITDIV:
        CALL    DIVIDE
        MOV     EDX, EBX

@LONGDIVRET:
        POP     ECX
        POP     ESI
        POP     EBX
        POP     EBP
        RET     16
END LDIVIS;
(*
%ELSIF Bits64 %THEN
PROCEDURE LDIVIS(R, L : LONGINT) : LONGINT [LeftToRight]; PUREASM;
ASM
        PUSH    RBX

        MOV     RAX, RSI                      (* get high word of dividend*)
        MOV     RBX, RDI                      (* get low word of dividend*)
        MOV     R9, RCX                       (* get high word of divisor *)
        MOV     R8, RDX                       (* get low word of divisor *)

        XOR     RSI, RSI

        CMP     RAX, 0
        JGE     @DIVPOSDIVIDEND
        NEG     RBX
        ADC     RAX, 0
        NEG     RAX
        INC     RSI
@DIVPOSDIVIDEND:


        MOV     RDX, R9                       (* get the high word of divisor*)
        MOV     RCX, R8
        OR      RDX, RDX
        JZ      @POS64DIVISOR
        JNS     @DO128BITDIV
        INC     RSI
        NEG     RCX
        ADC     RDX, 0
        NEG     RDX
        JZ      @POS64DIVISOR
        MOV     R9, RDX
        MOV     R8, RCX
        JMP     @DO128BITDIV

@POS64DIVISOR:
        CMP     RAX, RCX
        JAE     @Do2Divs
        MOV     RDX, RAX
        MOV     RAX, RBX
        DIV     RCX
        XOR     RDX, RDX
        CMP     RSI, 1
        JNE     @LONGDIVRET
        NEG     RAX
        ADC     RDX, 0
        NEG     RDX
        JMP     @LONGDIVRET

    @Do2Divs:
        XOR     RDX, RDX
        DIV     RCX
        XCHG    RAX, RBX
        DIV     RCX
        MOV     RDX, RBX
        CMP     RSI, 1
        JNE     @LONGDIVRET
        NEG     RAX
        ADC     RDX, 0
        NEG     RDX
        JMP     @LONGDIVRET

@DO128BITDIV:
        CALL    DIVIDE
        MOV     RDX, RBX

@LONGDIVRET:
        POP     RBX
        RET
END LDIVIS;
%ELSE
    fix me
*)
%END

%IF Bits32 %THEN
PROCEDURE LREMS(RIGHT, LEFT : LONGINT) : LONGINT [ALTERS(AX, DX)]; PUREASM;
ASM
        PUSH    EBP
        MOV     EBP, ESP

        PUSH    EBX
        PUSH    ESI
        PUSH    ECX

        XOR     ESI, ESI
        MOV     EAX, DWORD PTR LEFT+4     (* get high word of dividend*)
        MOV     EBX, DWORD PTR LEFT       (* get low word of dividend*)
        CMP     EAX, 0
        JGE     @DIVPOSDIVIDENDMOD
        NEG     EBX
        ADC     EAX, 0
        NEG     EAX
        INC     ESI
@DIVPOSDIVIDENDMOD:
        MOV     EDX, DWORD PTR RIGHT+4    (* get the high word of divisor*)
        MOV     ECX, DWORD PTR RIGHT
        OR      EDX, EDX
        JZ      @POS32DIVISORMOD
        JNS     @DO64BITMOD
        (*
        INC     ESI
        *)
        NEG     ECX
        ADC     EDX, 0
        NEG     EDX
        JZ      @POS32DIVISORMOD
        MOV     DWORD PTR RIGHT+4, EDX
        MOV     DWORD PTR RIGHT, ECX
        JMP     @DO64BITMOD

@POS32DIVISORMOD:
        CMP     EAX, ECX
        JAE     @Do2Divs
        MOV     EDX, EAX
        MOV     EAX, EBX
        DIV     ECX
        MOV     EAX, EDX
        XOR     EDX, EDX
        CMP     ESI, 1
        JNE     @LONGMODRET
        NEG     EAX
        CDQ
        JMP     @LONGMODRET

    @Do2Divs:
        XOR     EDX, EDX
        DIV     ECX
        MOV     EAX, EBX
        DIV     ECX
        MOV     EAX, EDX
        XOR     EDX, EDX
        CMP     ESI, 1
        JNE     @LONGMODRET
        NEG     EAX
        CDQ
        JMP     @LONGMODRET

@DO64BITMOD:
        CALL    DIVIDE
        MOV     EAX, ECX

@LONGMODRET:
        POP     ECX
        POP     ESI
        POP     EBX
        POP     EBP
        RET     16
END LREMS;
(*
%ELSIF Bits64 %THEN
PROCEDURE LREMS(R, L : LONGINT) : LONGINT [LeftToRight]; PUREASM;
ASM
        PUSH    RBX

        MOV     RAX, RSI                      (* get high word of dividend*)
        MOV     RBX, RDI                      (* get low word of dividend*)
        MOV     R9, RCX                       (* get high word of divisor *)
        MOV     R8, RDX                       (* get low word of divisor *)

        XOR     RSI, RSI

        CMP     RAX, 0
        JGE     @DIVPOSDIVIDENDMOD
        NEG     RBX
        ADC     RAX, 0
        NEG     RAX
        INC     RSI
@DIVPOSDIVIDENDMOD:
        MOV     RDX, R9                       (* get the high word of divisor*)
        MOV     RCX, R8
        OR      RDX, RDX
        JZ      @POS64DIVISORMOD
        JNS     @DO128BITMOD
        (*
        INC     ESI
        *)
        NEG     RCX
        ADC     RDX, 0
        NEG     RDX
        JZ      @POS64DIVISORMOD
        MOV     R9, RDX
        MOV     R8, RCX
        JMP     @DO128BITMOD

@POS64DIVISORMOD:
        CMP     RAX, RCX
        JAE     @Do2Divs
        MOV     RDX, RAX
        MOV     RAX, RBX
        DIV     RCX
        MOV     RAX, RDX
        XOR     RDX, RDX
        CMP     RSI, 1
        JNE     @LONGMODRET
        NEG     RAX
        CQO
        JMP     @LONGMODRET

    @Do2Divs:
        XOR     RDX, RDX
        DIV     RCX
        MOV     RAX, RBX
        DIV     RCX
        MOV     RAX, RDX
        XOR     RDX, RDX
        CMP     RSI, 1
        JNE     @LONGMODRET
        NEG     RAX
        CQO
        JMP     @LONGMODRET

@DO128BITMOD:
        CALL    DIVIDE
        MOV     RAX, RCX

@LONGMODRET:
        POP     RBX
        RET
END LREMS;
%ELSE
    fix me
*)
%END

%IF Bits32 %THEN
PROCEDURE LDIVIU(RIGHT, LEFT : LONGCARD) : LONGCARD [ALTERS(AX, DX)]; PUREASM;
ASM
        PUSH    EBP
        MOV     EBP, ESP
        PUSH    EBX

        MOV     EAX, dword ptr LEFT+4    (* get high dword of dividend*)
        MOV     EBX, dword ptr LEFT      (* get low dword of dividend*)
        MOV     EDX, dword ptr RIGHT+4   (* get the high dword of divisor*)
        OR      EDX, EDX
        JNZ     @DO64BITDIVU

        CMP     EAX, DWORD PTR RIGHT
        JAE     @Do2Divs
        MOV     EDX, EAX
        MOV     EAX, EBX
        DIV     DWORD PTR RIGHT
        XOR     EDX, EDX

        POP     EBX
        POP     EBP
        RET     16

@Do2Divs:
        DIV     dword ptr RIGHT
        XCHG    EAX, EBX
        DIV     dword ptr RIGHT
        MOV     EDX, EBX

        POP     EBX
        POP     EBP
        RET     16

@DO64BITDIVU:
        PUSH    ESI
        PUSH    ECX
        XOR     ESI, ESI
        CALL    DIVIDE
        POP     ECX
        POP     ESI
        MOV     EDX, EBX

        POP     EBX
        POP     EBP
        RET     16
END LDIVIU;

PROCEDURE LREMU(RIGHT, LEFT : LONGCARD) : LONGCARD [ALTERS(AX, DX)]; PUREASM;
ASM
        PUSH    EBP
        MOV     EBP, ESP
        PUSH    EBX

        MOV     EAX, dword ptr LEFT+4  (* get high dword of dividend*)
        MOV     EBX, dword ptr LEFT    (* get low dword of dividend*)
        MOV     EDX, dword ptr RIGHT+4 (* get the high dword of divisor*)
        OR      EDX, EDX
        JNZ     @DO64BITMODU

        CMP     EAX, DWORD PTR RIGHT
        JAE     @Do2Divs
        MOV     EDX, EAX
        MOV     EAX, EBX
        DIV     DWORD PTR RIGHT
        MOV     EAX, EDX
        XOR     EDX, EDX
        POP     EBX
        POP     EBP
        RET     16

    @Do2Divs:
        DIV     dword ptr RIGHT
        MOV     EAX, EBX
        DIV     dword ptr RIGHT
        MOV     EAX, EDX
        XOR     EDX, EDX
        POP     EBX
        POP     EBP
        RET     16

@DO64BITMODU:
        PUSH    ESI
        PUSH    ECX
        XOR     ESI, ESI
        CALL    DIVIDE
        MOV     EAX, ECX
        POP     ECX
        POP     ESI
        POP     EBX
        POP     EBP
        RET     16
END LREMU;

PROCEDURE LDIVS(RIGHT, LEFT : LONGINT) : LONGINT [ALTERS(AX, DX)];
VAR
    remainder   : LONGINT;
BEGIN
    IF RIGHT > 0 THEN
        IF LEFT >= 0 THEN
            RETURN LEFT / RIGHT;
        ELSE
            remainder := LEFT REM RIGHT;
            IF remainder = 0 THEN
                RETURN LEFT / RIGHT;
            ELSE
                RETURN (LEFT / RIGHT) - 1;
            END;
        END;
    ELSIF RIGHT = 0 THEN
        RaiseM2Exception(wholeDivException, "WHOLE-ZERO-DIVISION");
    ELSIF RIGHT < 0 THEN
        RaiseM2Exception(wholeDivException, "WHOLE-NONPOS-DIV");
    END;
    RETURN 0;
END LDIVS;

PROCEDURE LMODS(RIGHT, LEFT : LONGINT) : LONGINT [ALTERS(AX, DX)];
VAR
    remainder   : LONGINT;
BEGIN
    IF RIGHT > 0 THEN
        IF LEFT >= 0 THEN
            RETURN LEFT REM RIGHT;
        ELSE
            remainder := LEFT REM RIGHT;
            IF remainder >= 0 THEN
                RETURN remainder;
            ELSE
                RETURN RIGHT + remainder;
            END;
        END;
    ELSIF RIGHT = 0 THEN
        RaiseM2Exception(wholeDivException, "WHOLE-ZERO-DIVISION");
    ELSIF RIGHT < 0 THEN
        RaiseM2Exception(wholeDivException, "WHOLE-NONPOS-DIV");
    END;
    RETURN 0;
END LMODS;

%END

%IF IA32 %THEN
PROCEDURE DIVS(RIGHT, LEFT : INTEGER) : INTEGER [Alters(AX,DX)];
VAR
    remainder   : INTEGER;
%ELSE
PROCEDURE DIVS(RIGHT, LEFT : INTEGER64) : INTEGER64 [LeftToRight];
VAR
    remainder   : INTEGER64;
%END
BEGIN
    IF RIGHT > 0 THEN
        IF LEFT >= 0 THEN
            RETURN LEFT / RIGHT;
        ELSE
            remainder := LEFT REM RIGHT;
            IF remainder = 0 THEN
                RETURN LEFT / RIGHT;
            ELSE
                RETURN (LEFT / RIGHT) - 1;
            END;
        END;
    ELSIF RIGHT = 0 THEN
        RaiseM2Exception(wholeDivException, "WHOLE-ZERO-DIVISION");
    ELSIF RIGHT < 0 THEN
        RaiseM2Exception(wholeDivException, "WHOLE-NONPOS-DIV");
    END;
    RETURN 0;
END DIVS;

%IF IA32 %THEN
PROCEDURE MODS(RIGHT, LEFT : INTEGER) : INTEGER [Alters(AX,DX)];
VAR
    remainder   : INTEGER;
%ELSE
PROCEDURE MODS(RIGHT, LEFT : INTEGER64) : INTEGER64 [LeftToRight];
VAR
    remainder   : INTEGER64;
%END
BEGIN
    IF RIGHT > 0 THEN
        IF LEFT >= 0 THEN
            RETURN LEFT REM RIGHT;
        ELSE
            remainder := LEFT REM RIGHT;
            IF remainder >= 0 THEN
                RETURN remainder;
            ELSE
                RETURN RIGHT + remainder;
            END;
        END;
    ELSIF RIGHT = 0 THEN
        RaiseM2Exception(wholeDivException, "WHOLE-ZERO-DIVISION");
    ELSIF RIGHT < 0 THEN
        RaiseM2Exception(wholeDivException, "WHOLE-NONPOS-DIV");
    END;
    RETURN 0;
END MODS;

%IF Bits32 %THEN
PROCEDURE TTRUNC; PUREASM;
ASM
        PUSH    EBP
        MOV     EBP, ESP
        SUB     ESP, 12

        FSTCW   WORD PTR [EBP]-12
        FLDCW   WORD PTR @NEWCW
        FISTP   QWORD PTR [EBP]-8
        FLDCW   WORD PTR [EBP]-12
        FWAIT

        POP     EAX
        POP     EAX
        POP     EDX
        POP     EBP
        RET

@NEWCW:
        DW      0FFFH
END TTRUNC;
%END

%IF AMD64 %THEN
<*/PUSH/PACK*>
TYPE
    VaVariant =
        RECORD
        CASE : CARDINAL OF
        0: int8 : INTEGER8;|
        1: int16 : INTEGER16;|
        2: int32 : INTEGER32;|
        3: int64 : INTEGER64;|
        4: card8 : CARDINAL8;|
        5: card16 : CARDINAL16;|
        6: card32 : CARDINAL32;|
        7: card64 : CARDINAL64;|
        8: real4  : REAL;|
        9: real8  : LONGREAL;|
        10: machine : ADRCARD;
        (*
        %IF Bits64 %THEN
        |
        13: int128 : INTEGER128;
        %END
        *)
        ELSE
        END;
        END;
<*/POP*>
CONST
    AddressSize         = SIZE(ADDRESS);
    %IF Windows %THEN
        NumIntParams        = 4;
        NumFpuParams        = 4;
    %ELSIF Linux %THEN
        NumIntParams        = 6;
        NumFpuParams        = 8;
    %ELSE
        fix me
    %END

PROCEDURE Align(val : ADDRESS; align : ADRCARD) : ADDRESS [INLINE];
BEGIN
    RETURN CAST(ADDRESS, (CAST(ADRCARD, val) + (align-1)) BAND VAL(ADRCARD, -INT(align)));
END Align;

PROCEDURE VaArgInt(VAR INOUT valist : VA_LIST; typ : INTEGER) : ADRCARD [LeftToRight];
VAR
    ptr         : POINTER TO VaVariant;
BEGIN
    IF valist.intRegNum < NumIntParams THEN
        ptr := ADDADR(valist.regAddr, valist.intRegNum * AddressSize);
        INC(valist.intRegNum);
        %IF Windows %THEN
            INC(valist.fpuRegNum);
        %END
    ELSE
        ptr := valist.stackAddr;
        valist.stackAddr := ADDADR(ptr, AddressSize);
    END;
    (* we have register size specific operands so we do not need to sign extend to
       a "full" register size.
    *)
    CASE ABS(typ) OF
    4:
        RETURN ptr^.card32;
    |
    2:
        RETURN ptr^.card16;
    |
    1:
        RETURN ptr^.card8;
    %IF Bits64 %THEN
    |
    8:
        RETURN ptr^.card64;
    %END
    ELSE
        RaiseM2Exception(sysException, "Invalid typ field in VaArgInt");
    END;
END VaArgInt;

PROCEDURE VaArgLongInt(VAR INOUT valist : VA_LIST; typ : INTEGER) : DoubleInt [LeftToRight];
VAR
    ptr         : POINTER TO VaVariant;
BEGIN
    IF valist.intRegNum+1 < NumIntParams THEN
        ptr := ADDADR(valist.regAddr, valist.intRegNum * AddressSize);
        INC(valist.intRegNum, 2);
        %IF Windows %THEN
            INC(valist.fpuRegNum, 2);
        %END
    ELSE
        valist.intRegNum := NumIntParams;
        ptr := Align(valist.stackAddr, ABS(typ));
        valist.stackAddr := ADDADR(ptr, AddressSize*2);
    END;
    IF typ > 0 THEN
        RETURN ptr^.card64;
    ELSE
        RETURN ptr^.int64;
    END;
END VaArgLongInt;

PROCEDURE max(c1, c2 : CARDINAL) : CARDINAL [INLINE];
BEGIN
    IF c1 < c2 THEN
        RETURN c2;
    END;
    RETURN c1;
END max;

PROCEDURE VaArgReal(VAR INOUT valist : VA_LIST; typ : INTEGER) : LONGREAL [LeftToRight];
VAR
    ptr         : POINTER TO VaVariant;
BEGIN
    IF valist.fpuRegNum < NumFpuParams THEN
        %IF Windows %THEN
            ptr := ADDADR(valist.regAddr, valist.fpuRegNum * AddressSize);
            INC(valist.fpuRegNum);
            INC(valist.intRegNum);
        %ELSE
            ptr := ADDADR(valist.regAddr, valist.fpuRegNum * 8);
            ptr := ADDADR(ptr, NumIntParams * AddressSize);
            INC(valist.fpuRegNum);
        %END
        IF typ = 8 THEN
            RETURN ptr^.real8;
        ELSE
            RETURN ptr^.real4;
        END;
    ELSE
        ptr := Align(valist.stackAddr, typ);
        valist.stackAddr := ADDADR(ptr, max(typ, AddressSize));
        IF typ = 8 THEN
            RETURN ptr^.real8;
        ELSE
            RETURN ptr^.real4;
        END;
    END;
END VaArgReal;
%END

END SYSTEMC.
