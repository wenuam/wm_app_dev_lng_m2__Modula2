(*
Name:     VLI
Creation: 08-02-2001
LastEdit: 14-05-2001
Author:   Egbert J. van der Haring
System:   Stony Brook MODULA-2
Remarks:  very large integer in CARDINAL digit system.
          the procedure GCD is based on a description in the Dutch edition of
          'Cryptography and Network Security, Principles and Practice: second edition' by William Stallings
*)
(* adapted and performance tweaked by Stony Brook Software 2002 *)
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 2009                                  *)
(*                        by ADW Software                                  *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE VLI;
<*/OPT:T*>
<*/NOPACK*>

<*/VALIDVER:UseASM*>
<*/VER:UseASM*>

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRINT, ADRCARD,
    ADR, ADDADR, SUBADR,
    ASSERT, FUNC;

FROM ExStorage IMPORT
    ALLOCATE, DeallocateEx, ReallocateEx, DEALLOCATE,
    GetHeap, HeapInfoPointer;

FROM RandomNumbers IMPORT
    RandomHandle, RandomizeEx, SeedEx, RndEx, DisposeRandomHandle;

CONST
    DigitBits           = SIZE(Digit) * 8;
    AllocIncrement      = (1*1024) / DigitBits;

TYPE
    Digit2              = CARDINAL64;

    ASSERT(SIZE(Digit) = SIZE(DigitS));
    ASSERT(SIZE(Digit2) = 2*SIZE(Digit));

TYPE
    VLIrec =
        RECORD
        used            : CARDINAL;
        size            : CARDINAL;
        neg             : BOOLEAN;
        heap            : HeapInfoPointer;
        digits          : ARRAY [0..0] OF Digit;(*variable length*)
        END;
    VLI         = POINTER TO VLIrec;

    DigitsPointer       = POINTER TO ARRAY [0..0] OF Digit;

<*/PUSH/NOOPT:R*>(*IA-32*)
PROCEDURE Allocate(size : CARDINAL) : VLI;
VAR
    vli         : VLI;
BEGIN
    IF (size REM AllocIncrement) <> 0 THEN
        size := size + (AllocIncrement - (size REM AllocIncrement));
    END;
    ALLOCATE(vli, SIZE(VLIrec) - SIZE(VLIrec.digits) + (size*SIZE(Digit)));

    vli^.heap := GetHeap();
    vli^.neg := FALSE;
    vli^.size := size;
    vli^.used := 1;(*always one digit. eliminates a boundary condition*)
    vli^.digits[0] := 0;
    RETURN vli;
END Allocate;

PROCEDURE Grow(VAR INOUT vli : VLI; index : CARDINAL);
VAR
    alloc       : CARDINAL;
BEGIN
    IF index >= vli^.size THEN
        alloc := vli^.size;
        REPEAT
            INC(alloc, AllocIncrement);
        UNTIL alloc > index;
        vli^.size := alloc;

        alloc := SIZE(VLIrec) - SIZE(VLIrec.digits) + (alloc*SIZE(Digit));
        ReallocateEx(vli, alloc, vli^.heap);
    END;
END Grow;
<*/POP*>

PROCEDURE MoveDigits(ptrD, ptrS : DigitsPointer; count : CARDINAL);
BEGIN
    LOOP
        IF count >= 4 THEN
            count := count - 4;
            ptrD^[0] := ptrS^[0];
            ptrD^[1] := ptrS^[1];
            ptrD^[2] := ptrS^[2];
            ptrD^[3] := ptrS^[3];
            ptrD := ADDADR(ptrD, 4*SIZE(Digit));
            ptrS := ADDADR(ptrS, 4*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            ptrD^[0] := ptrS^[0];
            ptrD^[1] := ptrS^[1];
            ptrD := ADDADR(ptrD, 2*SIZE(Digit));
            ptrS := ADDADR(ptrS, 2*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;
        ELSE
            ptrD^[0] := ptrS^[0];
            EXIT;
        END;
    END;
END MoveDigits;

PROCEDURE ZapDigits(ptrD : DigitsPointer; count : CARDINAL);
BEGIN
    LOOP
        IF count >= 4 THEN
            count := count - 4;
            ptrD^[0] := 0;
            ptrD^[1] := 0;
            ptrD^[2] := 0;
            ptrD^[3] := 0;
            ptrD := ADDADR(ptrD, 4*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            ptrD^[0] := 0;
            ptrD^[1] := 0;
            ptrD := ADDADR(ptrD, 2*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;
        ELSE
            ptrD^[0] := 0;
            EXIT;
        END;
    END;
END ZapDigits;

PROCEDURE CreateI() : VLI [INLINE];
BEGIN
    RETURN Allocate(AllocIncrement);
END CreateI;

PROCEDURE Create() : VLI;
BEGIN
    RETURN CreateI();
END Create;

PROCEDURE Copy(vli : VLI) : VLI;
VAR
    vliCopy     : VLI;
BEGIN
    vliCopy := Allocate(vli^.size);

    vliCopy^.used := vli^.used;
    vliCopy^.neg := vli^.neg;
    MoveDigits(ADR(vliCopy^.digits), ADR(vli^.digits), vli^.used);

    RETURN vliCopy;
END Copy;

PROCEDURE Dispose(VAR INOUT vli : VLI);
VAR
    alloc       : CARDINAL;
BEGIN
    IF vli <> NIL THEN
        alloc := SIZE(VLIrec) - SIZE(VLIrec.digits) + (vli^.size*SIZE(Digit));
        DeallocateEx(vli, alloc, vli^.heap);
    END;
END Dispose;

PROCEDURE Assign(vliSrc : VLI; VAR INOUT vliDst : VLI);
BEGIN
    IF vliSrc <> vliDst THEN
        IF vliDst^.size < vliSrc^.used THEN
            Grow(vliDst, vliSrc^.used-1);
        END;

        vliDst^.neg := vliSrc^.neg;
        vliDst^.used := vliSrc^.used;
        MoveDigits(ADR(vliDst^.digits), ADR(vliSrc^.digits), vliSrc^.used);
    END;
END Assign;

PROCEDURE IsZeroI(vli : VLI) : BOOLEAN;
BEGIN
    RETURN (vli^.used = 1) AND (vli^.digits[0] = 0);
END IsZeroI;

PROCEDURE IsZero(vli : VLI) : BOOLEAN;
BEGIN
    RETURN IsZeroI(vli);
END IsZero;

PROCEDURE IsNegative(vli : VLI) : BOOLEAN;
BEGIN
    RETURN vli^.neg;
END IsNegative;

PROCEDURE IsOddI(VAR INOUT vli : VLI) : BOOLEAN [INLINE];
BEGIN
    RETURN ODD(vli^.digits[0]);
END IsOddI;

PROCEDURE IsOdd(vli : VLI) : BOOLEAN;
BEGIN
    RETURN IsOddI(vli);
END IsOdd;

PROCEDURE SetZeroI(VAR INOUT vli : VLI) [INLINE];
BEGIN
    vli^.neg := FALSE;
    vli^.used := 1;
    vli^.digits[0] := 0;
END SetZeroI;

PROCEDURE SetZero(vli : VLI);
BEGIN
    SetZeroI(vli);
END SetZero;

PROCEDURE Abs(vli : VLI);
BEGIN
    vli^.neg := FALSE;
END Abs;

PROCEDURE ToggleSign(vli : VLI);
BEGIN
    IF NOT IsZeroI(vli) THEN
        vli^.neg := NOT vli^.neg;
    END;
END ToggleSign;

PROCEDURE SetNegI(VAR INOUT vli : VLI; neg : BOOLEAN) [INLINE];
BEGIN
    vli^.neg := FALSE;
    IF NOT IsZeroI(vli) THEN
        vli^.neg := neg;
    END;
END SetNegI;

PROCEDURE GetBitSizeI(VAR INOUT vli : VLI) : CARDINAL [INLINE];
BEGIN
    RETURN vli^.used * DigitBits;
END GetBitSizeI;

PROCEDURE SetDigit(VAR INOUT vli : VLI; index : CARDINAL; dig : Digit);
BEGIN
    IF index >= vli^.size THEN
        Grow(vli, index);
    END;

    vli^.digits[index] := dig;

    IF dig > 0 THEN
        IF index >= vli^.used THEN
            vli^.used := index + 1;
        END;
    ELSE
        (* trim leading zeros. leave at least one digit. *)
        WHILE (vli^.used > 1) AND (vli^.digits[vli^.used-1] = 0) DO
            DEC(vli^.used);
        END;
    END;
END SetDigit;

PROCEDURE SetValue(vli : VLI; value : INTEGER64);
VAR
    i   : CARDINAL;
BEGIN
    IF value <> 0 THEN
        vli^.neg := value < 0;
        value := ABS(value);
        vli^.used := 1;
        FOR i := 0 TO (SIZE(value)/SIZE(Digit))-1 DO
            SetDigit(vli, i, value BAND VAL(INTEGER64, MAX(Digit)));
            value := value SHR DigitBits;
        END;
    ELSE
        SetZero(vli);
    END;
END SetValue;

PROCEDURE IsBitSet(vli : VLI; bit : CARDINAL) : BOOLEAN;
VAR
    index       : CARDINAL;
    b           : Digit;
BEGIN
    index := bit / DigitBits;
    bit := bit REM DigitBits;

    IF index < vli^.used THEN
        b := VAL(Digit, 1) SHL bit;
        RETURN (vli^.digits[index] BAND b) <> 0;
    END;
    RETURN FALSE;
END IsBitSet;

<*/GROUPLIBPROCS:Y*>
PROCEDURE Compare(left, right : VLI) : CompareResults;
VAR
    res         : CompareResults;
BEGIN
    IF left^.neg = right^.neg THEN
        res := CompareAbs(left, right);
        IF NOT left^.neg THEN
            RETURN res;
        ELSIF res = Greater THEN
            RETURN Less;
        ELSIF res = Less THEN
            RETURN Greater;
        END;
        RETURN Equal;
    ELSE
        IF left^.neg THEN
            RETURN Less;
        END;
        RETURN Greater;
    END;
END Compare;

PROCEDURE CompareAbs(left, right : VLI) : CompareResults;
VAR
    index       : ADRCARD;
BEGIN
    IF left^.used < right^.used THEN
        RETURN Less;
    ELSIF left^.used > right^.used THEN
        RETURN Greater;
    ELSE
        index := left^.used;
        REPEAT
            DEC(index);

            IF left^.digits[index] < right^.digits[index] THEN
                RETURN Less;
            ELSIF left^.digits[index] > right^.digits[index] THEN
                RETURN Greater;
            END;
        UNTIL index = 0;
        RETURN Equal;
    END;
END CompareAbs;

PROCEDURE CompareDigit(left : VLI; right : DigitS) : CompareResults;
VAR
    rightNeg    : BOOLEAN;
    res         : CompareResults;
BEGIN
    rightNeg := right < 0;
    right := ABS(right);

    IF left^.neg = rightNeg THEN
        res := CompareAbsDig(left, right);
        IF NOT left^.neg THEN
            RETURN res;
        ELSIF res = Greater THEN
            RETURN Less;
        ELSIF res = Less THEN
            RETURN Greater;
        END;
        RETURN Equal;
    ELSE
        IF left^.neg THEN
            RETURN Less;
        END;
        RETURN Greater;
    END;
END CompareDigit;

<*/GROUPLIBPROCS:N*>(*will be grouped with above procs*)
PROCEDURE CompareAbsDig(left : VLI; right : Digit) : CompareResults;
BEGIN
    IF left^.used > 1 THEN
        RETURN Greater;
    ELSE
        IF left^.digits[0] < right THEN
            RETURN Less;
        ELSIF left^.digits[0] > right THEN
            RETURN Greater;
        END;
        RETURN Equal;
    END;
END CompareAbsDig;

<*/GROUPLIBPROCS:Y*>
(* performs subtraction *)
(* NB: on entry vliA > vliB, vli = 0 *)
PROCEDURE DoSubtract2(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    res         : VLI;
    a,
    bUsed       : ADRCARD;
    borrow,
    bVal,
    aVal        : Digit;
    temp        : Digit2;
BEGIN
    ASSERT(IsZero(result));
    ASSERT(CompareAbs(vliA, vliB) > Equal);

    Grow(result, vliA^.used-1);
    res := result;

    res^.used := vliA^.used;

    bUsed := vliB^.used;
    borrow := 0;
    a := 0;
    REPEAT
        bVal := vliB^.digits[a];
        aVal := vliA^.digits[a];
        temp := VAL(Digit2, aVal) - VAL(Digit2, bVal) - VAL(Digit2, borrow);
        <*/PUSH/NOCHECK:A*>
        res^.digits[a] := temp;
        <*/POP*>
        borrow := VAL(Digit, temp SHR DigitBits) BAND 1;
        INC(a);
    UNTIL a = bUsed;
    <*/PUSH/NOWARN:U*>
    WHILE a <> VAL(ADRCARD, vliA^.used) DO
    <*/POP*>
        aVal := vliA^.digits[a];
        temp := VAL(Digit2, aVal) - VAL(Digit2, borrow);
        <*/PUSH/NOCHECK:A*>
        res^.digits[a] := temp;
        <*/POP*>
        borrow := VAL(Digit, temp SHR DigitBits) BAND 1;
        INC(a);
    END;

    (* trim leading zeros. leave at least one digit. *)
    WHILE (res^.used > 1) AND (res^.digits[res^.used-1] = 0) DO
        DEC(res^.used);
    END;
END DoSubtract2;

PROCEDURE DoSubtract(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    neg         : BOOLEAN;
    temp        : VLI;
BEGIN
    ASSERT(vliA <> result);
    ASSERT(vliB <> result);

    SetZeroI(result);
    neg := FALSE;

    IF CompareAbs(vliA, vliB) = Less THEN
        neg := TRUE;
        temp := vliA;
        vliA := vliB;
        vliB := temp;
    END;

    DoSubtract2(vliA, vliB, result);

    SetNegI(result, neg);
END DoSubtract;

PROCEDURE DoAdd(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    res         : VLI;
    a           : ADRCARD;
    carry       : Digit;
    bUsed       : ADRCARD;
    aVal,
    bVal        : Digit;
    temp        : Digit2;
    tv          : VLI;
BEGIN
    SetZeroI(result);

    IF vliA^.used < vliB^.used THEN
        tv := vliA;
        vliA := vliB;
        vliB := tv;
    END;

    Grow(result, vliA^.used);
    res := result;

    res^.used := vliA^.used+1;

    bUsed := vliB^.used;
    carry := 0;
    a := 0;
    REPEAT
        aVal := vliA^.digits[a];
        bVal := vliB^.digits[a];
        temp := VAL(Digit2, aVal) + VAL(Digit2, bVal) + VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        res^.digits[a] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        INC(a);
    UNTIL a = bUsed;
    <*/PUSH/NOWARN:U*>
    WHILE a <> VAL(ADRCARD, vliA^.used) DO
    <*/POP*>
        aVal := vliA^.digits[a];
        temp := VAL(Digit2, aVal) + VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        res^.digits[a] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        INC(a);
    END;
    res^.digits[a] := carry;

    (* trim leading zeros. leave at least one digit. *)
    IF res^.digits[res^.used-1] = 0 THEN
        DEC(res^.used);
    END;
END DoAdd;

PROCEDURE Add(vliA, vliB : VLI; VAR INOUT result : VLI);
BEGIN
    IF vliA^.neg THEN
        IF vliB^.neg THEN
            DoAdd(vliA, vliB, result);(* (-a)+(-b) = -(a+b) *)
            SetNegI(result, TRUE);
        ELSE
            DoSubtract(vliB, vliA, result);(* (-a)+b = b-a *)
        END;
    ELSIF vliB^.neg THEN
        DoSubtract(vliA, vliB, result);(* a+(-b) = a-b *)
    ELSE
        DoAdd(vliA, vliB, result);(* a + b *)
    END;
END Add;

PROCEDURE Subtract(vliA, vliB : VLI; VAR INOUT result : VLI);
BEGIN
    IF vliA^.neg THEN
        IF vliB^.neg THEN
            DoSubtract(vliB, vliA, result);(* (-a)-(-b) = (-a)+b = b-a *)
        ELSE
            DoAdd(vliA, vliB, result);(* (-a)-b = (-a)+(-b) = -(a+b) *)
            SetNegI(result, TRUE);
        END;
    ELSIF vliB^.neg THEN
        DoAdd(vliA, vliB, result);(* a-(-b) = a+b *)
    ELSE
        DoSubtract(vliA, vliB, result);(* a - b *)
    END;
    (*
    vliB^.neg := NOT vliB^.neg;
    Add(vliA, vliB, result);
    vliB^.neg := NOT vliB^.neg;
    *)
END Subtract;

%IF IA32 %AND UseASM %THEN

(* this procedure only works if Digit is 32-bit *)
PROCEDURE MultiplyDigitAdd(ptrA : DigitsPointer;
                           digitB : Digit;
                           ptrR : DigitsPointer;
                           count : CARDINAL;
                           assign : BOOLEAN) : Digit; ASSEMBLER;
(*
  ESI = ptrA
  EDI = ptrR
  ECX = digitB
  EBP = count

  EBX = carry      Initialized to 0

  This procedure loops computing as follows:
  tempDigit2 := (ptrA^[x] * digitB) + ptrR^[x] + carry;
  carry := tempDigit2 SHR DigitBits
  ptr^[x] := tempDigits;


  NOTE: tempDigit2 is accumulated in EDX:EAX. (* SIZE Digit2 *)
        ptrR, ptrA point to a Digit
        carry is a Digit
*)
ASM
        MOV     ESI, ptrA
        MOV     EDI, ptrR
        MOV     ECX, digitB
        MOV     EBX, 0              (* carry *)
        MOV     EAX, count
        PUSH    EBP
        MOV     EBP, EAX
    @Top:
        CMP     EBP, 8
        JB      @Try4

    @Do8:
        MOV     EAX, [ESI]
        SUB     EBP, 8
        MUL     ECX
        ADD     EAX, [EDI]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI], EAX

        MOV     EAX, [ESI+4]
        MUL     ECX
        ADD     EAX, [EDI+4]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+4], EAX

        MOV     EAX, [ESI+8]
        MUL     ECX
        ADD     EAX, [EDI+8]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+8], EAX

        MOV     EAX, [ESI+12]
        MUL     ECX
        ADD     EAX, [EDI+12]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+12], EAX

        MOV     EAX, [ESI+16]
        MUL     ECX
        ADD     EAX, [EDI+16]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+16], EAX

        MOV     EAX, [ESI+20]
        MUL     ECX
        ADD     EAX, [EDI+20]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+20], EAX

        MOV     EAX, [ESI+24]
        MUL     ECX
        ADD     EAX, [EDI+24]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+24], EAX

        MOV     EAX, [ESI+28]
        MUL     ECX
        ADD     EAX, [EDI+28]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+28], EAX

        ADD     ESI, 8*4
        ADD     EDI, 8*4

        CMP     EBP, 8
        JGE     @Do8
        TEST    EBP, EBP
        JLE     @EndLoop

    @Try4:
        CMP     EBP, 4
        JB      @Try2
        SUB     EBP, 4

        MOV     EAX, [ESI]
        MUL     ECX
        ADD     EAX, [EDI]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI], EAX

        MOV     EAX, [ESI+4]
        MUL     ECX
        ADD     EAX, [EDI+4]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+4], EAX

        MOV     EAX, [ESI+8]
        MUL     ECX
        ADD     EAX, [EDI+8]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+8], EAX

        MOV     EAX, [ESI+12]
        MUL     ECX
        ADD     EAX, EBX
        ADC     EDX, 0
        ADD     EAX, [EDI+12]
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+12], EAX

        ADD     ESI, 4*4
        ADD     EDI, 4*4

        TEST    EBP, EBP
        JE      @EndLoop

    @Try2:
        CMP     EBP, 2
        JB      @Try1
        SUB     EBP, 2

        MOV     EAX, [ESI]
        MUL     ECX
        ADD     EAX, [EDI]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI], EAX

        MOV     EAX, [ESI+4]
        MUL     ECX
        ADD     EAX, [EDI+4]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     EBX, EDX
        MOV     [EDI+4], EAX

        ADD     ESI, 2*4
        ADD     EDI, 2*4

        TEST    EBP, EBP
        JE      @EndLoop

    @Try1:
        MOV     EAX, [ESI]
        MUL     ECX
        ADD     EAX, [EDI]
        ADC     EDX, 0
        ADD     EAX, EBX
        ADC     EDX, 0
        MOV     [EDI], EAX
        (*MOV     EBX, EDX*)

        ADD     EDI, 4

    @EndLoop:
        POP     EBP

        CMP     assign, 0
        je      @Exit
        MOV     [EDI], EDX

    @Exit:
        MOV     EAX, EDX
END MultiplyDigitAdd;

%ELSE

PROCEDURE MultiplyDigitAdd(ptrA : DigitsPointer;
                           digitB : Digit;
                           ptrR : DigitsPointer;
                           count : CARDINAL;
                           assign : BOOLEAN) : Digit;
VAR
    temp        : Digit2;
    carry       : Digit;
BEGIN
    (* writing the code ugly (pointer arithmetic) causes simpler addressing
       to be used for memory ops.
       ugly is okay for a performance critical loop.
    *)
    carry := 0;
    LOOP
        IF count >= 8 THEN
            count := count - 8;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[0])) +
                     VAL(Digit2, ptrR^[0]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[1])) +
                     VAL(Digit2, ptrR^[1]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[2])) +
                     VAL(Digit2, ptrR^[2]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[2] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[3])) +
                     VAL(Digit2, ptrR^[3]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[3] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[4])) +
                     VAL(Digit2, ptrR^[4]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[4] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[5])) +
                     VAL(Digit2, ptrR^[5]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[5] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[6])) +
                     VAL(Digit2, ptrR^[6]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[6] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[7])) +
                     VAL(Digit2, ptrR^[7]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[7] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 8*SIZE(Digit));
            ptrR := ADDADR(ptrR, 8*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSIF count >= 4 THEN
            count := count - 4;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[0])) +
                     VAL(Digit2, ptrR^[0]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[1])) +
                     VAL(Digit2, ptrR^[1]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[2])) +
                     VAL(Digit2, ptrR^[2]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[2] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[3])) +
                     VAL(Digit2, ptrR^[3]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[3] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 4*SIZE(Digit));
            ptrR := ADDADR(ptrR, 4*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSIF count >= 2 THEN
            count := count - 2;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[0])) +
                     VAL(Digit2, ptrR^[0]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[1])) +
                     VAL(Digit2, ptrR^[1]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 2*SIZE(Digit));
            ptrR := ADDADR(ptrR, 2*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSE
            temp := (VAL(Digit2, digitB) * VAL(Digit2, ptrA^[0])) +
                     VAL(Digit2, ptrR^[0]) +
                     VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;
            ptrR := ADDADR(ptrR, SIZE(Digit));
            EXIT;
        END;
    END;

    IF assign THEN
        ptrR^[0] := carry;
    END;

    RETURN carry;
END MultiplyDigitAdd;

%END

PROCEDURE Multiply2(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    b           : ADRCARD;
    res         : VLI;
    aUsed,
    bUsed       : ADRCARD;
BEGIN
    ASSERT(IsZero(result));

    Grow(result, vliB^.used + vliA^.used -1);
    result^.used := vliB^.used + vliA^.used;
    ZapDigits(ADR(result^.digits), result^.used);

    res := result;
    bUsed := vliB^.used;
    aUsed := vliA^.used;
    b := 0;
    REPEAT
        FUNC MultiplyDigitAdd(ADR(vliA^.digits),
                              vliB^.digits[b],
                              ADR(res^.digits[b]),
                              aUsed,
                              TRUE);

        INC(b);
    UNTIL b = bUsed;

    (* trim leading zeros. leave at least one digit. *)
    IF res^.digits[res^.used-1] = 0 THEN
        DEC(res^.used);
    END;
END Multiply2;

PROCEDURE SquareDigits(R, A : DigitsPointer; N : CARDINAL) [INLINE];
VAR
    i           : ADRCARD;
    carry       : Digit;
    temp        : Digit2;
    last        : ADRCARD;
    digitB      : Digit;
    ptrR        : DigitsPointer;
BEGIN
    last := N-1;
    i := 0;
    REPEAT
        FUNC MultiplyDigitAdd(ADR(A^[i+1]),
                              A^[i],
                              ADR(R^[i+i+1]),
                              last-i,
                              TRUE);
        INC(i);
    UNTIL i = last;

    ptrR := R;
    carry := 0;
    <*/PUSH/NOWARN:U*>
    FOR i := 0 TO VAL(ADRCARD, (N*2)-1) DO
    <*/POP*>
        digitB := ptrR^[0];
        temp := VAL(Digit2, digitB) + VAL(Digit2, digitB) + VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        ptrR^[0] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        ptrR := ADDADR(ptrR, SIZE(Digit));
    END;

    ptrR := R;
    carry := 0;
    <*/PUSH/NOWARN:U*>
    FOR i := 0 TO VAL(ADRCARD, N-1) DO
    <*/POP*>
        digitB := A^[i];
        temp := (VAL(Digit2, digitB) * VAL(Digit2, digitB)) +
                 VAL(Digit2, ptrR^[0]) +
                 VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        ptrR^[0] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        temp := VAL(Digit2, ptrR^[1]) + VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        ptrR^[1] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        ptrR := ADDADR(ptrR, 2*SIZE(Digit));
    END;
END SquareDigits;

PROCEDURE Square(vliA : VLI; VAR INOUT result : VLI);
VAR
    res         : VLI;
BEGIN
    ASSERT(IsZero(result));

    Grow(result, vliA^.used*2-1);
    result^.used := vliA^.used*2;
    ZapDigits(ADR(result^.digits), result^.used);

    res := result;

    SquareDigits(ADR(res^.digits), ADR(vliA^.digits), vliA^.used);

    (* trim leading zeros. leave at least one digit. *)
    IF res^.digits[res^.used-1] = 0 THEN
        DEC(res^.used);
    END;
END Square;

PROCEDURE MultiplyByDigit(vliA : VLI; digitIn : Digit; VAR INOUT result : VLI);
VAR
    i           : CARDINAL;
    ptrA, ptrR  : DigitsPointer;
    temp        : Digit2;
    carry       : Digit;
    res         : VLI;
BEGIN
    ASSERT(IsZero(result));

    IF digitIn = 0 THEN
        RETURN;
    END;

    Grow(result, vliA^.used+1);
    res := result;

    res^.used := vliA^.used+1;
    res^.digits[0] := 0;

    (* writing the code ugly (pointer arithmetic) causes simpler addressing
       to be used for memory ops.
       ugly is okay for a performance critical loop.
    *)
    ptrA := ADR(vliA^.digits);
    ptrR := ADR(res^.digits);
    i := vliA^.used;
    carry := 0;
    LOOP
        IF i >= 8 THEN
            i := i - 8;

            temp := (VAL(Digit2, ptrA^[0]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[1]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[2]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[2] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[3]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[3] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[4]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[4] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[5]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[5] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[6]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[6] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[7]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[7] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 8*SIZE(Digit));
            ptrR := ADDADR(ptrR, 8*SIZE(Digit));
            IF i = 0 THEN
                EXIT;
            END;

        ELSIF i >= 4 THEN
            i := i - 4;

            temp := (VAL(Digit2, ptrA^[0]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[1]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[2]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[2] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[3]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[3] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 4*SIZE(Digit));
            ptrR := ADDADR(ptrR, 4*SIZE(Digit));
            IF i = 0 THEN
                EXIT;
            END;

        ELSIF i >= 2 THEN
            i := i - 2;

            temp := (VAL(Digit2, ptrA^[0]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            temp := (VAL(Digit2, ptrA^[1]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[1] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrA := ADDADR(ptrA, 2*SIZE(Digit));
            ptrR := ADDADR(ptrR, 2*SIZE(Digit));
            IF i = 0 THEN
                EXIT;
            END;

        ELSE
            temp := (VAL(Digit2, ptrA^[0]) * VAL(Digit2, digitIn)) + VAL(Digit2, carry);
            <*/PUSH/NOCHECK:A*>
            ptrR^[0] := temp;
            <*/POP*>
            carry := temp SHR DigitBits;

            ptrR := ADDADR(ptrR, SIZE(Digit));
            EXIT;
        END;
    END;

    IF carry <> 0 THEN
        ptrR^[0] := carry;
    ELSE
        DEC(res^.used);
    END;
END MultiplyByDigit;

PROCEDURE CompareDigits(L, R : DigitsPointer; N : CARDINAL) : INTEGER;
BEGIN
    REPEAT
        DEC(N);

        IF L^[N] < R^[N] THEN
            RETURN -1;
        ELSIF L^[N] > R^[N] THEN
            RETURN 1;
        END;
    UNTIL N = 0;
    RETURN 0;
END CompareDigits;

PROCEDURE SubDigits(R, A, B : DigitsPointer; N : CARDINAL);
VAR
    borrow      : Digit;
    i           : CARDINAL;
    temp        : Digit2;
    aVal,
    bVal        : Digit;
BEGIN
    borrow := 0;
    i := 0;
    REPEAT
        INC(i);
        aVal := A^[0];
        A := ADDADR(A, SIZE(Digit));
        bVal := B^[0];
        B := ADDADR(B, SIZE(Digit));
        temp := VAL(Digit2, aVal) - VAL(Digit2, bVal) - VAL(Digit2, borrow);
        <*/PUSH/NOCHECK:A*>
        R^[0] := temp;
        <*/POP*>
        borrow := VAL(Digit, temp SHR DigitBits) BAND 1;
        R := ADDADR(R, SIZE(Digit));
    UNTIL i = N;
END SubDigits;

PROCEDURE AddDigits(R, A, B : DigitsPointer; N : CARDINAL) : Digit;
VAR
    aVal, bVal  : Digit;
    carry       : Digit;
    i           : CARDINAL;
    temp        : Digit2;
BEGIN
    i := 0;
    carry := 0;
    REPEAT
        INC(i);
        aVal := A^[0];
        A := ADDADR(A, SIZE(Digit));
        bVal := B^[0];
        B := ADDADR(B, SIZE(Digit));
        temp := VAL(Digit2, aVal) + VAL(Digit2, bVal) + VAL(Digit2, carry);
        <*/PUSH/NOCHECK:A*>
        R^[0] := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        R := ADDADR(R, SIZE(Digit));
    UNTIL i = N;

    RETURN carry;
END AddDigits;

PROCEDURE IncDigits(A : DigitsPointer; B : Digit; N : CARDINAL) : Digit;
VAR
    save        : Digit2;
    i           : ADRCARD;
    carry       : Digit;
BEGIN
    carry := B;;

    i := 0;
    REPEAT
        save := VAL(Digit2, A^[i]) + VAL(Digit2, carry);
        A^[i] := save;
        carry := save SHR DigitBits;
        INC(i);
    <*/PUSH/NOWARN:U*>
    UNTIL (carry = 0) OR (i >= VAL(ADRCARD, N));
    <*/POP*>

    RETURN carry;
END IncDigits;

PROCEDURE Karatsuba(R, T, A, B : DigitsPointer; N : ADRCARD);
CONST
    BasicThreshold      = 32;
VAR
    i           : ADRCARD;
    N2          : ADRCARD;
    aComp       : INTEGER;
    bComp       : INTEGER;
    carry       : INTEGER;
    c           : Digit;
    Ah, Bh      : DigitsPointer;
    T1, T2, T3  : DigitsPointer;
    R1, R2, R3  : DigitsPointer;
BEGIN
    IF (N <= BasicThreshold) OR ((N REM 2) <> 0) THEN
        ZapDigits(R, N*2);
        i := 0;
        REPEAT
            FUNC MultiplyDigitAdd(A, B^[i], ADR(R^[i]), N, TRUE);
            INC(i);
        <*/PUSH/NOWARN:U*>
        UNTIL i = VAL(ADRCARD, N);
        <*/POP*>
    ELSE
        N2 := N / 2;

        T1 := ADR(T^[N2]);
        T2 := ADR(T^[N]);
        T3 := ADR(T^[N+N2]);
        R1 := ADR(R^[N2]);
        R2 := ADR(R^[N]);
        R3 := ADR(R^[N+N2]);
        Ah := ADR(A^[N2]);
        Bh := ADR(B^[N2]);

        aComp := CompareDigits(A, Ah, N2);
        bComp := CompareDigits(B, Bh, N2);
        CASE (3*aComp) + bComp OF
        -4:
            SubDigits(R, Ah, A, N2);
            SubDigits(R1, B, Bh, N2);
            Karatsuba(T, T2, R, R1, N2);
            SubDigits(T1, T1, R, N2);
            carry := -1;
        |
        -2:
            SubDigits(R, Ah, A, N2);
            SubDigits(R1, B, Bh, N2);
            Karatsuba(T, T2, R, R1, N2);
            carry := 0;
        |
        2:
            SubDigits(R, A, Ah, N2);
            SubDigits(R1, Bh, B, N2);
            Karatsuba(T, T2, R, R1, N2);
            carry := 0;
        |
        4:
            SubDigits(R, Ah, A, N2);
            SubDigits(R1, B, Bh, N2);
            Karatsuba(T, T2, R, R1, N2);
            SubDigits(T1, T1, R1, N2);
            carry := -1;
        ELSE
            ZapDigits(T, N);
            carry := 0;
        END;

        Karatsuba(R, T2, A, B, N2);
        Karatsuba(R2, T2, Ah, Bh, N2);

        (* T[0..1] = (Ah-Al)*(Bl-Bh),
           R[0..1] = Al*Bl,
           R[2..3] = Ah*Bh
        *)

        c := AddDigits(T, T, R, N);
        c := c + AddDigits(T, T, R2, N);
        c := c + AddDigits(R1, R1, T, N);

        c := IncDigits(R3, carry + INT(c), N2);
    END;
END Karatsuba;

PROCEDURE KaratsubaSquare(R, T, A : DigitsPointer; N : ADRCARD);
CONST
    BasicThreshold      = 32;
VAR
    N2          : ADRCARD;
    c           : Digit;
    Ah          : DigitsPointer;
    T2          : DigitsPointer;
    R1, R2, R3  : DigitsPointer;
BEGIN
    IF (N <= BasicThreshold) OR ((N REM 2) <> 0) THEN
        ZapDigits(R, N*2);
        SquareDigits(R, A, N);
    ELSE
        N2 := N / 2;

        T2 := ADR(T^[N]);
        R1 := ADR(R^[N2]);
        R2 := ADR(R^[N]);
        R3 := ADR(R^[N+N2]);
        Ah := ADR(A^[N2]);

        KaratsubaSquare(R, T2, A, N2);
        KaratsubaSquare(R2, T2, Ah, N2);
        Karatsuba(T, T2, A, Ah, N2);

        c := AddDigits(R1, R1, T, N);
        c := c + AddDigits(R1, R1, T, N);
        c := IncDigits(R3, c, N2);
    END;
END KaratsubaSquare;

CONST
    KaratsubaThreshold  = 48;

PROCEDURE KaraMultiply(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    N2          : CARDINAL;
    aN, bN      : ADRCARD;
    i, t        : ADRCARD;
    carry       : Digit;
    res         : VLI;
    tA          : DigitsPointer;
    A, B        : DigitsPointer;
    T, T2, R    : DigitsPointer;
BEGIN
    aN := vliA^.used;
    IF ODD(aN) THEN
        vliA^.digits[aN] := 0;(*place a leading zero*)
        INC(aN);
    END;

    bN := vliB^.used;
    IF ODD(bN) THEN
        vliB^.digits[bN] := 0;(*place a leading zero*)
        INC(bN);
    END;

    Grow(result, aN+bN-1);
    result^.used := aN+bN;

    N2 := aN * 2;

    ALLOCATE(T, N2*SIZE(Digit));

    R := ADR(result^.digits);
    A := ADR(vliA^.digits);
    B := ADR(vliB^.digits);

    IF A = B THEN
        KaratsubaSquare(R, T, A, bN);
    ELSE
        Karatsuba(R, T, A, B, bN);
    END;

    IF aN <> bN THEN
        (* put zeros into the high digits of the result
           which have not yet been assigned.
        *)
        <*/PUSH/NOWARN:U*>
        ZapDigits(ADR(R^[bN*2]), VAL(ADRCARD, result^.used) - (bN*2));
        <*/POP*>

        R := ADDADR(R, bN*SIZE(Digit));
        A := ADDADR(A, bN*SIZE(Digit));
        T2 := ADDADR(T, bN*2*SIZE(Digit));
        aN := aN - bN;

        (* we always keep A the same size or bigger than B *)
        IF aN < bN THEN
            tA := A;
            A := B;
            B := tA;

            t := aN;
            aN := bN;
            bN := t;
        END;

        WHILE bN >= KaratsubaThreshold DO
            Karatsuba(T, T2, A, B, bN);

            (* not possible to carry out of the last digit position.
               the high digit of this product is always adding into
               a zero in the result.
            *)
            carry := AddDigits(R, R, T, bN*2);
            IF carry <> 0 THEN
                A := NIL;
                A^[0] := 23;
            END;

            A := ADDADR(A, bN*SIZE(Digit));
            R := ADDADR(R, bN*SIZE(Digit));
            aN := aN - bN;

            IF aN < bN THEN
                tA := A;
                A := B;
                B := tA;

                t := aN;
                aN := bN;
                bN := t;
            END;
        END;

        (* handle anything left with a basic multiply *)

        IF aN <> 0 THEN
            ZapDigits(T, aN+bN);
            i := 0;
            REPEAT
                FUNC MultiplyDigitAdd(A, B^[i], ADR(T^[i]), aN, TRUE);
                INC(i);
            UNTIL i = bN;
            (* not possible to carry out of the last digit position
               the high digit of this product is always adding into
               a zero in the result.
            *)
            carry := AddDigits(R, R, T, aN+bN);
            IF carry <> 0 THEN
                A := NIL;
                A^[0] := 23;
            END;
        END;
    END;

    DEALLOCATE(T, N2*SIZE(Digit));

    (* trim leading zeros. leave at least one digit. *)
    res := result;
    WHILE (res^.used > 1) AND (res^.digits[res^.used-1] = 0) DO
        DEC(res^.used);
    END;
END KaraMultiply;

PROCEDURE Multiply(vliA, vliB : VLI; VAR INOUT result : VLI);
CONST
    KaratsubaSquareThreshold  = 256;
VAR
    neg         : BOOLEAN;
    t           : VLI;
BEGIN
    SetZeroI(result);

    IF (vliA^.used > 1) AND (vliB^.used > 1) THEN
        IF vliA = vliB THEN
            IF vliA^.used < KaratsubaSquareThreshold THEN
                Square(vliA, result);
            ELSE
                KaraMultiply(vliA, vliA, result);
            END;
        ELSE
            IF vliB^.used > vliA^.used THEN
                t := vliB;
                vliB := vliA;
                vliA := t;
            END;

            IF vliB^.used < KaratsubaThreshold THEN
                Multiply2(vliA, vliB, result);
            ELSE
                KaraMultiply(vliA, vliB, result);
            END;
        END;
    ELSIF vliB^.used = 1 THEN
        IF vliB^.digits[0] > 1 THEN
            MultiplyByDigit(vliA, vliB^.digits[0], result);
        ELSIF vliB^.digits[0] = 1 THEN
            Assign(vliA, result);
        END;
    ELSE
        IF vliA^.digits[0] > 1 THEN
            MultiplyByDigit(vliB, vliA^.digits[0], result);
        ELSIF vliA^.digits[0] = 1 THEN
            Assign(vliB, result);
        END;
    END;

    neg := FALSE;
    IF vliA^.neg THEN
        neg := NOT vliB^.neg;
    ELSIF vliB^.neg THEN
        neg := TRUE;
    END;
    SetNegI(result, neg);
END Multiply;

(* returns estimate for vliA/vliB. A > B in entry *)
PROCEDURE GetEstimate(vliA, vliB : VLI; VAR OUT lower, upper : Digit);
VAR
    digitA      : Digit;
    digitB      : Digit;
    temp        : Digit2;
BEGIN
    ASSERT(CompareAbs(vliA, vliB) > Equal);

    digitA := vliA^.digits[vliA^.used-1];
    digitB := vliB^.digits[vliB^.used-1];

    IF digitA >= digitB THEN
        upper := digitA / digitB;
        lower := digitA / (digitB+1);
    ELSE
        temp := VAL(Digit2, digitA) SHL DigitBits;
        temp := temp BOR VAL(Digit2, vliA^.digits[vliA^.used-2]);

        upper := temp / VAL(Digit2, digitB);
        lower := temp / VAL(Digit2, digitB+1);
    END;
END GetEstimate;

%IF IA32 %AND UseAsm %THEN

PROCEDURE DoSubtract3(vliA, vliB : VLI); ASSEMBLER;
ASM
    MOV         ESI, vliB
    MOV         ECX, [ESI].VLIrec.used       (* ECX = count *)
    LEA         ESI, [ESI].VLIrec.digits     (* ESI = ADR(vliB^.digits) *)

    MOV         EDI, vliA
    LEA         EDI, [EDI].VLIrec.digits     (* EDI = ADR(vliA^.digits) *)

    MOV         EDX, 0      (* borrow *)

    CMP         ECX, 8
    JB          @Try4
@LoopTop:
    SUB         ECX, 8
    ADD         EDX, 0FFFFFFFFh   (* Get carry flag out of EDX *)

    MOV         EAX, [EDI]
    SBB         EAX, [ESI]
    MOV         [EDI], EAX

    MOV         EDX, 0            (* clear EDX so we can put carry in later *)

    MOV         EAX, [EDI+4]
    SBB         EAX, [ESI+4]
    MOV         [EDI+4], EAX

    MOV         EAX, [EDI+8]
    SBB         EAX, [ESI+8]
    MOV         [EDI+8], EAX

    MOV         EAX, [EDI+12]
    SBB         EAX, [ESI+12]
    MOV         [EDI+12], EAX

    MOV         EAX, [EDI+16]
    SBB         EAX, [ESI+16]
    MOV         [EDI+16], EAX

    MOV         EAX, [EDI+20]
    SBB         EAX, [ESI+20]
    MOV         [EDI+20], EAX

    MOV         EAX, [EDI+24]
    SBB         EAX, [ESI+24]
    MOV         [EDI+24], EAX

    MOV         EAX, [EDI+28]
    SBB         EAX, [ESI+28]
    MOV         [EDI+28], EAX

    ADC         EDX, 0

    ADD         EDI, 8*4
    ADD         ESI, 8*4

    CMP         ECX, 8
    JGE         @LoopTop
    OR          ECX, ECX
    JZ          @CarryLoop

@Try4:
    CMP         ECX, 4
    JL          @Try2

    SUB         ECX, 4
    ADD         EDX, 0FFFFFFFFh   (* Get carry flag out of EDX *)

    MOV         EAX, [EDI]
    SBB         EAX, [ESI]
    MOV         [EDI], EAX

    MOV         EDX, 0            (* clear EDX so we can put carry in later *)

    MOV         EAX, [EDI+4]
    SBB         EAX, [ESI+4]
    MOV         [EDI+4], EAX

    MOV         EAX, [EDI+8]
    SBB         EAX, [ESI+8]
    MOV         [EDI+8], EAX

    MOV         EAX, [EDI+12]
    SBB         EAX, [ESI+12]
    MOV         [EDI+12], EAX

    ADC         EDX, 0

    ADD         EDI, 4*4
    ADD         ESI, 4*4

    OR          ECX, ECX
    JZ          @CarryLoop

@Try2:
    CMP         ECX, 2
    JL          @Do1
    SUB         ECX, 2

    ADD         EDX, 0FFFFFFFFh   (* Get carry flag out of EDX *)

    MOV         EAX, [EDI]
    SBB         EAX, [ESI]
    MOV         [EDI], EAX

    MOV         EDX, 0            (* clear EDX so we can put carry in later *)

    MOV         EAX, [EDI+4]
    SBB         EAX, [ESI+4]
    MOV         [EDI+4], EAX

    ADC         EDX, 0

    ADD         EDI, 2*4
    ADD         ESI, 2*4

    OR          ECX, ECX
    JZ          @CarryLoop

@Do1:
    ADD         EDX, 0FFFFFFFFh   (* Get carry flag out of EDX *)

    MOV         EAX, [EDI]
    SBB         EAX, [ESI]
    MOV         [EDI], EAX

    MOV         EDX, 0            (* clear EDX so we can put carry in later *)

    ADC         EDX, 0

    ADD         EDI, 4

@CarryLoop:
    OR          EDX, EDX
    JZ          @EndCarryLoop
    SHR         EDX, 1

@CarryLoop1:
    SBB         DWORD PTR [EDI], 0
    ADD         EDI, 4
    OR          EDX, EDX
    JNZ         @CarryLoop1
@EndCarryLoop:

    MOV         EDI, vliA
    MOV         EAX, [EDI].VLIrec.used
@UsedLoop:
    CMP         EAX, 1
    JLE         @Done
    CMP         DWORD PTR [EDI+EAX*4-4].VLIrec.digits, 0
    JNE         @Done
    DEC         EAX
    JMP         @UsedLoop
@Done:
    MOV         [EDI].VLIrec.used, EAX
END DoSubtract3;

%ELSE

PROCEDURE DoSubtract3(vliA, vliB : VLI);(* A := A - B *)
VAR
    count       : CARDINAL;
    borrow,
    aVal        : Digit;
    ptrA, ptrB  : DigitsPointer;
    temp        : Digit2;
BEGIN
    ASSERT(CompareAbs(vliA, vliB) > Equal);

    ptrA := ADR(vliA^.digits);
    ptrB := ADR(vliB^.digits);
    count := vliB^.used;
    borrow := 0;
    LOOP
        IF count >= 8 THEN
            count := count - 8;

            temp := VAL(Digit2, ptrA^[0]) - VAL(Digit2, ptrB^[0]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[0] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[1]) - VAL(Digit2, ptrB^[1]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[1] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[2]) - VAL(Digit2, ptrB^[2]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[2] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[3]) - VAL(Digit2, ptrB^[3]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[3] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[4]) - VAL(Digit2, ptrB^[4]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[4] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[5]) - VAL(Digit2, ptrB^[5]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[5] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[6]) - VAL(Digit2, ptrB^[6]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[6] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[7]) - VAL(Digit2, ptrB^[7]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[7] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            ptrA := ADDADR(ptrA, 8*SIZE(Digit));
            ptrB := ADDADR(ptrB, 8*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSIF count >= 4 THEN
            count := count - 4;

            temp := VAL(Digit2, ptrA^[0]) - VAL(Digit2, ptrB^[0]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[0] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[1]) - VAL(Digit2, ptrB^[1]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[1] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[2]) - VAL(Digit2, ptrB^[2]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[2] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[3]) - VAL(Digit2, ptrB^[3]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[3] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            ptrA := ADDADR(ptrA, 4*SIZE(Digit));
            ptrB := ADDADR(ptrB, 4*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSIF count >= 2 THEN
            count := count - 2;

            temp := VAL(Digit2, ptrA^[0]) - VAL(Digit2, ptrB^[0]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[0] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            temp := VAL(Digit2, ptrA^[1]) - VAL(Digit2, ptrB^[1]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[1] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            ptrA := ADDADR(ptrA, 2*SIZE(Digit));
            ptrB := ADDADR(ptrB, 2*SIZE(Digit));
            IF count = 0 THEN
                EXIT;
            END;

        ELSE
            temp := VAL(Digit2, ptrA^[0]) - VAL(Digit2, ptrB^[0]) - VAL(Digit2, borrow);
            <*/PUSH/NOCHECK:A*>
            ptrA^[0] := temp;
            <*/POP*>
            borrow := VAL(Digit, temp SHR DigitBits) BAND 1;

            ptrA := ADDADR(ptrA, SIZE(Digit));
            EXIT;
        END;
    END;
    WHILE borrow <> 0 DO
        aVal := ptrA^[0];
        <*/PUSH/NOCHECK:O*>
        ptrA^[0] := aVal - 1;
        <*/POP*>
        ptrA := ADDADR(ptrA, SIZE(Digit));

        borrow := ORD(aVal = 0);
    END;

    (* trim leading zeros. leave at least one digit. *)
    WHILE (vliA^.used > 1) AND (vliA^.digits[vliA^.used-1] = 0) DO
        DEC(vliA^.used);
    END;
END DoSubtract3;

%END

PROCEDURE ShiftLeftDigitAndSet(VAR INOUT vli : VLI; dig : Digit);
VAR
    count       : CARDINAL;
    ptr         : POINTER TO ARRAY [0..4] OF Digit;
BEGIN
    IF NOT IsZeroI(vli) THEN
        INC(vli^.used);
        Grow(vli, vli^.used-1);

        count := vli^.used;
        (*ptr := ADR(vli^.digits[count-5]);*)
        ptr := ADR(vli^.digits[count-1]);
        ptr := SUBADR(ptr, 4*SIZE(Digit));
        LOOP
            IF count >= 4 THEN
                count := count - 4;
                ptr^[4] := ptr^[3];
                ptr^[3] := ptr^[2];
                ptr^[2] := ptr^[1];
                ptr^[1] := ptr^[0];
                ptr := SUBADR(ptr, 4 * SIZE(Digit));
                IF count = 0 THEN
                    EXIT;
                END;

            ELSIF count >= 2 THEN
                count := count - 2;
                ptr^[4] := ptr^[3];
                ptr^[3] := ptr^[2];
                ptr := SUBADR(ptr, 2 * SIZE(Digit));
                IF count = 0 THEN
                    EXIT;
                END;

            ELSE
                ptr^[4] := ptr^[3];
                EXIT;
            END;
        END;
    END;

    vli^.digits[0] := dig;
END ShiftLeftDigitAndSet;

(* divides A / B *)
(* NB: on entry vliA <> 0, vliB <> 0, vliA > vliB *)
PROCEDURE DoDivide(vliA, vliB : VLI;
                   VAR INOUT result : VLI;
                   VAR INOUT remainder : VLI);

VAR
    indexA      : ADRCARD;
    times       : Digit;
    vliT1       : VLI;
    upper,
    lower       : Digit;
    aUsed,
    bUsed       : ADRCARD;
    top         : ADRCARD;
    firstTime   : BOOLEAN;
BEGIN
    ASSERT(IsZero(result));
    ASSERT(IsZero(remainder));
    ASSERT(NOT IsZero(vliA));
    ASSERT(NOT IsZero(vliB));
    ASSERT(CompareAbs(vliA, vliB) > Equal);

    vliT1 := CreateI();

    aUsed := vliA^.used;
    bUsed := vliB^.used;
    IF vliA^.digits[aUsed-1] < vliB^.digits[bUsed-1] THEN
        top := bUsed;
    ELSE
        top := bUsed-1;
    END;
    Grow(remainder, aUsed); (* Yes, aUsed*)
    remainder^.used := top+1;
    (*remainder^.digits[0..top] := vliA^.digits[aUsed-top-1..aUsed-1];*)
    MoveDigits(ADR(remainder^.digits),
               ADR(vliA^.digits[aUsed-top-1]),
               top+1);

    firstTime := TRUE;
    <*/PUSH/NOWARN:U*>
    indexA := VAL(ADRCARD, vliA^.used)-top;
    <*/POP*>
    REPEAT
        DEC(indexA);

        IF NOT firstTime THEN
            ShiftLeftDigitAndSet(remainder, vliA^.digits[indexA]);
        END;
        firstTime := FALSE;

        IF CompareAbs(remainder, vliB) >= Equal THEN
            GetEstimate(remainder, vliB, lower, upper);
            LOOP
                IF upper - lower <= 1 THEN
                    times := upper;
                ELSE
                    times := lower + ((upper-lower) / 2);
                END;
                SetZeroI(vliT1);
                MultiplyByDigit(vliB, times, vliT1);

                CASE CompareAbs(vliT1, remainder) OF
                Less:
                    IF times < MAX(Digit) THEN
                        lower := times + 1;
                    ELSE
                        upper := lower-1; (* get out of this loop *)
                    END;
                    IF lower > upper THEN
                        DoSubtract3(remainder, vliT1);
                        EXIT;
                    END;
                |
                Greater:
                    upper := times-1;
                    DEC(times);
                    IF lower > upper THEN
                        DoSubtract3(vliT1, vliB);
                        DoSubtract3(remainder, vliT1);
                        EXIT;
                    END;
                |
                Equal:
                    SetZero(remainder);
                    EXIT;
                END;
            END;
            ShiftLeftDigitAndSet(result, times);
        END;
    UNTIL indexA = 0;

    Dispose(vliT1);
END DoDivide;

PROCEDURE DivideByDigit(vliA : VLI;
                        digit : Digit;
                        VAR INOUT result : VLI;
                        VAR INOUT remainder : VLI);
VAR
    temp        : Digit2;
    numTimes    : Digit;
    rem         : Digit;
    index       : ADRINT;
    ptrA,
    ptrR        : POINTER TO Digit;
BEGIN
    ASSERT(IsZero(result));
    ASSERT(IsZero(remainder));
    ASSERT(digit <> 0);
    ASSERT(result <> vliA);
    ASSERT(remainder <> vliA);

    index := vliA^.used-1;
    remainder^.used := 1;
    IF index = 0 THEN
        result^.digits[0] := vliA^.digits[0] / digit;
        result^.used := 1;
        remainder^.digits[0] := vliA^.digits[0] REM digit;
        RETURN;
    END;

    Grow(result, vliA^.used-1);

    IF digit > vliA^.digits[index] THEN
        result^.used := index;
    ELSE
        result^.used := index+1;
    END;

    temp := 0;
    rem := 0;
    ptrA := ADR(vliA^.digits[index]);
    ptrR := ADR(result^.digits[index]);
    FOR index := index TO 0 BY -1 DO
        temp := (VAL(Digit2, rem) SHL DigitBits) BOR VAL(Digit2, ptrA^);
        numTimes := temp / VAL(Digit2, digit);
        ptrR^ := numTimes;
        rem := temp - (VAL(Digit2, numTimes) * VAL(Digit2, digit));
        ptrA := SUBADR(ptrA, SIZE(Digit));
        ptrR := SUBADR(ptrR, SIZE(Digit));
    END;
    remainder^.digits[0] := rem;
END DivideByDigit;

<*/GROUPLIBPROCS:N*>(*will be grouped with above procs*)
PROCEDURE Divide(vliA, vliB : VLI;
                 VAR INOUT result : VLI;
                 VAR INOUT remainder : VLI);
VAR
    neg         : BOOLEAN;
BEGIN
    SetZeroI(result);
    SetZeroI(remainder);

    IF NOT IsZeroI(vliA) THEN
        CASE CompareAbs(vliA, vliB) OF
        Greater:
            IF vliB^.used > 1 THEN
                DoDivide(vliA, vliB, result, remainder);
            ELSE
                DivideByDigit(vliA, vliB^.digits[0], result, remainder);
            END;
            neg := FALSE;
            IF vliA^.neg THEN
                SetNegI(remainder, TRUE);

                neg := NOT vliB^.neg;
            ELSIF vliB^.neg THEN
                neg := TRUE;
            END;
            SetNegI(result, neg);
        |
        Less:
            Assign(vliA, remainder);
        |
        Equal:
            SetDigit(result, 0, 1);
        END;
    END;
END Divide;

PROCEDURE Div(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    temp        : VLI;
BEGIN
    temp := CreateI();
    Divide(vliA, vliB, result, temp);
    Dispose(temp);
END Div;

PROCEDURE Rem(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    temp        : VLI;
BEGIN
    temp := CreateI();
    Divide(vliA, vliB, temp, result);
    Dispose(temp);
END Rem;

PROCEDURE Inc(VAR INOUT vli : VLI; increment : Digit);
VAR
    index       : ADRCARD;
    digit       : Digit;
    carry       : Digit;
    temp        : Digit2;
BEGIN
    temp := VAL(Digit2, vli^.digits[0]) + VAL(Digit2, increment);
    <*/PUSH/NOCHECK:A*>
    vli^.digits[0] := temp;
    <*/POP*>
    carry := temp SHR DigitBits;

    index := 1;
    WHILE carry <> 0 DO
        <*/PUSH/NOWARN:U*>
        IF index < VAL(ADRCARD, vli^.used) THEN
        <*/POP*>
            digit := vli^.digits[index];
        ELSE
            Grow(vli, index);
            digit := 0;
        END;

        temp := VAL(Digit2, digit) + 1;
        <*/PUSH/NOCHECK:A*>
        digit := temp;
        <*/POP*>
        carry := temp SHR DigitBits;
        vli^.digits[index] := digit;

        INC(index);
    END;
END Inc;

PROCEDURE Dec(VAR INOUT vli : VLI; decrement : Digit);
VAR
    index       : ADRCARD;
    digit       : Digit;
    borrow      : Digit;
BEGIN
    digit := vli^.digits[0];
    IF (vli^.used > 1) OR (digit > decrement) THEN
        <*/PUSH/NOCHECK:O*>
        vli^.digits[0] := digit - decrement;
        <*/POP*>
        borrow := ORD(digit < decrement);

        index := 1;
        WHILE borrow <> 0 DO
            digit := vli^.digits[index];

            <*/PUSH/NOCHECK:O*>
            vli^.digits[index] := digit - 1;
            <*/POP*>
            borrow := ORD(digit = 0);

            INC(index);
        END;

        (* trim leading zeros. leave at least one digit. *)
        WHILE (vli^.used > 1) AND (vli^.digits[vli^.used-1] = 0) DO
            DEC(vli^.used);
        END;
    ELSE
        vli^.digits[0] := decrement - digit;
        SetNegI(vli, TRUE);
    END;
END Dec;

PROCEDURE AddInt(vliA : VLI; B : DigitS; VAR INOUT result : VLI);
BEGIN
    IF vliA <> result THEN
        Assign(vliA, result);
    END;
    IF B > 0 THEN
        Inc(result, B);
    ELSIF B < 0 THEN
        Dec(result, -B);
    END;
END AddInt;

PROCEDURE SubInt(vliA : VLI; B : DigitS; VAR INOUT result : VLI);
BEGIN
    IF vliA <> result THEN
        Assign(vliA, result);
    END;
    IF B > 0 THEN
        Dec(result, B);
    ELSIF B < 0 THEN
        Inc(result, -B);
    END;
END SubInt;

PROCEDURE MulInt(vliA : VLI; B : DigitS; VAR INOUT result : VLI);
VAR
    neg         : BOOLEAN;
BEGIN
    neg := vliA^.neg;
    IF B < 0 THEN
        B := -B;
        neg := NOT neg;
    END;

    SetZero(result);

    MultiplyByDigit(vliA, B, result);

    IF neg THEN
        SetNegI(result, TRUE);
    END;
END MulInt;

PROCEDURE DivideInt(vliA : VLI;
                    B : DigitS;
                    VAR INOUT result : VLI;
                    VAR INOUT remainder : VLI);
VAR
    neg         : BOOLEAN;
BEGIN
    neg := vliA^.neg;
    IF B < 0 THEN
        B := -B;
        neg := NOT neg;
    END;

    SetZero(result);
    SetZero(remainder);

    DivideByDigit(vliA, B, result, remainder);

    IF neg THEN
        SetNegI(result, TRUE);
    END;
    IF vliA^.neg THEN
        SetNegI(remainder, TRUE);
    END;
END DivideInt;

PROCEDURE DivideInt2(vliA : VLI;
                     B : DigitS;
                     VAR INOUT result : VLI;
                     VAR OUT remainder : DigitS);
VAR
    vliRem      : VLI;
BEGIN
    vliRem := CreateI();

    DivideInt(vliA, B, result, vliRem);
    remainder := vliRem^.digits[0];
    IF vliRem^.neg THEN
        remainder := -remainder;
    END;

    Dispose(vliRem);
END DivideInt2;

PROCEDURE DivInt(vliA : VLI; B : DigitS; VAR INOUT result : VLI);
VAR
    vliRem      : VLI;
BEGIN
    vliRem := CreateI();

    DivideInt(vliA, B, result, vliRem);

    Dispose(vliRem);
END DivInt;

PROCEDURE RemInt(vliA : VLI; B : DigitS; VAR INOUT result : VLI);
VAR
    vliRes      : VLI;
BEGIN
    vliRes := CreateI();

    DivideInt(vliA, B, vliRes, result);

    Dispose(vliRes);
END RemInt;

PROCEDURE RemInt2(vliA : VLI; B : DigitS) : DigitS;
VAR
    vliDiv      : VLI;
    vliRem      : VLI;
    result      : DigitS;
BEGIN
    vliDiv := CreateI();
    vliRem := CreateI();

    DivideInt(vliA, B, vliDiv, vliRem);
    result := vliRem^.digits[0];
    IF vliRem^.neg THEN
        result := -result;
    END;

    Dispose(vliRem);
    Dispose(vliDiv);

    RETURN result;
END RemInt2;

(* returns GCD (greatest common divisor) of two numbers,
   and when gcd = 1, inverse = d^-1 mod f.
   NB: uses extended Euclids' algorithm, see pp. 261
*)
PROCEDURE GcdWithInverse(vliD, vliF : VLI;
                         VAR INOUT vliGCD : VLI;
                         VAR INOUT vliInverse : VLI);
VAR
    vliX1, vliX2, vliX3 : VLI;
    vliY1, vliY2, vliY3 : VLI;
    vliT1, vliT2, vliT3 : VLI;
    vliQ, vliT          : VLI;
    t                   : VLI;

    PROCEDURE swap(VAR INOUT a, b, t : VLI) [INLINE];
    BEGIN
        t := a;
        a := b;
        b := t;
    END swap;

BEGIN
    vliX1 := CreateI();
    vliX2 := CreateI();
    vliX3 := CreateI();
    vliY1 := CreateI();
    vliY2 := CreateI();
    vliY3 := CreateI();
    vliT1 := CreateI();
    vliT2 := CreateI();
    vliT3 := CreateI();
    vliQ := CreateI();
    vliT := CreateI();

    Assign(vliF, vliX3);
    Assign(vliD, vliY3);
    vliX1^.digits[0] := 1;
    vliY2^.digits[0] := 1;

    WHILE vliY3^.neg OR (CompareAbsDig(vliY3, 1) = Greater) DO
        Divide(vliX3, vliY3, vliQ, vliT);
        Multiply(vliQ, vliY1, vliT);
        Subtract(vliX1, vliT, vliT1);
        Multiply(vliQ, vliY2, vliT);
        Subtract(vliX2, vliT, vliT2);
        Multiply(vliQ, vliY3, vliT);
        Subtract(vliX3, vliT, vliT3);

        swap(vliY1, vliX1, t);
        swap(vliY2, vliX2, t);
        swap(vliY3, vliX3, t);
        swap(vliT1, vliY1, t);
        swap(vliT2, vliY2, t);
        swap(vliT3, vliY3, t);
        (*
        Assign(vliY1, vliX1);
        Assign(vliY2, vliX2);
        Assign(vliY3, vliX3);
        Assign(vliT1, vliY1);
        Assign(vliT2, vliY2);
        Assign(vliT3, vliY3);
        *)
    END;

    IF IsZero(vliY3) THEN
        Assign(vliX3, vliGCD);
    ELSE (* vliY3=1 *)
        Assign(vliY3, vliGCD);
        Assign(vliY2, vliInverse);
    END;

    Dispose(vliT);
    Dispose(vliQ);
    Dispose(vliT3);
    Dispose(vliT2);
    Dispose(vliT1);
    Dispose(vliY3);
    Dispose(vliY2);
    Dispose(vliY1);
    Dispose(vliX3);
    Dispose(vliX2);
    Dispose(vliX1);
END GcdWithInverse;

PROCEDURE GCD(vliA, vliB : VLI; VAR INOUT result : VLI);
VAR
    dummy       : VLI;
BEGIN
    dummy := CreateI();
    GcdWithInverse(vliA, vliB, result, dummy);
    Dispose(dummy);
END GCD;

PROCEDURE GCDeq1(vliA, vliB : VLI) : BOOLEAN;
VAR
    vliGCD,
    inv         : VLI;
    valid       : BOOLEAN;
BEGIN
    vliGCD := CreateI();
    inv := CreateI();

    GcdWithInverse(vliA, vliB, vliGCD, inv);
    valid := CompareDigit(vliGCD, 1) = Equal;

    Dispose(inv);
    Dispose(vliGCD);
    RETURN valid;
END GCDeq1;

PROCEDURE LCM(vliA, vliB : VLI; VAR INOUT result : VLI);
(* lcm = a / GCD(a,b) * b *)
VAR
    t           : VLI;
    gcd         : VLI;
BEGIN
    t := CreateI();
    gcd := CreateI();

    GcdWithInverse(vliA, vliB, gcd, t);
    Divide(vliA, gcd, t, result);
    Multiply(vliB, t, result);

    Dispose(gcd);
    Dispose(t);
END LCM;

PROCEDURE ModularInverse(vliA, vliN : VLI; VAR INOUT result : VLI) : BOOLEAN;
VAR
    vliGCD      : VLI;
    valid       : BOOLEAN;
BEGIN
    vliGCD := CreateI();

    GcdWithInverse(vliA, vliN, vliGCD, result);
    valid := CompareAbsDig(vliGCD, 1) = Equal;
    IF valid AND result^.neg THEN
        Add(result, vliN, vliGCD);
        Assign(vliGCD, result);
    END;

    Dispose(vliGCD);
    RETURN valid;
END ModularInverse;

PROCEDURE AdjustToModulus(VAR INOUT vli : VLI;
                          vliN : VLI;
                          VAR INOUT tQ, tR : VLI);
VAR
    created     : BOOLEAN;
BEGIN
    IF vli^.neg OR (CompareAbs(vli, vliN) >= Equal) THEN
        created := FALSE;
        IF tQ = NIL THEN
            created := TRUE;
            tQ := CreateI();
            tR := CreateI();
        END;

        Divide(vli, vliN, tQ, tR);
        IF tR^.neg THEN
            Add(tR, vliN, vli);
        ELSE
            Assign(tR, vli);
        END;

        IF created THEN
            Dispose(tQ);
            Dispose(tR);
        END;
    END;
END AdjustToModulus;

PROCEDURE AddMod(vliA, vliB, vliN : VLI; VAR INOUT result : VLI);
VAR
    tQ, tR      : VLI;
BEGIN
    ASSERT(NOT vliN^.neg);

    tQ := NIL;
    tR := NIL;
    Add(vliA, vliB, result);
    AdjustToModulus(result, vliN, tQ, tR);
END AddMod;

PROCEDURE SubMod(vliA, vliB, vliN : VLI; VAR INOUT result : VLI);
VAR
    tQ, tR      : VLI;
BEGIN
    ASSERT(NOT vliN^.neg);

    tQ := NIL;
    tR := NIL;
    Subtract(vliA, vliB, result);
    AdjustToModulus(result, vliN, tQ, tR);
END SubMod;

TYPE
    TempMR =
        RECORD
        vliAB,
        (*vliA1, vliB1,*)
        dummy           : VLI;
        END;

PROCEDURE CreateTempMR(digits : CARDINAL; VAR OUT T : TempMR);
BEGIN
    T.vliAB := Allocate(digits*2);
    T.dummy := Allocate(digits);
    (*
    T.vliA1 := Allocate(digits);
    T.vliB1 := Allocate(digits);
    *)
END CreateTempMR;

PROCEDURE DisposeTempMR(VAR INOUT T : TempMR);
BEGIN
    Dispose(T.vliAB);
    Dispose(T.dummy);
    (*
    Dispose(T.vliA1);
    Dispose(T.vliB1);
    *)
END DisposeTempMR;

PROCEDURE MultiplyRem(vliA, vliB, vliN : VLI;
                      VAR INOUT result : VLI;
                      VAR INOUT T : TempMR);
BEGIN
    Multiply(vliA, vliB, T.vliAB);
    Divide(T.vliAB, vliN, T.dummy, result);
    (*
    Divide(vliA, vliN, T.dummy, T.vliA1);
    Divide(vliB, vliN, T.dummy, T.vliB1);
    Multiply(T.vliA1, T.vliB1, T.vliAB);
    Divide(T.vliAB, vliN, T.dummy, result);
    *)
END MultiplyRem;

PROCEDURE MultiplyMod(vliA, vliB, vliN : VLI; VAR INOUT result : VLI);
VAR
    T   : TempMR;
BEGIN
    ASSERT(NOT vliN^.neg);

    CreateTempMR(vliA^.used, T);

    MultiplyRem(vliA, vliB, vliN, result, T);

    IF result^.neg THEN
        Add(result, vliN, T.dummy);
        Assign(T.dummy, result);
    END;

    DisposeTempMR(T);
END MultiplyMod;

PROCEDURE DivideMod(vliA, vliB, vliN : VLI; VAR INOUT result : VLI);
VAR
    tQ, tR      : VLI;
BEGIN
    ASSERT(NOT vliN^.neg);

    tQ := CreateI();
    tR := CreateI();

    Divide(vliA, vliB, result, tR);
    AdjustToModulus(result, vliN, tQ, tR);

    Dispose(tR);
    Dispose(tQ);
END DivideMod;

<*/GROUPLIBPROCS:Y*>
PROCEDURE ShiftLeftBits(VAR INOUT vli : VLI; bits : CARDINAL);
VAR
    res         : VLI;
    digits      : ADRCARD;
    extra       : CARDINAL;
    i, j        : ADRCARD;
    digit1,
    digit2      : Digit;
BEGIN
    digits := bits / DigitBits;
    bits := bits REM DigitBits;
    <*/PUSH/NOWARN:U*>
    extra := digits + VAL(ADRCARD, ORD(bits <> 0));
    <*/POP*>

    Grow(vli, vli^.used+extra-1);
    res := vli;

    IF digits <> 0 THEN
        i := res^.used;
        REPEAT
            DEC(i);
            res^.digits[i+digits] := res^.digits[i];
        UNTIL i = 0;
        ZapDigits(ADR(res^.digits), digits);
    END;

    res^.used := res^.used + extra;

    IF bits <> 0 THEN
        res^.digits[res^.used-1] := 0;

        IF res^.used > 1 THEN
            j := res^.used;
            REPEAT
                DEC(j);

                digit1 := res^.digits[j];
                digit2 := res^.digits[j-1];
                res^.digits[j] := (digit1 SHL bits) BOR (digit2 SHR (DigitBits-bits));
            UNTIL j = 1;
        END;

        res^.digits[0] := res^.digits[0] SHL bits;

        (* trim leading zeros. leave at least one digit. *)
        IF (res^.used > 1) AND (res^.digits[res^.used-1] = 0) THEN
            DEC(res^.used);
        END;
    END;
END ShiftLeftBits;

PROCEDURE ShiftRightBits(vli : VLI; bits : CARDINAL);
VAR
    digits      : CARDINAL;
    i, j        : ADRCARD;
    digit1,
    digit2      : Digit;
BEGIN
    digits := bits / DigitBits;
    bits := bits REM DigitBits;

    IF digits >= vli^.used THEN
        vli^.used := 1;
        vli^.digits[0] := 0;
        vli^.neg := FALSE;
        RETURN;
    END;

    IF digits <> 0 THEN
        i := vli^.used-digits;
        IF i <= 8 THEN
            j := digits;
            i := 0;
            REPEAT
                vli^.digits[i] := vli^.digits[j];
                INC(j);
                INC(i);
            <*/PUSH/NOWARN:U*>
            UNTIL j = VAL(ADRCARD, vli^.used);
            <*/POP*>
        ELSE
            MoveDigits(ADR(vli^.digits[0]), ADR(vli^.digits[digits]), i);
        END;
    END;

    vli^.used := vli^.used - digits;

    IF bits <> 0 THEN
        IF vli^.used > 1 THEN
            i := 0;
            REPEAT
                digit1 := vli^.digits[i];
                digit2 := vli^.digits[i+1];
                vli^.digits[i] := (digit1 SHR bits) BOR (digit2 SHL (DigitBits-bits));
                INC(i);
            <*/PUSH/NOWARN:U*>
            UNTIL i+1 = VAL(ADRCARD, vli^.used);
            <*/POP*>
        END;

        vli^.digits[vli^.used-1] := vli^.digits[vli^.used-1] SHR bits;

        (* trim leading zeros. leave at least one digit. *)
        IF (vli^.used > 1) AND (vli^.digits[vli^.used-1] = 0) THEN
            DEC(vli^.used);
        END;
    END;
END ShiftRightBits;

TYPE
    Montgomery =
    RECORD
    n           : VLI;(*not unique, just a copy of the original pointer*)
    rConv       : VLI;
    t           : VLI;
    n0Prime     : Digit;
    rBits       : CARDINAL;
    END;

PROCEDURE CreateMontgomery(vliN : VLI; VAR OUT monty : Montgomery);
VAR
    digits      : CARDINAL;
    t2, t3      : VLI;
BEGIN
    digits := vliN^.used;

    monty.rConv := Allocate(digits*2);
    monty.t := Allocate(digits*2);
    monty.n := vliN;
    monty.rBits := GetBitSizeI(vliN);(*optimizes shifts. no bit shifts.*)

    t2 := Allocate(digits*2);
    t3 := Allocate(digits*2);

    monty.t^.digits[0] := 1;
    ShiftLeftBits(monty.t, DigitBits);

    t2^.digits[0] := monty.n^.digits[0];
    IF NOT ModularInverse(monty.t, t2, monty.t) THEN
        vliN := NIL;
        vliN^.neg := TRUE;(*crash. should never happen. what else to do?*)
    END;
    ShiftLeftBits(monty.t, DigitBits);
    Dec(monty.t, 1);
    Divide(monty.t, t2, t3, monty.rConv);(* n'=(r*r'-1)/N *)
    monty.n0Prime := t3^.digits[0];

    (* rConv is used for conversions to prime space. *)
    monty.t^.used := 1;
    monty.t^.digits[0] := 1;
    ShiftLeftBits(monty.t, monty.rBits*2);
    Divide(monty.t, monty.n, t2, monty.rConv);
    Dispose(t2);
    Dispose(t3);
END CreateMontgomery;

PROCEDURE DisposeMontgomery(VAR INOUT monty : Montgomery);
BEGIN
    Dispose(monty.t);
    Dispose(monty.rConv);
END DisposeMontgomery;

PROCEDURE MontgomeryReduce(vli : VLI;
                           VAR INOUT result : VLI;
                           VAR INOUT monty : Montgomery);
(* REDC
  m = (x mod r) * n' mod r
  t = (x + m*n) / r
  return t or (t - n) if (t >= n)
*)
VAR
    max         : ADRCARD;
    i, N        : ADRCARD;
    res         : VLI;
    carry       : CARDINAL;
BEGIN
    ASSERT(vli <> result);

    Assign(vli, result);

    (* add one extra digit for extra overflow? because of the add.
       is it possible for the add to make the multiply overflow
       the last digit from the multiply? that digit normally only has
       overflow from the multiply.
    *)
    max := monty.n^.used + vli^.used(* + 1*);

    Grow(result, max-1);
    res := result;

    (* zero the digits of the add operand (x) *)
    <*/PUSH/NOWARN:U*>
    ZapDigits(ADR(res^.digits[res^.used]), max-VAL(ADRCARD, res^.used));
    <*/POP*>
    res^.used := max;

    (* this is one trick loop.
       m = x * n' is computed on the fly during the loop.
       t = x + m*n is also computed by this loop.
       x is preloaded into the result. that is how it gets added in.
       n' is the special multi-precision montgomery n' value

       computes t = x + (x * n') * n

       does not compute the same value as a normal reduction, but the
       value is still a proper multiple of 'r', so the modulus will still be the same.
    *)
    N := monty.n^.used;
    i := 0;
    REPEAT
        carry := MultiplyDigitAdd(ADR(monty.n^.digits),
                                  (res^.digits[i] * monty.n0Prime) BAND MAX(Digit),
                                  ADR(res^.digits[i]),
                                  monty.n^.used,
                                  FALSE);
        IF carry <> 0 THEN
            carry := IncDigits(ADR(res^.digits[i+N]), carry, max-i);
        END;

        INC(i);
    UNTIL i = N;

    (* trim leading zeros. leave at least one digit. *)
    WHILE (res^.used > 1) AND (res^.digits[res^.used-1] = 0) DO
        DEC(res^.used);
    END;

    (* t := t / r *)
    ShiftRightBits(res, monty.rBits);
    (* return t or (t - n) if (t >= n) *)
    IF CompareAbs(res, monty.n) >= Equal THEN
        DoSubtract3(res, monty.n);
    END;
END MontgomeryReduce;

PROCEDURE MultiplyModMontgomery(vliA, vliB : VLI; VAR INOUT result : VLI;
                                VAR INOUT monty : Montgomery);
BEGIN
    Multiply(vliA, vliB, monty.t);
    MontgomeryReduce(monty.t, result, monty);
END MultiplyModMontgomery;

PROCEDURE ToMontgomery(vli : VLI;
                       VAR INOUT result : VLI;
                       VAR INOUT monty : Montgomery);
BEGIN
    MultiplyModMontgomery(vli, monty.rConv, result, monty);
END ToMontgomery;

PROCEDURE FromMontgomery(vli : VLI;
                         VAR INOUT result : VLI;
                         VAR INOUT monty : Montgomery);
BEGIN
    (* using temp allows vli and result to be the same variable *)
    MontgomeryReduce(vli, monty.t, monty);
    Assign(monty.t, result);
END FromMontgomery;

CONST
    MaxPowerWindowSize  = 7;

PROCEDURE GetPowerWindowSize(vli : VLI) : CARDINAL;
VAR
    bits        : CARDINAL;
    window      : CARDINAL;
BEGIN
    bits := GetBitSizeI(vli);
    REPEAT
        DEC(bits);
    UNTIL IsBitSet(vli, bits);
    INC(bits);

    IF bits <= 32 THEN
        window := 1;(* likely F0..F4*)
    ELSIF bits <= 128 THEN
        window := 3;
    ELSIF bits <= 256 THEN
        window := 4;
    ELSIF bits <= 512 THEN
        window := 5;
    ELSIF bits <= 1024 THEN
        window := 6;
    ELSE
        window := MaxPowerWindowSize;
    END;
    RETURN window;
END GetPowerWindowSize;

(*
PROCEDURE PowerModBasic(vliA, vliB, vliN : VLI; VAR INOUT result : VLI);
VAR
    i, bit      : INTEGER;
    window      : INTEGER;
    wb          : INTEGER;
    didOne      : BOOLEAN;
    T           : TempMR;
    aPower      : ARRAY [1..MaxPowerWindowSize] OF VLI;
BEGIN
    CreateTempMR(vliN^.used, T);

    window := GetPowerWindowSize(vliB);

    (* build the power table *)

    aPower[1] := Allocate(vliN^.used);
    Divide(vliA, vliN, T.dummy, aPower[1]);

    FOR i := 2 TO window DO
        aPower[i] := Allocate(vliN^.used);
        MultiplyRem(aPower[i-1], aPower[i-1], vliN, aPower[i], T);
        MultiplyRem(aPower[i], aPower[1], vliN, aPower[i], T);
    END;

    SetZero(result);
    result^.digits[0] := 1;

    didOne := FALSE;
    bit := GetBitSizeI(vliB);
    DEC(bit);
    REPEAT
        (* skip zero bits *)

        WHILE (bit >= 0) AND NOT IsBitSet(vliB, bit) DO
            IF didOne THEN
                MultiplyRem(result, result, vliN, result, T);
            END;
            DEC(bit);
        END;

        IF bit >= 0 THEN
            (* we are at a 1 bit, find how many consecutive ones,
               within our window limit.
            *)

            DEC(bit);
            wb := 1;
            WHILE (bit >= 0) AND (wb < window) AND IsBitSet(vliB, bit) DO
                DEC(bit);
                INC(wb);
            END;

            IF didOne THEN
                FOR i := 1 TO wb DO
                    MultiplyRem(result, result, vliN, result, T);
                END;
            END;

            MultiplyRem(result, aPower[wb], vliN, result, T);

            didOne := TRUE;
        END;
    UNTIL bit < 0;

    FOR i := 1 TO window DO
        Dispose(aPower[i]);
    END;
    DisposeTempMR(T);
END PowerModBasic;
*)


PROCEDURE PowerModMontgomery(vliA, vliB : VLI;
                             VAR INOUT result : VLI;
                             VAR INOUT monty : Montgomery);
TYPE
    windowRec =
        RECORD
        exp             : VLI;
        skip            : CARDINAL;
        expBits         : CARDINAL;
        windowStart     : CARDINAL;
        windowVal       : CARDINAL;
        windowSize      : CARDINAL;
        mask            : Digit;
        done            : BOOLEAN;
        END;
VAR
    i           : ADRCARD;
    bit         : CARDINAL;
    windowSize  : CARDINAL;
    powersSize  : ADRCARD;
    window      : windowRec;
    powers      : ARRAY [0..(1 SHL (MaxPowerWindowSize-1))-1] OF VLI;

    PROCEDURE nextWindow(VAR INOUT w : windowRec);
    BEGIN
        LOOP
            IF w.skip < w.expBits THEN
                IF NOT IsBitSet(w.exp, w.skip) THEN
                    INC(w.skip);
                ELSE
                    EXIT;
                END;
            ELSE
                w.done := TRUE;
                RETURN;
            END;
        END;

        ShiftRightBits(w.exp, w.skip);
        w.windowStart := w.windowStart + w.skip;
        w.windowVal := (w.exp^.digits[0] BAND w.mask) / 2;
        w.expBits := w.expBits - w.skip;

        w.skip := w.windowSize;
    END nextWindow;

BEGIN
    windowSize := GetPowerWindowSize(vliB);

    powersSize := ORD(1) SHL (windowSize-1);
    FOR i := 0 TO powersSize-1 DO
        powers[i] := Allocate(monty.n^.used);
    END;

    (* using powers allows vliA to be the same paramter as result *)
    Divide(vliA, monty.n, monty.t, powers[0]);
    ToMontgomery(powers[0], result, monty);

    powers[0]^.used := 1;
    powers[0]^.digits[0] := 1;
    ToMontgomery(powers[0], powers[0], monty);
    FOR i := 1 TO powersSize-1 DO
        Assign(powers[0], powers[i]);
    END;

    window.exp := Copy(vliB);
    window.expBits := GetBitSizeI(window.exp);
    window.skip := 0;
    window.windowStart := 0;
    window.windowSize := windowSize;
    window.mask := (ORD(1) SHL windowSize) - 1;
    nextWindow(window);

    bit := 0;
    window.done := FALSE;
    REPEAT
        IF (NOT window.done) AND (bit = window.windowStart) THEN
            MultiplyModMontgomery(powers[window.windowVal],
                                  result,
                                  powers[window.windowVal],
                                  monty);

            nextWindow(window);
        END;
        IF NOT window.done THEN
            MultiplyModMontgomery(result, result, result, monty);
            INC(bit);
        END;
    UNTIL window.done;
(*
TYPE
    Montgomery =
    RECORD
    n           : VLI;(*not unique, just a copy of the original pointer*)
    rConv       : VLI;
    t           : VLI;
    n0Prime     : Digit;
    rBits       : CARDINAL;
    END;
*)

    Dispose(window.exp);

    Assign(powers[powersSize-1], result);
    IF powersSize > 1 THEN
        FOR i := powersSize-2 TO 1 BY -1 DO
            MultiplyModMontgomery(powers[i], powers[i+1], powers[i], monty);
            MultiplyModMontgomery(result, powers[i], result, monty);
        END;
        MultiplyModMontgomery(powers[0], powers[1], powers[0], monty);
        MultiplyModMontgomery(result, result, result, monty);
        MultiplyModMontgomery(result, powers[0], result, monty);
    END;

    FromMontgomery(result, result, monty);

    FOR i := 0 TO powersSize-1 DO
        Dispose(powers[i]);
    END;
END PowerModMontgomery;

PROCEDURE PowerModBasic(vliA, vliB, vliN : VLI; VAR INOUT result : VLI);
TYPE
    windowRec =
        RECORD
        exp             : VLI;
        skip            : CARDINAL;
        expBits         : CARDINAL;
        windowStart     : CARDINAL;
        windowVal       : CARDINAL;
        windowSize      : CARDINAL;
        mask            : Digit;
        done            : BOOLEAN;
        END;
VAR
    i           : ADRCARD;
    bit           : CARDINAL;
    windowSize  : CARDINAL;
    powersSize  : ADRCARD;
    window      : windowRec;
    T           : TempMR;
    powers      : ARRAY [0..(1 SHL (MaxPowerWindowSize-1))-1] OF VLI;

    PROCEDURE nextWindow(VAR INOUT w : windowRec);
    BEGIN
        LOOP
            IF w.skip < w.expBits THEN
                IF NOT IsBitSet(w.exp, w.skip) THEN
                    INC(w.skip);
                ELSE
                    EXIT;
                END;
            ELSE
                w.done := TRUE;
                RETURN;
            END;
        END;

        ShiftRightBits(w.exp, w.skip);
        w.windowStart := w.windowStart + w.skip;
        w.windowVal := (w.exp^.digits[0] BAND w.mask) / 2;
        w.expBits := w.expBits - w.skip;

        w.skip := w.windowSize;
    END nextWindow;

BEGIN
    CreateTempMR(vliN^.used, T);

    windowSize := GetPowerWindowSize(vliB);

    powersSize := ORD(1) SHL (windowSize-1);
    FOR i := 0 TO powersSize-1 DO
        powers[i] := Allocate(vliN^.used);
        powers[i]^.digits[0] := 1;
    END;

    Divide(vliA, vliN, T.dummy, T.vliAB);
    Assign(T.vliAB, result);(* allows vliA to be the same paramter as result *)

    window.exp := Copy(vliB);
    window.expBits := GetBitSizeI(window.exp);
    window.skip := 0;
    window.windowStart := 0;
    window.windowSize := windowSize;
    window.mask := (ORD(1) SHL windowSize) - 1;
    nextWindow(window);

    bit := 0;
    window.done := FALSE;
    REPEAT
        IF (NOT window.done) AND (bit = window.windowStart) THEN
            MultiplyRem(powers[window.windowVal],
                        result,
                        vliN,
                        powers[window.windowVal],
                        T);

            nextWindow(window);
        END;
        IF NOT window.done THEN
            MultiplyRem(result, result, vliN, result, T);
            INC(bit);
        END;
    UNTIL window.done;

    Dispose(window.exp);

    Assign(powers[powersSize-1], result);
    IF powersSize > 1 THEN
        FOR i := powersSize-2 TO 1 BY -1 DO
            MultiplyRem(powers[i], powers[i+1], vliN, powers[i], T);
            MultiplyRem(result, powers[i], vliN, result, T);
        END;
        MultiplyRem(powers[0], powers[1], vliN, powers[0], T);
        MultiplyRem(result, result, vliN, result, T);
        MultiplyRem(result, powers[0], vliN, result, T);
    END;

    FOR i := 0 TO powersSize-1 DO
        Dispose(powers[i]);
    END;
    DisposeTempMR(T);
END PowerModBasic;


<*/GROUPLIBPROCS:N*>(*will be grouped with above procs*)
PROCEDURE PowerMod(vliA, vliB, vliN : VLI; VAR INOUT result : VLI);
VAR
    monty       : Montgomery;
BEGIN
    ASSERT(NOT vliN^.neg);
    ASSERT(NOT vliB^.neg);
    ASSERT(NOT vliA^.neg);
    ASSERT(vliB <> result);
    ASSERT(vliN <> result);

    IF (NOT IsOddI(vliN)) OR (GetBitSizeI(vliB) <= 64) THEN
        PowerModBasic(vliA, vliB, vliN, result);
    ELSE
        CreateMontgomery(vliN, monty);
        PowerModMontgomery(vliA, vliB, result, monty);
        DisposeMontgomery(monty);
    END;
END PowerMod;

TYPE
    PowerModExRec =
        RECORD
        heap            : HeapInfoPointer;
        vliN            : VLI;
        monty           : Montgomery;
        oddModulus      : BOOLEAN;
        END;

    PowerModExInfo      = POINTER TO PowerModExRec;

PROCEDURE PowerModEx(vliA, vliB : VLI;
                     exInfo : PowerModExInfo;
                     VAR INOUT result : VLI);
BEGIN
    ASSERT(NOT vliB^.neg);
    ASSERT(NOT vliA^.neg);
    ASSERT(vliB <> result);

    IF exInfo^.oddModulus THEN
        PowerModMontgomery(vliA, vliB, result, exInfo^.monty);
    ELSE
        PowerModBasic(vliA, vliB, exInfo^.vliN, result);
    END;
END PowerModEx;

PROCEDURE CreatePowerModExInfo(vliN : VLI) : PowerModExInfo;
VAR
    info        : PowerModExInfo;
BEGIN
    ASSERT(NOT vliN^.neg);

    IF NOT vliN^.neg THEN
        NEW(info);
        info^.heap := GetHeap();
        info^.oddModulus := IsOdd(vliN);
        info^.vliN := Copy(vliN);
        IF info^.oddModulus THEN
            CreateMontgomery(info^.vliN, info^.monty);
        END;
        RETURN info;
    END;
    RETURN NIL;
END CreatePowerModExInfo;

PROCEDURE DisposePowerModExInfo(VAR INOUT info : PowerModExInfo);
BEGIN
    IF info <> NIL THEN
        Dispose(info^.vliN);
        DeallocateEx(info, SIZE(info^), info^.heap);
    END;
END DisposePowerModExInfo;

(*-------------- random number generation --------------------*)

VAR
    RandomH     : RandomHandle;

PROCEDURE SetRandomSeed(seed : CARDINAL);
BEGIN
    IF RandomH = NIL THEN
        RandomH := RandomizeEx(seed);
    ELSE
        SeedEx(RandomH, seed);
    END;
END SetRandomSeed;

PROCEDURE GetRandom(bits : CARDINAL; VAR INOUT result : VLI);
VAR
    index               : ADRCARD;
    size                : ADRCARD;
    mask                : Digit;
BEGIN
    IF RandomH = NIL THEN
        RandomH := RandomizeEx(0);
    END;

    SetZero(result);

    size := bits / DigitBits;
    IF (bits REM DigitBits) <> 0 THEN
        INC(size);
    END;

    index := 0;
    WHILE index < size DO
        SetDigit(result, index, RndEx(RandomH, 0));
        INC(index);
    END;

    IF (bits REM DigitBits) <> 0 THEN
        mask := (bits REM DigitBits) - 1;
        mask := VAL(Digit, 1) SHL mask;
        mask := (mask SHL 1) - 1;
        result^.digits[index-1] := result^.digits[index-1] BAND mask;
    END;
END GetRandom;

PROCEDURE GetRandomSmaller(vliA : VLI; VAR INOUT result : VLI);
VAR
    index       : ADRCARD;
BEGIN
    IF RandomH = NIL THEN
        RandomH := RandomizeEx(0);
    END;

    SetZero(result);

    index := vliA^.used-1;
    IF vliA^.digits[index] <> 0 THEN
        (* cannot pass a zero to RndEx since we want a smaller random number *)
        SetDigit(result, index, RndEx(RandomH, vliA^.digits[index]));
    END;
    WHILE index <> 0 DO
        DEC(index);
        SetDigit(result, index, RndEx(RandomH, 0));
    END;
END GetRandomSmaller;

(*-------------- prime number generation --------------------*)

CONST
    MaxSmallPrimes      = 4000-1;(*2000 min,
                                   6500 max for a 16-bit result*)
    PrimeSearchRange    = 10000;

TYPE
    SmallPrimesTable    = ARRAY [0..MaxSmallPrimes] OF CARDINAL16;
    SmallPrimesTablePtr = POINTER TO SmallPrimesTable;
VAR
    SmallPrimes         : SmallPrimesTablePtr;
    SmallPrimesHeap     : HeapInfoPointer;

TYPE
    TempIP2 =
        RECORD
        (* witness *)
        vliX            : VLI;
        vliD            : VLI;
        vliNm1          : VLI;

        vliTest         : VLI;

        (* multiple *)
        vliRem          : VLI;
        vliDummy        : VLI;

        (* MultiplyRem *)
        tMR             : TempMR;

        (* small prime fast sieve loop *)
        smallPrimeTop   : CARDINAL;
        pm1RelativeTo   : Digit;

        (* IsPrime2 *)
        witnessTests    : CARDINAL;

        (* small prime fast sieve loop *)
        smallPrimeRems  : SmallPrimesTable;
        END;

PROCEDURE BuildPrimeTable;

    <*/PUSH/INLINE:N*>
    PROCEDURE build;
    CONST
        max         = MAX(CARDINAL16);
    TYPE
        flagSet     = PACKEDSET OF [0..max];
    VAR
        flags       : flagSet;
        i, j, k     : ADRCARD;
        prime       : ADRCARD;
    BEGIN
        flags := flagSet{0..max};

        j := 0;
        FOR i := 3 TO max BY 2 DO
            IF i IN flags THEN
                prime := i;
                k := prime + prime;
                WHILE k <= max DO
                    EXCL(flags, k);
                    k := k + prime;
                END;

                SmallPrimes^[j] := i;
                INC(j);
                IF j > MaxSmallPrimes THEN
                    RETURN;
                END;
            END;
        END;
    END build;
    <*/POP*>

BEGIN
    IF SmallPrimes = NIL THEN
        NEW(SmallPrimes);
        SmallPrimesHeap := GetHeap();

        (* looks funny but saves stack allocation unless we actually do a build.
           yea, this is excessive about minimizing memory use...
           but what the heck.
        *)
        build;
    END;
END BuildPrimeTable;

PROCEDURE CreateTempIP2(bits, tests : CARDINAL;
                        pm1RelativeTo : Digit;
                        VAR OUT T : TempIP2);
VAR
    digits      : CARDINAL;
BEGIN
    digits := (bits+(DigitBits-1)) / DigitBits;

    T.vliX := Allocate(digits);
    T.vliD := Allocate(digits);
    T.vliNm1 := Allocate(digits);

    T.vliTest := Allocate(digits);

    T.vliRem := Allocate(digits);
    T.vliDummy := Allocate(digits);

    CreateTempMR(digits, T.tMR);

    IF bits >= 1024 THEN
        T.smallPrimeTop := MaxSmallPrimes;
    ELSIF bits >= 512 THEN
        T.smallPrimeTop := 1999;
    ELSIF bits >= 256 THEN
        T.smallPrimeTop := 999;
    ELSE
        T.smallPrimeTop := 499;
    END;

    T.pm1RelativeTo := pm1RelativeTo;

    IF tests = 0 THEN
        (* taken from rsa-oaep_spec.pdf found at www.rsasecurity.com
           error probability quoted as 2^-100.
        *)
        IF bits >= 1024 THEN
            T.witnessTests := 4;
        ELSIF bits >= 768 THEN
            T.witnessTests := 5;
        ELSIF bits >= 683 THEN(*multi-prime 2048/3*)
            T.witnessTests := 6;
        ELSIF bits >= 512 THEN
            T.witnessTests := 8;
        ELSIF bits >= 410 THEN(*multi-prime 2048/5*)
            T.witnessTests := 10;
        ELSIF bits >= 384 THEN
            T.witnessTests := 11;
        ELSIF bits >= 342 THEN(*multi-prime 1024/3*)
            T.witnessTests := 12;
        ELSE
            T.witnessTests := 17;
        END;
    ELSE
        T.witnessTests := tests;
    END;
END CreateTempIP2;

PROCEDURE DisposeTempIP2(VAR INOUT T : TempIP2);
BEGIN
    DisposeTempMR(T.tMR);

    Dispose(T.vliTest);

    Dispose(T.vliRem);
    Dispose(T.vliDummy);

    Dispose(T.vliNm1);
    Dispose(T.vliD);
    Dispose(T.vliX);
END DisposeTempIP2;

PROCEDURE IsPrime2(vli : VLI; VAR INOUT T : TempIP2) : BOOLEAN;
VAR
    bits        : CARDINAL;
    count       : CARDINAL;
    prime       : BOOLEAN;

    <*/PUSH/INLINE:N*>
    (* returns TRUE when vliN is not prime *)
    (* NB: according to Miller en Rabin, see page 258 *)
    PROCEDURE witness(bit : CARDINAL;
                      vliA, vliN : VLI;
                      VAR INOUT T : TempIP2) : BOOLEAN;
    VAR
        noPrime         : BOOLEAN;
    BEGIN
        SetZero(T.vliX);
        SetZero(T.vliD);
        SetZero(T.vliNm1);

        T.vliD^.digits[0] := 1;
        SubInt(vliN, 1, T.vliNm1);
        noPrime := FALSE;
        REPEAT
            DEC(bit);
            Assign(T.vliD, T.vliX);

            MultiplyRem(T.vliX, T.vliX, vliN, T.vliD, T.tMR);

            IF (CompareAbsDig(T.vliD, 1) = Equal) AND
               (CompareAbsDig(T.vliX, 1) <> Equal) AND
               (CompareAbs(T.vliX, T.vliNm1) <> Equal)
            THEN
                noPrime := TRUE;
            ELSIF IsBitSet(T.vliNm1, bit) THEN
                MultiplyRem(T.vliD, vliA, vliN, T.vliD, T.tMR);
            END;
        UNTIL noPrime OR (bit = 0);

        IF (NOT noPrime) AND (CompareAbsDig(T.vliD, 1) <> Equal) THEN
            noPrime := TRUE;
        END;

        RETURN noPrime;
    END witness;
    <*/POP*>

BEGIN
    count := 0;
    bits := GetBitSizeI(vli);
    REPEAT
        INC(count);
        GetRandomSmaller(vli, T.vliTest);
        prime := NOT witness(bits, T.vliTest, vli, T);
    UNTIL (NOT prime) OR (count >= T.witnessTests);

    RETURN prime;
END IsPrime2;

PROCEDURE LoadSmallPrimeRems(vli : VLI;
                             VAR INOUT T : TempIP2;
                             quickOut : BOOLEAN) : BOOLEAN;
VAR
    i   : ADRCARD;
BEGIN
    <*/PUSH/NOWARN:U*>
    FOR i := 0 TO VAL(ADRCARD, T.smallPrimeTop) DO
    <*/POP*>
        DivideInt(vli, SmallPrimes^[i], T.vliDummy, T.vliRem);
        T.smallPrimeRems[i] := T.vliRem^.digits[0];
        IF quickOut AND (T.smallPrimeRems[i] = 0) THEN
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END LoadSmallPrimeRems;

PROCEDURE IsPrime(vli : VLI; tests : CARDINAL) : BOOLEAN;
VAR
    T           : TempIP2;
    ok          : BOOLEAN;
BEGIN
    BuildPrimeTable;

    CreateTempIP2(GetBitSizeI(vli), tests, 0, T);

    ok := FALSE;
    IF LoadSmallPrimeRems(vli, T, TRUE) THEN
        ok := IsPrime2(vli, T);
    END;

    DisposeTempIP2(T);

    RETURN ok;
END IsPrime;

PROCEDURE CheckSmallPrimes(count : CARDINAL; VAR INOUT T : TempIP2) : BOOLEAN;
VAR
    i       : ADRCARD;
    base    : CARDINAL;
BEGIN
    count := count * 2;

    <*/PUSH/NOWARN:U*>
    FOR i := 0 TO VAL(ADRCARD, T.smallPrimeTop) DO
    <*/POP*>
        base := SmallPrimes^[i];
        IF ((ORD(T.smallPrimeRems[i])+count) REM base) = 0 THEN
            RETURN FALSE;
        END;
    END;

    RETURN TRUE;
END CheckSmallPrimes;

PROCEDURE CheckPm1RelativeTo(vli : VLI; VAR INOUT T : TempIP2) : BOOLEAN;
BEGIN
    IF T.pm1RelativeTo <> 0 THEN
        SubInt(vli, 1, T.vliNm1);
        DivideInt(T.vliNm1, T.pm1RelativeTo, T.vliDummy, T.vliRem);
        RETURN T.vliRem^.digits[0] <> 0;
    END;

    RETURN TRUE;
END CheckPm1RelativeTo;

PROCEDURE GetPrime(bits, tests : CARDINAL;
                   pm1RelativeTo : Digit;
                   VAR INOUT result : VLI);
VAR
    count               : CARDINAL;
    last                : ADRCARD;
    mask                : Digit;
    highBit             : Digit;
    T                   : TempIP2;
BEGIN
    BuildPrimeTable;

    CreateTempIP2(bits, tests, pm1RelativeTo, T);

    IF (bits REM DigitBits) = 0 THEN
        highBit := VAL(Digit, 1) SHL (DigitBits-1);
        mask := MAX(Digit);
    ELSE
        highBit := (bits REM DigitBits) - 1;
        highBit := VAL(Digit, 1) SHL highBit;
        mask := (highBit SHL 1) - 1;
    END;

    LOOP
        GetRandom(bits, result);
        last := result^.used-1;
        result^.digits[last] := (result^.digits[last] BAND mask) BOR highBit;
        result^.digits[0] := result^.digits[0] BOR 1;

        FUNC LoadSmallPrimeRems(result, T, FALSE);

        count := 0;
        REPEAT
            IF (NOT CheckSmallPrimes(count, T)) OR
               (NOT CheckPm1RelativeTo(result, T)) OR
               (NOT IsPrime2(result, T))
            THEN
                INC(count);
                Inc(result, 2);
            ELSE
                DisposeTempIP2(T);
                RETURN;
            END;
        UNTIL count > PrimeSearchRange;
    END;
END GetPrime;

PROCEDURE GetPrimeSmaller(vliA : VLI; tests : CARDINAL; VAR INOUT result : VLI);
VAR
    count               : CARDINAL;
    T                   : TempIP2;
BEGIN
    BuildPrimeTable;

    CreateTempIP2(GetBitSizeI(vliA), tests, 0, T);

    LOOP
        GetRandomSmaller(vliA, result);
        result^.digits[0] := result^.digits[0] BOR 1;

        FUNC LoadSmallPrimeRems(result, T, FALSE);

        count := 0;
        LOOP
            IF (NOT CheckSmallPrimes(count, T)) OR (NOT IsPrime2(result, T)) THEN
                IF count < PrimeSearchRange THEN
                    INC(count);
                    Inc(result, 2);
                ELSE
                    EXIT;
                END;
            ELSE
                IF CompareAbs(result, vliA) = Less THEN
                    DisposeTempIP2(T);
                    RETURN;
                ELSE
                    EXIT;
                END;
            END;
        END;
    END;
END GetPrimeSmaller;

PROCEDURE GetNextPrime(vliA : VLI; tests : CARDINAL; VAR INOUT result : VLI);
VAR
    count               : CARDINAL;
    T                   : TempIP2;
BEGIN
    BuildPrimeTable;

    CreateTempIP2(GetBitSizeI(vliA), tests, 0, T);

    AddInt(vliA, 1, result);
    result^.digits[0] := result^.digits[0] BOR 1;

    count := 0;
    FUNC LoadSmallPrimeRems(result, T, FALSE);
    LOOP
        IF (NOT CheckSmallPrimes(count, T)) OR (NOT IsPrime2(result, T)) THEN
            INC(count);
            Inc(result, 2);
        ELSE
            DisposeTempIP2(T);
            RETURN;
        END;
    END;
END GetNextPrime;

(*-------------- string functions --------------------*)

CONST
    StrDigits : ARRAY [0..15] OF ACHAR = {"0123456789ABCDEF"};

PROCEDURE ConvertHex(num : CARDINAL; VAR OUT str : ARRAY OF CHAR);
BEGIN
    str[1] := StrDigits[num REM 16];
    num := num / 16;
    str[0] := StrDigits[num REM 16];
END ConvertHex;

PROCEDURE ToHexString(vli : VLI; VAR OUT text : ARRAY OF CHAR);
VAR
    str         : ARRAY [0..1] OF CHAR;
    index       : CARDINAL;
    dig         : Digit;
    i           : ADRCARD;
    highText    : ADRCARD;
    j           : INTEGER;
BEGIN
    i := 0;
    IF vli^.neg THEN
        text := "-";
        INC(i);
    ELSE
        text := "";
    END;

    index := vli^.used;
    REPEAT
        DEC(index);

        dig := vli^.digits[index];
        highText := HIGH(text);
        FOR j := SIZE(Digit)-1 TO 0 BY -1 DO
            ConvertHex((dig SHR (j*8)) BAND 0FFh, str);
            IF i+1 <= highText THEN
                text[i] := str[0];
                text[i+1] := str[1];
                i := i + 2;
            ELSIF i <= highText THEN
                text[i] := str[0];
                INC(i);
            END;
        END;
    UNTIL index = 0;

    IF i <= highText THEN
        text[i] := '';
    END;
END ToHexString;

PROCEDURE FromHexString(text : ARRAY OF CHAR; VAR INOUT vli : VLI) : BOOLEAN;
VAR
    index       : CARDINAL;
    i, s        : ADRINT;
    j, l        : CARDINAL;
    dig         : Digit;
    shift       : CARDINAL;
    nibbles     : CARDINAL;
    t           : Digit;
    ch          : CHAR;
    neg         : BOOLEAN;
BEGIN
    s := 0;
    l := LENGTH(text);
    <*/PUSH/NOWARN:U*>
    WHILE (s < VAL(ADRINT, INT(l))) AND (text[s] = ' ') DO
    <*/POP*>
        INC(s);
    END;

    index := 0;
    SetZero(vli);

    neg := FALSE;
    IF text[s] = '-' THEN
        neg := TRUE;
        INC(s);
    ELSIF text[s] = '+' THEN
        INC(s);
    END;

    dig := 0;
    j := 0;
    shift := 0;
    nibbles := 0;
    i := l-1;
    WHILE i >= s DO
        ch := CAP(text[i]);
        DEC(i);
        IF (ch >= '0') AND (ch <= '9') THEN
            t := ORD(ch) - ORD('0');
        ELSIF (ch >= 'A') AND (ch <= 'F') THEN
            t := 10 + (ORD(ch)-ORD('A'));
        ELSE
            SetZero(vli);
            RETURN FALSE;
        END;

        IF t < 16 THEN
            INC(nibbles);
            IF nibbles = 2 THEN
                nibbles := 0;
                IF (i >= s) AND (text[i] = ' ') THEN
                    DEC(i);
                END;
            END;
            INC(j);

            t := t SHL shift;
            shift := shift + 4;
            dig := dig BOR t;

            IF j = 2*SIZE(Digit) THEN
                j := 0;
                shift := 0;
                SetDigit(vli, index, dig);
                INC(index);
                dig := 0;
            END;
        END;
    END;

    IF j <> 0 THEN
        SetDigit(vli, index, dig);
    END;

    SetNegI(vli, neg);

    RETURN TRUE;
END FromHexString;

PROCEDURE ToDecimalString(vli : VLI; VAR OUT str : ARRAY OF CHAR) : BOOLEAN;
VAR
    index               : ADRCARD;
    highStr             : ADRCARD;
    remainder           : DigitS;
    result              : VLI;
    num                 : VLI;
    numToReverse        : ADRCARD;
    i                   : ADRCARD;
    hadError            : BOOLEAN;
    ch                  : CHAR;
BEGIN
    IF HIGH(str) = 0 THEN
        str[0] := '';
        RETURN FALSE;
    END;
    num := Create();
    result := Create();

    str[0] := "0";
    str[1] := '';
    hadError := FALSE;
    index := 0;
    Assign(vli, num);
    num^.neg := FALSE;
    highStr := HIGH(str);
    WHILE NOT IsZero(num) DO
        DivideInt2(num, 10, result, remainder);
        Assign(result, num);
        IF index > highStr THEN
            hadError := TRUE;
            BREAK;
        END;
        str[index] := StrDigits[remainder];
        INC(index);
    END;
    IF vli^.neg THEN
        IF index <= highStr THEN
            str[index] := "-";
            INC(index);
        ELSE
            hadError := TRUE;
        END;
    END;
    IF index <= highStr THEN
        str[index] := '';
        DEC(index);
    END;
    (* index now contans the position of the last "real" character in the string *)
    numToReverse := (index+1) / 2;
    FOR i := 0 TO numToReverse DO
        ch := str[i];
        str[i] := str[index-i];
        str[index-i] := ch;
    END;

    Dispose(result);
    Dispose(num);

    RETURN NOT hadError;
END ToDecimalString;

PROCEDURE FromDecimalString(str : ARRAY OF CHAR; VAR INOUT vli : VLI) : BOOLEAN;
VAR
    i, l                : ADRCARD;
    t                   : Digit;
    num                 : VLI;
    ch                  : CHAR;
    hadError            : BOOLEAN;
    neg                 : BOOLEAN;
BEGIN
    num := Create();
    hadError := TRUE;
    SetZero(vli);
    l := LENGTH(str);
    IF l > 0 THEN
        i := 0;
        WHILE (i < l) AND (str[i] = ' ') DO
            INC(i);
        END;

        neg := FALSE;
        IF i < l THEN
            IF str[i] = '-' THEN
                neg := TRUE;
                INC(i);
            ELSIF str[i] = '+' THEN
                INC(i);
            END;
        END;

        FOR i := i TO l-1 DO
            MulInt(vli, 10, num);
            ch := str[i];
            IF (ch >= '0') AND (ch <= '9') THEN
                t := ORD(ch) - ORD('0');
                AddInt(num, t, vli);
                hadError := FALSE;
            ELSE
                SetZero(vli);
                hadError := TRUE;
                BREAK;
            END;
        END;

        SetNegI(vli, neg);
    END;
    Dispose(num);
    RETURN NOT hadError;
END FromDecimalString;

PROCEDURE GetDigitCount(vli : VLI) : CARDINAL;
BEGIN
    RETURN vli^.used;
END GetDigitCount;

PROCEDURE GetByteCount(vli : VLI) : CARDINAL;
VAR
    bytes1      : CARDINAL;
    dig         : Digit;
    i           : CARDINAL;
BEGIN
    bytes1 := (vli^.used-1) * SIZE(Digit);

    dig := vli^.digits[vli^.used-1];
    i := SIZE(Digit)-1;
    WHILE ((dig SHR (i*8)) BAND 0FFh) = 0 DO
        DEC(i);
    END;

    RETURN bytes1 + i + 1;
END GetByteCount;

PROCEDURE GetBitCount(vli : VLI) : CARDINAL;
VAR
    bits        : CARDINAL;
BEGIN
    bits := GetBitSizeI(vli);
    REPEAT
        DEC(bits);
    UNTIL IsBitSet(vli, bits);
    RETURN bits+1;
END GetBitCount;

PROCEDURE GetDigits(vli : VLI;
                    digitsA : ADDRESS;(*ARRAY OF Digit*)
                    VAR INOUT amount : CARDINAL;
                    VAR OUT negative : BOOLEAN) : BOOLEAN;
VAR
    i           : ADRCARD;
    digits      : POINTER TO ARRAY [0..0] OF Digit;
BEGIN
    IF vli^.used <= amount THEN
        digits := digitsA;
        amount := vli^.used;
        negative := vli^.neg;
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, vli^.used)-1 DO
        <*/POP*>
            digits^[i] := vli^.digits[i];
        END;
        RETURN TRUE;
    END;
    RETURN FALSE;
END GetDigits;

PROCEDURE SetDigits(VAR INOUT vli : VLI;
                    digitsI : ADDRESS(*ARRAY OF Digit*);
                    count : CARDINAL;
                    negative : BOOLEAN);
VAR
    i           : ADRCARD;
    digits      : POINTER TO ARRAY [0..0] OF Digit;
BEGIN
    SetZero(vli);
    IF count > 0 THEN
        digits := digitsI;
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, count)-1 DO
        <*/POP*>
            SetDigit(vli, i, digits^[i]);
        END;
        SetNegI(vli, negative);
    END;
END SetDigits;

PROCEDURE GetBytesLSB(vli : VLI;
                      bytesI : ADDRESS;
                      VAR INOUT amount : CARDINAL;
                      VAR OUT negative : BOOLEAN) : BOOLEAN;
VAR
    i, k        : ADRCARD;
    j           : CARDINAL;
    dig         : Digit;
    max         : ADRCARD;
    bytes       : POINTER TO ARRAY [0..0] OF BYTE;
BEGIN
    negative := vli^.neg;

    max := GetByteCount(vli);
    <*/PUSH/NOWARN:U*>
    IF max <= VAL(ADRCARD, amount) THEN
    <*/POP*>
        bytes := bytesI;
        i := 0;
        FOR j := 0 TO vli^.used-1 DO
            dig := vli^.digits[j];
            FOR k := 0 TO SIZE(Digit)-1 DO
                IF i < max THEN
                    bytes^[i] := VAL(CARDINAL8, (dig SHR (k*8)) BAND 0FFh);
                    INC(i);
                END;
            END;
        END;
        amount := i;
        RETURN TRUE;
    END;
    RETURN FALSE;
END GetBytesLSB;

PROCEDURE SetBytesLSB(VAR INOUT vli : VLI;
                      bytesI : ADDRESS;
                      count : CARDINAL;
                      negative : BOOLEAN);
VAR
    i           : ADRCARD;
    j, k        : CARDINAL;
    dig         : Digit;
    bytes       : POINTER TO ARRAY [0..0] OF BYTE;
BEGIN
    SetZero(vli);

    IF count > 0 THEN
        bytes := bytesI;
        i := 0;
        j := 0;
        REPEAT
            dig := 0;
            FOR k := 0 TO SIZE(Digit)-1 DO
                <*/PUSH/NOWARN:U*>
                IF i < VAL(ADRCARD, count) THEN
                <*/POP*>
                    dig := dig BOR (VAL(Digit, bytes^[i]) SHL (k*8));
                    INC(i);
                END;
            END;

            SetDigit(vli, j, dig);
            INC(j);
        <*/PUSH/NOWARN:U*>
        UNTIL i = VAL(ADRCARD, count);
        <*/POP*>

        SetNegI(vli, negative);
    END;
END SetBytesLSB;

PROCEDURE GetBytesMSB(vli : VLI;
                      bytesI : ADDRESS;
                      VAR INOUT amount : CARDINAL;
                      VAR OUT negative : BOOLEAN) : BOOLEAN;
VAR
    i, j        : ADRCARD;
    k           : INTEGER;
    dig         : Digit;
    db          : CARDINAL8;
    max         : CARDINAL;
    bytes       : POINTER TO ARRAY [0..0] OF BYTE;
BEGIN
    negative := vli^.neg;

    max := GetByteCount(vli);
    IF max <= amount THEN
        bytes := bytesI;
        i := 0;
        j := vli^.used;
        REPEAT
            DEC(j);
            dig := vli^.digits[j];

            FOR k := SIZE(Digit)-1 TO 0 BY -1 DO
                db := (dig SHR (k*8)) BAND 0FFh;
                IF (i > 0) OR (db <> 0) THEN
                    bytes^[i] := db;
                    INC(i);
                END;
            END;
        UNTIL j = 0;
        amount := i;
        RETURN TRUE;
    END;
    RETURN FALSE;
END GetBytesMSB;

PROCEDURE SetBytesMSB(VAR INOUT vli : VLI;
                      bytesI : ADDRESS;
                      count : CARDINAL;
                      negative : BOOLEAN);
VAR
    j, k        : CARDINAL;
    i           : ADRINT;
    dig         : Digit;
    bytes       : POINTER TO ARRAY [0..0] OF BYTE;
BEGIN
    SetZero(vli);

    IF count > 0 THEN
        bytes := bytesI;
        i := count-1;
        j := 0;
        REPEAT
            dig := 0;
            FOR k := 0 TO SIZE(Digit)-1 DO
                IF i >= 0 THEN
                    dig := dig BOR (VAL(Digit, bytes^[i]) SHL (k*8));
                    DEC(i);
                END;
            END;

            SetDigit(vli, j, dig);
            INC(j);
        UNTIL i < 0;

        SetNegI(vli, negative);
    END;
END SetBytesMSB;

BEGIN
    SmallPrimes := NIL;
    SmallPrimesHeap := NIL;
    RandomH := NIL;
FINALLY
    IF SmallPrimes <> NIL THEN
        DeallocateEx(SmallPrimes, SIZE(SmallPrimesTable), SmallPrimesHeap);
    END;
    IF RandomH <> NIL THEN
        DisposeRandomHandle(RandomH);
    END;
END VLI.
