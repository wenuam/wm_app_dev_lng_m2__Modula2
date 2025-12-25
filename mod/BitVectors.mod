(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE BitVectors;
<*/OPT:T*>

(* make sure this version tag is set the same in the DEF and MOD *)
<*/VALIDVERSION:PROTECT*>
%IF Windows %OR UNIX %THEN
<*/VERSION:PROTECT*>
%END

FROM SYSTEM IMPORT
    ADR, ADDADR, ADRCARD, SHIFT;

%IF PROTECT %THEN
FROM SYSTEM IMPORT
    ATOMIC_CMPXCHG, MEMORY_FENCE, CPUCOUNT;
FROM Threads IMPORT
    YieldThread;
%ELSE
FROM SYSTEM IMPORT
    UNREFERENCED_PARAMETER;
%END

FROM ExStorage IMPORT
    ALLOCATE, DeallocateEx, GetHeap, PushHeap, PopHeap;

CONST
    ElementSize         = SIZE(BitVectorElement);

TYPE
    ElementPointer      = POINTER TO ARRAY [0..3] OF BitVectorElement;

PROCEDURE NewVector(VAR OUT v : BitVector; lowBit, highBit : CARDINAL);
VAR
    numElements : CARDINAL;
BEGIN
    v := NIL;
    IF highBit >= lowBit THEN
        numElements := (highBit-lowBit+1) / BitVectorBits;
        IF ((highBit-lowBit+1) REM BitVectorBits) <> 0 THEN
            INC(numElements);
        END;

        ALLOCATE(v, (SIZE(v^) - SIZE(v^.bits)) + (ElementSize*numElements));
        IF v <> NIL THEN
            v^.heap := GetHeap();
            v^.lowBit := lowBit;
            v^.highBit := highBit;
            v^.numElements := numElements;
            %IF PROTECT %THEN
            v^.lock := 0;
            %END
            Zap(EmptyElement, v);
        END;
    END;
END NewVector;

PROCEDURE EnterLock(v : BitVector) [INLINE];
BEGIN
%IF PROTECT %THEN
    LOOP
        IF v^.lock = 0 THEN
            IF ATOMIC_CMPXCHG(v^.lock, 0, 1) = 0 THEN
                EXIT;
            END;
        END;
        IF CPUCOUNT = 1 THEN
            YieldThread;
        END;
    END;
    MEMORY_FENCE;
%ELSE
    UNREFERENCED_PARAMETER(v);
%END
END EnterLock;

PROCEDURE LeaveLock(v : BitVector) [INLINE];
BEGIN
%IF PROTECT %THEN
    MEMORY_FENCE;
    v^.lock := 0;
%ELSE
    UNREFERENCED_PARAMETER(v);
%END
END LeaveLock;

PROCEDURE EnterLocks(v1, v2 : BitVector) [INLINE];
BEGIN
%IF PROTECT %THEN
    LOOP
        IF v1^.lock = 0 THEN
            IF ATOMIC_CMPXCHG(v1^.lock, 0, 1) = 0 THEN
                EXIT;
            END;
        END;
        IF CPUCOUNT = 1 THEN
            YieldThread;
        END;
    END;
    LOOP
        IF v2^.lock = 0 THEN
            IF ATOMIC_CMPXCHG(v2^.lock, 0, 1) = 0 THEN
                EXIT;
            END;
        END;
        IF CPUCOUNT = 1 THEN
            YieldThread;
        END;
    END;
    MEMORY_FENCE;
%ELSE
    UNREFERENCED_PARAMETER(v1);
    UNREFERENCED_PARAMETER(v2);
%END
END EnterLocks;

PROCEDURE LeaveLocks(v1, v2 : BitVector) [INLINE];
BEGIN
%IF PROTECT %THEN
    MEMORY_FENCE;
    v2^.lock := 0;
    v1^.lock := 0;
%ELSE
    UNREFERENCED_PARAMETER(v1);
    UNREFERENCED_PARAMETER(v2);
%END
END LeaveLocks;

PROCEDURE DisposeVector(VAR INOUT v : BitVector);
BEGIN
    IF v <> NIL THEN
        EnterLock(v);
        DeallocateEx(v,
                     (SIZE(v^) - SIZE(v^.bits)) + (ElementSize * v^.numElements),
                     v^.heap);
    END;
END DisposeVector;

PROCEDURE ReallocVector(VAR INOUT v : BitVector; newHighBit : CARDINAL);
VAR
    newV        : BitVector;
BEGIN
    (* keep the new vector in the same heap as the current vector *)
    PushHeap(v^.heap);
    NewVector(newV, v^.lowBit, newHighBit);
    PopHeap;
    IF newV <> NIL THEN
        Copy(v, newV);
        DisposeVector(v);
        v := newV;
    END;
END ReallocVector;

PROCEDURE SetBit(b : CARDINAL; v : BitVector);
BEGIN
    EnterLock(v);

    IF (b >= v^.lowBit) AND (b <= v^.highBit) THEN
        b := b - v^.lowBit;
        INCL(v^.bits[b / BitVectorBits], b REM BitVectorBits);
    END;

    LeaveLock(v);
END SetBit;

PROCEDURE ClearBit(b : CARDINAL; v : BitVector);
BEGIN
    EnterLock(v);

    IF (b >= v^.lowBit) AND (b <= v^.highBit) THEN
        b := b - v^.lowBit;
        EXCL(v^.bits[b / BitVectorBits], b REM BitVectorBits);
    END;

    LeaveLock(v);
END ClearBit;

PROCEDURE SetBits(bottom, top : CARDINAL; v : BitVector);
VAR
    b   : CARDINAL;
BEGIN
    EnterLock(v);

    IF bottom < v^.lowBit THEN
        bottom := v^.lowBit;
    END;
    IF top > v^.highBit THEN
        top := v^.highBit;
    END;

    FOR b := (bottom-v^.lowBit) TO (top-v^.lowBit) DO
        INCL(v^.bits[b / BitVectorBits], b REM BitVectorBits);
    END;

    LeaveLock(v);
END SetBits;

PROCEDURE ClearBits(bottom, top : CARDINAL; v : BitVector);
VAR
    b   : CARDINAL;
BEGIN
    EnterLock(v);

    IF bottom < v^.lowBit THEN
        bottom := v^.lowBit;
    END;
    IF top > v^.highBit THEN
        top := v^.highBit;
    END;

    FOR b := (bottom-v^.lowBit) TO (top-v^.lowBit) DO
        EXCL(v^.bits[b / BitVectorBits], b REM BitVectorBits);
    END;

    LeaveLock(v);
END ClearBits;

PROCEDURE BitIsSet(b : CARDINAL; v : BitVector) : BOOLEAN;
VAR
    yes : BOOLEAN;
BEGIN
    EnterLock(v);

    IF (b >= v^.lowBit) AND (b <= v^.highBit) THEN
        b := b - v^.lowBit;
        yes := (b REM BitVectorBits) IN v^.bits[b / BitVectorBits];

        LeaveLock(v);
        RETURN yes;
    END;

    LeaveLock(v);
    RETURN FALSE;
END BitIsSet;

PROCEDURE AnyBitSet(bottom, top : CARDINAL; v : BitVector) : BOOLEAN;
VAR
    b   : CARDINAL;
BEGIN
    EnterLock(v);

    IF bottom < v^.lowBit THEN
        bottom := v^.lowBit;
    END;
    IF top > v^.highBit THEN
        top := v^.highBit;
    END;

    FOR b := (bottom-v^.lowBit) TO (top-v^.lowBit) DO
        IF (b REM BitVectorBits) IN v^.bits[b / BitVectorBits] THEN
            LeaveLock(v);
            RETURN TRUE;
        END;
    END;

    LeaveLock(v);
    RETURN FALSE;
END AnyBitSet;

PROCEDURE AnyBitClear(bottom, top : CARDINAL; v : BitVector) : BOOLEAN;
VAR
    b   : CARDINAL;
BEGIN
    EnterLock(v);

    IF bottom < v^.lowBit THEN
        LeaveLock(v);
        RETURN TRUE;
        (*bottom := v^.lowBit;*)
    END;
    IF top > v^.highBit THEN
        LeaveLock(v);
        RETURN TRUE;
        (*top := v^.highBit;*)
    END;

    FOR b := (bottom-v^.lowBit) TO (top-v^.lowBit) DO
        IF NOT ((b REM BitVectorBits) IN v^.bits[b / BitVectorBits]) THEN
            LeaveLock(v);
            RETURN TRUE;
        END;
    END;

    LeaveLock(v);
    RETURN FALSE;
END AnyBitClear;

PROCEDURE FindSetBits(startBit, numBits : CARDINAL;
                      v : BitVector;
                      VAR OUT bottom : CARDINAL) : BOOLEAN;
VAR
    element,
    mask,
    bitsMask    : BitVectorElement;
    w,
    bit,
    found,
    count       : CARDINAL;
BEGIN
    EnterLock(v);

    IF numBits < BitVectorBits THEN
        bitsMask := EmptyElement;
        count := numBits;
        REPEAT
            DEC(count);
            INCL(bitsMask, count);
        UNTIL count = 0;
    ELSE
        bitsMask := FullElement;
    END;

    startBit := startBit - v^.lowBit;
    w := startBit / BitVectorBits;
    bit := startBit REM BitVectorBits;
    WHILE w < v^.numElements DO
        element := v^.bits[w];
        IF element <> EmptyElement THEN
            LOOP
                IF bit < BitVectorBits THEN
                    mask := SHIFT(bitsMask, bit);
                    IF (mask * element) <> mask THEN
                        INC(bit);
                    ELSE
                        found := (w*BitVectorBits) + bit + v^.lowBit;
                        count := BitVectorBits - bit;

                        IF count >= numBits THEN
                            bottom := found;
                            LeaveLock(v);
                            RETURN TRUE;
                        ELSE
                            INC(w);
                            count := numBits - count;
                            WHILE (count >= BitVectorBits) AND
                                  (w < v^.numElements) AND
                                  (v^.bits[w] = FullElement)
                            DO
                                count := count - BitVectorBits;
                                INC(w);
                            END;
                            IF count = 0 THEN
                                bottom := found;
                                LeaveLock(v);
                                RETURN TRUE;
                            ELSIF w < v^.numElements THEN
                                bit := count;
                                element := EmptyElement;
                                IF w < v^.numElements THEN
                                    element := v^.bits[w];
                                END;

                                mask := EmptyElement;
                                WHILE count <> 0 DO
                                    DEC(count);
                                    INCL(mask, count);
                                END;

                                IF (mask * element) = mask THEN
                                    bottom := found;
                                    LeaveLock(v);
                                    RETURN TRUE;
                                END;
                            ELSE
                                EXIT;
                            END
                        END;
                    END;
                ELSE
                    EXIT;
                END;
            END;
        END;

        INC(w);
        bit := 0;
    END;

    LeaveLock(v);
    RETURN FALSE;
END FindSetBits;

PROCEDURE FindClearBits(startBit, numBits : CARDINAL;
                        v : BitVector;
                        VAR OUT bottom : CARDINAL) : BOOLEAN;
VAR
    element,
    mask,
    bitsMask    : BitVectorElement;
    w,
    bit,
    found,
    count       : CARDINAL;
BEGIN
    EnterLock(v);

    IF numBits < BitVectorBits THEN
        bitsMask := EmptyElement;
        count := numBits;
        REPEAT
            DEC(count);
            INCL(bitsMask, count);
        UNTIL count = 0;
    ELSE
        bitsMask := FullElement;
    END;

    startBit := startBit - v^.lowBit;
    w := startBit / BitVectorBits;
    bit := startBit REM BitVectorBits;
    WHILE w < v^.numElements DO
        element := v^.bits[w];
        IF element <> FullElement THEN
            LOOP
                IF bit < BitVectorBits THEN
                    mask := SHIFT(bitsMask, bit);
                    IF (mask * element) <> EmptyElement THEN
                        INC(bit);
                    ELSE
                        found := (w*BitVectorBits) + bit + v^.lowBit;
                        count := BitVectorBits - bit;

                        IF count >= numBits THEN
                            bottom := found;
                            LeaveLock(v);
                            RETURN TRUE;
                        ELSE
                            INC(w);
                            count := numBits - count;
                            WHILE (count >= BitVectorBits) AND
                                  (w < v^.numElements) AND
                                  (v^.bits[w] = EmptyElement)
                            DO
                                count := count - BitVectorBits;
                                INC(w);
                            END;
                            IF count = 0 THEN
                                bottom := found;
                                LeaveLock(v);
                                RETURN TRUE;
                            ELSIF w < v^.numElements THEN
                                bit := count;
                                element := EmptyElement;
                                IF w < v^.numElements THEN
                                    element := v^.bits[w];
                                END;

                                mask := EmptyElement;
                                WHILE count <> 0 DO
                                    DEC(count);
                                    INCL(mask, count);
                                END;

                                IF (mask * element) = EmptyElement THEN
                                    bottom := found;
                                    LeaveLock(v);
                                    RETURN TRUE;
                                END;
                            ELSE
                                EXIT;
                            END
                        END;
                    END;
                ELSE
                    EXIT;
                END;
            END;
        END;

        INC(w);
        bit := 0;
    END;

    LeaveLock(v);
    RETURN FALSE;
END FindClearBits;

PROCEDURE FindSetBitsEx(startBit, minBits : CARDINAL;
                        v : BitVector;
                        VAR OUT result : ARRAY OF BitRange) : CARDINAL;
VAR
    element,
    mask,
    bitsMask    : BitVectorElement;
    w,
    bit,
    bottom,
    top,
    count,
    index       : CARDINAL;
BEGIN
    EnterLock(v);

    IF minBits < BitVectorBits THEN
        bitsMask := EmptyElement;
        index := minBits;
        REPEAT
            DEC(index);
            INCL(bitsMask, index);
        UNTIL index = 0;
    ELSE
        bitsMask := FullElement;
    END;

    startBit := startBit - v^.lowBit;
    w := startBit / BitVectorBits;
    bit := startBit REM BitVectorBits;
    index := 0;
    WHILE w < v^.numElements DO
        element := v^.bits[w];
        IF element <> EmptyElement THEN
            LOOP
                IF bit < BitVectorBits THEN
                    mask := SHIFT(bitsMask, bit);
                    IF (mask * element) <> mask THEN
                        INC(bit);
                    ELSE
                        bottom := (w*BitVectorBits) + bit + v^.lowBit;
                        count := BitVectorBits - bit;
                        IF count > minBits THEN
                            top := bottom + minBits - 1;
                            bit := bit + minBits;
                        ELSE
                            top := bottom + count - 1;
                            bit := bit + count;
                        END;

                        IF bit < BitVectorBits THEN
                            mask := BitVectorElement{bit};
                            WHILE (mask * element) = mask DO
                                mask := SHIFT(mask, 1);
                                INC(top);
                                INC(bit);
                            END;
                        END;

                        IF bit = BitVectorBits THEN
                            INC(w);
                            WHILE (w < v^.numElements) AND (v^.bits[w] = FullElement) DO
                                top := top + BitVectorBits;
                                INC(w);
                            END;
                            IF w < v^.numElements THEN
                                element := v^.bits[w];
                                bit := 0;
                                mask := BitVectorElement{0};
                                WHILE (mask * element) = mask DO
                                    mask := SHIFT(mask, 1);
                                    INC(top);
                                    INC(bit);
                                END;
                            END;
                        END;

                        IF ((top-bottom+1) >= minBits) AND (index <= HIGH(result)) THEN
                            result[index].bottom := bottom;
                            result[index].top := top;
                            INC(index);
                        END;
                    END;
                ELSE
                    EXIT;
                END;
            END;
        END;

        INC(w);
        bit := 0;
    END;

    LeaveLock(v);
    RETURN index;
END FindSetBitsEx;

PROCEDURE FindClearBitsEx(startBit, minBits : CARDINAL;
                          v : BitVector;
                          VAR OUT result : ARRAY OF BitRange) : CARDINAL;
VAR
    element,
    mask,
    bitsMask    : BitVectorElement;
    w,
    bit,
    bottom,
    top,
    count,
    index       : CARDINAL;
BEGIN
    EnterLock(v);

    IF minBits < BitVectorBits THEN
        bitsMask := EmptyElement;
        index := minBits;
        REPEAT
            DEC(index);
            INCL(bitsMask, index);
        UNTIL index = 0;
    ELSE
        bitsMask := FullElement;
    END;

    startBit := startBit - v^.lowBit;
    w := startBit / BitVectorBits;
    bit := startBit REM BitVectorBits;
    index := 0;
    WHILE w < v^.numElements DO
        element := v^.bits[w];
        IF element <> FullElement THEN
            LOOP
                IF bit < BitVectorBits THEN
                    mask := SHIFT(bitsMask, bit);
                    IF (mask * element) <> EmptyElement THEN
                        INC(bit);
                    ELSE
                        bottom := (w*BitVectorBits) + bit + v^.lowBit;
                        count := BitVectorBits - bit;
                        IF count > minBits THEN
                            top := bottom + minBits - 1;
                            bit := bit + minBits;
                        ELSE
                            top := bottom + count - 1;
                            bit := bit + count;
                        END;

                        IF bit < BitVectorBits THEN
                            mask := BitVectorElement{bit};
                            WHILE (mask * element) = EmptyElement DO
                                mask := SHIFT(mask, 1);
                                INC(top);
                                INC(bit);
                            END;
                        END;

                        IF bit = BitVectorBits THEN
                            INC(w);
                            WHILE (w < v^.numElements) AND (v^.bits[w] = EmptyElement) DO
                                top := top + BitVectorBits;
                                INC(w);
                            END;
                            IF w < v^.numElements THEN
                                element := v^.bits[w];
                                bit := 0;
                                mask := BitVectorElement{0};
                                WHILE (mask * element) = EmptyElement DO
                                    mask := SHIFT(mask, 1);
                                    INC(top);
                                    INC(bit);
                                END;
                            END;
                        END;

                        IF ((top-bottom+1) >= minBits) AND (index <= HIGH(result)) THEN
                            result[index].bottom := bottom;
                            result[index].top := top;
                            INC(index);
                        END;
                    END;
                ELSE
                    EXIT;
                END;
            END;
        END;

        INC(w);
        bit := 0;
    END;

    LeaveLock(v);
    RETURN index;
END FindClearBitsEx;

PROCEDURE Empty(v : BitVector) : BOOLEAN;
VAR
    i           : CARDINAL;
BEGIN
    EnterLock(v);

    FOR i := 0 TO v^.numElements-1 DO
        IF v^.bits[i] <> EmptyElement THEN
            LeaveLock(v);
            RETURN FALSE;
        END;
    END;

    LeaveLock(v);
    RETURN TRUE;
END Empty;

PROCEDURE test(b : CARDINAL; v : BitVector) : BOOLEAN [INLINE];
BEGIN
    b := b - v^.lowBit;
    RETURN (b REM BitVectorBits) IN v^.bits[b / BitVectorBits];
END test;

PROCEDURE include(b : CARDINAL; v : BitVector) [INLINE];
BEGIN
    b := b - v^.lowBit;
    INCL(v^.bits[b / BitVectorBits], b REM BitVectorBits);
END include;

PROCEDURE exclude(b : CARDINAL; v : BitVector) [INLINE];
BEGIN
    b := b - v^.lowBit;
    EXCL(v^.bits[b / BitVectorBits], b REM BitVectorBits);
END exclude;

PROCEDURE And(v1, v2 : BitVector);
VAR
    p1, p2      : ElementPointer;
    count, fill : CARDINAL;

    b           : CARDINAL;
BEGIN
    EnterLocks(v1, v2);

    IF v1^.lowBit = v2^.lowBit THEN
        p1 := ADR(v1^.bits);
        p2 := ADR(v2^.bits);

        (* which is smaller *)

        fill := 0;
        count := v2^.numElements;
        IF v1^.numElements <= v2^.numElements THEN
            fill := v2^.numElements - v1^.numElements;
            count := v1^.numElements;
        END;

        (* performance loop *)

        LOOP
            IF count >= 4 THEN
                count := count - 4;
                p2^[0] := p2^[0] * p1^[0];
                p2^[1] := p2^[1] * p1^[1];
                p2^[2] := p2^[2] * p1^[2];
                p2^[3] := p2^[3] * p1^[3];
                p2 := ADDADR(p2, 4*ElementSize);
                p1 := ADDADR(p1, 4*ElementSize);
                IF count = 0 THEN
                    EXIT;
                END;

            ELSIF count >= 2 THEN
                count := count - 2;
                p2^[0] := p2^[0] * p1^[0];
                p2^[1] := p2^[1] * p1^[1];
                p2 := ADDADR(p2, 2*ElementSize);
                p1 := ADDADR(p1, 2*ElementSize);
                IF count = 0 THEN
                    EXIT;
                END;

            ELSE
                p2^[0] := p2^[0] * p1^[0];
                p2 := ADDADR(p2, ElementSize);
                EXIT;
            END;
        END;

        (* the smaller vector has assumed zeros beyond its end *)

        IF fill > 0 THEN
            LOOP
                IF fill >= 4 THEN
                    fill := fill - 4;
                    p2^[0] := EmptyElement;
                    p2^[1] := EmptyElement;
                    p2^[2] := EmptyElement;
                    p2^[3] := EmptyElement;
                    p2 := ADDADR(p2, 4*ElementSize);
                    IF fill = 0 THEN
                        EXIT;
                    END;
                ELSIF fill >= 2 THEN
                    fill := fill - 2;
                    p2^[0] := EmptyElement;
                    p2^[1] := EmptyElement;
                    p2 := ADDADR(p2, 2*ElementSize);
                    IF fill = 0 THEN
                        EXIT;
                    END;
                ELSE
                    p2^[0] := EmptyElement;
                    EXIT;
                END;
            END;
        END;
    ELSE
        FOR b := v2^.lowBit TO v2^.highBit DO
            IF (b >= v1^.lowBit) AND (b <= v1^.highBit) THEN
                IF test(b, v1) THEN
                    IF NOT test(b, v2) THEN
                        exclude(b, v2);
                    END;
                ELSE
                    exclude(b, v2);
                END;
            ELSE
                exclude(b, v2);
            END;
        END;
    END;

    LeaveLocks(v1, v2);
END And;

PROCEDURE Or(v1, v2 : BitVector);
VAR
    p1, p2      : ElementPointer;
    count       : CARDINAL;

    b           : CARDINAL;
BEGIN
    EnterLocks(v1, v2);

    IF v1^.lowBit = v2^.lowBit THEN
        p1 := ADR(v1^.bits);
        p2 := ADR(v2^.bits);

        (* which is smaller *)

        count := v2^.numElements;
        IF v1^.numElements <= v2^.numElements THEN
            count := v1^.numElements;
        END;

        (* performance loop *)

        LOOP
            IF count >= 4 THEN
                count := count - 4;
                p2^[0] := p2^[0] + p1^[0];
                p2^[1] := p2^[1] + p1^[1];
                p2^[2] := p2^[2] + p1^[2];
                p2^[3] := p2^[3] + p1^[3];
                p2 := ADDADR(p2, 4*ElementSize);
                p1 := ADDADR(p1, 4*ElementSize);
                IF count = 0 THEN
                    EXIT;
                END;

            ELSIF count >= 2 THEN
                count := count - 2;
                p2^[0] := p2^[0] + p1^[0];
                p2^[1] := p2^[1] + p1^[1];
                p2 := ADDADR(p2, 2*ElementSize);
                p1 := ADDADR(p1, 2*ElementSize);
                IF count = 0 THEN
                    EXIT;
                END;

            ELSE
                p2^[0] := p2^[0] + p1^[0];
                EXIT;
            END;
        END;
    ELSE
        FOR b := v2^.lowBit TO v2^.highBit DO
            IF (b >= v1^.lowBit) AND (b <= v1^.highBit) THEN
                IF test(b, v1) THEN
                    include(b, v2);
                END;
            END;
        END;
    END;

    LeaveLocks(v1, v2);
END Or;

PROCEDURE Minus(v1, v2 : BitVector);
VAR
    p1, p2      : ElementPointer;
    count       : CARDINAL;

    b           : CARDINAL;
BEGIN
    EnterLocks(v1, v2);

    IF v1^.lowBit = v2^.lowBit THEN
        p1 := ADR(v1^.bits);
        p2 := ADR(v2^.bits);

        (* which is smaller *)

        count := v2^.numElements;
        IF v1^.numElements <= v2^.numElements THEN
            count := v1^.numElements;
        END;

        (* performance loop *)

        LOOP
            IF count >= 4 THEN
                count := count - 4;
                p2^[0] := p2^[0] - p1^[0];
                p2^[1] := p2^[1] - p1^[1];
                p2^[2] := p2^[2] - p1^[2];
                p2^[3] := p2^[3] - p1^[3];
                p2 := ADDADR(p2, 4*ElementSize);
                p1 := ADDADR(p1, 4*ElementSize);
                IF count = 0 THEN
                    EXIT;
                END;

            ELSIF count >= 2 THEN
                count := count - 2;
                p2^[0] := p2^[0] - p1^[0];
                p2^[1] := p2^[1] - p1^[1];
                p2 := ADDADR(p2, 2*ElementSize);
                p1 := ADDADR(p1, 2*ElementSize);
                IF count = 0 THEN
                    EXIT;
                END;

            ELSE
                p2^[0] := p2^[0] - p1^[0];
                EXIT;
            END;
        END;
    ELSE
        FOR b := v2^.lowBit TO v2^.highBit DO
            IF (b >= v1^.lowBit) AND (b <= v1^.highBit) THEN
                IF test(b, v1) THEN
                    exclude(b, v2);
                END;
            END;
        END;
    END;

    LeaveLocks(v1, v2);
END Minus;

PROCEDURE Invert(v : BitVector);
VAR
    p           : ElementPointer;
    count       : CARDINAL;
BEGIN
    EnterLock(v);

    p := ADR(v^.bits);
    count := v^.numElements;

    (* performance loop *)

    (* XOR with all bits is the same as a NOT *)

    LOOP
        IF count >= 4 THEN
            count := count - 4;
            p^[0] := p^[0] / FullElement;
            p^[1] := p^[1] / FullElement;
            p^[2] := p^[2] / FullElement;
            p^[3] := p^[3] / FullElement;
            p := ADDADR(p, 4*ElementSize);
            IF count = 0 THEN
                EXIT;
            END;

        ELSIF count >= 2 THEN
            count := count - 2;
            p^[0] := p^[0] / FullElement;
            p^[1] := p^[1] / FullElement;
            p := ADDADR(p, 2*ElementSize);
            IF count = 0 THEN
                EXIT;
            END;

        ELSE
            p^[0] := p^[0] / FullElement;
            EXIT;
        END;
    END;

    LeaveLock(v);
END Invert;

PROCEDURE ShiftLeft(v : BitVector; bits : CARDINAL);
VAR
    words       : CARDINAL;
    i, j        : CARDINAL;
    temp        : CARDINAL;
    count       : CARDINAL;
    ptr         : POINTER TO ARRAY [0..0] OF ADRCARD;
BEGIN
    ptr := ADR(v^.bits);
    count := v^.numElements;

    (* shift words *)

    words := bits / BitVectorBits;
    IF words <> 0 THEN
        IF words < count THEN
            i := count - words;
            j := i + words;
            REPEAT
                DEC(i);
                DEC(j);
                ptr^[j] := ptr^[i];
            UNTIL i = 0;
        ELSE
            words := count;
        END;
        FOR i := 0 TO words-1 DO
            ptr^[i] := 0;
        END;
    END;

    (* shift bits *)

    bits := bits REM BitVectorBits;
    IF bits <> 0 THEN
        temp := BitVectorBits - bits;
        IF count > 1 THEN
            j := count - 2;
            FOR i := count-1 TO 1 BY -1 DO
                ptr^[i] := (ptr^[i] SHL bits) BOR (ptr^[j] SHR temp);
                DEC(j);
            END;
        END;
        ptr^[0] := ptr^[0] SHL bits;
    END;
END ShiftLeft;

PROCEDURE ShiftRight(v : BitVector; bits : CARDINAL);
VAR
    words       : CARDINAL;
    i, j        : CARDINAL;
    temp        : CARDINAL;
    count       : CARDINAL;
    ptr         : POINTER TO ARRAY [0..0] OF ADRCARD;
BEGIN
    count := v^.numElements;
    ptr := ADR(v^.bits);

    (* shift words *)

    words := bits / BitVectorBits;
    IF words <> 0 THEN
        IF words < count THEN
            j := words;
            FOR i := 0 TO count-words-1 DO
                ptr^[i] := ptr^[j];
                INC(j);
            END;
        ELSE
            words := count;
        END;
        FOR i := count-words TO count-1 DO
            ptr^[i] := 0;
        END;
    END;

    (* shift bits *)

    bits := bits REM BitVectorBits;
    IF bits <> 0 THEN
        IF count > 1 THEN
            temp := BitVectorBits - bits;
            j := 1;
            FOR i := 0 TO count-2 DO
                ptr^[i] := (ptr^[i] SHR bits) BOR (ptr^[j] SHL temp);
                INC(j);
            END;
        END;
        ptr^[count-1] := ptr^[count-1] SHR bits;
    END;
END ShiftRight;

PROCEDURE BitsInCommon(v1, v2 : BitVector) : BOOLEAN;
VAR
    p1, p2      : ElementPointer;
    count       : CARDINAL;

    b           : CARDINAL;
BEGIN
    EnterLocks(v1, v2);

    IF v1^.lowBit = v2^.lowBit THEN
        p1 := ADR(v1^.bits);
        p2 := ADR(v2^.bits);

        (* which is smaller *)

        count := v2^.numElements;
        IF v1^.numElements <= v2^.numElements THEN
            count := v1^.numElements;
        END;

        (* performance loop *)

        LOOP
            IF count >= 4 THEN
                count := count - 4;

                IF ((p2^[0] * p1^[0]) = EmptyElement) AND
                   ((p2^[1] * p1^[1]) = EmptyElement) AND
                   ((p2^[2] * p1^[2]) = EmptyElement) AND
                   ((p2^[3] * p1^[3]) = EmptyElement)
                THEN
                    IF count > 0 THEN
                        p2 := ADDADR(p2, 4*ElementSize);
                        p1 := ADDADR(p1, 4*ElementSize);
                    ELSE
                        LeaveLocks(v1, v2);
                        RETURN FALSE;
                    END;
                ELSE
                    LeaveLocks(v1, v2);
                    RETURN TRUE;
                END;

            ELSIF count >= 2 THEN
                count := count - 2;

                IF ((p2^[0] * p1^[0]) = EmptyElement) AND
                   ((p2^[1] * p1^[1]) = EmptyElement)
                THEN
                    IF count > 0 THEN
                        p2 := ADDADR(p2, 2*ElementSize);
                        p1 := ADDADR(p1, 2*ElementSize);
                    ELSE
                        LeaveLocks(v1, v2);
                        RETURN FALSE;
                    END;
                ELSE
                    LeaveLocks(v1, v2);
                    RETURN TRUE;
                END;

            ELSE
                IF (p2^[0] * p1^[0]) = EmptyElement THEN
                    LeaveLocks(v1, v2);
                    RETURN FALSE;
                ELSE
                    LeaveLocks(v1, v2);
                    RETURN TRUE;
                END;
            END;
        END;
    ELSE
        FOR b := v2^.lowBit TO v2^.highBit DO
            IF (b >= v1^.lowBit) AND (b <= v1^.highBit) THEN
                IF test(b, v1) AND test(b, v2) THEN
                    LeaveLocks(v1, v2);
                    RETURN TRUE;
                END;
            END;
        END;
    END;

    LeaveLocks(v1, v2);
    RETURN FALSE;
END BitsInCommon;

PROCEDURE Equal(v1, v2 : BitVector) : BOOLEAN;
VAR
    p1, p2      : ElementPointer;
    count, fill : CARDINAL;

    b           : CARDINAL;
    lowBit      : CARDINAL;
    highBit     : CARDINAL;
BEGIN
    EnterLocks(v1, v2);

    IF v1^.lowBit = v2^.lowBit THEN
        p1 := ADR(v1^.bits);
        p2 := ADR(v2^.bits);

        (* which is smaller *)

        IF v1^.numElements <= v2^.numElements THEN
            fill := v2^.numElements - v1^.numElements;
            count := v1^.numElements;
        ELSE
            fill := v1^.numElements - v2^.numElements;
            count := v2^.numElements;
        END;

        (* performance loop *)

        LOOP
            IF count >= 4 THEN
                count := count - 4;

                IF (p2^[0] = p1^[0]) AND
                   (p2^[1] = p1^[1]) AND
                   (p2^[2] = p1^[2]) AND
                   (p2^[3] = p1^[3])
                THEN
                    IF count > 0 THEN
                        p2 := ADDADR(p2, 4*ElementSize);
                        p1 := ADDADR(p1, 4*ElementSize);
                    ELSE
                        EXIT;
                    END;
                ELSE
                    LeaveLocks(v1, v2);
                    RETURN FALSE;
                END;

            ELSIF count >= 2 THEN
                count := count - 2;

                IF (p2^[0] = p1^[0]) AND (p2^[1] = p1^[1]) THEN
                    IF count > 0 THEN
                        p2 := ADDADR(p2, 2*ElementSize);
                        p1 := ADDADR(p1, 2*ElementSize);
                    ELSE
                        EXIT;
                    END;
                ELSE
                    LeaveLocks(v1, v2);
                    RETURN FALSE;
                END;

            ELSE
                IF p2^[0] = p1^[0] THEN
                    EXIT;
                ELSE
                    LeaveLocks(v1, v2);
                    RETURN FALSE;
                END;
            END;
        END;

        (* make sure the larger vector is empty from this point on *)

        IF fill > 0 THEN
            IF v1^.numElements > v2^.numElements THEN
                p2 := p1;
            END;

            LOOP
                IF fill >= 4 THEN
                    fill := fill - 4;

                    IF (p2^[0] = EmptyElement) AND
                       (p2^[1] = EmptyElement) AND
                       (p2^[2] = EmptyElement) AND
                       (p2^[3] = EmptyElement)
                    THEN
                        p2 := ADDADR(p2, 4*ElementSize);
                    ELSE
                        LeaveLocks(v1, v2);
                        RETURN FALSE;
                    END;

                ELSIF fill >= 2 THEN
                    fill := fill - 2;

                    IF (p2^[0] = EmptyElement) AND (p2^[1] = EmptyElement) THEN
                        p2 := ADDADR(p2, 2*ElementSize);
                    ELSE
                        LeaveLocks(v1, v2);
                        RETURN FALSE;
                    END;

                ELSIF fill > 0 THEN
                    DEC(fill);

                    IF p2^[0] = EmptyElement THEN
                        p2 := ADDADR(p2, ElementSize);
                    ELSE
                        LeaveLocks(v1, v2);
                        RETURN FALSE;
                    END;
                ELSE
                    EXIT;
                END;
            END;
        END;
    ELSE
        lowBit := v1^.lowBit;
        IF lowBit > v2^.lowBit THEN
            lowBit := v2^.lowBit;
        END;
        highBit := v1^.highBit;
        IF highBit < v2^.highBit THEN
            highBit := v2^.highBit;
        END;
        FOR b := lowBit TO highBit DO
            IF (b >= v1^.lowBit) AND (b <= v1^.highBit) THEN
                IF (b >= v2^.lowBit) AND (b <= v2^.highBit) THEN
                    IF test(b, v1) <> test(b, v2) THEN
                        LeaveLocks(v1, v2);
                        RETURN FALSE;
                    END;
                ELSE
                    IF test(b, v1) THEN
                        LeaveLocks(v1, v2);
                        RETURN FALSE;
                    END;
                END;
            ELSIF (b >= v2^.lowBit) AND (b <= v2^.highBit) THEN
                IF test(b, v2) THEN
                    LeaveLocks(v1, v2);
                    RETURN FALSE;
                END;
            END;
        END;
    END;

    LeaveLocks(v1, v2);
    RETURN TRUE;
END Equal;

PROCEDURE Copy(v1, v2 : BitVector);
VAR
    p1, p2      : ElementPointer;
    count, fill : CARDINAL;

    b           : CARDINAL;
BEGIN
    EnterLocks(v1, v2);

    IF v1^.lowBit = v2^.lowBit THEN
        p1 := ADR(v1^.bits);
        p2 := ADR(v2^.bits);

        (* which is smaller *)

        fill := 0;
        count := v2^.numElements;
        IF v1^.numElements <= v2^.numElements THEN
            fill := v2^.numElements - v1^.numElements;
            count := v1^.numElements;
        END;

        (* performance loop *)

        LOOP
            IF count >= 4 THEN
                count := count - 4;
                p2^[0] := p1^[0];
                p2^[1] := p1^[1];
                p2^[2] := p1^[2];
                p2^[3] := p1^[3];
                p2 := ADDADR(p2, 4*ElementSize);
                p1 := ADDADR(p1, 4*ElementSize);
                IF count = 0 THEN
                    EXIT;
                END;
            ELSIF count >= 2 THEN
                count := count - 2;
                p2^[0] := p1^[0];
                p2^[1] := p1^[1];
                p2 := ADDADR(p2, 2*ElementSize);
                p1 := ADDADR(p1, 2*ElementSize);
                IF count = 0 THEN
                    EXIT;
                END;
            ELSE
                p2^[0] := p1^[0];
                p2 := ADDADR(p2, ElementSize);
                EXIT;
            END;
        END;

        (* the smaller vector has assumed zeros beyond its end *)

        IF fill > 0 THEN
            LOOP
                IF fill >= 4 THEN
                    fill := fill - 4;
                    p2^[0] := EmptyElement;
                    p2^[1] := EmptyElement;
                    p2^[2] := EmptyElement;
                    p2^[3] := EmptyElement;
                    p2 := ADDADR(p2, 4*ElementSize);
                    IF fill = 0 THEN
                        EXIT;
                    END;
                ELSIF fill >= 2 THEN
                    fill := fill - 2;
                    p2^[0] := EmptyElement;
                    p2^[1] := EmptyElement;
                    p2 := ADDADR(p2, 2*ElementSize);
                    IF fill = 0 THEN
                        EXIT;
                    END;
                ELSE
                    p2^[0] := EmptyElement;
                    EXIT;
                END;
            END;
        END;
    ELSE
        FOR b := v2^.lowBit TO v2^.highBit DO
            IF (b >= v1^.lowBit) AND (b <= v1^.highBit) THEN
                IF test(b, v1) THEN
                    include(b, v2);
                ELSE
                    exclude(b, v2);
                END;
            ELSE
                exclude(b, v2);
            END;
        END;
    END;

    LeaveLocks(v1, v2);
END Copy;

PROCEDURE Duplicate(v1 : BitVector; VAR OUT v2 : BitVector);
BEGIN
    EnterLock(v1);
    NewVector(v2, v1^.lowBit, v1^.highBit);
    LeaveLock(v1);

    IF v2 <> NIL THEN
        Copy(v1, v2);
    END;
END Duplicate;

PROCEDURE Zap(val : BitVectorElement; v : BitVector);
VAR
    p1          : ElementPointer;
    count       : CARDINAL;
BEGIN
    EnterLock(v);

    p1 := ADR(v^.bits);
    count := v^.numElements;

    LOOP
        IF count >= 4 THEN
            count := count - 4;
            p1^[0] := val;
            p1^[1] := val;
            p1^[2] := val;
            p1^[3] := val;
            p1 := ADDADR(p1, 4*ElementSize);
            IF count = 0 THEN
                EXIT;
            END;
        ELSIF count >= 2 THEN
            count := count - 2;
            p1^[0] := val;
            p1^[1] := val;
            p1 := ADDADR(p1, 2*ElementSize);
            IF count = 0 THEN
                EXIT;
            END;
        ELSE
            p1^[0] := val;
            EXIT;
        END;
    END;

    LeaveLock(v);
END Zap;

END BitVectors.
