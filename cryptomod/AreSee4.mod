(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 2009                                  *)
(*                         by ADW Software                                 *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE AreSee4;
<*/OPTIMIZE:T*>

(* the encryption algorithm we can't mention by name since the name is trademarked. *)

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, CAST, ADR, ADDADR;

FROM ExStorage IMPORT
    ALLOCATE, DeallocateEx, HeapInfoPointer, GetHeap;

TYPE
    tBox                = ARRAY [0..255] OF CARDINAL8;

    AreSee4rec =
        RECORD
        compute,
        start           : tBox;
        x, y            : CARDINAL;
        heap            : HeapInfoPointer;
        END;
    AreSee4         = POINTER TO AreSee4rec;

PROCEDURE Create(key : ARRAY OF BYTE; keySize : CARDINAL) : AreSee4;
VAR
    crypt       : AreSee4;
    swap        : CARDINAL8;
    key256      : tBox;
    i, j, l     : ADRCARD;
BEGIN
    NEW(crypt);
    crypt^.heap := GetHeap();

    l := keySize;
    IF l > 256 THEN
        l := 256;
    END;
    FOR i := 0 TO 255 DO
        key256[i] := CAST(CARDINAL8, key[i REM l]);
        crypt^.start[i] := i;
    END;

    j := 0;
    FOR i := 0 TO 255 DO
        j := (j + VAL(ADRCARD, crypt^.start[i]) + VAL(ADRCARD, key256[i])) REM 256;
        swap := crypt^.start[i];
        crypt^.start[i] := crypt^.start[j];
        crypt^.start[j] := swap
    END;

    Reset(crypt);
    RETURN crypt;
END Create;

PROCEDURE Create2(key : ARRAY OF CHAR) : AreSee4;
VAR
    l   : CARDINAL;
BEGIN
    l := LENGTH(key);
    IF l = 0 THEN
        l := 1;
    END;
    RETURN Create(key, l*SIZE(CHAR));
END Create2;

PROCEDURE Destroy(VAR INOUT crypt : AreSee4);
BEGIN
    DeallocateEx(crypt, SIZE(crypt^), crypt^.heap);
END Destroy;

PROCEDURE Reset(crypt : AreSee4);
BEGIN
    crypt^.compute := crypt^.start;
    crypt^.x := 0;
    crypt^.y := 0;
END Reset;

PROCEDURE EnDeCrypt(crypt : AreSee4; input, output : ADDRESS; amount : CARDINAL);
VAR
    x, y, xy            : ADRCARD;
    tx, ty              : CARDINAL8;
BEGIN
    x := crypt^.x;
    y := crypt^.y;

    LOOP
        IF amount > 0 THEN
            DEC(amount);
            x := (x + 1) REM 256;
            y := (y + VAL(ADRCARD, crypt^.compute[x])) REM 256;
            tx := crypt^.compute[x];
            ty := crypt^.compute[y];
            crypt^.compute[x] := ty;
            crypt^.compute[y] := tx;
            xy := (ORD(tx) + ORD(ty)) REM 256;
            output^ := input^ BXOR CAST(BYTE, crypt^.compute[xy]);
            input := ADDADR(input, 1);
            output := ADDADR(output, 1);

            IF amount > 0 THEN
                DEC(amount);
                x := (x + 1) REM 256;
                y := (y + VAL(ADRCARD, crypt^.compute[x])) REM 256;
                tx := crypt^.compute[x];
                ty := crypt^.compute[y];
                crypt^.compute[x] := ty;
                crypt^.compute[y] := tx;
                xy := (ORD(tx) + ORD(ty)) REM 256;
                output^ := input^ BXOR CAST(BYTE, crypt^.compute[xy]);
                input := ADDADR(input, 1);
                output := ADDADR(output, 1);
            ELSE
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;

    crypt^.x := x;
    crypt^.y := y;
END EnDeCrypt;

PROCEDURE SelfTest() : BOOLEAN;
CONST
    key         : ARRAY [0..7] OF CARDINAL8 =
        {01h, 23h, 45h, 67h, 89h, 0abh, 0cdh, 0efh};
    input1      : ARRAY [0..7] OF CARDINAL8 =
        {01h, 23h, 45h, 67h, 89h, 0abh, 0cdh, 0efh};
    output1      : ARRAY [0..7] OF CARDINAL8 =
        {75h, 0b7h, 87h, 80h, 99h, 0e0h, 0c5h, 96h};
    input2      : ARRAY [0..7] OF CARDINAL8 =
        {0 BY 8};
    output2      : ARRAY [0..7] OF CARDINAL8 =
        {74h, 94h, 0c2h, 0e7h, 10h, 4bh, 08h, 79h};
VAR
    crypt       : AreSee4;
    <*/PUSH/NOCHECK:U*>
    output      : ARRAY [0..7] OF CARDINAL8;
    <*/POP*>
    ok          : BOOLEAN;

    PROCEDURE verify(a, b : ARRAY OF CARDINAL8) : BOOLEAN;
    VAR
        i       : CARDINAL;
    BEGIN
        FOR i := 0 TO HIGH(a) DO
            IF a[i] <> b[i] THEN
                RETURN FALSE;
            END;
        END;
        RETURN TRUE;
    END verify;

BEGIN
    ok := TRUE;

    crypt := Create(key, SIZE(key));

    EnDeCrypt(crypt, ADR(input1), ADR(output), SIZE(input1));
    ok := ok AND verify(output, output1);
    Reset(crypt);
    EnDeCrypt(crypt, ADR(output), ADR(output), SIZE(output));
    ok := ok AND verify(output, input1);

    Reset(crypt);
    EnDeCrypt(crypt, ADR(input2), ADR(output), SIZE(input2));
    ok := ok AND verify(output, output2);

    Destroy(crypt);

    RETURN ok;
END SelfTest;

END AreSee4.
