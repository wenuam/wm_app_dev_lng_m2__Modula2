IMPLEMENTATION MODULE DES;
<*/OPTIMIZE:T*>

(*
  the code in this module was based on the DES implementation in
  CryptPak 4.05 by Markus Hahn.
  http://come.to/hahn
*)

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, ADR, ADDADR, BIGENDIAN;

FROM ExStorage IMPORT
    ALLOCATE, DeallocateEx, HeapInfoPointer, GetHeap;

TYPE
    KeyArray            = ARRAY [0..31] OF CARDINAL32;

    DESRec =
        RECORD
        k1e, k2e, k3e   : KeyArray;(* encrypt *)
        k1d, k2d, k3d   : KeyArray;(* decrypt *)
        iv              : IV;
        heap            : HeapInfoPointer;
        triple          : BOOLEAN;
        END;
    DES            = POINTER TO DESRec;

CONST
    pc1 : ARRAY [0..55] OF CARDINAL8 =
    {
        56, 48, 40, 32, 24, 16,  8,  0, 57, 49, 41, 33, 25, 17,
         9,  1, 58, 50, 42, 34, 26, 18, 10,  2, 59, 51, 43, 35,
        62, 54, 46, 38, 30, 22, 14,  6, 61, 53, 45, 37, 29, 21,
        13,  5, 60, 52, 44, 36, 28, 20, 12,  4, 27, 19, 11,  3
    };

    totrot : ARRAY [0..15] OF CARDINAL =
    {
        1,2,4,6,8,10,12,14,15,17,19,21,23,25,27,28
    };

    pc2 : ARRAY [0..47] OF CARDINAL8 =
    {
        13, 16, 10, 23,  0,  4,  2, 27, 14,  5, 20,  9,
        22, 18, 11,  3, 25,  7, 15,  6, 26, 19, 12,  1,
        40, 51, 30, 36, 46, 54, 29, 39, 50, 44, 32, 47,
        43, 48, 38, 55, 33, 52, 45, 41, 49, 35, 28, 31
    };


    bytebit : ARRAY [0..7] OF CARDINAL8 =
    {
     80h, 40h, 20h, 10h, 8, 4, 2, 1
    };

    bigbyte : ARRAY [0..23] OF CARDINAL32 =
    {
        0800000h,  0400000h,  0200000h,  0100000h,
        080000h,   040000h,   020000h,   010000h,
        08000h,    04000h,    02000h,    01000h,
        0800h,     0400h,     0200h,     0100h,
        080h,      040h,      020h,      010h,
        08h,       04h,       02h,       01h
    };

    SP0 : ARRAY [0..63] OF CARDINAL32 =
    {
        001010400h, 000000000h, 000010000h, 001010404h,
        001010004h, 000010404h, 000000004h, 000010000h,
        000000400h, 001010400h, 001010404h, 000000400h,
        001000404h, 001010004h, 001000000h, 000000004h,
        000000404h, 001000400h, 001000400h, 000010400h,
        000010400h, 001010000h, 001010000h, 001000404h,
        000010004h, 001000004h, 001000004h, 000010004h,
        000000000h, 000000404h, 000010404h, 001000000h,
        000010000h, 001010404h, 000000004h, 001010000h,
        001010400h, 001000000h, 001000000h, 000000400h,
        001010004h, 000010000h, 000010400h, 001000004h,
        000000400h, 000000004h, 001000404h, 000010404h,
        001010404h, 000010004h, 001010000h, 001000404h,
        001000004h, 000000404h, 000010404h, 001010400h,
        000000404h, 001000400h, 001000400h, 000000000h,
        000010004h, 000010400h, 000000000h, 001010004h
    };

    SP1 : ARRAY [0..63] OF CARDINAL32 =
    {
        080108020h, 080008000h, 000008000h, 000108020h,
        000100000h, 000000020h, 080100020h, 080008020h,
        080000020h, 080108020h, 080108000h, 080000000h,
        080008000h, 000100000h, 000000020h, 080100020h,
        000108000h, 000100020h, 080008020h, 000000000h,
        080000000h, 000008000h, 000108020h, 080100000h,
        000100020h, 080000020h, 000000000h, 000108000h,
        000008020h, 080108000h, 080100000h, 000008020h,
        000000000h, 000108020h, 080100020h, 000100000h,
        080008020h, 080100000h, 080108000h, 000008000h,
        080100000h, 080008000h, 000000020h, 080108020h,
        000108020h, 000000020h, 000008000h, 080000000h,
        000008020h, 080108000h, 000100000h, 080000020h,
        000100020h, 080008020h, 080000020h, 000100020h,
        000108000h, 000000000h, 080008000h, 000008020h,
        080000000h, 080100020h, 080108020h, 000108000h
    };

    SP2 : ARRAY [0..63] OF CARDINAL32 =
    {
        000000208h, 008020200h, 000000000h, 008020008h,
        008000200h, 000000000h, 000020208h, 008000200h,
        000020008h, 008000008h, 008000008h, 000020000h,
        008020208h, 000020008h, 008020000h, 000000208h,
        008000000h, 000000008h, 008020200h, 000000200h,
        000020200h, 008020000h, 008020008h, 000020208h,
        008000208h, 000020200h, 000020000h, 008000208h,
        000000008h, 008020208h, 000000200h, 008000000h,
        008020200h, 008000000h, 000020008h, 000000208h,
        000020000h, 008020200h, 008000200h, 000000000h,
        000000200h, 000020008h, 008020208h, 008000200h,
        008000008h, 000000200h, 000000000h, 008020008h,
        008000208h, 000020000h, 008000000h, 008020208h,
        000000008h, 000020208h, 000020200h, 008000008h,
        008020000h, 008000208h, 000000208h, 008020000h,
        000020208h, 000000008h, 008020008h, 000020200h
    };

    SP3 : ARRAY [0..63] OF CARDINAL32 =
    {
        000802001h, 000002081h, 000002081h, 000000080h,
        000802080h, 000800081h, 000800001h, 000002001h,
        000000000h, 000802000h, 000802000h, 000802081h,
        000000081h, 000000000h, 000800080h, 000800001h,
        000000001h, 000002000h, 000800000h, 000802001h,
        000000080h, 000800000h, 000002001h, 000002080h,
        000800081h, 000000001h, 000002080h, 000800080h,
        000002000h, 000802080h, 000802081h, 000000081h,
        000800080h, 000800001h, 000802000h, 000802081h,
        000000081h, 000000000h, 000000000h, 000802000h,
        000002080h, 000800080h, 000800081h, 000000001h,
        000802001h, 000002081h, 000002081h, 000000080h,
        000802081h, 000000081h, 000000001h, 000002000h,
        000800001h, 000002001h, 000802080h, 000800081h,
        000002001h, 000002080h, 000800000h, 000802001h,
        000000080h, 000800000h, 000002000h, 000802080h
    };

    SP4 : ARRAY [0..63] OF CARDINAL32 =
    {
        000000100h, 002080100h, 002080000h, 042000100h,
        000080000h, 000000100h, 040000000h, 002080000h,
        040080100h, 000080000h, 002000100h, 040080100h,
        042000100h, 042080000h, 000080100h, 040000000h,
        002000000h, 040080000h, 040080000h, 000000000h,
        040000100h, 042080100h, 042080100h, 002000100h,
        042080000h, 040000100h, 000000000h, 042000000h,
        002080100h, 002000000h, 042000000h, 000080100h,
        000080000h, 042000100h, 000000100h, 002000000h,
        040000000h, 002080000h, 042000100h, 040080100h,
        002000100h, 040000000h, 042080000h, 002080100h,
        040080100h, 000000100h, 002000000h, 042080000h,
        042080100h, 000080100h, 042000000h, 042080100h,
        002080000h, 000000000h, 040080000h, 042000000h,
        000080100h, 002000100h, 040000100h, 000080000h,
        000000000h, 040080000h, 002080100h, 040000100h
    };

    SP5 : ARRAY [0..63] OF CARDINAL32 =
    {
        020000010h, 020400000h, 000004000h, 020404010h,
        020400000h, 000000010h, 020404010h, 000400000h,
        020004000h, 000404010h, 000400000h, 020000010h,
        000400010h, 020004000h, 020000000h, 000004010h,
        000000000h, 000400010h, 020004010h, 000004000h,
        000404000h, 020004010h, 000000010h, 020400010h,
        020400010h, 000000000h, 000404010h, 020404000h,
        000004010h, 000404000h, 020404000h, 020000000h,
        020004000h, 000000010h, 020400010h, 000404000h,
        020404010h, 000400000h, 000004010h, 020000010h,
        000400000h, 020004000h, 020000000h, 000004010h,
        020000010h, 020404010h, 000404000h, 020400000h,
        000404010h, 020404000h, 000000000h, 020400010h,
        000000010h, 000004000h, 020400000h, 000404010h,
        000004000h, 000400010h, 020004010h, 000000000h,
        020404000h, 020000000h, 000400010h, 020004010h
    };

    SP6 : ARRAY [0..63] OF CARDINAL32 =
    {
        000200000h, 004200002h, 004000802h, 000000000h,
        000000800h, 004000802h, 000200802h, 004200800h,
        004200802h, 000200000h, 000000000h, 004000002h,
        000000002h, 004000000h, 004200002h, 000000802h,
        004000800h, 000200802h, 000200002h, 004000800h,
        004000002h, 004200000h, 004200800h, 000200002h,
        004200000h, 000000800h, 000000802h, 004200802h,
        000200800h, 000000002h, 004000000h, 000200800h,
        004000000h, 000200800h, 000200000h, 004000802h,
        004000802h, 004200002h, 004200002h, 000000002h,
        000200002h, 004000000h, 004000800h, 000200000h,
        004200800h, 000000802h, 000200802h, 004200800h,
        000000802h, 004000002h, 004200802h, 004200000h,
        000200800h, 000000000h, 000000002h, 004200802h,
        000000000h, 000200802h, 004200000h, 000000800h,
        004000002h, 004000800h, 000000800h, 000200002h
    };

    SP7 : ARRAY [0..63] OF CARDINAL32 =
    {
        010001040h, 000001000h, 000040000h, 010041040h,
        010000000h, 010001040h, 000000040h, 010000000h,
        000040040h, 010040000h, 010041040h, 000041000h,
        010041000h, 000041040h, 000001000h, 000000040h,
        010040000h, 010000040h, 010001000h, 000001040h,
        000041000h, 000040040h, 010040040h, 010041000h,
        000001040h, 000000000h, 000000000h, 010040040h,
        010000040h, 010001000h, 000041040h, 000040000h,
        000041040h, 000040000h, 010041000h, 000001000h,
        000000040h, 010040040h, 000001000h, 000041040h,
        010001000h, 000000040h, 010000040h, 010040000h,
        010040040h, 010000000h, 000040000h, 010001040h,
        000000000h, 010041040h, 000040040h, 010000040h,
        010040000h, 010001000h, 010001040h, 000000000h,
        010041040h, 000041000h, 000041000h, 000001040h,
        000001040h, 000040040h, 010000000h, 010041000h
    };

PROCEDURE ParityCheck(b : CARDINAL8) : BOOLEAN [INLINE];
VAR
    a   : CARDINAL8;
BEGIN
    a := b BXOR (b SHR 4);
    RETURN ((a BXOR (a SHR 1) BXOR (a SHR 2) BXOR (a SHR 3)) BAND 1) <> 0;
END ParityCheck;

PROCEDURE SetParity(VAR INOUT key : ARRAY OF Key1);
VAR
    i, j        : ADRCARD;
    highKey     : ADRCARD;
BEGIN
    highKey := HIGH(key);
    FOR i := 0 TO highKey DO
        FOR j := 0 TO 7 DO
            IF NOT ParityCheck(key[i, j]) THEN
                key[i, j] := VAL(CARDINAL8, key[i, j]) BXOR 1;
            END;
        END;
    END;
END SetParity;

PROCEDURE CheckParity(key : ARRAY OF Key1) : BOOLEAN;
VAR
    i, j        : CARDINAL;
    highKey     : CARDINAL;
BEGIN
    highKey := HIGH(key);
    FOR i := 0 TO highKey DO
        FOR j := 0 TO HIGH(key) DO
            IF NOT ParityCheck(key[i, j]) THEN
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END CheckParity;

PROCEDURE KeySetup(inKey : Key1;
                   VAR OUT outKey : KeyArray;
                   encrypt : BOOLEAN);
VAR
    i, j, l, m, n       : ADRCARD;
    totr                : ADRCARD;
    pc1m, pcr           : ARRAY [0..55] OF CARDINAL8;
    kn                  : KeyArray;
    temp                : CARDINAL32;
BEGIN
    FOR i := 0 TO 55 DO
        l := pc1[i];
        IF (VAL(CARDINAL8, inKey[l / 8]) BAND bytebit[l REM 8]) <> 0 THEN
            pc1m[i] := 1;
        ELSE
            pc1m[i] := 0;
        END;
    END;

    FOR i := 0 TO 15 DO
        IF encrypt THEN
            m := i SHL 1;
        ELSE
            m := (15 - i) SHL 1;
        END;
        n := m + 1;
        kn[m] := 0;
        kn[n] := 0;

        FOR j := 0 TO 27 DO
            totr := totrot[i];
            l := j + totr;
            IF l < 28 THEN
                pcr[j] := pc1m[l];
            ELSE
                pcr[j] := pc1m[l - 28];
            END;
        END;

        FOR j := 28 TO 55 DO
            totr := totrot[i];
            l := j + totr;
            IF l < 56 THEN
                pcr[j] := pc1m[l];
            ELSE
                pcr[j] := pc1m[l - 28];
            END;
        END;

        FOR j := 0 TO 23 DO
            IF pcr[pc2[j]] <> 0 THEN
                kn[m] := kn[m] BOR bigbyte[j];
            END;
            IF pcr[pc2[j + 24]] <> 0 THEN
                kn[n] := kn[n] BOR bigbyte[j];
            END;
        END;
    END;

    FOR i := 0 TO 30 BY 2 DO
        temp := (kn[i] BAND 000fc0000h) SHL 6;
        temp := temp BOR ((kn[i] BAND 000000fc0h) SHL 10);
        temp := temp BOR ((kn[i+1] BAND 000fc0000h) SHR 10);
        outKey[i] := temp BOR ((kn[i+1] BAND 000000fc0h) SHR 6);

        temp := (kn[i] BAND 00003f000h) SHL 12;
        temp := temp BOR ((kn[i] BAND 00000003fh) SHL 16);
        temp := temp BOR ((kn[i+1] BAND 00003f000h) SHR 4);
        outKey[i+1] := temp BOR (kn[i+1] BAND 00000003fh);
    END;
END KeySetup;

PROCEDURE Create(key : ARRAY OF Key1) : DES;
VAR
    crypt       : DES;
BEGIN
    crypt := NIL;

    IF (HIGH(key) >= 0) AND (HIGH(key) <= 2) THEN
        NEW(crypt);
        crypt^.heap := GetHeap();
        crypt^.triple := FALSE;

        KeySetup(key[0], crypt^.k1e, TRUE);
        KeySetup(key[0], crypt^.k1d, FALSE);

        IF HIGH(key) >= 1 THEN
            crypt^.triple := TRUE;

            KeySetup(key[1], crypt^.k2e, FALSE);
            KeySetup(key[1], crypt^.k2d, TRUE);

            IF HIGH(key) = 2 THEN
                KeySetup(key[2], crypt^.k3e, TRUE);
                KeySetup(key[2], crypt^.k3d, FALSE);
            ELSE
                crypt^.k3e := crypt^.k1e;
                crypt^.k3d := crypt^.k1d;
            END;
        END;
    END;

    RETURN crypt;
END Create;

PROCEDURE Destroy(VAR INOUT crypt : DES);
BEGIN
    DeallocateEx(crypt, SIZE(crypt^), crypt^.heap);
END Destroy;

PROCEDURE ResetIV(crypt : DES; iv : IV);
BEGIN
    crypt^.iv := iv;
END ResetIV;

PROCEDURE load(input : ADDRESS; VAR OUT low, high : CARDINAL32) [INLINE];
VAR
    data        : POINTER TO ARRAY [0..1] OF CARDINAL32;
BEGIN
    (* alignment and endian dependent but faster.
       2 reads as opposed to 8.
    *)
    data := input;
    low := BIGENDIAN(data^[0]);
    high := BIGENDIAN(data^[1]);
END load;

PROCEDURE store(low, high : CARDINAL32; output : ADDRESS) [INLINE];
VAR
    data        : POINTER TO ARRAY [0..1] OF CARDINAL32;
BEGIN
    (* alignment and endian dependent but faster.
       2 writes as opposed to 8.
    *)
    data := output;
    data^[0] := BIGENDIAN(low);
    data^[1] := BIGENDIAN(high);
END store;

PROCEDURE IP(VAR INOUT left, right : CARDINAL32) [INLINE];
VAR
    temp        : CARDINAL32;
BEGIN
    (* initial permutation *)

    temp := ((left SHR 4) BXOR right) BAND 00f0f0f0fh;
    right := right BXOR temp;
    left := left BXOR (temp SHL 4);

    temp := ((left SHR 16) BXOR right) BAND 00000ffffh;
    right := right BXOR temp;
    left := left BXOR (temp SHL 16);

    temp := ((right SHR 2) BXOR left) BAND 033333333h;
    left := left BXOR temp;
    right := right BXOR (temp SHL 2);

    temp := ((right SHR 8) BXOR left) BAND 000ff00ffh;
    left := left BXOR temp;
    right := right BXOR (temp SHL 8);
    right := right ROL 1;
        (*right := ((right SHL 1) BOR ((right SHR 31) BAND 1)) BAND 0ffffffffh;*)

    temp := (left BXOR right) BAND 0aaaaaaaah;
    left := left BXOR temp;
    right := right BXOR temp;
    left := left ROL 1;
        (*left := ((left SHL 1) BOR ((left SHR 31) BAND 1)) BAND 0ffffffffh;*)
END IP;

PROCEDURE FP(VAR INOUT left, right : CARDINAL32) [INLINE];
VAR
    temp        : CARDINAL32;
BEGIN
    (* final permutation *)

    right := right ROR 1;
        (*right := (right SHL 31) BOR (right SHR 1);*)
    temp := (left BXOR right) BAND 0aaaaaaaah;
    left := left BXOR temp;
    right := right BXOR temp;

    left := left ROR 1;
        (*left := (left SHL 31) BOR (left SHR 1);*)
    temp := ((left SHR 8) BXOR right) BAND 000ff00ffh;
    right := right BXOR temp;
    left := left BXOR (temp SHL 8);

    temp := ((left SHR 2) BXOR right) BAND 033333333h;
    right := right BXOR temp;
    left := left BXOR (temp SHL 2);

    temp := ((right SHR 16) BXOR left) BAND 00000ffffh;
    left := left BXOR temp;
    right := right BXOR (temp SHL 16);

    temp := ((right SHR 4) BXOR left) BAND 00f0f0f0fh;
    left := left BXOR temp;
    right := right BXOR (temp SHL 4);
END FP;

PROCEDURE ROUNDS(key : KeyArray;
                 VAR INOUT left, right : CARDINAL32) [INLINE];
VAR
    i           : CARDINAL;
    temp        : CARDINAL32;
    subs        : CARDINAL32;
    subs1       : CARDINAL32;
BEGIN
    (* en- or decryption rounds *)
    i := 0;
    REPEAT
        subs := right ROR 4;
            (*subs := (right SHL 28) BOR (right SHR 4);*)
        subs := subs BXOR key[i];
        subs1 := right BXOR key[i+1];
        temp :=          SP6[ subs         BAND 03fh];
        temp := temp BOR SP4[(subs SHR  8) BAND 03fh];
        temp := temp BOR SP2[(subs SHR 16) BAND 03fh];
        temp := temp BOR SP0[(subs SHR 24) BAND 03fh];

        temp := temp BOR SP7[ subs1         BAND 03fh];
        temp := temp BOR SP5[(subs1 SHR  8) BAND 03fh];
        temp := temp BOR SP3[(subs1 SHR 16) BAND 03fh];
        temp := temp BOR SP1[(subs1 SHR 24) BAND 03fh];
        left := left BXOR temp;

        subs := left ROR 4;
            (*subs := (left SHL 28) BOR (left SHR 4);*)
        subs := subs BXOR key[i+2];
        subs1 := left BXOR key[i+3];
        i := i + 4;
        temp :=          SP6[ subs         BAND 03fh];
        temp := temp BOR SP4[(subs SHR  8) BAND 03fh];
        temp := temp BOR SP2[(subs SHR 16) BAND 03fh];
        temp := temp BOR SP0[(subs SHR 24) BAND 03fh];

        temp := temp BOR SP7[ subs1         BAND 03fh];
        temp := temp BOR SP5[(subs1 SHR  8) BAND 03fh];
        temp := temp BOR SP3[(subs1 SHR 16) BAND 03fh];
        temp := temp BOR SP1[(subs1 SHR 24) BAND 03fh];
        right := right BXOR temp;
    UNTIL i > 28;
END ROUNDS;

PROCEDURE process(key : KeyArray; VAR INOUT dataL, dataR : CARDINAL32);
VAR
    left, right : CARDINAL32;
BEGIN
    left := dataL;
    right := dataR;

    IP(left, right);

    ROUNDS(key, left, right);

    FP(left, right);

    (* we still have a final swap to do *)
    dataL := right;
    dataR := left;
END process;

PROCEDURE process3(key1, key2, key3 : KeyArray; VAR INOUT dataL, dataR : CARDINAL32);
VAR
    left, right : CARDINAL32;
BEGIN
    left := dataL;
    right := dataR;

    IP(left, right);

    ROUNDS(key1, left, right);
    ROUNDS(key2, right, left);
    ROUNDS(key3, left, right);

    FP(left, right);

    (* we still have a final swap to do *)
    dataL := right;
    dataR := left;
END process3;

PROCEDURE EncryptECB(crypt : DES; input, output : ADDRESS; amount : CARDINAL);
VAR
    left, right : CARDINAL32;
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        REPEAT
            amount := amount - BlockSize;

            load(input, left, right);
            IF crypt^.triple THEN
                process3(crypt^.k1e, crypt^.k2e, crypt^.k3e, left, right);
            ELSE
                process(crypt^.k1e, left, right);
            END;
            store(left, right, output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        UNTIL amount = 0;
    END;
END EncryptECB;

PROCEDURE DecryptECB(crypt : DES; input, output : ADDRESS; amount : CARDINAL);
VAR
    left, right : CARDINAL32;
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        REPEAT
            amount := amount - BlockSize;

            load(input, left, right);
            IF crypt^.triple THEN
                process3(crypt^.k3d, crypt^.k2d, crypt^.k1d, left, right);
            ELSE
                process(crypt^.k1d, left, right);
            END;
            store(left, right, output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        UNTIL amount = 0;
    END;
END DecryptECB;

PROCEDURE EncryptCBC(crypt : DES; input, output : ADDRESS; amount : CARDINAL);
VAR
    ivl, ivr    : CARDINAL32;
    left, right : CARDINAL32;
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        load(ADR(crypt^.iv.bytes), ivl, ivr);

        REPEAT
            amount := amount - BlockSize;

            load(input, left, right);
            left := left BXOR ivl;
            right := right BXOR ivr;
            IF crypt^.triple THEN
                process3(crypt^.k1e, crypt^.k2e, crypt^.k3e, left, right);
            ELSE
                process(crypt^.k1e, left, right);
            END;
            store(left, right, output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);

            ivl := left;
            ivr := right;
        UNTIL amount = 0;

        store(ivl, ivr, ADR(crypt^.iv.bytes));
    END;
END EncryptCBC;

PROCEDURE DecryptCBC(crypt : DES; input, output : ADDRESS; amount : CARDINAL);
VAR
    ivl, ivr            : CARDINAL32;
    left, right         : CARDINAL32;
    saveL, saveR        : CARDINAL32;
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        load(ADR(crypt^.iv.bytes), ivl, ivr);

        REPEAT
            amount := amount - BlockSize;

            load(input, left, right);
            saveL := left;
            saveR := right;
            IF crypt^.triple THEN
                process3(crypt^.k3d, crypt^.k2d, crypt^.k1d, left, right);
            ELSE
                process(crypt^.k1d, left, right);
            END;
            store(left BXOR ivl, right BXOR ivr, output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);

            ivl := saveL;
            ivr := saveR;
        UNTIL amount = 0;

        store(ivl, ivr, ADR(crypt^.iv.bytes));
    END;
END DecryptCBC;

PROCEDURE EncryptCFB(crypt : DES; input, output : ADDRESS; amount : ADRCARD);
VAR
    i           : ADRCARD;
    inl, inr    : CARDINAL32;
    left, right : CARDINAL32;
    <*/PUSH/NOCHECK:U*>
    result      : ARRAY [0..BlockSize-1] OF BYTE;
    <*/POP*>
    inP, outP   : POINTER TO ARRAY [0..BlockSize-1] OF BYTE;
BEGIN
    load(ADR(crypt^.iv.bytes), left, right);

    REPEAT
        IF crypt^.triple THEN
            process3(crypt^.k1e, crypt^.k2e, crypt^.k3e, left, right);
        ELSE
            process(crypt^.k1e, left, right);
        END;

        IF amount >= BlockSize THEN
            (* full block *)
            amount := amount - BlockSize;

            load(input, inl, inr);
            left := left BXOR inl;
            right := right BXOR inr;
            store(left, right, output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        ELSE
            (* partial block *)

            store(left, right, ADR(result));
            inP := input;
            outP := output;
            FOR i := 0 TO amount-1 DO
                outP^[i] := inP^[i] BXOR result[i];
            END;
            amount := 0;
        END;
    UNTIL amount = 0;

    store(left, right, ADR(crypt^.iv.bytes));
END EncryptCFB;

PROCEDURE DecryptCFB(crypt : DES; input, output : ADDRESS; amount : ADRCARD);
VAR
    i           : ADRCARD;
    inl, inr    : CARDINAL32;
    left, right : CARDINAL32;
    <*/PUSH/NOCHECK:U*>
    result      : ARRAY [0..BlockSize-1] OF BYTE;
    <*/POP*>
    inP, outP   : POINTER TO ARRAY [0..BlockSize-1] OF BYTE;
BEGIN
    load(ADR(crypt^.iv.bytes), left, right);

    REPEAT
        IF crypt^.triple THEN
            process3(crypt^.k1e, crypt^.k2e, crypt^.k3e, left, right);
        ELSE
            process(crypt^.k1e, left, right);
        END;

        IF amount >= BlockSize THEN
            (* full block *)
            amount := amount - BlockSize;

            load(input, inl, inr);
            store(left BXOR inl, right BXOR inr, output);
            left := inl;
            right := inr;

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        ELSE
            (* partial block *)

            store(left, right, ADR(result));
            inP := input;
            outP := output;
            FOR i := 0 TO amount-1 DO
                outP^[i] := inP^[i] BXOR result[i];
            END;
            amount := 0;
        END;
    UNTIL amount = 0;

    store(left, right, ADR(crypt^.iv.bytes));
END DecryptCFB;

PROCEDURE SelfTest() : BOOLEAN;
TYPE
    tArray      = ARRAY [0..BlockSize-1] OF CARDINAL8;

    tAlign = (* force 4-byte alignment of our test buffers *)
    RECORD
    dummy       : CARDINAL32;
    input,
    result      : tArray;
    input1      : ARRAY [0..31] OF BYTE;
    result1     : ARRAY [0..31] OF BYTE;
    END;

CONST
    numTests    = 6;
    numEncrypt  = 5;

    keys : ARRAY [1..numTests] OF Key1 =
    {
     {001h, 001h, 001h, 001h, 001h, 001h, 001h, 001h},(* initial permutation*)
     {001h, 001h, 001h, 001h, 001h, 001h, 001h, 001h},(* inverse permutation *)
     {080h, 001h, 001h, 001h, 001h, 001h, 001h, 001h},(* key permutation *)
     {07Ch, 0A1h, 010h, 045h, 04Ah, 01Ah, 06Eh, 057h},(* sbox test*)
     {010h, 046h, 091h, 034h, 089h, 098h, 001h, 031h},(* data permutation *)
     (* from this point on are decrypt tests *)
     {080h, 001h, 001h, 001h, 001h, 001h, 001h, 001h} (* right shifts *)
    };

    inputs : ARRAY [1..numTests] OF tArray =
    {
     {095h, 0F8h, 0A5h, 0E5h, 0DDh, 031h, 0D9h, 000h},
     {080h, 000h, 000h, 000h, 000h, 000h, 000h, 000h},
     {0 BY 8},
     {001h, 0A1h, 0D6h, 0D0h, 039h, 077h, 067h, 042h},
     {0 BY 8},

     {095h, 0A8h, 0D7h, 028h, 013h, 0DAh, 0A9h, 04Dh}
    };

    outputs : ARRAY [1..numTests] OF tArray =
    {
     {080h, 000h, 000h, 000h, 000h, 000h, 000h, 000h},
     {095h, 0F8h, 0A5h, 0E5h, 0DDh, 031h, 0D9h, 000h},
     {095h, 0A8h, 0D7h, 028h, 013h, 0DAh, 0A9h, 04Dh},
     {069h, 00Fh, 05Bh, 00Dh, 09Ah, 026h, 093h, 09Bh},
     {088h, 0D5h, 05Eh, 054h, 0F5h, 04Ch, 097h, 0B4h},
     {0 BY 8}
    };

    keyCBC = Key3{
                 {01h, 23h, 45h, 67h, 89h, 0abh, 0cdh, 0efh},
                 {0F0h, 0E1h, 0D2h, 0C3h, 0B4h, 0A5h, 96h, 87h},
                 {0feh, 0dch, 0bah, 098h, 076h, 054h, 032h, 010h}
                };

    inputCBC : ARRAY [0..31] OF CARDINAL8 =
    {
     037h, 036h, 035h, 034h, 033h, 032h, 031h, 020h,
     04Eh, 06Fh, 077h, 020h, 069h, 073h, 020h, 074h,
     068h, 065h, 020h, 074h, 069h, 06Dh, 065h, 020h,
     066h, 06Fh, 072h, 020h, 000h, 000h, 000h, 000h
    };

    resultCBC : ARRAY [0..31] OF CARDINAL8 =
    {
     03Fh, 0E3h, 001h, 0C9h, 062h, 0ACh, 001h, 0D0h,
     022h, 013h, 076h, 03Ch, 01Ch, 0BDh, 04Ch, 0DCh,
     079h, 096h, 057h, 0C0h, 064h, 0ECh, 0F5h, 0D4h,
     01Ch, 067h, 038h, 012h, 0CFh, 0DEh, 096h, 075h
    };

    ivCBC = tArray{0FEh, 0DCh, 0BAh, 098h, 076h, 054h, 032h, 010h};


    keyCFB = Key1{01h, 23h, 45h, 67h, 89h, 0abh, 0cdh, 0efh};

    inputCFB : ARRAY [0..23] OF CARDINAL8 =
    {
     04eh, 06fh, 077h, 020h, 069h, 073h,
     020h, 074h, 068h, 065h, 020h, 074h,
     069h, 06dh, 065h, 020h, 066h, 06fh,
     072h, 020h, 061h, 06ch, 06ch, 020h
    };

    resultCFB : ARRAY [0..23] OF CARDINAL8 =
    {
     0F3h, 009h, 062h, 049h, 0C7h, 0F4h, 06Eh, 051h, 0A6h, 09Eh, 083h, 09Bh,
     01Ah, 092h, 0F7h, 084h, 003h, 046h, 071h, 033h, 089h, 08Eh, 0A6h, 022h
    };

    ivCFB = tArray{12h, 34h, 56h, 78h, 90h, 0abh, 0cdh, 0efh};

VAR
    data        : tAlign;
    k           : KeyArray;
    crypt       : DES;
    iv          : IV;
    ok          : BOOLEAN;
    i           : ADRCARD;

    PROCEDURE verify(a, b : ARRAY OF BYTE) : BOOLEAN;
    VAR
        i       : ADRCARD;
        highA   : ADRCARD;
    BEGIN
        highA := HIGH(a);
        FOR i := 0 TO highA DO
            IF a[i] <> b[i] THEN
                RETURN FALSE;
            END;
        END;
        RETURN TRUE;
    END verify;

    PROCEDURE encrypt(key : KeyArray;
                      input : ARRAY OF BYTE;
                      VAR OUT output : ARRAY OF BYTE);
    VAR
        left, right     : CARDINAL32;
    BEGIN
        load(ADR(input), left, right);
        process(key, left, right);
        store(left, right, ADR(output));
    END encrypt;

BEGIN
    ok := TRUE;

    (* single DES tests. tests the basic encryption engine.
       triple DES is multiple calls to that engine.
    *)

    FOR i := 1 TO numTests DO
        KeySetup(keys[i], k, i <= numEncrypt);
        data.input := inputs[i];
        encrypt(k, data.input, data.result);
        ok := ok AND verify(data.result, outputs[i]);
        IF i <= numEncrypt THEN
            (* may as well make sure the encrypt tests, decrypt properly *)
            KeySetup(keys[i], k, FALSE);
            encrypt(k, data.result, data.result);
            ok := ok AND verify(data.result, inputs[i]);
        END;
    END;

    crypt := Create(Key3{keys[1], keys[1], keys[1]});

    (* test ECB mode, with a single key we should get a single DES result *)

    data.input := inputs[1];
    EncryptECB(crypt, ADR(data.input), ADR(data.result), 8);
    ok := ok AND verify(data.result, outputs[1]);
    DecryptECB(crypt, ADR(data.result), ADR(data.result), 8);
    ok := ok AND verify(data.result, inputs[1]);

    (* test CBC *)

    KeySetup(keyCBC[1], crypt^.k1e, TRUE);
    KeySetup(keyCBC[2], crypt^.k2e, FALSE);
    KeySetup(keyCBC[3], crypt^.k3e, TRUE);
    KeySetup(keyCBC[1], crypt^.k1d, FALSE);
    KeySetup(keyCBC[2], crypt^.k2d, TRUE);
    KeySetup(keyCBC[3], crypt^.k3d, FALSE);
    iv.bytes[0..7] := ivCBC;
    data.input1[0..31] := inputCBC;

    ResetIV(crypt, iv);
    EncryptCBC(crypt, ADR(data.input1), ADR(data.result1), 16);
    EncryptCBC(crypt, ADR(data.input1[16]), ADR(data.result1[16]), 16);
    ok := ok AND verify(data.result1, resultCBC);
    ResetIV(crypt, iv);
    DecryptCBC(crypt, ADR(data.result1), ADR(data.result1), 16);
    DecryptCBC(crypt, ADR(data.result1[16]), ADR(data.result1[16]), 16);
    ok := ok AND verify(data.result1, data.input1);

    (* test CFB *)

    KeySetup(keyCFB, crypt^.k1e, TRUE);
    KeySetup(keyCFB, crypt^.k2e, FALSE);
    KeySetup(keyCFB, crypt^.k3e, TRUE);
    KeySetup(keyCFB, crypt^.k1d, FALSE);
    KeySetup(keyCFB, crypt^.k2d, TRUE);
    KeySetup(keyCFB, crypt^.k3d, FALSE);
    iv.bytes[0..7] := ivCFB;
    data.input1[0..23] := inputCFB;

    ResetIV(crypt, iv);
    EncryptCFB(crypt, ADR(data.input1), ADR(data.result1), 16);
    EncryptCFB(crypt, ADR(data.input1[16]), ADR(data.result1[16]), 8);
    ok := ok AND verify(data.result1[0..23], resultCFB);
    ResetIV(crypt, iv);
    DecryptCFB(crypt, ADR(data.result1), ADR(data.result1), 16);
    DecryptCFB(crypt, ADR(data.result1[16]), ADR(data.result1[16]), 8);
    ok := ok AND verify(data.result1[0..23], data.input1);

    Destroy(crypt);
    RETURN ok;
END SelfTest;

END DES.
