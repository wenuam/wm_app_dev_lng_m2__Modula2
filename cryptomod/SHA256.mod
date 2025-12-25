IMPLEMENTATION MODULE SHA256;
<*/OPTIMIZE:T*>
(* this code is based on the SHA-2 implementation in Crypto++ 4.2

// sha.cpp - modified by Wei Dai from Steve Reid's public domain sha1.c

// Steve Reid implemented SHA-1. Wei Dai implemented SHA-2.
// Both are in the public domain.
*)

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, BIGENDIAN, ADR, ADDADR;

FROM ExStorage IMPORT
    HeapInfoPointer,
    ALLOCATE, GetHeap, DeallocateEx;

TYPE
    WordType            = CARDINAL32;

CONST
    WordSize            = SIZE(WordType);
    BufferWords         = 16;
    BufferBytes         = BufferWords * WordSize;

    DigestWords         = 8;
    DigestBytes         = DigestWords * WordSize;

TYPE
    DigestType          = ARRAY [0..DigestWords-1] OF WordType;
    BufferType          = ARRAY [0..BufferWords-1] OF WordType;
    BufferPointer       = POINTER TO BufferType;

    SHA256rec =
        RECORD
        digest          : DigestType;
        length          : CARDINAL64;
        last            : CARDINAL32;

        (* buffer must be aligned for word access,
           and the variant parts must overlap perfectly.
         *)
        CASE : CARDINAL OF
        0: bBuffer : ARRAY [0..BufferBytes-1] OF BYTE; |
        1: wBuffer : BufferType;
        ELSE
        END;

        (* buffer must be aligned for word access *)
        bytes           : ARRAY [0..DigestBytes-1] OF CARDINAL8;

        heap            : HeapInfoPointer;
        finalized       : BOOLEAN;
        END;

    SHA256      = POINTER TO SHA256rec;

CONST
    K : ARRAY [0..63] OF WordType =
        {
        0428a2f98h, 071374491h, 0b5c0fbcfh, 0e9b5dba5h,
        03956c25bh, 059f111f1h, 0923f82a4h, 0ab1c5ed5h,
        0d807aa98h, 012835b01h, 0243185beh, 0550c7dc3h,
        072be5d74h, 080deb1feh, 09bdc06a7h, 0c19bf174h,
        0e49b69c1h, 0efbe4786h, 00fc19dc6h, 0240ca1cch,
        02de92c6fh, 04a7484aah, 05cb0a9dch, 076f988dah,
        0983e5152h, 0a831c66dh, 0b00327c8h, 0bf597fc7h,
        0c6e00bf3h, 0d5a79147h, 006ca6351h, 014292967h,
        027b70a85h, 02e1b2138h, 04d2c6dfch, 053380d13h,
        0650a7354h, 0766a0abbh, 081c2c92eh, 092722c85h,
        0a2bfe8a1h, 0a81a664bh, 0c24b8b70h, 0c76c51a3h,
        0d192e819h, 0d6990624h, 0f40e3585h, 0106aa070h,
        019a4c116h, 01e376c08h, 02748774ch, 034b0bcb5h,
        0391c0cb3h, 04ed8aa4ah, 05b9cca4fh, 0682e6ff3h,
        0748f82eeh, 078a5636fh, 084c87814h, 08cc70208h,
        090befffah, 0a4506cebh, 0bef9a3f7h, 0c67178f2h
        };

PROCEDURE Create() : SHA256;
VAR
    hash        : SHA256;
BEGIN
    NEW(hash);
    hash^.heap := GetHeap();
    Reset(hash);
    RETURN hash;
END Create;

PROCEDURE Destroy(VAR OUT hash : SHA256);
BEGIN
    DeallocateEx(hash, SIZE(hash^), hash^.heap);
END Destroy;

PROCEDURE Reset(hash : SHA256);
BEGIN
    hash^.last := 0;
    hash^.length := 0;
    hash^.finalized := FALSE;
    hash^.digest[0] := 06a09e667h;
    hash^.digest[1] := 0bb67ae85h;
    hash^.digest[2] := 03c6ef372h;
    hash^.digest[3] := 0a54ff53ah;
    hash^.digest[4] := 0510e527fh;
    hash^.digest[5] := 09b05688ch;
    hash^.digest[6] := 01f83d9abh;
    hash^.digest[7] := 05be0cd19h;
END Reset;

(* use VARs on params that you do not need a copy and are not passed an expression.
   this speeds up compilation.
   otherwise assignments are generated and then optimized via propagation.
*)
PROCEDURE Ch(x, y, z : WordType) : WordType [INLINE];
BEGIN
    RETURN z BXOR (x BAND (y BXOR z));
END Ch;

PROCEDURE Maj(x, y, z : WordType) : WordType [INLINE];
BEGIN
    RETURN (x BAND y) BOR (z BAND (x BOR y));
END Maj;

PROCEDURE S0(x : WordType) : WordType [INLINE];
BEGIN
    RETURN (x ROR 2) BXOR (x ROR 13) BXOR (x ROR 22);
END S0;

PROCEDURE S1(x : WordType) : WordType [INLINE];
BEGIN
    RETURN (x ROR 6) BXOR (x ROR 11) BXOR (x ROR 25);
END S1;

PROCEDURE s0(x : WordType) : WordType [INLINE];
BEGIN
    RETURN ((x ROR 7) BXOR (x ROR 18) BXOR (x SHR 3));
END s0;

PROCEDURE s1(x : WordType) : WordType [INLINE];
BEGIN
    RETURN ((x ROR 17) BXOR (x ROR 19) BXOR (x SHR 10));
END s1;

<*/PUSH/NOCHECK:O*>
(* these funky word access procs, a-h, with their array subscripting
   eliminate the need to do the word rotation assignments in the algorithm.
   The subscripts fall out to compile time offsets due to the inlining.
*)
PROCEDURE a(VAR INOUT T : DigestType; i : ADRCARD) : WordType [INLINE];
BEGIN
    RETURN T[(0-i) BAND 7];
END a;

PROCEDURE b(VAR INOUT T : DigestType; i : ADRCARD) : WordType [INLINE];
BEGIN
    RETURN T[(1-i) BAND 7];
END b;

PROCEDURE c(VAR INOUT T : DigestType; i : ADRCARD) : WordType [INLINE];
BEGIN
    RETURN T[(2-i) BAND 7];
END c;

PROCEDURE d(VAR INOUT T : DigestType; i : ADRCARD) : WordType [INLINE];
BEGIN
    RETURN T[(3-i) BAND 7];
END d;

PROCEDURE e(VAR INOUT T : DigestType; i : ADRCARD) : WordType [INLINE];
BEGIN
    RETURN T[(4-i) BAND 7];
END e;

PROCEDURE f(VAR INOUT T : DigestType; i : ADRCARD) : WordType [INLINE];
BEGIN
    RETURN T[(5-i) BAND 7];
END f;

PROCEDURE g(VAR INOUT T : DigestType; i : ADRCARD) : WordType [INLINE];
BEGIN
    RETURN T[(6-i) BAND 7];
END g;

PROCEDURE h(VAR INOUT T : DigestType; i : ADRCARD) : WordType [INLINE];
BEGIN
    RETURN T[(7-i) BAND 7];
END h;

PROCEDURE R(VAR INOUT data : BufferPointer;
            VAR INOUT W : BufferType;
            VAR INOUT T : DigestType;
            i : ADRCARD;
            j : ADRCARD) [INLINE];
VAR
    T1  : WordType;
BEGIN
    (* this if statement can be optimized away, but it results in a bunch more code,
       for only a small speed increase. not really worth it.
       our code is already pretty big with the unrolling we already do.
    *)
    IF j > 0 THEN
        W[i] := W[i] +
                s1(W[(i-2) BAND 15])+
                W[(i-7) BAND 15]+
                s0(W[(i-15) BAND 15]);
    ELSE
        W[i] := BIGENDIAN(data^[i]);
    END;

    T1 := h(T, i) +
          S1(e(T, i)) +
          Ch(e(T, i), f(T, i), g(T, i)) +
          K[i+j] +
          W[i];

    T[(3-i) BAND 7](*d(i)*) := T1 + d(T, i);
    T[(7-i) BAND 7](*h(i)*) := T1 + S0(a(T, i)) + Maj(a(T, i), b(T, i), c(T, i));
END R;
<*/POP*>

<*/GROUPLIBPROCS:Y*>
PROCEDURE HashBuffer(hash : SHA256);
VAR
    <*/PUSH/NOCHECK:U*>
    W   : BufferType;
    <*/POP*>
    D   : BufferPointer;
    T   : DigestType;
    j   : CARDINAL;
BEGIN
    T := hash^.digest;

    (* 64 operations, partially loop unrolled *)

    D := ADR(hash^.wBuffer);(*helps parameter inlining. smaller code gen*)
    FOR j := 0 TO 48 BY 16 DO
        R(D, W, T, 0, j);
        R(D, W, T, 1, j);
        R(D, W, T, 2, j);
        R(D, W, T, 3, j);
        R(D, W, T, 4, j);
        R(D, W, T, 5, j);
        R(D, W, T, 6, j);
        R(D, W, T, 7, j);
        R(D, W, T, 8, j);
        R(D, W, T, 9, j);
        R(D, W, T, 10, j);
        R(D, W, T, 11, j);
        R(D, W, T, 12, j);
        R(D, W, T, 13, j);
        R(D, W, T, 14, j);
        R(D, W, T, 15, j);
    END;

    (* Add the working digest back into the previous digest *)

    hash^.digest[0] := hash^.digest[0] + a(T, 0);
    hash^.digest[1] := hash^.digest[1] + b(T, 0);
    hash^.digest[2] := hash^.digest[2] + c(T, 0);
    hash^.digest[3] := hash^.digest[3] + d(T, 0);
    hash^.digest[4] := hash^.digest[4] + e(T, 0);
    hash^.digest[5] := hash^.digest[5] + f(T, 0);
    hash^.digest[6] := hash^.digest[6] + g(T, 0);
    hash^.digest[7] := hash^.digest[7] + h(T, 0);
END HashBuffer;

PROCEDURE HashBytes(hash : SHA256; data : ADDRESS; amount : CARDINAL);
VAR
    bp          : ADRCARD;
    ptrB        : POINTER TO ARRAY [0..7] OF BYTE;
BEGIN
    IF NOT hash^.finalized THEN
        ptrB := data;

        bp := hash^.last;
        LOOP
            IF amount > 0 THEN
                IF (bp+8 <= BufferBytes) AND
                   ((bp REM 4) = 0) AND
                   (amount >= 8)
                THEN
                    (* all this saves memory write operations.
                       also helps processor data prefetch.
                    *)
                    hash^.bBuffer[bp]:CARDINAL32 :=
                        %IF LittleEndian %THEN
                             ORD(ptrB^[0]  )       BOR
                            (ORD(ptrB^[1]) SHL 8)  BOR
                            (ORD(ptrB^[2]) SHL 16) BOR
                            (ORD(ptrB^[3]) SHL 24);
                        %ELSE
                            (ORD(ptrB^[0]) SHL 24) BOR
                            (ORD(ptrB^[1]) SHL 16) BOR
                            (ORD(ptrB^[2]) SHL  8) BOR
                             ORD(ptrB^[3]);
                        %END
                    hash^.bBuffer[bp+4]:CARDINAL32 :=
                        %IF LittleEndian %THEN
                             ORD(ptrB^[4])         BOR
                            (ORD(ptrB^[5]) SHL 8)  BOR
                            (ORD(ptrB^[6]) SHL 16) BOR
                            (ORD(ptrB^[7]) SHL 24);
                        %ELSE
                            (ORD(ptrB^[4]) SHL 24) BOR
                            (ORD(ptrB^[5]) SHL 16) BOR
                            (ORD(ptrB^[6]) SHL  8) BOR
                             ORD(ptrB^[7]);
                        %END

                    bp := bp + 8;
                    amount := amount - 8;
                    ptrB := ADDADR(ptrB, 8);
                    INC(hash^.length, 8*8); (* count bits *)

                ELSIF (bp+4 <= BufferBytes) AND
                      ((bp REM 4) = 0) AND
                      (amount >= 4)
                THEN
                    (* all this saves memory write operations.
                       also helps processor data prefetch.
                    *)
                    hash^.bBuffer[bp]:CARDINAL32 :=
                        %IF LittleEndian %THEN
                             ORD(ptrB^[0])         BOR
                            (ORD(ptrB^[1]) SHL 8)  BOR
                            (ORD(ptrB^[2]) SHL 16) BOR
                            (ORD(ptrB^[3]) SHL 24);
                        %ELSE
                            (ORD(ptrB^[0]) SHL 24) BOR
                            (ORD(ptrB^[1]) SHL 16) BOR
                            (ORD(ptrB^[2]) SHL  8) BOR
                             ORD(ptrB^[3]);
                        %END
                    bp := bp + 4;
                    amount := amount - 4;
                    ptrB := ADDADR(ptrB, 4);
                    INC(hash^.length, 4*8); (* count bits *)

                ELSIF bp < BufferBytes THEN
                    hash^.bBuffer[bp] := ptrB^[0];
                    ptrB := ADDADR(ptrB, 1);
                    INC(bp);
                    DEC(amount);
                    INC(hash^.length, 8); (* count bits *)

                ELSE
                    hash^.last := bp;
                    HashBuffer(hash);
                    bp := 0;
                END;
            ELSE
                EXIT;
            END;
        END;
        hash^.last := bp;
    END;
END HashBytes;

<*/GROUPLIBPROCS:N*>(*will be grouped with above*)
PROCEDURE Finalize(hash : SHA256);

%IF LittleEndian %THEN
CONST
    cFIRST      = 7;
    cLAST       = 0;
    nSTEP       = -1;
%ELSE
CONST
    cFIRST      = 0;
    cLAST       = 7;
    nSTEP       = 1;
%END

TYPE
    CARD64      = ARRAY [0..7] OF BYTE;

VAR
    index       : ADRCARD;
BEGIN
    IF hash^.finalized THEN
        RETURN;
    END;
    hash^.finalized := TRUE;

    IF hash^.last = BufferBytes THEN
        HashBuffer(hash);
        hash^.last := 0;
    END;

    hash^.bBuffer[hash^.last] := 80h;
    INC(hash^.last);

    WHILE (hash^.last REM 64) <> 56 DO  (* cLast REM 512 = 448 in bits *)
        IF hash^.last = BufferBytes THEN
            HashBuffer(hash);
            hash^.last := 0;
        END;
        hash^.bBuffer[hash^.last] := 0;
        INC(hash^.last);
    END;

    (* put the bitcount into the buffer in big endian format.*)
    FOR index := cFIRST TO cLAST BY nSTEP DO
        hash^.bBuffer[hash^.last] := hash^.length:CARD64[index];
        INC(hash^.last);
    END;

    HashBuffer(hash);

    (* save a byte ordered result *)

    hash^.bytes:DigestType[0] := BIGENDIAN(hash^.digest[0]);
    hash^.bytes:DigestType[1] := BIGENDIAN(hash^.digest[1]);
    hash^.bytes:DigestType[2] := BIGENDIAN(hash^.digest[2]);
    hash^.bytes:DigestType[3] := BIGENDIAN(hash^.digest[3]);
    hash^.bytes:DigestType[4] := BIGENDIAN(hash^.digest[4]);
    hash^.bytes:DigestType[5] := BIGENDIAN(hash^.digest[5]);
    hash^.bytes:DigestType[6] := BIGENDIAN(hash^.digest[6]);
    hash^.bytes:DigestType[7] := BIGENDIAN(hash^.digest[7]);
END Finalize;

PROCEDURE GetHash(hash : SHA256; VAR OUT data : ARRAY OF BYTE);
BEGIN
    IF NOT hash^.finalized THEN
        Finalize(hash);
    END;
    data := hash^.bytes;
END GetHash;

PROCEDURE GetString(hash : SHA256; VAR OUT str : ARRAY OF CHAR);
CONST
    HexDig      : ARRAY [0..15] OF CHAR = "0123456789ABCDEF";
VAR
    i, j        : ADRCARD;
    highStr     : ADRCARD;
BEGIN
    IF NOT hash^.finalized THEN
        Finalize(hash);
    END;

    str := "";
    IF HIGH(str) >= 32*2-1 THEN
        j := 0;
        FOR i := 0 TO 19 DO
            str[j] := HexDig[hash^.bytes[i] / 16];
            str[j+1] := HexDig[hash^.bytes[i] REM 16];
            j := j + 2;
        END;
        highStr := HIGH(str);
        IF j <= highStr THEN
            str[j] := '';
        END;
    END;
END GetString;

PROCEDURE SelfTest() : BOOLEAN;
CONST
    (* official test vectors taken from the FIPS document *)
    input1      = "abc";
    input2      = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";

    output1     : ARRAY [0..DigestBytes-1] OF CARDINAL8 =
        {0bah, 078h, 016h, 0bfh, 08fh, 001h, 0cfh, 0eah,
         041h, 041h, 040h, 0deh, 05dh, 0aeh, 022h, 023h,
         0b0h, 003h, 061h, 0a3h, 096h, 017h, 07ah, 09ch,
         0b4h, 010h, 0ffh, 061h, 0f2h, 000h, 015h, 0adh};

    output2     : ARRAY [0..DigestBytes-1] OF CARDINAL8 =
        {024h, 08dh, 06ah, 061h, 0d2h, 006h, 038h, 0b8h,
         0e5h, 0c0h, 026h, 093h, 00ch, 03eh, 060h, 039h,
         0a3h, 03ch, 0e4h, 059h, 064h, 0ffh, 021h, 067h,
         0f6h, 0ech, 0edh, 0d4h, 019h, 0dbh, 006h, 0c1h};
VAR
    hash        : SHA256;
    output      : ARRAY [0..DigestBytes-1] OF CARDINAL8;
    ok          : BOOLEAN;

    PROCEDURE verify(d1, d2 : ARRAY OF CARDINAL8) : BOOLEAN;
    VAR
        i       : ADRCARD;
        highD1  : ADRCARD;
    BEGIN
        highD1 := HIGH(d1);
        FOR i := 0 TO highD1 DO
            IF d1[i] <> d2[i] THEN
                RETURN FALSE;
            END;
        END;
        RETURN TRUE;
    END verify;

BEGIN
    ok := FALSE;

    hash := Create();
    HashBytes(hash, ADR(input1), LENGTH(input1));
    GetHash(hash, output);
    IF verify(output, output1) THEN
        Reset(hash);
        HashBytes(hash, ADR(input2), LENGTH(input2));
        GetHash(hash, output);
        IF verify(output, output2) THEN
            ok := TRUE;
        END;
    END;
    Destroy(hash);

    RETURN ok;
END SelfTest;

END SHA256.
