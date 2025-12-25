IMPLEMENTATION MODULE SHA512;
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
    WordType            = CARDINAL64;

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

    SHA512rec =
        RECORD
        digest          : DigestType;
        length          : CARDINAL64;(* SHA-512 specifices 2**128 bits of data
                                        capability. we blow that off and only support
                                        2**64 bits, which is an HUGE amount of data.
                                     *)
        last            : CARDINAL32;
        pad             : CARDINAL32;

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

    SHA512      = POINTER TO SHA512rec;

CONST
    K : ARRAY [0..79] OF WordType =
        {
        0428a2f98d728ae22h, 07137449123ef65cdh,
        0b5c0fbcfec4d3b2fh, 0e9b5dba58189dbbch,
        03956c25bf348b538h, 059f111f1b605d019h,
        0923f82a4af194f9bh, 0ab1c5ed5da6d8118h,
        0d807aa98a3030242h, 012835b0145706fbeh,
        0243185be4ee4b28ch, 0550c7dc3d5ffb4e2h,
        072be5d74f27b896fh, 080deb1fe3b1696b1h,
        09bdc06a725c71235h, 0c19bf174cf692694h,
        0e49b69c19ef14ad2h, 0efbe4786384f25e3h,
        00fc19dc68b8cd5b5h, 0240ca1cc77ac9c65h,
        02de92c6f592b0275h, 04a7484aa6ea6e483h,
        05cb0a9dcbd41fbd4h, 076f988da831153b5h,
        0983e5152ee66dfabh, 0a831c66d2db43210h,
        0b00327c898fb213fh, 0bf597fc7beef0ee4h,
        0c6e00bf33da88fc2h, 0d5a79147930aa725h,
        006ca6351e003826fh, 0142929670a0e6e70h,
        027b70a8546d22ffch, 02e1b21385c26c926h,
        04d2c6dfc5ac42aedh, 053380d139d95b3dfh,
        0650a73548baf63deh, 0766a0abb3c77b2a8h,
        081c2c92e47edaee6h, 092722c851482353bh,
        0a2bfe8a14cf10364h, 0a81a664bbc423001h,
        0c24b8b70d0f89791h, 0c76c51a30654be30h,
        0d192e819d6ef5218h, 0d69906245565a910h,
        0f40e35855771202ah, 0106aa07032bbd1b8h,
        019a4c116b8d2d0c8h, 01e376c085141ab53h,
        02748774cdf8eeb99h, 034b0bcb5e19b48a8h,
        0391c0cb3c5c95a63h, 04ed8aa4ae3418acbh,
        05b9cca4f7763e373h, 0682e6ff3d6b2b8a3h,
        0748f82ee5defb2fch, 078a5636f43172f60h,
        084c87814a1f0ab72h, 08cc702081a6439ech,
        090befffa23631e28h, 0a4506cebde82bde9h,
        0bef9a3f7b2c67915h, 0c67178f2e372532bh,
        0ca273eceea26619ch, 0d186b8c721c0c207h,
        0eada7dd6cde0eb1eh, 0f57d4f7fee6ed178h,
        006f067aa72176fbah, 00a637dc5a2c898a6h,
        0113f9804bef90daeh, 01b710b35131c471bh,
        028db77f523047d84h, 032caab7b40c72493h,
        03c9ebe0a15c9bebch, 0431d67c49c100d4ch,
        04cc5d4becb3e42b6h, 0597f299cfc657e2ah,
        05fcb6fab3ad6faech, 06c44198c4a475817h
        };

PROCEDURE Create() : SHA512;
VAR
    hash        : SHA512;
BEGIN
    NEW(hash);
    hash^.heap := GetHeap();
    Reset(hash);
    RETURN hash;
END Create;

PROCEDURE Destroy(VAR OUT hash : SHA512);
BEGIN
    DeallocateEx(hash, SIZE(hash^), hash^.heap);
END Destroy;

PROCEDURE Reset(hash : SHA512);
BEGIN
    hash^.last := 0;
    hash^.length := 0;
    hash^.finalized := FALSE;
    hash^.digest[0] := 06a09e667f3bcc908h;
    hash^.digest[1] := 0bb67ae8584caa73bh;
    hash^.digest[2] := 03c6ef372fe94f82bh;
    hash^.digest[3] := 0a54ff53a5f1d36f1h;
    hash^.digest[4] := 0510e527fade682d1h;
    hash^.digest[5] := 09b05688c2b3e6c1fh;
    hash^.digest[6] := 01f83d9abfb41bd6bh;
    hash^.digest[7] := 05be0cd19137e2179h;
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

PROCEDURE rotr(x : WordType; bits : CARDINAL) : WordType [INLINE];
(* the compiler long rotate sucks on IA-32. it loops.
   on RISC it is fine.
*)
BEGIN
    RETURN (x SHR bits) BOR (x SHL (64-bits));
END rotr;

PROCEDURE S0(x : WordType) : WordType [INLINE];
BEGIN
    RETURN rotr(x, 28) BXOR rotr(x, 34) BXOR rotr(x, 39);
END S0;

PROCEDURE S1(x : WordType) : WordType [INLINE];
BEGIN
    RETURN rotr(x, 14) BXOR rotr(x, 18) BXOR rotr(x, 41);
END S1;

PROCEDURE s0(x : WordType) : WordType [INLINE];
BEGIN
    RETURN (rotr(x, 1) BXOR rotr(x, 8) BXOR (x SHR 7));
END s0;

PROCEDURE s1(x : WordType) : WordType [INLINE];
BEGIN
    RETURN (rotr(x, 19) BXOR rotr(x, 61) BXOR (x SHR 6));
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
            i, j : ADRCARD) [INLINE];
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
PROCEDURE HashBuffer(hash : SHA512);
VAR
    <*/PUSH/NOCHECK:U*>
    W   : BufferType;
    <*/POP*>
    D   : BufferPointer;
    T   : DigestType;
    j   : CARDINAL;
BEGIN
    T := hash^.digest;

    (* 80 operations, partially loop unrolled *)

    D := ADR(hash^.wBuffer);(*helps parameter inlining. smaller code gen*)
    FOR j := 0 TO 64 BY 16 DO
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

PROCEDURE HashBytes(hash : SHA512; data : ADDRESS; amount : CARDINAL);
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
PROCEDURE Finalize(hash : SHA512);

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
    i   : ADRCARD;
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

    WHILE (hash^.last REM 128) <> 112 DO  (* cLast REM 896 = 1024 in bits *)
        IF hash^.last = BufferBytes THEN
            HashBuffer(hash);
            hash^.last := 0;
        END;
        hash^.bBuffer[hash^.last] := 0;
        INC(hash^.last);
    END;

    (* we do not support 2**128 bits of data, only 2**64 bits.
       so fill with leading zeros for the first eight bytes of the
       128-bit bitcount.
    *)
    FOR i := 0 TO 7 DO
        hash^.bBuffer[hash^.last] := 0;
        INC(hash^.last);
    END;

    (* put the bitcount into the buffer in big endian format.*)
    FOR i := cFIRST TO cLAST BY nSTEP DO
        hash^.bBuffer[hash^.last] := hash^.length:CARD64[i];
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

PROCEDURE GetHash(hash : SHA512; VAR OUT data : ARRAY OF BYTE);
BEGIN
    IF NOT hash^.finalized THEN
        Finalize(hash);
    END;
    data := hash^.bytes;
END GetHash;

PROCEDURE GetString(hash : SHA512; VAR OUT str : ARRAY OF CHAR);
CONST
    hexDig      : ARRAY [0..15] OF CHAR = "0123456789ABCDEF";
VAR
    i, j        : ADRCARD;
    highStr     : ADRCARD;
BEGIN
    IF NOT hash^.finalized THEN
        Finalize(hash);
    END;

    str := "";
    IF HIGH(str) >= 64*2-1 THEN
        j := 0;
        FOR i := 0 TO 19 DO
            str[j] := hexDig[hash^.bytes[i] / 16];
            str[j+1] := hexDig[hash^.bytes[i] REM 16];
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
    input2      = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn"+
                  "hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu";

    output1     : ARRAY [0..DigestBytes-1] OF CARDINAL8 =
        {0ddh, 0afh, 035h, 0a1h, 093h, 061h, 07ah, 0bah,
         0cch, 041h, 073h, 049h, 0aeh, 020h, 041h, 031h,
         012h, 0e6h, 0fah, 04eh, 089h, 0a9h, 07eh, 0a2h,
         00ah, 09eh, 0eeh, 0e6h, 04bh, 055h, 0d3h, 09ah,
         021h, 092h, 099h, 02ah, 027h, 04fh, 0c1h, 0a8h,
         036h, 0bah, 03ch, 023h, 0a3h, 0feh, 0ebh, 0bdh,
         045h, 04dh, 044h, 023h, 064h, 03ch, 0e8h, 00eh,
         02ah, 09ah, 0c9h, 04fh, 0a5h, 04ch, 0a4h, 09fh
        };

    output2     : ARRAY [0..DigestBytes-1] OF CARDINAL8 =
        {08eh, 095h, 09bh, 075h, 0dah, 0e3h, 013h, 0dah,
         08ch, 0f4h, 0f7h, 028h, 014h, 0fch, 014h, 03fh,
         08fh, 077h, 079h, 0c6h, 0ebh, 09fh, 07fh, 0a1h,
         072h, 099h, 0aeh, 0adh, 0b6h, 088h, 090h, 018h,
         050h, 01dh, 028h, 09eh, 049h, 000h, 0f7h, 0e4h,
         033h, 01bh, 099h, 0deh, 0c4h, 0b5h, 043h, 03ah,
         0c7h, 0d3h, 029h, 0eeh, 0b6h, 0ddh, 026h, 054h,
         05eh, 096h, 0e5h, 05bh, 087h, 04bh, 0e9h, 009h
        };

VAR
    hash        : SHA512;
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

END SHA512.
