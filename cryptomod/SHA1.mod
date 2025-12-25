(*
Name:     SHA1
Creation: 29-01-2001
LastEdit: 11-05-2001
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:  NIST Secure Hash Algorith SHA1, based on Netwerkbeveiliging en Cryptografie, pp323-328.
          RFC 3174

"abc"
A9 99 3E 36 47 06 81 6A BA 3E 25 71 78 50 C2 6C 9C D0 D8 9D

"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
84 98 3E 44 1C 3B D2 6E BA AE 4A A1 F9 51 29 E5 E5 46 70 F1

*)

(* adapted by ADW Software, 2009 *)
(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 2009                                  *)
(*                         by ADW Software                                 *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE SHA1;
<*/OPTIMIZE:T*>

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, BIGENDIAN, ADR, ADDADR;
%IF LittleEndian %THEN
FROM SYSTEM IMPORT
    SWAPENDIAN;
%END

FROM ExStorage IMPORT
    HeapInfoPointer,
    ALLOCATE, GetHeap, DeallocateEx;

CONST
    cINITA      = 067452301H;
    cINITB      = 0EFCDAB89H;
    cINITC      = 098BADCFEH;
    cINITD      = 010325476H;
    cINITE      = 0C3D2E1F0H;

    cBUFFER     = 64;
    cROUNDS     = 4;  (* 4 rounds *)

TYPE
    SHA1rec =
        RECORD
        a, b, c, d, e   : CARDINAL32;
        last            : CARDINAL32;
        length          : CARDINAL64;

        (* buffer must be 4-byte aligned,
           and the variant parts must overlap perfectly.
         *)
        CASE : CARDINAL OF
        0: btBuffer : ARRAY [0..cBUFFER-1] OF BYTE; |
        1: cBuffer : ARRAY [0..(cBUFFER/4)-1] OF CARDINAL32;
        ELSE
        END;

        (*must be 4 byte aligned*)
        bytes           : ARRAY [0..19] OF CARDINAL8;

        heap            : HeapInfoPointer;
        finalized       : BOOLEAN;
        END;

    SHA1        = POINTER TO SHA1rec;

    tROUNDS     = CARDINAL[0..cROUNDS-1];

    MAGIC       = ARRAY [0..3] OF CARDINAL32;

CONST
    cK          = MAGIC{05A827999H,
                        06ED9EBA1H,
                        08F1BBCDCH,
                        0CA62C1D6H};

PROCEDURE Create() : SHA1;
VAR
    hash        : SHA1;
BEGIN
    NEW(hash);
    hash^.heap := GetHeap();
    Reset(hash);
    RETURN hash;
END Create;

PROCEDURE Destroy(VAR OUT hash : SHA1);
BEGIN
    DeallocateEx(hash, SIZE(hash^), hash^.heap);
END Destroy;

PROCEDURE Reset(hash : SHA1);
BEGIN
    hash^.last := 0;
    hash^.length := 0;
    hash^.a := cINITA;
    hash^.b := cINITB;
    hash^.c := cINITC;
    hash^.d := cINITD;
    hash^.e := cINITE;
    hash^.finalized := FALSE;
END Reset;

(* use VARs on params that you do not need a copy and are not passed an expression.
   this speeds up compilation.
   otherwise assignments are generated and then optimized via propagation.
*)
PROCEDURE f(VAR INOUT B, C, D : CARDINAL32; round : tROUNDS) : CARDINAL32 [INLINE];
BEGIN
    CASE round OF
    0:(*f1*)
        (*RETURN ((B BAND C) BOR ((BNOT B) BAND D));*)
        RETURN ((B BAND C) BOR (D BAND (BNOT B)));
    |
    1:(*f2*)
        RETURN (B BXOR C BXOR D);
    |
    2:(*f3*)
        RETURN ((B BAND C) BOR (B BAND D) BOR (C BAND D));
    |
    3:(*f4*)
        RETURN (B BXOR C BXOR D);
    END;
    RETURN 0;
END f;

(* having two W and hence two compute procedures just speeds up compilation
   by saving the optimizer from throwing out unreachable code since the IF
   in a single W procedure would always evaluate to a constant.
*)
PROCEDURE W1(VAR INOUT hash : SHA1; step : ADRCARD) : CARDINAL32 [INLINE];
VAR
    word        : CARDINAL32;
BEGIN
    %IF LittleEndian %THEN
        word := SWAPENDIAN(hash^.cBuffer[step]);
        hash^.cBuffer[step] := word;
    %ELSE
        word := hash^.cBuffer[step];
    %END
    RETURN word;
END W1;

PROCEDURE W(VAR INOUT hash : SHA1; step : ADRCARD) : CARDINAL32 [INLINE];
VAR
    word        : CARDINAL32;
BEGIN
    word := hash^.cBuffer[step REM 16] BXOR
                hash^.cBuffer[(step-14) REM 16] BXOR
                hash^.cBuffer[(step-8) REM 16] BXOR
                hash^.cBuffer[(step-3) REM 16];
    word := word ROL 1;

    hash^.cBuffer[step REM 16] := word;
    RETURN word;
END W;

PROCEDURE compute1(VAR INOUT hash : SHA1;
                   round : tROUNDS;
                   step : CARDINAL;
                   VAR INOUT a, b, c, d, e, a1 : CARDINAL32) [INLINE];
BEGIN
    <*/PUSH/NOCHECK:O*>
    a1 := e + cK[round] + f(b, c, d, round);
    a1 := a1 + (a ROL 5);
    a1 := a1 + W1(hash, step);
    <*/POP*>

    e := d;
    d := c;
    c := b ROL 30;
    b := a;
    a := a1;
END compute1;

PROCEDURE compute(VAR INOUT hash : SHA1;
                  round : tROUNDS;
                  step : CARDINAL;
                  VAR INOUT a, b, c, d, e, a1 : CARDINAL32) [INLINE];
BEGIN
    <*/PUSH/NOCHECK:O*>
    a1 := e + cK[round] + f(b, c, d, round);
    a1 := a1 + (a ROL 5);
    a1 := a1 + W(hash, step);
    <*/POP*>

    e := d;
    d := c;
    c := b ROL 30;
    b := a;
    a := a1;
END compute;

<*/GROUPLIBPROCS:Y*>
PROCEDURE HashBuffer(hash : SHA1);
VAR
    a2,
    a1, b1,
    c1, d1, e1  : CARDINAL32;
BEGIN
    a1 := hash^.a;
    b1 := hash^.b;
    c1 := hash^.c;
    d1 := hash^.d;
    e1 := hash^.e;

    (* round 1 *)
    compute1(hash, 0, 0, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 1, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 2, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 3, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 4, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 5, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 6, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 7, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 8, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 9, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 10, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 11, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 12, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 13, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 14, a1, b1, c1, d1, e1, a2);
    compute1(hash, 0, 15, a1, b1, c1, d1, e1, a2);
    compute(hash, 0, 16, a1, b1, c1, d1, e1, a2);
    compute(hash, 0, 17, a1, b1, c1, d1, e1, a2);
    compute(hash, 0, 18, a1, b1, c1, d1, e1, a2);
    compute(hash, 0, 19, a1, b1, c1, d1, e1, a2);

    (* round 2 *)
    compute(hash, 1, 20, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 21, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 22, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 23, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 24, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 25, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 26, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 27, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 28, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 29, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 30, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 31, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 32, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 33, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 34, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 35, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 36, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 37, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 38, a1, b1, c1, d1, e1, a2);
    compute(hash, 1, 39, a1, b1, c1, d1, e1, a2);

    (* round 3 *)
    compute(hash, 2, 40, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 41, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 42, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 43, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 44, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 45, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 46, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 47, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 48, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 49, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 50, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 51, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 52, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 53, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 54, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 55, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 56, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 57, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 58, a1, b1, c1, d1, e1, a2);
    compute(hash, 2, 59, a1, b1, c1, d1, e1, a2);

    (* round 4 *)
    compute(hash, 3, 60, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 61, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 62, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 63, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 64, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 65, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 66, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 67, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 68, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 69, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 70, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 71, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 72, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 73, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 74, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 75, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 76, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 77, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 78, a1, b1, c1, d1, e1, a2);
    compute(hash, 3, 79, a1, b1, c1, d1, e1, a2);

    <*/PUSH/NOCHECK:O*>
    hash^.a := hash^.a + a1;
    hash^.b := hash^.b + b1;
    hash^.c := hash^.c + c1;
    hash^.d := hash^.d + d1;
    hash^.e := hash^.e + e1;
    <*/POP*>
END HashBuffer;

PROCEDURE HashBytes(hash : SHA1; data : ADDRESS; amount : CARDINAL);
VAR
    bp          : ADRCARD;
    ptrB        : POINTER TO ARRAY [0..7] OF BYTE;
BEGIN
    IF NOT hash^.finalized THEN
        ptrB := data;

        bp := hash^.last;
        LOOP
            IF amount > 0 THEN
                IF (bp+8 <= cBUFFER) AND
                   ((bp REM 4) = 0) AND
                   (amount >= 8)
                THEN
                    (* all this saves memory write operations.
                       also helps processor data prefetch.
                    *)
                    hash^.btBuffer[bp]:CARDINAL32 :=
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
                    hash^.btBuffer[bp+4]:CARDINAL32 :=
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

                ELSIF (bp+4 <= cBUFFER) AND
                      ((bp REM 4) = 0) AND
                      (amount >= 4)
                THEN
                    (* all this saves memory write operations.
                       also helps processor data prefetch.
                    *)
                    hash^.btBuffer[bp]:CARDINAL32 :=
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

                ELSIF bp < cBUFFER THEN
                    hash^.btBuffer[bp] := ptrB^[0];
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
PROCEDURE Finalize(hash : SHA1);

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
    LCARD64     = ARRAY [0..7] OF BYTE;

VAR
    index       : ADRCARD;
BEGIN
    IF hash^.finalized THEN
        RETURN;
    END;
    hash^.finalized := TRUE;

    IF hash^.last = cBUFFER THEN
        HashBuffer(hash);
        hash^.last := 0;
    END;

    hash^.btBuffer[hash^.last] := 80h;
    INC(hash^.last);

    WHILE (hash^.last REM 64) <> 56 DO  (* cLast REM 512 = 448 in bits *)
        IF hash^.last = cBUFFER THEN
            HashBuffer(hash);
            hash^.last := 0;
        END;
        hash^.btBuffer[hash^.last] := 0;
        INC(hash^.last);
    END;

    (* put the bitcount into the buffer in big endian order. *)
    FOR index := cFIRST TO cLAST BY nSTEP DO
        hash^.btBuffer[hash^.last] := hash^.length:LCARD64[index];
        INC(hash^.last);
    END;

    HashBuffer(hash);

    (* save a byte ordered result *)

    hash^.bytes[0]:CARDINAL32 := BIGENDIAN(hash^.a);
    hash^.bytes[4]:CARDINAL32 := BIGENDIAN(hash^.b);
    hash^.bytes[8]:CARDINAL32 := BIGENDIAN(hash^.c);
    hash^.bytes[12]:CARDINAL32 := BIGENDIAN(hash^.d);
    hash^.bytes[16]:CARDINAL32 := BIGENDIAN(hash^.e);
END Finalize;

PROCEDURE GetHash(hash : SHA1; VAR OUT data : ARRAY OF BYTE);
BEGIN
    IF NOT hash^.finalized THEN
        Finalize(hash);
    END;
    data := hash^.bytes;
END GetHash;

PROCEDURE GetString(hash : SHA1; VAR OUT str : ARRAY OF CHAR);
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
    IF HIGH(str) >= 20*2-1 THEN
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
(* test vectors taken from the RFC document *)
CONST
    input1      = "abc";
    input2      = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq";

    output1     : ARRAY [0..19] OF CARDINAL8 =
        {0A9h, 99h, 3Eh, 36h, 47h, 06h, 81h, 6Ah, 0BAh, 3Eh,
         25h, 71h, 78h, 50h, 0C2h, 6Ch, 9Ch, 0D0h, 0D8h, 9Dh};
    output2     : ARRAY [0..19] OF CARDINAL8 =
        {84h, 98h, 3Eh, 44h, 1Ch, 3Bh, 0D2h, 6Eh, 0BAh, 0AEh,
         4Ah, 0A1h, 0F9h, 51h, 29h, 0E5h, 0E5h, 46h, 70h, 0F1h};
VAR
    hash        : SHA1;
    output      : ARRAY [0..19] OF CARDINAL8;
    ok          : BOOLEAN;

    PROCEDURE verify(a, b : ARRAY OF CARDINAL8) : BOOLEAN;
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

END SHA1.
