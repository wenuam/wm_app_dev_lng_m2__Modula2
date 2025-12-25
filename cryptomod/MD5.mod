(*
Name:     MD5
Creation: 08-03-2001
LastEdit: 11-05-2001
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:

Test values:
""                  D4 1D 8C D9 8F 00 B2 04  E9 80 09 98 EC F8 42 7E
"a"                 0C C1 75 B9 C0 F1 B6 A8  31 C3 99 E2 69 77 26 61
"abc                90 01 50 98 3C D2 4F B0  D6 96 3F 7D 28 E1 7F 72
"message digest"    F9 6B 69 7D 7C B7 93 8D  52 5A 2F 31 AA F1 61 D0
"12345678901234567890123456789012345678901234567890123456789012345678901234567890"
                    57 ED F4 A2 2B E3 C9 55  AC 49 DA 2E 21 07 B6 7A
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
IMPLEMENTATION MODULE MD5;
<*/OPTIMIZE:T*>

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, LITTLEENDIAN, ADR, ADDADR;
%IF BigEndian %THEN
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

    cBUFFER     = 64;
    cROUNDS     = 4;  (* 4 rounds *)
    cSTEPS      = 16; (* 16 steps per round *)
    cALLSTEPS   = cROUNDS * cSTEPS;
    cROLS       = 4;

TYPE
    tROUNDS     = CARDINAL[0..cROUNDS-1];

    ROLS        = ARRAY [0..cROUNDS-1],[0..cROLS-1] OF CARDINAL;
    SINUS       = ARRAY [0..cALLSTEPS-1] OF CARDINAL;

    MD5rec =
        RECORD
        a, b, c, d      : CARDINAL32;
        length          : CARDINAL64;
        last            : CARDINAL32;

        (* buffer must be 4-byte aligned,
           and the variant parts must overlap perfectly.
         *)
        CASE : CARDINAL OF
        0: btBuffer : ARRAY [0..cBUFFER-1] OF BYTE; |
        1: cBuffer : ARRAY [0..(cBUFFER/4)-1] OF CARDINAL32;
        ELSE
        END;

        (*must be 4 byte aligned*)
        bytes           : ARRAY [0..15] OF CARDINAL8;

        heap            : HeapInfoPointer;
        finalized       : BOOLEAN;
        END;

    MD5         = POINTER TO MD5rec;

CONST
    cT = SINUS{0D76AA478H, 0E8C7B756H, 0242070DBH, 0C1BDCEEEH, 0F57C0FAFH,
               04787C62AH, 0A8304613H, 0FD469501H, 0698098D8H, 08B44F7AFH,
               0FFFF5BB1H, 0895CD7BEH, 06B901122H, 0FD987193H, 0A679438EH,
               049B40821H, 0F61E2562H, 0C040B340H, 0265E5A51H, 0E9B6C7AAH,
               0D62F105DH, 002441453H, 0D8A1E681H, 0E7D3FBC8H, 021E1CDE6H,
               0C33707D6H, 0F4D50D87H, 0455A14EDH, 0A9E3E905H, 0FCEFA3F8H,
               0676F02D9H, 08D2A4C8AH, 0FFFA3942H, 08771F681H, 06D9D6122H,
               0FDE5380CH, 0A4BEEA44H, 04BDECFA9H, 0F6BB4B60H, 0BEBFBC70H,
               0289B7EC6H, 0EAA127FAH, 0D4EF3085H, 004881D05H, 0D9D4D039H,
               0E6DB99E5H, 01FA27CF8H, 0C4AC5665H, 0F4292244H, 0432AFF97H,
               0AB9423A7H, 0FC93A039H, 0655B59C3H, 08F0CCC92H, 0FFEFF47DH,
               085845DD1H, 06FA87E4FH, 0FE2CE6E0H, 0A3014314H, 04E0811A1H,
               0F7537E82H, 0BD3AF235H, 02AD7D2BBH, 0EB86D391H};

    cROL = ROLS{{7,12,17,22},
                {5,9,14,20},
                {4,11,16,23},
                {6,10,15,21}};


PROCEDURE Create() : MD5;
VAR
    hash        : MD5;
BEGIN
    NEW(hash);
    hash^.heap := GetHeap();
    Reset(hash);
    RETURN hash;
END Create;

PROCEDURE Destroy(VAR OUT hash : MD5);
BEGIN
    DeallocateEx(hash, SIZE(hash^), hash^.heap);
END Destroy;

PROCEDURE Reset(hash : MD5);
BEGIN
    hash^.last := 0;
    hash^.length := 0;
    hash^.a := cINITA;
    hash^.b := cINITB;
    hash^.c := cINITC;
    hash^.d := cINITD;
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
        RETURN ((B BAND D) BOR (C BAND (BNOT D)));
    |
    2:(*f3*)
        RETURN (B BXOR C BXOR D);
    |
    3:(*f4*)
        RETURN (C BXOR (B BOR (BNOT D)));
    END;
    RETURN 0;
END f;

PROCEDURE W(VAR INOUT hash : MD5; round : tROUNDS; step : ADRCARD) : CARDINAL32 [INLINE];
BEGIN
    %IF LittleEndian %THEN
        CASE round OF
        0:
            RETURN hash^.cBuffer[step];
        |
        1:
            RETURN hash^.cBuffer[(1+5*step) REM 16];
        |
        2:
            RETURN hash^.cBuffer[(5+3*step) REM 16];
        |
        3:
            RETURN hash^.cBuffer[(7*step) REM 16];
        END;
    %ELSE
        (* buffer is little endian, get word into native endian *)
        CASE round OF
        0:
            RETURN SWAPENDIAN(hash^.cBuffer[step]);
        |
        1:
            RETURN SWAPENDIAN(hash^.cBuffer[(1+5*step) REM 16]);
        |
        2:
            RETURN SWAPENDIAN(hash^.cBuffer[(5+3*step) REM 16]);
        |
        3:
            RETURN SWAPENDIAN(hash^.cBuffer[(7*step) REM 16]);
        END;
    %END
END W;

PROCEDURE compute(VAR INOUT hash : MD5;
                  round : tROUNDS;
                  step : ADRCARD;
                  VAR INOUT a, b, c, d, b1 : CARDINAL32) [INLINE];
BEGIN
    <*/PUSH/NOCHECK:O*>
    b1 := a + cT[step] + f(b, c, d, round);
    b1 := b1 + W(hash, round, step);
    b1 := b1 ROL cROL[round, step REM cROLS];
    b1 := b1 + b;
    <*/POP*>
    a := d;
    d := c;
    c := b;
    b := b1;
END compute;

<*/GROUPLIBPROCS:Y*>
PROCEDURE HashBuffer(hash : MD5);
VAR
    b2,
    a1, b1,
    c1, d1      : CARDINAL32;
BEGIN
    a1 := hash^.a;
    b1 := hash^.b;
    c1 := hash^.c;
    d1 := hash^.d;

    (* round 1 *)
    compute(hash, 0, 0, a1, b1, c1, d1, b2);
    compute(hash, 0, 1, a1, b1, c1, d1, b2);
    compute(hash, 0, 2, a1, b1, c1, d1, b2);
    compute(hash, 0, 3, a1, b1, c1, d1, b2);
    compute(hash, 0, 4, a1, b1, c1, d1, b2);
    compute(hash, 0, 5, a1, b1, c1, d1, b2);
    compute(hash, 0, 6, a1, b1, c1, d1, b2);
    compute(hash, 0, 7, a1, b1, c1, d1, b2);
    compute(hash, 0, 8, a1, b1, c1, d1, b2);
    compute(hash, 0, 9, a1, b1, c1, d1, b2);
    compute(hash, 0, 10, a1, b1, c1, d1, b2);
    compute(hash, 0, 11, a1, b1, c1, d1, b2);
    compute(hash, 0, 12, a1, b1, c1, d1, b2);
    compute(hash, 0, 13, a1, b1, c1, d1, b2);
    compute(hash, 0, 14, a1, b1, c1, d1, b2);
    compute(hash, 0, 15, a1, b1, c1, d1, b2);

    (* round 2 *)
    compute(hash, 1, 16, a1, b1, c1, d1, b2);
    compute(hash, 1, 17, a1, b1, c1, d1, b2);
    compute(hash, 1, 18, a1, b1, c1, d1, b2);
    compute(hash, 1, 19, a1, b1, c1, d1, b2);
    compute(hash, 1, 20, a1, b1, c1, d1, b2);
    compute(hash, 1, 21, a1, b1, c1, d1, b2);
    compute(hash, 1, 22, a1, b1, c1, d1, b2);
    compute(hash, 1, 23, a1, b1, c1, d1, b2);
    compute(hash, 1, 24, a1, b1, c1, d1, b2);
    compute(hash, 1, 25, a1, b1, c1, d1, b2);
    compute(hash, 1, 26, a1, b1, c1, d1, b2);
    compute(hash, 1, 27, a1, b1, c1, d1, b2);
    compute(hash, 1, 28, a1, b1, c1, d1, b2);
    compute(hash, 1, 29, a1, b1, c1, d1, b2);
    compute(hash, 1, 30, a1, b1, c1, d1, b2);
    compute(hash, 1, 31, a1, b1, c1, d1, b2);

    (* round 3 *)
    compute(hash, 2, 32, a1, b1, c1, d1, b2);
    compute(hash, 2, 33, a1, b1, c1, d1, b2);
    compute(hash, 2, 34, a1, b1, c1, d1, b2);
    compute(hash, 2, 35, a1, b1, c1, d1, b2);
    compute(hash, 2, 36, a1, b1, c1, d1, b2);
    compute(hash, 2, 37, a1, b1, c1, d1, b2);
    compute(hash, 2, 38, a1, b1, c1, d1, b2);
    compute(hash, 2, 39, a1, b1, c1, d1, b2);
    compute(hash, 2, 40, a1, b1, c1, d1, b2);
    compute(hash, 2, 41, a1, b1, c1, d1, b2);
    compute(hash, 2, 42, a1, b1, c1, d1, b2);
    compute(hash, 2, 43, a1, b1, c1, d1, b2);
    compute(hash, 2, 44, a1, b1, c1, d1, b2);
    compute(hash, 2, 45, a1, b1, c1, d1, b2);
    compute(hash, 2, 46, a1, b1, c1, d1, b2);
    compute(hash, 2, 47, a1, b1, c1, d1, b2);

    (* round 4 *)
    compute(hash, 3, 48, a1, b1, c1, d1, b2);
    compute(hash, 3, 49, a1, b1, c1, d1, b2);
    compute(hash, 3, 50, a1, b1, c1, d1, b2);
    compute(hash, 3, 51, a1, b1, c1, d1, b2);
    compute(hash, 3, 52, a1, b1, c1, d1, b2);
    compute(hash, 3, 53, a1, b1, c1, d1, b2);
    compute(hash, 3, 54, a1, b1, c1, d1, b2);
    compute(hash, 3, 55, a1, b1, c1, d1, b2);
    compute(hash, 3, 56, a1, b1, c1, d1, b2);
    compute(hash, 3, 57, a1, b1, c1, d1, b2);
    compute(hash, 3, 58, a1, b1, c1, d1, b2);
    compute(hash, 3, 59, a1, b1, c1, d1, b2);
    compute(hash, 3, 60, a1, b1, c1, d1, b2);
    compute(hash, 3, 61, a1, b1, c1, d1, b2);
    compute(hash, 3, 62, a1, b1, c1, d1, b2);
    compute(hash, 3, 63, a1, b1, c1, d1, b2);

    <*/PUSH/NOCHECK:O*>
    hash^.a := hash^.a + a1;
    hash^.b := hash^.b + b1;
    hash^.c := hash^.c + c1;
    hash^.d := hash^.d + d1;
    <*/POP*>
END HashBuffer;

PROCEDURE HashBytes(hash : MD5; data : ADDRESS; amount : CARDINAL);
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
PROCEDURE Finalize(hash : MD5);
%IF LittleEndian %THEN
CONST
    cFIRST      = 0;
    cLAST       = 7;
    nSTEP       = 1;
%ELSE
CONST
    cFIRST      = 7;
    cLAST       = 0;
    nSTEP       = -1;
%END

TYPE
    LCARD64     = ARRAY [0..7] OF BYTE;

VAR
    index       : CARDINAL;
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

    hash^.bytes[0]:CARDINAL32 := LITTLEENDIAN(hash^.a);
    hash^.bytes[4]:CARDINAL32 := LITTLEENDIAN(hash^.b);
    hash^.bytes[8]:CARDINAL32 := LITTLEENDIAN(hash^.c);
    hash^.bytes[12]:CARDINAL32 := LITTLEENDIAN(hash^.d);
END Finalize;

PROCEDURE GetHash(hash : MD5; VAR OUT data : ARRAY OF BYTE);
BEGIN
    IF NOT hash^.finalized THEN
        Finalize(hash);
    END;
    data := hash^.bytes;
END GetHash;

PROCEDURE GetString(hash : MD5; VAR OUT str : ARRAY OF CHAR);
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
    IF HIGH(str) >= 16*2-1 THEN
        j := 0;
        FOR i := 0 TO 15 DO
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
    input1      = "";
    input2      = "message digest";

    output1     : ARRAY [0..15] OF CARDINAL8 =
        {0D4h, 1Dh, 8Ch, 0D9h, 8Fh, 00h, 0B2h, 04h,
         0E9h, 80h, 09h, 98h, 0ECh, 0F8h, 42h, 7Eh};
    output2     : ARRAY [0..15] OF CARDINAL8 =
        {0F9h, 6Bh, 69h, 7Dh, 7Ch, 0B7h, 93h, 8Dh,
         52h, 5Ah, 2Fh, 31h, 0AAh, 0F1h, 61h, 0D0h};
VAR
    hash        : MD5;
    output      : ARRAY [0..15] OF CARDINAL8;
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

END MD5.
