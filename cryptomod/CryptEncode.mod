(***************************************************************************)
(*                                                                         *)
(*                           Copyright (C) 2009                            *)
(*                               by ADW Software                           *)
(*                                                                         *)
(*                           All rights reserved.                          *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE CryptEncode;

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, ADR, ADDADR;

FROM MemUtils IMPORT
    MoveMem, ZeroMem;

FROM RandomNumbers IMPORT
    RandomHandle, RandomizeEx, RndEx, DisposeRandomHandle;

FROM ExStorage IMPORT
    ALLOCATE, DEALLOCATE;

IMPORT SHA1;

TYPE
    RandomBytesProc     = PROCEDURE(CARDINAL, ADDRESS);

    HashArray           = ARRAY [0..MaxHashLen-1] OF CARDINAL8;

VAR
    RandomH     : RandomHandle;
    GetRandom   : RandomBytesProc;

PROCEDURE PadBlock_RFC1423(block : ADDRESS;
                           VAR INOUT blockSize : CARDINAL;
                           blockPad : CARDINAL) : BOOLEAN;
VAR
    padding     : CARDINAL;
    padVal      : CARDINAL8;
    i           : ADRCARD;
    bp          : POINTER TO ARRAY [0..0] OF CARDINAL8;
BEGIN
    padding := blockPad - (blockSize REM blockPad);
    padVal := padding;

    bp := block;
    i := blockSize;
    REPEAT
        DEC(padding);
        bp^[i] := padVal;
        INC(i);
    UNTIL padding = 0;
    blockSize := i;
    RETURN TRUE;
END PadBlock_RFC1423;

PROCEDURE UnpadBlock_RFC1423(block : ADDRESS;
                             VAR INOUT blockSize : CARDINAL;
                             blockPad : CARDINAL) : BOOLEAN;
VAR
    padding     : CARDINAL;
    padVal      : CARDINAL8;
    i           : ADRCARD;
    bp          : POINTER TO ARRAY [0..0] OF CARDINAL8;
BEGIN
    IF (blockSize >= blockPad) AND ((blockSize REM blockPad) = 0) THEN
        bp := block;
        i := blockSize-1;
        padVal := bp^[i];
        padding := ORD(padVal);

        (* now verify that all of the padding bytes are what they should be *)
        REPEAT
            DEC(padding);
            IF bp^[i] <> padVal THEN
                RETURN FALSE;
            END;
            DEC(i);
        UNTIL padding = 0;
        blockSize := i+1;
        RETURN TRUE;
    END;
    RETURN FALSE;
END UnpadBlock_RFC1423;

PROCEDURE HashFunc_SHA1(userData : ADDRESS;
                       input : ADDRESS;
                       inputSize : CARDINAL;
                       VAR OUT output : ARRAY OF BYTE);
VAR
    h   : SHA1.SHA1;
BEGIN
    h := userData;
    SHA1.Reset(h);
    IF (inputSize <> 0) AND (input <> NIL) THEN
        SHA1.HashBytes(h, input, inputSize);
    END;
    SHA1.GetHash(h, output);
END HashFunc_SHA1;

PROCEDURE InitRandom;
BEGIN
    IF RandomH = NIL THEN
        RandomH := RandomizeEx(0);
    END;
END InitRandom;

PROCEDURE GetRandomNormal(count : CARDINAL; data : ADDRESS);
VAR
    num         : CARDINAL;
    bytes       : CARDINAL;
BEGIN
    InitRandom;

    REPEAT
        num := RndEx(RandomH, 0);

        bytes := SIZE(num);
        WHILE (count > 0) AND (bytes > 0) DO
            DEC(bytes);
            DEC(count);
            data^ := VAL(CARDINAL8, num BAND 0FFh);
            data := ADDADR(data, 1);
            num := num SHR 8;
        END;
    UNTIL count = 0;
END GetRandomNormal;

PROCEDURE Card32ToBytes(num : CARDINAL32; VAR OUT data : ARRAY OF BYTE);
BEGIN
    data[0] := VAL(CARDINAL8, (num SHR 24) BAND 0FFh);
    data[1] := VAL(CARDINAL8, (num SHR 16) BAND 0FFh);
    data[2] := VAL(CARDINAL8, (num SHR 8) BAND 0FFh);
    data[3] := VAL(CARDINAL8, num BAND 0FFh);
END Card32ToBytes;

PROCEDURE XorBytes(A, B : ADDRESS; count : CARDINAL);
BEGIN
    REPEAT
        DEC(count);
        A^ := A^ BXOR B^;
        A := ADDADR(A, 1);
        B := ADDADR(B, 1);
    UNTIL count = 0;
END XorBytes;

PROCEDURE DataBlocksEqual(A, B : ADDRESS; count : CARDINAL) : BOOLEAN;
BEGIN
    REPEAT
        DEC(count);
        IF A^ <> B^ THEN
            RETURN FALSE;
        END;
        A := ADDADR(A, 1);
        B := ADDADR(B, 1);
    UNTIL count = 0;
    RETURN TRUE;
END DataBlocksEqual;

PROCEDURE MGF1(Z : ADDRESS;
               Zsize : CARDINAL;
               l : ADRCARD;
               mask : ADDRESS;
               hashFunc : EncodeHashFunction;
               hashUserData : ADDRESS;
               hashLen : CARDINAL
               );
VAR
    hashVal     : HashArray;
    ZC, mp      : POINTER TO ARRAY [0..0] OF CARDINAL8;
    i, j        : ADRCARD;
    move        : ADRCARD;
BEGIN
    ALLOCATE(ZC, Zsize+4);
    MoveMem(ZC, Z, Zsize);

    mp := mask;
    j := 0;
    i := 0;
    REPEAT
        Card32ToBytes(i, ZC^[Zsize..Zsize+3]);
        hashFunc(hashUserData, ZC, Zsize+4, hashVal);

        INC(i);

        move := hashLen;
        IF j + move > l THEN
            move := l - j;
        END;
        MoveMem(ADR(mp^[j]), ADR(hashVal), move);
        j := j + move;
    UNTIL j >= l;

    DEALLOCATE(ZC, Zsize+4);
END MGF1;

PROCEDURE Encode_OAEP(message : ADDRESS;
                      messageLen : CARDINAL;
                      encodingParams : ADDRESS;
                      encodingParamsLen : CARDINAL;
                      encodedMessage : ADDRESS;
                      emLen : CARDINAL;
                      hashFunc : EncodeHashFunction;
                      hashUserData : ADDRESS;
                      hashLen : CARDINAL
                     ) : BOOLEAN;
VAR
    seed                : HashArray;
    <*/PUSH/NOCHECK:U*>
    seedMask            : HashArray;
    <*/POP*>
    dbMask, emP         : POINTER TO ARRAY [0..0] OF CARDINAL8;
    maskLen             : CARDINAL;
    PS                  : CARDINAL;
BEGIN
    IF (hashLen <= MaxHashLen) AND (emLen >= messageLen+(2*hashLen)+1) THEN
        IF encodingParams = NIL THEN
            encodingParamsLen := 0;
        END;

        ALLOCATE(dbMask, emLen);
        emP := encodedMessage;

        PS := emLen - messageLen - (2*hashLen) - 1;
        maskLen := emLen-hashLen;

        hashFunc(hashUserData, encodingParams, encodingParamsLen, seed);
        MoveMem(emP, ADR(seed), hashLen);
        IF PS <> 0 THEN
            ZeroMem(ADR(emP^[hashLen]), PS);
        END;
        emP^[hashLen+PS] := 1;
        MoveMem(ADR(emP^[hashLen+PS+1]), message, messageLen);

        GetRandom(hashLen, ADR(seed));
        MGF1(ADR(seed), hashLen,
             maskLen,
             dbMask,
             hashFunc, hashUserData, hashLen);
        XorBytes(dbMask, emP, maskLen);

        MGF1(dbMask, maskLen,
             hashLen,
             ADR(seedMask),
             hashFunc, hashUserData, hashLen);
        XorBytes(ADR(seedMask), ADR(seed), hashLen);

        MoveMem(emP, ADR(seedMask), hashLen);
        MoveMem(ADR(emP^[hashLen]), dbMask, maskLen);

        DEALLOCATE(dbMask, emLen);

        RETURN TRUE;
    END;
    RETURN FALSE;
END Encode_OAEP;

PROCEDURE Decode_OAEP(encodedMessage : ADDRESS;
                      emLen : ADRCARD;
                      encodingParams : ADDRESS;
                      encodingParamsLen : CARDINAL;
                      message : ADDRESS;
                      VAR INOUT messageLen : CARDINAL;
                      hashFunc : EncodeHashFunction;
                      hashUserData : ADDRESS;
                      hashLen : ADRCARD
                     ) : BOOLEAN;
VAR
    <*/PUSH/NOCHECK:U*>
    seed                : HashArray;
    seedMask            : HashArray;
    <*/POP*>
    dbMask,
    maskedDB, emP       : POINTER TO ARRAY [0..0] OF CARDINAL8;
    maskLen             : CARDINAL;
    msgLen              : CARDINAL;
    i                   : ADRCARD;
    valid               : BOOLEAN;
BEGIN
    IF (hashLen <= MaxHashLen) AND (emLen > ((2*hashLen)+1)) THEN
        IF encodingParams = NIL THEN
            encodingParamsLen := 0;
        END;
        valid := FALSE;
        emP := encodedMessage;

        ALLOCATE(dbMask, emLen);
        ALLOCATE(maskedDB, emLen);

        maskLen := emLen - hashLen;
        MoveMem(ADR(seedMask), emP, hashLen);
        MoveMem(maskedDB, ADR(emP^[hashLen]), maskLen);

        MGF1(maskedDB, maskLen,
             hashLen,
             ADR(seed),
             hashFunc, hashUserData, hashLen);
        XorBytes(ADR(seed), ADR(seedMask), hashLen);

        MGF1(ADR(seed), hashLen,
             maskLen,
             dbMask,
             hashFunc, hashUserData, hashLen);
        XorBytes(maskedDB, dbMask, maskLen);

        hashFunc(hashUserData, encodingParams, encodingParamsLen, seed);
        IF DataBlocksEqual(maskedDB, ADR(seed), hashLen) THEN
            (* need to find the message in the block *)

            i := hashLen;

            IF maskedDB^[i] = 0 THEN
                WHILE (i < emLen) AND (maskedDB^[i] = 0) DO
                    INC(i);
                END;
            END;

            IF (i < emLen) AND (maskedDB^[i] = 1) THEN
                INC(i);
                msgLen := emLen - hashLen - i;
                IF msgLen <= messageLen THEN
                    messageLen := msgLen;
                    MoveMem(message, ADR(maskedDB^[i]), msgLen);
                    valid := TRUE;
                END;
            END;
        END;

        DEALLOCATE(maskedDB, emLen);
        DEALLOCATE(dbMask, emLen);

        RETURN valid;
    END;

    RETURN FALSE;
END Decode_OAEP;

PROCEDURE Encode_PSS(messageHash : ADDRESS;
                     saltLen : CARDINAL;
                     encodedMessage : ADDRESS;
                     emLen : CARDINAL;
                     hashFunc : EncodeHashFunction;
                     hashUserData : ADDRESS;
                     hashLen : CARDINAL
                    ) : BOOLEAN;
VAR
    dbMask,
    salt,
    emP         : POINTER TO ARRAY [0..0] OF CARDINAL8;
    H           : HashArray;
    PS          : ADRCARD;
    maskLen     : CARDINAL;
BEGIN
    IF (hashLen <= MaxHashLen) AND (emLen >= (hashLen + saltLen + 1)) THEN
        maskLen := emLen - hashLen - 1;
        ALLOCATE(dbMask, emLen);
        emP := encodedMessage;

        (*hashFunc(hashUserData, message, messageLen, H);*)

        ZeroMem(emP, 8);
        MoveMem(ADR(emP^[8]), messageHash, hashLen);

        salt := NIL;
        IF saltLen <> 0 THEN
            ALLOCATE(salt, saltLen);
            GetRandom(saltLen, salt);
            MoveMem(ADR(emP^[8+hashLen]), salt, saltLen);
        END;

        hashFunc(hashUserData, emP, hashLen + saltLen + 8, H);

        PS := emLen - saltLen - hashLen - 2;

        IF PS <> 0 THEN
            ZeroMem(emP, PS);
        END;
        emP^[PS] := 1;
        IF saltLen <> 0 THEN
            MoveMem(ADR(emP^[PS+1]), salt, saltLen);
        END;

        MGF1(ADR(H), hashLen,
             maskLen,
             dbMask,
             hashFunc, hashUserData, hashLen);
        XorBytes(emP, dbMask, maskLen);

        MoveMem(ADR(emP^[maskLen]), ADR(H), hashLen);
        emP^[maskLen+hashLen] := 0BCh;

        IF saltLen <> 0 THEN
            DEALLOCATE(salt, saltLen);
        END;
        DEALLOCATE(dbMask, emLen);

        RETURN TRUE;
    END;
    RETURN FALSE;
END Encode_PSS;

PROCEDURE Verify_PSS(encodedMessage : ADDRESS;
                     emLen : CARDINAL;
                     saltLen : CARDINAL;
                     messageHash : ADDRESS;
                     hashFunc : EncodeHashFunction;
                     hashUserData : ADDRESS;
                     hashLen : CARDINAL
                    ) : BOOLEAN;
VAR
    dbMask,
    maskedDB,
    emP         : POINTER TO ARRAY [0..0] OF CARDINAL8;
    mH          : HashArray;
    <*/PUSH/NOCHECK:U*>
    H           : HashArray;
    <*/POP*>
    PS          : CARDINAL;
    i           : CARDINAL;
    maskLen     : CARDINAL;
    valid       : BOOLEAN;
BEGIN
    valid := FALSE;
    emP := encodedMessage;

    IF (hashLen <= MaxHashLen) AND
       (emLen >= (hashLen + saltLen + 1)) AND
       (ORD(emP^[emLen-1]) = 0BCh)
    THEN
        (*hashFunc(hashUserData, message, messageLen, mH);*)

        maskLen := emLen - hashLen - 1;
        ALLOCATE(dbMask, emLen);
        ALLOCATE(maskedDB, emLen);

        MoveMem(maskedDB, emP, maskLen);
        MoveMem(ADR(H), ADR(emP^[maskLen]), hashLen);

        MGF1(ADR(H), hashLen,
             maskLen,
             dbMask,
             hashFunc, hashUserData, hashLen);
        XorBytes(dbMask, maskedDB, maskLen);

        PS := emLen - hashLen - saltLen - 2;
        valid := dbMask^[PS] = 1;
        i := PS;
        WHILE (i <> 0) AND valid DO
            DEC(i);
            valid := dbMask^[i] = 0;
        END;

        IF valid THEN
            ZeroMem(maskedDB, 8);
            MoveMem(ADR(maskedDB^[8]), messageHash, hashLen);
            IF saltLen <> 0 THEN
                MoveMem(ADR(maskedDB^[8+hashLen]), ADR(dbMask^[PS+1]), saltLen);
            END;
            hashFunc(hashUserData, maskedDB, hashLen + saltLen + 8, mH);
            valid := DataBlocksEqual(ADR(H), ADR(mH), hashLen);
        END;

        DEALLOCATE(dbMask, emLen);
        DEALLOCATE(maskedDB, emLen);
    END;

    RETURN valid;
END Verify_PSS;

PROCEDURE GetRandomSelfTest_OAEP_SHA1(count : CARDINAL; data : ADDRESS);
CONST
    value : ARRAY [0..SHA1.HashLength-1] OF CARDINAL8 =
            {0aah, 0fdh, 012h, 0f6h, 059h, 0cah, 0e6h, 034h,
             089h, 0b4h, 079h, 0e5h, 007h, 06dh, 0deh, 0c2h,
             0f0h, 06ch, 0b5h, 08fh};
BEGIN
    MoveMem(data, ADR(value), count);
END GetRandomSelfTest_OAEP_SHA1;

PROCEDURE GetRandomSelfTest_PSS_SHA1(count : CARDINAL; data : ADDRESS);
CONST
    value : ARRAY [0..SHA1.HashLength-1] OF CARDINAL8 =
            {
            0e3h, 0b5h, 0d5h, 0d0h, 002h, 0c1h, 0bch, 0e5h,
            00ch, 02bh, 065h, 0efh, 088h, 0a1h, 088h, 0d8h,
            03bh, 0ceh, 07eh, 061h
            };
BEGIN
    MoveMem(data, ADR(value), count);
END GetRandomSelfTest_PSS_SHA1;

PROCEDURE SelfTest() : BOOLEAN;
(* OAEP test vector taken from rsa-oaep_spec.pdf found at www.rsasecurity.com *)
CONST
    emOAEP   : ARRAY [0..126] OF CARDINAL8 =
        {
        0ebh, 07ah, 019h, 0ach, 0e9h, 0e3h, 000h, 063h,
        050h, 0e3h, 029h, 050h, 04bh, 045h, 0e2h, 0cah,
        082h, 031h, 00bh, 026h, 0dch, 0d8h, 07dh, 05ch,
        068h, 0f1h, 0eeh, 0a8h, 0f5h, 052h, 067h, 0c3h,
        01bh, 02eh, 08bh, 0b4h, 025h, 01fh, 084h, 0d7h,
        0e0h, 0b2h, 0c0h, 046h, 026h, 0f5h, 0afh, 0f9h,
        03eh, 0dch, 0fbh, 025h, 0c9h, 0c2h, 0b3h, 0ffh,
        08ah, 0e1h, 00eh, 083h, 09ah, 02dh, 0dbh, 04ch,
        0dch, 0feh, 04fh, 0f4h, 077h, 028h, 0b4h, 0a1h,
        0b7h, 0c1h, 036h, 02bh, 0aah, 0d2h, 09ah, 0b4h,
        08dh, 028h, 069h, 0d5h, 002h, 041h, 021h, 043h,
        058h, 011h, 059h, 01bh, 0e3h, 092h, 0f9h, 082h,
        0fbh, 03eh, 087h, 0d0h, 095h, 0aeh, 0b4h, 004h,
        048h, 0dbh, 097h, 02fh, 03ah, 0c1h, 04fh, 07bh,
        0c2h, 075h, 019h, 052h, 081h, 0ceh, 032h, 0d2h,
        0f1h, 0b7h, 06dh, 04dh, 035h, 03eh, 02dh
        };

    messageOAEP     : ARRAY [0..15] OF CARDINAL8 = {0d4h, 036h, 0e9h, 095h,
                                                    069h, 0fdh, 032h, 0a7h,
                                                    0c8h, 0a0h, 05bh, 0bch,
                                                    090h, 0d3h, 02ch, 049h};

(* PSS test vector obtained from www.rsasecurity.com *)
    messagePSS   : ARRAY [0..113] OF CARDINAL8 =
        {
        085h, 09eh, 0efh, 02fh, 0d7h, 08ah, 0cah, 000h,
        030h, 08bh, 0dch, 047h, 011h, 093h, 0bfh, 055h,
        0bfh, 09dh, 078h, 0dbh, 08fh, 08ah, 067h, 02bh,
        048h, 046h, 034h, 0f3h, 0c9h, 0c2h, 06eh, 064h,
        078h, 0aeh, 010h, 026h, 00fh, 0e0h, 0ddh, 08ch,
        008h, 02eh, 053h, 0a5h, 029h, 03ah, 0f2h, 017h,
        03ch, 0d5h, 00ch, 06dh, 05dh, 035h, 04fh, 0ebh,
        0f7h, 08bh, 026h, 002h, 01ch, 025h, 0c0h, 027h,
        012h, 0e7h, 08ch, 0d4h, 069h, 04ch, 09fh, 046h,
        097h, 077h, 0e4h, 051h, 0e7h, 0f8h, 0e9h, 0e0h,
        04ch, 0d3h, 073h, 09ch, 06bh, 0bfh, 0edh, 0aeh,
        048h, 07fh, 0b5h, 056h, 044h, 0e9h, 0cah, 074h,
        0ffh, 077h, 0a5h, 03ch, 0b7h, 029h, 080h, 02fh,
        06eh, 0d4h, 0a5h, 0ffh, 0a8h, 0bah, 015h, 098h,
        090h, 0fch};

    emPSS   : ARRAY [0..127] OF CARDINAL8 =
        {
        066h, 0e4h, 067h, 02eh, 083h, 06ah, 0d1h, 021h,
        0bah, 024h, 04bh, 0edh, 065h, 076h, 0b8h, 067h,
        0d9h, 0a4h, 047h, 0c2h, 08ah, 06eh, 066h, 0a5h,
        0b8h, 07dh, 0eeh, 07fh, 0bch, 07eh, 065h, 0afh,
        050h, 057h, 0f8h, 06fh, 0aeh, 089h, 084h, 0d9h,
        0bah, 07fh, 096h, 09ah, 0d6h, 0feh, 002h, 0a4h,
        0d7h, 05fh, 074h, 045h, 0feh, 0fdh, 0d8h, 05bh,
        06dh, 03ah, 047h, 07ch, 028h, 0d2h, 04bh, 0a1h,
        0e3h, 075h, 06fh, 079h, 02dh, 0d1h, 0dch, 0e8h,
        0cah, 094h, 044h, 00eh, 0cbh, 052h, 079h, 0ech,
        0d3h, 018h, 03ah, 031h, 01fh, 0c8h, 096h, 0dah,
        01ch, 0b3h, 093h, 011h, 0afh, 037h, 0eah, 04ah,
        075h, 0e2h, 04bh, 0dbh, 0fdh, 05ch, 01dh, 0a0h,
        0deh, 07ch, 0ech, 0dfh, 01ah, 089h, 06fh, 09dh,
        08bh, 0c8h, 016h, 0d9h, 07ch, 0d7h, 0a2h, 0c4h,
        03bh, 0adh, 054h, 06fh, 0beh, 08ch, 0feh, 0bch
        };

VAR
    valid       : BOOLEAN;
    emT         : ARRAY [0..127] OF CARDINAL8;
    <*/PUSH/NOCHECK:U*>
    msg         : ARRAY [0..15] OF CARDINAL8;
    <*/POP*>
    msgLen      : CARDINAL;
    hash        : HashArray;
    save        : RandomBytesProc;
    h           : SHA1.SHA1;
BEGIN
    save := GetRandom;

    valid := FALSE;
    emT[0] := 0;
    emT[1] := 0;
    emT[8] := 0FFh;
    msgLen := 2;
    IF PadBlock_RFC1423(ADR(emT), msgLen, 8) THEN
        IF (msgLen = 8) AND (emT[8] = 0FFh) THEN
            IF UnpadBlock_RFC1423(ADR(emT), msgLen, 8) THEN
                IF msgLen = 2 THEN
                    valid := TRUE;
                END;
            END;
        END;
    END;

    IF valid THEN
        valid := FALSE;

        h := SHA1.Create();

        GetRandom := GetRandomSelfTest_OAEP_SHA1;

        IF Encode_OAEP(ADR(messageOAEP), SIZE(messageOAEP),
                       NIL, 0,
                       ADR(emT), SIZE(emOAEP),
                       HashFunc_SHA1, h, SHA1.HashLength)
        THEN
            IF DataBlocksEqual(ADR(emT), ADR(emOAEP), SIZE(emOAEP)) THEN
                msgLen := SIZE(msg);
                IF Decode_OAEP(ADR(emT), SIZE(emOAEP),
                               NIL, 0,
                               ADR(msg), msgLen,
                               HashFunc_SHA1, h, SHA1.HashLength)
                THEN
                    IF msgLen = SIZE(messageOAEP) THEN
                        valid := DataBlocksEqual(ADR(msg), ADR(messageOAEP), msgLen);
                    END;
                END;
            END;
        END;

        IF valid THEN
            valid := FALSE;

            GetRandom := GetRandomSelfTest_PSS_SHA1;

            SHA1.Reset(h);
            SHA1.HashBytes(h, ADR(messagePSS), SIZE(messagePSS));
            SHA1.GetHash(h, hash);

            IF Encode_PSS(ADR(hash),
                          SHA1.HashLength,
                          ADR(emT), SIZE(emPSS),
                          HashFunc_SHA1, h, SHA1.HashLength)
            THEN
                IF DataBlocksEqual(ADR(emT), ADR(emPSS), SIZE(emPSS)) THEN
                    valid := Verify_PSS(ADR(emT), SIZE(emPSS),
                                        SHA1.HashLength,
                                        ADR(hash),
                                        HashFunc_SHA1, h, SHA1.HashLength);
                END;
            END;
        END;

        SHA1.Destroy(h);
    END;

    GetRandom := save;
    RETURN valid;

EXCEPT
    GetRandom := save;
    RETURN FALSE;
END SelfTest;

BEGIN
    RandomH := NIL;
    GetRandom := GetRandomNormal;
FINALLY
    IF RandomH <> NIL THEN
        DisposeRandomHandle(RandomH);
    END;
END CryptEncode.
