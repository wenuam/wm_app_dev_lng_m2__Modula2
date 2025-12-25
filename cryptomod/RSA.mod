(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 2009                                  *)
(*                        by ADW Software                                  *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE RSA;

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, ADR;

FROM ExStorage IMPORT
    HeapInfoPointer, GetHeap,
    ALLOCATE, DeallocateEx;

FROM MemUtils IMPORT
    MoveMem;

FROM CryptEncode IMPORT
    HashFunc_SHA1,
    Encode_OAEP, Decode_OAEP, Encode_PSS, Verify_PSS;

IMPORT VLI, SHA1;

TYPE
    RSArec =
    RECORD
    (* public key values *)
    e, n        : VLI.VLI;

    (* private key values *)
    p, q,
    dp, dq,
    qInv        : VLI.VLI;

    (* temps *)
    t           : VLI.VLI;

    (* private key temps *)
    m1, m2,
    t1, t2      : VLI.VLI;

    (* hash type(s) *)
    sha1        : SHA1.SHA1;

    (* PowerModEx values *)
    pmExN,
    pmExP,
    pmExQ       : VLI.PowerModExInfo;

    buffer      : POINTER TO ARRAY [0..0] OF CARDINAL8;
    bufSize     : CARDINAL;
    emLen       : CARDINAL;

    heap        : HeapInfoPointer;
    bigE        : BOOLEAN;
    END;

    RSA         = POINTER TO RSArec;

PROCEDURE GenerateKeys(bits : CARDINAL; e : VLI.Digit; VAR OUT p, q, n : VLI.VLI);
BEGIN
    VLI.GetPrime(bits/2, 0, e, p);
    VLI.GetPrime(bits/2, 0, e, q);
    VLI.Multiply(p, q, n);
END GenerateKeys;

PROCEDURE GenerateKeys2(bits : CARDINAL; e : VLI.VLI; VAR OUT p, q, n : VLI.VLI);
VAR
    t1, t2, t3  : VLI.VLI;
BEGIN
    t1 := VLI.Create();
    t2 := VLI.Create();
    t3 := VLI.Create();

    LOOP
        VLI.GetPrime(bits/2, 0, 0, p);
        VLI.SubInt(p, 1, t1);
        IF VLI.GCDeq1(e, t1) THEN
            VLI.GetPrime(bits/2, 0, 0, q);
            VLI.SubInt(q, 1, t2);
            IF VLI.GCDeq1(e, t2) THEN
                VLI.Multiply(t1, t2, t3);
                IF VLI.GCDeq1(e, t3) THEN
                    EXIT;
                END;
            END;
        END;
    END;

    VLI.Multiply(p, q, n);

    VLI.Dispose(t1);
    VLI.Dispose(t2);
    VLI.Dispose(t3);
END GenerateKeys2;

PROCEDURE Create(e : VLI.Digit; p, q, n : VLI.VLI) : RSA;
VAR
    t           : VLI.VLI;
    crypt       : RSA;
BEGIN
    t := VLI.Create();
    VLI.SetValue(t, e);
    crypt := Create2(t, p, q, n);
    VLI.Dispose(t);
    RETURN crypt;
END Create;

PROCEDURE Create2(e, p, q, n : VLI.VLI) : RSA;
VAR
    crypt       : RSA;
BEGIN
    IF (e <> NIL) AND
       ((n <> NIL) OR ((p <> NIL) AND (q <> NIL)))
    THEN
        NEW(crypt);
        crypt^.heap := GetHeap();
        crypt^.p := NIL;
        crypt^.q := NIL;
        crypt^.dp := NIL;
        crypt^.dq := NIL;
        crypt^.qInv := NIL;
        crypt^.m1 := NIL;
        crypt^.m2 := NIL;
        crypt^.t1 := NIL;
        crypt^.t2 := NIL;
        crypt^.t := NIL;
        crypt^.pmExP := NIL;
        crypt^.pmExQ := NIL;
        crypt^.sha1 := NIL;

        crypt^.e := VLI.Copy(e);
        crypt^.bigE := VLI.GetDigitCount(crypt^.e) > 2;
        crypt^.t := VLI.Create();

        IF p <> NIL THEN
            crypt^.p := VLI.Copy(p);
            crypt^.q := VLI.Copy(q);

            crypt^.n := VLI.Create();
            crypt^.qInv := VLI.Create();
            crypt^.dp := VLI.Create();
            crypt^.dq := VLI.Create();
            crypt^.m1 := VLI.Create();
            crypt^.m2 := VLI.Create();
            crypt^.t1 := VLI.Create();
            crypt^.t2 := VLI.Create();

            IF NOT VLI.ModularInverse(q, p, crypt^.qInv) THEN
                Destroy(crypt);
                RETURN NIL;
            END;

            VLI.SubInt(crypt^.p, 1, crypt^.t1);
            VLI.SubInt(crypt^.q, 1, crypt^.t2);
            VLI.Multiply(crypt^.t1, crypt^.t2, crypt^.n);

            IF NOT VLI.ModularInverse(crypt^.e, crypt^.n, crypt^.m2) THEN
                Destroy(crypt);
                RETURN NIL;
            END;

            VLI.Rem(crypt^.m2, crypt^.t1, crypt^.dp);
            VLI.Rem(crypt^.m2, crypt^.t2, crypt^.dq);

            crypt^.pmExP := VLI.CreatePowerModExInfo(crypt^.p);
            crypt^.pmExQ := VLI.CreatePowerModExInfo(crypt^.q);

            VLI.Multiply(p, q, crypt^.n);
        ELSE
            crypt^.n := VLI.Copy(n);
        END;

        crypt^.pmExN := VLI.CreatePowerModExInfo(crypt^.n);

        crypt^.bufSize := VLI.GetDigitCount(crypt^.n) * SIZE(VLI.Digit);
        ALLOCATE(crypt^.buffer, crypt^.bufSize);

        crypt^.emLen := crypt^.bufSize-1;

        RETURN crypt;
    END;
    RETURN NIL;
END Create2;

PROCEDURE Destroy(VAR INOUT crypt : RSA);
BEGIN
    IF crypt <> NIL THEN
        DeallocateEx(crypt^.buffer, crypt^.bufSize, crypt^.heap);

        VLI.DisposePowerModExInfo(crypt^.pmExN);
        IF crypt^.p <> NIL THEN
            VLI.DisposePowerModExInfo(crypt^.pmExP);
            VLI.DisposePowerModExInfo(crypt^.pmExQ);
        END;

        VLI.Dispose(crypt^.t);
        VLI.Dispose(crypt^.e);
        VLI.Dispose(crypt^.n);
        VLI.Dispose(crypt^.p);
        VLI.Dispose(crypt^.q);
        VLI.Dispose(crypt^.dp);
        VLI.Dispose(crypt^.dq);
        VLI.Dispose(crypt^.qInv);
        VLI.Dispose(crypt^.m1);
        VLI.Dispose(crypt^.m2);
        VLI.Dispose(crypt^.t1);
        VLI.Dispose(crypt^.t2);

        IF crypt^.sha1 <> NIL THEN
            SHA1.Destroy(crypt^.sha1);
        END;

        DeallocateEx(crypt, SIZE(crypt^), crypt^.heap);
    END;
END Destroy;

PROCEDURE HasPrivateKey(crypt : RSA) : BOOLEAN;
BEGIN
    RETURN crypt^.p <> NIL;
END HasPrivateKey;

PROCEDURE EncodeVLI(crypt : RSA;
                    input : ADDRESS;
                    amount : CARDINAL;
                    VAR INOUT output : VLI.VLI) : BOOLEAN;
BEGIN
    VLI.SetBytesMSB(output, input, amount, FALSE);
    RETURN VLI.Compare(output, crypt^.n) = VLI.Less;
END EncodeVLI;

PROCEDURE DecodeVLI(input : VLI.VLI;
                    output : ADDRESS;
                    VAR INOUT amount : CARDINAL) : BOOLEAN;
VAR
    neg         : BOOLEAN;
BEGIN
    RETURN VLI.GetBytesMSB(input, output, amount, neg);
END DecodeVLI;

PROCEDURE PublicFunctionVLI(crypt : RSA; input : VLI.VLI; VAR OUT output : VLI.VLI);
BEGIN
    IF crypt^.bigE AND (crypt^.p <> NIL) THEN
        (* use the Chinese Remainder Theorem
            m1 = c ^ (e mod (p-1)) mod p
            m2 = c ^ (e mod (q-1)) mod q
            h = qInv * (m1-m2) mod p
            result = m2 + h * q
        *)
        VLI.SubInt(crypt^.p, 1, crypt^.t1);
        VLI.Rem(crypt^.e, crypt^.t1, crypt^.t2);
        VLI.PowerModEx(input, crypt^.t2, crypt^.pmExP, crypt^.m1);

        VLI.SubInt(crypt^.q, 1, crypt^.t1);
        VLI.Rem(crypt^.e, crypt^.t1, crypt^.t2);
        VLI.PowerModEx(input, crypt^.t2, crypt^.pmExQ, crypt^.m2);

        VLI.Subtract(crypt^.m1, crypt^.m2, crypt^.t1);
        VLI.MultiplyMod(crypt^.qInv, crypt^.t1, crypt^.p, crypt^.t1);
        VLI.Multiply(crypt^.t1, crypt^.q, crypt^.t2);
        VLI.Add(crypt^.m2, crypt^.t2, output);
    ELSE
        VLI.PowerModEx(input, crypt^.e, crypt^.pmExN, output);
    END;
END PublicFunctionVLI;

PROCEDURE PublicFunction(crypt : RSA;
                         input : ADDRESS;
                         inputSize : CARDINAL;
                         output : ADDRESS;
                         VAR INOUT outputSize : CARDINAL) : BOOLEAN;
BEGIN
    IF EncodeVLI(crypt, input, inputSize, crypt^.t) THEN
        PublicFunctionVLI(crypt, crypt^.t, crypt^.t);
        RETURN DecodeVLI(crypt^.t, output, outputSize);
    END;
    RETURN FALSE;
END PublicFunction;

PROCEDURE PrivateFunctionVLI(crypt : RSA; input : VLI.VLI; VAR OUT output : VLI.VLI);
BEGIN
    (* use the Chinese Remainder Theorem
        m1 = c ^ (d mod (p-1)) mod p
        m2 = c ^ (d mod (q-1)) mod q
        h = qInv * (m1-m2) mod p
        result = m2 + h * q
    *)
    VLI.PowerModEx(input, crypt^.dp, crypt^.pmExP, crypt^.m1);
    VLI.PowerModEx(input, crypt^.dq, crypt^.pmExQ, crypt^.m2);
    VLI.Subtract(crypt^.m1, crypt^.m2, crypt^.t1);
    VLI.MultiplyMod(crypt^.qInv, crypt^.t1, crypt^.p, crypt^.t1);
    VLI.Multiply(crypt^.t1, crypt^.q, crypt^.t2);
    VLI.Add(crypt^.m2, crypt^.t2, output);
END PrivateFunctionVLI;

PROCEDURE PrivateFunction(crypt : RSA;
                          input : ADDRESS;
                          inputSize : CARDINAL;
                          output : ADDRESS;
                          VAR INOUT outputSize : CARDINAL) : BOOLEAN;
BEGIN
    IF EncodeVLI(crypt, input, inputSize, crypt^.t) THEN
        PrivateFunctionVLI(crypt, crypt^.t, crypt^.t);
        RETURN DecodeVLI(crypt^.t, output, outputSize);
    END;
    RETURN FALSE;
END PrivateFunction;

PROCEDURE Encrypt_OAEP_SHA1(crypt : RSA;
                            message : ADDRESS;
                            messageLen : CARDINAL;
                            encodingParams : ADDRESS;
                            encodingParamsLen : CARDINAL;
                            cipher : ADDRESS;
                            VAR INOUT cipherLen : CARDINAL) : BOOLEAN;
BEGIN
    IF crypt^.sha1  = NIL THEN
        crypt^.sha1 := SHA1.Create();
    END;

    IF Encode_OAEP(message, messageLen,
                   encodingParams, encodingParamsLen,
                   crypt^.buffer, crypt^.emLen,
                   HashFunc_SHA1, crypt^.sha1, SHA1.HashLength)
    THEN
        RETURN PublicFunction(crypt,
                              crypt^.buffer, crypt^.emLen,
                              cipher, cipherLen);
    END;
    RETURN FALSE;
END Encrypt_OAEP_SHA1;

PROCEDURE Decrypt_OAEP_SHA1(crypt : RSA;
                            cipher : ADDRESS;
                            cipherLen : CARDINAL;
                            encodingParams : ADDRESS;
                            encodingParamsLen : CARDINAL;
                            message : ADDRESS;
                            VAR INOUT messageLen : CARDINAL) : BOOLEAN;
VAR
    emLen       : CARDINAL;
    pad         : ADRCARD;
    i           : ADRCARD;
BEGIN
    IF crypt^.sha1  = NIL THEN
        crypt^.sha1 := SHA1.Create();
    END;

    emLen := crypt^.bufSize;
    IF PrivateFunction(crypt,
                       cipher, cipherLen,
                       crypt^.buffer, emLen)
    THEN
        pad := crypt^.emLen-emLen;
        IF pad <> 0 THEN
            MoveMem(ADR(crypt^.buffer^[pad]), crypt^.buffer, emLen);
            i := 0;
            REPEAT
                DEC(pad);
                crypt^.buffer^[i] := 0;
                INC(i);
            UNTIL pad = 0;
        END;
        RETURN Decode_OAEP(crypt^.buffer, crypt^.emLen,
                           encodingParams, encodingParamsLen,
                           message, messageLen,
                           HashFunc_SHA1, crypt^.sha1, SHA1.HashLength);
    END;
    RETURN FALSE;
END Decrypt_OAEP_SHA1;

PROCEDURE Sign_PSS_SHA1(crypt : RSA;
                        messageHash : ADDRESS;
                        saltLen : CARDINAL;
                        cipher : ADDRESS;
                        VAR INOUT cipherLen : CARDINAL) : BOOLEAN;
BEGIN
    IF crypt^.sha1  = NIL THEN
        crypt^.sha1 := SHA1.Create();
    END;

    IF Encode_PSS(messageHash,
                  saltLen,
                  crypt^.buffer, crypt^.emLen,
                  HashFunc_SHA1, crypt^.sha1, SHA1.HashLength)
    THEN
        RETURN PrivateFunction(crypt,
                               crypt^.buffer, crypt^.emLen,
                               cipher, cipherLen);
    END;
    RETURN FALSE;
END Sign_PSS_SHA1;

PROCEDURE Verify_PSS_SHA1(crypt : RSA;
                          cipher : ADDRESS;
                          cipherLen : CARDINAL;
                          saltLen : CARDINAL;
                          messageHash : ADDRESS) : BOOLEAN;
VAR
    emLen       : CARDINAL;
    pad         : ADRCARD;
    i           : ADRCARD;
BEGIN
    IF crypt^.sha1  = NIL THEN
        crypt^.sha1 := SHA1.Create();
    END;

    emLen := crypt^.bufSize-1;
    IF PublicFunction(crypt,
                      cipher, cipherLen,
                      crypt^.buffer, emLen)
    THEN
        pad := crypt^.emLen-emLen;
        IF pad <> 0 THEN
            MoveMem(ADR(crypt^.buffer^[pad]), crypt^.buffer, emLen);
            i := 0;
            REPEAT
                DEC(pad);
                crypt^.buffer^[i] := 0;
                INC(i);
            UNTIL pad = 0;
        END;
        RETURN Verify_PSS(crypt^.buffer, crypt^.emLen,
                          saltLen,
                          messageHash,
                          HashFunc_SHA1, crypt^.sha1, SHA1.HashLength);
    END;

    RETURN FALSE;
END Verify_PSS_SHA1;

PROCEDURE SelfTest() : BOOLEAN;
(* test vectors taken from rsa-oaep_spec.pdf found at www.rsasecurity.com *)
CONST
    ps  = "ee cf ae 81 b1 b9 b3 c9 08 81 0b 10 a1 b5 60 01 99 eb 9f 44 ae f4 fd a4 " +
          "93 b8 1a 9e 3d 84 f6 32 12 4e f0 23 6e 5d 1e 3b 7e 28 fa e7 aa 04 0a 2d " +
          "5b 25 21 76 45 9d 1f 39 75 41 ba 2a 58 fb 65 99";

    qs  = "c9 7f b1 f0 27 f4 53 f6 34 12 33 ea aa d1 d9 35 3f 6c 42 d0 88 66 b1 d0 " +
          "5a 0f 20 35 02 8b 9d 86 98 40 b4 16 66 b4 2e 92 ea 0d a3 b4 32 04 b5 cf " +
          "ce 33 52 52 4d 04 16 a5 a4 41 e7 00 af 46 15 03";

    em   : ARRAY [0..126] OF CARDINAL8 =
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

    ct  : ARRAY [0..127] OF CARDINAL8 =
        {
        012h, 053h, 0e0h, 04dh, 0c0h, 0a5h, 039h, 07bh,
        0b4h, 04ah, 07ah, 0b8h, 07eh, 09bh, 0f2h, 0a0h,
        039h, 0a3h, 03dh, 01eh, 099h, 06fh, 0c8h, 02ah,
        094h, 0cch, 0d3h, 000h, 074h, 0c9h, 05dh, 0f7h,
        063h, 072h, 020h, 017h, 006h, 09eh, 052h, 068h,
        0dah, 05dh, 01ch, 00bh, 04fh, 087h, 02ch, 0f6h,
        053h, 0c1h, 01dh, 0f8h, 023h, 014h, 0a6h, 079h,
        068h, 0dfh, 0eah, 0e2h, 08dh, 0efh, 004h, 0bbh,
        06dh, 084h, 0b1h, 0c3h, 01dh, 065h, 04ah, 019h,
        070h, 0e5h, 078h, 03bh, 0d6h, 0ebh, 096h, 0a0h,
        024h, 0c2h, 0cah, 02fh, 04ah, 090h, 0feh, 09fh,
        02eh, 0f5h, 0c9h, 0c1h, 040h, 0e5h, 0bbh, 048h,
        0dah, 095h, 036h, 0adh, 087h, 000h, 0c8h, 04fh,
        0c9h, 013h, 00ah, 0deh, 0a7h, 04eh, 055h, 08dh,
        051h, 0a7h, 04dh, 0dfh, 085h, 0d8h, 0b5h, 00dh,
        0e9h, 068h, 038h, 0d6h, 006h, 03eh, 009h, 055h
        };
        (*
    ems  = "eb 7a 19 ac e9 e3 00 63 50 e3 29 50 4b 45 e2 ca 82 31 0b 26 dc d8 7d 5c " +
           "68 f1 ee a8 f5 52 67 c3 1b 2e 8b b4 25 1f 84 d7 e0 b2 c0 46 26 f5 af f9 " +
           "3e dc fb 25 c9 c2 b3 ff 8a e1 0e 83 9a 2d db 4c dc fe 4f f4 77 28 b4 a1 " +
           "b7 c1 36 2b aa d2 9a b4 8d 28 69 d5 02 41 21 43 58 11 59 1b e3 92 f9 82 " +
           "fb 3e 87 d0 95 ae b4 04 48 db 97 2f 3a c1 4f 7b c2 75 19 52 81 ce 32 d2 " +
           "f1 b7 6d 4d 35 3e 2d";

    cs  = "12 53 e0 4d c0 a5 39 7b b4 4a 7a b8 7e 9b f2 a0 39 a3 3d 1e 99 6f c8 2a " +
          "94 cc d3 00 74 c9 5d f7 63 72 20 17 06 9e 52 68 da 5d 1c 0b 4f 87 2c f6 " +
          "53 c1 1d f8 23 14 a6 79 68 df ea e2 8d ef 04 bb 6d 84 b1 c3 1d 65 4a 19 " +
          "70 e5 78 3b d6 eb 96 a0 24 c2 ca 2f 4a 90 fe 9f 2e f5 c9 c1 40 e5 bb 48 " +
          "da 95 36 ad 87 00 c8 4f c9 13 0a de a7 4e 55 8d 51 a7 4d df 85 d8 b5 0d " +
          "e9 68 38 d6 06 3e 09 55";*)

    e   = 17;

    message     : ARRAY [0..15] OF CARDINAL8 = {0d4h, 036h, 0e9h, 095h,
                                                069h, 0fdh, 032h, 0a7h,
                                                0c8h, 0a0h, 05bh, 0bch,
                                                090h, 0d3h, 02ch, 049h};
VAR
    p, q, n     : VLI.VLI;
    valid       : BOOLEAN;
    <*/PUSH/NOCHECK:U*>
    output      : ARRAY [0..127] OF CARDINAL8;
    msg         : ARRAY [0..15] OF CARDINAL8;
    <*/POP*>
    outputLen   : CARDINAL;
    msgLen      : CARDINAL;
    cryptPQ,
    cryptN      : RSA;

    PROCEDURE verify(a, b : ARRAY OF BYTE; count : ADRCARD) : BOOLEAN;
    VAR
        i       : ADRCARD;
    BEGIN
        FOR i := 0 TO count-1 DO
            IF a[i] <> b[i] THEN
                RETURN FALSE;
            END;
        END;
        RETURN TRUE;
    END verify;

BEGIN
    p := VLI.Create();
    q := VLI.Create();
    n := VLI.Create();

    valid := VLI.FromHexString(ps, p);
    valid := valid AND VLI.FromHexString(qs, q);
    IF valid THEN
        valid := FALSE;

        VLI.Multiply(p, q, n);
        cryptPQ := Create(e, p, q, NIL);
        cryptN := Create(e, NIL, NIL, n);
        IF (cryptPQ <> NIL) AND (cryptN <> NIL) THEN
            outputLen := SIZE(output);
            IF PublicFunction(cryptN, ADR(em), SIZE(em), ADR(output), outputLen) THEN
                IF outputLen = SIZE(ct) THEN
                    IF verify(ct, output, SIZE(ct)) THEN
                        msgLen := outputLen;
                        outputLen := SIZE(output);
                        IF PrivateFunction(cryptPQ,
                                           ADR(output), msgLen,
                                           ADR(output), outputLen)
                        THEN
                            IF outputLen = SIZE(em) THEN
                                IF verify(output, em, SIZE(em)) THEN
                                    outputLen := SIZE(output);
                                    IF PublicFunction(cryptPQ, ADR(em), SIZE(em), ADR(output), outputLen) THEN
                                        IF outputLen = SIZE(ct) THEN
                                            valid := verify(ct, output, SIZE(ct));
                                        END;
                                    END;
                                END;
                            END;
                        END;
                    END;
                END;
            END;

            IF valid THEN
                valid := FALSE;

                outputLen := SIZE(output);
                IF Encrypt_OAEP_SHA1(cryptN,
                                     ADR(message), SIZE(message),
                                     NIL, 0,
                                     ADR(output), outputLen)
                THEN
                    msgLen := SIZE(msg);
                    IF Decrypt_OAEP_SHA1(cryptPQ,
                                         ADR(output), outputLen,
                                         NIL, 0,
                                         ADR(msg), msgLen)
                    THEN
                        IF msgLen = SIZE(message) THEN
                            valid := verify(msg, message, SIZE(message));
                        END;
                    END;
                END;
            END;

            Destroy(cryptPQ);
            Destroy(cryptN);
        END;
    END;

    VLI.Dispose(n);
    VLI.Dispose(q);
    VLI.Dispose(p);

    RETURN valid;

EXCEPT
    RETURN FALSE;
END SelfTest;

END RSA.
