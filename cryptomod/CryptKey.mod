(***************************************************************************)
(*                                                                         *)
(*                           Copyright (C) 2009                            *)
(*                               by ADW Software                           *)
(*                                                                         *)
(*                           All rights reserved.                          *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE CryptKey;

FROM SYSTEM IMPORT
    DWORD, ADDRESS, ADRCARD, BIGENDIAN, ADR, ADDADR;

FROM MemUtils IMPORT
    MoveMem;

IMPORT HMAC, SHA1;

PROCEDURE KDF1(password : ARRAY OF CHAR;
               salt : ADDRESS;
               saltLen : CARDINAL;
               iterationCount : CARDINAL;
               hashFunc : KeyHashFunction;
               hashUserData : ADDRESS;
               hashLen : CARDINAL;
               derivedKey : ADDRESS;
               keyLen : CARDINAL) : BOOLEAN;
VAR
    i, pLen     : CARDINAL;
    <*/PUSH/NOCHECK:U*>
    buffer      : ARRAY [0..127] OF CARDINAL8;
    <*/POP*>
BEGIN
    pLen := LENGTH(password) * SIZE(CHAR);
    IF (hashLen >= keyLen) AND (saltLen+pLen <= SIZE(buffer)) THEN
        MoveMem(ADR(buffer), ADR(password[0]), pLen);
        MoveMem(ADR(buffer[pLen]), salt, saltLen);
        hashFunc(hashUserData, ADR(buffer), saltLen+pLen, buffer);

        FOR i := 2 TO iterationCount DO
            hashFunc(hashUserData, ADR(buffer), hashLen, buffer);
        END;

        MoveMem(derivedKey, ADR(buffer), keyLen);
        RETURN TRUE;
    END;
    RETURN FALSE;
END KDF1;

PROCEDURE KDF2(password : ARRAY OF CHAR;
               salt : ADDRESS;
               saltLen : CARDINAL;
               iterationCount : CARDINAL;
               derivedKey : ADDRESS;
               keyLen : CARDINAL) : BOOLEAN;
TYPE
    alignRec =
        RECORD
        align           : CARDINAL32;(* get the byte arrays 32-bit aligned.
                                        assumes HashBlockSize is 32-bit aligned.
                                        for all known hashes this is true. *)
        h, h1, hRes     : ARRAY [0..SHA1.HashLength-1] OF CARDINAL8;
        END;

VAR
    pLen        : CARDINAL;
    i           : CARDINAL32;
    j           : CARDINAL;
    k           : ADRCARD;
    <*/PUSH/NOCHECK:U*>
    msg         : ARRAY [0..63] OF CARDINAL8;
    <*/POP*>
    h           : alignRec;
    DD          : DWORD;
    amount      : CARDINAL;
    hash        : SHA1.SHA1;
BEGIN
    pLen := LENGTH(password) * SIZE(CHAR);
    IF saltLen+4 <= SIZE(msg) THEN
        hash := SHA1.Create();

        MoveMem(ADR(msg), salt, saltLen);

        i := 1;
        REPEAT
            DD := BIGENDIAN(i);
            INC(i);
            msg[saltLen] := DD[0];
            msg[saltLen+1] := DD[1];
            msg[saltLen+2] := DD[2];
            msg[saltLen+3] := DD[3];
            HMAC.HMAC_SHA1(hash, ADR(password), pLen, ADR(msg), saltLen+4, h.hRes);

            h.h := h.hRes;
            FOR j := 2 TO iterationCount DO
                HMAC.HMAC_SHA1(hash,
                               ADR(password), pLen,
                               ADR(h.h), SHA1.HashLength,
                               h.h1);
                h.h := h.h1;

                FOR k := 0 TO SHA1.HashLength-1 BY 4 DO
                    h.hRes[k]:CARDINAL32 := h.hRes[k]:CARDINAL32 BXOR h.h1[k]:CARDINAL32;
                END;
            END;

            amount := SHA1.HashLength;
            IF amount > keyLen THEN
                amount := keyLen;
            END;
            MoveMem(derivedKey, ADR(h.hRes), amount);
            derivedKey := ADDADR(derivedKey, amount);
            keyLen := keyLen - amount;
        UNTIL keyLen = 0;

        SHA1.Destroy(hash);

        RETURN TRUE;
    END;
    RETURN FALSE;
END KDF2;

END CryptKey.
