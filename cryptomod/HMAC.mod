(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 2009                                  *)
(*                        by ADW Software                                  *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE HMAC;

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, ADR;

FROM MemUtils IMPORT
    ZeroMem, MoveMem;

IMPORT SHA1;

PROCEDURE HMAC_SHA1(hash : SHA1.SHA1;
                    key : ADDRESS;
                    keyLen : CARDINAL;
                    message : ADDRESS;
                    messageLen : CARDINAL;
                    VAR OUT mac : ARRAY OF BYTE);
TYPE
    alignRec =
        RECORD
        align           : CARDINAL32;(* get the byte arrays 32-bit aligned.
                                        assumes HashBlockSize is 32-bit aligned.
                                        for all known hashes this is true. *)
        iPad, oPad      : ARRAY [0..SHA1.HashBlockSize-1] OF CARDINAL8;
        END;
VAR
    <*/PUSH/NOCHECK:U*>
    pad         : alignRec;
    <*/POP*>
    h           : ARRAY [0..SHA1.HashLength-1] OF CARDINAL8;
    i           : ADRCARD;
    dispose     : BOOLEAN;
BEGIN
    dispose := FALSE;
    IF hash = NIL THEN
        dispose := TRUE;
        hash := SHA1.Create();
    END;

    ZeroMem(ADR(pad.iPad), SHA1.HashBlockSize);
    ZeroMem(ADR(pad.oPad), SHA1.HashBlockSize);

    IF keyLen > SHA1.HashBlockSize THEN
        SHA1.Reset(hash);
        SHA1.HashBytes(hash, key, keyLen);
        SHA1.GetHash(hash, h);
        MoveMem(ADR(pad.iPad), ADR(h), SHA1.HashLength);
        MoveMem(ADR(pad.oPad), ADR(h), SHA1.HashLength);
    ELSE
        MoveMem(ADR(pad.iPad), key, keyLen);
        MoveMem(ADR(pad.oPad), key, keyLen);
    END;

    FOR i := 0 TO SHA1.HashBlockSize-1 BY 4 DO
        pad.iPad[i]:CARDINAL32 := pad.iPad[i]:CARDINAL32 BXOR 36363636h;
        pad.oPad[i]:CARDINAL32 := pad.oPad[i]:CARDINAL32 BXOR 5C5C5C5Ch;
    END;

    SHA1.Reset(hash);
    SHA1.HashBytes(hash, ADR(pad.iPad), SHA1.HashBlockSize);
    SHA1.HashBytes(hash, message, messageLen);
    SHA1.GetHash(hash, h);
    SHA1.Reset(hash);
    SHA1.HashBytes(hash, ADR(pad.oPad), SHA1.HashBlockSize);
    SHA1.HashBytes(hash, ADR(h), SHA1.HashLength);
    SHA1.GetHash(hash, mac);

    IF dispose THEN
        SHA1.Destroy(hash);
    END;
END HMAC_SHA1;

END HMAC.
