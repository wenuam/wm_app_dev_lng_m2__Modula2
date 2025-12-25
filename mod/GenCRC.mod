(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE GenCRC;
<*/OPTIMIZE:T*>

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, ADR, ADDADR, IsThread;

CONST
    Magic       = 0EDB88320h; (* CRC polynomial number *)

VAR
    Table       : ARRAY [0..255] OF CARDINAL32;

PROCEDURE CrcByte(b : BYTE; crc : CARDINAL32) : CARDINAL32;
BEGIN
    <*/PUSH/NOWARN:U*>(* ORD(crc) is unecessary in 32-bit mode *)
    RETURN (crc SHR 8) BXOR Table[(ORD(b) BXOR ORD(crc)) BAND 0FFh];
    <*/POP*>
END CrcByte;

PROCEDURE CrcBlock(data : ARRAY OF BYTE; crc : CARDINAL32) : CARDINAL32;
BEGIN
    RETURN CrcBytes(ADR(data), HIGH(data)+1, crc);
END CrcBlock;

PROCEDURE CrcBytes(data : ADDRESS; numBytes : CARDINAL; crc : CARDINAL32) : CARDINAL32;
VAR
    lcrc        : CARDINAL32;
BEGIN
    lcrc := crc;
    LOOP
        IF numBytes > 0 THEN
            DEC(numBytes);
            <*/PUSH/NOWARN:U*>(* ORD(lcrc) is unecessary in 32-bit mode *)
            lcrc := (lcrc SHR 8) BXOR Table[(ORD(data^) BXOR ORD(lcrc)) BAND 0FFh];
            <*/POP*>
            data := ADDADR(data, 1);
        ELSE
            EXIT;
        END;
    END;
    RETURN lcrc;
END CrcBytes;

PROCEDURE CrcPostcondition(crc : CARDINAL32) : CARDINAL32;
BEGIN
    RETURN crc BXOR 0FFFFFFFFh;
END CrcPostcondition;

PROCEDURE GenCRC(data : ADDRESS; numBytes : CARDINAL) : CARDINAL32;
BEGIN
    RETURN CrcPostcondition(CrcBytes(data, numBytes, CrcPrecondition));
END GenCRC;

PROCEDURE InitCrcTable;
VAR
    i, j        : ADRCARD;
    val         : CARDINAL32;
BEGIN
    (* this loop generates a table for doing a CRC with bytes as input *)

    FOR i := 0 TO 255 DO
        val := i;
        FOR j := 1 TO 8 DO
            IF (val BAND 1) = 1 THEN
                val := (val SHR 1) BXOR Magic;
            ELSE
                val := val SHR 1;
            END;
        END;
        Table[i] := val;
    END;
END InitCrcTable;

BEGIN
    IF NOT IsThread THEN
        InitCrcTable;
    END;
END GenCRC.
