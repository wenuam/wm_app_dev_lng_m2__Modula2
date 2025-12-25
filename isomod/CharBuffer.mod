IMPLEMENTATION MODULE CharBuffer;

(* =========================================
  Definition and Implementation © 1993
                by R. Sutcliffe
        Trinity Western University
7600 Glover Rd., Langley, BC Canada V3A 6H4
         e-mail: rsutc@twu.ca
    Last modification date 1993 10 20
=========================================== *)

(* provides a first in first out 1024 character buffer facility *)

FROM SYSTEM IMPORT
    ADDRESS, ADRCARD;

FROM ExStorage IMPORT
    AllocateEx, DeallocateEx, GetDefaultHeap;

CONST
  max = 1024;

TYPE
  Buffer = POINTER TO BuffRec;
  BuffRec =
    RECORD
      size,
      in,
      out : CARDINAL;
      buf : ARRAY [0..max] OF CHAR;
    END;

PROCEDURE ALLOCATE(VAR OUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    AllocateEx(addr, amount, GetDefaultHeap());
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    DeallocateEx(addr, amount, GetDefaultHeap());
END DEALLOCATE;

PROCEDURE Init (VAR b : Buffer);
(* create a new empty buffer *)

BEGIN
  NEW (b);
  Flush (b);
END Init;

PROCEDURE Destroy (VAR b: Buffer);
(* give all memory back to the system *)

BEGIN
  DISPOSE (b);
  b := NIL;
END Destroy;

PROCEDURE Flush (b : Buffer);
(* empty a buffer *)

BEGIN
  b^.size := 0;
  b^.in := 0;
  b^.out := 0;
END Flush;

PROCEDURE Full (b: Buffer) : BOOLEAN;
(* returns TRUE if the buffer cannot take any more characters
FALSE if it can *)

BEGIN
  IF b^.size = max
    THEN
      RETURN TRUE
    ELSE
      RETURN FALSE;
    END;
END Full;

PROCEDURE Empty (b: Buffer) : BOOLEAN;
(* returns TRUE if the buffer cannot give back any more characters
FALSE if it can *)

BEGIN
  IF b^.size = 0
    THEN
      RETURN TRUE
    ELSE
      RETURN FALSE;
    END;
END Empty;

PROCEDURE Enter (b : Buffer; ch: CHAR);
(* Enters the character.  If it was full, the first in is lost.
 If you don't like that way of doing it, write your own. *)

BEGIN
   b^.buf [b^.in] := ch;
   b^.in := (b^.in + 1) MOD max;
   IF NOT Full (b)
     THEN
       INC (b^.size);
     ELSE
       b^.out := (b^.out + 1) MOD max;
     END;
END Enter;

PROCEDURE Look (b : Buffer; VAR ch: CHAR);
(* get the first in without removing it *)

BEGIN
  IF NOT Empty (b)
    THEN
      ch := b^.buf [b^.out];
    ELSE
      ch := 0C
    END;
END Look;

PROCEDURE Fetch (b : Buffer; VAR ch: CHAR);
(* get the first in and removes it *)

BEGIN
   Look (b, ch);
   IF ch # 0C
   THEN
     b^.out := (b^.out + 1) MOD max;
     DEC (b^.size);
   END;
END Fetch;

PROCEDURE Skip (b : Buffer);
(* remove the first in *)

BEGIN
  IF NOT Empty (b)
    THEN
     b^.out := (b^.out + 1) MOD max;
     DEC (b^.size);
    ELSE
    END;
END Skip;

PROCEDURE Erase (b : Buffer);
(* remove the last in *)

BEGIN
  IF NOT Empty (b)
    THEN
     IF b^.in = 0
       THEN
         b^.in := max
       ELSE
         DEC (b^.in);
       END;
     DEC (b^.size);
    ELSE
    END;
END Erase;

PROCEDURE Size (b : Buffer) : CARDINAL;
(* returns number of characters in the buffer *)

BEGIN
  RETURN b^.size
END Size;

END CharBuffer.
