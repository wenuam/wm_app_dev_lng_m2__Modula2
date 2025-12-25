(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                            by ADW Software                              *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE Pipes;
<*/OPTIMIZE:T/NOPACK*>

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADRCARD, ADR, ADDADR;

FROM ExStorage IMPORT
    ALLOCATE, DeallocateEx, GetHeap, HeapInfoPointer;

FROM Threads IMPORT
    CriticalSection, SignalSem, SemWaitForever,
    CreateCriticalSection, CloseCriticalSection,
    EnterCriticalSection, LeaveCriticalSection,
    CreateSignalSem, CloseSignalSem, WaitForSignalSem, WaitResult,
    SendSignalSem, ResetSignalSem;

CONST
    MinBufferSize       = 1*1024;
    MaxBufferSize       = 128*1024;
    DefaultBufferSize   = 4*1024;

TYPE
    BufferPointer       = POINTER TO ARRAY [0..0] OF BYTE;
    PipeRecord  =
        RECORD
            mutex               : CriticalSection;
            readLock            : CriticalSection;
            writeLock           : CriticalSection;
            readWait            : SignalSem;
            writeWait           : SignalSem;
            buffer              : BufferPointer;
            heap                : HeapInfoPointer;
            bufferSize          : CARDINAL;
            inBuffer            : CARDINAL;
            readPos             : CARDINAL;
            writePos            : CARDINAL;
            moreData            : CARDINAL;
            readerWaiting       : BOOLEAN;
            writerWaiting       : BOOLEAN;
            pipeType            : PipeTypes;
        END;
    Pipe                = POINTER TO PipeRecord;
(*
 readLock and writeLock are used to allow only one reader/writer at a time.
 We need this because we support reads/writes larger than the available buffer space.
 This means that the reader/writer will have to hand off to each other.
 The mutex separates the allowed single reader and writer from each other.
 readWait allows for efficiently waiting for data to arrive when
 the buffer is empty and we still need to read data.
 writeWait is similar to readWait, used when the buffer is full and
 we still have data to write.
*)

PROCEDURE CreatePipe(VAR OUT p : Pipe;
                     pipeType : PipeTypes;
                     bufferSize : CARDINAL) : BOOLEAN;
VAR
    created     : BOOLEAN;
    ok          : BOOLEAN;
BEGIN
    IF bufferSize = 0 THEN
        bufferSize := DefaultBufferSize;
    ELSIF bufferSize > MaxBufferSize THEN
        bufferSize := MaxBufferSize;
    ELSIF bufferSize < MinBufferSize THEN
        bufferSize := MinBufferSize;
    END;

    ALLOCATE(p, SIZE(PipeRecord)+bufferSize);
    IF p = NIL THEN
        RETURN FALSE;
    END;
    p^.pipeType := pipeType;
    p^.buffer := ADDADR(p, SIZE(PipeRecord));
    p^.readPos := 0;
    p^.writePos := 0;
    p^.inBuffer := 0;
    p^.bufferSize := bufferSize;
    p^.readerWaiting := FALSE;
    p^.writerWaiting := FALSE;
    p^.moreData := 0;
    p^.heap := GetHeap();

    p^.mutex := NIL;
    p^.readLock := NIL;
    p^.writeLock := NIL;
    p^.readWait := NIL;
    p^.writeWait := NIL;

    IF CreateCriticalSection(p^.mutex) AND
       CreateCriticalSection(p^.readLock) AND
       CreateCriticalSection(p^.writeLock) AND
       CreateSignalSem(p^.readWait, 1, 0, "", created) AND
       CreateSignalSem(p^.writeWait, 1, 0, "", created)
    THEN
        RETURN TRUE;
    END;

    IF p^.mutex <> NIL THEN
        ok := CloseCriticalSection(p^.mutex);
    END;
    IF p^.readLock <> NIL THEN
        ok := CloseCriticalSection(p^.readLock);
    END;
    IF p^.writeLock <> NIL THEN
        ok := CloseCriticalSection(p^.writeLock);
    END;
    IF p^.readWait <> NIL THEN
        ok := CloseSignalSem(p^.readWait);
    END;
    IF p^.writeWait <> NIL THEN
        ok := CloseSignalSem(p^.writeWait);
    END;

    DeallocateEx(p, SIZE(PipeRecord)+bufferSize, p^.heap);
    RETURN FALSE;
END CreatePipe;

PROCEDURE ClosePipe(VAR INOUT p : Pipe) : BOOLEAN;
VAR
    ok          : BOOLEAN;
BEGIN
    EnterCriticalSection(p^.mutex);

    ok := CloseCriticalSection(p^.readLock);
    ok := CloseCriticalSection(p^.writeLock) AND ok;
    ok := CloseSignalSem(p^.readWait) AND ok;
    ok := CloseSignalSem(p^.writeWait) AND ok;

    LeaveCriticalSection(p^.mutex);

    ok := CloseCriticalSection(p^.mutex) AND ok;

    DeallocateEx(p, SIZE(PipeRecord)+p^.bufferSize, p^.heap);
    RETURN ok;
END ClosePipe;

PROCEDURE WritePipe(p : Pipe; addr : ADDRESS; amount : CARDINAL) : CARDINAL;
VAR
    messageSize         : CARDINAL;

    PROCEDURE put(addr : ADDRESS;
                  amount : ADRCARD;
                  atomic : BOOLEAN) : CARDINAL;
    VAR
        i, j            : ADRCARD;
        count           : ADRCARD;
        buffer          : BufferPointer;
        bufferSize      : ADRCARD;
        must            : CARDINAL;
        res             : WaitResult;
        data            : POINTER TO ARRAY [0..0] OF BYTE;
    BEGIN
        must := 0;
        IF atomic THEN
            must := amount;
        END;

        data := addr;
        i := 0;
        REPEAT
            (*
             this loop happens when a write occurs that is larger
             than the available buffer space.
             we "hand off" to the reader to make some room in
             the buffer for us to finish writing
             *)
            WHILE (p^.inBuffer + must) >= p^.bufferSize DO
                p^.writerWaiting := TRUE;

                LeaveCriticalSection(p^.mutex);
                res := WaitForSignalSem(p^.writeWait, SemWaitForever);
                EnterCriticalSection(p^.mutex);

                p^.writerWaiting := FALSE;

                IF res <> WaitSuccess THEN
                    RETURN i;
                END;
            END;

            j := p^.writePos;
            count := p^.inBuffer;
            buffer := p^.buffer;
            bufferSize := p^.bufferSize;
            buffer^[j] := buffer^[j];(* prime a writeback cache *)
            LOOP
                IF (i < amount) AND (count < bufferSize) THEN
                    buffer^[j] := data^[i];
                    INC(j);
                    INC(i);
                    INC(count);
                    IF j = bufferSize THEN
                        j := 0;
                    END;
                ELSE
                    EXIT;
                END;
            END;

            p^.writePos := j;
            p^.inBuffer := count;

            IF p^.readerWaiting THEN
                IF NOT SendSignalSem(p^.readWait, 1) THEN
                    (*???*)
                END;
            END;
        UNTIL i = amount;
        RETURN i;
    END put;

BEGIN
    EnterCriticalSection(p^.writeLock);(* unlimited contention, writers *)
    EnterCriticalSection(p^.mutex);(* at most one other, a reader *)

    IF p^.pipeType = MessagePipe THEN
        messageSize := amount;
        IF put(ADR(messageSize), SIZE(CARDINAL), TRUE) <> SIZE(CARDINAL) THEN
            EmptyPipeContents(p);
            LeaveCriticalSection(p^.mutex);
            LeaveCriticalSection(p^.writeLock);
            RETURN 0;
        END;
    END;

    amount := put(addr, amount, FALSE);

    LeaveCriticalSection(p^.mutex);
    LeaveCriticalSection(p^.writeLock);
    RETURN amount;
END WritePipe;

PROCEDURE ReadPipe(p : Pipe; addr : ADDRESS; amount : CARDINAL) : CARDINAL;
VAR
    <*/PUSH/NOCHECK:U*>
    messageSize : CARDINAL;
    <*/POP*>

    PROCEDURE get(addr : ADDRESS;
                  amount : ADRCARD;
                  atomic : BOOLEAN) : CARDINAL;
    VAR
        i, j            : ADRCARD;
        count           : CARDINAL;
        buffer          : BufferPointer;
        bufferSize      : ADRCARD;
        must            : CARDINAL;
        res             : WaitResult;
        data            : POINTER TO ARRAY [0..0] OF BYTE;
    BEGIN
        must := 0;
        IF atomic THEN
            must := amount;
        END;

        data := addr;
        i := 0;
        REPEAT
            WHILE (p^.inBuffer = 0) OR (p^.inBuffer < must) DO
                p^.readerWaiting := TRUE;

                LeaveCriticalSection(p^.mutex);
                res := WaitForSignalSem(p^.readWait, SemWaitForever);
                EnterCriticalSection(p^.mutex);

                p^.readerWaiting := FALSE;

                IF res <> WaitSuccess THEN
                    RETURN i;
                END;
            END;

            j := p^.readPos;
            count := p^.inBuffer;
            buffer := p^.buffer;
            bufferSize := p^.bufferSize;
            data^[i] := data^[i];(* prime a writeback cache *)
            LOOP
                IF (i < amount) AND (count > 0) THEN
                    data^[i] := buffer^[j];
                    INC(j);
                    INC(i);
                    DEC(count);
                    IF j = bufferSize THEN
                        j := 0;
                    END;
                ELSE
                    EXIT;
                END;
            END;

            p^.readPos := j;
            p^.inBuffer := count;

            IF p^.writerWaiting THEN
                IF NOT SendSignalSem(p^.writeWait, 1) THEN
                    (*???*)
                END;
            END;
        UNTIL (i = amount) OR (p^.pipeType = BytePipe);
        RETURN i;
    END get;

BEGIN
    EnterCriticalSection(p^.readLock);(* unlimited contention, readers *)
    EnterCriticalSection(p^.mutex);(* at most one other, a writer *)

    IF p^.pipeType = MessagePipe THEN
        IF p^.moreData = 0 THEN
            IF get(ADR(messageSize), SIZE(CARDINAL), TRUE) <> SIZE(CARDINAL) THEN
                EmptyPipeContents(p);
                LeaveCriticalSection(p^.mutex);
                LeaveCriticalSection(p^.readLock);
                RETURN 0;
            END;
        ELSE
            messageSize := p^.moreData;
            LeaveCriticalSection(p^.readLock);(* remove our extra lock *)
        END;
        IF messageSize < amount THEN
            amount := messageSize;
        END;
    END;

    amount := get(addr, amount, FALSE);

    IF p^.pipeType = MessagePipe THEN
        IF messageSize <= amount THEN
            p^.moreData := 0;
        ELSE
            (* all message data could not be read *)

            p^.moreData := messageSize-amount;
            INC(amount);
            EnterCriticalSection(p^.readLock);(* add an extra lock *)
        END;
    END;

    LeaveCriticalSection(p^.mutex);
    LeaveCriticalSection(p^.readLock);
    RETURN amount;
END ReadPipe;

PROCEDURE PeekPipe(p : Pipe; addr : ADDRESS; amount : CARDINAL) : CARDINAL;
VAR
    <*/PUSH/NOCHECK:U*>
    messageSize : CARDINAL;
    <*/POP*>
    count       : CARDINAL;

    PROCEDURE get(addr : ADDRESS;
                  amount : ADRCARD;
                  atomic : BOOLEAN) : CARDINAL;
    VAR
        i, j            : ADRCARD;
        count           : CARDINAL;
        buffer          : BufferPointer;
        bufferSize      : ADRCARD;
        must            : CARDINAL;
        data            : POINTER TO ARRAY [0..0] OF BYTE;
    BEGIN
        must := 0;
        IF atomic THEN
            must := amount;
        END;

        data := addr;
        i := 0;
        IF p^.inBuffer >= must THEN
            j := p^.readPos;
            count := p^.inBuffer;
            buffer := p^.buffer;
            bufferSize := p^.bufferSize;
            data^[i] := data^[i];(* prime a writeback cache *)
            LOOP
                IF (i < amount) AND (count > 0) THEN
                    data^[i] := buffer^[j];
                    INC(j);
                    INC(i);
                    DEC(count);
                    IF j = bufferSize THEN
                        j := 0;
                    END;
                ELSE
                    EXIT;
                END;
            END;
        END;
        RETURN i;
    END get;

BEGIN
    EnterCriticalSection(p^.readLock);
    EnterCriticalSection(p^.mutex);

    IF p^.pipeType = MessagePipe THEN
        IF p^.moreData = 0 THEN
            count := get(ADR(messageSize), SIZE(CARDINAL), TRUE);
            IF count = 0 THEN
                LeaveCriticalSection(p^.mutex);
                LeaveCriticalSection(p^.readLock);
                RETURN 0;
            ELSIF count <> SIZE(CARDINAL) THEN
                EmptyPipeContents(p);
                LeaveCriticalSection(p^.mutex);
                LeaveCriticalSection(p^.readLock);
                RETURN 0;
            END;
        ELSE
            messageSize := p^.moreData;
        END;
        IF messageSize < amount THEN
            amount := messageSize;
        END;
    END;

    amount := get(addr, amount, FALSE);

    IF p^.pipeType = MessagePipe THEN
        IF messageSize > amount THEN
            INC(amount);
        END;
    END;

    LeaveCriticalSection(p^.mutex);
    LeaveCriticalSection(p^.readLock);
    RETURN amount;
END PeekPipe;

PROCEDURE PipeMessageSize(p : Pipe) : CARDINAL;
VAR
    amount      : CARDINAL;

    PROCEDURE get(addr : ADDRESS) : CARDINAL;
    VAR
        i, j    : ADRCARD;
        count   : CARDINAL;
        data    : POINTER TO ARRAY [0..0] OF BYTE;
    BEGIN
        data := addr;
        i := 0;
        IF p^.inBuffer >= SIZE(CARDINAL) THEN
            j := p^.readPos;
            count := p^.inBuffer;
            LOOP
                IF (i < SIZE(CARDINAL)) AND (count > 0) THEN
                    data^[i] := p^.buffer^[j];
                    INC(j);
                    INC(i);
                    DEC(count);
                    <*/PUSH/NOWARN:U*>
                    IF j = VAL(ADRCARD, p^.bufferSize) THEN
                    <*/POP*>
                        j := 0;
                    END;
                ELSE
                    EXIT;
                END;
            END;
        END;
        RETURN i;
    END get;

BEGIN
    EnterCriticalSection(p^.readLock);
    EnterCriticalSection(p^.mutex);

    amount := 0;
    IF p^.pipeType = MessagePipe THEN
        IF p^.moreData = 0 THEN
            IF get(ADR(amount)) <> SIZE(CARDINAL) THEN
                amount := 0;
            END;
        ELSE
            amount := p^.moreData;
        END;
    END;

    LeaveCriticalSection(p^.mutex);
    LeaveCriticalSection(p^.readLock);
    RETURN amount;
END PipeMessageSize;

PROCEDURE WriteLockPipe(p : Pipe);
BEGIN
    EnterCriticalSection(p^.writeLock);
END WriteLockPipe;

PROCEDURE UnLockPipe(p : Pipe);
BEGIN
    LeaveCriticalSection(p^.writeLock);
END UnLockPipe;

PROCEDURE PipeIsEmpty(p : Pipe) : BOOLEAN;
VAR
    count       : CARDINAL;
BEGIN
    EnterCriticalSection(p^.mutex);

    count := p^.inBuffer;

    LeaveCriticalSection(p^.mutex);

    RETURN count = 0;
END PipeIsEmpty;

PROCEDURE EmptyPipeContents(p : Pipe);
VAR
    ok          : BOOLEAN;
BEGIN
    EnterCriticalSection(p^.mutex);

    p^.readPos := 0;
    p^.writePos := 0;
    p^.inBuffer := 0;
    ok := SendSignalSem(p^.readWait, 1);
    ResetSignalSem(p^.readWait);
    ok := SendSignalSem(p^.writeWait, 1);
    ResetSignalSem(p^.writeWait);

    LeaveCriticalSection(p^.mutex);
END EmptyPipeContents;

END Pipes.
