(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE RealRandomNumbers;

<*/VALIDVERSION:PROTECT*>

%IF Windows %OR UNIX %THEN
<*/VERSION:PROTECT*>
%END

FROM SYSTEM IMPORT
    ADRCARD, IsThread;

%IF PROTECT %THEN
FROM SYSTEM IMPORT
    ATOMIC_CMPXCHG, MEMORY_FENCE, CPUCOUNT;
FROM Threads IMPORT
    YieldThread;
%END

FROM SysClock IMPORT
    DateTime, GetClock;

FROM ExStorage IMPORT
    ALLOCATE, DeallocateEx, GetHeap, HeapInfoPointer;

%IF Windows %THEN

FROM WIN32 IMPORT
    GetTickCount, GetCurrentProcessId, GetCurrentThreadId;

%ELSIF UNIX %THEN

FROM UNIX IMPORT
    getpid, getppid, getuid;

%END

CONST
    %IF PROTECT %THEN
    CacheLinePad        = 128;
    %END

    Top                 = 55; (* magic number *)
    Big                 = 4.0E15;
    Factor              = 1.0 / Big;

TYPE
    DataTable   =
        RECORD
        %IF PROTECT %THEN
        lock            : CARDINAL;
        pad             : ARRAY [1..CacheLinePad] OF CARDINAL8;
        %END
        i, j            : CARDINAL;
        <*/PUSH/NOWARN:A*>
        oldStuff        : ARRAY [1..Top] OF LONGREAL;
        <*/POP*>
        heap            : HeapInfoPointer;
        END;

VAR
    Table       : DataTable;

%IF PROTECT %THEN
PROCEDURE EnterLock(VAR INOUT lock : CARDINAL) [INLINE];
BEGIN
    WHILE ATOMIC_CMPXCHG(lock, 0, 1) <> 0 DO
        IF CPUCOUNT = 1 THEN
            YieldThread;
        END;
    END;
    MEMORY_FENCE;
END EnterLock;

PROCEDURE LeaveLock(VAR INOUT lock : CARDINAL) [INLINE];
BEGIN
    MEMORY_FENCE;
    lock := 0;
END LeaveLock;
%END

PROCEDURE GetSeed() : CARDINAL;
VAR
    dt          : DateTime;
    seed        : CARDINAL;

    %IF Windows %OR Unix %THEN
    PROCEDURE mul(seed : CARDINAL; value : CARDINAL) : CARDINAL [INLINE];
    BEGIN
        <*/PUSH/NOCHECK:O*>
        RETURN seed * (value + ORD(value = 0));
        <*/POP*>
    END mul;
    %END

BEGIN
    GetClock(dt);

    <*/PUSH/NOCHECK:O*>

    seed := dt.fractions + (1000 * (dt.second + (60 * (dt.minute + (60 * dt.hour)))));

    seed := seed * dt.day;
    seed := seed * dt.month;
    seed := seed * dt.year;

    %IF Windows %THEN
        seed := mul(seed, GetTickCount());
        seed := mul(seed, GetCurrentThreadId());
        seed := mul(seed, GetCurrentProcessId());
    %ELSIF UNIX %THEN
        seed := mul(seed, getpid());
        seed := mul(seed, getppid());
        seed := mul(seed, getuid());
    %END

    seed := seed * 9821 + 1;
    seed := seed * 9821 + 1;

    <*/POP*>

    RETURN seed;
END GetSeed;

PROCEDURE DoRandomize(VAR OUT table : DataTable; seed : CARDINAL32);
VAR
    k, kk       : ADRCARD;
    i, j        : ADRCARD;
    num         : LONGREAL;
    temp        : LONGREAL;
BEGIN
    %IF PROTECT %THEN
        EnterLock(table.lock);
    %END

    IF seed = 0 THEN
        seed := GetSeed();
    END;

    num := LFLOAT(seed) + 16080331608033.0;
    IF num >= 0.0 THEN
        num := num - (Big * LFLOAT(TRUNC(num / Big)));
    ELSE
        num := Big - ABS(num) + (Big * LFLOAT(TRUNC(ABS(num) / Big)));
    END;
    table.oldStuff[Top] := num;
    temp := 1.0;
    FOR k := 1 TO Top-1 DO
        kk := (21 * k) REM Top;
        table.oldStuff[kk] := temp;
        temp := num - temp;
        IF temp < 0.0 THEN
            temp := temp + Big;
        END;
        num := table.oldStuff[kk];
    END;
    table.i := 0;
    table.j := 31; (* magic number *)

    (* warm up the generator *)

    i := table.i;
    j := table.j;
    FOR k := 1 TO 4*Top DO
        INC(i);
        INC(j);
        IF i > Top THEN
            i := 1;
        END;
        IF j > Top THEN
            j := 1;
        END;
        table.oldStuff[i] := table.oldStuff[i] - table.oldStuff[j];
        IF table.oldStuff[i] < 0.0 THEN
            table.oldStuff[i] := table.oldStuff[i] + Big;
        END;
    END;
    table.i := i;
    table.j := j;

    %IF PROTECT %THEN
        LeaveLock(table.lock);
    %END
END DoRandomize;

PROCEDURE Randomize(seed : CARDINAL32);
BEGIN
    DoRandomize(Table, seed);
END Randomize;

PROCEDURE DoRandom(VAR INOUT table : DataTable) : LONGREAL;
VAR
    num         : LONGREAL;
BEGIN
    %IF PROTECT %THEN
        EnterLock(table.lock);
    %END

    INC(table.i);
    INC(table.j);
    IF table.i > Top THEN
        table.i := 1;
    END;
    IF table.j > Top THEN
        table.j := 1;
    END;
    table.oldStuff[table.i] := table.oldStuff[table.i] - table.oldStuff[table.j];
    IF table.oldStuff[table.i] < 0.0 THEN
        table.oldStuff[table.i] := table.oldStuff[table.i] + Big;
    END;

    num := table.oldStuff[table.i] * Factor;

    %IF PROTECT %THEN
        LeaveLock(table.lock);
    %END

    RETURN num;
END DoRandom;

PROCEDURE Random() : LONGREAL;
BEGIN
    RETURN DoRandom(Table);
END Random;

TYPE
    RandomHandle        = POINTER TO DataTable;

PROCEDURE SeedEx(h : RandomHandle; seed : CARDINAL);
BEGIN
    DoRandomize(h^, seed);
END SeedEx;

PROCEDURE RandomizeEx(seed : CARDINAL32) : RandomHandle;
VAR
    ptr         : RandomHandle;
BEGIN
    NEW(ptr);
    IF ptr <> NIL THEN
        %IF PROTECT %THEN
            ptr^.lock := 0;
        %END
        ptr^.heap := GetHeap();

        SeedEx(ptr, seed);
    END;
    RETURN ptr;
END RandomizeEx;

PROCEDURE DisposeRandomHandle(VAR INOUT h : RandomHandle);
BEGIN
    DeallocateEx(h, SIZE(h^), h^.heap);
END DisposeRandomHandle;

PROCEDURE RandomEx(h : RandomHandle) : LONGREAL;
BEGIN
    RETURN DoRandom(h^);
END RandomEx;

BEGIN
    IF NOT IsThread THEN
        %IF PROTECT %THEN
            Table.lock := 0;
        %END
        Table.heap := NIL;

        Randomize(0);
    END;
END RealRandomNumbers.
