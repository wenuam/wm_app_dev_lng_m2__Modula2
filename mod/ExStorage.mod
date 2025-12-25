(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE ExStorage;
<*/INLINE:N/NOWARN:F/NOPACK/OPT:TN*>

<*/VALIDVERSION:HaveSem*>

<*/VALIDVERSION:ALIGN8*>
%IF Bits64 %OR (Bits32 %AND (%NOT IA32)) %THEN
    <*/VERSION:ALIGN8*>
%END

<*/VALIDVERSION:UseMalloc*>(*Unix systems*)
%IF UNIX %THEN
    <*/VERSION:UseMalloc*>
%END

<*/VALIDVERSION:DirectAlloc*>
%IF Windows %OR UNIX %THEN
    <*/VERSION:DirectAlloc*>(*requires 4-byte alignment minimum*)
%END

FROM SYSTEM IMPORT
    IsThread,
    ADDRESS, ADR, ADRCARD, CAST, ADDADR, SUBADR, OFFS, ASSERT, TSIZE,
    UNREFERENCED_PARAMETER;

FROM EXCEPTIONS IMPORT
	ExceptionSource, IsCurrentSource, AllocateSource, CurrentNumber, RaiseRTL;

FROM M2EXCEPTION IMPORT
    M2Exceptions, IsM2Exception, M2Exception;

%IF Windows %THEN

FROM SYSTEM IMPORT
	FUNC;

FROM WIN32 IMPORT
    ULONG, HANDLE, HeapCompatibilityInformation, HeapAlloc, HeapFree, GetProcessHeap,
    CRITICAL_SECTION, InitializeCriticalSection, DeleteCriticalSection,
    EnterCriticalSection, LeaveCriticalSection,
    DWORD, FARPROC, HINSTANCE, LoadLibrary, FreeLibrary, GetProcAddress, HeapSetInformation;

FROM WINX IMPORT
    NULL_HINSTANCE;

%ELSIF UNIX %THEN

FROM UNIX IMPORT
    %IF MultiThreadLib %THEN
    errno,
    %END
    %IF UseMalloc %THEN
    malloc, free;
    %ELSE
    PROT_READ, PROT_WRITE, (*PROT_EXEC,*)
    MAP_PRIVATE, MAP_ANONYMOUS,
    MAP_FAILED,
    mmap, munmap, sbrk;
    %END

%IF MultiThreadLib %THEN
FROM PTHREADS IMPORT
    pthread_mutexattr_t, pthread_mutex_t, PTHREAD_MUTEX_RECURSIVE,
    pthread_mutex_init, pthread_mutex_destroy,
    pthread_mutex_lock, pthread_mutex_unlock,
    pthread_mutexattr_init, pthread_mutexattr_settype;
%END

%END


TYPE
    BlockSizeType = ADRCARD;

    <*/PUSH/PACK/WARN:A*>
    (* we will manually enforce alignment for these.
       therefore no alignment warning is acceptable.
    *)

    MemoryChunkPtr      = POINTER TO MemoryChunk;
    MemoryChunk         = RECORD
                              size      : BlockSizeType;
                              next      : MemoryChunkPtr;
                              (* must be 8 byte aligned for ALIGN8
                              userData  : ARRAY [0..?] OF CARDINAL;
                              permBlock : CARDINAL;
                              *)
                          END;

    (* when a block is deallocated, this is how it looks.
      the block size includes the size field.
      *)
    FreeBlockPtr        = POINTER TO FreeBlock;
    FreeBlock           = RECORD
                              size      : BlockSizeType;
                              %IF ALIGN8 %AND (%NOT Bits64) %THEN
                              align8pad : CARDINAL32;
                              %END
                              (* double linked circular for speed of removal *)
                              next      : FreeBlockPtr;
                              prev      : FreeBlockPtr;
                          END;

    (* when a block is allocated, this is how it looks.
       the block size includes the size field.
       *)
    AllocBlock          = RECORD
                              size      : BlockSizeType;
                              %IF ALIGN8 %AND (%NOT Bits64) %THEN
                              align8pad : CARDINAL32;
                              %END

                              (* this case is "short allocated"
                                 depending on the debug mode of the heap.
                              *)
                              CASE (*debug*): BOOLEAN OF
                              FALSE:
                                  (* this is where the user memory would be
                                     must be 8 byte aligned for ALIGN8
                                  userData  : ARRAY [0..?] OF CARDINAL;
                                  *)
                              |
                              TRUE:
                                  allocId       : CARDINAL32;
                                  underwrite    : CARDINAL32;
                                  (* this is where the user memory would be
                                     must be 8 byte aligned for ALIGN8
                                  userData  : ARRAY [0..?] OF CARDINAL;
                                  *)
                              END;
                          END;
    AllocBlockPtr       = POINTER TO AllocBlock;

    %IF DirectAlloc %THEN
    DirectAllocPtr      = POINTER TO DirectAllocBlock;
    DirectAllocBlock    = RECORD
                              (* double linked circular for speed of removal *)
                              next      : DirectAllocPtr;
                              prev      : DirectAllocPtr;

                              alloc     : AllocBlock;
                          END;
    %END
    <*/POP*>

    HeapInfoPointer = POINTER TO HeapInfo;
    HeapInfo =
        RECORD
            freeList            : FreeBlockPtr;
            chunkList           : MemoryChunkPtr;
            %IF DirectAlloc %THEN
            directList          : DirectAllocPtr;
            directBlocks        : CARDINAL;
            %END
            heapError           : HeapErrorProc;
            srcAlloc            : MemorySourceAlloc;
            srcDealloc          : MemorySourceDealloc;
            srcAvail            : MemorySourceAvail;
            stopLeak            : StopLeakCallbackProc;
            userData            : ADDRESS;
            %IF Windows %THEN
                sem             : CRITICAL_SECTION;
            %ELSIF UNIX %THEN
                %IF MultiThreadLib %THEN
                sem             : pthread_mutex_t;
                %END
            %END
            memoryInUse,
            maxMemoryUsed,
            heapSize,
            maxSize,
            chunkSize,
            minBlockSize        : BlockSizeType;
            freeBlocks          : CARDINAL;
            allocId             : CARDINAL32;
            stopLeakId          : CARDINAL32;
            strategy            : AllocStrategy;
            combine             : CombineStrategy;
            debug               : BOOLEAN;
            frozen              : BOOLEAN;
            protected           : BOOLEAN;
            deallocated         : BOOLEAN;
            apiOverwriteScan    : BOOLEAN;
            doDirectAlloc       : BOOLEAN;
            threadSafe          : BOOLEAN;

            heapUsed            : BOOLEAN;
        END;

    HeapArrayPointer    = POINTER TO ARRAY [0..0] OF HeapInfo;

CONST
    %IF IA32 %THEN
        %IF Bits32 %THEN
            OverwriteDetect         = 0BAADF00Dh;
            AllocPageSize           = 4 * 1024;
            MaxHeapSize             = 3 * 1024 * 1024 * 1024;
            MaxChunkSize            = 1 * 1024 * 1024 * 1024;
            DefaultChunkSize        = 64 * 1024;
        %ELSE
            fix me
        %END
    %ELSIF AMD64 %THEN
        %IF Bits64 %THEN
            OverwriteDetect         = 0BAADF00Dh;
            AllocPageSize           = 4 * 1024;
            MaxHeapSize             = 64 * 1024 * 1024 * 1024;
            MaxChunkSize            = 4 * 1024 * 1024 * 1024;
            DefaultChunkSize        = 64 * 1024;
        %ELSE
            fix me
        %END
    %ELSE
        fix me
    %END

    BlockAllocFlag      = 1;
    %IF DirectAlloc %THEN
    BlockDirectFlag     = 2;
    %END
    FlagsMask           = VAL(BlockSizeType, BlockOverhead-1);

    Sentinel            = BlockOverhead BOR BlockAllocFlag;

    ChunkOverheadFront  = SIZE(MemoryChunk);
    ChunkOverheadTail   = BlockOverhead;
    ChunkOverhead       = ChunkOverheadFront + ChunkOverheadTail;

    DebugOverheadFront  = SIZE(CARDINAL32) + SIZE(CARDINAL32);
    DebugOverheadTail   = SIZE(CARDINAL32);
    DebugOverhead       = DebugOverheadFront + DebugOverheadTail;

    %IF DirectAlloc %THEN
    DirectAllocOverhead = OFFS(DirectAllocBlock.alloc);
    %END

    MinBlockSize        = SIZE(FreeBlock);
    MaxBlockSize        = MaxChunkSize -
                          ChunkOverhead(*tracking info*) -
                          BlockOverhead(*per alloc overhead*) -
                          DebugOverhead(*when debug mode is on *);

    (* assert certain alignment restrictions *)

    ASSERT(TSIZE(AllocBlock, FALSE) = BlockOverhead);
    ASSERT((TSIZE(AllocBlock, TRUE) REM BlockOverhead) = 0);
    ASSERT(OFFS(FreeBlock.next) = BlockOverhead);
    ASSERT((ChunkOverheadFront REM BlockOverhead) = 0);
    ASSERT((ChunkOverheadTail REM BlockOverhead) = 0);
    ASSERT((DebugOverheadFront REM BlockOverhead) = 0);
    %IF DirectAlloc %THEN
    ASSERT((SIZE(DirectAllocBlock) REM BlockOverhead) = 0);
    %END

VAR
    CurHeap             : HeapInfoPointer;
    DefaultHeap         : HeapInfoPointer;
    StorageSrc          : ExceptionSource;

    Heaps               : HeapArrayPointer;
    MaxHeaps            : ADRCARD;
    NumHeapPages        : CARDINAL;

    HeapStack           : ARRAY [1..8] OF HeapInfoPointer;
    HeapSP              : CARDINAL;

%IF WINDOWS %THEN
    StorageSem          : CRITICAL_SECTION;
    SemCount            : CARDINAL;

    SetSpinCount        : PROCEDURE(VAR CRITICAL_SECTION, DWORD) : DWORD [WINDOWS];
    ProcessHeap         : HANDLE;

%ELSIF UNIX %THEN
    %IF MultiThreadLib %THEN
    StorageSem          : pthread_mutex_t;
    SemCount            : CARDINAL;
    SemAttr             : pthread_mutexattr_t;
    %END

    %IF %NOT UseMalloc %THEN
    sbrkBase            : ADDRESS;
    %END
%END

PROCEDURE RequestHeapSem(heap : HeapInfoPointer) [INLINE];
BEGIN
    (* if this is implemented, HaveSem must be defined *)

    IF heap^.threadSafe THEN
        %IF Windows %THEN
            <*/VERSION:HaveSem*>
            EnterCriticalSection(heap^.sem);

        %ELSIF UNIX %THEN
            %IF MultiThreadLib %THEN
                <*/VERSION:HaveSem*>
                pthread_mutex_lock(heap^.sem);
            %END
        %END
    END;
END RequestHeapSem;

PROCEDURE ReleaseHeapSem(heap : HeapInfoPointer) [INLINE];
BEGIN
    IF heap^.threadSafe THEN
        %IF Windows %THEN
            LeaveCriticalSection(heap^.sem);
        %ELSIF UNIX %THEN
            %IF MultiThreadLib %THEN
                pthread_mutex_unlock(heap^.sem);
            %END
        %END
    END;
END ReleaseHeapSem;

PROCEDURE RequestSem() [INLINE];
BEGIN
    %IF Windows %THEN
        EnterCriticalSection(StorageSem);
        INC(SemCount);
    %ELSIF UNIX %THEN
        %IF MultiThreadLib %THEN
            pthread_mutex_lock(StorageSem);
            INC(SemCount);
        %END
    %END
END RequestSem;

PROCEDURE ReleaseSem() [INLINE];
BEGIN
    %IF Windows %THEN
        IF SemCount > 0 THEN
            DEC(SemCount);
        END;
        LeaveCriticalSection(StorageSem);
    %ELSIF UNIX %THEN
        %IF MultiThreadLib %THEN
            IF SemCount > 0 THEN
                DEC(SemCount);
            END;
            pthread_mutex_unlock(StorageSem);
        %END
    %END
END ReleaseSem;

PROCEDURE BlockIsAllocated(size : BlockSizeType) : BOOLEAN [INLINE];
BEGIN
    RETURN (size BAND BlockAllocFlag) <> 0;
END BlockIsAllocated;

%IF DirectAlloc %THEN
PROCEDURE BlockIsDirect(size : BlockSizeType) : BOOLEAN [INLINE];
BEGIN
    RETURN (size BAND BlockDirectFlag) <> 0;
END BlockIsDirect;
%END

PROCEDURE ZapFlags(VAR INOUT size : BlockSizeType) [INLINE];
BEGIN
    size := size BAND (BNOT FlagsMask);
END ZapFlags;

PROCEDURE GetBlockSize(size : BlockSizeType) : BlockSizeType [INLINE];
BEGIN
    RETURN size BAND (BNOT FlagsMask);
END GetBlockSize;

PROCEDURE CheckOverwriteScan(heap : HeapInfoPointer) [INLINE];
BEGIN
    IF heap^.debug AND heap^.apiOverwriteScan THEN
        ScanForMemoryOverwritesEx(heap);
    END;
END CheckOverwriteScan;

PROCEDURE ComputeBlockSize(heap : HeapInfoPointer; size : BlockSizeType) : BlockSizeType [INLINE];
BEGIN
    (* adjust for the debug overhead *)

    IF heap^.debug THEN
        size := size + DebugOverhead;
    END;

    (* adjust size for overhead and align *)

    size := size + ((2*BlockOverhead)-1);
    size := size BAND CAST(BlockSizeType, -BlockOverhead);

    (* adjust for minimum *)

    IF size < heap^.minBlockSize THEN
        size := heap^.minBlockSize;
    END;

    RETURN size;
END ComputeBlockSize;

<*/GROUPLIBPROCS:Y*>
PROCEDURE OSALLOC(size : ADRCARD; userData : ADDRESS) : ADDRESS;
%IF UNIX %THEN
%IF %NOT UseMalloc %THEN
VAR
    addr        : ADDRESS;
%END

%END
BEGIN
    UNREFERENCED_PARAMETER(userData);

    %IF Windows %THEN
        RETURN HeapAlloc(ProcessHeap, 0, size);

    %ELSIF UNIX %THEN
        %IF UseMalloc %THEN
            RETURN malloc(size);
        %ELSE
            addr := mmap(NIL,(*system has total control of the returned address *)
                         size,
                         PROT_READ BOR PROT_WRITE(* BOR PROT_EXEC*),
                         MAP_PRIVATE BOR MAP_ANONYMOUS,
                         -1(*file handle*),
                         0(*offset*));
            IF addr <> MAP_FAILED THEN
                RETURN addr;
            ELSE
                (* Linux will not mmap all memory address space when we pass
                   NIL to mmap. (kernel <= 2.4.3 and likely later).
                   why the hell not is beyond me.
                   try again suggesting an address at the base sbrk address.
                *)
                addr := mmap(sbrkBase,
                             size,
                             PROT_READ BOR PROT_WRITE(* BOR PROT_EXEC*),
                             MAP_PRIVATE BOR MAP_ANONYMOUS,
                             -1(*file handle*),
                             0(*offset*));
                IF addr <> MAP_FAILED THEN
                    RETURN addr;
                END;
            END;
            RETURN NIL;
        %END
    %ELSE
        fix me;
    %END
END OSALLOC;

PROCEDURE OSDEALLOC(addr : ADDRESS; size : ADRCARD; userData : ADDRESS);
BEGIN
    UNREFERENCED_PARAMETER(userData);
    UNREFERENCED_PARAMETER(size);

    %IF Windows %THEN
        HeapFree(ProcessHeap, 0, addr);

    %ELSIF UNIX %THEN
        %IF UseMalloc %THEN
            free(addr);
        %ELSE
            munmap(addr, size);
        %END

    %ELSE
        UNREFERENCED_PARAMETER(addr);
    %END
END OSDEALLOC;

PROCEDURE OSAVAIL(userData : ADDRESS) : ADRCARD;
BEGIN
    UNREFERENCED_PARAMETER(userData);
    RETURN 0;
END OSAVAIL;

PROCEDURE SourceAlloc(heap : HeapInfoPointer; VAR INOUT size : BlockSizeType) : ADDRESS;
VAR
    lSize       : BlockSizeType;
    save1       : BlockSizeType;
    addr        : ADDRESS;
BEGIN
    lSize := size + ChunkOverhead;
    IF lSize < heap^.chunkSize THEN
        lSize := heap^.chunkSize;
    END;

    IF heap^.heapSize+lSize > heap^.maxSize THEN
        lSize := heap^.maxSize-heap^.heapSize;
    END;

    (* compute size in page increments *)

    save1 := lSize;
    lSize := lSize / AllocPageSize;

    IF (save1 REM AllocPageSize) <> 0 THEN
        INC(lSize);
    END;
    <*/PUSH/NOCHECK:O*>(*sixteenbit *)
    size := lSize * AllocPageSize;
    <*/POP*>

    addr := heap^.srcAlloc(size, heap^.userData);
    RETURN addr;
END SourceAlloc;

PROCEDURE FillBlock(addr : ADDRESS);
VAR
    size        : BlockSizeType;
    ptr         : POINTER TO ARRAY [0..3] OF CARDINAL32;
    ptrBlk      : POINTER TO BlockSizeType;
BEGIN
    (* get the block size *)

    ptrBlk := addr;
    size := ptrBlk^;

    (* take out the overhead below the underwrite indicator *)

    size := size - OFFS(AllocBlock.underwrite);
    ptr := ADDADR(addr, OFFS(AllocBlock.underwrite));

    (* and get the number of CARDINAL units to fill *)

    size := size / SIZE(CARDINAL32);
    size := size - 2;(* over and under write *)

    (* insert the lower memory overwrite detect *)

    ptr^[0] := OverwriteDetect;
    ptr := ADDADR(ptr, SIZE(CARDINAL32));

    (* fill the user area with nil *)

    LOOP
        IF size >= 4 THEN
            size := size - 4;
            ptr^[0] := CAST(CARDINAL32, NIL);
            ptr^[1] := CAST(CARDINAL32, NIL);
            ptr^[2] := CAST(CARDINAL32, NIL);
            ptr^[3] := CAST(CARDINAL32, NIL);
            ptr := ADDADR(ptr, 4*SIZE(CARDINAL32));
            IF size = 0 THEN
                EXIT;
            END;
        ELSIF size >= 2 THEN
            size := size - 2;
            ptr^[0] := CAST(CARDINAL32, NIL);
            ptr^[1] := CAST(CARDINAL32, NIL);
            ptr := ADDADR(ptr, 2*SIZE(CARDINAL32));
            IF size = 0 THEN
                EXIT;
            END;
        ELSIF size > 0 THEN
            ptr^[0] := CAST(CARDINAL32, NIL);
            ptr := ADDADR(ptr, SIZE(CARDINAL32));
            EXIT;
        ELSE
            EXIT;
        END;
    END;

    ptr^[0] := OverwriteDetect;
END FillBlock;

PROCEDURE DoFullCombine(heap : HeapInfoPointer);
VAR
    saveLock    : BOOLEAN;
BEGIN
    saveLock := heap^.frozen;
    heap^.frozen := TRUE;
    FindBlockCF(heap, MAX(BlockSizeType));
    heap^.frozen := saveLock;
    heap^.deallocated := FALSE;
END DoFullCombine;

PROCEDURE InList(heap : HeapInfoPointer; combblock : FreeBlockPtr) : BOOLEAN;
VAR
    i   : CARDINAL;
    ptr : FreeBlockPtr;
BEGIN
    ptr := heap^.freeList;
    i := heap^.freeBlocks;
    LOOP
        IF i <> 0 THEN
            DEC(i);
            IF ptr <> combblock THEN
                ptr := ptr^.next;
            ELSE
                RETURN TRUE;
            END;
        ELSE
            EXIT;
        END;
    END;
    RETURN FALSE;
END InList;

PROCEDURE RemoveBlock(heap : HeapInfoPointer; block : FreeBlockPtr);
BEGIN
    IF (heap^.freeBlocks = 0) OR (heap^.debug AND NOT InList(heap, block)) THEN
        RaiseRTL(StorageSrc, ORD(heapCorrupt), "HEAP-CORRUPT");
    END;

    DEC(heap^.freeBlocks);
    IF heap^.freeBlocks <> 0 THEN
        block^.prev^.next := block^.next;
        block^.next^.prev := block^.prev;

        IF heap^.freeList = block THEN
            heap^.freeList := heap^.freeList^.next;
        END;
    ELSE
        heap^.freeList := NIL;
    END;
END RemoveBlock;

PROCEDURE InsertBlock(heap : HeapInfoPointer; block : FreeBlockPtr);
BEGIN
    INC(heap^.freeBlocks);
    IF heap^.freeList <> NIL THEN
        block^.next := heap^.freeList;
        block^.prev := heap^.freeList^.prev;
        block^.prev^.next := block;
        block^.next^.prev := block;

        heap^.freeList := block;
    ELSE
        block^.next := block;
        block^.prev := block;
        heap^.freeList := block;
    END;
END InsertBlock;

PROCEDURE AddBlock(heap : HeapInfoPointer; ptr : MemoryChunkPtr; size : BlockSizeType);
VAR
    perm        : FreeBlockPtr;
    block       : FreeBlockPtr;
BEGIN
    (* build our list of allocated blocks from the OS *)

    ptr^.size := size;
    ptr^.next := heap^.chunkList;
    heap^.chunkList := ptr;
    heap^.heapSize := heap^.heapSize + size;

    (* now setup the allocatable block *)

    block := ADDADR(ptr, ChunkOverheadFront);
    block^.size := size - ChunkOverhead;

    (* put a permanently allocated block at the end of this chunk *)
    (* so that block combining is trivial *)

    perm := ADDADR(block, block^.size);
    perm^.size := Sentinel;

    (* put it in the FreeList *)

    InsertBlock(heap, block);
END AddBlock;

PROCEDURE GetMore(heap : HeapInfoPointer; size : BlockSizeType) : BOOLEAN;
VAR
    block               : ADDRESS;
    mustHaveSize        : BlockSizeType;
BEGIN
    IF (NOT heap^.frozen) AND (heap^.heapSize < heap^.maxSize) THEN
        mustHaveSize := heap^.minBlockSize+ChunkOverhead;
        block := SourceAlloc(heap, size);

        IF (block = NIL) OR (size < mustHaveSize) THEN
            IF block <> NIL THEN
                heap^.srcDealloc(block, size, heap^.userData);
            END;
            RETURN FALSE;
        END;

        AddBlock(heap, block, size);
        RETURN TRUE;
    END;
    RETURN FALSE;
END GetMore;

%IF DirectAlloc %THEN
PROCEDURE DirectAlloc(heap : HeapInfoPointer; amount : BlockSizeType) : ADDRESS;
VAR
    lSize       : BlockSizeType;
    block       : DirectAllocPtr;
    ptrC        : POINTER TO ARRAY [0..3] OF CARDINAL32;
    count       : CARDINAL;
BEGIN
    (* the block and debug overhead have already been included *)

    lSize := amount + DirectAllocOverhead;

    IF (heap^.heapSize + lSize) <= heap^.maxSize THEN
        block := heap^.srcAlloc(lSize, heap^.userData);
        IF block <> NIL THEN
            block^.alloc.size := lSize BOR (BlockAllocFlag BOR BlockDirectFlag);

            IF heap^.directList <> NIL THEN
                block^.next := heap^.directList;
                block^.prev := heap^.directList^.prev;
                block^.prev^.next := block;
                block^.next^.prev := block;
            ELSE
                block^.next := block;
                block^.prev := block;
            END;
            heap^.directList := block;
            INC(heap^.directBlocks);

            INC(heap^.heapSize, lSize);
            INC(heap^.memoryInUse, lSize);
            IF heap^.memoryInUse > heap^.maxMemoryUsed THEN
                heap^.maxMemoryUsed := heap^.memoryInUse;
            END;

            IF NOT heap^.debug THEN
                RETURN ADDADR(block, DirectAllocOverhead+BlockOverhead);
            ELSE
                INC(heap^.allocId);
                block^.alloc.allocId := heap^.allocId;

                IF heap^.stopLeakId = heap^.allocId THEN
                    heap^.stopLeak(heap^.stopLeakId);
                END;

                block^.alloc.underwrite := OverwriteDetect;

                ptrC := ADDADR(block, DirectAllocOverhead+BlockOverhead+DebugOverheadFront);

                count := (amount-(BlockOverhead+DebugOverhead)) / SIZE(CARDINAL32);
                LOOP
                    IF count >= 4 THEN
                        count := count - 4;
                        ptrC^[0] := CAST(CARDINAL32, NIL);
                        ptrC^[1] := CAST(CARDINAL32, NIL);
                        ptrC^[2] := CAST(CARDINAL32, NIL);
                        ptrC^[3] := CAST(CARDINAL32, NIL);
                        ptrC := ADDADR(ptrC, 4*SIZE(CARDINAL32));
                        IF count = 0 THEN
                            EXIT;
                        END;
                    ELSIF count >= 2 THEN
                        count := count - 2;
                        ptrC^[0] := CAST(CARDINAL32, NIL);
                        ptrC^[1] := CAST(CARDINAL32, NIL);
                        ptrC := ADDADR(ptrC, 2*SIZE(CARDINAL32));
                        IF count = 0 THEN
                            EXIT;
                        END;
                    ELSIF count > 0 THEN
                        ptrC^[0] := CAST(CARDINAL32, NIL);
                        ptrC := ADDADR(ptrC, SIZE(CARDINAL32));
                        EXIT;
                    ELSE
                        EXIT;
                    END;
                END;

                ptrC^[0] := OverwriteDetect;

                RETURN ADDADR(block, DirectAllocOverhead+BlockOverhead+DebugOverheadFront);
            END;
        END;
    END;
    RETURN NIL;
END DirectAlloc;
%END

PROCEDURE FindBlockNCF(heap : HeapInfoPointer; size : BlockSizeType) : ADDRESS;
VAR
    i           : CARDINAL;
    testblock   : FreeBlockPtr;
BEGIN
    REPEAT
        i := heap^.freeBlocks;
        testblock := heap^.freeList;

        LOOP
            IF i <> 0 THEN
                DEC(i);

                IF testblock^.size < size THEN
                    testblock := testblock^.next;
                ELSE
                    RETURN testblock;
                END;
            ELSE
                EXIT;
            END;
        END;
    UNTIL NOT GetMore(heap, size);
    RETURN NIL;
END FindBlockNCF;

PROCEDURE FindBlockB(heap : HeapInfoPointer; size : BlockSizeType) : ADDRESS;
VAR
    i           : CARDINAL;
    testblock   : FreeBlockPtr;
    combblock   : FreeBlockPtr;
    bestB       : FreeBlockPtr;
    bestD       : BlockSizeType;
    diff        : BlockSizeType;
BEGIN
    REPEAT
        (* loop to find the best block *)

        i := heap^.freeBlocks;
        testblock := heap^.freeList;

        bestB := NIL;
        bestD := MAX(BlockSizeType);

        LOOP
            IF i <> 0 THEN
                DEC(i);

                combblock := ADDADR(testblock, testblock^.size);

                IF BlockIsAllocated(combblock^.size) THEN
                    IF testblock^.size < size THEN
                        testblock := testblock^.next;
                    ELSE
                        diff := testblock^.size - size;
                        IF diff <> 0 THEN
                            IF diff < bestD THEN
                                bestD := diff;
                                bestB := testblock;
                            END;
                            testblock := testblock^.next;
                        ELSE
                            RETURN testblock;
                        END;
                    END;
                ELSE
                    (* block is free, combine the blocks *)

                    INC(testblock^.size, combblock^.size);
                    RemoveBlock(heap, combblock);
                    INC(i);

                    IF combblock = bestB THEN
                        (* ouch, we just removed our best block *)
                        (* restart the search to get the best block *)

                        bestB := NIL;
                        bestD := MAX(BlockSizeType);
                        i := heap^.freeBlocks;
                    END;
                END;
            ELSE
                EXIT;
            END;
        END;

        IF bestB <> NIL THEN
            RETURN bestB;
        END;
    UNTIL NOT GetMore(heap, size);
    RETURN NIL;
END FindBlockB;

PROCEDURE FindBlockCF(heap : HeapInfoPointer; size : BlockSizeType) : ADDRESS;
VAR
    i           : CARDINAL;
    testblock   : FreeBlockPtr;
    combblock   : FreeBlockPtr;
BEGIN
    REPEAT
        i := heap^.freeBlocks;
        testblock := heap^.freeList;

        LOOP
            IF i <> 0 THEN
                DEC(i);

                IF testblock^.size <> size THEN
                    combblock := ADDADR(testblock, testblock^.size);

                    IF BlockIsAllocated(combblock^.size) THEN
                        IF testblock^.size < size THEN
                            testblock := testblock^.next;
                        ELSE
                            RETURN testblock;
                        END;
                    ELSE
                        (* block is free, combine the blocks *)

                        INC(testblock^.size, combblock^.size);
                        RemoveBlock(heap, combblock);
                        INC(i);(*???*)
                    END;
                ELSE
                    RETURN testblock;
                END;
            ELSE
                EXIT;
            END;
        END;
    UNTIL NOT GetMore(heap, size);
    RETURN NIL;
END FindBlockCF;

PROCEDURE ALLOCATE(VAR OUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    AllocateEx(addr, amount, CurHeap);
END ALLOCATE;

PROCEDURE AllocateEx(VAR OUT addr : ADDRESS;
                     amount : ADRCARD;
                     heap : HeapInfoPointer);
VAR
    lSize       : BlockSizeType;
    testblock   : FreeBlockPtr;
    allocPtr    : AllocBlockPtr;
BEGIN
    RequestHeapSem(heap);

    CheckOverwriteScan(heap);

    IF amount <= MaxBlockSize THEN
        lSize := ComputeBlockSize(heap, amount);

        %IF DirectAlloc %THEN
        IF (lSize < heap^.chunkSize) OR (NOT heap^.doDirectAlloc) THEN
        %END
            IF heap^.strategy = FirstFit THEN
                IF heap^.combine = NormalCombine THEN
                    testblock := FindBlockCF(heap, lSize);
                ELSIF heap^.combine = NoCombine THEN
                    testblock := FindBlockNCF(heap, lSize);
                ELSE
                    IF heap^.deallocated THEN
                        DoFullCombine(heap);
                    END;
                    testblock := FindBlockNCF(heap, lSize);
                END;
            ELSE
                IF (heap^.combine = FullCombine) AND heap^.deallocated THEN
                    DoFullCombine(heap);
                END;
                testblock := FindBlockB(heap, lSize);
            END;

            IF testblock <> NIL THEN
                IF (testblock^.size - lSize) > heap^.minBlockSize THEN
                    testblock^.size := testblock^.size - lSize;
                    testblock := ADDADR(testblock, testblock^.size);
                    testblock^.size := lSize;
                ELSE
                    RemoveBlock(heap, testblock);
                END;

                heap^.deallocated := FALSE;
                INC(heap^.memoryInUse, testblock^.size);
                IF heap^.memoryInUse > heap^.maxMemoryUsed THEN
                    heap^.maxMemoryUsed := heap^.memoryInUse;
                END;

                IF NOT heap^.debug THEN
                    INC(heap^.allocId);
                    (* make the blocksize odd, means its allocated *)

                    testblock^.size := testblock^.size BOR BlockAllocFlag;
                    addr := ADDADR(testblock, BlockOverhead);

                    ReleaseHeapSem(heap);
                    RETURN;
                ELSE
                    FillBlock(testblock);
                    allocPtr := CAST(AllocBlockPtr, testblock);
                    INC(heap^.allocId);
                    allocPtr^.allocId := heap^.allocId;

                    IF heap^.stopLeakId = heap^.allocId THEN
                        heap^.stopLeak(heap^.stopLeakId);
                    END;

                    testblock^.size := testblock^.size BOR BlockAllocFlag;
                    addr := ADDADR(testblock, BlockOverhead+DebugOverheadFront);

                    ReleaseHeapSem(heap);
                    RETURN;
                END;
            END;
        %IF DirectAlloc %THEN
        ELSE
            addr := DirectAlloc(heap, lSize);
            IF addr <> NIL THEN
                ReleaseHeapSem(heap);
                RETURN;
            END;
        END;
        %END
    END;

    CASE heap^.heapError(amount) OF
    ReturnNIL:
        addr := NIL;
    |
    DoException:
        RaiseRTL(StorageSrc, ORD(outOfStorage), "OUT-OF-STORAGE");
    |
    TryAgain:
        AllocateEx(addr, amount, heap);
    END;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END AllocateEx;

%IF DirectAlloc %THEN
PROCEDURE DirectDealloc(heap : HeapInfoPointer; addr : ADDRESS; amount : BlockSizeType);
VAR
    block       : DirectAllocPtr;
    ptr         : DirectAllocPtr;
    count       : CARDINAL;
BEGIN
    block := SUBADR(addr, OFFS(DirectAllocBlock.alloc));

    IF NOT BlockIsAllocated(block^.alloc.size) THEN
        RaiseRTL (StorageSrc, ORD(pointerToUnallocatedStorage), "WRONG-UNALLOCATE-FREE");
    END;
    ZapFlags(block^.alloc.size);

    IF heap^.debug THEN
        amount := ComputeBlockSize(heap, amount) + DirectAllocOverhead;
        IF block^.alloc.size <> amount THEN
            RaiseRTL (StorageSrc, ORD(wrongStorageToUnallocate), "WRONG-UNALLOCATE-SIZE");
        END;

        count := heap^.directBlocks;
        ptr := heap^.directList;
        LOOP
            IF count <> 0 THEN
                DEC(count);
                IF ptr <> block THEN
                    ptr := ptr^.next;
                ELSE
                    EXIT;
                END;
            ELSE
                RaiseRTL (StorageSrc, ORD(deallocateToWrongHeap), "WRONG-UNALLOCATE-HEAP");
            END;
        END;
    END;

    IF heap^.directBlocks > 0 THEN
        DEC(heap^.directBlocks);
        IF heap^.directBlocks <> 0 THEN
            block^.prev^.next := block^.next;
            block^.next^.prev := block^.prev;

            IF heap^.directList = block THEN
                heap^.directList := heap^.directList^.next;
            END;
        ELSE
            heap^.directList := NIL;
        END;

        DEC(heap^.memoryInUse, block^.alloc.size);
        DEC(heap^.heapSize, block^.alloc.size);

        heap^.srcDealloc(block, block^.alloc.size, heap^.userData);
    ELSE
        RaiseRTL (StorageSrc, ORD(heapCorrupt), "HEAP-CORRUPT");
    END;
END DirectDealloc;
%END

PROCEDURE DEALLOCATE(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    DeallocateEx(addr, amount, CurHeap);
END DEALLOCATE;

<*/GROUPLIBPROCS:N*>(*will be grouped with above procs*)
PROCEDURE DeallocateEx(VAR INOUT addr : ADDRESS;
                       amount : ADRCARD;
                       heap : HeapInfoPointer);
VAR
    laddr       : AllocBlockPtr;
    ptr         : MemoryChunkPtr;
    ptrC        : POINTER TO CARDINAL32;
    lSize       : BlockSizeType;
    laddrc      : BlockSizeType;
    st          : BlockSizeType;
BEGIN
    RequestHeapSem(heap);

    CheckOverwriteScan(heap);

    IF addr = NIL THEN
        RaiseRTL (StorageSrc, ORD(nilDeallocation), "NIL-DEALLOCATE");
    END;

    laddr := SUBADR(addr, BlockOverhead);

    IF heap^.debug THEN
        laddr := SUBADR(laddr, DebugOverheadFront);

        %IF DirectAlloc %THEN
            IF BlockIsDirect(laddr^.size) THEN
                DirectDealloc(heap, laddr, amount);
                addr := NIL;
                ReleaseHeapSem(heap);
                RETURN;
            END;
        %END

        IF laddr^.underwrite <> OverwriteDetect THEN
            RaiseRTL (StorageSrc, ORD(memoryOverwrite), "MEMORY-OVERWRITE-FRONT");
        END;

        laddrc := CAST(BlockSizeType, laddr);
        ptr := heap^.chunkList;
        WHILE ptr <> NIL DO
            st := CAST(BlockSizeType, ptr);

            IF (laddrc > st) AND (laddrc < st+ptr^.size) THEN
                BREAK;
            END;

            ptr := ptr^.next;
        END;

        IF ptr = NIL THEN
            RaiseRTL (StorageSrc, ORD(deallocateToWrongHeap), "WRONG-UNALLOCATE-HEAP");
        END;

    %IF DirectAlloc %THEN
    ELSIF BlockIsDirect(laddr^.size) THEN
        DirectDealloc(heap, laddr, amount);
        addr := NIL;
        ReleaseHeapSem(heap);
        RETURN;
    %END
    END;

    IF NOT BlockIsAllocated(laddr^.size) THEN
        RaiseRTL (StorageSrc, ORD(pointerToUnallocatedStorage), "WRONG-UNALLOCATE-FREE");
    END;
    ZapFlags(laddr^.size);

    IF heap^.debug THEN
        ptrC := ADDADR(laddr, laddr^.size-SIZE(CARDINAL32));
        IF ptrC^ <> OverwriteDetect THEN
            RaiseRTL (StorageSrc, ORD(memoryOverwrite), "MEMORY-OVERWRITE-TAIL");
        END;

        lSize := ComputeBlockSize(heap, amount);

        IF (laddr^.size < lSize) OR (laddr^.size-lSize > heap^.minBlockSize) THEN
            RaiseRTL (StorageSrc, ORD(wrongStorageToUnallocate), "WRONG-UNALLOCATE-SIZE");
        END;

        FillBlock(laddr);
    END;

    heap^.deallocated := TRUE;
    DEC(heap^.memoryInUse, laddr^.size);

    InsertBlock(heap, CAST(FreeBlockPtr, laddr));

    addr := NIL;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END DeallocateEx;

PROCEDURE AvailableEx(amount : ADRCARD; heap : HeapInfoPointer) : BOOLEAN;
VAR
    testblock   : FreeBlockPtr;
    lSize       : ADRCARD;
BEGIN
    RequestHeapSem(heap);

    CheckOverwriteScan(heap);

    lSize := ComputeBlockSize(heap, amount);

    IF heap^.strategy = FirstFit THEN
        CASE heap^.combine OF
        NormalCombine:
            testblock := FindBlockCF(heap, lSize);
        |
        FullCombine:
            IF heap^.deallocated THEN
                DoFullCombine(heap);
            END;
            testblock := FindBlockNCF(heap, lSize);
        |
        NoCombine:
            testblock := FindBlockNCF(heap, lSize);
        END;
    ELSE
        IF (heap^.combine = FullCombine) AND heap^.deallocated THEN
            DoFullCombine(heap);
        END;
        testblock := FindBlockB(heap, lSize);
    END;

    ReleaseHeapSem(heap);

    RETURN testblock <> NIL;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END AvailableEx;

PROCEDURE Available(amount : ADRCARD) : BOOLEAN;
BEGIN
    RETURN AvailableEx(amount, CurHeap);
END Available;

PROCEDURE IsStorageException() : BOOLEAN;
BEGIN
    RETURN IsCurrentSource(StorageSrc);
END IsStorageException;

PROCEDURE StorageException() : StorageExceptions;
BEGIN
    RETURN VAL(StorageExceptions, CurrentNumber(StorageSrc));
END StorageException;

PROCEDURE ReallocateEx(VAR INOUT addr : ADDRESS;
                       amount : ADRCARD;
                       heap : HeapInfoPointer);
VAR
    newBlock    : ADDRESS;
    ptrS, ptrD  : POINTER TO CARDINAL32;
    oldAmount   : BlockSizeType;
    block       : FreeBlockPtr;
BEGIN
    RequestHeapSem(heap);

    AllocateEx(newBlock, amount, heap);

    IF (newBlock <> NIL) AND (addr <> NIL) THEN
        IF NOT heap^.debug THEN
            block := SUBADR(newBlock, BlockOverhead);
            amount := block^.size-BlockOverhead-1;
            block := SUBADR(addr, BlockOverhead);
            oldAmount := block^.size-BlockOverhead-1;
        ELSE
            block := SUBADR(newBlock, BlockOverhead + DebugOverheadFront);
            amount := block^.size-(BlockOverhead+DebugOverhead)-1;
            block := SUBADR(addr, BlockOverhead + DebugOverheadFront);
            oldAmount := block^.size-(BlockOverhead+DebugOverhead)-1;
        END;

        (* now copy the memory in CARDINAL units *)

        IF oldAmount < amount THEN
            amount := oldAmount;
        END;
        amount := amount / SIZE(CARDINAL32);
        ptrS := addr;
        ptrD := newBlock;
        LOOP
            IF amount <> 0 THEN
                DEC(amount);
                ptrD^ := ptrS^;
                ptrD := ADDADR(ptrD, SIZE(CARDINAL32));
                ptrS := ADDADR(ptrS, SIZE(CARDINAL32));

                IF amount <> 0 THEN
                    DEC(amount);
                    ptrD^ := ptrS^;
                    ptrD := ADDADR(ptrD, SIZE(CARDINAL32));
                    ptrS := ADDADR(ptrS, SIZE(CARDINAL32));
                ELSE
                    EXIT;
                END;
            ELSE
                EXIT;
            END;
        END;

        DeallocateEx(addr, oldAmount, heap);
    END;

    addr := newBlock;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END ReallocateEx;

PROCEDURE ReALLOCATE(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    ReallocateEx(addr, amount, CurHeap);
END ReALLOCATE;

PROCEDURE AllocateAlignedEx(VAR OUT addr, alignedAddr : ADDRESS;
                            amountIn, alignIn : ADRCARD;
                            heap : HeapInfoPointer);
VAR
    compute     : ADRCARD;
    align       : BlockSizeType;
    amount      : BlockSizeType;
BEGIN
    align := alignIn;
    amount := amountIn;
    IF align = 0 THEN
        align := 1;
    END;
    AllocateEx(addr, amount + align, heap);
    IF addr <> NIL THEN
        compute := CAST(ADRCARD, addr);
        compute := (compute + (align-1)) / align * align;
        alignedAddr := CAST(ADDRESS, compute);
    END;
END AllocateAlignedEx;

PROCEDURE AllocateAligned(VAR OUT addr, alignedAddr : ADDRESS;
                          amount, align : ADRCARD);
BEGIN
    AllocateAlignedEx(addr, alignedAddr, amount, align, CurHeap);
END AllocateAligned;

PROCEDURE MaxAvailableEx(heap : HeapInfoPointer) : ADRCARD;
VAR
    i           : CARDINAL;
    lSize       : BlockSizeType;
    testblock   : FreeBlockPtr;
BEGIN
    RequestHeapSem(heap);

    lSize := 0;
    IF NOT heap^.frozen THEN
        lSize := heap^.srcAvail(heap^.userData);
        IF lSize >= ChunkOverhead THEN
            lSize := lSize - ChunkOverhead;
        ELSE
            lSize := 0;
        END;
    END;

    IF lSize > heap^.maxSize-heap^.heapSize THEN
        lSize := heap^.maxSize-heap^.heapSize;
    END;

    IF lSize < MaxBlockSize THEN
        CombineHeapEx(heap);

        i := heap^.freeBlocks;
        testblock := heap^.freeList;

        WHILE i <> 0 DO
            IF testblock^.size-BlockOverhead > lSize THEN
                lSize := testblock^.size-BlockOverhead;
            END;
            DEC(i);
            testblock := testblock^.next;
        END;
    END;

    ReleaseHeapSem(heap);

    IF lSize > MaxBlockSize THEN
        RETURN MaxBlockSize;
    ELSE
        RETURN lSize;
    END;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END MaxAvailableEx;

PROCEDURE MaxAvailable() : ADRCARD;
BEGIN
    RETURN MaxAvailableEx(CurHeap);
END MaxAvailable;

PROCEDURE MemAvailableEx(heap : HeapInfoPointer) : ADRCARD;
VAR
    i           : CARDINAL;
    lSize       : BlockSizeType;
    testblock   : FreeBlockPtr;
BEGIN
    RequestHeapSem(heap);

    lSize := 0;
    IF NOT heap^.frozen THEN
        lSize := heap^.srcAvail(heap^.userData);
        IF lSize >= ChunkOverhead THEN
            lSize := lSize - ChunkOverhead;
        ELSE
            lSize := 0;
        END;
    END;

    IF lSize > heap^.maxSize-heap^.heapSize THEN
        lSize := heap^.maxSize-heap^.heapSize;
    END;

    CombineHeapEx(heap);

    i := heap^.freeBlocks;
    testblock := heap^.freeList;

    WHILE i <> 0 DO
        INC(lSize, testblock^.size - BlockOverhead);
        DEC(i);
        testblock := testblock^.next;
    END;

    ReleaseHeapSem(heap);

    RETURN lSize;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END MemAvailableEx;

PROCEDURE MemAvailable() : ADRCARD;
BEGIN
    RETURN MemAvailableEx(CurHeap);
END MemAvailable;

PROCEDURE MemoryInUseEx(heap : HeapInfoPointer) : ADRCARD;
VAR
    val         : BlockSizeType;
    i           : ADRCARD;
    temp        : HeapInfoPointer;
BEGIN
    IF heap <> NIL THEN
        RequestHeapSem(heap);
        val := heap^.memoryInUse;
        ReleaseHeapSem(heap);
    ELSE
        RequestSem;

        val := 0;
        FOR i := 0 TO MaxHeaps-1 DO
            temp := ADR(Heaps^[i]);

            IF temp^.heapUsed THEN
                RequestHeapSem(temp);
                val := val + temp^.memoryInUse;
                ReleaseHeapSem(temp);
            END;
        END;

        ReleaseSem;
    END;


    RETURN val;

%IF HaveSem %THEN
EXCEPT
    IF heap <> NIL THEN
        ReleaseHeapSem(heap);
    ELSE
        ReleaseHeapSem(temp);
        ReleaseSem;
    END;
%END
END MemoryInUseEx;

PROCEDURE MemoryInUse() : ADRCARD;
BEGIN
    RETURN MemoryInUseEx(CurHeap)
END MemoryInUse;

PROCEDURE MaxMemoryUsedEx(heap : HeapInfoPointer) : ADRCARD;
VAR
    val         : BlockSizeType;
    i           : ADRCARD;
    temp        : HeapInfoPointer;
BEGIN
    IF heap <> NIL THEN
        RequestHeapSem(heap);
        val := heap^.maxMemoryUsed;
        ReleaseHeapSem(heap);
    ELSE
        RequestSem;

        val := 0;
        FOR i := 0 TO MaxHeaps-1 DO
            temp := ADR(Heaps^[i]);

            IF temp^.heapUsed THEN
                RequestHeapSem(temp);
                val := val + temp^.maxMemoryUsed;
                ReleaseHeapSem(temp);
            END;
        END;

        ReleaseSem;
    END;

    RETURN val;

%IF HaveSem %THEN
EXCEPT
    IF heap <> NIL THEN
        ReleaseHeapSem(heap);
    ELSE
        ReleaseHeapSem(temp);
        ReleaseSem;
    END;
%END
END MaxMemoryUsedEx;

PROCEDURE MaxMemoryUsed() : ADRCARD;
BEGIN
    RETURN MaxMemoryUsedEx(CurHeap);
END MaxMemoryUsed;

PROCEDURE ResetMaxMemoryUsedEx(heap : HeapInfoPointer);
VAR
    i           : ADRCARD;
    temp        : HeapInfoPointer;
BEGIN
    IF heap <> NIL THEN
        RequestHeapSem(heap);
        heap^.maxMemoryUsed := 0;
        ReleaseHeapSem(heap);
    ELSE
        RequestSem;

        FOR i := 0 TO MaxHeaps-1 DO
            temp := ADR(Heaps^[i]);

            IF temp^.heapUsed THEN
                RequestHeapSem(temp);
                temp^.maxMemoryUsed := 0;
                ReleaseHeapSem(temp);
            END;
        END;

        ReleaseSem;
    END;
END ResetMaxMemoryUsedEx;

PROCEDURE ResetMaxMemoryUsed;
BEGIN
    ResetMaxMemoryUsedEx(CurHeap);
END ResetMaxMemoryUsed;

PROCEDURE HeapMemoryEx(heap : HeapInfoPointer) : ADRCARD;
VAR
    val         : BlockSizeType;
    i           : ADRCARD;
    temp        : HeapInfoPointer;
BEGIN
    IF heap <> NIL THEN
        RequestHeapSem(heap);
        val := heap^.heapSize;
        ReleaseHeapSem(heap);
    ELSE
        RequestSem;

        val := 0;
        FOR i := 0 TO MaxHeaps-1 DO
            temp := ADR(Heaps^[i]);

            IF temp^.heapUsed THEN
                RequestHeapSem(temp);
                val := val + temp^.heapSize;
                ReleaseHeapSem(temp);
            END;
        END;

        ReleaseSem;
    END;

    RETURN val;

%IF HaveSem %THEN
EXCEPT
    IF heap <> NIL THEN
        ReleaseHeapSem(heap);
    ELSE
        ReleaseHeapSem(temp);
        ReleaseSem;
    END;
%END
END HeapMemoryEx;

PROCEDURE HeapMemory() : ADRCARD;
BEGIN
    RETURN HeapMemoryEx(CurHeap);
END HeapMemory;

PROCEDURE FreezeHeapEx(yes : BOOLEAN; heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);

    heap^.frozen := yes;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END FreezeHeapEx;

PROCEDURE FreezeHeap(fix : BOOLEAN);
BEGIN
    FreezeHeapEx(fix, CurHeap);
END FreezeHeap;

PROCEDURE CombineHeapEx(heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);

    DoFullCombine(heap);

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END CombineHeapEx;

PROCEDURE CombineHeap;
BEGIN
    CombineHeapEx(CurHeap);
END CombineHeap;

PROCEDURE FreeHeapEx(heap : HeapInfoPointer);
VAR
    save        : ADDRESS;
    addr        : ADDRESS;
    size        : BlockSizeType;
    %IF DirectAlloc %THEN
    direct      : DirectAllocPtr;
    %END
BEGIN
    RequestHeapSem(heap);

    WHILE heap^.chunkList <> NIL DO
        save := heap^.chunkList^.next;

        size := heap^.chunkList^.size;
        addr := heap^.chunkList;
        heap^.srcDealloc(addr, size, heap^.userData);

        heap^.chunkList := save;
    END;

    %IF DirectAlloc %THEN
        (* free the blocks nack to the source *)

        direct := heap^.directList;
        WHILE heap^.directBlocks <> 0 DO
            save := direct^.next;

            ZapFlags(direct^.alloc.size);
            heap^.srcDealloc(direct, direct^.alloc.size, heap^.userData);
            DEC(heap^.directBlocks);

            direct := save;
        END;
        heap^.directList := NIL;
    %END

    heap^.freeBlocks := 0;
    heap^.memoryInUse := 0;
    (*heap^.maxMemoryUsed := 0;*)
    heap^.heapSize := 0;
    heap^.freeList := NIL;
    heap^.chunkList := NIL;
    heap^.deallocated := FALSE;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END FreeHeapEx;

PROCEDURE FreeHeap;
BEGIN
    FreeHeapEx(CurHeap);
END FreeHeap;

PROCEDURE ClearHeapEx(heap : HeapInfoPointer);
VAR
    ptr         : MemoryChunkPtr;
    save        : ADDRESS;
    %IF DirectAlloc %THEN
    direct      : DirectAllocPtr;
    %END
BEGIN
    RequestHeapSem(heap);

    ptr := heap^.chunkList;
    heap^.freeList := NIL;
    heap^.freeBlocks := 0;
    heap^.heapSize := 0;
    heap^.chunkList := NIL;
    heap^.deallocated := FALSE;

    (* build the new segment list *)

    WHILE ptr <> NIL DO
        save := ptr^.next;
        AddBlock(heap, ptr, ptr^.size);
        ptr := save;
    END;

    %IF DirectAlloc %THEN
        (* free the blocks nack to the source *)

        direct := heap^.directList;
        WHILE heap^.directBlocks <> 0 DO
            save := direct^.next;

            ZapFlags(direct^.alloc.size);
            heap^.srcDealloc(direct, direct^.alloc.size, heap^.userData);
            DEC(heap^.directBlocks);

            direct := save;
        END;
        heap^.directList := NIL;
    %END

    heap^.memoryInUse := 0;
    (*heap^.maxMemoryUsed := 0;*)

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END ClearHeapEx;

PROCEDURE ClearHeap;
BEGIN
    ClearHeapEx(CurHeap);
END ClearHeap;

PROCEDURE SetMemorySourceEx(alloc : MemorySourceAlloc;
                            dealloc : MemorySourceDealloc;
                            avail : MemorySourceAvail;
                            userData : ADDRESS;
                            heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);

    IF heap^.chunkList = NIL THEN
        heap^.srcAlloc := alloc;
        heap^.srcDealloc := dealloc;
        heap^.srcAvail := avail;
        heap^.userData := userData;
    END;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END SetMemorySourceEx;

PROCEDURE SetMemorySource(alloc : MemorySourceAlloc;
                          dealloc : MemorySourceDealloc;
                          avail : MemorySourceAvail;
                          userData : ADDRESS);
BEGIN
    SetMemorySourceEx(alloc, dealloc, avail, userData, CurHeap);
END SetMemorySource;

PROCEDURE SetHeapErrorEx(err : HeapErrorProc; heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);

    IF err = HeapErrorNIL THEN
        heap^.heapError := StdHeapErrorNIL;
    ELSIF err = HeapErrorException THEN
        heap^.heapError := StdHeapErrorException;
    ELSE
        heap^.heapError := err;
    END;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END SetHeapErrorEx;

PROCEDURE SetHeapError(err : HeapErrorProc);
BEGIN
    SetHeapErrorEx(err, CurHeap);
END SetHeapError;

PROCEDURE GetHeapErrorEx(heap : HeapInfoPointer) : HeapErrorProc;
VAR
    err         : HeapErrorProc;
BEGIN
    RequestHeapSem(heap);

    err := heap^.heapError;

    ReleaseHeapSem(heap);

    RETURN err;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END GetHeapErrorEx;

PROCEDURE GetHeapError() : HeapErrorProc;
BEGIN
    RETURN GetHeapErrorEx(CurHeap);
END GetHeapError;

PROCEDURE GetHeapMaxEx(heap : HeapInfoPointer) : ADRCARD;
VAR
    max : ADRCARD;
BEGIN
    RequestHeapSem(heap);

    max := heap^.maxSize;

    ReleaseHeapSem(heap);

    RETURN max;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END GetHeapMaxEx;

PROCEDURE GetHeapMax() : ADRCARD;
BEGIN
    RETURN GetHeapMaxEx(CurHeap);
END GetHeapMax;

PROCEDURE SetHeapMaxEx(max : ADRCARD; heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);

    IF max <> 0 THEN
        heap^.maxSize := max;
    ELSE
        heap^.maxSize := MaxHeapSize;
    END;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END SetHeapMaxEx;

PROCEDURE SetHeapMax(max : ADRCARD);
BEGIN
    SetHeapMaxEx(max, CurHeap);
END SetHeapMax;

PROCEDURE GetChunkSizeEx(heap : HeapInfoPointer) : ADRCARD;
VAR
    chunk       : ADRCARD;
BEGIN
    RequestHeapSem(heap);

    chunk := heap^.chunkSize;

    ReleaseHeapSem(heap);

    RETURN chunk;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END GetChunkSizeEx;

PROCEDURE GetChunkSize() : ADRCARD;
BEGIN
    RETURN GetChunkSizeEx(CurHeap);
END GetChunkSize;

PROCEDURE SetChunkSizeEx(chunk : ADRCARD; direct : BOOLEAN; heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);

    heap^.chunkSize := chunk;
    %IF DirectAlloc %THEN
        heap^.doDirectAlloc := direct;
    %ELSE
        UNREFERENCED_PARAMETER(direct);
    %END

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END SetChunkSizeEx;

PROCEDURE SetChunkSize(chunk : ADRCARD; direct : BOOLEAN);
BEGIN
    SetChunkSizeEx(chunk, direct, CurHeap);
END SetChunkSize;

PROCEDURE GetMinAllocSizeEx(heap : HeapInfoPointer) : ADRCARD;
VAR
    min : ADRCARD;
BEGIN
    RequestHeapSem(heap);

    min := heap^.minBlockSize-BlockOverhead;
    IF heap^.debug THEN
        min := min - DebugOverhead;
    END;

    ReleaseHeapSem(heap);

    RETURN min;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END GetMinAllocSizeEx;

PROCEDURE GetMinAllocSize() : ADRCARD;
BEGIN
    RETURN GetMinAllocSizeEx(CurHeap);
END GetMinAllocSize;

PROCEDURE SetMinAllocSizeEx(min : ADRCARD; heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);

    IF heap^.debug THEN
        min := min + DebugOverhead;
    END;

    (* add overhead and align *)

    min := min + ((2*BlockOverhead)-1);
    min := min BAND CAST(ADRCARD, -BlockOverhead);

    IF min < MinBlockSize THEN
        min := MinBlockSize;
    END;

    heap^.minBlockSize := min;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END SetMinAllocSizeEx;

PROCEDURE SetMinAllocSize(min : ADRCARD);
BEGIN
    SetMinAllocSizeEx(min, CurHeap);
END SetMinAllocSize;

PROCEDURE GetDebugEx(heap : HeapInfoPointer) : BOOLEAN;
VAR
    debug       : BOOLEAN;
BEGIN
    RequestHeapSem(heap);

    debug := heap^.debug;

    ReleaseHeapSem(heap);

    RETURN debug;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END GetDebugEx;

PROCEDURE GetDebug() : BOOLEAN;
BEGIN
    RETURN GetDebugEx(CurHeap);
END GetDebug;

PROCEDURE SetDebugEx(debug : BOOLEAN; heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);

    IF (debug <> heap^.debug) AND (heap^.chunkList = NIL) THEN
        heap^.debug := debug;
        IF debug THEN
            heap^.minBlockSize := heap^.minBlockSize + DebugOverhead;
        ELSE
            heap^.minBlockSize := heap^.minBlockSize - DebugOverhead;
        END;
    END;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END SetDebugEx;

PROCEDURE SetDebug(debug : BOOLEAN);
BEGIN
    SetDebugEx(debug, CurHeap);
END SetDebug;

PROCEDURE ScanForMemoryOverwritesEx(heap : HeapInfoPointer);
VAR
    i           : CARDINAL;
    testblock   : FreeBlockPtr;
    chunk       : MemoryChunkPtr;
    ptr         : AllocBlockPtr;
    ptrC        : POINTER TO CARDINAL32;
    size        : BlockSizeType;
    %IF DirectAlloc %THEN
    direct      : DirectAllocPtr;
    count       : CARDINAL;
    %END
BEGIN
    RequestHeapSem(heap);

    IF heap^.debug THEN
        (* check the internal data structures for errors *)

        i := heap^.freeBlocks;
        testblock := heap^.freeList;
        WHILE i <> 0 DO
            (* for freelist blocks, check the pointers *)
            (* if they point back to us then we are ok *)
            (* if we are wacked, then the pointer dereferences *)
            (* may cause a fault, we trap the faults below *)

            IF (testblock^.next^.prev <> testblock) OR
               (testblock^.prev^.next <> testblock)
            THEN
                RaiseRTL (StorageSrc, ORD(memoryOverwrite), "MEMORY-OVERWRITE-FREELIST");
            END;

            DEC(i);
            testblock := testblock^.next;
        END;

        (* now check allocated memory blocks *)

        chunk := heap^.chunkList;
        WHILE chunk <> NIL DO
            ptr := ADDADR(chunk, ChunkOverheadFront);
            WHILE ptr^.size <> Sentinel DO
                IF NOT BlockIsAllocated(ptr^.size) THEN
                    ptr := ADDADR(ptr, ptr^.size);
                ELSE
                    (* check the lower overwrite detect *)

                    IF ptr^.underwrite <> OverwriteDetect THEN
                        RaiseRTL (StorageSrc, ORD(memoryOverwrite), "MEMORY-OVERWRITE-FRONT");
                    END;

                    size := GetBlockSize(ptr^.size);

                    (* check the upper overwrite detect *)

                    ptrC := ADDADR(ptr, size-SIZE(CARDINAL32));
                    IF ptrC^ <> OverwriteDetect THEN
                        RaiseRTL (StorageSrc, ORD(memoryOverwrite), "MEMORY-OVERWRITE-TAIL");
                    END;

                    ptr := ADDADR(ptr, size);
                END;
            END;

            chunk := chunk^.next;
        END;

        %IF DirectAlloc %THEN
            direct := heap^.directList;
            count := heap^.directBlocks;
            WHILE count <> 0 DO
                DEC(count);

                IF (direct^.next^.prev <> direct) OR (direct^.prev^.next <> direct) THEN
                    RaiseRTL (StorageSrc, ORD(memoryOverwrite), "MEMORY-OVERWRITE-FREELIST");
                END;

                IF direct^.alloc.underwrite <> OverwriteDetect THEN
                    RaiseRTL (StorageSrc, ORD(memoryOverwrite), "MEMORY-OVERWRITE-FRONT");
                END;

                size := GetBlockSize(direct^.alloc.size);

                ptrC := ADDADR(direct, size-SIZE(CARDINAL32));
                IF ptrC^ <> OverwriteDetect THEN
                    RaiseRTL (StorageSrc, ORD(memoryOverwrite), "MEMORY-OVERWRITE-TAIL");
                END;

                direct := direct^.next;
            END;
        %END
    END;

    ReleaseHeapSem(heap);

EXCEPT
    ReleaseHeapSem(heap);

    IF IsM2Exception() THEN
        IF M2Exception() = sysException THEN
            (* the pointer check faulted *)
            (* or a size field got whacked *)
            (* change this into a memoryOverwrite exception *)
            (* this only works where the M2 exception system traps *)
            (* faults *)

            RaiseRTL (StorageSrc, ORD(memoryOverwrite), "MEMORY-OVERWRITE");
        END;
    END;
END ScanForMemoryOverwritesEx;

PROCEDURE ScanForMemoryOverwrites;
BEGIN
    ScanForMemoryOverwritesEx(CurHeap);
END ScanForMemoryOverwrites;

PROCEDURE ScanForMemoryOverwritesOnApiCallsEx(yes : BOOLEAN;
                                              heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);
    heap^.apiOverwriteScan := yes;
    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END ScanForMemoryOverwritesOnApiCallsEx;

PROCEDURE ScanForMemoryOverwritesOnApiCalls(yes : BOOLEAN);
BEGIN
    ScanForMemoryOverwritesOnApiCallsEx(yes, CurHeap);
END ScanForMemoryOverwritesOnApiCalls;

PROCEDURE CheckForMemoryLeaksEx(callback : CheckLeakCallbackProc; heap : HeapInfoPointer);
VAR
    chunk       : MemoryChunkPtr;
    ptr         : AllocBlockPtr;
    size        : BlockSizeType;
    %IF DirectAlloc %THEN
    direct      : DirectAllocPtr;
    count       : CARDINAL;
    %END
BEGIN
    RequestHeapSem(heap);

    IF heap^.debug THEN
        CombineHeapEx(heap);

        chunk := heap^.chunkList;
        WHILE chunk <> NIL DO
            ptr := ADDADR(chunk, ChunkOverheadFront);
            WHILE ptr^.size <> Sentinel DO
                IF NOT BlockIsAllocated(ptr^.size) THEN
                    ptr := ADDADR(ptr, ptr^.size);
                ELSE
                    size := GetBlockSize(ptr^.size);
                    callback(ptr^.allocId, size);
                    ptr := ADDADR(ptr, size);
                END;
            END;

            chunk := chunk^.next;
        END;

        %IF DirectAlloc %THEN
            (* anything in the direct list at this point is a leak *)

            direct := heap^.directList;
            count := heap^.directBlocks;
            WHILE count <> 0 DO
                DEC(count);

                size := GetBlockSize(direct^.alloc.size);
                callback(direct^.alloc.allocId, size);

                direct := direct^.next;
            END;
        %END
    END;

    ReleaseHeapSem(heap);

EXCEPT
    ReleaseHeapSem(heap);
END CheckForMemoryLeaksEx;

PROCEDURE CheckForMemoryLeaks(callback : CheckLeakCallbackProc);
BEGIN
    CheckForMemoryLeaksEx(callback, CurHeap);
END CheckForMemoryLeaks;

PROCEDURE SetMemoryStopLeakCallbackEx(callback : StopLeakCallbackProc;
                                      allocId : CARDINAL32;
                                      heap : HeapInfoPointer);
BEGIN
    heap^.stopLeak := callback;
    heap^.stopLeakId := allocId;
END SetMemoryStopLeakCallbackEx;

PROCEDURE SetMemoryStopLeakCallback(callback : StopLeakCallbackProc;
                                    allocId : CARDINAL32);
BEGIN
    SetMemoryStopLeakCallbackEx(callback, allocId, CurHeap);
END SetMemoryStopLeakCallback;

PROCEDURE GetCombineEx(heap : HeapInfoPointer) : CombineStrategy;
VAR
    comb        : CombineStrategy;
BEGIN
    RequestHeapSem(heap);

    comb := heap^.combine;

    ReleaseHeapSem(heap);

    RETURN comb;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END GetCombineEx;

PROCEDURE GetCombine() : CombineStrategy;
BEGIN
    RETURN GetCombineEx(CurHeap);
END GetCombine;

PROCEDURE SetCombineEx(comb : CombineStrategy; heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);

    heap^.combine := comb;
    IF comb = NoCombine THEN
        heap^.strategy := FirstFit;
    END;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END SetCombineEx;

PROCEDURE SetCombine(comb : CombineStrategy);
BEGIN
    SetCombineEx(comb, CurHeap);
END SetCombine;

PROCEDURE GetStrategyEx(heap : HeapInfoPointer) : AllocStrategy;
VAR
    s   : AllocStrategy;
BEGIN
    RequestHeapSem(heap);

    s := heap^.strategy;

    ReleaseHeapSem(heap);

    RETURN s;

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END GetStrategyEx;

PROCEDURE GetStrategy() : AllocStrategy;
BEGIN
    RETURN GetStrategyEx(CurHeap);
END GetStrategy;

PROCEDURE SetStrategyEx(s : AllocStrategy; heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);

    heap^.strategy := s;
    IF s = BestFit THEN
        IF heap^.combine = NoCombine THEN
            heap^.combine := NormalCombine;
        END;
    END;

    ReleaseHeapSem(heap);

%IF HaveSem %THEN
EXCEPT
    ReleaseHeapSem(heap);
%END
END SetStrategyEx;

PROCEDURE SetStrategy(s : AllocStrategy);
BEGIN
    SetStrategyEx(s, CurHeap);
END SetStrategy;

PROCEDURE SetThreadSafeEx(yes : BOOLEAN; heap : HeapInfoPointer);
BEGIN
    heap^.threadSafe := yes;
END SetThreadSafeEx;

PROCEDURE SetThreadSafe(yes : BOOLEAN);
BEGIN
    SetThreadSafeEx(yes, CurHeap);
END SetThreadSafe;

PROCEDURE AllocHeapEx(defaultError : HeapErrorCodes) : HeapInfoPointer;
VAR
    i           : ADRCARD;
    heap        : HeapInfoPointer;
    %IF Windows %OR Unix %THEN
    oldHeaps    : HeapArrayPointer;
    oldCount    : ADRCARD;
    %END

    PROCEDURE setupHeap(i : ADRCARD) : HeapInfoPointer;
    VAR
        heap    : HeapInfoPointer;
    BEGIN
        heap := ADR(Heaps^[i]);

        heap^.frozen := FALSE;
        heap^.debug := FALSE;
        heap^.deallocated := FALSE;
        heap^.apiOverwriteScan := FALSE;
        heap^.threadSafe := TRUE;
        heap^.minBlockSize := MinBlockSize;
        heap^.allocId := 0;
        heap^.stopLeakId := 0;

        heap^.freeBlocks := 0;
        heap^.memoryInUse := 0;
        heap^.maxMemoryUsed := 0;
        heap^.heapSize := 0;
        heap^.maxSize := MaxHeapSize;
        heap^.chunkSize := DefaultChunkSize;
        heap^.freeList := NIL;
        heap^.chunkList := NIL;
        heap^.strategy := FirstFit;
        heap^.combine := NormalCombine;

        IF defaultError = DoException THEN
            heap^.heapError := StdHeapErrorException;
        ELSE
            heap^.heapError := StdHeapErrorNIL;
        END;

        %IF DirectAlloc %THEN
            heap^.directBlocks := 0;
            heap^.directList := NIL;
            heap^.doDirectAlloc := FALSE;
        %ELSE
            heap^.doDirectAlloc := FALSE;
        %END

        heap^.srcAlloc := OSALLOC;
        heap^.srcDealloc := OSDEALLOC;
        heap^.srcAvail := OSAVAIL;
        heap^.userData := NIL;

        %IF Windows %THEN
            InitializeCriticalSection(heap^.sem);
            SetSpinCount(heap^.sem, 2500);
        %ELSIF UNIX %THEN
            %IF MultiThreadLib %THEN
                IF pthread_mutex_init(heap^.sem, SemAttr) <> 0 THEN
                    RETURN NIL;
                END;
            %END
        %END

        heap^.heapUsed := TRUE;
        RETURN heap;
    END setupHeap;

BEGIN
    RequestSem;

    FOR i := 0 TO MaxHeaps-1 DO
        IF NOT Heaps^[i].heapUsed THEN
            heap := setupHeap(i);
            ReleaseSem;
            RETURN heap;
        END;
    END;

    heap := NIL;
    %IF Windows %OR Unix %THEN
        (* increase the size of the heap array *)
        oldHeaps := Heaps;
        oldCount := MaxHeaps;
        INC(NumHeapPages);
        Heaps := OSALLOC(NumHeapPages * AllocPageSize, NIL);
        IF Heaps <> NIL THEN
            MaxHeaps := (NumHeapPages * AllocPageSize) / SIZE(HeapInfo);
            FOR i := 0 TO oldCount-1 DO
                Heaps^[i] := oldHeaps^[i];
            END;
            FOR i := oldCount TO MaxHeaps-1 DO
                Heaps^[i].heapUsed := FALSE;
            END;
            OSDEALLOC(oldHeaps, (NumHeapPages-1) * AllocPageSize, NIL);

            heap := setupHeap(oldCount);
        ELSE
            Heaps := oldHeaps;
        END;
    %END
    ReleaseSem;
    RETURN heap;

EXCEPT
    ReleaseSem;
    RETURN NIL;
END AllocHeapEx;

PROCEDURE AllocHeap() : HeapInfoPointer;
BEGIN
    RETURN AllocHeapEx(ReturnNIL);
END AllocHeap;

PROCEDURE DeallocHeap(heap : HeapInfoPointer);
BEGIN
    RequestSem;
    IF heap^.heapUsed THEN
        FreeHeapEx(heap);

        heap^.heapUsed := FALSE;
        %IF Windows %THEN
            DeleteCriticalSection(heap^.sem);
        %ELSIF UNIX %THEN
            %IF MultiThreadLib %THEN
                pthread_mutex_destroy(heap^.sem);
            %END
        %END
        IF heap = CurHeap THEN
            CurHeap := NIL;
        END;
    END;
    ReleaseSem;
EXCEPT
    ReleaseSem;
END DeallocHeap;

PROCEDURE GetHeap() : HeapInfoPointer;
BEGIN
    RETURN CurHeap;
END GetHeap;

PROCEDURE GetDefaultHeap() : HeapInfoPointer;
BEGIN
    RETURN DefaultHeap;
END GetDefaultHeap;

PROCEDURE UseHeap(heap : HeapInfoPointer) : HeapInfoPointer;
VAR
    old : HeapInfoPointer;
BEGIN
    RequestSem;

    old := CurHeap;
    CurHeap := heap;

    ReleaseSem;

    RETURN old;

%IF HaveSem %THEN
EXCEPT
    ReleaseSem;
%END
END UseHeap;

PROCEDURE PushHeap(heap : HeapInfoPointer);
BEGIN
    RequestSem;

    IF HeapSP < HIGH(HeapStack) THEN
        INC(HeapSP);
        HeapStack[HeapSP] := CurHeap;

        CurHeap := heap;
    ELSE
        ReleaseSem;
        RaiseRTL (StorageSrc, ORD(heapStackOverflow), "HEAP-STACK-OVERFLOW");
    END;
END PushHeap;

PROCEDURE PopHeap;
BEGIN
    RequestSem;

    IF HeapSP > 0 THEN
        CurHeap := HeapStack[HeapSP];
        DEC(HeapSP);

        ReleaseSem;(* the one we held from the push *)

        ReleaseSem;
    ELSE
        ReleaseSem;
        RaiseRTL (StorageSrc, ORD(heapStackUnderflow), "HEAP-STACK-UNDERFLOW");
    END;
END PopHeap;

PROCEDURE ClearHeapStack;
BEGIN
    RequestSem;

    WHILE HeapSP <> 0 DO
        CurHeap := HeapStack[HeapSP];
        DEC(HeapSP);
    END;

%IF HaveSem %THEN
    WHILE SemCount > 0 DO
        ReleaseSem;
    END;
%END
END ClearHeapStack;

PROCEDURE LockHeap(heap : HeapInfoPointer);
BEGIN
    RequestHeapSem(heap);
END LockHeap;

PROCEDURE UnlockHeap(heap : HeapInfoPointer);
BEGIN
    ReleaseHeapSem(heap);
END UnlockHeap;

PROCEDURE AllocSystemMemory(VAR OUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    addr := OSALLOC(amount, NIL);(*userData param is unused*)
END AllocSystemMemory;

PROCEDURE DeallocSystemMemory(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    OSDEALLOC(addr, amount, NIL);(*userData param is unused*)
END DeallocSystemMemory;

PROCEDURE StdHeapErrorNIL(size : ADRCARD) : HeapErrorCodes;
BEGIN
    UNREFERENCED_PARAMETER(size);
    RETURN ReturnNIL;
END StdHeapErrorNIL;

PROCEDURE StdHeapErrorException(size : ADRCARD) : HeapErrorCodes;
BEGIN
    UNREFERENCED_PARAMETER(size);
    RETURN DoException;
END StdHeapErrorException;

%IF Windows %THEN
PROCEDURE NullSetSpinProc(VAR cri : CRITICAL_SECTION;
                          spin : DWORD) : DWORD [WINDOWS];
BEGIN
    UNREFERENCED_PARAMETER(cri);
    UNREFERENCED_PARAMETER(spin);
    RETURN 0;
END NullSetSpinProc;
%END

PROCEDURE Init;
VAR
    i           : ADRCARD;
    %IF Windows %THEN
    dll         : HINSTANCE;
    temp        : FARPROC;
    %ELSIF UNIX %AND MultiThreadLib %THEN
    ok          : BOOLEAN;
    %END

    PROCEDURE abort(release : BOOLEAN);
    BEGIN
        IF release THEN
            ReleaseSem;
        END;
        RaiseRTL (StorageSrc, ORD(outOfStorage), "ExStorage failed Initialization");
    END abort;

BEGIN
    AllocateSource(StorageSrc);

    Heaps := NIL;
    MaxHeaps := 0;

    %IF Windows %THEN
        SetSpinCount := NullSetSpinProc;
        dll := LoadLibrary("KERNEL32");
        IF dll <> NULL_HINSTANCE THEN
            temp := GetProcAddress(dll, "SetCriticalSectionSpinCount");
            IF CAST(ADDRESS, temp) <> NIL THEN
                SetSpinCount:FARPROC := temp;
            END;

            FreeLibrary(dll);
        END;

        InitializeCriticalSection(StorageSem);
        SemCount := 0;

        ProcessHeap := GetProcessHeap();

    %ELSIF UNIX %THEN
        %IF MultiThreadLib %THEN
            SemCount := 0;

            ok := pthread_mutexattr_init(SemAttr) = 0;
            ok := ok AND (pthread_mutexattr_settype(SemAttr, PTHREAD_MUTEX_RECURSIVE) = 0);
            ok := ok AND (pthread_mutex_init(StorageSem, SemAttr) = 0);
            IF NOT ok THEN
                errno();
                abort(FALSE);
            END;
        %END

        %IF %NOT UseMalloc %THEN
            sbrkBase := sbrk(0);
        %END
    %END

    NumHeapPages := 1;
    Heaps := OSALLOC(NumHeapPages * AllocPageSize, NIL);
    MaxHeaps := (NumHeapPages * AllocPageSize) / SIZE(HeapInfo);
    FOR i := 0 TO MaxHeaps-1 DO
        Heaps^[i].heapUsed := FALSE;
    END;
    HeapSP := 0;

    CurHeap := AllocHeap();
    DefaultHeap := CurHeap;
    IF CurHeap = NIL THEN
        abort(TRUE);
    END;
END Init;

PROCEDURE Term;
VAR
    i   : ADRCARD;
BEGIN
    IF Heaps <> NIL THEN
        FOR i := MaxHeaps-1 TO 0 BY -1 DO
            IF Heaps^[i].heapUsed THEN
                FreeHeapEx(ADR(Heaps^[i]));
            END;
        END;
        OSDEALLOC(Heaps, NumHeapPages * AllocPageSize, NIL);
    END;
END Term;

PROCEDURE EnableLFH;
%IF Windows %THEN
VAR
	HeapKind : ULONG = 2;
BEGIN
	FUNC HeapSetInformation (ProcessHeap, HeapCompatibilityInformation, ADR(HeapKind), SIZE(HeapKind));
%END
END EnableLFH;

BEGIN
    (* in case this module is part of a WIN32 DLL, that inits per thread *)

    IF NOT IsThread THEN
        Init;
    END;

FINALLY
    IF NOT IsThread THEN
        Term;
    END;
END ExStorage.
