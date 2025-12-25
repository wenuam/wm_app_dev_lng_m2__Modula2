(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE MemShare;
<*/NOWARN:F*>

FROM SYSTEM IMPORT
    ADDRESS, ADDADR, CPUCOUNT, ATOMIC_CMPXCHG, ATOMIC_ADD, MEMORY_FENCE;

FROM EXCEPTIONS IMPORT
    ExceptionSource, AllocateSource, RaiseRTL, IsCurrentSource;

FROM Strings IMPORT
    Concat;

FROM ExStorage IMPORT
    ALLOCATE, DEALLOCATE, DeallocateEx,
    HeapInfoPointer, GetHeap;

FROM FileFunc IMPORT
    FileSpecString;

FROM WIN32 IMPORT
    HANDLE, SECURITY_ATTRIBUTES, WAIT_OBJECT_0, WAIT_TIMEOUT, PAGE_READWRITE,
    INVALID_HANDLE_VALUE, FILE_MAP_ALL_ACCESS,
    GetLastError,
    CreateSemaphore, CloseHandle, WaitForSingleObject, ReleaseSemaphore,
    CreateFileMapping, OpenFileMapping, MapViewOfFile, UnmapViewOfFile,
    GetCurrentThreadId, Sleep;

FROM WINX IMPORT
    NULL_HANDLE;

<*/NOPACK*>
%IF IA32 %THEN
<*/ALIGN:8*>
%END

CONST
    CacheLinePad        = 128;
TYPE
    ControlData =
        RECORD
        lock            : CARDINAL;
        contention      : INTEGER;
        recursion       : CARDINAL;
        init1           : INTEGER;
        init2           : INTEGER;
        (* do this to get the control data in a different *)
        (* cache line than the user data. *)
        pad2            : ARRAY [1..CacheLinePad-8] OF CARDINAL8;
        dummy           : LONGREAL;(*force eight byte natural alignment*)
        END;

    SharedMemory        = POINTER TO ShareData;
    ShareData =
        RECORD
        map             : HANDLE;
        signal          : HANDLE;
        control         : POINTER TO ControlData;
        heap            : HeapInfoPointer;
        spinCount       : CARDINAL;
        allocLock       : CARDINAL;
        sigName         : FileSpecString;
        END;

VAR
    ExceptSrc   : ExceptionSource;

PROCEDURE AllocateSharedMemory(VAR OUT sm : SharedMemory;
                               name : ARRAY OF CHAR;
                               size : CARDINAL;
                               VAR OUT allocResult : AllocResults) : ADDRESS;
VAR
    mapName     : FileSpecString;
    secAttr     : SECURITY_ATTRIBUTES;
BEGIN
    allocResult := AllocMemCreated;
    IF (name[0] = '') OR (size = 0) THEN
        RETURN NIL;
    END;

    NEW(sm);

    sm^.spinCount := 0;
    IF CPUCOUNT > 1 THEN
        sm^.spinCount := 200;
    END;

    Concat(name, "_SM_SIG", sm^.sigName);
    sm^.signal := NIL;
    sm^.allocLock := 0;
    sm^.heap := GetHeap();

    (* add room for our overhead *)

    size := size + SIZE(ControlData);

    Concat(name, "_SM_MEM", mapName);

    sm^.map := OpenFileMapping(FILE_MAP_ALL_ACCESS, FALSE, mapName);
    IF sm^.map <> NULL_HANDLE THEN
        allocResult := AllocMemOpened;
    ELSE
        secAttr.nLength := SIZE(secAttr);
        secAttr.lpSecurityDescriptor := NIL;
        secAttr.bInheritHandle := FALSE;

        sm^.map := CreateFileMapping(INVALID_HANDLE_VALUE,
                                     secAttr,
                                     PAGE_READWRITE,
                                     0, size,
                                     mapName);
        allocResult := AllocMemCreated;
        IF sm^.map = NULL_HANDLE THEN
            DISPOSE(sm);
            RETURN NIL;
        END;
    END;

    sm^.control := MapViewOfFile(sm^.map,
                                 FILE_MAP_ALL_ACCESS,
                                 0,
                                 0,
                                 size);
    IF sm^.control <> NIL THEN
        IF allocResult = AllocMemCreated THEN
            sm^.control^.lock := 0;
            sm^.control^.contention := 0;
            sm^.control^.recursion := 0;
            MEMORY_FENCE;
            sm^.control^.init1 := 12345678h;
            sm^.control^.init2 := -12345678h;
        ELSE
            WHILE (sm^.control^.init1 <> 12345678h) AND
                  (sm^.control^.init2 <> -12345678h)
            DO
                Sleep(0);
            END;
        END;
        RETURN ADDADR(sm^.control, SIZE(ControlData));
    ELSE
        CloseHandle(sm^.map);
        DISPOSE(sm);
        RETURN NIL;
    END;
END AllocateSharedMemory;

PROCEDURE DeallocateSharedMemory(VAR INOUT sm : SharedMemory);
BEGIN
    IF sm^.signal <> NIL THEN
        CloseHandle(sm^.signal);
    END;
    UnmapViewOfFile(sm^.map);
    CloseHandle(sm^.map);

    DeallocateEx(sm, SIZE(sm^), sm^.heap);
    sm := NIL;
END DeallocateSharedMemory;

PROCEDURE LockSharedMemory(sm : SharedMemory) : BOOLEAN;
BEGIN
    RETURN LockSharedMemoryEx(sm, -1);
END LockSharedMemory;

PROCEDURE AllocSignal(VAR INOUT sm : SharedMemory);
VAR
    secAttr     : SECURITY_ATTRIBUTES;
BEGIN
    WHILE ATOMIC_CMPXCHG(sm^.allocLock, 0, 1) <> 0 DO
        Sleep(0);
    END;
    MEMORY_FENCE;

    IF sm^.signal = NIL THEN
        secAttr.nLength := SIZE(secAttr);
        secAttr.lpSecurityDescriptor := NIL;
        secAttr.bInheritHandle := FALSE;
        sm^.signal := CreateSemaphore(secAttr, 0, 1, sm^.sigName);
    END;

    MEMORY_FENCE;
    sm^.allocLock := 0;
END AllocSignal;

PROCEDURE LockSharedMemoryEx(sm : SharedMemory; timeout : INTEGER) : BOOLEAN;
VAR
    spin        : CARDINAL;
    res         : CARDINAL;
    tid         : CARDINAL;
    old         : CARDINAL;
BEGIN
    tid := GetCurrentThreadId();

    old := ATOMIC_CMPXCHG(sm^.control^.lock, 0, tid);
    IF old = 0 THEN
        ATOMIC_ADD(sm^.control^.contention, 1);
    ELSIF old <> tid THEN
        (* is owned by another, we must wait *)

        ATOMIC_ADD(sm^.control^.contention, 1);

        (* given the algorithm used here it is possible for an unused
           semaphore signal to be sent. This is why we use a loop to get
           the lock value.
           this algorithm is used so that we can support a timeout value.
        *)

        spin := sm^.spinCount;
        WHILE ATOMIC_CMPXCHG(sm^.control^.lock, 0, tid) <> 0 DO
            IF spin <> 0 THEN
                DEC(spin);
            ELSE
                spin := sm^.spinCount;

                IF sm^.signal = NIL THEN
                    AllocSignal(sm);
                END;
                res := WaitForSingleObject(sm^.signal, timeout);

                IF res = WAIT_TIMEOUT THEN
                    ATOMIC_ADD(sm^.control^.contention, -1);
                    RETURN FALSE;
                ELSIF res <> WAIT_OBJECT_0 THEN
                    RaiseRTL(ExceptSrc, GetLastError(), "MEMSHARE-SEMOP-ERROR");
                END;
            END;
        END;
    END;

    INC(sm^.control^.recursion);
    MEMORY_FENCE;
    RETURN TRUE;
END LockSharedMemoryEx;

PROCEDURE UnlockSharedMemory(sm : SharedMemory);
BEGIN
    IF sm^.control^.lock = GetCurrentThreadId() THEN
        DEC(sm^.control^.recursion);
        IF sm^.control^.recursion = 0 THEN
            MEMORY_FENCE;
            sm^.control^.lock := 0;

            IF ATOMIC_ADD(sm^.control^.contention, -1) > 0 THEN
                IF sm^.signal = NIL THEN
                    AllocSignal(sm);
                END;
                ReleaseSemaphore(sm^.signal, 1, NIL);
            END;
        END;
    ELSE
        RaiseRTL(ExceptSrc, 0, "MEMSHARE-UNOWNED-UNLOCK");
    END;
END UnlockSharedMemory;

PROCEDURE IsMemShareException() : BOOLEAN;
BEGIN
    RETURN IsCurrentSource(ExceptSrc);
END IsMemShareException;

BEGIN
    AllocateSource(ExceptSrc);
END MemShare.
