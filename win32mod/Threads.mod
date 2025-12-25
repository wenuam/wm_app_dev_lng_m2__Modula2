(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 1993-2007                          *)
(*                            by Saperion Inc                              *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE Threads;
<*/NOWARN:F/NOPACK/INLINE:N/OPT:T*>

FROM SYSTEM IMPORT
    %IF Bits32 %THEN
    TIB,
    %END
    ADDRESS, ADR, CAST, IsThread, CPUCOUNT, ADRCARD,
    ATOMIC_CMPXCHG, ATOMIC_ADD, MEMORY_FENCE;

FROM EXCEPTIONS IMPORT
    ExceptionSource, AllocateSource, RaiseRTL, IsCurrentSource;

FROM ExStorage IMPORT
    ALLOCATE, DEALLOCATE;

FROM WIN32 IMPORT
    HANDLE, LPVOID, DWORD, HINSTANCE, BOOL, FARPROC, CRITICAL_SECTION,
    SYNCHRONIZE, EVENT_MODIFY_STATE, SEMAPHORE_MODIFY_STATE,
    THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST,
    THREAD_PRIORITY_TIME_CRITICAL, THREAD_PRIORITY_IDLE,
    NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS, IDLE_PRIORITY_CLASS,
    REALTIME_PRIORITY_CLASS, STACK_SIZE_PARAM_IS_A_RESERVATION,
    WAIT_OBJECT_0, WAIT_ABANDONED_0, WAIT_TIMEOUT, WAIT_IO_COMPLETION,
    MAXIMUM_WAIT_OBJECTS,
    SECURITY_ATTRIBUTES, SECURITY_DESCRIPTOR, SECURITY_DESCRIPTOR_REVISION,
    InitializeSecurityDescriptor, SetSecurityDescriptorDacl,
    CloseHandle, GetTickCount,
    TerminateThread, ExitThread, Sleep,
    GetCurrentThreadId, GetCurrentProcess,
    DuplicateHandle, DUPLICATE_SAME_ACCESS,
    WaitForSingleObjectEx, WaitForSingleObject, WaitForMultipleObjectsEx, INFINITE,
    CreateMutex, OpenMutex, ReleaseMutex,
    CreateEvent, OpenEvent, SetEvent, ResetEvent, PulseEvent,
    CreateSemaphore, OpenSemaphore, ReleaseSemaphore,
    InitializeCriticalSection,
    TLS_OUT_OF_INDEXES, TlsAlloc, TlsFree, TlsSetValue, TlsGetValue,
    LoadLibrary, FreeLibrary, GetProcAddress,
    GetVersionEx, OSVERSIONINFO, VER_PLATFORM_WIN32_NT;

IMPORT WIN32;

FROM WINERROR IMPORT
    ERROR_ALREADY_EXISTS;

FROM WINX IMPORT
    NULL_HANDLE, NULL_HINSTANCE,
    NIL_STR;

CONST
    CacheLinePad        = 128;

    MaxMultiSem         = MAXIMUM_WAIT_OBJECTS;
    MaxTermProcs        = 8;

    LockSpinCount       = 500;
    LockYieldCount      = 3;
    LockSleepTime       = 5;

    ReapInterval        = 5000;(*milliseconds*)

TYPE
    (* some of these records are padded such that consecutively allocated objects will
       not exist in the same memory cache line.
       this makes the spinlocks used by the objects more efficient.
    *)

    Thread              = POINTER TO ThreadRecord;
    ThreadRecord =
        RECORD
			magic           : CARDINAL;
			next            : Thread;
			nextQ           : Thread;
			handle          : HANDLE;
			hibernate       : HANDLE;
			proc            : ThreadProcedure;
			param           : ADDRESS;
			tid             : CARDINAL;
			termCount       : CARDINAL;
			waiters         : CARDINAL;
			code            : CARDINAL;
			lock            : CARDINAL;
			waitForWait     : BOOLEAN;
			external        : BOOLEAN;
			lastInBarrier   : BOOLEAN;
			zombie          : BOOLEAN;
			termProcs       : ARRAY [1..8] OF ThreadTermProcedure;
			termData        : ARRAY [1..8] OF ADDRESS;
        END;

    CriticalSectionRecord =
        RECORD
			owner           : CARDINAL;
			lock            : INTEGER;  (* needs to be INTEGER, -1 is added atomically. Flags are incorrect if CARDINAL *)
			recursion       : CARDINAL;
			spinCount       : CARDINAL;
			allocLock       : CARDINAL;
			sem             : HANDLE;
			allowRecursion  : BOOLEAN;
			pad1            : ARRAY [1..CacheLinePad] OF CARDINAL8;
        END;
    CriticalSection     = POINTER TO CriticalSectionRecord;

    ConditionVariableRecord =
        RECORD
			firstQ, lastQ   : Thread;
			lock            : CARDINAL;
			pad1            : ARRAY [1..CacheLinePad] OF CARDINAL8;
        END;
    ConditionVariable   = POINTER TO ConditionVariableRecord;

    RwLockRecord =
        RECORD
			lock            : CARDINAL;
			active          : INTEGER;(* 0 = none, -1 = write, 1 = read *)
			numReaders      : CARDINAL;
			readersWaiting  : CARDINAL;
			writer			: Thread;	(* meaningful only when active < 0 *)
			wakePriority    : INTEGER;(* 0 = default, -1 = write, 1 = read *)
	        firstReadQ,
	        lastReadQ,
	        firstWriteQ,
	        lastWriteQ      : Thread;
			pad1            : ARRAY [1..CacheLinePad] OF CARDINAL8;
        END;
    RwLock            = POINTER TO RwLockRecord;

    BarrierRecord =
        RECORD
			lock            : CARDINAL;
			count           : CARDINAL;
			inBarrier       : CARDINAL;
			firstQ, lastQ	: Thread;
			pad1            : ARRAY [1..CacheLinePad] OF CARDINAL8;
        END;
    Barrier             = POINTER TO BarrierRecord;

    MutexSemRecord =
        RECORD
			mutex           : HANDLE;
			count           : CARDINAL;
        END;
    MutexSem            = POINTER TO MutexSemRecord;

    EventSemRecord =
        RECORD
			event           : HANDLE;
        END;
    EventSem            = POINTER TO EventSemRecord;

    SignalSemRecord =
        RECORD
			sem             : HANDLE;
        END;
    SignalSem           = POINTER TO SignalSemRecord;

    MultiSemRecord =
        RECORD
			lock            : CARDINAL;
			max             : CARDINAL;
			count           : CARDINAL;
			handles         : ARRAY [1..MaxMultiSem] OF HANDLE;
			user            : ARRAY [1..MaxMultiSem] OF CARDINAL;
			allTrigger      : BOOLEAN;
        END;
    MultiSem            = POINTER TO MultiSemRecord;

VAR
    FirstThread         : Thread;
    LastThread          : Thread;
    ExtThreadCount      : CARDINAL;
    ReapTick            : CARDINAL;

    ThreadSem           : CRITICAL_SECTION;

    ThreadTls           : CARDINAL;

    ExceptSrc           : ExceptionSource;

    SecAttr             : SECURITY_ATTRIBUTES;
    SecDesc             : SECURITY_DESCRIPTOR;

    SpinCount           : CARDINAL;

    OsVersion           : OSVERSIONINFO;
    ScheduleThread      : PROCEDURE() : BOOL [WINDOWS];

PROCEDURE Raise(str : ARRAY OF CHAR) [NEVERRETURNS];
BEGIN
    RaiseRTL(ExceptSrc, 0, str);
END Raise;

PROCEDURE IsThreadsException() : BOOLEAN;
BEGIN
    RETURN IsCurrentSource(ExceptSrc);
END IsThreadsException;

PROCEDURE GetThreadTls() : Thread;
VAR
    th  : Thread;
BEGIN
    th := TlsGetValue(ThreadTls);
    IF th <> NIL THEN
        RETURN th;
    END;
    (* allows us to handle threads created outside this module.
       for example you might have a DLL that external code calls.
       that external code may allocate threads.
       in this way the thread specific data and various synchronization
       objects will work with external threads.
    *)
    NEW(th);
	IF th = NIL THEN Raise ("No memory for new thread") END;
    th^.next := NIL;
    th^.external := TRUE;
    th^.waitForWait := FALSE;
    ThreadSetup(th);
    ATOMIC_ADD(ExtThreadCount, 1);

    WIN32.EnterCriticalSection(ThreadSem);
    LastThread^.next := th;
    LastThread := th;
    WIN32.LeaveCriticalSection(ThreadSem);

    RETURN th;
END GetThreadTls;

PROCEDURE ReapExternalThreads;
VAR
    th, next    : Thread;
    curTh       : Thread;
    res         : DWORD;
    tick        : CARDINAL;
BEGIN
    IF ExtThreadCount > 0 THEN
        tick := GetTickCount();(*not kernel*)
        IF (tick < ReapTick) OR (tick-ReapTick >= ReapInterval) THEN
            ReapTick := tick;

            WIN32.EnterCriticalSection(ThreadSem);(*not kernel unless contested*)

            curTh := TlsGetValue(ThreadTls);(*not kernel*)

            th := FirstThread;
            WHILE th <> NIL DO
                next := th^.next;

                IF th^.external AND (th <> curTh) THEN
                    res := WaitForSingleObject(th^.handle, 0);(*kernel call*)
                    IF res = WAIT_OBJECT_0 THEN
                        ATOMIC_ADD(ExtThreadCount, -1);

                        RemoveThread(th);
                    END;
                END;

                th := next;
            END;

            WIN32.LeaveCriticalSection(ThreadSem);(*not kernel unless contested*)
        END;
    END;
END ReapExternalThreads;

PROCEDURE ReapExtThread;
VAR
    th  : Thread;
BEGIN
    th := TlsGetValue(ThreadTls);
    IF (th <> NIL) AND th^.external THEN
        ATOMIC_ADD(ExtThreadCount, -1);
        WIN32.EnterCriticalSection(ThreadSem);
        RemoveThread(th);
        WIN32.LeaveCriticalSection(ThreadSem);
    END;
END ReapExtThread;

PROCEDURE MyWaitForSingleObject(h : HANDLE; timeout : INTEGER) : DWORD;
VAR
    res         : DWORD;
    winTime     : DWORD;
BEGIN
    IF timeout < 0 THEN
        winTime := INFINITE;
    ELSE
        winTime := timeout;
    END;
    LOOP
        res := WaitForSingleObjectEx(h, winTime, TRUE);
        IF res <> WAIT_IO_COMPLETION THEN
            EXIT;
        END;
    END;

    (*
      we just made a kernel call so we are not adding much overhead in trying to
      dispose of terminated external threads.
      also, external threads are unlikely in the vast majority of code.
      an M2 DLL attaching to some non M2 system might end up with one.
      if that DLL sets up for per thread init/term then we will never
      have any reaping to do here.
    *)
    ReapExternalThreads;

    RETURN res;
END MyWaitForSingleObject;

PROCEDURE GetWaitResult(res : CARDINAL; num : CARDINAL) : WaitResult;
BEGIN
    IF (res >= WAIT_OBJECT_0) AND (res <= WAIT_OBJECT_0+num-1) THEN
        RETURN WaitSuccess;
    ELSIF (res >= WAIT_ABANDONED_0) AND (res <= WAIT_ABANDONED_0+num-1) THEN
        RETURN WaitAbandoned;
    ELSIF res = WAIT_TIMEOUT THEN
        RETURN WaitTimeout;
    ELSE
        RETURN WaitError;
    END;
END GetWaitResult;

PROCEDURE ThreadSetup(T : Thread);
BEGIN
    TlsSetValue(ThreadTls, T);

    T^.waiters := 0;
    T^.code := 0;
    T^.hibernate := NULL_HANDLE;
    T^.nextQ := NIL;
    T^.termCount := 0;
    T^.zombie := FALSE;

    T^.tid := GetCurrentThreadId();
    <*/PUSH/NOCHECK:O*>
    T^.magic := (0 - T^.tid);
    <*/POP*>

    (* get a "real" thread handle for this thread. *)

    T^.handle := WIN32.GetCurrentThread();
    IF NOT DuplicateHandle(GetCurrentProcess(), T^.handle,
                           GetCurrentProcess(), T^.handle,
                           0, FALSE, DUPLICATE_SAME_ACCESS)
    THEN
        Raise("ThreadSetup - Failed DuplicateHandle");
    END;

    MEMORY_FENCE;
    T^.lock := 0;
END ThreadSetup;

PROCEDURE CallTermProcs(T : Thread);
VAR
    i           : ADRCARD;
    count       : ADRCARD;
BEGIN
    count := T^.termCount;
    FOR i := 1 TO count DO
        T^.termProcs[i](T, T^.code, T^.termData[i]);
    END;
EXCEPT
    RETURN;
END CallTermProcs;

PROCEDURE MyThread(param : LPVOID) : WIN32.DWORD [EXPORT, WINDOWS];
VAR
    exitCode    : CARDINAL;
    T           : Thread;
BEGIN
    T := param;

    ThreadSetup(T);

    (* finally call the user proc *)

    exitCode := T^.proc(T^.param);
    T^.code := exitCode;

    CallTermProcs(T);

    WIN32.EnterCriticalSection(ThreadSem);

    T^.zombie := TRUE;

    IF NOT T^.waitForWait THEN
        RemoveThread(T);
    END;

    WIN32.LeaveCriticalSection(ThreadSem);

    RETURN exitCode;

EXCEPT
    T^.lock := 0;

    T^.code := MAX(CARDINAL);
    CallTermProcs(T);

    WIN32.EnterCriticalSection(ThreadSem);
    RemoveThread(T);
    WIN32.LeaveCriticalSection(ThreadSem);

    (* let the exception go unhandled. *)
    (* the system will terminate *)
END MyThread;

PROCEDURE CreateThread(VAR OUT T : Thread;
                       P : ThreadProcedure;
                       threadParam : ADDRESS;
                       stackSize : CARDINAL;
                       requireWait : BOOLEAN) : BOOLEAN;
VAR
    tid         : CARDINAL;
    secAttr     : SECURITY_ATTRIBUTES;
    th          : Thread;
    handle      : HANDLE;
    flags       : DWORD;
BEGIN
    (* in Windows stack size specifies the committed memory.
       the reserved stack space is specified in the executable header.
       XP and later changes this. we can optionally specify the stack reserve.
    *)

    flags := 0;
    IF (OsVersion.dwPlatformId = VER_PLATFORM_WIN32_NT) AND
       (OsVersion.dwMajorVersion > 5) OR
       ((OsVersion.dwMajorVersion = 5) AND (OsVersion.dwMinorVersion >= 1))
    THEN
        IF stackSize <> 0 THEN
            flags := flags BOR STACK_SIZE_PARAM_IS_A_RESERVATION;
        ELSE
            (* just commit some small value other than the system default of 1 page *)
            stackSize := 16*1024;
        END;
    ELSE
        (* just commit some small value other than the system default of 1 page *)
        stackSize := 16*1024;
    END;

    NEW(T);
    IF T <> NIL THEN
        T^.proc := P;
        T^.param := threadParam;
        T^.next := NIL;
        T^.external := FALSE;
        T^.waitForWait := requireWait;
        T^.lock := 1;

        secAttr.nLength := SIZE(secAttr);
        secAttr.lpSecurityDescriptor := NIL;
        secAttr.bInheritHandle := FALSE;

        th := T;(* don't even think of deleting this! it must exist.
                   think about it, T is a VAR param.
                   we do not know the lifetime of the thread created,
                   it could complete before we ever get back from the
                   create call.
                   if T is in memory the user thread deallocates when it(the thread)
                   terminates, then T is invalid and might have been overwritten.
                   we need th to have a valid pointer to the thread handle.
                *)

        WIN32.EnterCriticalSection(ThreadSem);

        handle := WIN32.CreateThread(secAttr, stackSize, MyThread, th, flags, tid);
        IF handle <> NULL_HANDLE THEN
            LastThread^.next := th;
            LastThread := th;

            (* wait for the thread to finish initializing *)

            WHILE ATOMIC_CMPXCHG(th^.lock, 0, 1) <> 0 DO
                ScheduleThread;
            END;
            th^.lock := 0;

            (* the thread gets its own copy of the handle in its setup.
               it does this so calls that want a thread handle can be used.
               we have no idea when CreateThread will return and thus give this
               code the thread handle, such that it could assign the field.
            *)
            CloseHandle(handle);
        ELSE
            DISPOSE(T);
        END;

        WIN32.LeaveCriticalSection(ThreadSem);
    END;
    RETURN T <> NIL;
END CreateThread;

PROCEDURE IsValid(T : Thread) : BOOLEAN [INLINE];
BEGIN
    <*/PUSH/NOCHECK:O*>
    RETURN (T <> NIL) AND (T^.magic = (0 - T^.tid));
    <*/POP*>
END IsValid;

PROCEDURE InList(T : Thread) : BOOLEAN;
VAR
    ptr         : Thread;
BEGIN
    ptr := FirstThread;
    WHILE (ptr <> NIL) AND (ptr <> T) DO
        ptr := ptr^.next;
    END;
    RETURN ptr <> NIL;
END InList;

PROCEDURE RequireThreadWait(T : Thread; yes : BOOLEAN);
BEGIN
    WIN32.EnterCriticalSection(ThreadSem);
    IF IsValid(T) AND InList(T) THEN
        T^.waitForWait := yes;
        IF (NOT yes) AND (T^.waiters = 0) AND T^.zombie THEN
            RemoveThread(T);
        END;
    END;
    WIN32.LeaveCriticalSection(ThreadSem);
END RequireThreadWait;

PROCEDURE GetCurrentThread() : Thread;
BEGIN
    RETURN GetThreadTls();
END GetCurrentThread;

PROCEDURE GetSystemThreadHandle() : ADDRESS;
VAR
    th  : Thread;
BEGIN
    th := GetThreadTls();
    RETURN th^.handle;
END GetSystemThreadHandle;

PROCEDURE RemoveThread(VAR INOUT T : Thread);
VAR
    prev        : Thread;
    ptr         : Thread;
BEGIN
    prev := NIL;
    ptr := FirstThread;
    WHILE (ptr <> NIL) AND (ptr <> T) DO
        prev := ptr;
        ptr := ptr^.next;
    END;

    IF ptr <> NIL THEN
        IF ptr^.waiters = 0 THEN
            IF prev = NIL THEN
                FirstThread := FirstThread^.next;
            ELSE
                prev^.next := ptr^.next;
            END;
            IF ptr = LastThread THEN
                LastThread := prev;
            END;

            CloseHandle(ptr^.handle);
            IF ptr^.hibernate <> NULL_HANDLE THEN
                CloseHandle(ptr^.hibernate);
            END;
            DISPOSE (ptr);

            T := NIL;
        END;

    END;
END RemoveThread;

PROCEDURE KillThread(VAR INOUT T : Thread; code : CARDINAL) : BOOLEAN;
VAR
    ok  : BOOLEAN;
BEGIN
    WIN32.EnterCriticalSection(ThreadSem);

    IF IsValid(T) AND InList(T) THEN
        IF T <> GetCurrentThread() THEN
            T^.code := code;
            ok := TerminateThread(T^.handle, code);
            IF ok THEN
                CallTermProcs(T);
                RemoveThread(T);
            END;
            WIN32.LeaveCriticalSection(ThreadSem);
            RETURN ok;
        ELSE
            T^.code := code;

            CallTermProcs(T);

            IF FirstThread^.next <> NIL THEN
                RemoveThread(T);
                WIN32.LeaveCriticalSection(ThreadSem);
                ExitThread(code);
            ELSE
                WIN32.LeaveCriticalSection(ThreadSem);

                (* the last thread, terminate the process *)
                HALT(code);
            END;

            (* actually never get here *)
        END;
    END;

    WIN32.LeaveCriticalSection(ThreadSem);
    RETURN TRUE;

EXCEPT
    WIN32.LeaveCriticalSection(ThreadSem);
END KillThread;

PROCEDURE AddThreadTermination(T : Thread;
                               proc : ThreadTermProcedure;
                               data : ADDRESS) : BOOLEAN;
BEGIN
    IF T^.termCount < MaxTermProcs THEN
        INC(T^.termCount);
        T^.termProcs[T^.termCount] := proc;
        T^.termData[T^.termCount] := data;
        RETURN TRUE;
    END;
    RETURN FALSE;
END AddThreadTermination;

PROCEDURE WaitForThreadTermination(VAR INOUT T : Thread;
                                   timeout : INTEGER;
                                   VAR OUT code : CARDINAL) : WaitResult;
VAR
    winRes      : CARDINAL;
    res         : WaitResult;
BEGIN
    WIN32.EnterCriticalSection(ThreadSem);

    res := WaitError;

    IF T <> GetCurrentThread() THEN
        IF IsValid(T) AND InList(T) AND T^.waitForWait THEN
            INC(T^.waiters);

            WIN32.LeaveCriticalSection(ThreadSem);
            winRes := MyWaitForSingleObject(T^.handle, timeout);
            WIN32.EnterCriticalSection(ThreadSem);

            res := GetWaitResult(winRes, 1);

            DEC(T^.waiters);

            IF res = WaitSuccess THEN
                code := T^.code;
                RemoveThread(T);
            END;
        END;
    END;

    WIN32.LeaveCriticalSection(ThreadSem);
    RETURN res;

EXCEPT
    WIN32.LeaveCriticalSection(ThreadSem);
END WaitForThreadTermination;

PROCEDURE YieldThread;
BEGIN
    ScheduleThread;
    ReapExternalThreads;
END YieldThread;

PROCEDURE SleepThread(time : CARDINAL);
BEGIN
    IF time = 0 THEN
        ScheduleThread;
    ELSE
        Sleep(time);
    END;
    ReapExternalThreads;
END SleepThread;

PROCEDURE AllocHibernate(T : Thread);
BEGIN
    IF T^.hibernate = NULL_HANDLE THEN
        WHILE ATOMIC_CMPXCHG(T^.lock, 0, 1) <> 0 DO
            ScheduleThread;
        END;
        MEMORY_FENCE;

        IF T^.hibernate = NULL_HANDLE THEN
            T^.hibernate := CreateSemaphore(SecAttr, 0, 1, NIL_STR);
        END;

        MEMORY_FENCE;
        T^.lock := 0;
    END;
END AllocHibernate;

PROCEDURE Hibernate(timeout : INTEGER) : WaitResult;
VAR
    th  : Thread;
    winRes      : DWORD;
BEGIN
    th := GetCurrentThread();
    AllocHibernate(th);
    winRes := MyWaitForSingleObject(th^.hibernate, timeout);
    RETURN GetWaitResult(winRes, 1);
END Hibernate;

PROCEDURE Awaken(T : Thread) : BOOLEAN;
BEGIN
    AllocHibernate(T);
    RETURN ReleaseSemaphore(T^.hibernate, 1, NIL);
END Awaken;

PROCEDURE SetThreadPriority(T : Thread; pri : ThreadPriority);
CONST
    winPri      : ARRAY ThreadPriority OF INTEGER =
        {
         THREAD_PRIORITY_IDLE,
         THREAD_PRIORITY_LOWEST,
         THREAD_PRIORITY_BELOW_NORMAL,
         THREAD_PRIORITY_NORMAL,
         THREAD_PRIORITY_ABOVE_NORMAL,
         THREAD_PRIORITY_HIGHEST,
         THREAD_PRIORITY_TIME_CRITICAL,
         THREAD_PRIORITY_NORMAL
        };
BEGIN
    IF IsValid(T) THEN
        WIN32.SetThreadPriority(T^.handle, winPri[pri]);
    END;
END SetThreadPriority;

PROCEDURE GetThreadPriority(T : Thread) : ThreadPriority;
VAR
    retVal      : ThreadPriority;
    err         : CARDINAL;
BEGIN
    retVal := TpUnknown;
    IF IsValid(T) THEN
        CASE WIN32.GetThreadPriority(T^.handle) OF
        THREAD_PRIORITY_IDLE:
            retVal := TpIdle;
        |
        THREAD_PRIORITY_LOWEST:
            retVal := TpLow;
        |
        THREAD_PRIORITY_BELOW_NORMAL:
            retVal := TpMedLow;
        |
        THREAD_PRIORITY_NORMAL:
            retVal := TpNormal;
        |
        THREAD_PRIORITY_ABOVE_NORMAL:
            retVal := TpMedHigh;
        |
        THREAD_PRIORITY_HIGHEST:
            retVal := TpHigh;
        |
        THREAD_PRIORITY_TIME_CRITICAL:
            retVal := TpTimeCritical;
        ELSE
            retVal := TpUnknown;
            err := WIN32.GetLastError();
        END;
    END;
    RETURN retVal;
END GetThreadPriority;

PROCEDURE SetPriorityClass(class : PriorityClass);
CONST
    winClass    : ARRAY PriorityClass OF CARDINAL =
        {
         IDLE_PRIORITY_CLASS,
         NORMAL_PRIORITY_CLASS,
         HIGH_PRIORITY_CLASS,
         REALTIME_PRIORITY_CLASS,
         NORMAL_PRIORITY_CLASS
        };
BEGIN
    WIN32.SetPriorityClass(GetCurrentProcess(), winClass[class]);
END SetPriorityClass;

PROCEDURE GetPriorityClass() : PriorityClass;
VAR
    retVal      : PriorityClass;
BEGIN
    CASE WIN32.GetPriorityClass(GetCurrentProcess()) OF
    NORMAL_PRIORITY_CLASS:
        retVal := PcNormal;
    |
    HIGH_PRIORITY_CLASS:
        retVal := PcHigh;
    |
    IDLE_PRIORITY_CLASS:
        retVal := PcLow;
    |
    REALTIME_PRIORITY_CLASS:
        retVal := PcRealTime;
    ELSE
        retVal := PcUnknown;
    END;
    RETURN retVal;
END GetPriorityClass;

PROCEDURE GetThreadSystemId(T : Thread) : ADRCARD;
VAR
    id  : CARDINAL;
BEGIN
    id := 0;
    IF IsValid(T) THEN
        id := T^.tid;
    END;
    RETURN id;
END GetThreadSystemId;

PROCEDURE GetTid() : ADRCARD [INLINE];
BEGIN
    %IF Bits32 %THEN
        RETURN TIB^.threadID;
    %ELSE
        RETURN GetCurrentThreadId();
    %END
END GetTid;

PROCEDURE GetCurrentThreadSystemId() : ADRCARD;
BEGIN
    RETURN GetTid();
END GetCurrentThreadSystemId;

PROCEDURE GetThreadFromSystemId(threadId : ADRCARD) : Thread;
VAR
    th  : Thread;
BEGIN
    WIN32.EnterCriticalSection(ThreadSem);
    th := FirstThread;
    WHILE (th <> NIL) AND (th^.tid <> ORD(threadId)) DO
        th := th^.next;
    END;
    WIN32.LeaveCriticalSection(ThreadSem);
    RETURN th;

EXCEPT
    WIN32.LeaveCriticalSection(ThreadSem);
END GetThreadFromSystemId;

PROCEDURE GetCurrentProcessId() : CARDINAL;
BEGIN
    RETURN WIN32.GetCurrentProcessId();
END GetCurrentProcessId;

PROCEDURE AllocateTlsIndex(VAR OUT index : CARDINAL) : BOOLEAN;
BEGIN
    index := TlsAlloc();
    RETURN index <> TLS_OUT_OF_INDEXES;
END AllocateTlsIndex;

PROCEDURE FreeTlsIndex(index : CARDINAL);
BEGIN
    TlsFree(index);
END FreeTlsIndex;

PROCEDURE SetTlsData(index : CARDINAL; data : ADDRESS) : BOOLEAN;
BEGIN
    RETURN TlsSetValue(index, data);
END SetTlsData;

PROCEDURE GetTlsData(index : CARDINAL) : ADDRESS;
BEGIN
    RETURN TlsGetValue(index);
END GetTlsData;

PROCEDURE CreateCriticalSection(VAR OUT C : CriticalSection) : BOOLEAN;
BEGIN
    RETURN CreateCriticalSectionEx(C, 0, TRUE);
END CreateCriticalSection;

PROCEDURE CreateCriticalSectionEx(VAR OUT C : CriticalSection;
                                  spinCount : CARDINAL;
                                  allowRecursion : BOOLEAN) : BOOLEAN;
BEGIN
    NEW(C);
    IF C <> NIL THEN
        C^.spinCount := 1;
        IF CPUCOUNT # 1 THEN
        	INC (C^.spinCount, spinCount);
        END;
        C^.owner := 0;
        C^.lock := 0;
        C^.recursion := 0;
        C^.allocLock := 0;
        C^.allowRecursion := allowRecursion;
	    C^.sem := CreateSemaphore(SecAttr, 0, 1, NIL_STR);
        RETURN TRUE;
    END;
    RETURN FALSE;
END CreateCriticalSectionEx;

PROCEDURE CloseCriticalSection(VAR INOUT C : CriticalSection) : BOOLEAN;
BEGIN
    IF C <> NIL THEN
		WHILE ATOMIC_CMPXCHG(C^.allocLock, 0, 1) <> 0 DO
			ScheduleThread;
		END;
		MEMORY_FENCE;
        CloseHandle(C^.sem);
        DISPOSE (C);
    END;
    RETURN TRUE;
END CloseCriticalSection;

PROCEDURE SetCriticalSectionSpinCount(C : CriticalSection; spinCount : CARDINAL);
BEGIN
    IF CPUCOUNT # 1 THEN
    	C^.spinCount := spinCount+1;
    END;
END SetCriticalSectionSpinCount;

PROCEDURE EnterCriticalSection(C : CriticalSection);
VAR
    spin        : CARDINAL;
    winRes      : CARDINAL;
    tid         : CARDINAL;
BEGIN
    tid := GetTid();

    IF C^.owner = tid THEN
        IF C^.allowRecursion THEN
            INC(C^.recursion);
            RETURN;
        ELSE
            Raise("Recursive attempt at EnterCriticalSection");
        END;
    END;
    spin := C^.spinCount;
    WHILE spin <> 0 DO
		IF ATOMIC_CMPXCHG(C^.lock, 0, 1) = 0 THEN
			C^.owner := tid;
			C^.recursion := 1;
			RETURN;
		END;
		DEC (spin);
    END;
    IF ATOMIC_ADD(C^.lock, 1) > 1 THEN
        winRes := MyWaitForSingleObject(C^.sem, -1);
        IF GetWaitResult(winRes, 1) <> WaitSuccess THEN
            Raise("CriticalSection semaphore wait error");
        END;
    END;

    C^.owner := tid;
    C^.recursion := 1;
END EnterCriticalSection;

PROCEDURE TryEnterCriticalSection(C : CriticalSection) : BOOLEAN;
VAR
    tid         : CARDINAL;
BEGIN
    tid := GetTid();

    IF C^.owner = tid THEN
        IF C^.allowRecursion THEN
            INC(C^.recursion);
            RETURN TRUE;
        ELSE
            Raise("Recursive attempt at TryEnterCriticalSection");
        END;
    END;
    IF ATOMIC_CMPXCHG(C^.lock, 0, 1) = 0 THEN
        C^.owner := tid;
        C^.recursion := 1;
        RETURN TRUE;
    END;
    RETURN FALSE;
END TryEnterCriticalSection;

PROCEDURE LeaveCriticalSection(C : CriticalSection);
VAR
    tid         : CARDINAL;
BEGIN
    tid := GetTid();

    IF C^.owner = tid THEN
        DEC(C^.recursion);
        IF C^.recursion = 0 THEN
            C^.owner := 0;
            MEMORY_FENCE;

            IF ATOMIC_ADD(C^.lock, -1) > 0 THEN
                IF NOT ReleaseSemaphore(C^.sem, 1, NIL) THEN
                    Raise("CriticalSection semaphore post error");
                END;
            END;
        END;
    ELSE
        Raise("Non owner in LeaveCriticalSection");
    END;
END LeaveCriticalSection;

PROCEDURE CriticalSectionOwnedByThread(C : CriticalSection) : BOOLEAN;
VAR
    tid         : CARDINAL;
BEGIN
    tid := GetTid();
    RETURN C^.owner = tid;
END CriticalSectionOwnedByThread;

PROCEDURE GetCriticalSectionCount(C : CriticalSection) : CARDINAL;
BEGIN
    IF CriticalSectionOwnedByThread(C) THEN
        RETURN C^.recursion;
    END;
    RETURN 0;
END GetCriticalSectionCount;

PROCEDURE PopCriticalSection(C : CriticalSection; count : CARDINAL);
BEGIN
    IF CriticalSectionOwnedByThread(C) THEN
        WHILE C^.recursion > count DO
            LeaveCriticalSection(C);
        END;
    END;
END PopCriticalSection;

PROCEDURE Enqueue(th : Thread; VAR INOUT firstQ, lastQ : Thread);
BEGIN
    (* FIFO *)

    th^.nextQ := NIL;
    IF firstQ = NIL THEN
        firstQ := th;
        lastQ := th;
    ELSE
        lastQ^.nextQ := th;
        lastQ := th;
    END;
END Enqueue;

PROCEDURE Dequeue(th : Thread; VAR INOUT firstQ, lastQ : Thread) : BOOLEAN;
VAR
    ptr, prev   : Thread;
BEGIN
    prev := NIL;
    ptr := firstQ;
    WHILE (ptr # NIL) & (ptr <> th) DO
        prev := ptr;
        ptr := ptr^.nextQ;
    END;
    IF ptr <> NIL THEN
        IF prev = NIL THEN
            firstQ := firstQ^.nextQ;
        ELSE
            prev^.nextQ := ptr^.nextQ;
        END;
        IF lastQ = ptr THEN
            lastQ := prev;
        END;
        ptr^.nextQ := NIL;
        RETURN TRUE;
    END;
    RETURN FALSE;
END Dequeue;

PROCEDURE SpinLock(VAR INOUT lock : CARDINAL) [INLINE];
VAR
    spin        : CARDINAL;
    yield       : CARDINAL;
BEGIN
    yield := 0;
    spin := SpinCount;
    LOOP
        IF lock = 0 THEN
            IF ATOMIC_CMPXCHG(lock, 0, 1) = 0 THEN
                MEMORY_FENCE;
                EXIT;
            END;
        END;

        IF spin <> 0 THEN
            DEC(spin);
        ELSE
            (*
              this elaborate stuff handles priority inversion.
              Windows eventually handles inversion, but this should be quicker.
            *)
            spin := SpinCount;
            IF yield < LockYieldCount THEN
                INC(yield);
                ScheduleThread;
            ELSE
                yield := 0;
                Sleep(LockSleepTime);
            END;
        END;
    END;
END SpinLock;

PROCEDURE LockCV(CV : ConditionVariable) [INLINE];
BEGIN
    SpinLock(CV^.lock);
END LockCV;

PROCEDURE UnlockCV(CV : ConditionVariable) [INLINE];
BEGIN
    MEMORY_FENCE;
    CV^.lock := 0;
END UnlockCV;

PROCEDURE CreateConditionVariable(VAR OUT CV : ConditionVariable) : BOOLEAN;
BEGIN
    NEW(CV);
    IF CV <> NIL THEN
        CV^.firstQ := NIL;
        CV^.lastQ := NIL;
        CV^.lock := 0;
        RETURN TRUE;
    END;
    RETURN FALSE;
END CreateConditionVariable;

PROCEDURE CloseConditionVariable(VAR INOUT CV : ConditionVariable) : BOOLEAN;
BEGIN
    IF CV <> NIL THEN
        LockCV(CV);
        DISPOSE (CV);
        RETURN TRUE;
    END;
    RETURN FALSE;
END CloseConditionVariable;

PROCEDURE WaitForCondition(CS : CriticalSection;
                           CV : ConditionVariable;
                           timeout : INTEGER) : WaitResult;
VAR
    th          : Thread;
    res         : WaitResult;
    ok          : BOOLEAN;
BEGIN
    res := WaitError;
    IF timeout <> 0 THEN
        th := GetThreadTls();

        LockCV(CV);
        Enqueue(th, CV^.firstQ, CV^.lastQ);
        UnlockCV(CV);

        LeaveCriticalSection(CS);
        res := Hibernate(timeout);
        IF res = WaitTimeout THEN
            LockCV(CV);
            ok := Dequeue(th, CV^.firstQ, CV^.lastQ);
            UnlockCV(CV);
            IF NOT ok THEN
                (* we were not in the queue and we probably timed out. *)
                (* someone awoke us before we got the lock. *)
                (* we need to clear the awaken signal. *)
                res := Hibernate(-1);
            END;
        END;
        EnterCriticalSection(CS);
    END;
    RETURN res;
END WaitForCondition;

PROCEDURE SignalCondition(CV : ConditionVariable) : BOOLEAN;
VAR
    ok  : BOOLEAN;
    th  : Thread;
BEGIN
    LockCV(CV);
    th := CV^.firstQ;
    IF th = CV^.lastQ THEN
        CV^.firstQ := NIL;
        CV^.lastQ := NIL;
    ELSE
        CV^.firstQ := CV^.firstQ^.nextQ;
    END;
    UnlockCV(CV);

    ok := FALSE;
    IF th <> NIL THEN
        ok := TRUE;
        th^.nextQ := NIL;
        Awaken(th);
    END;
    RETURN ok;
END SignalCondition;

PROCEDURE BroadcastCondition(CV : ConditionVariable) : BOOLEAN;
VAR
    ok          : BOOLEAN;
    th          : Thread;
    next        : Thread;
BEGIN
    LockCV(CV);
    th := CV^.firstQ;
    CV^.firstQ := NIL;
    CV^.lastQ := NIL;
    UnlockCV(CV);

    ok := FALSE;
    WHILE th <> NIL DO
        next := th^.nextQ;

        ok := TRUE;
        th^.nextQ := NIL;
        Awaken(th);

        th := next;
    END;
    RETURN ok;
END BroadcastCondition;

PROCEDURE LockRW(RW : RwLock) [INLINE];
BEGIN
    SpinLock(RW^.lock);
END LockRW;

PROCEDURE UnlockRW(RW : RwLock) [INLINE];
BEGIN
    MEMORY_FENCE;
    RW^.lock := 0;
END UnlockRW;

PROCEDURE CreateRwLock(VAR OUT RW : RwLock) : BOOLEAN;
BEGIN
    NEW(RW);
    IF RW <> NIL THEN
        RW^.active := 0;
        RW^.numReaders := 0;
        RW^.readersWaiting := 0;
        RW^.lock := 0;
        RW^.wakePriority := 0;
        RW^.firstReadQ := NIL;
        RW^.lastReadQ := NIL;
        RW^.firstWriteQ := NIL;
        RW^.lastWriteQ := NIL;
        RETURN TRUE;
    END;
    RETURN FALSE;
END CreateRwLock;

PROCEDURE CloseRwLock(VAR INOUT RW : RwLock) : BOOLEAN;
VAR
    ok          : BOOLEAN;
BEGIN
    ok := TRUE;
    IF RW <> NIL THEN
        LockRW(RW);
        DISPOSE (RW);
    END;
    RETURN ok;
END CloseRwLock;

PROCEDURE RwReadLock(RW : RwLock; timeout : INTEGER) : WaitResult;
VAR
    res         : WaitResult;
    ok          : BOOLEAN;
    th          : Thread;
BEGIN
    LockRW(RW);

    IF RW^.active = 0 THEN
        RW^.active := 1;
        RW^.numReaders := 1;
        UnlockRW(RW);
        RETURN WaitSuccess;
    ELSIF (RW^.firstWriteQ = NIL) AND (RW^.active > 0) THEN
        INC(RW^.numReaders);
        UnlockRW(RW);
        RETURN WaitSuccess;
    ELSIF timeout = 0 THEN
        UnlockRW(RW);
        RETURN WaitTimeout;
    END;
	th := GetThreadTls();

    IF (RW^.active < 0) & (RW^.writer = th) THEN
        UnlockRW(RW);
		Raise ("RwReadLock is called after RwWriteLock");
	END;
	IF RW^.wakePriority = 0 THEN
		RW^.wakePriority := 1;
	END;

	INC(RW^.readersWaiting);
	Enqueue(th, RW^.firstReadQ, RW^.lastReadQ);
	UnlockRW(RW);
	res := Hibernate(timeout);
	IF res = WaitTimeout THEN
		LockRW(RW);
		ok := Dequeue(th, RW^.firstReadQ, RW^.lastReadQ);
		UnlockRW(RW);
		IF NOT ok THEN
			(* we were not in the queue and we probably timed out. *)
			(* someone awoke us before we got the lock. *)
			(* we need to clear the awaken signal. *)
			res := Hibernate(-1);
		END;
	END;
	RETURN res;
END RwReadLock;

PROCEDURE RwWriteLock(RW : RwLock; timeout : INTEGER) : WaitResult;
VAR
    th          : Thread;
    res         : WaitResult;
    ok          : BOOLEAN;
BEGIN
    LockRW(RW);
	th := GetThreadTls();
	IF (RW^.active < 0) & (RW^.writer = th) THEN
		UnlockRW(RW);
		Raise ("Recursive call of RwWriteLock");
	END;

    IF RW^.active = 0 THEN
        RW^.active := -1;
		RW^.writer := th;
        UnlockRW(RW);
        RETURN WaitSuccess;
    ELSIF timeout = 0 THEN
        UnlockRW(RW);
        RETURN WaitTimeout;
    END;
	IF RW^.wakePriority = 0 THEN
		RW^.wakePriority := -1;
	END;

    Enqueue(th, RW^.firstWriteQ, RW^.lastWriteQ);
	UnlockRW(RW);

	res := Hibernate(timeout);
	IF res = WaitTimeout THEN
		LockRW(RW);
        ok := Dequeue(th, RW^.firstWriteQ, RW^.lastWriteQ);
		UnlockRW(RW);
		IF NOT ok THEN
			(* we were not in the queue and we probably timed out. *)
			(* someone awoke us before we got the lock. *)
			(* we need to clear the awaken signal. *)
			res := Hibernate(-1);
		END;
	END;

	RETURN res;
END RwWriteLock;

PROCEDURE RwUnlock(RW : RwLock);
VAR
    th, next    : Thread;
BEGIN
    LockRW(RW);

    IF RW^.active > 0 THEN
        DEC(RW^.numReaders);
        IF RW^.numReaders = 0 THEN
            RW^.active := 0;
        END;
    ELSIF RW^.active < 0 THEN
		IF GetThreadTls() # RW^.writer THEN
	        UnlockRW(RW);
    	    Raise("RwUnlock issued when RwLock is write-locked by another thread");
		END;
        RW^.active := 0;
    ELSE
        UnlockRW(RW);
        Raise("Unlocked RwLock in RwUnlock");
    END;

    IF RW^.active = 0 THEN
        IF (RW^.firstWriteQ <> NIL) AND (RW^.wakePriority <= 0) THEN
            (* start one waiting writer *)

            th := RW^.firstWriteQ;
            IF th = RW^.lastWriteQ THEN
                RW^.firstWriteQ := NIL;
                RW^.lastWriteQ := NIL;
            ELSE
                RW^.firstWriteQ := RW^.firstWriteQ^.nextQ;
            END;

            RW^.active := -1;
			RW^.writer := th;

            RW^.wakePriority := INT(RW^.firstReadQ <> NIL);

            UnlockRW(RW);

            th^.nextQ := NIL;
            Awaken(th);

            RETURN;

        ELSIF RW^.firstReadQ <> NIL THEN
            (* start all waiting readers *)

            th := RW^.firstReadQ;

            RW^.active := 1;
            RW^.numReaders := RW^.readersWaiting;
            RW^.readersWaiting := 0;
            RW^.firstReadQ := NIL;
            RW^.lastReadQ := NIL;

            RW^.wakePriority := -INT(RW^.firstWriteQ <> NIL);

            UnlockRW(RW);

            WHILE th <> NIL DO
                next := th^.nextQ;

                th^.nextQ := NIL;
                Awaken(th);

                th := next;
            END;

            RETURN;
        END;
    END;

    UnlockRW(RW);
END RwUnlock;

PROCEDURE LockBarrier(B : Barrier) [INLINE];
BEGIN
    SpinLock(B^.lock);
END LockBarrier;

PROCEDURE UnlockBarrier(B : Barrier) [INLINE];
BEGIN
    MEMORY_FENCE;
    B^.lock := 0;
END UnlockBarrier;

PROCEDURE CreateBarrier(VAR OUT B : Barrier; count : CARDINAL) : BOOLEAN;
BEGIN
    NEW(B);
    IF B <> NIL THEN
        B^.lock := 0;
        B^.count := count;
        B^.inBarrier := 0;
        B^.firstQ := NIL;
        B^.lastQ := NIL;
        RETURN TRUE;
    END;
    RETURN FALSE;
END CreateBarrier;

PROCEDURE CloseBarrier(VAR INOUT B : Barrier) : BOOLEAN;
BEGIN
    IF B <> NIL THEN
        LockBarrier(B);
        IF B^.firstQ = NIL THEN
            DISPOSE (B);
            RETURN TRUE;
        END;
        UnlockBarrier(B);
        RETURN FALSE;
    END;
    RETURN TRUE;
END CloseBarrier;

PROCEDURE ReleaseBarrierQ(B : Barrier);
VAR
    th, next    : Thread;
BEGIN
    th := B^.firstQ;
    B^.firstQ := NIL;
    B^.lastQ := NIL;
    B^.inBarrier := 0;

    UnlockBarrier(B);

    WHILE th <> NIL DO
        next := th^.nextQ;

        th^.nextQ := NIL;
        Awaken(th);

        th := next;
    END;
END ReleaseBarrierQ;

PROCEDURE GetBarrierCount(B : Barrier) : CARDINAL;
VAR
    count       : CARDINAL;
BEGIN
    LockBarrier(B);

    count := B^.count;

    UnlockBarrier(B);

    RETURN count;
END GetBarrierCount;

PROCEDURE SetBarrierCount(B : Barrier; count : CARDINAL);
BEGIN
    LockBarrier(B);

    B^.count := count;

    IF B^.inBarrier < B^.count THEN
        UnlockBarrier(B);
    ELSE
        IF B^.lastQ <> NIL THEN
            B^.lastQ^.lastInBarrier := TRUE;
        END;
        ReleaseBarrierQ(B);
    END;
END SetBarrierCount;

PROCEDURE AdjustBarrierCount(B : Barrier; incr : INTEGER);
BEGIN
    LockBarrier(B);

    IF incr >= 0 THEN
        B^.count := B^.count + ORD(incr);
    ELSE
        incr := -incr;
        IF B^.count >= ORD(incr) THEN
            B^.count := B^.count - ORD(incr);
        ELSE
            B^.count := 0;
        END;
    END;

    IF B^.inBarrier < B^.count THEN
        UnlockBarrier(B);
    ELSE
        IF B^.lastQ <> NIL THEN
            B^.lastQ^.lastInBarrier := TRUE;
        END;
        ReleaseBarrierQ(B);
    END;
END AdjustBarrierCount;

PROCEDURE WaitAtBarrier(B : Barrier) : BOOLEAN;
VAR
    th          : Thread;
    last        : BOOLEAN;
    res         : WaitResult;
BEGIN
    last := FALSE;

    LockBarrier(B);

    INC(B^.inBarrier);
    last := B^.inBarrier >= B^.count;

    IF NOT last THEN
		th := GetThreadTls();

	    th^.lastInBarrier := FALSE;

	    Enqueue(th, B^.firstQ, B^.lastQ);

		UnlockBarrier(B);

		res := Hibernate(-1);

        (* this is in case threads were waiting in the barrier and the
           Set/Adjust barrier count calls were made and the queue needed
           to be released.
           Some thread always says it was last.
        *)
        last := th^.lastInBarrier;

		IF res <> WaitSuccess THEN
			Raise("WaitError in WaitAtBarrier");
		END;
	ELSE
    	ReleaseBarrierQ(B);
    END;

    RETURN last;
END WaitAtBarrier;

PROCEDURE CreateMutexSem(VAR OUT M : MutexSem;
                         name : ARRAY OF CHAR;
                         VAR OUT created : BOOLEAN) : BOOLEAN;
BEGIN
    NEW(M);
    IF M <> NIL THEN
        IF name[0] = '' THEN
            M^.mutex := CreateMutex(SecAttr, FALSE, NIL_STR);
        ELSE
            M^.mutex := CreateMutex(SecAttr, FALSE, name);
        END;
        IF M^.mutex <> NULL_HANDLE THEN
            M^.count := 0;
            created := WIN32.GetLastError() <> ERROR_ALREADY_EXISTS;
            RETURN TRUE;
        END;
        DISPOSE(M);
    END;
    RETURN FALSE;
END CreateMutexSem;

PROCEDURE OpenMutexSem(VAR OUT M : MutexSem; name : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    M := NIL;
    IF name[0] <> '' THEN
        NEW(M);
        IF M <> NIL THEN
            M^.mutex := OpenMutex(SYNCHRONIZE, FALSE, name);
            IF M^.mutex <> NULL_HANDLE THEN
                M^.count := 0;
                RETURN TRUE;
            END;
            DISPOSE(M);
        END;
    END;
    RETURN FALSE;
END OpenMutexSem;

PROCEDURE CloseMutexSem(VAR INOUT M : MutexSem) : BOOLEAN;
BEGIN
    IF M <> NIL THEN
        IF CloseHandle(M^.mutex) THEN
            DISPOSE (M);
            RETURN TRUE;
        END;
        RETURN FALSE;
    END;
    RETURN TRUE;
END CloseMutexSem;

PROCEDURE RequestMutexSem(M : MutexSem; timeout : INTEGER) : WaitResult;
VAR
    winRes      : CARDINAL;
    waitRes     : WaitResult;
BEGIN
    winRes := MyWaitForSingleObject(M^.mutex, timeout);
    waitRes := GetWaitResult(winRes, 1);
    IF waitRes = WaitSuccess THEN
        INC(M^.count);
    END;
    RETURN waitRes;
END RequestMutexSem;

PROCEDURE ReleaseMutexSem(M : MutexSem);
BEGIN
    IF M^.count > 0 THEN
        DEC(M^.count);
    END;

    IF NOT ReleaseMutex(M^.mutex) THEN
        Raise("ReleaseMutex error in ReleaseMutexSem");
    END;
END ReleaseMutexSem;

PROCEDURE GetMutexSemCount(M : MutexSem) : CARDINAL;
VAR
    count       : CARDINAL;
BEGIN
    count := 0;
    IF MyWaitForSingleObject(M^.mutex, -1) = WAIT_OBJECT_0 THEN
        count := M^.count;
        IF NOT ReleaseMutex(M^.mutex) THEN
            Raise("ReleaseMutex error in GetMutexSemCount");
        END;
    END;
    RETURN count;
END GetMutexSemCount;

PROCEDURE PopMutexSem(M : MutexSem; count : CARDINAL);
BEGIN
    IF MyWaitForSingleObject(M^.mutex, -1) = WAIT_OBJECT_0 THEN
        WHILE M^.count > count DO
            ReleaseMutex(M^.mutex);
            DEC(M^.count);
        END;
        IF NOT ReleaseMutex(M^.mutex) THEN
            Raise("ReleaseMutex error in PopMutexSem");
        END;
    END;
END PopMutexSem;

PROCEDURE CreateEventSem(VAR OUT E : EventSem;
                         name : ARRAY OF CHAR;
                         VAR OUT created : BOOLEAN) : BOOLEAN;
BEGIN
    NEW(E);
    IF E <> NIL THEN
        IF name[0] = '' THEN
            E^.event := CreateEvent(SecAttr, TRUE, FALSE, NIL_STR);
        ELSE
            E^.event := CreateEvent(SecAttr, TRUE, FALSE, name);
        END;
        IF E^.event <> NULL_HANDLE THEN
            created := WIN32.GetLastError() <> ERROR_ALREADY_EXISTS;
            RETURN TRUE;
        END;
        DISPOSE(E);
    END;
    RETURN FALSE;
END CreateEventSem;

PROCEDURE OpenEventSem(VAR OUT E : EventSem; name : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    E := NIL;
    IF name[0] <> '' THEN
        NEW(E);
        IF E <> NIL THEN
            E^.event := OpenEvent(SYNCHRONIZE BOR EVENT_MODIFY_STATE, FALSE, name);
            IF E^.event <> NULL_HANDLE THEN
                RETURN TRUE;
            END;
            DISPOSE(E);
        END;
    END;
    RETURN FALSE;
END OpenEventSem;

PROCEDURE CloseEventSem(VAR INOUT E : EventSem) : BOOLEAN;
BEGIN
    IF E <> NIL THEN
        IF CloseHandle(E^.event) THEN
            DISPOSE (E);
            RETURN TRUE;
        END;
        RETURN FALSE;
    END;
    RETURN TRUE;
END CloseEventSem;

PROCEDURE SetEventSem(E : EventSem);
BEGIN
    IF NOT SetEvent(E^.event) THEN
        Raise("SetEvent error");
    END;
END SetEventSem;

PROCEDURE ResetEventSem(E : EventSem);
BEGIN
    IF NOT ResetEvent(E^.event) THEN
        Raise("ResetEvent error");
    END;
END ResetEventSem;

PROCEDURE PulseEventSem(E : EventSem);
BEGIN
    IF NOT PulseEvent(E^.event) THEN
        Raise("PulseEvent error");
    END;
END PulseEventSem;

PROCEDURE WaitForEventSem(E : EventSem; timeout : INTEGER) : WaitResult;
VAR
    winRes      : CARDINAL;
BEGIN
    winRes := MyWaitForSingleObject(E^.event, timeout);
    RETURN GetWaitResult(winRes, 1);
END WaitForEventSem;

PROCEDURE CreateSignalSem(VAR OUT S : SignalSem;
                          maxSent, initialSent : CARDINAL;
                          name : ARRAY OF CHAR;
                          VAR OUT created : BOOLEAN) : BOOLEAN;
BEGIN
    NEW(S);
    IF S <> NIL THEN
        IF maxSent = 0 THEN
            maxSent := MAX(INTEGER);
        ELSIF maxSent > MAX(INTEGER) THEN
            RETURN FALSE;
        END;
        IF initialSent > maxSent THEN
            initialSent := maxSent;
        END;
        IF name[0] = '' THEN
            S^.sem := CreateSemaphore(SecAttr, initialSent, maxSent, NIL_STR);
        ELSE
            S^.sem := CreateSemaphore(SecAttr, initialSent, maxSent, name);
        END;
        IF S^.sem <> NULL_HANDLE THEN
            created := WIN32.GetLastError() <> ERROR_ALREADY_EXISTS;
            RETURN TRUE;
        END;
        DISPOSE(S);
    END;
    RETURN FALSE;
END CreateSignalSem;

PROCEDURE OpenSignalSem(VAR OUT S : SignalSem; name : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    S := NIL;
    IF name[0] <> '' THEN
        NEW(S);
        IF S <> NIL THEN
            S^.sem := OpenSemaphore(SYNCHRONIZE BOR SEMAPHORE_MODIFY_STATE, FALSE, name);
            IF S^.sem <> NULL_HANDLE THEN
                RETURN TRUE;
            END;
            DISPOSE(S);
        END;
    END;
    RETURN FALSE;
END OpenSignalSem;

PROCEDURE CloseSignalSem(VAR INOUT S : SignalSem) : BOOLEAN;
BEGIN
    IF S <> NIL THEN
        IF CloseHandle(S^.sem) THEN
            DISPOSE (S);
            RETURN TRUE;
        END;
        RETURN FALSE;
    END;
    RETURN TRUE;
END CloseSignalSem;

PROCEDURE SendSignalSem(S : SignalSem; count : CARDINAL) : BOOLEAN;
BEGIN
    RETURN ReleaseSemaphore(S^.sem, count, NIL);
END SendSignalSem;

PROCEDURE ResetSignalSem(S : SignalSem);
BEGIN
    WHILE MyWaitForSingleObject(S^.sem, 0) = WAIT_OBJECT_0 DO
    END;
END ResetSignalSem;

PROCEDURE WaitForSignalSem(S : SignalSem; timeout : INTEGER) : WaitResult;
VAR
    winRes      : CARDINAL;
BEGIN
    winRes := MyWaitForSingleObject(S^.sem, timeout);
    RETURN GetWaitResult(winRes, 1);
END WaitForSignalSem;

PROCEDURE CreateMultiSem(VAR OUT M : MultiSem;
                         allTrigger : BOOLEAN) : BOOLEAN;
BEGIN
    NEW(M);
    IF M <> NIL THEN
        M^.lock := 0;
        M^.max := MaxMultiSem;
        M^.count := 0;
        M^.allTrigger := allTrigger;
        RETURN TRUE;
    END;

    RETURN FALSE;
END CreateMultiSem;

PROCEDURE CloseMultiSem(VAR INOUT M : MultiSem) : BOOLEAN;
BEGIN
    IF M <> NIL THEN
        DISPOSE (M);
    END;
    RETURN TRUE;
END CloseMultiSem;

PROCEDURE AddMulti(M : MultiSem; h : HANDLE; user : CARDINAL) : BOOLEAN;
VAR
    i   : CARDINAL;
    ok  : BOOLEAN;
BEGIN
    WHILE ATOMIC_CMPXCHG(M^.lock, 0, 1) <> 0 DO
        ScheduleThread;
    END;
    MEMORY_FENCE;

    FOR i := 1 TO M^.count DO
        IF M^.handles[i] = h THEN
            MEMORY_FENCE;
            M^.lock := 0;
            RETURN FALSE;
        END;
    END;

    ok := FALSE;
    IF M^.count < M^.max THEN
        ok := TRUE;
        INC(M^.count);
        M^.handles[M^.count] := h;
        M^.user[M^.count] := user;
    END;

    MEMORY_FENCE;
    M^.lock := 0;
    RETURN ok;

EXCEPT
    MEMORY_FENCE;
    M^.lock := 0;
END AddMulti;

PROCEDURE RemoveMulti(M : MultiSem; h : HANDLE) : BOOLEAN;
VAR
    i           : ADRCARD;
    j           : ADRCARD;
    count       : ADRCARD;
    max         : ADRCARD;
BEGIN
    WHILE ATOMIC_CMPXCHG(M^.lock, 0, 1) <> 0 DO
        ScheduleThread;
    END;
    MEMORY_FENCE;

    count := M^.count;
    FOR i := 1 TO count DO
        IF M^.handles[i] = h THEN
            max := M^.max;
            FOR j := i+1 TO max DO
                M^.handles[j-1] := M^.handles[j];
                M^.user[j-1] := M^.user[j];
            END;
            DEC(M^.count);

            MEMORY_FENCE;
            M^.lock := 0;
            RETURN TRUE;
        END;
    END;

    MEMORY_FENCE;
    M^.lock := 0;
    RETURN FALSE;

EXCEPT
    MEMORY_FENCE;
    M^.lock := 0;
END RemoveMulti;

PROCEDURE AddEventSemToMultiSem(M : MultiSem;
                                S : EventSem;
                                user : CARDINAL) : BOOLEAN;
BEGIN
    RETURN AddMulti(M, S^.event, user);
END AddEventSemToMultiSem;

PROCEDURE AddMutexSemToMultiSem(M : MultiSem;
                                S : MutexSem;
                                user : CARDINAL) : BOOLEAN;
BEGIN
    RETURN AddMulti(M, S^.mutex, user);
END AddMutexSemToMultiSem;

PROCEDURE AddSignalSemToMultiSem(M : MultiSem;
                                 S : SignalSem;
                                 user : CARDINAL) : BOOLEAN;
BEGIN
    RETURN AddMulti(M, S^.sem, user);
END AddSignalSemToMultiSem;

PROCEDURE RemoveEventSemFromMultiSem(M : MultiSem; S : EventSem) : BOOLEAN;
BEGIN
    RETURN RemoveMulti(M, S^.event);
END RemoveEventSemFromMultiSem;

PROCEDURE RemoveMutexSemFromMultiSem(M : MultiSem; S : MutexSem) : BOOLEAN;
BEGIN
    RETURN RemoveMulti(M, S^.mutex);
END RemoveMutexSemFromMultiSem;

PROCEDURE RemoveSignalSemFromMultiSem(M : MultiSem; S : SignalSem) : BOOLEAN;
BEGIN
    RETURN RemoveMulti(M, S^.sem);
END RemoveSignalSemFromMultiSem;

PROCEDURE WaitForMultiSem(M : MultiSem;
                          VAR OUT user : CARDINAL;
                          timeout : INTEGER) : WaitResult;
VAR
    winRes      : CARDINAL;
    index       : ADRCARD;
    res         : WaitResult;
    winTime     : DWORD;
BEGIN
    IF timeout < 0 THEN
        winTime := INFINITE;
    ELSE
        winTime := timeout;
    END;
    LOOP
        winRes := WaitForMultipleObjectsEx(M^.count,
                                           M^.handles,
                                           M^.allTrigger,
                                           winTime,
                                           TRUE);
        IF winRes <> WAIT_IO_COMPLETION THEN
            EXIT;
        END;
    END;

    index := 0;
    res := GetWaitResult(winRes, M^.count);
    CASE res OF
    WaitSuccess:
        index := (winRes - WAIT_OBJECT_0) + 1;
    |
    WaitAbandoned:
        index := (winRes - WAIT_ABANDONED_0) + 1;
    ELSE
    END;

    IF index > 0 THEN
        user := M^.user[index];
    END;

    ReapExternalThreads;

    RETURN res;
END WaitForMultiSem;

PROCEDURE DefaultScheduleThread() : BOOL [WINDOWS];
BEGIN
    Sleep(0);
    RETURN FALSE;
END DefaultScheduleThread;

PROCEDURE InitThreads;
VAR
    dll         : HINSTANCE;
    temp        : FARPROC;
BEGIN
    AllocateSource(ExceptSrc);

    SpinCount := LockSpinCount;
    IF CPUCOUNT = 1 THEN
        SpinCount := 0;
    END;

    ThreadTls := TlsAlloc();
    IF ThreadTls = TLS_OUT_OF_INDEXES THEN
        Raise("Threads init no TLS slot available");
    END;

    InitializeCriticalSection(ThreadSem);

    (* setup a full access security descriptor *)

    InitializeSecurityDescriptor(SecDesc, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(SecDesc, TRUE, NIL, FALSE);
    SecAttr.nLength := SIZE(SecAttr);
    SecAttr.lpSecurityDescriptor := ADR(SecDesc);
    SecAttr.bInheritHandle := FALSE;

    OsVersion.dwOSVersionInfoSize := SIZE(OsVersion);
    GetVersionEx(OsVersion);

    ScheduleThread := DefaultScheduleThread;
    dll := LoadLibrary("KERNEL32");
    IF dll <> NULL_HINSTANCE THEN
        temp := GetProcAddress(dll, "SwitchToThread");
        IF CAST(ADDRESS, temp) <> NIL THEN
            ScheduleThread:FARPROC := temp;
        END;

        FreeLibrary(dll);
    END;

    ExtThreadCount := 0;
    ReapTick := GetTickCount();

    (* now allocate a thread record for the current thread *)

    NEW(FirstThread);
    IF FirstThread = NIL THEN
        Raise("Threads module could not initialize");
    END;
    LastThread := FirstThread;

    FirstThread^.next := NIL;
    FirstThread^.param := NIL;
    FirstThread^.proc := CAST(ThreadProcedure, NIL);
    FirstThread^.external := FALSE;
    FirstThread^.waitForWait := FALSE;
    ThreadSetup(FirstThread);
END InitThreads;

PROCEDURE DisposeThreads;
VAR
    next        : Thread;
BEGIN
    (* clean up our allocated memory.
       only the main thread should still be running at this point,
       but still free up memory consumed by other threads.
       threads really should not be running during termination.
    *)

    ReapExternalThreads;

    WHILE FirstThread <> NIL DO
        next := FirstThread^.next;
        DISPOSE(FirstThread);
        FirstThread := next;
    END;
    LastThread := NIL;

    IF ThreadTls <> TLS_OUT_OF_INDEXES THEN
        TlsFree(ThreadTls);
    END;
END DisposeThreads;

BEGIN
    (* in case this module is part of a Windows DLL, that inits per thread *)

    IF NOT IsThread THEN
        InitThreads;
    END;

FINALLY
    IF IsThread THEN
        ReapExtThread;
    ELSE
        DisposeThreads;
    END;
END Threads.
