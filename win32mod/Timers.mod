(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE Timers;

FROM SYSTEM IMPORT
    ADDRESS, ADRCARD, UNREFERENCED_PARAMETER, FUNC, IsThread;

FROM WIN32 IMPORT
    UINT, DWORD_PTR;

FROM MMSYSTEM IMPORT
    TIMECAPS, TIME_PERIODIC,
    timeGetDevCaps, timeBeginPeriod, timeEndPeriod,
    timeSetEvent, timeKillEvent;

CONST
    StandardResolution  = 10;

    NumTimers           = 10;

TYPE
    TimerRec =
    RECORD
        proc            : TimerProc;
        data            : ADDRESS;
        sysId           : CARDINAL;
        userId          : CARDINAL;
        resolution      : CARDINAL;
        used            : BOOLEAN;
    END;

VAR
    TimerArray          : ARRAY [1..NumTimers] OF TimerRec;
    TimerMinRes         : CARDINAL;
    TimerMaxRes         : CARDINAL;
    CurResolution       : CARDINAL;

PROCEDURE MyTimerProc(id : UINT;
                      res1 : UINT;
                      user : DWORD_PTR;
                      res2, res3 : DWORD_PTR) [OSCall, EXPORT];
BEGIN
    UNREFERENCED_PARAMETER(id);
    UNREFERENCED_PARAMETER(res1);
    UNREFERENCED_PARAMETER(res2);
    UNREFERENCED_PARAMETER(res3);

    TimerArray[user].proc(TimerArray[user].userId, TimerArray[user].data);
END MyTimerProc;

PROCEDURE SetResolution;
VAR
    min : CARDINAL;
    i   : ADRCARD;
BEGIN
    min := MAX(CARDINAL);
    FOR i := 1 TO NumTimers DO
        IF TimerArray[i].used THEN
            IF TimerArray[i].resolution < min THEN
                min := TimerArray[i].resolution;
            END;
        END;
    END;

    IF min <> CurResolution THEN
        IF CurResolution <> MAX(CARDINAL) THEN
            FUNC timeEndPeriod(CurResolution);
        END;
        CurResolution := min;
        IF CurResolution <> MAX(CARDINAL) THEN
            FUNC timeBeginPeriod(CurResolution);
        END;
    END;
END SetResolution;

PROCEDURE CreateTimer(proc : TimerProc;
                      data : ADDRESS;
                      timerId : CARDINAL;
                      interval : CARDINAL;
                      timer : Timers) : BOOLEAN;
VAR
    i           : ADRCARD;
    resolution  : CARDINAL;
BEGIN
    FOR i := 1 TO NumTimers DO
        IF NOT TimerArray[i].used THEN

            resolution := StandardResolution;
            IF timer = HighResTimer THEN
                resolution := interval / 3;
                IF resolution = 0 THEN
                    resolution := 1;
                ELSIF resolution > StandardResolution THEN
                    resolution := StandardResolution;
                END;
            END;
            IF resolution < TimerMinRes THEN
                resolution := TimerMinRes;
            ELSIF resolution > TimerMaxRes THEN
                resolution := TimerMaxRes;
            END;

            TimerArray[i].proc := proc;
            TimerArray[i].data := data;
            TimerArray[i].userId := timerId;
            TimerArray[i].resolution := resolution;
            TimerArray[i].used := TRUE;

            SetResolution;

            TimerArray[i].sysId := timeSetEvent(interval,
                                                resolution,
                                                MyTimerProc,
                                                i,
                                                TIME_PERIODIC);
            IF TimerArray[i].sysId <> 0 THEN
                RETURN TRUE;
            END;
            TimerArray[i].used := FALSE;
        END;
    END;
    RETURN FALSE;
END CreateTimer;

PROCEDURE DestroyTimer(timerId : CARDINAL);
VAR
    i   : ADRCARD;
BEGIN
    FOR i := 1 TO NumTimers DO
        IF TimerArray[i].used AND (TimerArray[i].userId = timerId) THEN
            FUNC timeKillEvent(TimerArray[i].sysId);
            TimerArray[i].used := FALSE;
            TimerArray[i].resolution := MAX(CARDINAL);
            SetResolution;
            RETURN;
        END;
    END;
END DestroyTimer;

PROCEDURE Init;
VAR
    caps        : TIMECAPS;
    i           : CARDINAL;
BEGIN
    FUNC timeGetDevCaps(caps, SIZE(caps));
    TimerMinRes := caps.wPeriodMin;
    TimerMaxRes := caps.wPeriodMax;
    CurResolution := MAX(CARDINAL);
    FOR i := 1 TO NumTimers DO
        TimerArray[i].used := FALSE;
        TimerArray[i].resolution := MAX(CARDINAL);
    END;
END Init;

PROCEDURE Term;
VAR
    i           : ADRCARD;
    setRes      : BOOLEAN;
BEGIN
    setRes := FALSE;
    FOR i := 1 TO NumTimers DO
        IF TimerArray[i].used THEN
            setRes := TRUE;
            FUNC timeKillEvent(TimerArray[i].sysId);
            TimerArray[i].used := FALSE;
            TimerArray[i].resolution := MAX(CARDINAL);
        END;
    END;
    IF setRes THEN
        SetResolution;
    END;
END Term;

BEGIN
    IF NOT IsThread THEN
        Init;
    END;
FINALLY
    IF NOT IsThread THEN
        Term;
    END;
END Timers.
