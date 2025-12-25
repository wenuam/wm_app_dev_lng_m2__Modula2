(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE ElapsedTime;

FROM SYSTEM IMPORT
    IsThread;

%IF Windows %THEN

FROM WIN32 IMPORT
    Sleep, GetTickCount, QueryPerformanceFrequency, QueryPerformanceCounter;

%ELSIF UNIX %THEN

FROM UNIX IMPORT
    gettimeofday, timeval, timezone, timespec, nanosleep, time_t;

%ELSE

FROM SysClock IMPORT
    GetClock, DateTime;

%END

VAR
    Start       : CARDINAL32;

%IF Windows %THEN
    BaseTime    : LONGINT;
    ToMilli     : LONGINT;
    CurrentTime : PROCEDURE() : CARDINAL32;
%ELSIF UNIX %THEN
    BaseTime    : time_t;
%END

%IF Windows %THEN

PROCEDURE CurrentTimePerf() : CARDINAL32;
VAR
    time        : LONGINT;
    ok          : BOOLEAN;
BEGIN
    ok := QueryPerformanceCounter(time);
    RETURN (time - BaseTime) / ToMilli;
END CurrentTimePerf;

PROCEDURE CurrentTimeTick() : CARDINAL32;
BEGIN
    RETURN GetTickCount();
END CurrentTimeTick;

%ELSIF UNIX %THEN

PROCEDURE CurrentTime() : CARDINAL32;
VAR
    info        : timeval;
    tz          : timezone;
    time        : CARDINAL;
BEGIN
    time := gettimeofday(info, tz);
    <*/PUSH/NOCHECK:O*>
    time := ((info.tv_sec-BaseTime) * 1000) + (info.tv_usec / 1000);
    <*/POP*>
    RETURN time;
END CurrentTime;

%ELSE

PROCEDURE CurrentTime() : CARDINAL32;
VAR
    dt  : DateTime;
BEGIN
    GetClock(dt);
    RETURN (VAL(CARDINAL32, dt.fractions)) +
           (1000 *
            (VAL(CARDINAL32, dt.second) +
             (60 *
              (VAL(CARDINAL32, dt.minute) +
               (60 * VAL(CARDINAL32, dt.hour))
              )
             )
            )
           );
END CurrentTime;

%END

PROCEDURE StartTime;
BEGIN
    Start := StartTimeEx();
END StartTime;

PROCEDURE GetTime() : CARDINAL32;
BEGIN
    RETURN GetTimeEx(Start);
END GetTime;

PROCEDURE StartTimeEx() : CARDINAL32;
BEGIN
    RETURN CurrentTime();
END StartTimeEx;

PROCEDURE GetTimeEx(start : CARDINAL32) : CARDINAL32;
CONST
    last = MAX(CARDINAL32);
VAR
    now         : CARDINAL32;
BEGIN
    now := CurrentTime();
    IF now >= start THEN
        RETURN now-start;
    ELSE
        RETURN (last-start) + now;
    END;
END GetTimeEx;

PROCEDURE Delay(milli : CARDINAL32);
VAR
    start       : CARDINAL32;
    time        : CARDINAL32;
%IF UNIX %THEN
    time1       : timespec;
    time2       : timespec;
%END
BEGIN
    start := CurrentTime();
    LOOP
        time := GetTimeEx(start);
        IF time < milli THEN
            (* sleep only approximates real time *)
            (* thus sleep for less than the remaining delay time *)
            time := (milli-time) / 2;
            %IF Windows %THEN
                Sleep(time);
            %ELSIF UNIX %THEN
                time1.tv_sec := time / 1000;
                time1.tv_nsec := (time REM 1000) * 1000000;
                IF nanosleep(time1, time2) < 0 THEN
                END;
            %END
        ELSE
            EXIT;
        END;
    END;
END Delay;

%IF Windows %THEN

PROCEDURE Init;
BEGIN
    CurrentTime := CurrentTimeTick;

    IF QueryPerformanceFrequency(ToMilli) THEN
        ToMilli := ToMilli / 1000;
        IF QueryPerformanceCounter(BaseTime) THEN
            CurrentTime := CurrentTimePerf;
        END;
    END;
END Init;

%ELSIF UNIX %THEN

PROCEDURE Init;
VAR
    info        : timeval;
    tz          : timezone;
    time        : CARDINAL;
BEGIN
    time := gettimeofday(info, tz);
    BaseTime := info.tv_sec;
END Init;

%ELSE

PROCEDURE Init;
BEGIN
END Init;

%END

BEGIN
    IF NOT IsThread THEN
        Init;
    END;
END ElapsedTime.
