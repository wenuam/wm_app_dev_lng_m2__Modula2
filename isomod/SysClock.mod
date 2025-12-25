IMPLEMENTATION MODULE SysClock;
<*/NOWARN:F*>

(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs  1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman

    Stony Brook Compiler port Implementation
        copyright (c) 1994-2004
        by ADW Software
=========================================== *)

%IF Windows %THEN

FROM WIN32 IMPORT
    GetLocalTime, SYSTEMTIME,
    GetTimeZoneInformation, TIME_ZONE_INFORMATION,
    TIME_ZONE_ID_DAYLIGHT, TIME_ZONE_ID_UNKNOWN;

%ELSIF UNIX %THEN

FROM UNIX IMPORT
    %IF %NOT LINUX %THEN
    time_t, mktime, gmtime_r,
    %END
    timeval, timezone, gettimeofday, tm, localtime_r;

%END

PROCEDURE CanGetClock() : BOOLEAN;
BEGIN
    RETURN TRUE;
END CanGetClock;

PROCEDURE CanSetClock() : BOOLEAN;
BEGIN
    RETURN FALSE;
END CanSetClock;

PROCEDURE IsValidDateTime(userData : DateTime) : BOOLEAN;
BEGIN
    RETURN (userData.hour <= MAX(Hour)) AND
           (userData.minute <= MAX(Min)) AND
           (userData.second <= MAX(Sec)) AND
           (userData.fractions <= MAX(Fraction)) AND
           (userData.year >= 1600) AND
           (userData.year <= 3999) AND
           (userData.month >= MIN(Month)) AND
           (userData.month <= MAX(Month)) AND
           (userData.day >= MIN(Day)) AND
           (userData.day <= MAX(Day));
END IsValidDateTime;

PROCEDURE GetClock(VAR userDt : DateTime);
%IF Windows %THEN
VAR
    st          : SYSTEMTIME;
    zone        : TIME_ZONE_INFORMATION;
    retVal      : CARDINAL32;
%ELSIF LINUX %THEN
VAR
    st  : tm;
    t   : timeval;
    tz  : timezone;
%ELSIF UNIX %THEN
VAR
    st          : tm;
    t           : timeval;
    tz          : timezone;
    t1          : time_t;
%END
BEGIN
    %IF Windows %THEN
        GetLocalTime(st);

        userDt.year := st.wYear;
        userDt.month := st.wMonth;
        userDt.day := st.wDay;

        userDt.hour := st.wHour;
        userDt.minute := st.wMinute;
        userDt.second := st.wSecond;
        userDt.fractions := st.wMilliseconds;

        retVal := GetTimeZoneInformation(zone);
        IF retVal <> 0FFFFFFFFh THEN
            userDt.zone := zone.Bias;

            IF retVal <> TIME_ZONE_ID_UNKNOWN THEN
                userDt.summerTimeFlag := retVal = TIME_ZONE_ID_DAYLIGHT;
                IF userDt.summerTimeFlag THEN
                    userDt.zone := userDt.zone + zone.DaylightBias;
                END;
            END;
        END;

    %ELSIF UNIX %THEN

        (* does not give fractions of a second
        time(t);
        localtime_r(t, st);
        *)
        gettimeofday(t, tz);
        localtime_r(t.tv_sec, st);

        userDt.year := st.tm_year + 1900;
        userDt.month := st.tm_mon+1;
        userDt.day := st.tm_mday;

        userDt.hour := st.tm_hour;
        userDt.minute := st.tm_min;
        userDt.second := st.tm_sec;
        userDt.fractions := t.tv_usec / 1000;
        userDt.summerTimeFlag := st.tm_isdst > 0;

        %IF LINUX %THEN
            userDt.zone := -(st.tm_gmtoff / 60);
        %ELSE
            gmtime_r(t.tv_sec, st);
            t1 := mktime(st);
            userDt.zone := (t1 - t.tv_sec) / 60;
        %END
    %END
END GetClock;

PROCEDURE SetClock(userDt : DateTime);
BEGIN
    IF IsValidDateTime(userDt) THEN
    END;
END SetClock;

END SysClock.
