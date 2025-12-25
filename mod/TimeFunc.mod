(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE TimeFunc;

FROM SYSTEM IMPORT
    ADRCARD;

FROM SysClock IMPORT
    GetClock;

TYPE
    MonthArray  = ARRAY [1..12] OF CARDINAL32;
CONST
    MinSecs     = 60;
    HourSecs    = MinSecs * 60;
    DaySecs     = HourSecs * 24;
    MonthSecs   = MonthArray{31*DaySecs, 28*DaySecs,
                             31*DaySecs, 30*DaySecs,
                             31*DaySecs, 30*DaySecs,
                             31*DaySecs, 31*DaySecs,
                             30*DaySecs, 31*DaySecs,
                             30*DaySecs, 31*DaySecs};
    YearSecs    = DaySecs * 365;

TYPE JulianDate = CARDINAL32;
CONST MinYear = 1600;
CONST FirstTwoMonths = 59;
CONST FirstDayOfWeek = Saturday;

PROCEDURE DMYtoJulian(Day, Month, Year : CARDINAL32) : JulianDate;
BEGIN
    IF (Year = MinYear) AND (Month < 3) THEN
        IF Month = 1 THEN
            RETURN Day-1;
        ELSE
            RETURN Day+30;
        END;
    ELSE
        IF Month > 2 THEN
            Month := Month - 3;
        ELSE
            Month := Month + 9;
            Year := Year - 1;
        END;

        Year := Year - MinYear;
        RETURN (((Year / 100)*146097) / 4) +
               (((Year REM 100)*1461) / 4) +
               (((153*Month)+2) / 5)+Day+FirstTwoMonths;
    END;
END DMYtoJulian;

(*
PROCEDURE JulianToDMY(Julian : JulianDate;
                      VAR OUT Day, Month, Year : CARDINAL);
VAR
    I, J : CARDINAL;
BEGIN
    IF Julian <= FirstTwoMonths THEN
        Year := MinYear;
        IF Julian <= 30 THEN
            Month := 1;
            Day := Julian + 1;
        ELSE
            Month := 2;
            Day := Julian - 30;
        END;
    ELSE
        I := (4*(Julian-FirstTwoMonths))-1;
        J := (4*((I REM 146097) / 4))+3;
        Year := (100*(I / 146097))+(J / 1461);
        I := (5*(((J REM 1461)+4) / 4))-3;

        Month := I / 153;
        Day := ((I REM 153)+5) / 5;
        IF Month < 10 THEN
            Month := Month + 3;
        ELSE
            Month := Month - 9;
            Year := Year + 1;
        END;
        Year := Year + MinYear;
    END;
END JulianToDMY;
*)

(*
PROCEDURE IncJulianDate(julian : JulianDate;
                        days, months, years : INTEGER) : JulianDate;
VAR
    day,
    month,
    year        : CARDINAL;
    dayI,
    monthI,
    yearI       : INTEGER;
    day28Delta  : INTEGER;
    temp        : INTEGER;
BEGIN
    JulianToDMY(julian, day, month, year);
    dayI := day;
    monthI := month;
    yearI := year;

    day28Delta := dayI-28;
    IF day28Delta < 0 THEN
        day28Delta := 0;
    ELSE
        day := 28;
    END;

    yearI := yearI + years;
    yearI := yearI + (months / 12);
    monthI := monthI + (months REM 12);
    IF monthI < 1 THEN
        monthI := monthI + 12;
        yearI := yearI - 1;
    ELSIF monthI > 12 THEN
        monthI := monthI - 12;
        yearI := yearI + 1;
    END;

    temp := DMYtoJulian(dayI, monthI, yearI);
    temp := temp + days;
    temp := temp + day28Delta;
    IF temp < 0 THEN
        temp := 0;
    END;
    RETURN temp;
END IncJulianDate;
*)

PROCEDURE GetDayOfWeek(dt : DateTime) : DayOfWeek;
VAR
    julian      : JulianDate;
BEGIN
    julian := DMYtoJulian(dt.day, dt.month, dt.year);
    RETURN VAL(DayOfWeek, (julian+VAL(JulianDate, FirstDayOfWeek)) REM 7);
END GetDayOfWeek;

PROCEDURE CompareTime(left, right : DateTime) : INTEGER;

    PROCEDURE cmp(left, right : CARDINAL) : INTEGER;
    BEGIN
        IF left > right THEN
            RETURN 1;
        END;
        RETURN -1;
    END cmp;

BEGIN
    IF left.year = right.year THEN
        IF left.month = right.month THEN
            IF left.day = right.day THEN
                IF left.hour = right.hour THEN
                    IF left.minute = right.minute THEN
                        IF left.second = right.second THEN
                            IF left.fractions = right.fractions THEN
                                RETURN 0;
                            END;
                            RETURN cmp(left.fractions, right.fractions);
                        END;
                        RETURN cmp(left.second, right.second);
                    END;
                    RETURN cmp(left.minute, right.minute);
                END;
                RETURN cmp(left.hour, right.hour);
            END;
            RETURN cmp(left.day, right.day);
        END;
        RETURN cmp(left.month, right.month);
    END;
    RETURN cmp(left.year, right.year);
END CompareTime;

PROCEDURE DateTimeEqual(left, right : DateTime) : BOOLEAN;
BEGIN
    RETURN CompareTime(left, right) = 0;
END DateTimeEqual;

PROCEDURE DateTimeGreater(left, right : DateTime) : BOOLEAN;
BEGIN
    RETURN CompareTime(left, right) > 0;
END DateTimeGreater;

PROCEDURE GetCurrentZone(VAR OUT time : DateTime);
VAR
    local       : DateTime;
BEGIN
    GetClock(local);
    time.zone := local.zone;
    time.summerTimeFlag := local.summerTimeFlag;
END GetCurrentZone;

PROCEDURE LocalTimeToUTCTime(VAR INOUT time : DateTime);
VAR
    offset      : INTEGER;
    cdt         : CARDINAL32;
    saveFrac    : CARDINAL;
BEGIN
    saveFrac := time.fractions;

    DateTimeToC(time, cdt);
    offset := time.zone * 60;
    IF offset >= 0 THEN
        cdt := cdt + VAL(CARDINAL32, offset);
    ELSE
        cdt := cdt - VAL(CARDINAL32, -offset);
    END;
    CToDateTime(cdt, time);

    time.fractions := saveFrac;
END LocalTimeToUTCTime;

PROCEDURE UTCTimeToLocalTime(VAR INOUT time : DateTime);
VAR
    offset      : INTEGER;
    cdt         : CARDINAL32;
    saveFrac    : CARDINAL;
BEGIN
    saveFrac := time.fractions;

    DateTimeToC(time, cdt);
    offset := time.zone * 60;
    IF offset >= 0 THEN
        cdt := cdt - VAL(CARDINAL32, offset);
    ELSE
        cdt := cdt + VAL(CARDINAL32, -offset);
    END;
    CToDateTime(cdt, time);

    time.fractions := saveFrac;
END UTCTimeToLocalTime;

<*/PUSH/NOWARN:U*>
PROCEDURE DateTimeToC(dt : DateTime; VAR OUT cdt : CARDINAL32);
VAR
    i           : ADRCARD;
    month       : ADRCARD;
    tmp         : CARDINAL32;
    base        : CARDINAL;
    leapYears   : CARDINAL;
BEGIN
    tmp := dt.second;
    tmp := tmp + (VAL(CARDINAL32, dt.minute) * MinSecs);
    tmp := tmp + (VAL(CARDINAL32, dt.hour) * HourSecs);
    tmp := tmp + (VAL(CARDINAL32, dt.day-1) * DaySecs);
    month := dt.month;
    FOR i := 1 TO month-1 DO
        tmp := tmp + MonthSecs[i];
    END;

    (* if this is a leap year add in an extra day if we are *)
    (* after Feburary *)

    IF ((dt.year REM 4) = 0) AND
       ((dt.year REM 4000) <> 0) AND
       (
        ((dt.year REM 100) <> 0) OR
        ((dt.year REM 400) = 0)
       )
       AND
       (dt.month >= 3)
    THEN
        tmp := tmp + DaySecs;
    END;

    (* now add in the seconds for all previous years *)

    IF dt.year > 1970 THEN
        tmp := tmp + (VAL(CARDINAL32, dt.year - 1970) * YearSecs);

        IF dt.year >= 2000 THEN
            leapYears := 7;

            base := 2000;
            LOOP
                IF dt.year > base THEN
                    IF ((base REM 4) = 0) AND
                       ((base REM 4000) <> 0) AND
                       (
                        ((base REM 100) <> 0) OR
                        ((base REM 400) = 0)
                       )
                    THEN
                        INC(leapYears);
                    END;
                END;

                IF dt.year > base+100 THEN
                    leapYears := leapYears + 24;
                    base := base + 100;
                ELSE
                    EXIT;
                END;
            END;
        ELSE
            leapYears := 0;
            base := 1968;
        END;

        IF dt.year > base THEN
            leapYears := leapYears + ((dt.year - (base+1)) / 4);
        END;

        tmp := tmp + (VAL(CARDINAL32, leapYears) * DaySecs);
    END;
    cdt := tmp;
END DateTimeToC;
<*/POP*>

<*/PUSH/NOWARN:U*>
PROCEDURE CToDateTime(cdt : CARDINAL32; VAR OUT dt : DateTime);
VAR
    leapSecs    : CARDINAL32;
    addDay      : CARDINAL;
    i           : ADRCARD;
    base        : CARDINAL;
    leapYears   : CARDINAL;
BEGIN
    dt.year := (cdt / YearSecs) + 1970;
    cdt := cdt REM YearSecs;

    IF dt.year > 1970 THEN
        (* how many leap years do we think should have occured *)
        (* code fails on a time span more than 364 leap years *)

        IF dt.year >= 2000 THEN
            leapYears := 7;

            base := 2000;
            LOOP
                IF dt.year > base THEN
                    IF ((base REM 4) = 0) AND
                       ((base REM 4000) <> 0) AND
                       (
                        ((base REM 100) <> 0) OR
                        ((base REM 400) = 0)
                       )
                    THEN
                        INC(leapYears);
                    END;
                END;

                IF dt.year > base+100 THEN
                    leapYears := leapYears + 24;
                    base := base + 100;
                ELSE
                    EXIT;
                END;
            END;
        ELSE
            leapYears := 0;
            base := 1968;
        END;

        IF dt.year > base THEN
            leapYears := leapYears + ((dt.year - (base+1)) / 4);
        END;

        IF (VAL(CARDINAL32, leapYears)*DaySecs) <= cdt THEN
            cdt := cdt - (VAL(CARDINAL32, leapYears)*DaySecs);
        ELSE
            (* we are going to underflow a year, deal with it *)

            DEC(dt.year);
            cdt := cdt + YearSecs;

            cdt := cdt - (VAL(CARDINAL32, leapYears) * DaySecs);
        END;
    END;

    (* deal with Feb during a leap year *)

    leapSecs := MonthSecs[1] + MonthSecs[2];
    addDay := 0;
    IF ((dt.year REM 4) = 0) AND
       ((dt.year REM 4000) <> 0) AND
       (
        ((dt.year REM 100) <> 0) OR
        ((dt.year REM 400) = 0)
       )
       AND
       (cdt > leapSecs)
    THEN
        (* leap year beyond Feb 28 *)
        (* take a day out to make Feb look normal for the month loop *)

        IF cdt <= leapSecs+DaySecs THEN
            (* we are in the middle of Feb 29, add a fudge day *)
            (* because we are taking a day out *)

            addDay := 1;
        END;

        cdt := cdt - DaySecs;
    END;

    (* subtract out months to compute the month *)

    i := 1;
    WHILE MonthSecs[i] < cdt DO
        cdt := cdt - MonthSecs[i];
        INC(i);
    END;
    dt.month := i;

    (* now simply divide and remainder the rest out of whats left *)

    dt.day := ORD(cdt / DaySecs) + 1 + addDay;
    cdt := cdt REM DaySecs;
    dt.hour := cdt / HourSecs;
    cdt := cdt REM HourSecs;
    dt.minute := cdt / MinSecs;
    cdt := cdt REM MinSecs;
    dt.second := cdt;
    dt.fractions := 0;

    GetCurrentZone(dt);
END CToDateTime;
<*/POP*>

PROCEDURE DateTimeToDos(dt : DateTime; VAR OUT date, time : CARDINAL16);
BEGIN
    <*/PUSH/NOWARN:U*>
    date := (dt.year-1980) SHL 9;
    date := date BOR VAL(CARDINAL16, dt.month BAND 0Fh SHL 5);
    date := date BOR VAL(CARDINAL16, dt.day BAND 1Fh);

    time := dt.hour SHL 11;
    time := time BOR VAL(CARDINAL16, dt.minute BAND 3Fh SHL 5);
    time := time BOR VAL(CARDINAL16, (dt.second / 2) BAND 1Fh);
    <*/POP*>
END DateTimeToDos;

PROCEDURE DosToDateTime(date, time : CARDINAL16; VAR OUT dt : DateTime);
BEGIN
    dt.year := (date SHR 9) + 1980;
    dt.month := (date SHR 5 BAND 0Fh);
    dt.day := (date BAND 1Fh);

    dt.hour := (time SHR 11);
    dt.minute := (time SHR 5 BAND 3Fh);
    dt.second := (time BAND 1Fh) * 2;
    dt.fractions := 0;

    GetCurrentZone(dt);
END DosToDateTime;

END TimeFunc.
