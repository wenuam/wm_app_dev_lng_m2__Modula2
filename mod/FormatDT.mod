(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE FormatDT;
<*/NOWARN:F*>

FROM SYSTEM IMPORT
    %IF Windows %THEN
    ADRCARD,
    %END
    IsThread;

FROM SysClock IMPORT
    DateTime, GetClock;

FROM Strings IMPORT
    Append;

FROM ExStrings IMPORT
    EqualI;

FROM CharClass IMPORT
    IsNumeric, IsWhiteSpace;

FROM Conversions IMPORT
    StringToCard, CardToStr;

%IF Windows %THEN

FROM Conversions IMPORT
    StrToCard;

FROM WIN32 IMPORT
    LOCALE_USER_DEFAULT;

FROM WINNLS IMPORT
    GetLocaleInfo,
    LOCALE_ITLZERO, LOCALE_ITIME, LOCALE_S1159, LOCALE_S2359,
    LOCALE_IDATE, LOCALE_SDATE, LOCALE_STIME, LOCALE_SSHORTDATE;
%END

PROCEDURE SetDefaults;
BEGIN
    LeadingDayZero := TRUE;
    LeadingMonthZero := TRUE;
    LeadingTimeZero := TRUE;

    Time24Hour := FALSE;

    TimeSep := ':';
    DateSep := '/';

    FullYear := TRUE;

    AmStr := 'AM';
    PmStr := 'PM';

    DateFormat := MonthDayYear;
END SetDefaults;

%IF Windows %THEN

PROCEDURE GetSystemFormatInfo;
VAR
    card        : CARDINAL;
    i           : ADRCARD;
    l           : ADRCARD;
    numDig      : CARDINAL;
    data        : ARRAY [0..63] OF CHAR;
BEGIN
    SetDefaults;

    IF GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_ITLZERO, data, SIZE(data)) <> 0 THEN
        StrToCard(data, card);
        LeadingTimeZero := (card <> 0);
    END;

    IF GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_ITIME, data, SIZE(data)) <> 0 THEN
        StrToCard(data, card);
        Time24Hour := (card <> 0);
    END;

    IF GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_S1159, data, SIZE(data)) <> 0 THEN
        AmStr := data;
    END;
    IF GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_S2359, data, SIZE(data)) <> 0 THEN
        PmStr := data;
    END;

    IF GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IDATE, data, SIZE(data)) <> 0 THEN
        StrToCard(data, card);
        DateFormat := VAL(DateFormats, card);
    END;

    IF GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SDATE, data, SIZE(data)) <> 0 THEN
        DateSep := data;
    END;
    IF GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_STIME, data, SIZE(data)) <> 0 THEN
        TimeSep := data;
    END;

    IF GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SSHORTDATE, data, SIZE(data)) <> 0 THEN
        l := LENGTH(data);
        i := 0;
        WHILE i < l DO
            IF data[i] = 'd' THEN
                numDig := 0;
                REPEAT
                    INC(i);
                    INC(numDig);
                UNTIL (i > l) OR (data[i] <> 'd');
                LeadingDayZero := (numDig > 1);
            ELSIF data[i] = 'M' THEN
                numDig := 0;
                REPEAT
                    INC(i);
                    INC(numDig);
                UNTIL (i > l) OR (data[i] <> 'M');
                LeadingMonthZero := (numDig > 1);
            ELSIF data[i] = 'y' THEN
                numDig := 0;
                REPEAT
                    INC(i);
                    INC(numDig);
                UNTIL (i > l) OR (data[i] <> 'y');
                FullYear := (numDig > 2);
            ELSE
                INC(i);
            END;
        END;
    END;
END GetSystemFormatInfo;

%ELSE

PROCEDURE GetSystemFormatInfo;
BEGIN
    SetDefaults;
END GetSystemFormatInfo;

%END

PROCEDURE DateTimeToString(dt : DateTime; VAR OUT dateStr : ARRAY OF CHAR; VAR OUT timeStr : ARRAY OF CHAR);
VAR
    year        : CARDINAL;
    hour        : CARDINAL;
    am          : BOOLEAN;

    PROCEDURE putNum(num : CARDINAL; VAR INOUT str : ARRAY OF CHAR; leadingZero : BOOLEAN);
    VAR
        numStr  : ARRAY [0..9] OF CHAR;
    BEGIN
        CardToStr(num, numStr);
        IF (num < 10) AND leadingZero THEN
            Append('0', str);
        END;
        Append(numStr, str);
    END putNum;

BEGIN
    dateStr := "";
    timeStr := "";

    year := dt.year;
    IF NOT FullYear THEN
        year := year - ((year / 100) * 100);
    END;

    CASE DateFormat OF
    MonthDayYear:
        putNum(dt.month, dateStr, LeadingMonthZero);
        Append(DateSep, dateStr);
        putNum(dt.day, dateStr, LeadingDayZero);
        Append(DateSep, dateStr);
        putNum(year, dateStr, TRUE);
    |
    DayMonthYear:
        putNum(dt.day, dateStr, LeadingDayZero);
        Append(DateSep, dateStr);
        putNum(dt.month, dateStr, LeadingMonthZero);
        Append(DateSep, dateStr);
        putNum(year, dateStr, TRUE);
    |
    YearMonthDay:
        putNum(year, dateStr, TRUE);
        Append(DateSep, dateStr);
        putNum(dt.month, dateStr, LeadingMonthZero);
        Append(DateSep, dateStr);
        putNum(dt.day, dateStr, LeadingDayZero);
    END;


    hour := dt.hour;
    am := (hour < 12);
    IF (hour > 12) AND (NOT Time24Hour) THEN
        hour := hour - 12;
    END;

    putNum(hour, timeStr, LeadingTimeZero);
    Append(TimeSep, timeStr);
    putNum(dt.minute, timeStr, TRUE);

    IF NOT Time24Hour THEN
        Append(' ', timeStr);
        IF am THEN
            Append(AmStr, timeStr);
        ELSE
            Append(PmStr, timeStr);
        END;
    END;
END DateTimeToString;

PROCEDURE StringToDateTime(dateStr : ARRAY OF CHAR; timeStr : ARRAY OF CHAR; VAR OUT dt : DateTime) : BOOLEAN;
VAR
    i           : CARDINAL;
    l           : CARDINAL;
    ok          : BOOLEAN;
    num         : CARDINAL;

    PROCEDURE skipSpace(str : ARRAY OF CHAR);
    BEGIN
        WHILE (i <l) AND IsWhiteSpace(str[i]) DO
            INC(i);
        END;
    END skipSpace;

    PROCEDURE present(item : ARRAY OF CHAR; str : ARRAY OF CHAR; pos : CARDINAL) : BOOLEAN;
    BEGIN
        RETURN EqualI(item, str[pos..pos+LENGTH(item)-1]);
    END present;

    PROCEDURE getNum(str : ARRAY OF CHAR) : CARDINAL;
    VAR
        temp    : CARDINAL;
    BEGIN
        StringToCard(str, i, temp, ok);

        WHILE (i < l) AND NOT IsNumeric(str[i]) DO
            INC(i);
        END;
        RETURN temp;
    END getNum;

BEGIN
    (* this fills out the the fields we will not touch, like the time zone *)

    GetClock(dt);
    dt.fractions := 0;
    dt.second := 0;

    IF dateStr[0] <> 0C THEN
        i := 0;
        l := LENGTH(dateStr);

        CASE DateFormat OF
        MonthDayYear:
            dt.month := getNum(dateStr);
            dt.day := getNum(dateStr);
            dt.year := getNum(dateStr);
        |
        DayMonthYear:
            dt.day := getNum(dateStr);
            dt.month := getNum(dateStr);
            dt.year := getNum(dateStr);
        |
        YearMonthDay:
            dt.year := getNum(dateStr);
            dt.month := getNum(dateStr);
            dt.day := getNum(dateStr);
        END;

        (* this is a hack to allow two digit years and still support
           a 1900 and 2000 base.
        *)
        IF dt.year < 20 THEN
            dt.year := dt.year + 2000;
        ELSIF dt.year <= 99 THEN
            dt.year := dt.year + 1900;
        END;

        IF NOT ok THEN
            RETURN FALSE;
        END;
    END;

    IF timeStr[0] <> 0C THEN

        i := 0;
        l := LENGTH(timeStr);

        StringToCard(timeStr, i, num, ok);
        IF ok THEN
            dt.hour := num;
        ELSE
            RETURN FALSE;
        END;
        IF (i < l) AND present(TimeSep, timeStr, i) THEN
            i := i + LENGTH(TimeSep);

            StringToCard(timeStr, i, num, ok);
            IF ok THEN
                dt.minute := num;
            ELSE
                RETURN FALSE;
            END;
        END;

        skipSpace(timeStr);

        IF (i < l) AND present(AmStr, timeStr, i) THEN
            IF dt.hour > 12 THEN
                RETURN FALSE;
            END;
            i := i + LENGTH(AmStr);
            skipSpace(timeStr);
            RETURN i >= l;
        ELSIF (i < l) AND present(PmStr, timeStr, i) THEN
            IF dt.hour < 12 THEN
                dt.hour := dt.hour + 12;
            ELSE
                RETURN FALSE;
            END;
            i := i + LENGTH(PmStr);
            skipSpace(timeStr);
            RETURN i >= l;
        END;
        RETURN TRUE;
    END;
    RETURN FALSE;
END StringToDateTime;

BEGIN
    IF NOT IsThread THEN
        GetSystemFormatInfo;
    END;
END FormatDT.
