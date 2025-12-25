(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE StringCache;

<*/VALIDVERSION:PROTECT*>
%IF Windows %THEN
<*/VERSION:PROTECT*>
%END

%IF PROTECT %THEN
FROM SYSTEM IMPORT
    ATOMIC_CMPXCHG, MEMORY_FENCE;
%END
FROM SYSTEM IMPORT
    ADRCARD, IsThread;

FROM ExStorage IMPORT
    ALLOCATE, DEALLOCATE;

FROM WINX IMPORT
    Instance;

%IF Windows %THEN
FROM WINUSER IMPORT
%ELSE
FROM Windows IMPORT
%END
    LoadString;

%IF PROTECT %THEN
FROM Threads IMPORT
    YieldThread;
%END

TYPE
    ElementPointer      = POINTER TO Element;
    Element =
        RECORD
        idNum           : CARDINAL;
        dataHigh        : CARDINAL;
        data            : ARRAY [0..0] OF CHAR;
        END;

VAR
    Elements    : ADRCARD;
    StringSize  : CARDINAL;
    Cache       : POINTER TO ARRAY [0..0] OF ElementPointer;

%IF PROTECT %THEN
    Lock        : CARDINAL;
%END

PROCEDURE InitStringCache(numStrings, maxStringSize : CARDINAL);
VAR
    i   : ADRCARD;
BEGIN
    %IF PROTECT %THEN
        WHILE ATOMIC_CMPXCHG(Lock, 0, 1) <> 0 DO
            YieldThread;
        END;
        MEMORY_FENCE;
    %END

    IF Cache = NIL THEN
        Elements := numStrings;
        StringSize := maxStringSize;
        IF (maxStringSize REM 4) <> 0 THEN
            StringSize := StringSize + (4 - (maxStringSize REM 4));
        END;

        ALLOCATE(Cache, numStrings * SIZE(ElementPointer));
        FOR i := 0 TO Elements-1 DO
            ALLOCATE(Cache^[i],
                     SIZE(Element)-SIZE(CHAR)+(StringSize*SIZE(CHAR)));
            Cache^[i]^.dataHigh := 0;
            Cache^[i]^.data[0] := CHR(0);
            Cache^[i]^.idNum := MAX(CARDINAL);
        END;
    END;

%IF PROTECT %THEN
    <*/PUSH/NOWARN:F*>
    MEMORY_FENCE;
    Lock := 0;

EXCEPT
    MEMORY_FENCE;
    Lock := 0;
    <*/POP*>
%END
END InitStringCache;

PROCEDURE CacheHit(idNum : CARDINAL) : BOOLEAN;
VAR
    i           : ADRCARD;
    save        : ElementPointer;
BEGIN
    i := 0;

    LOOP
        IF i < Elements THEN
            IF idNum <> Cache^[i]^.idNum THEN
                INC(i);
            ELSE
                IF i <> 0 THEN
                    save := Cache^[i];
                    Cache^[1..i] := Cache^[0..i-1];
                    Cache^[0] := save;
                END;
                RETURN TRUE;
            END;
        ELSE
            EXIT;
        END;
    END;
    RETURN FALSE;
END CacheHit;

PROCEDURE LoadStringRes(idNum : CARDINAL; VAR str : ARRAY OF CHAR);
VAR
    len         : CARDINAL;
BEGIN
    len := LoadString(Instance, idNum, str, HIGH(str)+1);
    IF len = 0 THEN
        len := 1;
    END;

    IF len <= StringSize THEN
        Cache^[1..Elements-1] := Cache^[0..Elements-2];

        Cache^[0]^.dataHigh := len-1;
        Cache^[0]^.data[0..Cache^[0]^.dataHigh] := str;
        Cache^[0]^.idNum := idNum;
    END;
END LoadStringRes;

PROCEDURE GetStringResource(idNum : CARDINAL; VAR str : ARRAY OF CHAR);
BEGIN
    %IF PROTECT %THEN
        WHILE ATOMIC_CMPXCHG(Lock, 0, 1) <> 0 DO
            YieldThread;
        END;
        MEMORY_FENCE;
    %END

    IF CacheHit(idNum) THEN
        str := Cache^[0]^.data[0..Cache^[0]^.dataHigh];
    ELSE
        LoadStringRes(idNum, str);
    END;

%IF PROTECT %THEN
    <*/PUSH/NOWARN:F*>
    MEMORY_FENCE;
    Lock := 0;
EXCEPT
    MEMORY_FENCE;
    Lock := 0;
    <*/POP*>
%END
END GetStringResource;

PROCEDURE Term;
VAR
    i   : ADRCARD;
BEGIN
    IF Cache <> NIL THEN
        FOR i := 0 TO Elements-1 DO
            DEALLOCATE(Cache^[i],
                       SIZE(Element)-SIZE(CHAR)+(StringSize*SIZE(CHAR)));
        END;
        DEALLOCATE(Cache, Elements * SIZE(ElementPointer));
    END;
END Term;

BEGIN
    IF NOT IsThread THEN
        Cache := NIL;
        %IF PROTECT %THEN
            Lock := 0;
        %END
    END;
FINALLY
    IF NOT IsThread THEN
        Term;
    END;
END StringCache.
