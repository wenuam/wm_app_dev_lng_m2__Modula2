(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE ExStrings;
<*/OPT:T*>

FROM SYSTEM IMPORT
    ADR, ADDADR;

IMPORT Strings;

%IF Windows %THEN

FROM SYSTEM IMPORT
    FUNC;

FROM WIN32 IMPORT
    BOOL;

FROM WINUSER IMPORT
    CharLowerBuff;

IMPORT WINNLS;

%ELSIF UNIX %THEN

FROM UNIX IMPORT
    mbstowcs, wctob, EOF;

%ELSE

FROM SYSTEM IMPORT
    UNREFERENCED_PARAMETER;

%END

CONST
    (* threshold of doing a byte move loop, verses an array slice
       which uses a memory move procedure.

       the way the char move loops are written helps processor data prefetch,
       and on RISC processors gets things such that a single instruction load
       is generated. this is less code but possibly more importantly it
       helps processor with data prefetch.
       helping the processor data prefetch reduces pipeline stalls.
    *)
%IF IA32 %THEN
    Threshold   = 24;
%ELSE
    Threshold   = 32;
%END

    Digits : ARRAY [0..15] OF CHAR = {"0123456789ABCDEF"};

PROCEDURE AssignNullTerm(source : ARRAY OF CHAR; VAR OUT destination : ARRAY OF CHAR);
VAR
    i           : CARDINAL;
    h           : CARDINAL;
    highSource  : CARDINAL;
    highDest    : CARDINAL;
BEGIN
    h := HIGH(destination);
    highDest := h;
    highSource := HIGH(source);
    IF highSource < h THEN
        h := highSource;
    END;

    i := 0;
    LOOP
        IF (i <= h) AND (source[i] <> '') THEN
            destination[i] := source[i];
            INC(i);

            IF (i <= h) AND (source[i] <> '') THEN
                destination[i] := source[i];
                INC(i);
            ELSE
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;

    IF i <= highDest THEN
        destination[i] := '';
    ELSE
        destination[highDest] := '';
    END;
END AssignNullTerm;

PROCEDURE CompareI(stringVal1, stringVal2 : ARRAY OF CHAR) : CompareResults;
VAR
    i           : CARDINAL;
    h           : CARDINAL;
    highVal1    : CARDINAL;
BEGIN
    h := HIGH(stringVal2);
    highVal1 := HIGH(stringVal1);
    IF highVal1 < h THEN
        h := highVal1;
    END;

    i := 0;
    LOOP
        IF (i <= h) AND
           (CAP(stringVal1[i]) = CAP(stringVal2[i])) AND
           (stringVal1[i] <> '')
        THEN
            INC(i);
        ELSE
            EXIT;
        END;
    END;

    IF i <= h THEN
        IF CAP(stringVal1[i]) < CAP(stringVal2[i]) THEN
            RETURN less;
        ELSIF CAP(stringVal1[i]) > CAP(stringVal2[i]) THEN
            RETURN greater;
        END;
        RETURN equal;
    ELSE
        IF HIGH(stringVal1) < HIGH(stringVal2) THEN
            IF stringVal2[i] = CHR(0) THEN
                RETURN equal;
            END;
            RETURN less;
        ELSIF HIGH(stringVal1) > HIGH(stringVal2) THEN
            IF stringVal1[i] = CHR(0) THEN
                RETURN equal;
            END;
            RETURN greater;
        ELSE
            RETURN equal;
        END;
    END;
END CompareI;

PROCEDURE EqualI(stringVal1, stringVal2 : ARRAY OF CHAR) : BOOLEAN;
VAR
    i           : CARDINAL;
    h           : CARDINAL;
    highVal1    : CARDINAL;
BEGIN
    h := HIGH(stringVal2);
    highVal1 := HIGH(stringVal1);
    IF highVal1 < h THEN
        h := highVal1;
    END;

    i := 0;
    LOOP
        IF (i <= h) AND
           (CAP(stringVal1[i]) = CAP(stringVal2[i])) AND
           (stringVal1[i] <> '')
        THEN
            INC(i);
        ELSE
            EXIT;
        END;
    END;

    IF i <= h THEN
        RETURN CAP(stringVal1[i]) = CAP(stringVal2[i]);
    ELSE
        IF HIGH(stringVal1) < HIGH(stringVal2) THEN
            IF stringVal2[i] = CHR(0) THEN
                RETURN TRUE;
            END;
            RETURN FALSE;
        ELSIF HIGH(stringVal1) > HIGH(stringVal2) THEN
            IF stringVal1[i] = CHR(0) THEN
                RETURN TRUE;
            END;
            RETURN FALSE;
        END;
        RETURN TRUE;
    END;
END EqualI;

PROCEDURE FindNextI(pattern, stringToSearch : ARRAY OF CHAR;
                    startIndex : CARDINAL;
                    VAR OUT patternFound : BOOLEAN;
                    VAR OUT posOfPattern : CARDINAL);
VAR
    i           : CARDINAL;
    j           : CARDINAL;
    k           : CARDINAL;
    endP        : CARDINAL;
    l2          : CARDINAL;
    ch          : CHAR;
BEGIN
    patternFound := FALSE;

    endP := LENGTH(pattern);
    IF endP <> 0 THEN
        DEC(endP);

        l2 := LENGTH(stringToSearch);
        i := startIndex;

        IF endP = 0 THEN
            ch := CAP(pattern[0]);
            WHILE (i < l2) AND (ch <> CAP(stringToSearch[i])) DO
                INC(i);
                IF (i < l2) AND (ch <> CAP(stringToSearch[i])) THEN
                    INC(i);
                END;
            END;

            IF i < l2 THEN
                patternFound := TRUE;
                posOfPattern := i;
                RETURN;
            END;
        ELSE
            ch := CAP(pattern[endP]);
            WHILE i + endP < l2 DO
                IF ch = CAP(stringToSearch[i+endP]) THEN
                    j := 0;
                    k := i;
                    LOOP
                        IF CAP(pattern[j]) <> CAP(stringToSearch[k]) THEN
                            EXIT;
                        END;
                        INC(j);
                        INC(k);
                        IF j = endP THEN
                            patternFound := TRUE;
                            posOfPattern := i;
                            RETURN;
                        END;
                    END;
                END;
                INC(i);
            END;
        END;
    END;
END FindNextI;

PROCEDURE FindPrevI(pattern, stringToSearch : ARRAY OF CHAR;
                    startIndex : CARDINAL;
                    VAR OUT patternFound : BOOLEAN;
                    VAR OUT posOfPattern : CARDINAL);
VAR
    i           : CARDINAL;
    j           : CARDINAL;
    k           : CARDINAL;
    endP        : CARDINAL;
    l2          : CARDINAL;
    ch          : CHAR;
BEGIN
    patternFound := FALSE;

    endP := LENGTH(pattern);
    IF endP <> 0 THEN
        DEC(endP);

        l2 := LENGTH(stringToSearch);
        i := startIndex;

        IF endP = 0 THEN
            ch := CAP(pattern[0]);
            WHILE (i > 0) AND (ch <> CAP(stringToSearch[i])) DO
                DEC(i);
                IF (i > 0) AND (ch <> CAP(stringToSearch[i])) THEN
                    DEC(i);
                END;
            END;

            IF (i > 0) OR (ch = CAP(stringToSearch[0])) THEN
                patternFound := TRUE;
                posOfPattern := i;
                RETURN;
            END;
        ELSE
            ch := CAP(pattern[endP]);
            INC(i);
            REPEAT
                DEC(i);
                IF (i + endP < l2) AND (ch = CAP(stringToSearch[i+endP])) THEN
                    j := 0;
                    k := i;
                    LOOP
                        IF CAP(pattern[j]) <> CAP(stringToSearch[k]) THEN
                            EXIT;
                        END;
                        INC(j);
                        INC(k);
                        IF j = endP THEN
                            patternFound := TRUE;
                            posOfPattern := i;
                            RETURN;
                        END;
                    END;
                END;
            UNTIL i = 0;
        END;
    END;
END FindPrevI;

PROCEDURE FindDiffI(stringVal1, stringVal2 : ARRAY OF CHAR;
                    VAR OUT differenceFound : BOOLEAN;
                    VAR OUT posOfDifference : CARDINAL);
VAR
    i   : CARDINAL;
    l1  : CARDINAL;
    l2  : CARDINAL;
BEGIN
    differenceFound := FALSE;

    l1 := LENGTH(stringVal1);
    l2 := LENGTH(stringVal2);

    i := 0;
    WHILE (i < l1) AND
          (i < l2) AND
          (CAP(stringVal1[i]) = CAP(stringVal2[i]))
    DO
        INC(i);
    END;

    IF (l1 <> l2) OR (i <> l1) THEN
        differenceFound := TRUE;
        posOfDifference := i;
    END;
END FindDiffI;

PROCEDURE NullTerminate(VAR str : ARRAY OF CHAR);
BEGIN
    str[HIGH(str)] := '';
END NullTerminate;

PROCEDURE Lowercase(VAR INOUT stringVar : ARRAY OF CHAR);
%IF %NOT Windows %THEN
VAR
    i   : CARDINAL;
    len : CARDINAL;
%END
BEGIN
    IF stringVar[0] <> '' THEN
        %IF Windows %THEN
            FUNC CharLowerBuff(stringVar, LENGTH(stringVar));
        %ELSE
            len := LENGTH(stringVar);
            FOR i := 0 TO len-1 DO
                IF (stringVar[i] >= 'A') AND (stringVar[i] <= 'Z') THEN
                    stringVar[i] := CHR(ORD(stringVar[i])+ORD('a')-ORD('A'));
                END;
            END;
        %END
    END;
END Lowercase;

%IF Windows %THEN

PROCEDURE AnsiToUnicode(strA : ARRAY OF ACHAR; VAR OUT strU : ARRAY OF UCHAR);
VAR
    l   : CARDINAL;
BEGIN
    l := WINNLS.MultiByteToWideChar(WINNLS.CP_ACP,
                                  0,
                                  strA, LENGTH(strA),
                                  strU, HIGH(strU)+1);
    IF l <= HIGH(strU) THEN
        strU[l] := '';
    END;
END AnsiToUnicode;

PROCEDURE UnicodeToAnsi(strU : ARRAY OF UCHAR;
                        VAR OUT strA : ARRAY OF ACHAR;
                        replaceChar : ACHAR);
VAR
    l           : CARDINAL;
    replaced    : BOOL;
BEGIN
    l := WINNLS.WideCharToMultiByte(WINNLS.CP_ACP,
                                  0,
                                  strU, LENGTH(strU),
                                  strA, HIGH(strA)+1,
                                  replaceChar,
                                  replaced);
    IF l <= HIGH(strA) THEN
        strA[l] := '';
    END;
END UnicodeToAnsi;

%ELSIF UNIX %THEN

PROCEDURE AnsiToUnicode(strA : ARRAY OF ACHAR; VAR OUT strU : ARRAY OF UCHAR);
VAR
    l           : CARDINAL;
    mblen       : CARDINAL;
BEGIN
    l := LENGTH(strA);
    mblen := mbstowcs(strU, strA, l);
    IF mblen <> l THEN
        l := 0;
    END;
    IF l <= HIGH(strU) THEN
        strU[l] := '';
    END;
END AnsiToUnicode;

PROCEDURE UnicodeToAnsi(strU : ARRAY OF UCHAR;
                        VAR OUT strA : ARRAY OF ACHAR;
                        replaceChar : ACHAR);
VAR
    i, l        : CARDINAL;
    highStrA    : CARDINAL;
    b           : INTEGER;
BEGIN
    l := LENGTH(strU);
    i := 0;
    WHILE i < l DO
        b := wctob(INT(strU[i]));
        IF b = EOF THEN
            b := ORD(replaceChar);
        END;
        strA[i] := ACHR(b);
        INC(i);
    END;
    highStrA := HIGH(strA);
    IF i <= highStrA THEN
        strA[l] := '';
    END;
END UnicodeToAnsi;

%ELSE

(* these are used for operating systems without support
   for Unicode.
   we just do a dumb conversion.
*)

PROCEDURE AnsiToUnicode(strA : ARRAY OF ACHAR; VAR OUT strU : ARRAY OF UCHAR);
VAR
    i, l        : CARDINAL;
BEGIN
    l := LENGTH(strA);
    i := 0;
    WHILE i < l DO
        strU[i] := UCHR(ORD(strA[i]));
        INC(i);
    END;
END AnsiToUnicode;

PROCEDURE UnicodeToAnsi(strU : ARRAY OF UCHAR;
                        VAR OUT strA : ARRAY OF ACHAR;
                        replaceChar : ACHAR);
VAR
    i, l        : CARDINAL;
BEGIN
    UNREFERENCED_PARAMETER(replaceChar);

    l := LENGTH(strU);
    i := 0;
    WHILE i < l DO
        strA[i] := ACHR(ORD(strU[i]));
        INC(i);
    END;
END UnicodeToAnsi;

%END

PROCEDURE Utf8Length(strU : ARRAY OF UCHAR) : CARDINAL;
VAR
    wc          : CARDINAL;
    i, j, l     : CARDINAL;
BEGIN
    l := LENGTH(strU);
    j := 0;
    i := 0;
    WHILE i < l DO
        wc := ORD(strU[i]);
        INC(i);

        IF wc <= 7Fh THEN
            INC(j);
        ELSIF wc <= 7FFh THEN
            j := j + 2;
        ELSE
            j := j + 3;
        END;
    END;

    RETURN j;
END Utf8Length;

PROCEDURE LengthUtf8(strA : ARRAY OF ACHAR) : CARDINAL;
VAR
    ch          : CARDINAL;
    i, j        : CARDINAL;
    highStrA    : CARDINAL;
BEGIN
    j := 0;
    i := 0;
    highStrA := HIGH(strA);
    LOOP
        IF (i <= highStrA) AND (strA[i] <> '') THEN
            ch := ORD(strA[i]);
            INC(i);
            INC(j);

            IF (ch BAND 80h) <> 0 THEN
                (* multi byte sequence *)

                IF (ch BAND 0E0h) = 0C0h THEN
                    (* two bytes *)
                    i := i + 1;
                ELSIF (ch BAND 0F0h) = 0E0h THEN
                    (* three bytes *)
                    i := i + 2;
                ELSIF (ch BAND 0F8h) = 0F0h THEN
                    (* four bytes *)
                    i := i + 3;
                ELSIF (ch BAND 0FCh) = 0F8h THEN
                    (* five bytes *)
                    i := i + 4;
                ELSE
                    (* six bytes *)
                    i := i + 5;
                END;
            END;
        ELSE
            EXIT;
        END;
    END;
    RETURN j;
END LengthUtf8;

PROCEDURE IsValidUtf8(strA : ARRAY OF ACHAR) : BOOLEAN;
VAR
    ch1, ch2    : CARDINAL;
    i, l        : CARDINAL;
BEGIN
    l := LENGTH(strA);
    i := 0;
    WHILE i < l DO
        ch1 := ORD(strA[i]);
        INC(i);

        IF (ch1 BAND 80h) <> 0 THEN
            (* multi byte sequence *)

            IF (ch1 BAND 0E0h) = 0C0h THEN
                (* two bytes *)

                IF i < l THEN
                    IF (ch1 BAND 0FEh) = 0C0h THEN
                        (* overlong *)
                        RETURN FALSE;
                    END;
                ELSE
                    (* truncated*)
                    RETURN FALSE;
                END;

            ELSIF (ch1 BAND 0F0h) = 0E0h THEN
                (* three bytes *)

                IF i+1 < l THEN
                    ch2 := ORD(strA[i]);
                    i := i + 2;

                    IF (ch1 = 0E0h) AND ((ch2 BAND 0E0h = 80h)) THEN
                        (* overlong *)
                        RETURN FALSE;
                    END;
                ELSE
                    (* truncated*)
                    RETURN FALSE;
                END;

            ELSIF (ch1 BAND 0F8h) = 0F0h THEN
                (* four bytes *)

                IF i+2 < l THEN
                    ch2 := ORD(strA[i]);
                    i := i + 3;

                    IF (ch1 = 0F0h) AND ((ch2 BAND 0F0h = 80h)) THEN
                        (* overlong *)
                        RETURN FALSE;
                    END;
                ELSE
                    (* truncated*)
                    RETURN FALSE;
                END;

            ELSIF (ch1 BAND 0FCh) = 0F8h THEN
                (* five bytes *)

                IF i+3 < l THEN
                    ch2 := ORD(strA[i]);
                    i := i + 4;

                    IF (ch1 = 0F8h) AND ((ch2 BAND 0F8h = 80h)) THEN
                        (* overlong *)
                        RETURN FALSE;
                    END;
                ELSE
                    (* truncated*)
                    RETURN FALSE;
                END;
            ELSE
                (* six bytes *)

                IF i+4 < l THEN
                    ch2 := ORD(strA[i]);
                    i := i + 5;

                    IF (ch1 = 0FCh) AND ((ch2 BAND 0FCh = 80h)) THEN
                        (* overlong *)
                        RETURN FALSE;
                    END;
                ELSE
                    (* truncated*)
                    RETURN FALSE;
                END;
            END;
        END;
    END;

    RETURN TRUE;
END IsValidUtf8;

PROCEDURE UnicodeToUtf8(strU : ARRAY OF UCHAR; VAR OUT strA : ARRAY OF ACHAR);
VAR
    wc          : CARDINAL;
    i, j, l     : CARDINAL;
    highStrA    : CARDINAL;
BEGIN
    l := LENGTH(strU);
    j := 0;
    i := 0;
    highStrA := HIGH(strA);
    WHILE i < l DO
        wc := ORD(strU[i]);
        INC(i);

        IF wc <= 7Fh THEN
            IF j <= highStrA THEN
                strA[j] := VAL(ACHAR, wc);
                INC(j);
            ELSE
                i := l;
            END;

        ELSIF wc <= 7FFh THEN
            IF j+1 <= highStrA THEN
                strA[j]   := VAL(ACHAR, 0C0h BOR ((wc SHR 6) BAND 1Fh));
                strA[j+1] := VAL(ACHAR, 080h BOR ( wc        BAND 3Fh));
                j := j + 2;
            ELSE
                i := l;
            END;

        ELSE
            IF j+2 <= highStrA THEN
                strA[j]   := VAL(ACHAR, 0E0h BOR ((wc SHR 12) BAND 0Fh));
                strA[j+1] := VAL(ACHAR, 080h BOR ((wc SHR 6)  BAND 3Fh));
                strA[j+2] := VAL(ACHAR, 080h BOR ( wc         BAND 3Fh));
                j := j + 3;
            ELSE
                i := l;
            END;
        END;
    END;

    IF j <= highStrA THEN
        strA[j] := '';
    END;
END UnicodeToUtf8;

PROCEDURE Utf8ToUnicode(strA : ARRAY OF ACHAR;
                        VAR OUT strU : ARRAY OF UCHAR;
                        replaceChar : UCHAR);
VAR
    ch1,
    ch2,
    ch3         : CARDINAL;
    uch         : CARDINAL;
    i, j, l     : CARDINAL;
    highStrU    : CARDINAL;
    bad         : BOOLEAN;
BEGIN
    l := LENGTH(strA);
    bad := FALSE;
    j := 0;
    i := 0;
    highStrU := HIGH(strU);
    WHILE (i < l) AND (j <= highStrU) AND NOT bad DO
        ch1 := ORD(strA[i]);
        INC(i);

        IF (ch1 BAND 80h) = 0 THEN
            strU[j] := UCHR(ch1);
            INC(j);
        ELSE
            (* multi byte sequence *)

            IF (ch1 BAND 0E0h) = 0C0h THEN
                (* two bytes *)

                IF i < l THEN
                    ch2 := ORD(strA[i]);
                    INC(i);
                    uch := ((ch1 BAND 1Fh) SHL 6) BOR (ch2 BAND 3Fh);
                    strU[j] := UCHR(uch);
                ELSE
                    bad := TRUE;
                END;

            ELSIF (ch1 BAND 0F0h) = 0E0h THEN
                (* three bytes *)

                IF i+1 < l THEN
                    ch2 := ORD(strA[i]);
                    ch3 := ORD(strA[i+1]);
                    i := i + 2;

                    uch := ((ch1 BAND 0Fh) SHL 12) BOR
                           ((ch2 BAND 3Fh) SHL 6)  BOR
                           ( ch3 BAND 3Fh);
                    strU[j] := UCHR(uch);
                ELSE
                    bad := TRUE;
                END;

            ELSE
                (* character is too large for 2-byte unicode *)

                IF (ch1 BAND 0F8h) = 0F0h THEN
                    (* four bytes *)
                    i := i + 3;
                ELSIF (ch1 BAND 0FCh) = 0F8h THEN
                    (* five bytes *)
                    i := i + 4;
                ELSE
                    (* six bytes *)
                    i := i + 5;
                END;

                strU[j] := replaceChar;
                INC(j);
            END;
        END;
    END;

    IF j <= highStrU THEN
        strU[j] := '';
    END;
END Utf8ToUnicode;

<*/PUSH/ALIAS:P*>
PROCEDURE Utf8ToAnsi(utf8 : ARRAY OF ACHAR;
                     VAR OUT strA : ARRAY OF ACHAR;
                     replaceChar : ACHAR);
VAR
    ch1, ch2    : CARDINAL;
    uch         : CARDINAL;
    i, j, l     : CARDINAL;
    highStrA    : CARDINAL;
    bad         : BOOLEAN;
BEGIN
    l := LENGTH(utf8);
    bad := FALSE;
    j := 0;
    i := 0;
    highStrA := HIGH(strA);
    WHILE (i < l) AND (j <= highStrA) AND NOT bad DO
        ch1 := ORD(utf8[i]);
        INC(i);

        IF (ch1 BAND 80h) = 0 THEN
            (* <= 7Fh *)
            strA[j] := ACHR(ch1);
            INC(j);
        ELSE
            (* multi byte sequence *)

            IF (ch1 BAND 0E0h) = 0C0h THEN
                (* two bytes, we will accept a full 8-bit char *)

                IF i < l THEN
                    ch2 := ORD(utf8[i]);
                    INC(i);
                    uch := ((ch1 BAND 1Fh) SHL 6) BOR (ch2 BAND 3Fh);
                    IF uch > 255 THEN
                        uch := ORD(replaceChar);
                    END;
                    strA[j] := ACHR(uch);
                    INC(j);
                ELSE
                    bad := TRUE;
                END;

            ELSE
                IF (ch1 BAND 0F0h) = 0E0h THEN
                    (* three bytes *)
                    i := i + 2;
                ELSIF (ch1 BAND 0F8h) = 0F0h THEN
                    (* four bytes *)
                    i := i + 3;
                ELSIF (ch1 BAND 0FCh) = 0F8h THEN
                    (* five bytes *)
                    i := i + 4;
                ELSE
                    (* six bytes *)
                    i := i + 5;
                END;

                strA[j] := replaceChar;
                INC(j);
            END;
        END;
    END;

    IF j <= highStrA THEN
        strA[j] := '';
    END;
END Utf8ToAnsi;
<*/POP*>

PROCEDURE FindAndReplace(find, replace : ARRAY OF CHAR;
                         VAR INOUT str : ARRAY OF CHAR) : BOOLEAN;
VAR
    pos         : CARDINAL;
    found       : BOOLEAN;
BEGIN
    Strings.FindNext(find, str, 0, found, pos);
    IF found THEN
        Strings.Delete(str, pos, LENGTH(find));
        Strings.Insert(replace, pos, str);
        RETURN TRUE;
    END;
    RETURN FALSE;
END FindAndReplace;

PROCEDURE FindAndReplaceI(find, replace : ARRAY OF CHAR;
                          VAR INOUT str : ARRAY OF CHAR) : BOOLEAN;
VAR
    pos         : CARDINAL;
    found       : BOOLEAN;
BEGIN
    FindNextI(find, str, 0, found, pos);
    IF found THEN
        Strings.Delete(str, pos, LENGTH(find));
        Strings.Insert(replace, pos, str);
        RETURN TRUE;
    END;
    RETURN FALSE;
END FindAndReplaceI;

PROCEDURE AppendWithLengths(source : ARRAY OF CHAR;
                            srcLen : CARDINAL;
                            VAR INOUT destination : ARRAY OF CHAR;
                            destLen : CARDINAL) : CARDINAL;
VAR
    newLen      : CARDINAL;
    c           : CARDINAL;
    ps, pd      : POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
    newLen := srcLen + destLen;
    IF (srcLen > 0) AND (destLen <= HIGH(destination)) THEN
        IF newLen > HIGH(destination)+1 THEN
            newLen := HIGH(destination)+1;
        END;
        c := newLen - destLen;
        IF c <= Threshold THEN
            ps := ADR(source);
            pd := ADR(destination[destLen]);
            LOOP
                IF c >= 4 THEN
                    c := c - 4;
                    pd^[0] := ps^[0];
                    pd^[1] := ps^[1];
                    pd^[2] := ps^[2];
                    pd^[3] := ps^[3];
                    ps := ADDADR(ps, 4*SIZE(CHAR));
                    pd := ADDADR(pd, 4*SIZE(CHAR));
                ELSIF c > 0 THEN
                    DEC(c);
                    pd^[0] := ps^[0];
                    ps := ADDADR(ps, 1*SIZE(CHAR));
                    pd := ADDADR(pd, 1*SIZE(CHAR));
                ELSE
                    EXIT;
                END;
            END;
        ELSE
            destination[destLen..newLen-1] := source;
        END;
        IF newLen <= HIGH(destination) THEN
            destination[newLen] := '';
        END;
        RETURN newLen;
    END;
    RETURN destLen;
END AppendWithLengths;

PROCEDURE AppendChar(ch : CHAR; VAR INOUT str : ARRAY OF CHAR);
VAR
    len         : CARDINAL;
    highStr     : CARDINAL;
BEGIN
    len := LENGTH(str);
    highStr := HIGH(str);
    IF len <= highStr THEN
        str[len] := ch;
        INC(len);
        IF len <= highStr THEN
            str[len] := '';
        END;
    END;
END AppendChar;

PROCEDURE AppendCharCond(ch : CHAR; VAR INOUT str : ARRAY OF CHAR);
VAR
    len         : CARDINAL;
    highStr     : CARDINAL;
BEGIN
    len := LENGTH(str);
    highStr := HIGH(str);
    IF (len <= highStr) AND
       (len <> 0) AND
       (str[len-1] <> ch)
    THEN
        str[len] := ch;
        INC(len);
        IF len <= highStr THEN
            str[len] := '';
        END;
    END;
END AppendCharCond;

PROCEDURE AppendNum (num : CARDINAL64; VAR INOUT str : ARRAY OF CHAR);
VAR
    i           : CARDINAL;
    len         : CARDINAL;
    highStr     : CARDINAL;
    buf         : ARRAY [0..23] OF CHAR;
BEGIN
    i := 0;
    REPEAT
        buf[i] := Digits[num REM 10];
        num := num / 10;
        INC(i);
    UNTIL num = 0;

    len := LENGTH(str);

    highStr := HIGH(str);
    REPEAT
        DEC(i);
        IF len <= highStr THEN
            str[len] := buf[i];
            INC(len);
            IF len <= highStr THEN
                str[len] := '';
            END;
        END;
    UNTIL i = 0;
END AppendNum;

PROCEDURE AppendHex (num : CARDINAL64; digits : CARDINAL; VAR INOUT str : ARRAY OF CHAR);
VAR
    i           : CARDINAL;
    len         : CARDINAL;
    highStr     : CARDINAL;
    buf         : ARRAY [0..23] OF CHAR;
BEGIN
    i := 0;
    REPEAT
        buf[i] := Digits[num REM 16];
        num := num / 16;
        INC(i);
    UNTIL num = 0;

    WHILE (i < digits) AND (i <= HIGH(buf)) DO
        buf[i] := '0';
        INC(i);
    END;

    len := LENGTH(str);
    IF digits = 0 THEN
        digits := 1;
    END;

    highStr := HIGH(str);
    REPEAT
        DEC(digits);
        DEC(i);
        IF len <= highStr THEN
            str[len] := buf[i];
            INC(len);
            IF len <= highStr THEN
                str[len] := '';
            END;
        END;
    UNTIL (i = 0) OR (digits = 0);
END AppendHex;

PROCEDURE GetNextItem(list : ARRAY OF CHAR;
                      VAR INOUT iInOut : CARDINAL;
                      VAR OUT item : ARRAY OF CHAR;
                      sep : ARRAY OF CHAR) : BOOLEAN;
VAR
    s, e        : CARDINAL;
    j           : CARDINAL;
    i           : CARDINAL;
    highList    : CARDINAL;
    highItem    : CARDINAL;
    sepLen      : CARDINAL;

    PROCEDURE inSep(ch : CHAR) : BOOLEAN;
    VAR
        i       : CARDINAL;
    BEGIN
        i := 0;
        LOOP
            IF i < sepLen THEN
                IF ch <> sep[i] THEN
                    INC(i);
                ELSE
                    RETURN TRUE;
                END;
            ELSE
                EXIT;
            END;
        END;
        RETURN FALSE;
    END inSep;

BEGIN
    sepLen := LENGTH(sep);
    IF sepLen = 0 THEN
        RETURN FALSE;
    END;

(*
    WHILE (i <= HIGH(list)) AND
          (list[i] <> '') AND
          (inSep(list[i]))
    DO
        INC(i);
    END;
    *)

    item := "";
    i := iInOut;
    s := i;
    e := i;
    highList := HIGH(list);
    LOOP
        IF (i <= highList) AND (list[i] <> '') THEN
            IF NOT inSep(list[i]) THEN
                INC(i);
            ELSE
                e := i;
                INC(i);(*eat the separator*)
                EXIT;
            END;
        ELSE
            e := i;
            EXIT;
        END;
    END;

    highItem := HIGH(item);
    IF s < e THEN
        item := list[s..e-1];
        IF e-s <= highItem THEN
            j := e-s;
            IF j <= highItem THEN
                item[j] := '';
            END;
        END;
        iInOut := i;
        RETURN TRUE;
    END;

    (* possibly empty item *)
    iInOut := i;
    RETURN (i <= highList) AND (list[i] <> '');
END GetNextItem;

PROCEDURE InList(item, list : ARRAY OF CHAR; sep : ARRAY OF CHAR) : BOOLEAN;
VAR
    i   : CARDINAL;
    s   : ARRAY [0..255] OF CHAR;
BEGIN
    i := 0;
    WHILE GetNextItem(list, i, s, sep) DO
        IF EqualI(s, item) THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END InList;

PROCEDURE AddItem(item : ARRAY OF CHAR;
                  VAR INOUT list : ARRAY OF CHAR;
                  ch : CHAR);
BEGIN
    Strings.Insert(ch, 0, list);
    Strings.Insert(item, 0, list);
END AddItem;

PROCEDURE RemoveItem(item : ARRAY OF CHAR;
                     VAR INOUT list : ARRAY OF CHAR;
                     sep : ARRAY OF CHAR);
VAR
    i, j        : CARDINAL;
    s           : ARRAY [0..255] OF CHAR;
BEGIN
    j := 0;
    i := 0;
    WHILE GetNextItem(list, j, s, sep) DO
        IF EqualI(s, item) THEN
            Strings.Delete(list, i, j-i);
            RETURN;
        END;
        i := j;
    END;
END RemoveItem;

END ExStrings.
