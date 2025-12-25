IMPLEMENTATION MODULE Strings;
<*/OPT:T*>

(* =========================================
            Definition Module from
                  ISO Modula-2
Draft Standard CD10515 by JTC1/SC22/WG13
    Language and Module designs © 1992 by
BSI, D.J. Andrews, B.J. Cornelius, R. B. Henry
R. Sutcliffe, D.P. Ward, and M. Woodman

    Stony Brook Compiler port Implementation
        copyright (c) 1994-2004
        by ADW Software
=========================================== *)

FROM SYSTEM IMPORT
    ADRCARD, ADR, ADDADR, SUBADR;

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

%IF IA32 %THEN

PROCEDURE Length(stringVal : ARRAY OF CHAR) : CARDINAL;
VAR
    i                   : ADRCARD;
    highStringVal       : ADRCARD;
BEGIN
    i := 0;
    highStringVal := HIGH(stringVal);
    LOOP
        IF i+3 <= highStringVal THEN
            IF stringVal[i] <> '' THEN
                IF stringVal[i+1] <> '' THEN
                    IF stringVal[i+2] <> '' THEN
                        IF stringVal[i+3] <> '' THEN
                            i := i + 4;
                            IF i > highStringVal THEN
                                RETURN i;
                            END;
                        ELSE
                            RETURN i + 3;
                        END;
                    ELSE
                        RETURN i + 2;
                    END;
                ELSE
                    RETURN i + 1;
                END;
            ELSE
                RETURN i;
            END;
        ELSIF i+1 <= highStringVal THEN
            IF stringVal[i] <> '' THEN
                IF stringVal[i+1] <> '' THEN
                    i := i + 2;
                    IF i > highStringVal THEN
                        RETURN i;
                    END;
                ELSE
                    RETURN i + 1;
                END;
            ELSE
                RETURN i;
            END;
        ELSIF stringVal[i] <> '' THEN
            RETURN i + 1;
        ELSE
            RETURN i;
        END;
    END;

    RETURN i;
END Length;

%ELSE

PROCEDURE Length(stringVal : ARRAY OF CHAR) : CARDINAL;
VAR
    i           : CARDINAL;
    ptrC        : POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
    ptrC := ADR(stringVal);
    i := 0;
    LOOP
        IF i+3 <= HIGH(stringVal) THEN
            IF ptrC^[0] <> '' THEN
                IF ptrC^[1] <> '' THEN
                    IF ptrC^[2] <> '' THEN
                        IF ptrC^[3] <> '' THEN
                            i := i + 4;
                            ptrC := ADDADR(ptrC, 4*SIZE(CHAR));
                            IF i > HIGH(stringVal) THEN
                                RETURN i;
                            END;
                        ELSE
                            RETURN i + 3;
                        END;
                    ELSE
                        RETURN i + 2;
                    END;
                ELSE
                    RETURN i + 1;
                END;
            ELSE
                RETURN i;
            END;

        ELSIF i+1 <= HIGH(stringVal) THEN
            IF ptrC^[0] <> '' THEN
                IF ptrC^[1] <> '' THEN
                    i := i + 2;
                    ptrC := ADDADR(ptrC, 2*SIZE(CHAR));
                    IF i > HIGH(stringVal) THEN
                        RETURN i;
                    END;
                ELSE
                    RETURN i + 1;
                END;
            ELSE
                RETURN i;
            END;

        ELSIF ptrC^[0] <> '' THEN
            RETURN i + 1;
        ELSE
            RETURN i;
        END;
    END;
    RETURN i;
END Length;

%END

PROCEDURE Assign(source : ARRAY OF CHAR; VAR OUT destination : ARRAY OF CHAR);
VAR
    i           : CARDINAL;
    hs, hd      : CARDINAL;
    pd, ps      : POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
    hs := LENGTH(source);
    IF hs > 0 THEN
        DEC(hs);
    END;
    hd := HIGH(destination);
    IF hs < hd THEN
        hd := hs;
    END;

    IF hd <= Threshold THEN
        pd := ADR(destination);
        ps := ADR(source);
        i := 0;
        LOOP
            IF i+3 <= hd THEN
                i := i + 4;
                pd^[0] := ps^[0];
                pd^[1] := ps^[1];
                pd^[2] := ps^[2];
                pd^[3] := ps^[3];
                pd := ADDADR(pd, 4*SIZE(CHAR));
                ps := ADDADR(ps, 4*SIZE(CHAR));
                IF i > hd THEN
                    EXIT;
                END;
            ELSIF i+1 <= hd THEN
                i := i + 2;
                pd^[0] := ps^[0];
                pd^[1] := ps^[1];
                pd := ADDADR(pd, 2*SIZE(CHAR));
                ps := ADDADR(ps, 2*SIZE(CHAR));
                IF i > hd THEN
                    EXIT;
                END;
            ELSE
                pd^[0] := ps^[0];
                EXIT;
            END;
        END;
    ELSE
        destination[0..hd] := source[0..hd];
    END;

    IF hd+1 <= HIGH(destination) THEN
        destination[hd+1] := '';
    END;
END Assign;

<*/PUSH/ALIAS:P*>
PROCEDURE Extract(source : ARRAY OF CHAR;
                  startIndex, numberToExtract : CARDINAL;
                  VAR OUT destination : ARRAY OF CHAR);
VAR
    i, l        : CARDINAL;
    pd, ps      : POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
    l := LENGTH(source);
    IF startIndex > l THEN
        startIndex := l;
    END;
    IF numberToExtract > l-startIndex THEN
        numberToExtract := l-startIndex;
    END;
    IF numberToExtract > HIGH(destination)+1 THEN
        numberToExtract := HIGH(destination)+1;
    END;

    i := 0;
    IF numberToExtract > 0 THEN
        IF numberToExtract <= Threshold THEN
            pd := ADR(destination);
            ps := ADR(source[startIndex]);
            LOOP
                IF i+3 < numberToExtract THEN
                    i := i + 4;
                    pd^[0] := ps^[0];
                    pd^[1] := ps^[1];
                    pd^[2] := ps^[2];
                    pd^[3] := ps^[3];
                    pd := ADDADR(pd, 4*SIZE(CHAR));
                    ps := ADDADR(ps, 4*SIZE(CHAR));
                    IF i = numberToExtract THEN
                        EXIT;
                    END;
                ELSIF i+1 < numberToExtract THEN
                    i := i + 2;
                    pd^[0] := ps^[0];
                    pd^[1] := ps^[1];
                    pd := ADDADR(pd, 2*SIZE(CHAR));
                    ps := ADDADR(ps, 2*SIZE(CHAR));
                    IF i = numberToExtract THEN
                        EXIT;
                    END;
                ELSE
                    INC(i);
                    pd^[0] := ps^[0];
                    EXIT;
                END;
            END;
        ELSE
            destination[0..numberToExtract-1] :=
                    source[startIndex..startIndex+numberToExtract-1];
            i := i + numberToExtract;
        END;
    END;

    IF i <= HIGH(destination) THEN
        destination[i] := '';
    END;
END Extract;

<*/POP*>

PROCEDURE Delete(VAR INOUT stringVar : ARRAY OF CHAR;
                 startIndex, numberToDelete: CARDINAL);
VAR
    i, j, k, l  : CARDINAL;
    %IF %NOT IA32 %THEN
    pi, pj      : POINTER TO ARRAY [0..3] OF CHAR;
    %END
BEGIN
    l := LENGTH(stringVar);
    i := startIndex;
    IF (numberToDelete > 0) AND (i < l) THEN
        j := i + numberToDelete;
        IF j > l THEN
            j := l;
        END;

        IF l-j <= Threshold THEN
            %IF IA32 %THEN
                LOOP
                    IF j+3 < l THEN
                        stringVar[i] := stringVar[j];
                        stringVar[i+1] := stringVar[j+1];
                        stringVar[i+2] := stringVar[j+2];
                        stringVar[i+3] := stringVar[j+3];
                        j := j + 4;
                        i := i + 4;
                        IF j = l THEN
                            EXIT;
                        END;
                    ELSIF j+1 < l THEN
                        stringVar[i] := stringVar[j];
                        stringVar[i+1] := stringVar[j+1];
                        j := j + 2;
                        i := i + 2;
                        IF j = l THEN
                            EXIT;
                        END;
                    ELSIF j < l THEN
                        stringVar[i] := stringVar[j];
                        INC(i);
                        INC(j);
                        EXIT;
                    ELSE
                        EXIT;
                    END;
                END;
            %ELSE
                pi := ADR(stringVar[i]);
                pj := ADR(stringVar[j]);
                LOOP
                    IF j+3 < l THEN
                        j := j + 4;
                        i := i + 4;
                        pi^[0] := pj^[0];
                        pi^[1] := pj^[1];
                        pi^[2] := pj^[2];
                        pi^[3] := pj^[3];
                        pi := ADDADR(pi, 4*SIZE(CHAR));
                        pj := ADDADR(pj, 4*SIZE(CHAR));
                        IF j = l THEN
                            EXIT;
                        END;
                    ELSIF j+1 < l THEN
                        j := j + 2;
                        i := i + 2;
                        pi^[0] := pj^[0];
                        pi^[1] := pj^[1];
                        pi := ADDADR(pi, 2*SIZE(CHAR));
                        pj := ADDADR(pj, 2*SIZE(CHAR));
                        IF j = l THEN
                            EXIT;
                        END;
                    ELSIF j < l THEN
                        INC(i);
                        pi^[0] := pj^[0];
                        EXIT;
                    ELSE
                        EXIT;
                    END;
                END;
            %END
            IF i <= HIGH(stringVar) THEN
                stringVar[i] := '';
            END;
        ELSE
            k := l - j;
            stringVar[i..i+k-1] := stringVar[j..l-1];
            IF i+k <= HIGH(stringVar) THEN
                stringVar[i+k] := '';
            END;
        END;
    END;
END Delete;

<*/PUSH/ALIAS:P*>
PROCEDURE Insert(source : ARRAY OF CHAR;
                 startIndex : CARDINAL;
                 VAR INOUT destination : ARRAY OF CHAR);

VAR
    h, c        : CARDINAL;
    srcLen      : CARDINAL;
    destLen     : CARDINAL;
    tooMuch     : BOOLEAN;
    ps, pd      : POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
    srcLen := LENGTH(source);

    IF srcLen > 0 THEN

        destLen := LENGTH(destination);

        IF startIndex > destLen THEN
            startIndex := destLen;
        END;

        IF startIndex > HIGH(destination) THEN
            RETURN;
        END;

        IF destLen <= HIGH(destination) THEN
            (* Include the zero terminator in the length *)
            destLen := destLen + 1;
        END;

        (* check for any chars after the insertion to be moved up *)

        IF (destLen > startIndex) AND (startIndex+srcLen <= HIGH(destination)) THEN
            tooMuch := FALSE;
            h := destLen+srcLen-1;
            IF h > HIGH(destination) THEN
                h := HIGH(destination);
                tooMuch := TRUE;
            END;
            c := h - (startIndex+srcLen) + 1;
            IF c <= Threshold THEN
                IF tooMuch OR (destLen > HIGH(destination)) THEN
                    DEC(destLen);
                END;
                ps := ADR(destination[destLen-4]);
                pd := ADR(destination[h-3]);
                LOOP
                    IF c >= 4 THEN
                        c := c - 4;
                        pd^[3] := ps^[3];
                        pd^[2] := ps^[2];
                        pd^[1] := ps^[1];
                        pd^[0] := ps^[0];
                        ps := SUBADR(ps, 4*SIZE(CHAR));
                        pd := SUBADR(pd, 4*SIZE(CHAR));
                        IF c = 0 THEN
                            EXIT;
                        END;
                    ELSIF c >= 2 THEN
                        c := c - 2;
                        pd^[3] := ps^[3];
                        pd^[2] := ps^[2];
                        ps := SUBADR(ps, 2*SIZE(CHAR));
                        pd := SUBADR(pd, 2*SIZE(CHAR));
                        IF c = 0 THEN
                            EXIT;
                        END;
                    ELSIF c > 0 THEN
                        pd^[3] := ps^[3];
                        EXIT;
                    ELSE
                        EXIT;
                    END;
                END;
            ELSE
                destination[startIndex+srcLen..h] := destination[startIndex..destLen-1];
            END;
        END;

        (* insert the source text *)

        h := startIndex+srcLen-1;
        IF h > HIGH(destination) THEN
            h := HIGH(destination);
        END;
        c := h - startIndex + 1;
        IF c <= Threshold THEN
            ps := ADR(source);
            pd := ADR(destination[startIndex]);
            LOOP
                IF c >= 4 THEN
                    c := c - 4;
                    pd^[0] := ps^[0];
                    pd^[1] := ps^[1];
                    pd^[2] := ps^[2];
                    pd^[3] := ps^[3];
                    ps := ADDADR(ps, 4*SIZE(CHAR));
                    pd := ADDADR(pd, 4*SIZE(CHAR));
                    IF c = 0 THEN
                        EXIT;
                    END;
                ELSIF c >= 2 THEN
                    c := c - 2;
                    pd^[0] := ps^[0];
                    pd^[1] := ps^[1];
                    ps := ADDADR(ps, 2*SIZE(CHAR));
                    pd := ADDADR(pd, 2*SIZE(CHAR));
                    IF c = 0 THEN
                        EXIT;
                    END;
                ELSIF c > 0 THEN
                    pd^[0] := ps^[0];
                    EXIT;
                ELSE
                    EXIT;
                END;
            END;
        ELSE
            destination[startIndex..h] := source;
        END;
    END;
END Insert;

<*/POP*>

<*/PUSH/ALIAS:P*>
PROCEDURE Replace(source : ARRAY OF CHAR;
                  startIndex : CARDINAL;
                  VAR INOUT destination : ARRAY OF CHAR);
VAR
    i                   : ADRCARD;
    j                   : ADRCARD;
    l                   : ADRCARD;
    highSource          : ADRCARD;
    highDestination     : ADRCARD;
BEGIN
    l := LENGTH(destination);
    i := startIndex;

    IF i < l THEN
        highSource := HIGH(source);
        j := 0;
        WHILE (j <= highSource) AND (source[j] <> '') AND (i < l) DO
            destination[i] := source[j];
            INC(i);
            INC(j);
        END;

        highDestination := HIGH(destination);
        IF (i >= l) AND (i <= highDestination) THEN
            destination[i] := '';
        END;
    END;
END Replace;

<*/POP*>

PROCEDURE Append(source : ARRAY OF CHAR; VAR INOUT destination : ARRAY OF CHAR);
VAR
    i, j, k, l  : CARDINAL;
    ps, pd      : POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
    l := LENGTH(source);
    i := LENGTH(destination);
    k := HIGH(destination)+1-i;
    IF k < l THEN
        l := k;
    END;

    IF l <= Threshold THEN
        ps := ADR(source);
        pd := ADR(destination[i]);
        LOOP
            IF l >= 4 THEN
                l := l - 4;
                i := i + 4;
                pd^[0] := ps^[0];
                pd^[1] := ps^[1];
                pd^[2] := ps^[2];
                pd^[3] := ps^[3];
                ps := ADDADR(ps, 4*SIZE(CHAR));
                pd := ADDADR(pd, 4*SIZE(CHAR));
                IF l = 0 THEN
                    EXIT;
                END;
            ELSIF l >= 2 THEN
                l := l - 2;
                i := i + 2;
                pd^[0] := ps^[0];
                pd^[1] := ps^[1];
                ps := ADDADR(ps, 2*SIZE(CHAR));
                pd := ADDADR(pd, 2*SIZE(CHAR));
                IF l = 0 THEN
                    EXIT;
                END;
            ELSIF l > 0 THEN
                INC(i);
                pd^[0] := ps^[0];
                EXIT;
            ELSE
                EXIT;
            END;
        END;
        IF i <= HIGH(destination) THEN
            destination[i] := '';
        END;
    ELSE
        j := l + i - 1;
        IF j >= HIGH(destination) THEN
            j := HIGH(destination);
        END;
        destination[i..j] := source;
        IF j+1 <= HIGH(destination) THEN
            destination[j+1] := '';
        END;
    END;
END Append;

<*/PUSH/ALIAS:P*>
PROCEDURE Concat(source1, source2 : ARRAY OF CHAR;
                 VAR OUT destination : ARRAY OF CHAR);
BEGIN
    Assign(source1, destination);
    Append(source2, destination);
END Concat;
<*/POP*>

PROCEDURE CanAssignAll(sourceLength : CARDINAL;
                       VAR OUT destination : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN sourceLength <= (HIGH(destination)+1);
END CanAssignAll;

PROCEDURE CanExtractAll(sourceLength, startIndex, numberToExtract : CARDINAL;
                        VAR OUT destination : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN ((startIndex + numberToExtract) <= sourceLength) AND
           (numberToExtract <= (HIGH(destination)+1));
END CanExtractAll;

PROCEDURE CanDeleteAll(stringLength,
                       startIndex,
                       numberToDelete : CARDINAL) : BOOLEAN;
BEGIN
    RETURN (startIndex + numberToDelete) <= stringLength;
END CanDeleteAll;

PROCEDURE CanInsertAll(sourceLength, startIndex : CARDINAL;
                       VAR INOUT destination : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN (startIndex <= LENGTH(destination)) AND
           ((LENGTH(destination) + sourceLength) <= HIGH(destination));
END CanInsertAll;

PROCEDURE CanReplaceAll(sourceLength, startIndex : CARDINAL;
                        VAR INOUT destination : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN (startIndex + sourceLength) <= LENGTH(destination);
END CanReplaceAll;

PROCEDURE CanAppendAll(sourceLength : CARDINAL;
                       VAR INOUT destination : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN (LENGTH(destination) + sourceLength) <= (HIGH(destination)+1);
END CanAppendAll;

PROCEDURE CanConcatAll(source1Length, source2Length : CARDINAL;
                       VAR OUT destination: ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN (source1Length + source2Length) <= (HIGH(destination)+1);
END CanConcatAll;

PROCEDURE Compare(stringVal1, stringVal2 : ARRAY OF CHAR) : CompareResults;
VAR
    i           : CARDINAL;
    h           : CARDINAL;
    ptr1, ptr2  : POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
    h := HIGH(stringVal2);
    IF HIGH(stringVal1) < h THEN
        h := HIGH(stringVal1);
    END;

    ptr1 := ADR(stringVal1);
    ptr2 := ADR(stringVal2);
    i := 0;
    LOOP
        IF i+3 <= h THEN
            IF (ptr1^[0] <> '') AND (ptr1^[0] = ptr2^[0]) THEN
                IF (ptr1^[1] <> '') AND (ptr1^[1] = ptr2^[1]) THEN
                    IF (ptr1^[2] <> '') AND (ptr1^[2] = ptr2^[2]) THEN
                        IF (ptr1^[3] <> '') AND (ptr1^[3] = ptr2^[3]) THEN
                            i := i + 4;
                            ptr1 := ADDADR(ptr1, 4*SIZE(CHAR));
                            ptr2 := ADDADR(ptr2, 4*SIZE(CHAR));
                        ELSE
                            i := i + 3;
                            EXIT;
                        END;
                    ELSE
                        i := i + 2;
                        EXIT;
                    END;
                ELSE
                    i := i + 1;
                    EXIT;
                END;
            ELSE
                EXIT;
            END;

        ELSIF (i <= h) AND
              (ptr1^[0] <> '') AND
              (ptr1^[0] = ptr2^[0])
        THEN
            INC(i);
            ptr1 := ADDADR(ptr1, 1*SIZE(CHAR));
            ptr2 := ADDADR(ptr2, 1*SIZE(CHAR));
        ELSE
            EXIT;
        END;
    END;

    IF i <= h THEN
        IF stringVal1[i] < stringVal2[i] THEN
            RETURN less;
        ELSIF stringVal1[i] > stringVal2[i] THEN
            RETURN greater;
        END;
        RETURN equal;
    ELSE
        IF HIGH(stringVal1) < HIGH(stringVal2) THEN
            IF stringVal2[i] = '' THEN
                RETURN equal;
            END;
            RETURN less;
        ELSIF HIGH(stringVal1) > HIGH(stringVal2) THEN
            IF stringVal1[i] = '' THEN
                RETURN equal;
            END;
            RETURN greater;
        ELSE
            RETURN equal;
        END;
    END;
END Compare;

PROCEDURE Equal(stringVal1, stringVal2 : ARRAY OF CHAR) : BOOLEAN;
VAR
    i           : CARDINAL;
    h           : CARDINAL;
    ptr1, ptr2  : POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
    h := HIGH(stringVal2);
    IF HIGH(stringVal1) < h THEN
        h := HIGH(stringVal1);
    END;

    ptr1 := ADR(stringVal1);
    ptr2 := ADR(stringVal2);
    i := 0;
    LOOP
        IF i+3 <= h THEN
            IF (ptr1^[0] <> '') AND (ptr1^[0] = ptr2^[0]) THEN
                IF (ptr1^[1] <> '') AND (ptr1^[1] = ptr2^[1]) THEN
                    IF (ptr1^[2] <> '') AND (ptr1^[2] = ptr2^[2]) THEN
                        IF (ptr1^[3] <> '') AND (ptr1^[3] = ptr2^[3]) THEN
                            i := i + 4;
                            ptr1 := ADDADR(ptr1, 4*SIZE(CHAR));
                            ptr2 := ADDADR(ptr2, 4*SIZE(CHAR));
                        ELSE
                            i := i + 3;
                            EXIT;
                        END;
                    ELSE
                        i := i + 2;
                        EXIT;
                    END;
                ELSE
                    i := i + 1;
                    EXIT;
                END;
            ELSE
                EXIT;
            END;

        ELSIF (i <= h) AND
              (ptr1^[0] <> '') AND
              (ptr1^[0] = ptr2^[0])
        THEN
            INC(i);
            ptr1 := ADDADR(ptr1, 1*SIZE(CHAR));
            ptr2 := ADDADR(ptr2, 1*SIZE(CHAR));
        ELSE
            EXIT;
        END;
    END;

    IF i <= h THEN
        RETURN stringVal1[i] = stringVal2[i];
    ELSE
        IF HIGH(stringVal1) < HIGH(stringVal2) THEN
            IF stringVal2[i] = '' THEN
                RETURN TRUE;
            END;
            RETURN FALSE;
        ELSIF HIGH(stringVal1) > HIGH(stringVal2) THEN
            IF stringVal1[i] = '' THEN
                RETURN TRUE;
            END;
            RETURN FALSE;
        END;
        RETURN TRUE;
    END;
END Equal;

PROCEDURE FindNext(pattern, stringToSearch : ARRAY OF CHAR;
                   startIndex : CARDINAL;
                   VAR OUT patternFound : BOOLEAN;
                   VAR OUT posOfPattern : CARDINAL);
VAR
    i           : ADRCARD;
    j           : ADRCARD;
    k           : ADRCARD;
    endP        : ADRCARD;
    l2          : ADRCARD;
    ch          : CHAR;
BEGIN
    patternFound := FALSE;

    endP := LENGTH(pattern);
    IF endP <> 0 THEN
        DEC(endP);

        l2 := LENGTH(stringToSearch);
        i := startIndex;

        IF endP = 0 THEN
            ch := pattern[0];
            WHILE (i < l2) AND (ch <> stringToSearch[i]) DO
                INC(i);
                IF (i < l2) AND (ch <> stringToSearch[i]) THEN
                    INC(i);
                END;
            END;

            IF i < l2 THEN
                patternFound := TRUE;
                posOfPattern := i;
                RETURN;
            END;
        ELSE
            ch := pattern[endP];
            WHILE i + endP < l2 DO
                IF ch = stringToSearch[i+endP] THEN
                    j := 0;
                    k := i;
                    LOOP
                        IF pattern[j] <> stringToSearch[k] THEN
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
END FindNext;

PROCEDURE FindPrev(pattern, stringToSearch : ARRAY OF CHAR;
                   startIndex : CARDINAL;
                   VAR OUT patternFound : BOOLEAN;
                   VAR OUT posOfPattern : CARDINAL);
VAR
    i           : ADRCARD;
    j           : ADRCARD;
    k           : ADRCARD;
    endP        : ADRCARD;
    l2          : ADRCARD;
    ch          : CHAR;
BEGIN
    patternFound := FALSE;

    endP := LENGTH(pattern);
    IF endP <> 0 THEN
        DEC(endP);

        l2 := LENGTH(stringToSearch);
        i := startIndex;

        IF endP = 0 THEN
            ch := pattern[0];
            WHILE (i > 0) AND (ch <> stringToSearch[i]) DO
                DEC(i);
                IF (i > 0) AND (ch <> stringToSearch[i]) THEN
                    DEC(i);
                END;
            END;

            IF (i > 0) OR (ch = stringToSearch[0]) THEN
                patternFound := TRUE;
                posOfPattern := i;
                RETURN;
            END;
        ELSE
            ch := pattern[endP];
            INC(i);
            REPEAT
                DEC(i);
                IF (i + endP < l2) AND (ch = stringToSearch[i+endP]) THEN
                    j := 0;
                    k := i;
                    LOOP
                        IF pattern[j] <> stringToSearch[k] THEN
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
END FindPrev;

PROCEDURE FindDiff(stringVal1, stringVal2 : ARRAY OF CHAR;
                   VAR OUT differenceFound : BOOLEAN;
                   VAR OUT posOfDifference : CARDINAL);
VAR
    i           : CARDINAL;
    l, l1, l2   : CARDINAL;
    ptr1, ptr2  : POINTER TO ARRAY [0..3] OF CHAR;
BEGIN
    differenceFound := FALSE;
    posOfDifference := 0;

    l1 := LENGTH(stringVal1);
    l2 := LENGTH(stringVal2);
    l := l2;
    IF l1 < l2 THEN
        l := l1;
    END;

    ptr1 := ADR(stringVal1);
    ptr2 := ADR(stringVal2);
    i := 0;
    LOOP
        IF i+3 < l THEN
            IF ptr1^[0] = ptr2^[0] THEN
                IF ptr1^[1] = ptr2^[1] THEN
                    IF ptr1^[2] = ptr2^[2] THEN
                        IF ptr1^[3] = ptr2^[3] THEN
                            i := i + 4;
                            ptr1 := ADDADR(ptr1, 4*SIZE(CHAR));
                            ptr2 := ADDADR(ptr2, 4*SIZE(CHAR));
                        ELSE
                            i := i + 3;
                            EXIT;
                        END;
                    ELSE
                        i := i + 2;
                        EXIT;
                    END;
                ELSE
                    i := i + 1;
                    EXIT;
                END;
            ELSE
                EXIT;
            END;

        ELSIF (i < l) AND (ptr1^[0] = ptr2^[0]) THEN
            INC(i);
            ptr1 := ADDADR(ptr1, 1*SIZE(CHAR));
            ptr2 := ADDADR(ptr2, 1*SIZE(CHAR));
        ELSE
            EXIT;
        END;
    END;

    IF (l1 <> l2) OR (i <> l1) THEN
        differenceFound := TRUE;
        posOfDifference := i;
    END;
END FindDiff;

PROCEDURE Capitalize(VAR INOUT stringVar : ARRAY OF CHAR);
VAR
    i   : ADRCARD;
    len : ADRCARD;
BEGIN
    IF stringVar[0] <> 0C THEN
        len := LENGTH(stringVar);
        FOR i := 0 TO len-1 DO
            stringVar[i] := CAP(stringVar[i]);
        END;
    END;
END Capitalize;

END Strings.
