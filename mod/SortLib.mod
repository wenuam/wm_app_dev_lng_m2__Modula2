(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE SortLib;
<*/OPT:TN/INLINE:N*>

FROM SYSTEM IMPORT
    ADDRESS;

PROCEDURE QuickSort(numItems : CARDINAL;
                    lessEq : LessEqProc;
                    swap : SwapProc);
CONST
    cutoffValue = 10;(*small sublist optimization*)

    PROCEDURE quick(left, right : CARDINAL);
    VAR
        j               : CARDINAL;
        k               : CARDINAL;
    BEGIN
        WHILE (left < right) AND ((right-left) > cutoffValue) DO
            (* median of three optimization *)

            swap((left+right) / 2, left+1);

            IF lessEq(right, left+1, FALSE) THEN
                swap(right, left+1);
            END;
            IF lessEq(right, left, FALSE) THEN
                swap(right, left);
            END;
            IF lessEq(left, left+1, FALSE) THEN
                swap(left, left+1);
            END;

            (* split the sequence *)

            j := left+1;
            k := right;

            REPEAT
                REPEAT
                    j := j + 1;
                UNTIL lessEq(left, j, TRUE);
                REPEAT
                    k := k - 1;
                UNTIL lessEq(k, left, TRUE);

                IF j < k THEN
                    swap(j, k);
                END;
            UNTIL j > k;

            swap(left, k);

            (* tail recursion optimization *)

            IF (k-left) < (right-k) THEN
                quick(left, k-1);
                left := k+1;
            ELSE
                quick(k+1, right);
                right := k-1;
            END;
        END;
    END quick;

    PROCEDURE insertSort(upper : CARDINAL);
    VAR
        i       : CARDINAL;
        j       : CARDINAL;
    BEGIN
        FOR i := 1+1 TO upper DO
            j := i;
            WHILE (j > 1) AND lessEq(j, j-1, FALSE) DO
                swap(j, j-1);
                DEC(j);
            END;
        END;
    END insertSort;

BEGIN
    IF numItems > 1 THEN
        quick(1, numItems);
        insertSort(numItems);
    END;
END QuickSort;

PROCEDURE QuickSortEx(numItems : CARDINAL;
                      lessEq : LessEqProcEx;
                      swap : SwapProcEx;
                      userData : ADDRESS);
CONST
    cutoffValue = 10;(*small sublist optimization*)

    PROCEDURE quick(left, right : CARDINAL);
    VAR
        j               : CARDINAL;
        k               : CARDINAL;
    BEGIN
        WHILE (left < right) AND ((right-left) > cutoffValue) DO
            (* median of three optimization *)

            swap((left+right) / 2, left+1, userData);

            IF lessEq(right, left+1, FALSE, userData) THEN
                swap(right, left+1, userData);
            END;
            IF lessEq(right, left, FALSE, userData) THEN
                swap(right, left, userData);
            END;
            IF lessEq(left, left+1, FALSE, userData) THEN
                swap(left, left+1, userData);
            END;

            (* split the sequence *)

            j := left+1;
            k := right;

            REPEAT
                REPEAT
                    j := j + 1;
                UNTIL lessEq(left, j, TRUE, userData);
                REPEAT
                    k := k - 1;
                UNTIL lessEq(k, left, TRUE, userData);

                IF j < k THEN
                    swap(j, k, userData);
                END;
            UNTIL j > k;

            swap(left, k, userData);

            (* tail recursion optimization *)

            IF (k-left) < (right-k) THEN
                quick(left, k-1);
                left := k+1;
            ELSE
                quick(k+1, right);
                right := k-1;
            END;
        END;
    END quick;

    PROCEDURE insertSort(upper : CARDINAL);
    VAR
        i       : CARDINAL;
        j       : CARDINAL;
    BEGIN
        FOR i := 1+1 TO upper DO
            j := i;
            WHILE (j > 1) AND lessEq(j, j-1, FALSE, userData) DO
                swap(j, j-1, userData);
                DEC(j);
            END;
        END;
    END insertSort;

BEGIN
    IF numItems > 1 THEN
        quick(1, numItems);
        insertSort(numItems);
    END;
END QuickSortEx;

PROCEDURE HeapSort(numItems : CARDINAL;
                   lessEq : LessEqProc;
                   swap : SwapProc);

    PROCEDURE restoreHeap(idx, last_element : CARDINAL);
    VAR
        l       : CARDINAL;
        r       : CARDINAL;
        largest : CARDINAL;
    BEGIN
        LOOP
            l := 2*idx;
            IF (l <= last_element) AND lessEq(idx, l, FALSE) THEN
                largest := l;
            ELSE
                largest := idx;
            END;

            r := l+1(*2*idx+1*);
            IF (r <= last_element) AND lessEq(largest, r, FALSE) THEN
                largest := r;
            END;

            IF largest <> idx THEN
                swap(idx, largest);
                idx := largest;
            ELSE
                EXIT;
            END;
        END;
    END restoreHeap;

VAR
    i            : CARDINAL;
BEGIN
    (* Convert to Heap *)

    FOR i := (numItems / 2) TO 1 BY -1 DO
        restoreHeap(i, numItems);
    END;

    (* Sort *)

    FOR i := numItems TO 2 BY -1 DO
        swap(1, i);
        restoreHeap(1, i-1);
    END;
END HeapSort;

PROCEDURE HeapSortEx(numItems : CARDINAL;
                     lessEq : LessEqProcEx;
                     swap : SwapProcEx;
                     userData : ADDRESS);

    PROCEDURE restoreHeap(idx, last_element : CARDINAL);
    VAR
        l       : CARDINAL;
        r       : CARDINAL;
        largest : CARDINAL;
    BEGIN
        LOOP
            l := 2*idx;
            IF (l <= last_element) AND lessEq(idx, l, FALSE, userData) THEN
                largest := l;
            ELSE
                largest := idx;
            END;

            r := l+1(*2*idx+1*);
            IF (r <= last_element) AND lessEq(largest, r, FALSE, userData) THEN
                largest := r;
            END;

            IF largest <> idx THEN
                swap(idx, largest, userData);
                idx := largest;
            ELSE
                EXIT;
            END;
        END;
    END restoreHeap;

VAR
    i            : CARDINAL;
BEGIN
    (* Convert to Heap *)

    FOR i := (numItems / 2) TO 1 BY -1 DO
        restoreHeap(i, numItems);
    END;

    (* Sort *)

    FOR i := numItems TO 2 BY -1 DO
        swap(1, i, userData);
        restoreHeap(1, i-1);
    END;
END HeapSortEx;

PROCEDURE ShellSort(numItems : CARDINAL;
                    lessEq : LessEqProc;
                    assign : AssignProc);
VAR
    i   : CARDINAL;
    j   : INTEGER;
    h   : INTEGER;
BEGIN
    IF numItems > 1 THEN
        h := 1;
        REPEAT
            h := (3*h) + 1;
        UNTIL ORD(h) > numItems;

        REPEAT
            h := h / 3;
            FOR i := ORD(h) TO numItems DO
                assign(0, i);
                j := i;

                LOOP
                    IF lessEq(0, j-h, TRUE) THEN
                        assign(j, j-h);
                        j := j - h;
                        IF j < h THEN
                            EXIT;
                        END;
                    ELSE
                        EXIT;
                    END;
                END;

                IF j <> INT(i) THEN
                    assign(j, 0);
                END;
            END;
        UNTIL h = 1;
    END;
END ShellSort;

PROCEDURE ShellSortEx(numItems : CARDINAL;
                      lessEq : LessEqProcEx;
                      assign : AssignProcEx;
                      userData : ADDRESS);
VAR
    i   : CARDINAL;
    j   : INTEGER;
    h   : INTEGER;
BEGIN
    IF numItems > 1 THEN
        h := 1;
        REPEAT
            h := (3*h) + 1;
        UNTIL ORD(h) > numItems;

        REPEAT
            h := h / 3;
            FOR i := ORD(h) TO numItems DO
                assign(0, i, userData);
                j := i;

                LOOP
                    IF lessEq(0, j-h, TRUE, userData) THEN
                        assign(j, j-h, userData);
                        j := j - h;
                        IF j < h THEN
                            EXIT;
                        END;
                    ELSE
                        EXIT;
                    END;
                END;

                IF j <> INT(i) THEN
                    assign(j, 0, userData);
                END;
            END;
        UNTIL h = 1;
    END;
END ShellSortEx;

PROCEDURE BinaryInsertSort(numItems : CARDINAL;
                           lessEq : LessEqProc;
                           assign : AssignProc);
VAR
    i           : CARDINAL;
    j           : CARDINAL;
    left        : CARDINAL;
    middle      : CARDINAL;
    right       : CARDINAL;
BEGIN
    IF numItems > 1 THEN
        FOR i := 2 TO numItems DO
            left := 1;
            right := i;
            WHILE left < right DO
                middle := (left+right) / 2;
                IF lessEq(middle, i, TRUE) THEN
                    left := middle + 1;
                ELSE
                    right := middle;
                END;
            END;

            assign(0, i);
            FOR j := i TO (right+1) BY -1 DO
                assign(j, j-1);
            END;
            assign(right, 0);
            (* alternate but slower implementation
               does not need index 0 temp
            FOR j := i TO (right+1) BY -1 DO
                swap(j, j-1);
            END;
            *)
        END;
    END;
END BinaryInsertSort;

PROCEDURE BinaryInsertSortEx(numItems : CARDINAL;
                             lessEq : LessEqProcEx;
                             assign : AssignProcEx;
                             userData : ADDRESS);
VAR
    i           : CARDINAL;
    j           : CARDINAL;
    left        : CARDINAL;
    middle      : CARDINAL;
    right       : CARDINAL;
BEGIN
    IF numItems > 1 THEN
        FOR i := 2 TO numItems DO
            left := 1;
            right := i;
            WHILE left < right DO
                middle := (left+right) / 2;
                IF lessEq(middle, i, TRUE, userData) THEN
                    left := middle + 1;
                ELSE
                    right := middle;
                END;
            END;

            assign(0, i, userData);
            FOR j := i TO (right+1) BY -1 DO
                assign(j, j-1, userData);
            END;
            assign(right, 0, userData);
            (* alternate but slower implementation
               does not need index 0 temp
            FOR j := i TO (right+1) BY -1 DO
                swap(j, j-1);
            END;
            *)
        END;
    END;
END BinaryInsertSortEx;

PROCEDURE MergeSort(numItems : CARDINAL;
                    lessEq : LessEqProc;
                    assign : AssignProc);
VAR
    i                   : CARDINAL;
    j                   : CARDINAL;
    k                   : CARDINAL;
    limit               : CARDINAL;
    temp                : CARDINAL;
    h, m, p, q, r       : INTEGER;
    up                  : BOOLEAN;
BEGIN
    up := TRUE;
    p  := 1;
    REPEAT
        h := 1;
        m := numItems;

        IF up THEN
            i := 1;
            j := numItems;
            k := numItems + 1;
            limit := 2 * numItems;
        ELSE
            k := 1;
            limit := numItems;
            i := numItems + 1;
            j := 2 * numItems;
        END;

        REPEAT
            IF m >= p THEN
                q := p;
            ELSE
                q := m;
            END;
            m := m - q;

            IF m >= p THEN
                r := p;
            ELSE
                r := m;
            END;
            m := m - r;

            WHILE (q > 0) AND (r > 0) DO
                IF lessEq(i, j, FALSE) THEN
                    assign(k, i);
                    k := INT(k) + h;
                    INC(i);
                    DEC(q);
                ELSE
                    assign(k, j);
                    k := INT(k) + h;
                    DEC(j);
                    DEC(r);
                END;
            END;

            WHILE r > 0 DO
                assign(k, j);
                k := INT(k) + h;
                DEC(j);
                DEC(r);
            END;

            WHILE q > 0 DO
                assign(k, i);
                k := INT(k) + h;
                INC(i);
                DEC(q);
            END;

            h := -h;
            temp := k;
            k := limit;
            limit := temp;
        UNTIL m = 0;

        up := NOT up;
        p := 2 * p;
    UNTIL ORD(p) >= numItems;

    IF NOT up THEN
        FOR i := 1 TO numItems DO
            assign(i, i+numItems);
        END;
    END;
END MergeSort;

PROCEDURE MergeSortEx(numItems : CARDINAL;
                      lessEq : LessEqProcEx;
                      assign : AssignProcEx;
                      userData : ADDRESS);
VAR
    i                   : CARDINAL;
    j                   : CARDINAL;
    k                   : CARDINAL;
    limit               : CARDINAL;
    temp                : CARDINAL;
    h, m, p, q, r       : INTEGER;
    up                  : BOOLEAN;
BEGIN
    up := TRUE;
    p  := 1;
    REPEAT
        h := 1;
        m := numItems;

        IF up THEN
            i := 1;
            j := numItems;
            k := numItems + 1;
            limit := 2 * numItems;
        ELSE
            k := 1;
            limit := numItems;
            i := numItems + 1;
            j := 2 * numItems;
        END;

        REPEAT
            IF m >= p THEN
                q := p;
            ELSE
                q := m;
            END;
            m := m - q;

            IF m >= p THEN
                r := p;
            ELSE
                r := m;
            END;
            m := m - r;

            WHILE (q > 0) AND (r > 0) DO
                IF lessEq(i, j, FALSE, userData) THEN
                    assign(k, i, userData);
                    k := INT(k) + h;
                    INC(i);
                    DEC(q);
                ELSE
                    assign(k, j, userData);
                    k := INT(k) + h;
                    DEC(j);
                    DEC(r);
                END;
            END;

            WHILE r > 0 DO
                assign(k, j, userData);
                k := INT(k) + h;
                DEC(j);
                DEC(r);
            END;

            WHILE q > 0 DO
                assign(k, i, userData);
                k := INT(k) + h;
                INC(i);
                DEC(q);
            END;

            h := -h;
            temp := k;
            k := limit;
            limit := temp;
        UNTIL m = 0;

        up := NOT up;
        p := 2 * p;
    UNTIL ORD(p) >= numItems;

    IF NOT up THEN
        FOR i := 1 TO numItems DO
            assign(i, i+numItems, userData);
        END;
    END;
END MergeSortEx;

END SortLib.
