(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                           All rights reserved.                          *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE Terminal;
<*/NOWARN:F*>

FROM SYSTEM IMPORT
    ADDRESS, ADRCARD, ADR, CAST, UNREFERENCED_PARAMETER, IsThread;

FROM ASCII IMPORT
    bs, ht, cr, lf;

FROM WINUSER IMPORT
    WNDCLASS, RegisterClass, CreateWindowEx, SetCaretPos,
    TranslateMessage, DispatchMessage, DefWindowProc,
    WS_OVERLAPPEDWINDOW, MSG, MINMAXINFO, SetWindowPos,
    CS_VREDRAW, CS_HREDRAW, CS_BYTEALIGNCLIENT, MessageBox, ShowWindow, GetDC,
    PAINTSTRUCT, BeginPaint, EndPaint, WM_PAINT, UpdateWindow,
    GetMessage, MessageBeep, IsIconic, DestroyWindow,
    ReleaseDC,
    WM_CHAR, WM_KEYDOWN, WM_GETMINMAXINFO, WM_APP,
    ScrollWindow, IntersectRect, IDC_ARROW, LoadCursor,
    WM_SETFOCUS, WM_KILLFOCUS, CreateCaret, ShowCaret,
    DestroyCaret, CW_USEDEFAULT, GetWindowRect, GetClientRect,
    SW_SHOW, PostQuitMessage, WM_DESTROY, SWP_NOMOVE, SWP_NOZORDER,
    SendMessage, HideCaret, LoadIcon, IDI_APPLICATION,
    GetKeyState, VK_CONTROL, VK_MENU, VK_SHIFT,
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT;

FROM WIN32 IMPORT
    HWND, LPARAM, WPARAM, UINT, LRESULT, RECT, HDC;

FROM WINX IMPORT
    NIL_RECT, NULL_HWND, NULL_HMENU, NULL_HINSTANCE,
    NULL_HBITMAP, Instance, GetStockBrush;

FROM WINGDI IMPORT
    GetStockObject, TextOut, WHITE_BRUSH, SelectObject,
    TEXTMETRIC, GetTextMetrics, OEM_FIXED_FONT;

IMPORT Threads;

TYPE
    PMINMAXINFO         = POINTER TO MINMAXINFO;

CONST
    MAX_CHARS   = 64;

    WM_CARETMESSAGE     = WM_APP + 1;
    WM_TERMINATE        = WM_APP + 2;

    SHOW_CARET          = 0;
    HIDE_CARET          = 1;

VAR
    Class       : WNDCLASS;
    MainWindow  : HWND;
    TM          : TEXTMETRIC;
    DC          : HDC;
    CharHeight  : CARDINAL;
    CharWidth   : CARDINAL;
    Leading     : CARDINAL;
    X,Y         : CARDINAL;
    DX, DY      : CARDINAL;
    Screen      : ARRAY [0..24], [0..79] OF CHAR;
    KeyBuf      : ARRAY [0..MAX_CHARS-1] OF CHAR;
    kBegin      : CARDINAL;
    kEnd        : CARDINAL;
    HaveFocus   : BOOLEAN;
    CaretVisible: BOOLEAN;
    client      : RECT;
    wind        : RECT;

    BufferLock  : Threads.CriticalSection;
    WriteLock   : Threads.CriticalSection;
    MessageThread : Threads.Thread;
    CharInBuf   : Threads.SignalSem;

    InitDone    : Threads.EventSem;

    TermInited  : BOOLEAN;

PROCEDURE WaitForChar();
BEGIN
    SendMessage(MainWindow, WM_CARETMESSAGE, SHOW_CARET, 0);
    Threads.WaitForSignalSem(CharInBuf, Threads.SemWaitForever);
    SendMessage(MainWindow, WM_CARETMESSAGE, HIDE_CARET, 0);
END WaitForChar;

PROCEDURE AddChar(ch : CHAR);
BEGIN
    Threads.EnterCriticalSection(BufferLock);

    IF kEnd = kBegin THEN
        MessageBeep(0ffffh);
    ELSE
        Threads.SendSignalSem(CharInBuf, 1);

        KeyBuf[kEnd - 1] := ch;
        INC(kEnd);
        IF kEnd = MAX_CHARS + 1 THEN
            kEnd := 1;
        END;
    END;

    Threads.LeaveCriticalSection(BufferLock);
END AddChar;

PROCEDURE GetChar() : CHAR;
VAR
    ch  : CHAR;
BEGIN
    Threads.EnterCriticalSection(BufferLock);

    IF (kEnd > kBegin) AND (kEnd - 1 = kBegin) THEN
        Threads.LeaveCriticalSection(BufferLock);
        RETURN 0C;
    ELSE
        ch := KeyBuf[kBegin];
        INC(kBegin);
        IF kBegin = MAX_CHARS THEN
            kBegin := 0;
        END;
        Threads.LeaveCriticalSection(BufferLock);
        RETURN ch;
    END;
END GetChar;

(* Write to the window, teletype style *)

PROCEDURE OutPutCh(ch : CHAR);
TYPE
    AOC     = ARRAY [0..0] OF CHAR;
VAR
    DC  : HDC;
    i   : ADRCARD;
BEGIN
    IF MainWindow <> NULL_HWND THEN
        IF ch = cr THEN
            X := 0;
        ELSIF ch = lf THEN
            Y := Y + 1;
            IF Y = 25 THEN
                FOR i := 0 TO 23 DO
                    Screen[i] := Screen[i+1];
                END;
                FOR i := 0 TO 79 DO
                    Screen[24][i] := ' ';
                END;
                ScrollWindow(MainWindow, 0, -INT(CharHeight), NIL_RECT, NIL_RECT);
                UpdateWindow(MainWindow);
                Y := 24;
            END;
        ELSIF ch = bs THEN
            IF X > 0 THEN
                X := X - 1;
            END;
        ELSIF ch = ht THEN
            REPEAT
                X := X + 1;
            UNTIL X REM 8 = 0;
            IF X > 79 THEN
                X := 79;
            END;
        ELSIF X < 80 THEN
            DC := GetDC(MainWindow);
            SelectObject(DC, GetStockObject(OEM_FIXED_FONT));
            TextOut(DC, X*CharWidth, Y*CharHeight+Leading, CAST(AOC, ch), 1);
            ReleaseDC(MainWindow, DC);
            Screen[Y, X] := ch;
            X := X + 1;
        ELSIF X >= 80 THEN
            X := 0;
            Y := Y + 1;
            IF Y = 25 THEN
                FOR i := 0 TO 23 DO
                    Screen[i] := Screen[i+1];
                END;
                FOR i := 0 TO 79 DO
                    Screen[24][i] := ' ';
                END;
                ScrollWindow(MainWindow, 0,
                    -INT(CharHeight),
                    NIL_RECT, NIL_RECT);
                UpdateWindow(MainWindow);
                Y := 24;
            END;
            DC := GetDC(MainWindow);
            SelectObject(DC, GetStockObject(OEM_FIXED_FONT));
            TextOut(DC, X*CharWidth, Y*CharHeight+Leading, CAST(AOC, ch), 1);
            ReleaseDC(MainWindow, DC);
            Screen[Y, X] := ch;
            X := X + 1;
        END;
    END;
END OutPutCh;

PROCEDURE Write(ch : CHAR);
BEGIN
    IF NOT TermInited THEN
        TermInit;
    END;

    Threads.EnterCriticalSection(WriteLock);

    OutPutCh(ch);

    Threads.LeaveCriticalSection(WriteLock);
END Write;

PROCEDURE OutPutString(str : ARRAY OF CHAR);
TYPE
    AOC     = ARRAY [0..0] OF CHAR;
VAR
    DC          : HDC;
    ch          : CHAR;
    i           : ADRCARD;
    highStr     : ADRCARD;
    j           : CARDINAL;
    l           : CARDINAL;
    ctrlChars   : BOOLEAN;
BEGIN
    IF MainWindow = NULL_HWND THEN
        RETURN;
    END;

    l := LENGTH(str);
    IF l = 0 THEN
        RETURN;
    END;

    DC := GetDC(MainWindow);
    SelectObject(DC, GetStockObject(OEM_FIXED_FONT));

    ctrlChars := FALSE;
    i := 0;
    <*/PUSH/NOWARN:U*>
    WHILE i < VAL(ADRCARD, l) DO
    <*/POP*>
        IF (str[i] = cr) OR
           (str[i] = lf) OR
           (str[i] = bs) OR
           (str[i] = ht)
        THEN
            ctrlChars := TRUE;
            i := l;
        END;
        INC(i);
    END;

    IF (NOT ctrlChars) AND (X + l <= 80) THEN
        Screen[Y][X..X+l-1] := str;
        TextOut(DC, X*CharWidth, Y*CharHeight+Leading, str, l);
        X := X + l;
    ELSE
        i := 0;
        highStr := HIGH(str);
        WHILE (i <= highStr) AND (str[i] <> CHR(0)) DO
            ch := str[i];

            IF ch = cr THEN
                X := 0;
            ELSIF ch = lf THEN
                Y := Y + 1;
                IF Y = 25 THEN
                    FOR j := 0 TO 23 DO
                        Screen[j] := Screen[j+1];
                    END;
                    FOR j := 0 TO 79 DO
                        Screen[24][j] := ' ';
                    END;
                    ScrollWindow(MainWindow, 0, -INT(CharHeight), NIL_RECT, NIL_RECT);
                    UpdateWindow(MainWindow);
                    Y := 24;
                END;
            ELSIF ch = bs THEN
                IF X > 0 THEN
                    X := X - 1;
                END;
            ELSIF ch = ht THEN
                REPEAT
                    X := X + 1;
                UNTIL X REM 8 = 0;
                IF X > 79 THEN
                    X := 79;
                END;
            ELSIF X < 80 THEN
                TextOut(DC, X*CharWidth, Y*CharHeight+Leading, CAST(AOC, ch), 1);
                Screen[Y, X] := ch;
                X := X + 1;
            ELSIF X >= 80 THEN
                X := 0;
                Y := Y + 1;
                IF Y = 25 THEN
                    FOR j := 0 TO 23 DO
                        Screen[j] := Screen[j+1];
                    END;
                    FOR j := 0 TO 79 DO
                        Screen[24][j] := ' ';
                    END;
                    ScrollWindow(MainWindow, 0, -INT(CharHeight), NIL_RECT, NIL_RECT);
                    UpdateWindow(MainWindow);
                    Y := 24;
                END;
                TextOut(DC, X*CharWidth, Y*CharHeight+Leading, CAST(AOC, ch), 1);
                Screen[Y, X] := ch;
                X := X + 1;
            END;

            INC(i);
        END;
    END;

    ReleaseDC(MainWindow, DC);
END OutPutString;

PROCEDURE WriteString(s : ARRAY OF CHAR);
BEGIN
    IF NOT TermInited THEN
        TermInit;
    END;

    Threads.EnterCriticalSection(WriteLock);

    (*
    i := 0;
    WHILE (i <= HIGH(s)) AND (s[i] <> CHR(0)) DO
        OutPutCh(s[i]);
        INC(i);
    END;
    *)
    OutPutString(s);
    Threads.LeaveCriticalSection(WriteLock);
END WriteString;

PROCEDURE WriteLn;
BEGIN
    IF NOT TermInited THEN
        TermInit;
    END;

    Threads.EnterCriticalSection(WriteLock);

    Write(cr);
    Write(lf);

    Threads.LeaveCriticalSection(WriteLock);
END WriteLn;

PROCEDURE Position(NewX, NewY : CARDINAL);
BEGIN
    IF NOT TermInited THEN
        TermInit;
    END;

    Threads.EnterCriticalSection(WriteLock);

    X := NewX;
    Y := NewY;

    Threads.LeaveCriticalSection(WriteLock);
END Position;

PROCEDURE CharAvail() : BOOLEAN;
BEGIN
    IF NOT TermInited THEN
        TermInit;
    END;

    Threads.EnterCriticalSection(BufferLock);

    IF (kEnd > kBegin) AND (kEnd - 1 = kBegin) THEN
        Threads.LeaveCriticalSection(BufferLock);
        RETURN FALSE;
    ELSE
        Threads.LeaveCriticalSection(BufferLock);
        RETURN TRUE;
    END;
END CharAvail;

PROCEDURE ReadChar() : CHAR;
BEGIN
    IF NOT TermInited THEN
        TermInit;
    END;

    IF MainWindow <> NULL_HWND THEN
        WaitForChar();
        RETURN GetChar();
    ELSE
        RETURN 0c;
    END;
END ReadChar;

PROCEDURE Read(VAR OUT ch : CHAR);
BEGIN
    IF NOT TermInited THEN
        TermInit;
    END;

    ch := ReadChar();
END Read;

PROCEDURE Reset;
VAR
    X, Y        : ADRCARD;
BEGIN
    IF TermInited THEN
        FOR Y := 0 TO 24 DO
            FOR X := 0 TO 79 DO
                Screen[Y,X] := ' ';
            END;
        END;
        RepaintWindow(FALSE);
        Position(0, 0);
    END;
END Reset;

PROCEDURE RepaintWindow(paintMessage : BOOLEAN);
VAR
    DC              : HDC;
    <*/PUSH/NOCHECK:U*>
    PS              : PAINTSTRUCT;
    <*/POP*>
    i               : CARDINAL;
    LineRect        : RECT;
    Intersect       : RECT;
BEGIN
    IF paintMessage THEN
        DC := BeginPaint(MainWindow, PS);
    ELSE
        DC := GetDC(MainWindow);
    END;
    SelectObject(DC, GetStockObject(OEM_FIXED_FONT));
    LineRect.left := 0;
    LineRect.right := 80 * CharWidth-1;
    LineRect.top := 0;
    LineRect.bottom := INT(CharHeight) - 1;
    FOR i := 0 TO 24 DO
        IF (NOT paintMessage) OR IntersectRect(Intersect, PS.rcPaint, LineRect) THEN
            TextOut(DC, 0, i*CharHeight+Leading, Screen[i], 80);
        END;
        LineRect.top := LineRect.top + INT(CharHeight);
        LineRect.bottom := LineRect.bottom + INT(CharHeight);
    END;
    IF paintMessage THEN
        EndPaint(MainWindow, PS);
    ELSE
        ReleaseDC(MainWindow, DC);
    END;
END RepaintWindow;

PROCEDURE DoKeyDown(wParam : WPARAM) : BOOLEAN;
VAR
    normal : BOOLEAN;
BEGIN
    normal := TRUE;

    IF (ORD(GetKeyState(VK_SHIFT)) BAND 08000h) = 08000h THEN
        normal := FALSE;
    END;

    IF (ORD(GetKeyState(VK_CONTROL)) BAND 08000h) = 08000h THEN
        normal := FALSE;
    END;

    IF (ORD(GetKeyState(VK_MENU)) BAND 08000h) = 08000h THEN
        normal := FALSE;
    END;

    IF normal THEN
        CASE wParam OF
        VK_UP:
            AddChar(CursorUp);
            RETURN TRUE;
        |
        VK_DOWN:
            AddChar(CursorDown);
            RETURN TRUE;
        |
        VK_LEFT:
            AddChar(CursorLeft);
            RETURN TRUE;
        |
        VK_RIGHT:
            AddChar(CursorRight);
            RETURN TRUE;
        |
        VK_PRIOR:
            AddChar(PageUp);
            RETURN TRUE;
        |
        VK_NEXT:
            AddChar(PageDown);
            RETURN TRUE;
        ELSE
        END;
    END;
    RETURN FALSE;
END DoKeyDown;

PROCEDURE TerminalWindowProc(hWnd : HWND;
                             wMsg : UINT;
                             wParam : WPARAM;
                             lParam : LPARAM) : LRESULT [EXPORT, WINDOWS];
VAR
    pmmi        : PMINMAXINFO;
BEGIN
    CASE wMsg OF
    WM_PAINT:
        IF NOT IsIconic(hWnd) THEN
            RepaintWindow(TRUE);
        ELSE
            RETURN DefWindowProc(hWnd, wMsg, wParam, lParam);
        END;
    |
    WM_DESTROY:
        PostQuitMessage(0);
        AddChar(0C);
    |
    WM_SETFOCUS:
        HaveFocus := TRUE;
        IF CharWidth <> 0 THEN
            CreateCaret(hWnd, NULL_HBITMAP, CharWidth, CharHeight);
            IF CaretVisible THEN
                ShowCaret(hWnd);
                SetCaretPos(X*CharWidth, Y*CharHeight);
            END;
        END;
    |
    WM_KILLFOCUS:
        HaveFocus := FALSE;
        DestroyCaret;
    |
    WM_CHAR:
        AddChar(CHR(wParam));
    |
    WM_KEYDOWN:
        IF NOT DoKeyDown(wParam) THEN
            RETURN DefWindowProc(hWnd, wMsg, wParam, lParam);
        END;
    |
    WM_GETMINMAXINFO:
        IF MainWindow <> NULL_HWND THEN
            pmmi := CAST(PMINMAXINFO, lParam);
            pmmi^.ptMaxSize.x := 80 * CharWidth + DX;
            pmmi^.ptMaxSize.y := 25 * CharHeight + DY;
            pmmi^.ptMaxTrackSize.x := 80 * CharWidth + DX;
            pmmi^.ptMaxTrackSize.y := 25 * CharHeight + DY;
            pmmi^.ptMinTrackSize.x := 80 * CharWidth + DX;
            pmmi^.ptMinTrackSize.y := 25 * CharHeight + DY;
        END;
        RETURN DefWindowProc(hWnd, wMsg, wParam, lParam);
    |
    WM_CARETMESSAGE:
        IF wParam = SHOW_CARET THEN
            CaretVisible := TRUE;
            IF HaveFocus THEN
                ShowCaret(hWnd);
                SetCaretPos(X*CharWidth, Y*CharHeight);
            END;
        ELSE
            CaretVisible := FALSE;
            IF HaveFocus THEN
                HideCaret(hWnd);
            END;
        END;
    |
    WM_TERMINATE:
        DestroyWindow(hWnd);
    ELSE
        RETURN DefWindowProc(hWnd, wMsg, wParam, lParam);
    END;
    RETURN 0;
END TerminalWindowProc;

PROCEDURE StartWindow() : BOOLEAN;
VAR
    ok  : BOOLEAN;
BEGIN
    ok := TRUE;

    (* Create an Terminal window *)

    Class.style := CS_BYTEALIGNCLIENT BOR CS_VREDRAW BOR CS_HREDRAW;
    Class.lpfnWndProc := TerminalWindowProc;
    Class.cbClsExtra := 0;
    Class.cbWndExtra := 0;
    Class.hInstance := Instance;
    Class.hIcon := LoadIcon(NULL_HINSTANCE, IDI_APPLICATION^);
    Class.hCursor := LoadCursor(NULL_HINSTANCE, IDC_ARROW^);
    Class.hbrBackground := GetStockBrush(WHITE_BRUSH);
    Class.lpszMenuName := NIL;
    Class.lpszClassName := ADR("Modula-2 Terminal Window");

    RegisterClass(Class);

    HaveFocus := FALSE;
    CaretVisible := FALSE;
    CharWidth := 0;
    MainWindow := CreateWindowEx(0,
                                 "Modula-2 Terminal Window",
                                 "Terminal Window",
                                 WS_OVERLAPPEDWINDOW,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 NULL_HWND, NULL_HMENU,
                                 Instance,
                                 CAST(ADDRESS, 0));


    IF MainWindow <> NULL_HWND THEN
        GetWindowRect(MainWindow, wind);
        GetClientRect(MainWindow, client);

        (* Get the text info *)

        DC := GetDC(MainWindow);
        SelectObject(DC, GetStockObject(OEM_FIXED_FONT));
        GetTextMetrics(DC, TM);
        CharHeight := TM.tmHeight + TM.tmExternalLeading;
        CharWidth := TM.tmAveCharWidth;
        ReleaseDC(MainWindow, DC);

        DX := (wind.right - wind.left) - (client.right - client.left);
        DY := (wind.bottom - wind.top) - (client.bottom - client.top);
        SetWindowPos(MainWindow, NULL_HWND, 0,0,3000,3000,
                     SWP_NOMOVE BOR SWP_NOZORDER);

        ShowWindow(MainWindow, SW_SHOW);
        UpdateWindow(MainWindow);

        X := 0;
        Y := 0;
        Leading := TM.tmExternalLeading;

        IF HaveFocus THEN
            SendMessage(MainWindow, WM_SETFOCUS, 0, 0);
        END;

    ELSE
        MessageBox(NULL_HWND, "CreateWindow failed", "Terminal error", 0);
        ok := FALSE;
    END;

    Threads.SetEventSem(InitDone);
    RETURN ok;
END StartWindow;

PROCEDURE FlushMessage(dummy : ADDRESS) : CARDINAL;
VAR
    msg : MSG;
BEGIN
    UNREFERENCED_PARAMETER(dummy);

    IF NOT StartWindow() THEN
        TermInited := FALSE;
        RETURN 1;
    END;

    WHILE GetMessage(msg, NULL_HWND, 0, 0) DO
        TranslateMessage(msg);
        DispatchMessage(msg);
    END;

    TermInited := FALSE;

    RETURN 0;
END FlushMessage;

PROCEDURE TermInit;
VAR
    X, Y        : CARDINAL;
    created     : BOOLEAN;
BEGIN
    (* Erase screen before doing the create *)

    FOR Y := 0 TO 24 DO
        FOR X := 0 TO 79 DO
            Screen[Y,X] := ' ';
        END;
    END;

    Threads.ResetSignalSem(CharInBuf);

    Threads.CreateEventSem(InitDone, "", created);
    Threads.CreateThread(MessageThread, FlushMessage, NIL, 0, FALSE);
    Threads.WaitForEventSem(InitDone, Threads.SemWaitForever);
    Threads.CloseEventSem(InitDone);
    TermInited := TRUE;
END TermInit;

VAR
    created     : BOOLEAN;
BEGIN
    IF NOT IsThread THEN
        TermInited :=  FALSE;
        MainWindow := NULL_HWND;
        kBegin := 0;
        kEnd := 1;
        X := 0;
        Y := 0;
        Threads.CreateCriticalSectionEx(BufferLock, 1000, TRUE);
        Threads.CreateCriticalSection(WriteLock);
        Threads.CreateSignalSem(CharInBuf, MAX_CHARS, 0, "", created);
    END;

FINALLY
    IF NOT IsThread THEN
        IF TermInited THEN
            SendMessage(MainWindow, WM_TERMINATE, 0, 0);
            Threads.SleepThread(100);
        END;

        Threads.CloseCriticalSection(BufferLock);
        Threads.CloseCriticalSection(WriteLock);
        Threads.CloseSignalSem(CharInBuf);
    END;
END Terminal.
