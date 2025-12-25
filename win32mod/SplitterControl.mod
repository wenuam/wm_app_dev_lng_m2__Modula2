IMPLEMENTATION MODULE SplitterControl;
(*
 * Module   : CCsplitter.c
 * Copyright: R.W.G. Hünen (rhunen@xs4all.nl)
 * http://www.xs4all.nl/~rhunen/ccsplitter/
 *
 * Based on original code by Rob Pitt.
 *
 * This file is part of the Splitter Control.
 *
 * The Splitter Control is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option) any
 * later version.
 *
 * The Splitter Control is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * the Splitter Control; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * $Log: $
 *)

(* ported to Modula-2, and extended, by Stony Brook Software 2002*)

<*/NOWARN:F*>

FROM SYSTEM IMPORT
    ADRCARD, ADR, CAST;

FROM WIN32 IMPORT
    HWND, DWORD, UINT, WORD, BOOL, WPARAM, LPARAM, LRESULT, HINSTANCE, HMODULE,
    RECT, LPRECT, POINT, LONG_PTR,
    HBITMAP, HCURSOR, HBRUSH, HDC,
    GetModuleHandle,
    HeapAlloc, HeapFree, GetProcessHeap;

FROM WINUSER IMPORT
    HIWORD, LOWORD,
    WNDCLASSEX, GetClassInfoEx, RegisterClassEx,
    IDC_ARROW, IDC_SIZENS, IDC_SIZEWE, IDC_SIZEALL, LoadCursor, SetCursor,
    COLOR_ACTIVEBORDER, GetSysColorBrush,
    DefWindowProc, IsWindow, GetParent,
    GetWindowLongPtr, SetWindowLongPtr, GetWindowLong, SetWindowLong, GWL_STYLE, GWL_ID,
    WS_CLIPSIBLINGS,
    WM_CAPTURECHANGED, WM_CREATE, WM_DESTROY, WM_LBUTTONDOWN, WM_LBUTTONUP,
    WM_MOUSEMOVE, WM_SETCURSOR, WM_SIZE, WM_NOTIFY, WM_USER, WM_SETFOCUS, WM_KILLFOCUS,
    SendMessage, PostMessage, SetCapture, ReleaseCapture, SetFocus,
    GetClientRect, SetRect, ScreenToClient, GetMessagePos,
    DCX_PARENTCLIP, GetDCEx, ReleaseDC,
    HDWP, BeginDeferWindowPos, DeferWindowPos, EndDeferWindowPos,
    HWND_TOP, SWP_NOACTIVATE, SWP_SHOWWINDOW, SWP_NOZORDER,
    SW_HIDE, ShowWindow,
    MapWindowPoints;

FROM WINGDI IMPORT
    CreateBitmap, CreatePatternBrush,
    PATINVERT, PatBlt;

FROM WINX IMPORT
    NIL_STR,
    SelectBrush, DeleteBitmap;

FROM MemUtils IMPORT
    ZeroMem;

(********************************************************************************/
/* Constant definitions                                                         */
/********************************************************************************)

CONST
    GWL_SPINFO  = 0;                   (* GetWindowLong offset of SPINFO ptr *)

    NVIEWS      = 4;                   (* Number of View windows *)

    WM_SETFOCUS_DELAY   = WM_USER + 203;

(********************************************************************************/
/* Macro definitions                                                            */
/********************************************************************************)

PROCEDURE SPLT(pos, w : INTEGER) : INTEGER;
BEGIN
    RETURN (pos - w/2 + 1);
END SPLT;

PROCEDURE SPRB(pos, w : INTEGER) : INTEGER;
BEGIN
    RETURN (pos - w/2 + 1 + w);
END SPRB;

PROCEDURE SPLEFT = SPLT;
PROCEDURE SPTOP = SPLT;
PROCEDURE SPRIGHT = SPRB;
PROCEDURE SPBOTTOM = SPRB;

PROCEDURE CURSORPOS_OK(cur, spl, w : INTEGER) : BOOLEAN;
BEGIN
    RETURN ((spl >= 0) AND (cur >= SPLT(spl, w)) AND (cur <= SPRB(spl, w)));
END CURSORPOS_OK;

(********************************************************************************/
/* Type definitions                                                             */
/********************************************************************************)

TYPE
    SPINFO =
    RECORD
    hView       : ARRAY [0..NVIEWS-1] OF HWND;              (* View window handles *)
    xpos        : INTEGER;              (* X-position of split (-1 = no split) *)
    ypos        : INTEGER;              (* Y-position of split (-1 = no split) *)
    xwidth      : INTEGER;              (* X-width of split bar *)
    ywidth      : INTEGER;              (* Y-width of split bar *)
    xlower      : INTEGER;              (* X-lower drag margin *)
    xupper      : INTEGER;              (* X-upper drag margin *)
    ylower      : INTEGER;              (* Y-lower drag margin *)
    yupper      : INTEGER;              (* Y-upper drag margin high *)
    xdrag       : INTEGER;              (* X-position during drag (-1 = no drag) *)
    ydrag       : INTEGER;              (* Y-position during drag (-1 = no drag) *)
    activeView  : CARDINAL;(*sb added*)
    dragDone    : BOOLEAN;              (* Drag operation completed flag *)
    focus       : BOOLEAN;(*SB added*)
    userResize  : BOOLEAN;(*SB added*)
    active      : BOOLEAN;(*SB added*)
    END;
    SPINFO_PTR  = POINTER TO SPINFO;

VAR
    hcurVert    : HCURSOR;              (* Cursor for North/South split *)
    hcurHorz    : HCURSOR;              (* Cursor for East/West split *)
    hcurBoth    : HCURSOR;              (* Cursor for double split *)
    hbrDotty    : HBRUSH;               (* Dotty brush handle *)

CONST
    dottyData : ARRAY [0..7] OF WORD = {055h,0AAh,055h,0AAh,055h,0AAh,055h,0AAh};

PROCEDURE SpGetRect (
        hSplitter : HWND;               (* Splitter window handle *)
        id : INTEGER;                   (* Rectangle ID *)
        prc : LPRECT) : BOOL;           (* Rectangle data ptr *)
VAR
    rc          : RECT;                 (* Window rectangle *)
    sp          : SPINFO_PTR;           (* Splitter info ptr *)
BEGIN
    IF prc = NIL THEN
        RETURN FALSE;
    END;

    SetRect(prc^, 0, 0, 0, 0);
    GetClientRect(hSplitter, rc);

    sp := CAST(SPINFO_PTR, GetWindowLongPtr(hSplitter, GWL_SPINFO));

    CASE id OF
    CCSPV_TOPLEFT:
        prc^.left   := 0;
        prc^.top    := 0;
        IF sp^.xpos >= 0 THEN
            prc^.right  := SPLEFT(sp^.xpos, sp^.xwidth);
        ELSE
            prc^.right := rc.right;
        END;
        IF sp^.ypos >= 0 THEN
            prc^.bottom := SPTOP(sp^.ypos, sp^.ywidth);;
        ELSE
            prc^.bottom := rc.bottom;
        END;
        RETURN TRUE;
    |
    CCSPV_TOPRIGHT:
        IF sp^.xpos >= 0 THEN
            prc^.left   := SPRIGHT(sp^.xpos, sp^.xwidth);
            prc^.top    := 0;
            prc^.right  := rc.right;
            IF sp^.ypos >= 0 THEN
                prc^.bottom := SPTOP(sp^.ypos, sp^.ywidth);
            ELSE
                prc^.bottom := rc.bottom;
            END;
            RETURN TRUE;
        END;
    |
    CCSPV_BOTTOMLEFT:
        IF sp^.ypos >= 0 THEN
            prc^.top    := SPBOTTOM(sp^.ypos, sp^.ywidth);
            IF sp^.xpos >= 0 THEN
                prc^.right  := SPLEFT(sp^.xpos, sp^.xwidth);
            ELSE
                prc^.right := rc.right;
            END;
            prc^.bottom := rc.bottom;
            RETURN TRUE;
        END;
    |
    CCSPV_BOTTOMRIGHT:
        IF (sp^.ypos >= 0) AND (sp^.xpos >= 0) THEN
            prc^.left   := SPRIGHT(sp^.xpos, sp^.xwidth);
            prc^.top    := SPBOTTOM(sp^.ypos, sp^.ywidth);
            prc^.right  := rc.right;
            prc^.bottom := rc.bottom;
            RETURN TRUE;
        END;
    ELSE
    END;

    (* Rectangle does not exist... *)
    RETURN FALSE;
END SpGetRect;

PROCEDURE SpNotifyParent (
        hSplitter       : HWND;              (* Splitter window handle *)
        code            : UINT;              (* Notification code *)
        xParam          : INTEGER;           (* X-parameter to notification *)
        yParam          : INTEGER);          (* Y-parameter to notification *)
VAR
    notify      : NMCCSPLIT;                 (* Notification data *)
BEGIN
    notify.hdr.hwndFrom := hSplitter;
    notify.hdr.idFrom   := GetWindowLong(hSplitter, GWL_ID);
    notify.hdr.code     := code;
    notify.x            := xParam;
    notify.y            := yParam;

    SendMessage(GetParent(hSplitter), WM_NOTIFY, notify.hdr.idFrom, CAST(LPARAM, ADR(notify)));
END SpNotifyParent;

PROCEDURE SpPositionViewWindows(hSplitter : HWND);
VAR
    sp          : SPINFO_PTR;               (* Splitter info ptr *)
    hDefer      : HDWP;                     (* DeferWindowPos handle *)
    <*/PUSH/NOCHECK:U*>
    rc          : RECT;                     (* View window rectangle *)
    <*/POP*>
    i           : ADRCARD;                  (* Loop counter *)
    flags       : CARDINAL;
BEGIN
    sp := CAST(SPINFO_PTR, GetWindowLongPtr(hSplitter, GWL_SPINFO));

    flags := SWP_NOACTIVATE BOR SWP_SHOWWINDOW;
    IF NOT sp^.active THEN
        flags := flags BOR SWP_NOZORDER;
    END;

    hDefer := BeginDeferWindowPos(NVIEWS);
    FOR i := 0 TO NVIEWS-1 DO
        IF sp^.hView[i] <> NIL THEN
            IF SpGetRect(hSplitter, i, ADR(rc)) THEN
                MapWindowPoints(hSplitter, GetParent(sp^.hView[i]), CAST(POINT, rc), 2);
                hDefer := DeferWindowPos(hDefer,
                                         sp^.hView[i],
                                         HWND_TOP,
                                         rc.left, rc.top,
                                         rc.right-rc.left, rc.bottom-rc.top,
                                         flags);
            ELSE
                ShowWindow(sp^.hView[i], SW_HIDE);
            END;
        END;
    END;
    EndDeferWindowPos(hDefer);

    SpNotifyParent(hSplitter, CCSPN_POSCHANGED, sp^.xpos, sp^.ypos);
END SpPositionViewWindows;

PROCEDURE SpWindowProc (
        hSplitter  : HWND;              (* Splitter window handle *)
        uMsg   : UINT;                  (* Message code *)
        wParam : WPARAM;                (* Message parameter *)
        lParam : LPARAM) : LRESULT [OSCall, EXPORT];     (* Message parameter *)
VAR
    spx         : PCCSPLIT;             (* X-split data ptr *)
    spy         : PCCSPLIT;             (* Y-split data ptr *)
    sp          : SPINFO_PTR;           (* Splitter info *)
    rc          : RECT;                 (* Window rectangle *)
    hdc         : HDC;                  (* Device Context handle *)
    hbr         : HBRUSH;               (* Brush handle *)
    pt          : POINT;                (* Coordinate of point *)
    dd          : DWORD;                (* Double word temp *)
    xold, yold  : INTEGER;
    i           : ADRCARD;
    wnd         : HWND;
BEGIN
    sp := CAST(SPINFO_PTR, GetWindowLongPtr(hSplitter, GWL_SPINFO));

    CASE uMsg OF
    WM_CAPTURECHANGED:
        IF (sp^.xdrag >= 0) OR (sp^.ydrag >= 0) THEN
            xold := sp^.xpos;(*SB changed*)
            yold := sp^.ypos;(*SB changed*)

            GetClientRect(hSplitter, rc);

            hdc := GetDCEx(hSplitter, NIL, DCX_PARENTCLIP);
            hbr := SelectBrush(hdc, hbrDotty);

            IF sp^.ydrag >= 0 THEN
                PatBlt(hdc,
                       0, SPTOP(sp^.ydrag, sp^.ywidth),
                       rc.right, sp^.ywidth,
                       PATINVERT);
                IF sp^.dragDone THEN
                    sp^.ypos := sp^.ydrag;
                END;
                sp^.ydrag := -1;
            END;
            IF sp^.xdrag >= 0 THEN
                PatBlt(hdc,
                       SPLEFT (sp^.xdrag, sp^.xwidth), 0,
                       sp^.xwidth, rc.bottom,
                       PATINVERT);
                IF sp^.dragDone THEN
                    sp^.xpos := sp^.xdrag;
                END;
                sp^.xdrag := -1;
            END;

            SelectBrush(hdc, hbr);
            ReleaseDC(hSplitter, hdc);

            IF sp^.dragDone THEN
                IF (sp^.xpos <= 0) OR (sp^.xpos >= rc.right) THEN
                    sp^.xpos := -1;
                END;
                IF (sp^.ypos <= 0) OR (sp^.ypos >= rc.bottom) THEN
                    sp^.ypos := -1;
                END;
                sp^.dragDone := FALSE;

                SpPositionViewWindows(hSplitter);

                IF (xold <> sp^.xpos) OR (yold <> sp^.ypos) THEN(*SB changed*)
                    SpNotifyParent(hSplitter, CCSPN_SPLITCHANGED, ORD(sp^.xpos >= 0), ORD(sp^.ypos >= 0));
                END;
            END;
        END;
        RETURN 1;
    |
    WM_CREATE:
        sp := HeapAlloc(GetProcessHeap(), 0, SIZE(sp^));

        sp^.xpos   := -1;
        sp^.xdrag  := -1;
        sp^.ypos   := -1;
        sp^.ydrag  := -1;
        sp^.xlower := 0;
        sp^.xupper := 0;
        sp^.ylower := 0;
        sp^.yupper := 0;
        sp^.xwidth := CCSP_DEFWIDTH;
        sp^.ywidth := CCSP_DEFWIDTH;
        sp^.activeView := 0;
        sp^.dragDone := FALSE;
        sp^.userResize := TRUE;
        sp^.active := TRUE;
        FOR i := 0 TO NVIEWS-1 DO
            sp^.hView[i] := NIL;
        END;

        SetWindowLongPtr(hSplitter, GWL_SPINFO, CAST(LONG_PTR, sp));
        SetWindowLong(hSplitter,
                      GWL_STYLE,
                      GetWindowLong(hSplitter, GWL_STYLE) BOR WS_CLIPSIBLINGS);
    |
    WM_DESTROY:
        HeapFree(GetProcessHeap(), 0, sp);
    |
    WM_LBUTTONDOWN:
        sp^.xdrag := -1;
        sp^.ydrag := -1;
        IF sp^.userResize THEN
            dd := GetMessagePos();
            pt.x := CAST(INTEGER16, LOWORD(dd));
            pt.y := CAST(INTEGER16, HIWORD(dd));
            ScreenToClient(hSplitter, pt);

            IF CURSORPOS_OK(pt.x, sp^.xpos, sp^.xwidth) THEN
                sp^.xdrag := pt.x;
            ELSE
            END;
            IF CURSORPOS_OK(pt.y, sp^.ypos, sp^.ywidth) THEN
                sp^.ydrag := pt.y;
            END;

            IF (sp^.xdrag >=0) OR (sp^.ydrag >= 0) THEN
                GetClientRect(hSplitter, rc);

                hdc := GetDCEx(hSplitter, NIL, DCX_PARENTCLIP);
                hbr := SelectBrush(hdc, hbrDotty);

                IF sp^.xdrag >= 0 THEN
                    PatBlt(hdc,
                           SPLEFT(sp^.xdrag, sp^.xwidth), 0,
                           sp^.xwidth, rc.bottom,
                           PATINVERT);
                END;
                IF sp^.ydrag >= 0 THEN
                    PatBlt(hdc,
                           0, SPTOP(sp^.ydrag, sp^.ywidth),
                           rc.right, sp^.ywidth,
                           PATINVERT);
                END;

                SelectBrush(hdc, hbr);
                ReleaseDC(hSplitter, hdc);

                SetCapture(hSplitter);
            END;
        END;
        RETURN 1;
    |
    WM_LBUTTONUP:
        IF (sp^.xdrag >= 0) OR (sp^.ydrag >= 0) THEN
            sp^.dragDone := TRUE;
            ReleaseCapture();
        END;
        RETURN 1;
    |
    WM_MOUSEMOVE:
        IF (sp^.xdrag >= 0) OR (sp^.ydrag >= 0) THEN
            GetClientRect(hSplitter, rc);

            dd := GetMessagePos();
            pt.x := CAST(INTEGER16, LOWORD(dd));
            pt.y := CAST(INTEGER16, HIWORD(dd));
            ScreenToClient(hSplitter, pt);

            IF pt.x < sp^.xlower THEN
                pt.x := sp^.xlower;
            END;
            IF pt.y < sp^.ylower THEN
                pt.y := sp^.ylower;
            END;
            IF (sp^.xupper <> 0) AND (pt.x > sp^.xupper) THEN
                pt.x := sp^.xupper;
            END;
            IF (sp^.yupper <> 0) AND (pt.y > sp^.yupper) THEN
                pt.y := sp^.yupper;
            END;

            hdc := GetDCEx(hSplitter, NIL, DCX_PARENTCLIP);
            hbr := SelectBrush(hdc, hbrDotty);

            IF sp^.ydrag >= 0 THEN
                PatBlt(hdc,
                       0, SPTOP(sp^.ydrag, sp^.ywidth),
                       rc.right, sp^.ywidth,
                       PATINVERT);
                sp^.ydrag := pt.y;
            END;
            IF sp^.xdrag >= 0 THEN
               PatBlt(hdc,
                       SPLEFT(sp^.xdrag, sp^.xwidth), 0,
                       sp^.xwidth, rc.bottom,
                       PATINVERT);
                sp^.xdrag := pt.x;

                PatBlt(hdc,
                       SPLEFT(sp^.xdrag, sp^.xwidth), 0,
                       sp^.xwidth, rc.bottom,
                       PATINVERT);
            END;
            IF sp^.ydrag >= 0 THEN
                PatBlt(hdc,
                       0, SPTOP(sp^.ydrag, sp^.ywidth),
                       rc.right, sp^.ywidth,
                       PATINVERT);
            END;

            SelectBrush(hdc, hbr);
            ReleaseDC(hSplitter, hdc);
        END;
        RETURN 1;
    |
    WM_SETCURSOR:
        IF sp^.userResize THEN
            dd := GetMessagePos();
            pt.x := CAST(INTEGER16, LOWORD(dd));
            pt.y := CAST(INTEGER16, HIWORD(dd));
            ScreenToClient(hSplitter, pt);

            IF CURSORPOS_OK (pt.x, sp^.xpos, sp^.xwidth) THEN
                IF CURSORPOS_OK (pt.y, sp^.ypos, sp^.ywidth) THEN
                    SetCursor(hcurBoth);
                ELSE
                    SetCursor(hcurHorz);
                END;
                RETURN 1;
            ELSE
                IF CURSORPOS_OK (pt.y, sp^.ypos, sp^.ywidth) THEN
                    SetCursor(hcurVert);
                    RETURN 1;
                END;
            END;
        END;
    |
    WM_SIZE:
        SpPositionViewWindows(hSplitter);
    |
    WM_SETFOCUS:(*SB added*)
        PostMessage(hSplitter, WM_SETFOCUS_DELAY, 0, 0);
        sp^.focus := TRUE;
    |
    WM_KILLFOCUS:(*SB added*)
        sp^.focus := FALSE;
    |
    WM_SETFOCUS_DELAY:(*SB added*)
        IF sp^.focus THEN
            wnd := sp^.hView[sp^.activeView];
            IF (wnd <> NIL) AND IsWindow(wnd) THEN
                SetFocus(wnd);
                RETURN 0;
            ELSE
                FOR i := 0 TO NVIEWS-1 DO
                    IF sp^.hView[i] <> NIL THEN
                        SetFocus(sp^.hView[i]);
                        sp^.activeView := i;
                        RETURN 0;
                    END;
                END;
            END;
        END;
    |
    CCSPM_GETHANDLE:
        CASE wParam OF
        CCSPV_TOPLEFT,
        CCSPV_TOPRIGHT,
        CCSPV_BOTTOMLEFT,
        CCSPV_BOTTOMRIGHT:
            RETURN CAST(LRESULT, sp^.hView[wParam]);
        ELSE
        END;
        RETURN 0;
    |
    CCSPM_GETRECT:
        RETURN ORD(SpGetRect(hSplitter, wParam, CAST(LPRECT, lParam)));
    |
    CCSPM_GETSPLIT:
        spx := CAST(PCCSPLIT, wParam);
        spy := CAST(PCCSPLIT, lParam);

        IF ((spx <> NIL) AND (spx^.cbSize <> SIZE(spx^))) OR
           ((spy <> NIL) AND (spy^.cbSize <> SIZE(spy^)))
        THEN
            RETURN 0;
        END;

        IF spx <> NIL THEN
            spx^.flags := CCSP_POS BOR CCSP_LOWER BOR CCSP_UPPER BOR CCSP_WIDTH;
            spx^.pos   := sp^.xpos;
            spx^.lower := sp^.xlower;
            spx^.upper := sp^.xupper;
            spx^.width := sp^.xwidth;
        END;

        IF spy <> NIL THEN
            spy^.flags := CCSP_POS BOR CCSP_LOWER BOR CCSP_UPPER BOR CCSP_WIDTH;
            spy^.pos   := sp^.ypos;
            spy^.lower := sp^.ylower;
            spy^.upper := sp^.yupper;
            spy^.width := sp^.ywidth;
        END;

        RETURN 1;
    |
    CCSPM_SETHANDLE:
        CASE wParam OF
        CCSPV_TOPLEFT,
        CCSPV_TOPRIGHT,
        CCSPV_BOTTOMLEFT,
        CCSPV_BOTTOMRIGHT:
            wnd := CAST(HWND, lParam);
            IF (wnd = NIL) OR IsWindow(wnd) THEN
                IF (sp^.hView[wParam] <> NIL) AND (sp^.hView[wParam] <> wnd) THEN
                    ShowWindow(sp^.hView[wParam], SW_HIDE);
                END;

                sp^.hView[wParam] := wnd;

                (* has problems if a splitter is inside a tab control
                   we now just move children to the top.
                SetWindowPos(hSplitter,
                             HWND_BOTTOM,
                             0, 0, 0, 0,
                             SWP_NOACTIVATE BOR
                                 SWP_NOMOVE BOR
                                 SWP_NOREDRAW BOR
                                 SWP_NOSIZE);*)

                SpPositionViewWindows(hSplitter);
                RETURN 1;
            END;
        ELSE
        END;
        RETURN 0;
    |
    CCSPM_POSITIONVIEWS:(*SB added*)
        SpPositionViewWindows(hSplitter);
        RETURN 1;
    |
    CCSPM_SETACTIVEVIEW:(*SB added*)
        IF (wParam <= 3) AND IsWindow(sp^.hView[wParam]) THEN
            sp^.activeView := wParam;
            RETURN 1;
        END;
        RETURN 0;
    |
    CCSPM_SETACTIVE:(*SB added*)
        sp^.active := wParam <> 0;
        RETURN 0;
    |
    CCSPM_SETSPLIT:
        GetClientRect(hSplitter, rc);

        spx := CAST(PCCSPLIT, wParam);
        spy := CAST(PCCSPLIT, lParam);

        IF ((spx <> NIL) AND (spx^.cbSize <> SIZE(spx^))) OR
           ((spy <> NIL) AND (spy^.cbSize <> SIZE(spy^)))
        THEN
            RETURN 0;
        END;

        IF spx <> NIL THEN
            IF (spx^.flags BAND CCSP_FPOS) <> 0 THEN
                sp^.xpos   := spx^.pos * rc.right / 100;
            ELSIF (spx^.flags BAND CCSP_POS) <> 0 THEN
                sp^.xpos   := spx^.pos;
            END;
            IF sp^.xpos <= 0 THEN
                sp^.xpos   := -1;
            END;

            IF (spx^.flags BAND CCSP_FLOWER) <> 0 THEN
                sp^.xlower := spx^.lower * rc.right / 100;
            ELSIF (spx^.flags BAND CCSP_LOWER) <> 0 THEN
                sp^.xlower := spx^.lower;
            END;
            IF sp^.xlower <= 0 THEN
                sp^.xlower := 0;
            END;

            IF (spx^.flags BAND CCSP_FUPPER) <> 0 THEN
                sp^.xupper := spx^.upper * rc.right / 100;
            ELSIF (spx^.flags BAND CCSP_UPPER) <> 0 THEN
                sp^.xupper := spx^.upper;
            END;
            IF sp^.xupper <= 0 THEN
                sp^.xupper := 0;
            END;

            IF (spx^.flags BAND CCSP_WIDTH) <> 0 THEN
                sp^.xwidth := spx^.width;
            END;
            IF sp^.xwidth < CCSP_DEFWIDTH THEN
                sp^.xwidth := CCSP_DEFWIDTH;
            END;
        END;

        IF spy <> NIL THEN
            IF (spy^.flags BAND CCSP_FPOS) <> 0 THEN
                sp^.ypos   := spy^.pos * rc.bottom / 100;
            ELSIF (spy^.flags BAND CCSP_POS) <> 0 THEN
                sp^.ypos   := spy^.pos;
            END;
            IF sp^.ypos <= 0 THEN
                sp^.ypos   := -1;
            END;

            IF (spy^.flags BAND CCSP_FLOWER) <> 0 THEN
                sp^.ylower := spy^.lower * rc.bottom / 100;
            ELSIF (spy^.flags BAND CCSP_LOWER) <> 0 THEN
                sp^.ylower := spy^.lower;
            END;
            IF sp^.ylower <= 0 THEN
                sp^.ylower := 0;
            END;

            IF (spy^.flags BAND CCSP_FUPPER) <> 0 THEN
                sp^.yupper := spy^.upper * rc.bottom / 100;
            ELSIF (spy^.flags BAND CCSP_UPPER) <> 0 THEN
                sp^.yupper := spy^.upper;
            END;
            IF sp^.yupper <= 0 THEN
                sp^.yupper := 0;
            END;

            IF (spy^.flags BAND CCSP_WIDTH) <> 0 THEN
                sp^.ywidth := spy^.width;
            END;
            IF sp^.ywidth < CCSP_DEFWIDTH THEN
                sp^.ywidth := CCSP_DEFWIDTH;
            END;
        END;

        SpPositionViewWindows(hSplitter);
        SpNotifyParent(hSplitter, CCSPN_SPLITCHANGED, ORD(sp^.xpos >= 0), ORD(sp^.ypos >= 0));
        RETURN 1;
    |
    CCSPM_SETUSERRESIZE:
        sp^.userResize := wParam <> 0;
        RETURN 1;
    ELSE
    END;

    RETURN DefWindowProc(hSplitter, uMsg, wParam, lParam);
END SpWindowProc;

PROCEDURE InitCCsplitter() : BOOL;
VAR
    wc          : WNDCLASSEX;   (* Window class data *)
    hbm         : HBITMAP;      (* Bitmap handle *)
    instance    : HMODULE;
BEGIN
    hcurVert := LoadCursor(NIL, IDC_SIZENS^);
    hcurHorz := LoadCursor(NIL, IDC_SIZEWE^);
    hcurBoth := LoadCursor(NIL, IDC_SIZEALL^);

    hbm      := CreateBitmap(8, 8, 1, 1, ADR(dottyData));
    hbrDotty := CreatePatternBrush(hbm);
    DeleteBitmap(hbm);

    instance := GetModuleHandle(NIL_STR);
    IF GetClassInfoEx(CAST(HINSTANCE, instance), WC_CCSPLITTER, wc) THEN
        RETURN TRUE;
    END;

    ZeroMem(ADR(wc), SIZE(wc));
    wc.cbSize        := SIZE(wc);
    wc.cbWndExtra    := SIZE(SPINFO_PTR);
    wc.hInstance     := CAST(HINSTANCE, instance);
    wc.hCursor       := LoadCursor(NIL, IDC_ARROW^);
    wc.hbrBackground := GetSysColorBrush(COLOR_ACTIVEBORDER);
    wc.lpfnWndProc   := SpWindowProc;
    wc.lpszClassName := ADR(WC_CCSPLITTER);

    RETURN RegisterClassEx(wc) <> 0;
END InitCCsplitter;

BEGIN
    InitCCsplitter();
END SplitterControl.
