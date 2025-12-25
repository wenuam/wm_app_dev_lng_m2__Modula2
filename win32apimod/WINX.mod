(***************************************************************************)
(*                                                                         *)
(*                           Copyright (C) 2009                            *)
(*                             by ADW Software                             *)
(*                                                                         *)
(*                           All rights reserved.                          *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE WINX;
<*/NOWARN:FU*>

FROM SYSTEM IMPORT
    ADR, CAST, ADDRESS, MAKEADR, OFFS;

FROM WIN32 IMPORT
    HINSTANCE, HMODULE, DWORD, BOOL, HGLOBAL, GlobalLock, GlobalHandle,
    GlobalUnlock, GlobalAlloc, GlobalReAlloc, GlobalFree, GetCurrentThreadId, HPEN, LPSTR, LPWSTR,
    HDC, HBRUSH, HRGN, HPALETTE, HGDIOBJ, HFONT, HBITMAP, RECT, HWND, HANDLE,
    WPARAM, LPARAM, POINT, WORD, LRESULT, UINT, HICON, HLOCAL, WINT,
    LPTSTR, LONG, HMENU, HACCEL, HCURSOR, LONG_PTR, ULONG_PTR;

FROM WINGDI IMPORT
    GetStockObject, SelectObject, DeleteObject,
    CombineRgn, RGN_AND, RGN_COPY,
    RGN_DIFF, RGN_OR, RGN_XOR,
    LOGPEN, LOGBRUSH, LOGFONT, LOGFONTW, BITMAP, GetObject;

FROM WINUSER IMPORT
    LOWORD, HIWORD,
    EnableScrollBar, SB_CTL, ShowWindow, ShowWindowAsync, SetScrollPos, GetScrollPos, SendMessage,
    SendMessageA, SendMessageW, InflateRect, GetWindowLong, GetClassLong, GetClassLongPtr, SetClassLongPtr,
    GWL_STYLE, GWL_EXSTYLE, GW_HWNDFIRST, GW_HWNDNEXT, GW_HWNDPREV, GetWindow, GetWindowThreadProcessId,
    GCL_CBCLSEXTRA, GCL_CBWNDEXTRA, GCL_HBRBACKGROUND, GCL_HCURSOR, GCL_HICON, GCL_HMODULE, GCL_STYLE,
    GCL_WNDPROC,
    GetTopWindow, GetDlgCtrlID, GW_OWNER, GW_HWNDLAST,
    WM_SETREDRAW, WM_CTLCOLORMSGBOX,
    IsIconic, IsZoomed, WS_MINIMIZE, WS_MAXIMIZE, MapWindowPoints,
    GetKeyState, DialogBoxParam, CreateDialogParamW, MAKEINTRESOURCEW,
    VK_LBUTTON, VK_RBUTTON, VK_MBUTTON, VK_SHIFT, VK_CONTROL,
    LB_SETTOPINDEX,
    LB_GETTOPINDEX, LB_GETHORIZONTALEXTENT, LB_SETHORIZONTALEXTENT,
    LB_SETCOLUMNWIDTH, LB_GETITEMRECT, LB_SETCARETINDEX, LB_GETCARETINDEX,
    LB_FINDSTRINGEXACT, LB_SETITEMHEIGHT, LB_GETITEMHEIGHT, GetWindowText, GetWindowTextA, GetWindowTextW,
    GetWindowTextLength, GetWindowTextLengthA, GetWindowTextLengthW, SetWindowText, SetWindowTextA,
    SetWindowTextW, CB_LIMITTEXT, CB_GETEDITSEL, CB_SETEDITSEL, CB_GETCOUNT, CB_RESETCONTENT, CB_ADDSTRING,
    CB_INSERTSTRING, CB_DELETESTRING,
    CB_GETLBTEXTLEN, CB_GETLBTEXT, CB_GETITEMDATA, CB_SETITEMDATA,
    CB_FINDSTRING, CB_GETCURSEL, CB_SETCURSEL,
    CB_SELECTSTRING, CB_SHOWDROPDOWN, CB_FINDSTRINGEXACT, CB_GETDROPPEDSTATE,
    CB_GETDROPPEDCONTROLRECT, CB_GETITEMHEIGHT, CB_SETITEMHEIGHT,
    CB_GETEXTENDEDUI, CB_SETEXTENDEDUI, CB_GETTOPINDEX, CB_SETTOPINDEX,
    EnableWindow, MAKELPARAM,
    WNDPROC, GWL_WNDPROC, DLGPROC, DWLP_DLGPROC, SetWindowLong, SetWindowLongPtr,
    WM_GETFONT, WM_SETFONT, CB_DIR, LB_DIR, BM_GETCHECK, BM_SETCHECK, BM_CLICK,
    BM_GETSTATE, BM_SETSTATE, BM_SETSTYLE, EM_LIMITTEXT, EM_GETLINECOUNT,
    EM_GETLINE, EM_GETRECT, EM_SETRECT, EM_SETRECTNP, EM_GETSEL, EM_SETSEL,
    EM_REPLACESEL, EM_GETMODIFY, EDITWORDBREAKPROC, EDITWORDBREAKPROCA, EDITWORDBREAKPROCW,
    EM_LINEFROMCHAR, EM_SETMODIFY, STM_SETICON, STM_GETICON, STM_SETIMAGE, STM_GETIMAGE,
    EM_LINEINDEX, EM_LINELENGTH, EM_LINESCROLL, EM_CANUNDO, EM_UNDO,
    EM_EMPTYUNDOBUFFER, EM_SETPASSWORDCHAR, EM_SETTABSTOPS, EM_FMTLINES,
    EM_GETHANDLE, EM_SETHANDLE, EM_GETFIRSTVISIBLELINE, EM_SETREADONLY,
    EM_GETPASSWORDCHAR, EM_SETWORDBREAKPROC, EM_GETWORDBREAKPROC,
    SetScrollRange, LB_GETCOUNT, LB_RESETCONTENT, LB_ADDSTRING,
    LB_INSERTSTRING, LB_DELETESTRING,
    LB_GETTEXTLEN, LB_GETTEXT, LB_GETITEMDATA, LB_SETITEMDATA,
    LB_FINDSTRING, LB_SETSEL, LB_SELITEMRANGE, LB_GETCURSEL,
    LB_SETCURSEL, LB_SELECTSTRING, LB_GETSEL,
    LB_GETSELCOUNT, LB_GETSELITEMS, GetScrollRange, LB_SETTABSTOPS,
    LoadMenu, LoadAccelerators, LoadIcon, LoadImage, LoadCursor, LoadBitmap;

FROM WINUSER IMPORT
    EM_SCROLLCARET, GWL_HINSTANCE, CharUpper, CharUpperA, CharUpperW, CharLower, CharLowerA, CharLowerW,
    BM_SETIMAGE, IMAGE_BITMAP, IMAGE_ICON;

<*/NOWARN:CU*>

PROCEDURE GetInstanceModule(hInstance : HINSTANCE) : HMODULE;
BEGIN
    RETURN CAST(HMODULE, hInstance);
END GetInstanceModule;

PROCEDURE GlobalPtrHandle(lp : ADDRESS) : HGLOBAL;
BEGIN
    RETURN GlobalHandle(lp);
END GlobalPtrHandle;

PROCEDURE GlobalLockPtr(lp : ADDRESS) : BOOL;
BEGIN
    RETURN CAST(BOOL, GlobalLock(GlobalPtrHandle(lp)));
END GlobalLockPtr;

PROCEDURE GlobalUnlockPtr(lp : ADDRESS) : BOOL;
BEGIN
    RETURN GlobalUnlock(GlobalPtrHandle(lp));
END GlobalUnlockPtr;

PROCEDURE GlobalAllocPtr(flags, cb : DWORD) : ADDRESS;
BEGIN
    RETURN GlobalLock(GlobalAlloc((flags), (cb)));
END GlobalAllocPtr;

PROCEDURE GlobalReAllocPtr(lp : ADDRESS; cbNew, flags : DWORD) : ADDRESS;
BEGIN
    GlobalUnlockPtr(lp);
    RETURN GlobalLock(GlobalReAlloc(GlobalPtrHandle(lp) , cbNew, flags));
END GlobalReAllocPtr;

PROCEDURE GlobalFreePtr(lp : ADDRESS) : BOOL;
BEGIN
    GlobalUnlockPtr(lp);
    RETURN CAST(BOOL, GlobalFree(GlobalPtrHandle(lp)));
END GlobalFreePtr;

(**********************GDI***********************)

PROCEDURE DeletePen(hpen : HPEN) : BOOL;
BEGIN
    RETURN DeleteObject(CAST(HGDIOBJ, hpen));
END DeletePen;

PROCEDURE SelectPen(hdc : HDC; hpen : HPEN) : HPEN;
BEGIN
    RETURN CAST(HPEN, SelectObject(hdc, CAST(HGDIOBJ, hpen)));
END SelectPen;

PROCEDURE GetStockPen(i : DWORD) : HPEN;
BEGIN
    RETURN CAST(HPEN, GetStockObject(i));
END GetStockPen;

PROCEDURE GetLOGPEN(h : HPEN; VAR pen : LOGPEN) : BOOLEAN;
BEGIN
    RETURN GetObject(CAST(HGDIOBJ, h), SIZE(LOGPEN), ADR(pen))
                = SIZE(LOGPEN);
END GetLOGPEN;

PROCEDURE DeleteBrush(hbrush : HBRUSH) : BOOL;
BEGIN
    RETURN DeleteObject(CAST(HGDIOBJ, hbrush));
END DeleteBrush;

PROCEDURE SelectBrush(hdc : HDC; hbrush : HBRUSH) : HBRUSH;
BEGIN
    RETURN CAST(HBRUSH, SelectObject(hdc, CAST(HGDIOBJ, hbrush)));
END SelectBrush;

PROCEDURE GetStockBrush(i : DWORD) : HBRUSH;
BEGIN
    RETURN CAST(HBRUSH, GetStockObject(i));
END GetStockBrush;

PROCEDURE GetLOGBRUSH(h : HBRUSH; VAR br : LOGBRUSH) : BOOLEAN;
BEGIN
    RETURN GetObject(CAST(HGDIOBJ, h), SIZE(LOGBRUSH), ADR(br))
                = SIZE(LOGBRUSH);
END GetLOGBRUSH;

PROCEDURE DeleteRgn(hrgn : HRGN) : BOOL;
BEGIN
    RETURN DeleteObject(CAST(HGDIOBJ, hrgn));
END DeleteRgn;

PROCEDURE CopyRgn(hrgnDst, hrgnSrc : HRGN) : INTEGER;
BEGIN
    RETURN CombineRgn(hrgnDst, hrgnSrc, NULL_HRGN, RGN_COPY);
END CopyRgn;

PROCEDURE IntersectRgn(hrgnResult, hrgnA, hrgnB : HRGN) : INTEGER;
BEGIN
    RETURN CombineRgn(hrgnResult, hrgnA, hrgnB, RGN_AND);
END IntersectRgn;

PROCEDURE SubtractRgn(hrgnResult, hrgnA, hrgnB : HRGN) : INTEGER;
BEGIN
    RETURN CombineRgn(hrgnResult, hrgnA, hrgnB, RGN_DIFF);
END SubtractRgn;

PROCEDURE UnionRgn(hrgnResult, hrgnA, hrgnB : HRGN) : INTEGER;
BEGIN
    RETURN CombineRgn(hrgnResult, hrgnA, hrgnB, RGN_OR);
END UnionRgn;

PROCEDURE XorRgn(hrgnResult, hrgnA, hrgnB : HRGN) : INTEGER;
BEGIN
    RETURN CombineRgn(hrgnResult, hrgnA, hrgnB, RGN_XOR);
END XorRgn;

PROCEDURE DeletePalette(h : HPALETTE) : BOOL;
BEGIN
    RETURN DeleteObject(CAST(HGDIOBJ, h));
END DeletePalette;

PROCEDURE GetPaletteSize(h : HPALETTE; VAR numEntries : CARDINAL) : BOOLEAN;
VAR
    result      : CARDINAL16;
    retVal      : CARDINAL;
BEGIN
    retVal := GetObject(CAST(HGDIOBJ, h), SIZE(CARDINAL16), ADR(result));
    numEntries := result;
    RETURN retVal = 2;
END GetPaletteSize;

PROCEDURE DeleteFont(hfont : HFONT) : BOOL;
BEGIN
    RETURN DeleteObject(CAST(HGDIOBJ, hfont));
END DeleteFont;

PROCEDURE SelectFont(hdc : HDC; hfont : HFONT) : HFONT;
BEGIN
    RETURN CAST(HFONT, SelectObject(hdc, CAST(HGDIOBJ, hfont)));
END SelectFont;

PROCEDURE GetStockFont(i : DWORD) : HFONT;
BEGIN
    RETURN CAST(HFONT, GetStockObject(i));
END GetStockFont;

PROCEDURE GetLOGFONT(h : HFONT; VAR logfont : LOGFONT) : BOOLEAN;
VAR
    retVal      : CARDINAL;
BEGIN
    retVal := GetObject(CAST(HGDIOBJ, h), SIZE(LOGFONT), ADR(logfont));
    RETURN (retVal = (SIZE(LOGFONT))) OR
           (retVal = (SIZE(LOGFONT)-
                         SIZE(logfont.lfFaceName)+
                         LENGTH(logfont.lfFaceName)+1));
END GetLOGFONT;

PROCEDURE GetLOGFONTW(h : HFONT; VAR logfont : LOGFONTW) : BOOLEAN;
VAR
    retVal      : CARDINAL;
BEGIN
    retVal := GetObject(CAST(HGDIOBJ, h), SIZE(LOGFONTW), ADR(logfont));
    RETURN (retVal = (SIZE(LOGFONTW))) OR
           (retVal = (SIZE(LOGFONTW)-
                         SIZE(logfont.lfFaceName)+
                         LENGTH(logfont.lfFaceName)*SIZE(UCHAR)+2));
END GetLOGFONTW;

PROCEDURE DeleteBitmap(h : HBITMAP) : BOOL;
BEGIN
    RETURN DeleteObject(CAST(HGDIOBJ, h));
END DeleteBitmap;

PROCEDURE SelectBitmap(hdc : HDC; h : HBITMAP) : HBITMAP;
BEGIN
    RETURN CAST(HBITMAP, SelectObject(hdc, CAST(HGDIOBJ, h)));
END SelectBitmap;

PROCEDURE GetBITMAP(h : HBITMAP; VAR OUT map : BITMAP) : BOOLEAN;
BEGIN
    RETURN GetObject(CAST(HGDIOBJ, h), SIZE(BITMAP), ADR(map)) = SIZE(BITMAP);
END GetBITMAP;

PROCEDURE InsetRect(VAR INOUT lprc : RECT; dx, dy : INTEGER);
BEGIN
    InflateRect(lprc, -dx, -dy);
END InsetRect;

(************************** User *********************)

PROCEDURE ShowWindowGen(hWnd : HWND;
                     nCmdShow : INTEGER) : BOOL;
BEGIN
    IF IsCurrentThread(hWnd) THEN
        RETURN ShowWindow(hWnd, nCmdShow);
    ELSE
        RETURN ShowWindowAsync(hWnd, nCmdShow);
    END;
END ShowWindowGen;

PROCEDURE GetWindowInstance(hwnd : HWND) : HMODULE;
BEGIN
    RETURN CAST(HMODULE, GetWindowLong(hwnd, GWL_HINSTANCE));
END GetWindowInstance;

PROCEDURE GetWindowStyle(hwnd : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, GetWindowLong(hwnd, GWL_STYLE));
END GetWindowStyle;

PROCEDURE GetWindowExStyle(hwnd : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, GetWindowLong(hwnd, GWL_EXSTYLE));
END GetWindowExStyle;

PROCEDURE GetWindowOwner(hwnd : HWND) : HWND;
BEGIN
    RETURN GetWindow(hwnd, GW_OWNER);
END GetWindowOwner;

PROCEDURE GetFirstChild(hwnd : HWND) : HWND;
BEGIN
    RETURN GetTopWindow(hwnd);
END GetFirstChild;

PROCEDURE GetFirstSibling(hwnd : HWND) : HWND;
BEGIN
    RETURN GetWindow(hwnd, GW_HWNDFIRST);
END GetFirstSibling;

PROCEDURE GetLastSibling(hwnd : HWND) : HWND;
BEGIN
    RETURN GetWindow(hwnd, GW_HWNDLAST);
END GetLastSibling;

PROCEDURE GetNextSibling(hwnd : HWND) : HWND;
BEGIN
    RETURN GetWindow(hwnd, GW_HWNDNEXT);
END GetNextSibling;

PROCEDURE GetPrevSibling(hwnd : HWND) : HWND;
BEGIN
    RETURN GetWindow(hwnd, GW_HWNDPREV);
END GetPrevSibling;

PROCEDURE GetWindowID(hwnd : HWND) : INTEGER;
BEGIN
    RETURN GetDlgCtrlID(hwnd);
END GetWindowID;

PROCEDURE SetWindowRedraw(hwnd : HWND; fRedraw : BOOL);
BEGIN
    SendMessage(hwnd, WM_SETREDRAW, VAL(WPARAM, fRedraw), 0);
END SetWindowRedraw;

PROCEDURE SubclassWindow(hwnd : HWND; lpfn : WNDPROC) : WNDPROC;
BEGIN
    RETURN CAST(WNDPROC, SetWindowLongPtr(hwnd,
                                       GWL_WNDPROC,
                                       CAST(LONG_PTR, lpfn))
               );
END SubclassWindow;

PROCEDURE IsMinimized(hwnd : HWND) : BOOL;
BEGIN
    RETURN IsIconic(hwnd);
END IsMinimized;

PROCEDURE IsMaximized(hwnd : HWND) : BOOL;
BEGIN
    RETURN IsZoomed(hwnd);
END IsMaximized;

PROCEDURE IsRestored(hwnd : HWND) : BOOL;
BEGIN
    RETURN ((GetWindowStyle(hwnd) BAND (WS_MINIMIZE BOR WS_MAXIMIZE)) = 0);
END IsRestored;

PROCEDURE SetWindowFont(hwnd : HWND; hfont : HFONT; fRedraw : BOOL);
BEGIN
    SendMessage(hwnd,
                WM_SETFONT,
                CAST(WPARAM, hfont),
                MAKELPARAM(ORD(fRedraw), 0));
END SetWindowFont;

PROCEDURE GetWindowFont(hwnd : HWND) : HFONT;
BEGIN
    RETURN CAST(HFONT, SendMessage(hwnd, WM_GETFONT, 0, 0));
END GetWindowFont;

PROCEDURE MapWindowRect(hwndFrom, hwndTo : HWND; VAR lprc : RECT);
BEGIN
    MapWindowPoints(hwndFrom, hwndTo, lprc:POINT, 2);
END MapWindowRect;

PROCEDURE IsLButtonDown() : BOOL;
BEGIN
  RETURN GetKeyState(VK_LBUTTON) < 0;
END IsLButtonDown;

PROCEDURE IsRButtonDown() : BOOL;
BEGIN
  RETURN GetKeyState(VK_RBUTTON) < 0;
END IsRButtonDown;

PROCEDURE IsMButtonDown() : BOOL;
BEGIN
  RETURN GetKeyState(VK_MBUTTON) < 0;
END IsMButtonDown;

PROCEDURE IsShiftKeyDown() : BOOL;
BEGIN
  RETURN GetKeyState(VK_SHIFT) < 0;
END IsShiftKeyDown;

PROCEDURE IsControlKeyDown() : BOOL;
BEGIN
  RETURN GetKeyState(VK_CONTROL) < 0;
END IsControlKeyDown;

PROCEDURE CharUpperCase(ch : CHAR) : CHAR;
BEGIN
    RETURN CharUpper(ORD(ch));
END CharUpperCase;

PROCEDURE CharUpperCaseA(ch : ACHAR) : ACHAR;
BEGIN
    RETURN CharUpperA(ORD(ch));
END CharUpperCaseA;

PROCEDURE CharUpperCaseW(ch : UCHAR) : UCHAR;
BEGIN
    RETURN CharUpperW(ORD(ch));
END CharUpperCaseW;

PROCEDURE CharLowerCase(ch : CHAR) : CHAR;
BEGIN
    RETURN CharLower(ORD(ch));
END CharLowerCase;

PROCEDURE CharLowerCaseA(ch : ACHAR) : ACHAR;
BEGIN
    RETURN CharLowerA(ORD(ch));
END CharLowerCaseA;

PROCEDURE CharLowerCaseW(ch : UCHAR) : UCHAR;
BEGIN
    RETURN CharLowerW(ORD(ch));
END CharLowerCaseW;

PROCEDURE SubclassDialog(hwndDlg : HWND; lpfn : DLGPROC) : DLGPROC;
BEGIN
    RETURN CAST(DLGPROC, SetWindowLongPtr(hwndDlg, DWLP_DLGPROC,
                CAST(LONG_PTR, lpfn)));
END SubclassDialog;

PROCEDURE DialogBoxId(a : HINSTANCE;
                      idNum : CARDINAL;
                      hWndParent : HWND;
                      lpDialogFunc : DLGPROC) : INTEGER;
BEGIN
    RETURN DialogBoxParamId(a, idNum, hWndParent, lpDialogFunc, 0);
END DialogBoxId;

PROCEDURE DialogBoxParamId(a : HINSTANCE;
                           idNum : CARDINAL;
                           hWndParent : HWND;
                           lpDialogFunc : DLGPROC;
                           dwInitParam : LPARAM) : INTEGER;
VAR
    dialog      : LPTSTR;
BEGIN
    dialog := MAKEADR(idNum);
    RETURN DialogBoxParam(a,
                          dialog^,
                          hWndParent,
                          lpDialogFunc,
                          dwInitParam);
END DialogBoxParamId;

PROCEDURE LoadMenuId(inst : HINSTANCE; idNum : CARDINAL) : HMENU;
VAR
    ptr         : LPTSTR;
BEGIN
    ptr := MAKEADR(idNum);
    RETURN LoadMenu(inst, ptr^);
END LoadMenuId;

PROCEDURE LoadAcceleratorsId(inst : HINSTANCE; idNum : CARDINAL) : HACCEL;
VAR
    ptr         : LPTSTR;
BEGIN
    ptr := MAKEADR(idNum);
    RETURN LoadAccelerators(inst, ptr^);
END LoadAcceleratorsId;

PROCEDURE LoadIconId(inst : HINSTANCE; idNum : CARDINAL) : HICON;
VAR
    ptr         : LPTSTR;
BEGIN
    ptr := MAKEADR(idNum);
    RETURN LoadIcon(inst, ptr^);
END LoadIconId;

PROCEDURE LoadImageId(a: HINSTANCE;
                      idNum: CARDINAL;
                      c: UINT;
                      d: WINT;
                      e: WINT;
                      f: UINT) : HANDLE;
VAR
    ptr         : LPTSTR;
BEGIN
    ptr := MAKEADR(idNum);
    RETURN LoadImage(a, ptr^, c, d, e, f);
END LoadImageId;

PROCEDURE LoadCursorId(inst : HINSTANCE; idNum : CARDINAL) : HCURSOR;
VAR
    ptr         : LPTSTR;
BEGIN
    ptr := MAKEADR(idNum);
    RETURN LoadCursor(inst, ptr^);
END LoadCursorId;

PROCEDURE LoadBitmapId(inst : HINSTANCE; idNum : CARDINAL) : HBITMAP;
VAR
    ptr         : LPTSTR;
BEGIN
    ptr := MAKEADR(idNum);
    RETURN LoadBitmap(inst, ptr^);
END LoadBitmapId;

PROCEDURE GetClassExtra ( hwnd : HWND ) : CARDINAL;
BEGIN
    RETURN GetClassLong(hwnd, GCL_CBCLSEXTRA);
END GetClassExtra;

PROCEDURE GetClassWindowExtra ( hwnd : HWND ) : CARDINAL;
BEGIN
    RETURN GetClassLong(hwnd, GCL_CBWNDEXTRA);
END GetClassWindowExtra;

PROCEDURE GetClassHandle( hWnd  : HWND;
                          ofs   : WINT ) : ULONG_PTR;

BEGIN
    RETURN GetClassLongPtr( hWnd, ofs );
END GetClassHandle;

PROCEDURE GetClassBackgroundBrush ( hwnd : HWND ) : HBRUSH;
BEGIN
  RETURN CAST( HBRUSH, GetClassHandle(hwnd, GCL_HBRBACKGROUND));
END GetClassBackgroundBrush;

PROCEDURE SetClassBackgroundBrush ( hwnd : HWND; hbr : HBRUSH );
BEGIN
    SetClassLongPtr(hwnd, GCL_HBRBACKGROUND, CAST( LONG_PTR, hbr ) );
END SetClassBackgroundBrush;

PROCEDURE GetClassCursor ( hwnd : HWND ) : HCURSOR;
BEGIN
  RETURN CAST( HCURSOR,GetClassHandle(hwnd, GCL_HCURSOR));
END GetClassCursor;

PROCEDURE GetClassIcon ( hwnd : HWND ) : HICON;
BEGIN
  RETURN CAST( HICON,GetClassHandle(hwnd, GCL_HICON));
END GetClassIcon;

PROCEDURE GetClassModule ( hwnd : HWND ) : HMODULE;
BEGIN
  RETURN CAST( HMODULE,GetClassHandle(hwnd, GCL_HMODULE));
END GetClassModule;

PROCEDURE GetClassStyle ( hwnd : HWND ) : CARDINAL;
BEGIN
  RETURN GetClassLong(hwnd, GCL_STYLE);
END GetClassStyle;

PROCEDURE GetClassWindowProc ( hwnd : HWND ) : WNDPROC;
BEGIN
  RETURN CAST( WNDPROC, GetClassLongPtr(hwnd, GCL_WNDPROC));
END GetClassWindowProc;

PROCEDURE SetClassWindowProc ( hwnd : HWND; proc : WNDPROC );
BEGIN
  SetClassLongPtr(hwnd, GCL_WNDPROC, CAST( LONG_PTR, proc ) );
END SetClassWindowProc;

PROCEDURE SetWindowWindowProc(hwnd : HWND; proc : WNDPROC );
BEGIN
    SetWindowLongPtr( hwnd, GWL_WNDPROC, CAST( LONG_PTR, proc ) );
END SetWindowWindowProc;

PROCEDURE CreateDialogId(a : HINSTANCE; b : INTEGER32; c : HWND; d : DLGPROC) : HWND;
VAR
    res : LPWSTR;
BEGIN
    res := MAKEINTRESOURCEW(b);
    RETURN CreateDialogParamW(a, res^, c, d, 0);
END CreateDialogId;

PROCEDURE LoadDefaultIcon ( idc : LPTSTR ) : HICON;(*ok*)
  BEGIN
    RETURN LoadIcon(NULL_HINSTANCE,idc^);
  END LoadDefaultIcon;

PROCEDURE LoadDefaultCursor ( idc : LPTSTR ) : HCURSOR;(*ok*)
  BEGIN
    RETURN LoadCursor(NULL_HINSTANCE,idc^);
  END LoadDefaultCursor;

PROCEDURE SetWindowExStyle(hwnd : HWND; style : DWORD );
BEGIN
  SetWindowLong(hwnd, GWL_EXSTYLE, style );
END SetWindowExStyle;

PROCEDURE SetWindowStyle(hwnd : HWND; style : DWORD );

BEGIN
  SetWindowLong(hwnd, GWL_STYLE, style );
END SetWindowStyle;

PROCEDURE GetStockPalette(i : DWORD) : HPALETTE;
BEGIN
    RETURN CAST(HPALETTE, GetStockObject(i));
END GetStockPalette;

(************* Static controls************************************)

PROCEDURE IndexMsg(hwnd : HWND;
                   msg : UINT;
                   wParam : WPARAM;
                   lParam : LPARAM;
                   VAR OUT Index : UINT) : BOOL;
VAR
    ret : LRESULT;
BEGIN
    Index := MAX(UINT);
    ret := SendMessage(hwnd, msg, wParam, lParam);
    IF ret >= 0 THEN
        Index := ret;
        RETURN TRUE;
    END;
    RETURN FALSE;
END IndexMsg;

PROCEDURE IndexMsgA(hwnd : HWND;
                   msg : UINT;
                   wParam : WPARAM;
                   lParam : LPARAM;
                   VAR OUT Index : UINT) : BOOL;
VAR
    ret : LRESULT;
BEGIN
    Index := MAX(UINT);
    ret := SendMessageA(hwnd, msg, wParam, lParam);
    IF ret >= 0 THEN
        Index := ret;
        RETURN TRUE;
    END;
    RETURN FALSE;
END IndexMsgA;

PROCEDURE IndexMsgW(hwnd : HWND;
                   msg : UINT;
                   wParam : WPARAM;
                   lParam : LPARAM;
                   VAR OUT Index : UINT) : BOOL;
VAR
    ret : LRESULT;
BEGIN
    Index := MAX(UINT);
    ret := SendMessageW(hwnd, msg, wParam, lParam);
    IF ret >= 0 THEN
        Index := ret;
        RETURN TRUE;
    END;
    RETURN FALSE;
END IndexMsgW;

PROCEDURE LenMsg(hwnd : HWND; msg : UINT; wParam : WPARAM; lParam : LPARAM; VAR OUT len : UINT ) : BOOL;
VAR
    ret : WINT;
BEGIN
    len := 0;
    ret := SendMessage(hwnd, msg, wParam, lParam);
    IF ret >= 0 THEN
        len := ret;
        RETURN TRUE;
    END;
    RETURN FALSE;
END LenMsg;

PROCEDURE LenMsgA(hwnd : HWND; msg : UINT; wParam : WPARAM; lParam : LPARAM; VAR OUT len : UINT ) : BOOL;
VAR
    ret : WINT;
BEGIN
    len := 0;
    ret := SendMessageA(hwnd, msg, wParam, lParam);
    IF ret >= 0 THEN
        len := ret;
        RETURN TRUE;
    END;
    RETURN FALSE;
END LenMsgA;

PROCEDURE LenMsgW(hwnd : HWND; msg : UINT; wParam : WPARAM; lParam : LPARAM; VAR OUT len : UINT ) : BOOL;
VAR
    ret : WINT;
BEGIN
    len := 0;
    ret := SendMessageW(hwnd, msg, wParam, lParam);
    IF ret >= 0 THEN
        len := ret;
        RETURN TRUE;
    END;
    RETURN FALSE;
END LenMsgW;

PROCEDURE IsCurrentThread(hw : HWND) : BOOL;
VAR
        procId : DWORD;
BEGIN
        IF GetCurrentThreadId() = GetWindowThreadProcessId(hw, procId) THEN
                RETURN TRUE;
        ELSE
                RETURN FALSE;
        END;
END IsCurrentThread;

PROCEDURE Static_Enable(hwndCtl : HWND; fEnable : BOOL) : BOOL;
BEGIN
    RETURN EnableWindow(hwndCtl, fEnable);
END Static_Enable;

PROCEDURE Static_GetText(hwndCtl : HWND;
                         VAR lpch : ARRAY OF CHAR;
                         cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowText(hwndCtl, lpch, cchMax);
END Static_GetText;

PROCEDURE Static_GetTextA(hwndCtl : HWND;
                         VAR lpch : ARRAY OF ACHAR;
                         cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowTextA(hwndCtl, lpch, cchMax);
END Static_GetTextA;

PROCEDURE Static_GetTextW(hwndCtl : HWND;
                         VAR lpch : ARRAY OF UCHAR;
                         cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowTextW(hwndCtl, lpch, cchMax);
END Static_GetTextW;

PROCEDURE Static_GetTextLength(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLength(hwndCtl);
END Static_GetTextLength;

PROCEDURE Static_GetTextLengthA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLengthA(hwndCtl);
END Static_GetTextLengthA;

PROCEDURE Static_GetTextLengthW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLengthW(hwndCtl);
END Static_GetTextLengthW;

PROCEDURE Static_SetText(hwndCtl : HWND; lpsz : ARRAY OF CHAR) : BOOL;
BEGIN
    RETURN SetWindowText(hwndCtl, lpsz);
END Static_SetText;

PROCEDURE Static_SetTextA(hwndCtl : HWND; lpsz : ARRAY OF ACHAR) : BOOL;
BEGIN
    RETURN SetWindowTextA(hwndCtl, lpsz);
END Static_SetTextA;

PROCEDURE Static_SetTextW(hwndCtl : HWND; lpsz : ARRAY OF UCHAR) : BOOL;
BEGIN
    RETURN SetWindowTextW(hwndCtl, lpsz);
END Static_SetTextW;

PROCEDURE Static_SetIcon(hwndCtl : HWND; hIcon : HICON) : HICON;
BEGIN
    RETURN CAST(HICON, SendMessage(hwndCtl, STM_SETICON,
                                   CAST(WPARAM,hIcon), 0));
END Static_SetIcon;

PROCEDURE Static_GetIcon(hwndCtl : HWND) : HICON;
BEGIN
    RETURN CAST(HICON,SendMessage(hwndCtl, STM_GETICON, 0, 0));
END Static_GetIcon;

PROCEDURE Static_SetBitmap(hwndCtl : HWND; hIcon : HBITMAP) : HBITMAP;
BEGIN
    RETURN CAST(HBITMAP, SendMessage(hwndCtl, STM_SETIMAGE,
                                   IMAGE_BITMAP, CAST(WPARAM,hIcon)));
END Static_SetBitmap;

PROCEDURE Static_GetBitmap(hwndCtl : HWND) : HBITMAP;
BEGIN
    RETURN CAST(HBITMAP,SendMessage(hwndCtl, STM_GETIMAGE, IMAGE_BITMAP, 0));
END Static_GetBitmap;

(*********** Buttons ************)
PROCEDURE Button_Enable(hwndCtl : HWND; fEnable : BOOL) : BOOL;
BEGIN
    RETURN EnableWindow(hwndCtl, fEnable);
END Button_Enable;

PROCEDURE Button_GetText(hwndCtl : HWND;
                         VAR lpch : ARRAY OF CHAR;
                         cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowText(hwndCtl, lpch, cchMax);
END Button_GetText;

PROCEDURE Button_GetTextA(hwndCtl : HWND;
                         VAR lpch : ARRAY OF ACHAR;
                         cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowTextA(hwndCtl, lpch, cchMax);
END Button_GetTextA;

PROCEDURE Button_GetTextW(hwndCtl : HWND;
                         VAR lpch : ARRAY OF UCHAR;
                         cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowTextW(hwndCtl, lpch, cchMax);
END Button_GetTextW;

PROCEDURE Button_GetTextLength(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLength(hwndCtl);
END Button_GetTextLength;

PROCEDURE Button_GetTextLengthA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLengthA(hwndCtl);
END Button_GetTextLengthA;

PROCEDURE Button_GetTextLengthW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLengthW(hwndCtl);
END Button_GetTextLengthW;

PROCEDURE Button_SetText(hwndCtl : HWND; lpsz : ARRAY OF CHAR) : BOOL;
BEGIN
    RETURN SetWindowText(hwndCtl, lpsz);
END Button_SetText;

PROCEDURE Button_SetTextA(hwndCtl : HWND; lpsz : ARRAY OF ACHAR) : BOOL;
BEGIN
    RETURN SetWindowTextA(hwndCtl, lpsz);
END Button_SetTextA;

PROCEDURE Button_SetTextW(hwndCtl : HWND; lpsz : ARRAY OF UCHAR) : BOOL;
BEGIN
    RETURN SetWindowTextW(hwndCtl, lpsz);
END Button_SetTextW;

PROCEDURE Button_GetCheck(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, BM_GETCHECK, 0, 0));
END Button_GetCheck;

PROCEDURE Button_SetCheck(hwndCtl : HWND; check : UINT);
BEGIN
    SendMessage(hwndCtl, BM_SETCHECK, check, 0);
END Button_SetCheck;

PROCEDURE Button_Click(hwndCtl : HWND);
BEGIN
    SendMessage(hwndCtl, BM_CLICK, 0, 0);
END Button_Click;

PROCEDURE Button_GetState(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, BM_GETSTATE, 0, 0));
END Button_GetState;

PROCEDURE Button_SetState(hwndCtl : HWND; state : INTEGER) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwndCtl,
                                  BM_SETSTATE,
                                  VAL(WPARAM, state),
                                  0)
                );
END Button_SetState;

PROCEDURE Button_SetStyle(hwndCtl : HWND; style : DWORD; fRedraw : BOOL);
BEGIN
    SendMessage(hwndCtl,
                BM_SETSTYLE,
                VAL(WPARAM, ORD(LOWORD(style))),
                MAKELPARAM(ORD(fRedraw), 0));
END Button_SetStyle;

PROCEDURE Button_SetImage_Bitmap(hwndCtl : HWND; image : HBITMAP) : HBITMAP;
BEGIN
    RETURN CAST(HBITMAP,
                SendMessage(hwndCtl,
                            BM_SETIMAGE,
                            IMAGE_BITMAP,
                            CAST(LPARAM, image)));
END Button_SetImage_Bitmap;

PROCEDURE Button_SetImage_Icon(hwndCtl : HWND; image : HICON) : HICON;
BEGIN
    RETURN CAST(HICON,
                SendMessage(hwndCtl,
                            BM_SETIMAGE,
                            IMAGE_ICON,
                            CAST(LPARAM, image)));
END Button_SetImage_Icon;

(************ Edit controls message APIs **************)
PROCEDURE Edit_Enable(hwndCtl : HWND; fEnable : BOOL) : BOOL;
BEGIN
    RETURN EnableWindow(hwndCtl, fEnable);
END Edit_Enable;

PROCEDURE Edit_GetText(hwndCtl : HWND;
                       VAR OUT lpch : ARRAY OF CHAR;
                       cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowText(hwndCtl, lpch, cchMax);
END Edit_GetText;

PROCEDURE Edit_GetTextA(hwndCtl : HWND;
                       VAR OUT lpch : ARRAY OF ACHAR;
                       cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowTextA(hwndCtl, lpch, cchMax);
END Edit_GetTextA;

PROCEDURE Edit_GetTextW(hwndCtl : HWND;
                       VAR OUT lpch : ARRAY OF UCHAR;
                       cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowTextW(hwndCtl, lpch, cchMax);
END Edit_GetTextW;

PROCEDURE Edit_GetTextLength(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLength(hwndCtl);
END Edit_GetTextLength;

PROCEDURE Edit_GetTextLengthA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLengthA(hwndCtl);
END Edit_GetTextLengthA;

PROCEDURE Edit_GetTextLengthW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLengthW(hwndCtl);
END Edit_GetTextLengthW;

PROCEDURE Edit_SetText(hwndCtl : HWND; lpsz : ARRAY OF CHAR) : BOOL;
BEGIN
    RETURN SetWindowText(hwndCtl, lpsz);
END Edit_SetText;

PROCEDURE Edit_SetTextA(hwndCtl : HWND; lpsz : ARRAY OF ACHAR) : BOOL;
BEGIN
    RETURN SetWindowTextA(hwndCtl, lpsz);
END Edit_SetTextA;

PROCEDURE Edit_SetTextW(hwndCtl : HWND; lpsz : ARRAY OF UCHAR) : BOOL;
BEGIN
    RETURN SetWindowTextW(hwndCtl, lpsz);
END Edit_SetTextW;

PROCEDURE Edit_LimitText(hwndCtl : HWND; cchMax : DWORD);
BEGIN
    SendMessage(hwndCtl, EM_LIMITTEXT, VAL(WPARAM, cchMax), 0);
END Edit_LimitText;

PROCEDURE Edit_LimitTextA(hwndCtl : HWND; cchMax : DWORD);
BEGIN
    SendMessageA(hwndCtl, EM_LIMITTEXT, VAL(WPARAM, cchMax), 0);
END Edit_LimitTextA;

PROCEDURE Edit_LimitTextW(hwndCtl : HWND; cchMax : DWORD);
BEGIN
    SendMessageW(hwndCtl, EM_LIMITTEXT, VAL(WPARAM, cchMax), 0);
END Edit_LimitTextW;

PROCEDURE Edit_GetLineCount(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN SendMessage(hwndCtl, EM_GETLINECOUNT, 0, 0);
END Edit_GetLineCount;

PROCEDURE Edit_GetLineCountA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN SendMessageA(hwndCtl, EM_GETLINECOUNT, 0, 0);
END Edit_GetLineCountA;

PROCEDURE Edit_GetLineCountW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN SendMessageW(hwndCtl, EM_GETLINECOUNT, 0, 0);
END Edit_GetLineCountW;

PROCEDURE Edit_GetLine(hwndCtl : HWND;
                       line : DWORD;
                       VAR lpch : ARRAY OF CHAR;
                       cchMax : DWORD) : INTEGER;
BEGIN
    lpch:LPARAM := cchMax;
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                    EM_GETLINE,
                                    VAL(WPARAM, line),
                                    CAST(LPARAM, ADR(lpch)))
                );
END Edit_GetLine;

PROCEDURE Edit_GetLineA(hwndCtl : HWND;
                       line : DWORD;
                       VAR lpch : ARRAY OF ACHAR;
                       cchMax : DWORD) : INTEGER;
BEGIN
    lpch:LPARAM := cchMax;
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                    EM_GETLINE,
                                    VAL(WPARAM, line),
                                    CAST(LPARAM, ADR(lpch)))
                );
END Edit_GetLineA;

PROCEDURE Edit_GetLineW(hwndCtl : HWND;
                        line : DWORD;
                        VAR lpch : ARRAY OF UCHAR;
                        cchMax : DWORD) : INTEGER;
BEGIN
    lpch:LPARAM := cchMax;
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     EM_GETLINE,
                                     VAL(WPARAM, line),
                                     CAST(LPARAM, ADR(lpch)))
                );
END Edit_GetLineW;

PROCEDURE Edit_GetRect(hwndCtl : HWND; VAR lprc : RECT);
BEGIN
    SendMessage(hwndCtl, EM_GETRECT, 0, CAST(LPARAM, ADR(lprc)));
END Edit_GetRect;

PROCEDURE Edit_GetRectA(hwndCtl : HWND; VAR lprc : RECT);
BEGIN
    SendMessageA(hwndCtl, EM_GETRECT, 0, CAST(LPARAM, ADR(lprc)));
END Edit_GetRectA;

PROCEDURE Edit_GetRectW(hwndCtl : HWND; VAR lprc : RECT);
BEGIN
    SendMessageW(hwndCtl, EM_GETRECT, 0, CAST(LPARAM, ADR(lprc)));
END Edit_GetRectW;

PROCEDURE Edit_SetRect(hwndCtl : HWND; lprc : RECT);
BEGIN
    SendMessage(hwndCtl, EM_SETRECT, 0, CAST(LPARAM,ADR(lprc)));
END Edit_SetRect;

PROCEDURE Edit_SetRectA(hwndCtl : HWND; lprc : RECT);
BEGIN
    SendMessageA(hwndCtl, EM_SETRECT, 0, CAST(LPARAM,ADR(lprc)));
END Edit_SetRectA;

PROCEDURE Edit_SetRectW(hwndCtl : HWND; lprc : RECT);
BEGIN
    SendMessageW(hwndCtl, EM_SETRECT, 0, CAST(LPARAM,ADR(lprc)));
END Edit_SetRectW;

PROCEDURE Edit_SetRectNoPaint(hwndCtl : HWND; lprc : RECT);
BEGIN
    SendMessage(hwndCtl, EM_SETRECTNP, 0, CAST(LPARAM, ADR(lprc)));
END Edit_SetRectNoPaint;

PROCEDURE Edit_SetRectNoPaintA(hwndCtl : HWND; lprc : RECT);
BEGIN
    SendMessageA(hwndCtl, EM_SETRECTNP, 0, CAST(LPARAM, ADR(lprc)));
END Edit_SetRectNoPaintA;

PROCEDURE Edit_SetRectNoPaintW(hwndCtl : HWND; lprc : RECT);
BEGIN
    SendMessageW(hwndCtl, EM_SETRECTNP, 0, CAST(LPARAM, ADR(lprc)));
END Edit_SetRectNoPaintW;

PROCEDURE Edit_GetSel(hwndCtl : HWND; VAR OUT startPos, endPos : CARDINAL);
BEGIN
    SendMessage(hwndCtl,
                EM_GETSEL,
                CAST(WPARAM, ADR(startPos)),
                CAST(LPARAM, ADR(endPos)));
END Edit_GetSel;

PROCEDURE Edit_GetSelA(hwndCtl : HWND; VAR OUT startPos, endPos : CARDINAL);
BEGIN
    SendMessageA(hwndCtl,
                EM_GETSEL,
                CAST(WPARAM, ADR(startPos)),
                CAST(LPARAM, ADR(endPos)));
END Edit_GetSelA;

PROCEDURE Edit_GetSelW(hwndCtl : HWND; VAR OUT startPos, endPos : CARDINAL);
BEGIN
    SendMessageW(hwndCtl,
                EM_GETSEL,
                CAST(WPARAM, ADR(startPos)),
                CAST(LPARAM, ADR(endPos)));
END Edit_GetSelW;

PROCEDURE Edit_SetSel(hwndCtl : HWND; startPos, endPos : INTEGER);
BEGIN
    SendMessage(hwndCtl, EM_SETSEL, startPos, endPos);
END Edit_SetSel;

PROCEDURE Edit_SetSelA(hwndCtl : HWND; startPos, endPos : INTEGER);
BEGIN
    SendMessageA(hwndCtl, EM_SETSEL, startPos, endPos);
END Edit_SetSelA;

PROCEDURE Edit_SetSelW(hwndCtl : HWND; startPos, endPos : INTEGER);
BEGIN
    SendMessageW(hwndCtl, EM_SETSEL, startPos, endPos);
END Edit_SetSelW;

PROCEDURE Edit_ScrollCaret(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndCtl, EM_SCROLLCARET, 0, 0));
END Edit_ScrollCaret;

PROCEDURE Edit_ScrollCaretA(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndCtl, EM_SCROLLCARET, 0, 0));
END Edit_ScrollCaretA;

PROCEDURE Edit_ScrollCaretW(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndCtl, EM_SCROLLCARET, 0, 0));
END Edit_ScrollCaretW;

PROCEDURE Edit_ReplaceSel(hwndCtl : HWND; lpszReplace : ARRAY OF CHAR);
BEGIN
   SendMessage(hwndCtl, EM_REPLACESEL, 0, CAST(LPARAM, ADR(lpszReplace)));
END Edit_ReplaceSel;

PROCEDURE Edit_ReplaceSelA(hwndCtl : HWND; lpszReplace : ARRAY OF ACHAR);
BEGIN
   SendMessageA(hwndCtl, EM_REPLACESEL, 0, CAST(LPARAM, ADR(lpszReplace)));
END Edit_ReplaceSelA;

PROCEDURE Edit_ReplaceSelW(hwndCtl : HWND; lpszReplace : ARRAY OF UCHAR);
BEGIN
   SendMessageW(hwndCtl, EM_REPLACESEL, 0, CAST(LPARAM, ADR(lpszReplace)));
END Edit_ReplaceSelW;

PROCEDURE Edit_GetModify(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndCtl, EM_GETMODIFY, 0, 0));
END Edit_GetModify;

PROCEDURE Edit_GetModifyA(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndCtl, EM_GETMODIFY, 0, 0));
END Edit_GetModifyA;

PROCEDURE Edit_GetModifyW(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndCtl, EM_GETMODIFY, 0, 0));
END Edit_GetModifyW;

PROCEDURE Edit_SetModify(hwndCtl : HWND; fModified : BOOL);
BEGIN
    SendMessage(hwndCtl, EM_SETMODIFY, VAL(WPARAM, fModified), 0);
END Edit_SetModify;

PROCEDURE Edit_SetModifyA(hwndCtl : HWND; fModified : BOOL);
BEGIN
    SendMessageA(hwndCtl, EM_SETMODIFY, VAL(WPARAM, fModified), 0);
END Edit_SetModifyA;

PROCEDURE Edit_SetModifyW(hwndCtl : HWND; fModified : BOOL);
BEGIN
    SendMessageW(hwndCtl, EM_SETMODIFY, VAL(WPARAM, fModified), 0);
END Edit_SetModifyW;

PROCEDURE Edit_LineFromChar(hwndCtl : HWND; ich : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     EM_LINEFROMCHAR,
                                     VAL(WPARAM,ich),
                                     0)
                );
END Edit_LineFromChar;

PROCEDURE Edit_LineFromCharA(hwndCtl : HWND; ich : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     EM_LINEFROMCHAR,
                                     VAL(WPARAM,ich),
                                     0)
                );
END Edit_LineFromCharA;

PROCEDURE Edit_LineFromCharW(hwndCtl : HWND; ich : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     EM_LINEFROMCHAR,
                                     VAL(WPARAM,ich),
                                     0)
                );
END Edit_LineFromCharW;

PROCEDURE Edit_LineIndex(hwndCtl : HWND; line : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     EM_LINEINDEX,
                                     VAL(WPARAM,line),
                                     0)
                );
END Edit_LineIndex;

PROCEDURE Edit_LineIndexA(hwndCtl : HWND; line : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     EM_LINEINDEX,
                                     VAL(WPARAM,line),
                                     0)
                );
END Edit_LineIndexA;

PROCEDURE Edit_LineIndexW(hwndCtl : HWND; line : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     EM_LINEINDEX,
                                     VAL(WPARAM,line),
                                     0)
                );
END Edit_LineIndexW;

PROCEDURE Edit_LineLength(hwndCtl : HWND; line : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     EM_LINELENGTH,
                                     VAL(WPARAM,line),
                                     0)
               );
END Edit_LineLength;

PROCEDURE Edit_LineLengthA(hwndCtl : HWND; line : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     EM_LINELENGTH,
                                     VAL(WPARAM,line),
                                     0)
               );
END Edit_LineLengthA;

PROCEDURE Edit_LineLengthW(hwndCtl : HWND; line : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     EM_LINELENGTH,
                                     VAL(WPARAM,line),
                                     0)
               );
END Edit_LineLengthW;

PROCEDURE Edit_Scroll(hwndCtl : HWND; dv, dh : INTEGER);
BEGIN
    SendMessage(hwndCtl,
                EM_LINESCROLL,
                VAL(WPARAM,dh),
                VAL(LPARAM, dv));
END Edit_Scroll;

PROCEDURE Edit_ScrollA(hwndCtl : HWND; dv, dh : INTEGER);
BEGIN
    SendMessageA(hwndCtl,
                EM_LINESCROLL,
                VAL(WPARAM,dh),
                VAL(LPARAM, dv));
END Edit_ScrollA;

PROCEDURE Edit_ScrollW(hwndCtl : HWND; dv, dh : INTEGER);
BEGIN
    SendMessageW(hwndCtl,
                EM_LINESCROLL,
                VAL(WPARAM,dh),
                VAL(LPARAM, dv));
END Edit_ScrollW;

PROCEDURE Edit_CanUndo(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL,SendMessage(hwndCtl, EM_CANUNDO, 0, 0));
END Edit_CanUndo;

PROCEDURE Edit_CanUndoA(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL,SendMessageA(hwndCtl, EM_CANUNDO, 0, 0));
END Edit_CanUndoA;

PROCEDURE Edit_CanUndoW(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL,SendMessageW(hwndCtl, EM_CANUNDO, 0, 0));
END Edit_CanUndoW;

PROCEDURE Edit_Undo(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndCtl, EM_UNDO, 0, 0));
END Edit_Undo;

PROCEDURE Edit_UndoA(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndCtl, EM_UNDO, 0, 0));
END Edit_UndoA;

PROCEDURE Edit_UndoW(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndCtl, EM_UNDO, 0, 0));
END Edit_UndoW;

PROCEDURE Edit_EmptyUndoBuffer(hwndCtl : HWND);
BEGIN
    SendMessage(hwndCtl, EM_EMPTYUNDOBUFFER, 0, 0);
END Edit_EmptyUndoBuffer;

PROCEDURE Edit_EmptyUndoBufferA(hwndCtl : HWND);
BEGIN
    SendMessageA(hwndCtl, EM_EMPTYUNDOBUFFER, 0, 0);
END Edit_EmptyUndoBufferA;

PROCEDURE Edit_EmptyUndoBufferW(hwndCtl : HWND);
BEGIN
    SendMessageW(hwndCtl, EM_EMPTYUNDOBUFFER, 0, 0);
END Edit_EmptyUndoBufferW;

PROCEDURE Edit_SetPasswordChar(hwndCtl : HWND; ch : CHAR);
BEGIN
    SendMessage(hwndCtl, EM_SETPASSWORDCHAR, VAL(WPARAM, ORD(ch)), 0);
END Edit_SetPasswordChar;

PROCEDURE Edit_SetPasswordCharA(hwndCtl : HWND; ch : ACHAR);
BEGIN
    SendMessageA(hwndCtl, EM_SETPASSWORDCHAR, VAL(WPARAM, ORD(ch)), 0);
END Edit_SetPasswordCharA;

PROCEDURE Edit_SetPasswordCharW(hwndCtl : HWND; ch : UCHAR);
BEGIN
    SendMessageW(hwndCtl, EM_SETPASSWORDCHAR, VAL(WPARAM, ORD(ch)), 0);
END Edit_SetPasswordCharW;

PROCEDURE Edit_SetTabStops(hwndCtl : HWND;
                           cTabs : DWORD;
                           lpTabs : ARRAY OF DWORD);
BEGIN
    SendMessage(hwndCtl,
                EM_SETTABSTOPS,
                VAL(WPARAM, cTabs),
                CAST(LPARAM, ADR(lpTabs)));
END Edit_SetTabStops;

PROCEDURE Edit_SetTabStopsA(hwndCtl : HWND;
                           cTabs : DWORD;
                           lpTabs : ARRAY OF DWORD);
BEGIN
    SendMessageA(hwndCtl,
                EM_SETTABSTOPS,
                VAL(WPARAM, cTabs),
                CAST(LPARAM, ADR(lpTabs)));
END Edit_SetTabStopsA;

PROCEDURE Edit_SetTabStopsW(hwndCtl : HWND;
                           cTabs : DWORD;
                           lpTabs : ARRAY OF DWORD);
BEGIN
    SendMessageW(hwndCtl,
                EM_SETTABSTOPS,
                VAL(WPARAM, cTabs),
                CAST(LPARAM, ADR(lpTabs)));
END Edit_SetTabStopsW;

PROCEDURE Edit_FmtLines(hwndCtl : HWND; fAddEOL : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndCtl,
                                  EM_FMTLINES,
                                  VAL(WPARAM, fAddEOL),
                                  0)
               );
END Edit_FmtLines;

PROCEDURE Edit_FmtLinesA(hwndCtl : HWND; fAddEOL : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndCtl,
                                  EM_FMTLINES,
                                  VAL(WPARAM, fAddEOL),
                                  0)
               );
END Edit_FmtLinesA;

PROCEDURE Edit_FmtLinesW(hwndCtl : HWND; fAddEOL : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndCtl,
                                  EM_FMTLINES,
                                  VAL(WPARAM, fAddEOL),
                                  0)
               );
END Edit_FmtLinesW;

PROCEDURE Edit_GetHandle(hwndCtl : HWND) : HLOCAL;
BEGIN
    RETURN CAST(HLOCAL, SendMessage(hwndCtl, EM_GETHANDLE, 0, 0));
END Edit_GetHandle;

PROCEDURE Edit_GetHandleA(hwndCtl : HWND) : HLOCAL;
BEGIN
    RETURN CAST(HLOCAL, SendMessageA(hwndCtl, EM_GETHANDLE, 0, 0));
END Edit_GetHandleA;

PROCEDURE Edit_GetHandleW(hwndCtl : HWND) : HLOCAL;
BEGIN
    RETURN CAST(HLOCAL, SendMessageW(hwndCtl, EM_GETHANDLE, 0, 0));
END Edit_GetHandleW;

PROCEDURE Edit_SetHandle(hwndCtl : HWND; h : HLOCAL);
BEGIN
    SendMessage(hwndCtl, EM_SETHANDLE, CAST(WPARAM, h), 0);
END Edit_SetHandle;

PROCEDURE Edit_SetHandleA(hwndCtl : HWND; h : HLOCAL);
BEGIN
    SendMessageA(hwndCtl, EM_SETHANDLE, CAST(WPARAM, h), 0);
END Edit_SetHandleA;

PROCEDURE Edit_SetHandleW(hwndCtl : HWND; h : HLOCAL);
BEGIN
    SendMessageW(hwndCtl, EM_SETHANDLE, CAST(WPARAM, h), 0);
END Edit_SetHandleW;

PROCEDURE Edit_GetFirstVisibleLine(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     EM_GETFIRSTVISIBLELINE,
                                     0,
                                     0)
               );
END Edit_GetFirstVisibleLine;

PROCEDURE Edit_GetFirstVisibleLineA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     EM_GETFIRSTVISIBLELINE,
                                     0,
                                     0)
               );
END Edit_GetFirstVisibleLineA;

PROCEDURE Edit_GetFirstVisibleLineW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     EM_GETFIRSTVISIBLELINE,
                                     0,
                                     0)
               );
END Edit_GetFirstVisibleLineW;

PROCEDURE Edit_SetReadOnly(hwndCtl : HWND; fReadOnly : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndCtl,
                                  EM_SETREADONLY,
                                  VAL(WPARAM, fReadOnly),
                                  0)
               );
END Edit_SetReadOnly;

PROCEDURE Edit_SetReadOnlyA(hwndCtl : HWND; fReadOnly : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndCtl,
                                  EM_SETREADONLY,
                                  VAL(WPARAM, fReadOnly),
                                  0)
               );
END Edit_SetReadOnlyA;

PROCEDURE Edit_SetReadOnlyW(hwndCtl : HWND; fReadOnly : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndCtl,
                                  EM_SETREADONLY,
                                  VAL(WPARAM, fReadOnly),
                                  0)
               );
END Edit_SetReadOnlyW;

PROCEDURE Edit_GetPasswordChar(hwndCtl : HWND) : CHAR;
BEGIN
    RETURN CHR(SendMessage(hwndCtl, EM_GETPASSWORDCHAR, 0, 0));
END Edit_GetPasswordChar;

PROCEDURE Edit_GetPasswordCharA(hwndCtl : HWND) : ACHAR;
BEGIN
    RETURN ACHR(SendMessageA(hwndCtl, EM_GETPASSWORDCHAR, 0, 0));
END Edit_GetPasswordCharA;

PROCEDURE Edit_GetPasswordCharW(hwndCtl : HWND) : UCHAR;
BEGIN
    RETURN UCHR(SendMessageW(hwndCtl, EM_GETPASSWORDCHAR, 0, 0));
END Edit_GetPasswordCharW;

PROCEDURE Edit_SetWordBreakProc(hwndCtl : HWND;
                                lpfnWordBreak : EDITWORDBREAKPROC);
BEGIN
    SendMessage(hwndCtl,
                EM_SETWORDBREAKPROC,
                0,
                CAST(LPARAM, lpfnWordBreak));
END Edit_SetWordBreakProc;

PROCEDURE Edit_SetWordBreakProcA(hwndCtl : HWND;
                                lpfnWordBreak : EDITWORDBREAKPROCA);
BEGIN
    SendMessageA(hwndCtl,
                EM_SETWORDBREAKPROC,
                0,
                CAST(LPARAM, lpfnWordBreak));
END Edit_SetWordBreakProcA;

PROCEDURE Edit_SetWordBreakProcW(hwndCtl : HWND;
                                lpfnWordBreak : EDITWORDBREAKPROCW);
BEGIN
    SendMessageW(hwndCtl,
                EM_SETWORDBREAKPROC,
                0,
                CAST(LPARAM, lpfnWordBreak));
END Edit_SetWordBreakProcW;

PROCEDURE Edit_GetWordBreakProc(hwndCtl : HWND) : EDITWORDBREAKPROC;
BEGIN
    RETURN CAST(EDITWORDBREAKPROC, SendMessage(hwndCtl,
                                               EM_GETWORDBREAKPROC,
                                               0, 0)
                );
END Edit_GetWordBreakProc;

PROCEDURE Edit_GetWordBreakProcA(hwndCtl : HWND) : EDITWORDBREAKPROCA;
BEGIN
    RETURN CAST(EDITWORDBREAKPROCA, SendMessageA(hwndCtl,
                                               EM_GETWORDBREAKPROC,
                                               0, 0)
                );
END Edit_GetWordBreakProcA;

PROCEDURE Edit_GetWordBreakProcW(hwndCtl : HWND) : EDITWORDBREAKPROCW;
BEGIN
    RETURN CAST(EDITWORDBREAKPROCW, SendMessageW(hwndCtl,
                                               EM_GETWORDBREAKPROC,
                                               0, 0)
                );
END Edit_GetWordBreakProcW;

(**************** Scroll Bars ****************)
PROCEDURE ScrollBar_Enable(hwndCtl : HWND; flags : INTEGER) : BOOL;
BEGIN
    RETURN EnableScrollBar(hwndCtl, SB_CTL, flags);
END ScrollBar_Enable;

PROCEDURE ScrollBar_Show(hwndCtl : HWND; fShow : BOOL) : BOOL;
BEGIN
    RETURN ShowWindow(hwndCtl, ORD(fShow));
END ScrollBar_Show;

PROCEDURE ScrollBar_SetPos(hwndCtl : HWND;
                           pos : DWORD;
                           fRedraw : BOOL) : INTEGER;
BEGIN
     RETURN SetScrollPos(hwndCtl, SB_CTL, pos, fRedraw);
END ScrollBar_SetPos;

PROCEDURE ScrollBar_GetPos(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetScrollPos(hwndCtl, SB_CTL);
END ScrollBar_GetPos;

PROCEDURE ScrollBar_SetRange(hwndCtl : HWND;
                             posMin, posMax : DWORD;
                             fRedraw : BOOL) : BOOL;
BEGIN
    RETURN SetScrollRange(hwndCtl, SB_CTL, posMin, posMax, fRedraw);
END ScrollBar_SetRange;

PROCEDURE ScrollBar_GetRange(hwndCtl : HWND;
                             VAR lpposMin : WINT;
                             VAR lpposMax : WINT) : BOOL;
BEGIN
    RETURN GetScrollRange(hwndCtl, SB_CTL, lpposMin, lpposMax);
END ScrollBar_GetRange;

(******** LIST BOXS *********************)

PROCEDURE ListBox_Enable(hwndCtl : HWND; fEnable : BOOL) : BOOL;
BEGIN
    RETURN EnableWindow(hwndCtl, fEnable);
END ListBox_Enable;

PROCEDURE ListBox_GetCount(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, LB_GETCOUNT, 0, 0));
END ListBox_GetCount;

PROCEDURE ListBox_GetCountA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl, LB_GETCOUNT, 0, 0));
END ListBox_GetCountA;

PROCEDURE ListBox_GetCountW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl, LB_GETCOUNT, 0, 0));
END ListBox_GetCountW;

PROCEDURE ListBox_ResetContent(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndCtl, LB_RESETCONTENT, 0, 0));
END ListBox_ResetContent;

PROCEDURE ListBox_ResetContentA(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndCtl, LB_RESETCONTENT, 0, 0));
END ListBox_ResetContentA;

PROCEDURE ListBox_ResetContentW(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndCtl, LB_RESETCONTENT, 0, 0));
END ListBox_ResetContentW;

PROCEDURE ListBox_AddString(hwndCtl : HWND; lpsz : ARRAY OF CHAR; VAR OUT index : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl, LB_ADDSTRING, 0, CAST(LPARAM, ADR(lpsz)), index);
END ListBox_AddString;

PROCEDURE ListBox_AddStringA(hwndCtl : HWND; lpsz : ARRAY OF ACHAR; VAR OUT index : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsgA(hwndCtl, LB_ADDSTRING, 0, CAST(LPARAM, ADR(lpsz)), index);
END ListBox_AddStringA;

PROCEDURE ListBox_AddStringW(hwndCtl : HWND; lpsz : ARRAY OF UCHAR; VAR OUT index : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsgW(hwndCtl, LB_ADDSTRING, 0, CAST(LPARAM, ADR(lpsz)), index);
END ListBox_AddStringW;

PROCEDURE ListBox_InsertString(hwndCtl : HWND;
                               index : INTEGER;
                               lpsz : ARRAY OF CHAR;
                               VAR OUT idx : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl, LB_INSERTSTRING, VAL(WPARAM,index), CAST(LPARAM, ADR(lpsz)), idx);
END ListBox_InsertString;

PROCEDURE ListBox_InsertStringA(hwndCtl : HWND;
                               index : INTEGER;
                               lpsz : ARRAY OF ACHAR;
                               VAR OUT idx : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsgA(hwndCtl, LB_INSERTSTRING, VAL(WPARAM,index), CAST(LPARAM, ADR(lpsz)), idx);
END ListBox_InsertStringA;

PROCEDURE ListBox_InsertStringW(hwndCtl : HWND;
                               index : INTEGER;
                               lpsz : ARRAY OF UCHAR;
                               VAR OUT idx : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsgW(hwndCtl, LB_INSERTSTRING, VAL(WPARAM,index), CAST(LPARAM, ADR(lpsz)), idx);
END ListBox_InsertStringW;

PROCEDURE ListBox_AddItemData(hwndCtl : HWND; data : LRESULT; VAR OUT index : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl, LB_ADDSTRING, 0, data, index);
END ListBox_AddItemData;

PROCEDURE ListBox_AddItemDataA(hwndCtl : HWND; data : LRESULT; VAR OUT index : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsgA(hwndCtl, LB_ADDSTRING, 0, data, index);
END ListBox_AddItemDataA;

PROCEDURE ListBox_AddItemDataW(hwndCtl : HWND; data : LRESULT; VAR OUT index : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsgW(hwndCtl, LB_ADDSTRING, 0, data, index);
END ListBox_AddItemDataW;

PROCEDURE ListBox_InsertItemData(hwndCtl : HWND;
                                 index : DWORD;
                                 data : LRESULT;
                                 VAR OUT idx : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl, LB_INSERTSTRING, VAL(WPARAM,index), CAST(LPARAM,data), idx);
END ListBox_InsertItemData;

PROCEDURE ListBox_InsertItemDataA(hwndCtl : HWND;
                                 index : DWORD;
                                 data : LRESULT;
                                 VAR OUT idx : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsgA(hwndCtl, LB_INSERTSTRING, VAL(WPARAM,index), CAST(LPARAM,data), idx);
END ListBox_InsertItemDataA;

PROCEDURE ListBox_InsertItemDataW(hwndCtl : HWND;
                                 index : DWORD;
                                 data : LRESULT;
                                 VAR OUT idx : CARDINAL) : BOOL;
BEGIN
    RETURN IndexMsgW(hwndCtl, LB_INSERTSTRING, VAL(WPARAM,index), CAST(LPARAM,data), idx);
END ListBox_InsertItemDataW;

PROCEDURE ListBox_DeleteString(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_DELETESTRING,
                                     VAL(WPARAM,index), 0)
               );
END ListBox_DeleteString;

PROCEDURE ListBox_DeleteStringA(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_DELETESTRING,
                                     VAL(WPARAM,index), 0)
               );
END ListBox_DeleteStringA;

PROCEDURE ListBox_DeleteStringW(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_DELETESTRING,
                                     VAL(WPARAM,index), 0)
               );
END ListBox_DeleteStringW;

PROCEDURE ListBox_GetTextLen(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_GETTEXTLEN,
                                     VAL(WPARAM, index),
                                     0)
               );
END ListBox_GetTextLen;

PROCEDURE ListBox_GetTextLenA(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_GETTEXTLEN,
                                     VAL(WPARAM, index),
                                     0)
               );
END ListBox_GetTextLenA;

PROCEDURE ListBox_GetTextLenW(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_GETTEXTLEN,
                                     VAL(WPARAM, index),
                                     0)
               );
END ListBox_GetTextLenW;

PROCEDURE ListBox_GetText(hwndCtl : HWND;
                          index : DWORD;
                          VAR lpszBuffer : ARRAY OF CHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_GETTEXT,
                                     VAL(WPARAM,index),
                                     CAST(LPARAM, ADR(lpszBuffer)))
               );
END ListBox_GetText;

PROCEDURE ListBox_GetTextA(hwndCtl : HWND;
                          index : DWORD;
                          VAR lpszBuffer : ARRAY OF ACHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_GETTEXT,
                                     VAL(WPARAM,index),
                                     CAST(LPARAM, ADR(lpszBuffer)))
               );
END ListBox_GetTextA;

PROCEDURE ListBox_GetTextW(hwndCtl : HWND;
                          index : DWORD;
                          VAR lpszBuffer : ARRAY OF UCHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_GETTEXT,
                                     VAL(WPARAM,index),
                                     CAST(LPARAM, ADR(lpszBuffer)))
               );
END ListBox_GetTextW;

PROCEDURE ListBox_GetItemData(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN CAST(LRESULT, SendMessage(hwndCtl,
                                     LB_GETITEMDATA,
                                     VAL(WPARAM, index), 0)
               );
END ListBox_GetItemData;

PROCEDURE ListBox_GetItemDataA(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN CAST(LRESULT, SendMessageA(hwndCtl,
                                     LB_GETITEMDATA,
                                     VAL(WPARAM, index), 0)
               );
END ListBox_GetItemDataA;

PROCEDURE ListBox_GetItemDataW(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN CAST(LRESULT, SendMessageW(hwndCtl,
                                     LB_GETITEMDATA,
                                     VAL(WPARAM, index), 0)
               );
END ListBox_GetItemDataW;

PROCEDURE ListBox_SetItemData(hwndCtl : HWND; index : DWORD; data : LRESULT) : LRESULT;
BEGIN
    RETURN SendMessage(hwndCtl, LB_SETITEMDATA, index, data);
END ListBox_SetItemData;

PROCEDURE ListBox_SetItemDataA(hwndCtl : HWND; index : DWORD; data : LRESULT) : LRESULT;
BEGIN
    RETURN SendMessageA(hwndCtl, LB_SETITEMDATA, index, data);
END ListBox_SetItemDataA;

PROCEDURE ListBox_SetItemDataW(hwndCtl : HWND; index : DWORD; data : LRESULT) : LRESULT;
BEGIN
    RETURN SendMessageW(hwndCtl, LB_SETITEMDATA, index, data);
END ListBox_SetItemDataW;

PROCEDURE ListBox_FindString(hwndCtl : HWND;
                             indexStart : INTEGER;
                             lpszFind : ARRAY OF CHAR;
                             VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl, LB_FINDSTRING, indexStart, CAST(LPARAM,ADR(lpszFind)), index);
END ListBox_FindString;

PROCEDURE ListBox_FindStringA(hwndCtl : HWND;
                             indexStart : INTEGER;
                             lpszFind : ARRAY OF ACHAR;
                             VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgA(hwndCtl, LB_FINDSTRING, indexStart, CAST(LPARAM,ADR(lpszFind)), index);
END ListBox_FindStringA;

PROCEDURE ListBox_FindStringW(hwndCtl : HWND;
                             indexStart : INTEGER;
                             lpszFind : ARRAY OF UCHAR;
                             VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgW(hwndCtl, LB_FINDSTRING, indexStart, CAST(LPARAM,ADR(lpszFind)), index);
END ListBox_FindStringW;

PROCEDURE ListBox_FindItemData(hwndCtl : HWND;
                               indexStart : INTEGER;
                               data : LRESULT;
                               VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl, LB_FINDSTRING, indexStart, data, index);
END ListBox_FindItemData;

PROCEDURE ListBox_FindItemDataA(hwndCtl : HWND;
                               indexStart : INTEGER;
                               data : LRESULT;
                               VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgA(hwndCtl, LB_FINDSTRING, indexStart, data, index);
END ListBox_FindItemDataA;

PROCEDURE ListBox_FindItemDataW(hwndCtl : HWND;
                               indexStart : INTEGER;
                               data : LRESULT;
                               VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgW(hwndCtl, LB_FINDSTRING, indexStart, data, index);
END ListBox_FindItemDataW;

PROCEDURE ListBox_SetSel(hwndCtl : HWND;
                         fSelect : BOOL; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_SETSEL,
                                     VAL(WPARAM, fSelect),
                                     CAST(LPARAM, index))
               );
END ListBox_SetSel;

PROCEDURE ListBox_SetSelA(hwndCtl : HWND;
                         fSelect : BOOL; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_SETSEL,
                                     VAL(WPARAM, fSelect),
                                     CAST(LPARAM, index))
               );
END ListBox_SetSelA;

PROCEDURE ListBox_SetSelW(hwndCtl : HWND;
                         fSelect : BOOL; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_SETSEL,
                                     VAL(WPARAM, fSelect),
                                     CAST(LPARAM, index))
               );
END ListBox_SetSelW;

PROCEDURE ListBox_SelItemRange(hwndCtl : HWND;
                               fSelect : BOOL;
                               first, last : WORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_SELITEMRANGE,
                                     VAL(WPARAM,fSelect),
                                     MAKELPARAM(first, last))
               );
END ListBox_SelItemRange;

PROCEDURE ListBox_SelItemRangeA(hwndCtl : HWND;
                               fSelect : BOOL;
                               first, last : WORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_SELITEMRANGE,
                                     VAL(WPARAM,fSelect),
                                     MAKELPARAM(first, last))
               );
END ListBox_SelItemRangeA;

PROCEDURE ListBox_SelItemRangeW(hwndCtl : HWND;
                               fSelect : BOOL;
                               first, last : WORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_SELITEMRANGE,
                                     VAL(WPARAM,fSelect),
                                     MAKELPARAM(first, last))
               );
END ListBox_SelItemRangeW;

PROCEDURE ListBox_GetCurSel(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, LB_GETCURSEL, 0, 0));
END ListBox_GetCurSel;

PROCEDURE ListBox_GetCurSelA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl, LB_GETCURSEL, 0, 0));
END ListBox_GetCurSelA;

PROCEDURE ListBox_GetCurSelW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl, LB_GETCURSEL, 0, 0));
END ListBox_GetCurSelW;

PROCEDURE ListBox_SetCurSel(hwndCtl : HWND; index : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER,SendMessage(hwndCtl, LB_SETCURSEL, index, 0));
END ListBox_SetCurSel;

PROCEDURE ListBox_SetCurSelA(hwndCtl : HWND; index : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER,SendMessageA(hwndCtl, LB_SETCURSEL, index, 0));
END ListBox_SetCurSelA;

PROCEDURE ListBox_SetCurSelW(hwndCtl : HWND; index : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER,SendMessageW(hwndCtl, LB_SETCURSEL, index, 0));
END ListBox_SetCurSelW;

PROCEDURE ListBox_SelectString(hwndCtl : HWND;
                               indexStart: INTEGER;
                               lpszFind : ARRAY OF CHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_SELECTSTRING,
                                     VAL(WPARAM,indexStart),
                                     CAST(LPARAM, ADR(lpszFind)))
               );
END ListBox_SelectString;

PROCEDURE ListBox_SelectStringA(hwndCtl : HWND;
                               indexStart: INTEGER;
                               lpszFind : ARRAY OF ACHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_SELECTSTRING,
                                     VAL(WPARAM,indexStart),
                                     CAST(LPARAM, ADR(lpszFind)))
               );
END ListBox_SelectStringA;

PROCEDURE ListBox_SelectStringW(hwndCtl : HWND;
                               indexStart: INTEGER;
                               lpszFind : ARRAY OF UCHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_SELECTSTRING,
                                     VAL(WPARAM,indexStart),
                                     CAST(LPARAM, ADR(lpszFind)))
               );
END ListBox_SelectStringW;

PROCEDURE ListBox_SelectItemData(hwndCtl : HWND;
                                 indexStart : DWORD;
                                 data : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_SELECTSTRING,
                                     indexStart, data)
               );
END ListBox_SelectItemData;

PROCEDURE ListBox_SelectItemDataA(hwndCtl : HWND;
                                 indexStart : DWORD;
                                 data : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_SELECTSTRING,
                                     indexStart, data)
               );
END ListBox_SelectItemDataA;

PROCEDURE ListBox_SelectItemDataW(hwndCtl : HWND;
                                 indexStart : DWORD;
                                 data : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_SELECTSTRING,
                                     indexStart, data)
               );
END ListBox_SelectItemDataW;

PROCEDURE ListBox_GetSel(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, LB_GETSEL, index, 0));
END ListBox_GetSel;

PROCEDURE ListBox_GetSelA(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl, LB_GETSEL, index, 0));
END ListBox_GetSelA;

PROCEDURE ListBox_GetSelW(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl, LB_GETSEL, index, 0));
END ListBox_GetSelW;

PROCEDURE ListBox_GetSelCount(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, LB_GETSELCOUNT, 0, 0));
END ListBox_GetSelCount;

PROCEDURE ListBox_GetSelCountA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl, LB_GETSELCOUNT, 0, 0));
END ListBox_GetSelCountA;

PROCEDURE ListBox_GetSelCountW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl, LB_GETSELCOUNT, 0, 0));
END ListBox_GetSelCountW;

PROCEDURE ListBox_GetTopIndex(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, LB_GETTOPINDEX, 0, 0));
END ListBox_GetTopIndex;

PROCEDURE ListBox_GetTopIndexA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl, LB_GETTOPINDEX, 0, 0));
END ListBox_GetTopIndexA;

PROCEDURE ListBox_GetTopIndexW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl, LB_GETTOPINDEX, 0, 0));
END ListBox_GetTopIndexW;

PROCEDURE ListBox_GetSelItems(hwndCtl : HWND;
                              cItems : DWORD;
                              VAR OUT lpItems : ARRAY OF WINT;
                              VAR OUT count : UINT) : BOOL;
BEGIN
    RETURN LenMsg(hwndCtl, LB_GETSELITEMS, VAL(WPARAM, cItems), CAST(LPARAM, ADR(lpItems)), count);
END ListBox_GetSelItems;

PROCEDURE ListBox_GetSelItemsA(hwndCtl : HWND;
                              cItems : DWORD;
                              VAR OUT lpItems : ARRAY OF WINT;
                              VAR OUT count : UINT) : BOOL;
BEGIN
    RETURN LenMsgA(hwndCtl, LB_GETSELITEMS, VAL(WPARAM, cItems), CAST(LPARAM, ADR(lpItems)), count);
END ListBox_GetSelItemsA;

PROCEDURE ListBox_GetSelItemsW(hwndCtl : HWND;
                              cItems : DWORD;
                              VAR OUT lpItems : ARRAY OF WINT;
                              VAR OUT count : UINT) : BOOL;
BEGIN
    RETURN LenMsgW(hwndCtl, LB_GETSELITEMS, VAL(WPARAM, cItems), CAST(LPARAM, ADR(lpItems)), count);
END ListBox_GetSelItemsW;

PROCEDURE ListBox_SetTopIndex(hwndCtl : HWND; indexTop : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_SETTOPINDEX,
                                     CAST(LPARAM,indexTop), 0)
               );
END ListBox_SetTopIndex;

PROCEDURE ListBox_SetTopIndexA(hwndCtl : HWND; indexTop : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_SETTOPINDEX,
                                     CAST(LPARAM,indexTop), 0)
               );
END ListBox_SetTopIndexA;

PROCEDURE ListBox_SetTopIndexW(hwndCtl : HWND; indexTop : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_SETTOPINDEX,
                                     CAST(LPARAM,indexTop), 0)
               );
END ListBox_SetTopIndexW;

PROCEDURE ListBox_SetColumnWidth(hwndCtl : HWND; cxColumn : DWORD);
BEGIN
    SendMessage(hwndCtl, LB_SETCOLUMNWIDTH, VAL(WPARAM, cxColumn), 0);
END ListBox_SetColumnWidth;

PROCEDURE ListBox_SetColumnWidthA(hwndCtl : HWND; cxColumn : DWORD);
BEGIN
    SendMessageA(hwndCtl, LB_SETCOLUMNWIDTH, VAL(WPARAM, cxColumn), 0);
END ListBox_SetColumnWidthA;

PROCEDURE ListBox_SetColumnWidthW(hwndCtl : HWND; cxColumn : DWORD);
BEGIN
    SendMessageW(hwndCtl, LB_SETCOLUMNWIDTH, VAL(WPARAM, cxColumn), 0);
END ListBox_SetColumnWidthW;

PROCEDURE ListBox_GetHorizontalExtent(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_GETHORIZONTALEXTENT, 0, 0)
               );
END ListBox_GetHorizontalExtent;

PROCEDURE ListBox_GetHorizontalExtentA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_GETHORIZONTALEXTENT, 0, 0)
               );
END ListBox_GetHorizontalExtentA;

PROCEDURE ListBox_GetHorizontalExtentW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_GETHORIZONTALEXTENT, 0, 0)
               );
END ListBox_GetHorizontalExtentW;

PROCEDURE ListBox_SetHorizontalExtent(hwndCtl : HWND; cxExtent : DWORD);
BEGIN
    SendMessage(hwndCtl, LB_SETHORIZONTALEXTENT, VAL(WPARAM,cxExtent), 0);
END ListBox_SetHorizontalExtent;

PROCEDURE ListBox_SetHorizontalExtentA(hwndCtl : HWND; cxExtent : DWORD);
BEGIN
    SendMessageA(hwndCtl, LB_SETHORIZONTALEXTENT, VAL(WPARAM,cxExtent), 0);
END ListBox_SetHorizontalExtentA;

PROCEDURE ListBox_SetHorizontalExtentW(hwndCtl : HWND; cxExtent : DWORD);
BEGIN
    SendMessageW(hwndCtl, LB_SETHORIZONTALEXTENT, VAL(WPARAM,cxExtent), 0);
END ListBox_SetHorizontalExtentW;

PROCEDURE ListBox_SetTabStops(hwndCtl : HWND;
                              cTabs : DWORD;
                              lpTabs : ARRAY OF INTEGER) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndCtl,
                                  LB_SETTABSTOPS,
                                  VAL(WPARAM,cTabs),
                                  CAST(LPARAM, ADR(lpTabs)))
               );
END ListBox_SetTabStops;

PROCEDURE ListBox_SetTabStopsA(hwndCtl : HWND;
                              cTabs : DWORD;
                              lpTabs : ARRAY OF INTEGER) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndCtl,
                                  LB_SETTABSTOPS,
                                  VAL(WPARAM,cTabs),
                                  CAST(LPARAM, ADR(lpTabs)))
               );
END ListBox_SetTabStopsA;

PROCEDURE ListBox_SetTabStopsW(hwndCtl : HWND;
                              cTabs : DWORD;
                              lpTabs : ARRAY OF INTEGER) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndCtl,
                                  LB_SETTABSTOPS,
                                  VAL(WPARAM,cTabs),
                                  CAST(LPARAM, ADR(lpTabs)))
               );
END ListBox_SetTabStopsW;

PROCEDURE ListBox_GetItemRect(hwndCtl : HWND;
                              index : DWORD;
                              VAR lprc : RECT) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_GETITEMRECT,
                                     VAL(WPARAM,index),
                                     CAST(LPARAM,ADR(lprc)))
               );
END ListBox_GetItemRect;

PROCEDURE ListBox_GetItemRectA(hwndCtl : HWND;
                              index : DWORD;
                              VAR lprc : RECT) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_GETITEMRECT,
                                     VAL(WPARAM,index),
                                     CAST(LPARAM,ADR(lprc)))
               );
END ListBox_GetItemRectA;

PROCEDURE ListBox_GetItemRectW(hwndCtl : HWND;
                              index : DWORD;
                              VAR lprc : RECT) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_GETITEMRECT,
                                     VAL(WPARAM,index),
                                     CAST(LPARAM,ADR(lprc)))
               );
END ListBox_GetItemRectW;

PROCEDURE ListBox_SetCaretIndex(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_SETCARETINDEX,
                                     VAL(WPARAM,index), 0)
               );
END ListBox_SetCaretIndex;

PROCEDURE ListBox_SetCaretIndexA(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_SETCARETINDEX,
                                     VAL(WPARAM,index), 0)
               );
END ListBox_SetCaretIndexA;

PROCEDURE ListBox_SetCaretIndexW(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_SETCARETINDEX,
                                     VAL(WPARAM,index), 0)
               );
END ListBox_SetCaretIndexW;

PROCEDURE ListBox_GetCaretIndex(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, LB_GETCARETINDEX, 0, 0));
END ListBox_GetCaretIndex;

PROCEDURE ListBox_GetCaretIndexA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl, LB_GETCARETINDEX, 0, 0));
END ListBox_GetCaretIndexA;

PROCEDURE ListBox_GetCaretIndexW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl, LB_GETCARETINDEX, 0, 0));
END ListBox_GetCaretIndexW;

PROCEDURE ListBox_FindStringExact(hwndCtl : HWND;
                                  indexStart : DWORD;
                                  lpszFind : ARRAY OF CHAR;
                                  VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl,
                    LB_FINDSTRINGEXACT,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, ADR(lpszFind)),
                    index);
END ListBox_FindStringExact;

PROCEDURE ListBox_FindStringExactA(hwndCtl : HWND;
                                  indexStart : DWORD;
                                  lpszFind : ARRAY OF ACHAR;
                                  VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgA(hwndCtl,
                    LB_FINDSTRINGEXACT,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, ADR(lpszFind)),
                    index);
END ListBox_FindStringExactA;

PROCEDURE ListBox_FindStringExactW(hwndCtl : HWND;
                                  indexStart : DWORD;
                                  lpszFind : ARRAY OF UCHAR;
                                  VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgW(hwndCtl,
                    LB_FINDSTRINGEXACT,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, ADR(lpszFind)),
                    index);
END ListBox_FindStringExactW;

PROCEDURE ListBox_SetItemHeight(hwndCtl : HWND;
                                index : DWORD; cy : WORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_SETITEMHEIGHT,
                                     VAL(WPARAM,index),
                                     MAKELPARAM(cy, 0))
               );
END ListBox_SetItemHeight;

PROCEDURE ListBox_SetItemHeightA(hwndCtl : HWND;
                                index : DWORD; cy : WORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_SETITEMHEIGHT,
                                     VAL(WPARAM,index),
                                     MAKELPARAM(cy, 0))
               );
END ListBox_SetItemHeightA;

PROCEDURE ListBox_SetItemHeightW(hwndCtl : HWND;
                                index : DWORD; cy : WORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_SETITEMHEIGHT,
                                     VAL(WPARAM,index),
                                     MAKELPARAM(cy, 0))
               );
END ListBox_SetItemHeightW;

PROCEDURE ListBox_GetItemHeight(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_GETITEMHEIGHT,
                                     VAL(WPARAM, index), 0)
               );
END ListBox_GetItemHeight;

PROCEDURE ListBox_GetItemHeightA(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_GETITEMHEIGHT,
                                     VAL(WPARAM, index), 0)
               );
END ListBox_GetItemHeightA;

PROCEDURE ListBox_GetItemHeightW(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_GETITEMHEIGHT,
                                     VAL(WPARAM, index), 0)
               );
END ListBox_GetItemHeightW;

PROCEDURE ListBox_Dir(hwndCtl : HWND;
                      attrs : DWORD;
                      lpszFileSpec : ARRAY OF CHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     LB_DIR,
                                     VAL(WPARAM, attrs),
                                     CAST(LPARAM, ADR(lpszFileSpec)))
               );
END ListBox_Dir;

PROCEDURE ListBox_DirA(hwndCtl : HWND;
                      attrs : DWORD;
                      lpszFileSpec : ARRAY OF ACHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     LB_DIR,
                                     VAL(WPARAM, attrs),
                                     CAST(LPARAM, ADR(lpszFileSpec)))
               );
END ListBox_DirA;

PROCEDURE ListBox_DirW(hwndCtl : HWND;
                      attrs : DWORD;
                      lpszFileSpec : ARRAY OF UCHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     LB_DIR,
                                     VAL(WPARAM, attrs),
                                     CAST(LPARAM, ADR(lpszFileSpec)))
               );
END ListBox_DirW;

(********** ComboBox's **************************)

PROCEDURE ComboBox_Enable(hwndCtl : HWND; fEnable : BOOL) : BOOL;
BEGIN
    RETURN EnableWindow(hwndCtl, fEnable);
END ComboBox_Enable;

PROCEDURE ComboBox_GetText(hwndCtl : HWND;
                         VAR lpch : ARRAY OF CHAR;
                         cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowText(hwndCtl, lpch, cchMax);
END ComboBox_GetText;

PROCEDURE ComboBox_GetTextA(hwndCtl : HWND;
                         VAR lpch : ARRAY OF ACHAR;
                         cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowTextA(hwndCtl, lpch, cchMax);
END ComboBox_GetTextA;

PROCEDURE ComboBox_GetTextW(hwndCtl : HWND;
                         VAR lpch : ARRAY OF UCHAR;
                         cchMax : INTEGER) : INTEGER;
BEGIN
    RETURN GetWindowTextW(hwndCtl, lpch, cchMax);
END ComboBox_GetTextW;

PROCEDURE ComboBox_GetTextLength(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLength(hwndCtl);
END ComboBox_GetTextLength;

PROCEDURE ComboBox_GetTextLengthA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLengthA(hwndCtl);
END ComboBox_GetTextLengthA;

PROCEDURE ComboBox_GetTextLengthW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN GetWindowTextLengthW(hwndCtl);
END ComboBox_GetTextLengthW;

PROCEDURE ComboBox_SetText(hwndCtl : HWND; lpsz : ARRAY OF CHAR) : BOOL;
BEGIN
    RETURN SetWindowText(hwndCtl, lpsz);
END ComboBox_SetText;

PROCEDURE ComboBox_SetTextA(hwndCtl : HWND; lpsz : ARRAY OF ACHAR) : BOOL;
BEGIN
    RETURN SetWindowTextA(hwndCtl, lpsz);
END ComboBox_SetTextA;

PROCEDURE ComboBox_SetTextW(hwndCtl : HWND; lpsz : ARRAY OF UCHAR) : BOOL;
BEGIN
    RETURN SetWindowTextW(hwndCtl, lpsz);
END ComboBox_SetTextW;

PROCEDURE ComboBox_LimitText(hwndCtl : HWND; cchLimit : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_LIMITTEXT,
                                     VAL(WPARAM,cchLimit), 0)
               );
END ComboBox_LimitText;

PROCEDURE ComboBox_LimitTextA(hwndCtl : HWND; cchLimit : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_LIMITTEXT,
                                     VAL(WPARAM,cchLimit), 0)
               );
END ComboBox_LimitTextA;

PROCEDURE ComboBox_LimitTextW(hwndCtl : HWND; cchLimit : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_LIMITTEXT,
                                     VAL(WPARAM,cchLimit), 0)
               );
END ComboBox_LimitTextW;

PROCEDURE ComboBox_GetEditSel(hwndCtl : HWND; VAR startPos, endPos : INTEGER);
VAR
    result      : DWORD;
BEGIN
    result := CAST(DWORD, SendMessage(hwndCtl, CB_GETEDITSEL, 0, 0));
    startPos := VAL(INTEGER16, LOWORD(result));
    endPos := VAL(INTEGER16, HIWORD(result));
END ComboBox_GetEditSel;

PROCEDURE ComboBox_GetEditSelA(hwndCtl : HWND; VAR startPos, endPos : INTEGER);
VAR
    result      : DWORD;
BEGIN
    result := CAST(DWORD, SendMessageA(hwndCtl, CB_GETEDITSEL, 0, 0));
    startPos := VAL(INTEGER16, LOWORD(result));
    endPos := VAL(INTEGER16, HIWORD(result));
END ComboBox_GetEditSelA;

PROCEDURE ComboBox_GetEditSelW(hwndCtl : HWND; VAR startPos, endPos : INTEGER);
VAR
    result      : DWORD;
BEGIN
    result := CAST(DWORD, SendMessageW(hwndCtl, CB_GETEDITSEL, 0, 0));
    startPos := VAL(INTEGER16, LOWORD(result));
    endPos := VAL(INTEGER16, HIWORD(result));
END ComboBox_GetEditSelW;

PROCEDURE ComboBox_SetEditSel(hwndCtl : HWND;
                              startPos, endPos : INTEGER) : BOOL;
BEGIN
    RETURN SendMessage(hwndCtl,
                       CB_SETEDITSEL,
                       0,
                       MAKELPARAM(startPos, endPos)) <> 0;
END ComboBox_SetEditSel;

PROCEDURE ComboBox_SetEditSelA(hwndCtl : HWND;
                              startPos, endPos : INTEGER) : BOOL;
BEGIN
    RETURN SendMessageA(hwndCtl,
                       CB_SETEDITSEL,
                       0,
                       MAKELPARAM(startPos, endPos)) <> 0;
END ComboBox_SetEditSelA;

PROCEDURE ComboBox_SetEditSelW(hwndCtl : HWND;
                              startPos, endPos : INTEGER) : BOOL;
BEGIN
    RETURN SendMessageW(hwndCtl,
                       CB_SETEDITSEL,
                       0,
                       MAKELPARAM(startPos, endPos)) <> 0;
END ComboBox_SetEditSelW;

PROCEDURE ComboBox_GetCount(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, CB_GETCOUNT, 0, 0));
END ComboBox_GetCount;

PROCEDURE ComboBox_GetCountA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl, CB_GETCOUNT, 0, 0));
END ComboBox_GetCountA;

PROCEDURE ComboBox_GetCountW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl, CB_GETCOUNT, 0, 0));
END ComboBox_GetCountW;

PROCEDURE ComboBox_ResetContent(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN SendMessage(hwndCtl, CB_RESETCONTENT, 0, 0);
END ComboBox_ResetContent;

PROCEDURE ComboBox_ResetContentA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN SendMessageA(hwndCtl, CB_RESETCONTENT, 0, 0);
END ComboBox_ResetContentA;

PROCEDURE ComboBox_ResetContentW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN SendMessageW(hwndCtl, CB_RESETCONTENT, 0, 0);
END ComboBox_ResetContentW;

PROCEDURE ComboBox_AddString(hwndCtl : HWND;
                             lpsz : ARRAY OF CHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_ADDSTRING,
                                     0,
                                     CAST(LPARAM, ADR(lpsz)))
               );
END ComboBox_AddString;

PROCEDURE ComboBox_AddStringA(hwndCtl : HWND;
                             lpsz : ARRAY OF ACHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_ADDSTRING,
                                     0,
                                     CAST(LPARAM, ADR(lpsz)))
               );
END ComboBox_AddStringA;

PROCEDURE ComboBox_AddStringW(hwndCtl : HWND;
                             lpsz : ARRAY OF UCHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_ADDSTRING,
                                     0,
                                     CAST(LPARAM, ADR(lpsz)))
               );
END ComboBox_AddStringW;

PROCEDURE ComboBox_InsertString(hwndCtl : HWND;
                                index : INTEGER;
                                lpsz : ARRAY OF CHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_INSERTSTRING,
                                     VAL(WPARAM,index),
                                     CAST(LPARAM,ADR(lpsz)))
               );
END ComboBox_InsertString;

PROCEDURE ComboBox_InsertStringA(hwndCtl : HWND;
                                index : INTEGER;
                                lpsz : ARRAY OF ACHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_INSERTSTRING,
                                     VAL(WPARAM,index),
                                     CAST(LPARAM,ADR(lpsz)))
               );
END ComboBox_InsertStringA;

PROCEDURE ComboBox_InsertStringW(hwndCtl : HWND;
                                index : INTEGER;
                                lpsz : ARRAY OF UCHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_INSERTSTRING,
                                     VAL(WPARAM,index),
                                     CAST(LPARAM,ADR(lpsz)))
               );
END ComboBox_InsertStringW;

PROCEDURE ComboBox_AddItemData(hwndCtl : HWND; data : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_ADDSTRING,
                                     0,
                                     CAST(LPARAM,data))
               );
END ComboBox_AddItemData;

PROCEDURE ComboBox_AddItemDataA(hwndCtl : HWND; data : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_ADDSTRING,
                                     0,
                                     CAST(LPARAM,data))
               );
END ComboBox_AddItemDataA;

PROCEDURE ComboBox_AddItemDataW(hwndCtl : HWND; data : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_ADDSTRING,
                                     0,
                                     CAST(LPARAM,data))
               );
END ComboBox_AddItemDataW;

PROCEDURE ComboBox_InsertItemData(hwndCtl : HWND;
                                  index : DWORD;
                                  data : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_INSERTSTRING,
                                     VAL(WPARAM, index),
                                     CAST(LPARAM, data))
               );
END ComboBox_InsertItemData;

PROCEDURE ComboBox_InsertItemDataA(hwndCtl : HWND;
                                  index : DWORD;
                                  data : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_INSERTSTRING,
                                     VAL(WPARAM, index),
                                     CAST(LPARAM, data))
               );
END ComboBox_InsertItemDataA;

PROCEDURE ComboBox_InsertItemDataW(hwndCtl : HWND;
                                  index : DWORD;
                                  data : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_INSERTSTRING,
                                     VAL(WPARAM, index),
                                     CAST(LPARAM, data))
               );
END ComboBox_InsertItemDataW;

PROCEDURE ComboBox_DeleteString(hwndCtl : HWND; index : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_DELETESTRING,
                                     VAL(WPARAM,index), 0)
               );
END ComboBox_DeleteString;

PROCEDURE ComboBox_DeleteStringA(hwndCtl : HWND; index : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_DELETESTRING,
                                     VAL(WPARAM,index), 0)
               );
END ComboBox_DeleteStringA;

PROCEDURE ComboBox_DeleteStringW(hwndCtl : HWND; index : INTEGER) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_DELETESTRING,
                                     VAL(WPARAM,index), 0)
               );
END ComboBox_DeleteStringW;

PROCEDURE ComboBox_GetLBTextLen(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_GETLBTEXTLEN,
                                     VAL(WPARAM, index), 0)
               );
END ComboBox_GetLBTextLen;

PROCEDURE ComboBox_GetLBTextLenA(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_GETLBTEXTLEN,
                                     VAL(WPARAM, index), 0)
               );
END ComboBox_GetLBTextLenA;

PROCEDURE ComboBox_GetLBTextLenW(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_GETLBTEXTLEN,
                                     VAL(WPARAM, index), 0)
               );
END ComboBox_GetLBTextLenW;

PROCEDURE ComboBox_GetLBText(hwndCtl : HWND;
                             index : DWORD;
                             VAR OUT lpszBuffer : ARRAY OF CHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_GETLBTEXT,
                                     VAL(WPARAM, index),
                                     CAST(LPARAM, ADR(lpszBuffer)))
               );
END ComboBox_GetLBText;

PROCEDURE ComboBox_GetLBTextA(hwndCtl : HWND;
                             index : DWORD;
                             VAR OUT lpszBuffer : ARRAY OF ACHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_GETLBTEXT,
                                     VAL(WPARAM, index),
                                     CAST(LPARAM, ADR(lpszBuffer)))
               );
END ComboBox_GetLBTextA;

PROCEDURE ComboBox_GetLBTextW(hwndCtl : HWND;
                             index : DWORD;
                             VAR OUT lpszBuffer : ARRAY OF UCHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_GETLBTEXT,
                                     VAL(WPARAM, index),
                                     CAST(LPARAM, ADR(lpszBuffer)))
               );
END ComboBox_GetLBTextW;

PROCEDURE ComboBox_GetItemData(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN CAST(DWORD, SendMessage(hwndCtl,
                                   CB_GETITEMDATA,
                                   VAL(WPARAM, index), 0)
               );
END ComboBox_GetItemData;

PROCEDURE ComboBox_GetItemDataA(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN CAST(DWORD, SendMessageA(hwndCtl,
                                   CB_GETITEMDATA,
                                   VAL(WPARAM, index), 0)
               );
END ComboBox_GetItemDataA;

PROCEDURE ComboBox_GetItemDataW(hwndCtl : HWND; index : DWORD) : LRESULT;
BEGIN
    RETURN CAST(DWORD, SendMessageW(hwndCtl,
                                   CB_GETITEMDATA,
                                   VAL(WPARAM, index), 0)
               );
END ComboBox_GetItemDataW;

PROCEDURE ComboBox_SetItemData(hwndCtl : HWND;
                               index : DWORD; data : LRESULT) : LRESULT;
BEGIN
    RETURN SendMessage(hwndCtl, CB_SETITEMDATA, VAL(WPARAM, index), CAST(LPARAM, data))
END ComboBox_SetItemData;

PROCEDURE ComboBox_SetItemDataA(hwndCtl : HWND;
                               index : DWORD; data : LRESULT) : LRESULT;
BEGIN
    RETURN SendMessageA(hwndCtl, CB_SETITEMDATA, VAL(WPARAM, index), CAST(LPARAM, data))
END ComboBox_SetItemDataA;

PROCEDURE ComboBox_SetItemDataW(hwndCtl : HWND;
                               index : DWORD; data : LRESULT) : LRESULT;
BEGIN
    RETURN SendMessageW(hwndCtl, CB_SETITEMDATA, VAL(WPARAM, index), CAST(LPARAM, data))
END ComboBox_SetItemDataW;

PROCEDURE ComboBox_FindString(hwndCtl : HWND;
                              indexStart : INTEGER;
                              lpszFind : ARRAY OF CHAR;
                              VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl,
                    CB_FINDSTRING,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, ADR(lpszFind)),
                    index);
END ComboBox_FindString;

PROCEDURE ComboBox_FindStringA(hwndCtl : HWND;
                              indexStart : INTEGER;
                              lpszFind : ARRAY OF ACHAR;
                              VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgA(hwndCtl,
                    CB_FINDSTRING,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, ADR(lpszFind)),
                    index);
END ComboBox_FindStringA;

PROCEDURE ComboBox_FindStringW(hwndCtl : HWND;
                              indexStart : INTEGER;
                              lpszFind : ARRAY OF UCHAR;
                              VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgW(hwndCtl,
                    CB_FINDSTRING,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, ADR(lpszFind)),
                    index);
END ComboBox_FindStringW;

PROCEDURE ComboBox_FindItemData(hwndCtl : HWND;
                                indexStart : INTEGER;
                                data : LRESULT;
                                VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl,
                    CB_FINDSTRING,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, data),
                    index);
END ComboBox_FindItemData;

PROCEDURE ComboBox_FindItemDataA(hwndCtl : HWND;
                                indexStart : INTEGER;
                                data : LRESULT;
                                VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgA(hwndCtl,
                    CB_FINDSTRING,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, data),
                    index);
END ComboBox_FindItemDataA;

PROCEDURE ComboBox_FindItemDataW(hwndCtl : HWND;
                                indexStart : INTEGER;
                                data : LRESULT;
                                VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgW(hwndCtl,
                    CB_FINDSTRING,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, data),
                    index);
END ComboBox_FindItemDataW;

PROCEDURE ComboBox_GetCurSel(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, CB_GETCURSEL, 0, 0));
END ComboBox_GetCurSel;

PROCEDURE ComboBox_GetCurSelA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl, CB_GETCURSEL, 0, 0));
END ComboBox_GetCurSelA;

PROCEDURE ComboBox_GetCurSelW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl, CB_GETCURSEL, 0, 0));
END ComboBox_GetCurSelW;

PROCEDURE ComboBox_SetCurSel(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_SETCURSEL,
                                     VAL(WPARAM, index), 0)
               );
END ComboBox_SetCurSel;

PROCEDURE ComboBox_SetCurSelA(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_SETCURSEL,
                                     VAL(WPARAM, index), 0)
               );
END ComboBox_SetCurSelA;

PROCEDURE ComboBox_SetCurSelW(hwndCtl : HWND; index : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_SETCURSEL,
                                     VAL(WPARAM, index), 0)
               );
END ComboBox_SetCurSelW;

PROCEDURE ComboBox_SelectString(hwndCtl : HWND;
                                indexStart : DWORD;
                                lpszSelect : ARRAY OF CHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_SELECTSTRING,
                                     VAL(WPARAM,indexStart),
                                     CAST(LPARAM, ADR(lpszSelect)))
               );
END ComboBox_SelectString;

PROCEDURE ComboBox_SelectStringA(hwndCtl : HWND;
                                indexStart : DWORD;
                                lpszSelect : ARRAY OF ACHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_SELECTSTRING,
                                     VAL(WPARAM,indexStart),
                                     CAST(LPARAM, ADR(lpszSelect)))
               );
END ComboBox_SelectStringA;

PROCEDURE ComboBox_SelectStringW(hwndCtl : HWND;
                                indexStart : DWORD;
                                lpszSelect : ARRAY OF UCHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_SELECTSTRING,
                                     VAL(WPARAM,indexStart),
                                     CAST(LPARAM, ADR(lpszSelect)))
               );
END ComboBox_SelectStringW;

PROCEDURE ComboBox_SelectItemData(hwndCtl : HWND; indexStart : DWORD; data : LRESULT) : LRESULT;
BEGIN
    RETURN SendMessage(hwndCtl, CB_SELECTSTRING, VAL(WPARAM, indexStart), CAST(LPARAM, data))
END ComboBox_SelectItemData;

PROCEDURE ComboBox_SelectItemDataA(hwndCtl : HWND; indexStart : DWORD; data : LRESULT) : LRESULT;
BEGIN
    RETURN SendMessageA(hwndCtl, CB_SELECTSTRING, VAL(WPARAM, indexStart), CAST(LPARAM, data))
END ComboBox_SelectItemDataA;

PROCEDURE ComboBox_SelectItemDataW(hwndCtl : HWND; indexStart : DWORD; data : LRESULT) : LRESULT;
BEGIN
    RETURN SendMessageW(hwndCtl, CB_SELECTSTRING, VAL(WPARAM, indexStart), CAST(LPARAM, data))
END ComboBox_SelectItemDataW;

PROCEDURE ComboBox_Dir(hwndCtl : HWND;
                       attrs : DWORD;
                       lpszFileSpec : ARRAY OF CHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_DIR,
                                     VAL(WPARAM, attrs),
                                     CAST(LPARAM, ADR(lpszFileSpec)))
               );
END ComboBox_Dir;

PROCEDURE ComboBox_DirA(hwndCtl : HWND;
                       attrs : DWORD;
                       lpszFileSpec : ARRAY OF ACHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_DIR,
                                     VAL(WPARAM, attrs),
                                     CAST(LPARAM, ADR(lpszFileSpec)))
               );
END ComboBox_DirA;

PROCEDURE ComboBox_DirW(hwndCtl : HWND;
                       attrs : DWORD;
                       lpszFileSpec : ARRAY OF UCHAR) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_DIR,
                                     VAL(WPARAM, attrs),
                                     CAST(LPARAM, ADR(lpszFileSpec)))
               );
END ComboBox_DirW;

PROCEDURE ComboBox_ShowDropdown(hwndCtl : HWND; fShow : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndCtl,
                                  CB_SHOWDROPDOWN,
                                  VAL(WPARAM, fShow), 0)
               );
END ComboBox_ShowDropdown;

PROCEDURE ComboBox_ShowDropdownA(hwndCtl : HWND; fShow : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndCtl,
                                  CB_SHOWDROPDOWN,
                                  VAL(WPARAM, fShow), 0)
               );
END ComboBox_ShowDropdownA;

PROCEDURE ComboBox_ShowDropdownW(hwndCtl : HWND; fShow : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndCtl,
                                  CB_SHOWDROPDOWN,
                                  VAL(WPARAM, fShow), 0)
               );
END ComboBox_ShowDropdownW;

PROCEDURE ComboBox_FindStringExact(hwndCtl : HWND;
                                   indexStart : INTEGER;
                                   lpszFind : ARRAY OF CHAR;
                                   VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl,
                    CB_FINDSTRINGEXACT,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, ADR(lpszFind)),
                    index);
END ComboBox_FindStringExact;

PROCEDURE ComboBox_FindStringExactA(hwndCtl : HWND;
                                   indexStart : INTEGER;
                                   lpszFind : ARRAY OF ACHAR;
                                   VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsg(hwndCtl,
                    CB_FINDSTRINGEXACT,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, ADR(lpszFind)),
                    index);
END ComboBox_FindStringExactA;

PROCEDURE ComboBox_FindStringExactW(hwndCtl : HWND;
                                   indexStart : INTEGER;
                                   lpszFind : ARRAY OF UCHAR;
                                   VAR OUT index : UINT) : BOOL;
BEGIN
    RETURN IndexMsgW(hwndCtl,
                    CB_FINDSTRINGEXACT,
                    VAL(WPARAM, indexStart),
                    CAST(LPARAM, ADR(lpszFind)),
                    index);
END ComboBox_FindStringExactW;

PROCEDURE ComboBox_GetDroppedState(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndCtl, CB_GETDROPPEDSTATE, 0, 0));
END ComboBox_GetDroppedState;

PROCEDURE ComboBox_GetDroppedStateA(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndCtl, CB_GETDROPPEDSTATE, 0, 0));
END ComboBox_GetDroppedStateA;

PROCEDURE ComboBox_GetDroppedStateW(hwndCtl : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndCtl, CB_GETDROPPEDSTATE, 0, 0));
END ComboBox_GetDroppedStateW;

PROCEDURE ComboBox_GetDroppedControlRect(hwndCtl : HWND; VAR lprc : RECT);
BEGIN
    SendMessage(hwndCtl,
                CB_GETDROPPEDCONTROLRECT,
                0, CAST(LPARAM, ADR(lprc)));
END ComboBox_GetDroppedControlRect;

PROCEDURE ComboBox_GetDroppedControlRectA(hwndCtl : HWND; VAR lprc : RECT);
BEGIN
    SendMessageA(hwndCtl,
                CB_GETDROPPEDCONTROLRECT,
                0, CAST(LPARAM, ADR(lprc)));
END ComboBox_GetDroppedControlRectA;

PROCEDURE ComboBox_GetDroppedControlRectW(hwndCtl : HWND; VAR lprc : RECT);
BEGIN
    SendMessageW(hwndCtl,
                CB_GETDROPPEDCONTROLRECT,
                0, CAST(LPARAM, ADR(lprc)));
END ComboBox_GetDroppedControlRectW;

PROCEDURE ComboBox_GetItemHeight(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, CB_GETITEMHEIGHT, 0, 0));
END ComboBox_GetItemHeight;

PROCEDURE ComboBox_GetItemHeightA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl, CB_GETITEMHEIGHT, 0, 0));
END ComboBox_GetItemHeightA;

PROCEDURE ComboBox_GetItemHeightW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl, CB_GETITEMHEIGHT, 0, 0));
END ComboBox_GetItemHeightW;

PROCEDURE ComboBox_SetItemHeight(hwndCtl : HWND; cyItem : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_SETITEMHEIGHT,
                                     VAL(WPARAM, cyItem), 0)
               );
END ComboBox_SetItemHeight;

PROCEDURE ComboBox_SetItemHeightA(hwndCtl : HWND; cyItem : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_SETITEMHEIGHT,
                                     VAL(WPARAM, cyItem), 0)
               );
END ComboBox_SetItemHeightA;

PROCEDURE ComboBox_SetItemHeightW(hwndCtl : HWND; cyItem : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_SETITEMHEIGHT,
                                     VAL(WPARAM, cyItem), 0)
               );
END ComboBox_SetItemHeightW;

PROCEDURE ComboBox_GetExtendedUI(hwndCtl : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwndCtl, CB_GETEXTENDEDUI, 0, 0));
END ComboBox_GetExtendedUI;

PROCEDURE ComboBox_GetExtendedUIA(hwndCtl : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageA(hwndCtl, CB_GETEXTENDEDUI, 0, 0));
END ComboBox_GetExtendedUIA;

PROCEDURE ComboBox_GetExtendedUIW(hwndCtl : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageW(hwndCtl, CB_GETEXTENDEDUI, 0, 0));
END ComboBox_GetExtendedUIW;

PROCEDURE ComboBox_SetExtendedUI(hwndCtl : HWND; flags : UINT) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_SETEXTENDEDUI,
                                     VAL(WPARAM, flags), 0)
               );
END ComboBox_SetExtendedUI;

PROCEDURE ComboBox_SetExtendedUIA(hwndCtl : HWND; flags : UINT) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_SETEXTENDEDUI,
                                     VAL(WPARAM, flags), 0)
               );
END ComboBox_SetExtendedUIA;

PROCEDURE ComboBox_SetExtendedUIW(hwndCtl : HWND; flags : UINT) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_SETEXTENDEDUI,
                                     VAL(WPARAM, flags), 0)
               );
END ComboBox_SetExtendedUIW;

PROCEDURE ComboBox_GetTopIndex(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl, CB_GETTOPINDEX, 0, 0));
END ComboBox_GetTopIndex;

PROCEDURE ComboBox_GetTopIndexA(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl, CB_GETTOPINDEX, 0, 0));
END ComboBox_GetTopIndexA;

PROCEDURE ComboBox_GetTopIndexW(hwndCtl : HWND) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl, CB_GETTOPINDEX, 0, 0));
END ComboBox_GetTopIndexW;

PROCEDURE ComboBox_SetTopIndex(hwndCtl : HWND; indexTop : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessage(hwndCtl,
                                     CB_SETTOPINDEX,
                                     CAST(LPARAM,indexTop), 0)
               );
END ComboBox_SetTopIndex;

PROCEDURE ComboBox_SetTopIndexA(hwndCtl : HWND; indexTop : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageA(hwndCtl,
                                     CB_SETTOPINDEX,
                                     CAST(LPARAM,indexTop), 0)
               );
END ComboBox_SetTopIndexA;

PROCEDURE ComboBox_SetTopIndexW(hwndCtl : HWND; indexTop : DWORD) : INTEGER;
BEGIN
    RETURN VAL(INTEGER, SendMessageW(hwndCtl,
                                     CB_SETTOPINDEX,
                                     CAST(LPARAM,indexTop), 0)
               );
END ComboBox_SetTopIndexW;

END WINX.
