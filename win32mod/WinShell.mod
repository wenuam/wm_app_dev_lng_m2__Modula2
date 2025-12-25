(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE WinShell;
<*/NOWARN:F*>
<*/NOPACK*>

<*/RESOURCE:WinShell*>

FROM SYSTEM IMPORT
    ADDRESS, ADR, MAKEADR, ADDADR, ADRINT, ADRCARD, MACHINEWORD, CAST, EXCEPTADR, IsThread,
    ATOMIC_CMPXCHG, MEMORY_FENCE,
    UNREFERENCED_PARAMETER, FUNC, OutputCallTrace;

IMPORT EXCEPTIONS, Float;

FROM RealMath IMPORT
    sin, cos, pi, round;

FROM Strings IMPORT
    CompareResults,
    Assign, Concat, Append, Delete, Compare;

FROM ExStrings IMPORT
    EqualI;

FROM ExStorage IMPORT
    HeapInfoPointer, GetDefaultHeap, AllocateEx, DeallocateEx, ReallocateEx;

FROM Conversions IMPORT
    IntToStr, StrToInt, StrToCard;

FROM MemUtils IMPORT
    MoveMem, FillMemBYTE, ZeroMem;

FROM FileFunc IMPORT
    File, FileUseInfo, FileUseInfoSet,
    FileSpecString, FileNameParts, ParseFileName,
    CreateFileEx, WriteBlock, CloseFile, DeleteFile;

FROM WIN32 IMPORT
    (* CONSTS *)
    GMEM_DDESHARE, GMEM_MOVEABLE, VER_PLATFORM_WIN32_NT, NULL,
    (* TYPES *)
    UINT, HWND, HANDLE, HMENU, HDC, HINSTANCE, HBRUSH, HPEN, HBITMAP,
    HFONT, HRSRC, ULONG_PTR, LONG_PTR, UINT_PTR, DWORD_PTR,
    WPARAM, LPARAM, LRESULT, DWORD, RECT, POINT, HPALETTE,
    HACCEL, HICON, HCURSOR, OSVERSIONINFO, LPTSTR, STARTUPINFO,
    PROCESS_INFORMATION,
    (* PROCS *)
    GlobalAlloc, GlobalLock, GlobalUnlock, GetVersionEx,
    LoadLibrary, FreeLibrary, GetProcAddress,
    FindResource, LoadResource, LockResource, FreeResource, SizeofResource,
    CreateProcess;
IMPORT WIN32;

FROM WINUSER IMPORT
    (* PROCS *)
    CreateWindowEx, DestroyWindow, ShowWindow, CallWindowProc,
    RegisterClass, GetClassInfo, GetDC, ReleaseDC, GetClientRect,
    GetWindowRect, MapWindowPoints, IsIconic, IsZoomed, WinHelp,
    IsWindowVisible, IsDialogMessage, GetDesktopWindow,
    LoadImage, LoadIcon, LoadCursor, SetCursor, AppendMenu,
    DestroyAcceleratorTable, TranslateAccelerator,
    MessageBox, MoveWindow, EnableWindow, SetParent, GetParent, GetWindow,
    GetMessage, PeekMessage, DispatchMessage, TranslateMessage, PostQuitMessage,
    DefWindowProc, DefDlgProc, DrawIcon, InvalidateRect,
    SetWindowLong, SetWindowLongPtr, GetWindowLong, GetWindowLongPtr, SetScrollInfo,
    SetFocus, SetWindowText, SetCapture, ReleaseCapture,
    GetKeyState, SetMenu, MessageBeep,
    CreateCaret, DestroyCaret, SetCaretPos,
    GetSystemMetrics, GetSysColor, SendMessage, PostMessage,
    GetWindowPlacement, SetWindowPlacement,
    CheckMenuItem, EnableMenuItem, SetMenuItemInfo, DrawMenuBar, TrackPopupMenu,
    IsClipboardFormatAvailable, SetClipboardData, GetClipboardData,
    (* MACROS *)
    LOWORD, HIWORD, MAKELONG,
    (* TYPES *)
    WNDCLASS, MSG, PAINTSTRUCT, WNDPROC,
    LPCREATESTRUCT, WINDOWPLACEMENT, SCROLLINFO,
    MINMAXINFO, LPWINDOWPOS, LPHELPINFO, MENUITEMINFO,
    TIMERPROC, LPNMHDR,
    (* CONSTS *)
    WM_CREATE, WM_COMMAND, WM_PAINT, WM_CLOSE, WM_QUERYENDSESSION, WM_DESTROY,
    WM_NOTIFY, WM_SETFONT, WM_SETREDRAW, WM_QUIT, WM_GETTEXT, WM_GETTEXTLENGTH,
    WM_ACTIVATE, WM_CHAR, WM_ERASEBKGND, WM_CONTEXTMENU, WM_HELP,
    WM_SETFOCUS, WM_KILLFOCUS, WM_HSCROLL, WM_VSCROLL,
    WM_LBUTTONDOWN, WM_MBUTTONDOWN, WM_RBUTTONDOWN,
    WM_LBUTTONUP, WM_MBUTTONUP, WM_RBUTTONUP,
    WM_LBUTTONDBLCLK, WM_MBUTTONDBLCLK, WM_RBUTTONDBLCLK,
    WM_MOUSEMOVE, WM_MOUSEWHEEL, WM_MOUSEACTIVATE, WM_CAPTURECHANGED,
    WM_ACTIVATEAPP, WM_INITMENU, WM_TIMER, WM_NEXTDLGCTL,
    WM_SIZE, WM_KEYDOWN, WM_QUERYDRAGICON, WM_ICONERASEBKGND, WM_SETCURSOR,
    WM_MENUSELECT, WM_GETMINMAXINFO, WM_APP, WM_GETICON, WM_SETICON,
    WM_WINDOWPOSCHANGING, WM_MOVE, WM_NCHITTEST,
    DM_SETDEFID,
    WA_INACTIVE,
    MA_NOACTIVATE, MA_ACTIVATE,
    GWL_STYLE, GWL_EXSTYLE, GWL_USERDATA, GWL_ID, GWL_WNDPROC, GW_OWNER,
    WS_OVERLAPPED, WS_CLIPCHILDREN, WS_CHILD, WS_VISIBLE, WS_POPUP,
    WS_CAPTION, WS_MINIMIZEBOX, WS_MAXIMIZEBOX, WS_THICKFRAME,
    WS_BORDER, WS_VSCROLL, WS_HSCROLL, WS_SYSMENU, WS_CLIPSIBLINGS,
    WS_TABSTOP, WS_GROUP,
    WS_EX_CLIENTEDGE, WS_EX_CONTROLPARENT, WS_EX_TOPMOST, WS_EX_TRANSPARENT,
    CS_BYTEALIGNCLIENT, CS_DBLCLKS, CS_HREDRAW, CS_VREDRAW,
    CW_USEDEFAULT,
    COLOR_WINDOW, COLOR_BTNFACE,
    COLOR_HIGHLIGHT, COLOR_HIGHLIGHTTEXT, COLOR_GRAYTEXT,
    PM_NOREMOVE, PM_REMOVE,
    TPM_LEFTBUTTON, TPM_RIGHTBUTTON, TPM_LEFTALIGN,
    IDI_APPLICATION, IDC_ARROW, IDC_WAIT, IDC_IBEAM, IDC_CROSS,
    IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW, IDC_SIZEWE, IDC_SIZEALL,
    MK_LBUTTON, MK_RBUTTON, MK_MBUTTON, MK_CONTROL, MK_SHIFT, WHEEL_DELTA,
    SB_LINEUP, SB_THUMBPOSITION, SB_BOTTOM, SB_LINELEFT, SB_RIGHT,
    SB_HORZ, SB_VERT, SB_ENDSCROLL, SB_THUMBTRACK,
    SIF_PAGE, SIF_RANGE, SIF_DISABLENOSCROLL, SIF_POS,
    VK_PRIOR, VK_DOWN, VK_DELETE, VK_INSERT, VK_CONTROL, VK_SHIFT, VK_TAB,
    VK_F1, VK_F12, VK_MENU,
    HELP_CONTEXT, HELP_FINDER, HELP_QUIT,
    SW_HIDE, SW_SHOW, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED,
    SW_SHOWMINNOACTIVE, SW_MINIMIZE, SW_SHOWNA,
    SIZE_MINIMIZED, SIZE_MAXIMIZED, SIZE_RESTORED,
    MB_OK, MB_ICONSTOP, MB_SETFOREGROUND, MB_TASKMODAL,
    MB_ICONQUESTION, MB_ICONEXCLAMATION, MB_TOPMOST,
    MF_SYSMENU, MF_CHECKED, MF_UNCHECKED, MF_BYCOMMAND, MF_BYPOSITION,
    MF_ENABLED, MF_GRAYED, MF_STRING, MF_SEPARATOR,
    MIIM_TYPE, MIIM_SUBMENU, MFT_STRING,
    CF_TEXT, CF_UNICODETEXT,
    SM_CYCAPTION, SM_CXSIZE, SM_CYSIZEFRAME, SM_CXDOUBLECLK, SM_CYDOUBLECLK,
    SM_CXFULLSCREEN, SM_CYFULLSCREEN,
    IMAGE_ICON, LR_SHARED, LR_DEFAULTCOLOR,
(*MVN+*)
    HWND_TOPMOST, HWND_NOTOPMOST,
(*MVN-*)
    HWND_TOP, HWND_BOTTOM, HWND_DESKTOP,
    SWP_NOMOVE, SWP_NOSIZE, SWP_SHOWWINDOW, SWP_NOACTIVATE, SWP_NOZORDER, SWP_NOREDRAW,
    SWP_HIDEWINDOW, SWP_ASYNCWINDOWPOS,
    HTCLIENT, HTTRANSPARENT, RT_BITMAP, DLGWINDOWEXTRA,
    DS_3DLOOK,
    BS_AUTOCHECKBOX, BS_AUTORADIOBUTTON, BS_PUSHBUTTON, BS_NOTIFY, BS_PUSHLIKE,
    BS_GROUPBOX, BS_DEFPUSHBUTTON,
    BST_CHECKED,
    BN_CLICKED, BN_DBLCLK, BN_SETFOCUS, BN_KILLFOCUS,
    LBN_SELCHANGE, LBN_SETFOCUS, LBN_KILLFOCUS, LBN_DBLCLK,
    CBS_HASSTRINGS, CBS_AUTOHSCROLL, CBS_DROPDOWN, CBS_DROPDOWNLIST,
    CB_ERR,
    CBN_EDITCHANGE, CBN_SETFOCUS, CBN_KILLFOCUS, CBN_SELCHANGE, CBN_DBLCLK, CBN_DROPDOWN, CBN_CLOSEUP,
    SS_LEFT, SS_CENTER, SS_RIGHT,
    ES_LEFT, ES_CENTER, ES_RIGHT, ES_MULTILINE, ES_AUTOHSCROLL, ES_AUTOVSCROLL,
    ES_NUMBER, ES_WANTRETURN, ES_PASSWORD, EN_CHANGE, EN_SETFOCUS, EN_KILLFOCUS;
IMPORT WINUSER;

FROM WINGDI IMPORT
    TEXTMETRIC, BITMAP, LPBITMAPINFOHEADER, BITMAPINFO, BITMAPINFOHEADER, BI_RGB,
    GetDeviceCaps, BITSPIXEL, PLANES, LOGPIXELSY, LOGPIXELSX,
    LOGFONT, FW_LIGHT, FW_NORMAL, FW_DEMIBOLD, FW_BOLD, FW_HEAVY,
    OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
    CreateFontIndirect,
    DEFAULT_QUALITY, FF_DONTCARE,
    NULL_BRUSH, WHITE_BRUSH, BLACK_BRUSH, NULL_PEN, WHITE_PEN, BLACK_PEN,
    CreateSolidBrush,
    GetClipBox, SIMPLEREGION, ExcludeClipRect,
    GetRValue, GetGValue, GetBValue, RGB, RGBQUAD, RGBTRIPLE,
    CreateDIBitmap, CBM_INIT, LPBITMAPINFO, DIB_RGB_COLORS,
    SetBkColor, SetTextColor, SetBkMode, TRANSPARENT,
    TextOut, ExtTextOut, ETO_OPAQUE, ETO_CLIPPED, SetTextCharacterExtra,
    PatBlt, BitBlt, StretchBlt, SetStretchBltMode, COLORONCOLOR,
    PATCOPY, DSTINVERT, PATINVERT, SRCCOPY, SRCINVERT,
    SetROP2, R2_COPYPEN, R2_NOT, R2_XORPEN,
    TA_CENTER, TA_BASELINE, TA_LEFT, TA_TOP,
    ExtCreatePen, LOGBRUSH, BS_SOLID,
    PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_USERSTYLE,
    PS_COSMETIC, PS_GEOMETRIC, PS_JOIN_BEVEL, PS_JOIN_MITER,
    PS_JOIN_ROUND, PS_ENDCAP_ROUND, PS_ENDCAP_FLAT, PS_ENDCAP_SQUARE,
    MoveToEx, LineTo, Rectangle, Polyline, Pie, SetPixelV,
    CreateCompatibleDC, DeleteDC,
    CreateCompatibleBitmap, CreateDIBSection;
IMPORT WINGDI;

FROM COMMCTRL IMPORT
    INITCOMMONCONTROLSEX,
    ICC_WIN95_CLASSES, ICC_USEREX_CLASSES, ICC_COOL_CLASSES,
    ICC_DATE_CLASSES, ICC_INTERNET_CLASSES, (*ICC_STANDARD_CLASSES,*)
    WC_TABCONTROL, TCN_SELCHANGE, TCITEM, TCIF_TEXT, WC_STATIC,
    TCS_BOTTOM, TCS_VERTICAL, TCS_RIGHT, TCS_MULTILINE, TCS_FOCUSNEVER,
    TCHITTESTINFO, TCHT_ONITEM, TC_ITEM,
    TabCtrl_AdjustRect, TabCtrl_DeleteAllItems,
    TabCtrl_GetCurSel, TabCtrl_SetCurSel, TabCtrl_GetItemCount, TabCtrl_HitTest,
    TabCtrl_InsertItem, TabCtrl_DeleteItem, TabCtrl_GetItem, TabCtrl_SetItem,
    TBBUTTON, LPTBNOTIFY, TBADDBITMAP,
    HINST_COMMCTRL, NIL_TBBUTTON,
    IDB_STD_LARGE_COLOR,
    CCS_ADJUSTABLE,
    TB_GETTOOLTIPS, TB_BUTTONCOUNT, TB_AUTOSIZE, TB_ADDBITMAP,
    TB_ADDSTRING, TB_DELETEBUTTON, TB_ADDBUTTONS, TB_GETBUTTON,
    TB_ISBUTTONHIDDEN, TB_ISBUTTONCHECKED, TB_ISBUTTONENABLED,
    TB_HIDEBUTTON, TB_CHECKBUTTON, TB_ENABLEBUTTON,
    TBSTATE_ENABLED,
    TBN_QUERYDELETE, TBN_GETBUTTONINFO, TBN_QUERYINSERT,
    TBN_BEGINADJUST, TBN_TOOLBARCHANGE,
    SB_SETTEXT, SB_SETPARTS,
    TBSTYLE_TOOLTIPS, TBSTYLE_CHECK, TBSTYLE_BUTTON, TBSTYLE_SEP,
    CreateStatusWindow, CreateToolbarEx,
    WC_LISTVIEW, UPDOWN_CLASS, WC_TREEVIEW,
    LVITEM, LVCOLUMN, LVFINDINFO, LVS_REPORT, LVS_SINGLESEL, LVS_SHOWSELALWAYS,
    LVS_EX_GRIDLINES,
    LVS_NOCOLUMNHEADER, LVS_NOSORTHEADER, LVS_EX_FULLROWSELECT, LVS_EX_SUBITEMIMAGES,
    LVSIL_SMALL, LVIF_TEXT, LVIF_PARAM, LVIF_IMAGE,
    LVCF_WIDTH, LVCF_FMT, LVCF_TEXT, LVCF_SUBITEM,
    LVCFMT_LEFT, LVCFMT_CENTER, LVCFMT_RIGHT,
    LVFI_STRING, LVFI_PARAM, LVFI_PARTIAL,
    LVIS_SELECTED, LVIS_FOCUSED, LVNI_SELECTED,
    LVSCW_AUTOSIZE, LVSCW_AUTOSIZE_USEHEADER, LPNMLISTVIEW,
    ListView_EnsureVisible, ListView_GetItemState, ListView_SetItemState, ListView_GetNextItem,
    ListView_DeleteAllItems, ListView_DeleteItem, ListView_InsertItem,
    ListView_SetExtendedListViewStyle, ListView_InsertColumn, ListView_SetColumnWidth,
    ListView_SetItemText, ListView_GetItemCount, ListView_GetItemText,
    ListView_DeleteColumn, ListView_FindItem, ListView_GetSelectedCount,
    ListView_GetItem, ListView_SetItem, ListView_GetColumnWidth, ListView_SortItems,
    ListView_SortItemsEx, ListView_GetHeader, ListView_SetImageList,
    HDITEM, HDI_FORMAT, HDI_IMAGE, HDF_BITMAP_ON_RIGHT, HDF_IMAGE, HDI_TEXT,
    Header_SetImageList, Header_GetItem, Header_SetItem,
    NM_CLICK, NM_DBLCLK, NM_KILLFOCUS, NM_SETFOCUS, NM_RETURN,
    LVN_ITEMCHANGED, LVN_COLUMNCLICK,
    UDS_ALIGNRIGHT, UDS_SETBUDDYINT, UDS_NOTHOUSANDS, UDS_ARROWKEYS,
    UDM_SETBUDDY,
    UpDown_SetRange32, UpDown_SetPos,
    TOOLINFO, TOOLTIPS_CLASS, TTS_NOPREFIX,
    LPSTR_TEXTCALLBACK,
    TTM_ADDTOOL, TTM_DELTOOL, TTM_SETMAXTIPWIDTH,
    TTF_SUBCLASS, TTF_IDISHWND,
    TTN_GETDISPINFO, LPNMTTDISPINFO,
    HTREEITEM, TVITEM, TVINSERTSTRUCT, LPNMTREEVIEW,
    TVS_HASBUTTONS, TVS_SHOWSELALWAYS, TVS_HASLINES, TVS_LINESATROOT,
    TVIS_SELECTED, TVSIL_NORMAL,
    TVIF_TEXT, TVIF_PARAM, TVIF_IMAGE, TVIF_SELECTEDIMAGE,
    TVN_SELCHANGED, TVN_ITEMEXPANDED,
    TVE_COLLAPSE, TVE_EXPAND, TVI_ROOT, TVI_LAST, TVI_FIRST,
    TreeView_InsertItem, TreeView_SetItem, TreeView_GetItem, TreeView_SelectItem,
    TreeView_GetSelection, TreeView_Expand, TreeView_GetChild, TreeView_GetItemRect,
    TreeView_GetRoot, TreeView_GetNextSibling, TreeView_GetPrevSibling,
    TreeView_SetImageList, TreeView_DeleteItem, TreeView_GetParent, TreeView_SortChildren,
    TreeView_DeleteAllItems,
    HIMAGELIST, ILC_COLOR, ILC_MASK, CLR_NONE,
    ImageList_Create, ImageList_Add, ImageList_AddMasked, ImageList_AddIcon,
    ImageList_GetImageCount, ImageList_Destroy, ImageList_GetIconSize,
    ImageList_SetBkColor,
    HDN_ENDTRACK, HDN_BEGINTRACK, LPNMHEADER;

FROM WINX IMPORT
    NIL_STR, NIL_RECT, NIL_WINT, NIL_POINT,
    Instance, SubclassWindow,
    ShowWindowGen,
    SelectBrush, DeleteBrush, SelectFont, SelectPen, DeletePen,
    SelectBitmap, GetBITMAP,
    GetStockBrush, GetStockPen,
    SetWindowFont,
    Button_SetCheck, Button_GetCheck, Button_GetTextLength, Button_SetText,
    Button_GetText, Button_Click, Button_SetStyle,
    Static_GetTextLength, Static_GetText, Static_SetText,
    Edit_GetTextLength, Edit_SetText, Edit_GetText, Edit_LimitText, Edit_SetReadOnly,
    Edit_SetSel, Edit_ReplaceSel, Edit_ScrollCaret, Edit_GetLineCount, Edit_LineIndex,
    Edit_LineLength, Edit_GetLine,
    ComboBox_SetCurSel, ComboBox_GetCurSel, ComboBox_SetText,
    ComboBox_GetText, ComboBox_ResetContent, ComboBox_DeleteString,
    ComboBox_AddString, ComboBox_InsertString, ComboBox_LimitText,
    ComboBox_FindString, ComboBox_GetCount, ComboBox_SetExtendedUI,
    ComboBox_GetItemData, ComboBox_SetItemData, ComboBox_FindItemData,
    ComboBox_GetLBText, ComboBox_GetLBTextLen, IsCurrentThread;

IMPORT WINX;

FROM HTMLHELP IMPORT
    HH_INITIALIZE, HH_UNINITIALIZE, HH_PRETRANSLATEMESSAGE,
    HH_HELP_CONTEXT, HH_DISPLAY_TOC,
    HtmlHelp;

%IF %NOT Unicode %THEN
FROM WINNLS IMPORT
    MultiByteToWideChar, CP_ACP;
%END

FROM SplitterControl IMPORT
    WC_CCSPLITTER, CCSPN_SPLITCHANGED, CCSPN_POSCHANGED,
    CCSPV_TOP, CCSPV_BOTTOM, CCSPV_LEFT, CCSPV_RIGHT, CCsplitter_SetHandle,
    CCSPLIT, CCSP_POS, CCSP_FPOS, CCSP_FLOWER, CCSP_FUPPER,
    CCsplitter_SetSplit, CCsplitter_GetSplit,
    CCsplitter_PositionViews, CCsplitter_SetActiveView, CCsplitter_SetActive;

FROM Gdiplus IMPORT
    GdiplusStartupInput, GdiplusStartupOutput,
    InterpolationMode, GpUnit, ARGB, MakeARGB, GpImage, GpBitmap, GpImageAttributes,
    GpStatus, GpGraphics, DrawImageAbort;

FROM Threads IMPORT
    AllocateTlsIndex, GetTlsData, SetTlsData, GetThreadFromSystemId,
(*MVN+*)
    Thread, CreateThread,
(*MVN-*)
    YieldThread;

IMPORT Uxtheme;

CONST
    MagicNumber         = 8048454Ch;
    MaxModeless         = 16;
    MaxIdleProcs        = 8;
    MaxNotifyProcs      = 8;
    MaxFields           = 32;

    FirstChild_ID       = 100;
    TOOLBAR_ID          = FirstChild_ID - 1;
    STATUSLINE_ID       = FirstChild_ID - 2;
    DRAG_ID             = FirstChild_ID - 3;
    DRAWCLIENT_ID       = FirstChild_ID - 4;
    TABCLIENT_ID        = FirstChild_ID - 5;
    FORMCLIENT_ID       = FirstChild_ID - 6;
    SPLITTER_ID         = FirstChild_ID - 7;
    LISTCLIENT_ID       = FirstChild_ID - 8;
    TREECLIENT_ID       = FirstChild_ID - 9;

    TimerIdFudge        = 10;
    DragTimer           = 1;
    DragTimerTimeout    = 250;

    MaxUserData         = 31;

    StrCacheElements    = 32;
    StrCacheStringSize  = 128;

    MaxContextStack     = 4;

    FormUnitX           = 4;
    FormUnitY           = 8;

    DragHandleSize      = 6;

    NewLine             : ARRAY [0..2] OF CHAR = {CHR(13), CHR(10), ''};

    WindowDataPos       : ARRAY ClientTypes OF INTEGER =
        {0,(*draw*)
         GWL_USERDATA,(*list*)
         GWL_USERDATA,(*tree*)
         DLGWINDOWEXTRA,(*form*)
         GWL_USERDATA,(*tab*)
         GWL_USERDATA(*splitter*)
         };

    Win32R2Ops          : ARRAY DrawFunctions OF DWORD =
        {R2_COPYPEN, R2_NOT, R2_XORPEN};
    Win32PatBltOps      : ARRAY DrawFunctions OF DWORD =
        {PATCOPY, DSTINVERT, PATINVERT};
    Win32BitBltOps      : ARRAY DrawFunctions OF DWORD =
        {SRCCOPY, DSTINVERT, SRCINVERT};

TYPE
    PMINMAXINFO         = POINTER TO MINMAXINFO;

    MenuHandle          = HMENU;
    BitmapHandle        = HBITMAP;
    ImageListHandle     = HIMAGELIST;
    FontHandle          = HFONT;
    PrintDriverInfo     = POINTER TO RECORD END;(* magic cookie *)
    TreeClientNode      = HTREEITEM;

    HitTest         = (HtClient,
                       HtTopLeft, HtTop, HtTopRight,
                       HtLeft, HtRight,
                       HtBottomLeft, HtBottom, HtBottomRight);

    StatusbarInfo       =
        RECORD
            wnd             : HWND;
            charWidth       : CARDINAL;
            numFields       : CARDINAL;
            fields          : ARRAY [0..MaxFields-1] OF INTEGER;
        END;
    StatusbarInfoPointer        = POINTER TO StatusbarInfo;

    TbButtonInfo =
        RECORD
            bmpHandle       : HBITMAP;
            bmpId           : CARDINAL;
            textId          : CARDINAL;
            helpId          : CARDINAL;
            actionId        : CARDINAL;
            visible         : BOOLEAN;
            type            : ToolbarButtonTypes;
        END;

    ToolbarCreateInfo =
        (* used so an Mdi child can copy the frame toolbar when it is made Toplevel *)
        RECORD
            buttons         : POINTER TO ARRAY [0..0] OF ToolbarButtonInfo;
            fmt             : POINTER TO ARRAY [0..0] OF CARDINAL;
            numButtons      : CARDINAL;
            numFmt          : CARDINAL;
        END;

    ToolbarInfo =
        RECORD
            wnd             : HWND;
            tipWnd          : HWND;
            buttons         : POINTER TO ARRAY [0..0] OF TbButtonInfo;
            numButtons      : CARDINAL;
            queryNum        : CARDINAL;
            customize       : BOOLEAN;
            hasText         : BOOLEAN;
            hasHelp         : BOOLEAN;
            createInfo      : ToolbarCreateInfo;
        END;
    ToolbarInfoPointer  = POINTER TO ToolbarInfo;

    <*/PUSH/PACK*>
    DIB_Pixel =
        RECORD
            blue, green, red        : CARDINAL8;
        END;
    DIB_PixelRow                = POINTER TO ARRAY [0..0] OF DIB_Pixel;

    Rgb8_Pixel =
        RECORD
            red, green, blue        : CARDINAL8;
        END;
    Rgba8_Pixel =
        RECORD
            red, green, blue, alpha : CARDINAL8;
        END;
    Rgb16_Pixel =
        RECORD
            red, green, blue        : CARDINAL16;
        END;
    Rgba16_Pixel =
        RECORD
            red, green, blue, alpha : CARDINAL16;
        END;

    Bgr8_Pixel =
        RECORD
            blue, green, red        : CARDINAL8;
        END;
    Bgra8_Pixel =
        RECORD
            blue, green, red, alpha : CARDINAL8;
        END;
    Bgr16_Pixel =
        RECORD
            blue, green, red        : CARDINAL16;
        END;
    Bgra16_Pixel =
        RECORD
            blue, green, red, alpha : CARDINAL16;
        END;

    Abgr8_Pixel =
        RECORD
            alpha, blue, green, red : CARDINAL8;
        END;
    Abgr16_Pixel =
        RECORD
            alpha, blue, green, red : CARDINAL16;
        END;

    RgbPixel =
        RECORD
            CASE : CARDINAL OF
            0: rgb8:Rgb8_Pixel;|
            1: rgba8:Rgba8_Pixel;|
            2: rgb16:Rgb16_Pixel;|
            3: rgba16:Rgba16_Pixel;|

            4: bgr8:Bgr8_Pixel;|
            5: bgra8:Bgra8_Pixel;|
            6: bgr16:Bgr16_Pixel;|
            7: bgra16:Bgra16_Pixel;|

            8: abgr8:Abgr16_Pixel;|
            9: abgr16:Abgr16_Pixel;|
            ELSE
            END;
        END;
    <*/POP*>
    RgbPixelPointer     = POINTER TO RgbPixel;

CONST
    RgbPixelFormatSize  : ARRAY RgbPixelFormat OF CARDINAL =
        {
            SIZE(Rgb8_Pixel),
            SIZE(Rgba8_Pixel),
            SIZE(Rgb16_Pixel),
            SIZE(Rgba16_Pixel),
            SIZE(Bgr8_Pixel),
            SIZE(Bgra8_Pixel),
            SIZE(Bgr16_Pixel),
            SIZE(Bgra16_Pixel),
            SIZE(Abgr8_Pixel),
            SIZE(Abgr16_Pixel)
        };

TYPE
    DrawContextInfo =
        RECORD
            values          : DrawContextValues;
            drawables       : Drawable;
        END;
    DrawContext         = POINTER TO DrawContextInfo;

    DrawableTypes       = (WindowDrawable,
                           OffscreenDrawable,
                           BitmapDrawable,
                           RgbDrawable);

    Drawable            = POINTER TO DrawableInfo;
    DrawableInfo        =
        RECORD
            context         : DrawContext;
            next            : Drawable;

            type            : DrawableTypes;
            wnd             : HWND;
            bmp             : HBITMAP;
            saveBmp         : HBITMAP;

            winDC           : HDC;

            pen             : HPEN;
            penValues       : DrawContextValues;

            brush           : HBRUSH;
            brushColor      : ColorValue;

            dibHeader       : BITMAPINFO;
            dibBits         : ADDRESS;
            width, height   : CARDINAL;
            rowSize         : CARDINAL;
            rowStride       : CARDINAL;

            saveFont        : HFONT;
            savePen         : HPEN;
            saveBrush       : HBRUSH;

            contextStackSp  : CARDINAL;
            contextStack    : ARRAY [1..MaxContextStack] OF DrawContext;
        END;

    ColumnInfoPointer    = POINTER TO ARRAY OF ClientColumnInfo;

    ColumnAutoSizeRec =
        RECORD
            wnd             : HWND;
            info            : ColumnInfoPointer;
            posted          : POINTER TO BOOLEAN;
            numColumns      : CARDINAL;
        END;
    ColumnAutoSizePointer       = POINTER TO ColumnAutoSizeRec;

    ControlInfoPointer  = POINTER TO ControlInfo;

    MultiPageInfo =
        RECORD
            label           : StringData;
            numControls     : CARDINAL;
            fudgeX,
            fudgeY          : COORDINATE;
            firstChild,
            lastChild       : ControlInfoPointer;
        END;
    PageInfoPointer     = POINTER TO ARRAY OF MultiPageInfo;

    ControlInfo =
        RECORD
            next, prev,
            nextSel,
            parent,
            firstChild,
            lastChild       : ControlInfoPointer;
            wnd             : HWND;
            spinWnd         : HWND;(*actual spin control, wnd is the edit control buddy*)
            dragWnd         : HWND;
            w               : Window;(*FormClient*)
            columnInfo      : ColumnInfoPointer;
            pageInfo        : PageInfoPointer;
            idNum           : CARDINAL;
            tipTextId       : INTEGER;
            captionId       : INTEGER;
            x, y,
            width, height   : INTEGER;
            childFudgeX,
            childFudgeY     : COORDINATE;
            group           : CARDINAL;(* 0 = no group*)
            count           : CARDINAL;(*numColumns, numPages, numControls(group)*)
            currentPage     : CARDINAL;
            spinLow,
            spinHigh        : INTEGER;
            editLimit       : CARDINAL;
            hitTest         : HitTest;
            type            : ControlTypes;
            selected        : BOOLEAN;
            visible         : BOOLEAN;
            enabled         : BOOLEAN;
            editable        : BOOLEAN;
            vertScroll      : BOOLEAN;
            horizScroll     : BOOLEAN;
            multiLine       : BOOLEAN;
            password        : BOOLEAN;
            multiSelect     : BOOLEAN;
            default         : BOOLEAN;
            tipCreated      : BOOLEAN;
            pageVisible     : BOOLEAN;
            autoSizePosted  : BOOLEAN;
            prevProc        : WNDPROC;
            textAlign       : TextAlignment;
            caption         : ARRAY [0..255] OF CHAR;
            tipText         : ARRAY [0..255] OF CHAR;
        END;

    DragMode            = (PreDrag, Dragging, NoDrag);

    Window              = POINTER TO WndInstance;
    WndInstance         =
        RECORD
            validate                : CARDINAL;
            next,
            nextSibling,
            menuWindow,
            parent,
            firstChild,
            lastChild,
            active,
            nextActive              : Window;

            wnd                     : HWND;
            clientWnd               : HWND;
            menu                    : HMENU;
            popMenu                 : HMENU;
            activeMenu              : HMENU;
            bigIcon                 : HICON;
            smallIcon               : HICON;
            cursor                  : HCURSOR;
            busySave                : HCURSOR;
            accel                   : HACCEL;

            oldWndProc              : WNDPROC;

    (*MVN+*)
            GuiThread               : Thread;
            GuiEvent                : WIN32.HANDLE;
            modalForm               : BOOLEAN;
    (*MVN-*)

            firstControl,
            lastControl             : ControlInfoPointer;
            numControlsMain         : CARDINAL;
            numControlsAll          : CARDINAL;
            selectedControl         : ControlInfoPointer;
            selectionCount          : CARDINAL;
            nextCtrlId              : CARDINAL;
            formPreEditMode         : BOOLEAN;
            formEditMode            : BOOLEAN;
            formChanged             : BOOLEAN;
            formUsePopupMenu        : BOOLEAN;
            toolTipWnd              : HWND;
            formMenu                : HMENU;
            formFont                : HFONT;
            formCharX,
            formCharY               : COORDINATE;
            dropControl             : POINTER TO ControlProperties;
            propProc                : ControlPropertiesProc;
            newControlProc          : NewControlProc;
            exitProc                : ControlEndEditProc;

            dragDC                  : HDC;
            dragX1, dragY1,
            dragSX, dragSY,
            dragX, dragY,
            dragW, dragH            : INTEGER;
            dragMode                : DragMode;

            columnInfo              : ColumnInfoPointer;
            numColumns              : CARDINAL;
            sortColumn              : INTEGER;
            sortDirection           : SortDirection;
            compareProc             : ListClientSortCompareProc;
            compareData             : ADDRESS;
            headerWnd               : HWND;
            autoSizePosted          : BOOLEAN;
            sortPosted              : BOOLEAN;

            imageList               : HIMAGELIST;
            headerImageList         : HIMAGELIST;

            splitterChild           : ARRAY [0..1] OF Window;

            dc                      : HDC;
            paintLock               : CARDINAL;
            drawable                : Drawable;

            type                    : WindowTypes;
            clientType              : ClientTypes;
            tabPos                  : TabPosition;
            wndProc                 : WindowProcedure;
            defaultDrawContext      : DrawContext;
            statusbar               : StatusbarInfoPointer;
            createData              : ClientCreateDataPointer;
            toolbar                 : ToolbarInfoPointer;
            attr                    : WinAttrSet;
            caretPos                : wsPOINT;
            caretSize               : wsPOINT;
            caretBmp                : HBITMAP;
            caretType               : CaretTypes;
            hideCaret               : CARDINAL;
            x, y                    : COORDINATE;
            width, height           : COORDINATE;
            vScrollMin              : COORDINATE;
            vScrollMax              : COORDINATE;
            vScrollPage             : COORDINATE;
            vScrollPos              : COORDINATE;
            hScrollMin              : COORDINATE;
            hScrollMax              : COORDINATE;
            hScrollPage             : COORDINATE;
            hScrollPos              : COORDINATE;
            maxW                    : COORDINATE;
            minW                    : COORDINATE;
            maxH                    : COORDINATE;
            minH                    : COORDINATE;
            vGrain                  : COORDINATE;
            hGrain                  : COORDINATE;
            offsetX                 : COORDINATE;
            offsetY                 : COORDINATE;
            mouseWheelTotal         : INTEGER;
            tabIndex                : INTEGER;
            splitterView            : CARDINAL;
            updateColors            : CARDINAL;
            busy                    : CARDINAL;
            disable                 : CARDINAL;
            mouseTrap               : CARDINAL;
            notifyCount             : CARDINAL;
            extRefCount             : CARDINAL;
            backgroundColor         : ColorValue;
            hScale                  : REAL;
            vScale                  : REAL;
            createParam             : ADDRESS;
            state                   : WindowStateType;
            hasFocus                : BOOLEAN;
            hasCaret                : BOOLEAN;
            caretCreated            : BOOLEAN;
            firstMinMax             : BOOLEAN;
            disableScrollH          : BOOLEAN;
            disableScrollV          : BOOLEAN;
            calledHelp              : BOOLEAN;
            autoErase               : BOOLEAN;
            delayFocus              : BOOLEAN;
            createClient            : BOOLEAN;
            tabChildNumbers         : BOOLEAN;
            suppressWndProc         : BOOLEAN;
            splitTopBottom          : BOOLEAN;
            modal                   : BOOLEAN;
            returnKeys              : BOOLEAN;
            external                : BOOLEAN;
            userData                : ARRAY [0..MaxUserData] OF MACHINEWORD;
            notifyProcs             : ARRAY [1..MaxNotifyProcs] OF WindowNotifyProcedure;
            notifyData              : ARRAY [1..MaxNotifyProcs] OF MACHINEWORD;
            title                   : ARRAY [0..255] OF CHAR;
        END;

(*MVN+*)
    CreateWindowRecord  =
        RECORD
            parent : Window;
            wnd : Window;
            name : ARRAY [0..255] OF CHAR;
            menu : ARRAY [0..255] OF CHAR;
            icon : ARRAY [0..255] OF CHAR;
            windowType : WindowTypes;
            clientType : ClientTypes;
            wndProc : WindowProcedure;
            attribs : WinAttrSet;
            x, y : COORDINATE;
            width, height : COORDINATE;
            clientCreateData : ClientCreateDataPointer;
            createParam : ADDRESS;
            GuiEvent : WIN32.HANDLE;
        END;
(*MVN-*)

    TlsInfo             = POINTER TO TlsRecord;
    TlsRecord           =
        RECORD
            accel           : HACCEL;
            accelWnd        : HWND;
            nestLevel       : CARDINAL;
            numModeless     : CARDINAL;
            numIdle         : CARDINAL;
            modeless        : ARRAY [1..MaxModeless] OF HWND;
            idleProcs       : ARRAY [1..MaxIdleProcs] OF IdleProcType;
            idleData        : ARRAY [1..MaxIdleProcs] OF ADDRESS;
        END;

    StrCacheElementRec =
        RECORD
            idNum           : CARDINAL;
            data            : ARRAY [0..StrCacheStringSize-1] OF CHAR;
        END;
    StrCacheElement      = POINTER TO StrCacheElementRec;

    LoadedMenuPointer   = POINTER TO LoadedMenuRecord;
    LoadedMenuRecord    =
        RECORD
            next            : LoadedMenuPointer;
            menuRes         : HMENU;
            menu            : HMENU;
        END;

    CachedBitmapPointer = POINTER TO CachedBitmapRecord;
    CachedBitmapRecord  =
        RECORD
            next            : CachedBitmapPointer;
            bmp             : HBITMAP;
            refs            : CARDINAL;
            len             : CARDINAL;
            name            : ARRAY [0..63] OF CHAR;
        END;

    <*/PUSH/CALLS:WINDOWSCALL/NOHIGH/ENUMSIZE:BIG*>
    GdiplusInfoRec =
        RECORD
            dll             : WIN32.HINSTANCE;
            init            : BOOLEAN;

            startupInput    : GdiplusStartupInput;
            startupOutput   : GdiplusStartupOutput;
            token           : ULONG_PTR;

            (* startup and shutdown procs *)
            GdiplusStartup  : PROCEDURE(VAR OUT ULONG_PTR, GdiplusStartupInput, VAR OUT GdiplusStartupOutput) : GpStatus;
            GdiplusShutdown : PROCEDURE(ULONG_PTR);

            (* bitmap procs *)
            GdipCreateBitmapFromFile        : PROCEDURE(ARRAY OF UCHAR, VAR OUT GpBitmap) : GpStatus;

            GdipCreateHBITMAPFromBitmap     : PROCEDURE(GpBitmap, VAR OUT HBITMAP, ARGB) : GpStatus;

            GdipCreateBitmapFromHBITMAP     : PROCEDURE(HBITMAP, HPALETTE, VAR OUT GpBitmap) : GpStatus;

            GdipCreateBitmapFromGdiDib      : PROCEDURE(BITMAPINFO, ADDRESS, VAR OUT GpBitmap) : GpStatus;

            GdipDisposeImage                : PROCEDURE(GpImage) : GpStatus;

            (* graphics procs *)
            GdipCreateFromHDC               : PROCEDURE(HDC, VAR OUT GpGraphics) : GpStatus;

            GdipDeleteGraphics              : PROCEDURE(GpGraphics) : GpStatus;

            GdipSetInterpolationMode        : PROCEDURE(GpGraphics, InterpolationMode) : GpStatus;

            GdipDrawImageRectRectI           : PROCEDURE(GpGraphics,
                                                         GpImage,
                                                         (*dstX,Y*) INTEGER, INTEGER,
                                                         (*dstW,H*) INTEGER, INTEGER,
                                                         (*srcX,Y*) INTEGER, INTEGER,
                                                         (*srcW,H*) INTEGER, INTEGER,
                                                         (*srcUnit*)GpUnit,
                                                         (*imageAttrib*)GpImageAttributes,
                                                         (*callback*)DrawImageAbort,
                                                         (*callback data*)ADDRESS) : GpStatus;
        END;
    <*/POP*>

CONST
    DragBoxClassName            = "SBS_WinShell_DragBox";
    ToplevelClassName           = "SBS_WinShell_Toplevel";
    DrawClientClassName         = "SBS_WinShell_DrawClient";
    FormClientClassName         = "SBS_WinShell_FormClient";

    Alphabet            = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
                          "abcdefghijklmnopqrstuvwxyz" +
                          "0123456789" +
                          (*" _-+,.;[]{}<>()!@#$%^&*=/|?";*)
                          "_~`!@#$%^&*()-+=?<>[]{}/\:;,.'";

    Digits                      = "0123456789";

    (* Our user defined messages *)

<*/PUSH/NOWARN:U*>
    WM_SETFOCUS_DELAY   = WM_APP + 0;
    WM_COLUMN_AUTOSIZE  = WM_APP + 1;
    WM_LIST_SORT        = WM_APP + 2;
    WM_ENABLE_WINDOW    = WM_APP + 4;
    WM_LIST_REPAINT     = WM_APP + 8;
    WM_USER_USER        = WM_APP + 20;
<*/POP*>

VAR
    Heap                : HeapInfoPointer;

    FirstWindow         : Window;
    DefaultIcon         : HICON;
    DefaultContext      : DrawContext;
    DefaultContextInfo  : DrawContextInfo;
    TlsIndex            : CARDINAL;

    CommCtrlDLL         : WIN32.HINSTANCE;

    HtmlHelpCookie      : DWORD;
    HtmlHelpInit        : BOOLEAN;

    TabControlFont      : HFONT;
    StatuslineFont      : HFONT;
    FormFont            : HFONT;

    HitTestCursors      : ARRAY HitTest OF HCURSOR;
    MoveCursor          : HCURSOR;
    DropCursor          : HCURSOR;

    DragBoxPatternBitmap: HBITMAP;
    DragBoxPatternBrush : HBRUSH;
    DragDC              : HDC;
    DragHandlePrimary   : HBITMAP;
    DragHandleSecondary : HBITMAP;

    DragCX, DragCY      : COORDINATE;

    ResourceInst        : WIN32.HINSTANCE;
    ResourceFile        : FileSpecString;

    StrCache            : ARRAY [0..StrCacheElements-1] OF StrCacheElement;
    StrCacheLock        : CARDINAL;
    StrCacheInit        : BOOLEAN;

    LoadedMenus         : LoadedMenuPointer;

    CachedBitmaps       : CachedBitmapPointer;

    Gdip                : GdiplusInfoRec;
(*MVN+*)
    CreateWindowTemp [VOLATILE]   : CreateWindowRecord;
    Transparent24     : RGBTRIPLE = {0F0H, 0F0H, 0F0H};
(*MVN-*)

PROCEDURE ALLOCATE(VAR OUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    AllocateEx(addr, amount, Heap);
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    DeallocateEx(addr, amount, Heap);
END DEALLOCATE;

PROCEDURE AllocWinShellMem(VAR OUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    AllocateEx(addr, amount, Heap);
END AllocWinShellMem;

PROCEDURE ReallocWinShellMem(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    ReallocateEx(addr, amount, Heap);
END ReallocWinShellMem;

PROCEDURE DeallocWinShellMem(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    DeallocateEx(addr, amount, Heap);
END DeallocWinShellMem;

PROCEDURE Min(a, b : COORDINATE) : COORDINATE [INLINE];
BEGIN
    IF a > b THEN
        RETURN b;
    END;
    RETURN a;
END Min;

PROCEDURE Max(a, b : COORDINATE) : COORDINATE [INLINE];
BEGIN
    IF a > b THEN
        RETURN a;
    END;
    RETURN b;
END Max;

PROCEDURE CreateStringData(str : ARRAY OF CHAR) : StringData;
VAR
    l           : CARDINAL;
    ptr         : StringData;
BEGIN
    l := LENGTH(str);
    IF l <> 0 THEN
        ALLOCATE(ptr, (l+1) * SIZE(CHAR));
        Assign(str, ptr^[0..l]);
        RETURN ptr;
    END;
    RETURN NIL;
END CreateStringData;

PROCEDURE DisposeStringData(strPtr : StringData);
BEGIN
    IF strPtr <> NIL THEN
        DEALLOCATE(strPtr, (StringDataLength(strPtr)+1) * SIZE(CHAR));
    END;
END DisposeStringData;

PROCEDURE DuplicateStringData(strPtr : StringData) : StringData;
VAR
    len         : CARDINAL;
BEGIN
    IF strPtr <> NIL THEN
        len := StringDataLength(strPtr);
        RETURN CreateStringData(strPtr^[0..len]);
    END;
    RETURN NIL;
END DuplicateStringData;

PROCEDURE StringDataLength(strPtr : StringData) : CARDINAL;
VAR
    i   : ADRCARD;
BEGIN
    i := 0;
    IF strPtr <> NIL THEN
        WHILE strPtr^[i] <> '' DO
            INC(i);
        END;
    END;
    RETURN i;
END StringDataLength;

PROCEDURE AssignStringData(strPtr : StringData; VAR OUT str : ARRAY OF CHAR);
VAR
    i           : ADRCARD;
    highStr     : ADRCARD;
BEGIN
    highStr := HIGH(str);
    i := 0;
    IF strPtr <> NIL THEN
        WHILE (i <= highStr) AND (strPtr^[i] <> '') DO
            str[i] := strPtr^[i];
            INC(i);
        END;
    END;
    IF i <= highStr THEN
        str[i] := '';
    END;
END AssignStringData;

PROCEDURE SetResourceFile(name : ARRAY OF CHAR) : BOOLEAN;
VAR
    inst        : HINSTANCE;
BEGIN
    ResourceFile := name;
    IF name[0] = '' THEN
        ResourceInst := Instance;
        RETURN TRUE;
    ELSE
        inst := LoadLibrary(name);
        IF inst <> NIL THEN
            ResourceInst := inst;
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END SetResourceFile;

PROCEDURE GetResourceFile(VAR OUT name : ARRAY OF CHAR);
BEGIN
    name := ResourceFile;
END GetResourceFile;

PROCEDURE GetResNameAndId(name : ARRAY OF CHAR; VAR OUT resName : ARRAY OF CHAR; VAR OUT resId : CARDINAL);
VAR
    i, j, l     : ADRCARD;
    numStr      : ARRAY [0..7] OF CHAR;
BEGIN
    resName := "";
    resId := 0;

    i := 0;
    l := LENGTH(name);
    LOOP
        IF i < l THEN
            IF name[i] <> '#' THEN
                INC(i);
            ELSE
                IF i <> 0 THEN
                    resName := name[0..i-1];
                END;
                INC(i);

                j := 0;
                WHILE i < l DO
                    IF j < HIGH(numStr) THEN
                        numStr[j] := name[i];
                        INC(j);
                    END;
                    INC(i);
                END;
                numStr[j] := '';
                IF StrToCard(numStr, resId) THEN
                END;
                RETURN;
            END;
        ELSE
            resName := name;
            RETURN;
        END;
    END;
END GetResNameAndId;

PROCEDURE MyLoadBitmap(inst : HINSTANCE; name : ARRAY OF CHAR) : HBITMAP;
VAR
    resName     : ARRAY [0..63] OF CHAR;
    resId       : CARDINAL;
    bmp         : HBITMAP;
BEGIN
    bmp := NIL;
    GetResNameAndId(name, resName, resId);
    IF resName[0] <> '' THEN
        bmp := WINUSER.LoadBitmap(inst, resName);
    END;
    IF (bmp = NIL) AND (resId <> 0) THEN
        bmp := WINX.LoadBitmapId(inst, resId);
    END;
    RETURN bmp;
END MyLoadBitmap;

PROCEDURE MyLoadMenu(inst : HINSTANCE; name : ARRAY OF CHAR) : HMENU;
VAR
    resName     : ARRAY [0..63] OF CHAR;
    resId       : CARDINAL;
    menu        : HMENU;
BEGIN
    menu := NIL;
    GetResNameAndId(name, resName, resId);
    IF resName[0] <> '' THEN
        menu := WINUSER.LoadMenu(inst, resName);
    END;
    IF (menu = NIL) AND (resId <> 0) THEN
        menu := WINX.LoadMenuId(inst, resId);
    END;
    RETURN menu;
END MyLoadMenu;

PROCEDURE MyLoadAccelerators(inst : HINSTANCE; name : ARRAY OF CHAR) : HACCEL;
VAR
    resName     : ARRAY [0..63] OF CHAR;
    resId       : CARDINAL;
    accel       : HACCEL;
BEGIN
    accel := NIL;
    GetResNameAndId(name, resName, resId);
    IF resName[0] <> '' THEN
        accel := WINUSER.LoadAccelerators(inst, resName);
    END;
    IF (accel = NIL) AND (resId <> 0) THEN
        accel := WINX.LoadAcceleratorsId(inst, resId);
    END;
    RETURN accel;
END MyLoadAccelerators;

PROCEDURE MyLoadIcon(inst : HINSTANCE; name : ARRAY OF CHAR; width, height : CARDINAL) : HICON;
VAR
    resName     : ARRAY [0..63] OF CHAR;
    resId       : CARDINAL;
    icon        : HICON;
BEGIN
    icon := NIL;
    GetResNameAndId(name, resName, resId);
    IF resName[0] <> '' THEN
        IF width = 0 THEN
            icon := WINUSER.LoadIcon(inst, resName);
        ELSE
            icon := LoadImage(inst, resName, IMAGE_ICON, width, height, LR_DEFAULTCOLOR BOR LR_SHARED);
        END;
    END;
    IF (icon = NIL) AND (resId <> 0) THEN
        IF width = 0 THEN
            icon := WINX.LoadIconId(inst, resId);
        ELSE
            icon := WINX.LoadImageId(inst, resId, IMAGE_ICON, width, height, LR_DEFAULTCOLOR BOR LR_SHARED);
        END;
    END;
    RETURN icon;
END MyLoadIcon;

PROCEDURE MyFindResource(inst : HINSTANCE; name, type : ARRAY OF CHAR) : HRSRC;
VAR
    resName     : ARRAY [0..63] OF CHAR;
    resId       : CARDINAL;
    idStr       : LPTSTR;
    res         : HRSRC;
BEGIN
    res := NIL;
    GetResNameAndId(name, resName, resId);
    IF resName[0] <> '' THEN
        res := FindResource(inst, resName, type);
    END;
    IF (res = NIL) AND (resId <> 0) THEN
        idStr := WINUSER.MAKEINTRESOURCE(resId);
        res := FindResource(inst, idStr^, type);
    END;
    RETURN res;
END MyFindResource;

PROCEDURE GetThreadInfo() : TlsInfo;
VAR
    i           : ADRCARD;
    info        : TlsInfo;
BEGIN
    info := GetTlsData(TlsIndex);
    IF info = NIL THEN
        NEW(info);

        info^.nestLevel := 0;

        info^.accel := NIL;
        info^.accelWnd := NIL;

        info^.numModeless := 0;
        FOR i := 1 TO MaxModeless DO
            info^.modeless[i] := NIL;
        END;

        info^.numIdle := 0;
        FOR i := 1 TO MaxIdleProcs DO
            info^.idleProcs[i] := NILPROC;
            info^.idleData[i] := NIL;
        END;

        IF NOT SetTlsData(TlsIndex, info) THEN
            DISPOSE(info);
        END;
    END;
    RETURN info;
END GetThreadInfo;

PROCEDURE ComputeAverageCharWidth(dc : HDC) : CARDINAL;
VAR
    size        : WIN32.WSIZE;
    ave         : INTEGER;
BEGIN
    WINGDI.GetTextExtentPoint32(dc, Alphabet, LENGTH(Alphabet), size);
    ave := size.cx / INT(LENGTH(Alphabet));

    (* round up *)
    size.cx := size.cx REM INT(LENGTH(Alphabet));
    IF size.cx >= (ave / 2) THEN
        INC(ave);
    END;

    RETURN ave;
END ComputeAverageCharWidth;

PROCEDURE ComputeAverageDigitWidth(dc : HDC) : CARDINAL;
VAR
    size        : WIN32.WSIZE;
    ave         : INTEGER;
BEGIN
    WINGDI.GetTextExtentPoint32(dc, Digits, LENGTH(Digits), size);
    ave := size.cx / INT(LENGTH(Digits));

    (* round up *)
    size.cx := size.cx REM INT(LENGTH(Digits));
    IF size.cx >= (ave / 2) THEN
        INC(ave);
    END;

    RETURN ave;
END ComputeAverageDigitWidth;

PROCEDURE FindWindow(wnd : HWND) :  Window;
VAR
    w   : Window;
BEGIN
    w := FirstWindow;
    WHILE w <> NIL DO
        IF (w^.wnd = wnd) OR (w^.clientWnd = wnd) THEN
            RETURN w;
        END;
        w := w^.next;
    END;
    RETURN NIL;
END FindWindow;

PROCEDURE FindTooltipWindow(wnd : HWND) :  Window;
VAR
    w   : Window;
BEGIN
    w := FirstWindow;
    WHILE w <> NIL DO
        IF (w^.toolTipWnd = wnd) OR
           ((w^.toolbar <> NIL) AND (w^.toolbar^.tipWnd = wnd))
        THEN
            RETURN w;
        END;
        w := w^.next;
    END;
    RETURN NIL;
END FindTooltipWindow;

PROCEDURE GetToplevel(w : Window) : Window;
BEGIN
    WHILE w^.type <> ToplevelWindow DO
        w := w^.parent;
    END;
    RETURN w;
END GetToplevel;

PROCEDURE IsTabChild(w : Window) : BOOLEAN;
BEGIN
    IF (w^.parent <> NIL) AND (w^.parent^.clientType = TabClient) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END IsTabChild;

PROCEDURE IsSplitterChild(w : Window) : BOOLEAN;
BEGIN
    IF (w^.parent <> NIL) AND (w^.parent^.clientType = SplitterClient) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END IsSplitterChild;

PROCEDURE FindControl(w : Window; ctrlId : CARDINAL) : ControlInfoPointer;

    PROCEDURE find(ctrl : ControlInfoPointer; ctrlId : CARDINAL) : ControlInfoPointer;
    VAR
        ptr     : ControlInfoPointer;
        i       : ADRCARD;
    BEGIN
        WHILE ctrl <> NIL DO
            IF ctrl^.idNum = ctrlId THEN
                RETURN ctrl;
            END;

            IF (ctrl^.type = MultiPage) AND (ctrl^.count > 0) THEN
                <*/PUSH/NOWARN:U*>
                FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
                <*/POP*>
                    ptr := find(ctrl^.pageInfo^[i].firstChild, ctrlId);
                    IF ptr <> NIL THEN
                        RETURN ptr;
                    END;
                END;
            ELSIF (ctrl^.type = GroupBox) AND (ctrl^.count > 0) THEN
                <*/PUSH/NOWARN:U*>
                FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
                <*/POP*>
                    ptr := find(ctrl^.firstChild, ctrlId);
                    IF ptr <> NIL THEN
                        RETURN ptr;
                    END;
                END;
            END;

            ctrl := ctrl^.next;
        END;
        RETURN NIL;
    END find;

BEGIN
    IF IsWindow(w) AND (w^.clientType = FormClient) THEN
        RETURN find(w^.firstControl, ctrlId);
    END;
    RETURN NIL;
END FindControl;

PROCEDURE FindRadioGroup(w : Window; group : CARDINAL) : ControlInfoPointer;

    PROCEDURE find(ctrl : ControlInfoPointer; ctrlId : CARDINAL) : ControlInfoPointer;
    VAR
        ptr     : ControlInfoPointer;
        i       : ADRCARD;
    BEGIN
        WHILE ctrl <> NIL DO
            IF (ctrl^.type = RadioButton) AND (ctrl^.group = group) THEN
                RETURN ctrl;
            END;

            IF (ctrl^.type = MultiPage) AND (ctrl^.count > 0) THEN
                <*/PUSH/NOWARN:U*>
                FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
                <*/POP*>
                    ptr := find(ctrl^.pageInfo^[i].firstChild, ctrlId);
                    IF ptr <> NIL THEN
                        RETURN ptr;
                    END;
                END;
            ELSIF (ctrl^.type = GroupBox) AND (ctrl^.count > 0) THEN
                <*/PUSH/NOWARN:U*>
                FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
                <*/POP*>
                    ptr := find(ctrl^.firstChild, ctrlId);
                    IF ptr <> NIL THEN
                        RETURN ptr;
                    END;
                END;
            END;

            ctrl := ctrl^.next;
        END;
        RETURN NIL;
    END find;

BEGIN
    IF IsWindow(w) AND (w^.clientType = FormClient) THEN
        RETURN find(w^.firstControl, group);
    END;
    RETURN NIL;
END FindRadioGroup;

PROCEDURE GetControlFirstSibling(ctrl : ControlInfoPointer) : ControlInfoPointer;
BEGIN
    IF ctrl^.parent = NIL THEN
        RETURN ctrl^.w^.firstControl;
    ELSE
        IF ctrl^.parent^.type = GroupBox THEN
            RETURN ctrl^.parent^.firstChild;
        ELSIF ctrl^.parent^.type = MultiPage THEN
            RETURN ctrl^.parent^.pageInfo^[ctrl^.parent^.currentPage].firstChild;
        END;
    END;
    RETURN NIL;
END GetControlFirstSibling;

PROCEDURE FormToPixel(w : Window; VAR INOUT x, y : COORDINATE);
BEGIN
    x := (x * w^.formCharX) / FormUnitX;
    y := (y * w^.formCharY) / FormUnitY;
END FormToPixel;

PROCEDURE PixelToForm(w : Window; VAR INOUT x, y : COORDINATE);
VAR
    t   : COORDINATE;
BEGIN
    t := x * FormUnitX;
    x := t / w^.formCharX;
    IF (t REM w^.formCharX) >= (w^.formCharX/2) THEN
        INC(x);
    END;

    t := y * FormUnitY;
    y := t / w^.formCharY;
    IF (t REM w^.formCharY) >= (w^.formCharY/2) THEN
        INC(y);
    END;
END PixelToForm;

PROCEDURE CallNotifyProcs(w : Window; notify : WindowNotification);
VAR
    i   : ADRCARD;
BEGIN
    <*/PUSH/NOWARN:U*>
    FOR i := 1 TO VAL(ADRCARD, w^.notifyCount) DO
    <*/POP*>
        w^.notifyProcs[i](w, notify, w^.notifyData[i]);
    END;
EXCEPT
    RETURN;
END CallNotifyProcs;

PROCEDURE CallWndProc(w : Window; msg : MessageRec) : ResponseType;
VAR
    retVal      : ResponseType;
BEGIN
    IF (w <> NIL) AND
       (
        (NOT w^.suppressWndProc) OR
        (msg.msg = WSM_CLOSE)
       )
    THEN
        retVal := w^.wndProc(w, msg);

        CASE msg.msg OF
        WSM_CLOSE:
            IF msg.closeMode = CM_DICTATE THEN
                CallNotifyProcs(w, WindowClosed);
            END;
        |
        WSM_SIZE:
            CallNotifyProcs(w, WindowSized);
        |
        WSM_STATE:
            IF msg.newState = WindowStateMinimized THEN
                CallNotifyProcs(w, WindowMinimized);
            ELSIF msg.newState = WindowStateMaximized THEN
                CallNotifyProcs(w, WindowMaximized);
            END;
        |
        WSM_POSITIONCHANGED:
            CallNotifyProcs(w, WindowMoved);
        ELSE
        END;

        RETURN retVal;
    END;
    RETURN DEFAULT_HANDLE;
END CallWndProc;

PROCEDURE SetWindowIconOS(w : Window);
BEGIN
    IF HostMajorVersion >= 4 THEN
        IF w^.bigIcon <> NIL THEN
            WINUSER.SendMessage(w^.wnd, WM_SETICON, 1, CAST(LPARAM, w^.bigIcon));
        END;

        IF w^.smallIcon <> NIL THEN
            WINUSER.SendMessage(w^.wnd, WM_SETICON, 0, CAST(LPARAM, w^.smallIcon));
        ELSE
            WINUSER.SendMessage(w^.wnd, WM_SETICON, 0, CAST(LPARAM, w^.bigIcon));
        END;
    END;
END SetWindowIconOS;

(* drawable stuff *)

PROCEDURE SetWin32TextAlign(dc : HDC; textOrigin : TextDrawOrigin);
VAR
    flags       : DWORD;
BEGIN
    flags := WINGDI.GetTextAlign(dc);

    flags := flags BAND BNOT CAST(DWORD, TA_CENTER+TA_BASELINE);

    CASE textOrigin OF
    OriginTopLeft:
        flags := flags BOR TA_TOP BOR TA_LEFT;
    |
    OriginBaseLeft:
        flags := flags BOR TA_BASELINE BOR TA_LEFT;
    |
    OriginTopCenter:
        flags := flags BOR TA_TOP BOR TA_CENTER;
    |
    OriginBaseCenter:
        flags := flags BOR TA_BASELINE BOR TA_CENTER;
    END;

    WINGDI.SetTextAlign(dc, flags);
END SetWin32TextAlign;

PROCEDURE SetupDrawableDC(draw : Drawable);
BEGIN
    draw^.saveFont := NIL;
    draw^.savePen := NIL;
    draw^.saveBrush := NIL;
    draw^.saveBmp := NIL;

    IF draw^.winDC <> NIL THEN
        SetBkMode(draw^.winDC, TRANSPARENT);
        SetTextColor(draw^.winDC, draw^.context^.values.foreground);
        SetBkColor(draw^.winDC, draw^.context^.values.background);
        SetTextCharacterExtra(draw^.winDC, draw^.context^.values.textExtraSpacing);
        SetWin32TextAlign(draw^.winDC, draw^.context^.values.textOrigin);
        SetStretchBltMode(draw^.winDC, COLORONCOLOR);
        SetROP2(draw^.winDC, Win32R2Ops[draw^.context^.values.drawFunc]);
        IF draw^.context^.values.font <> NIL THEN
            draw^.saveFont := SelectFont(draw^.winDC, draw^.context^.values.font);
        END;
    END;
END SetupDrawableDC;

PROCEDURE CleanupDrawableDC(draw : Drawable);
BEGIN
    IF draw^.savePen <> NIL THEN
        SelectPen(draw^.winDC, draw^.savePen);
        draw^.savePen := NIL;
    END;
    IF draw^.pen <> NIL THEN
        DeletePen(draw^.pen);
        draw^.pen :=  NIL;
    END;

    IF draw^.saveBrush <> NIL THEN
        SelectBrush(draw^.winDC, draw^.saveBrush);
        draw^.saveBrush := NIL;
    END;
    IF draw^.brush <> NIL THEN
        DeleteBrush(draw^.brush);
        draw^.brush :=  NIL;
    END;

    IF draw^.saveFont <> NIL THEN
        SelectFont(draw^.winDC, draw^.saveFont);
        draw^.saveFont := NIL;
    END;

    IF draw^.saveBmp <> NIL THEN
        SelectBitmap(draw^.winDC, draw^.saveBmp);
        draw^.saveBmp := NIL;
    END;

    SetROP2(draw^.winDC, R2_COPYPEN);
END CleanupDrawableDC;

PROCEDURE UnlinkDrawable(draw : Drawable);
VAR
    prev        : Drawable;
    ptr         : Drawable;
    context     : DrawContext;
BEGIN
    context := draw^.context;

    IF context <> NIL THEN
        ptr := context^.drawables;
        prev := NIL;
        WHILE (ptr <> NIL) AND (ptr <> draw) DO
            prev := ptr;
            ptr := ptr^.next;
        END;
        IF ptr <> NIL THEN
            IF prev = NIL THEN
                context^.drawables := ptr^.next;
            ELSE
                prev^.next := ptr^.next;
            END;
        END;
    END;

    draw^.context := NIL;
    draw^.next := NIL;
END UnlinkDrawable;

PROCEDURE LinkDrawable(draw : Drawable; context : DrawContext);
BEGIN
    draw^.next := context^.drawables;
    context^.drawables := draw;

    draw^.context := context;
END LinkDrawable;

PROCEDURE InitDrawable(VAR OUT draw : DrawableInfo);
BEGIN
    draw.next := NIL;
    draw.wnd := NIL;
    draw.winDC := NIL;
    draw.bmp := NIL;
    draw.dibBits := NIL;
    draw.pen := NIL;
    draw.brush := NIL;
    draw.savePen := NIL;
    draw.saveBrush := NIL;
    draw.saveBmp := NIL;
    draw.context := NIL;
    draw.contextStackSp := 0;
END InitDrawable;

(******************** COMMON Message Handlers *******************************)

PROCEDURE DoAccelSet(w : Window; active : BOOLEAN);
VAR
    info        : TlsInfo;
BEGIN
    info := GetThreadInfo();
    IF info <> NIL THEN
        IF (w^.type = ToplevelWindow) OR
           ((w^.parent^.type = ToplevelWindow) AND (w^.parent^.clientType = TabClient))
        THEN
            info^.accel := NIL;
            info^.accelWnd := NIL;

            IF active AND (w^.accel <> NIL) THEN
                info^.accel := w^.accel;
                info^.accelWnd := w^.wnd;
            END;
        END;
    END;
END DoAccelSet;

PROCEDURE DoSetFocus(w : Window; wParam : WPARAM) : BOOLEAN;
VAR
    msg         : MessageRec;
BEGIN
    UNREFERENCED_PARAMETER(wParam);

    IF w <> NIL THEN
        w^.hasFocus := TRUE;

        IF w^.hasCaret THEN
            CreateWindowCaret(w);
        END;

        DoAccelSet(w, TRUE);

        IF IsSplitterChild(w) THEN
            CCsplitter_SetActiveView(w^.parent^.clientWnd, w^.splitterView);
        END;

        msg.msg := WSM_GAINFOCUS;
        CallWndProc(w, msg);
    END;
    RETURN FALSE;
END DoSetFocus;

PROCEDURE DoKillFocus(w : Window);
VAR
    msg         : MessageRec;
BEGIN
    IF w <> NIL THEN
        w^.hasFocus := FALSE;

        DoAccelSet(w, FALSE);

        IF w^.hasCaret AND w^.caretCreated THEN
            DestroyCaret();
            w^.caretCreated := FALSE;
        END;

        msg.msg := WSM_LOSEFOCUS;
        CallWndProc(w, msg);
    END;
END DoKillFocus;

PROCEDURE DoActivate(w : Window; active : BOOLEAN);
BEGIN
    IF w <> NIL THEN
        DoAccelSet(w, active);

        IF IsSplitterChild(w) THEN
            CCsplitter_SetActiveView(w^.parent^.clientWnd, w^.splitterView);
        END;
    END;
END DoActivate;

PROCEDURE DoActivateApp(w : Window; wParam : WPARAM; lParam : LPARAM);
VAR
    msg : MessageRec;
BEGIN
    IF w <> NIL THEN
        IF wParam <> 0 THEN
            msg.msg := WSM_ACTIVATEAPP;
        ELSE
            msg.msg := WSM_DEACTIVATEAPP;
        END;

        IF NOT WasSystemDialogBox THEN
            IF GetThreadFromSystemId(CAST(CARDINAL, lParam)) = NIL THEN
                CallWndProc(w, msg);
            END;
        ELSE
            IF wParam <> 0 THEN
                WasSystemDialogBox := FALSE;
            END;
        END;
    END;
END DoActivateApp;

PROCEDURE DoQueryEndSession(w : Window) : ResponseType;
VAR
    msg : MessageRec;
BEGIN
    IF w <> NIL THEN
        msg.msg := WSM_TERMINATEAPP;
        RETURN CallWndProc(w, msg);
    END;
    RETURN DEFAULT_HANDLE;
END DoQueryEndSession;

PROCEDURE DoPaint(w : Window; wnd : HWND);
VAR
    ps          : PAINTSTRUCT;
    msg         : MessageRec;
    dc          : HDC;
    draw        : DrawableInfo;
BEGIN
    IF w <> NIL THEN
        IF w^.updateColors = 0 THEN
            HideCaret(w);

            dc := WINUSER.BeginPaint(wnd, ps);

            IF IsIconic(wnd) THEN
                WINUSER.SendMessage(wnd, WM_ICONERASEBKGND, CAST(WPARAM, dc), 0);
                DrawIcon(dc, 0, 0, w^.bigIcon);
            ELSE
                InitDrawable(draw);

                draw.type := WindowDrawable;
                draw.winDC := dc;
                draw.width := ps.rcPaint.right - ps.rcPaint.left;
                draw.height := ps.rcPaint.bottom - ps.rcPaint.top;

                IF w^.defaultDrawContext <> NIL THEN
                    SelectDrawContext(ADR(draw), w^.defaultDrawContext);
                ELSE
                    draw.context := DefaultContext;
                    SetupDrawableDC(ADR(draw));
                END;

                msg.msg := WSM_PAINT;
                msg.paintRect.x1 := ps.rcPaint.left;
                msg.paintRect.y1 := ps.rcPaint.top;
                msg.paintRect.x2 := ps.rcPaint.right;
                msg.paintRect.y2 := ps.rcPaint.bottom;
                msg.paintDraw := ADR(draw);
                msg.paintContext := w^.defaultDrawContext;
                CallWndProc(w, msg);

                UnlinkDrawable(ADR(draw));
                CleanupDrawableDC(ADR(draw));
            END;

            WINUSER.EndPaint(wnd, ps);

            ShowCaret(w);
        ELSE
            WINUSER.BeginPaint(wnd, ps);
            WINUSER.EndPaint(wnd, ps);

            w^.updateColors := 0;

            InvalidateRect(wnd, NIL_RECT, TRUE);
        END;
    ELSE
        WINUSER.BeginPaint(wnd, ps);
        WINUSER.EndPaint(wnd, ps);
    END;
END DoPaint;

PROCEDURE DoEraseBackground(w : Window; wParam : WPARAM);
VAR
    brush       : HBRUSH;
    oldBrush    : HBRUSH;
    rect        : RECT;
    dc          : HDC;
BEGIN
    IF (w <> NIL) THEN
        IF w^.autoErase THEN
            dc := CAST(HDC, wParam);

            GetClipBox(dc, rect);

            brush := CreateSolidBrush(w^.backgroundColor);
            oldBrush := SelectBrush(dc, brush);
            PatBlt(dc, rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top, PATCOPY);
            SelectBrush(dc, oldBrush);

            DeleteBrush(brush);
        ELSE
(*
            dc := CAST(HDC, wParam);

            GetClipBox(dc, rect);

            brush := WINUSER.GetSysColorBrush(COLOR_BTNFACE);
            oldBrush := SelectBrush(dc, brush);
            PatBlt(dc, rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top, PATCOPY);
            SelectBrush(dc, oldBrush);

            DeleteBrush(brush);
*)
        END;
    END;
END DoEraseBackground;

PROCEDURE DoScroll(w : Window; mess : UINT; wParam : WPARAM; lParam : LPARAM);
TYPE
    DARec = RECORD
        d : ScrollDirection;
        c : ScrollClass;
    END;
CONST
    vArr  : ARRAY[SB_LINEUP..SB_BOTTOM] OF DARec = {
    (* SB_LINEUP            = 0; *)       { DIR_UP, SCROLL_LINE },
    (* SB_LINEDOWN          = 1; *)       { DIR_DOWN, SCROLL_LINE },
    (* SB_PAGEUP            = 2; *)       { DIR_UP, SCROLL_PAGE },
    (* SB_PAGEDOWN          = 3; *)       { DIR_DOWN, SCROLL_PAGE },
    (* SB_THUMBPOSITION     = 4; *)       { DIR_UP, SCROLL_ABSOLUTE },
    (* SB_THUMBTRACK        = 5; *)       { DIR_UP, SCROLL_ABSOLUTE },
    (* SB_TOP               = 6; *)       { DIR_UP, SCROLL_EXTREME },
    (* SB_BOTTOM            = 7; *)       { DIR_DOWN, SCROLL_EXTREME}
    (* SB_ENDSCROLL     ??  = 8; *)                };

    hArr  : ARRAY[SB_LINELEFT..SB_RIGHT] OF DARec = {
    (* SB_LINELEFT          = 0; *)       { DIR_LEFT, SCROLL_LINE },
    (* SB_LINERIGHT         = 1; *)       { DIR_RIGHT, SCROLL_LINE },
    (* SB_PAGELEFT          = 2; *)       { DIR_LEFT, SCROLL_PAGE },
    (* SB_PAGERIGHT         = 3; *)       { DIR_RIGHT, SCROLL_PAGE },
    (* SB_THUMBPOSITION     = 4; *)       { DIR_LEFT, SCROLL_ABSOLUTE },
    (* SB_THUMBTRACK        = 5; *)       { DIR_LEFT, SCROLL_ABSOLUTE },
    (* SB_LEFT              = 6; *)       { DIR_LEFT, SCROLL_EXTREME },
    (* SB_RIGHT             = 7; *)       { DIR_RIGHT, SCROLL_EXTREME }
    (* SB_ENDSCROLL     ??  = 8; *)                 };

    bar : ARRAY ScrollDirection OF WinAttr =
        {WA_VSCROLL, WA_VSCROLL, WA_HSCROLL, WA_HSCROLL};

VAR
    <*/PUSH/NOCHECK:U*>
    dir         : ScrollDirection;
    class       : ScrollClass;
    <*/POP*>
    pos         : ScrollRange;
    msg         : MessageRec;
    sbmsg       : ADRINT;
BEGIN
    IF w = NIL THEN
        RETURN;
    END;

    UNREFERENCED_PARAMETER(lParam);
    sbmsg := LOWORD(wParam);

    IF sbmsg >= SB_ENDSCROLL THEN
        RETURN;
    END;

    IF mess = WM_HSCROLL THEN
        IF (sbmsg = SB_THUMBTRACK) AND (NOT (WA_HSCROLLTRACK IN w^.attr)) THEN
            RETURN;
        ELSIF (sbmsg >= SB_LINELEFT) AND (sbmsg <= SB_RIGHT) THEN
            dir := hArr[sbmsg].d;
            class := hArr[sbmsg].c;
        END;
    ELSE
        IF (sbmsg = SB_THUMBTRACK) AND (NOT (WA_VSCROLLTRACK IN w^.attr)) THEN
            RETURN;
        ELSIF (sbmsg >= SB_LINEUP) AND (sbmsg <= SB_BOTTOM) THEN
            dir := vArr[sbmsg].d;
            class := vArr[sbmsg].c;
        END;
    END;

    IF (sbmsg = SB_THUMBPOSITION) OR (sbmsg = SB_THUMBTRACK) THEN
        pos := CAST(INTEGER16, HIWORD(wParam));

        IF (dir = DIR_UP) OR (dir = DIR_DOWN) THEN
            IF w^.vScale <> 1.0 THEN
                (* scale up and transform from a zero based range *)

                pos := VAL(ScrollRange, FLOAT(pos + w^.vScrollMin) * w^.vScale);
            END;
        ELSE
            IF w^.hScale <> 1.0 THEN
                (* scale up and transform from a zero based range *)

                pos := VAL(ScrollRange, FLOAT(pos + w^.hScrollMin) * w^.hScale);
            END;
        END;
    ELSE
        pos := 0;
    END;

    msg.msg := WSM_SCROLL;
    msg.scrollDir := dir;
    msg.scrollClass := class;
    msg.scrollPos := pos;

    IF CallWndProc(w, msg) = DEFAULT_HANDLE THEN
        CASE class OF
        SCROLL_LINE:
            CASE dir OF
            DIR_DOWN:
                pos := w^.vScrollPos + 1;
            |
            DIR_UP:
                pos := w^.vScrollPos - 1;
            |
            DIR_RIGHT:
                pos := w^.hScrollPos + 1;
            |
            DIR_LEFT:
                pos := w^.hScrollPos - 1;
            END;
        |
        SCROLL_PAGE:
            CASE dir OF
            DIR_DOWN:
                pos := w^.vScrollPos + w^.vScrollPage;
            |
            DIR_UP:
                pos := w^.vScrollPos - w^.vScrollPage;
            |
            DIR_RIGHT:
                pos := w^.hScrollPos + w^.hScrollPage;
            |
            DIR_LEFT:
                pos := w^.hScrollPos - w^.hScrollPage;
            END;
        |
        SCROLL_ABSOLUTE:
            (* pos already set *)
        |
        SCROLL_EXTREME:
            CASE dir OF
            DIR_DOWN:
                pos := w^.vScrollMax;
            |
            DIR_UP:
                pos := w^.vScrollMin;
            |
            DIR_RIGHT:
                pos := w^.hScrollMax;
            |
            DIR_LEFT:
                pos := w^.hScrollMin;
            END;
        END;

        SetScrollBarPos(w, bar[dir], pos);
    END;
END DoScroll;

PROCEDURE DoSize(w : Window; wParam : WPARAM; lParam : LPARAM);
VAR
    msg         : MessageRec;
BEGIN
    IF w <> NIL THEN
        IF w^.type = ToplevelWindow THEN
            msg.msg := WSM_STATE;
            msg.oldState := w^.state;
            IF wParam = SIZE_MINIMIZED THEN
                w^.state := WindowStateMinimized;
                msg.newState := WindowStateMinimized;
                CallWndProc(w, msg);
                RETURN;
            ELSIF wParam = SIZE_MAXIMIZED THEN
                w^.state := WindowStateMaximized;
                msg.newState := WindowStateMaximized;
                CallWndProc(w, msg);
            ELSE
                IF w^.state <> WindowStateNormal THEN
                    w^.state := WindowStateNormal;
                    msg.newState := WindowStateNormal;
                    CallWndProc(w, msg);
                END;
            END;
        END;

        msg.msg := WSM_SIZE;
        msg.width := LOWORD(lParam);
        msg.height := HIWORD(lParam);

        IF (msg.width <> w^.width) OR (msg.height <> w^.height) THEN
            w^.width := msg.width;
            w^.height := msg.height;
            CallWndProc(w, msg);
        END;
    END;
END DoSize;

PROCEDURE DoTimer(w : Window; wParam : WPARAM);
VAR
    msg : MessageRec;
BEGIN
    IF w <> NIL THEN
        IF wParam >= TimerIdFudge THEN
            msg.msg := WSM_TIMER;
            msg.timerId := wParam-TimerIdFudge;
            CallWndProc(w, msg);
        END;
    END;
END DoTimer;

PROCEDURE GetWindowDelta(w : Window; VAR OUT size : wsPOINT);
VAR
    wind        : RECT;
    client      : RECT;
BEGIN
    IF GetWindowRect(w^.wnd, wind) AND
       GetClientRect(w^.clientWnd, client)
    THEN
        size.x := (wind.right - wind.left) - (client.right - client.left);
        size.y := (wind.bottom - wind.top) - (client.bottom - client.top);
    ELSE
        size.x := 0;
        size.y := 0;
    END;
END GetWindowDelta;

PROCEDURE DoMinMaxInfo(w : Window; pmmi : PMINMAXINFO);
VAR
    pt  : wsPOINT;
BEGIN
    IF (w <> NIL) AND (w^.type = ToplevelWindow) THEN
        GetWindowDelta(w, pt);
        pmmi^.ptMaxTrackSize.x := Min(w^.maxW + pt.x, pmmi^.ptMaxTrackSize.x);
        pmmi^.ptMaxTrackSize.y := Min(w^.maxH + pt.y, pmmi^.ptMaxTrackSize.y);
        pmmi^.ptMinTrackSize.x := Max(w^.minW + pt.x, pmmi^.ptMinTrackSize.x);
        pmmi^.ptMinTrackSize.y := Max(w^.minH + pt.y, pmmi^.ptMinTrackSize.y);

        IF w^.hGrain > 1 THEN
            pmmi^.ptMaxTrackSize.x := pmmi^.ptMaxTrackSize.x -
                    ((pmmi^.ptMaxTrackSize.x - pt.x) REM w^.hGrain);
            pmmi^.ptMinTrackSize.x := pmmi^.ptMinTrackSize.x -
                    ((pmmi^.ptMinTrackSize.x - pt.x) REM w^.hGrain);
        END;

        IF w^.vGrain > 1 THEN
            pmmi^.ptMaxTrackSize.y := pmmi^.ptMaxTrackSize.y -
                    ((pmmi^.ptMaxTrackSize.y - pt.y) REM w^.vGrain);
            pmmi^.ptMinTrackSize.y := pmmi^.ptMinTrackSize.y -
                    ((pmmi^.ptMinTrackSize.y - pt.y) REM w^.vGrain);
        END;
    END;
END DoMinMaxInfo;

PROCEDURE DoPosChanging(w : Window; pwp : LPWINDOWPOS);
VAR
    pt  :  wsPOINT;
BEGIN
    IF w <> NIL THEN
        IF (w^.hGrain > 1) OR (w^.vGrain > 1) THEN
            IF (NOT IsIconic(w^.wnd)) AND (NOT IsZoomed(w^.wnd)) THEN
                GetWindowDelta(w, pt);

                pwp^.cx := pwp^.cx - ((pwp^.cx - pt.x) REM w^.hGrain);
                pwp^.cy := pwp^.cy - ((pwp^.cy - pt.y) REM w^.vGrain);
            END;
        END;
    END;
END DoPosChanging;

PROCEDURE DoMove(w : Window; lParam : LPARAM);
VAR
    msg         : MessageRec;
    x, y        : COORDINATE;
BEGIN
    IF w <> NIL THEN
        x := CAST(INTEGER16, LOWORD(lParam));
        y := CAST(INTEGER16, HIWORD(lParam));

        IF (x <> w^.x) OR (y <> w^.y) THEN
            w^.x := x;
            w^.y := y;

            msg.msg := WSM_POSITIONCHANGED;
            msg.windowPos.x := x;
            msg.windowPos.y := y;
            CallWndProc(w, msg);
        END;
    END;
END DoMove;

PROCEDURE DecomposeChar(wParam : WPARAM;
                        lParam : LPARAM;
                        VAR OUT ch : CHAR;
                        VAR OUT num : INTEGER;
                        VAR OUT state : KeyStateSet);
BEGIN
    ch := VAL(CHAR, wParam);
    num := LOWORD(lParam);
    state := KeyStateSet{};

    IF GetKeyState(VK_SHIFT) < 0 THEN
        INCL(state, KS_SHIFT);
    END;
    IF GetKeyState(VK_CONTROL) < 0 THEN
        INCL(state, KS_CONTROL);
    END;
    IF GetKeyState(VK_MENU) < 0 THEN
        INCL(state, KS_ALT);
    END;
END DecomposeChar;

PROCEDURE DoChar(w : Window; wParam : WPARAM; lParam : LPARAM) : ResponseType;
VAR
    ch          : CHAR;
    num         : INTEGER;
    state       : KeyStateSet;
    msg         : MessageRec;
BEGIN
    IF w <> NIL THEN
        DecomposeChar(wParam, lParam, ch, num, state);
        msg.msg := WSM_KEY;
        msg.k_state := state;
        msg.k_ch := ch;
        msg.k_count := num;
        msg.k_special := KEY_NOTSPECIAL;

        RETURN CallWndProc(w, msg);
    END;
    RETURN DEFAULT_HANDLE;
END DoChar;

PROCEDURE DoKeyDown(w : Window; wParam : WPARAM; lParam : LPARAM) : ResponseType;
CONST
    funcTrans   : ARRAY [VK_F1..VK_F12] OF SpecialKeys =
                  {
                      KEY_F1,
                      KEY_F2,
                      KEY_F3,
                      KEY_F4,
                      KEY_F5,
                      KEY_F6,
                      KEY_F7,
                      KEY_F8,
                      KEY_F9,
                      KEY_F10,
                      KEY_F11,
                      KEY_F12
                  };

    specTrans   : ARRAY [VK_PRIOR..VK_DOWN] OF SpecialKeys =
                  {
                      KEY_PAGEUP,
                      KEY_PAGEDOWN,
                      KEY_END,
                      KEY_HOME,
                      KEY_LEFTARROW,
                      KEY_UPARROW,
                      KEY_RIGHTARROW,
                      KEY_DOWNARROW
                  };

VAR
    ch          : CHAR;
    ret         : ResponseType;
    num         : INTEGER;
    state       : KeyStateSet;
    msg         : MessageRec;
BEGIN
    IF w <> NIL THEN
        IF ((wParam >= VK_PRIOR) AND (wParam <= VK_DOWN)) OR
           ((wParam >= VK_F1) AND (wParam <= VK_F12)) OR
           (wParam = VK_INSERT) OR
           (wParam = VK_DELETE) OR
           (wParam = VK_TAB)
        THEN
            DecomposeChar(wParam, lParam, ch, num, state);
            msg.msg := WSM_KEY;
            msg.k_state := state;
            msg.k_ch := ch;
            msg.k_count := num;
            msg.k_special := KEY_NOTSPECIAL;

            CASE wParam OF
            VK_PRIOR..VK_DOWN:
                msg.k_special := specTrans[wParam];
            |
            VK_F1..VK_F12:
                msg.k_special := funcTrans[wParam];
            |
            VK_INSERT:
                msg.k_special := KEY_INSERT;
            |
            VK_DELETE:
                msg.k_special := KEY_DELETE;
            |
            VK_TAB:
                msg.k_ch := CHR(9);
                IF state = KeyStateSet{} THEN
                    RETURN DEFAULT_HANDLE;(* handle via WM_CHAR *)
                ELSIF state = KeyStateSet{KS_CONTROL} THEN
                    IF IsTabChild(w) THEN
                        CycleActiveTabChild(w, 1);
                        RETURN USER_HANDLE;
                    END;
                ELSIF state = KeyStateSet{KS_SHIFT, KS_CONTROL} THEN
                    IF IsTabChild(w) THEN
                        CycleActiveTabChild(w, -1);
                        RETURN USER_HANDLE;
                    END;
                END;
            ELSE
            END;

            ret := CallWndProc(w, msg);
            RETURN ret;
        END;
    END;
    RETURN DEFAULT_HANDLE;
END DoKeyDown;

PROCEDURE DoCommand(w : Window; wParam : WPARAM; lParam : LPARAM);
VAR
    control     : HWND;
    notify,
    id          : CARDINAL;
    msg         : MessageRec;
BEGIN
    IF w <> NIL THEN
        id := LOWORD(wParam);
        control := CAST(HWND, lParam);
        notify := HIWORD(wParam);

        IF (control = NIL) OR
           ((w^.toolbar <> NIL) AND (w^.toolbar^.wnd = control))
        THEN
            IF (notify = 0) OR (notify = 1) THEN
                msg.msg := WSM_MENU;
                msg.menuId := id;
                IF w^.menuWindow = NIL THEN
                    CallWndProc(w, msg);
                ELSE
                    CallWndProc(w^.menuWindow, msg);
                END;
            END;
        END;
    END;
END DoCommand;

PROCEDURE DoMenuSelect(w : Window; wParam : WPARAM; lParam : LPARAM) : LRESULT;
VAR
    msg         : MessageRec;
    flags       : CARDINAL;
    menu        : HMENU;
    send        : BOOLEAN;
BEGIN
    IF w <> NIL THEN
        msg.msg := WSM_MENUSELECT;

        msg.menuId := LOWORD(wParam);
        flags := HIWORD(wParam);
        menu := CAST(HMENU, lParam);

        send := TRUE;

        IF (menu <> NIL) AND ((MF_SYSMENU BAND flags) <> 0) THEN
            send := FALSE;
        ELSIF (menu = NIL) AND (flags = 0FFFFh) THEN

            (* the menu disappeared *)

            msg.msg := WSM_MENUEND;
        END;

        IF send THEN
            IF w^.menuWindow = NIL THEN
                CallWndProc(w, msg);
            ELSE
                CallWndProc(w^.menuWindow, msg);
            END;
        END;
    END;
    RETURN 0;
END DoMenuSelect;

PROCEDURE DoCaptureChanged(w : Window);
BEGIN
    IF w <> NIL THEN
        w^.mouseTrap := 0;
    END;
END DoCaptureChanged;

PROCEDURE CaptureMouse(w : Window);
BEGIN
    INC(w^.mouseTrap);
    IF w^.mouseTrap = 1 THEN
        SetCapture(w^.clientWnd);
    END;
END CaptureMouse;

PROCEDURE ReleaseMouse(w : Window);
BEGIN
    IF w^.mouseTrap > 0 THEN
        DEC(w^.mouseTrap);
        IF w^.mouseTrap = 0 THEN
            ReleaseCapture();
        END;
    END;
END ReleaseMouse;

PROCEDURE ReleaseMouseAll(w : Window);
BEGIN
    IF w <> NIL THEN
        IF w^.mouseTrap > 0 THEN
            w^.mouseTrap := 0;
            ReleaseCapture();
        END;
    END;
END ReleaseMouseAll;

PROCEDURE DoMouse(w : Window; mess : UINT; wParam : WPARAM; lParam : LPARAM) : ResponseType;
VAR
    msg         : MessageRec;
    wheel       : INTEGER;
BEGIN
    IF w <> NIL THEN
        msg.msg := WSM_MOUSE;

        msg.m_pos.y := CAST(INTEGER16, HIWORD(lParam));
        msg.m_pos.x := CAST(INTEGER16, LOWORD(lParam));

        msg.m_state := MouseStateSet{};
        IF (MK_LBUTTON BAND wParam) = MK_LBUTTON THEN
            INCL(msg.m_state, MS_LEFTBUTTON);
        END;
        IF (MK_RBUTTON BAND wParam) = MK_RBUTTON THEN
            INCL(msg.m_state, MS_RIGHTBUTTON);
        END;
        IF (MK_MBUTTON BAND wParam) = MK_MBUTTON THEN
            INCL(msg.m_state, MS_MIDDLEBUTTON);
        END;
        IF (MK_CONTROL BAND wParam) = MK_CONTROL THEN
            INCL(msg.m_state, MS_CONTROL);
        END;
        IF (MK_SHIFT BAND wParam) = MK_SHIFT THEN
            INCL(msg.m_state, MS_SHIFT);
        END;

        CASE mess OF
        WM_LBUTTONDOWN:
            CaptureMouse(w);
            msg.m_button := LeftButton;
            msg.m_event := ButtonDown;
        |
        WM_LBUTTONUP:
            IF w^.mouseTrap > 0 THEN
                ReleaseMouse(w);
                msg.m_button := LeftButton;
                msg.m_event := ButtonUp;
            ELSE
                RETURN DEFAULT_HANDLE;
            END;
        |
        WM_RBUTTONDOWN:
            CaptureMouse(w);
            msg.m_button := RightButton;
            msg.m_event := ButtonDown;
        |
        WM_RBUTTONUP:
            IF w^.mouseTrap > 0 THEN
                ReleaseMouse(w);
                msg.m_button := RightButton;
                msg.m_event := ButtonUp;
            ELSE
                RETURN DEFAULT_HANDLE;
            END;
        |
        WM_MBUTTONDOWN:
            CaptureMouse(w);
            msg.m_button := MiddleButton;
            msg.m_event := ButtonDown;
        |
        WM_MBUTTONUP:
            IF w^.mouseTrap > 0 THEN
                ReleaseMouse(w);
                msg.m_button := MiddleButton;
                msg.m_event := ButtonUp;
            ELSE
                RETURN DEFAULT_HANDLE;
            END;
        |
        WM_LBUTTONDBLCLK:
            CaptureMouse(w);

            msg.m_button := LeftButton;
            msg.m_event := ButtonDown;
            CallWndProc(w, msg);

            msg.m_button := LeftButton;
            msg.m_event := ButtonDouble;
        |
        WM_MBUTTONDBLCLK:
            CaptureMouse(w);

            msg.m_button := MiddleButton;
            msg.m_event := ButtonDown;
            CallWndProc(w, msg);

            msg.m_button := MiddleButton;
            msg.m_event := ButtonDouble;
        |
        WM_RBUTTONDBLCLK:
            CaptureMouse(w);

            msg.m_button := RightButton;
            msg.m_event := ButtonDown;
            CallWndProc(w, msg);

            msg.m_button := RightButton;
            msg.m_event := ButtonDouble;
        |
        WM_MOUSEMOVE:
            msg.m_button := NoButton;
            msg.m_event := MouseMove;

            IF w^.mouseTrap = 0 THEN
                msg.m_state := msg.m_state - MouseStateSet{MS_LEFTBUTTON, MS_RIGHTBUTTON, MS_MIDDLEBUTTON};
            END;
        |
        WM_MOUSEWHEEL:
            msg.m_button := NoButton;
            msg.m_event := MouseWheel;
            wheel := CAST(INTEGER16, HIWORD(wParam));
            w^.mouseWheelTotal := w^.mouseWheelTotal + wheel;
            wheel := w^.mouseWheelTotal / WHEEL_DELTA;
            w^.mouseWheelTotal := w^.mouseWheelTotal REM WHEEL_DELTA;
            msg.m_wheel_count := ABS(wheel);
            IF wheel < 0 THEN
                msg.m_wheel_dir := DIR_DOWN;
            ELSIF wheel > 0 THEN
                msg.m_wheel_dir := DIR_UP;
            ELSE
                RETURN DEFAULT_HANDLE;
            END;
        ELSE
            RETURN DEFAULT_HANDLE;
        END;

        RETURN CallWndProc(w, msg);
    END;
    RETURN DEFAULT_HANDLE
END DoMouse;

PROCEDURE DoContextMenu(w : Window; lParam : LPARAM);
VAR
    msg         : MessageRec;
    pos         : POINT;
BEGIN
    pos.x := CAST(INTEGER16, LOWORD(lParam));
    pos.y := CAST(INTEGER16, HIWORD(lParam));
    IF (pos.x < 0) AND (pos.y < 0) THEN
        (* only handle the keyboard version of message *)

        msg.msg := WSM_KEY;
        msg.k_state := KeyStateSet{};
        msg.k_ch := '';
        msg.k_count := 1;
        msg.k_special := KEY_MENU;

        CallWndProc(w, msg);
    END;
END DoContextMenu;

PROCEDURE DoHelp(w : Window; lParam : LPARAM);
VAR
    (*msg         : MessageRec;*)
    hlp         : LPHELPINFO;
BEGIN
    IF w <> NIL THEN
        hlp := CAST(LPHELPINFO, lParam);
        (*
        msg.msg := WSM_HELP;
        CASE hlp^.iContextType OF
        HELPINFO_MENUITEM:
            msg.helpContext := HelpMenu;
        |
        HELPINFO_WINDOW:
            msg.helpContext := HelpWindow;
        ELSE
            RETURN;
        END;

        msg.helpContextId := hlp^.iCtrlId;
        msg.helpHelpId := hlp^.dwContextId;

        CallWndProc(w, msg);
        *)
    END;
END DoHelp;

PROCEDURE DoUser(w : Window; wParam : WPARAM; lParam : LPARAM);
VAR
    msg         : MessageRec;
BEGIN
    IF w <> NIL THEN
        msg.msg := WSM_USER;
        msg.userId := wParam;
        msg.userData := CAST(ADDRESS, lParam);
        CallWndProc(w, msg);
    END;
END DoUser;

PROCEDURE DoExcept(w : Window; wnd : HWND; procName : ARRAY OF CHAR);
CONST
    crlf        : ARRAY OF CHAR = {CHR(13),CHR(10)};
    hexDig      : ARRAY OF CHAR = {"0123456789ABCDEF"};
VAR
    str         : ARRAY [0..255] OF CHAR;
    mess        : ARRAY [0..63] OF CHAR;
    hexStr      : ARRAY [0..7] OF CHAR;
    num         : CARDINAL;
    i           : ADRCARD;
BEGIN
    OutputCallTrace;

    IF w <> NIL THEN
        w^.suppressWndProc := FALSE;
    END;

    EXCEPTIONS.GetMessage(mess);

    num := CAST(CARDINAL, EXCEPTADR());
    FOR i := 7 TO 0 BY -1 DO
        hexStr[i] := hexDig[num REM 16];
        num := num / 16;
    END;

    Concat("Exception Trapped in WinShell.", procName, str);
    Append(" at ", str);
    Append(hexStr, str);
    Append(crlf, str);
    Append(mess, str);

    MessageBeep(MB_ICONSTOP);
    MessageBox(wnd,
               str,
               "Severe Program Error",
               MB_OK BOR
               MB_ICONSTOP BOR
               MB_SETFOREGROUND BOR
               MB_TOPMOST BOR
               MB_TASKMODAL);
END DoExcept;

PROCEDURE DoCloseRequest(w : Window) : BOOLEAN;
VAR
    msg         : MessageRec;
BEGIN
    IF w <> NIL THEN
        msg.msg := WSM_CLOSE;
        msg.closeMode := CM_REQUEST;
        CASE CallWndProc(w, msg) OF
        NotOkayToClose, USER_HANDLE:
            RETURN FALSE;
        ELSE
        END;
    END;
    RETURN TRUE;
END DoCloseRequest;

PROCEDURE GetButtonInfo(w : Window; tbn : LPTBNOTIFY) : BOOLEAN;
VAR
    count       : CARDINAL;
    j           : ADRCARD;
    i           : ADRCARD;
    add         : BOOLEAN = TRUE;
BEGIN
    IF (w^.toolbar <> NIL) AND (w^.toolbar^.buttons <> NIL) THEN
        count := 0;

        FOR j := 0 TO 1 DO
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, w^.toolbar^.numButtons)-1 DO
            <*/POP*>
                IF w^.toolbar^.buttons^[i].visible <> add THEN
                    IF count = ORD(tbn^.iItem) THEN
                        tbn^.tbButton.iBitmap := w^.toolbar^.buttons^[i].bmpId;
                        tbn^.tbButton.idCommand := w^.toolbar^.buttons^[i].actionId;
                        tbn^.tbButton.fsState := TBSTATE_ENABLED;
                        CASE w^.toolbar^.buttons^[i].type OF
                        TbPushButton:
                            tbn^.tbButton.fsStyle := TBSTYLE_BUTTON;
                        |
                        TbToggleButton:
                            tbn^.tbButton.fsStyle := TBSTYLE_CHECK;
                        END;
                        tbn^.tbButton.dwData := 0;
                        tbn^.tbButton.iString := i;

                        LoadString(w^.toolbar^.buttons^[i].textId, tbn^.pszText^[0..tbn^.cchText-1]);
                        RETURN TRUE;
                    END;
                    count := count + 1;
                END;
            END;

            add := FALSE;
        END;

        RETURN FALSE;
    END;

    RETURN FALSE;
END GetButtonInfo;

PROCEDURE UpdateButtonInfo(w : Window);
VAR
    i   : ADRCARD;
BEGIN
    IF (w^.toolbar <> NIL) AND (w^.toolbar^.buttons <> NIL) THEN
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, w^.toolbar^.numButtons)-1 DO
        <*/POP*>
            w^.toolbar^.buttons^[i].visible := IsToolbarButtonShown(w, i);
        END;
    END;
END UpdateButtonInfo;

PROCEDURE SetMdiMenu(w : Window);
VAR
    frame       : Window;
BEGIN
    frame := GetToplevel(w);

    IF frame <> NIL THEN
        frame^.menuWindow := NIL;
        IF w^.menu <> NIL THEN
            frame^.activeMenu := w^.menu;
            IF w <> frame THEN
                frame^.menuWindow := w;
            END;
        ELSE
            frame^.activeMenu := frame^.menu;
        END;
        SetMenu(frame^.wnd, frame^.activeMenu);
        DrawMenuBar(frame^.wnd);
    END;
END SetMdiMenu;

PROCEDURE GetTabChildTitle(w : Window; VAR OUT title : ARRAY OF CHAR);
VAR
    numStr      : ARRAY [0..7] OF CHAR;
BEGIN
    title := w^.title;

    IF (w^.parent^.tabChildNumbers) AND (w^.tabIndex <= 9) THEN
        numStr := "  ";
        numStr[0] := CHR(w^.tabIndex + INT('0'));
        Concat(numStr, title, title);
    END;
END GetTabChildTitle;

PROCEDURE ReindexTabChildren(frame : Window; forcePaint : BOOLEAN);
VAR
    index       : CARDINAL;
    w           : Window;
    item        : TCITEM;
    title       : ARRAY [0..127] OF CHAR;
BEGIN
    IF frame^.clientType = TabClient THEN
        index := 0;
        w := frame^.firstChild;
        WHILE w <> NIL DO
            IF IsTabChild(w) THEN
                w^.tabIndex := -1;
                IF WA_VISIBLE IN w^.attr THEN
                    w^.tabIndex := index;
                    INC(index);

                    IF forcePaint OR frame^.tabChildNumbers THEN
                        GetTabChildTitle(w, title);
                        item.mask := TCIF_TEXT;
                        item.pszText := ADR(title);
                        TabCtrl_SetItem(frame^.clientWnd, w^.tabIndex, item);
                    END;
                END;
            END;

            w := w^.nextSibling;
        END;
    END;
END ReindexTabChildren;

PROCEDURE RaiseChild(w : Window; activate : BOOLEAN);
VAR
    child       : Window;
    flags       : CARDINAL;
BEGIN
    IF IsWindow(w) THEN
        flags := SWP_NOMOVE BOR SWP_NOSIZE BOR SWP_SHOWWINDOW;
        IF NOT activate THEN
            flags := flags BOR SWP_NOACTIVATE;
        END;
        WINUSER.SetWindowPos(w^.wnd, HWND_TOP, 0, 0, 0, 0, flags);

        IF w^.clientType = SplitterClient THEN
            CCsplitter_SetActive(w^.clientWnd, TRUE);

            child := w^.firstChild;
            WHILE child <> NIL DO
                RaiseChild(child, FALSE);
                child := child^.nextSibling;
            END;

        ELSIF w^.clientType = TabClient THEN
            IF w^.active <> NIL THEN
                RaiseChild(w^.active, FALSE);
            END;
        END;
    END;
END RaiseChild;

PROCEDURE LowerChild(w : Window);
VAR
    child       : Window;
BEGIN
    IF IsWindow(w) THEN
        (* we do not actually lower anything but the splitter controls need to know they are not active
           so that on resizing they do not change the z-order of their children.
        *)
        IF w^.clientType = SplitterClient THEN
            CCsplitter_SetActive(w^.clientWnd, FALSE);

            child := w^.firstChild;
            WHILE child <> NIL DO
                LowerChild(child);
                child := child^.nextSibling;
            END;

        ELSIF w^.clientType = TabClient THEN
            IF w^.active <> NIL THEN
                LowerChild(w^.active);
            END;
        END;
    END;
END LowerChild;

PROCEDURE DoSetActiveTabChild(w : Window; focus : BOOLEAN);
VAR
    msg         : MessageRec;
    parent      : Window;
BEGIN
    parent := w^.parent;

    LowerChild(parent^.active);

    (* remove our previous list position *)

    UnlinkActive(w);

    (* put ourself at the head of the active list *)

    w^.nextActive := parent^.active;
    parent^.active := w;

    IF parent^.type = ToplevelWindow THEN
        SetMdiMenu(w);
    END;

    TabCtrl_SetCurSel(parent^.clientWnd, w^.tabIndex);

    RaiseChild(w, TRUE);

    msg.msg := WSM_TAB_ACTIVE;
    CallWndProc(w, msg);

    IF focus THEN
        SetFocus(w^.wnd);
    END;
END DoSetActiveTabChild;

PROCEDURE IsCurrentTabChild(w : Window) : BOOLEAN;
BEGIN
    RETURN w^.parent^.active = w;
END IsCurrentTabChild;

PROCEDURE ShowTabChild(w : Window);
VAR
    tab         : TCITEM;
    title       : ARRAY [0..127] OF CHAR;
BEGIN
    IF IsTabChild(w) AND (w^.tabIndex < 0) THEN
        w^.tabIndex := TabCtrl_GetItemCount(w^.parent^.clientWnd);

        GetTabChildTitle(w, title);
        tab.mask := TCIF_TEXT;
        tab.pszText := ADR(title);
        TabCtrl_InsertItem(w^.parent^.clientWnd, w^.tabIndex, tab);

        (* need this in case the number of tab lines change.
           then all the children need to be resized.
        *)
        AdjustClientSize(w^.parent);

        INCL(w^.attr, WA_VISIBLE);
        ShowWindow(w^.wnd, SW_SHOWNA);

        IF w^.parent^.active = NIL THEN
            DoSetActiveTabChild(w, TRUE);

            (* handles a Windows painting quirk/bug when a tab control goes
               from nothing and gets its first tab.
               the quirk is nothing serious, just looks different from
               when a second tab is added.
            *)
            InvalidateRect(w^.parent^.clientWnd, NIL_RECT, TRUE);
        END;
    END;
END ShowTabChild;

PROCEDURE HideTabChild(w : Window);
VAR
    parent      : Window;
    isCurrent   : BOOLEAN;
    index       : INTEGER;
    focus       : BOOLEAN;
BEGIN
    IF IsTabChild(w) AND (w^.tabIndex >= 0) THEN
        parent := w^.parent;
        focus := w^.hasFocus;

        isCurrent := IsCurrentTabChild(w);

        index := w^.tabIndex;
        TabCtrl_DeleteItem(parent^.clientWnd, w^.tabIndex);
        IF w^.nextSibling = NIL THEN
            DEC(index);
        END;

        EXCL(w^.attr, WA_VISIBLE);
        ReindexTabChildren(parent, FALSE);

        UnlinkActive(w);

        IF parent^.active <> NIL THEN
            IF isCurrent THEN
                DoSetActiveTabChild(parent^.active, focus);
            END;
        ELSE
            IF parent^.type = ToplevelWindow THEN
                SetMdiMenu(parent);
            END;
        END;

        (* doing this after we activate the new child might eliminate
           some screen flicker.
        *)
        ShowWindow(w^.wnd, SW_HIDE);
    END;
END HideTabChild;

PROCEDURE AdjustContainerChildren(w : Window; rc : RECT);
VAR
    rc2         : RECT;
    child       : Window;
    curSel      : INTEGER;
    top         : Window;
BEGIN
    IF w^.clientType = TabClient THEN
        rc2 := rc;
        TabCtrl_AdjustRect(w^.clientWnd, FALSE, rc2);

        curSel := TabCtrl_GetCurSel(w^.clientWnd);
        child := w^.firstChild;
        WHILE child <> NIL DO
            MoveWindow(child^.wnd, rc2.left, rc2.top, rc2.right - rc2.left, rc2.bottom - rc2.top, child^.tabIndex = curSel);
            AdjustContainerChildren(child, rc2);

            child := child^.nextSibling;
        END;
    ELSIF w^.clientType = SplitterClient THEN
        CCsplitter_PositionViews(w^.clientWnd);

        top := GetToplevel(w);
        child := w^.firstChild;
        WHILE child <> NIL DO
            GetClientRect(child^.wnd, rc2);
            MapWindowPoints(child^.wnd, top^.wnd, CAST(POINT, rc2), 2);
            AdjustContainerChildren(child, rc2);

            child := child^.nextSibling;
        END;
    END;
END AdjustContainerChildren;

PROCEDURE AdjustClientSize(w : Window);
VAR
    rc, rc2     : RECT;
    top         : Window;
BEGIN
    IF (w <> NIL) AND (w^.clientWnd <> NIL) AND NOT IsIconic(w^.wnd) THEN
        GetClientRect(w^.wnd, rc);
        IF w^.type = ToplevelWindow THEN
            IF w^.statusbar <> NIL THEN
                WINUSER.SendMessage(w^.statusbar^.wnd, WM_SIZE, 0, 0);
                GetWindowRect(w^.statusbar^.wnd, rc2);
                rc.bottom := rc.bottom - (rc2.bottom - rc2.top + 1);
            END;

            IF w^.toolbar <> NIL THEN
                WINUSER.SendMessage(w^.toolbar^.wnd, TB_AUTOSIZE, 0, 0);
                GetWindowRect(w^.toolbar^.wnd, rc2);
                rc.top := rc.top + (rc2.bottom -rc2.top + 1);
            END;

            MoveWindow(w^.clientWnd, rc.left, rc.top, rc.right-rc.left, rc.bottom-rc.top, TRUE);
        ELSE
            top := GetToplevel(w);
            MapWindowPoints(w^.wnd, top^.wnd, CAST(POINT, rc), 2);
        END;

        AdjustContainerChildren(w, rc);
    END;
END AdjustClientSize;

PROCEDURE DoToolbarTooltip(w : Window; nmhdr : LPNMHDR) : LRESULT;
VAR
    pttt        : LPNMTTDISPINFO;
    actionId    : CARDINAL;
    i           : ADRCARD;
BEGIN
    pttt := CAST(LPNMTTDISPINFO, nmhdr);
    pttt^.hinst := NIL;

    actionId := pttt^.hdr.idFrom;
    IF (w^.toolbar <> NIL) AND
       (w^.toolbar^.buttons <> NIL) AND
       w^.toolbar^.hasHelp
    THEN
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, w^.toolbar^.numButtons)-1 DO
        <*/POP*>
            IF w^.toolbar^.buttons^[i].actionId = actionId THEN
                pttt^.lpszText := CAST(WIN32.LPTSTR, MAKEADR(w^.toolbar^.buttons^[i].helpId));
                pttt^.hinst := ResourceInst;
                RETURN 1;
            END;
        END;
        RETURN 1;
    END;
    RETURN 0;
END DoToolbarTooltip;

PROCEDURE DoFormControlTooltip(w : Window; nPtr : LPNMTTDISPINFO);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, nPtr^.lParam);
    IF ctrl <> NIL THEN
        nPtr^.lpszText := ADR(ctrl^.tipText);
        nPtr^.hinst := NIL;
    ELSE
        nPtr^.lpszText := ADR("InternalError: Control tooltip not found");
    END;
END DoFormControlTooltip;

PROCEDURE SetTreeNodeFolderImage(w : Window; node : HTREEITEM; expand : BOOLEAN);
VAR
    tvItem      : TVITEM;
BEGIN
    IF w^.imageList <> NIL THEN
        tvItem.mask := TVIF_IMAGE BOR TVIF_SELECTEDIMAGE;
        tvItem.hItem := node;
        IF expand THEN
            tvItem.iImage := 2;
            tvItem.iSelectedImage := 2;
        ELSE
            tvItem.iImage := 1;
            tvItem.iSelectedImage := 1;
        END;
        TreeView_SetItem(w^.clientWnd, tvItem);
    END;
END SetTreeNodeFolderImage;

PROCEDURE DoNotify(w : Window; nPtr : LPNMHDR) : LRESULT;
VAR
    index       : CARDINAL;
    changes     : CARDINAL;
    msg         : MessageRec;
    parent,
    from,
    child       : Window;
    column      : ADRCARD;
    ctrl        : ControlInfoPointer;
    lvn         : LPNMLISTVIEW;
    tvn         : LPNMTREEVIEW;
    hdn         : LPNMHEADER;

    PROCEDURE sendCtrlNotify(nm : NotifyMessage);
    BEGIN
        msg.notify := nm;
        CallWndProc(parent, msg);
    END sendCtrlNotify;

BEGIN
    parent := FindWindow(WINUSER.GetParent(nPtr^.hwndFrom));
    from := FindWindow(nPtr^.hwndFrom);

    CASE nPtr^.code OF
    TTN_GETDISPINFO:
        (* tooltip only sends to a toplevel *)

        w := FindTooltipWindow(nPtr^.hwndFrom);
        IF w <> NIL THEN
            IF w^.clientType = FormClient THEN
                DoFormControlTooltip(w, CAST(LPNMTTDISPINFO, nPtr));
                RETURN 1;
            ELSIF w^.toolbar <> NIL THEN
                RETURN DoToolbarTooltip(w, nPtr);
            END;
        END;
    |
    TBN_QUERYDELETE:
        RETURN 1;
    |
    TBN_GETBUTTONINFO:
        RETURN ORD(GetButtonInfo(w, CAST(LPTBNOTIFY, nPtr)));
    |
    TBN_QUERYINSERT:
        RETURN 1;
    |
    TBN_BEGINADJUST:
        UpdateButtonInfo(w);
        RETURN 1;
    |
    TBN_TOOLBARCHANGE:
        AdjustClientSize(w);
        RETURN 0;
    |
    TCN_SELCHANGE:
        (* tab control only sends to toplevel window *)

        IF (parent <> NIL) AND (parent^.clientType = FormClient) THEN
            ctrl := FindControl(parent, nPtr^.idFrom);
            IF ctrl <> NIL THEN
                index := TabCtrl_GetCurSel(ctrl^.wnd);
                IF index >= 0 THEN
                    ctrl^.currentPage := index;
                    MultiPageSetActivePage(ctrl, index);

                    msg.msg := WSM_NOTIFY;
                    msg.ctrlId := ctrl^.idNum;
                    msg.notify := NM_SelectionChanged;
                    CallWndProc(ctrl^.w, msg);
                END;
            END;
        ELSE
            w := FindWindow(nPtr^.hwndFrom);
            IF w <> NIL THEN
                index := TabCtrl_GetCurSel(w^.clientWnd);
                IF index >= 0 THEN
                    child := GetIthTabChild(w, index);
                    IF child <> NIL THEN
                        DoSetActiveTabChild(child, TRUE);
                    END;
                END;
            END;
        END;
        RETURN 0;
    |
    CCSPN_SPLITCHANGED:
        IF from <> NIL THEN
            msg.msg := WSM_SPLIT_CHANGED;
            msg.splitPosPixels := GetSplitPosition(from, FALSE);
            msg.splitPosPercent := GetSplitPosition(from, TRUE);
            CallWndProc(from, msg);
        END;
        RETURN 0;
    |
    CCSPN_POSCHANGED:
        IF from <> NIL THEN
            (* the splitter raises its "child" windows.
               this puts a tab client above its current active window.
               go through and raise tab client active windows.
            *)
            child := from^.firstChild;
            WHILE child <> NIL DO
                IF (child^.clientType = TabClient) AND (child^.active <> NIL) THEN
                    RaiseChild(child^.active, FALSE);
                END;
                child := child^.nextSibling;
            END;
        END;
        RETURN 0;
    |
    HDN_ENDTRACK:
        IF (parent <> NIL) AND (parent^.clientType = ListClient) THEN
            hdn := CAST(LPNMHEADER, nPtr);
            msg.msg := WSM_LIST_COLUMN_WIDTH_CHANGED;
            msg.listColumn := hdn^.iItem;
            msg.listColumnWidth := hdn^.pitem^.cxy;
            CallWndProc(parent, msg);
        END;
    |
    HDN_BEGINTRACK:
        IF (parent <> NIL) AND (parent^.clientType = ListClient) THEN
            hdn := CAST(LPNMHEADER, nPtr);
            column := hdn^.iItem;
            IF (parent^.columnInfo <> NIL) AND
               (parent^.columnInfo^[column].width < 0)
            THEN
                RETURN 1;(*do not allow resizing*)
            END;
        END;
    ELSE
        IF (parent <> NIL) AND (parent^.clientType = FormClient) THEN
            ctrl := FindControl(parent, nPtr^.idFrom);
            IF ctrl <> NIL THEN
                msg.msg := WSM_NOTIFY;
                msg.ctrlId := ctrl^.idNum;

                CASE nPtr^.code OF
                NM_CLICK:
                    sendCtrlNotify(NM_Clicked);
                    RETURN 1;
                |
                NM_DBLCLK:
                    sendCtrlNotify(NM_DoubleClicked);
                    RETURN 1;
                |
                NM_KILLFOCUS:
                    sendCtrlNotify(NM_LoseFocus);
                    RETURN 1;
                |
                NM_SETFOCUS:
                    sendCtrlNotify(NM_GainFocus);
                    RETURN 1;
                |
                LVN_ITEMCHANGED:
                    lvn := CAST(LPNMLISTVIEW, nPtr);
                    changes := lvn^.uNewState BXOR lvn^.uOldState;
                    IF (changes BAND LVIS_SELECTED) <> 0 THEN
                        sendCtrlNotify(NM_SelectionChanged);
                    END;
                    RETURN 1;
                ELSE
                END;
            END;
        ELSE
            IF from <> NIL THEN
                CASE nPtr^.code OF
                NM_DBLCLK:
                    msg.msg := WSM_SELECTION_DOUBLECLICK;
                    CallWndProc(from, msg);
                    RETURN 0;
                |
                NM_KILLFOCUS:
                    msg.msg := WSM_LOSEFOCUS;
                    CallWndProc(from, msg);
                    RETURN 1;
                |
                NM_SETFOCUS:
                    msg.msg := WSM_GAINFOCUS;
                    CallWndProc(from, msg);
                    RETURN 1;
                |
                NM_RETURN:
                    msg.msg := WSM_SELECTION_ENTERKEY;
                    CallWndProc(from, msg);
                    RETURN 1;
                |
                LVN_ITEMCHANGED:
                    lvn := CAST(LPNMLISTVIEW, nPtr);
                    changes := lvn^.uNewState BAND (BNOT lvn^.uOldState);
                    IF (changes BAND LVIS_SELECTED) <> 0 THEN
                        msg.msg := WSM_SELECTION_CHANGED;
                        CallWndProc(from, msg);
                    END;
                    RETURN 1;
                |
                LVN_COLUMNCLICK:
                    lvn := CAST(LPNMLISTVIEW, nPtr);
                    column := lvn^.iSubItem;
                    IF (from^.columnInfo <> NIL) AND
                       (from^.columnInfo^[column].sortable)
                    THEN
                        IF INT(column) <> from^.sortColumn THEN
                            from^.sortDirection := SortAscending;
                        ELSE
                            IF from^.sortDirection = SortAscending THEN
                                from^.sortDirection := SortDescending;
                            ELSE
                                from^.sortDirection := SortAscending;
                            END;
                        END;
                        ListClientSetSortColumn(from, lvn^.iSubItem, from^.sortDirection);
                    END;
                |
                TVN_SELCHANGED:
                    tvn := CAST(LPNMTREEVIEW, nPtr);
                    changes := tvn^.itemNew.state BAND (BNOT tvn^.itemOld.state);
                    IF (changes BAND TVIS_SELECTED) <> 0 THEN
                        msg.msg := WSM_SELECTION_CHANGED;
                        CallWndProc(from, msg);
                    END;
                    RETURN 1;
                |
                TVN_ITEMEXPANDED:
                    tvn := CAST(LPNMTREEVIEW, nPtr);
                    IF tvn^.action = TVE_EXPAND THEN
                        SetTreeNodeFolderImage(from, tvn^.itemNew.hItem, TRUE);

                        msg.msg := WSM_TREE_EXPAND;
                        msg.treeNode := tvn^.itemNew.hItem;
                        CallWndProc(from, msg);
                    ELSIF tvn^.action = TVE_COLLAPSE THEN
                        SetTreeNodeFolderImage(from, tvn^.itemNew.hItem, FALSE);

                        msg.msg := WSM_TREE_COLLAPSE;
                        msg.treeNode := tvn^.itemNew.hItem;
                        CallWndProc(from, msg);
                    END;
                    RETURN 1;
                ELSE
                END;
            END;
        END;
    END;
    RETURN 0;
END DoNotify;

PROCEDURE DoColumnAutoSize(info : ColumnAutoSizePointer);
VAR
    attr        : INTEGER;
    i           : ADRCARD;
BEGIN
    <*/PUSH/NOWARN:U*>
    FOR i := 0 TO VAL(ADRCARD, info^.numColumns)-1 DO
    <*/POP*>
        IF (info^.info = NIL) OR (info^.info^[i].width < 0) THEN
            attr := LVSCW_AUTOSIZE;
            IF (info^.info <> NIL) AND (info^.info^[i].header <> NIL) THEN
                attr := LVSCW_AUTOSIZE_USEHEADER;
            END;
            ListView_SetColumnWidth(info^.wnd, i, attr);
        END;
    END;
    IF info^.posted <> NIL THEN
        info^.posted^ := FALSE;
    END;
    DISPOSE(info);
END DoColumnAutoSize;

PROCEDURE DrawClientWndProc(hwnd : HWND; mess : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT [EXPORT, SbOsSystem];
VAR
    w           : Window;
    msg         : MessageRec;
BEGIN
    w := CAST(Window, GetWindowLongPtr(hwnd, WindowDataPos[DrawClient]));

    CASE mess OF
    WM_CREATE:
        w := CAST(LPCREATESTRUCT, lParam)^.lpCreateParams;
        SetWindowLongPtr(hwnd, WindowDataPos[DrawClient], CAST(LONG_PTR, w));

        w^.clientWnd := hwnd;
        IF w^.type = ChildWindow THEN
            w^.wnd := hwnd;
        END;

        RETURN 0;
    |
    WM_CLOSE:
        IF DoCloseRequest(w) THEN
            DestroyWindow(w^.wnd);
            RETURN 1;
        END;
        RETURN 0;
    |
    WM_DESTROY:
        IF IsWindow(w) THEN(*be paranoid safe*)
            msg.msg := WSM_CLOSE;
            msg.closeMode := CM_DICTATE;
            CallWndProc(w, msg);

            SetWindowLongPtr(hwnd, WindowDataPos[DrawClient], CAST(LONG_PTR, NIL));

            IF IsTabChild(w) THEN
                HideTabChild(w);
            ELSIF IsSplitterChild(w) THEN
                SetSplitterHandle(w, FALSE);
            END;
            IF w^.type = ChildWindow THEN
                DisposeWindow(w);
            END;
        END;
        RETURN 0;
    |
    WM_SETFOCUS:
        DoSetFocus(w, wParam);
        RETURN 0;
    |
    WM_KILLFOCUS:
        DoKillFocus(w);
        RETURN 0;
    |
    WM_ACTIVATE:
        DoActivate(w, LOWORD(wParam) <> WA_INACTIVE);
    |
    WM_MOUSEACTIVATE:
        SetFocus(hwnd);
        RETURN MA_ACTIVATE;
    |
    WM_CHAR:
        IF DoChar(w, wParam, lParam) = USER_HANDLE THEN
            RETURN 0;
        END;
    |
    WM_KEYDOWN:
        IF DoKeyDown(w, wParam, lParam) = USER_HANDLE THEN
            RETURN 0;
        END;
    |
    WM_COMMAND:
        DoCommand(w, wParam, lParam);
        RETURN 0;
    |
    WM_CONTEXTMENU:
        DoContextMenu(w, lParam);
        RETURN 0;
    |
    WM_INITMENU:
        IF w <> NIL THEN
            msg.msg := WSM_MENUSTART;
            IF w^.menuWindow = NIL THEN
                CallWndProc(w, msg);
            ELSE
                CallWndProc(w^.menuWindow, msg);
            END;
        END;
    |
    WM_MENUSELECT:
        RETURN DoMenuSelect(w, wParam, lParam);
    |
    WM_PAINT:
        DoPaint(w, hwnd);
        RETURN 0;
    |
    WM_ERASEBKGND:
        DoEraseBackground(w, wParam);
        RETURN 1;
    |
    WM_VSCROLL, WM_HSCROLL:
        DoScroll(w, mess, wParam, lParam);
        RETURN 0;
    |
    WM_SIZE:
        DoSize(w, wParam, lParam);
        RETURN 0;
    |
    WM_LBUTTONDOWN, WM_LBUTTONUP, WM_RBUTTONDOWN,
    WM_RBUTTONUP, WM_MBUTTONDOWN, WM_MBUTTONUP,
    WM_LBUTTONDBLCLK, WM_MBUTTONDBLCLK, WM_RBUTTONDBLCLK,
    WM_MOUSEMOVE, WM_MOUSEWHEEL:
        IF DoMouse(w, mess, wParam, lParam) = USER_HANDLE THEN
            RETURN 0;
        END;
    |
    WM_CAPTURECHANGED:
        DoCaptureChanged(w);
    |
    WM_TIMER:
        DoTimer(w, wParam);
        RETURN 0;
    |
    WM_SETCURSOR:
        IF w <> NIL THEN
            IF LOWORD(lParam) = HTCLIENT THEN
                SetCursor(w^.cursor);
                RETURN 1;
            END;
        END;
    |
    WM_HELP:
        DoHelp(w, lParam);
        RETURN 1;
    |
    WM_USER_USER:
        IF w <> NIL THEN
            DoUser(w, wParam, lParam);
        END;
        RETURN 0;
    ELSE
    END;

    RETURN DefWindowProc(hwnd, mess, wParam, lParam);

EXCEPT
    ReleaseMouseAll(w);
    DoExcept(w, hwnd, "DrawClientWndProc");
    RETURN DefWindowProc(hwnd, mess, wParam, lParam);
END DrawClientWndProc;

PROCEDURE DoFormCommand(w : Window; wParam : WPARAM; lParam : LPARAM);
VAR
    ctrlWnd     : HWND;
    ctrlMsg     : DWORD;
    ctrl        : ControlInfoPointer;
    msg         : MessageRec;

    PROCEDURE send(nm : NotifyMessage);
    BEGIN
        msg.notify := nm;
        CallWndProc(w, msg);
    END send;

    PROCEDURE fillEditBox;
    VAR
        len, i : INTEGER;
        TempTextPtr : StringData;
    BEGIN
        i := ComboBox_GetCurSel(ctrl^.wnd);
        IF i # CB_ERR THEN
            len := ComboBox_GetLBTextLen(ctrl^.wnd, i);
            IF len >= 0 THEN
                ALLOCATE(TempTextPtr, (len+1) * SIZE(CHAR));
                ComboBox_GetLBText(ctrl^.wnd, i, TempTextPtr^);
                ComboBox_SetText(ctrl^.wnd, TempTextPtr^);
                DEALLOCATE(TempTextPtr, (len+1) * SIZE(CHAR));
            END;
        END;
    END fillEditBox;

BEGIN
    msg.msg := WSM_NOTIFY;
    msg.ctrlId := LOWORD(wParam);
    ctrlWnd := CAST(HWND, lParam);
    ctrlMsg := HIWORD(wParam);

    ctrl := FindControl(w, msg.ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        CheckBox, PushButton, ToggleButton, RadioButton:
            IF ctrlMsg = BN_CLICKED THEN
                send(NM_Clicked);
            ELSIF ctrlMsg = BN_DBLCLK THEN
                send(NM_DoubleClicked);
            ELSIF ctrlMsg = BN_SETFOCUS THEN
                send(NM_GainFocus);
            ELSIF ctrlMsg = BN_KILLFOCUS THEN
                send(NM_LoseFocus);
            END;
        |
        TextEdit:
            IF ctrlMsg = EN_CHANGE THEN
                send(NM_ValueChanged);
            ELSIF ctrlMsg = EN_SETFOCUS THEN
                send(NM_GainFocus);
            ELSIF ctrlMsg = EN_KILLFOCUS THEN
                send(NM_LoseFocus);
            END;
        |
        SpinButton:
            IF ctrlMsg = EN_CHANGE THEN
                send(NM_ValueChanged);
            ELSIF ctrlMsg = EN_SETFOCUS THEN
                send(NM_GainFocus);
            ELSIF ctrlMsg = EN_KILLFOCUS THEN
                send(NM_LoseFocus);
            END;
        |
        ListBox:
            IF ctrlMsg = LBN_KILLFOCUS THEN
                send(NM_LoseFocus);
            ELSIF ctrlMsg = LBN_SETFOCUS THEN
                send(NM_GainFocus);
            ELSIF ctrlMsg = LBN_SELCHANGE THEN
                send(NM_SelectionChanged);
            ELSIF ctrlMsg = LBN_DBLCLK THEN
                send(NM_DoubleClicked);
            END;
        |
        ComboBox, DropDownList:
            IF ctrlMsg = CBN_EDITCHANGE THEN
                send(NM_ValueChanged);
            ELSIF ctrlMsg = CBN_KILLFOCUS THEN
                send(NM_LoseFocus);
            ELSIF ctrlMsg = CBN_SETFOCUS THEN
                send(NM_GainFocus);
            ELSIF ctrlMsg = CBN_SELCHANGE THEN
                fillEditBox;
                send(NM_SelectionChanged);
            ELSIF ctrlMsg = CBN_DBLCLK THEN
                send(NM_DoubleClicked);
            ELSIF ctrlMsg = CBN_DROPDOWN THEN
                send(NM_DropButtonClick);
            ELSIF ctrlMsg = CBN_CLOSEUP THEN
                send(NM_DropButtonClick);
            END;
        ELSE
        END;
    END;
END DoFormCommand;

PROCEDURE AlignFormControls(w : Window; align : CARDINAL);
VAR
     ctrl, sel  : ControlInfoPointer;
     newPos     : COORDINATE;
BEGIN
    sel := w^.selectedControl;
    ctrl := sel^.nextSel;

    CASE align OF
    CtrlMenuAlignLeft:
        newPos := sel^.x;
        WHILE ctrl <> NIL DO
            MoveControl(ctrl, newPos, ctrl^.y);
            ctrl := ctrl^.nextSel;
        END;
    |
    CtrlMenuAlignCenter:
        newPos := sel^.x + (sel^.width/2);
        WHILE ctrl <> NIL DO
            MoveControl(ctrl, newPos - (ctrl^.width/2), ctrl^.y);
            ctrl := ctrl^.nextSel;
        END;
    |
    CtrlMenuAlignRight:
        newPos := sel^.x + sel^.width;
        WHILE ctrl <> NIL DO
            MoveControl(ctrl, newPos - ctrl^.width, ctrl^.y);
            ctrl := ctrl^.nextSel;
        END;
    |
    CtrlMenuAlignTop:
        newPos := sel^.y;
        WHILE ctrl <> NIL DO
            MoveControl(ctrl, ctrl^.x, newPos);
            ctrl := ctrl^.nextSel;
        END;
    |
    CtrlMenuAlignMiddle:
        newPos := sel^.y + (sel^.height/2);
        WHILE ctrl <> NIL DO
            MoveControl(ctrl, ctrl^.x, newPos - (ctrl^.height/2));
            ctrl := ctrl^.nextSel;
        END;
    |
    CtrlMenuAlignBottom:
        newPos := sel^.y + sel^.height;
        WHILE ctrl <> NIL DO
            MoveControl(ctrl, ctrl^.x, newPos - ctrl^.height);
            ctrl := ctrl^.nextSel;
        END;
    ELSE
    END;
END AlignFormControls;

PROCEDURE ResizeFormControls(w : Window; resize : CARDINAL);
VAR
     ctrl, sel  : ControlInfoPointer;
BEGIN
    sel := w^.selectedControl;
    ctrl := sel^.nextSel;

    CASE resize OF
    CtrlMenuSizeWidth:
        WHILE ctrl <> NIL DO
            ResizeControl(ctrl, sel^.width, ctrl^.height);
            ctrl := ctrl^.nextSel;
        END;
    |
    CtrlMenuSizeHeight:
        WHILE ctrl <> NIL DO
            ResizeControl(ctrl, ctrl^.width, sel^.height);
            ctrl := ctrl^.nextSel;
        END;
    |
    CtrlMenuSizeBoth:
        WHILE ctrl <> NIL DO
            ResizeControl(ctrl, sel^.width, sel^.height);
            ctrl := ctrl^.nextSel;
        END;
    ELSE
    END;
END ResizeFormControls;

PROCEDURE OrderFormControls(w : Window; resize : CARDINAL);
VAR
     sel        : ControlInfoPointer;
     p          : ADRCARD;

    PROCEDURE moveZ(ctrl : ControlInfoPointer; afterThis : HWND);
    BEGIN
        WINUSER.SetWindowPos(ctrl^.wnd,
                             afterThis,
                             0, 0, 0, 0,
                             SWP_NOMOVE BOR SWP_NOSIZE BOR SWP_NOACTIVATE BOR SWP_NOREDRAW);

        WINUSER.SetWindowPos(ctrl^.dragWnd,
                             HWND_TOP,
                             0, 0, 0, 0,
                             SWP_NOMOVE BOR SWP_NOSIZE BOR SWP_NOACTIVATE BOR SWP_NOREDRAW);
    END moveZ;

    PROCEDURE moveUp(sel : ControlInfoPointer; VAR INOUT first, last : ControlInfoPointer);
    VAR
        prev,
        prevPrev,
        next            : ControlInfoPointer;
    BEGIN
        prev := sel^.prev;
        next := sel^.next;
        prevPrev := NIL;
        IF prev <> NIL THEN
            prevPrev := prev^.prev;
        END;

        prev^.next := next;
        prev^.prev := sel;
        sel^.next := prev;
        sel^.prev := prevPrev;
        IF next <> NIL THEN
            next^.prev := prev;
        END;

        IF prev = first THEN
            first := sel;
        END;

        IF prevPrev <> NIL THEN
            prevPrev^.next := sel;
        ELSE
            IF sel = last THEN
                last := prev;
            END;
        END;
    END moveUp;

    PROCEDURE moveDown(sel : ControlInfoPointer; VAR INOUT first, last : ControlInfoPointer);
    VAR
        prev,
        next,
        nextNext        : ControlInfoPointer;
    BEGIN
        prev := sel^.prev;
        next := sel^.next;
        nextNext := NIL;
        IF next <> NIL THEN
            nextNext := next^.next;
        END;

        next^.prev := prev;
        next^.next := sel;
        sel^.next := nextNext;
        sel^.prev := next;
        IF prev <> NIL THEN
            prev^.next := next;
        END;

        IF sel = first THEN
            first := next;
        END;

        IF nextNext <> NIL THEN
            nextNext^.prev := sel;
        ELSE
            IF next = last THEN
                last := sel;
            END;
        END;
    END moveDown;

    PROCEDURE moveTop(sel : ControlInfoPointer; VAR INOUT first, last : ControlInfoPointer);
    BEGIN
        sel^.prev^.next := sel^.next;
        IF sel^.next <> NIL THEN
            sel^.next^.prev := sel^.prev;
        ELSE
            last := sel^.prev;
        END;

        first^.prev := sel;
        sel^.next := first;
        sel^.prev := NIL;
        first := sel;
    END moveTop;

    PROCEDURE moveBottom(sel : ControlInfoPointer; VAR INOUT first, last : ControlInfoPointer);
    BEGIN
        sel^.next^.prev := sel^.prev;
        IF sel^.prev <> NIL THEN
            sel^.prev^.next := sel^.next;
        ELSE
            first := sel^.next;
        END;

        last^.next := sel;
        sel^.prev := last;
        sel^.next := NIL;
        last := sel;
    END moveBottom;

BEGIN
    sel := w^.selectedControl;

    CASE resize OF
    CtrlMenuOrderMoveUp:
        IF sel^.prev <> NIL THEN
            IF sel^.parent = NIL THEN
                w^.formChanged := TRUE;
                moveUp(sel, w^.firstControl, w^.lastControl);
            ELSIF sel^.parent^.type = MultiPage THEN
                w^.formChanged := TRUE;
                p := sel^.parent^.currentPage;
                moveUp(sel, sel^.parent^.pageInfo^[p].firstChild, sel^.parent^.pageInfo^[p].lastChild);
            ELSIF sel^.parent^.type = GroupBox THEN
                w^.formChanged := TRUE;
                moveUp(sel, sel^.parent^.firstChild, sel^.parent^.lastChild);
            ELSE
                RETURN;
            END;

            IF sel^.prev <> NIL THEN
                moveZ(sel, sel^.prev^.wnd);
            ELSE
                IF sel^.parent = NIL THEN
                    moveZ(sel, HWND_TOP);
                ELSE
                    moveZ(sel, sel^.parent^.wnd);
                END;
            END;
        END;
    |
    CtrlMenuOrderMoveDown:
        IF sel^.next <> NIL THEN
            IF sel^.parent = NIL THEN
                w^.formChanged := TRUE;
                moveDown(sel, w^.firstControl, w^.lastControl);
            ELSIF sel^.parent^.type = MultiPage THEN
                w^.formChanged := TRUE;
                p := sel^.parent^.currentPage;
                moveDown(sel, sel^.parent^.pageInfo^[p].firstChild, sel^.parent^.pageInfo^[p].lastChild);
            ELSIF sel^.parent^.type = GroupBox THEN
                w^.formChanged := TRUE;
                moveDown(sel, sel^.parent^.firstChild, sel^.parent^.lastChild);
            ELSE
                RETURN;
            END;

            moveZ(sel, sel^.prev^.wnd);
        END;
    |
    CtrlMenuOrderMoveToTop:
        IF sel^.prev <> NIL THEN
            IF sel^.parent = NIL THEN
                w^.formChanged := TRUE;
                moveTop(sel, w^.firstControl, w^.lastControl);
                moveZ(sel, HWND_TOP);
            ELSIF sel^.parent^.type = MultiPage THEN
                w^.formChanged := TRUE;
                p := sel^.parent^.currentPage;
                moveTop(sel, sel^.parent^.pageInfo^[p].firstChild, sel^.parent^.pageInfo^[p].lastChild);
                moveZ(sel, sel^.parent^.wnd);
            ELSIF sel^.parent^.type = GroupBox THEN
                w^.formChanged := TRUE;
                moveTop(sel, sel^.parent^.firstChild, sel^.parent^.lastChild);
                moveZ(sel, sel^.parent^.wnd);
            ELSE
                RETURN;
            END;
        END;
    |
    CtrlMenuOrderMoveToBottom:
        IF sel^.next <> NIL THEN
            IF sel^.parent = NIL THEN
                w^.formChanged := TRUE;
                moveBottom(sel, w^.firstControl, w^.lastControl);
                moveZ(sel, HWND_BOTTOM);
            ELSIF sel^.parent^.type = MultiPage THEN
                w^.formChanged := TRUE;
                p := sel^.parent^.currentPage;
                moveBottom(sel, sel^.parent^.pageInfo^[p].firstChild, sel^.parent^.pageInfo^[p].lastChild);
                moveZ(sel, sel^.parent^.wnd);
            ELSIF sel^.parent^.type = GroupBox THEN
                w^.formChanged := TRUE;
                moveBottom(sel, sel^.parent^.firstChild, sel^.parent^.lastChild);
                moveZ(sel, sel^.parent^.wnd);
            ELSE
                RETURN;
            END;
        END;
    ELSE
    END;
END OrderFormControls;

PROCEDURE GroupFormRadioControls(w : Window; group : BOOLEAN);
VAR
    ctrl        : ControlInfoPointer;
    groupNum    : CARDINAL;
    allRadio,
    sameGroup   : BOOLEAN;

    PROCEDURE getUniqueGroupNum(ctrl : ControlInfoPointer; group : CARDINAL) : CARDINAL;
    VAR
        i       : CARDINAL;
    BEGIN
        WHILE ctrl <> NIL DO
            IF ctrl^.group >= group THEN
                group := ctrl^.group + 1;
            END;

            IF ctrl^.type = GroupBox THEN
                group := getUniqueGroupNum(ctrl^.firstChild, group);
            ELSIF ctrl^.type = MultiPage THEN
                IF ctrl^.count > 0 THEN
                    FOR i := 0 TO ctrl^.count-1 DO
                        group := getUniqueGroupNum(ctrl^.pageInfo^[i].firstChild, group);
                    END;
                END;
            END;

            ctrl := ctrl^.next;
        END;
        RETURN group;
    END getUniqueGroupNum;

BEGIN
    IF w^.selectionCount > 1 THEN
        allRadio := TRUE;
        sameGroup := TRUE;
        ctrl := w^.selectedControl;
        WHILE ctrl <> NIL DO
            allRadio := allRadio AND (ctrl^.type = RadioButton);
            sameGroup := sameGroup AND (ctrl^.group = w^.selectedControl^.group);
            ctrl := ctrl^.nextSel;
        END;
        IF (NOT allRadio) OR ((NOT group) AND (NOT sameGroup)) THEN
            RETURN;
        END;
    ELSE
        RETURN;
    END;

    groupNum := 0;
    IF group THEN
        groupNum := getUniqueGroupNum(w^.firstControl, 1);
    END;

    group := FALSE;
    ctrl := w^.selectedControl;
    WHILE ctrl <> NIL DO
        group := group OR (ctrl^.group <> groupNum);
        ctrl^.group := groupNum;
        ctrl := ctrl^.nextSel;
    END;

    IF group THEN
        w^.formChanged := TRUE;
        SetRadioGrouping(w);
    END;
END GroupFormRadioControls;

PROCEDURE FormMultiPageMenu(w : Window; menuId : CARDINAL);
VAR
    ctrl        : ControlInfoPointer;
    pi          : PageInfoPointer;
    pageInfo    : MultiPageInfo;
    i, j        : ADRCARD;
    tcItem      : TCITEM;
    curPage     : ADRCARD;
    text        : ARRAY [0..127] OF CHAR;
BEGIN
    ctrl := w^.selectedControl;

    curPage := TabCtrl_GetCurSel(ctrl^.wnd);

    CASE menuId OF
    CtrlMenuMultiPageAdd:
        NEW(pi, ctrl^.count);
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
        <*/POP*>
            pi^[i] := ctrl^.pageInfo^[i];
        END;
        DISPOSE(ctrl^.pageInfo);
        ctrl^.pageInfo := pi;
        pi^[ctrl^.count].label := CreateStringData("NewPage");
        pi^[ctrl^.count].numControls := 0;
        pi^[ctrl^.count].firstChild := NIL;
        pi^[ctrl^.count].lastChild := NIL;

        tcItem.mask := TCIF_TEXT;
        tcItem.pszText := CAST(LPTSTR, pi^[ctrl^.count].label);
        TabCtrl_InsertItem(ctrl^.wnd, ctrl^.count, tcItem);
        INC(ctrl^.count);

        MultiPageSetActivePage(ctrl, ctrl^.count-1);
    |
    CtrlMenuMultiPageDelete:
        IF ctrl^.count > 1 THEN
            NEW(pi, ctrl^.count-1);
            j := 0;
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
            <*/POP*>
                IF i <> curPage THEN
                    pi^[j] := ctrl^.pageInfo^[i];
                    INC(j);
                END;
            END;
            DISPOSE(ctrl^.pageInfo);
            ctrl^.pageInfo := pi;
            DEC(ctrl^.count);

            TabCtrl_DeleteItem(ctrl^.wnd, curPage);

            <*/PUSH/NOWARN:U*>
            IF curPage = VAL(ADRCARD, ctrl^.count) THEN
            <*/POP*>
                DEC(curPage);
            END;
            MultiPageSetActivePage(ctrl, curPage);
        END;
    |
    CtrlMenuMultiPageMoveLeft:
        IF curPage > 0 THEN
            text[0] := '';(*suppress uninit warning*)
            tcItem.mask := TCIF_TEXT;
            tcItem.pszText := ADR(text);
            tcItem.cchTextMax := SIZE(text) / SIZE(CHAR);
            TabCtrl_GetItem(ctrl^.wnd, curPage, tcItem);
            TabCtrl_DeleteItem(ctrl^.wnd, curPage);
            TabCtrl_InsertItem(ctrl^.wnd, curPage-1, tcItem);
            pageInfo := ctrl^.pageInfo^[curPage];
            ctrl^.pageInfo^[curPage] := ctrl^.pageInfo^[curPage-1];
            ctrl^.pageInfo^[curPage-1] := pageInfo;

            MultiPageSetActivePage(ctrl, curPage-1);
        END;
    |
    CtrlMenuMultiPageMoveRight:
        <*/PUSH/NOWARN:U*>
        IF curPage+1 < VAL(ADRCARD, ctrl^.count) THEN
        <*/POP*>
            text[0] := '';(*suppress uninit warning*)
            tcItem.mask := TCIF_TEXT;
            tcItem.pszText := ADR(text);
            tcItem.cchTextMax := SIZE(text) / SIZE(CHAR);
            TabCtrl_GetItem(ctrl^.wnd, curPage, tcItem);
            TabCtrl_DeleteItem(ctrl^.wnd, curPage);
            TabCtrl_InsertItem(ctrl^.wnd, curPage, tcItem);
            pageInfo := ctrl^.pageInfo^[curPage];
            ctrl^.pageInfo^[curPage] := ctrl^.pageInfo^[curPage+1];
            ctrl^.pageInfo^[curPage+1] := pageInfo;

            MultiPageSetActivePage(ctrl, curPage+1);
        END;
    ELSE
    END;
END FormMultiPageMenu;

PROCEDURE CreateNewControl(w : Window; ctrlType : ControlTypes);
CONST
    pages       : ARRAY [0..1] OF ControlPageInfo =
    {
     {ADR("Page1"), 0, NIL},
     {ADR("Page2"), 0, NIL}
    };

    defaultProps : ARRAY ControlTypes OF ControlProperties =
    {
     {0, 0, 0, 60, 10, NIL, -1, TRUE, TRUE, CheckBox, ADR("CheckBox"), -1},
     {0, 0, 0, 60, 10, NIL, -1, TRUE, TRUE, RadioButton, ADR("RadioButton"), -1, 0},
     {0, 0, 0, 60, 14, NIL, -1, TRUE, TRUE, PushButton, ADR("PushButton"), -1, FALSE},
     {0, 0, 0, 60, 14, NIL, -1, TRUE, TRUE, ToggleButton, ADR("ToggleButton"), -1, FALSE},
     {0, 0, 0, 60, 14, NIL, -1, TRUE, TRUE, SpinButton, 0, 100, TRUE},
     {0, 0, 0, 60, 40, NIL, -1, TRUE, TRUE, ListBox, 1, NIL, FALSE},
     {0, 0, 0, 60, 40, NIL, -1, TRUE, TRUE, DropDownList},
     {0, 0, 0, 60, 40, NIL, -1, TRUE, TRUE, ComboBox, 0},
     {0, 0, 0, 60,  8, NIL, -1, TRUE, TRUE, TextLabel, ADR("TextLabel"), -1, AlignLeft},
     {0, 0, 0, 60, 12, NIL, -1, TRUE, TRUE, TextEdit, AlignLeft, 0, FALSE, FALSE, FALSE, FALSE},
     {0, 0, 0, 60, 40, NIL, -1, TRUE, TRUE, GroupBox, ADR("GroupBox"), -1, 0, NIL},
     {0, 0, 0, 60, 40, NIL, -1, TRUE, TRUE, MultiPage, 2, ADR(pages)}
    };
VAR
    info        : ControlProperties;
    ctrl        : ControlInfoPointer;
    pi          : ControlPageInfoArray;
    i           : CARDINAL;
BEGIN
    info := defaultProps[ctrlType];
    info.idNum := w^.nextCtrlId;
    INC(w^.nextCtrlId);

    CASE info.controlType OF
    CheckBox:
        info.chk_textPtr := DuplicateStringData(info.chk_textPtr);
    |
    RadioButton:
        info.rb_textPtr := DuplicateStringData(info.rb_textPtr);
    |
    PushButton, ToggleButton:
        info.b_textPtr := DuplicateStringData(info.b_textPtr);
    |
    SpinButton:
    |
    ListBox:
    |
    DropDownList:
    |
    ComboBox:
    |
    TextLabel:
        info.tl_textPtr := DuplicateStringData(info.tl_textPtr);
    |
    TextEdit:
    |
    GroupBox:
        info.grp_textPtr := DuplicateStringData(info.grp_textPtr);
        info.grp_numControls := 0;
        info.grp_controls := NIL;
    |
    MultiPage:
        ALLOCATE(pi, info.mp_numPages * SIZE(ControlPageInfo));
        info.mp_pageInfo := pi;
        i := 0;
        WHILE i < info.mp_numPages DO
            pi^[i].label := DuplicateStringData(pages[i].label);
            pi^[i].numControls := 0;
            pi^[i].controls := NIL;
            INC(i);
        END;
    END;

    w^.newControlProc(w, info);

    w^.dropControl := ADR(info);
    CaptureMouse(w);
    SetCursor(DropCursor);

    DispatchMessagesTemp();

    ReleaseMouse(w);

    w^.dropControl := NIL;
    w^.formChanged := TRUE;

    ctrl := FindControl(w, info.idNum);
    IF ctrl <> NIL THEN
        SetSelectedControl(w, ctrl);
    END;

    DisposeControlProps(info);
END CreateNewControl;

PROCEDURE DoFormMenu(w : Window; menuId : ADRCARD);
CONST
    ctrlType    : ARRAY [CtrlMenuNewCheckBox..CtrlMenuNewMultiPage] OF ControlTypes =
    {
     CheckBox, RadioButton,
     PushButton, ToggleButton,
     SpinButton,
     ListBox, DropDownList, ComboBox,
     TextLabel, TextEdit,
     GroupBox,
     MultiPage
    };

    PROCEDURE editTitle;
    VAR
        prop    : ControlProperties;
        text    : ARRAY [0..255] OF CHAR;
    BEGIN
        prop.idNum := MAX(CARDINAL);
        prop.controlType := TextEdit;
        WINUSER.GetWindowText(w^.wnd, text, HIGH(text)+1);
        prop.tipTextPtr := CreateStringData(text);

        IF w^.propProc(w, prop) THEN
            w^.formChanged := TRUE;
        END;

        SetWindowText(w^.wnd, prop.tipTextPtr^);

        DisposeStringData(prop.tipTextPtr);
    END editTitle;

BEGIN
    CASE menuId OF
    CtrlMenuProperties:
        IF w^.selectedControl <> NIL THEN
            CallPropProc(w^.selectedControl);
        END;
    |
    CtrlMenuDelete:
        WHILE w^.selectedControl <> NIL DO
            DestroyControl(w^.selectedControl);
        END;
    |
    CtrlMenuGroup, CtrlMenuUngroup:
        IF w^.selectionCount > 1 THEN
            GroupFormRadioControls(w, menuId = CtrlMenuGroup);
        END;
    |
    CtrlMenuFormTitle:
        editTitle;
    |
    CtrlMenuAlignLeft..CtrlMenuAlignBottom:
        IF w^.selectionCount > 1 THEN
            AlignFormControls(w, menuId);
        END;
    |
    CtrlMenuSizeWidth..CtrlMenuSizeBoth:
        IF w^.selectionCount > 1 THEN
            ResizeFormControls(w, menuId);
        END;
    |
    CtrlMenuOrderMoveUp..CtrlMenuOrderMoveToBottom:
        IF w^.selectionCount = 1 THEN
            OrderFormControls(w, menuId);
        END;
    |
    CtrlMenuMultiPageAdd..CtrlMenuMultiPageMoveRight:
        IF (w^.selectionCount = 1) AND
           (w^.selectedControl <> NIL) AND
           (w^.selectedControl^.type = MultiPage)
        THEN
            FormMultiPageMenu(w, menuId);
        END;
    |
    CtrlMenuNewCheckBox..CtrlMenuNewMultiPage:
        CreateNewControl(w, ctrlType[menuId]);
    |
    CtrlMenuExitFormEdit:
        w^.exitProc(w);
    ELSE
    END;
END DoFormMenu;

PROCEDURE DrawDragRect(w : Window);
CONST
    thickness   = 1;
BEGIN
    PatBlt(w^.dragDC, w^.dragX, w^.dragY, w^.dragW, thickness, DSTINVERT);

    PatBlt(w^.dragDC, w^.dragX, w^.dragY, thickness, w^.dragH, DSTINVERT);

    PatBlt(w^.dragDC, w^.dragX + w^.dragW - 1, w^.dragY, thickness, w^.dragH, DSTINVERT);

    PatBlt(w^.dragDC, w^.dragX, w^.dragY + w^.dragH - 1, w^.dragW, thickness, DSTINVERT);
END DrawDragRect;

PROCEDURE DragRectDragBegin(w : Window; ctrl : ControlInfoPointer);
VAR
    rc, rc1, rc2        : RECT;
BEGIN
    w^.dragMode := Dragging;

    IF ctrl <> NIL THEN
        SetCapture(ctrl^.dragWnd);

        IF ctrl^.hitTest = HtClient THEN
            SetCursor(MoveCursor);
        END;

        GetDragRect(ctrl, rc);

        IF w^.selectionCount > 1 THEN
            ctrl := w^.selectedControl;
            WHILE ctrl <> NIL DO
                GetDragRect(ctrl, rc1);
                WINUSER.UnionRect(rc2, rc, rc1);
                rc := rc2;
                ctrl := ctrl^.nextSel;
            END;
        END;
    ELSE
        SetCapture(w^.clientWnd);

        rc.left := w^.dragX1;
        rc.top := w^.dragY1;
        rc.right := rc.left + 1;
        rc.bottom := rc.top + 1;
    END;

    w^.dragX := rc.left;
    w^.dragSX := rc.left;
    w^.dragY := rc.top;
    w^.dragSY := rc.top;
    w^.dragW := rc.right - rc.left;
    w^.dragH := rc.bottom - rc.top;

    w^.dragDC := GetDC(w^.clientWnd);

    DrawDragRect(w);
END DragRectDragBegin;

PROCEDURE CheckControlContainment(ctrl : ControlInfoPointer);

    PROCEDURE findContainer(ctrl : ControlInfoPointer) : ControlInfoPointer;
    BEGIN
        WHILE (ctrl <> NIL) AND (ctrl^.type <> MultiPage) AND (ctrl^.type <> GroupBox) DO
            ctrl := ctrl^.next;
        END;
        RETURN ctrl;
    END findContainer;

    PROCEDURE ctrlInContainer(ctrl, cont : ControlInfoPointer) : BOOLEAN;
    VAR
        rectCont        : RECT;
        rectCtrl        : RECT;
        pt              : POINT;
    BEGIN
        GetControlRect(cont, FALSE, rectCont);
        GetControlRect(ctrl, FALSE, rectCtrl);

        pt.x := rectCtrl.left;
        pt.y := rectCtrl.top;
        RETURN WINUSER.PtInRect(rectCont, pt);
    END ctrlInContainer;

    PROCEDURE moveWndPos(ctrl : ControlInfoPointer; wndPos : HWND);
    VAR
        i       : ADRCARD;
        child   : ControlInfoPointer;
    BEGIN
        WINUSER.SetWindowPos(ctrl^.wnd,
                             wndPos,
                             0, 0, 0, 0,
                             SWP_NOMOVE BOR SWP_NOSIZE BOR SWP_NOACTIVATE BOR SWP_NOREDRAW);
        IF ctrl^.spinWnd <> NIL THEN
            WINUSER.SetWindowPos(ctrl^.spinWnd,
                                 ctrl^.wnd,
                                 0, 0, 0, 0,
                                 SWP_NOMOVE BOR SWP_NOSIZE BOR SWP_NOACTIVATE BOR SWP_NOREDRAW);
        END;

        (*IF ctrl^.dragWnd <> NIL THEN
            WINUSER.SetWindowPos(ctrl^.dragWnd,
                                 HWND_TOP,
                                 0, 0, 0, 0,
                                 SWP_NOMOVE BOR SWP_NOSIZE BOR SWP_NOACTIVATE BOR SWP_NOREDRAW);
        END;*)

        IF ctrl^.type = GroupBox THEN
            ctrl := ctrl^.firstChild;
            WHILE ctrl <> NIL DO
                moveWndPos(ctrl, wndPos);
                ctrl := ctrl^.next;
            END;
        ELSIF (ctrl^.type = MultiPage) AND (ctrl^.count > 0) THEN
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
            <*/POP*>
                child := ctrl^.pageInfo^[i].firstChild;
                WHILE child <> NIL DO
                    moveWndPos(child, wndPos);
                    child := child^.next;
                END;
            END;
        END;
    END moveWndPos;

    PROCEDURE linkToContainer(ctrl : ControlInfoPointer; VAR INOUT first, last : ControlInfoPointer);
    BEGIN
        (* remove it from the main list *)

        IF ctrl^.prev <> NIL THEN
            ctrl^.prev^.next := ctrl^.next;
        ELSE
            ctrl^.w^.firstControl := ctrl^.next;
            ctrl^.w^.firstControl^.prev := NIL;
        END;
        IF ctrl^.next <> NIL THEN
            ctrl^.next^.prev := ctrl^.prev;
        ELSE
            ctrl^.w^.lastControl := ctrl^.prev;
            ctrl^.w^.lastControl^.next := NIL;
        END;
        DEC(ctrl^.w^.numControlsMain);

        (* add it to the container list *)

        IF first <> NIL THEN
            ctrl^.prev := last;
            last^.next := ctrl;
        ELSE
            ctrl^.prev := NIL;
            first := ctrl;
        END;
        last := ctrl;
        ctrl^.next := NIL;
    END linkToContainer;

    PROCEDURE moveControlToPage(ctrl, multi : ControlInfoPointer);
    VAR
        mpi     : POINTER TO MultiPageInfo;
        wndPos  : HWND;
    BEGIN
        IF ctrl^.parent <> multi THEN
            ctrl^.w^.formChanged := TRUE;

            mpi := ADR(multi^.pageInfo^[multi^.currentPage]);

            IF mpi^.lastChild <> NIL THEN
                wndPos := mpi^.lastChild^.wnd;
            ELSE
                wndPos := multi^.wnd;
            END;

            ctrl^.parent := multi;
            linkToContainer(ctrl, mpi^.firstChild, mpi^.lastChild);
            INC(mpi^.numControls);

            moveWndPos(ctrl, wndPos);
        END;
    END moveControlToPage;

    PROCEDURE moveControlToGroup(ctrl, group : ControlInfoPointer);
    VAR
        wndPos  : HWND;
    BEGIN
        IF ctrl^.parent <> group THEN
            ctrl^.w^.formChanged := TRUE;

            IF group^.lastChild <> NIL THEN
                wndPos := group^.lastChild^.wnd;
            ELSE
                wndPos := group^.wnd;
            END;

            ctrl^.parent := group;
            linkToContainer(ctrl, group^.firstChild, group^.lastChild);
            INC(group^.count);

            moveWndPos(ctrl, wndPos);
        END;
    END moveControlToGroup;

    PROCEDURE moveControlToContainer(ctrl, cont : ControlInfoPointer);
    BEGIN
        IF cont^.type = MultiPage THEN
            moveControlToPage(ctrl, cont);
        ELSIF cont^.type = GroupBox THEN
            moveControlToGroup(ctrl, cont);
        END;
    END moveControlToContainer;

    PROCEDURE linkToMain(ctrl, cont : ControlInfoPointer; VAR INOUT first, last : ControlInfoPointer);
    BEGIN
        ctrl^.parent := NIL;

        (* remove it from the container control list *)

        IF ctrl^.prev <> NIL THEN
            ctrl^.prev^.next := ctrl^.next;
        ELSE
            first := ctrl^.next;
            IF first <> NIL THEN
                first^.prev := NIL;
            END;
        END;
        IF ctrl^.next <> NIL THEN
            ctrl^.next^.prev := ctrl^.prev;
        ELSE
            last := ctrl^.prev;
            IF last <> NIL THEN
                last^.next := NIL;
            END;
        END;
(*MVN+*)
(*
        (* put it into the main list just after the container control *)

        ctrl^.prev := cont;
        ctrl^.next := cont^.next;
        IF cont^.next <> NIL THEN
            cont^.next^.prev := ctrl;
        ELSE
            cont^.w^.lastControl := ctrl;
        END;
        cont^.next := ctrl;
*)
        (* put it into the main list just after the last control *)

        ctrl^.prev := cont^.w^.lastControl;
        IF cont^.w^.lastControl # NIL THEN
            ctrl^.next := cont^.w^.lastControl^.next;
            cont^.w^.lastControl^.next := ctrl;
        ELSE
            cont^.w^.firstControl := ctrl;
            ctrl^.next := NIL;
        END;
        cont^.w^.lastControl := ctrl;
(*MVN-*)

        INC(cont^.w^.numControlsMain);
    END linkToMain;

    PROCEDURE removeControlFromPage(ctrl : ControlInfoPointer);
    VAR
        mpi     : POINTER TO MultiPageInfo;
        multi   : ControlInfoPointer;
        wndPos  : HWND;
    BEGIN
        ctrl^.w^.formChanged := TRUE;

        multi := ctrl^.parent;
        mpi := ADR(multi^.pageInfo^[multi^.currentPage]);

        linkToMain(ctrl, multi, mpi^.firstChild, mpi^.lastChild);
        DEC(mpi^.numControls);

        IF mpi^.lastChild <> NIL THEN
            wndPos := mpi^.lastChild^.wnd;
        ELSE
            wndPos := multi^.wnd;
        END;
        moveWndPos(ctrl, wndPos);
    END removeControlFromPage;

    PROCEDURE removeControlFromGroup(ctrl : ControlInfoPointer);
    VAR
        group   : ControlInfoPointer;
        wndPos  : HWND;
    BEGIN
        ctrl^.w^.formChanged := TRUE;

        group := ctrl^.parent;

        linkToMain(ctrl, group, group^.firstChild, group^.lastChild);
        DEC(group^.count);

        IF ctrl^.lastChild <> NIL THEN
            wndPos := ctrl^.lastChild^.wnd;
        ELSE
            wndPos := group^.wnd;
        END;
        moveWndPos(ctrl, wndPos);
    END removeControlFromGroup;

    PROCEDURE removeControlFromContainer(ctrl : ControlInfoPointer);
    BEGIN
        IF ctrl^.parent^.type = MultiPage THEN
            removeControlFromPage(ctrl);
        ELSIF ctrl^.parent^.type = GroupBox THEN
            removeControlFromGroup(ctrl);
        END;
    END removeControlFromContainer;

    PROCEDURE checkContainers(list, ctrl : ControlInfoPointer) : BOOLEAN;
    VAR
        cont            : ControlInfoPointer;
        cp              : ADRCARD;
        contained       : BOOLEAN;
    BEGIN
        cont := findContainer(list);
        LOOP
            IF cont <> NIL THEN
                IF ctrl <> cont THEN
                    IF ctrlInContainer(ctrl, cont) THEN
                        contained := FALSE;
                        IF cont^.count > 0 THEN
                            IF cont^.type = GroupBox THEN
                                contained := checkContainers(cont^.firstChild, ctrl);
                            ELSIF (cont^.type = MultiPage) AND (cont^.count > 0) THEN
                                cp := cont^.currentPage;
                                contained := checkContainers(cont^.pageInfo^[cp].firstChild, ctrl);
                            END;
                        END;

                        IF NOT contained THEN
                            moveControlToContainer(ctrl, cont);
                        END;
                        RETURN TRUE;
                    END;
                END;

                cont := findContainer(cont^.next);
            ELSE
                RETURN FALSE;
            END;
        END;
    END checkContainers;

BEGIN
    (* check to see if the control has been moved into or out of the confines of a container control. *)

    IF ctrl^.parent <> NIL THEN
(*MVN+*)
(*
        IF NOT ctrlInContainer(ctrl, ctrl^.parent) THEN
            (* it moved out of its current parent *)

            removeControlFromContainer(ctrl);
        END;
*)
        removeControlFromContainer(ctrl);
(*MVN-*)
    END;

    (* did it move into a container control *)

    IF checkContainers(ctrl^.w^.firstControl, ctrl) THEN
    END;
END CheckControlContainment;

PROCEDURE GetChildFudge(ctrl : ControlInfoPointer);
VAR
    rc          : RECT;
BEGIN
    IF ctrl^.type = GroupBox THEN
        GetWindowRect(ctrl^.wnd, rc);
        MapWindowPoints(HWND_DESKTOP, ctrl^.w^.clientWnd, CAST(POINT, rc), 2);
        ctrl^.childFudgeX := rc.left + (ctrl^.w^.formCharX / 2);
        ctrl^.childFudgeY := rc.top + ctrl^.w^.formCharY;
    ELSIF ctrl^.type = MultiPage THEN
        GetWindowRect(ctrl^.wnd, rc);
        MapWindowPoints(HWND_DESKTOP, ctrl^.w^.clientWnd, CAST(POINT, rc), 2);
        TabCtrl_AdjustRect(ctrl^.wnd, FALSE, rc);
        ctrl^.childFudgeX := rc.left;
        ctrl^.childFudgeY := rc.top;
    END;
END GetChildFudge;

PROCEDURE MoveSizeControl(ctrl : ControlInfoPointer; x, y, w, h : COORDINATE);

    PROCEDURE showHide(ctrl : ControlInfoPointer; flags : CARDINAL);
    BEGIN
        WINUSER.SetWindowPos(ctrl^.wnd, NIL, 0, 0, 0, 0, flags);
        IF ctrl^.spinWnd <> NIL THEN
            WINUSER.SetWindowPos(ctrl^.spinWnd, NIL, 0, 0, 0, 0, flags);
        END;
    END showHide;

    PROCEDURE moveChildList(child : ControlInfoPointer; dx, dy : COORDINATE);
    BEGIN
        WHILE child <> NIL DO
            MoveSizeControl(child, child^.x + dx, child^.y + dy, child^.width, child^.height);
(*MVN+
            moveChildren(child, dx, dy);
MVN-*)
            child := child^.next;
        END;
    END moveChildList;

    PROCEDURE moveChildren(ctrl : ControlInfoPointer; dx, dy : COORDINATE);
    VAR
        i       : ADRCARD;
    BEGIN
        IF ctrl^.type = MultiPage THEN
            IF ctrl^.count > 0 THEN
                <*/PUSH/NOWARN:U*>
                FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
                <*/POP*>
                    moveChildList(ctrl^.pageInfo^[i].firstChild, dx, dy);
                END;
            END;
        ELSIF ctrl^.type = GroupBox THEN
            moveChildList(ctrl^.firstChild, dx, dy);
        END;
    END moveChildren;

VAR
    dx, dy      : COORDINATE;
    posChanged  : BOOLEAN;
    sizeChanged : BOOLEAN;
BEGIN
(*MVN+*)
    IF w <= 0 THEN
        w := 1;
    END;
    IF h <= 0 THEN
        h := 1;
    END;
(*MVN-*)
    dx := x - ctrl^.x;
    dy := y - ctrl^.y;

    posChanged := (ctrl^.x <> x) OR (ctrl^.y <> y);
    sizeChanged := (ctrl^.width <> w) OR (ctrl^.height <> h);
    ctrl^.w^.formChanged := ctrl^.w^.formChanged OR posChanged OR sizeChanged;

    ctrl^.x := x;
    ctrl^.y := y;
    ctrl^.width := w;
    ctrl^.height := h;

    FormToPixel(ctrl^.w, x, y);
    FormToPixel(ctrl^.w, w, h);

    IF ctrl^.pageVisible THEN
        (* Windows has some strange painting quirks with some apparent bitblts when
           an overlapping move occurs with container controls and its children.
           garbage ramains on screen inside the container with regards to the children.
           easy solution is to just hide and then show.
        *)
        showHide(ctrl, SWP_NOMOVE BOR SWP_NOSIZE BOR SWP_HIDEWINDOW BOR SWP_NOZORDER);
    END;

    MoveWindow(ctrl^.wnd, x, y, w, h, FALSE);
    IF ctrl^.spinWnd <> NIL THEN
        WINUSER.SendMessage(ctrl^.spinWnd, UDM_SETBUDDY, CAST(WPARAM, ctrl^.wnd), 0);
    END;
    IF ctrl^.selected THEN
        MoveWindow(ctrl^.dragWnd,
                   x - DragHandleSize, y - DragHandleSize,
                   w + (2*DragHandleSize), h + (2*DragHandleSize),
                   ctrl^.pageVisible);
    ELSE
        MoveWindow(ctrl^.dragWnd, x, y, w, h, ctrl^.pageVisible);
    END;

    IF (ctrl^.type = GroupBox) OR (ctrl^.type = MultiPage) THEN
        GetChildFudge(ctrl);
        IF posChanged THEN
            moveChildren(ctrl, dx, dy);
        END;
    END;

    IF ctrl^.pageVisible THEN
        showHide(ctrl, SWP_NOMOVE BOR
                            SWP_NOSIZE BOR
                            SWP_SHOWWINDOW BOR
                            SWP_NOZORDER BOR
                            SWP_NOACTIVATE);
    END;
END MoveSizeControl;

PROCEDURE MoveControl(ctrl : ControlInfoPointer; x, y : COORDINATE);
BEGIN
    MoveSizeControl(ctrl, x, y, ctrl^.width, ctrl^.height);
END MoveControl;

PROCEDURE ResizeControl(ctrl : ControlInfoPointer; w, h : COORDINATE);
BEGIN
    MoveSizeControl(ctrl, ctrl^.x, ctrl^.y, w, h);
END ResizeControl;

PROCEDURE DragRectDragEnd(w : Window; ctrl : ControlInfoPointer);
VAR
    rc          : RECT;
    temp        : INTEGER;
    clear       : BOOLEAN;

    PROCEDURE selectControls(ctrl : ControlInfoPointer);
    VAR
        rc1, rc2        : RECT;
    BEGIN
        WHILE ctrl <> NIL DO
            IF IsWindowVisible(ctrl^.wnd) THEN
                GetControlRect(ctrl, FALSE, rc1);
                IF WINUSER.IntersectRect(rc2, rc, rc1) THEN
                    IF clear THEN
                        clear := FALSE;
                        ClearSelectedControls(w);
                    END;
                    AddSelectedControl(w, ctrl);
                END;
            END;

            IF ctrl^.type = GroupBox THEN
                selectControls(ctrl^.firstChild);
            ELSIF ctrl^.type = MultiPage THEN
                selectControls(ctrl^.pageInfo^[ctrl^.currentPage].firstChild);
            END;

            ctrl := ctrl^.next;
        END;
    END selectControls;

    PROCEDURE containerInSelection(w : Window; ctrl : ControlInfoPointer) : BOOLEAN;
    BEGIN
        IF ctrl^.parent <> NIL THEN
            IF ctrl^.parent^.selected THEN
                RETURN TRUE;
            END;
            RETURN containerInSelection(w, ctrl^.parent);
        END;
        RETURN FALSE;
    END containerInSelection;

BEGIN
    DrawDragRect(w);

    ReleaseDC(w^.clientWnd, w^.dragDC);
    w^.dragMode := NoDrag;
    w^.dragDC := NIL;

    ReleaseCapture;

    IF ctrl <> NIL THEN
        IF w^.selectionCount = 1 THEN
            w^.dragX := w^.dragX + DragHandleSize;
            w^.dragY := w^.dragY + DragHandleSize;
            PixelToForm(w, w^.dragX, w^.dragY);

            w^.dragW := w^.dragW - (2*DragHandleSize);
            w^.dragH := w^.dragH - (2*DragHandleSize);
            PixelToForm(w, w^.dragW, w^.dragH);

            IF (w^.dragW = ctrl^.width) AND (w^.dragH = ctrl^.height) THEN
                MoveControl(ctrl, w^.dragX, w^.dragY);
                CheckControlContainment(ctrl);
            ELSE
                MoveSizeControl(ctrl, w^.dragX, w^.dragY, w^.dragW, w^.dragH);
            END;
        ELSE
            w^.dragX := w^.dragX - w^.dragSX;
            w^.dragY := w^.dragY - w^.dragSY;
            PixelToForm(w, w^.dragX, w^.dragY);

            ctrl := w^.selectedControl;
            WHILE ctrl <> NIL DO
                IF NOT containerInSelection(w, ctrl) THEN
                    MoveControl(ctrl, ctrl^.x + w^.dragX, ctrl^.y + w^.dragY);
                END;
                ctrl := ctrl^.nextSel;
            END;

            ctrl := w^.selectedControl;
            WHILE ctrl <> NIL DO
                CheckControlContainment(ctrl);
                ctrl := ctrl^.nextSel;
            END;
        END;
    ELSE
        (* select all controls within the dragged rectangle *)

        rc.left := w^.dragX;
        rc.top := w^.dragY;
        rc.right := rc.left + w^.dragW;
        rc.bottom := rc.top + w^.dragH;
        IF rc.right < rc.left THEN
            temp := rc.left;
            rc.left := rc.right;
            rc.right := temp;
        END;
        IF rc.bottom < rc.top THEN
            temp := rc.top;
            rc.top := rc.bottom;
            rc.bottom := temp;
        END;

        clear := TRUE;
        selectControls(w^.firstControl);
    END;
END DragRectDragEnd;

PROCEDURE UpdateDragRect(w : Window; ctrl : ControlInfoPointer; dx, dy : INTEGER);
BEGIN
    DrawDragRect(w);

    IF ctrl <> NIL THEN
        CASE ctrl^.hitTest OF
        HtClient:
            w^.dragX := w^.dragX + dx;
            w^.dragY := w^.dragY + dy;
        |
        HtTopLeft:
            w^.dragX := w^.dragX + dx;
            w^.dragW := w^.dragW - dx;
            w^.dragY := w^.dragY + dy;
            w^.dragH := w^.dragH - dy;
        |
        HtTop:
            w^.dragY := w^.dragY + dy;
            w^.dragH := w^.dragH - dy;
        |
        HtTopRight:
            w^.dragW := w^.dragW + dx;
            w^.dragY := w^.dragY + dy;
            w^.dragH := w^.dragH - dy;
        |
        HtLeft:
            w^.dragX := w^.dragX + dx;
            w^.dragW := w^.dragW - dx;
        |
        HtRight:
            w^.dragW := w^.dragW + dx;
        |
        HtBottomLeft:
            w^.dragX := w^.dragX + dx;
            w^.dragW := w^.dragW - dx;
            w^.dragH := w^.dragH + dy;
        |
        HtBottom:
            w^.dragH := w^.dragH + dy;
        |
        HtBottomRight:
            w^.dragW := w^.dragW + dx;
            w^.dragH := w^.dragH + dy;
        END;
    ELSE
        w^.dragW := w^.dragW + dx;
        w^.dragH := w^.dragH + dy;
    END;

    DrawDragRect(w);
END UpdateDragRect;

PROCEDURE GetFormUnits(w : Window; font : HFONT);
VAR
    dc          : HDC;
    oldFont     : HFONT;
    tm          : TEXTMETRIC;
BEGIN
    w^.formFont := font;

    dc := GetDC(w^.wnd);

    oldFont := SelectFont(dc, font);

    WINGDI.GetTextMetrics(dc, tm);
    w^.formCharX := ComputeAverageCharWidth(dc);
    w^.formCharY := tm.tmHeight;

    SelectFont(dc, oldFont);

    ReleaseDC(w^.wnd, dc);
END GetFormUnits;

PROCEDURE DoFormMouse(w : Window; mess : UINT; wParam : WPARAM; lParam : LPARAM);
(* some controls pass mouse clicks to their parent.
    specifically TabControl and static text.
    we filter those situations out.
*)
VAR
    ctrl        : ControlInfoPointer;
    x, y        : COORDINATE;
BEGIN
(*MVN+*)
    IF w^.formEditMode THEN
(*MVN-*)
    x := CAST(INTEGER16, LOWORD(lParam));
    y := CAST(INTEGER16, HIWORD(lParam));
    PixelToForm(w, x, y);

    ctrl := w^.firstControl;
    WHILE ctrl <> NIL DO
        IF ((x >= ctrl^.x) AND (x < ctrl^.x+ctrl^.width)) AND
           ((y >= ctrl^.y) AND (y < ctrl^.y+ctrl^.height))
        THEN
            RETURN;
        END;

        ctrl := ctrl^.next;
    END;
(*MVN+*)
    END;
(*MVN-*)

    DoMouse(w, mess, wParam, lParam);
END DoFormMouse;

PROCEDURE FormClientWndProc(hwnd : HWND; mess : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT [EXPORT, SbOsSystem];
VAR
    w           : Window;
    msg         : MessageRec;
    x, y        : COORDINATE;
    pt          : POINT;
    info        : ControlProperties;
    ctrl        : ControlInfoPointer;
    top         : Window;

BEGIN
    w := CAST(Window, GetWindowLongPtr(hwnd, WindowDataPos[FormClient]));

    CASE mess OF
    WM_CREATE:
        w := CAST(LPCREATESTRUCT, lParam)^.lpCreateParams;
        SetWindowLongPtr(hwnd, WindowDataPos[FormClient], CAST(LONG_PTR, w));

        SetModelessDialog(hwnd, TRUE);

        GetFormUnits(w, FormFont);

        w^.clientWnd := hwnd;
        IF w^.type = ChildWindow THEN
            w^.wnd := hwnd;
        END;

        top := GetToplevel(w);

        w^.toolTipWnd := CreateWindowEx(WS_EX_TOPMOST,
                                        TOOLTIPS_CLASS,
                                        NIL_STR,
                                        WS_POPUP BOR TTS_NOPREFIX,
                                        CW_USEDEFAULT, CW_USEDEFAULT,
                                        CW_USEDEFAULT, CW_USEDEFAULT,
                                        top^.wnd,(*parent*)
                                        NIL,(*hmenu*)
                                        Instance,
                                        NIL(*lparam*));

        IF w^.toolTipWnd <> NIL THEN
            WINUSER.SendMessage(w^.toolTipWnd, TTM_SETMAXTIPWIDTH, 0, ScreenInfo.xSize / 2);
        END;

        (*let def dialog proc have a look*)
    |
    WM_CLOSE:
        IF DoCloseRequest(w) THEN
            DestroyWindow(w^.wnd);
            RETURN 1;
        END;
        RETURN 0;
    |
    WM_DESTROY:
        SetModelessDialog(hwnd, FALSE);

        IF IsWindow(w) THEN(*be paranoid safe*)
            msg.msg := WSM_CLOSE;
            msg.closeMode := CM_DICTATE;
            CallWndProc(w, msg);

            SetWindowLongPtr(hwnd, WindowDataPos[FormClient], CAST(LONG_PTR, NIL));

            IF IsTabChild(w) THEN
                HideTabChild(w);
            ELSIF IsSplitterChild(w) THEN
                SetSplitterHandle(w, FALSE);
            END;
            IF w^.type = ChildWindow THEN
                DisposeWindow(w);
            END;
        END;
        RETURN 0;
    |
    WM_SETFOCUS:
        DoSetFocus(w, wParam);
        (*let def dialog proc have a look*)
    |
    WM_KILLFOCUS:
        DoKillFocus(w);
        (*let def dialog proc have a look*)
    |
    WM_COMMAND:
        IF lParam <> 0 THEN
            DoFormCommand(w, wParam, lParam);
        ELSE
            (* a popup menu command *)
            DoFormMenu(w, LOWORD(wParam));
        END;
        RETURN 0;
    |
    WM_NOTIFY:
        IF DoNotify(w, CAST(LPNMHDR, lParam)) = 1 THEN
            RETURN 1;
        END;
    |
    WM_LBUTTONDOWN:
        IF w <> NIL THEN
            CaptureMouse(w);

            IF w^.formEditMode THEN
                IF w^.dropControl = NIL THEN
                    w^.dragX1 := CAST(INTEGER16, LOWORD(lParam));
                    w^.dragY1 := CAST(INTEGER16, HIWORD(lParam));
                    w^.dragMode := PreDrag;

                    IF w^.selectionCount > 1 THEN
                        ctrl := w^.selectedControl;
                        ClearSelectedControls(w);
                        SetSelectedControl(w, ctrl);
                    END;
                END;
            ELSE
                DoFormMouse(w, mess, wParam, lParam);
            END;

            RETURN 0;
        END;
    |
    WM_RBUTTONDOWN:
        IF w <> NIL THEN
            IF w^.formEditMode THEN
                CaptureMouse(w);
                pt.y := CAST(INTEGER16, HIWORD(lParam));
                pt.x := CAST(INTEGER16, LOWORD(lParam));
                WINUSER.ClientToScreen(hwnd, pt);
                FormEditorPopupMenu(w, pt.x, pt.y);
            ELSE
                DoFormMouse(w, mess, wParam, lParam);
            END;
            RETURN 0;
        END;
    |
    WM_LBUTTONUP:
        IF w <> NIL THEN
            ReleaseMouse(w);

            IF w^.formEditMode THEN
                IF w^.dropControl <> NIL THEN
                    info := w^.dropControl^;
                    w^.dropControl := NIL;
                    x := CAST(INTEGER16, LOWORD(lParam));
                    y := CAST(INTEGER16, HIWORD(lParam));
                    PixelToForm(w, x, y);
                    info.x := x - INT(info.width / 2);
                    info.y := y - INT(info.height / 2);
                    DoAddControl(w, info, NIL);

                    CheckControlContainment(w^.lastControl);

                    TerminateDispatchMessages(0);

                ELSIF w^.dragMode = Dragging THEN
                    DragRectDragEnd(w, NIL);
                END;
            ELSE
                DoFormMouse(w, mess, wParam, lParam);
            END;
            RETURN 0;
        END;
    |
    WM_RBUTTONUP:
        IF w <> NIL THEN
            ReleaseMouse(w);
            DoFormMouse(w, mess, wParam, lParam);
        END;
    |
    WM_MOUSEMOVE:
        IF w <> NIL THEN
            IF w^.formEditMode THEN
                x := CAST(INTEGER16, LOWORD(lParam));
                y := CAST(INTEGER16, HIWORD(lParam));

                IF (wParam BAND MK_LBUTTON) = MK_LBUTTON THEN
                    IF (x <> w^.dragX1) OR (y <> w^.dragY1) THEN
                        IF w^.dragMode = PreDrag THEN
                            DragRectDragBegin(w, NIL);
                        END;

                        UpdateDragRect(w, NIL, x - w^.dragX1, y - w^.dragY1);
                        w^.dragX1 := x;
                        w^.dragY1 := y;
                    END;
                END;
            ELSE
                DoFormMouse(w, mess, wParam, lParam);
            END;
            RETURN 0;
        END;
    |
    WM_CAPTURECHANGED:
        w^.mouseTrap := 0;
        IF (w <> NIL) AND w^.formEditMode AND (w^.dropControl <> NIL) THEN
            PostQuitMessage(0);
        END;
    |
    WM_SETCURSOR:
        IF (w <> NIL) AND w^.formEditMode AND (w^.dropControl <> NIL) THEN
            SetCursor(DropCursor);
            RETURN 1;
        END;
    |
    WM_SIZE:
        DoSize(w, wParam, lParam);
        RETURN 0;
    |
    WM_TIMER:
        DoTimer(w, wParam);
        RETURN 0;
    |
    WM_COLUMN_AUTOSIZE:
        DoColumnAutoSize(CAST(ColumnAutoSizePointer, wParam));
(**)
        IF IsVisible(w) THEN
            RepaintWindow(w);
            UpdateWindow(w);
        END;
(**)
    |
    WM_USER_USER:
        IF w <> NIL THEN
            DoUser(w, wParam, lParam);
        END;
        RETURN 0;
    ELSE
    END;

    RETURN DefDlgProc(hwnd, mess, wParam, lParam);

EXCEPT
    ReleaseMouseAll(w);
    DoExcept(w, hwnd, "FormClientWndProc");
    RETURN DefDlgProc(hwnd, mess, wParam, lParam);
END FormClientWndProc;


PROCEDURE ListViewWndProc (hwnd : HWND; mess : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT [EXPORT, SbOsSystem];
VAR
    w : Window;
BEGIN
    w := CAST (Window, GetWindowLongPtr (hwnd, GWL_USERDATA));
    IF (mess = WM_PAINT) & (w^.state # WindowStateMinimized) THEN
        PostMessage (hwnd, WM_HSCROLL, SB_ENDSCROLL, 0);
        (*
         * Under some circumstances Windows forgets to repaint scrollbars, see bug 3982 for exmaple.
         *
         * Sending WM_HSCROLL message solves the problem partially,
         * scroll bars are repainted but without arrows.
         *)
    END;
    RETURN CallWindowProc (w^.oldWndProc, hwnd, mess, wParam, lParam);
END ListViewWndProc;


PROCEDURE ListViewWndProc1 (hwnd : HWND; mess : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT [EXPORT, SbOsSystem];
VAR
    c : ControlInfoPointer;
BEGIN
    c := CAST (ControlInfoPointer, GetWindowLongPtr (hwnd, GWL_USERDATA));
    IF (mess = WM_PAINT) & c^.visible THEN
        PostMessage (hwnd, WM_HSCROLL, SB_ENDSCROLL, 0);
        (*
         * Under some circumstances Windows forgets to repaint scrollbars, see bug 3982 for exmaple.
         *
         * Sending WM_HSCROLL message solves the problem partially,
         * scroll bars are repainted but without arrows.
         *)
    END;
    RETURN CallWindowProc (c^.prevProc, hwnd, mess, wParam, lParam);
END ListViewWndProc1;


PROCEDURE ClientSubclassWndProc1(hwnd : HWND; mess : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT [EXPORT, SbOsSystem];
VAR
    w           : Window;
    msg         : MessageRec;
    oldProc     : WNDPROC;
BEGIN
    (* these windows store the window handle in the same location *)
    w := CAST(Window, GetWindowLongPtr(hwnd, WindowDataPos[TabClient]));

    CASE mess OF
    WM_CLOSE:
        IF w <> NIL THEN
            IF DoCloseRequest(w) THEN
                DestroyWindow(hwnd);
                RETURN 1;
            END;
            RETURN 0;
        END;
    |
    WM_DESTROY:
        oldProc := w^.oldWndProc;
        SetWindowLongPtr(hwnd, WindowDataPos[TabClient], CAST(LONG_PTR, NIL));
        SubclassWindow(hwnd, oldProc);

        CloseAllChildren(w, CM_DICTATE);

        msg.msg := WSM_CLOSE;
        msg.closeMode := CM_DICTATE;
        CallWndProc(w, msg);

        IF IsTabChild(w) THEN
            HideTabChild(w);
        ELSIF IsSplitterChild(w) THEN
            SetSplitterHandle(w, FALSE);
        END;
        DisposeWindow(w);

        RETURN CallWindowProc(oldProc, hwnd, mess, wParam, lParam);
    |
    WM_SIZE:
        IF w <> NIL THEN
            AdjustClientSize(w);
            DoSize(w, wParam, lParam);
        END;
    |
    WM_COLUMN_AUTOSIZE:
        DoColumnAutoSize(CAST(ColumnAutoSizePointer, wParam));
    |
    WM_LIST_SORT:
        IF w <> NIL THEN
            w^.sortPosted := FALSE;
            ListClientSort(w);
            RETURN 0;
        END;
    |
    WM_CHAR:
        IF (w <> NIL) AND
           ((w^.clientType = ListClient) OR (w^.clientType = TreeClient)) AND
           w^.returnKeys
        THEN
            IF DoChar(w, wParam, lParam) = USER_HANDLE THEN
                RETURN 0;
            END;
        END;
    |
    WM_NOTIFY:
        IF DoNotify(w, CAST(LPNMHDR, lParam)) = 1 THEN
            RETURN 1;
        END;
    |
    WM_USER_USER:
        IF w <> NIL THEN
            DoUser(w, wParam, lParam);
        END;
        RETURN 0;
    ELSE
    END;

    IF w <> NIL THEN
        RETURN CallWindowProc(w^.oldWndProc, hwnd, mess, wParam, lParam);
    ELSE
        RETURN DefWindowProc(hwnd, mess, wParam, lParam);
    END;

EXCEPT
    ReleaseMouseAll(w);
    DoExcept(w, hwnd, "ClientSubclassWndProc1");
    IF w <> NIL THEN
        RETURN CallWindowProc(w^.oldWndProc, hwnd, mess, wParam, lParam);
    ELSE
        RETURN DefWindowProc(hwnd, mess, wParam, lParam);
    END;
END ClientSubclassWndProc1;

PROCEDURE GetWindowState(w : Window);
(* top level windows only *)
BEGIN
    w^.state := WindowStateNormal;
    IF IsIconic(w^.wnd) THEN
        w^.state := WindowStateMinimized;
    ELSIF IsZoomed(w^.wnd) THEN
        w^.state := WindowStateMaximized;
    END;
END GetWindowState;

PROCEDURE ToplevelWndProc(hwnd : HWND; mess : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT [EXPORT, SbOsSystem];
VAR
    w           : Window;
    msg         : MessageRec;
    fmt         : ARRAY [0..0] OF INTEGER;
BEGIN
    w := CAST(Window, GetWindowLongPtr(hwnd, 0));

    CASE mess OF
    WM_CREATE:
        w := CAST(LPCREATESTRUCT, lParam)^.lpCreateParams;
        SetWindowLongPtr(hwnd, 0, CAST(LONG_PTR, w));

        w^.wnd := hwnd;
        GetWindowState(w);

        SetWindowIconOS(w);

        IF w^.createClient THEN
            IF CreateClient(w, hwnd, TRUE) THEN
                IF WA_STATUSLINE IN w^.attr THEN
                    fmt[0] := -1;
                    CreateStatusLine(w, fmt);
                END;
            ELSE
                DestroyWindow(hwnd);
            END;
        ELSE
            w^.clientWnd := hwnd;
        END;
(*MVN+*)
        IF (NOT w^.modalForm) AND (w^.type = ToplevelWindow) THEN
            CreateWindowTemp.wnd := w;
(*
            WIN32.SetEvent(CreateWindowTemp.GuiEvent);
*)
            msg.msg := WSM_CREATE;
            msg.createParam := w^.createParam;
            CallWndProc(w, msg);
            WIN32.SetEvent(CreateWindowTemp.GuiEvent);

            w^.createData := NIL;
        END;
(*MVN-*)
        RETURN 0;
    |
    WM_CLOSE:
        IF DoCloseRequest(w) THEN
            DestroyWindow(hwnd);
            RETURN 1;
        END;
        RETURN 0;
    |
    WM_QUERYENDSESSION:
        IF DoQueryEndSession(w) = DEFAULT_HANDLE THEN
            RETURN 1;
        END;
        RETURN 0;
    |
    WM_DESTROY:
        IF IsWindow(w) THEN
            (* windows does this for us
            CloseAllChildren(w, CM_DICTATE);
            *)

            (* draw gets its dictate close from the client window *)
            IF w^.clientType <> DrawClient THEN
                msg.msg := WSM_CLOSE;
                msg.closeMode := CM_DICTATE;
                CallWndProc(w, msg);
            END;

            DestroyWindow(w^.clientWnd);
            w^.clientWnd := NIL;

            SetWindowLongPtr(hwnd, 0, CAST(LONG_PTR, NIL));
            DisposeWindow(w);
        END;
        RETURN 0;
    |
    WM_ACTIVATEAPP:
        DoActivateApp(w, wParam, lParam);
        RETURN 0;
    |
    WM_SETFOCUS:
        IF w <> NIL THEN
            w^.delayFocus := TRUE;
            WINUSER.PostMessage(hwnd, WM_SETFOCUS_DELAY, wParam, lParam);
            RETURN 0;

            (* did not work in some situations
               the wonderfull world of windows quirks....
            IF w^.type <> MdiFrame THEN
                (* a hack to work around a windows bug *)
                w^.delayFocus := TRUE;
                WINUSER.PostMessage(w^.clientWnd, WM_SETFOCUS_DELAY, 0, 0);
                (*
                SetFocus(w^.clientWnd);
                *)
            ELSE
                WINUSER.PostMessage(hwnd, WM_SETFOCUS_DELAY, 0, 0);
            END;
            *)
        END;
        RETURN 0;
    |
    WM_SETFOCUS_DELAY:
        IF (w <> NIL) AND w^.delayFocus THEN
            w^.delayFocus := FALSE;

            IF w^.clientType = TabClient THEN
                IF w^.active <> NIL THEN
                    SetFocus(w^.active^.wnd);
                ELSE
                    DoSetFocus(w, wParam);
                    RETURN 0;
                END;
            ELSE
                SetFocus(w^.clientWnd);
            END;
        END;
        RETURN 0;
    |
    (*WM_ENABLE_WINDOW:
        Static_Enable(CAST(HWND, wParam), CAST(BOOL, lParam));
        RETURN 0;
    |*)
    WM_KILLFOCUS:
        IF w <> NIL THEN
            w^.delayFocus := FALSE;
            IF w^.clientType = TabClient THEN
                DoKillFocus(w);
            END;
        END;
        RETURN 0;
    |
    WM_ACTIVATE:
        DoActivate(w, LOWORD(wParam) <> WA_INACTIVE);
    |
    WM_INITMENU:
        IF w <> NIL THEN
            msg.msg := WSM_MENUSTART;
            IF w^.menuWindow = NIL THEN
                CallWndProc(w, msg);
            ELSE
                CallWndProc(w^.menuWindow, msg);
            END;
        END;
    |
    WM_MENUSELECT:
        RETURN DoMenuSelect(w, wParam, lParam);
    |
    WM_CONTEXTMENU:
        DoContextMenu(w, lParam);
        RETURN 0;
    |
    WM_COMMAND:
        DoCommand(w, wParam, lParam);
        RETURN 0;
    |
    WM_CHAR:
        (* yes this is dead code... but it has to be here, as is, for some reason.
           this is the source of the strange dialog box repaint quirk
           where the dialog font becomes bold on repaints when it is
           unconvered.
           this has only been observed on Win2k.
           maybe someday we should get to the bottom of this.
           this is just a stupid hack that seems to work.
        *)
        IF DoChar(w, wParam, lParam) = USER_HANDLE THEN
            RETURN 0;
        END;
        RETURN 0;
    |
    WM_NOTIFY:
        IF w <> NIL THEN
            RETURN DoNotify(w, CAST(LPNMHDR, lParam));
        END;
    |
    WM_QUERYDRAGICON:
        RETURN CAST(LRESULT, w^.bigIcon);
    |
    WM_GETICON:
        IF w <> NIL THEN
            IF wParam <> 0 THEN
                RETURN CAST(LRESULT, w^.bigIcon);
            ELSE
                IF w^.smallIcon <> NIL THEN
                    RETURN CAST(LRESULT, w^.bigIcon);
                ELSE
                    RETURN CAST(LRESULT, w^.bigIcon);
                END;
            END;
        END;
    |
    WM_SIZE:
        IF w <> NIL THEN
            AdjustClientSize(w);
            IF (wParam = SIZE_MAXIMIZED) OR
               (wParam = SIZE_MINIMIZED) OR
               ((w^.state = WindowStateMinimized) AND (wParam = SIZE_RESTORED))
            THEN
                DoSize(w, wParam, lParam);
            END;
        END;
    |
    WM_GETMINMAXINFO:
        DoMinMaxInfo(w, CAST(PMINMAXINFO, lParam));
    |
    WM_WINDOWPOSCHANGING:
        DoPosChanging(w, CAST(LPWINDOWPOS, lParam));
        (* Let it pass through to get a min max size proper *)
    |
    WM_MOVE:
        DoMove(w, lParam);
    |
    WM_CAPTURECHANGED:
        DoCaptureChanged(w);
    |
    WM_TIMER:
        DoTimer(w, wParam);
        RETURN 0;
    |
    WM_HELP:
        DoHelp(w, lParam);
        RETURN 1;
    |
    WM_USER_USER:
        IF w <> NIL THEN
            DoUser(w, wParam, lParam);
        END;
        RETURN 0;
    ELSE
    END;

    RETURN DefWindowProc(hwnd, mess, wParam, lParam);

EXCEPT
    ReleaseMouseAll(w);
    DoExcept(w, hwnd, "ToplevelWndProc");
    RETURN DefWindowProc(hwnd, mess, wParam, lParam);
END ToplevelWndProc;

PROCEDURE GetControlRect(ctrl : ControlInfoPointer; selected : BOOLEAN; VAR OUT rc : RECT);
VAR
    rc2         : RECT;
BEGIN
    IF selected THEN
        rc.left := ctrl^.x;
        rc.top := ctrl^.y;
        rc.right := rc.left + ctrl^.width;
        rc.bottom := rc.top + ctrl^.height;
        FormToPixel(ctrl^.w, rc.left, rc.top);
        FormToPixel(ctrl^.w, rc.right, rc.bottom);
    ELSE
        GetWindowRect(ctrl^.wnd, rc);
        IF ctrl^.spinWnd <> NIL THEN
            GetWindowRect(ctrl^.spinWnd, rc2);
            rc.right := rc2.right;
        END;

        MapWindowPoints(HWND_DESKTOP, ctrl^.w^.clientWnd, CAST(POINT, rc), 2);
    END;

END GetControlRect;

PROCEDURE GetDragRect(ctrl : ControlInfoPointer; VAR OUT rc : RECT);
BEGIN
    GetWindowRect(ctrl^.dragWnd, rc);
    MapWindowPoints(HWND_DESKTOP, ctrl^.w^.clientWnd, CAST(POINT, rc), 2);
END GetDragRect;

PROCEDURE PaintDragBox(ctrl : ControlInfoPointer);
VAR
    ps          : PAINTSTRUCT;
    rc          : RECT;
    oldBrush    : HBRUSH;
    oldBitmap   : HBITMAP;
    x2, y2      : INTEGER;
    xMid, yMid  : INTEGER;
BEGIN
    WINUSER.BeginPaint(ctrl^.dragWnd, ps);

    IF ctrl^.selected THEN
        (* clip the underlying control *)

        GetControlRect(ctrl, FALSE, rc);
        MapWindowPoints(ctrl^.w^.clientWnd, ctrl^.dragWnd, CAST(POINT, rc), 2);
        ExcludeClipRect(ps.hdc, rc.left, rc.top, rc.right, rc.bottom);

        GetClientRect(ctrl^.dragWnd, rc);

        (* paint the boundary pattern *)

        oldBrush := SelectBrush(ps.hdc, DragBoxPatternBrush);
        PatBlt(ps.hdc, rc.left, rc.top, rc.right-rc.left, rc.bottom-rc.top, PATCOPY);
        SelectBrush(ps.hdc, oldBrush);

        IF ctrl = ctrl^.w^.selectedControl THEN
            oldBitmap := SelectBitmap(DragDC, DragHandlePrimary);
        ELSE
            oldBitmap := SelectBitmap(DragDC, DragHandleSecondary);
        END;

        xMid := ((rc.right + 1) / 2) - (DragHandleSize / 2);
        yMid := ((rc.bottom + 1) / 2) - (DragHandleSize / 2);
        x2 := rc.right - DragHandleSize;
        y2 := rc.bottom - DragHandleSize;

        BitBlt(ps.hdc, 0, 0, DragHandleSize, DragHandleSize, DragDC, 0, 0, SRCCOPY);
        BitBlt(ps.hdc, xMid, 0, DragHandleSize, DragHandleSize, DragDC, 0, 0, SRCCOPY);
        BitBlt(ps.hdc, x2, 0, DragHandleSize, DragHandleSize, DragDC, 0, 0, SRCCOPY);
        BitBlt(ps.hdc, x2, yMid, DragHandleSize, DragHandleSize, DragDC, 0, 0, SRCCOPY);
        BitBlt(ps.hdc, x2, y2, DragHandleSize, DragHandleSize, DragDC, 0, 0, SRCCOPY);
        BitBlt(ps.hdc, xMid, y2, DragHandleSize, DragHandleSize, DragDC, 0, 0, SRCCOPY);
        BitBlt(ps.hdc, 0, y2, DragHandleSize, DragHandleSize, DragDC, 0, 0, SRCCOPY);
        BitBlt(ps.hdc, 0, yMid, DragHandleSize, DragHandleSize, DragDC, 0, 0, SRCCOPY);

        SelectBitmap(DragDC, oldBitmap);
    END;

    WINUSER.EndPaint(ctrl^.dragWnd, ps);
END PaintDragBox;

PROCEDURE DragBoxHitTest(ctrl : ControlInfoPointer; lParam : LPARAM) : LRESULT;
VAR
    x2, y2      : INTEGER;
    xMid, yMid  : INTEGER;
    hit         : HitTest;
    rc          : RECT;
    pt          : POINT;
    ptr         : ControlInfoPointer;
BEGIN
    pt.x := CAST(INTEGER16, LOWORD(lParam));
    pt.y := CAST(INTEGER16, HIWORD(lParam));
    MapWindowPoints(HWND_DESKTOP, ctrl^.dragWnd, pt, 1);

    GetClientRect(ctrl^.dragWnd, rc);

    xMid := ((rc.right + 1) / 2) - (DragHandleSize / 2);
    yMid := ((rc.bottom + 1) / 2) - (DragHandleSize / 2);
    x2 := rc.right - DragHandleSize;
    y2 := rc.bottom - DragHandleSize;

    hit := HtClient;

    IF (ctrl^.w^.selectionCount <= 1) AND
       (pt.x >= 0) AND (pt.x < rc.right) AND
       (pt.y >= 0) AND (pt.y < rc.bottom)
    THEN
        IF pt.y < DragHandleSize THEN
            IF pt.x < DragHandleSize THEN
                hit := HtTopLeft;
            ELSIF (pt.x >= xMid) AND (pt.x < (xMid+DragHandleSize)) THEN
                hit := HtTop;
            ELSIF pt.x >= x2 THEN
                hit := HtTopRight;
            END;
        ELSIF pt.y >= y2 THEN
            IF pt.x < DragHandleSize THEN
                hit := HtBottomLeft;
            ELSIF (pt.x >= xMid) AND (pt.x < (xMid+DragHandleSize)) THEN
                hit := HtBottom;
            ELSIF pt.x >= x2 THEN
                hit := HtBottomRight;
            END;
        ELSIF (pt.y >= yMid) AND (pt.y < yMid+DragHandleSize) THEN
            IF pt.x < DragHandleSize THEN
                hit := HtLeft;
            ELSIF pt.x >= x2 THEN
                hit := HtRight;
            END;
        END;
    END;

    ctrl^.hitTest := HtClient;
    IF ctrl^.selected THEN
        ctrl^.hitTest := hit;
    END;

    IF hit = HtClient THEN
        ptr := NIL;
        IF ctrl^.count > 0 THEN
            IF ctrl^.type = MultiPage THEN
                ptr := ctrl^.pageInfo^[ctrl^.currentPage].firstChild;
            ELSIF ctrl^.type = GroupBox THEN
                ptr := ctrl^.firstChild;
            END;
        END;
        IF ptr <> NIL THEN
            MapWindowPoints(ctrl^.dragWnd, ctrl^.w^.clientWnd, pt, 1);
            PixelToForm(ctrl^.w, pt.x, pt.y);
            WHILE ptr <> NIL DO
                IF (pt.x >= ptr^.x) AND
                   (pt.y >= ptr^.y) AND
                   (pt.x < ptr^.x+ptr^.width) AND
                   (pt.y < ptr^.y+ptr^.height)
                THEN
                    RETURN HTTRANSPARENT;
                END;

                ptr := ptr^.next;
            END;
        END;
    END;

    RETURN HTCLIENT;
END DragBoxHitTest;

PROCEDURE AddSelectedControl(w : Window; ctrl : ControlInfoPointer);
VAR
    rc          : RECT;
    ptr         : ControlInfoPointer;
BEGIN
    INC(w^.selectionCount);

    ctrl^.nextSel := w^.selectedControl;
    w^.selectedControl := ctrl;
    ctrl^.selected := TRUE;

    (* redraw the other selections, if any *)

    ptr := ctrl^.nextSel;
    WHILE ptr <> NIL DO
        InvalidateRect(ptr^.dragWnd, NIL_RECT, TRUE);
        ptr := ptr^.nextSel;
    END;

    (* now resize and redraw the drag box *)

    (*IF ctrl^.type <> MultiPage THEN
        WINUSER.BringWindowToTop(ctrl^.dragWnd);
    END;*)
    GetControlRect(ctrl, TRUE, rc);
    rc.right := rc.right - rc.left;
    rc.bottom := rc.bottom - rc.top;
    MoveWindow(ctrl^.dragWnd,
               rc.left - DragHandleSize, rc.top - DragHandleSize,
               rc.right + (2*DragHandleSize), rc.bottom + (2*DragHandleSize),
               TRUE);
    (* invalidate is necessary when not using WS_EX_TRANSPARENT for drag window*)
    InvalidateRect(ctrl^.dragWnd, NIL_RECT, TRUE);
    SetFocus(ctrl^.dragWnd);
END AddSelectedControl;

PROCEDURE UnselectControl(w : Window; ctrl : ControlInfoPointer);
VAR
    rc          : RECT;
    prev, ptr   : ControlInfoPointer;
BEGIN
    ptr := w^.selectedControl;
    prev := NIL;
    WHILE (ptr <> NIL) AND (ptr <> ctrl) DO
        prev := ptr;
        ptr := ptr^.nextSel;
    END;
    IF ptr <> NIL THEN
        DEC(w^.selectionCount);

        IF prev = NIL THEN
            w^.selectedControl := ctrl^.nextSel;
        ELSE
            prev^.nextSel := ctrl^.nextSel;
        END;
        ctrl^.selected := FALSE;

        ctrl^.nextSel := NIL;
        InvalidateRect(ctrl^.dragWnd, NIL_RECT, TRUE);
        GetControlRect(ctrl, FALSE, rc);
        rc.right := rc.right - rc.left;
        rc.bottom := rc.bottom - rc.top;
        MoveWindow(ctrl^.dragWnd, rc.left, rc.top, rc.right, rc.bottom, TRUE);

        IF (prev = NIL) AND (w^.selectedControl <> NIL) THEN
            (* the primary selection changed *)
            InvalidateRect(w^.selectedControl^.dragWnd, NIL_RECT, TRUE);
        END;
    END;
END UnselectControl;

PROCEDURE ClearSelectedControls(w : Window);
VAR
    ctrl, next  : ControlInfoPointer;
    rc          : RECT;
BEGIN
    ctrl := w^.selectedControl;
    WHILE ctrl <> NIL DO
        next := ctrl^.nextSel;

        ctrl^.selected := FALSE;
        ctrl^.nextSel := NIL;
        InvalidateRect(ctrl^.dragWnd, NIL_RECT, TRUE);
        GetControlRect(ctrl, FALSE, rc);
        rc.right := rc.right - rc.left;
        rc.bottom := rc.bottom - rc.top;
        MoveWindow(ctrl^.dragWnd, rc.left, rc.top, rc.right, rc.bottom, TRUE);

        ctrl := next;
    END;

    w^.selectedControl := NIL;
    w^.selectionCount := 0;
END ClearSelectedControls;

PROCEDURE SetSelectedControl(w : Window; ctrl : ControlInfoPointer);
VAR
    ptr, prev   : ControlInfoPointer;
BEGIN
    IF NOT ctrl^.selected THEN
        ClearSelectedControls(w);

        IF (ctrl^.type = RadioButton) AND (ctrl^.group <> 0) THEN
            ptr := GetControlFirstSibling(ctrl);
            WHILE ptr <> NIL DO
                IF (ptr <> ctrl) AND (ptr^.type = RadioButton) AND (ptr^.group = ctrl^.group) THEN
                    AddSelectedControl(w, ptr);
                END;
                ptr := ptr^.next;
            END;
        END;

        AddSelectedControl(w, ctrl);

    ELSIF w^.selectionCount > 1 THEN
        IF w^.selectedControl <> ctrl THEN
            (* move this control to the selection front *)

            prev := NIL;
            ptr := w^.selectedControl;
            WHILE (ptr <> NIL) AND (ptr <> ctrl) DO
                prev := ptr;
                ptr := ptr^.nextSel;
            END;
            IF ptr <> NIL THEN
                prev^.nextSel := ptr^.nextSel;
                ptr^.nextSel := w^.selectedControl;
                prev := w^.selectedControl;
                w^.selectedControl := ptr;
                IF ptr^.type <> MultiPage THEN
                    WINUSER.BringWindowToTop(ptr^.dragWnd);
                END;
                InvalidateRect(prev^.dragWnd, NIL_RECT, TRUE);
                InvalidateRect(ptr^.dragWnd, NIL_RECT, TRUE);
            END;
        END;
    END;
END SetSelectedControl;

PROCEDURE GetDragPoint(w : Window;
                       ctrl : ControlInfoPointer;
                       lParam : LPARAM;
                       VAR OUT x, y : INTEGER);
VAR
    pt  : POINT;
BEGIN
    pt.x := CAST(INTEGER16, LOWORD(lParam));
    pt.y := CAST(INTEGER16, HIWORD(lParam));
    MapWindowPoints(ctrl^.dragWnd, w^.clientWnd, pt, 1);
    x := pt.x;
    y := pt.y;
END GetDragPoint;

PROCEDURE CheckMultiPageDragBoxClick(ctrl : ControlInfoPointer; lParam : LPARAM);
VAR
    ht          : TCHITTESTINFO;
    index       : INTEGER;
BEGIN
    ht.pt.x := CAST(INTEGER16, LOWORD(lParam));
    ht.pt.y := CAST(INTEGER16, HIWORD(lParam));

    index := TabCtrl_HitTest(ctrl^.wnd, ht);
    IF (index >= 0) AND ((ht.flags BAND TCHT_ONITEM) <> 0) THEN
        MultiPageSetActivePage(ctrl, index);
    END;
END CheckMultiPageDragBoxClick;

PROCEDURE ShowMultiPageControls(ctrl : ControlInfoPointer; page : ADRCARD; show : BOOLEAN);

    PROCEDURE doPos(ptr : ControlInfoPointer; thisShow : BOOLEAN; VAR INOUT wndPos : HWND);
    CONST
        flags   : ARRAY BOOLEAN OF CARDINAL =
        {
         SWP_NOMOVE BOR SWP_NOSIZE BOR SWP_HIDEWINDOW BOR SWP_NOZORDER,
         SWP_NOMOVE BOR SWP_NOSIZE BOR SWP_SHOWWINDOW BOR SWP_NOACTIVATE
        };
    BEGIN
        ptr^.pageVisible := thisShow;

        WINUSER.SetWindowPos(ptr^.wnd, wndPos, 0, 0, 0, 0, flags[thisShow]);
        wndPos := ptr^.wnd;
        IF ptr^.spinWnd <> NIL THEN
            WINUSER.SetWindowPos(ptr^.spinWnd, ptr^.wnd, 0, 0, 0, 0, flags[thisShow]);
            wndPos := ptr^.spinWnd;
        END;
        IF ptr^.dragWnd <> NIL THEN
            WINUSER.SetWindowPos(ptr^.dragWnd, HWND_TOP, 0, 0, 0, 0, flags[thisShow]);
        END;
        (*IF ptr^.type = ListBox THEN*)
            InvalidateRect(ptr^.wnd, NIL_RECT, TRUE);
            (*IF IsWindowVisible(ptr^.wnd) THEN
                WINUSER.UpdateWindow(ptr^.wnd);
            END;*)
        (*END;*)
    END doPos;

    PROCEDURE showList(ptr : ControlInfoPointer; thisShow : BOOLEAN; wndPos : HWND);
    VAR
        bShow : BOOLEAN;
    BEGIN
        WHILE ptr <> NIL DO
            bShow := thisShow AND (ptr^.visible OR edit);
            doPos(ptr, bShow, wndPos);

            IF ptr^.type = GroupBox THEN
                showList(ptr^.firstChild, bShow, wndPos);
            ELSIF ptr^.type = MultiPage THEN
                showList(ptr^.pageInfo^[ptr^.currentPage].firstChild, bShow, wndPos);
            END;

            ptr := ptr^.next;
        END;
    END showList;

VAR
    ptr         : ControlInfoPointer;
    thisShow    : BOOLEAN;
    edit        : BOOLEAN;
    wndPos      : HWND;
BEGIN
    IF ctrl^.pageInfo^[page].numControls > 0 THEN
        wndPos := ctrl^.wnd;
        edit := ctrl^.w^.formEditMode OR ctrl^.w^.formPreEditMode;
        ptr := ctrl^.pageInfo^[page].firstChild;
        WHILE ptr <> NIL DO
            thisShow := show AND (ptr^.visible OR edit);

            doPos(ptr, thisShow, wndPos);

            IF ptr^.type = GroupBox THEN
                showList(ptr^.firstChild, thisShow, wndPos);
            ELSIF ptr^.type = MultiPage THEN
                showList(ptr^.pageInfo^[ptr^.currentPage].firstChild, thisShow, wndPos);
            END;

            ptr := ptr^.next;
        END;
    END;
END ShowMultiPageControls;

PROCEDURE MultiPageSetActivePage(ctrl : ControlInfoPointer; page : ADRCARD);
VAR
    i   : ADRCARD;
BEGIN
    <*/PUSH/NOWARN:U*>
    IF VAL(ADRCARD, ctrl^.currentPage) <> page THEN
    <*/POP*>
        TabCtrl_SetCurSel(ctrl^.wnd, page);
        ctrl^.currentPage := page;
    END;

    IF ctrl^.count <> 0 THEN
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
        <*/POP*>
            IF ctrl^.pageInfo^[i].numControls > 0 THEN
                ShowMultiPageControls(ctrl, i, i = page);
            END;
        END;
    END;
END MultiPageSetActivePage;

PROCEDURE CallPropProc(ctrl : ControlInfoPointer);
CONST
    labelMask   = SS_LEFT BOR SS_CENTER BOR SS_RIGHT;
    labelSet    : ARRAY TextAlignment OF CARDINAL = {SS_LEFT, SS_CENTER, SS_RIGHT};
    editMask    = ES_LEFT BOR ES_CENTER BOR ES_RIGHT;
    editSet     : ARRAY TextAlignment OF CARDINAL = {ES_LEFT, ES_CENTER, ES_RIGHT};
    editHSset   : ARRAY BOOLEAN OF CARDINAL = {0, WS_HSCROLL BOR ES_AUTOHSCROLL};
    editHSmask  = WS_HSCROLL BOR ES_AUTOHSCROLL;
    editVSset   : ARRAY BOOLEAN OF CARDINAL = {0, WS_VSCROLL BOR ES_AUTOVSCROLL};
    editVSmask  = WS_VSCROLL BOR ES_AUTOVSCROLL;
    editMLset   : ARRAY BOOLEAN OF CARDINAL = {0, ES_MULTILINE BOR ES_WANTRETURN};
    editMLmask  = ES_MULTILINE BOR ES_WANTRETURN;
    editPASSset : ARRAY BOOLEAN OF CARDINAL = {0, ES_PASSWORD};
    editPASSmask= ES_PASSWORD;
    listSelSet  : ARRAY BOOLEAN OF CARDINAL = {LVS_SINGLESEL, 0};
    listSelMask = LVS_SINGLESEL;
VAR
    info        : ControlProperties;
    w           : Window;
    pi          : ControlPageInfoArray;
    newColumns  : ColumnInfoPointer;
    text        : ARRAY [0..255] OF CHAR;
    tcItem      : TC_ITEM;
    top         : ADRCARD;
    i           : ADRCARD;

    PROCEDURE equal(strPtr : StringData; str : ARRAY OF CHAR) : BOOLEAN;
    VAR
        l1, l2  : ADRCARD;
    BEGIN
        l1 := StringDataLength(strPtr);
        l2 := LENGTH(str);
        IF l1 = l2 THEN
            l1 := 0;
            WHILE (l1 < l2) AND (strPtr^[l1] = str[l1]) DO
                INC(l1);
            END;
        END;
        RETURN l1 = l2;
    END equal;

    PROCEDURE checkCaption(ctrl : ControlInfoPointer; VAR INOUT strPtr : StringData; VAR INOUT strId : INTEGER);
    BEGIN
        IF strId = -1 THEN
            IF NOT equal(strPtr, ctrl^.caption) THEN
                ctrl^.w^.formChanged := TRUE;
                AssignStringData(strPtr, ctrl^.caption);
                ControlSetText(ctrl^.w, ctrl^.idNum, ctrl^.caption);
            END;
        ELSIF strId <> ctrl^.captionId THEN
            ctrl^.w^.formChanged := TRUE;
            ctrl^.captionId := strId;
            LoadString(strId, ctrl^.caption);
            ctrl^.caption[HIGH(ctrl^.caption)] := '';
            ControlSetText(ctrl^.w, ctrl^.idNum, ctrl^.caption);
        END;
    END checkCaption;

    PROCEDURE checkLabel(w : Window; VAR INOUT current, new : StringData);
    BEGIN
        IF current <> new THEN
            w^.formChanged := TRUE;
            DisposeStringData(current);
            current := DuplicateStringData(new);
        END;
    END checkLabel;

    PROCEDURE checkBool(w : Window; VAR INOUT current, new : BOOLEAN);
    BEGIN
        IF current <> new THEN
            w^.formChanged := TRUE;
            current := new;
        END;
    END checkBool;

    PROCEDURE checkNum(w : Window; VAR INOUT current, new : INTEGER);
    BEGIN
        IF current <> new THEN
            w^.formChanged := TRUE;
            current := new;
        END;
    END checkNum;

    PROCEDURE checkAlign(w : Window; VAR INOUT current, new : TextAlignment);
    BEGIN
        IF current <> new THEN
            w^.formChanged := TRUE;
            current := new;
        END;
    END checkAlign;

    PROCEDURE setStyle(ctrl : ControlInfoPointer; mask, set : CARDINAL);
    VAR
        style   : CARDINAL;
    BEGIN
        style := GetWindowLong(ctrl^.wnd, GWL_STYLE);
        style := style BAND (BNOT mask);
        style := style BOR set;
        SetWindowLong(ctrl^.wnd, GWL_STYLE, style);
    END setStyle;

BEGIN
    w := ctrl^.w;

    GetControlProps(ctrl, info);

    IF NOT w^.propProc(w, info) THEN
        DisposeControlProps(info);
        RETURN;
    END;

    (* now check for changes *)

    IF info.idNum <> ctrl^.idNum THEN
        w^.formChanged := TRUE;
        ctrl^.idNum := info.idNum;
        SetWindowLong(ctrl^.wnd, GWL_ID, info.idNum);
    END;

    IF info.tipTextId = -1 THEN
        IF NOT equal(info.tipTextPtr, ctrl^.tipText) THEN
            w^.formChanged := TRUE;
            AssignStringData(info.tipTextPtr, ctrl^.tipText);
        END;
    ELSIF info.tipTextId <> ctrl^.tipTextId THEN
        w^.formChanged := TRUE;
        ctrl^.tipTextId := info.tipTextId;
        LoadString(info.tipTextId, ctrl^.tipText);
    END;
    SetupControlTip(ctrl);

    checkBool(w, ctrl^.visible, info.visible);
    checkBool(w, ctrl^.enabled, info.enabled);

    CASE ctrl^.type OF
    CheckBox:
        checkCaption(ctrl, info.chk_textPtr, info.chk_textId);
    |
    RadioButton:
        checkCaption(ctrl, info.rb_textPtr, info.rb_textId);
        IF ctrl^.group <> info.rb_group THEN
            w^.formChanged := TRUE;
            ctrl^.group := info.rb_group;
            SetRadioGrouping(ctrl^.w);
        END;
    |
    PushButton:
        checkCaption(ctrl, info.b_textPtr, info.b_textId);
        IF ctrl^.default <> info.b_default THEN
            w^.formChanged := TRUE;
            IF info.b_default THEN
                DoControlSetDefaultButton(w, ctrl);
            ELSE
                ctrl^.default := FALSE;
            END;
        END;
    |
    ToggleButton:
        checkCaption(ctrl, info.b_textPtr, info.b_textId);
    |
    SpinButton:
        checkNum(w, ctrl^.spinLow, info.sb_low);
        checkNum(w, ctrl^.spinHigh, info.sb_high);
        UpDown_SetRange32(ctrl^.spinWnd, ctrl^.spinLow, ctrl^.spinHigh);
    |
    ListBox:
        IF ctrl^.count <> info.lb_numColumns THEN
            w^.formChanged := TRUE;
        END;

        <*/PUSH/NOWARN:U*>
        FOR i := VAL(ADRCARD, ctrl^.count)-1 TO 0 BY -1 DO
        <*/POP*>
            ListView_DeleteColumn(ctrl^.wnd, i);
        END;

        NEW(newColumns, info.lb_numColumns-1);
        top := ctrl^.count;
        <*/PUSH/NOWARN:U*>
        IF top < VAL(ADRCARD, info.lb_numColumns) THEN
        <*/POP*>
            top := info.lb_numColumns;
        END;
        FOR i := top-1 TO 0 BY -1 DO
            <*/PUSH/NOWARN:U*>
            IF i < VAL(ADRCARD, info.lb_numColumns) THEN
                IF (i < VAL(ADRCARD, ctrl^.count)) AND (ctrl^.columnInfo <> NIL) THEN
            <*/POP*>
                    newColumns^[i] := ctrl^.columnInfo^[i];
                    checkLabel(w, newColumns^[i].header, info.lb_columnInfo^[i].header);
                    checkAlign(w, newColumns^[i].align, info.lb_columnInfo^[i].align);
                    checkNum(w, newColumns^[i].width, info.lb_columnInfo^[i].width);
                ELSE
                    w^.formChanged := TRUE;
                    newColumns^[i].header := DuplicateStringData(info.lb_columnInfo^[i].header);
                    newColumns^[i].align := info.lb_columnInfo^[i].align;
                    newColumns^[i].width := info.lb_columnInfo^[i].width;
                    newColumns^[i].content := ColumnText;
                    newColumns^[i].sortable := FALSE;
                END;
            ELSE
                IF ctrl^.columnInfo <> NIL THEN
                    DisposeStringData(ctrl^.columnInfo^[i].header);
                END;
            END;
        END;
        IF ctrl^.columnInfo <> NIL THEN
            DISPOSE(ctrl^.columnInfo);
        END;
        ctrl^.columnInfo := newColumns;
        ctrl^.count := info.lb_numColumns;

        checkBool(w, ctrl^.multiSelect, info.lb_multiSelect);
        setStyle(ctrl, listSelMask, listSelSet[ctrl^.multiSelect]);

        CreateListViewColumns(ctrl^.w, ctrl^.wnd, ctrl^.count, ctrl^.columnInfo, TRUE);
    |
    DropDownList:
    |
    ComboBox:
        ctrl^.editLimit := info.cb_editLimit;
        IF ctrl^.editLimit <> 0 THEN
            ComboBox_LimitText(ctrl^.wnd, ctrl^.editLimit);
        ELSE
            ComboBox_LimitText(ctrl^.wnd, 32767);
        END;
    |
    TextLabel:
        checkAlign(w, ctrl^.textAlign, info.tl_align);
        checkCaption(ctrl, info.tl_textPtr, info.tl_textId);
        setStyle(ctrl, labelMask, labelSet[ctrl^.textAlign]);
    |
    TextEdit:
        checkAlign(w, ctrl^.textAlign, info.te_align);
        checkBool(w, ctrl^.password, info.te_password);
        checkBool(w, ctrl^.multiLine, info.te_multiLine);
        checkBool(w, ctrl^.horizScroll, info.te_horizScroll);
        checkBool(w, ctrl^.vertScroll, info.te_vertScroll);
        setStyle(ctrl, editMask, editSet[ctrl^.textAlign]);
        setStyle(ctrl, editMLmask, editMLset[ctrl^.multiLine]);
        setStyle(ctrl, editHSmask, editHSset[ctrl^.horizScroll]);
        setStyle(ctrl, editVSmask, editVSset[ctrl^.vertScroll]);
        setStyle(ctrl, editPASSmask, editPASSset[ctrl^.password]);

        ctrl^.editLimit := info.te_limit;
        IF ctrl^.editLimit <> 0 THEN
            Edit_LimitText(ctrl^.wnd, ctrl^.editLimit);
        ELSE
            Edit_LimitText(ctrl^.wnd, 32767);
        END;
    |
    GroupBox:
        checkCaption(ctrl, info.grp_textPtr, info.grp_textId);
    |
    MultiPage:
        IF ctrl^.count > 0 THEN
            pi := info.mp_pageInfo;
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
            <*/POP*>
                checkLabel(w, ctrl^.pageInfo^[i].label, pi^[i].label);
                AssignStringData(pi^[i].label, text);
                tcItem.mask := TCIF_TEXT;
                tcItem.pszText := ADR(text);
                TabCtrl_SetItem(ctrl^.wnd, i, tcItem);
            END;
        END;
        IF ctrl^.currentPage < ctrl^.count THEN
            ShowMultiPageControls (ctrl, ctrl^.currentPage, TRUE);
        END;
    END;

    InvalidateRect(ctrl^.wnd, NIL_RECT, TRUE);

    DisposeControlProps(info);
END CallPropProc;

PROCEDURE EnableFormMenuItems(w : Window; menu : HMENU);
CONST
    attr        : ARRAY BOOLEAN OF CARDINAL =
        {MF_BYCOMMAND BOR MF_GRAYED, MF_BYCOMMAND BOR MF_ENABLED};
VAR
    id          : CARDINAL;
    allRadio    : BOOLEAN;
    sameGroup   : BOOLEAN;
    multiple    : BOOLEAN;
    haveSel     : BOOLEAN;
    group       : CARDINAL;
    ctrl        : ControlInfoPointer;
    sel         : ControlInfoPointer;
    on          : BOOLEAN;
BEGIN
    allRadio := FALSE;
    sameGroup := FALSE;
    multiple := FALSE;
    haveSel := FALSE;
    group := 0;
    sel := w^.selectedControl;
    IF sel <> NIL THEN
        haveSel := TRUE;
        group := sel^.group;
    END;
    IF w^.selectionCount > 1 THEN
        multiple := TRUE;
    END;
    allRadio := TRUE;
    sameGroup := TRUE;
    ctrl := sel;
    WHILE ctrl <> NIL DO
        allRadio := allRadio AND (ctrl^.type = RadioButton);
        sameGroup := sameGroup AND (ctrl^.group = group);
        ctrl := ctrl^.nextSel;
    END;

    EnableMenuItem(menu, CtrlMenuProperties, attr[haveSel]);
    EnableMenuItem(menu, CtrlMenuDelete, attr[haveSel]);

    EnableMenuItem(menu, CtrlMenuFormTitle, attr[w^.type = ToplevelWindow]);

    FOR id := CtrlMenuAlignLeft TO CtrlMenuAlignBottom DO
        EnableMenuItem(menu, id, attr[multiple]);
    END;
    FOR id := CtrlMenuSizeWidth TO CtrlMenuSizeBoth DO
        EnableMenuItem(menu, id, attr[multiple]);
    END;

    on := haveSel AND (NOT multiple);
    EnableMenuItem(menu, CtrlMenuOrderMoveUp, attr[on AND (sel^.prev <> NIL)]);
    EnableMenuItem(menu, CtrlMenuOrderMoveDown, attr[on AND (sel^.next <> NIL)]);
    EnableMenuItem(menu, CtrlMenuOrderMoveToTop, attr[on AND (sel^.prev <> NIL)]);
    EnableMenuItem(menu, CtrlMenuOrderMoveToBottom, attr[on AND (sel^.next <> NIL)]);

    (* radio button menu *)

    on := multiple AND allRadio AND ((NOT sameGroup) OR (group = 0));
    EnableMenuItem(menu, CtrlMenuGroup, attr[on]);
    on := multiple AND sameGroup AND allRadio AND (group <> 0);
    EnableMenuItem(menu, CtrlMenuUngroup, attr[on]);

    (* multi page menu *)

    on := (sel <> NIL) AND (sel^.type = MultiPage);
    FOR id := CtrlMenuMultiPageAdd TO CtrlMenuMultiPageMoveRight DO
        EnableMenuItem(menu, id, attr[on]);
    END;
END EnableFormMenuItems;

PROCEDURE FormEditorPopupMenu(w : Window; x, y : CARDINAL);
VAR
    popup       : HMENU;
BEGIN
    IF (w^.formMenu = NIL) OR (NOT w^.formUsePopupMenu) THEN
        RETURN;
    END;

    popup := WINUSER.GetSubMenu(w^.formMenu, 0);
    IF popup = NIL THEN
        RETURN;
    END;

    EnableFormMenuItems(w, popup);

    TrackPopupMenu(popup,
                   TPM_LEFTALIGN BOR TPM_RIGHTBUTTON,
                   x, y,
                   0,
                   w^.clientWnd,
                   NIL_RECT);
END FormEditorPopupMenu;

PROCEDURE DoDragBoxKey(ctrl : ControlInfoPointer; wParam : WPARAM);
VAR
    newSel      : ControlInfoPointer;
    doChild     : BOOLEAN;
BEGIN
    CASE wParam OF
    VK_TAB:
        (* cycle the active control *)

        IF GetKeyState(VK_SHIFT) >= 0 THEN(* plain tab *)
            REPEAT
                IF (ctrl^.type = MultiPage) AND
                   (ctrl^.count > 0) AND
                   (ctrl^.pageInfo^[ctrl^.currentPage].firstChild <> NIL)
                THEN
                    ctrl := ctrl^.pageInfo^[ctrl^.currentPage].firstChild;
                ELSIF (ctrl^.type = GroupBox) AND (ctrl^.count > 0) AND (ctrl^.firstChild <> NIL) THEN
                    ctrl := ctrl^.firstChild;
                ELSIF ctrl^.next <> NIL THEN
                    ctrl := ctrl^.next;
                ELSE
                    IF (ctrl^.parent <> NIL) AND (ctrl^.parent^.next <> NIL) THEN
                        ctrl := ctrl^.parent^.next;
                    ELSE
                        ctrl := ctrl^.w^.firstControl;
                    END;
                END;
            UNTIL ctrl^.pageVisible;

        ELSE(* shift tab *)
            REPEAT
                doChild := TRUE;
                IF ctrl^.prev <> NIL THEN
                    ctrl := ctrl^.prev;
                ELSE
                    IF ctrl^.parent <> NIL THEN
                        ctrl := ctrl^.parent;
                        doChild := FALSE;
                    ELSE
                        ctrl := ctrl^.w^.lastControl;
                    END;
                END;
                IF doChild THEN
                    IF (ctrl^.type = MultiPage) AND
                       (ctrl^.count > 0) AND
                       (ctrl^.pageInfo^[ctrl^.currentPage].lastChild <> NIL)
                    THEN
                        ctrl := ctrl^.pageInfo^[ctrl^.currentPage].lastChild;
                    ELSIF (ctrl^.type = GroupBox) AND (ctrl^.count > 0) AND (ctrl^.lastChild <> NIL) THEN
                        ctrl := ctrl^.lastChild;
                    END;
                END;
            UNTIL ctrl^.pageVisible;
        END;

        SetSelectedControl(ctrl^.w, ctrl);
    |
    VK_DELETE:
        newSel := ctrl^.next;
        IF newSel = NIL THEN
            newSel := ctrl^.prev;
            IF newSel = NIL THEN
                newSel := ctrl^.parent;
            END;
        END;
        DestroyControl(ctrl);
        ctrl^.w^.formChanged := TRUE;
        IF newSel <> NIL THEN
            SetSelectedControl(ctrl^.w, newSel);
        END;
    ELSE
    END;
END DoDragBoxKey;

PROCEDURE DragBoxWndProc(hwnd : HWND; mess : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT [EXPORT, SbOsSystem];
VAR
    ctrl        : ControlInfoPointer;
    w           : Window;
    x, y        : INTEGER;
    dx, dy      : INTEGER;
    pt          : POINT;
BEGIN
    ctrl := CAST(ControlInfoPointer, GetWindowLongPtr(hwnd, 0));

    CASE mess OF
    WM_CREATE:
        ctrl := CAST(LPCREATESTRUCT, lParam)^.lpCreateParams;
        SetWindowLongPtr(hwnd, 0, CAST(LONG_PTR, ctrl));
        RETURN 0;
    |
    WM_CLOSE:
        RETURN 1;
    |
    WM_DESTROY:
        IF ctrl <> NIL THEN
            ctrl^.dragWnd := NIL;
        END;
    |
    WM_PAINT:
        IF ctrl <> NIL THEN
            PaintDragBox(ctrl);
            RETURN 0;
        END;
    |
    WM_ERASEBKGND:
        RETURN 1;
    |
    WM_NCHITTEST:
        IF ctrl <> NIL THEN
            RETURN DragBoxHitTest(ctrl, lParam);
        END;
    |
    WM_LBUTTONDOWN:
        IF ctrl <> NIL THEN
            SetCapture(hwnd);

            w := ctrl^.w;
            w^.dragMode := PreDrag;
            WINUSER.SetTimer(hwnd, DragTimer, DragTimerTimeout, CAST(TIMERPROC, NULL));

            GetDragPoint(w, ctrl, lParam, w^.dragX1, w^.dragY1);

            IF (MK_SHIFT BAND wParam) = MK_SHIFT THEN
                IF NOT ctrl^.selected THEN
                    AddSelectedControl(w, ctrl);
                ELSIF w^.selectionCount > 1 THEN
                    UnselectControl(w, ctrl);
                END;
            ELSE
                IF ctrl^.type = MultiPage THEN
                    CheckMultiPageDragBoxClick(ctrl, lParam);
                END;
                SetSelectedControl(w, ctrl);
            END;

        END;
        RETURN 0;
    |
    WM_LBUTTONDBLCLK:
        IF ctrl <> NIL THEN
            SetCapture(hwnd);
            CallPropProc(ctrl);
        END;
    |
    WM_LBUTTONUP:
        IF ctrl <> NIL THEN
            ReleaseCapture;

            w := ctrl^.w;
            IF ctrl^.selected AND (w^.dragMode = Dragging) THEN
                DragRectDragEnd(w, ctrl);
            END;
            w^.dragMode := NoDrag;
        END;
        RETURN 0;
    |
    WM_RBUTTONDOWN:
        IF ctrl <> NIL THEN
            pt.y := CAST(INTEGER16, HIWORD(lParam));
            pt.x := CAST(INTEGER16, LOWORD(lParam));
            WINUSER.ClientToScreen(hwnd, pt);
            FormEditorPopupMenu(ctrl^.w, pt.x, pt.y);
        END;
        RETURN 0;
    |
    WM_RBUTTONUP:
        IF ctrl <> NIL THEN
        END;
        RETURN 0;
    |
    WM_MOUSEACTIVATE:
        RETURN MA_NOACTIVATE;(* control this ourselves *)
    |
    WM_MOUSEMOVE:
        IF ctrl <> NIL THEN
            w := ctrl^.w;
            GetDragPoint(w, ctrl, lParam, x, y);
            dx := x - w^.dragX1;
            dy := y - w^.dragY1;

            IF (wParam BAND MK_LBUTTON) = MK_LBUTTON THEN
                IF w^.dragMode = PreDrag THEN
                    IF (ABS(dx) >= DragCX) OR (ABS(dy) >= DragCY) THEN
                        DragRectDragBegin(w, ctrl);
                        UpdateDragRect(w, ctrl, dx, dy);
                        w^.dragX1 := x;
                        w^.dragY1 := y;
                    END;
                ELSIF w^.dragMode = Dragging THEN
                    UpdateDragRect(w, ctrl, dx, dy);
                    w^.dragX1 := x;
                    w^.dragY1 := y;
                END;
            END;
        END;
        RETURN 0;
    |
    WM_TIMER:
        IF ctrl <> NIL THEN
            w := ctrl^.w;
            IF wParam = DragTimer THEN
                WINUSER.KillTimer(hwnd, DragTimer);

                IF w^.dragMode = PreDrag THEN
                    pt.x := w^.dragX1;
                    pt.y := w^.dragY1;
                    WINUSER.ClientToScreen(w^.clientWnd, pt);
                    WINUSER.SetCursorPos(pt.x, pt.y);
                    DragRectDragBegin(w, ctrl);
                END;
            END;
            RETURN 0;
        END;
    |
    WM_KEYDOWN:
        DoDragBoxKey(ctrl, wParam);
        RETURN 0;
    |
    WM_SETCURSOR:
        IF ctrl <> NIL THEN
            IF ctrl^.selected AND (ctrl^.w^.selectionCount = 1) AND (LOWORD(lParam) = HTCLIENT) THEN
                SetCursor(HitTestCursors[ctrl^.hitTest]);
            ELSE
                SetCursor(HitTestCursors[HtClient]);
            END;
            RETURN 1;
        END;
    ELSE
    END;

    RETURN DefWindowProc(hwnd, mess, wParam, lParam);

EXCEPT
    RETURN DefWindowProc(hwnd, mess, wParam, lParam);
END DragBoxWndProc;

PROCEDURE RedirectMessage(w : Window; msg : MessageRec);
BEGIN
    IF IsWindow(w) THEN
        CallWndProc(w, msg);
    END;
END RedirectMessage;

PROCEDURE SendUserMessage(w : Window; userId : CARDINAL; userData : ADDRESS);
BEGIN
    IF IsWindow(w) THEN
        WINUSER.SendMessage(w^.wnd, WM_USER_USER, userId, CAST(LPARAM, userData));
    END;
END SendUserMessage;

PROCEDURE PostUserMessage(w : Window; userId : CARDINAL; userData : ADDRESS);
BEGIN
    IF IsWindow(w) THEN
        WINUSER.PostMessage(w^.wnd, WM_USER_USER, userId, CAST(LPARAM, userData));
    END;
END PostUserMessage;

PROCEDURE IsUserMessageWaiting(w : Window) : BOOLEAN;
VAR
    msg : MSG;
    wnd : HWND;
BEGIN
    IF w = NIL THEN
        wnd := NIL;
    ELSE
        wnd := w^.wnd;
    END;
    IF PeekMessage(msg, wnd, WM_USER_USER, WM_USER_USER, PM_NOREMOVE) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END IsUserMessageWaiting;

PROCEDURE ClientToScreen(w : Window; VAR INOUT x, y : COORDINATE);
VAR
    winPt       : POINT;
BEGIN
    winPt.x := x;
    winPt.y := y;
    WINUSER.ClientToScreen(w^.wnd, winPt);
    x := winPt.x;
    y := winPt.y;
END ClientToScreen;

PROCEDURE ScreenToClient(w : Window; VAR INOUT x, y : COORDINATE);
VAR
    winPt       : POINT;
BEGIN
    winPt.x := x;
    winPt.y := y;
    WINUSER.ScreenToClient(w^.wnd, winPt);
    x := winPt.x;
    y := winPt.y;
END ScreenToClient;

PROCEDURE InitWindow(w : Window);
BEGIN
    w^.validate := MagicNumber;
    w^.type := ToplevelWindow;
    w^.clientType := DrawClient;
    w^.parent := NIL;
    w^.firstChild := NIL;
    w^.lastChild := NIL;
    w^.active := NIL;
    w^.nextActive := NIL;
    w^.nextSibling := NIL;
    w^.menuWindow := NIL;
    w^.toolbar := NIL;
    w^.statusbar := NIL;
    w^.menu := NIL;
    w^.popMenu := NIL;
    w^.activeMenu := NIL;
    w^.defaultDrawContext := NIL;
    w^.accel := NIL;
    w^.wnd := NIL;
    w^.clientWnd := NIL;
    w^.bigIcon := NIL;
    w^.smallIcon := NIL;
    w^.cursor := NIL;
    w^.busySave := NIL;
    w^.busy := 0;
    w^.createParam := NIL;
    w^.splitterChild[0] := NIL;
    w^.splitterChild[1] := NIL;
    w^.dc := NIL;
    w^.firstControl := NIL;
    w^.lastControl := NIL;
    w^.selectedControl := NIL;
    w^.selectionCount := 0;
    w^.numControlsMain := 0;
    w^.numControlsAll := 0;
    w^.dropControl := NIL;
    w^.dragDC := NIL;
    w^.dragMode := NoDrag;
    w^.formFont := NIL;
    w^.columnInfo := NIL;
    w^.numColumns := 0;
    w^.autoSizePosted := FALSE;
    w^.sortPosted := FALSE;
    w^.headerWnd := NIL;
    w^.sortColumn := -1;
    w^.sortDirection := SortAscending;
    w^.compareProc := NILPROC;
    w^.compareData := NIL;
    w^.imageList := NIL;
    w^.headerImageList := NIL;
    w^.hasCaret := FALSE;
    w^.caretCreated := FALSE;
    w^.caretType := CtFullBlock;
    w^.caretBmp := NIL;
    w^.hideCaret := 0;
    w^.firstMinMax := TRUE;
    w^.mouseTrap := 0;
    w^.hasFocus := FALSE;
    w^.disableScrollH := FALSE;
    w^.disableScrollV := FALSE;
    w^.calledHelp := FALSE;
    w^.autoErase := FALSE;
    w^.delayFocus := FALSE;
    w^.tabChildNumbers := FALSE;
    w^.createClient := TRUE;
    w^.suppressWndProc := FALSE;
    w^.formPreEditMode := FALSE;
    w^.formEditMode := FALSE;
    w^.splitTopBottom := TRUE;
    w^.modal := FALSE;
    w^.returnKeys := FALSE;
    w^.external := FALSE;
    w^.extRefCount := 0;
    w^.notifyCount := 0;
    w^.tabIndex := -1;
    w^.splitterView := 0;
    w^.x := 0;
    w^.y := 0;
    w^.width := 0;
    w^.height := 0;
    w^.state := WindowStateNormal;
    w^.vScrollPos := 0;
    w^.vScrollMin := 0;
    w^.vScrollMax := 100;
    w^.vScrollPage := 0;
    w^.hScrollPos := 0;
    w^.hScrollMin := 0;
    w^.hScrollMax := 100;
    w^.hScrollPage := 0;
    w^.hScale := 1.0;
    w^.vScale := 1.0;
    w^.caretPos.x := 0;
    w^.caretPos.y := 0;
    w^.caretSize.x := 0;
    w^.caretSize.y := 0;
    w^.offsetX := 0;
    w^.offsetY := 0;
    w^.vGrain := 1;
    w^.hGrain := 1;
    w^.maxW := 16384;
    w^.maxH := 16384;
    w^.minW := 1;
    w^.minH := 1;
    w^.updateColors := 0;
    w^.mouseWheelTotal := 0;
    w^.disable := 0;
    w^.tabPos := TabBottom;
    w^.formCharX := 1;
    w^.formCharY := 1;
    w^.formMenu := NIL;
    w^.formUsePopupMenu := TRUE;
    w^.formFont := NIL;
    w^.nextCtrlId := FirstChild_ID;
    w^.toolTipWnd := NIL;
    w^.modalForm := TRUE;

    w^.paintLock := 0;
    NEW(w^.drawable);
    InitDrawable(w^.drawable^);
END InitWindow;

PROCEDURE SetModelessDialog(wnd : ADDRESS; add : BOOLEAN) : BOOLEAN;
VAR
    info        : TlsInfo;
    i           : ADRCARD;
    j           : ADRCARD;
BEGIN
    info := GetThreadInfo();
    IF info = NIL THEN
        RETURN FALSE;
    END;

    IF add THEN
        IF info^.numModeless < MaxModeless THEN
            INC(info^.numModeless);
            info^.modeless[info^.numModeless] := wnd;
            RETURN TRUE;
        END;
    ELSE
        i := 1;
        <*/PUSH/NOWARN:U*>
        WHILE i <= VAL(ADRCARD, info^.numModeless) DO
        <*/POP*>
            IF info^.modeless[i] = CAST(HWND, wnd) THEN
                <*/PUSH/NOWARN:U*>
                FOR j := i+1 TO VAL(ADRCARD, info^.numModeless) DO
                <*/POP*>
                    info^.modeless[j-1] := info^.modeless[j];
                END;
                info^.modeless[MaxModeless] := NIL;
                DEC(info^.numModeless);
                RETURN TRUE;
            END;
            INC(i);
        END;
    END;
    RETURN FALSE;
END SetModelessDialog;

PROCEDURE AddIdleProcedure(idle : IdleProcType; data : ADDRESS) : CARDINAL;
VAR
    info        : TlsInfo;
BEGIN
    info := GetThreadInfo();
    IF info <> NIL THEN
        IF info^.numIdle < MaxIdleProcs THEN
            INC(info^.numIdle);
            info^.idleProcs[info^.numIdle] := idle;
            info^.idleData[info^.numIdle] := data;
            RETURN info^.numIdle;
        END;
    END;
    RETURN 0;
END AddIdleProcedure;

PROCEDURE RemoveIdleProcedure(id : ADRCARD);
VAR
    i           : ADRCARD;
    info        : TlsInfo;
BEGIN
    info := GetThreadInfo();
    IF info <> NIL THEN
        IF (id >= 1) AND (id < MaxIdleProcs) THEN
            <*/PUSH/NOWARN:U*>
            FOR i := id+1 TO VAL(ADRCARD, info^.numIdle) DO
            <*/POP*>
                info^.idleProcs[i-1] := info^.idleProcs[i];
                info^.idleData[i-1] := info^.idleData[i];
            END;
            info^.idleProcs[MaxIdleProcs] := NILPROC;
            info^.idleData[MaxIdleProcs] := NIL;
            DEC(info^.numIdle);
        END;
    END;
END RemoveIdleProcedure;

PROCEDURE DoDispatch(info : TlsInfo; VAR INOUT msg : MSG);
VAR
    dispatch    : BOOLEAN;
    i           : ADRCARD;
BEGIN
    dispatch := TRUE;

    IF info^.accel <> NIL THEN
        IF TranslateAccelerator(info^.accelWnd, info^.accel, msg) <> 0 THEN
            dispatch := FALSE;
        END;
    END;

    i := 1;
    <*/PUSH/NOWARN:U*>
    WHILE dispatch AND (i <= VAL(ADRCARD, info^.numModeless)) DO
    <*/POP*>
        IF WINUSER.IsWindow(info^.modeless[i]) AND
           IsDialogMessage(info^.modeless[i], msg)
        THEN
            dispatch := FALSE;
        END;
        INC(i);
    END;

    IF dispatch AND
       HtmlHelpInit AND
       (HtmlHelp(NIL,
                 NIL_STR,
                 HH_PRETRANSLATEMESSAGE,
                 CAST(DWORD_PTR, ADR(msg))) <> NIL)
    THEN
        dispatch := FALSE;
    END;

    IF dispatch THEN
        TranslateMessage(msg);
        DispatchMessage(msg);
    END;
END DoDispatch;

PROCEDURE DispatchMessages(startup : DispatchStartupProc; param : ADDRESS) : CARDINAL;
TYPE
    idleSet             = PACKEDSET OF [1..MaxIdleProcs];
VAR
    msg         : MSG;
    info        : TlsInfo;
    code        : CARDINAL;
    callIdle    : BOOLEAN;
    i           : ADRCARD;
    count       : CARDINAL;
    skipIdle    : idleSet;
    idle        : IdleProcType;

    PROCEDURE shutdown(info : TlsInfo);
    BEGIN
        DEC(info^.nestLevel);
        IF info^.nestLevel = 0 THEN
            DISPOSE(info);
            SetTlsData(TlsIndex, NIL);
        END;
    END shutdown;
BEGIN
    info := GetThreadInfo();
    IF info = NIL THEN
        RETURN MAX(CARDINAL);
    END;

    INC(info^.nestLevel);

    callIdle := info^.numIdle > 0;
    skipIdle := idleSet{};
    count := 0;

    startup(param);

    LOOP
        IF PeekMessage(msg, NIL, 0, 0, PM_NOREMOVE) THEN
            IF GetMessage(msg, NIL, 0, 0) THEN
                DoDispatch(info, msg);
                callIdle := info^.numIdle > 0;
                skipIdle := idleSet{};
                count := 0;
            ELSE
                EXIT;
            END;
        ELSE
            IF callIdle THEN
                callIdle := FALSE;
                INC(count);
                i := 1;
                LOOP
                    <*/PUSH/NOWARN:U*>
                    IF i <= VAL(ADRCARD, info^.numIdle) THEN
                    <*/POP*>
                        IF NOT (i IN skipIdle) THEN
                            idle := info^.idleProcs[i];
                            CASE idle(info^.idleData[i], count) OF
                            ContinueIdle:
                                callIdle := TRUE;
                            |
                            SuspendIdle:
                                INCL(skipIdle, i);
                            |
                            TerminateIdle:
                                RemoveIdleProcedure(i);
                                DEC(i);
                            END;
                        END;
                        INC(i);

                        IF PeekMessage(msg, NIL, 0, 0, PM_NOREMOVE) THEN
                            EXIT;
                        END;
                    ELSE
                        EXIT;
                    END;
                END;
            ELSE
                IF GetMessage(msg, NIL, 0, 0) THEN
                    DoDispatch(info, msg);
                    callIdle := info^.numIdle > 0;
                    skipIdle := idleSet{};
                    count := 0;
                ELSE
                    EXIT;
                END;
            END;
        END;
    END;

    code := msg.wParam;

    shutdown(info);

    RETURN code;

EXCEPT
    (* if there is a quit pending then eat it since we are propagating the exception.
       and this procedure is "returning".
    *)
    PeekMessage(msg, NIL, WM_QUIT, WM_QUIT, PM_REMOVE);

    shutdown(info);
END DispatchMessages;

PROCEDURE DummyStart(param : ADDRESS);
BEGIN
    UNREFERENCED_PARAMETER(param);
END DummyStart;

PROCEDURE DispatchMessagesTemp() : CARDINAL;
VAR
    info        : TlsInfo;
BEGIN
    info := GetTlsData(TlsIndex);
    IF info <> NIL THEN
        RETURN DispatchMessages(DummyStart, NIL);
    END;
    RETURN MAX(CARDINAL);
END DispatchMessagesTemp;

PROCEDURE CheckMessages() : BOOLEAN;
VAR
    msg         : MSG;
    info        : TlsInfo;
BEGIN
    info := GetThreadInfo();
    IF info = NIL THEN
        RETURN FALSE;
    END;

    WHILE PeekMessage(msg, NIL, 0, 0, PM_NOREMOVE) DO
        IF msg.message <> WM_QUIT THEN
            PeekMessage(msg, NIL, 0, 0, PM_REMOVE);
            DoDispatch(info, msg);
        ELSE
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END CheckMessages;

PROCEDURE TerminateDispatchMessages(code : CARDINAL);
BEGIN
    PostQuitMessage(code);
END TerminateDispatchMessages;

PROCEDURE RunWindowModalLoop(w : Window) : CARDINAL;
VAR
    parent      : HWND;
BEGIN
    IF IsWindow(w) AND (w^.type = ToplevelWindow) AND (NOT w^.modal) THEN
        parent := GetWindow(w^.wnd, GW_OWNER);
        IF parent <> NIL THEN
            EnableWindow(parent, FALSE);
        END;
        w^.modal := TRUE;
        RETURN DispatchMessagesTemp();
    END;
    RETURN MAX(CARDINAL);

EXCEPT
    IF IsWindow(w) AND w^.modal THEN
        w^.modal := FALSE;
        parent := GetWindow(w^.wnd, GW_OWNER);
        IF parent <> NIL THEN
            EnableWindow(parent, TRUE);
        END;
    END;
END RunWindowModalLoop;

PROCEDURE QuitWindowModalLoop(w : Window; code : CARDINAL);
VAR
    parent      : HWND;
BEGIN
    IF IsWindow(w) AND (w^.type = ToplevelWindow) AND w^.modal THEN
        parent := GetWindow(w^.wnd, GW_OWNER);
        IF parent <> NIL THEN
            EnableWindow(parent, TRUE);
            WINUSER.SetForegroundWindow(parent);
        END;
        TerminateDispatchMessages(code);
        w^.modal := FALSE;
    END;
END QuitWindowModalLoop;

PROCEDURE InitClientCreateData(clientType : ClientTypes; VAR OUT data : ClientCreateData);
BEGIN
    CASE clientType OF
    DrawClient:
    |
    ListClient:
        data.numColumns := 1;
        data.singleSelect := TRUE;
        data.columnInfo := NIL;
        data.headerSortButtons := FALSE;
        data.sortColumn := -1;
        data.sortCompareData := NIL;
        data.sortCompareProc := NILPROC;
        data.sortDirection := SortAscending;
        data.imageList := NIL;
    |
    TreeClient:
        data.treeLines := FALSE;
        data.treeFolders := TRUE;
        data.folderClosedImage := "";
        data.folderOpenImage := "";
        data.folderMaskColor := Colors[TransparentColor];
    |
    FormClient:
    |
    TabClient:
        data.tabPos := TabBottom;
        data.multiLineTabs := FALSE;
    |
    SplitterClient:
        data.splitHorizontal := FALSE;
        data.splitPos := 50;
        data.splitPosPercent := TRUE;
    END;
END InitClientCreateData;

PROCEDURE SetSplitterHandle(w : Window; yes : BOOLEAN);
VAR
    wnd         : HWND;
    pos         : ADRCARD;
BEGIN
    pos := ORD((w^.splitterView = CCSPV_BOTTOM) OR (w^.splitterView = CCSPV_RIGHT));

    IF yes THEN
        wnd := w^.wnd;
        w^.parent^.splitterChild[pos] := w;
    ELSE
        wnd := NIL;
        w^.parent^.splitterChild[pos] := NIL;
    END;

    CCsplitter_SetHandle(w^.parent^.clientWnd, w^.splitterView, wnd);
END SetSplitterHandle;

PROCEDURE GetWinStyle(attr : WinAttrSet; VAR OUT style, exStyle : DWORD);
BEGIN
    exStyle := 0;
    style := WS_OVERLAPPED BOR WS_CLIPCHILDREN BOR WS_BORDER;

    IF WA_TITLE IN attr THEN
        style := style BOR WS_CAPTION;
    END;
    IF WA_SYSMENU IN attr THEN
        style := style BOR WS_SYSMENU;
    END;
    IF WA_MINIMIZEBOX IN attr THEN
        style := style BOR WS_MINIMIZEBOX;
    END;
    IF WA_MAXIMIZEBOX IN attr THEN
        style := style BOR WS_MAXIMIZEBOX;
    END;
    IF WA_RESIZABLE IN attr THEN
        style := style BOR WS_THICKFRAME;
    END;
    IF WA_VISIBLE IN attr THEN
        style := style BOR WS_VISIBLE;
    END;
    IF WA_TOPMOST IN attr THEN
        exStyle := WS_EX_TOPMOST;
    END;
END GetWinStyle;

PROCEDURE GetSplitterView(w : Window);
BEGIN
    IF w^.parent^.splitTopBottom THEN
        w^.splitterView := CCSPV_TOP;
        IF WA_SPLIT_POS2 IN w^.attr THEN
            w^.splitterView := CCSPV_BOTTOM;
        END;
    ELSE
        w^.splitterView := CCSPV_LEFT;
        IF WA_SPLIT_POS2 IN w^.attr THEN
            w^.splitterView := CCSPV_RIGHT;
        END;
    END;
END GetSplitterView;

PROCEDURE CreateListViewColumns(w : Window; wnd : HWND; numColumns : ADRCARD; info : ColumnInfoPointer; form : BOOLEAN);
VAR
    i           : ADRCARD;
    lvColumn    : LVCOLUMN;
    dummy       : INTEGER;
    style       : CARDINAL;
    headers     : BOOLEAN;
    bmp         : HBITMAP;
BEGIN
    headers := FALSE;
    FOR i := 0 TO numColumns-1 DO
        lvColumn.mask := LVCF_FMT BOR LVCF_SUBITEM;
        lvColumn.iSubItem := i;
        lvColumn.fmt := LVCFMT_LEFT;

        IF info <> NIL THEN
            CASE info^[i].align OF
            AlignLeft:
                lvColumn.fmt := LVCFMT_LEFT;
            |
            AlignCenter:
                lvColumn.fmt := LVCFMT_CENTER;
            |
            AlignRight:
                lvColumn.fmt := LVCFMT_RIGHT;
            END;

            IF info^[i].width >= 0 THEN
                lvColumn.mask := lvColumn.mask BOR LVCF_WIDTH;
                lvColumn.cx := info^[i].width;
                dummy := 1;
                IF form THEN
                    FormToPixel(w, lvColumn.cx, dummy);
                END;
            END;

            IF info^[i].header <> NIL THEN
                headers := TRUE;
                lvColumn.mask := lvColumn.mask BOR LVCF_TEXT;
                lvColumn.pszText := CAST(LPTSTR, info^[i].header);
            END;
        END;

        ListView_InsertColumn(wnd, i, lvColumn);
    END;

    style := GetWindowLong(wnd, GWL_STYLE);
    IF headers THEN
        style := style BAND (BNOT ORD(LVS_NOCOLUMNHEADER));
    ELSE
        style := style BOR LVS_NOCOLUMNHEADER;
    END;
    SetWindowLong(wnd, GWL_STYLE, style);

    IF NOT form THEN
        w^.headerWnd := ListView_GetHeader(w^.clientWnd);
        IF w^.headerWnd <> NIL THEN
            IF w^.headerImageList = NIL THEN
                w^.headerImageList := ImageList_Create(12, 12, ILC_COLOR BOR ILC_MASK, 2, 0);
                bmp := WINUSER.LoadBitmap(ResourceInst, "LIST-SORT-ASCENDING");
                ImageList_AddMasked(w^.headerImageList, bmp, RGB(0,255,255));
                DeleteBitmap(bmp);
                bmp := WINUSER.LoadBitmap(ResourceInst, "LIST-SORT-DESCENDING");
                ImageList_AddMasked(w^.headerImageList, bmp, RGB(0,255,255));
                DeleteBitmap(bmp);
                IF ImageList_GetImageCount(w^.headerImageList) <> 2 THEN
                    ImageList_Destroy(w^.headerImageList);
                    w^.headerImageList := NIL;
                END;
            END;

            IF w^.headerImageList <> NIL THEN
                Header_SetImageList(w^.headerWnd, w^.headerImageList);
            END;
        END;

        IF headers THEN
            ListClientSetSortColumn(w, w^.sortColumn, w^.sortDirection);
        END;
    END;

    IF info <> NIL THEN
        FOR i := 0 TO numColumns-1 DO
            IF info^[i].width < 0 THEN
                ListView_SetColumnWidth(wnd, i, LVSCW_AUTOSIZE_USEHEADER);
            END;
        END;
    END;
END CreateListViewColumns;

PROCEDURE CreateClient(w : Window; parent : HWND; sendCreate : BOOLEAN) : BOOLEAN;
VAR
    style       : DWORD;
    styleEx     : DWORD;
    client      : HWND;
    split       : CCSPLIT;
    msg         : MessageRec;
    rc          : RECT;
    i           : ADRCARD;
    bmp         : HBITMAP;

    PROCEDURE addImage(name : ARRAY OF CHAR; mask : ColorValue);
    VAR
        bmp     : HBITMAP;
        ico     : HICON;
    BEGIN
        ico := MyLoadIcon(ResourceInst, name, 16, 16);
        IF ico <> NIL THEN
            ImageList_AddIcon(w^.imageList, ico);
        ELSE
            bmp := LoadResBitmap(name, NullColorMap);
            IF bmp <> NIL THEN
                ImageList_AddMasked(w^.imageList, bmp, mask);
                DeleteBitmap(bmp);
            END;
        END;
    END addImage;

BEGIN
    GetClientRect(parent, rc);

    client := NIL;

    styleEx := 0;
    style := WS_CHILD BOR WS_CLIPSIBLINGS BOR WS_CLIPCHILDREN;
    IF (WA_VISIBLE IN w^.attr) OR (w^.type = ToplevelWindow) THEN
        style := style BOR WS_VISIBLE;
    END;
    IF IsSplitterChild(w) THEN
        styleEx := WS_EX_CLIENTEDGE;
    END;

    CASE w^.clientType OF
    DrawClient:
        IF WA_VSCROLL IN w^.attr THEN
            style := style BOR WS_VSCROLL;
        END;
        IF WA_HSCROLL IN w^.attr THEN
            style := style BOR WS_HSCROLL;
        END;

        client := CreateWindowEx(styleEx,
                                 DrawClientClassName,
                                 NIL_STR,
                                 style,
                                 0, 0, rc.right, rc.bottom,
                                 parent,
                                 CAST(HMENU, DRAWCLIENT_ID),
                                 Instance,
                                 w);
    |
    TabClient:
        style := style BOR TCS_FOCUSNEVER;
        CASE w^.createData^.tabPos OF
        TabBottom:
            style := style BOR TCS_BOTTOM;
        |
        TabTop:
            (* normal position *)
        |
        TabLeft:
            style := style BOR TCS_VERTICAL;
        |
        TabRight:
            style := style BOR (TCS_RIGHT BOR TCS_VERTICAL);
        END;
        IF w^.createData^.multiLineTabs THEN
            style := style BOR TCS_MULTILINE;
        END;

        client := CreateWindowEx(styleEx,
                                 WC_TABCONTROL,
                                 NIL_STR,
                                 style,
                                 0, 0, rc.right, rc.bottom,
                                 parent,
                                 CAST(HMENU, TABCLIENT_ID),
                                 Instance,
                                 NIL);

        IF client <> NIL THEN
            WINUSER.SendMessage(client, WM_SETFONT, CAST(WPARAM, TabControlFont), 1);

            SetWindowLongPtr(client, WindowDataPos[TabClient], CAST(LONG_PTR, w));
            w^.oldWndProc := SubclassWindow(client, ClientSubclassWndProc1);
        END;
    |
    FormClient:
        style := style BOR DS_3DLOOK;
        style := style BAND (BNOT ORD(WS_CLIPCHILDREN));(*groupbox and rubber band rect*)
        styleEx := styleEx BOR WS_EX_CONTROLPARENT;

        client := CreateWindowEx(styleEx,
                                 FormClientClassName,
                                 NIL_STR,
                                 style,
                                 0, 0, rc.right, rc.bottom,
                                 parent,
                                 CAST(HMENU, FORMCLIENT_ID),
                                 Instance,
                                 w);
    |
    SplitterClient:
        client := CreateWindowEx(styleEx,
                                 WC_CCSPLITTER,
                                 NIL_STR,
                                 style,
                                 0, 0, rc.right, rc.bottom,
                                 parent,
                                 CAST(HMENU, SPLITTER_ID),
                                 Instance,
                                 w);

        w^.splitTopBottom := NOT w^.createData^.splitHorizontal;

        IF client <> NIL THEN
            split.cbSize := SIZE(split);
            split.flags := CCSP_FLOWER BOR CCSP_FUPPER;
            split.pos := w^.createData^.splitPos;
            IF w^.createData^.splitPosPercent THEN
                split.flags := split.flags BOR CCSP_FPOS;
            ELSE
                split.flags := split.flags BOR CCSP_POS;
            END;
            split.lower := 10;
            split.upper := 90;
            IF w^.splitTopBottom THEN
                CCsplitter_SetSplit(client, NIL, ADR(split));
            ELSE
                CCsplitter_SetSplit(client, ADR(split), NIL);
            END;

            SetWindowLongPtr(client, WindowDataPos[SplitterClient], CAST(LONG_PTR, w));
            w^.oldWndProc := SubclassWindow(client, ClientSubclassWndProc1);
        END;
    |
    ListClient:
        style := style BOR LVS_REPORT BOR LVS_SHOWSELALWAYS;
        IF NOT w^.createData^.headerSortButtons THEN
            style := style BOR LVS_NOSORTHEADER;
        END;
        IF w^.createData^.singleSelect THEN
            style := style BOR LVS_SINGLESEL;
        END;
        w^.sortColumn := w^.createData^.sortColumn;
        w^.sortDirection := w^.createData^.sortDirection;
        w^.compareProc := w^.createData^.sortCompareProc;
        w^.numColumns := w^.createData^.numColumns;
        w^.returnKeys := w^.createData^.listReturnKeys;
        IF w^.numColumns = 0 THEN
            w^.numColumns := 1;
        END;
        w^.columnInfo := NIL;

        client := CreateWindowEx(styleEx,
                                 WC_LISTVIEW,
                                 NIL_STR,
                                 style,
                                 0, 0, rc.right, rc.bottom,
                                 parent,
                                 CAST(HMENU, LISTCLIENT_ID),
                                 Instance,
                                 w);

        IF client <> NIL THEN
            FUNC SetWindowLongPtr (client, GWL_WNDPROC, CAST (ADRCARD, ListViewWndProc));
            styleEx := LVS_EX_FULLROWSELECT BOR LVS_EX_GRIDLINES;

            IF w^.createData^.columnInfo <> NIL THEN
                NEW(w^.columnInfo, w^.numColumns-1);
                <*/PUSH/NOWARN:U*>
                FOR i := 0 TO VAL(ADRCARD, w^.numColumns)-1 DO
                <*/POP*>
                    w^.columnInfo^[i].header := DuplicateStringData(w^.createData^.columnInfo^[i].header);
                    w^.columnInfo^[i].align := w^.createData^.columnInfo^[i].align;
                    w^.columnInfo^[i].width := w^.createData^.columnInfo^[i].width;
                    w^.columnInfo^[i].content :=  w^.createData^.columnInfo^[i].content;
                    w^.columnInfo^[i].sortable := w^.createData^.columnInfo^[i].sortable;
                    IF w^.columnInfo^[i].content <> ColumnText THEN
                        styleEx := styleEx BOR LVS_EX_SUBITEMIMAGES;
                    END;
                END;
            END;

            ListView_SetExtendedListViewStyle(client, styleEx);

            IF w^.createData^.imageList <> NIL THEN
                ListView_SetImageList(client, w^.createData^.imageList, LVSIL_SMALL);
            END;

            SetWindowLongPtr(client, WindowDataPos[ListClient], CAST(LONG_PTR, w));
            w^.oldWndProc := SubclassWindow(client, ClientSubclassWndProc1);
        END;
    |
    TreeClient:
        w^.returnKeys := w^.createData^.treeReturnKeys;

        style := style BOR
                    TVS_SHOWSELALWAYS BOR
                    TVS_HASBUTTONS BOR
                    TVS_LINESATROOT(*needed for buttons to show at root level*);
        IF w^.createData^.treeLines THEN
            style := style BOR TVS_HASLINES;
        END;
        client := CreateWindowEx(styleEx,
                                 WC_TREEVIEW,
                                 NIL_STR,
                                 style,
                                 0, 0, rc.right, rc.bottom,
                                 parent,
                                 CAST(HMENU, TREECLIENT_ID),
                                 Instance,
                                 w);

        IF client <> NIL THEN
            SetWindowLongPtr(client, WindowDataPos[TreeClient], CAST(LONG_PTR, w));
            w^.oldWndProc := SubclassWindow(client, ClientSubclassWndProc1);

            IF w^.createData^.treeFolders THEN
                w^.imageList := ImageList_Create(16, 16, ILC_COLOR BOR ILC_MASK, 3, 0);

                bmp := WINUSER.LoadBitmap(ResourceInst, "TREE-DOCUMENT");(*100% masked*)
                ImageList_AddMasked(w^.imageList, bmp, RGB(0,255,255));
                DeleteBitmap(bmp);

                IF w^.createData^.folderOpenImage[0] = '' THEN
                    bmp := WINUSER.LoadBitmap(ResourceInst, "TREE-FOLDER-CLOSED");
                    ImageList_AddMasked(w^.imageList, bmp, RGB(0,255,255));
                    DeleteBitmap(bmp);

                    bmp := WINUSER.LoadBitmap(ResourceInst, "TREE-FOLDER-OPEN");
                    ImageList_AddMasked(w^.imageList, bmp, RGB(0,255,255));
                    DeleteBitmap(bmp);
                ELSE
                    addImage(w^.createData^.folderClosedImage, w^.createData^.folderMaskColor);
                    addImage(w^.createData^.folderOpenImage, w^.createData^.folderMaskColor);
                END;

                IF ImageList_GetImageCount(w^.imageList) = 3 THEN
                    TreeView_SetImageList(client, w^.imageList, TVSIL_NORMAL);
                ELSE
                    ImageList_Destroy(w^.imageList);
                    w^.imageList := NIL;
                END;
            END;
        END;
    END;

    w^.clientWnd := client;

    IF client <> NIL THEN
        IF w^.type = ChildWindow THEN
            w^.wnd := client;

            w^.suppressWndProc := TRUE;
            IF IsTabChild(w) THEN
                IF WA_VISIBLE IN w^.attr THEN
                    ShowTabChild(w);
                ELSE
                    (* just to size the hidden child *)
                    AdjustClientSize(w^.parent);
                    ShowWindow(client, SW_HIDE);
                END;
            ELSIF IsSplitterChild(w) THEN
                GetSplitterView(w);
                SetSplitterHandle(w, TRUE);
                IF WA_VISIBLE IN w^.attr THEN
                    ShowWindow(client, SW_SHOW);
                END;
            END;
            w^.suppressWndProc := FALSE;
        END;

        IF w^.clientType = ListClient THEN
            CreateListViewColumns(w, client, w^.numColumns, w^.columnInfo, FALSE);
        END;

        IF sendCreate AND w^.modalForm THEN
            msg.msg := WSM_CREATE;
            msg.createParam := w^.createParam;
            CallWndProc(w, msg);
        END;

        IF (w^.type = ChildWindow) AND (w^.clientType = DrawClient) THEN
            msg.msg := WSM_SIZE;
            msg.width := w^.width;
            msg.height := w^.height;
            CallWndProc(w, msg);
        END;
    END;

    IF w^.modalForm THEN
        w^.createData := NIL;
    END;

    RETURN client <> NIL;
END CreateClient;

PROCEDURE CreateWindow(parent : Window;
                       name : ARRAY OF CHAR;
                       menu : ARRAY OF CHAR;
                       icon : ARRAY OF CHAR;
                       windowType : WindowTypes;
                       clientType : ClientTypes;
                       wndProc : WindowProcedure;
                       attribs : WinAttrSet;
                       x, y : COORDINATE;
                       width, height : COORDINATE;
                       clientCreateData : ClientCreateDataPointer;
                       createParam : ADDRESS) : Window;
BEGIN
    RETURN CreateWindowCommon(parent, name, menu, icon, windowType, clientType, wndProc, attribs,
                              x, y, width, height, clientCreateData, createParam,TRUE);
END CreateWindow;


PROCEDURE CreateWindowCommon(parent : Window;
                             name : ARRAY OF CHAR;
                             menu : ARRAY OF CHAR;
                             icon : ARRAY OF CHAR;
                             windowType : WindowTypes;
                             clientType : ClientTypes;
                             wndProc : WindowProcedure;
                             attribs : WinAttrSet;
                             x, y : COORDINATE;
                             width, height : COORDINATE;
                       createData : ClientCreateDataPointer;
                             createParam : ADDRESS;
                             isModal : BOOLEAN) : Window;
VAR
    w           : Window;
    style       : DWORD;
    exStyle     : DWORD;
    parentWnd   : HWND;
    data        : ClientCreateData;
    wx, wy      : ADRINT;
    ww, wh      : INTEGER;
    i           : ADRCARD;
    msg         : MessageRec;
    res         : ResponseType;
    info        : TlsInfo;
BEGIN
    IF (parent <> NIL) AND NOT IsWindow(parent) THEN
        RETURN NIL;
    END;
    CASE windowType OF
    ToplevelWindow:
        IF parent <> NIL THEN
            IF parent^.type = ChildWindow THEN
                RETURN NIL;
            END;
            IF parent^.external THEN
                INC(parent^.extRefCount);
            END;
        END;
    |
    ChildWindow:
        EXCL(attribs, WA_STATUSLINE);

        IF (parent = NIL) OR parent^.external THEN
            RETURN NIL;
        ELSIF (parent^.clientType <> TabClient) AND (parent^.clientType <> SplitterClient) THEN
            RETURN NIL;
        ELSIF parent^.clientType = SplitterClient THEN
            wx := ORD(WA_SPLIT_POS2 IN attribs);
            IF parent^.splitterChild[wx] <> NIL THEN
                RETURN NIL;
            END;
        END;
    END;

    parentWnd := NIL;
    IF parent <> NIL THEN
        (* we make all children in a toplevel siblings for Windows.
           for ourselves we maintain the parent child relationship.
        *)
        w := GetToplevel(parent);
        parentWnd := w^.wnd;
    END;

    IF clientType <> DrawClient THEN
        attribs := attribs - AddScrollBars;
    END;

    NEW(w);
    IF w = NIL THEN
        RETURN NIL;
    END;

    InitWindow(w);

    w^.type := windowType;
    w^.clientType := clientType;
    IF (windowType = ToplevelWindow) AND (clientType = FormClient) THEN
        w^.modalForm := isModal;
    ELSE
        w^.modalForm := TRUE;
    END;

    IF createData <> NIL THEN
        w^.createData := createData;
    ELSE
        InitClientCreateData(clientType, data);
        w^.createData := ADR(data);
    END;

    w^.parent := parent;
    IF menu[0] <> '' THEN
        w^.menu := MyLoadMenu(ResourceInst, menu);
        w^.activeMenu := w^.menu;
        w^.accel := MyLoadAccelerators(ResourceInst, menu);
    END;
    IF icon[0] <> '' THEN
        w^.bigIcon := MyLoadIcon(ResourceInst, icon, 0, 0);
        IF w^.bigIcon <> NIL THEN
            w^.smallIcon := MyLoadIcon(ResourceInst, icon, 16, 16);
        END;
    END;
    IF w^.bigIcon = NIL THEN
        w^.bigIcon := LoadIcon(NIL, IDI_APPLICATION^);
        w^.smallIcon := NIL;
    END;
    w^.cursor := LoadCursor(NIL, IDC_ARROW^);
    w^.createParam := createParam;
    w^.wndProc := wndProc;
    w^.attr := attribs;
    w^.title := name;

    FOR i := 0 TO MaxUserData DO
        w^.userData[i] := NIL;
    END;

    IF parent <> NIL THEN
        IF parent^.firstChild = NIL THEN
            parent^.firstChild := w;
        ELSE
            parent^.lastChild^.nextSibling := w;
        END;
        parent^.lastChild := w;
    END;

    IF (MainWindow = NIL) AND (windowType = ToplevelWindow) THEN
        MainWindow := w;
    END;

    w^.next := FirstWindow;
    FirstWindow := w;

    wx := CW_USEDEFAULT;
    wy := CW_USEDEFAULT;
    ww := CW_USEDEFAULT;
    wh := CW_USEDEFAULT;
    IF x >= 0 THEN
        wx := x;
    END;
    IF y >= 0 THEN
        wy := y;
    END;
    IF width >= 0 THEN
        ww := width;
    END;
    IF height >= 0 THEN
        wh := height;
    END;

    CASE windowType OF
    ToplevelWindow:
        (* the client will be created during the create message *)
        GetWinStyle(w^.attr, style, exStyle);
        w^.wnd := CreateWindowEx(exStyle,
                                 ToplevelClassName,
                                 name,
                                 style,
                                 wx, wy,
                                 ww, wh,
                                 parentWnd,
                                 w^.menu,
                                 Instance,
                                 w);
        IF w^.wnd <> NIL THEN
            info := GetThreadInfo();
        END;
    |
    ChildWindow:
        CreateClient(w, parentWnd, TRUE);
    END;

    IF w^.wnd <> NIL THEN
        IF IsTabChild(w) AND (w^.parent^.active = w) THEN
            msg.msg := WSM_TAB_ACTIVE;
            res := CallWndProc(w, msg);
        END;
        UpdateWindow(w);
        RETURN w;
    ELSE
        DisposeWindow(w);
        RETURN NIL;
    END;
END CreateWindowCommon;

(*MVN+*)

<*/PUSH/NOWARN:U*>
PROCEDURE MsgLoopThread(param : ADDRESS) : CARDINAL;
<*/POP*>

VAR
    w : Window;
    code : CARDINAL;
    GuiEvent : WIN32.HANDLE;


    PROCEDURE Prologue;

    BEGIN

        CreateWindowTemp.wnd := CreateWindowCommon(CreateWindowTemp.parent,
                                        CreateWindowTemp.name,
                                        CreateWindowTemp.menu,(*menu*)
                                        CreateWindowTemp.icon,(*icon*)
                                        CreateWindowTemp.windowType,
                                        CreateWindowTemp.clientType,
                                        CreateWindowTemp.wndProc,
                                        CreateWindowTemp.attribs,
                                        CreateWindowTemp.x,
                                        CreateWindowTemp.y,
                                        CreateWindowTemp.width,
                                        CreateWindowTemp.height,
                                        CreateWindowTemp.clientCreateData,
                                        CreateWindowTemp.createParam,
                                        FALSE);

        w := CreateWindowTemp.wnd;
        IF w = NIL THEN
            WIN32.SetEvent(CreateWindowTemp.GuiEvent);
        END;

    EXCEPT

        WIN32.SetEvent(CreateWindowTemp.GuiEvent);

    END Prologue;

BEGIN
    Float.InitBoth;
    GuiEvent := CreateWindowTemp.GuiEvent;
    Prologue;
    code := 0;
(*
    WHILE WINUSER.GetMessage(msg, NIL, 0, 0) DO
        TranslateMessage(msg);
        DispatchMessage(msg);
    END;
*)
    IF w # NIL THEN
        code := RunWindowModalLoop(w);
    END;

    (*IF pw^ # NIL THEN
        pw^^.GuiThread := NIL;
    END;*)

    WIN32.CloseHandle(GuiEvent);

    RETURN code;
END MsgLoopThread;


PROCEDURE CreateNonmodalWindow(name : ARRAY OF CHAR;
                       menu : ARRAY OF CHAR;
                       icon : ARRAY OF CHAR;
                       windowType : WindowTypes;
                       clientType : ClientTypes;
                       wndProc : WindowProcedure;
                       attribs : WinAttrSet;
                       x, y : COORDINATE;
                       width, height : COORDINATE;
                       createData : ClientCreateDataPointer;
                       createParam : ADDRESS) : Window;

VAR
    w : Window;
    t : Thread;
    res : DWORD;

BEGIN
    CreateWindowTemp.parent := NIL;
    CreateWindowTemp.name := name;
    CreateWindowTemp.menu := menu;
    CreateWindowTemp.icon := icon;
    CreateWindowTemp.windowType := windowType;
    CreateWindowTemp.clientType := clientType;
    CreateWindowTemp.wndProc := wndProc;
    CreateWindowTemp.wnd := NIL;
    CreateWindowTemp.attribs := attribs;
    CreateWindowTemp.x := x;
    CreateWindowTemp.y := y;
    CreateWindowTemp.width := width;
    CreateWindowTemp.height := height;
    CreateWindowTemp.clientCreateData := createData;
    CreateWindowTemp.createParam := createParam;

    CreateWindowTemp.GuiEvent := WIN32.CreateEvent(WINX.NIL_SECURITY_ATTRIBUTES, FALSE, FALSE, NIL_STR);
    IF CreateWindowTemp.GuiEvent = NIL THEN
        RETURN NIL;
    END;
    w := NIL;
    CreateThread(t, MsgLoopThread, NIL, 0, FALSE);
    res := WIN32.WaitForSingleObject(CreateWindowTemp.GuiEvent, WIN32.INFINITE);
    w := CreateWindowTemp.wnd;
    IF IsWindow(w) THEN
        w^.GuiThread := t;
        w^.GuiEvent := CreateWindowTemp.GuiEvent;
(*
        msg.msg := WSM_CREATE;
        msg.createParam := w^.createParam;
        CallWndProc(w, msg);

        IF (w^.type = ChildWindow) AND (w^.clientType = DrawClient) THEN
            msg.msg := WSM_SIZE;
            msg.width := w^.width;
            msg.height := w^.height;
            CallWndProc(w, msg);
        END;

        w^.createData := NIL;
*)
    END;

    RETURN w;
END CreateNonmodalWindow;

PROCEDURE GetScreenSize(VAR x : COORDINATE; VAR y : COORDINATE);
BEGIN
    x := WINUSER.GetSystemMetrics(WINUSER.SM_CXFULLSCREEN);
    y := WINUSER.GetSystemMetrics(WINUSER.SM_CYFULLSCREEN);
END GetScreenSize;

PROCEDURE SetTopMost(w : Window; topmost : BOOLEAN);
VAR pos : HWND;
BEGIN
    IF IsWindow(w) THEN
        IF topmost THEN
            pos := HWND_TOPMOST;
            WINUSER.SetForegroundWindow(w^.wnd);
        ELSE
            (*pos := HWND_TOP;*)
            pos := HWND_NOTOPMOST;
        END;
        IF IsCurrentThread(w^.wnd) THEN
            WINUSER.SetWindowPos(w^.wnd,
                                 pos,
                                 0, 0, 0, 0,
                                 SWP_NOMOVE BOR SWP_NOSIZE (*BOR SWP_NOACTIVATE*));
        ELSE
            WINUSER.SetWindowPos(w^.wnd,
                                 pos,
                                 0, 0, 0, 0,
                                 SWP_ASYNCWINDOWPOS BOR SWP_NOMOVE BOR SWP_NOSIZE (*BOR SWP_NOACTIVATE*));
        END;
    END;
END SetTopMost;
(*MVN-*)

PROCEDURE SetDefaultDrawContext(w : Window; context : DrawContext);
BEGIN
    IF IsWindow(w) AND (w^.clientType = DrawClient) THEN
        w^.defaultDrawContext := context;
    END;
END SetDefaultDrawContext;

PROCEDURE SetSplitPosition(w : Window; pos : CARDINAL; percent : BOOLEAN);
VAR
    split       : CCSPLIT;
BEGIN
    IF IsWindow(w) AND (w^.clientType = SplitterClient) AND (pos <> 0) THEN
        split.cbSize := SIZE(split);
        IF percent THEN
            split.flags := CCSP_FPOS;
            IF pos > 99 THEN
                pos := 99;
            END;
        ELSE
            split.flags := CCSP_POS;
        END;
        split.pos := pos;
        IF w^.splitTopBottom THEN
            CCsplitter_SetSplit(w^.clientWnd, NIL, ADR(split));
        ELSE
            CCsplitter_SetSplit(w^.clientWnd, ADR(split), NIL);
        END;
    END;
END SetSplitPosition;

PROCEDURE GetSplitPosition(w : Window; percent : BOOLEAN) : CARDINAL;
VAR
    split       : CCSPLIT;
    rc          : RECT;
    pos         : INTEGER;
BEGIN
    pos := 0;
    IF IsWindow(w) AND (w^.clientType = SplitterClient) THEN
        split.cbSize := SIZE(split);
        GetClientRect(w^.clientWnd, rc);
        IF w^.splitTopBottom THEN
            CCsplitter_GetSplit(w^.clientWnd, NIL, ADR(split));
            IF split.pos > 0 THEN
                pos := split.pos;
                IF percent THEN
                    pos := pos * 100 / rc.bottom;
                END;
            END;
        ELSE
            CCsplitter_GetSplit(w^.clientWnd, ADR(split), NIL);
            IF split.pos > 0 THEN
                pos := split.pos;
                IF percent THEN
                    pos := pos * 100 / rc.right;
                END;
            END;
        END;
    END;
    RETURN pos;
END GetSplitPosition;

PROCEDURE SplitWindow(w : Window; wndProc : WindowProcedure; splitY : BOOLEAN; VAR OUT newChild : Window);
VAR
    style       : DWORD;
    ctrl        : ControlInfoPointer;
    top         : Window;
    createData  : ClientCreateData;
BEGIN
    newChild := NIL;
    IF IsWindow(w) THEN
        NEW(newChild);
        IF newChild <> NIL THEN
            top := GetToplevel(w);

            newChild^ := w^;

            (* menu stays with the window being split *)

            newChild^.menu := NIL;
            newChild^.accel := NIL;

            NEW(w^.drawable);
            InitDrawable(w^.drawable^);

            w^.clientType := SplitterClient;
            w^.wndProc := wndProc;
            w^.firstControl := NIL;
            w^.lastControl := NIL;
            w^.numControlsMain := 0;
            w^.numControlsAll := 0;
            w^.notifyCount := 0;
            w^.clientWnd := NIL;
            w^.caretBmp := NIL;
            w^.cursor := NIL;
            w^.width := -1;
            w^.height := -1;
            InitClientCreateData(SplitterClient, createData);
            createData.splitHorizontal := NOT splitY;
            w^.createData := ADR(createData);
            CreateClient(w, top^.wnd, FALSE);

            IF w^.firstChild = NIL THEN
                w^.firstChild := newChild;
            ELSE
                w^.lastChild^.nextSibling := newChild;
            END;
            w^.lastChild := newChild;
            newChild^.nextSibling := NIL;

            newChild^.next := FirstWindow;
            FirstWindow := newChild;

            newChild^.type := ChildWindow;
            newChild^.wnd := newChild^.clientWnd;
            newChild^.parent := w;
            newChild^.smallIcon := NIL;
            newChild^.bigIcon := NIL;
            newChild^.toolbar := NIL;
            newChild^.statusbar := NIL;
            newChild^.activeMenu := NIL;
            newChild^.menuWindow := NIL;
            newChild^.tabIndex := -1;
            EXCL(newChild^.attr, WA_SPLIT_POS2);
            GetSplitterView(newChild);

            SetWindowLongPtr(newChild^.wnd, WindowDataPos[newChild^.clientType], CAST(LONG_PTR, newChild));

            style := GetWindowLong(newChild^.wnd, GWL_EXSTYLE);
            style := style BOR WS_EX_CLIENTEDGE;
            SetWindowLong(newChild^.wnd, GWL_EXSTYLE, style);

            IF newChild^.clientType = FormClient THEN
                ctrl := newChild^.firstControl;
                WHILE ctrl <> NIL DO
                    ctrl^.w := newChild;
                    ctrl := ctrl^.next;
                END;
            END;

            IF (w^.type = ChildWindow) AND (w^.parent^.clientType = SplitterClient) THEN
                SetSplitterHandle(w, TRUE);
            END;

            IF w^.parent <> NIL THEN
                AdjustClientSize(w^.parent);
            ELSE
                AdjustClientSize(w);
            END;

            SetSplitterHandle(newChild, TRUE);

            IF IsTabChild(w) THEN
                DoSetActiveTabChild(w, FALSE);
            END;
        END;
    END;
END SplitWindow;

PROCEDURE UnsplitWindow(w : Window; keepChild : Window) : BOOLEAN;
VAR
    child, next : Window;
    splitter    : HWND;
    style       : DWORD;
    ctrl        : ControlInfoPointer;
    parent      : HWND;
    save        : WndInstance;
BEGIN
    IF IsWindow(w) AND
       (w^.clientType = SplitterClient) AND
       IsWindow(keepChild) AND
       IsChildOfWindow(keepChild, w)
    THEN
        splitter := w^.clientWnd;
        SubclassWindow(splitter, w^.oldWndProc);

        IF w^.parent = NIL THEN
            parent := w^.wnd;
        ELSE
            parent := w^.parent^.wnd;
        END;

        child := w^.firstChild;
        WHILE child <> NIL DO
            next := child^.nextSibling;
            IF child <> keepChild THEN
                DestroyWindow(child^.wnd);
            END;
            child := next;
        END;

        save := w^;

        DISPOSE(w^.drawable);

        w^ := keepChild^;

        w^.type := save.type;
        w^.next := save.next;
        w^.nextActive := save.nextActive;
        w^.nextSibling := save.nextSibling;
        w^.parent := save.parent;

        IF w^.type = ToplevelWindow THEN
            w^.wnd := save.wnd;
        END;
        w^.activeMenu := save.activeMenu;
        w^.menuWindow := save.menuWindow;
        w^.accel := save.accel;
        w^.menu := save.menu;
        w^.toolbar := save.toolbar;
        w^.statusbar := save.statusbar;
        w^.smallIcon := save.smallIcon;
        w^.bigIcon := save.bigIcon;

        w^.tabIndex := save.tabIndex;
        w^.notifyProcs := save.notifyProcs;
        w^.notifyCount := save.notifyCount;
        w^.notifyData := save.notifyData;
        w^.title := save.title;

        w^.width := -1;(*so we get a size message *)
        w^.height := -1;

        EXCL(w^.attr, WA_SPLIT_POS2);

        style := GetWindowLong(w^.clientWnd, GWL_EXSTYLE);
        style := style BAND (BNOT ORD(WS_EX_CLIENTEDGE));
        SetWindowLong(w^.clientWnd, GWL_EXSTYLE, style);

        SetWindowLongPtr(w^.clientWnd, WindowDataPos[w^.clientType], CAST(LONG_PTR, w));

        IF keepChild^.clientType = FormClient THEN
            ctrl := w^.firstControl;
            WHILE ctrl <> NIL DO
                ctrl^.w := w;
                ctrl := ctrl^.next;
            END;
        END;

        keepChild^.firstControl := NIL;
        keepChild^.drawable := NIL;
        keepChild^.caretBmp := NIL;
        keepChild^.cursor := NIL;
        DisposeWindow(keepChild);

        DestroyWindow(splitter);

        IF w^.parent <> NIL THEN
            AdjustClientSize(w^.parent);
        ELSE
            AdjustClientSize(w);
        END;

        IF IsTabChild(w) THEN
            DoSetActiveTabChild(w, FALSE);
        END;
        RETURN TRUE;
    END;
    RETURN FALSE;
END UnsplitWindow;

PROCEDURE SetTabPosition(w : Window; tabPos : TabPosition);
CONST
    winStyle    : ARRAY TabPosition OF CARDINAL =
        {0, TCS_BOTTOM, TCS_VERTICAL, TCS_RIGHT BOR TCS_VERTICAL};
VAR
    style       : CARDINAL;
BEGIN
    IF (w^.clientType = TabClient) AND (w^.tabPos <> tabPos) THEN
        w^.tabPos := tabPos;

        style := GetWindowLong(w^.clientWnd, GWL_STYLE);
        style := style BAND (BNOT ORD(TCS_RIGHT BOR TCS_BOTTOM BOR TCS_VERTICAL));
        style := style BOR winStyle[tabPos];
        SetWindowLong(w^.clientWnd, GWL_STYLE, style);

        InvalidateRect(w^.clientWnd, NIL_RECT, TRUE);
        AdjustClientSize(w);
    END;
END SetTabPosition;

PROCEDURE CycleActiveTabChild(w : Window; direction : INTEGER);
VAR
    parent      : Window;
    child       : Window;
    prev        : Window;
BEGIN
    parent := w;
    IF IsTabChild(w) THEN
        parent := w^.parent;
    END;

    IF (parent^.clientType = TabClient) AND (parent^.active <> NIL) THEN
        child := parent^.active;

        IF direction > 0 THEN
            REPEAT
                child := child^.nextSibling;
            UNTIL (child = NIL) OR IsTabChild(child);
            IF child = NIL THEN
                child := parent^.firstChild;
                WHILE (child <> NIL) AND (NOT IsTabChild(child)) DO
                    child := child^.nextSibling;
                END;
            END;
        ELSIF direction < 0 THEN
            prev := NIL;
            child := parent^.firstChild;
            WHILE (child <> NIL) AND (child <> w) DO
                IF IsTabChild(child) THEN
                    prev := child;
                END;
                child := child^.nextSibling;
            END;

            IF prev = NIL THEN
                child := parent^.firstChild;
                WHILE child <> NIL DO
                    IF IsTabChild(child) THEN
                        prev := child;
                    END;
                    child := child^.nextSibling;
                END;
            END;

            child := prev;
        END;

        IF child <> parent^.active THEN
            DoSetActiveTabChild(child, parent^.active^.hasFocus);
        END;
    END;
END CycleActiveTabChild;

PROCEDURE ConvertTabChildToTopLevel(w : Window) : BOOLEAN;
VAR
    style       : DWORD;
    exStyle     : DWORD;
    rect        : RECT;
    isCurrent   : BOOLEAN;
    index,
    newIndex    : INTEGER;
    top,
    parent      : Window;
    client      : HWND;
    count       : CARDINAL;
    menuW       : Window;
    tb          : ToolbarInfoPointer;

    PROCEDURE reparent(w : Window; parent : HWND);
    VAR
        child   : Window;
    BEGIN
        SetParent(w^.clientWnd, parent);

        child := w^.firstChild;
        WHILE child <> NIL DO
            reparent(child, parent);

            child := child^.nextSibling;
        END;
    END reparent;

BEGIN
    IF (w^.type = ChildWindow) AND IsTabChild(w) THEN
        parent := w^.parent;
        top := GetToplevel(parent);
        isCurrent := IsCurrentTabChild(w);
        index := w^.tabIndex;
        GetWindowRect(w^.clientWnd, rect);

        newIndex := index;
        IF w^.nextSibling = NIL THEN
            DEC(newIndex);
        END;

        UnlinkChild(w);
        UnlinkActive(w);
        w^.parent := NIL;

        menuW := w;
        IF menuW^.clientType = SplitterClient THEN
            IF menuW^.menu = NIL THEN
                menuW := menuW^.firstChild;
                WHILE (menuW <> NIL) AND (menuW^.menu = NIL) DO
                    menuW := menuW^.nextSibling;
                END;
                IF menuW = NIL THEN
                    menuW := w;
                END;
            END;
        END;

        w^.type := ToplevelWindow;
        w^.attr := w^.attr + NormalWindow;
        w^.menuWindow := NIL;
        GetWinStyle(w^.attr-WinAttrSet{WA_VISIBLE}, style, exStyle);
        w^.createClient := FALSE;
        client := w^.clientWnd;
        w^.wnd := CreateWindowEx(exStyle,
                                 ToplevelClassName,
                                 w^.title,
                                 style,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 CW_USEDEFAULT, CW_USEDEFAULT,
                                 NIL(*parent^.wnd*),
                                 menuW^.menu,
                                 Instance,
                                 w);
        w^.clientWnd := client;

        reparent(w, w^.wnd);

        AdjustClientSize(w);

        IF top^.statusbar <> NIL THEN
            count := top^.statusbar^.numFields;
            CreateStatusLine(w, top^.statusbar^.fields[0..count-1]);
        END;

        tb := top^.toolbar;
        IF (tb <> NIL) AND
           (tb^.createInfo.buttons <> NIL) AND
           (tb^.createInfo.fmt <> NIL)
        THEN
            count := tb^.createInfo.numButtons;
            CreateToolbar(w,
                          tb^.createInfo.buttons^[0..count-1],
                          tb^.hasText,
                          tb^.hasHelp,
                          FALSE(*tb^.customize*));
            count := tb^.createInfo.numFmt;
            SetToolbarButtons(w, tb^.createInfo.fmt^[0..count-1]);
        END;

        (* now get rid of the tab, and activate a new child *)

        TabCtrl_DeleteItem(parent^.clientWnd, index);

        ReindexTabChildren(parent, FALSE);
        IF parent^.active <> NIL THEN
            IF isCurrent THEN
                DoSetActiveTabChild(parent^.active, FALSE);
            END;
        ELSE
            IF parent^.type = ToplevelWindow THEN
                SetMdiMenu(parent);
            END;
        END;

        (* finally show our creation *)

        SetDisplayMode(w, DisplayNormal);
        RETURN TRUE;
    END;
    RETURN FALSE;
END ConvertTabChildToTopLevel;

PROCEDURE UnlinkChild(w : Window);
VAR
    ptr, prev   : Window;
BEGIN
    ptr := w^.parent^.firstChild;
    prev := NIL;
    WHILE (ptr <> NIL) AND (ptr <> w) DO
        prev := ptr;
        ptr := ptr^.nextSibling;
    END;
    IF ptr <> NIL THEN
        IF prev = NIL THEN
            w^.parent^.firstChild := ptr^.nextSibling;
        ELSE
            prev^.nextSibling := ptr^.nextSibling;
        END;
        IF w^.parent^.lastChild = ptr THEN
            w^.parent^.lastChild := prev;
        END;
    END;
END UnlinkChild;

PROCEDURE UnlinkActive(w : Window);
VAR
    ptr, prev   : Window;
BEGIN
    ptr := w^.parent^.active;
    prev := NIL;
    WHILE (ptr <> NIL) AND (ptr <> w) DO
        prev := ptr;
        ptr := ptr^.nextActive;
    END;
    IF ptr <> NIL THEN
        IF prev <> NIL THEN
            prev^.nextActive := ptr^.nextActive;
        ELSE
            w^.parent^.active := ptr^.nextActive;
        END;
    END;
END UnlinkActive;

PROCEDURE DisposeWindow(VAR INOUT w : Window);
VAR
    ptr, prev   : Window;
    i           : ADRCARD;
BEGIN
    IF IsWindow(w) THEN
        (* dispose some stuff *)

        IF w^.menu <> NIL THEN
            (*SetMenu(w^.wnd, NIL);*)
            WINUSER.DestroyMenu(w^.menu);
            w^.menu := NIL;
        END;
        IF w^.accel <> NIL THEN
            DestroyAcceleratorTable(w^.accel);
            w^.accel := NIL;
        END;
        IF w^.caretBmp <> NIL THEN
            IF w^.hasCaret AND w^.caretCreated THEN
                DestroyCaret();
                w^.caretCreated := FALSE;
            END;
            WINX.DeleteBitmap(w^.caretBmp);
            w^.caretBmp := NIL;
        END;
        IF w^.calledHelp THEN
            WinHelp(w^.wnd, "", HELP_QUIT, 0);
            w^.calledHelp := FALSE;
        END;
        IF w^.clientType = FormClient THEN
            WHILE w^.firstControl <> NIL DO
                DestroyControl(w^.firstControl);
            END;
        END;
        IF w^.drawable <> NIL THEN
            DISPOSE(w^.drawable);
        END;
        IF w^.toolTipWnd <> NIL THEN
            DestroyWindow(w^.toolTipWnd);
        END;
        IF w^.columnInfo <> NIL THEN
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, w^.numColumns)-1 DO
            <*/POP*>
                DisposeStringData(w^.columnInfo^[i].header);
            END;
            DISPOSE(w^.columnInfo);
        END;
        IF w^.imageList <> NIL THEN
            ImageList_Destroy(w^.imageList);
        END;
        IF w^.headerImageList <> NIL THEN
            ImageList_Destroy(w^.headerImageList);
        END;
        IF w^.toolbar <> NIL THEN
            DoDestroyToolbar(w^.toolbar);
        END;
        IF w^.statusbar <> NIL THEN
            DISPOSE(w^.statusbar);
        END;

        (* unlink from list *)

        ptr := FirstWindow;
        prev := NIL;
        WHILE (ptr <> NIL) AND (ptr <> w) DO
            prev := ptr;
            ptr := ptr^.next;
        END;
        IF ptr <> NIL THEN
            IF prev = NIL THEN
                FirstWindow := ptr^.next;
            ELSE
                prev^.next := ptr^.next;
            END;
        END;

        IF w = MainWindow THEN
            ptr := FirstWindow;
            WHILE (ptr <> NIL) AND (ptr^.type <> ToplevelWindow) AND (ptr <> w) DO
                ptr := ptr^.next;
            END;
            MainWindow := ptr;
        END;

        (* unlink from child list *)

        IF w^.parent <> NIL THEN
            UnlinkChild(w);
            UnlinkActive(w);

            IF w^.parent^.external THEN
                IF w^.parent^.extRefCount > 0 THEN
                    DEC(w^.parent^.extRefCount);
                END;
                IF w^.parent^.extRefCount = 0 THEN
                    DisposeWindow(w^.parent);
                END;
            END;
        END;

        w^.validate := 0;

        DISPOSE(w);
    ELSE
        w := NIL;
    END;
END DisposeWindow;

PROCEDURE AddWindowNotify(w : Window; proc : WindowNotifyProcedure; data : MACHINEWORD) : BOOLEAN;
BEGIN
    IF w^.notifyCount < MaxNotifyProcs THEN
        INC(w^.notifyCount);
        w^.notifyProcs[w^.notifyCount] := proc;
        w^.notifyData[w^.notifyCount] := data;
        RETURN TRUE;
    END;
    RETURN FALSE;
END AddWindowNotify;

PROCEDURE RemoveWindowNotify(w : Window; proc : WindowNotifyProcedure);
VAR
    i, j        : ADRCARD;
BEGIN
    i := 1;
    <*/PUSH/NOWARN:U*>
    WHILE i <= VAL(ADRCARD, w^.notifyCount) DO
    <*/POP*>
        IF w^.notifyProcs[i] = proc THEN
            <*/PUSH/NOWARN:U*>
            FOR j := i+1 TO VAL(ADRCARD, w^.notifyCount) DO
            <*/POP*>
                w^.notifyProcs[j-1] := w^.notifyProcs[j];
                w^.notifyData[j-1] := w^.notifyData[j];
            END;
            DEC(w^.notifyCount);
            w^.notifyProcs[MaxNotifyProcs] := NILPROC;
            w^.notifyData[MaxNotifyProcs] := NIL;
        ELSE
            INC(i);
        END;
    END;
END RemoveWindowNotify;

PROCEDURE CloseWindow(w : Window; mode : CloseModes) : BOOLEAN;
BEGIN
    IF IsWindow(w) THEN
        CASE mode OF
        CM_REQUEST:
            RETURN WINUSER.SendMessage(w^.wnd, WM_CLOSE, 0, 0) = 1;
        |
        CM_DICTATE:
            DestroyWindow(w^.wnd);
        END;
    END;
    RETURN TRUE;
END CloseWindow;

PROCEDURE CloseAllChildren(parent : Window; mode : CloseModes) : BOOLEAN;
VAR
    w, next     : Window;
BEGIN
    w := parent^.firstChild;
    WHILE w <> NIL DO
        next := w^.nextSibling;

        IF NOT CloseWindow(w, mode) THEN
            IF mode <> CM_DICTATE THEN
                RETURN FALSE;
            END;
        END;

        w := next;
    END;
    RETURN TRUE;
END CloseAllChildren;

PROCEDURE DisplayTabChildNumber(w : Window; yes : BOOLEAN);
BEGIN
    IF (w^.clientType = TabClient) AND (w^.tabChildNumbers <> yes) THEN
        w^.tabChildNumbers := yes;

        w := w^.firstChild;
        WHILE w <> NIL DO
            IF IsTabChild(w) THEN
                SetWindowTitle(w, w^.title);
            END;

            w := w^.nextSibling;
        END;
    END;
END DisplayTabChildNumber;

PROCEDURE SetBackgroundAutoErase(w : Window; color : ColorValue);
BEGIN
    w^.autoErase := color <> Colors[TransparentColor];
    w^.backgroundColor := color;
    IF w^.autoErase THEN
        InvalidateRect(w^.clientWnd, NIL_RECT, TRUE);
    END;
END SetBackgroundAutoErase;

PROCEDURE IsWindow(w : Window) : BOOLEAN;
BEGIN
    IF (w <> NIL) AND (w^.validate = MagicNumber) AND WINUSER.IsWindow(w^.wnd) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END IsWindow;

PROCEDURE IsChildOfWindow(child, w : Window) : BOOLEAN;
BEGIN
    IF IsWindow(child) AND
       IsWindow(w) AND
       (child^.type = ChildWindow) AND
       (child^.parent = w)
    THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END IsChildOfWindow;

PROCEDURE GetWindowType(w : Window) : WindowTypes;
BEGIN
    IF IsWindow(w) THEN
        RETURN w^.type;
    END;
    RETURN ToplevelWindow;
END GetWindowType;

PROCEDURE GetClientType(w : Window) : ClientTypes;
BEGIN
    IF IsWindow(w) THEN
        RETURN w^.clientType;
    END;
    RETURN DrawClient;
END GetClientType;

PROCEDURE GetWindowParent(w : Window) : Window;
BEGIN
    IF IsWindow(w) THEN
        RETURN w^.parent;
    END;
    RETURN NIL;
END GetWindowParent;

PROCEDURE GetIthTabChild(w : Window; num : CARDINAL) : Window;
VAR
    child       : Window;
BEGIN
    IF IsWindow(w) AND (w^.clientType = TabClient) THEN
        child := w^.firstChild;
        LOOP
            IF child <> NIL THEN
                IF IsTabChild(child) AND (child^.tabIndex >= 0) THEN
                    IF num <> 0 THEN
                        DEC(num);
                        child := child^.nextSibling;
                    ELSE
                        RETURN child;
                    END;
                ELSE
                    child := child^.nextSibling;
                END;
            ELSE
                EXIT;
            END;
        END;
    END;
    RETURN NIL;
END GetIthTabChild;

PROCEDURE GetActiveTabChild(w : Window) : Window;
BEGIN
    IF IsWindow(w) AND (w^.clientType = TabClient) THEN
        RETURN w^.active;
    END;
    RETURN NIL;
END GetActiveTabChild;

PROCEDURE SetActiveTabChild(w : Window);
BEGIN
    IF IsWindow(w) AND IsTabChild(w) THEN
        DoSetActiveTabChild(w, FALSE);
    END;
END SetActiveTabChild;

PROCEDURE SetTabChildPosition(w : Window; index : CARDINAL);
VAR
    child, prev : Window;
    parent      : Window;
BEGIN
    IF IsWindow(w) AND IsTabChild(w) THEN
        IF (w^.tabIndex >= 0) AND (index <> ORD(w^.tabIndex)) THEN
            UnlinkChild(w);

            IF index > ORD(w^.tabIndex) THEN
                INC(index);
            END;
            parent := w^.parent;

            prev := NIL;
            child := parent^.firstChild;
            WHILE (child <> NIL) AND (INT(index) > child^.tabIndex) DO
                prev := child;
                child := child^.nextSibling;
            END;

            w^.nextSibling := child;
            IF prev <> NIL THEN
                prev^.nextSibling := w;
            ELSE
                parent^.firstChild := w;
            END;
            IF child = NIL THEN
                parent^.lastChild := w;
            END;

            ReindexTabChildren(parent, TRUE);
        END;
    END;
END SetTabChildPosition;

PROCEDURE GetTabChildPosition(w : Window) : INTEGER;
BEGIN
    IF IsWindow(w) AND IsTabChild(w) THEN
        RETURN w^.tabIndex;
    END;
    RETURN -1;
END GetTabChildPosition;

PROCEDURE CreateImageList(width, height : CARDINAL; initialCount, growth : CARDINAL) : ImageListHandle;
VAR
    im          : ImageListHandle;
    dc, drawDC  : HDC;
    bmp, oldBmp : HBITMAP;
    brush       : HBRUSH;
    rc          : RECT;
BEGIN
    im := ImageList_Create(width, height, ILC_COLOR BOR ILC_MASK, initialCount+1, growth);
    IF im <> NIL THEN
        ImageList_SetBkColor(im, CLR_NONE);

        dc := GetDC(NIL);
        bmp := CreateCompatibleBitmap(dc, width, height);
        drawDC := CreateCompatibleDC(dc);
        ReleaseDC(NIL, dc);
        oldBmp := SelectBitmap(drawDC, bmp);
        brush := CreateSolidBrush(0FFFFFFh);
        rc.left := 0;
        rc.top := 0;
        rc.right := width;
        rc.bottom := height;
        WINUSER.FillRect(drawDC, rc, brush);
        DeleteBrush(brush);
        SelectBitmap(drawDC, oldBmp);
        DeleteDC(drawDC);
        ImageList_AddMasked(im, bmp, 0FFFFFFh);
        WINX.DeleteBitmap(bmp);
    END;
    RETURN im;
END CreateImageList;

PROCEDURE DestroyImageList(im : ImageListHandle);
BEGIN
    IF im <> NIL THEN
        ImageList_Destroy(im);
    END;
END DestroyImageList;

PROCEDURE ImageListLoadImage(im : ImageListHandle;
                             resName : ARRAY OF CHAR;
                             bmp : BitmapHandle;
                             transparent : ColorValue) : INTEGER;
VAR
    ico         : HICON;
    retVal      : INTEGER;
    width,
    height      : INTEGER;
BEGIN
    IF im <> NIL THEN
        IF bmp = NIL THEN
            bmp := LoadResBitmap(resName, NullColorMap);
        END;
        IF bmp <> NIL THEN
            IF transparent <> Colors[TransparentColor] THEN
                retVal := ImageList_AddMasked(im, bmp, transparent);
            ELSE
                retVal := ImageList_Add(im, bmp, NIL);
            END;
            WINX.DeleteBitmap(bmp);
            RETURN retVal;
        END;

        IF NOT ImageList_GetIconSize(im, width, height) THEN
            width := 0;
            height := 0;
        END;
        ico := MyLoadIcon(ResourceInst, resName, width, height);
        IF ico <> NIL THEN
            RETURN ImageList_AddIcon(im, ico);
        END;
    END;
    RETURN -1;
END ImageListLoadImage;

PROCEDURE SetDisplayMode(w : Window; mode : DisplayModes);
CONST
    winMode     : ARRAY DisplayModes OF CARDINAL =
                    {
                     SW_SHOW,
                     SW_SHOWNA,
                     SW_HIDE,
                     SW_SHOWMINNOACTIVE,
                     SW_SHOWMAXIMIZED
                    };
BEGIN
    CASE w^.type OF
    ToplevelWindow:
        ShowWindowGen(w^.wnd, winMode[mode]);
        IF mode <> DisplayHidden THEN
            INCL(w^.attr, WA_VISIBLE);
        ELSE
            EXCL(w^.attr, WA_VISIBLE);
        END;

        (* this should be redundant but is harmless to leave it here *)
        GetWindowState(w);
    |
    ChildWindow:
        CASE mode OF
        DisplayNormal,
        DisplayMaximized:
            SetForegroundWindow(w);
            INCL(w^.attr, WA_VISIBLE);
        |
        DisplayVisible,
        DisplayMinimized:
            IF IsTabChild(w) THEN
                ShowTabChild(w);
            END;
            INCL(w^.attr, WA_VISIBLE);
        |
        DisplayHidden:
            IF IsTabChild(w) THEN
                HideTabChild(w);
            END;
            EXCL(w^.attr, WA_VISIBLE);
        END;
    END;
END SetDisplayMode;

PROCEDURE GetDisplayMode(w : Window) : DisplayModes;
VAR
    mode : DisplayModes;
BEGIN
    mode := DisplayNormal;
    IF IsWindow(w) THEN
        IF w^.state = WindowStateMaximized THEN
            mode := DisplayMaximized;
        ELSIF w^.state = WindowStateMinimized THEN
            mode := DisplayMinimized;
        ELSIF NOT IsWindowVisible(w^.wnd) THEN
            mode := DisplayHidden;
        END;
    END;
    RETURN mode;
END GetDisplayMode;

PROCEDURE SetWindowEnable(w : Window; enabled : BOOLEAN);
VAR
    wasDisabled : BOOLEAN;
BEGIN
    IF IsWindow(w) THEN
        IF enabled THEN
            IF w^.disable > 0 THEN
                DEC(w^.disable);
                IF w^.disable = 0 THEN
                    wasDisabled := EnableWindow(w^.wnd, TRUE);
                END;
            END;
        ELSE
            INC(w^.disable);
            IF w^.disable = 1 THEN
                wasDisabled := EnableWindow(w^.wnd, FALSE);
            END;
        END;
    ELSE
        w := FirstWindow;
        WHILE w <> NIL DO
            IF w^.type = ToplevelWindow THEN
                SetWindowEnable(w, enabled);
            END;
            w := w^.next;
        END;
    END;
END SetWindowEnable;

PROCEDURE IsMinimized(w : Window) : BOOLEAN;
BEGIN
    RETURN IsWindow(w) AND (w^.state = WindowStateMinimized);
END IsMinimized;

PROCEDURE IsMaximized(w : Window) : BOOLEAN;
BEGIN
    RETURN IsWindow(w) AND (w^.state = WindowStateMaximized);
END IsMaximized;

PROCEDURE IsVisible(w : Window) : BOOLEAN;
BEGIN
    RETURN IsWindow(w) AND WINUSER.IsWindowVisible(w^.wnd) AND (w^.state <> WindowStateMinimized);
END IsVisible;

PROCEDURE ClientAreaIsClipped(w : Window) : BOOLEAN;
VAR
    rect, rect1 : RECT;
    dc          : HDC;
    clipped     : BOOLEAN;
BEGIN
    dc := GetDC(w^.clientWnd);
    clipped := TRUE;
    IF GetClipBox(dc, rect) = SIMPLEREGION THEN
        GetClientRect(w^.clientWnd, rect1);

        IF (rect.left = rect1.left) AND
           (rect.right = rect1.right) AND
           (rect.top = rect1.top) AND
           (rect.bottom = rect1.bottom)
        THEN
            clipped := FALSE;
        END;
    END;
    ReleaseDC(w^.clientWnd, dc);
    RETURN clipped;
END ClientAreaIsClipped;

PROCEDURE BringWindowToTop(w : Window);
VAR
    top : Window;
BEGIN
    top := w;
    WHILE top^.type = ChildWindow DO
        top := top^.parent;
    END;
    IF IsCurrentThread(top^.wnd) THEN
    WINUSER.BringWindowToTop(top^.wnd);
    ELSE
        WINUSER.SetWindowPos(top^.wnd, HWND_TOP, 0, 0, 0, 0,
                             SWP_ASYNCWINDOWPOS BOR SWP_NOMOVE BOR SWP_NOSIZE);
    END;

    IF IsTabChild(w) THEN
        ShowTabChild(w);
        DoSetActiveTabChild(w, FALSE);
    END;
END BringWindowToTop;

PROCEDURE SetForegroundWindow(w : Window);
VAR
    top         : Window;
BEGIN
    top := w;
    WHILE top^.type <> ToplevelWindow DO
        top := top^.parent;
    END;
    WINUSER.SetForegroundWindow(top^.wnd);

    IF IsTabChild(w) THEN
        ShowTabChild(w);
        DoSetActiveTabChild(w, TRUE);
    ELSE
        SetFocus(w^.clientWnd);
    END;
END SetForegroundWindow;

PROCEDURE GetForegroundWindow() : Window;
VAR
    wnd         : HWND;
    w, w1       : Window;
    curSel      : INTEGER;
BEGIN
    (* anything focused is our toplevel *)

    w := FirstWindow;
    WHILE w <> NIL DO
        IF w^.hasFocus THEN
            RETURN w;
        END;
        w := w^.next;
    END;

    (* fallback and determine this from the system *)

    wnd := WINUSER.GetForegroundWindow();
    IF wnd <> NIL THEN
        w := FirstWindow;
        WHILE w <> NIL DO
            IF w^.wnd = wnd THEN
                IF w^.clientType = TabClient THEN
                    curSel := TabCtrl_GetCurSel(w^.clientWnd);
                    IF curSel >= 0 THEN
                        w1 := w^.firstChild;
                        WHILE w1 <> NIL DO
                            IF IsTabChild(w) AND (w^.tabIndex = curSel) THEN
                                RETURN w1;
                            END;
                            w1 := w1^.nextSibling;
                        END;
                    END;
                END;

                RETURN w;
            END;

            w := w^.next;
        END;
    END;
    RETURN NIL;
END GetForegroundWindow;

PROCEDURE RepaintRect(w : Window; rect : wsRECT);
VAR
    wr          : RECT;
BEGIN
    wr.left := rect.x1;
    wr.right := rect.x2;
    wr.top := rect.y1;
    wr.bottom := rect.y2;
    InvalidateRect(w^.clientWnd, wr, TRUE);
END RepaintRect;

PROCEDURE RepaintWindow(w : Window);
BEGIN
    InvalidateRect(w^.clientWnd, NIL_RECT, TRUE);
END RepaintWindow;

PROCEDURE UpdateWindow(w : Window);
BEGIN
    WINUSER.UpdateWindow(w^.clientWnd);
END UpdateWindow;

PROCEDURE SetScrollBarRange(w : Window; which : WinAttr; min : ScrollRange; max : ScrollRange; pageSize : ScrollRange);
VAR
    info        : SCROLLINFO;
BEGIN
    (*doMove := FALSE;*)

    IF (which = WA_VSCROLL) AND (WA_VSCROLL IN w^.attr) THEN
        IF (w^.vScrollMax <> max) OR
           (w^.vScrollMin <> min) OR
           (w^.vScrollPage <> pageSize)
        THEN
            w^.vScrollMin := min;
            w^.vScrollMax := max;
            w^.vScrollPage := pageSize;

            IF (min >= -32767) AND (max <= 32767) THEN
                w^.vScale := 1.0;
            ELSE
                w^.vScale := LFLOAT(max - min) / 32767.0;
                min := 0;
                max := 32767;
            END;

            info.cbSize := SIZE(info);
            info.fMask := SIF_RANGE BOR SIF_PAGE;
            IF w^.disableScrollV THEN
                info.fMask := info.fMask BOR SIF_DISABLENOSCROLL;
            END;
            info.nMin := min;
            info.nMax := max;
            info.nPage := pageSize;
            SetScrollInfo(w^.clientWnd, SB_VERT, info, TRUE);
        END;

    ELSIF (which = WA_HSCROLL) AND (WA_HSCROLL IN w^.attr) THEN
        IF (w^.hScrollMax <> max) OR
           (w^.hScrollMin <> min) OR
           (w^.hScrollPage <> pageSize)
        THEN
            w^.hScrollMin := min;
            w^.hScrollMax := max;
            w^.hScrollPage := pageSize;

            IF (min >= -32767) AND (max <= 32767) THEN
                w^.hScale := 1.0;
            ELSE
                w^.hScale := LFLOAT(max - min) / 32767.0;
                min := 0;
                max := 32767;
            END;

            info.cbSize := SIZE(info);
            info.fMask := SIF_RANGE BOR SIF_PAGE;
            IF w^.disableScrollH THEN
                info.fMask := info.fMask BOR SIF_DISABLENOSCROLL;
            END;
            info.nMin := min;
            info.nMax := max;
            info.nPage := pageSize;
            SetScrollInfo(w^.clientWnd, SB_HORZ, info, TRUE);
        END;
    END;
END SetScrollBarRange;

PROCEDURE SetScrollBarRanges(w : Window;
                             minX : ScrollRange;
                             maxX : ScrollRange;
                             pageX : ScrollRange;
                             minY : ScrollRange;
                             maxY : ScrollRange;
                             pageY : ScrollRange);
VAR
    info        : SCROLLINFO;
BEGIN
    IF WA_VSCROLL IN w^.attr THEN
        IF (w^.vScrollMax <> maxY) OR
           (w^.vScrollMin <> minY) OR
           (w^.vScrollPage <> pageY)
        THEN
            w^.vScrollMin := minY;
            w^.vScrollMax := maxY;
            w^.vScrollPage := pageY;

            IF (minY >= -32767) AND (maxY <= 32767) THEN
                w^.vScale := 1.0;
            ELSE
                w^.vScale := LFLOAT(maxY - minY) / 32767.0;
                minY := 0;
                maxY := 32767;
            END;

            info.cbSize := SIZE(info);
            info.fMask := SIF_RANGE BOR SIF_PAGE;
            IF w^.disableScrollV THEN
                info.fMask := info.fMask BOR SIF_DISABLENOSCROLL;
            END;
            info.nMin := minY;
            info.nMax := maxY;
            info.nPage := pageY;
            SetScrollInfo(w^.clientWnd, SB_VERT, info, TRUE);
        END;
    END;

    IF WA_HSCROLL IN w^.attr THEN
        IF (w^.hScrollMax <> maxX) OR
           (w^.hScrollMin <> minX) OR
           (w^.hScrollPage <> pageX)
        THEN
            w^.hScrollMin := minX;
            w^.hScrollMax := maxX;
            w^.hScrollPage := pageX;

            IF (minX >= -32767) AND (maxX <= 32767) THEN
                w^.hScale := 1.0;
            ELSE
                w^.hScale := LFLOAT(maxX - minX) / 32767.0;
                minX := 0;
                maxX := 32767;
            END;

            info.cbSize := SIZE(info);
            info.fMask := SIF_RANGE BOR SIF_PAGE;
            IF w^.disableScrollH THEN
                info.fMask := info.fMask BOR SIF_DISABLENOSCROLL;
            END;
            info.nMin := minX;
            info.nMax := maxX;
            info.nPage := pageX;
            SetScrollInfo(w^.clientWnd, SB_HORZ, info, TRUE);
        END;
    END;
END SetScrollBarRanges;

PROCEDURE SetScrollBarPos(w : Window; which : WinAttr; pos : ScrollRange);
VAR
    disp        : ScrollRange;
    info        : SCROLLINFO;
BEGIN
    disp := pos;

    IF (which = WA_VSCROLL) AND (WA_VSCROLL IN w^.attr) THEN
        IF disp < w^.vScrollMin THEN
            disp := w^.vScrollMin;
        ELSIF disp > w^.vScrollMax THEN
            disp := w^.vScrollMax;
        END;
        w^.vScrollPos := disp;

        IF w^.vScale <> 1.0 THEN
            (* transform to a zero based range and scale down *)

            disp := VAL(ScrollRange, FLOAT(disp - w^.vScrollMin) / w^.vScale);
        END;

        info.cbSize := SIZE(info);
        info.fMask := SIF_POS;
        info.nPos := disp;
        SetScrollInfo(w^.clientWnd, SB_VERT, info, TRUE);

    ELSIF (which = WA_HSCROLL) AND (WA_HSCROLL IN w^.attr) THEN
        IF disp < w^.hScrollMin THEN
            disp := w^.hScrollMin;
        ELSIF disp > w^.hScrollMax THEN
            disp := w^.hScrollMax;
        END;
        w^.hScrollPos := disp;

        IF w^.hScale <> 1.0 THEN
            (* transform to a zero based range and scale down *)

            disp := VAL(ScrollRange, FLOAT(disp - w^.hScrollMin) / w^.hScale);
        END;

        info.cbSize := SIZE(info);
        info.fMask := SIF_POS;
        info.nPos := disp;
        SetScrollInfo(w^.clientWnd, SB_HORZ, info, TRUE);
    ELSE
        RETURN;
    END;
END SetScrollBarPos;

PROCEDURE GetScrollBarPos(w : Window; which : WinAttr) : ScrollRange;
BEGIN
    IF (which = WA_HSCROLL) AND (WA_HSCROLL IN w^.attr) THEN
        RETURN w^.hScrollPos;
    ELSIF (which = WA_VSCROLL) AND (WA_VSCROLL IN w^.attr) THEN
        RETURN w^.vScrollPos;
    END;
    RETURN 0;
END GetScrollBarPos;

PROCEDURE SetScrollDisableWhenNone(w : Window; yesH, yesV : BOOLEAN);
BEGIN
    w^.disableScrollH := yesH;
    w^.disableScrollV := yesV;
END SetScrollDisableWhenNone;

PROCEDURE SetWindowData(w : Window; index : ADRCARD; data : MACHINEWORD) : BOOLEAN;
BEGIN
    IF index <= MaxUserData THEN
        w^.userData[index] := data;
        RETURN TRUE;
    END;
    RETURN FALSE;
END SetWindowData;

PROCEDURE SetWindowDataNum(w : Window; index : CARDINAL; data : INTEGER) : BOOLEAN;
BEGIN
    RETURN SetWindowData(w, index, MAKEADR(data));
END SetWindowDataNum;

PROCEDURE GetWindowData(w : Window; index : ADRCARD) : MACHINEWORD;
VAR
    data        : MACHINEWORD;
BEGIN
    data := 0;
    IF index <= MaxUserData THEN
        data := w^.userData[index];
    END;
    RETURN data;
END GetWindowData;

PROCEDURE GetWindowDataNum(w : Window; index : CARDINAL) : INTEGER;
BEGIN
    RETURN CAST(ADRINT, GetWindowData(w, index));
END GetWindowDataNum;

PROCEDURE ShowWindowCursor(w : Window);
VAR
    pt          : POINT;
BEGIN
    (* move the cursor one pixel to force it to be drawn.
       move it back so we do not alter the cursor position.
    *)
    UNREFERENCED_PARAMETER(w);
    WINUSER.GetCursorPos(pt);
    WINUSER.SetCursorPos(pt.x+1, pt.y+1);
    WINUSER.SetCursorPos(pt.x, pt.y);
END ShowWindowCursor;

PROCEDURE SetWindowIsBusy(w : Window; busy : BOOLEAN);
BEGIN
    IF busy THEN
        IF w^.busy = 0 THEN
            w^.busySave := w^.cursor;
            w^.cursor := LoadCursor(NIL, IDC_WAIT^);
            ShowWindowCursor(w);
        END;
        INC(w^.busy);
    ELSE
        IF w^.busy > 0 THEN
            DEC(w^.busy);

            IF w^.busy = 0 THEN
                w^.cursor := w^.busySave;
                w^.busySave := NIL;
                ShowWindowCursor(w);
            END;
        END;
    END;
END SetWindowIsBusy;

PROCEDURE CreateWindowCaret(w : Window);
TYPE
    ByteSet     = PACKEDSET OF [0..7];
VAR
    bits        : POINTER TO ARRAY [0..0] OF ByteSet;
    bitsSize    : CARDINAL;
    bound       : COORDINATE;
    x, y        : COORDINATE;

    PROCEDURE setBit(x, y : CARDINAL);
    VAR
        bytes   : CARDINAL;
    BEGIN
        bytes := ((w^.caretSize.x + 15) / 16) * 2;
        INCL(bits^[y * bytes + (x / 8)], ((x REM 8) BXOR 7h) BAND 7h);
    END setBit;

BEGIN
    IF w^.hasFocus THEN
        IF w^.caretBmp = NIL THEN
            bitsSize := (w^.caretSize.x + 15) / 16;
            bitsSize := bitsSize * ORD(w^.caretSize.y);
            bitsSize := bitsSize * 2;
            ALLOCATE(bits, bitsSize);
            CASE w^.caretType OF
            CtVerticalBar:
                ZeroMem(bits, bitsSize);
                FOR x := 0 TO 1 DO
                    FOR y := 0 TO w^.caretSize.y-1 DO
                        setBit(x, y);
                    END;
                END;
            |
            CtHorizontalBar:
                ZeroMem(bits, bitsSize);
                FOR x := 0 TO w^.caretSize.x-1 DO
                    bound := w^.caretSize.y-2;
                    IF bound < 0 THEN
                        bound := 0;
                    END;
                    FOR y := bound TO w^.caretSize.y-1 DO
                        setBit(x, y);
                    END;
                END;
            |
            CtHalfBlock:
                ZeroMem(bits, bitsSize);
                FOR x := 0 TO w^.caretSize.x-1 DO
                    FOR y := ((w^.caretSize.y / 2) +1) TO w^.caretSize.y-1 DO
                        setBit(x, y);
                    END;
                END;
            |
            CtFullBlock:
                FillMemBYTE(bits, bitsSize, 0FFh);
            END;
            w^.caretBmp := WINGDI.CreateBitmap(w^.caretSize.x, w^.caretSize.y, 1, 1, bits);
            DEALLOCATE(bits, bitsSize);
        END;

        IF w^.caretBmp <> NIL THEN
            w^.caretCreated := TRUE;
            WINUSER.CreateCaret(w^.clientWnd, w^.caretBmp, 0, 0);
            SetCaretPos(w^.caretPos.x, w^.caretPos.y);

            IF w^.hideCaret = 0 THEN
                WINUSER.ShowCaret(w^.clientWnd);
            END;
        ELSE
            w^.hasCaret := FALSE;
        END;
    END;
END CreateWindowCaret;

PROCEDURE CaretOn(w : Window);
BEGIN
    IF w^.clientType = DrawClient THEN
        IF NOT w^.hasCaret THEN
            w^.hasCaret := TRUE;
            w^.hideCaret := 0;

            IF w^.hasFocus THEN
                CreateWindowCaret(w);
            END;
        END;
    END;
END CaretOn;

PROCEDURE CaretOff(w : Window);
BEGIN
    IF w^.hasCaret AND w^.caretCreated THEN
        DestroyCaret();
        w^.caretCreated := FALSE;

        IF w^.caretBmp <> NIL THEN
            WINX.DeleteBitmap(w^.caretBmp);
            w^.caretBmp := NIL;
        END;
    END;

    w^.hasCaret := FALSE;
END CaretOff;

PROCEDURE HideCaret(w : Window);
BEGIN
    IF w^.hasCaret THEN
        IF w^.hideCaret = 0 THEN
            IF w^.hasFocus THEN
                WINUSER.HideCaret(w^.clientWnd);
            END;
        END;
        INC(w^.hideCaret);
    END;
END HideCaret;

PROCEDURE ShowCaret(w : Window);
BEGIN
    IF w^.hasCaret THEN
        IF w^.hideCaret > 0 THEN
            DEC(w^.hideCaret);
            IF w^.hideCaret = 0 THEN
                IF w^.hasFocus THEN
                    WINUSER.ShowCaret(w^.clientWnd);
                END;
            END;
        END;
    END;
END ShowCaret;

PROCEDURE MoveCaretTo(w : Window; x, y : INTEGER);
BEGIN
    w^.caretPos.x := x;
    w^.caretPos.y := y;

    IF w^.hasFocus AND w^.hasCaret THEN
        SetCaretPos(x, y);
    END;
END MoveCaretTo;

PROCEDURE GetCaretPos(w : Window; VAR OUT x, y : COORDINATE);
BEGIN
    x := w^.caretPos.x;
    y := w^.caretPos.y;
END GetCaretPos;

PROCEDURE SetCaretType(w : Window; ct : CaretTypes; width, height : CARDINAL);
BEGIN
    w^.caretType := ct;
    w^.caretSize.x := width;
    w^.caretSize.y := height;
    IF w^.caretSize.x = 0 THEN
        INC(w^.caretSize.x);
    END;
    IF w^.caretSize.y = 0 THEN
        INC(w^.caretSize.y);
    END;

    IF w^.hasCaret THEN
        IF w^.hasFocus THEN
            CaretOff(w);
        END;

        IF w^.caretBmp <> NIL THEN
            WINX.DeleteBitmap(w^.caretBmp);
            w^.caretBmp := NIL;
        END;

        IF w^.hasFocus THEN
            CaretOn(w);
        END;
    END;
END SetCaretType;

PROCEDURE GetClientSize(w : Window; VAR OUT width, height : COORDINATE);
VAR
    r           : RECT;
BEGIN
    GetClientRect(w^.clientWnd, r);
    width := r.right;
    height := r.bottom;
END GetClientSize;

PROCEDURE SetClientSize(w : Window; width, height : COORDINATE);
VAR
    wRect       : RECT;
    diff        : wsPOINT;
    tl          : POINT;
BEGIN
    IF w^.type = ToplevelWindow THEN
        IF (width > 0) AND (height > 0) THEN
            GetWindowDelta(w, diff);

            GetWindowRect(w^.wnd, wRect);
            tl.x := wRect.left;
            tl.y := wRect.top;

            IF width < w^.minW THEN
                width := w^.minW;
            ELSIF width > w^.maxW THEN
                width := w^.maxW;
            END;

            IF height < w^.minH THEN
                height := w^.minH;
            ELSIF height > w^.maxH THEN
                height := w^.maxH;
            END;

            MoveWindow(w^.wnd, tl.x, tl.y, width + diff.x, height + diff.y, TRUE);
        END;
    END;
END SetClientSize;

PROCEDURE SetMaxClientSize(w : Window; width, height : COORDINATE);
VAR
    wRect       : RECT;
BEGIN
    IF (width > 0) AND (height > 0) THEN
        w^.maxW := width;
        w^.maxH := height;

        IF w^.type = ToplevelWindow THEN
            GetClientRect(w^.clientWnd, wRect);
            IF (width < (wRect.right - wRect.left)) OR
               (height < (wRect.bottom - wRect.top))
            THEN
                (* the GETMINMAXINFO message will handle the sizing *)

                GetWindowRect(w^.wnd, wRect);
                MoveWindow(w^.wnd, wRect.left, wRect.top, wRect.right - wRect.left, wRect.bottom - wRect.top, TRUE);
            END;
        END;
    END;
END SetMaxClientSize;

PROCEDURE SetMinClientSize(w : Window; width, height : COORDINATE);
VAR
    wRect       : RECT;
BEGIN
    IF (width > 0) AND (height > 0) THEN
        w^.minW := width;
        w^.minH := height;

        IF w^.type = ToplevelWindow THEN
            GetClientRect(w^.clientWnd, wRect);
            IF (width > (wRect.right - wRect.left)) OR
               (height > (wRect.bottom - wRect.top))
            THEN
                (* the GETMINMAXINFO message will handle the sizing *)

                GetWindowRect(w^.wnd, wRect);
                MoveWindow(w^.wnd, wRect.left, wRect.top, wRect.right - wRect.left, wRect.bottom - wRect.top, TRUE);
            END;
        END;
    END;
END SetMinClientSize;

PROCEDURE SetWindowGrains(w : Window; width, height : COORDINATE);
VAR
    wRect       : RECT;
BEGIN
    IF (width > 0) AND (height > 0) THEN
        w^.vGrain := height;
        IF w^.vGrain <= 0 THEN
            w^.vGrain := 1;
        END;

        w^.hGrain := width;
        IF w^.hGrain <= 0 THEN
            w^.hGrain := 1;
        END;

        IF w^.type = ToplevelWindow THEN
            IF (w^.vGrain > 1) OR (w^.hGrain > 1) THEN
                (* the GETMINMAXINFO message will handle the sizing *)

                GetWindowRect(w^.wnd, wRect);
                MoveWindow(w^.wnd, wRect.left, wRect.top, wRect.right - wRect.left, wRect.bottom - wRect.top, TRUE);
            END;
        END;
    END;
END SetWindowGrains;

PROCEDURE IsWindowThreadCurrent(w : Window) : BOOLEAN;
BEGIN
    RETURN IsCurrentThread(w^.wnd);
END IsWindowThreadCurrent;

PROCEDURE GetWindowSize(w : Window; VAR OUT width, height : COORDINATE);
VAR
    wRect       : RECT;
BEGIN
    GetWindowRect(w^.wnd, wRect);
    width := wRect.right - wRect.left;
    height := wRect.bottom - wRect.top;
END GetWindowSize;

PROCEDURE SetWindowSize(w : Window; width, height : COORDINATE);
VAR
    wRect       : RECT;
    tl          : POINT;
BEGIN
    IF w^.type = ToplevelWindow THEN
        IF (width > 0) AND (height > 0) THEN
            GetWindowRect(w^.wnd, wRect);
            tl.x := wRect.left;
            tl.y := wRect.top;

            MoveWindow(w^.wnd, tl.x, tl.y, width, height, TRUE);
        END;
    END;
END SetWindowSize;

PROCEDURE GetWindowPos(w : Window; VAR OUT x, y : COORDINATE);
VAR
    wRect       : RECT;
    tl          : POINT;
BEGIN
    x := 0;
    y := 0;
    IF w^.type = ToplevelWindow THEN
        GetWindowRect(w^.wnd, wRect);
        tl.x := wRect.left;
        tl.y := wRect.top;
        x := tl.x;
        y := tl.y;
    END;
END GetWindowPos;

PROCEDURE SetWindowPos(w : Window; x, y : COORDINATE);
VAR
    wRect       : RECT;
BEGIN
    IF w^.type = ToplevelWindow THEN
        GetWindowRect(w^.wnd, wRect);

        MoveWindow(w^.wnd, x, y, wRect.right - wRect.left, wRect.bottom - wRect.top, TRUE);
    END;
END SetWindowPos;

PROCEDURE SetWindowPosition(w : Window; pos : WindowPositions);
VAR
    wndRect     : RECT;
    resRect     : RECT;
    scrRect     : RECT;
    pt          : POINT;
    base        : POINT;
    scrBase     : POINT;
    wndWidth    : COORDINATE;
    wndHeight   : COORDINATE;
    scrX, scrY  : COORDINATE;
    owner       : HWND;
BEGIN
    IF w^.type = ToplevelWindow THEN
        owner := NIL;
        IF (pos = CenterOnParent) AND (w^.parent <> NIL) THEN
            owner := w^.parent^.wnd;
        END;
        IF owner = NIL THEN
            owner := GetDesktopWindow();
        ELSE
            IF IsIconic(owner) THEN
                RETURN;
            END;
        END;

        (* center over the client area if possible *)

        GetClientRect(owner, resRect);
        base.x := resRect.left;
        base.y := resRect.top;
        scrBase := base;
        WINUSER.ClientToScreen(owner, scrBase);
        base := scrBase;

        resRect.left := scrBase.x;
        resRect.top := scrBase.y;
        pt.x := resRect.right;
        pt.y := resRect.bottom;
        WINUSER.ClientToScreen(owner, pt);
        resRect.right := pt.x;
        resRect.bottom := pt.y;

        GetWindowRect(w^.wnd, wndRect);
        wndWidth := wndRect.right - wndRect.left;
        wndHeight := wndRect.bottom - wndRect.top;

        (* Offset the result and window rectangles so that the *)
        (* right and bottom values represent the width and height, *)
        (* and then offset the result again to discard space taken up *)
        (* by the window *)

        WINUSER.OffsetRect(wndRect, -wndRect.left, -wndRect.top);
        WINUSER.OffsetRect(resRect, -resRect.left, -resRect.top);
        WINUSER.OffsetRect(resRect, -wndRect.right, -wndRect.bottom);

        (* force beginning at top of client area *)
        (* happens when dialog is larger than client area *)

        IF resRect.right < 0 THEN
            resRect.right := 0;
        END;
        IF resRect.bottom < 0 THEN
            resRect.bottom := 0;
        END;

        (* set new rectangle *)

        resRect.left := base.x + (resRect.right / 2);
        resRect.top := base.y + (resRect.bottom / 2);
        resRect.right := resRect.left + wndWidth-1;
        resRect.bottom := resRect.top + wndHeight-1;

        (* make sure it is all visible on screen *)

        scrX := GetSystemMetrics(SM_CXFULLSCREEN);
        scrY := GetSystemMetrics(SM_CYFULLSCREEN);
        scrRect := resRect;
        IF scrRect.right >= scrX THEN
            WINUSER.OffsetRect(resRect, -(scrRect.right - scrX), 0);
        END;
        IF scrRect.bottom >= scrY THEN
            WINUSER.OffsetRect(resRect,  0, -(scrRect.bottom - scrY));
        END;

        WINUSER.SetWindowPos(w^.wnd,
                             WINX.NULL_HWND,
                             resRect.left, resRect.top,
                             0, 0,
                             SWP_NOSIZE BOR SWP_NOZORDER BOR SWP_NOACTIVATE);
    END;
END SetWindowPosition;

PROCEDURE SetWindowPosClient(w, parent : Window; x, y : COORDINATE);
VAR
    wRect               : RECT;
    pt                  : POINT;
BEGIN
    IF w^.type = ToplevelWindow THEN
        pt.x := x;
        pt.y := y;
        WINUSER.ClientToScreen(parent^.clientWnd, pt);

        GetWindowRect(w^.wnd, wRect);

        MoveWindow(w^.wnd, pt.x + x, pt.y + y, wRect.right - wRect.left, wRect.bottom - wRect.top, TRUE);
    END;
END SetWindowPosClient;

PROCEDURE GetWindowDisplayInfo(w : Window; VAR OUT info : WindowDisplayInfo);
VAR
    place       : WINDOWPLACEMENT;
    rect        : RECT;
BEGIN
    IF w^.type = ToplevelWindow THEN
        place.length := SIZE(place);
        GetWindowPlacement(w^.wnd, place);

        info.x := place.rcNormalPosition.left;
        info.y := place.rcNormalPosition.top;

        info.width := place.rcNormalPosition.right - place.rcNormalPosition.left;
        info.height := place.rcNormalPosition.bottom - place.rcNormalPosition.top;

        CASE place.showCmd OF
        SW_SHOWMINIMIZED, SW_MINIMIZE, SW_SHOWMINNOACTIVE:
            info.mode := DisplayMinimized;
        |
        SW_SHOWMAXIMIZED:
            info.mode := DisplayMaximized;
        |
        SW_HIDE:
            info.mode := DisplayHidden;
        ELSE
            info.mode := DisplayNormal;
        END;
    ELSE
        GetWindowRect(w^.wnd, rect);
        info.x := 0;
        info.y := 0;
        info.width := rect.right-rect.left;
        info.height := rect.bottom-rect.top;
        info.mode := DisplayNormal;
        IF IsTabChild(w) AND (w^.tabIndex < 0) THEN
            info.mode := DisplayHidden;
        END;
    END;
END GetWindowDisplayInfo;

PROCEDURE SetWindowDisplayInfo(w : Window; info : WindowDisplayInfo);
VAR
    place       : WINDOWPLACEMENT;
BEGIN
    IF w^.type = ToplevelWindow THEN
        place.length := SIZE(place);
        GetWindowPlacement(w^.wnd, place);

        INCL(w^.attr, WA_VISIBLE);
        CASE info.mode OF
        DisplayNormal:
            place.showCmd := SW_SHOW;
        |
        DisplayVisible:
            place.showCmd := SW_SHOWNA;
        |
        DisplayHidden:
            EXCL(w^.attr, WA_VISIBLE);
            place.showCmd := SW_HIDE;
        |
        DisplayMaximized:
            place.showCmd := SW_SHOWMAXIMIZED;
        |
        DisplayMinimized:
            place.showCmd := SW_SHOWMINNOACTIVE;
        END;

        place.flags := 0;
        place.rcNormalPosition.left := info.x;
        place.rcNormalPosition.top := info.y;
        place.rcNormalPosition.right := info.x + INT(info.width);
        place.rcNormalPosition.bottom := info.y + INT(info.height);

        SetWindowPlacement(w^.wnd, place);

        (* this should be redundant but is harmless to leave it here *)
        GetWindowState(w);
    ELSE
        SetDisplayMode(w, info.mode);
    END;
END SetWindowDisplayInfo;

PROCEDURE CascadeWindow(cascadeThis, onThis : Window);
VAR
    destX, destY,
    srcX, srcY  : COORDINATE;
    caption     : INTEGER;
    border      : INTEGER;
    button      : INTEGER;
BEGIN
    IF (cascadeThis^.type = ToplevelWindow) AND
       (onThis^.type = ToplevelWindow)
    THEN
        GetWindowPos(cascadeThis, destX, destY);
        GetWindowPos(onThis, srcX, srcY);

        caption := GetSystemMetrics(SM_CYCAPTION);
        border := GetSystemMetrics(SM_CYSIZEFRAME);
        button := GetSystemMetrics(SM_CXSIZE);

        destY := srcY + border + caption;
        destX := srcX + border + button;
        SetWindowPos(cascadeThis, destX, destY);
    END;
END CascadeWindow;

PROCEDURE CreateStatusLine(w : Window; fmt : ARRAY OF INTEGER) : BOOLEAN;
VAR
    dc          : HDC;
    size        : WIN32.WSIZE;
    max         : CARDINAL;
    i           : ADRCARD;
    len         : ADRCARD;
BEGIN
    IF HIGH(fmt) < MaxFields THEN
        NEW(w^.statusbar);
        IF w^.statusbar <> NIL THEN
            w^.statusbar^.wnd := CreateStatusWindow(WS_CHILD BOR WS_CLIPSIBLINGS BOR WS_VISIBLE,
                                                    "",
                                                    w^.wnd,
                                                    STATUSLINE_ID);
            IF w^.statusbar^.wnd <> NIL THEN
                WINUSER.SendMessage(w^.statusbar^.wnd, WM_SETFONT, CAST(WPARAM, StatuslineFont), 1);

                INCL(w^.attr, WA_STATUSLINE);

                w^.statusbar^.numFields := 1;
                w^.statusbar^.fields[0] := 255;

                dc := GetDC(w^.statusbar^.wnd);

                w^.statusbar^.charWidth := ComputeAverageCharWidth(dc);
                max := 0;
                len := LENGTH(Alphabet);
                FOR i := 0 TO len-1 DO
                    WINGDI.GetTextExtentPoint32(dc, Alphabet[i], 1, size);
                    IF ORD(size.cx) > max THEN
                        max := size.cx;
                    END;
                END;
                w^.statusbar^.charWidth := (w^.statusbar^.charWidth + max) / 2;

                ReleaseDC(w^.statusbar^.wnd, dc);

                SetStatusFormat(w, fmt);
                RETURN TRUE;
            ELSE
                DISPOSE(w^.statusbar);
                EXCL(w^.attr, WA_STATUSLINE);
            END;
        END;
    END;
    RETURN FALSE;
END CreateStatusLine;

PROCEDURE RemoveStatusLine(w : Window);
BEGIN
    IF w^.statusbar <> NIL THEN
        DestroyWindow(w^.statusbar^.wnd);
        EXCL(w^.attr, WA_STATUSLINE);
        DISPOSE(w^.statusbar);
    END;
END RemoveStatusLine;

PROCEDURE SetStatusFormat(w : Window; fmt : ARRAY OF INTEGER);
VAR
    i           : ADRCARD;
    highFmt     : ADRCARD;
    pixel       : INTEGER;
    widths      : ARRAY [0..MaxFields-1] OF INTEGER;
BEGIN
    IF (w^.type = ChildWindow) AND
       (w^.parent^.type = ToplevelWindow) AND
       (w^.parent^.clientType = TabClient)
    THEN
        w := w^.parent;
    END;

    IF (w^.statusbar <> NIL) AND (HIGH(fmt)+1 <= MaxFields) THEN

        w^.statusbar^.fields := fmt;
        widths[0] := -1;

        pixel := 0;
        i := 0;
        highFmt := HIGH(fmt);
        LOOP
            IF i <= highFmt THEN
                IF fmt[i] >= 0 THEN
                    pixel := pixel + (fmt[i] * INT(w^.statusbar^.charWidth));
                    widths[i] := pixel;
                    INC(i);
                ELSE
                    widths[i] := -1;
                    INC(i);
                    EXIT;
                END;
            ELSE
                EXIT;
            END;
        END;

        w^.statusbar^.numFields := i;
        WINUSER.SendMessage(w^.statusbar^.wnd, SB_SETPARTS, i, CAST(LPARAM, ADR(widths)));
    END;
END SetStatusFormat;

PROCEDURE WriteStatusField(w : Window;
                           field : CARDINAL;
                           txt : ARRAY OF CHAR);
BEGIN
    IF (w^.type = ChildWindow) AND
       (w^.parent^.type = ToplevelWindow) AND
       (w^.parent^.clientType = TabClient)
    THEN
        w := w^.parent;
    END;

    IF w^.statusbar <> NIL THEN
        WINUSER.SendMessage(w^.statusbar^.wnd, SB_SETTEXT, field, CAST(LPARAM, ADR(txt)));
    END;
END WriteStatusField;

PROCEDURE InitGdiplus() : BOOLEAN;

    PROCEDURE getProcAddr(name : ARRAY OF ACHAR; VAR OUT addr : ADDRESS);
    BEGIN
        addr := CAST(ADDRESS, GetProcAddress(Gdip.dll, name));
        IF addr = NIL THEN
            <*/PUSH/CHECK*>
            addr^ := 0;(*just cause an exception*)
            <*/POP*>
        END;
    END getProcAddr;

BEGIN
    IF NOT Gdip.init THEN
        (* dynamically load GDI+ so that code using this module does not require
           GDI+ to be installed unless actually used.
        *)
        Gdip.init := TRUE;
        Gdip.dll := LoadLibrary("Gdiplus.dll");
        IF Gdip.dll <> NIL THEN
            getProcAddr("GdiplusStartup", Gdip.GdiplusStartup:ADDRESS);

            Gdip.startupInput.GdiplusVersion := 1;
            Gdip.startupInput.DebugEventCallback:ADDRESS := NIL;
            Gdip.startupInput.SuppressBackgroundThread := FALSE;
            Gdip.startupInput.SuppressExternalCodecs := FALSE;
            IF Gdip.GdiplusStartup(Gdip.token, Gdip.startupInput, Gdip.startupOutput) = Ok THEN
                getProcAddr("GdiplusShutdown", Gdip.GdiplusShutdown:ADDRESS);
                getProcAddr("GdipCreateBitmapFromFile", Gdip.GdipCreateBitmapFromFile:ADDRESS);
                getProcAddr("GdipCreateBitmapFromHBITMAP", Gdip.GdipCreateBitmapFromHBITMAP:ADDRESS);
                getProcAddr("GdipCreateHBITMAPFromBitmap", Gdip.GdipCreateHBITMAPFromBitmap:ADDRESS);
                getProcAddr("GdipCreateBitmapFromGdiDib", Gdip.GdipCreateBitmapFromGdiDib:ADDRESS);

                getProcAddr("GdipDisposeImage", Gdip.GdipDisposeImage:ADDRESS);
                getProcAddr("GdipCreateFromHDC", Gdip.GdipCreateFromHDC:ADDRESS);
                getProcAddr("GdipDeleteGraphics", Gdip.GdipDeleteGraphics:ADDRESS);
                getProcAddr("GdipSetInterpolationMode", Gdip.GdipSetInterpolationMode:ADDRESS);
                getProcAddr("GdipDrawImageRectRectI", Gdip.GdipDrawImageRectRectI:ADDRESS);

                (*getProcAddr("", Gdip.:ADDRESS);*)
            ELSE
                FreeLibrary(Gdip.dll);
                Gdip.dll := NIL;
            END;
        END;
    END;

    RETURN Gdip.dll <> NIL;

EXCEPT
    FreeLibrary(Gdip.dll);
    Gdip.dll := NIL;
    RETURN FALSE;
END InitGdiplus;

TYPE
    ColorMapRecord =
        RECORD
        from    : ColorValue;
        to      : ColorValue;
        END;
CONST
    NullColorMap = ColorMapRecord{Colors[TransparentColor], Colors[TransparentColor]};

PROCEDURE LoadGdiplusBitmapFromFile(name : ARRAY OF CHAR; map : ColorMapRecord) : HBITMAP;
VAR
    bmp         : HBITMAP;
    bmpGdip     : GpBitmap;
    argb        : ARGB;
    r, g, b, a  : CARDINAL8;
    %IF %NOT Unicode %THEN
        len     : ADRCARD;
    %END
    specW       : ARRAY [0..WIN32.MAX_PATH] OF UCHAR;
BEGIN
    ColorValueToRgba(map.to, r, g, b, a);
    argb := MakeARGB(r, g, b, a);

    bmpGdip := NIL;
    bmp := NIL;

    IF InitGdiplus() THEN
        %IF Unicode %THEN
            specW := name;
        %ELSE
            len := LENGTH(name);
            IF MultiByteToWideChar(CP_ACP, 0, name, len, specW, HIGH(specW)+1) = 0 THEN
                specW[0] := ''
            END;
            IF len <= HIGH(specW) THEN
                specW[len] := '';
            END;
        %END
        specW[HIGH(specW)] := '';

        IF Gdip.GdipCreateBitmapFromFile(specW, bmpGdip) = Ok THEN
            Gdip.GdipCreateHBITMAPFromBitmap(bmpGdip, bmp, argb);
            Gdip.GdipDisposeImage(bmpGdip);
        END;
    END;

    RETURN bmp;

EXCEPT
    IF bmpGdip <> NIL THEN
        Gdip.GdipDisposeImage(bmpGdip);
    END;
    RETURN NIL;
END LoadGdiplusBitmapFromFile;

PROCEDURE InvertRGB(rgb : DWORD) : DWORD [INVARIANT];
(*
* Reverses the RGB order of a color.  This needs to be done to match
* the resource file format of the color table.
*)
BEGIN
    RETURN RGB(GetBValue(rgb), GetGValue(rgb), GetRValue(rgb));
END InvertRGB;

PROCEDURE LoadMappedBitmap(name : ARRAY OF CHAR; map : ColorMapRecord) : HBITMAP;
VAR
    i           : CARDINAL;
    bmpPtr      : LPBITMAPINFOHEADER;
    hdcScreen   : HDC;
    pClr        : POINTER TO DWORD;
    pData24     : POINTER TO RGBTRIPLE;
    pData24To   : POINTER TO RGBQUAD;
    bits        : ADDRESS;
    hbmp        : HBITMAP;
    hresFind    : HRSRC;
    hres        : HRSRC;
    data        : ADDRESS;
    amount      : CARDINAL;
BEGIN
    hresFind := MyFindResource(ResourceInst, name,  RT_BITMAP^);
    IF hresFind = NIL THEN
        RETURN NIL;
    END;

    hres := LoadResource(ResourceInst, hresFind);
    IF hres = NIL THEN
        RETURN NIL;
    END;

    amount := SizeofResource(ResourceInst, hresFind);
    bmpPtr := CAST(LPBITMAPINFOHEADER, LockResource(hres));

    ALLOCATE(data, amount);
    MoveMem(data, bmpPtr, amount);

    FreeResource(hres);

    bmpPtr := data;
    pClr := ADDADR(bmpPtr, bmpPtr^.biSize);

    (* walk the color table converting colors *)

    IF bmpPtr^.biBitCount < 24 THEN
        i := 0;
        WHILE i < (ORD(1) SHL bmpPtr^.biBitCount) DO
            IF map.from <> Colors[TransparentColor] THEN
                IF pClr^ = InvertRGB(map.from) THEN
                    pClr^ := InvertRGB(map.to);
                END;
            END;

            INC(i);
            pClr := ADDADR(pClr, SIZE(pClr^));
        END;
    END;

    (*
     * First skip over the header structure.
     *)
    bits := ADDADR(bmpPtr, SIZE(bmpPtr^));

    (*
     * Skip the color table entries, IF any.
     *)
    IF bmpPtr^.biBitCount < 24 THEN
        bits := ADDADR(bits, (ORD(1) SHL bmpPtr^.biBitCount) * SIZE(RGBQUAD));
    ELSE
        (* walk the data converting colors *)
        i := 0;
        pData24 := bits;
        pData24To := ADR(map.to);
        WHILE i < bmpPtr^.biSizeImage DO
            (*IF (pData24^[0] = 0F0H) AND (pData24^[0] = 0F0H) AND (pData24^[0] = 0F0H) THEN*)
            IF  (pData24^.rgbtBlue = Transparent24.rgbtBlue) AND
                (pData24^.rgbtGreen = Transparent24.rgbtGreen) AND
                (pData24^.rgbtRed = Transparent24.rgbtRed)
            THEN
                pData24^.rgbtBlue := pData24To^.rgbBlue;
                pData24^.rgbtGreen := pData24To^.rgbGreen;
                pData24^.rgbtRed := pData24To^.rgbRed;
            END;
            INC(i, 3);
            pData24 := ADDADR(pData24, SIZE(RGBTRIPLE));
        END;
    END;

    (*
     * Create a color bitmap compatible with the display device.
     *)
    hbmp := NIL;
    hdcScreen := GetDC(NIL);
    IF hdcScreen <> NIL THEN
        hbmp := CreateDIBitmap(hdcScreen,
                               bmpPtr^,
                               CBM_INIT,
                               bits,
                               CAST(LPBITMAPINFO, bmpPtr)^,
                               DIB_RGB_COLORS);

        ReleaseDC(NIL, hdcScreen);
    END;

    DEALLOCATE(data, amount);

    RETURN hbmp;
END LoadMappedBitmap;

PROCEDURE LoadResBitmap(name : ARRAY OF CHAR; map : ColorMapRecord) : HBITMAP;
CONST
    resTypes    : ARRAY [0..3] OF ARRAY [0..7] OF CHAR =
        {"JPEG", "PNG", "GIF", "TIFF"};
VAR
    bmp         : HBITMAP;
    i           : ADRCARD;
    hresFind    : HRSRC;
    hres        : HRSRC;
    data        : ADDRESS;
    amount      : CARDINAL;
    f           : File;
    tempSpec    : FileSpecString;
    resName     : ARRAY [0..63] OF CHAR;
    resId       : CARDINAL;
    idStr       : LPTSTR;
BEGIN
    IF map.from = Colors[TransparentColor] THEN
        bmp := MyLoadBitmap(ResourceInst, name);
    ELSE
        bmp := LoadMappedBitmap(name, map);
    END;
    IF bmp = NIL THEN
        hresFind := NIL;
        GetResNameAndId(name, resName, resId);
        i := 0;
        IF resName[0] <> '' THEN
            i := 0;
            WHILE (i <= HIGH(resTypes)) AND (hresFind = NIL) DO
                hresFind := FindResource(ResourceInst, name, resTypes[i]);
                INC(i);
            END;
        END;
        IF (hresFind = NIL) AND (resId <> 0) THEN
            idStr := WINUSER.MAKEINTRESOURCE(resId);
            i := 0;
            WHILE (i <= HIGH(resTypes)) AND (hresFind = NIL) DO
                hresFind := FindResource(ResourceInst, idStr^, resTypes[i]);
                INC(i);
            END;
        END;

        IF hresFind <> NIL THEN
            hres := LoadResource(ResourceInst, hresFind);
            IF hres = NIL THEN
                RETURN NIL;
            END;

            amount := SizeofResource(ResourceInst, hresFind);
            data := LockResource(hres);

            IF (amount <> 0) AND (data <> NIL) THEN
                WIN32.GetTempPath(HIGH(tempSpec)+1, tempSpec);
                Append("~WinShellTempImage.", tempSpec);
                Append(resTypes[i-1], tempSpec);
                CreateFileEx(f, tempSpec, FileUseInfoSet{TemporaryFile});
                IF f.status = 0 THEN
                    WriteBlock(f, data, amount);
                    CloseFile(f);

                    bmp := LoadGdiplusBitmapFromFile(tempSpec, map);

                    DeleteFile(tempSpec);
                END;
            END;

            FreeResource(hres);
        END;
    END;

    RETURN bmp;
END LoadResBitmap;

PROCEDURE LoadToolbarBitmaps(VAR INOUT buttons : ARRAY OF ToolbarButtonInfo; transparent : ColorValue) : BOOLEAN;
VAR
    i           : ADRCARD;
    highButtons : ADRCARD;
    bmp         : BitmapHandle;
    colorMap    : ColorMapRecord;
BEGIN
    colorMap.from := transparent;
    colorMap.to := GetSystemColor(SC_BUTTON_FACE);

    highButtons := HIGH(buttons);
    FOR i := 0 TO highButtons DO
        IF (buttons[i].image = NIL) AND
           (NOT buttons[i].stdButton) AND
           (buttons[i].bmpResName[0] <> '')
        THEN
            bmp := LoadResBitmap(buttons[i].bmpResName, colorMap);
            IF bmp <> NIL THEN
                buttons[i].image := bmp;
            ELSE
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END LoadToolbarBitmaps;

PROCEDURE UnloadToolbarBitmaps(VAR INOUT buttons : ARRAY OF ToolbarButtonInfo);
VAR
    i           : ADRCARD;
    highButtons : ADRCARD;
BEGIN
    highButtons := HIGH(buttons);
    FOR i := 0 TO highButtons DO
        IF (NOT buttons[i].stdButton) AND (buttons[i].image <> NIL) THEN
            DeleteBitmap(buttons[i].image);
            buttons[i].image := NIL;
        END;
    END;
END UnloadToolbarBitmaps;

PROCEDURE DoCreateToolbar(w : Window; hasText : BOOLEAN; hasHelp : BOOLEAN; customize : BOOLEAN);
VAR
    style       : CARDINAL;
BEGIN
    NEW(w^.toolbar);
    IF w^.toolbar <> NIL THEN
        style := WS_CLIPSIBLINGS BOR WS_VISIBLE BOR WS_CHILD;

        IF hasHelp THEN
            style := style BOR TBSTYLE_TOOLTIPS;
        END;
        IF customize THEN
            style := style BOR CCS_ADJUSTABLE;
        END;

        w^.toolbar^.wnd := CreateToolbarEx(w^.wnd,
                                           style,
                                           TOOLBAR_ID,
                                           15,
                                           HINST_COMMCTRL,
                                           IDB_STD_LARGE_COLOR,
                                           NIL_TBBUTTON, 0,
                                           0, 0,
                                           24, 24,
                                           SIZE(TBBUTTON));

        w^.toolbar^.tipWnd :=
            CAST(HWND, WINUSER.SendMessage(w^.toolbar^.wnd, TB_GETTOOLTIPS, 0, 0));
        w^.toolbar^.buttons := NIL;
        w^.toolbar^.numButtons := 0;
        w^.toolbar^.customize := customize;
        w^.toolbar^.hasText := hasText;
        w^.toolbar^.hasHelp := hasHelp;
        w^.toolbar^.queryNum := 0;

        w^.toolbar^.createInfo.numButtons := 0;
        w^.toolbar^.createInfo.numFmt := 0;
        w^.toolbar^.createInfo.buttons := NIL;
        w^.toolbar^.createInfo.fmt := NIL;
    END;
END DoCreateToolbar;

PROCEDURE DoDestroyToolbar(VAR INOUT toolbar : ToolbarInfoPointer);
VAR
    wnd         : HWND;
BEGIN
    IF toolbar <> NIL THEN
        wnd := toolbar^.wnd;

        IF toolbar^.buttons <> NIL THEN
            DEALLOCATE(toolbar^.buttons, toolbar^.numButtons * SIZE(TbButtonInfo));
        END;
        IF toolbar^.createInfo.buttons <> NIL THEN
            DEALLOCATE(toolbar^.createInfo.buttons, toolbar^.createInfo.numButtons * SIZE(ToolbarButtonInfo));
        END;
        IF toolbar^.createInfo.fmt <> NIL THEN
            DEALLOCATE(toolbar^.createInfo.fmt, toolbar^.createInfo.numFmt * SIZE(CARDINAL));
        END;
        DISPOSE(toolbar);

        DestroyWindow(wnd);
    END;
END DoDestroyToolbar;

PROCEDURE CreateToolbar(w : Window;
                        buttons : ARRAY OF ToolbarButtonInfo;
                        hasText : BOOLEAN;
                        hasHelp : BOOLEAN;
                        canCustomize : BOOLEAN) : BOOLEAN;

CONST
    BmpIds      : ARRAY StdToolbarBitmaps OF CARDINAL =
                    {0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14};

VAR
    addRec      : TBADDBITMAP;
    i           : ADRCARD;
    highButtons : ADRCARD;
    toolbar     : ToolbarInfoPointer;
    count       : CARDINAL;
    text        : ARRAY [0..63] OF CHAR;
BEGIN
    IF w^.type = ChildWindow THEN
        RETURN FALSE;
    END;

    toolbar := w^.toolbar;
    w^.toolbar := NIL;
    DoCreateToolbar(w, hasText, hasHelp, canCustomize);
    DoDestroyToolbar(toolbar);
    IF w^.toolbar = NIL THEN
        AdjustClientSize(w);
        RETURN FALSE;
    END;

    w^.toolbar^.numButtons := HIGH(buttons) + 1;
    ALLOCATE(w^.toolbar^.buttons, w^.toolbar^.numButtons * SIZE(TbButtonInfo));
    IF w^.toolbar^.buttons = NIL THEN
        DestroyToolbar(w);
        RETURN FALSE;
    END;

    highButtons := HIGH(buttons);
    FOR i := 0 TO highButtons DO
        w^.toolbar^.buttons^[i].type := buttons[i].type;
        w^.toolbar^.buttons^[i].actionId := buttons[i].actionId;
        w^.toolbar^.buttons^[i].textId := buttons[i].textId;
        w^.toolbar^.buttons^[i].helpId := buttons[i].helpId;

        IF NOT buttons[i].stdButton THEN
            w^.toolbar^.buttons^[i].bmpHandle := buttons[i].image;
            addRec.hInst := NIL;
            addRec.nID := CAST(CARDINAL, buttons[i].image);
            w^.toolbar^.buttons^[i].bmpId :=
                    WINUSER.SendMessage(w^.toolbar^.wnd,
                                        TB_ADDBITMAP,
                                        1,
                                        CAST(LPARAM, ADR(addRec)));
        ELSE
            w^.toolbar^.buttons^[i].bmpId := BmpIds[buttons[i].stdName];
            w^.toolbar^.buttons^[i].bmpHandle := NIL;
        END;

        IF hasText THEN
            LoadString(w^.toolbar^.buttons^[i].textId, text);
            IF text[0] = '' THEN
                text := "?";
            END;
            text[LENGTH(text)+1] := '';(* double null terminate *)
            WINUSER.SendMessage(w^.toolbar^.wnd, TB_ADDSTRING, CAST(WPARAM, NIL), CAST(LPARAM, ADR(text)));
        END;
    END;

    AdjustClientSize(w);

    IF (w^.type = ToplevelWindow) AND (w^.clientType = TabClient) THEN
        count := HIGH(buttons)+1;
        w^.toolbar^.createInfo.numButtons := count;
        ALLOCATE(w^.toolbar^.createInfo.buttons, count * SIZE(ToolbarButtonInfo));
        IF w^.toolbar^.createInfo.buttons <> NIL THEN
            w^.toolbar^.createInfo.buttons^[0..count-1] := buttons;
        END;
    END;

    RETURN TRUE;
END CreateToolbar;

PROCEDURE DestroyToolbar(w : Window);
BEGIN
    DoDestroyToolbar(w^.toolbar);
    AdjustClientSize(w);
END DestroyToolbar;

PROCEDURE SetToolbarButtons(w : Window; fmt : ARRAY OF CARDINAL);
VAR
    button      : POINTER TO ARRAY [0..0] OF TBBUTTON;
    i           : ADRCARD;
    buttons     : ADRCARD;
    highFmt     : ADRCARD;
BEGIN
    IF (w^.toolbar <> NIL) AND (w^.toolbar^.buttons <> NIL) THEN
        ALLOCATE(button, (HIGH(fmt)+1)*SIZE(TBBUTTON));
        IF button = NIL THEN
            RETURN;
        END;

        buttons := WINUSER.SendMessage(w^.toolbar^.wnd, TB_BUTTONCOUNT, 0, 0);
        FOR i := 1 TO buttons DO
            WINUSER.SendMessage(w^.toolbar^.wnd, TB_DELETEBUTTON, 0, 0);
        END;
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, w^.toolbar^.numButtons)-1 DO
        <*/POP*>
            w^.toolbar^.buttons^[i].visible := FALSE;
        END;

        highFmt := HIGH(fmt);
        FOR i := 0 TO highFmt DO
            IF fmt[i] <> TOOLBAR_SEPARATOR THEN
                IF fmt[i] < w^.toolbar^.numButtons THEN
                    button^[i].iBitmap := w^.toolbar^.buttons^[fmt[i]].bmpId;
                    button^[i].idCommand := w^.toolbar^.buttons^[fmt[i]].actionId;
                    button^[i].fsState := TBSTATE_ENABLED;
                    CASE w^.toolbar^.buttons^[fmt[i]].type OF
                    TbPushButton:
                        button^[i].fsStyle := TBSTYLE_BUTTON;
                    |
                    TbToggleButton:
                        button^[i].fsStyle := TBSTYLE_CHECK;
                    END;
                    button^[i].dwData := TOOLBAR_SEPARATOR;
                    button^[i].iString := 0;
                    IF w^.toolbar^.hasText THEN
                        button^[i].iString := fmt[i];
                    END;

                    w^.toolbar^.buttons^[fmt[i]].visible := TRUE;
                ELSE
                    (* bogus, just place a separator *)

                    button^[i].iBitmap := 0;
                    button^[i].idCommand := 0;
                    button^[i].fsState := TBSTATE_ENABLED;
                    button^[i].fsStyle := TBSTYLE_SEP;
                    button^[i].fsState := 0;
                    button^[i].dwData := TOOLBAR_SEPARATOR;
                    button^[i].iString := 0;
                END;
            ELSE
                button^[i].iBitmap := 0;
                button^[i].idCommand := 0;
                button^[i].fsState := TBSTATE_ENABLED;
                button^[i].fsStyle := TBSTYLE_SEP;
                button^[i].fsState := 0;
                button^[i].dwData := TOOLBAR_SEPARATOR;
                button^[i].iString := 0;
            END;
        END;
        WINUSER.SendMessage(w^.toolbar^.wnd, TB_ADDBUTTONS, HIGH(fmt)+1, CAST(LPARAM, button));

        DEALLOCATE(button, (HIGH(fmt)+1)*SIZE(TBBUTTON));

        IF (w^.type = ToplevelWindow) AND (w^.clientType = TabClient) THEN
            IF w^.toolbar^.createInfo.fmt <> NIL THEN
                DEALLOCATE(w^.toolbar^.createInfo.fmt, w^.toolbar^.createInfo.numFmt * SIZE(CARDINAL));
            END;

            buttons := HIGH(fmt)+1;
            w^.toolbar^.createInfo.numFmt := buttons;
            ALLOCATE(w^.toolbar^.createInfo.fmt, buttons * SIZE(CARDINAL));
            IF w^.toolbar^.createInfo.fmt <> NIL THEN
                w^.toolbar^.createInfo.fmt^[0..buttons-1] := fmt;
            END;
        END;
    END;
END SetToolbarButtons;

PROCEDURE GetToolbarButtons(w : Window; VAR OUT fmt : ARRAY OF CARDINAL) : CARDINAL;
VAR
    buttonCount         : ADRCARD;
    count               : CARDINAL;
    i                   : ADRCARD;
    j                   : ADRCARD;
    highFmt             : ADRCARD;
    <*/PUSH/NOCHECK:U*>
    button              : TBBUTTON;
    <*/POP*>
BEGIN
    IF (w^.toolbar <> NIL) AND (w^.toolbar^.buttons <> NIL) THEN
        buttonCount := WINUSER.SendMessage(w^.toolbar^.wnd, TB_BUTTONCOUNT, 0, 0);

        highFmt := HIGH(fmt);
        IF buttonCount-1 > highFmt THEN
            buttonCount := HIGH(fmt) + 1;
        END;

        FOR i := 1 TO buttonCount DO
            count := WINUSER.SendMessage(w^.toolbar^.wnd, TB_GETBUTTON, i-1, CAST(LPARAM, ADR(button)));

            IF button.fsStyle <> TBSTYLE_SEP THEN
                <*/PUSH/NOWARN:U*>
                FOR j := 0 TO VAL(ADRCARD, w^.toolbar^.numButtons)-1 DO
                <*/POP*>
                    IF ORD(button.idCommand) = w^.toolbar^.buttons^[j].actionId THEN
                        fmt[i-1] := j;
                        BREAK;
                    END;
                END;
            ELSE
                fmt[i-1] := TOOLBAR_SEPARATOR;
            END;

        END;

        RETURN buttonCount;
    END;
    RETURN 0;
END GetToolbarButtons;

PROCEDURE IsToolbarButtonShown(w : Window; index : ADRCARD) : BOOLEAN;
BEGIN
    <*/PUSH/NOWARN:U*>
    IF (w^.toolbar <> NIL) AND
       (w^.toolbar^.buttons <> NIL) AND
       (index < VAL(ADRCARD, w^.toolbar^.numButtons))
    THEN
    <*/POP*>
        RETURN WINUSER.SendMessage(w^.toolbar^.wnd, TB_ISBUTTONHIDDEN, w^.toolbar^.buttons^[index].actionId, 0) = 0;
    END;
    RETURN FALSE;
END IsToolbarButtonShown;

PROCEDURE IsToolbarButtonDown(w : Window; index : ADRCARD) : BOOLEAN;
BEGIN
    <*/PUSH/NOWARN:U*>
    IF (w^.toolbar <> NIL) AND
       (w^.toolbar^.buttons <> NIL) AND
       (index < VAL(ADRCARD, w^.toolbar^.numButtons))
    THEN
    <*/POP*>
        RETURN WINUSER.SendMessage(w^.toolbar^.wnd, TB_ISBUTTONCHECKED, w^.toolbar^.buttons^[index].actionId, 0) <> 0;
    END;
    RETURN FALSE;
END IsToolbarButtonDown;

PROCEDURE IsToolbarButtonEnabled(w : Window; index : ADRCARD) : BOOLEAN;
BEGIN
    <*/PUSH/NOWARN:U*>
    IF (w^.toolbar <> NIL) AND
       (w^.toolbar^.buttons <> NIL) AND
       (index < VAL(ADRCARD, w^.toolbar^.numButtons))
    THEN
    <*/POP*>
        RETURN WINUSER.SendMessage(w^.toolbar^.wnd, TB_ISBUTTONENABLED, w^.toolbar^.buttons^[index].actionId, 0) <> 0;
    END;
    RETURN FALSE;
END IsToolbarButtonEnabled;

PROCEDURE ShowToolbarButton(w : Window; index : ADRCARD; show : BOOLEAN) : BOOLEAN;
BEGIN
    <*/PUSH/NOWARN:U*>
    IF (w^.toolbar <> NIL) AND
       (w^.toolbar^.buttons <> NIL) AND
       (index < VAL(ADRCARD, w^.toolbar^.numButtons))
    THEN
    <*/POP*>
        WINUSER.SendMessage(w^.toolbar^.wnd,
                            TB_HIDEBUTTON,
                            w^.toolbar^.buttons^[index].actionId,
                            WINUSER.MAKELONG(ORD(NOT show), 0));
        RETURN TRUE;
    END;
    RETURN FALSE;
END ShowToolbarButton;

PROCEDURE ToggleToolbarButton(w : Window; index : ADRCARD; down : BOOLEAN) : BOOLEAN;
BEGIN
    <*/PUSH/NOWARN:U*>
    IF (w^.toolbar <> NIL) AND
       (w^.toolbar^.buttons <> NIL) AND
       (index < VAL(ADRCARD, w^.toolbar^.numButtons))
    THEN
    <*/POP*>
        WINUSER.SendMessage(w^.toolbar^.wnd,
                            TB_CHECKBUTTON,
                            w^.toolbar^.buttons^[index].actionId,
                            WINUSER.MAKELONG(ORD(down), 0));
        RETURN TRUE;
    END;
    RETURN FALSE;
END ToggleToolbarButton;

PROCEDURE EnableToolbarButton(w : Window; index : ADRCARD; enable : BOOLEAN) : BOOLEAN;
BEGIN
    <*/PUSH/NOWARN:U*>
    IF (w^.toolbar <> NIL) AND
       (w^.toolbar^.buttons <> NIL) AND
       (index < VAL(ADRCARD, w^.toolbar^.numButtons))
    THEN
    <*/POP*>
        WINUSER.SendMessage(w^.toolbar^.wnd,
                            TB_ENABLEBUTTON,
                            w^.toolbar^.buttons^[index].actionId,
                            WINUSER.MAKELONG(ORD(enable), 0));
        RETURN TRUE;
    END;
    RETURN FALSE;
END EnableToolbarButton;

PROCEDURE SetWindowTitle(w : Window; title : ARRAY OF CHAR);
VAR
    str         : ARRAY [0..127] OF CHAR;
    item        : TCITEM;
BEGIN
    str := title;
    str[HIGH(str)] := '';
    w^.title := str;
    IF w^.type = ToplevelWindow THEN
        SetWindowText(w^.wnd, str);
    ELSIF IsTabChild(w) THEN
        GetTabChildTitle(w, str);
        item.mask := TCIF_TEXT;
        item.pszText := ADR(str);
        TabCtrl_SetItem(w^.parent^.clientWnd, w^.tabIndex, item);
    END;
END SetWindowTitle;

PROCEDURE SetWindowIcon(w : Window; icon : ARRAY OF CHAR) : BOOLEAN;
VAR
    ico         : HICON;
BEGIN
    ico := LoadIcon(ResourceInst, icon);
    IF ico <> NIL THEN
        w^.bigIcon := ico;

        ico := LoadImage(ResourceInst, icon, IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR BOR LR_SHARED);
        IF ico <> NIL THEN
            w^.smallIcon := ico;
        END;

        SetWindowIconOS(w);
        RETURN TRUE;
    END;
    RETURN FALSE;
END SetWindowIcon;

PROCEDURE SetWindowCursor(w : Window; typ : CursorTypes);
VAR
    cur         : HCURSOR;
    ptr         : LPTSTR;
    inst        : HINSTANCE;
BEGIN
    IF w^.clientType = DrawClient THEN
        inst := NIL;
        CASE typ OF
        LeftArrowCursor:
            ptr := IDC_ARROW;
        |
        RightArrowCursor:
            inst := ResourceInst;
            ptr := ADR("RIGHT-ARROW-CURSOR");
        |
        WaitCursor:
            ptr := IDC_WAIT;
        |
        TextCursor:
            ptr := IDC_IBEAM;
        |
        CrossHairCursor:
            ptr := IDC_CROSS;
        END;
        cur := LoadCursor(inst, ptr^);

        IF cur <> NIL THEN
            IF w^.busy = 0 THEN
                w^.cursor := cur;
                ShowWindowCursor(w);
            ELSE
                w^.busySave := cur;
            END;
        END;
    END;
END SetWindowCursor;

PROCEDURE SetWindowMenu(w : Window; menu : ARRAY OF CHAR) : BOOLEAN;
VAR
    oldMenu     : HMENU;
BEGIN
    oldMenu := w^.menu;

    w^.menu := MyLoadMenu(ResourceInst, menu);

    IF w^.menu <> NIL THEN
        IF w^.type = ToplevelWindow THEN
            IF (w^.clientType <> TabClient) OR
               (w^.active = NIL) OR
               (w^.active^.menu = NIL)
            THEN
                w^.activeMenu := w^.menu;
                SetMenu(w^.wnd, w^.menu);
                DrawMenuBar(w^.wnd);
            END;
        ELSE
            IF IsTabChild(w) AND IsCurrentTabChild(w) THEN
                IF w^.parent^.type = ToplevelWindow THEN
                    SetMdiMenu(w);
                END;
            END;
        END;

        IF oldMenu <> NIL THEN
            DestroyMenu(oldMenu);
        END;

        RETURN TRUE;
    END;
    RETURN FALSE;
END SetWindowMenu;

PROCEDURE GetWindowMenu(w : Window) : MenuHandle;
BEGIN
    RETURN w^.menu;
END GetWindowMenu;

PROCEDURE SetMenuItemEnable(w : Window; id : CARDINAL; state : BOOLEAN);
VAR
    attrib      : CARDINAL;
BEGIN
    IF state THEN
        attrib := MF_BYCOMMAND BOR MF_ENABLED;
    ELSE
        attrib := MF_BYCOMMAND BOR MF_GRAYED;
    END;

    IF w^.activeMenu <> NIL THEN
        EnableMenuItem(w^.activeMenu, id, attrib);
    END;
    IF w^.popMenu <> NIL THEN
        EnableMenuItem(w^.popMenu, id, attrib);
    END;
END SetMenuItemEnable;

PROCEDURE GetMenuItemEnable(w : Window; id : CARDINAL) : BOOLEAN;
VAR
    state       : CARDINAL;
BEGIN
    IF w^.activeMenu <> NIL THEN
        state := EnableMenuItem(w^.activeMenu, id, MF_BYCOMMAND BOR MF_ENABLED);
        EnableMenuItem(w^.activeMenu, id, MF_BYCOMMAND BOR state);
        RETURN state = MF_ENABLED;
    END;
    RETURN FALSE;
END GetMenuItemEnable;

PROCEDURE SetMenuItemCheck(w : Window; id : CARDINAL; state : BOOLEAN);
VAR
    attrib      : CARDINAL;
BEGIN
    IF state THEN
        attrib := MF_BYCOMMAND BOR MF_CHECKED;
    ELSE
        attrib := MF_BYCOMMAND BOR MF_UNCHECKED;
    END;

    IF w^.activeMenu <> NIL THEN
        CheckMenuItem(w^.activeMenu, id, attrib);
    END;
    IF w^.popMenu <> NIL THEN
        CheckMenuItem(w^.popMenu, id, attrib);
    END;
END SetMenuItemCheck;

PROCEDURE GetMenuItemCheck(w : Window; id : CARDINAL) : BOOLEAN;
VAR
    state : CARDINAL;
BEGIN
    IF w^.activeMenu <> NIL THEN
        state := CheckMenuItem(w^.activeMenu, id, MF_BYCOMMAND BOR MF_CHECKED);
        CheckMenuItem(w^.activeMenu, id, MF_BYCOMMAND BOR state);
        RETURN state = MF_CHECKED;
    END;
    RETURN FALSE;
END GetMenuItemCheck;

PROCEDURE SetMenuItemRadioCheck(w : Window;
                                first, last, set : CARDINAL);
VAR
    i   : CARDINAL;
BEGIN
    FOR i := first TO last DO
        SetMenuItemCheck(w, i, i = set);
    END;
END SetMenuItemRadioCheck;

PROCEDURE GetMenuItemRadioCheck(w : Window; first, last : CARDINAL) : CARDINAL;
VAR
    i   : CARDINAL;
BEGIN
    FOR i := first TO last DO
        IF GetMenuItemCheck(w, i) THEN
            RETURN i;
        END;
    END;
    RETURN 0;
END GetMenuItemRadioCheck;

PROCEDURE LoadMenu(w : Window; menu : ARRAY OF CHAR; popup : BOOLEAN) : MenuHandle;
VAR
    load        : LoadedMenuPointer;
    menuR,
    menuH       : HMENU;
BEGIN
    UNREFERENCED_PARAMETER(w);

    menuR := MyLoadMenu(ResourceInst, menu);
    menuH := menuR;
    IF (menuR <> NIL) AND popup THEN
        menuH := WINUSER.GetSubMenu(menuR, 0)
    END;

    IF menuH <> NIL THEN
        NEW(load);
        load^.menuRes := menuR;
        load^.menu := menuH;
        load^.next := LoadedMenus;
        LoadedMenus := load;

        RETURN menuH;
    END;

    IF menuR <> NIL THEN
        WINUSER.DestroyMenu(menuR);
    END;
    RETURN NIL;
END LoadMenu;

PROCEDURE DestroyMenu(VAR INOUT menuH : MenuHandle);
VAR
    ptr, prev   : LoadedMenuPointer;
BEGIN
    prev := NIL;
    ptr := LoadedMenus;
    WHILE (ptr <> NIL) AND (ptr^.menu <> menuH) DO
        prev := ptr;
        ptr := ptr^.next;
    END;
    IF ptr <> NIL THEN
        IF prev = NIL THEN
            LoadedMenus := ptr^.next;
        ELSE
            prev^.next := ptr^.next;
        END;

        WINUSER.DestroyMenu(ptr^.menuRes);

        DISPOSE(ptr);
        menuH := NIL;
    END;
END DestroyMenu;

PROCEDURE GetSubMenu(menuH : MenuHandle; subMenuId  : CARDINAL) : MenuHandle;
VAR
    info        : MENUITEMINFO;
BEGIN
    info.cbSize := SIZE(info);
    info.fMask := MIIM_SUBMENU;
    info.hSubMenu := NIL;
    WINUSER.GetMenuItemInfo(menuH, subMenuId, FALSE, info);
    RETURN info.hSubMenu;
END GetSubMenu;

PROCEDURE DisposeSubMenu(menuH : MenuHandle);
BEGIN
    UNREFERENCED_PARAMETER(menuH);(*nothing to do on Windows*)
END DisposeSubMenu;

PROCEDURE AppendMenuItemStr(w : Window; menuH : MenuHandle; str : ARRAY OF CHAR; id : CARDINAL) : BOOLEAN;
VAR
    lstr        : ARRAY [0..127] OF CHAR;
    i, l        : ADRCARD;
BEGIN
    UNREFERENCED_PARAMETER(w);

    lstr := str;
    lstr[HIGH(lstr)] := '';

    l := LENGTH(lstr);
    i := 0;
    WHILE i < l DO
        IF lstr[i] = '_' THEN
            IF lstr[i+1] <> '_' THEN
                lstr[i] := '&';
            ELSE
                Delete(lstr, i, 1);
                DEC(l);
            END;
        END;
        INC(i);
    END;

    RETURN AppendMenu(menuH, MF_ENABLED BOR MF_STRING, id, lstr);
END AppendMenuItemStr;

PROCEDURE AppendMenuItemSeparator(w : Window; menuH : MenuHandle) : BOOLEAN;
VAR
    str : ARRAY [0..0] OF CHAR;
BEGIN
    UNREFERENCED_PARAMETER(w);

    str[0] := '';

    RETURN AppendMenu(menuH, MF_SEPARATOR, 0, str);
END AppendMenuItemSeparator;

PROCEDURE SetMenuItemStr(w : Window; id : CARDINAL; str : ARRAY OF CHAR) : BOOLEAN;
VAR
    lstr        : ARRAY [0..127] OF CHAR;
    i, l        : ADRCARD;
    info        : MENUITEMINFO;
BEGIN
    lstr := str;
    lstr[HIGH(lstr)] := '';

    l := LENGTH(lstr);
    i := 0;
    WHILE i < l DO
        IF lstr[i] = '_' THEN
            IF lstr[i+1] <> '_' THEN
                lstr[i] := '&';
            ELSE
                Delete(lstr, i, 1);
                DEC(l);
            END;
        END;
        INC(i);
    END;

    info.cbSize := SIZE(info);
    info.fMask := MIIM_TYPE;
    info.fType := MFT_STRING;
    info.dwTypeData := ADR(lstr);

    IF w^.activeMenu <> NIL THEN
        RETURN SetMenuItemInfo(w^.activeMenu, id, FALSE, info) <> FALSE;
    END;
    IF w^.popMenu <> NIL THEN
        RETURN SetMenuItemInfo(w^.popMenu, id, FALSE, info) <> FALSE;
    END;

    RETURN FALSE;
END SetMenuItemStr;

PROCEDURE DeleteMenuItemPosition(menuH : MenuHandle; pos : CARDINAL) : BOOLEAN;
BEGIN
    RETURN WINUSER.DeleteMenu(menuH, pos, MF_BYPOSITION);
END DeleteMenuItemPosition;

PROCEDURE PopupMenuHandle(w : Window; menu : MenuHandle; button : MouseButton; x, y : COORDINATE);
VAR
    pt          : POINT;
    attrib      : CARDINAL;
    top         : Window;
BEGIN
    IF menu <> NIL THEN
        pt.x := x;
        pt.y := y;

        WINUSER.ClientToScreen(w^.clientWnd, pt);

        attrib := TPM_LEFTALIGN;
        CASE button OF
        LeftButton:
            attrib := attrib BOR TPM_LEFTBUTTON;
        |
        RightButton:
            attrib := attrib BOR TPM_RIGHTBUTTON;
        ELSE
            RETURN;
        END;

        w^.popMenu := menu;
        top := GetToplevel(w);
        top^.popMenu := menu;

        TrackPopupMenu(w^.popMenu, attrib, pt.x, pt.y, 0, w^.clientWnd, NIL_RECT);

        w^.popMenu := NIL;
        top^.popMenu := NIL;
    END;
END PopupMenuHandle;

PROCEDURE PopupMenu(w : Window; menu : ARRAY OF CHAR; button : MouseButton; x, y : COORDINATE);
VAR
    menuH       : MenuHandle;
BEGIN
    menuH := LoadMenu(w, menu, TRUE);
    IF menuH <> NIL THEN
        PopupMenuHandle(w, menuH, button, x, y);
        DestroyMenu(menuH);
    END;
END PopupMenu;

PROCEDURE InitStringCache;
VAR
    i   : ADRCARD;
BEGIN
    IF NOT StrCacheInit THEN
        StrCacheInit := TRUE;

        FOR i := 0 TO StrCacheElements-1 DO
            NEW(StrCache[i]);
            StrCache[i]^.idNum := MAX(CARDINAL);
            StrCache[i]^.data := "";
        END;
    END;
END InitStringCache;

PROCEDURE CacheHit(idNum : CARDINAL) : BOOLEAN;
VAR
    i           : ADRCARD;
    save        : StrCacheElement;
BEGIN
    i := 0;
    LOOP
        IF i < StrCacheElements THEN
            IF idNum <> StrCache[i]^.idNum THEN
                INC(i);
            ELSE
                IF i <> 0 THEN
                    save := StrCache[i];
                    StrCache[1..i] := StrCache[0..i-1];
                    StrCache[0] := save;
                END;
                RETURN TRUE;
            END;
        ELSE
            EXIT;
        END;
    END;
    RETURN FALSE;
END CacheHit;

PROCEDURE LoadStringRes(idNum : CARDINAL; VAR OUT str : ARRAY OF CHAR);
VAR
    len         : CARDINAL;
BEGIN
    len := WINUSER.LoadString(ResourceInst, idNum, str, HIGH(str)+1);
    IF len = 0 THEN
        len := 1;
    END;

    IF len <= StrCacheStringSize THEN
        StrCache[1..StrCacheElements-1] := StrCache[0..StrCacheElements-2];

        StrCache[0]^.data := str;
        StrCache[0]^.idNum := idNum;
    END;
END LoadStringRes;

PROCEDURE LoadString(idNum : CARDINAL; VAR OUT str : ARRAY OF CHAR);
BEGIN
    WHILE ATOMIC_CMPXCHG(StrCacheLock, 0, 1) <> 0 DO
        YieldThread;
    END;
    MEMORY_FENCE;

    IF NOT StrCacheInit THEN
        InitStringCache;
    END;

    IF CacheHit(idNum) THEN
        str := StrCache[0]^.data;
    ELSE
        LoadStringRes(idNum, str);
    END;

    MEMORY_FENCE;
    StrCacheLock := 0;

EXCEPT
    MEMORY_FENCE;
    StrCacheLock := 0;
END LoadString;

CONST
    WinClipFmt  : ARRAY ClipboardFormat OF CARDINAL = {CF_TEXT, CF_UNICODETEXT};
VAR
    ClipOwner           : Window;
    ClipSetMem          : HANDLE;
    ClipSetSize         : CARDINAL;
    ClipGetMem          : HANDLE;

PROCEDURE OpenClipboard(w : Window) : BOOLEAN;
BEGIN
    IF ClipOwner = NIL THEN
        IF WINUSER.OpenClipboard(w^.wnd) THEN
            ClipOwner := w;
            RETURN TRUE;
        END;
    END;
    RETURN ClipOwner = w;
END OpenClipboard;

PROCEDURE CloseClipboard(w : Window);
BEGIN
    IF ClipOwner = w THEN
        IF WINUSER.CloseClipboard() THEN
            ClipOwner := NIL;
        END;
    END;
END CloseClipboard;

PROCEDURE EmptyClipboard(w : Window) : BOOLEAN;
BEGIN
    IF ClipOwner = w THEN
        RETURN WINUSER.EmptyClipboard() <> FALSE;
    END;
    RETURN FALSE;
END EmptyClipboard;

PROCEDURE ClipboardFormatAvailable(fmt : ClipboardFormat) : BOOLEAN;
BEGIN
    RETURN IsClipboardFormatAvailable(WinClipFmt[fmt]) <> FALSE;
END ClipboardFormatAvailable;

PROCEDURE AllocClipboardMemory(size : CARDINAL) : ADDRESS;
BEGIN
    ClipSetSize := size;
    ClipSetMem := GlobalAlloc(GMEM_DDESHARE BOR GMEM_MOVEABLE, size);
    RETURN GlobalLock(ClipSetMem);
END AllocClipboardMemory;

PROCEDURE SetClipboard(fmt : ClipboardFormat) : BOOLEAN;
BEGIN
    IF ClipOwner <> NIL THEN
        GlobalUnlock(ClipSetMem);
        RETURN SetClipboardData(WinClipFmt[fmt], ClipSetMem) <> NIL;
    END;
    RETURN FALSE;
END SetClipboard;

PROCEDURE GetClipboard(fmt : ClipboardFormat) : ADDRESS;
BEGIN
    IF ClipOwner <> NIL THEN
        ClipGetMem := WINUSER.GetClipboardData(WinClipFmt[fmt]);
        RETURN GlobalLock(ClipGetMem);
    END;
    RETURN NIL;
END GetClipboard;

PROCEDURE UnlockClipboardMemory;
BEGIN
    GlobalUnlock(ClipGetMem);
END UnlockClipboardMemory;

PROCEDURE SetTimer(w : Window; timerId : CARDINAL; interval : CARDINAL);
BEGIN
    IF interval > 0 THEN
        WINUSER.SetTimer(w^.wnd, timerId+TimerIdFudge, interval, CAST(TIMERPROC, NULL));
    ELSE
        KillTimer(w, timerId);
    END;
END SetTimer;

PROCEDURE KillTimer(w : Window; timerId : CARDINAL);
BEGIN
    WINUSER.KillTimer(w^.wnd, timerId+TimerIdFudge);
END KillTimer;

PROCEDURE Beep(beep : Beeps);
CONST
    winBeep : ARRAY Beeps OF CARDINAL = {MB_OK, MB_ICONQUESTION, MB_ICONEXCLAMATION, MB_ICONSTOP};
BEGIN
    MessageBeep(winBeep[beep]);
END Beep;

PROCEDURE PointInRect(x, y : COORDINATE; rect : wsRECT) : BOOLEAN;
BEGIN
    IF ((x >= rect.x1) AND (x <= rect.x2)) AND
       ((y >= rect.y1) AND (y <= rect.y2))
    THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END PointInRect;

PROCEDURE RectOverlap(r1, r2 : wsRECT) : BOOLEAN;
BEGIN
    IF (
        ((r1.x1 <= r2.x1) AND (r1.x2 >= r2.x1)) OR
        ((r1.x1 >= r2.x1) AND (r2.x2 >= r1.x1))
       )
       AND
       (
        ((r1.y1 <= r2.y1) AND (r1.y2 >= r2.y1)) OR
        ((r1.y1 >= r2.y1) AND (r2.y2 >= r1.y1))
       )
    THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END RectOverlap;

PROCEDURE IntersectRect(r1, r2 : wsRECT; VAR OUT r3 : wsRECT) : BOOLEAN;
BEGIN
    IF RectOverlap(r1, r2) THEN
        r3.x1 := Max(r1.x1, r2.x1);
        r3.x2 := Min(r1.x2, r2.x2);
        r3.y1 := Max(r1.y1, r2.y1);
        r3.y2 := Min(r1.y2, r2.y2);
        RETURN TRUE;
    END;
    RETURN FALSE;
END IntersectRect;

PROCEDURE UnionRect(r1, r2 : wsRECT; VAR OUT r3 : wsRECT);
BEGIN
    r3.x1 := Min(r1.x1, r2.x1);
    r3.x2 := Max(r1.x2, r2.x2);
    r3.y1 := Min(r1.y1, r2.y1);
    r3.y2 := Max(r1.y2, r2.y2);
END UnionRect;

PROCEDURE OffsetRect(x, y : COORDINATE; VAR INOUT rect : wsRECT);
BEGIN
    rect.x1 := rect.x1 + x;
    rect.x2 := rect.x2 + x;
    rect.y1 := rect.y1 + y;
    rect.y2 := rect.y2 + y;
END OffsetRect;

PROCEDURE GetSystemColor(sysColor : SystemColors) : ColorValue;
CONST
    winSysColor : ARRAY SystemColors OF CARDINAL =
        {
         COLOR_WINDOW,
         COLOR_HIGHLIGHT,
         COLOR_HIGHLIGHTTEXT,
         COLOR_GRAYTEXT,
         COLOR_BTNFACE
        };
BEGIN
    RETURN GetSysColor(winSysColor[sysColor]);
END GetSystemColor;

PROCEDURE LoadFont(font : FontInfo) : FontHandle;
CONST
    weights     : ARRAY FontWeights OF CARDINAL =
        {FW_LIGHT, FW_NORMAL, FW_DEMIBOLD, FW_BOLD, FW_HEAVY};
VAR
    <*/PUSH/NOCHECK:U*>
    lf          : LOGFONT;
    <*/POP*>
    dc          : HDC;
    pixels      : INTEGER;
BEGIN
    ZeroMem(ADR(lf), SIZE(lf));

    dc := GetDC(NIL);
    pixels := GetDeviceCaps(dc, LOGPIXELSY);
    ReleaseDC(NIL, dc);

    IF font.height >= 0 THEN
        pixels := font.height * pixels;
        lf.lfHeight := -(pixels / 720);
        IF (pixels REM 720) > 360 THEN
            DEC(lf.lfHeight);
        END;
    ELSE
        lf.lfHeight := font.height;
    END;
    lf.lfPitchAndFamily := FF_DONTCARE;
    lf.lfFaceName := font.familyName;
    lf.lfWeight := weights[font.weight];
    lf.lfCharSet := WINGDI.DEFAULT_CHARSET;

    lf.lfWidth := 0;
    lf.lfEscapement := 0;
    lf.lfOrientation := 0;
    lf.lfItalic := font.italic;
    lf.lfUnderline := FALSE;
    lf.lfStrikeOut := FALSE;
    lf.lfOutPrecision := OUT_TT_PRECIS;
    lf.lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lf.lfQuality := DEFAULT_QUALITY;

    RETURN CreateFontIndirect(lf);
END LoadFont;

PROCEDURE DeleteFont(font : FontHandle);
BEGIN
    WINX.DeleteFont(font);
END DeleteFont;

PROCEDURE GetTextMetrics(font : FontHandle; VAR OUT extents : TextMetrics);
VAR
    dc          : HDC;
    tm          : TEXTMETRIC;
    oldFont     : HFONT;
    aveCh       : CARDINAL;
    aveDigit    : CARDINAL;
BEGIN
    dc := GetDC(NIL);
    oldFont := SelectFont(dc, font);
    WINGDI.GetTextMetrics(dc, tm);
    aveCh := ComputeAverageCharWidth(dc);
    aveDigit := ComputeAverageDigitWidth(dc);
    SelectFont(dc, oldFont);
    ReleaseDC(NIL, dc);

    extents.aveCharWidth := tm.tmAveCharWidth;
    extents.aveDigitWidth := aveDigit;
    extents.ascent := tm.tmAscent;
    extents.descent := tm.tmDescent;
    extents.height := tm.tmHeight;
    extents.externalLeading := tm.tmExternalLeading;
END GetTextMetrics;

PROCEDURE LoadBitmap(name : ARRAY OF CHAR; shared : BOOLEAN) : BitmapHandle;
VAR
    bmp         : BitmapHandle;
    ptr         : CachedBitmapPointer;
    l           : CARDINAL;
BEGIN
    IF shared THEN
        ptr := CachedBitmaps;
        WHILE ptr <> NIL DO
            IF EqualI(ptr^.name, name) THEN
                INC(ptr^.refs);
                RETURN ptr^.bmp;
            END;

            ptr := ptr^.next;
        END;

        bmp := LoadResBitmap(name, NullColorMap);
        IF bmp <> NIL THEN
            l := LENGTH(name);
            ALLOCATE(ptr, SIZE(ptr^)-SIZE(ptr^.name)+l+1);
            ptr^.name[0..l] := name;
            ptr^.bmp := bmp;
            ptr^.refs := 1;
            ptr^.len := l;
            ptr^.next := CachedBitmaps;
            CachedBitmaps := ptr;
        END;

        RETURN bmp;
    ELSE
        RETURN LoadResBitmap(name, NullColorMap);
    END;
END LoadBitmap;

PROCEDURE LoadBitmapFromFile(name : ARRAY OF CHAR) : BitmapHandle;
VAR
    parts       : FileNameParts;
BEGIN
    ParseFileName(name, parts);
    IF EqualI(parts.extension, ".bmp") THEN
        RETURN WINUSER.LoadImage(NIL, name, WINUSER.IMAGE_BITMAP, 0, 0, WINUSER.LR_LOADFROMFILE);
    ELSE
        RETURN LoadGdiplusBitmapFromFile(name, NullColorMap);
    END;
END LoadBitmapFromFile;

PROCEDURE CreateBitmap(w : Window; width, height : CARDINAL) : BitmapHandle;
VAR
    dc          : HDC;
    bmp         : HBITMAP;
BEGIN
    dc := GetDC(w^.wnd);
    bmp := CreateCompatibleBitmap(dc, width, height);
    ReleaseDC(w^.wnd, dc);
    RETURN bmp;
END CreateBitmap;

PROCEDURE GetBitmapSize(bmp : BitmapHandle; VAR OUT width, height : CARDINAL);
VAR
    info        : BITMAP;
BEGIN
    width := 0;
    height := 0;
    IF GetBITMAP(bmp, info) THEN
        width := info.bmWidth;
        height := info.bmHeight;
    END;
END GetBitmapSize;

PROCEDURE ExtractBitmap(bmp : BitmapHandle; x, y : COORDINATE; width, height : CARDINAL) : BitmapHandle;
VAR
    bmp2        : BitmapHandle;
    dc,
    dc1, dc2    : HDC;
    oldBmp1,
    oldBmp2     : HBITMAP;
BEGIN
    dc := GetDC(NIL);
    dc1 := CreateCompatibleDC(dc);
    dc2 := CreateCompatibleDC(dc);
    ReleaseDC(NIL, dc);

    bmp2 := NIL;
    IF (dc1 <> NIL) AND (dc2 <> NIL) THEN
        oldBmp1 := SelectBitmap(dc1, bmp);

        bmp2 := CreateCompatibleBitmap(dc1, width, height);

        IF bmp2 <> NIL THEN
            oldBmp2 := SelectBitmap(dc2, bmp2);

            BitBlt(dc2, 0, 0, width, height, dc1, x, y, SRCCOPY);

            SelectBitmap(dc2, oldBmp2);
        END;

        SelectBitmap(dc1, oldBmp1);

        DeleteDC(dc1);
        DeleteDC(dc2);
    END;

    RETURN bmp2;
END ExtractBitmap;

PROCEDURE DeleteBitmap(bmp : BitmapHandle);
VAR
    prev,
    ptr         : CachedBitmapPointer;
BEGIN
    prev := NIL;
    ptr := CachedBitmaps;
    LOOP
        IF ptr <> NIL THEN
            IF ptr^.bmp <> bmp THEN
                prev := ptr;
                ptr := ptr^.next;
            ELSE
                DEC(ptr^.refs);
                IF ptr^.refs <> 0 THEN
                    RETURN;
                ELSE
                    IF prev <> NIL THEN
                        prev^.next := ptr^.next;
                    ELSE
                        CachedBitmaps := CachedBitmaps^.next;
                    END;
                    DEALLOCATE(ptr, SIZE(ptr^)-SIZE(ptr^.name)+ptr^.len+1);
                END;
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;

    WINX.DeleteBitmap(bmp);
END DeleteBitmap;

PROCEDURE CreateDrawContext(w : Window;
                            dcValues : DrawContextValues) : DrawContext;
VAR
    dc  : DrawContext;
BEGIN
    IF w^.clientType = DrawClient THEN
        NEW(dc);
        dc^.values := dcValues;
        dc^.drawables := NIL;
        RETURN dc;
    END;
    RETURN NIL;
END CreateDrawContext;

PROCEDURE DestroyDrawContext(VAR INOUT dc : DrawContext);
BEGIN
    IF dc <> NIL THEN
        DISPOSE(dc);
    END;
END DestroyDrawContext;

PROCEDURE SetDrawContextValues(dc : DrawContext; dcValues : DrawContextValues);
BEGIN
    dc^.values := dcValues;
END SetDrawContextValues;

PROCEDURE GetDrawContextValues(dc : DrawContext; VAR OUT dcValues : DrawContextValues);
BEGIN
    dcValues := dc^.values;
END GetDrawContextValues;

PROCEDURE SetForegroundColor(dc : DrawContext; color : ColorValue);
VAR
    draw        : Drawable;
BEGIN
    IF dc^.values.foreground <> color THEN
        dc^.values.foreground := color;

        draw := dc^.drawables;
        WHILE draw <> NIL DO
            IF draw^.winDC <> NIL THEN
                SetTextColor(draw^.winDC, color);
            END;

            draw := draw^.next;
        END;
    END;
END SetForegroundColor;

PROCEDURE SetBackgroundColor(dc : DrawContext; color : ColorValue);
VAR
    draw        : Drawable;
BEGIN
    IF dc^.values.background <> color THEN
        dc^.values.background := color;

        draw := dc^.drawables;
        WHILE draw <> NIL DO
            IF draw^.winDC <> NIL THEN
                SetBkColor(draw^.winDC, color);
            END;

            draw := draw^.next;
        END;
    END;
END SetBackgroundColor;

PROCEDURE SetFont(dc : DrawContext; font : FontHandle);
VAR
    draw        : Drawable;
    old         : HFONT;
BEGIN
    IF dc^.values.font <> font THEN
        dc^.values.font := font;

        draw := dc^.drawables;
        WHILE draw <> NIL DO
            IF draw^.winDC <> NIL THEN
                old := SelectFont(draw^.winDC, font);
                IF draw^.saveFont <> NIL THEN
                    draw^.saveFont := old;
                END;
            END;

            draw := draw^.next;
        END;
    END;
END SetFont;

PROCEDURE SetTextExtraSpacing(dc : DrawContext; extra : INTEGER);
VAR
    draw        : Drawable;
BEGIN
    IF dc^.values.textExtraSpacing <> extra THEN
        dc^.values.textExtraSpacing := extra;

        draw := dc^.drawables;
        WHILE draw <> NIL DO
            IF draw^.winDC <> NIL THEN
                SetTextCharacterExtra(draw^.winDC, extra);
            END;

            draw := draw^.next;
        END;
    END;
END SetTextExtraSpacing;

PROCEDURE SetTextDrawOrigin(dc : DrawContext; origin : TextDrawOrigin);
VAR
    draw        : Drawable;
BEGIN
    IF dc^.values.textOrigin <> origin THEN
        dc^.values.textOrigin := origin;

        draw := dc^.drawables;
        WHILE draw <> NIL DO
            IF draw^.winDC <> NIL THEN
                SetWin32TextAlign(draw^.winDC, origin);
            END;

            draw := draw^.next;
        END;
    END;
END SetTextDrawOrigin;

PROCEDURE SetLineWidth(dc : DrawContext; width : CARDINAL);
BEGIN
    dc^.values.lineWidth := width;
END SetLineWidth;

PROCEDURE SetLineStyle(dc : DrawContext; style : LineStyles);
BEGIN
    dc^.values.lineStyle := style;
END SetLineStyle;

PROCEDURE SetLineAttributes(dc : DrawContext; style : LineStyles; join : JoinStyles; endCap : EndCaps;
                            width : CARDINAL);
BEGIN
    dc^.values.lineStyle := style;
    dc^.values.joinStyle := join;
    dc^.values.endCap := endCap;
    dc^.values.lineWidth := width;
END SetLineAttributes;

PROCEDURE SetDrawFunction(dc : DrawContext; func : DrawFunctions);
VAR
    draw        : Drawable;
BEGIN
    IF dc^.values.drawFunc <> func THEN
        dc^.values.drawFunc := func;

        draw := dc^.drawables;
        WHILE draw <> NIL DO
            IF draw^.winDC <> NIL THEN
                SetROP2(draw^.winDC, Win32R2Ops[func]);
            END;

            draw := draw^.next;
        END;
    END;
END SetDrawFunction;

PROCEDURE BeginPaint(w : Window; dc : DrawContext) : Drawable;
VAR
    r   : RECT;
BEGIN
    IF w^.paintLock = 0 THEN
        w^.paintLock := 1;

        HideCaret(w);

        IF dc = NIL THEN
            dc := w^.defaultDrawContext;
        END;
        IF dc = NIL THEN
            RETURN NIL;
        END;

        LinkDrawable(w^.drawable, dc);
        w^.drawable^.winDC := GetDC(w^.clientWnd);
        w^.drawable^.wnd := w^.clientWnd;
        SetupDrawableDC(w^.drawable);

        w^.drawable^.type := WindowDrawable;

        GetClientRect(w^.clientWnd, r);
        w^.drawable^.width := r.right;
        w^.drawable^.height := r.bottom;
    ELSE
        INC(w^.paintLock);
    END;

    RETURN w^.drawable;
END BeginPaint;

PROCEDURE EndPaint(w : Window);
BEGIN
    IF w^.paintLock > 0 THEN
        DEC(w^.paintLock);

        IF w^.paintLock = 0 THEN
            CleanupDrawableDC(w^.drawable);
            UnlinkDrawable(w^.drawable);

            IF w^.drawable^.type = WindowDrawable THEN
                ReleaseDC(w^.drawable^.wnd, w^.drawable^.winDC);
                w^.drawable^.winDC := NIL;
            END;

            ShowCaret(w);
        END;
    END;
END EndPaint;

PROCEDURE CreateOffscreenDrawable(w : Window; width, height : CARDINAL; context : DrawContext) : Drawable;
VAR
    draw        : Drawable;
    dc          : HDC;
    bmp         : HBITMAP;
    ww, wh      : COORDINATE;
BEGIN
    IF (NOT IsWindow(w)) OR (w^.clientType <> DrawClient) OR (context = NIL) THEN
        RETURN NIL;
    END;

    NEW(draw);
    InitDrawable(draw^);
    draw^.type := OffscreenDrawable;

    IF (width = 0) OR (height = 0) THEN
        GetClientSize(w, ww, wh);
        IF width = 0 THEN
            width := ww;
        END;
        IF height = 0 THEN
            height := wh;
        END;
    END;
    draw^.width := width;
    draw^.height := height;

    dc := GetDC(w^.wnd);
    draw^.winDC := CreateCompatibleDC(dc);
    bmp := WINGDI.CreateCompatibleBitmap(dc, width, height);
    ReleaseDC(w^.wnd, dc);

    draw^.bmp := bmp;

    LinkDrawable(draw, context);
    SetupDrawableDC(draw);

    draw^.saveBmp := SelectBitmap(draw^.winDC, draw^.bmp);

    RETURN draw;
END CreateOffscreenDrawable;

PROCEDURE CreateBitmapDrawable(bmp : BitmapHandle; context : DrawContext) : Drawable;
VAR
    draw        : Drawable;
    dc          : HDC;
    info        : BITMAP;
BEGIN
    IF (bmp = NIL) OR (context = NIL) THEN
        RETURN NIL;
    END;

    NEW(draw);
    InitDrawable(draw^);
    draw^.type := BitmapDrawable;

    dc := GetDC(NIL);
    draw^.winDC := CreateCompatibleDC(dc);
    ReleaseDC(NIL, dc);

    draw^.bmp := bmp;
    draw^.width := 0;
    draw^.height := 0;
    IF GetBITMAP(bmp, info) THEN
        draw^.width := info.bmWidth;
        draw^.height := info.bmHeight;
    END;

    LinkDrawable(draw, context);
    SetupDrawableDC(draw);

    draw^.saveBmp := SelectBitmap(draw^.winDC, draw^.bmp);

    RETURN draw;
END CreateBitmapDrawable;

PROCEDURE CreateRgbDrawable(width, height : CARDINAL; context : DrawContext) : Drawable;
VAR
    dc          : HDC;
    draw        : Drawable;
BEGIN
    IF (width = 0) OR (height = 0) OR (context = NIL) THEN
        RETURN NIL;
    END;

    NEW(draw);
    InitDrawable(draw^);

    draw^.type := RgbDrawable;
    draw^.width := width;
    draw^.height := height;
    draw^.rowSize := draw^.width * SIZE(DIB_Pixel);

    (* DIB always has a 4 byte row stride *)

    draw^.rowStride := draw^.rowSize;
    IF (draw^.rowStride REM 4) <> 0 THEN
        draw^.rowStride := draw^.rowStride + (4 - (draw^.rowStride REM 4));
    END;

    dc := GetDC(NIL);
    draw^.winDC := CreateCompatibleDC(dc);
    ReleaseDC(NIL, dc);
    IF draw^.winDC = NIL THEN
        DISPOSE(draw);
        RETURN NIL;
    END;

    draw^.dibHeader.bmiHeader.biSize := SIZE(BITMAPINFOHEADER);
    draw^.dibHeader.bmiHeader.biWidth := draw^.width;
    draw^.dibHeader.bmiHeader.biHeight := -INT(draw^.height);
    draw^.dibHeader.bmiHeader.biPlanes := 1;
    draw^.dibHeader.bmiHeader.biBitCount := 24;
    draw^.dibHeader.bmiHeader.biCompression := BI_RGB;
    draw^.dibHeader.bmiHeader.biSizeImage := 0;
    draw^.dibHeader.bmiHeader.biXPelsPerMeter := TRUNC((LFLOAT(ScreenInfo.xDpi) / 2.54 * 100.0) + 0.5);
    draw^.dibHeader.bmiHeader.biYPelsPerMeter := TRUNC((LFLOAT(ScreenInfo.yDpi) / 2.54 * 100.0) + 0.5);
    draw^.dibHeader.bmiHeader.biClrUsed := 0;
    draw^.dibHeader.bmiHeader.biClrImportant := 0;
    draw^.bmp := CreateDIBSection(draw^.winDC, draw^.dibHeader, 0(*DIB_RGB_COLORS*), draw^.dibBits, NIL, 0);
    IF draw^.bmp = NIL THEN
        DeleteDC(draw^.winDC);
        DISPOSE(draw);
        RETURN NIL;
    END;

    LinkDrawable(draw, context);
    SetupDrawableDC(draw);

    draw^.saveBmp := SelectBitmap(draw^.winDC, draw^.bmp);

    RETURN draw;
END CreateRgbDrawable;

PROCEDURE GetDrawableSize(draw : Drawable; VAR OUT width, height : CARDINAL);
BEGIN
    width := draw^.width;
    height := draw^.height;
END GetDrawableSize;

PROCEDURE DestroyDrawable(draw : Drawable);
BEGIN
    UnlinkDrawable(draw);

    CleanupDrawableDC(draw);

    IF draw^.type <> WindowDrawable THEN
        DeleteDC(draw^.winDC);

        IF draw^.type <> BitmapDrawable THEN
            WINX.DeleteBitmap(draw^.bmp);
        END;
    END;

    DISPOSE(draw);
END DestroyDrawable;

PROCEDURE SelectDrawContext(draw : Drawable; context : DrawContext);
BEGIN
    IF draw^.context <> context THEN
        UnlinkDrawable(draw);
        IF context <> NIL THEN
            LinkDrawable(draw, context);
        END;

        IF draw^.winDC <> NIL THEN
            CleanupDrawableDC(draw);
            IF context <> NIL THEN
                SetupDrawableDC(draw);
            END;
        END;
    END;
END SelectDrawContext;

PROCEDURE PushDrawContext(draw : Drawable; context : DrawContext) : BOOLEAN;
BEGIN
    IF draw^.contextStackSp < MaxContextStack THEN
        INC(draw^.contextStackSp);
        draw^.contextStack[draw^.contextStackSp] := draw^.context;
        SelectDrawContext(draw, context);
        RETURN TRUE;
    END;
    RETURN FALSE;
END PushDrawContext;

PROCEDURE PopDrawContext(draw : Drawable);
BEGIN
    IF draw^.contextStackSp > 0 THEN
        SelectDrawContext(draw, draw^.contextStack[draw^.contextStackSp]);
        DEC(draw^.contextStackSp);
    END;
END PopDrawContext;

PROCEDURE CopyPixelRowToRgbDrawable(draw : Drawable;
                                    x, y : CARDINAL;
                                    numPixels : CARDINAL;
                                    pixels : ADDRESS;
                                    pixelFormat : RgbPixelFormat) : BOOLEAN;
VAR
    i           : CARDINAL;
    dibRow      : DIB_PixelRow;
    pixel       : RgbPixelPointer;
BEGIN
    IF draw^.type = RgbDrawable THEN
        IF y < draw^.height THEN
            dibRow := ADDADR(draw^.dibBits, y * draw^.rowStride);
            pixel := pixels;
            i := 0;

            CASE pixelFormat OF
            Rgb8:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    dibRow^[x].blue := pixel^.rgb8.blue;
                    dibRow^[x].green := pixel^.rgb8.green;
                    dibRow^[x].red := pixel^.rgb8.red;

                    pixel := ADDADR(pixel, SIZE(Rgb8_Pixel));
                    INC(x);
                END;
            |
            Rgba8:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    dibRow^[x].blue := pixel^.rgba8.blue;
                    dibRow^[x].green := pixel^.rgba8.green;
                    dibRow^[x].red := pixel^.rgba8.red;

                    pixel := ADDADR(pixel, SIZE(Rgba8_Pixel));
                    INC(x);
                END;
            |
            Rgb16:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    dibRow^[x].blue := pixel^.rgb16.blue / 257;
                    dibRow^[x].green := pixel^.rgb16.green / 257;
                    dibRow^[x].red := pixel^.rgb16.red / 257;

                    pixel := ADDADR(pixel, SIZE(Rgb16_Pixel));
                    INC(x);
                END;
            |
            Rgba16:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    dibRow^[x].blue := pixel^.rgba16.blue / 257;
                    dibRow^[x].green := pixel^.rgba16.green / 257;
                    dibRow^[x].red := pixel^.rgba16.red / 257;

                    pixel := ADDADR(pixel, SIZE(Rgba16_Pixel));
                    INC(x);
                END;
            |
            Bgr8:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    dibRow^[x].blue := pixel^.bgr8.blue;
                    dibRow^[x].green := pixel^.bgr8.green;
                    dibRow^[x].red := pixel^.bgr8.red;

                    pixel := ADDADR(pixel, SIZE(Bgr8_Pixel));
                    INC(x);
                END;
            |
            Bgra8:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    dibRow^[x].blue := pixel^.bgra8.blue;
                    dibRow^[x].green := pixel^.bgra8.green;
                    dibRow^[x].red := pixel^.bgra8.red;

                    pixel := ADDADR(pixel, SIZE(Bgra8_Pixel));
                    INC(x);
                END;
            |
            Bgr16:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    dibRow^[x].blue := pixel^.bgr16.blue / 257;
                    dibRow^[x].green := pixel^.bgr16.green / 257;
                    dibRow^[x].red := pixel^.bgr16.red / 257;

                    pixel := ADDADR(pixel, SIZE(Bgr16_Pixel));
                    INC(x);
                END;
            |
            Bgra16:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    dibRow^[x].blue := pixel^.bgra16.blue / 257;
                    dibRow^[x].green := pixel^.bgra16.green / 257;
                    dibRow^[x].red := pixel^.bgra16.red / 257;

                    pixel := ADDADR(pixel, SIZE(Bgra16_Pixel));
                    INC(x);
                END;
            |
            Abgr8:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    dibRow^[x].blue := pixel^.abgr8.blue;
                    dibRow^[x].green := pixel^.abgr8.green;
                    dibRow^[x].red := pixel^.abgr8.red;

                    pixel := ADDADR(pixel, SIZE(Abgr8_Pixel));
                    INC(x);
                END;
            |
            Abgr16:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    dibRow^[x].blue := pixel^.abgr16.blue / 257;
                    dibRow^[x].green := pixel^.abgr16.green / 257;
                    dibRow^[x].red := pixel^.abgr16.red / 257;

                    pixel := ADDADR(pixel, SIZE(Abgr16_Pixel));
                    INC(x);
                END;
            END;

            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END CopyPixelRowToRgbDrawable;

PROCEDURE CopyPixelRectToRgbDrawable(draw : Drawable;
                                     destX, destY : CARDINAL;
                                     srcX, srcY : CARDINAL;
                                     width, height : CARDINAL;
                                     rowStride : CARDINAL;
                                     pixels : ADDRESS;
                                     pixelFormat : RgbPixelFormat) : BOOLEAN;
VAR
    pixelStart  : ADDRESS;
BEGIN
    pixelStart := ADDADR(pixels, srcY * rowStride);
    pixelStart := ADDADR(pixelStart, srcX * RgbPixelFormatSize[pixelFormat]);
    WHILE height <> 0 DO
        IF CopyPixelRowToRgbDrawable(draw,
                                     destX, destY,
                                     width,
                                     pixelStart, pixelFormat)
        THEN
            pixelStart := ADDADR(pixelStart, rowStride);
            DEC(height);
            INC(destY);
        ELSE
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END CopyPixelRectToRgbDrawable;

PROCEDURE GetRgbDrawablePixelRow(draw : Drawable;
                                 x, y : CARDINAL;
                                 numPixels : CARDINAL;
                                 pixels : ADDRESS;
                                 pixelFormat : RgbPixelFormat) : CARDINAL;
VAR
    i           : CARDINAL;
    dibRow      : DIB_PixelRow;
    pixel       : RgbPixelPointer;
    count       : CARDINAL;
BEGIN
    IF draw^.type = RgbDrawable THEN
        IF y < draw^.height THEN
            WINGDI.GdiFlush;

            dibRow := ADDADR(draw^.dibBits, y * draw^.rowStride);
            pixel := pixels;
            i := 0;
            count := 0;

            CASE pixelFormat OF
            Rgb8:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    pixel^.rgb8.red := dibRow^[x].red;
                    pixel^.rgb8.green := dibRow^[x].green;
                    pixel^.rgb8.blue := dibRow^[x].blue;

                    pixel := ADDADR(pixel, SIZE(Rgb8_Pixel));
                    INC(x);
                    INC(count);
                END;
            |
            Rgba8:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    pixel^.rgba8.red := dibRow^[x].red;
                    pixel^.rgba8.green := dibRow^[x].green;
                    pixel^.rgba8.blue := dibRow^[x].blue;
                    pixel^.rgba8.alpha := OpaqueOpacity;

                    pixel := ADDADR(pixel, SIZE(Rgba8_Pixel));
                    INC(x);
                    INC(count);
                END;
            |
            Rgb16:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    pixel^.rgb16.red := ORD(dibRow^[x].red) * 257;
                    pixel^.rgb16.green := ORD(dibRow^[x].green) * 257;
                    pixel^.rgb16.blue := ORD(dibRow^[x].blue) * 257;

                    pixel := ADDADR(pixel, SIZE(Rgb16_Pixel));
                    INC(x);
                    INC(count);
                END;
            |
            Rgba16:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    pixel^.rgba16.red := ORD(dibRow^[x].red) * 257;
                    pixel^.rgba16.green := ORD(dibRow^[x].green) * 257;
                    pixel^.rgba16.blue := ORD(dibRow^[x].blue) * 257;
                    pixel^.rgba16.alpha := OpaqueOpacity* 257;

                    pixel := ADDADR(pixel, SIZE(Rgba16_Pixel));
                    INC(x);
                    INC(count);
                END;
            |
            Bgr8:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    pixel^.bgr8.blue := dibRow^[x].blue;
                    pixel^.bgr8.green := dibRow^[x].green;
                    pixel^.bgr8.red := dibRow^[x].red;

                    pixel := ADDADR(pixel, SIZE(Bgr8_Pixel));
                    INC(x);
                    INC(count);
                END;
            |
            Bgra8:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    pixel^.bgra8.blue := dibRow^[x].blue;
                    pixel^.bgra8.green := dibRow^[x].green;
                    pixel^.bgra8.red := dibRow^[x].red;
                    pixel^.bgra8.alpha := OpaqueOpacity;

                    pixel := ADDADR(pixel, SIZE(Bgra8_Pixel));
                    INC(x);
                    INC(count);
                END;
            |
            Bgr16:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    pixel^.bgr16.blue := ORD(dibRow^[x].blue) * 257;
                    pixel^.bgr16.green := ORD(dibRow^[x].green) * 257;
                    pixel^.bgr16.red := ORD(dibRow^[x].red) * 257;

                    pixel := ADDADR(pixel, SIZE(Bgr16_Pixel));
                    INC(x);
                    INC(count);
                END;
            |
            Bgra16:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    pixel^.bgra16.blue := ORD(dibRow^[x].blue) * 257;
                    pixel^.bgra16.green := ORD(dibRow^[x].green) * 257;
                    pixel^.bgra16.red := ORD(dibRow^[x].red) * 257;
                    pixel^.bgra16.alpha := OpaqueOpacity * 257;

                    pixel := ADDADR(pixel, SIZE(Bgra16_Pixel));
                    INC(x);
                    INC(count);
                END;
            |
            Abgr8:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    pixel^.abgr8.alpha := OpaqueOpacity;
                    pixel^.abgr8.blue := dibRow^[x].blue;
                    pixel^.abgr8.green := dibRow^[x].green;
                    pixel^.abgr8.red := dibRow^[x].red;

                    pixel := ADDADR(pixel, SIZE(Abgr8_Pixel));
                    INC(x);
                END;
            |
            Abgr16:
                WHILE (i < numPixels) AND (x < draw^.width) DO
                    INC(i);
                    pixel^.abgr16.alpha := OpaqueOpacity * 257;
                    pixel^.abgr16.blue := ORD(dibRow^[x].blue) * 257;
                    pixel^.abgr16.green := ORD(dibRow^[x].green) * 257;
                    pixel^.abgr16.red := ORD(dibRow^[x].red) * 257;

                    pixel := ADDADR(pixel, SIZE(Abgr16_Pixel));
                    INC(x);
                END;
            END;

            RETURN count;
        END;
    END;
    RETURN 0;
END GetRgbDrawablePixelRow;

PROCEDURE SyncRgbDrawable(draw : Drawable);
BEGIN
    UNREFERENCED_PARAMETER(draw);

    WINGDI.GdiFlush;
END SyncRgbDrawable;

PROCEDURE GetTextWidth(draw : Drawable; text : ARRAY OF CHAR; length : CARDINAL) : CARDINAL;
VAR
    pt  : WIN32.WSIZE;
BEGIN
    IF draw^.winDC <> NIL THEN
        IF length = 0 THEN
            length := LENGTH(text);
        END;
        WINGDI.GetTextExtentPoint32(draw^.winDC, text, length, pt);
        RETURN pt.cx;
    END;
    RETURN 0;
END GetTextWidth;

PROCEDURE DrawText(draw : Drawable; x, y : COORDINATE; text : ARRAY OF CHAR; length : CARDINAL);
BEGIN
    IF (draw^.winDC <> NIL) AND (draw^.context <> NIL) THEN
        IF length = 0 THEN
            length := LENGTH(text);
        END;

        TextOut(draw^.winDC, x, y, text, length);
    END;
END DrawText;

PROCEDURE DrawTextRect(draw : Drawable;
                       x, y : COORDINATE;
                       text : ARRAY OF CHAR;
                       length : CARDINAL;
                       rect : wsRECT;
                       flags : DrawTextOptionSet);
VAR
    r           : RECT;
    winFlags    : DWORD;
BEGIN
    IF (draw^.winDC <> NIL) AND (draw^.context <> NIL) THEN
        IF length = 0 THEN
            length := LENGTH(text);
        END;

        r.left := rect.x1;
        r.top := rect.y1;
        r.right := rect.x2;
        r.bottom := rect.y2;

        winFlags := 0;
        IF DT_CLIPPED IN flags THEN
            winFlags := winFlags BOR ETO_CLIPPED;
        END;
        IF DT_OPAQUE IN flags THEN
            winFlags := winFlags BOR ETO_OPAQUE;
        END;

        ExtTextOut(draw^.winDC, x, y, winFlags, r, text, length, NIL_WINT);
    END;
END DrawTextRect;

PROCEDURE GetWindowsPen(draw : Drawable);
CONST
    penStyle    : ARRAY LineStyles OF CARDINAL =
        {PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT};
TYPE
    dashArray   = ARRAY [0..5] OF CARDINAL;
VAR
    pen         : HPEN;
    style       : DWORD;
    lb          : LOGBRUSH;
    values      : DrawContextValues;
    dashCount   : CARDINAL;
    dashes      : dashArray;
    dashPtr     : POINTER TO dashArray;
BEGIN
    IF (draw^.pen = NIL) OR
       (draw^.penValues.foreground <> draw^.context^.values.foreground) OR
       (draw^.penValues.lineWidth <> draw^.context^.values.lineWidth) OR
       (draw^.penValues.lineStyle <> draw^.context^.values.lineStyle) OR
       (draw^.penValues.endCap <> draw^.context^.values.endCap) OR
       (draw^.penValues.joinStyle <> draw^.context^.values.joinStyle)
    THEN
        IF draw^.pen <> NIL THEN
            IF draw^.savePen <> NIL THEN
                SelectPen(draw^.winDC, draw^.savePen);
            END;

            DeletePen(draw^.pen);
            draw^.pen := NIL;
        END;

        values := draw^.context^.values;

        dashPtr := NIL;
        dashCount := 0;

        IF values.lineWidth = 1 THEN
            style := penStyle[values.lineStyle] BOR PS_COSMETIC;
        ELSE
            CASE values.lineStyle OF
            SolidLine:
                style := PS_SOLID BOR PS_GEOMETRIC;
            |
            DashLine:
                style := PS_USERSTYLE BOR PS_GEOMETRIC;
                dashes[0] := values.lineWidth * 3;
                dashes[1] := values.lineWidth;
                dashCount := 2;
                dashPtr := ADR(dashes);
            |
            DotLine:
                style := PS_USERSTYLE BOR PS_GEOMETRIC;
                dashes[0] := values.lineWidth;
                dashes[1] := values.lineWidth;
                dashCount := 2;
                dashPtr := ADR(dashes);
            |
            DashDotLine:
                style := PS_USERSTYLE BOR PS_GEOMETRIC;
                dashes[0] := values.lineWidth * 3;
                dashes[1] := values.lineWidth;
                dashes[2] := values.lineWidth;
                dashes[3] := values.lineWidth;
                dashCount := 4;
                dashPtr := ADR(dashes);
            |
            DashDotDotLine:
                style := PS_USERSTYLE BOR PS_GEOMETRIC;
                dashes[0] := values.lineWidth * 3;
                dashes[1] := values.lineWidth;
                dashes[2] := values.lineWidth;
                dashes[3] := values.lineWidth;
                dashes[4] := values.lineWidth;
                dashes[5] := values.lineWidth;
                dashCount := 6;
                dashPtr := ADR(dashes);
            END;
        END;

        CASE values.joinStyle OF
        JoinBevel:
            style := style BOR PS_JOIN_BEVEL;
        |
        JoinMiter:
            style := style BOR PS_JOIN_MITER;
        |
        JoinRound:
            style := style BOR PS_JOIN_ROUND;
        END;

        CASE values.endCap OF
        EndCapRound:
            style := style BOR PS_ENDCAP_ROUND;
        |
        EndCapFlat:
            style := style BOR PS_ENDCAP_FLAT;
        |
        EndCapSquare:
            style := style BOR PS_ENDCAP_SQUARE;
        END;

        lb.lbColor := values.foreground;
        lb.lbStyle := BS_SOLID;

        pen := ExtCreatePen(style, values.lineWidth, lb, dashCount, dashPtr^);
        draw^.pen := pen;
        draw^.penValues := values;

        pen := SelectPen(draw^.winDC, pen);
        IF draw^.savePen = NIL THEN
            draw^.savePen := pen;
        END;
    END;
END GetWindowsPen;

PROCEDURE GetWindowsBrush(draw : Drawable; foreground : BOOLEAN);
VAR
    brush       : HBRUSH;
    color       : ColorValue;
BEGIN
    IF foreground THEN
        color := draw^.context^.values.foreground;
    ELSE
        color := draw^.context^.values.background;
    END;

    IF (draw^.brush = NIL) OR (draw^.brushColor <> color) THEN
        IF draw^.brush <> NIL THEN
            IF draw^.saveBrush <> NIL THEN
                SelectBrush(draw^.winDC, draw^.saveBrush);
            END;

            DeleteBrush(draw^.brush);
            draw^.brush := NIL;
        END;

        brush := CreateSolidBrush(color);
        draw^.brush := brush;
        draw^.brushColor := color;

        brush := SelectBrush(draw^.winDC, brush);
        IF draw^.saveBrush = NIL THEN
            draw^.saveBrush := brush;
        END;
    END;
END GetWindowsBrush;

PROCEDURE DrawPixel(draw : Drawable; x, y : COORDINATE; color : ColorValue);
VAR
    dibRow      : DIB_PixelRow;
BEGIN
    IF (draw^.winDC <> NIL) AND (draw^.context <> NIL) THEN
        IF draw^.type <> RgbDrawable THEN
            SetPixelV(draw^.winDC, x, y, color);
        ELSE
            (* using SetPixelV will work, but this might be faster
               since the GDI probably batch processes a bunch of DIB section
               pixel draws.
            *)

            IF (x >= 0) AND (y >= 0) AND
               (x < INT(draw^.width)) AND (y < INT(draw^.height))
            THEN
                dibRow := ADDADR(draw^.dibBits, ORD(y) * draw^.rowStride);
                ColorValueToRgb(color, dibRow^[x].red, dibRow^[x].green, dibRow^[x].blue);
            END;
        END;
    END;
END DrawPixel;

PROCEDURE DrawLine(draw : Drawable; x1, y1, x2, y2 : COORDINATE);
BEGIN
    IF (draw^.winDC <> NIL) AND (draw^.context <> NIL) THEN
        GetWindowsPen(draw);

        MoveToEx(draw^.winDC, x1, y1, NIL_POINT);
        LineTo(draw^.winDC, x2, y2);
    END;
END DrawLine;

PROCEDURE DrawLines(draw : Drawable; points : ARRAY OF wsPOINT);
CONST
    maxPoints   = 63;
VAR
    i           : ADRCARD;
    highPoints  : ADRCARD;
    data        : ARRAY [0..maxPoints] OF POINT;
    pointsW     : POINTER TO ARRAY [0..0] OF POINT;
BEGIN
    IF (draw^.winDC <> NIL) AND (draw^.context <> NIL) THEN
        GetWindowsPen(draw);

        IF HIGH(points) <= maxPoints THEN
            data[0].x := 32;(*suppress uninit warning*)
            pointsW := ADR(data);
        ELSE
            ALLOCATE(pointsW, (HIGH(points)+1)*SIZE(POINT));
        END;

        highPoints := HIGH(points);
        FOR i := 0 TO highPoints DO
            pointsW^[i].x := points[i].x;
            pointsW^[i].y := points[i].y;
        END;
        Polyline(draw^.winDC, pointsW^, HIGH(points)+1);

        IF HIGH(points) > maxPoints THEN
            DEALLOCATE(pointsW, (HIGH(points)+1)*SIZE(POINT));
        END;
    END;
END DrawLines;

PROCEDURE DrawRectangle(draw : Drawable;
                        x, y : COORDINATE;
                        width, height : CARDINAL;
                        filled : BOOLEAN);
VAR
    saveBrush   : HBRUSH;
BEGIN
    IF (draw^.winDC <> NIL) AND (draw^.context <> NIL) THEN
        IF filled THEN
            GetWindowsBrush(draw, TRUE);
            PatBlt(draw^.winDC, x, y, width, height, Win32PatBltOps[draw^.context^.values.drawFunc]);
        ELSE
            GetWindowsPen(draw);
            saveBrush := SelectBrush(draw^.winDC, GetStockBrush(NULL_BRUSH));
            Rectangle(draw^.winDC, x, y, x + INT(width), y + INT(height));
            SelectBrush(draw^.winDC, saveBrush);
        END;
    END;
END DrawRectangle;

PROCEDURE EraseRectangle(draw : Drawable; x, y : COORDINATE; width, height : CARDINAL);
BEGIN
    IF (draw^.winDC <> NIL) AND (draw^.context <> NIL) THEN
        GetWindowsBrush(draw, FALSE);
        PatBlt(draw^.winDC, x, y, width, height, Win32PatBltOps[draw^.context^.values.drawFunc]);
    END;
END EraseRectangle;

PROCEDURE DrawPie(draw : Drawable;
                  x, y : COORDINATE;
                  radius : CARDINAL;
                  startAngle, arcAngle : REAL;
                  filled : BOOLEAN);
CONST
    deg2rad = (2.0*pi) / 360.0;
VAR
    context     : DrawContext;
    startArcX,
    startArcY,
    endArcX,
    endArcY     : COORDINATE;
    saveBrush   : HBRUSH;
    savePen     : HPEN;
BEGIN
    IF (draw^.winDC <> NIL) AND (draw^.context <> NIL) THEN
        context := draw^.context;

        IF filled THEN
            INC(radius);(* because we use a null pen *)
        END;

        startArcX := round(FLOAT(x) + (FLOAT(radius) * cos(startAngle * deg2rad)));
        startArcY := round(FLOAT(y) - (FLOAT(radius) * sin(startAngle * deg2rad)));

        arcAngle := startAngle + arcAngle;
        endArcX := round(FLOAT(x) + (FLOAT(radius) * cos(arcAngle * deg2rad)));
        endArcY := round(FLOAT(y) - (FLOAT(radius) * sin(arcAngle * deg2rad)));

        IF filled THEN
            GetWindowsBrush(draw, TRUE);
            saveBrush := NIL;(* suppress uninit warning *)
            savePen := SelectPen(draw^.winDC, GetStockPen(NULL_PEN));
        ELSE
            GetWindowsPen(draw);
            savePen := NIL;(* suppress uninit warning *)
            saveBrush := SelectBrush(draw^.winDC, GetStockBrush(NULL_BRUSH));
        END;

        IF (startArcX = endArcX) AND
           (startArcY = endArcY) AND
           (arcAngle < 180.)
        THEN
            MoveToEx(draw^.winDC, x, y, NIL_POINT);
            LineTo(draw^.winDC, endArcX, endArcY)
        ELSE
            Pie(draw^.winDC,
                x-INT(radius), y-INT(radius),
                x+INT(radius), y+INT(radius),
                startArcX, startArcY,
                endArcX, endArcY);
        END;

        IF filled THEN
            SelectPen(draw^.winDC, savePen);
        ELSE
            SelectBrush(draw^.winDC, saveBrush);
        END;
    END;
END DrawPie;

PROCEDURE DrawBitmap(destDraw : Drawable; destX, destY : COORDINATE; bmp : BitmapHandle);

VAR
    info        : BITMAP;
BEGIN
    IF bmp <> NIL THEN
        IF GetBITMAP(bmp, info) THEN
            DrawBitmapRect(destDraw, destX, destY, bmp, 0, 0, info.bmWidth, info.bmHeight);
        END;
    END;
END DrawBitmap;

PROCEDURE DrawBitmapRect(destDraw : Drawable;
                         destX, destY : COORDINATE;
                         bmp : BitmapHandle;
                         srcX, srcY : COORDINATE;
                         width, height : CARDINAL);
VAR
    srcDC       : HDC;
BEGIN
    IF (bmp <> NIL) AND
       (destDraw^.winDC <> NIL) AND
       (destDraw^.context <> NIL)
    THEN
        srcDC := CreateCompatibleDC(destDraw^.winDC);
        SelectBitmap(srcDC, bmp);

        BitBlt(destDraw^.winDC,
               destX, destY,
               width, height,
               srcDC,
               srcX, srcY,
               Win32BitBltOps[destDraw^.context^.values.drawFunc]);

        DeleteDC(srcDC);
    END;
END DrawBitmapRect;

PROCEDURE CopyDrawableArea(destDraw : Drawable;
                           destX, destY : COORDINATE;
                           srcDraw : Drawable;
                           srcX, srcY : COORDINATE;
                           width, height : CARDINAL);
BEGIN
    IF (srcDraw^.winDC <> NIL) AND
       (destDraw^.winDC <> NIL) AND
       (destDraw^.context <> NIL)
    THEN
        BitBlt(destDraw^.winDC,
               destX, destY,
               width, height,
               srcDraw^.winDC,
               srcX, srcY,
               Win32BitBltOps[destDraw^.context^.values.drawFunc]);
    END;
END CopyDrawableArea;

PROCEDURE ScaleDrawableArea(destDraw : Drawable;
                            destX, destY : COORDINATE;
                            destWidth, destHeight : CARDINAL;
                            srcDraw : Drawable;
                            srcX, srcY : COORDINATE;
                            srcWidth, srcHeight : CARDINAL;
                            interp : PixelInterpolation);
CONST
    gdipInterp  : ARRAY PixelInterpolation OF InterpolationMode =
        {InterpolationModeLowQuality,
         InterpolationModeBilinear,
         InterpolationModeHighQuality};

VAR
    graphics    : GpGraphics;
    image       : GpImage;
BEGIN
    IF (srcDraw^.winDC <> NIL) AND
       (destDraw^.winDC <> NIL) AND
       (destDraw^.context <> NIL)
    THEN
        IF (interp = InterpLowQuality) OR
           (srcDraw^.type = WindowDrawable) OR
           (NOT InitGdiplus())
        THEN
            StretchBlt(destDraw^.winDC,
                       destX, destY,
                       destWidth, destHeight,
                       srcDraw^.winDC,
                       srcX, srcY,
                       srcWidth, srcHeight,
                       Win32BitBltOps[destDraw^.context^.values.drawFunc]);
        ELSE
            Gdip.GdipCreateFromHDC(destDraw^.winDC, graphics);
            Gdip.GdipSetInterpolationMode(graphics, gdipInterp[interp]);

            IF srcDraw^.type <> RgbDrawable THEN
                SelectBitmap(srcDraw^.winDC, srcDraw^.saveBmp);
                Gdip.GdipCreateBitmapFromHBITMAP(srcDraw^.bmp, NIL, image);
            ELSE
                Gdip.GdipCreateBitmapFromGdiDib(srcDraw^.dibHeader, srcDraw^.dibBits, image);
            END;

            Gdip.GdipDrawImageRectRectI(graphics,
                                        image,
                                        destX, destY, destWidth, destHeight,
                                        srcX, srcY, srcWidth, srcHeight,
                                        UnitPixel,
                                        NIL,(*attribs*)
                                        CAST(DrawImageAbort, NIL), NIL);

            Gdip.GdipDeleteGraphics(graphics);
            Gdip.GdipDisposeImage(image);

            IF srcDraw^.type <> RgbDrawable THEN
                SelectBitmap(srcDraw^.winDC, srcDraw^.bmp);
            END;
        END;
    END;
END ScaleDrawableArea;

PROCEDURE DisplayHelp(w : Window; command : HelpCommand; helpFile : ARRAY OF CHAR; helpIndex : CARDINAL) : BOOLEAN;
CONST
    cmdWH : ARRAY HelpCommand OF CARDINAL = {HELP_CONTEXT, HELP_FINDER};
    cmdHH : ARRAY HelpCommand OF CARDINAL = {HH_HELP_CONTEXT, HH_DISPLAY_TOC};
VAR
    parts       : FileNameParts;
    ok          : BOOLEAN;
    startup     : STARTUPINFO;
    info        : PROCESS_INFORMATION;
BEGIN
    ParseFileName(helpFile, parts);
    IF EqualI(parts.extension, ".chm") THEN
        IF NOT HtmlHelpInit THEN
            HtmlHelpInit := TRUE;
            HtmlHelp(NIL, NIL_STR, HH_INITIALIZE, CAST(DWORD_PTR, ADR(HtmlHelpCookie)));
        END;

        IF HtmlHelp(w^.wnd, helpFile, cmdHH[command], helpIndex) <> NIL THEN
            RETURN TRUE;
        END;

    ELSIF EqualI(parts.extension, ".hlp") THEN
        IF WinHelp(w^.wnd, helpFile, cmdWH[command], helpIndex) THEN
            w^.calledHelp := TRUE;
            RETURN TRUE;
        END;
    ELSE
        startup.cb := SIZE(startup);
        startup.lpReserved := NIL;
        startup.lpDesktop := NIL;
        startup.lpTitle := NIL;
        startup.dwFlags := 0;
        startup.dwX := 0;
        startup.dwY := 0;
        startup.dwXSize := 0;
        startup.dwYSize := 0;
        startup.cbReserved2 := 0;
        startup.lpReserved2 := NIL;

        ok := CreateProcess(WINX.NIL_STR,   (* program name *)
                            helpFile,(* command line *)
                            WINX.NIL_SECURITY_ATTRIBUTES,(* process security *)
                            WINX.NIL_SECURITY_ATTRIBUTES,(* thread 0 security *)
                            FALSE,              (* Inherit handles *)
                            WIN32.CREATE_NEW_PROCESS_GROUP,
                            NIL,               (* copy our environment *)
                            WINX.NIL_STR,   (* use our directory *)
                            startup,
                            info);
        IF ok THEN
            WIN32.CloseHandle(info.hThread);
            WIN32.CloseHandle(info.hProcess);
            RETURN TRUE;
        END;
    END;

    RETURN FALSE;
END DisplayHelp;

PROCEDURE CheckListClientColumnAutosize(w : Window; column : ADRCARD);
VAR
    info        : ColumnAutoSizePointer;
BEGIN
    IF (NOT w^.autoSizePosted) AND
       (
        (w^.columnInfo = NIL) OR
        (w^.columnInfo^[column].width < 0)
       )
    THEN
        (* we post this message so a bunch of item adds can be done
           and we will do the autosize after all have been done.
        *)
        w^.autoSizePosted := TRUE;

        NEW(info);
        info^.wnd := w^.clientWnd;
        info^.numColumns := w^.numColumns;
        info^.info := w^.columnInfo;
        info^.posted := ADR(w^.autoSizePosted);
        WINUSER.PostMessage(w^.clientWnd,
                            WM_COLUMN_AUTOSIZE,
                            CAST(WPARAM, info), 0);
    END;
END CheckListClientColumnAutosize;

PROCEDURE CheckListClientSort(w : Window);
BEGIN
    IF NOT w^.sortPosted THEN
        (* we post this message so a bunch of item adds can be done
           and we will do the sort after all have been done.
        *)
        w^.sortPosted := TRUE;
        WINUSER.PostMessage(w^.clientWnd, WM_LIST_SORT, 0, 0);
    END;
END CheckListClientSort;

PROCEDURE ListClientInsertItem(w : Window; item : ListClientInsertRec) : INTEGER;
VAR
    retVal      : INTEGER;
    lvItem      : LVITEM;
    text        : ARRAY [0..255] OF CHAR;
    i           : CARDINAL;

    PROCEDURE getText(i : ADRCARD);
    BEGIN
        IF item.textPtr[i] <> NIL THEN
            lvItem.pszText := CAST(LPTSTR, item.textPtr[i]);
        ELSE
            text[0] := '';
            IF item.textId[i] >= 0 THEN
                LoadString(item.textId[i], text);
            END;
            lvItem.pszText := ADR(text);
        END;
    END getText;

    PROCEDURE getItem(i : ADRCARD);
    BEGIN
        IF (w^.columnInfo^[i].content = ColumnText) OR
           (w^.columnInfo^[i].content = ColumnBitmapText)
        THEN
            lvItem.mask := lvItem.mask BOR LVIF_TEXT;
            getText(i);
        END;
        IF (w^.columnInfo^[i].content = ColumnBitmap) OR
           (w^.columnInfo^[i].content = ColumnBitmapText)
        THEN
            lvItem.mask := lvItem.mask BOR LVIF_IMAGE;
            lvItem.iImage := item.bmpId[i];
        END;
    END getItem;

BEGIN
    retVal := -1;

    IF w^.clientType = ListClient THEN
        lvItem.mask := LVIF_PARAM;
        lvItem.lParam := CAST(LPARAM, item.userData);
        IF item.insertPos < 0 THEN
            lvItem.iItem := MAX(INTEGER);
        ELSE
            lvItem.iItem := item.insertPos;
        END;
        lvItem.iSubItem := 0;
        getItem(0);
        retVal := ListView_InsertItem(w^.clientWnd, lvItem);
        CheckListClientColumnAutosize(w, 0);

        IF retVal >= 0 THEN
            FOR i := 1 TO item.numColumns-1 DO
                IF i < w^.numColumns THEN
                    lvItem.mask := 0;
                    lvItem.iItem := retVal;
                    lvItem.iSubItem := i;
                    getItem(i);
                    ListView_SetItem(w^.clientWnd, lvItem);
                    CheckListClientColumnAutosize(w, i);
                END;
            END;
        END;

        CheckListClientSort(w);
    END;

    RETURN retVal;
END ListClientInsertItem;

PROCEDURE ListClientSetItem(w : Window; row, column : CARDINAL; text : ARRAY OF CHAR;
                            image : CARDINAL);
CONST
    mask        : ARRAY ColumnContentTypes OF CARDINAL =
        {LVIF_TEXT, LVIF_IMAGE, LVIF_TEXT BOR LVIF_IMAGE};
VAR
    lvItem      : LVITEM;
BEGIN
    IF (w^.clientType = ListClient) AND (column < w^.numColumns) THEN
        lvItem.mask := mask[w^.columnInfo^[column].content];
        lvItem.iItem := row;
        lvItem.iSubItem := column;
        lvItem.iImage := image;
        lvItem.pszText := ADR(text);
        ListView_SetItem(w^.clientWnd, lvItem);

        CheckListClientColumnAutosize(w, column);
        IF INT(column) = w^.sortColumn THEN
            CheckListClientSort(w);
        END;
    END;
END ListClientSetItem;

PROCEDURE ListClientGetItem(w : Window; row, column : CARDINAL; VAR OUT text : ARRAY OF CHAR; VAR OUT image : CARDINAL);
CONST
    mask        : ARRAY ColumnContentTypes OF CARDINAL =
        {LVIF_TEXT, LVIF_IMAGE, LVIF_TEXT BOR LVIF_IMAGE};
VAR
    count       : CARDINAL;
    lvItem      : LVITEM;
BEGIN
    text := "";
    image := 0;
    IF (w^.clientType = ListClient) AND (column < w^.numColumns) THEN
        count := ListView_GetItemCount(w^.clientWnd);
        IF row < count THEN
            lvItem.mask := mask[w^.columnInfo^[column].content];
            lvItem.iItem := row;
            lvItem.iSubItem := column;
            lvItem.pszText := ADR(text);
            lvItem.cchTextMax := HIGH(text)+1;
            lvItem.iImage := 0;
            ListView_GetItem(w^.clientWnd, lvItem);
            image := lvItem.iImage;
        END;
    END;
END ListClientGetItem;

PROCEDURE ListClientGetItemData(w : Window; row : CARDINAL) : ADDRESS;
VAR
    lvItem      : LVITEM;
BEGIN
    IF w^.clientType = ListClient THEN
        lvItem.mask := LVIF_PARAM;
        lvItem.iItem := row;
        lvItem.iSubItem := 0;
        ListView_GetItem(w^.clientWnd, lvItem);
        RETURN CAST(ADDRESS, lvItem.lParam);
    END;
    RETURN NIL;
END ListClientGetItemData;

PROCEDURE ListClientSetItemData(w : Window; row : CARDINAL; data : ADDRESS);
VAR
    lvItem      : LVITEM;
BEGIN
    IF w^.clientType = ListClient THEN
        lvItem.mask := LVIF_PARAM;
        lvItem.lParam := CAST(LPARAM, data);
        lvItem.iItem := row;
        lvItem.iSubItem := 0;
        ListView_SetItem(w^.clientWnd, lvItem);
    END;
END ListClientSetItemData;

PROCEDURE ListClientFindItemByData(w : Window; data : ADDRESS) : INTEGER;
VAR
    lvFind      : LVFINDINFO;
    sel         : INTEGER;
BEGIN
    IF w^.clientType = ListClient THEN
        lvFind.flags := LVFI_PARAM BOR LVFI_PARTIAL;
        lvFind.lParam := CAST(LPARAM, data);
        sel := ListView_FindItem(w^.clientWnd, -1, lvFind);
        IF sel >= 0 THEN
            RETURN sel;
        END;
    END;
    RETURN -1;
END ListClientFindItemByData;

PROCEDURE ListClientGetSelectionCount(w : Window) : CARDINAL;
BEGIN
    IF w^.clientType = ListClient THEN
        RETURN ListView_GetSelectedCount(w^.clientWnd);
    END;
    RETURN 0;
END ListClientGetSelectionCount;

PROCEDURE ListClientGetSelectedItems(w : Window; VAR OUT items : ARRAY OF CARDINAL) : CARDINAL;
VAR
    count       : INTEGER;
    sel         : INTEGER;
    i           : ADRCARD;
    highItems   : ADRCARD;
BEGIN
    IF w^.clientType = ListClient THEN
        count := ListView_GetSelectedCount(w^.clientWnd);
        IF count > 0 THEN
            sel := -1;
            FOR i := 0 TO VAL(ADRCARD, count)-1 DO
                sel := ListView_GetNextItem(w^.clientWnd, sel, LVNI_SELECTED);
                IF sel <> -1 THEN
                    highItems := HIGH(items);
                    IF i <= highItems THEN
                        items[i] := sel;
                    END;
                END;
            END;
            RETURN count;
        END;
    END;
    RETURN 0;
END ListClientGetSelectedItems;

PROCEDURE ListClientGetItemSelectedState(w : Window; row : CARDINAL) : BOOLEAN;
VAR
    result : UINT;
    retval : BOOLEAN;
BEGIN
    retval := FALSE;
    IF w^.clientType = ListClient THEN
        result := ListView_GetItemState(w^.clientWnd, row, LVIS_SELECTED);
        IF (result BAND LVIS_SELECTED) # 0 THEN
            retval := TRUE;
        END
    END;
    RETURN retval;
END ListClientGetItemSelectedState;

PROCEDURE ListClientSetSelectedItem(w : Window; sel : CARDINAL) : BOOLEAN;
BEGIN
    IF w^.clientType = ListClient THEN
        ListView_EnsureVisible(w^.clientWnd, sel, TRUE);
        RETURN ListView_SetItemState(w^.clientWnd,
                                     sel,
                                     LVIS_SELECTED BOR LVIS_FOCUSED,
                                     LVIS_SELECTED BOR LVIS_FOCUSED) <> FALSE;
    END;
    RETURN FALSE;
END ListClientSetSelectedItem;

PROCEDURE ListClientRemoveItem(w : Window; item : CARDINAL);
BEGIN
    IF w^.clientType = ListClient THEN
        ListView_DeleteItem(w^.clientWnd, item);
    END;
END ListClientRemoveItem;

PROCEDURE ListClientRemoveAllItems(w : Window);
BEGIN
    IF w^.clientType = ListClient THEN
        ListView_DeleteAllItems(w^.clientWnd);
    END;
END ListClientRemoveAllItems;

PROCEDURE ListClientFreezeDisplay(w : Window; yes : BOOLEAN);
BEGIN
    IF w^.clientType = ListClient THEN
        IF yes THEN
            WINUSER.SendMessage(w^.clientWnd, WM_SETREDRAW, ORD(FALSE), 0);
        ELSE
            WINUSER.SendMessage(w^.clientWnd, WM_SETREDRAW, ORD(TRUE), 0);
            InvalidateRect(w^.clientWnd, NIL_RECT, TRUE);
        END;
    END;
END ListClientFreezeDisplay;

PROCEDURE ListClientGetColumnWidth(w : Window; column : ADRCARD) : INTEGER;
BEGIN
    IF w^.clientType = ListClient THEN
        <*/PUSH/NOWARN:U*>
        IF column < VAL(ADRCARD, w^.numColumns) THEN
        <*/POP*>
            IF w^.columnInfo <> NIL THEN
                IF w^.columnInfo^[column].width < 0 THEN
                    RETURN -1;
                END;
            END;
            RETURN ListView_GetColumnWidth(w^.clientWnd, column);
        END;
    END;
    RETURN -1;
END ListClientGetColumnWidth;

PROCEDURE ListClientSetColumnWidth(w : Window; column : ADRCARD; width : INTEGER);
BEGIN
    IF w^.clientType = ListClient THEN
        <*/PUSH/NOWARN:U*>
        IF column < VAL(ADRCARD, w^.numColumns) THEN
        <*/POP*>
            IF w^.columnInfo <> NIL THEN
                w^.columnInfo^[column].width := width;
            END;
            IF width >= 0 THEN
                ListView_SetColumnWidth(w^.clientWnd, column, width);
            ELSE
                CheckListClientColumnAutosize(w, column);
            END;
        END;
    END;
END ListClientSetColumnWidth;

PROCEDURE ListClientSetSortColumn(w : Window; column : INTEGER; dir : SortDirection);
CONST
    flags       = HDF_BITMAP_ON_RIGHT BOR HDF_IMAGE;
VAR
    item        : HDITEM;
BEGIN
    IF (w^.clientType = ListClient) AND (w^.headerWnd <> NIL) THEN
        IF w^.sortColumn >= 0 THEN
            item.mask := HDI_FORMAT;
            Header_GetItem(w^.headerWnd, w^.sortColumn, item);
            item.mask := HDI_FORMAT;
            item.fmt := item.fmt BAND (BNOT flags);
            Header_SetItem(w^.headerWnd, w^.sortColumn, item);
        END;

        IF (column >= INT(w^.numColumns)) OR
           (w^.columnInfo = NIL) OR
           (NOT w^.columnInfo^[column].sortable)
        THEN
            column := -1;
        END;
        w^.sortColumn := column;
        w^.sortDirection := dir;

        IF w^.sortColumn >= 0 THEN
            item.mask := HDI_FORMAT;
            Header_GetItem(w^.headerWnd, w^.sortColumn, item);
            item.mask := HDI_FORMAT BOR HDI_IMAGE;
            item.fmt := item.fmt BOR flags;
            item.iImage := 1;
            IF w^.sortDirection = SortAscending THEN
                item.iImage := 0;
            END;
            Header_SetItem(w^.headerWnd, w^.sortColumn, item);

            ListClientSort(w);
        END;
    END;
END ListClientSetSortColumn;

PROCEDURE ListClientGetSortColumn(w : Window; VAR OUT column : INTEGER; VAR OUT dir : SortDirection);
BEGIN
    column := -1;
    dir := SortAscending;
    IF w^.clientType = ListClient THEN
        column := w^.sortColumn;
        dir := w^.sortDirection;
    END;
END ListClientGetSortColumn;

PROCEDURE ListClientWinCompareProc(item1, item2, data : LPARAM) : INTEGER [EXPORT, WINDOWS];
VAR
    w           : Window;
BEGIN
    w := CAST(Window, data);
    RETURN w^.compareProc(CAST(ADDRESS, item1), CAST(ADDRESS, item2), w^.sortColumn, w^.compareData);
EXCEPT
    RETURN 0;
END ListClientWinCompareProc;

PROCEDURE DefaultListClientCompareProc(item1, item2, data : LPARAM) : INTEGER [EXPORT, WINDOWS];
VAR
    w           : Window;
    text1,
    text2       : ARRAY [0..127] OF CHAR;
    res         : CompareResults;
BEGIN
    w := CAST(Window, data);
    IF (w <> NIL) AND (w^.sortColumn >= 0) AND (w^.sortColumn < INT(w^.numColumns)) THEN
        text1 := "";
        text2 := "";
        ListView_GetItemText(w^.clientWnd, item1, w^.sortColumn, text1, HIGH(text1)+1);
        ListView_GetItemText(w^.clientWnd, item2, w^.sortColumn, text2, HIGH(text2)+1);
        res := Compare(text1, text2);
        IF w^.sortDirection = SortAscending THEN
            RETURN INT(res) - 1;
        ELSE
            RETURN -(INT(res) - 1);
        END;
    END;
    RETURN 0;

EXCEPT
    RETURN 0;
END DefaultListClientCompareProc;

PROCEDURE ListClientSort(w : Window);
VAR
    msg         : MessageRec;
BEGIN
    IF (w^.clientType = ListClient) AND (w^.sortColumn >= 0) THEN
        IF w^.compareProc = NILPROC THEN
            ListView_SortItemsEx(w^.clientWnd, DefaultListClientCompareProc, CAST(LPARAM, w));
        ELSE
            ListView_SortItems(w^.clientWnd, ListClientWinCompareProc, CAST(LPARAM, w));
        END;

        msg.msg := WSM_LIST_SORTED;
        CallWndProc(w, msg);
    END;
END ListClientSort;

PROCEDURE TreeClientInsertNode(w : Window; item : TreeClientInsertRec) : TreeClientNode;
VAR
    tvIns       : TVINSERTSTRUCT;
    text        : ARRAY [0..255] OF CHAR;
    node        : TreeClientNode;
BEGIN
    IF w^.clientType = TreeClient THEN
        IF item.parent <> NIL THEN
            tvIns.hParent := item.parent;
        ELSE
            tvIns.hParent := TVI_ROOT;
        END;
        IF item.sibling = NIL THEN
            IF item.insertBefore THEN
                tvIns.hInsertAfter := TVI_FIRST;
            ELSE
                tvIns.hInsertAfter := TVI_LAST;
            END;
        ELSE
            IF item.insertBefore THEN
                tvIns.hInsertAfter := TreeView_GetPrevSibling(w^.clientWnd, item.sibling);
                IF tvIns.hInsertAfter = NIL THEN
                    tvIns.hInsertAfter := TVI_FIRST;
                END;
            ELSE
                tvIns.hInsertAfter := item.sibling;
            END;
        END;

        tvIns.item.mask := TVIF_TEXT BOR TVIF_PARAM;
        tvIns.item.lParam := CAST(LPARAM, item.userData);
        IF item.textPtr <> NIL THEN
            tvIns.item.pszText := CAST(LPTSTR, item.textPtr);
        ELSE
            text[0] := '';
            IF item.textId >= 0 THEN
                LoadString(item.textId, text);
            END;
            tvIns.item.pszText := ADR(text);
        END;
        node := TreeView_InsertItem(w^.clientWnd, tvIns);

        IF item.folder AND (node <> NIL) THEN
            SetTreeNodeFolderImage(w, node, FALSE);
        END;

        RETURN node;
    END;
    RETURN NIL;
END TreeClientInsertNode;

PROCEDURE TreeClientSetNodeText(w : Window;
                                node : TreeClientNode;
                                text : ARRAY OF CHAR);
VAR
    tvItem      : TVITEM;
BEGIN
    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        tvItem.mask := TVIF_TEXT;
        tvItem.pszText := ADR(text);
        tvItem.hItem := node;
        TreeView_SetItem(w^.clientWnd, tvItem);
    END;
END TreeClientSetNodeText;

PROCEDURE TreeClientGetNodeText(w : Window;
                                node : TreeClientNode;
                                VAR OUT text : ARRAY OF CHAR);
VAR
    tvItem      : TVITEM;
BEGIN
    text[0] := '';

    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        tvItem.mask := TVIF_TEXT;
        tvItem.pszText := ADR(text);
        tvItem.cchTextMax := HIGH(text)+1;
        tvItem.hItem := node;
        TreeView_GetItem(w^.clientWnd, tvItem);
    END;
END TreeClientGetNodeText;

PROCEDURE TreeClientGetNodeData(w : Window; node : TreeClientNode) : ADDRESS;
VAR
    tvItem      : TVITEM;
BEGIN
    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        tvItem.mask := TVIF_PARAM;
        tvItem.hItem := node;
        TreeView_GetItem(w^.clientWnd, tvItem);
        RETURN CAST(ADDRESS, tvItem.lParam);
    END;
    RETURN NIL;
END TreeClientGetNodeData;

PROCEDURE TreeClientSetNodeData(w : Window; node : TreeClientNode; data : ADDRESS);
VAR
    tvItem      : TVITEM;
BEGIN
    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        tvItem.mask := TVIF_PARAM;
        tvItem.lParam := CAST(LPARAM, data);
        tvItem.hItem := node;
        TreeView_SetItem(w^.clientWnd, tvItem);
    END;
END TreeClientSetNodeData;

PROCEDURE TreeClientGetSelectedNode(w : Window) : TreeClientNode;
BEGIN
    IF w^.clientType = TreeClient THEN
        RETURN TreeView_GetSelection(w^.clientWnd);
    END;
    RETURN NIL;
END TreeClientGetSelectedNode;

PROCEDURE TreeClientSetSelectedNode(w : Window; sel : TreeClientNode) : BOOLEAN;
BEGIN
    IF (w^.clientType = TreeClient) AND (sel <> NIL) THEN
        RETURN TreeView_SelectItem(w^.clientWnd, sel) <> FALSE;
    END;
    RETURN FALSE;
END TreeClientSetSelectedNode;

PROCEDURE TreeClientExpandNode(w : Window;
                               node : TreeClientNode;
                               yes : BOOLEAN) : BOOLEAN;
CONST
    flags       : ARRAY BOOLEAN OF CARDINAL = {TVE_COLLAPSE, TVE_EXPAND};
BEGIN
    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        RETURN TreeView_Expand(w^.clientWnd, node, flags[yes]) <> FALSE;
    END;
    RETURN FALSE;
END TreeClientExpandNode;

PROCEDURE TreeClientSortNode(w : Window;
                             node : TreeClientNode;
                             recurse : BOOLEAN);
BEGIN
    IF w^.clientType = TreeClient THEN
        IF node = NIL THEN
            node := TVI_ROOT;
        END;
        TreeView_SortChildren(w^.clientWnd, node, recurse);
    END;
END TreeClientSortNode;

PROCEDURE TreeClientRemoveNode(w : Window; node : TreeClientNode);
BEGIN
    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        TreeView_DeleteItem(w^.clientWnd, node);
    END;
END TreeClientRemoveNode;

PROCEDURE TreeClientRemoveAllNodes(w : Window);
BEGIN
    IF w^.clientType = TreeClient THEN
        TreeView_DeleteAllItems(w^.clientWnd);
    END;
END TreeClientRemoveAllNodes;

PROCEDURE TreeClientFreezeDisplay(w : Window; yes : BOOLEAN);
BEGIN
    IF w^.clientType = TreeClient THEN
        IF yes THEN
            WINUSER.SendMessage(w^.clientWnd, WM_SETREDRAW, ORD(FALSE), 0);
        ELSE
            WINUSER.SendMessage(w^.clientWnd, WM_SETREDRAW, ORD(TRUE), 0);
            InvalidateRect(w^.clientWnd, NIL_RECT, TRUE);
        END;
    END;
END TreeClientFreezeDisplay;

PROCEDURE TreeClientRootNode(w : Window) : TreeClientNode;
BEGIN
    IF w^.clientType = TreeClient THEN
        RETURN TreeView_GetRoot(w^.clientWnd);
    END;
    RETURN NIL;
END TreeClientRootNode;

PROCEDURE TreeClientGetChildNode(w : Window; node : TreeClientNode) : TreeClientNode;
BEGIN
    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        RETURN TreeView_GetChild(w^.clientWnd, node);
    END;
    RETURN NIL;
END TreeClientGetChildNode;

PROCEDURE TreeClientGetParentNode(w : Window; node : TreeClientNode) : TreeClientNode;
BEGIN
    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        RETURN TreeView_GetParent(w^.clientWnd, node);
    END;
    RETURN NIL;
END TreeClientGetParentNode;

PROCEDURE TreeClientNextSiblingNode(w : Window; node : TreeClientNode) : TreeClientNode;
BEGIN
    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        RETURN TreeView_GetNextSibling(w^.clientWnd, node);
    END;
    RETURN NIL;
END TreeClientNextSiblingNode;

PROCEDURE TreeClientPrevSiblingNode(w : Window; node : TreeClientNode) : TreeClientNode;
BEGIN
    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        RETURN TreeView_GetPrevSibling(w^.clientWnd, node);
    END;
    RETURN NIL;
END TreeClientPrevSiblingNode;

PROCEDURE TreeClientNodeIsVisible(w : Window; node : TreeClientNode) : BOOLEAN;
VAR
    rect        : RECT;
BEGIN
    IF (w^.clientType = TreeClient) AND (node <> NIL) THEN
        RETURN TreeView_GetItemRect(w^.clientWnd, node, rect, ORD(TRUE)) <> FALSE;
    END;
    RETURN FALSE;
END TreeClientNodeIsVisible;

PROCEDURE GetControlText(textPtr : StringData;
                         textId : INTEGER;
                         VAR INOUT ptr : LPTSTR;
                         VAR OUT text : ARRAY OF CHAR);
BEGIN
    IF textPtr <> NIL THEN
        ptr := CAST(LPTSTR, textPtr);
        AssignStringData(textPtr, text);
    ELSIF textId >= 0 THEN
        LoadString(textId, text);
        ptr := ADR(text);
    END;
END GetControlText;

PROCEDURE CreateControlDragBox(ctrl : ControlInfoPointer);
VAR
    rc          : RECT;
BEGIN
    GetControlRect(ctrl, FALSE, rc);

    ctrl^.dragWnd := CreateWindowEx(WS_EX_TRANSPARENT,
                                    DragBoxClassName,
                                    NIL_STR,
                                    WS_CHILD,
                                    rc.left, rc.top,
                                    rc.right - rc.left, rc.bottom - rc.top,
                                    ctrl^.w^.clientWnd,
                                    CAST(HMENU, DRAG_ID),
                                    Instance,
                                    ctrl);

    IF ctrl^.pageVisible THEN
        WINUSER.SetWindowPos(ctrl^.dragWnd,
                             HWND_TOP,
                             0, 0, 0, 0,
                             SWP_NOMOVE BOR SWP_NOSIZE BOR
                                 SWP_SHOWWINDOW BOR SWP_NOACTIVATE);
    END;
END CreateControlDragBox;

PROCEDURE SetRadioGrouping(w : Window);
(* radio button groups are in our list in order.
   the first radio button has the group flag, the rest do not.
   this procedure needs to sort the list by radio button groups.
   then it can clear the proper group flags.

   only the first radio button in a group has a tab stop flag.
*)

    PROCEDURE setGroup(ctrl : ControlInfoPointer);
    VAR
        style   : CARDINAL;
    BEGIN
        style := GetWindowLong(ctrl^.wnd, GWL_STYLE);
        style := style BOR WS_GROUP BOR WS_TABSTOP;
        SetWindowLong(ctrl^.wnd, GWL_STYLE, style);
    END setGroup;

    PROCEDURE clearGroup(ctrl : ControlInfoPointer);
    VAR
        style   : CARDINAL;
    BEGIN
        style := GetWindowLong(ctrl^.wnd, GWL_STYLE);
        style := style BAND (BNOT ORD(WS_GROUP BOR WS_TABSTOP));
        SetWindowLong(ctrl^.wnd, GWL_STYLE, style);
    END clearGroup;

    PROCEDURE putAfter(after, ctrl : ControlInfoPointer);
    BEGIN
        WINUSER.SetWindowPos(ctrl^.wnd,
                             after^.wnd,
                             0, 0, 0, 0,
                             SWP_NOMOVE BOR SWP_NOSIZE BOR SWP_NOACTIVATE);
    END putAfter;

    PROCEDURE doGroup(VAR INOUT firstControl, lastControl : ControlInfoPointer);
    VAR
        ctrl,
        first,
        last    : ControlInfoPointer;
        i       : ADRCARD;
    BEGIN
        first := firstControl;

        LOOP
            WHILE (first <> NIL) AND ((first^.type <> RadioButton) OR (first^.group = 0)) DO
                IF first^.type = RadioButton THEN
                    setGroup(first);
                END;
                first := first^.next;
            END;

            IF first <> NIL THEN
                setGroup(first);

                last := first;
                ctrl := last^.next;
                WHILE (ctrl <> NIL) AND (ctrl^.type = RadioButton) AND (ctrl^.group = first^.group) DO
                    clearGroup(ctrl);
                    last := ctrl;
                    ctrl := ctrl^.next;
                END;

                ctrl := last^.next;
                LOOP
                    WHILE (ctrl <> NIL) AND ((ctrl^.type <> RadioButton) OR (ctrl^.group <> first^.group)) DO
                        ctrl := ctrl^.next;
                    END;

                    IF ctrl <> NIL THEN
                        clearGroup(ctrl);
                        putAfter(last, ctrl);

                        ctrl^.prev^.next := ctrl^.next;
                        IF ctrl^.next <> NIL THEN
                            ctrl^.next^.prev := ctrl^.prev;
                        ELSE
                            lastControl := ctrl^.prev;
                            lastControl^.next := NIL;
                        END;

                        ctrl^.prev := last;
                        ctrl^.next := last^.next;
                        last^.next^.prev := ctrl;
                        last^.next := ctrl;

                        last := ctrl;

                        ctrl := last^.next;
                    ELSE
                        EXIT;
                    END;
                END;

                IF last = first THEN
                    first^.group := 0;
                END;

                first := last^.next;
            ELSE
                EXIT;
            END;
        END;

        ctrl := firstControl;
        WHILE ctrl <> NIL DO
            IF ctrl^.type = GroupBox THEN
                doGroup(ctrl^.firstChild, ctrl^.lastChild);
            ELSIF ctrl^.type = MultiPage THEN
                <*/PUSH/NOWARN:U*>
                FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
                <*/POP*>
                    doGroup(ctrl^.pageInfo^[i].firstChild, ctrl^.pageInfo^[i].lastChild);
                END;
            END;

            ctrl := ctrl^.next;
        END;
    END doGroup;

BEGIN
    doGroup(w^.firstControl, w^.lastControl);
END SetRadioGrouping;

PROCEDURE SetupControlTip(ctrl : ControlInfoPointer);
VAR
    ti          : TOOLINFO;
    top         : Window;
BEGIN
    IF ctrl^.tipText[0] <> '' THEN
        IF (NOT ctrl^.tipCreated) AND (ctrl^.w^.toolTipWnd <> NIL) THEN
            top := GetToplevel(ctrl^.w);

            ti.cbSize := SIZE(TOOLINFO);
            ti.uFlags := TTF_SUBCLASS BOR TTF_IDISHWND;
            ti.hwnd := top^.wnd;(*tooltip only sends to a toplevel window*)
            ti.uId := CAST(UINT_PTR, ctrl^.wnd);
            (*ti.hinst := Instance;*)
            ti.hinst := NIL;
            ti.lpszText := LPSTR_TEXTCALLBACK;
            ti.lParam := ctrl^.idNum;
            ctrl^.tipCreated := WINUSER.SendMessage(ctrl^.w^.toolTipWnd, TTM_ADDTOOL, 0, CAST(LPARAM, ADR(ti))) <> 0;
        END;
    ELSIF ctrl^.tipCreated THEN
        ti.cbSize := SIZE(TOOLINFO);
        ti.hwnd := ctrl^.w^.clientWnd;
        ti.uId := CAST(UINT_PTR, ctrl^.wnd);
        WINUSER.SendMessage(ctrl^.w^.toolTipWnd, TTM_DELTOOL, 0, CAST(LPARAM, ADR(ti)));
    END;
END SetupControlTip;

PROCEDURE DoControlSetDefaultButton(w : Window; ctrl : ControlInfoPointer);
CONST
    buttonSet   : ARRAY BOOLEAN OF CARDINAL = {BS_PUSHBUTTON, BS_DEFPUSHBUTTON};
VAR
    ptr         : ControlInfoPointer;
    default     : BOOLEAN;
BEGIN
    WINUSER.SendMessage(w^.clientWnd, DM_SETDEFID, ctrl^.idNum, 0);

    (* Windows had some wierdness with the default button drawing, so we
       explicitly set the button styles.
    *)
    ptr := w^.firstControl;
    WHILE ptr <> NIL DO
        IF ptr^.type = PushButton THEN
            default := ptr = ctrl;
            IF default <> ptr^.default THEN
                ptr^.default := default;
                Button_SetStyle(ptr^.wnd, buttonSet[default], TRUE);
            END;
        END;

        ptr := ptr^.next;
    END;
END DoControlSetDefaultButton;

PROCEDURE CreateMultiPagePages(ctrl : ControlInfoPointer);
VAR
    i           : ADRCARD;
    tab         : TCITEM;
BEGIN
    TabCtrl_DeleteAllItems(ctrl^.wnd);

    IF ctrl^.count <> 0 THEN
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
        <*/POP*>
            tab.mask := TCIF_TEXT;
            tab.pszText := CAST(LPTSTR, ctrl^.pageInfo^[i].label);
            TabCtrl_InsertItem(ctrl^.wnd, i, tab);
        END;
    END;

    GetChildFudge(ctrl);
END CreateMultiPagePages;

PROCEDURE SetFormEditMode(w : Window; mode : BOOLEAN);
BEGIN
    w^.formPreEditMode := mode;
END SetFormEditMode;

PROCEDURE DoAddControl(w : Window; control : ControlProperties; parent : ControlInfoPointer);
TYPE
    string      = ARRAY [0..15] OF CHAR;
CONST
    classNames  : ARRAY ControlTypes OF string =
        {
         (*CheckBox*)           "Button",
         (*RadioButton*)        "Button",
         (*CommandButton*)      "Button",
         (*ToggleButton*)       "Button",
         (*SpinButton*)         "Edit",(*edit buddy*)
         (*ListBox*)            WC_LISTVIEW,
         (*DropDownList*)       "ComboBox",
         (*ComboBox*)           "ComboBox",
         (*TextLabel*)          WC_STATIC (*"Static"*),
         (*TextEdit*)           "Edit",
         (*GroupBox*)           "Button",
         (*MultiPage*)          WC_TABCONTROL
        };

    styles      : ARRAY ControlTypes OF DWORD =
        {
         (*CheckBox*)           BS_AUTOCHECKBOX BOR BS_NOTIFY,
         (*RadioButton*)        BS_AUTORADIOBUTTON BOR BS_NOTIFY,
         (*PushButton*)         BS_NOTIFY,
         (*ToggleButton*)       BS_AUTOCHECKBOX BOR BS_PUSHLIKE BOR BS_NOTIFY,
         (*SpinButton*)         ES_NUMBER,(*edit buddy*)
         (*ListBox*)            LVS_REPORT BOR LVS_SHOWSELALWAYS BOR LVS_NOSORTHEADER,
         (*DropDownList*)       CBS_DROPDOWNLIST BOR
                                     CBS_HASSTRINGS BOR CBS_AUTOHSCROLL BOR WS_VSCROLL,
         (*ComboBox*)           CBS_DROPDOWN BOR
                                     CBS_HASSTRINGS BOR CBS_AUTOHSCROLL BOR WS_VSCROLL,
         (*TextLabel*)          0,
         (*TextEdit*)           0,
         (*GroupBox*)           BS_GROUPBOX,
         (*MultiPage*)          TCS_FOCUSNEVER
        };

    exStyles      : ARRAY ControlTypes OF DWORD =
        {
         (*CheckBox*)           0,
         (*RadioButton*)        0,
         (*CommandButton*)      0,
         (*ToggleButton*)       0,
         (*SpinButton*)         WS_EX_CLIENTEDGE,(*edit buddy*)
         (*ListBox*)            WS_EX_CLIENTEDGE,
         (*DropDownList*)       WS_EX_CLIENTEDGE,
         (*ComboBox*)           WS_EX_CLIENTEDGE,
         (*TextLabel*)          0,
         (*TextEdit*)           WS_EX_CLIENTEDGE,
         (*GroupBox*)           0,
         (*MultiPage*)          0 (*WS_EX_CONTROLPARENT (*MVN. Was = 0 *)*)
        };

    tabStop     : ARRAY ControlTypes OF BOOLEAN =
        {
         (*CheckBox*)           TRUE,
         (*RadioButton*)        FALSE,
         (*CommandButton*)      TRUE,
         (*ToggleButton*)       TRUE,
         (*SpinButton*)         TRUE,(*edit buddy*)
         (*ListBox*)            TRUE,
         (*DropDownList*)       TRUE,
         (*ComboBox*)           TRUE,
         (*TextLabel*)          FALSE,
         (*TextEdit*)           TRUE,
         (*GroupBox*)           FALSE,
         (*MultiPage*)          TRUE
        };

    textAlign   : ARRAY TextAlignment OF DWORD =
        {ES_LEFT, ES_CENTER, ES_RIGHT};
    staticAlign   : ARRAY TextAlignment OF DWORD =
        {SS_LEFT, SS_CENTER, SS_RIGHT};

VAR
    i, j        : ADRCARD;
    ctrl        : ControlInfoPointer;
    x, y,
    width,
    height      : INTEGER;
    style       : DWORD;
    title       : LPTSTR;
    class       : LPTSTR;
    pages       : ControlPageInfoArray;
    ctrls       : ControlPropertyArrayPtr;
    hres        : Uxtheme.THEMEAPI;
BEGIN
    NEW(ctrl);
    ctrl^.nextSel := NIL;
    ctrl^.next := NIL;
    ctrl^.parent := parent;
    ctrl^.firstChild := NIL;
    ctrl^.lastChild := NIL;

    INC(w^.numControlsAll);
    IF parent = NIL THEN
        INC(w^.numControlsMain);

        ctrl^.prev := w^.lastControl;
        IF w^.lastControl <> NIL THEN
            w^.lastControl^.next := ctrl;
        ELSE
            w^.firstControl := ctrl;
        END;
        w^.lastControl := ctrl;
    ELSIF parent^.type = GroupBox THEN
        ctrl^.prev := parent^.lastChild;
        IF parent^.lastChild <> NIL THEN
            parent^.lastChild^.next := ctrl;
        ELSE
            parent^.firstChild := ctrl;
        END;
        parent^.lastChild := ctrl;
    ELSIF parent^.type = MultiPage THEN
        i := parent^.currentPage;
        ctrl^.prev := parent^.pageInfo^[i].lastChild;
        IF parent^.pageInfo^[i].lastChild <> NIL THEN
            parent^.pageInfo^[i].lastChild^.next := ctrl;
        ELSE
            parent^.pageInfo^[i].firstChild := ctrl;
        END;
        parent^.pageInfo^[i].lastChild := ctrl;
    ELSE
        (*!!!*)
    END;

    IF control.idNum >= w^.nextCtrlId THEN
        INC(w^.nextCtrlId);
    END;

    ctrl^.type := control.controlType;
    ctrl^.spinWnd := NIL;
    ctrl^.dragWnd := NIL;
    ctrl^.w := w;
    ctrl^.idNum := control.idNum;
    ctrl^.selected := FALSE;
    ctrl^.hitTest := HtClient;
    ctrl^.count := 0;
    ctrl^.spinLow := 0;
    ctrl^.spinHigh := 0;
    ctrl^.editLimit := 0;
    ctrl^.columnInfo := NIL;
    ctrl^.pageInfo := NIL;
    ctrl^.visible := control.visible;
    ctrl^.enabled := control.enabled;
    ctrl^.captionId := -1;
    ctrl^.caption[0] := '';
    ctrl^.group := 0;
    ctrl^.default := FALSE;
    ctrl^.x := control.x;
    ctrl^.y := control.y;
    ctrl^.width := control.width;
    ctrl^.height := control.height;
    ctrl^.childFudgeX := 0;
    ctrl^.childFudgeY := 0;
    ctrl^.tipCreated := FALSE;
    ctrl^.editable := TRUE;
    ctrl^.currentPage := 0;
    ctrl^.pageVisible := TRUE;(*multi page*)
    ctrl^.autoSizePosted := FALSE;
    ctrl^.password := FALSE;
    ctrl^.multiSelect := FALSE;
    ctrl^.prevProc := NILPROC;

    ctrl^.tipTextId := control.tipTextId;
    IF ctrl^.tipTextId < 0 THEN
        AssignStringData(control.tipTextPtr, ctrl^.tipText);
    ELSE
        LoadString(ctrl^.tipTextId, ctrl^.tipText);
    END;

    style := WS_CHILD BOR WS_GROUP BOR styles[ctrl^.type];
    class := ADR(classNames[ctrl^.type]);
    title := NIL;

    CASE ctrl^.type OF
    TextEdit:
        ctrl^.multiLine := control.te_multiLine;
        ctrl^.textAlign := control.te_align;
        ctrl^.horizScroll := control.te_horizScroll;
        ctrl^.vertScroll := control.te_vertScroll;
        ctrl^.editLimit := control.te_limit;
        ctrl^.password := control.te_password;

        style := style BOR textAlign[control.te_align];
        IF ctrl^.multiLine THEN
            style := style BOR ES_MULTILINE BOR ES_WANTRETURN;
        END;
        IF ctrl^.horizScroll THEN
            IF ctrl^.multiLine THEN
                style := style BOR WS_HSCROLL BOR ES_AUTOHSCROLL;
            ELSE
                style := style BOR ES_AUTOHSCROLL;
            END;
        END;
        IF ctrl^.vertScroll AND ctrl^.multiLine THEN
            style := style BOR WS_VSCROLL BOR ES_AUTOVSCROLL;
        END;
        IF ctrl^.password AND (NOT ctrl^.multiLine) THEN
            style := style BOR ES_PASSWORD;
        END;
    |
    TextLabel:
        style := style BOR staticAlign[control.tl_align];
        ctrl^.textAlign := control.tl_align;
        ctrl^.captionId := control.tl_textId;
        GetControlText(control.tl_textPtr, control.tl_textId, title, ctrl^.caption);
    |
    DropDownList:
    |
    ComboBox:
        ctrl^.editLimit := control.cb_editLimit;
    |
    PushButton:
        ctrl^.captionId := control.b_textId;
        GetControlText(control.b_textPtr, control.b_textId, title, ctrl^.caption);

        ctrl^.default := control.b_default;
        style := style BOR BS_PUSHBUTTON;(*we handle default later*)

        (*IF control.b_bitmap <> NIL THEN
            style := style BOR BS_BITMAP;
        END;*)
    |
    ToggleButton:
        ctrl^.captionId := control.b_textId;
        GetControlText(control.b_textPtr, control.b_textId, title, ctrl^.caption);

        (*IF control.b_bitmap <> NIL THEN
            style := style BOR BS_BITMAP;
        END;*)
    |
    RadioButton:
        ctrl^.captionId := control.rb_textId;
        GetControlText(control.rb_textPtr, control.rb_textId, title, ctrl^.caption);
        ctrl^.group := control.rb_group;
    |
    CheckBox:
        ctrl^.captionId := control.chk_textId;
        GetControlText(control.chk_textPtr, control.chk_textId, title, ctrl^.caption);
    |
    ListBox:
        ctrl^.multiSelect := control.lb_multiSelect;
        IF NOT ctrl^.multiSelect THEN
            style := style BOR LVS_SINGLESEL;
        END;

        ctrl^.count := control.lb_numColumns;
        IF control.lb_columnInfo <> NIL THEN
            NEW(ctrl^.columnInfo, ctrl^.count-1);
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
            <*/POP*>
                ctrl^.columnInfo^[i].header := DuplicateStringData(control.lb_columnInfo^[i].header);
                ctrl^.columnInfo^[i].align := control.lb_columnInfo^[i].align;
                ctrl^.columnInfo^[i].width := control.lb_columnInfo^[i].width;
                ctrl^.columnInfo^[i].content := ColumnText;
                ctrl^.columnInfo^[i].sortable := FALSE;
            END;
        END;
    |
    SpinButton:
        ctrl^.spinLow := control.sb_low;
        ctrl^.spinHigh := control.sb_high;
        ctrl^.editable := control.sb_editable;
    |
    GroupBox:
        ctrl^.captionId := control.grp_textId;
        GetControlText(control.grp_textPtr, control.grp_textId, title, ctrl^.caption);
    |
    MultiPage:
        ctrl^.count := control.mp_numPages;
        IF ctrl^.count > 0 THEN
            pages := control.mp_pageInfo;

            NEW(ctrl^.pageInfo, ctrl^.count-1);
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
            <*/POP*>
                ctrl^.pageInfo^[i].label := DuplicateStringData(pages^[i].label);
                ctrl^.pageInfo^[i].numControls := pages^[i].numControls;
                ctrl^.pageInfo^[i].firstChild := NIL;
                ctrl^.pageInfo^[i].lastChild := NIL;
            END;
        END;
    END;

    IF control.visible OR w^.formEditMode  OR w^.formPreEditMode THEN
        style := style BOR WS_VISIBLE;
    END;
    IF tabStop[ctrl^.type] THEN
        style := style BOR WS_TABSTOP;
    END;

    x := ctrl^.x;
    y := ctrl^.y;
    width := ctrl^.width;
    height := ctrl^.height;
    FormToPixel(w, x, y);
    FormToPixel(w, width, height);

    IF parent <> NIL THEN
        x := x + parent^.childFudgeX;
        ctrl^.x := x;
        y := y + parent^.childFudgeY;
        ctrl^.y := y;
        PixelToForm(w, ctrl^.x, ctrl^.y);
    END;

    ctrl^.wnd := CreateWindowEx(exStyles[ctrl^.type],
                                class^,
                                title^,
                                style,
                                x, y,
                                width, height,
                                w^.clientWnd,
                                CAST(HMENU, MAKEADR(ctrl^.idNum)),
                                Instance,
                                NIL);

    FUNC SetWindowLongPtr (ctrl^.wnd, GWL_USERDATA, CAST (ADRCARD, ctrl));
    SetWindowFont(ctrl^.wnd, w^.formFont, FALSE);
    IF (NOT w^.formPreEditMode) AND (NOT w^.formEditMode) AND (NOT control.enabled) THEN
        EnableWindow(ctrl^.wnd, FALSE);
    END;

    CASE ctrl^.type OF
    TextEdit:
        IF ctrl^.editLimit <> 0 THEN
            Edit_LimitText(ctrl^.wnd, ctrl^.editLimit);
        END;
    |
    ListBox:
        ctrl^.prevProc := CAST (WNDPROC, SetWindowLongPtr (ctrl^.wnd, GWL_WNDPROC, CAST (ADRCARD, ListViewWndProc1)));
        ListView_SetExtendedListViewStyle(ctrl^.wnd, LVS_EX_FULLROWSELECT BOR LVS_EX_GRIDLINES);
        CreateListViewColumns(w, ctrl^.wnd, ctrl^.count, ctrl^.columnInfo, TRUE);
    |
    ComboBox:
        ComboBox_SetExtendedUI(ctrl^.wnd, 1);
        IF ctrl^.editLimit <> 0 THEN
            ComboBox_LimitText(ctrl^.wnd, ctrl^.editLimit);
        END;
    |
    DropDownList:
        ComboBox_SetExtendedUI(ctrl^.wnd, 1);
    |
    SpinButton:
        Edit_SetReadOnly(ctrl^.wnd, NOT ctrl^.editable);

        style := WS_CHILD BOR
                     WS_BORDER BOR
                     UDS_ALIGNRIGHT BOR
                     UDS_SETBUDDYINT BOR
                     UDS_NOTHOUSANDS BOR
                     UDS_ARROWKEYS;
        IF control.visible OR w^.formPreEditMode OR w^.formEditMode THEN
            style := style BOR WS_VISIBLE;
        END;
        ctrl^.spinWnd := CreateWindowEx(0,
                                        UPDOWN_CLASS,
                                        NIL_STR,
                                        style,
                                        0, 0,
                                        0, height,
                                        w^.clientWnd,
                                        CAST(HMENU, MAKEADR(ctrl^.idNum)),
                                        Instance,
                                        NIL);
        (* auto buddy set the x, y position and the width.
           it also reduces the size of the edit control by the width of
           the spin control.
        *)
        WINUSER.SendMessage(ctrl^.spinWnd, UDM_SETBUDDY, CAST(WPARAM, ctrl^.wnd), 0);
        UpDown_SetRange32(ctrl^.spinWnd, control.sb_low, control.sb_high);
        IF (NOT w^.formPreEditMode) AND (NOT w^.formEditMode) AND (NOT control.enabled) THEN
            EnableWindow(ctrl^.spinWnd, FALSE);
        END;
    |
    PushButton:
        (*IF control.b_bitmap <> NIL THEN
            Button_SetImage_Bitmap(ctrl^.wnd, control.b_bitmap);
        END;*)
        IF ctrl^.default THEN
            DoControlSetDefaultButton(w, ctrl);
        END;
    |
    ToggleButton:
        (*IF control.b_bitmap <> NIL THEN
            Button_SetImage_Bitmap(ctrl^.wnd, control.b_bitmap);
        END;*)
    |
    GroupBox:
        GetChildFudge(ctrl);

        ctrl^.count := control.grp_numControls;
        ctrls := control.grp_controls;

        IF (ctrl^.count <> 0) AND (ctrls <> NIL) THEN
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
            <*/POP*>
                DoAddControl(w, ctrls^[i], ctrl);
            END;
        END;
    |
    MultiPage:
        (* the tab control apparently implicitly sets the clip siblings style.
           if a tab control is placed inside a groupbox then the tabs will not be visible.
        *)
        hres := Uxtheme.SetWindowTheme(ctrl^.wnd, "", "");
        (*hres := Uxtheme.SetWindowTheme(ctrl^.wnd, "Explorer", "");*)
        style := GetWindowLong(ctrl^.wnd, GWL_STYLE);
        style := style BAND (BNOT ORD(WS_CLIPSIBLINGS));
        SetWindowLong(ctrl^.wnd, GWL_STYLE, style);

        IF ctrl^.count <> 0 THEN
            CreateMultiPagePages(ctrl);

            pages := control.mp_pageInfo;
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
            <*/POP*>
                IF pages^[i].numControls <> 0 THEN
                    ctrl^.currentPage := i;
                    <*/PUSH/NOWARN:U*>
                    FOR j := 0 TO VAL(ADRCARD, pages^[i].numControls)-1 DO
                    <*/POP*>
                        DoAddControl(w, pages^[i].controls^[j], ctrl);
                    END;
                END;
            END;

            ctrl^.currentPage := MAX(CARDINAL);
            MultiPageSetActivePage(ctrl, 0);
        END;
    ELSE
    END;

    IF w^.formEditMode THEN
        CreateControlDragBox(ctrl);
    END;

    SetupControlTip(ctrl);
END DoAddControl;

PROCEDURE AddFormControls(w : Window; controls : ARRAY OF ControlProperties);
VAR
    i                   : ADRCARD;
    highControls        : ADRCARD;
BEGIN
    IF IsWindow(w) AND (w^.clientType = FormClient) THEN
        highControls := HIGH(controls);
        FOR i := 0 TO highControls DO
            DoAddControl(w, controls[i], NIL);
        END;

        SetRadioGrouping(w);
    END;
END AddFormControls;

PROCEDURE DestroyControl(ctrl : ControlInfoPointer);
VAR
    ti          : TOOLINFO;
    i           : ADRCARD;
    p           : ADRCARD;

    PROCEDURE findPage(multi, ctrl : ControlInfoPointer) : CARDINAL;
    VAR
        i       : ADRCARD;
        ptr     : ControlInfoPointer;
    BEGIN
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, multi^.count)-1 DO
        <*/POP*>
            ptr := multi^.pageInfo^[i].firstChild;
            WHILE ptr <> NIL DO
                IF ptr = ctrl THEN
                    RETURN i;
                END;
                ptr := ptr^.next;
            END;
        END;
        (*!!!*)
        RETURN 0;
    END findPage;

    PROCEDURE unlink(ctrl : ControlInfoPointer; VAR INOUT first, last : ControlInfoPointer);
    BEGIN
        IF ctrl^.prev <> NIL THEN
            ctrl^.prev^.next := ctrl^.next;
        ELSE
            first := ctrl^.next;
            IF first <> NIL THEN
                first^.prev := NIL;
            END;
        END;
        IF ctrl^.next <> NIL THEN
            ctrl^.next^.prev := ctrl^.prev;
        ELSE
            last := ctrl^.prev;
            IF last <> NIL THEN
                last^.next := NIL;
            END;
        END;
    END unlink;

BEGIN
    IF ctrl^.type = MultiPage THEN
        i := 0;
        <*/PUSH/NOWARN:U*>
        WHILE i < VAL(ADRCARD, ctrl^.count) DO
        <*/POP*>
            WHILE ctrl^.pageInfo^[i].firstChild <> NIL DO
                DestroyControl(ctrl^.pageInfo^[i].firstChild);
            END;

            INC(i);
        END;
    ELSIF ctrl^.type = GroupBox THEN
        WHILE ctrl^.firstChild <> NIL DO
            DestroyControl(ctrl^.firstChild);
        END;
    END;

    DEC(ctrl^.w^.numControlsAll);

    IF ctrl^.parent = NIL THEN
        unlink(ctrl, ctrl^.w^.firstControl, ctrl^.w^.lastControl);
        DEC(ctrl^.w^.numControlsMain);
    ELSIF ctrl^.parent^.type = MultiPage THEN
        p := findPage(ctrl^.parent, ctrl);
        DEC(ctrl^.parent^.pageInfo^[p].numControls);
        unlink(ctrl, ctrl^.parent^.pageInfo^[p].firstChild, ctrl^.parent^.pageInfo^[p].lastChild);
    ELSIF ctrl^.parent^.type = GroupBox THEN
        DEC(ctrl^.parent^.count);
        unlink(ctrl, ctrl^.parent^.firstChild, ctrl^.parent^.lastChild);
    END;

    IF ctrl^.selected THEN
        UnselectControl(ctrl^.w, ctrl);
    END;

    IF ctrl^.tipCreated THEN
        ti.cbSize := SIZE(TOOLINFO);
        ti.hwnd := ctrl^.w^.clientWnd;
        ti.uId := CAST(UINT_PTR, ctrl^.wnd);
        WINUSER.SendMessage(ctrl^.w^.toolTipWnd, TTM_DELTOOL, 0, CAST(LPARAM, ADR(ti)));
    END;

    ctrl^.w^.formChanged := TRUE;
    DestroyWindow(ctrl^.wnd);
    IF ctrl^.spinWnd <> NIL THEN
        DestroyWindow(ctrl^.spinWnd);
    END;
    IF ctrl^.dragWnd <> NIL THEN
        DestroyWindow(ctrl^.dragWnd);
    END;

    IF ctrl^.count > 0 THEN
        IF ctrl^.type = ListBox THEN
            IF ctrl^.columnInfo <> NIL THEN
                <*/PUSH/NOWARN:U*>
                FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
                <*/POP*>
                    DisposeStringData(ctrl^.columnInfo^[i].header);
                END;
                DISPOSE(ctrl^.columnInfo);
            END;
        ELSIF ctrl^.type = MultiPage THEN
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
            <*/POP*>
                DisposeStringData(ctrl^.pageInfo^[i].label);
            END;
            DISPOSE(ctrl^.pageInfo);
        END;
    END;

    DISPOSE(ctrl);
END DestroyControl;

PROCEDURE RemoveControl(w : Window; ctrlId : CARDINAL) : BOOLEAN;
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        DestroyControl(ctrl);
        RETURN TRUE;
    END;
    RETURN FALSE;
END RemoveControl;

PROCEDURE RemoveAllControls(w : Window);
BEGIN
    IF IsWindow(w) AND (w^.clientType = FormClient) THEN
        WHILE w^.firstControl <> NIL DO
            DestroyControl(w^.firstControl);
        END;
    END;
END RemoveAllControls;

PROCEDURE CreateDragObjects;
VAR
    oldBitmap           : HBITMAP;
    oldBrush            : HBRUSH;
    oldPen              : HPEN;
    dc                  : HDC;
BEGIN
    IF DragDC = NIL THEN
        DragBoxPatternBitmap := WINUSER.LoadBitmap(Instance, "DRAG-PATTERN");
        DragBoxPatternBrush := WINGDI.CreatePatternBrush(DragBoxPatternBitmap);
        dc := GetDC(NIL);
        DragDC := CreateCompatibleDC(dc);
        DragHandlePrimary := CreateCompatibleBitmap(dc, DragHandleSize, DragHandleSize);
        DragHandleSecondary := CreateCompatibleBitmap(dc, DragHandleSize, DragHandleSize);
        ReleaseDC(NIL, dc);

        oldBitmap := SelectBitmap(DragDC, DragHandlePrimary);
        oldBrush := SelectBrush(DragDC, GetStockBrush(WHITE_BRUSH));
        oldPen := SelectPen(DragDC, GetStockPen(BLACK_PEN));
        Rectangle(DragDC, 0, 0, DragHandleSize, DragHandleSize);

        SelectBitmap(DragDC, DragHandleSecondary);
        SelectBrush(DragDC, GetStockBrush(BLACK_BRUSH));
        SelectPen(DragDC, GetStockPen(WHITE_PEN));
        Rectangle(DragDC, 0, 0, DragHandleSize, DragHandleSize);

        SelectPen(DragDC, oldPen);
        SelectBrush(DragDC, oldBrush);
        SelectBitmap(DragDC, oldBitmap);
    END;
END CreateDragObjects;

PROCEDURE DestroyDragObjects;
BEGIN
    IF DragDC <> NIL THEN
        DeleteDC(DragDC);
        WINX.DeleteBitmap(DragHandlePrimary);
        WINX.DeleteBitmap(DragHandleSecondary);
        WINX.DeleteBrush(DragBoxPatternBrush);
        WINX.DeleteBitmap(DragBoxPatternBitmap);
    END;
END DestroyDragObjects;

PROCEDURE StartFormEditMode(w : Window;
                            usePopupMenu : BOOLEAN;
                            popupMenu : ARRAY OF CHAR;
                            newControlProc : NewControlProc;
                            propProc : ControlPropertiesProc;
                            exitProc : ControlEndEditProc);

    PROCEDURE createDragBox(ctrl : ControlInfoPointer);
    VAR
        i       : ADRCARD;
    BEGIN
        WHILE ctrl <> NIL DO
            ctrl^.selected := FALSE;
            ctrl^.nextSel := NIL;

            (* always visble and enabled while editing *)

            IF ctrl^.pageVisible THEN
                ShowWindow(ctrl^.wnd, SW_SHOWNA);
            END;
            EnableWindow(ctrl^.wnd, TRUE);
            IF ctrl^.spinWnd <> NIL THEN
                IF ctrl^.pageVisible THEN
                    ShowWindow(ctrl^.spinWnd, SW_SHOWNA);
                END;
                EnableWindow(ctrl^.spinWnd, TRUE);
            END;

            CreateControlDragBox(ctrl);

            IF ctrl^.type = MultiPage THEN
                IF ctrl^.count > 0 THEN
                    <*/PUSH/NOWARN:U*>
                    FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
                    <*/POP*>
                        createDragBox(ctrl^.pageInfo^[i].firstChild);
                    END;
                END;
            ELSIF ctrl^.type = GroupBox THEN
                IF ctrl^.count > 0 THEN
                    createDragBox(ctrl^.firstChild);
                END;
            END;

            ctrl := ctrl^.next;
        END;
    END createDragBox;

BEGIN
    IF (w^.clientType = FormClient) AND (NOT w^.formEditMode) THEN
        w^.formEditMode := TRUE;
        w^.formPreEditMode := TRUE;
        w^.formChanged := FALSE;
        w^.propProc := propProc;
        w^.newControlProc := newControlProc;
        w^.exitProc := exitProc;
        w^.selectionCount := 0;
        w^.selectedControl := NIL;
        w^.formUsePopupMenu := usePopupMenu;
        IF usePopupMenu THEN
            IF popupMenu[0] = '' THEN
                w^.formMenu := WINUSER.LoadMenu(ResourceInst, "FORM-EDIT-MENU");
            ELSE
                w^.formMenu := MyLoadMenu(ResourceInst, popupMenu);
            END;
        END;

        CreateDragObjects;

        (* we do not want the system to handle the standard dialog
           keystrokes for dialogs.
        *)
        SetModelessDialog(w^.clientWnd, FALSE);

        createDragBox(w^.firstControl);

        IF w^.firstControl <> NIL THEN
            SetSelectedControl(w, w^.firstControl);
        END;
    END;
END StartFormEditMode;

PROCEDURE ExecuteFormEditorMenuCommand(w : Window; menuId : CARDINAL);
BEGIN
    IF (w^.clientType = FormClient) AND w^.formEditMode THEN
        DoFormMenu(w, menuId);
    END;
END ExecuteFormEditorMenuCommand;

PROCEDURE EnableFormEditorMenuItems(w : Window);
VAR
    top         : Window;
BEGIN
    IF (w^.clientType = FormClient) AND w^.formEditMode THEN
        top := GetToplevel(w);
        IF top^.menu <> NIL THEN
            EnableFormMenuItems(w, top^.menu);
        END;
    END;
END EnableFormEditorMenuItems;

PROCEDURE EndFormEditMode(w : Window) : BOOLEAN;
VAR
    changed     : BOOLEAN;

    PROCEDURE disposeDragBox(ctrl : ControlInfoPointer);
    VAR
        i       : ADRCARD;
    BEGIN
        WHILE ctrl <> NIL DO
            IF ctrl^.dragWnd <> NIL THEN
                DestroyWindow(ctrl^.dragWnd);
                ctrl^.dragWnd := NIL;
            END;

            IF NOT ctrl^.enabled THEN
                EnableWindow(ctrl^.wnd, FALSE);
                IF ctrl^.spinWnd <> NIL THEN
                    EnableWindow(ctrl^.spinWnd, FALSE);
                END;
            END;
            IF ctrl^.pageVisible AND NOT ctrl^.visible THEN
                ShowWindow(ctrl^.wnd, SW_HIDE);
                IF ctrl^.spinWnd <> NIL THEN
                    ShowWindow(ctrl^.spinWnd, SW_HIDE);
                END;
            END;

            ctrl^.selected := FALSE;
            ctrl^.nextSel := NIL;

            IF ctrl^.type = MultiPage THEN
                IF ctrl^.count > 0 THEN
                    <*/PUSH/NOWARN:U*>
                    FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
                    <*/POP*>
                        disposeDragBox(ctrl^.pageInfo^[i].firstChild);
                    END;
                END;
            ELSIF ctrl^.type = GroupBox THEN
                disposeDragBox(ctrl^.firstChild);
            END;

            ctrl := ctrl^.next;
        END;
    END disposeDragBox;

BEGIN
    changed := FALSE;
    IF (w^.clientType = FormClient) AND w^.formEditMode THEN
        changed := w^.formChanged;
        w^.formPreEditMode := FALSE;
        w^.formEditMode := FALSE;
        w^.selectionCount := 0;
        w^.selectedControl := NIL;
        IF w^.formMenu <> NIL THEN
            DestroyMenu(w^.formMenu);
            w^.formMenu := NIL;
        END;

        SetModelessDialog(w^.clientWnd, TRUE);

        disposeDragBox(w^.firstControl);
    END;
    RETURN changed;
END EndFormEditMode;

PROCEDURE GetControlCount(w : Window; includeContainerChildren : BOOLEAN) : CARDINAL;
BEGIN
    IF w^.clientType = FormClient THEN
        IF includeContainerChildren THEN
            RETURN w^.numControlsAll;
        ELSE
            RETURN w^.numControlsMain;
        END;
    END;
    RETURN 0;
END GetControlCount;

PROCEDURE GetControlProps(ctrl : ControlInfoPointer;  VAR OUT info : ControlProperties);
VAR
    i, j        : ADRCARD;
    pages       : ControlPageInfoArray;
    ctrls       : ControlPropertyArrayPtr;
    ptr         : ControlInfoPointer;
    fx, fy      : COORDINATE;
BEGIN
    info.controlType := ctrl^.type;
    info.idNum := ctrl^.idNum;
    info.visible := ctrl^.visible;
    info.enabled := ctrl^.enabled;
    info.x := ctrl^.x;
    info.y := ctrl^.y;
    info.width := ctrl^.width;
    info.height := ctrl^.height;

    info.tipTextId := ctrl^.tipTextId;
    info.tipTextPtr := NIL;
    IF info.tipTextId < 0 THEN
        info.tipTextPtr := CreateStringData(ctrl^.tipText);
    END;

    CASE ctrl^.type OF
    CheckBox:
        info.chk_textId := ctrl^.captionId;
        info.chk_textPtr := NIL;
        IF info.chk_textId < 0 THEN
            info.chk_textPtr := CreateStringData(ctrl^.caption);
        END;
    |
    RadioButton:
        info.rb_textId := ctrl^.captionId;
        info.rb_textPtr := NIL;
        IF info.rb_textId < 0 THEN
            info.rb_textPtr := CreateStringData(ctrl^.caption);
        END;
        info.rb_group := ctrl^.group;
    |
    PushButton, ToggleButton:
        info.b_textId := ctrl^.captionId;
        info.b_textPtr := NIL;
        IF info.b_textId < 0 THEN
            info.b_textPtr := CreateStringData(ctrl^.caption);
        END;
        info.b_default := ctrl^.default;
        (*info.b_bitmap := NIL;*)
    |
    SpinButton:
        info.sb_low := ctrl^.spinLow;
        info.sb_high := ctrl^.spinHigh;
    |
    ListBox:
        info.lb_multiSelect := ctrl^.multiSelect;
        info.lb_numColumns := ctrl^.count;
        info.lb_columnInfo := NIL;
        IF ctrl^.columnInfo <> NIL THEN
            ALLOCATE(info.lb_columnInfo, info.lb_numColumns * SIZE(ControlColumnInfo));
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, info.lb_numColumns)-1 DO
            <*/POP*>
                info.lb_columnInfo^[i].header := ctrl^.columnInfo^[i].header;
                info.lb_columnInfo^[i].width := ctrl^.columnInfo^[i].width;
                info.lb_columnInfo^[i].align := ctrl^.columnInfo^[i].align;
                IF info.lb_columnInfo^[i].header <> NIL THEN
                    info.lb_columnInfo^[i].header := DuplicateStringData(info.lb_columnInfo^[i].header);
                END;
            END;
        END;
    |
    DropDownList:
    |
    ComboBox:
        info.cb_editLimit := ctrl^.editLimit;
    |
    TextLabel:
        info.tl_align := ctrl^.textAlign;
        info.tl_textId := ctrl^.captionId;
        info.tl_textPtr := NIL;
        IF info.tl_textId < 0 THEN
            info.tl_textPtr := CreateStringData(ctrl^.caption);
        END;
    |
    TextEdit:
        info.te_align := ctrl^.textAlign;
        info.te_multiLine := ctrl^.multiLine;
        info.te_horizScroll := ctrl^.horizScroll;
        info.te_vertScroll := ctrl^.vertScroll;
        info.te_limit := ctrl^.editLimit;
        info.te_password := ctrl^.password;
    |
    GroupBox:
        info.grp_textId := ctrl^.captionId;
        info.grp_textPtr := NIL;
        IF info.grp_textId < 0 THEN
            info.grp_textPtr := CreateStringData(ctrl^.caption);
        END;

        info.grp_numControls := ctrl^.count;
        info.grp_controls := NIL;
        IF ctrl^.count > 0 THEN
            fx := ctrl^.childFudgeX;
            fy := ctrl^.childFudgeY;
            PixelToForm(ctrl^.w, fx, fy);

            ALLOCATE(info.grp_controls, ctrl^.count * SIZE(ControlProperties));
            ctrls := info.grp_controls;

            i := 0;
            ptr := ctrl^.firstChild;
            WHILE ptr <> NIL DO
                GetControlProps(ptr, ctrls^[i]);

                ctrls^[i].x := ctrls^[i].x - fx;
                ctrls^[i].y := ctrls^[i].y - fy;

                INC(i);
                ptr := ptr^.next;
            END;
        END;
    |
    MultiPage:
        info.mp_numPages := ctrl^.count;
        info.mp_pageInfo := NIL;
        IF ctrl^.count > 0 THEN
            fx := ctrl^.childFudgeX;
            fy := ctrl^.childFudgeY;
            PixelToForm(ctrl^.w, fx, fy);

            ALLOCATE(info.mp_pageInfo, info.mp_numPages * SIZE(ControlPageInfo));

            pages := info.mp_pageInfo;

            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, info.mp_numPages)-1 DO
            <*/POP*>
                pages^[i].label := DuplicateStringData(ctrl^.pageInfo^[i].label);
                pages^[i].numControls := ctrl^.pageInfo^[i].numControls;
                pages^[i].controls := NIL;

                IF pages^[i].numControls > 0 THEN
                    ALLOCATE(pages^[i].controls, pages^[i].numControls * SIZE(ControlProperties));

                    j := 0;
                    ptr := ctrl^.pageInfo^[i].firstChild;
                    WHILE ptr <> NIL DO
                        GetControlProps(ptr, pages^[i].controls^[j]);

                        pages^[i].controls^[j].x := pages^[i].controls^[j].x - fx;
                        pages^[i].controls^[j].y := pages^[i].controls^[j].y - fy;

                        INC(j);
                        ptr := ptr^.next;
                    END;
                END;
            END;
        END;
    END;
END GetControlProps;

PROCEDURE GetControlInfo(w : Window; VAR OUT controls : ARRAY OF ControlProperties);
VAR
    ctrl                : ControlInfoPointer;
    i                   : ADRCARD;
    highControls        : ADRCARD;
BEGIN
    IF IsWindow(w) AND (w^.clientType = FormClient) THEN
        i := 0;
        highControls := HIGH(controls);
        ctrl := w^.firstControl;
        WHILE (ctrl <> NIL) AND (i <= highControls) DO
            IF ctrl^.parent = NIL THEN
                GetControlProps(ctrl, controls[i]);
                INC(i);
            END;

            ctrl := ctrl^.next;
        END;
    END;
END GetControlInfo;

PROCEDURE DisposeControlProps(VAR INOUT info : ControlProperties);
VAR
    i, j        : ADRCARD;
    pages       : ControlPageInfoArray;
    ctrls       : ControlPropertyArrayPtr;
BEGIN
    DisposeStringData(info.tipTextPtr);

    CASE info.controlType OF
    CheckBox:
        DisposeStringData(info.chk_textPtr);
    |
    RadioButton:
        DisposeStringData(info.rb_textPtr);
    |
    PushButton, ToggleButton:
        DisposeStringData(info.b_textPtr);
    |
    SpinButton:
    |
    ListBox:
        IF info.lb_columnInfo <> NIL THEN
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, info.lb_numColumns)-1 DO
            <*/POP*>
                DisposeStringData(info.lb_columnInfo^[i].header);
            END;
            DEALLOCATE(info.lb_columnInfo, info.lb_numColumns * SIZE(ControlColumnInfo));
        END;
    |
    DropDownList:
    |
    ComboBox:
    |
    TextLabel:
        DisposeStringData(info.tl_textPtr);
    |
    TextEdit:
    |
    GroupBox:
        DisposeStringData(info.grp_textPtr);

        IF info.grp_numControls > 0 THEN
            ctrls := info.grp_controls;
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, info.grp_numControls)-1 DO
            <*/POP*>
                DisposeControlProps(ctrls^[i]);
            END;

            DEALLOCATE(info.grp_controls, info.grp_numControls * SIZE(ControlProperties));
        END;
    |
    MultiPage:
        IF info.mp_numPages > 0 THEN
            pages := info.mp_pageInfo;

            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, info.mp_numPages)-1 DO
            <*/POP*>
                DisposeStringData(pages^[i].label);

                IF pages^[i].numControls > 0 THEN
                    <*/PUSH/NOWARN:U*>
                    FOR j := 0 TO VAL(ADRCARD, pages^[i].numControls)-1 DO
                    <*/POP*>
                        DisposeControlProps(pages^[i].controls^[j]);
                    END;

                    DEALLOCATE(pages^[i].controls, pages^[i].numControls * SIZE(ControlProperties));
                END;
            END;

            DEALLOCATE(info.mp_pageInfo, info.mp_numPages * SIZE(ControlPageInfo));
        END;
    END;
END DisposeControlProps;

PROCEDURE DisposeControlInfo(VAR INOUT controls : ARRAY OF ControlProperties);
VAR
    i                   : ADRCARD;
    highControls        : ADRCARD;
BEGIN
    highControls := HIGH(controls);
    FOR i := 0 TO highControls DO
        DisposeControlProps(controls[i]);
    END;
END DisposeControlInfo;

PROCEDURE ControlSetEditLimit(w : Window; ctrlId : CARDINAL; limit : CARDINAL);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        TextEdit:
            Edit_LimitText(ctrl^.wnd, limit);
        |
        ComboBox:
            ComboBox_LimitText(ctrl^.wnd, limit);
        ELSE
        END;
    END;
END ControlSetEditLimit;

PROCEDURE ControlSetEditable(w : Window; ctrlId : CARDINAL; yes : BOOLEAN);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        TextEdit:
            Edit_SetReadOnly(ctrl^.wnd, yes);
        |
        SpinButton:
            Edit_SetReadOnly(ctrl^.wnd, yes);
        ELSE
        END;
    END;
END ControlSetEditable;

PROCEDURE ControlSetTipText(w : Window; ctrlId : CARDINAL; text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        ctrl^.tipText := text;

        SetupControlTip(ctrl);
    END;
END ControlSetTipText;

PROCEDURE ControlGetTipText(w : Window; ctrlId : CARDINAL; VAR OUT text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    text := "";

    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        text := ctrl^.tipText;
    END;
END ControlGetTipText;

PROCEDURE ControlSetText(w : Window; ctrlId : CARDINAL; text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
    sel         : INTEGER;
    idx         : CARDINAL;
    lvFind      : LVFINDINFO;
    item        : TCITEM;
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        PushButton, ToggleButton, CheckBox, RadioButton, GroupBox:
            Button_SetText(ctrl^.wnd, text);
        |
        TextEdit:
            Edit_SetText(ctrl^.wnd, text);
        |
        TextLabel:
            Static_SetText(ctrl^.wnd, text);
        |
        ComboBox:
            IF ComboBox_FindString(ctrl^.wnd, -1, text, idx) THEN
                ComboBox_SetCurSel(ctrl^.wnd, idx);
            END;
            ComboBox_SetText(ctrl^.wnd, text);
        |
        DropDownList:
            IF ComboBox_FindString(ctrl^.wnd, -1, text, idx) THEN
                ComboBox_SetCurSel(ctrl^.wnd, idx);
            END;
        |
        ListBox:
            lvFind.flags := LVFI_STRING BOR LVFI_PARTIAL;
            lvFind.psz := ADR(text);
            sel := ListView_FindItem(ctrl^.wnd, -1, lvFind);
            IF sel >= 0 THEN
                ListView_EnsureVisible(ctrl^.wnd, sel, TRUE);
                ListView_SetItemState(ctrl^.wnd, sel, LVIS_SELECTED BOR LVIS_FOCUSED, LVIS_SELECTED BOR LVIS_FOCUSED);
            END;
        |
        MultiPage:
            str := text;
            str[HIGH(str)] := '';
            item.mask := TCIF_TEXT;
            item.pszText := ADR(str);
            idx := TabCtrl_GetCurSel(ctrl^.wnd);
            TabCtrl_SetItem(ctrl^.wnd, idx, item);
        ELSE
        END;
    END;
END ControlSetText;

PROCEDURE ControlGetText(w : Window; ctrlId : CARDINAL; VAR OUT text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
    sel         : INTEGER;
    index       : CARDINAL;
    item        : TCITEM;
BEGIN
    text := "";

    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        PushButton, ToggleButton, CheckBox, RadioButton, GroupBox:
            Button_GetText(ctrl^.wnd, text, HIGH(text)+1);
        |
        DropDownList:
            ComboBox_GetText(ctrl^.wnd, text, HIGH(text)+1);
        |
        ComboBox:
            (*ComboBox_GetText(ctrl^.wnd, text, HIGH(text)+1);*)
            sel := SendMessage(ctrl^.wnd, WM_GETTEXT, HIGH(text)+1, CAST(DWORD, ADR(text)));
        |
        TextEdit:
            Edit_GetText(ctrl^.wnd, text, HIGH(text)+1);
        |
        TextLabel:
            Static_GetText(ctrl^.wnd, text, HIGH(text)+1);
        |
        ListBox:
            sel := ListView_GetNextItem(ctrl^.wnd, -1, LVNI_SELECTED);
            IF sel >= 0 THEN
                ListView_GetItemText(ctrl^.wnd, sel, 0, text, HIGH(text)+1);
            END;
        |
        MultiPage:
            text[0] := '';
            item.mask := TCIF_TEXT;
            item.pszText := ADR(text);
            item.cchTextMax := HIGH(text)+1;
            index := TabCtrl_GetCurSel(ctrl^.wnd);
            TabCtrl_GetItem(ctrl^.wnd, index, item);
            IF item.pszText # CAST(LPTSTR,ADR(text)) THEN
                IF item.pszText # NIL THEN
                    Assign(item.pszText^, text);
                    (*text := item.pszText^;*)
                ELSE
                    text[0] := '';
                END;
            END;
        ELSE
        END;
    END;
END ControlGetText;

PROCEDURE ControlGetTextLength(w : Window; ctrlId : CARDINAL) : INTEGER;
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        PushButton, ToggleButton, CheckBox, RadioButton, GroupBox:
            RETURN Button_GetTextLength(ctrl^.wnd);
        |
        ComboBox:
            (*RETURN ComboBox_GetTextLength(ctrl^.wnd);*)
            RETURN SendMessage(ctrl^.wnd, WM_GETTEXTLENGTH, 0, 0);
        |
        TextLabel:
            RETURN Static_GetTextLength(ctrl^.wnd);
        |
        TextEdit:
            RETURN Edit_GetTextLength(ctrl^.wnd);
        ELSE
        END;
    END;
    RETURN -1;
END ControlGetTextLength;

PROCEDURE ControlGetSelectedText(w : Window; ctrlId : CARDINAL; VAR OUT text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
    sel         : INTEGER;
BEGIN
    text := "";

    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        DropDownList, ComboBox:
            ComboBox_GetLBText(ctrl^.wnd, ComboBox_GetCurSel(ctrl^.wnd), text);
        |
        ListBox:
            sel := ListView_GetNextItem(ctrl^.wnd, -1, LVNI_SELECTED);
            IF sel >= 0 THEN
                ListView_GetItemText(ctrl^.wnd, sel, 0, text, HIGH(text)+1);
            END;
        ELSE
        END;
    END;
END ControlGetSelectedText;

PROCEDURE ControlSetNumericValue(w : Window; ctrlId : CARDINAL; num : INTEGER);
VAR
    ctrl        : ControlInfoPointer;
    text        : ARRAY [0..63] OF CHAR;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        IntToStr(num, text);

        CASE ctrl^.type OF
        SpinButton:
            UpDown_SetPos(ctrl^.spinWnd, num);
            Edit_SetText(ctrl^.wnd, text);
        |
        TextEdit:
            Edit_SetText(ctrl^.wnd, text);
        |
        TextLabel:
            Static_SetText(ctrl^.wnd, text);
        |
        PushButton, ToggleButton, CheckBox, RadioButton, GroupBox:
            Button_SetText(ctrl^.wnd, text);
        ELSE
        END;
    END;
END ControlSetNumericValue;

PROCEDURE ControlGetNumericValue(w : Window; ctrlId : CARDINAL; VAR OUT num : INTEGER) : BOOLEAN;
VAR
    ctrl        : ControlInfoPointer;
    text        : ARRAY [0..63] OF CHAR;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        num := 0;
        CASE ctrl^.type OF
        SpinButton, TextEdit:
            Edit_GetText(ctrl^.wnd, text, HIGH(text)+1);
            IF StrToInt(text, num) THEN
                RETURN TRUE;
            END;
        |
        TextLabel:
            Static_GetText(ctrl^.wnd, text, HIGH(text)+1);
            IF StrToInt(text, num) THEN
                RETURN TRUE;
            END;
        |
        PushButton, ToggleButton, CheckBox, RadioButton, GroupBox:
            Button_GetText(ctrl^.wnd, text, HIGH(text)+1);
            IF StrToInt(text, num) THEN
                RETURN TRUE;
            END;
        ELSE
        END;
    END;
    RETURN FALSE;
END ControlGetNumericValue;

PROCEDURE ControlSetSelectedItem(w : Window; ctrlId : CARDINAL; sel : CARDINAL) : BOOLEAN;
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            ListView_EnsureVisible(ctrl^.wnd, sel, TRUE);
            RETURN ListView_SetItemState(ctrl^.wnd, sel, LVIS_SELECTED BOR LVIS_FOCUSED, LVIS_SELECTED BOR LVIS_FOCUSED);
        |
        DropDownList, ComboBox:
            RETURN ComboBox_SetCurSel(ctrl^.wnd, sel) <> CB_ERR;
        |
        MultiPage:
(*MVN+*)
(*
            RETURN TabCtrl_SetCurSel(ctrl^.wnd, sel) >= 0;
*)
(*MVN-*)
(*MVN+*)
            IF sel <= ctrl^.count THEN
                MultiPageSetActivePage(ctrl, sel);
                RETURN TRUE;
            ELSE
                RETURN FALSE;
            END;
(*MVN-*)
        ELSE
        END;
    END;
    RETURN FALSE;
END ControlSetSelectedItem;

PROCEDURE ControlGetSelectedItem(w : Window; ctrlId : CARDINAL) : INTEGER;
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            RETURN ListView_GetNextItem(ctrl^.wnd, -1, LVNI_SELECTED);
        |
        ComboBox, DropDownList:
            RETURN ComboBox_GetCurSel(ctrl^.wnd);
        |
        MultiPage:
            RETURN TabCtrl_GetCurSel(ctrl^.wnd);
        ELSE
        END;
    END;
    RETURN -1;
END ControlGetSelectedItem;

PROCEDURE ControlGetSelectedItemState(w : Window; ctrlId : CARDINAL; row : CARDINAL) : BOOLEAN;
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            RETURN (ListView_GetItemState(ctrl^.wnd, row, LVIS_SELECTED) BAND LVIS_SELECTED) # 0;
        ELSE
        END;
    END;
    RETURN FALSE;
END ControlGetSelectedItemState;

PROCEDURE ControlSetSelectedItemState(w : Window; ctrlId : CARDINAL; row : CARDINAL; selected : BOOLEAN) : BOOLEAN;
VAR
    ctrl    : ControlInfoPointer;
    sel     : UINT;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            IF selected THEN
                sel := LVIS_SELECTED BOR LVIS_FOCUSED;
            ELSE
                sel := LVIS_FOCUSED;
            END;
            ListView_EnsureVisible(ctrl^.wnd, row, TRUE);
            RETURN ListView_SetItemState(ctrl^.wnd, row, sel, LVIS_SELECTED BOR LVIS_FOCUSED) # FALSE;
        ELSE
        END;
    END;
    RETURN FALSE;
END ControlSetSelectedItemState;

PROCEDURE ControlGetSelectedItemCount(w : Window; ctrlId : CARDINAL) : CARDINAL;
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        IF ctrl^.type = ListBox THEN
            RETURN ListView_GetSelectedCount(ctrl^.wnd);
        END;
    END;
    RETURN 0;
END ControlGetSelectedItemCount;

PROCEDURE ControlGetSelectedItems(w : Window; ctrlId : CARDINAL; VAR OUT items : ARRAY OF CARDINAL) : CARDINAL;
VAR
    ctrl        : ControlInfoPointer;
    count       : INTEGER;
    sel         : INTEGER;
    i           : ADRCARD;
    highItems   : ADRCARD;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        IF ctrl^.type = ListBox THEN
            count := ListView_GetSelectedCount(ctrl^.wnd);
            IF count > 0 THEN
                sel := -1;
                FOR i := 0 TO VAL(ADRCARD, count)-1 DO
                    sel := ListView_GetNextItem(ctrl^.wnd, sel, LVNI_SELECTED);
                    IF sel <> -1 THEN
                        highItems := HIGH(items);
                        IF i <= highItems THEN
                            items[i] := sel;
                        END;
                    END;
                END;
            END;
            RETURN count;
        END;
    END;
    RETURN 0;
END ControlGetSelectedItems;

PROCEDURE ControlSetButtonState(w : Window; ctrlId : CARDINAL; checked : BOOLEAN);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        CheckBox, ToggleButton:
            Button_SetCheck(ctrl^.wnd, ORD(checked));
        |
        PushButton:
            IF checked THEN
                Button_Click(ctrl^.wnd);
            END;
        |
        RadioButton:
            IF checked THEN
                ControlSetRadioGroup(w, ctrl^.group, ctrl^.idNum);
            END;
        ELSE
        END;
    END;
END ControlSetButtonState;

PROCEDURE ControlGetButtonState(w : Window; ctrlId : CARDINAL) : BOOLEAN;
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        CheckBox, RadioButton, ToggleButton:
            RETURN Button_GetCheck(ctrl^.wnd) = BST_CHECKED;
        ELSE
        END;
    END;
    RETURN FALSE;
END ControlGetButtonState;

PROCEDURE ControlSetDefaultButton(w : Window; ctrlId : CARDINAL);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF (ctrl <> NIL) AND (ctrl^.type = PushButton) AND (NOT ctrl^.default) THEN
        DoControlSetDefaultButton(w, ctrl);
    END;
END ControlSetDefaultButton;

PROCEDURE ControlSetRadioGroup(w : Window; group : CARDINAL; ctrlId : CARDINAL);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    IF IsWindow(w) AND (w^.clientType = FormClient) THEN
        ctrl := FindControl(w, ctrlId);
        IF ctrl <> NIL THEN
            ctrl := GetControlFirstSibling(ctrl);
            WHILE ctrl <> NIL DO
                IF (ctrl^.type = RadioButton) AND (ctrl^.group = group) THEN
                    Button_SetCheck(ctrl^.wnd, ORD(ctrl^.idNum = ctrlId));
                END;

                ctrl := ctrl^.next;
            END;
        END;
    END;
END ControlSetRadioGroup;

PROCEDURE ControlGetRadioGroup(w : Window; group : CARDINAL) : CARDINAL;
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    IF IsWindow(w) AND (w^.clientType = FormClient) THEN
        ctrl := FindRadioGroup(w, group);
        IF ctrl <> NIL THEN
            ctrl := GetControlFirstSibling(ctrl);
            WHILE ctrl <> NIL DO
                IF (ctrl^.type = RadioButton) AND (ctrl^.group = group) THEN
                    IF Button_GetCheck(ctrl^.wnd) = BST_CHECKED THEN
                        RETURN ctrl^.idNum;
                    END;
                END;

                ctrl := ctrl^.next;
            END;
        END;
    END;
    RETURN 0;
END ControlGetRadioGroup;

PROCEDURE CheckControlColumnAutosize(ctrl : ControlInfoPointer; column : ADRCARD);
VAR
    info        : ColumnAutoSizePointer;
BEGIN
    IF (NOT ctrl^.autoSizePosted) AND
       (
        (ctrl^.columnInfo = NIL) OR
        (ctrl^.columnInfo^[column].width < 0)
       )
    THEN
        (* we post this message so a bunch of item adds can be done
           and we will do the autosize after all have been done.
        *)
        ctrl^.autoSizePosted := TRUE;

        NEW(info);
        info^.wnd := ctrl^.wnd;
        info^.numColumns := ctrl^.count;
        info^.info := ctrl^.columnInfo;
        info^.posted := ADR(ctrl^.autoSizePosted);
        WINUSER.PostMessage(ctrl^.w^.clientWnd,
                            WM_COLUMN_AUTOSIZE,
                            CAST(WPARAM, info), 0);
    END;
END CheckControlColumnAutosize;

PROCEDURE ControlAppendItem(w : Window; ctrlId : CARDINAL; text : ARRAY OF CHAR) : INTEGER;
VAR
    ctrl        : ControlInfoPointer;
    retVal      : INTEGER;
    lvItem      : LVITEM;
BEGIN
    retVal := -1;

    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            lvItem.mask := LVIF_TEXT;
            lvItem.iItem := MAX(INTEGER);
            lvItem.iSubItem := 0;
            lvItem.pszText := ADR(text);
            retVal := ListView_InsertItem(ctrl^.wnd, lvItem);
            CheckControlColumnAutosize(ctrl, 0);
        |
        DropDownList, ComboBox:
            retVal := ComboBox_AddString(ctrl^.wnd, text);
        ELSE
        END;
    END;
    RETURN retVal;
END ControlAppendItem;

PROCEDURE ControlAppendItemColumns(w : Window; ctrlId : CARDINAL; strs : ARRAY OF StringData) : INTEGER;
VAR
    ctrl        : ControlInfoPointer;
    retVal      : INTEGER;
    lvItem      : LVITEM;
    top         : ADRCARD;
    i           : ADRCARD;
    highStrs    : ADRCARD;
BEGIN
    retVal := -1;

    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        IF ctrl^.type = ListBox THEN
            lvItem.mask := LVIF_TEXT;
            lvItem.iItem := MAX(INTEGER);
            lvItem.iSubItem := 0;
            lvItem.pszText := CAST(LPTSTR, strs[0]);
            retVal := ListView_InsertItem(ctrl^.wnd, lvItem);
            CheckControlColumnAutosize(ctrl, 0);

            IF retVal >= 0 THEN
                top := ctrl^.count-1;
                highStrs := HIGH(strs);
                IF highStrs < top THEN
                    top := HIGH(strs);
                END;
                FOR i := 1 TO top DO
                    ListView_SetItemText(ctrl^.wnd, retVal, i, strs[i]^);
                    CheckControlColumnAutosize(ctrl, i);
                END;
            END;
        END;
    END;
    RETURN retVal;
END ControlAppendItemColumns;

PROCEDURE ControlInsertItem(w : Window; ctrlId : CARDINAL; position : CARDINAL; text : ARRAY OF CHAR) : INTEGER;
VAR
    ctrl        : ControlInfoPointer;
    lvItem      : LVITEM;
    retVal      : INTEGER;
BEGIN
    retVal := -1;

    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            lvItem.mask := LVIF_TEXT;
            lvItem.iItem := position;
            lvItem.iSubItem := 0;
            lvItem.pszText := ADR(text);
            retVal := ListView_InsertItem(ctrl^.wnd, lvItem);
            CheckControlColumnAutosize(ctrl, 0);
        |
        DropDownList, ComboBox:
            retVal := ComboBox_InsertString(ctrl^.wnd, position, text);
        ELSE
        END;
    END;
    RETURN retVal;
END ControlInsertItem;

PROCEDURE ControlInsertItemColumns(w : Window; ctrlId : CARDINAL; position : CARDINAL; strs : ARRAY OF StringData) : INTEGER;
VAR
    ctrl        : ControlInfoPointer;
    retVal      : INTEGER;
    lvItem      : LVITEM;
    top         : ADRCARD;
    i           : ADRCARD;
BEGIN
    retVal := -1;

    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        IF ctrl^.type = ListBox THEN
            lvItem.mask := LVIF_TEXT;
            lvItem.iItem := position;
            lvItem.iSubItem := 0;
            lvItem.pszText := CAST(LPTSTR, strs[0]);
            retVal := ListView_InsertItem(ctrl^.wnd, lvItem);
            CheckControlColumnAutosize(ctrl, 0);

            IF retVal >= 0 THEN
                top := Min(ctrl^.count-1, HIGH(strs));
                FOR i := 1 TO top DO
                    ListView_SetItemText(ctrl^.wnd, retVal, i, strs[i]^);
                    CheckControlColumnAutosize(ctrl, i);
                END;
            END;
        END;
    END;
    RETURN retVal;
END ControlInsertItemColumns;

PROCEDURE ControlSetItemText(w : Window; ctrlId : CARDINAL; item, column : CARDINAL; text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
    saveSel     : INTEGER;
    retVal      : INTEGER;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            IF column < ctrl^.count THEN
                ListView_SetItemText(ctrl^.wnd, item, column, text);
                CheckControlColumnAutosize(ctrl, column);
            END;
        |
        DropDownList, ComboBox:
            saveSel := ComboBox_GetCurSel(ctrl^.wnd);
            ComboBox_DeleteString(ctrl^.wnd, item);
            retVal := ComboBox_InsertString(ctrl^.wnd, item, text);
            ComboBox_SetCurSel(ctrl^.wnd, saveSel);
        ELSE
        END;
    END;
END ControlSetItemText;

PROCEDURE ControlGetItemText(w : Window; ctrlId : CARDINAL; item, column : CARDINAL; VAR OUT text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
    count       : CARDINAL;
BEGIN
    text := "";

    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        IF ctrl^.type = ListBox THEN
            count := ListView_GetItemCount(ctrl^.wnd);
            IF item < count THEN
                ListView_GetItemText(ctrl^.wnd, item, column, text, HIGH(text)+1);
            END;
        ELSIF (ctrl^.type = ComboBox) OR (ctrl^.type = DropDownList) THEN
            count := ComboBox_GetCount(ctrl^.wnd);
            IF item < count THEN
                ComboBox_GetLBText(ctrl^.wnd, item, text);
            END;
        END;
    END;
END ControlGetItemText;

PROCEDURE ControlSetListHeader(w : Window; ctrlId : CARDINAL; column : INTEGER; text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
    item        : HDITEM;
    hw          : HWND;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF (ctrl <> NIL)
        AND (ctrl^.type = ListBox)
        AND (column < VAL(INTEGER, ctrl^.count))
    THEN
        hw := ListView_GetHeader(ctrl^.wnd);
        IF hw <> NIL THEN
            item.pszText := ADR(text);
            item.cchTextMax := HIGH(text)+1;
            item.mask := HDI_TEXT;
            Header_SetItem(hw, column, item);
            CheckControlColumnAutosize(ctrl, column);
        END;
    END;
END ControlSetListHeader;

PROCEDURE ControlGetListHeader(w : Window; ctrlId : CARDINAL; column : INTEGER; VAR OUT text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
    item        : HDITEM;
    hw          : HWND;
BEGIN
    text := "";
    ctrl := FindControl(w, ctrlId);
    IF (ctrl <> NIL)
        AND (ctrl^.type = ListBox)
        AND (column < VAL(INTEGER, ctrl^.count))
    THEN
        hw := ListView_GetHeader(ctrl^.wnd);
        IF hw <> NIL THEN
            item.mask := HDI_TEXT;
            item.pszText := ADR(text);
            item.cchTextMax := HIGH(text)+1;
            Header_GetItem(hw, column, item);
        END;
    END;
END ControlGetListHeader;

PROCEDURE ControlGetItemCount(w : Window; ctrlId : CARDINAL) : CARDINAL;
VAR
    ctrl        : ControlInfoPointer;
    count       : CARDINAL;
BEGIN
    count := 0;
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            count := ListView_GetItemCount(ctrl^.wnd);
        |
        DropDownList, ComboBox:
            count := ComboBox_GetCount(ctrl^.wnd);
        ELSE
        END;
    END;
    RETURN count;
END ControlGetItemCount;

PROCEDURE ControlRemoveItem(w : Window; ctrlId : CARDINAL; item : CARDINAL);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            ListView_DeleteItem(ctrl^.wnd, item);
        |
        DropDownList, ComboBox:
            ComboBox_DeleteString(ctrl^.wnd, item);
        ELSE
        END;
    END;
END ControlRemoveItem;

PROCEDURE ControlRemoveAllItems(w : Window; ctrlId : CARDINAL);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            ListView_DeleteAllItems(ctrl^.wnd);
        |
        DropDownList, ComboBox:
            ComboBox_ResetContent(ctrl^.wnd);
        ELSE
        END;
    END;
END ControlRemoveAllItems;

PROCEDURE ControlGetItemData(w : Window; ctrlId : CARDINAL; item : CARDINAL) : ADDRESS;
VAR
    ctrl        : ControlInfoPointer;
    lvItem      : LVITEM;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            lvItem.mask := LVIF_PARAM;
            lvItem.iItem := item;
            lvItem.iSubItem := 0;
            ListView_GetItem(ctrl^.wnd, lvItem);
            RETURN CAST(ADDRESS, lvItem.lParam);
        |
        DropDownList, ComboBox:
            RETURN CAST(ADDRESS, ComboBox_GetItemData(ctrl^.wnd, item));
        ELSE
        END;
    END;
    RETURN NIL;
END ControlGetItemData;

PROCEDURE ControlSetItemData(w : Window; ctrlId : CARDINAL; item : CARDINAL; data : ADDRESS);
VAR
    ctrl        : ControlInfoPointer;
    lvItem      : LVITEM;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            lvItem.mask := LVIF_PARAM;
            lvItem.lParam := CAST(LPARAM, data);
            lvItem.iItem := item;
            lvItem.iSubItem := 0;
            ListView_SetItem(w^.clientWnd, lvItem);
        |
        DropDownList, ComboBox:
            ComboBox_SetItemData(ctrl^.wnd, item, CAST(LPARAM, data));
        ELSE
        END;
    END;
END ControlSetItemData;

PROCEDURE ControlFindItemByData(w : Window; ctrlId : CARDINAL; data : ADDRESS) : INTEGER;
VAR
    lvFind      : LVFINDINFO;
    sel         : INTEGER;
    idx         : CARDINAL;
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        CASE ctrl^.type OF
        ListBox:
            lvFind.flags := LVFI_PARAM BOR LVFI_PARTIAL;
            lvFind.lParam := CAST(LPARAM, data);
            sel := ListView_FindItem(ctrl^.wnd, -1, lvFind);
            IF sel >= 0 THEN
                RETURN sel;
            END;
        |
        DropDownList, ComboBox:
            IF ComboBox_FindItemData(ctrl^.wnd, -1, CAST(INTEGER, data), idx) THEN
                RETURN idx;
            END;
        ELSE
        END;
    END;
    RETURN -1;
END ControlFindItemByData;

PROCEDURE ControlSetColumnWidth(w : Window; ctrlId : CARDINAL; column : ADRINT; width : INTEGER);
VAR
    ctrl        : ControlInfoPointer;
    wid, dummy  : INTEGER;
    i           : ADRCARD;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        IF (ctrl^.type = ListBox) AND (column < VAL(ADRINT, ctrl^.count)) THEN
            wid := width;
            IF wid > 0 THEN
                dummy := 1;
                FormToPixel(w, wid, dummy);
            END;
            IF column < 0 THEN
                <*/PUSH/NOWARN:U*>
                FOR i := 0 TO VAL(ADRCARD, ctrl^.count)-1 DO
                <*/POP*>
                    IF width < 0 THEN
                        wid := LVSCW_AUTOSIZE;
                    END;
                    IF ctrl^.columnInfo <> NIL THEN
                        ctrl^.columnInfo^[i].width := width;

                        IF ctrl^.columnInfo^[i].header <> NIL THEN
                            wid := LVSCW_AUTOSIZE_USEHEADER;
                        END;
                    END;
                    ListView_SetColumnWidth(ctrl^.wnd, i, wid);
                END;
            ELSE
                IF width < 0 THEN
                    wid := LVSCW_AUTOSIZE;
                END;
                IF ctrl^.columnInfo <> NIL THEN
                    ctrl^.columnInfo^[column].width := width;

                    IF ctrl^.columnInfo^[column].header <> NIL THEN
                        wid := LVSCW_AUTOSIZE_USEHEADER;
                    END;
                END;
                ListView_SetColumnWidth(ctrl^.wnd, column, wid);
            END;
            IF ctrl^.visible THEN
                ControlShow (w, ctrlId, TRUE);
                (* This is a partial solution of bug 3982. necesity of this is not clear. *)
            END;
        END;
    END;
END ControlSetColumnWidth;

PROCEDURE ControlAppendText(w : Window; ctrlId : CARDINAL; text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
    chIndex     : INTEGER;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF (ctrl <> NIL) AND (ctrl^.type = TextEdit) AND ctrl^.multiLine THEN
        chIndex := Edit_GetTextLength(ctrl^.wnd);

        Edit_SetSel(ctrl^.wnd, chIndex, chIndex);
        Edit_ReplaceSel(ctrl^.wnd, text);
    END;
END ControlAppendText;

PROCEDURE ControlAppendLine(w : Window; ctrlId : CARDINAL; text : ARRAY OF CHAR);
BEGIN
    ControlAppendText(w, ctrlId, text);
    ControlAppendText(w, ctrlId, NewLine);
END ControlAppendLine;

PROCEDURE ControlGetLineCount(w : Window; ctrlId : CARDINAL) : CARDINAL;
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF (ctrl <> NIL) AND (ctrl^.type = TextEdit) AND ctrl^.multiLine THEN
        RETURN Edit_GetLineCount(ctrl^.wnd);
    END;
    RETURN 0;
END ControlGetLineCount;

PROCEDURE ControlGetLineLength(w : Window; ctrlId : CARDINAL; lineNum : CARDINAL) : CARDINAL;
VAR
    ctrl        : ControlInfoPointer;
    chIndex     : INTEGER;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF (ctrl <> NIL) AND
       (ctrl^.type = TextEdit) AND
       ctrl^.multiLine AND
       (lineNum > 0)
    THEN
        chIndex := Edit_LineIndex(ctrl^.wnd, lineNum-1);

        RETURN Edit_LineLength(ctrl^.wnd, chIndex);
    END;
    RETURN 0;
END ControlGetLineLength;

PROCEDURE ControlGetLineText(w : Window; ctrlId : CARDINAL; lineNum : CARDINAL; VAR OUT text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
    count       : ADRCARD;
    highText    : ADRCARD;
BEGIN
    text := "";

    ctrl := FindControl(w, ctrlId);
    IF (ctrl <> NIL) AND
       (ctrl^.type = TextEdit) AND
       ctrl^.multiLine AND
       (lineNum > 0)
    THEN
        count := Edit_GetLine(ctrl^.wnd, lineNum-1, text, HIGH(text)+1);

        IF count <> 0 THEN
            highText := HIGH(text);
            IF count <= highText THEN
                text[count] := '';
            END;
        END;
    END;
END ControlGetLineText;

PROCEDURE ControlSetLineText(w : Window; ctrlId : CARDINAL; lineNum : CARDINAL; text : ARRAY OF CHAR);
VAR
    ctrl        : ControlInfoPointer;
    chSt, chEnd : INTEGER;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF (ctrl <> NIL) AND
       (ctrl^.type = TextEdit) AND
       ctrl^.multiLine AND
       (lineNum > 0)
    THEN
        chSt := Edit_LineIndex(ctrl^.wnd, lineNum-1);
        chEnd := Edit_LineIndex(ctrl^.wnd, lineNum);
        Edit_SetSel(ctrl^.wnd, chSt, chEnd);
        Edit_ReplaceSel(ctrl^.wnd, text);
        chSt := chSt + INT(LENGTH(text));
        Edit_SetSel(ctrl^.wnd, chSt, chSt);
        Edit_ReplaceSel(ctrl^.wnd, NewLine);
    END;
END ControlSetLineText;

PROCEDURE ControlRemoveLine(w : Window;
                            ctrlId : CARDINAL;
                            lineNum : CARDINAL);
VAR
    ctrl        : ControlInfoPointer;
    chSt, chEnd : INTEGER;
    text        : ARRAY [0..0] OF CHAR;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF (ctrl <> NIL) AND
       (ctrl^.type = TextEdit) AND
       ctrl^.multiLine AND
       (lineNum > 0)
    THEN
        chSt := Edit_LineIndex(ctrl^.wnd, lineNum-1);
        chEnd := Edit_LineIndex(ctrl^.wnd, lineNum);
        Edit_SetSel(ctrl^.wnd, chSt, chEnd);

        text[0] := '';
        Edit_ReplaceSel(ctrl^.wnd, text);
    END;
END ControlRemoveLine;

PROCEDURE ControlPositionCaret(w : Window; ctrlId : CARDINAL; lineNum, column : CARDINAL);
VAR
    ctrl        : ControlInfoPointer;
    chIndex     : INTEGER;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF (ctrl <> NIL) AND (ctrl^.type = TextEdit) THEN
        IF lineNum = MAX(CARDINAL) THEN
            lineNum := Edit_GetLineCount(ctrl^.wnd);
        ELSIF lineNum = 0 THEN
            lineNum := 1;
        END;
        chIndex := Edit_LineIndex(ctrl^.wnd, lineNum-1);
        chIndex := chIndex + INT(column);
        Edit_SetSel(ctrl^.wnd, chIndex, chIndex);
        Edit_ScrollCaret(ctrl^.wnd);
    END;
END ControlPositionCaret;

PROCEDURE ControlRedraw(w : Window; ctrlId : CARDINAL; yes : BOOLEAN);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        IF yes THEN
            WINUSER.SendMessage(ctrl^.wnd, WM_SETREDRAW, ORD(TRUE), 0);
            InvalidateRect(ctrl^.wnd, NIL_RECT, TRUE);
        ELSE
            WINUSER.SendMessage(ctrl^.wnd, WM_SETREDRAW, ORD(FALSE), 0);
        END;
    END;
END ControlRedraw;

PROCEDURE ControlEnable(w : Window; ctrlId : CARDINAL; yes : BOOLEAN);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        ctrl^.enabled := yes;
        yes := yes OR w^.formEditMode;

        EnableWindow(ctrl^.wnd, yes);
        IF ctrl^.spinWnd <> NIL THEN
            EnableWindow(ctrl^.spinWnd, yes);
        END;
    END;
END ControlEnable;

PROCEDURE GetControlMultiPageAncestor(ctrl : ControlInfoPointer) : ControlInfoPointer;
BEGIN
    WHILE (ctrl # NIL) AND (ctrl^.parent # NIL) AND (ctrl^.parent^.type # MultiPage) DO
        ctrl := ctrl^.parent;
    END;

    IF (ctrl = NIL) OR (ctrl^.parent = NIL) THEN
        RETURN NIL;
    END;

    RETURN ctrl^.parent;
END GetControlMultiPageAncestor;

PROCEDURE ControlShow(w : Window; ctrlId : CARDINAL; yes : BOOLEAN);
CONST
    show : ARRAY BOOLEAN OF CARDINAL = {SW_HIDE, SW_SHOWNA};
VAR
    ctrl, mpAncestor : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        ctrl^.visible := yes;
        yes := yes OR w^.formEditMode;

        mpAncestor := GetControlMultiPageAncestor(ctrl);
        IF mpAncestor # NIL THEN
            IF  mpAncestor^.pageVisible THEN
                MultiPageSetActivePage(mpAncestor, mpAncestor^.currentPage);
            END;
        ELSE
            ShowWindow(ctrl^.wnd, show[yes]);
            IF ctrl^.spinWnd <> NIL THEN
                ShowWindow(ctrl^.spinWnd, show[yes]);
            END;
        END;
    END;
END ControlShow;

PROCEDURE ControlSetFocus(w : Window; ctrlId : CARDINAL);
VAR
    ctrl        : ControlInfoPointer;
BEGIN
    ctrl := FindControl(w, ctrlId);
    IF ctrl <> NIL THEN
        WINUSER.PostMessage(w^.clientWnd, WM_NEXTDLGCTL, CAST(WPARAM, ctrl^.wnd), ORD(TRUE));
    END;
END ControlSetFocus;

(* functions that export native operating system values *)

PROCEDURE GetWindowHandle(w : Window) : ADDRESS;
BEGIN
    IF IsWindow(w) THEN
        RETURN w^.wnd;
    END;
    RETURN NIL;
END GetWindowHandle;

PROCEDURE GetClientHandle(w : Window) : ADDRESS;
BEGIN
    IF IsWindow(w) THEN
        RETURN w^.clientWnd;
    END;
    RETURN NIL;
END GetClientHandle;

PROCEDURE GetDrawableHandle(draw : Drawable) : ADDRESS;
BEGIN
    IF draw <> NIL THEN
        RETURN draw^.winDC;
    END;
    RETURN NIL;
END GetDrawableHandle;

PROCEDURE CreateExternalWindowHandle(wnd : ADDRESS) : Window;
VAR
    w   : Window;
BEGIN
    IF wnd <> NIL THEN
        NEW(w);
        InitWindow(w);
        w^.wnd := wnd;
        w^.clientWnd := wnd;
        w^.external := TRUE;
        w^.extRefCount := 0;
        RETURN w;
    END;
    RETURN NIL;
END CreateExternalWindowHandle;

(*PROCEDURE GetStartupDisplayMode;
VAR
    startInfo   : STARTUPINFO;
BEGIN
    GetStartupInfo(startInfo);
    CASE startInfo.wShowWindow OF
    SW_HIDE:
        StartupDisplayMode := DisplayHidden;
    |
    SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE, SW_MINIMIZE:
        StartupDisplayMode := DisplayMinimized;
    |
    SW_SHOWMAXIMIZED:
        StartupDisplayMode := DisplayMaximized;
    |
    SW_SHOWNA, SW_SHOWNOACTIVATE:
        StartupDisplayMode := DisplayVisible;
    ELSE
        StartupDisplayMode := DisplayNormal;
    END;
END GetStartupDisplayMode;*)

PROCEDURE SetupHost;
VAR
    osInfo      : OSVERSIONINFO;
BEGIN
    Host := Windows;
    HostMajorVersion := 4;
    HostMinorVersion := 0;
    WindowsNT := FALSE;

    osInfo.dwOSVersionInfoSize := SIZE(osInfo);
    IF GetVersionEx(osInfo) THEN
        HostMajorVersion := osInfo.dwMajorVersion;
        HostMinorVersion := osInfo.dwMinorVersion;

        IF osInfo.dwPlatformId = VER_PLATFORM_WIN32_NT THEN
            WindowsNT := TRUE;
        END;
    END;
END SetupHost;

PROCEDURE GetScreenInfo;
VAR
    dc          : HDC;
    colors      : CARDINAL;
    (*rect        : RECT;*)
BEGIN
    ScreenInfo.xSize := GetSystemMetrics(SM_CXFULLSCREEN);
    ScreenInfo.ySize := GetSystemMetrics(SM_CYFULLSCREEN);

    dc := GetDC(NIL);
    colors := GetDeviceCaps(dc, BITSPIXEL) * GetDeviceCaps(dc, PLANES);
    IF colors > 24 THEN
        colors := 24;
    END;
    ScreenInfo.colors := ORD(1) SHL colors;
    ScreenInfo.xDpi := GetDeviceCaps(dc, LOGPIXELSX);
    ScreenInfo.yDpi := GetDeviceCaps(dc, LOGPIXELSY);
    ReleaseDC(NIL, dc);

    (*
    IF WINUSER.SystemParametersInfo(WINUSER.SPI_GETWORKAREA, 0, ADR(rect), 0) THEN
        ScreenInfo.workArea.x1 := rect.left;
        ScreenInfo.workArea.y1 := rect.top;
        ScreenInfo.workArea.x2 := rect.right;
        ScreenInfo.workArea.y2 := rect.bottom;
    ELSE
        ScreenInfo.workArea.x1 := 0;
        ScreenInfo.workArea.y1 := 0;
        ScreenInfo.workArea.x2 := ScreenInfo.xSize-1;
        ScreenInfo.workArea.y2 := ScreenInfo.ySize-1;
    END;
    *)
END GetScreenInfo;

PROCEDURE GetFont(name : ARRAY OF CHAR; size : CARDINAL) : HFONT;
(* size is in tenths of a point *)
VAR
    <*/PUSH/NOCHECK:U*>
    lf          : LOGFONT;
    <*/POP*>
    dc          : HDC;
    pixels      : CARDINAL;
BEGIN
    ZeroMem(ADR(lf), SIZE(lf));

    dc := GetDC(NIL);
    pixels := GetDeviceCaps(dc, LOGPIXELSY);
    ReleaseDC(NIL, dc);

    lf.lfFaceName := name;
    pixels := size * pixels;
    lf.lfHeight := pixels / 720;
    IF (pixels REM 720) > 360 THEN
        INC(lf.lfHeight);
    END;
    lf.lfHeight := -lf.lfHeight;

    lf.lfPitchAndFamily := FF_DONTCARE;
    lf.lfWeight := FW_NORMAL;
    lf.lfCharSet := WINGDI.DEFAULT_CHARSET(*ANSI_CHARSET*);
    lf.lfWidth := 0;
    lf.lfEscapement := 0;
    lf.lfOrientation := 0;
    lf.lfItalic := FALSE;
    lf.lfUnderline := FALSE;
    lf.lfStrikeOut := FALSE;
    lf.lfOutPrecision := OUT_TT_PRECIS;
    lf.lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lf.lfQuality := DEFAULT_QUALITY;

    (*pixels := WINNLS.GetThreadLocale();
    pixels := WIN32.LANGIDFROMLCID(pixels);
    pixels := WINNLS.GetSystemDefaultLCID();
    pixels := WIN32.LANGIDFROMLCID(pixels);
    pixels := WINNLS.GetUserDefaultLCID();
    pixels := WIN32.LANGIDFROMLCID(pixels);
    pixels := WINNLS.GetUserDefaultUILanguage();*)

    RETURN CreateFontIndirect(lf);
END GetFont;

PROCEDURE GetTabControlFont;
VAR
    size        : CARDINAL;
BEGIN
    IF ScreenInfo.ySize < 768 THEN
        size := 80;
    ELSIF ScreenInfo.ySize = 768 THEN
        size := 90;
    ELSE
        size := 100;
    END;
    TabControlFont := GetFont("Arial", size);
END GetTabControlFont;

PROCEDURE GetStatuslineFont;
VAR
    size        : CARDINAL;
BEGIN
    IF ScreenInfo.ySize < 768 THEN
        size := 80;
    ELSIF ScreenInfo.ySize = 768 THEN
        size := 90;
    ELSE
        size := 100;
    END;
    StatuslineFont := GetFont("Arial", size);
END GetStatuslineFont;

PROCEDURE GetFormFont;
BEGIN
    FormFont := GetFont("MS Shell Dlg 2", 80);
END GetFormFont;

PROCEDURE GetStdCursors;
CONST
    cursor      : ARRAY HitTest OF LPTSTR =
        {IDC_ARROW,
         IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW,
         IDC_SIZEWE, IDC_SIZEWE,
         IDC_SIZENESW, IDC_SIZENS, IDC_SIZENWSE
        };
VAR
    ht          : HitTest;
BEGIN
    FOR ht := MIN(ht) TO MAX(ht) DO
        HitTestCursors[ht] := LoadCursor(NIL, cursor[ht]^);
    END;
    MoveCursor := LoadCursor(NIL, IDC_SIZEALL^);
    DropCursor := LoadCursor(Instance, "DROP-CURSOR");
END GetStdCursors;

PROCEDURE LoadCommonControls;
VAR
    init        : PROCEDURE() [WINDOWS];
    initEx      : PROCEDURE (INITCOMMONCONTROLSEX) [WINDOWS];
    initFlags   : INITCOMMONCONTROLSEX;

    PROCEDURE getProcAddr(h : HANDLE; name : ARRAY OF ACHAR) : ADDRESS;
    BEGIN
        RETURN CAST(ADDRESS, GetProcAddress(h, name));
    END getProcAddr;

BEGIN
    CommCtrlDLL := LoadLibrary("COMCTL32");
    IF CommCtrlDLL <> NIL THEN
        initEx:ADDRESS := getProcAddr(CommCtrlDLL, "InitCommonControlsEx");
        IF initEx:ADDRESS <> NIL THEN
            initFlags.dwSize := SIZE(initFlags);
            initFlags.dwICC := ICC_WIN95_CLASSES (*BOR
                               ICC_STANDARD_CLASSES*);
            initEx(initFlags);

            initFlags.dwSize := SIZE(initFlags);
            initFlags.dwICC := ICC_DATE_CLASSES BOR
                                ICC_USEREX_CLASSES BOR
                                    ICC_COOL_CLASSES;
            initEx(initFlags);

            initFlags.dwSize := SIZE(initFlags);
            initFlags.dwICC := ICC_INTERNET_CLASSES;
            initEx(initFlags);
        ELSE
            init:ADDRESS := getProcAddr(CommCtrlDLL, "InitCommonControls");
            IF init:ADDRESS <> NIL THEN
                init;
            ELSE
                FreeLibrary(CommCtrlDLL);
                CommCtrlDLL := NIL;
                RETURN;
            END;
        END;
    END;
END LoadCommonControls;

PROCEDURE RegisterAClass(class : WNDCLASS) : BOOLEAN;
VAR
    dummy       : WNDCLASS;
BEGIN
    IF RegisterClass(class) = 0 THEN
        IF NOT GetClassInfo(class.hInstance, class.lpszClassName^, dummy) THEN
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END RegisterAClass;

PROCEDURE InitClasses;
VAR
    class       : WNDCLASS;
BEGIN
    class.style := CS_BYTEALIGNCLIENT;
    class.cbClsExtra := 0;
    class.cbWndExtra := SIZE(ADDRESS);
    class.hInstance := Instance;
    class.hIcon := NIL;
    class.hCursor := LoadCursor(NIL, IDC_ARROW^);
    class.hbrBackground := CAST(HBRUSH, COLOR_BTNFACE+1);
    class.lpszMenuName := NIL;
    class.lpfnWndProc := ToplevelWndProc;
    class.lpszClassName := ADR(ToplevelClassName);
    IF NOT RegisterAClass(class) THEN
        HALT(255);
    END;

    class.style := CS_DBLCLKS BOR CS_HREDRAW BOR CS_VREDRAW;
    class.cbClsExtra := 0;
    class.cbWndExtra := SIZE(ADDRESS);
    class.hInstance := Instance;
    class.hIcon := NIL;
    class.hCursor := LoadCursor(NIL, IDC_ARROW^);
    class.hbrBackground := CAST(HBRUSH, COLOR_WINDOW+1);
    class.lpszMenuName := NIL;
    class.lpfnWndProc := DrawClientWndProc;
    class.lpszClassName := ADR(DrawClientClassName);
    IF NOT RegisterAClass(class) THEN
        HALT(255);
    END;

    class.style := 0;
    class.cbClsExtra := 0;
    class.cbWndExtra := DLGWINDOWEXTRA + SIZE(ADDRESS);
    class.hInstance := Instance;
    class.hIcon := NIL;
    class.hCursor := LoadCursor(NIL, IDC_ARROW^);
    class.hbrBackground := CAST(HBRUSH, COLOR_BTNFACE+1);
    class.lpszMenuName := NIL;
    class.lpfnWndProc := FormClientWndProc;
    class.lpszClassName := ADR(FormClientClassName);
    IF NOT RegisterAClass(class) THEN
        HALT(255);
    END;

    class.style := CS_DBLCLKS;
    class.cbClsExtra := 0;
    class.cbWndExtra := SIZE(ADDRESS);
    class.hInstance := Instance;
    class.hIcon := NIL;
    class.hCursor := LoadCursor(NIL, IDC_ARROW^);
    class.hbrBackground := NIL;
    class.lpszMenuName := NIL;
    class.lpfnWndProc := DragBoxWndProc;
    class.lpszClassName := ADR(DragBoxClassName);
    IF NOT RegisterAClass(class) THEN
        HALT(255);
    END;
END InitClasses;

PROCEDURE Init;
BEGIN
    FirstWindow := NIL;
    MainWindow := NIL;
    (*StartupDisplayMode := DisplayNormal;*)
    DefaultIcon := NIL;
    StrCacheInit := FALSE;
    StrCacheLock := 0;
    LoadedMenus := NIL;
    CachedBitmaps := NIL;
    ClipOwner := NIL;
    HtmlHelpInit := FALSE;
    WasSystemDialogBox := FALSE;

    DragBoxPatternBitmap := NIL;
    DragBoxPatternBrush := NIL;
    DragDC := NIL;
    DragHandlePrimary := NIL;
    DragHandleSecondary := NIL;

    Heap := GetDefaultHeap();

    InitClasses;

    AllocateTlsIndex(TlsIndex);
    SetTlsData(TlsIndex, NIL);

    SetupHost;
    (*GetStartupDisplayMode;*)
    GetScreenInfo;
    GetTabControlFont;
    GetStatuslineFont;
    GetFormFont;
    GetStdCursors;
    LoadCommonControls;

    DragCX := GetSystemMetrics(SM_CXDOUBLECLK);
    DragCY := GetSystemMetrics(SM_CYDOUBLECLK);

    DefaultContext := ADR(DefaultContextInfo);
    DefaultContext^.values := DrawContextDefaults;
    DefaultContext^.drawables := NIL;

    SetResourceFile("");

    Gdip.dll := NIL;
    Gdip.init := FALSE;
END Init;

PROCEDURE Term;
BEGIN
    IF CommCtrlDLL <> NIL THEN
        FreeLibrary(CommCtrlDLL);
    END;

    IF HtmlHelpInit THEN
        HtmlHelp(NIL, NIL_STR, HH_UNINITIALIZE, HtmlHelpCookie);
        HtmlHelpInit := FALSE;
    END;

    DeleteFont(TabControlFont);
    DeleteFont(StatuslineFont);
    DeleteFont(FormFont);
    DestroyDragObjects;

    IF Gdip.dll <> NIL THEN
        Gdip.GdiplusShutdown(Gdip.token);
        FreeLibrary(Gdip.dll);
    END;
END Term;

BEGIN
    IF NOT IsThread THEN
        Init;
    END;
FINALLY
    IF NOT IsThread THEN
        Term;
    END;
END WinShell.
