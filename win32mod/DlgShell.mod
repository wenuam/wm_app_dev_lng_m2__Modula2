(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE DlgShell;
<*/NOWARN:F*>
<*/NOPACK*>

FROM SYSTEM IMPORT
    ADDRESS, ADR, CAST, ADDADR, MAKEADR, ADRINT, IsThread, MACHINEWORD, ADRCARD,
    EXCEPTADR, UNREFERENCED_PARAMETER, FUNC, OutputCallTrace;

FROM EXCEPTIONS IMPORT
    GetMessage;

FROM Strings IMPORT
    Concat, Append;

FROM ExStrings IMPORT
    AppendChar, EqualI, GetNextItem;

FROM Conversions IMPORT
    StrToInt, IntToStr, StrToCard, StrBaseToCard, CardBaseToStr;

FROM FileFunc IMPORT
    FileSpecString,
    File, AccessModes, OpenFile, CreateFile, CloseFile, SetFileBuffer,
    ReadChar, WriteChar, WriteBlock, EOL;

FROM Threads IMPORT
    CriticalSection, CreateCriticalSection, CloseCriticalSection,
    EnterCriticalSection, LeaveCriticalSection,
    AllocateTlsIndex, GetTlsData, SetTlsData;

FROM MemUtils IMPORT
    MoveMem;

FROM WinShell IMPORT
    Window, MainWindow, COORDINATE, WindowTypes, StringData,
    AllocWinShellMem, ReallocWinShellMem, DeallocWinShellMem,
    GetResourceFile,
    GetWindowHandle, GetForegroundWindow,
    GetWindowParent, GetWindowType,
    SetModelessDialog;
IMPORT WinShell;

FROM WIN32 IMPORT
    HWND, RECT, WPARAM, LPARAM, UINT, BOOL, DWORD, HRSRC, HINSTANCE,
    NULL, POINT, COLORREF, HBRUSH, HFONT, HPALETTE, HDC, LPTSTR,
    LRESULT, HBITMAP,
    GetLastError, FindResource, LoadResource, LockResource,
    FreeResource, SizeofResource, LoadLibrary, GetTickCount,
    INT_PTR;

FROM WINUSER IMPORT
    WNDPROC, CallWindowProc, DefWindowProc,
    CheckRadioButton, EndDialog, SetFocus,
    DialogBoxIndirectParam, CreateDialogIndirectParam,
    CreateWindowEx, DestroyWindow,
    WS_POPUP, WS_EX_TOPMOST, CW_USEDEFAULT,
    GetParent, GetDesktopWindow, SetWindowPos, GetDC, ReleaseDC,
    GetWindowRect, GetClientRect, OffsetRect, IsWindow, MoveWindow,
    GetClassName, ScreenToClient,
    GetDlgItem, SetWindowLong, SetWindowLongPtr, GetWindowLong, GetWindowLongPtr,
    GWL_USERDATA, DWLP_MSGRESULT, GWL_STYLE,
    SendMessage, PostMessage, MSG, PeekMessage, PM_NOREMOVE, PM_REMOVE,
    InvalidateRect, ClientToScreen, IsIconic, GetDlgCtrlID,
    SetWindowText, EnableWindow, ShowWindow,
    GetSystemMetrics, FillRect, DrawFocusRect,
    SetCapture, ReleaseCapture,
    (*WinHelp, HELP_CONTEXTPOPUP,*)
    WM_INITDIALOG, WM_DESTROY, WM_COMMAND, WM_TIMER,
    WM_CHAR, (*WM_KEYUP, WM_KEYDOWN,*)
    WM_SETREDRAW, WM_SHOWWINDOW, WM_NEXTDLGCTL,
    WM_MEASUREITEM, WM_DRAWITEM, WM_CTLCOLORSTATIC, WM_NOTIFY,
    WM_SETFONT, WM_HELP,
    WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN, WM_KEYDOWN,
    DM_GETDEFID, DM_SETDEFID, DC_HASDEFID,
    SWP_NOSIZE, SWP_NOZORDER, SWP_NOACTIVATE,
    WM_ACTIVATE, WA_INACTIVE,
    BN_CLICKED, BN_DBLCLK, BN_SETFOCUS, BN_KILLFOCUS,
    EN_KILLFOCUS, EN_CHANGE, EN_SETFOCUS,
    LB_ERR, LBN_KILLFOCUS, LBN_SELCHANGE, LBS_MULTIPLESEL,
    LBN_DBLCLK, LBN_SETFOCUS,
    CBN_KILLFOCUS, CB_ERR, CB_ERRSPACE, CBN_SELCHANGE, CBN_EDITCHANGE,
    CBN_DBLCLK, CBN_SETFOCUS, ODS_FOCUS,
    CB_GETDROPPEDCONTROLRECT,
    SW_SHOW, SW_HIDE, SW_PARENTOPENING, SM_CXSCREEN, SM_CYSCREEN,
    HELPINFO_WINDOW, LOWORD, HIWORD,
    LPMEASUREITEMSTRUCT, LPDRAWITEMSTRUCT, LPNMHDR, LPHELPINFO,
    MessageBeep, MessageBox,
    MB_OK, MB_ICONSTOP, MB_TASKMODAL,
    MB_SETFOREGROUND, MB_TOPMOST,
    SetTimer, KillTimer, TIMERPROC,
    (*SB_LINEUP, SB_LINEDOWN, SB_PAGEUP, SB_PAGEDOWN, SB_THUMBPOSITION,*)
    LPDLGTEMPLATE, RT_DIALOG, DS_SETFONT, DS_SHELLFONT,
    MAKELONG, MAKEINTRESOURCE;
    (*GetKeyboardState, ToAscii;*)

FROM WINGDI IMPORT
    LOGFONT, TEXTMETRIC, LOGPIXELSY,
    FW_NORMAL, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
    PROOF_QUALITY, FF_DONTCARE,
    CreateFontIndirect, CreateSolidBrush, SetTextColor, SetBkColor,
    GetTextMetrics, GetDeviceCaps;

FROM WINX IMPORT
    Instance,
    NULL_HWND, NULL_HBRUSH, NULL_HFONT,
    NIL_RECT, NIL_STR,
    DeleteBrush, DeleteFont, SelectFont,
    GetWindowFont, SetWindowFont,
    SubclassWindow, GetFirstChild, GetNextSibling, GetLOGFONT,
    Edit_GetText, Edit_SetText,
    Edit_GetTextLength, Edit_LimitText, Edit_GetLineCount,
    Edit_GetLine, Edit_SetSel, Edit_ReplaceSel,
    Edit_ScrollCaret, Edit_LineIndex, Edit_LineLength,
    Button_GetText, Button_SetText, Button_SetCheck,
    Static_GetText, Static_SetText, Static_SetBitmap,
    ListBox_GetText, ListBox_AddString, ListBox_DeleteString,
    ListBox_SelectString, ListBox_GetCurSel, ListBox_SetCurSel, ListBox_SetSel,
    ListBox_GetCount, ListBox_ResetContent, ListBox_GetSelCount, ListBox_GetSelItems,
    ListBox_InsertString, ListBox_GetItemData, ListBox_SetItemData,
    ComboBox_GetText, ComboBox_SetText, ComboBox_AddString,
    ComboBox_LimitText, ComboBox_SetCurSel, ComboBox_GetCurSel,
    ComboBox_FindString, ComboBox_SetEditSel, ComboBox_GetCount,
    ComboBox_DeleteString, ComboBox_InsertString, ComboBox_ResetContent,
    ComboBox_GetItemData, ComboBox_SetItemData, ComboBox_SetExtendedUI,
    ComboBox_AddItemData, ComboBox_GetLBText;

FROM COMMCTRL IMPORT
    PROPSHEETHEADER, LPPROPSHEETPAGE,
    PFNPROPSHEETCALLBACK, LPFNPSPCALLBACK,
    PSP_DLGINDIRECT, PSP_USETITLE,
    PSH_PROPSHEETPAGE, PSH_NOAPPLYNOW,
    PSN_SETACTIVE, PSN_KILLACTIVE, PSN_RESET, PSN_APPLY,
    PSNRET_NOERROR, PSNRET_INVALID,
    PSM_SETCURSEL,
    PROPSHEETHEADER_V1_SIZE, PROPSHEETPAGE_V1_SIZE,
    PropertySheet,
    UpDown_GetBuddy, UpDown_SetRange32, UpDown_SetPos, UDN_DELTAPOS, LPNMUPDOWN,
    LVITEM, LVCOLUMN, LV_FINDINFO, LPNMLISTVIEW,
    LVFI_PARTIAL, LVFI_STRING,
    LVIF_PARAM, LVIF_TEXT,
    LVCF_TEXT, LVCF_WIDTH, LVCF_SUBITEM, LVCF_FMT,
    LVCFMT_LEFT, LVCFMT_CENTER, LVCFMT_RIGHT,
    LVIS_SELECTED, LVIS_FOCUSED,
    LVNI_SELECTED,
    LVN_DELETEALLITEMS, LVN_ITEMCHANGED,
    NM_DBLCLK, NM_KILLFOCUS, NM_SETFOCUS,
    LVS_EX_FULLROWSELECT, LVSCW_AUTOSIZE_USEHEADER,
    ListView_InsertItem, ListView_DeleteItem, ListView_DeleteAllItems,
    ListView_SetItemText, ListView_SetItem, ListView_GetItemText,
    ListView_GetItem, ListView_GetStringWidth, ListView_EnsureVisible,
    ListView_GetItemCount, ListView_GetNextItem, ListView_FindItem,
    ListView_SetItemState, ListView_SetExtendedListViewStyle,
    ListView_InsertColumn, ListView_GetColumnWidth, ListView_SetColumnWidth,
    ListView_GetSelectedCount,
    TOOLINFO, TOOLTIPS_CLASS, TTS_NOPREFIX,
    LPSTR_TEXTCALLBACK,
    TTM_ADDTOOL, TTM_TRACKACTIVATE, TTM_TRACKPOSITION, TTM_SETMAXTIPWIDTH,
    TTF_TRACK,
    TTN_GETDISPINFO, LPNMTTDISPINFO;

IMPORT BasicDialogs;

CONST
    MinusOne            = MAX(CARDINAL);
    ToolTipKey          = MAKEADR(1);
    ToolTipStringSize   = 4*1024;
    ToolTipID           = 1;

    CrLf                : ARRAY [0..2] OF CHAR = {CHR(13),CHR(10),CHR(0)};
    Show                : ARRAY BOOLEAN OF CARDINAL = {SW_HIDE, SW_SHOW};

TYPE
    InternalDataRecord =
        RECORD
        key     : ADDRESS;
        data    : ADDRESS;
        amount  : CARDINAL;
        END;

    MatchString         = ARRAY [0..15] OF CHAR;
    MatchListInfoPointer = POINTER TO MatchListInfo;
    MatchListInfo =
        RECORD
        next            : MatchListInfoPointer;
        wnd             : HWND;
        oldWndProc      : WNDPROC;
        ctrlType        : ControlType;
        startPos        : CARDINAL;
        column          : CARDINAL;
        lastKeyTime     : CARDINAL;
        matchStr        : MatchString;
        END;

    INFORMATION =
        RECORD
        wnd             : HWND;
        parent          : Window;
        palette         : HPALETTE;
        dlgFont         : HFONT;
        toolTip         : HWND;
        ctrls           : ControlsPointer;
        pages           : POINTER TO ARRAY [0..0] OF PAGE;
        pageWnd         : POINTER TO ARRAY [0..0] OF HWND;
        pageModified    : POINTER TO ARRAY [0..0] OF BOOLEAN;
        internalData    : POINTER TO ARRAY [1..1] OF InternalDataRecord;
        (*userData        : ADDRESS;*)
        notify          : NotifyProc;
        oldTipWndProc   : WNDPROC;
        numCtrls        : CARDINAL;
        numPages        : CARDINAL;
        lastFocus       : CARDINAL;
        activePage      : CARDINAL;
        numInternal     : CARDINAL;
        internalSize    : CARDINAL;
        helpId          : CARDINAL;
        lastCtrlId      : CARDINAL;
        position        : DialogPositions;
        modeless        : BOOLEAN;
        positionWhenRestored    : BOOLEAN;
        propertySheet   : BOOLEAN;
        modified        : BOOLEAN;
        free            : BOOLEAN;
        suppressNotify  : BOOLEAN;
        firstInit       : BOOLEAN;
        closeWasCancel  : BOOLEAN;
        (*initialized     : BOOLEAN;*)
        toolTipShown    : BOOLEAN;
    END;

    StaticTextInternalRec =
        RECORD
        brush           : HBRUSH;
        font            : HFONT;
        END;
    StaticTextInternalPointer  = POINTER TO StaticTextInternalRec;

    ControlTypeSet      = SET OF ControlType;
    ButtonTypeSet       = SET OF PushButtonType;

CONST
    TimerIdFudge        = 10;
    MaxDlgs             = 16;

    ValidateControls    = ControlTypeSet{
                                         LineEdit,
                                         SpinButton,
                                         MultiLineEdit,
                                         EditTextFile,
                                         ComboBox
                                        };

    ValidateButtons     = ButtonTypeSet{CloseButton, NotifyButton};

    NullDialog  = INFORMATION{NIL,(*hwnd*)
                              NIL,(*parent*)
                              NIL,(*pelette*)
                              NIL,(*font*)
                              NIL,(*tooltip*)
                              NIL,(*ctrls*)
                              NIL,(*pages*)
                              NIL,(*pageWnd*)
                              NIL,(*pageModified*)
                              NIL,(*internalData*)
                              (*NIL,(*userData*)*)
                              NIL_NOTIFY,
                              CAST(WNDPROC, NIL),(*oldTipWndProc*)
                              0,(*numCtrls*)
                              0,(*numPages*)
                              MAX(CARDINAL),(*lastFocus*)
                              0,(*activePage*)
                              0,(*numInternal*)
                              0,(*internalSize*)
                              0,(*helpId*)
                              0,(*lastCtrlId*)
                              NormalPosition,
                              FALSE,(*modeless*)
                              FALSE,(*positionWndRestored*)
                              FALSE,(*propertySheet*)
                              FALSE,(*modified*)
                              TRUE,(*free*)
                              FALSE,(*suppressNotify*)
                              TRUE,(*firstInit*)
                              FALSE,(*closeWasCancel*)
                              (*FALSE,(*initialized*)*)
                              FALSE(*tooltip shown*)
                              };

VAR
    Dialogs             : ARRAY [1..MaxDlgs] OF INFORMATION;
    TlsIndex            : CARDINAL;
    Critic              : CriticalSection;

    ResourceFile        : FileSpecString;
    ResourceInst        : HINSTANCE;

    ControlIdMode       : ControlIdModes;

PROCEDURE ALLOCATE(VAR OUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    AllocWinShellMem(addr, amount);
END ALLOCATE;

PROCEDURE ReALLOCATE(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    ReallocWinShellMem(addr, amount);
END ReALLOCATE;

PROCEDURE DEALLOCATE(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    DeallocWinShellMem(addr, amount);
END DEALLOCATE;

PROCEDURE GetResFile;
VAR
    fname       : FileSpecString;
    inst        : HINSTANCE;
BEGIN
    GetResourceFile(fname);
    IF NOT EqualI(fname, ResourceFile) THEN
        IF fname[0] = '' THEN
            ResourceFile := "";
            ResourceInst := Instance;
        ELSE
            inst := LoadLibrary(fname);
            IF inst <> NIL THEN
                ResourceFile := fname;
                ResourceInst := inst;
            END;
        END;
    END;
END GetResFile;

PROCEDURE GetResNameAndId(name : ARRAY OF CHAR;
                          VAR OUT resName : ARRAY OF CHAR;
                          VAR OUT resId : CARDINAL);
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

PROCEDURE GetDialogParent() : Window;
VAR
    w           : Window;
    thisDlg     : ADRCARD;
BEGIN
    thisDlg := GetLastDialog();
    IF (thisDlg > 0) AND
       (thisDlg <= MaxDlgs) AND
       (NOT Dialogs[thisDlg].free)
    THEN
        IF Dialogs[thisDlg].modeless THEN
            w := Dialogs[thisDlg].parent;
        ELSE
            w := CurrentAsParent;
        END;
    ELSE
        w := GetForegroundWindow();
        IF w = NIL THEN
            w := MainWindow;
        END;
    END;

    IF (w <> NIL) AND (w <> CurrentAsParent) THEN
        WHILE GetWindowType(w) = ChildWindow DO
            w := GetWindowParent(w);
        END;
    END;

    RETURN w;
END GetDialogParent;

PROCEDURE SetControlIdMode(mode : ControlIdModes);
BEGIN
    ControlIdMode := mode;
END SetControlIdMode;

PROCEDURE GetControlIdMode() : ControlIdModes;
BEGIN
    RETURN ControlIdMode;
END GetControlIdMode;

PROCEDURE MultiSelectListBox(ctrlWnd : HWND) : BOOLEAN;
VAR
    style       : DWORD;
BEGIN
    style := GetWindowLong(ctrlWnd, GWL_STYLE);
    RETURN (style BAND LBS_MULTIPLESEL) <> 0;
END MultiSelectListBox;

PROCEDURE ValidateDlgNum(dlgNum : ADRCARD) : BOOLEAN;
BEGIN
    IF (dlgNum >= 1) AND (dlgNum <= MaxDlgs) THEN
        IF NOT Dialogs[dlgNum].free THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END ValidateDlgNum;

PROCEDURE IsDialogValidDlg(dlgNum : CARDINAL) : BOOLEAN;
BEGIN
    RETURN ValidateDlgNum(dlgNum);
END IsDialogValidDlg;

PROCEDURE IsDialogValid() : BOOLEAN;
BEGIN
    RETURN ValidateDlgNum(GetLastDialog());
END IsDialogValid;

PROCEDURE GetHexOrDecNum(str : ARRAY OF CHAR; VAR OUT num : INTEGER) : BOOLEAN;
VAR
    l           : ADRCARD;
    card        : CARDINAL;
    ok          : BOOLEAN;
BEGIN
    l := LENGTH(str);
    IF (l >= 2) AND (CAP(str[l-1]) = 'H') THEN
        ok := StrBaseToCard(str[0..l-2], 16, card);
        num := card;
    ELSIF (l >= 3) AND (str[0] = '0') AND (str[1] = 'x') THEN
        ok := StrBaseToCard(str[2..l], 16, card);
        num := card;
    ELSE
        ok := StrToInt(str, num);
    END;
    RETURN ok;
END GetHexOrDecNum;

PROCEDURE StrAssign(source : NOHIGH ARRAY OF CHAR;
                    VAR OUT dest : NOHIGH ARRAY OF CHAR;
                    maxChars : ADRCARD);
VAR
    i   : ADRCARD;
BEGIN
    (* make maxChars a HIGH bound *)

    DEC(maxChars);

    i := 0;
    WHILE (i < maxChars) AND (source[i] <> '') DO
        dest[i] := source[i];
        INC(i);
    END;
    dest[i] := '';
END StrAssign;

PROCEDURE GetLastDialog() : ADRCARD;
VAR
    index       : ADDRESS;
BEGIN
    index := GetTlsData(TlsIndex);
    RETURN CAST(ADRCARD, index);
END GetLastDialog;

PROCEDURE SetLastDialog(index : ADRCARD);
BEGIN
    SetTlsData(TlsIndex, CAST(ADDRESS, index));
END SetLastDialog;

PROCEDURE GetControlSubscript(dlgNum : CARDINAL; index : CARDINAL) : CARDINAL;
VAR
    i   : ADRCARD;
BEGIN
    IF ControlIdMode = ControlSubscript THEN
        RETURN index;
    ELSE
        WITH Dialogs[dlgNum] DO
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, numCtrls)-1 DO
            <*/POP*>
                IF ctrls^[i].ct <> RadioGroup THEN
                    IF ctrls^[i].id = index THEN
                        RETURN i;
                    END;
                ELSE
                    IF (index >= ctrls^[i].r_first) AND (index <= ctrls^[i].r_last) THEN
                        RETURN i;
                    END;
                END;
            END;
        END;
        RETURN MAX(CARDINAL);
    END;
END GetControlSubscript;

PROCEDURE GetUserControlId(dlgNum : ADRCARD; index : CARDINAL) : CARDINAL;
BEGIN
    IF (ControlIdMode = ControlSubscript) OR (index = MAX(CARDINAL)) THEN
        RETURN index;
    ELSE
        WITH Dialogs[dlgNum] DO
            IF ctrls^[index].ct <> RadioGroup THEN
                RETURN ctrls^[index].id
            ELSE
                RETURN ctrls^[index].r_first;
            END;
        END;
    END;
END GetUserControlId;

PROCEDURE AllocateInternalData(thisDlg : ADRCARD; key : ADDRESS; amount : CARDINAL) : ADDRESS;
BEGIN
    WITH Dialogs[thisDlg] DO
        IF numInternal = internalSize THEN
            internalSize := internalSize + 16;

            IF internalData = NIL THEN
                ALLOCATE(internalData, internalSize*SIZE(InternalDataRecord));
            ELSE
                ReALLOCATE(internalData, internalSize*SIZE(InternalDataRecord));
            END;
        END;
        INC(numInternal);
        internalData^[numInternal].key := key;
        internalData^[numInternal].amount := amount;
        ALLOCATE(internalData^[numInternal].data, amount);
        RETURN internalData^[numInternal].data;
    END;
END AllocateInternalData;

PROCEDURE FreeAllInternalData(thisDlg : ADRCARD);
VAR
    i   : ADRCARD;
BEGIN
    WITH Dialogs[thisDlg] DO
        <*/PUSH/NOWARN:U*>
        FOR i := 1 TO VAL(ADRCARD, numInternal) DO
        <*/POP*>
            DEALLOCATE(internalData^[i].data, internalData^[i].amount);
        END;
        IF internalData <> NIL THEN
            DEALLOCATE(internalData, internalSize*SIZE(InternalDataRecord));
        END;
    END;
END FreeAllInternalData;

PROCEDURE FindInternalData(thisDlg : ADRCARD; key : ADDRESS) : ADDRESS;
VAR
    i   : ADRCARD;
BEGIN
    WITH Dialogs[thisDlg] DO
        <*/PUSH/NOWARN:U*>
        FOR i := 1 TO VAL(ADRCARD, numInternal) DO
        <*/POP*>
            IF internalData^[i].key = key THEN
                RETURN internalData^[i].data;
            END;
        END;
    END;
    RETURN NIL;
END FindInternalData;

PROCEDURE FindDlg(wnd : HWND; lastDialog : ADRCARD) : BOOLEAN;
VAR
    i   : ADRCARD;
BEGIN
    IF (lastDialog <> 0) AND (Dialogs[lastDialog].wnd = wnd) THEN
        RETURN TRUE;
    END;
    FOR i := 1 TO MaxDlgs DO
        IF Dialogs[i].wnd = wnd THEN
            SetLastDialog(i);
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END FindDlg;

PROCEDURE FindDlgPage(wnd : HWND; lastDialog : ADRCARD) : BOOLEAN;
VAR
    i, j        : ADRCARD;
BEGIN
    IF (lastDialog <> 0) AND (Dialogs[lastDialog].wnd = wnd) THEN
        RETURN TRUE;
    END;
    FOR i := 1 TO MaxDlgs DO
        IF Dialogs[i].propertySheet THEN
            <*/PUSH/NOWARN:U*>
            FOR j := 0 TO VAL(ADRCARD, Dialogs[i].numPages) DO
            <*/POP*>
                IF Dialogs[i].pageWnd^[j] = wnd THEN
                    Dialogs[i].pageModified^[Dialogs[i].activePage] := Dialogs[i].modified;

                    Dialogs[i].activePage := j;
                    Dialogs[i].wnd := Dialogs[i].pageWnd^[j];
                    Dialogs[i].modified := Dialogs[i].pageModified^[j];
                    Dialogs[i].ctrls := Dialogs[i].pages^[j].controls;
                    Dialogs[i].numCtrls := Dialogs[i].pages^[j].numControls;
                    Dialogs[i].notify := Dialogs[i].pages^[j].notify;
                    SetLastDialog(i);
                    RETURN TRUE;
                END;
            END;
        END;
    END;
    RETURN FALSE;
END FindDlgPage;

PROCEDURE AllocateDialog() : CARDINAL;
VAR
    i   : ADRCARD;
BEGIN
    EnterCriticalSection(Critic);

    i := 1;
    WHILE (i <= MaxDlgs) AND (NOT Dialogs[i].free) DO
        INC(i);
    END;
    IF i <= MaxDlgs THEN
        Dialogs[i].free := FALSE;
    ELSE
        i := MAX(CARDINAL);
    END;

    LeaveCriticalSection(Critic);
    RETURN i;
END AllocateDialog;

PROCEDURE DeallocateDialog(thisDlg : ADRCARD);
BEGIN
    EnterCriticalSection(Critic);
    IF NOT Dialogs[thisDlg].free THEN
        Dialogs[thisDlg] := NullDialog;
    END;
    LeaveCriticalSection(Critic);
END DeallocateDialog;

TYPE
    <*/PUSH/PACK/NOWARN:A*>
    DIALOGBOXHEADER = RECORD
          lStyle         : DWORD;
          lExtendedStyle : DWORD;
          NumberOfItems  : CARDINAL16;
          x              : INTEGER16;
          y              : INTEGER16;
          cx             : CARDINAL16;
          cy             : CARDINAL16;
    END;
    <*/POP*>

    PDIALOGBOXHEADEREX = POINTER TO DIALOGBOXHEADEREX;
    <*/PUSH/PACK/NOWARN:A*>
    DIALOGBOXHEADEREX = RECORD
          version        : CARDINAL16;(* 1 *)
          signature      : CARDINAL16;(* 0FFFFh *)
          helpId         : CARDINAL32;
          lExtendedStyle : CARDINAL32;
          lStyle         : CARDINAL32;
          NumberOfItems  : CARDINAL16;
          x              : INTEGER16;
          y              : INTEGER16;
          cx             : CARDINAL16;
          cy             : CARDINAL16;
    END;
    <*/POP*>

PROCEDURE LoadAlterDialog(name : ARRAY OF CHAR; VAR OUT amount : CARDINAL) : LPDLGTEMPLATE;
TYPE
    tPtrW       = POINTER TO CARDINAL16;
    tPtrDW      = POINTER TO CARDINAL32;

    PROCEDURE skipName(VAR INOUT ptrW : tPtrW);
    BEGIN
        IF ptrW^ = 0FFFFh THEN
            ptrW := ADDADR(ptrW, 2+2);
        ELSE
            WHILE ptrW^ <> 0 DO
                ptrW := ADDADR(ptrW, 2);
            END;
            ptrW := ADDADR(ptrW, 2);
        END;
    END skipName;

VAR
    hresFind    : HRSRC;
    hres        : HRSRC;
    lpDlgEx     : PDIALOGBOXHEADEREX;
    data        : ADDRESS;
    style       : CARDINAL;
    ptrW        : tPtrW;
    ext         : BOOLEAN;
    resName     : ARRAY [0..63] OF CHAR;
    resId       : CARDINAL;
    lpstr       : LPTSTR;
BEGIN
    GetResNameAndId(name, resName, resId);
    hresFind := NIL;
    IF resName[0] <> '' THEN
        hresFind := FindResource(ResourceInst, resName, RT_DIALOG^);
    END;
    IF resId <> 0 THEN
        lpstr := MAKEINTRESOURCE(resId);
        hresFind := FindResource(ResourceInst, lpstr^, RT_DIALOG^);
    END;
    IF hresFind = NIL THEN
        RETURN NIL;
    END;

    hres := LoadResource(ResourceInst, hresFind);
    IF hres = NIL THEN
        RETURN NIL;
    END;

    amount := SizeofResource(ResourceInst, hresFind);
    lpDlgEx := CAST(PDIALOGBOXHEADEREX, LockResource(hres));

    ALLOCATE(data, amount);

    MoveMem(data, lpDlgEx, amount);

    FreeResource(hres);

    lpDlgEx := data;

    (* check for an extended or regular dialog resource *)

    IF lpDlgEx^.signature = 0FFFFh THEN
        ext := TRUE;
        IF lpDlgEx^.version = 1 THEN
            ptrW := ADDADR(lpDlgEx, SIZE(DIALOGBOXHEADEREX));
            style := lpDlgEx^.lStyle;
        ELSE
            RETURN data;
        END;
    ELSE
        ext := FALSE;
        ptrW := ADDADR(lpDlgEx, SIZE(DIALOGBOXHEADER));
        style := CAST(tPtrDW, lpDlgEx)^;
    END;

    skipName(ptrW); (* menu name *)
    skipName(ptrW); (* class name *)
    skipName(ptrW); (* caption name *)

    IF ((style BAND DS_SETFONT) = DS_SETFONT) OR
       ((style BAND DS_SHELLFONT) = DS_SHELLFONT)
    THEN
        CASE DialogSize OF
        NormalDialog:
            (* use the resource size *)
        |
        LargeDialog:
            ptrW^ := ptrW^ + 2;
        |
        ExtraLargeDialog:
            ptrW^ := ptrW^ + 4;
        END;
    END;
    RETURN data;
END LoadAlterDialog;

PROCEDURE DisposeDialogTemplate(VAR INOUT dlg : LPDLGTEMPLATE; amount : CARDINAL);
BEGIN
    DEALLOCATE(dlg, amount);
END DisposeDialogTemplate;

PROCEDURE CreateDialogFont(name : ARRAY OF CHAR) : HFONT;
TYPE
    tPtrW       = POINTER TO CARDINAL16;
    tPtrDW      = POINTER TO CARDINAL32;

    PROCEDURE skipName(VAR INOUT ptrW : tPtrW);
    BEGIN
        IF ptrW^ = 0FFFFh THEN
            ptrW := ADDADR(ptrW, 2+2);
        ELSE
            WHILE ptrW^ <> 0 DO
                ptrW := ADDADR(ptrW, 2);
            END;
            ptrW := ADDADR(ptrW, 2);
        END;
    END skipName;

VAR
    hresFind    : HRSRC;
    hres        : HRSRC;
    lpDlgEx     : PDIALOGBOXHEADEREX;
    style       : CARDINAL;
    ptrW        : tPtrW;
    i           : ADRCARD;
    ext         : BOOLEAN;
    lf          : LOGFONT;
    dc          : HDC;
    pixels      : INTEGER;
BEGIN
    hresFind := FindResource(ResourceInst, name,  RT_DIALOG^);
    IF hresFind = NIL THEN
        RETURN NULL_HFONT;
    END;

    hres := LoadResource(ResourceInst, hresFind);
    IF hres = NIL THEN
        RETURN NULL_HFONT;
    END;

    lpDlgEx := CAST(PDIALOGBOXHEADEREX, LockResource(hres));

    (* check for an extended or regular dialog resource *)

    IF lpDlgEx^.signature = 0FFFFh THEN
        ext := TRUE;
        IF lpDlgEx^.version = 1 THEN
            ptrW := ADDADR(lpDlgEx, SIZE(DIALOGBOXHEADEREX));
            style := lpDlgEx^.lStyle;
        ELSE
            FreeResource(hres);
            RETURN NULL_HFONT;
        END;
    ELSE
        ext := FALSE;
        ptrW := ADDADR(lpDlgEx, SIZE(DIALOGBOXHEADER));
        style := CAST(tPtrDW, lpDlgEx)^;
    END;

    skipName(ptrW); (* menu name *)
    skipName(ptrW); (* class name *)
    skipName(ptrW); (* caption name *)

    IF (style BAND DS_SETFONT) <> 0 THEN
        CASE DialogSize OF
        NormalDialog:
            lf.lfHeight := ptrW^;
        |
        LargeDialog:
            lf.lfHeight := ptrW^ + 2;
        |
        ExtraLargeDialog:
            lf.lfHeight := ptrW^ + 4;
        END;
        ptrW := ADDADR(ptrW, 2);

        lf.lfWeight := FW_NORMAL;
        lf.lfItalic := FALSE;

        IF ext THEN
            lf.lfWeight := ptrW^;
            ptrW := ADDADR(ptrW, 2);
            lf.lfItalic := ptrW^ <> 0;
            ptrW := ADDADR(ptrW, 2);
        END;

        i := 0;
        WHILE ptrW^ <> 0 DO
            IF i < HIGH(lf.lfFaceName) THEN
                lf.lfFaceName[i] := CHR(ptrW^);
                INC(i);
            END;
            ptrW := ADDADR(ptrW, 2);
        END;
        lf.lfFaceName[i] :=  '';

        dc := GetDC(NULL_HWND);
        pixels := GetDeviceCaps(dc, LOGPIXELSY);
        ReleaseDC(NULL_HWND, dc);

        pixels := pixels * lf.lfHeight;
        lf.lfHeight := -(pixels / 72);
        IF (pixels REM 72) > 36 THEN
            DEC(lf.lfHeight);
        END;

        lf.lfWidth := 0;
        lf.lfEscapement := 0;
        lf.lfOrientation := 0;
        lf.lfUnderline := FALSE;
        lf.lfStrikeOut := FALSE;
        lf.lfCharSet := ANSI_CHARSET;
        lf.lfOutPrecision := OUT_DEFAULT_PRECIS;
        lf.lfClipPrecision := CLIP_DEFAULT_PRECIS;
        lf.lfQuality := PROOF_QUALITY;
        lf.lfPitchAndFamily := FF_DONTCARE;

        FreeResource(hres);

        RETURN CreateFontIndirect(lf);
    END;

    FreeResource(hres);
    RETURN NULL_HFONT;
END CreateDialogFont;

PROCEDURE CallDialog(parent : Window;
                     name : ARRAY OF CHAR;
                     VAR INOUT ctrls : ARRAY OF CONTROL;
                     notify : NotifyProc;
                     position : DialogPositions) : CARDINAL;
VAR
    res         : CARDINAL;
    i           : ADRCARD;
    wnd         : HWND;
    lpDlg       : LPDLGTEMPLATE;
    amount      : CARDINAL;
BEGIN
    GetResFile;

    res := MAX(CARDINAL);
    i := AllocateDialog();
    IF i <> MAX(CARDINAL) THEN
        Dialogs[i].numCtrls := HIGH(ctrls) + 1;
        Dialogs[i].ctrls := ADR(ctrls);
        Dialogs[i].notify := notify;
        Dialogs[i].position := position;

        IF parent = NIL THEN
            wnd := NULL_HWND;
        ELSIF parent = CurrentAsParent THEN
            wnd := Dialogs[GetLastDialog()].wnd;
        ELSE
            WHILE GetWindowType(parent) = ChildWindow DO
                parent := GetWindowParent(parent);
            END;

            wnd := GetWindowHandle(parent);
        END;
        Dialogs[i].parent := parent;


        lpDlg := LoadAlterDialog(name, amount);
        IF lpDlg <> NIL THEN
            res := DialogBoxIndirectParam(Instance, lpDlg, wnd, MyDlgProc, VAL(LPARAM, i));
            DisposeDialogTemplate(lpDlg, amount);

            IF res = MinusOne THEN
                DlgError := GetLastError();
                res := MAX(CARDINAL);
            END;
        ELSE
            DlgError := GetLastError();
            res := MAX(CARDINAL);
        END;

        DeallocateDialog(i);
    END;
    RETURN res;
END CallDialog;

PROCEDURE ModelessDialog(parent : Window;
                         name : ARRAY OF CHAR;
                         VAR INOUT ctrls : ARRAY OF CONTROL;
                         notify : NotifyProc;
                         position : DialogPositions) : CARDINAL;
VAR
    i           : ADRCARD;
    wnd         : HWND;
    lpDlg       : LPDLGTEMPLATE;
    amount      : CARDINAL;
BEGIN
    GetResFile;

    i := AllocateDialog();
    IF i <> MAX(CARDINAL) THEN
        Dialogs[i].numCtrls := HIGH(ctrls) + 1;
        Dialogs[i].ctrls := ADR(ctrls);
        Dialogs[i].notify := notify;
        Dialogs[i].modeless := TRUE;
        Dialogs[i].position := position;
        Dialogs[i].wnd := NULL_HWND;

        IF parent = NIL THEN
            wnd := NULL_HWND;
        ELSIF parent = CurrentAsParent THEN
            wnd := Dialogs[GetLastDialog()].wnd;
        ELSE
            WHILE GetWindowType(parent) = ChildWindow DO
                parent := GetWindowParent(parent);
            END;

            wnd := GetWindowHandle(parent);
        END;
        Dialogs[i].parent := parent;

        lpDlg := LoadAlterDialog(name, amount);
        IF lpDlg <> NIL THEN
            Dialogs[i].wnd := CreateDialogIndirectParam(Instance, lpDlg, wnd, MyDlgProc, VAL(LPARAM, i));
            DisposeDialogTemplate(lpDlg, amount);
        ELSE
            i := MAX(CARDINAL);
        END;

        IF Dialogs[i].wnd = NULL_HWND THEN
            DeallocateDialog(i);
            i := MAX(CARDINAL);
        END;
    END;
    RETURN i;
END ModelessDialog;

PROCEDURE CallTabbedDialog(parent : Window;
                           title : ARRAY OF CHAR;
                           VAR INOUT pages : ARRAY OF PAGE;
                           position : DialogPositions) : CARDINAL;
VAR
    res         : CARDINAL;
    i, j        : ADRCARD;
    highPages   : ADRCARD;
    wnd         : HWND;
    header      : PROPSHEETHEADER;
    numPages    : CARDINAL;
    ltitle      : ARRAY [0..79] OF CHAR;
    pspPages,
    pspPtr      : LPPROPSHEETPAGE;
    amounts     : POINTER TO ARRAY [0..0] OF CARDINAL;
    ok          : BOOLEAN;
BEGIN
    GetResFile;

    res := MAX(CARDINAL);
    i := AllocateDialog();
    IF i <> MAX(CARDINAL) THEN
        numPages := HIGH(pages)+1;

        Dialogs[i].pages := ADR(pages);
        Dialogs[i].numPages := numPages;
        Dialogs[i].propertySheet := TRUE;

        Dialogs[i].numCtrls := pages[0].numControls;
        Dialogs[i].ctrls := ADR(pages[0].controls);
        Dialogs[i].notify := pages[0].notify;

        Dialogs[i].position := position;

        IF parent = NIL THEN
            wnd := NULL_HWND;
        ELSIF parent = CurrentAsParent THEN
            wnd := Dialogs[GetLastDialog()].wnd;
        ELSE
            WHILE GetWindowType(parent) = ChildWindow DO
                parent := GetWindowParent(parent);
            END;

            wnd := GetWindowHandle(parent);
        END;
        Dialogs[i].parent := parent;

        ALLOCATE(pspPages, numPages * PROPSHEETPAGE_V1_SIZE);
        ALLOCATE(Dialogs[i].pageWnd, numPages * SIZE(HWND));
        ALLOCATE(Dialogs[i].pageModified, numPages * SIZE(BOOLEAN));
        ALLOCATE(amounts, numPages * SIZE(amounts^[0]));
        IF (pspPages <> NIL) AND
           (Dialogs[i].pageWnd <> NIL) AND
           (Dialogs[i].pageModified <> NIL) AND
           (amounts <> NIL)
        THEN
            ok := TRUE;
            pspPtr := pspPages;
            highPages := HIGH(pages);
            FOR j := 0 TO highPages DO
                Dialogs[i].pageWnd^[j] := NULL_HWND;
                Dialogs[i].pageModified^[j] := FALSE;

                pspPtr^.dwSize := PROPSHEETPAGE_V1_SIZE;
                pspPtr^.dwFlags := PSP_DLGINDIRECT;
                IF pages[j].title <> NIL THEN
                    pspPtr^.dwFlags := pspPtr^.dwFlags BOR PSP_USETITLE;
                END;
                pspPtr^.hInstance := Instance;
                pspPtr^.pszTemplate := CAST(LPTSTR, pages[j].name);
                (*pspPtr^.pszTemplate := ADR(pages[j].name);*)
                IF Dialogs[i].dlgFont = NULL_HFONT THEN
                    Dialogs[i].dlgFont := CreateDialogFont(pspPtr^.pszTemplate^);
                END;
                pspPtr^.pResource := LoadAlterDialog(pspPtr^.pszTemplate^, amounts^[j]);
                ok := ok AND (pspPtr^.pResource <> NIL);
                pspPtr^.pszIcon := NIL;
                pspPtr^.pfnDlgProc := MyPropSheetDlgProc;
                pspPtr^.pszTitle := CAST(ADDRESS, pages[j].title);
                pspPtr^.lParam := i + (j * 65536);
                pspPtr^.pfnCallback := CAST(LPFNPSPCALLBACK, NIL);
                pspPtr^.pcRefParent := NIL;

                pspPtr := ADDADR(pspPtr, PROPSHEETPAGE_V1_SIZE);
            END;

            ltitle := title;
            ltitle[HIGH(ltitle)] := '';

            header.dwSize := PROPSHEETHEADER_V1_SIZE;
            header.dwFlags := PSH_PROPSHEETPAGE BOR PSH_NOAPPLYNOW;
            header.hwndParent := wnd;
            header.hInstance := Instance;
            header.pszIcon := NIL;
            header.pszCaption := ADR(ltitle);
            header.nPages := numPages;
            header.nStartPage := 0;
            header.ppsp := pspPages;
            header.pfnCallback := CAST(PFNPROPSHEETCALLBACK, NIL);

            res := MinusOne;
            IF ok THEN
                res := PropertySheet(header);
            END;

            pspPtr := pspPages;
            FOR j := 0 TO highPages DO
                DisposeDialogTemplate(pspPtr^.pResource, amounts^[j]);
                pspPtr := ADDADR(pspPtr, PROPSHEETPAGE_V1_SIZE);
            END;

            IF res = MinusOne THEN
                res := MAX(CARDINAL);
                DlgError := GetLastError();
            ELSIF res = 0 THEN
                res := 2;
            ELSE
                res := 1;
            END;
        END;

        IF pspPages <> NIL THEN
            DEALLOCATE(pspPages, numPages * PROPSHEETPAGE_V1_SIZE);
        END;
        IF Dialogs[i].pageWnd <> NIL THEN
            DEALLOCATE(Dialogs[i].pageWnd, numPages * SIZE(HWND));
        END;
        IF Dialogs[i].pageModified <> NIL THEN
            DEALLOCATE(Dialogs[i].pageModified, numPages * SIZE(BOOLEAN));
        END;
        IF amounts <> NIL THEN
            DEALLOCATE(amounts, numPages * SIZE(amounts^[0]));
        END;

        DeallocateDialog(i);
    END;
    RETURN res;
END CallTabbedDialog;

PROCEDURE ShowDialog(dlgNum : ADRCARD; yes : BOOLEAN);
BEGIN
    IF Dialogs[dlgNum].wnd <> NULL_HWND THEN
        ShowWindow(Dialogs[dlgNum].wnd, Show[yes]);
    END;
END ShowDialog;

PROCEDURE EnableDialog(dlgNum : ADRCARD; yes : BOOLEAN);
BEGIN
    IF Dialogs[dlgNum].wnd <> NULL_HWND THEN
        EnableWindow(Dialogs[dlgNum].wnd, yes);
    END;
END EnableDialog;

PROCEDURE SetATimer(wnd : HWND; timerId, timerInterval : CARDINAL);
BEGIN
    SetTimer(wnd, timerId+TimerIdFudge, timerInterval, CAST(TIMERPROC, NULL));
END SetATimer;

PROCEDURE KillATimer(wnd : HWND; timerId : CARDINAL);
VAR
    msg         : MSG;
BEGIN
    KillTimer(wnd, timerId+TimerIdFudge);

    (* just in case any timer is pending in the input queue. *)
    (* if this is possible at all *)

    <*/PUSH/NOWARN:U*>
    IF PeekMessage(msg, wnd, WM_TIMER, WM_TIMER, PM_NOREMOVE) AND
       (msg.wParam = VAL(WPARAM, timerId+TimerIdFudge))
    THEN
    <*/POP*>
        PeekMessage(msg, wnd, WM_TIMER, WM_TIMER, PM_REMOVE);
    END;
END KillATimer;

PROCEDURE DialogCleanup(thisDlg : ADRCARD);
VAR
    i           : ADRCARD;
    stIntPtr    : StaticTextInternalPointer;
    matchInfo   : MatchListInfoPointer;
BEGIN
    WITH Dialogs[thisDlg] DO
        IF wnd <> NULL_HWND THEN
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, numCtrls)-1 DO
            <*/POP*>
                CASE ctrls^[i].ct OF
                Timer:
                    IF ctrls^[i].timerInterval <> 0 THEN
                        KillATimer(wnd, ctrls^[i].id);
                        ctrls^[i].timerInterval := 0;
                    END;
                |
                StaticTextAttrib:
                    stIntPtr := FindInternalData(thisDlg, ADR(ctrls^[i]));
                    IF stIntPtr <> NIL THEN
                        IF stIntPtr^.brush <> NULL_HBRUSH THEN
                            DeleteBrush(stIntPtr^.brush);
                            stIntPtr^.brush := NULL_HBRUSH;
                        END;
                        IF stIntPtr^.font <> NULL_HFONT THEN
                            DeleteFont(stIntPtr^.font);
                            stIntPtr^.font := NULL_HFONT;
                        END;
                    END;
                |
                ListBox, ColumnListBox:
                    matchInfo := FindInternalData(thisDlg, GetDlgItem(wnd, ctrls^[i].id));
                    IF matchInfo <> NIL THEN
                        FUNC SubclassWindow(matchInfo^.wnd, matchInfo^.oldWndProc);
                    END;
                ELSE
                END;
            END;

            IF toolTip <> NIL THEN
                DestroyWindow(toolTip);
                toolTip := NIL;
            END;
        END;

        IF dlgFont <> NULL_HFONT THEN
            DeleteFont(dlgFont);
        END;

        FreeAllInternalData(thisDlg);
    END;
END DialogCleanup;

PROCEDURE SaveTextFiles(thisDlg : ADRCARD);
VAR
    i           : ADRCARD;
BEGIN
    WITH Dialogs[thisDlg] DO
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, numCtrls)-1 DO
        <*/POP*>
            CASE ctrls^[i].ct OF
            EditTextFile:
                IF modified AND ctrls^[i].etf_altered THEN
                    SaveFile(thisDlg, i, ctrls^[i].etf_name);
                END;
            ELSE
            END;
        END;
    END;
END SaveTextFiles;

PROCEDURE DoCloseDialog(dlgNum : ADRCARD; closeType : NotifyResult; id : CARDINAL);
VAR
    nItem       : CARDINAL;
BEGIN
    Dialogs[dlgNum].closeWasCancel := closeType = CancelDialog;

    IF NOT Dialogs[dlgNum].free THEN
        IF NOT Dialogs[dlgNum].closeWasCancel THEN
            IF NOT ValidateFields(dlgNum) THEN
                RETURN;
            END;

            IF Dialogs[dlgNum].pages = NIL THEN
                IF Dialogs[dlgNum].modified THEN
                    nItem := MAX(CARDINAL);
                    CallNotify(dlgNum, DialogModified, nItem);
                END;
            END;

            SaveTextFiles(dlgNum);
        END;

        IF Dialogs[dlgNum].modeless THEN
            IF IsWindow(Dialogs[dlgNum].wnd) THEN
                DestroyWindow(Dialogs[dlgNum].wnd);
            END;
        ELSE
            IF IsWindow(Dialogs[dlgNum].wnd) THEN
                EndDialog(Dialogs[dlgNum].wnd, id);
            END;
        END;
    END;
END DoCloseDialog;

PROCEDURE CloseDialog(dlgNum : ADRCARD; cancel : BOOLEAN);
VAR
    nRes        : NotifyResult;
BEGIN
    nRes := TerminateDialog;
    IF cancel THEN
        nRes := CancelDialog;
    END;
    DoCloseDialog(dlgNum, nRes, MAX(CARDINAL));
END CloseDialog;

PROCEDURE GetDialogNumber() : CARDINAL;
BEGIN
    RETURN GetLastDialog();
END GetDialogNumber;

PROCEDURE GetActivePage() : CARDINAL;
VAR
    thisDlg     : ADRCARD;
BEGIN
    thisDlg := GetLastDialog();
    IF (thisDlg > 0) AND (thisDlg <= MaxDlgs) THEN
        RETURN Dialogs[thisDlg].activePage;
    END;
    RETURN 0;
END GetActivePage;

PROCEDURE SetActivePage(page : CARDINAL);
VAR
    thisDlg     : ADRCARD;
BEGIN
    thisDlg := GetLastDialog();
    IF (thisDlg > 0) AND
       (page <> Dialogs[thisDlg].activePage) AND
       (page < Dialogs[thisDlg].numPages)
    THEN
        PostMessage(GetParent(Dialogs[thisDlg].wnd), PSM_SETCURSEL, page, NULL);
    END;
END SetActivePage;

PROCEDURE GetUserDataDlg(dlgNum : ADRCARD) : ADDRESS;
VAR
    i   : ADRCARD;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, numCtrls)-1 DO
            <*/POP*>
                IF ctrls^[i].ct = UserData THEN
                    RETURN ctrls^[i].userData;
                END;
            END;
        END;
    END;
    RETURN NIL;
END GetUserDataDlg;

PROCEDURE GetUserData() : ADDRESS;
BEGIN
    RETURN GetUserDataDlg(GetLastDialog());
END GetUserData;

PROCEDURE LoadFile(thisDlg, ctrl : CARDINAL; name : ADDRESS);
VAR
    fileName    : FileSpecString;
    ptrC        : POINTER TO CHAR;
    i           : ADRCARD;
    f           : File;
    ch          : CHAR;
    buffer      : ARRAY [0..(1*1024)-1] OF CHAR;
    line        : ARRAY [0..255] OF CHAR;
BEGIN
    i := 0;
    ptrC := name;
    WHILE (i < HIGH(fileName)) AND (ptrC^ <> '') DO
        fileName[i] := ptrC^;
        ptrC := ADDADR(ptrC, SIZE(CHAR));
        INC(i);
    END;
    fileName[i] := '';

    OpenFile(f, fileName, ReadWriteDenyAll);
    IF f.status = 0 THEN
        SetFileBuffer(f, buffer);

        REPEAT
            i := 0;
            LOOP
                ch := ReadChar(f);
                IF (NOT f.eof) AND (f.status = 0) THEN
                    IF ch <> EOL THEN
                        IF i < HIGH(line) THEN
                            line[i] := ch;
                            INC(i);
                        END;
                    ELSE
                        EXIT;
                    END;
                ELSE
                    EXIT;
                END;
            END;

            IF (i <> 0) OR (NOT f.eof) THEN
                line[i] := '';
                MleAppendLineDlg(thisDlg, ctrl, line);
            END;

        UNTIL f.eof OR (f.status <> 0);

        CloseFile(f);
    END;
END LoadFile;

PROCEDURE SaveFile(thisDlg, ctrl : CARDINAL; name : ADDRESS);
VAR
    fileName    : FileSpecString;
    ptrC        : POINTER TO CHAR;
    i           : ADRCARD;
    f           : File;
    buffer      : ARRAY [0..(1*1024)-1] OF CHAR;
    line        : ARRAY [0..255] OF CHAR;
    lineCount   : ADRCARD;
BEGIN
    i := 0;
    ptrC := name;
    WHILE (i < HIGH(fileName)) AND (ptrC^ <> '') DO
        fileName[i] := ptrC^;
        ptrC := ADDADR(ptrC, SIZE(CHAR));
        INC(i);
    END;
    fileName[i] := '';

    CreateFile(f, fileName);
    IF f.status = 0 THEN
        SetFileBuffer(f, buffer);

        lineCount := MleGetLineCountDlg(thisDlg, ctrl);
        FOR i := 1 TO lineCount DO
            MleGetLineDlg(thisDlg, ctrl, i, line);
            IF (i <> lineCount) OR (line[0] <> '') THEN
                IF line[0] <> '' THEN
                    WriteBlock(f, ADR(line), LENGTH(line));
                END;
                WriteChar(f, EOL);
            END;
        END;

        CloseFile(f);
    END;
END SaveFile;

PROCEDURE DoExcept(parent : HWND);
CONST
    hexDig      : ARRAY OF CHAR = {'0123456789ABCDEF'};
VAR
    str         : ARRAY [0..255] OF CHAR;
    mess        : ARRAY [0..63] OF CHAR;
    hexStr      : ARRAY [0..7] OF CHAR;
    num         : CARDINAL;
    i           : ADRCARD;
BEGIN
    OutputCallTrace;

    GetMessage(mess);

    num := CAST(CARDINAL, EXCEPTADR());
    FOR i := 7 TO 0 BY -1 DO
        hexStr[i] := hexDig[num REM 16];
        num := num / 16;
    END;

    Concat("Exception Trapped in DlgShell at ", hexStr, str);
    Append(CrLf, str);
    Append(mess, str);

    IF NOT IsWindow(parent) THEN
        parent := NULL_HWND;
    END;

    MessageBeep(MB_ICONSTOP);
    MessageBox(parent,
               str,
               "Program Error",
               MB_OK BOR
               MB_ICONSTOP BOR
               MB_SETFOREGROUND BOR
               MB_TOPMOST BOR
               MB_TASKMODAL);
END DoExcept;

PROCEDURE RadioSelect(hdlg : HWND; VAR INOUT c : CONTROL; id : CARDINAL);
BEGIN
    CheckRadioButton(hdlg, c.r_first, c.r_last, id);
    c.id := id - c.r_first;
END RadioSelect;

PROCEDURE CallNotify(thisDlg : ADRCARD;
                     typ : NotifyType;
                     VAR INOUT nItem : CARDINAL) : NotifyResult;
VAR
    nRes        : NotifyResult;
BEGIN
    nRes := ContinueDialog;
    WITH Dialogs[thisDlg] DO
        IF (notify <> NIL_NOTIFY) AND (NOT suppressNotify) THEN
            nItem := GetUserControlId(thisDlg, nItem);
            nRes := notify(typ, nItem);

            (* just in case the user called a dialog *)
            (* from the notification procedure *)
            SetLastDialog(thisDlg);

            IF (nRes = TerminateDialog) OR (nRes = CancelDialog) THEN
                IF (typ <> DestroyDialog) AND (typ <> InitDialog) THEN
                    DoCloseDialog(thisDlg, nRes, nItem);
                END;
            END;
        END;
    END;
    RETURN nRes;
END CallNotify;

PROCEDURE InitToolTips(thisDlg : ADRCARD);
VAR
    ti  : TOOLINFO;
BEGIN
    WITH Dialogs[thisDlg] DO
        IF toolTip = NIL THEN
            toolTip := CreateWindowEx(WS_EX_TOPMOST,
                                      TOOLTIPS_CLASS,
                                      NIL_STR,
                                      WS_POPUP BOR TTS_NOPREFIX,
                                      CW_USEDEFAULT, CW_USEDEFAULT,
                                      CW_USEDEFAULT, CW_USEDEFAULT,
                                      NIL,(*parent*)
                                      NIL,(*hmenu*)
                                      Instance,
                                      NIL(*lparam*));

            oldTipWndProc := SubclassWindow(toolTip, ToolTipSubclassProc);
        END;

        IF toolTip <> NIL THEN
            (* Prepare TOOLINFO structure for use as tracking tooltip. *)

            ti.cbSize := SIZE(TOOLINFO);
            ti.uFlags := TTF_TRACK;
            ti.hwnd := wnd;
            ti.uId := ToolTipID + activePage;
            ti.hinst := Instance;
            ti.lpszText  := LPSTR_TEXTCALLBACK;
            ti.rect.left := 0;
            ti.rect.top := 0;
            ti.rect.bottom := 0;
            ti.rect.right := 0;

            (* Add the tool to the control *)

            IF SendMessage(toolTip, TTM_ADDTOOL, 0, CAST(LPARAM, ADR(ti))) = 0 THEN
                DestroyWindow(toolTip);
                toolTip := NIL;
            END;
        END;
    END;
END InitToolTips;

PROCEDURE HandleDialogInit(thisDlg : ADRCARD) : BOOLEAN;
VAR
    nRes        : NotifyResult;
    ctrlWnd     : HWND;
    i           : ADRCARD;
    j           : ADRCARD;
    k           : CARDINAL;
    sel         : INTEGER;
    ret         : INTEGER;
    idx         : CARDINAL;
    nItem       : CARDINAL;
    save        : BOOLEAN;
    stIntPtr    : StaticTextInternalPointer;
    numStr      : ARRAY [0..11] OF CHAR;
    lvColumn    : LVCOLUMN;
BEGIN
    WITH Dialogs[thisDlg] DO
        save := suppressNotify;
        suppressNotify := TRUE;

        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, numCtrls)-1 DO
        <*/POP*>
            ctrlWnd := GetDlgItem(wnd, ctrls^[i].id);

            IF ctrls^[i].helpId <> 0 THEN
                InitToolTips(thisDlg);
            END;

            CASE ctrls^[i].ct OF
            CheckBox:
                Button_SetCheck(ctrlWnd, ORD(ctrls^[i].chk_on));
            |
            RadioGroup:
                RadioSelect(wnd, ctrls^[i], ctrls^[i].r_first+ctrls^[i].id);
            |
            LineEdit:
                Edit_LimitText(ctrlWnd, ctrls^[i].le_max);
                IF ctrls^[i].le_text <> NIL THEN
                    Edit_SetText(ctrlWnd, ctrls^[i].le_text^);
                END;
            |
            MultiLineEdit:
                Edit_LimitText(ctrlWnd, ctrls^[i].mle_max);
                IF ctrls^[i].mle_text <> NIL THEN
                    Edit_SetText(ctrlWnd, ctrls^[i].mle_text^);
                END;
            |
            EditTextFile:
                Edit_LimitText(ctrlWnd, (64*1024)-1);
                ctrls^[i].etf_altered := FALSE;
                LoadFile(thisDlg, i, ctrls^[i].etf_name);
            |
            NumberEdit:
                Edit_LimitText(ctrlWnd, 12);
                IF ctrls^[i].number_Hex THEN
                    CardBaseToStr(ctrls^[i].number_Val, 16, numStr);
                    AppendChar('h', numStr);
                ELSE
                    IntToStr(ctrls^[i].number_Val, numStr);
                END;
                Edit_SetText(ctrlWnd, numStr);
            |
            SpinButton:
                UpDown_SetRange32(ctrlWnd, ctrls^[i].spin_Min, ctrls^[i].spin_Max);
                ctrlWnd := UpDown_GetBuddy(ctrlWnd);
                Edit_LimitText(ctrlWnd, 12);
                IntToStr(ctrls^[i].spin_Val, numStr);
                Edit_SetText(ctrlWnd, numStr);
            |
            StaticText:
                IF ctrls^[i].st_text <> NIL THEN
                    Static_SetText(ctrlWnd, ctrls^[i].st_text^);
                END;
            |
            StaticTextAttrib:
                stIntPtr := AllocateInternalData(thisDlg, ADR(ctrls^[i]), SIZE(StaticTextInternalRec));
                stIntPtr^.brush := NULL_HBRUSH;
                stIntPtr^.font := NULL_HFONT;

                IF ctrls^[i].sta_text <> NIL THEN
                    Static_SetText(ctrlWnd, ctrls^[i].sta_text^);
                END;

                IF ctrls^[i].sta_font <> NIL THEN
                    SetColorAndFontDlg(thisDlg, i, ctrls^[i].sta_fore, ctrls^[i].sta_back, ctrls^[i].sta_font);
                END;

            |
            ListBox:
                IF ctrls^[i].lb_text <> NIL THEN
                    IF ctrls^[i].lb_text^[0] <> '' THEN
                        j := 0;
                        REPEAT
                            FUNC ListBox_AddString(ctrlWnd, ctrls^[i].lb_text^[j], idx);

                            WHILE ctrls^[i].lb_text^[j] <> '' DO
                                INC(j);
                            END;
                            INC(j);
                        UNTIL ctrls^[i].lb_text^[j] = '';
                    END;

                    IF (ctrls^[i].lb_selStr <> NIL) AND (ctrls^[i].lb_selStr^[0] <> '') THEN
                        ret := ListBox_SelectString(ctrlWnd, -1, ctrls^[i].lb_selStr^);
                        IF ret = LB_ERR THEN
                            ctrls^[i].lb_selStr^[0] := '';
                        ELSE
                            ListBox_SetCurSel(ctrlWnd, ret);
                        END;
                    ELSE
                        ListBox_SetCurSel(ctrlWnd, ctrls^[i].lb_selIndex);
                    END;

                    ret := ListBox_GetCurSel(ctrlWnd);
                    IF ret <> LB_ERR THEN
                        ctrls^[i].lb_selIndex := ret;
                    ELSE
                        ctrls^[i].lb_selIndex := MAX(CARDINAL);
                    END;
                END;
            |
            ColumnListBox:
                (* setup the columns *)

                ListView_SetExtendedListViewStyle(ctrlWnd,
                                                  LVS_EX_FULLROWSELECT);

                IF ctrls^[i].clb_columnInfo <> NIL THEN
                    <*/PUSH/NOWARN:U*>
                    FOR j := 0 TO VAL(ADRCARD, ctrls^[i].clb_columns)-1 DO
                    <*/POP*>
                        WITH ctrls^[i].clb_columnInfo^[j] DO
                            lvColumn.mask := LVCF_FMT BOR LVCF_SUBITEM;
                            lvColumn.iSubItem := j;

                            CASE align OF
                            AlignLeft:
                                lvColumn.fmt := LVCFMT_LEFT;
                            |
                            AlignCenter:
                                lvColumn.fmt := LVCFMT_CENTER;
                            |
                            AlignRight:
                                lvColumn.fmt := LVCFMT_RIGHT;
                            END;

                            IF width <> 0 THEN
                                lvColumn.mask := lvColumn.mask BOR LVCF_WIDTH;
                                lvColumn.cx := width;
                            END;
                            IF header <> NIL THEN
                                lvColumn.mask := lvColumn.mask BOR LVCF_TEXT;
                                lvColumn.pszText := CAST(LPTSTR, header);
                            END;
                        END;

                        ListView_InsertColumn(ctrlWnd, j, lvColumn);
                    END;
                END;
            |
            DropDownList:
                ComboBox_SetExtendedUI(ctrlWnd, 1);

                IF (ctrls^[i].ddl_text <> NIL) AND (ctrls^[i].ddl_text^[0] <> '') THEN
                    k := 0;
                    j := 0;
                    REPEAT
                        sel := ComboBox_AddString(ctrlWnd, ctrls^[i].ddl_text^[j]);
                        IF sel <> CB_ERR THEN
                            ComboBox_SetItemData(ctrlWnd, sel, k);
                        END;
                        INC(k);

                        WHILE ctrls^[i].ddl_text^[j] <> '' DO
                            INC(j);
                        END;
                        INC(j);
                    UNTIL ctrls^[i].ddl_text^[j] = '';
                END;

                ret := ctrls^[i].ddl_selIndex;
                IF ret <> CB_ERR THEN
                    ComboBox_SetCurSel(ctrlWnd, ret);
                END;
            |
            ComboBox:
                ComboBox_SetExtendedUI(ctrlWnd, 1);
                ComboBox_LimitText(ctrlWnd, ctrls^[i].cb_max);

                IF (ctrls^[i].cb_text <> NIL) AND (ctrls^[i].cb_text^[0] <> '') THEN
                    k := 0;
                    j := 0;
                    REPEAT
                        sel := ComboBox_AddString(ctrlWnd, ctrls^[i].cb_text^[j]);
                        IF sel <> CB_ERR THEN
                            ComboBox_SetItemData(ctrlWnd, sel, k);
                        END;
                        INC(k);

                        WHILE ctrls^[i].cb_text^[j] <> '' DO
                            INC(j);
                        END;
                        INC(j);
                    UNTIL ctrls^[i].cb_text^[j] = '';
                END;

                IF ctrls^[i].cb_selStr <> NIL THEN
                    IF ctrls^[i].cb_selStr^[0] <> '' THEN
                        IF ComboBox_FindString(ctrlWnd, -1, ctrls^[i].cb_selStr^, idx) THEN
                            ComboBox_SetCurSel(ctrlWnd, idx);
                        END;
                    END;

                    ComboBox_SetText(ctrlWnd, ctrls^[i].cb_selStr^);
                END;
            |
            ColorDropDownList:
                ComboBox_SetExtendedUI(ctrlWnd, 1);

                <*/PUSH/NOWARN:U*>
                FOR j := 1 TO VAL(ADRCARD, ctrls^[i].cddl_colorCount) DO
                <*/POP*>
                    ComboBox_AddItemData(ctrlWnd, j-1);
                END;

                ComboBox_SetCurSel(ctrlWnd, ctrls^[i].cddl_selIndex);
            |
            DisplayBitmap:
                IF (ctrlWnd <> NIL) AND (ctrls^[i].db_bitmap <> NIL) THEN
                    Static_SetBitmap(ctrlWnd, CAST(HBITMAP, ctrls^[i].db_bitmap));
                END;
            |
            DefaultButton:
                SetDefaultButtonDlg(thisDlg, ctrls^[i].default_button);
            |
            Timer:
                IF ctrls^[i].timerInterval <> 0 THEN
                    SetATimer(wnd, ctrls^[i].id, ctrls^[i].timerInterval);
                END;
            |
            DialogTitle:
                IF ctrls^[i].dlgTitle <> NIL THEN
                    SetWindowText(wnd, ctrls^[i].dlgTitle^);
                END;
            ELSE
            END;
        END;

        Dialogs[thisDlg].modified := FALSE;
        suppressNotify := save;

        IF notify <> NIL_NOTIFY THEN
            save := suppressNotify;
            suppressNotify := TRUE;

            nItem := MAX(CARDINAL);
            nRes := Dialogs[thisDlg].notify(InitDialog, nItem);

            suppressNotify := save;

            SetLastDialog(thisDlg);

            IF (nRes = TerminateDialog) OR (nRes = CancelDialog) THEN
                DoCloseDialog(thisDlg, CancelDialog, nItem);
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END HandleDialogInit;

PROCEDURE MatchControl(thisDlg : ADRCARD; id : CARDINAL) : CARDINAL;
VAR
    i   : ADRCARD;
BEGIN
    WITH Dialogs[thisDlg] DO
        lastCtrlId := id;

        i := 0;
        LOOP
            <*/PUSH/NOWARN:U*>
            IF i < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                CASE ctrls^[i].ct OF
                RadioGroup:
                    IF (id >= ctrls^[i].r_first) AND (id <= ctrls^[i].r_last) THEN
                        EXIT;
                    END;
                ELSE
                    IF ctrls^[i].id = id THEN
                        EXIT;
                    END;
                END;
                INC(i);
            ELSE
                EXIT;
            END;
        END;
    END;
    RETURN i;
END MatchControl;

PROCEDURE ValidateNumberEdit(thisDlg, index : ADRCARD) : BOOLEAN;
VAR
    numStr      : ARRAY [0..11] OF CHAR;
    val         : INTEGER;
    ctrlWnd     : HWND;

    PROCEDURE message(msgId : CARDINAL);
    VAR
        str     : ARRAY [0..127] OF CHAR;
    BEGIN
        IF msgId = 0 THEN
            str := "Invalid entry";
        ELSE
            WinShell.LoadString(msgId, str);
        END;
        BasicDialogs.MessageBox(str, BasicDialogs.MsgWarning);
    END message;

BEGIN
    WITH Dialogs[thisDlg] DO
        ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);
        IF ctrlWnd <> NIL THEN
            CASE ctrls^[index].ct OF
            NumberEdit:
                Edit_GetText(ctrlWnd, numStr, HIGH(numStr)+1);
                IF ctrls^[index].number_Hex THEN
                    IF GetHexOrDecNum(numStr, val) AND
                       (val >= ctrls^[index].number_Min) AND
                       (val <= ctrls^[index].number_Max)
                    THEN
                        ctrls^[index].number_Val := val;
                    ELSE
                        message(ctrls^[index].number_Msg);
                        SetInputFocusToDlg(thisDlg, GetUserControlId(thisDlg, index));
                        RETURN FALSE;
                    END;
                ELSE
                    IF StrToInt(numStr, val) AND
                       (val >= ctrls^[index].number_Min) AND
                       (val <= ctrls^[index].number_Max)
                    THEN
                        ctrls^[index].number_Val := val;
                    ELSE
                        message(ctrls^[index].number_Msg);
                        SetInputFocusToDlg(thisDlg, GetUserControlId(thisDlg, index));
                        RETURN FALSE;
                    END;
                END;
            ELSE
            END;
        END;
    END;
    RETURN TRUE;
END ValidateNumberEdit;

PROCEDURE ValidateFields(thisDlg : ADRCARD) : BOOLEAN;
VAR
    i           : ADRCARD;
BEGIN
    WITH Dialogs[thisDlg] DO
        <*/PUSH/NOWARN:U*>
        FOR i := 0 TO VAL(ADRCARD, numCtrls)-1 DO
        <*/POP*>
            CASE ctrls^[i].ct OF
            NumberEdit:
                IF NOT ValidateNumberEdit(thisDlg, i) THEN
                    RETURN FALSE;
                END;
            ELSE
            END;
        END;
    END;
    RETURN TRUE;
END ValidateFields;

PROCEDURE CheckValidate(thisDlg, index : ADRCARD) : BOOLEAN;
VAR
    nItem       : CARDINAL;
    nRes        : NotifyResult;
BEGIN
    IF Dialogs[thisDlg].ctrls^[index].ct IN ValidateControls THEN
        nItem := index;
        nRes := CallNotify(thisDlg, Validate, nItem);
        IF nRes = FailedValidation THEN
            SetInputFocusToDlg(thisDlg, GetUserControlId(thisDlg, index));
            RETURN FALSE;
        END;
    END;
    RETURN TRUE;
END CheckValidate;

PROCEDURE doSetFocus(thisDlg : ADRCARD; index : ADRCARD) : NotifyResult;
VAR
    nItem       : CARDINAL;
BEGIN
    WITH Dialogs[thisDlg] DO
        <*/PUSH/NOWARN:U*>
        IF (VAL(ADRCARD, lastFocus) <> index) AND
           (lastFocus <> MAX(CARDINAL)) AND
           (
            (ctrls^[index].ct <> PushButton) OR
            (ctrls^[index].b_type IN ValidateButtons)
           )
        THEN
        <*/POP*>
            IF NOT CheckValidate(thisDlg, lastFocus) THEN
                RETURN ContinueDialog;
            END;
        END;

        lastFocus := index;
        nItem := index;
        RETURN CallNotify(thisDlg, GainFocus, nItem);
    END;
END doSetFocus;

PROCEDURE doKillFocus(thisDlg : CARDINAL; ctrl : CARDINAL) : NotifyResult;
VAR
    nItem       : CARDINAL;
BEGIN
    nItem := ctrl;
    RETURN CallNotify(thisDlg, LoseFocus, nItem);
END doKillFocus;

PROCEDURE HandleUpDownBuddy(thisDlg : ADRCARD; ctrlWnd : HWND; ctrlMsg : CARDINAL);
VAR
    i           : ADRCARD;
    ctrl        : HWND;
    buddy       : HWND;
    val         : INTEGER;
    nItem       : CARDINAL;
    nRes        : NotifyResult;
    numStr      : ARRAY [0..15] OF CHAR;
BEGIN
    WITH Dialogs[thisDlg] DO
        i := 0;
        LOOP
            <*/PUSH/NOWARN:U*>
            IF i < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                IF ctrls^[i].ct = SpinButton THEN
                    ctrl := GetDlgItem(wnd, ctrls^[i].id);
                    buddy := UpDown_GetBuddy(ctrl);
                    IF buddy = ctrlWnd THEN
                        (* the message is from the buddy *)
                        (* we only support edit controls as buddy *)

                        nItem := i;
                        IF ctrlMsg = EN_KILLFOCUS THEN
                            Edit_GetText(buddy, numStr, HIGH(numStr)+1);
                            IF StrToInt(numStr, val) THEN
                                IF val > ctrls^[i].spin_Max THEN
                                    val := ctrls^[i].spin_Max;
                                ELSIF val < ctrls^[i].spin_Min THEN
                                    val := ctrls^[i].spin_Min;
                                END;
                                modified := TRUE;
                                ctrls^[i].spin_Val := val;
                                UpDown_SetPos(ctrl, val);
                                IntToStr(val, numStr);
                                Edit_SetText(buddy, numStr);

                                nRes := CallNotify(thisDlg, ValueChanged, nItem);
                            ELSE
                                IntToStr(ctrls^[i].spin_Val, numStr);
                                Edit_SetText(buddy, numStr);
                            END;

                            nRes := doKillFocus(thisDlg, nItem);

                        ELSIF ctrlMsg = EN_SETFOCUS THEN
                            nRes := doSetFocus(thisDlg, nItem);
                        END;

                        RETURN;
                    END;
                END;

                INC(i);
            ELSE
                EXIT;
            END;
        END;
    END;
END HandleUpDownBuddy;

PROCEDURE HandleCommand(thisDlg : ADRCARD; ctrlWnd : HWND; ctrlMsg : CARDINAL; id : CARDINAL) : BOOLEAN;
VAR
    nRes        : NotifyResult;
    i           : ADRCARD;
    ret         : INTEGER;
    nItem       : CARDINAL;
BEGIN
    WITH Dialogs[thisDlg] DO
        i := MatchControl(thisDlg, id);

        nItem := i;
        nRes := ContinueDialog;

        <*/PUSH/NOWARN:U*>
        IF i < VAL(ADRCARD, numCtrls) THEN
        <*/POP*>
            CASE ctrls^[i].ct OF
            CheckBox:
                IF ctrlMsg = BN_CLICKED THEN
                    modified := TRUE;
                    ctrls^[i].chk_on := NOT ctrls^[i].chk_on;
                    Button_SetCheck(ctrlWnd, ORD(ctrls^[i].chk_on));
                    nRes := CallNotify(thisDlg, Pressed, nItem);
                ELSIF ctrlMsg = BN_DBLCLK THEN
                    nRes := CallNotify(thisDlg, DoubleClicked, nItem);
                ELSIF ctrlMsg = BN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                ELSIF ctrlMsg = BN_KILLFOCUS THEN
                    nRes := doKillFocus(thisDlg, nItem);
                END;
            |
            RadioGroup:
                IF ctrlMsg = BN_CLICKED THEN
                    modified := TRUE;
                    RadioSelect(wnd, ctrls^[i], id);
                    nRes := CallNotify(thisDlg, Pressed, nItem);
                ELSIF ctrlMsg = BN_DBLCLK THEN
                    nRes := CallNotify(thisDlg, DoubleClicked, nItem);
                ELSIF ctrlMsg = BN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                ELSIF ctrlMsg = BN_KILLFOCUS THEN
                    nRes := doKillFocus(thisDlg, nItem);
                END;
            |
            LineEdit:
                IF ctrlMsg = EN_KILLFOCUS THEN
                    Edit_GetText(ctrlWnd, ctrls^[i].le_text^, ctrls^[i].le_max+1);
                END;

                IF ctrlMsg = EN_CHANGE THEN
                    modified := TRUE;
                    nRes := CallNotify(thisDlg, ValueChanged, nItem);
                ELSIF ctrlMsg = EN_KILLFOCUS THEN
                    nRes := doKillFocus(thisDlg, nItem);
                ELSIF ctrlMsg = EN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                END;
            |
            MultiLineEdit:
                IF (ctrlMsg = EN_KILLFOCUS) AND
                   (ctrls^[i].mle_text <> NIL) AND
                   (ctrls^[i].mle_max <> 0)
                THEN
                    Edit_GetText(ctrlWnd, ctrls^[i].mle_text^, ctrls^[i].mle_max+1);
                END;

                IF ctrlMsg = EN_CHANGE THEN
                    modified := TRUE;
                    nRes := CallNotify(thisDlg, ValueChanged, nItem);
                ELSIF ctrlMsg = EN_KILLFOCUS THEN
                    nRes := doKillFocus(thisDlg, nItem);
                ELSIF ctrlMsg = EN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                END;
            |
            EditTextFile:
                IF ctrlMsg = EN_CHANGE THEN
                    modified := TRUE;
                    ctrls^[i].etf_altered := TRUE;
                    nRes := CallNotify(thisDlg, ValueChanged, nItem);
                ELSIF ctrlMsg = EN_KILLFOCUS THEN
                    nRes := doKillFocus(thisDlg, nItem);
                ELSIF ctrlMsg = EN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                END;
            |
            NumberEdit:
                IF ctrlMsg = EN_CHANGE THEN
                    modified := TRUE;
                    nRes := CallNotify(thisDlg, ValueChanged, nItem);
                ELSIF ctrlMsg = EN_KILLFOCUS THEN
                    nRes := doKillFocus(thisDlg, nItem);
                ELSIF ctrlMsg = EN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                END;
            |
            ListBox:
                ret := ListBox_GetCurSel(ctrlWnd);
                IF ret <> LB_ERR THEN
                    ctrls^[i].lb_selIndex := ret;
                ELSE
                    ctrls^[i].lb_selIndex := MAX(CARDINAL);
                END;

                IF ctrlMsg = LBN_KILLFOCUS THEN
                    IF ctrls^[i].lb_selStr <> NIL THEN
                        IF ctrls^[i].lb_selIndex <> MAX(CARDINAL) THEN
                            ListBox_GetText(ctrlWnd, ctrls^[i].lb_selIndex, ctrls^[i].lb_selStr^);
                        ELSE
                            ctrls^[i].lb_selStr^[0] := '';
                        END;
                    END;

                    nRes := doKillFocus(thisDlg, nItem);
                ELSIF ctrlMsg = LBN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                ELSIF ctrlMsg = LBN_SELCHANGE THEN
                    modified := TRUE;
                    nRes := CallNotify(thisDlg, SelectionChanged, nItem);
                ELSIF ctrlMsg = LBN_DBLCLK THEN
                    nRes := CallNotify(thisDlg, DoubleClicked, nItem);
                END;

                (*
                IF (nRes = TerminateDialog) OR (nRes = CancelDialog) THEN
                    IF (nRes <> CancelDialog) AND (ctrls^[i].lb_selStr <> NIL) THEN
                        IF ctrls^[i].lb_selIndex <> MAX(CARDINAL) THEN
                            ListBox_GetText(ctrlWnd, ctrls^[i].lb_selIndex, ctrls^[i].lb_selStr^);
                        ELSE
                            ctrls^[i].lb_selStr^[0] := '';
                        END;
                    END;

                    DoCloseDialog(thisDlg, nRes, nItem);
                END;
                *)
            |
            ColumnListBox:
                (* these come via notify messages *)
            |
            DropDownList:
                IF ctrlMsg = CBN_KILLFOCUS THEN
                    ctrls^[i].ddl_selIndex := ComboBox_GetCurSel(ctrlWnd);
                END;

                IF ctrlMsg = CBN_KILLFOCUS THEN
                    nRes := doKillFocus(thisDlg, nItem);
                ELSIF ctrlMsg = CBN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                ELSIF ctrlMsg = CBN_SELCHANGE THEN
                    modified := TRUE;
                    nRes := CallNotify(thisDlg, SelectionChanged, nItem);
                ELSIF ctrlMsg = CBN_DBLCLK THEN
                    nRes := CallNotify(thisDlg, DoubleClicked, nItem);
                END;

                (*
                IF (nRes = TerminateDialog) OR (nRes = CancelDialog) THEN
                    IF nRes <> CancelDialog THEN
                        ctrls^[i].ddl_selIndex := ComboBox_GetCurSel(ctrlWnd);
                    END;

                    DoCloseDialog(thisDlg, nRes, nItem);
                END;
                *)
            |
            ComboBox:
                IF ctrlMsg = CBN_KILLFOCUS THEN
                    IF ctrls^[i].cb_selStr <> NIL THEN
                        ComboBox_GetText(ctrlWnd, ctrls^[i].cb_selStr^, ctrls^[i].cb_max+1);
                    END;
                    (*ctrls^[i].cb_selIndex := ComboBox_GetCurSel(ctrlWnd);*)
                END;

                IF ctrlMsg = CBN_EDITCHANGE THEN
                    modified := TRUE;
                    nRes := CallNotify(thisDlg, ValueChanged, nItem);
                ELSIF ctrlMsg = CBN_KILLFOCUS THEN
                    nRes := doKillFocus(thisDlg, nItem);
                ELSIF ctrlMsg = CBN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                ELSIF ctrlMsg = CBN_SELCHANGE THEN
                    modified := TRUE;
                    nRes := CallNotify(thisDlg, SelectionChanged, nItem);
                ELSIF ctrlMsg = CBN_DBLCLK THEN
                    nRes := CallNotify(thisDlg, DoubleClicked, nItem);
                END;

                (*
                IF (nRes = TerminateDialog) OR (nRes = CancelDialog) THEN
                    IF nRes <> CancelDialog THEN
                        IF ctrls^[i].cb_selStr <> NIL THEN
                            ComboBox_GetText(ctrlWnd, ctrls^[i].cb_selStr^, ctrls^[i].cb_max);
                        END;

                        (*
                        ctrls^[i].cb_selIndex := ComboBox_GetCurSel(ctrlWnd);
                                    *)
                    END;

                    DoCloseDialog(thisDlg, nRes, nItem);
                END;
                *)
            |
            ColorDropDownList:
                IF ctrlMsg = CBN_KILLFOCUS THEN
                    ctrls^[i].cddl_selIndex := ComboBox_GetCurSel(ctrlWnd);
                END;

                IF ctrlMsg = CBN_KILLFOCUS THEN
                    nRes := doKillFocus(thisDlg, nItem);
                ELSIF ctrlMsg = CBN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                ELSIF ctrlMsg = CBN_SELCHANGE THEN
                    modified := TRUE;
                    nRes := CallNotify(thisDlg, SelectionChanged, nItem);
                ELSIF ctrlMsg = CBN_DBLCLK THEN
                    nRes := CallNotify(thisDlg, DoubleClicked, nItem);
                END;

                (*
                IF (nRes = TerminateDialog) OR (nRes = CancelDialog) THEN
                    (*
                    IF nRes <> CancelDialog THEN
                        ctrls^[i].cb_selIndex := ComboBox_GetCurSel(ctrlWnd);
                    END;
                    *)

                    DoCloseDialog(thisDlg, nRes, nItem);
                END;
                *)
            |
            SpinButton:
                (* comes via a other messages *)
            |
            PushButton:
                IF ctrlMsg = BN_CLICKED THEN
                    CASE ctrls^[i].b_type OF
                    CloseButton:
                        DoCloseDialog(thisDlg, TerminateDialog, ctrls^[i].b_return);
                    |
                    CancelButton:
                        DoCloseDialog(thisDlg, CancelDialog, ctrls^[i].b_return);
                    ELSE
                        nRes := CallNotify(thisDlg, Pressed, nItem);
                    END;

                ELSIF ctrlMsg = BN_DBLCLK THEN
                    nRes := CallNotify(thisDlg, DoubleClicked, nItem);
                ELSIF ctrlMsg = BN_SETFOCUS THEN
                    nRes := doSetFocus(thisDlg, nItem);
                ELSIF ctrlMsg = BN_KILLFOCUS THEN
                    nRes := doKillFocus(thisDlg, nItem);
                END;
            |
            DisplayBitmap,
            DialogTitle,
            StaticText,
            StaticTextAttrib,
            Timer,
            DefaultButton,
            UserData:
            END;

        ELSE
            HandleUpDownBuddy(thisDlg, ctrlWnd, ctrlMsg);
        END;
    END;
    RETURN FALSE;
END HandleCommand;

PROCEDURE HandleToolTipDisplay(thisDlg : CARDINAL; nPtr : LPNMTTDISPINFO) : BOOL;
VAR
    str         : POINTER TO ARRAY [0..ToolTipStringSize-1] OF CHAR;
BEGIN
    SendMessage(nPtr^.hdr.hwndFrom, TTM_SETMAXTIPWIDTH, 0, WinShell.ScreenInfo.xSize / 2);

    str := FindInternalData(thisDlg, ToolTipKey);
    IF str <> NIL THEN
        nPtr^.lpszText := ADR(str^);
    ELSE
        nPtr^.lpszText := ADR("InternalError: Internal data not found (HelpText)");
    END;

    RETURN VAL(BOOL, 0);
END HandleToolTipDisplay;

PROCEDURE GetColumnSelStr(ctrlWnd : HWND; VAR INOUT ctrl : CONTROL);
VAR
    text        : ARRAY [0..127] OF CHAR;
    result      : ARRAY [0..255] OF CHAR;
    i, l        : CARDINAL;
BEGIN
    result := "";
    FOR i := 1 TO ctrl.clb_columns DO
        ListView_GetItemText(ctrlWnd, ctrl.clb_selIndex, i-1, text, HIGH(text));

        Append(text, result);
        AppendChar(CHR(1), result);
    END;
    result[HIGH(result)] := '';

    l := LENGTH(result);
    ConvertToNulls(CHR(1), result);
    MoveMem(ctrl.clb_selStr, ADR(result), l+1);
END GetColumnSelStr;

PROCEDURE HandleNotify(thisDlg : ADRCARD; nPtr : LPNMHDR) : BOOL;
VAR
    nRes        : NotifyResult;
    i           : ADRCARD;
    nItem       : CARDINAL;
    ctrlWnd     : HWND;
    ret         : INTEGER;
    lvn         : LPNMLISTVIEW;
    udn         : LPNMUPDOWN;
    changes     : CARDINAL;
BEGIN
    IF nPtr^.code = TTN_GETDISPINFO THEN
        RETURN HandleToolTipDisplay(thisDlg, CAST(LPNMTTDISPINFO, nPtr));
    END;

    WITH Dialogs[thisDlg] DO
        i := MatchControl(thisDlg, nPtr^.idFrom);

        nItem := i;
        nRes := ContinueDialog;

        <*/PUSH/NOWARN:U*>
        IF i < VAL(ADRCARD, numCtrls) THEN
        <*/POP*>
            ctrlWnd := GetDlgItem(wnd, ctrls^[i].id);

            CASE ctrls^[i].ct OF
            SpinButton:
                IF nPtr^.code = UDN_DELTAPOS THEN
                    udn := CAST(LPNMUPDOWN, nPtr);
                    ctrls^[i].spin_Val := udn^.iPos + udn^.iDelta;
                    modified := TRUE;
                    nRes := CallNotify(thisDlg, ValueChanged, nItem);
                END;
            |
            ColumnListBox:
                IF nPtr^.code = LVN_DELETEALLITEMS THEN
                    SetWindowLong(wnd, DWLP_MSGRESULT, ORD(TRUE));
                    RETURN TRUE;
                ELSE
                    ret := ListView_GetNextItem(ctrlWnd, -1, LVNI_SELECTED);
                    IF ret <> LB_ERR THEN
                        ctrls^[i].clb_selIndex := ret;
                    ELSE
                        ctrls^[i].clb_selIndex := MAX(CARDINAL);
                    END;

                    CASE nPtr^.code OF
                    LVN_ITEMCHANGED:
                        lvn := CAST(LPNMLISTVIEW, nPtr);
                        changes := lvn^.uNewState BAND (BNOT lvn^.uOldState);
                        IF (changes BAND LVIS_SELECTED) <> 0 THEN
                            modified := TRUE;
                            nRes := CallNotify(thisDlg, SelectionChanged, nItem);
                        END;
                    |
                    NM_DBLCLK:
                        nRes := CallNotify(thisDlg, DoubleClicked, nItem);
                    |
                    NM_KILLFOCUS:
                        IF ctrls^[i].clb_selStr <> NIL THEN
                            IF ctrls^[i].clb_selIndex <> MAX(CARDINAL) THEN
                                GetColumnSelStr(ctrlWnd, ctrls^[i]);
                            ELSE
                                ctrls^[i].clb_selStr^[0] := '';
                            END;
                        END;
                        nRes := doKillFocus(thisDlg, nItem);
                    |
                    NM_SETFOCUS:
                        nRes := doSetFocus(thisDlg, nItem);
                    ELSE
                    END;

                    (*
                    IF (nRes = TerminateDialog) OR (nRes = CancelDialog) THEN
                        IF (nRes <> CancelDialog) AND (ctrls^[i].clb_selStr <> NIL) THEN
                           IF ctrls^[i].clb_selStr <> NIL THEN
                               IF ctrls^[i].clb_selIndex <> MAX(CARDINAL) THEN
                                   GetColumnSelStr(ctrlWnd, ctrls^[i]);
                               ELSE
                                   ctrls^[i].clb_selStr^[0] := '';
                               END;
                           END;
                        END;

                        DoCloseDialog(thisDlg, nRes, nItem);
                    END;
                    *)
                END;
            ELSE
            END;
        END;
    END;
    RETURN FALSE;
END HandleNotify;

PROCEDURE HandleMeasureItem(thisDlg : ADRCARD; id : CARDINAL; pMeasure : LPMEASUREITEMSTRUCT) : BOOLEAN;
VAR
    nRes        : NotifyResult;
    i           : ADRCARD;
    nItem       : CARDINAL;
BEGIN
    UNREFERENCED_PARAMETER(pMeasure);

    WITH Dialogs[thisDlg] DO
        i := MatchControl(thisDlg, id);

        nItem := i;
        nRes := ContinueDialog;

        <*/PUSH/NOWARN:U*>
        IF i < VAL(ADRCARD, numCtrls) THEN
        <*/POP*>
            CASE ctrls^[i].ct OF
            ColorDropDownList:
                RETURN TRUE;
            ELSE
            END;
        END;
    END;
    RETURN FALSE;
END HandleMeasureItem;

PROCEDURE HandleDrawItem(thisDlg : ADRCARD; id : CARDINAL; pDraw : LPDRAWITEMSTRUCT) : BOOLEAN;
VAR
    nRes        : NotifyResult;
    i           : ADRCARD;
    nItem       : CARDINAL;
    clr         : COLORREF;
    myBrush     : HBRUSH;
BEGIN
    WITH Dialogs[thisDlg] DO
        i := MatchControl(thisDlg, id);

        nItem := i;
        nRes := ContinueDialog;

        <*/PUSH/NOWARN:U*>
        IF i < VAL(ADRCARD, numCtrls) THEN
        <*/POP*>
            CASE ctrls^[i].ct OF
            ColorDropDownList:
                clr := ctrls^[i].cddl_colorData^[pDraw^.itemID];

                myBrush := CreateSolidBrush(clr);
                FillRect(pDraw^.hDC, pDraw^.rcItem, myBrush);
                DeleteBrush(myBrush);

                IF (pDraw^.itemState BAND ODS_FOCUS) <> 0 THEN
                    DrawFocusRect(pDraw^.hDC, pDraw^.rcItem);
                END;

                RETURN TRUE;
            ELSE
            END;
        END;
    END;
    RETURN FALSE;
END HandleDrawItem;

PROCEDURE HandleControlColor(thisDlg : ADRCARD; id : CARDINAL; ctrlWnd : HWND; dc : HDC) : HBRUSH;
VAR
    nRes        : NotifyResult;
    i           : ADRCARD;
    nItem       : CARDINAL;
    clr         : COLORREF;
    intPtr      : StaticTextInternalPointer;
BEGIN
    UNREFERENCED_PARAMETER(ctrlWnd);

    WITH Dialogs[thisDlg] DO
        i := MatchControl(thisDlg, id);

        nItem := i;
        nRes := ContinueDialog;

        <*/PUSH/NOWARN:U*>
        IF i < VAL(ADRCARD, numCtrls) THEN
        <*/POP*>
            CASE ctrls^[i].ct OF
            StaticTextAttrib:
                intPtr := FindInternalData(thisDlg, ADR(ctrls^[i]));
                IF intPtr <> NIL THEN
                    IF intPtr^.brush <> NULL_HBRUSH THEN
                        DeleteBrush(intPtr^.brush);
                    END;
                    intPtr^.brush := CreateSolidBrush(ctrls^[i].sta_back);

                    clr := ctrls^[i].sta_fore;
                    clr := (clr BAND 0FFFFFFh) BOR 2000000h;
                    SetTextColor(dc, clr);

                    clr := ctrls^[i].sta_back;
                    clr := (clr BAND 0FFFFFFh) BOR 2000000h;
                    SetBkColor(dc, clr);
                    RETURN intPtr^.brush;
                END;
            ELSE
            END;
        END;
    END;
    RETURN NULL_HBRUSH;
END HandleControlColor;

PROCEDURE HandleTimer(thisDlg : ADRCARD; wParam : CARDINAL);
VAR
    i           : ADRCARD;
    nRes        : NotifyResult;
    nItem       : CARDINAL;
BEGIN
    WITH Dialogs[thisDlg] DO
        IF notify <> NIL_NOTIFY THEN
            <*/PUSH/NOWARN:U*>
            FOR i := 0 TO VAL(ADRCARD, numCtrls)-1 DO
            <*/POP*>
                IF (ctrls^[i].ct = Timer) AND
                   (ctrls^[i].id = wParam-TimerIdFudge)
                THEN
                    nItem := i;
                    nRes := CallNotify(thisDlg, TimerElapsed, nItem);

                    RETURN;
                END;
            END;
        END;
    END;
END HandleTimer;

PROCEDURE HandleActivate(thisDlg : CARDINAL; active : BOOLEAN);
VAR
    nItem       : CARDINAL;
    nRes        : NotifyResult;
BEGIN
    nItem := MAX(CARDINAL);
    IF active THEN
        nRes := CallNotify(thisDlg, GainFocus, nItem);
    ELSE
        nRes := CallNotify(thisDlg, LoseFocus, nItem);
    END;
END HandleActivate;

(*
PROCEDURE HandleScroll(thisDlg : CARDINAL; ctrlWnd : HWND; code : CARDINAL; pos : INTEGER) : BOOLEAN;
VAR
    i           : CARDINAL;
    nItem       : CARDINAL;
    nRes        : NotifyResult;
BEGIN
    WITH Dialogs[thisDlg] DO
        i := 0;
        WHILE i < numCtrls DO
            IF (ctrls^[i].ct = ScrollBar) AND
               (ctrlWnd = GetDlgItem(wnd, ctrls^[i].id))
            THEN
                scrollPos := pos;
                CASE code OF
                SB_LINEUP:
                    scrollCode := ScrollLineDec;
                |
                SB_LINEDOWN:
                    scrollCode := ScrollLineInc;
                |
                SB_PAGEUP:
                    scrollCode := ScrollPageDec;
                |
                SB_PAGEDOWN:
                    scrollCode := ScrollPageInc;
                |
                SB_THUMBPOSITION:
                    scrollCode := ScrollPos;
                ELSE
                    RETURN FALSE;
                END;

                nItem := i;
                nRes := CallNotify(thisDlg, ScrollInfo, nItem);
                RETURN TRUE;
            END;
            INC(i);
        END;
    END;
    RETURN FALSE;
END HandleScroll;
*)

PROCEDURE ToolTipSubclassProc(hwnd : HWND; mess : UINT; wParam : WPARAM; lParam : LPARAM) : LRESULT [EXPORT, SbOsSystem];
VAR
    i   : ADRCARD;
    ti  : TOOLINFO;
BEGIN
    FOR i := 1 TO MaxDlgs DO
        IF Dialogs[i].toolTip = hwnd THEN
            IF Dialogs[i].toolTipShown AND
               (
                (mess = WM_LBUTTONDOWN) OR
                (mess = WM_RBUTTONDOWN) OR
                (mess = WM_MBUTTONDOWN) OR
                (mess = WM_KEYDOWN)
               )
            THEN
                ReleaseCapture;

                ti.cbSize := SIZE(ti);
                ti.hwnd := Dialogs[i].wnd;
                ti.uId := ToolTipID + Dialogs[i].activePage;
                SendMessage(hwnd,
                            TTM_TRACKACTIVATE,
                            ORD(FALSE),
                            CAST(LPARAM, ADR(ti)));
            END;

            RETURN CallWindowProc(Dialogs[i].oldTipWndProc, hwnd, mess, wParam, lParam);
        END;
    END;
    RETURN DefWindowProc(hwnd, mess, wParam, lParam);
END ToolTipSubclassProc;

PROCEDURE HandleHelp(thisDlg : ADRCARD; hlp : LPHELPINFO);
VAR
    index       : ADRCARD;
    ctrlWnd     : HWND;
    rect        : RECT;
    nItem       : CARDINAL;
    nRes        : NotifyResult;
    height      : INTEGER;
    ti          : TOOLINFO;
    str         : POINTER TO ARRAY [0..ToolTipStringSize-1] OF CHAR;
BEGIN
    IF hlp^.iContextType = HELPINFO_WINDOW THEN
        WITH Dialogs[thisDlg] DO
            ctrlWnd := hlp^.hItemHandle;
            index := MatchControl(thisDlg, hlp^.iCtrlId);

            <*/PUSH/NOWARN:U*>
            IF index >= VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                (* did not find a matching control. try looking for a label
                   control by the defined convenience mechanism.
                *)

                index := MatchControl(thisDlg, hlp^.iCtrlId+1);
                <*/PUSH/NOWARN:U*>
                IF index < VAL(ADRCARD, numCtrls) THEN
                <*/POP*>
                    CASE ctrls^[index].ct OF
                    RadioGroup,
                    LineEdit, MultiLineEdit, NumberEdit,
                    ListBox, ColumnListBox,
                    ComboBox, DropDownList, ColorDropDownList:
                        (* the special cased controls *)
                    ELSE
                        index := numCtrls;
                    END;
                END;
            END;

            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                IF (ctrls^[index].helpId <> 0) AND
                   (toolTip <> NIL) AND
                   (ctrlWnd <> NIL)
                THEN
                    str := FindInternalData(thisDlg, ToolTipKey);
                    IF str = NIL THEN
                        str := AllocateInternalData(thisDlg, ToolTipKey, ToolTipStringSize);
                    END;
                    WinShell.LoadString(ctrls^[index].helpId, str^);

                    CASE ctrls^[index].ct OF
                    MultiLineEdit, EditTextFile,
                    ListBox,
                    ColumnListBox:
                        height := 0;
                    ELSE
                        GetClientRect(ctrlWnd, rect);
                        height := rect.bottom;
                    END;
                    GetWindowRect(ctrlWnd, rect);
                    SendMessage(toolTip, TTM_TRACKPOSITION, 0, MAKELONG(rect.left, rect.top+height));

                    ti.cbSize := SIZE(ti);
                    ti.uId := ToolTipID + activePage;
                    ti.hwnd := wnd;
                    SendMessage(toolTip, TTM_TRACKACTIVATE, ORD(TRUE), CAST(LPARAM, ADR(ti)));

                    SetFocus(toolTip);

                    toolTipShown := TRUE;
                    SetCapture(toolTip);
                ELSE
                    nItem := index;
                    nRes := CallNotify(thisDlg, ContextHelp, nItem);
                END;
            END;
        END;
    END;
END HandleHelp;

PROCEDURE ShutDownDialog(dlgNum : ADRCARD);
VAR
    nItem       : CARDINAL;
BEGIN
    nItem := MAX(CARDINAL);
    CallNotify(dlgNum, DestroyDialog, nItem);

    IF IsWindow(Dialogs[dlgNum].wnd) THEN
        IF Dialogs[dlgNum].modeless THEN
            IF Dialogs[dlgNum].pages = NIL THEN
                SetModelessDialog(Dialogs[dlgNum].wnd, FALSE);
            ELSE
                SetModelessDialog(GetParent(Dialogs[dlgNum].wnd), FALSE);
            END;
        END;
    END;

    DialogCleanup(dlgNum);
    DeallocateDialog(dlgNum);
END ShutDownDialog;

PROCEDURE MyDlgProc(hdlg : HWND;
                    msg : UINT;
                    wParam : WPARAM;
                    lParam : LPARAM) : INT_PTR [EXPORT, WINDOWS];
VAR
    id          : CARDINAL;
    ctrlMsg     : CARDINAL;
    ctrlWnd     : HWND;
    thisDlg     : ADRCARD;
    saveLast    : CARDINAL;
    dlgRes      : BOOL;
    brush       : HBRUSH;
    nPtr        : LPNMHDR;
    pMeasure    : LPMEASUREITEMSTRUCT;
    pDraw       : LPDRAWITEMSTRUCT;
    dc          : HDC;

BEGIN
    dlgRes := FALSE;
    saveLast := GetLastDialog();

    IF msg <> WM_INITDIALOG THEN
        IF NOT FindDlg(hdlg, saveLast) THEN
            RETURN 0;
        END;
    END;

    thisDlg := GetLastDialog();

    CASE msg OF
    WM_INITDIALOG:
        thisDlg := ORD(lParam);
        SetLastDialog(thisDlg);
        Dialogs[thisDlg].wnd := hdlg;

        Dialogs[thisDlg].firstInit := FALSE;

        IF Dialogs[thisDlg].modeless THEN
            IF NOT SetModelessDialog(hdlg, TRUE) THEN
                DoCloseDialog(thisDlg, CancelDialog, MAX(CARDINAL));
                SetLastDialog(saveLast);
                RETURN 0;
            END;
        END;

        PositionDialogDlg(thisDlg, Dialogs[thisDlg].position);

        dlgRes := HandleDialogInit(thisDlg);

        SetLastDialog(saveLast);
        RETURN ORD(dlgRes);
    |
    WM_COMMAND:
        id := LOWORD(wParam);
        ctrlWnd := CAST(HWND, lParam);
        ctrlMsg := HIWORD(wParam);

        dlgRes := HandleCommand(thisDlg, ctrlWnd, ctrlMsg, id);
        SetLastDialog(saveLast);
        RETURN ORD(dlgRes);
    |
    WM_NOTIFY:
        nPtr := CAST(LPNMHDR, lParam);
        dlgRes := HandleNotify(thisDlg, nPtr);
        SetLastDialog(saveLast);
        RETURN ORD(dlgRes);
    |
    (*
    WM_VSCROLL, WM_HSCROLL:
        ctrlWnd := CAST(HWND, lParam);
        IF ctrlWnd <> NULL_HWND THEN
            code := LOWORD(wParam);
            pos := CAST(INTEGER16, HIWORD(wParam));
            dlgRes := HandleScroll(thisDlg, ctrlWnd, code, pos);
            SetLastDialog(saveLast);
            RETURN dlgRes;
        END;
    |
    *)
    WM_MEASUREITEM:
        id := LOWORD(wParam);
        pMeasure := CAST(LPMEASUREITEMSTRUCT, lParam);

        dlgRes := HandleMeasureItem(thisDlg, id, pMeasure);
        SetLastDialog(saveLast);
        RETURN ORD(dlgRes);
    |
    WM_DRAWITEM:
        id := LOWORD(wParam);
        pDraw := CAST(LPDRAWITEMSTRUCT, lParam);

        dlgRes := HandleDrawItem(thisDlg, id, pDraw);
        SetLastDialog(saveLast);
        RETURN ORD(dlgRes);
    |
    WM_CTLCOLORSTATIC:
        dc := CAST(HDC, wParam);
        ctrlWnd := CAST(HWND, lParam);
        id := GetDlgCtrlID(ctrlWnd);

        brush := HandleControlColor(thisDlg, id, ctrlWnd, dc);
        SetLastDialog(saveLast);
        RETURN CAST(INT_PTR, brush);
    |
    WM_DESTROY:
        ShutDownDialog(thisDlg);
    |
    WM_TIMER:
        HandleTimer(thisDlg, wParam);
    |
    WM_HELP:
        HandleHelp(thisDlg, CAST(LPHELPINFO, lParam));
    |
    WM_ACTIVATE:
        HandleActivate(thisDlg, LOWORD(wParam) <> WA_INACTIVE);
    |
    WM_SHOWWINDOW:
        IF wParam <> 0 THEN
            IF (lParam = SW_PARENTOPENING) AND (Dialogs[thisDlg].positionWhenRestored) THEN
                Dialogs[thisDlg].positionWhenRestored := FALSE;
                PositionDialogDlg(thisDlg, Dialogs[thisDlg].position);
            END;
        END;
    ELSE
    END;

    SetLastDialog(saveLast);
    RETURN 0;

EXCEPT
    DoExcept(hdlg);
    SetLastDialog(saveLast);
    RETURN 0;
END MyDlgProc;

TYPE
    ChangeDialogFontFlags = (CDF_CENTER, CDF_TOPLEFT, CDF_NONE);

PROCEDURE ChangeDialogFont(wnd : HWND; font : HFONT; flag : ChangeDialogFontFlags);
VAR
    clientRect,
    newClientRect,
    newWindowRect       : RECT;
    tmOld,
    tmNew               : TEXTMETRIC;
    dc                  : HDC;
    oldFont             : HFONT;
    oldHeight,
    newHeight,
    xDiff,
    yDiff               : INTEGER;
    rect,
    windowRect          : RECT;
    pt                  : POINT;
    childWnd            : HWND;
    str                 : ARRAY [0..31] OF CHAR;
BEGIN
    (* grab old and new text metrics *)

    dc := GetDC(wnd);

    oldFont := SelectFont(dc, GetWindowFont(wnd));
    GetTextMetrics(dc, tmOld);

    SelectFont(dc, font);
    GetTextMetrics(dc, tmNew);
    oldFont := SelectFont(dc, oldFont);

    ReleaseDC(wnd, dc);

    oldHeight := tmOld.tmHeight+tmOld.tmExternalLeading;
    newHeight := tmNew.tmHeight+tmNew.tmExternalLeading;

    IF flag <> CDF_NONE THEN
        (* calculate new dialog window rectangle *)

        GetWindowRect(wnd, windowRect);
        GetClientRect(wnd, clientRect);
        xDiff := (windowRect.right - windowRect.left) - (clientRect.right - clientRect.left);
        yDiff := (windowRect.bottom - windowRect.top) - (clientRect.bottom - clientRect.top);

        newClientRect.top := 0;
        newClientRect.left := 0;
        newClientRect.right := (clientRect.right * tmNew.tmAveCharWidth) / tmOld.tmAveCharWidth;
        newClientRect.bottom := (clientRect.bottom * newHeight) / oldHeight;

        IF flag = CDF_CENTER THEN
            newWindowRect.left :=
                windowRect.left - ((newClientRect.right - clientRect.right)/2);
            newWindowRect.top :=
                windowRect.top - ((newClientRect.bottom - clientRect.bottom)/2);
        ELSE
            newWindowRect.left := windowRect.left;
            newWindowRect.top := windowRect.top;
        END;
        newWindowRect.right := newWindowRect.left + newClientRect.right + xDiff;
        newWindowRect.bottom := newWindowRect.top + newClientRect.bottom + yDiff;

        MoveWindow(wnd,
                   newWindowRect.left,
                   newWindowRect.top,
                   newWindowRect.right-newWindowRect.left,
                   newWindowRect.bottom-newWindowRect.top,
                   FALSE);
    END;

    SetWindowFont(wnd, font, TRUE);

    (* iterate through and move all child windows and change their font. *)

    childWnd := GetFirstChild(wnd);
    WHILE childWnd <> NULL_HWND DO
        SetWindowFont(childWnd, font, FALSE);

        GetWindowRect(childWnd, windowRect);

        GetClassName(childWnd, str, HIGH(str)+1);
        IF EqualI(str, "COMBOBOX") THEN
            rect.top := 0;(*suppress uninit warning*)
            SendMessage(childWnd, CB_GETDROPPEDCONTROLRECT, 0, CAST(LPARAM, ADR(rect)));
            windowRect.right := rect.right;
            windowRect.bottom := rect.bottom;
        END;

        pt.x := windowRect.left;
        pt.y := windowRect.top;
        ScreenToClient(wnd, pt);
        windowRect.left := pt.x;
        windowRect.top := pt.y;

        pt.x := windowRect.right;
        pt.y := windowRect.bottom;
        ScreenToClient(wnd, pt);
        windowRect.right := pt.x;
        windowRect.bottom := pt.y;

        windowRect.left := (windowRect.left * tmNew.tmAveCharWidth) / tmOld.tmAveCharWidth;
        windowRect.right := (windowRect.right * tmNew.tmAveCharWidth) / tmOld.tmAveCharWidth;
        windowRect.top := (windowRect.top * newHeight) / oldHeight;
        windowRect.bottom := (windowRect.bottom * newHeight) / oldHeight;

        MoveWindow(childWnd,
                   windowRect.left,
                   windowRect.top,
                   windowRect.right-windowRect.left,
                   windowRect.bottom-windowRect.top,
                   TRUE);

        childWnd := GetNextSibling(childWnd);
    END;
END ChangeDialogFont;

PROCEDURE MyPropSheetDlgProc(hdlg : HWND;
                             msg : UINT;
                             wParam : WPARAM;
                             lParam : LPARAM) : INT_PTR [EXPORT, WINDOWS];
VAR
    id          : CARDINAL;
    ctrlMsg     : CARDINAL;
    ctrlWnd     : HWND;
    nItem       : CARDINAL;
    thisDlg     : ADRCARD;
    thisPage    : ADRCARD;
    saveLast    : CARDINAL;
    dlgRes      : BOOL;
    nRes        : NotifyResult;
    brush       : HBRUSH;
    pMeasure    : LPMEASUREITEMSTRUCT;
    pDraw       : LPDRAWITEMSTRUCT;
    dc          : HDC;
    nPtr        : LPNMHDR;
    pagePtr     : LPPROPSHEETPAGE;
BEGIN
    dlgRes := FALSE;
    saveLast := GetLastDialog();

    IF msg <> WM_INITDIALOG THEN
        IF NOT FindDlgPage(hdlg, saveLast) THEN
            RETURN 0;
        END;
    END;

    thisDlg := GetLastDialog();
    thisPage := 0;
    IF thisDlg <> 0 THEN
        thisPage := Dialogs[thisDlg].activePage;
    END;

    CASE msg OF
    WM_INITDIALOG:
        pagePtr := CAST(LPPROPSHEETPAGE, lParam);
        thisDlg := pagePtr^.lParam REM 65536;
        thisPage := pagePtr^.lParam / 65536;

        SetLastDialog(thisDlg);

        Dialogs[thisDlg].pageWnd^[thisPage] := hdlg;
        Dialogs[thisDlg].wnd := hdlg;
        Dialogs[thisDlg].activePage := thisPage;
        Dialogs[thisDlg].ctrls := Dialogs[thisDlg].pages^[thisPage].controls;
        Dialogs[thisDlg].numCtrls := Dialogs[thisDlg].pages^[thisPage].numControls;
        Dialogs[thisDlg].notify := Dialogs[thisDlg].pages^[thisPage].notify;

        IF Dialogs[thisDlg].firstInit THEN
            IF Dialogs[thisDlg].modeless THEN
                IF NOT SetModelessDialog(GetParent(hdlg), TRUE) THEN
                    DoCloseDialog(thisDlg, CancelDialog, MAX(CARDINAL));
                    SetLastDialog(saveLast);
                    RETURN 0;
                END;
            END;
        END;

        IF Dialogs[thisDlg].dlgFont <> NULL_HFONT THEN
            IF Dialogs[thisDlg].firstInit THEN
                ChangeDialogFont(GetParent(hdlg), Dialogs[thisDlg].dlgFont, CDF_TOPLEFT);
            END;
            ChangeDialogFont(hdlg, Dialogs[thisDlg].dlgFont, CDF_TOPLEFT);
        END;

        IF Dialogs[thisDlg].firstInit THEN
            PositionDialogDlg(thisDlg, Dialogs[thisDlg].position);
        END;

        dlgRes := HandleDialogInit(thisDlg);

        Dialogs[thisDlg].firstInit := FALSE;

        SetLastDialog(saveLast);
        RETURN 1;
    |
    WM_NOTIFY:
        nPtr := CAST(LPNMHDR, lParam);

        nItem := MAX(CARDINAL);
        nRes := ContinueDialog;
        CASE nPtr^.code OF
        PSN_SETACTIVE:
            nRes := CallNotify(thisDlg, PageActivate, nItem);
            SetWindowLong(hdlg, DWLP_MSGRESULT, PSNRET_NOERROR);
        |
        PSN_KILLACTIVE:
            IF NOT ValidateFields(thisDlg) THEN
                SetWindowLong(hdlg, DWLP_MSGRESULT, PSNRET_INVALID);
            ELSE
                nRes := CallNotify(thisDlg, PageDeactivate, nItem);
                IF nRes = DisallowPageDeactivate THEN
                    SetWindowLong(hdlg, DWLP_MSGRESULT, PSNRET_INVALID);
                ELSE
                    SetWindowLong(hdlg, DWLP_MSGRESULT, PSNRET_NOERROR);
                END;
            END;
        |
        PSN_APPLY:
            Dialogs[thisDlg].closeWasCancel := FALSE;
            IF Dialogs[thisDlg].modified THEN
                nItem := MAX(CARDINAL);
                nRes := CallNotify(thisDlg, DialogModified, nItem);
            END;
            SaveTextFiles(thisDlg);
            SetWindowLong(hdlg, DWLP_MSGRESULT, PSNRET_NOERROR);
        |
        PSN_RESET:
            Dialogs[thisDlg].closeWasCancel := TRUE;
            Dialogs[thisDlg].modified := FALSE;
            nRes := CallNotify(thisDlg, PageCancel, nItem);
        ELSE
            dlgRes := HandleNotify(thisDlg, nPtr);
            SetLastDialog(saveLast);
            RETURN ORD(dlgRes);
        END;

        SetLastDialog(saveLast);
        RETURN 1;
    |
    WM_COMMAND:
        id := LOWORD(wParam);
        ctrlWnd := CAST(HWND, lParam);
        ctrlMsg := HIWORD(wParam);

        dlgRes := HandleCommand(thisDlg, ctrlWnd, ctrlMsg, id);
        SetLastDialog(saveLast);
        RETURN ORD(dlgRes);
    |
    (*
    WM_VSCROLL, WM_HSCROLL:
        ctrlWnd := CAST(HWND, lParam);
        IF ctrlWnd <> NULL_HWND THEN
            code := LOWORD(wParam);
            pos := CAST(INTEGER16, HIWORD(wParam));
            dlgRes := HandleScroll(thisDlg, ctrlWnd, code, pos);
            SetLastDialog(saveLast);
            RETURN dlgRes;
        END;
    |
    *)
    WM_MEASUREITEM:
        id := LOWORD(wParam);
        pMeasure := CAST(LPMEASUREITEMSTRUCT, lParam);

        dlgRes := HandleMeasureItem(thisDlg, id, pMeasure);
        SetLastDialog(saveLast);
        RETURN ORD(dlgRes);
    |
    WM_DRAWITEM:
        id := LOWORD(wParam);
        pDraw := CAST(LPDRAWITEMSTRUCT, lParam);

        dlgRes := HandleDrawItem(thisDlg, id, pDraw);
        SetLastDialog(saveLast);
        RETURN ORD(dlgRes);
    |
    WM_CTLCOLORSTATIC:
        dc := CAST(HDC, wParam);
        ctrlWnd := CAST(HWND, lParam);
        id := GetDlgCtrlID(ctrlWnd);

        brush := HandleControlColor(thisDlg, id, ctrlWnd, dc);
        SetLastDialog(saveLast);
        RETURN CAST(INT_PTR, brush);
    |
    WM_DESTROY:
        ShutDownDialog(thisDlg);
    |
    WM_TIMER:
        HandleTimer(thisDlg, wParam);
    |
    WM_HELP:
        HandleHelp(thisDlg, CAST(LPHELPINFO, lParam));
    |
    WM_ACTIVATE:
        HandleActivate(thisDlg, LOWORD(wParam) <> WA_INACTIVE);
    |
    WM_SHOWWINDOW:
        IF wParam <> 0 THEN
            IF (lParam = SW_PARENTOPENING) AND (Dialogs[thisDlg].positionWhenRestored) THEN
                Dialogs[thisDlg].positionWhenRestored := FALSE;
                PositionDialogDlg(thisDlg, Dialogs[thisDlg].position);
            END;
        END;
    ELSE
    END;

    SetLastDialog(saveLast);
    RETURN 0;

EXCEPT
    DoExcept(hdlg);
    SetLastDialog(saveLast);
    RETURN 0;
END MyPropSheetDlgProc;

PROCEDURE PositionDialogDlg(dlgNum : ADRCARD; pos : DialogPositions);
VAR
    dlgRect     : RECT;
    resRect     : RECT;
    scrRect     : RECT;
    pt          : POINT;
    base        : POINT;
    scrBase     : POINT;
    dlgWidth    : COORDINATE;
    dlgHeight   : COORDINATE;
    scrX, scrY  : COORDINATE;
    owner       : HWND;
    dlgWnd      : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            position := pos;

            IF pos <> NormalPosition THEN
                dlgWnd := wnd;
                IF propertySheet THEN
                    dlgWnd := GetParent(dlgWnd);
                END;

                owner := NULL_HWND;
                IF pos = CenterOnParent THEN
                    owner := GetParent(dlgWnd);
                END;
                IF owner = NULL_HWND THEN
                    owner := GetDesktopWindow();
                ELSE
                    (*
                    WHILE GetParent(owner) <> NULL_HWND DO
                        owner := GetParent(owner);
                    END;
                    *)

                    IF IsIconic(owner) THEN
                        positionWhenRestored := TRUE;
                        RETURN;
                    END;
                END;

                (* center over the client area if possible *)

                GetClientRect(owner, resRect);
                base.x := resRect.left;
                base.y := resRect.top;
                scrBase := base;
                ClientToScreen(owner, scrBase);
                base := scrBase;

                resRect.left := scrBase.x;
                resRect.top := scrBase.y;
                pt.x := resRect.right;
                pt.y := resRect.bottom;
                ClientToScreen(owner, pt);
                resRect.right := pt.x;
                resRect.bottom := pt.y;

                GetWindowRect(dlgWnd, dlgRect);
                dlgWidth := dlgRect.right - dlgRect.left;
                dlgHeight := dlgRect.bottom - dlgRect.top;

                (* Offset the result and dialog rectangles so that the *)
                (* right and bottom values represent the width and height, *)
                (* and then offset the result again to discard space taken up *)
                (* by the dialog box *)

                OffsetRect(dlgRect, -dlgRect.left, -dlgRect.top);
                OffsetRect(resRect, -resRect.left, -resRect.top);
                OffsetRect(resRect, -dlgRect.right, -dlgRect.bottom);

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
                resRect.right := resRect.left + dlgWidth-1;
                resRect.bottom := resRect.top + dlgHeight-1;

                (* make sure it is all visible on screen *)

                scrX := GetSystemMetrics(SM_CXSCREEN);
                scrY := GetSystemMetrics(SM_CYSCREEN);
                scrRect := resRect;
                IF scrRect.right >= scrX THEN
                    OffsetRect(resRect, -(scrRect.right - scrX), 0);
                END;
                IF scrRect.bottom >= scrY THEN
                    OffsetRect(resRect,  0, -(scrRect.bottom - scrY));
                END;

                SetWindowPos(dlgWnd,
                             NULL_HWND,
                             resRect.left, resRect.top,
                             0, 0,
                             SWP_NOSIZE BOR SWP_NOZORDER BOR SWP_NOACTIVATE);
            END;
        END;
    END;
END PositionDialogDlg;

PROCEDURE PositionDialog(pos : DialogPositions);
BEGIN
    PositionDialogDlg(GetLastDialog(), pos);
END PositionDialog;

PROCEDURE GetDialogSizeDlg(dlgNum : ADRCARD; VAR OUT x, y : COORDINATE);
VAR
    rc  : RECT;
BEGIN
    x := 0;
    y := 0;
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            GetWindowRect(wnd, rc);
            x := rc.right - rc.left;
            y := rc.bottom - rc.top;
        END;
    END;
END GetDialogSizeDlg;

PROCEDURE GetDialogSize(VAR OUT x, y : COORDINATE);
BEGIN
    GetDialogSizeDlg(GetLastDialog(), x, y);
END GetDialogSize;

PROCEDURE GetDefaultButtonDlg(dlgNum : ADRCARD) : CARDINAL;
VAR
    id          : CARDINAL;
    i           : ADRCARD;
    dd          : DWORD;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            IF ctrls <> NIL THEN
                dd := SendMessage(wnd, DM_GETDEFID, 0, 0);
                IF HIWORD(dd) = DC_HASDEFID THEN
                    id := LOWORD(dd);
                    <*/PUSH/NOWARN:U*>
                    FOR i := 0 TO VAL(ADRCARD, numCtrls)-1 DO
                    <*/POP*>
                        IF (ctrls^[i].ct = PushButton) AND (ctrls^[i].id = id) THEN
                            RETURN GetUserControlId(dlgNum, i);
                        END;
                    END;
                END;
            END;
        END;
    END;
    RETURN MAX(CARDINAL);
END GetDefaultButtonDlg;

PROCEDURE GetDefaultButton() : CARDINAL;
BEGIN
    RETURN GetDefaultButtonDlg(GetLastDialog());
END GetDefaultButton;

PROCEDURE SetDefaultButtonDlg(dlgNum : ADRCARD; index : ADRCARD);
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                IF ctrls^[index].ct = PushButton THEN
                    FUNC SendMessage(wnd, DM_SETDEFID, ctrls^[index].id, 0);
                END;
            END;
        END;
    END;
END SetDefaultButtonDlg;

PROCEDURE SetDefaultButton(index : CARDINAL);
BEGIN
    SetDefaultButtonDlg(GetLastDialog(), index);
END SetDefaultButton;

PROCEDURE SetPaintDlg(dlgNum : ADRCARD; index : ADRCARD; on : BOOLEAN);
VAR
    ctrlWnd     : HWND;
    i           : CARDINAL;
    first, last : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                IF ctrls^[index].ct <> RadioGroup THEN
                    ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                    IF on THEN
                        SendMessage(ctrlWnd, WM_SETREDRAW, ORD(TRUE), 0);
                        InvalidateRect(ctrlWnd, NIL_RECT, TRUE);
                    ELSE
                        SendMessage(ctrlWnd, WM_SETREDRAW, ORD(FALSE), 0);
                    END;
                ELSE
                    first := ctrls^[index].r_first;
                    last := ctrls^[index].r_last;
                    FOR i := first TO last DO
                        ctrlWnd := GetDlgItem(wnd, i);

                        IF on THEN
                            SendMessage(ctrlWnd, WM_SETREDRAW, ORD(TRUE), 0);
                            InvalidateRect(ctrlWnd, NIL_RECT, TRUE);
                        ELSE
                            SendMessage(ctrlWnd, WM_SETREDRAW, ORD(FALSE), 0);
                        END;
                    END;
                END;
            END;
        END;
    END;
END SetPaintDlg;

PROCEDURE SetPaint(index : CARDINAL; on : BOOLEAN);
BEGIN
    SetPaintDlg(GetLastDialog(), index, on);
END SetPaint;

PROCEDURE SetEnableDlg(dlgNum : ADRCARD; index : ADRCARD; enabled : BOOLEAN);
VAR
    i           : CARDINAL;
    first, last : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                IF ctrls^[index].ct <> RadioGroup THEN
                    EnableWindow(GetDlgItem(wnd, ctrls^[index].id), enabled);
                ELSE
                    first := ctrls^[index].r_first;
                    last := ctrls^[index].r_last;
                    FOR i := first TO last DO
                        EnableWindow(GetDlgItem(wnd, i), enabled);
                    END;
                END;
            END;
        END;
    END;
END SetEnableDlg;

PROCEDURE SetEnable(index : CARDINAL; enabled : BOOLEAN);
BEGIN
    SetEnableDlg(GetLastDialog(), index, enabled);
END SetEnable;

PROCEDURE SetEnableRangeDlg(dlgNum : CARDINAL; first, last : CARDINAL; enabled : BOOLEAN);
VAR
    i   : CARDINAL;
BEGIN
    FOR i := first TO last DO
        SetEnableDlg(dlgNum, i, enabled);
    END;
END SetEnableRangeDlg;

PROCEDURE SetEnableRange(first, last : CARDINAL; enabled : BOOLEAN);
BEGIN
    SetEnableRangeDlg(GetLastDialog(), first, last, enabled);
END SetEnableRange;

PROCEDURE SetEnableRadioDlg(dlgNum : ADRCARD; index : ADRCARD; radioControl : CARDINAL; enabled : BOOLEAN);
VAR
    ctrlNum     : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                IF ctrls^[index].ct = RadioGroup THEN
                    ctrlNum := ctrls^[index].r_first + radioControl;
                    IF ctrlNum <= ctrls^[index].r_last THEN
                        EnableWindow(GetDlgItem(wnd, ctrlNum), enabled);
                    END;
                END;
            END;
        END;
    END;
END SetEnableRadioDlg;

PROCEDURE SetEnableRadio(index : CARDINAL; radioControl : CARDINAL; enabled : BOOLEAN);
BEGIN
    SetEnableRadioDlg(GetLastDialog(), index, radioControl, enabled);
END SetEnableRadio;

PROCEDURE SetVisibleDlg(dlgNum : ADRCARD; index : ADRCARD; visible : BOOLEAN);
VAR
    i           : CARDINAL;
    first, last : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                IF ctrls^[index].ct <> RadioGroup THEN
                    ShowWindow(GetDlgItem(wnd, ctrls^[index].id), Show[visible]);
                ELSE
                    first := ctrls^[index].r_first;
                    last := ctrls^[index].r_last;
                    FOR i := first TO last DO
                        ShowWindow(GetDlgItem(wnd, i), Show[visible]);
                    END;
                END;
            END;
        END;
    END;
END SetVisibleDlg;

PROCEDURE SetVisible(index : CARDINAL; visible : BOOLEAN);
BEGIN
    SetVisibleDlg(GetLastDialog(), index, visible);
END SetVisible;

PROCEDURE SetVisibleRadioDlg(dlgNum : ADRCARD; index : ADRCARD; item : CARDINAL; visible : BOOLEAN);
VAR
    ctrlWnd     : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND (ctrls^[index].ct = RadioGroup) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].r_first+item);
                ShowWindow(ctrlWnd, Show[visible]);
            END;
        END;
    END;
END SetVisibleRadioDlg;

PROCEDURE SetVisibleRadio(index, item : CARDINAL; visible : BOOLEAN);
BEGIN
    SetVisibleRadioDlg(GetLastDialog(), index, item, visible);
END SetVisibleRadio;

PROCEDURE SetColorAndFontDlg(dlgNum : ADRCARD;
                             index : ADRCARD;
                             fore, back : ColorValue;
                             font : FontInfoPointer);
VAR
    stIntPtr    : StaticTextInternalPointer;
    ctrlWnd     : HWND;
    hf          : HFONT;
    lf          : LOGFONT;
    temp        : FontInfo;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                CASE ctrls^[index].ct OF
                StaticTextAttrib:
                    ctrls^[index].sta_fore := fore;
                    ctrls^[index].sta_back := back;

                    ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);
                    stIntPtr := FindInternalData(dlgNum, ADR(ctrls^[index]));
                    IF (ctrlWnd <> NIL) AND (font <> NIL) AND (stIntPtr <> NIL) THEN
                        temp := font^;

                        hf := GetWindowFont(ctrlWnd);
                        IF GetLOGFONT(hf, lf) THEN
                            temp.height := lf.lfHeight;
                        ELSE
                            temp.height := 8;
                        END;

                        hf := CAST(HFONT, WinShell.LoadFont(temp));

                        SendMessage(ctrlWnd, WM_SETFONT, CAST(WPARAM, hf), ORD(TRUE));

                        IF stIntPtr^.font <> NULL_HFONT THEN
                            DeleteFont(stIntPtr^.font);
                            stIntPtr^.font := NULL_HFONT;
                        END;
                        stIntPtr^.font := hf;
                    END;

                    SetPaintDlg(dlgNum, index, TRUE);
                ELSE
                END;
            END;
        END;
    END;
END SetColorAndFontDlg;

PROCEDURE SetColorAndFont(index : CARDINAL; fore, back : ColorValue; font : FontInfoPointer);
BEGIN
    SetColorAndFontDlg(GetLastDialog(), index, fore, back, font);
END SetColorAndFont;

PROCEDURE SetCtrlFocus(hdlg : HWND; hwnd : HWND);
BEGIN
    (*
    UNREFERENCED_PARAMETER(hdlg);
    SetFocus(hwnd);
    *)
    (**)
    PostMessage(hdlg, WM_NEXTDLGCTL, CAST(WPARAM, hwnd), ORD(TRUE));
    (**)
END SetCtrlFocus;

PROCEDURE SetInputFocusToDlg(dlgNum : ADRCARD; index : ADRCARD);
VAR
    id  : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                id := ctrls^[index].id;
                IF ctrls^[index].ct = RadioGroup THEN
                    id := id + ctrls^[index].r_first;
                END;

                SetCtrlFocus(wnd, GetDlgItem(wnd, id));
            END;
        END;
    END;
END SetInputFocusToDlg;

PROCEDURE SetInputFocusTo(index : CARDINAL);
BEGIN
    SetInputFocusToDlg(GetLastDialog(), index);
END SetInputFocusTo;

PROCEDURE GetInputFocusControlDlg(dlgNum : ADRCARD) : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        RETURN Dialogs[dlgNum].lastFocus;
    END;
    RETURN MAX(CARDINAL);
END GetInputFocusControlDlg;

PROCEDURE GetInputFocusControl() : CARDINAL;
BEGIN
    RETURN GetInputFocusControlDlg(GetLastDialog());
END GetInputFocusControl;

PROCEDURE StringMatchWndProc(wnd : HWND;
                             msg : UINT;
                             wParam : WPARAM;
                             lParam : LPARAM) : LRESULT [WINDOWS, EXPORT];
CONST
    MatchTimeout        = 1250;
VAR
    sel         : INTEGER;
    info        : MatchListInfoPointer;
    keyTime     : CARDINAL;

    PROCEDURE findMatch(info : MatchListInfoPointer; ch : CHAR) : INTEGER;
    VAR
        i, j, k, lm, lt : ADRCARD;
        count           : ADRCARD;
        text            : ARRAY [0..255] OF CHAR;
        (*sel             : INTEGER;*)
    BEGIN
        CASE info^.ctrlType OF
        ListBox:
            count := ListBox_GetCount(info^.wnd);
        |
        ColumnListBox:
            count := ListView_GetItemCount(info^.wnd);
        ELSE
            info^.matchStr := "";
            RETURN -1;
        END;

        AppendChar(CAP(ch), info^.matchStr);
        lm := LENGTH(info^.matchStr);
        i := 0;
        WHILE i < count DO
            CASE info^.ctrlType OF
            ListBox:
                ListBox_GetText(info^.wnd, i, text);
            |
            ColumnListBox:
                ListView_GetItemText(info^.wnd,
                                     i,
                                     info^.column,
                                     text,
                                     SIZE(MatchString)/SIZE(CHAR));
            ELSE
                RETURN -1;
            END;
            lt := LENGTH(text);

            j := 0;
            k := info^.startPos;
            WHILE (j < lm) AND (k < lt) AND (info^.matchStr[j] = CAP(text[k])) DO
                INC(j);
                INC(k);
            END;
            IF j = lm THEN
                RETURN i;
            END;

            INC(i);
        END;

        info^.matchStr := "";
        RETURN -1;
    END findMatch;

BEGIN
    info := CAST(MatchListInfoPointer, GetWindowLongPtr(wnd, GWL_USERDATA));
    IF info <> NIL THEN
        IF (msg = WM_CHAR) AND (CHR(wParam) >= ' ') THEN
            keyTime := GetTickCount();
            IF keyTime-info^.lastKeyTime >= MatchTimeout THEN
                info^.matchStr := "";
            END;
            info^.lastKeyTime := keyTime;

            sel := findMatch(info, CHR(wParam));
            IF sel >= 0 THEN
                CASE info^.ctrlType OF
                ListBox:
                    ListBox_SetCurSel(info^.wnd, sel);
                |
                ColumnListBox:
                    ListView_EnsureVisible(info^.wnd, sel, TRUE);
                    ListView_SetItemState(info^.wnd,
                                          sel,
                                          LVIS_SELECTED BOR LVIS_FOCUSED,
                                          LVIS_SELECTED BOR LVIS_FOCUSED);
                ELSE
                END;
            END;
            RETURN 0;
        END;
    END;

    IF info <> NIL THEN
        RETURN CallWindowProc(info^.oldWndProc, wnd, msg, wParam, lParam);
    ELSE
        BasicDialogs.MessageBox("info not found in KeyMatchWndProc", BasicDialogs.MsgAppError);
        RETURN 0;
    END;
END StringMatchWndProc;

PROCEDURE SetupStringMatchDlg(dlgNum : ADRCARD; index : ADRCARD; startPos : CARDINAL; column : CARDINAL);
VAR
    ctrlWnd     : HWND;
    info        : MatchListInfoPointer;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                IF (ctrls^[index].ct = ListBox) OR (ctrls^[index].ct = ColumnListBox) THEN
                    IF GetWindowLong(wnd, GWL_USERDATA) = 0 THEN
                        ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                        info := AllocateInternalData(dlgNum, ctrlWnd, SIZE(info^));

                        SetWindowLongPtr(ctrlWnd, GWL_USERDATA, CAST(DWORD, info));

                        info^.wnd := ctrlWnd;
                        info^.ctrlType := ctrls^[index].ct;
                        info^.matchStr := "";
                        info^.column := column;
                        info^.startPos := startPos;
                        info^.lastKeyTime := 0;
                        info^.oldWndProc := SubclassWindow(ctrlWnd, StringMatchWndProc);
                    END;
                END;
            END;
        END;
    END;
END SetupStringMatchDlg;

PROCEDURE SetupStringMatch(index : CARDINAL; startPos : CARDINAL; column : CARDINAL);
BEGIN
    SetupStringMatchDlg(GetLastDialog(), index, startPos, column);
END SetupStringMatch;

PROCEDURE SetCheckBoxDlg(dlgNum : ADRCARD; index : ADRCARD; on : BOOLEAN);
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (ctrls^[index].ct = CheckBox) AND
               (ctrls^[index].chk_on <> on)
            THEN
            <*/POP*>
                ctrls^[index].chk_on := on;

                Button_SetCheck(GetDlgItem(wnd, ctrls^[index].id), ORD(ctrls^[index].chk_on));
            END;
        END;
    END;
END SetCheckBoxDlg;

PROCEDURE SetCheckBox(index : CARDINAL; on : BOOLEAN);
BEGIN
    SetCheckBoxDlg(GetLastDialog(), index, on);
END SetCheckBox;

PROCEDURE GetCheckBoxDlg(dlgNum : ADRCARD; index : ADRCARD) : BOOLEAN;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (ctrls^[index].ct = CheckBox)
            THEN
            <*/POP*>
                RETURN ctrls^[index].chk_on;
            END;
        END;
    END;
    RETURN FALSE;
END GetCheckBoxDlg;

PROCEDURE GetCheckBox(index : CARDINAL) : BOOLEAN;
BEGIN
    RETURN GetCheckBoxDlg(GetLastDialog(), index);
END GetCheckBox;

PROCEDURE SetRadioGroupDlg(dlgNum : ADRCARD; index : ADRCARD; item : CARDINAL);
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND (ctrls^[index].ct = RadioGroup) THEN
            <*/POP*>
                IF (ctrls^[index].id <> item) AND
                   (item <= (ctrls^[index].r_last-ctrls^[index].r_first))
                THEN
                    ctrls^[index].id := item;

                    CheckRadioButton(wnd, ctrls^[index].r_first, ctrls^[index].r_last, ctrls^[index].r_first+item);
                END;
            END;
        END;
    END;
END SetRadioGroupDlg;

PROCEDURE SetRadioGroup(index : CARDINAL; item : CARDINAL);
BEGIN
    SetRadioGroupDlg(GetLastDialog(), index, item);
END SetRadioGroup;

PROCEDURE GetRadioGroupDlg(dlgNum : ADRCARD; index : ADRCARD) : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND (ctrls^[index].ct = RadioGroup) THEN
            <*/POP*>
                RETURN ctrls^[index].id;
            END;
        END;
    END;
    RETURN MAX(CARDINAL);
END GetRadioGroupDlg;

PROCEDURE GetRadioGroup(index : CARDINAL) : CARDINAL;
BEGIN
    RETURN GetRadioGroupDlg(GetLastDialog(), index);
END GetRadioGroup;

PROCEDURE SetRadioTextDlg(dlgNum : ADRCARD; index : ADRCARD; item : CARDINAL; text : ARRAY OF CHAR);
VAR
    ctrlWnd     : HWND;
    str         : ARRAY [0..255] OF CHAR;
    i, l        : ADRCARD;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (ctrls^[index].ct = RadioGroup) AND
               (item <= (ctrls^[index].r_last-ctrls^[index].r_first))
            THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].r_first+item);

                str := text;
                str[HIGH(str)] := '';
                l := LENGTH(str);
                IF l > 0 THEN
                    FOR i := 0 TO l-1 DO
                        IF str[i] = '_' THEN
                            str[i] :=  '&';
                        END;
                    END;
                END;

                Button_SetText(ctrlWnd, str);
            END;
        END;
    END;
END SetRadioTextDlg;

PROCEDURE SetRadioText(index, item : CARDINAL; text : ARRAY OF CHAR);
BEGIN
    SetRadioTextDlg(GetLastDialog(), index, item, text);
END SetRadioText;

PROCEDURE GetRadioTextDlg(dlgNum : ADRCARD; index : ADRCARD; item : CARDINAL; VAR OUT text : ARRAY OF CHAR);
VAR
    ctrlWnd     : HWND;
BEGIN
    text := "";

    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (ctrls^[index].ct = RadioGroup) AND
               (item <= (ctrls^[index].r_last-ctrls^[index].r_first))
            THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].r_first+item);
                Button_GetText(ctrlWnd, text, HIGH(text)+1);
            END;
        END;
    END;
END GetRadioTextDlg;

PROCEDURE GetRadioText(index, item : CARDINAL; VAR OUT text : ARRAY OF CHAR);
BEGIN
    GetRadioTextDlg(GetLastDialog(), index, item, text);
END GetRadioText;

PROCEDURE SetStockButtonDlg(dlgNum : ADRCARD; control : CARDINAL; typ : StockButtonType);
TYPE
    string      = ARRAY [0..15] OF CHAR;
CONST
    win_stock   : ARRAY StockButtonType OF string =
        {"OK", "Cancel"};
VAR
    ctrlW       : HWND;
    index       : ADRCARD;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, control);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND (ctrls^[index].ct = PushButton) THEN
            <*/POP*>
                ctrlW := GetDlgItem(wnd, ctrls^[index].id);
                Button_SetText(ctrlW, win_stock[typ]);
            END;
        END;
    END;
END SetStockButtonDlg;

PROCEDURE SetStockButton(control : CARDINAL; typ : StockButtonType);
BEGIN
    SetStockButtonDlg(GetLastDialog(), control, typ);
END SetStockButton;

PROCEDURE SetTimerIntervalDlg(dlgNum : ADRCARD; index : ADRCARD; time : CARDINAL);
VAR
    oldInterval : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND (ctrls^[index].ct = Timer) THEN
            <*/POP*>
                oldInterval := ctrls^[index].timerInterval;
                ctrls^[index].timerInterval := time;

                IF time <> 0 THEN
                    SetATimer(wnd, ctrls^[index].id, time);
                ELSE
                    IF oldInterval <> 0 THEN
                        KillATimer(wnd, ctrls^[index].id);
                    END;
                END;
            END;
        END;
    END;
END SetTimerIntervalDlg;

PROCEDURE SetTimerInterval(index : CARDINAL; time : CARDINAL);
BEGIN
    SetTimerIntervalDlg(GetLastDialog(), index, time);
END SetTimerInterval;

PROCEDURE SetTextDlg(dlgNum : ADRCARD; index : ADRCARD; text : ARRAY OF CHAR) : BOOLEAN;
VAR
    ctrlWnd     : HWND;
    ret         : INTEGER;
    idx         : CARDINAL;
    str         : ARRAY [0..127] OF CHAR;
    i, l        : ADRCARD;
    lvFind      : LV_FINDINFO;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index >= VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                RETURN FALSE;
            END;

            ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

            CASE ctrls^[index].ct OF
            LineEdit:
                IF ctrls^[index].le_text <> NIL THEN
                    StrAssign(text, ctrls^[index].le_text^, ctrls^[index].le_max);
                END;
                Edit_SetText(ctrlWnd, text);
            |
            NumberEdit:
                Edit_SetText(ctrlWnd, text);
            |
            MultiLineEdit:
                IF (ctrls^[index].mle_text <> NIL) AND (ctrls^[index].mle_max <> 0) THEN
                    StrAssign(text, ctrls^[index].mle_text^, ctrls^[index].mle_max);
                END;
                Edit_SetText(ctrlWnd, text);
            |
            EditTextFile:
                Edit_SetText(ctrlWnd, text);
            |
            StaticText, StaticTextAttrib:
                Static_SetText(ctrlWnd, text);
            |
            ListBox:
                ret := ListBox_SelectString(ctrlWnd, -1, text);

                IF ret = LB_ERR THEN
                    IF ctrls^[index].lb_selStr <> NIL THEN
                        ctrls^[index].lb_selStr^[0] := '';
                    END;
                    ctrls^[index].lb_selIndex := MAX(CARDINAL);
                ELSE
                    ctrls^[index].lb_selIndex := ret;
                    IF ctrls^[index].lb_selStr <> NIL THEN
                        ListBox_GetText(ctrlWnd, ret, ctrls^[index].lb_selStr^);
                    END;

                    IF NOT MultiSelectListBox(ctrlWnd) THEN
                        ListBox_SetCurSel(ctrlWnd, ret);
                    ELSE
                        ListBox_SetSel(ctrlWnd, TRUE, ret);
                    END;
                END;
            |
            ColumnListBox:
                lvFind.flags := LVFI_STRING BOR LVFI_PARTIAL;
                lvFind.psz := ADR(text);
                ret := ListView_FindItem(ctrlWnd, -1, lvFind);
                IF ret >= 0 THEN
                    ListView_EnsureVisible(ctrlWnd, ret, TRUE);
                    RETURN ListView_SetItemState(ctrlWnd,
                                                 ret,
                                                 LVIS_SELECTED BOR LVIS_FOCUSED,
                                                 LVIS_SELECTED BOR LVIS_FOCUSED);
                END;
                RETURN FALSE;
            |
            ComboBox:
                IF ctrls^[index].cb_selStr <> NIL THEN
                    StrAssign(text, ctrls^[index].cb_selStr^, ctrls^[index].cb_max);
                END;

                IF ComboBox_FindString(ctrlWnd, -1, text, idx) THEN
                    ComboBox_SetCurSel(ctrlWnd, idx);
                    (*ctrls^[index].cb_selIndex := ret;*)
                END;

                ComboBox_SetText(ctrlWnd, text);
            |
            PushButton, CheckBox:
                str := text;
                str[HIGH(str)] := '';
                l := LENGTH(str);
                IF l > 0 THEN
                    FOR i := 0 TO l-1 DO
                        IF str[i] = '_' THEN
                            str[i] :=  '&';
                        END;
                    END;
                END;
                Button_SetText(ctrlWnd, str);
            |
            DialogTitle:
                SetWindowText(wnd, text);
            ELSE
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END SetTextDlg;

PROCEDURE SetText(index : CARDINAL; text : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN SetTextDlg(GetLastDialog(), index, text);
END SetText;

PROCEDURE GetTextDlg(dlgNum : ADRCARD; index : ADRCARD; VAR OUT text : ARRAY OF CHAR) : BOOLEAN;
VAR
    ctrlWnd     : HWND;
    ret         : INTEGER;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index >= VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                RETURN FALSE;
            END;

            ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

            CASE ctrls^[index].ct OF
            LineEdit:
                IF ctrls^[index].le_text <> NIL THEN
                    Edit_GetText(ctrlWnd, ctrls^[index].le_text^, ctrls^[index].le_max+1);
                END;

                Edit_GetText(ctrlWnd, text, HIGH(text)+1);
            |
            NumberEdit:
                Edit_GetText(ctrlWnd, text, HIGH(text)+1);
            |
            MultiLineEdit:
                IF (ctrls^[index].mle_text <> NIL) AND (ctrls^[index].mle_max <> 0) THEN
                    Edit_GetText(ctrlWnd, ctrls^[index].mle_text^, ctrls^[index].mle_max+1);
                END;
                Edit_GetText(ctrlWnd, text, HIGH(text)+1);
            |
            EditTextFile:
                Edit_GetText(ctrlWnd, text, HIGH(text)+1);
            |
            ListBox:
                ret := ListBox_GetCurSel(ctrlWnd);
                IF ret <> LB_ERR THEN
                    ctrls^[index].lb_selIndex := ret;
                ELSE
                    ctrls^[index].lb_selIndex := MAX(CARDINAL);
                END;

                text := "";
                IF ctrls^[index].lb_selIndex <> MAX(CARDINAL) THEN
                    IF ctrls^[index].lb_selStr <> NIL THEN
                        ListBox_GetText(ctrlWnd, ctrls^[index].lb_selIndex, ctrls^[index].lb_selStr^);

                        StrAssign(ctrls^[index].lb_selStr^, text, HIGH(text)+1);
                    ELSE
                        ListBox_GetText(ctrlWnd, ctrls^[index].lb_selIndex, text);
                    END;
                END;
            |
            DropDownList:
                ComboBox_GetText(ctrlWnd, text, HIGH(text)+1);
            |
            ComboBox:
                IF ctrls^[index].cb_selStr <> NIL THEN
                    ComboBox_GetText(ctrlWnd, ctrls^[index].cb_selStr^, ctrls^[index].cb_max+1);

                END;
                ComboBox_GetText(ctrlWnd, text, HIGH(text)+1);
            |
            StaticText:
                Static_GetText(ctrlWnd, text, HIGH(text)+1);
            |
            PushButton, CheckBox:
                Button_GetText(ctrlWnd, text, HIGH(text)+1);
            ELSE
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END GetTextDlg;

PROCEDURE GetText(index : CARDINAL; VAR OUT text : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN GetTextDlg(GetLastDialog(), index, text);
END GetText;

PROCEDURE SetNumericValueDlg(dlgNum : ADRCARD; index : ADRCARD; value : INTEGER) : BOOLEAN;
VAR
    ctrlWnd     : HWND;
    text        : ARRAY [0..15] OF CHAR;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index >= VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                RETURN FALSE;
            END;

            IntToStr(value, text);

            ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

            CASE ctrls^[index].ct OF
            LineEdit:
                IF ctrls^[index].le_text <> NIL THEN
                    StrAssign(text, ctrls^[index].le_text^, ctrls^[index].le_max);
                END;
                Edit_SetText(ctrlWnd, text);
            |
            NumberEdit:
                IF ctrls^[index].number_Hex THEN
                    CardBaseToStr(value, 16, text);
                    AppendChar('h', text);
                END;
                Edit_SetText(ctrlWnd, text);
            |
            StaticText, StaticTextAttrib:
                Static_SetText(ctrlWnd, text);
            |
            SpinButton:
                IF (value >= ctrls^[index].spin_Min) AND (value <= ctrls^[index].spin_Max) THEN
                    ctrls^[index].spin_Val := value;
                    UpDown_SetPos(ctrlWnd, value);
                    ctrlWnd := UpDown_GetBuddy(ctrlWnd);
                    Edit_SetText(ctrlWnd, text);
                ELSE
                    RETURN FALSE;
                END;
            ELSE
                RETURN FALSE;
            END;
        END;
    END;
    RETURN TRUE;
END SetNumericValueDlg;

PROCEDURE SetNumericValue(index : CARDINAL; value : INTEGER) : BOOLEAN;
BEGIN
    RETURN SetNumericValueDlg(GetLastDialog(), index, value);
END SetNumericValue;

PROCEDURE GetSpinButtonValueDlg(dlgNum : ADRCARD; index : ADRCARD) : INTEGER;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND (ctrls^[index].ct = SpinButton) THEN
            <*/POP*>
                RETURN ctrls^[index].spin_Val;
            END;
        END;
    END;
    RETURN MIN(INTEGER);
END GetSpinButtonValueDlg;

PROCEDURE GetSpinButtonValue(index : CARDINAL) : INTEGER;
BEGIN
    RETURN GetSpinButtonValueDlg(GetLastDialog(), index);
END GetSpinButtonValue;

PROCEDURE SetSpinButtonRangeDlg(dlgNum : ADRCARD; index : ADRCARD; min, max : INTEGER);
VAR
    ctrlWnd     : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND (ctrls^[index].ct = SpinButton) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                ctrls^[index].spin_Min := min;
                ctrls^[index].spin_Max := max;
                UpDown_SetRange32(ctrlWnd, min, max);
            END;
        END;
    END;
END SetSpinButtonRangeDlg;

PROCEDURE SetSpinButtonRange(index : CARDINAL; min, max : INTEGER);
BEGIN
    SetSpinButtonRangeDlg(GetLastDialog(), index, min, max);
END SetSpinButtonRange;

PROCEDURE SetItemTextDlg(dlgNum : ADRCARD; index : ADRCARD; item, column : CARDINAL; text : ARRAY OF CHAR) : BOOLEAN;
VAR
    retVal      : INTEGER;
    saveSel     : INTEGER;
    idx         : CARDINAL;
    ok          : BOOLEAN;
    ctrlWnd     : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    SetPaint(index, FALSE);
                    saveSel := ListBox_GetCurSel(ctrlWnd);
                    ListBox_DeleteString(ctrlWnd, item);
                    ok := ListBox_InsertString(ctrlWnd, item, text, idx);
                    SetPaint(index, TRUE);
                    IF ok THEN
                        IF INT(index) = saveSel THEN
                            IF NOT MultiSelectListBox(ctrlWnd) THEN
                                ListBox_SetCurSel(ctrlWnd, idx);
                            ELSE
                                ListBox_SetSel(ctrlWnd, TRUE, idx);
                            END;
                        END;
                        RETURN TRUE;
                    END;
                |
                ColumnListBox:
                    RETURN ListView_SetItemText(ctrlWnd, item, column, text);
                |
                ComboBox, DropDownList:
                    SetPaint(index, FALSE);
                    saveSel := ListBox_GetCurSel(ctrlWnd);
                    ComboBox_DeleteString(ctrlWnd, item);
                    retVal := ComboBox_InsertString(ctrlWnd, item, text);
                    SetPaint(index, TRUE);
                    IF (retVal <> CB_ERR) AND (retVal <> CB_ERRSPACE) THEN
                        IF INT(index) = saveSel THEN
                            ComboBox_SetCurSel(ctrlWnd, retVal);
                        END;
                        RETURN TRUE;
                    END;
                ELSE
                END;
            END;
        END;
    END;
    RETURN FALSE;
END SetItemTextDlg;

PROCEDURE SetItemText(index, item, column : CARDINAL; text : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN SetItemTextDlg(GetLastDialog(), index, item, column, text);
END SetItemText;

PROCEDURE GetItemTextDlg(dlgNum : ADRCARD; index : ADRCARD; item, column : CARDINAL; VAR OUT text : ARRAY OF CHAR) : BOOLEAN;
VAR
    ctrlWnd     : HWND;
    count       : INTEGER;
BEGIN
    text[0] := '';

    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    count := ListBox_GetCount(ctrlWnd);
                    IF INT(item) < count THEN
                        ListBox_GetText(ctrlWnd, item, text);
                        RETURN TRUE;
                    END;
                |
                ColumnListBox:
                    count := ListView_GetItemCount(ctrlWnd);
                    IF INT(item) < count THEN
                        ListView_GetItemText(ctrlWnd, item, column, text, HIGH(text)+1);
                        RETURN TRUE;
                    END;
                |
                ComboBox, DropDownList:
                    count := ComboBox_GetCount(ctrlWnd);
                    IF INT(item) < count THEN
                        ComboBox_GetLBText(ctrlWnd, item, text);
                        RETURN TRUE;
                    END;
                ELSE
                END;
            END;
        END;
    END;
    RETURN FALSE;
END GetItemTextDlg;

PROCEDURE GetItemText(index, item, column : CARDINAL; VAR OUT text : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN GetItemTextDlg(GetLastDialog(), index, item, column, text);
END GetItemText;

PROCEDURE SetTextSelectionDlg(dlgNum : ADRCARD; index : ADRCARD; startPos, endPos : CARDINAL) : BOOLEAN;
VAR
    ctrlWnd     : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                LineEdit, NumberEdit, MultiLineEdit, EditTextFile:
                    Edit_SetSel(ctrlWnd, startPos, endPos);
                    RETURN TRUE;
                |
                ComboBox:
                    RETURN ComboBox_SetEditSel(ctrlWnd, startPos, endPos);
                ELSE
                END;
            END;
        END;
    END;
    RETURN FALSE;
END SetTextSelectionDlg;

PROCEDURE SetTextSelection(index : CARDINAL;
                           startPos, endPos : CARDINAL) : BOOLEAN;
BEGIN
    RETURN SetTextSelectionDlg(GetLastDialog(), index, startPos, endPos);
END SetTextSelection;

PROCEDURE ClearTextSelectionDlg(dlgNum : ADRCARD; index : ADRCARD);
VAR
    ctrlWnd     : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                LineEdit, NumberEdit, MultiLineEdit, EditTextFile:
                    Edit_SetSel(ctrlWnd, -1, -1);
                |
                ComboBox:
                    ComboBox_SetEditSel(ctrlWnd, -1, -1);
                ELSE
                END;
            END;
        END;
    END;
END ClearTextSelectionDlg;

PROCEDURE ClearTextSelection(index : CARDINAL);
BEGIN
    ClearTextSelectionDlg(GetLastDialog(), index);
END ClearTextSelection;

PROCEDURE SetTextCaretPosDlg(dlgNum : ADRCARD; index : ADRCARD; pos : INTEGER);
VAR
    ctrlWnd     : HWND;
    line        : CARDINAL;
    chIndex     : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                LineEdit, NumberEdit:
                    Edit_SetSel(ctrlWnd, pos, pos);
                    Edit_ScrollCaret(ctrlWnd);
                |
                MultiLineEdit, EditTextFile:
                    IF pos >= 0 THEN
                        line := 1;
                    ELSE
                        line := Edit_GetLineCount(ctrlWnd);
                    END;
                    chIndex := Edit_LineIndex(ctrlWnd, line-1);
                    Edit_SetSel(ctrlWnd, chIndex, chIndex);
                    Edit_ScrollCaret(ctrlWnd);
                |
                ComboBox:
                    Edit_SetSel(ctrlWnd, pos, pos);
                    Edit_ScrollCaret(ctrlWnd);
                ELSE
                END;
            END;
        END;
    END;
END SetTextCaretPosDlg;

PROCEDURE SetTextCaretPos(control : CARDINAL; pos : INTEGER);
BEGIN
    SetTextCaretPosDlg(GetLastDialog(), control, pos);
END SetTextCaretPos;

PROCEDURE GetItemCountDlg(dlgNum : ADRCARD; index : ADRCARD) : CARDINAL;
VAR
    sel         : CARDINAL;
    ctrlWnd     : HWND;
BEGIN
    sel := MAX(CARDINAL);
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    sel := ListBox_GetCount(ctrlWnd);
                |
                ComboBox, ColorDropDownList, DropDownList:
                    sel := ComboBox_GetCount(ctrlWnd);
                |
                ColumnListBox:
                    sel := ListView_GetItemCount(ctrlWnd);
                ELSE
                END;
            END;
        END;
    END;
    RETURN sel;
END GetItemCountDlg;

PROCEDURE GetItemCount(index : CARDINAL) : CARDINAL;
BEGIN
    RETURN GetItemCountDlg(GetLastDialog(), index);
END GetItemCount;

PROCEDURE GetSelectedDlg(dlgNum : ADRCARD; index : ADRCARD) : CARDINAL;
VAR
    sel         : CARDINAL;
    ret         : INTEGER;
    ctrlWnd     : HWND;
BEGIN
    sel := MAX(CARDINAL);
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    ret := ListBox_GetCurSel(ctrlWnd);
                    IF ret <> LB_ERR THEN
                        sel := ret;
                    END;
                |
                ColumnListBox:
                    ret := ListView_GetNextItem(ctrlWnd, -1, LVNI_SELECTED);
                    IF ret <> -1 THEN
                        sel := ret;
                    END;
                |
                ComboBox, ColorDropDownList, DropDownList:
                    ret := ComboBox_GetCurSel(ctrlWnd);
                    IF ret <> CB_ERR THEN
                        sel := ret;
                    END;
                ELSE
                END;
            END;
        END;
    END;
    RETURN sel;
END GetSelectedDlg;

PROCEDURE GetSelected(index : CARDINAL) : CARDINAL;
BEGIN
    RETURN GetSelectedDlg(GetLastDialog(), index);
END GetSelected;

PROCEDURE SetSelectedDlg(dlgNum : ADRCARD;
                         index : ADRCARD;
                         sel : CARDINAL) : BOOLEAN;
VAR
    retVal      : INTEGER;
    ctrlWnd     : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    IF NOT MultiSelectListBox(ctrlWnd) THEN
                        retVal := ListBox_SetCurSel(ctrlWnd, sel);
                    ELSE
                        retVal := ListBox_SetSel(ctrlWnd, TRUE, sel);
                    END;
                    RETURN retVal <> LB_ERR;
                |
                ColumnListBox:
                    ListView_EnsureVisible(ctrlWnd, sel, TRUE);
                    RETURN ListView_SetItemState(ctrlWnd,
                                                 sel,
                                                 LVIS_SELECTED BOR LVIS_FOCUSED,
                                                 LVIS_SELECTED BOR LVIS_FOCUSED);
                |
                ComboBox, ColorDropDownList, DropDownList:
                    retVal := ComboBox_SetCurSel(ctrlWnd, sel);
                    RETURN retVal <> CB_ERR;
                ELSE
                END;
            END;
        END;
    END;
    RETURN FALSE;
END SetSelectedDlg;

PROCEDURE SetSelected(index : CARDINAL; sel : CARDINAL) : BOOLEAN;
BEGIN
    RETURN SetSelectedDlg(GetLastDialog(), index, sel);
END SetSelected;

PROCEDURE GetSelectedItemsCountDlg(dlgNum : ADRCARD; index : ADRCARD) : CARDINAL;
VAR
    ctrlWnd     : HWND;
    count       : INTEGER;
BEGIN
    count := 0;
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    count := ListBox_GetSelCount(ctrlWnd);
                    IF count < 0 THEN
                        count := 0;
                    END;
                |
                ColumnListBox:
                    count := ListView_GetSelectedCount(ctrlWnd);
                ELSE
                END;
            END;
        END;
    END;
    RETURN count;
END GetSelectedItemsCountDlg;

PROCEDURE GetSelectedItemsCount(index : CARDINAL) : CARDINAL;
BEGIN
    RETURN GetSelectedItemsCountDlg(GetLastDialog(), index);
END GetSelectedItemsCount;

PROCEDURE GetSelectedItemsDlg(dlgNum : ADRCARD; index : ADRCARD; VAR OUT items : ARRAY OF CARDINAL) : CARDINAL;
VAR
    ctrlWnd     : HWND;
    count       : CARDINAL;
    listItems   : POINTER TO ARRAY OF INTEGER;
    i           : ADRCARD;
    sel         : INTEGER;
BEGIN
    count := 0;
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    count := ListBox_GetSelCount(ctrlWnd);
                    IF count > 0 THEN
                        NEW(listItems, count-1);
                        IF ListBox_GetSelItems(ctrlWnd, count, listItems^, count) THEN
                            <*/PUSH/NOWARN:U*>
                            FOR i := 0 TO VAL(ADRCARD, count)-1 DO
                                IF i <= VAL(ADRCARD, HIGH(items)) THEN
                                    items[i] := listItems^[i];
                                END;
                            END;
                            <*/POP*>
                        ELSE
                            count := 0;
                        END;
                        DISPOSE(listItems);
                    ELSE
                        count := 0;
                    END;
                |
                ColumnListBox:
                    count := ListView_GetSelectedCount(ctrlWnd);
                    IF count > 0 THEN
                        sel := -1;
                        <*/PUSH/NOWARN:U*>
                        FOR i := 0 TO VAL(ADRCARD, count)-1 DO
                            sel := ListView_GetNextItem(ctrlWnd, sel, LVNI_SELECTED);
                            IF sel <> -1 THEN
                                IF i <= VAL(ADRCARD, HIGH(items)) THEN
                                    items[i] := sel;
                                END;
                            END;
                        END;
                        <*/POP*>
                    END;
                ELSE
                END;
            END;
        END;
    END;
    RETURN count;
END GetSelectedItemsDlg;

PROCEDURE GetSelectedItems(index : CARDINAL;
                           VAR OUT items : ARRAY OF CARDINAL) : CARDINAL;
BEGIN
    RETURN GetSelectedItemsDlg(GetLastDialog(), index, items);
END GetSelectedItems;

PROCEDURE ClearMatchBufferDlg(dlgNum : ADRCARD; index : ADRCARD);
VAR
    info        : MatchListInfoPointer;
    ctrlWnd     : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                CASE ctrls^[index].ct OF
                ListBox, ColumnListBox:
                    ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                    info := CAST(MatchListInfoPointer, GetWindowLongPtr(ctrlWnd, GWL_USERDATA));
                    IF info <> NIL THEN
                        info^.matchStr := "";
                    END;
                ELSE
                END;
            END;
        END;
    END;
END ClearMatchBufferDlg;

PROCEDURE ClearMatchBuffer(index : CARDINAL);
BEGIN
    ClearMatchBufferDlg(GetLastDialog(), index);
END ClearMatchBuffer;

PROCEDURE AppendItemDlg(dlgNum : ADRCARD; index : ADRCARD; str : ARRAY OF CHAR) : INTEGER;
VAR
    retVal      : INTEGER;
    idx         : CARDINAL;
    ctrlWnd     : HWND;
    lvItem      : LVITEM;
BEGIN
    retVal := -1;
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    IF ListBox_AddString(ctrlWnd, str, idx) THEN
                        retVal := idx;
                    END;
                |
                ColumnListBox:
                    lvItem.mask := LVIF_TEXT;
                    lvItem.iItem := MAX(INTEGER);
                    lvItem.iSubItem := 0;
                    lvItem.pszText := ADR(str);
                    retVal := ListView_InsertItem(ctrlWnd, lvItem);
                |
                ComboBox, DropDownList:
                    retVal := ComboBox_AddString(ctrlWnd, str);
                    IF (retVal = CB_ERR) OR (retVal = CB_ERRSPACE) THEN
                        retVal := -1;
                    END;
                ELSE
                END;
            END;
        END;
    END;
    RETURN retVal;
END AppendItemDlg;

PROCEDURE AppendItem(index : CARDINAL; str : ARRAY OF CHAR) : INTEGER;
BEGIN
    RETURN AppendItemDlg(GetLastDialog(), index, str);
END AppendItem;

PROCEDURE RemoveItemDlg(dlgNum : ADRCARD; index : ADRCARD; item : CARDINAL);
VAR
    ctrlWnd     : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    ListBox_DeleteString(ctrlWnd, item);
                |
                ColumnListBox:
                    ListView_DeleteItem(ctrlWnd, item);
                |
                ComboBox, ColorDropDownList, DropDownList:
                    ComboBox_DeleteString(ctrlWnd, item);
                ELSE
                END;
            END;
        END;
    END;
END RemoveItemDlg;

PROCEDURE RemoveItem(index : CARDINAL; item : CARDINAL);
BEGIN
    RemoveItemDlg(GetLastDialog(), index, item);
END RemoveItem;

PROCEDURE RemoveAllItemsDlg(dlgNum : ADRCARD; index : ADRCARD);
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                CASE ctrls^[index].ct OF
                ListBox:
                    ListBox_ResetContent(GetDlgItem(wnd, ctrls^[index].id));
                |
                ColumnListBox:
                    ListView_DeleteAllItems(GetDlgItem(wnd, ctrls^[index].id));
                |
                ComboBox, ColorDropDownList, DropDownList:
                    ComboBox_ResetContent(GetDlgItem(wnd, ctrls^[index].id));
                ELSE
                END;
            END;
        END;
    END;
END RemoveAllItemsDlg;

PROCEDURE RemoveAllItems(index : CARDINAL);
BEGIN
    RemoveAllItemsDlg(GetLastDialog(), index);
END RemoveAllItems;

PROCEDURE SetItemDataDlg(dlgNum : ADRCARD; index : ADRCARD; item : CARDINAL; data : MACHINEWORD);
VAR
    lvItem      : LVITEM;
    ctrlWnd     : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    ListBox_SetItemData(ctrlWnd, item, data);
                |
                ColumnListBox:
                    lvItem.mask := LVIF_PARAM;
                    lvItem.lParam := data;
                    lvItem.iItem := item;
                    lvItem.iSubItem := 0;
                    ListView_SetItem(ctrlWnd, lvItem);
                |
                ComboBox, ColorDropDownList, DropDownList:
                    ComboBox_SetItemData(ctrlWnd, item, data);
                ELSE
                END;
            END;
        END;
    END;
END SetItemDataDlg;

PROCEDURE SetItemData(index : CARDINAL; item : CARDINAL; data : MACHINEWORD);
BEGIN
    SetItemDataDlg(GetLastDialog(), index, item, data);
END SetItemData;

PROCEDURE SetItemDataNumDlg(dlgNum : CARDINAL; index : CARDINAL; item : CARDINAL; data : INTEGER);
VAR
    word        : ADRINT;
BEGIN
    word := data;
    SetItemDataDlg(dlgNum, index, item, word);
END SetItemDataNumDlg;

PROCEDURE SetItemDataNum(index : CARDINAL; item : CARDINAL; data : INTEGER);
VAR
    word        : ADRINT;
BEGIN
    word := data;
    SetItemDataNumDlg(GetLastDialog(), index, item, word);
END SetItemDataNum;

PROCEDURE GetItemDataDlg(dlgNum : ADRCARD; index : ADRCARD; item : CARDINAL) : MACHINEWORD;
VAR
    lvItem      : LVITEM;
    ctrlWnd     : HWND;
    data        : MACHINEWORD;
BEGIN
    data := 0;

    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ListBox:
                    data := ListBox_GetItemData(ctrlWnd, item);
                |
                ColumnListBox:
                    lvItem.mask := LVIF_PARAM;
                    lvItem.lParam := data;
                    lvItem.iItem := item;
                    lvItem.iSubItem := 0;
                    ListView_GetItem(ctrlWnd, lvItem);
                    data := lvItem.lParam;
                |
                ComboBox, ColorDropDownList, DropDownList:
                    data := ComboBox_GetItemData(ctrlWnd, item);
                ELSE
                END;
            END;
        END;
    END;

    RETURN data;
END GetItemDataDlg;

PROCEDURE GetItemData(index : CARDINAL; item : CARDINAL) : MACHINEWORD;
BEGIN
    RETURN GetItemDataDlg(GetLastDialog(), index, item);
END GetItemData;

PROCEDURE GetItemDataNumDlg(dlgNum : CARDINAL; index : CARDINAL; item : CARDINAL) : INTEGER;
VAR
    word        : ADRINT;
BEGIN
    word := GetItemDataDlg(dlgNum, index, item);
    RETURN word;
END GetItemDataNumDlg;

PROCEDURE GetItemDataNum(index : CARDINAL; item : CARDINAL) : INTEGER;
VAR
    word        : ADRINT;
BEGIN
    word := GetItemDataDlg(GetLastDialog(), index, item);
    RETURN word;
END GetItemDataNum;

PROCEDURE AppendItemColumnsDlg(dlgNum : ADRCARD; index : ADRCARD; strs : ARRAY OF StringData) : INTEGER;
VAR
    ctrlWnd     : HWND;
    i           : ADRCARD;
    lvItem      : LVITEM;
    retVal      : INTEGER;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ColumnListBox:
                    lvItem.mask := LVIF_TEXT;
                    lvItem.iItem := MAX(INTEGER);
                    lvItem.iSubItem := 0;
                    lvItem.pszText := CAST(LPTSTR, strs[0]);
                    retVal := ListView_InsertItem(ctrlWnd, lvItem);

                    IF retVal >= 0 THEN
                        <*/PUSH/NOWARN:U*>
                        FOR i := 1 TO VAL(ADRCARD, ctrls^[index].clb_columns)-1 DO
                        <*/POP*>
                             ListView_SetItemText(ctrlWnd, retVal, i, strs[i]^);
                        END;
                    END;

                    RETURN retVal;
                ELSE
                END;
            END;
        END;
    END;
    RETURN -1;
END AppendItemColumnsDlg;

PROCEDURE AppendItemColumns(index : CARDINAL; strs : ARRAY OF StringData) : INTEGER;
BEGIN
    RETURN AppendItemColumnsDlg(GetLastDialog(), index, strs);
END AppendItemColumns;

PROCEDURE GetColumnWidthDlg(dlgNum : ADRCARD; index : ADRCARD; column : CARDINAL) : CARDINAL;
VAR
    ctrlWnd     : HWND;
    ret         : INTEGER;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ColumnListBox:
                    ret := ListView_GetColumnWidth(ctrlWnd, column);
                    IF ret = -1 THEN
                        ret := 0;
                    END;
                    RETURN ret;
                ELSE
                END;
            END;
        END;
    END;
    RETURN 0;
END GetColumnWidthDlg;

PROCEDURE GetColumnWidth(index : CARDINAL; column : CARDINAL) : CARDINAL;
BEGIN
    RETURN GetColumnWidthDlg(GetLastDialog(), index, column);
END GetColumnWidth;

PROCEDURE GetOptimalColumnWidthDlg(dlgNum : ADRCARD; index : ADRCARD; column : CARDINAL) : CARDINAL;
VAR
    ctrlWnd     : HWND;
    count       : CARDINAL;
    i           : CARDINAL;
    maxWidth    : CARDINAL;
    width       : CARDINAL;
    header      : StringData;
    text        : ARRAY [0..255] OF CHAR;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ColumnListBox:
                    maxWidth := 0;

                    header := ctrls^[index].clb_columnInfo^[column].header;
                    IF header <> NIL THEN
                        maxWidth := ListView_GetStringWidth(ctrlWnd, header^);
                    END;

                    count := ListView_GetItemCount(ctrlWnd);
                    IF count > 0 THEN
                        FOR i := 0 TO count-1 DO
                            ListView_GetItemText(ctrlWnd, i, column, text, HIGH(text));
                            text[HIGH(text)] := '';
                            width := ListView_GetStringWidth(ctrlWnd, text);

                            IF width > maxWidth THEN
                                maxWidth := width;
                            END;
                        END;
                    END;

                    (* add some padding pixels, emperically determined  *)

                    maxWidth := maxWidth + 15;

                    RETURN maxWidth;
                ELSE
                END;
            END;
        END;
    END;
    RETURN 0;
END GetOptimalColumnWidthDlg;

PROCEDURE GetOptimalColumnWidth(index : CARDINAL; column : CARDINAL) : CARDINAL;
BEGIN
    RETURN GetOptimalColumnWidthDlg(GetLastDialog(), index, column);
END GetOptimalColumnWidth;

PROCEDURE SetColumnWidthDlg(dlgNum : ADRCARD; index : ADRCARD; column : INTEGER; width : CARDINAL);
VAR
    ctrlWnd     : HWND;
    i           : CARDINAL;
    wid         : INTEGER;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                CASE ctrls^[index].ct OF
                ColumnListBox:
                    wid := width;
                    IF wid = 0 THEN
                        wid := LVSCW_AUTOSIZE_USEHEADER;
                    END;

                    IF column >= 0 THEN
                        (*IF width = 0 THEN
                            width := GetOptimalColumnWidthDlg(dlgNum, index, column);
                        END;*)
                        ListView_SetColumnWidth(ctrlWnd, column, wid);
                    ELSE
                        FOR i := 0 TO ctrls^[index].clb_columns-1 DO
                            (*IF width = 0 THEN
                                width := GetOptimalColumnWidthDlg(dlgNum, index, i);
                            END;*)
                            ListView_SetColumnWidth(ctrlWnd, i, wid);
                        END;
                    END;
                ELSE
                END;
            END;
        END;
    END;
END SetColumnWidthDlg;

PROCEDURE SetColumnWidth(index : CARDINAL; column : INTEGER; width : CARDINAL);
BEGIN
    SetColumnWidthDlg(GetLastDialog(), index, column, width);
END SetColumnWidth;

PROCEDURE MleGetCharCountDlg(dlgNum : ADRCARD; index : ADRCARD) : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (
                (ctrls^[index].ct = MultiLineEdit) OR
                (ctrls^[index].ct = EditTextFile)
               )
            THEN
            <*/POP*>
                RETURN Edit_GetTextLength(GetDlgItem(wnd, ctrls^[index].id));
            END;
        END;
    END;
    RETURN 0;
END MleGetCharCountDlg;

PROCEDURE MleGetCharCount(index : CARDINAL) : CARDINAL;
BEGIN
    RETURN MleGetCharCountDlg(GetLastDialog(), index);
END MleGetCharCount;

PROCEDURE MleAppendDlg(dlgNum : ADRCARD; index : ADRCARD; text : ARRAY OF CHAR) : BOOLEAN;
VAR
    ctrlWnd     : HWND;
    chIndex     : INTEGER;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (
                (ctrls^[index].ct = MultiLineEdit) OR
                (ctrls^[index].ct = EditTextFile)
               )
            THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                chIndex := Edit_GetTextLength(ctrlWnd);

                Edit_SetSel(ctrlWnd, chIndex, chIndex);
                Edit_ReplaceSel(ctrlWnd, text);

                RETURN TRUE;
            END;
        END;
    END;
    RETURN FALSE;
END MleAppendDlg;

PROCEDURE MleAppend(index : CARDINAL; text : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN MleAppendDlg(GetLastDialog(), index, text);
END MleAppend;

PROCEDURE MleAppendLineDlg(dlgNum : CARDINAL; index : CARDINAL; text : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    IF MleAppendDlg(dlgNum, index, text) THEN
        RETURN MleAppendDlg(dlgNum, index, CrLf);
    END;
    RETURN FALSE;
END MleAppendLineDlg;

PROCEDURE MleAppendLine(index : CARDINAL; text : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN MleAppendLineDlg(GetLastDialog(), index, text);
END MleAppendLine;

PROCEDURE MleAppendToLineDlg(dlgNum : ADRCARD; index : ADRCARD; line : CARDINAL; text : ARRAY OF CHAR);
VAR
    ctrlWnd     : HWND;
    chSt        : CARDINAL;
    chEnd       : CARDINAL;
    l           : CARDINAL;
    curText     : ARRAY [0..511] OF CHAR;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (
                (ctrls^[index].ct = MultiLineEdit) OR
                (ctrls^[index].ct = EditTextFile)
               )
               AND
               (line > 0)
            THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                curText := "";
                l := Edit_GetLine(ctrlWnd, line-1, curText, HIGH(curText)+1);
                IF l <= HIGH(curText) THEN
                    curText[l] := '';
                END;

                chSt := Edit_LineIndex(ctrlWnd, line-1);
                chEnd := Edit_LineIndex(ctrlWnd, line);

                Edit_SetSel(ctrlWnd, chSt, chEnd);

                Append(text, curText);
                Append(CrLf, curText);
                Edit_ReplaceSel(ctrlWnd, curText);
            END;
        END;
    END;
END MleAppendToLineDlg;

PROCEDURE MleAppendToLine(index : CARDINAL; line : CARDINAL; text : ARRAY OF CHAR);
BEGIN
    MleAppendToLineDlg(GetLastDialog(), index, line, text);
END MleAppendToLine;

PROCEDURE MleGetLineCountDlg(dlgNum : ADRCARD; index : ADRCARD) : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (
                (ctrls^[index].ct = MultiLineEdit) OR
                (ctrls^[index].ct = EditTextFile)
               )
            THEN
            <*/POP*>
                RETURN Edit_GetLineCount(GetDlgItem(wnd, ctrls^[index].id));
            END;
        END;
    END;
    RETURN 0;
END MleGetLineCountDlg;

PROCEDURE MleGetLineCount(index : CARDINAL) : CARDINAL;
BEGIN
    RETURN MleGetLineCountDlg(GetLastDialog(), index);
END MleGetLineCount;

PROCEDURE MleGetLineLengthDlg(dlgNum : ADRCARD; index : ADRCARD; line : CARDINAL) : CARDINAL;
VAR
    chIndex     : CARDINAL;
    ctrlWnd     : HWND;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (
                (ctrls^[index].ct = MultiLineEdit) OR
                (ctrls^[index].ct = EditTextFile)
               )
               AND
               (line > 0)
            THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                chIndex := Edit_LineIndex(ctrlWnd, line-1);

                RETURN Edit_LineLength(ctrlWnd, chIndex);
            END;
        END;
    END;
    RETURN 0;
END MleGetLineLengthDlg;

PROCEDURE MleGetLineLength(index : CARDINAL; line : CARDINAL) : CARDINAL;
BEGIN
    RETURN MleGetLineLengthDlg(GetLastDialog(), index, line);
END MleGetLineLength;

PROCEDURE MleGetLineDlg(dlgNum : ADRCARD; index : ADRCARD; line : CARDINAL; VAR OUT text : ARRAY OF CHAR) : BOOLEAN;
VAR
    count       : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (
                (ctrls^[index].ct = MultiLineEdit) OR
                (ctrls^[index].ct = EditTextFile)
               )
               AND
               (line > 0)
            THEN
            <*/POP*>
                count := Edit_GetLine(GetDlgItem(wnd, ctrls^[index].id), line-1, text, SIZE(text));

                IF count <> 0 THEN
                    IF count <= HIGH(text) THEN
                        text[count] := '';
                    END;
                    RETURN TRUE;
                END;
            END;
        END;
    END;
    RETURN FALSE;
END MleGetLineDlg;

PROCEDURE MleGetLine(index : CARDINAL; line : CARDINAL; VAR OUT text : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN MleGetLineDlg(GetLastDialog(), index, line, text);
END MleGetLine;

PROCEDURE MleRemoveLineDlg(dlgNum : ADRCARD; index : ADRCARD; line : CARDINAL);
VAR
    ctrlWnd     : HWND;
    chSt        : CARDINAL;
    chEnd       : CARDINAL;
    text        : ARRAY [0..0] OF CHAR;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (
                (ctrls^[index].ct = MultiLineEdit) OR
                (ctrls^[index].ct = EditTextFile)
               )
               AND
               (line > 0)
            THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);

                chSt := Edit_LineIndex(ctrlWnd, line-1);
                chEnd := Edit_LineIndex(ctrlWnd, line);

                Edit_SetSel(ctrlWnd, chSt, chEnd);

                text[0] := '';
                Edit_ReplaceSel(ctrlWnd, text);
            END;
        END;
    END;
END MleRemoveLineDlg;

PROCEDURE MleRemoveLine(index : CARDINAL; line : CARDINAL);
BEGIN
    MleRemoveLineDlg(GetLastDialog(), index, line);
END MleRemoveLine;

PROCEDURE MlePositionCaretDlg(dlgNum : ADRCARD; index : ADRCARD; line, column : CARDINAL);
VAR
    ctrlWnd     : HWND;
    chIndex     : CARDINAL;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND
               (
                (ctrls^[index].ct = MultiLineEdit) OR
                (ctrls^[index].ct = EditTextFile)
               )
            THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);
                IF line = MAX(CARDINAL) THEN
                    line := Edit_GetLineCount(ctrlWnd);
                ELSIF line = 0 THEN
                    line := 1;
                END;
                chIndex := Edit_LineIndex(ctrlWnd, line-1);
                chIndex := chIndex + column;
                Edit_SetSel(ctrlWnd, chIndex, chIndex);
                Edit_ScrollCaret(ctrlWnd);
            END;
        END;
    END;
END MlePositionCaretDlg;

PROCEDURE MlePositionCaret(index : CARDINAL; line, column : CARDINAL);
BEGIN
    MlePositionCaretDlg(GetLastDialog(), index, line, column);
END MlePositionCaret;

PROCEDURE SaveTextFileDlg(dlgNum : ADRCARD; index : ADRCARD);
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF (index < VAL(ADRCARD, numCtrls)) AND (ctrls^[index].ct = EditTextFile) THEN
            <*/POP*>
                IF ctrls^[index].etf_altered THEN
                    SaveFile(dlgNum, index, ctrls^[index].etf_name);
                END;
            END;
        END;
    END;
END SaveTextFileDlg;

PROCEDURE SaveTextFile(index : CARDINAL);
BEGIN
    SaveTextFileDlg(GetLastDialog(), index);
END SaveTextFile;

PROCEDURE NIL_NOTIFY(n : NotifyType; VAR INOUT item : CARDINAL) : NotifyResult;
BEGIN
    UNREFERENCED_PARAMETER(n);
    UNREFERENCED_PARAMETER(item);
    RETURN ContinueDialog;
END NIL_NOTIFY;

PROCEDURE ConvertToNulls(ch : CHAR; VAR INOUT str : ARRAY OF CHAR);
VAR
    l   : ADRCARD;
    i   : ADRCARD;
BEGIN
    l := LENGTH(str);
    IF l > 0 THEN
        FOR i := 0 TO l-1 DO
            IF str[i] = ch THEN
                str[i] := '';
            END;
        END;
    END;
END ConvertToNulls;

PROCEDURE ConvertFromNulls(ch : CHAR; VAR INOUT str : ARRAY OF CHAR);
VAR
    i           : ADRCARD;
    highStr     : ADRCARD;
BEGIN
    highStr := HIGH(str);
    i := 0;
    LOOP
        IF i = highStr THEN
            EXIT;
        ELSIF str[i] = '' THEN
            str[i] := ch;

            IF str[i+1] = '' THEN
                EXIT;
            END;
        END;
        INC(i);
    END;
END ConvertFromNulls;

PROCEDURE GetNthString(list : ARRAY OF CHAR; listSep : CHAR; VAR OUT str : ARRAY OF CHAR; n : CARDINAL);
VAR
    i   : CARDINAL;
    j   : CARDINAL;
BEGIN
    i := 0;
    j := 0;
    LOOP
        IF NOT GetNextItem(list, i, str, listSep) THEN
            str[0] := '';
            EXIT;
        ELSIF j = n THEN
            EXIT;
        END;

        INC(j);
    END;
END GetNthString;

PROCEDURE GetNthFromList(list : ARRAY OF CHAR; listSep : CHAR; str : ARRAY OF CHAR; VAR OUT n : CARDINAL);
VAR
    i           : CARDINAL;
    item        : ARRAY [0..127] OF CHAR;
BEGIN
    i := 0;
    n := 0;
    LOOP
        IF NOT GetNextItem(list, i, item, listSep) THEN
            n := MAX(CARDINAL);
            EXIT;
        ELSIF EqualI(item, str) THEN
            EXIT;
        END;

        INC(n);
    END;
END GetNthFromList;

PROCEDURE GetDlgWindowDlg(dlgNum : CARDINAL) : ADDRESS;
BEGIN
    IF ValidateDlgNum(dlgNum) THEN
        RETURN Dialogs[dlgNum].wnd;
    END;
    RETURN NIL;
END GetDlgWindowDlg;

PROCEDURE GetDlgWindow() : ADDRESS;
BEGIN
    RETURN GetDlgWindowDlg(GetLastDialog());
END GetDlgWindow;

PROCEDURE GetControlHandleDlg(dlgNum : ADRCARD; index : ADRCARD) : ADDRESS;
VAR
    ctrlWnd     : HWND;
BEGIN
    ctrlWnd := NIL;
    IF ValidateDlgNum(dlgNum) THEN
        WITH Dialogs[dlgNum] DO
            index := GetControlSubscript(dlgNum, index);
            <*/PUSH/NOWARN:U*>
            IF index < VAL(ADRCARD, numCtrls) THEN
            <*/POP*>
                ctrlWnd := GetDlgItem(wnd, ctrls^[index].id);
            END;
        END;
    END;
    RETURN ctrlWnd;
END GetControlHandleDlg;

PROCEDURE GetControlHandle(control : CARDINAL) : ADDRESS;
BEGIN
    RETURN GetControlHandleDlg(GetLastDialog(), control);
END GetControlHandle;

PROCEDURE Init;
VAR
    i   : ADRCARD;
BEGIN
    FOR i := 1 TO MaxDlgs DO
        Dialogs[i] := NullDialog;
    END;

    DialogSize := NormalDialog;
    ControlIdMode := ControlSubscript;

    AllocateTlsIndex(TlsIndex);
    SetLastDialog(0);

    CreateCriticalSection(Critic);

    GetResFile;
END Init;

PROCEDURE Term;
BEGIN
    IF Critic <> NIL THEN
        CloseCriticalSection(Critic);
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
END DlgShell.
