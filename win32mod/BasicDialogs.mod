(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE BasicDialogs;

<*/RESOURCE:BasicDialogs*>

FROM SYSTEM IMPORT
    ADR, BYTE, ADDRESS, ADRCARD, CAST, IsThread, EXCEPTADR, FUNC, OutputCallTrace;

FROM EXCEPTIONS IMPORT
    GetMessage;

FROM Strings IMPORT
    Concat, Delete, Append;

FROM COMMDLG IMPORT
    OPENFILENAME, GetOpenFileName, GetSaveFileName,
    OFN_FILEMUSTEXIST, OFN_PATHMUSTEXIST, OFN_NOCHANGEDIR,
    OFN_HIDEREADONLY, OFN_OVERWRITEPROMPT, OFN_SHAREAWARE, OFN_LONGNAMES,
    CHOOSEFONT, ChooseFont,
    CF_SCREENFONTS, SCREEN_FONTTYPE, CF_FIXEDPITCHONLY,
    CF_INITTOLOGFONTSTRUCT, CF_NOSTYLESEL, CF_NOSIZESEL, CF_SCALABLEONLY,
    PRINTDLG, PrintDlg,
    PD_ALLPAGES, PD_SELECTION, PD_PAGENUMS, PD_NOSELECTION, PD_NOPAGENUMS,
    PD_COLLATE, PD_PRINTSETUP;

FROM DlgShell IMPORT
    CONTROL, NIL_NOTIFY, ControlType, PushButtonType, CallDialog, DialogPositions,
    GetDialogParent, CurrentAsParent, GetDlgWindow,
    CloseDialog, ModelessDialog, SetInputFocusTo,
    NotifyType, NotifyResult, GetText,
    ConvertToNulls, ConvertFromNulls,
    ControlIdModes, SetControlIdMode, GetControlIdMode;

FROM WinShell IMPORT
    Window, FontInfo, FontWeights, WasSystemDialogBox,
    IsMinimized, GetWindowHandle,
    PrintDriverInfo, LoadString;

FROM Conversions IMPORT
    CardToStr, StrBaseToCard;

FROM WIN32 IMPORT
    HANDLE, GMEM_MOVEABLE, HDC, HWND,
    GlobalHandle, GlobalLock, GlobalUnlock, GlobalFree,
    GetProfileString;

FROM WINUSER IMPORT
    MB_YESNO, MB_YESNOCANCEL, MB_OKCANCEL,
    MB_OK, MB_SETFOREGROUND, MB_TASKMODAL, MB_TOPMOST,
    MB_ICONEXCLAMATION, MB_ICONINFORMATION, MB_ICONSTOP, MB_ICONQUESTION,
    MB_DEFBUTTON1, MB_DEFBUTTON2, MB_DEFBUTTON3,
    IDYES, IDNO, IDOK, IDCANCEL;
IMPORT WINUSER;

FROM WINGDI IMPORT
    LOGFONT, DEVMODE, DMDUP_SIMPLEX, DM_DUPLEX, DM_COPY,
    FW_LIGHT, FW_NORMAL, FW_DEMIBOLD, FW_BOLD, FW_HEAVY,
    GetDeviceCaps, LOGPIXELSY;
IMPORT WINGDI;

FROM WINX IMPORT
    Instance, NULL_HWND, NULL_HDC, NULL_HANDLE,
    GlobalAllocPtr;

FROM WINSPOOL IMPORT
    OpenPrinter, ClosePrinter, DocumentProperties;

PROCEDURE GetDlgParent() : HWND;
VAR
    w   : Window;
BEGIN
    w := GetDialogParent();
    IF w <> CurrentAsParent THEN
        RETURN GetWindowHandle(w);
    END;
    RETURN GetDlgWindow();
END GetDlgParent;

PROCEDURE MessageBox(mess : ARRAY OF CHAR; msgType : MessageTypes);
CONST
    warning     : ARRAY OF CHAR = {"Warning",CHR(13),CHR(10)};
    error       : ARRAY OF CHAR = {"Error",CHR(13),CHR(10)};
    appError    : ARRAY OF CHAR = {"Internal Error",CHR(13),CHR(10)};
    crlf        : ARRAY [0..1] OF CHAR = {CHR(13),CHR(10)};
    hexDig      : ARRAY [0..15] OF CHAR = {"0123456789ABCDEF"};
VAR
    str         : ARRAY [0..255] OF CHAR;
    exMess      : ARRAY [0..63] OF CHAR;
    hexStr      : ARRAY [0..7] OF CHAR;
    flags       : CARDINAL;
    num         : CARDINAL;
    i           : ADRCARD;
BEGIN
    WasSystemDialogBox := TRUE;

    str := mess;
    flags := MB_OK BOR MB_SETFOREGROUND BOR MB_TASKMODAL;

    CASE msgType OF
    MsgInfo:
        flags := flags BOR MB_ICONINFORMATION;
    |
    MsgQuestion:
        flags := flags BOR MB_ICONQUESTION;
    |
    MsgWarning:
        flags := flags BOR MB_ICONEXCLAMATION;
        Concat(warning, str, str);
    |
    MsgError:
        flags := flags BOR MB_ICONSTOP BOR MB_TOPMOST;
        Concat(error, str, str);
    |
    MsgAppError:
        flags := flags BOR MB_ICONSTOP BOR MB_TOPMOST;
        Concat(appError, str, str);
    |
    MsgException:
        flags := flags BOR MB_ICONSTOP BOR MB_TOPMOST;

        OutputCallTrace;

        num := CAST(CARDINAL, EXCEPTADR());
        FOR i := 7 TO 0 BY -1 DO
            hexStr[i] := hexDig[num REM 16];
            num := num / 16;
        END;

        Concat("Program Exception at ", hexStr, exMess);
        Append(crlf, exMess);
        Concat(exMess, str, str);
        Append(crlf, str);
        GetMessage(exMess);
        Append(exMess, str);
    END;

    FUNC WINUSER.MessageBox(NULL_HWND(*GetDlgParent()*), str, MessageTitle, flags);
END MessageBox;

PROCEDURE MessageBoxId(mess : CARDINAL; msgType : MessageTypes);
VAR
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    LoadString(mess, str);
    MessageBox(str, msgType);
END MessageBoxId;

PROCEDURE YesNo(prompt : ARRAY OF CHAR; default : CHAR) : CHAR;
VAR
    flags       : CARDINAL;
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    WasSystemDialogBox := TRUE;

    CASE CAP(default) OF
    'Y':
        flags := MB_DEFBUTTON1;
    |
    'N':
        flags := MB_DEFBUTTON2;
    |
    ELSE
        flags := 0;
    END;
    flags := flags BOR
             MB_YESNO BOR
             MB_ICONQUESTION BOR
             MB_SETFOREGROUND BOR
             MB_TASKMODAL;

    str := prompt;

    CASE WINUSER.MessageBox(NULL_HWND(*GetDlgParent()*), str, MessageTitle, flags) OF
    IDYES:
        RETURN 'Y';
    |
    IDNO:
        RETURN 'N';
    |
    ELSE
        (* Report error ?? *)
        RETURN '';
    END;
END YesNo;

PROCEDURE YesNoId(prompt : CARDINAL; default : CHAR) : CHAR;
VAR
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    LoadString(prompt, str);
    RETURN YesNo(str, default);
END YesNoId;

PROCEDURE YesNoCancel(prompt : ARRAY OF CHAR; default : CHAR) : CHAR;
VAR
    flags       : CARDINAL;
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    WasSystemDialogBox := TRUE;

    CASE CAP(default) OF
    'Y':
        flags := MB_DEFBUTTON1;
    |
    'N':
        flags := MB_DEFBUTTON2;
    |
    'C':
        flags := MB_DEFBUTTON3;
    |
    ELSE
        flags := 0;
    END;
    flags := flags BOR
             MB_YESNOCANCEL BOR
             MB_ICONQUESTION BOR
             MB_SETFOREGROUND BOR
             MB_TASKMODAL;

    str := prompt;

    CASE WINUSER.MessageBox(NULL_HWND(*GetDlgParent()*), str, MessageTitle, flags) OF
    IDYES:
        RETURN 'Y';
    |
    IDNO:
        RETURN 'N';
    |
    IDCANCEL:
        RETURN 'C';
    |
    ELSE
        (* Report error ?? *)
        RETURN '';
    END;
END YesNoCancel;

PROCEDURE YesNoCancelId(prompt : CARDINAL; default : CHAR) : CHAR;
VAR
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    LoadString(prompt, str);
    RETURN YesNoCancel(str, default);
END YesNoCancelId;

PROCEDURE OkCancel(prompt : ARRAY OF CHAR;
                   default : CHAR;
                   msgType : MessageTypes) : CHAR;
VAR
    flags       : CARDINAL;
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    WasSystemDialogBox := TRUE;

    CASE CAP(default) OF
    'O':
        flags := MB_DEFBUTTON1;
    |
    'C':
        flags := MB_DEFBUTTON2;
    ELSE
        flags := 0;
    END;
    flags := flags BOR
             MB_OKCANCEL BOR
             MB_SETFOREGROUND BOR
             MB_TASKMODAL;
    CASE msgType OF
    MsgWarning:
        flags := flags BOR MB_ICONEXCLAMATION;
    |
    MsgInfo:
        flags := flags BOR MB_ICONINFORMATION;
    |
    MsgQuestion:
        flags := flags BOR MB_ICONQUESTION;
    |
    MsgError, MsgAppError, MsgException:
        flags := flags BOR MB_ICONSTOP;
    END;

    str := prompt;

    CASE WINUSER.MessageBox(NULL_HWND(*GetDlgParent()*), str, MessageTitle, flags) OF
    IDOK:
        RETURN 'O';
    |
    IDCANCEL:
        RETURN 'C';
    |
    ELSE
        (* Report error ?? *)
        RETURN '';
    END;
END OkCancel;

PROCEDURE OkCancelId(prompt : CARDINAL;
                     default : CHAR;
                     msgType : MessageTypes) : CHAR;
VAR
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    LoadString(prompt, str);
    RETURN OkCancel(str, default, msgType);
END OkCancelId;

VAR
    BusyBoxControls : ARRAY [0..1] OF CONTROL =
    {
        {0, 0, DialogTitle, NIL},
        {1000, 0, StaticText, NIL}
    };
    BusyBoxStr  : ARRAY [0..255] OF CHAR;
    BusyBox     : CARDINAL;

PROCEDURE OpenBusyBox(text : ARRAY OF CHAR) : BOOLEAN;
VAR
    parent      : Window;
BEGIN
    parent := GetDialogParent();
    IF (parent = NIL) OR (NOT IsMinimized(parent)) THEN
        BusyBoxControls[0].dlgTitle := ADR(MessageTitle);

        BusyBoxStr := text;
        BusyBoxStr[HIGH(BusyBoxStr)] := '';
        BusyBoxControls[1].sta_text := ADR(BusyBoxStr);

        BusyBox := ModelessDialog(parent,
                                  "Basic_BusyBox",
                                  BusyBoxControls,
                                  NIL_NOTIFY,
                                  CenterOnParent);
        RETURN BusyBox <> MAX(CARDINAL);
    END;
    BusyBox := MAX(CARDINAL);
    RETURN FALSE;
END OpenBusyBox;

PROCEDURE OpenBusyBoxId(text : CARDINAL) : BOOLEAN;
VAR
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    str := "";
    LoadString(text, str);
    RETURN OpenBusyBox(str);
END OpenBusyBoxId;

PROCEDURE CloseBusyBox;
BEGIN
    IF BusyBox <> MAX(CARDINAL) THEN
        CloseDialog(BusyBox, FALSE);
        BusyBox := MAX(CARDINAL);
    END;
END CloseBusyBox;

PROCEDURE PromptString(prompt : ARRAY OF CHAR;
                       VAR INOUT response : ARRAY OF CHAR) : BOOLEAN;
VAR
    dialogBoxControls : ARRAY [0..3] OF CONTROL =
    {
        {0, 0, DialogTitle, NIL},
        {1000, 0, LineEdit, NIL, 0},
        {1, 0, PushButton, CloseButton, 1},
        {2, 0, PushButton, CancelButton, 2}
    };
VAR
    retStr      : ARRAY [0..2047] OF CHAR;
    title       : ARRAY [0..131] OF CHAR;
    retVal      : CARDINAL;
BEGIN
    retStr := response;
    title := prompt;

    dialogBoxControls[0].dlgTitle := ADR(title);
    dialogBoxControls[1].le_text := ADR(retStr);
    IF HIGH(response) < HIGH(retStr) THEN
        dialogBoxControls[1].le_max := HIGH(response)+1;
    ELSE
        dialogBoxControls[1].le_max := HIGH(retStr)+1;
    END;
    retVal := CallDialog(GetDialogParent(),
                         "Basic_PromptString",
                         dialogBoxControls,
                         NIL_NOTIFY,
                         CenterOnParent);
    IF retVal = 1 THEN
        response := retStr;
        RETURN TRUE;
    END;
    RETURN FALSE;
END PromptString;

PROCEDURE PromptStringId(prompt : CARDINAL;
                         VAR INOUT response : ARRAY OF CHAR) : BOOLEAN;
VAR
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    LoadString(prompt, str);
    RETURN PromptString(str, response);
END PromptStringId;

PROCEDURE PromptPassword(prompt : ARRAY OF CHAR;
                         VAR INOUT response : ARRAY OF CHAR) : BOOLEAN;
VAR
    dialogBoxControls : ARRAY [0..3] OF CONTROL =
    {
        {0, 0, DialogTitle, NIL},
        {1000, 0, LineEdit, NIL, 0},
        {1, 0, PushButton, CloseButton, 1},
        {2, 0, PushButton, CancelButton, 2}
    };
VAR
    retStr      : ARRAY [0..127] OF CHAR;
    title       : ARRAY [0..131] OF CHAR;
    retVal      : CARDINAL;
BEGIN
    retStr := response;
    title := prompt;

    dialogBoxControls[0].dlgTitle := ADR(title);
    dialogBoxControls[1].le_text := ADR(retStr);
    IF HIGH(response) < HIGH(retStr) THEN
        dialogBoxControls[1].le_max := HIGH(response)+1;
    ELSE
        dialogBoxControls[1].le_max := HIGH(retStr)+1;
    END;
    retVal := CallDialog(GetDialogParent(),
                         "Basic_PromptPassword",
                         dialogBoxControls,
                         NIL_NOTIFY,
                         CenterOnParent);
    IF retVal = 1 THEN
        response := retStr;
        RETURN TRUE;
    END;
    RETURN FALSE;
END PromptPassword;

PROCEDURE PromptPasswordId(prompt : CARDINAL;
                           VAR INOUT response : ARRAY OF CHAR) : BOOLEAN;
VAR
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    LoadString(prompt, str);
    RETURN PromptPassword(str, response);
END PromptPasswordId;

VAR
    CardMin, CardMax    : CARDINAL;
    AllowZeroCard       : BOOLEAN;

PROCEDURE ConvertNumber(VAR numStr : ARRAY OF CHAR; VAR val : CARDINAL) : BOOLEAN;
VAR
    l           : ADRCARD;
    ok          : BOOLEAN;
BEGIN
    val := 0;
    ok := FALSE;
    l := LENGTH(numStr);
    IF l > 0 THEN
        IF CAP(numStr[l-1]) = 'H' THEN
            numStr[l-1] := CHR(0);
            DEC(l);
            ok := StrBaseToCard(numStr, 16, val);
        ELSE
            ok := StrBaseToCard(numStr, 10, val);
        END;
    END;
    RETURN ok;
END ConvertNumber;

PROCEDURE CardValidate(typ : NotifyType;
                       VAR INOUT item : CARDINAL) : NotifyResult
                      %IF DLL %THEN
                      [EXPORT, PROPAGATEEXCEPTION]
                      %END
                       ;
VAR
    val         : CARDINAL;
    numStr      : ARRAY [0..15] OF CHAR;
BEGIN

    IF (item = 2) AND (typ = Pressed) THEN
        IF GetText(1, numStr) THEN
            IF ConvertNumber(numStr, val) THEN
                IF ((val >= CardMin) AND (val <= CardMax)) OR
                   (AllowZeroCard AND (val = 0))
                THEN
                    item := 1;
                    RETURN TerminateDialog;
                ELSE
                    MessageBox("Number is outside valid range", MsgWarning);
                    SetInputFocusTo(1);
                END;
            ELSE
                MessageBox("Invalid number", MsgWarning);
                SetInputFocusTo(1);
            END;
        END;
    END;
    RETURN ContinueDialog;
END CardValidate;

PROCEDURE PromptCard(prompt : ARRAY OF CHAR;
                     min, max : CARDINAL;
                     allowZero : BOOLEAN;
                     VAR INOUT response : CARDINAL) : BOOLEAN;
VAR
    dialogBoxControls : ARRAY [0..3] OF CONTROL =
    {
        {0, 0, DialogTitle, NIL},
        {1000, 0, LineEdit, NIL, 8},
        {1, 0, PushButton, NotifyButton, 1},
        {2, 0, PushButton, CancelButton, 2}
    };

    numStr      : ARRAY [0..15] OF CHAR;
    title       : ARRAY [0..79] OF CHAR;
    save        : ControlIdModes;
    retVal      : CARDINAL;
BEGIN
    FUNC CardToStr(response, numStr);

    title := prompt;

    dialogBoxControls[0].dlgTitle := ADR(title);
    dialogBoxControls[1].le_text := ADR(numStr);
    dialogBoxControls[1].le_max := SIZE(numStr);

    CardMax := max;
    CardMin := min;
    AllowZeroCard := allowZero;

    save := GetControlIdMode();
    SetControlIdMode(ControlSubscript);
    retVal := CallDialog(GetDialogParent(),
                         "Basic_PromptCard",
                         dialogBoxControls,
                         CardValidate,
                         CenterOnParent);
    SetControlIdMode(save);

    IF retVal = 1 THEN
        RETURN ConvertNumber(numStr, response);
    END;
    RETURN FALSE;
END PromptCard;

PROCEDURE PromptCardId(prompt : CARDINAL;
                       min, max : CARDINAL;
                       allowEmpty : BOOLEAN;
                       VAR INOUT response : CARDINAL) : BOOLEAN;
VAR
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    LoadString(prompt, str);
    RETURN PromptCard(str, min, max, allowEmpty, response);
END PromptCardId;

PROCEDURE PromptListNotify(typ : NotifyType;
                           VAR INOUT item : CARDINAL) : NotifyResult
                          %IF DLL %THEN
                          [EXPORT, PROPAGATEEXCEPTION]
                          %END
                           ;
BEGIN
    IF item = 1 THEN
        IF typ = DoubleClicked THEN
            item := 1;
            RETURN TerminateDialog;
        END;
    END;
    RETURN ContinueDialog;
END PromptListNotify;

PROCEDURE PromptList(prompt : ARRAY OF CHAR;
                     VAR INOUT list : ARRAY OF CHAR;
                     listSep : CHAR;
                     selStr : ADDRESS;
                     VAR INOUT selIndex : CARDINAL;
                     listWidth : PromptListWidth) : BOOLEAN;
VAR
    dialogboxControls : ARRAY [0..3] OF CONTROL =
    {
        {0, 0, DialogTitle, NIL},
        {1000, 0, ListBox, NIL, NIL, 0},
        {1, 0, PushButton, CloseButton, 1},
        {2, 0, PushButton, CancelButton, 2}
    };

VAR
    title       : ARRAY [0..127] OF CHAR;
    retVal      : CARDINAL;
    save        : ControlIdModes;
    dialog      : ARRAY [0..31] OF CHAR;
BEGIN
    title := prompt;
    dialogboxControls[0].dlgTitle := ADR(title);
    dialogboxControls[1].lb_text := ADR(list);
    dialogboxControls[1].lb_selStr := selStr;
    dialogboxControls[1].lb_selIndex := selIndex;

    ConvertToNulls(listSep, list);

    CASE listWidth OF
    NormalListWidth:
        dialog := "Basic_PromptList_Normal";
    |
    WideListWidth:
        dialog := "Basic_PromptList_Wide";
    END;

    save := GetControlIdMode();
    SetControlIdMode(ControlSubscript);
    retVal := CallDialog(GetDialogParent(),
                         dialog,
                         dialogboxControls,
                         PromptListNotify,
                         CenterOnParent);
    SetControlIdMode(save);

    ConvertFromNulls(listSep, list);

    IF retVal = 1 THEN
        selIndex := dialogboxControls[1].lb_selIndex;
        RETURN TRUE;
    END;
    RETURN FALSE;
END PromptList;

PROCEDURE PromptListStr(prompt : ARRAY OF CHAR;
                        VAR INOUT list : ARRAY OF CHAR;
                        listSep : CHAR;
                        VAR INOUT selStr : ARRAY OF CHAR;
                        listWidth : PromptListWidth) : BOOLEAN;
VAR
    idx : CARDINAL;
BEGIN
    idx := 0;
    RETURN PromptList(prompt, list, listSep, ADR(selStr), idx, listWidth);
END PromptListStr;

PROCEDURE PromptListStrId(prompt : CARDINAL;
                          VAR INOUT list : ARRAY OF CHAR;
                          listSep : CHAR;
                          VAR INOUT selStr : ARRAY OF CHAR;
                          listWidth : PromptListWidth) : BOOLEAN;
VAR
    str         : ARRAY [0..255] OF CHAR;
    idx         : CARDINAL;
BEGIN
    LoadString(prompt, str);
    idx := 0;
    RETURN PromptList(str, list, listSep, ADR(selStr), idx, listWidth);
END PromptListStrId;

PROCEDURE PromptListIndex(prompt : ARRAY OF CHAR;
                          VAR INOUT list : ARRAY OF CHAR;
                          listSep : CHAR;
                          VAR INOUT selIndex : CARDINAL;
                          listWidth : PromptListWidth) : BOOLEAN;
BEGIN
    RETURN PromptList(prompt, list, listSep, NIL, selIndex, listWidth);
END PromptListIndex;

PROCEDURE PromptListIndexId(prompt : CARDINAL;
                            VAR INOUT list : ARRAY OF CHAR;
                            listSep : CHAR;
                            VAR INOUT selIndex : CARDINAL;
                            listWidth : PromptListWidth) : BOOLEAN;
VAR
    str         : ARRAY [0..255] OF CHAR;
BEGIN
    LoadString(prompt, str);
    RETURN PromptList(str, list, listSep, NIL, selIndex, listWidth);
END PromptListIndexId;

PROCEDURE FillBytes(VAR a : ARRAY OF BYTE);
VAR
    i           : CARDINAL;
BEGIN
    FOR i := 0 TO HIGH(a) DO
        a[i] := 0;
    END;
END FillBytes;

PROCEDURE PromptOpenFile(VAR INOUT name : ARRAY OF CHAR;
                         filters : ARRAY OF CHAR;
                         VAR INOUT defFilter : CARDINAL;
                         defDir : ARRAY OF CHAR;
                         defExt : ARRAY OF CHAR;
                         title : ARRAY OF CHAR;
                         createable : BOOLEAN) : BOOLEAN;
VAR
    ofn         : OPENFILENAME;
    (*lDefDir     : ARRAY [0..260] OF CHAR;*)
    lDefExt     : ARRAY [0..7] OF CHAR;
BEGIN
    WasSystemDialogBox := TRUE;

    FillBytes(ofn);

    ofn.hwndOwner := GetDlgParent();
    ofn.lStructSize := SIZE(OPENFILENAME);
    ofn.lpstrFilter := ADR(filters);
    ofn.nFilterIndex := defFilter;
    ofn.lpstrFile := ADR(name);
    ofn.nMaxFile := HIGH(name) + 1;
    IF defDir[0] <> '' THEN
        ofn.lpstrInitialDir := ADR(defDir);
        (*
    ELSE
        IF GetCurrentDirectory(HIGH(lDefDir)+1, lDefDir) <> 0 THEN
            ofn.lpstrInitialDir := ADR(lDefDir);
        END;
        *)
    END;
    IF defExt[0] <> '' THEN
        lDefExt := defExt;
        IF lDefExt[0] = '.' THEN
            Delete(lDefExt, 0, 1);
        END;
        ofn.lpstrDefExt := ADR(lDefExt);
    END;

    ofn.Flags := OFN_NOCHANGEDIR BOR
                 OFN_HIDEREADONLY BOR
                 OFN_SHAREAWARE BOR
                 OFN_LONGNAMES;
    IF NOT createable THEN
        ofn.Flags := ofn.Flags BOR OFN_FILEMUSTEXIST;
    END;

    IF title[0] <> '' THEN
        ofn.lpstrTitle := ADR(title);
    END;
    IF GetOpenFileName(ofn) THEN
        defFilter := ofn.nFilterIndex;
        RETURN TRUE;
    END;
    RETURN FALSE;
END PromptOpenFile;

PROCEDURE PromptSaveAsFile(VAR INOUT name : ARRAY OF CHAR;
                           filters : ARRAY OF CHAR;
                           VAR INOUT defFilter : CARDINAL;
                           defDir : ARRAY OF CHAR;
                           defExt : ARRAY OF CHAR;
                           title : ARRAY OF CHAR) : BOOLEAN;
VAR
    ofn         : OPENFILENAME;
    (*lDefDir     : ARRAY [0..260] OF CHAR;*)
    lDefExt     : ARRAY [0..7] OF CHAR;
BEGIN
    WasSystemDialogBox := TRUE;

    FillBytes(ofn);

    ofn.hwndOwner := GetDlgParent();
    ofn.lStructSize := SIZE(OPENFILENAME);
    ofn.lpstrFilter := ADR(filters);
    ofn.nFilterIndex := defFilter;
    ofn.lpstrFile := ADR(name);
    ofn.nMaxFile := HIGH(name) + 1;
    IF defDir[0] <> '' THEN
        ofn.lpstrInitialDir := ADR(defDir);
        (*
    ELSE
        IF GetCurrentDirectory(HIGH(lDefDir)+1, lDefDir) <> 0 THEN
            ofn.lpstrInitialDir := ADR(lDefDir);
        END;
        *)
    END;
    IF defExt[0] <> '' THEN
        lDefExt := defExt;
        IF lDefExt[0] = '.' THEN
            Delete(lDefExt, 0, 1);
        END;
        ofn.lpstrDefExt := ADR(lDefExt);
    END;

    ofn.Flags := OFN_NOCHANGEDIR BOR
                 OFN_HIDEREADONLY BOR
                 OFN_PATHMUSTEXIST BOR
                 OFN_LONGNAMES BOR
                 OFN_OVERWRITEPROMPT;

    IF title[0] <> '' THEN
        ofn.lpstrTitle := ADR(title);
    END;
    IF GetSaveFileName(ofn) THEN
        defFilter := ofn.nFilterIndex;
        RETURN TRUE;
    END;
    RETURN FALSE;
END PromptSaveAsFile;

PROCEDURE PromptChooseFont(VAR INOUT font : FontInfo;
                           opt : FontOptionSet) : BOOLEAN;

CONST
    weights     : ARRAY FontWeights OF CARDINAL =
        {FW_LIGHT, FW_NORMAL, FW_DEMIBOLD, FW_BOLD, FW_HEAVY};
VAR
    cf          : CHOOSEFONT;
    lf          : LOGFONT;
    dc          : HDC;
    pixels      : INTEGER;
    temp        : INTEGER;
BEGIN
    WasSystemDialogBox := TRUE;

    FillBytes(lf);
    lf.lfFaceName := font.familyName;
    lf.lfItalic := font.italic;
    lf.lfWeight := weights[font.weight];
    dc := WINUSER.GetDC(NULL_HWND);
    pixels := GetDeviceCaps(dc, LOGPIXELSY);
    FUNC WINUSER.ReleaseDC(NULL_HWND, dc);
    IF font.height >= 0 THEN
        temp := font.height * pixels;
        lf.lfHeight := (temp / 720);
        IF (temp REM 720) > 360 THEN
            INC(lf.lfHeight);
        END;
        lf.lfHeight := -lf.lfHeight;
    ELSE
        lf.lfHeight := font.height;
    END;
    lf.lfCharSet := WINGDI.DEFAULT_CHARSET;

    FillBytes(cf);
    cf.lStructSize := SIZE(CHOOSEFONT);
    cf.hwndOwner := GetDlgParent();
    cf.hDC := NULL_HDC;
    cf.lpLogFont := ADR(lf);
    cf.nFontType := SCREEN_FONTTYPE;
    cf.Flags := CF_SCREENFONTS BOR CF_INITTOLOGFONTSTRUCT;

    IF FixedPitchOnly IN opt THEN
        cf.Flags := cf.Flags BOR CF_FIXEDPITCHONLY;
    END;
    IF ScalableOnly IN opt THEN
        cf.Flags := cf.Flags BOR CF_SCALABLEONLY;
    END;
    IF NoStyle IN opt THEN
        cf.Flags := cf.Flags BOR CF_NOSTYLESEL;
    END;
    IF NoSize IN opt THEN
        cf.Flags := cf.Flags BOR CF_NOSIZESEL;
    END;

    IF ChooseFont(cf) THEN
        IF lf.lfHeight < 0 THEN
            lf.lfHeight := -lf.lfHeight;
        ELSE
            (* we have no concept of "cell height", only "character height". *)
        END;

        (* convert to tenths of a point *)

        temp := lf.lfHeight * 720;
        font.height := temp / pixels;(*whole points*)
        temp := temp REM pixels;
        temp := (temp * 10) / 720;
        font.height := font.height + temp;(* tenths of a point *)

        font.familyName := lf.lfFaceName;
        font.italic := lf.lfItalic;
        IF lf.lfWeight < FW_NORMAL THEN
            font.weight := FwLight;
        ELSIF lf.lfWeight < FW_DEMIBOLD THEN
            font.weight := FwNormal;
        ELSIF lf.lfWeight < FW_BOLD THEN
            font.weight := FwDemiBold;
        ELSIF lf.lfWeight < FW_HEAVY THEN
            font.weight := FwBold;
        ELSE
            font.weight := FwHeavy;
        END;
        RETURN TRUE;
    END;
    RETURN FALSE;
END PromptChooseFont;

PROCEDURE PromptPrint(VAR INOUT info : PrintInfo) : BOOLEAN;
VAR
    pd          : PRINTDLG;
    pDev        : POINTER TO DEVMODE;
    (*base      : POINTER TO DEVNAMES;*)

    (*
    PROCEDURE getName(offset : CARDINAL; VAR str : ARRAY OF CHAR);
    VAR
        ptr     : POINTER TO CHAR;
        i       : CARDINAL;
    BEGIN
        i := 0;
        ptr := ADDADR(base, offset);
        WHILE ptr^ <> '' DO
            IF i < HIGH(str) THEN
                str[i] := ptr^;
                INC(i);
            END;
            ptr := ADDADR(ptr, SIZE(CHAR));
        END;
        str[i] := '';
    END getName;
    *)

BEGIN
    FillBytes(pd);

    pd.lStructSize := SIZE(PRINTDLG);
    pd.hwndOwner := GetDlgParent();
    pd.hInstance := Instance;
    pd.hDC := NULL_HDC;
    pd.nCopies := info.copies;
    pd.Flags := 0;
    pd.hDevNames := NULL_HANDLE;
    (*
    IF info.printerName[0] <> '' THEN
        pd.hDevNames := MakeDEVNAMES(info.printerName,
                                     info.driverName,
                                     info.portName);
    END;
    *)
    IF info.driverInfo <> NIL THEN
        pd.hDevMode := GlobalHandle(info.driverInfo);
        FUNC GlobalUnlock(pd.hDevMode);
    ELSE
        pd.hDevMode := NULL_HANDLE;
    END;

    IF info.toPage = 0 THEN
        pd.Flags := pd.Flags BOR PD_NOPAGENUMS;
    ELSE
        pd.nFromPage := info.fromPage;
        pd.nToPage := info.toPage;
        pd.nMinPage := info.minPage;
        pd.nMaxPage := info.maxPage;
    END;

    IF NOT info.selected THEN
        pd.Flags := pd.Flags BOR PD_ALLPAGES BOR PD_NOSELECTION;
    ELSE
        pd.Flags := pd.Flags BOR PD_SELECTION;
    END;

    IF info.collate THEN
        pd.Flags := pd.Flags BOR PD_COLLATE;
    END;

    (*pd.Flags := pd.Flags BOR PD_RETURNDC*);

    IF PrintDlg(pd) THEN
        info.duplex := FALSE;

        info.selected := (pd.Flags BAND PD_SELECTION) <> 0;

        info.fromPage := 0;
        info.toPage := 0;
        IF (pd.Flags BAND PD_PAGENUMS) <> 0 THEN
            info.fromPage := pd.nFromPage;
            info.toPage := pd.nToPage;
        END;

        info.collate := FALSE;
        IF (pd.Flags BAND PD_COLLATE) <> 0 THEN
            info.collate := TRUE;
        END;

        (*
        base := GlobalLock(pd.hDevNames);
        IF base <> NIL THEN
            getName(base^.wDriverOffset, info.driverName);
            getName(base^.wDeviceOffset, info.printerName);
            getName(base^.wOutputOffset, info.portName);
        END;
        FUNC GlobalUnlock(pd.hDevNames);
        *)
        FUNC GlobalFree(pd.hDevNames);

        info.driverInfo := NIL;
        IF pd.hDevMode <> NULL_HANDLE THEN
            pDev := GlobalLock(pd.hDevMode);
            info.driverInfo := CAST(PrintDriverInfo, pDev);
            IF (DM_DUPLEX BAND pDev^.dmFields) <> 0 THEN
                info.duplex := pDev^.dmDuplex <> DMDUP_SIMPLEX;
            END;
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END PromptPrint;

PROCEDURE PromptPrintSetup(VAR INOUT driverInfo : PrintDriverInfo) : BOOLEAN;
VAR
    pd          : PRINTDLG;
    pDev        : POINTER TO DEVMODE;
BEGIN
    FillBytes(pd);

    pd.lStructSize := SIZE(PRINTDLG);
    pd.hwndOwner := GetDlgParent();
    pd.hInstance := Instance;
    pd.hDC := NULL_HDC;
    pd.nCopies := 1;
    pd.Flags := PD_PRINTSETUP;
    pd.hDevNames := NULL_HANDLE;
    IF driverInfo <> NIL THEN
        pd.hDevMode := GlobalHandle(driverInfo);
        FUNC GlobalUnlock(pd.hDevMode);
    ELSE
        pd.hDevMode := NULL_HANDLE;
    END;

    IF PrintDlg(pd) THEN
        FUNC GlobalFree(pd.hDevNames);

        IF pd.hDevMode <> NULL_HANDLE THEN
            pDev := GlobalLock(pd.hDevMode);
            driverInfo := CAST(PrintDriverInfo, pDev);
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END PromptPrintSetup;

PROCEDURE GetDefaultPrinter(VAR OUT printer : ARRAY OF CHAR) : BOOLEAN;
VAR
    count       : ADRCARD;
    str         : ARRAY [0..255] OF CHAR;
    i, j        : ADRCARD;
    highPrinter : ADRCARD;
    default     : ARRAY [0..255] OF CHAR;
BEGIN
    default := "";
    count := GetProfileString("Windows", "Device", default, str, SIZE(str));
    IF count <> 0 THEN
        j := 0;
        i := 0;
        WHILE (i < count) AND (str[i] <> ',') DO
            highPrinter := HIGH(printer);
            IF j < highPrinter THEN
                printer[j] := str[i];
                INC(j);
            END;
            INC(i);
        END;
        printer[j] := '';

        (*
        INC(i);

        j := 0;
        WHILE (i < count) AND (str[i] <> ',') DO
            IF j < HIGH(DriverName) THEN
                DriverName[j] := str[i];
                INC(j);
            END;
            INC(i);
        END;
        DriverName[j] := '';

        INC(i);

        j := 0;
        WHILE (i < count) AND (str[i] <> ',') DO
            IF j < HIGH(PortName) THEN
                PortName[j] := str[i];
                INC(j);
            END;
            INC(i);
        END;
        PortName[j] := '';
        *)

        RETURN TRUE;
    END;
    RETURN FALSE;
END GetDefaultPrinter;

PROCEDURE GetPrintDriverInfo(printerName : ARRAY OF CHAR;
                             VAR OUT info :  PrintInfo) : BOOLEAN;
VAR
    hPrn        : HANDLE;
    ok          : BOOLEAN;
    mode        : POINTER TO DEVMODE;
    dummy       : POINTER TO DEVMODE;
    size        : CARDINAL;
    printer     : ARRAY [0..255] OF CHAR;
BEGIN
    printer := printerName;

    IF printer[0] = '' THEN
        IF NOT GetDefaultPrinter(printer) THEN
            RETURN FALSE;
        END;
    END;

    ok := FALSE;

    IF OpenPrinter(printer, hPrn, NIL) THEN

        mode := NIL;
        dummy := NIL;

        size := DocumentProperties(NULL_HWND, hPrn, printer, dummy^, dummy^,  0);
        IF size > 0 THEN

            mode := GlobalAllocPtr(GMEM_MOVEABLE, size);

            IF DocumentProperties(NULL_HWND,
                                  hPrn,
                                  printer,
                                  mode^, dummy^,
                                  DM_COPY) = 1
            THEN
                ok := TRUE;

                info.driverInfo := CAST(PrintDriverInfo, mode);
                info.duplex := FALSE;
                IF (DM_DUPLEX BAND mode^.dmFields) <> 0 THEN
                    info.duplex := mode^.dmDuplex <> DMDUP_SIMPLEX;
                END;
            END;
        END;

        FUNC ClosePrinter(hPrn);
    END;

    RETURN ok;
END GetPrintDriverInfo;

BEGIN
    IF NOT IsThread THEN
        MessageTitle := "Basic Dialogs";
    END;
END BasicDialogs.
