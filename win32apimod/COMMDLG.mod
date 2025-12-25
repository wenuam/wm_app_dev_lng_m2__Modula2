IMPLEMENTATION MODULE COMMDLG;
<*/NOWARN:I*>

FROM SYSTEM IMPORT
    CAST, ADR, FUNC;

FROM WIN32 IMPORT
    HWND, WPARAM, WINT, LPVOID, LPARAM, TCHAR;

FROM WINUSER IMPORT
    SendMessage, SendMessageA, SendMessageW;

<*/NOHIGH*>
<*/CALLS:StonyBrook*>
PROCEDURE CommDlg_OpenSave_GetSpecA(_hdlg : HWND;
                                    VAR _psz : ARRAY OF ACHAR;
                                    _cbmax : WPARAM) : WINT;
BEGIN
    RETURN SendMessageA(_hdlg,
                       CDM_GETSPEC,
                       _cbmax,
                       CAST(LPARAM, ADR(_psz)));
END CommDlg_OpenSave_GetSpecA;

PROCEDURE CommDlg_OpenSave_GetSpecW(_hdlg : HWND;
                                    VAR _psz : ARRAY OF UCHAR;
                                    _cbmax : WPARAM) : WINT;
BEGIN
    RETURN SendMessageW(_hdlg,
                       CDM_GETSPEC,
                       _cbmax,
                       CAST(LPARAM, ADR(_psz)));
END CommDlg_OpenSave_GetSpecW;

PROCEDURE CommDlg_OpenSave_GetFilePathA(_hdlg : HWND;
                                        VAR _psz : ARRAY OF ACHAR;
                                        _cbmax : WPARAM) : WINT;
BEGIN
    RETURN SendMessageA(_hdlg,
                       CDM_GETFILEPATH,
                       _cbmax,
                       CAST(LPARAM, ADR(_psz)));
END CommDlg_OpenSave_GetFilePathA;

PROCEDURE CommDlg_OpenSave_GetFilePathW(_hdlg : HWND;
                                        VAR _psz : ARRAY OF UCHAR;
                                        _cbmax : WPARAM) : WINT;
BEGIN
    RETURN SendMessageW(_hdlg,
                       CDM_GETFILEPATH,
                       _cbmax,
                       CAST(LPARAM, ADR(_psz)));
END CommDlg_OpenSave_GetFilePathW;

PROCEDURE CommDlg_OpenSave_GetFolderPathA(_hdlg : HWND;
                                          VAR _psz : ARRAY OF ACHAR;
                                          _cbmax : WPARAM) : WINT;
BEGIN
    RETURN SendMessageA(_hdlg,
                       CDM_GETFOLDERPATH,
                       _cbmax,
                       CAST(LPARAM, ADR(_psz)));
END CommDlg_OpenSave_GetFolderPathA;

PROCEDURE CommDlg_OpenSave_GetFolderPathW(_hdlg : HWND;
                                          VAR _psz : ARRAY OF UCHAR;
                                          _cbmax : WPARAM) : WINT;
BEGIN
    RETURN SendMessageW(_hdlg,
                       CDM_GETFOLDERPATH,
                       _cbmax,
                       CAST(LPARAM, ADR(_psz)));
END CommDlg_OpenSave_GetFolderPathW;

PROCEDURE CommDlg_OpenSave_GetFolderIDList(_hdlg : HWND;
                                           _pidl : LPVOID;
                                           _cbmax : WPARAM) : WINT;
BEGIN
    RETURN SendMessage(_hdlg,
                       CDM_GETFOLDERIDLIST,
                       _cbmax,
                       CAST(LPARAM, _pidl));
END CommDlg_OpenSave_GetFolderIDList;

PROCEDURE CommDlg_OpenSave_GetFolderIDListA(_hdlg : HWND;
                                           _pidl : LPVOID;
                                           _cbmax : WPARAM) : WINT;
BEGIN
    RETURN SendMessageA(_hdlg,
                       CDM_GETFOLDERIDLIST,
                       _cbmax,
                       CAST(LPARAM, _pidl));
END CommDlg_OpenSave_GetFolderIDListA;

PROCEDURE CommDlg_OpenSave_GetFolderIDListW(_hdlg : HWND;
                                           _pidl : LPVOID;
                                           _cbmax : WPARAM) : WINT;
BEGIN
    RETURN SendMessageW(_hdlg,
                       CDM_GETFOLDERIDLIST,
                       _cbmax,
                       CAST(LPARAM, _pidl));
END CommDlg_OpenSave_GetFolderIDListW;

PROCEDURE CommDlg_OpenSave_SetControlText(_hdlg : HWND;
                                        _id : WPARAM;
                                        VAR _text : ARRAY OF TCHAR);
BEGIN
    FUNC SendMessage(_hdlg,
                     CDM_SETCONTROLTEXT,
                     _id,
                     CAST(LPARAM, ADR(_text)));
END CommDlg_OpenSave_SetControlText;

PROCEDURE CommDlg_OpenSave_SetControlTextA(_hdlg : HWND;
                                        _id : WPARAM;
                                        VAR _text : ARRAY OF ACHAR);
BEGIN
    FUNC SendMessageA(_hdlg,
                     CDM_SETCONTROLTEXT,
                     _id,
                     CAST(LPARAM, ADR(_text)));
END CommDlg_OpenSave_SetControlTextA;

PROCEDURE CommDlg_OpenSave_SetControlTextW(_hdlg : HWND;
                                        _id : WPARAM;
                                        VAR _text : ARRAY OF UCHAR);
BEGIN
    FUNC SendMessageW(_hdlg,
                     CDM_SETCONTROLTEXT,
                     _id,
                     CAST(LPARAM, ADR(_text)));
END CommDlg_OpenSave_SetControlTextW;

PROCEDURE CommDlg_OpenSave_HideControl(_hdlg : HWND; _id : WPARAM);
BEGIN
    FUNC SendMessage(_hdlg, CDM_HIDECONTROL, _id, 0);
END CommDlg_OpenSave_HideControl;

PROCEDURE CommDlg_OpenSave_HideControlA(_hdlg : HWND; _id : WPARAM);
BEGIN
    FUNC SendMessageA(_hdlg, CDM_HIDECONTROL, _id, 0);
END CommDlg_OpenSave_HideControlA;

PROCEDURE CommDlg_OpenSave_HideControlW(_hdlg : HWND; _id : WPARAM);
BEGIN
    FUNC SendMessageW(_hdlg, CDM_HIDECONTROL, _id, 0);
END CommDlg_OpenSave_HideControlW;

PROCEDURE CommDlg_OpenSave_SetDefExt(_hdlg : HWND; VAR _pszext : ARRAY OF TCHAR);
BEGIN
    FUNC SendMessage(_hdlg,
                     CDM_SETDEFEXT,
                     0,
                     CAST(LPARAM, ADR(_pszext)))
END CommDlg_OpenSave_SetDefExt;

PROCEDURE CommDlg_OpenSave_SetDefExtA(_hdlg : HWND; VAR _pszext : ARRAY OF ACHAR);
BEGIN
    FUNC SendMessageA(_hdlg,
                     CDM_SETDEFEXT,
                     0,
                     CAST(LPARAM, ADR(_pszext)))
END CommDlg_OpenSave_SetDefExtA;

PROCEDURE CommDlg_OpenSave_SetDefExtW(_hdlg : HWND; VAR _pszext : ARRAY OF UCHAR);
BEGIN
    FUNC SendMessageW(_hdlg,
                     CDM_SETDEFEXT,
                     0,
                     CAST(LPARAM, ADR(_pszext)))
END CommDlg_OpenSave_SetDefExtW;

END COMMDLG.
