IMPLEMENTATION MODULE HTMLHELP;

FROM SYSTEM IMPORT
    ADDRESS, CAST, FUNC, IsThread;

FROM WIN32 IMPORT
    HWND, UINT, HINSTANCE, LoadLibrary, FreeLibrary, GetProcAddress, DWORD_PTR;

FROM WINX IMPORT
    NULL_HINSTANCE, NULL_HWND;

<*/CALLS:WINDOWSCALL*>
<*/NOHIGH*>
<*/ALIGN:8/NOPACK*>

<*/NOWARN:I*>

TYPE
    HTMLPROCW = PROCEDURE(HWND, ARRAY OF UCHAR, UINT, DWORD_PTR) : HWND;
    HTMLPROCA = PROCEDURE(HWND, ARRAY OF ACHAR, UINT, DWORD_PTR) : HWND;

VAR
    DllInst     : HINSTANCE;
    HelpProcA   : HTMLPROCA;
    HelpProcW   : HTMLPROCW;

PROCEDURE HtmlHelpW   (hwndCaller : HWND;
                       pszFile : ARRAY OF UCHAR;
                       uCommand : UINT;
                       dwData : DWORD_PTR) : HWND;
VAR
    hwnd        : HWND;
BEGIN
    hwnd := NULL_HWND;
    IF CAST(ADDRESS, HelpProcW) <> NIL THEN
        hwnd := HelpProcW(hwndCaller, pszFile, uCommand, dwData);
    ELSE
        IF DllInst = NULL_HINSTANCE THEN
            DllInst := LoadLibrary("hhctrl.ocx");
        END;
        IF DllInst <> NULL_HINSTANCE THEN
            HelpProcW := CAST(HTMLPROCW, GetProcAddress(DllInst, "HtmlHelpW"));
            IF CAST(ADDRESS, HelpProcW) <> NIL THEN
                hwnd := HelpProcW(hwndCaller, pszFile, uCommand, dwData);
            END;
        END;
    END;
    RETURN hwnd;
END HtmlHelpW;

PROCEDURE HtmlHelpA   (hwndCaller : HWND;
                       pszFile : ARRAY OF ACHAR;
                       uCommand : UINT;
                       dwData : DWORD_PTR) : HWND;
VAR
    hwnd        : HWND;
BEGIN
    hwnd := NULL_HWND;
    IF CAST(ADDRESS, HelpProcA) <> NIL THEN
        hwnd := HelpProcA(hwndCaller, pszFile, uCommand, dwData);
    ELSE
        IF DllInst = NULL_HINSTANCE THEN
            DllInst := LoadLibrary("hhctrl.ocx");
        END;
        IF DllInst <> NULL_HINSTANCE THEN
            HelpProcA := CAST(HTMLPROCA, GetProcAddress(DllInst, "HtmlHelpA"));
            IF CAST(ADDRESS, HelpProcA) <> NIL THEN
                hwnd := HelpProcA(hwndCaller, pszFile, uCommand, dwData);
            END;
        END;
    END;
    RETURN hwnd;
END HtmlHelpA;

BEGIN
    IF NOT IsThread THEN
        DllInst := NULL_HINSTANCE;
        HelpProcA := CAST(HTMLPROCA, NIL);
        HelpProcW := CAST(HTMLPROCW, NIL);
    END;

FINALLY
    IF NOT IsThread THEN
        IF DllInst <> NULL_HINSTANCE THEN
            FUNC FreeLibrary(DllInst);
        END;
    END;
END HTMLHELP.
