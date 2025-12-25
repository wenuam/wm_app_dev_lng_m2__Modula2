IMPLEMENTATION MODULE SimpleMAPI;
(* Simple MAPI API *)

<*/CALLS:WINDOWSCALL*>
%IF AMD64 %THEN
<*/NOHIGH/NOPACK/ALIGN:8*>
%ELSE
<*/NOHIGH/PACK/ALIGN:4*>
%END

FROM SYSTEM IMPORT
    IsThread;

FROM WIN32 IMPORT
    ULONG, HINSTANCE, LoadLibrary, GetProcAddress, FreeLibrary, FARPROC, LPVOID, ULONG_PTR;

FROM WINX IMPORT
    NULL_HINSTANCE;

VAR
    Initialized         : BOOLEAN;
    DllInst             : HINSTANCE;

    vMAPILogon,
    vMAPILogoff,
    vMAPISendMail,
    vMAPISendDocuments,
    vMAPIFindNext,
    vMAPIReadMail,
    vMAPISaveMail,
    vMAPIDeleteMail,
    vMAPIFreeBuffer,
    vMAPIAddress,
    vMAPIDetails,
    vMAPIResolveName    : FARPROC;

PROCEDURE MAPIinit () : BOOLEAN;
BEGIN
    IF NOT Initialized THEN
        DllInst := LoadLibrary ("MAPI32.DLL");
        IF DllInst = NULL_HINSTANCE THEN
            RETURN FALSE;
        END;
        Initialized := TRUE;

        vMAPILogon := GetProcAddress(DllInst, "MAPILogon");
        vMAPILogoff := GetProcAddress(DllInst, "MAPILogoff");
        vMAPISendMail := GetProcAddress(DllInst, "MAPISendMail");
        vMAPISendDocuments := GetProcAddress(DllInst, "MAPISendDocuments");
        vMAPIFindNext := GetProcAddress(DllInst, "MAPIFindNext");
        vMAPIReadMail := GetProcAddress(DllInst, "MAPIReadMail");
        vMAPISaveMail := GetProcAddress(DllInst, "MAPISaveMail");
        vMAPIDeleteMail := GetProcAddress(DllInst, "MAPIDeleteMail");
        vMAPIFreeBuffer := GetProcAddress(DllInst, "MAPIFreeBuffer");
        vMAPIAddress := GetProcAddress(DllInst, "MAPIAddress");
        vMAPIDetails := GetProcAddress(DllInst, "MAPIDetails");
        vMAPIResolveName := GetProcAddress(DllInst, "MAPIResolveName");
    END;
    RETURN TRUE;
END MAPIinit;

PROCEDURE MAPIexit;
VAR
    ok  : BOOLEAN;
BEGIN
    vMAPILogon := NILPROC;
    vMAPILogoff := NILPROC;
    vMAPISendMail := NILPROC;
    vMAPISendDocuments := NILPROC;
    vMAPIFindNext := NILPROC;
    vMAPIReadMail := NILPROC;
    vMAPISaveMail := NILPROC;
    vMAPIDeleteMail := NILPROC;
    vMAPIFreeBuffer := NILPROC;
    vMAPIAddress := NILPROC;
    vMAPIDetails := NILPROC;
    vMAPIResolveName := NILPROC;

    IF Initialized THEN
        Initialized := FALSE;
        ok := FreeLibrary(DllInst);
    END;
END MAPIexit;

PROCEDURE MAPILogon(ulUIParam : ULONG_PTR;
                    lpszName, lpszPassword : ARRAY OF CHAR;
                    flFlags : FLAGS;
                    ulReserved : ULONG;
                    VAR OUT lhSession : LHANDLE) : ULONG; PUREASM;
ASM
    jmp         vMAPILogon;
END MAPILogon;

PROCEDURE MAPILogoff(lhSession : LHANDLE;
                     ulUIParam : ULONG_PTR;
                     flFlags : FLAGS;
                     ulReserved : ULONG) : ULONG; PUREASM;
ASM
    jmp         vMAPILogoff;
END MAPILogoff;

PROCEDURE MAPISendMail(lhSession : LHANDLE;
                       ulUIParam : ULONG_PTR;
                       lpMessage : MapiMessage;
                       flFlags : FLAGS;
                       ulReserved : ULONG) : ULONG; PUREASM;
ASM
    jmp         vMAPISendMail;
END MAPISendMail;

PROCEDURE MAPISendDocuments(ulUIParam : ULONG_PTR;
                            lpszDelimChar, lpszFilePaths, lpszFileNames  : ARRAY OF CHAR;
                            ulReserved : ULONG) : ULONG; PUREASM;
ASM
    jmp         vMAPISendDocuments;
END MAPISendDocuments;

PROCEDURE MAPIFindNext(lhSession : LHANDLE;
                       ulUIParam : ULONG_PTR;
                       lpszMessageType, lpszSeedMessageID : ARRAY OF CHAR;
                       flFlags : FLAGS;
                       ulReserved : ULONG;
                       lpszMessageID : ARRAY OF CHAR) : ULONG; PUREASM;
ASM
    jmp         vMAPIFindNext;
END MAPIFindNext;

PROCEDURE MAPIReadMail(lhSession : LHANDLE;
                       ulUIParam : ULONG_PTR;
                       lpszMessageID : ARRAY OF CHAR;
                       flFlags : FLAGS;
                       ulReserved : ULONG;
                       VAR OUT lppMessageOut : lpMapiMessage) : ULONG; PUREASM;
ASM
    jmp         vMAPIReadMail;
END MAPIReadMail;

PROCEDURE MAPISaveMail(lhSession : LHANDLE;
                       ulUIParam : ULONG_PTR;
                       lpMessage : MapiMessage;
                       flFlags : FLAGS;
                       ulReserved : ULONG;
                       lpszMessageID : ARRAY OF CHAR) : ULONG; PUREASM;
ASM
    jmp         vMAPISaveMail;
END MAPISaveMail;

PROCEDURE MAPIDeleteMail(lhSession : LHANDLE;
                         ulUIParam : ULONG_PTR;
                         lpszMessageID : ARRAY OF CHAR;
                         flFlags : FLAGS;
                         ulReserved : ULONG) : ULONG; PUREASM;
ASM
    jmp         vMAPIDeleteMail;
END MAPIDeleteMail;

PROCEDURE MAPIFreeBuffer(pv : LPVOID) : ULONG; PUREASM;
ASM
    jmp         vMAPIFreeBuffer;
END MAPIFreeBuffer;

PROCEDURE MAPIAddress(lhSession : LHANDLE;
                      ulUIParam : ULONG_PTR;
                      lpszCaption : ARRAY OF CHAR;
                      nEditFields : ULONG;
                      lpszLabels : ARRAY OF CHAR;
                      nRecips : ULONG;
                      lpRecips : ARRAY OF MapiRecipDesc;
                      flFlags : FLAGS;
                      ulReserved : ULONG;
                      VAR OUT lpnNewRecips : ULONG;
                      VAR OUT lppNewRecips : lpMapiRecipDescArray) : ULONG; PUREASM;
ASM
    jmp         vMAPIAddress;
END MAPIAddress;

PROCEDURE MAPIDetails(lhSession : LHANDLE;
                      ulUIParam : ULONG_PTR;
                      lpRecips : MapiRecipDesc;
                      flFlags : FLAGS;
                      ulReserved : ULONG) : ULONG; PUREASM;
ASM
    jmp         vMAPIDetails;
END MAPIDetails;

PROCEDURE MAPIResolveName(lhSession : LHANDLE;
                          ulUIParam : ULONG_PTR;
                          lpszName : ARRAY OF CHAR;
                          flFlags : FLAGS;
                          ulReserved : ULONG;
                          VAR OUT lppRecip : lpMapiRecipDesc) : ULONG; PUREASM;
ASM
    jmp         vMAPIResolveName;
END MAPIResolveName;

BEGIN
    IF NOT IsThread THEN
        Initialized := FALSE;
        MAPIexit;
    END;

FINALLY
    IF NOT IsThread THEN
        MAPIexit;
    END;
END SimpleMAPI.
