(*
Name:     ComRegister
Creation: 16-12-1998
LastEdit: 07-02-2002
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:
*)

IMPLEMENTATION MODULE ComRegister;

IMPORT SYSTEM;
IMPORT WINX;
IMPORT WIN32;
IMPORT WINREG;
IMPORT WINERROR;
IMPORT Strings;

CONST szCLSIDpost=    'CLSID\';
      szCLSIDpre=     '\CLSID';
      szAPPIDpost=    'AppID\';
      szINPROC32=     '\InProcServer32';
      szLOCAL32=      '\LocalServer32';
      szINDEPENDENT=  '\VersionIndependentProgID';
      szPROGID=       '\ProgID';
      szCURVER=       '\CurVer';


(* registers a DLL in the registry *)
(* szServer is name of server, e.g. 'Server' *)
(* szClass is name of class, e.g 'Example' *)
(* szVersion is version of class, e.g. '1' *)
(* szCLSID is GUID of class, e.g. '{2EEE...C0C7}' *)
PROCEDURE RegisterDLL(szServer:  ARRAY OF CHAR;
                      szClass:   ARRAY OF CHAR;
                      szVersion: ARRAY OF CHAR;
                      szCLSID:   ARRAY OF CHAR): BOOLEAN;

VAR szDLL:  ARRAY [0..260] OF CHAR;
    bValid: BOOLEAN;

BEGIN
  bValid:=FALSE;
  SYSTEM.FUNC WIN32.GetModuleFileName(WINX.Instance,szDLL,HIGH(szDLL));
  IF RegisterCLSID(szServer,szClass,szCLSID) THEN
    (* specific for in process DLL servers *)
    (* [HKEY_CLASSES_ROOT\CLSID\{2EEE...C0C7}\InProcServer32] *)
    (* @="e:\StonyBrook\ComServer\Server.DLL" *)
    IF RegisterWrite(szCLSIDpost,szCLSID,szINPROC32,szDLL) THEN
      bValid:=RegisterProg(szServer,szClass,szVersion,szCLSID);
    END; (* if *)
  END; (* if *)
  RETURN bValid;
END RegisterDLL;


(* un-registers a DLL from the registry *)
(* entries in Registry are removed in reversed order *)
PROCEDURE UnRegisterDLL(szServer:  ARRAY OF CHAR;
                        szClass:   ARRAY OF CHAR;
                        szVersion: ARRAY OF CHAR;
                        szCLSID:   ARRAY OF CHAR): BOOLEAN;

VAR bValid: BOOLEAN;

BEGIN
  bValid:=UnRegisterProg(szServer,szClass,szVersion,szCLSID);
  (* [HKEY_CLASSES_ROOT\CLSID\{2EEE...C0C7}\InProcServer32] *)
  bValid:=RegisterDelete(szCLSIDpost,szCLSID,szINPROC32) AND bValid;
  (* [HKEY_CLASSES_ROOT\CLSID\{2EEE...C0C7}] *)
  bValid:=RegisterDelete(szCLSIDpost,szCLSID,'') AND bValid;
  RETURN bValid;
END UnRegisterDLL;


(* registers a EXE in the registry *)
PROCEDURE RegisterEXE(szServer:  ARRAY OF CHAR;
                      szClass:   ARRAY OF CHAR;
                      szVersion: ARRAY OF CHAR;
                      szCLSID:   ARRAY OF CHAR): BOOLEAN;

VAR szEXE:  ARRAY [0..260] OF CHAR;
    bValid: BOOLEAN;

BEGIN
  bValid:=FALSE;
  SYSTEM.FUNC WIN32.GetModuleFileName(WINX.Instance,szEXE,HIGH(szEXE));
  IF RegisterCLSID(szServer,szClass,szCLSID) THEN
    (* specific for local servers *)
    (* [HKEY_CLASSES_ROOT\CLSID\{2EEE...C0C7}\LocalServer32] *)
    (* @="e:\StonyBrook\ComServer\Server.EXE" *)
    (* [HKEY_CLASSES_ROOT\CLSID\{2EEE...C0C7}\InProcHandler32] *)
    (* @="ole32.dll" *)
    IF RegisterWrite(szCLSIDpost,szCLSID,szLOCAL32,szEXE) THEN
      bValid:=RegisterProg(szServer,szClass,szVersion,szCLSID);
    END; (* if *)
  END; (* if *)
  RETURN bValid;
END RegisterEXE;


(* un-registers a EXE from the registry *)
(* entries in Registry are removed in reversed order *)
PROCEDURE UnRegisterEXE(szServer:  ARRAY OF CHAR;
                        szClass:   ARRAY OF CHAR;
                        szVersion: ARRAY OF CHAR;
                        szCLSID:   ARRAY OF CHAR): BOOLEAN;

VAR bValid: BOOLEAN;

BEGIN
  bValid:=UnRegisterProg(szServer,szClass,szVersion,szCLSID);
  (* [HKEY_CLASSES_ROOT\CLSID\{2EEE...C0C7}\InProcServer32] *)
  bValid:=RegisterDelete(szCLSIDpost,szCLSID,szLOCAL32) AND bValid;
  (* [HKEY_CLASSES_ROOT\CLSID\{2EEE...C0C7}] *)
  bValid:=RegisterDelete(szCLSIDpost,szCLSID,'') AND bValid;
  RETURN bValid;
END UnRegisterEXE;


(* un-registers typelib from registry *)
PROCEDURE UnRegisterTypeLib(szTypeLib: ARRAY OF CHAR);
BEGIN
  IF (szTypeLib[0]<>0C) THEN
    (* [HKEY_CLASSES_ROOT\TypeLib\{2EEE...C0C7}] *)
    SYSTEM.FUNC RegisterDelete('TypeLib\',szTypeLib,'');
  END; (* if *)
END UnRegisterTypeLib;


(* registers CLSID *)
(* [HKEY_CLASSES_ROOT\CLSID\{2EEE...C0C7}] *)
(* @="Server Exaple" *)
PROCEDURE RegisterCLSID(szServer: ARRAY OF CHAR;
                        szClass:  ARRAY OF CHAR;
                        szCLSID:  ARRAY OF CHAR): BOOLEAN;

VAR szDesc: ARRAY [0..80] OF CHAR;

BEGIN
  Strings.Concat(szServer,' ',szDesc);
  Strings.Append(szClass,szDesc);
  RETURN RegisterWrite(szCLSIDpost,szCLSID,'',szDesc);
END RegisterCLSID;


(* adds support for ProgIDFromCLSID and CLSIDfromProgID *)
(* [HKEY_CLASSES_ROOT\CLSID\{2EEE...C0C7}] *)
(*   VersionIndependantProgId="Server.Example" *)
(*   ProgId="Server.Example.1" *)
(* [HKEY_CLASSES_ROOT\Server.Example\] *)
(*   @="Example" *)
(*   CLSID="{2EEEB790-DC3C-11d1-BBC6-0020AFE0C0C7}" *)
(*   CurVer="Server.Example.1" *)
(* [HKEY_CLASSES_ROOT\Server.Example.1] *)
(*   @="Example" *)
(*   CLSID="{2EEEB790-DC3C-11d1-BBC6-0020AFE0C0C7}" *)
PROCEDURE RegisterProg(szServer:  ARRAY OF CHAR;
                       szClass:   ARRAY OF CHAR;
                       szVersion: ARRAY OF CHAR;
                       szCLSID:   ARRAY OF CHAR): BOOLEAN;

VAR szProgID:  ARRAY [0..260] OF CHAR;
    szCurrent: ARRAY [0..260] OF CHAR;
    bValid:    BOOLEAN;

BEGIN
  MakeProgID(szServer,szClass,szVersion,szProgID,szCurrent);
  IF  RegisterWrite(szCLSIDpost,szCLSID,szINDEPENDENT,szProgID)
  AND RegisterWrite(szCLSIDpost,szCLSID,szPROGID,szCurrent)
  AND RegisterWrite('',szProgID,'',szClass)
  AND RegisterWrite('',szProgID,szCLSIDpre,szCLSID)
  AND RegisterWrite('',szProgID,szCURVER,szCurrent)
  AND RegisterWrite('',szCurrent,'',szClass)
  AND RegisterWrite('',szCurrent,szCLSIDpre,szCLSID) THEN
    bValid:=TRUE;
  ELSE
    bValid:=FALSE;
  END; (* if *)
  RETURN bValid;
END RegisterProg;


(* un-registers Prog, inverse of above *)
PROCEDURE UnRegisterProg(szServer:  ARRAY OF CHAR;
                         szClass:   ARRAY OF CHAR;
                         szVersion: ARRAY OF CHAR;
                         szCLSID:   ARRAY OF CHAR): BOOLEAN;

VAR szProgID:  ARRAY [0..260] OF CHAR;
    szCurrent: ARRAY [0..260] OF CHAR;
    bValid:    BOOLEAN;

BEGIN
  MakeProgID(szServer,szClass,szVersion,szProgID,szCurrent);
  IF  RegisterDelete(szCLSIDpost,szCLSID,szINDEPENDENT)
  AND RegisterDelete(szCLSIDpost,szCLSID,szPROGID)
  AND RegisterDelete('',szProgID,szCLSIDpre)
  AND RegisterDelete('',szProgID,szCURVER)
  AND RegisterDelete('',szProgID,'')
  AND RegisterDelete('',szCurrent,szCLSIDpre)
  AND RegisterDelete('',szCurrent,'')
  AND RegisterDelete(szAPPIDpost,szCLSID,'') THEN
    bValid:=TRUE;
  ELSE
    bValid:=FALSE;
  END; (* if *)
  RETURN bValid;
END UnRegisterProg;


(* un-registers interface *)
(* [HKEY_CLASSES_ROOT\Interface\{2EEE...C0C7}] *)
PROCEDURE UnRegisterInterface(szGUID: ARRAY OF CHAR);

VAR szKey: ARRAY [0..260] OF CHAR;

BEGIN
  Strings.Concat('Interface\',szGUID,szKey);
  SYSTEM.FUNC WINREG.RegDeleteKey(WINREG.HKEY_CLASSES_ROOT,szKey);
END UnRegisterInterface;


(* writes stuff for registration *)
PROCEDURE RegisterWrite(szPre:   ARRAY OF CHAR;
                        szMain:  ARRAY OF CHAR;
                        szPost:  ARRAY OF CHAR;
                        szValue: ARRAY OF CHAR): BOOLEAN;

VAR hkey:   WINREG.HKEY;
    szKey:  ARRAY [0..260] OF CHAR;
    bValid: BOOLEAN;

BEGIN
  Strings.Concat(szPre,szMain,szKey);
  Strings.Append(szPost,szKey);
  IF (WINREG.RegCreateKey(WINREG.HKEY_CLASSES_ROOT,szKey,hkey)=WINERROR.ERROR_SUCCESS) THEN
    bValid:=(WINREG.RegSetValueEx(hkey,'',0,WIN32.REG_SZ,szValue,Strings.Length(szValue)*SIZE(CHAR))=WINERROR.ERROR_SUCCESS);
    SYSTEM.FUNC WINREG.RegCloseKey(hkey);
  ELSE
    bValid:=FALSE;
  END; (* if *)
  RETURN bValid;
END RegisterWrite;


(* deletes stuff for registration *)
PROCEDURE RegisterDelete(szPre:  ARRAY OF CHAR;
                         szMain: ARRAY OF CHAR;
                         szPost: ARRAY OF CHAR): BOOLEAN;

VAR szKey: ARRAY [0..260] OF CHAR;

BEGIN
  Strings.Concat(szPre,szMain,szKey);
  Strings.Append(szPost,szKey);
  RETURN (WINREG.RegDeleteKey(WINREG.HKEY_CLASSES_ROOT,szKey)=WINERROR.ERROR_SUCCESS);
END RegisterDelete;


(* makes ProgID *)
PROCEDURE MakeProgID(    szServer:  ARRAY OF CHAR;
                         szClass:   ARRAY OF CHAR;
                         szVersion: ARRAY OF CHAR;
                     VAR szProgID:  ARRAY OF CHAR;
                     VAR szCurrent: ARRAY OF CHAR);
BEGIN
  Strings.Concat(szServer,'.',szProgID);
  Strings.Append(szClass,szProgID);
  Strings.Concat(szProgID,'.',szCurrent);
  Strings.Append(szVersion,szCurrent);
END MakeProgID;



END ComRegister.
