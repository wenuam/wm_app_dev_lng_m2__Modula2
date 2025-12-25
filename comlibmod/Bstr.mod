(*
Name:     Bstr
Creation: 28-10-1998
LastEdit: 09-04-2001
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:  Implements BSTR procedures that can be used for
          ASCII character strings. Most of these procedures can also
          be found in Ole2 for OLECHAR character strings.
*)

IMPLEMENTATION MODULE Bstr;

IMPORT SYSTEM;
IMPORT WINX;
IMPORT Ole2;
%IF UNICODE %THEN
  IMPORT MemUtils;
%ELSE
  IMPORT WINNLS;
  IMPORT Strings;
%END


(* Allocates a new string and copies the passed string into it. *)
(* Returns NULL if insufficient memory exists or if NULL is passed in. *)
PROCEDURE Create(szText: ARRAY OF CHAR): T;
%IF UNICODE %THEN
  BEGIN
    RETURN Ole2.SysAllocString(szText);
  END Create;
%ELSE
  VAR cLength: CARDINAL;
      bstr:    T;

  BEGIN
    cLength:=Strings.Length(szText);
    bstr:=Ole2.SysAllocStringLen(WINX.NIL_USTR,cLength);
    SYSTEM.FUNC WINNLS.MultiByteToWideChar(WINNLS.CP_ACP,WINNLS.MB_PRECOMPOSED,szText,-1,bstr^,cLength);
    RETURN bstr;
  END Create;
%END


(* creates empty Bstr of specified length *)
PROCEDURE Create2(cLength: CARDINAL): T;
BEGIN
  RETURN Ole2.SysAllocStringLen(WINX.NIL_USTR,cLength);
END Create2;


(* returns a copy of bstr *)
PROCEDURE Copy(bstr: T): T;
BEGIN
  RETURN Ole2.SysAllocString(bstr^);
END Copy;


(* Allocates a new BSTR and copies the passed string into it, *)
(* then frees the BSTR currently referenced by pbstr and resets *)
(* pbstr to point to the new BSTR. *)
PROCEDURE ReSize(VAR bstr:   T;
                     szText: ARRAY OF CHAR);
%IF UNICODE %THEN
  BEGIN
    SYSTEM.FUNC Ole2.SysReAllocString(bstr,szText);
  END ReSize;
%ELSE
  VAR cLength: CARDINAL;
      bstrNew: T;

  BEGIN
    cLength:=Strings.Length(szText);
    bstrNew:=Ole2.SysAllocStringLen(WINX.NIL_USTR,cLength);
    IF (bstrNew<>NIL) THEN
      SYSTEM.FUNC WINNLS.MultiByteToWideChar(WINNLS.CP_ACP,WINNLS.MB_PRECOMPOSED,szText,-1,bstrNew^,cLength);
      Ole2.SysFreeString(bstr);
      bstr:=bstrNew;
    END; (* if *)
  END ReSize;
%END


(* Frees a string previously allocated by SysAllocString, *)
(* SysAllocStringByteLen, SysReAllocString, SysAllocStringLen, *)
(* or SysReAllocStringLen. *)
PROCEDURE Dispose(VAR INOUT bstr: T);
BEGIN
  IF (bstr<>NIL) THEN
    Ole2.SysFreeString(bstr);
    bstr:=NIL;
  END; (* if *)
END Dispose;


(* Returns the length of a BSTR. *)
PROCEDURE Length(bstr: T): CARDINAL;
BEGIN
  RETURN Ole2.SysStringLen(bstr);
END Length;


(* returns contents of bstr as ASCII *)
PROCEDURE ToASCII(    bstr:   T;
                  VAR szText: ARRAY OF CHAR);

%IF UNICODE %THEN
  VAR cLength: CARDINAL;
%END

BEGIN
  IF (bstr=NIL) THEN
    szText:='';
  ELSE
    %IF UNICODE %THEN
      cLength:=Length(bstr)+1;
      IF (cLength>HIGH(szText)) THEN
        cLength:=HIGH(szText);
      END; (* if *)
      MemUtils.MoveMem(szText,bstr^,cLength*SIZE(CHAR));
    %ELSE
      SYSTEM.FUNC WINNLS.WideCharToMultiByte(WINNLS.CP_ACP,WINNLS.WC_COMPOSITECHECK,bstr^,-1,szText,HIGH(szText),WINX.NIL_STR,WINX.NIL_BOOL);
    %END
  END; (* if *)
END ToASCII;



END Bstr.
