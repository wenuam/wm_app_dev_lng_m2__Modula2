(*
Name:     DispInterface
Creation: 24-03-2000
LastEdit: 24-03-2000
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:
*)

UNSAFEGUARDED IMPLEMENTATION MODULE DispInterface;

IMPORT SYSTEM;
IMPORT WIN32;
IMPORT WINERROR;
IMPORT RPC;
IMPORT Ole2;
IMPORT OleException;


CLASS C;
  PROCEDURE Init(clsid: WIN32.GUID);
  BEGIN
    bValid:=WINERROR.SUCCEEDED(Ole2.CoCreateInstance(clsid,EMPTY,Ole2.CLSCTX_ALL,Ole2.IID_IDispatch,iDispatch:Ole2.IUnknown));
  END Init;

  PROCEDURE Invoke(    szuName:    ARRAY OF UCHAR;
                   VAR INOUT dispId: Ole2.DISPID;
                       wFlags:     WIN32.WORD;
                       dispparams: Ole2.DISPPARAMS;
                   VAR vResult:    Ole2.VARIANT): WIN32.HRESULT;

  VAR hresult: WIN32.HRESULT;
      cError:  WIN32.UINT;
      exinfo:  Ole2.EXCEPINFO;
      lpNames: ARRAY [0..0] OF Ole2.LPOLESTR;

  BEGIN
    hresult:=WINERROR.S_FALSE;
    lpNames[0]:=SYSTEM.ADR(szuName);
    IF  (iDispatch<>EMPTY)
    AND (   (dispId<>Ole2.DISPID_UNKNOWN)
         OR (iDispatch.GetIDsOfNames(RPC.IID_NULL,lpNames,1,0,dispId)=WINERROR.S_OK)) THEN
      hresult:=iDispatch.Invoke(dispId,RPC.IID_NULL,0,wFlags,dispparams,vResult,exinfo,cError);
      IF (hresult=WINERROR.DISP_E_EXCEPTION) THEN
        OleException.Show(exinfo);
      ELSIF (hresult<>WINERROR.S_OK) THEN
        OleException.ShowResult(hresult);
      END; (* if *)
    END; (* if *)
    RETURN hresult;
  END Invoke;

BEGIN
  bValid:=FALSE;
  iDispatch:=EMPTY;
FINALLY
  IF (iDispatch<>EMPTY) THEN
    SYSTEM.FUNC iDispatch.Release();
  END; (* if *)
END C;


END DispInterface.
