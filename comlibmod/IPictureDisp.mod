(*
Name:     IPictureDisp
Creation: 04-03-2000
LastEdit: 04-03-2000
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:
*)

UNSAFEGUARDED IMPLEMENTATION MODULE IPictureDisp;

IMPORT SYSTEM;
IMPORT WIN32;
IMPORT WINERROR;
IMPORT RPC;
IMPORT Ole2;
IMPORT OleC;
IMPORT OleException;

CONST DISPID_PICT_HANDLE= 0;
      DISPID_PICT_HPAL=   2;
      DISPID_PICT_TYPE=   3;
      DISPID_PICT_WIDTH=  4;
      DISPID_PICT_HEIGHT= 5;
      DISPID_PICT_RENDER= 6;

<*/CALLS:WIN32SYSTEM*>
<*/NOHIGH*>
<*/ALIGN:8*>
<*/NOPACK*>
CLASS C;
  PROCEDURE Init(iPictureDisp: Ole2.IDispatch);
  BEGIN
    IF (iDispatch<>EMPTY) THEN
      SYSTEM.FUNC iDispatch.Release();
    END; (* if *)
    iDispatch:=iPictureDisp;
    SYSTEM.FUNC iDispatch.AddRef();
  END Init;

  PROCEDURE get_Handle(VAR handle: OleC.OLE_HANDLE): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_PICT_HANDLE,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      handle:=vResult.lVal;
    END; (* if *)
    RETURN hresult;
  END get_Handle;

  PROCEDURE get_hPal(VAR hPal: OleC.OLE_HANDLE): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_PICT_HPAL,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      hPal:=vResult.lVal;
    END; (* if *)
    RETURN hresult;
  END get_hPal;

  PROCEDURE set_hPal(hPal: OleC.OLE_HANDLE): WIN32.HRESULT;

  VAR dispIdParam: Ole2.DISPID;
      dispparams:  Ole2.DISPPARAMS;
      vInput:      Ole2.VARIANT;
      vResult:     Ole2.VARIANT;

  BEGIN
    vInput.vt:=Ole2.VT_UI4;
    vInput.lVal:=hPal;
    dispIdParam:=Ole2.DISPID_PROPERTYPUT;
    dispparams.cArgs:=1;
    dispparams.rgvarg:=SYSTEM.ADR(vInput);
    dispparams.cNamedArgs:=1;
    dispparams.rgdispidNamedArgs:=SYSTEM.ADR(dispIdParam);
    RETURN Invoke(DISPID_PICT_HANDLE,Ole2.DISPATCH_PROPERTYPUT,dispparams,vResult);
  END set_hPal;

  PROCEDURE get_Type(VAR nType: WIN32.SHORT): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_PICT_TYPE,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      nType:=vResult.iVal;
    END; (* if *)
    RETURN hresult;
  END get_Type;

  PROCEDURE get_Width(VAR lWidth: OleC.OLE_XSIZE_HIMETRIC): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_PICT_WIDTH,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      lWidth:=vResult.lVal;
    END; (* if *)
    RETURN hresult;
  END get_Width;

  PROCEDURE get_Height(VAR lHeight: OleC.OLE_YSIZE_HIMETRIC): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_PICT_HEIGHT,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      lHeight:=vResult.lVal;
    END; (* if *)
    RETURN hresult;
  END get_Height;

  PROCEDURE Render(hdc: WIN32.HDC;
                   x:   WIN32.LONG;
                   y:   WIN32.LONG;
                   cx:  WIN32.LONG;
                   cy:  WIN32.LONG;
                   xSrc:  OleC.OLE_XPOS_HIMETRIC;
                   ySrc:  OleC.OLE_YPOS_HIMETRIC;
                   cxSrc: OleC.OLE_XSIZE_HIMETRIC;
                   cySrc: OleC.OLE_YSIZE_HIMETRIC;
                   rectBounds: WIN32.RECT): WIN32.HRESULT;

  VAR dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;
      vArgs:      ARRAY [0..9] OF Ole2.VARIANT;

  BEGIN
    vArgs[0].vt:=Ole2.VT_PTR;
    vArgs[0].byref:=hdc;
    vArgs[1].vt:=Ole2.VT_I4;
    vArgs[1].lVal:=x;
    vArgs[2].vt:=Ole2.VT_I4;
    vArgs[2].lVal:=y;
    vArgs[3].vt:=Ole2.VT_I4;
    vArgs[3].lVal:=cx;
    vArgs[4].vt:=Ole2.VT_I4;
    vArgs[4].lVal:=cy;
    vArgs[5].vt:=Ole2.VT_I4;
    vArgs[5].lVal:=xSrc;
    vArgs[6].vt:=Ole2.VT_I4;
    vArgs[6].lVal:=ySrc;
    vArgs[7].vt:=Ole2.VT_I4;
    vArgs[7].lVal:=cxSrc;
    vArgs[8].vt:=Ole2.VT_I4;
    vArgs[8].lVal:=cySrc;
    vArgs[9].vt:=Ole2.VT_PTR;
    vArgs[9].byref:=SYSTEM.ADR(rectBounds);
    dispparams.cArgs:=10;
    dispparams.rgvarg:=SYSTEM.ADR(vArgs);
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    RETURN Invoke(DISPID_PICT_RENDER,Ole2.DISPATCH_METHOD,dispparams,vResult);
  END Render;

  PROCEDURE Invoke(    dispId:     Ole2.DISPID;
                       wFlags:     WIN32.WORD;
                       dispparams: Ole2.DISPPARAMS;
                   VAR vResult:    Ole2.VARIANT): WIN32.HRESULT;

  VAR hresult: WIN32.HRESULT;
      cError:  WIN32.UINT;
      exinfo:  Ole2.EXCEPINFO;

  BEGIN
    hresult:=WINERROR.S_FALSE;
    IF (iDispatch<>EMPTY) THEN
      hresult:=iDispatch.Invoke(dispId,RPC.IID_NULL,0,wFlags,dispparams,vResult,exinfo,cError);
      IF (hresult=WINERROR.DISP_E_EXCEPTION) THEN
        OleException.Show(exinfo);
      END; (* if *)
    END; (* if *)
    RETURN hresult;
  END Invoke;

BEGIN
  iDispatch:=EMPTY;
FINALLY
  IF (iDispatch<>EMPTY) THEN
    SYSTEM.FUNC iDispatch.Release();
  END; (* if *)
END C;

END IPictureDisp.
