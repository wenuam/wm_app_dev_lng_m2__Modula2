(*
Name:     IFontDisp
Creation: 29-02-2000
LastEdit: 29-02-2000
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:  interface to IFontDisp objects.
*)

UNSAFEGUARDED IMPLEMENTATION MODULE IFontDisp;

IMPORT SYSTEM;
IMPORT WIN32;
IMPORT WINERROR;
IMPORT RPC;
IMPORT Ole2;
IMPORT OleException;
IMPORT Bstr;

CONST DISPID_FONT_NAME=    0;
      DISPID_FONT_SIZE=    2;
      DISPID_FONT_BOLD=    3;
      DISPID_FONT_ITALIC=  4;
      DISPID_FONT_UNDER=   5;
      DISPID_FONT_STRIKE=  6;
      DISPID_FONT_WEIGHT=  7;
      DISPID_FONT_CHARSET= 8;

<*/CALLS:WIN32SYSTEM*>
<*/NOHIGH*>
<*/ALIGN:8*>
<*/NOPACK*>
CLASS C;
  PROCEDURE Init(iFontDisp: Ole2.IDispatch);
  BEGIN
    IF (iDispatch<>EMPTY) THEN
      SYSTEM.FUNC iDispatch.Release();
    END; (* if *)
    iDispatch:=iFontDisp;
    SYSTEM.FUNC iDispatch.AddRef();
  END Init;

  PROCEDURE get_Name(VAR bstr: Bstr.T): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_FONT_NAME,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      bstr:=vResult.bstrVal;
    END; (* if *)
    RETURN hresult;
  END get_Name;

  PROCEDURE put_Name(bstr: Bstr.T): WIN32.HRESULT;

  VAR dispIdParam: Ole2.DISPID;
      dispparams:  Ole2.DISPPARAMS;
      vInput:      Ole2.VARIANT;
      vResult:     Ole2.VARIANT;

  BEGIN
    vInput.vt:=Ole2.VT_BSTR;
    vInput.bstrVal:=bstr;
    dispIdParam:=Ole2.DISPID_PROPERTYPUT;
    dispparams.cArgs:=1;
    dispparams.rgvarg:=SYSTEM.ADR(vInput);
    dispparams.cNamedArgs:=1;
    dispparams.rgdispidNamedArgs:=SYSTEM.ADR(dispIdParam);
    RETURN Invoke(DISPID_FONT_NAME,Ole2.DISPATCH_PROPERTYPUT,dispparams,vResult);
  END put_Name;

  PROCEDURE get_Size(VAR cySize: Ole2.CY): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_FONT_SIZE,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      cySize:=vResult.cyVal;
    END; (* if *)
    RETURN hresult;
  END get_Size;

  PROCEDURE put_Size(cySize: Ole2.CY): WIN32.HRESULT;

  VAR dispIdParam: Ole2.DISPID;
      dispparams:  Ole2.DISPPARAMS;
      vInput:      Ole2.VARIANT;
      vResult:     Ole2.VARIANT;

  BEGIN
    vInput.vt:=Ole2.VT_CY;
    vInput.cyVal:=cySize;
    dispIdParam:=Ole2.DISPID_PROPERTYPUT;
    dispparams.cArgs:=1;
    dispparams.rgvarg:=SYSTEM.ADR(vInput);
    dispparams.cNamedArgs:=1;
    dispparams.rgdispidNamedArgs:=SYSTEM.ADR(dispIdParam);
    RETURN Invoke(DISPID_FONT_SIZE,Ole2.DISPATCH_PROPERTYPUT,dispparams,vResult);
  END put_Size;

  PROCEDURE get_Bold(VAR bBold: Ole2.VARIANT_BOOL): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_FONT_BOLD,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      bBold:=vResult.bool;
    END; (* if *)
    RETURN hresult;
  END get_Bold;

  PROCEDURE put_Bold(bBold: Ole2.VARIANT_BOOL): WIN32.HRESULT;

  VAR dispIdParam: Ole2.DISPID;
      dispparams:  Ole2.DISPPARAMS;
      vInput:      Ole2.VARIANT;
      vResult:     Ole2.VARIANT;

  BEGIN
    vInput.vt:=Ole2.VT_BOOL;
    vInput.bool:=bBold;
    dispIdParam:=Ole2.DISPID_PROPERTYPUT;
    dispparams.cArgs:=1;
    dispparams.rgvarg:=SYSTEM.ADR(vInput);
    dispparams.cNamedArgs:=1;
    dispparams.rgdispidNamedArgs:=SYSTEM.ADR(dispIdParam);
    RETURN Invoke(DISPID_FONT_BOLD,Ole2.DISPATCH_PROPERTYPUT,dispparams,vResult);
  END put_Bold;

  PROCEDURE get_Italic(VAR bItalic: Ole2.VARIANT_BOOL): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_FONT_ITALIC,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      bItalic:=vResult.bool;
    END; (* if *)
    RETURN hresult;
  END get_Italic;

  PROCEDURE put_Italic(bItalic: Ole2.VARIANT_BOOL): WIN32.HRESULT;

  VAR dispIdParam: Ole2.DISPID;
      dispparams:  Ole2.DISPPARAMS;
      vInput:      Ole2.VARIANT;
      vResult:     Ole2.VARIANT;

  BEGIN
    vInput.vt:=Ole2.VT_BOOL;
    vInput.bool:=bItalic;
    dispIdParam:=Ole2.DISPID_PROPERTYPUT;
    dispparams.cArgs:=1;
    dispparams.rgvarg:=SYSTEM.ADR(vInput);
    dispparams.cNamedArgs:=1;
    dispparams.rgdispidNamedArgs:=SYSTEM.ADR(dispIdParam);
    RETURN Invoke(DISPID_FONT_ITALIC,Ole2.DISPATCH_PROPERTYPUT,dispparams,vResult);
  END put_Italic;

  PROCEDURE get_Underline(VAR bUnderline: Ole2.VARIANT_BOOL): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_FONT_UNDER,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      bUnderline:=vResult.bool;
    END; (* if *)
    RETURN hresult;
  END get_Underline;

  PROCEDURE put_Underline(bUnderline: Ole2.VARIANT_BOOL): WIN32.HRESULT;

  VAR dispIdParam: Ole2.DISPID;
      dispparams:  Ole2.DISPPARAMS;
      vInput:      Ole2.VARIANT;
      vResult:     Ole2.VARIANT;

  BEGIN
    vInput.vt:=Ole2.VT_BOOL;
    vInput.bool:=bUnderline;
    dispIdParam:=Ole2.DISPID_PROPERTYPUT;
    dispparams.cArgs:=1;
    dispparams.rgvarg:=SYSTEM.ADR(vInput);
    dispparams.cNamedArgs:=1;
    dispparams.rgdispidNamedArgs:=SYSTEM.ADR(dispIdParam);
    RETURN Invoke(DISPID_FONT_UNDER,Ole2.DISPATCH_PROPERTYPUT,dispparams,vResult);
  END put_Underline;

  PROCEDURE get_Strikethrough(VAR bStrikethrough: Ole2.VARIANT_BOOL): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_FONT_STRIKE,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      bStrikethrough:=vResult.bool;
    END; (* if *)
    RETURN hresult;
  END get_Strikethrough;

  PROCEDURE put_Strikethrough(bStrikethrough: Ole2.VARIANT_BOOL): WIN32.HRESULT;

  VAR dispIdParam: Ole2.DISPID;
      dispparams:  Ole2.DISPPARAMS;
      vInput:      Ole2.VARIANT;
      vResult:     Ole2.VARIANT;

  BEGIN
    vInput.vt:=Ole2.VT_BOOL;
    vInput.bool:=bStrikethrough;
    dispIdParam:=Ole2.DISPID_PROPERTYPUT;
    dispparams.cArgs:=1;
    dispparams.rgvarg:=SYSTEM.ADR(vInput);
    dispparams.cNamedArgs:=1;
    dispparams.rgdispidNamedArgs:=SYSTEM.ADR(dispIdParam);
    RETURN Invoke(DISPID_FONT_STRIKE,Ole2.DISPATCH_PROPERTYPUT,dispparams,vResult);
  END put_Strikethrough;

  PROCEDURE get_Weight(VAR nWeight: WIN32.SHORT): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_FONT_WEIGHT,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      nWeight:=vResult.iVal;
    END; (* if *)
    RETURN hresult;
  END get_Weight;

  PROCEDURE put_Weight(nWeight: WIN32.SHORT): WIN32.HRESULT;

  VAR dispIdParam: Ole2.DISPID;
      dispparams:  Ole2.DISPPARAMS;
      vInput:      Ole2.VARIANT;
      vResult:     Ole2.VARIANT;

  BEGIN
    vInput.vt:=Ole2.VT_I2;
    vInput.iVal:=nWeight;
    dispIdParam:=Ole2.DISPID_PROPERTYPUT;
    dispparams.cArgs:=1;
    dispparams.rgvarg:=SYSTEM.ADR(vInput);
    dispparams.cNamedArgs:=1;
    dispparams.rgdispidNamedArgs:=SYSTEM.ADR(dispIdParam);
    RETURN Invoke(DISPID_FONT_WEIGHT,Ole2.DISPATCH_PROPERTYPUT,dispparams,vResult);
  END put_Weight;

  PROCEDURE get_Charset(VAR nCharset: WIN32.SHORT): WIN32.HRESULT;

  VAR hresult:    WIN32.HRESULT;
      dispparams: Ole2.DISPPARAMS;
      vResult:    Ole2.VARIANT;

  BEGIN
    dispparams.cArgs:=0;
    dispparams.rgvarg:=NIL;
    dispparams.cNamedArgs:=0;
    dispparams.rgdispidNamedArgs:=NIL;
    hresult:=Invoke(DISPID_FONT_CHARSET,Ole2.DISPATCH_PROPERTYGET,dispparams,vResult);
    IF WINERROR.SUCCEEDED(hresult) THEN
      nCharset:=vResult.iVal;
    END; (* if *)
    RETURN hresult;
  END get_Charset;

  PROCEDURE put_Charset(nCharset: WIN32.SHORT): WIN32.HRESULT;

  VAR dispIdParam: Ole2.DISPID;
      dispparams:  Ole2.DISPPARAMS;
      vInput:      Ole2.VARIANT;
      vResult:     Ole2.VARIANT;

  BEGIN
    vInput.vt:=Ole2.VT_I2;
    vInput.iVal:=nCharset;
    dispIdParam:=Ole2.DISPID_PROPERTYPUT;
    dispparams.cArgs:=1;
    dispparams.rgvarg:=SYSTEM.ADR(vInput);
    dispparams.cNamedArgs:=1;
    dispparams.rgdispidNamedArgs:=SYSTEM.ADR(dispIdParam);
    RETURN Invoke(DISPID_FONT_CHARSET,Ole2.DISPATCH_PROPERTYPUT,dispparams,vResult);
  END put_Charset;

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



END IFontDisp.
