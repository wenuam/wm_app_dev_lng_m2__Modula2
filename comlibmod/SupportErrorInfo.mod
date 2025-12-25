(*
Name:     SupportErrorInfo
Creation: 29-06-99
LastEdit: 29-06-99
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:
*)

UNSAFEGUARDED IMPLEMENTATION MODULE SupportErrorInfo;

IMPORT WIN32;
IMPORT WINERROR;
IMPORT Ole2;


<*/PUSH*>
<*/NOPACK*>
<*/ALIGN:8*>
<*/NOHIGH*>
<*/CALLS:WIN32SYSTEM*>
CLASS C;
  OVERRIDE PROCEDURE QueryInterface(    iid:      WIN32.IID;
                                    VAR iUnknown: Ole2.IUnknown): WIN32.HRESULT;
  BEGIN
    RETURN m_iUnknown.QueryInterface(iid,iUnknown);
  END QueryInterface;

  OVERRIDE PROCEDURE AddRef(): WIN32.ULONG;
  BEGIN
    RETURN m_iUnknown.AddRef();
  END AddRef;

  OVERRIDE PROCEDURE Release(): WIN32.ULONG;
  BEGIN
    RETURN m_iUnknown.Release();
  END Release;

  OVERRIDE PROCEDURE InterfaceSupportsErrorInfo(iid: WIN32.IID): WIN32.HRESULT;

  VAR hresult: WIN32.HRESULT;

  BEGIN
    IF Ole2.IsEqualGUID(iid,m_iid) THEN
      hresult:=WINERROR.S_OK;
    ELSE
      hresult:=WINERROR.S_FALSE;
    END; (* if *)
    RETURN hresult;
  END InterfaceSupportsErrorInfo;

END C;
<*/POP*>


END SupportErrorInfo.
