(*
Name:     ConnectionPoint
Creation: 02-03-2000
LastEdit: 02-03-2000
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:
*)

UNSAFEGUARDED IMPLEMENTATION MODULE ConnectionPoint;

IMPORT SYSTEM;
IMPORT WIN32;
IMPORT WINERROR;
IMPORT Ole2;
IMPORT OleC;


CLASS C;
  (* connects a source interface on a sink object to a connectable object *)
  PROCEDURE Connect(iUnknownSource: Ole2.IUnknown;
                    iUnknownSink:   Ole2.IUnknown;
                    iidSink:        WIN32.IID): BOOLEAN;

  VAR hresult:      WIN32.HRESULT;
      iCPContainer: OleC.IConnectionPointContainer;
      bValid:       BOOLEAN;

  BEGIN
    bValid:=FALSE;
    hresult:=iUnknownSource.QueryInterface(OleC.IID_IConnectionPointContainer,iCPContainer:Ole2.IUnknown);
    IF WINERROR.SUCCEEDED(hresult) THEN
      hresult:=iCPContainer.FindConnectionPoint(iidSink,iConnectionPoint);
      IF WINERROR.SUCCEEDED(hresult) THEN
        hresult:=iConnectionPoint.Advise(iUnknownSink,dwCookie);
        IF WINERROR.SUCCEEDED(hresult) THEN
          bValid:=TRUE;
        ELSE
          SYSTEM.FUNC iConnectionPoint.Release();
          iConnectionPoint:=EMPTY;
        END; (* if *)
      END; (* if *)
      SYSTEM.FUNC iCPContainer.Release();
    END; (* if *)
    RETURN bValid;
  END Connect;

  VAR dwCookie:         WIN32.DWORD;
      iConnectionPoint: OleC.IConnectionPoint;

BEGIN
  dwCookie:=0;
  iConnectionPoint:=EMPTY;
FINALLY
  IF (iConnectionPoint<>EMPTY) THEN
    SYSTEM.FUNC iConnectionPoint.Unadvise(dwCookie);
    SYSTEM.FUNC iConnectionPoint.Release();
  END; (* if *)
END C;


END ConnectionPoint.
