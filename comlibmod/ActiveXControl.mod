(*
Name:     ActiveXControl
Creation: 25-02-2000
LastEdit: 24-03-2000
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:
*)

UNSAFEGUARDED IMPLEMENTATION MODULE ActiveXControl;

IMPORT SYSTEM;
IMPORT WIN32;
IMPORT WINERROR;
IMPORT Ole2;
IMPORT OleC;
IMPORT DispInterface;


CLASS C;
  OVERRIDE PROCEDURE Init(clsid: WIN32.GUID);

  VAR hresult: WIN32.HRESULT;

  BEGIN
    DispInterface.C.Init(clsid);
    IF bValid THEN
      bValid:=FALSE;
      hresult:=iDispatch.QueryInterface(Ole2.IID_IViewObject2,iViewObject2:Ole2.IUnknown);
      IF WINERROR.SUCCEEDED(hresult) THEN
        hresult:=iDispatch.QueryInterface(OleC.IID_IOleControl,iOleControl:Ole2.IUnknown);
        IF WINERROR.SUCCEEDED(hresult) THEN
          bControlInfo:=WINERROR.SUCCEEDED(iOleControl.GetControlInfo(controlInfo));
          hresult:=iDispatch.QueryInterface(Ole2.IID_IOleObject,iOleObject:Ole2.IUnknown);
          IF WINERROR.SUCCEEDED(hresult) THEN
            bValid:=TRUE;
          END; (* if *)
        END; (* if *)
      END; (* if *)
    END; (* if *)
  END Init;

  PROCEDURE DeactivateInPlaceObject(bFull: BOOLEAN);
  BEGIN
    IF (iOleInPlaceObject<>EMPTY) THEN
      (* Inside-out objects stay in-place active but not UI active *)
      (* unless we're fully deactivating. *)
      IF (*((Ole2.OLEMISC_INSIDEOUT & m_grfMisc)
      AND *)(NOT bFull) THEN
        SYSTEM.FUNC iOleInPlaceObject.UIDeactivate();
      ELSE
        (* Only deactivate when there's no locks.  If there *)
        (* is a lock, then remember that we need to deactivate *)
        (* when all the locks go away. *)
        IF (cLockInPlace=0) THEN
          SYSTEM.FUNC iOleInPlaceObject.InPlaceDeactivate();
        ELSE
          bPendingDeactivate:=TRUE;
        END; (* if *)
      END; (* if *)
    END; (* if *)
  END DeactivateInPlaceObject;


BEGIN
  iViewObject2:=EMPTY;
  iOleObject:=EMPTY;
  iOleControl:=EMPTY;
  iOleInPlaceObject:=EMPTY;
  bControlInfo:=FALSE;
  cLockInPlace:=0;
  bPendingDeactivate:=FALSE;
FINALLY
  IF (iOleControl<>EMPTY) THEN
    (*SYSTEM.FUNC iOleControl.Release();*)
  END; (* if *)
  IF (iOleInPlaceObject<>EMPTY) THEN
    (*SYSTEM.FUNC iOleInPlaceObject.Release();*)
  END; (* if *)
  IF (iOleObject<>EMPTY) THEN
    (*SYSTEM.FUNC iOleObject.Release();*)
  END; (* if *)
  IF (iViewObject2<>EMPTY) THEN
    (*SYSTEM.FUNC iViewObject2.Release();*)
  END; (* if *)
END C;


END ActiveXControl.
