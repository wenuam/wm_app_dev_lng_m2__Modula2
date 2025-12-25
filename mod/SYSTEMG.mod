(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)

IMPLEMENTATION MODULE SYSTEMG;
<*/NOOPT:X*>

FROM SYSTEM IMPORT
    UNREFERENCED_PARAMETER;

PROCEDURE IsCollectionEnabled ["GARBAGECOLLECTION_IsCollectionEnabled"]
                    () : BOOLEAN;
BEGIN
    RETURN FALSE;
END IsCollectionEnabled;

PROCEDURE SetCollectionEnable ["GARBAGECOLLECTION_SetCollectionEnable"]
                    (on : BOOLEAN);
BEGIN
    UNREFERENCED_PARAMETER(on);
END SetCollectionEnable;

PROCEDURE ForceCollection ["GARBAGECOLLECTION_ForceCollection"];
BEGIN
END ForceCollection;

END SYSTEMG.
