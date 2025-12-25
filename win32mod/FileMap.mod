(***************************************************************************)
(*                                                                         *)
(*                        Copyright (C) 2009                               *)
(*                          by ADW Software                                *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE FileMap;
<*/NOWARN:F*>

FROM SYSTEM IMPORT
    ADDRESS, CAST, ADR, ADRCARD, ADRINT;

FROM FileFunc IMPORT
    FileSpecString, InvalidHandle;

%IF Windows %THEN

FROM WIN32 IMPORT
    HANDLE, DWORD,
    PAGE_READONLY, PAGE_READWRITE, FILE_MAP_READ, FILE_MAP_ALL_ACCESS,
    GENERIC_READ, GENERIC_WRITE, FILE_ATTRIBUTE_NORMAL, OPEN_EXISTING,
    SECURITY_ATTRIBUTES, SECURITY_DESCRIPTOR, SECURITY_DESCRIPTOR_REVISION,
    CreateFileMapping, OpenFileMapping,
    MapViewOfFile, UnmapViewOfFile, FlushViewOfFile, MapViewOfFileEx,
    CloseHandle, GetLastError,
    CreateFile, SetFilePointer, FILE_END,
    InitializeSecurityDescriptor, SetSecurityDescriptorDacl;

FROM WINX IMPORT
    NULL_HANDLE, NIL_STR;

%END

PROCEDURE CreateFileMap(VAR OUT mf : MappedFile;
                        fileName : ARRAY OF CHAR;
                        mapName : ARRAY OF CHAR;
                        mode : MapMode;
                        maxSize : ADRCARD) : FileMapResults;
VAR
    attr        : DWORD;
    h           : HANDLE;
    secAttr     : SECURITY_ATTRIBUTES;
    secDesc     : SECURITY_DESCRIPTOR;
    access      : CARDINAL;
    tempSpec    : FileSpecString;
    ok          : BOOLEAN;
BEGIN
    mf.status := 0;
    mf.mapHandle := CAST(ADRINT, NULL_HANDLE);
    mf.maxSize := maxSize;
    mf.mapPtr := NIL;
    mf.mapStart := 0;
    mf.mapLength := 0;
    mf.mode := mode;
    mf.fileHandle := InvalidHandle;

    IF (fileName[0] = '') AND (maxSize = 0) THEN
        mf.status := 2;
        RETURN FileMapFailed;
    END;

    (* first see if it already exists *)

    IF mapName[0] <> '' THEN
        IF mode = MapReadOnly THEN
            attr := FILE_MAP_READ;
        ELSE
            attr := FILE_MAP_ALL_ACCESS;
        END;

        h := OpenFileMapping(attr, FALSE, mapName);
        mf.mapHandle := CAST(ADRINT, h);
        IF h <> NULL_HANDLE THEN
            RETURN FileMapOpened;
        END;
    END;

    InitializeSecurityDescriptor(secDesc, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(secDesc, TRUE, NIL, FALSE);
    secAttr.nLength := SIZE(secAttr);
    secAttr.lpSecurityDescriptor := ADR(secDesc);
    secAttr.bInheritHandle := FALSE;

    (* nope, create the mapping object *)

    ok := TRUE;
    IF fileName[0] <> '' THEN
        (* get exclusive access to the file to be mapped *)
        (* this keeps the file map in memory and the file on disk coherent *)

        CASE mode OF
        MapReadOnly:
            access := GENERIC_READ;
        |
        MapReadWrite:
            access := GENERIC_READ BOR GENERIC_WRITE;
        END;

        tempSpec := fileName;
        tempSpec[HIGH(tempSpec)] := '';
        mf.fileHandle := CAST(ADRINT, CreateFile(tempSpec,
                                                 access,
                                                 0,
                                                 secAttr,
                                                 OPEN_EXISTING,
                                                 FILE_ATTRIBUTE_NORMAL,
                                                 NULL_HANDLE));
        IF mf.fileHandle = InvalidHandle THEN
            mf.status := GetLastError();
        END;
    END;
    IF mf.status = 0 THEN
        IF (maxSize = 0) AND (mf.fileHandle <> InvalidHandle) THEN
            mf.maxSize := SetFilePointer(CAST(HANDLE, mf.fileHandle),
                                         0, NIL,
                                         FILE_END);
        END;

        IF mode = MapReadOnly THEN
            attr := PAGE_READONLY;
        ELSE
            attr := PAGE_READWRITE;
        END;

        IF mapName[0] <> '' THEN
            h := CreateFileMapping(CAST(HANDLE, mf.fileHandle),
                                   secAttr,
                                   attr,
                                   0, maxSize,
                                   mapName);
        ELSE
            h := CreateFileMapping(CAST(HANDLE, mf.fileHandle),
                                   secAttr,
                                   attr,
                                   0, maxSize,
                                   NIL_STR);
        END;
        mf.mapHandle := CAST(ADRINT, h);

        IF h <> NULL_HANDLE THEN
            RETURN FileMapCreated;
        END;
        mf.status := GetLastError();

        IF mf.fileHandle <> InvalidHandle THEN
            CloseHandle(CAST(HANDLE, mf.fileHandle));
        END;
    ELSE
        mf.status := GetLastError();
    END;
    RETURN FileMapFailed;
END CreateFileMap;

PROCEDURE CloseFileMap(VAR INOUT mf : MappedFile) : BOOLEAN;
BEGIN
    IF mf.mapPtr <> NIL THEN
        IF NOT UnmapViewOfFile(mf.mapPtr) THEN
            RETURN FALSE;
        END;
    END;
    CloseHandle(CAST(HANDLE, mf.mapHandle));
    mf.status := GetLastError();

    IF mf.fileHandle <> InvalidHandle THEN
        CloseHandle(CAST(HANDLE, mf.fileHandle));
    END;

    mf.mapHandle := CAST(ADRINT, NULL_HANDLE);
    mf.fileHandle := InvalidHandle;
    RETURN TRUE;
END CloseFileMap;

PROCEDURE MapFileView(VAR INOUT mf : MappedFile; start, length : ADRCARD) : ADDRESS;
VAR
    attr        : DWORD;
BEGIN
    IF mf.mode = MapReadOnly THEN
        attr := FILE_MAP_READ;
    ELSE
        attr := FILE_MAP_ALL_ACCESS;
    END;

    IF length = 0 THEN
        length := mf.maxSize;
    END;

    mf.mapStart := start;
    mf.mapLength := length;
    mf.mapPtr := MapViewOfFile(CAST(HANDLE, mf.mapHandle), attr, 0, start, length);
    mf.status := GetLastError();
    RETURN mf.mapPtr;
END MapFileView;

PROCEDURE MapFileViewEx(VAR INOUT mf : MappedFile;
                        start, length : ADRCARD;
                        addr : ADDRESS) : BOOLEAN;
VAR
    attr        : DWORD;
BEGIN
    IF mf.mode = MapReadOnly THEN
        attr := FILE_MAP_READ;
    ELSE
        attr := FILE_MAP_ALL_ACCESS;
    END;

    IF length = 0 THEN
        length := mf.maxSize;
    END;

    mf.mapStart := start;
    mf.mapLength := length;
    mf.mapPtr := addr;
    addr := MapViewOfFileEx(CAST(HANDLE, mf.mapHandle),
                            attr,
                            0,
                            start,
                            length,
                            addr);
    mf.status := GetLastError();
    RETURN addr <> NIL;
END MapFileViewEx;

PROCEDURE UnMapFileView(VAR INOUT mf : MappedFile) : BOOLEAN;
BEGIN
    IF mf.mapPtr <> NIL THEN
        IF UnmapViewOfFile(mf.mapPtr) THEN
            mf.mapPtr := NIL;
            RETURN TRUE;
        ELSE
            mf.status := GetLastError();
        END;
    ELSE
        mf.status := 2;
    END;
    RETURN FALSE;
END UnMapFileView;

PROCEDURE FlushMappedFile(VAR INOUT mf : MappedFile) : BOOLEAN;
VAR
    ok  : BOOLEAN;
BEGIN
    ok := FlushViewOfFile(mf.mapPtr, mf.mapLength) <> FALSE;
    mf.status := GetLastError();
    RETURN ok;
END FlushMappedFile;

END FileMap.
