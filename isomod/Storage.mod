(***************************************************************************)
(*                                                                         *)
(*                         Copyright (C) 2009                              *)
(*                           by ADW Software                               *)
(*                                                                         *)
(*                        All rights reserved.                             *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE Storage;

FROM EXCEPTIONS IMPORT
    ExceptionSource, AllocateSource, RaiseRTL,
    IsCurrentSource, CurrentNumber, GetMessage;

IMPORT SYSTEM;

IMPORT ExStorage;

VAR
    StorageSrc  : ExceptionSource;

    Heap        : ExStorage.HeapInfoPointer;

PROCEDURE HandleExStorageException;
VAR
    mess        : ARRAY [0..63] OF CHAR;
BEGIN
    IF ExStorage.IsStorageException() THEN
        GetMessage(mess);
        CASE ExStorage.StorageException() OF
        ExStorage.nilDeallocation:
            RaiseRTL(StorageSrc, ORD(nilDeallocation), mess);
        |
        ExStorage.pointerToUnallocatedStorage:
            RaiseRTL(StorageSrc, ORD(pointerToUnallocatedStorage), mess);
        |
        ExStorage.wrongStorageToUnallocate:
            RaiseRTL(StorageSrc, ORD(wrongStorageToUnallocate), mess);
        ELSE
        END;
    END;
END HandleExStorageException;

PROCEDURE ALLOCATE(VAR addr : SYSTEM.ADDRESS; amount : SYSTEM.ADRCARD);
BEGIN
    ExStorage.AllocateEx(addr, amount, Heap);
EXCEPT
    HandleExStorageException;
END ALLOCATE;

PROCEDURE DEALLOCATE(VAR addr : SYSTEM.ADDRESS; amount : SYSTEM.ADRCARD);
BEGIN
    ExStorage.DeallocateEx(addr, amount, Heap);
EXCEPT
    HandleExStorageException;
END DEALLOCATE;

PROCEDURE IsStorageException() : BOOLEAN;
BEGIN
    RETURN IsCurrentSource(StorageSrc);
END IsStorageException;

PROCEDURE StorageException() : StorageExceptions;
BEGIN
    RETURN VAL(StorageExceptions, CurrentNumber(StorageSrc));
END StorageException;

BEGIN
    IF NOT SYSTEM.IsThread THEN
        AllocateSource(StorageSrc);
        Heap := ExStorage.GetDefaultHeap();
    END;
END Storage.
