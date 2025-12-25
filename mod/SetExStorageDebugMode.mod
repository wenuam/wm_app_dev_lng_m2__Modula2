IMPLEMENTATION MODULE SetExStorageDebugMode;

FROM ExStorage IMPORT
    SetDebugEx, GetDefaultHeap;

BEGIN
    SetDebugEx(TRUE, GetDefaultHeap());
    (*ScanForMemoryOverwritesOnApiCalls(TRUE);*)
END SetExStorageDebugMode.
