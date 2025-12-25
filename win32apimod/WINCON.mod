IMPLEMENTATION MODULE WINCON;
<*/NOWARN:I*>

FROM WIN32 IMPORT
    HANDLE, DWORD;

PROCEDURE GetLargestConsoleWindowSize(hConsoleOutput : HANDLE) : COORD [StonyBrook];
VAR
    tmp : DWORD;
    res : COORD;
BEGIN
    tmp := WINDOWSCALL_GetLargestConsoleWindowSize(hConsoleOutput);
    res.Y := tmp BAND 0ffffh;
    res.X := tmp SHR 16;
    RETURN res;
END GetLargestConsoleWindowSize;

END WINCON.
