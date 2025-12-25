IMPLEMENTATION MODULE RichEdit;
<*/NOWARN:I*>

IMPORT SYSTEM, WIN32, WINX;

<*/VALIDVER:RichEdit20*>
<*/VER:RichEdit20*>(*also change directive in DEF module*)

VAR
    hMod        : WIN32.HANDLE;
BEGIN
    IF NOT SYSTEM.IsThread THEN
        %IF RichEdit20 %THEN
            hMod := WIN32.LoadLibrary("Riched20");
        %ELSE
            hMod := WIN32.LoadLibrary("Riched32");
        %END
    END;
FINALLY
    IF NOT SYSTEM.IsThread THEN
        IF hMod <> WINX.NULL_HANDLE THEN
            SYSTEM.FUNC WIN32.FreeLibrary(hMod);
        END;
    END;
END RichEdit.
