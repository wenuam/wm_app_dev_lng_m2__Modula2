IMPLEMENTATION MODULE GLU;

FROM GL IMPORT
    GLenum;

FROM WIN32 IMPORT
    %IF UNICODE %THEN
    LPCWSTR;
    %ELSE
    LPCSTR;
    %END

<*/CALLS:WINDOWSCALL*>
<*/NOHIGH*>
<*/PACK*>
<*/NOWARN:i*>

%IF UNICODE %THEN
PROCEDURE gluErrorStringWIN(errCode  : GLenum) : LPCWSTR;
BEGIN
    RETURN gluErrorUnicodeStringEXT(errCode);
END gluErrorStringWIN;
%ELSE
PROCEDURE gluErrorStringWIN(errCode  : GLenum) : LPCSTR;
BEGIN
    RETURN gluErrorString(errCode);
END gluErrorStringWIN;
%END

END GLU.
