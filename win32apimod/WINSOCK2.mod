IMPLEMENTATION MODULE WINSOCK2;
<*/NOWARN:IF*>

FROM SYSTEM IMPORT
    CAST;

FROM WIN32 IMPORT
    BOOL, LONG, ULONG, LPARAM, DWORD;

FROM WINUSER IMPORT
    MAKELONG, LOWORD, HIWORD;

<*/CALLS:StonyBrook*>
<*/NOHIGH/NOPACK*>

PROCEDURE FD_CLR(fd : SOCKET; VAR OUT set : fd_set);
VAR
    i   : CARDINAL;
BEGIN
    i := 0;
    LOOP
        IF i < set.fd_count THEN
            IF fd <> set.fd_array[i] THEN
                INC(i);
            ELSE
                DEC(set.fd_count);
                LOOP
                    IF i < set.fd_count THEN
                        set.fd_array[i] := set.fd_array[i+1];
                        INC(i);
                    ELSE
                        EXIT;
                    END;
                END;
                EXIT;
            END;
        ELSE
            EXIT;
        END;
    END;
END FD_CLR;

PROCEDURE FD_SET(fd : SOCKET; VAR INOUT set : fd_set);
VAR
    i   : CARDINAL;
BEGIN
    i := 0;
    LOOP
        IF i < set.fd_count THEN
            IF fd <> set.fd_array[i] THEN
                INC(i);
            ELSE
                EXIT;
            END;
        ELSE
            IF i < FD_SETSIZE THEN
                set.fd_array[i] := fd;
                INC(set.fd_count);
            END;
            EXIT;
        END;
    END;
END FD_SET;

PROCEDURE timerisset(tvp : timeval) : BOOLEAN;
BEGIN
    RETURN (tvp.tv_sec <> 0) OR (tvp.tv_usec <> 0);
END timerisset;

PROCEDURE timercmpequal(tvp : timeval; uvp : timeval) : BOOL;
BEGIN
    RETURN (tvp.tv_sec = uvp.tv_sec) AND
           (tvp.tv_usec = uvp.tv_usec);
END timercmpequal;

PROCEDURE timercmpless(tvp : timeval; uvp : timeval) : BOOL;
BEGIN
    RETURN (tvp.tv_sec < uvp.tv_sec) OR
           ((tvp.tv_sec = uvp.tv_sec) AND (tvp.tv_usec < uvp.tv_usec));
END timercmpless;

PROCEDURE timercmpgreater(tvp : timeval; uvp : timeval) : BOOL;
BEGIN
    RETURN (tvp.tv_sec > uvp.tv_sec) OR
           ((tvp.tv_sec = uvp.tv_sec) AND (tvp.tv_usec > uvp.tv_usec));
END timercmpgreater;

PROCEDURE timerclear(VAR tvp : timeval);
BEGIN
    tvp.tv_sec := 0;
    tvp.tv_usec := 0;
END timerclear;

PROCEDURE IN_CLASSA(i : LONG) : BOOL;
BEGIN
    RETURN (CAST(ULONG,i) BAND 80000000h) = 0;
END IN_CLASSA;

PROCEDURE IN_CLASSB(i : LONG) : BOOL;
BEGIN
    RETURN (CAST(ULONG,i) BAND 0c0000000h) = 80000000h;
END IN_CLASSB;

PROCEDURE IN_CLASSC(i : LONG) : BOOL;
BEGIN
    RETURN (CAST(ULONG,i) BAND 0e0000000h) = 0c0000000h;
END IN_CLASSC;

PROCEDURE IN_CLASSD(i : long) : BOOL;
BEGIN
    RETURN ((CAST(CARDINAL, i) BAND 0f0000000h) = 0e0000000h);
END IN_CLASSD;

PROCEDURE IN_MULTICAST(i : long) : BOOL;
BEGIN
    RETURN IN_CLASSD(i);
END IN_MULTICAST;

PROCEDURE h_errno() : CARDINAL;
BEGIN
    RETURN WSAGetLastError();
END h_errno;

PROCEDURE WSAMAKEASYNCREPLY(buflen, error : u_short) : LONG;
BEGIN
    RETURN MAKELONG(buflen,error);
END WSAMAKEASYNCREPLY;

PROCEDURE WSAMAKESELECTREPLY(event,error : u_short) : LONG;
BEGIN
    RETURN MAKELONG(event,error);
END WSAMAKESELECTREPLY;

PROCEDURE WSAGETASYNCBUFLEN(lParam : LPARAM) : DWORD;
BEGIN
    RETURN LOWORD(lParam);
END WSAGETASYNCBUFLEN;

PROCEDURE WSAGETASYNCERROR(lParam : LPARAM) : DWORD;
BEGIN
    RETURN HIWORD(lParam);
END WSAGETASYNCERROR;

PROCEDURE WSAGETSELECTEVENT(lParam : LPARAM) : DWORD;
BEGIN
    RETURN LOWORD(lParam);
END WSAGETSELECTEVENT;

PROCEDURE WSAGETSELECTERROR(lParam : LPARAM) : DWORD;
BEGIN
    RETURN HIWORD(lParam);
END WSAGETSELECTERROR;

END WINSOCK2.
