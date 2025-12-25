(*
Name:     Socket
Creation: 21-09-2000
LastEdit: 04-10-2000
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:
*)

IMPLEMENTATION MODULE Socket;

IMPORT SYSTEM;
IMPORT WIN32;
IMPORT WINSOCK2;
%IF UNICODE %THEN
IMPORT WINNLS, WINX, ExStrings;
%ELSE
IMPORT Strings;
%END

CONST cVERSION= WINSOCK2.WINSOCK_VERSION; (* Windows Sockets version *)

VAR bStarted: BOOLEAN;


(* creates socket *)
PROCEDURE Create(    nDomain:   INTEGER;
                     nType:     INTEGER;
                     nProtocol: INTEGER;
                 VAR OUT socket:    Socket): BOOLEAN;
BEGIN
  IF bStarted THEN
    socket:=WINSOCK2.socket(nDomain,nType,nProtocol);
  ELSE
    socket:=WINSOCK2.INVALID_SOCKET;
  END; (* if *)
  RETURN (socket<>WINSOCK2.INVALID_SOCKET);
END Create;

(* closes of socket *)
PROCEDURE Close(VAR INOUT socket: Socket);
BEGIN
  IF (socket<>WINSOCK2.INVALID_SOCKET) THEN
    SYSTEM.FUNC WINSOCK2.closesocket(socket);
  END; (* if *)
END Close;

(* binds socket to the local address *)
PROCEDURE Bind(socket:  Socket;
               nFamily: CARDINAL16;
               cPort:   CARDINAL16;
               ip:      IP): BOOLEAN;

VAR sockaddr: WINSOCK2.SOCKADDR;

BEGIN
  sockaddr:WINSOCK2.sockaddr_in.sin_family:=nFamily;
  sockaddr:WINSOCK2.sockaddr_in.sin_addr:=ip;
  sockaddr:WINSOCK2.sockaddr_in.sin_port:=WINSOCK2.htons(cPort);
  sockaddr:WINSOCK2.sockaddr_in.sin_zero[0]:=0C;
  RETURN (WINSOCK2.bind(socket,sockaddr,SIZE(sockaddr))=0);
END Bind;

(* connects socket to host *)
PROCEDURE Connect(socket:  Socket;
                  nFamily: CARDINAL16;
                  cPort:   CARDINAL16;
                  ip:      IP): BOOLEAN;

VAR server: WINSOCK2.sockaddr;

BEGIN
  server:WINSOCK2.sockaddr_in.sin_family:=nFamily;
  server:WINSOCK2.sockaddr_in.sin_port:=WINSOCK2.htons(cPort);
  server:WINSOCK2.sockaddr_in.sin_addr:=ip;
  server:WINSOCK2.sockaddr_in.sin_zero[0]:=0C;
  RETURN (WINSOCK2.connect(socket,server,SIZE(server))<>WINSOCK2.SOCKET_ERROR);
END Connect;

(* listens on port *)
PROCEDURE Listen(socket: Socket): BOOLEAN;
BEGIN
  RETURN (WINSOCK2.listen(socket,WINSOCK2.SOMAXCONN)=0);
END Listen;


(* accepts incoming connection of sListen *)
PROCEDURE Accept(    sListen:  Socket;
                 VAR OUT sConnect: Socket): BOOLEAN;

VAR sockaddr: WINSOCK2.sockaddr;
    nSize:    INTEGER;

BEGIN
  nSize:=SIZE(sockaddr);
  sConnect:=WINSOCK2.accept(sListen,sockaddr,nSize);
  RETURN (sConnect<>WINSOCK2.INVALID_SOCKET);
END Accept;

(* same as Accept *)
(* returns ip and port of sConnect *)
PROCEDURE Accept2(         sListen: Socket;
                  VAR OUT sConnect: Socket;
                  VAR OUT ip:       IP;
                  VAR OUT cPort:    CARDINAL16): BOOLEAN;

VAR sockaddr: WINSOCK2.sockaddr;
    cSize:    INTEGER;

BEGIN
  cSize:=SIZE(sockaddr);
  sConnect:=WINSOCK2.accept(sListen,sockaddr,cSize);
  IF (sConnect<>WINSOCK2.INVALID_SOCKET) THEN
    cPort:=WINSOCK2.ntohs(sockaddr:WINSOCK2.sockaddr_in.sin_port);
    ip:=sockaddr:WINSOCK2.sockaddr_in.sin_addr;
  END; (* if *)
  RETURN (sConnect<>WINSOCK2.INVALID_SOCKET);
END Accept2;

CONST
    Win32SockOpt : ARRAY SocketOptions OF CARDINAL =
    {
    WINSOCK2.SO_DEBUG,
    WINSOCK2.SO_BROADCAST,
    WINSOCK2.SO_REUSEADDR,
    WINSOCK2.SO_SNDBUF,
    WINSOCK2.SO_RCVBUF,
    WINSOCK2.SO_SNDTIMEO,
    WINSOCK2.SO_RCVTIMEO,
    WINSOCK2.IP_ADD_MEMBERSHIP,
    WINSOCK2.IP_DROP_MEMBERSHIP
    };

    Win32LevelOpt : ARRAY SocketOptions OF CARDINAL =
    {
    WINSOCK2.SOL_SOCKET,
    WINSOCK2.SOL_SOCKET,
    WINSOCK2.SOL_SOCKET,
    WINSOCK2.SOL_SOCKET,
    WINSOCK2.SOL_SOCKET,
    WINSOCK2.SOL_SOCKET,
    WINSOCK2.SOL_SOCKET,
    WINSOCK2.IPPROTO_IP,
    WINSOCK2.IPPROTO_IP
    };

PROCEDURE SetSocketOptions(socket : Socket; opt : SocketOptionRecord) : BOOLEAN;
VAR
    res         : INTEGER;
BEGIN
    CASE opt.opt OF
    SoSendBufSize, SoReceiveBufSize:
        res := WINSOCK2.setsockopt(socket,
                                   Win32LevelOpt[opt.opt],
                                   Win32SockOpt[opt.opt],
                                   opt.bufSize, SIZE(opt.bufSize));
    |
    SoReuseAddr, SoBroadcast, SoDebug:
        res := WINSOCK2.setsockopt(socket,
                                   Win32LevelOpt[opt.opt],
                                   Win32SockOpt[opt.opt],
                                   opt.boolean, SIZE(opt.boolean));
    |
    SoSendTimeout, SoReceiveTimeout:
        res := WINSOCK2.setsockopt(socket,
                                   Win32LevelOpt[opt.opt],
                                   Win32SockOpt[opt.opt],
                                   opt.timeout, SIZE(opt.timeout));
    |
    So_IP_AddMembership, So_IP_DropMembership:
        res := WINSOCK2.setsockopt(socket,
                                   Win32LevelOpt[opt.opt],
                                   Win32SockOpt[opt.opt],
                                   opt.groupIP, SIZE(opt.groupIP)*2);
    END;

    RETURN res = 0;
END SetSocketOptions;

(* sends text over socket *)
(* returns number of bytes sent *)
PROCEDURE Send(socket: Socket;
               btData: ARRAY OF SYSTEM.LOC;
               cSize:  CARDINAL): INTEGER;
BEGIN
  RETURN WINSOCK2.send(socket,SYSTEM.ADR(btData),cSize,0);
END Send;

(* sends text over socket *)
PROCEDURE SendText(socket: Socket;
                   szText: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN (Send(socket,szText,LENGTH(szText)*SIZE(CHAR))>0);
END SendText;

PROCEDURE SendTo(socket: Socket;
                 btData: ARRAY OF SYSTEM.LOC;
                 cSize:  CARDINAL;
                 nFamily: CARDINAL16;
                 cPort:   CARDINAL16;
                 ip:      IP): INTEGER;

VAR
    server      : WINSOCK2.sockaddr;
BEGIN
    server:WINSOCK2.sockaddr_in.sin_family:=nFamily;
    server:WINSOCK2.sockaddr_in.sin_port:=WINSOCK2.htons(cPort);
    server:WINSOCK2.sockaddr_in.sin_addr:=ip;
    server:WINSOCK2.sockaddr_in.sin_zero[0]:=0C;
    RETURN WINSOCK2.sendto(socket,SYSTEM.ADR(btData),cSize,0,server,SIZE(server));
END SendTo;

(* receives text over socket *)
(* returns number of bytes received *)
PROCEDURE Receive(    socket: Socket;
                  VAR OUT btData: ARRAY OF SYSTEM.LOC;
                       cSize: CARDINAL): INTEGER;
BEGIN
  RETURN WINSOCK2.recv(socket,SYSTEM.ADR(btData),cSize,0);
END Receive;

(* receives text over socket *)
PROCEDURE ReceiveText(    socket: Socket;
                      VAR OUT szText: ARRAY OF CHAR): BOOLEAN;

VAR nEnd: INTEGER;

BEGIN
  nEnd:=Receive(socket,szText,SIZE(szText));
  IF (nEnd>=0) THEN
    nEnd := nEnd / SIZE(CHAR);
    IF ORD(nEnd) <= HIGH(szText) THEN
        szText[nEnd]:=0C;
    END;
  ELSE
    szText:="";
  END; (* if *)
  RETURN (nEnd>0);
END ReceiveText;

PROCEDURE ReceiveFrom(socket:  Socket;
                      VAR OUT btData: ARRAY OF SYSTEM.LOC;
                      cSize: CARDINAL;
                      VAR OUT port: CARDINAL16;
                      VAR OUT ip: IP): INTEGER;

VAR
  n : INTEGER;
  from: WINSOCK2.SOCKADDR;
  nSize:INTEGER;
BEGIN
    nSize := SIZE(from);
    n := WINSOCK2.recvfrom(socket,SYSTEM.ADR(btData),cSize,0,from,nSize);
    IF n > 0 THEN
        ip := from:WINSOCK2.sockaddr_in.sin_addr;
        port := WINSOCK2.ntohs(from:WINSOCK2.sockaddr_in.sin_port);
    ELSE
        port := 0;
        ip   := 0;
    END;
    RETURN n;
END ReceiveFrom;

(* Shut down all or part of the connection open on socket FD. *)
PROCEDURE Shutdown(socket: Socket;
                   nHow:   INTEGER): BOOLEAN;
BEGIN
  RETURN (WINSOCK2.shutdown(socket,nHow)=0);
END Shutdown;


(* starts up Windows sockets library *)
PROCEDURE StartUp;

VAR wsa: WINSOCK2.WSADATA;

BEGIN
  wsa.wVersion:=0; (* prevents warning *)
  bStarted:=(WINSOCK2.WSAStartup(cVERSION,SYSTEM.ADR(wsa))=0);
END StartUp;

(* GetHostAddr -- gets host's internet address. Interprets argument first *)
(* as dotted internet address string, and failing that, as a DNS host name. *)
PROCEDURE GetHostAddr(    szHost: ARRAY OF CHAR;
                      VAR OUT ip:     IP): BOOLEAN;

VAR phe:    WINSOCK2.LPHOSTENT;
    bValid: BOOLEAN;
%IF UNICODE %THEN
    szaHost: ARRAY [0..256] OF ACHAR;
%END

BEGIN
  bValid:=FALSE;
  %IF UNICODE %THEN
    SYSTEM.FUNC WINNLS.WideCharToMultiByte(WINNLS.CP_ACP,
                                           WINNLS.WC_COMPOSITECHECK,
                                           szHost, -1,
                                           szaHost,HIGH(szaHost),
                                           WINX.NIL_ASTR,
                                           WINX.NIL_BOOL);
    ip:=SYSTEM.CAST(IP,WINSOCK2.inet_addr(szaHost));
  %ELSE
    ip:=SYSTEM.CAST(IP,WINSOCK2.inet_addr(szHost));
  %END
  IF (ip:CARDINAL<>WINSOCK2.INADDR_NONE) THEN
    bValid:=TRUE;
  ELSE
    %IF UNICODE %THEN
      phe:=WINSOCK2.gethostbyname(szaHost);
    %ELSE
      phe:=WINSOCK2.gethostbyname(szHost);
    %END
    IF (phe<>NIL) THEN
      <*/PUSH/NOWARN:C*>
      (* this does not look very nice, but it seems to work *)
      ip:=SYSTEM.CAST(CARDINAL32,phe^.h_addr_list^[0]^);
      <*/POP*>
      bValid:=(ip:CARDINAL<>WINSOCK2.INADDR_NONE);
    END; (* if *)
  END; (* if *)
  RETURN bValid;
END GetHostAddr;

PROCEDURE GetHostName(VAR OUT hostName : ARRAY OF CHAR) : BOOLEAN;
%IF UNICODE %THEN
VAR
    strA        : ARRAY [0..255] OF ACHAR;
%END
BEGIN
    %IF UNICODE %THEN
        IF WINSOCK2.gethostname(strA, HIGH(strA)+1) = 0 THEN
            ExStrings.AnsiToUnicode(strA, hostName);
            RETURN TRUE;
        END;
        RETURN FALSE;
    %ELSE
        RETURN WINSOCK2.gethostname(hostName, HIGH(hostName)+1) = 0;
    %END
END GetHostName;

(* returns port for service *)
PROCEDURE GetServicePort(    szService: ARRAY OF ACHAR;
                         VAR cPort:     CARDINAL): BOOLEAN;

VAR lpServent: WINSOCK2.LPSERVENT;
    bValid:    BOOLEAN;

BEGIN
  bValid:=FALSE;
  lpServent:=WINSOCK2.getservbyname(szService,'tcp');
  IF (lpServent<>NIL) THEN
    cPort:=NetToHost16(SYSTEM.CAST(CARDINAL16,lpServent^.s_port));
    bValid:=TRUE;
  END; (* if *)
  RETURN bValid;
END GetServicePort;

(* converts IP number to text *)
PROCEDURE IPtoStr(    ip:     IP;
                  VAR OUT szText: ARRAY OF CHAR);

VAR pstr: WIN32.LPSTR;

BEGIN
  pstr:=WINSOCK2.inet_ntoa(ip);
  IF (pstr<>NIL) THEN
    %IF UNICODE %THEN
      SYSTEM.FUNC WINNLS.MultiByteToWideChar(WINNLS.CP_ACP,
                                             WINNLS.MB_PRECOMPOSED,
                                             pstr^, -1,
                                             szText, HIGH(szText));
    %ELSE
        Strings.Assign(pstr^, szText);
    %END
  ELSE
    szText:="";
  END; (* if *)
END IPtoStr;

PROCEDURE FD_ZERO(VAR OUT set: fd_set);
BEGIN
    WINSOCK2.FD_ZERO(set);
END FD_ZERO;

PROCEDURE FD_ISZERO(set: fd_set) : BOOLEAN;
BEGIN
    RETURN set.fd_count = 0;
END FD_ISZERO;

PROCEDURE Select(VAR INOUT readfds, writefds, exceptfds : fd_set;
                 timeout : INTEGER) : INTEGER;
VAR
    tv          : WINSOCK2.timeval;
    utimeout    : POINTER TO WINSOCK2.timeval;
    read,
    write,
    except      : POINTER TO fd_set;
BEGIN
    IF timeout < 0 THEN
        utimeout := NIL;
    ELSE
        tv.tv_sec := timeout / 1000;
        tv.tv_usec := (timeout REM 1000) * 1000;
        utimeout := SYSTEM.ADR(tv);
    END;

    read := NIL;
    write := NIL;
    except := NIL;
    IF readfds.fd_count <> 0 THEN
        read := SYSTEM.ADR(readfds);
    END;
    IF writefds.fd_count <> 0 THEN
        write := SYSTEM.ADR(writefds);
    END;
    IF exceptfds.fd_count <> 0 THEN
        except := SYSTEM.ADR(exceptfds);
    END;

    RETURN WINSOCK2.select(FD_SETSIZE,(* this param ignored by Win32*)
                           read^, write^, except^, utimeout^);
END Select;

PROCEDURE Select2(readfds_in, writefds_in, exceptfds_in : fd_set;
                  VAR OUT readfds_out, writefds_out, exceptfds_out : fd_set;
                  timeout : INTEGER): INTEGER;
BEGIN
    readfds_out := readfds_in;
    writefds_out := writefds_in;
    exceptfds_out := exceptfds_in;
    RETURN Select(readfds_out, writefds_out, exceptfds_out, timeout);
END Select2;

(*
static gint
g_poll (GPollFD *fds,
        guint    nfds,
        gint     timeout)
{
  struct timeval tv;
  SELECT_MASK rset, wset, xset;
  GPollFD *f;
  int ready;
  int maxfd = 0;

  FD_ZERO (&rset);
  FD_ZERO (&wset);
  FD_ZERO (&xset);

  for (f = fds; f < &fds[nfds]; ++f)
    if (f->fd >= 0)
      {
        if (f->events & G_IO_IN)
          FD_SET (f->fd, &rset);
        if (f->events & G_IO_OUT)
          FD_SET (f->fd, &wset);
        if (f->events & G_IO_PRI)
          FD_SET (f->fd, &xset);
        if (f->fd > maxfd && (f->events & (G_IO_IN|G_IO_OUT|G_IO_PRI)))
          maxfd = f->fd;
      }

  tv.tv_sec = timeout / 1000;
  tv.tv_usec = (timeout % 1000) * 1000;

  ready = select (maxfd + 1, &rset, &wset, &xset,
                  timeout == -1 ? NULL : &tv);
  if (ready > 0)
    for (f = fds; f < &fds[nfds]; ++f)
      {
        f->revents = 0;
        if (f->fd >= 0)
          {
            if (FD_ISSET (f->fd, &rset))
              f->revents |= G_IO_IN;
            if (FD_ISSET (f->fd, &wset))
              f->revents |= G_IO_OUT;
            if (FD_ISSET (f->fd, &xset))
              f->revents |= G_IO_PRI;
          }
      }

  return ready;
}
*)

(* creates a TCP socket that listens on cPort *)
PROCEDURE TCPlisten(    cPort:   CARDINAL;
                    VAR OUT sListen: Socket): BOOLEAN;

VAR bValid: BOOLEAN;

BEGIN
  bValid:=FALSE;
  IF Create(AF_INET,SOCK_STREAM,0,sListen) THEN
    IF  Bind(sListen,AF_INET,cPort,ipANY)
    AND Listen(sListen) THEN
      bValid:=TRUE;
    END; (* if *)
  END; (* if *)
  RETURN bValid;
END TCPlisten;


(* creates a TCP socket connected to szHost *)
PROCEDURE TCPconnect(    szHost:   ARRAY OF CHAR;
                         cPort:    CARDINAL;
                     VAR OUT sConnect: Socket): BOOLEAN;

VAR bValid: BOOLEAN;
    ip:     IP;

BEGIN
  bValid:=FALSE;
  IF GetHostAddr(szHost,ip) THEN
    IF  Create(AF_INET,SOCK_STREAM,0,sConnect)
    AND Connect(sConnect,AF_INET,cPort,ip) THEN
      bValid:=TRUE;
    END; (* if *)
  END; (* if *)
  RETURN bValid;
END TCPconnect;



BEGIN
    IF NOT SYSTEM.IsThread THEN
        StartUp;
    END;
FINALLY
    IF NOT SYSTEM.IsThread THEN
        IF bStarted THEN
            SYSTEM.FUNC WINSOCK2.WSACleanup();
        END; (* if *)
    END;
END Socket.
