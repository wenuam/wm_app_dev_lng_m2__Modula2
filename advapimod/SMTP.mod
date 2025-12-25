(*
Name:     SMTP
Creation: 18-11-2003
LastEdit: 06-02-2004
Author:   Egbert J. van der Haring
System:   StonyBrook Modula-2
Remarks:  see RFC 821, SIMPLE MAIL TRANSFER PROTOCOL
*)

IMPLEMENTATION MODULE SMTP;

FROM Conversions IMPORT
    CardToStr;
FROM Strings IMPORT
    Append;

IMPORT ASCII;
IMPORT Socket;
IMPORT ExStorage;
IMPORT Strings;
IMPORT SysClock;

TYPE T= POINTER TO CONNECTION;
     CONNECTION= RECORD
                   socket:  Socket.Socket;
                   szReply: ARRAY [0..3] OF CHAR;
                   bValid:  BOOLEAN;
                   bData:   BOOLEAN;
                 END; (* record *)
     szMONTH= ARRAY [0..3] OF CHAR;

VAR szCRLF:  ARRAY [0..2] OF CHAR;
    szMonth: ARRAY [1..12] OF szMONTH;

(* sends mail via host *)
PROCEDURE SendShortMessage(szHost:    ARRAY OF CHAR;
                           szFrom:    ARRAY OF CHAR;
                           szTo:      ARRAY OF CHAR;
                           szCC:      ARRAY OF CHAR;
                           szBCC:     ARRAY OF CHAR;
                           szSubject: ARRAY OF CHAR;
                           szMessage: ARRAY OF CHAR): BOOLEAN;

VAR bValid: BOOLEAN;
    smtp:   T;

BEGIN
  bValid:=FALSE;
  IF SendMailHeader(szHost,szFrom,szTo,szCC,szBCC,szSubject,smtp) THEN
    IF SendMailText(smtp,szMessage) THEN
      bValid:=Send(smtp);
    END; (* if *)
    DisConnect(smtp);
  END; (* if *)
  RETURN bValid;
END SendShortMessage;


(* sends mail header to SMTP host *)
PROCEDURE SendMailHeader(    szHost:    ARRAY OF CHAR;
                             szFrom:    ARRAY OF CHAR;
                             szTo:      ARRAY OF CHAR;
                             szCC:      ARRAY OF CHAR;
                             szBCC:     ARRAY OF CHAR;
                             szSubject: ARRAY OF CHAR;
                         VAR smtp:      T): BOOLEAN;

VAR clock:  SysClock.DateTime;
    szDate: ARRAY [0..40] OF CHAR;
    bValid: BOOLEAN;

BEGIN
  bValid:=FALSE;
  IF Connect(szHost,smtp) THEN
    SysClock.GetClock(clock);
    DateTimeForSMTP(clock,szDate);
    IF  SendFrom(smtp,szFrom)
    AND MultipleTo(smtp,szTo)
    AND MultipleTo(smtp,szCC)
    AND MultipleTo(smtp,szBCC)
    AND SendField(smtp,'Date: ',szDate)
    AND SendField(smtp,'From: ',szFrom)
    AND SendField(smtp,'To: ',szTo)
    AND SendField(smtp,'cc: ',szCC)
    AND SendField(smtp,'Bcc:',szBCC)
    AND SendField(smtp,'Subject: ',szSubject)
    AND AddEmptyLine(smtp) THEN (* empty line between header and body *)
      bValid:=TRUE;
    ELSE
      DisConnect(smtp);
    END; (* if *)
  END; (* if *)
  RETURN bValid;
END SendMailHeader;


(* sends mail and closes connection to SMTP host *)
PROCEDURE SendAndClose(VAR INOUT smtp: T): BOOLEAN;

VAR bValid: BOOLEAN;

BEGIN
  bValid:=Send(smtp);
  DisConnect(smtp);
  RETURN bValid;
END SendAndClose;


(* returns date in format according to RFC 822, something like: *)
(* [Sun,] 23 Nov 2003 14:15:58 CET *)
(* NB: this should probably go to FormatDT *)
(*     it would be nice if it would be extended with the time zone *)
PROCEDURE DateTimeForSMTP(    dt:      SysClock.DateTime;
                          VAR timeStr: ARRAY OF CHAR);

    PROCEDURE putNum(num : CARDINAL;
                     VAR str : ARRAY OF CHAR;
                     leadingZero : BOOLEAN);
    VAR
        numStr  : ARRAY [0..9] OF CHAR;
        ok      : BOOLEAN;
    BEGIN
        ok := CardToStr(num, numStr);
        IF (num < 10) AND leadingZero THEN
            Append('0', str);
        END;
        Append(numStr, str);
    END putNum;

BEGIN
    timeStr := "";

    putNum(dt.day, timeStr, FALSE);
    Append(' ', timeStr);
    Append(szMonth[dt.month],timeStr);
    Append(' ', timeStr);
    putNum(dt.year, timeStr, TRUE);
    Append(' ', timeStr);

    putNum(dt.hour, timeStr, TRUE);
    Append(':', timeStr);
    putNum(dt.minute, timeStr, TRUE);
    Append(':', timeStr);
    putNum(dt.second, timeStr, TRUE);
END DateTimeForSMTP;


(* calls toproc for each address *)
PROCEDURE MultipleTo(smtp: T;
                     szTo: ARRAY OF CHAR): BOOLEAN;

VAR szAddress: ARRAY [0..400] OF CHAR;
    cStart:    CARDINAL;
    cEnd:      CARDINAL;
    bFound:    BOOLEAN;
    bValid:    BOOLEAN;

BEGIN
  cStart:=0;
  bValid:=TRUE;
  REPEAT
    Strings.FindNext('<',szTo,cStart,bFound,cStart);
    IF bFound THEN
      Strings.FindNext('>',szTo,cStart,bFound,cEnd);
      IF bFound THEN
        INC(cStart);
        Strings.Extract(szTo,cStart,cEnd-cStart,szAddress);
        bValid:=To(smtp,szAddress);
        cStart:=cEnd+1;
      END; (* if *)
    END; (* if *)
  UNTIL NOT (bValid AND bFound);
  RETURN bValid;
END MultipleTo;


(* sends from *)
PROCEDURE SendFrom(smtp:   T;
                   szFrom: ARRAY OF CHAR): BOOLEAN;

VAR szAddress: ARRAY [0..400] OF CHAR;
    cStart:    CARDINAL;
    cEnd:      CARDINAL;
    bFound:    BOOLEAN;
    bValid:    BOOLEAN;

BEGIN
  cStart:=0;
  bValid:=FALSE;
  Strings.FindNext('<',szFrom,cStart,bFound,cStart);
  IF bFound THEN
    Strings.FindNext('>',szFrom,cStart,bFound,cEnd);
    IF bFound THEN
      INC(cStart);
      Strings.Extract(szFrom,cStart,cEnd-cStart,szAddress);
      bValid:=From(smtp,szAddress);
    END; (* if *)
  END; (* if *)
  RETURN bValid;
END SendFrom;


(* sends field: value as data *)
PROCEDURE SendField(smtp:    T;
                    szField: ARRAY OF CHAR;
                    szValue: ARRAY OF CHAR): BOOLEAN;

VAR bValid: BOOLEAN;

BEGIN
  bValid:=FALSE;
  IF (szValue[0]=0C) THEN
    bValid:=TRUE;
  ELSIF (smtp^.socket<>0) THEN
    IF NOT smtp^.bData THEN
      smtp^.bData:=TRUE;
      IF SendMailText(smtp,'DATA') THEN
        IF Receive(smtp) THEN
          IF (Strings.Compare(smtp^.szReply,'354')=Strings.equal) THEN
            IF Socket.SendText(smtp^.socket,szField) THEN
              bValid:=SendMailText(smtp,szValue);
            END; (* if *)
          END; (* if *)
        END; (* if *)
      END; (* if *)
    ELSIF Socket.SendText(smtp^.socket,szField) THEN
      bValid:=SendMailText(smtp,szValue);
    END; (* if *)
  END; (* if *)
  RETURN bValid;
END SendField;


(* returns TRUE when connected *)
PROCEDURE Connect(    szHost: ARRAY OF CHAR;
                  VAR smtp:   T): BOOLEAN;

VAR ipHost: Socket.IP;
    szText: ARRAY [0..256] OF CHAR;
    bValid: BOOLEAN;

BEGIN
  bValid:=FALSE;
  ExStorage.ALLOCATE(smtp,SIZE(CONNECTION));
  IF Socket.GetHostAddr(szHost,ipHost) THEN
    IF Socket.Create(Socket.AF_INET,Socket.SOCK_STREAM,0,smtp^.socket) THEN
      IF Socket.Connect(smtp^.socket,Socket.AF_INET,25,ipHost) THEN
        IF Receive(smtp) THEN
          IF (Strings.Compare(smtp^.szReply,'220')=Strings.equal) THEN
            IF Socket.GetHostName(szText) THEN
              Strings.Insert('HELO ',0,szText);
              IF SendMailText(smtp,szText) THEN
                IF Receive(smtp) THEN
                  IF (Strings.Compare(smtp^.szReply,'250')=Strings.equal) THEN
                    smtp^.bValid:=TRUE;
                    smtp^.bData:=FALSE;
                    bValid:=TRUE;
                  END; (* if *)
                END; (* if *)
              END; (* if *)
            END; (* if *)
          END; (* if *)
        END; (* if *)
        IF NOT bValid THEN
          Socket.Close(smtp^.socket);
        END; (* if *)
      END; (* if *)
    END; (* if *)
  END; (* if *)
  IF NOT bValid THEN
    ExStorage.DEALLOCATE(smtp,SIZE(CONNECTION));
  END; (* if *)
  RETURN bValid;
END Connect;


(* disconnects from host *)
PROCEDURE DisConnect(VAR INOUT smtp: T);
BEGIN
  IF (smtp<>NIL) THEN
    IF  smtp^.bValid  (* only send QUIT when connection is still valid *)
    AND SendMailText(smtp,'QUIT') THEN
      IF Receive(smtp) THEN
      END; (* if *)
    END; (* if *)
    Socket.Close(smtp^.socket);
    ExStorage.DEALLOCATE(smtp,SIZE(CONNECTION));
  END; (* if *)
END DisConnect;


(* starts mail from ... *)
PROCEDURE From(smtp:   T;
               szFrom: ARRAY OF CHAR): BOOLEAN;

VAR szText: ARRAY [0..512] OF CHAR;
    bValid: BOOLEAN;

BEGIN
  bValid:=FALSE;
  IF (smtp^.socket<>0) THEN
    Strings.Concat('MAIL FROM:',szFrom,szText);
    IF SendMailText(smtp,szText) THEN
      IF Receive(smtp) THEN
        IF (Strings.Compare(smtp^.szReply,'250')=Strings.equal) THEN
          bValid:=TRUE;
        END; (* if *)
      END; (* if *)
    END; (* if *)
    smtp^.bValid:=bValid;
  END; (* if *)
  RETURN bValid;
END From;


(* sets recipient, may be called multiple times *)
PROCEDURE To(smtp: T;
             szTo: ARRAY OF CHAR): BOOLEAN;

VAR szText: ARRAY [0..512] OF CHAR;
    bValid: BOOLEAN;

BEGIN
  bValid:=FALSE;
  IF (smtp^.socket<>0) THEN
    Strings.Concat('RCPT TO:',szTo,szText);
    IF SendMailText(smtp,szText) THEN
      IF Receive(smtp) THEN
        IF (Strings.Compare(smtp^.szReply,'250')=Strings.equal)
        OR (Strings.Compare(smtp^.szReply,'251')=Strings.equal) THEN
          bValid:=TRUE;
        END; (* if *)
      END; (* if *)
    END; (* if *)
    smtp^.bValid:=bValid;
  END; (* if *)
  RETURN bValid;
END To;


(* sets text of message, may be called multiple times *)
PROCEDURE Data(smtp:   T;
               szText: ARRAY OF CHAR): BOOLEAN;

VAR bValid: BOOLEAN;

BEGIN
  bValid:=FALSE;
  IF (smtp^.socket<>0) THEN
    IF NOT smtp^.bData THEN
      smtp^.bData:=TRUE;
      IF SendMailText(smtp,'DATA') THEN
        IF Receive(smtp) THEN
          IF (Strings.Compare(smtp^.szReply,'354')=Strings.equal) THEN
            bValid:=SendMailText(smtp,szText);
          END; (* if *)
        END; (* if *)
      END; (* if *)
    ELSE
      bValid:=SendMailText(smtp,szText);
    END; (* if *)
    smtp^.bValid:=bValid;
  END; (* if *)
  RETURN bValid;
END Data;


(* adds empty line to message *)
PROCEDURE AddEmptyLine(smtp: T): BOOLEAN;
BEGIN
  RETURN (Socket.Send(smtp^.socket,szCRLF,2)=2);
END AddEmptyLine;


(* sends message *)
PROCEDURE Send(smtp: T): BOOLEAN;

VAR bValid: BOOLEAN;

BEGIN
  bValid:=FALSE;
  IF (smtp^.socket<>0) THEN
    smtp^.bData:=FALSE;
    IF SendMailText(smtp,'.') THEN
      IF Receive(smtp) THEN
        IF (Strings.Compare(smtp^.szReply,'250')=Strings.equal) THEN
          bValid:=TRUE;
        END; (* if *)
      END; (* if *)
    END; (* if *)
    smtp^.bValid:=bValid;
  END; (* if *)
  RETURN bValid;
END Send;


(* sends mail text line *)
PROCEDURE SendMailText(smtp:   T;
                       szLine: ARRAY OF CHAR): BOOLEAN;

VAR bValid: BOOLEAN;

BEGIN
  IF (szLine[0]<>0C) THEN
    bValid:=Socket.SendText(smtp^.socket,szLine) AND AddEmptyLine(smtp);
  ELSE
    bValid:=AddEmptyLine(smtp);
  END; (* if *)
  RETURN bValid;
END SendMailText;


(* receives text from SMTP server *)
PROCEDURE Receive(smtp: T): BOOLEAN;

VAR setRead:   Socket.fd_set;
    setWrite:  Socket.fd_set;
    setExcept: Socket.fd_set;
    szText:    ARRAY [0..400] OF CHAR;
    nSelect:   INTEGER;
    bValid:    BOOLEAN;

BEGIN
  bValid:=FALSE;
  Socket.FD_ZERO(setRead);
  Socket.FD_ZERO(setWrite);
  Socket.FD_ZERO(setExcept);
  Socket.FD_SET(smtp^.socket,setRead);
  nSelect:=Socket.Select(setRead,setWrite,setExcept,-1);
  IF (nSelect>0) THEN
    IF Socket.FD_ISSET(smtp^.socket,setRead) THEN
      IF Socket.ReceiveText(smtp^.socket,szText) THEN
        Strings.Extract(szText,0,3,smtp^.szReply);
        bValid:=TRUE;
      END; (* if *)
    END; (* if *)
  END; (* if *)
  RETURN bValid;
END Receive;


(* returns last reply from SMTP server *)
PROCEDURE GetLastReply(    smtp:    T;
                       VAR szReply: ARRAY OF CHAR);
BEGIN
  szReply:=smtp^.szReply;
END GetLastReply;



BEGIN
  szCRLF[0]:=ASCII.cr;
  szCRLF[1]:=ASCII.lf;
  szCRLF[2]:=0C;

  szMonth[1]:="Jan";
  szMonth[2]:="Feb";
  szMonth[3]:="Mar";
  szMonth[4]:="Apr";
  szMonth[5]:="May";
  szMonth[6]:="Jun";
  szMonth[7]:="Jul";
  szMonth[8]:="Aug";
  szMonth[9]:="Sep";
  szMonth[10]:="Oct";
  szMonth[11]:="Nov";
  szMonth[12]:="Dec";
END SMTP.
