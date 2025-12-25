UNSAFEGUARDED IMPLEMENTATION MODULE RPC;

FROM WIN32 IMPORT
    LONG, ULONG, BYTE, DWORD, WINT, LPBYTE, PULONG, ULONG_PTR;

FROM SYSTEM IMPORT
    ADDADR, CAST, ADR, ADDRESS;

FROM RPCCore IMPORT
    RPC_BINDING_HANDLE, RPC_MESSAGE;

<*/NOWARN:I*>
<*/CALLS:WINDOWSCALL*>
<*/NOHIGH*>

PROCEDURE NDRSContextValue(hContext : NDR_SCONTEXT) : ADDRESS;
BEGIN
    RETURN ADR(hContext^.userContext);
END NDRSContextValue;

PROCEDURE byte_from_ndr(VAR source : RPC_MESSAGE; VAR target : BYTE);
BEGIN
    target := CAST(LPBYTE, source.Buffer)^;
    source.Buffer := ADDADR(source.Buffer, 1);
END byte_from_ndr;

PROCEDURE byte_array_from_ndr(VAR Source : RPC_MESSAGE;
                              LowerIndex : DWORD;
                              UpperIndex : DWORD;
                              VAR Target : ARRAY OF BYTE);
VAR
    temp        : ADDRESS;
BEGIN
    temp := ADDADR(ADR(Target), LowerIndex);
    NDRcopy(temp, Source.Buffer, UpperIndex - LowerIndex);
    Source.Buffer := ADDADR(Source.Buffer, UpperIndex - LowerIndex);
END byte_array_from_ndr;

PROCEDURE boolean_from_ndr(VAR source : RPC_MESSAGE; VAR target : BOOLEAN);
VAR
    byte        : BYTE;
BEGIN
    byte_from_ndr(source, byte);
    target := byte <> 0;
END boolean_from_ndr;

(*USER MUST NORMALIZE THESE BOOLEANS AS THE MACRO DOES NOT*)
PROCEDURE boolean_array_from_ndr(VAR Source : RPC_MESSAGE;
                              LowerIndex : DWORD;
                              UpperIndex : DWORD;
                              VAR Target : ARRAY OF BYTE);
BEGIN
    byte_array_from_ndr(Source, LowerIndex, UpperIndex, Target);
END boolean_array_from_ndr;

PROCEDURE MIDL_ascii_strlen(string : ARRAY OF ACHAR) : WINT;
VAR
    i   : CARDINAL = 0;
BEGIN
    WHILE string[i] <> 0c DO
        INC(i);
    END;

    RETURN i;
END MIDL_ascii_strlen;

PROCEDURE MIDL_ascii_strcpy(VAR target : ARRAY OF ACHAR; source : ARRAY OF ACHAR) : WINT;
VAR
    i   : CARDINAL = 0;
BEGIN
    WHILE source[i] <> 0c DO
        target[i] := source[i];
        INC(i);
    END;
    RETURN 0;
END MIDL_ascii_strcpy;

PROCEDURE MIDL_memset(VAR s : ARRAY OF BYTE; c : BYTE; n : WINT);
VAR
    i   : WINT = 0;
BEGIN
    WHILE i <= n DO
        s[i] := c;
        INC(i);
    END;
END MIDL_memset;

PROCEDURE _midl_fa2(VAR p : RPC_BUFPTR);
BEGIN
    p := CAST(RPC_BUFPTR, (CAST(ULONG_PTR, p) + 1) BAND CAST(ULONG_PTR, -2) (*0fffffffeh*) );
END _midl_fa2;

PROCEDURE _midl_fa4(VAR p : RPC_BUFPTR);
BEGIN
    p := CAST(RPC_BUFPTR, (CAST(ULONG_PTR, p) + 3) BAND CAST(ULONG_PTR, -4) (*0fffffffch*) );
END _midl_fa4;

PROCEDURE _midl_fa8(VAR p : RPC_BUFPTR);
BEGIN
    p := CAST(RPC_BUFPTR, (CAST(ULONG_PTR, p) + 7) BAND CAST(ULONG_PTR, -8) (*0fffffff8h*) );
END _midl_fa8;

PROCEDURE NdrMarshCCtxtHdl(pc : NDR_CCONTEXT; p : ADDRESS) : ADDRESS;
BEGIN
    NDRCContextMarshall(pc, p );
    RETURN ADDADR(p,20);
END NdrMarshCCtxtHdl;

PROCEDURE NdrUnMarshCCtxtHdl(VAR pc : NDR_CCONTEXT;
                             p : RPC_BINDING_HANDLE;
                             h : ADDRESS;
                             drep : ULONG) : ADDRESS;
BEGIN
    NDRCContextUnmarshall(pc,h,p,drep);
    RETURN ADDADR(ADR(p), 20);
END NdrUnMarshCCtxtHdl;

PROCEDURE NdrUnMarshSCtxtHdl(pc : NDR_SCONTEXT;
                             p : ADDRESS;
                             drep : ULONG);
BEGIN
    pc := NDRSContextUnmarshall(p,drep);
END NdrUnMarshSCtxtHdl;

PROCEDURE NdrMarshSCtxtHdl(CContext : NDR_SCONTEXT;
                          pBuff : ADDRESS;
                          userRunDownIn : NDR_RUNDOWN);
BEGIN
    NDRSContextMarshall(CContext, pBuff, userRunDownIn);
END NdrMarshSCtxtHdl;

PROCEDURE _midl_marsh_up(VAR mp : ADDRESS; p : ULONG);
VAR
    tp : PULONG;
BEGIN
     tp := CAST(PULONG, mp);
     tp^ := p;
     mp := ADDADR(mp, SIZE(p));
END _midl_marsh_up;

PROCEDURE _midl_advmp(VAR mp : ADDRESS) : ULONG;
VAR
    tmp : LONG;
BEGIN
    tmp := CAST(PULONG, mp)^;
    mp := ADDADR(mp, SIZE(ULONG));
    RETURN tmp;
END _midl_advmp;

PROCEDURE _midl_unmarsh_up(VAR p : ADDRESS) : ULONG;
VAR
    tmp : LONG;
BEGIN
    tmp := CAST(PULONG, p)^;
    p := ADDADR(p, SIZE(ULONG));
    RETURN tmp;
END _midl_unmarsh_up;

END RPC.
