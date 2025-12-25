IMPLEMENTATION MODULE WIN32;
<*/NOWARN:I*>

<*/CALLS:WINDOWSCALL*>
<*/NOHIGH*>

PROCEDURE Int32x32To64(a, b : LONG) : LONGLONG [Invariant];
BEGIN
    RETURN VAL(LONGLONG,a) * VAL(LONGLONG,b);
END Int32x32To64;

PROCEDURE Int64ShllMod32(a : DWORDLONG; b : DWORD) : DWORDLONG [Invariant];
BEGIN
    RETURN VAL(DWORDLONG, VAL(LONGINT,a) SHL b);
END Int64ShllMod32;

PROCEDURE Int64ShrlMod32(a : DWORDLONG; b : DWORD) : DWORDLONG [Invariant];
BEGIN
    RETURN VAL(DWORDLONG, VAL(LONGINT, a) SHR b);
END Int64ShrlMod32;

PROCEDURE HEAP_MAKE_TAG_FLAGS( b, o : DWORD) : DWORD;
BEGIN
    RETURN b + (o SHL 16);
END HEAP_MAKE_TAG_FLAGS;

PROCEDURE INIT_GUID(VAR guid : GUID;
                    l : ULONG;
                    w1 : USHORT;
                    w2 : USHORT;
                    b1, b2, b3, b4, b5, b6, b7, b8 : BYTE) [Invariant];
BEGIN
    WITH guid DO
        Data1 := l;
        Data2 := w1;
        Data3 := w2;
        Data4[0] := b1;
        Data4[1] := b2;
        Data4[2] := b3;
        Data4[3] := b4;
        Data4[4] := b5;
        Data4[5] := b6;
        Data4[6] := b7;
        Data4[7] := b8;
    END;
END INIT_GUID;

END WIN32.
