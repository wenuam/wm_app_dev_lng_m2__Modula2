UNSAFEGUARDED IMPLEMENTATION MODULE VFW;

IMPORT SYSTEM;

FROM WINX IMPORT
    NIL_LONG;

FROM SYSTEM IMPORT
    ADDRESS, CAST, ADR, FUNC;

FROM WIN32 IMPORT
    DWORD, LONG, BOOL, WORD, TCHAR, LPVOID, LPARAM, WPARAM, LPPOINT,
    HWND, LRESULT, UINT, WINT, HPALETTE, HDC, RECT, BYTE, SCODE, DWORD_PTR;

FROM WINERROR IMPORT
    FACILITY_ITF, MAKE_SCODE, SEVERITY_ERROR;

FROM WINGDI IMPORT
    LPBITMAPINFOHEADER;

FROM MMSYSTEM IMPORT
    LPWAVEFORMATEX;

FROM OOle2 IMPORT
    IUnknown;

FROM WINUSER IMPORT
    IsWindow, SendMessage, LOBYTE, HIBYTE, LOWORD, HIWORD,
    MAKELONG, WM_CLOSE;

<*/CALLS:WINDOWSCALL*>
<*/NOHIGH*>
<*/NOPACK*>
<*/NOWARN:I*>

PROCEDURE AVICapSM(hwnd : HWND; m : UINT; w : WPARAM; l : LPARAM) : UINT;
BEGIN
    IF IsWindow(hwnd) THEN
        RETURN SendMessage(hwnd,m,w,l);
    ELSE
        RETURN 0;
    END;
END AVICapSM;

PROCEDURE SB_AVICapSM(hwnd : HWND; m : UINT; w : WPARAM; l : LPARAM) : UINT [INLINE];
BEGIN
    IF IsWindow(hwnd) THEN
        RETURN SendMessage(hwnd,m,w,l);
    ELSE
        RETURN 0;
    END;
END SB_AVICapSM;

PROCEDURE MKFOURCC(c0, c1, c2, c3 : ACHAR) : DWORD;
BEGIN
    RETURN ORD(c0) BOR (ORD(c1) SHL 8) BOR (ORD(c2) SHL 16) BOR (ORD(c3) SHL 24);
END MKFOURCC;

PROCEDURE aviTWOCC(ch0, ch1 : ACHAR) : WORD;
BEGIN
    RETURN VAL(WORD, ORD(ch0)) BOR VAL(WORD, ORD(ch1) SHL 8);
END aviTWOCC;

PROCEDURE ICQueryAbout(hic : HIC) : BOOLEAN;
BEGIN
    %IF Bits32 %THEN
    RETURN ICSendMessage(hic, ICM_ABOUT, 0ffffFFFFh, ICMF_ABOUT_QUERY) = ICERR_OK;
    %ELSE
    RETURN ICSendMessage(hic, ICM_ABOUT, 0ffffFFFFffffFFFFh, ICMF_ABOUT_QUERY) = ICERR_OK;
    %END
END ICQueryAbout;

PROCEDURE ICAbout(hic : HIC; hwnd : HWND) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_ABOUT, CAST(DWORD_PTR, hwnd), 0);
END ICAbout;

PROCEDURE ICQueryConfigure(hic : HIC) : BOOLEAN;
BEGIN
    %IF Bits32 %THEN
    RETURN ICSendMessage(hic, ICM_CONFIGURE, 0ffffFFFFh, ICMF_CONFIGURE_QUERY) = ICERR_OK;
    %ELSE
    RETURN ICSendMessage(hic, ICM_CONFIGURE, 0ffffFFFFffffFFFFh, ICMF_CONFIGURE_QUERY) = ICERR_OK;
    %END
END ICQueryConfigure;

PROCEDURE ICConfigure(hic : HIC; hwnd : HWND) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_CONFIGURE, CAST(DWORD_PTR, hwnd), 0);
END ICConfigure;

PROCEDURE  ICGetState(hic : HIC; pv : LPVOID; cb : DWORD) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_GETSTATE, CAST(DWORD_PTR, pv), cb);
END ICGetState;

PROCEDURE ICSetState(hic : HIC; pv : LPVOID; cb : DWORD) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_SETSTATE, CAST(DWORD_PTR, pv), cb);
END ICSetState;

PROCEDURE ICGetStateSize(hic : HIC) : LRESULT;
BEGIN
    RETURN ICGetState(hic, NIL, 0);
END ICGetStateSize;

PROCEDURE ICGetDefaultQuality(hic : HIC) : DWORD;
VAR
    dw  : DWORD;
BEGIN
    dw := 0;
    FUNC ICSendMessage(hic, ICM_GETDEFAULTQUALITY, CAST(DWORD_PTR, ADR(dw)), SIZE(DWORD));
    RETURN dw;
END ICGetDefaultQuality;

PROCEDURE ICGetDefaultKeyFrameRate(hic : HIC) : LRESULT;
VAR
    dw  : DWORD;
BEGIN
    dw := 0;
    FUNC ICSendMessage(hic, ICM_GETDEFAULTKEYFRAMERATE,
                        CAST(DWORD_PTR, ADR(dw)), SIZE(DWORD));
    RETURN dw;
END ICGetDefaultKeyFrameRate;

PROCEDURE ICDrawWindow(hic : HIC; prc : RECT) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_WINDOW, CAST(DWORD_PTR, ADR(prc)), SIZE(RECT));
END ICDrawWindow;

PROCEDURE ICCompressBegin(hic : HIC; lpbiInput, lpbiOutput : LPBITMAPINFOHEADER) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_COMPRESS_BEGIN,
                        CAST(DWORD_PTR, lpbiInput), CAST(DWORD, lpbiOutput));
END ICCompressBegin;

PROCEDURE ICCompressQuery(hic : HIC; lpbiInput, lpbiOutput : LPBITMAPINFOHEADER) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_COMPRESS_QUERY,
                        CAST(DWORD_PTR, lpbiInput), CAST(DWORD, lpbiOutput));
END ICCompressQuery;

PROCEDURE ICCompressGetFormat(hic : HIC; lpbiInput, lpbiOutput : LPBITMAPINFOHEADER) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_COMPRESS_GET_FORMAT,
                        CAST(DWORD_PTR, lpbiInput), CAST(DWORD, lpbiOutput));
END ICCompressGetFormat;

PROCEDURE ICCompressGetFormatSize(hic : HIC; lpbi : LPBITMAPINFOHEADER)  : LRESULT;
BEGIN
    RETURN ICCompressGetFormat(hic, lpbi, NIL);
END ICCompressGetFormatSize;

PROCEDURE ICCompressGetSize(hic : HIC; lpbiInput, lpbiOutput : LPBITMAPINFOHEADER) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_COMPRESS_GET_SIZE,
                        CAST(DWORD_PTR, lpbiInput), CAST(DWORD, lpbiOutput));
END ICCompressGetSize;

PROCEDURE ICCompressEnd(hic : HIC) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_COMPRESS_END, 0, 0);
END ICCompressEnd;

PROCEDURE ICDecompressBegin(hic : HIC; lpbiInput, lpbiOutput : LPBITMAPINFOHEADER) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DECOMPRESS_BEGIN,
                        CAST(DWORD_PTR, lpbiInput), CAST(DWORD, lpbiOutput));
END ICDecompressBegin;

PROCEDURE ICDecompressQuery(hic : HIC; lpbiInput, lpbiOutput : LPBITMAPINFOHEADER) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DECOMPRESS_QUERY,
                        CAST(DWORD_PTR, lpbiInput), CAST(DWORD, lpbiOutput));
END ICDecompressQuery;

PROCEDURE ICDecompressGetFormat(hic : HIC; lpbiInput, lpbiOutput : LPBITMAPINFOHEADER) : LONG;
BEGIN
    RETURN ICSendMessage(hic, ICM_DECOMPRESS_GET_FORMAT,
                        CAST(DWORD_PTR, lpbiInput), CAST(DWORD, lpbiOutput));
END ICDecompressGetFormat;

PROCEDURE ICDecompressGetFormatSize(hic: HIC; lpbi : LPBITMAPINFOHEADER) : LONG;
BEGIN
    RETURN ICDecompressGetFormat(hic, lpbi, NIL);
END ICDecompressGetFormatSize;

PROCEDURE ICDecompressGetPalette(hic : HIC; lpbiInput, lpbiOutput : LPBITMAPINFOHEADER) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DECOMPRESS_GET_PALETTE,
                        CAST(DWORD_PTR, lpbiInput), CAST(DWORD, lpbiOutput));
END ICDecompressGetPalette;

PROCEDURE ICDecompressSetPalette(hic : HIC; lpbiPalette : LPBITMAPINFOHEADER) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DECOMPRESS_SET_PALETTE, CAST(DWORD, lpbiPalette), 0);
END ICDecompressSetPalette;

PROCEDURE ICDecompressEnd(hic : HIC) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DECOMPRESS_END, 0, 0);
END ICDecompressEnd;

%IF Windows %THEN
PROCEDURE ICDrawSuggestFormat(
                                hic : HIC;
                                lpbiIn : LPBITMAPINFOHEADER;
                                lpbiOut : LPBITMAPINFOHEADER;
                                dxSrc : WINT;
                                dySrc : WINT;
                                dxDst : WINT;
                                dyDst : WINT;
                                hicDecomp : HIC) : LRESULT;
VAR
    ic          : ICDRAWSUGGEST;
BEGIN

    ic.lpbiIn := lpbiIn;
    ic.lpbiSuggest := lpbiOut;
    ic.dxSrc := dxSrc;
    ic.dySrc := dySrc;
    ic.dxDst := dxDst;
    ic.dyDst := dyDst;
    ic.hicDecompressor := hicDecomp;

    RETURN ICSendMessage(hic, ICM_DRAW_SUGGESTFORMAT,
                         CAST(DWORD_PTR, ADR(ic)), SIZE(ic));
END ICDrawSuggestFormat;
%ELSE
PROCEDURE ICDrawSuggestFormat(
                                hic : HIC;
                                lpbiIn : LPBITMAPINFOHEADER;
                                lpbiOut : LPBITMAPINFOHEADER;
                                dxSrc : WINT;
                                dySrc : WINT;
                                dxDst : WINT;
                                dyDst : WINT;
                                hicDecomp : HIC) : LRESULT;
BEGIN
    RETURN ICMessage(hic, ICM_DRAW_SUGGESTFORMAT, SIZE(ICDRAWSUGGEST),
            lpbiIn, lpbiOut), dxSrc, dySrc,dxDst,dyDst, hicDecomp);
END ICDrawSuggestFormat;

%END

PROCEDURE ICDrawQuery(hic : HIC; lpbiInput : LPBITMAPINFOHEADER) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_QUERY, CAST(DWORD_PTR, lpbiInput), 0);
END ICDrawQuery;

PROCEDURE ICDrawChangePalette(hic : HIC; lpbiInput : LPBITMAPINFOHEADER) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_CHANGEPALETTE, CAST(DWORD_PTR, lpbiInput), 0);
END ICDrawChangePalette;

PROCEDURE ICGetBuffersWanted(hic : HIC; VAR lpdwBuffers : DWORD) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_GETBUFFERSWANTED, CAST(DWORD_PTR, ADR(lpdwBuffers)), 0);
END ICGetBuffersWanted;

PROCEDURE ICDrawEnd(hic : HIC) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_END, 0, 0);
END ICDrawEnd;

PROCEDURE ICDrawStart(hic : HIC) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_START, 0, 0);
END ICDrawStart;

PROCEDURE ICDrawStartPlay(hic : HIC; lFrom, lTo : DWORD_PTR) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_START_PLAY, lFrom, lTo);
END ICDrawStartPlay;

PROCEDURE ICDrawStop(hic : HIC) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_STOP, 0, 0);
END ICDrawStop;

PROCEDURE ICDrawStopPlay(hic : HIC) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_STOP_PLAY, 0, 0);
END ICDrawStopPlay;

PROCEDURE ICDrawGetTime(hic : HIC; VAR lplTime : LONG) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_GETTIME, CAST(DWORD_PTR, ADR(lplTime)), 0);
END ICDrawGetTime;

PROCEDURE ICDrawSetTime(hic : HIC; lTime : LONG) : LRESULT;
BEGIN
    (*???RSG*)
    RETURN ICSendMessage(hic, ICM_DRAW_SETTIME, CAST(DWORD, lTime), 0);
END ICDrawSetTime;

PROCEDURE ICDrawRealize(hic : HIC; hdc : HDC; fBackground : BOOL) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_REALIZE, CAST(DWORD_PTR, hdc),
                                CAST(DWORD, fBackground));
END ICDrawRealize;

PROCEDURE ICDrawFlush(hic : HIC) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_FLUSH, 0, 0);
END ICDrawFlush;

PROCEDURE ICDrawRenderBuffer(hic : HIC) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DRAW_RENDERBUFFER, 0, 0);
END ICDrawRenderBuffer;

%IF Windows %THEN

PROCEDURE ICSetStatusProc(
            hic : HIC;
            dwFlags : DWORD;
            lParam : LRESULT;
            fpfnStatus : FPFN_PROC
             ) : LRESULT;

VAR
    ic : ICSETSTATUSPROC;
BEGIN

    ic.dwFlags := dwFlags;
    ic.lParam := lParam;
    ic.Status := fpfnStatus;

    RETURN ICSendMessage(hic, ICM_SET_STATUS_PROC, CAST(DWORD_PTR, ADR(ic)),
                        SIZE(ic));
END ICSetStatusProc;

%ELSE

PROCEDURE ICSetStatusProc(
            hic : HIC;
            dwFlags : DWORD;
            lParam : LRESULT;
            fpfnStatus : FPFN_PROC
             ) : LRESULT;

VAR
    ic : ICSETSTATUSPROC;
BEGIN

    ic.dwFlags = dwFlags;
    ic.lParam = lParam;
    ic.Status = fpfnStatus;

    RETURN ICMessage(hic, ICM_SET_STATUS_PROC, SIZE(ic), CAST(DWORD, dwFlags),
                        lParam, fpfnStatus);
END ICSetStatusProc;

%END

PROCEDURE ICDecompressOpen(fccType, fccHandler : DWORD;
                           lpbiIn, lpbiOut : LPBITMAPINFOHEADER) : HIC;
BEGIN
    RETURN ICLocate(fccType, fccHandler, lpbiIn, lpbiOut, ICMODE_DECOMPRESS);
END ICDecompressOpen;

PROCEDURE ICDrawOpen(fccType, fccHandler : DWORD; lpbiIn : LPBITMAPINFOHEADER) : HIC;
BEGIN
    RETURN ICLocate(fccType, fccHandler, lpbiIn, NIL, ICMODE_DRAW);
END ICDrawOpen;

PROCEDURE DrawDibUpdate(hdd : HDRAWDIB; hdc : HDC; x, y : WINT) : BOOL;
BEGIN
    RETURN DrawDibDraw(hdd, hdc, x, y, 0, 0, NIL, NIL, 0, 0, 0, 0, DDF_UPDATE);
END DrawDibUpdate;

PROCEDURE FromHex(n : ACHAR) : DWORD;
BEGIN
    IF n >= 'A' THEN
        RETURN ORD(n) + 10 - ORD('A');
    ELSE
        RETURN ORD(n) - ORD('0');
    END;
END FromHex;

PROCEDURE StreamFromFOURCC(fcc : FOURCC) : WORD;
BEGIN
    RETURN VAL(WORD, FromHex( CAST(ACHAR, LOBYTE(LOWORD(fcc)) )) SHL 4 +
                     FromHex( CAST(ACHAR, HIBYTE(LOWORD(fcc)) ))
               );
END StreamFromFOURCC;

PROCEDURE TWOCCFromFOURCC(fcc : FOURCC) : TWOCC;
BEGIN
   RETURN HIWORD(fcc);
END TWOCCFromFOURCC;

PROCEDURE ToHex(n : DWORD) : ACHAR;
BEGIN
    IF n > 9 THEN
        RETURN ACHR(n - 10 + ORD('A'));
    ELSE
        RETURN ACHR(n + ORD('0') );
    END;
END ToHex;

PROCEDURE MAKEAVICKID(tcc : TWOCC; stream : DWORD) : LONG;
BEGIN
    RETURN MAKELONG( (VAL(DWORD, CAST(BYTE, ToHex(stream BAND 00fh))) SHL 8) BOR
                     (VAL(DWORD, CAST(BYTE, ToHex(stream BAND 0f0h))) SHR 4), tcc);
END MAKEAVICKID;

PROCEDURE AVIStreamSampleToSample(pavi1, pavi2 : PAVISTREAM; l : LONG) : LONG;
BEGIN
    RETURN AVIStreamTimeToSample(pavi1,AVIStreamSampleToTime(pavi2, l));
END AVIStreamSampleToSample;

PROCEDURE AVIStreamNextSample(pavi : PAVISTREAM; l : LONG) : LONG;
BEGIN
    RETURN AVIStreamFindSample(pavi,l+1,FIND_NEXT BOR FIND_ANY);
END AVIStreamNextSample;

PROCEDURE AVIStreamPrevSample(pavi : PAVISTREAM; l : LONG) : LONG;
BEGIN
    RETURN AVIStreamFindSample(pavi,l-1,FIND_PREV BOR FIND_ANY);
END AVIStreamPrevSample;

PROCEDURE AVIStreamNearestSample(pavi : PAVISTREAM; l : LONG) : LONG;
BEGIN
    RETURN AVIStreamFindSample(pavi,l,FIND_PREV BOR FIND_ANY);
END AVIStreamNearestSample;

PROCEDURE AVIStreamNextKeyFrame(pavi : PAVISTREAM; l : LONG) : LONG;
BEGIN
    RETURN AVIStreamFindSample(pavi,l+1,FIND_NEXT BOR FIND_KEY);
END AVIStreamNextKeyFrame;

PROCEDURE AVIStreamPrevKeyFrame(pavi : PAVISTREAM; l : LONG) : LONG;
BEGIN
    RETURN AVIStreamFindSample(pavi,l-1,FIND_PREV BOR FIND_KEY);
END AVIStreamPrevKeyFrame;

PROCEDURE AVIStreamNearestKeyFrame(pavi : PAVISTREAM; l : LONG) : LONG;
BEGIN
    RETURN AVIStreamFindSample(pavi,l,FIND_PREV BOR FIND_KEY);
END AVIStreamNearestKeyFrame;

PROCEDURE AVIStreamIsKeyFrame(pavi : PAVISTREAM; l : LONG) : BOOL;
BEGIN
    RETURN AVIStreamNearestKeyFrame(pavi,l) = l;
END AVIStreamIsKeyFrame;

PROCEDURE AVIStreamPrevSampleTime(pavi : PAVISTREAM; t : LONG) : LONG;
BEGIN
    RETURN AVIStreamSampleToTime(pavi,
                AVIStreamPrevSample(pavi,AVIStreamTimeToSample(pavi,t)));
END AVIStreamPrevSampleTime;

PROCEDURE AVIStreamNextSampleTime(pavi : PAVISTREAM; t : LONG) : LONG;
BEGIN
    RETURN AVIStreamSampleToTime(pavi,
                AVIStreamNextSample(pavi,AVIStreamTimeToSample(pavi,t)));
END AVIStreamNextSampleTime;

PROCEDURE AVIStreamNearestSampleTime(pavi : PAVISTREAM; t : LONG) : LONG;
BEGIN
    RETURN AVIStreamSampleToTime(pavi,
                AVIStreamNearestSample(pavi,AVIStreamTimeToSample(pavi,t)));
END AVIStreamNearestSampleTime;

PROCEDURE AVIStreamNextKeyFrameTime(pavi : PAVISTREAM; t : LONG) : LONG;
BEGIN
    RETURN AVIStreamSampleToTime(pavi,
                AVIStreamNextKeyFrame(pavi,AVIStreamTimeToSample(pavi, t)));
END AVIStreamNextKeyFrameTime;

PROCEDURE AVIStreamPrevKeyFrameTime(pavi : PAVISTREAM; t : LONG) : LONG;
BEGIN
    RETURN AVIStreamSampleToTime(pavi,
                AVIStreamPrevKeyFrame(pavi,AVIStreamTimeToSample(pavi, t)));
END AVIStreamPrevKeyFrameTime;

PROCEDURE AVIStreamNearestKeyFrameTime(pavi : PAVISTREAM; t : LONG) : LONG;
BEGIN
    RETURN AVIStreamSampleToTime(pavi,
                AVIStreamNearestKeyFrame(pavi,AVIStreamTimeToSample(pavi, t)));
END AVIStreamNearestKeyFrameTime;

PROCEDURE AVIStreamStartTime(pavi : PAVISTREAM) : LONG;
BEGIN
    RETURN AVIStreamSampleToTime(pavi, AVIStreamStart(pavi));
END AVIStreamStartTime;

PROCEDURE AVIStreamLengthTime(pavi : PAVISTREAM) : LONG;
BEGIN
    RETURN AVIStreamSampleToTime(pavi, AVIStreamLength(pavi));
END AVIStreamLengthTime;

PROCEDURE AVIStreamEnd(pavi : PAVISTREAM) : LONG;
BEGIN
    RETURN AVIStreamStart(pavi) + AVIStreamLength(pavi);
END AVIStreamEnd;

PROCEDURE AVIStreamEndTime(pavi : PAVISTREAM) : LONG;
BEGIN
    RETURN AVIStreamSampleToTime(pavi, AVIStreamEnd(pavi));
END AVIStreamEndTime;

PROCEDURE AVIStreamSampleSize(pavi : PAVISTREAM; lPos : LONG; VAR plSize : LONG) : LONG;
BEGIN
    RETURN AVIStreamRead(pavi, lPos, 1, NIL, 0, plSize, NIL_LONG);
END AVIStreamSampleSize;

PROCEDURE AVIStreamFormatSize(pavi : PAVISTREAM; lPos : LONG; VAR plSize : LONG) : LONG;
BEGIN
    RETURN AVIStreamReadFormat(pavi, lPos, NIL, plSize);
END AVIStreamFormatSize;

PROCEDURE AVIStreamDataSize(pavi : PAVISTREAM; fcc : FOURCC; VAR plSize : LONG) : LONG;
BEGIN
    RETURN AVIStreamReadData(pavi, fcc, NIL, plSize);
END AVIStreamDataSize;

PROCEDURE MAKE_AVIERR(error : LONG) : SCODE;
BEGIN
    RETURN MAKE_SCODE(SEVERITY_ERROR, FACILITY_ITF, 4000h + error);
END MAKE_AVIERR;

PROCEDURE MCIWndCanPlay(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,MCIWNDM_CAN_PLAY,0,0));
END MCIWndCanPlay;

PROCEDURE MCIWndCanRecord(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,MCIWNDM_CAN_RECORD,0,0));
END MCIWndCanRecord;

PROCEDURE MCIWndCanSave(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,MCIWNDM_CAN_SAVE,0,0));
END MCIWndCanSave;

PROCEDURE MCIWndCanWindow(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,MCIWNDM_CAN_WINDOW,0,0));
END MCIWndCanWindow;

PROCEDURE MCIWndCanEject(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,MCIWNDM_CAN_EJECT,0,0));
END MCIWndCanEject;

PROCEDURE MCIWndCanConfig(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,MCIWNDM_CAN_CONFIG,0,0));
END MCIWndCanConfig;

PROCEDURE MCIWndPaletteKick(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,MCIWNDM_PALETTEKICK,0,0));
END MCIWndPaletteKick;

PROCEDURE MCIWndSave(hwnd : HWND; szFile : ARRAY OF ACHAR) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCI_SAVE, 0, CAST(LPARAM, ADR(szFile)));
END MCIWndSave;

PROCEDURE MCIWndSaveDialog(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCI_SAVE, 0, CAST(LPARAM, -1));
END MCIWndSaveDialog;

PROCEDURE MCIWndNew(hwnd : HWND; lp : ADDRESS) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_NEW, 0, CAST(LPARAM, lp));
END MCIWndNew;

PROCEDURE MCIWndRecord(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCI_RECORD, 0, 0);
END MCIWndRecord;

%IF Windows %THEN
PROCEDURE MCIWndOpen(hwnd : HWND; sz : ARRAY OF ACHAR; f : UINT) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_OPEN, f, CAST(LPARAM, ADR(sz)));
END MCIWndOpen;

%ELSE
PROCEDURE MCIWndOpen(hwnd : HWND; sz : ARRAY OF ACHAR; f : UINT) : LONG;
BEGIN
    RETURN CAST(LONG, SendMessage(hwnd, MCI_OPEN,
                                CAST(WPARAM, f), CAST(LPARAM ADR(sz)));
END MCIWndOpen;
%END

PROCEDURE MCIWndOpenDialog(hwnd : HWND) : LONG;
VAR
    MINUS1      [SYSTEM.MAKEADR(-1)] : ARRAY [0..0] OF ACHAR;
BEGIN
    RETURN MCIWndOpen(hwnd, MINUS1, 0);
END MCIWndOpenDialog;

PROCEDURE MCIWndClose(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCI_CLOSE, 0, 0);
END MCIWndClose;

PROCEDURE MCIWndPlay(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCI_PLAY, 0, 0);
END MCIWndPlay;

PROCEDURE MCIWndStop(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCI_STOP, 0, 0);
END MCIWndStop;

PROCEDURE MCIWndPause(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCI_PAUSE, 0, 0);
END MCIWndPause;

PROCEDURE MCIWndResume(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCI_RESUME, 0, 0);
END MCIWndResume;

PROCEDURE MCIWndSeek(hwnd : HWND; lPos : LONG) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCI_SEEK, 0, lPos);
END MCIWndSeek;

PROCEDURE MCIWndEject(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_EJECT, 0, 0);
END MCIWndEject;

PROCEDURE MCIWndHome(hwnd : HWND) : LONG;
BEGIN
    RETURN MCIWndSeek(hwnd, MCIWND_START);
END MCIWndHome;

PROCEDURE MCIWndEnd(hwnd : HWND) : LONG;
BEGIN
    RETURN MCIWndSeek(hwnd, MCIWND_END);
END MCIWndEnd;

PROCEDURE MCIWndGetSource(hwnd : HWND; VAR prc : RECT): LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GET_SOURCE, 0, CAST(LPARAM, ADR(prc)));
END MCIWndGetSource;

PROCEDURE MCIWndPutSource(hwnd : HWND; prc : RECT) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_PUT_SOURCE, 0, CAST(LPARAM, ADR(prc)));
END MCIWndPutSource;

PROCEDURE MCIWndGetDest(hwnd : HWND; VAR prc : RECT): LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GET_DEST, 0, CAST(LPARAM, ADR(prc)));
END MCIWndGetDest;

PROCEDURE MCIWndPutDest(hwnd : HWND; VAR prc : RECT): LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_PUT_DEST, 0, CAST(LPARAM, prc));
END MCIWndPutDest;

PROCEDURE MCIWndPlayReverse(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_PLAYREVERSE, 0, 0);
END MCIWndPlayReverse;

PROCEDURE MCIWndPlayFrom(hwnd : HWND; lPos : LONG) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_PLAYFROM, 0, lPos);
END MCIWndPlayFrom;

PROCEDURE MCIWndPlayTo(hwnd : HWND; lPos : LONG) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_PLAYTO, 0, lPos);
END MCIWndPlayTo;

PROCEDURE MCIWndPlayFromTo(hwnd : HWND; lStart, lEnd : LONG) : LONG;
BEGIN
    FUNC MCIWndSeek(hwnd, lStart);
    RETURN MCIWndPlayTo(hwnd, lEnd);
END MCIWndPlayFromTo;

PROCEDURE MCIWndGetDeviceID(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, MCIWNDM_GETDEVICEID, 0, 0));
END MCIWndGetDeviceID;

PROCEDURE MCIWndGetAlias(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, MCIWNDM_GETALIAS, 0, 0));
END MCIWndGetAlias;

PROCEDURE MCIWndGetMode(hwnd : HWND; VAR lp : ARRAY OF TCHAR; len : LONG) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETMODE,
                        VAL(WPARAM, len), CAST(LPARAM, ADR(lp)));
END MCIWndGetMode;

PROCEDURE MCIWndGetPosition(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETPOSITION, 0, 0);
END MCIWndGetPosition;

PROCEDURE MCIWndGetPositionString(hwnd : HWND;
                                   VAR lp : ARRAY OF TCHAR;
                                   len : UINT) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETPOSITION, len,
                CAST(LPARAM, ADR(lp)));
END MCIWndGetPositionString;

PROCEDURE MCIWndGetStart(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETSTART, 0, 0);
END MCIWndGetStart;

PROCEDURE MCIWndGetLength(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETLENGTH, 0, 0);
END MCIWndGetLength;

PROCEDURE MCIWndGetEnd(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETEND, 0, 0);
END MCIWndGetEnd;

PROCEDURE MCIWndStep(hwnd : HWND; n : LONG) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCI_STEP, 0, n);
END MCIWndStep;

PROCEDURE MCIWndDestroy(hwnd : HWND);
BEGIN
    FUNC SendMessage(hwnd, WM_CLOSE, 0, 0);
END MCIWndDestroy;

PROCEDURE MCIWndSetZoom(hwnd : HWND; iZoom : UINT);
BEGIN
    FUNC SendMessage(hwnd, MCIWNDM_SETZOOM, 0, VAL(LPARAM, iZoom));
END MCIWndSetZoom;

PROCEDURE MCIWndGetZoom(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, MCIWNDM_GETZOOM, 0, 0));
END MCIWndGetZoom;

PROCEDURE MCIWndSetVolume(hwnd : HWND; iVol : UINT) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_SETVOLUME, 0, VAL(LPARAM, iVol));
END MCIWndSetVolume;

PROCEDURE MCIWndGetVolume(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETVOLUME, 0, 0);
END MCIWndGetVolume;

PROCEDURE MCIWndSetSpeed(hwnd : HWND; iSpeed : UINT) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_SETSPEED, 0, VAL(LPARAM, iSpeed));
END MCIWndSetSpeed;

PROCEDURE MCIWndGetSpeed(hwnd : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETSPEED, 0, 0);
END MCIWndGetSpeed;

PROCEDURE MCIWndSetTimeFormat(hwnd : HWND; lp : ARRAY OF TCHAR) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_SETTIMEFORMAT, 0, CAST(LPARAM, ADR(lp)));
END MCIWndSetTimeFormat;

PROCEDURE MCIWndGetTimeFormat(hwnd : HWND; VAR lp : ARRAY OF TCHAR; len : UINT) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETTIMEFORMAT, len,
                CAST(LPARAM, ADR(lp)));
END MCIWndGetTimeFormat;

PROCEDURE MCIWndValidateMedia(hwnd : HWND);
BEGIN
    FUNC SendMessage(hwnd, MCIWNDM_VALIDATEMEDIA, 0, 0);
END MCIWndValidateMedia;

PROCEDURE MCIWndSetRepeat(hwnd : HWND; f : BOOL);
BEGIN
     FUNC SendMessage(hwnd, MCIWNDM_SETREPEAT, 0, VAL(LPARAM, f));
END MCIWndSetRepeat;

PROCEDURE MCIWndGetRepeat(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, MCIWNDM_GETREPEAT, 0, 0));
END MCIWndGetRepeat;

PROCEDURE MCIWndUseFrames(hwnd : HWND) : LONG;
BEGIN
    RETURN MCIWndSetTimeFormat(hwnd, "frames");
END MCIWndUseFrames;

PROCEDURE MCIWndUseTime(hwnd : HWND) : LONG;
BEGIN
    RETURN MCIWndSetTimeFormat(hwnd, "ms");
END MCIWndUseTime;

PROCEDURE MCIWndSetActiveTimer(hwnd : HWND; active : UINT);
BEGIN
    FUNC SendMessage(hwnd, MCIWNDM_SETACTIVETIMER,
                        active, 0);
END MCIWndSetActiveTimer;

PROCEDURE MCIWndSetInactiveTimer(hwnd : HWND; inactive : UINT);
BEGIN
    FUNC SendMessage(hwnd, MCIWNDM_SETINACTIVETIMER, inactive, 0);
END MCIWndSetInactiveTimer;

PROCEDURE MCIWndSetTimers(hwnd : HWND; active, inactive : UINT);
BEGIN
    FUNC SendMessage(hwnd, MCIWNDM_SETTIMERS, active,
                    VAL(LPARAM, inactive));
END MCIWndSetTimers;

PROCEDURE MCIWndGetActiveTimer(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, MCIWNDM_GETACTIVETIMER, 0, 0));
END MCIWndGetActiveTimer;

PROCEDURE MCIWndGetInactiveTimer(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, MCIWNDM_GETINACTIVETIMER, 0, 0));
END MCIWndGetInactiveTimer;

PROCEDURE MCIWndRealize(hwnd : HWND; fBkgnd : BOOL) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_REALIZE, VAL(WPARAM, fBkgnd),0);
END MCIWndRealize;

PROCEDURE MCIWndSendString(hwnd : HWND; sz : ARRAY OF TCHAR) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_SENDSTRING, 0, CAST(LPARAM, ADR(sz)));
END MCIWndSendString;

<*/PUSH/HIGH*>
PROCEDURE MCIWndReturnString(hwnd : HWND; VAR lp : ARRAY OF TCHAR) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_RETURNSTRING, HIGH(lp),
                                 CAST(LPARAM, ADR(lp)));
END MCIWndReturnString;

PROCEDURE MCIWndGetError(hwnd : HWND; VAR lp : ARRAY OF TCHAR) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETERROR, HIGH(lp),
                                  CAST(LPARAM, ADR(lp)));
END MCIWndGetError;
<*/POP*>

PROCEDURE MCIWndGetPalette(hwnd : HWND ) : HPALETTE;
BEGIN
    RETURN CAST(HPALETTE, SendMessage(hwnd, MCIWNDM_GETPALETTE, 0, 0));
END MCIWndGetPalette;

PROCEDURE MCIWndSetPalette(hwnd : HWND; hpal : HPALETTE) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_SETPALETTE, CAST(WPARAM, hpal), 0);
END MCIWndSetPalette;

<*/PUSH/HIGH*>
PROCEDURE MCIWndGetFileName(hwnd : HWND; VAR lp : ARRAY OF TCHAR) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETFILENAME, HIGH(lp),
                                  CAST(LPARAM, ADR(lp)));
END MCIWndGetFileName;

PROCEDURE MCIWndGetDevice(hwnd : HWND; VAR lp : ARRAY OF TCHAR) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_GETDEVICE, HIGH(lp),
                                  CAST(LPARAM, ADR(lp)));
END MCIWndGetDevice;
<*/POP*>

PROCEDURE MCIWndGetStyles(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, MCIWNDM_GETSTYLES, 0, 0));
END MCIWndGetStyles;

PROCEDURE MCIWndChangeStyles(hwnd : HWND; mask : UINT; value : LONG) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_CHANGESTYLES, mask, value);
END MCIWndChangeStyles;

PROCEDURE MCIWndOpenInterface(hwnd : HWND; pUnk : IUnknown) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_OPENINTERFACE, 0,
                                 CAST(LPARAM, pUnk));
END MCIWndOpenInterface;

PROCEDURE MCIWndSetOwner(hwnd : HWND; hwndP : HWND) : LONG;
BEGIN
    RETURN SendMessage(hwnd, MCIWNDM_SETOWNER, CAST(WPARAM, hwndP), 0);
END MCIWndSetOwner;

PROCEDURE capSetCallbackOnError(hwnd : HWND; fpProc : CAPERRORCALLBACK) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_CALLBACK_ERROR, 0,
                                CAST(LPARAM, fpProc)));
END capSetCallbackOnError;

PROCEDURE capSetCallbackOnStatus(hwnd : HWND; fpProc : CAPSTATUSCALLBACK) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_CALLBACK_STATUS, 0,
                               CAST(LPARAM, fpProc)));
END capSetCallbackOnStatus;

PROCEDURE capSetCallbackOnYield(hwnd : HWND; fpProc : CAPYIELDCALLBACK) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_CALLBACK_YIELD, 0,
                                CAST(LPARAM, fpProc)));
END capSetCallbackOnYield;

PROCEDURE capSetCallbackOnFrame(hwnd : HWND; fpProc : LPVOID) : BOOL;
BEGIN
    RETURN CAST(BOOL, AVICapSM(hwnd, WM_CAP_SET_CALLBACK_FRAME, 0, CAST(LPARAM,fpProc)));
END capSetCallbackOnFrame;

PROCEDURE capSetCallbackOnVideoStream(hwnd : HWND; fpProc : CAPVIDEOCALLBACK) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_CALLBACK_VIDEOSTREAM, 0,
                               CAST(LPARAM, fpProc)));
END capSetCallbackOnVideoStream;

PROCEDURE capSetCallbackOnWaveStream(hwnd : HWND; fpProc : CAPWAVECALLBACK) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_CALLBACK_WAVESTREAM, 0,
                                CAST(LPARAM, fpProc)));
END capSetCallbackOnWaveStream;

PROCEDURE capSetCallbackOnCapControl(hwnd : HWND; fpProc : CAPCONTROLCALLBACK) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_CALLBACK_CAPCONTROL, 0,
                                CAST(LPARAM, fpProc)));
END capSetCallbackOnCapControl;

PROCEDURE capSetUserData(hwnd : HWND; lUser : LPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_USER_DATA, 0, lUser));
END capSetUserData;

PROCEDURE capGetUserData(hwnd : HWND) : UINT;
BEGIN
    RETURN SB_AVICapSM(hwnd, WM_CAP_GET_USER_DATA, 0, 0);
END capGetUserData;

PROCEDURE capDriverConnect(hwnd : HWND; i : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_DRIVER_CONNECT, i, 0));
END capDriverConnect;

PROCEDURE capDriverDisconnect(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_DRIVER_DISCONNECT, 0, 0));
END capDriverDisconnect;

<*/PUSH/HIGH*>
PROCEDURE capDriverGetName(hwnd : HWND; VAR szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_DRIVER_GET_NAME, HIGH(szName), CAST(LPARAM, ADR(szName))));
END capDriverGetName;

PROCEDURE capDriverGetVersion(hwnd : HWND; VAR szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_DRIVER_GET_VERSION, HIGH(szName), CAST(LPARAM, ADR(szName))));
END capDriverGetVersion;
<*/POP*>

PROCEDURE capDriverGetCaps(hwnd : HWND; s : LPCAPDRIVERCAPS; wSize : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_DRIVER_GET_CAPS, wSize, CAST(LPARAM, s)));
END capDriverGetCaps;

PROCEDURE capFileSetCaptureFile(hwnd : HWND; szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_FILE_SET_CAPTURE_FILE, 0, CAST(LPARAM, ADR(szName))));
END capFileSetCaptureFile;

<*/PUSH/HIGH*>
PROCEDURE capFileGetCaptureFile(hwnd : HWND; VAR szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_FILE_GET_CAPTURE_FILE,
                                HIGH(szName), CAST(LPARAM, ADR(szName))));
END capFileGetCaptureFile;
<*/POP*>

PROCEDURE capFileAlloc(hwnd : HWND; dwSize : DWORD) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_FILE_ALLOCATE, 0, VAL(LPARAM, dwSize)));
END capFileAlloc;

PROCEDURE capFileSaveAs(hwnd : HWND; szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_FILE_SAVEAS, 0,
                             CAST(LPARAM, ADR(szName))));
END capFileSaveAs;

PROCEDURE capFileSetInfoChunk(hwnd : HWND; lpInfoChunk : LPCAPINFOCHUNK) : BOOL;
BEGIN
    RETURN (CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_FILE_SET_INFOCHUNK, 0,
                                CAST(LPARAM, lpInfoChunk))));
END capFileSetInfoChunk;

PROCEDURE capFileSaveDIB(hwnd : HWND; szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_FILE_SAVEDIB, 0, CAST(LPARAM, ADR(szName))));
END capFileSaveDIB;

PROCEDURE capEditCopy(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_EDIT_COPY, 0, 0));
END capEditCopy;

PROCEDURE capSetAudioFormat(hwnd : HWND; s : LPWAVEFORMATEX; size : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_AUDIOFORMAT, size, CAST(LPARAM, s)));
END capSetAudioFormat;

PROCEDURE capGetAudioFormat(hwnd : HWND; s : LPWAVEFORMATEX; size : WPARAM) : DWORD;
BEGIN
    RETURN SB_AVICapSM(hwnd, WM_CAP_GET_AUDIOFORMAT, size,
                        CAST(LPARAM, s));
END capGetAudioFormat;

PROCEDURE capGetAudioFormatSize(hwnd : HWND) : DWORD;
BEGIN
    RETURN SB_AVICapSM(hwnd, WM_CAP_GET_AUDIOFORMAT, 0, 0);
END capGetAudioFormatSize;

PROCEDURE capDlgVideoFormat(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_DLG_VIDEOFORMAT, 0, 0));
END capDlgVideoFormat;

PROCEDURE capDlgVideoSource(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_DLG_VIDEOSOURCE, 0, 0));
END capDlgVideoSource;

PROCEDURE capDlgVideoDisplay(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_DLG_VIDEODISPLAY, 0, 0));
END capDlgVideoDisplay;

PROCEDURE capDlgVideoCompression(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_DLG_VIDEOCOMPRESSION, 0, 0));
END capDlgVideoCompression;

PROCEDURE capGetVideoFormat(hwnd : HWND; s : ADDRESS; size : WPARAM) : DWORD;
BEGIN
    RETURN SB_AVICapSM(hwnd, WM_CAP_GET_VIDEOFORMAT, size,
                                CAST(LPARAM, s));
END capGetVideoFormat;

PROCEDURE capGetVideoFormatSize(hwnd : HWND) : DWORD;
BEGIN
    RETURN SB_AVICapSM(hwnd, WM_CAP_GET_VIDEOFORMAT, 0, 0);
END capGetVideoFormatSize;

PROCEDURE capSetVideoFormat(hwnd : HWND; s : ADDRESS; size : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_VIDEOFORMAT, size,
                               CAST(LPARAM, s)));
END capSetVideoFormat;


PROCEDURE capPreview(hwnd : HWND; f : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_PREVIEW, VAL(WPARAM, f), 0));
END capPreview;

PROCEDURE capPreviewRate(hwnd : HWND; wMS : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_PREVIEWRATE, wMS, 0));
END capPreviewRate;

PROCEDURE capOverlay(hwnd : HWND; f : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_OVERLAY, VAL(WPARAM, f), 0));
END capOverlay;

PROCEDURE capPreviewScale(hwnd : HWND; f : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_SCALE, VAL(WPARAM, f), 0));
END capPreviewScale;

PROCEDURE capGetStatus(hwnd : HWND; s : LPCAPSTATUS; wSize : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_GET_STATUS, wSize,
                               CAST(LPARAM, s)));
END capGetStatus;

PROCEDURE capSetScrollPos(hwnd : HWND; lpP : LPPOINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_SCROLL, 0, CAST(LPARAM, lpP)));
END capSetScrollPos;

PROCEDURE capGrabFrame(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_GRAB_FRAME, 0, 0));
END capGrabFrame;

PROCEDURE capGrabFrameNoStop(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_GRAB_FRAME_NOSTOP, 0, 0));
END capGrabFrameNoStop;

PROCEDURE capCaptureSequence(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SEQUENCE, 0, 0));
END capCaptureSequence;

PROCEDURE capCaptureSequenceNoFile(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SEQUENCE_NOFILE, 0, 0));
END capCaptureSequenceNoFile;

PROCEDURE capCaptureStop(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_STOP, 0, 0));
END capCaptureStop;

PROCEDURE capCaptureAbort(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_ABORT, 0, 0));
END capCaptureAbort;

PROCEDURE capCaptureSingleFrameOpen(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SINGLE_FRAME_OPEN, 0, 0));
END capCaptureSingleFrameOpen;

PROCEDURE capCaptureSingleFrameClose(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SINGLE_FRAME_CLOSE, 0, 0));
END capCaptureSingleFrameClose;

PROCEDURE capCaptureSingleFrame(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SINGLE_FRAME, 0, 0));
END capCaptureSingleFrame;

PROCEDURE capCaptureGetSetup(hwnd : HWND; s : LPCAPTUREPARMS; wSize : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_GET_SEQUENCE_SETUP, wSize,
                               CAST(LPARAM, s)));
END capCaptureGetSetup;

PROCEDURE capCaptureSetSetup(hwnd : HWND; s : LPCAPTUREPARMS; wSize : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_SEQUENCE_SETUP,
                               wSize, CAST(LPARAM, s)));
END capCaptureSetSetup;

PROCEDURE capSetMCIDeviceName(hwnd : HWND; szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_SET_MCI_DEVICE, 0,
                               CAST(LPARAM, ADR(szName))));
END capSetMCIDeviceName;

<*/PUSH/HIGH*>
PROCEDURE capGetMCIDeviceName(hwnd : HWND; VAR szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_GET_MCI_DEVICE, HIGH(szName),
                               CAST(LPARAM, ADR(szName))));
END capGetMCIDeviceName;
<*/POP*>
PROCEDURE capPaletteOpen(hwnd : HWND; szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_PAL_OPEN, 0,
                               CAST(LPARAM, ADR(szName))));
END capPaletteOpen;

PROCEDURE capPaletteSave(hwnd : HWND; szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_PAL_SAVE, 0, CAST(LPARAM, ADR(szName))));
END capPaletteSave;

PROCEDURE capPalettePaste(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_PAL_PASTE, 0, 0));
END capPalettePaste;

PROCEDURE capPaletteAuto(hwnd : HWND; iFrames : WPARAM; iColors : DWORD) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_PAL_AUTOCREATE, iFrames,
                                VAL(LPARAM, iColors)));
END capPaletteAuto;

PROCEDURE capPaletteManual(hwnd : HWND; fGrab : WPARAM; iColors : DWORD) : BOOL;
BEGIN
    RETURN CAST(BOOL, SB_AVICapSM(hwnd, WM_CAP_PAL_MANUALCREATE, fGrab,
                               VAL(LPARAM, iColors)));
END capPaletteManual;

PROCEDURE ICDecompressEx(hic : HIC;
                         dwFlags : DWORD;
                         lpbiSrc : LPBITMAPINFOHEADER;
                         lpSrc : LPVOID;
                         xSrc : WINT;
                         ySrc : WINT;
                         dxSrc : WINT;
                         dySrc : WINT;
                         lpbiDst : LPBITMAPINFOHEADER;
                         lpDst : LPVOID;
                         xDst : WINT;
                         yDst : WINT;
                         dxDst : WINT;
                         dyDst : WINT) : LRESULT;
VAR
    ic          : ICDECOMPRESSEX;
BEGIN
    ic.dwFlags := dwFlags;
    ic.lpbiSrc := lpbiSrc;
    ic.lpSrc := lpSrc;
    ic.xSrc := xSrc;
    ic.ySrc := ySrc;
    ic.dxSrc := dxSrc;
    ic.dySrc := dySrc;
    ic.lpbiDst := lpbiDst;
    ic.lpDst := lpDst;
    ic.xDst := xDst;
    ic.yDst := yDst;
    ic.dxDst := dxDst;
    ic.dyDst := dyDst;

    RETURN ICSendMessage(hic, ICM_DECOMPRESSEX,
                         CAST(DWORD, ADR(ic)),
                         SIZE(ic));
END ICDecompressEx;

PROCEDURE ICDecompressExBegin(hic : HIC;
                         dwFlags : DWORD;
                         lpbiSrc : LPBITMAPINFOHEADER;
                         lpSrc : LPVOID;
                         xSrc : WINT;
                         ySrc : WINT;
                         dxSrc : WINT;
                         dySrc : WINT;
                         lpbiDst : LPBITMAPINFOHEADER;
                         lpDst : LPVOID;
                         xDst : WINT;
                         yDst : WINT;
                         dxDst : WINT;
                         dyDst : WINT) : LRESULT;
VAR
    ic  : ICDECOMPRESSEX;
BEGIN
    ic.dwFlags := dwFlags;
    ic.lpbiSrc := lpbiSrc;
    ic.lpSrc := lpSrc;
    ic.xSrc := xSrc;
    ic.ySrc := ySrc;
    ic.dxSrc := dxSrc;
    ic.dySrc := dySrc;
    ic.lpbiDst := lpbiDst;
    ic.lpDst := lpDst;
    ic.xDst := xDst;
    ic.yDst := yDst;
    ic.dxDst := dxDst;
    ic.dyDst := dyDst;

    RETURN ICSendMessage(hic, ICM_DECOMPRESSEX_BEGIN,
                        CAST(DWORD, ADR(ic)), SIZE(ic));
END ICDecompressExBegin;

PROCEDURE ICDecompressExQuery(hic : HIC;
                         dwFlags : DWORD;
                         lpbiSrc : LPBITMAPINFOHEADER;
                         lpSrc : LPVOID;
                         xSrc : WINT;
                         ySrc : WINT;
                         dxSrc : WINT;
                         dySrc : WINT;
                         lpbiDst : LPBITMAPINFOHEADER;
                         lpDst : LPVOID;
                         xDst : WINT;
                         yDst : WINT;
                         dxDst : WINT;
                         dyDst : WINT) : LRESULT;
VAR
    ic   : ICDECOMPRESSEX;
BEGIN
    ic.dwFlags := dwFlags;
    ic.lpbiSrc := lpbiSrc;
    ic.lpSrc := lpSrc;
    ic.xSrc := xSrc;
    ic.ySrc := ySrc;
    ic.dxSrc := dxSrc;
    ic.dySrc := dySrc;
    ic.lpbiDst := lpbiDst;
    ic.lpDst := lpDst;
    ic.xDst := xDst;
    ic.yDst := yDst;
    ic.dxDst := dxDst;
    ic.dyDst := dyDst;

    RETURN ICSendMessage(hic, ICM_DECOMPRESSEX_QUERY, CAST(DWORD, ADR(ic)),
                         SIZE(ic));
END ICDecompressExQuery;

PROCEDURE ICDecompressExEnd(hic : HIC) : LRESULT;
BEGIN
    RETURN ICSendMessage(hic, ICM_DECOMPRESSEX_END, 0, 0);
END ICDecompressExEnd;
END VFW.
