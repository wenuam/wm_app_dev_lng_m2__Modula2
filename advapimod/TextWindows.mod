(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 2009                                  *)
(*                        by ADW Software                                  *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE TextWindows;
<*/NOWARN:F*>
<*/NOPACK*>

<*/VALIDVER:ValidateData*>
(*<*/VER:ValidateData*>*)

FROM SYSTEM IMPORT
    ADDRESS, CAST, ADR, ADRCARD, IsThread;

FROM EXCEPTIONS IMPORT
    AllocateSource, ExceptionSource, RaiseRTL;

%IF ValidateData %THEN
FROM Strings IMPORT
    Append, Concat;
FROM Conversions IMPORT
    IntToStr;
%END

FROM MemUtils IMPORT
    FillMemWORD,
    %IF %NOT UNICODE %THEN
    FillMemBYTE,
    %END
    MoveMem;

IMPORT WinShell;

CONST
    TextWinDataIndex    = 31;

    MagicNumber         = 54455854h;

    (*
    Color = ColorTable{
               000 + (000 * 256) + (000 * 65536), (* black *)
               000 + (000 * 256) + (255 * 65536), (* blue *)
               000 + (128 * 256) + (000 * 65536), (* green *)
               000 + (160 * 256) + (160 * 65536), (* cyan *)
               160 + (000 * 256) + (000 * 65536), (* red *)
               128 + (000 * 256) + (255 * 65536), (* purple *)
               160 + (096 * 256) + (000 * 65536), (* brown *)
               128 + (128 * 256) + (128 * 65536), (* darkgray *)

               192 + (192 * 256) + (192 * 65536), (* lt gray *)
               000 + (128 * 256) + (255 * 65536), (* lt blue *)
               064 + (255 * 256) + (064 * 65536), (* lt green *)
               000 + (255 * 256) + (255 * 65536), (* lt cyan *)
               255 + (032 * 256) + (032 * 65536), (* lt red *)
               255 + (064 * 256) + (255 * 65536), (* magenta *)
               255 + (255 * 256) + (064 * 65536), (* yellow *)
               255 + (255 * 256) + (255 * 65536)  (* white *)
                      };*)

    Color = ColorTable{
               WinShell.Colors[WinShell.Black], (* black *)
               WinShell.Colors[WinShell.Blue], (* blue *)
               WinShell.Colors[WinShell.Green], (* green *)
               WinShell.Colors[WinShell.DarkCyan], (* cyan *)
               WinShell.Colors[WinShell.DarkRed], (* red *)
               WinShell.Colors[WinShell.Purple], (* purple *)
               WinShell.Colors[WinShell.Brown], (* brown *)
               WinShell.Colors[WinShell.DarkGray], (* darkgray *)

               WinShell.Colors[WinShell.LightGray], (* lt gray *)
               WinShell.Colors[WinShell.DeepSkyBlue], (* lt blue *)
               WinShell.Colors[WinShell.Lime], (* lt green *)
               WinShell.Colors[WinShell.Cyan], (* lt cyan *)
               WinShell.Colors[WinShell.Red], (* lt red *)
               WinShell.Colors[WinShell.Magenta], (* magenta *)
               WinShell.Colors[WinShell.Yellow], (* yellow *)
               WinShell.Colors[WinShell.White]  (* white *)
               };

    DefaultContextValues =
            WinShell.DrawContextValues{0,(*foreground*)
                                       0FFFFFFh,(*background*)
                                       NIL,(*font*)
                                       0,(*spacing*)
                                       WinShell.OriginTopLeft,
                                       1,
                                       WinShell.SolidLine,
                                       WinShell.JoinBevel,
                                       WinShell.EndCapFlat,
                                       WinShell.DrawFuncCopy};

TYPE
    ScreenType  = POINTER TO ARRAY OF ARRAY OF CHAR;
    AttrType    = POINTER TO ARRAY OF ARRAY OF ScreenAttribute;

    FontTypes   = (FtNormal, FtItalic, FtBold, FtBoldItalic);
    FontArray   = ARRAY FontTypes OF WinShell.FontHandle;

    TextWindow = POINTER TO TxtWnd;
    TxtWnd =
        RECORD
        validate        : CARDINAL;
        next            : TextWindow;
        w               : WinShell.Window;
        dc              : WinShell.DrawContext;
        draw            : WinShell.Drawable;
        wndProc         : TextWindowProcedure;
        (* Indicates whether painting is on(0) or off(>0) *)
        (* rect is the area to repaint when turned back on *)
        paintLock       : CARDINAL;
        painted         : BOOLEAN;
        lockRect        : twRECT;
        size            : twPOINT;
        backgroundColor : ScreenAttribute;
        attr            : WinAttrSet;
        (* vertical and horizontal, min and max scroll values *)
        hsmax           : COORDINATE;
        hsval           : COORDINATE;
        vsmax           : COORDINATE;
        vsval           : COORDINATE;
        (* number of positions to scroll on a page command *)
        pagev           : COORDINATE;
        pageh           : COORDINATE;
        (* User Coordinates of what is currently in our screen *)
        row             : COORDINATE;
        col             : COORDINATE;
        lines           : COORDINATE;
        cols            : COORDINATE;
        (* THESE COORDINATES are world relative *)
        vtop            : COORDINATE;   (* Visible top of screen    *)
        vbottom         : COORDINATE;   (* Visible bottom of screen *)
        vleft           : COORDINATE;   (* Visible left of screen   *)
        vright          : COORDINATE;   (* Visible right of screen  *)

        caretX          : COORDINATE;
        caretY          : COORDINATE;
        caretOn         : BOOLEAN;
        caretOffScreen  : BOOLEAN;
        caretType       : CaretTypes;

        lastMouse       : twPOINT;
        fonts           : FontArray;
        fontExtra       : ARRAY FontTypes OF COORDINATE;
        fontHeight      : ARRAY FontTypes OF COORDINATE;
        fontInfo        : FontInfo;
        cw              : COORDINATE;           (* CHARACTER width  *)
        ch              : COORDINATE;           (* CHARACTER height *)
        minX            : COORDINATE;
        minY            : COORDINATE;
        maxX            : COORDINATE;
        maxY            : COORDINATE;
        scrCh           : ScreenType;
        scrAttr         : AttrType;
        createParam     : ADDRESS;
        state           : WindowStateType;
        scrollRangeSet  : BOOLEAN;
        suppressMessage : BOOLEAN;
        suppressWndProc : BOOLEAN;
        fixedBuffer     : BOOLEAN;
        autoScroll      : BOOLEAN;
        snapToFont      : BOOLEAN;
        partialX        : BOOLEAN;
        partialY        : BOOLEAN;
        gutter          : BOOLEAN;
    END;

VAR
    FirstWindow         : TextWindow;
    TextWinExSrc        : ExceptionSource;

PROCEDURE ALLOCATE(VAR OUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    WinShell.AllocWinShellMem(addr, amount);
END ALLOCATE;

(*PROCEDURE ReALLOCATE(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    WinShell.ReallocWinShellMem(addr, amount);
END ReALLOCATE;*)

PROCEDURE DEALLOCATE(VAR INOUT addr : ADDRESS; amount : ADRCARD);
BEGIN
    WinShell.DeallocWinShellMem(addr, amount);
END DEALLOCATE;

PROCEDURE WinShellToTextWindowMessage(wmsg : WinShell.MessageRec;
                                      VAR OUT msg : TWMessageRec) : BOOLEAN;
VAR
    ok          : BOOLEAN;
BEGIN
    ok := TRUE;

    CASE wmsg.msg OF
    WinShell.WSM_GAINFOCUS:
        msg.msg := TWM_GAINFOCUS;
    |
    WinShell.WSM_LOSEFOCUS:
        msg.msg := TWM_LOSEFOCUS;
    |
    WinShell.WSM_ACTIVATEAPP:
        msg.msg := TWM_ACTIVATEAPP;
    |
    WinShell.WSM_DEACTIVATEAPP:
        msg.msg := TWM_DEACTIVATEAPP;
    |
    WinShell.WSM_KEY:
        msg.msg := TWM_KEY;
        msg.k_ch := wmsg.k_ch;
        msg.k_count := wmsg.k_count;
        msg.k_state := wmsg.k_state;
        msg.k_special := wmsg.k_special;
    |
    WinShell.WSM_CLOSE:
        msg.msg := TWM_CLOSE;
        msg.closeMode := wmsg.closeMode;
    |
    WinShell.WSM_POSITIONCHANGED:
        msg.msg := TWM_POSITIONCHANGED;
        msg.windowPos := wmsg.windowPos;
    |
    WinShell.WSM_MENU:
        msg.msg := TWM_MENU;
        msg.menuId := wmsg.menuId;
    |
    WinShell.WSM_MENUSELECT:
        msg.msg := TWM_MENUSELECT;
        msg.menuId := wmsg.menuId;
    |
    WinShell.WSM_MENUSTART:
        msg.msg := TWM_MENUSTART;
    |
    WinShell.WSM_MENUEND:
        msg.msg := TWM_MENUEND;
    |
    WinShell.WSM_TIMER:
        msg.msg := TWM_TIMER;
        msg.timerId := wmsg.timerId;
    |
    (*
    WinShell.WSM_HELP:
        msg.msg := TWM_HELP;
        msg.helpContext := wmsg.helpContext;
        msg.helpContextId := wmsg.helpContextId;
        msg.helpHelpId := wmsg.helpHelpId;
    |
    *)
    WinShell.WSM_USER:
        msg.msg := TWM_USER;
        msg.userId := wmsg.userId;
        msg.userData := wmsg.userData;
    ELSE
        ok := FALSE;
    END;
    RETURN ok;
END WinShellToTextWindowMessage;

PROCEDURE CallWndProc(tw : TextWindow; msg : TWMessageRec) : ResponseType;
BEGIN
    IF (tw <> NIL) AND (NOT tw^.suppressWndProc) THEN
        RETURN tw^.wndProc(tw, msg);
    END;
    RETURN DEFAULT_HANDLE;
END CallWndProc;

PROCEDURE ComputeTextSize(tw : TextWindow; VAR INOUT x, y : COORDINATE);
BEGIN
    IF tw^.gutter THEN
        x := x - tw^.cw;
        IF x < 0 THEN
            x := 0;
        END;
    END;

    tw^.partialX := (x REM tw^.cw) <> 0;
    tw^.partialY := (y REM tw^.ch) <> 0;

    x := x / tw^.cw;
    y := y / tw^.ch;
END ComputeTextSize;

PROCEDURE LoadFonts(tw : TextWindow; font : FontInfo) : BOOLEAN;
CONST
    weights     : ARRAY FontTypes OF FontWeights =
        {FwNormal, FwNormal, FwBold, FwBold};
    italic      : ARRAY FontTypes OF BOOLEAN =
        {FALSE, TRUE, FALSE, TRUE};
VAR
    lf          : FontInfo;
    fonts       : FontArray;
    ft          : FontTypes;
    bad         : BOOLEAN;
BEGIN
    lf := font;

    bad := FALSE;
    FOR ft := MIN(ft) TO MAX(ft) DO
        lf.weight := weights[ft];
        lf.italic := italic[ft];
        fonts[ft] := WinShell.LoadFont(lf);
        bad := bad OR (fonts[ft] = NIL);
    END;

    IF bad THEN
        IF fonts[FtNormal] <> NIL THEN
            (* as long as the "normal" exists, we will let the user use
               that font. obviously with dimished capacity.
            *)
            IF fonts[FtBoldItalic] = NIL THEN
                IF fonts[FtBold] <> NIL THEN
                    (* we have bold, so the italic busted *)
                    lf.weight := FwBold;
                    lf.italic := FALSE;
                    fonts[FtBoldItalic] := WinShell.LoadFont(lf);
                ELSIF fonts[FtItalic] <> NIL THEN
                    (* we have italic, so the bold busted *)
                    lf.weight := FwNormal;
                    lf.italic := TRUE;
                    fonts[FtBoldItalic] := WinShell.LoadFont(lf);
                ELSE
                    lf.weight := FwNormal;
                    lf.italic := FALSE;
                    fonts[FtBoldItalic] := WinShell.LoadFont(lf);
                END;
            END;
            IF fonts[FtBold] = NIL THEN
                lf.weight := FwNormal;
                lf.italic := FALSE;
                fonts[FtBold] := WinShell.LoadFont(lf);
            END;
            IF fonts[FtItalic] = NIL THEN
                lf.weight := FwNormal;
                lf.italic := FALSE;
                fonts[FtItalic] := WinShell.LoadFont(lf);
            END;

            (* just to be super safe, check that we have full fonts *)

            bad := FALSE;
            FOR ft := MIN(ft) TO MAX(ft) DO
                bad := bad OR (fonts[ft] = NIL);
            END;
        END;

        IF bad THEN
            FOR ft := MIN(ft) TO MAX(ft) DO
                IF fonts[ft] <> NIL THEN
                    WinShell.DeleteFont(fonts[ft]);
                END;
            END;
            RETURN FALSE;
        END;
    END;

    (* delete the old window fonts *)

    FOR ft := MIN(ft) TO MAX(ft) DO
        IF tw^.fonts[ft] <> NIL THEN
            WinShell.DeleteFont(tw^.fonts[ft]);
        END;
    END;

    (* install the new fonts *)

    tw^.fonts := fonts;
    tw^.fontInfo := font;
    GetFontSize(tw);

    RETURN TRUE;
END LoadFonts;

PROCEDURE CreateWindow(parent : WinShell.Window;
                       name : ARRAY OF CHAR;
                       menu : ARRAY OF CHAR;
                       icon : ARRAY OF CHAR;
                       x, y : COORDINATE;
                       xSize, ySize : COORDINATE;
                       xBuffer, yBuffer : COORDINATE;
                       gutter : BOOLEAN;
                       font : FontInfo;
                       background : ScreenAttribute;
                       windowType : WindowTypes;
                       wndProc : TextWindowProcedure;
                       attribs : WinAttrSet;
                       createParam : ADDRESS) : TextWindow;
VAR
    tw          : TextWindow;
    width       : COORDINATE;
    height      : COORDINATE;
    temp        : COORDINATE;
    umsg        : TWMessageRec;
    ft          : FontTypes;
    for, back   : Colors;
    style       : FontStyleSet;
BEGIN
    NEW(tw);
    IF tw = NIL THEN
        RETURN NIL;
    END;

    tw^.validate := MagicNumber;
    tw^.w := NIL;
    tw^.createParam := createParam;
    tw^.suppressMessage := TRUE;
    tw^.painted := FALSE;
    tw^.autoScroll := TRUE;
    tw^.snapToFont := TRUE;
    tw^.paintLock := 0;
    tw^.size.x := xBuffer;
    tw^.size.y := yBuffer;
    tw^.attr := attribs;
    tw^.col := 0;
    tw^.row := 0;
    tw^.vleft := 0;
    tw^.vright := 1;
    tw^.vtop := 0;
    tw^.vbottom := 1;
    tw^.hsmax := 0;
    tw^.hsval := 0;
    tw^.vsmax := 0;
    tw^.vsval := 0;
    tw^.pagev := 1;
    tw^.pageh := 1;
    tw^.lastMouse.x := -1;
    tw^.lastMouse.y := -1;
    tw^.backgroundColor := background;
    tw^.caretOn := FALSE;
    tw^.caretOffScreen := FALSE;
    tw^.caretX := 0;
    tw^.caretY := 0;
    tw^.gutter := gutter;
    tw^.wndProc := wndProc;
    tw^.suppressWndProc := FALSE;

    tw^.draw := NIL;
    tw^.dc := NIL;

    tw^.minX := 1;
    tw^.minY := 1;
    tw^.maxX := -1;
    tw^.maxY := -1;

    FOR ft := MIN(ft) TO MAX(ft) DO
        tw^.fonts[ft] := NIL;
    END;
    tw^.scrCh := NIL;
    tw^.scrAttr := NIL;

    IF xBuffer > 0 THEN
        IF xSize > xBuffer THEN
            xSize := xBuffer;
        END;
    END;
    IF yBuffer > 0 THEN
        IF ySize > yBuffer THEN
            ySize := yBuffer;
        END;
    END;

    tw^.fixedBuffer := FALSE;
    IF (xBuffer > 0) OR (yBuffer > 0) THEN
        tw^.fixedBuffer := TRUE;
        IF (xBuffer <= 0) OR (ySize <= 0) THEN
            DISPOSE(tw);
            RETURN NIL;
        END;
    END;

    tw^.fontInfo := font;
    IF NOT LoadFonts(tw, font) THEN
        tw^.fontInfo := DefaultFontInfo;
        IF NOT LoadFonts(tw, DefaultFontInfo) THEN
            DISPOSE(tw);
            RETURN NIL;
        END;
    END;

    (* these are only approximate pixel sizes as they do not include *)
    (* window extras *)
    (* we will be exact after the window creation *)
    (* this only helps the operating system better position the window *)
    (* when using default x, y but providing a specific initial size *)

    width := -1;
    IF xSize > 0 THEN
        width := xSize * tw^.cw;
        IF gutter THEN
            width := width + tw^.cw;
        END;
    END;
    height := -1;
    IF ySize > 0 THEN
        height := ySize * tw^.ch;
    END;

    tw^.w := WinShell.CreateWindow(parent,
                                   name,
                                   menu,
                                   icon,
                                   windowType,
                                   WinShell.DrawClient,
                                   TextWndProc,
                                   %IF Windows %THEN
                                   tw^.attr-WinAttrSet{WA_VISIBLE},
                                   %ELSE
                                   (* this is an adjustment to work around a bug in the
                                      KDE window manager (2.1).
                                      (
                                       if a window had BOTH vert and horiz scroll bars
                                       the KDE window manager barfed our window into
                                       a "popup" window when we hide and then show
                                       the window.
                                       note. this bug did not show up when running on
                                       an SMP system. only single processor.
                                      )
                                      in the X windows world it is best to not create
                                      a window hidden. A hidden window appears to not
                                      be a full fledged window.
                                   *)
                                   tw^.attr,
                                   %END
                                   x, y,
                                   width, height,
                                   NIL,
                                   tw);
    IF tw^.w = NIL THEN
        DISPOSE(tw);
        RETURN NIL;
    END;

    SetCaretType(tw, CtVerticalBar);

    IF tw^.dc = NIL THEN
        DISPOSE(tw);
        RETURN NIL;
    END;

    tw^.next := FirstWindow;
    FirstWindow := tw;

    WinShell.SetScrollBarRanges(tw^.w, 0, 100, 0, 0, 100, 0);
    WinShell.SetScrollDisableWhenNone(tw^.w, TRUE, TRUE);

    WinShell.SetWindowGrains(tw^.w, tw^.cw, tw^.ch);
    WinShell.SetMinClientSize(tw^.w, tw^.cw, tw^.ch);
    IF tw^.fixedBuffer THEN
        tw^.maxX := tw^.size.x;
        tw^.maxY := tw^.size.y;

        temp := tw^.maxX;
        IF gutter THEN
            INC(temp);
        END;
        WinShell.SetMaxClientSize(tw^.w, temp * tw^.cw, tw^.size.y * tw^.ch);
    END;

    WinShell.GetClientSize(tw^.w, width, height);
    ComputeTextSize(tw, width, height);

    IF (xSize > 0) OR (ySize > 0) THEN
        IF xSize > 0 THEN
            width := xSize;
        END;

        IF ySize > 0 THEN
            height := ySize;
        END;

        SetClientSize(tw, width, height);

        WinShell.GetClientSize(tw^.w, width, height);
        ComputeTextSize(tw, width, height);
    END;

    IF NOT tw^.fixedBuffer THEN
        tw^.size.x := width;
        tw^.size.y := height;
    END;

    IF (tw^.size.x = 0) OR (tw^.size.y = 0) THEN
        tw^.size.x := 1;
        tw^.size.y := 1;
    END;

    (*IF tw^.size.x <> 0 THEN*)
        NEW(tw^.scrCh, tw^.size.y-1, tw^.size.x-1);
        IF tw^.scrCh = NIL THEN
            WinShell.CloseWindow(tw^.w, CM_DICTATE);
            RETURN NIL;
        END;

        NEW(tw^.scrAttr, tw^.size.y-1, tw^.size.x-1);
        IF tw^.scrAttr = NIL THEN
            WinShell.CloseWindow(tw^.w, CM_DICTATE);
            RETURN NIL;
        END;

        %IF UNICODE %THEN
            FillMemWORD(ADR(tw^.scrCh^[0,0]), tw^.size.x * tw^.size.y, ' ');
        %ELSE
            FillMemBYTE(ADR(tw^.scrCh^[0,0]), tw^.size.x * tw^.size.y, ' ');
        %END
        FillMemWORD(ADR(tw^.scrAttr^[0,0]), tw^.size.x * tw^.size.y, background);

        IF width > tw^.size.x THEN
            width := tw^.size.x;
        END;

        IF height > tw^.size.y THEN
            height := tw^.size.y;
        END;

        tw^.vright := width - 1;
        tw^.vbottom := height - 1;

        tw^.lines := height;
        tw^.cols := width;

        tw^.hsmax := tw^.size.x;
        tw^.hsval := 0;
        tw^.pageh := tw^.size.x;
        tw^.vsmax := tw^.size.y;
        tw^.vsval := 0;
        tw^.pagev := tw^.size.y;

        WinShell.GetClientSize(tw^.w, width, height);
        tw^.draw := WinShell.BeginPaint(tw^.w, tw^.dc);
        DecomposeAttribute(tw^.backgroundColor, for, back, style);
        WinShell.SetBackgroundColor(tw^.dc, Color[back]);
        WinShell.EraseRectangle(tw^.draw, 0, 0, width, height);
        WinShell.EndPaint(tw^.w);
        tw^.draw := NIL;
    (*ELSE
        (* the parent window may be minimized, thus we have no size *)
        tw^.lines := 0;
        tw^.cols := 0;
        tw^.hsmax := 0;
        tw^.hsval := 0;
        tw^.pageh := 0;
        tw^.vsmax := 0;
        tw^.vsval := 0;
        tw^.pagev := 0;
    END;*)

    tw^.suppressMessage := FALSE;

    umsg.msg := TWM_CREATE;
    umsg.createParam := createParam;
    CallWndProc(tw, umsg);

    IF WA_VISIBLE IN attribs THEN
        SetDisplayMode(tw, DisplayVisible);
    END;

    umsg.msg := TWM_SIZE;
    umsg.width := tw^.cols;
    umsg.height := tw^.lines;
    CallWndProc(tw, umsg);

    IF tw^.cols <> 0 THEN
        IF windowType = ToplevelWindow THEN
            umsg.msg := TWM_POSITIONCHANGED;
            WinShell.GetWindowPos(tw^.w, umsg.windowPos.x, umsg.windowPos.y);
            CallWndProc(tw, umsg);
        END;

        umsg.msg := TWM_PAINT;
        umsg.paintRect.x1 := 0;
        umsg.paintRect.y1 := 0;
        umsg.paintRect.x2 := tw^.size.x - 1;
        umsg.paintRect.y2 := tw^.size.y - 1;
        PaintOff(tw);
        CallWndProc(tw, umsg);
        PaintOn(tw);
    END;

    RETURN tw;
END CreateWindow;

PROCEDURE UpdateSizeAfterSplit(tw : TextWindow);
VAR
    msg         : WinShell.MessageRec;
BEGIN
    msg.msg := WinShell.WSM_SIZE;
    WinShell.GetClientSize(tw^.w, msg.width, msg.height);
    WinShell.RedirectMessage(tw^.w, msg);
    WinShell.RepaintWindow(tw^.w);
END UpdateSizeAfterSplit;

PROCEDURE SplitWindow(tw : TextWindow;
                      wndProc : WinShell.WindowProcedure;
                      splitY : BOOLEAN;
                      VAR OUT splitter : WinShell.Window);
VAR
    newW        : WinShell.Window;
    save        : BOOLEAN;
BEGIN
    splitter := tw^.w;
    save := tw^.suppressMessage;
    tw^.suppressMessage := TRUE;

    WinShell.SetWindowData(tw^.w, TextWinDataIndex, NIL);
    WinShell.SplitWindow(tw^.w, wndProc, splitY, newW);

    tw^.suppressMessage := save;

    IF newW <> NIL THEN
        tw^.w := newW;
        WinShell.SetWindowData(tw^.w, TextWinDataIndex, tw);

        UpdateSizeAfterSplit(tw);
    ELSE
        splitter := NIL;
        WinShell.SetWindowData(tw^.w, TextWinDataIndex, tw);
    END;
END SplitWindow;

PROCEDURE UnsplitWindow(splitter : WinShell.Window; keepChild : TextWindow);
VAR
    save        : BOOLEAN;
BEGIN
    IF WinShell.GetClientType(splitter) = WinShell.SplitterClient THEN
        save := keepChild^.suppressMessage;
        keepChild^.suppressMessage := TRUE;
        WinShell.SetWindowData(keepChild^.w, TextWinDataIndex, NIL);

        WinShell.UnsplitWindow(splitter, keepChild^.w);

        keepChild^.suppressMessage := save;

        keepChild^.w := splitter;
        WinShell.SetWindowData(keepChild^.w, TextWinDataIndex, keepChild);

        UpdateSizeAfterSplit(keepChild);
    END;
END UnsplitWindow;

PROCEDURE ConvertTabChildToTopLevel(tw : TextWindow) : BOOLEAN;
BEGIN
    IF WinShell.GetWindowType(tw^.w) = ChildWindow THEN
        RETURN WinShell.ConvertTabChildToTopLevel(tw^.w);
    END;
    RETURN FALSE;
END ConvertTabChildToTopLevel;

PROCEDURE CycleActiveTabChild(tw : TextWindow; direction : INTEGER);
BEGIN
    WinShell.CycleActiveTabChild(tw^.w, direction);
END CycleActiveTabChild;

PROCEDURE DisposeWindow(VAR INOUT tw : TextWindow);
VAR
    prev, next  : TextWindow;
    ft          : FontTypes;
BEGIN
    next := FirstWindow;
    prev := NIL;
    WHILE (next <> NIL) AND (next <> tw) DO
        prev := next;
        next := next^.next;
    END;
    IF next <> NIL THEN
        next := tw^.next;
        IF prev = NIL THEN
            FirstWindow := next;
        ELSE
            prev^.next := next;
        END;
    END;

    FOR ft := MIN(ft) TO MAX(ft) DO
        IF tw^.fonts[ft] <> NIL THEN
            WinShell.DeleteFont(tw^.fonts[ft]);
        END;
    END;
    WinShell.DestroyDrawContext(tw^.dc);

    IF tw^.scrCh <> NIL THEN
        DISPOSE(tw^.scrCh);
    END;
    IF tw^.scrAttr <> NIL THEN
        DISPOSE(tw^.scrAttr);
    END;
    tw^.validate := 0;

    DISPOSE(tw);
END DisposeWindow;

PROCEDURE CloseWindow(tw : TextWindow; mode : CloseModes) : BOOLEAN;
BEGIN
    IF tw <> NIL THEN
        RETURN WinShell.CloseWindow(tw^.w, mode);
    END;
    RETURN TRUE;
END CloseWindow;

PROCEDURE IsTextWindow(tw : TextWindow) : BOOLEAN;
BEGIN
    IF (tw <> NIL) AND (tw^.validate = MagicNumber) THEN
        RETURN TRUE;
    END;
    RETURN FALSE;
END IsTextWindow;

PROCEDURE GetTextWindowType(tw : TextWindow) : WindowTypes;
BEGIN
    IF IsTextWindow(tw) THEN
        RETURN WinShell.GetWindowType(tw^.w);
    END;
    RETURN ToplevelWindow;
END GetTextWindowType;

PROCEDURE SetDisplayMode(tw : TextWindow; dispMode : DisplayModes);
BEGIN
    WinShell.SetDisplayMode(tw^.w, dispMode);
END SetDisplayMode;

PROCEDURE GetDisplayMode(tw : TextWindow) : DisplayModes;
BEGIN
    RETURN WinShell.GetDisplayMode(tw^.w);
END GetDisplayMode;

PROCEDURE SetWindowEnable(tw : TextWindow; enabled : BOOLEAN);
BEGIN
    WinShell.SetWindowEnable(tw^.w, enabled);
END SetWindowEnable;

PROCEDURE IsMinimized(tw : TextWindow) : BOOLEAN;
BEGIN
    RETURN WinShell.IsMinimized(tw^.w);
END IsMinimized;

PROCEDURE IsMaximized(tw : TextWindow) : BOOLEAN;
BEGIN
    RETURN WinShell.IsMaximized(tw^.w);
END IsMaximized;

PROCEDURE SetWindowBackground(tw : TextWindow;
                              background : ScreenAttribute);
BEGIN
    tw^.backgroundColor := background;
    RepaintVirtual(tw, tw^.col-1, tw^.row, tw^.col+tw^.cols, tw^.row+tw^.lines);
END SetWindowBackground;

PROCEDURE SetWindowTitle(tw : TextWindow; title : ARRAY OF CHAR);
BEGIN
    WinShell.SetWindowTitle(tw^.w, title);
END SetWindowTitle;

PROCEDURE GetWindowFont(tw : TextWindow; VAR OUT font : FontInfo);
BEGIN
    font := tw^.fontInfo;
END GetWindowFont;

PROCEDURE SetWindowFont(tw : TextWindow; font : FontInfo) : BOOLEAN;
VAR
    x, y        : COORDINATE;
    temp        : COORDINATE;
    msg         : WinShell.MessageRec;
BEGIN
    IF LoadFonts(tw, font) THEN
        WinShell.GetClientSize(tw^.w, x, y);

        IF tw^.fixedBuffer THEN
            ComputeTextSize(tw, x, y);

            IF x > tw^.size.x THEN
                x := tw^.size.x;
            END;

            IF y > tw^.size.y THEN
                y := tw^.size.y;
            END;

            tw^.vright := tw^.vleft + x - 1;
            tw^.vbottom := tw^.vtop + y - 1;

            tw^.lines := y;
            tw^.cols := x;

            IF tw^.snapToFont THEN
                WinShell.SetWindowGrains(tw^.w, tw^.cw, tw^.ch);
            END;
            WinShell.SetMinClientSize(tw^.w, tw^.minX * tw^.cw, tw^.minY * tw^.ch);
            WinShell.SetMaxClientSize(tw^.w, tw^.maxX * tw^.cw, tw^.maxY * tw^.ch);
            IF NOT WinShell.IsMinimized(tw^.w) THEN
                RepaintVirtual(tw, tw^.vleft, tw^.vtop, tw^.vright, tw^.vbottom);
            END;
        ELSE
            IF NOT WinShell.IsMinimized(tw^.w) THEN
                PaintOff(tw);

                msg.msg := WinShell.WSM_SIZE;
                msg.width := x;
                msg.height := y;
                VariableBufferSizeProc(tw, msg);

                RepaintScreen(tw);
                PaintOn(tw);
            END;

            temp := tw^.minX;
            IF tw^.gutter THEN
                INC(temp);
            END;
            WinShell.SetMinClientSize(tw^.w, temp * tw^.cw, tw^.minY * tw^.ch);
            IF tw^.snapToFont THEN
                WinShell.SetWindowGrains(tw^.w, tw^.cw, tw^.ch);
            END;
        END;

        IF tw^.caretOn THEN
            MakeCaretVisible(tw);
        END;
        RETURN TRUE;
    END;
    RETURN FALSE;
END SetWindowFont;

PROCEDURE SetScrollRangeAllowed(tw : TextWindow;
                                which : WinAttr;
                                range : ScrollRange);
BEGIN
    tw^.scrollRangeSet := TRUE;

    IF which = WA_HSCROLL THEN
        tw^.hsval := range;
        IF range < VAL(ScrollRange, tw^.size.x) THEN
            tw^.hsmax := tw^.size.x;
        ELSE
            tw^.hsmax := range;
        END;

        IF tw^.autoScroll THEN
            IF tw^.col+tw^.size.x > tw^.hsmax THEN
                ScrollVirtual(tw, DIR_LEFT, SCROLL_ABSOLUTE, tw^.hsmax - tw^.size.x, 0);
            END;
        END;

        IF range = tw^.cols THEN
            range := 0;
        END;

        WinShell.SetScrollBarRange(tw^.w, WA_HSCROLL, 0, range, tw^.cols);
    ELSE
        tw^.vsval := range;
        IF range < VAL(ScrollRange, tw^.size.y) THEN
            tw^.vsmax := tw^.size.y;
        ELSE
            tw^.vsmax := range;
        END;

        IF tw^.autoScroll THEN
            IF tw^.row+tw^.size.y > tw^.vsmax THEN
                ScrollVirtual(tw, DIR_UP, SCROLL_ABSOLUTE, tw^.vsmax - tw^.size.y, 0);
            END;
        END;
        IF range = tw^.lines THEN
            range := 0;
        END;

        WinShell.SetScrollBarRange(tw^.w, WA_VSCROLL, 0, range, tw^.lines);
    END;
END SetScrollRangeAllowed;

PROCEDURE SetScrollRangesAllowed(tw : TextWindow;
                                 rangeX, rangeY : ScrollRange);
BEGIN
    tw^.scrollRangeSet := TRUE;

    tw^.hsval := rangeX;
    IF rangeX < VAL(ScrollRange, tw^.size.x) THEN
        tw^.hsmax := tw^.size.x;
    ELSE
        tw^.hsmax := rangeX;
    END;

    tw^.vsval := rangeY;
    IF rangeY < VAL(ScrollRange, tw^.size.y) THEN
        tw^.vsmax := tw^.size.y;
    ELSE
        tw^.vsmax := rangeY;
    END;

    IF tw^.autoScroll THEN
        IF tw^.row+tw^.size.y > tw^.vsmax THEN
            ScrollVirtual(tw, DIR_UP, SCROLL_ABSOLUTE, tw^.vsmax - tw^.size.y, 0);
        END;
        IF tw^.col+tw^.size.x > tw^.hsmax THEN
            ScrollVirtual(tw, DIR_LEFT, SCROLL_ABSOLUTE, tw^.hsmax - tw^.size.x, 0);
        END;
    END;

    IF rangeX < tw^.cols THEN
        rangeX := 0;
    END;
    IF rangeY <= tw^.lines THEN
        rangeY := 0;
    END;

    WinShell.SetScrollBarRanges(tw^.w, 0, rangeX, tw^.cols, 0, rangeY, tw^.lines);
END SetScrollRangesAllowed;

PROCEDURE SetPageSize(tw : TextWindow; x, y : ScrollRange);
BEGIN
    tw^.pagev := y;
    IF tw^.pagev = 0 THEN
        tw^.pagev := 1;
    END;

    tw^.pageh := x;
    IF tw^.pageh = 0 THEN
        tw^.pageh := 1;
    END;
END SetPageSize;

PROCEDURE SetAutoScroll(tw : TextWindow; on : BOOLEAN);
BEGIN
    IF tw^.autoScroll AND NOT on THEN
        IF (tw^.vleft <> 0) OR (tw^.vtop <> 0) THEN
            PaintOff(tw);
            ScrollVirtual(tw, DIR_UP, SCROLL_EXTREME, 0, 0);
            ScrollVirtual(tw, DIR_LEFT, SCROLL_EXTREME, 0, 0);
            PaintOn(tw);
        END;
    END;
    tw^.autoScroll := on;
END SetAutoScroll;

PROCEDURE SetCaretType(tw : TextWindow; ct : CaretTypes);
BEGIN
    tw^.caretType := ct;
    WinShell.SetCaretType(tw^.w, ct, tw^.cw, tw^.ch);
END SetCaretType;

PROCEDURE SetScrollBarPos(tw : TextWindow;
                          which : WinAttr;
                          pos : ScrollRange);
BEGIN
    WinShell.SetScrollBarPos(tw^.w, which, pos);
END SetScrollBarPos;

PROCEDURE SetWindowData(tw : TextWindow;
                        index : CARDINAL;
                        data : ADDRESS) : BOOLEAN;
BEGIN
    IF index <> TextWinDataIndex THEN
        RETURN WinShell.SetWindowData(tw^.w, index, data);
    END;
    RETURN FALSE;
END SetWindowData;

PROCEDURE SetWindowDataNum(tw : TextWindow;
                           index : CARDINAL;
                           data : CARDINAL) : BOOLEAN;
BEGIN
    IF index <> TextWinDataIndex THEN
        RETURN WinShell.SetWindowDataNum(tw^.w, index, data);
    END;
    RETURN FALSE;
END SetWindowDataNum;

PROCEDURE GetWindowData(tw : TextWindow;
                        index : CARDINAL) : ADDRESS;
BEGIN
    RETURN WinShell.GetWindowData(tw^.w, index);
END GetWindowData;

PROCEDURE GetWindowDataNum(tw : TextWindow;
                           index : CARDINAL) : CARDINAL;
BEGIN
    RETURN WinShell.GetWindowDataNum(tw^.w, index);
END GetWindowDataNum;

PROCEDURE SetMinClientSize(tw : TextWindow; x, y : COORDINATE);
BEGIN
    IF x <= 0 THEN
        x := 1;
    ELSIF (tw^.fixedBuffer) AND (x > tw^.size.x) THEN
        x := tw^.size.x;
    END;
    tw^.minX := x;

    IF y <= 0 THEN
        y := 1;
    ELSIF (tw^.fixedBuffer) AND (y > tw^.size.y) THEN
        y := tw^.size.y;
    END;
    tw^.minY := y;

    IF tw^.gutter THEN
        INC(x);
    END;
    WinShell.SetMinClientSize(tw^.w, x * tw^.cw, y * tw^.ch);
END SetMinClientSize;

PROCEDURE GetClientSize(tw : TextWindow; VAR OUT x, y : COORDINATE);
BEGIN
    x := tw^.cols;
    y := tw^.lines;
END GetClientSize;

PROCEDURE SetClientSize(tw : TextWindow; x, y : COORDINATE);
BEGIN
    IF tw^.gutter THEN
        INC(x);
    END;
    WinShell.SetClientSize(tw^.w, x * tw^.cw, y * tw^.ch);
END SetClientSize;

PROCEDURE SnapWindowToFont(tw : TextWindow; on : BOOLEAN);
BEGIN
    IF tw^.snapToFont <> on THEN
        tw^.snapToFont := on;
        IF on THEN
            WinShell.SetWindowGrains(tw^.w, tw^.cw, tw^.ch);
        ELSE
            WinShell.SetWindowGrains(tw^.w, 1, 1);
        END;
    END;
END SnapWindowToFont;

PROCEDURE ResizeScreen(tw : TextWindow; xSize, ySize : COORDINATE) : BOOLEAN;
VAR
    x, y        : COORDINATE;
    temp        : COORDINATE;
    umsg        : TWMessageRec;
    saveScr     : ScreenType;
    saveAttr    : AttrType;
BEGIN
    saveScr := tw^.scrCh;
    NEW(tw^.scrCh, ySize - 1, xSize - 1);
    IF tw^.scrCh = NIL THEN
        tw^.scrCh := saveScr;
        RETURN FALSE;
    END;

    saveAttr := tw^.scrAttr;
    NEW(tw^.scrAttr, ySize - 1, xSize - 1);
    IF tw^.scrAttr = NIL THEN
        tw^.scrAttr := saveAttr;
        RETURN FALSE;
    END;

    DISPOSE(saveScr);
    DISPOSE(saveAttr);

    tw^.size.x := xSize;
    tw^.size.y := ySize;
    tw^.hsmax := xSize;
    tw^.vsmax := ySize;
    tw^.row := 0;
    tw^.col := 0;
    tw^.vtop := 0;
    tw^.vleft := 0;
    tw^.caretX := 0;
    tw^.caretY := 0;

    %IF UNICODE %THEN
        FillMemWORD(ADR(tw^.scrCh^[0,0]), xSize * ySize, ' ');
    %ELSE
        FillMemBYTE(ADR(tw^.scrCh^[0,0]), xSize * ySize, ' ');
    %END
    FillMemWORD(ADR(tw^.scrAttr^[0,0]), xSize * ySize, tw^.backgroundColor);

    WinShell.GetClientSize(tw^.w, x, y);
    ComputeTextSize(tw, x, y);

    IF x > tw^.size.x THEN
        x := tw^.size.x;
    END;

    IF y > tw^.size.y THEN
        y := tw^.size.y;
    END;

    tw^.vright := tw^.vleft + x - 1;
    tw^.vbottom := tw^.vtop + y - 1;

    tw^.lines := y;
    tw^.cols := x;

    SetScrollRangesAllowed(tw, tw^.hsval, tw^.vsval);

    temp := tw^.size.x;
    IF tw^.gutter THEN
        INC(temp);
    END;
    WinShell.SetMaxClientSize(tw^.w, temp * tw^.cw, tw^.size.y * tw^.ch);

    umsg.msg := TWM_SIZE;
    umsg.width := tw^.cols;
    umsg.height := tw^.lines;
    CallWndProc(tw, umsg);

    umsg.msg := TWM_PAINT;
    umsg.paintRect.x1 := 0;
    umsg.paintRect.y1 := 0;
    umsg.paintRect.x2 := tw^.size.x - 1;
    umsg.paintRect.y2 := tw^.size.y - 1;
    PaintOff(tw);
    CallWndProc(tw, umsg);
    PaintOn(tw);

    RETURN TRUE;
END ResizeScreen;

PROCEDURE RedirectMessage(tw : TextWindow; msg : TWMessageRec);
BEGIN
    IF IsTextWindow(tw) THEN
        CallWndProc(tw, msg);
    END;
END RedirectMessage;

PROCEDURE SendUserMessage(tw : TextWindow;
                          userId : CARDINAL; userData : ADDRESS);
BEGIN
    IF IsTextWindow(tw) THEN
        WinShell.SendUserMessage(tw^.w, userId, userData);
    END;
END SendUserMessage;

PROCEDURE PostUserMessage(tw : TextWindow;
                          userId : CARDINAL; userData : ADDRESS);
BEGIN
    IF IsTextWindow(tw) THEN
        WinShell.PostUserMessage(tw^.w, userId, userData);
    END;
END PostUserMessage;

PROCEDURE IsUserMessageWaiting(tw : TextWindow) : BOOLEAN;
VAR
    w   : WinShell.Window;
BEGIN
    IF tw = NIL THEN
        w := NIL;
    ELSE
        w := tw^.w;
    END;
    RETURN WinShell.IsUserMessageWaiting(w);
END IsUserMessageWaiting;

PROCEDURE ScrollScreen(tw : TextWindow; dir : ScrollDirection; n : COORDINATE);
VAR
    srcX, srcY, destX, destY    : COORDINATE;
    rx1, rx2, ry1, ry2          : COORDINATE;
    height, width               : COORDINATE;
    sizeX, sizeY                : COORDINATE;
    isClipped                   : BOOLEAN;
    draw                        : WinShell.Drawable;
BEGIN
    (* make sure any latent paint messages are handled before we scroll *)

    WinShell.UpdateWindow(tw^.w);

    draw := WinShell.BeginPaint(tw^.w, tw^.dc);

    WinShell.GetClientSize(tw^.w, sizeX, sizeY);

    (* leave the partial character diaplay areas alone *)

    sizeX := (sizeX / tw^.cw) * tw^.cw;
    sizeY := (sizeY / tw^.ch) * tw^.ch;

    (* determine if we are obsucured *)

    isClipped := WinShell.ClientAreaIsClipped(tw^.w);

    IF (dir = DIR_UP) OR (dir = DIR_DOWN) THEN
        width := sizeX;
        srcX := 0;
        destX := 0;
        rx1 := 0;
        rx2 := width / tw^.cw;
        height := sizeY - (n * tw^.ch);

        IF dir = DIR_UP THEN
            DEC(tw^.vtop, n);
            DEC(tw^.vbottom, n);
            srcY := 0;
            destY := tw^.ch * n;
            ry1 := 0;
            ry2 := n;
        ELSE
            INC(tw^.vtop, n);
            INC(tw^.vbottom, n);
            srcY := tw^.ch * n;
            destY := 0;
            ry2 := (sizeY / tw^.ch) - 1;
            ry1 := ry2 - n;
            IF ry1 < 0 THEN
                ry1 := 0;
            END;
        END;

        IF NOT isClipped THEN
            WinShell.CopyDrawableArea(draw, destX, destY, draw, srcX, srcY, width, height);
        ELSE
            rx1 := 0;
            ry1 := 0;
            rx2 := sizeX;
            ry2 := sizeY;
        END;

        IF (tw^.vtop < tw^.row) OR (tw^.vbottom >= tw^.row + tw^.size.y) THEN
            GetVLines(tw);
        END;
    ELSE
        height := sizeY;
        srcY := 0;
        destY := 0;
        ry1 := 0;
        ry2 := height / tw^.ch;
        width := sizeX - (n * tw^.cw);

        IF dir = DIR_LEFT THEN
            DEC(tw^.vleft, n);
            DEC(tw^.vright, n);
            srcX := 0;
            destX := tw^.cw * n;
            rx1 := 0;
            rx2 := n;
        ELSE
            IF tw^.gutter THEN
                (* it looks ugly if we bitblit and then wipe the gutter *)

                isClipped := TRUE;
            END;

            INC(tw^.vleft, n);
            INC(tw^.vright, n);
            srcX := tw^.cw * n;
            destX := 0;
            rx2 := (sizeX / tw^.cw) - 1;
            rx1 := rx2 - n;
            IF rx1 < 0 THEN
                rx1 := 0;
            END;
        END;

        IF NOT isClipped THEN
            WinShell.CopyDrawableArea(draw, destX, destY, draw, srcX, srcY, width, height);
        ELSE
            rx1 := 0;
            ry1 := 0;
            rx2 := sizeX;
            ry2 := sizeY;
        END;

        IF (tw^.vleft < tw^.col) OR (tw^.vright >= tw^.col+tw^.size.x) THEN
            GetHLines(tw);
        END;
    END;

    RepaintVirtual(tw, tw^.vleft + rx1, tw^.vtop + ry1, tw^.vleft + rx2, tw^.vtop + ry2);

    WinShell.EndPaint(tw^.w);
END ScrollScreen;

PROCEDURE ScrollVirtual(tw : TextWindow;
                        dir : ScrollDirection;
                        class  : ScrollClass;
                        pos : ScrollRange;
                        amount : COORDINATE) : ResponseType;
VAR
    delta       : COORDINATE;
    newPos      : COORDINATE;
    half        : COORDINATE;
    msg         : TWMessageRec;

    PROCEDURE scrollUP(lines : INTEGER);
    VAR
        disp    : COORDINATE;
    BEGIN
        IF (tw^.vtop-lines) < 0 THEN
            lines := tw^.vtop;
            IF lines < 0 THEN
                lines := 0;
            END;
        END;
        disp := tw^.vtop - lines;

        IF lines <> 0 THEN
            IF tw^.autoScroll THEN
                WinShell.SetScrollBarPos(tw^.w, WA_VSCROLL, disp);
            END;
            ScrollScreen(tw, DIR_UP, lines);
        END;
    END scrollUP;

    PROCEDURE scrollDOWN(lines : INTEGER);
    VAR
        disp    : COORDINATE;
    BEGIN
        IF (tw^.vbottom + lines) >= tw^.vsmax THEN
            lines := (tw^.vsmax - 1) - tw^.vbottom;
            IF lines < 0 THEN
                lines := 0;
            END;
        END;
        disp := tw^.vtop + lines;

        IF lines <> 0 THEN
            IF tw^.autoScroll THEN
                WinShell.SetScrollBarPos(tw^.w, WA_VSCROLL, disp);
            END;
            ScrollScreen(tw, DIR_DOWN, lines);
        END;
    END scrollDOWN;

    PROCEDURE scrollLEFT(lines : INTEGER);
    VAR
        disp    : COORDINATE;
    BEGIN
        IF (tw^.vleft - lines) < 0 THEN
            lines := tw^.vleft;
            IF lines < 0 THEN
                lines := 0;
            END;
        END;
        disp := tw^.vleft - lines;

        IF lines <> 0 THEN
            IF tw^.autoScroll THEN
                WinShell.SetScrollBarPos(tw^.w, WA_HSCROLL, disp);
            END;
            ScrollScreen(tw, DIR_LEFT, lines);
        END;
    END scrollLEFT;

    PROCEDURE scrollRIGHT(lines : INTEGER);
    VAR
        disp    : COORDINATE;
    BEGIN
        IF (tw^.vright + lines) >= tw^.hsmax THEN
            lines := (tw^.hsmax - 1) - tw^.vright;
            IF lines < 0 THEN
                lines := 0;
            END;
        END;
        disp := tw^.vleft + lines;

        IF lines <> 0 THEN
            IF tw^.autoScroll THEN
                WinShell.SetScrollBarPos(tw^.w, WA_HSCROLL, disp);
            END;
            ScrollScreen(tw, DIR_RIGHT, lines);
        END;
    END scrollRIGHT;

    PROCEDURE doNotify;
    BEGIN
        IF WindowInList(tw) AND tw^.autoScroll THEN
            msg.msg := TWM_NOTIFYSCROLL;
            CallWndProc(tw, msg);
        END;
    END doNotify;

BEGIN
    CASE class OF
    SCROLL_LINE:
        CASE dir OF
        DIR_UP:
            scrollUP(amount);
        |
        DIR_DOWN:
            scrollDOWN(amount);
        |
        DIR_RIGHT:
            scrollRIGHT(amount);
        |
        DIR_LEFT:
            scrollLEFT(amount);
        END;
        MoveCaretTo(tw, tw^.caretX, tw^.caretY);
        doNotify;
        RETURN USER_HANDLE;
    |
    SCROLL_PAGE:
        CASE dir OF
        DIR_UP:
            amount := tw^.pagev * amount;

            IF amount < (tw^.lines / 2) THEN
                scrollUP(amount);
                MoveCaretTo(tw, tw^.caretX, tw^.caretY);
                doNotify;
                RETURN USER_HANDLE;
            ELSIF tw^.vtop > 0 THEN
                newPos := tw^.vtop - amount;
                IF newPos < 0 THEN
                    newPos := 0;
                END;
            ELSE
                RETURN USER_HANDLE;
            END;
        |
        DIR_DOWN:
            amount := tw^.pagev * amount;

            IF amount < (tw^.lines / 2) THEN
                scrollDOWN(amount);
                MoveCaretTo(tw, tw^.caretX, tw^.caretY);
                doNotify;
                RETURN USER_HANDLE;
            ELSIF tw^.vtop < (tw^.vsmax - tw^.lines) THEN
                newPos := tw^.vtop + amount;
                IF newPos > (tw^.vsmax - tw^.lines) THEN
                    newPos := tw^.vsmax - tw^.lines;
                END;
            ELSE
                RETURN USER_HANDLE;
            END;
        |
        DIR_RIGHT:
            amount := tw^.pageh * amount;

            IF amount < (tw^.cols / 2) THEN
                scrollRIGHT(amount);
                MoveCaretTo(tw, tw^.caretX, tw^.caretY);
                doNotify;
                RETURN USER_HANDLE;
            ELSIF tw^.vleft < (tw^.hsmax - tw^.cols) THEN
                newPos := tw^.vleft + amount;
                IF newPos > (tw^.hsmax - tw^.cols) THEN
                    newPos := tw^.hsmax - tw^.cols;
                END;
            ELSE
                RETURN USER_HANDLE;
            END;
        |
        DIR_LEFT:
            amount := tw^.pageh * amount;

            IF amount < (tw^.cols / 2) THEN
                scrollLEFT(amount);
                MoveCaretTo(tw, tw^.caretX, tw^.caretY);
                doNotify;
                RETURN USER_HANDLE;
            ELSIF tw^.vleft > 0 THEN
                newPos := tw^.vleft - amount;
                IF newPos < 0 THEN
                    newPos := 0;
                END;
            ELSE
                RETURN USER_HANDLE;
            END;
        END;
    |
    SCROLL_ABSOLUTE:
        newPos := pos;
        IF newPos < 0 THEN
            newPos := 0;
        END;

        IF (dir = DIR_UP) OR (dir = DIR_DOWN) THEN
            IF newPos > (tw^.vsmax - tw^.lines) THEN
                newPos := tw^.vsmax - tw^.lines;
            END;
        ELSE
            IF newPos > (tw^.hsmax - tw^.cols) THEN
                newPos := tw^.hsmax - tw^.cols;
            END;
        END;
    |
    SCROLL_EXTREME:
        CASE dir OF
        DIR_UP, DIR_LEFT:
            newPos := 0;
        |
        DIR_DOWN:
            newPos := tw^.vsmax - tw^.lines;
        |
        DIR_RIGHT:
            newPos := tw^.hsmax - tw^.cols;
        END;
    END;

    IF (dir = DIR_UP) OR (dir = DIR_DOWN) THEN
        delta := tw^.vtop - newPos;
        IF delta <> 0 THEN
            half := tw^.lines / 2;
            IF (delta > 0) AND (delta <= half) THEN
                scrollUP(delta);
                MoveCaretTo(tw, tw^.caretX, tw^.caretY);
                doNotify;
                RETURN USER_HANDLE;
            ELSIF (delta < 0) AND (delta >= -half) THEN
                scrollDOWN(-delta);
                MoveCaretTo(tw, tw^.caretX, tw^.caretY);
                doNotify;
                RETURN USER_HANDLE;
            ELSE
                tw^.vtop := tw^.vtop - delta;
                tw^.vbottom := tw^.vbottom - delta;

                GetVLines(tw);
                IF tw^.autoScroll THEN
                    WinShell.SetScrollBarPos(tw^.w, WA_VSCROLL, newPos);
                END;
            END;
        ELSE
            RETURN USER_HANDLE;
        END;
    ELSE
        delta := tw^.vleft - newPos;
        IF delta <> 0 THEN
            half := tw^.cols / 2;
            IF (delta > 0) AND (delta <= half) THEN
                scrollLEFT(delta);
                MoveCaretTo(tw, tw^.caretX, tw^.caretY);
                doNotify;
                RETURN USER_HANDLE;
            ELSIF (delta < 0) AND (delta >= -half) THEN
                scrollRIGHT(-delta);
                MoveCaretTo(tw, tw^.caretX, tw^.caretY);
                doNotify;
                RETURN USER_HANDLE;
            ELSE
                tw^.vleft := tw^.vleft - delta;
                tw^.vright := tw^.vright - delta;

                GetHLines(tw);

                IF tw^.autoScroll THEN
                    WinShell.SetScrollBarPos(tw^.w, WA_HSCROLL, newPos);
                END;
            END;
        ELSE
            RETURN USER_HANDLE;
        END;
    END;

    RepaintVirtual(tw, tw^.vleft, tw^.vtop, tw^.vright, tw^.vbottom);

    MoveCaretTo(tw, tw^.caretX, tw^.caretY);

    doNotify;

    RETURN USER_HANDLE;
END ScrollVirtual;

PROCEDURE ScrollWindow(tw : TextWindow;
                       dir : ScrollDirection;
                       class  : ScrollClass;
                       pos : ScrollRange;
                       amount : COORDINATE);
BEGIN
    ScrollVirtual(tw, dir, class, pos, amount);
END ScrollWindow;

PROCEDURE ScrollBuffer(tw : TextWindow;
                       dir : ScrollDirection;
                       amount : ADRCARD);
VAR
    bytes       : CARDINAL;
    i           : ADRCARD;
    j           : ADRCARD;
BEGIN
    IF dir = DIR_UP THEN
        bytes := (tw^.size.x * tw^.size.y) - tw^.size.x;

        FOR i := 1 TO amount DO
            MoveMem(ADR(tw^.scrCh^[0,0]), ADR(tw^.scrCh^[1,0]), bytes);
            MoveMem(ADR(tw^.scrAttr^[0,0]), ADR(tw^.scrAttr^[1,0]), bytes*2);

            %IF UNICODE %THEN
                FillMemWORD(ADR(tw^.scrCh^[tw^.size.y-1, 0]), tw^.size.x, ' ');
            %ELSE
                FillMemBYTE(ADR(tw^.scrCh^[tw^.size.y-1, 0]), tw^.size.x, ' ');
            %END
            FillMemWORD(ADR(tw^.scrAttr^[tw^.size.y-1, 0]),
                        tw^.size.x, tw^.backgroundColor);
        END;

    ELSIF dir = DIR_DOWN THEN
        bytes := (tw^.size.x * tw^.size.y) - tw^.size.x;

        FOR i := 1 TO amount DO
            MoveMem(ADR(tw^.scrCh^[1,0]), ADR(tw^.scrCh^[0,0]), bytes);
            MoveMem(ADR(tw^.scrAttr^[1,0]), ADR(tw^.scrAttr^[0,0]), bytes*2);

            %IF UNICODE %THEN
                FillMemWORD(ADR(tw^.scrCh^[0, 0]), tw^.size.x, ' ');
            %ELSE
                FillMemBYTE(ADR(tw^.scrCh^[0, 0]), tw^.size.x, ' ');
            %END
            FillMemWORD(ADR(tw^.scrAttr^[0, 0]), tw^.size.x, tw^.backgroundColor);
        END;
    ELSIF dir = DIR_LEFT THEN
        bytes := tw^.size.x-1;
        FOR i := 1 TO amount DO
            FOR j := 0 TO VAL(ADRCARD, tw^.size.y)-1 DO
                MoveMem(ADR(tw^.scrCh^[j,0]), ADR(tw^.scrCh^[j,1]), bytes);
                MoveMem(ADR(tw^.scrAttr^[j,0]), ADR(tw^.scrAttr^[j,1]), bytes*2);

                tw^.scrCh^[j, tw^.size.x-1] := ' ';
                tw^.scrAttr^[j, tw^.size.x-1] := tw^.backgroundColor;
            END;
        END;
    ELSIF dir = DIR_RIGHT THEN
        bytes := tw^.size.x-1;
        FOR i := 1 TO amount DO
            FOR j := 0 TO VAL(ADRCARD, tw^.size.y)-1 DO
                MoveMem(ADR(tw^.scrCh^[j,1]), ADR(tw^.scrCh^[j,0]), bytes);
                MoveMem(ADR(tw^.scrAttr^[j,1]), ADR(tw^.scrAttr^[j,0]), bytes*2);

                tw^.scrCh^[j, 0] := ' ';
                tw^.scrAttr^[j, 0] := tw^.backgroundColor;
            END;
        END;
    END;

    RepaintVirtual(tw, tw^.vleft, tw^.vtop, tw^.vright, tw^.vbottom);
END ScrollBuffer;

PROCEDURE CaretOn(tw : TextWindow);
BEGIN
    IF NOT tw^.caretOn THEN
        tw^.caretOn := TRUE;
        WinShell.CaretOn(tw^.w);
        MoveCaretTo(tw, tw^.caretX, tw^.caretY);
    END;
END CaretOn;

PROCEDURE CaretOff(tw : TextWindow);
BEGIN
    IF tw^.caretOn THEN
        WinShell.CaretOff(tw^.w);
        tw^.caretOn := FALSE;
    END;
END CaretOff;

PROCEDURE ShowCaret(tw : TextWindow);
BEGIN
    IF tw^.caretOn THEN
        WinShell.ShowCaret(tw^.w);
    END;
END ShowCaret;

PROCEDURE HideCaret(tw : TextWindow);
BEGIN
    IF tw^.caretOn THEN
        WinShell.HideCaret(tw^.w);
    END;
END HideCaret;

PROCEDURE MoveCaretTo(tw : TextWindow; x, y : COORDINATE);
BEGIN
    tw^.caretX := x;
    tw^.caretY := y;

    IF tw^.caretOn THEN
        IF (x >= tw^.vleft) AND
           (x <= tw^.vright) AND
           (y >= tw^.vtop) AND
           (y <= tw^.vbottom)
        THEN
            x := (x - tw^.vleft) * tw^.cw;
            y := (y - tw^.vtop) * tw^.ch;

            IF tw^.gutter THEN
                x := x + tw^.cw;
            END;

            IF tw^.caretOffScreen THEN
                WinShell.ShowCaret(tw^.w);
            END;
            WinShell.MoveCaretTo(tw^.w, x, y);
        ELSE
            IF NOT tw^.caretOffScreen THEN
                tw^.caretOffScreen := TRUE;
                WinShell.HideCaret(tw^.w);
            END;
        END;
    END;
END MoveCaretTo;

PROCEDURE GetCaretPos(tw : TextWindow; VAR OUT x, y : COORDINATE);
BEGIN
    x := tw^.caretX;
    y := tw^.caretY;
END GetCaretPos;

PROCEDURE IsCaretVisible(tw : TextWindow) : BOOLEAN;
VAR
    rect        : twRECT;
BEGIN
    rect.x1 := tw^.caretX;
    rect.y1 := tw^.caretY;
    rect.x2 := rect.x1;
    rect.y2 := rect.y1;
    RETURN IsRectVisible(tw, rect);
END IsCaretVisible;

PROCEDURE MakeCaretVisible(tw : TextWindow);
VAR
    rect        : twRECT;
BEGIN
    rect.x1 := tw^.caretX;
    rect.y1 := tw^.caretY;
    rect.x2 := rect.x1;
    rect.y2 := rect.y1;
    MakeRectVisible(tw, rect);
END MakeCaretVisible;

PROCEDURE ComposeAttribute(fore, back : Colors;
                           style : FontStyleSet) : ScreenAttribute;
VAR
    attr        : CARDINAL;
    font        : CARDINAL;
BEGIN
    attr := ORD(back) BOR (ORD(fore) SHL 4);
    font := CAST(CARDINAL8, style);
    attr := attr BOR (font SHL 12);
    RETURN attr;
END ComposeAttribute;

PROCEDURE DecomposeAttribute(sc : ScreenAttribute;
                             VAR OUT fore, back : Colors;
                             VAR OUT style : FontStyleSet);
VAR
    font        : CARDINAL8;
BEGIN
    back := VAL(Colors, sc BAND 0Fh);
    fore := VAL(Colors, (sc SHR 4) BAND 0Fh);
    font := sc SHR 12;
    style := CAST(FontStyleSet, font);
END DecomposeAttribute;

PROCEDURE GetFontType(style : FontStyleSet) : FontTypes;
BEGIN
    IF style = NormalFont THEN
        RETURN FtNormal;
    ELSIF style = ItalicFont THEN
        RETURN FtItalic;
    ELSIF style = BoldFont THEN
        RETURN FtBold;
    ELSIF style = BoldFont+ItalicFont THEN
        RETURN FtBoldItalic;
    END;
    RETURN FtNormal;
END GetFontType;

PROCEDURE PutStringAt(tw : TextWindow;
                      x, y : COORDINATE;
                      str : ARRAY OF CHAR;
                      a : ScreenAttribute);
VAR
    len         : COORDINATE;
    start       : COORDINATE;
    i           : COORDINATE;
BEGIN
    (* convert to screen coordinates *)

    x := x - tw^.col;
    y := y - tw^.row;

    len := LENGTH(str);
    IF (len <> 0) AND
       (y >= 0) AND
       (y < tw^.size.y) AND
       (x < tw^.size.x)
    THEN
        start := 0;
        IF x < 0 THEN
            start := -x;
            len := len - start;
            x := 0;
        END;

        IF (x + len) > tw^.size.x THEN
            len := tw^.size.x - x;
        END;

        IF len > 0 THEN

            FOR i := 0 TO (len-1) DO
                tw^.scrCh^[y, x + i] := str[start+i];
                tw^.scrAttr^[y, x + i] := a;
            END;

            RepaintPhysical(tw, x, y, x + len-1, y);
        END;
    END;
END PutStringAt;

PROCEDURE PutAttrAt(tw : TextWindow;
                    x, y : COORDINATE;
                    attr : ARRAY OF ScreenAttribute);
VAR
    len         : COORDINATE;
    start       : COORDINATE;
    i           : COORDINATE;
BEGIN
    (* convert to screen coordinates *)

    x := x - tw^.col;
    y := y - tw^.row;

    IF (y >= 0) AND (y < tw^.size.y) AND (x < tw^.size.x) THEN
        len := HIGH(attr) + 1;
        start := 0;
        IF x < 0 THEN
            start := -x;
            len := len - start;
            x := 0;
        END;

        IF (x + len) > tw^.size.x THEN
            len := tw^.size.x - x;
        END;

        IF len > 0 THEN

            FOR i := 0 TO (len-1) DO
                tw^.scrAttr^[y, x + i] := attr[start+i];
            END;

            RepaintPhysical(tw, x, y, x + len-1, y);
        END;
    END;
END PutAttrAt;

PROCEDURE WriteStringAt(tw : TextWindow;
                        x, y : COORDINATE;
                        str : ARRAY OF CHAR;
                        a : ScreenAttribute);
VAR
    len         : COORDINATE;
    i           : COORDINATE;
    start       : COORDINATE;
    newX        : COORDINATE;
    newY        : COORDINATE;
BEGIN
    len := LENGTH(str);

    newX := x + len;
    newY := y;

    (* convert to screen coordinates *)

    x := x - tw^.col;
    y := y - tw^.row;

    IF (len > 0) AND
       (y >= 0) AND
       (y < tw^.size.y) AND
       (x < tw^.size.x)
    THEN
        start := 0;
        IF x < 0 THEN
            start := -x;
            len := len - start;
            x := 0;
        END;

        IF (x + len) > tw^.size.x THEN
            len := tw^.size.x - x;
        END;

        IF len > 0 THEN

            FOR i := 0 TO (len-1) DO
                tw^.scrAttr^[y, x + i] := a;
                tw^.scrCh^[y, x + i] := str[start+i];
            END;

            RepaintPhysical(tw, x, y, x + len-1, y);
        END;
    END;

    MoveCaretTo(tw, newX, newY);
END WriteStringAt;

PROCEDURE WriteString(tw : TextWindow;
                      str : ARRAY OF CHAR;
                      a : ScreenAttribute);
BEGIN
    WriteStringAt(tw, tw^.caretX, tw^.caretY, str, a);
END WriteString;

PROCEDURE WriteCellsAt(tw : TextWindow;
                       x, y : COORDINATE;
                       cells : ARRAY OF Cell);
VAR
    i           : COORDINATE;
    len         : COORDINATE;
    start       : COORDINATE;
BEGIN
    (* convert to screen coordinates *)

    x := x - tw^.col;
    y := y - tw^.row;

    IF (y >= 0) AND (y < tw^.size.y) AND (x < tw^.size.x) THEN
        len := HIGH(cells)+1;
        start := 0;
        IF x < 0 THEN
            start := -x;
            len := len - start;
            x := 0;
        END;

        IF (x + len) > tw^.size.x THEN
            len := tw^.size.x - x;
        END;

        IF len > 0 THEN
            FOR i := 0 TO len-1 DO
                tw^.scrCh^[y, x+i] := cells[start+i].ch;
                tw^.scrAttr^[y, x+i] := cells[start+i].attr;
            END;

            RepaintPhysical(tw, x, y, x + len-1, y);
        END;
    END;
END WriteCellsAt;

PROCEDURE WriteCells(tw : TextWindow; cells : ARRAY OF Cell);
BEGIN
    WriteCellsAt(tw, tw^.caretX, tw^.caretY, cells);
END WriteCells;

PROCEDURE WriteLn(tw : TextWindow);
VAR
    x           : COORDINATE;
    y           : COORDINATE;
BEGIN
    (* convert to screen coordinates *)

    x := tw^.caretX - tw^.col;
    y := tw^.caretY - tw^.row;

    IF (y >= 0) AND
       (y < tw^.size.y) AND
       (x < tw^.size.x) AND
       (y+1 = tw^.size.y)
    THEN
        ScrollVirtual(tw, DIR_DOWN, SCROLL_LINE, 0, 1);
    END;
    MoveCaretTo(tw, tw^.col, tw^.caretY + 1);
END WriteLn;

PROCEDURE EraseToEOL(tw : TextWindow; a : ScreenAttribute);
VAR
    bytes       : COORDINATE;
    x           : COORDINATE;
    y           : COORDINATE;
BEGIN
    (* convert to screen coordinates *)

    x := tw^.caretX - tw^.col;
    y := tw^.caretY - tw^.row;

    IF (y >= 0) AND
       (y < tw^.size.y) AND
       (x < tw^.size.x)
    THEN
        IF x < 0 THEN
            x := 0;
        END;

        bytes := tw^.size.x - x;
        IF bytes > 0 THEN
            %IF UNICODE %THEN
                FillMemWORD(ADR(tw^.scrCh^[y, x]), bytes, ' ');
            %ELSE
                FillMemBYTE(ADR(tw^.scrCh^[y, x]), bytes, ' ');
            %END
            FillMemWORD(ADR(tw^.scrAttr^[y, x]), bytes, a);
            RepaintPhysical(tw, x, y, (x + bytes)-1, y);
        END;
    END;

    MoveCaretTo(tw, tw^.col+tw^.size.x, tw^.caretY);
END EraseToEOL;

PROCEDURE ChangeAttr(tw : TextWindow; y, x1, x2 : COORDINATE; a :ScreenAttribute);
BEGIN
    (* convert to screen coordinates *)

    x1 := x1 - tw^.col;
    x2 := x2 - tw^.col;
    y := y - tw^.row;

    IF (x2 >= x1) AND
       (x2 >= 0) AND
       (y >= 0) AND
       (y < tw^.size.y) AND
       (x1 < tw^.size.x)
    THEN
        IF x1 < 0 THEN
            x1 := 0;
        END;
        IF x2 >= tw^.size.x THEN
            x2 := tw^.size.x-1;
        END;

        FillMemWORD(ADR(tw^.scrAttr^[y, x1]), (x2 - x1) + 1, a);
        RepaintPhysical(tw, x1, y, x2, y);
    END;
END ChangeAttr;

PROCEDURE ReadBufferString(tw : TextWindow;
                           y, x1, x2 : COORDINATE;
                           VAR OUT str : ARRAY OF CHAR);
BEGIN
    (* convert to screen coordinates *)

    x1 := x1 - tw^.col;
    x2 := x2 - tw^.col;
    y := y - tw^.row;

    IF (x2 >= x1) AND
       (x2 >= 0) AND
       (y >= 0) AND
       (y < tw^.size.y) AND
       (x1 < tw^.size.x)
    THEN
        IF x1 < 0 THEN
            x1 := 0;
        END;
        IF x2 >= tw^.size.x THEN
            x2 := tw^.size.x-1;
        END;

        str := tw^.scrCh^[y, x1..x2];
    END;
END ReadBufferString;

PROCEDURE RepaintRect(tw : TextWindow; rect : twRECT);
VAR
    msg         : TWMessageRec;
BEGIN
    msg.msg := TWM_PAINT;
    msg.paintRect := rect;
    PaintOff(tw);
    CallWndProc(tw, msg);
    PaintOn(tw);
END RepaintRect;

PROCEDURE RepaintScreen(tw : TextWindow);
VAR
    rect        : twRECT;
BEGIN
    rect.x1 := tw^.col;
    rect.y1 := tw^.row;
    rect.x2 := tw^.col+tw^.size.x-1;
    rect.y2 := tw^.row+tw^.size.y-1;
    RepaintRect(tw, rect);
END RepaintScreen;

PROCEDURE MakeRowVisible(tw : TextWindow; y : COORDINATE);
VAR
    pos         : COORDINATE;
BEGIN
    pos := 0;
    IF tw^.partialY THEN
        pos := 1;
    END;

    IF y < tw^.vtop THEN
        ScrollVirtual(tw, DIR_UP, SCROLL_ABSOLUTE, y, 0);
    ELSIF y > tw^.vbottom-pos THEN
        pos := tw^.vtop + (y - (tw^.vbottom - pos));
        ScrollVirtual(tw, DIR_DOWN, SCROLL_ABSOLUTE, pos, 0);
    END;
END MakeRowVisible;

PROCEDURE IsRectVisible(tw : TextWindow; theRect : twRECT) : BOOLEAN;
VAR
    fudgeX, fudgeY      : COORDINATE;
BEGIN
    fudgeX := 0;
    IF tw^.partialX THEN
        fudgeX := 1;
    END;
    fudgeY := 0;
    IF tw^.partialY THEN
        fudgeY := 1;
    END;

    IF (theRect.x1 < tw^.vleft) OR
       (theRect.x2 > tw^.vright-fudgeX) OR
       (theRect.y1 < tw^.vtop) OR
       (theRect.y2 > tw^.vbottom-fudgeY)
    THEN
        RETURN FALSE;
    END;
    RETURN TRUE;
END IsRectVisible;

PROCEDURE MakeRectVisible(tw : TextWindow; theRect : twRECT);
VAR
    off         : BOOLEAN;
    pos         : COORDINATE;
BEGIN
    off := FALSE;

    IF theRect.x1 < tw^.vleft THEN
        PaintOff(tw);
        off := TRUE;
        ScrollVirtual(tw, DIR_LEFT, SCROLL_ABSOLUTE, theRect.x1, 0);
    ELSE
        IF theRect.x2 > tw^.vright THEN
            PaintOff(tw);
            off := TRUE;
            pos := tw^.vleft + (theRect.x2 - tw^.vright);
            ScrollVirtual(tw, DIR_RIGHT, SCROLL_ABSOLUTE, pos, 0);
        END;
    END;

    IF theRect.y1 < tw^.vtop THEN
        IF NOT off THEN
            PaintOff(tw);
            off := TRUE;
        END;
        ScrollVirtual(tw, DIR_UP, SCROLL_ABSOLUTE, theRect.y1, 0);
    ELSE
        IF theRect.y2 > tw^.vbottom THEN
            IF NOT off THEN
                PaintOff(tw);
                off := TRUE;
            END;
            pos := tw^.vtop + (theRect.y2 - tw^.vbottom);
            ScrollVirtual(tw, DIR_DOWN, SCROLL_ABSOLUTE, pos, 0);
        END;
    END;

    IF off THEN
        PaintOn(tw);
    END;
END MakeRectVisible;

PROCEDURE GetVisibleRect(tw : TextWindow; VAR OUT theRect : twRECT);
BEGIN
    theRect.x1 := tw^.vleft;
    theRect.y1 := tw^.vtop;
    theRect.x2 := tw^.vright;
    theRect.y2 := tw^.vbottom;
    (*
    IF tw^.partialX THEN
        DEC(theRect.x2);
    END;
    IF tw^.partialY THEN
        DEC(theRect.y2);
    END;
    *)
END GetVisibleRect;

PROCEDURE GetBufferRect(tw : TextWindow; VAR OUT theRect : twRECT);
BEGIN
    theRect.x1 := tw^.col;
    theRect.y1 := tw^.row;
    theRect.x2 := theRect.x1 + tw^.size.x -1;
    theRect.y2 := theRect.y1 + tw^.size.y -1;
END GetBufferRect;

PROCEDURE EraseScreen(tw : TextWindow; a : ScreenAttribute);
VAR
    bytes       : INTEGER;
BEGIN
    bytes := tw^.size.x * tw^.size.y;
    %IF UNICODE %THEN
        FillMemWORD(ADR(tw^.scrCh^[0,0]), bytes, ' ');
    %ELSE
        FillMemBYTE(ADR(tw^.scrCh^[0,0]), bytes, ' ');
    %END
    FillMemWORD(ADR(tw^.scrAttr^[0,0]), bytes, a);
    RepaintPhysical(tw, 0, 0, tw^.size.x-1, tw^.size.y-1);
END EraseScreen;

PROCEDURE EraseRect(tw : TextWindow; rect : twRECT; a : ScreenAttribute);
VAR
    x   : COORDINATE;
    y   : COORDINATE;
BEGIN
    FOR y := rect.y1-tw^.row TO rect.y2-tw^.row DO
        IF (y >= 0) AND (y < tw^.size.y) THEN
            FOR x := rect.x1-tw^.col TO rect.x2-tw^.col DO
                IF (x >= 0) AND (x < tw^.size.x) THEN
                    tw^.scrCh^[y, x] := ' ';
                    tw^.scrAttr^[y, x] := a;
                END;
            END;
        END;
    END;
    RepaintVirtual(tw, rect.x1, rect.y1, rect.x2, rect.y2);
END EraseRect;

PROCEDURE Xpos(tw : TextWindow) : COORDINATE;
BEGIN
    RETURN tw^.caretX;
END Xpos;

PROCEDURE Ypos(tw : TextWindow) : COORDINATE;
BEGIN
    RETURN tw^.caretY;
END Ypos;

PROCEDURE Xorg(tw : TextWindow) : COORDINATE;
BEGIN
    RETURN tw^.col;
END Xorg;

PROCEDURE Yorg(tw : TextWindow) : COORDINATE;
BEGIN
    RETURN tw^.row;
END Yorg;

PROCEDURE Xmax(tw : TextWindow) : COORDINATE;
BEGIN
    RETURN tw^.col + tw^.size.x - 1;
END Xmax;

PROCEDURE Ymax(tw : TextWindow) : COORDINATE;
BEGIN
    RETURN tw^.row + tw^.size.y - 1;
END Ymax;

PROCEDURE VirtualToBuffer(tw : TextWindow; VAR INOUT pt : twPOINT);
BEGIN
    IF pt.x >= tw^.col THEN
        pt.x := pt.x - tw^.col;
    ELSE
        pt.x := 0;
    END;
    IF pt.y >= tw^.row THEN
        pt.y := pt.y - tw^.row;
    ELSE
        pt.y := 0;
    END;
END VirtualToBuffer;

PROCEDURE BufferToVirtual(tw : TextWindow; VAR INOUT pt : twPOINT);
BEGIN
    IF pt.x >= 0 THEN
        pt.x := pt.x + tw^.col;
    END;
    IF pt.y >= 0 THEN
        pt.y := pt.y + tw^.row;
    END;
END BufferToVirtual;

PROCEDURE ClientToBuffer(tw : TextWindow; VAR INOUT pt : twPOINT);
BEGIN
    IF tw^.gutter THEN
        IF pt.x >= tw^.cw THEN
            pt.x := ((pt.x-tw^.cw) / tw^.cw) + (tw^.vleft - tw^.col);
        ELSE
            pt.x := -1;
        END;
    ELSE
        pt.x := (pt.x / tw^.cw) + (tw^.vleft - tw^.col);
    END;
    pt.y := (pt.y / tw^.ch) + (tw^.vtop - tw^.row);
END ClientToBuffer;

PROCEDURE BufferToClient(tw : TextWindow; VAR INOUT pt : twPOINT);
BEGIN
    pt.x := (pt.x - (tw^.vleft - tw^.col)) * tw^.cw;
    pt.y := (pt.y - (tw^.vtop - tw^.row)) * tw^.ch;

    IF tw^.gutter THEN
        pt.x := pt.x + tw^.cw;
    END;
END BufferToClient;

PROCEDURE GetColorTable(VAR OUT table : ColorTable);
BEGIN
    table := Color;
END GetColorTable;

PROCEDURE PaintOff(tw : TextWindow);
BEGIN
    IF tw^.paintLock = 0 THEN
        tw^.painted := FALSE;
        tw^.lockRect.x1 := MAX(COORDINATE);
        tw^.lockRect.y1 := MAX(COORDINATE);
        tw^.lockRect.x2 := MIN(COORDINATE);
        tw^.lockRect.y2 := MIN(COORDINATE);
    END;

    INC(tw^.paintLock);
END PaintOff;

PROCEDURE PaintOn(tw : TextWindow);
BEGIN
    IF tw^.paintLock <> 0 THEN
        DEC(tw^.paintLock);
        IF tw^.paintLock = 0 THEN
            IF tw^.painted THEN
                tw^.painted := FALSE;
                RepaintVirtual(tw, tw^.lockRect.x1, tw^.lockRect.y1, tw^.lockRect.x2, tw^.lockRect.y2);
            END;
        END;
    END;
END PaintOn;

PROCEDURE FlushPaint(tw : TextWindow);
VAR
    saveLock    : CARDINAL;
BEGIN
    IF tw^.paintLock <> 0 THEN
        IF tw^.painted THEN
            tw^.painted := FALSE;

            saveLock := tw^.paintLock;
            tw^.paintLock := 0;

            RepaintVirtual(tw,
                           tw^.lockRect.x1, tw^.lockRect.y1,
                           tw^.lockRect.x2, tw^.lockRect.y2);

            tw^.lockRect.x1 := MAX(COORDINATE);
            tw^.lockRect.y1 := MAX(COORDINATE);
            tw^.lockRect.x2 := MIN(COORDINATE);
            tw^.lockRect.y2 := MIN(COORDINATE);

            tw^.paintLock := saveLock;
        END;
    END;
END FlushPaint;

PROCEDURE IsPaintOn(tw : TextWindow) : BOOLEAN;
BEGIN
    RETURN tw^.paintLock = 0;
END IsPaintOn;

PROCEDURE GetWinShellHandle(tw : TextWindow) : WinShell.Window;
BEGIN
    IF tw <> NIL THEN
        RETURN tw^.w;
    END;
    RETURN NIL;
END GetWinShellHandle;

PROCEDURE FindTextWindow(w : WinShell.Window) : TextWindow;
VAR
    tw  : TextWindow;
BEGIN
    tw := FirstWindow;
    WHILE (tw <> NIL) AND (tw^.w <> w) DO
        tw := tw^.next;
    END;
    RETURN tw;
END FindTextWindow;

(* START WinShell exported stuff *)

PROCEDURE Beep(beep : Beeps);
BEGIN
    WinShell.Beep(beep);
END Beep;

PROCEDURE GetWindowSize(tw : TextWindow; VAR OUT width, height : COORDINATE);
BEGIN
    WinShell.GetWindowSize(tw^.w, width, height);
END GetWindowSize;

PROCEDURE SetWindowSize(tw : TextWindow; width, height : COORDINATE);
BEGIN
    WinShell.SetWindowSize(tw^.w, width, height);
END SetWindowSize;

PROCEDURE GetWindowPos(tw : TextWindow; VAR OUT x, y : COORDINATE);
BEGIN
    WinShell.GetWindowPos(tw^.w, x, y);
END GetWindowPos;

PROCEDURE SetWindowPos(tw : TextWindow; x, y : COORDINATE);
BEGIN
    WinShell.SetWindowPos(tw^.w, x, y);
END SetWindowPos;

PROCEDURE CascadeWindow(cascadeThis, onThis : TextWindow);
BEGIN
    WinShell.CascadeWindow(cascadeThis^.w, onThis^.w);
END CascadeWindow;

PROCEDURE SetWindowIsBusy(tw : TextWindow; busy : BOOLEAN);
BEGIN
    WinShell.SetWindowIsBusy(tw^.w, busy);
END SetWindowIsBusy;

PROCEDURE GetWindowDisplayInfo(tw : TextWindow; VAR OUT info : WindowDisplayInfo);
BEGIN
    WinShell.GetWindowDisplayInfo(tw^.w, info);
END GetWindowDisplayInfo;

PROCEDURE SetWindowDisplayInfo(tw : TextWindow; info : WindowDisplayInfo);
BEGIN
    WinShell.SetWindowDisplayInfo(tw^.w, info);
END SetWindowDisplayInfo;

PROCEDURE SetScrollDisableWhenNone(tw : TextWindow; yesH, yesV : BOOLEAN);
BEGIN
    WinShell.SetScrollDisableWhenNone(tw^.w, yesH, yesV);
END SetScrollDisableWhenNone;

PROCEDURE SetActiveTabChild(tw : TextWindow);
BEGIN
    WinShell.SetActiveTabChild(tw^.w);
END SetActiveTabChild;

PROCEDURE SetTabChildPosition(tw : TextWindow; index : CARDINAL);
BEGIN
    WinShell.SetTabChildPosition(tw^.w, index);
END SetTabChildPosition;

PROCEDURE GetForegroundWindow() : TextWindow;
VAR
    tw  : TextWindow;
    w   : WinShell.Window;
BEGIN
    w := WinShell.GetForegroundWindow();
    IF w <> NIL THEN
        tw := FirstWindow;
        WHILE tw <> NIL DO
            IF tw^.w = w THEN
                RETURN tw;
            END;
            tw := tw^.next;
        END;
    END;
    RETURN NIL;
END GetForegroundWindow;

PROCEDURE SetForegroundWindow(tw : TextWindow);
BEGIN
    WinShell.SetForegroundWindow(tw^.w);
END SetForegroundWindow;

PROCEDURE CreateStatusLine(tw : TextWindow; fmt : ARRAY OF INTEGER) : BOOLEAN;
BEGIN
    RETURN WinShell.CreateStatusLine(tw^.w, fmt);
END CreateStatusLine;

PROCEDURE RemoveStatusLine(tw :TextWindow);
BEGIN
    WinShell.RemoveStatusLine(tw^.w);
END RemoveStatusLine;

PROCEDURE SetStatusFormat(tw : TextWindow; fmt : ARRAY OF INTEGER);
BEGIN
    WinShell.SetStatusFormat(tw^.w, fmt);
END SetStatusFormat;

PROCEDURE WriteStatusField(tw :TextWindow; field : CARDINAL; txt : ARRAY OF CHAR);
BEGIN
    WinShell.WriteStatusField(tw^.w, field, txt);
END WriteStatusField;

PROCEDURE SetWindowIcon(tw : TextWindow; icon : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN WinShell.SetWindowIcon(tw^.w, icon);
END SetWindowIcon;

PROCEDURE SetWindowCursor(tw : TextWindow; typ : CursorTypes);
BEGIN
    WinShell.SetWindowCursor(tw^.w, typ);
END SetWindowCursor;

PROCEDURE SetWindowMenu(tw : TextWindow; menu : ARRAY OF CHAR) : BOOLEAN;
BEGIN
    RETURN WinShell.SetWindowMenu(tw^.w, menu);
END SetWindowMenu;

PROCEDURE SetMenuItemEnable(tw : TextWindow; id : CARDINAL; state : BOOLEAN);
BEGIN
    WinShell.SetMenuItemEnable(tw^.w, id, state);
END SetMenuItemEnable;

PROCEDURE GetMenuItemEnable(tw : TextWindow; id : CARDINAL) : BOOLEAN;
BEGIN
    RETURN WinShell.GetMenuItemEnable(tw^.w, id);
END GetMenuItemEnable;

PROCEDURE SetMenuItemCheck(tw : TextWindow; id : CARDINAL; state : BOOLEAN);
BEGIN
    WinShell.SetMenuItemCheck(tw^.w, id, state);
END SetMenuItemCheck;

PROCEDURE GetMenuItemCheck(tw : TextWindow; id : CARDINAL) : BOOLEAN;
BEGIN
    RETURN WinShell.GetMenuItemCheck(tw^.w, id);
END GetMenuItemCheck;

PROCEDURE SetMenuItemRadioCheck(tw : TextWindow;
                                first, last, set : CARDINAL);
BEGIN
    WinShell.SetMenuItemRadioCheck(tw^.w, first, last, set);
END SetMenuItemRadioCheck;

PROCEDURE GetMenuItemRadioCheck(tw : TextWindow;
                                first, last : CARDINAL) : CARDINAL;
BEGIN
    RETURN WinShell.GetMenuItemRadioCheck(tw^.w, first, last);
END GetMenuItemRadioCheck;

PROCEDURE GetWindowMenu(tw : TextWindow) : MenuHandle;
BEGIN
    RETURN WinShell.GetWindowMenu(tw^.w);
END GetWindowMenu;

PROCEDURE LoadMenu(tw : TextWindow;
                   resId : ARRAY OF CHAR;
                   popup : BOOLEAN) : MenuHandle;
BEGIN
    RETURN WinShell.LoadMenu(tw^.w, resId, popup);
END LoadMenu;

PROCEDURE AppendMenuItemStr(tw : TextWindow;
                            menuH : MenuHandle;
                            str : ARRAY OF CHAR;
                            id : CARDINAL) : BOOLEAN;
BEGIN
    RETURN WinShell.AppendMenuItemStr(tw^.w, menuH, str, id);
END AppendMenuItemStr;

PROCEDURE AppendMenuItemSeparator(tw : TextWindow; menuH : MenuHandle) : BOOLEAN;
BEGIN
    RETURN WinShell.AppendMenuItemSeparator(tw^.w, menuH);
END AppendMenuItemSeparator;

PROCEDURE DeleteMenuItemPosition(menuH : MenuHandle; pos : CARDINAL) : BOOLEAN;
BEGIN
    RETURN WinShell.DeleteMenuItemPosition(menuH, pos);
END DeleteMenuItemPosition;

PROCEDURE PopupMenuHandle(tw : TextWindow;
                          menuH : MenuHandle;
                          button : MouseButton;
                          x, y : COORDINATE);
BEGIN
    IF (x >= tw^.vleft) AND
       (x <= tw^.vright) AND
       (y >= tw^.vtop) AND
       (y <= tw^.vbottom)
    THEN
        x := (x - tw^.vleft) * tw^.cw;
        y := (y - tw^.vtop) * tw^.ch;
        WinShell.PopupMenuHandle(tw^.w, menuH, button, x, y);
    END;
END PopupMenuHandle;

PROCEDURE PopupMenu(tw : TextWindow;
                    menuId : ARRAY OF CHAR;
                    button : MouseButton;
                    x, y : COORDINATE);
BEGIN
    IF (x >= tw^.vleft) AND
       (x <= tw^.vright) AND
       (y >= tw^.vtop) AND
       (y <= tw^.vbottom)
    THEN
        x := (x - tw^.vleft) * tw^.cw;
        y := (y - tw^.vtop) * tw^.ch;
        WinShell.PopupMenu(tw^.w, menuId, button, x, y);
    END;
END PopupMenu;

PROCEDURE SetTimer(tw : TextWindow; timerId : CARDINAL; interval : CARDINAL);
BEGIN
    WinShell.SetTimer(tw^.w, timerId, interval);
END SetTimer;

PROCEDURE KillTimer(tw : TextWindow; timerId : CARDINAL);
BEGIN
    WinShell.KillTimer(tw^.w, timerId);
END KillTimer;

PROCEDURE OpenClipboard(tw : TextWindow) : BOOLEAN;
BEGIN
    RETURN WinShell.OpenClipboard(tw^.w);
END OpenClipboard;

PROCEDURE CloseClipboard(tw : TextWindow);
BEGIN
    WinShell.CloseClipboard(tw^.w);
END CloseClipboard;

PROCEDURE EmptyClipboard(tw : TextWindow) : BOOLEAN;
BEGIN
    RETURN WinShell.EmptyClipboard(tw^.w);
END EmptyClipboard;

PROCEDURE ClipboardFormatAvailable(fmt : ClipboardFormat) : BOOLEAN;
BEGIN
    RETURN WinShell.ClipboardFormatAvailable(fmt);
END ClipboardFormatAvailable;

PROCEDURE AllocClipboardMemory(size : CARDINAL) : ADDRESS;
BEGIN
    RETURN WinShell.AllocClipboardMemory(size);
END AllocClipboardMemory;

PROCEDURE UnlockClipboardMemory;
BEGIN
    WinShell.UnlockClipboardMemory;
END UnlockClipboardMemory;

PROCEDURE SetClipboard(fmt : ClipboardFormat) : BOOLEAN;
BEGIN
    RETURN WinShell.SetClipboard(fmt);
END SetClipboard;

PROCEDURE GetClipboard(fmt : ClipboardFormat) : ADDRESS;
BEGIN
    RETURN WinShell.GetClipboard(fmt);
END GetClipboard;

(*------------------ Start InternalStuff -----------------------*)

PROCEDURE GetFontSize(tw : TextWindow);
VAR
    tm                  : ARRAY FontTypes OF WinShell.TextMetrics;
    ft                  : FontTypes;
    height              : CARDINAL;
    maxWidth            : CARDINAL;
    maxHeight           : CARDINAL;
BEGIN
    FOR ft := MIN(ft) TO MAX(ft) DO
        WinShell.GetTextMetrics(tw^.fonts[ft], tm[ft]);
    END;

    maxWidth := 0;
    maxHeight := 0;
    FOR ft := MIN(ft) TO MAX(ft) DO
        IF tm[ft].aveDigitWidth > maxWidth THEN
            maxWidth := tm[ft].aveDigitWidth;
        END;

        height := tm[ft].height + tm[ft].externalLeading;
        tw^.fontHeight[ft] := height;
        IF height > maxHeight THEN
            maxHeight := height;
        END;
    END;

    (* we take the normal height because "normal" text just looks better.
       many times the italic and/or bold are a pixel or two taller and the
       extra white space makes things look a bit funny.
    *)

    maxHeight := tm[FtNormal].height + tm[FtNormal].externalLeading;

    FOR ft := MIN(ft) TO MAX(ft) DO
        tw^.fontExtra[ft] := maxWidth - tm[ft].aveDigitWidth;
    END;

    tw^.ch := maxHeight;
    tw^.cw := maxWidth;

    IF tw^.w <> NIL THEN
        SetCaretType(tw, tw^.caretType);
    END;
END GetFontSize;

(* Repaint the rect, coordinates are visible*)

PROCEDURE RepaintVirtual(tw : TextWindow; x1, y1, x2, y2 : COORDINATE);
VAR
    sc                  : ScreenAttribute;
    r                   : WinShell.wsRECT;
    for, back           : Colors;
    style               : FontStyleSet;
    ft                  : FontTypes;
    lastx, lasty,
    startx, endx,
    eolnPaint, eosPaint,
    hadjust, vadjust,
    y                   : COORDINATE;
    paintGutter         : BOOLEAN;
    draw                : WinShell.Drawable;
BEGIN
    IF tw^.paintLock = 0 THEN

        paintGutter := FALSE;
        IF x1 <= tw^.vleft THEN
            (* less than or equal because of the gutter if present *)
            (* some fonts may paint to the left of the character box *)
            (* by a pixel or two *)

            paintGutter := TRUE;
            x1 := tw^.vleft;
        END;

        IF x2 > tw^.vright THEN
            x2 := tw^.vright;
            IF tw^.partialX THEN
                INC(x2);
            END;
        END;

        IF y1 < tw^.vtop THEN
            y1 := tw^.vtop;
        END;

        IF y2 > tw^.vbottom THEN
            y2 := tw^.vbottom;
            IF tw^.partialY THEN
                INC(y2);
            END;
        END;

        (* convert to zero based client coordinates *)

        x1 := x1 - tw^.col;
        x2 := x2 - tw^.col;
        y1 := y1 - tw^.row;
        y2 := y2 - tw^.row;

        draw := tw^.draw;
        IF draw = NIL THEN
            draw := WinShell.BeginPaint(tw^.w, tw^.dc);
        END;

        IF (y2 >= y1) AND (x2 >= x1) THEN

            hadjust := tw^.vleft - tw^.col;
            vadjust := tw^.vtop - tw^.row;

            IF x2 >= tw^.size.x THEN
                IF x1 >= tw^.size.x THEN
                    x1 := tw^.size.x - 2;
                END;

                lastx := tw^.size.x - 1;
                eolnPaint := x2 - lastx;
            ELSE
                lastx := x2;
                eolnPaint := 0;
            END;

            IF y2 >= tw^.size.y THEN
                IF y1 >= tw^.size.y THEN
                    y1 := tw^.size.y - 2;
                END;

                lasty := tw^.size.y - 1;
                eosPaint := y2 - lasty;
            ELSE
                lasty := y2;
                eosPaint := 0;
                IF tw^.partialY AND (y2 = tw^.size.y-1) THEN
                    (* just in case a decender extended into the blank area *)
                    eosPaint := 1;
                END;
            END;

            (* erase the excess areas first *)

            IF eolnPaint <> 0 THEN
                (* the screen is wider than the buffered text *)

                DecomposeAttribute(tw^.backgroundColor, for, back, style);
                WinShell.SetBackgroundColor(tw^.dc, Color[back]);

                r.x1 := (lastx + 1 - hadjust) * tw^.cw;
                r.y1 := y1 * tw^.ch;
                IF tw^.gutter THEN
                    r.x1 := r.x1 + tw^.cw;
                END;

                WinShell.EraseRectangle(draw,
                                        r.x1, r.y1,
                                        eolnPaint * tw^.cw,
                                        ((y2 - y1) + 1) * tw^.ch);
            END;

            IF eosPaint <> 0 THEN
                (* the screen is taller than the buffered text *)

                DecomposeAttribute(tw^.backgroundColor, for, back, style);
                WinShell.SetBackgroundColor(tw^.dc, Color[back]);

                r.x1 := (x1 - hadjust) * tw^.cw;
                r.y1 := (lasty + 1 - vadjust) * tw^.ch;
                IF tw^.gutter THEN
                    r.x1 := r.x1 + tw^.cw;
                END;

                WinShell.EraseRectangle(draw,
                                        r.x1, r.y1,
                                        ((x2 - x1) + 1) * tw^.cw,
                                        eosPaint * tw^.ch);
            END;

            (* we draw bottom up because we use the normal text height.
               this might clip some decenders on the bold and/or italic fonts.
               specifically the underscore character which is used often.
            *)

            y := lasty;
            WHILE y >= y1 DO
                startx := x1;
                endx := startx;

                WHILE startx <= lastx DO
                    endx := startx + 1;
                    sc := tw^.scrAttr^[y, startx];

                    WHILE (endx <= lastx) AND (sc = tw^.scrAttr^[y, endx]) DO
                        INC(endx);
                    END;

                    DecomposeAttribute(sc, for, back, style);
                    ft := GetFontType(style);

                    WinShell.SetForegroundColor(tw^.dc, Color[for]);
                    WinShell.SetBackgroundColor(tw^.dc, Color[back]);
                    WinShell.SetFont(tw^.dc, tw^.fonts[ft]);
                    WinShell.SetTextExtraSpacing(tw^.dc, tw^.fontExtra[ft]);

                    r.x1 := ((startx - hadjust) * tw^.cw);
                    r.x2 := r.x1 + ((endx - startx) * tw^.cw);
                    r.y1 := (y - vadjust) * tw^.ch;
                    r.y2 := r.y1 + tw^.ch;

                    IF tw^.gutter THEN
                        r.x1 := r.x1 + tw^.cw;
                        r.x2 := r.x2 + tw^.cw;
                    END;

                    WinShell.DrawTextRect(draw,
                                          r.x1, r.y1,
                                          tw^.scrCh^[y][startx..endx-1], (endx - startx),
                                          r, WinShell.DrawTextOpaque);

                    startx := endx;
                END;

                DEC(y);

                IF (y < y1) AND (y >= 0) THEN
                    (* try to deal with the underscore/decender character issue. *)

                    startx := x1;
                    LOOP
                        IF startx <= lastx THEN
                            endx := startx;
                            WHILE (endx <= lastx) AND (tw^.scrCh^[y, endx] <> '_') DO
                                INC(endx);
                            END;

                            IF endx <= lastx THEN
                                sc := tw^.scrAttr^[y, endx];
                                INC(endx);
                                DecomposeAttribute(sc, for, back, style);
                                ft := GetFontType(style);
                                IF tw^.fontHeight[ft] > tw^.ch THEN
                                    (* one more line because we will have painted
                                       over the decender.
                                    *)
                                    DEC(y1);
                                    EXIT;
                                END;
                            END;

                            startx := endx;
                        ELSE
                            EXIT;
                        END;
                    END;
                END;
            END;

        (* what the hell does this think it is doing?
        ELSE
            (* put this back. *)

            x1 := x1 + tw^.col;
            x2 := x2 + tw^.col;
            y1 := y1 + tw^.row;
            y2 := y2 + tw^.row;

            DecomposeAttribute(tw^.backgroundColor, for, back, style);
            WinShell.SetBackgroundColor(tw^.dc, Color[back]);
            WinShell.EraseRectangle(draw,
                                    x1, y1,
                                    ((x2 - x1) + 1) * tw^.cw,
                                    (y2-y1) * tw^.ch);
                                    *)
        END;

        IF paintGutter AND tw^.gutter THEN
            DecomposeAttribute(tw^.backgroundColor, for, back, style);
            WinShell.SetBackgroundColor(tw^.dc, Color[back]);

            WinShell.EraseRectangle(draw,
                                    0, 0,
                                    tw^.cw,
                                    (tw^.lines+INT(tw^.partialY)) * tw^.ch);
        END;

        IF tw^.draw = NIL THEN
            WinShell.EndPaint(tw^.w);
        END;
    ELSE
        tw^.painted := TRUE;

        (* adjust the repaint rectangle *)

        IF x1 < tw^.lockRect.x1 THEN
            tw^.lockRect.x1 := x1;
        END;
        IF x2 > tw^.lockRect.x2 THEN
            tw^.lockRect.x2 := x2;
        END;

        IF y1 < tw^.lockRect.y1 THEN
            tw^.lockRect.y1 := y1;
        END;
        IF y2 > tw^.lockRect.y2 THEN
            tw^.lockRect.y2 := y2;
        END;
    END;
END RepaintVirtual;

PROCEDURE RepaintPhysical(tw : TextWindow; x1, y1, x2, y2 : COORDINATE);
BEGIN
    RepaintVirtual(tw, x1 + tw^.col, y1 + tw^.row, x2 + tw^.col, y2 + tw^.row)
END RepaintPhysical;

(* Takes a windows with vbottom, vtop and adjust them to be *)
(* within the min/max of the window                         *)

PROCEDURE GetVLines(tw : TextWindow);
VAR
    width,
    height,
    bytes,
    j           : INTEGER;
    msg         : TWMessageRec;
BEGIN
    width := tw^.size.x;
    height := tw^.size.y;

    IF tw^.vbottom > tw^.vsmax THEN
        j := tw^.vbottom - tw^.vsmax;
        tw^.vbottom := tw^.vbottom - j;
        tw^.vtop := tw^.vtop - j;
    END;

    IF tw^.vbottom >= (tw^.row + height) THEN
        IF tw^.vtop < (tw^.row + height) THEN
            (* Needs plus 1 , vbottom is inclusive, height is not *)

            j := tw^.vbottom - (tw^.row + height) + 1;
            tw^.row := tw^.row + j;
        ELSE
            j := height;
            tw^.row := tw^.vtop;
        END;

        IF tw^.row + height > tw^.vsmax THEN
            tw^.row := tw^.vsmax - height;
            j := height;
        ELSE
            bytes := (height - j) * width;

            IF bytes > 0 THEN
                MoveMem(ADR(tw^.scrCh^[0,0]), ADR(tw^.scrCh^[j,0]), bytes);
                MoveMem(ADR(tw^.scrAttr^[0,0]), ADR(tw^.scrAttr^[j,0]), bytes*2);
            END;
        END;


        msg.msg := TWM_PAINT;
        msg.paintRect.x1 := tw^.col;
        msg.paintRect.y1 := tw^.row + (height - j);
        msg.paintRect.x2 := msg.paintRect.x1 + width - 1;
        msg.paintRect.y2 := msg.paintRect.y1 + j - 1;
        PaintOff(tw);
        CallWndProc(tw, msg);
        PaintOn(tw);

    ELSIF tw^.vtop < tw^.row THEN
        IF (tw^.vtop+height) >= tw^.row THEN
            j := tw^.row - tw^.vtop;
            bytes := (height - j) * width;
        ELSE
            bytes := -1;
            j := 0;
        END;

        IF bytes > 0 THEN
            MoveMem(ADR(tw^.scrCh^[j,0]), ADR(tw^.scrCh^[0,0]), bytes);
            MoveMem(ADR(tw^.scrAttr^[j,0]), ADR(tw^.scrAttr^[0,0]), bytes*2);
        END;

        IF ( ABS(tw^.vtop - tw^.row) - 1 ) >= height THEN
            j := height - 1;
        ELSE
            j := ABS(tw^.vtop - tw^.row) - 1;
        END;

        tw^.row := tw^.vtop;
        WHILE (tw^.row + height) > tw^.vsmax DO
            DEC(tw^.row);
            INC(j);
        END;

        msg.msg := TWM_PAINT;
        msg.paintRect.x1 := tw^.col;
        msg.paintRect.y1 := tw^.row;
        msg.paintRect.x2 := msg.paintRect.x1 + width - 1;
        msg.paintRect.y2 := msg.paintRect.y1 + j;
        PaintOff(tw);
        CallWndProc(tw, msg);
        PaintOn(tw);

    ELSIF tw^.row + height > tw^.vsmax THEN
        (* this happens when the buffer is bigger than the displayed area *)
        (* and the bottom of the buffer is vbottom and vsmax is reduced *)
        (* then a scroll up occurs and it still is in the buffer *)
        (* be need to push up row so that the buffer bottom *)
        (* is vbottom again *)

        (* this is very rare, be lazy, just brute force it *)

        tw^.row := tw^.vsmax - height;

        msg.msg := TWM_PAINT;
        msg.paintRect.x1 := tw^.col;
        msg.paintRect.y1 := tw^.row;
        msg.paintRect.x2 := msg.paintRect.x1 + width - 1;
        msg.paintRect.y2 := msg.paintRect.y1 + height - 1;
        PaintOff(tw);
        CallWndProc(tw, msg);
        PaintOn(tw);
    END;

    %IF ValidateData %THEN
        VerifyCoords("AFTER GetVLines", WinShell.WSM_CREATE, tw, TRUE);
    %END
END GetVLines;

PROCEDURE GetHLines(tw : TextWindow);
VAR
    width, height,
    bytes,
    j           : COORDINATE;
    i           : COORDINATE;
    umsg        : TWMessageRec;
BEGIN
    width := tw^.size.x;
    height := tw^.size.y;

    umsg.msg := TWM_PAINT;

    IF tw^.vright > tw^.hsmax THEN
        j := tw^.vright - tw^.hsmax;
        tw^.vright := tw^.vright - j;
        tw^.vleft := tw^.vleft - j;
    END;

    IF tw^.vright >= (tw^.col + width) THEN
        IF tw^.vleft < tw^.col + width THEN
            j := tw^.vright - (tw^.col + width) + 1;
            bytes := width - j;
            tw^.col := tw^.col + j;
        ELSE
            j := width;
            bytes := -1;
            tw^.col := tw^.vleft;
        END;

        IF (tw^.col + width) > tw^.hsmax THEN
            (* This is for visible regions smaller than the screen size *)
            (* That move vleft so that col is moved so far right that *)
            (* col + width has points outside of the allowed scroll *)
            (* range.  take the easy way out and fudge col to a legal value *)
            (* and tell the user to repaint EVERYTHING *)

            tw^.col := tw^.hsmax - width;

            umsg.paintRect.x1 := tw^.col;
            umsg.paintRect.y1 := tw^.row;
            umsg.paintRect.x2 := umsg.paintRect.x1 + width - 1;
            umsg.paintRect.y2 := umsg.paintRect.y1 + height - 1;
        ELSE
            IF bytes > 0 THEN
                FOR i := 0 TO height-1 DO
                    MoveMem(ADR(tw^.scrCh^[i,0]), ADR(tw^.scrCh^[i,j]), bytes);
                    MoveMem(ADR(tw^.scrAttr^[i,0]), ADR(tw^.scrAttr^[i,j]), bytes*2);
                END;
            END;

            umsg.paintRect.x1 := (tw^.col + width) - j;
            umsg.paintRect.y1 := tw^.row;
            umsg.paintRect.x2 := umsg.paintRect.x1 + j - 1;
            umsg.paintRect.y2 := umsg.paintRect.y1 + height - 1;
        END;

        PaintOff(tw);
        CallWndProc(tw, umsg);
        PaintOn(tw);

    ELSIF tw^.vleft < tw^.col THEN
        IF (tw^.vleft+width) >= tw^.col THEN
            j := tw^.col - tw^.vleft;
            bytes := width - j;
        ELSE
            j := width;
            bytes := -1;
        END;

        IF bytes > 0 THEN
            FOR i := 0 TO height-1 DO
                MoveMem(ADR(tw^.scrCh^[i,j]), ADR(tw^.scrCh^[i,0]), bytes);
                MoveMem(ADR(tw^.scrAttr^[i,j]), ADR(tw^.scrAttr^[i,0]), bytes*2);
            END;
        END;

        tw^.col := tw^.vleft;

        umsg.paintRect.x1 := tw^.col;
        umsg.paintRect.y1 := tw^.row;
        umsg.paintRect.x2 := umsg.paintRect.x1 + j - 1;
        umsg.paintRect.y2 := umsg.paintRect.y1 + height - 1;

        PaintOff(tw);
        CallWndProc(tw, umsg);
        PaintOn(tw);
    END;

    %IF ValidateData %THEN
        VerifyCoords("AFTER GetHLines", WinShell.WSM_CREATE, tw, TRUE);
    %END
END GetHLines;

PROCEDURE CheckPartialPaint(tw : TextWindow);
BEGIN
    IF tw^.partialX THEN
        RepaintVirtual(tw, tw^.vright+1, tw^.vtop, tw^.vright+1, tw^.vbottom);
    END;
    IF tw^.partialY THEN
        RepaintVirtual(tw, tw^.vleft, tw^.vbottom+1, tw^.vright, tw^.vbottom+1);
    END;
END CheckPartialPaint;

PROCEDURE FixedBufferSizeProc(tw : TextWindow; msg : WinShell.MessageRec);
VAR
    x, y        : COORDINATE;
    paintX      : COORDINATE;
    paintY      : COORDINATE;
    oldLines    : COORDINATE;
    oldCols     : COORDINATE;
    paint       : CARDINAL;
    umsg        : TWMessageRec;
BEGIN
    IF (tw^.cw = 0) OR (tw^.ch = 0) THEN
        RETURN;
    END;

    x := msg.width;
    y := msg.height;
    ComputeTextSize(tw, x, y);

    CheckPartialPaint(tw);

    IF x > tw^.size.x THEN
        x := tw^.size.x;
    END;

    IF y > tw^.size.y THEN
        y := tw^.size.y;
    END;

    IF ((x = tw^.cols) AND (y = tw^.lines)) AND (tw^.state <> WindowStateMaximized) THEN
        RETURN;
    END;

    paintX := tw^.vright;
    paintY := tw^.vbottom;

    tw^.vright := tw^.vleft + x - 1;
    tw^.vbottom := tw^.vtop + y - 1;

    oldLines := tw^.lines;
    tw^.lines := y;

    oldCols := tw^.cols;
    tw^.cols := x;

    paint := 0;

    IF tw^.vright >= tw^.hsmax THEN
        paint := 2;
        tw^.vright := tw^.hsmax - 1;
        tw^.vleft := tw^.vright - tw^.cols + 1;
        IF tw^.vleft < 0 THEN
            tw^.vright := tw^.vright - tw^.vleft;
            tw^.vleft := 0;
        END;

        GetHLines(tw);
        IF tw^.autoScroll THEN
            WinShell.SetScrollBarPos(tw^.w, WA_HSCROLL, tw^.vleft);
        END;
    ELSIF tw^.vright >= (tw^.col + tw^.size.x) THEN
        (* the window was sized beyond the edge of the screen buffer *)

        paint := 1;
        GetHLines(tw);
    END;

    IF tw^.vbottom >= tw^.vsmax THEN
        paint := 2;
        tw^.vbottom := tw^.vsmax - 1;
        tw^.vtop := tw^.vbottom - tw^.lines + 1;
        IF tw^.vtop < 0 THEN
            tw^.vbottom := tw^.vbottom - tw^.vtop;
            tw^.vtop := 0;
        END;

        GetVLines(tw);
        IF tw^.autoScroll THEN
            WinShell.SetScrollBarPos(tw^.w, WA_VSCROLL, tw^.vtop);
        END;
    ELSIF tw^.vbottom >= (tw^.row + tw^.size.y) THEN
        (* the window was sized beyond the edge of the screen buffer *)

        paint := 1;
        GetVLines(tw);
    END;

    IF (paint <> 0) OR (oldCols < x) OR (oldLines < y) THEN
        IF paint = 1 THEN
            RepaintVirtual(tw,
                           paintX, paintY,
                           tw^.vright, tw^.vbottom);
        ELSE
            RepaintVirtual(tw,
                           tw^.vleft, tw^.vtop,
                           tw^.vright, tw^.vbottom);
        END;
    END;

    SetScrollRangesAllowed(tw, tw^.hsval, tw^.vsval);
    MoveCaretTo(tw, tw^.caretX, tw^.caretY);

    umsg.msg := TWM_SIZE;
    umsg.width := tw^.cols;
    umsg.height := tw^.lines;
    CallWndProc(tw, umsg);
END FixedBufferSizeProc;

PROCEDURE VariableBufferSizeProc(tw : TextWindow; msg : WinShell.MessageRec);
VAR
    x, y        : COORDINATE;
    diff        : COORDINATE;
    newScrCh    : ScreenType;
    newScrAttr  : AttrType;
    umsg        : TWMessageRec;
BEGIN
    IF (tw^.cw = 0) OR (tw^.ch = 0) THEN
        RETURN;
    END;

    x := msg.width;
    y := msg.height;
    ComputeTextSize(tw, x, y);

    CheckPartialPaint(tw);

    IF ((x = tw^.cols) AND (y = tw^.lines)) AND (tw^.state <> WindowStateMaximized) THEN
        RETURN;
    END;

    NEW(newScrCh, y-1, x-1);
    IF newScrCh = NIL THEN
        (*!!!*)
    END;

    NEW(newScrAttr, y-1, x-1);
    IF newScrAttr = NIL THEN
        (*!!! *)
    END;

    IF tw^.scrCh <> NIL THEN
        DISPOSE(tw^.scrCh);
    END;
    IF tw^.scrAttr <> NIL THEN
        DISPOSE(tw^.scrAttr);
    END;
    tw^.scrCh := newScrCh;
    tw^.scrAttr := newScrAttr;

    tw^.size.x := x;
    tw^.size.y := y;

    %IF UNICODE %THEN
        FillMemWORD(ADR(tw^.scrCh^[0,0]), tw^.size.x * tw^.size.y, ' ');
    %ELSE
        FillMemBYTE(ADR(tw^.scrCh^[0,0]), tw^.size.x * tw^.size.y, ' ');
    %END
    FillMemWORD(ADR(tw^.scrAttr^[0,0]), tw^.size.x * tw^.size.y, tw^.backgroundColor);

    tw^.lines := y;
    tw^.cols := x;

    MoveCaretTo(tw, tw^.caretX, tw^.caretY);

    IF tw^.row+tw^.size.y > tw^.vsmax THEN
        diff := tw^.row;
        tw^.row := tw^.vsmax - tw^.size.y;
        IF tw^.row < 0 THEN
            tw^.row := 0;
        END;
        diff := diff - tw^.row;
        tw^.vtop := tw^.vtop - diff;
    END;
    IF tw^.col+tw^.size.x > tw^.hsmax THEN
        diff := tw^.col;
        tw^.col := tw^.hsmax - tw^.size.x;
        IF tw^.col < 0 THEN
            tw^.col := 0;
        END;
        diff := diff - tw^.col;
        tw^.vleft := tw^.vleft - diff;
    END;

    tw^.vright := tw^.vleft + tw^.cols - 1;
    tw^.vbottom := tw^.vtop + tw^.lines - 1;

    PaintOff(tw);

    tw^.scrollRangeSet := FALSE;

    umsg.msg := TWM_SIZE;
    umsg.width := tw^.cols;
    umsg.height := tw^.lines;
    CallWndProc(tw, umsg);

    IF NOT tw^.scrollRangeSet THEN
        SetScrollRangesAllowed(tw, tw^.hsval, tw^.vsval);
    END;

    umsg.msg := TWM_PAINT;
    umsg.paintRect.x1 := tw^.col;
    umsg.paintRect.y1 := tw^.row;
    umsg.paintRect.x2 := umsg.paintRect.x1 + tw^.size.x - 1;
    umsg.paintRect.y2 := umsg.paintRect.y1 + tw^.size.y - 1;
    CallWndProc(tw, umsg);

    PaintOn(tw);
END VariableBufferSizeProc;

PROCEDURE TextPaintProc(tw : TextWindow; msg : WinShell.MessageRec);
VAR
    startX, endX,
    startY, endY        : COORDINATE;
    saveDraw            : WinShell.Drawable;
BEGIN
    IF tw^.gutter THEN
        startX := (msg.paintRect.x1-tw^.cw) / tw^.cw;
        endX := ((msg.paintRect.x2-tw^.cw) / tw^.cw) + 1;
    ELSE
        startX := msg.paintRect.x1 / tw^.cw;
        endX := (msg.paintRect.x2 / tw^.cw) + 1;
    END;
    startY := msg.paintRect.y1 / tw^.ch;
    endY := (msg.paintRect.y2 / tw^.ch) + 1;

    saveDraw := tw^.draw;

    (*WinShell.SelectDrawContext(msg.paintDraw, tw^.dc);*)
    tw^.draw := msg.paintDraw;

    RepaintVirtual(tw, tw^.vleft + startX, tw^.vtop + startY, tw^.vleft + endX, tw^.vtop + endY);

    tw^.draw := saveDraw;
END TextPaintProc;

PROCEDURE TextMouseProc(tw : TextWindow; msg : WinShell.MessageRec);
VAR
    umsg        : TWMessageRec;
BEGIN
    umsg.msg := TWM_MOUSE;
    umsg.m_button := msg.m_button;
    umsg.m_event := msg.m_event;
    umsg.m_state := msg.m_state;
    umsg.m_pos := msg.m_pos;
    umsg.m_wheel_count := msg.m_wheel_count;
    umsg.m_wheel_dir := msg.m_wheel_dir;

    ClientToBuffer(tw, umsg.m_pos);
    BufferToVirtual(tw, umsg.m_pos);

    IF (umsg.m_event <> MouseMove) OR
       (umsg.m_pos.x <> tw^.lastMouse.x) OR
       (umsg.m_pos.y <> tw^.lastMouse.y)
    THEN
        tw^.lastMouse := umsg.m_pos;
        CallWndProc(tw, umsg);
    END;
END TextMouseProc;

PROCEDURE WindowInList(tw : TextWindow) : BOOLEAN;
VAR
    ptr         : TextWindow;
BEGIN
    ptr := FirstWindow;
    LOOP
        IF ptr <> NIL THEN
            IF ptr <> tw THEN
                ptr := ptr^.next;
            ELSE
                RETURN TRUE;
            END;
        ELSE
            RETURN FALSE;
        END;
    END;
END WindowInList;

PROCEDURE CreateToolbar(tw : TextWindow;
                        buttons : ARRAY OF ToolbarButtonInfo;
                        hasText : BOOLEAN;
                        hasHelp : BOOLEAN;
                        canCustomize : BOOLEAN) : BOOLEAN;
BEGIN
    RETURN WinShell.CreateToolbar(tw^.w, buttons, hasText, hasHelp, canCustomize);
END CreateToolbar;

PROCEDURE DestroyToolbar(tw : TextWindow);
BEGIN
    WinShell.DestroyToolbar(tw^.w);
END DestroyToolbar;

PROCEDURE SetToolbarButtons(tw : TextWindow; fmt : ARRAY OF CARDINAL);
BEGIN
    WinShell.SetToolbarButtons(tw^.w, fmt);
END SetToolbarButtons;

PROCEDURE GetToolbarButtons(tw : TextWindow; VAR OUT fmt : ARRAY OF CARDINAL) : CARDINAL;
BEGIN
    RETURN WinShell.GetToolbarButtons(tw^.w, fmt);
END GetToolbarButtons;

PROCEDURE IsToolbarButtonShown(tw : TextWindow; index : CARDINAL) : BOOLEAN;
BEGIN
    RETURN WinShell.IsToolbarButtonShown(tw^.w, index);
END IsToolbarButtonShown;

PROCEDURE IsToolbarButtonDown(tw : TextWindow; index : CARDINAL) : BOOLEAN;
BEGIN
    RETURN WinShell.IsToolbarButtonDown(tw^.w, index);
END IsToolbarButtonDown;

PROCEDURE IsToolbarButtonEnabled(tw : TextWindow; index : CARDINAL) : BOOLEAN;
BEGIN
    RETURN WinShell.IsToolbarButtonEnabled(tw^.w, index);
END IsToolbarButtonEnabled;

PROCEDURE ShowToolbarButton(tw : TextWindow;
                            index : CARDINAL;
                            show : BOOLEAN) : BOOLEAN;
BEGIN
    RETURN WinShell.ShowToolbarButton(tw^.w, index, show);
END ShowToolbarButton;

PROCEDURE ToggleToolbarButton(tw : TextWindow;
                              index : CARDINAL;
                              down : BOOLEAN) : BOOLEAN;
BEGIN
    RETURN WinShell.ToggleToolbarButton(tw^.w, index, down);
END ToggleToolbarButton;

PROCEDURE EnableToolbarButton(tw : TextWindow;
                              index : CARDINAL;
                              enable : BOOLEAN) : BOOLEAN;
BEGIN
    RETURN WinShell.EnableToolbarButton(tw^.w, index, enable);
END EnableToolbarButton;

PROCEDURE DisplayHelp(tw : TextWindow;
                      command : HelpCommand;
                      helpFile : ARRAY OF CHAR;
                      helpIndex : CARDINAL) : BOOLEAN;
BEGIN
    RETURN WinShell.DisplayHelp(tw^.w, command, helpFile, helpIndex);
END DisplayHelp;

%IF ValidateData %THEN
PROCEDURE VerifyCoords(str : ARRAY OF CHAR;
                       mess : WinShell.WinShellMsg;
                       tw : TextWindow;
                       okIfMissing : BOOLEAN);
TYPE
    nameStr     = ARRAY [0..15] OF CHAR;
CONST
    names : ARRAY WinShell.WinShellMsg OF nameStr =
        {
         "",(*create*)
         "GAINFOCUS",
         "LOSEFOCUS",
         "ACTIVATE",
         "DEACTIVATE",
         "ACTIVATEAPP",
         "DEACTIVATEAPP",
         "MOUSE",
         "KEY",
         "CLOSE",
         "PAINT",
         "SIZE",
         "POSITIONCHANGED",
         "SCROLL",
         "MENU",
         "MENUSELECT",
         "MENUSTART",
         "MENUEND",
         "MDIACTIVE",
         "CONTEXTMENU",
         "HELP",
         "TIMER",
         "USER"
        };
VAR
    lstr        : ARRAY [0..63] OF CHAR;
    numStr      : ARRAY [0..15] OF CHAR;
    didOne      : BOOLEAN;
BEGIN
    IF NOT WindowInList(tw) THEN
        IF NOT okIfMissing THEN
            Concat(str, " Invalid tw", lstr);
            RaiseRTL(TextWinExSrc, 0, lstr);
        END;
        RETURN;
    END;

    Concat(str, names[mess], lstr);
    didOne := FALSE;

    IF (tw^.col < 0) OR (tw^.col + tw^.size.x > tw^.hsmax) THEN
        didOne := TRUE;
        Concat(lstr, " Invalid Col ", lstr);
        IntToStr(tw^.col, numStr);
        Append(numStr, lstr);
    END;

    IF (tw^.row < 0) OR (tw^.row + tw^.size.y > tw^.vsmax) THEN
        didOne := TRUE;
        Concat(lstr, " Invalid row ", lstr);
        IntToStr(tw^.row, numStr);
        Append(numStr, lstr);
    END;

    IF (tw^.vleft < tw^.col) OR (tw^.vleft >= tw^.col + tw^.size.x) THEN
        didOne := TRUE;
        Concat(lstr, " Invalid vleft ", lstr);
        IntToStr(tw^.vleft, numStr);
        Append(numStr, lstr);
    END;

    IF (tw^.vright < tw^.col) OR (tw^.vright >= tw^.col + tw^.size.x) THEN
        didOne := TRUE;
        Concat(lstr, " Invalid vright ", lstr);
        IntToStr(tw^.vright, numStr);
        Append(numStr, lstr);
    END;

    IF (tw^.vtop < tw^.row) OR (tw^.vtop >= tw^.row + tw^.size.y) THEN
        didOne := TRUE;
        Concat(lstr, " Invalid vtop ", lstr);
        IntToStr(tw^.vtop, numStr);
        Append(numStr, lstr);
    END;

    IF (tw^.vbottom < tw^.row) OR (tw^.vbottom >= tw^.row + tw^.size.y) THEN
        didOne := TRUE;
        Concat(lstr, " Invalid vbottom ", lstr);
        IntToStr(tw^.vbottom, numStr);
        Append(numStr, lstr);
    END;

    IF tw^.vtop > tw^.vbottom THEN
        didOne := TRUE;
        Concat(lstr, " vtop is greater than vbottom", lstr);
    END;

    IF tw^.vleft > tw^.vright THEN
        didOne := TRUE;
        Concat(lstr, " vleft is greater than vright", lstr);
    END;

    IF (tw^.lines < 0) OR (tw^.lines > tw^.size.y) THEN
        didOne := TRUE;
        Concat(lstr, " Invalid lines ", lstr);
        IntToStr(tw^.lines, numStr);
        Append(numStr, lstr);
    END;

    IF (tw^.cols < 0) OR (tw^.cols > tw^.size.x) THEN
        didOne := TRUE;
        Concat(lstr, " Invalid cols ", lstr);
        IntToStr(tw^.cols, numStr);
        Append(numStr, lstr);
    END;

    IF didOne THEN
        RaiseRTL(TextWinExSrc, 0, lstr);
    END;
END VerifyCoords;
%END

PROCEDURE TextWndProc(w : WinShell.Window;
                      msg : WinShell.MessageRec) : WinShell.ResponseType [PROPAGATEEXCEPTION];
VAR
    tw          : TextWindow;
    umsg        : TWMessageRec;
    retVal      : WinShell.ResponseType;
BEGIN
    tw := WinShell.GetWindowData(w, TextWinDataIndex);
    IF tw <> NIL THEN
        IF tw^.validate <> MagicNumber THEN
            (*DebugSC("MessageNumber=", ORD(msg.msg));*)
            RaiseRTL(TextWinExSrc, 0, "TextWndProc INVALID WINDOW");
        END;
    ELSE
        IF msg.msg <> WinShell.WSM_CREATE THEN
            RETURN DEFAULT_HANDLE;
        END;
    END;

    %IF ValidateData %THEN
        VerifyCoords("Before ", msg.msg, tw, FALSE);
    %END

    retVal := DEFAULT_HANDLE;

    (* Mostly boomerang the message with TextWindows message types *)

    CASE msg.msg OF
    WinShell.WSM_CREATE:
        tw := msg.createParam;
        tw^.w := w;
        WinShell.SetWindowData(w, TextWinDataIndex, tw);

        tw^.dc := WinShell.CreateDrawContext(w, DefaultContextValues);
        WinShell.SetDefaultDrawContext(w, tw^.dc);
    |
    WinShell.WSM_GAINFOCUS:
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_GAINFOCUS;
            retVal := CallWndProc(tw, umsg);
        END;
    |
    WinShell.WSM_LOSEFOCUS:
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_LOSEFOCUS;
            retVal := CallWndProc(tw, umsg);
        END;
    |
    WinShell.WSM_ACTIVATEAPP:
        umsg.msg := TWM_ACTIVATEAPP;
        retVal := CallWndProc(tw, umsg);
    |
    WinShell.WSM_DEACTIVATEAPP:
        umsg.msg := TWM_DEACTIVATEAPP;
        retVal := CallWndProc(tw, umsg);
    |
    WinShell.WSM_MOUSE:
        IF NOT tw^.suppressMessage THEN
            TextMouseProc(tw, msg);
            retVal := USER_HANDLE;
        END;
    |
    WinShell.WSM_KEY:
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_KEY;
            umsg.k_ch := msg.k_ch;
            umsg.k_count := msg.k_count;
            umsg.k_state := msg.k_state;
            umsg.k_special := msg.k_special;
            retVal := CallWndProc(tw, umsg);
        END;
    |
    WinShell.WSM_CLOSE:
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_CLOSE;
            umsg.closeMode := msg.closeMode;
            retVal := CallWndProc(tw, umsg);
        ELSE
            retVal := OkayToClose;
        END;
        IF msg.closeMode = CM_DICTATE THEN
            DisposeWindow(tw);
            WinShell.SetWindowData(w, TextWinDataIndex, NIL);
        END;
    |
    WinShell.WSM_PAINT:
        IF NOT tw^.suppressMessage THEN
            TextPaintProc(tw, msg);
        END;
    |
    WinShell.WSM_SIZE:
        IF NOT tw^.suppressMessage THEN
            IF tw^.fixedBuffer THEN
                FixedBufferSizeProc(tw, msg);
            ELSE
                VariableBufferSizeProc(tw, msg);
            END;
        END;
    |
    WinShell.WSM_STATE:
        tw^.state := msg.newState;
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_STATE;
            umsg.oldState := msg.oldState;
            umsg.newState := msg.newState;
            retVal := CallWndProc(tw, umsg);
        END;
    |
    WinShell.WSM_POSITIONCHANGED:
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_POSITIONCHANGED;
            umsg.windowPos := msg.windowPos;
            retVal := CallWndProc(tw, umsg);
        END;
    |
    WinShell.WSM_SCROLL:
        IF NOT tw^.suppressMessage THEN
            IF tw^.autoScroll THEN
                retVal := ScrollVirtual(tw, msg.scrollDir, msg.scrollClass, msg.scrollPos, 1);
            ELSE
                umsg.msg := TWM_SCROLL;
                umsg.scrollDir := msg.scrollDir;
                umsg.scrollClass := msg.scrollClass;
                umsg.scrollPos := msg.scrollPos;
                retVal := CallWndProc(tw, umsg);
            END;
        END;
    |
    WinShell.WSM_MENU:
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_MENU;
            umsg.menuId := msg.menuId;
            retVal := CallWndProc(tw, umsg);
        END;
    |
    WinShell.WSM_MENUSELECT:
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_MENUSELECT;
            umsg.menuId := msg.menuId;
            retVal := CallWndProc(tw, umsg);
        END;
    |
    WinShell.WSM_MENUSTART:
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_MENUSTART;
            retVal := CallWndProc(tw, umsg);
        END;
    |
    WinShell.WSM_MENUEND:
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_MENUEND;
            retVal := CallWndProc(tw, umsg);
        END;
    |
    WinShell.WSM_TAB_ACTIVE:
        IF NOT tw^.suppressMessage THEN
            umsg.msg := TWM_TAB_ACTIVE;
            retVal := CallWndProc(tw, umsg);
        END;
    |
    WinShell.WSM_TIMER:
        umsg.msg := TWM_TIMER;
        umsg.timerId := msg.timerId;
        retVal := CallWndProc(tw, umsg);
    |
    WinShell.WSM_USER:
        umsg.msg := TWM_USER;
        umsg.userId := msg.userId;
        umsg.userData := msg.userData;
        retVal := CallWndProc(tw, umsg);
    ELSE
        (*RAISE(TextWinExSrc, 0, "TXTWndProc, message not handled");*)
    END;

    %IF ValidateData %THEN
        VerifyCoords("After ", msg.msg, tw, TRUE);
    %END

    RETURN retVal;
END TextWndProc;

BEGIN
    IF NOT IsThread THEN
        AllocateSource(TextWinExSrc);

        FirstWindow := NIL;

        DefaultFontInfo := FontInfo{
                                    %IF Windows %THEN
                                    100,(* 10.0 points. 96dpi for screen *)
                                    %ELSE
                                    120,(* 12.0 points. 72dpi for screen *)
                                    %END
                                    FALSE,(*italic*)
                                    FwNormal,
                                    %IF Windows %THEN
                                    "Courier New"
                                    %ELSE
                                    "courier"
                                    %END
                                    };
    END;
END TextWindows.
