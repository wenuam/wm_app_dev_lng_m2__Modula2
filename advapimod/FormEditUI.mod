(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 2009                                  *)
(*                         by ADW Software                                 *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE FormEditUI;
%IF Windows %THEN
    <*/RESOURCE:FormEditUI*>
%END

FROM SYSTEM IMPORT
    ADDRESS, ADRCARD, CAST, ADR, UNREFERENCED_PARAMETER;

FROM Strings IMPORT
    Equal;

FROM ExStorage IMPORT
    ALLOCATE, DEALLOCATE;

FROM Conversions IMPORT
    StrToInt;

FROM MemUtils IMPORT
    MoveMem;

FROM DlgShell IMPORT
    CONTROL, NotifyType, NotifyResult, ControlIdModes, DialogPositions,
    GetControlIdMode, SetControlIdMode, CallDialog, GetUserData,
    GetText, SetText, SetRadioGroup, GetRadioGroup, SetNumericValue, GetCheckBox,
    SetCheckBox,
    GetSpinButtonValue, SetSpinButtonRange,
    SetInputFocusTo;
IMPORT DlgShell;

FROM WinShell IMPORT
    Window,
    ControlProperties, ControlColumnInfo, TextAlignment, ControlPageInfoArray,
    AllocWinShellMem, DeallocWinShellMem,
    CreateStringData, DisposeStringData, AssignStringData,
    EndFormEditMode;
IMPORT WinShell;

FROM BasicDialogs IMPORT
    PromptString;

CONST
    (* property dialog control ids *)

    OkButton            = 1;
    CancelButton        = 2;
    ControlVisible      = 101;
    ControlEnabled      = 102;
    CaptionText         = 103;
    DefaultButton       = 104;
    SpinButtonLow       = 105;
    SpinButtonHigh      = 106;
    TextAlignLeft       = 107;
    (*TextAlignCenter     = 108;*)
    TextAlignRight      = 109;
    EditMultiLine       = 110;
    EditHorizScroll     = 111;
    EditVertScroll      = 112;
    EditLimit           = 113;
    ListNumColumns      = 114;
    RadioButtonGroup    = 115;
    MultiPageIndex      = 116;
    MultiPageTitle      = 117;
    ListHeaderText      = 118;
    ListColumnSelect    = 119;
    ListColumnWidth     = 120;
    EditPassword        = 121;
    ListMultiSelect     = 122;


    NumCommonControls           = 5;
    NumCheckBoxControls         = 1;
    NumRadioControls            = 2;
    NumToggleControls           = 1;
    NumPushControls             = 2;
    NumSpinControls             = 2;
    NumLabelControls            = 2;
    NumGroupControls            = 1;
    NumEditControls             = 5;
    NumComboControls            = 1;
    NumDropDownListControls     = 0;
    NumListControls             = 6;
    NumMultiPageControls        = 2;

TYPE
    CommonControlArray          = ARRAY [0..NumCommonControls-1] OF CONTROL;
    CheckBoxControlArray        = ARRAY [0..NumCheckBoxControls-1] OF CONTROL;
    RadioControlArray           = ARRAY [0..NumRadioControls-1] OF CONTROL;
    ToggleControlArray          = ARRAY [0..NumToggleControls-1] OF CONTROL;
    PushControlArray            = ARRAY [0..NumPushControls-1] OF CONTROL;
    SpinControlArray            = ARRAY [0..NumSpinControls-1] OF CONTROL;
    LabelControlArray           = ARRAY [0..NumLabelControls-1] OF CONTROL;
    GroupControlArray           = ARRAY [0..NumGroupControls-1] OF CONTROL;
    EditControlArray            = ARRAY [0..NumEditControls-1] OF CONTROL;
    ComboControlArray           = ARRAY [0..NumComboControls-1] OF CONTROL;
    ListControlArray            = ARRAY [0..NumListControls-1] OF CONTROL;
    MultiPageControlArray       = ARRAY [0..NumMultiPageControls-1] OF CONTROL;

CONST
    CommonControls = CommonControlArray
    {
     {OkButton, 0, DlgShell.PushButton, DlgShell.NotifyButton, OkButton},
     {CancelButton, 0, DlgShell.PushButton, DlgShell.CancelButton, CancelButton},
     {ControlVisible, 0, DlgShell.CheckBox, FALSE},
     {ControlEnabled, 0, DlgShell.CheckBox, FALSE},
     {0, 0, DlgShell.UserData, NIL}
    };

    CheckBoxControls = CheckBoxControlArray
    {
     {CaptionText, 0, DlgShell.LineEdit, NIL, 64}
    };

    RadioControls = RadioControlArray
    {
     {CaptionText, 0, DlgShell.LineEdit, NIL, 64},
     {RadioButtonGroup, 0, DlgShell.SpinButton, 1, 0, 99}
    };

    ToggleControls = ToggleControlArray
    {
     {CaptionText, 0, DlgShell.LineEdit, NIL, 64}
    };

    PushControls = PushControlArray
    {
     {CaptionText, 0, DlgShell.LineEdit, NIL, 64},
     {DefaultButton, 0, DlgShell.CheckBox, FALSE}
    };

    SpinControls = SpinControlArray
    {
     {SpinButtonLow, 0, DlgShell.LineEdit, NIL, 64},
     {SpinButtonHigh, 0, DlgShell.LineEdit, NIL, 64}
    };

    LabelControls = LabelControlArray
    {
     {CaptionText, 0, DlgShell.LineEdit, NIL, 64},
     {0, 0, DlgShell.RadioGroup, TextAlignLeft, TextAlignRight}
    };

    GroupControls = GroupControlArray
    {
     {CaptionText, 0, DlgShell.LineEdit, NIL, 64}
    };

    EditControls = EditControlArray
    {
     {0, 0, DlgShell.RadioGroup, TextAlignLeft, TextAlignRight},
     {EditMultiLine, 0, DlgShell.CheckBox, FALSE},
     {EditHorizScroll, 0, DlgShell.CheckBox, FALSE},
     {EditVertScroll, 0, DlgShell.CheckBox, FALSE},
     {EditPassword, 0, DlgShell.CheckBox, FALSE}
    };

    ComboControls = ComboControlArray
    {
     {EditLimit, 0, DlgShell.LineEdit, NIL, 5}
    };

    ListControls = ListControlArray
    {
     {ListNumColumns, 0, DlgShell.SpinButton, 1, 1, 99},
     {ListColumnSelect, 0, DlgShell.SpinButton, 0, 0, 0},
     {0, 0, DlgShell.RadioGroup, TextAlignLeft, TextAlignRight},
     {ListHeaderText, 0, DlgShell.LineEdit, NIL, 64},
     {ListColumnWidth, 0, DlgShell.LineEdit, NIL, 5},
     {ListMultiSelect, 0, DlgShell.CheckBox, FALSE}
    };

    MultiPageControls = MultiPageControlArray
    {
     {MultiPageIndex, 0, DlgShell.SpinButton, 0, 0, 0},
     {MultiPageTitle, 0, DlgShell.LineEdit, NIL, 64}
    };

TYPE
    ControlPropPointer  = POINTER TO ControlProperties;
    DialogDataRec =
        RECORD
        selected        : CARDINAL;
        prop            : ControlPropPointer;
        END;
    DialogData  = POINTER TO DialogDataRec;

PROCEDURE PropNotify(typ : NotifyType; VAR INOUT item : CARDINAL) : NotifyResult;
VAR
    data        : DialogData;
    text, text1 : ARRAY [0..255] OF CHAR;
    prop        : ControlPropPointer;
    index       : CARDINAL;
    newCount    : CARDINAL;
    ok          : BOOLEAN;
    low, high   : INTEGER;

    PROCEDURE allocColumnInfo(data : DialogData; count : ADRCARD);
    VAR
        i               : ADRCARD;
        newColumns      : POINTER TO ARRAY [0..0] OF ControlColumnInfo;
        prop            : ControlPropPointer;
    BEGIN
        prop := data^.prop;
        AllocWinShellMem(newColumns, count * SIZE(ControlColumnInfo));
        FOR i := 0 TO count-1 DO
            <*/PUSH/NOWARN:U*>
            IF (prop^.lb_columnInfo <> NIL) AND (i < VAL(ADRCARD, prop^.lb_numColumns)) THEN
            <*/POP*>
                newColumns^[i] := prop^.lb_columnInfo^[i];
            ELSE
                newColumns^[i].header := NIL;
                newColumns^[i].align := AlignLeft;
                newColumns^[i].width := -1;
            END;
        END;
        <*/PUSH/NOWARN:U*>
        FOR i := count TO VAL(ADRCARD, prop^.lb_numColumns)-1 DO
        <*/POP*>
            DisposeStringData(prop^.lb_columnInfo^[i].header);
        END;
        IF prop^.lb_columnInfo <> NIL THEN
            DeallocWinShellMem(prop^.lb_columnInfo,
                               prop^.lb_numColumns * SIZE(ControlColumnInfo));
        END;

        prop^.lb_columnInfo := CAST(ADDRESS, newColumns);
        prop^.lb_numColumns := count;
    END allocColumnInfo;

    PROCEDURE getListFields(data : DialogData);
    VAR
        index   : ADRCARD;
        num     : INTEGER;
        prop    : ControlPropPointer;
    BEGIN
        prop := data^.prop;
        index := data^.selected;
        <*/PUSH/NOWARN:U*>
        IF index < VAL(ADRCARD, prop^.lb_numColumns) THEN
        <*/POP*>
            IF prop^.lb_columnInfo = NIL THEN
                allocColumnInfo(data, prop^.lb_numColumns);
            END;

            IF GetText(ListHeaderText, text) THEN
                AssignStringData(prop^.lb_columnInfo^[index].header, text1);
                IF NOT Equal(text, text1) THEN
                    DisposeStringData(prop^.lb_columnInfo^[index].header);
                    prop^.lb_columnInfo^[index].header := CreateStringData(text);
                END;
            END;

            prop^.lb_columnInfo^[index].align :=
                        VAL(TextAlignment, GetRadioGroup(TextAlignLeft));

            IF GetText(ListColumnWidth, text) THEN
                IF StrToInt(text, num) THEN
                    prop^.lb_columnInfo^[index].width := num;
                END;
            END;
        END;
    END getListFields;

    PROCEDURE setListFields(data : DialogData; index : CARDINAL);
    VAR
        prop            : ControlPropPointer;
    BEGIN
        prop := data^.prop;
        IF index < prop^.lb_numColumns THEN
            data^.selected := index;
            IF prop^.lb_columnInfo <> NIL THEN
                SetRadioGroup(TextAlignLeft, ORD(prop^.lb_columnInfo^[index].align));
                AssignStringData(prop^.lb_columnInfo^[index].header, text);
                ok := SetText(ListHeaderText, text);
                ok := SetNumericValue(ListColumnWidth, prop^.lb_columnInfo^[index].width);
            ELSE
                SetRadioGroup(TextAlignLeft, 0);
                ok := SetText(ListHeaderText, "");
                ok := SetNumericValue(ListColumnWidth, -1);
            END;
        END;
    END setListFields;

    PROCEDURE getMultiPageFields(data : DialogData);
    VAR
        pi      : ControlPageInfoArray;
        index   : ADRCARD;
        prop    : ControlPropPointer;
    BEGIN
        prop := data^.prop;
        index := data^.selected;
        IF GetText(MultiPageTitle, text) THEN
            pi := prop^.mp_pageInfo;
            AssignStringData(pi^[index].label, text1);
            IF NOT Equal(text, text1) THEN
                DisposeStringData(pi^[index].label);
                pi^[index].label := CreateStringData(text);
            END;
        END;
    END getMultiPageFields;

    PROCEDURE setMultiPageFields(data : DialogData; page : CARDINAL);
    VAR
        ok      : BOOLEAN;
        pi      : ControlPageInfoArray;
    BEGIN
        IF page < data^.prop^.mp_numPages THEN
            data^.selected := page;
            pi := data^.prop^.mp_pageInfo;
            AssignStringData(pi^[page].label, text);
            ok := SetText(MultiPageTitle, text);
        END;
    END setMultiPageFields;

BEGIN
    data := GetUserData();
    prop := data^.prop;

    CASE item OF
    MultiPageIndex:
        IF typ = ValueChanged THEN
            getMultiPageFields(data);
            setMultiPageFields(data, GetSpinButtonValue(MultiPageIndex));
        END;
    |
    ListNumColumns:
        IF typ = ValueChanged THEN
            newCount := GetSpinButtonValue(ListNumColumns);
            IF newCount <> data^.prop^.lb_numColumns THEN
                allocColumnInfo(data, newCount);

                SetSpinButtonRange(ListColumnSelect, 0, newCount-1);

                index := GetSpinButtonValue(ListColumnSelect);
                IF index >= newCount THEN
                    ok := SetNumericValue(ListColumnSelect, newCount-1);
                END;
            END;
        END;
    |
    ListColumnSelect:
        IF typ = ValueChanged THEN
            getListFields(data);
            setListFields(data, GetSpinButtonValue(ListColumnSelect));
        END;
    |
    ListColumnWidth:
        IF typ = Validate THEN
            IF GetText(ListColumnWidth, text) THEN
                IF (NOT StrToInt(text, low)) OR
                   (low < -1) OR
                   (low > 999)
                THEN
                    index := GetSpinButtonValue(ListColumnSelect);
                    ok := SetNumericValue(ListColumnSelect, index);
                    setListFields(data, index);
                    RETURN FailedValidation;
                END;
            END;
        END;
    |
    OkButton:
        IF typ = Pressed THEN
            prop^.visible := GetCheckBox(ControlVisible);
            prop^.enabled := GetCheckBox(ControlEnabled);

            CASE prop^.controlType OF
            WinShell.CheckBox:
                IF GetText(CaptionText, text) THEN
                    DisposeStringData(prop^.chk_textPtr);
                    prop^.chk_textPtr := CreateStringData(text);
                END;
            |
            WinShell.RadioButton:
                IF GetText(CaptionText, text) THEN
                    DisposeStringData(prop^.rb_textPtr);
                    prop^.rb_textPtr := CreateStringData(text);
                END;

                prop^.rb_group := GetSpinButtonValue(RadioButtonGroup);
            |
            WinShell.PushButton, WinShell.ToggleButton:
                IF GetText(CaptionText, text) THEN
                    DisposeStringData(prop^.b_textPtr);
                    prop^.b_textPtr := CreateStringData(text);
                END;

                IF prop^.controlType = WinShell.PushButton THEN
                    prop^.b_default := GetCheckBox(DefaultButton);
                END;
            |
            WinShell.SpinButton:
                IF GetText(SpinButtonLow, text) THEN
                    IF StrToInt(text, low) THEN
                        IF GetText(SpinButtonHigh, text) THEN
                            IF StrToInt(text, high) THEN
                                IF low <= high THEN
                                    prop^.sb_low := low;
                                    prop^.sb_high := high;
                                END;
                            END;
                        END;
                    END;
                END;
            |
            WinShell.ListBox:
                prop^.lb_multiSelect := GetCheckBox(ListMultiSelect);
                getListFields(data);
            |
            WinShell.DropDownList:
            |
            WinShell.ComboBox:
                IF GetText(EditLimit, text) THEN
                    IF StrToInt(text, low) THEN
                        prop^.cb_editLimit := low;
                    ELSE
                        SetInputFocusTo(EditLimit);
                        RETURN ContinueDialog;
                    END;
                END;
            |
            WinShell.TextLabel:
                IF GetText(CaptionText, text) THEN
                    DisposeStringData(prop^.tl_textPtr);
                    prop^.tl_textPtr := CreateStringData(text);
                END;
                index := GetRadioGroup(TextAlignLeft);
                prop^.tl_align := VAL(TextAlignment, index);
            |
            WinShell.TextEdit:
                prop^.te_multiLine := GetCheckBox(EditMultiLine);
                prop^.te_horizScroll := GetCheckBox(EditHorizScroll);
                prop^.te_vertScroll := GetCheckBox(EditVertScroll);
                prop^.te_password :=  GetCheckBox(EditPassword);
                index := GetRadioGroup(TextAlignLeft);
                prop^.te_align := VAL(TextAlignment, index);
                text[0] := '';
                IF GetText(EditLimit, text) THEN
                    IF StrToInt(text, low) THEN
                        prop^.te_limit := low;
                    ELSE
                        SetInputFocusTo(EditLimit);
                        RETURN ContinueDialog;
                    END;
                END;
            |
            WinShell.GroupBox:
                IF GetText(CaptionText, text) THEN
                    DisposeStringData(prop^.grp_textPtr);
                    prop^.grp_textPtr := CreateStringData(text);
                END;
            |
            WinShell.MultiPage:
                getMultiPageFields(data);
            END;

            item := OkButton;
            RETURN TerminateDialog;
        END;
    |
    MAX(CARDINAL):
        IF typ = InitDialog THEN
            SetCheckBox(ControlVisible, prop^.visible);
            SetCheckBox(ControlEnabled, prop^.enabled);

            CASE prop^.controlType OF
            WinShell.CheckBox:
                AssignStringData(prop^.chk_textPtr, text);
                ok := SetText(CaptionText, text);
            |
            WinShell.RadioButton:
                AssignStringData(prop^.rb_textPtr, text);
                ok := SetText(CaptionText, text);
                ok := SetNumericValue(RadioButtonGroup, prop^.rb_group);
            |
            WinShell.PushButton, WinShell.ToggleButton:
                AssignStringData(prop^.b_textPtr, text);
                ok := SetText(CaptionText, text);

                IF prop^.controlType = WinShell.PushButton THEN
                    SetCheckBox(DefaultButton, prop^.b_default);
                END;
            |
            WinShell.SpinButton:
                ok := SetNumericValue(SpinButtonLow, prop^.sb_low);
                ok := SetNumericValue(SpinButtonHigh, prop^.sb_high);
            |
            WinShell.ListBox:
                index := prop^.lb_numColumns;
                ok := SetNumericValue(ListNumColumns, index);
                IF index > 0 THEN
                    DEC(index);
                END;
                SetSpinButtonRange(ListColumnSelect, 0, index);
                ok := SetNumericValue(ListColumnSelect, 0);
                setListFields(data, 0);
                SetCheckBox(ListMultiSelect, prop^.lb_multiSelect);
            |
            WinShell.DropDownList:
            |
            WinShell.ComboBox:
                ok := SetNumericValue(EditLimit, prop^.cb_editLimit);
            |
            WinShell.TextLabel:
                AssignStringData(prop^.tl_textPtr, text);
                ok := SetText(CaptionText, text);
                SetRadioGroup(TextAlignLeft, ORD(prop^.tl_align));
            |
            WinShell.TextEdit:
                SetCheckBox(EditMultiLine, prop^.te_multiLine);
                SetCheckBox(EditHorizScroll, prop^.te_horizScroll);
                SetCheckBox(EditVertScroll, prop^.te_vertScroll);
                SetRadioGroup(TextAlignLeft, ORD(prop^.te_align));
                ok := SetNumericValue(EditLimit, prop^.te_limit);
                SetCheckBox(EditPassword, prop^.te_password);
            |
            WinShell.GroupBox:
                AssignStringData(prop^.grp_textPtr, text);
                ok := SetText(CaptionText, text);
            |
            WinShell.MultiPage:
                index := prop^.mp_numPages;
                IF index > 0 THEN
                    DEC(index);
                END;
                SetSpinButtonRange(MultiPageIndex, 0, index);
                ok := SetNumericValue(MultiPageIndex, 0);
                setMultiPageFields(data, 0);
            END;
        END;
    ELSE
    END;

    RETURN ContinueDialog;
END PropNotify;

PROCEDURE BasicPropertiesProc(w : Window;
                              VAR INOUT prop : ControlProperties) : BOOLEAN;
TYPE
    nameStr     = ARRAY [0..31] OF CHAR;
    namesArray  = ARRAY WinShell.ControlTypes OF nameStr;
    countArray  = ARRAY WinShell.ControlTypes OF CARDINAL;
    addrArray   = ARRAY WinShell.ControlTypes OF ADDRESS;
CONST
    dialogNames = namesArray
    {
     "FORM-EDIT-CHECKBOX",
     "FORM-EDIT-RADIOBUTTON",
     "FORM-EDIT-PUSHBUTTON",
     "FORM-EDIT-TOGGLEBUTTON",
     "FORM-EDIT-SPINBUTTON",
     "FORM-EDIT-LISTBOX",
     "FORM-EDIT-DROPDOWNLIST",
     "FORM-EDIT-COMBOBOX",
     "FORM-EDIT-TEXTLABEL",
     "FORM-EDIT-TEXTEDIT",
     "FORM-EDIT-GROUPBOX",
     "FORM-EDIT-MULTIPAGE"
    };

    propCount = countArray
    {
     NumCheckBoxControls,
     NumRadioControls,
     NumPushControls,
     NumToggleControls,
     NumSpinControls,
     NumListControls,
     NumDropDownListControls,
     NumComboControls,
     NumLabelControls,
     NumEditControls,
     NumGroupControls,
     NumMultiPageControls
    };

    controlAddr = addrArray
    {
     ADR(CheckBoxControls),
     ADR(RadioControls),
     ADR(PushControls),
     ADR(ToggleControls),
     ADR(SpinControls),
     ADR(ListControls),
     NIL(*ADR(DropDownListControls)*),
     ADR(ComboControls),
     ADR(LabelControls),
     ADR(EditControls),
     ADR(GroupControls),
     ADR(MultiPageControls)
    };

VAR
    retVal      : CARDINAL;
    controls    : POINTER TO ARRAY OF CONTROL;
    save        : ControlIdModes;
    text        : ARRAY [0..255] OF CHAR;
    data        : DialogDataRec;
BEGIN
    IF prop.idNum = MAX(CARDINAL) THEN
        AssignStringData(prop.tipTextPtr, text);
        IF PromptString("Form Title"(*PromptEditTitle*), text) THEN
            DisposeStringData(prop.tipTextPtr);
            prop.tipTextPtr := CreateStringData(text);
            RETURN TRUE;
        END;
    ELSE
        NEW(controls, NumCommonControls + propCount[prop.controlType] - 1);
        MoveMem(ADR(controls^[0]), ADR(CommonControls), SIZE(CommonControls));
        IF propCount[prop.controlType] <> 0 THEN
            MoveMem(ADR(controls^[NumCommonControls]),
                    controlAddr[prop.controlType],
                    propCount[prop.controlType] * SIZE(CONTROL));
        END;

        data.selected := 0;
        data.prop := ADR(prop);
        controls^[NumCommonControls-1].userData := ADR(data);

        save := GetControlIdMode();
        SetControlIdMode(ControlId);

        retVal := CallDialog(w,
                             dialogNames[prop.controlType],
                             controls^,
                             PropNotify,
                             CenterOnParent);

        SetControlIdMode(save);

        DISPOSE(controls);

        RETURN retVal = OkButton;
    END;
    RETURN FALSE;
END BasicPropertiesProc;

PROCEDURE BasicNewControlProc(w : Window; VAR INOUT prop : ControlProperties);
BEGIN
    UNREFERENCED_PARAMETER(w);
    UNREFERENCED_PARAMETER(prop);
END BasicNewControlProc;

PROCEDURE BasicEndEditProc(w : Window);
VAR
    ok  : BOOLEAN;
BEGIN
    ok := EndFormEditMode(w);
END BasicEndEditProc;

END FormEditUI.
