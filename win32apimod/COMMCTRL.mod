UNSAFEGUARDED IMPLEMENTATION MODULE COMMCTRL;

FROM SYSTEM IMPORT
    CAST, ADR, MAKEADR, UNREFERENCED_PARAMETER, FUNC, ADDADR;

FROM WINX IMPORT
    NIL_STR, NIL_ASTR, NIL_USTR;

FROM WINUSER IMPORT
    MSG, LPNMHDR, WM_NOTIFY, SendMessage, SendMessageA, SendMessageW, PostMessage, PostMessageA, PostMessageW,
    IMAGE_BITMAP, MAKELPARAM, CreateWindow, CreateWindowA, CreateWindowW, MAKELONG, LOWORD, HIWORD;

FROM WIN32 IMPORT
    TCHAR, WCHAR, UINT, WINT, PINT, SHORT, LONG, WPARAM, LPARAM, HWND, HICON, HCURSOR, HFONT, BYTE,
    DWORD, HINSTANCE, BOOL, COLORREF, POINT, RECT, HMENU, WORD, SYSTEMTIME, LRESULT, LPVOID, WSIZE;

FROM WINNLS IMPORT
	CALID;

FROM OOle2 IMPORT
    IUnknown;

<*/CALLS:StonyBrook*>
<*/NOWARN:I*>
<*/NOWARN:F*>
<*/NOHIGH*>

(*MACROS*)

PROCEDURE HANDLE_WM_NOTIFY(hwnd : HWND;
                           wParam : WPARAM;
                           lParam : LPARAM;
                           fn : HANDLE_PROC) : WINT;
BEGIN
    RETURN fn(hwnd, wParam, CAST(LPNMHDR, lParam));
END HANDLE_WM_NOTIFY;

PROCEDURE FORWARD_WM_NOTIFY(hwnd : HWND;
                            idFrom : WINT;
                            pnmhdr : LPNMHDR;
                            fn : FORWARD_PROC) : WINT;
BEGIN
    RETURN fn(hwnd, WM_NOTIFY, VAL(WPARAM, idFrom), CAST(LPARAM, pnmhdr));
END FORWARD_WM_NOTIFY;


PROCEDURE ImageList_AddIcon(himl : HIMAGELIST;
                                hicon : HICON) : WINT;
BEGIN
    RETURN ImageList_ReplaceIcon(himl, -1, hicon);
END ImageList_AddIcon;

PROCEDURE INDEXTOOVERLAYMASK(i : DWORD) : DWORD;
BEGIN
   RETURN i SHL 8;
END INDEXTOOVERLAYMASK;

PROCEDURE ImageList_RemoveAll(himl : HIMAGELIST) : BOOL;
BEGIN
    RETURN ImageList_Remove(himl, -1);
END ImageList_RemoveAll;

PROCEDURE ImageList_ExtractIcon(hi : HINSTANCE;
                                himl : HIMAGELIST;
                                i : WINT) : HICON;
BEGIN
    UNREFERENCED_PARAMETER(hi);
    RETURN ImageList_GetIcon(himl, i, 0);
END ImageList_ExtractIcon;

PROCEDURE ImageList_LoadBitmap(hi : HINSTANCE;
                               %IF UNICODE %THEN
                                   lpbmp : ARRAY OF UCHAR;
                               %ELSE
                                   lpbmp : ARRAY OF ACHAR;
                               %END
                               cx : WINT;
                               cGrow : WINT;
                               crMask : UINT) : HIMAGELIST;
BEGIN
    RETURN ImageList_LoadImage(hi, lpbmp, cx, cGrow, crMask, IMAGE_BITMAP, 0);
END ImageList_LoadBitmap;


PROCEDURE Header_GetItemCount(hwndHD : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwndHD, HDM_GETITEMCOUNT, 0, 0);
END Header_GetItemCount;

PROCEDURE Header_GetItemCountA(hwndHD : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwndHD, HDM_GETITEMCOUNT, 0, 0);
END Header_GetItemCountA;

PROCEDURE Header_GetItemCountW(hwndHD : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwndHD, HDM_GETITEMCOUNT, 0, 0);
END Header_GetItemCountW;

PROCEDURE Header_InsertItem(hwndHD : HWND;
                            i : WINT;
                            phdi : HD_ITEM) : WINT;
BEGIN
    RETURN SendMessage(hwndHD, HDM_INSERTITEM, i, CAST(LPARAM, ADR(phdi)) );
END Header_InsertItem;

PROCEDURE Header_InsertItemA(hwndHD : HWND;
                            i : WINT;
                            phdi : HD_ITEMA) : WINT;
BEGIN
    RETURN SendMessageA(hwndHD, HDM_INSERTITEMA, i, CAST(LPARAM, ADR(phdi)) );
END Header_InsertItemA;

PROCEDURE Header_InsertItemW(hwndHD : HWND;
                            i : WINT;
                            phdi : HD_ITEMW) : WINT;
BEGIN
    RETURN SendMessageW(hwndHD, HDM_INSERTITEMW, i, CAST(LPARAM, ADR(phdi)) );
END Header_InsertItemW;

PROCEDURE Header_DeleteItem(hwndHD : HWND; i : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndHD, HDM_DELETEITEM, i, 0));
END Header_DeleteItem;

PROCEDURE Header_DeleteItemA(hwndHD : HWND; i : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndHD, HDM_DELETEITEM, i, 0));
END Header_DeleteItemA;

PROCEDURE Header_DeleteItemW(hwndHD : HWND; i : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndHD, HDM_DELETEITEM, i, 0));
END Header_DeleteItemW;

PROCEDURE Header_GetItem(hwndHD : HWND;
                         i : WINT;
                         VAR OUT phdi : HD_ITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndHD, HDM_GETITEM, VAL(WPARAM, i),
                                 CAST(LPARAM, ADR(phdi)) ));
END Header_GetItem;

PROCEDURE Header_GetItemA(hwndHD : HWND;
                         i : WINT;
                         VAR OUT phdi : HD_ITEMA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndHD, HDM_GETITEMA, VAL(WPARAM, i),
                                 CAST(LPARAM, ADR(phdi)) ));
END Header_GetItemA;

PROCEDURE Header_GetItemW(hwndHD : HWND;
                         i : WINT;
                         VAR OUT phdi : HD_ITEMW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndHD, HDM_GETITEMW, VAL(WPARAM, i),
                                 CAST(LPARAM, ADR(phdi)) ));
END Header_GetItemW;

PROCEDURE Header_SetItem(hwndHD : HWND;
                         i : WINT;
                         phdi : HD_ITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndHD,
                                  HDM_SETITEM,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(phdi)) )
               );
END Header_SetItem;

PROCEDURE Header_SetItemA(hwndHD : HWND;
                         i : WINT;
                         phdi : HD_ITEMA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndHD,
                                  HDM_SETITEMA,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(phdi)) )
               );
END Header_SetItemA;

PROCEDURE Header_SetItemW(hwndHD : HWND;
                         i : WINT;
                         phdi : HD_ITEMW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndHD,
                                  HDM_SETITEMW,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(phdi)) )
               );
END Header_SetItemW;

PROCEDURE Header_Layout(hwndHD : HWND; VAR playout : HD_LAYOUT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndHD, HDM_LAYOUT, 0,
                    CAST(LPARAM, ADR(playout)) ));
END Header_Layout;

PROCEDURE Header_LayoutA(hwndHD : HWND; VAR playout : HD_LAYOUT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndHD, HDM_LAYOUT, 0,
                    CAST(LPARAM, ADR(playout)) ));
END Header_LayoutA;

PROCEDURE Header_LayoutW(hwndHD : HWND; VAR playout : HD_LAYOUT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndHD, HDM_LAYOUT, 0,
                    CAST(LPARAM, ADR(playout)) ));
END Header_LayoutW;

PROCEDURE Header_GetItemRect(hwnd : HWND;
                             iItem : WINT;
                             VAR OUT lprc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  HDM_GETITEMRECT,
                                  iItem,
                                  CAST(LPARAM, ADR(lprc))));
END Header_GetItemRect;

PROCEDURE Header_GetItemRectA(hwnd : HWND;
                             iItem : WINT;
                             VAR OUT lprc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  HDM_GETITEMRECT,
                                  iItem,
                                  CAST(LPARAM, ADR(lprc))));
END Header_GetItemRectA;

PROCEDURE Header_GetItemRectW(hwnd : HWND;
                             iItem : WINT;
                             VAR OUT lprc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  HDM_GETITEMRECT,
                                  iItem,
                                  CAST(LPARAM, ADR(lprc))));
END Header_GetItemRectW;

PROCEDURE Header_SetImageList(hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd,
                                        HDM_SETIMAGELIST,
                                        HDSIL_NORMAL, CAST(LPARAM, himl)));
END Header_SetImageList;

PROCEDURE Header_SetImageListA(hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd,
                                        HDM_SETIMAGELIST,
                                        HDSIL_NORMAL, CAST(LPARAM, himl)));
END Header_SetImageListA;

PROCEDURE Header_SetImageListW(hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd,
                                        HDM_SETIMAGELIST,
                                        HDSIL_NORMAL, CAST(LPARAM, himl)));
END Header_SetImageListW;

PROCEDURE Header_SetStateImageList(hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd,
                                        HDM_SETIMAGELIST,
                                        HDSIL_STATE, CAST(LPARAM, himl)));
END Header_SetStateImageList;

PROCEDURE Header_SetStateImageListA(hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd,
                                        HDM_SETIMAGELIST,
                                        HDSIL_STATE, CAST(LPARAM, himl)));
END Header_SetStateImageListA;

PROCEDURE Header_SetStateImageListW(hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd,
                                        HDM_SETIMAGELIST,
                                        HDSIL_STATE, CAST(LPARAM, himl)));
END Header_SetStateImageListW;

PROCEDURE Header_GetImageList(hwnd : HWND) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd, HDM_GETIMAGELIST, HDSIL_NORMAL, 0));
END Header_GetImageList;

PROCEDURE Header_GetImageListA(hwnd : HWND) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd, HDM_GETIMAGELIST, HDSIL_NORMAL, 0));
END Header_GetImageListA;

PROCEDURE Header_GetImageListW(hwnd : HWND) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd, HDM_GETIMAGELIST, HDSIL_NORMAL, 0));
END Header_GetImageListW;

PROCEDURE Header_GetStateImageList(hwnd : HWND) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd, HDM_GETIMAGELIST, HDSIL_STATE, 0));
END Header_GetStateImageList;

PROCEDURE Header_GetStateImageListA(hwnd : HWND) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd, HDM_GETIMAGELIST, HDSIL_STATE, 0));
END Header_GetStateImageListA;

PROCEDURE Header_GetStateImageListW(hwnd : HWND) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd, HDM_GETIMAGELIST, HDSIL_STATE, 0));
END Header_GetStateImageListW;

PROCEDURE Header_OrderToIndex(hwnd : HWND; i : WINT) : WINT;
BEGIN
    RETURN SendMessage(hwnd, HDM_ORDERTOINDEX, i, 0);
END Header_OrderToIndex;

PROCEDURE Header_OrderToIndexA(hwnd : HWND; i : WINT) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, HDM_ORDERTOINDEX, i, 0);
END Header_OrderToIndexA;

PROCEDURE Header_OrderToIndexW(hwnd : HWND; i : WINT) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, HDM_ORDERTOINDEX, i, 0);
END Header_OrderToIndexW;

PROCEDURE Header_CreateDragImage(hwnd : HWND; i : WINT) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd,
                                        HDM_CREATEDRAGIMAGE,
                                        i, 0));
END Header_CreateDragImage;

PROCEDURE Header_CreateDragImageA(hwnd : HWND; i : WINT) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd,
                                        HDM_CREATEDRAGIMAGE,
                                        i, 0));
END Header_CreateDragImageA;

PROCEDURE Header_CreateDragImageW(hwnd : HWND; i : WINT) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd,
                                        HDM_CREATEDRAGIMAGE,
                                        i, 0));
END Header_CreateDragImageW;

PROCEDURE Header_GetOrderArray(hwnd : HWND;
                               iCount : WINT;
                               VAR lpi : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  HDM_GETORDERARRAY,
                                  iCount, CAST(LPARAM, ADR(lpi))));
END Header_GetOrderArray;

PROCEDURE Header_GetOrderArrayA(hwnd : HWND;
                               iCount : WINT;
                               VAR lpi : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  HDM_GETORDERARRAY,
                                  iCount, CAST(LPARAM, ADR(lpi))));
END Header_GetOrderArrayA;

PROCEDURE Header_GetOrderArrayW(hwnd : HWND;
                               iCount : WINT;
                               VAR lpi : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  HDM_GETORDERARRAY,
                                  iCount, CAST(LPARAM, ADR(lpi))));
END Header_GetOrderArrayW;

PROCEDURE Header_SetOrderArray(hwnd : HWND;
                               iCount : WINT;
                               VAR lpi : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  HDM_SETORDERARRAY,
                                  iCount,
                                  CAST(LPARAM, ADR(lpi))));
END Header_SetOrderArray;

PROCEDURE Header_SetOrderArrayA(hwnd : HWND;
                               iCount : WINT;
                               VAR lpi : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  HDM_SETORDERARRAY,
                                  iCount,
                                  CAST(LPARAM, ADR(lpi))));
END Header_SetOrderArrayA;

PROCEDURE Header_SetOrderArrayW(hwnd : HWND;
                               iCount : WINT;
                               VAR lpi : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  HDM_SETORDERARRAY,
                                  iCount,
                                  CAST(LPARAM, ADR(lpi))));
END Header_SetOrderArrayW;

PROCEDURE Header_SetHotDivider(hwnd : HWND;
                               fPos : BOOL;
                               dw : DWORD) : WINT;
BEGIN
    RETURN SendMessage(hwnd, HDM_SETHOTDIVIDER, ORD(fPos), dw);
END Header_SetHotDivider;

PROCEDURE Header_SetHotDividerA(hwnd : HWND;
                               fPos : BOOL;
                               dw : DWORD) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, HDM_SETHOTDIVIDER, ORD(fPos), dw);
END Header_SetHotDividerA;

PROCEDURE Header_SetHotDividerW(hwnd : HWND;
                               fPos : BOOL;
                               dw : DWORD) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, HDM_SETHOTDIVIDER, ORD(fPos), dw);
END Header_SetHotDividerW;

PROCEDURE Header_SetBitmapMargin(hwnd : HWND; iWidth : WINT) : WINT;
BEGIN
	RETURN SendMessage (hwnd, HDM_SETBITMAPMARGIN, iWidth, 0);
END Header_SetBitmapMargin;

PROCEDURE Header_SetBitmapMarginA(hwnd : HWND; iWidth : WINT) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, HDM_SETBITMAPMARGIN, iWidth, 0);
END Header_SetBitmapMarginA;

PROCEDURE Header_SetBitmapMarginW(hwnd : HWND; iWidth : WINT) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, HDM_SETBITMAPMARGIN, iWidth, 0);
END Header_SetBitmapMarginW;

PROCEDURE Header_GetBitmapMargin(hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessage (hwnd, HDM_GETBITMAPMARGIN, 0, 0);
END Header_GetBitmapMargin;

PROCEDURE Header_GetBitmapMarginA(hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, HDM_GETBITMAPMARGIN, 0, 0);
END Header_GetBitmapMarginA;

PROCEDURE Header_GetBitmapMarginW(hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, HDM_GETBITMAPMARGIN, 0, 0);
END Header_GetBitmapMarginW;

PROCEDURE Header_SetUnicodeFormat(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  HDM_SETUNICODEFORMAT,
                                  ORD(fUnicode), 0));
END Header_SetUnicodeFormat;

PROCEDURE Header_SetUnicodeFormatA(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  HDM_SETUNICODEFORMAT,
                                  ORD(fUnicode), 0));
END Header_SetUnicodeFormatA;

PROCEDURE Header_SetUnicodeFormatW(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  HDM_SETUNICODEFORMAT,
                                  ORD(fUnicode), 0));
END Header_SetUnicodeFormatW;

PROCEDURE Header_GetUnicodeFormat(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, HDM_GETUNICODEFORMAT, 0, 0));
END Header_GetUnicodeFormat;

PROCEDURE Header_GetUnicodeFormatA(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, HDM_GETUNICODEFORMAT, 0, 0));
END Header_GetUnicodeFormatA;

PROCEDURE Header_GetUnicodeFormatW(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, HDM_GETUNICODEFORMAT, 0, 0));
END Header_GetUnicodeFormatW;

PROCEDURE Header_SetFilterChangeTimeout(hwnd : HWND; i : WINT) : WINT;
BEGIN
	RETURN SendMessage (hwnd, HDM_SETFILTERCHANGETIMEOUT, 0, i);
END Header_SetFilterChangeTimeout;

PROCEDURE Header_SetFilterChangeTimeoutA(hwnd : HWND; i : WINT) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, HDM_SETFILTERCHANGETIMEOUT, 0, i);
END Header_SetFilterChangeTimeoutA;

PROCEDURE Header_SetFilterChangeTimeoutW(hwnd : HWND; i : WINT) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, HDM_SETFILTERCHANGETIMEOUT, 0, i);
END Header_SetFilterChangeTimeoutW;

PROCEDURE Header_EditFilter (hwnd : HWND; i : WINT; fDiscardChanges : WINT) : WINT;
BEGIN
	RETURN SendMessage (hwnd, HDM_EDITFILTER, i, MAKELPARAM(fDiscardChanges,0));
END Header_EditFilter;

PROCEDURE Header_EditFilterA (hwnd : HWND; i : WINT; fDiscardChanges : WINT) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, HDM_EDITFILTER, i, MAKELPARAM(fDiscardChanges,0));
END Header_EditFilterA;

PROCEDURE Header_EditFilterW (hwnd : HWND; i : WINT; fDiscardChanges : WINT) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, HDM_EDITFILTER, i, MAKELPARAM(fDiscardChanges,0));
END Header_EditFilterW;

PROCEDURE Header_ClearFilter (hwnd : HWND; i : WINT) : WINT;
BEGIN
	RETURN SendMessage (hwnd, HDM_CLEARFILTER, i, 0);
END Header_ClearFilter;

PROCEDURE Header_ClearFilterA (hwnd : HWND; i : WINT) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, HDM_CLEARFILTER, i, 0);
END Header_ClearFilterA;

PROCEDURE Header_ClearFilterW (hwnd : HWND; i : WINT) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, HDM_CLEARFILTER, i, 0);
END Header_ClearFilterW;

PROCEDURE Header_ClearAllFilters (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessage (hwnd, HDM_CLEARFILTER, CAST(WPARAM,VAL(LPARAM,-1)), 0);
END Header_ClearAllFilters;
(* Clear filter takes -1 as a column value to indicate that all *)
(* the filter should be cleared.  When this happens you will *)
(* only receive a single filter changed notification. *)

PROCEDURE Header_ClearAllFiltersA (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, HDM_CLEARFILTER, CAST(WPARAM,VAL(LPARAM,-1)), 0);
END Header_ClearAllFiltersA;

PROCEDURE Header_ClearAllFiltersW (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, HDM_CLEARFILTER, CAST(WPARAM,VAL(LPARAM,-1)), 0);
END Header_ClearAllFiltersW;

PROCEDURE Header_GetItemDropDownRect (hwnd : HWND; iItem : WINT; VAR lpItemRect : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, HDM_GETITEMDROPDOWNRECT, iItem, CAST(LPARAM,ADR(lpItemRect))));
END Header_GetItemDropDownRect;

PROCEDURE Header_GetItemDropDownRectA (hwnd : HWND; iItem : WINT; VAR lpItemRect : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, HDM_GETITEMDROPDOWNRECT, iItem, CAST(LPARAM,ADR(lpItemRect))));
END Header_GetItemDropDownRectA;

PROCEDURE Header_GetItemDropDownRectW (hwnd : HWND; iItem : WINT; VAR lpItemRect : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, HDM_GETITEMDROPDOWNRECT, iItem, CAST(LPARAM,ADR(lpItemRect))));
END Header_GetItemDropDownRectW;

PROCEDURE Header_GetOverflowRect (hwnd : HWND; VAR lpItemRect : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, HDM_GETOVERFLOWRECT, 0, CAST(LPARAM,ADR(lpItemRect))));
END Header_GetOverflowRect;

PROCEDURE Header_GetOverflowRectA (hwnd : HWND; VAR lpItemRect : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, HDM_GETOVERFLOWRECT, 0, CAST(LPARAM,ADR(lpItemRect))));
END Header_GetOverflowRectA;

PROCEDURE Header_GetOverflowRectW (hwnd : HWND; VAR lpItemRect : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, HDM_GETOVERFLOWRECT, 0, CAST(LPARAM,ADR(lpItemRect))));
END Header_GetOverflowRectW;

PROCEDURE Header_GetFocusedItem (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessage (hwnd, HDM_GETFOCUSEDITEM, 0, 0);
END Header_GetFocusedItem;

PROCEDURE Header_GetFocusedItemA (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, HDM_GETFOCUSEDITEM, 0, 0);
END Header_GetFocusedItemA;

PROCEDURE Header_GetFocusedItemW (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, HDM_GETFOCUSEDITEM, 0, 0);
END Header_GetFocusedItemW;

PROCEDURE Header_SetFocusedItem (hwnd : HWND; iItem : WINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, HDM_SETFOCUSEDITEM, 0, iItem));
END Header_SetFocusedItem;

PROCEDURE Header_SetFocusedItemA (hwnd : HWND; iItem : WINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, HDM_SETFOCUSEDITEM, 0, iItem));
END Header_SetFocusedItemA;

PROCEDURE Header_SetFocusedItemW (hwnd : HWND; iItem : WINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, HDM_SETFOCUSEDITEM, 0, iItem));
END Header_SetFocusedItemW;

(* ListView *)
PROCEDURE ListView_SetUnicodeFormat(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_SETUNICODEFORMAT,
                                  ORD(fUnicode), 0));
END ListView_SetUnicodeFormat;

PROCEDURE ListView_SetUnicodeFormatA(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_SETUNICODEFORMAT,
                                  ORD(fUnicode), 0));
END ListView_SetUnicodeFormatA;

PROCEDURE ListView_SetUnicodeFormatW(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_SETUNICODEFORMAT,
                                  ORD(fUnicode), 0));
END ListView_SetUnicodeFormatW;

PROCEDURE ListView_GetUnicodeFormat(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_GETUNICODEFORMAT, 0, 0));
END ListView_GetUnicodeFormat;

PROCEDURE ListView_GetUnicodeFormatA(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_GETUNICODEFORMAT, 0, 0));
END ListView_GetUnicodeFormatA;

PROCEDURE ListView_GetUnicodeFormatW(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_GETUNICODEFORMAT, 0, 0));
END ListView_GetUnicodeFormatW;

PROCEDURE ListView_GetBkColor(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd, LVM_GETBKCOLOR, 0, 0));
END ListView_GetBkColor;

PROCEDURE ListView_GetBkColorA(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd, LVM_GETBKCOLOR, 0, 0));
END ListView_GetBkColorA;

PROCEDURE ListView_GetBkColorW(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd, LVM_GETBKCOLOR, 0, 0));
END ListView_GetBkColorW;

PROCEDURE ListView_SetBkColor(hwnd : HWND; clrBk : COLORREF) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_SETBKCOLOR,
                                  0,
                                  VAL(LPARAM, clrBk)
                                  )
               );
END ListView_SetBkColor;

PROCEDURE ListView_SetBkColorA(hwnd : HWND; clrBk : COLORREF) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_SETBKCOLOR,
                                  0,
                                  VAL(LPARAM, clrBk)
                                  )
               );
END ListView_SetBkColorA;

PROCEDURE ListView_SetBkColorW(hwnd : HWND; clrBk : COLORREF) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_SETBKCOLOR,
                                  0,
                                  VAL(LPARAM, clrBk)
                                  )
               );
END ListView_SetBkColorW;

PROCEDURE ListView_GetImageList(hwnd : HWND; iImageList : WINT) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd,
                                        LVM_GETIMAGELIST,
                                        VAL(WPARAM, iImageList),
                                        0
                                        )
               );
END ListView_GetImageList;

PROCEDURE ListView_GetImageListA(hwnd : HWND; iImageList : WINT) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd,
                                        LVM_GETIMAGELIST,
                                        VAL(WPARAM, iImageList),
                                        0
                                        )
               );
END ListView_GetImageListA;

PROCEDURE ListView_GetImageListW(hwnd : HWND; iImageList : WINT) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd,
                                        LVM_GETIMAGELIST,
                                        VAL(WPARAM, iImageList),
                                        0
                                        )
               );
END ListView_GetImageListW;

PROCEDURE ListView_SetImageList(hwnd : HWND;
                                himl : HIMAGELIST;
                                iImageList : WINT) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd, LVM_SETIMAGELIST,
                                        VAL(WPARAM, iImageList),
                                        CAST(LPARAM, himl) ));
END ListView_SetImageList;

PROCEDURE ListView_SetImageListA(hwnd : HWND;
                                himl : HIMAGELIST;
                                iImageList : WINT) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd, LVM_SETIMAGELIST,
                                        VAL(WPARAM, iImageList),
                                        CAST(LPARAM, himl) ));
END ListView_SetImageListA;

PROCEDURE ListView_SetImageListW(hwnd : HWND;
                                himl : HIMAGELIST;
                                iImageList : WINT) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd, LVM_SETIMAGELIST,
                                        VAL(WPARAM, iImageList),
                                        CAST(LPARAM, himl) ));
END ListView_SetImageListW;

PROCEDURE ListView_GetItemCount(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwnd, LVM_GETITEMCOUNT, 0, 0);
END ListView_GetItemCount;

PROCEDURE ListView_GetItemCountA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, LVM_GETITEMCOUNT, 0, 0);
END ListView_GetItemCountA;

PROCEDURE ListView_GetItemCountW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, LVM_GETITEMCOUNT, 0, 0);
END ListView_GetItemCountW;

PROCEDURE INDEXTOSTATEIMAGEMASK(i : DWORD) : DWORD;
BEGIN
    RETURN i SHL 12;
END INDEXTOSTATEIMAGEMASK;

PROCEDURE ListView_GetItem(hwnd : HWND; VAR OUT pitem : LV_ITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, LVM_GETITEM, 0, CAST(LPARAM, ADR(pitem))));
END ListView_GetItem;

PROCEDURE ListView_GetItemA(hwnd : HWND; VAR OUT pitem : LV_ITEMA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, LVM_GETITEMA, 0, CAST(LPARAM, ADR(pitem))));
END ListView_GetItemA;

PROCEDURE ListView_GetItemW(hwnd : HWND; VAR OUT pitem : LV_ITEMW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, LVM_GETITEMW, 0, CAST(LPARAM, ADR(pitem))));
END ListView_GetItemW;

PROCEDURE ListView_SetItem(hwnd : HWND; pitem : LV_ITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_SETITEM, 0,
                                  CAST(LPARAM, ADR(pitem))
                                  )
               );
END ListView_SetItem;

PROCEDURE ListView_SetItemA(hwnd : HWND; pitem : LV_ITEMA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_SETITEMA, 0,
                                  CAST(LPARAM, ADR(pitem))
                                  )
               );
END ListView_SetItemA;

PROCEDURE ListView_SetItemW(hwnd : HWND; pitem : LV_ITEMW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_SETITEMW, 0,
                                  CAST(LPARAM, ADR(pitem))
                                  )
               );
END ListView_SetItemW;

PROCEDURE ListView_InsertItem(hwnd : HWND; pitem : LV_ITEM) : WINT;
BEGIN
    RETURN SendMessage(hwnd, LVM_INSERTITEM, 0, CAST(LPARAM, ADR(pitem)) );
END ListView_InsertItem;

PROCEDURE ListView_InsertItemA(hwnd : HWND; pitem : LV_ITEMA) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, LVM_INSERTITEMA, 0, CAST(LPARAM, ADR(pitem)) );
END ListView_InsertItemA;

PROCEDURE ListView_InsertItemW(hwnd : HWND; pitem : LV_ITEMW) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, LVM_INSERTITEMW, 0, CAST(LPARAM, ADR(pitem)) );
END ListView_InsertItemW;

PROCEDURE ListView_DeleteItem(hwnd : HWND; i : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, LVM_DELETEITEM, VAL(WPARAM, i), 0));
END ListView_DeleteItem;

PROCEDURE ListView_DeleteItemA(hwnd : HWND; i : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, LVM_DELETEITEM, VAL(WPARAM, i), 0));
END ListView_DeleteItemA;

PROCEDURE ListView_DeleteItemW(hwnd : HWND; i : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, LVM_DELETEITEM, VAL(WPARAM, i), 0));
END ListView_DeleteItemW;

PROCEDURE ListView_DeleteAllItems(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, LVM_DELETEALLITEMS, 0, 0));
END ListView_DeleteAllItems;

PROCEDURE ListView_DeleteAllItemsA(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, LVM_DELETEALLITEMS, 0, 0));
END ListView_DeleteAllItemsA;

PROCEDURE ListView_DeleteAllItemsW(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, LVM_DELETEALLITEMS, 0, 0));
END ListView_DeleteAllItemsW;

(*#CSB UINT was BOOL in C header, docs say UINT which is better for mask*)
PROCEDURE ListView_GetCallbackMask(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, LVM_GETCALLBACKMASK, 0, 0));
END ListView_GetCallbackMask;

PROCEDURE ListView_GetCallbackMaskA(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageA(hwnd, LVM_GETCALLBACKMASK, 0, 0));
END ListView_GetCallbackMaskA;

PROCEDURE ListView_GetCallbackMaskW(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageW(hwnd, LVM_GETCALLBACKMASK, 0, 0));
END ListView_GetCallbackMaskW;

PROCEDURE ListView_SetCallbackMask(hwnd : HWND; mask : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, LVM_SETCALLBACKMASK, mask, 0) );
END ListView_SetCallbackMask;

PROCEDURE ListView_SetCallbackMaskA(hwnd : HWND; mask : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, LVM_SETCALLBACKMASK, mask, 0) );
END ListView_SetCallbackMaskA;

PROCEDURE ListView_SetCallbackMaskW(hwnd : HWND; mask : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, LVM_SETCALLBACKMASK, mask, 0) );
END ListView_SetCallbackMaskW;

PROCEDURE ListView_GetNextItem(hwnd : HWND; i : WINT; flags : WORD) : WINT;
BEGIN
    RETURN SendMessage(hwnd, LVM_GETNEXTITEM, VAL(WPARAM, i),
                       MAKELPARAM(flags, 0));
END ListView_GetNextItem;

PROCEDURE ListView_GetNextItemA(hwnd : HWND; i : WINT; flags : WORD) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, LVM_GETNEXTITEM, VAL(WPARAM, i),
                       MAKELPARAM(flags, 0));
END ListView_GetNextItemA;

PROCEDURE ListView_GetNextItemW(hwnd : HWND; i : WINT; flags : WORD) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, LVM_GETNEXTITEM, VAL(WPARAM, i),
                       MAKELPARAM(flags, 0));
END ListView_GetNextItemW;

PROCEDURE ListView_FindItem(hwnd : HWND; iStart : WINT; plvfi : LV_FINDINFO) : WINT;
BEGIN
    RETURN SendMessage(hwnd, LVM_FINDITEM, VAL(WPARAM, iStart),
                             CAST(LPARAM, ADR(plvfi)) );
END ListView_FindItem;

PROCEDURE ListView_FindItemA(hwnd : HWND; iStart : WINT; plvfi : LV_FINDINFOA) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, LVM_FINDITEMA, VAL(WPARAM, iStart),
                             CAST(LPARAM, ADR(plvfi)) );
END ListView_FindItemA;

PROCEDURE ListView_FindItemW(hwnd : HWND; iStart : WINT; plvfi : LV_FINDINFOW) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, LVM_FINDITEMW, VAL(WPARAM, iStart),
                             CAST(LPARAM, ADR(plvfi)) );
END ListView_FindItemW;

PROCEDURE ListView_GetItemRect(hwnd : HWND;
                               i : WINT;
                               VAR prc : RECT;
                               code : DWORD) : BOOL;
BEGIN
    prc.left := code;
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_GETITEMRECT,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(prc))
                                  )
                );
END ListView_GetItemRect;

PROCEDURE ListView_GetItemRectA(hwnd : HWND;
                               i : WINT;
                               VAR prc : RECT;
                               code : DWORD) : BOOL;
BEGIN
    prc.left := code;
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_GETITEMRECT,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(prc))
                                  )
                );
END ListView_GetItemRectA;

PROCEDURE ListView_GetItemRectW(hwnd : HWND;
                               i : WINT;
                               VAR prc : RECT;
                               code : DWORD) : BOOL;
BEGIN
    prc.left := code;
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_GETITEMRECT,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(prc))
                                  )
                );
END ListView_GetItemRectW;

PROCEDURE ListView_SetItemPosition(hwndLV : HWND;
                                   i : WINT;
                                   x : WORD;
                                   y : WORD) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV,
                                  LVM_SETITEMPOSITION,
                                  VAL(WPARAM, i),
                                  MAKELPARAM(x, y)
                                  )
                );
END ListView_SetItemPosition;

PROCEDURE ListView_SetItemPositionA(hwndLV : HWND;
                                   i : WINT;
                                   x : WORD;
                                   y : WORD) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV,
                                  LVM_SETITEMPOSITION,
                                  VAL(WPARAM, i),
                                  MAKELPARAM(x, y)
                                  )
                );
END ListView_SetItemPositionA;

PROCEDURE ListView_SetItemPositionW(hwndLV : HWND;
                                   i : WINT;
                                   x : WORD;
                                   y : WORD) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV,
                                  LVM_SETITEMPOSITION,
                                  VAL(WPARAM, i),
                                  MAKELPARAM(x, y)
                                  )
                );
END ListView_SetItemPositionW;

PROCEDURE ListView_GetItemPosition(hwndLV : HWND;
                                   i : WINT;
                                   VAR ppt : POINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV,
                                  LVM_GETITEMPOSITION,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(ppt))
                                  )
                );
END ListView_GetItemPosition;

PROCEDURE ListView_GetItemPositionA(hwndLV : HWND;
                                   i : WINT;
                                   VAR ppt : POINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV,
                                  LVM_GETITEMPOSITION,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(ppt))
                                  )
                );
END ListView_GetItemPositionA;

PROCEDURE ListView_GetItemPositionW(hwndLV : HWND;
                                   i : WINT;
                                   VAR ppt : POINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV,
                                  LVM_GETITEMPOSITION,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(ppt))
                                  )
                );
END ListView_GetItemPositionW;

PROCEDURE ListView_GetStringWidth(hwndLV : HWND;
                                  psz : ARRAY OF TCHAR) : WINT;
BEGIN
    RETURN SendMessage(hwndLV, LVM_GETSTRINGWIDTH, 0, CAST(LPARAM, ADR(psz)));
END ListView_GetStringWidth;

PROCEDURE ListView_GetStringWidthA(hwndLV : HWND;
                                  psz : ARRAY OF ACHAR) : WINT;
BEGIN
    RETURN SendMessageA(hwndLV, LVM_GETSTRINGWIDTHA, 0, CAST(LPARAM, ADR(psz)));
END ListView_GetStringWidthA;

PROCEDURE ListView_GetStringWidthW(hwndLV : HWND;
                                   psz : ARRAY OF WCHAR) : WINT;
BEGIN
    RETURN SendMessageW(hwndLV, LVM_GETSTRINGWIDTHW, 0, CAST(LPARAM, ADR(psz)));
END ListView_GetStringWidthW;

PROCEDURE ListView_HitTest(hwndLV : HWND;
                           VAR pinfo : LV_HITTESTINFO) : WINT;
BEGIN
    RETURN SendMessage(hwndLV, LVM_HITTEST, 0, CAST(LPARAM, ADR(pinfo)) );
END ListView_HitTest;

PROCEDURE ListView_HitTestA(hwndLV : HWND;
                           VAR pinfo : LV_HITTESTINFO) : WINT;
BEGIN
    RETURN SendMessageA(hwndLV, LVM_HITTEST, 0, CAST(LPARAM, ADR(pinfo)) );
END ListView_HitTestA;

PROCEDURE ListView_HitTestW(hwndLV : HWND;
                           VAR pinfo : LV_HITTESTINFO) : WINT;
BEGIN
    RETURN SendMessageW(hwndLV, LVM_HITTEST, 0, CAST(LPARAM, ADR(pinfo)) );
END ListView_HitTestW;

PROCEDURE ListView_HitTestEx(hwndLV : HWND;
                           VAR pinfo : LV_HITTESTINFO) : WINT;
BEGIN
    RETURN SendMessage(hwndLV, LVM_HITTEST, CAST(WPARAM,VAL(LPARAM,-1)), CAST(LPARAM, ADR(pinfo)) );
END ListView_HitTestEx;

PROCEDURE ListView_HitTestExA(hwndLV : HWND;
                           VAR pinfo : LV_HITTESTINFO) : WINT;
BEGIN
    RETURN SendMessageA(hwndLV, LVM_HITTEST, CAST(WPARAM,VAL(LPARAM,-1)), CAST(LPARAM, ADR(pinfo)) );
END ListView_HitTestExA;

PROCEDURE ListView_HitTestExW(hwndLV : HWND;
                           VAR pinfo : LV_HITTESTINFO) : WINT;
BEGIN
    RETURN SendMessageW(hwndLV, LVM_HITTEST, CAST(WPARAM,VAL(LPARAM,-1)), CAST(LPARAM, ADR(pinfo)) );
END ListView_HitTestExW;

PROCEDURE ListView_EnsureVisible(hwndLV : HWND; i : WINT; fPartialOK : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV, LVM_ENSUREVISIBLE,
                                 VAL(WPARAM, i), MAKELPARAM(VAL(WORD,fPartialOK), 0))
               );
END ListView_EnsureVisible;

PROCEDURE ListView_EnsureVisibleA(hwndLV : HWND; i : WINT; fPartialOK : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV, LVM_ENSUREVISIBLE,
                                 VAL(WPARAM, i), MAKELPARAM(VAL(WORD,fPartialOK), 0))
               );
END ListView_EnsureVisibleA;

PROCEDURE ListView_EnsureVisibleW(hwndLV : HWND; i : WINT; fPartialOK : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV, LVM_ENSUREVISIBLE,
                                 VAL(WPARAM, i), MAKELPARAM(VAL(WORD,fPartialOK), 0))
               );
END ListView_EnsureVisibleW;

PROCEDURE ListView_Scroll(hwndLV : HWND; dx : WINT; dy : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV, LVM_SCROLL,
                                  VAL(WPARAM, dx), dy)
               );
END ListView_Scroll;

PROCEDURE ListView_ScrollA(hwndLV : HWND; dx : WINT; dy : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV, LVM_SCROLL,
                                  VAL(WPARAM, dx), dy)
               );
END ListView_ScrollA;

PROCEDURE ListView_ScrollW(hwndLV : HWND; dx : WINT; dy : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV, LVM_SCROLL,
                                  VAL(WPARAM, dx), dy)
               );
END ListView_ScrollW;

PROCEDURE ListView_RedrawItems(hwndLV : HWND;
                               iFirst : WINT;
                               iLast : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV, LVM_REDRAWITEMS,
                                  VAL(WPARAM, iFirst), iLast)
               );
END ListView_RedrawItems;

PROCEDURE ListView_RedrawItemsA(hwndLV : HWND;
                               iFirst : WINT;
                               iLast : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV, LVM_REDRAWITEMS,
                                  VAL(WPARAM, iFirst), iLast)
               );
END ListView_RedrawItemsA;

PROCEDURE ListView_RedrawItemsW(hwndLV : HWND;
                               iFirst : WINT;
                               iLast : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV, LVM_REDRAWITEMS,
                                  VAL(WPARAM, iFirst), iLast)
               );
END ListView_RedrawItemsW;

PROCEDURE ListView_Arrange(hwndLV : HWND; code : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV, LVM_ARRANGE, code, 0));
END ListView_Arrange;

PROCEDURE ListView_ArrangeA(hwndLV : HWND; code : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV, LVM_ARRANGE, code, 0));
END ListView_ArrangeA;

PROCEDURE ListView_ArrangeW(hwndLV : HWND; code : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV, LVM_ARRANGE, code, 0));
END ListView_ArrangeW;

PROCEDURE ListView_EditLabel(hwndLV : HWND; i : WINT) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwndLV, LVM_EDITLABEL,
                                  VAL(WPARAM, i), 0)
               );
END ListView_EditLabel;

PROCEDURE ListView_EditLabelA(hwndLV : HWND; i : WINT) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwndLV, LVM_EDITLABELA,
                                  VAL(WPARAM, i), 0)
               );
END ListView_EditLabelA;

PROCEDURE ListView_EditLabelW(hwndLV : HWND; i : WINT) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwndLV, LVM_EDITLABELW,
                                  VAL(WPARAM, i), 0)
               );
END ListView_EditLabelW;

PROCEDURE ListView_GetEditControl(hwndLV : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwndLV, LVM_GETEDITCONTROL, 0, 0));
END ListView_GetEditControl;

PROCEDURE ListView_GetEditControlA(hwndLV : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwndLV, LVM_GETEDITCONTROL, 0, 0));
END ListView_GetEditControlA;

PROCEDURE ListView_GetEditControlW(hwndLV : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwndLV, LVM_GETEDITCONTROL, 0, 0));
END ListView_GetEditControlW;

PROCEDURE ListView_GetColumn(hwnd : HWND;
                             iCol : WINT;
                             VAR pcol : LV_COLUMN) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_GETCOLUMN,
                                  VAL(WPARAM, iCol),
                                  CAST(LPARAM, ADR(pcol))
                                  )
               );
END ListView_GetColumn;

PROCEDURE ListView_GetColumnA(hwnd : HWND;
                              iCol : WINT;
                              VAR pcol : LV_COLUMNA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_GETCOLUMNA,
                                  VAL(WPARAM, iCol),
                                  CAST(LPARAM, ADR(pcol))
                                  )
               );
END ListView_GetColumnA;

PROCEDURE ListView_GetColumnW(hwnd : HWND;
                              iCol : WINT;
                              VAR pcol : LV_COLUMNW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_GETCOLUMNW,
                                  VAL(WPARAM, iCol),
                                  CAST(LPARAM, ADR(pcol))
                                  )
               );
END ListView_GetColumnW;

PROCEDURE ListView_SetColumn(hwnd : HWND;
                             iCol : WINT;
                             pcol : LV_COLUMN) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, LVM_SETCOLUMN,
                                  VAL(WPARAM, iCol),
                                  CAST(LPARAM, ADR(pcol)))
               );
END ListView_SetColumn;

PROCEDURE ListView_SetColumnA(hwnd : HWND;
                              iCol : WINT;
                              pcol : LV_COLUMNA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, LVM_SETCOLUMNA,
                                  VAL(WPARAM, iCol),
                                  CAST(LPARAM, ADR(pcol)))
               );
END ListView_SetColumnA;

PROCEDURE ListView_SetColumnW(hwnd : HWND;
                              iCol : WINT;
                              pcol : LV_COLUMNW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, LVM_SETCOLUMNW,
                                  VAL(WPARAM, iCol),
                                  CAST(LPARAM, ADR(pcol)))
               );
END ListView_SetColumnW;

PROCEDURE ListView_InsertColumn(hwnd : HWND;
                                iCol : WINT;
                                pcol : LV_COLUMN) : WINT;
BEGIN
    RETURN SendMessage(hwnd, LVM_INSERTCOLUMN, VAL(WPARAM, iCol), CAST(LPARAM, ADR(pcol)));
END ListView_InsertColumn;

PROCEDURE ListView_InsertColumnA(hwnd : HWND;
                                iCol : WINT;
                                pcol : LV_COLUMNA) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, LVM_INSERTCOLUMNA, VAL(WPARAM, iCol), CAST(LPARAM, ADR(pcol)));
END ListView_InsertColumnA;

PROCEDURE ListView_InsertColumnW(hwnd : HWND;
                                iCol : WINT;
                                pcol : LV_COLUMNW) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, LVM_INSERTCOLUMNW, VAL(WPARAM, iCol), CAST(LPARAM, ADR(pcol)));
END ListView_InsertColumnW;

PROCEDURE ListView_DeleteColumn(hwnd : HWND; iCol : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_DELETECOLUMN,
                                  VAL(WPARAM, iCol),
                                  0
                                  )
                );
END ListView_DeleteColumn;

PROCEDURE ListView_DeleteColumnA(hwnd : HWND; iCol : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_DELETECOLUMN,
                                  VAL(WPARAM, iCol),
                                  0
                                  )
                );
END ListView_DeleteColumnA;

PROCEDURE ListView_DeleteColumnW(hwnd : HWND; iCol : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_DELETECOLUMN,
                                  VAL(WPARAM, iCol),
                                  0
                                  )
                );
END ListView_DeleteColumnW;

PROCEDURE ListView_GetColumnWidth(hwnd : HWND; iCol : WINT) : WINT;
BEGIN
    RETURN SendMessage(hwnd, LVM_GETCOLUMNWIDTH, VAL(WPARAM, iCol), 0);
END ListView_GetColumnWidth;

PROCEDURE ListView_GetColumnWidthA(hwnd : HWND; iCol : WINT) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, LVM_GETCOLUMNWIDTH, VAL(WPARAM, iCol), 0);
END ListView_GetColumnWidthA;

PROCEDURE ListView_GetColumnWidthW(hwnd : HWND; iCol : WINT) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, LVM_GETCOLUMNWIDTH, VAL(WPARAM, iCol), 0);
END ListView_GetColumnWidthW;

PROCEDURE ListView_SetColumnWidth(hwnd : HWND; iCol : WINT; cx : WINT) : BOOL;
VAR
    wcx         : INTEGER16;
BEGIN
    wcx := cx;
    RETURN CAST(BOOL, SendMessage(hwnd, LVM_SETCOLUMNWIDTH,
                                  VAL(WPARAM, iCol), MAKELPARAM(wcx, 0))
               );
END ListView_SetColumnWidth;

PROCEDURE ListView_SetColumnWidthA(hwnd : HWND; iCol : WINT; cx : WINT) : BOOL;
VAR
    wcx         : INTEGER16;
BEGIN
    wcx := cx;
    RETURN CAST(BOOL, SendMessageA(hwnd, LVM_SETCOLUMNWIDTH,
                                  VAL(WPARAM, iCol), MAKELPARAM(wcx, 0))
               );
END ListView_SetColumnWidthA;

PROCEDURE ListView_SetColumnWidthW(hwnd : HWND; iCol : WINT; cx : WINT) : BOOL;
VAR
    wcx         : INTEGER16;
BEGIN
    wcx := cx;
    RETURN CAST(BOOL, SendMessageW(hwnd, LVM_SETCOLUMNWIDTH,
                                  VAL(WPARAM, iCol), MAKELPARAM(wcx, 0))
               );
END ListView_SetColumnWidthW;

PROCEDURE ListView_GetHeader(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwnd, LVM_GETHEADER, 0, 0));
END ListView_GetHeader;

PROCEDURE ListView_GetHeaderA(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwnd, LVM_GETHEADER, 0, 0));
END ListView_GetHeaderA;

PROCEDURE ListView_GetHeaderW(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwnd, LVM_GETHEADER, 0, 0));
END ListView_GetHeaderW;

PROCEDURE ListView_CreateDragImage(hwnd : HWND;
                                   i : WINT;
                                   lpptUpLeft : POINT) : HIMAGELIST;
BEGIN
    RETURN  CAST(HIMAGELIST, SendMessage(hwnd, LVM_CREATEDRAGIMAGE,
                                         VAL(WPARAM, i),
                                         CAST(LPARAM, ADR(lpptUpLeft))
                                         )
                );
END ListView_CreateDragImage;

PROCEDURE ListView_CreateDragImageA(hwnd : HWND;
                                   i : WINT;
                                   lpptUpLeft : POINT) : HIMAGELIST;
BEGIN
    RETURN  CAST(HIMAGELIST, SendMessageA(hwnd, LVM_CREATEDRAGIMAGE,
                                         VAL(WPARAM, i),
                                         CAST(LPARAM, ADR(lpptUpLeft))
                                         )
                );
END ListView_CreateDragImageA;

PROCEDURE ListView_CreateDragImageW(hwnd : HWND;
                                   i : WINT;
                                   lpptUpLeft : POINT) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd, LVM_CREATEDRAGIMAGE,
                                         VAL(WPARAM, i),
                                         CAST(LPARAM, ADR(lpptUpLeft))
                                         )
                );
END ListView_CreateDragImageW;

PROCEDURE ListView_GetViewRect(hwnd : HWND; VAR prc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, LVM_GETVIEWRECT, 0,
                                  CAST(LPARAM, ADR(prc))
                                  )
               );
END ListView_GetViewRect;

PROCEDURE ListView_GetViewRectA(hwnd : HWND; VAR prc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, LVM_GETVIEWRECT, 0,
                                  CAST(LPARAM, ADR(prc))
                                  )
               );
END ListView_GetViewRectA;

PROCEDURE ListView_GetViewRectW(hwnd : HWND; VAR prc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, LVM_GETVIEWRECT, 0,
                                  CAST(LPARAM, ADR(prc))
                                  )
               );
END ListView_GetViewRectW;

PROCEDURE ListView_GetTextColor(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd, LVM_GETTEXTCOLOR, 0, 0) );
END ListView_GetTextColor;

PROCEDURE ListView_GetTextColorA(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd, LVM_GETTEXTCOLOR, 0, 0) );
END ListView_GetTextColorA;

PROCEDURE ListView_GetTextColorW(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd, LVM_GETTEXTCOLOR, 0, 0) );
END ListView_GetTextColorW;

PROCEDURE ListView_SetTextColor(hwnd : HWND; clrText : COLORREF) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, LVM_SETTEXTCOLOR, 0,
                                  VAL(LPARAM, clrText))
               );
END ListView_SetTextColor;

PROCEDURE ListView_SetTextColorA(hwnd : HWND; clrText : COLORREF) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, LVM_SETTEXTCOLOR, 0,
                                  VAL(LPARAM, clrText))
               );
END ListView_SetTextColorA;

PROCEDURE ListView_SetTextColorW(hwnd : HWND; clrText : COLORREF) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, LVM_SETTEXTCOLOR, 0,
                                  VAL(LPARAM, clrText))
               );
END ListView_SetTextColorW;

PROCEDURE ListView_GetTextBkColor(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd, LVM_GETTEXTBKCOLOR, 0, 0) );
END ListView_GetTextBkColor;

PROCEDURE ListView_GetTextBkColorA(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd, LVM_GETTEXTBKCOLOR, 0, 0) );
END ListView_GetTextBkColorA;

PROCEDURE ListView_GetTextBkColorW(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd, LVM_GETTEXTBKCOLOR, 0, 0) );
END ListView_GetTextBkColorW;

PROCEDURE ListView_SetTextBkColor(hwnd : HWND; clrTextBk : COLORREF) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, LVM_SETTEXTBKCOLOR, 0,
                                 VAL(LPARAM, clrTextBk))
               );
END ListView_SetTextBkColor;

PROCEDURE ListView_SetTextBkColorA(hwnd : HWND; clrTextBk : COLORREF) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, LVM_SETTEXTBKCOLOR, 0,
                                 VAL(LPARAM, clrTextBk))
               );
END ListView_SetTextBkColorA;

PROCEDURE ListView_SetTextBkColorW(hwnd : HWND; clrTextBk : COLORREF) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, LVM_SETTEXTBKCOLOR, 0,
                                 VAL(LPARAM, clrTextBk))
               );
END ListView_SetTextBkColorW;

PROCEDURE ListView_GetTopIndex(hwndLV : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwndLV, LVM_GETTOPINDEX, 0, 0);
END ListView_GetTopIndex;

PROCEDURE ListView_GetTopIndexA(hwndLV : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwndLV, LVM_GETTOPINDEX, 0, 0);
END ListView_GetTopIndexA;

PROCEDURE ListView_GetTopIndexW(hwndLV : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwndLV, LVM_GETTOPINDEX, 0, 0);
END ListView_GetTopIndexW;

PROCEDURE ListView_GetCountPerPage(hwndLV : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwndLV, LVM_GETCOUNTPERPAGE, 0, 0);
END ListView_GetCountPerPage;

PROCEDURE ListView_GetCountPerPageA(hwndLV : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwndLV, LVM_GETCOUNTPERPAGE, 0, 0);
END ListView_GetCountPerPageA;

PROCEDURE ListView_GetCountPerPageW(hwndLV : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwndLV, LVM_GETCOUNTPERPAGE, 0, 0);
END ListView_GetCountPerPageW;

PROCEDURE ListView_GetOrigin(hwndLV : HWND; VAR ppt : POINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV, LVM_GETORIGIN, 0,
                                  CAST(LPARAM, ADR(ppt))
                                  )
               );
END ListView_GetOrigin;

PROCEDURE ListView_GetOriginA(hwndLV : HWND; VAR ppt : POINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV, LVM_GETORIGIN, 0,
                                  CAST(LPARAM, ADR(ppt))
                                  )
               );
END ListView_GetOriginA;

PROCEDURE ListView_GetOriginW(hwndLV : HWND; VAR ppt : POINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV, LVM_GETORIGIN, 0,
                                  CAST(LPARAM, ADR(ppt))
                                  )
               );
END ListView_GetOriginW;

PROCEDURE ListView_Update(hwndLV : HWND; i : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV, LVM_UPDATE, VAL(WPARAM, i), 0));
END ListView_Update;

PROCEDURE ListView_UpdateA(hwndLV : HWND; i : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV, LVM_UPDATE, VAL(WPARAM, i), 0));
END ListView_UpdateA;

PROCEDURE ListView_UpdateW(hwndLV : HWND; i : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV, LVM_UPDATE, VAL(WPARAM, i), 0));
END ListView_UpdateW;

PROCEDURE ListView_SetItemState(hwndLV : HWND;
                                i : WINT;
                                data : UINT;
                                mask : UINT) : BOOL;
VAR
    _ms_lvi     : LV_ITEM;
BEGIN
    _ms_lvi.stateMask := mask;
    _ms_lvi.state := data;
    RETURN CAST(BOOL, SendMessage(hwndLV,
                                  LVM_SETITEMSTATE,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(_ms_lvi))
                                 )
               );
END ListView_SetItemState;

PROCEDURE ListView_SetItemStateA(hwndLV : HWND;
                                i : WINT;
                                data : UINT;
                                mask : UINT) : BOOL;
VAR
    _ms_lvi     : LV_ITEMA;
BEGIN
    _ms_lvi.stateMask := mask;
    _ms_lvi.state := data;
    RETURN CAST(BOOL, SendMessageA(hwndLV,
                                  LVM_SETITEMSTATE,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(_ms_lvi))
                                 )
               );
END ListView_SetItemStateA;

PROCEDURE ListView_SetItemStateW(hwndLV : HWND;
                                i : WINT;
                                data : UINT;
                                mask : UINT) : BOOL;
VAR
    _ms_lvi     : LV_ITEMW;
BEGIN
    _ms_lvi.stateMask := mask;
    _ms_lvi.state := data;
    RETURN CAST(BOOL, SendMessageW(hwndLV,
                                  LVM_SETITEMSTATE,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(_ms_lvi))
                                 )
               );
END ListView_SetItemStateW;

PROCEDURE ListView_SetCheckState (hwndLV : HWND; iIndex : WINT; fCheck : BOOL);
VAR
    _ms_lvi     : LV_ITEM;
BEGIN
    _ms_lvi.stateMask := LVIS_STATEIMAGEMASK;
	IF fCheck THEN
    	_ms_lvi.state := 2000H;
	ELSE
    	_ms_lvi.state := 1000H;
	END;
	FUNC SendMessage (hwndLV, LVM_SETITEMSTATE, VAL (WPARAM, iIndex), CAST (LPARAM, ADR(_ms_lvi)));
END ListView_SetCheckState;

PROCEDURE ListView_SetCheckStateA (hwndLV : HWND; iIndex : WINT; fCheck : BOOL);
VAR
    _ms_lvi     : LV_ITEMA;
BEGIN
    _ms_lvi.stateMask := LVIS_STATEIMAGEMASK;
	IF fCheck THEN
    	_ms_lvi.state := 2000H;
	ELSE
    	_ms_lvi.state := 1000H;
	END;
	FUNC SendMessageA (hwndLV, LVM_SETITEMSTATE, VAL (WPARAM, iIndex), CAST (LPARAM, ADR(_ms_lvi)));
END ListView_SetCheckStateA;

PROCEDURE ListView_SetCheckStateW (hwndLV : HWND; iIndex : WINT; fCheck : BOOL);
VAR
    _ms_lvi     : LV_ITEMW;
BEGIN
    _ms_lvi.stateMask := LVIS_STATEIMAGEMASK;
	IF fCheck THEN
    	_ms_lvi.state := 2000H;
	ELSE
    	_ms_lvi.state := 1000H;
	END;
	FUNC SendMessageW (hwndLV, LVM_SETITEMSTATE, VAL (WPARAM, iIndex), CAST (LPARAM, ADR(_ms_lvi)));
END ListView_SetCheckStateW;

PROCEDURE ListView_GetItemState(hwndLV : HWND; i : WINT; mask : UINT) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwndLV,
                                  LVM_GETITEMSTATE,
                                  VAL(WPARAM, i),
                                  VAL(LPARAM, mask))
               );
END ListView_GetItemState;

PROCEDURE ListView_GetItemStateA(hwndLV : HWND; i : WINT; mask : UINT) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageA(hwndLV,
                                  LVM_GETITEMSTATE,
                                  VAL(WPARAM, i),
                                  VAL(LPARAM, mask))
               );
END ListView_GetItemStateA;

PROCEDURE ListView_GetItemStateW(hwndLV : HWND; i : WINT; mask : UINT) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageW(hwndLV,
                                  LVM_GETITEMSTATE,
                                  VAL(WPARAM, i),
                                  VAL(LPARAM, mask))
               );
END ListView_GetItemStateW;

PROCEDURE ListView_GetCheckState(hwndLV : HWND; i : WINT) : UINT;
BEGIN
    RETURN SendMessage(hwndLV,
                       LVM_GETITEMSTATE,
                       i,
                       LVIS_STATEIMAGEMASK) SHR 12 -1;
END ListView_GetCheckState;

PROCEDURE ListView_GetCheckStateA(hwndLV : HWND; i : WINT) : UINT;
BEGIN
    RETURN SendMessageA(hwndLV,
                       LVM_GETITEMSTATE,
                       i,
                       LVIS_STATEIMAGEMASK) SHR 12 -1;
END ListView_GetCheckStateA;

PROCEDURE ListView_GetCheckStateW(hwndLV : HWND; i : WINT) : UINT;
BEGIN
    RETURN SendMessageW(hwndLV,
                       LVM_GETITEMSTATE,
                       i,
                       LVIS_STATEIMAGEMASK) SHR 12 -1;
END ListView_GetCheckStateW;

PROCEDURE ListView_GetItemText(hwndLV : HWND;
                               i : WINT;
                               iSubItem_ : WINT;
                               VAR pszText_ : ARRAY OF TCHAR;
                               cchTextMax_ : WINT);
VAR
    _ms_lvi     : LV_ITEM;
BEGIN
  _ms_lvi.iSubItem := iSubItem_;
  _ms_lvi.cchTextMax := cchTextMax_;
  _ms_lvi.pszText := ADR(pszText_);
  SendMessage(hwndLV, LVM_GETITEMTEXT,
              VAL(WPARAM, i),
              CAST(LPARAM, ADR(_ms_lvi))
             );
END ListView_GetItemText;

PROCEDURE ListView_GetItemTextA(hwndLV : HWND;
                               i : WINT;
                               iSubItem_ : WINT;
                               VAR pszText_ : ARRAY OF ACHAR;
                               cchTextMax_ : WINT);
VAR
    _ms_lvi     : LV_ITEM;
BEGIN
  _ms_lvi.iSubItem := iSubItem_;
  _ms_lvi.cchTextMax := cchTextMax_;
  _ms_lvi.pszText := ADR(pszText_);
  SendMessageA(hwndLV, LVM_GETITEMTEXTA,
              VAL(WPARAM, i),
              CAST(LPARAM, ADR(_ms_lvi))
             );
END ListView_GetItemTextA;

PROCEDURE ListView_GetItemTextW(hwndLV : HWND;
                               i : WINT;
                               iSubItem_ : WINT;
                               VAR pszText_ : ARRAY OF UCHAR;
                               cchTextMax_ : WINT);
VAR
    _ms_lvi     : LV_ITEM;
BEGIN
  _ms_lvi.iSubItem := iSubItem_;
  _ms_lvi.cchTextMax := cchTextMax_;
  _ms_lvi.pszText := ADR(pszText_);
  SendMessageW(hwndLV, LVM_GETITEMTEXTW,
              VAL(WPARAM, i),
              CAST(LPARAM, ADR(_ms_lvi))
             );
END ListView_GetItemTextW;

PROCEDURE ListView_SetItemText(hwndLV : HWND;
                               i : WINT;
                               iSubItem_ : WINT;
                               pszText_ : ARRAY OF TCHAR) : BOOL;
VAR
    _ms_lvi     : LV_ITEM;
BEGIN
    _ms_lvi.iSubItem := iSubItem_;
    _ms_lvi.pszText := ADR(pszText_);
    RETURN CAST(BOOL, SendMessage(hwndLV,
                                  LVM_SETITEMTEXT,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(_ms_lvi))
                                 )
               );
END ListView_SetItemText;

PROCEDURE ListView_SetItemTextA(hwndLV : HWND;
                               i : WINT;
                               iSubItem_ : WINT;
                               pszText_ : ARRAY OF ACHAR) : BOOL;
VAR
    _ms_lvi     : LV_ITEM;
BEGIN
    _ms_lvi.iSubItem := iSubItem_;
    _ms_lvi.pszText := ADR(pszText_);
    RETURN CAST(BOOL, SendMessageA(hwndLV,
                                  LVM_SETITEMTEXTA,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(_ms_lvi))
                                 )
               );
END ListView_SetItemTextA;

PROCEDURE ListView_SetItemTextW(hwndLV : HWND;
                                i : WINT;
                                iSubItem_ : WINT;
                                pszText_ : ARRAY OF WCHAR) : BOOL;
VAR
    _ms_lvi     : LV_ITEM;
BEGIN
    _ms_lvi.iSubItem := iSubItem_;
    _ms_lvi.pszText := ADR(pszText_);
    RETURN CAST(BOOL, SendMessageW(hwndLV,
                                  LVM_SETITEMTEXTW,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(_ms_lvi))
                                 )
               );
END ListView_SetItemTextW;

PROCEDURE ListView_SetItemCount(hwndLV : HWND; cItems : WINT) : BOOL;
BEGIN
  RETURN CAST(BOOL, SendMessage(hwndLV,
                                LVM_SETITEMCOUNT,
                                VAL(WPARAM, cItems),
                                0)
             );
END ListView_SetItemCount;

PROCEDURE ListView_SetItemCountA(hwndLV : HWND; cItems : WINT) : BOOL;
BEGIN
  RETURN CAST(BOOL, SendMessageA(hwndLV,
                                LVM_SETITEMCOUNT,
                                VAL(WPARAM, cItems),
                                0)
             );
END ListView_SetItemCountA;

PROCEDURE ListView_SetItemCountW(hwndLV : HWND; cItems : WINT) : BOOL;
BEGIN
  RETURN CAST(BOOL, SendMessageW(hwndLV,
                                LVM_SETITEMCOUNT,
                                VAL(WPARAM, cItems),
                                0)
             );
END ListView_SetItemCountW;

PROCEDURE ListView_SetItemCountEx(hwndLV : HWND;
                                  cItems : WINT;
                                  dwFlags : DWORD);
BEGIN
    SendMessage(hwndLV,
                                  LVM_SETITEMCOUNT,
                                  cItems,
                                  dwFlags);
END ListView_SetItemCountEx;

PROCEDURE ListView_SetItemCountExA(hwndLV : HWND;
                                  cItems : WINT;
                                  dwFlags : DWORD);
BEGIN
    SendMessageA(hwndLV,
                                  LVM_SETITEMCOUNT,
                                  cItems,
                                  dwFlags);
END ListView_SetItemCountExA;

PROCEDURE ListView_SetItemCountExW(hwndLV : HWND;
                                  cItems : WINT;
                                  dwFlags : DWORD);
BEGIN
    SendMessageW(hwndLV,
                                  LVM_SETITEMCOUNT,
                                  cItems,
                                  dwFlags);
END ListView_SetItemCountExW;

PROCEDURE ListView_SortItems(hwndLV : HWND;
                             _pfnCompare : PFNLVCOMPARE;
                             _lPrm : LPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV,
                                  LVM_SORTITEMS,
                                  CAST(WPARAM, _lPrm),
                                  CAST(LPARAM, _pfnCompare)
                                 )
        );
END ListView_SortItems;

PROCEDURE ListView_SortItemsA(hwndLV : HWND;
                             _pfnCompare : PFNLVCOMPARE;
                             _lPrm : LPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV,
                                  LVM_SORTITEMS,
                                  CAST(WPARAM, _lPrm),
                                  CAST(LPARAM, _pfnCompare)
                                 )
        );
END ListView_SortItemsA;

PROCEDURE ListView_SortItemsW(hwndLV : HWND;
                             _pfnCompare : PFNLVCOMPARE;
                             _lPrm : LPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV,
                                  LVM_SORTITEMS,
                                  CAST(WPARAM, _lPrm),
                                  CAST(LPARAM, _pfnCompare)
                                 )
        );
END ListView_SortItemsW;

PROCEDURE ListView_SetItemPosition32(hwndLV : HWND;
                                     i : WINT;
                                     x : WINT;
                                     y : WINT) : BOOL;
VAR
    ptNewPos    : POINT;
BEGIN
    ptNewPos.x := x;
    ptNewPos.y := y;
    RETURN CAST(BOOL, SendMessage(hwndLV,
                                  LVM_SETITEMPOSITION32,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(ptNewPos))
                                 )
               );
END ListView_SetItemPosition32;

PROCEDURE ListView_SetItemPosition32A(hwndLV : HWND;
                                     i : WINT;
                                     x : WINT;
                                     y : WINT) : BOOL;
VAR
    ptNewPos    : POINT;
BEGIN
    ptNewPos.x := x;
    ptNewPos.y := y;
    RETURN CAST(BOOL, SendMessageA(hwndLV,
                                  LVM_SETITEMPOSITION32,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(ptNewPos))
                                 )
               );
END ListView_SetItemPosition32A;

PROCEDURE ListView_SetItemPosition32W(hwndLV : HWND;
                                     i : WINT;
                                     x : WINT;
                                     y : WINT) : BOOL;
VAR
    ptNewPos    : POINT;
BEGIN
    ptNewPos.x := x;
    ptNewPos.y := y;
    RETURN CAST(BOOL, SendMessageW(hwndLV,
                                  LVM_SETITEMPOSITION32,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(ptNewPos))
                                 )
               );
END ListView_SetItemPosition32W;

PROCEDURE ListView_GetSelectedCount(hwndLV : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwndLV, LVM_GETSELECTEDCOUNT, 0, 0));
END ListView_GetSelectedCount;

PROCEDURE ListView_GetSelectedCountA(hwndLV : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageA(hwndLV, LVM_GETSELECTEDCOUNT, 0, 0));
END ListView_GetSelectedCountA;

PROCEDURE ListView_GetSelectedCountW(hwndLV : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageW(hwndLV, LVM_GETSELECTEDCOUNT, 0, 0));
END ListView_GetSelectedCountW;

PROCEDURE ListView_GetItemSpacing(hwndLV : HWND; fSmall : WPARAM) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessage(hwndLV, LVM_GETITEMSPACING, fSmall, 0));
END ListView_GetItemSpacing;

PROCEDURE ListView_GetItemSpacingA(hwndLV : HWND; fSmall : WPARAM) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageA(hwndLV, LVM_GETITEMSPACING, fSmall, 0));
END ListView_GetItemSpacingA;

PROCEDURE ListView_GetItemSpacingW(hwndLV : HWND; fSmall : WPARAM) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageW(hwndLV, LVM_GETITEMSPACING, fSmall, 0));
END ListView_GetItemSpacingW;

PROCEDURE ListView_GetISearchString(hwndLV : HWND;
                                    VAR lpsz : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV,
                                  LVM_GETISEARCHSTRING,
                                  0,
                                  CAST(LPARAM, ADR(lpsz))
                                 )
               );
END ListView_GetISearchString;

PROCEDURE ListView_GetISearchStringA(hwndLV : HWND;
                                    VAR lpsz : ARRAY OF ACHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV,
                                  LVM_GETISEARCHSTRINGA,
                                  0,
                                  CAST(LPARAM, ADR(lpsz))
                                 )
               );
END ListView_GetISearchStringA;

PROCEDURE ListView_GetISearchStringW(hwndLV : HWND;
                                    VAR lpsz : ARRAY OF WCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV,
                                  LVM_GETISEARCHSTRINGW,
                                  0,
                                  CAST(LPARAM, ADR(lpsz))
                                 )
               );
END ListView_GetISearchStringW;

PROCEDURE ListView_SetIconSpacing(hwndLV : HWND;
                                  cx, cy : WINT) : DWORD;
BEGIN
    RETURN SendMessage(hwndLV, LVM_SETICONSPACING, 0, MAKELONG(cx, cy));
END ListView_SetIconSpacing;

PROCEDURE ListView_SetIconSpacingA(hwndLV : HWND;
                                  cx, cy : WINT) : DWORD;
BEGIN
    RETURN SendMessageA(hwndLV, LVM_SETICONSPACING, 0, MAKELONG(cx, cy));
END ListView_SetIconSpacingA;

PROCEDURE ListView_SetIconSpacingW(hwndLV : HWND;
                                  cx, cy : WINT) : DWORD;
BEGIN
    RETURN SendMessageW(hwndLV, LVM_SETICONSPACING, 0, MAKELONG(cx, cy));
END ListView_SetIconSpacingW;

PROCEDURE ListView_SetExtendedListViewStyle(hwndLV : HWND; dw : DWORD) : DWORD;
BEGIN
    RETURN SendMessage(hwndLV, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, dw)
END ListView_SetExtendedListViewStyle;

PROCEDURE ListView_SetExtendedListViewStyleA(hwndLV : HWND; dw : DWORD) : DWORD;
BEGIN
    RETURN SendMessageA(hwndLV, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, dw)
END ListView_SetExtendedListViewStyleA;

PROCEDURE ListView_SetExtendedListViewStyleW(hwndLV : HWND; dw : DWORD) : DWORD;
BEGIN
    RETURN SendMessageW(hwndLV, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, dw)
END ListView_SetExtendedListViewStyleW;

PROCEDURE ListView_SetExtendedListViewStyleEx(hwndLV : HWND;
                                              dwMask : DWORD;
                                              dw : DWORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessage(hwndLV,
                                  LVM_SETEXTENDEDLISTVIEWSTYLE,
                                  dwMask, dw));
END ListView_SetExtendedListViewStyleEx;

PROCEDURE ListView_SetExtendedListViewStyleExA(hwndLV : HWND;
                                              dwMask : DWORD;
                                              dw : DWORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageA(hwndLV,
                                  LVM_SETEXTENDEDLISTVIEWSTYLE,
                                  dwMask, dw));
END ListView_SetExtendedListViewStyleExA;

PROCEDURE ListView_SetExtendedListViewStyleExW(hwndLV : HWND;
                                              dwMask : DWORD;
                                              dw : DWORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageW(hwndLV,
                                  LVM_SETEXTENDEDLISTVIEWSTYLE,
                                  dwMask, dw));
END ListView_SetExtendedListViewStyleExW;

PROCEDURE ListView_GetExtendedListViewStyle(hwndLV : HWND) : DWORD;
BEGIN
    RETURN SendMessage(hwndLV, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0);
END ListView_GetExtendedListViewStyle;

PROCEDURE ListView_GetExtendedListViewStyleA(hwndLV : HWND) : DWORD;
BEGIN
    RETURN SendMessageA(hwndLV, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0);
END ListView_GetExtendedListViewStyleA;

PROCEDURE ListView_GetExtendedListViewStyleW(hwndLV : HWND) : DWORD;
BEGIN
    RETURN SendMessageW(hwndLV, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0);
END ListView_GetExtendedListViewStyleW;

PROCEDURE ListView_GetSubItemRect(hwnd : HWND;
                                  iItem, iSubItem : WINT;
                                  code : DWORD;
                                  VAR prc : RECT) :  BOOL;
BEGIN
    prc.top := iSubItem;
    prc.left := code;
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_GETSUBITEMRECT,
                                  iItem,
                                  CAST(LPARAM, ADR(prc))));
END ListView_GetSubItemRect;

PROCEDURE ListView_GetSubItemRectA(hwnd : HWND;
                                  iItem, iSubItem : WINT;
                                  code : DWORD;
                                  VAR prc : RECT) :  BOOL;
BEGIN
    prc.top := iSubItem;
    prc.left := code;
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_GETSUBITEMRECT,
                                  iItem,
                                  CAST(LPARAM, ADR(prc))));
END ListView_GetSubItemRectA;

PROCEDURE ListView_GetSubItemRectW(hwnd : HWND;
                                  iItem, iSubItem : WINT;
                                  code : DWORD;
                                  VAR prc : RECT) :  BOOL;
BEGIN
    prc.top := iSubItem;
    prc.left := code;
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_GETSUBITEMRECT,
                                  iItem,
                                  CAST(LPARAM, ADR(prc))));
END ListView_GetSubItemRectW;

PROCEDURE ListView_SubItemHitTest(hwnd : HWND;
                                  VAR plvhti : LV_HITTESTINFO) : WINT;
BEGIN
    RETURN SendMessage(hwnd,
                       LVM_SUBITEMHITTEST,
                       0,
                       CAST(LPARAM, ADR(plvhti)));
END ListView_SubItemHitTest;

PROCEDURE ListView_SubItemHitTestA(hwnd : HWND;
                                  VAR plvhti : LV_HITTESTINFO) : WINT;
BEGIN
    RETURN SendMessageA(hwnd,
                       LVM_SUBITEMHITTEST,
                       0,
                       CAST(LPARAM, ADR(plvhti)));
END ListView_SubItemHitTestA;

PROCEDURE ListView_SubItemHitTestW(hwnd : HWND;
                                  VAR plvhti : LV_HITTESTINFO) : WINT;
BEGIN
    RETURN SendMessageW(hwnd,
                       LVM_SUBITEMHITTEST,
                       0,
                       CAST(LPARAM, ADR(plvhti)));
END ListView_SubItemHitTestW;

PROCEDURE ListView_SubItemHitTestEx (hwnd : HWND; VAR plvhti : LVHITTESTINFO) : WINT;
BEGIN
    RETURN SendMessage(hwnd,
                       LVM_SUBITEMHITTEST,
                       CAST(WPARAM,VAL(LPARAM,-1)),
                       CAST(LPARAM, ADR(plvhti)));
END ListView_SubItemHitTestEx;

PROCEDURE ListView_SubItemHitTestExA (hwnd : HWND; VAR plvhti : LVHITTESTINFO) : WINT;
BEGIN
    RETURN SendMessageA(hwnd,
                       LVM_SUBITEMHITTEST,
                       CAST(WPARAM,VAL(LPARAM,-1)),
                       CAST(LPARAM, ADR(plvhti)));
END ListView_SubItemHitTestExA;

PROCEDURE ListView_SubItemHitTestExW (hwnd : HWND; VAR plvhti : LVHITTESTINFO) : WINT;
BEGIN
    RETURN SendMessageW(hwnd,
                       LVM_SUBITEMHITTEST,
                       CAST(WPARAM,VAL(LPARAM,-1)),
                       CAST(LPARAM, ADR(plvhti)));
END ListView_SubItemHitTestExW;

PROCEDURE ListView_SetColumnOrderArray(hwnd : HWND;
                                      iCount : WINT;
                                      pi : ARRAY OF WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_SETCOLUMNORDERARRAY,
                                  iCount,
                                  CAST(LPARAM, ADR(pi))));
END ListView_SetColumnOrderArray;

PROCEDURE ListView_SetColumnOrderArrayA(hwnd : HWND;
                                       iCount : WINT;
                                       pi : ARRAY OF WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_SETCOLUMNORDERARRAY,
                                  iCount,
                                  CAST(LPARAM, ADR(pi))));
END ListView_SetColumnOrderArrayA;

PROCEDURE ListView_SetColumnOrderArrayW(hwnd : HWND;
                                       iCount : WINT;
                                       pi : ARRAY OF WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_SETCOLUMNORDERARRAY,
                                  iCount,
                                  CAST(LPARAM, ADR(pi))));
END ListView_SetColumnOrderArrayW;

PROCEDURE ListView_GetColumnOrderArray(hwnd : HWND;
                                       iCount : WINT;
                                       VAR pi : ARRAY OF WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_GETCOLUMNORDERARRAY,
                                  iCount,
                                  CAST(LPARAM, ADR(pi))));
END ListView_GetColumnOrderArray;

PROCEDURE ListView_GetColumnOrderArrayA(hwnd : HWND;
                                       iCount : WINT;
                                       VAR pi : ARRAY OF WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_GETCOLUMNORDERARRAY,
                                  iCount,
                                  CAST(LPARAM, ADR(pi))));
END ListView_GetColumnOrderArrayA;

PROCEDURE ListView_GetColumnOrderArrayW(hwnd : HWND;
                                       iCount : WINT;
                                       VAR pi : ARRAY OF WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_GETCOLUMNORDERARRAY,
                                  iCount,
                                  CAST(LPARAM, ADR(pi))));
END ListView_GetColumnOrderArrayW;

PROCEDURE ListView_SetHotItem(hwnd : HWND; i : WINT) : WINT;
BEGIN
    RETURN SendMessage(hwnd,
                       LVM_SETHOTITEM, i, 0);
END ListView_SetHotItem;

PROCEDURE ListView_SetHotItemA(hwnd : HWND; i : WINT) : WINT;
BEGIN
    RETURN SendMessageA(hwnd,
                       LVM_SETHOTITEM, i, 0);
END ListView_SetHotItemA;

PROCEDURE ListView_SetHotItemW(hwnd : HWND; i : WINT) : WINT;
BEGIN
    RETURN SendMessageW(hwnd,
                       LVM_SETHOTITEM, i, 0);
END ListView_SetHotItemW;

PROCEDURE ListView_GetHotItem(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwnd,
                       LVM_GETHOTITEM, 0, 0);
END ListView_GetHotItem;

PROCEDURE ListView_GetHotItemA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwnd,
                       LVM_GETHOTITEM, 0, 0);
END ListView_GetHotItemA;

PROCEDURE ListView_GetHotItemW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwnd,
                       LVM_GETHOTITEM, 0, 0);
END ListView_GetHotItemW;

PROCEDURE ListView_SetHotCursor(hwnd : HWND; hcur : HCURSOR) : HCURSOR;
BEGIN
    RETURN CAST(HCURSOR, SendMessage(hwnd,
                                     LVM_SETHOTCURSOR, 0, CAST(LPARAM, hcur)));
END ListView_SetHotCursor;

PROCEDURE ListView_SetHotCursorA(hwnd : HWND; hcur : HCURSOR) : HCURSOR;
BEGIN
    RETURN CAST(HCURSOR, SendMessageA(hwnd,
                                     LVM_SETHOTCURSOR, 0, CAST(LPARAM, hcur)));
END ListView_SetHotCursorA;

PROCEDURE ListView_SetHotCursorW(hwnd : HWND; hcur : HCURSOR) : HCURSOR;
BEGIN
    RETURN CAST(HCURSOR, SendMessageW(hwnd,
                                     LVM_SETHOTCURSOR, 0, CAST(LPARAM, hcur)));
END ListView_SetHotCursorW;

PROCEDURE ListView_GetHotCursor(hwnd : HWND) : HCURSOR;
BEGIN
    RETURN CAST(HCURSOR, SendMessage(hwnd,
                                     LVM_GETHOTCURSOR, 0, 0));
END ListView_GetHotCursor;

PROCEDURE ListView_GetHotCursorA(hwnd : HWND) : HCURSOR;
BEGIN
    RETURN CAST(HCURSOR, SendMessageA(hwnd,
                                     LVM_GETHOTCURSOR, 0, 0));
END ListView_GetHotCursorA;

PROCEDURE ListView_GetHotCursorW(hwnd : HWND) : HCURSOR;
BEGIN
    RETURN CAST(HCURSOR, SendMessageW(hwnd,
                                     LVM_GETHOTCURSOR, 0, 0));
END ListView_GetHotCursorW;

PROCEDURE ListView_ApproximateViewRect(hwnd : HWND;
                                       iWidth, iHeight, iCount : WINT) : DWORD;
BEGIN
    RETURN SendMessage(hwnd,
                       LVM_APPROXIMATEVIEWRECT,
                       iCount, MAKELPARAM(iWidth, iHeight));
END ListView_ApproximateViewRect;

PROCEDURE ListView_ApproximateViewRectA(hwnd : HWND;
                                       iWidth, iHeight, iCount : WINT) : DWORD;
BEGIN
    RETURN SendMessageA(hwnd,
                       LVM_APPROXIMATEVIEWRECT,
                       iCount, MAKELPARAM(iWidth, iHeight));
END ListView_ApproximateViewRectA;

PROCEDURE ListView_ApproximateViewRectW(hwnd : HWND;
                                       iWidth, iHeight, iCount : WINT) : DWORD;
BEGIN
    RETURN SendMessageW(hwnd,
                       LVM_APPROXIMATEVIEWRECT,
                       iCount, MAKELPARAM(iWidth, iHeight));
END ListView_ApproximateViewRectW;

PROCEDURE ListView_SetWorkAreas(hwnd : HWND;
                                nWorkAreas : WINT;
                                prc : ARRAY OF RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_SETWORKAREAS,
                                  nWorkAreas,
                                  CAST(LPARAM, ADR(prc))));
END ListView_SetWorkAreas;

PROCEDURE ListView_SetWorkAreasA(hwnd : HWND;
                                nWorkAreas : WINT;
                                prc : ARRAY OF RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_SETWORKAREAS,
                                  nWorkAreas,
                                  CAST(LPARAM, ADR(prc))));
END ListView_SetWorkAreasA;

PROCEDURE ListView_SetWorkAreasW(hwnd : HWND;
                                nWorkAreas : WINT;
                                prc : ARRAY OF RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_SETWORKAREAS,
                                  nWorkAreas,
                                  CAST(LPARAM, ADR(prc))));
END ListView_SetWorkAreasW;

PROCEDURE ListView_GetWorkAreas(hwnd : HWND;
                                nWorkAreas : WINT;
                                VAR prc : ARRAY OF RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_GETWORKAREAS,
                                  nWorkAreas,
                                  CAST(LPARAM, ADR(prc))));
END ListView_GetWorkAreas;

PROCEDURE ListView_GetWorkAreasA(hwnd : HWND;
                                nWorkAreas : WINT;
                                VAR prc : ARRAY OF RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_GETWORKAREAS,
                                  nWorkAreas,
                                  CAST(LPARAM, ADR(prc))));
END ListView_GetWorkAreasA;

PROCEDURE ListView_GetWorkAreasW(hwnd : HWND;
                                nWorkAreas : WINT;
                                VAR prc : ARRAY OF RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_GETWORKAREAS,
                                  nWorkAreas,
                                  CAST(LPARAM, ADR(prc))));
END ListView_GetWorkAreasW;

PROCEDURE ListView_GetNumberOfWorkAreas(hwnd : HWND;
                                        VAR pnWorkAreas : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_GETNUMBEROFWORKAREAS,
                                  0,
                                  CAST(LPARAM, ADR(pnWorkAreas))));
END ListView_GetNumberOfWorkAreas;

PROCEDURE ListView_GetNumberOfWorkAreasA(hwnd : HWND;
                                        VAR pnWorkAreas : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_GETNUMBEROFWORKAREAS,
                                  0,
                                  CAST(LPARAM, ADR(pnWorkAreas))));
END ListView_GetNumberOfWorkAreasA;

PROCEDURE ListView_GetNumberOfWorkAreasW(hwnd : HWND;
                                        VAR pnWorkAreas : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_GETNUMBEROFWORKAREAS,
                                  0,
                                  CAST(LPARAM, ADR(pnWorkAreas))));
END ListView_GetNumberOfWorkAreasW;

PROCEDURE ListView_GetSelectionMark(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwnd, LVM_GETSELECTIONMARK, 0, 0);
END ListView_GetSelectionMark;

PROCEDURE ListView_GetSelectionMarkA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, LVM_GETSELECTIONMARK, 0, 0);
END ListView_GetSelectionMarkA;

PROCEDURE ListView_GetSelectionMarkW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, LVM_GETSELECTIONMARK, 0, 0);
END ListView_GetSelectionMarkW;

PROCEDURE ListView_SetSelectionMark(hwnd : HWND; i : WINT) : WINT;
BEGIN
    RETURN SendMessage(hwnd, LVM_SETSELECTIONMARK, 0, i);
END ListView_SetSelectionMark;

PROCEDURE ListView_SetSelectionMarkA(hwnd : HWND; i : WINT) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, LVM_SETSELECTIONMARK, 0, i);
END ListView_SetSelectionMarkA;

PROCEDURE ListView_SetSelectionMarkW(hwnd : HWND; i : WINT) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, LVM_SETSELECTIONMARK, 0, i);
END ListView_SetSelectionMarkW;

PROCEDURE ListView_SetHoverTime(hwndLV : HWND; dwHoverTimeMs : DWORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessage(hwndLV,
                                  LVM_SETHOVERTIME, 0, dwHoverTimeMs));
END ListView_SetHoverTime;

PROCEDURE ListView_SetHoverTimeA(hwndLV : HWND; dwHoverTimeMs : DWORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageA(hwndLV,
                                  LVM_SETHOVERTIME, 0, dwHoverTimeMs));
END ListView_SetHoverTimeA;

PROCEDURE ListView_SetHoverTimeW(hwndLV : HWND; dwHoverTimeMs : DWORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageW(hwndLV,
                                  LVM_SETHOVERTIME, 0, dwHoverTimeMs));
END ListView_SetHoverTimeW;

PROCEDURE ListView_GetHoverTime(hwndLV : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessage(hwndLV, LVM_GETHOVERTIME, 0, 0));
END ListView_GetHoverTime;

PROCEDURE ListView_GetHoverTimeA(hwndLV : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageA(hwndLV, LVM_GETHOVERTIME, 0, 0));
END ListView_GetHoverTimeA;

PROCEDURE ListView_GetHoverTimeW(hwndLV : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageW(hwndLV, LVM_GETHOVERTIME, 0, 0));
END ListView_GetHoverTimeW;

PROCEDURE ListView_SetToolTips(hwndLV : HWND; hwndNewHwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwndLV,
                                  LVM_SETTOOLTIPS,
                                  CAST(WPARAM, hwndNewHwnd), 0));
END ListView_SetToolTips;

PROCEDURE ListView_SetToolTipsA(hwndLV : HWND; hwndNewHwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwndLV,
                                  LVM_SETTOOLTIPS,
                                  CAST(WPARAM, hwndNewHwnd), 0));
END ListView_SetToolTipsA;

PROCEDURE ListView_SetToolTipsW(hwndLV : HWND; hwndNewHwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwndLV,
                                  LVM_SETTOOLTIPS,
                                  CAST(WPARAM, hwndNewHwnd), 0));
END ListView_SetToolTipsW;

PROCEDURE ListView_GetToolTips(hwndLV : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwndLV, LVM_GETTOOLTIPS, 0, 0));
END ListView_GetToolTips;

PROCEDURE ListView_GetToolTipsA(hwndLV : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwndLV, LVM_GETTOOLTIPS, 0, 0));
END ListView_GetToolTipsA;

PROCEDURE ListView_GetToolTipsW(hwndLV : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwndLV, LVM_GETTOOLTIPS, 0, 0));
END ListView_GetToolTipsW;

PROCEDURE ListView_SortItemsEx(hwndLV : HWND;
                               _pfnCompare : PFNLVCOMPARE;
                               _lPrm : LPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndLV,
                                  LVM_SORTITEMSEX,
                                  CAST(WPARAM, _lPrm),
                                  CAST(LPARAM, _pfnCompare)
                                 )
        );
END ListView_SortItemsEx;

PROCEDURE ListView_SortItemsExA(hwndLV : HWND;
                               _pfnCompare : PFNLVCOMPARE;
                               _lPrm : LPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndLV,
                                  LVM_SORTITEMSEX,
                                  CAST(WPARAM, _lPrm),
                                  CAST(LPARAM, _pfnCompare)
                                 )
        );
END ListView_SortItemsExA;

PROCEDURE ListView_SortItemsExW(hwndLV : HWND;
                               _pfnCompare : PFNLVCOMPARE;
                               _lPrm : LPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndLV,
                                  LVM_SORTITEMSEX,
                                  CAST(WPARAM, _lPrm),
                                  CAST(LPARAM, _pfnCompare)
                                 )
        );
END ListView_SortItemsExW;

PROCEDURE ListView_SetSelectedColumn  (hwnd : HWND; iCol : WINT);
BEGIN
    FUNC SendMessage (hwnd, LVM_SETSELECTEDCOLUMN, VAL(WPARAM,iCol), 0);
END ListView_SetSelectedColumn;

PROCEDURE ListView_SetSelectedColumnA (hwnd : HWND; iCol : WINT);
BEGIN
    FUNC SendMessageA (hwnd, LVM_SETSELECTEDCOLUMN, VAL(WPARAM,iCol), 0);
END ListView_SetSelectedColumnA;

PROCEDURE ListView_SetSelectedColumnW (hwnd : HWND; iCol : WINT);
BEGIN
    FUNC SendMessageW (hwnd, LVM_SETSELECTEDCOLUMN, VAL(WPARAM,iCol), 0);
END ListView_SetSelectedColumnW;

PROCEDURE ListView_SetView (hwnd : HWND; iView : DWORD) : WINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_SETVIEW, iView, 0);
END ListView_SetView;

PROCEDURE ListView_SetViewA (hwnd : HWND; iView : DWORD) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_SETVIEW, iView, 0);
END ListView_SetViewA;

PROCEDURE ListView_SetViewW (hwnd : HWND; iView : DWORD) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_SETVIEW, iView, 0);
END ListView_SetViewW;

PROCEDURE ListView_GetView (hwnd :HWND) : DWORD;
BEGIN
	RETURN SendMessage (hwnd, LVM_GETITEM, 0, 0);
END ListView_GetView;

PROCEDURE ListView_GetViewA (hwnd :HWND) : DWORD;
BEGIN
	RETURN SendMessageA (hwnd, LVM_GETITEM, 0, 0);
END ListView_GetViewA;

PROCEDURE ListView_GetViewW (hwnd :HWND) : DWORD;
BEGIN
	RETURN SendMessageW (hwnd, LVM_GETITEM, 0, 0);
END ListView_GetViewW;

PROCEDURE ListView_InsertGroup (hwnd : HWND; index : WINT; pgrp : LVGROUP) : WINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_INSERTGROUP, index, CAST(LPARAM,ADR(pgrp)));
END ListView_InsertGroup;

PROCEDURE ListView_InsertGroupA (hwnd : HWND; index : WINT; pgrp : LVGROUP) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_INSERTGROUP, index, CAST(LPARAM,ADR(pgrp)));
END ListView_InsertGroupA;

PROCEDURE ListView_InsertGroupW (hwnd : HWND; index : WINT; pgrp : LVGROUP) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_INSERTGROUP, index, CAST(LPARAM,ADR(pgrp)));
END ListView_InsertGroupW;

PROCEDURE ListView_SetGroupInfo (hwnd : HWND; iGroupId : WINT; pgrp : LVGROUP) : WINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_SETGROUPINFO, iGroupId, CAST(LPARAM,ADR(pgrp)));
END ListView_SetGroupInfo;

PROCEDURE ListView_SetGroupInfoA (hwnd : HWND; iGroupId : WINT; pgrp : LVGROUP) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_SETGROUPINFO, iGroupId, CAST(LPARAM,ADR(pgrp)));
END ListView_SetGroupInfoA;

PROCEDURE ListView_SetGroupInfoW (hwnd : HWND; iGroupId : WINT; pgrp : LVGROUP) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_SETGROUPINFO, iGroupId, CAST(LPARAM,ADR(pgrp)));
END ListView_SetGroupInfoW;

PROCEDURE ListView_GetGroupInfo (hwnd : HWND; iGroupId : WINT; VAR pgrp : LVGROUP) : WINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_GETGROUPINFO, iGroupId, CAST(LPARAM,ADR(pgrp)));
END ListView_GetGroupInfo;

PROCEDURE ListView_GetGroupInfoA (hwnd : HWND; iGroupId : WINT; VAR pgrp : LVGROUP) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_GETGROUPINFO, iGroupId, CAST(LPARAM,ADR(pgrp)));
END ListView_GetGroupInfoA;

PROCEDURE ListView_GetGroupInfoW (hwnd : HWND; iGroupId : WINT; VAR pgrp : LVGROUP) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_GETGROUPINFO, iGroupId, CAST(LPARAM,ADR(pgrp)));
END ListView_GetGroupInfoW;

PROCEDURE ListView_RemoveGroup (hwnd : HWND; iGroupId : WINT) : WINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_REMOVEGROUP, iGroupId, 0);
END ListView_RemoveGroup;

PROCEDURE ListView_RemoveGroupA (hwnd : HWND; iGroupId : WINT) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_REMOVEGROUP, iGroupId, 0);
END ListView_RemoveGroupA;

PROCEDURE ListView_RemoveGroupW (hwnd : HWND; iGroupId : WINT) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_REMOVEGROUP, iGroupId, 0);
END ListView_RemoveGroupW;

PROCEDURE ListView_GetGroupCount (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_GETGROUPCOUNT, 0, 0);
END ListView_GetGroupCount;

PROCEDURE ListView_GetGroupCountA (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_GETGROUPCOUNT, 0, 0);
END ListView_GetGroupCountA;

PROCEDURE ListView_GetGroupCountW (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_GETGROUPCOUNT, 0, 0);
END ListView_GetGroupCountW;

PROCEDURE ListView_GetGroupInfoByIndex (hwnd : HWND; iIndex : WINT; VAR pgrp : LVGROUP) : LRESULT;
BEGIN
	RETURN SendMessage (hwnd, LVM_GETGROUPINFOBYINDEX, iIndex, CAST(LPARAM,ADR(pgrp)));
END ListView_GetGroupInfoByIndex;

PROCEDURE ListView_GetGroupInfoByIndexA (hwnd : HWND; iIndex : WINT; VAR pgrp : LVGROUP) : LRESULT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_GETGROUPINFOBYINDEX, iIndex, CAST(LPARAM,ADR(pgrp)));
END ListView_GetGroupInfoByIndexA;

PROCEDURE ListView_GetGroupInfoByIndexW (hwnd : HWND; iIndex : WINT; VAR pgrp : LVGROUP) : LRESULT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_GETGROUPINFOBYINDEX, iIndex, CAST(LPARAM,ADR(pgrp)));
END ListView_GetGroupInfoByIndexW;

PROCEDURE ListView_GetGroupRect (hwnd : HWND; iGroupId : WINT; type : LONG; VAR prc : RECT) : BOOL;
BEGIN
	IF ADR(prc) # NIL THEN
		prc.top := type;
	END;
	RETURN CAST (BOOL, SendMessage (hwnd, LVM_GETGROUPRECT, iGroupId, CAST(LPARAM,ADR(prc))));
END ListView_GetGroupRect;

PROCEDURE ListView_GetGroupRectA (hwnd : HWND; iGroupId : WINT; type : LONG; VAR prc : RECT) : BOOL;
BEGIN
	IF ADR(prc) # NIL THEN
		prc.top := type;
	END;
	RETURN CAST (BOOL, SendMessageA (hwnd, LVM_GETGROUPRECT, iGroupId, CAST(LPARAM,ADR(prc))));
END ListView_GetGroupRectA;

PROCEDURE ListView_GetGroupRectW (hwnd : HWND; iGroupId : WINT; type : LONG; VAR prc : RECT) : BOOL;
BEGIN
	IF ADR(prc) # NIL THEN
		prc.top := type;
	END;
	RETURN CAST (BOOL, SendMessageW (hwnd, LVM_GETGROUPRECT, iGroupId, CAST(LPARAM,ADR(prc))));
END ListView_GetGroupRectW;

PROCEDURE ListView_SetGroupMetrics (hwnd : HWND; pGroupMetrics : LVGROUPMETRICS);
BEGIN
	FUNC SendMessage (hwnd, LVM_SETGROUPMETRICS, 0, CAST(LPARAM,ADR(pGroupMetrics)));
END ListView_SetGroupMetrics;

PROCEDURE ListView_SetGroupMetricsA (hwnd : HWND; pGroupMetrics : LVGROUPMETRICS);
BEGIN
	FUNC SendMessageA (hwnd, LVM_SETGROUPMETRICS, 0, CAST(LPARAM,ADR(pGroupMetrics)));
END ListView_SetGroupMetricsA;

PROCEDURE ListView_SetGroupMetricsW (hwnd : HWND; pGroupMetrics : LVGROUPMETRICS);
BEGIN
	FUNC SendMessageW (hwnd, LVM_SETGROUPMETRICS, 0, CAST(LPARAM,ADR(pGroupMetrics)));
END ListView_SetGroupMetricsW;

PROCEDURE ListView_GetGroupMetrics (hwnd : HWND; VAR pGroupMetrics : LVGROUPMETRICS);
BEGIN
	FUNC SendMessage (hwnd, LVM_GETGROUPMETRICS, 0, CAST(LPARAM,ADR(pGroupMetrics)));
END ListView_GetGroupMetrics;

PROCEDURE ListView_GetGroupMetricsA (hwnd : HWND; VAR pGroupMetrics : LVGROUPMETRICS);
BEGIN
	FUNC SendMessageA (hwnd, LVM_GETGROUPMETRICS, 0, CAST(LPARAM,ADR(pGroupMetrics)));
END ListView_GetGroupMetricsA;

PROCEDURE ListView_GetGroupMetricsW (hwnd : HWND; VAR pGroupMetrics : LVGROUPMETRICS);
BEGIN
	FUNC SendMessageW (hwnd, LVM_GETGROUPMETRICS, 0, CAST(LPARAM,ADR(pGroupMetrics)));
END ListView_GetGroupMetricsW;

PROCEDURE ListView_EnableGroupView (hwnd : HWND; fEnable : BOOL) : WINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_ENABLEGROUPVIEW, VAL(WPARAM,fEnable), 0);
END ListView_EnableGroupView;

PROCEDURE ListView_EnableGroupViewA (hwnd : HWND; fEnable : BOOL) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_ENABLEGROUPVIEW, VAL(WPARAM,fEnable), 0);
END ListView_EnableGroupViewA;

PROCEDURE ListView_EnableGroupViewW (hwnd : HWND; fEnable : BOOL) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_ENABLEGROUPVIEW, VAL(WPARAM,fEnable), 0);
END ListView_EnableGroupViewW;

PROCEDURE ListView_SortGroups (hwnd : HWND; pfnGroupCompare : PFNLVGROUPCOMPARE; plv : LPVOID) : WINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_SORTGROUPS, CAST(WPARAM,pfnGroupCompare), CAST(LPARAM,plv));
END ListView_SortGroups;

PROCEDURE ListView_SortGroupsA (hwnd : HWND; pfnGroupCompare : PFNLVGROUPCOMPARE; plv : LPVOID) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_SORTGROUPS, CAST(WPARAM,pfnGroupCompare), CAST(LPARAM,plv));
END ListView_SortGroupsA;

PROCEDURE ListView_SortGroupsW (hwnd : HWND; pfnGroupCompare : PFNLVGROUPCOMPARE; plv : LPVOID) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_SORTGROUPS, CAST(WPARAM,pfnGroupCompare), CAST(LPARAM,plv));
END ListView_SortGroupsW;

PROCEDURE ListView_InsertGroupSorted (hwnd : HWND; structInsert : PLVINSERTGROUPSORTED);
BEGIN
	FUNC SendMessage (hwnd, LVM_INSERTGROUPSORTED, CAST(WPARAM,structInsert), 0);
END ListView_InsertGroupSorted;

PROCEDURE ListView_InsertGroupSortedA (hwnd : HWND; structInsert : PLVINSERTGROUPSORTED);
BEGIN
	FUNC SendMessageA (hwnd, LVM_INSERTGROUPSORTED, CAST(WPARAM,structInsert), 0);
END ListView_InsertGroupSortedA;

PROCEDURE ListView_InsertGroupSortedW (hwnd : HWND; structInsert : PLVINSERTGROUPSORTED);
BEGIN
	FUNC SendMessageW (hwnd, LVM_INSERTGROUPSORTED, CAST(WPARAM,structInsert), 0);
END ListView_InsertGroupSortedW;

PROCEDURE ListView_RemoveAllGroups (hwnd : HWND);
BEGIN
	FUNC SendMessage (hwnd, LVM_REMOVEALLGROUPS, 0, 0);
END ListView_RemoveAllGroups;

PROCEDURE ListView_RemoveAllGroupsA (hwnd : HWND);
BEGIN
	FUNC SendMessageA (hwnd, LVM_REMOVEALLGROUPS, 0, 0);
END ListView_RemoveAllGroupsA;

PROCEDURE ListView_RemoveAllGroupsW (hwnd : HWND);
BEGIN
	FUNC SendMessageW (hwnd, LVM_REMOVEALLGROUPS, 0, 0);
END ListView_RemoveAllGroupsW;

PROCEDURE ListView_HasGroup (hwnd : HWND; dwGroupId : WINT) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessage (hwnd, LVM_HASGROUP, dwGroupId, 0));
END ListView_HasGroup;

PROCEDURE ListView_HasGroupA (hwnd : HWND; dwGroupId : WINT) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessageA (hwnd, LVM_HASGROUP, dwGroupId, 0));
END ListView_HasGroupA;

PROCEDURE ListView_HasGroupW (hwnd : HWND; dwGroupId : WINT) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessageW (hwnd, LVM_HASGROUP, dwGroupId, 0));
END ListView_HasGroupW;

PROCEDURE ListView_SetGroupState (hwnd : HWND; dwGroupId : UINT; dwMask : UINT; dwState : UINT) : LRESULT;
VAR
	lvg : LVGROUP;
BEGIN
	lvg.cbSize := SIZE(LVGROUP);
  	lvg.mask := LVGF_STATE;
  	lvg.stateMask := dwMask;
  	lvg.state := dwState;
	RETURN SendMessage (hwnd, LVM_SETGROUPINFO, dwGroupId, CAST(LPARAM,ADR(lvg)));
END ListView_SetGroupState;

PROCEDURE ListView_SetGroupStateA (hwnd : HWND; dwGroupId : UINT; dwMask : UINT; dwState : UINT) : LRESULT;
VAR
	lvg : LVGROUP;
BEGIN
	lvg.cbSize := SIZE(LVGROUP);
  	lvg.mask := LVGF_STATE;
  	lvg.stateMask := dwMask;
  	lvg.state := dwState;
	RETURN SendMessageA (hwnd, LVM_SETGROUPINFO, dwGroupId, CAST(LPARAM,ADR(lvg)));
END ListView_SetGroupStateA;

PROCEDURE ListView_SetGroupStateW (hwnd : HWND; dwGroupId : UINT; dwMask : UINT; dwState : UINT) : LRESULT;
VAR
	lvg : LVGROUP;
BEGIN
	lvg.cbSize := SIZE(LVGROUP);
  	lvg.mask := LVGF_STATE;
  	lvg.stateMask := dwMask;
  	lvg.state := dwState;
	RETURN SendMessageW (hwnd, LVM_SETGROUPINFO, dwGroupId, CAST(LPARAM,ADR(lvg)));
END ListView_SetGroupStateW;

PROCEDURE ListView_GetGroupState (hwnd : HWND; dwGroupId : UINT; dwMask : UINT) : UINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_GETGROUPSTATE, dwGroupId, dwMask);
END ListView_GetGroupState;

PROCEDURE ListView_GetGroupStateA (hwnd : HWND; dwGroupId : UINT; dwMask : UINT) : UINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_GETGROUPSTATE, dwGroupId, dwMask);
END ListView_GetGroupStateA;

PROCEDURE ListView_GetGroupStateW (hwnd : HWND; dwGroupId : UINT; dwMask : UINT) : UINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_GETGROUPSTATE, dwGroupId, dwMask);
END ListView_GetGroupStateW;

PROCEDURE ListView_GetFocusedGroup (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_GETFOCUSEDGROUP, 0, 0);
END ListView_GetFocusedGroup;

PROCEDURE ListView_GetFocusedGroupA (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_GETFOCUSEDGROUP, 0, 0);
END ListView_GetFocusedGroupA;

PROCEDURE ListView_GetFocusedGroupW (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_GETFOCUSEDGROUP, 0, 0);
END ListView_GetFocusedGroupW;

PROCEDURE ListView_SetTileViewInfo (hwnd : HWND; plvtvinfo : LVTILEVIEWINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_SETTILEVIEWINFO, 0, CAST(LPARAM,ADR(plvtvinfo))));
END ListView_SetTileViewInfo;

PROCEDURE ListView_SetTileViewInfoA (hwnd : HWND; plvtvinfo : LVTILEVIEWINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_SETTILEVIEWINFO, 0, CAST(LPARAM,ADR(plvtvinfo))));
END ListView_SetTileViewInfoA;

PROCEDURE ListView_SetTileViewInfoW (hwnd : HWND; plvtvinfo : LVTILEVIEWINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_SETTILEVIEWINFO, 0, CAST(LPARAM,ADR(plvtvinfo))));
END ListView_SetTileViewInfoW;

PROCEDURE ListView_GetTileViewInfo (hwnd : HWND; VAR plvtvinfo : LVTILEVIEWINFO);
BEGIN
	FUNC SendMessage (hwnd, LVM_GETTILEVIEWINFO, 0, CAST(LPARAM,ADR(plvtvinfo)));
END ListView_GetTileViewInfo;

PROCEDURE ListView_GetTileViewInfoA (hwnd : HWND; VAR plvtvinfo : LVTILEVIEWINFO);
BEGIN
	FUNC SendMessageA (hwnd, LVM_GETTILEVIEWINFO, 0, CAST(LPARAM,ADR(plvtvinfo)));
END ListView_GetTileViewInfoA;

PROCEDURE ListView_GetTileViewInfoW (hwnd : HWND; VAR plvtvinfo : LVTILEVIEWINFO);
BEGIN
	FUNC SendMessageW (hwnd, LVM_GETTILEVIEWINFO, 0, CAST(LPARAM,ADR(plvtvinfo)));
END ListView_GetTileViewInfoW;

PROCEDURE ListView_SetTileInfo (hwnd : HWND; plvtinfo : LVTILEINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_SETTILEINFO, 0, CAST(LPARAM,ADR(plvtinfo))));
END ListView_SetTileInfo;

PROCEDURE ListView_SetTileInfoA (hwnd : HWND; plvtinfo : LVTILEINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_SETTILEINFO, 0, CAST(LPARAM,ADR(plvtinfo))));
END ListView_SetTileInfoA;

PROCEDURE ListView_SetTileInfoW (hwnd : HWND; plvtinfo : LVTILEINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_SETTILEINFO, 0, CAST(LPARAM,ADR(plvtinfo))));
END ListView_SetTileInfoW;

PROCEDURE ListView_GetTileInfo (hwnd : HWND; VAR plvtinfo : LVTILEINFO);
BEGIN
	FUNC SendMessage (hwnd, LVM_GETTILEINFO, 0, CAST(LPARAM,ADR(plvtinfo)));
END ListView_GetTileInfo;

PROCEDURE ListView_GetTileInfoA (hwnd : HWND; VAR plvtinfo : LVTILEINFO);
BEGIN
	FUNC SendMessageA (hwnd, LVM_GETTILEINFO, 0, CAST(LPARAM,ADR(plvtinfo)));
END ListView_GetTileInfoA;

PROCEDURE ListView_GetTileInfoW (hwnd : HWND; VAR plvtinfo : LVTILEINFO);
BEGIN
	FUNC SendMessageW (hwnd, LVM_GETTILEINFO, 0, CAST(LPARAM,ADR(plvtinfo)));
END ListView_GetTileInfoW;

PROCEDURE ListView_SetInsertMark (hwnd : HWND; plvim : LVINSERTMARK) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_SETINSERTMARK, 0, CAST(LPARAM,ADR(plvim))));
END ListView_SetInsertMark;

PROCEDURE ListView_SetInsertMarkA (hwnd : HWND; plvim : LVINSERTMARK) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_SETINSERTMARK, 0, CAST(LPARAM,ADR(plvim))));
END ListView_SetInsertMarkA;

PROCEDURE ListView_SetInsertMarkW (hwnd : HWND; plvim : LVINSERTMARK) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_SETINSERTMARK, 0, CAST(LPARAM,ADR(plvim))));
END ListView_SetInsertMarkW;

PROCEDURE ListView_GetInsertMark (hwnd : HWND; VAR plvim : LVINSERTMARK) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_GETINSERTMARK, 0, CAST(LPARAM,ADR(plvim))));
END ListView_GetInsertMark;

PROCEDURE ListView_GetInsertMarkA (hwnd : HWND; VAR plvim : LVINSERTMARK) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_GETINSERTMARK, 0, CAST(LPARAM,ADR(plvim))));
END ListView_GetInsertMarkA;

PROCEDURE ListView_GetInsertMarkW (hwnd : HWND; VAR plvim : LVINSERTMARK) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_GETINSERTMARK, 0, CAST(LPARAM,ADR(plvim))));
END ListView_GetInsertMarkW;

PROCEDURE ListView_InsertMarkHitTest (hwnd : HWND; point : POINT; VAR plvim : LVINSERTMARK) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_INSERTMARKHITTEST, CAST(LPARAM,ADR(point)), CAST(LPARAM,ADR(plvim))));
END ListView_InsertMarkHitTest;

PROCEDURE ListView_InsertMarkHitTestA (hwnd : HWND; point : POINT; VAR plvim : LVINSERTMARK) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_INSERTMARKHITTEST, CAST(LPARAM,ADR(point)), CAST(LPARAM,ADR(plvim))));
END ListView_InsertMarkHitTestA;

PROCEDURE ListView_InsertMarkHitTestW (hwnd : HWND; point : POINT; VAR plvim : LVINSERTMARK) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_INSERTMARKHITTEST, CAST(LPARAM,ADR(point)), CAST(LPARAM,ADR(plvim))));
END ListView_InsertMarkHitTestW;

PROCEDURE ListView_GetInsertMarkRect (hwnd : HWND; VAR prc : RECT) : WINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_GETINSERTMARKRECT, 0, CAST(LPARAM,ADR(prc)));
END ListView_GetInsertMarkRect;

PROCEDURE ListView_GetInsertMarkRectA (hwnd : HWND; VAR prc : RECT) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_GETINSERTMARKRECT, 0, CAST(LPARAM,ADR(prc)));
END ListView_GetInsertMarkRectA;

PROCEDURE ListView_GetInsertMarkRectW (hwnd : HWND; VAR prc : RECT) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_GETINSERTMARKRECT, 0, CAST(LPARAM,ADR(prc)));
END ListView_GetInsertMarkRectW;

PROCEDURE ListView_SetInsertMarkColor (hwnd : HWND; color : COLORREF) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessage (hwnd, LVM_SETINSERTMARKCOLOR, 0, VAL(LPARAM,color)));
END ListView_SetInsertMarkColor;

PROCEDURE ListView_SetInsertMarkColorA (hwnd : HWND; color : COLORREF) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessageA (hwnd, LVM_SETINSERTMARKCOLOR, 0, VAL(LPARAM,color)));
END ListView_SetInsertMarkColorA;

PROCEDURE ListView_SetInsertMarkColorW (hwnd : HWND; color : COLORREF) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessageW (hwnd, LVM_SETINSERTMARKCOLOR, 0, VAL(LPARAM,color)));
END ListView_SetInsertMarkColorW;

PROCEDURE ListView_GetInsertMarkColor (hwnd : HWND) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessage (hwnd, LVM_GETINSERTMARKCOLOR, 0, 0));
END ListView_GetInsertMarkColor;

PROCEDURE ListView_GetInsertMarkColorA (hwnd : HWND) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessageA (hwnd, LVM_GETINSERTMARKCOLOR, 0, 0));
END ListView_GetInsertMarkColorA;

PROCEDURE ListView_GetInsertMarkColorW (hwnd : HWND) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessageW (hwnd, LVM_GETINSERTMARKCOLOR, 0, 0));
END ListView_GetInsertMarkColorW;

PROCEDURE ListView_SetInfoTip (hwnd : HWND; plvInfoTip : LVSETINFOTIP) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_SETINFOTIP, 0, CAST(LPARAM,ADR(plvInfoTip))));
END ListView_SetInfoTip;

PROCEDURE ListView_SetInfoTipA (hwnd : HWND; plvInfoTip : LVSETINFOTIP) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_SETINFOTIP, 0, CAST(LPARAM,ADR(plvInfoTip))));
END ListView_SetInfoTipA;

PROCEDURE ListView_SetInfoTipW (hwnd : HWND; plvInfoTip : LVSETINFOTIP) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_SETINFOTIP, 0, CAST(LPARAM,ADR(plvInfoTip))));
END ListView_SetInfoTipW;

PROCEDURE ListView_GetSelectedColumn (hwnd : HWND) : UINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_GETSELECTEDCOLUMN, 0, 0);
END ListView_GetSelectedColumn;

PROCEDURE ListView_GetSelectedColumnA (hwnd : HWND) : UINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_GETSELECTEDCOLUMN, 0, 0);
END ListView_GetSelectedColumnA;

PROCEDURE ListView_GetSelectedColumnW (hwnd : HWND) : UINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_GETSELECTEDCOLUMN, 0, 0);
END ListView_GetSelectedColumnW;

PROCEDURE ListView_IsGroupViewEnabled (hwnd : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_ISGROUPVIEWENABLED, 0, 0));
END ListView_IsGroupViewEnabled;

PROCEDURE ListView_IsGroupViewEnabledA (hwnd : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_ISGROUPVIEWENABLED, 0, 0));
END ListView_IsGroupViewEnabledA;

PROCEDURE ListView_IsGroupViewEnabledW (hwnd : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_ISGROUPVIEWENABLED, 0, 0));
END ListView_IsGroupViewEnabledW;

PROCEDURE ListView_GetOutlineColor (hwnd : HWND) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessage (hwnd, LVM_GETOUTLINECOLOR, 0, 0));
END ListView_GetOutlineColor;

PROCEDURE ListView_GetOutlineColorA (hwnd : HWND) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessageA (hwnd, LVM_GETOUTLINECOLOR, 0, 0));
END ListView_GetOutlineColorA;

PROCEDURE ListView_GetOutlineColorW (hwnd : HWND) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessageW (hwnd, LVM_GETOUTLINECOLOR, 0, 0));
END ListView_GetOutlineColorW;

PROCEDURE ListView_SetOutlineColor (hwnd : HWND; color : COLORREF) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessage (hwnd, LVM_SETOUTLINECOLOR, 0, VAL(LPARAM,color)));
END ListView_SetOutlineColor;

PROCEDURE ListView_SetOutlineColorA (hwnd : HWND; color : COLORREF) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessageA (hwnd, LVM_SETOUTLINECOLOR, 0, VAL(LPARAM,color)));
END ListView_SetOutlineColorA;

PROCEDURE ListView_SetOutlineColorW (hwnd : HWND; color : COLORREF) : COLORREF;
BEGIN
	RETURN CAST (COLORREF, SendMessageW (hwnd, LVM_SETOUTLINECOLOR, 0, VAL(LPARAM,color)));
END ListView_SetOutlineColorW;

PROCEDURE ListView_CancelEditLabel (hwnd : HWND);
BEGIN
	FUNC SendMessage (hwnd, LVM_CANCELEDITLABEL, 0, 0);
END ListView_CancelEditLabel;

PROCEDURE ListView_CancelEditLabelA (hwnd : HWND);
BEGIN
	FUNC SendMessageA (hwnd, LVM_CANCELEDITLABEL, 0, 0);
END ListView_CancelEditLabelA;

PROCEDURE ListView_CancelEditLabelW (hwnd : HWND);
BEGIN
	FUNC SendMessageW (hwnd, LVM_CANCELEDITLABEL, 0, 0);
END ListView_CancelEditLabelW;

PROCEDURE ListView_MapIndexToID (hwnd : HWND; index : UINT) : UINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_MAPINDEXTOID, index, 0);
END ListView_MapIndexToID;

PROCEDURE ListView_MapIndexToIDA (hwnd : HWND; index : UINT) : UINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_MAPINDEXTOID, index, 0);
END ListView_MapIndexToIDA;

PROCEDURE ListView_MapIndexToIDW (hwnd : HWND; index : UINT) : UINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_MAPINDEXTOID, index, 0);
END ListView_MapIndexToIDW;

PROCEDURE ListView_MapIDToIndex (hwnd : HWND; index : UINT) : UINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_MAPIDTOINDEX, index, 0);
END ListView_MapIDToIndex;

PROCEDURE ListView_MapIDToIndexA (hwnd : HWND; index : UINT) : UINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_MAPIDTOINDEX, index, 0);
END ListView_MapIDToIndexA;

PROCEDURE ListView_MapIDToIndexW (hwnd : HWND; index : UINT) : UINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_MAPIDTOINDEX, index, 0);
END ListView_MapIDToIndexW;

PROCEDURE ListView_IsItemVisible (hwnd : HWND; index : UINT) : UINT;
BEGIN
	RETURN SendMessage (hwnd, LVM_ISITEMVISIBLE, index, 0);
END ListView_IsItemVisible;

PROCEDURE ListView_IsItemVisibleA (hwnd : HWND; index : UINT) : UINT;
BEGIN
	RETURN SendMessageA (hwnd, LVM_ISITEMVISIBLE, index, 0);
END ListView_IsItemVisibleA;

PROCEDURE ListView_IsItemVisibleW (hwnd : HWND; index : UINT) : UINT;
BEGIN
	RETURN SendMessageW (hwnd, LVM_ISITEMVISIBLE, index, 0);
END ListView_IsItemVisibleW;

PROCEDURE ListView_SetGroupHeaderImageList (hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
	RETURN CAST (HIMAGELIST, SendMessage (hwnd, LVM_SETIMAGELIST, LVSIL_GROUPHEADER, CAST(LPARAM,himl)));
END ListView_SetGroupHeaderImageList;

PROCEDURE ListView_SetGroupHeaderImageListA (hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
	RETURN CAST (HIMAGELIST, SendMessageA (hwnd, LVM_SETIMAGELIST, LVSIL_GROUPHEADER, CAST(LPARAM,himl)));
END ListView_SetGroupHeaderImageListA;

PROCEDURE ListView_SetGroupHeaderImageListW (hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
	RETURN CAST (HIMAGELIST, SendMessageW (hwnd, LVM_SETIMAGELIST, LVSIL_GROUPHEADER, CAST(LPARAM,himl)));
END ListView_SetGroupHeaderImageListW;

PROCEDURE ListView_GetGroupHeaderImageList (hwnd : HWND) : HIMAGELIST;
BEGIN
	RETURN CAST (HIMAGELIST, SendMessage (hwnd, LVM_GETIMAGELIST, LVSIL_GROUPHEADER, 0));
END ListView_GetGroupHeaderImageList;

PROCEDURE ListView_GetGroupHeaderImageListA (hwnd : HWND) : HIMAGELIST;
BEGIN
	RETURN CAST (HIMAGELIST, SendMessageA (hwnd, LVM_GETIMAGELIST, LVSIL_GROUPHEADER, 0));
END ListView_GetGroupHeaderImageListA;

PROCEDURE ListView_GetGroupHeaderImageListW (hwnd : HWND) : HIMAGELIST;
BEGIN
	RETURN CAST (HIMAGELIST, SendMessageW (hwnd, LVM_GETIMAGELIST, LVSIL_GROUPHEADER, 0));
END ListView_GetGroupHeaderImageListW;

PROCEDURE ListView_GetEmptyText (hwnd : HWND; VAR pszText : ARRAY OF WCHAR; cchText : UINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_GETEMPTYTEXT, cchText, CAST (LPARAM, ADR(pszText))));
END ListView_GetEmptyText;

PROCEDURE ListView_GetEmptyTextA (hwnd : HWND; VAR pszText : ARRAY OF WCHAR; cchText : UINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_GETEMPTYTEXT, cchText, CAST (LPARAM, ADR(pszText))));
END ListView_GetEmptyTextA;

PROCEDURE ListView_GetEmptyTextW (hwnd : HWND; VAR pszText : ARRAY OF WCHAR; cchText : UINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_GETEMPTYTEXT, cchText, CAST (LPARAM, ADR(pszText))));
END ListView_GetEmptyTextW;

PROCEDURE ListView_GetFooterRect (hwnd : HWND; VAR prc : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_GETFOOTERRECT, 0, CAST(LPARAM,ADR(prc))));
END ListView_GetFooterRect;

PROCEDURE ListView_GetFooterRectA (hwnd : HWND; VAR prc : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_GETFOOTERRECT, 0, CAST(LPARAM,ADR(prc))));
END ListView_GetFooterRectA;

PROCEDURE ListView_GetFooterRectW (hwnd : HWND; VAR prc : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_GETFOOTERRECT, 0, CAST(LPARAM,ADR(prc))));
END ListView_GetFooterRectW;

PROCEDURE ListView_GetFooterInfo (hwnd : HWND; VAR plvfi : LVFOOTERINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_GETFOOTERINFO, 0, CAST(LPARAM,ADR(plvfi))));
END ListView_GetFooterInfo;

PROCEDURE ListView_GetFooterInfoA (hwnd : HWND; VAR plvfi : LVFOOTERINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_GETFOOTERINFO, 0, CAST(LPARAM,ADR(plvfi))));
END ListView_GetFooterInfoA;

PROCEDURE ListView_GetFooterInfoW (hwnd : HWND; VAR plvfi : LVFOOTERINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_GETFOOTERINFO, 0, CAST(LPARAM,ADR(plvfi))));
END ListView_GetFooterInfoW;

PROCEDURE ListView_GetFooterItemRect (hwnd : HWND; iItem : UINT; VAR prc : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_GETFOOTERITEMRECT, iItem, CAST(LPARAM,ADR(prc))));
END ListView_GetFooterItemRect;

PROCEDURE ListView_GetFooterItemRectA (hwnd : HWND; iItem : UINT; VAR prc : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_GETFOOTERITEMRECT, iItem, CAST(LPARAM,ADR(prc))));
END ListView_GetFooterItemRectA;

PROCEDURE ListView_GetFooterItemRectW (hwnd : HWND; iItem : UINT; VAR prc : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_GETFOOTERITEMRECT, iItem, CAST(LPARAM,ADR(prc))));
END ListView_GetFooterItemRectW;

PROCEDURE ListView_GetFooterItem (hwnd : HWND; iItem : UINT; VAR pvi : LVFOOTERITEM) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_GETFOOTERITEM, iItem, CAST(LPARAM,ADR(pvi))));
END ListView_GetFooterItem;

PROCEDURE ListView_GetFooterItemA (hwnd : HWND; iItem : UINT; VAR pvi : LVFOOTERITEM) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_GETFOOTERITEM, iItem, CAST(LPARAM,ADR(pvi))));
END ListView_GetFooterItemA;

PROCEDURE ListView_GetFooterItemW (hwnd : HWND; iItem : UINT; VAR pvi : LVFOOTERITEM) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_GETFOOTERITEM, iItem, CAST(LPARAM,ADR(pvi))));
END ListView_GetFooterItemW;

PROCEDURE ListView_GetItemIndexRect (hwnd : HWND; plvii : LVITEMINDEX; iSubItem : LONG; code : LONG; VAR prc : RECT) : BOOL;
BEGIN
	IF ADR(prc) # NIL THEN
		prc.top := iSubItem;
		prc.left := code;
	END;
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_GETITEMINDEXRECT, CAST (WPARAM, ADR(plvii)), CAST (LPARAM, ADR(prc))));
END ListView_GetItemIndexRect;

PROCEDURE ListView_GetItemIndexRectA (hwnd : HWND; plvii : LVITEMINDEX; iSubItem : LONG; code : LONG; VAR prc : RECT) : BOOL;
BEGIN
	IF ADR(prc) # NIL THEN
		prc.top := iSubItem;
		prc.left := code;
	END;
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_GETITEMINDEXRECT, CAST (WPARAM, ADR(plvii)), CAST (LPARAM, ADR(prc))));
END ListView_GetItemIndexRectA;

PROCEDURE ListView_GetItemIndexRectW (hwnd : HWND; plvii : LVITEMINDEX; iSubItem : LONG; code : LONG; VAR prc : RECT) : BOOL;
BEGIN
	IF ADR(prc) # NIL THEN
		prc.top := iSubItem;
		prc.left := code;
	END;
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_GETITEMINDEXRECT, CAST (WPARAM, ADR(plvii)), CAST (LPARAM, ADR(prc))));
END ListView_GetItemIndexRectW;

PROCEDURE ListView_SetItemIndexState (hwndLV : HWND; plvii : LVITEMINDEX; data : UINT; mask : UINT) : HRESULT;
VAR
	lvi : LV_ITEM;
BEGIN
	lvi.stateMask := mask;
	lvi.state := data;
	RETURN SendMessage (hwndLV, LVM_SETITEMINDEXSTATE, CAST (WPARAM, ADR(plvii)), CAST (LPARAM, ADR(lvi)));
END ListView_SetItemIndexState;

PROCEDURE ListView_SetItemIndexStateA (hwndLV : HWND; plvii : LVITEMINDEX; data : UINT; mask : UINT) : HRESULT;
VAR
	lvi : LV_ITEM;
BEGIN
	lvi.stateMask := mask;
	lvi.state := data;
	RETURN SendMessageA (hwndLV, LVM_SETITEMINDEXSTATE, CAST (WPARAM, ADR(plvii)), CAST (LPARAM, ADR(lvi)));
END ListView_SetItemIndexStateA;

PROCEDURE ListView_SetItemIndexStateW (hwndLV : HWND; plvii : LVITEMINDEX; data : UINT; mask : UINT) : HRESULT;
VAR
	lvi : LV_ITEM;
BEGIN
	lvi.stateMask := mask;
	lvi.state := data;
	RETURN SendMessageW (hwndLV, LVM_SETITEMINDEXSTATE, CAST (WPARAM, ADR(plvii)), CAST (LPARAM, ADR(lvi)));
END ListView_SetItemIndexStateW;

PROCEDURE ListView_GetNextItemIndex (hwnd : HWND; VAR plvii : LVITEMINDEX; flags : LPARAM) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, LVM_GETNEXTITEMINDEX, CAST (WPARAM, ADR(plvii)), MAKELPARAM(flags,0)));
END ListView_GetNextItemIndex;

PROCEDURE ListView_GetNextItemIndexA (hwnd : HWND; VAR plvii : LVITEMINDEX; flags : LPARAM) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, LVM_GETNEXTITEMINDEX, CAST (WPARAM, ADR(plvii)), MAKELPARAM(flags,0)));
END ListView_GetNextItemIndexA;

PROCEDURE ListView_GetNextItemIndexW (hwnd : HWND; VAR plvii : LVITEMINDEX; flags : LPARAM) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, LVM_GETNEXTITEMINDEX, CAST (WPARAM, ADR(plvii)), MAKELPARAM(flags,0)));
END ListView_GetNextItemIndexW;

PROCEDURE ListView_SetBkImage(hwnd : HWND; plvbki : LVBKIMAGE) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_SETBKIMAGE,
                                  0, CAST(LPARAM, ADR(plvbki))));
END ListView_SetBkImage;

PROCEDURE ListView_SetBkImageA(hwnd : HWND; plvbki : LVBKIMAGEA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_SETBKIMAGEA,
                                  0, CAST(LPARAM, ADR(plvbki))));
END ListView_SetBkImageA;

PROCEDURE ListView_SetBkImageW(hwnd : HWND; plvbki : LVBKIMAGEW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_SETBKIMAGEW,
                                  0, CAST(LPARAM, ADR(plvbki))));
END ListView_SetBkImageW;

PROCEDURE ListView_GetBkImage(hwnd : HWND; VAR plvbki : LVBKIMAGE) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  LVM_GETBKIMAGE,
                                  0, CAST(LPARAM, ADR(plvbki))));
END ListView_GetBkImage;

PROCEDURE ListView_GetBkImageA(hwnd : HWND; VAR plvbki : LVBKIMAGEA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  LVM_GETBKIMAGEA,
                                  0, CAST(LPARAM, ADR(plvbki))));
END ListView_GetBkImageA;

PROCEDURE ListView_GetBkImageW(hwnd : HWND; VAR plvbki : LVBKIMAGEW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  LVM_GETBKIMAGEW,
                                  0, CAST(LPARAM, ADR(plvbki))));
END ListView_GetBkImageW;

(*TreeView*)

PROCEDURE TreeView_InsertItem(hwnd : HWND;
                              lpis : TV_INSERTSTRUCT) : HTREEITEM;
BEGIN
    RETURN CAST(HTREEITEM, SendMessage(hwnd,
                                       TVM_INSERTITEM,
                                       0,
                                       CAST(LPARAM, ADR(lpis))
                                       )
               );
END TreeView_InsertItem;

PROCEDURE TreeView_InsertItemA(hwnd : HWND;
                              lpis : TV_INSERTSTRUCTA) : HTREEITEM;
BEGIN
    RETURN CAST(HTREEITEM, SendMessageA(hwnd,
                                        TVM_INSERTITEMA,
                                        0,
                                        CAST(LPARAM, ADR(lpis))
                                        )
               );
END TreeView_InsertItemA;

PROCEDURE TreeView_InsertItemW(hwnd : HWND;
                              lpis : TV_INSERTSTRUCTW) : HTREEITEM;
BEGIN
    RETURN CAST(HTREEITEM, SendMessageW(hwnd,
                                        TVM_INSERTITEMW,
                                        0,
                                        CAST(LPARAM, ADR(lpis))
                                        )
               );
END TreeView_InsertItemW;

PROCEDURE TreeView_DeleteItem(hwnd : HWND; hitem : HTREEITEM) :  BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_DELETEITEM,
                                  0,
                                  CAST(LPARAM, hitem)
                                 )
               );
END TreeView_DeleteItem;

PROCEDURE TreeView_DeleteItemA(hwnd : HWND; hitem : HTREEITEM) :  BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                    TVM_DELETEITEM,
                                    0,
                                    CAST(LPARAM, hitem)
                                   )
               );
END TreeView_DeleteItemA;

PROCEDURE TreeView_DeleteItemW(hwnd : HWND; hitem : HTREEITEM) :  BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  TVM_DELETEITEM,
                                  0,
                                  CAST(LPARAM, hitem)
                                 )
               );
END TreeView_DeleteItemW;

PROCEDURE TreeView_DeleteAllItems(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_DELETEITEM,
                                  0,
                                  CAST(LPARAM, TVI_ROOT)
                                 )
               );
END TreeView_DeleteAllItems;

PROCEDURE TreeView_DeleteAllItemsA(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_DELETEITEM,
                                   0,
                                   CAST(LPARAM, TVI_ROOT)
                                  )
               );
END TreeView_DeleteAllItemsA;

PROCEDURE TreeView_DeleteAllItemsW(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_DELETEITEM,
                                   0,
                                   CAST(LPARAM, TVI_ROOT)
                                  )
               );
END TreeView_DeleteAllItemsW;

PROCEDURE TreeView_Expand(hwnd : HWND;
                          hitem : HTREEITEM;
                          code : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_EXPAND,
                                  VAL(WPARAM, code),
                                  CAST(LPARAM, hitem)
                                 )
               );
END TreeView_Expand;

PROCEDURE TreeView_ExpandA(hwnd : HWND;
                          hitem : HTREEITEM;
                          code : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_EXPAND,
                                   VAL(WPARAM, code),
                                   CAST(LPARAM, hitem)
                                  )
               );
END TreeView_ExpandA;

PROCEDURE TreeView_ExpandW(hwnd : HWND;
                          hitem : HTREEITEM;
                          code : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_EXPAND,
                                   VAL(WPARAM, code),
                                   CAST(LPARAM, hitem)
                                  )
               );
END TreeView_ExpandW;

PROCEDURE TreeView_GetItemRect(hwnd : HWND;
                               hitem : HTREEITEM;
                               VAR prc : RECT;
                               code : WINT) : BOOL;
BEGIN
    prc:HTREEITEM := hitem;
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_GETITEMRECT,
                                  VAL(WPARAM, code),
                                  CAST(LPARAM, ADR(prc))
                                 )
               );
END TreeView_GetItemRect;

PROCEDURE TreeView_GetItemRectA(hwnd : HWND;
                               hitem : HTREEITEM;
                               VAR prc : RECT;
                               code : WINT) : BOOL;
BEGIN
    prc:HTREEITEM := hitem;
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_GETITEMRECT,
                                   VAL(WPARAM, code),
                                   CAST(LPARAM, ADR(prc))
                                  )
               );
END TreeView_GetItemRectA;

PROCEDURE TreeView_GetItemRectW(hwnd : HWND;
                               hitem : HTREEITEM;
                               VAR prc : RECT;
                               code : WINT) : BOOL;
BEGIN
    prc:HTREEITEM := hitem;
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_GETITEMRECT,
                                   VAL(WPARAM, code),
                                   CAST(LPARAM, ADR(prc))
                                  )
               );
END TreeView_GetItemRectW;

PROCEDURE TreeView_GetCount(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, TVM_GETCOUNT, 0, 0));
END TreeView_GetCount;

PROCEDURE TreeView_GetCountA(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageA(hwnd, TVM_GETCOUNT, 0, 0));
END TreeView_GetCountA;

PROCEDURE TreeView_GetCountW(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageW(hwnd, TVM_GETCOUNT, 0, 0));
END TreeView_GetCountW;

PROCEDURE TreeView_GetIndent(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, TVM_GETINDENT, 0, 0));
END TreeView_GetIndent;

PROCEDURE TreeView_GetIndentA(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageA(hwnd, TVM_GETINDENT, 0, 0));
END TreeView_GetIndentA;

PROCEDURE TreeView_GetIndentW(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageW(hwnd, TVM_GETINDENT, 0, 0));
END TreeView_GetIndentW;

PROCEDURE TreeView_SetIndent(hwnd : HWND; indent : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_SETINDENT,
                                  VAL(WPARAM, indent),
                                  0
                                 )
               );
END TreeView_SetIndent;

PROCEDURE TreeView_SetIndentA(hwnd : HWND; indent : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_SETINDENT,
                                   VAL(WPARAM, indent),
                                   0
                                  )
               );
END TreeView_SetIndentA;

PROCEDURE TreeView_SetIndentW(hwnd : HWND; indent : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_SETINDENT,
                                   VAL(WPARAM, indent),
                                   0
                                  )
               );
END TreeView_SetIndentW;

PROCEDURE TreeView_GetImageList(hwnd : HWND; iImage : WPARAM) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd, TVM_GETIMAGELIST, iImage, 0));
END TreeView_GetImageList;

PROCEDURE TreeView_GetImageListA(hwnd : HWND; iImage : WPARAM) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd, TVM_GETIMAGELIST, iImage, 0));
END TreeView_GetImageListA;

PROCEDURE TreeView_GetImageListW(hwnd : HWND; iImage : WPARAM) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd, TVM_GETIMAGELIST, iImage, 0));
END TreeView_GetImageListW;

PROCEDURE TreeView_SetImageList(hwnd : HWND;
                                himl : HIMAGELIST;
                                iImage : WPARAM) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd,
                                        TVM_SETIMAGELIST,
                                        iImage,
                                        CAST(LPARAM, himl)
                                       )
               );
END TreeView_SetImageList;

PROCEDURE TreeView_SetImageListA(hwnd : HWND;
                                himl : HIMAGELIST;
                                iImage : WPARAM) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd,
                                         TVM_SETIMAGELIST,
                                         iImage,
                                         CAST(LPARAM, himl)
                                        )
               );
END TreeView_SetImageListA;

PROCEDURE TreeView_SetImageListW(hwnd : HWND;
                                himl : HIMAGELIST;
                                iImage : WPARAM) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd,
                                         TVM_SETIMAGELIST,
                                         iImage,
                                         CAST(LPARAM, himl)
                                        )
               );
END TreeView_SetImageListW;

PROCEDURE TreeView_GetNextItem(hwnd : HWND;
                               hitem : HTREEITEM;
                               code : WINT) : HTREEITEM;
BEGIN
    RETURN CAST(HTREEITEM, SendMessage(hwnd,
                                       TVM_GETNEXTITEM,
                                       VAL(WPARAM, code),
                                       CAST(LPARAM, hitem)
                                      )
               );
END TreeView_GetNextItem;

PROCEDURE TreeView_GetNextItemA(hwnd : HWND;
                               hitem : HTREEITEM;
                               code : WINT) : HTREEITEM;
BEGIN
    RETURN CAST(HTREEITEM, SendMessage(hwnd,
                                       TVM_GETNEXTITEM,
                                       VAL(WPARAM, code),
                                       CAST(LPARAM, hitem)
                                      )
               );
END TreeView_GetNextItemA;

PROCEDURE TreeView_GetNextItemW(hwnd : HWND;
                               hitem : HTREEITEM;
                               code : WINT) : HTREEITEM;
BEGIN
    RETURN CAST(HTREEITEM, SendMessage(hwnd,
                                       TVM_GETNEXTITEM,
                                       VAL(WPARAM, code),
                                       CAST(LPARAM, hitem)
                                      )
               );
END TreeView_GetNextItemW;

PROCEDURE TreeView_GetChild(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, hitem, TVGN_CHILD);
END TreeView_GetChild;

PROCEDURE TreeView_GetChildA(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, hitem, TVGN_CHILD);
END TreeView_GetChildA;

PROCEDURE TreeView_GetChildW(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, hitem, TVGN_CHILD);
END TreeView_GetChildW;

PROCEDURE TreeView_GetNextSibling(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, hitem, TVGN_NEXT);
END TreeView_GetNextSibling;

PROCEDURE TreeView_GetNextSiblingA(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, hitem, TVGN_NEXT);
END TreeView_GetNextSiblingA;

PROCEDURE TreeView_GetNextSiblingW(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, hitem, TVGN_NEXT);
END TreeView_GetNextSiblingW;

PROCEDURE TreeView_GetPrevSibling(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, hitem, TVGN_PREVIOUS);
END TreeView_GetPrevSibling;

PROCEDURE TreeView_GetPrevSiblingA(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, hitem, TVGN_PREVIOUS);
END TreeView_GetPrevSiblingA;

PROCEDURE TreeView_GetPrevSiblingW(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, hitem, TVGN_PREVIOUS);
END TreeView_GetPrevSiblingW;

PROCEDURE TreeView_GetParent(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, hitem, TVGN_PARENT);
END TreeView_GetParent;

PROCEDURE TreeView_GetParentA(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, hitem, TVGN_PARENT);
END TreeView_GetParentA;

PROCEDURE TreeView_GetParentW(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, hitem, TVGN_PARENT);
END TreeView_GetParentW;

PROCEDURE TreeView_GetFirstVisible(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, NULL_HTREEITEM,  TVGN_FIRSTVISIBLE);
END TreeView_GetFirstVisible;

PROCEDURE TreeView_GetFirstVisibleA(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, NULL_HTREEITEM,  TVGN_FIRSTVISIBLE);
END TreeView_GetFirstVisibleA;

PROCEDURE TreeView_GetFirstVisibleW(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, NULL_HTREEITEM,  TVGN_FIRSTVISIBLE);
END TreeView_GetFirstVisibleW;

PROCEDURE TreeView_GetNextVisible(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
   RETURN TreeView_GetNextItem(hwnd, hitem, TVGN_NEXTVISIBLE);
END TreeView_GetNextVisible;

PROCEDURE TreeView_GetNextVisibleA(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
   RETURN TreeView_GetNextItemA(hwnd, hitem, TVGN_NEXTVISIBLE);
END TreeView_GetNextVisibleA;

PROCEDURE TreeView_GetNextVisibleW(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
   RETURN TreeView_GetNextItemW(hwnd, hitem, TVGN_NEXTVISIBLE);
END TreeView_GetNextVisibleW;

PROCEDURE TreeView_GetPrevVisible(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, hitem, TVGN_PREVIOUSVISIBLE);
END TreeView_GetPrevVisible;

PROCEDURE TreeView_GetPrevVisibleA(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, hitem, TVGN_PREVIOUSVISIBLE);
END TreeView_GetPrevVisibleA;

PROCEDURE TreeView_GetPrevVisibleW(hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, hitem, TVGN_PREVIOUSVISIBLE);
END TreeView_GetPrevVisibleW;

PROCEDURE TreeView_GetSelection(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, NULL_HTREEITEM, TVGN_CARET);
END TreeView_GetSelection;

PROCEDURE TreeView_GetSelectionA(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, NULL_HTREEITEM, TVGN_CARET);
END TreeView_GetSelectionA;

PROCEDURE TreeView_GetSelectionW(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, NULL_HTREEITEM, TVGN_CARET);
END TreeView_GetSelectionW;

PROCEDURE TreeView_GetDropHilight(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, NULL_HTREEITEM, TVGN_DROPHILITE);
END TreeView_GetDropHilight;

PROCEDURE TreeView_GetDropHilightA(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, NULL_HTREEITEM, TVGN_DROPHILITE);
END TreeView_GetDropHilightA;

PROCEDURE TreeView_GetDropHilightW(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, NULL_HTREEITEM, TVGN_DROPHILITE);
END TreeView_GetDropHilightW;

PROCEDURE TreeView_GetRoot(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, NULL_HTREEITEM, TVGN_ROOT);
END TreeView_GetRoot;

PROCEDURE TreeView_GetRootA(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, NULL_HTREEITEM, TVGN_ROOT);
END TreeView_GetRootA;

PROCEDURE TreeView_GetRootW(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, NULL_HTREEITEM, TVGN_ROOT);
END TreeView_GetRootW;

PROCEDURE TreeView_GetLastVisible(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, NULL_HTREEITEM, TVGN_LASTVISIBLE);
END TreeView_GetLastVisible;

PROCEDURE TreeView_GetLastVisibleA(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, NULL_HTREEITEM, TVGN_LASTVISIBLE);
END TreeView_GetLastVisibleA;

PROCEDURE TreeView_GetLastVisibleW(hwnd : HWND) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, NULL_HTREEITEM, TVGN_LASTVISIBLE);
END TreeView_GetLastVisibleW;

PROCEDURE TreeView_GetNextSelected (hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItem(hwnd, hitem, TVGN_NEXTSELECTED);
END TreeView_GetNextSelected;

PROCEDURE TreeView_GetNextSelectedA (hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemA(hwnd, hitem, TVGN_NEXTSELECTED);
END TreeView_GetNextSelectedA;

PROCEDURE TreeView_GetNextSelectedW (hwnd : HWND; hitem : HTREEITEM) : HTREEITEM;
BEGIN
    RETURN TreeView_GetNextItemW(hwnd, hitem, TVGN_NEXTSELECTED);
END TreeView_GetNextSelectedW;

PROCEDURE TreeView_Select(hwnd : HWND;
                          hitem : HTREEITEM;
                          code : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_SELECTITEM,
                                  VAL(WPARAM, code),
                                  CAST(LPARAM, hitem)
                                 )
               );
END TreeView_Select;

PROCEDURE TreeView_SelectA(hwnd : HWND;
                          hitem : HTREEITEM;
                          code : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_SELECTITEM,
                                   VAL(WPARAM, code),
                                   CAST(LPARAM, hitem)
                                  )
               );
END TreeView_SelectA;

PROCEDURE TreeView_SelectW(hwnd : HWND;
                          hitem : HTREEITEM;
                          code : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_SELECTITEM,
                                   VAL(WPARAM, code),
                                   CAST(LPARAM, hitem)
                                  )
               );
END TreeView_SelectW;

PROCEDURE TreeView_SelectItem(hwnd : HWND; hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN TreeView_Select(hwnd, hitem , TVGN_CARET);
END TreeView_SelectItem;

PROCEDURE TreeView_SelectItemA(hwnd : HWND; hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN TreeView_SelectA(hwnd, hitem , TVGN_CARET);
END TreeView_SelectItemA;

PROCEDURE TreeView_SelectItemW(hwnd : HWND; hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN TreeView_SelectW(hwnd, hitem , TVGN_CARET);
END TreeView_SelectItemW;

PROCEDURE TreeView_SelectDropTarget(hwnd : HWND; hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN TreeView_Select(hwnd, hitem, TVGN_DROPHILITE);
END TreeView_SelectDropTarget;

PROCEDURE TreeView_SelectDropTargetA(hwnd : HWND; hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN TreeView_SelectA(hwnd, hitem, TVGN_DROPHILITE);
END TreeView_SelectDropTargetA;

PROCEDURE TreeView_SelectDropTargetW(hwnd : HWND; hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN TreeView_SelectW(hwnd, hitem, TVGN_DROPHILITE);
END TreeView_SelectDropTargetW;

PROCEDURE TreeView_SelectSetFirstVisible(hwnd : HWND;
                                         hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN TreeView_Select(hwnd, hitem, TVGN_FIRSTVISIBLE);
END TreeView_SelectSetFirstVisible;

PROCEDURE TreeView_SelectSetFirstVisibleA(hwnd : HWND;
                                         hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN TreeView_SelectA(hwnd, hitem, TVGN_FIRSTVISIBLE);
END TreeView_SelectSetFirstVisibleA;

PROCEDURE TreeView_SelectSetFirstVisibleW(hwnd : HWND;
                                         hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN TreeView_SelectW(hwnd, hitem, TVGN_FIRSTVISIBLE);
END TreeView_SelectSetFirstVisibleW;

PROCEDURE TreeView_GetItem(hwnd : HWND; VAR pitem : TV_ITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_GETITEM,
                                  0,
                                  CAST(LPARAM, ADR(pitem))
                                 )

               );
END TreeView_GetItem;

PROCEDURE TreeView_GetItemA(hwnd : HWND; VAR pitem : TV_ITEMA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  TVM_GETITEMA,
                                  0,
                                  CAST(LPARAM, ADR(pitem))
                                 )

               );
END TreeView_GetItemA;

PROCEDURE TreeView_GetItemW(hwnd : HWND; VAR pitem : TV_ITEMW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  TVM_GETITEMW,
                                  0,
                                  CAST(LPARAM, ADR(pitem))
                                 )

               );
END TreeView_GetItemW;

PROCEDURE TreeView_SetItem(hwnd : HWND; pitem : TV_ITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_SETITEM,
                                  0,
                                  CAST(LPARAM, ADR(pitem))
                                 )
               );
END TreeView_SetItem;

PROCEDURE TreeView_SetItemA(hwnd : HWND; pitem : TV_ITEMA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  TVM_SETITEMA,
                                  0,
                                  CAST(LPARAM, ADR(pitem))
                                 )
               );
END TreeView_SetItemA;

PROCEDURE TreeView_SetItemW(hwnd : HWND; pitem : TV_ITEMW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  TVM_SETITEMW,
                                  0,
                                  CAST(LPARAM, ADR(pitem))
                                 )
               );
END TreeView_SetItemW;

PROCEDURE TreeView_EditLabel(hwnd : HWND; hitem : HTREEITEM) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwnd,
                                  TVM_EDITLABEL,
                                  0,
                                  CAST(LPARAM, hitem)
                                 )
               );
END TreeView_EditLabel;

PROCEDURE TreeView_EditLabelA(hwnd : HWND; hitem : HTREEITEM) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwnd,
                                  TVM_EDITLABELA,
                                  0,
                                  CAST(LPARAM, hitem)
                                 )
               );
END TreeView_EditLabelA;

PROCEDURE TreeView_EditLabelW(hwnd : HWND; hitem : HTREEITEM) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwnd,
                                  TVM_EDITLABELW,
                                  0,
                                  CAST(LPARAM, hitem)
                                 )
               );
END TreeView_EditLabelW;

PROCEDURE TreeView_GetEditControl(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwnd, TVM_GETEDITCONTROL, 0, 0));
END TreeView_GetEditControl;

PROCEDURE TreeView_GetEditControlA(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwnd, TVM_GETEDITCONTROL, 0, 0));
END TreeView_GetEditControlA;

PROCEDURE TreeView_GetEditControlW(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwnd, TVM_GETEDITCONTROL, 0, 0));
END TreeView_GetEditControlW;

PROCEDURE TreeView_GetVisibleCount(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, TVM_GETVISIBLECOUNT, 0, 0));
END TreeView_GetVisibleCount;

PROCEDURE TreeView_GetVisibleCountA(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageA(hwnd, TVM_GETVISIBLECOUNT, 0, 0));
END TreeView_GetVisibleCountA;

PROCEDURE TreeView_GetVisibleCountW(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageW(hwnd, TVM_GETVISIBLECOUNT, 0, 0));
END TreeView_GetVisibleCountW;

PROCEDURE TreeView_HitTest(hwnd : HWND; lpht : TV_HITTESTINFO) : HTREEITEM;
BEGIN
    RETURN CAST(HTREEITEM, SendMessage(hwnd,
                                       TVM_HITTEST,
                                       0,
                                       CAST(LPARAM, ADR(lpht))
                                      )
               );
END TreeView_HitTest;

PROCEDURE TreeView_HitTestA(hwnd : HWND; lpht : TV_HITTESTINFO) : HTREEITEM;
BEGIN
    RETURN CAST(HTREEITEM, SendMessageA(hwnd,
                                       TVM_HITTEST,
                                       0,
                                       CAST(LPARAM, ADR(lpht))
                                      )
               );
END TreeView_HitTestA;

PROCEDURE TreeView_HitTestW(hwnd : HWND; lpht : TV_HITTESTINFO) : HTREEITEM;
BEGIN
    RETURN CAST(HTREEITEM, SendMessageW(hwnd,
                                       TVM_HITTEST,
                                       0,
                                       CAST(LPARAM, ADR(lpht))
                                      )
               );
END TreeView_HitTestW;

PROCEDURE TreeView_CreateDragImage(hwnd : HWND;
                                   hitem : HTREEITEM) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd,
                                        TVM_CREATEDRAGIMAGE,
                                        0,
                                        CAST(LPARAM, hitem)
                                       )
               );
END TreeView_CreateDragImage;

PROCEDURE TreeView_CreateDragImageA(hwnd : HWND;
                                    hitem : HTREEITEM) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd,
                                         TVM_CREATEDRAGIMAGE,
                                         0,
                                         CAST(LPARAM, hitem)
                                        )
               );
END TreeView_CreateDragImageA;

PROCEDURE TreeView_CreateDragImageW(hwnd : HWND;
                                    hitem : HTREEITEM) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd,
                                         TVM_CREATEDRAGIMAGE,
                                         0,
                                         CAST(LPARAM, hitem)
                                        )
               );
END TreeView_CreateDragImageW;

PROCEDURE TreeView_SortChildren(hwnd : HWND;
                                hitem : HTREEITEM;
                                recurse : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_SORTCHILDREN,
                                  VAL(WPARAM, recurse),
                                  CAST(LPARAM, hitem)
                                 )
               );
END TreeView_SortChildren;

PROCEDURE TreeView_SortChildrenA(hwnd : HWND;
                                 hitem : HTREEITEM;
                                 recurse : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_SORTCHILDREN,
                                   VAL(WPARAM, recurse),
                                   CAST(LPARAM, hitem)
                                  )
               );
END TreeView_SortChildrenA;

PROCEDURE TreeView_SortChildrenW(hwnd : HWND;
                                 hitem : HTREEITEM;
                                 recurse : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_SORTCHILDREN,
                                   VAL(WPARAM, recurse),
                                   CAST(LPARAM, hitem)
                                  )
               );
END TreeView_SortChildrenW;

PROCEDURE TreeView_EnsureVisible(hwnd : HWND; hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_ENSUREVISIBLE,
                                  0,
                                  CAST(LPARAM, hitem)
                                 )

               );
END TreeView_EnsureVisible;

PROCEDURE TreeView_EnsureVisibleA(hwnd : HWND; hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_ENSUREVISIBLE,
                                   0,
                                   CAST(LPARAM, hitem)
                                  )

               );
END TreeView_EnsureVisibleA;

PROCEDURE TreeView_EnsureVisibleW(hwnd : HWND; hitem : HTREEITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_ENSUREVISIBLE,
                                   0,
                                   CAST(LPARAM, hitem)
                                  )

               );
END TreeView_EnsureVisibleW;

PROCEDURE TreeView_SortChildrenCB(hwnd : HWND;
                                  psort : TV_SORTCB;
                                  recurse : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_SORTCHILDRENCB,
                                  VAL(WPARAM, recurse),
                                  CAST(LPARAM, ADR(psort))
                                 )
               );
END TreeView_SortChildrenCB;

PROCEDURE TreeView_SortChildrenCBA(hwnd : HWND;
                                   psort : TV_SORTCB;
                                   recurse : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_SORTCHILDRENCB,
                                   VAL(WPARAM, recurse),
                                   CAST(LPARAM, ADR(psort))
                                  )
               );
END TreeView_SortChildrenCBA;

PROCEDURE TreeView_SortChildrenCBW(hwnd : HWND;
                                   psort : TV_SORTCB;
                                   recurse : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_SORTCHILDRENCB,
                                   VAL(WPARAM, recurse),
                                   CAST(LPARAM, ADR(psort))
                                  )
               );
END TreeView_SortChildrenCBW;

PROCEDURE TreeView_EndEditLabelNow(hwnd : HWND; fCancel : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                   TVM_ENDEDITLABELNOW,
                                   fCancel,
                                   0
                                  )
                );
END TreeView_EndEditLabelNow;

PROCEDURE TreeView_EndEditLabelNowA(hwnd : HWND; fCancel : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_ENDEDITLABELNOW,
                                   fCancel,
                                   0
                                  )
                );
END TreeView_EndEditLabelNowA;

PROCEDURE TreeView_EndEditLabelNowW(hwnd : HWND; fCancel : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_ENDEDITLABELNOW,
                                   fCancel,
                                   0
                                  )
                );
END TreeView_EndEditLabelNowW;

PROCEDURE TreeView_SetToolTips(hwnd : HWND; hwndTT : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwnd,
                                  TVM_SETTOOLTIPS,
                                  CAST(WPARAM, hwndTT), 0));
END TreeView_SetToolTips;

PROCEDURE TreeView_SetToolTipsA(hwnd : HWND; hwndTT : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwnd,
                                   TVM_SETTOOLTIPS,
                                   CAST(WPARAM, hwndTT), 0));
END TreeView_SetToolTipsA;

PROCEDURE TreeView_SetToolTipsW(hwnd : HWND; hwndTT : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwnd,
                                   TVM_SETTOOLTIPS,
                                   CAST(WPARAM, hwndTT), 0));
END TreeView_SetToolTipsW;

PROCEDURE TreeView_GetToolTips(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwnd, TVM_GETTOOLTIPS, 0, 0));
END TreeView_GetToolTips;

PROCEDURE TreeView_GetToolTipsA(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwnd, TVM_GETTOOLTIPS, 0, 0));
END TreeView_GetToolTipsA;

PROCEDURE TreeView_GetToolTipsW(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwnd, TVM_GETTOOLTIPS, 0, 0));
END TreeView_GetToolTipsW;

PROCEDURE TreeView_GetISearchString(hwndTV : HWND;
                                    VAR lpsz : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndTV,
                                  TVM_GETISEARCHSTRING,
                                  0,
                                  CAST(LPARAM, ADR(lpsz))
                                 )
               );
END TreeView_GetISearchString;

PROCEDURE TreeView_GetISearchStringA(hwndTV : HWND;
                                    VAR lpsz : ARRAY OF ACHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndTV,
                                   TVM_GETISEARCHSTRINGA,
                                   0,
                                   CAST(LPARAM, ADR(lpsz))
                                  )
               );
END TreeView_GetISearchStringA;

PROCEDURE TreeView_GetISearchStringW(hwndTV : HWND;
                                    VAR lpsz : ARRAY OF WCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndTV,
                                   TVM_GETISEARCHSTRINGW,
                                   0,
                                   CAST(LPARAM, ADR(lpsz))
                                  )
               );
END TreeView_GetISearchStringW;

PROCEDURE TreeView_SetInsertMark(hwnd : HWND;
                                 hItem : HTREEITEM;
                                 fAfter : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_SETINSERTMARK,
                                  VAL(WPARAM, fAfter),
                                  CAST(LPARAM, hItem)));
END TreeView_SetInsertMark;

PROCEDURE TreeView_SetInsertMarkA(hwnd : HWND;
                                  hItem : HTREEITEM;
                                  fAfter : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_SETINSERTMARK,
                                   VAL(WPARAM, fAfter),
                                   CAST(LPARAM, hItem)));
END TreeView_SetInsertMarkA;

PROCEDURE TreeView_SetInsertMarkW(hwnd : HWND;
                                  hItem : HTREEITEM;
                                  fAfter : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_SETINSERTMARK,
                                   VAL(WPARAM, fAfter),
                                   CAST(LPARAM, hItem)));
END TreeView_SetInsertMarkW;

PROCEDURE TreeView_SetUnicodeFormat(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TVM_SETUNICODEFORMAT,
                                  VAL(WPARAM, fUnicode), 0));
END TreeView_SetUnicodeFormat;

PROCEDURE TreeView_SetUnicodeFormatA(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TVM_SETUNICODEFORMAT,
                                   VAL(WPARAM, fUnicode), 0));
END TreeView_SetUnicodeFormatA;

PROCEDURE TreeView_SetUnicodeFormatW(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TVM_SETUNICODEFORMAT,
                                   VAL(WPARAM, fUnicode), 0));
END TreeView_SetUnicodeFormatW;

PROCEDURE TreeView_GetUnicodeFormat(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, TVM_GETUNICODEFORMAT, 0, 0));
END TreeView_GetUnicodeFormat;

PROCEDURE TreeView_GetUnicodeFormatA(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, TVM_GETUNICODEFORMAT, 0, 0));
END TreeView_GetUnicodeFormatA;

PROCEDURE TreeView_GetUnicodeFormatW(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, TVM_GETUNICODEFORMAT, 0, 0));
END TreeView_GetUnicodeFormatW;

PROCEDURE TreeView_SetItemHeight(hwnd : HWND; iHeight : WINT) : WINT;
BEGIN
    RETURN SendMessage(hwnd, TVM_SETITEMHEIGHT, iHeight, 0);
END TreeView_SetItemHeight;

PROCEDURE TreeView_SetItemHeightA(hwnd : HWND; iHeight : WINT) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, TVM_SETITEMHEIGHT, iHeight, 0);
END TreeView_SetItemHeightA;

PROCEDURE TreeView_SetItemHeightW(hwnd : HWND; iHeight : WINT) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, TVM_SETITEMHEIGHT, iHeight, 0);
END TreeView_SetItemHeightW;

PROCEDURE TreeView_GetItemHeight(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwnd, TVM_GETITEMHEIGHT, 0, 0);
END TreeView_GetItemHeight;

PROCEDURE TreeView_GetItemHeightA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, TVM_GETITEMHEIGHT, 0, 0);
END TreeView_GetItemHeightA;

PROCEDURE TreeView_GetItemHeightW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, TVM_GETITEMHEIGHT, 0, 0);
END TreeView_GetItemHeightW;

PROCEDURE TreeView_SetBkColor(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd,
                                      TVM_SETBKCOLOR,
                                      0, VAL(LPARAM, clr)));
END TreeView_SetBkColor;

PROCEDURE TreeView_SetBkColorA(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd,
                                       TVM_SETBKCOLOR,
                                       0, VAL(LPARAM, clr)));
END TreeView_SetBkColorA;

PROCEDURE TreeView_SetBkColorW(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd,
                                       TVM_SETBKCOLOR,
                                       0, VAL(LPARAM, clr)));
END TreeView_SetBkColorW;

PROCEDURE TreeView_SetTextColor(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd,
                                      TVM_SETTEXTCOLOR,
                                      0, VAL(LPARAM, clr)));
END TreeView_SetTextColor;

PROCEDURE TreeView_SetTextColorA(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd,
                                       TVM_SETTEXTCOLOR,
                                       0, VAL(LPARAM, clr)));
END TreeView_SetTextColorA;

PROCEDURE TreeView_SetTextColorW(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd,
                                       TVM_SETTEXTCOLOR,
                                       0, VAL(LPARAM, clr)));
END TreeView_SetTextColorW;

PROCEDURE TreeView_GetBkColor(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd, TVM_GETBKCOLOR, 0, 0));
END TreeView_GetBkColor;

PROCEDURE TreeView_GetBkColorA(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd, TVM_GETBKCOLOR, 0, 0));
END TreeView_GetBkColorA;

PROCEDURE TreeView_GetBkColorW(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd, TVM_GETBKCOLOR, 0, 0));
END TreeView_GetBkColorW;

PROCEDURE TreeView_GetTextColor(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd, TVM_GETTEXTCOLOR, 0, 0));
END TreeView_GetTextColor;

PROCEDURE TreeView_GetTextColorA(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd, TVM_GETTEXTCOLOR, 0, 0));
END TreeView_GetTextColorA;

PROCEDURE TreeView_GetTextColorW(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd, TVM_GETTEXTCOLOR, 0, 0));
END TreeView_GetTextColorW;

PROCEDURE TreeView_SetScrollTime(hwnd : HWND; uTime : UINT) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, TVM_SETSCROLLTIME, uTime, 0));
END TreeView_SetScrollTime;

PROCEDURE TreeView_SetScrollTimeA(hwnd : HWND; uTime : UINT) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageA(hwnd, TVM_SETSCROLLTIME, uTime, 0));
END TreeView_SetScrollTimeA;

PROCEDURE TreeView_SetScrollTimeW(hwnd : HWND; uTime : UINT) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageW(hwnd, TVM_SETSCROLLTIME, uTime, 0));
END TreeView_SetScrollTimeW;

PROCEDURE TreeView_GetScrollTime(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessage(hwnd, TVM_GETSCROLLTIME, 0, 0));
END TreeView_GetScrollTime;

PROCEDURE TreeView_GetScrollTimeA(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageA(hwnd, TVM_GETSCROLLTIME, 0, 0));
END TreeView_GetScrollTimeA;

PROCEDURE TreeView_GetScrollTimeW(hwnd : HWND) : UINT;
BEGIN
    RETURN CAST(UINT, SendMessageW(hwnd, TVM_GETSCROLLTIME, 0, 0));
END TreeView_GetScrollTimeW;

PROCEDURE TreeView_SetInsertMarkColor(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd,
                                      TVM_SETINSERTMARKCOLOR,
                                      0, VAL(LPARAM, clr)));
END TreeView_SetInsertMarkColor;

PROCEDURE TreeView_SetInsertMarkColorA(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd,
                                       TVM_SETINSERTMARKCOLOR,
                                       0, VAL(LPARAM, clr)));
END TreeView_SetInsertMarkColorA;

PROCEDURE TreeView_SetInsertMarkColorW(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd,
                                       TVM_SETINSERTMARKCOLOR,
                                       0, VAL(LPARAM, clr)));
END TreeView_SetInsertMarkColorW;

PROCEDURE TreeView_GetInsertMarkColor(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd, TVM_GETINSERTMARKCOLOR, 0, 0));
END TreeView_GetInsertMarkColor;

PROCEDURE TreeView_GetInsertMarkColorA(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd, TVM_GETINSERTMARKCOLOR, 0, 0));
END TreeView_GetInsertMarkColorA;

PROCEDURE TreeView_GetInsertMarkColorW(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd, TVM_GETINSERTMARKCOLOR, 0, 0));
END TreeView_GetInsertMarkColorW;

PROCEDURE TreeView_SetBorder (hwndTV : HWND; dwFlags : DWORD; xBorder : SHORT; yBorder : SHORT) : WINT;
BEGIN
    RETURN SendMessage (hwndTV, TVM_SETBORDER, dwFlags, MAKELPARAM (xBorder,yBorder));
END TreeView_SetBorder;

PROCEDURE TreeView_SetBorderA (hwndTV : HWND; dwFlags : DWORD; xBorder : SHORT; yBorder : SHORT) : WINT;
BEGIN
    RETURN SendMessageA (hwndTV, TVM_SETBORDER, dwFlags, MAKELPARAM (xBorder,yBorder));
END TreeView_SetBorderA;

PROCEDURE TreeView_SetBorderW (hwndTV : HWND; dwFlags : DWORD; xBorder : SHORT; yBorder : SHORT) : WINT;
BEGIN
    RETURN SendMessageW (hwndTV, TVM_SETBORDER, dwFlags, MAKELPARAM (xBorder,yBorder));
END TreeView_SetBorderW;

PROCEDURE TreeView_SetItemState (hwndTV : HWND; hItem : HTREEITEM; state : UINT; stateMask : UINT) : UINT;
VAR
	TVi : TVITEM;
BEGIN
	TVi.mask      := TVIF_STATE;
	TVi.hItem     := hItem;
	TVi.stateMask := stateMask;
	TVi.state     := state;
	RETURN SendMessage (hwndTV, TVM_SETITEM, 0, CAST (LPARAM, ADR(TVi)));
END TreeView_SetItemState;

PROCEDURE TreeView_SetItemStateA (hwndTV : HWND; hItem : HTREEITEM; state : UINT; stateMask : UINT) : UINT;
VAR
	TVi : TVITEMA;
BEGIN
	TVi.mask      := TVIF_STATE;
	TVi.hItem     := hItem;
	TVi.stateMask := stateMask;
	TVi.state     := state;
	RETURN SendMessageA (hwndTV, TVM_SETITEM, 0, CAST (LPARAM, ADR(TVi)));
END TreeView_SetItemStateA;

PROCEDURE TreeView_SetItemStateW (hwndTV : HWND; hItem : HTREEITEM; state : UINT; stateMask : UINT) : UINT;
VAR
	TVi : TVITEMW;
BEGIN
	TVi.mask      := TVIF_STATE;
	TVi.hItem     := hItem;
	TVi.stateMask := stateMask;
	TVi.state     := state;
	RETURN SendMessageW (hwndTV, TVM_SETITEM, 0, CAST (LPARAM, ADR(TVi)));
END TreeView_SetItemStateW;

PROCEDURE TreeView_SetCheckState (hwndTV : HWND; hItem : HTREEITEM; fCheck : BOOL) : UINT;
BEGIN
	RETURN TreeView_SetItemState (hwndTV, hItem, INDEXTOSTATEIMAGEMASK(ORD(fCheck)+1), TVIS_STATEIMAGEMASK);
END TreeView_SetCheckState;

PROCEDURE TreeView_SetCheckStateA (hwndTV : HWND; hItem : HTREEITEM; fCheck : BOOL) : UINT;
BEGIN
	RETURN TreeView_SetItemStateA (hwndTV, hItem, INDEXTOSTATEIMAGEMASK(ORD(fCheck)+1), TVIS_STATEIMAGEMASK);
END TreeView_SetCheckStateA;

PROCEDURE TreeView_SetCheckStateW (hwndTV : HWND; hItem : HTREEITEM; fCheck : BOOL) : UINT;
BEGIN
	RETURN TreeView_SetItemStateW (hwndTV, hItem, INDEXTOSTATEIMAGEMASK(ORD(fCheck)+1), TVIS_STATEIMAGEMASK);
END TreeView_SetCheckStateW;

PROCEDURE TreeView_GetItemState (hwndTV : HWND; hItem : HTREEITEM; stateMask : UINT) : UINT;
BEGIN
	RETURN SendMessage (hwndTV, TVM_GETITEMSTATE, CAST (WPARAM, hItem), stateMask);
END TreeView_GetItemState;

PROCEDURE TreeView_GetItemStateA (hwndTV : HWND; hItem : HTREEITEM; stateMask : UINT) : UINT;
BEGIN
	RETURN SendMessageA (hwndTV, TVM_GETITEMSTATE, CAST (WPARAM, hItem), stateMask);
END TreeView_GetItemStateA;

PROCEDURE TreeView_GetItemStateW (hwndTV : HWND; hItem : HTREEITEM; stateMask : UINT) : UINT;
BEGIN
	RETURN SendMessageW (hwndTV, TVM_GETITEMSTATE, CAST (WPARAM, hItem), stateMask);
END TreeView_GetItemStateW;

PROCEDURE TreeView_GetCheckState (hwndTV : HWND; hItem : HTREEITEM) : UINT;
BEGIN
	RETURN (SendMessage (hwndTV, TVM_GETITEMSTATE, CAST (WPARAM, hItem), TVIS_STATEIMAGEMASK) SHR 12) - 1;
END TreeView_GetCheckState;

PROCEDURE TreeView_GetCheckStateA (hwndTV : HWND; hItem : HTREEITEM) : UINT;
BEGIN
	RETURN (SendMessageA (hwndTV, TVM_GETITEMSTATE, CAST (WPARAM, hItem), TVIS_STATEIMAGEMASK) SHR 12) - 1;
END TreeView_GetCheckStateA;

PROCEDURE TreeView_GetCheckStateW (hwndTV : HWND; hItem : HTREEITEM) : UINT;
BEGIN
	RETURN (SendMessageW (hwndTV, TVM_GETITEMSTATE, CAST (WPARAM, hItem), TVIS_STATEIMAGEMASK) SHR 12) - 1;
END TreeView_GetCheckStateW;

PROCEDURE TreeView_SetLineColor (hwndTV : HWND; clrLine : COLORREF) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessage (hwndTV, TVM_SETLINECOLOR, 0, VAL(LPARAM, clrLine)));
END TreeView_SetLineColor;

PROCEDURE TreeView_SetLineColorA (hwndTV : HWND; clrLine : COLORREF) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessageA (hwndTV, TVM_SETLINECOLOR, 0, VAL(LPARAM, clrLine)));
END TreeView_SetLineColorA;

PROCEDURE TreeView_SetLineColorW (hwndTV : HWND; clrLine : COLORREF) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessageW (hwndTV, TVM_SETLINECOLOR, 0, VAL(LPARAM, clrLine)));
END TreeView_SetLineColorW;

PROCEDURE TreeView_GetLineColor (hwndTV : HWND) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessage (hwndTV, TVM_GETLINECOLOR, 0, 0));
END TreeView_GetLineColor;

PROCEDURE TreeView_GetLineColorA (hwndTV : HWND) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessageA (hwndTV, TVM_GETLINECOLOR, 0, 0));
END TreeView_GetLineColorA;

PROCEDURE TreeView_GetLineColorW (hwndTV : HWND) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessageW (hwndTV, TVM_GETLINECOLOR, 0, 0));
END TreeView_GetLineColorW;

PROCEDURE TreeView_MapAccIDToHTREEITEM (hwnd : HWND; id : UINT) : HTREEITEM;
BEGIN
	RETURN CAST (HTREEITEM, SendMessage (hwnd, TVM_MAPACCIDTOHTREEITEM, id, 0));
END TreeView_MapAccIDToHTREEITEM;

PROCEDURE TreeView_MapAccIDToHTREEITEMA (hwnd : HWND; id : UINT) : HTREEITEM;
BEGIN
	RETURN CAST (HTREEITEM, SendMessageA (hwnd, TVM_MAPACCIDTOHTREEITEM, id, 0));
END TreeView_MapAccIDToHTREEITEMA;

PROCEDURE TreeView_MapAccIDToHTREEITEMW (hwnd : HWND; id : UINT) : HTREEITEM;
BEGIN
	RETURN CAST (HTREEITEM, SendMessageW (hwnd, TVM_MAPACCIDTOHTREEITEM, id, 0));
END TreeView_MapAccIDToHTREEITEMW;

PROCEDURE TreeView_MapHTREEITEMtoAccID (hwnd : HWND; htreeitem : HTREEITEM) : UINT;
BEGIN
	RETURN SendMessage (hwnd, TVM_MAPHTREEITEMTOACCID, CAST (WPARAM, htreeitem), 0);
END TreeView_MapHTREEITEMtoAccID;

PROCEDURE TreeView_MapHTREEITEMtoAccIDA (hwnd : HWND; htreeitem : HTREEITEM) : UINT;
BEGIN
	RETURN SendMessageA (hwnd, TVM_MAPHTREEITEMTOACCID, CAST (WPARAM, htreeitem), 0);
END TreeView_MapHTREEITEMtoAccIDA;

PROCEDURE TreeView_MapHTREEITEMtoAccIDW (hwnd : HWND; htreeitem : HTREEITEM) : UINT;
BEGIN
	RETURN SendMessageW (hwnd, TVM_MAPHTREEITEMTOACCID, CAST (WPARAM, htreeitem), 0);
END TreeView_MapHTREEITEMtoAccIDW;

PROCEDURE TreeView_SetExtendedStyle (hwnd : HWND; dw : DWORD; mask : UINT) : HRESULT;
BEGIN
	RETURN SendMessage (hwnd, TVM_SETEXTENDEDSTYLE, mask, dw);
END TreeView_SetExtendedStyle;

PROCEDURE TreeView_SetExtendedStyleA (hwnd : HWND; dw : DWORD; mask : UINT) : HRESULT;
BEGIN
	RETURN SendMessageA (hwnd, TVM_SETEXTENDEDSTYLE, mask, dw);
END TreeView_SetExtendedStyleA;

PROCEDURE TreeView_SetExtendedStyleW (hwnd : HWND; dw : DWORD; mask : UINT) : HRESULT;
BEGIN
	RETURN SendMessageW (hwnd, TVM_SETEXTENDEDSTYLE, mask, dw);
END TreeView_SetExtendedStyleW;

PROCEDURE TreeView_GetExtendedStyle (hwnd : HWND) : DWORD;
BEGIN
	RETURN SendMessage (hwnd, TVM_GETEXTENDEDSTYLE, 0, 0);
END TreeView_GetExtendedStyle;

PROCEDURE TreeView_GetExtendedStyleA (hwnd : HWND) : DWORD;
BEGIN
	RETURN SendMessageA (hwnd, TVM_GETEXTENDEDSTYLE, 0, 0);
END TreeView_GetExtendedStyleA;

PROCEDURE TreeView_GetExtendedStyleW (hwnd : HWND) : DWORD;
BEGIN
	RETURN SendMessageW (hwnd, TVM_GETEXTENDEDSTYLE, 0, 0);
END TreeView_GetExtendedStyleW;

PROCEDURE TreeView_SetAutoScrollInfo (hwnd : HWND; uPixPerSec : UINT; uUpdateTime : UINT) : LRESULT;
BEGIN
	RETURN SendMessage (hwnd, TVM_SETAUTOSCROLLINFO, uPixPerSec, uUpdateTime);
END TreeView_SetAutoScrollInfo;

PROCEDURE TreeView_SetAutoScrollInfoA (hwnd : HWND; uPixPerSec : UINT; uUpdateTime : UINT) : LRESULT;
BEGIN
	RETURN SendMessageA (hwnd, TVM_SETAUTOSCROLLINFO, uPixPerSec, uUpdateTime);
END TreeView_SetAutoScrollInfoA;

PROCEDURE TreeView_SetAutoScrollInfoW (hwnd : HWND; uPixPerSec : UINT; uUpdateTime : UINT) : LRESULT;
BEGIN
	RETURN SendMessageW (hwnd, TVM_SETAUTOSCROLLINFO, uPixPerSec, uUpdateTime);
END TreeView_SetAutoScrollInfoW;

PROCEDURE TreeView_SetHot (hwndTV : HWND; hitem : HTREEITEM) : LRESULT;
BEGIN
	RETURN SendMessage (hwndTV, TVM_SETHOT, 0, CAST (LPARAM, hitem));
END TreeView_SetHot;

PROCEDURE TreeView_SetHotA (hwndTV : HWND; hitem : HTREEITEM) : LRESULT;
BEGIN
	RETURN SendMessageA (hwndTV, TVM_SETHOT, 0, CAST (LPARAM, hitem));
END TreeView_SetHotA;

PROCEDURE TreeView_SetHotW (hwndTV : HWND; hitem : HTREEITEM) : LRESULT;
BEGIN
	RETURN SendMessageW (hwndTV, TVM_SETHOT, 0, CAST (LPARAM, hitem));
END TreeView_SetHotW;

PROCEDURE TreeView_ShowInfoTip (hwnd : HWND; hitem : HTREEITEM) : DWORD;
BEGIN
	RETURN SendMessage (hwnd, TVM_SHOWINFOTIP, 0, CAST (LPARAM, hitem));
END TreeView_ShowInfoTip;

PROCEDURE TreeView_ShowInfoTipA (hwnd : HWND; hitem : HTREEITEM) : DWORD;
BEGIN
	RETURN SendMessageA (hwnd, TVM_SHOWINFOTIP, 0, CAST (LPARAM, hitem));
END TreeView_ShowInfoTipA;

PROCEDURE TreeView_ShowInfoTipW (hwnd : HWND; hitem : HTREEITEM) : DWORD;
BEGIN
	RETURN SendMessageW (hwnd, TVM_SHOWINFOTIP, 0, CAST (LPARAM, hitem));
END TreeView_ShowInfoTipW;

PROCEDURE TreeView_GetItemPartRect (hwnd : HWND; hitem : HTREEITEM; VAR prc : RECT; partid : TVITEMPART) : BOOL;
VAR
	info : TVGETITEMPARTRECTINFO;
BEGIN
	info.hti := hitem;
	info.prc := ADR(prc);
  	info.partID := partid;
	RETURN CAST (BOOL, SendMessage (hwnd, TVM_GETITEMPARTRECT, 0, CAST (LPARAM, ADR(info))));
END TreeView_GetItemPartRect;

PROCEDURE TreeView_GetItemPartRectA (hwnd : HWND; hitem : HTREEITEM; VAR prc : RECT; partid : TVITEMPART) : BOOL;
VAR
	info : TVGETITEMPARTRECTINFO;
BEGIN
	info.hti := hitem;
	info.prc := ADR(prc);
  	info.partID := partid;
	RETURN CAST (BOOL, SendMessageA (hwnd, TVM_GETITEMPARTRECT, 0, CAST (LPARAM, ADR(info))));
END TreeView_GetItemPartRectA;

PROCEDURE TreeView_GetItemPartRectW (hwnd : HWND; hitem : HTREEITEM; VAR prc : RECT; partid : TVITEMPART) : BOOL;
VAR
	info : TVGETITEMPARTRECTINFO;
BEGIN
	info.hti := hitem;
	info.prc := ADR(prc);
  	info.partID := partid;
	RETURN CAST (BOOL, SendMessageW (hwnd, TVM_GETITEMPARTRECT, 0, CAST (LPARAM, ADR(info))));
END TreeView_GetItemPartRectW;

(* Tab Control *)

PROCEDURE TabCtrl_GetImageList(hwnd : HWND) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd, TCM_GETIMAGELIST, 0, 0));
END TabCtrl_GetImageList;

PROCEDURE TabCtrl_GetImageListA(hwnd : HWND) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd, TCM_GETIMAGELIST, 0, 0));
END TabCtrl_GetImageListA;

PROCEDURE TabCtrl_GetImageListW(hwnd : HWND) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd, TCM_GETIMAGELIST, 0, 0));
END TabCtrl_GetImageListW;

PROCEDURE TabCtrl_SetImageList(hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessage(hwnd,
                                        TCM_SETIMAGELIST,
                                        0,
                                        CAST(LPARAM, himl)
                                       )
               );
END TabCtrl_SetImageList;

PROCEDURE TabCtrl_SetImageListA(hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageA(hwnd,
                                        TCM_SETIMAGELIST,
                                        0,
                                        CAST(LPARAM, himl)
                                       )
               );
END TabCtrl_SetImageListA;

PROCEDURE TabCtrl_SetImageListW(hwnd : HWND; himl : HIMAGELIST) : HIMAGELIST;
BEGIN
    RETURN CAST(HIMAGELIST, SendMessageW(hwnd,
                                        TCM_SETIMAGELIST,
                                        0,
                                        CAST(LPARAM, himl)
                                       )
               );
END TabCtrl_SetImageListW;

PROCEDURE TabCtrl_GetItemCount(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage((hwnd), TCM_GETITEMCOUNT, 0, 0);
END TabCtrl_GetItemCount;

PROCEDURE TabCtrl_GetItemCountA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA((hwnd), TCM_GETITEMCOUNT, 0, 0);
END TabCtrl_GetItemCountA;

PROCEDURE TabCtrl_GetItemCountW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW((hwnd), TCM_GETITEMCOUNT, 0, 0);
END TabCtrl_GetItemCountW;

PROCEDURE TabCtrl_GetItem(hwnd : HWND; iItem : WPARAM; VAR pitem : TCITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TCM_GETITEM,
                                  iItem,
                                  CAST(LPARAM, ADR(pitem))
                                 )
               );
END TabCtrl_GetItem;

PROCEDURE TabCtrl_GetItemA(hwnd : HWND; iItem : WPARAM; VAR pitem : TCITEMA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TCM_GETITEMA,
                                   iItem,
                                   CAST(LPARAM, ADR(pitem))
                                  )
               );
END TabCtrl_GetItemA;

PROCEDURE TabCtrl_GetItemW(hwnd : HWND; iItem : WPARAM; VAR pitem : TCITEMW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TCM_GETITEMW,
                                   iItem,
                                   CAST(LPARAM, ADR(pitem))
                                  )
               );
END TabCtrl_GetItemW;

PROCEDURE TabCtrl_SetItem(hwnd : HWND;
                          iItem : WINT;
                          pitem : TCITEM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TCM_SETITEM,
                                  VAL(WPARAM, iItem),
                                  CAST(LPARAM, ADR(pitem))
                                 )
               );
END TabCtrl_SetItem;

PROCEDURE TabCtrl_SetItemA(hwnd : HWND;
                          iItem : WINT;
                          pitem : TCITEMA) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TCM_SETITEMA,
                                   VAL(WPARAM, iItem),
                                   CAST(LPARAM, ADR(pitem))
                                  )
               );
END TabCtrl_SetItemA;

PROCEDURE TabCtrl_SetItemW(hwnd : HWND;
                           iItem : WINT;
                           pitem : TCITEMW) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TCM_SETITEMW,
                                   VAL(WPARAM, iItem),
                                   CAST(LPARAM, ADR(pitem))
                                  )
               );
END TabCtrl_SetItemW;

PROCEDURE TabCtrl_InsertItem(hwnd : HWND;
                             iItem : WINT;
                             pitem : TCITEM) : WINT;
BEGIN
    RETURN SendMessage(hwnd,
                       TCM_INSERTITEM,
                       VAL(WPARAM, iItem),
                       CAST(LPARAM, ADR(pitem))
                      );
END TabCtrl_InsertItem;

PROCEDURE TabCtrl_InsertItemA(hwnd : HWND;
                              iItem : WINT;
                              pitem : TCITEMA) : WINT;
BEGIN
    RETURN SendMessageA(hwnd,
                        TCM_INSERTITEMA,
                        VAL(WPARAM, iItem),
                        CAST(LPARAM, ADR(pitem))
                       );
END TabCtrl_InsertItemA;

PROCEDURE TabCtrl_InsertItemW(hwnd : HWND;
                              iItem : WINT;
                              pitem : TCITEMW) : WINT;
BEGIN
    RETURN SendMessageW(hwnd,
                        TCM_INSERTITEMW,
                        VAL(WPARAM, iItem),
                        CAST(LPARAM, ADR(pitem))
                       );
END TabCtrl_InsertItemW;

PROCEDURE TabCtrl_DeleteItem(hwnd : HWND; i : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, TCM_DELETEITEM, VAL(WPARAM, i), 0));
END TabCtrl_DeleteItem;

PROCEDURE TabCtrl_DeleteItemA(hwnd : HWND; i : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, TCM_DELETEITEM, VAL(WPARAM, i), 0));
END TabCtrl_DeleteItemA;

PROCEDURE TabCtrl_DeleteItemW(hwnd : HWND; i : WINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, TCM_DELETEITEM, VAL(WPARAM, i), 0));
END TabCtrl_DeleteItemW;

PROCEDURE TabCtrl_DeleteAllItems(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, TCM_DELETEALLITEMS, 0, 0));
END TabCtrl_DeleteAllItems;

PROCEDURE TabCtrl_DeleteAllItemsA(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, TCM_DELETEALLITEMS, 0, 0));
END TabCtrl_DeleteAllItemsA;

PROCEDURE TabCtrl_DeleteAllItemsW(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, TCM_DELETEALLITEMS, 0, 0));
END TabCtrl_DeleteAllItemsW;

PROCEDURE TabCtrl_GetItemRect(hwnd : HWND; i : WINT; VAR prc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TCM_GETITEMRECT,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(prc))
                                 )
               );
END TabCtrl_GetItemRect;

PROCEDURE TabCtrl_GetItemRectA(hwnd : HWND; i : WINT; VAR prc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                  TCM_GETITEMRECT,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(prc))
                                 )
               );
END TabCtrl_GetItemRectA;

PROCEDURE TabCtrl_GetItemRectW(hwnd : HWND; i : WINT; VAR prc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                  TCM_GETITEMRECT,
                                  VAL(WPARAM, i),
                                  CAST(LPARAM, ADR(prc))
                                 )
               );
END TabCtrl_GetItemRectW;

PROCEDURE TabCtrl_GetCurSel(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwnd, TCM_GETCURSEL, 0, 0);
END TabCtrl_GetCurSel;

PROCEDURE TabCtrl_GetCurSelA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, TCM_GETCURSEL, 0, 0);
END TabCtrl_GetCurSelA;

PROCEDURE TabCtrl_GetCurSelW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, TCM_GETCURSEL, 0, 0);
END TabCtrl_GetCurSelW;

PROCEDURE TabCtrl_SetCurSel(hwnd : HWND; i : WPARAM) : WINT;
BEGIN
    RETURN SendMessage(hwnd, TCM_SETCURSEL, i, 0);
END TabCtrl_SetCurSel;

PROCEDURE TabCtrl_SetCurSelA(hwnd : HWND; i : WPARAM) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, TCM_SETCURSEL, i, 0);
END TabCtrl_SetCurSelA;

PROCEDURE TabCtrl_SetCurSelW(hwnd : HWND; i : WPARAM) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, TCM_SETCURSEL, i, 0);
END TabCtrl_SetCurSelW;

PROCEDURE TabCtrl_HitTest(hwndTC : HWND;
                          pinfo : TCHITTESTINFO) : WINT;
BEGIN
    RETURN SendMessage(hwndTC,
                       TCM_HITTEST,
                       0,
                       CAST(LPARAM, ADR(pinfo))
                      );
END TabCtrl_HitTest;

PROCEDURE TabCtrl_HitTestA(hwndTC : HWND;
                           pinfo : TCHITTESTINFO) : WINT;
BEGIN
    RETURN SendMessageA(hwndTC,
                        TCM_HITTEST,
                        0,
                        CAST(LPARAM, ADR(pinfo))
                       );
END TabCtrl_HitTestA;

PROCEDURE TabCtrl_HitTestW(hwndTC : HWND;
                           pinfo : TCHITTESTINFO) : WINT;
BEGIN
    RETURN SendMessageW(hwndTC,
                        TCM_HITTEST,
                        0,
                        CAST(LPARAM, ADR(pinfo))
                       );
END TabCtrl_HitTestW;

PROCEDURE TabCtrl_SetItemExtra(hwndTC : HWND; cb : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwndTC,
                                  TCM_SETITEMEXTRA,
                                  cb,
                                  0
                                 )
               );
END TabCtrl_SetItemExtra;

PROCEDURE TabCtrl_SetItemExtraA(hwndTC : HWND; cb : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwndTC,
                                   TCM_SETITEMEXTRA,
                                   cb,
                                   0
                                  )
               );
END TabCtrl_SetItemExtraA;

PROCEDURE TabCtrl_SetItemExtraW(hwndTC : HWND; cb : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwndTC,
                                   TCM_SETITEMEXTRA,
                                   cb,
                                   0
                                  )
               );
END TabCtrl_SetItemExtraW;

PROCEDURE TabCtrl_AdjustRect(hwnd : HWND;
                             bLarger : BOOL;
                             VAR prc : RECT);
BEGIN
    SendMessage(hwnd,
                TCM_ADJUSTRECT,
                VAL(WPARAM, bLarger),
                CAST(LPARAM, ADR(prc)));
END TabCtrl_AdjustRect;

PROCEDURE TabCtrl_AdjustRectA(hwnd : HWND;
                             bLarger : BOOL;
                             VAR prc : RECT);
BEGIN
    SendMessageA(hwnd,
                 TCM_ADJUSTRECT,
                 VAL(WPARAM, bLarger),
                 CAST(LPARAM, ADR(prc)));
END TabCtrl_AdjustRectA;

PROCEDURE TabCtrl_AdjustRectW(hwnd : HWND;
                             bLarger : BOOL;
                             VAR prc : RECT);
BEGIN
    SendMessageW(hwnd,
                 TCM_ADJUSTRECT,
                 VAL(WPARAM, bLarger),
                 CAST(LPARAM, ADR(prc)));
END TabCtrl_AdjustRectW;

PROCEDURE TabCtrl_SetItemSize(hwnd : HWND; x: WORD; y : WORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessage(hwnd,
                                   TCM_SETITEMSIZE,
                                   0,
                                   MAKELPARAM(x,y)
                                  )
               );
END TabCtrl_SetItemSize;

PROCEDURE TabCtrl_SetItemSizeA(hwnd : HWND; x: WORD; y : WORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageA(hwnd,
                                    TCM_SETITEMSIZE,
                                    0,
                                    MAKELPARAM(x,y)
                                   )
               );
END TabCtrl_SetItemSizeA;

PROCEDURE TabCtrl_SetItemSizeW(hwnd : HWND; x: WORD; y : WORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageW(hwnd,
                                    TCM_SETITEMSIZE,
                                    0,
                                    MAKELPARAM(x,y)
                                   )
               );
END TabCtrl_SetItemSizeW;

PROCEDURE TabCtrl_RemoveImage(hwnd : HWND; i : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, TCM_REMOVEIMAGE, i, 0));
END TabCtrl_RemoveImage;

PROCEDURE TabCtrl_RemoveImageA(hwnd : HWND; i : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, TCM_REMOVEIMAGE, i, 0));
END TabCtrl_RemoveImageA;

PROCEDURE TabCtrl_RemoveImageW(hwnd : HWND; i : WPARAM) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, TCM_REMOVEIMAGE, i, 0));
END TabCtrl_RemoveImageW;

PROCEDURE TabCtrl_SetPadding(hwnd : HWND; cx : WORD; cy : WORD);
BEGIN
    SendMessage(hwnd, TCM_SETPADDING, 0, MAKELPARAM(cx, cy))
END TabCtrl_SetPadding;

PROCEDURE TabCtrl_SetPaddingA(hwnd : HWND; cx : WORD; cy : WORD);
BEGIN
    SendMessageA(hwnd, TCM_SETPADDING, 0, MAKELPARAM(cx, cy))
END TabCtrl_SetPaddingA;

PROCEDURE TabCtrl_SetPaddingW(hwnd : HWND; cx : WORD; cy : WORD);
BEGIN
    SendMessageW(hwnd, TCM_SETPADDING, 0, MAKELPARAM(cx, cy))
END TabCtrl_SetPaddingW;

PROCEDURE TabCtrl_GetRowCount(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwnd, TCM_GETROWCOUNT, 0, 0);
END TabCtrl_GetRowCount;

PROCEDURE TabCtrl_GetRowCountA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, TCM_GETROWCOUNT, 0, 0);
END TabCtrl_GetRowCountA;

PROCEDURE TabCtrl_GetRowCountW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, TCM_GETROWCOUNT, 0, 0);
END TabCtrl_GetRowCountW;

PROCEDURE TabCtrl_GetToolTips(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwnd, TCM_GETTOOLTIPS, 0, 0));
END TabCtrl_GetToolTips;

PROCEDURE TabCtrl_GetToolTipsA(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwnd, TCM_GETTOOLTIPS, 0, 0));
END TabCtrl_GetToolTipsA;

PROCEDURE TabCtrl_GetToolTipsW(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwnd, TCM_GETTOOLTIPS, 0, 0));
END TabCtrl_GetToolTipsW;

PROCEDURE TabCtrl_SetToolTips(hwnd : HWND; hwndTT : HWND);
BEGIN
    SendMessage(hwnd, TCM_SETTOOLTIPS, CAST(WPARAM, hwndTT), 0);
END TabCtrl_SetToolTips;

PROCEDURE TabCtrl_SetToolTipsA(hwnd : HWND; hwndTT : HWND);
BEGIN
    SendMessageA(hwnd, TCM_SETTOOLTIPS, CAST(WPARAM, hwndTT), 0);
END TabCtrl_SetToolTipsA;

PROCEDURE TabCtrl_SetToolTipsW(hwnd : HWND; hwndTT : HWND);
BEGIN
    SendMessageW(hwnd, TCM_SETTOOLTIPS, CAST(WPARAM, hwndTT), 0);
END TabCtrl_SetToolTipsW;

PROCEDURE TabCtrl_GetCurFocus(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwnd, TCM_GETCURFOCUS, 0, 0);
END TabCtrl_GetCurFocus;

PROCEDURE TabCtrl_GetCurFocusA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, TCM_GETCURFOCUS, 0, 0);
END TabCtrl_GetCurFocusA;

PROCEDURE TabCtrl_GetCurFocusW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, TCM_GETCURFOCUS, 0, 0);
END TabCtrl_GetCurFocusW;

PROCEDURE TabCtrl_SetCurFocus(hwnd : HWND; i : WPARAM) : WINT;
BEGIN
    RETURN SendMessage(hwnd, TCM_SETCURFOCUS, i, 0);
END TabCtrl_SetCurFocus;

PROCEDURE TabCtrl_SetCurFocusA(hwnd : HWND; i : WPARAM) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, TCM_SETCURFOCUS, i, 0);
END TabCtrl_SetCurFocusA;

PROCEDURE TabCtrl_SetCurFocusW(hwnd : HWND; i : WPARAM) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, TCM_SETCURFOCUS, i, 0);
END TabCtrl_SetCurFocusW;

PROCEDURE TabCtrl_SetMinTabWidth(hwnd : HWND; x : WINT) : WINT;
BEGIN
    RETURN SendMessage(hwnd, TCM_SETMINTABWIDTH, 0, x);
END TabCtrl_SetMinTabWidth;

PROCEDURE TabCtrl_SetMinTabWidthA(hwnd : HWND; x : WINT) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, TCM_SETMINTABWIDTH, 0, x);
END TabCtrl_SetMinTabWidthA;

PROCEDURE TabCtrl_SetMinTabWidthW(hwnd : HWND; x : WINT) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, TCM_SETMINTABWIDTH, 0, x);
END TabCtrl_SetMinTabWidthW;

PROCEDURE TabCtrl_DeselectAll(hwnd : HWND; fExcludeFocus : BOOL);
BEGIN
    SendMessage(hwnd, TCM_DESELECTALL, VAL(WPARAM, fExcludeFocus), 0);
END TabCtrl_DeselectAll;

PROCEDURE TabCtrl_DeselectAllA(hwnd : HWND; fExcludeFocus : BOOL);
BEGIN
    SendMessageA(hwnd, TCM_DESELECTALL, VAL(WPARAM, fExcludeFocus), 0);
END TabCtrl_DeselectAllA;

PROCEDURE TabCtrl_DeselectAllW(hwnd : HWND; fExcludeFocus : BOOL);
BEGIN
    SendMessageW(hwnd, TCM_DESELECTALL, VAL(WPARAM, fExcludeFocus), 0);
END TabCtrl_DeselectAllW;

PROCEDURE TabCtrl_HighlightItem(hwnd : HWND;
                                i : WINT;
                                fHighlight : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TCM_HIGHLIGHTITEM,
                                  i,
                                  MAKELONG (ORD(fHighlight), 0)));
END TabCtrl_HighlightItem;

PROCEDURE TabCtrl_HighlightItemA(hwnd : HWND;
                                 i : WINT;
                                 fHighlight : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TCM_HIGHLIGHTITEM,
                                   i,
                                   MAKELONG (ORD(fHighlight), 0)));
END TabCtrl_HighlightItemA;

PROCEDURE TabCtrl_HighlightItemW(hwnd : HWND;
                                 i : WINT;
                                 fHighlight : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TCM_HIGHLIGHTITEM,
                                   i,
                                   MAKELONG (ORD(fHighlight), 0)));
END TabCtrl_HighlightItemW;

PROCEDURE TabCtrl_SetExtendedStyle(hwnd : HWND; dw : DWORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessage(hwnd, TCM_SETEXTENDEDSTYLE, 0, dw));
END TabCtrl_SetExtendedStyle;

PROCEDURE TabCtrl_SetExtendedStyleA(hwnd : HWND; dw : DWORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageA(hwnd, TCM_SETEXTENDEDSTYLE, 0, dw));
END TabCtrl_SetExtendedStyleA;

PROCEDURE TabCtrl_SetExtendedStyleW(hwnd : HWND; dw : DWORD) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageW(hwnd, TCM_SETEXTENDEDSTYLE, 0, dw));
END TabCtrl_SetExtendedStyleW;

PROCEDURE TabCtrl_GetExtendedStyle(hwnd : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessage(hwnd, TCM_GETEXTENDEDSTYLE, 0, 0));
END TabCtrl_GetExtendedStyle;

PROCEDURE TabCtrl_GetExtendedStyleA(hwnd : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageA(hwnd, TCM_GETEXTENDEDSTYLE, 0, 0));
END TabCtrl_GetExtendedStyleA;

PROCEDURE TabCtrl_GetExtendedStyleW(hwnd : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageW(hwnd, TCM_GETEXTENDEDSTYLE, 0, 0));
END TabCtrl_GetExtendedStyleW;

PROCEDURE TabCtrl_SetUnicodeFormat(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  TCM_SETUNICODEFORMAT,
                                  ORD(fUnicode), 0));
END TabCtrl_SetUnicodeFormat;

PROCEDURE TabCtrl_SetUnicodeFormatA(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   TCM_SETUNICODEFORMAT,
                                   ORD(fUnicode), 0));
END TabCtrl_SetUnicodeFormatA;

PROCEDURE TabCtrl_SetUnicodeFormatW(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   TCM_SETUNICODEFORMAT,
                                   ORD(fUnicode), 0));
END TabCtrl_SetUnicodeFormatW;

PROCEDURE TabCtrl_GetUnicodeFormat(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, TCM_GETUNICODEFORMAT, 0, 0));
END TabCtrl_GetUnicodeFormat;

PROCEDURE TabCtrl_GetUnicodeFormatA(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, TCM_GETUNICODEFORMAT, 0, 0));
END TabCtrl_GetUnicodeFormatA;

PROCEDURE TabCtrl_GetUnicodeFormatW(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, TCM_GETUNICODEFORMAT, 0, 0));
END TabCtrl_GetUnicodeFormatW;

(* Animation *)

PROCEDURE Animate_Create(hwndP : HWND;
                         id : UINT;
                         dwStyle : DWORD;
                         hInstance : HINSTANCE) : HWND;
BEGIN
    RETURN CreateWindow(ANIMATE_CLASS,
                        NIL_STR,
                        dwStyle,
                        0, 0, 0, 0,
                        hwndP,
                        CAST(HMENU, MAKEADR(id)),
                        hInstance,
                        NIL);
END Animate_Create;

PROCEDURE Animate_CreateA(hwndP : HWND;
                          id : UINT;
                          dwStyle : DWORD;
                          hInstance : HINSTANCE) : HWND;
BEGIN
    RETURN CreateWindowA(ANIMATE_CLASS,
                         NIL_ASTR,
                         dwStyle,
                         0, 0, 0, 0,
                         hwndP,
                         CAST(HMENU, MAKEADR(id)),
                         hInstance,
                         NIL);
END Animate_CreateA;

PROCEDURE Animate_CreateW(hwndP : HWND;
                          id : UINT;
                          dwStyle : DWORD;
                          hInstance : HINSTANCE) : HWND;
BEGIN
    RETURN CreateWindowW(ANIMATE_CLASS,
                         NIL_USTR,
                         dwStyle,
                         0, 0, 0, 0,
                         hwndP,
                         CAST(HMENU, MAKEADR(id)),
                         hInstance,
                         NIL);
END Animate_CreateW;

PROCEDURE Animate_Open(hwnd : HWND; szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  ACM_OPEN,
                                  0,
                                  CAST(LPARAM, ADR(szName))
                                 )
               );
END Animate_Open;

PROCEDURE Animate_OpenA(hwnd : HWND; szName : ARRAY OF ACHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   ACM_OPENA,
                                   0,
                                   CAST(LPARAM, ADR(szName))
                                  )
               );
END Animate_OpenA;

PROCEDURE Animate_OpenW(hwnd : HWND; szName : ARRAY OF WCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   ACM_OPENW,
                                   0,
                                   CAST(LPARAM, ADR(szName))
                                  )
               );
END Animate_OpenW;

PROCEDURE Animate_OpenEx(hwnd : HWND;
                         hInst  : HINSTANCE;
                         szName : ARRAY OF TCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  ACM_OPEN,
                                  CAST(WPARAM, hInst),
                                  CAST(LPARAM, ADR(szName))));
END Animate_OpenEx;

PROCEDURE Animate_OpenExA(hwnd : HWND;
                          hInst  : HINSTANCE;
                          szName : ARRAY OF ACHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   ACM_OPENA,
                                   CAST(WPARAM, hInst),
                                   CAST(LPARAM, ADR(szName))));
END Animate_OpenExA;

PROCEDURE Animate_OpenExW(hwnd : HWND;
                          hInst  : HINSTANCE;
                          szName : ARRAY OF WCHAR) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   ACM_OPENW,
                                   CAST(WPARAM, hInst),
                                   CAST(LPARAM, ADR(szName))));
END Animate_OpenExW;

PROCEDURE Animate_Play(hwnd : HWND;
                       from : WORD;
                       to : WORD;
                       rep : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  ACM_PLAY,
                                  rep,
                                  MAKELPARAM(from, to)
                                 )
               );
END Animate_Play;

PROCEDURE Animate_PlayA(hwnd : HWND;
                        from : WORD;
                        to : WORD;
                        rep : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   ACM_PLAY,
                                   rep,
                                   MAKELPARAM(from, to)
                                  )
               );
END Animate_PlayA;

PROCEDURE Animate_PlayW(hwnd : HWND;
                        from : WORD;
                        to : WORD;
                        rep : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   ACM_PLAY,
                                   rep,
                                   MAKELPARAM(from, to)
                                  )
               );
END Animate_PlayW;

PROCEDURE Animate_Stop(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, ACM_STOP, 0, 0));
END Animate_Stop;

PROCEDURE Animate_StopA(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, ACM_STOP, 0, 0));
END Animate_StopA;

PROCEDURE Animate_StopW(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, ACM_STOP, 0, 0));
END Animate_StopW;

PROCEDURE Animate_Close(hwnd : HWND) : BOOL;
BEGIN
    RETURN Animate_Open(hwnd, NIL_STR);
END Animate_Close;

PROCEDURE Animate_CloseA(hwnd : HWND) : BOOL;
BEGIN
    RETURN Animate_OpenA(hwnd, NIL_ASTR);
END Animate_CloseA;

PROCEDURE Animate_CloseW(hwnd : HWND) : BOOL;
BEGIN
    RETURN Animate_OpenW(hwnd, NIL_USTR);
END Animate_CloseW;

PROCEDURE Animate_Seek(hwnd : HWND; frame : WORD) : BOOL;
BEGIN
    RETURN Animate_Play(hwnd, frame, frame, 1);
END Animate_Seek;

PROCEDURE Animate_SeekA(hwnd : HWND; frame : WORD) : BOOL;
BEGIN
    RETURN Animate_PlayA(hwnd, frame, frame, 1);
END Animate_SeekA;

PROCEDURE Animate_SeekW(hwnd : HWND; frame : WORD) : BOOL;
BEGIN
    RETURN Animate_PlayW(hwnd, frame, frame, 1);
END Animate_SeekW;

(* MonthCal control *)

PROCEDURE MonthCal_GetCurSel(hmc : HWND; VAR pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(
                                  hmc,
                                  MCM_GETCURSEL,
                                  0, CAST(LPARAM, ADR(pst))));
END MonthCal_GetCurSel;

PROCEDURE MonthCal_GetCurSelA(hmc : HWND; VAR pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(
                                   hmc,
                                   MCM_GETCURSEL,
                                   0, CAST(LPARAM, ADR(pst))));
END MonthCal_GetCurSelA;

PROCEDURE MonthCal_GetCurSelW(hmc : HWND; VAR pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(
                                   hmc,
                                   MCM_GETCURSEL,
                                   0, CAST(LPARAM, ADR(pst))));
END MonthCal_GetCurSelW;

PROCEDURE MonthCal_SetCurSel(hmc : HWND; pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(
                                  hmc, MCM_SETCURSEL,
                                  0, CAST(LPARAM, ADR(pst))));
END MonthCal_SetCurSel;

PROCEDURE MonthCal_SetCurSelA(hmc : HWND; pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(
                                   hmc, MCM_SETCURSEL,
                                   0, CAST(LPARAM, ADR(pst))));
END MonthCal_SetCurSelA;

PROCEDURE MonthCal_SetCurSelW(hmc : HWND; pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(
                                   hmc, MCM_SETCURSEL,
                                   0, CAST(LPARAM, ADR(pst))));
END MonthCal_SetCurSelW;

PROCEDURE MonthCal_GetMaxSelCount(hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessage(hmc, MCM_GETMAXSELCOUNT, 0, 0);
END MonthCal_GetMaxSelCount;

PROCEDURE MonthCal_GetMaxSelCountA(hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessageA(hmc, MCM_GETMAXSELCOUNT, 0, 0);
END MonthCal_GetMaxSelCountA;

PROCEDURE MonthCal_GetMaxSelCountW(hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessageW(hmc, MCM_GETMAXSELCOUNT, 0, 0);
END MonthCal_GetMaxSelCountW;

PROCEDURE MonthCal_SetMaxSelCount(hmc : HWND; n : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hmc, MCM_SETMAXSELCOUNT, n, 0));
END MonthCal_SetMaxSelCount;

PROCEDURE MonthCal_SetMaxSelCountA(hmc : HWND; n : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hmc, MCM_SETMAXSELCOUNT, n, 0));
END MonthCal_SetMaxSelCountA;

PROCEDURE MonthCal_SetMaxSelCountW(hmc : HWND; n : UINT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hmc, MCM_SETMAXSELCOUNT, n, 0));
END MonthCal_SetMaxSelCountW;

PROCEDURE MonthCal_GetSelRange(hmc : HWND;
                               VAR rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(
                                  hmc,
                                  MCM_GETSELRANGE,
                                  0, CAST(LPARAM, ADR(rgst))));
END MonthCal_GetSelRange;

PROCEDURE MonthCal_GetSelRangeA(hmc : HWND;
                               VAR rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(
                                   hmc,
                                   MCM_GETSELRANGE,
                                   0, CAST(LPARAM, ADR(rgst))));
END MonthCal_GetSelRangeA;

PROCEDURE MonthCal_GetSelRangeW(hmc : HWND;
                               VAR rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(
                                   hmc,
                                   MCM_GETSELRANGE,
                                   0, CAST(LPARAM, ADR(rgst))));
END MonthCal_GetSelRangeW;

PROCEDURE MonthCal_SetSelRange(hmc : HWND;
                               rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(
                                  hmc,
                                  MCM_SETSELRANGE,
                                  0, CAST(LPARAM, ADR(rgst))));
END MonthCal_SetSelRange;

PROCEDURE MonthCal_SetSelRangeA(hmc : HWND;
                               rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(
                                   hmc,
                                   MCM_SETSELRANGE,
                                   0, CAST(LPARAM, ADR(rgst))));
END MonthCal_SetSelRangeA;

PROCEDURE MonthCal_SetSelRangeW(hmc : HWND;
                               rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(
                                   hmc,
                                   MCM_SETSELRANGE,
                                   0, CAST(LPARAM, ADR(rgst))));
END MonthCal_SetSelRangeW;

PROCEDURE MonthCal_GetMonthRange(hmc : HWND;
                                 gmr : DWORD;
                                 VAR rgst : ARRAY OF SYSTEMTIME) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessage(
                                   hmc,
                                   MCM_GETMONTHRANGE,
                                   gmr, CAST(LPARAM, ADR(rgst))));
END MonthCal_GetMonthRange;

PROCEDURE MonthCal_GetMonthRangeA(hmc : HWND;
                                  gmr : DWORD;
                                  VAR rgst : ARRAY OF SYSTEMTIME) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageA(
                                    hmc,
                                    MCM_GETMONTHRANGE,
                                    gmr, CAST(LPARAM, ADR(rgst))));
END MonthCal_GetMonthRangeA;

PROCEDURE MonthCal_GetMonthRangeW(hmc : HWND;
                                  gmr : DWORD;
                                  VAR rgst : ARRAY OF SYSTEMTIME) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageW(
                                    hmc,
                                    MCM_GETMONTHRANGE,
                                    gmr, CAST(LPARAM, ADR(rgst))));
END MonthCal_GetMonthRangeW;

PROCEDURE MonthCal_SetDayState (hwndMC : HWND; iMonth : WINT; lpDayStateArray : ARRAY OF MONTHDAYSTATE) : BOOL;
BEGIN
    RETURN	CAST (BOOL,	SendMessage (hwndMC, MCM_SETDAYSTATE, iMonth, CAST (LPARAM, ADR(lpDayStateArray))));
END MonthCal_SetDayState;

PROCEDURE MonthCal_SetDayStateA (hwndMC : HWND; iMonth : WINT; lpDayStateArray : ARRAY OF MONTHDAYSTATE) : BOOL;
BEGIN
    RETURN	CAST (BOOL,	SendMessageA (hwndMC, MCM_SETDAYSTATE, iMonth, CAST (LPARAM, ADR(lpDayStateArray))));
END MonthCal_SetDayStateA;

PROCEDURE MonthCal_SetDayStateW (hwndMC : HWND; iMonth : WINT; lpDayStateArray : ARRAY OF MONTHDAYSTATE) : BOOL;
BEGIN
    RETURN	CAST (BOOL,	SendMessageW (hwndMC, MCM_SETDAYSTATE, iMonth, CAST (LPARAM, ADR(lpDayStateArray))));
END MonthCal_SetDayStateW;

PROCEDURE MonthCal_GetMinReqRect(hmc : HWND; VAR prc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(
                                  hmc,
                                  MCM_GETMINREQRECT,
                                  0, CAST(LPARAM, ADR(prc))));
END MonthCal_GetMinReqRect;

PROCEDURE MonthCal_GetMinReqRectA(hmc : HWND; VAR prc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(
                                  hmc,
                                  MCM_GETMINREQRECT,
                                  0, CAST(LPARAM, ADR(prc))));
END MonthCal_GetMinReqRectA;

PROCEDURE MonthCal_GetMinReqRectW(hmc : HWND; VAR prc : RECT) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(
                                  hmc,
                                  MCM_GETMINREQRECT,
                                  0, CAST(LPARAM, ADR(prc))));
END MonthCal_GetMinReqRectW;

PROCEDURE MonthCal_SetColor(hmc : HWND; iColor : WINT; clr : COLORREF);
BEGIN
    SendMessage(hmc, MCM_SETCOLOR, iColor, clr);
END MonthCal_SetColor;

PROCEDURE MonthCal_SetColorA(hmc : HWND; iColor : WINT; clr : COLORREF);
BEGIN
    SendMessageA(hmc, MCM_SETCOLOR, iColor, clr);
END MonthCal_SetColorA;

PROCEDURE MonthCal_SetColorW(hmc : HWND; iColor : WINT; clr : COLORREF);
BEGIN
    SendMessageW(hmc, MCM_SETCOLOR, iColor, clr);
END MonthCal_SetColorW;

PROCEDURE MonthCal_GetColor(hmc : HWND; iColor : WINT) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hmc, MCM_GETCOLOR, iColor, 0));
END MonthCal_GetColor;

PROCEDURE MonthCal_GetColorA(hmc : HWND; iColor : WINT) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hmc, MCM_GETCOLOR, iColor, 0));
END MonthCal_GetColorA;

PROCEDURE MonthCal_GetColorW(hmc : HWND; iColor : WINT) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hmc, MCM_GETCOLOR, iColor, 0));
END MonthCal_GetColorW;

PROCEDURE MonthCal_SetToday(hmc : HWND; pst : SYSTEMTIME);
BEGIN
    SendMessage(hmc, MCM_SETTODAY, 0, CAST(LPARAM, ADR(pst)));
END MonthCal_SetToday;

PROCEDURE MonthCal_SetTodayA(hmc : HWND; pst : SYSTEMTIME);
BEGIN
    SendMessageA(hmc, MCM_SETTODAY, 0, CAST(LPARAM, ADR(pst)));
END MonthCal_SetTodayA;

PROCEDURE MonthCal_SetTodayW(hmc : HWND; pst : SYSTEMTIME);
BEGIN
    SendMessageW(hmc, MCM_SETTODAY, 0, CAST(LPARAM, ADR(pst)));
END MonthCal_SetTodayW;

PROCEDURE MonthCal_GetToday(hmc : HWND; VAR pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(
                                  hmc,
                                  MCM_GETTODAY,
                                  0, CAST(LPARAM, ADR(pst))));
END MonthCal_GetToday;

PROCEDURE MonthCal_GetTodayA(hmc : HWND; VAR pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(
                                   hmc,
                                   MCM_GETTODAY,
                                   0, CAST(LPARAM, ADR(pst))));
END MonthCal_GetTodayA;

PROCEDURE MonthCal_GetTodayW(hmc : HWND; VAR pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(
                                   hmc,
                                   MCM_GETTODAY,
                                   0, CAST(LPARAM, ADR(pst))));
END MonthCal_GetTodayW;

PROCEDURE MonthCal_HitTest(hmc : HWND; VAR pinfo : MCHITTESTINFO);
BEGIN
    SendMessage(hmc, MCM_HITTEST, 0, CAST(LPARAM, ADR(pinfo)));
END MonthCal_HitTest;

PROCEDURE MonthCal_HitTestA(hmc : HWND; VAR pinfo : MCHITTESTINFO);
BEGIN
    SendMessageA(hmc, MCM_HITTEST, 0, CAST(LPARAM, ADR(pinfo)));
END MonthCal_HitTestA;

PROCEDURE MonthCal_HitTestW(hmc : HWND; VAR pinfo : MCHITTESTINFO);
BEGIN
    SendMessageW(hmc, MCM_HITTEST, 0, CAST(LPARAM, ADR(pinfo)));
END MonthCal_HitTestW;

PROCEDURE MonthCal_SetFirstDayOfWeek(hmc : HWND; iDay : WINT);
BEGIN
    SendMessage(hmc, MCM_SETFIRSTDAYOFWEEK, 0, iDay);
END MonthCal_SetFirstDayOfWeek;

PROCEDURE MonthCal_SetFirstDayOfWeekA(hmc : HWND; iDay : WINT);
BEGIN
    SendMessageA(hmc, MCM_SETFIRSTDAYOFWEEK, 0, iDay);
END MonthCal_SetFirstDayOfWeekA;

PROCEDURE MonthCal_SetFirstDayOfWeekW(hmc : HWND; iDay : WINT);
BEGIN
    SendMessageW(hmc, MCM_SETFIRSTDAYOFWEEK, 0, iDay);
END MonthCal_SetFirstDayOfWeekW;

PROCEDURE MonthCal_GetFirstDayOfWeek(hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessage(hmc, MCM_GETFIRSTDAYOFWEEK, 0, 0);
END MonthCal_GetFirstDayOfWeek;

PROCEDURE MonthCal_GetFirstDayOfWeekA(hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessageA(hmc, MCM_GETFIRSTDAYOFWEEK, 0, 0);
END MonthCal_GetFirstDayOfWeekA;

PROCEDURE MonthCal_GetFirstDayOfWeekW(hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessageW(hmc, MCM_GETFIRSTDAYOFWEEK, 0, 0);
END MonthCal_GetFirstDayOfWeekW;

PROCEDURE MonthCal_GetRange(hmc : HWND; VAR rgst : SYSTEMTIME) : DWORD;
BEGIN
    RETURN SendMessage(hmc, MCM_GETRANGE, 0, CAST(LPARAM, ADR(rgst)));
END MonthCal_GetRange;

PROCEDURE MonthCal_GetRangeA(hmc : HWND; VAR rgst : SYSTEMTIME) : DWORD;
BEGIN
    RETURN SendMessageA(hmc, MCM_GETRANGE, 0, CAST(LPARAM, ADR(rgst)));
END MonthCal_GetRangeA;

PROCEDURE MonthCal_GetRangeW(hmc : HWND; VAR rgst : SYSTEMTIME) : DWORD;
BEGIN
    RETURN SendMessageW(hmc, MCM_GETRANGE, 0, CAST(LPARAM, ADR(rgst)));
END MonthCal_GetRangeW;

PROCEDURE MonthCal_SetRange(hmc : HWND;
                            gd : DWORD;
                            rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(
                                  hmc,
                                  MCM_SETRANGE,
                                  gd,
                                  CAST(LPARAM, ADR(rgst))));
END MonthCal_SetRange;

PROCEDURE MonthCal_SetRangeA(hmc : HWND;
                             gd : DWORD;
                             rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(
                                   hmc,
                                   MCM_SETRANGE,
                                   gd,
                                   CAST(LPARAM, ADR(rgst))));
END MonthCal_SetRangeA;

PROCEDURE MonthCal_SetRangeW(hmc : HWND;
                             gd : DWORD;
                             rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(
                                   hmc,
                                   MCM_SETRANGE,
                                   gd,
                                   CAST(LPARAM, ADR(rgst))));
END MonthCal_SetRangeW;

PROCEDURE MonthCal_GetMonthDelta(hmc : HWND) : WINT;
BEGIN
    RETURN SendMessage(hmc, MCM_GETMONTHDELTA, 0, 0);
END MonthCal_GetMonthDelta;

PROCEDURE MonthCal_GetMonthDeltaA(hmc : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hmc, MCM_GETMONTHDELTA, 0, 0);
END MonthCal_GetMonthDeltaA;

PROCEDURE MonthCal_GetMonthDeltaW(hmc : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hmc, MCM_GETMONTHDELTA, 0, 0);
END MonthCal_GetMonthDeltaW;

PROCEDURE MonthCal_SetMonthDelta(hmc : HWND; n : WINT) : WINT;
BEGIN
    RETURN SendMessage(hmc, MCM_SETMONTHDELTA, n, 0);
END MonthCal_SetMonthDelta;

PROCEDURE MonthCal_SetMonthDeltaA(hmc : HWND; n : WINT) : WINT;
BEGIN
    RETURN SendMessageA(hmc, MCM_SETMONTHDELTA, n, 0);
END MonthCal_SetMonthDeltaA;

PROCEDURE MonthCal_SetMonthDeltaW(hmc : HWND; n : WINT) : WINT;
BEGIN
    RETURN SendMessageW(hmc, MCM_SETMONTHDELTA, n, 0);
END MonthCal_SetMonthDeltaW;

PROCEDURE MonthCal_GetMaxTodayWidth(hmc : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessage(hmc, MCM_GETMAXTODAYWIDTH, 0, 0));
END MonthCal_GetMaxTodayWidth;

PROCEDURE MonthCal_GetMaxTodayWidthA(hmc : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageA(hmc, MCM_GETMAXTODAYWIDTH, 0, 0));
END MonthCal_GetMaxTodayWidthA;

PROCEDURE MonthCal_GetMaxTodayWidthW(hmc : HWND) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageW(hmc, MCM_GETMAXTODAYWIDTH, 0, 0));
END MonthCal_GetMaxTodayWidthW;

PROCEDURE MonthCal_SetUnicodeFormat(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd,
                                  MCM_SETUNICODEFORMAT,
                                  ORD(fUnicode), 0));
END MonthCal_SetUnicodeFormat;

PROCEDURE MonthCal_SetUnicodeFormatA(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd,
                                   MCM_SETUNICODEFORMAT,
                                   ORD(fUnicode), 0));
END MonthCal_SetUnicodeFormatA;

PROCEDURE MonthCal_SetUnicodeFormatW(hwnd : HWND; fUnicode : BOOL) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd,
                                   MCM_SETUNICODEFORMAT,
                                   ORD(fUnicode), 0));
END MonthCal_SetUnicodeFormatW;

PROCEDURE MonthCal_GetUnicodeFormat(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hwnd, MCM_GETUNICODEFORMAT, 0, 0));
END MonthCal_GetUnicodeFormat;

PROCEDURE MonthCal_GetUnicodeFormatA(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hwnd, MCM_GETUNICODEFORMAT, 0, 0));
END MonthCal_GetUnicodeFormatA;

PROCEDURE MonthCal_GetUnicodeFormatW(hwnd : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hwnd, MCM_GETUNICODEFORMAT, 0, 0));
END MonthCal_GetUnicodeFormatW;

PROCEDURE MonthCal_GetCurrentView (hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessage (hmc, MCM_GETCURRENTVIEW, 0, 0);
END MonthCal_GetCurrentView;

PROCEDURE MonthCal_GetCurrentViewA (hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessageA (hmc, MCM_GETCURRENTVIEW, 0, 0);
END MonthCal_GetCurrentViewA;

PROCEDURE MonthCal_GetCurrentViewW (hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessageW (hmc, MCM_GETCURRENTVIEW, 0, 0);
END MonthCal_GetCurrentViewW;

PROCEDURE MonthCal_GetCalendarCount (hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessage (hmc, MCM_GETCALENDARCOUNT, 0, 0);
END MonthCal_GetCalendarCount;

PROCEDURE MonthCal_GetCalendarCountA (hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessageA (hmc, MCM_GETCALENDARCOUNT, 0, 0);
END MonthCal_GetCalendarCountA;

PROCEDURE MonthCal_GetCalendarCountW (hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessageW (hmc, MCM_GETCALENDARCOUNT, 0, 0);
END MonthCal_GetCalendarCountW;

PROCEDURE MonthCal_GetCalendarGridInfo (hmc : HWND; VAR pmcGridInfo : MCGRIDINFO) : BOOL;
BEGIN
    RETURN CAST (BOOL, SendMessage (hmc, MCM_GETCALENDARGRIDINFO, 0, CAST (LPARAM, ADR(pmcGridInfo))));
END MonthCal_GetCalendarGridInfo;

PROCEDURE MonthCal_GetCalendarGridInfoA (hmc : HWND; VAR pmcGridInfo : MCGRIDINFO) : BOOL;
BEGIN
    RETURN CAST (BOOL, SendMessageA (hmc, MCM_GETCALENDARGRIDINFO, 0, CAST (LPARAM, ADR(pmcGridInfo))));
END MonthCal_GetCalendarGridInfoA;

PROCEDURE MonthCal_GetCalendarGridInfoW (hmc : HWND; VAR pmcGridInfo : MCGRIDINFO) : BOOL;
BEGIN
    RETURN CAST (BOOL, SendMessageW (hmc, MCM_GETCALENDARGRIDINFO, 0, CAST (LPARAM, ADR(pmcGridInfo))));
END MonthCal_GetCalendarGridInfoW;

PROCEDURE MonthCal_GetCALID (hmc : HWND) : CALID;
BEGIN
    RETURN CAST (CALID, SendMessage (hmc, MCM_GETCALID, 0, 0));
END MonthCal_GetCALID;

PROCEDURE MonthCal_GetCALIDA (hmc : HWND) : CALID;
BEGIN
    RETURN CAST (CALID, SendMessageA (hmc, MCM_GETCALID, 0, 0));
END MonthCal_GetCALIDA;

PROCEDURE MonthCal_GetCALIDW (hmc : HWND) : CALID;
BEGIN
    RETURN CAST (CALID, SendMessageW (hmc, MCM_GETCALID, 0, 0));
END MonthCal_GetCALIDW;

PROCEDURE MonthCal_SetCALID (hmc : HWND; calid : UINT) : LRESULT;
BEGIN
    RETURN SendMessage (hmc, MCM_SETCALID, calid, 0);
END MonthCal_SetCALID;

PROCEDURE MonthCal_SetCALIDA (hmc : HWND; calid : UINT) : LRESULT;
BEGIN
    RETURN SendMessageA (hmc, MCM_SETCALID, calid, 0);
END MonthCal_SetCALIDA;

PROCEDURE MonthCal_SetCALIDW (hmc : HWND; calid : UINT) : LRESULT;
BEGIN
    RETURN SendMessageW (hmc, MCM_SETCALID, calid, 0);
END MonthCal_SetCALIDW;

PROCEDURE MonthCal_SizeRectToMin (hmc : HWND; VAR prc : RECT) : LRESULT;
BEGIN
    RETURN SendMessage (hmc, MCM_SIZERECTTOMIN, 0, CAST (LPARAM, ADR(prc)));
END MonthCal_SizeRectToMin;

PROCEDURE MonthCal_SizeRectToMinA (hmc : HWND; VAR prc : RECT) : LRESULT;
BEGIN
    RETURN SendMessageA (hmc, MCM_SIZERECTTOMIN, 0, CAST (LPARAM, ADR(prc)));
END MonthCal_SizeRectToMinA;

PROCEDURE MonthCal_SizeRectToMinW (hmc : HWND; VAR prc : RECT) : LRESULT;
BEGIN
    RETURN SendMessageW (hmc, MCM_SIZERECTTOMIN, 0, CAST (LPARAM, ADR(prc)));
END MonthCal_SizeRectToMinW;

PROCEDURE MonthCal_SetCalendarBorder (hmc : HWND; fset : BOOL; xyborder : WINT) : LRESULT;
BEGIN
    RETURN SendMessage (hmc, MCM_SETCALENDARBORDER, VAL (WPARAM, fset), xyborder);
END MonthCal_SetCalendarBorder;

PROCEDURE MonthCal_SetCalendarBorderA (hmc : HWND; fset : BOOL; xyborder : WINT) : LRESULT;
BEGIN
    RETURN SendMessageA (hmc, MCM_SETCALENDARBORDER, VAL (WPARAM, fset), xyborder);
END MonthCal_SetCalendarBorderA;

PROCEDURE MonthCal_SetCalendarBorderW (hmc : HWND; fset : BOOL; xyborder : WINT) : LRESULT;
BEGIN
    RETURN SendMessageW (hmc, MCM_SETCALENDARBORDER, VAL (WPARAM, fset), xyborder);
END MonthCal_SetCalendarBorderW;

PROCEDURE MonthCal_GetCalendarBorder (hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessage (hmc, MCM_GETCALENDARBORDER, 0, 0);
END MonthCal_GetCalendarBorder;

PROCEDURE MonthCal_GetCalendarBorderA (hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessageA (hmc, MCM_GETCALENDARBORDER, 0, 0);
END MonthCal_GetCalendarBorderA;

PROCEDURE MonthCal_GetCalendarBorderW (hmc : HWND) : DWORD;
BEGIN
    RETURN SendMessageW (hmc, MCM_GETCALENDARBORDER, 0, 0);
END MonthCal_GetCalendarBorderW;

PROCEDURE MonthCal_SetCurrentView (hmc : HWND; dwNewView : DWORD) : BOOL;
BEGIN
    RETURN VAL (BOOL, SendMessage (hmc, MCM_SETCURRENTVIEW, 0, dwNewView));
END MonthCal_SetCurrentView;

PROCEDURE MonthCal_SetCurrentViewA (hmc : HWND; dwNewView : DWORD) : BOOL;
BEGIN
    RETURN VAL (BOOL, SendMessageA (hmc, MCM_SETCURRENTVIEW, 0, dwNewView));
END MonthCal_SetCurrentViewA;

PROCEDURE MonthCal_SetCurrentViewW (hmc : HWND; dwNewView : DWORD) : BOOL;
BEGIN
    RETURN VAL (BOOL, SendMessageW (hmc, MCM_SETCURRENTVIEW, 0, dwNewView));
END MonthCal_SetCurrentViewW;

(* datetimepick *)

PROCEDURE DateTime_GetSystemtime(hdp : HWND; VAR pst : SYSTEMTIME) : DWORD;
BEGIN
    RETURN SendMessage(hdp, DTM_GETSYSTEMTIME, 0, CAST(LPARAM, ADR(pst)));
END DateTime_GetSystemtime;

PROCEDURE DateTime_GetSystemtimeA(hdp : HWND; VAR pst : SYSTEMTIME) : DWORD;
BEGIN
    RETURN SendMessageA(hdp, DTM_GETSYSTEMTIME, 0, CAST(LPARAM, ADR(pst)));
END DateTime_GetSystemtimeA;

PROCEDURE DateTime_GetSystemtimeW(hdp : HWND; VAR pst : SYSTEMTIME) : DWORD;
BEGIN
    RETURN SendMessageW(hdp, DTM_GETSYSTEMTIME, 0, CAST(LPARAM, ADR(pst)));
END DateTime_GetSystemtimeW;

PROCEDURE DateTime_SetSystemtime(hdp : HWND;
                                 gd : DWORD;
                                 pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hdp,
                                  DTM_SETSYSTEMTIME,
                                  gd,
                                  CAST(LPARAM, ADR(pst))));
END DateTime_SetSystemtime;

PROCEDURE DateTime_SetSystemtimeA(hdp : HWND;
                                  gd : DWORD;
                                  pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hdp,
                                   DTM_SETSYSTEMTIME,
                                   gd,
                                   CAST(LPARAM, ADR(pst))));
END DateTime_SetSystemtimeA;

PROCEDURE DateTime_SetSystemtimeW(hdp : HWND;
                                  gd : DWORD;
                                  pst : SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hdp,
                                   DTM_SETSYSTEMTIME,
                                   gd,
                                   CAST(LPARAM, ADR(pst))));
END DateTime_SetSystemtimeW;

PROCEDURE DateTime_GetRange(hdp : HWND; VAR rgst : ARRAY OF SYSTEMTIME) : DWORD;
BEGIN
    RETURN SendMessage(hdp, DTM_GETRANGE, 0, CAST(LPARAM, ADR(rgst)));
END DateTime_GetRange;

PROCEDURE DateTime_GetRangeA(hdp : HWND; VAR rgst : ARRAY OF SYSTEMTIME) : DWORD;
BEGIN
    RETURN SendMessageA(hdp, DTM_GETRANGE, 0, CAST(LPARAM, ADR(rgst)));
END DateTime_GetRangeA;

PROCEDURE DateTime_GetRangeW(hdp : HWND; VAR rgst : ARRAY OF SYSTEMTIME) : DWORD;
BEGIN
    RETURN SendMessageW(hdp, DTM_GETRANGE, 0, CAST(LPARAM, ADR(rgst)));
END DateTime_GetRangeW;

PROCEDURE DateTime_SetRange(hdp : HWND;
                            gd : DWORD;
                            rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hdp,
                                  DTM_SETRANGE,
                                  gd,
                                  CAST(LPARAM, ADR(rgst))));
END DateTime_SetRange;

PROCEDURE DateTime_SetRangeA(hdp : HWND;
                             gd : DWORD;
                             rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hdp,
                                   DTM_SETRANGE,
                                   gd,
                                   CAST(LPARAM, ADR(rgst))));
END DateTime_SetRangeA;

PROCEDURE DateTime_SetRangeW(hdp : HWND;
                             gd : DWORD;
                             rgst : ARRAY OF SYSTEMTIME) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hdp,
                                   DTM_SETRANGE,
                                   gd,
                                   CAST(LPARAM, ADR(rgst))));
END DateTime_SetRangeW;

PROCEDURE DateTime_SetFormat(hdp : HWND; sz : ARRAY OF TCHAR);
BEGIN
    FUNC SendMessage(hdp, DTM_SETFORMAT, 0, CAST(LPARAM, ADR(sz)));
END DateTime_SetFormat;

PROCEDURE DateTime_SetFormatA(hdp : HWND; sz : ARRAY OF ACHAR);
BEGIN
    FUNC SendMessageA(hdp, DTM_SETFORMATA, 0, CAST(LPARAM, ADR(sz)));
END DateTime_SetFormatA;

PROCEDURE DateTime_SetFormatW(hdp : HWND; sz : ARRAY OF WCHAR);
BEGIN
    FUNC SendMessageW(hdp, DTM_SETFORMATW, 0, CAST(LPARAM, ADR(sz)));
END DateTime_SetFormatW;

PROCEDURE DateTime_SetMonthCalColor(hdp : HWND; iColor : WINT; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessage(hdp, DTM_SETMCCOLOR, iColor, clr));
END DateTime_SetMonthCalColor;

PROCEDURE DateTime_SetMonthCalColorA(hdp : HWND; iColor : WINT; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessageA(hdp, DTM_SETMCCOLOR, iColor, clr));
END DateTime_SetMonthCalColorA;

PROCEDURE DateTime_SetMonthCalColorW(hdp : HWND; iColor : WINT; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessageW(hdp, DTM_SETMCCOLOR, iColor, clr));
END DateTime_SetMonthCalColorW;

PROCEDURE DateTime_GetMonthCalColor(hdp : HWND; iColor : WINT) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessage(hdp, DTM_GETMCCOLOR, iColor, 0));
END DateTime_GetMonthCalColor;

PROCEDURE DateTime_GetMonthCalColorA(hdp : HWND; iColor : WINT) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessageA(hdp, DTM_GETMCCOLOR, iColor, 0));
END DateTime_GetMonthCalColorA;

PROCEDURE DateTime_GetMonthCalColorW(hdp : HWND; iColor : WINT) : COLORREF;
BEGIN
    RETURN CAST (COLORREF, SendMessageW(hdp, DTM_GETMCCOLOR, iColor, 0));
END DateTime_GetMonthCalColorW;

PROCEDURE DateTime_GetMonthCal(hdp : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hdp, DTM_GETMONTHCAL, 0, 0));
END DateTime_GetMonthCal;

PROCEDURE DateTime_GetMonthCalA(hdp : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hdp, DTM_GETMONTHCAL, 0, 0));
END DateTime_GetMonthCalA;

PROCEDURE DateTime_GetMonthCalW(hdp : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hdp, DTM_GETMONTHCAL, 0, 0));
END DateTime_GetMonthCalW;

PROCEDURE DateTime_SetMonthCalFont(hdp : HWND; hfont : HFONT; fRedraw : BOOL);
BEGIN
    FUNC
	SendMessage(hdp,
                DTM_SETMCFONT,
                CAST(WPARAM, hfont), VAL(LPARAM, fRedraw));
END DateTime_SetMonthCalFont;

PROCEDURE DateTime_SetMonthCalFontA(hdp : HWND; hfont : HFONT; fRedraw : BOOL);
BEGIN
    FUNC
	SendMessageA(hdp,
                 DTM_SETMCFONT,
                 CAST(WPARAM, hfont), VAL(LPARAM, fRedraw));
END DateTime_SetMonthCalFontA;

PROCEDURE DateTime_SetMonthCalFontW(hdp : HWND; hfont : HFONT; fRedraw : BOOL);
BEGIN
    FUNC
	SendMessageW(hdp,
                 DTM_SETMCFONT,
                 CAST(WPARAM, hfont), VAL(LPARAM, fRedraw));
END DateTime_SetMonthCalFontW;

PROCEDURE DateTime_GetMonthCalFont(hdp : HWND) : HFONT;
BEGIN
    RETURN CAST(HFONT, SendMessage(hdp, DTM_GETMCFONT, 0, 0));
END DateTime_GetMonthCalFont;

PROCEDURE DateTime_GetMonthCalFontA(hdp : HWND) : HFONT;
BEGIN
    RETURN CAST(HFONT, SendMessageA(hdp, DTM_GETMCFONT, 0, 0));
END DateTime_GetMonthCalFontA;

PROCEDURE DateTime_GetMonthCalFontW(hdp : HWND) : HFONT;
BEGIN
    RETURN CAST(HFONT, SendMessageW(hdp, DTM_GETMCFONT, 0, 0));
END DateTime_GetMonthCalFontW;

PROCEDURE DateTime_SetMonthCalStyle (hdp : HWND; dwStyle : DWORD) : LRESULT;
BEGIN
	RETURN SendMessage (hdp, DTM_SETMCSTYLE, 0, dwStyle);
END DateTime_SetMonthCalStyle;

PROCEDURE DateTime_SetMonthCalStyleA (hdp : HWND; dwStyle : DWORD) : LRESULT;
BEGIN
	RETURN SendMessageA (hdp, DTM_SETMCSTYLE, 0, dwStyle);
END DateTime_SetMonthCalStyleA;

PROCEDURE DateTime_SetMonthCalStyleW (hdp : HWND; dwStyle : DWORD) : LRESULT;
BEGIN
	RETURN SendMessageW (hdp, DTM_SETMCSTYLE, 0, dwStyle);
END DateTime_SetMonthCalStyleW;

PROCEDURE DateTime_GetMonthCalStyle (hdp : HWND) : LRESULT;
BEGIN
	RETURN SendMessage (hdp, DTM_GETMCSTYLE, 0, 0);
END DateTime_GetMonthCalStyle;

PROCEDURE DateTime_GetMonthCalStyleA (hdp : HWND) : LRESULT;
BEGIN
	RETURN SendMessageA (hdp, DTM_GETMCSTYLE, 0, 0);
END DateTime_GetMonthCalStyleA;

PROCEDURE DateTime_GetMonthCalStyleW (hdp : HWND) : LRESULT;
BEGIN
	RETURN SendMessageW (hdp, DTM_GETMCSTYLE, 0, 0);
END DateTime_GetMonthCalStyleW;

PROCEDURE DateTime_CloseMonthCal (hdp : HWND) : LRESULT;
BEGIN
	RETURN SendMessage (hdp, DTM_CLOSEMONTHCAL, 0, 0);
END DateTime_CloseMonthCal;

PROCEDURE DateTime_CloseMonthCalA (hdp : HWND) : LRESULT;
BEGIN
	RETURN SendMessageA (hdp, DTM_CLOSEMONTHCAL, 0, 0);
END DateTime_CloseMonthCalA;

PROCEDURE DateTime_CloseMonthCalW (hdp : HWND) : LRESULT;
BEGIN
	RETURN SendMessageW (hdp, DTM_CLOSEMONTHCAL, 0, 0);
END DateTime_CloseMonthCalW;

PROCEDURE DateTime_GetDateTimePickerInfo (hdp : HWND; VAR pdtpi : DATETIMEPICKERINFO) : LRESULT;
BEGIN
	RETURN SendMessage (hdp, DTM_GETDATETIMEPICKERINFO, 0, CAST (LPARAM, ADR(pdtpi)));
END DateTime_GetDateTimePickerInfo;

PROCEDURE DateTime_GetDateTimePickerInfoA (hdp : HWND; VAR pdtpi : DATETIMEPICKERINFO) : LRESULT;
BEGIN
	RETURN SendMessageA (hdp, DTM_GETDATETIMEPICKERINFO, 0, CAST (LPARAM, ADR(pdtpi)));
END DateTime_GetDateTimePickerInfoA;

PROCEDURE DateTime_GetDateTimePickerInfoW (hdp : HWND; VAR pdtpi : DATETIMEPICKERINFO) : LRESULT;
BEGIN
	RETURN SendMessageW (hdp, DTM_GETDATETIMEPICKERINFO, 0, CAST (LPARAM, ADR(pdtpi)));
END DateTime_GetDateTimePickerInfoW;

PROCEDURE DateTime_GetIdealSize (hdp : HWND; VAR psize : WSIZE) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessage (hdp, DTM_GETIDEALSIZE, 0, CAST (LPARAM, ADR(psize))));
END DateTime_GetIdealSize;

PROCEDURE DateTime_GetIdealSizeA (hdp : HWND; VAR psize : WSIZE) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessageA (hdp, DTM_GETIDEALSIZE, 0, CAST (LPARAM, ADR(psize))));
END DateTime_GetIdealSizeA;

PROCEDURE DateTime_GetIdealSizeW (hdp : HWND; VAR psize : WSIZE) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessageW (hdp, DTM_GETIDEALSIZE, 0, CAST (LPARAM, ADR(psize))));
END DateTime_GetIdealSizeW;

(* datetimepick *)

PROCEDURE MAKEIPRANGE(low, high : DWORD) : LPARAM;
BEGIN
    RETURN (high SHL 8) + low;
END MAKEIPRANGE;

PROCEDURE MAKEIPADDRESS(b1,b2,b3,b4 : BYTE) : LPARAM;
BEGIN
    RETURN (ORD(b1) SHL 24) + (ORD(b2) SHL 16) + (ORD(b3) SHL 8) + ORD(b4);
END MAKEIPADDRESS;

PROCEDURE FIRST_IPADDRESS(x : DWORD) : BYTE;
BEGIN
    RETURN (x SHR 24) BAND 0ffh;
END FIRST_IPADDRESS;

PROCEDURE SECOND_IPADDRESS(x : DWORD) : BYTE;
BEGIN
    RETURN (x SHR 16) BAND 0ffh;
END SECOND_IPADDRESS;

PROCEDURE THIRD_IPADDRESS(x : DWORD) : BYTE;
BEGIN
    RETURN (x SHR 8) BAND 0ffh
END THIRD_IPADDRESS;

PROCEDURE FOURTH_IPADDRESS(x : DWORD) : BYTE;
BEGIN
    RETURN x BAND 0ffh
END FOURTH_IPADDRESS;

(* Pager control *)

PROCEDURE Pager_SetChild(hwnd, hwndChild : HWND);
BEGIN
    FUNC SendMessage(hwnd, PGM_SETCHILD, 0, CAST(LPARAM, hwndChild));
END Pager_SetChild;

PROCEDURE Pager_SetChildA(hwnd, hwndChild : HWND);
BEGIN
    FUNC SendMessageA(hwnd, PGM_SETCHILD, 0, CAST(LPARAM, hwndChild));
END Pager_SetChildA;

PROCEDURE Pager_SetChildW(hwnd, hwndChild : HWND);
BEGIN
    FUNC SendMessage(hwnd, PGM_SETCHILD, 0, CAST(LPARAM, hwndChild));
END Pager_SetChildW;

PROCEDURE Pager_RecalcSize(hwnd : HWND);
BEGIN
    FUNC SendMessage(hwnd, PGM_RECALCSIZE, 0, 0);
END Pager_RecalcSize;

PROCEDURE Pager_RecalcSizeA(hwnd : HWND);
BEGIN
    FUNC SendMessageA(hwnd, PGM_RECALCSIZE, 0, 0);
END Pager_RecalcSizeA;

PROCEDURE Pager_RecalcSizeW(hwnd : HWND);
BEGIN
    FUNC SendMessageW(hwnd, PGM_RECALCSIZE, 0, 0);
END Pager_RecalcSizeW;

PROCEDURE Pager_ForwardMouse(hwnd : HWND; bForward : BOOL);
BEGIN
    FUNC SendMessage(hwnd, PGM_FORWARDMOUSE, ORD(bForward), 0)
END Pager_ForwardMouse;

PROCEDURE Pager_ForwardMouseA(hwnd : HWND; bForward : BOOL);
BEGIN
    FUNC SendMessageA(hwnd, PGM_FORWARDMOUSE, ORD(bForward), 0)
END Pager_ForwardMouseA;

PROCEDURE Pager_ForwardMouseW(hwnd : HWND; bForward : BOOL);
BEGIN
    FUNC SendMessageW(hwnd, PGM_FORWARDMOUSE, ORD(bForward), 0)
END Pager_ForwardMouseW;

PROCEDURE Pager_SetBkColor(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd,
                                      PGM_SETBKCOLOR,
                                      0, VAL(LPARAM, clr)));
END Pager_SetBkColor;

PROCEDURE Pager_SetBkColorA(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd,
                                       PGM_SETBKCOLOR,
                                       0, VAL(LPARAM, clr)));
END Pager_SetBkColorA;

PROCEDURE Pager_SetBkColorW(hwnd : HWND; clr : COLORREF) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd,
                                       PGM_SETBKCOLOR,
                                       0, VAL(LPARAM, clr)));
END Pager_SetBkColorW;

PROCEDURE Pager_GetBkColor(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessage(hwnd, PGM_GETBKCOLOR, 0, 0));
END Pager_GetBkColor;

PROCEDURE Pager_GetBkColorA(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageA(hwnd, PGM_GETBKCOLOR, 0, 0));
END Pager_GetBkColorA;

PROCEDURE Pager_GetBkColorW(hwnd : HWND) : COLORREF;
BEGIN
    RETURN CAST(COLORREF, SendMessageW(hwnd, PGM_GETBKCOLOR, 0, 0));
END Pager_GetBkColorW;

PROCEDURE Pager_SetBorder(hwnd : HWND; iBorder : WINT) : WINT;
BEGIN
    RETURN SendMessage(hwnd, PGM_SETBORDER, 0, iBorder);
END Pager_SetBorder;

PROCEDURE Pager_SetBorderA(hwnd : HWND; iBorder : WINT) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, PGM_SETBORDER, 0, iBorder);
END Pager_SetBorderA;

PROCEDURE Pager_SetBorderW(hwnd : HWND; iBorder : WINT) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, PGM_SETBORDER, 0, iBorder);
END Pager_SetBorderW;

PROCEDURE Pager_GetBorder(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwnd, PGM_GETBORDER, 0, 0);
END Pager_GetBorder;

PROCEDURE Pager_GetBorderA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, PGM_GETBORDER, 0, 0);
END Pager_GetBorderA;

PROCEDURE Pager_GetBorderW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, PGM_GETBORDER, 0, 0);
END Pager_GetBorderW;

PROCEDURE Pager_SetPos(hwnd : HWND; iPos : WINT) : WINT;
BEGIN
    RETURN SendMessage(hwnd, PGM_SETPOS, 0, iPos);
END Pager_SetPos;

PROCEDURE Pager_SetPosA(hwnd : HWND; iPos : WINT) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, PGM_SETPOS, 0, iPos);
END Pager_SetPosA;

PROCEDURE Pager_SetPosW(hwnd : HWND; iPos : WINT) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, PGM_SETPOS, 0, iPos);
END Pager_SetPosW;

PROCEDURE Pager_GetPos(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwnd, PGM_GETPOS, 0, 0);
END Pager_GetPos;

PROCEDURE Pager_GetPosA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, PGM_GETPOS, 0, 0);
END Pager_GetPosA;

PROCEDURE Pager_GetPosW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, PGM_GETPOS, 0, 0);
END Pager_GetPosW;

PROCEDURE Pager_SetButtonSize(hwnd : HWND; iSize : WINT) : WINT;
BEGIN
    RETURN SendMessage(hwnd, PGM_SETBUTTONSIZE, 0, iSize);
END Pager_SetButtonSize;

PROCEDURE Pager_SetButtonSizeA(hwnd : HWND; iSize : WINT) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, PGM_SETBUTTONSIZE, 0, iSize);
END Pager_SetButtonSizeA;

PROCEDURE Pager_SetButtonSizeW(hwnd : HWND; iSize : WINT) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, PGM_SETBUTTONSIZE, 0, iSize);
END Pager_SetButtonSizeW;

PROCEDURE Pager_GetButtonSize(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessage(hwnd, PGM_GETBUTTONSIZE, 0,0)
END Pager_GetButtonSize;

PROCEDURE Pager_GetButtonSizeA(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageA(hwnd, PGM_GETBUTTONSIZE, 0,0)
END Pager_GetButtonSizeA;

PROCEDURE Pager_GetButtonSizeW(hwnd : HWND) : WINT;
BEGIN
    RETURN SendMessageW(hwnd, PGM_GETBUTTONSIZE, 0,0)
END Pager_GetButtonSizeW;

PROCEDURE Pager_GetButtonState(hwnd : HWND; iButton : WINT) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessage(hwnd,
                                   PGM_GETBUTTONSTATE,
                                   0, iButton));
END Pager_GetButtonState;

PROCEDURE Pager_GetButtonStateA(hwnd : HWND; iButton : WINT) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageA(hwnd,
                                   PGM_GETBUTTONSTATE,
                                   0, iButton));
END Pager_GetButtonStateA;

PROCEDURE Pager_GetButtonStateW(hwnd : HWND; iButton : WINT) : DWORD;
BEGIN
    RETURN CAST(DWORD, SendMessageW(hwnd,
                                   PGM_GETBUTTONSTATE,
                                   0, iButton));
END Pager_GetButtonStateW;

PROCEDURE Pager_GetDropTarget (hwndPager : HWND; VAR ppDropTarget : IUnknown);
BEGIN
    FUNC SendMessage (hwndPager, PGM_GETDROPTARGET, 0, CAST(LPARAM, ADR(ppDropTarget)));
END Pager_GetDropTarget;

PROCEDURE Pager_GetDropTargetA (hwndPager : HWND; VAR ppDropTarget : IUnknown);
BEGIN
    FUNC SendMessageA (hwndPager, PGM_GETDROPTARGET, 0, CAST(LPARAM, ADR(ppDropTarget)));
END Pager_GetDropTargetA;

PROCEDURE Pager_GetDropTargetW (hwndPager : HWND; VAR ppDropTarget : IUnknown);
BEGIN
    FUNC SendMessageW (hwndPager, PGM_GETDROPTARGET, 0, CAST(LPARAM, ADR(ppDropTarget)));
END Pager_GetDropTargetW;

PROCEDURE Pager_SetScrollInfo (hwndPager : HWND; cTimeOut : UINT; cLinesPer : UINT; cPixelsPerLine : UINT) : WINT;
BEGIN
	RETURN SendMessage (hwndPager, PGM_SETSCROLLINFO, cTimeOut, MAKELONG(cLinesPer,cPixelsPerLine));
END Pager_SetScrollInfo;

PROCEDURE Pager_SetScrollInfoA (hwndPager : HWND; cTimeOut : UINT; cLinesPer : UINT; cPixelsPerLine : UINT) : WINT;
BEGIN
	RETURN SendMessageA (hwndPager, PGM_SETSCROLLINFO, cTimeOut, MAKELONG(cLinesPer,cPixelsPerLine));
END Pager_SetScrollInfoA;

PROCEDURE Pager_SetScrollInfoW (hwndPager : HWND; cTimeOut : UINT; cLinesPer : UINT; cPixelsPerLine : UINT) : WINT;
BEGIN
	RETURN SendMessageW (hwndPager, PGM_SETSCROLLINFO, cTimeOut, MAKELONG(cLinesPer,cPixelsPerLine));
END Pager_SetScrollInfoW;

(* Button control *)

PROCEDURE Button_GetIdealSize (hwnd : HWND; VAR pSize : WSIZE) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, BCM_GETIDEALSIZE, 0, CAST (LPARAM, ADR(pSize))));
END Button_GetIdealSize;

PROCEDURE Button_GetIdealSizeA (hwnd : HWND; VAR pSize : WSIZE) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, BCM_GETIDEALSIZE, 0, CAST (LPARAM, ADR(pSize))));
END Button_GetIdealSizeA;

PROCEDURE Button_GetIdealSizeW (hwnd : HWND; VAR pSize : WSIZE) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, BCM_GETIDEALSIZE, 0, CAST (LPARAM, ADR(pSize))));
END Button_GetIdealSizeW;

PROCEDURE Button_SetImageList (hwnd : HWND; pbuttonImageList : BUTTON_IMAGELIST) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, BCM_SETIMAGELIST, 0, CAST (LPARAM, ADR(pbuttonImageList))));
END Button_SetImageList;

PROCEDURE Button_SetImageListA (hwnd : HWND; pbuttonImageList : BUTTON_IMAGELIST) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, BCM_SETIMAGELIST, 0, CAST (LPARAM, ADR(pbuttonImageList))));
END Button_SetImageListA;

PROCEDURE Button_SetImageListW (hwnd : HWND; pbuttonImageList : BUTTON_IMAGELIST) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, BCM_SETIMAGELIST, 0, CAST (LPARAM, ADR(pbuttonImageList))));
END Button_SetImageListW;

PROCEDURE Button_GetImageList (hwnd : HWND; VAR pbuttonImageList : BUTTON_IMAGELIST) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, BCM_GETIMAGELIST, 0, CAST (LPARAM, ADR(pbuttonImageList))));
END Button_GetImageList;

PROCEDURE Button_GetImageListA (hwnd : HWND; VAR pbuttonImageList : BUTTON_IMAGELIST) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, BCM_GETIMAGELIST, 0, CAST (LPARAM, ADR(pbuttonImageList))));
END Button_GetImageListA;

PROCEDURE Button_GetImageListW (hwnd : HWND; VAR pbuttonImageList : BUTTON_IMAGELIST) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, BCM_GETIMAGELIST, 0, CAST (LPARAM, ADR(pbuttonImageList))));
END Button_GetImageListW;

PROCEDURE Button_SetTextMargin (hwnd : HWND; pmargin : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, BCM_SETTEXTMARGIN, 0, CAST (LPARAM, ADR(pmargin))));
END Button_SetTextMargin;

PROCEDURE Button_SetTextMarginA (hwnd : HWND; pmargin : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, BCM_SETTEXTMARGIN, 0, CAST (LPARAM, ADR(pmargin))));
END Button_SetTextMarginA;

PROCEDURE Button_SetTextMarginW (hwnd : HWND; pmargin : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, BCM_SETTEXTMARGIN, 0, CAST (LPARAM, ADR(pmargin))));
END Button_SetTextMarginW;

PROCEDURE Button_GetTextMargin (hwnd : HWND; VAR pmargin : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, BCM_GETTEXTMARGIN, 0, CAST (LPARAM, ADR(pmargin))));
END Button_GetTextMargin;

PROCEDURE Button_GetTextMarginA (hwnd : HWND; VAR pmargin : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, BCM_GETTEXTMARGIN, 0, CAST (LPARAM, ADR(pmargin))));
END Button_GetTextMarginA;

PROCEDURE Button_GetTextMarginW (hwnd : HWND; VAR pmargin : RECT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, BCM_GETTEXTMARGIN, 0, CAST (LPARAM, ADR(pmargin))));
END Button_GetTextMarginW;

PROCEDURE Button_SetDropDownState (hwnd : HWND; fDropDown : BOOL) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, BCM_SETDROPDOWNSTATE, VAL (WPARAM,fDropDown), 0));
END Button_SetDropDownState;

PROCEDURE Button_SetDropDownStateA (hwnd : HWND; fDropDown : BOOL) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, BCM_SETDROPDOWNSTATE, VAL (WPARAM,fDropDown), 0));
END Button_SetDropDownStateA;

PROCEDURE Button_SetDropDownStateW (hwnd : HWND; fDropDown : BOOL) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, BCM_SETDROPDOWNSTATE, VAL (WPARAM,fDropDown), 0));
END Button_SetDropDownStateW;

PROCEDURE Button_SetSplitInfo (hwnd : HWND; pInfo : BUTTON_SPLITINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, BCM_SETSPLITINFO, 0, CAST (LPARAM, ADR(pInfo))));
END Button_SetSplitInfo;

PROCEDURE Button_SetSplitInfoA (hwnd : HWND; pInfo : BUTTON_SPLITINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, BCM_SETSPLITINFO, 0, CAST (LPARAM, ADR(pInfo))));
END Button_SetSplitInfoA;

PROCEDURE Button_SetSplitInfoW (hwnd : HWND; pInfo : BUTTON_SPLITINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, BCM_SETSPLITINFO, 0, CAST (LPARAM, ADR(pInfo))));
END Button_SetSplitInfoW;

PROCEDURE Button_GetSplitInfo (hwnd : HWND; VAR pInfo : BUTTON_SPLITINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, BCM_GETSPLITINFO, 0, CAST (LPARAM, ADR(pInfo))));
END Button_GetSplitInfo;

PROCEDURE Button_GetSplitInfoA (hwnd : HWND; VAR pInfo : BUTTON_SPLITINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, BCM_GETSPLITINFO, 0, CAST (LPARAM, ADR(pInfo))));
END Button_GetSplitInfoA;

PROCEDURE Button_GetSplitInfoW (hwnd : HWND; VAR pInfo : BUTTON_SPLITINFO) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, BCM_GETSPLITINFO, 0, CAST (LPARAM, ADR(pInfo))));
END Button_GetSplitInfoW;

PROCEDURE Button_SetNote (hwnd : HWND; psz : ARRAY OF WCHAR) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, BCM_SETNOTE, 0, CAST (LPARAM, ADR(psz))));
END Button_SetNote;

PROCEDURE Button_SetNoteA (hwnd : HWND; psz : ARRAY OF WCHAR) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, BCM_SETNOTE, 0, CAST (LPARAM, ADR(psz))));
END Button_SetNoteA;

PROCEDURE Button_SetNoteW (hwnd : HWND; psz : ARRAY OF WCHAR) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, BCM_SETNOTE, 0, CAST (LPARAM, ADR(psz))));
END Button_SetNoteW;

PROCEDURE Button_GetNote (hwnd : HWND; psz : ARRAY OF WCHAR; pcc : WINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, BCM_GETNOTE, pcc, CAST (LPARAM, ADR(psz))));
END Button_GetNote;

PROCEDURE Button_GetNoteA (hwnd : HWND; psz : ARRAY OF WCHAR; pcc : WINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, BCM_GETNOTE, pcc, CAST (LPARAM, ADR(psz))));
END Button_GetNoteA;

PROCEDURE Button_GetNoteW (hwnd : HWND; psz : ARRAY OF WCHAR; pcc : WINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, BCM_GETNOTE, pcc, CAST (LPARAM, ADR(psz))));
END Button_GetNoteW;

PROCEDURE Button_GetNoteLength (hwnd : HWND) : LRESULT;
BEGIN
	RETURN SendMessage (hwnd, BCM_GETNOTELENGTH, 0, 0);
END Button_GetNoteLength;

PROCEDURE Button_GetNoteLengthA (hwnd : HWND) : LRESULT;
BEGIN
	RETURN SendMessageA (hwnd, BCM_GETNOTELENGTH, 0, 0);
END Button_GetNoteLengthA;

PROCEDURE Button_GetNoteLengthW (hwnd : HWND) : LRESULT;
BEGIN
	RETURN SendMessageW (hwnd, BCM_GETNOTELENGTH, 0, 0);
END Button_GetNoteLengthW;

PROCEDURE Button_SetElevationRequiredState (hwnd : HWND; fRequired : BOOL) : LRESULT;
BEGIN
	RETURN SendMessage (hwnd, BCM_SETSHIELD, 0, VAL (LPARAM, fRequired));
END Button_SetElevationRequiredState;

PROCEDURE Button_SetElevationRequiredStateA (hwnd : HWND; fRequired : BOOL) : LRESULT;
BEGIN
	RETURN SendMessageA (hwnd, BCM_SETSHIELD, 0, VAL (LPARAM, fRequired));
END Button_SetElevationRequiredStateA;

PROCEDURE Button_SetElevationRequiredStateW (hwnd : HWND; fRequired : BOOL) : LRESULT;
BEGIN
	RETURN SendMessageW (hwnd, BCM_SETSHIELD, 0, VAL (LPARAM, fRequired));
END Button_SetElevationRequiredStateW;

(* Edit Control *)

PROCEDURE Edit_SetCueBannerText (hwnd : HWND; lpcwText : ARRAY OF WCHAR) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, EM_SETCUEBANNER, 0, CAST (LPARAM, ADR(lpcwText))));
END Edit_SetCueBannerText;

PROCEDURE Edit_SetCueBannerTextA (hwnd : HWND; lpcwText : ARRAY OF WCHAR) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, EM_SETCUEBANNER, 0, CAST (LPARAM, ADR(lpcwText))));
END Edit_SetCueBannerTextA;

PROCEDURE Edit_SetCueBannerTextW (hwnd : HWND; lpcwText : ARRAY OF WCHAR) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, EM_SETCUEBANNER, 0, CAST (LPARAM, ADR(lpcwText))));
END Edit_SetCueBannerTextW;

PROCEDURE Edit_SetCueBannerTextFocused (hwnd : HWND; lpcwText : ARRAY OF WCHAR; fDrawFocused : BOOL) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, EM_SETCUEBANNER, VAL (WPARAM, fDrawFocused), CAST (LPARAM, ADR(lpcwText))));
END Edit_SetCueBannerTextFocused;

PROCEDURE Edit_SetCueBannerTextFocusedA (hwnd : HWND; lpcwText : ARRAY OF WCHAR; fDrawFocused : BOOL) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, EM_SETCUEBANNER, VAL (WPARAM, fDrawFocused), CAST (LPARAM, ADR(lpcwText))));
END Edit_SetCueBannerTextFocusedA;

PROCEDURE Edit_SetCueBannerTextFocusedW (hwnd : HWND; lpcwText : ARRAY OF WCHAR; fDrawFocused : BOOL) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, EM_SETCUEBANNER, VAL (WPARAM, fDrawFocused), CAST (LPARAM, ADR(lpcwText))));
END Edit_SetCueBannerTextFocusedW;

PROCEDURE Edit_GetCueBannerText (hwnd : HWND; VAR lpcwText : ARRAY OF WCHAR; cchText : LONG) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, EM_GETCUEBANNER, CAST (WPARAM, ADR(lpcwText)), cchText));
END Edit_GetCueBannerText;

PROCEDURE Edit_GetCueBannerTextA (hwnd : HWND; VAR lpcwText : ARRAY OF WCHAR; cchText : LONG) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, EM_GETCUEBANNER, CAST (WPARAM, ADR(lpcwText)), cchText));
END Edit_GetCueBannerTextA;

PROCEDURE Edit_GetCueBannerTextW (hwnd : HWND; VAR lpcwText : ARRAY OF WCHAR; cchText : LONG) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, EM_GETCUEBANNER, CAST (WPARAM, ADR(lpcwText)), cchText));
END Edit_GetCueBannerTextW;

PROCEDURE Edit_ShowBalloonTip (hwnd : HWND; peditballoontip : EDITBALLOONTIP) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, EM_SHOWBALLOONTIP, 0, CAST (LPARAM, ADR(peditballoontip))));
END Edit_ShowBalloonTip;

PROCEDURE Edit_ShowBalloonTipA (hwnd : HWND; peditballoontip : EDITBALLOONTIP) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, EM_SHOWBALLOONTIP, 0, CAST (LPARAM, ADR(peditballoontip))));
END Edit_ShowBalloonTipA;

PROCEDURE Edit_ShowBalloonTipW (hwnd : HWND; peditballoontip : EDITBALLOONTIP) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, EM_SHOWBALLOONTIP, 0, CAST (LPARAM, ADR(peditballoontip))));
END Edit_ShowBalloonTipW;

PROCEDURE Edit_HideBalloonTip (hwnd : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, EM_HIDEBALLOONTIP, 0, 0));
END Edit_HideBalloonTip;

PROCEDURE Edit_HideBalloonTipA (hwnd : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, EM_HIDEBALLOONTIP, 0, 0));
END Edit_HideBalloonTipA;

PROCEDURE Edit_HideBalloonTipW (hwnd : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, EM_HIDEBALLOONTIP, 0, 0));
END Edit_HideBalloonTipW;

(* ComboBox *)

PROCEDURE ComboBox_SetMinVisible (hwnd : HWND; iMinVisible : WINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hwnd, CB_SETMINVISIBLE, iMinVisible, 0));
END ComboBox_SetMinVisible;

PROCEDURE ComboBox_SetMinVisibleA (hwnd : HWND; iMinVisible : WINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hwnd, CB_SETMINVISIBLE, iMinVisible, 0));
END ComboBox_SetMinVisibleA;

PROCEDURE ComboBox_SetMinVisibleW (hwnd : HWND; iMinVisible : WINT) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hwnd, CB_SETMINVISIBLE, iMinVisible, 0));
END ComboBox_SetMinVisibleW;

PROCEDURE ComboBox_GetMinVisible (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessage (hwnd, CB_GETMINVISIBLE, 0, 0);
END ComboBox_GetMinVisible;

PROCEDURE ComboBox_GetMinVisibleA (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageA (hwnd, CB_GETMINVISIBLE, 0, 0);
END ComboBox_GetMinVisibleA;

PROCEDURE ComboBox_GetMinVisibleW (hwnd : HWND) : WINT;
BEGIN
	RETURN SendMessageW (hwnd, CB_GETMINVISIBLE, 0, 0);
END ComboBox_GetMinVisibleW;

PROCEDURE ComboBox_SetCueBannerText (hwnd : HWND; lpcwText : ARRAY OF WCHAR) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessage (hwnd, CB_SETCUEBANNER, 0, CAST (LPARAM, ADR(lpcwText))));
END ComboBox_SetCueBannerText;

PROCEDURE ComboBox_SetCueBannerTextA (hwnd : HWND; lpcwText : ARRAY OF WCHAR) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessageA (hwnd, CB_SETCUEBANNER, 0, CAST (LPARAM, ADR(lpcwText))));
END ComboBox_SetCueBannerTextA;

PROCEDURE ComboBox_SetCueBannerTextW (hwnd : HWND; lpcwText : ARRAY OF WCHAR) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessageW (hwnd, CB_SETCUEBANNER, 0, CAST (LPARAM, ADR(lpcwText))));
END ComboBox_SetCueBannerTextW;

PROCEDURE ComboBox_GetCueBannerText (hwnd : HWND; VAR lpwText : ARRAY OF WCHAR; cchText : WINT) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessage (hwnd, CB_GETCUEBANNER, CAST (WPARAM, ADR(lpwText)), cchText));
END ComboBox_GetCueBannerText;

PROCEDURE ComboBox_GetCueBannerTextA (hwnd : HWND; VAR lpwText : ARRAY OF WCHAR; cchText : WINT) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessageA (hwnd, CB_GETCUEBANNER, CAST (WPARAM, ADR(lpwText)), cchText));
END ComboBox_GetCueBannerTextA;

PROCEDURE ComboBox_GetCueBannerTextW (hwnd : HWND; VAR lpwText : ARRAY OF WCHAR; cchText : WINT) : BOOL;
BEGIN
	RETURN CAST (BOOL, SendMessageW (hwnd, CB_GETCUEBANNER, CAST (WPARAM, ADR(lpwText)), cchText));
END ComboBox_GetCueBannerTextW;

(* Dynamic arrays *)

PROCEDURE DSA_GetItemCount (hdsa : HDSA) : WINT;
VAR
	x : PINT;
BEGIN
	x := CAST (PINT, hdsa);
	RETURN x^;
END DSA_GetItemCount;

PROCEDURE DSA_AppendItem (hdsa : HDSA; pItem : LPVOID) : WINT;
BEGIN
	RETURN DSA_InsertItem (hdsa, DA_LAST, pItem);
END DSA_AppendItem;

PROCEDURE DPA_GetPtrCount (hdpa : HDPA) : WINT;
VAR
	x : PINT;
BEGIN
	x := CAST (PINT, hdpa);
	RETURN x^;
END DPA_GetPtrCount;

PROCEDURE DPA_SetPtrCount (hdpa : HDPA; cItems : WINT) : WINT;
VAR
	x : PINT;
BEGIN
	x := CAST (PINT, hdpa);
	x^ := cItems;
	RETURN cItems;
END DPA_SetPtrCount;

PROCEDURE DPA_FastDeleteLastPtr (hdpa : HDPA);
VAR
	x : PINT;
BEGIN
	x := CAST (PINT, hdpa);
	DEC (x^);
END DPA_FastDeleteLastPtr;

PROCEDURE DPA_GetPtrPtr (hdpa : HDPA) : LPVOID;
VAR
	x : POINTER TO LPVOID;
BEGIN
	x := ADDADR (hdpa, SIZE(LPVOID));
	RETURN x^;
END DPA_GetPtrPtr;

PROCEDURE DPA_FastGetPtr (hdpa : HDPA; i : WINT) : LPVOID;
VAR
	x : POINTER TO LPVOID;
BEGIN
	x := ADDADR (hdpa, SIZE(LPVOID));
	RETURN ADDADR (x^, i*SIZE(LPVOID));
END DPA_FastGetPtr;

PROCEDURE DPA_AppendPtr (pdpa : HDPA; pitem : LPVOID) : WINT;
BEGIN
	RETURN DPA_InsertPtr (pdpa, DA_LAST, pitem);
END DPA_AppendPtr;

PROCEDURE DPA_SortedInsertPtr (pdpa : HDPA; pFind : LPVOID; iStart : WINT; pfnCmp : PFNDPACOMPARE; lParam : LPARAM;
	options : UINT; pitem : LPVOID) : WINT;
BEGIN
	RETURN DPA_InsertPtr (pdpa, DPA_Search (pdpa, pFind, iStart, pfnCmp, lParam, DPAS_SORTED BOR options), pitem);
END DPA_SortedInsertPtr;

(* Property Sheet *)

PROCEDURE PropSheet_SetCurSel(hDlg : HWND;
                              hpage : HPROPSHEETPAGE;
                              index : WPARAM);
BEGIN
    SendMessage(hDlg, PSM_SETCURSEL, index, CAST(LPARAM, hpage));
END PropSheet_SetCurSel;

PROCEDURE PropSheet_SetCurSelA(hDlg : HWND;
                               hpage : HPROPSHEETPAGE;
                               index : WPARAM);
BEGIN
    SendMessageA(hDlg, PSM_SETCURSEL, index, CAST(LPARAM, hpage));
END PropSheet_SetCurSelA;

PROCEDURE PropSheet_SetCurSelW(hDlg : HWND;
                               hpage : HPROPSHEETPAGE;
                               index : WPARAM);
BEGIN
    SendMessageW(hDlg, PSM_SETCURSEL, index, CAST(LPARAM, hpage));
END PropSheet_SetCurSelW;

PROCEDURE PropSheet_RemovePage(hDlg : HWND;
                              hpage : HPROPSHEETPAGE;
                              index : WPARAM);
BEGIN
    SendMessage(hDlg, PSM_REMOVEPAGE, index, CAST(LPARAM, hpage));
END PropSheet_RemovePage;

PROCEDURE PropSheet_RemovePageA(hDlg : HWND;
                              hpage : HPROPSHEETPAGE;
                              index : WPARAM);
BEGIN
    SendMessageA(hDlg, PSM_REMOVEPAGE, index, CAST(LPARAM, hpage));
END PropSheet_RemovePageA;

PROCEDURE PropSheet_RemovePageW(hDlg : HWND;
                              hpage : HPROPSHEETPAGE;
                              index : WPARAM);
BEGIN
    SendMessageW(hDlg, PSM_REMOVEPAGE, index, CAST(LPARAM, hpage));
END PropSheet_RemovePageW;

PROCEDURE PropSheet_AddPage(hDlg : HWND;
                            hpage : HPROPSHEETPAGE);
BEGIN
    SendMessage(hDlg, PSM_ADDPAGE, 0, CAST(LPARAM, hpage));
END PropSheet_AddPage;

PROCEDURE PropSheet_AddPageA(hDlg : HWND;
                            hpage : HPROPSHEETPAGE);
BEGIN
    SendMessageA(hDlg, PSM_ADDPAGE, 0, CAST(LPARAM, hpage));
END PropSheet_AddPageA;

PROCEDURE PropSheet_AddPageW(hDlg : HWND;
                            hpage : HPROPSHEETPAGE);
BEGIN
    SendMessageW(hDlg, PSM_ADDPAGE, 0, CAST(LPARAM, hpage));
END PropSheet_AddPageW;

PROCEDURE PropSheet_Changed(hDlg : HWND;
                            hwnd : HWND);
BEGIN
    SendMessage(hDlg, PSM_CHANGED, CAST(WPARAM, hwnd), 0);
END PropSheet_Changed;

PROCEDURE PropSheet_ChangedA(hDlg : HWND;
                            hwnd : HWND);
BEGIN
    SendMessageA(hDlg, PSM_CHANGED, CAST(WPARAM, hwnd), 0);
END PropSheet_ChangedA;

PROCEDURE PropSheet_ChangedW(hDlg : HWND;
                            hwnd : HWND);
BEGIN
    SendMessageW(hDlg, PSM_CHANGED, CAST(WPARAM, hwnd), 0);
END PropSheet_ChangedW;

PROCEDURE PropSheet_RestartWindows(hDlg : HWND);
BEGIN
    SendMessage(hDlg, PSM_RESTARTWINDOWS, 0, 0);
END PropSheet_RestartWindows;

PROCEDURE PropSheet_RestartWindowsA(hDlg : HWND);
BEGIN
    SendMessageA(hDlg, PSM_RESTARTWINDOWS, 0, 0);
END PropSheet_RestartWindowsA;

PROCEDURE PropSheet_RestartWindowsW(hDlg : HWND);
BEGIN
    SendMessageW(hDlg, PSM_RESTARTWINDOWS, 0, 0);
END PropSheet_RestartWindowsW;

PROCEDURE PropSheet_RebootSystem(hDlg : HWND);
BEGIN
    SendMessage(hDlg, PSM_REBOOTSYSTEM, 0, 0);
END PropSheet_RebootSystem;

PROCEDURE PropSheet_RebootSystemA(hDlg : HWND);
BEGIN
    SendMessageA(hDlg, PSM_REBOOTSYSTEM, 0, 0);
END PropSheet_RebootSystemA;

PROCEDURE PropSheet_RebootSystemW(hDlg : HWND);
BEGIN
    SendMessageW(hDlg, PSM_REBOOTSYSTEM, 0, 0);
END PropSheet_RebootSystemW;

PROCEDURE PropSheet_CancelToClose(hDlg : HWND);
BEGIN
    SendMessage(hDlg, PSM_CANCELTOCLOSE, 0, 0);
END PropSheet_CancelToClose;

PROCEDURE PropSheet_CancelToCloseA(hDlg : HWND);
BEGIN
    SendMessageA(hDlg, PSM_CANCELTOCLOSE, 0, 0);
END PropSheet_CancelToCloseA;

PROCEDURE PropSheet_CancelToCloseW(hDlg : HWND);
BEGIN
    SendMessageW(hDlg, PSM_CANCELTOCLOSE, 0, 0);
END PropSheet_CancelToCloseW;

PROCEDURE PropSheet_QuerySiblings(hDlg : HWND;
                                  wParam : WPARAM;
                                  lParam : LPARAM);
BEGIN
    SendMessage(hDlg, PSM_QUERYSIBLINGS, wParam, lParam);
END PropSheet_QuerySiblings;

PROCEDURE PropSheet_QuerySiblingsA(hDlg : HWND;
                                  wParam : WPARAM;
                                  lParam : LPARAM);
BEGIN
    SendMessageA(hDlg, PSM_QUERYSIBLINGS, wParam, lParam);
END PropSheet_QuerySiblingsA;

PROCEDURE PropSheet_QuerySiblingsW(hDlg : HWND;
                                  wParam : WPARAM;
                                  lParam : LPARAM);
BEGIN
    SendMessageW(hDlg, PSM_QUERYSIBLINGS, wParam, lParam);
END PropSheet_QuerySiblingsW;

PROCEDURE PropSheet_UnChanged(hDlg : HWND; hwnd : HWND);
BEGIN
    SendMessage(hDlg, PSM_UNCHANGED, CAST(WPARAM, hwnd), 0)
END PropSheet_UnChanged;

PROCEDURE PropSheet_UnChangedA(hDlg : HWND; hwnd : HWND);
BEGIN
    SendMessageA(hDlg, PSM_UNCHANGED, CAST(WPARAM, hwnd), 0)
END PropSheet_UnChangedA;

PROCEDURE PropSheet_UnChangedW(hDlg : HWND; hwnd : HWND);
BEGIN
    SendMessageW(hDlg, PSM_UNCHANGED, CAST(WPARAM, hwnd), 0)
END PropSheet_UnChangedW;

PROCEDURE PropSheet_Apply(hDlg : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hDlg, PSM_APPLY, 0, 0));
END PropSheet_Apply;

PROCEDURE PropSheet_ApplyA(hDlg : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hDlg, PSM_APPLY, 0, 0));
END PropSheet_ApplyA;

PROCEDURE PropSheet_ApplyW(hDlg : HWND) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hDlg, PSM_APPLY, 0, 0));
END PropSheet_ApplyW;

PROCEDURE PropSheet_SetTitle(hDlg : HWND;
                             wStyle : WPARAM;
                             lpszText : ARRAY OF TCHAR) : WINT;
BEGIN
    RETURN SendMessage(hDlg,
                       PSM_SETTITLE,
                       wStyle,
                       CAST(LPARAM , ADR(lpszText)));
END PropSheet_SetTitle;

PROCEDURE PropSheet_SetTitleA(hDlg : HWND;
                             wStyle : WPARAM;
                             lpszText : ARRAY OF ACHAR) : WINT;
BEGIN
    RETURN SendMessageA(hDlg,
                       PSM_SETTITLEA,
                       wStyle,
                       CAST(LPARAM , ADR(lpszText)));
END PropSheet_SetTitleA;

PROCEDURE PropSheet_SetTitleW(hDlg : HWND;
                             wStyle : WPARAM;
                             lpszText : ARRAY OF WCHAR) : WINT;
BEGIN
    RETURN SendMessageW(hDlg,
                       PSM_SETTITLEW,
                       wStyle,
                       CAST(LPARAM , ADR(lpszText)));
END PropSheet_SetTitleW;

PROCEDURE PropSheet_SetWizButtons(hDlg : HWND; dwFlags : DWORD);
BEGIN
    PostMessage(hDlg, PSM_SETWIZBUTTONS, 0, dwFlags);
END PropSheet_SetWizButtons;

PROCEDURE PropSheet_SetWizButtonsA(hDlg : HWND; dwFlags : DWORD);
BEGIN
    PostMessageA(hDlg, PSM_SETWIZBUTTONS, 0, dwFlags);
END PropSheet_SetWizButtonsA;

PROCEDURE PropSheet_SetWizButtonsW(hDlg : HWND; dwFlags : DWORD);
BEGIN
    PostMessageW(hDlg, PSM_SETWIZBUTTONS, 0, dwFlags);
END PropSheet_SetWizButtonsW;

PROCEDURE PropSheet_PressButton(hDlg : HWND; iButton : WINT);
BEGIN
    SendMessage(hDlg, PSM_PRESSBUTTON, iButton, 0);
END PropSheet_PressButton;

PROCEDURE PropSheet_PressButtonA(hDlg : HWND; iButton : WINT);
BEGIN
    SendMessageA(hDlg, PSM_PRESSBUTTON, iButton, 0);
END PropSheet_PressButtonA;

PROCEDURE PropSheet_PressButtonW(hDlg : HWND; iButton : WINT);
BEGIN
    SendMessageW(hDlg, PSM_PRESSBUTTON, iButton, 0);
END PropSheet_PressButtonW;

PROCEDURE PropSheet_SetCurSelByID(hDlg : HWND; id : WINT);
BEGIN
    SendMessage(hDlg, PSM_SETCURSELID, 0, id);
END PropSheet_SetCurSelByID;

PROCEDURE PropSheet_SetCurSelByIDA(hDlg : HWND; id : WINT);
BEGIN
    SendMessageA(hDlg, PSM_SETCURSELID, 0, id);
END PropSheet_SetCurSelByIDA;

PROCEDURE PropSheet_SetCurSelByIDW(hDlg : HWND; id : WINT);
BEGIN
    SendMessageW(hDlg, PSM_SETCURSELID, 0, id);
END PropSheet_SetCurSelByIDW;

PROCEDURE PropSheet_SetFinishText(hDlg : HWND;
                                  lpszText : ARRAY OF TCHAR);
BEGIN
    SendMessage(hDlg, PSM_SETFINISHTEXT, 0,
                            CAST(LPARAM, ADR(lpszText)));
END PropSheet_SetFinishText;

PROCEDURE PropSheet_SetFinishTextA(hDlg : HWND;
                                  lpszText : ARRAY OF ACHAR);
BEGIN
    SendMessageA(hDlg, PSM_SETFINISHTEXTA, 0,
                            CAST(LPARAM, ADR(lpszText)));
END PropSheet_SetFinishTextA;

PROCEDURE PropSheet_SetFinishTextW(hDlg : HWND;
                                  lpszText : ARRAY OF WCHAR);
BEGIN
    SendMessageW(hDlg, PSM_SETFINISHTEXTW, 0,
                            CAST(LPARAM, ADR(lpszText)));
END PropSheet_SetFinishTextW;

PROCEDURE PropSheet_GetTabControl(hDlg : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hDlg, PSM_GETTABCONTROL, 0, 0));
END PropSheet_GetTabControl;

PROCEDURE PropSheet_GetTabControlA(hDlg : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hDlg, PSM_GETTABCONTROL, 0, 0));
END PropSheet_GetTabControlA;

PROCEDURE PropSheet_GetTabControlW(hDlg : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hDlg, PSM_GETTABCONTROL, 0, 0));
END PropSheet_GetTabControlW;

PROCEDURE PropSheet_IsDialogMessage(hDlg : HWND; pMsg : MSG) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessage(hDlg,
                                  PSM_ISDIALOGMESSAGE,
                                  0,
                                  CAST(LPARAM, ADR(pMsg)))
               );
END PropSheet_IsDialogMessage;

PROCEDURE PropSheet_IsDialogMessageA(hDlg : HWND; pMsg : MSG) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageA(hDlg,
                                  PSM_ISDIALOGMESSAGE,
                                  0,
                                  CAST(LPARAM, ADR(pMsg)))
               );
END PropSheet_IsDialogMessageA;

PROCEDURE PropSheet_IsDialogMessageW(hDlg : HWND; pMsg : MSG) : BOOL;
BEGIN
    RETURN CAST(BOOL, SendMessageW(hDlg,
                                  PSM_ISDIALOGMESSAGE,
                                  0,
                                  CAST(LPARAM, ADR(pMsg)))
               );
END PropSheet_IsDialogMessageW;

PROCEDURE PropSheet_GetCurrentPageHwnd(hDlg : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hDlg, PSM_GETCURRENTPAGEHWND, 0, 0));
END PropSheet_GetCurrentPageHwnd;

PROCEDURE PropSheet_GetCurrentPageHwndA(hDlg : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hDlg, PSM_GETCURRENTPAGEHWND, 0, 0));
END PropSheet_GetCurrentPageHwndA;

PROCEDURE PropSheet_GetCurrentPageHwndW(hDlg : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hDlg, PSM_GETCURRENTPAGEHWND, 0, 0));
END PropSheet_GetCurrentPageHwndW;

PROCEDURE PropSheet_InsertPage (hPropSheetDialog : HWND; wParam : HWND; hPage : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hPropSheetDialog, PSM_INSERTPAGE, CAST(WPARAM,wParam), CAST(LPARAM,hPage)));
END PropSheet_InsertPage;

PROCEDURE PropSheet_InsertPageA (hPropSheetDialog : HWND; wParam : HWND; hPage : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hPropSheetDialog, PSM_INSERTPAGE, CAST(WPARAM,wParam), CAST(LPARAM,hPage)));
END PropSheet_InsertPageA;

PROCEDURE PropSheet_InsertPageW (hPropSheetDialog : HWND; wParam : HWND; hPage : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hPropSheetDialog, PSM_INSERTPAGE, CAST(WPARAM,wParam), CAST(LPARAM,hPage)));
END PropSheet_InsertPageW;

PROCEDURE PropSheet_SetHeaderTitle (hWizardDlg : HWND; iPageIndex : WINT; pszHeaderTitle : ARRAY OF TCHAR) : WINT;
BEGIN
	RETURN SendMessage (hWizardDlg, PSM_SETHEADERTITLE, iPageIndex, CAST (LPARAM, ADR(pszHeaderTitle)));
END PropSheet_SetHeaderTitle;

PROCEDURE PropSheet_SetHeaderTitleA (hWizardDlg : HWND; iPageIndex : WINT; pszHeaderTitle : ARRAY OF ACHAR) : WINT;
BEGIN
	RETURN SendMessageA (hWizardDlg, PSM_SETHEADERTITLEA, iPageIndex, CAST (LPARAM, ADR(pszHeaderTitle)));
END PropSheet_SetHeaderTitleA;

PROCEDURE PropSheet_SetHeaderTitleW (hWizardDlg : HWND; iPageIndex : WINT; pszHeaderTitle : ARRAY OF WCHAR) : WINT;
BEGIN
	RETURN SendMessageW (hWizardDlg, PSM_SETHEADERTITLEW, iPageIndex, CAST (LPARAM, ADR(pszHeaderTitle)));
END PropSheet_SetHeaderTitleW;

PROCEDURE PropSheet_SetHeaderSubTitle (hWizardDlg : HWND; iPageIndex : WINT; pszHeaderSubTitle : ARRAY OF TCHAR);
BEGIN
	FUNC SendMessage (hWizardDlg, PSM_SETHEADERSUBTITLE, iPageIndex, CAST (LPARAM, ADR(pszHeaderSubTitle)));
END PropSheet_SetHeaderSubTitle;

PROCEDURE PropSheet_SetHeaderSubTitleA (hWizardDlg : HWND; iPageIndex : WINT; pszHeaderSubTitle : ARRAY OF ACHAR);
BEGIN
	FUNC SendMessageA (hWizardDlg, PSM_SETHEADERSUBTITLEA, iPageIndex, CAST (LPARAM, ADR(pszHeaderSubTitle)));
END PropSheet_SetHeaderSubTitleA;

PROCEDURE PropSheet_SetHeaderSubTitleW (hWizardDlg : HWND; iPageIndex : WINT; pszHeaderSubTitle : ARRAY OF WCHAR);
BEGIN
	FUNC SendMessageW (hWizardDlg, PSM_SETHEADERSUBTITLEW, iPageIndex, CAST (LPARAM, ADR(pszHeaderSubTitle)));
END PropSheet_SetHeaderSubTitleW;

PROCEDURE PropSheet_HwndToIndex (hPropSheetDlg : HWND; hPageDlg : HWND) : WINT;
BEGIN
	RETURN SendMessage (hPropSheetDlg, PSM_HWNDTOINDEX, CAST(WPARAM,hPageDlg), 0);
END PropSheet_HwndToIndex;

PROCEDURE PropSheet_HwndToIndexA (hPropSheetDlg : HWND; hPageDlg : HWND) : WINT;
BEGIN
	RETURN SendMessageA (hPropSheetDlg, PSM_HWNDTOINDEX, CAST(WPARAM,hPageDlg), 0);
END PropSheet_HwndToIndexA;

PROCEDURE PropSheet_HwndToIndexW (hPropSheetDlg : HWND; hPageDlg : HWND) : WINT;
BEGIN
	RETURN SendMessageW (hPropSheetDlg, PSM_HWNDTOINDEX, CAST(WPARAM,hPageDlg), 0);
END PropSheet_HwndToIndexW;

PROCEDURE PropSheet_IndexToHwnd (hPropSheetDlg : HWND; iPageIndex : WINT) : HWND;
BEGIN
	RETURN CAST (HWND, SendMessage (hPropSheetDlg, PSM_INDEXTOHWND, VAL(WPARAM,iPageIndex), 0));
END PropSheet_IndexToHwnd;

PROCEDURE PropSheet_IndexToHwndA (hPropSheetDlg : HWND; iPageIndex : WINT) : HWND;
BEGIN
	RETURN CAST (HWND, SendMessageA (hPropSheetDlg, PSM_INDEXTOHWND, VAL(WPARAM,iPageIndex), 0));
END PropSheet_IndexToHwndA;

PROCEDURE PropSheet_IndexToHwndW (hPropSheetDlg : HWND; iPageIndex : WINT) : HWND;
BEGIN
	RETURN CAST (HWND, SendMessageW (hPropSheetDlg, PSM_INDEXTOHWND, VAL(WPARAM,iPageIndex), 0));
END PropSheet_IndexToHwndW;

PROCEDURE PropSheet_PageToIndex (hPropSheetDlg : HWND; hPage : HPROPSHEETPAGE) : WINT;
BEGIN
	RETURN SendMessage (hPropSheetDlg, PSM_PAGETOINDEX, 0, CAST(LPARAM,hPage));
END PropSheet_PageToIndex;

PROCEDURE PropSheet_PageToIndexA (hPropSheetDlg : HWND; hPage : HPROPSHEETPAGE) : WINT;
BEGIN
	RETURN SendMessageA (hPropSheetDlg, PSM_PAGETOINDEX, 0, CAST(LPARAM,hPage));
END PropSheet_PageToIndexA;

PROCEDURE PropSheet_PageToIndexW (hPropSheetDlg : HWND; hPage : HPROPSHEETPAGE) : WINT;
BEGIN
	RETURN SendMessageW (hPropSheetDlg, PSM_PAGETOINDEX, 0, CAST(LPARAM,hPage));
END PropSheet_PageToIndexW;

PROCEDURE PropSheet_IndexToPage (hPropSheetDlg : HWND; iPageIndex : WINT) : HPROPSHEETPAGE;
BEGIN
	RETURN CAST (HPROPSHEETPAGE, SendMessage (hPropSheetDlg, PSM_INDEXTOPAGE, VAL(WPARAM,iPageIndex), 0));
END PropSheet_IndexToPage;

PROCEDURE PropSheet_IndexToPageA (hPropSheetDlg : HWND; iPageIndex : WINT) : HPROPSHEETPAGE;
BEGIN
	RETURN CAST (HPROPSHEETPAGE, SendMessageA (hPropSheetDlg, PSM_INDEXTOPAGE, VAL(WPARAM,iPageIndex), 0));
END PropSheet_IndexToPageA;

PROCEDURE PropSheet_IndexToPageW (hPropSheetDlg : HWND; iPageIndex : WINT) : HPROPSHEETPAGE;
BEGIN
	RETURN CAST (HPROPSHEETPAGE, SendMessageW (hPropSheetDlg, PSM_INDEXTOPAGE, VAL(WPARAM,iPageIndex), 0));
END PropSheet_IndexToPageW;

PROCEDURE PropSheet_IdToIndex (hPropSheetDlg : HWND; hPageID : WINT) : WINT;
BEGIN
	RETURN SendMessage (hPropSheetDlg, PSM_IDTOINDEX, 0, hPageID);
END PropSheet_IdToIndex;

PROCEDURE PropSheet_IdToIndexA (hPropSheetDlg : HWND; hPageID : WINT) : WINT;
BEGIN
	RETURN SendMessageA (hPropSheetDlg, PSM_IDTOINDEX, 0, hPageID);
END PropSheet_IdToIndexA;

PROCEDURE PropSheet_IdToIndexW (hPropSheetDlg : HWND; hPageID : WINT) : WINT;
BEGIN
	RETURN SendMessageW (hPropSheetDlg, PSM_IDTOINDEX, 0, hPageID);
END PropSheet_IdToIndexW;

PROCEDURE PropSheet_IndexToId (hPropSheetDlg : HWND; iPageIndex : WINT) : WINT;
BEGIN
	RETURN SendMessage (hPropSheetDlg, PSM_INDEXTOID, iPageIndex, 0);
END PropSheet_IndexToId;

PROCEDURE PropSheet_IndexToIdA (hPropSheetDlg : HWND; iPageIndex : WINT) : WINT;
BEGIN
	RETURN SendMessageA (hPropSheetDlg, PSM_INDEXTOID, iPageIndex, 0);
END PropSheet_IndexToIdA;

PROCEDURE PropSheet_IndexToIdW (hPropSheetDlg : HWND; iPageIndex : WINT) : WINT;
BEGIN
	RETURN SendMessageW (hPropSheetDlg, PSM_INDEXTOID, iPageIndex, 0);
END PropSheet_IndexToIdW;

PROCEDURE PropSheet_GetResult (hPropSheetDlg : HWND) : WINT;
BEGIN
	RETURN SendMessage (hPropSheetDlg, PSM_GETRESULT, 0, 0);
END PropSheet_GetResult;

PROCEDURE PropSheet_GetResultA (hPropSheetDlg : HWND) : WINT;
BEGIN
	RETURN SendMessageA (hPropSheetDlg, PSM_GETRESULT, 0, 0);
END PropSheet_GetResultA;

PROCEDURE PropSheet_GetResultW (hPropSheetDlg : HWND) : WINT;
BEGIN
	RETURN SendMessageW (hPropSheetDlg, PSM_GETRESULT, 0, 0);
END PropSheet_GetResultW;

PROCEDURE PropSheet_RecalcPageSizes (hPropSheetDlg : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessage (hPropSheetDlg, PSM_RECALCPAGESIZES, 0, 0));
END PropSheet_RecalcPageSizes;

PROCEDURE PropSheet_RecalcPageSizesA (hPropSheetDlg : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageA (hPropSheetDlg, PSM_RECALCPAGESIZES, 0, 0));
END PropSheet_RecalcPageSizesA;

PROCEDURE PropSheet_RecalcPageSizesW (hPropSheetDlg : HWND) : BOOL;
BEGIN
	RETURN VAL (BOOL, SendMessageW (hPropSheetDlg, PSM_RECALCPAGESIZES, 0, 0));
END PropSheet_RecalcPageSizesW;

PROCEDURE PropSheet_SetNextTextW (hDlg : HWND; lpszText : ARRAY OF WCHAR);
BEGIN
	FUNC SendMessageW (hDlg, PSM_SETNEXTTEXTW, 0, CAST (LPARAM, ADR(lpszText)));
END PropSheet_SetNextTextW;

PROCEDURE PropSheet_ShowWizButtons (hDlg : HWND; dwFlag : DWORD; dwButton : DWORD);
BEGIN
	FUNC SendMessage (hDlg, PSM_SHOWWIZBUTTONS, dwFlag, dwButton);
END PropSheet_ShowWizButtons;

PROCEDURE PropSheet_ShowWizButtonsA (hDlg : HWND; dwFlag : DWORD; dwButton : DWORD);
BEGIN
	FUNC SendMessageA (hDlg, PSM_SHOWWIZBUTTONS, dwFlag, dwButton);
END PropSheet_ShowWizButtonsA;

PROCEDURE PropSheet_ShowWizButtonsW (hDlg : HWND; dwFlag : DWORD; dwButton : DWORD);
BEGIN
	FUNC SendMessageW (hDlg, PSM_SHOWWIZBUTTONS, dwFlag, dwButton);
END PropSheet_ShowWizButtonsW;

PROCEDURE PropSheet_EnableWizButtons (hDlg : HWND; dwState : DWORD; dwMask : DWORD);
BEGIN
	FUNC SendMessage (hDlg, PSM_ENABLEWIZBUTTONS, dwState, dwMask);
END PropSheet_EnableWizButtons;

PROCEDURE PropSheet_EnableWizButtonsA (hDlg : HWND; dwState : DWORD; dwMask : DWORD);
BEGIN
	FUNC SendMessageA (hDlg, PSM_ENABLEWIZBUTTONS, dwState, dwMask);
END PropSheet_EnableWizButtonsA;

PROCEDURE PropSheet_EnableWizButtonsW (hDlg : HWND; dwState : DWORD; dwMask : DWORD);
BEGIN
	FUNC SendMessageW (hDlg, PSM_ENABLEWIZBUTTONS, dwState, dwMask);
END PropSheet_EnableWizButtonsW;

PROCEDURE PropSheet_SetButtonTextW (hDlg : HWND; dwButton : DWORD; lpszText : ARRAY OF WCHAR);
BEGIN
	FUNC SendMessageW (hDlg, PSM_SETBUTTONTEXTW, dwButton, CAST (LPARAM, ADR(lpszText)));
END PropSheet_SetButtonTextW;

(* Up-dow controls. *)

PROCEDURE UpDown_SetRange(hwnd : HWND; min, max : INTEGER);
BEGIN
    SendMessage(hwnd, UDM_SETRANGE, 0, MAKELONG(min, max));
END UpDown_SetRange;

PROCEDURE UpDown_SetRangeA(hwnd : HWND; min, max : INTEGER);
BEGIN
    SendMessageA(hwnd, UDM_SETRANGE, 0, MAKELONG(min, max));
END UpDown_SetRangeA;

PROCEDURE UpDown_SetRangeW(hwnd : HWND; min, max : INTEGER);
BEGIN
    SendMessageW(hwnd, UDM_SETRANGE, 0, MAKELONG(min, max));
END UpDown_SetRangeW;

PROCEDURE UpDown_SetRange32(hwnd : HWND; min, max : INTEGER);
BEGIN
    SendMessage(hwnd, UDM_SETRANGE32, min, max);
END UpDown_SetRange32;

PROCEDURE UpDown_SetRange32A(hwnd : HWND; min, max : INTEGER);
BEGIN
    SendMessageA(hwnd, UDM_SETRANGE32, min, max);
END UpDown_SetRange32A;

PROCEDURE UpDown_SetRange32W(hwnd : HWND; min, max : INTEGER);
BEGIN
    SendMessageW(hwnd, UDM_SETRANGE32, min, max);
END UpDown_SetRange32W;

PROCEDURE UpDown_SetPos(hwnd : HWND; pos : INTEGER);
BEGIN
    SendMessage(hwnd, UDM_SETPOS, 0, MAKELONG(pos, 0));
END UpDown_SetPos;

PROCEDURE UpDown_SetPosA(hwnd : HWND; pos : INTEGER);
BEGIN
    SendMessageA(hwnd, UDM_SETPOS, 0, MAKELONG(pos, 0));
END UpDown_SetPosA;

PROCEDURE UpDown_SetPosW(hwnd : HWND; pos : INTEGER);
BEGIN
    SendMessageW(hwnd, UDM_SETPOS, 0, MAKELONG(pos, 0));
END UpDown_SetPosW;

PROCEDURE UpDown_GetPos(hwnd : HWND; VAR pos : INTEGER) : BOOL;
VAR
    res         : DWORD;
BEGIN
    res := SendMessage(hwnd, UDM_GETPOS, 0, 0);
    pos := CAST(INTEGER16, LOWORD(pos));
    RETURN HIWORD(res) = 0;
END UpDown_GetPos;

PROCEDURE UpDown_GetPosA(hwnd : HWND; VAR pos : INTEGER) : BOOL;
VAR
    res         : DWORD;
BEGIN
    res := SendMessageA(hwnd, UDM_GETPOS, 0, 0);
    pos := CAST(INTEGER16, LOWORD(pos));
    RETURN HIWORD(res) = 0;
END UpDown_GetPosA;

PROCEDURE UpDown_GetPosW(hwnd : HWND; VAR pos : INTEGER) : BOOL;
VAR
    res         : DWORD;
BEGIN
    res := SendMessageW(hwnd, UDM_GETPOS, 0, 0);
    pos := CAST(INTEGER16, LOWORD(pos));
    RETURN HIWORD(res) = 0;
END UpDown_GetPosW;

PROCEDURE UpDown_SetPos32(hwnd : HWND; pos : INTEGER);
BEGIN
    SendMessage(hwnd, UDM_SETPOS32, 0, pos);
END UpDown_SetPos32;

PROCEDURE UpDown_SetPos32A(hwnd : HWND; pos : INTEGER);
BEGIN
    SendMessageA(hwnd, UDM_SETPOS32, 0, pos);
END UpDown_SetPos32A;

PROCEDURE UpDown_SetPos32W(hwnd : HWND; pos : INTEGER);
BEGIN
    SendMessageW(hwnd, UDM_SETPOS32, 0, pos);
END UpDown_SetPos32W;

PROCEDURE UpDown_GetPos32(hwnd : HWND; VAR pos : INTEGER) : BOOL;
VAR
    <*/PUSH/NOCHECK:U*>
    bool        : BOOL;
    <*/POP*>
BEGIN
    pos := SendMessage(hwnd, UDM_GETPOS32, 0, CAST(LPARAM, ADR(bool)));
    RETURN bool;
END UpDown_GetPos32;

PROCEDURE UpDown_GetPos32A(hwnd : HWND; VAR pos : INTEGER) : BOOL;
VAR
    <*/PUSH/NOCHECK:U*>
    bool        : BOOL;
    <*/POP*>
BEGIN
    pos := SendMessageA(hwnd, UDM_GETPOS32, 0, CAST(LPARAM, ADR(bool)));
    RETURN bool;
END UpDown_GetPos32A;

PROCEDURE UpDown_GetPos32W(hwnd : HWND; VAR pos : INTEGER) : BOOL;
VAR
    <*/PUSH/NOCHECK:U*>
    bool        : BOOL;
    <*/POP*>
BEGIN
    pos := SendMessageW(hwnd, UDM_GETPOS32, 0, CAST(LPARAM, ADR(bool)));
    RETURN bool;
END UpDown_GetPos32W;

PROCEDURE UpDown_SetBase(hwnd : HWND; base : INTEGER) : BOOL;
VAR
    res         : DWORD;
BEGIN
    res := SendMessage(hwnd, UDM_SETBASE, base, 0);
    RETURN res <> 0;
END UpDown_SetBase;

PROCEDURE UpDown_SetBaseA(hwnd : HWND; base : INTEGER) : BOOL;
VAR
    res         : DWORD;
BEGIN
    res := SendMessageA(hwnd, UDM_SETBASE, base, 0);
    RETURN res <> 0;
END UpDown_SetBaseA;

PROCEDURE UpDown_SetBaseW(hwnd : HWND; base : INTEGER) : BOOL;
VAR
    res         : DWORD;
BEGIN
    res := SendMessageW(hwnd, UDM_SETBASE, base, 0);
    RETURN res <> 0;
END UpDown_SetBaseW;

PROCEDURE UpDown_GetBase(hwnd : HWND; VAR base : INTEGER);
VAR
    res         : DWORD;
BEGIN
    res := SendMessage(hwnd, UDM_GETBASE, 0, 0);
    base := CAST(INTEGER16, LOWORD(res));
END UpDown_GetBase;

PROCEDURE UpDown_GetBaseA(hwnd : HWND; VAR base : INTEGER);
VAR
    res         : DWORD;
BEGIN
    res := SendMessageA(hwnd, UDM_GETBASE, 0, 0);
    base := CAST(INTEGER16, LOWORD(res));
END UpDown_GetBaseA;

PROCEDURE UpDown_GetBaseW(hwnd : HWND; VAR base : INTEGER);
VAR
    res         : DWORD;
BEGIN
    res := SendMessageW(hwnd, UDM_GETBASE, 0, 0);
    base := CAST(INTEGER16, LOWORD(res));
END UpDown_GetBaseW;

PROCEDURE UpDown_GetBuddy(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessage(hwnd, UDM_GETBUDDY, 0, 0));
END UpDown_GetBuddy;

PROCEDURE UpDown_GetBuddyA(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageA(hwnd, UDM_GETBUDDY, 0, 0));
END UpDown_GetBuddyA;

PROCEDURE UpDown_GetBuddyW(hwnd : HWND) : HWND;
BEGIN
    RETURN CAST(HWND, SendMessageW(hwnd, UDM_GETBUDDY, 0, 0));
END UpDown_GetBuddyW;

END COMMCTRL.
