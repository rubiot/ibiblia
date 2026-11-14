unit menubar_skin;

{ Dark/themed painting of a form's main menu bar (Win32 UAH messages and
  owner-drawn top-level items), ported from the 1.0.1 mod's formprincipal.
  Popup menus are themed by unitskins. }

{$mode objfpc}{$H+}
{$WARN 4055 OFF}
{$WARN 4056 OFF}
{$WARN 4082 OFF}

interface

uses
  // Windows first, so LCLType's TDrawItemStruct (_hDC) wins, as in the mod
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  Classes, SysUtils, Controls, Forms, Menus, Graphics, LCLType;

{ Installs (AIsDark) or removes the themed menu bar on AForm. }
procedure ApplyMenuBarSkin(AForm: TCustomForm; AIsDark: Boolean);

implementation

uses
  unitskins;

{$IFDEF WINDOWS}
const
  WM_UAHDRAWMENU = $0091;
  WM_UAHDRAWMENUITEM = $0092;
  UAH_OBJID_MENU = LongInt($FFFFFFFD);
  MF_BYPOSITION = $00000400;
  MF_OWNERDRAW  = $00000100;
  MF_POPUP      = $00000010;
  MF_STRING     = $00000000;
  ODT_MENU      = 1;
  ODS_SELECTED  = $0001;
  ODS_GRAYED    = $0002;
  ODS_DISABLED  = $0004;
  ODS_HOTLIGHT  = $0040;
  ODS_NOACCEL   = $0100;

function WinModifyMenuW(hMnu: HMENU; uPosition: UINT; uFlags: UINT; uIDNewItem: UINT_PTR; lpNewItem: PtrUInt): WINBOOL; stdcall; external 'user32' name 'ModifyMenuW';

type
  TUAHMenuBarInfo = record
    cbSize: DWORD;
    rcBar: RECT;
    hMenu: HMENU;
    hwndMenu: HWND;
    fBarFocused: BOOL;
    fFocused: BOOL;
  end;

  PUAHMenu = ^TUAHMenu;
  TUAHMenu = record
    hmenu: HMENU;
    hdc: HDC;
    dwFlags: DWORD;
    _pad: DWORD;
  end;

  PUAHMenuItem = ^TUAHMenuItem;
  TUAHMenuItem = record
    iPosition: Integer;
    rgsizeBar: array[0..1] of record cx, cy: DWORD; end;
    rgsizePopup: array[0..3] of record cx, cy: DWORD; end;
  end;

  PUAHDrawMenuItem = ^TUAHDrawMenuItem;
  TUAHDrawMenuItem = record
    dis: DRAWITEMSTRUCT;
    um: TUAHMenu;
    umi: TUAHMenuItem;
  end;

function WinGetMenuBarInfo(hwnd: HWND; idObject: LongInt; idItem: LongInt; pmbi: Pointer): WINBOOL; stdcall; external 'user32' name 'GetMenuBarInfo';
function WinClientToScreen(hwnd: HWND; lpPoint: Pointer): WINBOOL; stdcall; external 'user32' name 'ClientToScreen';

procedure PaintMenuBarNC(hWnd: HWND; dwRefData: PtrUInt);
var
  rcWindow, rcBarInWin, rcEmpty, rcLine: RECT;
  mbiBar, mbiLast: TUAHMenuBarInfo;
  hdcWin: THandle;
  br: THandle;
  ptClient: TPoint;
  cnt, k: Integer;
  hM: HMENU;
  clientTopInWindow, menuHeight, rightOfLastItem, totalWidth: Integer;
  cap: string;
begin
  if not IsThemedSkin then Exit;

  rcWindow := Default(RECT);
  GetWindowRect(hWnd, @rcWindow);

  ptClient.x := 0;
  ptClient.y := 0;
  WinClientToScreen(hWnd, @ptClient);
  clientTopInWindow := ptClient.y - rcWindow.Top;
  menuHeight := GetSystemMetrics(SM_CYMENU);

  // 1. Determine the menu bar rectangle in Window DC coordinates
  FillChar(mbiBar, SizeOf(mbiBar), 0);
  mbiBar.cbSize := SizeOf(mbiBar);
  if WinGetMenuBarInfo(hWnd, UAH_OBJID_MENU, 0, @mbiBar) and (mbiBar.rcBar.Bottom > mbiBar.rcBar.Top) then
  begin
    rcBarInWin.Left := mbiBar.rcBar.Left - rcWindow.Left;
    rcBarInWin.Top := mbiBar.rcBar.Top - rcWindow.Top;
    rcBarInWin.Right := mbiBar.rcBar.Right - rcWindow.Left;
    rcBarInWin.Bottom := mbiBar.rcBar.Bottom - rcWindow.Top;
  end
  else
  begin
    rcBarInWin.Left := ptClient.x - rcWindow.Left;
    rcBarInWin.Top := clientTopInWindow - menuHeight;
    rcBarInWin.Right := rcWindow.Right - rcWindow.Left;
    rcBarInWin.Bottom := clientTopInWindow;
  end;

  // 2. Find where the last menu item ends
  hM := GetMenu(hWnd);
  if hM <> 0 then
    cnt := GetMenuItemCount(hM)
  else
    cnt := 0;

  rightOfLastItem := 0;
  if cnt > 0 then
  begin
    FillChar(mbiLast, SizeOf(mbiLast), 0);
    mbiLast.cbSize := SizeOf(mbiLast);
    if WinGetMenuBarInfo(hWnd, UAH_OBJID_MENU, cnt, @mbiLast) and (mbiLast.rcBar.Right > mbiLast.rcBar.Left) then
      rightOfLastItem := mbiLast.rcBar.Right - rcWindow.Left;
  end;

  // Fallback approximation if GetMenuBarInfo for item cnt failed
  if (rightOfLastItem <= rcBarInWin.Left) and IsValidSubclassedControl(dwRefData) and (TObject(dwRefData) is TCustomForm) and Assigned(TCustomForm(dwRefData).Menu) then
  begin
    totalWidth := 0;
    for k := 0 to TCustomForm(dwRefData).Menu.Items.Count - 1 do
    begin
      cap := TCustomForm(dwRefData).Menu.Items[k].Caption;
      totalWidth := totalWidth + Length(cap) * 8 + 18;
    end;
    rightOfLastItem := rcBarInWin.Left + totalWidth;
  end;

  hdcWin := GetWindowDC(hWnd);
  if hdcWin <> 0 then
  begin
    br := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));

    // Fill the empty unskinned area to the right of the menu items
    if (rightOfLastItem > rcBarInWin.Left) and (rightOfLastItem < rcBarInWin.Right) then
    begin
      rcEmpty.Left := rightOfLastItem;
      rcEmpty.Top := rcBarInWin.Top;
      rcEmpty.Right := rcBarInWin.Right;
      rcEmpty.Bottom := rcBarInWin.Bottom;
      FillRect(hdcWin, rcEmpty, br);
    end
    else if rightOfLastItem <= rcBarInWin.Left then
    begin
      // Fallback: fill the entire bar
      FillRect(hdcWin, rcBarInWin, br);
    end;

    // Erase the 1-pixel separator line right above the client area
    rcLine.Left := 0;
    rcLine.Right := rcWindow.Right - rcWindow.Left;
    rcLine.Top := clientTopInWindow - 1;
    rcLine.Bottom := clientTopInWindow;
    FillRect(hdcWin, rcLine, br);

    DeleteObject(br);
    ReleaseDC(hWnd, hdcWin);
  end;
end;



function MenuSubclassProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM;
  uIdSubclass: PtrUInt; dwRefData: PtrUInt): LRESULT; stdcall;
var
  pUDM: PUAHMenu;
  pUDMI: PUAHDrawMenuItem;
  pMIS: PMEASUREITEMSTRUCT;
  pDIS: PDRAWITEMSTRUCT;
  mbi: TUAHMenuBarInfo;
  rc, rcWindow, rcItemRect: RECT;
  br: THandle;
  menuString: array[0..255] of WideChar;
  capStr: WideString;
  txtLen, idx: Integer;
  oldBkMode: Integer;
  oldTextColor: COLORREF;
  dwFlags: DWORD;
  hFont, oldFont: THandle;
  hdcMem, itemHdc: HDC;
  isHot, isSelected, isDis, matched: Boolean;
  k: Integer;
  itemBg, itemCol: TColor;
  sz: TSize;
begin
  if IsThemedSkin then
  begin
    case uMsg of
      WM_MEASUREITEM:
      begin
        pMIS := PMEASUREITEMSTRUCT(lParam);
        if (pMIS <> nil) and (pMIS^.CtlType = ODT_MENU) then
        begin
          matched := False;
          capStr := '';
          if IsValidSubclassedControl(dwRefData) and (TObject(dwRefData) is TCustomForm) and Assigned(TCustomForm(dwRefData).Menu) then
          begin
            for k := 0 to TCustomForm(dwRefData).Menu.Items.Count - 1 do
            begin
              if (PtrUInt(TCustomForm(dwRefData).Menu.Items[k]) = pMIS^.itemData) or
                 (pMIS^.itemData = PtrUInt(k)) then
              begin
                capStr := WideString(TCustomForm(dwRefData).Menu.Items[k].Caption);
                matched := True;
                Break;
              end;
            end;
          end;

          if matched then
          begin
            hdcMem := GetDC(hWnd);
            if hdcMem <> 0 then
            begin
              hFont := THandle(SendMessage(hWnd, WM_GETFONT, 0, 0));
              if (hFont = 0) and IsValidSubclassedControl(dwRefData) and (TObject(dwRefData) is TControl) then
                hFont := THandle(TControl(dwRefData).Font.Handle);

              if hFont <> 0 then
                oldFont := SelectObject(hdcMem, hFont)
              else
                oldFont := 0;

              FillChar(sz, SizeOf(sz), 0);
              if Length(capStr) > 0 then
                GetTextExtentPoint32W(hdcMem, @capStr[1], Length(capStr), @sz)
              else
                sz.cx := 45;

              if oldFont <> 0 then
                SelectObject(hdcMem, oldFont);
              ReleaseDC(hWnd, hdcMem);
            end
            else
              sz.cx := 50;

            pMIS^.itemWidth := sz.cx + 18;
            pMIS^.itemHeight := GetSystemMetrics(SM_CYMENU);
            Result := 1;
            Exit;
          end;
        end;
      end;

      WM_DRAWITEM:
      begin
        pDIS := PDRAWITEMSTRUCT(lParam);
        if (pDIS <> nil) and (pDIS^.CtlType = ODT_MENU) then
        begin
          matched := False;
          capStr := '';
          if IsValidSubclassedControl(dwRefData) and (TObject(dwRefData) is TCustomForm) and Assigned(TCustomForm(dwRefData).Menu) then
          begin
            for k := 0 to TCustomForm(dwRefData).Menu.Items.Count - 1 do
            begin
              if (PtrUInt(TCustomForm(dwRefData).Menu.Items[k]) = pDIS^.itemData) or
                 (pDIS^.itemData = PtrUInt(k)) then
              begin
                capStr := WideString(TCustomForm(dwRefData).Menu.Items[k].Caption);
                matched := True;
                Break;
              end;
            end;
          end;

          if matched then
          begin
            itemHdc := pDIS^._hDC;
            isHot := (pDIS^.itemState and ODS_HOTLIGHT) <> 0;
            isSelected := (pDIS^.itemState and ODS_SELECTED) <> 0;
            isDis := (pDIS^.itemState and (ODS_GRAYED or ODS_DISABLED)) <> 0;

            if isSelected then
              itemBg := CurrentSkin.HighlightBg
            else if isHot then
              itemBg := CurrentSkin.PointedBg
            else
              itemBg := CurrentSkin.PanelBg;

            br := CreateSolidBrush(ColorToRGB(itemBg));
            FillRect(itemHdc, pDIS^.rcItem, br);
            DeleteObject(br);

            if Length(capStr) > 0 then
            begin
              hFont := THandle(SendMessage(hWnd, WM_GETFONT, 0, 0));
              if (hFont = 0) and IsValidSubclassedControl(dwRefData) and (TObject(dwRefData) is TControl) then
                hFont := THandle(TControl(dwRefData).Font.Handle);

              if hFont <> 0 then
                oldFont := SelectObject(itemHdc, hFont)
              else
                oldFont := 0;

              oldBkMode := SetBkMode(itemHdc, TRANSPARENT);
              if isDis then
                oldTextColor := SetTextColor(itemHdc, ColorToRGB(CurrentSkin.WindowTextDim))
              else if isHot or isSelected then
                oldTextColor := SetTextColor(itemHdc, ColorToRGB(CurrentSkin.PointedText))
              else
                oldTextColor := SetTextColor(itemHdc, ColorToRGB(CurrentSkin.PanelText));

              dwFlags := DT_CENTER or DT_VCENTER or DT_SINGLELINE;
              if (pDIS^.itemState and ODS_NOACCEL) <> 0 then
                dwFlags := dwFlags or $00100000;

              rcItemRect := pDIS^.rcItem;
              DrawTextW(itemHdc, @capStr[1], Length(capStr), rcItemRect, dwFlags);

              SetTextColor(itemHdc, oldTextColor);
              SetBkMode(itemHdc, oldBkMode);
              if oldFont <> 0 then
                SelectObject(itemHdc, oldFont);
            end;

            Result := 1;
            Exit;
          end
          else
          begin
            if DrawThemedPopupMenuItem(hWnd, lParam) then
            begin
              Result := 1;
              Exit;
            end;
            PreFillDarkMenuItem(hWnd, lParam);
          end;
        end;
      end;

      WM_INITMENUPOPUP:
      begin
        SubclassAllPopupMenuWindows;
      end;

      WM_UAHDRAWMENU:
      begin
        pUDM := PUAHMenu(lParam);
        if (pUDM <> nil) and (pUDM^.hdc <> 0) then
        begin
          if (pUDM^.hmenu = 0) or (pUDM^.hmenu = GetMenu(hWnd)) then
          begin
            FillChar(mbi, SizeOf(mbi), 0);
            mbi.cbSize := SizeOf(mbi);
            if WinGetMenuBarInfo(hWnd, UAH_OBJID_MENU, 0, @mbi) then
            begin
              rcWindow := Default(RECT);
              GetWindowRect(hWnd, @rcWindow);
              rc.Left := mbi.rcBar.Left - rcWindow.Left;
              rc.Top := mbi.rcBar.Top - rcWindow.Top;
              rc.Right := mbi.rcBar.Right - rcWindow.Left;
              rc.Bottom := mbi.rcBar.Bottom - rcWindow.Top;
            end
            else
            begin
              rcWindow := Default(RECT);
              GetWindowRect(hWnd, @rcWindow);
              rc.Left := 0;
              rc.Top := 0;
              rc.Right := rcWindow.Right - rcWindow.Left;
              rc.Bottom := 60;
            end;

            br := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));
            FillRect(pUDM^.hdc, rc, br);
            DeleteObject(br);
            Result := 0;
            Exit;
          end
          else
          begin
            FillChar(rc, SizeOf(rc), 0);
            if GetClipBox(pUDM^.hdc, @rc) <> ERROR then
            begin
              FillRect(pUDM^.hdc, rc, GetCachedPopupMenuBrush);
              br := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBorder));
              FrameRect(pUDM^.hdc, rc, br);
              DeleteObject(br);
              Result := 0;
              Exit;
            end;
          end;
        end;
      end;

      WM_UAHDRAWMENUITEM:
      begin
        pUDMI := PUAHDrawMenuItem(lParam);
        if (pUDMI <> nil) and (pUDMI^.um.hdc <> 0) then
        begin
          if (pUDMI^.dis.itemState and (ODS_HOTLIGHT or ODS_SELECTED)) <> 0 then
            br := CreateSolidBrush(ColorToRGB(CurrentSkin.PointedBg))
          else
            br := CreateSolidBrush(ColorToRGB(CurrentSkin.PanelBg));

          FillRect(pUDMI^.um.hdc, pUDMI^.dis.rcItem, br);
          DeleteObject(br);

          FillChar(menuString, SizeOf(menuString), 0);
          txtLen := GetMenuStringW(pUDMI^.um.hmenu, pUDMI^.umi.iPosition, @menuString[0], 255, MF_BYPOSITION);

          if (txtLen <= 0) and IsValidSubclassedControl(dwRefData) and (TObject(dwRefData) is TCustomForm) and Assigned(TCustomForm(dwRefData).Menu) then
          begin
            if (pUDMI^.umi.iPosition >= 0) and (pUDMI^.umi.iPosition < TCustomForm(dwRefData).Menu.Items.Count) then
            begin
              capStr := WideString(TCustomForm(dwRefData).Menu.Items[pUDMI^.umi.iPosition].Caption);
              txtLen := Min(255, Length(capStr));
              if txtLen > 0 then
              begin
                Move(capStr[1], menuString[0], txtLen * SizeOf(WideChar));
                menuString[txtLen] := #0;
              end;
            end;
          end;

          if txtLen > 0 then
          begin
            hFont := THandle(SendMessage(hWnd, WM_GETFONT, 0, 0));
            if (hFont = 0) and IsValidSubclassedControl(dwRefData) and (TObject(dwRefData) is TControl) then
              hFont := THandle(TControl(dwRefData).Font.Handle);

            if hFont <> 0 then
              oldFont := SelectObject(pUDMI^.um.hdc, hFont)
            else
              oldFont := 0;

            dwFlags := DT_CENTER or DT_SINGLELINE or DT_VCENTER;
            if (pUDMI^.dis.itemState and ODS_NOACCEL) <> 0 then
              dwFlags := dwFlags or $00100000;

            oldBkMode := SetBkMode(pUDMI^.um.hdc, TRANSPARENT);
            if (pUDMI^.dis.itemState and (ODS_HOTLIGHT or ODS_SELECTED)) <> 0 then
              oldTextColor := SetTextColor(pUDMI^.um.hdc, ColorToRGB(CurrentSkin.PointedText))
            else if (pUDMI^.dis.itemState and (ODS_GRAYED or ODS_DISABLED)) <> 0 then
              oldTextColor := SetTextColor(pUDMI^.um.hdc, ColorToRGB(CurrentSkin.WindowTextDim))
            else
              oldTextColor := SetTextColor(pUDMI^.um.hdc, ColorToRGB(CurrentSkin.PanelText));

            rcItemRect := pUDMI^.dis.rcItem;
            DrawTextW(pUDMI^.um.hdc, @menuString[0], txtLen, rcItemRect, dwFlags);

            SetTextColor(pUDMI^.um.hdc, oldTextColor);
            SetBkMode(pUDMI^.um.hdc, oldBkMode);
            if oldFont <> 0 then
              SelectObject(pUDMI^.um.hdc, oldFont);
          end;

          Result := 0;
          Exit;
        end;
      end;

      WM_NCPAINT, WM_NCACTIVATE:
      begin
        Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
        PaintMenuBarNC(hWnd, dwRefData);
        Exit;
      end;
    end;
  end;

  Result := fnDefSubclassProc(hWnd, uMsg, wParam, lParam);
end;

procedure SetupMenuOwnerDraw(AForm: TCustomForm; AIsDark: Boolean);
var
  hM, hSub: HMENU;
  i, cnt: Integer;
  cap: string;
  wcap: WideString;
begin
  if not AForm.HandleAllocated or (AForm.Handle = 0) or not Assigned(AForm.Menu) then exit;
  hM := GetMenu(AForm.Handle);
  if hM = 0 then exit;

  cnt := GetMenuItemCount(hM);
  for i := 0 to cnt - 1 do
  begin
    hSub := GetSubMenu(hM, i);
    if hSub <> 0 then
    begin
      if AIsDark then
      begin
        if i < AForm.Menu.Items.Count then
          WinModifyMenuW(hM, i, MF_BYPOSITION or MF_OWNERDRAW or MF_POPUP, hSub, PtrUInt(AForm.Menu.Items[i]))
        else
          WinModifyMenuW(hM, i, MF_BYPOSITION or MF_OWNERDRAW or MF_POPUP, hSub, PtrUInt(i));
      end
      else
      begin
        if i < AForm.Menu.Items.Count then
        begin
          cap := AForm.Menu.Items[i].Caption;
          wcap := UTF8Decode(cap);
          WinModifyMenuW(hM, i, MF_BYPOSITION or MF_STRING or MF_POPUP, hSub, PtrUInt(PWideChar(wcap)));
        end;
      end;
    end;
  end;
  DrawMenuBar(AForm.Handle);
  if AIsDark then
    PaintMenuBarNC(AForm.Handle, PtrUInt(AForm));
end;


var
  SubclassedForms: TList = nil;

procedure ApplyMenuBarSkin(AForm: TCustomForm; AIsDark: Boolean);
begin
  if not Assigned(AForm) or not AForm.HandleAllocated or not Assigned(AForm.Menu) then
    Exit;
  if SubclassedForms = nil then
    SubclassedForms := TList.Create;
  if AIsDark then
  begin
    if Assigned(fnSetWindowSubclass) and (SubclassedForms.IndexOf(AForm) < 0) then
    begin
      fnSetWindowSubclass(AForm.Handle, @MenuSubclassProc, 1001, PtrUInt(AForm));
      SubclassedForms.Add(AForm);
    end;
  end
  else if SubclassedForms.IndexOf(AForm) >= 0 then
  begin
    if Assigned(fnRemoveWindowSubclass) then
      fnRemoveWindowSubclass(AForm.Handle, @MenuSubclassProc, 1001);
    SubclassedForms.Remove(AForm);
  end;
  SetupMenuOwnerDraw(AForm, AIsDark);
  DrawMenuBar(AForm.Handle);
  // repaint the whole frame so the bar takes the new skin at once
  RedrawWindow(AForm.Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW);
  if AIsDark then
    PaintMenuBarNC(AForm.Handle, PtrUInt(AForm));
end;

finalization
  SubclassedForms.Free;
{$ELSE}
procedure ApplyMenuBarSkin(AForm: TCustomForm; AIsDark: Boolean);
begin
end;
{$ENDIF}

end.
