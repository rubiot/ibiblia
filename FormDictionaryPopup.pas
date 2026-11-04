unit FormDictionaryPopup;

{$mode objfpc}{$H+}

interface

uses
  ui_style, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  {$IFDEF WINDOWS}Windows, Messages,{$ENDIF}
  ExtCtrls, ComCtrls, LCLType, LMessages, RichMemo, Types, Math;

type
  { TFrmDictionaryPopup }

  TFrmDictionaryPopup = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    RichMemoStrong: TRichMemo;
    RichMemoMorphology: TRichMemo;
    Splitter1: TSplitter;
    TabControlStrongs: TTabControl;
    TabControlMorphologies: TTabControl;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RichMemoMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TabControlMorphologiesChange(Sender: TObject);
    procedure TabControlStrongsChange(Sender: TObject);
  private
    { private declarations }
    FStrongs: TStringList;
    FMorphologies: TStringList;
    FInSizeMove: Boolean;
    FPositioning: Boolean;
    FLastAnchorPoint: TPoint;
    FAutoHideTimer: TTimer;
    FScrollbarPaintTimer: TTimer;
    FScrollbarPaintTicks: Integer;
    FHoveringSyntagm: Boolean;
    FCtrlEnteredPopup: Boolean;
    FMorphologyPaneHeight: Integer;
    FPopupWidth: Integer;
    FPopupHeight: Integer;
    function IsCtrlKeyHeld: Boolean;
    function IsCursorInsidePopupWindow: Boolean;
    procedure AutoHideTimerTick(Sender: TObject);
    procedure ScrollbarPaintTimerTick(Sender: TObject);
    procedure ClampToMonitorWorkarea(const RefPoint: TPoint);
    procedure Splitter1Moved(Sender: TObject);
  protected
    procedure DoOnResize; override;
    procedure WndProc(var TheMessage: TLMessage); override;
  public
    { public declarations }
    procedure ApplySkin;
    procedure SaveLayoutToOpts;
    procedure ShowRtf(Memo: TRichMemo; const Rtf: string);
    procedure AddStrong(const strong: string; const definition: string);
    procedure AddMorphology(const morfo: string; const definition: string);
    procedure ShowAt(x: Integer; y: Integer);
    procedure HidePopup;
    procedure NotifySyntagmHoverEnter;
    procedure RequestDeferredHide;
    function IsSizingOrMoving: Boolean;
    function IsPointerInteracting: Boolean;
    property Strongs: TStringList read FStrongs;
    property Morphologies: TStringList read FMorphologies;
  public
    procedure AfterConstruction; override;
  end;

var
  FrmDictionaryPopup: TFrmDictionaryPopup;

implementation

uses formPrincipal, unitskins;

{ Dictionary entries are RTF with their own colour table (black text, maroon
  headwords, dark blue links) and an "auto" colour that RichEdit shows as
  black. On a dark skin the auto entry becomes the skin's note text, the dark
  colours are swapped for light ones, and each paragraph is reset to it.
  Taken from the 1.0.1 mod (formpopup.pas). }
function AdaptRtfForSkin(const ARtf: string; const ASkin: TSkinColors): string;
var
  lightColorStr: string;
  rgbCol: LongInt;
  sb: TStringBuilder;
  i, len: Integer;
begin
  Result := ARtf;
  if (Result = '') or not ASkin.IsDark then exit;

  rgbCol := ColorToRGB(ASkin.NoteText);
  lightColorStr := Format('\red%d\green%d\blue%d;', [Red(rgbCol), Green(rgbCol), Blue(rgbCol)]);

  // 1. Set color table index 0 (\cf0, the default/auto entry before the first semicolon)
  //    to ASkin.NoteText without shifting the indices of \cf1, \cf2, \cf3, etc.
  if Pos('{\colortbl ;', Result) > 0 then
    Result := StringReplace(Result, '{\colortbl ;', '{\colortbl ' + lightColorStr, [rfReplaceAll])
  else if Pos('{\colortbl;', Result) > 0 then
    Result := StringReplace(Result, '{\colortbl;', '{\colortbl ' + lightColorStr, [rfReplaceAll]);

  Result := StringReplace(Result, '\red0\green0\blue0;', lightColorStr, [rfReplaceAll]);
  Result := StringReplace(Result, '\red128\green0\blue0;', lightColorStr, [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green0\blue255;', '\red115\green175\blue255;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green0\blue128;', '\red135\green195\blue255;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red150\green60\blue100;', lightColorStr, [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green128\blue0;', '\red125\green225\blue145;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green128\blue128;', '\red115\green215\blue215;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red128\green128\blue0;', '\red235\green235\blue135;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red96\green96\blue96;', '\red170\green175\blue185;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red128\green128\blue128;', '\red185\green195\blue205;', [rfReplaceAll]);

  // 2. Word-boundary scanner to ensure every \pard, \plain, \par defaults to \cf0 (ASkin.NoteText),
  //    not \cf1 (which in many dictionaries is maroon/red for headwords!).
  sb := TStringBuilder.Create;
  try
    len := Length(Result);
    i := 1;
    while i <= len do
    begin
      if (i <= len - 4) and (Copy(Result, i, 5) = '\pard') and
         ((i + 5 > len) or not (Result[i + 5] in ['a'..'z', 'A'..'Z'])) then
      begin
        sb.Append('\pard\cf0 ');
        Inc(i, 5);
      end
      else if (i <= len - 5) and (Copy(Result, i, 6) = '\plain') and
         ((i + 6 > len) or not (Result[i + 6] in ['a'..'z', 'A'..'Z'])) then
      begin
        sb.Append('\plain\cf0 ');
        Inc(i, 6);
      end
      else if (i <= len - 3) and (Copy(Result, i, 4) = '\par') and
         ((i + 4 > len) or not (Result[i + 4] in ['a'..'z', 'A'..'Z'])) then
      begin
        sb.Append('\par\cf0 ');
        Inc(i, 4);
      end
      else
      begin
        sb.Append(Result[i]);
        Inc(i);
      end;
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;

  Result := StringReplace(Result, '\cf0 \cf0 ', '\cf0 ', [rfReplaceAll]);
end;

procedure TFrmDictionaryPopup.ApplySkin;
begin
  ApplyFormStyle(Self);
  if HandleAllocated then
    ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  ApplySkinToForm(Self, CurrentSkin);
  if CurrentSkin.Id = skClassic then
  begin
    Panel1.Color := clWhite;
    Panel2.Color := clWhite;
    RichMemoStrong.Color := clWhite;
    RichMemoStrong.Font.Color := clWindowText;
    RichMemoMorphology.Color := clWhite;
    RichMemoMorphology.Font.Color := clWindowText;
  end
  else
  begin
    Panel1.Color := CurrentSkin.NoteBg;
    Panel2.Color := CurrentSkin.NoteBg;
    RichMemoStrong.Color := CurrentSkin.NoteBg;
    RichMemoStrong.Font.Color := CurrentSkin.NoteText;
    RichMemoMorphology.Color := CurrentSkin.NoteBg;
    RichMemoMorphology.Font.Color := CurrentSkin.NoteText;
  end;
  Splitter1.Invalidate;
  {$IFDEF WINDOWS}
  if RichMemoStrong.HandleAllocated then
  begin
    HookEditControl(RichMemoStrong);
    PaintThemedNCScrollBars(RichMemoStrong.Handle);
  end;
  if RichMemoMorphology.HandleAllocated then
  begin
    HookEditControl(RichMemoMorphology);
    PaintThemedNCScrollBars(RichMemoMorphology.Handle);
  end;
  {$ENDIF}
end;

procedure TFrmDictionaryPopup.SaveLayoutToOpts;
begin
  if not Assigned(opts) then Exit;
  if Visible and (not FPositioning) then
  begin
    if Width >= 180 then
      FPopupWidth := Width;
    if Height >= 140 then
      FPopupHeight := Height;
  end;
  if TabControlMorphologies.Visible and (TabControlMorphologies.Height >= 32) and (not FPositioning) then
    FMorphologyPaneHeight := TabControlMorphologies.Height;
  if FPopupWidth >= 180 then
    opts.WriteInteger('leiaute', 'popup.largura', FPopupWidth)
  else
    opts.WriteInteger('leiaute', 'popup.largura', Width);
  if FPopupHeight >= 140 then
    opts.WriteInteger('leiaute', 'popup.altura', FPopupHeight)
  else
    opts.WriteInteger('leiaute', 'popup.altura', Height);
  opts.WriteInteger('leiaute', 'popup.morfo.altura', FMorphologyPaneHeight);
  opts.WriteInteger('leiaute', 'popup.panel2.altura', FMorphologyPaneHeight);
  opts.WriteInteger('leiaute', 'popup.zoom.strong', Trunc(RichMemoStrong.ZoomFactor * 10));
  opts.WriteInteger('leiaute', 'popup.zoom.morfo',  Trunc(RichMemoMorphology.ZoomFactor * 10));
  opts.UpdateFile;
end;

procedure TFrmDictionaryPopup.Splitter1Moved(Sender: TObject);
begin
  if TabControlMorphologies.Visible and (TabControlMorphologies.Height >= 32) then
    FMorphologyPaneHeight := TabControlMorphologies.Height;
  SaveLayoutToOpts;
end;

procedure TFrmDictionaryPopup.ShowRtf(Memo: TRichMemo; const Rtf: string);
begin
  Memo.ParentColor := False;
  Memo.ParentFont := False;
  if CurrentSkin.Id = skClassic then
  begin
    Memo.Color := clWhite;
    Memo.Font.Color := clWindowText;
    Panel1.Color := clWhite;
    Panel2.Color := clWhite;
  end
  else
  begin
    Memo.Color := CurrentSkin.NoteBg;
    Memo.Font.Color := CurrentSkin.NoteText;
    Panel1.Color := CurrentSkin.NoteBg;
    Panel2.Color := CurrentSkin.NoteBg;
  end;
  ApplyDarkModeToControl(Memo, CurrentSkin.IsDark);
  Memo.Rtf := AdaptRtfForSkin(Rtf, CurrentSkin);
  {$IFDEF WINDOWS}
  if Memo.HandleAllocated then
  begin
    HookEditControl(Memo);
    PaintThemedNCScrollBars(Memo.Handle);
  end;
  if Assigned(FScrollbarPaintTimer) then
  begin
    FScrollbarPaintTicks := 0;
    FScrollbarPaintTimer.Enabled := True;
  end;
  {$ENDIF}
end;

{ TFrmDictionaryPopup }

procedure TFrmDictionaryPopup.FormCreate(Sender: TObject);
begin
  ApplyFormStyle(Self);
  fCompStyle := csHintWindow;
  Canvas.Brush.Style := bsClear;
  //BorderStyle := bsNone; // bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin; // bsNone;
  Caption := '';
  FStrongs := TStringList.Create;
  FMorphologies := TStringList.Create;
  FInSizeMove := False;
  FPositioning := True;
  try
    FHoveringSyntagm := False;
    FCtrlEnteredPopup := False;
    FLastAnchorPoint := Point(0, 0);
    FAutoHideTimer := TTimer.Create(Self);
    FAutoHideTimer.Enabled := False;
    FAutoHideTimer.Interval := 20;
    FAutoHideTimer.OnTimer := @AutoHideTimerTick;

    FScrollbarPaintTicks := 0;
    FScrollbarPaintTimer := TTimer.Create(Self);
    FScrollbarPaintTimer.Enabled := False;
    FScrollbarPaintTimer.Interval := 40;
    FScrollbarPaintTimer.OnTimer := @ScrollbarPaintTimerTick;

    RichMemoStrong.ZoomFactor := opts.ReadInteger('leiaute', 'popup.zoom.strong', 10) / 10.0;
    RichMemoMorphology.ZoomFactor := opts.ReadInteger('leiaute', 'popup.zoom.morfo',  10) / 10.0;
    FPopupWidth  := Max(180, opts.ReadInteger('leiaute', 'popup.largura', 425));
    FPopupHeight := Max(140, opts.ReadInteger('leiaute', 'popup.altura', 418));
    SetBounds(Left, Top, FPopupWidth, FPopupHeight);
  finally
    FPositioning := False;
  end;
  FMorphologyPaneHeight := opts.ReadInteger('leiaute', 'popup.morfo.altura',
    opts.ReadInteger('leiaute', 'popup.panel2.altura', TabControlMorphologies.Height));
  FMorphologyPaneHeight := EnsureRange(FMorphologyPaneHeight, 48, Max(60, ClientHeight - 60));
  DisableAlign;
  try
    TabControlMorphologies.Height := FMorphologyPaneHeight;
    TabControlMorphologies.Top := ClientHeight - TabControlMorphologies.Height;
    Splitter1.Top := TabControlMorphologies.Top - Splitter1.Height;
  finally
    EnableAlign;
  end;
  Splitter1.OnMoved := @Splitter1Moved;
end;

procedure TFrmDictionaryPopup.FormDestroy(Sender: TObject);
begin
  SaveLayoutToOpts;

  FStrongs.Destroy;
  FMorphologies.Destroy;
end;

procedure TFrmDictionaryPopup.FormHide(Sender: TObject);
begin
  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := False;
  if Assigned(FScrollbarPaintTimer) then
    FScrollbarPaintTimer.Enabled := False;
  FInSizeMove := False;
  FHoveringSyntagm := False;
  FCtrlEnteredPopup := False;
  FStrongs.Clear;
  FMorphologies.Clear;
  TabControlStrongs.Tabs.Clear;
  TabControlMorphologies.Tabs.Clear;
end;

procedure TFrmDictionaryPopup.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Visible := false;
    MainForm.SetFocus;
  end;
end;

procedure TFrmDictionaryPopup.RichMemoMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    TRichMemo(Sender).ZoomFactor := Max(TRichMemo(Sender).ZoomFactor + Sign(WheelDelta)*0.1, 0.1);
    SaveLayoutToOpts;
    Handled := true;
  end;
end;

procedure TFrmDictionaryPopup.TabControlMorphologiesChange(Sender: TObject);
begin
  ShowRtf(RichMemoMorphology, FMorphologies[TabControlMorphologies.TabIndex]);
end;

procedure TFrmDictionaryPopup.TabControlStrongsChange(Sender: TObject);
begin
  ShowRtf(RichMemoStrong, FStrongs[TabControlStrongs.TabIndex]);
end;

procedure TFrmDictionaryPopup.AddStrong(const strong: string; const definition: string);
begin
  FStrongs.Add(definition);
  TabControlStrongs.Tabs.Add(strong);
end;

procedure TFrmDictionaryPopup.AddMorphology(const morfo: string; const definition: string);
begin
  FMorphologies.Add(definition);
  TabControlMorphologies.Tabs.Add(morfo);
end;

procedure TFrmDictionaryPopup.WndProc(var TheMessage: TLMessage);
begin
  if TheMessage.msg = $0231 {WM_ENTERSIZEMOVE} then
  begin
    FInSizeMove := True;
    if Assigned(FAutoHideTimer) then
      FAutoHideTimer.Enabled := False;
  end;
  inherited WndProc(TheMessage);
  if TheMessage.msg = $0232 {WM_EXITSIZEMOVE} then
  begin
    FInSizeMove := False;
    ClampToMonitorWorkarea(Mouse.CursorPos);
    SaveLayoutToOpts;
  end;
end;

procedure TFrmDictionaryPopup.DoOnResize;
begin
  inherited DoOnResize;
  if Visible and not FPositioning then
  begin
    if Width >= 180 then
      FPopupWidth := Width;
    if Height >= 140 then
      FPopupHeight := Height;
    if not FInSizeMove then
      ClampToMonitorWorkarea(FLastAnchorPoint);
    SaveLayoutToOpts;
  end;
end;

function TFrmDictionaryPopup.IsSizingOrMoving: Boolean;
begin
  Result := FInSizeMove;
  {$IFDEF WINDOWS}
  if not Result and Visible and ((GetKeyState(VK_LBUTTON) and $8000) <> 0) then
  begin
    if (GetCapture = Handle) or (GetForegroundWindow = Handle) or
       PtInRect( Classes.Rect(Left - 20, Top - 20, Left + Width + 20, Top + Height + 20), Mouse.CursorPos) then
      Result := True;
  end;
  {$ENDIF}
end;

function TFrmDictionaryPopup.IsCtrlKeyHeld: Boolean;
begin
  {$IFDEF WINDOWS}
  Result := ((GetKeyState(VK_CONTROL) and $8000) <> 0);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TFrmDictionaryPopup.IsCursorInsidePopupWindow: Boolean;
begin
  if not Visible then
    Exit(False);
  if IsSizingOrMoving then
    Exit(True);
  Result := PtInRect(BoundsRect, Mouse.CursorPos);
end;

function TFrmDictionaryPopup.IsPointerInteracting: Boolean;
begin
  Result := IsCursorInsidePopupWindow;
end;

procedure TFrmDictionaryPopup.NotifySyntagmHoverEnter;
begin
  FHoveringSyntagm := True;
  FCtrlEnteredPopup := False;
  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := False;
end;

procedure TFrmDictionaryPopup.RequestDeferredHide;
begin
  FHoveringSyntagm := False;
  if not Visible then
    Exit;
  if IsSizingOrMoving then
    Exit;
  if IsCtrlKeyHeld then
  begin
    if IsCursorInsidePopupWindow then
      FCtrlEnteredPopup := True;
    if Assigned(FAutoHideTimer) then
      FAutoHideTimer.Enabled := True;
    Exit;
  end;
  if FCtrlEnteredPopup and IsCursorInsidePopupWindow then
  begin
    if Assigned(FAutoHideTimer) then
      FAutoHideTimer.Enabled := True;
    Exit;
  end;
  FCtrlEnteredPopup := False;
  HidePopup;
end;

procedure TFrmDictionaryPopup.ScrollbarPaintTimerTick(Sender: TObject);
begin
  if not Visible then
  begin
    FScrollbarPaintTimer.Enabled := False;
    Exit;
  end;
  {$IFDEF WINDOWS}
  if IsThemedSkin then
  begin
    if RichMemoStrong.HandleAllocated then
    begin
      HookEditControl(RichMemoStrong);
      PaintThemedNCScrollBars(RichMemoStrong.Handle);
    end;
    if RichMemoMorphology.Visible and RichMemoMorphology.HandleAllocated then
    begin
      HookEditControl(RichMemoMorphology);
      PaintThemedNCScrollBars(RichMemoMorphology.Handle);
    end;
  end;
  {$ENDIF}
  Inc(FScrollbarPaintTicks);
  if (FScrollbarPaintTicks >= 15) and (not IsPointerInteracting) then
    FScrollbarPaintTimer.Enabled := False;
end;

procedure TFrmDictionaryPopup.AutoHideTimerTick(Sender: TObject);
begin
  if not Visible then
  begin
    FAutoHideTimer.Enabled := False;
    FCtrlEnteredPopup := False;
    Exit;
  end;
  if FHoveringSyntagm or IsSizingOrMoving then
    Exit;
  if IsCtrlKeyHeld then
  begin
    if IsCursorInsidePopupWindow then
      FCtrlEnteredPopup := True;
    Exit;
  end;
  if FCtrlEnteredPopup and IsCursorInsidePopupWindow then
    Exit;
  FCtrlEnteredPopup := False;
  FAutoHideTimer.Enabled := False;
  HidePopup;
end;

procedure TFrmDictionaryPopup.ClampToMonitorWorkarea(const RefPoint: TPoint);
var
  Mon: TMonitor;
  WorkRect: TRect;
  WorkW, WorkH, NewW, NewH, NewL, NewT: Integer;
begin
  if FPositioning then
    Exit;
  Mon := Screen.MonitorFromPoint(RefPoint);
  if Assigned(Mon) then
    WorkRect := Mon.WorkareaRect
  else
    WorkRect := Screen.WorkAreaRect;
  if (WorkRect.Right <= WorkRect.Left) or (WorkRect.Bottom <= WorkRect.Top) then
    WorkRect := Classes.Rect(0, 0, Screen.Width, Screen.Height);

  WorkW := Max(180, WorkRect.Right - WorkRect.Left);
  WorkH := Max(140, WorkRect.Bottom - WorkRect.Top);
  NewW := EnsureRange(Width, 180, WorkW);
  NewH := EnsureRange(Height, 140, WorkH);
  NewL := EnsureRange(Left, WorkRect.Left, Max(WorkRect.Left, WorkRect.Right - NewW));
  NewT := EnsureRange(Top, WorkRect.Top, Max(WorkRect.Top, WorkRect.Bottom - NewH));

  if (NewL <> Left) or (NewT <> Top) or (NewW <> Width) or (NewH <> Height) then
  begin
    FPositioning := True;
    try
      SetBounds(NewL, NewT, NewW, NewH);
    finally
      FPositioning := False;
    end;
  end;
end;

procedure TFrmDictionaryPopup.ShowAt(x: Integer; y: Integer);
type
  TPopupDirection = (pdNE, pdSE, pdN, pdS, pdSW, pdNW);
const
  PointerGap = 16;
var
  CursorPt: TPoint;
  Mon: TMonitor;
  WorkRect, CandRect, PointerRect, OverlapRect: TRect;
  WorkW, WorkH, PopupW, PopupH: Integer;
  px, py: Integer;
  EastW, WestW, NorthH, SouthH, FullW: Integer;
  Dir: TPopupDirection;
  AvailW, AvailH, CandL, CandT: Integer;
  ClearsPointer, IsDiagonal, BestClearsPointer, BestIsDiagonal: Boolean;
  FitArea, TotalArea, BestFitArea, BestTotalArea: Int64;
  BestL, BestT: Integer;
begin
  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := False;
  FHoveringSyntagm := True;
  FCtrlEnteredPopup := False;

  if FStrongs.Count > 0 then
  begin
    TabControlStrongs.TabIndex := 0;
    ShowRtf(RichMemoStrong, FStrongs[0]);
    RichMemoStrong.ZoomFactor  := RichMemoStrong.ZoomFactor; { circunventing bug... }
  end;
  if FMorphologies.Count > 0 then
  begin
    DisableAlign;
    try
      TabControlMorphologies.Visible := true;
      Splitter1.Visible := true;
      TabControlMorphologies.Height := EnsureRange(FMorphologyPaneHeight, 48, Max(60, ClientHeight - 60));
      TabControlMorphologies.Top := ClientHeight - TabControlMorphologies.Height;
      Splitter1.Top := TabControlMorphologies.Top - Splitter1.Height;
    finally
      EnableAlign;
    end;
    TabControlMorphologies.TabIndex := 0;
    ShowRtf(RichMemoMorphology, FMorphologies[0]);
    RichMemoMorphology.ZoomFactor := RichMemoMorphology.ZoomFactor; { circunventing bug... }
  end else
  begin
    Splitter1.Visible := false;
    TabControlMorphologies.Visible := false;
  end;

  CursorPt := Mouse.CursorPos;
  if (CursorPt.X = 0) and (CursorPt.Y = 0) and ((x <> 0) or (y <> 0)) then
    CursorPt := Point(x, y)
  else if (x <> 0) or (y <> 0) then
  begin
    // Use actual mouse pointer when close, or passed point
    if (Abs(CursorPt.X - x) > 400) or (Abs(CursorPt.Y - y) > 400) then
      CursorPt := Mouse.CursorPos;
  end;
  FLastAnchorPoint := CursorPt;

  Mon := Screen.MonitorFromPoint(CursorPt);
  if Assigned(Mon) then
    WorkRect := Mon.WorkareaRect
  else
    WorkRect := Screen.WorkAreaRect;
  if (WorkRect.Right <= WorkRect.Left) or (WorkRect.Bottom <= WorkRect.Top) then
    WorkRect := Classes.Rect(0, 0, Screen.Width, Screen.Height);

  WorkW := Max(180, WorkRect.Right - WorkRect.Left);
  WorkH := Max(140, WorkRect.Bottom - WorkRect.Top);
  if FPopupWidth < 180 then
    FPopupWidth := Max(180, Width);
  if FPopupHeight < 140 then
    FPopupHeight := Max(140, Height);
  PopupW := EnsureRange(FPopupWidth, 180, WorkW);
  PopupH := EnsureRange(FPopupHeight, 140, WorkH);

  px := EnsureRange(CursorPt.X, WorkRect.Left, WorkRect.Right);
  py := EnsureRange(CursorPt.Y, WorkRect.Top, WorkRect.Bottom);

  EastW  := Max(0, WorkRect.Right - (px + PointerGap));
  WestW  := Max(0, (px - PointerGap) - WorkRect.Left);
  NorthH := Max(0, (py - PointerGap) - WorkRect.Top);
  SouthH := Max(0, WorkRect.Bottom - (py + PointerGap));
  FullW  := WorkW;

  PointerRect := Classes.Rect(px - PointerGap + 2, py - PointerGap + 2,
    px + PointerGap - 2, py + PointerGap - 2);

  BestClearsPointer := False;
  BestIsDiagonal := False;
  BestFitArea := -1;
  BestTotalArea := -1;
  BestL := EnsureRange(px + PointerGap, WorkRect.Left, Max(WorkRect.Left, WorkRect.Right - PopupW));
  BestT := EnsureRange(py + PointerGap, WorkRect.Top, Max(WorkRect.Top, WorkRect.Bottom - PopupH));

  for Dir := Low(TPopupDirection) to High(TPopupDirection) do
  begin
    case Dir of
      pdNE:
        begin
          AvailW := EastW;
          AvailH := NorthH;
          CandL  := px + PointerGap;
          CandT  := (py - PointerGap) - PopupH;
          IsDiagonal := True;
        end;
      pdSE:
        begin
          AvailW := EastW;
          AvailH := SouthH;
          CandL  := px + PointerGap;
          CandT  := py + PointerGap;
          IsDiagonal := True;
        end;
      pdN:
        begin
          AvailW := FullW;
          AvailH := NorthH;
          CandL  := px - (PopupW div 2);
          CandT  := (py - PointerGap) - PopupH;
          IsDiagonal := False;
        end;
      pdS:
        begin
          AvailW := FullW;
          AvailH := SouthH;
          CandL  := px - (PopupW div 2);
          CandT  := py + PointerGap;
          IsDiagonal := False;
        end;
      pdSW:
        begin
          AvailW := WestW;
          AvailH := SouthH;
          CandL  := (px - PointerGap) - PopupW;
          CandT  := py + PointerGap;
          IsDiagonal := True;
        end;
      pdNW:
        begin
          AvailW := WestW;
          AvailH := NorthH;
          CandL  := (px - PointerGap) - PopupW;
          CandT  := (py - PointerGap) - PopupH;
          IsDiagonal := True;
        end;
    end;

    CandL := EnsureRange(CandL, WorkRect.Left, Max(WorkRect.Left, WorkRect.Right - PopupW));
    CandT := EnsureRange(CandT, WorkRect.Top, Max(WorkRect.Top, WorkRect.Bottom - PopupH));
    CandRect := Classes.Rect(CandL, CandT, CandL + PopupW, CandT + PopupH);
    ClearsPointer := not IntersectRect(OverlapRect, CandRect, PointerRect);

    // If clamping caused CandRect to overlap PointerRect, try shifting horizontally or vertically if space allows
    if not ClearsPointer then
    begin
      if (Dir in [pdNE, pdSE]) and (px + PointerGap + PopupW <= WorkRect.Right) then
        CandL := px + PointerGap
      else if (Dir in [pdSW, pdNW]) and (px - PointerGap - PopupW >= WorkRect.Left) then
        CandL := px - PointerGap - PopupW
      else if (Dir in [pdNE, pdN, pdNW]) and (py - PointerGap - PopupH >= WorkRect.Top) then
        CandT := py - PointerGap - PopupH
      else if (Dir in [pdSE, pdS, pdSW]) and (py + PointerGap + PopupH <= WorkRect.Bottom) then
        CandT := py + PointerGap;
      CandRect := Classes.Rect(CandL, CandT, CandL + PopupW, CandT + PopupH);
      ClearsPointer := not IntersectRect(OverlapRect, CandRect, PointerRect);
    end;

    FitArea := Int64(Min(PopupW, AvailW)) * Int64(Min(PopupH, AvailH));
    TotalArea := Int64(AvailW) * Int64(AvailH);

    if (BestFitArea < 0) or
       (ClearsPointer and not BestClearsPointer) or
       ((ClearsPointer = BestClearsPointer) and (FitArea > BestFitArea)) or
       ((ClearsPointer = BestClearsPointer) and (FitArea = BestFitArea) and
        ((IsDiagonal and not BestIsDiagonal) or
         ((IsDiagonal = BestIsDiagonal) and (TotalArea > BestTotalArea)))) then
    begin
      BestClearsPointer := ClearsPointer;
      BestIsDiagonal := IsDiagonal;
      BestFitArea := FitArea;
      BestTotalArea := TotalArea;
      BestL := CandL;
      BestT := CandT;
    end;
  end;

  // Final safety: if the popup is so large in both dimensions that even the best clamped
  // rectangle still overlaps the pointer, push it to the side/vertical edge with the most space
  CandRect := Classes.Rect(BestL, BestT, BestL + PopupW, BestT + PopupH);
  if IntersectRect(OverlapRect, CandRect, PointerRect) then
  begin
    if Max(NorthH, SouthH) >= Max(EastW, WestW) then
    begin
      if SouthH >= NorthH then
        BestT := Min(py + PointerGap, Max(WorkRect.Top, WorkRect.Bottom - 80))
      else
        BestT := Max(WorkRect.Top, (py - PointerGap) - PopupH);
    end
    else
    begin
      if EastW >= WestW then
        BestL := Min(px + PointerGap, Max(WorkRect.Left, WorkRect.Right - 100))
      else
        BestL := Max(WorkRect.Left, (px - PointerGap) - PopupW);
    end;
    BestL := EnsureRange(BestL, WorkRect.Left, Max(WorkRect.Left, WorkRect.Right - PopupW));
    BestT := EnsureRange(BestT, WorkRect.Top, Max(WorkRect.Top, WorkRect.Bottom - PopupH));
  end;

  FPositioning := True;
  try
    SetBounds(BestL, BestT, PopupW, PopupH);
    if FMorphologies.Count > 0 then
    begin
      DisableAlign;
      try
        TabControlMorphologies.Height := EnsureRange(FMorphologyPaneHeight, 48, Max(60, ClientHeight - 60));
        TabControlMorphologies.Top := ClientHeight - TabControlMorphologies.Height;
        Splitter1.Top := TabControlMorphologies.Top - Splitter1.Height;
      finally
        EnableAlign;
      end;
    end;
  finally
    FPositioning := False;
  end;

  Visible := True;
  ApplySkin;
  if Assigned(FScrollbarPaintTimer) then
  begin
    FScrollbarPaintTicks := 0;
    FScrollbarPaintTimer.Enabled := True;
  end;
end;

procedure TFrmDictionaryPopup.HidePopup;
begin
  if Assigned(FAutoHideTimer) then
    FAutoHideTimer.Enabled := False;
  FHoveringSyntagm := False;
  FCtrlEnteredPopup := False;
  if not Visible then
    exit;
  Visible := false;
  FormHide(self);
  TabControlStrongs.Tabs.Clear;
  TabControlMorphologies.Tabs.Clear;
  RichMemoStrong.Rtf := '';
  RichMemoMorphology.Rtf := '';
end;

procedure TFrmDictionaryPopup.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
  if Assigned(opts) then
  begin
    FPopupWidth  := Max(180, opts.ReadInteger('leiaute', 'popup.largura', FPopupWidth));
    FPopupHeight := Max(140, opts.ReadInteger('leiaute', 'popup.altura', FPopupHeight));
    FPositioning := True;
    try
      SetBounds(Left, Top, FPopupWidth, FPopupHeight);
    finally
      FPositioning := False;
    end;
  end;
end;


initialization
{$IFNDEF FPCUNIT}
  {$R FormDictionaryPopup.lfm}
{$ENDIF}

end.

