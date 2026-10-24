unit formpopup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  ExtCtrls, ComCtrls, LCLType, RichMemo, Types, Math, unitskins;

type
  { TFrmDictionaryPopup }

  TFrmDictionaryPopup = class(TForm)
    HeaderMorfos: TPanel;
    HeaderStrongs: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    RichMemoStrong: TRichMemo;
    RichMemoMorpho: TRichMemo;
    Splitter1: TSplitter;
    TabControlStrongs: TPanel;
    TabControlMorfos: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HeaderMorfosMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HeaderMorfosPaint(Sender: TObject);
    procedure HeaderStrongsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HeaderStrongsPaint(Sender: TObject);
    procedure RichMemoMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TabControlMorfosChange(Sender: TObject);
    procedure TabControlStrongsChange(Sender: TObject);
    procedure ApplySkin(const ASkin: TSkinColors);
  private
    { private declarations }
    FStrongs: TStringList;
    FMorfos: TStringList;
    FTabsStrongs: TStringList;
    FTabsMorfos: TStringList;
    FTabIndexStrongs: Integer;
    FTabIndexMorfos: Integer;
    FZoomStrong: Double;
    FZoomMorpho: Double;
    FMorfoHeight: Integer;
    procedure Splitter1Moved(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ApplyZoom;
    procedure DrawTabs(ACanvas: TCanvas; AWidth, AHeight: Integer;
      ATabs: TStrings; AActiveIndex: Integer);
  public
    { public declarations }
    procedure AdicionarStrong(const strong: string; const definition: string);
    procedure AdicionarMorfo(const morfo: string; const definition: string);
    procedure MostrarEm(x: Integer; y: Integer);
    procedure Ocultar;
    procedure LoadSettings;
    procedure SaveSettings;
    property Strongs: TStringList read FStrongs;
    property Morfos: TStringList read FMorfos;
    property TabsStrongs: TStringList read FTabsStrongs;
    property TabsMorfos: TStringList read FTabsMorfos;
    property TabIndexStrongs: Integer read FTabIndexStrongs;
    property TabIndexMorfos: Integer read FTabIndexMorfos;
  end;

var
  FrmDictionaryPopup: TFrmDictionaryPopup;

implementation

uses formPrincipal;

{ TFrmDictionaryPopup }

function AdaptRtfForSkin(const ARtf: string; const ASkin: TSkinColors): string;
var
  lightColorStr: string;
  sb: TStringBuilder;
  i, len: Integer;
begin
  Result := ARtf;
  if (Result = '') or not ASkin.IsDark then exit;

  lightColorStr := '\red240\green240\blue240;';

  // 1. Color table transformations
  if Pos('{\colortbl ;', Result) > 0 then
    Result := StringReplace(Result, '{\colortbl ;', '{\colortbl ' + lightColorStr, [rfReplaceAll])
  else if Pos('{\colortbl;', Result) > 0 then
    Result := StringReplace(Result, '{\colortbl;', '{\colortbl ' + lightColorStr, [rfReplaceAll])
  else if Pos('{\colortbl', Result) > 0 then
    Result := StringReplace(Result, '{\colortbl', '{\colortbl ' + lightColorStr, [rfReplaceAll]);

  Result := StringReplace(Result, '\red0\green0\blue0;', lightColorStr, [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green0\blue255;', '\red115\green175\blue255;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green0\blue128;', '\red135\green195\blue255;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red128\green0\blue0;', '\red255\green135\blue135;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red150\green60\blue100;', '\red245\green145\blue195;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red0\green128\blue0;', '\red125\green225\blue145;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red128\green128\blue0;', '\red235\green235\blue135;', [rfReplaceAll]);
  Result := StringReplace(Result, '\red128\green128\blue128;', '\red185\green195\blue205;', [rfReplaceAll]);

  // 2. Invert any resets to color 0 (\cf0) to light text color 1 (\cf1)
  Result := StringReplace(Result, '\cf0', '\cf1', [rfReplaceAll]);

  // 3. Word-boundary scanner to ensure every \pard, \plain, \par has \cf1
  sb := TStringBuilder.Create;
  try
    len := Length(Result);
    i := 1;
    while i <= len do
    begin
      if (i <= len - 4) and (Copy(Result, i, 5) = '\pard') and
         ((i + 5 > len) or not (Result[i + 5] in ['a'..'z', 'A'..'Z'])) then
      begin
        sb.Append('\pard\cf1 ');
        Inc(i, 5);
      end
      else if (i <= len - 5) and (Copy(Result, i, 6) = '\plain') and
         ((i + 6 > len) or not (Result[i + 6] in ['a'..'z', 'A'..'Z'])) then
      begin
        sb.Append('\plain\cf1 ');
        Inc(i, 6);
      end
      else if (i <= len - 3) and (Copy(Result, i, 4) = '\par') and
         ((i + 4 > len) or not (Result[i + 4] in ['a'..'z', 'A'..'Z'])) then
      begin
        sb.Append('\par\cf1 ');
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

  Result := StringReplace(Result, '\cf1 \cf1 ', '\cf1 ', [rfReplaceAll]);
end;

procedure TFrmDictionaryPopup.DrawTabs(ACanvas: TCanvas; AWidth, AHeight: Integer;
  ATabs: TStrings; AActiveIndex: Integer);
var
  i, curX, tabW, textW, textH, textX, textY: Integer;
  tabTitle: string;
  tabRect, fillR, accentR: TRect;
  actBg, actFg, inactBg, inactFg, borderCol, accentCol: TColor;
begin
  if not Assigned(ACanvas) then exit;

  if CurrentSkin.IsDark then
  begin
    actBg     := CurrentSkin.PointedBg;     // Accent Blue
    actFg     := CurrentSkin.PointedText;   // Bright White
    accentCol := CurrentSkin.HighlightBg;   // Highlight Accent Bar
    inactBg   := CurrentSkin.PanelBg;       // Charcoal
    inactFg   := CurrentSkin.PanelText;     // Silver
    borderCol := CurrentSkin.PanelBorder;   // Slate
  end
  else
  begin
    actBg     := clWhite;
    actFg     := clBlack;
    accentCol := clHighlight;
    inactBg   := clBtnFace;
    inactFg   := clWindowText;
    borderCol := MakeRGB(180, 180, 180);
  end;

  // 1. Fill entire header background
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := inactBg;
  ACanvas.FillRect(0, 0, AWidth, AHeight);

  // 2. Continuous horizontal line across bottom of header strip
  ACanvas.Pen.Color := borderCol;
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Line(0, AHeight - 1, AWidth, AHeight - 1);

  if not Assigned(ATabs) or (ATabs.Count = 0) then exit;

  curX := 4;
  ACanvas.Font.Assign(Self.Font);
  ACanvas.Font.Size := 9;

  for i := 0 to ATabs.Count - 1 do
  begin
    tabTitle := ATabs[i];
    ACanvas.Font.Bold := (i = AActiveIndex);
    textW := ACanvas.TextWidth(tabTitle);
    textH := ACanvas.TextHeight(tabTitle);
    tabW := Max(64, textW + 20);

    tabRect := Rect(curX, 2, curX + tabW, AHeight);

    if i = AActiveIndex then
    begin
      // Active tab: filled with actBg
      fillR := tabRect;
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := actBg;
      ACanvas.FillRect(fillR);

      // Top 2px accent highlight
      accentR := Rect(curX, 2, curX + tabW, 4);
      ACanvas.Brush.Color := accentCol;
      ACanvas.FillRect(accentR);

      // Tab border (Left, Top, Right)
      ACanvas.Pen.Color := borderCol;
      ACanvas.Line(curX, 2, curX, AHeight);
      ACanvas.Line(curX, 2, curX + tabW - 1, 2);
      ACanvas.Line(curX + tabW - 1, 2, curX + tabW - 1, AHeight);

      // Erase bottom border line under active tab to merge with client area
      ACanvas.Pen.Color := actBg;
      ACanvas.Line(curX + 1, AHeight - 1, curX + tabW - 1, AHeight - 1);

      // Tab text
      ACanvas.Font.Color := actFg;
      textX := curX + (tabW - textW) div 2;
      textY := 2 + (AHeight - 2 - textH) div 2;
      ACanvas.Brush.Style := bsClear;
      ACanvas.TextOut(textX, textY, tabTitle);
    end
    else
    begin
      // Inactive tab: filled with inactBg
      fillR := Rect(curX, 4, curX + tabW, AHeight - 1);
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := inactBg;
      ACanvas.FillRect(fillR);

      // Tab border (Left, Top, Right)
      ACanvas.Pen.Color := borderCol;
      ACanvas.Line(curX, 4, curX, AHeight - 1);
      ACanvas.Line(curX, 4, curX + tabW - 1, 4);
      ACanvas.Line(curX + tabW - 1, 4, curX + tabW - 1, AHeight - 1);

      // Tab text
      ACanvas.Font.Color := inactFg;
      textX := curX + (tabW - textW) div 2;
      textY := 4 + (AHeight - 5 - textH) div 2;
      ACanvas.Brush.Style := bsClear;
      ACanvas.TextOut(textX, textY, tabTitle);
    end;

    curX := curX + tabW + 2;
  end;
end;

procedure TFrmDictionaryPopup.HeaderStrongsPaint(Sender: TObject);
begin
  DrawTabs(HeaderStrongs.Canvas, HeaderStrongs.Width, HeaderStrongs.Height,
    FTabsStrongs, FTabIndexStrongs);
end;

procedure TFrmDictionaryPopup.HeaderMorfosPaint(Sender: TObject);
begin
  DrawTabs(HeaderMorfos.Canvas, HeaderMorfos.Width, HeaderMorfos.Height,
    FTabsMorfos, FTabIndexMorfos);
end;

procedure TFrmDictionaryPopup.HeaderStrongsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, curX, tabW, textW: Integer;
begin
  if (Button <> mbLeft) or not Assigned(FTabsStrongs) or (FTabsStrongs.Count = 0) then exit;
  HeaderStrongs.Canvas.Font.Assign(Self.Font);
  HeaderStrongs.Canvas.Font.Size := 9;
  curX := 4;
  for i := 0 to FTabsStrongs.Count - 1 do
  begin
    HeaderStrongs.Canvas.Font.Bold := (i = FTabIndexStrongs);
    textW := HeaderStrongs.Canvas.TextWidth(FTabsStrongs[i]);
    tabW := Max(64, textW + 20);
    if (X >= curX) and (X < curX + tabW) then
    begin
      if FTabIndexStrongs <> i then
      begin
        FTabIndexStrongs := i;
        HeaderStrongs.Invalidate;
        TabControlStrongsChange(Self);
      end;
      Break;
    end;
    curX := curX + tabW + 2;
  end;
end;

procedure TFrmDictionaryPopup.HeaderMorfosMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, curX, tabW, textW: Integer;
begin
  if (Button <> mbLeft) or not Assigned(FTabsMorfos) or (FTabsMorfos.Count = 0) then exit;
  HeaderMorfos.Canvas.Font.Assign(Self.Font);
  HeaderMorfos.Canvas.Font.Size := 9;
  curX := 4;
  for i := 0 to FTabsMorfos.Count - 1 do
  begin
    HeaderMorfos.Canvas.Font.Bold := (i = FTabIndexMorfos);
    textW := HeaderMorfos.Canvas.TextWidth(FTabsMorfos[i]);
    tabW := Max(64, textW + 20);
    if (X >= curX) and (X < curX + tabW) then
    begin
      if FTabIndexMorfos <> i then
      begin
        FTabIndexMorfos := i;
        HeaderMorfos.Invalidate;
        TabControlMorfosChange(Self);
      end;
      Break;
    end;
    curX := curX + tabW + 2;
  end;
end;

procedure TFrmDictionaryPopup.ApplySkin(const ASkin: TSkinColors);
begin
  Self.Color := ASkin.PanelBg;

  TabControlStrongs.ParentColor := False;
  TabControlMorfos.ParentColor := False;
  TabControlStrongs.Color := ASkin.PanelBg;
  TabControlMorfos.Color := ASkin.PanelBg;

  HeaderStrongs.ParentColor := False;
  HeaderMorfos.ParentColor := False;
  HeaderStrongs.Color := ASkin.PanelBg;
  HeaderMorfos.Color := ASkin.PanelBg;

  Panel1.ParentColor := False;
  Panel2.ParentColor := False;
  Panel1.Color := ASkin.NoteBg;
  Panel2.Color := ASkin.NoteBg;

  RichMemoStrong.ParentColor := False;
  RichMemoStrong.ParentFont := False;
  RichMemoStrong.Color := ASkin.NoteBg;
  RichMemoStrong.Font.Color := ASkin.NoteText;

  RichMemoMorpho.ParentColor := False;
  RichMemoMorpho.ParentFont := False;
  RichMemoMorpho.Color := ASkin.NoteBg;
  RichMemoMorpho.Font.Color := ASkin.NoteText;

  Splitter1.Beveled := False;
  Splitter1.Color := ASkin.PanelBorder;

  ApplyDarkModeToControl(RichMemoStrong, ASkin.IsDark);
  ApplyDarkModeToControl(RichMemoMorpho, ASkin.IsDark);
  ApplyDarkModeToWindow(Self, ASkin.IsDark);

  HeaderStrongs.Invalidate;
  HeaderMorfos.Invalidate;
end;

procedure TFrmDictionaryPopup.FormCreate(Sender: TObject);
begin
  fCompStyle := csHintWindow;
  Canvas.Brush.Style := bsClear;
  Caption := '';

  FStrongs := TStringList.Create;
  FMorfos  := TStringList.Create;
  FTabsStrongs := TStringList.Create;
  FTabsMorfos  := TStringList.Create;
  FTabIndexStrongs := 0;
  FTabIndexMorfos  := 0;

  FZoomStrong := 1.0;
  FZoomMorpho := 1.0;
  FMorfoHeight := 150;

  Splitter1.OnMoved := @Splitter1Moved;
  OnResize := @FormResize;
  OnShow := @FormShow;

  ApplySkin(CurrentSkin);

  LoadSettings;
end;

procedure TFrmDictionaryPopup.FormShow(Sender: TObject);
begin
  ApplySkin(CurrentSkin);
  if Assigned(HeaderStrongs) then
    HeaderStrongs.Invalidate;
  if Assigned(HeaderMorfos) then
    HeaderMorfos.Invalidate;
end;

procedure TFrmDictionaryPopup.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  FStrongs.Free;
  FMorfos.Free;
  FTabsStrongs.Free;
  FTabsMorfos.Free;
end;

procedure TFrmDictionaryPopup.FormHide(Sender: TObject);
begin
  SaveSettings;
  FStrongs.Clear;
  FMorfos.Clear;
  FTabsStrongs.Clear;
  FTabsMorfos.Clear;
  FTabIndexStrongs := 0;
  FTabIndexMorfos := 0;
end;

procedure TFrmDictionaryPopup.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Visible := false;
    FrmPrincipal.SetFocus;
  end;
end;

procedure TFrmDictionaryPopup.Splitter1Moved(Sender: TObject);
begin
  if TabControlMorfos.Visible and (TabControlMorfos.Height > 30) and (TabControlMorfos.Height < Height - 50) then
    FMorfoHeight := TabControlMorfos.Height;
  SaveSettings;
end;

procedure TFrmDictionaryPopup.FormResize(Sender: TObject);
begin
  if Visible and (Width >= 150) and (Height >= 150) then
  begin
    if TabControlMorfos.Visible and (TabControlMorfos.Height > 30) and (TabControlMorfos.Height < Height - 50) then
      FMorfoHeight := TabControlMorfos.Height;
    SaveSettings;
  end;
end;

procedure TFrmDictionaryPopup.ApplyZoom;
begin
  try
    RichMemoStrong.ZoomFactor := FZoomStrong;
  except
  end;
  try
    RichMemoMorpho.ZoomFactor := FZoomMorpho;
  except
  end;
end;

procedure TFrmDictionaryPopup.LoadSettings;
var
  w, h, morfoH, zoomS, zoomM: Integer;
begin
  if not Assigned(opts) then exit;
  try
    w := opts.ReadInteger('leiaute', 'popup.largura', 425);
    h := opts.ReadInteger('leiaute', 'popup.altura', 418);
    morfoH := opts.ReadInteger('leiaute', 'popup.morfo.altura', 0);
    if morfoH <= 0 then
      morfoH := opts.ReadInteger('leiaute', 'popup.panel2.altura', 150);

    zoomS := opts.ReadInteger('leiaute', 'popup.zoom.strong', 100);
    zoomM := opts.ReadInteger('leiaute', 'popup.zoom.morfo', 100);

    if w >= 150 then Width := w;
    if h >= 150 then Height := h;

    if morfoH >= 30 then
      FMorfoHeight := morfoH
    else
      FMorfoHeight := 150;

    TabControlMorfos.Height := FMorfoHeight;

    if (zoomS > 0) and (zoomS <= 30) then
      FZoomStrong := zoomS / 10.0
    else if zoomS > 30 then
      FZoomStrong := zoomS / 100.0
    else
      FZoomStrong := 1.0;

    if (zoomM > 0) and (zoomM <= 30) then
      FZoomMorpho := zoomM / 10.0
    else if zoomM > 30 then
      FZoomMorpho := zoomM / 100.0
    else
      FZoomMorpho := 1.0;

    FZoomStrong := Max(0.2, Min(5.0, FZoomStrong));
    FZoomMorpho := Max(0.2, Min(5.0, FZoomMorpho));

    ApplyZoom;
  except
  end;
end;

procedure TFrmDictionaryPopup.SaveSettings;
begin
  if not Assigned(opts) then exit;
  try
    if (Width >= 150) and (Height >= 150) then
    begin
      opts.WriteInteger('leiaute', 'popup.largura', Width);
      opts.WriteInteger('leiaute', 'popup.altura', Height);
    end;

    if FMorfoHeight >= 30 then
    begin
      opts.WriteInteger('leiaute', 'popup.morfo.altura', FMorfoHeight);
      opts.WriteInteger('leiaute', 'popup.panel2.altura', FMorfoHeight);
    end;

    opts.WriteInteger('leiaute', 'popup.zoom.strong', Round(FZoomStrong * 100));
    opts.WriteInteger('leiaute', 'popup.zoom.morfo',  Round(FZoomMorpho * 100));
    opts.UpdateFile;
  except
  end;
end;

procedure TFrmDictionaryPopup.RichMemoMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    if Sender = RichMemoStrong then
    begin
      FZoomStrong := Max(0.2, Min(5.0, FZoomStrong + Sign(WheelDelta) * 0.1));
      RichMemoStrong.ZoomFactor := FZoomStrong;
      SaveSettings;
    end
    else if Sender = RichMemoMorpho then
    begin
      FZoomMorpho := Max(0.2, Min(5.0, FZoomMorpho + Sign(WheelDelta) * 0.1));
      RichMemoMorpho.ZoomFactor := FZoomMorpho;
      SaveSettings;
    end;
    Handled := true;
  end;
end;

procedure TFrmDictionaryPopup.TabControlMorfosChange(Sender: TObject);
begin
  if (FTabIndexMorfos >= 0) and (FTabIndexMorfos < FMorfos.Count) then
  begin
    RichMemoMorpho.Rtf := AdaptRtfForSkin(FMorfos[FTabIndexMorfos], CurrentSkin);
    RichMemoMorpho.ZoomFactor := FZoomMorpho;
  end;
end;

procedure TFrmDictionaryPopup.TabControlStrongsChange(Sender: TObject);
begin
  if (FTabIndexStrongs >= 0) and (FTabIndexStrongs < FStrongs.Count) then
  begin
    RichMemoStrong.Rtf := AdaptRtfForSkin(FStrongs[FTabIndexStrongs], CurrentSkin);
    RichMemoStrong.ZoomFactor := FZoomStrong;
  end;
end;

procedure TFrmDictionaryPopup.AdicionarStrong(const strong: string; const definition: string);
begin
  FStrongs.Add(AdaptRtfForSkin(definition, CurrentSkin));
  FTabsStrongs.Add(strong);
  if Assigned(HeaderStrongs) then
    HeaderStrongs.Invalidate;
end;

procedure TFrmDictionaryPopup.AdicionarMorfo(const morfo: string; const definition: string);
begin
  FMorfos.Add(AdaptRtfForSkin(definition, CurrentSkin));
  FTabsMorfos.Add(morfo);
  if Assigned(HeaderMorfos) then
    HeaderMorfos.Invalidate;
end;

procedure TFrmDictionaryPopup.MostrarEm(x: Integer; y: Integer);
begin
  ApplySkin(CurrentSkin);
  if FStrongs.Count > 0 then
  begin
    FTabIndexStrongs := 0;
    RichMemoStrong.Rtf         := AdaptRtfForSkin(FStrongs[0], CurrentSkin);
    RichMemoStrong.ZoomFactor  := FZoomStrong;
  end;
  if FMorfos.Count > 0 then
  begin
    if FMorfoHeight >= 30 then
      TabControlMorfos.Height  := FMorfoHeight;
    TabControlMorfos.Visible   := true;
    Splitter1.Visible          := true;
    FTabIndexMorfos := 0;
    RichMemoMorpho.Rtf         := AdaptRtfForSkin(FMorfos[0], CurrentSkin);
    RichMemoMorpho.ZoomFactor  := FZoomMorpho;
  end else
  begin
    Splitter1.Visible          := false;
    TabControlMorfos.Visible   := false;
  end;

  if x + Width > Screen.Width then
  begin
    Left := x - Width;
    if Left < 0 then Left := 0;
  end
  else
    Left := x;

  if y + Height > Screen.Height then
  begin
    Top := y - Height;
    if Top < 0 then Top := 0;
  end
  else
    Top := y;

  Visible := True;
  if Assigned(HeaderStrongs) then
    HeaderStrongs.Invalidate;
  if Assigned(HeaderMorfos) then
    HeaderMorfos.Invalidate;
end;

procedure TFrmDictionaryPopup.Ocultar;
begin
  if not Visible then
    exit;
  SaveSettings;
  Visible := false;
  FormHide(self);
  RichMemoStrong.Rtf := '';
  RichMemoMorpho.Rtf := '';
end;

initialization
  {$I formpopup.lrs}

end.
