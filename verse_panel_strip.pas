unit verse_panel_strip;

{ The 1.0.1 mod's in-verse controls, rebuilt for 1.1.0.

  TVersePanelStrip  the bar along the BOTTOM edge of a verse panel:
      centre  the status circles (destination panel only), one per status of
              the project; click one to set the verse status
      right   the Edit and Properties tick boxes (Edit = the visual editor,
              Properties = the module's properties in place of the text)
  TVerseChevrons    the "<" ">" pair that appears right after the end of the
      verse text while the pointer is over the panel (previous / next verse).

  The bar is a sibling of the panel (like the panel captions), so it never
  scrolls with the verse text; the verse keeps a matching bottom inset. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, Types, Math, Forms
  {$IFDEF LCLGTK2}, gtk2, gdk2{$ENDIF};

type
  TStripNavigateEvent = procedure(Sender: TObject; Delta: Integer) of object;
  TStripStatusEvent = procedure(Sender: TObject; DisplayIndex: Integer) of object;

  TStripHit = (shNone, shEdit, shProperties, shStatus);

  TVersePanelStrip = class(TCustomControl)
  private
    FTarget: TControl;
    FTitleCaption: string;
    FTitleHint: string;
    FShowControls: Boolean;
    FStatusColors: array of TColor;
    FStatusCaptions: array of string;
    FStatusIndex: Integer;
    FShowStatus: Boolean;
    FEditActive: Boolean;
    FPropertiesActive: Boolean;
    FHot: TStripHit;
    FHotStatus: Integer;
    FOnStatusPick: TStripStatusEvent;
    FOnEdit: TNotifyEvent;
    FOnProperties: TNotifyEvent;
    FPaintCanvas: TCanvas;
    FPaintBack: TColor;
    procedure PaintContent(ACanvas: TCanvas; ABack: TColor);
    procedure ApplyTransparency;
    procedure SetTitleCaption(const AValue: string);
    procedure SetShowControls(AValue: Boolean);
    procedure SetShowStatus(AValue: Boolean);
    procedure SetEditActive(AValue: Boolean);
    procedure SetPropertiesActive(AValue: Boolean);
    function HitTestAt(X, Y: Integer; out StatusIndex: Integer): TStripHit;
    function StatusCircleRect(Index: Integer): TRect;
    procedure DrawStatusCircle(const R: TRect; Index: Integer);
    function PropertiesRect: TRect;
    function EditRect: TRect;
    function BoxSize: Integer;
    function BackColor: TColor;
    function PC: TCanvas;
    procedure DrawBox(const R: TRect; const ALabel: string; Checked, Hot: Boolean);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    function PreferredHeight: Integer;
    { Puts the bar along the bottom edge of ATarget. }
    procedure PlaceOver(ATarget: TControl);
    procedure SetStatuses(const Colors: array of TColor;
      const Captions: array of string; CurrentIndex: Integer);
    property Target: TControl read FTarget write FTarget;
    property TitleCaption: string read FTitleCaption write SetTitleCaption;
    property TitleHint: string read FTitleHint write FTitleHint;
    property ShowControls: Boolean read FShowControls write SetShowControls;
    property ShowStatus: Boolean read FShowStatus write SetShowStatus;
    property EditActive: Boolean read FEditActive write SetEditActive;
    property PropertiesActive: Boolean read FPropertiesActive write SetPropertiesActive;
    property OnStatusPick: TStripStatusEvent read FOnStatusPick write FOnStatusPick;
    property OnEdit: TNotifyEvent read FOnEdit write FOnEdit;
    property OnProperties: TNotifyEvent read FOnProperties write FOnProperties;
  end;

  TVerseChevrons = class(TCustomControl)
  private
    FHot: Integer; // -1 none, 0 previous, 1 next
    FOnNavigate: TStripNavigateEvent;
    function SlotRect(Index: Integer): TRect;
    function SlotAt(X, Y: Integer): Integer;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    property OnNavigate: TStripNavigateEvent read FOnNavigate write FOnNavigate;
  end;

resourcestring
  SStripEdit = 'Edit';
  SStripProperties = 'Properties';
  SStripPreviousVerse = 'Previous verse';
  SStripNextVerse = 'Next verse';
  SStripEditHint = 'Edit this verse (same as double-clicking it)';
  SStripPropertiesHint = 'Show the properties of this text in place of the verse';

implementation

uses
  ui_style, unitskins;

constructor TVersePanelStrip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;
  FStatusIndex := -1;
  FHotStatus := -1;
  FHot := shNone;
  ShowHint := True;
  Height := 24;
end;

function TVersePanelStrip.PreferredHeight: Integer;
begin
  Canvas.Font.Assign(Font);
  Result := Max(22, Canvas.TextHeight('Hg') + 8);
end;

procedure TVersePanelStrip.PlaceOver(ATarget: TControl);
var
  H, NewL, NewT, NewW: Integer;
begin
  FTarget := ATarget;
  if not Assigned(ATarget) then
    Exit;
  H := PreferredHeight;
  NewL := ATarget.Left;
  NewT := ATarget.Top + ATarget.Height - H;
  NewW := ATarget.ClientWidth;
  if (Left <> NewL) or (Top <> NewT) or (Width <> NewW) or (Height <> H) then
  begin
    SetBounds(NewL, NewT, NewW, H);
    Invalidate;
    if HandleAllocated and Visible then
      Update;
  end;
end;

procedure TVersePanelStrip.SetTitleCaption(const AValue: string);
begin
  if FTitleCaption = AValue then
    Exit;
  FTitleCaption := AValue;
  Invalidate;
end;

procedure TVersePanelStrip.SetShowControls(AValue: Boolean);
begin
  if FShowControls = AValue then
    Exit;
  FShowControls := AValue;
  if not FShowControls then
  begin
    FHot := shNone;
    FHotStatus := -1;
  end;
  Invalidate;
end;

procedure TVersePanelStrip.SetStatuses(const Colors: array of TColor;
  const Captions: array of string; CurrentIndex: Integer);
var
  Index: Integer;
  IsDifferent: Boolean;
begin
  IsDifferent := (Length(FStatusColors) <> Length(Colors)) or
    (FStatusIndex <> CurrentIndex);
  if not IsDifferent then
    for Index := 0 to High(Colors) do
      if (FStatusColors[Index] <> Colors[Index]) or
         (FStatusCaptions[Index] <> Captions[Index]) then
      begin
        IsDifferent := True;
        Break;
      end;
  if not IsDifferent then
    Exit;
  SetLength(FStatusColors, Length(Colors));
  SetLength(FStatusCaptions, Length(Colors));
  for Index := 0 to High(Colors) do
  begin
    FStatusColors[Index] := Colors[Index];
    FStatusCaptions[Index] := Captions[Index];
  end;
  FStatusIndex := CurrentIndex;
  Invalidate;
end;

procedure TVersePanelStrip.SetShowStatus(AValue: Boolean);
begin
  if FShowStatus = AValue then
    Exit;
  FShowStatus := AValue;
  Invalidate;
end;

procedure TVersePanelStrip.SetEditActive(AValue: Boolean);
begin
  if FEditActive = AValue then
    Exit;
  FEditActive := AValue;
  Invalidate;
end;

procedure TVersePanelStrip.SetPropertiesActive(AValue: Boolean);
begin
  if FPropertiesActive = AValue then
    Exit;
  FPropertiesActive := AValue;
  Invalidate;
end;

type
  TControlColorAccess = class(TControl);

{ The bar has no background of its own: it shows what is behind it, which is
  the verse panel it sits on (the verse keeps a blank inset for the bar). }
function TVersePanelStrip.BackColor: TColor;
begin
  if Assigned(FPaintCanvas) then
    Exit(FPaintBack);
  Result := UIContentColor;
  if Assigned(FTarget) then
  begin
    Result := TControlColorAccess(FTarget).Color;
    if (Result = clDefault) or (Result = clNone) then
      Result := UIContentColor;
  end;
end;

function TVersePanelStrip.BoxSize: Integer;
begin
  Result := Max(11, Height - 12);
end;

{ The circles sit in the middle of the panel, as in the mod. }
function TVersePanelStrip.StatusCircleRect(Index: Integer): TRect;
var
  Size, Gap, GroupWidth, Left0: Integer;
begin
  Size := Max(10, Min(16, Height - 6));
  Gap := Max(3, Min(20, ((ClientWidth div 2 - Length(FStatusColors) * Size) div
    Max(1, Length(FStatusColors) * 3)) div 2));
  GroupWidth := Length(FStatusColors) * Size + Max(0, Length(FStatusColors) - 1) * Gap;
  Left0 := Max(4, ClientWidth div 2 - GroupWidth div 2);
  Result := Rect(Left0 + Index * (Size + Gap), (Height - Size) div 2,
    Left0 + Index * (Size + Gap) + Size, (Height - Size) div 2 + Size);
end;

function TVersePanelStrip.PropertiesRect: TRect;
var
  W: Integer;
begin
  Canvas.Font.Assign(Font);
  W := BoxSize + 6 + Canvas.TextWidth(SStripProperties) + 6;
  Result := Rect(ClientWidth - 4 - W, 0, ClientWidth - 4, Height);
end;

function TVersePanelStrip.EditRect: TRect;
var
  W: Integer;
  Props: TRect;
begin
  Props := PropertiesRect;
  Canvas.Font.Assign(Font);
  W := BoxSize + 6 + Canvas.TextWidth(SStripEdit) + 6;
  Result := Rect(Props.Left - 2 - W, 0, Props.Left - 2, Height);
end;

function TVersePanelStrip.HitTestAt(X, Y: Integer;
  out StatusIndex: Integer): TStripHit;
var
  Index: Integer;
  P: TPoint;
begin
  StatusIndex := -1;
  Result := shNone;
  if not FShowControls then
    Exit;
  P := Point(X, Y);
  if FShowStatus then
    for Index := 0 to High(FStatusColors) do
      if PtInRect(StatusCircleRect(Index), P) then
      begin
        StatusIndex := Index;
        Exit(shStatus);
      end;
  if PtInRect(PropertiesRect, P) then
    Exit(shProperties);
  if PtInRect(EditRect, P) then
    Exit(shEdit);
end;

function TVersePanelStrip.PC: TCanvas;
begin
  if Assigned(FPaintCanvas) then
    Result := FPaintCanvas
  else
    Result := Canvas;
end;

procedure TVersePanelStrip.DrawBox(const R: TRect; const ALabel: string;
  Checked, Hot: Boolean);
var
  Size, Y: Integer;
  Box: TRect;
begin
  Size := BoxSize;
  Y := (Height - Size) div 2;
  Box := Rect(R.Left + 2, Y, R.Left + 2 + Size, Y + Size);
  PC.Brush.Style := bsSolid;
  PC.Brush.Color := UIContentColor;
  if Hot then
    PC.Pen.Color := clHighlight
  else
    PC.Pen.Color := UIMutedTextColor;
  PC.Pen.Width := 1;
  PC.Rectangle(Box);
  if Checked then
  begin
    PC.Pen.Color := UITextColor;
    PC.Pen.Width := 2;
    PC.MoveTo(Box.Left + 3, Box.Top + Size div 2);
    PC.LineTo(Box.Left + Size div 2 - 1, Box.Bottom - 4);
    PC.LineTo(Box.Right - 3, Box.Top + 3);
    PC.Pen.Width := 1;
  end;
  PC.Brush.Style := bsClear;
  PC.Font.Color := UITextColor;
  PC.TextOut(Box.Right + 6, (Height - PC.TextHeight(ALabel)) div 2,
    ALabel);
end;

procedure TVersePanelStrip.DrawStatusCircle(const R: TRect; Index: Integer);
{ as in the mod: a fine anti-aliased ring; the current status also gets a dot
  in its colour }
  function Blend(C1, C2: TColor; T: Double): TColor;
  var
    A, B: LongInt;
  begin
    if T <= 0 then Exit(C1);
    if T >= 1 then Exit(C2);
    A := ColorToRGB(C1);
    B := ColorToRGB(C2);
    Result := RGBToColor(
      EnsureRange(Round((A and 255) + ((B and 255) - (A and 255)) * T), 0, 255),
      EnsureRange(Round(((A shr 8) and 255) + (((B shr 8) and 255) - ((A shr 8) and 255)) * T), 0, 255),
      EnsureRange(Round(((A shr 16) and 255) + (((B shr 16) and 255) - ((A shr 16) and 255)) * T), 0, 255));
  end;
var
  Bmp: TBitmap;
  W, H, PX, PY: Integer;
  Cx, Cy, D, ROuter, RMid, HRing, RDot, TFill, TRing, TDot: Double;
  BgCol, FillCol, DotCol, RingCol, PixCol: TColor;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  if (W < 4) or (H < 4) then
    Exit;
  BgCol := BackColor;
  FillCol := UIContentColor;
  DotCol := FStatusColors[Index];
  if DotCol = clNone then
    DotCol := UIMutedTextColor;
  if Index = FStatusIndex then
    RingCol := DotCol
  else if (FHot = shStatus) and (Index = FHotStatus) then
    RingCol := UITextColor
  else
    RingCol := UIMutedTextColor;
  ROuter := W / 2 - 0.8;
  RMid := ROuter - 0.75;
  HRing := 0.75;
  RDot := ROuter * 0.52;
  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(W, H);
    Cx := (W - 1) / 2;
    Cy := (H - 1) / 2;
    for PY := 0 to H - 1 do
      for PX := 0 to W - 1 do
      begin
        D := Sqrt(Sqr(PX - Cx) + Sqr(PY - Cy));
        PixCol := BgCol;
        TFill := EnsureRange((ROuter + 0.5) - D, 0.0, 1.0);
        if TFill > 0 then
          PixCol := Blend(PixCol, FillCol, TFill);
        TRing := EnsureRange((HRing + 0.55) - Abs(D - RMid), 0.0, 1.0);
        if TRing > 0 then
          PixCol := Blend(PixCol, RingCol, TRing);
        if Index = FStatusIndex then
        begin
          TDot := EnsureRange((RDot + 0.55) - D, 0.0, 1.0);
          if TDot > 0 then
            PixCol := Blend(PixCol, DotCol, TDot);
        end;
        Bmp.Canvas.Pixels[PX, PY] := PixCol;
      end;
    PC.Draw(R.Left, R.Top, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TVersePanelStrip.PaintContent(ACanvas: TCanvas; ABack: TColor);
var
  Index, CaptionIndex, CaptionLeft, MaxTitleRight, MaxTitleWidth, TitleX, TitleY: Integer;
  StatusText, DrawTitle: string;
begin
  FPaintCanvas := ACanvas;
  FPaintBack := ABack;
  try
  PC.Brush.Style := bsSolid;
  PC.Brush.Color := BackColor;
  PC.FillRect(ClientRect);
  PC.Font.Assign(Font);

  if FTitleCaption <> '' then
  begin
    TitleX := 10;
    if not FShowControls then
      MaxTitleRight := ClientWidth - 8
    else if FShowStatus and (Length(FStatusColors) > 0) then
      MaxTitleRight := Min((ClientWidth * 2) div 5, StatusCircleRect(0).Left - 8)
    else
      MaxTitleRight := Min((ClientWidth * 2) div 5, EditRect.Left - 8);
    MaxTitleWidth := Max(0, MaxTitleRight - TitleX);
    if MaxTitleWidth > 8 then
    begin
      PC.Font.Style := Font.Style + [fsBold];
      PC.Font.Color := UITextColor;
      PC.Brush.Style := bsClear;
      DrawTitle := FTitleCaption;
      if (PC.TextWidth(DrawTitle) > MaxTitleWidth) and (Length(DrawTitle) > 1) then
      begin
        while (Length(DrawTitle) > 1) and
              (PC.TextWidth(DrawTitle + '...') > MaxTitleWidth) do
          Delete(DrawTitle, Length(DrawTitle), 1);
        DrawTitle := DrawTitle + '...';
      end;
      TitleY := (Height - PC.TextHeight(DrawTitle)) div 2;
      PC.TextOut(TitleX, TitleY, DrawTitle);
      PC.Font.Style := Font.Style;
    end;
  end;

  if not FShowControls then
    Exit;

  if FShowStatus then
  begin
    for Index := 0 to High(FStatusColors) do
      DrawStatusCircle(StatusCircleRect(Index), Index);
    // the name of the hovered status, else of the current one
    if (FHot = shStatus) and (FHotStatus >= 0) then
      CaptionIndex := FHotStatus
    else
      CaptionIndex := FStatusIndex;
    if (CaptionIndex >= 0) and (CaptionIndex <= High(FStatusCaptions)) and
       (Length(FStatusColors) > 0) then
    begin
      StatusText := FStatusCaptions[CaptionIndex];
      CaptionLeft := StatusCircleRect(High(FStatusColors)).Right + 8;
      if CaptionLeft + PC.TextWidth(StatusText) < EditRect.Left - 4 then
      begin
        PC.Brush.Style := bsClear;
        PC.Font.Color := UIMutedTextColor;
        PC.TextOut(CaptionLeft, (Height - PC.TextHeight(StatusText)) div 2,
          StatusText);
      end;
    end;
  end;

  DrawBox(EditRect, SStripEdit, FEditActive, FHot = shEdit);
  DrawBox(PropertiesRect, SStripProperties, FPropertiesActive, FHot = shProperties);
  finally
    FPaintCanvas := nil;
  end;
end;

const
  StripKeyColor = TColor($00FF00FF);

{$IFDEF LCLGTK2}
procedure gdk_window_input_shape_combine_region(window: PGdkWindow;
  shape_region: PGdkRegion; offset_x, offset_y: LongInt); cdecl; external gdklib;
{$ENDIF}

{ The bar has no background at all. Everything it draws is drawn once on a key
  colour to learn which pixels are text or icon; the bar's window is then cut
  down to exactly those pixels (plus its click targets), so the verse text
  running behind it shows through everywhere else. }
procedure TVersePanelStrip.ApplyTransparency;
{$IFDEF LCLGTK2}
var
  Bmp: TBitmap;
  Wnd: PGtkWidget;
  Shape, Input: PGdkRegion;
  GR: TGdkRectangle;
  X, Y, RunStart, Key: Integer;
  Row: Boolean;
  procedure AddRect(Reg: PGdkRegion; const R: TRect);
  begin
    if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;
    GR.x := R.Left; GR.y := R.Top;
    GR.width := R.Right - R.Left; GR.height := R.Bottom - R.Top;
    gdk_region_union_with_rect(Reg, @GR);
  end;
{$ENDIF}
begin
{$IFDEF LCLGTK2}
  try
    if (not HandleAllocated) or (Width < 2) or (Height < 2) then Exit;
    Wnd := PGtkWidget(Handle);
    if (Wnd = nil) or (Wnd^.window = nil) then Exit;
    if (Wnd^.parent <> nil) and (Wnd^.window = Wnd^.parent^.window) then Exit;
    Bmp := TBitmap.Create;
    try
      Bmp.PixelFormat := pf24bit;
      Bmp.SetSize(Width, Height);
      Bmp.Canvas.Font.Assign(Font);
      PaintContent(Bmp.Canvas, StripKeyColor);
      Shape := gdk_region_new;
      Input := gdk_region_new;
      Key := ColorToRGB(StripKeyColor);
      for Y := 0 to Height - 1 do
      begin
        RunStart := -1;
        for X := 0 to Width do
        begin
          Row := (X < Width) and (ColorToRGB(Bmp.Canvas.Pixels[X, Y]) <> Key);
          if Row and (RunStart < 0) then
            RunStart := X
          else if (not Row) and (RunStart >= 0) then
          begin
            AddRect(Shape, Rect(RunStart, Y, X, Y + 1));
            RunStart := -1;
          end;
        end;
      end;
      // click targets stay clickable even where nothing is drawn
      gdk_region_union(Input, Shape);
      if FShowControls then
      begin
        AddRect(Input, EditRect);
        AddRect(Input, PropertiesRect);
        if FShowStatus then
          for X := 0 to High(FStatusColors) do
            AddRect(Input, StatusCircleRect(X));
      end;
      gdk_window_shape_combine_region(Wnd^.window, Shape, 0, 0);
      gdk_window_input_shape_combine_region(Wnd^.window, Input, 0, 0);
      gdk_region_destroy(Shape);
      gdk_region_destroy(Input);
    finally
      Bmp.Free;
    end;
  except
    // cosmetic only
  end;
{$ENDIF}
end;

procedure TVersePanelStrip.Paint;
begin
  PaintContent(Canvas, BackColor);
  ApplyTransparency;
end;

procedure TVersePanelStrip.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHot: TStripHit;
  NewStatus: Integer;
  NewHint: string;
begin
  inherited MouseMove(Shift, X, Y);
  NewHot := HitTestAt(X, Y, NewStatus);
  if (NewHot <> FHot) or (NewStatus <> FHotStatus) then
  begin
    FHot := NewHot;
    FHotStatus := NewStatus;
    if FHot <> shNone then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
    Invalidate;
  end;
  case FHot of
    shEdit: NewHint := SStripEditHint;
    shProperties: NewHint := SStripPropertiesHint;
    shStatus:
      if (FHotStatus >= 0) and (FHotStatus <= High(FStatusCaptions)) then
        NewHint := FStatusCaptions[FHotStatus]
      else
        NewHint := '';
  else
    if (X <= (ClientWidth * 2) div 5) and (FTitleHint <> '') then
      NewHint := FTitleHint
    else
      NewHint := '';
  end;
  Hint := NewHint;
end;

procedure TVersePanelStrip.MouseLeave;
begin
  inherited MouseLeave;
  if FHot <> shNone then
  begin
    FHot := shNone;
    FHotStatus := -1;
    Cursor := crDefault;
    Invalidate;
  end;
end;

procedure TVersePanelStrip.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Hit: TStripHit;
  StatusIndex: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button <> mbLeft then
    Exit;
  Hit := HitTestAt(X, Y, StatusIndex);
  case Hit of
    shEdit:
      if Assigned(FOnEdit) then
        FOnEdit(Self);
    shProperties:
      if Assigned(FOnProperties) then
        FOnProperties(Self);
    shStatus:
      if Assigned(FOnStatusPick) and (StatusIndex >= 0) then
        FOnStatusPick(Self, StatusIndex);
  end;
end;

{ TVerseChevrons }

constructor TVerseChevrons.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  DoubleBuffered := True;
  FHot := -1;
  ShowHint := True;
  SetBounds(0, 0, 38, 22);
end;

function TVerseChevrons.SlotRect(Index: Integer): TRect;
begin
  Result := Rect(Index * 20, 0, Index * 20 + 18, Height);
end;

function TVerseChevrons.SlotAt(X, Y: Integer): Integer;
var
  P: TPoint;
begin
  P := Point(X, Y);
  if PtInRect(SlotRect(0), P) then
    Exit(0);
  if PtInRect(SlotRect(1), P) then
    Exit(1);
  Result := -1;
end;

procedure TVerseChevrons.Paint;
var
  Slot, CX, CY, Tip, Arm: Integer;
  R: TRect;
  Idle, HotColor: TColor;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  Idle := CurrentSkin.WindowTextDim;
  if CurrentSkin.IsDark then
    HotColor := MakeRGB(130, 215, 255)
  else
    HotColor := CurrentSkin.PointedBg;
  if CurrentSkin.Id = skClassic then
  begin
    Idle := clGrayText;
    HotColor := clHighlight;
  end;
  for Slot := 0 to 1 do
  begin
    R := SlotRect(Slot);
    CX := (R.Left + R.Right) div 2;
    CY := (R.Top + R.Bottom) div 2;
    if Slot = 0 then
    begin
      Tip := CX - 2;
      Arm := CX + 3;
    end
    else
    begin
      Tip := CX + 2;
      Arm := CX - 3;
    end;
    if Slot = FHot then
      Canvas.Pen.Color := HotColor
    else
      Canvas.Pen.Color := Idle;
    Canvas.Pen.Width := 2;
    Canvas.MoveTo(Arm, CY - 5);
    Canvas.LineTo(Tip, CY);
    Canvas.LineTo(Arm, CY + 5);
  end;
  Canvas.Pen.Width := 1;
end;

procedure TVerseChevrons.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHot: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  NewHot := SlotAt(X, Y);
  if NewHot <> FHot then
  begin
    FHot := NewHot;
    case FHot of
      0: Hint := SStripPreviousVerse;
      1: Hint := SStripNextVerse;
    else
      Hint := '';
    end;
    if FHot >= 0 then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
    Invalidate;
  end;
end;

procedure TVerseChevrons.MouseLeave;
begin
  inherited MouseLeave;
  if FHot <> -1 then
  begin
    FHot := -1;
    Cursor := crDefault;
    Invalidate;
  end;
end;

procedure TVerseChevrons.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Slot: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button <> mbLeft then
    Exit;
  Slot := SlotAt(X, Y);
  if (Slot >= 0) and Assigned(FOnNavigate) then
    FOnNavigate(Self, IfThen(Slot = 0, -1, 1));
end;

end.
