unit directwrite_syntagm_view;

{$mode objfpc}{$H+}
{$interfaces COM}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, LCLType,
  verse_visual_document, verse_view_intf
  {$IFDEF MSWINDOWS}
  , Windows, win_directwrite_d2d
  {$ENDIF};

type
  TDirectWriteBackendState = (
    dwbsNotInitialized,
    dwbsAvailable,
    dwbsUnavailable
  );

  TVerseHitEvent = procedure(Sender: TObject; const Hit: TVerseHit) of object;
  TVerseMouseDownEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; const Hit: TVerseHit) of object;

  {$IFDEF MSWINDOWS}
  TDWriteHitTestMetricsArray = array of TDWriteHitTestMetrics;
  {$ENDIF}


  { A single paragraph surface. The document keeps logical syntagm identity;
    DirectWrite owns shaping, wrapping, visual order, and hit testing. }
  TDirectWriteSyntagmView = class(TCustomControl, IVerseView)
  private
    FBackendError: UnicodeString;
    FBackendState: TDirectWriteBackendState;
    FDisplayTags: Boolean;
    FDocument: IVerseVisualDocument;
    FLastHoverLogicalIndex: Integer;
    FLayoutPadding: Integer;
    FLastContentHeight: Integer;
    FGeometryMatchesPaint: Boolean;
    FOnContentHeightChanged: TNotifyEvent;
    FOnVerseDoubleClick: TNotifyEvent;
    FOnVerseMouseDown: TVerseMouseDownEvent;
    FOnVerseHover: TVerseHitEvent;
    {$IFDEF MSWINDOWS}
    FD2DFactory: IWinD2D1Factory;
    FDWriteFactory: IWinDWriteFactory;
    FDefaultBrush: IWinD2D1SolidColorBrush;
    FDrawingEffectsReady: Boolean;
    FLayoutDpi: Integer;
    FLayoutFontColor: TColor;
    FLayoutFontName: string;
    FLayoutFontSize: Integer;
    FLayoutFontStyle: TFontStyles;
    FLayoutRevision: QWord;
    FLayoutWidth: Integer;
    FRangeBrushes: array of IWinD2D1SolidColorBrush;
    FSuperscriptObjects: TList;
    FRenderTarget: IWinD2D1DCRenderTarget;
    FTextFormat: IWinDWriteTextFormat;
    FTextLayout: IWinDWriteTextLayout;
    FTextMetrics: TDWriteTextMetrics;
    function ApplyDrawingEffects: Boolean;
    function BuildLayout(ADpi: Integer): Boolean;
    procedure DiscardRenderTarget;
    function EnsureFactories: Boolean;
    function EnsureLayout: Boolean;
    function EnsureRenderTarget(DC: HDC; ADpi: Integer): Boolean;
    function GetHitTestMetrics(AStart, ALength: Integer;
      out Metrics: TDWriteHitTestMetricsArray): Boolean;
    function LayoutOriginDIP(ADpi: Integer): TD2DPoint2F;
    procedure PaintDirectWrite(DC: HDC; ADpi: Integer);
    procedure PaintSpanBackgrounds(ADpi: Integer);
    {$ENDIF}
    function GetBackendAvailable: Boolean;
    function GetBackendError: UnicodeString;
    function GetBackendName: string;
    function GetContentHeight: Integer;
    function GetDisplayTags: Boolean;
    function GetDocument: IVerseVisualDocument;
    procedure InvalidateLayout;
    procedure PaintFallback;
    procedure PaintLogicalTags;
    procedure NotifyContentHeightChanged;
    procedure SetDisplayTags(Value: Boolean);
    procedure SetLayoutPadding(Value: Integer);
  protected
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { IUnknown is deliberately non-refcounted: the LCL owner controls the
      component lifetime. Do not retain IVerseView after freeing the control. }
    function QueryInterface(constref IID: TGUID; out Obj): LongInt;
      {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: LongInt;
      {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: LongInt;
      {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

    { IVerseView }
    procedure SetDocument(const ADocument: IVerseVisualDocument);
    procedure NotifyChanges(Changes: TVerseVisualChanges; Revision: QWord);
    function HitTest(const ClientPoint: TPoint; out Hit: TVerseHit): Boolean;
    function GetLogicalRects(LogicalIndex: Integer;
      out Rects: TRectArray): Boolean;
    function TryGetLogicalScreenAnchor(LogicalIndex: Integer;
      out Anchor: TPoint): Boolean;

    property BackendAvailable: Boolean read GetBackendAvailable;
    property BackendError: UnicodeString read GetBackendError;
    property BackendName: string read GetBackendName;
    property ContentHeight: Integer read GetContentHeight;
    property Document: IVerseVisualDocument read GetDocument;
  published
    property Align;
    property Anchors;
    property Color default clWindow;
    property Font;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property DisplayTags: Boolean read GetDisplayTags write SetDisplayTags
      default False;
    property LayoutPadding: Integer read FLayoutPadding write SetLayoutPadding
      default 4;
    property OnContentHeightChanged: TNotifyEvent read FOnContentHeightChanged
      write FOnContentHeightChanged;
    property OnVerseDoubleClick: TNotifyEvent read FOnVerseDoubleClick
      write FOnVerseDoubleClick;
    property OnVerseMouseDown: TVerseMouseDownEvent read FOnVerseMouseDown
      write FOnVerseMouseDown;
    property OnVerseHover: TVerseHitEvent read FOnVerseHover write FOnVerseHover;
  end;

implementation

uses
  Math, Forms, text_direction
  {$IFDEF MSWINDOWS}
  , directwrite_superscript_inline
  {$ENDIF};

const
  E_NOINTERFACE_VALUE = LongInt(-2147467262); // $80004002

function Succeeded(HR: LongInt): Boolean; inline;
begin
  Result := HR >= 0;
end;

{ TDirectWriteSyntagmView }

constructor TDirectWriteSyntagmView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks];
  Color := clWindow;
  ParentColor := False;
  Width := 320;
  Height := 120;
  FBackendState := dwbsNotInitialized;
  FDisplayTags := False;
  FLastHoverLogicalIndex := -1;
  FLayoutPadding := 4;
  FLastContentHeight := -1;
  FGeometryMatchesPaint := False;
  {$IFDEF MSWINDOWS}
  FSuperscriptObjects := TList.Create;
  {$ENDIF}
end;

destructor TDirectWriteSyntagmView.Destroy;
begin
  FDocument := nil;
  {$IFDEF MSWINDOWS}
  InvalidateLayout;
  DiscardRenderTarget;
  FD2DFactory := nil;
  FDWriteFactory := nil;
  FSuperscriptObjects.Free;
  {$ENDIF}
  inherited Destroy;
end;

function TDirectWriteSyntagmView.QueryInterface(constref IID: TGUID;
  out Obj): LongInt;
  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE_VALUE;
end;

function TDirectWriteSyntagmView._AddRef: LongInt;
  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function TDirectWriteSyntagmView._Release: LongInt;
  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

procedure TDirectWriteSyntagmView.SetDocument(
  const ADocument: IVerseVisualDocument);
var
  LeaveHit: TVerseHit;
  HadHover: Boolean;
  OldRevision: QWord;
  PreserveHover: Boolean;
begin
  HadHover := FLastHoverLogicalIndex >= 0;
  OldRevision := 0;
  if Assigned(FDocument) then
    OldRevision := FDocument.Revision;
  PreserveHover := HadHover and Assigned(FDocument) and Assigned(ADocument) and
    (OldRevision = ADocument.Revision);
  if not PreserveHover then
  begin
    FLastHoverLogicalIndex := -1;
    Cursor := crDefault;
  end;

  { Install the new snapshot before notifying leave. The callback can rebuild
    the document, and its newer snapshot must not be overwritten on return. }
  FDocument := ADocument;
  InvalidateLayout;
  Invalidate;
  NotifyContentHeightChanged;

  if HadHover and not PreserveHover and Assigned(FOnVerseHover) then
  begin
    FillChar(LeaveHit, SizeOf(LeaveHit), 0);
    LeaveHit.Revision := OldRevision;
    LeaveHit.LogicalIndex := -1;
    LeaveHit.UTF16Offset := -1;
    FOnVerseHover(Self, LeaveHit);
  end;
end;

procedure TDirectWriteSyntagmView.DblClick;
begin
  inherited DblClick;
  if Assigned(FOnVerseDoubleClick) then
    FOnVerseDoubleClick(Self);
end;

procedure TDirectWriteSyntagmView.NotifyChanges(Changes: TVerseVisualChanges;
  Revision: QWord);
begin
  if Changes * [vvcContent, vvcStyle, vvcBaseDirection, vvcViewport] <> [] then
    InvalidateLayout;
  Invalidate;
  NotifyContentHeightChanged;
end;

function TDirectWriteSyntagmView.GetDocument: IVerseVisualDocument;
begin
  Result := FDocument;
end;

function TDirectWriteSyntagmView.GetDisplayTags: Boolean;
begin
  Result := FDisplayTags;
end;

procedure TDirectWriteSyntagmView.SetDisplayTags(Value: Boolean);
begin
  if FDisplayTags = Value then
    Exit;
  FDisplayTags := Value;
  Invalidate;
end;

procedure TDirectWriteSyntagmView.SetLayoutPadding(Value: Integer);
begin
  Value := EnsureRange(Value, 0, 64);
  if FLayoutPadding = Value then
    Exit;
  FLayoutPadding := Value;
  InvalidateLayout;
  Invalidate;
  NotifyContentHeightChanged;
end;

procedure TDirectWriteSyntagmView.InvalidateLayout;
begin
  FGeometryMatchesPaint := False;
  {$IFDEF MSWINDOWS}
  if Assigned(FSuperscriptObjects) then
    FSuperscriptObjects.Clear;
  FTextLayout := nil;
  FTextFormat := nil;
  FDefaultBrush := nil;
  SetLength(FRangeBrushes, 0);
  FDrawingEffectsReady := False;
  FillChar(FTextMetrics, SizeOf(FTextMetrics), 0);
  FLayoutWidth := -1;
  FLayoutDpi := 0;
  FLayoutRevision := 0;
  {$ENDIF}
end;

function TDirectWriteSyntagmView.GetBackendAvailable: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := EnsureFactories;
  {$ELSE}
  FBackendState := dwbsUnavailable;
  if FBackendError = '' then
    FBackendError := 'DirectWrite is available only on Windows';
  Result := False;
  {$ENDIF}
end;

function TDirectWriteSyntagmView.GetBackendError: UnicodeString;
begin
  if FBackendState = dwbsNotInitialized then
    GetBackendAvailable;
  Result := FBackendError;
end;

function TDirectWriteSyntagmView.GetBackendName: string;
begin
  if GetBackendAvailable then
    Result := 'DirectWrite + Direct2D DC render target'
  else
    Result := 'LCL text fallback';
end;

function TDirectWriteSyntagmView.GetContentHeight: Integer;
{$IFDEF MSWINDOWS}
var
  Dpi: Integer;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if EnsureLayout then
  begin
    Dpi := FLayoutDpi;
    if Dpi <= 0 then
      Dpi := 96;
    Result := Ceil(FTextMetrics.Height * Dpi / 96) + 2 * FLayoutPadding;
    Exit;
  end;
  {$ENDIF}
  Result := Height;
end;

procedure TDirectWriteSyntagmView.NotifyContentHeightChanged;
var
  NewHeight: Integer;
begin
  NewHeight := GetContentHeight;
  if NewHeight = FLastContentHeight then
    Exit;
  FLastContentHeight := NewHeight;
  if Assigned(FOnContentHeightChanged) then
    FOnContentHeightChanged(Self);
end;

procedure TDirectWriteSyntagmView.Resize;
begin
  inherited Resize;
  InvalidateLayout;
  NotifyContentHeightChanged;
end;

procedure TDirectWriteSyntagmView.PaintFallback;
var
  DrawRect: TRect;
  RenderText: string;
begin
  FGeometryMatchesPaint := False;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  if not Assigned(FDocument) then
    Exit;
  Canvas.Font.Assign(Font);
  Canvas.Font.Color := Font.Color;
  Canvas.Brush.Style := bsClear;
  DrawRect := ClientRect;
  Inc(DrawRect.Left, FLayoutPadding);
  Inc(DrawRect.Top, FLayoutPadding);
  Dec(DrawRect.Right, FLayoutPadding);
  Dec(DrawRect.Bottom, FLayoutPadding);
  RenderText := UTF8Encode(FDocument.Text);
  Canvas.TextRect(DrawRect, DrawRect.Left, DrawRect.Top, RenderText);
end;

procedure TDirectWriteSyntagmView.PaintLogicalTags;
var
  Index: Integer;
  Rects: TRectArray;
  SavedColor: TColor;
  SavedSize: Integer;
  Span: TVisualSpan;
begin
  if not FDisplayTags or not Assigned(FDocument) then
    Exit;
  SavedColor := Canvas.Font.Color;
  SavedSize := Canvas.Font.Size;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := clGrayText;
  Canvas.Font.Size := 7;
  for Index := 0 to FDocument.SpanCount - 1 do
  begin
    Span := FDocument.Spans[Index];
    if Span.Interactive and (Span.Annotation <> '') and
       GetLogicalRects(Span.LogicalIndex, Rects) and (Length(Rects) > 0) then
      Canvas.TextOut(Rects[0].Left, Rects[0].Top,
        UTF8Encode(Span.Annotation));
  end;
  Canvas.Font.Color := SavedColor;
  Canvas.Font.Size := SavedSize;
end;

procedure TDirectWriteSyntagmView.Paint;
{$IFDEF MSWINDOWS}
var
  Dpi: Integer;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Dpi := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
  if Dpi <= 0 then
    Dpi := 96;
  if EnsureFactories and EnsureRenderTarget(Canvas.Handle, Dpi) and
     EnsureLayout and ApplyDrawingEffects then
    PaintDirectWrite(Canvas.Handle, Dpi)
  else
    PaintFallback;
  {$ELSE}
  PaintFallback;
  {$ENDIF}
  NotifyContentHeightChanged;
  PaintLogicalTags;
end;

procedure TDirectWriteSyntagmView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ClientPoint: TPoint;
  Hit: TVerseHit;
begin
  inherited MouseDown(Button, Shift, X, Y);
  FillChar(Hit, SizeOf(Hit), 0);
  Hit.LogicalIndex := -1;
  Hit.UTF16Offset := -1;
  if Assigned(FDocument) then
    Hit.Revision := FDocument.Revision;
  ClientPoint.X := X;
  ClientPoint.Y := Y;
  HitTest(ClientPoint, Hit);
  if Assigned(FOnVerseMouseDown) then
    FOnVerseMouseDown(Self, Button, Shift, Hit);
end;

procedure TDirectWriteSyntagmView.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  ClientPoint: TPoint;
  HasHit: Boolean;
  Hit: TVerseHit;
  LeaveHit: TVerseHit;
begin
  inherited MouseMove(Shift, X, Y);
  ClientPoint.X := X;
  ClientPoint.Y := Y;
  HasHit := HitTest(ClientPoint, Hit);
  if HasHit then
  begin
    Cursor := crHandPoint;
    if Hit.LogicalIndex <> FLastHoverLogicalIndex then
    begin
      if (FLastHoverLogicalIndex >= 0) and Assigned(FOnVerseHover) then
      begin
        FillChar(LeaveHit, SizeOf(LeaveHit), 0);
        LeaveHit.LogicalIndex := -1;
        LeaveHit.UTF16Offset := -1;
        if Assigned(FDocument) then
          LeaveHit.Revision := FDocument.Revision;
        FOnVerseHover(Self, LeaveHit);
      end;
      if Assigned(FOnVerseHover) then
        FOnVerseHover(Self, Hit);
    end;
    FLastHoverLogicalIndex := Hit.LogicalIndex;
  end
  else
  begin
    Cursor := crDefault;
    if (FLastHoverLogicalIndex >= 0) and Assigned(FOnVerseHover) then
    begin
      FillChar(LeaveHit, SizeOf(LeaveHit), 0);
      LeaveHit.LogicalIndex := -1;
      LeaveHit.UTF16Offset := -1;
      if Assigned(FDocument) then
        LeaveHit.Revision := FDocument.Revision;
      FOnVerseHover(Self, LeaveHit);
    end;
    FLastHoverLogicalIndex := -1;
  end;
end;

procedure TDirectWriteSyntagmView.MouseLeave;
var
  LeaveHit: TVerseHit;
begin
  inherited MouseLeave;
  Cursor := crDefault;
  if (FLastHoverLogicalIndex >= 0) and Assigned(FOnVerseHover) then
  begin
    FillChar(LeaveHit, SizeOf(LeaveHit), 0);
    LeaveHit.LogicalIndex := -1;
    LeaveHit.UTF16Offset := -1;
    if Assigned(FDocument) then
      LeaveHit.Revision := FDocument.Revision;
    FOnVerseHover(Self, LeaveHit);
  end;
  FLastHoverLogicalIndex := -1;
end;

function TDirectWriteSyntagmView.HitTest(const ClientPoint: TPoint;
  out Hit: TVerseHit): Boolean;
{$IFDEF MSWINDOWS}
var
  Dpi: Integer;
  HR: HRESULT;
  IsInside: TDWriteBool;
  IsTrailing: TDWriteBool;
  LogicalIndex: Integer;
  Metrics: TDWriteHitTestMetrics;
  Origin: TD2DPoint2F;
  PointX: Single;
  PointY: Single;
{$ENDIF}
begin
  FillChar(Hit, SizeOf(Hit), 0);
  Hit.LogicalIndex := -1;
  Hit.UTF16Offset := -1;
  if not Assigned(FDocument) or not FGeometryMatchesPaint then
    Exit(False);
  Hit.Revision := FDocument.Revision;
  {$IFDEF MSWINDOWS}
  if not EnsureLayout then
    Exit(False);
  Dpi := FLayoutDpi;
  if Dpi <= 0 then
    Dpi := 96;
  Origin := LayoutOriginDIP(Dpi);
  PointX := ClientPoint.X * 96 / Dpi - Origin.X;
  PointY := ClientPoint.Y * 96 / Dpi - Origin.Y;
  HR := FTextLayout.HitTestPoint(PointX, PointY, IsTrailing, IsInside,
    Metrics);
  if not Succeeded(HR) or not IsInside then
    Exit(False);
  if not FDocument.TryGetInteractiveLogicalIndexAt(Metrics.TextPosition,
    LogicalIndex) then
    Exit(False);
  Hit.LogicalIndex := LogicalIndex;
  Hit.UTF16Offset := Metrics.TextPosition;
  Hit.Trailing := IsTrailing;
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TDirectWriteSyntagmView.GetLogicalRects(LogicalIndex: Integer;
  out Rects: TRectArray): Boolean;
{$IFDEF MSWINDOWS}
var
  Dpi: Integer;
  HitMetrics: TDWriteHitTestMetricsArray;
  Index: Integer;
  LengthUTF16: Integer;
  Origin: TD2DPoint2F;
  StartUTF16: Integer;
{$ENDIF}
begin
  SetLength(Rects, 0);
  if not Assigned(FDocument) or not FGeometryMatchesPaint then
    Exit(False);
  {$IFDEF MSWINDOWS}
  if not FDocument.TryGetUTF16Range(LogicalIndex, StartUTF16,
    LengthUTF16) or (LengthUTF16 <= 0) or not EnsureLayout then
    Exit(False);
  if not GetHitTestMetrics(StartUTF16, LengthUTF16, HitMetrics) then
    Exit(False);
  SetLength(Rects, Length(HitMetrics));
  Dpi := FLayoutDpi;
  if Dpi <= 0 then
    Dpi := 96;
  Origin := LayoutOriginDIP(Dpi);
  for Index := 0 to High(HitMetrics) do
  begin
    Rects[Index].Left := Floor((Origin.X + HitMetrics[Index].Left) *
      Dpi / 96);
    Rects[Index].Top := Floor((Origin.Y + HitMetrics[Index].Top) *
      Dpi / 96);
    Rects[Index].Right := Ceil((Origin.X + HitMetrics[Index].Left +
      HitMetrics[Index].Width) * Dpi / 96);
    Rects[Index].Bottom := Ceil((Origin.Y + HitMetrics[Index].Top +
      HitMetrics[Index].Height) * Dpi / 96);
  end;
  Result := Length(Rects) > 0;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TDirectWriteSyntagmView.TryGetLogicalScreenAnchor(
  LogicalIndex: Integer; out Anchor: TPoint): Boolean;
var
  Rects: TRectArray;
begin
  Anchor.X := 0;
  Anchor.Y := 0;
  Result := GetLogicalRects(LogicalIndex, Rects) and (Length(Rects) > 0);
  if Result then
  begin
    Anchor.X := Rects[0].Left;
    Anchor.Y := Rects[0].Bottom;
    Anchor := ClientToScreen(Anchor);
  end;
end;

{$IFDEF MSWINDOWS}

function TDirectWriteSyntagmView.EnsureFactories: Boolean;
var
  ErrorText: UnicodeString;
begin
  case FBackendState of
    dwbsAvailable: Exit(True);
    dwbsUnavailable: Exit(False);
  end;

  if not TryCreateDWriteFactory(FDWriteFactory, ErrorText) then
  begin
    FBackendError := ErrorText;
    FBackendState := dwbsUnavailable;
    Exit(False);
  end;
  if not TryCreateD2DFactory(FD2DFactory, ErrorText) then
  begin
    FDWriteFactory := nil;
    FBackendError := ErrorText;
    FBackendState := dwbsUnavailable;
    Exit(False);
  end;
  FBackendError := '';
  FBackendState := dwbsAvailable;
  Result := True;
end;

procedure TDirectWriteSyntagmView.DiscardRenderTarget;
begin
  FDefaultBrush := nil;
  SetLength(FRangeBrushes, 0);
  if Assigned(FSuperscriptObjects) then
    FSuperscriptObjects.Clear;
  FRenderTarget := nil;
  FDrawingEffectsReady := False;
  FTextLayout := nil;
end;

function TDirectWriteSyntagmView.EnsureRenderTarget(DC: HDC;
  ADpi: Integer): Boolean;
var
  HR: HRESULT;
  Properties: TD2DRenderTargetProperties;
begin
  if Assigned(FRenderTarget) then
  begin
    FRenderTarget.SetDpi(ADpi, ADpi);
    Exit(True);
  end;
  FillChar(Properties, SizeOf(Properties), 0);
  Properties.TargetType := D2D1_RENDER_TARGET_TYPE_DEFAULT;
  Properties.PixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  Properties.PixelFormat.AlphaMode := D2D1_ALPHA_MODE_IGNORE;
  Properties.DpiX := ADpi;
  Properties.DpiY := ADpi;
  Properties.Usage := D2D1_RENDER_TARGET_USAGE_NONE;
  Properties.MinLevel := D2D1_FEATURE_LEVEL_DEFAULT;
  HR := FD2DFactory.CreateDCRenderTarget(@Properties, FRenderTarget);
  Result := Succeeded(HR) and Assigned(FRenderTarget);
  if not Result then
    FBackendError := 'CreateDCRenderTarget failed: ' + HResultText(HR);
end;

function TDirectWriteSyntagmView.LayoutOriginDIP(
  ADpi: Integer): TD2DPoint2F;
begin
  if ADpi <= 0 then
    ADpi := 96;
  Result.X := FLayoutPadding * 96 / ADpi;
  Result.Y := FLayoutPadding * 96 / ADpi;
end;

function TDirectWriteSyntagmView.BuildLayout(ADpi: Integer): Boolean;
var
  BaseFontSize: Single;
  BaseFontStyle: LongInt;
  BaseFontWeight: LongInt;
  FamilyName: UnicodeString;
  HR: HRESULT;
  Index: Integer;
  Superscript: TDirectWriteSuperscriptInline;
  SuperscriptInterface: IWinDWriteInlineObject;
  SuperscriptStatus: HRESULT;
  SuperscriptText: UnicodeString;
  LayoutHeight: Single;
  LayoutWidth: Single;
  LocaleName: UnicodeString;
  Range: TDWriteTextRange;
  Span: TVisualSpan;
  DocumentText: UnicodeString;
begin
  Result := False;
  if not Assigned(FDocument) or not EnsureFactories then
    Exit;
  if ADpi <= 0 then
    ADpi := 96;
  FamilyName := UTF8Decode(Font.Name);
  if (FamilyName = '') or SameText(UTF8Encode(FamilyName), 'default') then
    FamilyName := UTF8Decode(Screen.MenuFont.Name);
  if FamilyName = '' then
    FamilyName := 'Segoe UI';
  BaseFontSize := Font.Size * 96 / 72;
  if BaseFontSize <= 0 then
    BaseFontSize := 13.333;
  LocaleName := 'pt-BR';


  BaseFontWeight := DWRITE_FONT_WEIGHT_NORMAL;
  if fsBold in Font.Style then
    BaseFontWeight := 700;
  BaseFontStyle := DWRITE_FONT_STYLE_NORMAL;
  if fsItalic in Font.Style then
    BaseFontStyle := 2;
  HR := FDWriteFactory.CreateTextFormat(PWideChar(FamilyName), nil,
    BaseFontWeight, BaseFontStyle,
    DWRITE_FONT_STRETCH_NORMAL, BaseFontSize, PWideChar(LocaleName),
    FTextFormat);
  if not Succeeded(HR) then
  begin
    FBackendError := 'CreateTextFormat failed: ' + HResultText(HR);
    Exit;
  end;
  FTextFormat.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
  FTextFormat.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
  FTextFormat.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP);
  if FDocument.BaseDirection = text_direction.tdRightToLeft then
    FTextFormat.SetReadingDirection(DWRITE_READING_DIRECTION_RIGHT_TO_LEFT)
  else
    FTextFormat.SetReadingDirection(DWRITE_READING_DIRECTION_LEFT_TO_RIGHT);
  FTextFormat.SetFlowDirection(DWRITE_FLOW_DIRECTION_TOP_TO_BOTTOM);

  LayoutWidth := Max(1, ClientWidth - 2 * FLayoutPadding) * 96 / ADpi;
  LayoutHeight := 1048576;
  DocumentText := FDocument.Text;
  HR := FDWriteFactory.CreateTextLayout(PWideChar(DocumentText), Length(DocumentText),
    FTextFormat, LayoutWidth, LayoutHeight, FTextLayout);
  if not Succeeded(HR) then
  begin
    FBackendError := 'CreateTextLayout failed: ' + HResultText(HR);
    Exit;
  end;

  for Index := 0 to FDocument.SpanCount - 1 do
  begin
    Span := FDocument.Spans[Index];
    if (Span.LengthUTF16 <= 0) or (Span.StartUTF16 < 0) or
       (Span.StartUTF16 + Span.LengthUTF16 > Length(DocumentText)) then
      Continue;
    Range.StartPosition := Span.StartUTF16;
    Range.Length := Span.LengthUTF16;
    if vtsfBold in Span.Style.Flags then
      FTextLayout.SetFontWeight(700, Range);
    if vtsfItalic in Span.Style.Flags then
      FTextLayout.SetFontStyle(2, Range);
    if vtsfUnderline in Span.Style.Flags then
      FTextLayout.SetUnderline(True, Range);
    if vtsfSuperscript in Span.Style.Flags then
    begin
      SuperscriptText := Copy(DocumentText, Span.StartUTF16 + 1,
        Span.LengthUTF16);
      Superscript := TDirectWriteSuperscriptInline.Create(FDWriteFactory,
        SuperscriptText, FamilyName, LocaleName, BaseFontSize, Span.Style,
        Span.Direction, Index, SuperscriptStatus);
      if Succeeded(SuperscriptStatus) then
      begin
        SuperscriptInterface := Superscript;
        SuperscriptStatus := FTextLayout.SetInlineObject(
          SuperscriptInterface, Range);
        if Succeeded(SuperscriptStatus) then
          FSuperscriptObjects.Add(Superscript)
        else
        begin
          SuperscriptInterface := nil;
          FTextLayout.SetFontSize(BaseFontSize * 0.72, Range);
        end;
      end
      else
      begin
        Superscript.Free;
        FTextLayout.SetFontSize(BaseFontSize * 0.72, Range);
      end;
    end;
  end;

  HR := FTextLayout.GetMetrics(FTextMetrics);
  if not Succeeded(HR) then
  begin
    FBackendError := 'GetMetrics failed: ' + HResultText(HR);
    FTextLayout := nil;
    Exit;
  end;
  FLayoutDpi := ADpi;
  FLayoutWidth := ClientWidth;
  FLayoutRevision := FDocument.Revision;
  FLayoutFontName := Font.Name;
  FLayoutFontSize := Font.Size;
  FLayoutFontColor := Font.Color;
  FDrawingEffectsReady := False;
  FBackendError := '';
  FLayoutFontStyle := Font.Style;
  Result := True;
end;

function TDirectWriteSyntagmView.EnsureLayout: Boolean;
var
  DC: HDC;
  Dpi: Integer;
begin
  if not Assigned(FDocument) then
    Exit(False);
  DC := 0;
  if HandleAllocated then
    DC := Windows.GetDC(Handle);
  try
    if DC <> 0 then
      Dpi := GetDeviceCaps(DC, LOGPIXELSX)
    else
      Dpi := Screen.PixelsPerInch;
  finally
    if DC <> 0 then
      Windows.ReleaseDC(Handle, DC);
  end;
  if Dpi <= 0 then
    Dpi := 96;
  if Assigned(FTextLayout) and (FLayoutDpi = Dpi) and
     (FLayoutWidth = ClientWidth) and
     (FLayoutRevision = FDocument.Revision) and
     (FLayoutFontColor = Font.Color) and
     (FLayoutFontName = Font.Name) and (FLayoutFontSize = Font.Size) and
     (FLayoutFontStyle = Font.Style) then
    Exit(True);
  InvalidateLayout;
  Result := BuildLayout(Dpi);
end;

function ColorToD2D(AColor: TColor): TD2DColorF;
var
  RGBColor: LongInt;
begin
  RGBColor := ColorToRGB(AColor);
  Result.R := Byte(RGBColor) / 255;
  Result.G := Byte(RGBColor shr 8) / 255;
  Result.B := Byte(RGBColor shr 16) / 255;
  Result.A := 1;
end;

function TDirectWriteSyntagmView.ApplyDrawingEffects: Boolean;
var
  RangeBrush: IWinD2D1SolidColorBrush;
  D2DColor: TD2DColorF;
  HR: HRESULT;
  Index: Integer;
  Range: TDWriteTextRange;
  Span: TVisualSpan;
  Superscript: TDirectWriteSuperscriptInline;
  SuperscriptIndex: Integer;
begin
  if FDrawingEffectsReady then
    Exit(True);
  Result := False;
  if not Assigned(FRenderTarget) or not Assigned(FTextLayout) then
    Exit;

  D2DColor := ColorToD2D(Font.Color);
  HR := FRenderTarget.CreateSolidColorBrush(@D2DColor, nil, FDefaultBrush);
  if not Succeeded(HR) then
  begin
    FBackendError := 'CreateSolidColorBrush failed: ' + HResultText(HR);
    Exit;
  end;
  for SuperscriptIndex := 0 to FSuperscriptObjects.Count - 1 do
  begin
    Superscript := TDirectWriteSuperscriptInline(
      FSuperscriptObjects[SuperscriptIndex]);
    Superscript.SetDrawingEffect(FDefaultBrush);
  end;
  SetLength(FRangeBrushes, 0);
  for Index := 0 to FDocument.SpanCount - 1 do
  begin
    Span := FDocument.Spans[Index];
    if not Span.Style.HasForegroundColor or (Span.LengthUTF16 <= 0) then
      Continue;
    D2DColor := ColorToD2D(TColor(Span.Style.ForegroundColor));
    RangeBrush := nil;
    HR := FRenderTarget.CreateSolidColorBrush(@D2DColor, nil, RangeBrush);
    if not Succeeded(HR) then
    begin
      FBackendError := 'Range brush creation failed: ' + HResultText(HR);
      Exit;
    end;
    Range.StartPosition := Span.StartUTF16;
    Range.Length := Span.LengthUTF16;
    HR := FTextLayout.SetDrawingEffect(RangeBrush, Range);
    if not Succeeded(HR) then
    begin
      FBackendError := 'SetDrawingEffect failed: ' + HResultText(HR);
      Exit;
    end;
    for SuperscriptIndex := 0 to FSuperscriptObjects.Count - 1 do
    begin
      Superscript := TDirectWriteSuperscriptInline(
        FSuperscriptObjects[SuperscriptIndex]);
      if Superscript.SpanIndex = Index then
        Superscript.SetDrawingEffect(RangeBrush);
    end;
    SetLength(FRangeBrushes, Length(FRangeBrushes) + 1);
    FRangeBrushes[High(FRangeBrushes)] := RangeBrush;
  end;
  FDrawingEffectsReady := True;
  Result := True;
end;

function TDirectWriteSyntagmView.GetHitTestMetrics(AStart,
  ALength: Integer; out Metrics: TDWriteHitTestMetricsArray): Boolean;
var
  Count: TDWriteUInt32;
  HR: HRESULT;
  Needed: TDWriteUInt32;
begin
  SetLength(Metrics, 0);
  Result := False;
  if not Assigned(FTextLayout) or (AStart < 0) or (ALength <= 0) then
    Exit;

  Count := 0;
  HR := FTextLayout.HitTestTextRange(AStart, ALength, 0, 0,
    nil, 0, Count);
  if (HR <> E_NOT_SUFFICIENT_BUFFER) and not Succeeded(HR) then
    Exit;
  if Count = 0 then
    Exit;

  repeat
    if Count > 1048576 then
      Exit;
    SetLength(Metrics, Count);
    Needed := 0;
    HR := FTextLayout.HitTestTextRange(AStart, ALength, 0, 0,
      @Metrics[0], Length(Metrics), Needed);
    if (HR = E_NOT_SUFFICIENT_BUFFER) and
       (Needed > TDWriteUInt32(Length(Metrics))) then
    begin
      Count := Needed;
      Continue;
    end;
    if not Succeeded(HR) then
    begin
      SetLength(Metrics, 0);
      Exit;
    end;
    SetLength(Metrics, Needed);
    Result := Needed > 0;
    Exit;
  until False;
end;

procedure TDirectWriteSyntagmView.PaintSpanBackgrounds(ADpi: Integer);
var
  BackgroundBrush: IWinD2D1SolidColorBrush;
  BackgroundColor: TD2DColorF;
  HR: HRESULT;
  Index: Integer;
  MetricIndex: Integer;
  Metrics: TDWriteHitTestMetricsArray;
  Origin: TD2DPoint2F;
  Rect: TD2DRectF;
  Span: TVisualSpan;
begin
  Origin := LayoutOriginDIP(ADpi);
  for Index := 0 to FDocument.SpanCount - 1 do
  begin
    Span := FDocument.Spans[Index];
    if not Span.Style.HasBackgroundColor or (Span.LengthUTF16 <= 0) then
      Continue;
    if not GetHitTestMetrics(Span.StartUTF16, Span.LengthUTF16, Metrics) then
      Continue;
    BackgroundColor := ColorToD2D(TColor(Span.Style.BackgroundColor));
    BackgroundBrush := nil;
    HR := FRenderTarget.CreateSolidColorBrush(@BackgroundColor, nil, BackgroundBrush);
    if not Succeeded(HR) then
      Continue;
    for MetricIndex := 0 to High(Metrics) do
    begin
      Rect.Left := Origin.X + Metrics[MetricIndex].Left;
      Rect.Top := Origin.Y + Metrics[MetricIndex].Top;
      Rect.Right := Rect.Left + Metrics[MetricIndex].Width;
      Rect.Bottom := Rect.Top + Metrics[MetricIndex].Height;
      FRenderTarget.FillRectangle(@Rect, BackgroundBrush);
    end;
  end;
end;

procedure TDirectWriteSyntagmView.PaintDirectWrite(DC: HDC; ADpi: Integer);
var
  Background: TD2DColorF;
  Client: TRect;
  HR: HRESULT;
  Origin: TD2DPoint2F;
begin
  FGeometryMatchesPaint := False;
  Client := ClientRect;
  HR := FRenderTarget.BindDC(DC, @Client);
  if not Succeeded(HR) then
  begin
    FBackendError := 'BindDC failed: ' + HResultText(HR);
    PaintFallback;
    Exit;
  end;
  FRenderTarget.SetDpi(ADpi, ADpi);
  Background := ColorToD2D(Color);
  Origin := LayoutOriginDIP(ADpi);
  FRenderTarget.BeginDraw;
  FRenderTarget.Clear(@Background);
  PaintSpanBackgrounds(ADpi);
  FRenderTarget.DrawTextLayout(Origin, FTextLayout, FDefaultBrush,
    D2D1_DRAW_TEXT_OPTIONS_CLIP);
  HR := FRenderTarget.EndDraw(nil, nil);
  if HR = D2DERR_RECREATE_TARGET then
  begin
    DiscardRenderTarget;
    Invalidate;
  end
  else if not Succeeded(HR) then
  begin
    FBackendError := 'EndDraw failed: ' + HResultText(HR);
    DiscardRenderTarget;
  end
  else
    FGeometryMatchesPaint := True;
end;

{$ENDIF}

end.
