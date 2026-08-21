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

  TVerseResolveInsertionStyleEvent = function(Sender: TObject;
    LayoutOffsetUTF16: Integer; out Style: TVisualTextStyle): Boolean of object;

  {$IFDEF MSWINDOWS}
  TDWriteHitTestMetricsArray = array of TDWriteHitTestMetrics;
  TDWriteLineMetricsArray = array of TDWriteLineMetrics;
  {$ENDIF}
  TVerseTextOffsetArray = array of Integer;


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
    FRecoveringInputGeometry: Boolean;
    FOnContentHeightChanged: TNotifyEvent;
    FOnResolveInsertionStyle: TVerseResolveInsertionStyleEvent;
    FOnVerseDoubleClick: TNotifyEvent;
    FOnVerseMouseDown: TVerseMouseDownEvent;
    FOnVerseHover: TVerseHitEvent;
    {$IFDEF MSWINDOWS}
    FD2DFactory: IWinD2D1Factory;
    FBaseCaretBaseline: Single;
    FBaseCaretDescent: Single;
    FBaseFontSizeDIP: Single;
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
    FScriptEffects: array of IUnknown;
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
    function EnsureInputGeometry: Boolean;
    procedure InvalidateLayout;
    procedure PaintFallback;
    procedure PaintLogicalTags;
    procedure NotifyContentHeightChanged;
    procedure SetDisplayTags(Value: Boolean);
    procedure SetLayoutPadding(Value: Integer);
    function ResolveInsertionStyle(UTF16Offset: Integer;
      out Style: TVisualTextStyle): Boolean;
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
    { Editor geometry works with layout UTF-16 offsets instead of Syntagm
      indexes. It intentionally accepts whitespace and the nearest position
      outside the ink bounds. }
    function HitTestTextOffset(const ClientPoint: TPoint;
      out UTF16Offset: Integer; out IsTrailing, IsInside: Boolean): Boolean;
    function GetTextPositionClientRect(UTF16Offset: Integer;
      out CaretRect: TRect): Boolean;
    function GetTextLineClientRect(UTF16Offset: Integer;
      out LineRect: TRect): Boolean;
    function GetTextRangeClientRects(StartUTF16, LengthUTF16: Integer;
      out Rects: TRectArray): Boolean;
    function GetVisualObjectClientRect(Role: TVisualSpanRole;
      ObjectId: QWord; out ObjectRect: TRect): Boolean;
    function GetCaretStops(out Stops: TVerseTextOffsetArray): Boolean;
    function GetLogicalRects(LogicalIndex: Integer;
      out Rects: TRectArray): Boolean;
    function TryGetLogicalScreenAnchor(LogicalIndex: Integer;
      out Anchor: TPoint): Boolean;

    property BackendAvailable: Boolean read GetBackendAvailable;
    property BackendError: UnicodeString read GetBackendError;
    property BackendName: string read GetBackendName;
    property ContentHeight: Integer read GetContentHeight;
    property OnResolveInsertionStyle: TVerseResolveInsertionStyleEvent
      read FOnResolveInsertionStyle write FOnResolveInsertionStyle;
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
  , directwrite_script_renderer, directwrite_superscript_inline
  {$ENDIF};

const
  E_NOINTERFACE_VALUE = LongInt(-2147467262); // $80004002

function Succeeded(HR: LongInt): Boolean; inline;
begin
  Result := HR >= 0;
end;

{$IFDEF MSWINDOWS}
function GetLayoutLineMetrics(const Layout: IWinDWriteTextLayout;
  out Metrics: TDWriteLineMetricsArray): Boolean;
var
  ActualCount: TDWriteUInt32;
  FilledCount: TDWriteUInt32;
  HR: HRESULT;
begin
  SetLength(Metrics, 0);
  Result := False;
  if not Assigned(Layout) then
    Exit;
  ActualCount := 0;
  HR := Layout.GetLineMetrics(nil, 0, ActualCount);
  if (HR <> E_NOT_SUFFICIENT_BUFFER) and not Succeeded(HR) then
    Exit;
  if (ActualCount = 0) or (ActualCount > 1048576) then
    Exit;
  SetLength(Metrics, ActualCount);
  FilledCount := 0;
  HR := Layout.GetLineMetrics(@Metrics[0], ActualCount, FilledCount);
  if not Succeeded(HR) then
  begin
    SetLength(Metrics, 0);
    Exit;
  end;
  SetLength(Metrics, FilledCount);
  Result := FilledCount > 0;
end;

function GetLayoutLineBox(const Layout: IWinDWriteTextLayout;
  TextPosition: Integer; out LineTop, LineHeight,
  LineBaseline: Single): Boolean;
var
  Index, Position: Integer;
  Metrics: TDWriteLineMetricsArray;
begin
  Result := False;
  LineTop := 0;
  LineHeight := 0;
  LineBaseline := 0;
  if not GetLayoutLineMetrics(Layout, Metrics) then
    Exit;
  Position := 0;
  for Index := 0 to High(Metrics) do
  begin
    if (TextPosition < Position + Integer(Metrics[Index].Length)) or
       (Index = High(Metrics)) then
    begin
      LineHeight := Metrics[Index].Height;
      LineBaseline := Metrics[Index].Baseline;
      Exit(True);
    end;
    Inc(Position, Metrics[Index].Length);
    LineTop := LineTop + Metrics[Index].Height;
  end;
end;

function MeasureTextFormatLineMetrics(const Factory: IWinDWriteFactory;
  const TextFormat: IWinDWriteTextFormat; const ProbeText: UnicodeString;
  FontSize: Single;
  Bold, Italic: Boolean; out Baseline, Descent, TopOverhang,
  BottomOverhang: Single): Boolean;
var
  HR: HRESULT;
  Index: Integer;
  Layout: IWinDWriteTextLayout;
  Metrics: TDWriteLineMetricsArray;
  Overhangs: TDWriteOverhangMetrics;
  Probe: UnicodeString;
  Range: TDWriteTextRange;
  TextMetrics: TDWriteTextMetrics;
begin
  Baseline := 0;
  Descent := 0;
  TopOverhang := 0;
  BottomOverhang := 0;
  Result := False;
  if not Assigned(Factory) or not Assigned(TextFormat) then
    Exit;
  Probe := ProbeText;
  if Probe = '' then
    Probe := 'Mg';
  Layout := nil;
  HR := Factory.CreateTextLayout(PWideChar(Probe), Length(Probe),
    TextFormat, 1048576, 1048576, Layout);
  if not Succeeded(HR) or not Assigned(Layout) then
    Exit;
  Range.StartPosition := 0;
  Range.Length := Length(Probe);
  HR := Layout.SetFontSize(Max(Single(1), FontSize), Range);
  if not Succeeded(HR) then
    Exit;
  if Bold then
  begin
    HR := Layout.SetFontWeight(700, Range);
    if not Succeeded(HR) then
      Exit;
  end;
  if Italic then
  begin
    HR := Layout.SetFontStyle(2, Range);
    if not Succeeded(HR) then
      Exit;
  end;
  FillChar(TextMetrics, SizeOf(TextMetrics), 0);
  HR := Layout.GetMetrics(TextMetrics);
  if not Succeeded(HR) then
    Exit;
  { GetOverhangMetrics reports ink outside the layout box. Tighten the probe
    to its measured content first; the initial effectively-infinite height
    would otherwise hide any bottom overhang. The extra horizontal DIP avoids
    introducing a wrap at a floating-point boundary. }
  HR := Layout.SetMaxWidth(Max(Single(1),
    TextMetrics.WidthIncludingTrailingWhitespace + 1));
  if not Succeeded(HR) then
    Exit;
  HR := Layout.SetMaxHeight(Max(Single(1), TextMetrics.Height));
  if not Succeeded(HR) then
    Exit;
  if not GetLayoutLineMetrics(Layout, Metrics) or (Length(Metrics) = 0) then
    Exit;
  for Index := 0 to High(Metrics) do
  begin
    Baseline := Max(Baseline, Metrics[Index].Baseline);
    Descent := Max(Descent,
      Metrics[Index].Height - Metrics[Index].Baseline);
  end;
  FillChar(Overhangs, SizeOf(Overhangs), 0);
  HR := Layout.GetOverhangMetrics(Overhangs);
  if not Succeeded(HR) then
    Exit;
  { Keep the sign. A negative overhang means the visible ink stays inside the
    typographic box, so it lets the caller reserve less than the whole em box;
    a positive value extends the ink beyond that box. }
  TopOverhang := Overhangs.Top;
  BottomOverhang := Overhangs.Bottom;
  Result := (Baseline > 0) and (Descent >= 0);
end;
{$ENDIF}

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
  FRecoveringInputGeometry := False;
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

function TDirectWriteSyntagmView.ResolveInsertionStyle(
  UTF16Offset: Integer; out Style: TVisualTextStyle): Boolean;
var
  Index: Integer;
  Span: TVisualSpan;
begin
  Style := DefaultVisualTextStyle;
  if Assigned(FOnResolveInsertionStyle) and
     FOnResolveInsertionStyle(Self, UTF16Offset, Style) then
    Exit(True);
  if not Assigned(FDocument) then
    Exit(False);

  { Standalone views have no domain resolver. Prefer the following editable
    character at a boundary; the composite supplies the authoritative
    strict-interior insertion style through OnResolveInsertionStyle. }
  for Index := 0 to FDocument.SpanCount - 1 do
  begin
    Span := FDocument.Spans[Index];
    if Span.Interactive and (Span.Role = vsrContent) and
       (Span.LengthUTF16 > 0) and
       (UTF16Offset >= Span.StartUTF16) and
       (UTF16Offset < Span.StartUTF16 + Span.LengthUTF16) then
    begin
      Style := Span.Style;
      Exit(True);
    end;
  end;
  for Index := 0 to FDocument.SpanCount - 1 do
  begin
    Span := FDocument.Spans[Index];
    if Span.Interactive and (Span.Role = vsrContent) and
       (Span.LengthUTF16 > 0) and (Span.StartUTF16 >= UTF16Offset) then
    begin
      Style := Span.Style;
      Exit(True);
    end;
  end;
  for Index := FDocument.SpanCount - 1 downto 0 do
  begin
    Span := FDocument.Spans[Index];
    if Span.Interactive and (Span.Role = vsrContent) and
       (Span.LengthUTF16 > 0) and
       (Span.StartUTF16 + Span.LengthUTF16 <= UTF16Offset) then
    begin
      Style := Span.Style;
      Exit(True);
    end;
  end;
  Result := False;
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
  SetLength(FScriptEffects, 0);
  FDrawingEffectsReady := False;
  FillChar(FTextMetrics, SizeOf(FTextMetrics), 0);
  FBaseCaretBaseline := 0;
  FBaseCaretDescent := 0;
  FBaseFontSizeDIP := 0;
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
  { Auto-height changes can happen from the tail of Paint. On Win32 that
    resize does not invariably leave another WM_PAINT pending, so without an
    explicit invalidation hit testing can remain disabled until the window is
    activated again. }
  Invalidate;
end;

function TDirectWriteSyntagmView.EnsureInputGeometry: Boolean;
const
  MaxPaintAttempts = 2;
var
  Attempt: Integer;
begin
  Result := False;
  if not Assigned(FDocument) then
    Exit;
  {$IFDEF MSWINDOWS}
  if not EnsureLayout then
    Exit;
  if FGeometryMatchesPaint then
    Exit(True);

  { Layout/document/style changes deliberately invalidate hit-test geometry.
    Usually the queued paint refreshes it. When an auto-height resize occurs
    while Paint is unwinding, however, Windows can coalesce that paint away.
    Recover locally on the first input instead of requiring an activation
    change (Alt+Tab). Two bounded passes cover the paint -> auto-height resize
    -> repaint sequence without allowing recursive painting. }
  if FRecoveringInputGeometry or IsProcessingPaintMsg or
     not HandleAllocated or not IsVisible then
    Exit;
  FRecoveringInputGeometry := True;
  try
    for Attempt := 1 to MaxPaintAttempts do
    begin
      Repaint;
      if FGeometryMatchesPaint then
        Break;
    end;
  finally
    FRecoveringInputGeometry := False;
  end;
  Result := EnsureLayout and FGeometryMatchesPaint;
  {$ENDIF}
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
  Span: TVisualSpan;
  Metrics: TDWriteHitTestMetrics;
  Origin: TD2DPoint2F;
  PointX: Single;
  PointY: Single;
{$ENDIF}
begin
  FillChar(Hit, SizeOf(Hit), 0);
  Hit.LogicalIndex := -1;
  Hit.UTF16Offset := -1;
  if not Assigned(FDocument) or not EnsureInputGeometry then
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
  if not FDocument.TryGetSpanAt(Metrics.TextPosition, Span) or
     not (((Span.Role = vsrContent) and Span.Interactive) or
          ((Span.Role = vsrTranslatorNote) and
           (Span.ObjectId <> 0))) then
    Exit(False);
  Hit.LogicalIndex := Span.LogicalIndex;
  Hit.UTF16Offset := Metrics.TextPosition;
  Hit.Trailing := IsTrailing;
  Hit.Role := Span.Role;
  Hit.ObjectId := Span.ObjectId;
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
  if not Assigned(FDocument) or not EnsureInputGeometry then
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
  SetLength(FScriptEffects, 0);
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
const
  MIN_FONT_SIZE_DIP = 1.0;
  SUPERSCRIPT_RISE = 0.30;
  SUBSCRIPT_DROP = 0.20;
var
  BaseFontSize: Single;
  BaseFontStyle: LongInt;
  BaseFontWeight: LongInt;
  BaseBottomOverhang: Single;
  BaseTopOverhang: Single;
  FamilyName: UnicodeString;
  ExtraBottom: Single;
  ExtraTop: Single;
  HasContentScript: Boolean;
  HR: HRESULT;
  Index: Integer;
  LineIndex: Integer;
  LineMetrics: TDWriteLineMetricsArray;
  LineSpacing: Single;
  LayoutFormat: IWinDWriteTextFormat;
  MaxScriptBottomExtent: Single;
  MaxScriptTopExtent: Single;
  NaturalBaseline: Single;
  NaturalDescent: Single;
  ScriptBaseline: Single;
  ScriptBottomOverhang: Single;
  ScriptCaretBaseline: Single;
  ScriptCaretDescent: Single;
  ScriptDescent: Single;
  ScriptFontSize: Single;
  ScriptBottomExtent: Single;
  ScriptTopOverhang: Single;
  ScriptTopExtent: Single;
  Superscript: TDirectWriteSuperscriptInline;
  SuperscriptInterface: IWinDWriteInlineObject;
  SuperscriptStatus: HRESULT;
  SuperscriptText: UnicodeString;
  ScriptPosition: TDirectWriteScriptPosition;
  LayoutHeight: Single;
  LayoutWidth: Single;
  LocaleName: UnicodeString;
  Range: TDWriteTextRange;
  Span: TVisualSpan;
  SpanFontSize: Single;
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

  HasContentScript := False;
  MaxScriptTopExtent := 0;
  MaxScriptBottomExtent := 0;
  FBaseFontSizeDIP := BaseFontSize;
  if not MeasureTextFormatLineMetrics(FDWriteFactory, FTextFormat, 'Mg',
    BaseFontSize, False, False, FBaseCaretBaseline,
    FBaseCaretDescent, BaseTopOverhang, BaseBottomOverhang) then
  begin
    FBaseCaretBaseline := BaseFontSize * 0.8;
    FBaseCaretDescent := BaseFontSize * 0.2;
  end;
  for Index := 0 to FDocument.SpanCount - 1 do
  begin
    Span := FDocument.Spans[Index];
    if (Span.LengthUTF16 <= 0) or (Span.StartUTF16 < 0) or
       (Span.StartUTF16 + Span.LengthUTF16 > Length(DocumentText)) then
      Continue;
    Range.StartPosition := Span.StartUTF16;
    Range.Length := Span.LengthUTF16;
    SpanFontSize := Max(Single(MIN_FONT_SIZE_DIP), BaseFontSize +
      Span.Style.FontSizeDelta * (96.0 / 72.0));
    if Span.Style.FontSizeDelta <> 0 then
      FTextLayout.SetFontSize(SpanFontSize, Range);
    if vtsfBold in Span.Style.Flags then
      FTextLayout.SetFontWeight(700, Range);
    if vtsfItalic in Span.Style.Flags then
      FTextLayout.SetFontStyle(2, Range);
    if vtsfUnderline in Span.Style.Flags then
      FTextLayout.SetUnderline(True, Range);
    if vtsfStrikethrough in Span.Style.Flags then
      FTextLayout.SetStrikethrough(True, Range);
    if vtsfSuperscript in Span.Style.Flags then
    begin
      ScriptFontSize := Max(Single(MIN_FONT_SIZE_DIP), SpanFontSize * 0.72);
      FTextLayout.SetFontSize(ScriptFontSize, Range);
      if Span.Role = vsrContent then
      begin
        HasContentScript := True;
        ScriptCaretBaseline := FBaseCaretBaseline *
          (ScriptFontSize / BaseFontSize);
        ScriptCaretDescent := FBaseCaretDescent *
          (ScriptFontSize / BaseFontSize);
        if not MeasureTextFormatLineMetrics(FDWriteFactory, FTextFormat,
          Copy(DocumentText, Span.StartUTF16 + 1, Span.LengthUTF16),
          ScriptFontSize, vtsfBold in Span.Style.Flags,
          vtsfItalic in Span.Style.Flags, ScriptBaseline,
          ScriptDescent, ScriptTopOverhang,
          ScriptBottomOverhang) then
        begin
          ScriptBaseline := FBaseCaretBaseline *
            (ScriptFontSize / BaseFontSize);
          ScriptDescent := FBaseCaretDescent *
            (ScriptFontSize / BaseFontSize);
          ScriptTopOverhang := 0;
          ScriptBottomOverhang := 0;
        end;
        { Reserve whichever is taller: actual glyph ink (including positive
          overhang) or the adaptive caret's typographic box. This keeps the
          caret inside the line without returning to the old full-size script
          reservation. It also protects decorations, which DirectWrite does
          not include in text overhang metrics. }
        ScriptTopExtent := Max(ScriptCaretBaseline, Max(Single(0),
          ScriptBaseline + ScriptTopOverhang));
        MaxScriptTopExtent := Max(MaxScriptTopExtent, ScriptTopExtent +
          SpanFontSize * SUPERSCRIPT_RISE);
        ScriptBottomExtent := Max(Single(0), Max(ScriptCaretDescent,
          ScriptDescent + ScriptBottomOverhang) -
          SpanFontSize * SUPERSCRIPT_RISE);
        MaxScriptBottomExtent := Max(MaxScriptBottomExtent,
          ScriptBottomExtent);
      end;
    end
    else if vtsfSubscript in Span.Style.Flags then
    begin
      ScriptFontSize := Max(Single(MIN_FONT_SIZE_DIP), SpanFontSize * 0.72);
      FTextLayout.SetFontSize(ScriptFontSize, Range);
      if Span.Role = vsrContent then
      begin
        HasContentScript := True;
        ScriptCaretBaseline := FBaseCaretBaseline *
          (ScriptFontSize / BaseFontSize);
        ScriptCaretDescent := FBaseCaretDescent *
          (ScriptFontSize / BaseFontSize);
        if not MeasureTextFormatLineMetrics(FDWriteFactory, FTextFormat,
          Copy(DocumentText, Span.StartUTF16 + 1, Span.LengthUTF16),
          ScriptFontSize, vtsfBold in Span.Style.Flags,
          vtsfItalic in Span.Style.Flags, ScriptBaseline,
          ScriptDescent, ScriptTopOverhang,
          ScriptBottomOverhang) then
        begin
          ScriptBaseline := FBaseCaretBaseline *
            (ScriptFontSize / BaseFontSize);
          ScriptDescent := FBaseCaretDescent *
            (ScriptFontSize / BaseFontSize);
          ScriptTopOverhang := 0;
          ScriptBottomOverhang := 0;
        end;
        ScriptBottomExtent := Max(ScriptCaretDescent, Max(Single(0),
          ScriptDescent + ScriptBottomOverhang));
        ScriptTopExtent := Max(Single(0), Max(ScriptCaretBaseline,
          ScriptBaseline + ScriptTopOverhang) -
          SpanFontSize * SUBSCRIPT_DROP);
        MaxScriptTopExtent := Max(MaxScriptTopExtent, ScriptTopExtent);
        MaxScriptBottomExtent := Max(MaxScriptBottomExtent,
          ScriptBottomExtent + SpanFontSize * SUBSCRIPT_DROP);
      end;
    end;
  end;

  for Index := 0 to FDocument.SpanCount - 1 do
  begin
    Span := FDocument.Spans[Index];
    if (Span.LengthUTF16 <= 0) or (Span.StartUTF16 < 0) or
       (Span.StartUTF16 + Span.LengthUTF16 > Length(DocumentText)) or
       not ((vtsfSuperscript in Span.Style.Flags) or
         (vtsfSubscript in Span.Style.Flags)) or
       (Span.Role = vsrContent) then
      Continue;
    Range.StartPosition := Span.StartUTF16;
    Range.Length := Span.LengthUTF16;
    SpanFontSize := Max(Single(MIN_FONT_SIZE_DIP), BaseFontSize +
      Span.Style.FontSizeDelta * (96.0 / 72.0));
    if (vtsfSuperscript in Span.Style.Flags) or
       (vtsfSubscript in Span.Style.Flags) then
    begin
      if vtsfSuperscript in Span.Style.Flags then
        ScriptPosition := dwspSuperscript
      else
        ScriptPosition := dwspSubscript;
      SuperscriptText := Copy(DocumentText, Span.StartUTF16 + 1,
        Span.LengthUTF16);
      Superscript := TDirectWriteSuperscriptInline.Create(FDWriteFactory,
        SuperscriptText, FamilyName, LocaleName, SpanFontSize, Span.Style,
        Span.Direction, ScriptPosition, Index,
        DWRITE_BREAK_CONDITION_NEUTRAL, DWRITE_BREAK_CONDITION_NEUTRAL,
        SuperscriptStatus);
      if Succeeded(SuperscriptStatus) then
      begin
        SuperscriptInterface := Superscript;
        SuperscriptStatus := FTextLayout.SetInlineObject(
          SuperscriptInterface, Range);
        if Succeeded(SuperscriptStatus) then
          FSuperscriptObjects.Add(Superscript)
        else
          SuperscriptInterface := nil;
      end
      else
        Superscript.Free;
    end;
  end;

  { Baseline offsets happen only at paint time, so native line metrics need
    explicit headroom. Measure after installing inline note markers because
    they can change both wrapping and natural ascent/descent. Uniform spacing
    keeps caret geometry and ContentHeight consistent with the final ink. }
  if HasContentScript then
  begin
    NaturalBaseline := 0;
    NaturalDescent := 0;
    if GetLayoutLineMetrics(FTextLayout, LineMetrics) then
      for LineIndex := 0 to High(LineMetrics) do
      begin
        NaturalBaseline := Max(NaturalBaseline,
          LineMetrics[LineIndex].Baseline);
        NaturalDescent := Max(NaturalDescent,
          LineMetrics[LineIndex].Height - LineMetrics[LineIndex].Baseline);
      end;
    NaturalBaseline := Max(NaturalBaseline, FBaseCaretBaseline);
    NaturalDescent := Max(NaturalDescent, FBaseCaretDescent);
    if (NaturalBaseline <= 0) or (NaturalDescent < 0) then
    begin
      NaturalBaseline := BaseFontSize * 0.8;
      NaturalDescent := BaseFontSize * 0.2;
    end;
    ExtraTop := Max(Single(0), MaxScriptTopExtent - NaturalBaseline);
    ExtraBottom := Max(Single(0), MaxScriptBottomExtent - NaturalDescent);
    { Even when the shifted ink fits within a normal line box, a line that
      contains only 72%-sized script has smaller native metrics. Install the
      base strut for every scripted document so such lines cannot overlap or
      shrink ContentHeight; ExtraTop/ExtraBottom remain only the net overhang. }
    LineSpacing := NaturalBaseline + NaturalDescent + ExtraTop + ExtraBottom;
    LayoutFormat := FTextLayout;
    HR := LayoutFormat.SetLineSpacing(DWRITE_LINE_SPACING_METHOD_UNIFORM,
      LineSpacing, NaturalBaseline + ExtraTop);
    if not Succeeded(HR) then
    begin
      FBackendError := 'SetLineSpacing failed: ' + HResultText(HR);
      FTextLayout := nil;
      Exit;
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
const
  SUPERSCRIPT_RISE = 0.30;
  SUBSCRIPT_DROP = 0.20;
var
  BaseFontSize: Single;
  BaselineOffset: Single;
  DrawingEffect: IUnknown;
  EffectBrush: IWinD2D1Brush;
  RangeBrush: IWinD2D1SolidColorBrush;
  D2DColor: TD2DColorF;
  HR: HRESULT;
  Index: Integer;
  IsScriptContent: Boolean;
  Range: TDWriteTextRange;
  ScriptEffect: IWinDWriteScriptEffect;
  Span: TVisualSpan;
  SpanFontSize: Single;
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
  BaseFontSize := Font.Size * 96 / 72;
  if BaseFontSize <= 0 then
    BaseFontSize := 13.333;
  SetLength(FRangeBrushes, 0);
  SetLength(FScriptEffects, 0);
  for Index := 0 to FDocument.SpanCount - 1 do
  begin
    Span := FDocument.Spans[Index];
    if Span.LengthUTF16 <= 0 then
      Continue;
    IsScriptContent := (Span.Role = vsrContent) and
      ((vtsfSuperscript in Span.Style.Flags) or
       (vtsfSubscript in Span.Style.Flags));
    if not Span.Style.HasForegroundColor and not IsScriptContent then
      Continue;
    EffectBrush := FDefaultBrush;
    if Span.Style.HasForegroundColor then
    begin
      D2DColor := ColorToD2D(TColor(Span.Style.ForegroundColor));
      RangeBrush := nil;
      HR := FRenderTarget.CreateSolidColorBrush(@D2DColor, nil, RangeBrush);
      if not Succeeded(HR) then
      begin
        FBackendError := 'Range brush creation failed: ' + HResultText(HR);
        Exit;
      end;
      SetLength(FRangeBrushes, Length(FRangeBrushes) + 1);
      FRangeBrushes[High(FRangeBrushes)] := RangeBrush;
      EffectBrush := RangeBrush;
    end;
    Range.StartPosition := Span.StartUTF16;
    Range.Length := Span.LengthUTF16;
    DrawingEffect := EffectBrush;
    if (Span.Role = vsrContent) and
       ((vtsfSuperscript in Span.Style.Flags) or
        (vtsfSubscript in Span.Style.Flags)) then
    begin
      SpanFontSize := Max(Single(1), BaseFontSize +
        Span.Style.FontSizeDelta * (96.0 / 72.0));
      if vtsfSuperscript in Span.Style.Flags then
        BaselineOffset := -SpanFontSize * SUPERSCRIPT_RISE
      else
        BaselineOffset := SpanFontSize * SUBSCRIPT_DROP;
      ScriptEffect := CreateDirectWriteScriptEffect(EffectBrush,
        BaselineOffset);
      DrawingEffect := ScriptEffect;
      SetLength(FScriptEffects, Length(FScriptEffects) + 1);
      FScriptEffects[High(FScriptEffects)] := ScriptEffect;
    end;
    HR := FTextLayout.SetDrawingEffect(DrawingEffect, Range);
    if not Succeeded(HR) then
    begin
      FBackendError := 'SetDrawingEffect failed: ' + HResultText(HR);
      Exit;
    end;
    if Span.Style.HasForegroundColor then
      for SuperscriptIndex := 0 to FSuperscriptObjects.Count - 1 do
    begin
      Superscript := TDirectWriteSuperscriptInline(
        FSuperscriptObjects[SuperscriptIndex]);
      if Superscript.SpanIndex = Index then
        Superscript.SetDrawingEffect(EffectBrush);
    end;
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
  LayoutHR: HRESULT;
  Origin: TD2DPoint2F;
  Renderer: IWinDWriteTextRenderer;
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
  Renderer := TDirectWriteScriptRenderer.Create(FRenderTarget,
    FDefaultBrush);
  LayoutHR := FTextLayout.Draw(nil, Renderer, Origin.X, Origin.Y);
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
  else if not Succeeded(LayoutHR) then
  begin
    FBackendError := 'TextLayout.Draw failed: ' + HResultText(LayoutHR);
  end
  else
    FGeometryMatchesPaint := True;
end;

{$ENDIF}

function TDirectWriteSyntagmView.HitTestTextOffset(
  const ClientPoint: TPoint; out UTF16Offset: Integer;
  out IsTrailing, IsInside: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  Dpi: Integer;
  HR: HRESULT;
  NativeInside: TDWriteBool;
  NativeTrailing: TDWriteBool;
  Metrics: TDWriteHitTestMetrics;
  Origin: TD2DPoint2F;
  PointX: Single;
  PointY: Single;
  TextLength: Integer;
{$ENDIF}
begin
  UTF16Offset := 0;
  IsTrailing := False;
  IsInside := False;
  if not Assigned(FDocument) then
    Exit(False);
  {$IFDEF MSWINDOWS}
  if not EnsureLayout then
    Exit(False);
  Dpi := FLayoutDpi;
  if Dpi <= 0 then
    Dpi := 96;
  Origin := LayoutOriginDIP(Dpi);
  PointX := ClientPoint.X * 96 / Dpi - Origin.X;
  PointY := ClientPoint.Y * 96 / Dpi - Origin.Y;
  FillChar(Metrics, SizeOf(Metrics), 0);
  HR := FTextLayout.HitTestPoint(PointX, PointY, NativeTrailing,
    NativeInside, Metrics);
  if not Succeeded(HR) then
    Exit(False);

  UTF16Offset := Metrics.TextPosition;
  if NativeTrailing then
    Inc(UTF16Offset, Metrics.Length);
  TextLength := Length(FDocument.Text);
  if UTF16Offset < 0 then
    UTF16Offset := 0
  else if UTF16Offset > TextLength then
    UTF16Offset := TextLength;
  IsTrailing := NativeTrailing;
  IsInside := NativeInside;
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TDirectWriteSyntagmView.GetTextPositionClientRect(
  UTF16Offset: Integer; out CaretRect: TRect): Boolean;
{$IFDEF MSWINDOWS}
const
  MIN_FONT_SIZE_DIP = 1.0;
  SCRIPT_SCALE = 0.72;
  SUPERSCRIPT_RISE = 0.30;
  SUBSCRIPT_DROP = 0.20;
var
  BaselineOffset: Single;
  CaretBaseline: Single;
  CaretDescent: Single;
  Dpi: Integer;
  HR: HRESULT;
  LineBaseline: Single;
  LineHeight: Single;
  LineTop: Single;
  Metrics: TDWriteHitTestMetrics;
  Origin: TD2DPoint2F;
  PointX: Single;
  PointY: Single;
  QueryPosition: Integer;
  QueryTrailing: TDWriteBool;
  Scale: Single;
  SpanFontSize: Single;
  Style: TVisualTextStyle;
  TextLength: Integer;
  VisualBaseline: Single;
{$ENDIF}
begin
  FillChar(CaretRect, SizeOf(CaretRect), 0);
  if not Assigned(FDocument) then
    Exit(False);
  {$IFDEF MSWINDOWS}
  if not EnsureLayout then
    Exit(False);
  Dpi := FLayoutDpi;
  if Dpi <= 0 then
    Dpi := 96;
  TextLength := Length(FDocument.Text);
  if UTF16Offset < 0 then
    UTF16Offset := 0
  else if UTF16Offset > TextLength then
    UTF16Offset := TextLength;

  Origin := LayoutOriginDIP(Dpi);
  if TextLength = 0 then
  begin
    CaretRect.Left := Round(Origin.X * Dpi / 96);
    CaretRect.Top := Round(Origin.Y * Dpi / 96);
    CaretRect.Right := CaretRect.Left + Max(1, Round(Dpi / 96));
    CaretRect.Bottom := CaretRect.Top +
      Max(1, Canvas.TextHeight('Mg'));
    Exit(True);
  end;

  QueryPosition := UTF16Offset;
  QueryTrailing := False;
  if QueryPosition = TextLength then
  begin
    QueryPosition := TextLength - 1;
    QueryTrailing := True;
  end;
  FillChar(Metrics, SizeOf(Metrics), 0);
  HR := FTextLayout.HitTestTextPosition(QueryPosition, QueryTrailing,
    PointX, PointY, Metrics);
  if not Succeeded(HR) then
    Exit(False);

  if not GetLayoutLineBox(FTextLayout, UTF16Offset, LineTop, LineHeight,
    LineBaseline) then
  begin
    LineTop := Metrics.Top;
    LineHeight := Metrics.Height;
    LineBaseline := Metrics.Height * 0.8;
  end;

  Style := DefaultVisualTextStyle;
  ResolveInsertionStyle(UTF16Offset, Style);
  SpanFontSize := Max(Single(MIN_FONT_SIZE_DIP), FBaseFontSizeDIP +
    Style.FontSizeDelta * (96.0 / 72.0));
  if FBaseFontSizeDIP > 0 then
    Scale := SpanFontSize / FBaseFontSizeDIP
  else
    Scale := 1;
  BaselineOffset := 0;
  if vtsfSuperscript in Style.Flags then
  begin
    Scale := Scale * SCRIPT_SCALE;
    BaselineOffset := -SpanFontSize * SUPERSCRIPT_RISE;
  end
  else if vtsfSubscript in Style.Flags then
  begin
    Scale := Scale * SCRIPT_SCALE;
    BaselineOffset := SpanFontSize * SUBSCRIPT_DROP;
  end;
  CaretBaseline := FBaseCaretBaseline * Scale;
  CaretDescent := FBaseCaretDescent * Scale;
  if (CaretBaseline <= 0) or (CaretDescent < 0) then
  begin
    if (vtsfSuperscript in Style.Flags) or
       (vtsfSubscript in Style.Flags) then
      SpanFontSize := SpanFontSize * SCRIPT_SCALE;
    CaretBaseline := Max(Single(1), SpanFontSize * 0.8);
    CaretDescent := Max(Single(0), SpanFontSize * 0.2);
  end;
  VisualBaseline := LineTop + LineBaseline + BaselineOffset;
  CaretRect.Left := Round((Origin.X + PointX) * Dpi / 96);
  CaretRect.Top := Round((Origin.Y + VisualBaseline - CaretBaseline) *
    Dpi / 96);
  CaretRect.Right := CaretRect.Left + Max(1, Round(Dpi / 96));
  CaretRect.Bottom := Round((Origin.Y + VisualBaseline + CaretDescent) *
    Dpi / 96);
  if CaretRect.Bottom <= CaretRect.Top then
    CaretRect.Bottom := CaretRect.Top + 1;
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TDirectWriteSyntagmView.GetTextLineClientRect(
  UTF16Offset: Integer; out LineRect: TRect): Boolean;
{$IFDEF MSWINDOWS}
var
  Dpi: Integer;
  LineBaseline: Single;
  LineHeight: Single;
  LineTop: Single;
  Origin: TD2DPoint2F;
  TextLength: Integer;
{$ENDIF}
begin
  FillChar(LineRect, SizeOf(LineRect), 0);
  if not Assigned(FDocument) then
    Exit(False);
  {$IFDEF MSWINDOWS}
  if not EnsureLayout then
    Exit(False);
  Dpi := FLayoutDpi;
  if Dpi <= 0 then
    Dpi := 96;
  TextLength := Length(FDocument.Text);
  if UTF16Offset < 0 then
    UTF16Offset := 0
  else if UTF16Offset > TextLength then
    UTF16Offset := TextLength;
  Origin := LayoutOriginDIP(Dpi);
  if TextLength = 0 then
  begin
    LineRect.Left := Round(Origin.X * Dpi / 96);
    LineRect.Top := Round(Origin.Y * Dpi / 96);
    LineRect.Right := Max(LineRect.Left + 1,
      ClientWidth - FLayoutPadding);
    LineRect.Bottom := LineRect.Top + Max(1, Canvas.TextHeight('Mg'));
    Exit(True);
  end;
  if not GetLayoutLineBox(FTextLayout, UTF16Offset, LineTop,
    LineHeight, LineBaseline) then
    Exit(False);
  LineRect.Left := Round(Origin.X * Dpi / 96);
  LineRect.Top := Round((Origin.Y + LineTop) * Dpi / 96);
  LineRect.Right := Max(LineRect.Left + 1,
    ClientWidth - FLayoutPadding);
  LineRect.Bottom := LineRect.Top + Max(1,
    Ceil(LineHeight * Dpi / 96));
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TDirectWriteSyntagmView.GetTextRangeClientRects(
  StartUTF16, LengthUTF16: Integer; out Rects: TRectArray): Boolean;
{$IFDEF MSWINDOWS}
var
  Dpi: Integer;
  HitMetrics: TDWriteHitTestMetricsArray;
  Index: Integer;
  Origin: TD2DPoint2F;
  TextLength: Integer;
{$ENDIF}
begin
  SetLength(Rects, 0);
  if not Assigned(FDocument) then
    Exit(False);
  {$IFDEF MSWINDOWS}
  if not EnsureLayout then
    Exit(False);
  TextLength := Length(FDocument.Text);
  if StartUTF16 < 0 then
    StartUTF16 := 0;
  if StartUTF16 > TextLength then
    StartUTF16 := TextLength;
  if LengthUTF16 > TextLength - StartUTF16 then
    LengthUTF16 := TextLength - StartUTF16;
  if (LengthUTF16 <= 0) or
     not GetHitTestMetrics(StartUTF16, LengthUTF16, HitMetrics) then
    Exit(False);

  Dpi := FLayoutDpi;
  if Dpi <= 0 then
    Dpi := 96;
  Origin := LayoutOriginDIP(Dpi);
  SetLength(Rects, Length(HitMetrics));
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

function TDirectWriteSyntagmView.GetVisualObjectClientRect(
  Role: TVisualSpanRole; ObjectId: QWord; out ObjectRect: TRect): Boolean;
var
  Index: Integer;
  RectIndex: Integer;
  Rects: TRectArray;
  Span: TVisualSpan;
begin
  FillChar(ObjectRect, SizeOf(ObjectRect), 0);
  Result := False;
  if not Assigned(FDocument) or (Role = vsrContent) then
    Exit;
  for Index := 0 to FDocument.SpanCount - 1 do
  begin
    Span := FDocument.Spans[Index];
    if (Span.Role <> Role) or (Span.ObjectId <> ObjectId) or
       (Span.LengthUTF16 <= 0) then
      Continue;
    if not GetTextRangeClientRects(Span.StartUTF16, Span.LengthUTF16,
      Rects) or (Length(Rects) = 0) then
      Exit;
    { A mixed-direction inline range may expose a zero-width boundary metric
      before its actual object metric. Use the widest rectangle from that
      atomic, no-wrap range instead of making the boundary its click anchor. }
    ObjectRect := Rects[0];
    for RectIndex := 1 to High(Rects) do
      if Rects[RectIndex].Right - Rects[RectIndex].Left >
         ObjectRect.Right - ObjectRect.Left then
        ObjectRect := Rects[RectIndex];
    Exit(True);
  end;
end;

function TDirectWriteSyntagmView.GetCaretStops(
  out Stops: TVerseTextOffsetArray): Boolean;
{$IFDEF MSWINDOWS}
type
  TClusterMetricsArray = array of TDWriteClusterMetrics;
var
  ActualCount: TDWriteUInt32;
  ClusterIndex: Integer;
  FilledCount: TDWriteUInt32;
  HR: HRESULT;
  Metrics: TClusterMetricsArray;
  Position: Integer;
  StopCount: Integer;
  TextLength: Integer;
{$ENDIF}
begin
  SetLength(Stops, 0);
  if not Assigned(FDocument) then
    Exit(False);
  {$IFDEF MSWINDOWS}
  if not EnsureLayout then
    Exit(False);
  TextLength := Length(FDocument.Text);
  ActualCount := 0;
  HR := FTextLayout.GetClusterMetrics(nil, 0, ActualCount);
  if (HR <> E_NOT_SUFFICIENT_BUFFER) and not Succeeded(HR) then
    Exit(False);

  if ActualCount = 0 then
  begin
    SetLength(Stops, 1);
    Stops[0] := 0;
    Exit(True);
  end;
  if ActualCount > 1048576 then
    Exit(False);

  SetLength(Metrics, ActualCount);
  FilledCount := 0;
  HR := FTextLayout.GetClusterMetrics(@Metrics[0], ActualCount,
    FilledCount);
  if not Succeeded(HR) then
    Exit(False);
  SetLength(Metrics, FilledCount);

  SetLength(Stops, Length(Metrics) + 2);
  Stops[0] := 0;
  StopCount := 1;
  Position := 0;
  for ClusterIndex := 0 to High(Metrics) do
  begin
    Inc(Position, Metrics[ClusterIndex].Length);
    if Position > TextLength then
      Position := TextLength;
    if Stops[StopCount - 1] <> Position then
    begin
      Stops[StopCount] := Position;
      Inc(StopCount);
    end;
  end;
  if Stops[StopCount - 1] <> TextLength then
  begin
    Stops[StopCount] := TextLength;
    Inc(StopCount);
  end;
  SetLength(Stops, StopCount);
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

end.
