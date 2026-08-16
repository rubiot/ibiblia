unit directwrite_superscript_inline;

{$mode objfpc}{$H+}
{$interfaces COM}
{$codepage utf8}

{$IFNDEF MSWINDOWS}
  {$ERROR directwrite_superscript_inline is available only on Windows}
{$ENDIF}

interface

uses
  Windows, SysUtils, Math, win_directwrite_d2d, verse_visual_document,
  text_direction;

type
  { DirectWrite has no baseline-offset property on text ranges. Replacing a
    superscript range with this inline layout gives it explicit ascent and
    baseline metrics while retaining the parent paragraph's logical range. }
  TDirectWriteSuperscriptInline = class(TInterfacedObject,
    IWinDWriteInlineObject)
  private
    FLayout: IWinDWriteTextLayout;
    FMetrics: TDWriteInlineObjectMetrics;
    FRange: TDWriteTextRange;
    FSpanIndex: Integer;
  public
    constructor Create(Factory: IWinDWriteFactory; const Text, FamilyName,
      LocaleName: UnicodeString; BaseFontSize: Single;
      const Style: TVisualTextStyle; Direction: TTextDirection;
      ASpanIndex: Integer; out Status: HRESULT);
    procedure SetDrawingEffect(Effect: IUnknown);

    function Draw(ClientDrawingContext: Pointer;
      Renderer: IWinDWriteTextRenderer; OriginX, OriginY: Single;
      IsSideways, IsRightToLeft: TDWriteBool;
      ClientDrawingEffect: IUnknown): HRESULT; stdcall;
    function GetMetrics(out Metrics: TDWriteInlineObjectMetrics): HRESULT;
      stdcall;
    function GetOverhangMetrics(out Overhangs: TDWriteOverhangMetrics): HRESULT;
      stdcall;
    function GetBreakConditions(out BreakConditionBefore,
      BreakConditionAfter: LongInt): HRESULT; stdcall;

    property SpanIndex: Integer read FSpanIndex;
  end;

implementation

const
  S_OK_VALUE = HRESULT(0);
  E_FAIL_VALUE = HRESULT(-2147467259); // $80004005
  SUPERSCRIPT_SCALE = 0.72;
  SUPERSCRIPT_RISE = 0.30;

function Succeeded(Status: HRESULT): Boolean; inline;
begin
  Result := Status >= 0;
end;

constructor TDirectWriteSuperscriptInline.Create(Factory: IWinDWriteFactory;
  const Text, FamilyName, LocaleName: UnicodeString; BaseFontSize: Single;
  const Style: TVisualTextStyle; Direction: TTextDirection;
  ASpanIndex: Integer; out Status: HRESULT);
var
  FontStyle: LongInt;
  FontWeight: LongInt;
  LineCount: TDWriteUInt32;
  LineMetrics: TDWriteLineMetrics;
  SuperscriptSize: Single;
  TextFormat: IWinDWriteTextFormat;
  TextMetrics: TDWriteTextMetrics;
  TextWidth: Single;
  Rise: Single;
begin
  inherited Create;
  FSpanIndex := ASpanIndex;
  FLayout := nil;
  FillChar(FMetrics, SizeOf(FMetrics), 0);
  FillChar(FRange, SizeOf(FRange), 0);
  Status := E_FAIL_VALUE;
  if not Assigned(Factory) or (Text = '') then
    Exit;

  SuperscriptSize := BaseFontSize * SUPERSCRIPT_SCALE;
  if SuperscriptSize <= 0 then
    SuperscriptSize := 9.6;
  FontWeight := DWRITE_FONT_WEIGHT_NORMAL;
  if vtsfBold in Style.Flags then
    FontWeight := 700;
  FontStyle := DWRITE_FONT_STYLE_NORMAL;
  if vtsfItalic in Style.Flags then
    FontStyle := 2;

  Status := Factory.CreateTextFormat(PWideChar(FamilyName), nil, FontWeight,
    FontStyle, DWRITE_FONT_STRETCH_NORMAL, SuperscriptSize,
    PWideChar(LocaleName), TextFormat);
  if not Succeeded(Status) then
    Exit;
  TextFormat.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
  TextFormat.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
  TextFormat.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);
  if Direction = tdRightToLeft then
    TextFormat.SetReadingDirection(DWRITE_READING_DIRECTION_RIGHT_TO_LEFT)
  else
    TextFormat.SetReadingDirection(DWRITE_READING_DIRECTION_LEFT_TO_RIGHT);
  TextFormat.SetFlowDirection(DWRITE_FLOW_DIRECTION_TOP_TO_BOTTOM);

  Status := Factory.CreateTextLayout(PWideChar(Text), Length(Text), TextFormat,
    1048576, 1048576, FLayout);
  if not Succeeded(Status) then
  begin
    FLayout := nil;
    Exit;
  end;
  FRange.StartPosition := 0;
  FRange.Length := Length(Text);
  if vtsfUnderline in Style.Flags then
    FLayout.SetUnderline(True, FRange);

  Status := FLayout.GetMetrics(TextMetrics);
  if not Succeeded(Status) then
  begin
    FLayout := nil;
    Exit;
  end;
  TextWidth := Max(Single(1), TextMetrics.WidthIncludingTrailingWhitespace);
  FLayout.SetMaxWidth(TextWidth);
  Status := FLayout.GetMetrics(TextMetrics);
  if not Succeeded(Status) then
  begin
    FLayout := nil;
    Exit;
  end;

  FillChar(LineMetrics, SizeOf(LineMetrics), 0);
  LineCount := 0;
  Status := FLayout.GetLineMetrics(@LineMetrics, 1, LineCount);
  if not Succeeded(Status) or (LineCount = 0) then
  begin
    LineMetrics.Height := TextMetrics.Height;
    LineMetrics.Baseline := TextMetrics.Height * 0.8;
  end;

  Rise := BaseFontSize * SUPERSCRIPT_RISE;
  FMetrics.Width := TextMetrics.WidthIncludingTrailingWhitespace;
  FMetrics.Height := LineMetrics.Height + Rise;
  FMetrics.Baseline := LineMetrics.Baseline + Rise;
  FMetrics.SupportsSideways := False;
  Status := S_OK_VALUE;
end;

procedure TDirectWriteSuperscriptInline.SetDrawingEffect(Effect: IUnknown);
begin
  if Assigned(FLayout) then
    FLayout.SetDrawingEffect(Effect, FRange);
end;

function TDirectWriteSuperscriptInline.Draw(ClientDrawingContext: Pointer;
  Renderer: IWinDWriteTextRenderer; OriginX, OriginY: Single;
  IsSideways, IsRightToLeft: TDWriteBool;
  ClientDrawingEffect: IUnknown): HRESULT; stdcall;
begin
  if not Assigned(FLayout) or not Assigned(Renderer) then
    Exit(E_FAIL_VALUE);
  Result := FLayout.Draw(ClientDrawingContext, Renderer, OriginX, OriginY);
end;

function TDirectWriteSuperscriptInline.GetMetrics(
  out Metrics: TDWriteInlineObjectMetrics): HRESULT; stdcall;
begin
  Metrics := FMetrics;
  Result := S_OK_VALUE;
end;

function TDirectWriteSuperscriptInline.GetOverhangMetrics(
  out Overhangs: TDWriteOverhangMetrics): HRESULT; stdcall;
begin
  FillChar(Overhangs, SizeOf(Overhangs), 0);
  Result := S_OK_VALUE;
end;

function TDirectWriteSuperscriptInline.GetBreakConditions(
  out BreakConditionBefore, BreakConditionAfter: LongInt): HRESULT; stdcall;
begin
  BreakConditionBefore := DWRITE_BREAK_CONDITION_NEUTRAL;
  BreakConditionAfter := DWRITE_BREAK_CONDITION_NEUTRAL;
  Result := S_OK_VALUE;
end;

end.
