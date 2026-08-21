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
  TDirectWriteScriptPosition = (dwspSuperscript, dwspSubscript);

  { DirectWrite has no baseline-offset property on text ranges. Replacing a
    script range with this inline layout gives it explicit ascent and
    baseline metrics while retaining the parent paragraph's logical range. }
  TDirectWriteSuperscriptInline = class(TInterfacedObject,
    IWinDWriteInlineObject)
  private
    FBreakConditionAfter: LongInt;
    FBreakConditionBefore: LongInt;
    FLayout: IWinDWriteTextLayout;
    FLayoutIsRightToLeft: Boolean;
    FMetrics: TDWriteInlineObjectMetrics;
    FRange: TDWriteTextRange;
    FSpanIndex: Integer;
  public
    constructor Create(Factory: IWinDWriteFactory; const Text, FamilyName,
      LocaleName: UnicodeString; BaseFontSize: Single;
      const Style: TVisualTextStyle; Direction: TTextDirection;
      APosition: TDirectWriteScriptPosition; ASpanIndex,
      ABreakConditionBefore, ABreakConditionAfter: Integer;
      out Status: HRESULT);
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
  SUBSCRIPT_DROP = 0.20;

function Succeeded(Status: HRESULT): Boolean; inline;
begin
  Result := Status >= 0;
end;

constructor TDirectWriteSuperscriptInline.Create(Factory: IWinDWriteFactory;
  const Text, FamilyName, LocaleName: UnicodeString; BaseFontSize: Single;
  const Style: TVisualTextStyle; Direction: TTextDirection;
  APosition: TDirectWriteScriptPosition; ASpanIndex,
  ABreakConditionBefore, ABreakConditionAfter: Integer;
  out Status: HRESULT);
var
  FontStyle: LongInt;
  FontWeight: LongInt;
  LineCount: TDWriteUInt32;
  LineMetrics: TDWriteLineMetrics;
  SuperscriptSize: Single;
  TextFormat: IWinDWriteTextFormat;
  TextMetrics: TDWriteTextMetrics;
  TextWidth: Single;
  BaselineOffset: Single;
begin
  inherited Create;
  FSpanIndex := ASpanIndex;
  FBreakConditionBefore := ABreakConditionBefore;
  FBreakConditionAfter := ABreakConditionAfter;
  FLayout := nil;
  FLayoutIsRightToLeft := Direction = tdRightToLeft;
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
  if vtsfStrikethrough in Style.Flags then
    FLayout.SetStrikethrough(True, FRange);

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

  FMetrics.Width := TextMetrics.WidthIncludingTrailingWhitespace;
  if APosition = dwspSuperscript then
  begin
    BaselineOffset := BaseFontSize * SUPERSCRIPT_RISE;
    FMetrics.Height := LineMetrics.Height + BaselineOffset;
    FMetrics.Baseline := LineMetrics.Baseline + BaselineOffset;
  end
  else
  begin
    BaselineOffset := BaseFontSize * SUBSCRIPT_DROP;
    FMetrics.Height := LineMetrics.Height;
    FMetrics.Baseline := Max(Single(0),
      LineMetrics.Baseline - BaselineOffset);
  end;
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
var
  LayoutFormat: IWinDWriteTextFormat;
  RequestedRightToLeft: Boolean;
begin
  if not Assigned(FLayout) or not Assigned(Renderer) then
    Exit(E_FAIL_VALUE);
  RequestedRightToLeft := IsRightToLeft <> False;
  if FLayoutIsRightToLeft <> RequestedRightToLeft then
  begin
    LayoutFormat := FLayout;
    if RequestedRightToLeft then
      Result := LayoutFormat.SetReadingDirection(
        DWRITE_READING_DIRECTION_RIGHT_TO_LEFT)
    else
      Result := LayoutFormat.SetReadingDirection(
        DWRITE_READING_DIRECTION_LEFT_TO_RIGHT);
    if not Succeeded(Result) then
      Exit;
    FLayoutIsRightToLeft := RequestedRightToLeft;
  end;
  { DirectWrite defines OriginX as the inline object's left edge even in an
    RTL run. Synchronize the nested textual layout with the resolved outer
    run, but never mirror or shift its origin as if it were directional
    graphics. The no-wrap layout's width and line metrics remain unchanged. }
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
  BreakConditionBefore := FBreakConditionBefore;
  BreakConditionAfter := FBreakConditionAfter;
  Result := S_OK_VALUE;
end;

end.
