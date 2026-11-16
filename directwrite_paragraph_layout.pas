unit directwrite_paragraph_layout;

{ A DirectWrite text layout made of one child layout per paragraph, so that
  theWord paragraph formatting (<PI>/<PF> indents and the <CM> gap) can be
  drawn. DirectWrite itself has no paragraph indent, so each paragraph gets its
  own IDWriteTextLayout: narrower by its left indent, drawn shifted to the right
  (to the left for right-to-left text the layout aligns itself), with an
  invisible spacer object in front of the first line for the first-line indent.

  The class implements IWinDWriteTextLayout, so the verse view keeps calling
  the same methods it calls on a single native layout. Text positions, line
  metrics, hit tests and cluster metrics are translated between the document
  and the children; methods the view never calls return E_NOTIMPL. }

{$mode objfpc}{$H+}
{$interfaces COM}

{$IFNDEF MSWINDOWS}
  {$ERROR directwrite_paragraph_layout is available only on Windows}
{$ENDIF}

interface

uses
  Windows, SysUtils, win_directwrite_d2d;

type
  TDWriteParagraphSpec = record
    StartUTF16: Integer;    { first document position of the paragraph }
    LengthUTF16: Integer;   { length including its closing line feed }
    LeftDIP: Single;        { <PIn> indent of every line }
    FirstLineDIP: Single;   { <PFn> extra indent of the first line }
    GapAboveDIP: Single;    { space above the paragraph (<CM>) }
  end;
  TDWriteParagraphSpecArray = array of TDWriteParagraphSpec;

  TDWriteParagraphLayout = class(TInterfacedObject, IWinDWriteTextFormat,
    IWinDWriteTextLayout)
  private
    type
      TChild = record
        Spec: TDWriteParagraphSpec;
        Prefix: Integer;       { 1 when a spacer object leads the child text }
        Layout: IWinDWriteTextLayout;
        OffsetX: Single;
        OffsetY: Single;       { top of the paragraph, gap included }
        Height: Single;        { paragraph height, gap included }
      end;
  private
    FChildren: array of TChild;
    FTextLength: Integer;
    FMaxWidth: Single;
    FRightToLeft: Boolean;
    procedure Restack;
    function ChildAtPosition(APosition: Integer): Integer;
    function ChildAtY(AY: Single): Integer;
    function ToChildRange(AIndex: Integer; const ARange: TDWriteTextRange;
      out AChildRange: TDWriteTextRange): Boolean;
    function ToDocumentPosition(AIndex: Integer; AChildPos: Cardinal): Cardinal;
  public
    constructor Create(const AFactory: IWinDWriteFactory;
      const AFormat: IWinDWriteTextFormat; const AText: UnicodeString;
      const ASpecs: TDWriteParagraphSpecArray; AMaxWidth, AMaxHeight: Single;
      ARightToLeft: Boolean; out AStatus: HRESULT);

    { IWinDWriteTextFormat }
    function SetTextAlignment(Value: LongInt): HRESULT; stdcall;
    function SetParagraphAlignment(Value: LongInt): HRESULT; stdcall;
    function SetWordWrapping(Value: LongInt): HRESULT; stdcall;
    function SetReadingDirection(Value: LongInt): HRESULT; stdcall;
    function SetFlowDirection(Value: LongInt): HRESULT; stdcall;
    procedure _Slot05; stdcall;
    procedure _Slot06; stdcall;
    function SetLineSpacing(Method: LongInt; LineSpacing,
      Baseline: TDWriteFloat): HRESULT; stdcall;
    procedure _Slot08; stdcall;
    procedure _Slot09; stdcall;
    procedure _Slot10; stdcall;
    procedure _Slot11; stdcall;
    procedure _Slot12; stdcall;
    procedure _Slot13; stdcall;
    procedure _Slot14; stdcall;
    procedure _Slot15; stdcall;
    procedure _Slot16; stdcall;
    procedure _Slot17; stdcall;
    procedure _Slot18; stdcall;
    procedure _Slot19; stdcall;
    procedure _Slot20; stdcall;
    procedure _Slot21; stdcall;
    procedure _Slot22; stdcall;
    procedure _Slot23; stdcall;
    procedure _Slot24; stdcall;

    { IWinDWriteTextLayout }
    function SetMaxWidth(Value: Single): HRESULT; stdcall;
    function SetMaxHeight(Value: Single): HRESULT; stdcall;
    procedure _LayoutSlot02; stdcall;
    function SetFontFamilyName(Value: PWideChar;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetFontWeight(Value: LongInt;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetFontStyle(Value: LongInt;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetFontStretch(Value: LongInt;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetFontSize(Value: Single;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetUnderline(Value: TDWriteBool;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetStrikethrough(Value: TDWriteBool;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetDrawingEffect(Effect: IUnknown;
      Range: TDWriteTextRange): HRESULT; stdcall;
    function SetInlineObject(InlineObject: IWinDWriteInlineObject;
      Range: TDWriteTextRange): HRESULT; stdcall;
    procedure _LayoutSlot12; stdcall;
    procedure _LayoutSlot13; stdcall;
    procedure _LayoutSlot14; stdcall;
    procedure _LayoutSlot15; stdcall;
    procedure _LayoutSlot16; stdcall;
    procedure _LayoutSlot17; stdcall;
    procedure _LayoutSlot18; stdcall;
    procedure _LayoutSlot19; stdcall;
    procedure _LayoutSlot20; stdcall;
    procedure _LayoutSlot21; stdcall;
    procedure _LayoutSlot22; stdcall;
    procedure _LayoutSlot23; stdcall;
    procedure _LayoutSlot24; stdcall;
    procedure _LayoutSlot25; stdcall;
    procedure _LayoutSlot26; stdcall;
    procedure _LayoutSlot27; stdcall;
    procedure _LayoutSlot28; stdcall;
    procedure _LayoutSlot29; stdcall;
    function Draw(ClientDrawingContext: Pointer;
      Renderer: IWinDWriteTextRenderer; OriginX, OriginY: Single): HRESULT;
      stdcall;
    function GetLineMetrics(Metrics: PDWriteLineMetrics;
      MaxLineCount: TDWriteUInt32; out ActualLineCount: TDWriteUInt32): HRESULT;
      stdcall;
    function GetMetrics(out Metrics: TDWriteTextMetrics): HRESULT; stdcall;
    function GetOverhangMetrics(
      out Overhangs: TDWriteOverhangMetrics): HRESULT; stdcall;
    function GetClusterMetrics(Metrics: PDWriteClusterMetrics;
      MaxClusterCount: TDWriteUInt32;
      out ActualClusterCount: TDWriteUInt32): HRESULT; stdcall;
    procedure _LayoutSlot35; stdcall;
    function HitTestPoint(PointX, PointY: Single; out IsTrailingHit,
      IsInside: TDWriteBool; out Metrics: TDWriteHitTestMetrics): HRESULT; stdcall;
    function HitTestTextPosition(TextPosition: TDWriteUInt32;
      IsTrailingHit: TDWriteBool; out PointX, PointY: Single;
      out Metrics: TDWriteHitTestMetrics): HRESULT; stdcall;
    function HitTestTextRange(TextPosition, TextLength: TDWriteUInt32;
      OriginX, OriginY: Single; Metrics: PDWriteHitTestMetrics;
      MaxMetricsCount: TDWriteUInt32;
      out ActualMetricsCount: TDWriteUInt32): HRESULT; stdcall;
  end;

implementation

const
  E_NOTIMPL_VALUE = HRESULT($80004001);
  E_FAIL_VALUE = HRESULT($80004005);
  OBJECT_REPLACEMENT = WideChar($FFFC);
  ZERO_WIDTH_SPACE = WideChar($200B);
  BREAK_CONDITION_MAY_NOT_BREAK = 2; { DWRITE_BREAK_CONDITION_MAY_NOT_BREAK }

type
  { Invisible object of a fixed width, used for the first-line indent. }
  TDWriteSpacerInline = class(TInterfacedObject, IWinDWriteInlineObject)
  private
    FWidth: Single;
  public
    constructor Create(AWidth: Single);
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
  end;

function IsOk(Status: HRESULT): Boolean; inline;
begin
  Result := Status >= 0;
end;

{ TDWriteSpacerInline }

constructor TDWriteSpacerInline.Create(AWidth: Single);
begin
  inherited Create;
  FWidth := AWidth;
end;

function TDWriteSpacerInline.Draw(ClientDrawingContext: Pointer;
  Renderer: IWinDWriteTextRenderer; OriginX, OriginY: Single;
  IsSideways, IsRightToLeft: TDWriteBool;
  ClientDrawingEffect: IUnknown): HRESULT; stdcall;
begin
  Result := S_OK;
end;

function TDWriteSpacerInline.GetMetrics(
  out Metrics: TDWriteInlineObjectMetrics): HRESULT; stdcall;
begin
  FillChar(Metrics, SizeOf(Metrics), 0);
  Metrics.Width := FWidth;
  Result := S_OK;
end;

function TDWriteSpacerInline.GetOverhangMetrics(
  out Overhangs: TDWriteOverhangMetrics): HRESULT; stdcall;
begin
  FillChar(Overhangs, SizeOf(Overhangs), 0);
  Result := S_OK;
end;

function TDWriteSpacerInline.GetBreakConditions(out BreakConditionBefore,
  BreakConditionAfter: LongInt): HRESULT; stdcall;
begin
  BreakConditionBefore := DWRITE_BREAK_CONDITION_NEUTRAL;
  { never leave the indent alone on a line of its own }
  BreakConditionAfter := BREAK_CONDITION_MAY_NOT_BREAK;
  Result := S_OK;
end;

{ TDWriteParagraphLayout }

constructor TDWriteParagraphLayout.Create(const AFactory: IWinDWriteFactory;
  const AFormat: IWinDWriteTextFormat; const AText: UnicodeString;
  const ASpecs: TDWriteParagraphSpecArray; AMaxWidth, AMaxHeight: Single;
  ARightToLeft: Boolean; out AStatus: HRESULT);
var
  Index: Integer;
  ChildText: UnicodeString;
  Width: Single;
  Range: TDWriteTextRange;
  Spacer: IWinDWriteInlineObject;
begin
  inherited Create;
  AStatus := E_FAIL_VALUE;
  FTextLength := Length(AText);
  FMaxWidth := AMaxWidth;
  FRightToLeft := ARightToLeft;
  SetLength(FChildren, Length(ASpecs));
  for Index := 0 to High(ASpecs) do
  begin
    FChildren[Index].Spec := ASpecs[Index];
    ChildText := Copy(AText, ASpecs[Index].StartUTF16 + 1,
      ASpecs[Index].LengthUTF16);
    { A trailing line feed would add an empty line to the child; the
      zero-width space keeps the position without the extra line. }
    if (ChildText <> '') and ((ChildText[Length(ChildText)] = #10) or
       (ChildText[Length(ChildText)] = #13)) then
      ChildText[Length(ChildText)] := ZERO_WIDTH_SPACE;
    if ChildText = '' then
      ChildText := ZERO_WIDTH_SPACE;
    if ASpecs[Index].FirstLineDIP > 0 then
    begin
      FChildren[Index].Prefix := 1;
      ChildText := OBJECT_REPLACEMENT + ChildText;
    end
    else
      FChildren[Index].Prefix := 0;
    Width := AMaxWidth - ASpecs[Index].LeftDIP;
    if Width < 1 then
      Width := 1;
    AStatus := AFactory.CreateTextLayout(PWideChar(ChildText), Length(ChildText),
      AFormat, Width, AMaxHeight, FChildren[Index].Layout);
    if not IsOk(AStatus) then
      Exit;
    if FChildren[Index].Prefix = 1 then
    begin
      Spacer := TDWriteSpacerInline.Create(ASpecs[Index].FirstLineDIP);
      Range.StartPosition := 0;
      Range.Length := 1;
      FChildren[Index].Layout.SetInlineObject(Spacer, Range);
    end;
    if ARightToLeft then
      FChildren[Index].OffsetX := 0
    else
      FChildren[Index].OffsetX := ASpecs[Index].LeftDIP;
  end;
  Restack;
  AStatus := S_OK;
end;

procedure TDWriteParagraphLayout.Restack;
var
  Index: Integer;
  Y: Single;
  M: TDWriteTextMetrics;
begin
  Y := 0;
  for Index := 0 to High(FChildren) do
  begin
    FChildren[Index].OffsetY := Y;
    FillChar(M, SizeOf(M), 0);
    if Assigned(FChildren[Index].Layout) then
      FChildren[Index].Layout.GetMetrics(M);
    FChildren[Index].Height := M.Height + FChildren[Index].Spec.GapAboveDIP;
    Y := Y + FChildren[Index].Height;
  end;
end;

function TDWriteParagraphLayout.ChildAtPosition(APosition: Integer): Integer;
var
  Index: Integer;
begin
  Result := High(FChildren);
  for Index := 0 to High(FChildren) do
    if APosition < FChildren[Index].Spec.StartUTF16 +
       FChildren[Index].Spec.LengthUTF16 then
      Exit(Index);
end;

function TDWriteParagraphLayout.ChildAtY(AY: Single): Integer;
var
  Index: Integer;
begin
  Result := High(FChildren);
  for Index := 0 to High(FChildren) do
    if AY < FChildren[Index].OffsetY + FChildren[Index].Height then
      Exit(Index);
end;

function TDWriteParagraphLayout.ToChildRange(AIndex: Integer;
  const ARange: TDWriteTextRange; out AChildRange: TDWriteTextRange): Boolean;
var
  RStart, REnd, CStart, CEnd, S, E: Int64;
begin
  RStart := ARange.StartPosition;
  REnd := RStart + ARange.Length;
  CStart := FChildren[AIndex].Spec.StartUTF16;
  CEnd := CStart + FChildren[AIndex].Spec.LengthUTF16;
  if RStart > CStart then S := RStart else S := CStart;
  if REnd < CEnd then E := REnd else E := CEnd;
  Result := E > S;
  if Result then
  begin
    AChildRange.StartPosition := S - CStart + FChildren[AIndex].Prefix;
    AChildRange.Length := E - S;
  end;
end;

function TDWriteParagraphLayout.ToDocumentPosition(AIndex: Integer;
  AChildPos: Cardinal): Cardinal;
var
  Pos: Int64;
begin
  Pos := Int64(AChildPos) - FChildren[AIndex].Prefix;
  if Pos < 0 then
    Pos := 0;
  Result := FChildren[AIndex].Spec.StartUTF16 + Pos;
end;

{ IWinDWriteTextFormat: forwarded to every child }

function TDWriteParagraphLayout.SetTextAlignment(Value: LongInt): HRESULT; stdcall;
var
  Index: Integer;
begin
  for Index := 0 to High(FChildren) do
    FChildren[Index].Layout.SetTextAlignment(Value);
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetParagraphAlignment(Value: LongInt): HRESULT; stdcall;
var
  Index: Integer;
begin
  for Index := 0 to High(FChildren) do
    FChildren[Index].Layout.SetParagraphAlignment(Value);
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetWordWrapping(Value: LongInt): HRESULT; stdcall;
var
  Index: Integer;
begin
  for Index := 0 to High(FChildren) do
    FChildren[Index].Layout.SetWordWrapping(Value);
  Restack;
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetReadingDirection(Value: LongInt): HRESULT; stdcall;
var
  Index: Integer;
begin
  for Index := 0 to High(FChildren) do
    FChildren[Index].Layout.SetReadingDirection(Value);
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetFlowDirection(Value: LongInt): HRESULT; stdcall;
var
  Index: Integer;
begin
  for Index := 0 to High(FChildren) do
    FChildren[Index].Layout.SetFlowDirection(Value);
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetLineSpacing(Method: LongInt; LineSpacing,
  Baseline: TDWriteFloat): HRESULT; stdcall;
var
  Index: Integer;
  HR: HRESULT;
begin
  Result := S_OK;
  for Index := 0 to High(FChildren) do
  begin
    HR := IWinDWriteTextFormat(FChildren[Index].Layout).SetLineSpacing(Method,
      LineSpacing, Baseline);
    if not IsOk(HR) then
      Result := HR;
  end;
  Restack;
end;

procedure TDWriteParagraphLayout._Slot05; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot06; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot08; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot09; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot10; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot11; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot12; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot13; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot14; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot15; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot16; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot17; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot18; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot19; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot20; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot21; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot22; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot23; stdcall; begin end;
procedure TDWriteParagraphLayout._Slot24; stdcall; begin end;

{ IWinDWriteTextLayout }

function TDWriteParagraphLayout.SetMaxWidth(Value: Single): HRESULT; stdcall;
var
  Index: Integer;
  Width: Single;
begin
  FMaxWidth := Value;
  for Index := 0 to High(FChildren) do
  begin
    Width := Value - FChildren[Index].Spec.LeftDIP;
    if Width < 1 then
      Width := 1;
    FChildren[Index].Layout.SetMaxWidth(Width);
  end;
  Restack;
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetMaxHeight(Value: Single): HRESULT; stdcall;
var
  Index: Integer;
begin
  for Index := 0 to High(FChildren) do
    FChildren[Index].Layout.SetMaxHeight(Value);
  Result := S_OK;
end;

procedure TDWriteParagraphLayout._LayoutSlot02; stdcall; begin end;

function TDWriteParagraphLayout.SetFontFamilyName(Value: PWideChar;
  Range: TDWriteTextRange): HRESULT; stdcall;
var
  Index: Integer;
  R: TDWriteTextRange;
begin
  for Index := 0 to High(FChildren) do
    if ToChildRange(Index, Range, R) then
      FChildren[Index].Layout.SetFontFamilyName(Value, R);
  Restack;
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetFontWeight(Value: LongInt;
  Range: TDWriteTextRange): HRESULT; stdcall;
var
  Index: Integer;
  R: TDWriteTextRange;
begin
  for Index := 0 to High(FChildren) do
    if ToChildRange(Index, Range, R) then
      FChildren[Index].Layout.SetFontWeight(Value, R);
  Restack;
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetFontStyle(Value: LongInt;
  Range: TDWriteTextRange): HRESULT; stdcall;
var
  Index: Integer;
  R: TDWriteTextRange;
begin
  for Index := 0 to High(FChildren) do
    if ToChildRange(Index, Range, R) then
      FChildren[Index].Layout.SetFontStyle(Value, R);
  Restack;
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetFontStretch(Value: LongInt;
  Range: TDWriteTextRange): HRESULT; stdcall;
var
  Index: Integer;
  R: TDWriteTextRange;
begin
  for Index := 0 to High(FChildren) do
    if ToChildRange(Index, Range, R) then
      FChildren[Index].Layout.SetFontStretch(Value, R);
  Restack;
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetFontSize(Value: Single;
  Range: TDWriteTextRange): HRESULT; stdcall;
var
  Index: Integer;
  R: TDWriteTextRange;
begin
  for Index := 0 to High(FChildren) do
    if ToChildRange(Index, Range, R) then
      FChildren[Index].Layout.SetFontSize(Value, R);
  Restack;
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetUnderline(Value: TDWriteBool;
  Range: TDWriteTextRange): HRESULT; stdcall;
var
  Index: Integer;
  R: TDWriteTextRange;
begin
  for Index := 0 to High(FChildren) do
    if ToChildRange(Index, Range, R) then
      FChildren[Index].Layout.SetUnderline(Value, R);
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetStrikethrough(Value: TDWriteBool;
  Range: TDWriteTextRange): HRESULT; stdcall;
var
  Index: Integer;
  R: TDWriteTextRange;
begin
  for Index := 0 to High(FChildren) do
    if ToChildRange(Index, Range, R) then
      FChildren[Index].Layout.SetStrikethrough(Value, R);
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetDrawingEffect(Effect: IUnknown;
  Range: TDWriteTextRange): HRESULT; stdcall;
var
  Index: Integer;
  R: TDWriteTextRange;
begin
  for Index := 0 to High(FChildren) do
    if ToChildRange(Index, Range, R) then
      FChildren[Index].Layout.SetDrawingEffect(Effect, R);
  Result := S_OK;
end;

function TDWriteParagraphLayout.SetInlineObject(
  InlineObject: IWinDWriteInlineObject;
  Range: TDWriteTextRange): HRESULT; stdcall;
var
  Index: Integer;
  R: TDWriteTextRange;
begin
  Result := S_OK;
  for Index := 0 to High(FChildren) do
    if ToChildRange(Index, Range, R) then
    begin
      Result := FChildren[Index].Layout.SetInlineObject(InlineObject, R);
      Break; { an inline object replaces one run; never split it }
    end;
  Restack;
end;

procedure TDWriteParagraphLayout._LayoutSlot12; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot13; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot14; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot15; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot16; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot17; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot18; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot19; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot20; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot21; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot22; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot23; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot24; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot25; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot26; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot27; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot28; stdcall; begin end;
procedure TDWriteParagraphLayout._LayoutSlot29; stdcall; begin end;

function TDWriteParagraphLayout.Draw(ClientDrawingContext: Pointer;
  Renderer: IWinDWriteTextRenderer; OriginX, OriginY: Single): HRESULT; stdcall;
var
  Index: Integer;
  HR: HRESULT;
begin
  Result := S_OK;
  for Index := 0 to High(FChildren) do
  begin
    HR := FChildren[Index].Layout.Draw(ClientDrawingContext, Renderer,
      OriginX + FChildren[Index].OffsetX,
      OriginY + FChildren[Index].OffsetY + FChildren[Index].Spec.GapAboveDIP);
    if not IsOk(HR) then
      Result := HR;
  end;
end;

function TDWriteParagraphLayout.GetLineMetrics(Metrics: PDWriteLineMetrics;
  MaxLineCount: TDWriteUInt32; out ActualLineCount: TDWriteUInt32): HRESULT; stdcall;
var
  Index, LineIndex: Integer;
  Count, Filled: TDWriteUInt32;
  Lines: array of TDWriteLineMetrics;
  All: array of TDWriteLineMetrics;
  Total: Integer;
begin
  SetLength(All, 0);
  Total := 0;
  for Index := 0 to High(FChildren) do
  begin
    Count := 0;
    FChildren[Index].Layout.GetLineMetrics(nil, 0, Count);
    if Count = 0 then
      Continue;
    SetLength(Lines, Count);
    Filled := 0;
    if not IsOk(FChildren[Index].Layout.GetLineMetrics(@Lines[0], Count,
      Filled)) then
      Continue;
    for LineIndex := 0 to Integer(Filled) - 1 do
    begin
      if LineIndex = 0 then
      begin
        { the spacer belongs to no document position; the gap above the
          paragraph is part of its first line box }
        Dec(Lines[0].Length, FChildren[Index].Prefix);
        Lines[0].Height := Lines[0].Height + FChildren[Index].Spec.GapAboveDIP;
        Lines[0].Baseline := Lines[0].Baseline + FChildren[Index].Spec.GapAboveDIP;
      end;
      SetLength(All, Total + 1);
      All[Total] := Lines[LineIndex];
      Inc(Total);
    end;
  end;
  ActualLineCount := Total;
  if (Metrics = nil) or (MaxLineCount < TDWriteUInt32(Total)) then
    Exit(E_NOT_SUFFICIENT_BUFFER);
  if Total > 0 then
    Move(All[0], Metrics^, Total * SizeOf(TDWriteLineMetrics));
  Result := S_OK;
end;

function TDWriteParagraphLayout.GetMetrics(
  out Metrics: TDWriteTextMetrics): HRESULT; stdcall;
var
  Index: Integer;
  M: TDWriteTextMetrics;
  Right: Single;
begin
  FillChar(Metrics, SizeOf(Metrics), 0);
  for Index := 0 to High(FChildren) do
  begin
    FillChar(M, SizeOf(M), 0);
    FChildren[Index].Layout.GetMetrics(M);
    Right := FChildren[Index].OffsetX + M.Left + M.Width;
    if Right > Metrics.Width then
      Metrics.Width := Right;
    Right := FChildren[Index].OffsetX + M.Left + M.WidthIncludingTrailingWhitespace;
    if Right > Metrics.WidthIncludingTrailingWhitespace then
      Metrics.WidthIncludingTrailingWhitespace := Right;
    Inc(Metrics.LineCount, M.LineCount);
    if M.MaxBidiReorderingDepth > Metrics.MaxBidiReorderingDepth then
      Metrics.MaxBidiReorderingDepth := M.MaxBidiReorderingDepth;
    Metrics.LayoutHeight := M.LayoutHeight;
  end;
  if Length(FChildren) > 0 then
    Metrics.Height := FChildren[High(FChildren)].OffsetY +
      FChildren[High(FChildren)].Height;
  Metrics.LayoutWidth := FMaxWidth;
  Result := S_OK;
end;

function TDWriteParagraphLayout.GetOverhangMetrics(
  out Overhangs: TDWriteOverhangMetrics): HRESULT; stdcall;
begin
  FillChar(Overhangs, SizeOf(Overhangs), 0);
  Result := S_OK;
end;

function TDWriteParagraphLayout.GetClusterMetrics(
  Metrics: PDWriteClusterMetrics; MaxClusterCount: TDWriteUInt32;
  out ActualClusterCount: TDWriteUInt32): HRESULT; stdcall;
var
  Index, ClusterIndex, Total, Skip: Integer;
  Count, Filled: TDWriteUInt32;
  Clusters: array of TDWriteClusterMetrics;
  All: array of TDWriteClusterMetrics;
begin
  SetLength(All, 0);
  Total := 0;
  for Index := 0 to High(FChildren) do
  begin
    Count := 0;
    FChildren[Index].Layout.GetClusterMetrics(nil, 0, Count);
    if Count = 0 then
      Continue;
    SetLength(Clusters, Count);
    Filled := 0;
    if not IsOk(FChildren[Index].Layout.GetClusterMetrics(@Clusters[0], Count,
      Filled)) then
      Continue;
    Skip := FChildren[Index].Prefix; { the spacer is one cluster of length 1 }
    for ClusterIndex := Skip to Integer(Filled) - 1 do
    begin
      SetLength(All, Total + 1);
      All[Total] := Clusters[ClusterIndex];
      Inc(Total);
    end;
  end;
  ActualClusterCount := Total;
  if (Metrics = nil) or (MaxClusterCount < TDWriteUInt32(Total)) then
    Exit(E_NOT_SUFFICIENT_BUFFER);
  if Total > 0 then
    Move(All[0], Metrics^, Total * SizeOf(TDWriteClusterMetrics));
  Result := S_OK;
end;

procedure TDWriteParagraphLayout._LayoutSlot35; stdcall; begin end;

function TDWriteParagraphLayout.HitTestPoint(PointX, PointY: Single;
  out IsTrailingHit, IsInside: TDWriteBool;
  out Metrics: TDWriteHitTestMetrics): HRESULT; stdcall;
var
  Index: Integer;
  LocalY: Single;
begin
  FillChar(Metrics, SizeOf(Metrics), 0);
  IsTrailingHit := False;
  IsInside := False;
  if Length(FChildren) = 0 then
    Exit(E_FAIL_VALUE);
  Index := ChildAtY(PointY);
  LocalY := PointY - FChildren[Index].OffsetY - FChildren[Index].Spec.GapAboveDIP;
  if LocalY < 0 then
    LocalY := 0;
  Result := FChildren[Index].Layout.HitTestPoint(
    PointX - FChildren[Index].OffsetX, LocalY, IsTrailingHit, IsInside, Metrics);
  if not IsOk(Result) then
    Exit;
  if (FChildren[Index].Prefix = 1) and (Metrics.TextPosition = 0) then
  begin
    { a click on the indent lands before the first character }
    IsTrailingHit := False;
    Metrics.TextPosition := 1;
    if Metrics.Length > 1 then
      Dec(Metrics.Length)
    else
      Metrics.Length := 1;
  end;
  Metrics.TextPosition := ToDocumentPosition(Index, Metrics.TextPosition);
  Metrics.Left := Metrics.Left + FChildren[Index].OffsetX;
  Metrics.Top := Metrics.Top + FChildren[Index].OffsetY +
    FChildren[Index].Spec.GapAboveDIP;
  if (PointY < FChildren[Index].OffsetY) or
     (PointY >= FChildren[Index].OffsetY + FChildren[Index].Height) then
    IsInside := False;
end;

function TDWriteParagraphLayout.HitTestTextPosition(TextPosition: TDWriteUInt32;
  IsTrailingHit: TDWriteBool; out PointX, PointY: Single;
  out Metrics: TDWriteHitTestMetrics): HRESULT; stdcall;
var
  Index: Integer;
  ChildPos: Int64;
begin
  PointX := 0;
  PointY := 0;
  FillChar(Metrics, SizeOf(Metrics), 0);
  if Length(FChildren) = 0 then
    Exit(E_FAIL_VALUE);
  Index := ChildAtPosition(TextPosition);
  ChildPos := Int64(TextPosition) - FChildren[Index].Spec.StartUTF16 +
    FChildren[Index].Prefix;
  if ChildPos < 0 then
    ChildPos := 0;
  Result := FChildren[Index].Layout.HitTestTextPosition(ChildPos, IsTrailingHit,
    PointX, PointY, Metrics);
  if not IsOk(Result) then
    Exit;
  PointX := PointX + FChildren[Index].OffsetX;
  PointY := PointY + FChildren[Index].OffsetY + FChildren[Index].Spec.GapAboveDIP;
  Metrics.TextPosition := ToDocumentPosition(Index, Metrics.TextPosition);
  Metrics.Left := Metrics.Left + FChildren[Index].OffsetX;
  Metrics.Top := Metrics.Top + FChildren[Index].OffsetY +
    FChildren[Index].Spec.GapAboveDIP;
end;

function TDWriteParagraphLayout.HitTestTextRange(TextPosition,
  TextLength: TDWriteUInt32; OriginX, OriginY: Single;
  Metrics: PDWriteHitTestMetrics; MaxMetricsCount: TDWriteUInt32;
  out ActualMetricsCount: TDWriteUInt32): HRESULT; stdcall;
var
  Index, Item, Total: Integer;
  Range, R: TDWriteTextRange;
  Count, Filled: TDWriteUInt32;
  Part: array of TDWriteHitTestMetrics;
  All: array of TDWriteHitTestMetrics;
  HR: HRESULT;
begin
  Range.StartPosition := TextPosition;
  Range.Length := TextLength;
  SetLength(All, 0);
  Total := 0;
  for Index := 0 to High(FChildren) do
  begin
    if not ToChildRange(Index, Range, R) then
      Continue;
    Count := 0;
    HR := FChildren[Index].Layout.HitTestTextRange(R.StartPosition, R.Length,
      0, 0, nil, 0, Count);
    if (HR <> E_NOT_SUFFICIENT_BUFFER) and not IsOk(HR) then
      Continue;
    if Count = 0 then
      Continue;
    SetLength(Part, Count);
    Filled := 0;
    if not IsOk(FChildren[Index].Layout.HitTestTextRange(R.StartPosition,
      R.Length, 0, 0, @Part[0], Count, Filled)) then
      Continue;
    for Item := 0 to Integer(Filled) - 1 do
    begin
      Part[Item].TextPosition := ToDocumentPosition(Index, Part[Item].TextPosition);
      Part[Item].Left := Part[Item].Left + OriginX + FChildren[Index].OffsetX;
      Part[Item].Top := Part[Item].Top + OriginY + FChildren[Index].OffsetY +
        FChildren[Index].Spec.GapAboveDIP;
      SetLength(All, Total + 1);
      All[Total] := Part[Item];
      Inc(Total);
    end;
  end;
  ActualMetricsCount := Total;
  if (Metrics = nil) or (MaxMetricsCount < TDWriteUInt32(Total)) then
    Exit(E_NOT_SUFFICIENT_BUFFER);
  if Total > 0 then
    Move(All[0], Metrics^, Total * SizeOf(TDWriteHitTestMetrics));
  Result := S_OK;
end;

end.
