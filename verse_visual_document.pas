unit verse_visual_document;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, text_direction;

type
  TTextDirection = text_direction.TTextDirection;

const
  LTR_ISOLATE: UnicodeChar = #$2066;
  RTL_ISOLATE: UnicodeChar = #$2067;
  POP_DIRECTIONAL_ISOLATE: UnicodeChar = #$2069;
  NO_LOGICAL_INDEX = -1;

type
  TVisualTextStyleFlag = (
    vtsfBold,
    vtsfItalic,
    vtsfUnderline,
    vtsfSuperscript,
    vtsfSelected,
    vtsfAssociated,
    vtsfPointed,
    vtsfStrongHighlight
  );
  TVisualTextStyleFlags = set of TVisualTextStyleFlag;

  { Colors are opaque values interpreted by the view implementation. }
  TVisualTextStyle = record
    Flags: TVisualTextStyleFlags;
    { Point-size adjustment relative to the view's base font. The DirectWrite
      backend converts it to device-independent pixels when building a run. }
    FontSizeDelta: Integer;
    HasForegroundColor: Boolean;
    ForegroundColor: LongInt;
    HasBackgroundColor: Boolean;
    BackgroundColor: LongInt;
  end;

  TVisualSpanKind = (vskText, vskHidden, vskDirectionControl);

  TVisualSpan = record
    LogicalIndex: Integer;
    StartUTF16: Integer;
    LengthUTF16: Integer;
    Kind: TVisualSpanKind;
    Direction: TTextDirection;
    Style: TVisualTextStyle;
    Interactive: Boolean;
    Annotation: UnicodeString;
  end;

  IVerseVisualDocument = interface
    ['{A5D64FAB-68E3-4CCB-A21A-E240580E0B84}']
    function GetText: UnicodeString;
    function GetBaseDirection: TTextDirection;
    function GetRevision: QWord;
    function GetSpanCount: Integer;
    function GetSpan(Index: Integer): TVisualSpan;
    function TryGetUTF16Range(LogicalIndex: Integer; out StartUTF16,
      LengthUTF16: Integer): Boolean;
    function TryGetLogicalIndexAt(UTF16Offset: Integer;
      out LogicalIndex: Integer): Boolean;
    function TryGetInteractiveLogicalIndexAt(UTF16Offset: Integer;
      out LogicalIndex: Integer): Boolean;
    property Text: UnicodeString read GetText;
    property BaseDirection: TTextDirection read GetBaseDirection;
    property Revision: QWord read GetRevision;
    property SpanCount: Integer read GetSpanCount;
    property Spans[Index: Integer]: TVisualSpan read GetSpan;
  end;

  EVerseVisualDocument = class(Exception);

  TVerseVisualDocumentBuilder = class
  private
    FBaseDirection: TTextDirection;
    FText: UnicodeString;
    FSpans: array of TVisualSpan;
    FDirectionStack: array of TTextDirection;
    FLastLogicalIndex: Integer;
    function CurrentDirection: TTextDirection;
    procedure RequireNextLogicalIndex(LogicalIndex: Integer);
    procedure AppendSpan(LogicalIndex: Integer; const Text: UnicodeString;
      Kind: TVisualSpanKind; Direction: TTextDirection;
      const Style: TVisualTextStyle; Interactive: Boolean;
      const Annotation: UnicodeString);
    procedure AppendSyntheticDirectionControl(
      Direction: TTextDirection);
    procedure OpenDirection(LogicalIndex: Integer;
      Direction: TTextDirection);
    procedure CloseDirection(LogicalIndex: Integer;
      ExpectedDirection: TTextDirection);
  public
    constructor Create(BaseDirection: TTextDirection);
    procedure Clear;
    procedure AddText(LogicalIndex: Integer; const Text: UnicodeString;
      const Style: TVisualTextStyle; Interactive: Boolean;
      const Annotation: UnicodeString = '');
    procedure AddMarkup(LogicalIndex: Integer; const Markup: UnicodeString);
    procedure AddHidden(LogicalIndex: Integer);
    function Build(Revision: QWord): IVerseVisualDocument;
    property BaseDirection: TTextDirection read FBaseDirection;
  end;

function DefaultVisualTextStyle: TVisualTextStyle;

implementation

type
  TVerseVisualDocument = class(TInterfacedObject, IVerseVisualDocument)
  private
    FText: UnicodeString;
    FBaseDirection: TTextDirection;
    FRevision: QWord;
    FSpans: array of TVisualSpan;
    function GetText: UnicodeString;
    function GetBaseDirection: TTextDirection;
    function GetRevision: QWord;
    function GetSpanCount: Integer;
    function GetSpan(Index: Integer): TVisualSpan;
  public
    constructor Create(const Text: UnicodeString;
      BaseDirection: TTextDirection; Revision: QWord;
      const Spans: array of TVisualSpan);
    function TryGetUTF16Range(LogicalIndex: Integer; out StartUTF16,
      LengthUTF16: Integer): Boolean;
    function TryGetLogicalIndexAt(UTF16Offset: Integer;
      out LogicalIndex: Integer): Boolean;
    function TryGetInteractiveLogicalIndexAt(UTF16Offset: Integer;
      out LogicalIndex: Integer): Boolean;
  end;

function DefaultVisualTextStyle: TVisualTextStyle;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

{ TVerseVisualDocument }

constructor TVerseVisualDocument.Create(const Text: UnicodeString;
  BaseDirection: TTextDirection; Revision: QWord;
  const Spans: array of TVisualSpan);
var
  Index: Integer;
begin
  inherited Create;
  FText := Text;
  FBaseDirection := BaseDirection;
  FRevision := Revision;
  SetLength(FSpans, Length(Spans));
  for Index := 0 to High(Spans) do
    FSpans[Index] := Spans[Index];
end;

function TVerseVisualDocument.GetText: UnicodeString;
begin
  Result := FText;
end;

function TVerseVisualDocument.GetBaseDirection: TTextDirection;
begin
  Result := FBaseDirection;
end;

function TVerseVisualDocument.GetRevision: QWord;
begin
  Result := FRevision;
end;

function TVerseVisualDocument.GetSpanCount: Integer;
begin
  Result := Length(FSpans);
end;

function TVerseVisualDocument.GetSpan(Index: Integer): TVisualSpan;
begin
  if (Index < 0) or (Index >= Length(FSpans)) then
    raise ERangeError.CreateFmt('Visual span index out of bounds [%d]', [Index]);
  Result := FSpans[Index];
end;

function TVerseVisualDocument.TryGetUTF16Range(LogicalIndex: Integer;
  out StartUTF16, LengthUTF16: Integer): Boolean;
var
  Span: TVisualSpan;
begin
  StartUTF16 := 0;
  LengthUTF16 := 0;
  if LogicalIndex < 0 then
    Exit(False);
  for Span in FSpans do
    if Span.LogicalIndex = LogicalIndex then
    begin
      StartUTF16 := Span.StartUTF16;
      LengthUTF16 := Span.LengthUTF16;
      Exit(True);
    end;
  Result := False;
end;

function TVerseVisualDocument.TryGetLogicalIndexAt(UTF16Offset: Integer;
  out LogicalIndex: Integer): Boolean;
var
  Span: TVisualSpan;
begin
  LogicalIndex := -1;
  if (UTF16Offset < 0) or (UTF16Offset >= Length(FText)) then
    Exit(False);
  for Span in FSpans do
    if (Span.LengthUTF16 > 0) and
       (UTF16Offset >= Span.StartUTF16) and
       (UTF16Offset < Span.StartUTF16 + Span.LengthUTF16) then
    begin
      if Span.LogicalIndex < 0 then
        Exit(False);
      LogicalIndex := Span.LogicalIndex;
      Exit(True);
    end;
  Result := False;
end;

function TVerseVisualDocument.TryGetInteractiveLogicalIndexAt(
  UTF16Offset: Integer; out LogicalIndex: Integer): Boolean;
var
  Span: TVisualSpan;
begin
  LogicalIndex := -1;
  if (UTF16Offset < 0) or (UTF16Offset >= Length(FText)) then
    Exit(False);
  for Span in FSpans do
    if Span.Interactive and (Span.LengthUTF16 > 0) and
       (UTF16Offset >= Span.StartUTF16) and
       (UTF16Offset < Span.StartUTF16 + Span.LengthUTF16) then
    begin
      LogicalIndex := Span.LogicalIndex;
      Exit(True);
    end;
  Result := False;
end;

{ TVerseVisualDocumentBuilder }

constructor TVerseVisualDocumentBuilder.Create(BaseDirection: TTextDirection);
begin
  inherited Create;
  FBaseDirection := BaseDirection;
  Clear;
end;

procedure TVerseVisualDocumentBuilder.Clear;
begin
  FText := '';
  SetLength(FSpans, 0);
  SetLength(FDirectionStack, 0);
  FLastLogicalIndex := -1;
end;

function TVerseVisualDocumentBuilder.CurrentDirection: TTextDirection;
begin
  if Length(FDirectionStack) = 0 then
    Result := FBaseDirection
  else
    Result := FDirectionStack[High(FDirectionStack)];
end;

procedure TVerseVisualDocumentBuilder.RequireNextLogicalIndex(
  LogicalIndex: Integer);
begin
  if LogicalIndex < 0 then
    raise EVerseVisualDocument.CreateFmt(
      'Logical index cannot be negative [%d]', [LogicalIndex]);
  if LogicalIndex <= FLastLogicalIndex then
    raise EVerseVisualDocument.CreateFmt(
      'Logical indexes must be strictly increasing [%d after %d]',
      [LogicalIndex, FLastLogicalIndex]);
  FLastLogicalIndex := LogicalIndex;
end;

procedure TVerseVisualDocumentBuilder.AppendSpan(LogicalIndex: Integer;
  const Text: UnicodeString; Kind: TVisualSpanKind;
  Direction: TTextDirection; const Style: TVisualTextStyle;
  Interactive: Boolean; const Annotation: UnicodeString);
var
  SpanIndex: Integer;
begin
  RequireNextLogicalIndex(LogicalIndex);
  SpanIndex := Length(FSpans);
  SetLength(FSpans, SpanIndex + 1);
  FSpans[SpanIndex].LogicalIndex := LogicalIndex;
  FSpans[SpanIndex].StartUTF16 := Length(FText);
  FSpans[SpanIndex].LengthUTF16 := Length(Text);
  FSpans[SpanIndex].Kind := Kind;
  FSpans[SpanIndex].Direction := Direction;
  FSpans[SpanIndex].Style := Style;
  FSpans[SpanIndex].Interactive := Interactive and (Kind = vskText) and
    (Length(Text) > 0);
  FSpans[SpanIndex].Annotation := Annotation;
  FText := FText + Text;
end;

procedure TVerseVisualDocumentBuilder.AppendSyntheticDirectionControl(
  Direction: TTextDirection);
var
  SpanIndex: Integer;
begin
  SpanIndex := Length(FSpans);
  SetLength(FSpans, SpanIndex + 1);
  FSpans[SpanIndex].LogicalIndex := NO_LOGICAL_INDEX;
  FSpans[SpanIndex].StartUTF16 := Length(FText);
  FSpans[SpanIndex].LengthUTF16 := 1;
  FSpans[SpanIndex].Kind := vskDirectionControl;
  FSpans[SpanIndex].Direction := Direction;
  FSpans[SpanIndex].Style := DefaultVisualTextStyle;
  FSpans[SpanIndex].Interactive := False;
  FSpans[SpanIndex].Annotation := '';
  FText := FText + UnicodeString(POP_DIRECTIONAL_ISOLATE);
end;

procedure TVerseVisualDocumentBuilder.OpenDirection(LogicalIndex: Integer;
  Direction: TTextDirection);
var
  StackIndex: Integer;
  Isolate: UnicodeString;
begin
  if Direction = tdRightToLeft then
    Isolate := UnicodeString(RTL_ISOLATE)
  else
    Isolate := UnicodeString(LTR_ISOLATE);
  AppendSpan(LogicalIndex, Isolate, vskDirectionControl, Direction,
    DefaultVisualTextStyle, False, '');
  StackIndex := Length(FDirectionStack);
  SetLength(FDirectionStack, StackIndex + 1);
  FDirectionStack[StackIndex] := Direction;
end;

procedure TVerseVisualDocumentBuilder.CloseDirection(LogicalIndex: Integer;
  ExpectedDirection: TTextDirection);
var
  PDI: UnicodeString;
begin
  if Length(FDirectionStack) = 0 then
  begin
    AddHidden(LogicalIndex);
    Exit;
  end;
  if CurrentDirection <> ExpectedDirection then
  begin
    AddHidden(LogicalIndex);
    Exit;
  end;
  PDI := UnicodeString(POP_DIRECTIONAL_ISOLATE);
  AppendSpan(LogicalIndex, PDI, vskDirectionControl, ExpectedDirection,
    DefaultVisualTextStyle, False, '');
  SetLength(FDirectionStack, Length(FDirectionStack) - 1);
end;

procedure TVerseVisualDocumentBuilder.AddText(LogicalIndex: Integer;
  const Text: UnicodeString; const Style: TVisualTextStyle;
  Interactive: Boolean; const Annotation: UnicodeString);
begin
  AppendSpan(LogicalIndex, Text, vskText, CurrentDirection, Style,
    Interactive, Annotation);
end;

procedure TVerseVisualDocumentBuilder.AddMarkup(LogicalIndex: Integer;
  const Markup: UnicodeString);
var
  Normalized: string;
begin
  Normalized := LowerCase(Trim(UTF8Encode(Markup)));
  if Normalized = '<rtl>' then
    OpenDirection(LogicalIndex, tdRightToLeft)
  else if Normalized = '<ltr>' then
    OpenDirection(LogicalIndex, tdLeftToRight)
  else if Normalized = '</rtl>' then
    CloseDirection(LogicalIndex, tdRightToLeft)
  else if Normalized = '</ltr>' then
    CloseDirection(LogicalIndex, tdLeftToRight)
  else
    AddHidden(LogicalIndex);
end;

procedure TVerseVisualDocumentBuilder.AddHidden(LogicalIndex: Integer);
begin
  AppendSpan(LogicalIndex, '', vskHidden, CurrentDirection,
    DefaultVisualTextStyle, False, '');
end;

function TVerseVisualDocumentBuilder.Build(
  Revision: QWord): IVerseVisualDocument;
begin
  while Length(FDirectionStack) > 0 do
  begin
    AppendSyntheticDirectionControl(CurrentDirection);
    SetLength(FDirectionStack, Length(FDirectionStack) - 1);
  end;
  Result := TVerseVisualDocument.Create(FText, FBaseDirection, Revision,
    FSpans);
end;

end.
