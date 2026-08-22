unit theword_clipboard_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, theword_ont_codec_v2;

const
  THEWORD_CLIPBOARD_FORMAT_V2 =
    'iBiblia.TheWord.SemanticVerseFragment.v1';
  THEWORD_CLIPBOARD_SCHEMA_V2 = 'ibiblia.theword-semantic-fragment';
  THEWORD_CLIPBOARD_VERSION_V1 = 1;
  THEWORD_CLIPBOARD_VERSION_V2 = 2;

type
  ETheWordClipboardV2 = class(Exception);

  TTheWordClipboardObjectKindV2 = (
    twcokLexical,
    twcokOpaque,
    twcokStructural
  );

  TTheWordClipboardMarkV2 = record
    Kind: TTheWordSemanticKind;
    StartOffset: Integer;
    EndOffset: Integer;
  end;

  TTheWordClipboardObjectV2 = record
    ObjectKind: TTheWordClipboardObjectKindV2;
    SubKind: Integer;
    Offset: Integer;
    RawLexeme: UnicodeString;
    SourceOrder: Integer;
  end;

  TTheWordClipboardFontMarkV2 = record
    StartOffset: Integer;
    EndOffset: Integer;
    OpenRaw: UnicodeString;
    CloseRaw: UnicodeString;
    OpenSourceOrder: Integer;
    CloseSourceOrder: Integer;
  end;

  TTheWordClipboardMarkArrayV2 = array of TTheWordClipboardMarkV2;
  TTheWordClipboardObjectArrayV2 = array of TTheWordClipboardObjectV2;
  TTheWordClipboardFontMarkArrayV2 = array of TTheWordClipboardFontMarkV2;

  { This is a transport object. Offsets are relative UTF-16 code-unit
    offsets into Text. }
  TTheWordClipboardFragmentV2 = class
  private
    FText: UnicodeString;
    FMarks: TTheWordClipboardMarkArrayV2;
    FObjects: TTheWordClipboardObjectArrayV2;
    FFontMarks: TTheWordClipboardFontMarkArrayV2;
    { Zero means locally built/current. One/two preserve wire provenance. }
    FDecodedVersion: Integer;
    function GetMark(AIndex: Integer): TTheWordClipboardMarkV2;
    function GetMarkCount: Integer;
    function GetObject(AIndex: Integer): TTheWordClipboardObjectV2;
    function GetObjectCount: Integer;
    function GetFontMark(AIndex: Integer): TTheWordClipboardFontMarkV2;
    function GetFontMarkCount: Integer;
    procedure AddMark(const AValue: TTheWordClipboardMarkV2);
    procedure AddObject(const AValue: TTheWordClipboardObjectV2);
    procedure AddFontMark(const AValue: TTheWordClipboardFontMarkV2);
    procedure ClearObjects;
  public
    property Text: UnicodeString read FText;
    property MarkCount: Integer read GetMarkCount;
    property Marks[AIndex: Integer]: TTheWordClipboardMarkV2 read GetMark;
    property FontMarkCount: Integer read GetFontMarkCount;
    property FontMarks[AIndex: Integer]: TTheWordClipboardFontMarkV2
      read GetFontMark;
    property ObjectCount: Integer read GetObjectCount;
    property Objects[AIndex: Integer]: TTheWordClipboardObjectV2
      read GetObject;
  end;

  TTheWordClipboardCodecV2 = class
  public
    class function EncodeSelection(ADocument: TTheWordVerseDocument;
      AStartUTF16, ALengthUTF16: Integer; out APlainText: UnicodeString;
      out APayload: string): Boolean; static;
    class function TryDecodePayload(const APayload: string;
      out AFragment: TTheWordClipboardFragmentV2): Boolean; static;
    class function NewPlainFragment(const AText: UnicodeString):
      TTheWordClipboardFragmentV2; static;
    class function BuildFragmentRaw(AFragment: TTheWordClipboardFragmentV2;
      out ARaw: UnicodeString): Boolean; static;
    class function BuildPasteRaw(ADocument: TTheWordVerseDocument;
      AStartUTF16, ALengthUTF16: Integer;
      AFragment: TTheWordClipboardFragmentV2;
      out ARaw: UnicodeString): Boolean; static;
    class function BuildCutRaw(ADocument: TTheWordVerseDocument;
      AStartUTF16, ALengthUTF16: Integer;
      out ARaw: UnicodeString): Boolean; static;
  end;

function NormalizeTheWordClipboardText(
  const AValue: UnicodeString): UnicodeString;

implementation

uses
  Math, fpjson, jsonparser, theword_inline_format_v2;

const
  MAX_CLIPBOARD_PAYLOAD_BYTES = 16 * 1024 * 1024;
  MAX_CLIPBOARD_ITEMS = 10000;
  MAX_CLIPBOARD_TEXT_UTF16 = 8 * 1024 * 1024;
  MAX_CLIPBOARD_RAW_UTF16 = 64 * 1024;

type
  TBooleanArray = array of Boolean;
  TIntegerArray = array of Integer;
  TClipboardOrderEventKind = (coekObject, coekFontOpen, coekFontClose);
  TClipboardOrderEventV2 = record
    Kind: TClipboardOrderEventKind;
    ObjectIndex: Integer;
    FontIndex: Integer;
    Offset: Integer;
    SourceOrder: Integer;
  end;
  TClipboardOrderEventArray = array of TClipboardOrderEventV2;
  TClipboardCrossRangeKind = (ccrkFont, ccrkSemantic);
  TClipboardCrossRangeV2 = record
    Id: Integer;
    Kind: TClipboardCrossRangeKind;
    StartOffset: Integer;
    EndOffset: Integer;
  end;
  TClipboardCrossRangeArray = array of TClipboardCrossRangeV2;

function IsHighSurrogate(AValue: WideChar): Boolean;
begin
  Result := (Ord(AValue) >= $D800) and (Ord(AValue) <= $DBFF);
end;

function IsLowSurrogate(AValue: WideChar): Boolean;
begin
  Result := (Ord(AValue) >= $DC00) and (Ord(AValue) <= $DFFF);
end;

function IsUtf16Boundary(const AText: UnicodeString;
  AOffset: Integer): Boolean;
begin
  Result := (AOffset >= 0) and (AOffset <= Length(AText));
  if Result and (AOffset > 0) and (AOffset < Length(AText)) then
    Result := not (IsHighSurrogate(AText[AOffset]) and
      IsLowSurrogate(AText[AOffset + 1]));
end;

function IsValidRange(const AText: UnicodeString; AStartUTF16,
  ALengthUTF16: Integer): Boolean;
begin
  Result := (AStartUTF16 >= 0) and (ALengthUTF16 >= 0) and
    (AStartUTF16 <= Length(AText)) and
    (ALengthUTF16 <= Length(AText) - AStartUTF16) and
    IsUtf16Boundary(AText, AStartUTF16) and
    IsUtf16Boundary(AText, AStartUTF16 + ALengthUTF16);
end;

function NormalizeTheWordClipboardText(
  const AValue: UnicodeString): UnicodeString;
var
  Index: Integer;
begin
  Result := '';
  Index := 1;
  while Index <= Length(AValue) do
  begin
    if AValue[Index] = #13 then
    begin
      Result := Result + ' ';
      if (Index < Length(AValue)) and (AValue[Index + 1] = #10) then
        Inc(Index);
    end
    else if AValue[Index] = #10 then
      Result := Result + ' '
    else
      Result := Result + AValue[Index];
    Inc(Index);
  end;
end;

function TTheWordClipboardFragmentV2.GetMark(
  AIndex: Integer): TTheWordClipboardMarkV2;
begin
  Result := FMarks[AIndex];
end;

function TTheWordClipboardFragmentV2.GetMarkCount: Integer;
begin
  Result := Length(FMarks);
end;

function TTheWordClipboardFragmentV2.GetObject(
  AIndex: Integer): TTheWordClipboardObjectV2;
begin
  Result := FObjects[AIndex];
end;

function TTheWordClipboardFragmentV2.GetObjectCount: Integer;
begin
  Result := Length(FObjects);
end;

function TTheWordClipboardFragmentV2.GetFontMark(
  AIndex: Integer): TTheWordClipboardFontMarkV2;
begin
  Result := FFontMarks[AIndex];
end;

function TTheWordClipboardFragmentV2.GetFontMarkCount: Integer;
begin
  Result := Length(FFontMarks);
end;

procedure TTheWordClipboardFragmentV2.AddMark(
  const AValue: TTheWordClipboardMarkV2);
begin
  SetLength(FMarks, Length(FMarks) + 1);
  FMarks[High(FMarks)] := AValue;
end;

procedure TTheWordClipboardFragmentV2.AddObject(
  const AValue: TTheWordClipboardObjectV2);
begin
  SetLength(FObjects, Length(FObjects) + 1);
  FObjects[High(FObjects)] := AValue;
end;

procedure TTheWordClipboardFragmentV2.AddFontMark(
  const AValue: TTheWordClipboardFontMarkV2);
begin
  SetLength(FFontMarks, Length(FFontMarks) + 1);
  FFontMarks[High(FFontMarks)] := AValue;
end;

procedure TTheWordClipboardFragmentV2.ClearObjects;
begin
  SetLength(FObjects, 0);
end;

function SemanticKindName(AKind: TTheWordSemanticKind): string;
begin
  Result := '';
  case AKind of
    twsmItalic: Result := 'FI';
    twsmWordsOfJesus: Result := 'FR';
    twsmOldTestamentQuote: Result := 'FO';
    twsmUnderline: Result := 'FU';
    twsmStrikethrough: Result := 'S';
    twsmSuperscript: Result := 'SUP';
    twsmSubscript: Result := 'SUB';
    twsmHtmlBold: Result := 'B';
    twsmHtmlItalic: Result := 'I';
    twsmHtmlUnderline: Result := 'U';
  end;
end;

function SemanticKindFromName(const AName: string): TTheWordSemanticKind;
begin
  if AName = 'FI' then Exit(twsmItalic);
  if AName = 'FR' then Exit(twsmWordsOfJesus);
  if AName = 'FO' then Exit(twsmOldTestamentQuote);
  if AName = 'FU' then Exit(twsmUnderline);
  if AName = 'S' then Exit(twsmStrikethrough);
  if AName = 'SUP' then Exit(twsmSuperscript);
  if AName = 'SUB' then Exit(twsmSubscript);
  if AName = 'B' then Exit(twsmHtmlBold);
  if AName = 'I' then Exit(twsmHtmlItalic);
  if AName = 'U' then Exit(twsmHtmlUnderline);
  raise ETheWordClipboardV2.CreateFmt('Unknown semantic kind: %s', [AName]);
end;

function ObjectKindName(AKind: TTheWordClipboardObjectKindV2): string;
begin
  Result := '';
  case AKind of
    twcokLexical: Result := 'lexical';
    twcokOpaque: Result := 'opaque';
    twcokStructural: Result := 'structural';
  end;
end;

function ObjectKindFromName(const AName: string):
  TTheWordClipboardObjectKindV2;
begin
  if AName = 'lexical' then Exit(twcokLexical);
  if AName = 'opaque' then Exit(twcokOpaque);
  if AName = 'structural' then Exit(twcokStructural);
  raise ETheWordClipboardV2.CreateFmt('Unknown object type: %s', [AName]);
end;

function SubKindName(AObjectKind: TTheWordClipboardObjectKindV2;
  ASubKind: Integer): string;
begin
  Result := '';
  case AObjectKind of
    twcokLexical:
      case TTheWordLexicalKind(ASubKind) of
        twlkStrongGreek: Result := 'strongGreek';
        twlkStrongHebrew: Result := 'strongHebrew';
        twlkMorphology: Result := 'morphology';
        twlkWordTokenEnd: Result := 'wordTokenEnd';
      end;
    twcokOpaque:
      case TTheWordOpaqueKind(ASubKind) of
        twokUnknownTag: Result := 'unknownTag';
        twokMalformedTag: Result := 'malformedTag';
      end;
    twcokStructural:
      case TTheWordStructuralKind(ASubKind) of
        twstSyntagmBoundary: Result := 'syntagmBoundary';
      end;
  end;
end;

function SubKindFromName(AObjectKind: TTheWordClipboardObjectKindV2;
  const AName: string): Integer;
begin
  case AObjectKind of
    twcokLexical:
      begin
        if AName = 'strongGreek' then Exit(Ord(twlkStrongGreek));
        if AName = 'strongHebrew' then Exit(Ord(twlkStrongHebrew));
        if AName = 'morphology' then Exit(Ord(twlkMorphology));
        if AName = 'wordTokenEnd' then Exit(Ord(twlkWordTokenEnd));
      end;
    twcokOpaque:
      begin
        if AName = 'unknownTag' then Exit(Ord(twokUnknownTag));
        if AName = 'malformedTag' then Exit(Ord(twokMalformedTag));
      end;
    twcokStructural:
      if AName = 'syntagmBoundary' then
        Exit(Ord(twstSyntagmBoundary));
  end;
  raise ETheWordClipboardV2.CreateFmt('Unknown object kind: %s', [AName]);
end;

function CompareMarks(const ALeft, ARight: TTheWordClipboardMarkV2): Integer;
begin
  if ALeft.StartOffset < ARight.StartOffset then Exit(-1);
  if ALeft.StartOffset > ARight.StartOffset then Exit(1);
  if ALeft.EndOffset > ARight.EndOffset then Exit(-1);
  if ALeft.EndOffset < ARight.EndOffset then Exit(1);
  if Ord(ALeft.Kind) < Ord(ARight.Kind) then Exit(-1);
  if Ord(ALeft.Kind) > Ord(ARight.Kind) then Exit(1);
  Result := 0;
end;

function CompareObjects(const ALeft,
  ARight: TTheWordClipboardObjectV2): Integer;
begin
  if ALeft.Offset < ARight.Offset then Exit(-1);
  if ALeft.Offset > ARight.Offset then Exit(1);
  if ALeft.SourceOrder < ARight.SourceOrder then Exit(-1);
  if ALeft.SourceOrder > ARight.SourceOrder then Exit(1);
  if Ord(ALeft.ObjectKind) < Ord(ARight.ObjectKind) then Exit(-1);
  if Ord(ALeft.ObjectKind) > Ord(ARight.ObjectKind) then Exit(1);
  Result := 0;
end;

function CompareFontMarks(const ALeft,
  ARight: TTheWordClipboardFontMarkV2): Integer;
begin
  if ALeft.StartOffset < ARight.StartOffset then Exit(-1);
  if ALeft.StartOffset > ARight.StartOffset then Exit(1);
  if ALeft.OpenSourceOrder < ARight.OpenSourceOrder then Exit(-1);
  if ALeft.OpenSourceOrder > ARight.OpenSourceOrder then Exit(1);
  if ALeft.EndOffset > ARight.EndOffset then Exit(-1);
  if ALeft.EndOffset < ARight.EndOffset then Exit(1);
  Result := 0;
end;

procedure MergeSortMarks(var AValues, ABuffer: TTheWordClipboardMarkArrayV2;
  ALeft, ARight: Integer);
var
  Middle, LeftIndex, RightIndex, TargetIndex, Index: Integer;
begin
  if ALeft >= ARight then Exit;
  Middle := ALeft + (ARight - ALeft) div 2;
  MergeSortMarks(AValues, ABuffer, ALeft, Middle);
  MergeSortMarks(AValues, ABuffer, Middle + 1, ARight);
  LeftIndex := ALeft;
  RightIndex := Middle + 1;
  TargetIndex := ALeft;
  while (LeftIndex <= Middle) and (RightIndex <= ARight) do
  begin
    if CompareMarks(AValues[LeftIndex], AValues[RightIndex]) <= 0 then
    begin
      ABuffer[TargetIndex] := AValues[LeftIndex];
      Inc(LeftIndex);
    end
    else
    begin
      ABuffer[TargetIndex] := AValues[RightIndex];
      Inc(RightIndex);
    end;
    Inc(TargetIndex);
  end;
  while LeftIndex <= Middle do
  begin
    ABuffer[TargetIndex] := AValues[LeftIndex];
    Inc(LeftIndex);
    Inc(TargetIndex);
  end;
  while RightIndex <= ARight do
  begin
    ABuffer[TargetIndex] := AValues[RightIndex];
    Inc(RightIndex);
    Inc(TargetIndex);
  end;
  for Index := ALeft to ARight do AValues[Index] := ABuffer[Index];
end;

procedure MergeSortObjects(var AValues,
  ABuffer: TTheWordClipboardObjectArrayV2; ALeft, ARight: Integer);
var
  Middle, LeftIndex, RightIndex, TargetIndex, Index: Integer;
begin
  if ALeft >= ARight then Exit;
  Middle := ALeft + (ARight - ALeft) div 2;
  MergeSortObjects(AValues, ABuffer, ALeft, Middle);
  MergeSortObjects(AValues, ABuffer, Middle + 1, ARight);
  LeftIndex := ALeft;
  RightIndex := Middle + 1;
  TargetIndex := ALeft;
  while (LeftIndex <= Middle) and (RightIndex <= ARight) do
  begin
    if CompareObjects(AValues[LeftIndex], AValues[RightIndex]) <= 0 then
    begin
      ABuffer[TargetIndex] := AValues[LeftIndex];
      Inc(LeftIndex);
    end
    else
    begin
      ABuffer[TargetIndex] := AValues[RightIndex];
      Inc(RightIndex);
    end;
    Inc(TargetIndex);
  end;
  while LeftIndex <= Middle do
  begin
    ABuffer[TargetIndex] := AValues[LeftIndex];
    Inc(LeftIndex);
    Inc(TargetIndex);
  end;
  while RightIndex <= ARight do
  begin
    ABuffer[TargetIndex] := AValues[RightIndex];
    Inc(RightIndex);
    Inc(TargetIndex);
  end;
  for Index := ALeft to ARight do AValues[Index] := ABuffer[Index];
end;

procedure MergeSortFontMarks(var AValues,
  ABuffer: TTheWordClipboardFontMarkArrayV2; ALeft, ARight: Integer);
var
  Middle, LeftIndex, RightIndex, TargetIndex, Index: Integer;
begin
  if ALeft >= ARight then Exit;
  Middle := ALeft + (ARight - ALeft) div 2;
  MergeSortFontMarks(AValues, ABuffer, ALeft, Middle);
  MergeSortFontMarks(AValues, ABuffer, Middle + 1, ARight);
  LeftIndex := ALeft;
  RightIndex := Middle + 1;
  TargetIndex := ALeft;
  while (LeftIndex <= Middle) and (RightIndex <= ARight) do
  begin
    if CompareFontMarks(AValues[LeftIndex], AValues[RightIndex]) <= 0 then
    begin
      ABuffer[TargetIndex] := AValues[LeftIndex];
      Inc(LeftIndex);
    end
    else
    begin
      ABuffer[TargetIndex] := AValues[RightIndex];
      Inc(RightIndex);
    end;
    Inc(TargetIndex);
  end;
  while LeftIndex <= Middle do
  begin
    ABuffer[TargetIndex] := AValues[LeftIndex];
    Inc(LeftIndex);
    Inc(TargetIndex);
  end;
  while RightIndex <= ARight do
  begin
    ABuffer[TargetIndex] := AValues[RightIndex];
    Inc(RightIndex);
    Inc(TargetIndex);
  end;
  for Index := ALeft to ARight do AValues[Index] := ABuffer[Index];
end;

procedure SortFragment(AFragment: TTheWordClipboardFragmentV2);
var
  MarkBuffer: TTheWordClipboardMarkArrayV2;
  ObjectBuffer: TTheWordClipboardObjectArrayV2;
  FontBuffer: TTheWordClipboardFontMarkArrayV2;
begin
  if AFragment.MarkCount > 1 then
  begin
    SetLength(MarkBuffer, AFragment.MarkCount);
    MergeSortMarks(AFragment.FMarks, MarkBuffer, 0,
      AFragment.MarkCount - 1);
  end;
  if AFragment.ObjectCount > 1 then
  begin
    SetLength(ObjectBuffer, AFragment.ObjectCount);
    MergeSortObjects(AFragment.FObjects, ObjectBuffer, 0,
      AFragment.ObjectCount - 1);
  end;
  if AFragment.FontMarkCount > 1 then
  begin
    SetLength(FontBuffer, AFragment.FontMarkCount);
    MergeSortFontMarks(AFragment.FFontMarks, FontBuffer, 0,
      AFragment.FontMarkCount - 1);
  end;
end;

procedure RequireFields(AObject: TJSONObject;
  const AAllowed: array of string; const AContext: string);
var
  I, J, K: Integer;
  Found: Boolean;
begin
  for I := 0 to AObject.Count - 1 do
  begin
    for J := 0 to I - 1 do
      if AObject.Names[J] = AObject.Names[I] then
        raise ETheWordClipboardV2.CreateFmt('Duplicate field "%s" in %s',
          [AObject.Names[I], AContext]);
    Found := False;
    for K := Low(AAllowed) to High(AAllowed) do
      if AObject.Names[I] = AAllowed[K] then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      raise ETheWordClipboardV2.CreateFmt('Unknown field "%s" in %s',
        [AObject.Names[I], AContext]);
  end;
end;

function RequireData(AObject: TJSONObject; const AName,
  AContext: string; AType: TJSONType): TJSONData;
begin
  Result := AObject.Find(AName);
  if not Assigned(Result) then
    raise ETheWordClipboardV2.CreateFmt('Missing field "%s" in %s',
      [AName, AContext]);
  if Result.JSONType <> AType then
    raise ETheWordClipboardV2.CreateFmt('Wrong type for "%s" in %s',
      [AName, AContext]);
end;

function RequireString(AObject: TJSONObject; const AName,
  AContext: string): string;
begin
  Result := RequireData(AObject, AName, AContext, jtString).AsString;
end;

function RequireInteger(AObject: TJSONObject; const AName,
  AContext: string): Integer;
var
  Value: Int64;
begin
  if not TryStrToInt64(RequireData(AObject, AName, AContext,
    jtNumber).AsJSON, Value) or (Value < Low(Integer)) or
    (Value > High(Integer)) then
    raise ETheWordClipboardV2.CreateFmt('Invalid integer "%s" in %s',
      [AName, AContext]);
  Result := Integer(Value);
end;

function CompareOrderEvents(const ALeft,
  ARight: TClipboardOrderEventV2): Integer;
begin
  if ALeft.SourceOrder < ARight.SourceOrder then Exit(-1);
  if ALeft.SourceOrder > ARight.SourceOrder then Exit(1);
  Result := 0;
end;

procedure MergeSortOrderEvents(var AValues, ABuffer: TClipboardOrderEventArray;
  ALeft, ARight: Integer);
var
  Middle, LeftIndex, RightIndex, TargetIndex, Index: Integer;
begin
  if ALeft >= ARight then Exit;
  Middle := ALeft + (ARight - ALeft) div 2;
  MergeSortOrderEvents(AValues, ABuffer, ALeft, Middle);
  MergeSortOrderEvents(AValues, ABuffer, Middle + 1, ARight);
  LeftIndex := ALeft;
  RightIndex := Middle + 1;
  TargetIndex := ALeft;
  while (LeftIndex <= Middle) and (RightIndex <= ARight) do
  begin
    if CompareOrderEvents(AValues[LeftIndex],
      AValues[RightIndex]) <= 0 then
    begin
      ABuffer[TargetIndex] := AValues[LeftIndex];
      Inc(LeftIndex);
    end
    else
    begin
      ABuffer[TargetIndex] := AValues[RightIndex];
      Inc(RightIndex);
    end;
    Inc(TargetIndex);
  end;
  while LeftIndex <= Middle do
  begin
    ABuffer[TargetIndex] := AValues[LeftIndex];
    Inc(LeftIndex);
    Inc(TargetIndex);
  end;
  while RightIndex <= ARight do
  begin
    ABuffer[TargetIndex] := AValues[RightIndex];
    Inc(RightIndex);
    Inc(TargetIndex);
  end;
  for Index := ALeft to ARight do
    AValues[Index] := ABuffer[Index];
end;

function CompareCrossRanges(const ALeft, ARight: TClipboardCrossRangeV2;
  AByStart: Boolean): Integer;
begin
  if AByStart then
  begin
    if ALeft.StartOffset < ARight.StartOffset then Exit(-1);
    if ALeft.StartOffset > ARight.StartOffset then Exit(1);
    if ALeft.EndOffset < ARight.EndOffset then Exit(-1);
    if ALeft.EndOffset > ARight.EndOffset then Exit(1);
  end
  else
  begin
    if ALeft.EndOffset < ARight.EndOffset then Exit(-1);
    if ALeft.EndOffset > ARight.EndOffset then Exit(1);
    if ALeft.StartOffset < ARight.StartOffset then Exit(-1);
    if ALeft.StartOffset > ARight.StartOffset then Exit(1);
  end;
  if Ord(ALeft.Kind) < Ord(ARight.Kind) then Exit(-1);
  if Ord(ALeft.Kind) > Ord(ARight.Kind) then Exit(1);
  if ALeft.Id < ARight.Id then Exit(-1);
  if ALeft.Id > ARight.Id then Exit(1);
  Result := 0;
end;

procedure MergeSortCrossRanges(var AValues, ABuffer: TClipboardCrossRangeArray;
  ALeft, ARight: Integer; AByStart: Boolean);
var
  Middle, LeftIndex, RightIndex, TargetIndex, Index: Integer;
begin
  if ALeft >= ARight then Exit;
  Middle := ALeft + (ARight - ALeft) div 2;
  MergeSortCrossRanges(AValues, ABuffer, ALeft, Middle, AByStart);
  MergeSortCrossRanges(AValues, ABuffer, Middle + 1, ARight, AByStart);
  LeftIndex := ALeft;
  RightIndex := Middle + 1;
  TargetIndex := ALeft;
  while (LeftIndex <= Middle) and (RightIndex <= ARight) do
  begin
    if CompareCrossRanges(AValues[LeftIndex],
      AValues[RightIndex], AByStart) <= 0 then
    begin
      ABuffer[TargetIndex] := AValues[LeftIndex];
      Inc(LeftIndex);
    end
    else
    begin
      ABuffer[TargetIndex] := AValues[RightIndex];
      Inc(RightIndex);
    end;
    Inc(TargetIndex);
  end;
  while LeftIndex <= Middle do
  begin
    ABuffer[TargetIndex] := AValues[LeftIndex];
    Inc(LeftIndex);
    Inc(TargetIndex);
  end;
  while RightIndex <= ARight do
  begin
    ABuffer[TargetIndex] := AValues[RightIndex];
    Inc(RightIndex);
    Inc(TargetIndex);
  end;
  for Index := ALeft to ARight do
    AValues[Index] := ABuffer[Index];
end;

procedure FenwickAdd(var ATree: TIntegerArray; AIndex, ADelta: Integer);
begin
  while AIndex < Length(ATree) do
  begin
    Inc(ATree[AIndex], ADelta);
    Inc(AIndex, AIndex and -AIndex);
  end;
end;

function FenwickPrefix(const ATree: TIntegerArray; AIndex: Integer): Integer;
begin
  Result := 0;
  while AIndex > 0 do
  begin
    Inc(Result, ATree[AIndex]);
    Dec(AIndex, AIndex and -AIndex);
  end;
end;

procedure ValidateFontSemanticCrossing(
  AFragment: TTheWordClipboardFragmentV2);
var
  Ranges, ByStart, ByEnd, Buffer: TClipboardCrossRangeArray;
  StartRanks, FontTree, SemanticTree: TIntegerArray;
  I, RangeCount, StartIndex, EndIndex, BatchEnd, UniqueStarts: Integer;

  procedure SetActive(const ARange: TClipboardCrossRangeV2;
    ADelta: Integer);
  begin
    if ARange.Kind = ccrkFont then
      FenwickAdd(FontTree, StartRanks[ARange.Id], ADelta)
    else
      FenwickAdd(SemanticTree, StartRanks[ARange.Id], ADelta);
  end;

  function HasLaterStart(const ARange: TClipboardCrossRangeV2): Boolean;
  begin
    if ARange.Kind = ccrkFont then
      Result := FenwickPrefix(SemanticTree, UniqueStarts) >
        FenwickPrefix(SemanticTree, StartRanks[ARange.Id])
    else
      Result := FenwickPrefix(FontTree, UniqueStarts) >
        FenwickPrefix(FontTree, StartRanks[ARange.Id]);
  end;

begin
  RangeCount := AFragment.MarkCount;
  for I := 0 to AFragment.FontMarkCount - 1 do
    if AFragment.FontMarks[I].StartOffset <
       AFragment.FontMarks[I].EndOffset then
      Inc(RangeCount);
  if RangeCount = 0 then Exit;
  SetLength(Ranges, RangeCount);
  RangeCount := 0;
  for I := 0 to AFragment.MarkCount - 1 do
  begin
    Ranges[RangeCount].Id := RangeCount;
    Ranges[RangeCount].Kind := ccrkSemantic;
    Ranges[RangeCount].StartOffset := AFragment.Marks[I].StartOffset;
    Ranges[RangeCount].EndOffset := AFragment.Marks[I].EndOffset;
    Inc(RangeCount);
  end;
  for I := 0 to AFragment.FontMarkCount - 1 do
    if AFragment.FontMarks[I].StartOffset <
       AFragment.FontMarks[I].EndOffset then
    begin
      Ranges[RangeCount].Id := RangeCount;
      Ranges[RangeCount].Kind := ccrkFont;
      Ranges[RangeCount].StartOffset :=
        AFragment.FontMarks[I].StartOffset;
      Ranges[RangeCount].EndOffset := AFragment.FontMarks[I].EndOffset;
      Inc(RangeCount);
    end;

  ByStart := Copy(Ranges, 0, Length(Ranges));
  ByEnd := Copy(Ranges, 0, Length(Ranges));
  SetLength(Buffer, RangeCount);
  if RangeCount > 1 then
  begin
    MergeSortCrossRanges(ByStart, Buffer, 0, High(ByStart), True);
    MergeSortCrossRanges(ByEnd, Buffer, 0, High(ByEnd), False);
  end;
  SetLength(StartRanks, RangeCount);
  UniqueStarts := 0;
  for I := 0 to High(ByStart) do
  begin
    if (I = 0) or
       (ByStart[I - 1].StartOffset <> ByStart[I].StartOffset) then
      Inc(UniqueStarts);
    StartRanks[ByStart[I].Id] := UniqueStarts;
  end;
  SetLength(FontTree, UniqueStarts + 1);
  SetLength(SemanticTree, UniqueStarts + 1);

  StartIndex := 0;
  EndIndex := 0;
  while EndIndex < RangeCount do
  begin
    BatchEnd := EndIndex + 1;
    while (BatchEnd < RangeCount) and
          (ByEnd[BatchEnd].EndOffset = ByEnd[EndIndex].EndOffset) do
      Inc(BatchEnd);
    while (StartIndex < RangeCount) and
          (ByStart[StartIndex].StartOffset < ByEnd[EndIndex].EndOffset) do
    begin
      SetActive(ByStart[StartIndex], 1);
      Inc(StartIndex);
    end;
    { Equal ends are boundaries, not crossings: remove the whole batch first. }
    for I := EndIndex to BatchEnd - 1 do
      SetActive(ByEnd[I], -1);
    for I := EndIndex to BatchEnd - 1 do
      if HasLaterStart(ByEnd[I]) then
        raise ETheWordClipboardV2.Create(
          'Clipboard font and semantic marks cross');
    EndIndex := BatchEnd;
  end;
end;

procedure BuildAndValidateOrderEvents(AFragment: TTheWordClipboardFragmentV2;
  ARequireDense: Boolean; out AEvents: TClipboardOrderEventArray);
var
  Buffer: TClipboardOrderEventArray;
  Stack: TIntegerArray;
  FontTag: TTheWordInlineFormatTagV2;
  FontError: UnicodeString;
  FontMark: TTheWordClipboardFontMarkV2;
  I, EventIndex, PreviousOffset, StackCount: Integer;
  LegacyV1: Boolean;
begin
  LegacyV1 := AFragment.FDecodedVersion = THEWORD_CLIPBOARD_VERSION_V1;
  if (not LegacyV1) and
     (Length(AFragment.Text) > MAX_CLIPBOARD_TEXT_UTF16) then
    raise ETheWordClipboardV2.Create('Clipboard text is too large');
  if (AFragment.MarkCount > MAX_CLIPBOARD_ITEMS) or
     (AFragment.ObjectCount > MAX_CLIPBOARD_ITEMS) or
     (AFragment.FontMarkCount > MAX_CLIPBOARD_ITEMS) then
    raise ETheWordClipboardV2.Create('Clipboard payload has too many items');
  if AFragment.FontMarkCount >
     (MAX_CLIPBOARD_ITEMS - AFragment.ObjectCount) div 2 then
    raise ETheWordClipboardV2.Create(
      'Clipboard payload has too many ordered events');

  SetLength(AEvents, AFragment.ObjectCount + 2 * AFragment.FontMarkCount);
  EventIndex := 0;
  for I := 0 to AFragment.MarkCount - 1 do
    if (AFragment.Marks[I].StartOffset < 0) or
       (AFragment.Marks[I].EndOffset <= AFragment.Marks[I].StartOffset) or
       (AFragment.Marks[I].EndOffset > Length(AFragment.Text)) or
       not IsUtf16Boundary(AFragment.Text, AFragment.Marks[I].StartOffset) or
       not IsUtf16Boundary(AFragment.Text, AFragment.Marks[I].EndOffset) then
      raise ETheWordClipboardV2.Create('Clipboard mark has an invalid range');
  for I := 0 to AFragment.ObjectCount - 1 do
  begin
    if (AFragment.Objects[I].Offset < 0) or
       (AFragment.Objects[I].Offset > Length(AFragment.Text)) or
       not IsUtf16Boundary(AFragment.Text, AFragment.Objects[I].Offset) or
       (AFragment.Objects[I].RawLexeme = '') or
       ((not LegacyV1) and
        (Length(AFragment.Objects[I].RawLexeme) >
         MAX_CLIPBOARD_RAW_UTF16)) or
       (AFragment.Objects[I].SourceOrder < 0) then
      raise ETheWordClipboardV2.Create('Clipboard object is invalid');
    AEvents[EventIndex].Kind := coekObject;
    AEvents[EventIndex].ObjectIndex := I;
    AEvents[EventIndex].FontIndex := -1;
    AEvents[EventIndex].Offset := AFragment.Objects[I].Offset;
    AEvents[EventIndex].SourceOrder := AFragment.Objects[I].SourceOrder;
    Inc(EventIndex);
  end;
  for I := 0 to AFragment.FontMarkCount - 1 do
  begin
    FontMark := AFragment.FontMarks[I];
    if (FontMark.StartOffset < 0) or
       (FontMark.EndOffset < FontMark.StartOffset) or
       (FontMark.EndOffset > Length(AFragment.Text)) or
       not IsUtf16Boundary(AFragment.Text, FontMark.StartOffset) or
       not IsUtf16Boundary(AFragment.Text, FontMark.EndOffset) then
      raise ETheWordClipboardV2.Create(
        'Clipboard font mark has an invalid range');
    if (FontMark.OpenRaw = '') or (FontMark.CloseRaw = '') or
       (Length(FontMark.OpenRaw) > MAX_CLIPBOARD_RAW_UTF16) or
       (Length(FontMark.CloseRaw) > MAX_CLIPBOARD_RAW_UTF16) then
      raise ETheWordClipboardV2.Create(
        'Clipboard font lexeme is empty or too large');
    if (FontMark.OpenSourceOrder < 0) or
       (FontMark.CloseSourceOrder <= FontMark.OpenSourceOrder) then
      raise ETheWordClipboardV2.Create(
        'Clipboard font mark has invalid source order');
    if not TryParseTheWordInlineFormatTagV2(FontMark.OpenRaw, FontTag,
      FontError) or (FontTag.Kind <> twiftv2Font) or FontTag.IsClosing then
      raise ETheWordClipboardV2.Create('Clipboard font opener is invalid');
    if not TryParseTheWordInlineFormatTagV2(FontMark.CloseRaw, FontTag,
      FontError) or (FontTag.Kind <> twiftv2Font) or not FontTag.IsClosing or
      (Length(FontTag.Attributes) <> 0) then
      raise ETheWordClipboardV2.Create('Clipboard font closer is invalid');
    AEvents[EventIndex].Kind := coekFontOpen;
    AEvents[EventIndex].ObjectIndex := -1;
    AEvents[EventIndex].FontIndex := I;
    AEvents[EventIndex].Offset := FontMark.StartOffset;
    AEvents[EventIndex].SourceOrder := FontMark.OpenSourceOrder;
    Inc(EventIndex);
    AEvents[EventIndex].Kind := coekFontClose;
    AEvents[EventIndex].ObjectIndex := -1;
    AEvents[EventIndex].FontIndex := I;
    AEvents[EventIndex].Offset := FontMark.EndOffset;
    AEvents[EventIndex].SourceOrder := FontMark.CloseSourceOrder;
    Inc(EventIndex);
  end;
  ValidateFontSemanticCrossing(AFragment);

  { V1 had no font marks and accepted duplicate, sparse and high source
    orders. Its objects are already sorted by offset/order on decode. }
  if LegacyV1 then
  begin
    if AFragment.FontMarkCount <> 0 then
      raise ETheWordClipboardV2.Create(
        'Clipboard v1 fragment cannot contain font marks');
    Exit;
  end;

  if Length(AEvents) > 1 then
  begin
    SetLength(Buffer, Length(AEvents));
    MergeSortOrderEvents(AEvents, Buffer, 0, High(AEvents));
  end;
  SetLength(Stack, AFragment.FontMarkCount);
  StackCount := 0;
  PreviousOffset := -1;
  for I := 0 to High(AEvents) do
  begin
    if (I > 0) and
       (AEvents[I - 1].SourceOrder = AEvents[I].SourceOrder) then
      raise ETheWordClipboardV2.Create(
        'Clipboard source order is not unique');
    if ARequireDense and (AEvents[I].SourceOrder <> I) then
      raise ETheWordClipboardV2.Create(
        'Clipboard source order is not dense');
    if AEvents[I].Offset < PreviousOffset then
      raise ETheWordClipboardV2.Create(
        'Clipboard source order contradicts offsets');
    PreviousOffset := AEvents[I].Offset;
    case AEvents[I].Kind of
      coekFontOpen:
        begin
          Stack[StackCount] := AEvents[I].FontIndex;
          Inc(StackCount);
        end;
      coekFontClose:
        begin
          if (StackCount = 0) or
             (Stack[StackCount - 1] <> AEvents[I].FontIndex) then
            raise ETheWordClipboardV2.Create(
              'Clipboard font marks are not nested');
          Dec(StackCount);
        end;
    end;
  end;
  if StackCount <> 0 then
    raise ETheWordClipboardV2.Create(
      'Clipboard font marks are unbalanced');
end;


procedure NormalizePostfixOrderAtSameOffset(
  AFragment: TTheWordClipboardFragmentV2;
  var AEvents: TClipboardOrderEventArray);
var
  Buffer: TClipboardOrderEventArray;
  I, GroupEnd, J, OutputIndex, Pass: Integer;

  function IsLexicalPostfix(
    const AEvent: TClipboardOrderEventV2): Boolean;
  var
    Obj: TTheWordClipboardObjectV2;
  begin
    if AEvent.Kind <> coekObject then Exit(False);
    Obj := AFragment.Objects[AEvent.ObjectIndex];
    if Obj.ObjectKind <> twcokLexical then Exit(False);
    case Obj.SubKind of
      Ord(twlkStrongGreek), Ord(twlkStrongHebrew),
      Ord(twlkMorphology), Ord(twlkWordTokenEnd): Result := True;
    else
      Result := False;
    end;
  end;
begin
  if AFragment.FDecodedVersion = THEWORD_CLIPBOARD_VERSION_V1 then Exit;
  SetLength(Buffer, Length(AEvents));
  I := 0;
  OutputIndex := 0;
  while I <= High(AEvents) do
  begin
    GroupEnd := I;
    while (GroupEnd < High(AEvents)) and
          (AEvents[GroupEnd + 1].Offset = AEvents[I].Offset) do
      Inc(GroupEnd);
    { Stable partition: WG/WH/WT/wt first, all other events second. }
    for Pass := 0 to 1 do
      for J := I to GroupEnd do
        if IsLexicalPostfix(AEvents[J]) = (Pass = 0) then
        begin
          Buffer[OutputIndex] := AEvents[J];
          Inc(OutputIndex);
        end;
    I := GroupEnd + 1;
  end;
  AEvents := Buffer;
end;

procedure BuildNormalizedOrderRanks(AFragment: TTheWordClipboardFragmentV2;
  out AObjectRanks, AOpenRanks, ACloseRanks: TIntegerArray);
var
  Events: TClipboardOrderEventArray;
  I: Integer;
begin
  BuildAndValidateOrderEvents(AFragment, False, Events);
  NormalizePostfixOrderAtSameOffset(AFragment, Events);
  SetLength(AObjectRanks, AFragment.ObjectCount);
  SetLength(AOpenRanks, AFragment.FontMarkCount);
  SetLength(ACloseRanks, AFragment.FontMarkCount);
  for I := 0 to High(Events) do
    case Events[I].Kind of
      coekObject:
        AObjectRanks[Events[I].ObjectIndex] := I;
      coekFontOpen:
        AOpenRanks[Events[I].FontIndex] := I;
      coekFontClose:
        ACloseRanks[Events[I].FontIndex] := I;
    end;
end;

function SerializeFragment(AFragment: TTheWordClipboardFragmentV2): string;
var
  Root, Item: TJSONObject;
  Marks, Objects, FontMarks: TJSONArray;
  ObjectRanks, OpenRanks, CloseRanks: TIntegerArray;
  ValidationEvents: TClipboardOrderEventArray;
  I: Integer;
begin
  BuildAndValidateOrderEvents(AFragment, False, ValidationEvents);
  SortFragment(AFragment);
  BuildNormalizedOrderRanks(AFragment, ObjectRanks, OpenRanks, CloseRanks);
  Root := TJSONObject.Create;
  try
    Root.Add('schema', THEWORD_CLIPBOARD_SCHEMA_V2);
    Root.Add('version', THEWORD_CLIPBOARD_VERSION_V2);
    Root.Add('text', UTF8Encode(AFragment.Text));
    Marks := TJSONArray.Create;
    Root.Add('marks', Marks);
    for I := 0 to AFragment.MarkCount - 1 do
    begin
      Item := TJSONObject.Create;
      Item.Add('kind', SemanticKindName(AFragment.Marks[I].Kind));
      Item.Add('startUTF16', AFragment.Marks[I].StartOffset);
      Item.Add('endUTF16', AFragment.Marks[I].EndOffset);
      Marks.Add(Item);
    end;
    Objects := TJSONArray.Create;
    Root.Add('objects', Objects);
    for I := 0 to AFragment.ObjectCount - 1 do
    begin
      Item := TJSONObject.Create;
      Item.Add('type', ObjectKindName(AFragment.Objects[I].ObjectKind));
      Item.Add('kind', SubKindName(AFragment.Objects[I].ObjectKind,
        AFragment.Objects[I].SubKind));
      Item.Add('offsetUTF16', AFragment.Objects[I].Offset);
      Item.Add('raw', UTF8Encode(AFragment.Objects[I].RawLexeme));
      Item.Add('order', ObjectRanks[I]);
      Objects.Add(Item);
    end;
    FontMarks := TJSONArray.Create;
    Root.Add('fontMarks', FontMarks);
    for I := 0 to AFragment.FontMarkCount - 1 do
    begin
      Item := TJSONObject.Create;
      Item.Add('startUTF16', AFragment.FontMarks[I].StartOffset);
      Item.Add('endUTF16', AFragment.FontMarks[I].EndOffset);
      Item.Add('openRaw', UTF8Encode(AFragment.FontMarks[I].OpenRaw));
      Item.Add('closeRaw', UTF8Encode(AFragment.FontMarks[I].CloseRaw));
      Item.Add('openSourceOrder', OpenRanks[I]);
      Item.Add('closeSourceOrder', CloseRanks[I]);
      FontMarks.Add(Item);
    end;
    Result := Root.AsJSON;
  finally
    Root.Free;
  end;
end;

function DeserializeFragment(const APayload: string):
  TTheWordClipboardFragmentV2;
var
  Data: TJSONData;
  Root, Item: TJSONObject;
  Marks, Objects, FontMarks: TJSONArray;
  Mark: TTheWordClipboardMarkV2;
  Obj: TTheWordClipboardObjectV2;
  FontMark: TTheWordClipboardFontMarkV2;
  OrderEvents: TClipboardOrderEventArray;
  I, Version: Integer;
begin
  if (APayload = '') or (Length(APayload) > MAX_CLIPBOARD_PAYLOAD_BYTES) then
    raise ETheWordClipboardV2.Create('Clipboard payload is empty or too large');
  Data := nil;
  Result := nil;
  try
    Data := GetJSON(APayload);
    if Data.JSONType <> jtObject then
      raise ETheWordClipboardV2.Create('Clipboard payload root must be an object');
    Root := TJSONObject(Data);
    if RequireString(Root, 'schema', 'clipboard payload') <>
       THEWORD_CLIPBOARD_SCHEMA_V2 then
      raise ETheWordClipboardV2.Create('Unknown clipboard schema');
    Version := RequireInteger(Root, 'version', 'clipboard payload');
    if (Version <> THEWORD_CLIPBOARD_VERSION_V1) and
       (Version <> THEWORD_CLIPBOARD_VERSION_V2) then
      raise ETheWordClipboardV2.Create('Unsupported clipboard version');
    if Version = THEWORD_CLIPBOARD_VERSION_V1 then
      RequireFields(Root, ['schema', 'version', 'text', 'marks', 'objects'],
        'clipboard payload v1')
    else
      RequireFields(Root, ['schema', 'version', 'text', 'marks', 'objects',
        'fontMarks'], 'clipboard payload v2');

    Result := TTheWordClipboardFragmentV2.Create;
    Result.FDecodedVersion := Version;
    Result.FText := UTF8Decode(RequireString(Root, 'text', 'clipboard payload'));
    if (Version = THEWORD_CLIPBOARD_VERSION_V2) and
       (Length(Result.Text) > MAX_CLIPBOARD_TEXT_UTF16) then
      raise ETheWordClipboardV2.Create('Clipboard text is too large');
    Marks := TJSONArray(RequireData(Root, 'marks', 'clipboard payload', jtArray));
    Objects := TJSONArray(RequireData(Root, 'objects', 'clipboard payload', jtArray));
    if Version = THEWORD_CLIPBOARD_VERSION_V2 then
      FontMarks := TJSONArray(RequireData(Root, 'fontMarks',
        'clipboard payload', jtArray))
    else
      FontMarks := nil;
    if (Marks.Count > MAX_CLIPBOARD_ITEMS) or
       (Objects.Count > MAX_CLIPBOARD_ITEMS) or
       (Assigned(FontMarks) and (FontMarks.Count > MAX_CLIPBOARD_ITEMS)) then
      raise ETheWordClipboardV2.Create('Clipboard payload has too many items');
    for I := 0 to Marks.Count - 1 do
    begin
      if Marks.Items[I].JSONType <> jtObject then
        raise ETheWordClipboardV2.Create('Clipboard mark must be an object');
      Item := TJSONObject(Marks.Items[I]);
      RequireFields(Item, ['kind', 'startUTF16', 'endUTF16'],
        'clipboard mark');
      Mark.Kind := SemanticKindFromName(RequireString(Item, 'kind',
        'clipboard mark'));
      Mark.StartOffset := RequireInteger(Item, 'startUTF16', 'clipboard mark');
      Mark.EndOffset := RequireInteger(Item, 'endUTF16', 'clipboard mark');
      if (Mark.StartOffset < 0) or (Mark.EndOffset <= Mark.StartOffset) or
         (Mark.EndOffset > Length(Result.Text)) or
         not IsUtf16Boundary(Result.Text, Mark.StartOffset) or
         not IsUtf16Boundary(Result.Text, Mark.EndOffset) then
        raise ETheWordClipboardV2.Create('Clipboard mark has an invalid range');
      Result.AddMark(Mark);
    end;
    for I := 0 to Objects.Count - 1 do
    begin
      if Objects.Items[I].JSONType <> jtObject then
        raise ETheWordClipboardV2.Create('Clipboard object must be an object');
      Item := TJSONObject(Objects.Items[I]);
      RequireFields(Item, ['type', 'kind', 'offsetUTF16', 'raw', 'order'],
        'clipboard object');
      Obj.ObjectKind := ObjectKindFromName(RequireString(Item, 'type',
        'clipboard object'));
      Obj.SubKind := SubKindFromName(Obj.ObjectKind, RequireString(Item,
        'kind', 'clipboard object'));
      Obj.Offset := RequireInteger(Item, 'offsetUTF16', 'clipboard object');
      Obj.RawLexeme := UTF8Decode(RequireString(Item, 'raw',
        'clipboard object'));
      Obj.SourceOrder := RequireInteger(Item, 'order', 'clipboard object');
      if (Obj.Offset < 0) or (Obj.Offset > Length(Result.Text)) or
         not IsUtf16Boundary(Result.Text, Obj.Offset) or
         (Obj.RawLexeme = '') or (Obj.SourceOrder < 0) or
         ((Version = THEWORD_CLIPBOARD_VERSION_V2) and
          (Length(Obj.RawLexeme) > MAX_CLIPBOARD_RAW_UTF16)) then
        raise ETheWordClipboardV2.Create('Clipboard object is invalid');
      Result.AddObject(Obj);
    end;
    if Assigned(FontMarks) then
      for I := 0 to FontMarks.Count - 1 do
      begin
        if FontMarks.Items[I].JSONType <> jtObject then
          raise ETheWordClipboardV2.Create(
            'Clipboard font mark must be an object');
        Item := TJSONObject(FontMarks.Items[I]);
        RequireFields(Item, ['startUTF16', 'endUTF16', 'openRaw',
          'closeRaw', 'openSourceOrder', 'closeSourceOrder'],
          'clipboard font mark');
        FontMark.StartOffset := RequireInteger(Item, 'startUTF16',
          'clipboard font mark');
        FontMark.EndOffset := RequireInteger(Item, 'endUTF16',
          'clipboard font mark');
        FontMark.OpenRaw := UTF8Decode(RequireString(Item, 'openRaw',
          'clipboard font mark'));
        FontMark.CloseRaw := UTF8Decode(RequireString(Item, 'closeRaw',
          'clipboard font mark'));
        FontMark.OpenSourceOrder := RequireInteger(Item, 'openSourceOrder',
          'clipboard font mark');
        FontMark.CloseSourceOrder := RequireInteger(Item,
          'closeSourceOrder', 'clipboard font mark');
        Result.AddFontMark(FontMark);
      end;
    if Version = THEWORD_CLIPBOARD_VERSION_V2 then
      BuildAndValidateOrderEvents(Result, True, OrderEvents);
    SortFragment(Result);
  except
    Result.Free;
    Result := nil;
    Data.Free;
    raise;
  end;
  Data.Free;
end;

function FindUniqueMarker(const ARaw: UnicodeString): UnicodeString;
var
  Code: Integer;
begin
  for Code := $E000 to $F8FF do
  begin
    Result := UnicodeString(WideChar(Code));
    if Pos(Result, ARaw) = 0 then
      Exit;
  end;
  raise ETheWordClipboardV2.Create('Unable to reserve an internal marker');
end;

function InjectObjectGroups(const ARaw: UnicodeString;
  AFragment: TTheWordClipboardFragmentV2; ABaseOffset: Integer;
  out AResult: UnicodeString): Boolean;
var
  Codec: ITheWordOntCodecV2;
  Doc: TTheWordVerseDocument;
  GroupRaw, Marker, Temporary: UnicodeString;
  GroupOffset, I, MarkerPos: Integer;
begin
  Result := False;
  AResult := ARaw;
  Codec := TTheWordOntCodecV2.Create;
  I := 0;
  try
    while I < AFragment.ObjectCount do
    begin
      GroupOffset := AFragment.Objects[I].Offset;
      GroupRaw := '';
      while (I < AFragment.ObjectCount) and
            (AFragment.Objects[I].Offset = GroupOffset) do
      begin
        GroupRaw := GroupRaw + AFragment.Objects[I].RawLexeme;
        Inc(I);
      end;
      Doc := Codec.DecodeVerse(AResult);
      try
        Marker := FindUniqueMarker(AResult);
        Doc.ReplaceVisibleText(ABaseOffset + GroupOffset, 0, Marker);
        Temporary := Codec.EncodeVerse(Doc);
      finally
        Doc.Free;
      end;
      MarkerPos := Pos(Marker, Temporary);
      if MarkerPos = 0 then
        Exit;
      if Pos(Marker, Copy(Temporary, MarkerPos + Length(Marker), MaxInt)) > 0 then
        Exit;
      AResult := Copy(Temporary, 1, MarkerPos - 1) + GroupRaw +
        Copy(Temporary, MarkerPos + Length(Marker), MaxInt);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function BuildOrderedFragmentRaw(AFragment: TTheWordClipboardFragmentV2;
  out ARaw: UnicodeString): Boolean;
var
  Events: TClipboardOrderEventArray;
  Event: TClipboardOrderEventV2;
  TextOffset, I: Integer;
begin
  Result := False;
  ARaw := '';
  try
    BuildAndValidateOrderEvents(AFragment, False, Events);
    NormalizePostfixOrderAtSameOffset(AFragment, Events);
    TextOffset := 0;
    for I := 0 to High(Events) do
    begin
      Event := Events[I];
      ARaw := ARaw + Copy(AFragment.Text, TextOffset + 1,
        Event.Offset - TextOffset);
      TextOffset := Event.Offset;
      case Event.Kind of
        coekObject:
          ARaw := ARaw + AFragment.Objects[Event.ObjectIndex].RawLexeme;
        coekFontOpen:
          ARaw := ARaw + AFragment.FontMarks[Event.FontIndex].OpenRaw;
        coekFontClose:
          ARaw := ARaw + AFragment.FontMarks[Event.FontIndex].CloseRaw;
      end;
    end;
    ARaw := ARaw + Copy(AFragment.Text, TextOffset + 1, MaxInt);
    Result := True;
  except
    Result := False;
  end;
end;

procedure AddDocumentTransportEvents(ADocument: TTheWordVerseDocument;
  AFragment: TTheWordClipboardFragmentV2);
var
  Obj: TTheWordClipboardObjectV2;
  FontMark: TTheWordClipboardFontMarkV2;
  I: Integer;
begin
  AFragment.FText := ADocument.VisibleText;
  for I := 0 to ADocument.LexicalItemCount - 1 do
  begin
    Obj.ObjectKind := twcokLexical;
    Obj.SubKind := Ord(ADocument.LexicalItems[I].Kind);
    Obj.Offset := ADocument.LexicalItems[I].Offset;
    Obj.RawLexeme := ADocument.LexicalItems[I].RawLexeme;
    Obj.SourceOrder := ADocument.LexicalItems[I].Ordinal;
    AFragment.AddObject(Obj);
  end;
  for I := 0 to ADocument.OpaqueItemCount - 1 do
  begin
    Obj.ObjectKind := twcokOpaque;
    Obj.SubKind := Ord(ADocument.OpaqueItems[I].Kind);
    Obj.Offset := ADocument.OpaqueItems[I].Offset;
    Obj.RawLexeme := ADocument.OpaqueItems[I].RawLexeme;
    Obj.SourceOrder := ADocument.OpaqueItems[I].Ordinal;
    AFragment.AddObject(Obj);
  end;
  for I := 0 to ADocument.StructuralItemCount - 1 do
  begin
    Obj.ObjectKind := twcokStructural;
    Obj.SubKind := Ord(ADocument.StructuralItems[I].Kind);
    Obj.Offset := ADocument.StructuralItems[I].Offset;
    Obj.RawLexeme := ADocument.StructuralItems[I].RawLexeme;
    Obj.SourceOrder := ADocument.StructuralItems[I].Ordinal;
    AFragment.AddObject(Obj);
  end;
  for I := 0 to ADocument.FontMarkCount - 1 do
  begin
    FontMark.StartOffset := ADocument.FontMarks[I].StartOffset;
    FontMark.EndOffset := ADocument.FontMarks[I].EndOffset;
    FontMark.OpenRaw := ADocument.FontMarks[I].OpenLexeme;
    FontMark.CloseRaw := ADocument.FontMarks[I].CloseLexeme;
    FontMark.OpenSourceOrder := ADocument.FontMarks[I].OpenOrdinal;
    FontMark.CloseSourceOrder := ADocument.FontMarks[I].CloseOrdinal;
    AFragment.AddFontMark(FontMark);
  end;
end;

function SameOrderedEvent(const ALeftFragment: TTheWordClipboardFragmentV2;
  const ALeft: TClipboardOrderEventV2;
  const ARightFragment: TTheWordClipboardFragmentV2;
  const ARight: TClipboardOrderEventV2): Boolean;
var
  LeftObject, RightObject: TTheWordClipboardObjectV2;
begin
  Result := (ALeft.Kind = ARight.Kind) and
    (ALeft.Offset = ARight.Offset);
  if not Result then Exit;
  case ALeft.Kind of
    coekObject:
      begin
        LeftObject := ALeftFragment.Objects[ALeft.ObjectIndex];
        RightObject := ARightFragment.Objects[ARight.ObjectIndex];
        Result := (LeftObject.ObjectKind = RightObject.ObjectKind) and
          (LeftObject.SubKind = RightObject.SubKind) and
          (LeftObject.RawLexeme = RightObject.RawLexeme);
      end;
    coekFontOpen:
      Result := ALeftFragment.FontMarks[ALeft.FontIndex].OpenRaw =
        ARightFragment.FontMarks[ARight.FontIndex].OpenRaw;
    coekFontClose:
      Result := ALeftFragment.FontMarks[ALeft.FontIndex].CloseRaw =
        ARightFragment.FontMarks[ARight.FontIndex].CloseRaw;
  end;
end;

function FragmentMatchesDocument(AFragment: TTheWordClipboardFragmentV2;
  ADocument: TTheWordVerseDocument): Boolean;
var
  UsedMarks, UsedFonts: TBooleanArray;
  FragmentEvents, DocumentEvents: TClipboardOrderEventArray;
  DocumentFragment: TTheWordClipboardFragmentV2;
  I, J: Integer;
  Mark: TTheWordSemanticMark;
  FontMark: TTheWordFontMark;
  Found: Boolean;
begin
  Result := ADocument.VisibleText = AFragment.Text;
  if not Result then Exit;
  SetLength(UsedMarks, ADocument.MarkCount);
  for I := 0 to AFragment.MarkCount - 1 do
  begin
    Found := False;
    for J := 0 to ADocument.MarkCount - 1 do
    begin
      Mark := ADocument.Marks[J];
      if not UsedMarks[J] and (Mark.Kind = AFragment.Marks[I].Kind) and
         (Mark.StartOffset = AFragment.Marks[I].StartOffset) and
         (Mark.EndOffset = AFragment.Marks[I].EndOffset) then
      begin
        UsedMarks[J] := True;
        Found := True;
        Break;
      end;
    end;
    if not Found then Exit(False);
  end;
  if ADocument.MarkCount <> AFragment.MarkCount then Exit(False);

  SetLength(UsedFonts, ADocument.FontMarkCount);
  for I := 0 to AFragment.FontMarkCount - 1 do
  begin
    Found := False;
    for J := 0 to ADocument.FontMarkCount - 1 do
    begin
      FontMark := ADocument.FontMarks[J];
      if not UsedFonts[J] and
         (FontMark.StartOffset = AFragment.FontMarks[I].StartOffset) and
         (FontMark.EndOffset = AFragment.FontMarks[I].EndOffset) and
         (FontMark.OpenLexeme = AFragment.FontMarks[I].OpenRaw) and
         (FontMark.CloseLexeme = AFragment.FontMarks[I].CloseRaw) then
      begin
        UsedFonts[J] := True;
        Found := True;
        Break;
      end;
    end;
    if not Found then Exit(False);
  end;
  if ADocument.FontMarkCount <> AFragment.FontMarkCount then Exit(False);

  DocumentFragment := TTheWordClipboardFragmentV2.Create;
  try
    DocumentFragment.FDecodedVersion := AFragment.FDecodedVersion;
    AddDocumentTransportEvents(ADocument, DocumentFragment);
    if DocumentFragment.FDecodedVersion = THEWORD_CLIPBOARD_VERSION_V1 then
      SortFragment(DocumentFragment);
    BuildAndValidateOrderEvents(AFragment, False, FragmentEvents);
    BuildAndValidateOrderEvents(DocumentFragment, False, DocumentEvents);
    NormalizePostfixOrderAtSameOffset(AFragment, FragmentEvents);
    NormalizePostfixOrderAtSameOffset(DocumentFragment, DocumentEvents);
    if Length(FragmentEvents) <> Length(DocumentEvents) then Exit(False);
    for I := 0 to High(FragmentEvents) do
      if not SameOrderedEvent(AFragment, FragmentEvents[I],
        DocumentFragment, DocumentEvents[I]) then
        Exit(False);
    Result := True;
  finally
    DocumentFragment.Free;
  end;
end;

class function TTheWordClipboardCodecV2.BuildFragmentRaw(
  AFragment: TTheWordClipboardFragmentV2;
  out ARaw: UnicodeString): Boolean;
var
  Codec: ITheWordOntCodecV2;
  Doc, Check: TTheWordVerseDocument;
  BaseRaw: UnicodeString;
  I: Integer;
begin
  Result := False;
  ARaw := '';
  if not Assigned(AFragment) then Exit;
  Codec := TTheWordOntCodecV2.Create;
  try
    if not BuildOrderedFragmentRaw(AFragment, BaseRaw) then Exit;
    Doc := Codec.DecodeVerse(BaseRaw);
    try
      if Doc.VisibleText <> AFragment.Text then Exit;
      for I := 0 to AFragment.MarkCount - 1 do
        Doc.AddSemanticMark(AFragment.Marks[I].Kind,
          AFragment.Marks[I].StartOffset, AFragment.Marks[I].EndOffset);
      ARaw := Codec.EncodeVerse(Doc);
    finally
      Doc.Free;
    end;
    Check := Codec.DecodeVerse(ARaw);
    try
      Result := FragmentMatchesDocument(AFragment, Check);
    finally
      Check.Free;
    end;
  except
    Result := False;
  end;
end;

procedure AddDocumentObjectsForSelection(ADocument: TTheWordVerseDocument;
  AStart, ALength: Integer; AFragment: TTheWordClipboardFragmentV2);
var
  I, Finish, WordStart: Integer;
  Obj: TTheWordClipboardObjectV2;
  TitleRanges: TTheWordTitleRangeArray;

  function IsTitleAnchor(AOrdinal: Integer): Boolean;
  var
    TitleIndex: Integer;
  begin
    for TitleIndex := 0 to High(TitleRanges) do
      if (TitleRanges[TitleIndex].OpenOrdinal = AOrdinal) or
         (TitleRanges[TitleIndex].CloseOrdinal = AOrdinal) then
        Exit(True);
    Result := False;
  end;
begin
  Finish := AStart + ALength;
  CollectTheWordTitleRanges(ADocument, TitleRanges);
  for I := 0 to ADocument.LexicalItemCount - 1 do
  begin
    WordStart := ADocument.LexicalProtectedStart(I);
    if (AStart <= WordStart) and
       (Finish >= ADocument.LexicalItems[I].Offset) then
    begin
      Obj.ObjectKind := twcokLexical;
      Obj.SubKind := Ord(ADocument.LexicalItems[I].Kind);
      Obj.Offset := ADocument.LexicalItems[I].Offset - AStart;
      Obj.RawLexeme := ADocument.LexicalItems[I].RawLexeme;
      Obj.SourceOrder := ADocument.LexicalItems[I].Ordinal;
      AFragment.AddObject(Obj);
    end;
  end;
  for I := 0 to ADocument.OpaqueItemCount - 1 do
    if not IsTitleAnchor(ADocument.OpaqueItems[I].Ordinal) and
       (ADocument.OpaqueItems[I].Offset >= AStart) and
       (ADocument.OpaqueItems[I].Offset < Finish) then
    begin
      Obj.ObjectKind := twcokOpaque;
      Obj.SubKind := Ord(ADocument.OpaqueItems[I].Kind);
      Obj.Offset := ADocument.OpaqueItems[I].Offset - AStart;
      Obj.RawLexeme := ADocument.OpaqueItems[I].RawLexeme;
      Obj.SourceOrder := ADocument.OpaqueItems[I].Ordinal;
      AFragment.AddObject(Obj);
    end;
  for I := 0 to ADocument.StructuralItemCount - 1 do
    if (ADocument.StructuralItems[I].Offset >= AStart) and
       (ADocument.StructuralItems[I].Offset < Finish) then
    begin
      Obj.ObjectKind := twcokStructural;
      Obj.SubKind := Ord(ADocument.StructuralItems[I].Kind);
      Obj.Offset := ADocument.StructuralItems[I].Offset - AStart;
      Obj.RawLexeme := ADocument.StructuralItems[I].RawLexeme;
      Obj.SourceOrder := ADocument.StructuralItems[I].Ordinal;
      AFragment.AddObject(Obj);
    end;
end;

class function TTheWordClipboardCodecV2.EncodeSelection(
  ADocument: TTheWordVerseDocument; AStartUTF16, ALengthUTF16: Integer;
  out APlainText: UnicodeString; out APayload: string): Boolean;
var
  Fragment: TTheWordClipboardFragmentV2;
  Mark: TTheWordSemanticMark;
  FontMark: TTheWordFontMark;
  ClipboardMark: TTheWordClipboardMarkV2;
  ClipboardFontMark: TTheWordClipboardFontMarkV2;
  Raw: UnicodeString;
  I, Finish, MarkStart, MarkEnd: Integer;
begin
  APlainText := '';
  APayload := '';
  Result := Assigned(ADocument) and (ALengthUTF16 > 0) and
    IsValidRange(ADocument.VisibleText, AStartUTF16, ALengthUTF16);
  if not Result then Exit;
  Finish := AStartUTF16 + ALengthUTF16;
  APlainText := Copy(ADocument.VisibleText, AStartUTF16 + 1, ALengthUTF16);
  Fragment := TTheWordClipboardFragmentV2.Create;
  try
    Fragment.FText := APlainText;
    for I := 0 to ADocument.MarkCount - 1 do
    begin
      Mark := ADocument.Marks[I];
      MarkStart := Max(Mark.StartOffset, AStartUTF16);
      MarkEnd := Min(Mark.EndOffset, Finish);
      if MarkEnd <= MarkStart then Continue;
      ClipboardMark.Kind := Mark.Kind;
      ClipboardMark.StartOffset := MarkStart - AStartUTF16;
      ClipboardMark.EndOffset := MarkEnd - AStartUTF16;
      Fragment.AddMark(ClipboardMark);
    end;
    for I := 0 to ADocument.FontMarkCount - 1 do
    begin
      FontMark := ADocument.FontMarks[I];
      if FontMark.StartOffset = FontMark.EndOffset then
      begin
        if (FontMark.StartOffset < AStartUTF16) or
           (FontMark.StartOffset > Finish) then
          Continue;
      end
      else if (FontMark.EndOffset <= AStartUTF16) or
              (FontMark.StartOffset >= Finish) then
        Continue;
      ClipboardFontMark.StartOffset :=
        Max(FontMark.StartOffset, AStartUTF16) - AStartUTF16;
      ClipboardFontMark.EndOffset :=
        Min(FontMark.EndOffset, Finish) - AStartUTF16;
      ClipboardFontMark.OpenRaw := FontMark.OpenLexeme;
      ClipboardFontMark.CloseRaw := FontMark.CloseLexeme;
      ClipboardFontMark.OpenSourceOrder := FontMark.OpenOrdinal;
      ClipboardFontMark.CloseSourceOrder := FontMark.CloseOrdinal;
      Fragment.AddFontMark(ClipboardFontMark);
    end;
    AddDocumentObjectsForSelection(ADocument, AStartUTF16, ALengthUTF16,
      Fragment);
    SortFragment(Fragment);
    if not BuildFragmentRaw(Fragment, Raw) then
    begin
      if Fragment.FontMarkCount > 0 then Exit(False);
      Fragment.ClearObjects;
      if not BuildFragmentRaw(Fragment, Raw) then
        Exit(True);
    end;
    APayload := SerializeFragment(Fragment);
  finally
    Fragment.Free;
  end;
end;

class function TTheWordClipboardCodecV2.TryDecodePayload(
  const APayload: string; out AFragment: TTheWordClipboardFragmentV2): Boolean;
var
  Raw: UnicodeString;
begin
  AFragment := nil;
  try
    AFragment := DeserializeFragment(APayload);
    Result := BuildFragmentRaw(AFragment, Raw);
    if not Result then FreeAndNil(AFragment);
  except
    FreeAndNil(AFragment);
    Result := False;
  end;
end;

class function TTheWordClipboardCodecV2.NewPlainFragment(
  const AText: UnicodeString): TTheWordClipboardFragmentV2;
begin
  Result := TTheWordClipboardFragmentV2.Create;
  Result.FText := NormalizeTheWordClipboardText(AText);
end;

class function TTheWordClipboardCodecV2.BuildPasteRaw(
  ADocument: TTheWordVerseDocument; AStartUTF16, ALengthUTF16: Integer;
  AFragment: TTheWordClipboardFragmentV2;
  out ARaw: UnicodeString): Boolean;
var
  Codec: ITheWordOntCodecV2;
  Working, Check: TTheWordVerseDocument;
  ExpectedText, FragmentRaw, Marker, ContainerRaw, OriginalRaw: UnicodeString;
  MarkerPos: Integer;
begin
  Result := False;
  ARaw := '';
  if not Assigned(ADocument) or not Assigned(AFragment) or
     not ADocument.CanReplaceVisibleText(AStartUTF16, ALengthUTF16) then
    Exit;
  Codec := TTheWordOntCodecV2.Create;
  try
    if not BuildFragmentRaw(AFragment, FragmentRaw) then Exit;
    OriginalRaw := Codec.EncodeVerse(ADocument);
    Marker := FindUniqueMarker(OriginalRaw + FragmentRaw);
    Working := Codec.DecodeVerse(OriginalRaw);
    try
      Working.ReplaceVisibleText(AStartUTF16, ALengthUTF16, Marker);
      ContainerRaw := Codec.EncodeVerse(Working);
    finally
      Working.Free;
    end;
    MarkerPos := Pos(Marker, ContainerRaw);
    if MarkerPos = 0 then Exit;
    if Pos(Marker, Copy(ContainerRaw, MarkerPos + Length(Marker),
      MaxInt)) > 0 then Exit;
    ARaw := Copy(ContainerRaw, 1, MarkerPos - 1) + FragmentRaw +
      Copy(ContainerRaw, MarkerPos + Length(Marker), MaxInt);
    ExpectedText := Copy(ADocument.VisibleText, 1, AStartUTF16) +
      AFragment.Text + Copy(ADocument.VisibleText,
      AStartUTF16 + ALengthUTF16 + 1, MaxInt);
    Check := Codec.DecodeVerse(ARaw);
    try
      Result := Check.VisibleText = ExpectedText;
    finally
      Check.Free;
    end;
  except
    Result := False;
  end;
end;

function LexicalTouchesRange(ADocument: TTheWordVerseDocument;
  AIndex, AStart, AFinish: Integer): Boolean;
var
  Anchor, WordStart: Integer;
begin
  Anchor := ADocument.LexicalItems[AIndex].Offset;
  WordStart := ADocument.LexicalProtectedStart(AIndex);
  Result := ((AStart < Anchor) and (AFinish > WordStart)) or
    ((Anchor = WordStart) and (Anchor >= AStart) and (Anchor < AFinish));
end;

function ShiftObjectOffset(AOffset, AStart, AFinish: Integer): Integer;
begin
  if AOffset >= AFinish then
    Result := AOffset - (AFinish - AStart)
  else
    Result := AOffset;
end;

function MapCutOffset(AOffset, AStart, AFinish: Integer): Integer;
begin
  if AOffset <= AStart then
    Result := AOffset
  else if AOffset >= AFinish then
    Result := AOffset - (AFinish - AStart)
  else
    Result := AStart;
end;

const
  CUT_NOTE_SUBKIND = -1;

function BuildCutFragmentRaw(AFragment: TTheWordClipboardFragmentV2;
  out ARaw: UnicodeString): Boolean;
var
  Codec: ITheWordOntCodecV2;
  Document, Check: TTheWordVerseDocument;
  Expected: TTheWordClipboardFragmentV2;
  BaseRaw: UnicodeString;
  I, NoteIndex, ExpectedNoteCount: Integer;
begin
  Result := False;
  ARaw := '';
  Codec := TTheWordOntCodecV2.Create;
  Expected := nil;
  try
    if not BuildOrderedFragmentRaw(AFragment, BaseRaw) then Exit;
    Document := Codec.DecodeVerse(BaseRaw);
    try
      if Document.VisibleText <> AFragment.Text then Exit;
      for I := 0 to AFragment.MarkCount - 1 do
        Document.AddSemanticMark(AFragment.Marks[I].Kind,
          AFragment.Marks[I].StartOffset, AFragment.Marks[I].EndOffset);
      ARaw := Codec.EncodeVerse(Document);
    finally
      Document.Free;
    end;

    Check := Codec.DecodeVerse(ARaw);
    try
      Expected := TTheWordClipboardFragmentV2.Create;
      Expected.FText := AFragment.Text;
      for I := 0 to AFragment.MarkCount - 1 do
        Expected.AddMark(AFragment.Marks[I]);
      for I := 0 to AFragment.FontMarkCount - 1 do
        Expected.AddFontMark(AFragment.FontMarks[I]);
      ExpectedNoteCount := 0;
      for I := 0 to AFragment.ObjectCount - 1 do
        if (AFragment.Objects[I].ObjectKind = twcokOpaque) and
           (AFragment.Objects[I].SubKind = CUT_NOTE_SUBKIND) then
          Inc(ExpectedNoteCount)
        else
          Expected.AddObject(AFragment.Objects[I]);
      SortFragment(Expected);
      if not FragmentMatchesDocument(Expected, Check) then Exit;
      if Check.TranslatorNoteCount <> ExpectedNoteCount then Exit;
      NoteIndex := 0;
      for I := 0 to AFragment.ObjectCount - 1 do
        if (AFragment.Objects[I].ObjectKind = twcokOpaque) and
           (AFragment.Objects[I].SubKind = CUT_NOTE_SUBKIND) then
        begin
          if (Check.TranslatorNotes[NoteIndex].OffsetUTF16 <>
              AFragment.Objects[I].Offset) or
             (Check.TranslatorNotes[NoteIndex].OpenLexeme +
              Check.TranslatorNotes[NoteIndex].BodyRaw +
              Check.TranslatorNotes[NoteIndex].CloseLexeme <>
              AFragment.Objects[I].RawLexeme) then
            Exit;
          Inc(NoteIndex);
        end;
      Result := True;
    finally
      Expected.Free;
      Check.Free;
    end;
  except
    Result := False;
  end;
end;

class function TTheWordClipboardCodecV2.BuildCutRaw(
  ADocument: TTheWordVerseDocument; AStartUTF16, ALengthUTF16: Integer;
  out ARaw: UnicodeString): Boolean;
var
  Fragment: TTheWordClipboardFragmentV2;
  Mark: TTheWordSemanticMark;
  FontMark: TTheWordFontMark;
  ClipboardMark: TTheWordClipboardMarkV2;
  ClipboardFontMark: TTheWordClipboardFontMarkV2;
  Obj: TTheWordClipboardObjectV2;
  Finish, I, RangeIndex, WordStart: Integer;
  OriginalFontWasEmpty: Boolean;
  TitleRanges: TTheWordTitleRangeArray;

  function IsTitleAnchor(AOrdinal: Integer): Boolean;
  var
    TitleIndex: Integer;
  begin
    for TitleIndex := 0 to High(TitleRanges) do
      if (TitleRanges[TitleIndex].OpenOrdinal = AOrdinal) or
         (TitleRanges[TitleIndex].CloseOrdinal = AOrdinal) then
        Exit(True);
    Result := False;
  end;
begin
  Result := False;
  ARaw := '';
  if not Assigned(ADocument) or (ALengthUTF16 <= 0) or
     not IsValidRange(ADocument.VisibleText, AStartUTF16, ALengthUTF16) then
    Exit;
  Finish := AStartUTF16 + ALengthUTF16;
  for I := 0 to ADocument.TranslatorNoteCount - 1 do
    if (ADocument.TranslatorNotes[I].OffsetUTF16 > AStartUTF16) and
       (ADocument.TranslatorNotes[I].OffsetUTF16 < Finish) then
      Exit;
  CollectTheWordTitleRanges(ADocument, TitleRanges);
  for RangeIndex := 0 to High(TitleRanges) do
    if (AStartUTF16 < TitleRanges[RangeIndex].EndOffset) and
       (Finish > TitleRanges[RangeIndex].StartOffset) then
      Exit;

  { A lexical object may be cut only with its complete protected word. }
  for I := 0 to ADocument.LexicalItemCount - 1 do
    if LexicalTouchesRange(ADocument, I, AStartUTF16, Finish) then
    begin
      WordStart := ADocument.LexicalProtectedStart(I);
      if (AStartUTF16 > WordStart) or
         (Finish < ADocument.LexicalItems[I].Offset) then
        Exit;
    end;

  Fragment := TTheWordClipboardFragmentV2.Create;
  try
    Fragment.FText := Copy(ADocument.VisibleText, 1, AStartUTF16) +
      Copy(ADocument.VisibleText, Finish + 1, MaxInt);
    for I := 0 to ADocument.MarkCount - 1 do
    begin
      Mark := ADocument.Marks[I];
      ClipboardMark.Kind := Mark.Kind;
      ClipboardMark.StartOffset := Mark.StartOffset;
      ClipboardMark.EndOffset := Mark.EndOffset;
      if Mark.EndOffset <= AStartUTF16 then
        { unchanged }
      else if Mark.StartOffset >= Finish then
      begin
        Dec(ClipboardMark.StartOffset, ALengthUTF16);
        Dec(ClipboardMark.EndOffset, ALengthUTF16);
      end
      else
      begin
        if ClipboardMark.StartOffset >= AStartUTF16 then
          ClipboardMark.StartOffset := AStartUTF16;
        if ClipboardMark.EndOffset <= Finish then
          ClipboardMark.EndOffset := AStartUTF16
        else
          Dec(ClipboardMark.EndOffset, ALengthUTF16);
      end;
      if ClipboardMark.EndOffset > ClipboardMark.StartOffset then
        Fragment.AddMark(ClipboardMark);
    end;

    for I := 0 to ADocument.FontMarkCount - 1 do
    begin
      FontMark := ADocument.FontMarks[I];
      OriginalFontWasEmpty := FontMark.StartOffset = FontMark.EndOffset;
      ClipboardFontMark.StartOffset := MapCutOffset(FontMark.StartOffset,
        AStartUTF16, Finish);
      ClipboardFontMark.EndOffset := MapCutOffset(FontMark.EndOffset,
        AStartUTF16, Finish);
      ClipboardFontMark.OpenRaw := FontMark.OpenLexeme;
      ClipboardFontMark.CloseRaw := FontMark.CloseLexeme;
      ClipboardFontMark.OpenSourceOrder := FontMark.OpenOrdinal;
      ClipboardFontMark.CloseSourceOrder := FontMark.CloseOrdinal;
      if (ClipboardFontMark.EndOffset >
          ClipboardFontMark.StartOffset) or
         (OriginalFontWasEmpty and FontMark.Imported) then
        Fragment.AddFontMark(ClipboardFontMark);
    end;

    for I := 0 to ADocument.LexicalItemCount - 1 do
      if not LexicalTouchesRange(ADocument, I, AStartUTF16, Finish) then
      begin
        Obj.ObjectKind := twcokLexical;
        Obj.SubKind := Ord(ADocument.LexicalItems[I].Kind);
        Obj.Offset := ShiftObjectOffset(ADocument.LexicalItems[I].Offset,
          AStartUTF16, Finish);
        Obj.RawLexeme := ADocument.LexicalItems[I].RawLexeme;
        Obj.SourceOrder := ADocument.LexicalItems[I].Ordinal;
        Fragment.AddObject(Obj);
    end;
    { Translator notes are not part of the clipboard schema, but a cut must
      keep every remaining note losslessly. They participate as trusted raw
      events only in this internal cut fragment. }
    for I := 0 to ADocument.TranslatorNoteCount - 1 do
    begin
      Obj.ObjectKind := twcokOpaque;
      Obj.SubKind := CUT_NOTE_SUBKIND;
      Obj.Offset := MapCutOffset(
        ADocument.TranslatorNotes[I].OffsetUTF16, AStartUTF16, Finish);
      Obj.RawLexeme := ADocument.TranslatorNotes[I].OpenLexeme +
        ADocument.TranslatorNotes[I].BodyRaw +
        ADocument.TranslatorNotes[I].CloseLexeme;
      Obj.SourceOrder := ADocument.TranslatorNotes[I].Ordinal;
      Fragment.AddObject(Obj);
    end;
    for I := 0 to ADocument.OpaqueItemCount - 1 do
      if IsTitleAnchor(ADocument.OpaqueItems[I].Ordinal) or
         not ((ADocument.OpaqueItems[I].Offset >= AStartUTF16) and
              (ADocument.OpaqueItems[I].Offset < Finish)) then
      begin
        Obj.ObjectKind := twcokOpaque;
        Obj.SubKind := Ord(ADocument.OpaqueItems[I].Kind);
        Obj.Offset := ShiftObjectOffset(ADocument.OpaqueItems[I].Offset,
          AStartUTF16, Finish);
        Obj.RawLexeme := ADocument.OpaqueItems[I].RawLexeme;
        Obj.SourceOrder := ADocument.OpaqueItems[I].Ordinal;
        Fragment.AddObject(Obj);
      end;
    for I := 0 to ADocument.StructuralItemCount - 1 do
      if not ((ADocument.StructuralItems[I].Offset >= AStartUTF16) and
              (ADocument.StructuralItems[I].Offset < Finish)) then
      begin
        Obj.ObjectKind := twcokStructural;
        Obj.SubKind := Ord(ADocument.StructuralItems[I].Kind);
        Obj.Offset := ShiftObjectOffset(ADocument.StructuralItems[I].Offset,
          AStartUTF16, Finish);
        Obj.RawLexeme := ADocument.StructuralItems[I].RawLexeme;
        Obj.SourceOrder := ADocument.StructuralItems[I].Ordinal;
        Fragment.AddObject(Obj);
      end;
    SortFragment(Fragment);
    Result := BuildCutFragmentRaw(Fragment, ARaw);
  finally
    Fragment.Free;
  end;
end;

end.
