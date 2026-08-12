unit ConcordanceExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, StrUtils, Math, PCRE, LazUTF8;

type
  TConcordanceExportMode = (cemAnalytical, cemSynthetic);
  TConcordanceContextMode = (
    ccmReferencesOnly,
    ccmRelevantExcerpt,
    ccmFullVerse
  );

  TConcordanceHighlightRange = record
    StartIndex: Integer;
    Length: Integer;
  end;

  TConcordanceHighlightRangeArray = array of TConcordanceHighlightRange;

const
  CONCORDANCE_CONTEXT_MIN_CHARACTERS = 40;
  CONCORDANCE_CONTEXT_DEFAULT_CHARACTERS = 120;
  CONCORDANCE_CONTEXT_MAX_CHARACTERS = 500;

function UnicodeToRtf(const Text: string): string;
function MakeStrongLink(const Strong, Anchor: string): string;
function MakeMorphologyLink(const Morphology, Anchor: string): string;

type
  TConcordance = class;

  TConcordanceReference = class
  private
    FContext: string;
    FOccurrences: Integer;
    FOrder: Integer;
    FReferenceID: string;
    FHighlightRanges: TConcordanceHighlightRangeArray;
    procedure AddHighlightRanges(
      const Ranges: TConcordanceHighlightRangeArray);
  public
    constructor Create(const AReferenceID, AContext: string;
      AOccurrences, AOrder: Integer;
      const AHighlightRanges: TConcordanceHighlightRangeArray);
    procedure Add(AOccurrences: Integer; const AContext: string;
      AOrder: Integer;
      const AHighlightRanges: TConcordanceHighlightRangeArray);
    property Context: string read FContext;
    property HighlightRanges: TConcordanceHighlightRangeArray
      read FHighlightRanges;
    property Occurrences: Integer read FOccurrences;
    property Order: Integer read FOrder;
    property ReferenceID: string read FReferenceID;
  end;

  TConcordanceTranslation = class
  private
    FReferences: TFPHashList;
    FText: string;
    FTotalOccurrences: Integer;
    function GetReferenceCount: Integer;
    function GetReference(Index: Integer): TConcordanceReference;
  public
    constructor Create(const AText: string);
    destructor Destroy; override;
    procedure AddReference(const ReferenceID, Context: string;
      Occurrences, ReferenceOrder: Integer;
      const HighlightRanges: TConcordanceHighlightRangeArray);
    property ReferenceCount: Integer read GetReferenceCount;
    property References[Index: Integer]: TConcordanceReference
      read GetReference;
    property Text: string read FText;
    property TotalOccurrences: Integer read FTotalOccurrences;
  end;

  TConcordancePhrase = class
  private
    FKey: string;
    FPhrase: string;
    FTotalOccurrences: Integer;
    FTranslations: TFPHashList;
    function GetTranslationCount: Integer;
    function GetTranslation(Index: Integer): TConcordanceTranslation;
  public
    constructor Create(const AKey, APhrase: string);
    destructor Destroy; override;
    procedure AddTranslation(const Translation, ReferenceID, Context: string;
      Occurrences, ReferenceOrder: Integer;
      const HighlightRanges: TConcordanceHighlightRangeArray);
    property Key: string read FKey;
    property Phrase: string read FPhrase;
    property TotalOccurrences: Integer read FTotalOccurrences;
    property TranslationCount: Integer read GetTranslationCount;
    property Translations[Index: Integer]: TConcordanceTranslation
      read GetTranslation;
  end;

  TConcordanceStrong = class
  private
    FPhrases: TFPHashList;
    FReferences: TFPHashList;
    FStrong: string;
    FTotalOccurrences: Integer;
    FTranslations: TFPHashList;
    function GetPhraseCount: Integer;
    function GetPhrase(Index: Integer): TConcordancePhrase;
    function GetReferenceCount: Integer;
    function GetTranslationCount: Integer;
    function GetTranslation(Index: Integer): TConcordanceTranslation;
  public
    constructor Create(const AStrong: string);
    destructor Destroy; override;
    procedure AddOccurrence(Phrase: TConcordancePhrase;
      const Translation, ReferenceID, Context: string;
      Occurrences, ReferenceOrder: Integer; AggregateTranslations: Boolean;
      const HighlightRanges: TConcordanceHighlightRangeArray);
    property PhraseCount: Integer read GetPhraseCount;
    property Phrases[Index: Integer]: TConcordancePhrase read GetPhrase;
    property ReferenceCount: Integer read GetReferenceCount;
    property Strong: string read FStrong;
    property TotalOccurrences: Integer read FTotalOccurrences;
    property TranslationCount: Integer read GetTranslationCount;
    property Translations[Index: Integer]: TConcordanceTranslation
      read GetTranslation;
  end;

  TConcordance = class
  private
    FContextMaxCharacters: Integer;
    FContextMode: TConcordanceContextMode;
    FMode: TConcordanceExportMode;
    FPhrases: TFPHashList;
    FRemoveMorphologyRegex: IRegex;
    FStrongRegex: IRegex;
    FSyntagmRegex: IRegex;
    FTagRegex: IRegex;
    FStrongs: TFPHashList;
    FNextReferenceOrder: Integer;
    function GetIncludeVerseContext: Boolean;
    function GetDetailed: Boolean;
    procedure SetContextMaxCharacters(AValue: Integer);
    procedure SetIncludeVerseContext(AValue: Boolean);
    procedure SetDetailed(AValue: Boolean);
    function GetOrCreatePhrase(const SourcePhrase: string): TConcordancePhrase;
    function GetOrCreateStrong(const Strong: string): TConcordanceStrong;
    function GetPhraseRtf(Phrase: TConcordancePhrase): string;
    function GetTranslationRtf(Translation: TConcordanceTranslation): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddOccurrence(const SourcePhrase, TargetPhrase,
      ReferenceID, DestinationContext: string; Occurrences: Integer;
      ReferenceOrder: Integer = -1); overload;
    procedure AddOccurrence(const SourcePhrase, TargetPhrase,
      ReferenceID, DestinationContext: string; Occurrences,
      ReferenceOrder: Integer;
      const HighlightRanges: TConcordanceHighlightRangeArray); overload;
    procedure AddPhrase(Pairs: TStringList; const Ref: string);
    function GetStrongRtf(const Lemma, Strong: string): string;
    property ContextMaxCharacters: Integer read FContextMaxCharacters
      write SetContextMaxCharacters;
    property ContextMode: TConcordanceContextMode read FContextMode
      write FContextMode;
    property Detailed: Boolean read GetDetailed write SetDetailed;
    property IncludeVerseContext: Boolean read GetIncludeVerseContext
      write SetIncludeVerseContext;
    property Mode: TConcordanceExportMode read FMode write FMode;
  end;

implementation

resourcestring
  SConcordanceOneOccurrence = '1 occurrence';
  SConcordanceManyOccurrences = '%d occurrences';
  SConcordanceOneVerse = '1 verse';
  SConcordanceManyVerses = '%d verses';
  SConcordanceCountSummary = '%s in %s';

function OccurrenceText(Count: Integer): string;
begin
  if Count = 1 then
    Result := SConcordanceOneOccurrence
  else
    Result := Format(SConcordanceManyOccurrences, [Count]);
end;

function VerseText(Count: Integer): string;
begin
  if Count = 1 then
    Result := SConcordanceOneVerse
  else
    Result := Format(SConcordanceManyVerses, [Count]);
end;

function CountSummary(Occurrences, Verses: Integer): string;
begin
  Result := Format(SConcordanceCountSummary,
    [OccurrenceText(Occurrences), VerseText(Verses)]);
end;

function UnicodeToRtf(const Text: string): string;
var
  Builder: TStringBuilder;
  CodeUnit: Word;
  Index: Integer;
  SignedCodeUnit: SmallInt;
  UTF16Text: UnicodeString;
begin
  Builder := TStringBuilder.Create;
  try
    UTF16Text := UTF8ToUTF16(Text);
    for Index := 1 to Length(UTF16Text) do
    begin
      CodeUnit := Ord(UTF16Text[Index]);
      if CodeUnit = 10 then
        Builder.Append('\line ')
      else if CodeUnit = 13 then
        Continue
      else if (CodeUnit = Ord('\')) or (CodeUnit = Ord('{')) or
        (CodeUnit = Ord('}')) then
      begin
        Builder.Append('\');
        Builder.Append(Char(CodeUnit));
      end
      else if (CodeUnit >= 32) and (CodeUnit <= 126) then
        Builder.Append(Char(CodeUnit))
      else
      begin
        SignedCodeUnit := SmallInt(CodeUnit);
        Builder.Append(Format('\u%d?', [SignedCodeUnit]));
      end;
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function ScriptFont(const Text: string): string;
var
  CharLength: Integer;
  CodePoint: Cardinal;
  Cursor: PChar;
begin
  Result := '\f0';
  Cursor := PChar(Text);
  repeat
    CodePoint := UTF8CodepointToUnicode(Cursor, CharLength);
    if (CodePoint >= $0370) and (CodePoint <= $03FF) then
      Exit('\f1');
    if (CodePoint >= $0590) and (CodePoint <= $05FF) then
      Exit('\f2');
    Inc(Cursor, CharLength);
  until (CharLength = 0) or (CodePoint = 0);
end;

function MakeStrongLink(const Strong, Anchor: string): string;
begin
  Result := Format(
    '{\field{\*\fldinst HYPERLINK "tw://[strong]?%s"}' +
    '{\fldrslt%s\b %s}}',
    [UpperCase(Strong), ScriptFont(Anchor), UnicodeToRtf(Anchor)]);
end;

function MakeMorphologyLink(const Morphology, Anchor: string): string;
begin
  Result := Format(
    '{\field{\*\fldinst HYPERLINK "tw://[morph]?%s"}' +
    '{\fldrslt\cf6 %s}}',
    [UpperCase(Morphology), UnicodeToRtf(Anchor)]);
end;

function MakeReferenceLink(const ReferenceID: string): string;
begin
  Result := Format(
    '{\field{\*\fldinst HYPERLINK "tw://bible.*?id=%s"}' +
    '{\fldrslt\cf2 [vref]}}',
    [StringReplace(ReferenceID, ',', '.', [rfReplaceAll])]);
end;

function RemoveMarkupTags(const Value: string): string;
var
  Builder: TStringBuilder;
  Index: Integer;
  TagEnd: Integer;
begin
  Builder := TStringBuilder.Create;
  try
    Index := 1;
    while Index <= Length(Value) do
    begin
      if Value[Index] = '<' then
      begin
        TagEnd := PosEx('>', Value, Index + 1);
        if TagEnd > 0 then
        begin
          Index := TagEnd + 1;
          Continue;
        end;
      end;
      Builder.Append(Value[Index]);
      Inc(Index);
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function FormatTargetPhrase(const Value: string): string;
begin
  Result := RemoveMarkupTags(Value);
  Result := Result
    .Replace('-;-', '-')
    .Replace('r-ei', 'rei')
    .Replace('r-ás', 'rás')
    .Replace('r-á', 'rá')
    .Replace('r-emos', 'remos')
    .Replace('r-eis', 'reis')
    .Replace('r-ão', 'rão')
    .Replace('r-ia', 'ria')
    .Replace('r-ias', 'rias')
    .Replace('r-íamos', 'ríamos')
    .Replace('r-íeis', 'ríeis')
    .Replace('r-iam', 'riam')
    .Replace('á-ei', 'arei')
    .Replace('á-ás', 'arás')
    .Replace('á-á', 'ará')
    .Replace('á-emos', 'aremos')
    .Replace('á-eis', 'areis')
    .Replace('á-ão', 'arão')
    .Replace('á-ia', 'aria')
    .Replace('á-ias', 'arias')
    .Replace('á-íamos', 'aríamos')
    .Replace('á-íeis', 'aríeis')
    .Replace('á-iam', 'ariam')
    .Replace(';', ' ')
    .Replace('- ', ' ');

  if Result.EndsWith('á-') then
    Result := Result.Replace('á-', 'ar');
  if Result.EndsWith('ê-') then
    Result := Result.Replace('ê-', 'er');
  Result := Trim(Result);
  if Result.EndsWith('-') then
    Delete(Result, Length(Result), 1);
end;

function NormalizeHighlightRanges(const Context: string;
  const Ranges: TConcordanceHighlightRangeArray;
  out Normalized: TConcordanceHighlightRangeArray): Boolean;
var
  CurrentEnd: Integer;
  Index: Integer;
  InsertIndex: Integer;
  NormalizedIndex: Integer;
  RangeValue: TConcordanceHighlightRange;
begin
  Result := False;
  Normalized := nil;
  if Context.IsEmpty or (Length(Ranges) = 0) then
    Exit;

  SetLength(Normalized, Length(Ranges));
  for Index := 0 to High(Ranges) do
  begin
    if (Ranges[Index].StartIndex < 1) or (Ranges[Index].Length <= 0) or
       (Ranges[Index].StartIndex > Length(Context)) or
       (Ranges[Index].Length >
        Length(Context) - Ranges[Index].StartIndex + 1) then
    begin
      SetLength(Normalized, 0);
      Exit;
    end;
    Normalized[Index] := Ranges[Index];
  end;

  for Index := 1 to High(Normalized) do
  begin
    RangeValue := Normalized[Index];
    InsertIndex := Index - 1;
    while (InsertIndex >= 0) and
          (Normalized[InsertIndex].StartIndex > RangeValue.StartIndex) do
    begin
      Normalized[InsertIndex + 1] := Normalized[InsertIndex];
      Dec(InsertIndex);
    end;
    Normalized[InsertIndex + 1] := RangeValue;
  end;

  NormalizedIndex := 0;
  for Index := 1 to High(Normalized) do
  begin
    CurrentEnd := Normalized[NormalizedIndex].StartIndex +
      Normalized[NormalizedIndex].Length - 1;
    if Normalized[Index].StartIndex <= CurrentEnd then
    begin
      CurrentEnd := Max(CurrentEnd, Normalized[Index].StartIndex +
        Normalized[Index].Length - 1);
      Normalized[NormalizedIndex].Length := CurrentEnd -
        Normalized[NormalizedIndex].StartIndex + 1;
    end
    else
    begin
      Inc(NormalizedIndex);
      Normalized[NormalizedIndex] := Normalized[Index];
    end;
  end;
  SetLength(Normalized, NormalizedIndex + 1);
  Result := True;
end;

type
  TUTF8ByteOffsetArray = array of Integer;

procedure BuildUTF8ByteOffsets(const Value: string;
  out Offsets: TUTF8ByteOffsetArray; out CharacterCount: Integer);
var
  ByteIndex: Integer;
  CharLength: Integer;
  CodePoint: Cardinal;
  Cursor: PChar;
begin
  CharacterCount := 0;
  Offsets := nil;
  SetLength(Offsets, Length(Value) + 2);
  ByteIndex := 1;
  while ByteIndex <= Length(Value) do
  begin
    Inc(CharacterCount);
    Offsets[CharacterCount] := ByteIndex;
    Cursor := PChar(Value) + ByteIndex - 1;
    CodePoint := UTF8CodepointToUnicode(Cursor, CharLength);
    if (CodePoint = 0) or (CharLength <= 0) or
       (ByteIndex + CharLength - 1 > Length(Value)) then
      CharLength := 1;
    Inc(ByteIndex, CharLength);
  end;
  Offsets[CharacterCount + 1] := Length(Value) + 1;
  SetLength(Offsets, CharacterCount + 2);
end;

function CharacterIndexForByteOffset(const Offsets: TUTF8ByteOffsetArray;
  ByteOffset: Integer): Integer;
begin
  for Result := 1 to High(Offsets) do
    if Offsets[Result] = ByteOffset then
      Exit;
  Result := 0;
end;

function CharacterCodePoint(const Value: string;
  const Offsets: TUTF8ByteOffsetArray; CharacterIndex: Integer): Cardinal;
var
  CharLength: Integer;
  Cursor: PChar;
begin
  Result := 0;
  if (CharacterIndex < 1) or (CharacterIndex >= High(Offsets)) then
    Exit;
  Cursor := PChar(Value) + Offsets[CharacterIndex] - 1;
  Result := UTF8CodepointToUnicode(Cursor, CharLength);
end;

function IsContextBoundary(CodePoint: Cardinal): Boolean;
begin
  Result := (CodePoint <= 32) or (CodePoint = $A0) or
    ((CodePoint >= Ord('!')) and (CodePoint <= Ord('/'))) or
    ((CodePoint >= Ord(':')) and (CodePoint <= Ord('@'))) or
    ((CodePoint >= $5B) and (CodePoint <= $60)) or
    ((CodePoint >= Ord('{')) and (CodePoint <= Ord('~'))) or
    ((CodePoint >= $2000) and (CodePoint <= $206F));
end;

procedure BuildRelevantExcerpt(const Context: string;
  const HighlightRanges: TConcordanceHighlightRangeArray;
  MaxCharacters: Integer; out Excerpt: string;
  out ExcerptRanges: TConcordanceHighlightRangeArray);
var
  ByteOffsets: TUTF8ByteOffsetArray;
  CharacterCount: Integer;
  EffectiveMax: Integer;
  Ellipsis: string;
  EndByte: Integer;
  EndCharacter: Integer;
  Expanded: Boolean;
  FirstHighlightCharacter: Integer;
  Index: Integer;
  LastHighlightCharacter: Integer;
  LeftExpansion: Integer;
  NormalizedRanges: TConcordanceHighlightRangeArray;
  Prefix: string;
  PrefixByteLength: Integer;
  RightExpansion: Integer;
  StartByte: Integer;
  StartCharacter: Integer;
  Suffix: string;

  function VisibleLength(AStartCharacter, AEndCharacter: Integer): Integer;
  begin
    Result := AEndCharacter - AStartCharacter + 1;
    if AStartCharacter > 1 then
      Inc(Result, 2);
    if AEndCharacter < CharacterCount then
      Inc(Result, 2);
  end;

  function TryExpandLeft: Boolean;
  begin
    Result := (StartCharacter > 1) and
      (VisibleLength(StartCharacter - 1, EndCharacter) <= EffectiveMax);
    if Result then
    begin
      Dec(StartCharacter);
      Inc(LeftExpansion);
    end;
  end;

  function TryExpandRight: Boolean;
  begin
    Result := (EndCharacter < CharacterCount) and
      (VisibleLength(StartCharacter, EndCharacter + 1) <= EffectiveMax);
    if Result then
    begin
      Inc(EndCharacter);
      Inc(RightExpansion);
    end;
  end;

begin
  Excerpt := Context;
  ExcerptRanges := HighlightRanges;
  if Context.IsEmpty then
    Exit;

  BuildUTF8ByteOffsets(Context, ByteOffsets, CharacterCount);
  EffectiveMax := MaxCharacters;
  if EffectiveMax < 1 then
    EffectiveMax := CONCORDANCE_CONTEXT_DEFAULT_CHARACTERS;
  if CharacterCount <= EffectiveMax then
    Exit;
  if not NormalizeHighlightRanges(Context, HighlightRanges,
      NormalizedRanges) then
    Exit;

  for Index := 0 to High(NormalizedRanges) do
    if (CharacterIndexForByteOffset(ByteOffsets,
        NormalizedRanges[Index].StartIndex) = 0) or
       (CharacterIndexForByteOffset(ByteOffsets,
        NormalizedRanges[Index].StartIndex +
        NormalizedRanges[Index].Length) = 0) then
      Exit;

  FirstHighlightCharacter := CharacterIndexForByteOffset(ByteOffsets,
    NormalizedRanges[0].StartIndex);
  LastHighlightCharacter := CharacterIndexForByteOffset(ByteOffsets,
    NormalizedRanges[High(NormalizedRanges)].StartIndex +
    NormalizedRanges[High(NormalizedRanges)].Length) - 1;
  if (FirstHighlightCharacter < 1) or
     (LastHighlightCharacter < FirstHighlightCharacter) then
    Exit;

  StartCharacter := FirstHighlightCharacter;
  EndCharacter := LastHighlightCharacter;
  LeftExpansion := 0;
  RightExpansion := 0;
  repeat
    if LeftExpansion <= RightExpansion then
    begin
      Expanded := TryExpandLeft;
      if not Expanded then
        Expanded := TryExpandRight;
    end
    else
    begin
      Expanded := TryExpandRight;
      if not Expanded then
        Expanded := TryExpandLeft;
    end;
  until not Expanded;

  if StartCharacter > 1 then
  begin
    while (StartCharacter < FirstHighlightCharacter) and
          not IsContextBoundary(CharacterCodePoint(Context, ByteOffsets,
            StartCharacter - 1)) and
          not IsContextBoundary(CharacterCodePoint(Context, ByteOffsets,
            StartCharacter)) do
      Inc(StartCharacter);
    while (StartCharacter < FirstHighlightCharacter) and
          IsContextBoundary(CharacterCodePoint(Context, ByteOffsets,
            StartCharacter)) do
      Inc(StartCharacter);
  end;

  if EndCharacter < CharacterCount then
  begin
    while (EndCharacter > LastHighlightCharacter) and
          not IsContextBoundary(CharacterCodePoint(Context, ByteOffsets,
            EndCharacter)) and
          not IsContextBoundary(CharacterCodePoint(Context, ByteOffsets,
            EndCharacter + 1)) do
      Dec(EndCharacter);
    while (EndCharacter > LastHighlightCharacter) and
          IsContextBoundary(CharacterCodePoint(Context, ByteOffsets,
            EndCharacter)) do
      Dec(EndCharacter);
  end;

  Ellipsis := UTF8Encode(WideChar($2026));
  Prefix := '';
  Suffix := '';
  if StartCharacter > 1 then
    Prefix := Ellipsis + ' ';
  if EndCharacter < CharacterCount then
    Suffix := ' ' + Ellipsis;

  StartByte := ByteOffsets[StartCharacter];
  EndByte := ByteOffsets[EndCharacter + 1];
  Excerpt := Prefix + Copy(Context, StartByte, EndByte - StartByte) + Suffix;
  PrefixByteLength := Length(Prefix);
  SetLength(ExcerptRanges, Length(NormalizedRanges));
  for Index := 0 to High(NormalizedRanges) do
  begin
    ExcerptRanges[Index].StartIndex := PrefixByteLength +
      NormalizedRanges[Index].StartIndex - StartByte + 1;
    ExcerptRanges[Index].Length := NormalizedRanges[Index].Length;
  end;
end;

function HighlightContextRanges(const Context: string;
  const HighlightRanges: TConcordanceHighlightRangeArray): string;
var
  Builder: TStringBuilder;
  Cursor: Integer;
  Index: Integer;
  Ranges: TConcordanceHighlightRangeArray;
begin
  if not NormalizeHighlightRanges(Context, HighlightRanges, Ranges) then
    Exit(UnicodeToRtf(Context));

  Builder := TStringBuilder.Create;
  try
    Cursor := 1;
    for Index := 0 to Length(Ranges) - 1 do
    begin
      if Ranges[Index].StartIndex > Cursor then
        Builder.Append(UnicodeToRtf(Copy(Context, Cursor,
          Ranges[Index].StartIndex - Cursor)));
      Builder.Append('{\b\ul ');
      Builder.Append(UnicodeToRtf(Copy(Context, Ranges[Index].StartIndex,
        Ranges[Index].Length)));
      Builder.Append('}');
      Cursor := Ranges[Index].StartIndex + Ranges[Index].Length;
    end;
    if Cursor <= Length(Context) then
      Builder.Append(UnicodeToRtf(Copy(Context, Cursor,
        Length(Context) - Cursor + 1)));
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function ComparePhrases(Item1, Item2: Pointer): Integer;
var
  Left: TConcordancePhrase;
  Right: TConcordancePhrase;
begin
  Left := TConcordancePhrase(Item1);
  Right := TConcordancePhrase(Item2);
  if Left.TotalOccurrences <> Right.TotalOccurrences then
    Exit(Right.TotalOccurrences - Left.TotalOccurrences);
  Result := CompareStr(UTF8LowerCase(Left.Phrase),
    UTF8LowerCase(Right.Phrase));
end;

function CompareTranslations(Item1, Item2: Pointer): Integer;
var
  Left: TConcordanceTranslation;
  Right: TConcordanceTranslation;
begin
  Left := TConcordanceTranslation(Item1);
  Right := TConcordanceTranslation(Item2);
  if Left.TotalOccurrences <> Right.TotalOccurrences then
    Exit(Right.TotalOccurrences - Left.TotalOccurrences);
  Result := CompareStr(UTF8LowerCase(FormatTargetPhrase(Left.Text)),
    UTF8LowerCase(FormatTargetPhrase(Right.Text)));
end;

function CompareReferences(Item1, Item2: Pointer): Integer;
var
  Left: TConcordanceReference;
  Right: TConcordanceReference;
begin
  Left := TConcordanceReference(Item1);
  Right := TConcordanceReference(Item2);
  Result := Left.Order - Right.Order;
  if Result = 0 then
    Result := CompareStr(Left.ReferenceID, Right.ReferenceID);
end;

{ TConcordanceReference }

constructor TConcordanceReference.Create(const AReferenceID, AContext: string;
  AOccurrences, AOrder: Integer;
  const AHighlightRanges: TConcordanceHighlightRangeArray);
begin
  inherited Create;
  FReferenceID := AReferenceID;
  FContext := AContext;
  FOccurrences := AOccurrences;
  FOrder := AOrder;
  AddHighlightRanges(AHighlightRanges);
end;

procedure TConcordanceReference.AddHighlightRanges(
  const Ranges: TConcordanceHighlightRangeArray);
var
  ExistingIndex: Integer;
  RangeIndex: Integer;
  RangeExists: Boolean;
begin
  for RangeIndex := 0 to High(Ranges) do
  begin
    RangeExists := False;
    for ExistingIndex := 0 to High(FHighlightRanges) do
      if (FHighlightRanges[ExistingIndex].StartIndex =
          Ranges[RangeIndex].StartIndex) and
         (FHighlightRanges[ExistingIndex].Length =
          Ranges[RangeIndex].Length) then
      begin
        RangeExists := True;
        Break;
      end;
    if RangeExists then
      Continue;
    ExistingIndex := Length(FHighlightRanges);
    SetLength(FHighlightRanges, ExistingIndex + 1);
    FHighlightRanges[ExistingIndex] := Ranges[RangeIndex];
  end;
end;

procedure TConcordanceReference.Add(AOccurrences: Integer;
  const AContext: string; AOrder: Integer;
  const AHighlightRanges: TConcordanceHighlightRangeArray);
begin
  Inc(FOccurrences, AOccurrences);
  if FContext.IsEmpty and not AContext.IsEmpty then
    FContext := AContext;
  AddHighlightRanges(AHighlightRanges);
  if (AOrder >= 0) and ((FOrder < 0) or (AOrder < FOrder)) then
    FOrder := AOrder;
end;

{ TConcordanceTranslation }

constructor TConcordanceTranslation.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
  FReferences := TFPHashList.Create;
end;

destructor TConcordanceTranslation.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FReferences.Count - 1 do
    TObject(FReferences[Index]).Free;
  FReferences.Free;
  inherited Destroy;
end;

function TConcordanceTranslation.GetReferenceCount: Integer;
begin
  Result := FReferences.Count;
end;

function TConcordanceTranslation.GetReference(
  Index: Integer): TConcordanceReference;
begin
  if (Index < 0) or (Index >= FReferences.Count) then
    Exit(nil);
  Result := TConcordanceReference(FReferences[Index]);
end;

procedure TConcordanceTranslation.AddReference(const ReferenceID,
  Context: string; Occurrences, ReferenceOrder: Integer;
  const HighlightRanges: TConcordanceHighlightRangeArray);
var
  Reference: TConcordanceReference;
begin
  if Occurrences <= 0 then
    Exit;
  Reference := TConcordanceReference(FReferences.Find(ReferenceID));
  if not Assigned(Reference) then
  begin
    Reference := TConcordanceReference.Create(ReferenceID, Context,
      Occurrences, ReferenceOrder, HighlightRanges);
    FReferences.Add(ReferenceID, Reference);
  end
  else
    Reference.Add(Occurrences, Context, ReferenceOrder, HighlightRanges);
  Inc(FTotalOccurrences, Occurrences);
end;

{ TConcordancePhrase }

constructor TConcordancePhrase.Create(const AKey, APhrase: string);
begin
  inherited Create;
  FKey := AKey;
  FPhrase := APhrase;
  FTranslations := TFPHashList.Create;
end;

destructor TConcordancePhrase.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FTranslations.Count - 1 do
    TObject(FTranslations[Index]).Free;
  FTranslations.Free;
  inherited Destroy;
end;

function TConcordancePhrase.GetTranslationCount: Integer;
begin
  Result := FTranslations.Count;
end;

function TConcordancePhrase.GetTranslation(
  Index: Integer): TConcordanceTranslation;
begin
  if (Index < 0) or (Index >= FTranslations.Count) then
    Exit(nil);
  Result := TConcordanceTranslation(FTranslations[Index]);
end;

procedure TConcordancePhrase.AddTranslation(const Translation, ReferenceID,
  Context: string; Occurrences, ReferenceOrder: Integer;
  const HighlightRanges: TConcordanceHighlightRangeArray);
var
  TranslationEntry: TConcordanceTranslation;
begin
  TranslationEntry := TConcordanceTranslation(FTranslations.Find(Translation));
  if not Assigned(TranslationEntry) then
  begin
    TranslationEntry := TConcordanceTranslation.Create(Translation);
    FTranslations.Add(Translation, TranslationEntry);
  end;
  TranslationEntry.AddReference(ReferenceID, Context, Occurrences,
    ReferenceOrder, HighlightRanges);
  Inc(FTotalOccurrences, Occurrences);
end;

{ TConcordanceStrong }

constructor TConcordanceStrong.Create(const AStrong: string);
begin
  inherited Create;
  FStrong := AStrong;
  FPhrases := TFPHashList.Create;
  FReferences := TFPHashList.Create;
  FTranslations := TFPHashList.Create;
end;

destructor TConcordanceStrong.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FReferences.Count - 1 do
    TObject(FReferences[Index]).Free;
  for Index := 0 to FTranslations.Count - 1 do
    TObject(FTranslations[Index]).Free;
  FPhrases.Free;
  FReferences.Free;
  FTranslations.Free;
  inherited Destroy;
end;

function TConcordanceStrong.GetPhraseCount: Integer;
begin
  Result := FPhrases.Count;
end;

function TConcordanceStrong.GetPhrase(Index: Integer): TConcordancePhrase;
begin
  if (Index < 0) or (Index >= FPhrases.Count) then
    Exit(nil);
  Result := TConcordancePhrase(FPhrases[Index]);
end;

function TConcordanceStrong.GetReferenceCount: Integer;
begin
  Result := FReferences.Count;
end;

function TConcordanceStrong.GetTranslationCount: Integer;
begin
  Result := FTranslations.Count;
end;

function TConcordanceStrong.GetTranslation(
  Index: Integer): TConcordanceTranslation;
begin
  if (Index < 0) or (Index >= FTranslations.Count) then
    Exit(nil);
  Result := TConcordanceTranslation(FTranslations[Index]);
end;

procedure TConcordanceStrong.AddOccurrence(Phrase: TConcordancePhrase;
  const Translation, ReferenceID, Context: string; Occurrences,
  ReferenceOrder: Integer; AggregateTranslations: Boolean;
  const HighlightRanges: TConcordanceHighlightRangeArray);
var
  Reference: TConcordanceReference;
  TranslationEntry: TConcordanceTranslation;
begin
  if FPhrases.Find(Phrase.Key) = nil then
    FPhrases.Add(Phrase.Key, Phrase);

  Reference := TConcordanceReference(FReferences.Find(ReferenceID));
  if not Assigned(Reference) then
  begin
    Reference := TConcordanceReference.Create(ReferenceID, '', Occurrences,
      ReferenceOrder, nil);
    FReferences.Add(ReferenceID, Reference);
  end
  else
    Reference.Add(Occurrences, '', ReferenceOrder, nil);

  if AggregateTranslations then
  begin
    TranslationEntry := TConcordanceTranslation(
      FTranslations.Find(Translation));
    if not Assigned(TranslationEntry) then
    begin
      TranslationEntry := TConcordanceTranslation.Create(Translation);
      FTranslations.Add(Translation, TranslationEntry);
    end;
    TranslationEntry.AddReference(ReferenceID, Context, Occurrences,
      ReferenceOrder, HighlightRanges);
  end;
  Inc(FTotalOccurrences, Occurrences);
end;

{ TConcordance }

constructor TConcordance.Create;
begin
  inherited Create;
  FContextMaxCharacters := CONCORDANCE_CONTEXT_DEFAULT_CHARACTERS;
  FContextMode := ccmReferencesOnly;
  FMode := cemAnalytical;
  FPhrases := TFPHashList.Create;
  FStrongs := TFPHashList.Create;
  FRemoveMorphologyRegex := RegexCreate('<WT[^>]+>',
    [rcoUTF8, rcoIgnoreCase]);
  FStrongRegex := RegexCreate('<W([GH][^<]+)>',
    [rcoUTF8, rcoIgnoreCase]);
  FSyntagmRegex := RegexCreate('^([^<]+)?((?:<[^>]+>)+)$',
    [rcoUTF8, rcoIgnoreCase]);
  FTagRegex := RegexCreate('<W([GH][^<]+)>|<WT([^ <]+)(?: l="([^"]+)")?>',
    [rcoUTF8, rcoIgnoreCase]);
end;

destructor TConcordance.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FStrongs.Count - 1 do
    TObject(FStrongs[Index]).Free;
  for Index := 0 to FPhrases.Count - 1 do
    TObject(FPhrases[Index]).Free;
  FStrongs.Free;
  FPhrases.Free;
  inherited Destroy;
end;

function TConcordance.GetIncludeVerseContext: Boolean;
begin
  Result := FContextMode <> ccmReferencesOnly;
end;

function TConcordance.GetDetailed: Boolean;
begin
  Result := FMode = cemAnalytical;
end;

procedure TConcordance.SetContextMaxCharacters(AValue: Integer);
begin
  if AValue < CONCORDANCE_CONTEXT_MIN_CHARACTERS then
    FContextMaxCharacters := CONCORDANCE_CONTEXT_MIN_CHARACTERS
  else if AValue > CONCORDANCE_CONTEXT_MAX_CHARACTERS then
    FContextMaxCharacters := CONCORDANCE_CONTEXT_MAX_CHARACTERS
  else
    FContextMaxCharacters := AValue;
end;

procedure TConcordance.SetIncludeVerseContext(AValue: Boolean);
begin
  if not AValue then
    FContextMode := ccmReferencesOnly
  else if FContextMode = ccmReferencesOnly then
    FContextMode := ccmFullVerse;
end;

procedure TConcordance.SetDetailed(AValue: Boolean);
begin
  if AValue then
    FMode := cemAnalytical
  else
    FMode := cemSynthetic;
end;

function ExtractTags(const Value: string): string;
var
  ByteIndex: Integer;
  CharLength: Integer;
  CodePoint: Cardinal;
  Cursor: PChar;
  InsideTag: Boolean;
begin
  Result := '';
  InsideTag := False;
  Cursor := PChar(Value);
  repeat
    CodePoint := UTF8CodepointToUnicode(Cursor, CharLength);
    if (CharLength = 1) and (CodePoint = Ord('<')) then
      InsideTag := True;
    if (CodePoint > 0) and InsideTag then
      for ByteIndex := 0 to CharLength - 1 do
        Result := Result + Cursor[ByteIndex];
    if InsideTag and (CharLength = 1) and (CodePoint = Ord('>')) then
      InsideTag := False;
    Inc(Cursor, CharLength);
  until (CharLength = 0) or (CodePoint = 0);
end;

function TConcordance.GetOrCreatePhrase(
  const SourcePhrase: string): TConcordancePhrase;
var
  Key: string;
  PhraseText: string;
begin
  if FMode = cemAnalytical then
  begin
    Key := SourcePhrase;
    PhraseText := SourcePhrase;
  end
  else
  begin
    PhraseText := FRemoveMorphologyRegex.Replace(SourcePhrase, '');
    Key := ExtractTags(PhraseText);
    PhraseText := Key;
  end;

  Result := TConcordancePhrase(FPhrases.Find(Key));
  if not Assigned(Result) then
  begin
    Result := TConcordancePhrase.Create(Key, PhraseText);
    FPhrases.Add(Key, Result);
  end;
end;

function TConcordance.GetOrCreateStrong(
  const Strong: string): TConcordanceStrong;
var
  NormalizedStrong: string;
begin
  NormalizedStrong := UpperCase(Strong);
  Result := TConcordanceStrong(FStrongs.Find(NormalizedStrong));
  if not Assigned(Result) then
  begin
    Result := TConcordanceStrong.Create(NormalizedStrong);
    FStrongs.Add(NormalizedStrong, Result);
  end;
end;

procedure TConcordance.AddOccurrence(const SourcePhrase, TargetPhrase,
  ReferenceID, DestinationContext: string; Occurrences: Integer;
  ReferenceOrder: Integer);
begin
  AddOccurrence(SourcePhrase, TargetPhrase, ReferenceID, DestinationContext,
    Occurrences, ReferenceOrder, nil);
end;

procedure TConcordance.AddOccurrence(const SourcePhrase, TargetPhrase,
  ReferenceID, DestinationContext: string; Occurrences,
  ReferenceOrder: Integer;
  const HighlightRanges: TConcordanceHighlightRangeArray);
var
  MatchIndex: Integer;
  Matches: IMatchCollection;
  Phrase: TConcordancePhrase;
  SeenStrongs: TStringList;
  Strong: string;
  StrongEntry: TConcordanceStrong;
begin
  if (Occurrences <= 0) or SourcePhrase.IsEmpty or TargetPhrase.IsEmpty then
    Exit;
  Matches := FStrongRegex.Matches(SourcePhrase);
  if Matches.Count = 0 then
    Exit;
  if ReferenceOrder < 0 then
  begin
    ReferenceOrder := FNextReferenceOrder;
    Inc(FNextReferenceOrder);
  end;

  Phrase := GetOrCreatePhrase(SourcePhrase);
  Phrase.AddTranslation(TargetPhrase, ReferenceID, DestinationContext,
    Occurrences, ReferenceOrder, HighlightRanges);

  SeenStrongs := TStringList.Create;
  try
    SeenStrongs.Sorted := True;
    SeenStrongs.CaseSensitive := False;
    SeenStrongs.Duplicates := dupIgnore;
    for MatchIndex := 0 to Matches.Count - 1 do
    begin
      Strong := UpperCase(Matches[MatchIndex].Groups[1].Value);
      if SeenStrongs.IndexOf(Strong) >= 0 then
        Continue;
      SeenStrongs.Add(Strong);
      StrongEntry := GetOrCreateStrong(Strong);
      StrongEntry.AddOccurrence(Phrase, TargetPhrase, ReferenceID,
        DestinationContext, Occurrences, ReferenceOrder,
        FMode = cemSynthetic, HighlightRanges);
    end;
  finally
    SeenStrongs.Free;
  end;
end;

procedure TConcordance.AddPhrase(Pairs: TStringList; const Ref: string);
var
  PairIndex: Integer;
begin
  if not Assigned(Pairs) or ((Pairs.Count mod 2) <> 0) then
    Exit;
  PairIndex := 0;
  while PairIndex < Pairs.Count do
  begin
    AddOccurrence(Pairs[PairIndex], Pairs[PairIndex + 1], Ref, '', 1);
    Inc(PairIndex, 2);
  end;
end;

function TConcordance.GetPhraseRtf(Phrase: TConcordancePhrase): string;
var
  Builder: TStringBuilder;
  DisplayText: string;
  MatchIndex: Integer;
  Matches: IMatchCollection;
  TagMatches: IMatchCollection;
  TagText: string;
  WordText: string;
  Words: TStringArray;
begin
  Builder := TStringBuilder.Create;
  try
    Words := Phrase.Phrase.Split(';');
    for WordText in Words do
    begin
      if Builder.Length > 0 then
        Builder.Append(' ');
      Matches := FSyntagmRegex.Matches(WordText);
      if Matches.Count = 0 then
      begin
        Builder.Append(UnicodeToRtf(WordText));
        Continue;
      end;
      DisplayText := Matches[0].Groups[1].Value;
      TagText := Matches[0].Groups[2].Value;
      TagMatches := FTagRegex.Matches(TagText);
      if TagMatches.Count = 0 then
        Builder.Append(UnicodeToRtf(DisplayText));
      for MatchIndex := 0 to TagMatches.Count - 1 do
      begin
        if TagMatches[MatchIndex].Groups[1].GetMatched then
          Builder.Append(MakeStrongLink(
            TagMatches[MatchIndex].Groups[1].Value, DisplayText))
        else if TagMatches[MatchIndex].Groups[2].GetMatched then
        begin
          Builder.Append('{\super ');
          Builder.Append(MakeMorphologyLink(
            TagMatches[MatchIndex].Groups[2].Value,
            UpperCase(TagMatches[MatchIndex].Groups[2].Value)));
          Builder.Append('}');
        end;
      end;
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TConcordance.GetTranslationRtf(
  Translation: TConcordanceTranslation): string;
var
  Builder: TStringBuilder;
  Context: string;
  HighlightRanges: TConcordanceHighlightRangeArray;
  Index: Integer;
  Reference: TConcordanceReference;
  References: TList;
begin
  Builder := TStringBuilder.Create;
  References := TList.Create;
  try
    for Index := 0 to Translation.ReferenceCount - 1 do
      References.Add(Translation.References[Index]);
    References.Sort(@CompareReferences);

    Builder.Append('\pard\li480\fi-180\sa20 ');
    Builder.Append('{\b ');
    Builder.Append(UnicodeToRtf(FormatTargetPhrase(Translation.Text)));
    Builder.Append('}  {\i ');
    Builder.Append(UnicodeToRtf(CountSummary(
      Translation.TotalOccurrences, Translation.ReferenceCount)));
    Builder.Append('}\par ');

    if FContextMode <> ccmReferencesOnly then
      for Index := 0 to References.Count - 1 do
      begin
        Reference := TConcordanceReference(References[Index]);
        Builder.Append('\pard\li720\sa30 ');
        Builder.Append(MakeReferenceLink(Reference.ReferenceID));
        if Reference.Occurrences > 1 then
          Builder.Append(UnicodeToRtf(Format(' ×%d',
            [Reference.Occurrences])));
        if not Reference.Context.IsEmpty then
        begin
          Builder.Append(UnicodeToRtf(' — '));
          if FContextMode = ccmRelevantExcerpt then
            BuildRelevantExcerpt(Reference.Context,
              Reference.HighlightRanges,
              FContextMaxCharacters, Context, HighlightRanges);
          if FContextMode = ccmFullVerse then
          begin
            Context := Reference.Context;
            HighlightRanges := Reference.HighlightRanges;
          end;
          Builder.Append(HighlightContextRanges(Context, HighlightRanges));
        end;
        Builder.Append('\par ');
      end
    else
    begin
      Builder.Append('\pard\li720\sa60 ');
      for Index := 0 to References.Count - 1 do
      begin
        Reference := TConcordanceReference(References[Index]);
        if Index > 0 then
          Builder.Append('; ');
        Builder.Append(MakeReferenceLink(Reference.ReferenceID));
        if Reference.Occurrences > 1 then
          Builder.Append(UnicodeToRtf(Format(' ×%d',
            [Reference.Occurrences])));
      end;
      Builder.Append('\par ');
    end;
    Result := Builder.ToString;
  finally
    References.Free;
    Builder.Free;
  end;
end;

function TConcordance.GetStrongRtf(const Lemma, Strong: string): string;
var
  Builder: TStringBuilder;
  Index: Integer;
  TranslationIndex: Integer;
  Phrase: TConcordancePhrase;
  Phrases: TList;
  StrongEntry: TConcordanceStrong;
  Translations: TList;
begin
  Result := '';
  StrongEntry := TConcordanceStrong(FStrongs.Find(UpperCase(Strong)));
  if not Assigned(StrongEntry) or (StrongEntry.TotalOccurrences <= 0) then
    Exit;

  Builder := TStringBuilder.Create;
  Phrases := TList.Create;
  Translations := TList.Create;
  try
    Builder.Append('\pard\li0\sa120 ');
    Builder.Append('{\fs28 ');
    Builder.Append(MakeStrongLink(Strong, Lemma));
    Builder.Append('}');
    Builder.Append('  {\i ');
    Builder.Append(UnicodeToRtf(CountSummary(
      StrongEntry.TotalOccurrences, StrongEntry.ReferenceCount)));
    Builder.Append('}\par ');

    if FMode = cemAnalytical then
    begin
      for Index := 0 to StrongEntry.PhraseCount - 1 do
        Phrases.Add(StrongEntry.Phrases[Index]);
      Phrases.Sort(@ComparePhrases);
      for Index := 0 to Phrases.Count - 1 do
      begin
        Phrase := TConcordancePhrase(Phrases[Index]);
        if Index > 0 then
          Builder.Append('\pard\li240\sb60\sa40 ')
        else
          Builder.Append('\pard\li240\sa40 ');
        Builder.Append('{\fs26 ');
        Builder.Append(GetPhraseRtf(Phrase));
        Builder.Append('}');
        Builder.Append('  {\i ');
        Builder.Append(UnicodeToRtf(OccurrenceText(
          Phrase.TotalOccurrences)));
        Builder.Append('}\par ');

        Translations.Clear;
        for TranslationIndex := 0 to Phrase.TranslationCount - 1 do
          Translations.Add(Phrase.Translations[TranslationIndex]);
        Translations.Sort(@CompareTranslations);
        for TranslationIndex := 0 to Translations.Count - 1 do
          Builder.Append(GetTranslationRtf(
            TConcordanceTranslation(Translations[TranslationIndex])));
      end;
    end
    else
    begin
      for TranslationIndex := 0 to StrongEntry.TranslationCount - 1 do
        Translations.Add(StrongEntry.Translations[TranslationIndex]);
      Translations.Sort(@CompareTranslations);
      for TranslationIndex := 0 to Translations.Count - 1 do
        Builder.Append(GetTranslationRtf(
          TConcordanceTranslation(Translations[TranslationIndex])));
    end;
    Result := Builder.ToString;
  finally
    Translations.Free;
    Phrases.Free;
    Builder.Free;
  end;
end;

end.
