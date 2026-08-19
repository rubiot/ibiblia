unit theword_ont_codec_v2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, theword_translator_note_body_v2;

type
  ETheWordOntCodec = class(Exception);
  ETheWordOntProtectedEdit = class(ETheWordOntCodec);
  ETheWordOntEncodingError = class(ETheWordOntCodec);

  TTheWordSemanticKind = (
    twsmItalic,
    twsmWordsOfJesus,
    twsmOldTestamentQuote,
    twsmUnderline
  );

  TTheWordLexicalKind = (
    twlkStrongGreek,
    twlkStrongHebrew,
    twlkMorphology,
    twlkWordTokenEnd
  );

  TTheWordOpaqueKind = (
    twokUnknownTag,
    twokMalformedTag
  );

  TTheWordStructuralKind = (
    twstSyntagmBoundary
  );

  TTheWordLayerCapability = (
    twlcFullWysiwyg,
    twlcProtectedRegions,
    twlcRawOnly
  );

  TTheWordOntEncoding = (
    twoeUtf8,
    twoeSystemAnsi,
    twoeDeclaredCodePage,
    twoeUnsupportedDeclared
  );

  TTheWordLineEnding = (
    twleNone,
    twleCRLF,
    twleLF,
    twleCR,
    twleMixed
  );

  TTheWordOntRecordKind = (
    tworkBlank,
    tworkProperty,
    tworkContinuation,
    tworkDisabled,
    tworkUnknown
  );

  TTheWordTitleRange = record
    StartOffset: Integer;
    EndOffset: Integer;
    Level: Integer;
    OpenOrdinal: Integer;
    CloseOrdinal: Integer;
  end;

  TTheWordTitleRangeArray = array of TTheWordTitleRange;

  TTheWordTranslatorNote = class
  private
    FId: QWord;
    FOffsetUTF16: Integer;
    FOrdinal: Integer;
    FQ: UnicodeString;
    FBodyRaw: UnicodeString;
    FOpenLexeme: UnicodeString;
    FCloseLexeme: UnicodeString;
    FCapability: TTheWordLayerCapability;
    FImported: Boolean;
    FModified: Boolean;
  public
    property Id: QWord read FId;
    property OffsetUTF16: Integer read FOffsetUTF16;
    property Ordinal: Integer read FOrdinal;
    property Q: UnicodeString read FQ;
    property BodyRaw: UnicodeString read FBodyRaw;
    property OpenLexeme: UnicodeString read FOpenLexeme;
    property CloseLexeme: UnicodeString read FCloseLexeme;
    property Capability: TTheWordLayerCapability read FCapability;
    property Imported: Boolean read FImported;
    property Modified: Boolean read FModified;
  end;

  { Offsets are zero-based UTF-16 code-unit offsets between characters. }
  TTheWordSemanticMark = class
  private
    FKind: TTheWordSemanticKind;
    FStartOffset: Integer;
    FEndOffset: Integer;
    FOpenLexeme: UnicodeString;
    FCloseLexeme: UnicodeString;
    FImported: Boolean;
    FOpenOrdinal: Integer;
    FCloseOrdinal: Integer;
  public
    property Kind: TTheWordSemanticKind read FKind;
    property StartOffset: Integer read FStartOffset;
    property EndOffset: Integer read FEndOffset;
    property OpenLexeme: UnicodeString read FOpenLexeme;
    property CloseLexeme: UnicodeString read FCloseLexeme;
    property Imported: Boolean read FImported;
    property OpenOrdinal: Integer read FOpenOrdinal;
    property CloseOrdinal: Integer read FCloseOrdinal;
  end;

  TTheWordLexicalItem = class
  private
    FKind: TTheWordLexicalKind;
    FOffset: Integer;
    FRawLexeme: UnicodeString;
    FValue: UnicodeString;
    FOrdinal: Integer;
  public
    property Kind: TTheWordLexicalKind read FKind;
    property Offset: Integer read FOffset;
    property RawLexeme: UnicodeString read FRawLexeme;
    property Value: UnicodeString read FValue;
    property Ordinal: Integer read FOrdinal;
  end;

  TTheWordOpaqueItem = class
  private
    FKind: TTheWordOpaqueKind;
    FOffset: Integer;
    FRawLexeme: UnicodeString;
    FOrdinal: Integer;
  public
    property Kind: TTheWordOpaqueKind read FKind;
    property Offset: Integer read FOffset;
    property RawLexeme: UnicodeString read FRawLexeme;
    property Ordinal: Integer read FOrdinal;
  end;

  TTheWordStructuralItem = class
  private
    FKind: TTheWordStructuralKind;
    FOffset: Integer;
    FRawLexeme: UnicodeString;
    FOrdinal: Integer;
  public
    property Kind: TTheWordStructuralKind read FKind;
    property Offset: Integer read FOffset;
    property RawLexeme: UnicodeString read FRawLexeme;
    property Ordinal: Integer read FOrdinal;
  end;

  TTheWordVerseDocument = class
  private
    FVisibleText: UnicodeString;
    FOriginalRaw: UnicodeString;
    FMarks: TObjectList;
    FLexicalItems: TObjectList;
    FOpaqueItems: TObjectList;
    FStructuralItems: TObjectList;
    FTranslatorNotes: TObjectList;
    FEvents: TObjectList;
    FNextOrdinal: Integer;
    FNextTranslatorNoteId: QWord;
    FModified: Boolean;
    FSourceBytes: RawByteString;
    FLineEndingBytes: RawByteString;
    FCanEdit: Boolean;
    FRequiredCodePage: Integer;
    FEditBlockReason: UnicodeString;
    procedure EnsureEditable;
    function GetMark(AIndex: Integer): TTheWordSemanticMark;
    function GetMarkCount: Integer;
    function GetLexicalItem(AIndex: Integer): TTheWordLexicalItem;
    function GetLexicalItemCount: Integer;
    function GetOpaqueItem(AIndex: Integer): TTheWordOpaqueItem;
    function GetOpaqueItemCount: Integer;
    function GetStructuralItem(AIndex: Integer): TTheWordStructuralItem;
    function GetStructuralItemCount: Integer;
    function GetTranslatorNote(AIndex: Integer): TTheWordTranslatorNote;
    function GetTranslatorNoteCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function CanApplySemanticRange(AStartOffset,
      AEndOffset: Integer): Boolean;
    function CanInsertParagraphMark(AOffsetUTF16: Integer): Boolean;
    function CanInsertTranslatorNote(AOffsetUTF16: Integer): Boolean;
    function CanReplaceVisibleText(AStartUTF16,
      ALengthUTF16: Integer): Boolean;
    function InsertParagraphMark(AOffsetUTF16: Integer): Boolean;
    function RemoveParagraphMark(AOffsetUTF16: Integer;
      AFromEnd: Boolean): Boolean;
    function InsertTranslatorNote(AOffsetUTF16: Integer;
      const AQ, ABodyRaw: UnicodeString): TTheWordTranslatorNote;
    function UpdateTranslatorNote(ANoteId: QWord;
      const AQ, ABodyRaw: UnicodeString): Boolean;
    function RemoveTranslatorNote(ANoteId: QWord): Boolean;
    function TryGetTranslatorNote(ANoteId: QWord;
      out ANote: TTheWordTranslatorNote): Boolean;
    function AddSemanticMark(AKind: TTheWordSemanticKind;
      AStartOffset, AEndOffset: Integer): TTheWordSemanticMark;
    procedure RemoveSemanticMark(AMark: TTheWordSemanticMark);
    function ToggleSemanticMark(AKind: TTheWordSemanticKind;
      AStartOffset, AEndOffset: Integer): Boolean;
    procedure ReplaceVisibleText(AStartUTF16, ALengthUTF16: Integer;
      const AReplacement: UnicodeString);
    function LexicalProtectedStart(AIndex: Integer): Integer;
    property VisibleText: UnicodeString read FVisibleText;
    property OriginalRaw: UnicodeString read FOriginalRaw;
    property MarkCount: Integer read GetMarkCount;
    property Marks[AIndex: Integer]: TTheWordSemanticMark read GetMark;
    property LexicalItemCount: Integer read GetLexicalItemCount;
    property LexicalItems[AIndex: Integer]: TTheWordLexicalItem
      read GetLexicalItem;
    property OpaqueItemCount: Integer read GetOpaqueItemCount;
    property OpaqueItems[AIndex: Integer]: TTheWordOpaqueItem
      read GetOpaqueItem;
    property StructuralItemCount: Integer read GetStructuralItemCount;
    property StructuralItems[AIndex: Integer]: TTheWordStructuralItem
      read GetStructuralItem;
    property TranslatorNoteCount: Integer read GetTranslatorNoteCount;
    property TranslatorNotes[AIndex: Integer]: TTheWordTranslatorNote
      read GetTranslatorNote;
    property Modified: Boolean read FModified;
    property CanEdit: Boolean read FCanEdit;
    property EditBlockReason: UnicodeString read FEditBlockReason;
  end;

  TTheWordOntRecord = class
  private
    FKind: TTheWordOntRecordKind;
    FRawText: UnicodeString;
    FKey: UnicodeString;
    FValue: UnicodeString;
    FActive: Boolean;
    FSourceBytes: RawByteString;
    FLineEndingBytes: RawByteString;
    function GetIsActiveRule: Boolean;
  public
    property Kind: TTheWordOntRecordKind read FKind;
    property RawText: UnicodeString read FRawText;
    property Key: UnicodeString read FKey;
    property Value: UnicodeString read FValue;
    property Active: Boolean read FActive;
    property IsActiveRule: Boolean read GetIsActiveRule;
  end;

  TTheWordOntModule = class
  private
    FVerses: TObjectList;
    FRecords: TObjectList;
    FOriginalBytes: RawByteString;
    FEncoding: TTheWordOntEncoding;
    FHasBom: Boolean;
    FNewLineStyle: TTheWordLineEnding;
    FPreferredNewLine: UnicodeString;
    FDeclaredCharset: UnicodeString;
    FCodePage: Integer;
    FCanReencode: Boolean;
    FEncodingIssue: UnicodeString;
    function GetVerse(AIndex: Integer): TTheWordVerseDocument;
    function GetVerseCount: Integer;
    function GetRecord(AIndex: Integer): TTheWordOntRecord;
    function GetRecordCount: Integer;
    function GetModified: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property VerseCount: Integer read GetVerseCount;
    property Verses[AIndex: Integer]: TTheWordVerseDocument read GetVerse;
    property RecordCount: Integer read GetRecordCount;
    property Records[AIndex: Integer]: TTheWordOntRecord read GetRecord;
    property Encoding: TTheWordOntEncoding read FEncoding;
    property HasBom: Boolean read FHasBom;
    property NewLineStyle: TTheWordLineEnding read FNewLineStyle;
    property PreferredNewLine: UnicodeString read FPreferredNewLine;
    property DeclaredCharset: UnicodeString read FDeclaredCharset;
    property CodePage: Integer read FCodePage;
    property CanReencode: Boolean read FCanReencode;
    property EncodingIssue: UnicodeString read FEncodingIssue;
    property Modified: Boolean read GetModified;
  end;

  ITheWordOntCodecV2 = interface
    ['{DA774B57-260D-460C-A3D5-FD4D4BC4F7DD}']
    function DecodeVerse(const ARaw: UnicodeString): TTheWordVerseDocument;
    function EncodeVerse(ADocument: TTheWordVerseDocument): UnicodeString;
    function DecodeModule(const ABytes: RawByteString;
      AVerseCount: Integer): TTheWordOntModule;
    function EncodeModule(AModule: TTheWordOntModule): RawByteString;
  end;

  TTheWordOntCodecV2 = class(TInterfacedObject, ITheWordOntCodecV2)
  private
    procedure AddOpaque(ADocument: TTheWordVerseDocument; AOffset: Integer;
      const ARaw: UnicodeString; AKind: TTheWordOpaqueKind);
    procedure AddLexical(ADocument: TTheWordVerseDocument; AOffset: Integer;
      AKind: TTheWordLexicalKind; const ARaw, AValue: UnicodeString);
    procedure AddStructural(ADocument: TTheWordVerseDocument;
      AOffset: Integer; const ARaw: UnicodeString);
  public
    function DecodeVerse(const ARaw: UnicodeString): TTheWordVerseDocument;
    function EncodeVerse(ADocument: TTheWordVerseDocument): UnicodeString;
    function DecodeModule(const ABytes: RawByteString;
      AVerseCount: Integer): TTheWordOntModule;
    function EncodeModule(AModule: TTheWordOntModule): RawByteString;
  end;

  TTheWordCapabilityAnalyzer = class
  public
    class function AnalyzeVerse(ADocument: TTheWordVerseDocument):
      TTheWordLayerCapability; static;
    class function AnalyzeModule(AModule: TTheWordOntModule):
      TTheWordLayerCapability; static;
  end;

function TheWordCanonicalOpen(AKind: TTheWordSemanticKind): UnicodeString;
function TheWordCanonicalClose(AKind: TTheWordSemanticKind): UnicodeString;
function TheWordClassifyOntRecord(const ARaw: UnicodeString):
  TTheWordOntRecordKind;
function CollectTheWordTitleRanges(ADocument: TTheWordVerseDocument;
  out ARanges: TTheWordTitleRangeArray): Integer;
function AnalyzeTheWordTranslatorNoteBody(const ARaw: UnicodeString):
  TTheWordLayerCapability;
function TryCanonicalizeTheWordTranslatorNoteBody(
  const ARaw: UnicodeString; out ACanonical: UnicodeString;
  out ACapability: TTheWordLayerCapability): Boolean;

implementation

type
  TTheWordInlineEventKind = (
    twiekPendingSemantic,
    twiekSemanticOpen,
    twiekSemanticClose,
    twiekLexical,
    twiekOpaque,
    twiekStructural,
    twiekTranslatorNote
  );

  TTheWordInlineEvent = class
  public
    Kind: TTheWordInlineEventKind;
    Offset: Integer;
    Ordinal: Integer;
    RawLexeme: UnicodeString;
    SemanticMark: TTheWordSemanticMark;
    TranslatorNote: TTheWordTranslatorNote;
  end;

  TPendingSemantic = class
  public
    Kind: TTheWordSemanticKind;
    StartOffset: Integer;
    OpenLexeme: UnicodeString;
    OpenEvent: TTheWordInlineEvent;
  end;

  TPendingTitle = record
    StartOffset: Integer;
    Level: Integer;
    OpenOrdinal: Integer;
  end;

  TPendingTitleArray = array of TPendingTitle;

  TPhysicalLine = class
  public
    Content: RawByteString;
    Ending: RawByteString;
  end;

const
  UTF8_BOM: RawByteString = #$EF#$BB#$BF;

function TheWordCanonicalOpen(AKind: TTheWordSemanticKind): UnicodeString;
begin
  Result := '';
  case AKind of
    twsmItalic: Result := '<FI>';
    twsmWordsOfJesus: Result := '<FR>';
    twsmOldTestamentQuote: Result := '<FO>';
    twsmUnderline: Result := '<FU>';
  end;
end;

function TheWordCanonicalClose(AKind: TTheWordSemanticKind): UnicodeString;
begin
  Result := '';
  case AKind of
    twsmItalic: Result := '<Fi>';
    twsmWordsOfJesus: Result := '<Fr>';
    twsmOldTestamentQuote: Result := '<Fo>';
    twsmUnderline: Result := '<Fu>';
  end;
end;

function TheWordTitleOpenLevel(const ARaw: UnicodeString;
  out ALevel: Integer): Boolean;
begin
  Result := True;
  if (ARaw = '<TS>') or (ARaw = '<TS0>') then
    ALevel := 0
  else if ARaw = '<TS1>' then
    ALevel := 1
  else if ARaw = '<TS2>' then
    ALevel := 2
  else if ARaw = '<TS3>' then
    ALevel := 3
  else
  begin
    ALevel := 0;
    Result := False;
  end;
end;

function CollectTheWordTitleRanges(ADocument: TTheWordVerseDocument;
  out ARanges: TTheWordTitleRangeArray): Integer;
var
  EventIndex, Level, StackCount, RangeIndex, SortIndex: Integer;
  Event: TTheWordInlineEvent;
  Stack: TPendingTitleArray;
  Pending: TPendingTitle;
  SwapRange: TTheWordTitleRange;
begin
  ARanges := nil;
  if ADocument = nil then
    Exit(0);

  Stack := nil;
  StackCount := 0;
  for EventIndex := 0 to ADocument.FEvents.Count - 1 do
  begin
    Event := TTheWordInlineEvent(ADocument.FEvents[EventIndex]);
    if (Event.Kind <> twiekOpaque) then
      Continue;

    if TheWordTitleOpenLevel(Event.RawLexeme, Level) then
    begin
      SetLength(Stack, StackCount + 1);
      Stack[StackCount].StartOffset := Event.Offset;
      Stack[StackCount].Level := Level;
      Stack[StackCount].OpenOrdinal := Event.Ordinal;
      Inc(StackCount);
    end
    else if Event.RawLexeme = '<Ts>' then
    begin
      if StackCount = 0 then
        Continue;
      Dec(StackCount);
      Pending := Stack[StackCount];
      SetLength(Stack, StackCount);
      if Event.Offset < Pending.StartOffset then
        Continue;

      RangeIndex := Length(ARanges);
      SetLength(ARanges, RangeIndex + 1);
      ARanges[RangeIndex].StartOffset := Pending.StartOffset;
      ARanges[RangeIndex].EndOffset := Event.Offset;
      ARanges[RangeIndex].Level := Pending.Level;
      ARanges[RangeIndex].OpenOrdinal := Pending.OpenOrdinal;
      ARanges[RangeIndex].CloseOrdinal := Event.Ordinal;
    end;
  end;

  { Nested titles close in reverse order. Expose every result in source/open
    order so consumers never have to depend on that implementation detail. }
  for RangeIndex := 1 to High(ARanges) do
  begin
    SortIndex := RangeIndex;
    while (SortIndex > 0) and
          (ARanges[SortIndex - 1].OpenOrdinal >
           ARanges[SortIndex].OpenOrdinal) do
    begin
      SwapRange := ARanges[SortIndex - 1];
      ARanges[SortIndex - 1] := ARanges[SortIndex];
      ARanges[SortIndex] := SwapRange;
      Dec(SortIndex);
    end;
  end;
  Result := Length(ARanges);
end;

function IsTheWordTitleAnchor(AOrdinal: Integer;
  const ARanges: TTheWordTitleRangeArray): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to High(ARanges) do
    if (ARanges[Index].OpenOrdinal = AOrdinal) or
       (ARanges[Index].CloseOrdinal = AOrdinal) then
      Exit(True);
  Result := False;
end;

function KeepTheWordTitleAnchorAtBodyInsertion(AOrdinal, AOffset: Integer;
  const ARanges: TTheWordTitleRangeArray): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to High(ARanges) do
    if ARanges[Index].EndOffset = AOffset then
    begin
      if ARanges[Index].CloseOrdinal = AOrdinal then
        Exit(True);
      if (ARanges[Index].StartOffset = ARanges[Index].EndOffset) and
         (ARanges[Index].OpenOrdinal = AOrdinal) then
        Exit(True);
    end;
  Result := False;
end;

function NewInlineEvent(ADocument: TTheWordVerseDocument;
  AKind: TTheWordInlineEventKind; AOffset: Integer;
  const ARaw: UnicodeString): TTheWordInlineEvent;
begin
  Result := TTheWordInlineEvent.Create;
  Result.Kind := AKind;
  Result.Offset := AOffset;
  Result.Ordinal := ADocument.FNextOrdinal;
  Inc(ADocument.FNextOrdinal);
  Result.RawLexeme := ARaw;
  Result.SemanticMark := nil;
  Result.TranslatorNote := nil;
  ADocument.FEvents.Add(Result);
end;

function FoldAscii(AChar: WideChar): WideChar;
begin
  if (AChar >= 'A') and (AChar <= 'Z') then
    Result := WideChar(Ord(AChar) + Ord('a') - Ord('A'))
  else
    Result := AChar;
end;

function AsciiSameText(const ALeft, ARight: UnicodeString): Boolean;
var
  Index: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for Index := 1 to Length(ALeft) do
    if FoldAscii(ALeft[Index]) <> FoldAscii(ARight[Index]) then
      Exit(False);
  Result := True;
end;

function TrimUnicode(const AValue: UnicodeString): UnicodeString;
var
  FirstIndex, LastIndex: Integer;
begin
  FirstIndex := 1;
  LastIndex := Length(AValue);
  while (FirstIndex <= LastIndex) and
    (AValue[FirstIndex] in [' ', #9, #10, #13]) do
    Inc(FirstIndex);
  while (LastIndex >= FirstIndex) and
    (AValue[LastIndex] in [' ', #9, #10, #13]) do
    Dec(LastIndex);
  Result := Copy(AValue, FirstIndex, LastIndex - FirstIndex + 1);
end;

function IsValidStrongPayload(const AValue: UnicodeString): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if (AValue = '') or not (AValue[1] in ['0'..'9']) then
    Exit;
  Index := 1;
  while (Index <= Length(AValue)) and (AValue[Index] in ['0'..'9']) do
    Inc(Index);
  if Index > Length(AValue) then
    Exit(True);
  if (Index = Length(AValue)) and
     (((AValue[Index] >= 'a') and (AValue[Index] <= 'z')) or
      ((AValue[Index] >= 'A') and (AValue[Index] <= 'Z'))) then
    Exit(True);
end;

function StartsWithCaseSensitive(const AValue, APrefix: UnicodeString): Boolean;
begin
  Result := (Length(AValue) >= Length(APrefix)) and
    (Copy(AValue, 1, Length(APrefix)) = APrefix);
end;

function SemanticOpenKind(const ARaw: UnicodeString;
  out AKind: TTheWordSemanticKind): Boolean;
begin
  Result := True;
  if ARaw = '<FI>' then
    AKind := twsmItalic
  else if ARaw = '<FR>' then
    AKind := twsmWordsOfJesus
  else if ARaw = '<FO>' then
    AKind := twsmOldTestamentQuote
  else if ARaw = '<FU>' then
    AKind := twsmUnderline
  else
    Result := False;
end;

function SemanticCloseKind(const ARaw: UnicodeString;
  out AKind: TTheWordSemanticKind): Boolean;
begin
  Result := True;
  if ARaw = '<Fi>' then
    AKind := twsmItalic
  else if ARaw = '<Fr>' then
    AKind := twsmWordsOfJesus
  else if ARaw = '<Fo>' then
    AKind := twsmOldTestamentQuote
  else if ARaw = '<Fu>' then
    AKind := twsmUnderline
  else
    Result := False;
end;

function FindTagEnd(const AText: UnicodeString; AStart: Integer): Integer;
var
  Index: Integer;
  Quote: WideChar;
begin
  Result := 0;
  Quote := #0;
  for Index := AStart + 1 to Length(AText) do
  begin
    if Quote <> #0 then
    begin
      if AText[Index] = Quote then
        Quote := #0;
      Continue;
    end;
    if (AText[Index] = '"') or (AText[Index] = '''') then
      Quote := AText[Index]
    else if AText[Index] = '>' then
      Exit(Index);
  end;
end;

function IsWellFormedUtf16(const AText: UnicodeString): Boolean; forward;

function NoteBodyCapabilityToLayer(
  ACapability: TTheWordNoteBodyCapability): TTheWordLayerCapability;
begin
  case ACapability of
    twnbcFullWysiwyg: Result := twlcFullWysiwyg;
    twnbcProtectedRegions: Result := twlcProtectedRegions;
  else
    Result := twlcRawOnly;
  end;
end;

function AnalyzeTheWordTranslatorNoteBody(const ARaw: UnicodeString):
  TTheWordLayerCapability;
begin
  Result := NoteBodyCapabilityToLayer(
    TTheWordTranslatorNoteBodyCodec.Analyze(ARaw));
end;

function TryCanonicalizeTheWordTranslatorNoteBody(
  const ARaw: UnicodeString; out ACanonical: UnicodeString;
  out ACapability: TTheWordLayerCapability): Boolean;
var
  BodyCapability: TTheWordNoteBodyCapability;
begin
  Result := TTheWordTranslatorNoteBodyCodec.TryCanonicalize(ARaw,
    ACanonical, BodyCapability);
  ACapability := NoteBodyCapabilityToLayer(BodyCapability);
end;

function TranslatorNoteOpenNameEnd(const ARaw: UnicodeString): Integer;
var
  Index: Integer;
begin
  Result := 0;
  if (Length(ARaw) < 4) or (ARaw[1] <> '<') then
    Exit;

  Index := 2;
  while (Index <= Length(ARaw)) and
    (ARaw[Index] in [' ', #9, #10, #13]) do
    Inc(Index);
  if (Index + 1 > Length(ARaw)) or
     (Copy(ARaw, Index, 2) <> 'RF') then
    Exit;
  Inc(Index, 2);
  if (Index > Length(ARaw)) or
     not ((ARaw[Index] = '>') or
       (ARaw[Index] in [' ', #9, #10, #13])) then
    Exit;
  Result := Index;
end;

function IsTranslatorNoteOpenTag(const ARaw: UnicodeString): Boolean;
begin
  Result := TranslatorNoteOpenNameEnd(ARaw) <> 0;
end;

function IsTranslatorNoteCloseTag(const ARaw: UnicodeString): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if (Length(ARaw) < 4) or (ARaw[1] <> '<') then
    Exit;

  Index := 2;
  while (Index <= Length(ARaw)) and
    (ARaw[Index] in [' ', #9, #10, #13]) do
    Inc(Index);
  if (Index + 1 > Length(ARaw)) or
     (Copy(ARaw, Index, 2) <> 'Rf') then
    Exit;
  Inc(Index, 2);
  while (Index <= Length(ARaw)) and
    (ARaw[Index] in [' ', #9, #10, #13]) do
    Inc(Index);
  if Index > Length(ARaw) then
    Exit;
  Result := (Index = Length(ARaw)) and (ARaw[Index] = '>');
end;

function ContainsTranslatorNoteOpenTag(const AText: UnicodeString): Boolean;
var
  Index, NameIndex: Integer;
begin
  Index := 1;
  while Index <= Length(AText) do
  begin
    if AText[Index] <> '<' then
    begin
      Inc(Index);
      Continue;
    end;
    NameIndex := Index + 1;
    while (NameIndex <= Length(AText)) and
      (AText[NameIndex] in [' ', #9, #10, #13]) do
      Inc(NameIndex);
    if (Copy(AText, NameIndex, 2) = 'RF') and
       (NameIndex + 2 <= Length(AText)) and
       ((AText[NameIndex + 2] = '>') or
         (AText[NameIndex + 2] in [' ', #9, #10, #13])) then
      Exit(True);
    Inc(Index);
  end;
  Result := False;
end;

function ContainsTranslatorNoteCloseTag(const AText: UnicodeString): Boolean;
var
  Index, TagEnd: Integer;
  RawTag: UnicodeString;
begin
  Index := 1;
  while Index <= Length(AText) do
  begin
    if AText[Index] <> '<' then
    begin
      Inc(Index);
      Continue;
    end;
    TagEnd := FindTagEnd(AText, Index);
    if TagEnd = 0 then
      Exit(False);
    RawTag := Copy(AText, Index, TagEnd - Index + 1);
    if IsTranslatorNoteCloseTag(RawTag) then
      Exit(True);
    Index := TagEnd + 1;
  end;
  Result := False;
end;

function TryParseTranslatorNoteOpen(const ARaw: UnicodeString;
  out AQ: UnicodeString; out AOnlySupportedAttributes: Boolean): Boolean;
var
  AttrName, Inner: UnicodeString;
  Index, NameEnd, NameStart, ValueStart: Integer;
  Quote: WideChar;
  SeenQ: Boolean;
begin
  NameEnd := TranslatorNoteOpenNameEnd(ARaw);
  Result := (NameEnd <> 0) and
    (ARaw[Length(ARaw)] = '>');
  AQ := '';
  AOnlySupportedAttributes := Result;
  if not Result then
    Exit;

  Inner := Copy(ARaw, NameEnd, Length(ARaw) - NameEnd);
  Index := 1;
  SeenQ := False;
  while Index <= Length(Inner) do
  begin
    while (Index <= Length(Inner)) and
      (Inner[Index] in [' ', #9, #10, #13]) do
      Inc(Index);
    if Index > Length(Inner) then
      Break;
    NameStart := Index;
    while (Index <= Length(Inner)) and not
      (Inner[Index] in [' ', #9, #10, #13, '=']) do
      Inc(Index);
    AttrName := Copy(Inner, NameStart, Index - NameStart);
    while (Index <= Length(Inner)) and
      (Inner[Index] in [' ', #9, #10, #13]) do
      Inc(Index);
    if (Index > Length(Inner)) or (Inner[Index] <> '=') then
    begin
      AOnlySupportedAttributes := False;
      while (Index <= Length(Inner)) and not
        (Inner[Index] in [' ', #9, #10, #13]) do
        Inc(Index);
      Continue;
    end;
    Inc(Index);
    while (Index <= Length(Inner)) and
      (Inner[Index] in [' ', #9, #10, #13]) do
      Inc(Index);
    if Index > Length(Inner) then
    begin
      AOnlySupportedAttributes := False;
      Break;
    end;

    Quote := #0;
    if Inner[Index] in ['"', ''''] then
    begin
      Quote := Inner[Index];
      Inc(Index);
    end;
    ValueStart := Index;
    if Quote <> #0 then
    begin
      while (Index <= Length(Inner)) and (Inner[Index] <> Quote) do
        Inc(Index);
      if Index > Length(Inner) then
      begin
        AOnlySupportedAttributes := False;
        Break;
      end;
    end
    else
      while (Index <= Length(Inner)) and not
        (Inner[Index] in [' ', #9, #10, #13]) do
        Inc(Index);

    if (AttrName <> 'q') or SeenQ then
      AOnlySupportedAttributes := False
    else
    begin
      SeenQ := True;
      AQ := TrimUnicode(Copy(Inner, ValueStart, Index - ValueStart));
    end;
    if Quote <> #0 then
      Inc(Index);
  end;
end;

function TryCanonicalTranslatorNoteOpen(const AQ: UnicodeString;
  out AOpen: UnicodeString): Boolean;
var
  Index: Integer;
  CanUseUnquoted, HasDoubleQuote, HasSingleQuote: Boolean;
begin
  Result := IsWellFormedUtf16(AQ);
  AOpen := '';
  if not Result then
    Exit;
  if AQ = '' then
  begin
    AOpen := '<RF>';
    Exit(True);
  end;

  CanUseUnquoted := True;
  HasDoubleQuote := False;
  HasSingleQuote := False;
  for Index := 1 to Length(AQ) do
  begin
    if AQ[Index] in [' ', #9, #10, #13, '<', '>', '"', ''''] then
      CanUseUnquoted := False;
    if AQ[Index] = '"' then
      HasDoubleQuote := True;
    if AQ[Index] = '''' then
      HasSingleQuote := True;
    if AQ[Index] in [#10, #13, '<', '>'] then
      Exit(False);
  end;
  if CanUseUnquoted then
    AOpen := '<RF q=' + AQ + '>'
  else if not HasDoubleQuote then
    AOpen := '<RF q="' + AQ + '">'
  else if not HasSingleQuote then
    AOpen := '<RF q=''' + AQ + '''>'
  else
    Exit(False);
  Result := True;
end;

function FindTranslatorNoteClose(const ARaw: UnicodeString;
  ABodyStart: Integer; out ACloseStart, ACloseEnd: Integer;
  out ANested: Boolean): Boolean;
var
  Index, TagEnd: Integer;
  RawTag: UnicodeString;
begin
  ACloseStart := 0;
  ACloseEnd := 0;
  ANested := False;
  Index := ABodyStart;
  while Index <= Length(ARaw) do
  begin
    if ARaw[Index] <> '<' then
    begin
      Inc(Index);
      Continue;
    end;
    TagEnd := FindTagEnd(ARaw, Index);
    if TagEnd = 0 then
      Break;
    RawTag := Copy(ARaw, Index, TagEnd - Index + 1);
    if IsTranslatorNoteOpenTag(RawTag) then
      ANested := True
    else if IsTranslatorNoteCloseTag(RawTag) then
    begin
      { theWord treats RF containers as non-greedy: even malformed nested
        openings are delimited by the first compatible closing tag. }
      ACloseStart := Index;
      ACloseEnd := TagEnd;
      Exit(True);
    end;
    Index := TagEnd + 1;
  end;
  Result := False;
end;

function IsValidUtf8(const ABytes: RawByteString): Boolean;
var
  Index, Needed, ContinuationIndex: Integer;
  Lead, Current: Byte;
begin
  Index := 1;
  while Index <= Length(ABytes) do
  begin
    Lead := Byte(ABytes[Index]);
    if Lead < $80 then
    begin
      Inc(Index);
      Continue;
    end;

    if (Lead >= $C2) and (Lead <= $DF) then
      Needed := 1
    else if (Lead >= $E0) and (Lead <= $EF) then
      Needed := 2
    else if (Lead >= $F0) and (Lead <= $F4) then
      Needed := 3
    else
      Exit(False);

    if Index + Needed > Length(ABytes) then
      Exit(False);
    for ContinuationIndex := 1 to Needed do
    begin
      Current := Byte(ABytes[Index + ContinuationIndex]);
      if (Current and $C0) <> $80 then
        Exit(False);
    end;

    if (Needed = 2) and (Lead = $E0) and
       (Byte(ABytes[Index + 1]) < $A0) then
      Exit(False);
    if (Needed = 2) and (Lead = $ED) and
       (Byte(ABytes[Index + 1]) >= $A0) then
      Exit(False);
    if (Needed = 3) and (Lead = $F0) and
       (Byte(ABytes[Index + 1]) < $90) then
      Exit(False);
    if (Needed = 3) and (Lead = $F4) and
       (Byte(ABytes[Index + 1]) >= $90) then
      Exit(False);

    Inc(Index, Needed + 1);
  end;
  Result := True;
end;

function DecodeBytes(const ABytes: RawByteString;
  ACodePage: Integer): UnicodeString;
var
  Text: RawByteString;
begin
  if ACodePage = 65001 then
    Result := UTF8Decode(ABytes)
  else
  begin
    Text := ABytes;
    SetCodePage(Text, ACodePage, False);
    Result := UnicodeString(Text);
  end;
end;

function TryEncodeBytes(const AText: UnicodeString; ACodePage: Integer;
  out ABytes: RawByteString): Boolean;
var
  Text: RawByteString;
begin
  ABytes := '';
  if ACodePage = 65001 then
  begin
    ABytes := RawByteString(UTF8Encode(AText));
    SetCodePage(ABytes, 0, False);
  end
  else
  begin
    Text := RawByteString(UTF8Encode(AText));
    SetCodePage(Text, 65001, False);
    SetCodePage(Text, ACodePage, True);
    ABytes := Text;
    SetCodePage(ABytes, 0, False);
  end;
  Result := DecodeBytes(ABytes, ACodePage) = AText;
end;

function RawBytesEqual(const ALeft, ARight: RawByteString): Boolean;
var
  Index: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);
  for Index := 1 to Length(ALeft) do
    if Byte(ALeft[Index]) <> Byte(ARight[Index]) then
      Exit(False);
  Result := True;
end;

function BytesRoundTrip(const ABytes: RawByteString;
  ACodePage: Integer): Boolean;
var
  Encoded: RawByteString;
begin
  Result := TryEncodeBytes(DecodeBytes(ABytes, ACodePage), ACodePage,
    Encoded) and RawBytesEqual(Encoded, ABytes);
end;

function TrimAsciiBytes(const AValue: RawByteString): RawByteString;
var
  FirstIndex, LastIndex: Integer;
begin
  FirstIndex := 1;
  LastIndex := Length(AValue);
  while (FirstIndex <= LastIndex) and
    (AValue[FirstIndex] in [' ', #9, #10, #13]) do
    Inc(FirstIndex);
  while (LastIndex >= FirstIndex) and
    (AValue[LastIndex] in [' ', #9, #10, #13]) do
    Dec(LastIndex);
  Result := Copy(AValue, FirstIndex, LastIndex - FirstIndex + 1);
end;

function AsciiBytesToUnicode(const AValue: RawByteString): UnicodeString;
var
  Index: Integer;
begin
  Result := '';
  SetLength(Result, Length(AValue));
  for Index := 1 to Length(AValue) do
    Result[Index] := WideChar(Byte(AValue[Index]));
end;

function IsAsciiBytes(const AValue: RawByteString): Boolean;
var
  Index: Integer;
begin
  for Index := 1 to Length(AValue) do
    if Byte(AValue[Index]) >= $80 then
      Exit(False);
  Result := True;
end;

function TryReadCharsetProperty(const ALine: RawByteString;
  out AValue: RawByteString): Boolean;
var
  Work, Key: RawByteString;
  Separator: Integer;
begin
  Result := False;
  AValue := '';
  Work := TrimAsciiBytes(ALine);
  if Work = '' then
    Exit;
  if (Work[1] = '#') or (Work[1] = '.') or (Work[1] = ';') then
    Exit;
  Separator := Pos('=', Work);
  if Separator = 0 then
    Exit;
  Key := TrimAsciiBytes(Copy(Work, 1, Separator - 1));
  if not IsAsciiBytes(Key) or
     not AsciiSameText(AsciiBytesToUnicode(Key), 'charset') then
    Exit;
  AValue := TrimAsciiBytes(Copy(Work, Separator + 1, MaxInt));
  Result := True;
end;

procedure FindDeclaredCharset(ALines: TObjectList; AFirstRecord: Integer;
  out ADeclared: UnicodeString; out AFound, AConflict: Boolean);
var
  Index: Integer;
  RawValue: RawByteString;
  Value: UnicodeString;
begin
  ADeclared := '';
  AFound := False;
  AConflict := False;
  for Index := AFirstRecord to ALines.Count - 1 do
  begin
    if not TryReadCharsetProperty(TPhysicalLine(ALines[Index]).Content,
      RawValue) then
      Continue;
    Value := AsciiBytesToUnicode(RawValue);
    if not AFound then
    begin
      ADeclared := Value;
      AFound := True;
    end
    else if not AsciiSameText(ADeclared, Value) then
      AConflict := True;
  end;
end;

function ResolveDeclaredCodePage(const ACharset: UnicodeString;
  out ACodePage: Integer): Boolean;
var
  Value: UnicodeString;
begin
  Value := TrimUnicode(ACharset);
  if AsciiSameText(Value, 'utf-8') or AsciiSameText(Value, 'utf8') then
  begin
    ACodePage := 65001;
    Exit(True);
  end;
  if AsciiSameText(Value, 'windows-1252') or
     AsciiSameText(Value, 'windows1252') or
     AsciiSameText(Value, 'cp1252') or AsciiSameText(Value, '1252') then
  begin
    ACodePage := 1252;
    Exit(True);
  end;
  ACodePage := 0;
  Result := False;
end;

function SystemCodePage: Integer;
begin
  Result := DefaultSystemCodePage;
  if Result = 0 then
    Result := 1252;
end;

procedure ConfigureModuleEncoding(AModule: TTheWordOntModule;
  const APayload: RawByteString; ALines: TObjectList; AVerseCount: Integer);
var
  CharsetFound, CharsetConflict, Supported: Boolean;
  DeclaredCodePage: Integer;
begin
  FindDeclaredCharset(ALines, AVerseCount, AModule.FDeclaredCharset,
    CharsetFound, CharsetConflict);
  AModule.FEncodingIssue := '';

  if CharsetConflict then
  begin
    AModule.FEncoding := twoeUnsupportedDeclared;
    if AModule.FHasBom or IsValidUtf8(APayload) then
      AModule.FCodePage := 65001
    else
      AModule.FCodePage := SystemCodePage;
    AModule.FCanReencode := False;
    AModule.FEncodingIssue := 'Conflicting active charset properties';
    Exit;
  end;

  if CharsetFound then
  begin
    Supported := ResolveDeclaredCodePage(AModule.FDeclaredCharset,
      DeclaredCodePage);
    if not Supported then
    begin
      AModule.FEncoding := twoeUnsupportedDeclared;
      if AModule.FHasBom or IsValidUtf8(APayload) then
        AModule.FCodePage := 65001
      else
        AModule.FCodePage := SystemCodePage;
      AModule.FCanReencode := False;
      AModule.FEncodingIssue := 'Unsupported declared charset: ' +
        AModule.FDeclaredCharset;
      Exit;
    end;

    if AModule.FHasBom and (DeclaredCodePage <> 65001) then
    begin
      AModule.FEncoding := twoeUnsupportedDeclared;
      AModule.FCodePage := 65001;
      AModule.FCanReencode := False;
      AModule.FEncodingIssue := 'UTF-8 BOM conflicts with declared charset ' +
        AModule.FDeclaredCharset;
      Exit;
    end;

    AModule.FCodePage := DeclaredCodePage;
    if DeclaredCodePage = 65001 then
      AModule.FEncoding := twoeUtf8
    else
      AModule.FEncoding := twoeDeclaredCodePage;
    AModule.FCanReencode := BytesRoundTrip(APayload, DeclaredCodePage);
    if not AModule.FCanReencode then
    begin
      AModule.FEncodingIssue := 'Module bytes do not round-trip through ' +
        AModule.FDeclaredCharset;
      AModule.FEncoding := twoeUnsupportedDeclared;
    end;
    Exit;
  end;

  if AModule.FHasBom or IsValidUtf8(APayload) then
  begin
    AModule.FEncoding := twoeUtf8;
    AModule.FCodePage := 65001;
  end
  else
  begin
    AModule.FEncoding := twoeSystemAnsi;
    AModule.FCodePage := SystemCodePage;
  end;
  AModule.FCanReencode := BytesRoundTrip(APayload, AModule.FCodePage);
  if not AModule.FCanReencode then
    AModule.FEncodingIssue := 'Detected code page cannot reproduce source bytes';
end;

procedure SplitPhysicalLines(const ABytes: RawByteString; ALines: TObjectList;
  out ACRLFCount, ALFCount, ACRCount: Integer);
var
  Index, StartIndex: Integer;
  Line: TPhysicalLine;
begin
  ACRLFCount := 0;
  ALFCount := 0;
  ACRCount := 0;
  Index := 1;
  while Index <= Length(ABytes) do
  begin
    StartIndex := Index;
    while (Index <= Length(ABytes)) and (ABytes[Index] <> #13) and
      (ABytes[Index] <> #10) do
      Inc(Index);

    Line := TPhysicalLine.Create;
    Line.Content := Copy(ABytes, StartIndex, Index - StartIndex);
    if Index <= Length(ABytes) then
    begin
      if (ABytes[Index] = #13) and (Index < Length(ABytes)) and
        (ABytes[Index + 1] = #10) then
      begin
        Line.Ending := #13#10;
        Inc(ACRLFCount);
        Inc(Index, 2);
      end
      else if ABytes[Index] = #13 then
      begin
        Line.Ending := #13;
        Inc(ACRCount);
        Inc(Index);
      end
      else
      begin
        Line.Ending := #10;
        Inc(ALFCount);
        Inc(Index);
      end;
    end;
    ALines.Add(Line);
  end;
end;

procedure SetNewLineMetadata(AModule: TTheWordOntModule; ACRLFCount,
  ALFCount, ACRCount: Integer);
var
  NonZeroKinds: Integer;
begin
  NonZeroKinds := 0;
  if ACRLFCount > 0 then Inc(NonZeroKinds);
  if ALFCount > 0 then Inc(NonZeroKinds);
  if ACRCount > 0 then Inc(NonZeroKinds);

  if NonZeroKinds = 0 then
    AModule.FNewLineStyle := twleNone
  else if NonZeroKinds > 1 then
    AModule.FNewLineStyle := twleMixed
  else if ACRLFCount > 0 then
    AModule.FNewLineStyle := twleCRLF
  else if ALFCount > 0 then
    AModule.FNewLineStyle := twleLF
  else
    AModule.FNewLineStyle := twleCR;

  if (ACRLFCount >= ALFCount) and (ACRLFCount >= ACRCount) and
     (ACRLFCount > 0) then
    AModule.FPreferredNewLine := #13#10
  else if (ALFCount >= ACRCount) and (ALFCount > 0) then
    AModule.FPreferredNewLine := #10
  else if ACRCount > 0 then
    AModule.FPreferredNewLine := #13
  else
    AModule.FPreferredNewLine := LineEnding;
end;

procedure ParseRecord(ARecord: TTheWordOntRecord);
var
  Work: UnicodeString;
  Separator: Integer;
begin
  Work := TrimUnicode(ARecord.FRawText);
  ARecord.FKey := '';
  ARecord.FValue := '';
  ARecord.FActive := False;

  if Work = '' then
  begin
    ARecord.FKind := tworkBlank;
    Exit;
  end;

  if (Work[1] = '#') or (Work[1] = '.') or (Work[1] = ';') then
  begin
    ARecord.FKind := tworkDisabled;
    Exit;
  end;

  Separator := Pos('=', Work);
  if Separator > 0 then
  begin
    ARecord.FKind := tworkProperty;
    ARecord.FKey := TrimUnicode(Copy(Work, 1, Separator - 1));
    ARecord.FValue := Copy(Work, Separator + 1, MaxInt);
    ARecord.FActive := ARecord.FKey <> '';
    Exit;
  end;

  if (Length(ARecord.FRawText) > 0) and
     ((ARecord.FRawText[1] = ' ') or (ARecord.FRawText[1] = #9)) then
    ARecord.FKind := tworkContinuation
  else
    ARecord.FKind := tworkUnknown;
end;

function TheWordClassifyOntRecord(const ARaw: UnicodeString):
  TTheWordOntRecordKind;
var
  RecordItem: TTheWordOntRecord;
begin
  RecordItem := TTheWordOntRecord.Create;
  try
    RecordItem.FRawText := ARaw;
    ParseRecord(RecordItem);
    Result := RecordItem.Kind;
  finally
    RecordItem.Free;
  end;
end;

function IsHighSurrogate(AChar: WideChar): Boolean;
begin
  Result := (Ord(AChar) >= $D800) and (Ord(AChar) <= $DBFF);
end;

function IsLowSurrogate(AChar: WideChar): Boolean;
begin
  Result := (Ord(AChar) >= $DC00) and (Ord(AChar) <= $DFFF);
end;

function IsWellFormedUtf16(const AText: UnicodeString): Boolean;
var
  Index: Integer;
begin
  Index := 1;
  while Index <= Length(AText) do
  begin
    if IsHighSurrogate(AText[Index]) then
    begin
      if (Index = Length(AText)) or not IsLowSurrogate(AText[Index + 1]) then
        Exit(False);
      Inc(Index, 2);
    end
    else
    begin
      if IsLowSurrogate(AText[Index]) then
        Exit(False);
      Inc(Index);
    end;
  end;
  Result := True;
end;

function IsUtf16Boundary(const AText: UnicodeString; AOffset: Integer): Boolean;
begin
  Result := (AOffset >= 0) and (AOffset <= Length(AText));
  if Result and (AOffset > 0) and (AOffset < Length(AText)) then
    Result := not (IsHighSurrogate(AText[AOffset]) and
      IsLowSurrogate(AText[AOffset + 1]));
end;

function IsTextSpace(AChar: WideChar): Boolean;
begin
  Result := AChar in [' ', #9, #10, #13];
end;

function IsTranslatorNoteWordChar(AChar: WideChar): Boolean;
begin
  Result := (AChar >= #$0080) or
    (AChar in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-', '''']);
end;

function TransformProtectedAnchor(AOffset, AStartOffset, AEndOffset,
  AReplacementLength: Integer): Integer;
var
  Delta: Integer;
begin
  Delta := AReplacementLength - (AEndOffset - AStartOffset);
  if AOffset < AStartOffset then
    Exit(AOffset);
  if AStartOffset = AEndOffset then
    Exit(AOffset + AReplacementLength);
  if AOffset >= AEndOffset then
    Exit(AOffset + Delta);
  Result := AStartOffset;
end;

{ TTheWordVerseDocument }

constructor TTheWordVerseDocument.Create;
begin
  inherited Create;
  FCanEdit := True;
  FRequiredCodePage := 65001;
  FMarks := TObjectList.Create(True);
  FLexicalItems := TObjectList.Create(True);
  FOpaqueItems := TObjectList.Create(True);
  FStructuralItems := TObjectList.Create(True);
  FTranslatorNotes := TObjectList.Create(True);
  FEvents := TObjectList.Create(True);
  FNextTranslatorNoteId := 1;
end;

procedure TTheWordVerseDocument.EnsureEditable;
begin
  if not FCanEdit then
    raise ETheWordOntEncodingError.Create(UTF8Encode(FEditBlockReason));
end;

destructor TTheWordVerseDocument.Destroy;
begin
  FEvents.Free;
  FTranslatorNotes.Free;
  FStructuralItems.Free;
  FOpaqueItems.Free;
  FLexicalItems.Free;
  FMarks.Free;
  inherited Destroy;
end;

function TTheWordVerseDocument.GetMark(AIndex: Integer):
  TTheWordSemanticMark;
begin
  Result := TTheWordSemanticMark(FMarks[AIndex]);
end;

function TTheWordVerseDocument.GetMarkCount: Integer;
begin
  Result := FMarks.Count;
end;

function TTheWordVerseDocument.GetLexicalItem(AIndex: Integer):
  TTheWordLexicalItem;
begin
  Result := TTheWordLexicalItem(FLexicalItems[AIndex]);
end;

function TTheWordVerseDocument.GetLexicalItemCount: Integer;
begin
  Result := FLexicalItems.Count;
end;

function TTheWordVerseDocument.GetOpaqueItem(AIndex: Integer):
  TTheWordOpaqueItem;
begin
  Result := TTheWordOpaqueItem(FOpaqueItems[AIndex]);
end;

function TTheWordVerseDocument.GetOpaqueItemCount: Integer;
begin
  Result := FOpaqueItems.Count;
end;

function TTheWordVerseDocument.GetStructuralItem(AIndex: Integer):
  TTheWordStructuralItem;
begin
  Result := TTheWordStructuralItem(FStructuralItems[AIndex]);
end;

function TTheWordVerseDocument.GetStructuralItemCount: Integer;
begin
  Result := FStructuralItems.Count;
end;

function TTheWordVerseDocument.GetTranslatorNote(AIndex: Integer):
  TTheWordTranslatorNote;
begin
  Result := TTheWordTranslatorNote(FTranslatorNotes[AIndex]);
end;

function TTheWordVerseDocument.GetTranslatorNoteCount: Integer;
begin
  Result := FTranslatorNotes.Count;
end;

function TTheWordVerseDocument.LexicalProtectedStart(AIndex: Integer): Integer;
var
  Anchor: Integer;
begin
  Anchor := LexicalItems[AIndex].Offset;
  Result := Anchor;
  while (Result > 0) and not IsTextSpace(FVisibleText[Result]) do
    Dec(Result);
end;

function TTheWordVerseDocument.CanApplySemanticRange(AStartOffset,
  AEndOffset: Integer): Boolean;
begin
  Result := FCanEdit and (AStartOffset >= 0) and
    (AEndOffset > AStartOffset) and
    (AEndOffset <= Length(FVisibleText)) and
    IsUtf16Boundary(FVisibleText, AStartOffset) and
    IsUtf16Boundary(FVisibleText, AEndOffset);
end;

function TTheWordVerseDocument.CanInsertParagraphMark(
  AOffsetUTF16: Integer): Boolean;
var
  Anchor, Index, RangeIndex, WordStart: Integer;
  OpaqueItem: TTheWordOpaqueItem;
  TitleRanges: TTheWordTitleRangeArray;
begin
  Result := FCanEdit and (AOffsetUTF16 >= 0) and
    (AOffsetUTF16 <= Length(FVisibleText)) and
    IsUtf16Boundary(FVisibleText, AOffsetUTF16);
  if not Result then
    Exit;

  CollectTheWordTitleRanges(Self, TitleRanges);
  for RangeIndex := 0 to High(TitleRanges) do
    if (AOffsetUTF16 >= TitleRanges[RangeIndex].StartOffset) and
       (AOffsetUTF16 < TitleRanges[RangeIndex].EndOffset) then
      Exit(False);

  for Index := 0 to FOpaqueItems.Count - 1 do
  begin
    OpaqueItem := TTheWordOpaqueItem(FOpaqueItems[Index]);
    if OpaqueItem.FOffset <> AOffsetUTF16 then
      Continue;
    if IsTheWordTitleAnchor(OpaqueItem.FOrdinal, TitleRanges) then
      Continue;
    { Consecutive paragraph marks are valid. Other opaque objects remain
      protected and cannot acquire new structure at their exact anchor. }
    if OpaqueItem.FRawLexeme <> '<CM>' then
      Exit(False);
  end;

  for Index := 0 to FStructuralItems.Count - 1 do
    if TTheWordStructuralItem(FStructuralItems[Index]).FOffset =
       AOffsetUTF16 then
      Exit(False);

  for Index := 0 to FLexicalItems.Count - 1 do
  begin
    Anchor := TTheWordLexicalItem(FLexicalItems[Index]).FOffset;
    WordStart := LexicalProtectedStart(Index);
    if (AOffsetUTF16 > WordStart) and (AOffsetUTF16 < Anchor) then
      Exit(False);
  end;
end;

function TTheWordVerseDocument.CanInsertTranslatorNote(
  AOffsetUTF16: Integer): Boolean;
begin
  Result := CanInsertParagraphMark(AOffsetUTF16);
  if Result and (AOffsetUTF16 > 0) and
     (AOffsetUTF16 < Length(FVisibleText)) and
     IsTranslatorNoteWordChar(FVisibleText[AOffsetUTF16]) and
     IsTranslatorNoteWordChar(FVisibleText[AOffsetUTF16 + 1]) then
    Result := False;
end;

function TTheWordVerseDocument.CanReplaceVisibleText(AStartUTF16,
  ALengthUTF16: Integer): Boolean;
var
  Anchor, EndOffset, Index, RangeIndex, WordStart: Integer;
  TitleRanges: TTheWordTitleRangeArray;
  TitleRange: TTheWordTitleRange;
  OpaqueItem: TTheWordOpaqueItem;
begin
  if not FCanEdit then
    Exit(False);
  Result := (AStartUTF16 >= 0) and (ALengthUTF16 >= 0) and
    (AStartUTF16 <= Length(FVisibleText)) and
    (ALengthUTF16 <= Length(FVisibleText) - AStartUTF16);
  if not Result then
    Exit;
  EndOffset := AStartUTF16 + ALengthUTF16;
  if not IsUtf16Boundary(FVisibleText, AStartUTF16) or
     not IsUtf16Boundary(FVisibleText, EndOffset) then
    Exit(False);

  CollectTheWordTitleRanges(Self, TitleRanges);
  for RangeIndex := 0 to High(TitleRanges) do
  begin
    TitleRange := TitleRanges[RangeIndex];
    if ALengthUTF16 = 0 then
    begin
      { EndOffset is the beginning of the Bible text that follows a title. }
      if (AStartUTF16 >= TitleRange.StartOffset) and
         (AStartUTF16 < TitleRange.EndOffset) then
        Exit(False);
    end
    else if TitleRange.StartOffset = TitleRange.EndOffset then
    begin
      if (AStartUTF16 < TitleRange.StartOffset) and
         (EndOffset > TitleRange.EndOffset) then
        Exit(False);
    end
    else if (AStartUTF16 < TitleRange.EndOffset) and
            (EndOffset > TitleRange.StartOffset) then
      Exit(False);
  end;

  for Index := 0 to FOpaqueItems.Count - 1 do
  begin
    OpaqueItem := TTheWordOpaqueItem(FOpaqueItems[Index]);
    if IsTheWordTitleAnchor(OpaqueItem.FOrdinal, TitleRanges) then
      Continue;
    Anchor := OpaqueItem.FOffset;
    if ((ALengthUTF16 = 0) and (Anchor = AStartUTF16)) or
       ((ALengthUTF16 > 0) and (Anchor > AStartUTF16) and
        (Anchor < EndOffset)) then
      Exit(False);
  end;
  for Index := 0 to FTranslatorNotes.Count - 1 do
  begin
    Anchor := TTheWordTranslatorNote(FTranslatorNotes[Index]).FOffsetUTF16;
    if (ALengthUTF16 > 0) and (Anchor > AStartUTF16) and
       (Anchor < EndOffset) then
      Exit(False);
  end;
  for Index := 0 to FStructuralItems.Count - 1 do
  begin
    Anchor := TTheWordStructuralItem(FStructuralItems[Index]).FOffset;
    if ((ALengthUTF16 = 0) and (Anchor = AStartUTF16)) or
       ((ALengthUTF16 > 0) and (Anchor > AStartUTF16) and
        (Anchor < EndOffset)) then
      Exit(False);
  end;
  for Index := 0 to FLexicalItems.Count - 1 do
  begin
    Anchor := TTheWordLexicalItem(FLexicalItems[Index]).FOffset;
    WordStart := LexicalProtectedStart(Index);
    if ALengthUTF16 = 0 then
    begin
      if (AStartUTF16 > WordStart) and (AStartUTF16 <= Anchor) then
        Exit(False);
    end
    else if ((AStartUTF16 < Anchor) and (EndOffset > WordStart)) or
            ((Anchor = WordStart) and (Anchor >= AStartUTF16) and
             (Anchor < EndOffset)) then
      Exit(False);
  end;
end;

function TTheWordVerseDocument.AddSemanticMark(AKind: TTheWordSemanticKind;
  AStartOffset, AEndOffset: Integer): TTheWordSemanticMark;
var
  Event: TTheWordInlineEvent;
begin
  EnsureEditable;
  if (AStartOffset < 0) or (AEndOffset < AStartOffset) or
     (AEndOffset > Length(FVisibleText)) then
    raise ETheWordOntCodec.CreateFmt('Invalid semantic range [%d,%d) for text length %d',
      [AStartOffset, AEndOffset, Length(FVisibleText)]);
  if not IsUtf16Boundary(FVisibleText, AStartOffset) or
     not IsUtf16Boundary(FVisibleText, AEndOffset) then
    raise ETheWordOntCodec.Create('Semantic range splits a UTF-16 surrogate pair');

  Result := TTheWordSemanticMark.Create;
  Result.FKind := AKind;
  Result.FStartOffset := AStartOffset;
  Result.FEndOffset := AEndOffset;
  Result.FOpenLexeme := TheWordCanonicalOpen(AKind);
  Result.FCloseLexeme := TheWordCanonicalClose(AKind);
  Result.FImported := False;
  FMarks.Add(Result);

  Event := NewInlineEvent(Self, twiekSemanticOpen, AStartOffset,
    Result.FOpenLexeme);
  Event.SemanticMark := Result;
  Result.FOpenOrdinal := Event.Ordinal;
  Event := NewInlineEvent(Self, twiekSemanticClose, AEndOffset,
    Result.FCloseLexeme);
  Event.SemanticMark := Result;
  Result.FCloseOrdinal := Event.Ordinal;
  FModified := True;
end;

procedure TTheWordVerseDocument.RemoveSemanticMark(
  AMark: TTheWordSemanticMark);
var
  Index: Integer;
begin
  if (AMark = nil) or (FMarks.IndexOf(AMark) < 0) then
    Exit;
  EnsureEditable;
  for Index := FEvents.Count - 1 downto 0 do
    if TTheWordInlineEvent(FEvents[Index]).SemanticMark = AMark then
      FEvents.Delete(Index);
  FMarks.Remove(AMark);
  FModified := True;
end;

function TTheWordVerseDocument.InsertParagraphMark(
  AOffsetUTF16: Integer): Boolean;
var
  Event: TTheWordInlineEvent;
  Item: TTheWordOpaqueItem;
begin
  Result := CanInsertParagraphMark(AOffsetUTF16);
  if not Result then
    Exit;
  EnsureEditable;
  { New events are appended after imported events at the same anchor. At a
    lexical word end this keeps WG/WH/WT/wt attached to the word and emits
    the paragraph mark only after that ordered postfix data. }
  Event := NewInlineEvent(Self, twiekOpaque, AOffsetUTF16, '<CM>');
  Item := TTheWordOpaqueItem.Create;
  Item.FKind := twokUnknownTag;
  Item.FOffset := AOffsetUTF16;
  Item.FRawLexeme := '<CM>';
  Item.FOrdinal := Event.Ordinal;
  FOpaqueItems.Add(Item);
  FModified := True;
end;

function TTheWordVerseDocument.RemoveParagraphMark(AOffsetUTF16: Integer;
  AFromEnd: Boolean): Boolean;
var
  CandidateIndex, CandidateOrdinal, EventIndex, Index, RangeIndex: Integer;
  Item: TTheWordOpaqueItem;
  TitleRanges: TTheWordTitleRangeArray;
begin
  Result := False;
  if not FCanEdit or (AOffsetUTF16 < 0) or
     (AOffsetUTF16 > Length(FVisibleText)) or
     not IsUtf16Boundary(FVisibleText, AOffsetUTF16) then
    Exit;

  CollectTheWordTitleRanges(Self, TitleRanges);
  for RangeIndex := 0 to High(TitleRanges) do
    if (AOffsetUTF16 >= TitleRanges[RangeIndex].StartOffset) and
       (AOffsetUTF16 < TitleRanges[RangeIndex].EndOffset) then
      Exit;

  CandidateIndex := -1;
  CandidateOrdinal := 0;
  for Index := 0 to FOpaqueItems.Count - 1 do
  begin
    Item := TTheWordOpaqueItem(FOpaqueItems[Index]);
    if (Item.FOffset <> AOffsetUTF16) or
       (Item.FRawLexeme <> '<CM>') then
      Continue;
    if (CandidateIndex < 0) or
       (AFromEnd and (Item.FOrdinal > CandidateOrdinal)) or
       ((not AFromEnd) and (Item.FOrdinal < CandidateOrdinal)) then
    begin
      CandidateIndex := Index;
      CandidateOrdinal := Item.FOrdinal;
    end;
  end;
  if CandidateIndex < 0 then
    Exit;

  EnsureEditable;
  for EventIndex := FEvents.Count - 1 downto 0 do
    if (TTheWordInlineEvent(FEvents[EventIndex]).Kind = twiekOpaque) and
       (TTheWordInlineEvent(FEvents[EventIndex]).Ordinal =
        CandidateOrdinal) then
    begin
      FEvents.Delete(EventIndex);
      Break;
    end;
  FOpaqueItems.Delete(CandidateIndex);
  FModified := True;
  Result := True;
end;

function TTheWordVerseDocument.InsertTranslatorNote(AOffsetUTF16: Integer;
  const AQ, ABodyRaw: UnicodeString): TTheWordTranslatorNote;
var
  Body, OpenTag, Raw: UnicodeString;
  BodyCapability: TTheWordLayerCapability;
  EncodedCandidate: RawByteString;
  Event: TTheWordInlineEvent;
begin
  Result := nil;
  if not CanInsertTranslatorNote(AOffsetUTF16) or
     not IsWellFormedUtf16(ABodyRaw) or
     ContainsTranslatorNoteOpenTag(ABodyRaw) or
     ContainsTranslatorNoteCloseTag(ABodyRaw) or
     not TryCanonicalTranslatorNoteOpen(AQ, OpenTag) then
    Exit;
  if not TryCanonicalizeTheWordTranslatorNoteBody(ABodyRaw, Body,
    BodyCapability) then
  begin
    Body := ABodyRaw;
    BodyCapability := AnalyzeTheWordTranslatorNoteBody(Body);
  end;
  Raw := OpenTag + Body + '<Rf>';
  if not TryEncodeBytes(Raw, FRequiredCodePage, EncodedCandidate) then
    Exit;

  EnsureEditable;
  Result := TTheWordTranslatorNote.Create;
  Result.FId := FNextTranslatorNoteId;
  Inc(FNextTranslatorNoteId);
  Result.FOffsetUTF16 := AOffsetUTF16;
  Result.FQ := AQ;
  Result.FBodyRaw := Body;
  Result.FOpenLexeme := OpenTag;
  Result.FCloseLexeme := '<Rf>';
  Result.FCapability := BodyCapability;
  Result.FImported := False;
  Result.FModified := True;
  Event := NewInlineEvent(Self, twiekTranslatorNote, AOffsetUTF16, Raw);
  Event.TranslatorNote := Result;
  Result.FOrdinal := Event.Ordinal;
  FTranslatorNotes.Add(Result);
  FModified := True;
end;

function TTheWordVerseDocument.TryGetTranslatorNote(ANoteId: QWord;
  out ANote: TTheWordTranslatorNote): Boolean;
var
  Index: Integer;
begin
  ANote := nil;
  for Index := 0 to FTranslatorNotes.Count - 1 do
    if TTheWordTranslatorNote(FTranslatorNotes[Index]).FId = ANoteId then
    begin
      ANote := TTheWordTranslatorNote(FTranslatorNotes[Index]);
      Exit(True);
    end;
  Result := False;
end;

function TTheWordVerseDocument.UpdateTranslatorNote(ANoteId: QWord;
  const AQ, ABodyRaw: UnicodeString): Boolean;
var
  Body, CloseTag, ExistingQ, OpenTag, Raw: UnicodeString;
  BodyCapability: TTheWordLayerCapability;
  EncodedCandidate: RawByteString;
  Event: TTheWordInlineEvent;
  Index: Integer;
  MarkerChanged, OpenAttributesSupported: Boolean;
  Note: TTheWordTranslatorNote;
begin
  Result := False;
  if not TryGetTranslatorNote(ANoteId, Note) or
     not IsWellFormedUtf16(ABodyRaw) or
     ContainsTranslatorNoteOpenTag(ABodyRaw) or
     ContainsTranslatorNoteCloseTag(ABodyRaw) then
    Exit;

  MarkerChanged := Note.FQ <> AQ;
  if MarkerChanged then
  begin
    TryParseTranslatorNoteOpen(Note.FOpenLexeme, ExistingQ,
      OpenAttributesSupported);
    if not OpenAttributesSupported or
       not TryCanonicalTranslatorNoteOpen(AQ, OpenTag) then
      Exit;
  end
  else
    OpenTag := Note.FOpenLexeme;

  if not MarkerChanged and (Note.FBodyRaw = ABodyRaw) then
    Exit;
  if not TryCanonicalizeTheWordTranslatorNoteBody(ABodyRaw, Body,
    BodyCapability) then
  begin
    Body := ABodyRaw;
    BodyCapability := AnalyzeTheWordTranslatorNoteBody(Body);
  end;

  { Editing only the body must not normalize imported RF whitespace, quote
    style, supported attributes, or a malformed missing close. }
  CloseTag := Note.FCloseLexeme;
  Raw := OpenTag + Body + CloseTag;
  if not TryEncodeBytes(Raw, FRequiredCodePage, EncodedCandidate) then
    Exit;

  EnsureEditable;
  Note.FQ := AQ;
  Note.FBodyRaw := Body;
  Note.FOpenLexeme := OpenTag;
  Note.FCloseLexeme := CloseTag;
  Note.FCapability := BodyCapability;
  TryParseTranslatorNoteOpen(OpenTag, ExistingQ,
    OpenAttributesSupported);
  if not OpenAttributesSupported or (CloseTag = '') then
    Note.FCapability := twlcRawOnly;
  Note.FModified := True;
  for Index := 0 to FEvents.Count - 1 do
  begin
    Event := TTheWordInlineEvent(FEvents[Index]);
    if Event.TranslatorNote = Note then
    begin
      Event.RawLexeme := Raw;
      Break;
    end;
  end;
  FModified := True;
  Result := True;
end;

function TTheWordVerseDocument.RemoveTranslatorNote(
  ANoteId: QWord): Boolean;
var
  Event: TTheWordInlineEvent;
  EventIndex, NoteIndex: Integer;
  Note: TTheWordTranslatorNote;
begin
  Result := False;
  if not TryGetTranslatorNote(ANoteId, Note) then
    Exit;
  EnsureEditable;
  for EventIndex := FEvents.Count - 1 downto 0 do
  begin
    Event := TTheWordInlineEvent(FEvents[EventIndex]);
    if Event.TranslatorNote = Note then
    begin
      FEvents.Delete(EventIndex);
      Break;
    end;
  end;
  NoteIndex := FTranslatorNotes.IndexOf(Note);
  if NoteIndex >= 0 then
    FTranslatorNotes.Delete(NoteIndex);
  FModified := True;
  Result := True;
end;

function TTheWordVerseDocument.ToggleSemanticMark(
  AKind: TTheWordSemanticKind; AStartOffset, AEndOffset: Integer): Boolean;
var
  Affected: TList;
  BestEnd, ComponentEnd, ComponentStart, CoverageEnd, Index: Integer;
  Expanded, FullyCovered: Boolean;
  Mark: TTheWordSemanticMark;
begin
  Result := False;
  EnsureEditable;
  if (AStartOffset < 0) or (AEndOffset < AStartOffset) or
     (AEndOffset > Length(FVisibleText)) then
    raise ETheWordOntCodec.CreateFmt(
      'Invalid semantic range [%d,%d) for text length %d',
      [AStartOffset, AEndOffset, Length(FVisibleText)]);
  if not IsUtf16Boundary(FVisibleText, AStartOffset) or
     not IsUtf16Boundary(FVisibleText, AEndOffset) then
    raise ETheWordOntCodec.Create(
      'Semantic range splits a UTF-16 surrogate pair');
  if AStartOffset = AEndOffset then
    Exit;

  { Treat all marks of this kind as one effective coverage.  Adjacent marks
    have no visible gap and therefore also count as continuous coverage. }
  CoverageEnd := AStartOffset;
  repeat
    BestEnd := CoverageEnd;
    for Index := 0 to FMarks.Count - 1 do
    begin
      Mark := TTheWordSemanticMark(FMarks[Index]);
      if (Mark.FKind = AKind) and
         (Mark.FStartOffset <= CoverageEnd) and
         (Mark.FEndOffset > BestEnd) then
        BestEnd := Mark.FEndOffset;
    end;
    if BestEnd = CoverageEnd then
      Break;
    CoverageEnd := BestEnd;
  until CoverageEnd >= AEndOffset;
  FullyCovered := CoverageEnd >= AEndOffset;

  Affected := TList.Create;
  try
    if FullyCovered then
    begin
      { Subtract the selection from the effective marked interval.  Marks
        wholly outside the selection are deliberately left untouched. }
      ComponentStart := AStartOffset;
      ComponentEnd := AEndOffset;
      for Index := 0 to FMarks.Count - 1 do
      begin
        Mark := TTheWordSemanticMark(FMarks[Index]);
        if (Mark.FKind = AKind) and
           (Mark.FStartOffset < AEndOffset) and
           (Mark.FEndOffset > AStartOffset) then
        begin
          Affected.Add(Mark);
          if Mark.FStartOffset < ComponentStart then
            ComponentStart := Mark.FStartOffset;
          if Mark.FEndOffset > ComponentEnd then
            ComponentEnd := Mark.FEndOffset;
        end;
      end;

      for Index := Affected.Count - 1 downto 0 do
        RemoveSemanticMark(TTheWordSemanticMark(Affected[Index]));
      if ComponentStart < AStartOffset then
        AddSemanticMark(AKind, ComponentStart, AStartOffset);
      if ComponentEnd > AEndOffset then
        AddSemanticMark(AKind, AEndOffset, ComponentEnd);
    end
    else
    begin
      { Fill the selection and merge every overlapping or adjacent mark in
        the connected component.  Repeating the scan is necessary when an
        interval discovered later expands the component towards an earlier
        mark in list order. }
      ComponentStart := AStartOffset;
      ComponentEnd := AEndOffset;
      repeat
        Expanded := False;
        for Index := 0 to FMarks.Count - 1 do
        begin
          Mark := TTheWordSemanticMark(FMarks[Index]);
          if (Mark.FKind <> AKind) or (Affected.IndexOf(Mark) >= 0) then
            Continue;
          if (Mark.FStartOffset <= ComponentEnd) and
             (Mark.FEndOffset >= ComponentStart) then
          begin
            Affected.Add(Mark);
            if Mark.FStartOffset < ComponentStart then
            begin
              ComponentStart := Mark.FStartOffset;
              Expanded := True;
            end;
            if Mark.FEndOffset > ComponentEnd then
            begin
              ComponentEnd := Mark.FEndOffset;
              Expanded := True;
            end;
          end;
        end;
      until not Expanded;

      for Index := Affected.Count - 1 downto 0 do
        RemoveSemanticMark(TTheWordSemanticMark(Affected[Index]));
      AddSemanticMark(AKind, ComponentStart, ComponentEnd);
    end;
    Result := True;
  finally
    Affected.Free;
  end;
end;

procedure TTheWordVerseDocument.ReplaceVisibleText(AStartUTF16,
  ALengthUTF16: Integer; const AReplacement: UnicodeString);
var
  EndOffset, ReplacementLength, Delta, Index: Integer;
  Candidate: UnicodeString;
  EncodedCandidate: RawByteString;
  Mark: TTheWordSemanticMark;
  Event: TTheWordInlineEvent;
  Lexical: TTheWordLexicalItem;
  Opaque: TTheWordOpaqueItem;
  Structural: TTheWordStructuralItem;
  TranslatorNote: TTheWordTranslatorNote;
  TitleRanges: TTheWordTitleRangeArray;
begin
  if (AStartUTF16 < 0) or (ALengthUTF16 < 0) or
     (AStartUTF16 > Length(FVisibleText)) or
     (ALengthUTF16 > Length(FVisibleText) - AStartUTF16) then
    raise ETheWordOntCodec.CreateFmt(
      'Invalid UTF-16 replacement range [%d,%d) for text length %d',
      [AStartUTF16, AStartUTF16 + ALengthUTF16, Length(FVisibleText)]);
  EndOffset := AStartUTF16 + ALengthUTF16;
  if not IsUtf16Boundary(FVisibleText, AStartUTF16) or
     not IsUtf16Boundary(FVisibleText, EndOffset) then
    raise ETheWordOntCodec.Create('Replacement range splits a UTF-16 surrogate pair');
  if not IsWellFormedUtf16(AReplacement) then
    raise ETheWordOntCodec.Create('Replacement contains an unpaired UTF-16 surrogate');

  if (ALengthUTF16 = Length(AReplacement)) and
     (Copy(FVisibleText, AStartUTF16 + 1, ALengthUTF16) = AReplacement) then
    Exit;
  EnsureEditable;
  if not CanReplaceVisibleText(AStartUTF16, ALengthUTF16) then
    raise ETheWordOntProtectedEdit.CreateFmt(
      'Replacement intersects a protected ONT region at UTF-16 range [%d,%d)',
      [AStartUTF16, EndOffset]);
  CollectTheWordTitleRanges(Self, TitleRanges);

  ReplacementLength := Length(AReplacement);
  Candidate := Copy(FVisibleText, 1, AStartUTF16) + AReplacement +
    Copy(FVisibleText, EndOffset + 1, MaxInt);
  if not TryEncodeBytes(Candidate, FRequiredCodePage, EncodedCandidate) then
    raise ETheWordOntEncodingError.CreateFmt(
      'Replacement cannot be represented losslessly in code page %d',
      [FRequiredCodePage]);

  Delta := ReplacementLength - ALengthUTF16;
  for Index := 0 to FMarks.Count - 1 do
  begin
    Mark := TTheWordSemanticMark(FMarks[Index]);
    if Mark.FEndOffset <= AStartUTF16 then
      Continue
    else if Mark.FStartOffset >= EndOffset then
    begin
      Inc(Mark.FStartOffset, Delta);
      Inc(Mark.FEndOffset, Delta);
    end
    else
    begin
      if Mark.FStartOffset >= AStartUTF16 then
        Mark.FStartOffset := AStartUTF16;
      if Mark.FEndOffset <= EndOffset then
        Mark.FEndOffset := AStartUTF16 + ReplacementLength
      else
        Inc(Mark.FEndOffset, Delta);
    end;
  end;

  for Index := 0 to FLexicalItems.Count - 1 do
  begin
    Lexical := TTheWordLexicalItem(FLexicalItems[Index]);
    Lexical.FOffset := TransformProtectedAnchor(Lexical.FOffset,
      AStartUTF16, EndOffset, ReplacementLength);
  end;
  for Index := 0 to FOpaqueItems.Count - 1 do
  begin
    Opaque := TTheWordOpaqueItem(FOpaqueItems[Index]);
    if not ((ALengthUTF16 = 0) and
      KeepTheWordTitleAnchorAtBodyInsertion(Opaque.FOrdinal,
        AStartUTF16, TitleRanges)) then
      Opaque.FOffset := TransformProtectedAnchor(Opaque.FOffset,
        AStartUTF16, EndOffset, ReplacementLength);
  end;

  for Index := 0 to FTranslatorNotes.Count - 1 do
  begin
    TranslatorNote := TTheWordTranslatorNote(FTranslatorNotes[Index]);
    TranslatorNote.FOffsetUTF16 := TransformProtectedAnchor(
      TranslatorNote.FOffsetUTF16, AStartUTF16, EndOffset,
      ReplacementLength);
  end;

  for Index := 0 to FStructuralItems.Count - 1 do
  begin
    Structural := TTheWordStructuralItem(FStructuralItems[Index]);
    Structural.FOffset := TransformProtectedAnchor(Structural.FOffset,
      AStartUTF16, EndOffset, ReplacementLength);
  end;

  for Index := 0 to FEvents.Count - 1 do
  begin
    Event := TTheWordInlineEvent(FEvents[Index]);
    case Event.Kind of
      twiekSemanticOpen:
        if Event.SemanticMark <> nil then
          Event.Offset := Event.SemanticMark.FStartOffset;
      twiekSemanticClose:
        if Event.SemanticMark <> nil then
          Event.Offset := Event.SemanticMark.FEndOffset;
      twiekOpaque:
        if not ((ALengthUTF16 = 0) and
          KeepTheWordTitleAnchorAtBodyInsertion(Event.Ordinal,
            AStartUTF16, TitleRanges)) then
          Event.Offset := TransformProtectedAnchor(Event.Offset,
            AStartUTF16, EndOffset, ReplacementLength);
      twiekTranslatorNote:
        if Event.TranslatorNote <> nil then
          Event.Offset := Event.TranslatorNote.FOffsetUTF16;
      twiekLexical, twiekStructural:
        Event.Offset := TransformProtectedAnchor(Event.Offset,
          AStartUTF16, EndOffset, ReplacementLength);
    end;
  end;

  FVisibleText := Copy(FVisibleText, 1, AStartUTF16) + AReplacement +
    Copy(FVisibleText, EndOffset + 1, MaxInt);
  FModified := True;
end;

{ TTheWordOntRecord }

function TTheWordOntRecord.GetIsActiveRule: Boolean;
begin
  Result := FActive and (FKind = tworkProperty) and
    (AsciiSameText(FKey, 'tag.rule') or AsciiSameText(FKey, 'verse.rule'));
end;

{ TTheWordOntModule }

constructor TTheWordOntModule.Create;
begin
  inherited Create;
  FVerses := TObjectList.Create(True);
  FRecords := TObjectList.Create(True);
  FEncoding := twoeUtf8;
  FCodePage := 65001;
  FCanReencode := True;
  FPreferredNewLine := LineEnding;
end;

destructor TTheWordOntModule.Destroy;
begin
  FRecords.Free;
  FVerses.Free;
  inherited Destroy;
end;

function TTheWordOntModule.GetVerse(AIndex: Integer):
  TTheWordVerseDocument;
begin
  Result := TTheWordVerseDocument(FVerses[AIndex]);
end;

function TTheWordOntModule.GetVerseCount: Integer;
begin
  Result := FVerses.Count;
end;

function TTheWordOntModule.GetRecord(AIndex: Integer): TTheWordOntRecord;
begin
  Result := TTheWordOntRecord(FRecords[AIndex]);
end;

function TTheWordOntModule.GetRecordCount: Integer;
begin
  Result := FRecords.Count;
end;

function TTheWordOntModule.GetModified: Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to FVerses.Count - 1 do
    if TTheWordVerseDocument(FVerses[Index]).Modified then
      Exit(True);
end;

{ TTheWordOntCodecV2 }

procedure TTheWordOntCodecV2.AddOpaque(ADocument: TTheWordVerseDocument;
  AOffset: Integer; const ARaw: UnicodeString; AKind: TTheWordOpaqueKind);
var
  Item: TTheWordOpaqueItem;
  Event: TTheWordInlineEvent;
begin
  Event := NewInlineEvent(ADocument, twiekOpaque, AOffset, ARaw);
  Item := TTheWordOpaqueItem.Create;
  Item.FKind := AKind;
  Item.FOffset := AOffset;
  Item.FRawLexeme := ARaw;
  Item.FOrdinal := Event.Ordinal;
  ADocument.FOpaqueItems.Add(Item);
end;

procedure TTheWordOntCodecV2.AddLexical(ADocument: TTheWordVerseDocument;
  AOffset: Integer; AKind: TTheWordLexicalKind; const ARaw,
  AValue: UnicodeString);
var
  Item: TTheWordLexicalItem;
  Event: TTheWordInlineEvent;
begin
  Event := NewInlineEvent(ADocument, twiekLexical, AOffset, ARaw);
  Item := TTheWordLexicalItem.Create;
  Item.FKind := AKind;
  Item.FOffset := AOffset;
  Item.FRawLexeme := ARaw;
  Item.FValue := AValue;
  Item.FOrdinal := Event.Ordinal;
  ADocument.FLexicalItems.Add(Item);
end;

procedure TTheWordOntCodecV2.AddStructural(
  ADocument: TTheWordVerseDocument; AOffset: Integer;
  const ARaw: UnicodeString);
var
  Item: TTheWordStructuralItem;
  Event: TTheWordInlineEvent;
begin
  Event := NewInlineEvent(ADocument, twiekStructural, AOffset, ARaw);
  Item := TTheWordStructuralItem.Create;
  Item.FKind := twstSyntagmBoundary;
  Item.FOffset := AOffset;
  Item.FRawLexeme := ARaw;
  Item.FOrdinal := Event.Ordinal;
  ADocument.FStructuralItems.Add(Item);
end;

function CompareSemanticMarks(ALeft, ARight: TTheWordSemanticMark): Integer;
begin
  if ALeft.FStartOffset <> ARight.FStartOffset then
    Exit(ALeft.FStartOffset - ARight.FStartOffset);
  if ALeft.FEndOffset <> ARight.FEndOffset then
    Exit(ARight.FEndOffset - ALeft.FEndOffset);
  if Ord(ALeft.FKind) <> Ord(ARight.FKind) then
    Exit(Ord(ALeft.FKind) - Ord(ARight.FKind));
  Result := ALeft.FOpenOrdinal - ARight.FOpenOrdinal;
end;

function ListContainsMark(AList: TList; AMark: TTheWordSemanticMark): Boolean;
begin
  Result := (AList <> nil) and (AList.IndexOf(AMark) >= 0);
end;

function SemanticMarksCross(ALeft, ARight: TTheWordSemanticMark): Boolean;
begin
  Result := ((ALeft.FStartOffset < ARight.FStartOffset) and
             (ARight.FStartOffset < ALeft.FEndOffset) and
             (ALeft.FEndOffset < ARight.FEndOffset)) or
            ((ARight.FStartOffset < ALeft.FStartOffset) and
             (ALeft.FStartOffset < ARight.FEndOffset) and
             (ARight.FEndOffset < ALeft.FEndOffset));
end;

procedure InsertSemanticMarkSorted(AList: TList;
  AMark: TTheWordSemanticMark);
var
  Index: Integer;
begin
  Index := 0;
  while (Index < AList.Count) and
    (CompareSemanticMarks(TTheWordSemanticMark(AList[Index]), AMark) <= 0) do
    Inc(Index);
  AList.Insert(Index, AMark);
end;

procedure BuildActiveSemanticMarks(ADocument: TTheWordVerseDocument;
  AOffset: Integer; ARegenerated, ATarget: TList);
var
  Index: Integer;
  Mark: TTheWordSemanticMark;
begin
  ATarget.Clear;
  for Index := 0 to ADocument.FMarks.Count - 1 do
  begin
    Mark := TTheWordSemanticMark(ADocument.FMarks[Index]);
    if ListContainsMark(ARegenerated, Mark) and
       (Mark.FStartOffset <= AOffset) and (Mark.FEndOffset > AOffset) then
      InsertSemanticMarkSorted(ATarget, Mark);
  end;
end;

procedure BuildRegeneratedSemanticMarks(ADocument: TTheWordVerseDocument;
  ARegenerated: TList);
var
  LeftIndex, RightIndex: Integer;
  LeftMark, RightMark: TTheWordSemanticMark;
begin
  ARegenerated.Clear;
  for LeftIndex := 0 to ADocument.FMarks.Count - 1 do
  begin
    LeftMark := TTheWordSemanticMark(ADocument.FMarks[LeftIndex]);
    if not LeftMark.FImported then
      ARegenerated.Add(LeftMark);
    for RightIndex := LeftIndex + 1 to ADocument.FMarks.Count - 1 do
    begin
      RightMark := TTheWordSemanticMark(ADocument.FMarks[RightIndex]);
      if SemanticMarksCross(LeftMark, RightMark) then
      begin
        if not ListContainsMark(ARegenerated, LeftMark) then
          ARegenerated.Add(LeftMark);
        if not ListContainsMark(ARegenerated, RightMark) then
          ARegenerated.Add(RightMark);
      end;
    end;
  end;
end;

procedure AppendLeadingPostfixEvents(ADocument: TTheWordVerseDocument;
  AOffset: Integer; ARegenerated, AEmitted: TList;
  var AOutput: UnicodeString);
var
  Index: Integer;
  Event: TTheWordInlineEvent;
begin
  AEmitted.Clear;
  for Index := 0 to ADocument.FEvents.Count - 1 do
  begin
    Event := TTheWordInlineEvent(ADocument.FEvents[Index]);
    if Event.Offset <> AOffset then
      Continue;
    case Event.Kind of
      twiekLexical, twiekStructural:
        begin
          AOutput := AOutput + Event.RawLexeme;
          AEmitted.Add(Event);
        end;
      twiekSemanticOpen, twiekSemanticClose:
        begin
          { Regenerated semantic events are replaced by the transition below
            and do not interrupt the original postfix prefix. }
          if (Event.SemanticMark <> nil) and
             ListContainsMark(ARegenerated, Event.SemanticMark) then
            Continue;
          Break;
        end;
    else
      { An opaque or otherwise preserved object breaks attachment to the
        word.  Lexical events after it must retain their original position so
        the legacy parser does not acquire an association that never existed. }
      Break;
    end;
  end;
end;

procedure AppendPreservedEvents(ADocument: TTheWordVerseDocument;
  AOffset: Integer; ARegenerated, AEmitted: TList;
  var AOutput: UnicodeString);
var
  Index: Integer;
  Event: TTheWordInlineEvent;

  procedure AppendInsertedNotes;
  var
    NoteIndex: Integer;
    NoteEvent: TTheWordInlineEvent;
  begin
    for NoteIndex := 0 to ADocument.FEvents.Count - 1 do
    begin
      NoteEvent := TTheWordInlineEvent(ADocument.FEvents[NoteIndex]);
      if (NoteEvent.Offset <> AOffset) or
         (NoteEvent.Kind <> twiekTranslatorNote) or
         (NoteEvent.TranslatorNote = nil) or
         NoteEvent.TranslatorNote.Imported or
         ((AEmitted <> nil) and (AEmitted.IndexOf(NoteEvent) >= 0)) then
        Continue;
      AOutput := AOutput + NoteEvent.RawLexeme;
      if AEmitted <> nil then
        AEmitted.Add(NoteEvent);
    end;
  end;

begin
  for Index := 0 to ADocument.FEvents.Count - 1 do
  begin
    Event := TTheWordInlineEvent(ADocument.FEvents[Index]);
    if Event.Offset <> AOffset then
      Continue;
    if Event.Kind in [twiekSemanticOpen, twiekSemanticClose] then
    begin
      if Event.Kind = twiekSemanticOpen then
        AppendInsertedNotes;
      if (Event.SemanticMark <> nil) and
         not ListContainsMark(ARegenerated, Event.SemanticMark) then
        AOutput := AOutput + Event.RawLexeme;
    end
    else if (Event.Kind = twiekTranslatorNote) and
            (Event.TranslatorNote <> nil) and
            not Event.TranslatorNote.Imported then
      Continue
    else if (Event.Kind in [twiekLexical, twiekOpaque, twiekStructural,
             twiekTranslatorNote]) and
            ((AEmitted = nil) or (AEmitted.IndexOf(Event) < 0)) then
      AOutput := AOutput + Event.RawLexeme;
  end;
  AppendInsertedNotes;
end;

procedure AppendZeroLengthMarks(ADocument: TTheWordVerseDocument;
  AOffset: Integer; ARegenerated: TList; var AOutput: UnicodeString);
var
  Index: Integer;
  ZeroMarks: TList;
  Mark: TTheWordSemanticMark;
begin
  ZeroMarks := TList.Create;
  try
    for Index := 0 to ADocument.FMarks.Count - 1 do
    begin
      Mark := TTheWordSemanticMark(ADocument.FMarks[Index]);
      if ListContainsMark(ARegenerated, Mark) and
         (Mark.FStartOffset = AOffset) and
         (Mark.FEndOffset = AOffset) then
        InsertSemanticMarkSorted(ZeroMarks, Mark);
    end;
    for Index := 0 to ZeroMarks.Count - 1 do
    begin
      Mark := TTheWordSemanticMark(ZeroMarks[Index]);
      AOutput := AOutput + Mark.FOpenLexeme + Mark.FCloseLexeme;
    end;
  finally
    ZeroMarks.Free;
  end;
end;

procedure MergeSyntheticReopenedMarks(ADocument: TTheWordVerseDocument);
var
  CloseIndex, OpenIndex, BetweenIndex, EventIndex: Integer;
  CloseEvent, OpenEvent, BetweenEvent, Event: TTheWordInlineEvent;
  LeftMark, RightMark: TTheWordSemanticMark;
  HasDifferentSemanticBetween: Boolean;
  Merged: Boolean;
begin
  repeat
    Merged := False;
    for CloseIndex := 0 to ADocument.FEvents.Count - 1 do
    begin
      CloseEvent := TTheWordInlineEvent(ADocument.FEvents[CloseIndex]);
      if (CloseEvent.Kind <> twiekSemanticClose) or
         (CloseEvent.SemanticMark = nil) then
        Continue;
      for OpenIndex := CloseIndex + 1 to ADocument.FEvents.Count - 1 do
      begin
        OpenEvent := TTheWordInlineEvent(ADocument.FEvents[OpenIndex]);
        if OpenEvent.Offset <> CloseEvent.Offset then
          Break;
        if (OpenEvent.Kind <> twiekSemanticOpen) or
           (OpenEvent.SemanticMark = nil) or
           (OpenEvent.SemanticMark.FKind <> CloseEvent.SemanticMark.FKind) then
          Continue;
        HasDifferentSemanticBetween := False;
        for BetweenIndex := CloseIndex + 1 to OpenIndex - 1 do
        begin
          BetweenEvent := TTheWordInlineEvent(ADocument.FEvents[BetweenIndex]);
          if (BetweenEvent.Kind in [twiekSemanticOpen, twiekSemanticClose]) and
             (BetweenEvent.SemanticMark <> nil) and
             (BetweenEvent.SemanticMark.FKind <> CloseEvent.SemanticMark.FKind) then
          begin
            HasDifferentSemanticBetween := True;
            Break;
          end;
        end;
        if not HasDifferentSemanticBetween then
          Continue;

        LeftMark := CloseEvent.SemanticMark;
        RightMark := OpenEvent.SemanticMark;
        if (LeftMark = RightMark) or
           (LeftMark.FEndOffset <> RightMark.FStartOffset) then
          Continue;
        LeftMark.FEndOffset := RightMark.FEndOffset;
        LeftMark.FCloseLexeme := RightMark.FCloseLexeme;
        LeftMark.FCloseOrdinal := RightMark.FCloseOrdinal;
        for EventIndex := 0 to ADocument.FEvents.Count - 1 do
        begin
          Event := TTheWordInlineEvent(ADocument.FEvents[EventIndex]);
          if Event.SemanticMark = RightMark then
            Event.SemanticMark := LeftMark;
        end;
        ADocument.FMarks.Remove(RightMark);
        Merged := True;
        Break;
      end;
      if Merged then
        Break;
    end;
  until not Merged;
end;

function TTheWordOntCodecV2.DecodeVerse(const ARaw: UnicodeString):
  TTheWordVerseDocument;
var
  CloseEnd, CloseStart, Index, NoteEnd, TextStart, TagEnd, Offset: Integer;
  Body, CloseTag, Q, RawNote, RawTag, Value: UnicodeString;
  SemanticKind, StackKind: TTheWordSemanticKind;
  Stacks: array[TTheWordSemanticKind] of TList;
  HasClose, Nested, OpenAttributesSupported: Boolean;
  Pending: TPendingSemantic;
  Event: TTheWordInlineEvent;
  Mark: TTheWordSemanticMark;
  Note: TTheWordTranslatorNote;
  OpaqueItem: TTheWordOpaqueItem;
  Stack: TList;
begin
  Result := TTheWordVerseDocument.Create;
  try
    Result.FOriginalRaw := ARaw;
    for StackKind := Low(TTheWordSemanticKind) to High(TTheWordSemanticKind) do
      Stacks[StackKind] := TList.Create;
    try
      Index := 1;
      TextStart := 1;
      while Index <= Length(ARaw) do
      begin
        if (ARaw[Index] <> '<') and (ARaw[Index] <> '|') then
        begin
          Inc(Index);
          Continue;
        end;

        if ARaw[Index] = '|' then
        begin
          if Index > TextStart then
            Result.FVisibleText := Result.FVisibleText +
              Copy(ARaw, TextStart, Index - TextStart);
          Offset := Length(Result.FVisibleText);
          AddStructural(Result, Offset, '|');
          Inc(Index);
          TextStart := Index;
          Continue;
        end;

        TagEnd := FindTagEnd(ARaw, Index);
        if TagEnd = 0 then
          Break;

        if Index > TextStart then
          Result.FVisibleText := Result.FVisibleText +
            Copy(ARaw, TextStart, Index - TextStart);
        Offset := Length(Result.FVisibleText);
        RawTag := Copy(ARaw, Index, TagEnd - Index + 1);

        if IsTranslatorNoteOpenTag(RawTag) then
        begin
          HasClose := FindTranslatorNoteClose(ARaw, TagEnd + 1,
            CloseStart, CloseEnd, Nested);
          if HasClose then
          begin
            Body := Copy(ARaw, TagEnd + 1, CloseStart - TagEnd - 1);
            CloseTag := Copy(ARaw, CloseStart, CloseEnd - CloseStart + 1);
            NoteEnd := CloseEnd;
          end
          else
          begin
            Body := Copy(ARaw, TagEnd + 1, MaxInt);
            CloseTag := '';
            NoteEnd := Length(ARaw);
          end;
          RawNote := Copy(ARaw, Index, NoteEnd - Index + 1);
          TryParseTranslatorNoteOpen(RawTag, Q,
            OpenAttributesSupported);

          Note := TTheWordTranslatorNote.Create;
          Note.FId := Result.FNextTranslatorNoteId;
          Inc(Result.FNextTranslatorNoteId);
          Note.FOffsetUTF16 := Offset;
          Note.FQ := Q;
          Note.FBodyRaw := Body;
          Note.FOpenLexeme := RawTag;
          Note.FCloseLexeme := CloseTag;
          Note.FCapability := AnalyzeTheWordTranslatorNoteBody(Body);
          if not OpenAttributesSupported or Nested or not HasClose then
            Note.FCapability := twlcRawOnly;
          Note.FImported := True;
          Note.FModified := False;
          Event := NewInlineEvent(Result, twiekTranslatorNote, Offset,
            RawNote);
          Event.TranslatorNote := Note;
          Note.FOrdinal := Event.Ordinal;
          Result.FTranslatorNotes.Add(Note);

          Index := NoteEnd + 1;
          TextStart := Index;
          Continue;
        end
        else if SemanticOpenKind(RawTag, SemanticKind) then
        begin
          Event := NewInlineEvent(Result, twiekPendingSemantic, Offset,
            RawTag);
          Pending := TPendingSemantic.Create;
          Pending.Kind := SemanticKind;
          Pending.StartOffset := Offset;
          Pending.OpenLexeme := RawTag;
          Pending.OpenEvent := Event;
          Stacks[SemanticKind].Add(Pending);
        end
        else if SemanticCloseKind(RawTag, SemanticKind) then
        begin
          Stack := Stacks[SemanticKind];
          if Stack.Count = 0 then
            AddOpaque(Result, Offset, RawTag, twokMalformedTag)
          else
          begin
            Pending := TPendingSemantic(Stack[Stack.Count - 1]);
            Stack.Delete(Stack.Count - 1);
            Mark := TTheWordSemanticMark.Create;
            Mark.FKind := SemanticKind;
            Mark.FStartOffset := Pending.StartOffset;
            Mark.FEndOffset := Offset;
            Mark.FOpenLexeme := Pending.OpenLexeme;
            Mark.FCloseLexeme := RawTag;
            Mark.FImported := True;
            Mark.FOpenOrdinal := Pending.OpenEvent.Ordinal;
            Result.FMarks.Add(Mark);
            Pending.OpenEvent.Kind := twiekSemanticOpen;
            Pending.OpenEvent.SemanticMark := Mark;
            Event := NewInlineEvent(Result, twiekSemanticClose, Offset,
              RawTag);
            Event.SemanticMark := Mark;
            Mark.FCloseOrdinal := Event.Ordinal;
            Pending.Free;
          end;
        end
        else if StartsWithCaseSensitive(RawTag, '<WG') and
          IsValidStrongPayload(Copy(RawTag, 4, Length(RawTag) - 4)) then
        begin
          Value := Copy(RawTag, 4, Length(RawTag) - 4);
          AddLexical(Result, Offset, twlkStrongGreek, RawTag, Value);
        end
        else if StartsWithCaseSensitive(RawTag, '<WH') and
          IsValidStrongPayload(Copy(RawTag, 4, Length(RawTag) - 4)) then
        begin
          Value := Copy(RawTag, 4, Length(RawTag) - 4);
          AddLexical(Result, Offset, twlkStrongHebrew, RawTag, Value);
        end
        else if StartsWithCaseSensitive(RawTag, '<WT') and
          (Length(RawTag) > 4) then
        begin
          Value := Copy(RawTag, 4, Length(RawTag) - 4);
          AddLexical(Result, Offset, twlkMorphology, RawTag, Value);
        end
        else if RawTag = '<wt>' then
          AddLexical(Result, Offset, twlkWordTokenEnd, RawTag, '')
        else if StartsWithCaseSensitive(RawTag, '<WG') or
                StartsWithCaseSensitive(RawTag, '<WH') or
                StartsWithCaseSensitive(RawTag, '<WT') then
          AddOpaque(Result, Offset, RawTag, twokMalformedTag)
        else
          AddOpaque(Result, Offset, RawTag, twokUnknownTag);

        Index := TagEnd + 1;
        TextStart := Index;
      end;

      if TextStart <= Length(ARaw) then
        Result.FVisibleText := Result.FVisibleText +
          Copy(ARaw, TextStart, MaxInt);

      for StackKind := Low(TTheWordSemanticKind) to High(TTheWordSemanticKind) do
      begin
        Stack := Stacks[StackKind];
        while Stack.Count > 0 do
        begin
          Pending := TPendingSemantic(Stack[Stack.Count - 1]);
          Stack.Delete(Stack.Count - 1);
          Pending.OpenEvent.Kind := twiekOpaque;
          Pending.OpenEvent.SemanticMark := nil;
          OpaqueItem := TTheWordOpaqueItem.Create;
          OpaqueItem.FKind := twokMalformedTag;
          OpaqueItem.FOffset := Pending.StartOffset;
          OpaqueItem.FRawLexeme := Pending.OpenLexeme;
          OpaqueItem.FOrdinal := Pending.OpenEvent.Ordinal;
          Result.FOpaqueItems.Add(OpaqueItem);
          Pending.Free;
        end;
      end;
    finally
      for StackKind := Low(TTheWordSemanticKind) to High(TTheWordSemanticKind) do
      begin
        for Index := 0 to Stacks[StackKind].Count - 1 do
          TObject(Stacks[StackKind][Index]).Free;
        Stacks[StackKind].Free;
      end;
    end;
    MergeSyntheticReopenedMarks(Result);
    Result.FModified := False;
  except
    Result.Free;
    raise;
  end;
end;

function TTheWordOntCodecV2.EncodeVerse(
  ADocument: TTheWordVerseDocument): UnicodeString;
var
  Offset, Index, CommonCount: Integer;
  HasSemanticTransition: Boolean;
  CurrentMarks, TargetMarks, RegeneratedMarks, EmittedEvents: TList;
  Mark: TTheWordSemanticMark;
begin
  if ADocument = nil then
    raise ETheWordOntCodec.Create('Cannot encode a nil verse document');
  if not ADocument.Modified then
    Exit(ADocument.OriginalRaw);

  Result := '';
  CurrentMarks := TList.Create;
  TargetMarks := TList.Create;
  RegeneratedMarks := TList.Create;
  EmittedEvents := TList.Create;
  try
    BuildRegeneratedSemanticMarks(ADocument, RegeneratedMarks);
    for Offset := 0 to Length(ADocument.FVisibleText) do
    begin
      BuildActiveSemanticMarks(ADocument, Offset, RegeneratedMarks,
        TargetMarks);
      CommonCount := 0;
      while (CommonCount < CurrentMarks.Count) and
            (CommonCount < TargetMarks.Count) and
            (CurrentMarks[CommonCount] = TargetMarks[CommonCount]) do
        Inc(CommonCount);

      HasSemanticTransition := (CommonCount < CurrentMarks.Count) or
        (CommonCount < TargetMarks.Count);
      EmittedEvents.Clear;
      if HasSemanticTransition then
      begin
        { WG/WH/WT/wt are postfix data of the content unit immediately to
          their left.  A regenerated crossing may need to close and reopen
          semantic spans at the same document offset; emitting those tags
          first would make the legacy TONTParser attach lexical data to a
          semantic metadata token instead of to the word. }
        AppendLeadingPostfixEvents(ADocument, Offset, RegeneratedMarks,
          EmittedEvents, Result);
      end;

      for Index := CurrentMarks.Count - 1 downto CommonCount do
      begin
        Mark := TTheWordSemanticMark(CurrentMarks[Index]);
        Result := Result + Mark.FCloseLexeme;
        CurrentMarks.Delete(Index);
      end;

      { Imported events keep their original ordinal at this anchor. }
      AppendPreservedEvents(ADocument, Offset, RegeneratedMarks,
        EmittedEvents, Result);

      for Index := CommonCount to TargetMarks.Count - 1 do
      begin
        Mark := TTheWordSemanticMark(TargetMarks[Index]);
        Result := Result + Mark.FOpenLexeme;
        CurrentMarks.Add(Mark);
      end;

      AppendZeroLengthMarks(ADocument, Offset, RegeneratedMarks, Result);
      if Offset < Length(ADocument.FVisibleText) then
        Result := Result + ADocument.FVisibleText[Offset + 1];
    end;
  finally
    EmittedEvents.Free;
    RegeneratedMarks.Free;
    TargetMarks.Free;
    CurrentMarks.Free;
  end;
end;

function TTheWordOntCodecV2.DecodeModule(const ABytes: RawByteString;
  AVerseCount: Integer): TTheWordOntModule;
var
  Payload: RawByteString;
  Lines: TObjectList;
  CRLFCount, LFCount, CRCount, Index: Integer;
  Physical: TPhysicalLine;
  Document: TTheWordVerseDocument;
  RecordItem: TTheWordOntRecord;
begin
  if AVerseCount < 0 then
    raise ETheWordOntCodec.Create('Verse count cannot be negative');

  Result := TTheWordOntModule.Create;
  Lines := TObjectList.Create(True);
  try
    Result.FOriginalBytes := ABytes;
    Result.FHasBom := Copy(ABytes, 1, Length(UTF8_BOM)) = UTF8_BOM;
    if Result.FHasBom then
      Payload := Copy(ABytes, Length(UTF8_BOM) + 1, MaxInt)
    else
      Payload := ABytes;

    SplitPhysicalLines(Payload, Lines, CRLFCount, LFCount, CRCount);
    SetNewLineMetadata(Result, CRLFCount, LFCount, CRCount);
    if Lines.Count < AVerseCount then
      raise ETheWordOntCodec.CreateFmt(
        'Module has %d physical lines but %d verse lines were requested',
        [Lines.Count, AVerseCount]);
    ConfigureModuleEncoding(Result, Payload, Lines, AVerseCount);

    for Index := 0 to AVerseCount - 1 do
    begin
      Physical := TPhysicalLine(Lines[Index]);
      Document := DecodeVerse(DecodeBytes(Physical.Content,
        Result.FCodePage));
      Document.FSourceBytes := Physical.Content;
      Document.FLineEndingBytes := Physical.Ending;
      Document.FCanEdit := Result.FCanReencode;
      Document.FRequiredCodePage := Result.FCodePage;
      Document.FEditBlockReason := Result.FEncodingIssue;
      Result.FVerses.Add(Document);
    end;

    for Index := AVerseCount to Lines.Count - 1 do
    begin
      Physical := TPhysicalLine(Lines[Index]);
      RecordItem := TTheWordOntRecord.Create;
      RecordItem.FSourceBytes := Physical.Content;
      RecordItem.FLineEndingBytes := Physical.Ending;
      RecordItem.FRawText := DecodeBytes(Physical.Content,
        Result.FCodePage);
      ParseRecord(RecordItem);
      if (Result.FDeclaredCharset = '') and RecordItem.FActive and
         AsciiSameText(RecordItem.FKey, 'charset') then
        Result.FDeclaredCharset := TrimUnicode(RecordItem.FValue);
      Result.FRecords.Add(RecordItem);
    end;
  except
    Lines.Free;
    Result.Free;
    raise;
  end;
  Lines.Free;
end;

function TTheWordOntCodecV2.EncodeModule(
  AModule: TTheWordOntModule): RawByteString;
var
  Index: Integer;
  Document: TTheWordVerseDocument;
  RecordItem: TTheWordOntRecord;
  EncodedVerse: RawByteString;
begin
  if AModule = nil then
    raise ETheWordOntCodec.Create('Cannot encode a nil module');
  if not AModule.Modified then
    Exit(AModule.FOriginalBytes);
  if not AModule.FCanReencode then
    raise ETheWordOntEncodingError.Create(
      UTF8Encode(AModule.FEncodingIssue));

  Result := '';
  if AModule.FHasBom then
    Result := UTF8_BOM;
  for Index := 0 to AModule.FVerses.Count - 1 do
  begin
    Document := TTheWordVerseDocument(AModule.FVerses[Index]);
    if Document.Modified then
    begin
      if not TryEncodeBytes(EncodeVerse(Document), AModule.FCodePage,
        EncodedVerse) then
        raise ETheWordOntEncodingError.CreateFmt(
          'Edited verse cannot be represented losslessly in code page %d',
          [AModule.FCodePage]);
      Result := Result + EncodedVerse;
    end
    else
      Result := Result + Document.FSourceBytes;
    Result := Result + Document.FLineEndingBytes;
  end;
  for Index := 0 to AModule.FRecords.Count - 1 do
  begin
    RecordItem := TTheWordOntRecord(AModule.FRecords[Index]);
    Result := Result + RecordItem.FSourceBytes + RecordItem.FLineEndingBytes;
  end;
end;

{ TTheWordCapabilityAnalyzer }

class function TTheWordCapabilityAnalyzer.AnalyzeVerse(
  ADocument: TTheWordVerseDocument): TTheWordLayerCapability;
begin
  if ADocument = nil then
    raise ETheWordOntCodec.Create('Cannot analyze a nil verse document');
  if ADocument.OpaqueItemCount > 0 then
    Result := twlcProtectedRegions
  else
    Result := twlcFullWysiwyg;
end;

class function TTheWordCapabilityAnalyzer.AnalyzeModule(
  AModule: TTheWordOntModule): TTheWordLayerCapability;
var
  Index: Integer;
  VerseCapability: TTheWordLayerCapability;
begin
  if AModule = nil then
    raise ETheWordOntCodec.Create('Cannot analyze a nil module');
  if not AModule.CanReencode then
    Exit(twlcRawOnly);
  for Index := 0 to AModule.RecordCount - 1 do
    if AModule.Records[Index].IsActiveRule then
      Exit(twlcRawOnly);

  Result := twlcFullWysiwyg;
  for Index := 0 to AModule.VerseCount - 1 do
  begin
    VerseCapability := AnalyzeVerse(AModule.Verses[Index]);
    if Ord(VerseCapability) > Ord(Result) then
      Result := VerseCapability;
  end;
end;

end.
