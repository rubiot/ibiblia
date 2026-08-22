unit theword_ont_codec_v2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, theword_translator_note_body_v2,
  theword_inline_format_v2;

type
  ETheWordOntCodec = class(Exception);
  ETheWordOntProtectedEdit = class(ETheWordOntCodec);
  ETheWordOntEncodingError = class(ETheWordOntCodec);

  TTheWordSemanticKind = (
    twsmItalic,
    twsmWordsOfJesus,
    twsmOldTestamentQuote,
    twsmUnderline,
    twsmStrikethrough,
    twsmSuperscript,
    twsmSubscript,
    twsmHtmlBold,
    twsmHtmlItalic,
    twsmHtmlUnderline
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

  { Shared with the pure relative-font-size planner.  These records live in
    the document unit so a prepared plan can be reconciled and rendered by
    the document without introducing a codec/planner unit cycle. }
  TTheWordFontSizeActionKindV2 = (
    twfsakRemoveSize,
    twfsakSetSize,
    twfsakAddSize
  );

  TTheWordFontMarkIdentityV2 = record
    OpenOrdinal: Integer;
    StartUTF16: Integer;
    EndUTF16: Integer;
  end;

  TTheWordFontSizeMarkActionV2 = record
    Kind: TTheWordFontSizeActionKindV2;
    Mark: TTheWordFontMarkIdentityV2;
    LeafStartUTF16: Integer;
    LeafLengthUTF16: Integer;
    OriginalHadSize: Boolean;
    OriginalSize: Integer;
    OriginalWasExplicitZero: Boolean;
    OriginalZeroSign: WideChar;
    TargetSize: Integer;
  end;
  TTheWordFontSizeMarkActionArrayV2 = array of TTheWordFontSizeMarkActionV2;

  TTheWordFontSizeLeafPlanV2 = record
    StartUTF16: Integer;
    LengthUTF16: Integer;
    CurrentEffectiveSize: Integer;
    CurrentHadExplicitSize: Boolean;
    CurrentExplicitZeroSign: WideChar;
    TargetEffectiveSize: Integer;
    Carrier: TTheWordFontMarkIdentityV2;
    CarrierIsNew: Boolean;
    Actions: TTheWordFontSizeMarkActionArrayV2;
  end;
  TTheWordFontSizeLeafPlanArrayV2 = array of TTheWordFontSizeLeafPlanV2;

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

  { Font ranges use zero-based UTF-16 code-unit offsets. The parsed opening
    record keeps every source span relative to OpenLexeme. }
  TTheWordFontMark = class
  private
    FStartOffset: Integer;
    FEndOffset: Integer;
    FOpenOrdinal: Integer;
    FCloseOrdinal: Integer;
    FOpenLexeme: UnicodeString;
    FCloseLexeme: UnicodeString;
    FImported: Boolean;
    FOpeningData: TTheWordInlineFormatTagV2;
    FColorRaw: UnicodeString;
  public
    property StartOffset: Integer read FStartOffset;
    property EndOffset: Integer read FEndOffset;
    property OpenOrdinal: Integer read FOpenOrdinal;
    property CloseOrdinal: Integer read FCloseOrdinal;
    property OpenLexeme: UnicodeString read FOpenLexeme;
    property CloseLexeme: UnicodeString read FCloseLexeme;
    property Imported: Boolean read FImported;
    property OpeningData: TTheWordInlineFormatTagV2 read FOpeningData;
    property HasColor: Boolean read FOpeningData.HasColor;
    property ResolvedColor: LongInt read FOpeningData.ResolvedColor;
    property ColorRaw: UnicodeString read FColorRaw;
    property HasSize: Boolean read FOpeningData.HasSize;
    property RelativeSize: Integer read FOpeningData.RelativeSize;
    property SizeSignRaw: WideChar read FOpeningData.SizeSignRaw;
    property HasFontName: Boolean read FOpeningData.HasFontName;
    property FontNameRaw: UnicodeString read FOpeningData.FontNameRaw;
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
    FFontMarks: TObjectList;
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
    function GetFontMark(AIndex: Integer): TTheWordFontMark;
    function GetFontMarkCount: Integer;
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
    function UpdateFontMarkLexemes(AMark: TTheWordFontMark;
      const AOpenLexeme, ACloseLexeme: UnicodeString): Boolean;
    function ToggleSemanticMark(AKind: TTheWordSemanticKind;
      AStartOffset, AEndOffset: Integer): Boolean;
    procedure ReplaceVisibleText(AStartUTF16, ALengthUTF16: Integer;
      const AReplacement: UnicodeString);
    { Builds and validates a replacement raw verse.  This is deliberately
      pure: no event, mark, ownership, ordinal or Modified state is changed. }
    function TryBuildRawWithFontTopology(
      const APlan: TTheWordFontSizeLeafPlanArrayV2;
      out ACandidateRaw: UnicodeString): Boolean;
    function LexicalProtectedStart(AIndex: Integer): Integer;
    property VisibleText: UnicodeString read FVisibleText;
    property OriginalRaw: UnicodeString read FOriginalRaw;
    property MarkCount: Integer read GetMarkCount;
    property Marks[AIndex: Integer]: TTheWordSemanticMark read GetMark;
    property FontMarkCount: Integer read GetFontMarkCount;
    property FontMarks[AIndex: Integer]: TTheWordFontMark
      read GetFontMark;
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
    twiekFontOpen,
    twiekFontClose,
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
    FontMark: TTheWordFontMark;
    TranslatorNote: TTheWordTranslatorNote;
  end;

  TPendingSemantic = class
  public
    Kind: TTheWordSemanticKind;
    StartOffset: Integer;
    OpenLexeme: UnicodeString;
    OpenEvent: TTheWordInlineEvent;
  end;

  TPendingFont = class
  public
    StartOffset: Integer;
    OpenLexeme: UnicodeString;
    OpeningData: TTheWordInlineFormatTagV2;
    OpenEvent: TTheWordInlineEvent;
    IsValid: Boolean;
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
    twsmStrikethrough: Result := '<s>';
    twsmSuperscript: Result := '<sup>';
    twsmSubscript: Result := '<sub>';
    twsmHtmlBold: Result := '<b>';
    twsmHtmlItalic: Result := '<i>';
    twsmHtmlUnderline: Result := '<u>';
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
    twsmStrikethrough: Result := '</s>';
    twsmSuperscript: Result := '</sup>';
    twsmSubscript: Result := '</sub>';
    twsmHtmlBold: Result := '</b>';
    twsmHtmlItalic: Result := '</i>';
    twsmHtmlUnderline: Result := '</u>';
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

procedure MoveInsertedEventBeforeFontOpenAtStart(
  ADocument: TTheWordVerseDocument; AEvent: TTheWordInlineEvent);
var
  EventIndex, FontIndex, MarkIndex: Integer;
  FontEvent: TTheWordInlineEvent;
  FontMark: TTheWordFontMark;
begin
  EventIndex := ADocument.FEvents.IndexOf(AEvent);
  if EventIndex < 0 then
    Exit;
  for FontIndex := 0 to EventIndex - 1 do
  begin
    FontEvent := TTheWordInlineEvent(ADocument.FEvents[FontIndex]);
    if (FontEvent.Kind <> twiekFontOpen) or
       (FontEvent.Offset <> AEvent.Offset) then
      Continue;
    for MarkIndex := 0 to ADocument.FFontMarks.Count - 1 do
    begin
      FontMark := TTheWordFontMark(ADocument.FFontMarks[MarkIndex]);
      if (FontEvent.FontMark = FontMark) and
         (FontMark.FStartOffset = AEvent.Offset) then
      begin
        while EventIndex > FontIndex do
        begin
          ADocument.FEvents.Exchange(EventIndex, EventIndex - 1);
          Dec(EventIndex);
        end;
        Exit;
      end;
    end;
  end;
end;
function KeepEventLeftOfFontBoundaryAtInsertion(
  ADocument: TTheWordVerseDocument; AOrdinal, AOffset: Integer): Boolean;
var
  Index, OpenPivot, ClosePivot: Integer;
  Event: TTheWordInlineEvent;
begin
  OpenPivot := MaxInt;
  ClosePivot := -1;
  for Index := 0 to ADocument.FEvents.Count - 1 do
  begin
    Event := TTheWordInlineEvent(ADocument.FEvents[Index]);
    if Event.Offset <> AOffset then
      Continue;
    if (Event.Kind = twiekFontOpen) and (Event.FontMark <> nil) and
       (Event.Ordinal < OpenPivot) then
      OpenPivot := Event.Ordinal
    else if (Event.Kind = twiekFontClose) and (Event.FontMark <> nil) and
            (Event.Ordinal > ClosePivot) then
      ClosePivot := Event.Ordinal;
  end;
  if OpenPivot <> MaxInt then
    Exit(AOrdinal < OpenPivot);
  Result := (ClosePivot >= 0) and (AOrdinal <= ClosePivot);
end;

procedure MakePendingFontOpaque(ADocument: TTheWordVerseDocument;
  APending: TPendingFont);
var
  Item: TTheWordOpaqueItem;
begin
  if (APending = nil) or not APending.IsValid then
    Exit;
  APending.IsValid := False;
  APending.OpenEvent.Kind := twiekOpaque;
  APending.OpenEvent.FontMark := nil;
  Item := TTheWordOpaqueItem.Create;
  Item.FKind := twokMalformedTag;
  Item.FOffset := APending.StartOffset;
  Item.FRawLexeme := APending.OpenLexeme;
  Item.FOrdinal := APending.OpenEvent.Ordinal;
  ADocument.FOpaqueItems.Add(Item);
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
  else if AsciiSameText(ARaw, '<s>') then
    AKind := twsmStrikethrough
  else if AsciiSameText(ARaw, '<sup>') then
    AKind := twsmSuperscript
  else if AsciiSameText(ARaw, '<sub>') then
    AKind := twsmSubscript
  else if AsciiSameText(ARaw, '<b>') then
    AKind := twsmHtmlBold
  else if AsciiSameText(ARaw, '<i>') then
    AKind := twsmHtmlItalic
  else if AsciiSameText(ARaw, '<u>') then
    AKind := twsmHtmlUnderline
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
  else if AsciiSameText(ARaw, '</s>') then
    AKind := twsmStrikethrough
  else if AsciiSameText(ARaw, '</sup>') then
    AKind := twsmSuperscript
  else if AsciiSameText(ARaw, '</sub>') then
    AKind := twsmSubscript
  else if AsciiSameText(ARaw, '</b>') then
    AKind := twsmHtmlBold
  else if AsciiSameText(ARaw, '</i>') then
    AKind := twsmHtmlItalic
  else if AsciiSameText(ARaw, '</u>') then
    AKind := twsmHtmlUnderline
  else
    Result := False;
end;

function IsLexicalFontOpenTag(const ARaw: UnicodeString): Boolean;
var
  Separator: WideChar;
begin
  Result := Length(ARaw) >= 6;
  if not Result or not AsciiSameText(Copy(ARaw, 1, 5), '<font') then
    Exit(False);
  Separator := ARaw[6];
  Result := (Separator = '>') or (Separator = '/') or
    (Separator = ' ') or (Separator = #9) or (Separator = #10) or
    (Separator = #13);
end;

function IsLexicalFontSelfClosingTag(const ARaw: UnicodeString): Boolean;
var
  Index: Integer;
begin
  if not IsLexicalFontOpenTag(ARaw) then
    Exit(False);
  Index := Length(ARaw) - 1;
  while (Index > 5) and ((ARaw[Index] = ' ') or (ARaw[Index] = #9) or
    (ARaw[Index] = #10) or (ARaw[Index] = #13)) do
    Dec(Index);
  Result := (Index > 5) and (ARaw[Index] = '/');
end;
function IsLexicalFontCloseTag(const ARaw: UnicodeString): Boolean;
begin
  Result := (Length(ARaw) >= 7) and
    AsciiSameText(Copy(ARaw, 1, 6), '</font') and
    ((ARaw[7] = '>') or (ARaw[7] = ' ') or (ARaw[7] = #9) or
     (ARaw[7] = #10) or (ARaw[7] = #13));
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
  FFontMarks := TObjectList.Create(True);
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
  FFontMarks.Free;
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

function TTheWordVerseDocument.GetFontMark(AIndex: Integer):
  TTheWordFontMark;
begin
  Result := TTheWordFontMark(FFontMarks[AIndex]);
end;

function TTheWordVerseDocument.GetFontMarkCount: Integer;
begin
  Result := FFontMarks.Count;
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

function TTheWordVerseDocument.UpdateFontMarkLexemes(
  AMark: TTheWordFontMark; const AOpenLexeme,
  ACloseLexeme: UnicodeString): Boolean;
var
  AttributeIndex, CloseCount, EventIndex, OpenCount: Integer;
  CloseData, OpenData: TTheWordInlineFormatTagV2;
  CloseEvent, Event, OpenEvent: TTheWordInlineEvent;
  ColorRaw, ErrorText: UnicodeString;
  EncodedLexemes: RawByteString;
begin
  Result := False;
  if (AMark = nil) or (FFontMarks.IndexOf(AMark) < 0) or
     not TryParseTheWordInlineFormatTagV2(AOpenLexeme, OpenData,
       ErrorText) or (OpenData.Kind <> twiftv2Font) or OpenData.IsClosing or
     not TryParseTheWordInlineFormatTagV2(ACloseLexeme, CloseData,
       ErrorText) or (CloseData.Kind <> twiftv2Font) or
     not CloseData.IsClosing or
     not TryEncodeBytes(AOpenLexeme + ACloseLexeme, FRequiredCodePage,
       EncodedLexemes) then
    Exit;

  OpenCount := 0;
  CloseCount := 0;
  OpenEvent := nil;
  CloseEvent := nil;
  for EventIndex := 0 to FEvents.Count - 1 do
  begin
    Event := TTheWordInlineEvent(FEvents[EventIndex]);
    if Event.FontMark <> AMark then
      Continue;
    case Event.Kind of
      twiekFontOpen:
        begin
          Inc(OpenCount);
          OpenEvent := Event;
        end;
      twiekFontClose:
        begin
          Inc(CloseCount);
          CloseEvent := Event;
        end;
    else
      Exit;
    end;
  end;
  if (OpenCount <> 1) or (CloseCount <> 1) then
    Exit;
  if (AMark.FStartOffset > AMark.FEndOffset) or
     (AMark.FOpenOrdinal >= AMark.FCloseOrdinal) or
     (OpenEvent.Ordinal <> AMark.FOpenOrdinal) or
     (CloseEvent.Ordinal <> AMark.FCloseOrdinal) or
     (OpenEvent.Offset <> AMark.FStartOffset) or
     (CloseEvent.Offset <> AMark.FEndOffset) then
    Exit;

  if (AOpenLexeme = AMark.FOpenLexeme) and
     (ACloseLexeme = AMark.FCloseLexeme) then
    Exit(True);

  ColorRaw := '';
  for AttributeIndex := 0 to High(OpenData.Attributes) do
    if OpenData.Attributes[AttributeIndex].NameRaw = 'color' then
    begin
      ColorRaw := OpenData.Attributes[AttributeIndex].ValueRaw;
      Break;
    end;
  EnsureEditable;
  AMark.FOpenLexeme := AOpenLexeme;
  AMark.FCloseLexeme := ACloseLexeme;
  AMark.FOpeningData := OpenData;
  AMark.FColorRaw := ColorRaw;
  AMark.FImported := False;
  OpenEvent.RawLexeme := '';
  CloseEvent.RawLexeme := '';
  FModified := True;
  Result := True;
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
  MoveInsertedEventBeforeFontOpenAtStart(Self, Event);
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
  MoveInsertedEventBeforeFontOpenAtStart(Self, Event);
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
  FontMark: TTheWordFontMark;
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

  for Index := 0 to FFontMarks.Count - 1 do
  begin
    FontMark := TTheWordFontMark(FFontMarks[Index]);
    if (ALengthUTF16 = 0) and
       (FontMark.FStartOffset = AStartUTF16) and
       (FontMark.FEndOffset = AStartUTF16) then
    begin
      Inc(FontMark.FStartOffset, ReplacementLength);
      Inc(FontMark.FEndOffset, ReplacementLength);
    end
    else if FontMark.FEndOffset <= AStartUTF16 then
      Continue
    else if FontMark.FStartOffset >= EndOffset then
    begin
      Inc(FontMark.FStartOffset, Delta);
      Inc(FontMark.FEndOffset, Delta);
    end
    else
    begin
      if FontMark.FStartOffset >= AStartUTF16 then
        FontMark.FStartOffset := AStartUTF16;
      if FontMark.FEndOffset <= EndOffset then
        FontMark.FEndOffset := AStartUTF16 + ReplacementLength
      else
        Inc(FontMark.FEndOffset, Delta);
    end;
  end;

  for Index := 0 to FLexicalItems.Count - 1 do
  begin
    Lexical := TTheWordLexicalItem(FLexicalItems[Index]);
    if not ((ALengthUTF16 = 0) and (Lexical.FOffset = AStartUTF16) and
      KeepEventLeftOfFontBoundaryAtInsertion(Self, Lexical.FOrdinal,
        AStartUTF16)) then
      Lexical.FOffset := TransformProtectedAnchor(Lexical.FOffset,
        AStartUTF16, EndOffset, ReplacementLength);
  end;
  for Index := 0 to FOpaqueItems.Count - 1 do
  begin
    Opaque := TTheWordOpaqueItem(FOpaqueItems[Index]);
    if not ((ALengthUTF16 = 0) and
      (KeepTheWordTitleAnchorAtBodyInsertion(Opaque.FOrdinal,
         AStartUTF16, TitleRanges) or
       ((Opaque.FOffset = AStartUTF16) and
        KeepEventLeftOfFontBoundaryAtInsertion(Self, Opaque.FOrdinal,
          AStartUTF16)))) then
      Opaque.FOffset := TransformProtectedAnchor(Opaque.FOffset,
        AStartUTF16, EndOffset, ReplacementLength);
  end;

  for Index := 0 to FTranslatorNotes.Count - 1 do
  begin
    TranslatorNote := TTheWordTranslatorNote(FTranslatorNotes[Index]);
    if not ((ALengthUTF16 = 0) and
      (TranslatorNote.FOffsetUTF16 = AStartUTF16) and
      KeepEventLeftOfFontBoundaryAtInsertion(Self, TranslatorNote.FOrdinal,
        AStartUTF16)) then
      TranslatorNote.FOffsetUTF16 := TransformProtectedAnchor(
        TranslatorNote.FOffsetUTF16, AStartUTF16, EndOffset,
        ReplacementLength);
  end;

  for Index := 0 to FStructuralItems.Count - 1 do
  begin
    Structural := TTheWordStructuralItem(FStructuralItems[Index]);
    if not ((ALengthUTF16 = 0) and
      (Structural.FOffset = AStartUTF16) and
      KeepEventLeftOfFontBoundaryAtInsertion(Self, Structural.FOrdinal,
        AStartUTF16)) then
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
      twiekFontOpen:
        if Event.FontMark <> nil then
          Event.Offset := Event.FontMark.FStartOffset;
      twiekFontClose:
        if Event.FontMark <> nil then
          Event.Offset := Event.FontMark.FEndOffset;
      twiekOpaque:
        if not ((ALengthUTF16 = 0) and
          (KeepTheWordTitleAnchorAtBodyInsertion(Event.Ordinal,
             AStartUTF16, TitleRanges) or
           ((Event.Offset = AStartUTF16) and
            KeepEventLeftOfFontBoundaryAtInsertion(Self, Event.Ordinal,
              AStartUTF16)))) then
          Event.Offset := TransformProtectedAnchor(Event.Offset,
            AStartUTF16, EndOffset, ReplacementLength);
      twiekTranslatorNote:
        if Event.TranslatorNote <> nil then
          Event.Offset := Event.TranslatorNote.FOffsetUTF16;
      twiekLexical, twiekStructural:
        if not ((ALengthUTF16 = 0) and (Event.Offset = AStartUTF16) and
          KeepEventLeftOfFontBoundaryAtInsertion(Self, Event.Ordinal,
            AStartUTF16)) then
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

function PreservedEventLexeme(AEvent: TTheWordInlineEvent): UnicodeString;
begin
  if (AEvent.Kind = twiekFontOpen) and (AEvent.FontMark <> nil) then
    Result := AEvent.FontMark.FOpenLexeme
  else if (AEvent.Kind = twiekFontClose) and (AEvent.FontMark <> nil) then
    Result := AEvent.FontMark.FCloseLexeme
  else
    Result := AEvent.RawLexeme;
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
    if Event.Kind = twiekFontOpen then
      AppendInsertedNotes;
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
             twiekTranslatorNote, twiekFontOpen, twiekFontClose]) and
            ((AEmitted = nil) or (AEmitted.IndexOf(Event) < 0)) then
      AOutput := AOutput + PreservedEventLexeme(Event);
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
          if BetweenEvent.Kind in [twiekFontOpen, twiekFontClose] then
          begin
            HasDifferentSemanticBetween := True;
            Break;
          end;
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
  AttributeIndex, CloseEnd, CloseStart, Index, NoteEnd, TextStart,
    TagEnd, Offset: Integer;
  Body, CloseTag, InlineError, Q, RawNote, RawTag, Value: UnicodeString;
  SemanticKind, StackKind: TTheWordSemanticKind;
  Stacks: array[TTheWordSemanticKind] of TList;
  HasClose, Nested, OpenAttributesSupported: Boolean;
  Pending: TPendingSemantic;
  FontPending: TPendingFont;
  InlineTag: TTheWordInlineFormatTagV2;
  Event: TTheWordInlineEvent;
  Mark: TTheWordSemanticMark;
  FontMark: TTheWordFontMark;
  Note: TTheWordTranslatorNote;
  OpaqueItem: TTheWordOpaqueItem;
  FontStack, Stack: TList;
begin
  Result := TTheWordVerseDocument.Create;
  try
    Result.FOriginalRaw := ARaw;
    for StackKind := Low(TTheWordSemanticKind) to High(TTheWordSemanticKind) do
      Stacks[StackKind] := TList.Create;
    FontStack := TList.Create;
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
        else if TryParseTheWordInlineFormatTagV2(RawTag, InlineTag,
          InlineError) and (InlineTag.Kind = twiftv2Font) then
        begin
          if InlineTag.IsClosing then
          begin
            if FontStack.Count = 0 then
              AddOpaque(Result, Offset, RawTag, twokMalformedTag)
            else
            begin
              FontPending := TPendingFont(FontStack[FontStack.Count - 1]);
              FontStack.Delete(FontStack.Count - 1);
              if FontPending.IsValid then
                for StackKind := Low(TTheWordSemanticKind) to
                  High(TTheWordSemanticKind) do
                  for AttributeIndex := 0 to Stacks[StackKind].Count - 1 do
                    if TPendingSemantic(Stacks[StackKind][AttributeIndex]).
                      OpenEvent.Ordinal > FontPending.OpenEvent.Ordinal then
                      MakePendingFontOpaque(Result, FontPending);
              if not FontPending.IsValid then
              begin
                AddOpaque(Result, Offset, RawTag, twokMalformedTag);
                FontPending.Free;
              end
              else
              begin
                FontMark := TTheWordFontMark.Create;
                FontMark.FStartOffset := FontPending.StartOffset;
                FontMark.FEndOffset := Offset;
                FontMark.FOpenLexeme := FontPending.OpenLexeme;
                FontMark.FCloseLexeme := RawTag;
                FontMark.FImported := True;
                FontMark.FOpeningData := FontPending.OpeningData;
                FontMark.FOpenOrdinal := FontPending.OpenEvent.Ordinal;
                for AttributeIndex := 0 to
                  High(FontMark.FOpeningData.Attributes) do
                  if FontMark.FOpeningData.Attributes[AttributeIndex].NameRaw =
                     'color' then
                    FontMark.FColorRaw :=
                      FontMark.FOpeningData.Attributes[AttributeIndex].ValueRaw;
                Result.FFontMarks.Add(FontMark);
                FontPending.OpenEvent.FontMark := FontMark;
                Event := NewInlineEvent(Result, twiekFontClose, Offset, RawTag);
                Event.FontMark := FontMark;
                FontMark.FCloseOrdinal := Event.Ordinal;
                FontPending.OpenEvent.RawLexeme := '';
                Event.RawLexeme := '';
                FontPending.Free;
              end;
            end;
          end
          else
          begin
            Event := NewInlineEvent(Result, twiekFontOpen, Offset, RawTag);
            FontPending := TPendingFont.Create;
            FontPending.StartOffset := Offset;
            FontPending.OpenLexeme := RawTag;
            FontPending.OpeningData := InlineTag;
            FontPending.OpenEvent := Event;
            FontPending.IsValid := True;
            FontStack.Add(FontPending);
          end;
        end
        else if IsLexicalFontSelfClosingTag(RawTag) then
          AddOpaque(Result, Offset, RawTag, twokMalformedTag)
        else if IsLexicalFontOpenTag(RawTag) then
        begin
          AddOpaque(Result, Offset, RawTag, twokMalformedTag);
          FontPending := TPendingFont.Create;
          FontPending.StartOffset := Offset;
          FontPending.OpenLexeme := RawTag;
          FontPending.IsValid := False;
          FontStack.Add(FontPending);
        end
        else if IsLexicalFontCloseTag(RawTag) then
        begin
          AddOpaque(Result, Offset, RawTag, twokMalformedTag);
          if (FontStack.Count > 0) and
             not TPendingFont(FontStack[FontStack.Count - 1]).IsValid then
          begin
            FontPending := TPendingFont(FontStack[FontStack.Count - 1]);
            FontStack.Delete(FontStack.Count - 1);
            FontPending.Free;
          end;
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
            for AttributeIndex := 0 to FontStack.Count - 1 do
            begin
              FontPending := TPendingFont(FontStack[AttributeIndex]);
              if FontPending.IsValid and
                 (FontPending.OpenEvent.Ordinal > Pending.OpenEvent.Ordinal) then
                MakePendingFontOpaque(Result, FontPending);
            end;
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
      while FontStack.Count > 0 do
      begin
        FontPending := TPendingFont(FontStack[FontStack.Count - 1]);
        FontStack.Delete(FontStack.Count - 1);
        if FontPending.IsValid then
        begin
          FontPending.OpenEvent.Kind := twiekOpaque;
          FontPending.OpenEvent.FontMark := nil;
          OpaqueItem := TTheWordOpaqueItem.Create;
          OpaqueItem.FKind := twokMalformedTag;
          OpaqueItem.FOffset := FontPending.StartOffset;
          OpaqueItem.FRawLexeme := FontPending.OpenLexeme;
          OpaqueItem.FOrdinal := FontPending.OpenEvent.Ordinal;
          Result.FOpaqueItems.Add(OpaqueItem);
        end;
        FontPending.Free;
      end;
    finally
      for StackKind := Low(TTheWordSemanticKind) to High(TTheWordSemanticKind) do
      begin
        for Index := 0 to Stacks[StackKind].Count - 1 do
          TObject(Stacks[StackKind][Index]).Free;
        Stacks[StackKind].Free;
      end;
      for Index := 0 to FontStack.Count - 1 do
        TObject(FontStack[Index]).Free;
      FontStack.Free;
    end;
    MergeSyntheticReopenedMarks(Result);
    Result.FModified := False;
  except
    Result.Free;
    raise;
  end;
end;

type
  TFontRecoverySpec = class
  public
    OpenOrdinal: Integer;
    OpenLexeme: UnicodeString;
    CloseLexeme: UnicodeString;
  end;

function RecoveryCanonicalSize(AValue: Integer): UnicodeString;
begin
  if AValue < 0 then
    Result := IntToStr(AValue)
  else
    Result := '+' + IntToStr(AValue);
end;

function RecoveryRewriteFontOpen(const AOpenLexeme: UnicodeString;
  AHasSize: Boolean; ASize: Integer; out ARewritten: UnicodeString;
  out AWrapperNeeded: Boolean): Boolean;
var
  Attribute: TTheWordInlineFormatAttributeV2;
  ErrorText, SizeText: UnicodeString;
  Index, InsertAt, SizeIndex: Integer;
  Parsed, Reparsed: TTheWordInlineFormatTagV2;
begin
  Result := False;
  ARewritten := '';
  AWrapperNeeded := False;
  if not TryParseTheWordInlineFormatTagV2(AOpenLexeme, Parsed, ErrorText) or
     (Parsed.Kind <> twiftv2Font) or Parsed.IsClosing then
    Exit;
  SizeIndex := -1;
  for Index := 0 to High(Parsed.Attributes) do
    if Parsed.Attributes[Index].NameRaw = 'size' then
    begin
      SizeIndex := Index;
      Break;
    end;
  if not AHasSize then
  begin
    if SizeIndex < 0 then
      ARewritten := AOpenLexeme
    else if Length(Parsed.Attributes) = 1 then
    begin
      Result := True;
      Exit;
    end
    else
    begin
      Attribute := Parsed.Attributes[SizeIndex];
      ARewritten := Copy(AOpenLexeme, 1,
        Attribute.FullSpanUTF16.StartUTF16 - 1) +
        Copy(AOpenLexeme, Attribute.FullSpanUTF16.StartUTF16 +
          Attribute.FullSpanUTF16.LengthUTF16, MaxInt);
    end;
  end
  else
  begin
    if (ASize < -4) or (ASize > 10) or (ASize = 0) then
      Exit;
    SizeText := RecoveryCanonicalSize(ASize);
    if SizeIndex >= 0 then
    begin
      Attribute := Parsed.Attributes[SizeIndex];
      ARewritten := Copy(AOpenLexeme, 1,
        Attribute.PayloadValueSpanUTF16.StartUTF16 - 1) + SizeText +
        Copy(AOpenLexeme, Attribute.PayloadValueSpanUTF16.StartUTF16 +
          Attribute.PayloadValueSpanUTF16.LengthUTF16, MaxInt);
    end
    else
    begin
      InsertAt := Parsed.ClosingTriviaSpanUTF16.StartUTF16;
      if InsertAt = 0 then
        InsertAt := Length(AOpenLexeme);
      ARewritten := Copy(AOpenLexeme, 1, InsertAt - 1) +
        ' size=' + SizeText + Copy(AOpenLexeme, InsertAt, MaxInt);
    end;
  end;
  if ARewritten = '' then
  begin
    Result := True;
    Exit;
  end;
  if not TryParseTheWordInlineFormatTagV2(ARewritten, Reparsed,
    ErrorText) or (Reparsed.Kind <> twiftv2Font) or Reparsed.IsClosing then
    Exit;
  AWrapperNeeded := True;
  Result := True;
end;

function RecoveryFindFont(ADocument: TTheWordVerseDocument;
  const AIdentity: TTheWordFontMarkIdentityV2): TTheWordFontMark;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to ADocument.FFontMarks.Count - 1 do
    if (TTheWordFontMark(ADocument.FFontMarks[Index]).FOpenOrdinal =
        AIdentity.OpenOrdinal) and
       (TTheWordFontMark(ADocument.FFontMarks[Index]).FStartOffset =
        AIdentity.StartUTF16) and
       (TTheWordFontMark(ADocument.FFontMarks[Index]).FEndOffset =
        AIdentity.EndUTF16) then
      Exit(TTheWordFontMark(ADocument.FFontMarks[Index]));
end;

function RecoveryFindLeaf(const APlan: TTheWordFontSizeLeafPlanArrayV2;
  AOffset: Integer): Integer;
var
  Index: Integer;
begin
  for Index := 0 to High(APlan) do
    if (APlan[Index].StartUTF16 <= AOffset) and
       (AOffset < APlan[Index].StartUTF16 + APlan[Index].LengthUTF16) then
      Exit(Index);
  Result := -1;
end;

function RecoveryFindAction(const ALeaf: TTheWordFontSizeLeafPlanV2;
  AMark: TTheWordFontMark): Integer;
var
  Index: Integer;
begin
  for Index := 0 to High(ALeaf.Actions) do
    if (AMark <> nil) and
       (ALeaf.Actions[Index].Mark.OpenOrdinal = AMark.FOpenOrdinal) and
       (ALeaf.Actions[Index].Mark.StartUTF16 = AMark.FStartOffset) and
       (ALeaf.Actions[Index].Mark.EndUTF16 = AMark.FEndOffset) then
      Exit(Index);
  Result := -1;
end;

procedure RecoveryClearSpecs(AList: TList);
var
  Index: Integer;
begin
  for Index := 0 to AList.Count - 1 do
    TObject(AList[Index]).Free;
  AList.Clear;
end;

procedure RecoveryCopySpecs(ASource, ADestination: TList);
var
  Index: Integer;
  Source, CopySpec: TFontRecoverySpec;
begin
  RecoveryClearSpecs(ADestination);
  for Index := 0 to ASource.Count - 1 do
  begin
    Source := TFontRecoverySpec(ASource[Index]);
    CopySpec := TFontRecoverySpec.Create;
    CopySpec.OpenOrdinal := Source.OpenOrdinal;
    CopySpec.OpenLexeme := Source.OpenLexeme;
    CopySpec.CloseLexeme := Source.CloseLexeme;
    ADestination.Add(CopySpec);
  end;
end;

procedure RecoveryInsertFontSpecSorted(AList: TList;
  ASpec: TFontRecoverySpec);
var
  Index: Integer;
begin
  Index := 0;
  while (Index < AList.Count) and
        (TFontRecoverySpec(AList[Index]).OpenOrdinal <=
         ASpec.OpenOrdinal) do
    Inc(Index);
  AList.Insert(Index, ASpec);
end;

function RecoverySameSpecs(ALeft, ARight: TList): Boolean;
var
  Index: Integer;
begin
  if ALeft.Count <> ARight.Count then
    Exit(False);
  for Index := 0 to ALeft.Count - 1 do
    if (TFontRecoverySpec(ALeft[Index]).OpenOrdinal <>
        TFontRecoverySpec(ARight[Index]).OpenOrdinal) or
       (TFontRecoverySpec(ALeft[Index]).OpenLexeme <>
        TFontRecoverySpec(ARight[Index]).OpenLexeme) or
       (TFontRecoverySpec(ALeft[Index]).CloseLexeme <>
        TFontRecoverySpec(ARight[Index]).CloseLexeme) then
      Exit(False);
  Result := True;
end;

function RecoverySameMarks(ALeft, ARight: TList): Boolean;
var
  Index: Integer;
begin
  if ALeft.Count <> ARight.Count then
    Exit(False);
  for Index := 0 to ALeft.Count - 1 do
    if ALeft[Index] <> ARight[Index] then
      Exit(False);
  Result := True;
end;

procedure RecoveryBuildSemanticTarget(ADocument: TTheWordVerseDocument;
  AOffset: Integer; ATarget: TList);
var
  Index: Integer;
  Mark: TTheWordSemanticMark;
begin
  ATarget.Clear;
  if AOffset >= Length(ADocument.FVisibleText) then
    Exit;
  for Index := 0 to ADocument.FMarks.Count - 1 do
  begin
    Mark := TTheWordSemanticMark(ADocument.FMarks[Index]);
    if (Mark.FStartOffset <= AOffset) and (AOffset < Mark.FEndOffset) then
      InsertSemanticMarkSorted(ATarget, Mark);
  end;
end;

function RecoveryBuildFontTarget(ADocument: TTheWordVerseDocument;
  const APlan: TTheWordFontSizeLeafPlanArrayV2; AOffset: Integer;
  ATarget: TList): Boolean;
var
  ActionIndex, Index, LeafIndex: Integer;
  Action: TTheWordFontSizeMarkActionV2;
  HasSize, WrapperNeeded: Boolean;
  Mark: TTheWordFontMark;
  OpenLexeme: UnicodeString;
  Spec: TFontRecoverySpec;
begin
  Result := False;
  RecoveryClearSpecs(ATarget);
  if AOffset >= Length(ADocument.FVisibleText) then
    Exit(True);
  LeafIndex := RecoveryFindLeaf(APlan, AOffset);
  for Index := 0 to ADocument.FFontMarks.Count - 1 do
  begin
    Mark := TTheWordFontMark(ADocument.FFontMarks[Index]);
    if not ((Mark.FStartOffset <= AOffset) and
      (AOffset < Mark.FEndOffset)) then
      Continue;
    HasSize := Mark.HasSize;
    OpenLexeme := Mark.FOpenLexeme;
    if LeafIndex >= 0 then
    begin
      ActionIndex := RecoveryFindAction(APlan[LeafIndex], Mark);
      if ActionIndex >= 0 then
      begin
        Action := APlan[LeafIndex].Actions[ActionIndex];
        case Action.Kind of
          twfsakRemoveSize: HasSize := False;
          twfsakSetSize, twfsakAddSize: HasSize := True;
        end;
        if not RecoveryRewriteFontOpen(Mark.FOpenLexeme, HasSize,
          Action.TargetSize, OpenLexeme, WrapperNeeded) then
          Exit;
        if not WrapperNeeded then
          Continue;
      end;
    end;
    Spec := TFontRecoverySpec.Create;
    Spec.OpenOrdinal := Mark.FOpenOrdinal;
    Spec.OpenLexeme := OpenLexeme;
    Spec.CloseLexeme := Mark.FCloseLexeme;
    RecoveryInsertFontSpecSorted(ATarget, Spec);
  end;
  if LeafIndex >= 0 then
    for ActionIndex := 0 to High(APlan[LeafIndex].Actions) do
    begin
      Action := APlan[LeafIndex].Actions[ActionIndex];
      if Action.Mark.OpenOrdinal <> -1 then
        Continue;
      if (Action.Kind <> twfsakAddSize) or (Action.TargetSize = 0) then
        Exit;
      Spec := TFontRecoverySpec.Create;
      { A leaf-scoped carrier is the innermost target wrapper. }
      Spec.OpenOrdinal := MaxInt;
      Spec.OpenLexeme := '<font size=' +
        RecoveryCanonicalSize(Action.TargetSize) + '>';
      Spec.CloseLexeme := '</font>';
      RecoveryInsertFontSpecSorted(ATarget, Spec);
    end;
  Result := True;
end;

function RecoveryPlanIsReconciled(ADocument: TTheWordVerseDocument;
  const APlan: TTheWordFontSizeLeafPlanArrayV2): Boolean;
var
  ActionIndex, Index, PreviousEnd: Integer;
  Action: TTheWordFontSizeMarkActionV2;
  Mark: TTheWordFontMark;
begin
  Result := False;
  if Length(APlan) = 0 then
    Exit;
  PreviousEnd := -1;
  for Index := 0 to High(APlan) do
  begin
    if (APlan[Index].LengthUTF16 <= 0) or
       (APlan[Index].StartUTF16 < 0) or
       (APlan[Index].StartUTF16 < PreviousEnd) or
       (APlan[Index].StartUTF16 + APlan[Index].LengthUTF16 >
        Length(ADocument.FVisibleText)) or
       not ADocument.CanApplySemanticRange(APlan[Index].StartUTF16,
         APlan[Index].StartUTF16 + APlan[Index].LengthUTF16) then
      Exit;
    PreviousEnd := APlan[Index].StartUTF16 + APlan[Index].LengthUTF16;
    for ActionIndex := 0 to High(APlan[Index].Actions) do
    begin
      Action := APlan[Index].Actions[ActionIndex];
      if (Action.LeafStartUTF16 <> APlan[Index].StartUTF16) or
         (Action.LeafLengthUTF16 <> APlan[Index].LengthUTF16) then
        Exit;
      if Action.Mark.OpenOrdinal = -1 then
      begin
        if (Action.Kind <> twfsakAddSize) or
           (Action.TargetSize = 0) or (Action.TargetSize < -4) or
           (Action.TargetSize > 10) then
          Exit;
        Continue;
      end;
      Mark := RecoveryFindFont(ADocument, Action.Mark);
      if Mark = nil then
        Exit;
      if (Mark.HasSize <> Action.OriginalHadSize) or
         (Mark.HasSize and (Mark.RelativeSize <> Action.OriginalSize)) then
        Exit;
      if Action.Kind in [twfsakSetSize, twfsakAddSize] then
        if (Action.TargetSize = 0) or (Action.TargetSize < -4) or
           (Action.TargetSize > 10) then
          Exit;
    end;
  end;
  Result := True;
end;

function RecoveryIsZeroWrapperEvent(AEvent: TTheWordInlineEvent): Boolean;
begin
  case AEvent.Kind of
    twiekSemanticOpen, twiekSemanticClose:
      Result := (AEvent.SemanticMark <> nil) and
        (AEvent.SemanticMark.FStartOffset = AEvent.SemanticMark.FEndOffset);
    twiekFontOpen, twiekFontClose:
      Result := (AEvent.FontMark <> nil) and
        (AEvent.FontMark.FStartOffset = AEvent.FontMark.FEndOffset);
  else
    Result := False;
  end;
end;

function RecoveryIsActiveWrapperEvent(AEvent: TTheWordInlineEvent): Boolean;
begin
  case AEvent.Kind of
    twiekSemanticOpen, twiekSemanticClose:
      Result := (AEvent.SemanticMark <> nil) and
        (AEvent.SemanticMark.FStartOffset <> AEvent.SemanticMark.FEndOffset);
    twiekFontOpen, twiekFontClose:
      Result := (AEvent.FontMark <> nil) and
        (AEvent.FontMark.FStartOffset <> AEvent.FontMark.FEndOffset);
  else
    Result := False;
  end;
end;

function RecoveryIsAnchorPayload(AEvent: TTheWordInlineEvent): Boolean;
begin
  Result := (AEvent.Kind in [twiekPendingSemantic, twiekLexical,
    twiekOpaque, twiekStructural, twiekTranslatorNote]) or
    RecoveryIsZeroWrapperEvent(AEvent);
end;

function RecoveryWrapperPhase(ADocument: TTheWordVerseDocument;
  AOffset, AOrdinal: Integer; out AHasBoundary: Boolean): Integer;
var
  EventIndex, FirstOrdinal, LastOrdinal: Integer;
  Event: TTheWordInlineEvent;
begin
  FirstOrdinal := MaxInt;
  LastOrdinal := -1;
  for EventIndex := 0 to ADocument.FEvents.Count - 1 do
  begin
    Event := TTheWordInlineEvent(ADocument.FEvents[EventIndex]);
    if (Event.Offset = AOffset) and RecoveryIsActiveWrapperEvent(Event) then
    begin
      if Event.Ordinal < FirstOrdinal then FirstOrdinal := Event.Ordinal;
      if Event.Ordinal > LastOrdinal then LastOrdinal := Event.Ordinal;
    end;
  end;
  AHasBoundary := LastOrdinal >= 0;
  if not AHasBoundary or (AOrdinal < FirstOrdinal) then Exit(0);
  if AOrdinal > LastOrdinal then Exit(2);
  Result := 1;
end;

function RecoveryPayloadPhase(ADocument: TTheWordVerseDocument;
  AEvent: TTheWordInlineEvent): Integer;
var
  HasBoundary: Boolean;
begin
  { WG/WH/WT/wt are postfix to the word immediately on their left. }
  if AEvent.Kind = twiekLexical then Exit(0);
  Result := RecoveryWrapperPhase(ADocument, AEvent.Offset,
    AEvent.Ordinal, HasBoundary);
end;

procedure RecoveryAppendAnchorPayload(ADocument: TTheWordVerseDocument;
  AOffset, APhase: Integer; var AOutput: UnicodeString);
var
  EventIndex: Integer;
  Event: TTheWordInlineEvent;
begin
  for EventIndex := 0 to ADocument.FEvents.Count - 1 do
  begin
    Event := TTheWordInlineEvent(ADocument.FEvents[EventIndex]);
    if (Event.Offset = AOffset) and RecoveryIsAnchorPayload(Event) and
       (RecoveryPayloadPhase(ADocument, Event) = APhase) then
      AOutput := AOutput + PreservedEventLexeme(Event);
  end;
end;

function RecoveryActiveSemanticCount(ADocument: TTheWordVerseDocument;
  AOffset: Integer; AKind: TTheWordSemanticKind): Integer;
var
  Index: Integer;
  Mark: TTheWordSemanticMark;
begin
  Result := 0;
  for Index := 0 to ADocument.FMarks.Count - 1 do
  begin
    Mark := TTheWordSemanticMark(ADocument.FMarks[Index]);
    if (Mark.FKind = AKind) and (Mark.FStartOffset <= AOffset) and
       (AOffset < Mark.FEndOffset) then Inc(Result);
  end;
end;

function RecoveryZeroSemanticCount(ADocument: TTheWordVerseDocument;
  AOffset: Integer; AKind: TTheWordSemanticKind): Integer;
var
  Index: Integer;
  Mark: TTheWordSemanticMark;
begin
  Result := 0;
  for Index := 0 to ADocument.FMarks.Count - 1 do
  begin
    Mark := TTheWordSemanticMark(ADocument.FMarks[Index]);
    if (Mark.FKind = AKind) and (Mark.FStartOffset = AOffset) and
       (Mark.FEndOffset = AOffset) then Inc(Result);
  end;
end;

function RecoverySemanticCoverageEqual(AExpected,
  AActual: TTheWordVerseDocument): Boolean;
var
  Kind: TTheWordSemanticKind;
  Offset: Integer;
begin
  if Length(AExpected.FVisibleText) > 0 then
    for Offset := 0 to Length(AExpected.FVisibleText) - 1 do
      for Kind := Low(TTheWordSemanticKind) to High(TTheWordSemanticKind) do
        if RecoveryActiveSemanticCount(AExpected, Offset, Kind) <>
           RecoveryActiveSemanticCount(AActual, Offset, Kind) then
          Exit(False);
  for Offset := 0 to Length(AExpected.FVisibleText) do
    for Kind := Low(TTheWordSemanticKind) to High(TTheWordSemanticKind) do
      if RecoveryZeroSemanticCount(AExpected, Offset, Kind) <>
         RecoveryZeroSemanticCount(AActual, Offset, Kind) then
        Exit(False);
  Result := True;
end;

function RecoveryPayloadSequenceEqual(AExpected,
  AActual: TTheWordVerseDocument): Boolean;
var
  ActualIndex, ExpectedIndex: Integer;
  ActualEvent, ExpectedEvent: TTheWordInlineEvent;
begin
  ActualIndex := 0;
  ExpectedIndex := 0;
  while True do
  begin
    while (ExpectedIndex < AExpected.FEvents.Count) and
          not RecoveryIsAnchorPayload(TTheWordInlineEvent(
            AExpected.FEvents[ExpectedIndex])) do Inc(ExpectedIndex);
    while (ActualIndex < AActual.FEvents.Count) and
          not RecoveryIsAnchorPayload(TTheWordInlineEvent(
            AActual.FEvents[ActualIndex])) do Inc(ActualIndex);
    if (ExpectedIndex >= AExpected.FEvents.Count) or
       (ActualIndex >= AActual.FEvents.Count) then
      Exit((ExpectedIndex >= AExpected.FEvents.Count) and
        (ActualIndex >= AActual.FEvents.Count));
    ExpectedEvent := TTheWordInlineEvent(AExpected.FEvents[ExpectedIndex]);
    ActualEvent := TTheWordInlineEvent(AActual.FEvents[ActualIndex]);
    if (ExpectedEvent.Kind <> ActualEvent.Kind) or
       (ExpectedEvent.Offset <> ActualEvent.Offset) or
       (PreservedEventLexeme(ExpectedEvent) <>
        PreservedEventLexeme(ActualEvent)) then Exit(False);
    Inc(ExpectedIndex);
    Inc(ActualIndex);
  end;
end;

function RecoveryAffinityEqual(AExpected,
  AActual: TTheWordVerseDocument): Boolean;
var
  ActualPhase, ExpectedPhase, EventIndex, Index: Integer;
  HasActualBoundary, HasExpectedBoundary: Boolean;
  Event: TTheWordInlineEvent;
begin
  for Index := 0 to AActual.FLexicalItems.Count - 1 do
    for EventIndex := 0 to AActual.FEvents.Count - 1 do
    begin
      Event := TTheWordInlineEvent(AActual.FEvents[EventIndex]);
      if (Event.Offset = TTheWordLexicalItem(
           AActual.FLexicalItems[Index]).FOffset) and
         (Event.Kind in [twiekSemanticOpen, twiekSemanticClose,
          twiekFontOpen, twiekFontClose]) and
         (Event.Ordinal < TTheWordLexicalItem(
           AActual.FLexicalItems[Index]).FOrdinal) then Exit(False);
    end;
  for Index := 0 to AExpected.FOpaqueItems.Count - 1 do
  begin
    ExpectedPhase := RecoveryWrapperPhase(AExpected,
      TTheWordOpaqueItem(AExpected.FOpaqueItems[Index]).FOffset,
      TTheWordOpaqueItem(AExpected.FOpaqueItems[Index]).FOrdinal,
      HasExpectedBoundary);
    ActualPhase := RecoveryWrapperPhase(AActual,
      TTheWordOpaqueItem(AActual.FOpaqueItems[Index]).FOffset,
      TTheWordOpaqueItem(AActual.FOpaqueItems[Index]).FOrdinal,
      HasActualBoundary);
    if HasExpectedBoundary and
       (not HasActualBoundary or (ExpectedPhase <> ActualPhase)) then
      Exit(False);
  end;
  for Index := 0 to AExpected.FStructuralItems.Count - 1 do
  begin
    ExpectedPhase := RecoveryWrapperPhase(AExpected,
      TTheWordStructuralItem(AExpected.FStructuralItems[Index]).FOffset,
      TTheWordStructuralItem(AExpected.FStructuralItems[Index]).FOrdinal,
      HasExpectedBoundary);
    ActualPhase := RecoveryWrapperPhase(AActual,
      TTheWordStructuralItem(AActual.FStructuralItems[Index]).FOffset,
      TTheWordStructuralItem(AActual.FStructuralItems[Index]).FOrdinal,
      HasActualBoundary);
    if HasExpectedBoundary and
       (not HasActualBoundary or (ExpectedPhase <> ActualPhase)) then
      Exit(False);
  end;
  for Index := 0 to AExpected.FTranslatorNotes.Count - 1 do
  begin
    ExpectedPhase := RecoveryWrapperPhase(AExpected,
      TTheWordTranslatorNote(AExpected.FTranslatorNotes[Index]).FOffsetUTF16,
      TTheWordTranslatorNote(AExpected.FTranslatorNotes[Index]).FOrdinal,
      HasExpectedBoundary);
    ActualPhase := RecoveryWrapperPhase(AActual,
      TTheWordTranslatorNote(AActual.FTranslatorNotes[Index]).FOffsetUTF16,
      TTheWordTranslatorNote(AActual.FTranslatorNotes[Index]).FOrdinal,
      HasActualBoundary);
    if HasExpectedBoundary and
       (not HasActualBoundary or (ExpectedPhase <> ActualPhase)) then
      Exit(False);
  end;
  Result := True;
end;

function RecoveryObjectsEqual(AExpected, AActual: TTheWordVerseDocument):
  Boolean;
var
  Index: Integer;
begin
  Result := False;
  if (AExpected.FVisibleText <> AActual.FVisibleText) or
     (AExpected.FLexicalItems.Count <> AActual.FLexicalItems.Count) or
     (AExpected.FOpaqueItems.Count <> AActual.FOpaqueItems.Count) or
     (AExpected.FStructuralItems.Count <> AActual.FStructuralItems.Count) or
     (AExpected.FTranslatorNotes.Count <> AActual.FTranslatorNotes.Count) or
     not RecoverySemanticCoverageEqual(AExpected, AActual) or
     not RecoveryPayloadSequenceEqual(AExpected, AActual) then
    Exit;
  for Index := 0 to AExpected.FLexicalItems.Count - 1 do
    if (TTheWordLexicalItem(AExpected.FLexicalItems[Index]).FKind <>
        TTheWordLexicalItem(AActual.FLexicalItems[Index]).FKind) or
       (TTheWordLexicalItem(AExpected.FLexicalItems[Index]).FOffset <>
        TTheWordLexicalItem(AActual.FLexicalItems[Index]).FOffset) or
       (TTheWordLexicalItem(AExpected.FLexicalItems[Index]).FRawLexeme <>
        TTheWordLexicalItem(AActual.FLexicalItems[Index]).FRawLexeme) or
       (TTheWordLexicalItem(AExpected.FLexicalItems[Index]).FValue <>
        TTheWordLexicalItem(AActual.FLexicalItems[Index]).FValue) then Exit;
  for Index := 0 to AExpected.FOpaqueItems.Count - 1 do
    if (TTheWordOpaqueItem(AExpected.FOpaqueItems[Index]).FKind <>
        TTheWordOpaqueItem(AActual.FOpaqueItems[Index]).FKind) or
       (TTheWordOpaqueItem(AExpected.FOpaqueItems[Index]).FOffset <>
        TTheWordOpaqueItem(AActual.FOpaqueItems[Index]).FOffset) or
       (TTheWordOpaqueItem(AExpected.FOpaqueItems[Index]).FRawLexeme <>
        TTheWordOpaqueItem(AActual.FOpaqueItems[Index]).FRawLexeme) then Exit;
  for Index := 0 to AExpected.FStructuralItems.Count - 1 do
    if (TTheWordStructuralItem(AExpected.FStructuralItems[Index]).FKind <>
        TTheWordStructuralItem(AActual.FStructuralItems[Index]).FKind) or
       (TTheWordStructuralItem(AExpected.FStructuralItems[Index]).FOffset <>
        TTheWordStructuralItem(AActual.FStructuralItems[Index]).FOffset) or
       (TTheWordStructuralItem(AExpected.FStructuralItems[Index]).FRawLexeme <>
        TTheWordStructuralItem(AActual.FStructuralItems[Index]).FRawLexeme) then Exit;
  for Index := 0 to AExpected.FTranslatorNotes.Count - 1 do
    if (TTheWordTranslatorNote(AExpected.FTranslatorNotes[Index]).FOffsetUTF16 <>
        TTheWordTranslatorNote(AActual.FTranslatorNotes[Index]).FOffsetUTF16) or
       (TTheWordTranslatorNote(AExpected.FTranslatorNotes[Index]).FQ <>
        TTheWordTranslatorNote(AActual.FTranslatorNotes[Index]).FQ) or
       (TTheWordTranslatorNote(AExpected.FTranslatorNotes[Index]).FBodyRaw <>
        TTheWordTranslatorNote(AActual.FTranslatorNotes[Index]).FBodyRaw) or
       (TTheWordTranslatorNote(AExpected.FTranslatorNotes[Index]).FOpenLexeme <>
        TTheWordTranslatorNote(AActual.FTranslatorNotes[Index]).FOpenLexeme) or
       (TTheWordTranslatorNote(AExpected.FTranslatorNotes[Index]).FCloseLexeme <>
        TTheWordTranslatorNote(AActual.FTranslatorNotes[Index]).FCloseLexeme) then Exit;
  Result := True;
end;

function RecoveryEffectiveSize(ADocument: TTheWordVerseDocument;
  AOffset: Integer; out ASize: Integer): Boolean;
var
  BestOrdinal, Index: Integer;
  Mark: TTheWordFontMark;
begin
  ASize := 0;
  Result := False;
  BestOrdinal := -1;
  for Index := 0 to ADocument.FFontMarks.Count - 1 do
  begin
    Mark := TTheWordFontMark(ADocument.FFontMarks[Index]);
    if (Mark.FStartOffset <= AOffset) and (AOffset < Mark.FEndOffset) and
       Mark.HasSize and (Mark.FOpenOrdinal > BestOrdinal) then
    begin
      ASize := Mark.RelativeSize;
      Result := True;
      BestOrdinal := Mark.FOpenOrdinal;
    end;
  end;
end;

type
  TRecoveryCombinedWrapperKind = (
    rcwkSemantic,
    rcwkFont
  );

  TRecoveryCombinedWrapper = class
  public
    Kind: TRecoveryCombinedWrapperKind;
    SemanticMark: TTheWordSemanticMark;
    FontMark: TTheWordFontMark;
    StartOffset: Integer;
    EndOffset: Integer;
    OpenOrdinal: Integer;
    OpenLexeme: UnicodeString;
    CloseLexeme: UnicodeString;
  end;

procedure RecoveryClearCombinedWrappers(AList: TList);
var
  Index: Integer;
begin
  for Index := 0 to AList.Count - 1 do
    TObject(AList[Index]).Free;
  AList.Clear;
end;

function RecoveryCopyCombinedWrapper(
  ASource: TRecoveryCombinedWrapper): TRecoveryCombinedWrapper;
begin
  Result := TRecoveryCombinedWrapper.Create;
  Result.Kind := ASource.Kind;
  Result.SemanticMark := ASource.SemanticMark;
  Result.FontMark := ASource.FontMark;
  Result.StartOffset := ASource.StartOffset;
  Result.EndOffset := ASource.EndOffset;
  Result.OpenOrdinal := ASource.OpenOrdinal;
  Result.OpenLexeme := ASource.OpenLexeme;
  Result.CloseLexeme := ASource.CloseLexeme;
end;

procedure RecoveryCopyCombinedWrappers(ASource, ADestination: TList);
var
  Index: Integer;
begin
  RecoveryClearCombinedWrappers(ADestination);
  for Index := 0 to ASource.Count - 1 do
    ADestination.Add(RecoveryCopyCombinedWrapper(
      TRecoveryCombinedWrapper(ASource[Index])));
end;

function RecoveryCompareCombinedWrappers(ALeft,
  ARight: TRecoveryCombinedWrapper): Integer;
begin
  if ALeft.OpenOrdinal < ARight.OpenOrdinal then
    Exit(-1);
  if ALeft.OpenOrdinal > ARight.OpenOrdinal then
    Exit(1);
  if Ord(ALeft.Kind) < Ord(ARight.Kind) then
    Exit(-1);
  if Ord(ALeft.Kind) > Ord(ARight.Kind) then
    Exit(1);
  if ALeft.StartOffset < ARight.StartOffset then
    Exit(-1);
  if ALeft.StartOffset > ARight.StartOffset then
    Exit(1);
  if ALeft.EndOffset > ARight.EndOffset then
    Exit(-1);
  if ALeft.EndOffset < ARight.EndOffset then
    Exit(1);
  if ALeft.OpenLexeme < ARight.OpenLexeme then
    Exit(-1);
  if ALeft.OpenLexeme > ARight.OpenLexeme then
    Exit(1);
  Result := 0;
end;

procedure RecoveryInsertCombinedWrapper(AList: TList;
  AWrapper: TRecoveryCombinedWrapper);
var
  Index: Integer;
begin
  Index := 0;
  while (Index < AList.Count) and
        (RecoveryCompareCombinedWrappers(
          TRecoveryCombinedWrapper(AList[Index]), AWrapper) <= 0) do
    Inc(Index);
  AList.Insert(Index, AWrapper);
end;

function RecoveryCombinedIdentityEqual(ALeft,
  ARight: TRecoveryCombinedWrapper): Boolean;
begin
  Result := (ALeft.Kind = ARight.Kind) and
    (ALeft.SemanticMark = ARight.SemanticMark) and
    (ALeft.FontMark = ARight.FontMark) and
    (ALeft.StartOffset = ARight.StartOffset) and
    (ALeft.EndOffset = ARight.EndOffset) and
    (ALeft.OpenLexeme = ARight.OpenLexeme) and
    (ALeft.CloseLexeme = ARight.CloseLexeme);
end;

function RecoveryFindCombinedSemantic(AList: TList;
  AMark: TTheWordSemanticMark): TRecoveryCombinedWrapper;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to AList.Count - 1 do
    if (TRecoveryCombinedWrapper(AList[Index]).Kind = rcwkSemantic) and
       (TRecoveryCombinedWrapper(AList[Index]).SemanticMark = AMark) then
      Exit(TRecoveryCombinedWrapper(AList[Index]));
end;

function RecoveryFindCombinedFont(AList: TList;
  AMark: TTheWordFontMark): TRecoveryCombinedWrapper;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to AList.Count - 1 do
    if (TRecoveryCombinedWrapper(AList[Index]).Kind = rcwkFont) and
       (TRecoveryCombinedWrapper(AList[Index]).FontMark = AMark) then
      Exit(TRecoveryCombinedWrapper(AList[Index]));
end;

function RecoveryBuildCombinedTarget(ADocument: TTheWordVerseDocument;
  const APlan: TTheWordFontSizeLeafPlanArrayV2; AOffset: Integer;
  ATarget: TList): Boolean;
var
  Action: TTheWordFontSizeMarkActionV2;
  ActionIndex, Index, LeafIndex: Integer;
  FontMark: TTheWordFontMark;
  HasSize, WrapperNeeded: Boolean;
  OpenLexeme: UnicodeString;
  SemanticMark: TTheWordSemanticMark;
  Wrapper: TRecoveryCombinedWrapper;
begin
  Result := False;
  RecoveryClearCombinedWrappers(ATarget);
  if AOffset >= Length(ADocument.FVisibleText) then
    Exit(True);
  LeafIndex := RecoveryFindLeaf(APlan, AOffset);

  for Index := 0 to ADocument.FMarks.Count - 1 do
  begin
    SemanticMark := TTheWordSemanticMark(ADocument.FMarks[Index]);
    if not ((SemanticMark.FStartOffset <= AOffset) and
      (AOffset < SemanticMark.FEndOffset)) then
      Continue;
    Wrapper := TRecoveryCombinedWrapper.Create;
    Wrapper.Kind := rcwkSemantic;
    Wrapper.SemanticMark := SemanticMark;
    Wrapper.FontMark := nil;
    Wrapper.StartOffset := SemanticMark.FStartOffset;
    Wrapper.EndOffset := SemanticMark.FEndOffset;
    Wrapper.OpenOrdinal := SemanticMark.FOpenOrdinal;
    Wrapper.OpenLexeme := SemanticMark.FOpenLexeme;
    Wrapper.CloseLexeme := SemanticMark.FCloseLexeme;
    RecoveryInsertCombinedWrapper(ATarget, Wrapper);
  end;

  for Index := 0 to ADocument.FFontMarks.Count - 1 do
  begin
    FontMark := TTheWordFontMark(ADocument.FFontMarks[Index]);
    if not ((FontMark.FStartOffset <= AOffset) and
      (AOffset < FontMark.FEndOffset)) then
      Continue;
    HasSize := FontMark.HasSize;
    OpenLexeme := FontMark.FOpenLexeme;
    if LeafIndex >= 0 then
    begin
      ActionIndex := RecoveryFindAction(APlan[LeafIndex], FontMark);
      if ActionIndex >= 0 then
      begin
        Action := APlan[LeafIndex].Actions[ActionIndex];
        case Action.Kind of
          twfsakRemoveSize: HasSize := False;
          twfsakSetSize, twfsakAddSize: HasSize := True;
        end;
        if not RecoveryRewriteFontOpen(FontMark.FOpenLexeme, HasSize,
          Action.TargetSize, OpenLexeme, WrapperNeeded) then
          Exit;
        if not WrapperNeeded then
          Continue;
      end;
    end;
    Wrapper := TRecoveryCombinedWrapper.Create;
    Wrapper.Kind := rcwkFont;
    Wrapper.SemanticMark := nil;
    Wrapper.FontMark := FontMark;
    Wrapper.StartOffset := FontMark.FStartOffset;
    Wrapper.EndOffset := FontMark.FEndOffset;
    Wrapper.OpenOrdinal := FontMark.FOpenOrdinal;
    Wrapper.OpenLexeme := OpenLexeme;
    Wrapper.CloseLexeme := FontMark.FCloseLexeme;
    RecoveryInsertCombinedWrapper(ATarget, Wrapper);
  end;

  if LeafIndex >= 0 then
    for ActionIndex := 0 to High(APlan[LeafIndex].Actions) do
    begin
      Action := APlan[LeafIndex].Actions[ActionIndex];
      if Action.Mark.OpenOrdinal <> -1 then
        Continue;
      if (Action.Kind <> twfsakAddSize) or
         (Action.TargetSize = 0) then
        Exit;
      Wrapper := TRecoveryCombinedWrapper.Create;
      Wrapper.Kind := rcwkFont;
      Wrapper.SemanticMark := nil;
      Wrapper.FontMark := nil;
      Wrapper.StartOffset := APlan[LeafIndex].StartUTF16;
      Wrapper.EndOffset := APlan[LeafIndex].StartUTF16 +
        APlan[LeafIndex].LengthUTF16;
      Wrapper.OpenOrdinal := MaxInt;
      Wrapper.OpenLexeme := '<font size=' +
        RecoveryCanonicalSize(Action.TargetSize) + '>';
      Wrapper.CloseLexeme := '</font>';
      RecoveryInsertCombinedWrapper(ATarget, Wrapper);
    end;
  Result := True;
end;

function RecoverySemanticContainsEvent(AMark: TTheWordSemanticMark;
  AEvent: TTheWordInlineEvent): Boolean;
begin
  Result := False;
  if (AMark.FStartOffset = AMark.FEndOffset) or
     (AEvent.Offset < AMark.FStartOffset) or
     (AEvent.Offset > AMark.FEndOffset) then
    Exit;
  if (AMark.FStartOffset < AEvent.Offset) and
     (AEvent.Offset < AMark.FEndOffset) then
    Exit(True);
  if AEvent.Offset = AMark.FStartOffset then
    Exit(AEvent.Ordinal > AMark.FOpenOrdinal);
  if AEvent.Offset = AMark.FEndOffset then
    Exit(AEvent.Ordinal < AMark.FCloseOrdinal);
end;

function RecoveryFontContainsEvent(AMark: TTheWordFontMark;
  AEvent: TTheWordInlineEvent): Boolean;
begin
  Result := False;
  if (AMark.FStartOffset = AMark.FEndOffset) or
     (AEvent.Offset < AMark.FStartOffset) or
     (AEvent.Offset > AMark.FEndOffset) then
    Exit;
  if (AMark.FStartOffset < AEvent.Offset) and
     (AEvent.Offset < AMark.FEndOffset) then
    Exit(True);
  if AEvent.Offset = AMark.FStartOffset then
    Exit(AEvent.Ordinal > AMark.FOpenOrdinal);
  if AEvent.Offset = AMark.FEndOffset then
    Exit(AEvent.Ordinal < AMark.FCloseOrdinal);
end;

procedure RecoveryBuildPayloadStack(ADocument: TTheWordVerseDocument;
  AEvent: TTheWordInlineEvent; ALeft, ARight, ATarget: TList);
var
  FontMark: TTheWordFontMark;
  Index: Integer;
  SemanticMark: TTheWordSemanticMark;
  Source: TRecoveryCombinedWrapper;
begin
  RecoveryClearCombinedWrappers(ATarget);
  for Index := 0 to ADocument.FMarks.Count - 1 do
  begin
    SemanticMark := TTheWordSemanticMark(ADocument.FMarks[Index]);
    if not RecoverySemanticContainsEvent(SemanticMark, AEvent) then
      Continue;
    Source := RecoveryFindCombinedSemantic(ALeft, SemanticMark);
    if Source = nil then
      Source := RecoveryFindCombinedSemantic(ARight, SemanticMark);
    if Source <> nil then
      RecoveryInsertCombinedWrapper(ATarget,
        RecoveryCopyCombinedWrapper(Source));
  end;
  for Index := 0 to ADocument.FFontMarks.Count - 1 do
  begin
    FontMark := TTheWordFontMark(ADocument.FFontMarks[Index]);
    if not RecoveryFontContainsEvent(FontMark, AEvent) then
      Continue;
    Source := RecoveryFindCombinedFont(ALeft, FontMark);
    if Source = nil then
      Source := RecoveryFindCombinedFont(ARight, FontMark);
    if Source <> nil then
      RecoveryInsertCombinedWrapper(ATarget,
        RecoveryCopyCombinedWrapper(Source));
  end;
end;

procedure RecoveryTransitionCombined(ACurrent, ATarget: TList;
  var AOutput: UnicodeString);
var
  CommonCount, Index: Integer;
begin
  CommonCount := 0;
  while (CommonCount < ACurrent.Count) and
        (CommonCount < ATarget.Count) and
        RecoveryCombinedIdentityEqual(
          TRecoveryCombinedWrapper(ACurrent[CommonCount]),
          TRecoveryCombinedWrapper(ATarget[CommonCount])) do
    Inc(CommonCount);
  for Index := ACurrent.Count - 1 downto CommonCount do
  begin
    AOutput := AOutput +
      TRecoveryCombinedWrapper(ACurrent[Index]).CloseLexeme;
    TObject(ACurrent[Index]).Free;
    ACurrent.Delete(Index);
  end;
  for Index := CommonCount to ATarget.Count - 1 do
  begin
    AOutput := AOutput +
      TRecoveryCombinedWrapper(ATarget[Index]).OpenLexeme;
    ACurrent.Add(RecoveryCopyCombinedWrapper(
      TRecoveryCombinedWrapper(ATarget[Index])));
  end;
end;

procedure RecoveryCollectAnchorPayload(ADocument: TTheWordVerseDocument;
  AOffset: Integer; ATarget: TList);
var
  Event: TTheWordInlineEvent;
  EventIndex, InsertAt: Integer;
begin
  ATarget.Clear;
  for EventIndex := 0 to ADocument.FEvents.Count - 1 do
  begin
    Event := TTheWordInlineEvent(ADocument.FEvents[EventIndex]);
    if (Event.Offset <> AOffset) or not RecoveryIsAnchorPayload(Event) then
      Continue;
    InsertAt := 0;
    while (InsertAt < ATarget.Count) and
          (TTheWordInlineEvent(ATarget[InsertAt]).Ordinal <=
           Event.Ordinal) do
      Inc(InsertAt);
    ATarget.Insert(InsertAt, Event);
  end;
end;

function RecoveryWrapperPresentationEqual(ALeft,
  ARight: TRecoveryCombinedWrapper): Boolean;
begin
  Result := (ALeft.Kind = ARight.Kind) and
    (ALeft.OpenLexeme = ARight.OpenLexeme) and
    (ALeft.CloseLexeme = ARight.CloseLexeme);
  if Result and (ALeft.Kind = rcwkSemantic) then
    Result := (ALeft.SemanticMark <> nil) and
      (ARight.SemanticMark <> nil) and
      (ALeft.SemanticMark.FKind = ARight.SemanticMark.FKind);
end;

function RecoveryCombinedPresentationEqual(ALeft, ARight: TList;
  AFontsOnly: Boolean): Boolean;
var
  LeftIndex, RightIndex: Integer;
begin
  LeftIndex := 0;
  RightIndex := 0;
  while True do
  begin
    if AFontsOnly then
    begin
      while (LeftIndex < ALeft.Count) and
            (TRecoveryCombinedWrapper(ALeft[LeftIndex]).Kind <>
             rcwkFont) do Inc(LeftIndex);
      while (RightIndex < ARight.Count) and
            (TRecoveryCombinedWrapper(ARight[RightIndex]).Kind <>
             rcwkFont) do Inc(RightIndex);
    end;
    if (LeftIndex >= ALeft.Count) or (RightIndex >= ARight.Count) then
      Exit((LeftIndex >= ALeft.Count) and
        (RightIndex >= ARight.Count));
    if not RecoveryWrapperPresentationEqual(
      TRecoveryCombinedWrapper(ALeft[LeftIndex]),
      TRecoveryCombinedWrapper(ARight[RightIndex])) then
      Exit(False);
    Inc(LeftIndex);
    Inc(RightIndex);
  end;
end;

function RecoveryBuildEventPresentationStack(
  ADocument: TTheWordVerseDocument;
  const APlan: TTheWordFontSizeLeafPlanArrayV2;
  AEvent: TTheWordInlineEvent; ATarget: TList): Boolean;
var
  LeftStack, RightStack: TList;
begin
  Result := False;
  LeftStack := TList.Create;
  RightStack := TList.Create;
  try
    if (AEvent.Offset > 0) and
       not RecoveryBuildCombinedTarget(ADocument, APlan,
         AEvent.Offset - 1, LeftStack) then
      Exit;
    if not RecoveryBuildCombinedTarget(ADocument, APlan,
      AEvent.Offset, RightStack) then
      Exit;
    if AEvent.Kind = twiekLexical then
      RecoveryCopyCombinedWrappers(LeftStack, ATarget)
    else
      RecoveryBuildPayloadStack(ADocument, AEvent,
        LeftStack, RightStack, ATarget);
    Result := True;
  finally
    RecoveryClearCombinedWrappers(RightStack);
    RecoveryClearCombinedWrappers(LeftStack);
    RightStack.Free;
    LeftStack.Free;
  end;
end;

function RecoveryFontProjectionEqual(AExpected,
  AActual: TTheWordVerseDocument;
  const APlan: TTheWordFontSizeLeafPlanArrayV2): Boolean;
var
  ActualStack, ExpectedStack: TList;
  Offset: Integer;
begin
  Result := False;
  if AExpected.FVisibleText <> AActual.FVisibleText then
    Exit;
  ActualStack := TList.Create;
  ExpectedStack := TList.Create;
  try
    for Offset := 0 to Length(AExpected.FVisibleText) - 1 do
    begin
      if not RecoveryBuildCombinedTarget(AExpected, APlan,
        Offset, ExpectedStack) or
         not RecoveryBuildCombinedTarget(AActual, nil,
           Offset, ActualStack) or
         not RecoveryCombinedPresentationEqual(ExpectedStack,
           ActualStack, True) then
        Exit;
    end;
    Result := True;
  finally
    RecoveryClearCombinedWrappers(ExpectedStack);
    RecoveryClearCombinedWrappers(ActualStack);
    ExpectedStack.Free;
    ActualStack.Free;
  end;
end;

function RecoveryNextPayloadEvent(ADocument: TTheWordVerseDocument;
  var AIndex: Integer): TTheWordInlineEvent;
begin
  Result := nil;
  while AIndex < ADocument.FEvents.Count do
  begin
    Result := TTheWordInlineEvent(ADocument.FEvents[AIndex]);
    Inc(AIndex);
    if RecoveryIsAnchorPayload(Result) then
      Exit;
  end;
  Result := nil;
end;

function RecoveryPayloadAffinityEqualPlan(AExpected,
  AActual: TTheWordVerseDocument;
  const APlan: TTheWordFontSizeLeafPlanArrayV2): Boolean;
var
  ActualEvent, ExpectedEvent: TTheWordInlineEvent;
  ActualIndex, ExpectedIndex: Integer;
  ActualStack, ExpectedStack: TList;
begin
  Result := False;
  ActualIndex := 0;
  ExpectedIndex := 0;
  ActualStack := TList.Create;
  ExpectedStack := TList.Create;
  try
    while True do
    begin
      ExpectedEvent := RecoveryNextPayloadEvent(
        AExpected, ExpectedIndex);
      ActualEvent := RecoveryNextPayloadEvent(
        AActual, ActualIndex);
      if (ExpectedEvent = nil) or (ActualEvent = nil) then
        Exit((ExpectedEvent = nil) and (ActualEvent = nil));
      if (ExpectedEvent.Kind <> ActualEvent.Kind) or
         (ExpectedEvent.Offset <> ActualEvent.Offset) or
         (PreservedEventLexeme(ExpectedEvent) <>
          PreservedEventLexeme(ActualEvent)) then
        Exit;
      if not RecoveryBuildEventPresentationStack(AExpected,
        APlan, ExpectedEvent, ExpectedStack) or
         not RecoveryBuildEventPresentationStack(AActual,
           nil, ActualEvent, ActualStack) or
         not RecoveryCombinedPresentationEqual(ExpectedStack,
           ActualStack, False) then
        Exit;
    end;
  finally
    RecoveryClearCombinedWrappers(ExpectedStack);
    RecoveryClearCombinedWrappers(ActualStack);
    ExpectedStack.Free;
    ActualStack.Free;
  end;
end;

procedure RecoveryAppendCombinedAnchor(
  ADocument: TTheWordVerseDocument; AOffset: Integer;
  ALeft, ARight, ACurrent, APayloads, APayloadStack: TList;
  var AOutput: UnicodeString);
var
  Event: TTheWordInlineEvent;
  Index: Integer;
begin
  RecoveryCollectAnchorPayload(ADocument, AOffset, APayloads);

  { Lexical records are postfix metadata of the visible word on the left.
    They must be emitted before any synthetic close/reopen at this anchor. }
  for Index := 0 to APayloads.Count - 1 do
  begin
    Event := TTheWordInlineEvent(APayloads[Index]);
    if Event.Kind = twiekLexical then
      AOutput := AOutput + PreservedEventLexeme(Event);
  end;

  for Index := 0 to APayloads.Count - 1 do
  begin
    Event := TTheWordInlineEvent(APayloads[Index]);
    if Event.Kind = twiekLexical then
      Continue;
    RecoveryBuildPayloadStack(ADocument, Event,
      ALeft, ARight, APayloadStack);
    RecoveryTransitionCombined(ACurrent,
      APayloadStack, AOutput);
    AOutput := AOutput + PreservedEventLexeme(Event);
  end;
  RecoveryTransitionCombined(ACurrent, ARight, AOutput);
end;

function RecoveryPlanTargetsMatch(ADocument: TTheWordVerseDocument;
  const APlan: TTheWordFontSizeLeafPlanArrayV2): Boolean;
var
  ExpectedExplicit, RestoredExplicit: Boolean;
  ExpectedSize, Index, RestoredSize: Integer;
begin
  for Index := 0 to High(APlan) do
  begin
    ExpectedExplicit := APlan[Index].TargetEffectiveSize <> 0;
    ExpectedSize := APlan[Index].TargetEffectiveSize;
    RestoredExplicit := RecoveryEffectiveSize(ADocument,
      APlan[Index].StartUTF16, RestoredSize);
    if (RestoredExplicit <> ExpectedExplicit) or
       (ExpectedExplicit and
        (RestoredSize <> ExpectedSize)) then
      Exit(False);
  end;
  Result := True;
end;


function TTheWordVerseDocument.TryBuildRawWithFontTopology(
  const APlan: TTheWordFontSizeLeafPlanArrayV2;
  out ACandidateRaw: UnicodeString): Boolean;
var
  Codec: ITheWordOntCodecV2;
  CurrentStack, LeftStack, Payloads, PayloadStack, TargetStack: TList;
  Offset: Integer;
  Restored: TTheWordVerseDocument;
begin
  Result := False;
  ACandidateRaw := '';
  if not RecoveryPlanIsReconciled(Self, APlan) then
    Exit;
  CurrentStack := TList.Create;
  LeftStack := TList.Create;
  Payloads := TList.Create;
  PayloadStack := TList.Create;
  TargetStack := TList.Create;
  try
    for Offset := 0 to Length(FVisibleText) do
    begin
      if not RecoveryBuildCombinedTarget(Self, APlan,
        Offset, TargetStack) then
      begin
        ACandidateRaw := '';
        Exit;
      end;
      RecoveryCopyCombinedWrappers(CurrentStack, LeftStack);
      RecoveryAppendCombinedAnchor(Self, Offset,
        LeftStack, TargetStack, CurrentStack,
        Payloads, PayloadStack, ACandidateRaw);
      if Offset < Length(FVisibleText) then
        ACandidateRaw := ACandidateRaw + FVisibleText[Offset + 1];
    end;
  finally
    RecoveryClearCombinedWrappers(TargetStack);
    RecoveryClearCombinedWrappers(PayloadStack);
    RecoveryClearCombinedWrappers(LeftStack);
    RecoveryClearCombinedWrappers(CurrentStack);
    TargetStack.Free;
    PayloadStack.Free;
    Payloads.Free;
    LeftStack.Free;
    CurrentStack.Free;
  end;

  Codec := TTheWordOntCodecV2.Create;
  try
    Restored := Codec.DecodeVerse(ACandidateRaw);
    try
      if not RecoveryObjectsEqual(Self, Restored) or
         not RecoveryFontProjectionEqual(Self, Restored, APlan) or
         not RecoveryPayloadAffinityEqualPlan(Self, Restored, APlan) or
         not RecoveryPlanTargetsMatch(Restored, APlan) then
      begin
        ACandidateRaw := '';
        Exit;
      end;
    finally
      Restored.Free;
    end;
  except
    ACandidateRaw := '';
    Exit;
  end;
  Result := True;
end;

function TTheWordOntCodecV2.EncodeVerse(
  ADocument: TTheWordVerseDocument): UnicodeString;
var
  Offset, Index, CommonCount: Integer;
  ForceFontBoundary, HasSemanticTransition: Boolean;
  CurrentMarks, TargetMarks, RegeneratedMarks, EmittedEvents: TList;
  Mark: TTheWordSemanticMark;
  Event: TTheWordInlineEvent;
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

      ForceFontBoundary := False;
      for Index := 0 to ADocument.FEvents.Count - 1 do
      begin
        Event := TTheWordInlineEvent(ADocument.FEvents[Index]);
        if (Event.Offset = Offset) and
           (Event.Kind in [twiekFontOpen, twiekFontClose]) then
        begin
          ForceFontBoundary := True;
          Break;
        end;
      end;
      if ForceFontBoundary then
        CommonCount := 0;

      HasSemanticTransition := ForceFontBoundary or
        (CommonCount < CurrentMarks.Count) or
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
