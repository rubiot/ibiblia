unit theword_wysiwyg_session_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, Contnrs, SysUtils, text_edit_provenance,
  theword_ont_codec_v2, theword_break_v2;

type
  ETheWordWysiwygSession = class(Exception);

  TTheWordWysiwygSessionSnapshot = class
  private
    FRaw: UnicodeString;
    FTrace: TVerseEditTrace;
  public
    constructor Create(const ARaw: UnicodeString; ATrace: TVerseEditTrace);
    destructor Destroy; override;
    property Raw: UnicodeString read FRaw;
    property Trace: TVerseEditTrace read FTrace;
  end;

  { The codec document is the single source of truth for both the visual and
    raw surfaces. Snapshots pair the ONT string with its origin map, so undo
    restores unknown/lexical objects and edit provenance atomically. }
  TTheWordWysiwygSessionV2 = class
  private
    FCodec: ITheWordOntCodecV2;
    FDocument: TTheWordVerseDocument;
    FRaw: UnicodeString;
    FOriginalRaw: UnicodeString;
    FUndo: TFPObjectList;
    FRedo: TFPObjectList;
    FTrace: TVerseEditTrace;
    FRevision: QWord;
    function CaptureSnapshot: TTheWordWysiwygSessionSnapshot;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCapability: TTheWordLayerCapability;
    function GetDirty: Boolean;
    function GetRawText: UnicodeString;
    function GetChangesAreSemanticOnly: Boolean;
    function GetVisibleText: UnicodeString;
    procedure PushUndo(ASnapshot: TTheWordWysiwygSessionSnapshot);
    procedure AdoptDocument(const ARaw: UnicodeString;
      var ADocument: TTheWordVerseDocument);
    procedure ReplaceDocument(const ARaw: UnicodeString);
    procedure RestoreFromStack(ASource, ADestination: TFPObjectList);
    function TryResolveEditableTitleScope(AStartUTF16,
      ALengthUTF16: Integer; out AHasTitle: Boolean;
      out ATitle: TTheWordTitleRange): Boolean;
    function TryPrepareTitleFreeDocument(AStartUTF16,
      ALengthUTF16: Integer; out ADocument: TTheWordVerseDocument;
      out AHasTitle: Boolean; out ATitle: TTheWordTitleRange): Boolean;
    function RestoreTitleInCandidate(const ATitle: TTheWordTitleRange;
      const ATitleFreeRaw: UnicodeString;
      out ACandidateRaw: UnicodeString): Boolean; overload;
    function RestoreTitleInCandidate(const ATitle: TTheWordTitleRange;
      const ATitleFreeRaw: UnicodeString;
      out ACandidateRaw: UnicodeString;
      out AOpenOrdinal: Integer): Boolean; overload;
    function ReplaceSelectionWithBreakInternal(AStartUTF16,
      ALengthUTF16: Integer; const ABreaks: TTheWordBreakIdentityArray;
      AKind: TTheWordBreakKind; AAfterAnchors,
      AHasInsertBefore: Boolean; const AInsertBefore:
      TTheWordBreakIdentity): Boolean;
  public
    constructor Create(const ARaw: UnicodeString;
      ATokenizerVersion: Integer = 0);
    destructor Destroy; override;
    function ApplySemantic(AKind: TTheWordSemanticKind; AStartUTF16,
      ALengthUTF16: Integer): Boolean;
    function ApplyFontColor(AStartUTF16, ALengthUTF16: Integer;
      AHasColor: Boolean; AColor: LongInt): Boolean;
    function ReplaceTextWithPendingFormatting(AStartUTF16,
      ALengthUTF16: Integer; const AReplacement: UnicodeString;
      AAfterAnchors, AApplyFontSize: Boolean;
      ACurrentLevel, ATargetLevel: Integer;
      AApplyFontColor, ATargetHasColor: Boolean;
      ATargetColor: LongInt): Boolean;
    function ReplaceTitleTextWithPendingFormatting(AOpenOrdinal,
      ARelativeStartUTF16, ALengthUTF16: Integer;
      const AReplacement: UnicodeString;
      AApplyFontSize: Boolean; ACurrentLevel, ATargetLevel: Integer;
      AApplyFontColor, ATargetHasColor: Boolean;
      ATargetColor: LongInt;
      out AResultOpenOrdinal: Integer): Boolean;
    function AdjustRelativeFontSize(AStartUTF16, ALengthUTF16,
      ADelta: Integer): Boolean;
    function ReplaceTextWithRelativeFontSize(AStartUTF16,
      ALengthUTF16: Integer; const AReplacement: UnicodeString;
      ACurrentLevel, ATargetLevel: Integer): Boolean;
    function CanApplySemantic(AStartUTF16, ALengthUTF16: Integer): Boolean;
    function CanInsertTitle(AOffsetUTF16: Integer): Boolean;
    function CanWrapTitle(AStartUTF16, ALengthUTF16: Integer): Boolean;
    function TryGetTitle(AOpenOrdinal: Integer;
      out ATitle: TTheWordTitleRange): Boolean;
    function ReplaceTitleText(AOpenOrdinal, ARelativeStartUTF16,
      ALengthUTF16: Integer; const AReplacement: UnicodeString): Boolean;
    function InsertTitle(AOffsetUTF16, ALevel: Integer;
      const AText: UnicodeString): Boolean;
    function WrapTitle(AStartUTF16, ALengthUTF16, ALevel: Integer;
      out AOpenOrdinal: Integer): Boolean;
    function UnwrapTitle(AOpenOrdinal: Integer): Boolean;
    function ChangeTitleLevel(AOpenOrdinal, ALevel: Integer): Boolean;
    function RemoveTitle(AOpenOrdinal: Integer): Boolean;
    function CanInsertTranslatorNote(AOffsetUTF16: Integer): Boolean;
    function InsertTranslatorNote(AOffsetUTF16: Integer;
      const AQ, ABodyRaw: UnicodeString; out ANoteId: QWord): Boolean;
    function UpdateTranslatorNote(ANoteId: QWord;
      const AQ, ABodyRaw: UnicodeString): Boolean;
    function RemoveTranslatorNote(ANoteId: QWord): Boolean;
    function TryGetTranslatorNote(ANoteId: QWord;
      out ANote: TTheWordTranslatorNote): Boolean;
    function CanReplaceText(AStartUTF16, ALengthUTF16: Integer): Boolean;
    function CopySelection(AStartUTF16, ALengthUTF16: Integer;
      out APlainText: UnicodeString; out APayload: string): Boolean; overload;
    function CopySelection(AStartUTF16, ALengthUTF16: Integer;
      const ABreaks: TTheWordBreakIdentityArray;
      out APlainText: UnicodeString; out APayload: string): Boolean; overload;
    function CutSelection(AStartUTF16, ALengthUTF16: Integer;
      out APlainText: UnicodeString; out APayload: string): Boolean; overload;
    function CutSelection(AStartUTF16, ALengthUTF16: Integer;
      const ABreaks: TTheWordBreakIdentityArray;
      out APlainText: UnicodeString; out APayload: string): Boolean; overload;
    function DeleteSelectionWithParagraphMarks(AStartUTF16,
      ALengthUTF16: Integer; const AParagraphMarkOffsets:
      array of Integer): Boolean;
    function DeleteSelectionWithBreaks(AStartUTF16,
      ALengthUTF16: Integer;
      const ABreaks: TTheWordBreakIdentityArray): Boolean;
    function ReplaceSelectionWithBreak(AStartUTF16,
      ALengthUTF16: Integer; const ABreaks: TTheWordBreakIdentityArray;
      AKind: TTheWordBreakKind; AAfterAnchors: Boolean): Boolean; overload;
    function ReplaceSelectionWithBreak(AStartUTF16,
      ALengthUTF16: Integer; const ABreaks: TTheWordBreakIdentityArray;
      AKind: TTheWordBreakKind; const AInsertBefore:
      TTheWordBreakIdentity): Boolean; overload;
    function RemoveBreak(const AIdentity: TTheWordBreakIdentity): Boolean;
    function InsertParagraphMark(AStartUTF16,
      ALengthUTF16: Integer): Boolean;
    function Paste(AStartUTF16, ALengthUTF16: Integer;
      const APayload: string; const APlainText: UnicodeString;
      out AInsertedLengthUTF16: Integer;
      out AUsedPrivatePayload: Boolean): Boolean; overload;
    function PasteTitle(AOpenOrdinal, ARelativeStartUTF16,
      ALengthUTF16: Integer; const APayload: string;
      const APlainText: UnicodeString; out AInsertedLengthUTF16: Integer;
      out AUsedPrivatePayload: Boolean): Boolean;
    function Paste(AStartUTF16, ALengthUTF16: Integer;
      const ABreaks: TTheWordBreakIdentityArray;
      AAfterAnchors: Boolean; const APayload: string;
      const APlainText: UnicodeString;
      out AInsertedLengthUTF16: Integer;
      out AUsedPrivatePayload: Boolean): Boolean; overload;
    function Paste(AStartUTF16, ALengthUTF16: Integer;
      const ABreaks: TTheWordBreakIdentityArray;
      AAfterAnchors, AHasInsertBefore: Boolean;
      const AInsertBefore: TTheWordBreakIdentity; const APayload: string;
      const APlainText: UnicodeString;
      out AInsertedLengthUTF16: Integer;
      out AUsedPrivatePayload: Boolean): Boolean; overload;
    function ReplaceText(AStartUTF16, ALengthUTF16: Integer;
      const AReplacement: UnicodeString): Boolean;
    function RemoveParagraphMark(AOffsetUTF16: Integer;
      AFromEnd: Boolean): Boolean;
    function ReplaceRawText(const ARaw: UnicodeString): Boolean;
    function CloneEditTrace: TVerseEditTrace;
    function ProtectedLexicalObjectsUnchanged: Boolean;
    function Undo: Boolean;
    function Redo: Boolean;
    function RejectLastChange: Boolean;
    procedure Cancel;
    procedure AcceptChanges;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property Capability: TTheWordLayerCapability read GetCapability;
    property ChangesAreSemanticOnly: Boolean
      read GetChangesAreSemanticOnly;
    property Dirty: Boolean read GetDirty;
    property Document: TTheWordVerseDocument read FDocument;
    property RawText: UnicodeString read GetRawText;
    property Revision: QWord read FRevision;
    property VisibleText: UnicodeString read GetVisibleText;
  end;

function CanCommitLegacyWysiwygChangeV2(AHasAssociations,
  AChangesAreSemanticOnly: Boolean): Boolean;

implementation

uses
  theword_verse_visual_adapter_v2, theword_clipboard_v2,
  theword_font_size_mutation_v2, theword_font_color_mutation_v2,
  performance_debug;

type
  TSessionOntCodec = class(TInterfacedObject, ITheWordOntCodecV2)
  private
    FInner: ITheWordOntCodecV2;
  public
    constructor Create;
    function DecodeVerse(const ARaw: UnicodeString): TTheWordVerseDocument;
    function EncodeVerse(ADocument: TTheWordVerseDocument): UnicodeString;
    function DecodeModule(const ABytes: RawByteString;
      AVerseCount: Integer): TTheWordOntModule;
    function EncodeModule(AModule: TTheWordOntModule): RawByteString;
  end;

constructor TSessionOntCodec.Create;
begin
  inherited Create;
  FInner := TTheWordOntCodecV2.Create;
end;

function TSessionOntCodec.DecodeVerse(
  const ARaw: UnicodeString): TTheWordVerseDocument;
begin
  IBibliaPerfIncrement(ipcWysiwygDecodes);
  Result := FInner.DecodeVerse(ARaw);
end;

function TSessionOntCodec.EncodeVerse(
  ADocument: TTheWordVerseDocument): UnicodeString;
begin
  IBibliaPerfIncrement(ipcWysiwygEncodes);
  Result := FInner.EncodeVerse(ADocument);
end;

function TSessionOntCodec.DecodeModule(const ABytes: RawByteString;
  AVerseCount: Integer): TTheWordOntModule;
begin
  Result := FInner.DecodeModule(ABytes, AVerseCount);
end;

function TSessionOntCodec.EncodeModule(
  AModule: TTheWordOntModule): RawByteString;
begin
  Result := FInner.EncodeModule(AModule);
end;

function TouchesTheWordTitle(ADocument: TTheWordVerseDocument;
  AStartUTF16, ALengthUTF16: Integer): Boolean;
var
  Finish, Index: Integer;
  Titles: TTheWordTitleRangeArray;
begin
  Result := False;
  if not Assigned(ADocument) then
    Exit;
  Finish := AStartUTF16 + ALengthUTF16;
  CollectTheWordTitleRanges(ADocument, Titles);
  for Index := 0 to High(Titles) do
    if (AStartUTF16 < Titles[Index].EndOffset) and
       (Finish > Titles[Index].StartOffset) then
      Exit(True);
end;

constructor TTheWordWysiwygSessionSnapshot.Create(
  const ARaw: UnicodeString; ATrace: TVerseEditTrace);
begin
  inherited Create;
  if not Assigned(ATrace) then
    raise EArgumentNilException.Create('ATrace');
  FRaw := ARaw;
  FTrace := ATrace;
end;

destructor TTheWordWysiwygSessionSnapshot.Destroy;
begin
  FTrace.Free;
  inherited Destroy;
end;

constructor TTheWordWysiwygSessionV2.Create(const ARaw: UnicodeString;
  ATokenizerVersion: Integer);
begin
  inherited Create;
  FCodec := TSessionOntCodec.Create;
  FUndo := TFPObjectList.Create(True);
  FRedo := TFPObjectList.Create(True);
  FRaw := ARaw;
  FOriginalRaw := ARaw;
  FDocument := FCodec.DecodeVerse(ARaw);
  FTrace := TVerseEditTrace.CreateIdentity(FDocument.VisibleText,
    ATokenizerVersion);
  FRevision := 0;
end;

destructor TTheWordWysiwygSessionV2.Destroy;
begin
  FDocument.Free;
  FTrace.Free;
  FRedo.Free;
  FUndo.Free;
  FCodec := nil;
  inherited Destroy;
end;

function TTheWordWysiwygSessionV2.CaptureSnapshot:
  TTheWordWysiwygSessionSnapshot;
begin
  Result := TTheWordWysiwygSessionSnapshot.Create(FRaw,
    FTrace.Clone);
end;

function TTheWordWysiwygSessionV2.CloneEditTrace: TVerseEditTrace;
begin
  Result := FTrace.Clone;
end;

function TTheWordWysiwygSessionV2.GetRawText: UnicodeString;
begin
  Result := FRaw;
end;

function TTheWordWysiwygSessionV2.GetVisibleText: UnicodeString;
begin
  Result := FDocument.VisibleText;
end;

function TTheWordWysiwygSessionV2.GetCapability: TTheWordLayerCapability;
begin
  Result := TTheWordCapabilityAnalyzer.AnalyzeVerse(FDocument);
end;

function TTheWordWysiwygSessionV2.GetDirty: Boolean;
begin
  Result := (FRaw <> FOriginalRaw) or
    not FTrace.IsBaselineIdentity;
end;

function TTheWordWysiwygSessionV2.GetCanUndo: Boolean;
begin
  Result := FUndo.Count > 0;
end;

function TTheWordWysiwygSessionV2.GetCanRedo: Boolean;
begin
  Result := FRedo.Count > 0;
end;

type
  TNonSemanticEventCategory = (
    nsecLexical,
    nsecOpaque,
    nsecStructural,
    nsecTranslatorNote
  );

  TNonSemanticEventSnapshot = record
    Category: TNonSemanticEventCategory;
    Kind: Integer;
    Offset: Integer;
    RawLexeme: UnicodeString;
    Ordinal: Integer;
  end;

  TNonSemanticEventSequence = array of TNonSemanticEventSnapshot;

procedure SortNonSemanticEvents(var AEvents: TNonSemanticEventSequence);
var
  Event: TNonSemanticEventSnapshot;
  Index, Position: Integer;
begin
  for Index := 1 to High(AEvents) do
  begin
    Event := AEvents[Index];
    Position := Index - 1;
    while (Position >= 0) and
      (AEvents[Position].Ordinal > Event.Ordinal) do
    begin
      AEvents[Position + 1] := AEvents[Position];
      Dec(Position);
    end;
    AEvents[Position + 1] := Event;
  end;
end;

function IsPresentationBreak(const ARaw: UnicodeString): Boolean;
begin
  Result := (ARaw = '<CM>') or (ARaw = '<CL>');
end;

function IsPresentationTitleOrdinal(AOrdinal: Integer;
  const ATitles: TTheWordTitleRangeArray): Boolean;
var
  Index: Integer;
begin
  for Index := 0 to High(ATitles) do
    if (ATitles[Index].Level >= 0) and
       (ATitles[Index].CloseOrdinal >= 0) and
       ((ATitles[Index].OpenOrdinal = AOrdinal) or
        (ATitles[Index].CloseOrdinal = AOrdinal)) then
      Exit(True);
  Result := False;
end;


function BuildNonSemanticEventSequence(
  ADocument: TTheWordVerseDocument): TNonSemanticEventSequence;
var
  Index, Position: Integer;
  Titles: TTheWordTitleRangeArray;
begin
  Result := nil;
  if not Assigned(ADocument) then
    Exit;
  SetLength(Result, ADocument.LexicalItemCount +
    ADocument.OpaqueItemCount + ADocument.StructuralItemCount +
    ADocument.TranslatorNoteCount);
  CollectTheWordTitleRanges(ADocument, Titles);
  Position := 0;
  for Index := 0 to ADocument.LexicalItemCount - 1 do
  begin
    Result[Position].Category := nsecLexical;
    Result[Position].Kind := Ord(ADocument.LexicalItems[Index].Kind);
    Result[Position].Offset := ADocument.LexicalItems[Index].Offset;
    Result[Position].RawLexeme := ADocument.LexicalItems[Index].RawLexeme;
    Result[Position].Ordinal := ADocument.LexicalItems[Index].Ordinal;
    Inc(Position);
  end;
  for Index := 0 to ADocument.OpaqueItemCount - 1 do
  begin
    if IsPresentationBreak(ADocument.OpaqueItems[Index].RawLexeme) or
       IsPresentationTitleOrdinal(ADocument.OpaqueItems[Index].Ordinal,
         Titles) then
      Continue;
    Result[Position].Category := nsecOpaque;
    Result[Position].Kind := Ord(ADocument.OpaqueItems[Index].Kind);
    Result[Position].Offset := ADocument.OpaqueItems[Index].Offset;
    Result[Position].RawLexeme := ADocument.OpaqueItems[Index].RawLexeme;
    Result[Position].Ordinal := ADocument.OpaqueItems[Index].Ordinal;
    Inc(Position);
  end;
  for Index := 0 to ADocument.StructuralItemCount - 1 do
  begin
    Result[Position].Category := nsecStructural;
    Result[Position].Kind := Ord(ADocument.StructuralItems[Index].Kind);
    Result[Position].Offset := ADocument.StructuralItems[Index].Offset;
    Result[Position].RawLexeme := ADocument.StructuralItems[Index].RawLexeme;
    Result[Position].Ordinal := ADocument.StructuralItems[Index].Ordinal;
    Inc(Position);
  end;
  for Index := 0 to ADocument.TranslatorNoteCount - 1 do
  begin
    Result[Position].Category := nsecTranslatorNote;
    Result[Position].Kind := Ord(ADocument.TranslatorNotes[Index].Capability);
    Result[Position].Offset := ADocument.TranslatorNotes[Index].OffsetUTF16;
    Result[Position].RawLexeme :=
      ADocument.TranslatorNotes[Index].OpenLexeme +
      ADocument.TranslatorNotes[Index].BodyRaw +
      ADocument.TranslatorNotes[Index].CloseLexeme;
    Result[Position].Ordinal := ADocument.TranslatorNotes[Index].Ordinal;
    Inc(Position);
  end;
  SetLength(Result, Position);
  SortNonSemanticEvents(Result);
end;

function SameNonSemanticContent(ALeft,
  ARight: TTheWordVerseDocument): Boolean;
var
  Index: Integer;
  LeftEvents, RightEvents: TNonSemanticEventSequence;
begin
  Result := Assigned(ALeft) and Assigned(ARight) and
    (ALeft.VisibleText = ARight.VisibleText);
  if not Result then
    Exit;
  LeftEvents := BuildNonSemanticEventSequence(ALeft);
  RightEvents := BuildNonSemanticEventSequence(ARight);
  if Length(LeftEvents) <> Length(RightEvents) then
    Exit(False);
  for Index := 0 to High(LeftEvents) do
    if (LeftEvents[Index].Category <> RightEvents[Index].Category) or
       (LeftEvents[Index].Kind <> RightEvents[Index].Kind) or
       (LeftEvents[Index].Offset <> RightEvents[Index].Offset) or
       (LeftEvents[Index].RawLexeme <> RightEvents[Index].RawLexeme) then
      Exit(False);
end;

function TTheWordWysiwygSessionV2.GetChangesAreSemanticOnly: Boolean;
var
  Original: TTheWordVerseDocument;
begin
  if not Dirty then Exit(True);
  Original := nil;
  try
    Original := FCodec.DecodeVerse(FOriginalRaw);
    Result := SameNonSemanticContent(Original, FDocument);
  except
    Result := False;
  end;
  Original.Free;
end;

function TTheWordWysiwygSessionV2.ProtectedLexicalObjectsUnchanged: Boolean;
var
  CurrentItem, OriginalItem: TTheWordLexicalItem;
  CurrentStart, Index, OriginalLength, OriginalStart: Integer;
  OriginalDocument: TTheWordVerseDocument;
begin
  OriginalDocument := nil;
  try
    OriginalDocument := FCodec.DecodeVerse(FOriginalRaw);
    try
      Result := OriginalDocument.LexicalItemCount =
        FDocument.LexicalItemCount;
      if not Result then
        Exit;

      for Index := 0 to OriginalDocument.LexicalItemCount - 1 do
      begin
        OriginalItem := OriginalDocument.LexicalItems[Index];
        CurrentItem := FDocument.LexicalItems[Index];
        if (OriginalItem.Kind <> CurrentItem.Kind) or
           (OriginalItem.RawLexeme <> CurrentItem.RawLexeme) or
           (OriginalItem.Value <> CurrentItem.Value) then
          Exit(False);

        OriginalStart := OriginalDocument.LexicalProtectedStart(Index);
        OriginalLength := OriginalItem.Offset - OriginalStart;
        if not FTrace.OriginMap.TryProjectOriginalRange(OriginalStart,
          OriginalLength, CurrentStart) or
           (FDocument.LexicalProtectedStart(Index) <> CurrentStart) or
           (CurrentItem.Offset <> CurrentStart + OriginalLength) then
          Exit(False);
      end;
    finally
      OriginalDocument.Free;
      OriginalDocument := nil;
    end;
  except
    Result := False;
  end;
end;

function CanCommitLegacyWysiwygChangeV2(AHasAssociations,
  AChangesAreSemanticOnly: Boolean): Boolean;
begin
  Result := not AHasAssociations or AChangesAreSemanticOnly;
end;

function TTheWordWysiwygSessionV2.CopySelection(AStartUTF16,
  ALengthUTF16: Integer; out APlainText: UnicodeString;
  out APayload: string): Boolean;
begin
  Result := TTheWordClipboardCodecV2.EncodeSelection(FDocument,
    AStartUTF16, ALengthUTF16, APlainText, APayload);
end;

function TTheWordWysiwygSessionV2.CopySelection(AStartUTF16,
  ALengthUTF16: Integer; const ABreaks: TTheWordBreakIdentityArray;
  out APlainText: UnicodeString; out APayload: string): Boolean;
begin
  Result := TTheWordClipboardCodecV2.EncodeSelection(FDocument,
    AStartUTF16, ALengthUTF16, ABreaks, APlainText, APayload);
end;

function TTheWordWysiwygSessionV2.CutSelection(AStartUTF16,
  ALengthUTF16: Integer; out APlainText: UnicodeString;
  out APayload: string): Boolean;
var
  NewTrace: TVerseEditTrace;
  Raw, SnapshotRaw: UnicodeString;
  Snapshot: TTheWordWysiwygSessionSnapshot;
begin
  Result := not TouchesTheWordTitle(FDocument, AStartUTF16,
    ALengthUTF16) and
    CopySelection(AStartUTF16, ALengthUTF16, APlainText, APayload) and
    TTheWordClipboardCodecV2.BuildCutRaw(FDocument, AStartUTF16,
      ALengthUTF16, Raw);
  if not Result then Exit;
  SnapshotRaw := GetRawText;
  if Raw = SnapshotRaw then Exit(False);
  Snapshot := nil;
  NewTrace := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    NewTrace.ApplyReplacement(AStartUTF16, ALengthUTF16, '');
    ReplaceDocument(Raw);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
  finally
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.CutSelection(AStartUTF16,
  ALengthUTF16: Integer; const ABreaks: TTheWordBreakIdentityArray;
  out APlainText: UnicodeString; out APayload: string): Boolean;
var
  NewTrace: TVerseEditTrace;
  Raw, SnapshotRaw: UnicodeString;
  Snapshot: TTheWordWysiwygSessionSnapshot;
begin
  Result := not TouchesTheWordTitle(FDocument, AStartUTF16,
    ALengthUTF16) and
    CopySelection(AStartUTF16, ALengthUTF16, ABreaks,
      APlainText, APayload) and
    TTheWordClipboardCodecV2.BuildCutRaw(FDocument, AStartUTF16,
      ALengthUTF16, ABreaks, Raw);
  if not Result then Exit;
  SnapshotRaw := GetRawText;
  if Raw = SnapshotRaw then Exit(False);
  Snapshot := nil;
  NewTrace := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    if ALengthUTF16 > 0 then
      NewTrace.ApplyReplacement(AStartUTF16, ALengthUTF16, '');
    ReplaceDocument(Raw);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.Paste(AStartUTF16,
  ALengthUTF16: Integer; const APayload: string;
  const APlainText: UnicodeString; out AInsertedLengthUTF16: Integer;
  out AUsedPrivatePayload: Boolean): Boolean;
var
  Fragment: TTheWordClipboardFragmentV2;
  NewTrace: TVerseEditTrace;
  Raw, SnapshotRaw: UnicodeString;
  Snapshot: TTheWordWysiwygSessionSnapshot;
begin
  Fragment := nil;
  AInsertedLengthUTF16 := 0;
  AUsedPrivatePayload := TTheWordClipboardCodecV2.TryDecodePayload(
    APayload, Fragment);
  if not AUsedPrivatePayload then
    Fragment := TTheWordClipboardCodecV2.NewPlainFragment(APlainText);
  try
    AInsertedLengthUTF16 := Length(Fragment.Text);
    Result := TTheWordClipboardCodecV2.BuildPasteRaw(FDocument,
      AStartUTF16, ALengthUTF16, Fragment, Raw);
    if not Result then Exit;
    SnapshotRaw := GetRawText;
    if Raw = SnapshotRaw then Exit(False);
    Snapshot := nil;
    NewTrace := nil;
    try
      Snapshot := CaptureSnapshot;
      NewTrace := FTrace.Clone;
      NewTrace.ApplyReplacement(AStartUTF16, ALengthUTF16,
        Fragment.Text);
      ReplaceDocument(Raw);
      FTrace.Free;
      FTrace := NewTrace;
      NewTrace := nil;
      PushUndo(Snapshot);
      Snapshot := nil;
    finally
      NewTrace.Free;
      Snapshot.Free;
    end;
  finally
    Fragment.Free;
  end;
end;

function TTheWordWysiwygSessionV2.PasteTitle(AOpenOrdinal,
  ARelativeStartUTF16, ALengthUTF16: Integer; const APayload: string;
  const APlainText: UnicodeString; out AInsertedLengthUTF16: Integer;
  out AUsedPrivatePayload: Boolean): Boolean;
var
  AbsoluteStart, I, TitleLength: Integer;
  AdjustedTitle, CandidateTitle, Title: TTheWordTitleRange;
  CandidateTitles: TTheWordTitleRangeArray;
  Candidate, SnapshotRaw, TitleFreeRaw: UnicodeString;
  Fragment: TTheWordClipboardFragmentV2;
  NewTrace: TVerseEditTrace;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  TitlePreserved: Boolean;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  Fragment := nil;
  AInsertedLengthUTF16 := 0;
  AUsedPrivatePayload := TTheWordClipboardCodecV2.TryDecodePayload(
    APayload, Fragment);
  if not AUsedPrivatePayload then
    Fragment := TTheWordClipboardCodecV2.NewPlainFragment(APlainText);
  try
    if (Fragment.ObjectCount <> 0) or
       not TryGetTitle(AOpenOrdinal, Title) or not Title.Editable then
      Exit;
    TitleLength := Title.EndOffset - Title.StartOffset;
    if (ARelativeStartUTF16 < 0) or (ALengthUTF16 < 0) or
       (ARelativeStartUTF16 > TitleLength) or
       (ALengthUTF16 > TitleLength - ARelativeStartUTF16) then
      Exit;
    AInsertedLengthUTF16 := Length(Fragment.Text);
    if (ARelativeStartUTF16 = 0) and (ALengthUTF16 = TitleLength) and
       (Fragment.Text = '') then
      Exit(RemoveTitle(AOpenOrdinal));

    AbsoluteStart := Title.StartOffset + ARelativeStartUTF16;
    SnapshotRaw := GetRawText;
    WorkingDocument := FCodec.DecodeVerse(SnapshotRaw);
    try
      if not WorkingDocument.UnwrapTitle(AOpenOrdinal) or
         not TTheWordClipboardCodecV2.BuildPasteRaw(WorkingDocument,
           AbsoluteStart, ALengthUTF16, Fragment, TitleFreeRaw) then
        Exit;
    finally
      WorkingDocument.Free;
    end;

    AdjustedTitle := Title;
    AdjustedTitle.EndOffset := AdjustedTitle.EndOffset - ALengthUTF16 +
      AInsertedLengthUTF16;
    if not RestoreTitleInCandidate(AdjustedTitle, TitleFreeRaw,
      Candidate) or (Candidate = SnapshotRaw) then
      Exit;

    WorkingDocument := FCodec.DecodeVerse(Candidate);
    try
      CollectTheWordTitleRanges(WorkingDocument, CandidateTitles);
      TitlePreserved := False;
      for I := 0 to High(CandidateTitles) do
      begin
        CandidateTitle := CandidateTitles[I];
        if CandidateTitle.Editable and
           (CandidateTitle.StartOffset = AdjustedTitle.StartOffset) and
           (CandidateTitle.EndOffset = AdjustedTitle.EndOffset) and
           (CandidateTitle.Level = AdjustedTitle.Level) and
           (CandidateTitle.OpenLexeme = AdjustedTitle.OpenLexeme) and
           (CandidateTitle.CloseLexeme = AdjustedTitle.CloseLexeme) then
        begin
          TitlePreserved := True;
          Break;
        end;
      end;
      if not TitlePreserved then
        Exit;
    finally
      WorkingDocument.Free;
    end;

    Snapshot := nil;
    NewTrace := nil;
    try
      Snapshot := CaptureSnapshot;
      NewTrace := FTrace.Clone;
      NewTrace.ApplyReplacement(AbsoluteStart, ALengthUTF16,
        Fragment.Text);
      ReplaceDocument(Candidate);
      FTrace.Free;
      FTrace := NewTrace;
      NewTrace := nil;
      PushUndo(Snapshot);
      Snapshot := nil;
      Result := True;
    finally
      NewTrace.Free;
      Snapshot.Free;
    end;
  finally
    Fragment.Free;
  end;
end;

function TTheWordWysiwygSessionV2.Paste(AStartUTF16,
  ALengthUTF16: Integer; const ABreaks: TTheWordBreakIdentityArray;
  AAfterAnchors: Boolean; const APayload: string;
  const APlainText: UnicodeString; out AInsertedLengthUTF16: Integer;
  out AUsedPrivatePayload: Boolean): Boolean;
var
  InsertBefore: TTheWordBreakIdentity;
begin
  FillChar(InsertBefore, SizeOf(InsertBefore), 0);
  Result := Paste(AStartUTF16, ALengthUTF16, ABreaks,
    AAfterAnchors, False, InsertBefore, APayload, APlainText,
    AInsertedLengthUTF16, AUsedPrivatePayload);
end;

function TTheWordWysiwygSessionV2.Paste(AStartUTF16,
  ALengthUTF16: Integer; const ABreaks: TTheWordBreakIdentityArray;
  AAfterAnchors, AHasInsertBefore: Boolean;
  const AInsertBefore: TTheWordBreakIdentity; const APayload: string;
  const APlainText: UnicodeString; out AInsertedLengthUTF16: Integer;
  out AUsedPrivatePayload: Boolean): Boolean;
var
  Fragment: TTheWordClipboardFragmentV2;
  NewTrace: TVerseEditTrace;
  Raw, SnapshotRaw: UnicodeString;
  Snapshot: TTheWordWysiwygSessionSnapshot;
begin
  Fragment := nil;
  AInsertedLengthUTF16 := 0;
  AUsedPrivatePayload := TTheWordClipboardCodecV2.TryDecodePayload(
    APayload, Fragment);
  if not AUsedPrivatePayload then
    Fragment := TTheWordClipboardCodecV2.NewPlainFragment(APlainText);
  try
    AInsertedLengthUTF16 := Length(Fragment.Text);
    Result := TTheWordClipboardCodecV2.BuildPasteRaw(FDocument,
      AStartUTF16, ALengthUTF16, ABreaks, AAfterAnchors,
      AHasInsertBefore, AInsertBefore, Fragment, Raw);
    if not Result then Exit;
    SnapshotRaw := GetRawText;
    if Raw = SnapshotRaw then Exit(False);
    Snapshot := nil;
    NewTrace := nil;
    try
      Snapshot := CaptureSnapshot;
      NewTrace := FTrace.Clone;
      NewTrace.ApplyReplacement(AStartUTF16, ALengthUTF16,
        Fragment.Text);
      ReplaceDocument(Raw);
      FTrace.Free;
      FTrace := NewTrace;
      NewTrace := nil;
      PushUndo(Snapshot);
      Snapshot := nil;
      Result := True;
    finally
      NewTrace.Free;
      Snapshot.Free;
    end;
  finally
    Fragment.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2.PushUndo(
  ASnapshot: TTheWordWysiwygSessionSnapshot);
begin
  if not Assigned(ASnapshot) then
    raise EArgumentNilException.Create('ASnapshot');
  FUndo.Add(ASnapshot);
  FRedo.Clear;
end;

procedure TTheWordWysiwygSessionV2.AdoptDocument(
  const ARaw: UnicodeString; var ADocument: TTheWordVerseDocument);
begin
  if not Assigned(ADocument) then
    raise EArgumentNilException.Create('ADocument');
  FDocument.Free;
  FDocument := ADocument;
  ADocument := nil;
  FRaw := ARaw;
  Inc(FRevision);
end;

procedure TTheWordWysiwygSessionV2.ReplaceDocument(
  const ARaw: UnicodeString);
var
  Replacement: TTheWordVerseDocument;
begin
  { Decode first. The raw cache and document are swapped only after the codec
    has accepted the complete candidate, so they can never describe different
    revisions of the verse. }
  Replacement := FCodec.DecodeVerse(ARaw);
  FDocument.Free;
  FDocument := Replacement;
  FRaw := ARaw;
  Inc(FRevision);
end;

procedure TTheWordWysiwygSessionV2.RestoreFromStack(ASource,
  ADestination: TFPObjectList);
var
  Current, Snapshot: TTheWordWysiwygSessionSnapshot;
  RestoredTrace: TVerseEditTrace;
begin
  if ASource.Count = 0 then
    Exit;
  Snapshot := TTheWordWysiwygSessionSnapshot(
    ASource[ASource.Count - 1]);
  RestoredTrace := Snapshot.Trace.Clone;
  Current := CaptureSnapshot;
  try
    ReplaceDocument(Snapshot.Raw);
    FTrace.Free;
    FTrace := RestoredTrace;
    RestoredTrace := nil;
    ADestination.Add(Current);
    Current := nil;
    ASource.Delete(ASource.Count - 1);
  finally
    RestoredTrace.Free;
    Current.Free;
  end;
end;

function TTheWordWysiwygSessionV2.TryResolveEditableTitleScope(
  AStartUTF16, ALengthUTF16: Integer; out AHasTitle: Boolean;
  out ATitle: TTheWordTitleRange): Boolean;
var
  EndUTF16, Index: Integer;
  Titles: TTheWordTitleRangeArray;
  Touches: Boolean;
begin
  AHasTitle := False;
  FillChar(ATitle, SizeOf(ATitle), 0);
  Result := Assigned(FDocument) and
    FDocument.CanApplySemanticRange(AStartUTF16,
      AStartUTF16 + ALengthUTF16);
  if not Result then
    Exit;
  EndUTF16 := AStartUTF16 + ALengthUTF16;
  CollectTheWordTitleRanges(FDocument, Titles);
  for Index := 0 to High(Titles) do
  begin
    if Titles[Index].StartOffset = Titles[Index].EndOffset then
      Touches := (Titles[Index].StartOffset > AStartUTF16) and
        (Titles[Index].StartOffset < EndUTF16)
    else
      Touches := (AStartUTF16 < Titles[Index].EndOffset) and
        (EndUTF16 > Titles[Index].StartOffset);
    if not Touches then
      Continue;
    if AHasTitle or not Titles[Index].Editable or
       (AStartUTF16 < Titles[Index].StartOffset) or
       (EndUTF16 > Titles[Index].EndOffset) then
      Exit(False);
    AHasTitle := True;
    ATitle := Titles[Index];
  end;
end;

function TTheWordWysiwygSessionV2.TryPrepareTitleFreeDocument(
  AStartUTF16, ALengthUTF16: Integer;
  out ADocument: TTheWordVerseDocument; out AHasTitle: Boolean;
  out ATitle: TTheWordTitleRange): Boolean;
begin
  ADocument := nil;
  Result := TryResolveEditableTitleScope(AStartUTF16,
    ALengthUTF16, AHasTitle, ATitle);
  if not Result then
    Exit;
  ADocument := FCodec.DecodeVerse(GetRawText);
  if AHasTitle and not ADocument.UnwrapTitle(ATitle.OpenOrdinal) then
  begin
    FreeAndNil(ADocument);
    Exit(False);
  end;
end;

function TTheWordWysiwygSessionV2.RestoreTitleInCandidate(
  const ATitle: TTheWordTitleRange;
  const ATitleFreeRaw: UnicodeString;
  out ACandidateRaw: UnicodeString): Boolean;
var
  OpenOrdinal: Integer;
begin
  Result := RestoreTitleInCandidate(ATitle, ATitleFreeRaw,
    ACandidateRaw, OpenOrdinal);
end;

function TTheWordWysiwygSessionV2.RestoreTitleInCandidate(
  const ATitle: TTheWordTitleRange;
  const ATitleFreeRaw: UnicodeString;
  out ACandidateRaw: UnicodeString;
  out AOpenOrdinal: Integer): Boolean;
var
  CandidateDocument: TTheWordVerseDocument;
begin
  Result := False;
  ACandidateRaw := '';
  AOpenOrdinal := -1;
  CandidateDocument := FCodec.DecodeVerse(ATitleFreeRaw);
  try
    if not CandidateDocument.RestoreTitle(ATitle.StartOffset,
      ATitle.EndOffset - ATitle.StartOffset, ATitle.Level,
      ATitle.OpenLexeme, ATitle.CloseLexeme, AOpenOrdinal) then
      Exit;
    ACandidateRaw := FCodec.EncodeVerse(CandidateDocument);
    Result := True;
  finally
    CandidateDocument.Free;
  end;
end;

function TTheWordWysiwygSessionV2.CanApplySemantic(AStartUTF16,
  ALengthUTF16: Integer): Boolean;
var
  HasTitle: Boolean;
  Title: TTheWordTitleRange;
begin
  Result := TryResolveEditableTitleScope(AStartUTF16,
    ALengthUTF16, HasTitle, Title);
  if Result and not HasTitle then
    Result := CanApplyTheWordSemanticMark(FDocument, AStartUTF16,
      ALengthUTF16);
end;

function TTheWordWysiwygSessionV2.CanInsertTitle(
  AOffsetUTF16: Integer): Boolean;
begin
  Result := Assigned(FDocument) and FDocument.CanInsertTitle(AOffsetUTF16);
end;

function TTheWordWysiwygSessionV2.CanWrapTitle(AStartUTF16,
  ALengthUTF16: Integer): Boolean;
begin
  Result := Assigned(FDocument) and
    FDocument.CanWrapTitle(AStartUTF16, ALengthUTF16);
end;

function TTheWordWysiwygSessionV2.TryGetTitle(AOpenOrdinal: Integer;
  out ATitle: TTheWordTitleRange): Boolean;
var
  Index: Integer;
  Titles: TTheWordTitleRangeArray;
begin
  CollectTheWordTitleRanges(FDocument, Titles);
  for Index := 0 to High(Titles) do
    if Titles[Index].OpenOrdinal = AOpenOrdinal then
    begin
      ATitle := Titles[Index];
      Exit(True);
    end;
  FillChar(ATitle, SizeOf(ATitle), 0);
  Result := False;
end;

function TTheWordWysiwygSessionV2.ReplaceTitleText(AOpenOrdinal,
  ARelativeStartUTF16, ALengthUTF16: Integer;
  const AReplacement: UnicodeString): Boolean;
var
  AbsoluteStart, TitleLength: Integer;
  NewTrace: TVerseEditTrace;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  Title: TTheWordTitleRange;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if not TryGetTitle(AOpenOrdinal, Title) or not Title.Editable then
    Exit;
  TitleLength := Title.EndOffset - Title.StartOffset;
  if (ARelativeStartUTF16 = 0) and
     (ALengthUTF16 = TitleLength) and (AReplacement = '') then
    Exit(RemoveTitle(AOpenOrdinal));
  AbsoluteStart := Title.StartOffset + ARelativeStartUTF16;
  Snapshot := nil;
  NewTrace := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    if not WorkingDocument.ReplaceTitleVisibleText(AOpenOrdinal,
      ARelativeStartUTF16, ALengthUTF16, AReplacement) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    NewTrace.ApplyReplacement(AbsoluteStart, ALengthUTF16,
      AReplacement);
    ReplaceDocument(UpdatedRaw);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.InsertTitle(AOffsetUTF16,
  ALevel: Integer; const AText: UnicodeString): Boolean;
var
  NewOpenOrdinal: Integer;
  NewTrace: TVerseEditTrace;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if not CanInsertTitle(AOffsetUTF16) then
    Exit;
  Snapshot := nil;
  NewTrace := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    if not WorkingDocument.InsertTitle(AOffsetUTF16, ALevel,
      AText, NewOpenOrdinal) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    NewTrace.ApplyReplacement(AOffsetUTF16, 0, AText);
    ReplaceDocument(UpdatedRaw);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.WrapTitle(AStartUTF16,
  ALengthUTF16, ALevel: Integer; out AOpenOrdinal: Integer): Boolean;
var
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  AOpenOrdinal := -1;
  if not CanWrapTitle(AStartUTF16, ALengthUTF16) then
    Exit;
  Snapshot := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    if not WorkingDocument.WrapTitle(AStartUTF16, ALengthUTF16,
      ALevel, AOpenOrdinal) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if (UpdatedRaw = Snapshot.Raw) or
       (WorkingDocument.VisibleText <> FDocument.VisibleText) then
      Exit;
    AdoptDocument(UpdatedRaw, WorkingDocument);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    Snapshot.Free;
    if not Result then
      AOpenOrdinal := -1;
  end;
end;

function TTheWordWysiwygSessionV2.UnwrapTitle(
  AOpenOrdinal: Integer): Boolean;
var
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  Snapshot := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    if not WorkingDocument.UnwrapTitle(AOpenOrdinal) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if (UpdatedRaw = Snapshot.Raw) or
       (WorkingDocument.VisibleText <> FDocument.VisibleText) then
      Exit;
    ReplaceDocument(UpdatedRaw);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.ChangeTitleLevel(AOpenOrdinal,
  ALevel: Integer): Boolean;
var
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  Snapshot := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    if not WorkingDocument.ChangeTitleLevel(AOpenOrdinal, ALevel) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if UpdatedRaw = Snapshot.Raw then
      Exit(True);
    ReplaceDocument(UpdatedRaw);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.RemoveTitle(
  AOpenOrdinal: Integer): Boolean;
var
  ReplacementText: UnicodeString;
  NewTrace: TVerseEditTrace;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  Title: TTheWordTitleRange;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if not TryGetTitle(AOpenOrdinal, Title) or not Title.Editable then
    Exit;
  Snapshot := nil;
  NewTrace := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    if not WorkingDocument.RemoveTitle(AOpenOrdinal,
      ReplacementText) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    NewTrace.ApplyReplacement(Title.StartOffset,
      Title.EndOffset - Title.StartOffset, ReplacementText);
    ReplaceDocument(UpdatedRaw);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.CanInsertTranslatorNote(
  AOffsetUTF16: Integer): Boolean;
begin
  Result := Assigned(FDocument) and
    FDocument.CanInsertTranslatorNote(AOffsetUTF16);
end;

function TTheWordWysiwygSessionV2.TryGetTranslatorNote(ANoteId: QWord;
  out ANote: TTheWordTranslatorNote): Boolean;
begin
  Result := Assigned(FDocument) and
    FDocument.TryGetTranslatorNote(ANoteId, ANote);
  if not Result then
    ANote := nil;
end;

function TTheWordWysiwygSessionV2.InsertTranslatorNote(
  AOffsetUTF16: Integer; const AQ, ABodyRaw: UnicodeString;
  out ANoteId: QWord): Boolean;
var
  AnchorRank, CandidateRank, Index: Integer;
  Candidate, Inserted: TTheWordTranslatorNote;
  FoundInserted: Boolean;
  Replacement, WorkingDocument: TTheWordVerseDocument;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
begin
  Result := False;
  ANoteId := 0;
  if not CanInsertTranslatorNote(AOffsetUTF16) then
    Exit;
  Snapshot := nil;
  Replacement := nil;
  WorkingDocument := nil;
  try
    WorkingDocument := FCodec.DecodeVerse(GetRawText);
    Inserted := WorkingDocument.InsertTranslatorNote(AOffsetUTF16,
      AQ, ABodyRaw);
    if Inserted = nil then
      Exit;

    { IDs are assigned again when the encoded document is validated. Locate
      the new note by its rank among notes at this anchor, so identical RFs
      at the same offset remain distinct. }
    FoundInserted := False;
    AnchorRank := -1;
    for Index := 0 to WorkingDocument.TranslatorNoteCount - 1 do
      if WorkingDocument.TranslatorNotes[Index].OffsetUTF16 =
         Inserted.OffsetUTF16 then
      begin
        Inc(AnchorRank);
        if WorkingDocument.TranslatorNotes[Index] = Inserted then
        begin
          FoundInserted := True;
          Break;
        end;
      end;
    if not FoundInserted then
      Exit;

    if WorkingDocument.VisibleText <> FDocument.VisibleText then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    Replacement := FCodec.DecodeVerse(UpdatedRaw);
    if Replacement.VisibleText <> FDocument.VisibleText then
      Exit;

    Candidate := nil;
    CandidateRank := -1;
    for Index := 0 to Replacement.TranslatorNoteCount - 1 do
      if Replacement.TranslatorNotes[Index].OffsetUTF16 =
         Inserted.OffsetUTF16 then
      begin
        Inc(CandidateRank);
        if CandidateRank = AnchorRank then
        begin
          Candidate := Replacement.TranslatorNotes[Index];
          Break;
        end;
      end;
    if Candidate = nil then
      Exit;
    ANoteId := Candidate.Id;

    Snapshot := CaptureSnapshot;
    FDocument.Free;
    FDocument := Replacement;
    Replacement := nil;
    FRaw := UpdatedRaw;
    Inc(FRevision);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    Snapshot.Free;
    Replacement.Free;
    WorkingDocument.Free;
  end;
end;

function TTheWordWysiwygSessionV2.UpdateTranslatorNote(ANoteId: QWord;
  const AQ, ABodyRaw: UnicodeString): Boolean;
var
  Existing: TTheWordTranslatorNote;
  Replacement, WorkingDocument: TTheWordVerseDocument;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
begin
  Result := False;
  if not FDocument.TryGetTranslatorNote(ANoteId, Existing) then
    Exit;
  if (Existing.Q = AQ) and (Existing.BodyRaw = ABodyRaw) then
    Exit(True);

  Snapshot := nil;
  Replacement := nil;
  WorkingDocument := nil;
  try
    WorkingDocument := FCodec.DecodeVerse(GetRawText);
    if not WorkingDocument.UpdateTranslatorNote(ANoteId, AQ,
      ABodyRaw) then
      Exit;
    if WorkingDocument.VisibleText <> FDocument.VisibleText then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    Replacement := FCodec.DecodeVerse(UpdatedRaw);
    if Replacement.VisibleText <> FDocument.VisibleText then
      Exit;

    Snapshot := CaptureSnapshot;
    FDocument.Free;
    FDocument := Replacement;
    Replacement := nil;
    FRaw := UpdatedRaw;
    Inc(FRevision);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    Snapshot.Free;
    Replacement.Free;
    WorkingDocument.Free;
  end;
end;

function TTheWordWysiwygSessionV2.RemoveTranslatorNote(
  ANoteId: QWord): Boolean;
var
  Replacement, WorkingDocument: TTheWordVerseDocument;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
begin
  Result := False;
  Snapshot := nil;
  Replacement := nil;
  WorkingDocument := nil;
  try
    WorkingDocument := FCodec.DecodeVerse(GetRawText);
    if not WorkingDocument.RemoveTranslatorNote(ANoteId) then
      Exit;
    if WorkingDocument.VisibleText <> FDocument.VisibleText then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    Replacement := FCodec.DecodeVerse(UpdatedRaw);
    if Replacement.VisibleText <> FDocument.VisibleText then
      Exit;

    Snapshot := CaptureSnapshot;
    FDocument.Free;
    FDocument := Replacement;
    Replacement := nil;
    FRaw := UpdatedRaw;
    Inc(FRevision);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    Snapshot.Free;
    Replacement.Free;
    WorkingDocument.Free;
  end;
end;

function TTheWordWysiwygSessionV2.CanReplaceText(AStartUTF16,
  ALengthUTF16: Integer): Boolean;
begin
  Result := CanReplaceTheWordText(FDocument, AStartUTF16, ALengthUTF16);
end;

function TTheWordWysiwygSessionV2.ApplySemantic(
  AKind: TTheWordSemanticKind; AStartUTF16, ALengthUTF16: Integer): Boolean;
var
  Snapshot: TTheWordWysiwygSessionSnapshot;
  SnapshotRaw, UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if not CanApplySemantic(AStartUTF16, ALengthUTF16) then
    Exit;
  SnapshotRaw := GetRawText;
  WorkingDocument := FCodec.DecodeVerse(SnapshotRaw);
  Snapshot := nil;
  try
    if not WorkingDocument.ToggleSemanticMark(AKind, AStartUTF16,
      AStartUTF16 + ALengthUTF16) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if UpdatedRaw = SnapshotRaw then
      Exit;

    { ReplaceDocument decodes before swapping FDocument, so a codec failure
      cannot leave the live session partially mutated. }
    Snapshot := CaptureSnapshot;
    AdoptDocument(UpdatedRaw, WorkingDocument);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    Snapshot.Free;
    WorkingDocument.Free;
  end;
end;

function TTheWordWysiwygSessionV2.ApplyFontColor(AStartUTF16,
  ALengthUTF16: Integer; AHasColor: Boolean; AColor: LongInt): Boolean;
var
  Candidate, SnapshotRaw, TitleFreeCandidate: UnicodeString;
  HasTitle: Boolean;
  Plan: TTheWordFontColorLeafPlanArrayV2;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  Title: TTheWordTitleRange;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  SnapshotRaw := GetRawText;
  Snapshot := nil;
  WorkingDocument := nil;
  try
    if not TryPrepareTitleFreeDocument(AStartUTF16,
      ALengthUTF16, WorkingDocument, HasTitle, Title) or
       not TryPlanTheWordFontColorV2(WorkingDocument, AStartUTF16,
         ALengthUTF16, AHasColor, AColor, Plan) then
      Exit;
    if not TheWordFontColorPlanHasChangesV2(Plan) then
      Exit(True);
    if not WorkingDocument.TryBuildRawWithFontColorTopology(Plan,
      Candidate) then
      Exit;
    if HasTitle then
    begin
      TitleFreeCandidate := Candidate;
      if not RestoreTitleInCandidate(Title,
        TitleFreeCandidate, Candidate) then
        Exit;
    end;
    if Candidate = SnapshotRaw then
      Exit(True);

    Snapshot := CaptureSnapshot;
    ReplaceDocument(Candidate);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.ReplaceTextWithPendingFormatting(
  AStartUTF16, ALengthUTF16: Integer;
  const AReplacement: UnicodeString;
  AAfterAnchors, AApplyFontSize: Boolean;
  ACurrentLevel, ATargetLevel: Integer;
  AApplyFontColor, ATargetHasColor: Boolean;
  ATargetColor: LongInt): Boolean;
var
  Candidate, NextCandidate: UnicodeString;
  ColorPlan: TTheWordFontColorLeafPlanArrayV2;
  CurrentLevel, Step: Integer;
  NewTrace: TVerseEditTrace;
  SizePlan: TTheWordFontSizeLeafPlanArrayV2;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if (AReplacement = '') or
     (AApplyFontSize and
       ((ATargetLevel < THEWORD_MIN_AUTHORIAL_SIZE_V2) or
        (ATargetLevel > THEWORD_MAX_AUTHORIAL_SIZE_V2))) or
     (AApplyFontColor and ATargetHasColor and
       ((ATargetColor < 0) or (ATargetColor > $00FFFFFF))) or
     not CanReplaceText(AStartUTF16, ALengthUTF16) then
    Exit;

  Snapshot := nil;
  NewTrace := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    WorkingDocument.ReplaceVisibleTextWithAffinity(AStartUTF16,
      ALengthUTF16, AReplacement, AAfterAnchors);
    Candidate := FCodec.EncodeVerse(WorkingDocument);
    if AApplyFontSize or AApplyFontColor then
      FreeAndNil(WorkingDocument);
    NewTrace.ApplyReplacement(AStartUTF16, ALengthUTF16,
      AReplacement);

    if AApplyFontSize then
    begin
      CurrentLevel := ACurrentLevel;
      while CurrentLevel <> ATargetLevel do
      begin
        if CurrentLevel < ATargetLevel then
          Step := 1
        else
          Step := -1;
        WorkingDocument := FCodec.DecodeVerse(Candidate);
        try
          if not TryPlanTheWordRelativeFontSizeV2(WorkingDocument,
            AStartUTF16, Length(AReplacement), Step, SizePlan) or
             not WorkingDocument.TryBuildRawWithFontTopology(SizePlan,
               NextCandidate) then
            Exit;
        finally
          FreeAndNil(WorkingDocument);
        end;
        Candidate := NextCandidate;
        Inc(CurrentLevel, Step);
      end;
    end;

    if AApplyFontColor then
    begin
      WorkingDocument := FCodec.DecodeVerse(Candidate);
      try
        if not TryPlanTheWordFontColorV2(WorkingDocument,
          AStartUTF16, Length(AReplacement), ATargetHasColor,
          ATargetColor, ColorPlan) then
          Exit;
        if TheWordFontColorPlanHasChangesV2(ColorPlan) then
        begin
          if not WorkingDocument.TryBuildRawWithFontColorTopology(
            ColorPlan, NextCandidate) then
            Exit;
          Candidate := NextCandidate;
        end;
      finally
        FreeAndNil(WorkingDocument);
      end;
    end;

    if Candidate = Snapshot.Raw then
      Exit(True);
    if Assigned(WorkingDocument) then
      AdoptDocument(Candidate, WorkingDocument)
    else
      ReplaceDocument(Candidate);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.
  ReplaceTitleTextWithPendingFormatting(AOpenOrdinal,
  ARelativeStartUTF16, ALengthUTF16: Integer;
  const AReplacement: UnicodeString;
  AApplyFontSize: Boolean; ACurrentLevel, ATargetLevel: Integer;
  AApplyFontColor, ATargetHasColor: Boolean;
  ATargetColor: LongInt;
  out AResultOpenOrdinal: Integer): Boolean;
var
  AbsoluteStart, CurrentLevel, Step, TitleLength: Integer;
  AdjustedTitle, Title: TTheWordTitleRange;
  Candidate, NextCandidate, TitleFreeCandidate: UnicodeString;
  ColorPlan: TTheWordFontColorLeafPlanArrayV2;
  NewTrace: TVerseEditTrace;
  SizePlan: TTheWordFontSizeLeafPlanArrayV2;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  AResultOpenOrdinal := -1;
  if (AReplacement = '') or
     (AApplyFontSize and
       ((ATargetLevel < THEWORD_MIN_AUTHORIAL_SIZE_V2) or
        (ATargetLevel > THEWORD_MAX_AUTHORIAL_SIZE_V2))) or
     (AApplyFontColor and ATargetHasColor and
       ((ATargetColor < 0) or (ATargetColor > $00FFFFFF))) or
     not TryGetTitle(AOpenOrdinal, Title) or not Title.Editable then
    Exit;
  TitleLength := Title.EndOffset - Title.StartOffset;
  if (ARelativeStartUTF16 < 0) or (ALengthUTF16 < 0) or
     (ARelativeStartUTF16 > TitleLength) or
     (ALengthUTF16 > TitleLength - ARelativeStartUTF16) then
    Exit;
  AbsoluteStart := Title.StartOffset + ARelativeStartUTF16;

  Snapshot := nil;
  NewTrace := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    { Route through the title edit primitive first. Besides preserving its
      range rules, this rejects markup delimiters, paragraph characters and
      malformed UTF-16 exactly like ordinary title typing. }
    if not WorkingDocument.ReplaceTitleVisibleText(AOpenOrdinal,
      ARelativeStartUTF16, ALengthUTF16, AReplacement) or
       not WorkingDocument.UnwrapTitle(AOpenOrdinal) then
      Exit;
    Candidate := FCodec.EncodeVerse(WorkingDocument);
    FreeAndNil(WorkingDocument);
    NewTrace.ApplyReplacement(AbsoluteStart, ALengthUTF16,
      AReplacement);

    if AApplyFontSize then
    begin
      CurrentLevel := ACurrentLevel;
      while CurrentLevel <> ATargetLevel do
      begin
        if CurrentLevel < ATargetLevel then
          Step := 1
        else
          Step := -1;
        WorkingDocument := FCodec.DecodeVerse(Candidate);
        try
          if not TryPlanTheWordRelativeFontSizeV2(WorkingDocument,
            AbsoluteStart, Length(AReplacement), Step, SizePlan) or
             not WorkingDocument.TryBuildRawWithFontTopology(SizePlan,
               NextCandidate) then
            Exit;
        finally
          FreeAndNil(WorkingDocument);
        end;
        Candidate := NextCandidate;
        Inc(CurrentLevel, Step);
      end;
    end;

    if AApplyFontColor then
    begin
      WorkingDocument := FCodec.DecodeVerse(Candidate);
      try
        if not TryPlanTheWordFontColorV2(WorkingDocument,
          AbsoluteStart, Length(AReplacement), ATargetHasColor,
          ATargetColor, ColorPlan) then
          Exit;
        if TheWordFontColorPlanHasChangesV2(ColorPlan) then
        begin
          if not WorkingDocument.TryBuildRawWithFontColorTopology(
            ColorPlan, NextCandidate) then
            Exit;
          Candidate := NextCandidate;
        end;
      finally
        FreeAndNil(WorkingDocument);
      end;
    end;

    AdjustedTitle := Title;
    AdjustedTitle.EndOffset := AdjustedTitle.EndOffset -
      ALengthUTF16 + Length(AReplacement);
    TitleFreeCandidate := Candidate;
    if not RestoreTitleInCandidate(AdjustedTitle,
      TitleFreeCandidate, Candidate, AResultOpenOrdinal) then
      Exit;
    if Candidate = Snapshot.Raw then
      Exit(True);
    ReplaceDocument(Candidate);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    NewTrace.Free;
    Snapshot.Free;
    if not Result then
      AResultOpenOrdinal := -1;
  end;
end;

function TTheWordWysiwygSessionV2.AdjustRelativeFontSize(
  AStartUTF16, ALengthUTF16, ADelta: Integer): Boolean;
var
  Candidate, SnapshotRaw, TitleFreeCandidate: UnicodeString;
  HasTitle: Boolean;
  Plan: TTheWordFontSizeLeafPlanArrayV2;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  Title: TTheWordTitleRange;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if (ALengthUTF16 <= 0) or ((ADelta <> -1) and (ADelta <> 1)) then
    Exit;
  SnapshotRaw := GetRawText;
  Snapshot := nil;
  WorkingDocument := nil;
  try
    if not TryPrepareTitleFreeDocument(AStartUTF16,
      ALengthUTF16, WorkingDocument, HasTitle, Title) or
       not TryPlanTheWordRelativeFontSizeV2(WorkingDocument,
         AStartUTF16, ALengthUTF16, ADelta, Plan) or
       not WorkingDocument.TryBuildRawWithFontTopology(Plan,
         Candidate) then
      Exit;
    if HasTitle then
    begin
      TitleFreeCandidate := Candidate;
      if not RestoreTitleInCandidate(Title,
        TitleFreeCandidate, Candidate) then
        Exit;
    end;
    if Candidate = SnapshotRaw then
      Exit;

    { ReplaceDocument decodes before swapping FDocument. The trace is left
      untouched because this command changes presentation, not text. }
    Snapshot := CaptureSnapshot;
    ReplaceDocument(Candidate);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.ReplaceTextWithRelativeFontSize(
  AStartUTF16, ALengthUTF16: Integer; const AReplacement: UnicodeString;
  ACurrentLevel, ATargetLevel: Integer): Boolean;
begin
  Result := ReplaceTextWithPendingFormatting(AStartUTF16,
    ALengthUTF16, AReplacement, False, True,
    ACurrentLevel, ATargetLevel, False, False, 0);
end;

function TTheWordWysiwygSessionV2.ReplaceText(AStartUTF16,
  ALengthUTF16: Integer; const AReplacement: UnicodeString): Boolean;
var
  NewTrace: TVerseEditTrace;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if not CanReplaceText(AStartUTF16, ALengthUTF16) then
    Exit;
  if (ALengthUTF16 = 0) and (AReplacement = '') then
    Exit;
  Snapshot := nil;
  NewTrace := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    WorkingDocument.ReplaceVisibleText(AStartUTF16, ALengthUTF16,
      AReplacement);
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    NewTrace.ApplyReplacement(AStartUTF16, ALengthUTF16, AReplacement);
    AdoptDocument(UpdatedRaw, WorkingDocument);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.DeleteSelectionWithBreaks(
  AStartUTF16, ALengthUTF16: Integer;
  const ABreaks: TTheWordBreakIdentityArray): Boolean;
var
  EndOffset, Index: Integer;
  NewTrace: TVerseEditTrace;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if (AStartUTF16 < 0) or (ALengthUTF16 < 0) or
     (AStartUTF16 > Length(FDocument.VisibleText)) or
     (ALengthUTF16 > Length(FDocument.VisibleText) - AStartUTF16) or
     ((ALengthUTF16 = 0) and (Length(ABreaks) = 0)) then
    Exit;
  EndOffset := AStartUTF16 + ALengthUTF16;
  for Index := 0 to High(ABreaks) do
    if (ABreaks[Index].OffsetUTF16 < AStartUTF16) or
       (ABreaks[Index].OffsetUTF16 > EndOffset) then
      Exit;

  Snapshot := nil;
  NewTrace := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    for Index := 0 to High(ABreaks) do
      if not WorkingDocument.RemoveBreak(ABreaks[Index]) then
        Exit;
    if ALengthUTF16 > 0 then
    begin
      if not WorkingDocument.CanReplaceVisibleText(AStartUTF16,
        ALengthUTF16) then
        Exit;
      WorkingDocument.ReplaceVisibleTextWithAffinity(AStartUTF16,
        ALengthUTF16, '', False);
      NewTrace.ApplyReplacement(AStartUTF16, ALengthUTF16, '');
    end;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if UpdatedRaw = Snapshot.Raw then
      Exit;

    AdoptDocument(UpdatedRaw, WorkingDocument);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.ReplaceSelectionWithBreak(
  AStartUTF16, ALengthUTF16: Integer;
  const ABreaks: TTheWordBreakIdentityArray;
  AKind: TTheWordBreakKind; AAfterAnchors: Boolean): Boolean;
var
  InsertBefore: TTheWordBreakIdentity;
begin
  FillChar(InsertBefore, SizeOf(InsertBefore), 0);
  Result := ReplaceSelectionWithBreakInternal(AStartUTF16,
    ALengthUTF16, ABreaks, AKind, AAfterAnchors, False, InsertBefore);
end;

function TTheWordWysiwygSessionV2.ReplaceSelectionWithBreak(
  AStartUTF16, ALengthUTF16: Integer;
  const ABreaks: TTheWordBreakIdentityArray;
  AKind: TTheWordBreakKind; const AInsertBefore:
  TTheWordBreakIdentity): Boolean;
begin
  Result := ReplaceSelectionWithBreakInternal(AStartUTF16,
    ALengthUTF16, ABreaks, AKind, True, True, AInsertBefore);
end;

function TTheWordWysiwygSessionV2.ReplaceSelectionWithBreakInternal(
  AStartUTF16, ALengthUTF16: Integer;
  const ABreaks: TTheWordBreakIdentityArray;
  AKind: TTheWordBreakKind; AAfterAnchors,
  AHasInsertBefore: Boolean; const AInsertBefore:
  TTheWordBreakIdentity): Boolean;
var
  EndOffset, Index: Integer;
  InsertedBreak: TTheWordBreakIdentity;
  NewTrace: TVerseEditTrace;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if (AStartUTF16 < 0) or (ALengthUTF16 < 0) or
     (AStartUTF16 > Length(FDocument.VisibleText)) or
     (ALengthUTF16 > Length(FDocument.VisibleText) - AStartUTF16) then
    Exit;
  EndOffset := AStartUTF16 + ALengthUTF16;
  for Index := 0 to High(ABreaks) do
    if (ABreaks[Index].OffsetUTF16 < AStartUTF16) or
       (ABreaks[Index].OffsetUTF16 > EndOffset) then
      Exit;

  Snapshot := nil;
  NewTrace := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    for Index := 0 to High(ABreaks) do
      if not WorkingDocument.RemoveBreak(ABreaks[Index]) then
        Exit;
    if ALengthUTF16 > 0 then
    begin
      if not WorkingDocument.CanReplaceVisibleText(AStartUTF16,
        ALengthUTF16) then
        Exit;
      WorkingDocument.ReplaceVisibleTextWithAffinity(AStartUTF16,
        ALengthUTF16, '', False);
      NewTrace.ApplyReplacement(AStartUTF16, ALengthUTF16, '');
    end;
    if AHasInsertBefore then
      Result := WorkingDocument.InsertBreak(AStartUTF16, AKind,
        AInsertBefore, InsertedBreak)
    else
      Result := WorkingDocument.InsertBreak(AStartUTF16, AKind,
        AAfterAnchors, InsertedBreak);
    if not Result then Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if UpdatedRaw = Snapshot.Raw then
      Exit;

    AdoptDocument(UpdatedRaw, WorkingDocument);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.RemoveBreak(
  const AIdentity: TTheWordBreakIdentity): Boolean;
var
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  Snapshot := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    if not WorkingDocument.RemoveBreak(AIdentity) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if UpdatedRaw = Snapshot.Raw then
      Exit;

    AdoptDocument(UpdatedRaw, WorkingDocument);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.InsertParagraphMark(AStartUTF16,
  ALengthUTF16: Integer): Boolean;
var
  NewTrace: TVerseEditTrace;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if (AStartUTF16 < 0) or (ALengthUTF16 < 0) or
     (AStartUTF16 > Length(FDocument.VisibleText)) or
     (ALengthUTF16 > Length(FDocument.VisibleText) - AStartUTF16) then
    Exit;
  if (ALengthUTF16 > 0) and
     not CanReplaceText(AStartUTF16, ALengthUTF16) then
    Exit;

  Snapshot := nil;
  NewTrace := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    if ALengthUTF16 > 0 then
    begin
      WorkingDocument.ReplaceVisibleText(AStartUTF16, ALengthUTF16, '');
      NewTrace.ApplyReplacement(AStartUTF16, ALengthUTF16, '');
    end;
    if not WorkingDocument.InsertParagraphMark(AStartUTF16) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if UpdatedRaw = Snapshot.Raw then
      Exit;

    AdoptDocument(UpdatedRaw, WorkingDocument);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.DeleteSelectionWithParagraphMarks(
  AStartUTF16, ALengthUTF16: Integer;
  const AParagraphMarkOffsets: array of Integer): Boolean;
var
  EndOffset, Index: Integer;
  NewTrace: TVerseEditTrace;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if (AStartUTF16 < 0) or (ALengthUTF16 < 0) or
     (AStartUTF16 > Length(FDocument.VisibleText)) or
     (ALengthUTF16 > Length(FDocument.VisibleText) - AStartUTF16) or
     ((ALengthUTF16 = 0) and (Length(AParagraphMarkOffsets) = 0)) then
    Exit;
  EndOffset := AStartUTF16 + ALengthUTF16;
  for Index := 0 to High(AParagraphMarkOffsets) do
    if (AParagraphMarkOffsets[Index] < AStartUTF16) or
       (AParagraphMarkOffsets[Index] > EndOffset) then
      Exit;

  Snapshot := nil;
  NewTrace := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    NewTrace := FTrace.Clone;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    for Index := 0 to High(AParagraphMarkOffsets) do
      if not WorkingDocument.RemoveParagraphMark(
        AParagraphMarkOffsets[Index], False) then
        Exit;
    if ALengthUTF16 > 0 then
    begin
      if not WorkingDocument.CanReplaceVisibleText(AStartUTF16,
        ALengthUTF16) then
        Exit;
      WorkingDocument.ReplaceVisibleText(AStartUTF16, ALengthUTF16, '');
      NewTrace.ApplyReplacement(AStartUTF16, ALengthUTF16, '');
    end;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if UpdatedRaw = Snapshot.Raw then
      Exit;

    AdoptDocument(UpdatedRaw, WorkingDocument);
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    NewTrace.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.RemoveParagraphMark(
  AOffsetUTF16: Integer; AFromEnd: Boolean): Boolean;
var
  Snapshot: TTheWordWysiwygSessionSnapshot;
  UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  Snapshot := nil;
  WorkingDocument := nil;
  try
    Snapshot := CaptureSnapshot;
    WorkingDocument := FCodec.DecodeVerse(Snapshot.Raw);
    if not WorkingDocument.RemoveParagraphMark(AOffsetUTF16,
      AFromEnd) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if UpdatedRaw = Snapshot.Raw then
      Exit;

    AdoptDocument(UpdatedRaw, WorkingDocument);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    WorkingDocument.Free;
    Snapshot.Free;
  end;
end;

function TTheWordWysiwygSessionV2.ReplaceRawText(
  const ARaw: UnicodeString): Boolean;
var
  NewDocument: TTheWordVerseDocument;
  NewTrace: TVerseEditTrace;
  Snapshot: TTheWordWysiwygSessionSnapshot;
  SnapshotRaw: UnicodeString;
begin
  SnapshotRaw := GetRawText;
  Result := ARaw <> SnapshotRaw;
  if not Result then
    Exit;
  NewDocument := FCodec.DecodeVerse(ARaw);
  Snapshot := nil;
  NewTrace := nil;
  try
    NewTrace := FTrace.Clone;
    NewTrace.ApplyRawVisibleChange(NewDocument.VisibleText);
    Snapshot := CaptureSnapshot;
    FDocument.Free;
    FDocument := NewDocument;
    NewDocument := nil;
    FRaw := ARaw;
    FTrace.Free;
    FTrace := NewTrace;
    NewTrace := nil;
    Inc(FRevision);
    PushUndo(Snapshot);
    Snapshot := nil;
  finally
    Snapshot.Free;
    NewTrace.Free;
    NewDocument.Free;
  end;
end;

function TTheWordWysiwygSessionV2.Undo: Boolean;
begin
  Result := CanUndo;
  if Result then
    RestoreFromStack(FUndo, FRedo);
end;

function TTheWordWysiwygSessionV2.Redo: Boolean;
begin
  Result := CanRedo;
  if Result then
    RestoreFromStack(FRedo, FUndo);
end;

function TTheWordWysiwygSessionV2.RejectLastChange: Boolean;
begin
  Result := Undo;
  if Result then
    FRedo.Clear;
end;

procedure TTheWordWysiwygSessionV2.Cancel;
begin
  ReplaceDocument(FOriginalRaw);
  FTrace.ResetBaseline(FDocument.VisibleText, FTrace.TokenizerVersion);
  FUndo.Clear;
  FRedo.Clear;
end;

procedure TTheWordWysiwygSessionV2.AcceptChanges;
begin
  FOriginalRaw := GetRawText;
  FTrace.ResetBaseline(FDocument.VisibleText, FTrace.TokenizerVersion);
  FUndo.Clear;
  FRedo.Clear;
end;

end.
