unit theword_wysiwyg_session_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, Contnrs, SysUtils, text_edit_provenance,
  theword_ont_codec_v2;

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
    procedure ReplaceDocument(const ARaw: UnicodeString);
    procedure RestoreFromStack(ASource, ADestination: TFPObjectList);
  public
    constructor Create(const ARaw: UnicodeString;
      ATokenizerVersion: Integer = 0);
    destructor Destroy; override;
    function ApplySemantic(AKind: TTheWordSemanticKind; AStartUTF16,
      ALengthUTF16: Integer): Boolean;
    function CanApplySemantic(AStartUTF16, ALengthUTF16: Integer): Boolean;
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
      out APlainText: UnicodeString; out APayload: string): Boolean;
    function CutSelection(AStartUTF16, ALengthUTF16: Integer;
      out APlainText: UnicodeString; out APayload: string): Boolean;
    function DeleteSelectionWithParagraphMarks(AStartUTF16,
      ALengthUTF16: Integer; const AParagraphMarkOffsets:
      array of Integer): Boolean;
    function InsertParagraphMark(AStartUTF16,
      ALengthUTF16: Integer): Boolean;
    function Paste(AStartUTF16, ALengthUTF16: Integer;
      const APayload: string; const APlainText: UnicodeString;
      out AInsertedLengthUTF16: Integer;
      out AUsedPrivatePayload: Boolean): Boolean;
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
  theword_verse_visual_adapter_v2, theword_clipboard_v2;

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
  FCodec := TTheWordOntCodecV2.Create;
  FUndo := TFPObjectList.Create(True);
  FRedo := TFPObjectList.Create(True);
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
  Result := TTheWordWysiwygSessionSnapshot.Create(GetRawText,
    FTrace.Clone);
end;

function TTheWordWysiwygSessionV2.CloneEditTrace: TVerseEditTrace;
begin
  Result := FTrace.Clone;
end;

function TTheWordWysiwygSessionV2.GetRawText: UnicodeString;
begin
  Result := FCodec.EncodeVerse(FDocument);
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
  Result := (GetRawText <> FOriginalRaw) or
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

function BuildNonSemanticEventSequence(
  ADocument: TTheWordVerseDocument): TNonSemanticEventSequence;
var
  Index, Position: Integer;
begin
  Result := nil;
  if not Assigned(ADocument) then
    Exit;
  SetLength(Result, ADocument.LexicalItemCount +
    ADocument.OpaqueItemCount + ADocument.StructuralItemCount +
    ADocument.TranslatorNoteCount);
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

procedure TTheWordWysiwygSessionV2.PushUndo(
  ASnapshot: TTheWordWysiwygSessionSnapshot);
begin
  if not Assigned(ASnapshot) then
    raise EArgumentNilException.Create('ASnapshot');
  FUndo.Add(ASnapshot);
  FRedo.Clear;
end;

procedure TTheWordWysiwygSessionV2.ReplaceDocument(
  const ARaw: UnicodeString);
var
  Replacement: TTheWordVerseDocument;
begin
  Replacement := FCodec.DecodeVerse(ARaw);
  FDocument.Free;
  FDocument := Replacement;
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

function TTheWordWysiwygSessionV2.CanApplySemantic(AStartUTF16,
  ALengthUTF16: Integer): Boolean;
begin
  Result := CanApplyTheWordSemanticMark(FDocument, AStartUTF16,
    ALengthUTF16);
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
    ReplaceDocument(UpdatedRaw);
    PushUndo(Snapshot);
    Snapshot := nil;
    Result := True;
  finally
    Snapshot.Free;
    WorkingDocument.Free;
  end;
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

    ReplaceDocument(UpdatedRaw);
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
