unit theword_wysiwyg_session_v2;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, theword_ont_codec_v2;

type
  ETheWordWysiwygSession = class(Exception);

  { The codec document is the single source of truth for both the visual and
    raw surfaces. Snapshots are ONT strings, so undo also restores unknown and
    lexical objects exactly as the codec understands them. }
  TTheWordWysiwygSessionV2 = class
  private
    FCodec: ITheWordOntCodecV2;
    FDocument: TTheWordVerseDocument;
    FOriginalRaw: UnicodeString;
    FUndo: TStringList;
    FRedo: TStringList;
    FRevision: QWord;
    function DecodeSnapshot(const AValue: string): UnicodeString;
    function EncodeSnapshot(const AValue: UnicodeString): string;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCapability: TTheWordLayerCapability;
    function GetDirty: Boolean;
    function GetRawText: UnicodeString;
    function GetChangesAreSemanticOnly: Boolean;
    function GetVisibleText: UnicodeString;
    procedure PushUndo(const ASnapshot: UnicodeString);
    procedure ReplaceDocument(const ARaw: UnicodeString);
    procedure RestoreFromStack(ASource, ADestination: TStringList);
  public
    constructor Create(const ARaw: UnicodeString);
    destructor Destroy; override;
    function ApplySemantic(AKind: TTheWordSemanticKind; AStartUTF16,
      ALengthUTF16: Integer): Boolean;
    function CanApplySemantic(AStartUTF16, ALengthUTF16: Integer): Boolean;
    function CanReplaceText(AStartUTF16, ALengthUTF16: Integer): Boolean;
    function CopySelection(AStartUTF16, ALengthUTF16: Integer;
      out APlainText: UnicodeString; out APayload: string): Boolean;
    function CutSelection(AStartUTF16, ALengthUTF16: Integer;
      out APlainText: UnicodeString; out APayload: string): Boolean;
    function Paste(AStartUTF16, ALengthUTF16: Integer;
      const APayload: string; const APlainText: UnicodeString;
      out AInsertedLengthUTF16: Integer;
      out AUsedPrivatePayload: Boolean): Boolean;
    function ReplaceText(AStartUTF16, ALengthUTF16: Integer;
      const AReplacement: UnicodeString): Boolean;
    function ReplaceRawText(const ARaw: UnicodeString): Boolean;
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

constructor TTheWordWysiwygSessionV2.Create(const ARaw: UnicodeString);
begin
  inherited Create;
  FCodec := TTheWordOntCodecV2.Create;
  FUndo := TStringList.Create;
  FRedo := TStringList.Create;
  FOriginalRaw := ARaw;
  FDocument := FCodec.DecodeVerse(ARaw);
  FRevision := 0;
end;

destructor TTheWordWysiwygSessionV2.Destroy;
begin
  FDocument.Free;
  FRedo.Free;
  FUndo.Free;
  FCodec := nil;
  inherited Destroy;
end;

function TTheWordWysiwygSessionV2.EncodeSnapshot(
  const AValue: UnicodeString): string;
begin
  Result := UTF8Encode(AValue);
end;

function TTheWordWysiwygSessionV2.DecodeSnapshot(
  const AValue: string): UnicodeString;
begin
  Result := UTF8Decode(AValue);
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
  Result := GetRawText <> FOriginalRaw;
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
    nsecStructural
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
    ADocument.OpaqueItemCount + ADocument.StructuralItemCount);
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
  Raw, Snapshot: UnicodeString;
begin
  Result := not TouchesTheWordTitle(FDocument, AStartUTF16,
    ALengthUTF16) and
    CopySelection(AStartUTF16, ALengthUTF16, APlainText, APayload) and
    TTheWordClipboardCodecV2.BuildCutRaw(FDocument, AStartUTF16,
      ALengthUTF16, Raw);
  if not Result then Exit;
  Snapshot := GetRawText;
  if Raw = Snapshot then Exit(False);
  ReplaceDocument(Raw);
  PushUndo(Snapshot);
end;

function TTheWordWysiwygSessionV2.Paste(AStartUTF16,
  ALengthUTF16: Integer; const APayload: string;
  const APlainText: UnicodeString; out AInsertedLengthUTF16: Integer;
  out AUsedPrivatePayload: Boolean): Boolean;
var
  Fragment: TTheWordClipboardFragmentV2;
  Raw, Snapshot: UnicodeString;
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
    Snapshot := GetRawText;
    if Raw = Snapshot then Exit(False);
    ReplaceDocument(Raw);
    PushUndo(Snapshot);
  finally
    Fragment.Free;
  end;
end;

procedure TTheWordWysiwygSessionV2.PushUndo(const ASnapshot: UnicodeString);
begin
  FUndo.Add(EncodeSnapshot(ASnapshot));
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
  ADestination: TStringList);
var
  Snapshot: UnicodeString;
begin
  if ASource.Count = 0 then
    Exit;
  ADestination.Add(EncodeSnapshot(GetRawText));
  Snapshot := DecodeSnapshot(ASource[ASource.Count - 1]);
  ASource.Delete(ASource.Count - 1);
  ReplaceDocument(Snapshot);
end;

function TTheWordWysiwygSessionV2.CanApplySemantic(AStartUTF16,
  ALengthUTF16: Integer): Boolean;
begin
  Result := CanApplyTheWordSemanticMark(FDocument, AStartUTF16,
    ALengthUTF16);
end;

function TTheWordWysiwygSessionV2.CanReplaceText(AStartUTF16,
  ALengthUTF16: Integer): Boolean;
begin
  Result := CanReplaceTheWordText(FDocument, AStartUTF16, ALengthUTF16);
end;

function TTheWordWysiwygSessionV2.ApplySemantic(
  AKind: TTheWordSemanticKind; AStartUTF16, ALengthUTF16: Integer): Boolean;
var
  Snapshot, UpdatedRaw: UnicodeString;
  WorkingDocument: TTheWordVerseDocument;
begin
  Result := False;
  if not CanApplySemantic(AStartUTF16, ALengthUTF16) then
    Exit;
  Snapshot := GetRawText;
  WorkingDocument := FCodec.DecodeVerse(Snapshot);
  try
    if not WorkingDocument.ToggleSemanticMark(AKind, AStartUTF16,
      AStartUTF16 + ALengthUTF16) then
      Exit;
    UpdatedRaw := FCodec.EncodeVerse(WorkingDocument);
    if UpdatedRaw = Snapshot then
      Exit;

    { ReplaceDocument decodes before swapping FDocument, so a codec failure
      cannot leave the live session partially mutated. }
    ReplaceDocument(UpdatedRaw);
    PushUndo(Snapshot);
    Result := True;
  finally
    WorkingDocument.Free;
  end;
end;

function TTheWordWysiwygSessionV2.ReplaceText(AStartUTF16,
  ALengthUTF16: Integer; const AReplacement: UnicodeString): Boolean;
var
  Snapshot: UnicodeString;
begin
  Result := False;
  if not CanReplaceText(AStartUTF16, ALengthUTF16) then
    Exit;
  if (ALengthUTF16 = 0) and (AReplacement = '') then
    Exit;
  Snapshot := GetRawText;
  FDocument.ReplaceVisibleText(AStartUTF16, ALengthUTF16, AReplacement);
  PushUndo(Snapshot);
  Inc(FRevision);
  Result := True;
end;

function TTheWordWysiwygSessionV2.ReplaceRawText(
  const ARaw: UnicodeString): Boolean;
var
  Snapshot: UnicodeString;
begin
  Snapshot := GetRawText;
  Result := ARaw <> Snapshot;
  if not Result then
    Exit;
  ReplaceDocument(ARaw);
  PushUndo(Snapshot);
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
  FUndo.Clear;
  FRedo.Clear;
end;

procedure TTheWordWysiwygSessionV2.AcceptChanges;
begin
  FOriginalRaw := GetRawText;
  FUndo.Clear;
  FRedo.Clear;
end;

end.
