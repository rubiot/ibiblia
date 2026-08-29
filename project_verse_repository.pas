unit project_verse_repository;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, sqlite3conn, sqldb, ai_batch_work_item,
  project_persistence, rf_note_scanner;

type
  TAutomaticRFNoteCountArray = array of Integer;
  TChapterTextArray = array of string;

  TChapterSnapshotEntry = record
    ReferenceID: string;
    Texts: TChapterTextArray;
    Associations: string;
    AutomaticNoteStarts: TAutomaticRFNoteCountArray;
  end;

  TChapterSnapshot = class
  private
    FBookID: Integer;
    FChapter: Integer;
    FContentRevision: QWord;
    FTextIndexes: array of Integer;
    FEntries: array of TChapterSnapshotEntry;
    FVerseToEntry: array of Integer;
    procedure AppendEntry(const AEntry: TChapterSnapshotEntry);
    procedure RebuildAutomaticNoteStarts(TextSlot: Integer);
  public
    constructor Create(ABookID, AChapter: Integer; AContentRevision: QWord;
      const ATextIndexes: array of Integer);
    function Matches(ABookID, AChapter: Integer; AContentRevision: QWord;
      const ATextIndexes: array of Integer): Boolean;
    function TextSlotForField(TextIndex: Integer): Integer;
    function EntryIndexForReference(const ReferenceID: string): Integer;
    function AutomaticNoteStart(const ReferenceID: string;
      TextIndex: Integer): Integer;
    function UpdateEntry(const ReferenceID: string;
      const TextIndexes: array of Integer;
      const TextValues: array of string; const Associations: string;
      AContentRevision: QWord): Boolean;
    function Count: Integer;
    function Entry(Index: Integer): TChapterSnapshotEntry;
    property BookID: Integer read FBookID;
    property Chapter: Integer read FChapter;
    property ContentRevision: QWord read FContentRevision;
  end;

  TProjectVerseRepository = class
  private
    FPersistence: TProjectPersistence;
    FOriginalTextIndex: Integer;
    FTranslatedTextIndex: Integer;
    procedure RequirePersistence;
    procedure RequireTextIndexes;
  public
    constructor Create(APersistence: TProjectPersistence; AOriginalTextIndex: Integer = -1;
      ATranslatedTextIndex: Integer = -1);
    procedure Attach(APersistence: TProjectPersistence; AOriginalTextIndex: Integer = -1;
      ATranslatedTextIndex: Integer = -1);
    procedure First;
    procedure Next;
    function EOF: Boolean;
    function CurrentReferenceID: string;
    function CurrentText(TextIndex: Integer): string;
    function HasCurrentAssociations: Boolean;
    procedure ClearCurrentAssociations(ResetStatus: Boolean = False);
    function ReadCurrentVerseData: TVerseData; overload;
    function ReadCurrentVerseData(const ReferenceID, ReferenceName: string): TVerseData;
    function ReadVerseData(const ReferenceID, ReferenceName: string): TVerseData;
    function TryReadVerseData(const ReferenceID, ReferenceName: string;
      out VerseData: TVerseData): Boolean;
    function TryReadText(const ReferenceID: string; TextIndex: Integer;
      var TextValue: string): Boolean;
    function LoadChapterSnapshot(BookID, Chapter: Integer;
      const TextIndexes: array of Integer): TChapterSnapshot;
    function CountAutomaticRFNotesBeforeMany(const ReferenceID: string;
      const TextIndexes: array of Integer): TAutomaticRFNoteCountArray;
    function CountAutomaticRFNotesBefore(const ReferenceID: string;
      TextIndex: Integer): Integer;
  end;

implementation

uses
  performance_debug;

function ReferenceVerseNumber(const ReferenceID: string; out BookID,
  Chapter, VerseNumber: Integer): Boolean;
var
  Parts: TStringArray;
begin
  Parts := ReferenceID.Split(',');
  Result := (Length(Parts) = 3) and TryStrToInt(Parts[0], BookID) and
    TryStrToInt(Parts[1], Chapter) and TryStrToInt(Parts[2], VerseNumber) and
    (BookID > 0) and (Chapter > 0) and (VerseNumber > 0);
end;

{ TChapterSnapshot }

constructor TChapterSnapshot.Create(ABookID, AChapter: Integer;
  AContentRevision: QWord; const ATextIndexes: array of Integer);
var
  Index: Integer;
begin
  inherited Create;
  FBookID := ABookID;
  FChapter := AChapter;
  FContentRevision := AContentRevision;
  SetLength(FTextIndexes, Length(ATextIndexes));
  for Index := 0 to High(ATextIndexes) do
    FTextIndexes[Index] := ATextIndexes[Index];
end;

procedure TChapterSnapshot.AppendEntry(const AEntry: TChapterSnapshotEntry);
var
  EntryIndex: Integer;
  ParsedBookID: Integer;
  ParsedChapter: Integer;
  VerseNumber: Integer;
  OldLength: Integer;
  Index: Integer;
begin
  EntryIndex := Length(FEntries);
  SetLength(FEntries, EntryIndex + 1);
  FEntries[EntryIndex] := AEntry;
  if not ReferenceVerseNumber(AEntry.ReferenceID, ParsedBookID,
    ParsedChapter, VerseNumber) or (ParsedBookID <> FBookID) or
    (ParsedChapter <> FChapter) then
    Exit;
  if VerseNumber >= Length(FVerseToEntry) then
  begin
    OldLength := Length(FVerseToEntry);
    SetLength(FVerseToEntry, VerseNumber + 1);
    for Index := OldLength to High(FVerseToEntry) do
      FVerseToEntry[Index] := -1;
  end;
  FVerseToEntry[VerseNumber] := EntryIndex;
end;

function TChapterSnapshot.Matches(ABookID, AChapter: Integer;
  AContentRevision: QWord; const ATextIndexes: array of Integer): Boolean;
var
  Index: Integer;
begin
  Result := (FBookID = ABookID) and (FChapter = AChapter) and
    (FContentRevision = AContentRevision) and
    (Length(FTextIndexes) = Length(ATextIndexes));
  if not Result then
    Exit;
  for Index := 0 to High(ATextIndexes) do
    if FTextIndexes[Index] <> ATextIndexes[Index] then
      Exit(False);
end;

function TChapterSnapshot.TextSlotForField(TextIndex: Integer): Integer;
begin
  for Result := 0 to High(FTextIndexes) do
    if FTextIndexes[Result] = TextIndex then
      Exit;
  Result := -1;
end;

function TChapterSnapshot.EntryIndexForReference(
  const ReferenceID: string): Integer;
var
  ParsedBookID: Integer;
  ParsedChapter: Integer;
  VerseNumber: Integer;
begin
  Result := -1;
  if not ReferenceVerseNumber(ReferenceID, ParsedBookID, ParsedChapter,
    VerseNumber) or (ParsedBookID <> FBookID) or
    (ParsedChapter <> FChapter) or (VerseNumber >= Length(FVerseToEntry)) then
    Exit;
  Result := FVerseToEntry[VerseNumber];
end;

function TChapterSnapshot.AutomaticNoteStart(const ReferenceID: string;
  TextIndex: Integer): Integer;
var
  EntryIndex: Integer;
  TextSlot: Integer;
begin
  Result := 0;
  EntryIndex := EntryIndexForReference(ReferenceID);
  TextSlot := TextSlotForField(TextIndex);
  if (EntryIndex < 0) or (TextSlot < 0) or
    (TextSlot >= Length(FEntries[EntryIndex].AutomaticNoteStarts)) then
    Exit;
  Result := FEntries[EntryIndex].AutomaticNoteStarts[TextSlot];
end;

procedure TChapterSnapshot.RebuildAutomaticNoteStarts(TextSlot: Integer);
var
  EntryIndex: Integer;
  RunningCount: Integer;
begin
  RunningCount := 0;
  for EntryIndex := 0 to High(FEntries) do
  begin
    FEntries[EntryIndex].AutomaticNoteStarts[TextSlot] := RunningCount;
    Inc(RunningCount, CountAutomaticRFNotes(
      UnicodeString(FEntries[EntryIndex].Texts[TextSlot])));
  end;
end;

function TChapterSnapshot.UpdateEntry(const ReferenceID: string;
  const TextIndexes: array of Integer; const TextValues: array of string;
  const Associations: string; AContentRevision: QWord): Boolean;
var
  ChangedSlots: array of Boolean;
  EntryIndex: Integer;
  Index: Integer;
  Slot: Integer;
  TextSlots: array of Integer;
begin
  Result := False;
  if Length(TextIndexes) <> Length(TextValues) then
    Exit;
  EntryIndex := EntryIndexForReference(ReferenceID);
  if EntryIndex < 0 then
    Exit;

  { Validate every field before changing the snapshot, so a stale field map
    can only invalidate the cache; it can never leave a partially patched
    chapter behind. }
  SetLength(TextSlots, Length(TextIndexes));
  for Index := 0 to High(TextIndexes) do
  begin
    TextSlots[Index] := TextSlotForField(TextIndexes[Index]);
    if TextSlots[Index] < 0 then
      Exit;
  end;

  SetLength(ChangedSlots, Length(FTextIndexes));
  for Index := 0 to High(TextSlots) do
  begin
    Slot := TextSlots[Index];
    if FEntries[EntryIndex].Texts[Slot] <> TextValues[Index] then
    begin
      FEntries[EntryIndex].Texts[Slot] := TextValues[Index];
      ChangedSlots[Slot] := True;
    end;
  end;
  FEntries[EntryIndex].Associations := Associations;
  for Slot := 0 to High(ChangedSlots) do
    if ChangedSlots[Slot] then
      RebuildAutomaticNoteStarts(Slot);
  FContentRevision := AContentRevision;
  Result := True;
end;

function TChapterSnapshot.Count: Integer;
begin
  Result := Length(FEntries);
end;

function TChapterSnapshot.Entry(Index: Integer): TChapterSnapshotEntry;
begin
  if (Index < 0) or (Index >= Length(FEntries)) then
    raise Exception.CreateFmt('Chapter snapshot index out of bounds [%d]',
      [Index]);
  Result := FEntries[Index];
end;

procedure TProjectVerseRepository.RequirePersistence;
begin
  if not Assigned(FPersistence) then
    raise Exception.Create('Project persistence is not attached');
end;

procedure TProjectVerseRepository.RequireTextIndexes;
begin
  if (FOriginalTextIndex < 0) or (FTranslatedTextIndex < 0) then
    raise Exception.Create('Project verse repository text indexes are not attached');
end;

constructor TProjectVerseRepository.Create(APersistence: TProjectPersistence;
  AOriginalTextIndex: Integer; ATranslatedTextIndex: Integer);
begin
  inherited Create;
  Attach(APersistence, AOriginalTextIndex, ATranslatedTextIndex);
end;

procedure TProjectVerseRepository.Attach(APersistence: TProjectPersistence;
  AOriginalTextIndex: Integer; ATranslatedTextIndex: Integer);
begin
  FPersistence := APersistence;
  FOriginalTextIndex := AOriginalTextIndex;
  FTranslatedTextIndex := ATranslatedTextIndex;
end;

procedure TProjectVerseRepository.First;
begin
  RequirePersistence;
  FPersistence.First;
end;

procedure TProjectVerseRepository.Next;
begin
  RequirePersistence;
  FPersistence.Next;
end;

function TProjectVerseRepository.EOF: Boolean;
begin
  RequirePersistence;
  Result := FPersistence.EOF;
end;

function TProjectVerseRepository.CurrentReferenceID: string;
begin
  RequirePersistence;
  Result := FPersistence.CurrentPairId;
end;

function TProjectVerseRepository.CurrentText(TextIndex: Integer): string;
begin
  RequirePersistence;
  if TextIndex < 0 then
    raise Exception.Create('Project verse repository text index is not attached');

  Result := FPersistence.CurrentText(TextIndex);
end;

function TProjectVerseRepository.HasCurrentAssociations: Boolean;
begin
  RequirePersistence;
  Result := not FPersistence.CurrentPairs.IsEmpty;
end;

procedure TProjectVerseRepository.ClearCurrentAssociations(ResetStatus: Boolean);
begin
  RequirePersistence;
  FPersistence.WriteCurrentPairs('');
  if ResetStatus then
    FPersistence.SetCurrentFieldAsInteger('pare_situacao', 0);
end;

function TProjectVerseRepository.ReadCurrentVerseData: TVerseData;
begin
  Result := ReadCurrentVerseData(CurrentReferenceID, '');
end;

function TProjectVerseRepository.ReadCurrentVerseData(const ReferenceID,
  ReferenceName: string): TVerseData;
begin
  RequirePersistence;
  RequireTextIndexes;

  Result.ReferenceID := ReferenceID;
  Result.ReferenceName := ReferenceName;
  Result.OriginalText := FPersistence.CurrentText(FOriginalTextIndex);
  Result.TranslatedText := FPersistence.CurrentText(FTranslatedTextIndex);
  Result.Associations := FPersistence.CurrentPairs;
  Result.Comments := FPersistence.CurrentComments;
  Result.Status := FPersistence.CurrentStatus;
end;

function TProjectVerseRepository.ReadVerseData(const ReferenceID,
  ReferenceName: string): TVerseData;
begin
  RequirePersistence;

  if (FPersistence.CurrentPairId <> ReferenceID) and
     (not FPersistence.LocateReference(ReferenceID)) then
    raise Exception.CreateFmt('Reference not found in project [%s]', [ReferenceID]);

  Result := ReadCurrentVerseData(ReferenceID, ReferenceName);
end;

function TProjectVerseRepository.TryReadVerseData(const ReferenceID,
  ReferenceName: string; out VerseData: TVerseData): Boolean;
begin
  RequirePersistence;

  Result := (FPersistence.CurrentPairId = ReferenceID) or
    FPersistence.LocateReference(ReferenceID);
  if Result then
    VerseData := ReadCurrentVerseData(ReferenceID, ReferenceName);
end;

function TProjectVerseRepository.TryReadText(const ReferenceID: string;
  TextIndex: Integer; var TextValue: string): Boolean;
begin
  RequirePersistence;
  if TextIndex < 0 then
    raise Exception.Create('Project verse repository text index is not attached');

  Result := (FPersistence.CurrentPairId = ReferenceID) or
    FPersistence.LocateReference(ReferenceID);
  if Result then
    TextValue := FPersistence.CurrentText(TextIndex);
end;

function TProjectVerseRepository.LoadChapterSnapshot(BookID, Chapter: Integer;
  const TextIndexes: array of Integer): TChapterSnapshot;
var
  Query: TSQLQuery;
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  PairTable: TDataSet;
  FieldNames: array of string;
  RunningNoteCounts: TAutomaticRFNoteCountArray;
  SnapshotEntry: TChapterSnapshotEntry;
  ReferenceID: string;
  CommittedValue: string;
  TextSlot: Integer;
begin
  RequirePersistence;
  if (BookID <= 0) or (Chapter <= 0) then
    raise Exception.CreateFmt('Invalid project chapter [%d,%d]',
      [BookID, Chapter]);
  PairTable := FPersistence.PairsTable;
  if not Assigned(PairTable) or not PairTable.Active then
    raise Exception.Create('Project pairs table is not open');

  IBibliaPerfIncrement(ipcChapterSnapshotLoads);
  IBibliaPerfIncrement(ipcRFPrefixBuilds);

  SetLength(FieldNames, Length(TextIndexes));
  SetLength(RunningNoteCounts, Length(TextIndexes));
  for TextSlot := 0 to High(TextIndexes) do
  begin
    if (TextIndexes[TextSlot] < 0) or
       (TextIndexes[TextSlot] >= PairTable.FieldCount) then
      raise Exception.Create(
        'Project verse repository text index is not attached');
    FieldNames[TextSlot] := PairTable.Fields[TextIndexes[TextSlot]].FieldName;
  end;

  Result := TChapterSnapshot.Create(BookID, Chapter,
    FPersistence.VerseContentRevision, TextIndexes);
  Query := TProjectPersistence.CreateQueryObject(FPersistence.FileName);
  Connection := TSQLite3Connection(Query.DataBase);
  Transaction := TSQLTransaction(Query.Transaction);
  try
    try
      Query.SQL.Text :=
        'select * from pares where pare_id like :prefix || ''%'' ' +
        'order by cast(substr(pare_id, length(:prefix) + 1) as integer)';
      Query.ParamByName('prefix').AsString := Format('%d,%d,',
        [BookID, Chapter]);
      Query.Open;
      while not Query.EOF do
      begin
        SnapshotEntry := Default(TChapterSnapshotEntry);
        ReferenceID := Query.FieldByName('pare_id').AsString;
        SnapshotEntry.ReferenceID := ReferenceID;
        SetLength(SnapshotEntry.Texts, Length(TextIndexes));
        SetLength(SnapshotEntry.AutomaticNoteStarts, Length(TextIndexes));
        for TextSlot := 0 to High(TextIndexes) do
        begin
          CommittedValue := Query.FieldByName(FieldNames[TextSlot]).AsString;
          SnapshotEntry.Texts[TextSlot] :=
            FPersistence.EffectivePairFieldValue(ReferenceID,
              FieldNames[TextSlot], CommittedValue);
          SnapshotEntry.AutomaticNoteStarts[TextSlot] :=
            RunningNoteCounts[TextSlot];
          Inc(RunningNoteCounts[TextSlot], CountAutomaticRFNotes(
            UnicodeString(SnapshotEntry.Texts[TextSlot])));
        end;
        SnapshotEntry.Associations := FPersistence.EffectivePairFieldValue(
          ReferenceID, 'pare_pares',
          Query.FieldByName('pare_pares').AsString);
        Result.AppendEntry(SnapshotEntry);
        Query.Next;
      end;
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    Query.Close;
    if Transaction.Active then
      Transaction.Rollback;
    Connection.Close;
    Transaction.Free;
    Connection.Free;
    Query.Free;
  end;
end;

function TProjectVerseRepository.CountAutomaticRFNotesBefore(
  const ReferenceID: string; TextIndex: Integer): Integer;
var
  Counts: TAutomaticRFNoteCountArray;
begin
  Counts := CountAutomaticRFNotesBeforeMany(ReferenceID, [TextIndex]);
  if Length(Counts) = 0 then
    Exit(0);
  Result := Counts[0];
end;

function TProjectVerseRepository.CountAutomaticRFNotesBeforeMany(
  const ReferenceID: string;
  const TextIndexes: array of Integer): TAutomaticRFNoteCountArray;
var
  BookID: Integer;
  ChapterNumber: Integer;
  Index: Integer;
  Snapshot: TChapterSnapshot;
  VerseNumber: Integer;
begin
  RequirePersistence;
  SetLength(Result, Length(TextIndexes));
  if not ReferenceVerseNumber(ReferenceID, BookID, ChapterNumber,
    VerseNumber) then
    raise Exception.CreateFmt('Invalid project reference [%s]', [ReferenceID]);
  Snapshot := LoadChapterSnapshot(BookID, ChapterNumber, TextIndexes);
  try
    for Index := 0 to High(TextIndexes) do
      Result[Index] := Snapshot.AutomaticNoteStart(ReferenceID,
        TextIndexes[Index]);
  finally
    Snapshot.Free;
  end;
end;

end.
