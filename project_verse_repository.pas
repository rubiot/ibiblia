unit project_verse_repository;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, ai_batch_work_item, project_persistence, rf_note_scanner;

type
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
    function CountAutomaticRFNotesBefore(const ReferenceID: string;
      TextIndex: Integer): Integer;
  end;

implementation

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

function TProjectVerseRepository.CountAutomaticRFNotesBefore(
  const ReferenceID: string; TextIndex: Integer): Integer;
var
  AfterScrollHandler, BeforeScrollHandler: TDataSetNotifyEvent;
  BookID, ChapterNumber, PreviousVerse, VerseNumber: Integer;
  PairTable: TDataSet;
  Parts: TStringArray;
  PreviousReference, SavedReference: string;
begin
  RequirePersistence;
  PairTable := FPersistence.PairsTable;
  if not Assigned(PairTable) or not PairTable.Active then
    raise Exception.Create('Project pairs table is not open');
  if (TextIndex < 0) or (TextIndex >= PairTable.FieldCount) then
    raise Exception.Create(
      'Project verse repository text index is not attached');

  Parts := ReferenceID.Split(',');
  if (Length(Parts) <> 3) or
     not TryStrToInt(Parts[0], BookID) or
     not TryStrToInt(Parts[1], ChapterNumber) or
     not TryStrToInt(Parts[2], VerseNumber) or
     (BookID <= 0) or (ChapterNumber <= 0) or (VerseNumber <= 0) then
    raise Exception.CreateFmt('Invalid project reference [%s]', [ReferenceID]);

  Result := 0;
  if VerseNumber = 1 then
    Exit;

  SavedReference := FPersistence.CurrentPairId;
  if SavedReference.IsEmpty then
    raise Exception.Create('Project has no current Verse Pair');

  BeforeScrollHandler := PairTable.BeforeScroll;
  AfterScrollHandler := PairTable.AfterScroll;
  PairTable.DisableControls;
  PairTable.BeforeScroll := nil;
  PairTable.AfterScroll := nil;
  try
    try
      for PreviousVerse := 1 to VerseNumber - 1 do
      begin
        PreviousReference := Format('%d,%d,%d',
          [BookID, ChapterNumber, PreviousVerse]);
        if FPersistence.LocateReference(PreviousReference) then
          Inc(Result, CountAutomaticRFNotes(UnicodeString(
            FPersistence.CurrentText(TextIndex))));
      end;
    finally
      if (FPersistence.CurrentPairId <> SavedReference) and
         not FPersistence.LocateReference(SavedReference) then
        raise Exception.CreateFmt(
          'Unable to restore current project reference [%s]',
          [SavedReference]);
    end;
  finally
    PairTable.BeforeScroll := BeforeScrollHandler;
    PairTable.AfterScroll := AfterScrollHandler;
    PairTable.EnableControls;
  end;
end;

end.