unit project_verse_repository;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, ai_batch_work_item, project_persistence;

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

end.