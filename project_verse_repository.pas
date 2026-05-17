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
    function ReadCurrentVerseData(const ReferenceID, ReferenceName: string): TVerseData;
    function ReadVerseData(const ReferenceID, ReferenceName: string): TVerseData;
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

end.