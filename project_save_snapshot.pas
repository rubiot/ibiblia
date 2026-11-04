unit project_save_snapshot;

{$mode objfpc}{$H+}

interface

type
  TProjectSaveSnapshotProgressEvent = procedure(Current, Total: Integer;
    const ReferenceName: string) of object;

procedure PrepareProjectSaveSnapshot(const FileName: string;
  OnProgress: TProjectSaveSnapshotProgressEvent = nil);

implementation

uses
  Classes, SysUtils, sqlite3conn, sqldb, project_persistence, project_config,
  project_verse_repository, ai_batch_work_item, verse_render_buffer,
  association_memory_service, SuggestionManager, ONTTokenizer;

type
  TSnapshotProgress = class
  public
    Callback: TProjectSaveSnapshotProgressEvent;
    procedure RebuildProgress(Phase: TAssociationMemoryRebuildPhase;
      Current, Total: Integer);
  end;

procedure TSnapshotProgress.RebuildProgress(
  Phase: TAssociationMemoryRebuildPhase; Current, Total: Integer);
begin
  if Assigned(Callback) then
    Callback(Current, Total, '');
end;

procedure ValidateSnapshot(const FileName: string);
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
begin
  Connection := TSQLite3Connection.Create(nil);
  try
    Transaction := TSQLTransaction.Create(Connection);
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FileName;
    Query := TSQLQuery.Create(Connection);
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Connection.Open;
    Query.SQL.Text := 'pragma integrity_check';
    Query.Open;
    if Query.EOF or (Query.Fields[0].AsString <> 'ok') then
      raise Exception.Create('The saved project failed SQLite integrity validation.');
    Query.Next;
    if not Query.EOF then
      raise Exception.Create('The saved project failed SQLite integrity validation.');
    Query.Close;
    Transaction.Rollback;
    Connection.Close;
  finally
    Connection.Free;
  end;
end;

procedure PrepareProjectSaveSnapshot(const FileName: string;
  OnProgress: TProjectSaveSnapshotProgressEvent);
var
  Persistence: TProjectPersistence;
  Config: TProjectConfig;
  Repository: TProjectVerseRepository;
  RenderBuffer: TVerseRenderBuffer;
  Manager: TSuggestionManager;
  Memory: TAssociationMemoryService;
  Progress: TSnapshotProgress;
  VerseData: TVerseData;
  Current, Total: Integer;
  LastProgress: QWord;
begin
  Persistence := nil;
  Config := nil;
  Repository := nil;
  RenderBuffer := nil;
  Manager := nil;
  Memory := nil;
  Progress := nil;
  try
    Persistence := TProjectPersistence.Create;
    Persistence.Open(FileName);
    Config := TProjectConfig.Create(Persistence);
    Repository := TProjectVerseRepository.Create(Persistence,
      Persistence.PairsTable.FieldByName('pare_texto_origem').Index,
      Persistence.PairsTable.FieldByName('pare_texto_destino').Index);
    RenderBuffer := TVerseRenderBuffer.Create;
    RenderBuffer.SetTokenizerVersion(TTokenizerFactory.NormalizeVersion(
      Config.ReadTokenizerVersion(TTokenizerFactory.LEGACY_VERSION)));
    Manager := TSuggestionManager.Create(FileName);
    Memory := TAssociationMemoryService.Create;
    Memory.Attach(Config, Manager, RenderBuffer);
    Progress := TSnapshotProgress.Create;
    Progress.Callback := OnProgress;
    Manager.OnRebuildProgress := @Progress.RebuildProgress;

    Memory.MarkSourceDirty;
    Manager.Rollback;
    Memory.MarkCacheOutdated;
    Persistence.CommitInfo;
    Memory.MarkSourceRevisionPersisted;
    Memory.BeginRebuild;
    try
      Total := Persistence.PairsTable.RecordCount;
      Current := 0;
      LastProgress := GetTickCount64;
      if Assigned(OnProgress) then
        OnProgress(Current, Total, '');
      Repository.First;
      while not Repository.EOF do
      begin
        VerseData := Repository.ReadCurrentVerseData(
          Repository.CurrentReferenceID,
          Persistence.CurrentFieldAsString('pare_ref'));
        Memory.AddVerse(VerseData.ReferenceID, VerseData.ReferenceName,
          VerseData.OriginalText, VerseData.TranslatedText,
          VerseData.Associations);
        Inc(Current);
        if Assigned(OnProgress) and ((Current = Total) or
          ((Current mod 50) = 0) or (GetTickCount64 - LastProgress >= 150)) then
        begin
          OnProgress(Current, Total, VerseData.ReferenceName);
          LastProgress := GetTickCount64;
        end;
        Repository.Next;
      end;
      Memory.FinishRebuild;
      Manager.Commit;
      Memory.StageBuiltRevision;
      Persistence.CommitInfo;
      Memory.CommitCompleted;
    except
      Memory.AbortRebuild;
      raise;
    end;
  finally
    Memory.Free;
    Manager.Free;
    Progress.Free;
    RenderBuffer.Free;
    Repository.Free;
    Config.Free;
    Persistence.Free;
  end;
  ValidateSnapshot(FileName);
end;

end.
