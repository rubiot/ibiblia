unit associationmemoryservicetest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, fpcunit, testregistry, sqlite3conn, sqldb,
  SuggestionManager, project_persistence, project_config,
  Syntagm, Verse, verse_render_buffer, association_memory_service,
  current_verse_workspace, text_edit_provenance, BibleScope, Project,
  project_database_bootstrap;

type
  TProjectReadyOccurrenceRebuildProbe = class(TProgressBar)
  private
    FAttempted: Boolean;
    FCallCount: Integer;
    FErrorMessage: string;
    FMatches: TTranslationPairMatchArray;
    FPage: TTranslationOccurrencePage;
    FProject: TProject;
    FReady: Boolean;
  public
    procedure Update; override;
  end;

  TAssociationMemoryServiceTest = class(TTestCase)
  private
    FFileName: string;
    FManagerConnection: TSQLite3Connection;
    FManagerTransaction: TSQLTransaction;
    FPersistence: TProjectPersistence;
    FConfig: TProjectConfig;
    FManager: TSuggestionManager;
    FRenderBuffer: TVerseRenderBuffer;
    FService: TAssociationMemoryService;
    procedure CreateDatabase;
    procedure OpenServices;
    procedure CloseServices;
    procedure CreateProjectOccurrenceFixture(const AFileName: string;
      AVerseCount: Integer);
    function ExpectedPair: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CommitRollbackReopenAndFailedRebuildRemainConsistent;
    procedure MatchingLegacyRevisionsWithoutCacheVersionRequireRebuild;
    procedure MatchingV3RevisionsRequireRebuildAndV4IsAccepted;
    procedure IncrementalReplayValidatesOnlyAffectedPairs;
    procedure StableTopologyTextReplacementSurvivesCommitAndReopen;
    procedure RevertedStableTopologyEditSurvivesCacheReplayAndReopen;
    procedure WorkspaceIndexShiftKeepsPersistenceAndMemoryInvariant;
    procedure NonCanonicalAssociationOrderIsAccepted;
    procedure ExtractedPairDataPreservesExactDestinationIndexes;
    procedure RebuildSkipsInvalidAssociations;
    procedure RenderBufferReuseReleasesParsedSyntagmLists;
    procedure ProjectReadyOccurrencesHonorLimitAndGlobalTotals;
    procedure ProjectReadyOccurrencesNeverRebuildUnavailableMemory;
  end;

implementation

{ TProjectReadyOccurrenceRebuildProbe }

procedure TProjectReadyOccurrenceRebuildProbe.Update;
begin
  if not FAttempted and Assigned(FProject) and
     (Style = pbstMarquee) then
  begin
    FAttempted := True;
    Inc(FCallCount);
    FReady := FProject.TryGetReadyTranslationMemoryOccurrences(
      FMatches, 10, FPage, FErrorMessage);
  end;
  inherited Update;
end;

procedure TAssociationMemoryServiceTest.CreateProjectOccurrenceFixture(
  const AFileName: string; AVerseCount: Integer);
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(AFileName, etNT,
    AVerseCount);
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := AFileName;
    Connection.Open;
    Transaction.StartTransaction;
    try
      Connection.ExecuteDirect(
        'update pares set pare_texto_origem = ''source<WG1>'', ' +
        'pare_texto_destino = ''word'', ' +
        'pare_pares = ''<par a="0" b="0">''');
      Transaction.Commit;
    except
      if Transaction.Active then
        Transaction.Rollback;
      raise;
    end;
  finally
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TAssociationMemoryServiceTest.CreateDatabase;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FFileName;
    Connection.Open;
    Transaction.StartTransaction;
    Connection.ExecuteDirect('create table traducoes (' +
      'trad_lori_id integer not null, trad_ldes_id integer not null, ' +
      'trad_ocorrencias integer)');
    Connection.ExecuteDirect('create table sintagmas_locucoes_origem (' +
      'slor_lori_id integer not null, slor_sori_id integer not null, ' +
      'slor_ordem integer not null)');
    Connection.ExecuteDirect('create table sintagmas_locucoes_destino (' +
      'slde_ldes_id integer not null, slde_sdes_id integer not null, ' +
      'slde_ordem integer not null)');
    Connection.ExecuteDirect('create table locucoes_origem (' +
      'lori_id integer not null primary key, lori_qtde_sintagmas integer, ' +
      'lori_locucao varchar(200) unique)');
    Connection.ExecuteDirect('create table locucoes_destino (' +
      'ldes_id integer not null primary key, ldes_qtde_sintagmas integer, ' +
      'ldes_locucao varchar(200) unique)');
    Connection.ExecuteDirect('create table info (' +
      'id text primary key, valor text)');
    Connection.ExecuteDirect('create table pares (' +
      'pare_id text primary key, pare_ref text, pare_texto_origem text, ' +
      'pare_texto_destino text, pare_texto_consulta1 text, ' +
      'pare_texto_consulta2 text, pare_pares text, pare_situacao number, ' +
      'pare_comentarios text)');
    Connection.ExecuteDirect(
      'create unique index trad_uk on traducoes(trad_lori_id, trad_ldes_id)');
    Connection.ExecuteDirect(
      'create unique index slor_uk on sintagmas_locucoes_origem' +
      '(slor_lori_id, slor_sori_id, slor_ordem)');
    Connection.ExecuteDirect(
      'create unique index slde_uk on sintagmas_locucoes_destino' +
      '(slde_ldes_id, slde_sdes_id, slde_ordem)');
    Connection.ExecuteDirect('insert into pares values (' +
      '''1,1,1'', ''Gen 1:1'', ''source<WG1>'', ''target'', '''', '''', ' +
      '''<par a="0" b="0">'', 0, '''')');
    Connection.ExecuteDirect('insert into pares values (' +
      '''1,1,2'', ''Gen 1:2'', ''source'', ''target'', '''', '''', ' +
      '''<par a="0" b="0">'', 0, '''')');
    Transaction.Commit;
  finally
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TAssociationMemoryServiceTest.OpenServices;
begin
  FPersistence := TProjectPersistence.Create;
  FPersistence.Open(FFileName);
  FConfig := TProjectConfig.Create(FPersistence);

  FManagerConnection := TSQLite3Connection.Create(nil);
  FManagerTransaction := TSQLTransaction.Create(nil);
  FManagerConnection.Transaction := FManagerTransaction;
  FManagerConnection.DatabaseName := FFileName;
  FManagerConnection.Open;
  FManagerTransaction.StartTransaction;
  FManager := TSuggestionManager.Create(nil, FManagerConnection);

  FRenderBuffer := TVerseRenderBuffer.Create;
  FService := TAssociationMemoryService.Create;
  FService.Attach(FConfig, FManager, FRenderBuffer);
end;

procedure TAssociationMemoryServiceTest.CloseServices;
begin
  FreeAndNil(FService);
  FreeAndNil(FRenderBuffer);
  FreeAndNil(FManager);
  if Assigned(FManagerTransaction) and FManagerTransaction.Active then
    FManagerTransaction.Rollback;
  if Assigned(FManagerConnection) then
    FManagerConnection.Close;
  FreeAndNil(FManagerConnection);
  FreeAndNil(FManagerTransaction);
  FreeAndNil(FConfig);
  FreeAndNil(FPersistence);
end;

function TAssociationMemoryServiceTest.ExpectedPair: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('<wg1>');
  Result.Add('target');
end;

procedure TAssociationMemoryServiceTest.SetUp;
begin
  inherited SetUp;
  FFileName := GetTempFileName(GetTempDir, 'ibm');
  CreateDatabase;
  OpenServices;
end;

procedure TAssociationMemoryServiceTest.TearDown;
begin
  CloseServices;
  if FileExists(FFileName) then
    DeleteFile(FFileName);
  inherited TearDown;
end;

procedure TAssociationMemoryServiceTest.
  CommitRollbackReopenAndFailedRebuildRemainConsistent;
var
  Expected: TStringList;
  ErrorMessage: string;
  FailureReported: Boolean;
  CacheValid: Boolean;
begin
  Expected := ExpectedPair;
  try
    AssertTrue('A project without revisions must be rebuilt',
      FService.NeedsRebuild);

    FService.MarkSourceDirty;
    FPersistence.CommitInfo;
    FPersistence.CommitPairs;
    AssertTrue('Source revision must be present after the canonical commit',
      FPersistence.InfoTable.Locate('id',
      ASSOCIATION_MEMORY_SOURCE_REVISION_KEY, []));
    AssertEquals('1', FPersistence.InfoTable.FieldByName('valor').AsString);
    FService.MarkSourceRevisionPersisted;
    FService.BeginRebuild;
    FService.AddVerse('1,1,1', 'Gen 1:1', 'source<WG1>', 'target',
      '<par a="0" b="0">');
    FService.FinishRebuild;
    AssertTrue('A completed rebuild must certify the cache',
      FService.CacheValidated);
    AssertTrue(FManager.ValidatePairs(Expected, ErrorMessage));

    AssertTrue('A completed rebuild over durable canonical data must be ' +
      'committed without replay', FService.CanCommitCompletedRebuild);
    FManager.Commit;
    AssertTrue(FService.StageBuiltRevision);
    FPersistence.CommitInfo;
    AssertTrue('Source revision must remain present after publishing cache',
      FPersistence.InfoTable.Locate('id',
      ASSOCIATION_MEMORY_SOURCE_REVISION_KEY, []));
    AssertEquals('1', FPersistence.InfoTable.FieldByName('valor').AsString);
    FService.CommitCompleted;
    AssertTrue(FService.RevisionsMatch);

    CloseServices;
    OpenServices;
    AssertEquals('Published cache version must survive reopen',
      ASSOCIATION_MEMORY_CACHE_VERSION,
      FConfig.ReadString(ASSOCIATION_MEMORY_CACHE_VERSION_KEY, ''));
    AssertFalse(Format('Matching durable revisions must survive reopen ' +
      '(source=%s, built=%s, version=%s)', [FService.SourceRevision,
      FService.BuiltRevision, FConfig.ReadString(
      ASSOCIATION_MEMORY_CACHE_VERSION_KEY, '')]), FService.NeedsRebuild);
    AssertTrue('Matching published revisions must retain the validation ' +
      'certificate on reopen', FService.CacheValidated);
    FService.ValidateCurrentCache;
    AssertTrue(FManager.ValidatePairs(Expected, ErrorMessage));

    FService.ApplyAddPair('G2', 'temporary');
    AssertTrue(FService.PendingDirty);
    FPersistence.Rollback;
    FManager.Rollback;
    FService.ReloadRevisionState;
    AssertFalse(Format(
      'Rollback must restore the durable revision state (source=%s, built=%s)',
      [FService.SourceRevision, FService.BuiltRevision]),
      FService.NeedsRebuild);
    AssertTrue('Rollback to matching durable revisions must restore the ' +
      'validation certificate', FService.CacheValidated);
    AssertTrue(FManager.ValidatePairs(Expected, ErrorMessage));

    // The validation query opens a read transaction on the independent
    // Association-memory connection. Release it before publishing canonical
    // project rows through the persistence connection.
    FManager.Rollback;
    FService.MarkSourceDirty;
    FPersistence.CommitInfo;
    FPersistence.CommitPairs;
    FService.MarkSourceRevisionPersisted;
    FService.BeginRebuild;
    FailureReported := False;
    try
      FService.AddVerse('1,1,1', 'Gen 1:1', 'source<WG1>', 'target',
        '<par a="0" b="0">');
      FManagerConnection.ExecuteDirect(
        'insert into traducoes values (1, 1, 99)');
      FService.FinishRebuild;
    except
      on E: Exception do
      begin
        FailureReported := True;
        if not FService.NeedsRebuild then
          FService.AbortRebuild;
      end;
    end;
    AssertTrue('Database failures must still abort the rebuild',
      FailureReported);
    AssertTrue('A failed rebuild must leave memory marked as outdated',
      FService.NeedsRebuild);
    AssertFalse('A failed rebuild must discard the validation certificate',
      FService.CacheValidated);
    CacheValid := FManager.ValidatePairs(Expected, ErrorMessage);
    AssertTrue('The previous cache must survive failed rebuild atomically: ' +
      ErrorMessage, CacheValid);

    FService.ApplyAddPair('G2', 'must-not-enter-stale-cache');
    AssertFalse('An outdated cache cannot accept replayable deltas',
      FService.CanReplayPendingCache);
    AssertTrue('Incremental edits after a failed rebuild must not mutate the ' +
      'old cache', FManager.ValidatePairs(Expected, ErrorMessage));
    FPersistence.Commit;
    FService.CommitSourceOutdated;
    AssertFalse(FService.PendingDirty);
    AssertTrue('Canonical data may be saved while the failed cache remains ' +
      'explicitly outdated', FService.NeedsRebuild);
    AssertFalse(FService.CacheValidated);
  finally
    Expected.Free;
  end;
end;

procedure TAssociationMemoryServiceTest.
  IncrementalReplayValidatesOnlyAffectedPairs;
begin
  FService.MarkSourceDirty;
  FPersistence.CommitInfo;
  FPersistence.CommitPairs;
  FService.MarkSourceRevisionPersisted;
  FService.BeginRebuild;
  FService.AddVerse('1,1,1', 'Gen 1:1', 'source<WG1>', 'target',
    '<par a="0" b="0">');
  FService.FinishRebuild;
  FManager.Commit;
  AssertTrue(FService.StageBuiltRevision);
  FPersistence.CommitInfo;
  FService.CommitCompleted;

  FService.ApplyAddPair('G2', 'second');
  AssertTrue(FService.CanReplayPendingCache);

  // Project.Commit first rolls the speculative cache update back, persists
  // the canonical source revision, and then replays its small recorded delta.
  FManager.Rollback;
  FPersistence.CommitInfo;
  FPersistence.CommitPairs;
  FService.MarkSourceRevisionPersisted;
  FService.MarkCacheOutdated;
  FService.ReplayPendingCache;

  AssertTrue('A successful delta replay must certify the resulting cache',
    FService.CacheValidated);
  AssertEquals('The affected pair must have its exact final multiplicity', 1,
    FManager.GetPairOccurrenceCount('G2', 'second'));
  AssertEquals('Unrelated pairs must remain untouched', 1,
    FManager.GetPairOccurrenceCount('<wg1>', 'target'));
end;

procedure TAssociationMemoryServiceTest.
  StableTopologyTextReplacementSurvivesCommitAndReopen;
var
  Associations: string;
  DestinationTextFieldIndex: Integer;
  DestinationVerse: TVerse;
  NewSourcePhrase: string;
  NewTargetPhrase: string;
  OldSourcePhrase: string;
  OldTargetPhrase: string;
  PairList: TStringList;
  SourceVerse: TVerse;
  Workspace: TCurrentVerseWorkspace;
begin
  AssertTrue(FPersistence.LocateReference('1,1,1'));
  DestinationTextFieldIndex := FPersistence.PairsTable.FieldByName(
    'pare_texto_destino').Index;

  // Publish a version-4 baseline whose canonical target is the old wording.
  FPersistence.SetCurrentText(DestinationTextFieldIndex, 'mister');
  FPersistence.PostCurrentEdit;
  FService.MarkSourceDirty;
  FPersistence.CommitInfo;
  FPersistence.CommitPairs;
  FService.MarkSourceRevisionPersisted;
  FService.BeginRebuild;
  FService.AddVerse('1,1,1', 'Gen 1:1', 'source<WG1>', 'mister',
    '<par a="0" b="0">');
  FService.FinishRebuild;
  FManager.Commit;
  AssertTrue(FService.StageBuiltRevision);
  FPersistence.CommitInfo;
  FService.CommitCompleted;
  AssertEquals(ASSOCIATION_MEMORY_CACHE_VERSION,
    FConfig.ReadString(ASSOCIATION_MEMORY_CACHE_VERSION_KEY, ''));

  Workspace := TCurrentVerseWorkspace.Create;
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.PairedVerse := DestinationVerse;
    DestinationVerse.PairedVerse := SourceVerse;
    SourceVerse.Text := 'source<WG1>';
    DestinationVerse.Text := 'mister';
    Associations := FPersistence.CurrentPairs;
    Workspace.ApplyLoadedAssociations(SourceVerse, Associations);

    PairList := SourceVerse.GetPairsList(plMetaData);
    try
      AssertEquals(2, PairList.Count);
      OldSourcePhrase := PairList[0];
      OldTargetPhrase := PairList[1];
    finally
      PairList.Free;
    end;
    AssertEquals(1, FManager.GetPairOccurrenceCount(OldSourcePhrase,
      OldTargetPhrase));

    DestinationVerse.Text := 'necessario';
    SourceVerse.Pairs := Associations;
    AssertEquals('A lexical replacement must preserve association topology',
      Associations, SourceVerse.Pairs);
    PairList := SourceVerse.GetPairsList(plMetaData);
    try
      AssertEquals(2, PairList.Count);
      NewSourcePhrase := PairList[0];
      NewTargetPhrase := PairList[1];
    finally
      PairList.Free;
    end;
    AssertEquals(OldSourcePhrase, NewSourcePhrase);
    AssertTrue('The live target phrase must replace the old wording',
      OldTargetPhrase <> NewTargetPhrase);

    AssertTrue('Live verses must reconcile the semantic pair immediately',
      Workspace.SaveAssociations(FManager, SourceVerse, DestinationVerse,
        FService, '1,1,1'));
    AssertEquals('The speculative cache must remove the old target', 0,
      FManager.GetPairOccurrenceCount(OldSourcePhrase, OldTargetPhrase));
    AssertEquals('The speculative cache must expose the live target', 1,
      FManager.GetPairOccurrenceCount(NewSourcePhrase, NewTargetPhrase));
    AssertEquals('Canonical text must still be unchanged before persistence',
      'mister', FPersistence.CurrentText(DestinationTextFieldIndex));
    AssertTrue(FService.CanReplayPendingCache);

    // Stage the canonical edit, then mirror TProject.Commit: discard the
    // speculative cache transaction, publish canonical rows and revision,
    // replay the recorded delta, and finally publish the built revision.
    FPersistence.SetCurrentText(DestinationTextFieldIndex,
      DestinationVerse.XML);
    FPersistence.WriteCurrentPairs(SourceVerse.Pairs);
    FManager.Rollback;
    FService.MarkCacheOutdated;
    FPersistence.CommitInfo;
    FPersistence.CommitPairs;
    FService.MarkSourceRevisionPersisted;
    AssertTrue(FService.CanReplayPendingCache);
    FService.ReplayPendingCache;
    FManager.Commit;
    AssertTrue(FService.StageBuiltRevision);
    FPersistence.CommitInfo;
    FService.CommitCompleted;

    AssertEquals('necessario',
      FPersistence.CurrentText(DestinationTextFieldIndex));
    AssertEquals(0, FManager.GetPairOccurrenceCount(OldSourcePhrase,
      OldTargetPhrase));
    AssertEquals(1, FManager.GetPairOccurrenceCount(NewSourcePhrase,
      NewTargetPhrase));
    AssertFalse(FService.NeedsRebuild);

    CloseServices;
    OpenServices;
    AssertTrue(FPersistence.LocateReference('1,1,1'));
    DestinationTextFieldIndex := FPersistence.PairsTable.FieldByName(
      'pare_texto_destino').Index;
    AssertEquals('The committed text must survive reopening', 'necessario',
      FPersistence.CurrentText(DestinationTextFieldIndex));
    AssertEquals('The obsolete pair must stay absent after reopening', 0,
      FManager.GetPairOccurrenceCount(OldSourcePhrase, OldTargetPhrase));
    AssertEquals('The replacement pair must survive reopening', 1,
      FManager.GetPairOccurrenceCount(NewSourcePhrase, NewTargetPhrase));
    AssertFalse('Published version-4 revisions must remain current',
      FService.NeedsRebuild);
  finally
    DestinationVerse.Free;
    SourceVerse.Free;
    Workspace.Free;
  end;
end;

procedure TAssociationMemoryServiceTest.
  RevertedStableTopologyEditSurvivesCacheReplayAndReopen;
var
  Associations: string;
  DestinationTextFieldIndex: Integer;
  DestinationVerse: TVerse;
  NewSourcePhrase: string;
  NewTargetPhrase: string;
  OldSourcePhrase: string;
  OldTargetPhrase: string;
  RestoredSourcePhrase: string;
  RestoredTargetPhrase: string;
  SourceVerse: TVerse;
  Workspace: TCurrentVerseWorkspace;

  procedure ReadSemanticPair(out SourcePhrase, TargetPhrase: string);
  var
    PairList: TStringList;
  begin
    PairList := SourceVerse.GetPairsList(plMetaData);
    try
      AssertEquals('The Verse Pair must contain one semantic association', 2,
        PairList.Count);
      SourcePhrase := PairList[0];
      TargetPhrase := PairList[1];
    finally
      PairList.Free;
    end;
  end;

begin
  Associations := '<par a="0" b="0,2">';
  AssertTrue(FPersistence.LocateReference('1,1,1'));
  DestinationTextFieldIndex := FPersistence.PairsTable.FieldByName(
    'pare_texto_destino').Index;

  // Publish the real UTF-8 wording as the version-4 canonical baseline.
  FPersistence.SetCurrentText(DestinationTextFieldIndex, 'é mister');
  FPersistence.WriteCurrentPairs(Associations);
  FService.MarkSourceDirty;
  FPersistence.CommitInfo;
  FPersistence.CommitPairs;
  FService.MarkSourceRevisionPersisted;
  FService.BeginRebuild;
  FService.AddVerse('1,1,1', 'Gen 1:1', 'source<WG1>', 'é mister',
    Associations);
  FService.FinishRebuild;
  FManager.Commit;
  AssertTrue(FService.StageBuiltRevision);
  FPersistence.CommitInfo;
  FService.CommitCompleted;

  Workspace := TCurrentVerseWorkspace.Create;
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.PairedVerse := DestinationVerse;
    DestinationVerse.PairedVerse := SourceVerse;
    SourceVerse.Text := 'source<WG1>';
    DestinationVerse.Text := 'é mister';
    Workspace.ApplyLoadedAssociations(SourceVerse, Associations);
    ReadSemanticPair(OldSourcePhrase, OldTargetPhrase);
    AssertEquals('é;mister', OldTargetPhrase);
    AssertEquals(1, FManager.GetPairOccurrenceCount(OldSourcePhrase,
      OldTargetPhrase));

    DestinationVerse.Text := 'é necessário';
    SourceVerse.Pairs := Associations;
    AssertEquals('The replacement must keep the serialized indexes',
      Associations, SourceVerse.Pairs);
    ReadSemanticPair(NewSourcePhrase, NewTargetPhrase);
    AssertEquals(OldSourcePhrase, NewSourcePhrase);
    AssertEquals('é;necessário', NewTargetPhrase);

    AssertTrue('The replacement must update the speculative cache',
      Workspace.SaveAssociations(FManager, SourceVerse, DestinationVerse,
        FService, '1,1,1'));
    AssertEquals(0, FManager.GetPairOccurrenceCount(OldSourcePhrase,
      OldTargetPhrase));
    AssertEquals(1, FManager.GetPairOccurrenceCount(NewSourcePhrase,
      NewTargetPhrase));

    // Restore the live canonical Verse Pair without replacing Workspace's
    // semantic baseline, so the second save must generate inverse deltas.
    SourceVerse.Text := 'source<WG1>';
    DestinationVerse.Text := 'é mister';
    SourceVerse.Pairs := Associations;
    AssertEquals('Restoring text must preserve association serialization',
      Associations, SourceVerse.Pairs);
    ReadSemanticPair(RestoredSourcePhrase, RestoredTargetPhrase);
    AssertEquals(OldSourcePhrase, RestoredSourcePhrase);
    AssertEquals(OldTargetPhrase, RestoredTargetPhrase);

    AssertTrue('Saving the restored verses must reverse the speculative pair',
      Workspace.SaveAssociations(FManager, SourceVerse, DestinationVerse,
        FService, '1,1,1'));
    AssertEquals('The canonical UTF-8 pair must be restored immediately', 1,
      FManager.GetPairOccurrenceCount(OldSourcePhrase, OldTargetPhrase));
    AssertEquals('The reverted replacement must disappear immediately', 0,
      FManager.GetPairOccurrenceCount(NewSourcePhrase, NewTargetPhrase));
    AssertEquals(Associations, SourceVerse.Pairs);
    AssertEquals('é mister',
      FPersistence.CurrentText(DestinationTextFieldIndex));
    AssertEquals(Associations, FPersistence.CurrentPairs);
    AssertTrue(FService.CanReplayPendingCache);

    // Mirror Commit: roll back all speculative cache writes, replay the net
    // delta over the canonical baseline, publish, and then reopen.
    FManager.Rollback;
    FService.MarkCacheOutdated;
    AssertFalse(FService.CacheValidated);
    FPersistence.CommitInfo;
    FPersistence.CommitPairs;
    FService.MarkSourceRevisionPersisted;
    AssertTrue(FService.CanReplayPendingCache);
    FService.ReplayPendingCache;
    AssertTrue('Only the replayed final state may receive a certificate',
      FService.CacheValidated);
    FManager.Commit;
    AssertTrue(FService.StageBuiltRevision);
    FPersistence.CommitInfo;
    FService.CommitCompleted;

    AssertEquals(1, FManager.GetPairOccurrenceCount(OldSourcePhrase,
      OldTargetPhrase));
    AssertEquals(0, FManager.GetPairOccurrenceCount(NewSourcePhrase,
      NewTargetPhrase));
    AssertFalse(FService.NeedsRebuild);

    CloseServices;
    OpenServices;
    AssertTrue(FPersistence.LocateReference('1,1,1'));
    DestinationTextFieldIndex := FPersistence.PairsTable.FieldByName(
      'pare_texto_destino').Index;
    AssertEquals('é mister',
      FPersistence.CurrentText(DestinationTextFieldIndex));
    AssertEquals(Associations, FPersistence.CurrentPairs);
    AssertEquals('The canonical pair must survive service reopening', 1,
      FManager.GetPairOccurrenceCount(OldSourcePhrase, OldTargetPhrase));
    AssertEquals('The reverted pair must not be certified after reopening', 0,
      FManager.GetPairOccurrenceCount(NewSourcePhrase, NewTargetPhrase));
    AssertFalse(FService.NeedsRebuild);
    AssertTrue(FService.CacheValidated);
  finally
    DestinationVerse.Free;
    SourceVerse.Free;
    Workspace.Free;
  end;
end;

procedure TAssociationMemoryServiceTest.
  MatchingLegacyRevisionsWithoutCacheVersionRequireRebuild;
begin
  FConfig.WriteString(ASSOCIATION_MEMORY_SOURCE_REVISION_KEY, '7');
  FConfig.WriteString(ASSOCIATION_MEMORY_BUILT_REVISION_KEY, '7');
  FConfig.WriteString(ASSOCIATION_MEMORY_CACHE_VERSION_KEY, '');
  FPersistence.CommitInfo;

  FService.ReloadRevisionState;
  AssertTrue('The durable source and built revisions should match',
    FService.RevisionsMatch);
  AssertTrue('A cache written before the baseline fix must be rebuilt once',
    FService.NeedsRebuild);
  AssertFalse(FService.CacheValidated);
end;

procedure TAssociationMemoryServiceTest.
  MatchingV3RevisionsRequireRebuildAndV4IsAccepted;
begin
  AssertEquals('The current cache contract must be version 4', '4',
    ASSOCIATION_MEMORY_CACHE_VERSION);
  FConfig.WriteString(ASSOCIATION_MEMORY_SOURCE_REVISION_KEY, '7');
  FConfig.WriteString(ASSOCIATION_MEMORY_BUILT_REVISION_KEY, '7');
  FConfig.WriteString(ASSOCIATION_MEMORY_CACHE_VERSION_KEY, '3');
  FPersistence.CommitInfo;

  FService.ReloadRevisionState;
  AssertTrue('The durable source and built revisions should match',
    FService.RevisionsMatch);
  AssertTrue('A version 3 cache must be rebuilt once for version 4',
    FService.NeedsRebuild);
  AssertFalse('An obsolete cache version cannot retain its certificate',
    FService.CacheValidated);

  FConfig.WriteString(ASSOCIATION_MEMORY_CACHE_VERSION_KEY,
    ASSOCIATION_MEMORY_CACHE_VERSION);
  FPersistence.CommitInfo;

  FService.ReloadRevisionState;
  AssertTrue('The durable source and built revisions should still match',
    FService.RevisionsMatch);
  AssertFalse('A version 4 cache with matching revisions is current',
    FService.NeedsRebuild);
  AssertTrue('A version 4 cache with matching revisions is certified',
    FService.CacheValidated);
end;

procedure TAssociationMemoryServiceTest.
  WorkspaceIndexShiftKeepsPersistenceAndMemoryInvariant;
var
  DestinationSyntagm: TSyntagm;
  DestinationVerse: TVerse;
  Impact: TAssociationEditImpact;
  OriginalPairs: string;
  SourceSyntagm: TSyntagm;
  SourceVerse: TVerse;
  Trace: TVerseEditTrace;
  Workspace: TCurrentVerseWorkspace;
begin
  FService.MarkSourceDirty;
  FPersistence.CommitInfo;
  FPersistence.CommitPairs;
  FService.MarkSourceRevisionPersisted;
  FService.BeginRebuild;
  FService.AddVerse('1,1,1', 'Gen 1:1', 'source<WG1>', 'target',
    '<par a="0" b="0">');
  FService.FinishRebuild;
  FManager.Commit;
  AssertTrue(FService.StageBuiltRevision);
  FPersistence.CommitInfo;
  FService.CommitCompleted;
  AssertEquals(1, FManager.GetPairOccurrenceCount('<wg1>', 'target'));

  Workspace := TCurrentVerseWorkspace.Create;
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  Trace := nil;
  try
    SourceVerse.PairedVerse := DestinationVerse;
    DestinationVerse.PairedVerse := SourceVerse;
    SourceVerse.Text := 'source<WG1>';
    DestinationVerse.Text := 'target';
    AssertTrue(FPersistence.LocateReference('1,1,1'));
    Workspace.ApplyLoadedAssociations(SourceVerse,
      FPersistence.CurrentPairs);
    OriginalPairs := SourceVerse.Pairs;
    SourceSyntagm := SourceVerse.Syntagms[0];
    DestinationSyntagm := DestinationVerse.Syntagms[0];

    Trace := TVerseEditTrace.CreateIdentity('source',
      SourceVerse.TokenizerVersion);
    Trace.ApplyReplacement(0, 0, 'prefix ');
    AssertTrue('the traced prefix insertion must apply',
      SourceVerse.TryApplyTracedEdit('prefix source<WG1>', Trace, Impact));
    AssertEquals('the associated source member must be preserved', 1,
      Impact.PreservedMembers);
    AssertEquals('an index-only shift must remove no member', 0,
      Impact.RemovedMembers);
    AssertTrue('the original Syntagm object must move to its new index',
      SourceVerse.Syntagms[2] = SourceSyntagm);
    AssertTrue('serialized indexes must reflect the inserted prefix',
      SourceVerse.Pairs <> OriginalPairs);
    AssertEquals(1, SourceSyntagm.Pairs.Count);
    AssertTrue(SourceSyntagm.Pairs[0] = DestinationSyntagm);
    AssertEquals(1, DestinationSyntagm.Pairs.Count);
    AssertTrue(DestinationSyntagm.Pairs[0] = SourceSyntagm);

    AssertFalse('index-only shifts must not mutate Association memory',
      Workspace.SaveAssociations(FManager, SourceVerse, DestinationVerse,
        FService, '1,1,1'));
    AssertFalse('index-only shifts must not dirty Association memory',
      FService.PendingDirty);
    AssertEquals('index shifts must not decrement or duplicate memory', 1,
      FManager.GetPairOccurrenceCount('<wg1>', 'target'));

    AssertTrue('the shifted indexes must still be persisted',
      Workspace.SavePairs(FPersistence, SourceVerse));
    AssertEquals(SourceVerse.Pairs, FPersistence.CurrentPairs);
    // GetPairOccurrenceCount above leaves the independent memory connection
    // in a read transaction, which must not block the canonical writer.
    FManager.Rollback;
    FPersistence.CommitPairs;
    AssertTrue(FPersistence.LocateReference('1,1,1'));
    AssertEquals('the shifted topology must survive persistence',
      SourceVerse.Pairs, FPersistence.CurrentPairs);
    AssertEquals('persistence must not alter Association memory', 1,
      FManager.GetPairOccurrenceCount('<wg1>', 'target'));
  finally
    Trace.Free;
    DestinationVerse.Free;
    SourceVerse.Free;
    Workspace.Free;
  end;
end;

procedure TAssociationMemoryServiceTest.
  RenderBufferReuseReleasesParsedSyntagmLists;
var
  AfterHeap: THeapStatus;
  BeforeHeap: THeapStatus;
  HeapGrowth: PtrUInt;
  Iteration: Integer;
  LongText: string;
  WordIndex: Integer;
begin
  LongText := '';
  for WordIndex := 1 to 500 do
    LongText := LongText + 'word ';

  FRenderBuffer.LoadTexts(LongText, LongText);
  BeforeHeap := GetHeapStatus;
  for Iteration := 1 to 300 do
    FRenderBuffer.LoadTexts(LongText, LongText);
  AfterHeap := GetHeapStatus;

  if AfterHeap.TotalAllocated > BeforeHeap.TotalAllocated then
    HeapGrowth := AfterHeap.TotalAllocated - BeforeHeap.TotalAllocated
  else
    HeapGrowth := 0;
  AssertTrue(Format('Repeated render-buffer loads retained %d bytes',
    [HeapGrowth]), HeapGrowth < 1024 * 1024);
end;

procedure TAssociationMemoryServiceTest.NonCanonicalAssociationOrderIsAccepted;
begin
  FRenderBuffer.SetTokenizerVersion(1);
  FRenderBuffer.LoadPair('alpha beta', 'one two',
    '<par a="2,0" b="2,0">');
  AssertEquals('Equivalent syntagm order must be normalized',
    '<par a="0,2" b="0,2">', FRenderBuffer.SourceVerse.Pairs);
end;

procedure TAssociationMemoryServiceTest.
  ExtractedPairDataPreservesExactDestinationIndexes;
var
  CorruptionDetails: string;
  PairData: TAssociationPairDataArray;
begin
  PairData := nil;
  FRenderBuffer.SetTokenizerVersion(1);
  AssertTrue(FRenderBuffer.TryExtractAssociationPairData(
    'source<WG3607>', 'um vaso como um grande lençol',
    '<par a="0" b="6,10">', plAll, PairData, CorruptionDetails));

  AssertEquals(1, Length(PairData));
  AssertEquals('source<wg3607>', PairData[0].SourcePhrase);
  AssertEquals('um;lençol', PairData[0].TargetPhrase);
  AssertEquals(2, Length(PairData[0].TargetSyntagmIndexes));
  AssertEquals(6, PairData[0].TargetSyntagmIndexes[0]);
  AssertEquals(10, PairData[0].TargetSyntagmIndexes[1]);
end;

procedure TAssociationMemoryServiceTest.RebuildSkipsInvalidAssociations;
var
  Expected: TStringList;
  ErrorMessage: string;
begin
  Expected := ExpectedPair;
  try
    FService.BeginRebuild;
    FService.AddVerse('1,1,1', 'Gen 1:1', 'source<WG1>', 'target',
      '<par a="0" b="0">');
    FService.AddVerse('1,1,2', 'Gen 1:2', 'source', 'target',
      '<par a="invalid" b="0">');
    FService.FinishRebuild;

    AssertEquals('Invalid verse pairs must be skipped', 1,
      FService.SkippedAssociationCount);
    AssertTrue('Valid verse pairs must remain in the rebuilt cache',
      FManager.ValidatePairs(Expected, ErrorMessage));
  finally
    Expected.Free;
  end;
end;

procedure TAssociationMemoryServiceTest.
  ProjectReadyOccurrencesHonorLimitAndGlobalTotals;
const
  PREVIEW_LIMIT = 10;
  VERSE_COUNT = 12;
var
  ErrorMessage: string;
  ItemIndex: Integer;
  Matches: TTranslationPairMatchArray;
  Page: TTranslationOccurrencePage;
  ProjectFile: string;
  ProjectInstance: TProject;
begin
  ProjectFile := FFileName + '.ready-limit.bib';
  ProjectInstance := nil;
  try
    CreateProjectOccurrenceFixture(ProjectFile, VERSE_COUNT);
    ProjectInstance := TProject.Create;
    ProjectInstance.Escopo := etNT;
    ProjectInstance.OpenProject(ProjectFile);

    Matches := nil;
    SetLength(Matches, 1);
    Matches[0].SourcePhrase := '<wg1>';
    Matches[0].TargetPhrase := 'word';
    Matches[0].Occurrences := VERSE_COUNT;

    AssertTrue(ProjectInstance.TryGetReadyTranslationMemoryOccurrences(
      Matches, PREVIEW_LIMIT, Page, ErrorMessage));
    AssertEquals('', ErrorMessage);
    AssertEquals('the page limit must not truncate the global association ' +
      'total', VERSE_COUNT, Page.TotalAssociations);
    AssertEquals('the page limit must not truncate the global verse total',
      VERSE_COUNT, Page.TotalVerses);
    AssertTrue(Page.HasMore);
    AssertEquals(PREVIEW_LIMIT, Length(Page.Items));
    for ItemIndex := 0 to High(Page.Items) do
    begin
      AssertEquals(1, Page.Items[ItemIndex].Occurrences);
      AssertEquals('word', Page.Items[ItemIndex].DestinationText);
      AssertFalse(Page.Items[ItemIndex].DisplayReference.IsEmpty);
    end;
  finally
    ProjectInstance.Free;
    if FileExists(ProjectFile) then
      DeleteFile(ProjectFile);
  end;
end;

procedure TAssociationMemoryServiceTest.
  ProjectReadyOccurrencesNeverRebuildUnavailableMemory;
const
  PREVIEW_LIMIT = 10;
  VERSE_COUNT = 12;
var
  BuiltRevisionBefore: string;
  ErrorMessage: string;
  Matches: TTranslationPairMatchArray;
  Page: TTranslationOccurrencePage;
  Probe: TProjectReadyOccurrenceRebuildProbe;
  ProjectFile: string;
  ProjectInstance: TProject;
  ReadyBuiltRevision: string;
  ReadySourceRevision: string;
  SourceRevisionBefore: string;
begin
  ProjectFile := FFileName + '.ready-state.bib';
  Probe := nil;
  ProjectInstance := nil;
  try
    CreateProjectOccurrenceFixture(ProjectFile, VERSE_COUNT);
    ProjectInstance := TProject.Create;
    ProjectInstance.Escopo := etNT;
    ProjectInstance.OpenProject(ProjectFile);
    ReadySourceRevision := ProjectInstance.GetInfo(
      ASSOCIATION_MEMORY_SOURCE_REVISION_KEY);
    ReadyBuiltRevision := ProjectInstance.GetInfo(
      ASSOCIATION_MEMORY_BUILT_REVISION_KEY);
    AssertEquals(ReadySourceRevision, ReadyBuiltRevision);

    Matches := nil;
    SetLength(Matches, 1);
    Matches[0].SourcePhrase := '<wg1>';
    Matches[0].TargetPhrase := 'word';
    Matches[0].Occurrences := VERSE_COUNT;

    ProjectInstance.ApplyVerseText('replacement source', tbOrigem, False);
    SourceRevisionBefore := ProjectInstance.GetInfo(
      ASSOCIATION_MEMORY_SOURCE_REVISION_KEY);
    BuiltRevisionBefore := ProjectInstance.GetInfo(
      ASSOCIATION_MEMORY_BUILT_REVISION_KEY);
    AssertFalse('the edit must advance the source revision',
      SourceRevisionBefore = ReadySourceRevision);
    AssertEquals('the edit must not publish a new built revision',
      ReadyBuiltRevision, BuiltRevisionBefore);
    AssertFalse('the edit must leave a revision mismatch requiring rebuild',
      SourceRevisionBefore = BuiltRevisionBefore);

    Page.TotalAssociations := 999;
    Page.TotalVerses := 999;
    Page.HasMore := True;
    SetLength(Page.Items, 1);
    AssertFalse('ready-only lookup must reject a cache needing rebuild',
      ProjectInstance.TryGetReadyTranslationMemoryOccurrences(
        Matches, PREVIEW_LIMIT, Page, ErrorMessage));
    AssertFalse(ErrorMessage.IsEmpty);
    AssertEquals(0, Page.TotalAssociations);
    AssertEquals(0, Page.TotalVerses);
    AssertFalse(Page.HasMore);
    AssertEquals(0, Length(Page.Items));
    AssertEquals('ready-only lookup must not advance the source revision',
      SourceRevisionBefore, ProjectInstance.GetInfo(
        ASSOCIATION_MEMORY_SOURCE_REVISION_KEY));
    AssertEquals('ready-only lookup must not publish a built revision',
      BuiltRevisionBefore, ProjectInstance.GetInfo(
        ASSOCIATION_MEMORY_BUILT_REVISION_KEY));

    AssertTrue('the regular API is the control that performs the rebuild',
      ProjectInstance.TryGetTranslationMemoryOccurrences(
        Matches, PREVIEW_LIMIT, Page, ErrorMessage));
    AssertEquals(SourceRevisionBefore, ProjectInstance.GetInfo(
      ASSOCIATION_MEMORY_BUILT_REVISION_KEY));
    AssertEquals(VERSE_COUNT - 1, Page.TotalAssociations);

    Probe := TProjectReadyOccurrenceRebuildProbe.Create(nil);
    Probe.FProject := ProjectInstance;
    Probe.FMatches := Matches;
    ProjectInstance.RebuildSuggestionDatabase(Probe);
    AssertTrue('the probe must run after the rebuild becomes active',
      Probe.FAttempted);
    AssertEquals(1, Probe.FCallCount);
    AssertFalse('ready-only lookup must reject an active rebuild',
      Probe.FReady);
    AssertFalse(Probe.FErrorMessage.IsEmpty);
    AssertEquals(0, Probe.FPage.TotalAssociations);
    AssertEquals(0, Probe.FPage.TotalVerses);
    AssertEquals(0, Length(Probe.FPage.Items));

    AssertTrue('the completed rebuild restores ready-only access',
      ProjectInstance.TryGetReadyTranslationMemoryOccurrences(
        Matches, PREVIEW_LIMIT, Page, ErrorMessage));
  finally
    Probe.Free;
    ProjectInstance.Free;
    if FileExists(ProjectFile) then
      DeleteFile(ProjectFile);
  end;
end;

initialization
  RegisterTest(TAssociationMemoryServiceTest);

end.
