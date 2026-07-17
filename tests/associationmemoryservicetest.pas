unit associationmemoryservicetest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, sqlite3conn, sqldb,
  SuggestionManager, project_persistence, project_config,
  Syntagm, Verse, verse_render_buffer, association_memory_service;

type
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
    function ExpectedPair: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CommitRollbackReopenAndFailedRebuildRemainConsistent;
    procedure MatchingLegacyRevisionsWithoutCacheVersionRequireRebuild;
    procedure IncrementalReplayValidatesOnlyAffectedPairs;
    procedure NonCanonicalAssociationOrderIsAccepted;
    procedure ExtractedPairDataPreservesExactDestinationIndexes;
    procedure RebuildSkipsInvalidAssociations;
    procedure RenderBufferReuseReleasesParsedSyntagmLists;
  end;

implementation

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
    '<par a="0,2" b="0,2">', FRenderBuffer.SourceVerse.Pares);
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

initialization
  RegisterTest(TAssociationMemoryServiceTest);

end.
