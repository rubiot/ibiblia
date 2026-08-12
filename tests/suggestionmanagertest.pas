unit suggestionmanagertest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, sqlite3conn, sqldb,
  SuggestionManager, current_verse_workspace, verse_render_buffer, Verse,
  Syntagm, translation_memory_model;

type
  TSuggestionManagerTest = class(TTestCase)
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FManager: TSuggestionManager;
    FLastRebuildPhase: TAssociationMemoryRebuildPhase;
    FRebuildProgressNotifications: Integer;
    FConcordanceOccurrences: TStringList;
    function QueryInt(const SQL: string): Integer;
    function QueryString(const SQL: string): string;
    procedure TrackRebuildProgress(Phase: TAssociationMemoryRebuildPhase;
      Current, Total: Integer);
    procedure CollectConcordanceOccurrence(
      const Data: TConcordanceOccurrenceData);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RepeatedAdditionAndPartialRemovalPreserveExactCount;
    procedure TotalAndExcessRemovalNeverCreateOrKeepNonPositiveRows;
    procedure RemovalCleansOnlyOrphanPhrases;
    procedure RebuildRollbackPreservesPreviousCache;
    procedure BulkRebuildAggregatesExactOccurrences;
    procedure BulkRebuildKeepsUnicodePhrasesDistinct;
    procedure BulkFlushFailurePreservesPreviousCache;
    procedure PreparedRebuildValidationDetectsCountMismatch;
    procedure PreparedRebuildReportsFinalizingPhase;
    procedure ValidatePairsUsesPairMultiplicity;
    procedure WorkspacePersistsIndividualCancellationAndIsIdempotent;
    procedure WorkspaceAutosaveKeepsAssociationBaseline;
    procedure WorkspaceSkipsUninitializedVersePair;
    procedure WorkspaceReconcilesDuplicatePairsAsMultiset;
    procedure WorkspaceReconcilesSourceAndDestinationTextChanges;
    procedure WorkspaceInvalidLoadIsAtomicAndRecoverable;
    procedure TranslationPopupUsesOccurrenceSum;
    procedure StructuredTranslationAlternativesPreserveCountsAndOrder;
    procedure PreloadedTranslationAlternativesAreCompleteAndInvalidated;
    procedure TranslationMemoryLookupDoesNotMatchTextPrefixes;
    procedure TranslationMemoryModelGroupsAndMarksCurrentAlternatives;
    procedure TranslationMemoryModelUsesMetadataWithoutStrong;
    procedure TranslationMemoryModelMatchesCurrentSuffixHyphen;
    procedure TranslationMemoryModelSortsByHistoricalFrequency;
    procedure TranslationMemoryOrderValueFallsBackSafely;
    procedure OccurrenceIndexTracksReferencesAndPagination;
    procedure OccurrenceIndexConsolidatesExactMatches;
    procedure OccurrenceIndexRollbackPreservesPreviousData;
    procedure PreparedRebuildValidationDetectsOccurrenceMismatch;
    procedure AffectedPairValidationChecksOnlyTheChangedPair;
    procedure ConcordanceOccurrenceEnumerationUsesReferenceIndex;
    procedure ConcordanceOccurrenceIndexReportsUnsafeFallbacks;
  end;

implementation

procedure TSuggestionManagerTest.SetUp;
begin
  inherited SetUp;
  FConnection := TSQLite3Connection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FConnection.Transaction := FTransaction;
  FConnection.DatabaseName := ':memory:';
  FConnection.Open;
  FTransaction.StartTransaction;

  FConnection.ExecuteDirect('create table traducoes (' +
    'trad_lori_id integer not null, trad_ldes_id integer not null, ' +
    'trad_ocorrencias integer)');
  FConnection.ExecuteDirect('create table sintagmas_locucoes_origem (' +
    'slor_lori_id integer not null, slor_sori_id integer not null, ' +
    'slor_ordem integer not null)');
  FConnection.ExecuteDirect('create table sintagmas_locucoes_destino (' +
    'slde_ldes_id integer not null, slde_sdes_id integer not null, ' +
    'slde_ordem integer not null)');
  FConnection.ExecuteDirect('create table locucoes_origem (' +
    'lori_id integer not null primary key, lori_qtde_sintagmas integer, ' +
    'lori_locucao varchar(200) unique)');
  FConnection.ExecuteDirect('create table locucoes_destino (' +
    'ldes_id integer not null primary key, ldes_qtde_sintagmas integer, ' +
    'ldes_locucao varchar(200) unique)');
  FConnection.ExecuteDirect('create table pares (' +
    'pare_id text primary key, pare_ref text, pare_texto_origem text, ' +
    'pare_texto_destino text, pare_texto_consulta1 text, ' +
    'pare_texto_consulta2 text, pare_pares text, pare_situacao number, ' +
    'pare_comentarios text)');
  FConnection.ExecuteDirect('insert into pares values ' +
    '(''1,1,1'', ''1,1,1'', '''', ''first destination'', '''', '''', '''', 0, ''''), ' +
    '(''1,1,2'', ''1,1,2'', '''', ''second destination'', '''', '''', '''', 0, ''''), ' +
    '(''1,1,3'', ''1,1,3'', '''', ''third destination'', '''', '''', '''', 0, '''')');
  FConnection.ExecuteDirect(
    'create unique index trad_uk on traducoes(trad_lori_id, trad_ldes_id)');
  FConnection.ExecuteDirect(
    'create unique index slor_uk on sintagmas_locucoes_origem' +
    '(slor_lori_id, slor_sori_id, slor_ordem)');
  FConnection.ExecuteDirect(
    'create unique index slde_uk on sintagmas_locucoes_destino' +
    '(slde_ldes_id, slde_sdes_id, slde_ordem)');

  FManager := TSuggestionManager.Create(nil, FConnection);
  FConcordanceOccurrences := TStringList.Create;
end;

procedure TSuggestionManagerTest.TearDown;
begin
  FConcordanceOccurrences.Free;
  FManager.Free;
  if FTransaction.Active then
    FTransaction.Rollback;
  FConnection.Close;
  FConnection.Free;
  FTransaction.Free;
  inherited TearDown;
end;

procedure TSuggestionManagerTest.CollectConcordanceOccurrence(
  const Data: TConcordanceOccurrenceData);
begin
  FConcordanceOccurrences.Add(Format('%s|%s|%s|%d|%d',
    [Data.ReferenceID, Data.SourcePhrase, Data.TargetPhrase,
     Data.Occurrences, Data.ReferenceOrder]));
end;

function TSuggestionManagerTest.QueryInt(const SQL: string): Integer;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FConnection;
    Query.SQL.Text := SQL;
    Query.Open;
    Result := Query.Fields[0].AsInteger;
  finally
    Query.Free;
  end;
end;

function TSuggestionManagerTest.QueryString(const SQL: string): string;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := FConnection;
    Query.SQL.Text := SQL;
    Query.Open;
    Result := Query.Fields[0].AsString;
  finally
    Query.Free;
  end;
end;

procedure TSuggestionManagerTest.TrackRebuildProgress(
  Phase: TAssociationMemoryRebuildPhase; Current, Total: Integer);
begin
  FLastRebuildPhase := Phase;
  Inc(FRebuildProgressNotifications);
end;

procedure TSuggestionManagerTest.RepeatedAdditionAndPartialRemovalPreserveExactCount;
var
  ErrorMessage: string;
begin
  FManager.AddPair('G1', 'word');
  FManager.AddPair('G1', 'word');
  AssertEquals(2, QueryInt('select trad_ocorrencias from traducoes'));
  AssertEquals(2, FManager.GetPairOccurrenceCount('G1', 'word'));
  AssertEquals(0, FManager.GetPairOccurrenceCount('missing', 'word'));

  FManager.RemovePair('G1', 'word');
  AssertEquals(1, QueryInt('select trad_ocorrencias from traducoes'));
  AssertEquals(1, FManager.GetPairOccurrenceCount('G1', 'word'));
  AssertTrue(FManager.Validate(ErrorMessage));
end;

procedure TSuggestionManagerTest.TotalAndExcessRemovalNeverCreateOrKeepNonPositiveRows;
var
  ErrorMessage: string;
begin
  FManager.AddPair('G1', 'word');
  FManager.RemovePair('G1', 'word');
  FManager.RemovePair('G1', 'word');
  FManager.RemovePair('MISSING', 'absent');

  AssertEquals(0, QueryInt('select count(*) from traducoes'));
  AssertEquals(0, QueryInt('select count(*) from traducoes ' +
    'where trad_ocorrencias <= 0'));
  AssertEquals(0, QueryInt('select count(*) from locucoes_origem'));
  AssertEquals(0, QueryInt('select count(*) from locucoes_destino'));
  AssertEquals(0, QueryInt('select count(*) from sintagmas_locucoes_origem'));
  AssertEquals(0, QueryInt('select count(*) from sintagmas_locucoes_destino'));
  AssertTrue(FManager.Validate(ErrorMessage));
end;

procedure TSuggestionManagerTest.RemovalCleansOnlyOrphanPhrases;
begin
  FManager.AddPair('G1', 'shared');
  FManager.AddPair('G2', 'shared');
  FManager.RemovePair('G1', 'shared');

  AssertEquals(1, QueryInt('select count(*) from traducoes'));
  AssertEquals(1, QueryInt('select count(*) from locucoes_origem'));
  AssertEquals(1, QueryInt('select count(*) from locucoes_destino'));
  AssertEquals(1, QueryInt('select count(*) from locucoes_origem ' +
    'where lori_locucao = ''g2'''));

  FManager.RemovePair('G2', 'shared');
  AssertEquals(0, QueryInt('select count(*) from locucoes_origem'));
  AssertEquals(0, QueryInt('select count(*) from locucoes_destino'));
end;

procedure TSuggestionManagerTest.RebuildRollbackPreservesPreviousCache;
begin
  FManager.AddPair('OLD', 'before');
  FManager.BeginRebuild;
  FManager.ClearAssociationDatabase;
  FManager.AddPair('NEW', 'partial');
  FManager.RollbackRebuild;

  AssertEquals(1, QueryInt('select count(*) from traducoes'));
  AssertEquals(1, QueryInt('select count(*) from locucoes_origem ' +
    'where lori_locucao = ''old'''));
  AssertEquals(0, QueryInt('select count(*) from locucoes_origem ' +
    'where lori_locucao = ''new'''));
end;

procedure TSuggestionManagerTest.BulkRebuildAggregatesExactOccurrences;
var
  ErrorMessage: string;
begin
  FManager.BeginRebuild;
  FManager.ClearAssociationDatabase;
  FManager.AddPair('G1', 'one');
  FManager.AddPair('G1', 'one');
  FManager.AddPair('G2', 'two');
  FManager.PrepareRebuildValidation;

  AssertEquals(2, QueryInt('select trad_ocorrencias from traducoes ' +
    'where trad_lori_id = 1 and trad_ldes_id = 1'));
  AssertEquals(3, QueryInt('select sum(trad_ocorrencias) from traducoes'));
  AssertTrue(FManager.ValidatePreparedRebuild(ErrorMessage));
  FManager.CommitRebuild;
  AssertTrue(FManager.Validate(ErrorMessage));
end;

procedure TSuggestionManagerTest.BulkRebuildKeepsUnicodePhrasesDistinct;
var
  ErrorMessage: string;
begin
  FManager.BeginRebuild;
  try
    FManager.ClearAssociationDatabase;
    FManager.AddPair('<wg2532><wtconj le="καί">', 'porém');
    FManager.AddPair('<wg2532><wtconj le="και">', 'porém');
    FManager.PrepareRebuildValidation;

    AssertEquals(2, QueryInt('select count(*) from locucoes_origem'));
    AssertEquals(2, QueryInt('select count(*) from traducoes'));
    AssertEquals(2, QueryInt('select sum(trad_ocorrencias) from traducoes'));
    AssertTrue(FManager.ValidatePreparedRebuild(ErrorMessage));
    FManager.CommitRebuild;
  except
    FManager.RollbackRebuild;
    raise;
  end;
end;

procedure TSuggestionManagerTest.BulkFlushFailurePreservesPreviousCache;
var
  FailureReported: Boolean;
begin
  FManager.AddPair('OLD', 'before');
  FManager.Commit;

  FManager.BeginRebuild;
  FManager.ClearAssociationDatabase;
  FManager.AddPair('NEW', 'partial');
  FConnection.ExecuteDirect(
    'insert into traducoes values (1, 1, 99)');
  FailureReported := False;
  try
    FManager.PrepareRebuildValidation;
  except
    FailureReported := True;
    FManager.RollbackRebuild;
  end;
  AssertTrue('The injected bulk failure must be reported', FailureReported);
  AssertEquals(1, QueryInt('select count(*) from traducoes'));
  AssertEquals(1, QueryInt('select count(*) from locucoes_origem ' +
    'where lori_locucao = ''old'''));
end;

procedure TSuggestionManagerTest.
  PreparedRebuildValidationDetectsCountMismatch;
var
  ErrorMessage: string;
begin
  FManager.BeginRebuild;
  try
    FManager.ClearAssociationDatabase;
    FManager.AddPair('G1', 'one');
    FManager.AddPair('G1', 'one');
    FManager.AddPair('G2', 'two');
    FManager.PrepareRebuildValidation;
    AssertTrue(FManager.ValidatePreparedRebuild(ErrorMessage));

    FConnection.ExecuteDirect('update traducoes set trad_ocorrencias = 1 ' +
      'where trad_lori_id = 1 and trad_ldes_id = 1');
    AssertFalse(FManager.ValidatePreparedRebuild(ErrorMessage));
    AssertTrue(Pos('expected 2, stored 1', ErrorMessage) > 0);
  finally
    FManager.RollbackRebuild;
  end;
end;

procedure TSuggestionManagerTest.PreparedRebuildReportsFinalizingPhase;
var
  ErrorMessage: string;
begin
  FRebuildProgressNotifications := 0;
  FManager.OnRebuildProgress := @TrackRebuildProgress;
  FManager.BeginRebuild;
  try
    FManager.ClearAssociationDatabase;
    FManager.AddPairForReference('1,1,1', '<wg1>', 'word');
    FManager.PrepareRebuildValidation;
    AssertTrue(FManager.ValidatePreparedRebuild(ErrorMessage));
    FManager.CommitRebuild;

    AssertTrue(FRebuildProgressNotifications > 0);
    AssertEquals(Ord(amrpFinalizing), Ord(FLastRebuildPhase));
  finally
    FManager.OnRebuildProgress := nil;
    FManager.RollbackRebuild;
  end;
end;

procedure TSuggestionManagerTest.ValidatePairsUsesPairMultiplicity;
var
  ExpectedPairs: TStringList;
  ErrorMessage: string;
begin
  FManager.AddPair('G1', 'one');
  FManager.AddPair('G1', 'one');
  FManager.AddPair('G2', 'two');
  ExpectedPairs := TStringList.Create;
  try
    ExpectedPairs.Add('G1');
    ExpectedPairs.Add('one');
    ExpectedPairs.Add('G1');
    ExpectedPairs.Add('one');
    ExpectedPairs.Add('G2');
    ExpectedPairs.Add('two');
    AssertTrue(FManager.ValidatePairs(ExpectedPairs, ErrorMessage));

    ExpectedPairs.Delete(ExpectedPairs.Count - 1);
    ExpectedPairs.Delete(ExpectedPairs.Count - 1);
    AssertFalse(FManager.ValidatePairs(ExpectedPairs, ErrorMessage));
  finally
    ExpectedPairs.Free;
  end;
end;

procedure TSuggestionManagerTest.WorkspacePersistsIndividualCancellationAndIsIdempotent;
var
  Workspace: TCurrentVerseWorkspace;
  SourceVerse: TVerse;
  DestinationVerse: TVerse;
begin
  Workspace := TCurrentVerseWorkspace.Create;
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.VersiculoPar := DestinationVerse;
    SourceVerse.Texto := 'source';
    DestinationVerse.Texto := 'target';
    SourceVerse.Sintagmas[0].AddToSelection;
    DestinationVerse.Sintagmas[0].AddToSelection;
    SourceVerse.AssociarSintagmas;

    AssertTrue(Workspace.SaveAssociations(FManager, SourceVerse,
      DestinationVerse));
    AssertEquals(1, QueryInt('select trad_ocorrencias from traducoes'));

    SourceVerse.Sintagmas[0].DisassociatePairs;
    AssertTrue(Workspace.SaveAssociations(FManager, SourceVerse,
      DestinationVerse));
    AssertEquals(0, QueryInt('select count(*) from traducoes'));
    AssertFalse(Workspace.SaveAssociations(FManager, SourceVerse,
      DestinationVerse));
    AssertEquals(0, QueryInt('select count(*) from traducoes'));
  finally
    Workspace.Free;
    SourceVerse.Free;
    DestinationVerse.Free;
  end;
end;

procedure TSuggestionManagerTest.WorkspaceAutosaveKeepsAssociationBaseline;
var
  Workspace: TCurrentVerseWorkspace;
  SourceVerse: TVerse;
  DestinationVerse: TVerse;
  VerseViews: array[0..1] of TVerse;
  TextFieldIndexes: array[0..1] of Integer;
begin
  Workspace := TCurrentVerseWorkspace.Create;
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.VersiculoPar := DestinationVerse;
    SourceVerse.Texto := 'source';
    DestinationVerse.Texto := 'target';
    SourceVerse.Sintagmas[0].AddToSelection;
    DestinationVerse.Sintagmas[0].AddToSelection;
    SourceVerse.AssociarSintagmas;

    AssertTrue(Workspace.SaveAssociations(FManager, SourceVerse,
      DestinationVerse));
    AssertEquals(1, QueryInt('select trad_ocorrencias from traducoes'));
    SourceVerse.Modificado := False;
    SourceVerse.XMLModificado := False;
    DestinationVerse.Modificado := False;
    DestinationVerse.XMLModificado := False;

    VerseViews[0] := SourceVerse;
    VerseViews[1] := DestinationVerse;
    TextFieldIndexes[0] := 0;
    TextFieldIndexes[1] := 1;
    AssertFalse('Autosave without navigation must not change associations',
      Workspace.SaveBeforeScroll(nil, FManager, nil, nil, VerseViews,
        TextFieldIndexes, 0, 1));

    AssertFalse('Autosave must preserve the baseline for the next navigation',
      Workspace.SaveAssociations(FManager, SourceVerse, DestinationVerse));
    AssertEquals('Autosave must not count the same association twice', 1,
      QueryInt('select trad_ocorrencias from traducoes'));
  finally
    Workspace.Free;
    SourceVerse.Free;
    DestinationVerse.Free;
  end;
end;

procedure TSuggestionManagerTest.WorkspaceSkipsUninitializedVersePair;
var
  Workspace: TCurrentVerseWorkspace;
  SourceVerse: TVerse;
  DestinationVerse: TVerse;
  Pairs: TStringList;
begin
  Workspace := TCurrentVerseWorkspace.Create;
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.VersiculoPar := DestinationVerse;
    AssertFalse(Workspace.SaveAssociations(FManager, SourceVerse,
      DestinationVerse));
    Pairs := SourceVerse.GetListaPares(plMetaData);
    try
      AssertEquals(0, Pairs.Count);
    finally
      Pairs.Free;
    end;
    AssertEquals(0, QueryInt('select count(*) from traducoes'));
  finally
    Workspace.Free;
    SourceVerse.Free;
    DestinationVerse.Free;
  end;
end;

procedure TSuggestionManagerTest.WorkspaceReconcilesDuplicatePairsAsMultiset;
var
  Workspace: TCurrentVerseWorkspace;
  SourceVerse: TVerse;
  DestinationVerse: TVerse;
begin
  Workspace := TCurrentVerseWorkspace.Create;
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.VersiculoPar := DestinationVerse;
    SourceVerse.Texto := 'first<WG1> second<WG1>';
    DestinationVerse.Texto := 'same same';

    SourceVerse.Sintagmas[0].AddToSelection;
    DestinationVerse.Sintagmas[0].AddToSelection;
    SourceVerse.AssociarSintagmas;
    SourceVerse.LimparSelecao;
    DestinationVerse.LimparSelecao;
    SourceVerse.Sintagmas[2].AddToSelection;
    DestinationVerse.Sintagmas[2].AddToSelection;
    SourceVerse.AssociarSintagmas;

    AssertTrue(Workspace.SaveAssociations(FManager, SourceVerse,
      DestinationVerse));
    AssertEquals(2, QueryInt('select trad_ocorrencias from traducoes'));

    SourceVerse.Sintagmas[0].DisassociatePairs;
    AssertTrue(Workspace.SaveAssociations(FManager, SourceVerse,
      DestinationVerse));
    AssertEquals(1, QueryInt('select trad_ocorrencias from traducoes'));
  finally
    Workspace.Free;
    SourceVerse.Free;
    DestinationVerse.Free;
  end;
end;

procedure TSuggestionManagerTest.WorkspaceReconcilesSourceAndDestinationTextChanges;
var
  Workspace: TCurrentVerseWorkspace;
  SourceVerse: TVerse;
  DestinationVerse: TVerse;
begin
  Workspace := TCurrentVerseWorkspace.Create;
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.VersiculoPar := DestinationVerse;
    SourceVerse.Texto := 'source';
    DestinationVerse.Texto := 'target';
    SourceVerse.Sintagmas[0].AddToSelection;
    DestinationVerse.Sintagmas[0].AddToSelection;
    SourceVerse.AssociarSintagmas;
    Workspace.SaveAssociations(FManager, SourceVerse, DestinationVerse);

    SourceVerse.AlterarTexto('changed');
    AssertTrue(Workspace.SaveAssociations(FManager, SourceVerse,
      DestinationVerse));
    AssertEquals(0, QueryInt('select count(*) from traducoes'));

    SourceVerse.Sintagmas[0].AddToSelection;
    DestinationVerse.Sintagmas[0].AddToSelection;
    SourceVerse.AssociarSintagmas;
    AssertTrue(Workspace.SaveAssociations(FManager, SourceVerse,
      DestinationVerse));
    AssertEquals('changed', QueryString(
      'select lori_locucao from locucoes_origem'));

    DestinationVerse.AlterarTexto('translated');
    AssertTrue(Workspace.SaveAssociations(FManager, SourceVerse,
      DestinationVerse));
    AssertEquals(0, QueryInt('select count(*) from traducoes'));
  finally
    Workspace.Free;
    SourceVerse.Free;
    DestinationVerse.Free;
  end;
end;

procedure TSuggestionManagerTest.WorkspaceInvalidLoadIsAtomicAndRecoverable;
var
  DestinationVerse: TVerse;
  FailureReported: Boolean;
  RenderBuffer: TVerseRenderBuffer;
  SourceVerse: TVerse;
  Workspace: TCurrentVerseWorkspace;
begin
  Workspace := TCurrentVerseWorkspace.Create;
  RenderBuffer := TVerseRenderBuffer.Create;
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.VersiculoPar := DestinationVerse;
    SourceVerse.Texto := 'first second';
    DestinationVerse.Texto := 'target';
    FManager.AddPair('unrelated', 'keep');

    FailureReported := False;
    try
      Workspace.ApplyLoadedAssociations(SourceVerse,
        '<par a="0" b="0"><par a="2" b="99">', RenderBuffer);
    except
      FailureReported := True;
    end;

    AssertTrue('Incomplete stored associations must be reported',
      FailureReported);
    AssertTrue('The workspace must remember that recovery is required',
      Workspace.LoadedAssociationsInvalid);
    AssertEquals('A corrupt suffix must not leave its valid prefix visible',
      '', SourceVerse.Pares);

    // This is what the explicit Clear command does even though the atomic
    // load deliberately left no partial associations in the Verse Pair.
    SourceVerse.Modificado := True;
    AssertTrue('The corrected empty set must replace the corrupt canonical row',
      Workspace.SaveAssociations(FManager, SourceVerse, DestinationVerse));
    AssertFalse(Workspace.LoadedAssociationsInvalid);
    AssertEquals('Recovery must not subtract pairs contributed by other verses',
      1, FManager.GetPairOccurrenceCount('unrelated', 'keep'));
  finally
    DestinationVerse.Free;
    SourceVerse.Free;
    RenderBuffer.Free;
    Workspace.Free;
  end;
end;

procedure TSuggestionManagerTest.TranslationPopupUsesOccurrenceSum;
var
  SourceVerse: TVerse;
  Alternatives: string;
begin
  SourceVerse := TVerse.Create;
  try
    SourceVerse.Texto := 'source<WG1>';
    FManager.AddPair('<wg1>', 'one');
    FManager.AddPair('<wg1>', 'one');
    FManager.AddPair('<wg1>', 'two');

    Alternatives := FManager.GetTranslationAlternatives(
      SourceVerse.Sintagmas[0]);
    AssertTrue(Pos('one (2)', Alternatives) > 0);
    AssertTrue(Pos('two (1)', Alternatives) > 0);
  finally
    SourceVerse.Free;
  end;
end;

procedure TSuggestionManagerTest.
  StructuredTranslationAlternativesPreserveCountsAndOrder;
var
  Alternatives: TTranslationAlternativeDataArray;
begin
  FManager.AddPair('<wg1>', 'two');
  FManager.AddPair('<wg1>', 'one');
  FManager.AddPair('<wg1>', 'one');

  Alternatives := FManager.GetTranslationAlternativeData('<wg1>');
  AssertEquals(2, Length(Alternatives));
  AssertEquals('one', Alternatives[0].Translation);
  AssertEquals(2, Alternatives[0].Occurrences);
  AssertEquals('two', Alternatives[1].Translation);
  AssertEquals(1, Alternatives[1].Occurrences);
end;

procedure TSuggestionManagerTest.
  PreloadedTranslationAlternativesAreCompleteAndInvalidated;
var
  Alternatives: TTranslationAlternativeDataArray;
  Keys: TStringList;
begin
  FManager.AddPair('<wg1><wtn-a>', 'one');
  FManager.AddPair('<wg1><wtn-b>', 'one');
  FManager.AddPair('<wg2><wtn-a>', 'other');

  Keys := TStringList.Create;
  try
    Keys.Add('<wg1>');
    Keys.Add('<wg2>');
    FManager.PreloadTranslationAlternativeData(Keys);
  finally
    Keys.Free;
  end;

  Alternatives := FManager.GetTranslationAlternativeData('<wg1>');
  AssertEquals(1, Length(Alternatives));
  AssertEquals('one', Alternatives[0].Translation);
  AssertEquals(2, Alternatives[0].Occurrences);
  Alternatives := FManager.GetTranslationAlternativeData('<wg2>');
  AssertEquals(1, Length(Alternatives));
  AssertEquals('other', Alternatives[0].Translation);

  FManager.AddPair('<wg1><wtn-c>', 'two');
  Alternatives := FManager.GetTranslationAlternativeData('<wg1>');
  AssertEquals('A memory mutation must invalidate preloaded alternatives',
    2, Length(Alternatives));
  AssertEquals('one', Alternatives[0].Translation);
  AssertEquals(2, Alternatives[0].Occurrences);
  AssertEquals('two', Alternatives[1].Translation);
  AssertEquals(1, Alternatives[1].Occurrences);
end;

procedure TSuggestionManagerTest.
  TranslationMemoryLookupDoesNotMatchTextPrefixes;
var
  Alternatives: TTranslationAlternativeDataArray;
  Keys: TStringList;
begin
  FManager.AddPair('man', 'homem');
  FManager.AddPair('many', 'muitas');
  FManager.AddPair('many', 'muitas');
  FManager.AddPair('manifest', 'manifesta');

  Alternatives := FManager.GetTranslationAlternativeData('man');
  AssertEquals('The direct lookup must not include longer words',
    1, Length(Alternatives));
  AssertEquals('homem', Alternatives[0].Translation);
  AssertEquals('man', Alternatives[0].Matches[0].SourcePhrase);

  // A mutation invalidates the direct-query cache so the same assertions
  // exercise the batch preload path below.
  FManager.AddPair('unrelated', 'other');
  Keys := TStringList.Create;
  try
    Keys.Add('man');
    Keys.Add('many');
    Keys.Add('manifest');
    FManager.PreloadTranslationAlternativeData(Keys);
  finally
    Keys.Free;
  end;

  Alternatives := FManager.GetTranslationAlternativeData('man');
  AssertEquals('The preloaded lookup must not include longer words',
    1, Length(Alternatives));
  AssertEquals('homem', Alternatives[0].Translation);
  AssertEquals(1, Alternatives[0].Occurrences);

  Alternatives := FManager.GetTranslationAlternativeData('many');
  AssertEquals(1, Length(Alternatives));
  AssertEquals('muitas', Alternatives[0].Translation);
  AssertEquals(2, Alternatives[0].Occurrences);
  AssertEquals('many', Alternatives[0].Matches[0].SourcePhrase);

  Alternatives := FManager.GetTranslationAlternativeData('manifest');
  AssertEquals(1, Length(Alternatives));
  AssertEquals('manifesta', Alternatives[0].Translation);
end;

procedure TSuggestionManagerTest.
  TranslationMemoryModelGroupsAndMarksCurrentAlternatives;
var
  SourceVerse: TVerse;
  DestinationVerse: TVerse;
  Entries: TTranslationMemoryEntryArray;
begin
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.VersiculoPar := DestinationVerse;
    SourceVerse.Texto :=
      'λόγος<WG3056><WTN-NSM le="λόγος"> ' +
      'λόγου<WG3056><WTN-GSM le="λόγος"> ' +
      'καί<WG2532><WTCONJ le="καί"> plain plain ' +
      'both<WG1><WG2>! none';
    DestinationVerse.Texto := 'palavra mas atual inédita';

    SourceVerse.Sintagmas[0].AddToSelection;
    SourceVerse.Sintagmas[2].AddToSelection;
    DestinationVerse.Sintagmas[0].AddToSelection;
    SourceVerse.AssociarSintagmas;
    SourceVerse.LimparSelecao;
    DestinationVerse.LimparSelecao;

    SourceVerse.Sintagmas[4].AddToSelection;
    DestinationVerse.Sintagmas[2].AddToSelection;
    SourceVerse.AssociarSintagmas;
    SourceVerse.LimparSelecao;
    DestinationVerse.LimparSelecao;

    SourceVerse.Sintagmas[6].AddToSelection;
    DestinationVerse.Sintagmas[6].AddToSelection;
    SourceVerse.AssociarSintagmas;

    FManager.AddPair('<wg3056>', 'termo');
    FManager.AddPair('<wg3056>', 'termo');
    FManager.AddPair('<wg3056>', 'termo');
    FManager.AddPair('<wg3056>', 'palavra');
    FManager.AddPair('<wg3056>', 'palavra');
    FManager.AddPair('<wg2532>', 'porém');
    FManager.AddPair('<wg2532>', 'porém');
    FManager.AddPair('<wg2532>', 'porém');
    FManager.AddPair('<wg2532>', 'porém');
    FManager.AddPair('<wg2532>', 'mas');
    FManager.AddPair('plain', 'usual');
    FManager.AddPair('plain', 'usual');
    FManager.AddPair('<wg1><wg2>', 'combined');

    Entries := TTranslationMemoryModel.Build(SourceVerse, FManager);
    AssertEquals(5, Length(Entries));

    AssertEquals('λόγος', Entries[0].DisplayTerm);
    AssertEquals(2, Length(Entries[0].Forms));
    AssertEquals('λόγος', Entries[0].Forms[0]);
    AssertEquals('λόγου', Entries[0].Forms[1]);
    AssertEquals(2, Length(Entries[0].SyntagmIndexes));
    AssertEquals('Repeated forms must query a Strong key only once',
      3, Entries[0].Alternatives[0].Occurrences);
    AssertEquals('termo', Entries[0].Alternatives[0].Translation);
    AssertFalse(Entries[0].Alternatives[0].IsCurrent);
    AssertEquals(2, Entries[0].Alternatives[1].Occurrences);
    AssertEquals('palavra', Entries[0].Alternatives[1].Translation);
    AssertTrue(Entries[0].Alternatives[1].IsCurrent);
    AssertEquals(5, Entries[0].HistoricalOccurrences);

    AssertEquals('καί', Entries[1].DisplayTerm);
    AssertEquals('porém', Entries[1].Alternatives[0].Translation);
    AssertEquals(4, Entries[1].Alternatives[0].Occurrences);
    AssertFalse(Entries[1].Alternatives[0].IsCurrent);
    AssertEquals('mas', Entries[1].Alternatives[1].Translation);
    AssertTrue(Entries[1].Alternatives[1].IsCurrent);
    AssertEquals(5, Entries[1].HistoricalOccurrences);

    AssertEquals('plain', Entries[2].DisplayTerm);
    AssertEquals(2, Length(Entries[2].SyntagmIndexes));
    AssertEquals('usual', Entries[2].Alternatives[0].Translation);
    AssertEquals(2, Entries[2].Alternatives[0].Occurrences);
    AssertFalse(Entries[2].Alternatives[0].IsCurrent);
    AssertEquals('Current translations without history must not be listed',
      1, Length(Entries[2].Alternatives));
    AssertEquals(2, Entries[2].HistoricalOccurrences);

    AssertEquals('both', Entries[3].DisplayTerm);
    AssertEquals(2, Length(Entries[3].Strongs));
    AssertEquals('G1', Entries[3].Strongs[0]);
    AssertEquals('G2', Entries[3].Strongs[1]);
    AssertEquals('combined', Entries[3].Alternatives[0].Translation);
    AssertEquals(1, Entries[3].HistoricalOccurrences);

    AssertEquals('none', Entries[4].DisplayTerm);
    AssertEquals(0, Length(Entries[4].Alternatives));
    AssertEquals(0, Entries[4].HistoricalOccurrences);
  finally
    DestinationVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TSuggestionManagerTest.
  TranslationMemoryModelUsesMetadataWithoutStrong;
var
  DestinationVerse: TVerse;
  Entries: TTranslationMemoryEntryArray;
  MemoryKey: string;
  SourceVerse: TVerse;
begin
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.VersiculoPar := DestinationVerse;
    SourceVerse.Texto := 'δοῦλος<WTnnms l="δοῦλος">';
    DestinationVerse.Texto := 'servo';

    SourceVerse.Sintagmas[0].AddToSelection;
    DestinationVerse.Sintagmas[0].AddToSelection;
    SourceVerse.AssociarSintagmas;

    AssertEquals('Without Strong, plStrong falls back to text',
      'δοῦλος', SourceVerse.Sintagmas[0].GetSuggestionKey(plStrong));
    MemoryKey := SourceVerse.Sintagmas[0].GetSuggestionKey(plMetaData);
    AssertEquals('<wtnnms l="δοῦλος">', MemoryKey);
    FManager.AddPair(MemoryKey, 'servo');

    Entries := TTranslationMemoryModel.Build(SourceVerse, FManager);

    AssertEquals(1, Length(Entries));
    AssertEquals(1, Length(Entries[0].Alternatives));
    AssertEquals('servo', Entries[0].Alternatives[0].Translation);
    AssertEquals(1, Entries[0].Alternatives[0].Occurrences);
    AssertTrue(Entries[0].Alternatives[0].IsCurrent);
  finally
    DestinationVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TSuggestionManagerTest.
  TranslationMemoryModelMatchesCurrentSuffixHyphen;
var
  DestinationVerse: TVerse;
  Entries: TTranslationMemoryEntryArray;
  SourceVerse: TVerse;
begin
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.VersiculoPar := DestinationVerse;
    SourceVerse.Texto := 'word<WG1>';
    DestinationVerse.Texto := 'que afligem-vos';

    SourceVerse.Sintagmas[0].AddToSelection;
    DestinationVerse.Sintagmas[0].AddToSelection;
    DestinationVerse.Sintagmas[2].AddToSelection;
    SourceVerse.AssociarSintagmas;

    AssertEquals('The text key must retain the suffix boundary marker',
      'afligem-', DestinationVerse.Sintagmas[2].GetSuggestionKey(plText));
    AssertEquals('The metadata fallback does not retain the marker',
      'afligem', DestinationVerse.Sintagmas[2].GetSuggestionKey(plMetaData));

    FManager.AddPair('<wg1>', 'que;afligem');
    FManager.AddPair('<wg1>', 'que;afligem-');
    Entries := TTranslationMemoryModel.Build(SourceVerse, FManager);

    AssertEquals(1, Length(Entries));
    AssertEquals('Suffix boundary variants must be consolidated',
      1, Length(Entries[0].Alternatives));
    AssertEquals('que afligem', Entries[0].Alternatives[0].Translation);
    AssertEquals(2, Entries[0].Alternatives[0].Occurrences);
    AssertEquals('Both exact memory pairs must remain navigable',
      2, Length(Entries[0].Alternatives[0].Matches));
    AssertTrue('The historical alternative must be marked as current',
      Entries[0].Alternatives[0].IsCurrent);
  finally
    DestinationVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TSuggestionManagerTest.
  TranslationMemoryModelSortsByHistoricalFrequency;
var
  SourceVerse: TVerse;
  DestinationVerse: TVerse;
  Entries: TTranslationMemoryEntryArray;
begin
  SourceVerse := TVerse.Create;
  DestinationVerse := TVerse.Create;
  try
    SourceVerse.VersiculoPar := DestinationVerse;
    SourceVerse.Texto :=
      'common<WG10><WTN-NSM le="shared"> ' +
      'commonform<WG10><WTN-GSM le="shared"> ' +
      'rare<WG20> unseen<WG30> tied<WG40>';
    DestinationVerse.Texto := 'new';

    SourceVerse.Sintagmas[6].AddToSelection;
    DestinationVerse.Sintagmas[0].AddToSelection;
    SourceVerse.AssociarSintagmas;

    FManager.AddPair('<wg10>', 'common translation');
    FManager.AddPair('<wg10>', 'common translation');
    FManager.AddPair('<wg10>', 'common translation');
    FManager.AddPair('<wg20>', 'rare translation');
    FManager.AddPair('<wg40>', 'tied translation');

    Entries := TTranslationMemoryModel.Build(SourceVerse, FManager);
    AssertEquals(4, Length(Entries));
    AssertEquals('shared', Entries[0].DisplayTerm);
    AssertEquals('rare', Entries[1].DisplayTerm);
    AssertEquals('unseen', Entries[2].DisplayTerm);
    AssertEquals('tied', Entries[3].DisplayTerm);
    AssertEquals('Repeated forms must not multiply historical frequency',
      3, Entries[0].HistoricalOccurrences);
    AssertEquals(2, Length(Entries[0].SyntagmIndexes));
    AssertEquals(0, Entries[2].HistoricalOccurrences);
    AssertEquals('Alternatives without historical occurrences are hidden',
      0, Length(Entries[2].Alternatives));

    Entries := TTranslationMemoryModel.Build(SourceVerse, FManager,
      tmoMemoryFrequencyAscending);
    AssertEquals('unseen', Entries[0].DisplayTerm);
    AssertEquals('rare', Entries[1].DisplayTerm);
    AssertEquals('tied', Entries[2].DisplayTerm);
    AssertEquals('shared', Entries[3].DisplayTerm);
    AssertEquals(0, Entries[0].HistoricalOccurrences);
    AssertEquals(1, Entries[1].HistoricalOccurrences);
    AssertEquals(1, Entries[2].HistoricalOccurrences);
    AssertEquals(3, Entries[3].HistoricalOccurrences);
  finally
    DestinationVerse.Free;
    SourceVerse.Free;
  end;
end;

procedure TSuggestionManagerTest.TranslationMemoryOrderValueFallsBackSafely;
begin
  AssertEquals(Ord(tmoVerseOccurrence),
    Ord(TranslationMemoryOrderFromInteger(-1)));
  AssertEquals(Ord(tmoVerseOccurrence),
    Ord(TranslationMemoryOrderFromInteger(Ord(High(TTranslationMemoryOrder)) + 1)));
  AssertEquals(Ord(tmoMemoryFrequencyAscending),
    Ord(TranslationMemoryOrderFromInteger(
      Ord(tmoMemoryFrequencyAscending))));
end;

procedure TSuggestionManagerTest.OccurrenceIndexTracksReferencesAndPagination;
var
  Alternatives: TTranslationAlternativeDataArray;
  Page: TTranslationOccurrencePage;
begin
  FManager.AddPairForReference('1,1,1', '<wg1>', 'word');
  FManager.AddPairForReference('1,1,1', '<wg1>', 'word');
  FManager.AddPairForReference('1,1,2', '<wg1>', 'word');

  Alternatives := FManager.GetTranslationAlternativeData('<wg1>');
  AssertEquals(1, Length(Alternatives));
  AssertEquals(3, Alternatives[0].Occurrences);
  AssertEquals(1, Length(Alternatives[0].Matches));

  Page := FManager.GetTranslationOccurrences(
    Alternatives[0].Matches, 1);
  AssertEquals(3, Page.TotalAssociations);
  AssertEquals(2, Page.TotalVerses);
  AssertTrue(Page.HasMore);
  AssertEquals(1, Length(Page.Items));
  AssertEquals('1,1,1', Page.Items[0].ReferenceID);
  AssertEquals(2, Page.Items[0].Occurrences);
  AssertEquals('first destination', Page.Items[0].DestinationText);

  Page := FManager.GetTranslationOccurrences(
    Alternatives[0].Matches, 100);
  AssertFalse(Page.HasMore);
  AssertEquals(2, Length(Page.Items));
  AssertEquals('1,1,2', Page.Items[1].ReferenceID);

  FManager.RemovePairForReference('1,1,1', '<wg1>', 'word');
  Page := FManager.GetTranslationOccurrences(
    Alternatives[0].Matches, 100);
  AssertEquals(2, Page.TotalAssociations);
  AssertEquals(1, Page.Items[0].Occurrences);

  FManager.RemovePairForReference('1,1,1', '<wg1>', 'word');
  FManager.RemovePairForReference('1,1,1', '<wg1>', 'word');
  Page := FManager.GetTranslationOccurrences(
    Alternatives[0].Matches, 100);
  AssertEquals(1, Page.TotalAssociations);
  AssertEquals(1, Page.TotalVerses);
  AssertEquals('1,1,2', Page.Items[0].ReferenceID);
end;

procedure TSuggestionManagerTest.OccurrenceIndexConsolidatesExactMatches;
var
  Alternatives: TTranslationAlternativeDataArray;
  Page: TTranslationOccurrencePage;
begin
  FManager.AddPairForReference('1,1,1', '<wg1><wtn-a>', 'word');
  FManager.AddPairForReference('1,1,2', '<wg1><wtn-b>', 'word');

  Alternatives := FManager.GetTranslationAlternativeData('<wg1>');
  AssertEquals(1, Length(Alternatives));
  AssertEquals(2, Alternatives[0].Occurrences);
  AssertEquals('Equivalent display alternatives must retain both exact pairs',
    2, Length(Alternatives[0].Matches));

  Page := FManager.GetTranslationOccurrences(
    Alternatives[0].Matches, 100);
  AssertEquals(2, Page.TotalAssociations);
  AssertEquals(2, Page.TotalVerses);
  AssertEquals(2, Length(Page.Items));
end;

procedure TSuggestionManagerTest.
  AffectedPairValidationChecksOnlyTheChangedPair;
var
  ErrorMessage: string;
begin
  FManager.AddPairForReference('1,1,1', '<wg1>', 'word');
  AssertTrue(FManager.ValidatePairConsistency('<wg1>', 'word', 1,
    ErrorMessage));

  FConnection.ExecuteDirect(
    'update association_memory_occurrences set amo_occurrences = 2 ' +
    'where amo_reference = ''1,1,1'' and amo_source = ''<wg1>'' ' +
    'and amo_target = ''word''');
  AssertFalse('A mismatch in the affected pair must be detected',
    FManager.ValidatePairConsistency('<wg1>', 'word', 1, ErrorMessage));
  AssertFalse(ErrorMessage.IsEmpty);

  FConnection.ExecuteDirect(
    'update association_memory_occurrences set amo_occurrences = 1 ' +
    'where amo_reference = ''1,1,1'' and amo_source = ''<wg1>'' ' +
    'and amo_target = ''word''');
  FConnection.ExecuteDirect(
    'insert into association_memory_occurrences values ' +
    '(''missing'', ''unrelated'', ''broken'', -1)');
  AssertTrue('Incremental validation must not scan unrelated cache rows',
    FManager.ValidatePairConsistency('<wg1>', 'word', 1, ErrorMessage));
end;

procedure TSuggestionManagerTest.OccurrenceIndexRollbackPreservesPreviousData;
var
  Alternatives: TTranslationAlternativeDataArray;
  Page: TTranslationOccurrencePage;
begin
  FManager.AddPairForReference('1,1,1', '<wg1>', 'word');
  FManager.BeginRebuild;
  try
    FManager.AddPairForReference('1,1,2', '<wg1>', 'word');
    FManager.RollbackRebuild;
  except
    FManager.RollbackRebuild;
    raise;
  end;

  Alternatives := FManager.GetTranslationAlternativeData('<wg1>');
  Page := FManager.GetTranslationOccurrences(
    Alternatives[0].Matches, 100);
  AssertEquals(1, Page.TotalAssociations);
  AssertEquals(1, Page.TotalVerses);
  AssertEquals('1,1,1', Page.Items[0].ReferenceID);
end;

procedure TSuggestionManagerTest.
  PreparedRebuildValidationDetectsOccurrenceMismatch;
var
  ErrorMessage: string;
begin
  FManager.BeginRebuild;
  try
    FManager.ClearAssociationDatabase;
    FManager.AddPairForReference('1,1,1', '<wg1>', 'word');
    FManager.PrepareRebuildValidation;
    FConnection.ExecuteDirect('update association_memory_occurrences set ' +
      'amo_occurrences = 2');
    AssertFalse(FManager.ValidatePreparedRebuild(ErrorMessage));
  finally
    FManager.RollbackRebuild;
  end;
end;

procedure TSuggestionManagerTest.
  ConcordanceOccurrenceEnumerationUsesReferenceIndex;
begin
  FManager.AddPairForReference('1,1,1', '<wg1>', 'word');
  FManager.AddPairForReference('1,1,1', '<wg1>', 'word');
  FManager.AddPairForReference('1,1,2', '<wg2>', 'other');

  AssertEquals('The export progress count represents indexed rows',
    2, FManager.IndexedConcordanceOccurrenceCount);
  FManager.EnumerateIndexedConcordanceOccurrences(
    @CollectConcordanceOccurrence);
  AssertEquals(2, FConcordanceOccurrences.Count);
  AssertEquals('1,1,1|<wg1>|word|2|1', FConcordanceOccurrences[0]);
  AssertEquals('1,1,2|<wg2>|other|1|2', FConcordanceOccurrences[1]);
end;

procedure TSuggestionManagerTest.
  ConcordanceOccurrenceIndexReportsUnsafeFallbacks;
var
  References: TStringList;
begin
  FManager.AddPairForReference('1,1,1', '<wg1>', '<font>word</font>');
  AssertFalse(FManager.IndexedConcordanceHasTaggedTargets);
  FManager.AddPairForReference('1,1,2', '<wg2>', '<wtfoo>');
  AssertTrue(FManager.IndexedConcordanceHasTaggedTargets);

  FConnection.ExecuteDirect(
    'update pares set pare_pares = ''<par a="0" b="0">'' ' +
    'where pare_id = ''1,1,3''');
  References := FManager.GetUnindexedAssociatedReferences;
  try
    AssertEquals(1, References.Count);
    AssertEquals('1,1,3', References[0]);
  finally
    References.Free;
  end;
end;

initialization
  RegisterTest(TSuggestionManagerTest);

end.
