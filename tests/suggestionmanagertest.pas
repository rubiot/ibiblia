unit suggestionmanagertest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, sqlite3conn, sqldb,
  SuggestionManager, current_verse_workspace, verse_render_buffer, Verse,
  Syntagm;

type
  TSuggestionManagerTest = class(TTestCase)
  private
    FConnection: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FManager: TSuggestionManager;
    function QueryInt(const SQL: string): Integer;
    function QueryString(const SQL: string): string;
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
    procedure ValidatePairsUsesPairMultiplicity;
    procedure WorkspacePersistsIndividualCancellationAndIsIdempotent;
    procedure WorkspaceAutosaveKeepsAssociationBaseline;
    procedure WorkspaceSkipsUninitializedVersePair;
    procedure WorkspaceReconcilesDuplicatePairsAsMultiset;
    procedure WorkspaceReconcilesSourceAndDestinationTextChanges;
    procedure WorkspaceInvalidLoadIsAtomicAndRecoverable;
    procedure TranslationPopupUsesOccurrenceSum;
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
  FConnection.ExecuteDirect(
    'create unique index trad_uk on traducoes(trad_lori_id, trad_ldes_id)');
  FConnection.ExecuteDirect(
    'create unique index slor_uk on sintagmas_locucoes_origem' +
    '(slor_lori_id, slor_sori_id, slor_ordem)');
  FConnection.ExecuteDirect(
    'create unique index slde_uk on sintagmas_locucoes_destino' +
    '(slde_ldes_id, slde_sdes_id, slde_ordem)');

  FManager := TSuggestionManager.Create(nil, FConnection);
end;

procedure TSuggestionManagerTest.TearDown;
begin
  FManager.Free;
  if FTransaction.Active then
    FTransaction.Rollback;
  FConnection.Close;
  FConnection.Free;
  FTransaction.Free;
  inherited TearDown;
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

initialization
  RegisterTest(TSuggestionManagerTest);

end.
