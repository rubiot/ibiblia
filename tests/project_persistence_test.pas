unit project_persistence_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, fpcunit, testregistry, sqlite3conn, sqldb,
  Verse, current_verse_workspace, project_persistence, project_config;

type
  TProjectPersistenceTest = class(TTestCase)
  private
    FFileName: string;
    FPersistence: TProjectPersistence;
    procedure CreateDatabase;
    function ReadDatabaseInfo(const Key: string): string;
    function ReadDatabasePairField(const FieldName: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ImmediateInfoIsDurableAndDoesNotMarkContentPending;
    procedure StagedInfoCommitsAndRollsBackIndependently;
    procedure PairAssignmentsTrackOnlyNetChanges;
    procedure EmptyValuesRemainTrackable;
    procedure ImmediateInfoFlushesAfterACompetingWriterFinishes;
    procedure WorkspaceDetectsAllVisibleContentChanges;
    procedure TypedConfigSeparatesMarkerAndTokenizer;
  end;

implementation

procedure TProjectPersistenceTest.CreateDatabase;
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
    Connection.ExecuteDirect(
      'create table info (id text primary key, valor text)');
    Connection.ExecuteDirect('create table pares (' +
      'pare_id text primary key, pare_ref text, pare_texto_origem text, ' +
      'pare_texto_destino text, pare_texto_consulta1 text, ' +
      'pare_texto_consulta2 text, pare_pares text, pare_situacao number, ' +
      'pare_comentarios text)');
    Connection.ExecuteDirect(
      'insert into info values (''option'', ''old-option'')');
    Connection.ExecuteDirect(
      'insert into info values (''structural'', ''old-structure'')');
    Connection.ExecuteDirect('insert into pares values (' +
      '''1,1,1'', ''Gen 1:1'', ''source'', ''[target]'', ''query 1'', ' +
      '''query 2'', '''', 0, ''comment'')');
    Transaction.Commit;
  finally
    Transaction.Free;
    Connection.Free;
  end;
end;

function TProjectPersistenceTest.ReadDatabaseInfo(const Key: string): string;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
begin
  Result := '';
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FFileName;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text := 'select valor from info where id = :id';
    Connection.Open;
    Transaction.StartTransaction;
    Query.ParamByName('id').AsString := Key;
    Query.Open;
    if not Query.EOF then
      Result := Query.Fields[0].AsString;
    Query.Close;
    Transaction.Rollback;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

function TProjectPersistenceTest.ReadDatabasePairField(
  const FieldName: string): string;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
begin
  Result := '';
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FFileName;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text := 'select ' + FieldName +
      ' from pares where pare_id = ''1,1,1''';
    Connection.Open;
    Transaction.StartTransaction;
    Query.Open;
    if not Query.EOF then
      Result := Query.Fields[0].AsString;
    Query.Close;
    Transaction.Rollback;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TProjectPersistenceTest.SetUp;
begin
  inherited SetUp;
  FFileName := GetTempFileName(GetTempDir, 'ipp');
  CreateDatabase;
  FPersistence := TProjectPersistence.Create;
  FPersistence.Open(FFileName);
end;

procedure TProjectPersistenceTest.TearDown;
begin
  FreeAndNil(FPersistence);
  if FileExists(FFileName) then
    DeleteFile(FFileName);
  inherited TearDown;
end;

procedure TProjectPersistenceTest.
  ImmediateInfoIsDurableAndDoesNotMarkContentPending;
begin
  AssertFalse(FPersistence.ContentUpdatesPending);
  AssertTrue(FPersistence.WriteInfoImmediate('option', 'new-option'));
  AssertEquals('new-option', FPersistence.ReadInfo('option'));
  AssertEquals('new-option', ReadDatabaseInfo('option'));
  AssertFalse('Immediate settings must not make project content dirty',
    FPersistence.ContentUpdatesPending);

  AssertFalse('An identical immediate assignment must be ignored',
    FPersistence.WriteInfoImmediate('option', 'new-option'));
  AssertFalse(FPersistence.ContentUpdatesPending);
end;

procedure TProjectPersistenceTest.
  StagedInfoCommitsAndRollsBackIndependently;
begin
  FPersistence.WriteInfoImmediate('option', 'survives-rollback');
  AssertTrue(FPersistence.StageInfo('structural', 'pending-structure'));
  AssertTrue(FPersistence.ContentUpdatesPending);
  AssertEquals('pending-structure', FPersistence.ReadInfo('structural'));
  AssertEquals('old-structure', ReadDatabaseInfo('structural'));

  FPersistence.Rollback;
  AssertFalse(FPersistence.ContentUpdatesPending);
  AssertEquals('old-structure', FPersistence.ReadInfo('structural'));
  AssertEquals('survives-rollback', ReadDatabaseInfo('option'));

  FPersistence.StageInfo('structural', 'committed-structure');
  FPersistence.CommitInfo;
  AssertFalse(FPersistence.ContentUpdatesPending);
  AssertEquals('committed-structure', ReadDatabaseInfo('structural'));
end;

procedure TProjectPersistenceTest.PairAssignmentsTrackOnlyNetChanges;
begin
  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'source');
  FPersistence.PostCurrentEdit;
  AssertFalse('Identical pair assignments must be ignored',
    FPersistence.ContentUpdatesPending);

  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'changed');
  FPersistence.PostCurrentEdit;
  AssertTrue(FPersistence.ContentUpdatesPending);
  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'source');
  FPersistence.PostCurrentEdit;
  AssertFalse('Restoring the original text must clear pending state',
    FPersistence.ContentUpdatesPending);

  FPersistence.SetCurrentFieldAsInteger('pare_situacao', 2);
  FPersistence.PostCurrentEdit;
  AssertTrue(FPersistence.ContentUpdatesPending);
  FPersistence.SetCurrentFieldAsInteger('pare_situacao', 0);
  FPersistence.PostCurrentEdit;
  AssertFalse('Restoring the original status must clear pending state',
    FPersistence.ContentUpdatesPending);

  FPersistence.CommitPairs;
  AssertEquals('source', ReadDatabasePairField('pare_texto_origem'));
  AssertEquals('0', ReadDatabasePairField('pare_situacao'));
end;

procedure TProjectPersistenceTest.EmptyValuesRemainTrackable;
begin
  AssertTrue(FPersistence.StageInfo('structural', ''));
  AssertTrue(FPersistence.ContentUpdatesPending);
  AssertEquals('', FPersistence.ReadInfo('structural'));
  FPersistence.CommitInfo;
  AssertFalse(FPersistence.ContentUpdatesPending);
  AssertEquals('', ReadDatabaseInfo('structural'));
end;

procedure TProjectPersistenceTest.
  ImmediateInfoFlushesAfterACompetingWriterFinishes;
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
    Connection.ExecuteDirect(
      'update pares set pare_ref = pare_ref where pare_id = ''1,1,1''');

    AssertTrue(FPersistence.WriteInfoImmediate('option', 'queued-option'));
    AssertEquals('queued-option', FPersistence.ReadInfo('option'));
    AssertFalse(FPersistence.ContentUpdatesPending);
    AssertEquals('old-option', ReadDatabaseInfo('option'));

    Transaction.Rollback;
    FPersistence.FlushImmediateInfo;
    AssertEquals('queued-option', ReadDatabaseInfo('option'));
  finally
    if Transaction.Active then
      Transaction.Rollback;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TProjectPersistenceTest.WorkspaceDetectsAllVisibleContentChanges;
var
  Workspace: TCurrentVerseWorkspace;
  Views: array[0..3] of TVerse;
  TextFields: array[0..3] of Integer;
  Comments: TMemo;
  Index: Integer;
begin
  Workspace := TCurrentVerseWorkspace.Create;
  Comments := TMemo.Create(nil);
  for Index := Low(Views) to High(Views) do
    Views[Index] := TVerse.Create;
  try
    TextFields[0] := FPersistence.PairsTable.FieldByName(
      'pare_texto_origem').Index;
    TextFields[1] := FPersistence.PairsTable.FieldByName(
      'pare_texto_destino').Index;
    TextFields[2] := FPersistence.PairsTable.FieldByName(
      'pare_texto_consulta1').Index;
    TextFields[3] := FPersistence.PairsTable.FieldByName(
      'pare_texto_consulta2').Index;
    for Index := Low(Views) to High(Views) do
    begin
      Views[Index].Texto := FPersistence.CurrentText(TextFields[Index]);
      Views[Index].Modificado := False;
      Views[Index].XMLModificado := False;
    end;
    Views[0].VersiculoPar := Views[1];
    Comments.Text := FPersistence.CurrentComments;
    Workspace.ApplyLoadedAssociations(Views[0], FPersistence.CurrentPairs);

    AssertFalse(Workspace.HasPendingChanges(FPersistence, nil, Comments,
      Views, TextFields, 0));

    Views[2].Texto := 'changed query 1';
    AssertTrue('Reference text 1 must participate in pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));
    Views[2].Texto := 'query 1';
    AssertFalse('Restoring reference text 1 must clear pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));

    Views[3].Texto := 'changed query 2';
    AssertTrue('Reference text 2 must participate in pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));
    Views[3].Texto := 'query 2';
    AssertFalse(Workspace.HasPendingChanges(FPersistence, nil, Comments,
      Views, TextFields, 0));

    Comments.Text := 'changed comment';
    AssertTrue('An open comment value must participate in pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));
    Comments.Text := 'comment';
    AssertFalse(Workspace.HasPendingChanges(FPersistence, nil, Comments,
      Views, TextFields, 0));

    Views[0].Sintagmas[0].AddToSelection;
    Views[1].Sintagmas[0].AddToSelection;
    Views[0].AssociarSintagmas;
    AssertTrue(
      'Associations with bracketed/italic syntagms must participate in pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));
    Views[0].LimparAssociacoes;
    AssertFalse('Restoring the original association set must clear pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));

    Views[0].Sintagmas[0].AddToSelection;
    Views[1].Sintagmas[0].AddToSelection;
    Views[0].AssociarSintagmas;
    AssertTrue('Saving must prepare an association containing an italic syntagm',
      Workspace.SaveBeforeScroll(FPersistence, nil, nil, Comments, Views,
        TextFields, 0, 1));
    AssertTrue(FPersistence.ContentUpdatesPending);
    AssertEquals(Views[0].Pares, FPersistence.CurrentPairs);
  finally
    for Index := Low(Views) to High(Views) do
      Views[Index].Free;
    Comments.Free;
    Workspace.Free;
  end;
end;

procedure TProjectPersistenceTest.TypedConfigSeparatesMarkerAndTokenizer;
var
  Config: TProjectConfig;
begin
  Config := TProjectConfig.Create(FPersistence);
  try
    Config.WriteMarker('1,1,1');
    AssertFalse('The navigation marker is immediate metadata',
      FPersistence.ContentUpdatesPending);
    AssertEquals('1,1,1', ReadDatabaseInfo(PROJECT_CONFIG_KEY_MARKER));

    Config.WriteTokenizerVersion(3);
    AssertTrue('Tokenizer version remains structural project content',
      FPersistence.ContentUpdatesPending);
    FPersistence.Rollback;
    AssertEquals('', ReadDatabaseInfo(PROJECT_CONFIG_KEY_TOKENIZER_VERSION));
    AssertEquals('1,1,1', ReadDatabaseInfo(PROJECT_CONFIG_KEY_MARKER));
  finally
    Config.Free;
  end;
end;

initialization
  RegisterTest(TProjectPersistenceTest);

end.
