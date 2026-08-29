unit project_persistence_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, StdCtrls, fpcunit, testregistry, sqlite3conn, sqldb,
  Verse, current_verse_workspace, project_persistence, project_config,
  project_verse_repository, CommentsView, text_direction, Project,
  theword_ont_codec_v2;

type
  TProjectPersistenceTest = class(TTestCase)
  private
    FFileName: string;
    FPersistence: TProjectPersistence;
    FPendingNotificationCount: Integer;
    FPairBeforeScrollCount: Integer;
    FPairAfterScrollCount: Integer;
    procedure CreateDatabase;
    function ReadDatabaseInfo(const Key: string): string;
    function ReadDatabasePairField(const FieldName: string): string;
    procedure PendingStateChanged(Sender: TObject);
    procedure PairBeforeScroll(DataSet: TDataSet);
    procedure PairAfterScroll(DataSet: TDataSet);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ImmediateInfoIsDurableAndDoesNotMarkContentPending;
    procedure StagedInfoCommitsAndRollsBackIndependently;
    procedure PairAssignmentsTrackOnlyNetChanges;
    procedure EmptyValuesRemainTrackable;
    procedure NetZeroPairChangeDoesNotEnterFailedCommitRecovery;
    procedure FailedPairCommitCanRetryAfterWriterReleases;
    procedure RollbackAfterFailedPairCommitRestoresDurableBaseline;
    procedure ImmediateInfoFlushesAfterACompetingWriterFinishes;
    procedure WorkspaceDetectsAllVisibleContentChanges;
    procedure TypedConfigSeparatesMarkerAndTokenizer;
    procedure TextDirectionConfigUsesStablePerTextKeys;
    procedure TextDirectionSetterDoesNotIndexNonVerseTypes;
    procedure CommentBreaksMapToAndFromWysiwygSession;
    procedure CommentWysiwygSaveCanonicalizesLegacyBreaks;
    procedure CommentPreviewProjectsBreaksAndPreservesOpaqueTags;
    procedure CommentPreviewSkipsNoOpAssignments;
    procedure UnknownEditorialMarkupUsesRawOnlyMode;
    procedure PendingStateNotificationsTrackNetChanges;
    procedure AutomaticRFCountUsesPendingRowsAndRestoresDatasetState;
  end;

implementation

procedure TProjectPersistenceTest.PendingStateChanged(Sender: TObject);
begin
  Inc(FPendingNotificationCount);
end;

procedure TProjectPersistenceTest.PairBeforeScroll(DataSet: TDataSet);
begin
  Inc(FPairBeforeScrollCount);
end;

procedure TProjectPersistenceTest.PairAfterScroll(DataSet: TDataSet);
begin
  Inc(FPairAfterScrollCount);
end;

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
    Connection.ExecuteDirect('insert into pares values (' +
      '''1,1,2'', ''Gen 1:2'', ''source 2'', ' +
      '''second<RF>two<Rf><RF q=x>custom<Rf><RF legacy=1>hidden<Rf>'', ' +
      '''query 1'', ''query 2'', '''', 0, '''')');
    Connection.ExecuteDirect('insert into pares values (' +
      '''1,1,3'', ''Gen 1:3'', ''source 3'', ' +
      '''third<RF>current<Rf>'', ''query 1'', ''query 2'', '''', 0, '''')');
    Connection.ExecuteDirect('insert into pares values (' +
      '''1,2,1'', ''Gen 2:1'', ''source 4'', ' +
      '''other chapter<RF>note<Rf>'', ''query 1'', ''query 2'', '''', 0, '''')');
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
  FPendingNotificationCount := 0;
  FPairBeforeScrollCount := 0;
  FPairAfterScrollCount := 0;
  FPersistence.OnContentPendingChanged := @PendingStateChanged;
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

procedure TProjectPersistenceTest.FailedPairCommitCanRetryAfterWriterReleases;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  FirstCommitFailed: Boolean;
begin
  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'changed');
  FPersistence.PostCurrentEdit;

  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FFileName;
    Connection.Open;
    Transaction.StartTransaction;
    Connection.ExecuteDirect(
      'update pares set pare_ref = pare_ref where pare_id = ''1,1,1''');

    FirstCommitFailed := False;
    try
      FPersistence.CommitPairs;
    except
      on E: Exception do
        FirstCommitFailed := True;
    end;
    AssertTrue('the competing SQLite writer must make the first commit fail',
      FirstCommitFailed);
    AssertTrue('a failed commit must remain visibly pending',
      FPersistence.ContentUpdatesPending);
    AssertEquals('the failed commit must not alter durable content', 'source',
      ReadDatabasePairField('pare_texto_origem'));

    Transaction.Rollback;

    FPersistence.CommitPairs;
    AssertFalse('the successful retry clears the tracked baseline',
      FPersistence.ContentUpdatesPending);
    AssertEquals('the retry must reconstruct and write the lost update',
      'changed',
      ReadDatabasePairField('pare_texto_origem'));
  finally
    if Transaction.Active then
      Transaction.Rollback;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TProjectPersistenceTest.
  NetZeroPairChangeDoesNotEnterFailedCommitRecovery;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'changed');
  FPersistence.PostCurrentEdit;
  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'source');
  FPersistence.PostCurrentEdit;
  AssertFalse('returning to the baseline is not a logical project change',
    FPersistence.ContentUpdatesPending);

  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FFileName;
    Connection.Open;
    Transaction.StartTransaction;
    Connection.ExecuteDirect(
      'update pares set pare_ref = pare_ref where pare_id = ''1,1,1''');

    // A net-zero cached update must be discarded without touching SQLite, so
    // an unrelated writer cannot turn it into a failed recovery state.
    FPersistence.CommitPairs;
    AssertFalse(FPersistence.ContentUpdatesPending);
    AssertEquals('source', FPersistence.CurrentFieldAsString(
      'pare_texto_origem'));
    AssertEquals('source', ReadDatabasePairField('pare_texto_origem'));
  finally
    if Transaction.Active then
      Transaction.Rollback;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TProjectPersistenceTest.
  RollbackAfterFailedPairCommitRestoresDurableBaseline;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  FirstCommitFailed: Boolean;
begin
  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'changed');
  FPersistence.PostCurrentEdit;

  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FFileName;
    Connection.Open;
    Transaction.StartTransaction;
    Connection.ExecuteDirect(
      'update pares set pare_ref = pare_ref where pare_id = ''1,1,1''');

    FirstCommitFailed := False;
    try
      FPersistence.CommitPairs;
    except
      on E: Exception do
        FirstCommitFailed := True;
    end;
    AssertTrue('the competing SQLite writer must make the commit fail',
      FirstCommitFailed);
    Transaction.Rollback;

    FPersistence.Rollback;
    AssertFalse('rollback clears the failed commit state only after restoring ' +
      'the durable baseline', FPersistence.ContentUpdatesPending);
    AssertEquals('source', FPersistence.CurrentFieldAsString(
      'pare_texto_origem'));
    AssertEquals('rollback must preserve the value from before the edit',
      'source', ReadDatabasePairField('pare_texto_origem'));
  finally
    if Transaction.Active then
      Transaction.Rollback;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TProjectPersistenceTest.PendingStateNotificationsTrackNetChanges;
begin
  AssertEquals(0, FPendingNotificationCount);

  FPersistence.StageInfo('structural', 'pending-structure');
  AssertEquals('Becoming dirty must notify the UI', 1,
    FPendingNotificationCount);
  FPersistence.StageInfo('structural', 'another-pending-structure');
  AssertEquals('Changing an already pending value must not duplicate the ' +
    'state notification', 1, FPendingNotificationCount);
  FPersistence.StageInfo('structural', 'old-structure');
  AssertEquals('Restoring the structural baseline must notify the UI', 2,
    FPendingNotificationCount);

  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'changed');
  FPersistence.PostCurrentEdit;
  AssertEquals('Posting the first content difference must notify the UI', 3,
    FPendingNotificationCount);
  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'changed again');
  FPersistence.PostCurrentEdit;
  AssertEquals(3, FPendingNotificationCount);
  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'source');
  FPersistence.PostCurrentEdit;
  AssertEquals('Restoring the content baseline must notify the UI', 4,
    FPendingNotificationCount);

  FPersistence.WriteInfoImmediate('option', 'new-option');
  AssertEquals('Immediate options must not affect the content indicator', 4,
    FPendingNotificationCount);
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
      Views[Index].Text := FPersistence.CurrentText(TextFields[Index]);
      Views[Index].Modified := False;
      Views[Index].XMLModified := False;
    end;
    Views[0].PairedVerse := Views[1];
    Comments.Text := FPersistence.CurrentComments;
    Workspace.ApplyLoadedAssociations(Views[0], FPersistence.CurrentPairs);

    AssertFalse(Workspace.HasPendingChanges(FPersistence, nil, Comments,
      Views, TextFields, 0));

    Views[2].Text := 'changed query 1';
    AssertTrue('Reference text 1 must participate in pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));
    Views[2].Text := 'query 1';
    AssertFalse('Restoring reference text 1 must clear pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));

    Views[3].Text := 'changed query 2';
    AssertTrue('Reference text 2 must participate in pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));
    Views[3].Text := 'query 2';
    AssertFalse(Workspace.HasPendingChanges(FPersistence, nil, Comments,
      Views, TextFields, 0));

    Comments.Text := 'changed comment';
    AssertTrue('An open comment value must participate in pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));
    Comments.Text := 'comment';
    AssertFalse(Workspace.HasPendingChanges(FPersistence, nil, Comments,
      Views, TextFields, 0));

    Views[0].Syntagms[0].AddToSelection;
    Views[1].Syntagms[0].AddToSelection;
    Views[0].AssociateSyntagms;
    AssertTrue(
      'Associations with bracketed/italic syntagms must participate in pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));
    Views[0].ClearAssociations;
    AssertFalse('Restoring the original association set must clear pending state',
      Workspace.HasPendingChanges(FPersistence, nil, Comments, Views,
        TextFields, 0));

    Views[0].Syntagms[0].AddToSelection;
    Views[1].Syntagms[0].AddToSelection;
    Views[0].AssociateSyntagms;
    AssertTrue('Saving must prepare an association containing an italic syntagm',
      Workspace.SaveBeforeScroll(FPersistence, nil, nil, Comments, Views,
        TextFields, 0, 1));
    AssertTrue(FPersistence.ContentUpdatesPending);
    AssertEquals(Views[0].Pairs, FPersistence.CurrentPairs);
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

    Config.StageMarkerForCommit('1,1,2');
    AssertFalse('staged navigation marker is not project content',
      FPersistence.ContentUpdatesPending);
    AssertEquals('staged marker is visible through typed configuration',
      '1,1,2', Config.ReadMarker(''));
    AssertEquals('staged marker is not written through a second connection',
      '1,1,1', ReadDatabaseInfo(PROJECT_CONFIG_KEY_MARKER));
    FPersistence.CommitInfo;
    AssertEquals('staged marker is published by the project commit',
      '1,1,2', ReadDatabaseInfo(PROJECT_CONFIG_KEY_MARKER));

    Config.StageMarkerForCommit('1,1,3');
    AssertFalse('a later staged marker remains silent',
      FPersistence.ContentUpdatesPending);
    AssertEquals('the later staged marker remains readable before rollback',
      '1,1,3', Config.ReadMarker(''));

    Config.WriteTokenizerVersion(3);
    AssertTrue('Tokenizer version remains structural project content',
      FPersistence.ContentUpdatesPending);
    FPersistence.Rollback;
    AssertEquals('', ReadDatabaseInfo(PROJECT_CONFIG_KEY_TOKENIZER_VERSION));
    AssertEquals('1,1,2', ReadDatabaseInfo(PROJECT_CONFIG_KEY_MARKER));
    AssertEquals('rollback restores the committed marker value',
      '1,1,2', Config.ReadMarker(''));
  finally
    Config.Free;
  end;
end;

procedure TProjectPersistenceTest.TextDirectionConfigUsesStablePerTextKeys;
var
  Config: TProjectConfig;
begin
  Config := TProjectConfig.Create(FPersistence);
  try
    AssertEquals('direction.text2', TProjectConfig.TextDirectionKey(2));
    AssertFalse('missing direction is not explicit',
      Config.HasTextDirection(0));
    AssertEquals('missing directions default to LTR', Ord(tdLeftToRight),
      Ord(Config.ReadTextDirection(0)));
    AssertEquals('callers may supply a migration default',
      Ord(tdRightToLeft),
      Ord(Config.ReadTextDirection(3, tdRightToLeft)));

    Config.WriteTextDirection(0, tdLeftToRight);
    Config.WriteTextDirection(1, tdRightToLeft);

    AssertTrue('saved LTR is explicit', Config.HasTextDirection(0));
    AssertTrue('saved RTL is explicit', Config.HasTextDirection(1));

    AssertEquals('ltr', FPersistence.ReadInfo('direction.text0'));
    AssertEquals('rtl', FPersistence.ReadInfo('direction.text1'));
    AssertEquals(Ord(tdLeftToRight),
      Ord(Config.ReadTextDirection(0, tdRightToLeft)));
    AssertEquals(Ord(tdRightToLeft),
      Ord(Config.ReadTextDirection(1)));
    AssertTrue('text direction is structural project content',
      FPersistence.ContentUpdatesPending);
  finally
    Config.Free;
  end;
end;

procedure TProjectPersistenceTest.TextDirectionSetterDoesNotIndexNonVerseTypes;
var
  ProjectInstance: TProject;
begin
  ProjectInstance := TProject.Create;
  try
    ProjectInstance.SetTextDirection(tbInterlinear, tdRightToLeft);
    ProjectInstance.SetTextDirection(tbNone, tdRightToLeft);
    AssertEquals('a detached project still uses the safe default',
      Ord(tdLeftToRight),
      Ord(ProjectInstance.GetTextDirection(tbInterlinear)));
  finally
    ProjectInstance.Free;
  end;
end;

procedure TProjectPersistenceTest.CommentBreaksMapToAndFromWysiwygSession;
var
  Source: string;
begin
  Source := 'first' + #13#10 + 'second<lf/>third<crlf/>fourth' +
    '<br/>fifth<br>sixth';
  AssertEquals(
    'first<p>second<p>third<p>fourth<CL>fifth<CL>sixth',
    CommentsTextForWysiwygEditor(Source));
  AssertEquals('first<p>second<CL>third<p>fourth',
    CommentsTextFromWysiwygEditor(
      'first<p>second<CL>third' + #10 + 'fourth'));
end;

procedure TProjectPersistenceTest.CommentWysiwygSaveCanonicalizesLegacyBreaks;
var
  Initial, Original: string;
begin
  Original := 'one<crlf/>two<br>three' + #10 + '<b>four</b>';
  Initial := CommentsTextForWysiwygEditor(Original);
  AssertEquals('saving migrates mixed legacy breaks to canonical body tags',
    'one<p>two<CL>three<p><b>four</b>',
    CommentsTextFromWysiwygEditor(Initial));
  AssertEquals('a real edit also keeps canonical body tags',
    'one<p>changed<CL>three<p><b>four</b>',
    CommentsTextFromWysiwygEditor(
      'one<p>changed<CL>three<p><b>four</b>'));
end;

procedure TProjectPersistenceTest.
  CommentPreviewProjectsBreaksAndPreservesOpaqueTags;
begin
  AssertEquals('one<br/>two<br/>three<br/>four<br/>five<CM>opaque<CI>',
    CommentsTextForPreview(
      'one<lf/>two<p>three<CL>four<br>five<CM>opaque<CI>'));
end;

procedure TProjectPersistenceTest.CommentPreviewSkipsNoOpAssignments;
var
  Generation: QWord;
  View: TCommentsView;
begin
  View := TCommentsView.Create(nil);
  try
    View.Text := 'same<p>comment';
    Generation := View.RenderGenerationFixture;
    View.Text := 'same<p>comment';
    View.Text := 'same<lf/>comment';
    AssertEquals('equivalent legacy breaks reuse the rendered preview',
      'same<lf/>comment', View.Text);
    View.FontName := View.FontName;
    View.FontName := UpperCase(View.FontName);
    View.FontSize := View.FontSize;
    AssertTrue('effectively identical text and style do not rerender',
      View.RenderGenerationFixture = Generation);

    View.FontSize := View.FontSize + 1;
    AssertTrue('an effective font change still rerenders',
      View.RenderGenerationFixture = Generation + 1);
  finally
    View.Free;
  end;
end;

procedure TProjectPersistenceTest.UnknownEditorialMarkupUsesRawOnlyMode;
begin
  AssertEquals(Ord(twlcRawOnly), Ord(AnalyzeTheWordTranslatorNoteBody(
    UTF8Decode(CommentsTextForWysiwygEditor(
      'known<lf/>before<CM>opaque<CI>after')))));
end;

procedure TProjectPersistenceTest.
  AutomaticRFCountUsesPendingRowsAndRestoresDatasetState;
var
  Counts: TAutomaticRFNoteCountArray;
  DestinationTextIndex: Integer;
  Repository: TProjectVerseRepository;
  Snapshot: TChapterSnapshot;
  SnapshotEntry: TChapterSnapshotEntry;
  SnapshotRevision: QWord;
  SourceTextIndex: Integer;
begin
  Repository := TProjectVerseRepository.Create(FPersistence);
  try
    AssertTrue(FPersistence.LocateReference('1,1,1'));
    DestinationTextIndex := FPersistence.PairsTable.FieldByName(
      'pare_texto_destino').Index;
    SourceTextIndex := FPersistence.PairsTable.FieldByName(
      'pare_texto_origem').Index;
    FPersistence.SetCurrentText(SourceTextIndex,
      'source<RF>source one<Rf><RF>source two<Rf>');
    FPersistence.SetCurrentText(DestinationTextIndex,
      'pending<RF>one<Rf><RF q=x>custom<Rf>');
    FPersistence.PostCurrentEdit;
    AssertTrue('edited first verse remains a cached update',
      FPersistence.ContentUpdatesPending);
    AssertEquals('cached update is not yet durable', '[target]',
      ReadDatabasePairField('pare_texto_destino'));

    AssertTrue(FPersistence.LocateReference('1,1,3'));
    FPersistence.PairsTable.BeforeScroll := @PairBeforeScroll;
    FPersistence.PairsTable.AfterScroll := @PairAfterScroll;

    AssertFalse(FPersistence.PairsTable.ControlsDisabled);
    Snapshot := Repository.LoadChapterSnapshot(1, 1,
      [DestinationTextIndex, SourceTextIndex]);
    try
      AssertEquals('snapshot contains only the requested chapter', 3,
        Snapshot.Count);
      AssertEquals('snapshot load does not move the active Verse Pair',
        '1,1,3', FPersistence.CurrentPairId);
      SnapshotEntry := Snapshot.Entry(0);
      AssertEquals('snapshot overlays pending destination text',
        'pending<RF>one<Rf><RF q=x>custom<Rf>',
        SnapshotEntry.Texts[0]);
      AssertEquals('snapshot overlays pending source text',
        'source<RF>source one<Rf><RF>source two<Rf>',
        SnapshotEntry.Texts[1]);
      AssertEquals('first verse starts automatic numbering at zero', 0,
        SnapshotEntry.AutomaticNoteStarts[0]);
      SnapshotEntry := Snapshot.Entry(2);
      AssertEquals('prefix count is stored on the target verse', 2,
        SnapshotEntry.AutomaticNoteStarts[0]);
      AssertEquals('snapshot lookups use the requested field index', 2,
        Snapshot.AutomaticNoteStart('1,1,3', DestinationTextIndex));
      AssertEquals('snapshot query never invokes BeforeScroll', 0,
        FPairBeforeScrollCount);
      AssertEquals('snapshot query never invokes AfterScroll', 0,
        FPairAfterScrollCount);

      SnapshotRevision := Snapshot.ContentRevision;
      AssertFalse('unknown fields reject the patch atomically',
        Snapshot.UpdateEntry('1,1,1', [High(Integer)], ['invalid'],
          'invalid-pairs', SnapshotRevision + 1));
      AssertEquals('a rejected patch keeps the snapshot revision',
        Int64(SnapshotRevision), Int64(Snapshot.ContentRevision));
      AssertTrue('the current Verse can update the existing snapshot',
        Snapshot.UpdateEntry('1,1,1',
          [DestinationTextIndex, SourceTextIndex],
          ['updated<RF>one<Rf><RF>two<Rf><RF>three<Rf>',
           'updated source'], 'updated-pairs', SnapshotRevision + 1));
      AssertTrue('the patched revision remains a cache match',
        Snapshot.Matches(1, 1, SnapshotRevision + 1,
          [DestinationTextIndex, SourceTextIndex]));
      SnapshotEntry := Snapshot.Entry(0);
      AssertEquals('incremental patch replaces destination text',
        'updated<RF>one<Rf><RF>two<Rf><RF>three<Rf>',
        SnapshotEntry.Texts[0]);
      AssertEquals('incremental patch replaces associations',
        'updated-pairs', SnapshotEntry.Associations);
      SnapshotEntry := Snapshot.Entry(2);
      AssertEquals('RF prefixes are rebuilt after an earlier Verse changes',
        4, SnapshotEntry.AutomaticNoteStarts[0]);
    finally
      Snapshot.Free;
    end;

    AssertEquals('only automatic RFs in previous verses are counted', 2,
      Repository.CountAutomaticRFNotesBefore('1,1,3',
        DestinationTextIndex));
    AssertEquals('current Verse Pair is restored', '1,1,3',
      FPersistence.CurrentPairId);
    Counts := Repository.CountAutomaticRFNotesBeforeMany(
      '1,1,3', [DestinationTextIndex, SourceTextIndex]);
    AssertEquals('bulk result follows requested field order', 2,
      Length(Counts));
    AssertEquals('destination count is calculated in the shared scan',
      2, Counts[0]);
    AssertEquals('source count is independent in the shared scan',
      2, Counts[1]);
    AssertEquals('bulk scan also restores the current Verse Pair',
      '1,1,3', FPersistence.CurrentPairId);
    AssertEquals('internal scan suppresses BeforeScroll', 0,
      FPairBeforeScrollCount);
    AssertEquals('internal scan suppresses AfterScroll', 0,
      FPairAfterScrollCount);
    AssertFalse('controls are re-enabled after the scan',
      FPersistence.PairsTable.ControlsDisabled);

    FPersistence.PairsTable.DisableControls;
    try
      AssertEquals(2, Repository.CountAutomaticRFNotesBefore('1,1,3',
        DestinationTextIndex));
      AssertTrue('a pre-existing disabled state is preserved',
        FPersistence.PairsTable.ControlsDisabled);
    finally
      FPersistence.PairsTable.EnableControls;
    end;

    AssertTrue(FPersistence.LocateReference('1,1,2'));
    AssertTrue('BeforeScroll callback is restored',
      FPairBeforeScrollCount > 0);
    AssertTrue('AfterScroll callback is restored',
      FPairAfterScrollCount > 0);
  finally
    Repository.Free;
  end;
end;

initialization
  RegisterTest(TProjectPersistenceTest);

end.
