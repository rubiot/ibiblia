unit ai_credentials_test;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, fpcunit, testregistry, ai_credential_store,
  ai_credentials, project_persistence;

type
  TAICredentialsTests = class(TTestCase)
  private
    FFileName: string;
    FPersistence: TProjectPersistence;
    FCredentials: TAIProjectCredentials;
    FStore: IAICredentialStore;
    FStoreObject: TObject;
    FSecrets: array[TAICredentialScope] of string;
    FAliasSecret: string;
    procedure CreateDatabase;
    procedure SeedInfo(const Key, Value: string);
    function ReadDatabaseInfo(const Key: string): string;
    procedure ReopenCredentials;
    procedure AssertLegacyPreserved;
    procedure AssertSecretAbsent(const Secret: string);
    procedure Migrate;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ReadAndStageDoNotWriteDatabaseOrVault;
    procedure SuccessfulMigrationAfterSaveRemovesAllLegacyBytes;
    procedure LegacyCommonAliasStillResolvesAndMigrates;
    procedure StoreWriteFailureKeepsLegacyAndSanitizesError;
    procedure StoreVerificationFailureKeepsLegacy;
    procedure ReferenceCommitFailurePreservesLegacyAndRetriesPreparedReferences;
    procedure CleanupFailureKeepsReferencesAndResumesAfterReopen;
    procedure MissingReferenceDoesNotFallBackToLegacy;
    procedure MissingScopedReferenceDoesNotUseCommonCredential;
    procedure CopiedProjectKeyChangeUsesANewReference;
    procedure SessionOnlyStoreKeepsLegacyCredentials;
    procedure ClearingScopeKeepsOtherReferencesAndCopiedCredentials;
    procedure MigrationRefusesUnsavedContentWithoutDiscardingIt;
    procedure GenericInfoWritesRejectEverySecretKey;
    procedure AttachingDiscardsUnpersistedSessionChanges;
    procedure WALReaderKeepsCleanupPendingUntilItsSnapshotCloses;
    procedure ConcurrentWriterKeepsLegacyAndStagedCredentialUntilRetry;
    {$IFDEF MSWINDOWS}
    procedure WindowsStoreRoundTripUsesUniqueSyntheticCredential;
    {$ENDIF}
  end;

implementation

uses sqlite3conn, sqldb, sqlite3dyn;

type
  TFaultStore = class(TMemoryAICredentialStore)
  public
    FailWrite, FailVerify: Boolean;
    WriteCalls: Integer;
    WrittenReferences: TStringList;
    constructor Create;
    destructor Destroy; override;
    function TryRead(const Reference: string; out Secret: string): Boolean; override;
    procedure Write(const Reference, Secret: string); override;
  end;

  TFaultPersistence = class(TProjectPersistence)
  public
    FailCommit, FailCleanup: Boolean;
    FailureDetail: string;
    procedure CommitCredentialReferences(const References: TStrings;
      RemoveLegacy: Boolean); override;
    procedure FinishCredentialCleanup; override;
  end;

function FileBytes(const FileName: string): RawByteString;
var Stream: TFileStream;
begin
  Result := '';
  if not FileExists(FileName) then Exit;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Length(Result) > 0 then Stream.ReadBuffer(Result[1], Length(Result));
  finally Stream.Free; end;
end;

procedure DeleteFixture(const FileName: string);
begin
  if FileName = '' then Exit;
  DeleteFile(FileName + '-wal');
  DeleteFile(FileName + '-shm');
  DeleteFile(FileName + '-journal');
  DeleteFile(FileName);
end;

constructor TFaultStore.Create;
begin
  inherited Create(True);
  WrittenReferences := TStringList.Create;
end;

destructor TFaultStore.Destroy;
begin
  WrittenReferences.Free;
  inherited Destroy;
end;

function TFaultStore.TryRead(const Reference: string; out Secret: string): Boolean;
begin
  Result := inherited TryRead(Reference, Secret);
  if FailVerify and Result then Secret := 'synthetic-invalid-readback';
end;

procedure TFaultStore.Write(const Reference, Secret: string);
begin
  Inc(WriteCalls);
  if FailWrite then raise Exception.Create('Synthetic store failure: ' + Secret);
  if WrittenReferences.IndexOf(Reference) < 0 then WrittenReferences.Add(Reference);
  inherited Write(Reference, Secret);
end;

procedure TFaultPersistence.CommitCredentialReferences(const References: TStrings;
  RemoveLegacy: Boolean);
begin
  if FailCommit then raise Exception.Create('Synthetic SQL failure: ' + FailureDetail);
  inherited CommitCredentialReferences(References, RemoveLegacy);
end;

procedure TFaultPersistence.FinishCredentialCleanup;
begin
  if FailCleanup then raise Exception.Create('Synthetic cleanup failure: ' + FailureDetail);
  inherited FinishCredentialCleanup;
end;

procedure TAICredentialsTests.CreateDatabase;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  try
    Connection.DatabaseName := FFileName;
    Connection.Transaction := Transaction;
    Connection.Open;
    Connection.ExecuteDirect('create table info (id text primary key, valor text)');
    Connection.ExecuteDirect('create table pares (' +
      'pare_id text primary key, pare_ref text, pare_texto_origem text, ' +
      'pare_texto_destino text, pare_texto_consulta1 text, ' +
      'pare_texto_consulta2 text, pare_pares text, pare_situacao number, pare_comentarios text)');
    Connection.ExecuteDirect('insert into pares values (' +
      '''1,1,1'', ''Gen 1:1'', ''source'', ''destination'', '''', '''', '''', 0, ''comment'')');
    Transaction.Commit;
  finally Connection.Free; end;
end;

procedure TAICredentialsTests.SeedInfo(const Key, Value: string);
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
begin
  if Assigned(FPersistence) then FPersistence.Close;
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  Query := TSQLQuery.Create(Connection);
  try
    Connection.DatabaseName := FFileName;
    Connection.Transaction := Transaction;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Connection.Open;
    Query.SQL.Text := 'insert or replace into info (id, valor) values (:id, :value)';
    Query.ParamByName('id').AsString := Key;
    Query.ParamByName('value').AsString := Value;
    Query.ExecSQL;
    Transaction.Commit;
  finally
    Connection.Free;
    if Assigned(FPersistence) then FPersistence.Open(FFileName);
  end;
end;

function TAICredentialsTests.ReadDatabaseInfo(const Key: string): string;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
begin
  Result := '';
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(Connection);
  Query := TSQLQuery.Create(Connection);
  try
    Connection.DatabaseName := FFileName;
    Connection.Transaction := Transaction;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Connection.Open;
    Query.SQL.Text := 'select valor from info where id = :id';
    Query.ParamByName('id').AsString := Key;
    Query.Open;
    if not Query.EOF then Result := Query.Fields[0].AsString;
    Query.Close;
    Transaction.Rollback;
  finally Connection.Free; end;
end;

procedure TAICredentialsTests.SetUp;
var Scope: TAICredentialScope;
begin
  FFileName := GetTempFileName(GetTempDir, 'aic');
  CreateDatabase;
  FPersistence := nil;
  for Scope in TAICredentialScope do
  begin
    FSecrets[Scope] := 'synthetic-ibiblia-secret-' + IntToStr(Ord(Scope)) + '-' +
      NewAICredentialReference;
    SeedInfo(AICredentialKey(Scope), FSecrets[Scope]);
  end;
  FAliasSecret := 'synthetic-legacy-alias-' + NewAICredentialReference;
  SeedInfo(AI_LEGACY_COMMON_CREDENTIAL_KEY, FAliasSecret);
  FPersistence := TFaultPersistence.Create;
  FPersistence.Open(FFileName);
  FStoreObject := TFaultStore.Create;
  FStore := TFaultStore(FStoreObject);
  FCredentials := TAIProjectCredentials.Create(FPersistence, FStore);
end;

procedure TAICredentialsTests.TearDown;
begin
  FreeAndNil(FCredentials);
  FreeAndNil(FPersistence);
  FStore := nil;
  FStoreObject := nil;
  DeleteFixture(FFileName);
end;

procedure TAICredentialsTests.ReopenCredentials;
begin
  FreeAndNil(FCredentials);
  FPersistence.Close;
  FPersistence.Open(FFileName);
  FCredentials := TAIProjectCredentials.Create(FPersistence, FStore);
end;

procedure TAICredentialsTests.AssertLegacyPreserved;
var Scope: TAICredentialScope;
begin
  for Scope in TAICredentialScope do
  begin
    AssertEquals(FSecrets[Scope], ReadDatabaseInfo(AICredentialKey(Scope)));
    AssertEquals('', ReadDatabaseInfo(AICredentialReferenceKey(Scope)));
  end;
  AssertEquals(FAliasSecret, ReadDatabaseInfo(AI_LEGACY_COMMON_CREDENTIAL_KEY));
end;

procedure TAICredentialsTests.AssertSecretAbsent(const Secret: string);
begin
  AssertEquals('No secret in SQLite', 0, Pos(Secret, FileBytes(FFileName)));
  AssertEquals('No secret in WAL', 0, Pos(Secret, FileBytes(FFileName + '-wal')));
  AssertEquals('No secret in shared memory', 0, Pos(Secret, FileBytes(FFileName + '-shm')));
  AssertEquals('No secret in journal', 0, Pos(Secret, FileBytes(FFileName + '-journal')));
end;

procedure TAICredentialsTests.Migrate;
var ErrorMessage: string; Success: Boolean;
begin
  FPersistence.Commit;
  Success := FCredentials.PersistAfterSave(ErrorMessage);
  AssertTrue('Migration must complete: ' + ErrorMessage, Success);
end;

procedure TAICredentialsTests.ReadAndStageDoNotWriteDatabaseOrVault;
var OriginalBytes: RawByteString; NewSecret: string;
begin
  OriginalBytes := FileBytes(FFileName);
  AssertEquals(FSecrets[acsCommon], FCredentials.Resolve(acsCommon));
  AssertTrue(FCredentials.MigrationPending);
  AssertEquals(0, TFaultStore(FStoreObject).WriteCalls);
  AssertEquals(OriginalBytes, FileBytes(FFileName));
  NewSecret := 'synthetic-staged-' + NewAICredentialReference;
  FCredentials.Stage(acsCommon, NewSecret);
  AssertTrue(FCredentials.HasPendingChanges);
  AssertEquals(NewSecret, FCredentials.Resolve(acsCommon));
  AssertEquals(0, TFaultStore(FStoreObject).WriteCalls);
  AssertEquals(OriginalBytes, FileBytes(FFileName));
  AssertLegacyPreserved;
  AssertSecretAbsent(NewSecret);
end;

procedure TAICredentialsTests.SuccessfulMigrationAfterSaveRemovesAllLegacyBytes;
var Scope: TAICredentialScope; Reference, Secret, PositionBefore: string;
begin
  FPersistence.StageInfo('title', 'saved project title');
  PositionBefore := FPersistence.CurrentPairId;
  Migrate;
  AssertEquals('saved project title', ReadDatabaseInfo('title'));
  AssertEquals(PositionBefore, FPersistence.CurrentPairId);
  AssertFalse(FCredentials.HasPendingChanges);
  AssertFalse(FCredentials.MigrationPending);
  AssertEquals('', ReadDatabaseInfo(AI_CREDENTIAL_CLEANUP_PENDING_KEY));
  for Scope in TAICredentialScope do
  begin
    AssertEquals('', ReadDatabaseInfo(AICredentialKey(Scope)));
    Reference := ReadDatabaseInfo(AICredentialReferenceKey(Scope));
    AssertEquals(36, Length(Reference));
    AssertTrue(FStore.TryRead(Reference, Secret));
    AssertEquals(FSecrets[Scope], Secret);
    AssertEquals(FSecrets[Scope], FCredentials.Resolve(Scope));
    AssertSecretAbsent(FSecrets[Scope]);
  end;
  AssertEquals('', ReadDatabaseInfo(AI_LEGACY_COMMON_CREDENTIAL_KEY));
  AssertSecretAbsent(FAliasSecret);
  ReopenCredentials;
  AssertEquals(FSecrets[acsCommon], FCredentials.Resolve(acsCommon));
end;

procedure TAICredentialsTests.LegacyCommonAliasStillResolvesAndMigrates;
var Reference, Secret: string;
begin
  SeedInfo(AICredentialKey(acsCommon), '');
  AssertEquals(FAliasSecret, FCredentials.Resolve(acsCommon));
  Migrate;
  Reference := ReadDatabaseInfo(AICredentialReferenceKey(acsCommon));
  AssertTrue(FStore.TryRead(Reference, Secret));
  AssertEquals(FAliasSecret, Secret);
  AssertSecretAbsent(FAliasSecret);
end;

procedure TAICredentialsTests.StoreWriteFailureKeepsLegacyAndSanitizesError;
var ErrorMessage: string;
begin
  TFaultStore(FStoreObject).FailWrite := True;
  AssertFalse(FCredentials.PersistAfterSave(ErrorMessage));
  AssertTrue(ErrorMessage <> '');
  AssertEquals(0, Pos(FSecrets[acsCommon], ErrorMessage));
  AssertLegacyPreserved;
  AssertTrue(FCredentials.MigrationPending);
  TFaultStore(FStoreObject).FailWrite := False;
  Migrate;
end;

procedure TAICredentialsTests.StoreVerificationFailureKeepsLegacy;
var ErrorMessage: string;
begin
  TFaultStore(FStoreObject).FailVerify := True;
  AssertFalse(FCredentials.PersistAfterSave(ErrorMessage));
  AssertTrue(ErrorMessage <> '');
  AssertLegacyPreserved;
  AssertTrue(FCredentials.MigrationPending);
  TFaultStore(FStoreObject).FailVerify := False;
  Migrate;
end;

procedure TAICredentialsTests.ReferenceCommitFailurePreservesLegacyAndRetriesPreparedReferences;
var ErrorMessage, NewSecret, Prepared: string;
begin
  NewSecret := 'synthetic-replacement-' + NewAICredentialReference;
  FCredentials.Stage(acsCommon, NewSecret);
  TFaultPersistence(FPersistence).FailCommit := True;
  TFaultPersistence(FPersistence).FailureDetail := NewSecret;
  AssertFalse(FCredentials.PersistAfterSave(ErrorMessage));
  AssertEquals(0, Pos(NewSecret, ErrorMessage));
  AssertLegacyPreserved;
  AssertTrue(FCredentials.HasPendingChanges);
  AssertEquals(NewSecret, FCredentials.Resolve(acsCommon));
  AssertEquals(3, TFaultStore(FStoreObject).WrittenReferences.Count);
  Prepared := TFaultStore(FStoreObject).WrittenReferences[0];
  TFaultPersistence(FPersistence).FailCommit := False;
  Migrate;
  AssertEquals(Prepared, ReadDatabaseInfo(AICredentialReferenceKey(acsCommon)));
  AssertEquals(3, TFaultStore(FStoreObject).WrittenReferences.Count);
  AssertSecretAbsent(NewSecret);
end;

procedure TAICredentialsTests.CleanupFailureKeepsReferencesAndResumesAfterReopen;
var ErrorMessage, Reference: string; WritesBefore: Integer;
begin
  TFaultPersistence(FPersistence).FailCleanup := True;
  TFaultPersistence(FPersistence).FailureDetail := FSecrets[acsCommon];
  AssertFalse(FCredentials.PersistAfterSave(ErrorMessage));
  AssertEquals(0, Pos(FSecrets[acsCommon], ErrorMessage));
  AssertEquals('', ReadDatabaseInfo(AICredentialKey(acsCommon)));
  AssertEquals('1', ReadDatabaseInfo(AI_CREDENTIAL_CLEANUP_PENDING_KEY));
  Reference := ReadDatabaseInfo(AICredentialReferenceKey(acsCommon));
  AssertEquals(36, Length(Reference));
  AssertFalse(FCredentials.HasPendingChanges);
  AssertTrue(FCredentials.MigrationPending);
  AssertEquals(FSecrets[acsCommon], FCredentials.Resolve(acsCommon));
  WritesBefore := TFaultStore(FStoreObject).WriteCalls;
  ReopenCredentials;
  TFaultPersistence(FPersistence).FailCleanup := False;
  Migrate;
  AssertEquals(Reference, ReadDatabaseInfo(AICredentialReferenceKey(acsCommon)));
  AssertEquals(WritesBefore, TFaultStore(FStoreObject).WriteCalls);
  AssertEquals('', ReadDatabaseInfo(AI_CREDENTIAL_CLEANUP_PENDING_KEY));
  AssertFalse(FCredentials.MigrationPending);
  AssertSecretAbsent(FSecrets[acsCommon]);
end;

procedure TAICredentialsTests.MissingReferenceDoesNotFallBackToLegacy;
var ErrorMessage, Secret, Reference: string;
begin
  Reference := NewAICredentialReference;
  SeedInfo(AICredentialReferenceKey(acsCommon), Reference);
  AssertTrue(FCredentials.HasConfiguredCredential(acsCommon));
  AssertFalse(FCredentials.TryResolve(acsCommon, Secret, ErrorMessage));
  AssertEquals('', Secret);
  AssertTrue(ErrorMessage <> '');
  AssertFalse(FCredentials.PersistAfterSave(ErrorMessage));
  AssertEquals(0, TFaultStore(FStoreObject).WriteCalls);
  AssertEquals(Reference, ReadDatabaseInfo(AICredentialReferenceKey(acsCommon)));
  AssertEquals(FSecrets[acsCommon], ReadDatabaseInfo(AICredentialKey(acsCommon)));
end;

procedure TAICredentialsTests.MissingScopedReferenceDoesNotUseCommonCredential;
var ErrorMessage, Secret, Reference: string;
begin
  Migrate;
  Reference := ReadDatabaseInfo(AICredentialReferenceKey(acsAssociation));
  FStore.Delete(Reference);
  AssertEquals(FSecrets[acsCommon], FCredentials.Resolve(acsCommon));
  AssertFalse(FCredentials.TryResolve(acsAssociation, Secret, ErrorMessage));
  AssertEquals('', Secret);
  AssertTrue(ErrorMessage <> '');
end;

procedure TAICredentialsTests.CopiedProjectKeyChangeUsesANewReference;
var
  CopyFileName, OriginalReference, NewReference, NewSecret, Secret, ErrorMessage: string;
  SourceStream, TargetStream: TFileStream;
  CopyPersistence: TProjectPersistence;
  CopyCredentials: TAIProjectCredentials;
begin
  Migrate;
  OriginalReference := ReadDatabaseInfo(AICredentialReferenceKey(acsCommon));
  FPersistence.Close;
  CopyFileName := GetTempFileName(GetTempDir, 'aic');
  CopyPersistence := nil;
  CopyCredentials := nil;
  try
    SourceStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      TargetStream := TFileStream.Create(CopyFileName, fmCreate);
      try TargetStream.CopyFrom(SourceStream, 0); finally TargetStream.Free; end;
    finally SourceStream.Free; end;
    FPersistence.Open(FFileName);
    CopyPersistence := TProjectPersistence.Create;
    CopyPersistence.Open(CopyFileName);
    CopyCredentials := TAIProjectCredentials.Create(CopyPersistence, FStore);
    AssertEquals(FSecrets[acsCommon], CopyCredentials.Resolve(acsCommon));
    NewSecret := 'synthetic-copy-key-' + NewAICredentialReference;
    CopyCredentials.Stage(acsCommon, NewSecret);
    CopyPersistence.Commit;
    AssertTrue(CopyCredentials.PersistAfterSave(ErrorMessage));
    NewReference := CopyPersistence.ReadInfo(AICredentialReferenceKey(acsCommon));
    AssertTrue(NewReference <> OriginalReference);
    AssertEquals(OriginalReference, ReadDatabaseInfo(AICredentialReferenceKey(acsCommon)));
    AssertTrue(FStore.TryRead(OriginalReference, Secret));
    AssertEquals(FSecrets[acsCommon], Secret);
    AssertEquals(FSecrets[acsCommon], FCredentials.Resolve(acsCommon));
    AssertEquals(NewSecret, CopyCredentials.Resolve(acsCommon));
    AssertEquals(0, Pos(NewSecret, FileBytes(CopyFileName)));
  finally
    CopyCredentials.Free;
    CopyPersistence.Free;
    DeleteFixture(CopyFileName);
  end;
end;

procedure TAICredentialsTests.SessionOnlyStoreKeepsLegacyCredentials;
var
  SessionStore: IAICredentialStore;
  SessionCredentials: TAIProjectCredentials;
  ErrorMessage, NewSecret: string;
begin
  SessionStore := TMemoryAICredentialStore.Create(False);
  SessionCredentials := TAIProjectCredentials.Create(FPersistence, SessionStore);
  try
    NewSecret := 'synthetic-session-' + NewAICredentialReference;
    SessionCredentials.Stage(acsCommon, NewSecret);
    AssertEquals(NewSecret, SessionCredentials.Resolve(acsCommon));
    AssertFalse(SessionCredentials.PersistAfterSave(ErrorMessage));
    AssertTrue(ErrorMessage <> '');
    AssertTrue(SessionCredentials.HasPendingChanges);
    AssertLegacyPreserved;
    AssertSecretAbsent(NewSecret);
  finally SessionCredentials.Free; end;
end;

procedure TAICredentialsTests.ClearingScopeKeepsOtherReferencesAndCopiedCredentials;
var OldReference, OtherReference, Secret: string;
begin
  Migrate;
  OldReference := ReadDatabaseInfo(AICredentialReferenceKey(acsAssociation));
  OtherReference := ReadDatabaseInfo(AICredentialReferenceKey(acsCommon));
  FCredentials.Stage(acsAssociation, '');
  AssertFalse(FCredentials.HasConfiguredCredential(acsAssociation));
  AssertEquals('', FCredentials.Resolve(acsAssociation));
  Migrate;
  AssertEquals('', ReadDatabaseInfo(AICredentialReferenceKey(acsAssociation)));
  AssertEquals(OtherReference, ReadDatabaseInfo(AICredentialReferenceKey(acsCommon)));
  AssertTrue('Another project may refer to the old key', FStore.TryRead(OldReference, Secret));
  AssertEquals(FSecrets[acsAssociation], Secret);
  ReopenCredentials;
  AssertEquals('', FCredentials.Resolve(acsAssociation));
  AssertEquals(FSecrets[acsCommon], FCredentials.Resolve(acsCommon));
end;

procedure TAICredentialsTests.MigrationRefusesUnsavedContentWithoutDiscardingIt;
var ErrorMessage: string;
begin
  FPersistence.StageInfo('title', 'pending title');
  AssertFalse(FCredentials.PersistAfterSave(ErrorMessage));
  AssertTrue(FPersistence.ContentUpdatesPending);
  AssertEquals('pending title', FPersistence.ReadInfo('title'));
  AssertEquals('', ReadDatabaseInfo('title'));
  AssertLegacyPreserved;
  Migrate;
  AssertEquals('pending title', ReadDatabaseInfo('title'));
end;

procedure TAICredentialsTests.GenericInfoWritesRejectEverySecretKey;
var Keys: TStringList; Scope: TAICredentialScope; I, Operation: Integer; Refused: Boolean;
begin
  Keys := TStringList.Create;
  try
    Keys.Add(AI_LEGACY_COMMON_CREDENTIAL_KEY);
    for Scope in TAICredentialScope do Keys.Add(AICredentialKey(Scope));
    for I := 0 to Keys.Count - 1 do
      for Operation := 0 to 3 do
      begin
        Refused := False;
        try
          case Operation of
            0: FPersistence.StageInfo(Keys[I], 'synthetic-forbidden');
            1: FPersistence.StageInfoSilently(Keys[I], 'synthetic-forbidden');
            2: FPersistence.WriteInfoImmediate(Keys[I], 'synthetic-forbidden');
            3: FPersistence.UpdateInfo(Keys[I], 'synthetic-forbidden');
          end;
        except on E: EAICredentialError do Refused := True; end;
        AssertTrue('Secret key must be rejected by operation ' + IntToStr(Operation), Refused);
        AssertFalse(FPersistence.ContentUpdatesPending);
      end;
    AssertLegacyPreserved;
    AssertSecretAbsent('synthetic-forbidden');
  finally Keys.Free; end;
end;

procedure TAICredentialsTests.AttachingDiscardsUnpersistedSessionChanges;
begin
  FCredentials.Stage(acsTranslation, 'synthetic-discarded-stage');
  AssertTrue(FCredentials.HasPendingChanges);
  FCredentials.Attach(FPersistence);
  AssertFalse(FCredentials.HasPendingChanges);
  AssertEquals(FSecrets[acsTranslation], FCredentials.Resolve(acsTranslation));
  AssertEquals(0, TFaultStore(FStoreObject).WriteCalls);
  AssertLegacyPreserved;
end;

procedure TAICredentialsTests.WALReaderKeepsCleanupPendingUntilItsSnapshotCloses;
var
  Reader: psqlite3;
  Statement: psqlite3_stmt;
  ErrorMessage, Reference, SQL: string;
  Scope: TAICredentialScope;
  WritesBefore: Integer;
begin
  Reader := nil;
  Statement := nil;
  InitializeSQLite;
  try
    FPersistence.Close;
    AssertEquals(SQLITE_OK, sqlite3_open(PChar(FFileName), @Reader));
    AssertEquals(SQLITE_OK, sqlite3_exec(Reader,
      'pragma journal_mode = WAL', nil, nil, nil));
    // A committed page containing legacy keys must actually exist in the WAL.
    AssertEquals(SQLITE_OK, sqlite3_exec(Reader,
      'insert into info(id, valor) values (''fixture-wal-generation'', ''before-reader'')',
      nil, nil, nil));
    AssertTrue('Fixture must contain a legacy secret in WAL before migration',
      Pos(FSecrets[acsCommon], FileBytes(FFileName + '-wal')) > 0);
    AssertEquals(SQLITE_OK, sqlite3_exec(Reader, 'begin', nil, nil, nil));
    SQL := 'select valor from info where id = ''ai.common.api_key''';
    AssertEquals(SQLITE_OK, sqlite3_prepare_v2(Reader, PChar(SQL), -1,
      @Statement, nil));
    AssertEquals(SQLITE_ROW, sqlite3_step(Statement));
    AssertEquals(FSecrets[acsCommon], string(PChar(sqlite3_column_text(Statement, 0))));

    FPersistence.Open(FFileName);
    FPersistence.Commit;
    AssertFalse('An old reader must prevent reporting successful WAL cleanup',
      FCredentials.PersistAfterSave(ErrorMessage));
    AssertTrue(ErrorMessage <> '');
    Reference := ReadDatabaseInfo(AICredentialReferenceKey(acsCommon));
    AssertEquals('Reference transaction may commit before checkpoint cleanup',
      36, Length(Reference));
    AssertEquals('', ReadDatabaseInfo(AICredentialKey(acsCommon)));
    AssertEquals('1', ReadDatabaseInfo(AI_CREDENTIAL_CLEANUP_PENDING_KEY));
    AssertEquals(FSecrets[acsCommon], FCredentials.Resolve(acsCommon));
    AssertTrue(FCredentials.MigrationPending);
    AssertTrue('The protected old WAL page still requires cleanup',
      Pos(FSecrets[acsCommon], FileBytes(FFileName + '-wal')) > 0);
    WritesBefore := TFaultStore(FStoreObject).WriteCalls;

    AssertEquals(SQLITE_OK, sqlite3_finalize(Statement));
    Statement := nil;
    AssertEquals(SQLITE_OK, sqlite3_exec(Reader, 'commit', nil, nil, nil));
    AssertEquals(SQLITE_OK, sqlite3_close(Reader));
    Reader := nil;
    Migrate;
    AssertFalse(FCredentials.MigrationPending);
    AssertEquals('', ReadDatabaseInfo(AI_CREDENTIAL_CLEANUP_PENDING_KEY));
    AssertEquals(Reference, ReadDatabaseInfo(AICredentialReferenceKey(acsCommon)));
    AssertEquals('Retry must only finish cleanup, not mint new keys',
      WritesBefore, TFaultStore(FStoreObject).WriteCalls);
    for Scope in TAICredentialScope do AssertSecretAbsent(FSecrets[Scope]);
    AssertSecretAbsent(FAliasSecret);
  finally
    if Statement <> nil then sqlite3_finalize(Statement);
    if Reader <> nil then
    begin
      sqlite3_exec(Reader, 'rollback', nil, nil, nil);
      sqlite3_close(Reader);
    end;
    ReleaseSQLite;
  end;
end;

procedure TAICredentialsTests.ConcurrentWriterKeepsLegacyAndStagedCredentialUntilRetry;
var
  Writer: psqlite3;
  ErrorMessage, NewSecret, PreparedReference: string;
begin
  Writer := nil;
  FPersistence.Commit;
  NewSecret := 'synthetic-busy-writer-' + NewAICredentialReference;
  FCredentials.Stage(acsCommon, NewSecret);
  InitializeSQLite;
  try
    AssertEquals(SQLITE_OK, sqlite3_open(PChar(FFileName), @Writer));
    AssertEquals(SQLITE_OK, sqlite3_exec(Writer, 'begin immediate', nil, nil, nil));
    AssertFalse('A real competing SQLite writer must block the reference commit',
      FCredentials.PersistAfterSave(ErrorMessage));
    AssertTrue(ErrorMessage <> '');
    AssertEquals(0, Pos(NewSecret, ErrorMessage));
    AssertLegacyPreserved;
    AssertTrue(FCredentials.HasPendingChanges);
    AssertEquals(NewSecret, FCredentials.Resolve(acsCommon));
    AssertEquals(3, TFaultStore(FStoreObject).WrittenReferences.Count);
    PreparedReference := TFaultStore(FStoreObject).WrittenReferences[0];

    AssertEquals(SQLITE_OK, sqlite3_exec(Writer, 'rollback', nil, nil, nil));
    AssertEquals(SQLITE_OK, sqlite3_close(Writer));
    Writer := nil;
    Migrate;
    AssertFalse(FCredentials.HasPendingChanges);
    AssertFalse(FCredentials.MigrationPending);
    AssertEquals(PreparedReference, ReadDatabaseInfo(AICredentialReferenceKey(acsCommon)));
    AssertEquals(NewSecret, FCredentials.Resolve(acsCommon));
    AssertEquals(3, TFaultStore(FStoreObject).WrittenReferences.Count);
    AssertSecretAbsent(NewSecret);
    AssertSecretAbsent(FSecrets[acsCommon]);
  finally
    if Writer <> nil then
    begin
      sqlite3_exec(Writer, 'rollback', nil, nil, nil);
      sqlite3_close(Writer);
    end;
    ReleaseSQLite;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TAICredentialsTests.WindowsStoreRoundTripUsesUniqueSyntheticCredential;
var Store: IAICredentialStore; Reference, Secret, Loaded: string;
begin
  Store := CreateAICredentialStore;
  AssertTrue(Store.IsPersistent);
  Reference := NewAICredentialReference;
  Secret := 'synthetic-offline-vault-test-' + Reference;
  try
    AssertFalse(Store.TryRead(Reference, Loaded));
    Store.Write(Reference, Secret);
    AssertTrue(Store.TryRead(Reference, Loaded));
    AssertEquals(Secret, Loaded);
    Store.Delete(Reference);
    AssertFalse(Store.TryRead(Reference, Loaded));
    AssertEquals('', Loaded);
  finally Store.Delete(Reference); end;
end;
{$ENDIF}

initialization
  RegisterTest(TAICredentialsTests);

end.
