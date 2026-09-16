unit project_save_snapshot_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, DB, fpcunit, testregistry, sqlite3dyn,
  project_persistence, sqlite_database_copy, project_save_staging;

type
  TProjectSaveSnapshotTests = class(TTestCase)
  private
    FDirectory: string;
    FSourceName: string;
    FPersistence: TProjectPersistence;
    FNotifications: Integer;
    function Path(const Name: string): string;
    function OpenDatabase(const FileName: string): psqlite3;
    procedure Exec(Database: psqlite3; const SQL: string);
    function Scalar(Database: psqlite3; const SQL: string): string;
    function ReadValue(const FileName, SQL: string): string;
    function ReadBytes(const FileName: string): string;
    procedure CreateDatabase(const FileName: string);
    procedure PendingChanged(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CopiesPendingContentAndPreservesSourceSession;
    procedure CopiesDeferredImmediateAndSilentMetadataInReadOrder;
    procedure CopiesCommittedWalContentAndSchema;
    procedure FailedCommitStateRemainsRecoverableAfterSnapshot;
    procedure FailedOverlayRemovesOnlyItsOwnSnapshot;
    procedure ExistingAndSamePhysicalDestinationsArePreserved;
    procedure NewPublicationCreatesCompleteUnicodeProject;
    procedure NewPublicationPreservesFileThatAppearedMeanwhile;
    procedure ExistingPublicationRespectsSQLiteWriterLock;
    procedure ExistingWalReaderRetainsItsSnapshot;
    procedure UnpublishedSnapshotAndSidecarsAreRemoved;
    procedure InvalidStagingPreservesExistingDestination;
  end;

implementation

{$IFDEF WINDOWS}
uses Windows;

function CreateHardLinkW(NewFileName, ExistingFileName: PWideChar;
  SecurityAttributes: Pointer): BOOL; stdcall; external 'kernel32.dll';
{$ELSE}
uses BaseUnix;
{$ENDIF}

function TProjectSaveSnapshotTests.Path(const Name: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FDirectory) + Name;
end;

function TProjectSaveSnapshotTests.OpenDatabase(const FileName: string): psqlite3;
var Code: Integer;
begin
  Result := nil;
  Code := sqlite3_open_v2(PAnsiChar(FileName), @Result,
    SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE or SQLITE_OPEN_PRIVATECACHE, nil);
  if Code <> SQLITE_OK then
  begin
    if Assigned(Result) then sqlite3_close(Result);
    Result := nil;
    Fail(Format('Opening test database failed (SQLite %d)', [Code]));
  end;
end;

procedure TProjectSaveSnapshotTests.Exec(Database: psqlite3; const SQL: string);
var Code: Integer;
begin
  Code := sqlite3_exec(Database, PAnsiChar(SQL), nil, nil, nil);
  AssertEquals(string(sqlite3_errmsg(Database)), SQLITE_OK, Code);
end;

function TProjectSaveSnapshotTests.Scalar(Database: psqlite3;
  const SQL: string): string;
var Statement: psqlite3_stmt; Code: Integer;
begin
  Statement := nil;
  try
    Code := sqlite3_prepare_v2(Database, PAnsiChar(SQL), -1, @Statement, nil);
    AssertEquals(string(sqlite3_errmsg(Database)), SQLITE_OK, Code);
    AssertEquals('Expected one result row', SQLITE_ROW, sqlite3_step(Statement));
    Result := string(sqlite3_column_text(Statement, 0));
  finally
    if Assigned(Statement) then sqlite3_finalize(Statement);
  end;
end;

function TProjectSaveSnapshotTests.ReadValue(const FileName, SQL: string): string;
var Database: psqlite3;
begin
  Database := OpenDatabase(FileName);
  try
    Result := Scalar(Database, SQL);
  finally
    sqlite3_close(Database);
  end;
end;

function TProjectSaveSnapshotTests.ReadBytes(const FileName: string): string;
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Result <> '' then Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

procedure TProjectSaveSnapshotTests.CreateDatabase(const FileName: string);
var Database: psqlite3;
begin
  Database := OpenDatabase(FileName);
  try
    Exec(Database, 'create table info (id text primary key, valor text)');
    Exec(Database, 'create table pares (' +
      'pare_id text primary key, pare_ref text, pare_texto_origem text, ' +
      'pare_texto_destino text, pare_texto_consulta1 text, ' +
      'pare_texto_consulta2 text, pare_pares text, pare_situacao number, ' +
      'pare_comentarios text)');
    Exec(Database, 'insert into info values (''option'', ''original option'')');
    Exec(Database, 'insert into pares values (''1,1,1'', ''Gen 1:1'', ' +
      '''source'', ''target'', ''reference one'', ''reference two'', ' +
      '''old associations'', 0, ''old comments'')');
    Exec(Database, 'insert into pares values (''1,1,2'', ''Gen 1:2'', ' +
      '''source two'', ''target two'', '''', '''', '''', 1, '''')');
    Exec(Database, 'create table custom_data (payload blob)');
    Exec(Database, 'insert into custom_data values (X''000102ff'')');
    Exec(Database, 'create index custom_index on custom_data(payload)');
    Exec(Database, 'pragma user_version=27');
  finally
    sqlite3_close(Database);
  end;
end;

procedure TProjectSaveSnapshotTests.PendingChanged(Sender: TObject);
begin
  Inc(FNotifications);
end;

procedure TProjectSaveSnapshotTests.SetUp;
begin
  inherited SetUp;
  FDirectory := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia_save_as_%x_%x', [PtrUInt(Self), GetTickCount64]);
  ForceDirectories(FDirectory);
  InitializeSQLite('');
  FSourceName := Path('original.ibiblia');
  CreateDatabase(FSourceName);
  FPersistence := TProjectPersistence.Create;
  FPersistence.Open(FSourceName);
  FNotifications := 0;
  FPersistence.OnContentPendingChanged := @PendingChanged;
end;

procedure TProjectSaveSnapshotTests.TearDown;
var Search: TSearchRec;
begin
  FreeAndNil(FPersistence);
  ReleaseSQLite;
  if FindFirst(Path('*'), faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then
        SysUtils.DeleteFile(Path(Search.Name));
    until FindNext(Search) <> 0;
  finally
    SysUtils.FindClose(Search);
  end;
  RemoveDir(FDirectory);
  inherited TearDown;
end;

procedure TProjectSaveSnapshotTests.CopiesPendingContentAndPreservesSourceSession;
var
  Snapshot: TProjectPersistence;
  OriginalBytes: string;
  Revision: QWord;
  Notifications: Integer;
begin
  OriginalBytes := ReadBytes(FSourceName);
  FPersistence.LocateReference('1,1,1');
  FPersistence.SetCurrentFieldAsString('pare_texto_origem', 'Ἀρχή');
  FPersistence.SetCurrentFieldAsString('pare_texto_destino', 'tradução');
  FPersistence.SetCurrentFieldAsString('pare_texto_consulta1', 'consulta um');
  FPersistence.SetCurrentFieldAsString('pare_texto_consulta2', 'consulta dois');
  FPersistence.SetCurrentFieldAsString('pare_pares', 'new associations');
  FPersistence.SetCurrentFieldAsString('pare_comentarios', '');
  FPersistence.SetCurrentFieldAsInteger('pare_situacao', 3);
  FPersistence.PostCurrentEdit;
  FPersistence.LocateReference('1,1,2');
  FPersistence.SetCurrentFieldAsString('pare_texto_destino', '');
  FPersistence.StageInfo('option', 'pending option');
  Revision := FPersistence.VerseContentRevision;
  Notifications := FNotifications;

  FPersistence.WriteSnapshotTo(Path('cópia com acentos.ibiblia'));
  AssertEquals(OriginalBytes, ReadBytes(FSourceName));
  AssertEquals('1,1,2', FPersistence.CurrentPairId);
  AssertEquals('An unposted edit must not be posted by snapshotting',
    Ord(dsEdit), Ord(FPersistence.PairsTable.State));
  AssertEquals(Int64(Revision), Int64(FPersistence.VerseContentRevision));
  AssertEquals(Notifications, FNotifications);
  AssertTrue(FPersistence.ContentUpdatesPending);

  Snapshot := TProjectPersistence.Create;
  try
    Snapshot.Open(Path('cópia com acentos.ibiblia'));
    Snapshot.LocateReference('1,1,1');
    AssertEquals('Ἀρχή', Snapshot.CurrentFieldAsString('pare_texto_origem'));
    AssertEquals('tradução', Snapshot.CurrentFieldAsString('pare_texto_destino'));
    AssertEquals('consulta um', Snapshot.CurrentFieldAsString('pare_texto_consulta1'));
    AssertEquals('consulta dois', Snapshot.CurrentFieldAsString('pare_texto_consulta2'));
    AssertEquals('new associations', Snapshot.CurrentPairs);
    AssertEquals('', Snapshot.CurrentComments);
    AssertEquals(3, Snapshot.CurrentStatus);
    Snapshot.LocateReference('1,1,2');
    AssertEquals('', Snapshot.CurrentFieldAsString('pare_texto_destino'));
    AssertEquals('pending option', Snapshot.ReadInfo('option'));
    AssertFalse(Snapshot.ContentUpdatesPending);
  finally
    Snapshot.Free;
  end;
  AssertEquals('000102FF', ReadValue(Path('cópia com acentos.ibiblia'),
    'select hex(payload) from custom_data'));
  AssertEquals('27', ReadValue(Path('cópia com acentos.ibiblia'), 'pragma user_version'));
end;

procedure TProjectSaveSnapshotTests.CopiesDeferredImmediateAndSilentMetadataInReadOrder;
var Writer: psqlite3; Snapshot: TProjectPersistence;
begin
  Writer := OpenDatabase(FSourceName);
  Snapshot := TProjectPersistence.Create;
  try
    Exec(Writer, 'begin immediate');
    FPersistence.WriteInfoImmediate('queued only', 'queued value');
    FPersistence.WriteInfoImmediate('option', 'queued option');
    FPersistence.StageInfo('option', 'staged option');
    FPersistence.StageInfoSilently('option', '');
    FPersistence.StageInfoSilently('marcador', '1,1,2');
    FPersistence.WriteSnapshotTo(Path('metadata.ibiblia'));
    Snapshot.Open(Path('metadata.ibiblia'));
    AssertEquals('queued value', Snapshot.ReadInfo('queued only'));
    AssertEquals('', Snapshot.ReadInfo('option'));
    AssertEquals('1,1,2', Snapshot.ReadInfo('marcador'));
    AssertEquals('original option', Scalar(Writer,
      'select valor from info where id = ''option'''));
    AssertEquals('0', Scalar(Writer,
      'select count(*) from info where id = ''queued only'''));
    AssertEquals('', FPersistence.ReadInfo('option'));
    AssertTrue(FPersistence.ContentUpdatesPending);
    Exec(Writer, 'rollback');
    FPersistence.FlushImmediateInfo;
    AssertEquals('Snapshotting must not consume queued source writes',
      'queued value', ReadValue(FSourceName,
      'select valor from info where id = ''queued only'''));
  finally
    Snapshot.Free;
    sqlite3_close(Writer);
  end;
end;

procedure TProjectSaveSnapshotTests.CopiesCommittedWalContentAndSchema;
var Writer: psqlite3; OriginalBytes, WalBytes: string;
begin
  Writer := OpenDatabase(FSourceName);
  try
    AssertEquals('wal', Scalar(Writer, 'pragma journal_mode=wal'));
    Exec(Writer, 'pragma wal_autocheckpoint=0');
    Exec(Writer, 'insert into info values (''in wal'', ''committed in wal'')');
    Exec(Writer, 'create table wal_table(value text)');
    Exec(Writer, 'insert into wal_table values (''wal table content'')');
    FPersistence.WriteCurrentComments('pending comment');
    OriginalBytes := ReadBytes(FSourceName);
    WalBytes := ReadBytes(FSourceName + '-wal');
    FPersistence.WriteSnapshotTo(Path('wal-copy.ibiblia'));
    AssertEquals(OriginalBytes, ReadBytes(FSourceName));
    AssertEquals(WalBytes, ReadBytes(FSourceName + '-wal'));
    AssertEquals('committed in wal', ReadValue(Path('wal-copy.ibiblia'),
      'select valor from info where id = ''in wal'''));
    AssertEquals('wal table content', ReadValue(Path('wal-copy.ibiblia'),
      'select value from wal_table'));
    AssertEquals('pending comment', ReadValue(Path('wal-copy.ibiblia'),
      'select pare_comentarios from pares where pare_id = ''1,1,1'''));
    AssertFalse(FileExists(Path('wal-copy.ibiblia-wal')));
    AssertEquals('delete', ReadValue(Path('wal-copy.ibiblia'), 'pragma journal_mode'));
  finally
    sqlite3_close(Writer);
  end;
end;

procedure TProjectSaveSnapshotTests.FailedCommitStateRemainsRecoverableAfterSnapshot;
var Writer: psqlite3; Failed: Boolean; BeforeBytes: string;
begin
  FPersistence.WriteCurrentComments('pending first');
  FPersistence.LocateReference('1,1,2');
  FPersistence.WriteCurrentComments('pending second');
  Writer := OpenDatabase(FSourceName);
  try
    Exec(Writer, 'begin immediate');
    Failed := False;
    try
      FPersistence.CommitPairs;
    except
      on E: Exception do Failed := True;
    end;
    AssertTrue('Competing writer must trigger tracked recovery', Failed);
    Exec(Writer, 'rollback');
    // Represent an earlier batch already made durable by a partial commit.
    Exec(Writer, 'update pares set pare_comentarios = ''pending first'' ' +
      'where pare_id = ''1,1,1''');
    BeforeBytes := ReadBytes(FSourceName);
    FPersistence.WriteSnapshotTo(Path('recovery.ibiblia'));
    AssertEquals(BeforeBytes, ReadBytes(FSourceName));
    AssertTrue(FPersistence.ContentUpdatesPending);
    AssertEquals('pending first', ReadValue(Path('recovery.ibiblia'),
      'select pare_comentarios from pares where pare_id = ''1,1,1'''));
    AssertEquals('pending second', ReadValue(Path('recovery.ibiblia'),
      'select pare_comentarios from pares where pare_id = ''1,1,2'''));
    FPersistence.CommitPairs;
    AssertFalse('The source retains a functioning retry path',
      FPersistence.ContentUpdatesPending);
    AssertEquals('pending second', ReadValue(FSourceName,
      'select pare_comentarios from pares where pare_id = ''1,1,2'''));
  finally
    sqlite3_close(Writer);
  end;
end;

procedure TProjectSaveSnapshotTests.FailedOverlayRemovesOnlyItsOwnSnapshot;
var Database: psqlite3; Failed: Boolean; BeforeBytes: string;
begin
  Database := OpenDatabase(FSourceName);
  try
    Exec(Database, 'create trigger reject_snapshot_update before update on pares ' +
      'begin select raise(abort, ''rejected for test''); end');
  finally
    sqlite3_close(Database);
  end;
  BeforeBytes := ReadBytes(FSourceName);
  FPersistence.WriteCurrentComments('unsaved comment');
  Failed := False;
  try
    FPersistence.WriteSnapshotTo(Path('failed.ibiblia'));
  except
    on E: Exception do Failed := True;
  end;
  AssertTrue(Failed);
  AssertFalse(FileExists(Path('failed.ibiblia')));
  AssertFalse(FileExists(Path('failed.ibiblia-journal')));
  AssertFalse(FileExists(Path('failed.ibiblia-wal')));
  AssertEquals(BeforeBytes, ReadBytes(FSourceName));
  AssertEquals('unsaved comment', FPersistence.CurrentComments);
  AssertTrue(FPersistence.ContentUpdatesPending);
end;

procedure TProjectSaveSnapshotTests.ExistingAndSamePhysicalDestinationsArePreserved;
var
  Failed: Boolean;
  BeforeBytes, AliasName: string;
  {$IFDEF WINDOWS}AliasWide, SourceWide: UnicodeString;{$ENDIF}
begin
  BeforeBytes := ReadBytes(FSourceName);
  FPersistence.WriteCurrentComments('unsaved');
  Failed := False;
  try
    FPersistence.WriteSnapshotTo(FSourceName);
  except
    on E: Exception do Failed := True;
  end;
  AssertTrue(Failed);
  AssertEquals(BeforeBytes, ReadBytes(FSourceName));
  AliasName := Path('source-alias.ibiblia');
  {$IFDEF WINDOWS}
  AliasWide := UTF8Decode(AliasName);
  SourceWide := UTF8Decode(FSourceName);
  AssertTrue('Creating hardlink fixture',
    CreateHardLinkW(PWideChar(AliasWide), PWideChar(SourceWide), nil));
  {$ELSE}
  AssertEquals(0, fpLink(FSourceName, AliasName));
  {$ENDIF}
  AssertTrue(SamePhysicalFile(FSourceName, AliasName));
  Failed := False;
  try
    CopySQLiteDatabase(FSourceName, AliasName, True);
  except
    on E: ESQLiteDatabaseCopyError do
    begin
      Failed := True;
      AssertEquals(SQLITE_MISUSE, E.SQLiteCode);
    end;
  end;
  AssertTrue(Failed);
  AssertEquals(BeforeBytes, ReadBytes(AliasName));
  CreateDatabase(Path('existing.ibiblia'));
  BeforeBytes := ReadBytes(Path('existing.ibiblia'));
  Failed := False;
  try
    FPersistence.WriteSnapshotTo(Path('existing.ibiblia'));
  except
    on E: Exception do Failed := True;
  end;
  AssertTrue(Failed);
  AssertEquals(BeforeBytes, ReadBytes(Path('existing.ibiblia')));
end;

procedure TProjectSaveSnapshotTests.NewPublicationCreatesCompleteUnicodeProject;
var Staging: TProjectSaveStaging; StageName, BeforeBytes: string;
begin
  BeforeBytes := ReadBytes(FSourceName);
  Staging := TProjectSaveStaging.Create(Path('tradução revisada.ibiblia'), False);
  StageName := Staging.StagingFileName;
  try
    FPersistence.WriteCurrentComments('cópia completa');
    FPersistence.WriteSnapshotTo(StageName);
    Staging.Publish;
    AssertFalse(FileExists(StageName));
    AssertEquals('cópia completa', ReadValue(Staging.FinalFileName,
      'select pare_comentarios from pares where pare_id = ''1,1,1'''));
    AssertEquals('ok', ReadValue(Staging.FinalFileName, 'pragma quick_check'));
    AssertEquals(BeforeBytes, ReadBytes(FSourceName));
  finally
    Staging.Free;
  end;
  AssertTrue(FileExists(Path('tradução revisada.ibiblia')));
end;
procedure TProjectSaveSnapshotTests.NewPublicationPreservesFileThatAppearedMeanwhile;
var Staging: TProjectSaveStaging; Failed: Boolean; BeforeBytes: string;
begin
  Staging := TProjectSaveStaging.Create(Path('new.ibiblia'), False);
  try
    FPersistence.WriteCurrentComments('new content');
    FPersistence.WriteSnapshotTo(Staging.StagingFileName);
    CreateDatabase(Staging.FinalFileName);
    BeforeBytes := ReadBytes(Staging.FinalFileName);
    Failed := False;
    try
      Staging.Publish;
    except
      on E: Exception do Failed := True;
    end;
    AssertTrue(Failed);
    AssertEquals(BeforeBytes, ReadBytes(Staging.FinalFileName));
  finally
    Staging.Free;
  end;
end;

procedure TProjectSaveSnapshotTests.ExistingPublicationRespectsSQLiteWriterLock;
var Staging: TProjectSaveStaging; Writer: psqlite3; Failed: Boolean;
begin
  CreateDatabase(Path('existing.ibiblia'));
  Staging := TProjectSaveStaging.Create(Path('existing.ibiblia'), True);
  Writer := OpenDatabase(Staging.FinalFileName);
  try
    FPersistence.WriteCurrentComments('new comments');
    FPersistence.WriteSnapshotTo(Staging.StagingFileName);
    Exec(Writer, 'begin immediate');
    Exec(Writer, 'update info set valor = ''uncommitted'' where id = ''option''');
    Failed := False;
    try
      Staging.Publish;
    except
      on E: ESQLiteDatabaseCopyError do
      begin
        Failed := True;
        AssertEquals(SQLITE_BUSY, E.SQLiteCode and $FF);
      end;
    end;
    AssertTrue(Failed);
    AssertEquals('uncommitted', Scalar(Writer,
      'select valor from info where id = ''option'''));
    Exec(Writer, 'rollback');
    AssertEquals('old comments', Scalar(Writer,
      'select pare_comentarios from pares where pare_id = ''1,1,1'''));
    Staging.Publish;
    AssertEquals('new comments', Scalar(Writer,
      'select pare_comentarios from pares where pare_id = ''1,1,1'''));
  finally
    sqlite3_close(Writer);
    Staging.Free;
  end;
end;

procedure TProjectSaveSnapshotTests.ExistingWalReaderRetainsItsSnapshot;
var Staging: TProjectSaveStaging; Reader: psqlite3;
begin
  CreateDatabase(Path('existing.ibiblia'));
  Staging := TProjectSaveStaging.Create(Path('existing.ibiblia'), True);
  Reader := OpenDatabase(Staging.FinalFileName);
  try
    AssertEquals('wal', Scalar(Reader, 'pragma journal_mode=wal'));
    Exec(Reader, 'begin');
    AssertEquals('old comments', Scalar(Reader,
      'select pare_comentarios from pares where pare_id = ''1,1,1'''));
    FPersistence.WriteCurrentComments('new comments');
    FPersistence.WriteSnapshotTo(Staging.StagingFileName);
    Staging.Publish;
    AssertEquals('old comments', Scalar(Reader,
      'select pare_comentarios from pares where pare_id = ''1,1,1'''));
    Exec(Reader, 'rollback');
    AssertEquals('new comments', Scalar(Reader,
      'select pare_comentarios from pares where pare_id = ''1,1,1'''));
  finally
    sqlite3_close(Reader);
    Staging.Free;
  end;
end;

procedure TProjectSaveSnapshotTests.UnpublishedSnapshotAndSidecarsAreRemoved;
var Staging: TProjectSaveStaging; StageName: string; Stream: TFileStream;
begin
  Staging := TProjectSaveStaging.Create(Path('never-published.ibiblia'), False);
  StageName := Staging.StagingFileName;
  try
    FPersistence.WriteSnapshotTo(StageName);
    Stream := TFileStream.Create(StageName + '-journal', fmCreate);
    Stream.Free;
  finally
    Staging.Free;
  end;
  AssertFalse(FileExists(Path('never-published.ibiblia')));
  AssertFalse(FileExists(StageName));
  AssertFalse(FileExists(StageName + '-journal'));
end;

procedure TProjectSaveSnapshotTests.InvalidStagingPreservesExistingDestination;
var Staging: TProjectSaveStaging; Stream: TFileStream; Failed: Boolean;
  BeforeBytes, Invalid: string;
begin
  CreateDatabase(Path('existing.ibiblia'));
  BeforeBytes := ReadBytes(Path('existing.ibiblia'));
  Staging := TProjectSaveStaging.Create(Path('existing.ibiblia'), True);
  try
    Stream := TFileStream.Create(Staging.StagingFileName, fmCreate);
    try
      Invalid := 'invalid database';
      Stream.WriteBuffer(Invalid[1], Length(Invalid));
    finally
      Stream.Free;
    end;
    Failed := False;
    try
      Staging.Publish;
    except
      on E: Exception do Failed := True;
    end;
    AssertTrue(Failed);
    AssertEquals(BeforeBytes, ReadBytes(Staging.FinalFileName));
  finally
    Staging.Free;
  end;
end;

initialization
  RegisterTest(TProjectSaveSnapshotTests);

end.
