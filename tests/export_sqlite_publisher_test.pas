unit export_sqlite_publisher_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, sqlite3dyn,
  export_sqlite_publisher;

type
  TExportSQLitePublisherTests = class(TTestCase)
  private
    FDirectory: string;
    function Path(const Name: string): string;
    function OpenDatabase(const FileName: string; Flags: Integer): psqlite3;
    procedure Exec(Database: psqlite3; const SQL: string);
    function Scalar(Database: psqlite3; const SQL: string): string;
    procedure CreateDatabase(const FileName, Value: string; PageSize: Integer = 4096);
    procedure WriteBytes(const FileName, Value: string);
    function ReadBytes(const FileName: string): string;
    procedure AssertPublicationFailure(const SourceName, DestinationName: string;
      ExpectedCode: Integer; ExpectInvalidDestination: Boolean = False);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure IdleOpenConnectionSeesPublishedDatabase;
    procedure RollbackReaderPreservesOldDatabaseWhenPublicationIsBusy;
    procedure ActiveWriterPreservesItsTransactionWhenPublicationIsBusy;
    procedure WalReaderKeepsSnapshotUntilItsTransactionEnds;
    procedure WalPageSizeMismatchPreservesOldDatabase;
    procedure InvalidSourceDoesNotAllowDestinationReplacement;
    procedure InvalidDestinationIsReportedWithoutModification;
    procedure MissingDestinationIsNeverCreated;
  end;

implementation

function TExportSQLitePublisherTests.Path(const Name: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FDirectory) + Name;
end;

function TExportSQLitePublisherTests.OpenDatabase(const FileName: string;
  Flags: Integer): psqlite3;
var Code: Integer;
begin
  Result := nil;
  Code := sqlite3_open_v2(PAnsiChar(FileName), @Result,
    Flags or SQLITE_OPEN_PRIVATECACHE, nil);
  if Code <> SQLITE_OK then
  begin
    if Assigned(Result) then sqlite3_close(Result);
    Result := nil;
    Fail(Format('Unable to open the temporary fixture, SQLite %d', [Code]));
  end;
end;

procedure TExportSQLitePublisherTests.Exec(Database: psqlite3; const SQL: string);
var Code: Integer;
begin
  Code := sqlite3_exec(Database, PAnsiChar(SQL), nil, nil, nil);
  AssertEquals(string(sqlite3_errmsg(Database)), SQLITE_OK, Code);
end;

function TExportSQLitePublisherTests.Scalar(Database: psqlite3;
  const SQL: string): string;
var
  Statement: psqlite3_stmt;
  Code: Integer;
begin
  Statement := nil;
  try
    Code := sqlite3_prepare_v2(Database, PAnsiChar(SQL), -1, @Statement, nil);
    AssertEquals(string(sqlite3_errmsg(Database)), SQLITE_OK, Code);
    Code := sqlite3_step(Statement);
    AssertEquals(string(sqlite3_errmsg(Database)), SQLITE_ROW, Code);
    Result := string(sqlite3_column_text(Statement, 0));
  finally
    if Assigned(Statement) then sqlite3_finalize(Statement);
  end;
end;

procedure TExportSQLitePublisherTests.CreateDatabase(
  const FileName, Value: string; PageSize: Integer);
var Database: psqlite3;
begin
  Database := OpenDatabase(FileName, SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE);
  try
    Exec(Database, 'PRAGMA page_size=' + IntToStr(PageSize));
    Exec(Database, 'CREATE TABLE content(value TEXT)');
    Exec(Database, 'INSERT INTO content VALUES (' + QuotedStr(Value) + ')');
  finally
    sqlite3_close(Database);
  end;
end;

procedure TExportSQLitePublisherTests.WriteBytes(const FileName, Value: string);
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Value <> '' then Stream.WriteBuffer(Value[1], Length(Value));
  finally
    Stream.Free;
  end;
end;

function TExportSQLitePublisherTests.ReadBytes(const FileName: string): string;
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

procedure TExportSQLitePublisherTests.AssertPublicationFailure(
  const SourceName, DestinationName: string; ExpectedCode: Integer;
  ExpectInvalidDestination: Boolean);
var Failed: Boolean;
begin
  Failed := False;
  try
    PublishExistingSQLiteExport(SourceName, DestinationName, 0);
  except
    on E: ESQLiteExportPublishError do
    begin
      Failed := True;
      AssertEquals(E.Message, ExpectedCode, E.SQLiteCode and $FF);
      AssertEquals('Only an invalid destination permits rename fallback',
        ExpectInvalidDestination, E.DestinationInvalid);
    end;
  end;
  AssertTrue('The publication must fail without replacing its destination', Failed);
end;

procedure TExportSQLitePublisherTests.SetUp;
begin
  inherited SetUp;
  FDirectory := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia_sqlite_export_%x_%x', [PtrUInt(Self), GetTickCount64]);
  ForceDirectories(FDirectory);
  InitializeSQLite('');
end;

procedure TExportSQLitePublisherTests.TearDown;
var Search: TSearchRec;
begin
  ReleaseSQLite;
  if FindFirst(Path('*'), faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then
        DeleteFile(Path(Search.Name));
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(FDirectory);
  inherited TearDown;
end;

procedure TExportSQLitePublisherTests.IdleOpenConnectionSeesPublishedDatabase;
const
  Extensions: array[0..1] of string = ('.dct.twm', '.bbl.mybible');
var
  Reader, Source: psqlite3;
  Extension, SourceName, DestinationName, SourceBefore: string;
begin
  for Extension in Extensions do
  begin
    SourceName := Path('exportação nova' + Extension);
    DestinationName := Path('módulo aberto' + Extension);
    CreateDatabase(SourceName, 'new export');
    CreateDatabase(DestinationName, 'old export');
    Source := OpenDatabase(SourceName, SQLITE_OPEN_READWRITE);
    try
      Exec(Source, 'CREATE TABLE new_metadata(value TEXT)');
      Exec(Source, 'INSERT INTO new_metadata VALUES (''published'')');
    finally
      sqlite3_close(Source);
    end;
    SourceBefore := ReadBytes(SourceName);
    Reader := OpenDatabase(DestinationName, SQLITE_OPEN_READONLY);
    try
      AssertEquals('old export', Scalar(Reader, 'SELECT value FROM content'));
      PublishExistingSQLiteExport(SourceName, DestinationName);
      AssertEquals('new export', Scalar(Reader, 'SELECT value FROM content'));
      AssertEquals('published', Scalar(Reader, 'SELECT value FROM new_metadata'));
      AssertEquals('ok', Scalar(Reader, 'PRAGMA quick_check'));
      AssertEquals('The source remains available for staging cleanup',
        SourceBefore, ReadBytes(SourceName));
    finally
      sqlite3_close(Reader);
    end;
  end;
end;

procedure TExportSQLitePublisherTests.
  RollbackReaderPreservesOldDatabaseWhenPublicationIsBusy;
var
  Reader: psqlite3;
  OriginalBytes: string;
begin
  CreateDatabase(Path('new.twm'), 'new export');
  CreateDatabase(Path('old.twm'), 'old export');
  OriginalBytes := ReadBytes(Path('old.twm'));
  Reader := OpenDatabase(Path('old.twm'), SQLITE_OPEN_READONLY);
  try
    Exec(Reader, 'BEGIN');
    AssertEquals('old export', Scalar(Reader, 'SELECT value FROM content'));
    AssertPublicationFailure(Path('new.twm'), Path('old.twm'), SQLITE_BUSY);
    AssertEquals(OriginalBytes, ReadBytes(Path('old.twm')));
    AssertEquals('old export', Scalar(Reader, 'SELECT value FROM content'));
    Exec(Reader, 'ROLLBACK');
    PublishExistingSQLiteExport(Path('new.twm'), Path('old.twm'));
    AssertEquals('new export', Scalar(Reader, 'SELECT value FROM content'));
  finally
    sqlite3_close(Reader);
  end;
end;

procedure TExportSQLitePublisherTests.
  ActiveWriterPreservesItsTransactionWhenPublicationIsBusy;
var Writer: psqlite3;
begin
  CreateDatabase(Path('new.twm'), 'new export');
  CreateDatabase(Path('old.twm'), 'old export');
  Writer := OpenDatabase(Path('old.twm'), SQLITE_OPEN_READWRITE);
  try
    Exec(Writer, 'BEGIN IMMEDIATE');
    Exec(Writer, 'UPDATE content SET value = ''uncommitted''');
    AssertPublicationFailure(Path('new.twm'), Path('old.twm'), SQLITE_BUSY);
    AssertEquals('uncommitted', Scalar(Writer, 'SELECT value FROM content'));
    Exec(Writer, 'ROLLBACK');
    AssertEquals('old export', Scalar(Writer, 'SELECT value FROM content'));
  finally
    sqlite3_close(Writer);
  end;
end;

procedure TExportSQLitePublisherTests.WalReaderKeepsSnapshotUntilItsTransactionEnds;
var Reader: psqlite3;
begin
  CreateDatabase(Path('new.twm'), 'new export');
  CreateDatabase(Path('old.twm'), 'old export');
  Reader := OpenDatabase(Path('old.twm'), SQLITE_OPEN_READWRITE);
  try
    AssertEquals('wal', Scalar(Reader, 'PRAGMA journal_mode=WAL'));
    Exec(Reader, 'BEGIN');
    AssertEquals('old export', Scalar(Reader, 'SELECT value FROM content'));
    PublishExistingSQLiteExport(Path('new.twm'), Path('old.twm'));
    AssertEquals('An existing reader retains its consistent snapshot',
      'old export', Scalar(Reader, 'SELECT value FROM content'));
    Exec(Reader, 'ROLLBACK');
    AssertEquals('new export', Scalar(Reader, 'SELECT value FROM content'));
    AssertEquals('wal', Scalar(Reader, 'PRAGMA journal_mode'));
    AssertEquals('ok', Scalar(Reader, 'PRAGMA quick_check'));
  finally
    sqlite3_close(Reader);
  end;
end;

procedure TExportSQLitePublisherTests.WalPageSizeMismatchPreservesOldDatabase;
var Reader: psqlite3;
begin
  CreateDatabase(Path('new.twm'), 'new export', 8192);
  CreateDatabase(Path('old.twm'), 'old export', 4096);
  Reader := OpenDatabase(Path('old.twm'), SQLITE_OPEN_READWRITE);
  try
    AssertEquals('wal', Scalar(Reader, 'PRAGMA journal_mode=WAL'));
    AssertPublicationFailure(Path('new.twm'), Path('old.twm'), SQLITE_READONLY);
    AssertEquals('old export', Scalar(Reader, 'SELECT value FROM content'));
    AssertEquals('wal', Scalar(Reader, 'PRAGMA journal_mode'));
  finally
    sqlite3_close(Reader);
  end;
end;

procedure TExportSQLitePublisherTests.InvalidSourceDoesNotAllowDestinationReplacement;
var OriginalBytes: string;
begin
  WriteBytes(Path('new.twm'), 'invalid staged export');
  CreateDatabase(Path('old.twm'), 'old export');
  OriginalBytes := ReadBytes(Path('old.twm'));
  AssertPublicationFailure(Path('new.twm'), Path('old.twm'), SQLITE_NOTADB);
  AssertEquals(OriginalBytes, ReadBytes(Path('old.twm')));
end;

procedure TExportSQLitePublisherTests.InvalidDestinationIsReportedWithoutModification;
begin
  CreateDatabase(Path('new.twm'), 'new export');
  WriteBytes(Path('old.twm'), 'previous non-database export');
  AssertPublicationFailure(Path('new.twm'), Path('old.twm'), SQLITE_NOTADB, True);
  AssertEquals('previous non-database export', ReadBytes(Path('old.twm')));
end;

procedure TExportSQLitePublisherTests.MissingDestinationIsNeverCreated;
begin
  CreateDatabase(Path('new.twm'), 'new export');
  AssertPublicationFailure(Path('new.twm'), Path('missing.twm'), SQLITE_CANTOPEN);
  AssertFalse(FileExists(Path('missing.twm')));
end;

initialization
  RegisterTest(TExportSQLitePublisherTests);

end.
