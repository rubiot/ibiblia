unit export_sqlite_publisher;

{$mode objfpc}{$H+}

interface

uses SysUtils;

type
  ESQLiteExportPublishError = class(Exception)
  private
    FSQLiteCode: Integer;
    FDestinationInvalid: Boolean;
  public
    constructor Create(const MessageText: string; ACode: Integer);
    property SQLiteCode: Integer read FSQLiteCode;
    property DestinationInvalid: Boolean read FDestinationInvalid;
  end;

// Both paths must exist. The caller retains ownership of the staging file.
procedure PublishExistingSQLiteExport(const StagingFileName,
  FinalFileName: string; BusyTimeoutMS: Integer = 250);

implementation

uses sqlite3dyn;

resourcestring
  SSQLiteExportBusy = 'The exported module is locked by another application. Its previous contents were preserved. Close the active module view and retry: %s';
  SSQLiteExportPublishFailed = 'Could not update the exported SQLite module %s: %s (SQLite %d)';
  SSQLiteExportFileMissing = 'The SQLite export file does not exist: %s';
  SSQLiteExportSameFile = 'The staged SQLite export and its destination must be different files';
  SSQLiteExportBackupUnavailable = 'The SQLite library does not support publishing database exports';
  SSQLiteExportInvalid = 'The staged SQLite export failed its integrity check';

constructor ESQLiteExportPublishError.Create(const MessageText: string;
  ACode: Integer);
begin
  inherited Create(MessageText);
  FSQLiteCode := ACode;
end;

procedure RaiseSQLitePublishError(Database: psqlite3; Code: Integer;
  const FileName: string);
var
  Details: string;
begin
  if (Code and $FF) in [SQLITE_BUSY, SQLITE_LOCKED] then
    raise ESQLiteExportPublishError.Create(
      Format(SSQLiteExportBusy, [FileName]), Code);
  Details := '';
  if Assigned(Database) then
    Details := string(sqlite3_errmsg(Database));
  raise ESQLiteExportPublishError.Create(
    Format(SSQLiteExportPublishFailed, [FileName, Details, Code]), Code);
end;

procedure CheckSourceDatabase(Database: psqlite3; const FileName: string);
const
  CheckSQL: PAnsiChar = 'PRAGMA quick_check';
var
  Statement: psqlite3_stmt;
  Code: Integer;
  CheckResult: string;
begin
  Statement := nil;
  Code := sqlite3_prepare_v2(Database, CheckSQL, -1, @Statement, nil);
  try
    if Code <> SQLITE_OK then
      RaiseSQLitePublishError(Database, Code, FileName);
    Code := sqlite3_step(Statement);
    if Code <> SQLITE_ROW then
      RaiseSQLitePublishError(Database, Code, FileName);
    CheckResult := string(sqlite3_column_text(Statement, 0));
    if CheckResult <> 'ok' then
      raise ESQLiteExportPublishError.Create(SSQLiteExportInvalid, SQLITE_CORRUPT);
    Code := sqlite3_step(Statement);
    if Code <> SQLITE_DONE then
      RaiseSQLitePublishError(Database, Code, FileName);
  finally
    if Assigned(Statement) then
      sqlite3_finalize(Statement);
  end;
end;

procedure CheckDestinationDatabase(Database: psqlite3; const FileName: string);
const
  CheckSQL: PAnsiChar = 'PRAGMA schema_version';
var
  Statement: psqlite3_stmt;
  Code: Integer;
begin
  Statement := nil;
  try
    try
      Code := sqlite3_prepare_v2(Database, CheckSQL, -1, @Statement, nil);
      if Code <> SQLITE_OK then
        RaiseSQLitePublishError(Database, Code, FileName);
      Code := sqlite3_step(Statement);
      if Code <> SQLITE_ROW then
        RaiseSQLitePublishError(Database, Code, FileName);
    except
      on E: ESQLiteExportPublishError do
      begin
        E.FDestinationInvalid := (E.SQLiteCode and $FF) = SQLITE_NOTADB;
        raise;
      end;
    end;
  finally
    if Assigned(Statement) then
      sqlite3_finalize(Statement);
  end;
end;

procedure PublishExistingSQLiteExport(const StagingFileName,
  FinalFileName: string; BusyTimeoutMS: Integer);
var
  Source, Destination: psqlite3;
  Backup: psqlite3backup;
  SourceName, DestinationName: string;
  Code, StepCode, FinishCode: Integer;
begin
  SourceName := ExpandFileName(StagingFileName);
  DestinationName := ExpandFileName(FinalFileName);
  if SameFileName(SourceName, DestinationName) then
    raise ESQLiteExportPublishError.Create(SSQLiteExportSameFile, SQLITE_MISUSE);
  if not FileExists(SourceName) then
    raise ESQLiteExportPublishError.Create(
      Format(SSQLiteExportFileMissing, [SourceName]), SQLITE_CANTOPEN);
  if not FileExists(DestinationName) then
    raise ESQLiteExportPublishError.Create(
      Format(SSQLiteExportFileMissing, [DestinationName]), SQLITE_CANTOPEN);
  if BusyTimeoutMS < 0 then
    BusyTimeoutMS := 0;

  Source := nil;
  Destination := nil;
  Backup := nil;
  InitializeSQLite('');
  try
    if not Assigned(sqlite3_backup_init) or not Assigned(sqlite3_backup_step) or
       not Assigned(sqlite3_backup_finish) then
      raise ESQLiteExportPublishError.Create(
        SSQLiteExportBackupUnavailable, SQLITE_MISUSE);
    Code := sqlite3_open_v2(PAnsiChar(SourceName), @Source,
      SQLITE_OPEN_READONLY or SQLITE_OPEN_PRIVATECACHE, nil);
    if Code <> SQLITE_OK then
      RaiseSQLitePublishError(Source, Code, SourceName);
    CheckSourceDatabase(Source, SourceName);

    // No CREATE flag: a disappearing target must not become an empty database.
    Code := sqlite3_open_v2(PAnsiChar(DestinationName), @Destination,
      SQLITE_OPEN_READWRITE or SQLITE_OPEN_PRIVATECACHE, nil);
    if Code <> SQLITE_OK then
      RaiseSQLitePublishError(Destination, Code, DestinationName);
    Code := sqlite3_busy_timeout(Destination, BusyTimeoutMS);
    if Code <> SQLITE_OK then
      RaiseSQLitePublishError(Destination, Code, DestinationName);
    CheckDestinationDatabase(Destination, DestinationName);
    Code := sqlite3_exec(Destination, 'PRAGMA synchronous=FULL', nil, nil, nil);
    if Code <> SQLITE_OK then
      RaiseSQLitePublishError(Destination, Code, DestinationName);

    Backup := sqlite3_backup_init(Destination, 'main', Source, 'main');
    if not Assigned(Backup) then
      RaiseSQLitePublishError(Destination, sqlite3_errcode(Destination), DestinationName);
    // SQLite owns the destination transaction. Do not use its handle until finish.
    // https://sqlite.org/c3ref/backup_finish.html
    StepCode := sqlite3_backup_step(Backup, -1);
    FinishCode := sqlite3_backup_finish(Backup);
    Backup := nil;
    // finish can return OK after BUSY; only DONE means the transaction committed.
    if StepCode <> SQLITE_DONE then
      RaiseSQLitePublishError(Destination, StepCode, DestinationName);
    if FinishCode <> SQLITE_OK then
      RaiseSQLitePublishError(Destination, FinishCode, DestinationName);
  finally
    // An unfinished backup rolls back all destination changes.
    if Assigned(Backup) then
      sqlite3_backup_finish(Backup);
    if Assigned(Destination) then
      sqlite3_close(Destination);
    if Assigned(Source) then
      sqlite3_close(Source);
    ReleaseSQLite;
  end;
end;

end.
