unit sqlite_database_copy;

{$mode objfpc}{$H+}

interface

uses SysUtils;

type
  ESQLiteDatabaseCopyError = class(Exception)
  private
    FSQLiteCode: Integer;
    FDestinationInvalid: Boolean;
  public
    constructor Create(const MessageText: string; ACode: Integer;
      ADestinationInvalid: Boolean = False);
    property SQLiteCode: Integer read FSQLiteCode;
    property DestinationInvalid: Boolean read FDestinationInvalid;
  end;

function SamePhysicalFile(const FirstFileName, SecondFileName: string): Boolean;
procedure ValidateSQLiteDatabase(const FileName: string);
// A new destination is reserved exclusively and removed if copying fails.
// Existing destinations are updated in SQLite's rollback-safe backup transaction.
procedure CopySQLiteDatabase(const SourceFileName, DestinationFileName: string;
  DestinationMustExist: Boolean; BusyTimeoutMS: Integer = 250);
procedure DeleteSQLiteFiles(const FileName: string);

implementation

uses sqlite3dyn
  {$IFDEF WINDOWS}, Windows{$ELSE}, BaseUnix{$ENDIF};

resourcestring
  SSQLiteCopySameFile = 'The database copy and its source must be different files';
  SSQLiteCopyMissing = 'The database file does not exist: %s';
  SSQLiteCopyExists = 'A file already exists at the database destination: %s';
  SSQLiteCopyUnavailable = 'The SQLite library does not support copying databases';
  SSQLiteCopyInvalid = 'The database failed its integrity check: %s';
  SSQLiteCopyFailed = 'Could not copy the database %s: %s (SQLite %d)';
  SSQLiteCopyBusy = 'The database is in use. Its previous contents were preserved. Close the other application and retry: %s';

constructor ESQLiteDatabaseCopyError.Create(const MessageText: string;
  ACode: Integer; ADestinationInvalid: Boolean);
begin
  inherited Create(MessageText);
  FSQLiteCode := ACode;
  FDestinationInvalid := ADestinationInvalid;
end;

function SamePhysicalFile(const FirstFileName, SecondFileName: string): Boolean;
{$IFDEF WINDOWS}
var
  FirstHandle, SecondHandle: THandle;
  FirstInfo, SecondInfo: TByHandleFileInformation;
  FirstWide, SecondWide: UnicodeString;
{$ELSE}
var FirstInfo, SecondInfo: Stat;
{$ENDIF}
begin
  Result := SameFileName(ExpandFileName(FirstFileName),
    ExpandFileName(SecondFileName));
  if Result then Exit;
  {$IFDEF WINDOWS}
  FirstWide := UTF8Decode(ExpandFileName(FirstFileName));
  SecondWide := UTF8Decode(ExpandFileName(SecondFileName));
  FirstHandle := CreateFileW(PWideChar(FirstWide), 0,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if FirstHandle = INVALID_HANDLE_VALUE then Exit;
  try
    SecondHandle := CreateFileW(PWideChar(SecondWide), 0,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
      OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
    if SecondHandle = INVALID_HANDLE_VALUE then Exit;
    try
      Result := GetFileInformationByHandle(FirstHandle, FirstInfo) and
        GetFileInformationByHandle(SecondHandle, SecondInfo) and
        (FirstInfo.dwVolumeSerialNumber = SecondInfo.dwVolumeSerialNumber) and
        (FirstInfo.nFileIndexHigh = SecondInfo.nFileIndexHigh) and
        (FirstInfo.nFileIndexLow = SecondInfo.nFileIndexLow);
    finally
      CloseHandle(SecondHandle);
    end;
  finally
    CloseHandle(FirstHandle);
  end;
  {$ELSE}
  Result := (fpStat(FirstFileName, FirstInfo) = 0) and
    (fpStat(SecondFileName, SecondInfo) = 0) and
    (FirstInfo.st_dev = SecondInfo.st_dev) and
    (FirstInfo.st_ino = SecondInfo.st_ino);
  {$ENDIF}
end;

procedure DeleteSQLiteFiles(const FileName: string);
const Suffixes: array[0..3] of string = ('-journal', '-wal', '-shm', '');
var Suffix: string;
begin
  for Suffix in Suffixes do
    SysUtils.DeleteFile(FileName + Suffix);
end;

procedure RaiseCopyError(Database: psqlite3; Code: Integer;
  const FileName: string; DestinationInvalid: Boolean = False);
var Details: string;
begin
  if (Code and $FF) in [SQLITE_BUSY, SQLITE_LOCKED] then
    raise ESQLiteDatabaseCopyError.Create(
      Format(SSQLiteCopyBusy, [FileName]), Code);
  Details := '';
  if Assigned(Database) then Details := string(sqlite3_errmsg(Database));
  raise ESQLiteDatabaseCopyError.Create(
    Format(SSQLiteCopyFailed, [FileName, Details, Code]), Code,
    DestinationInvalid);
end;

procedure CheckDatabase(Database: psqlite3; const FileName: string;
  FullCheck: Boolean);
var
  Statement: psqlite3_stmt;
  SQL: AnsiString;
  Code: Integer;
begin
  Statement := nil;
  if FullCheck then SQL := 'PRAGMA quick_check'
  else SQL := 'PRAGMA schema_version';
  try
    Code := sqlite3_prepare_v2(Database, PAnsiChar(SQL), -1, @Statement, nil);
    if Code <> SQLITE_OK then
      RaiseCopyError(Database, Code, FileName,
        not FullCheck and ((Code and $FF) = SQLITE_NOTADB));
    Code := sqlite3_step(Statement);
    if Code <> SQLITE_ROW then
      RaiseCopyError(Database, Code, FileName,
        not FullCheck and ((Code and $FF) = SQLITE_NOTADB));
    if FullCheck then
    begin
      if string(sqlite3_column_text(Statement, 0)) <> 'ok' then
        raise ESQLiteDatabaseCopyError.Create(
          Format(SSQLiteCopyInvalid, [FileName]), SQLITE_CORRUPT);
      Code := sqlite3_step(Statement);
      if Code <> SQLITE_DONE then RaiseCopyError(Database, Code, FileName);
    end;
  finally
    if Assigned(Statement) then sqlite3_finalize(Statement);
  end;
end;

procedure ValidateSQLiteDatabase(const FileName: string);
var Database: psqlite3; Code: Integer; Name: string;
begin
  Name := ExpandFileName(FileName);
  Database := nil;
  InitializeSQLite('');
  try
    Code := sqlite3_open_v2(PAnsiChar(Name), @Database,
      SQLITE_OPEN_READONLY or SQLITE_OPEN_PRIVATECACHE, nil);
    if Code <> SQLITE_OK then RaiseCopyError(Database, Code, Name);
    CheckDatabase(Database, Name, True);
  finally
    if Assigned(Database) then sqlite3_close(Database);
    ReleaseSQLite;
  end;
end;

procedure ReserveDestination(const FileName: string);
{$IFDEF WINDOWS}
var Handle: THandle; WideName: UnicodeString;
{$ELSE}
var Handle: cint;
{$ENDIF}
begin
  if FileExists(FileName + '-journal') or FileExists(FileName + '-wal') or
    FileExists(FileName + '-shm') then
    raise ESQLiteDatabaseCopyError.Create(
      Format(SSQLiteCopyExists, [FileName]), SQLITE_CANTOPEN);
  {$IFDEF WINDOWS}
  WideName := UTF8Decode(FileName);
  Handle := CreateFileW(PWideChar(WideName), GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  CloseHandle(Handle);
  {$ELSE}
  Handle := fpOpen(FileName, O_WRONLY or O_CREAT or O_EXCL, &600);
  if Handle < 0 then RaiseLastOSError;
  fpClose(Handle);
  {$ENDIF}
end;

procedure CopySQLiteDatabase(const SourceFileName, DestinationFileName: string;
  DestinationMustExist: Boolean; BusyTimeoutMS: Integer);
var
  Source, Destination: psqlite3;
  Backup: psqlite3backup;
  SourceName, DestinationName: string;
  Code, StepCode, FinishCode: Integer;
  CreatedDestination, Completed: Boolean;
begin
  SourceName := ExpandFileName(SourceFileName);
  DestinationName := ExpandFileName(DestinationFileName);
  if SamePhysicalFile(SourceName, DestinationName) then
    raise ESQLiteDatabaseCopyError.Create(SSQLiteCopySameFile, SQLITE_MISUSE);
  if not FileExists(SourceName) then
    raise ESQLiteDatabaseCopyError.Create(
      Format(SSQLiteCopyMissing, [SourceName]), SQLITE_CANTOPEN);
  if DestinationMustExist and not FileExists(DestinationName) then
    raise ESQLiteDatabaseCopyError.Create(
      Format(SSQLiteCopyMissing, [DestinationName]), SQLITE_CANTOPEN);
  if not DestinationMustExist and
    (FileExists(DestinationName) or DirectoryExists(DestinationName)) then
    raise ESQLiteDatabaseCopyError.Create(
      Format(SSQLiteCopyExists, [DestinationName]), SQLITE_CANTOPEN);
  if BusyTimeoutMS < 0 then BusyTimeoutMS := 0;

  Source := nil;
  Destination := nil;
  Backup := nil;
  CreatedDestination := False;
  Completed := False;
  InitializeSQLite('');
  try
    if not Assigned(sqlite3_backup_init) or not Assigned(sqlite3_backup_step) or
      not Assigned(sqlite3_backup_finish) then
      raise ESQLiteDatabaseCopyError.Create(SSQLiteCopyUnavailable, SQLITE_MISUSE);
    Code := sqlite3_open_v2(PAnsiChar(SourceName), @Source,
      SQLITE_OPEN_READONLY or SQLITE_OPEN_PRIVATECACHE, nil);
    if Code <> SQLITE_OK then RaiseCopyError(Source, Code, SourceName);
    sqlite3_busy_timeout(Source, BusyTimeoutMS);
    CheckDatabase(Source, SourceName, True);
    if not DestinationMustExist then
    begin
      ReserveDestination(DestinationName);
      CreatedDestination := True;
    end;
    // Never allow sqlite3_open to create a target that disappeared meanwhile.
    Code := sqlite3_open_v2(PAnsiChar(DestinationName), @Destination,
      SQLITE_OPEN_READWRITE or SQLITE_OPEN_PRIVATECACHE, nil);
    if Code <> SQLITE_OK then RaiseCopyError(Destination, Code, DestinationName);
    Code := sqlite3_busy_timeout(Destination, BusyTimeoutMS);
    if Code <> SQLITE_OK then RaiseCopyError(Destination, Code, DestinationName);
    CheckDatabase(Destination, DestinationName, False);
    Code := sqlite3_exec(Destination, 'PRAGMA synchronous=FULL', nil, nil, nil);
    if Code <> SQLITE_OK then RaiseCopyError(Destination, Code, DestinationName);
    Backup := sqlite3_backup_init(Destination, 'main', Source, 'main');
    if not Assigned(Backup) then
      RaiseCopyError(Destination, sqlite3_errcode(Destination), DestinationName);
    StepCode := sqlite3_backup_step(Backup, -1);
    FinishCode := sqlite3_backup_finish(Backup);
    Backup := nil;
    // finish can return OK after BUSY; only DONE committed the destination.
    if StepCode <> SQLITE_DONE then
      RaiseCopyError(Destination, StepCode, DestinationName);
    if FinishCode <> SQLITE_OK then
      RaiseCopyError(Destination, FinishCode, DestinationName);
    if CreatedDestination then
    begin
      // The unpublished snapshot must remain a self-contained file.
      Code := sqlite3_exec(Destination, 'PRAGMA journal_mode=DELETE', nil, nil, nil);
      if Code <> SQLITE_OK then RaiseCopyError(Destination, Code, DestinationName);
    end;
    Completed := True;
  finally
    if Assigned(Backup) then sqlite3_backup_finish(Backup);
    if Assigned(Destination) then sqlite3_close(Destination);
    if Assigned(Source) then sqlite3_close(Source);
    ReleaseSQLite;
    if CreatedDestination and not Completed then DeleteSQLiteFiles(DestinationName);
  end;
end;

end.
