unit export_file_staging_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, sqlite3conn, sqldb,
  export_file_staging, MySwordModule, TheWordDictionary;

type
  TExportFileStagingTests = class(TTestCase)
  private
    FTempDir: string;
    function TempPath(const Name: string): string;
    procedure WriteBytes(const FileName, Bytes: string);
    function ReadBytes(const FileName: string): string;
    function DatabaseCount(const FileName, TableName: string): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure UnpublishedPartialWritePreservesOldFile;
    procedure UnpublishedWriteLeavesNewDestinationAbsent;
    procedure PublishReplacesExistingUnicodeDestination;
    procedure LockedDestinationPreservesOldFile;
    procedure ClosedMySwordAndDictionaryModulesCanBePublished;
  end;

implementation

function TExportFileStagingTests.TempPath(const Name: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + Name;
end;

procedure TExportFileStagingTests.WriteBytes(const FileName, Bytes: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Bytes <> '' then
      Stream.WriteBuffer(Bytes[1], Length(Bytes));
  finally
    Stream.Free;
  end;
end;

function TExportFileStagingTests.ReadBytes(const FileName: string): string;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Result <> '' then
      Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

function TExportFileStagingTests.DatabaseCount(
  const FileName, TableName: string): Integer;
var
  Connection: TSQLite3Connection;
  Query: TSQLQuery;
begin
  Connection := TSQLite3Connection.Create(nil);
  try
    Connection.Transaction := TSQLTransaction.Create(Connection);
    Connection.DatabaseName := FileName;
    Query := TSQLQuery.Create(Connection);
    Query.DataBase := Connection;
    Query.Transaction := Connection.Transaction;
    Query.SQL.Text := 'select count(*) from ' + TableName;
    Query.Open;
    Result := Query.Fields[0].AsInteger;
    Query.Close;
    Connection.Transaction.Rollback;
    Connection.Close;
  finally
    Connection.Free;
  end;
end;

procedure TExportFileStagingTests.SetUp;
begin
  inherited SetUp;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia_export_staging_%x_%x', [PtrUInt(Self), GetTickCount64]);
  ForceDirectories(FTempDir);
end;

procedure TExportFileStagingTests.TearDown;
var
  Search: TSearchRec;
begin
  if FindFirst(TempPath('*'), faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then
        DeleteFile(TempPath(Search.Name));
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(FTempDir);
  inherited TearDown;
end;

procedure TExportFileStagingTests.UnpublishedPartialWritePreservesOldFile;
var
  Staging: TExportFileStaging;
  StagingName: string;
begin
  WriteBytes(TempPath('old.ont'), 'previous export');
  Staging := TExportFileStaging.Create(TempPath('old.ont'));
  StagingName := Staging.StagingFileName;
  try
    AssertFalse('SQLite needs a path that does not exist yet', FileExists(StagingName));
    WriteBytes(StagingName, 'partial export');
    WriteBytes(StagingName + '-journal', 'partial journal');
  finally
    Staging.Free;
  end;
  AssertEquals('previous export', ReadBytes(TempPath('old.ont')));
  AssertFalse(FileExists(StagingName));
  AssertFalse(FileExists(StagingName + '-journal'));
end;

procedure TExportFileStagingTests.UnpublishedWriteLeavesNewDestinationAbsent;
var
  Staging: TExportFileStaging;
begin
  Staging := TExportFileStaging.Create(TempPath('new.ont'));
  try
    WriteBytes(Staging.StagingFileName, 'partial export');
  finally
    Staging.Free;
  end;
  AssertFalse(FileExists(TempPath('new.ont')));
end;

procedure TExportFileStagingTests.PublishReplacesExistingUnicodeDestination;
var
  Staging: TExportFileStaging;
  FinalName, StagingName: string;
begin
  FinalName := TempPath('exportação bíblica.ont');
  WriteBytes(FinalName, 'previous export');
  Staging := TExportFileStaging.Create(FinalName);
  StagingName := Staging.StagingFileName;
  try
    WriteBytes(StagingName, 'complete export');
    Staging.Publish;
  finally
    Staging.Free;
  end;
  AssertEquals('complete export', ReadBytes(FinalName));
  AssertFalse(FileExists(StagingName));
end;

procedure TExportFileStagingTests.LockedDestinationPreservesOldFile;
{$IFDEF WINDOWS}
var
  Staging: TExportFileStaging;
  Lock: TFileStream;
  ErrorMessage: string;
begin
  WriteBytes(TempPath('locked.ont'), 'previous export');
  Staging := TExportFileStaging.Create(TempPath('locked.ont'));
  try
    WriteBytes(Staging.StagingFileName, 'complete export');
    Lock := TFileStream.Create(Staging.FinalFileName, fmOpenRead or fmShareExclusive);
    try
      ErrorMessage := '';
      try
        Staging.Publish;
      except
        on E: Exception do ErrorMessage := E.Message;
      end;
      AssertTrue('the locked output must reject publication', ErrorMessage <> '');
    finally
      Lock.Free;
    end;
    AssertEquals('previous export', ReadBytes(Staging.FinalFileName));
  finally
    Staging.Free;
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TExportFileStagingTests.ClosedMySwordAndDictionaryModulesCanBePublished;
var
  Staging: TExportFileStaging;
  Bible: TMySwordModule;
  Dictionary: TTheWordDictionary;
begin
  WriteBytes(TempPath('bible.bbl.mybible'), 'previous export');
  Staging := TExportFileStaging.Create(TempPath('bible.bbl.mybible'));
  try
    Bible := TMySwordModule.Create(Staging.StagingFileName, False, True, True,
      'short.title=Staging test');
    try
      Bible.AddVerse('Word<WG3056>', 43, 1, 1);
      Bible.Commit;
    finally
      Bible.Free;
    end;
    Staging.Publish;
    AssertEquals(1, DatabaseCount(Staging.FinalFileName, 'Bible'));
  finally
    Staging.Free;
  end;

  WriteBytes(TempPath('concordance.dct.twm'), 'previous export');
  Staging := TExportFileStaging.Create(TempPath('concordance.dct.twm'));
  try
    Dictionary := TTheWordDictionary.Create(Staging.StagingFileName, True);
    try
      Dictionary.AddEntry('G3056', 'Logos', 'Word');
      Dictionary.Commit;
    finally
      Dictionary.Free;
    end;
    Staging.Publish;
    AssertEquals(1, DatabaseCount(Staging.FinalFileName, 'content'));
  finally
    Staging.Free;
  end;
end;

initialization
  RegisterTest(TExportFileStagingTests);

end.
