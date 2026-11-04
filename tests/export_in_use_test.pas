unit export_in_use_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, export_text_publisher
  {$IFDEF WINDOWS}, Windows{$ENDIF};

{$IFDEF WINDOWS}
type
  TCopyFailure = (cfNone, cfAfterPartialCopy, cfAfterCompleteCopy);

  TFaultingTextPublisher = class(TExportTextPublisher)
  protected
    procedure CopyContents(Source, Destination: TStream;
      Phase: TExportCopyPhase); override;
  public
    Failures: array[TExportCopyPhase] of TCopyFailure;
    Calls: array[TExportCopyPhase] of Integer;
  end;

  TExportInUseTests = class(TTestCase)
  private
    FTempDir, FFinalName, FStagingName: string;
    function OpenHandle(const FileName: string; Access, ShareMode: DWORD): THandle;
    procedure WriteBytes(const FileName, Bytes: string);
    function ReadHandle(Handle: THandle): string;
    function ReadBytes(const FileName: string): string;
    procedure Prepare(const OldBytes, NewBytes: string);
    procedure AssertNoBackups;
    function PublishError(Publisher: TExportTextPublisher): string;
    procedure CheckSharedReplacement(const OldBytes, NewBytes: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SharedReaderSeesShorterUnicodeExport;
    procedure SharedReaderSeesLargerExport;
    procedure EmptyReplacementTruncatesFile;
    procedure ReaderDenyingWritesPreservesDestination;
    procedure ExistingWriterIsRejectedWithoutMutation;
    procedure ReadOnlyFileAttributePreservesDestination;
    procedure BackupFailureLeavesDestinationUntouched;
    procedure PartialGrowingWriteRestoresOriginalBytesAndLength;
    procedure FailureAfterShrinkRestoresOriginalBytesAndLength;
    procedure FailedRestorationPreservesOriginalBackupAfterPublisherFree;
    procedure MappedFileShrinkFailureRestoresOriginalWithoutResizing;
    procedure MappedFileAcceptsReplacementOfSameLength;
  end;
{$ENDIF}

implementation

{$IFDEF WINDOWS}
procedure TFaultingTextPublisher.CopyContents(Source, Destination: TStream;
  Phase: TExportCopyPhase);
const
  PhaseNames: array[TExportCopyPhase] of string =
    ('backup injected failure', 'replacement injected failure',
     'restore injected failure');
var
  Buffer: array[0..63] of Byte;
  Count: LongInt;
begin
  Inc(Calls[Phase]);
  if Failures[Phase] = cfAfterPartialCopy then
  begin
    // Touch the real file before failing, so rollback is exercised on disk.
    Source.Position := 0;
    Destination.Position := 0;
    Count := SizeOf(Buffer);
    if Source.Size < Count then Count := Source.Size;
    if Count > 0 then
    begin
      Source.ReadBuffer(Buffer, Count);
      Destination.WriteBuffer(Buffer, Count);
    end;
    raise Exception.Create(PhaseNames[Phase]);
  end;
  inherited CopyContents(Source, Destination, Phase);
  if Failures[Phase] = cfAfterCompleteCopy then
    raise Exception.Create(PhaseNames[Phase]);
end;

procedure TExportInUseTests.SetUp;
var
  Id: TGUID;
begin
  inherited SetUp;
  if CreateGUID(Id) <> 0 then
    raise Exception.Create('Could not create the export test directory name');
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'ibiblia_export_in_use_' + Copy(GUIDToString(Id), 2, 36);
  AssertTrue(ForceDirectories(FTempDir));
  FFinalName := IncludeTrailingPathDelimiter(FTempDir) + 'Bíblia em uso.ont';
  FStagingName := IncludeTrailingPathDelimiter(FTempDir) + 'tradução pronta.ont';
end;

procedure TExportInUseTests.TearDown;
var
  Search: TSearchRec;
  FileName: string;
begin
  if FindFirst(IncludeTrailingPathDelimiter(FTempDir) + '*', faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then
      begin
        FileName := IncludeTrailingPathDelimiter(FTempDir) + Search.Name;
        SetFileAttributesW(PWideChar(UTF8Decode(FileName)), FILE_ATTRIBUTE_NORMAL);
        DeleteFileW(PWideChar(UTF8Decode(FileName)));
      end;
    until FindNext(Search) <> 0;
  finally
    SysUtils.FindClose(Search);
  end;
  RemoveDir(FTempDir);
  inherited TearDown;
end;

function TExportInUseTests.OpenHandle(const FileName: string;
  Access, ShareMode: DWORD): THandle;
begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), Access, ShareMode, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Result = INVALID_HANDLE_VALUE then RaiseLastOSError;
end;

procedure TExportInUseTests.WriteBytes(const FileName, Bytes: string);
var
  Handle: THandle;
  Stream: THandleStream;
begin
  Handle := CreateFileW(PWideChar(UTF8Decode(FileName)), GENERIC_WRITE,
    FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  try
    Stream := THandleStream.Create(Handle);
    try
      if Bytes <> '' then Stream.WriteBuffer(Bytes[1], Length(Bytes));
    finally
      Stream.Free;
    end;
  finally
    CloseHandle(Handle);
  end;
end;

function TExportInUseTests.ReadHandle(Handle: THandle): string;
var
  Stream: THandleStream;
begin
  Stream := THandleStream.Create(Handle);
  try
    Stream.Position := 0;
    SetLength(Result, Stream.Size);
    if Result <> '' then Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

function TExportInUseTests.ReadBytes(const FileName: string): string;
var
  Handle: THandle;
begin
  Handle := OpenHandle(FileName, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE);
  try
    Result := ReadHandle(Handle);
  finally
    CloseHandle(Handle);
  end;
end;

procedure TExportInUseTests.Prepare(const OldBytes, NewBytes: string);
begin
  WriteBytes(FFinalName, OldBytes);
  WriteBytes(FStagingName, NewBytes);
end;

procedure TExportInUseTests.AssertNoBackups;
var
  Search: TSearchRec;
  Found: Boolean;
begin
  Found := FindFirst(FFinalName + '.export-backup-*', faAnyFile, Search) = 0;
  if Found then SysUtils.FindClose(Search);
  AssertFalse('A successful publication or restoration should remove its backup', Found);
end;

function TExportInUseTests.PublishError(Publisher: TExportTextPublisher): string;
begin
  Result := '';
  try
    Publisher.Publish(FStagingName, FFinalName);
  except
    on E: Exception do Result := E.Message;
  end;
  AssertTrue('The operation must report the injected or real Windows failure', Result <> '');
end;

procedure TExportInUseTests.CheckSharedReplacement(const OldBytes, NewBytes: string);
var
  Reader: THandle;
  Publisher: TExportTextPublisher;
begin
  Prepare(OldBytes, NewBytes);
  // This reader permits writes but denies rename/delete, as a loaded text module does.
  Reader := OpenHandle(FFinalName, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE);
  try
    Publisher := TExportTextPublisher.Create;
    try
      Publisher.Publish(FStagingName, FFinalName);
      AssertEquals('The existing reader must see the exported file, including its new length',
        NewBytes, ReadHandle(Reader));
      AssertEquals(NewBytes, ReadBytes(FFinalName));
      AssertEquals('', Publisher.BackupFileName);
      AssertNoBackups;
    finally
      Publisher.Free;
    end;
  finally
    CloseHandle(Reader);
  end;
end;

procedure TExportInUseTests.SharedReaderSeesShorterUnicodeExport;
begin
  CheckSharedReplacement('Original longer text: ' + StringOfChar('o', 200),
    'No princípio: λόγος.');
end;

procedure TExportInUseTests.SharedReaderSeesLargerExport;
begin
  CheckSharedReplacement('old', 'New export:' + StringOfChar('n', 200000));
end;

procedure TExportInUseTests.EmptyReplacementTruncatesFile;
begin
  CheckSharedReplacement('This text must disappear completely.', '');
end;

procedure TExportInUseTests.ReaderDenyingWritesPreservesDestination;
var
  Reader: THandle;
  Publisher: TFaultingTextPublisher;
begin
  Prepare('Original protected by reader.', 'Replacement.');
  Reader := OpenHandle(FFinalName, GENERIC_READ, FILE_SHARE_READ);
  try
    Publisher := TFaultingTextPublisher.Create;
    try
      PublishError(Publisher);
      AssertEquals('Original protected by reader.', ReadHandle(Reader));
      AssertEquals(0, Publisher.Calls[ecpBackup]);
      AssertEquals('', Publisher.BackupFileName);
      AssertNoBackups;
    finally
      Publisher.Free;
    end;
  finally
    CloseHandle(Reader);
  end;
end;

procedure TExportInUseTests.ExistingWriterIsRejectedWithoutMutation;
var
  Writer: THandle;
  Publisher: TFaultingTextPublisher;
begin
  Prepare('Original protected from competing writers.', 'Replacement.');
  Writer := OpenHandle(FFinalName, GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
  try
    Publisher := TFaultingTextPublisher.Create;
    try
      PublishError(Publisher);
      AssertEquals('Original protected from competing writers.', ReadHandle(Writer));
      AssertEquals('Backup must not start while another writer owns the file',
        0, Publisher.Calls[ecpBackup]);
      AssertNoBackups;
    finally
      Publisher.Free;
    end;
  finally
    CloseHandle(Writer);
  end;
end;

procedure TExportInUseTests.ReadOnlyFileAttributePreservesDestination;
var
  Publisher: TFaultingTextPublisher;
begin
  Prepare('Original read-only module.', 'Replacement.');
  AssertTrue(SetFileAttributesW(PWideChar(UTF8Decode(FFinalName)), FILE_ATTRIBUTE_READONLY));
  try
    Publisher := TFaultingTextPublisher.Create;
    try
      PublishError(Publisher);
      AssertEquals('Original read-only module.', ReadBytes(FFinalName));
      AssertTrue('The exporter must preserve the read-only attribute',
        (GetFileAttributesW(PWideChar(UTF8Decode(FFinalName))) and FILE_ATTRIBUTE_READONLY) <> 0);
      AssertEquals(0, Publisher.Calls[ecpBackup]);
      AssertNoBackups;
    finally
      Publisher.Free;
    end;
  finally
    SetFileAttributesW(PWideChar(UTF8Decode(FFinalName)), FILE_ATTRIBUTE_NORMAL);
  end;
end;

procedure TExportInUseTests.BackupFailureLeavesDestinationUntouched;
var
  Publisher: TFaultingTextPublisher;
  Original: string;
begin
  Original := 'Original:' + StringOfChar('o', 200);
  Prepare(Original, 'Replacement.');
  Publisher := TFaultingTextPublisher.Create;
  try
    Publisher.Failures[ecpBackup] := cfAfterPartialCopy;
    PublishError(Publisher);
    AssertEquals(Original, ReadBytes(FFinalName));
    AssertEquals(0, Publisher.Calls[ecpReplacement]);
    AssertEquals(0, Publisher.Calls[ecpRestore]);
    AssertEquals('', Publisher.BackupFileName);
    AssertNoBackups;
  finally
    Publisher.Free;
  end;
end;

procedure TExportInUseTests.PartialGrowingWriteRestoresOriginalBytesAndLength;
var
  Publisher: TFaultingTextPublisher;
  Reader: THandle;
begin
  Prepare('old', StringOfChar('n', 200));
  Reader := OpenHandle(FFinalName, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE);
  try
    Publisher := TFaultingTextPublisher.Create;
    try
      Publisher.Failures[ecpReplacement] := cfAfterPartialCopy;
      PublishError(Publisher);
      AssertEquals('A failed growing write must restore the old length as well as bytes',
        'old', ReadHandle(Reader));
      AssertEquals(1, Publisher.Calls[ecpRestore]);
      AssertEquals('', Publisher.BackupFileName);
      AssertNoBackups;
    finally
      Publisher.Free;
    end;
  finally
    CloseHandle(Reader);
  end;
end;

procedure TExportInUseTests.FailureAfterShrinkRestoresOriginalBytesAndLength;
var
  Publisher: TFaultingTextPublisher;
  Original: string;
begin
  Original := 'Original:' + StringOfChar('o', 200);
  Prepare(Original, 'new');
  Publisher := TFaultingTextPublisher.Create;
  try
    Publisher.Failures[ecpReplacement] := cfAfterCompleteCopy;
    PublishError(Publisher);
    AssertEquals('Restoration must also regrow a destination already truncated',
      Original, ReadBytes(FFinalName));
    AssertEquals(1, Publisher.Calls[ecpRestore]);
    AssertEquals('', Publisher.BackupFileName);
    AssertNoBackups;
  finally
    Publisher.Free;
  end;
end;

procedure TExportInUseTests.FailedRestorationPreservesOriginalBackupAfterPublisherFree;
var
  Publisher: TFaultingTextPublisher;
  Original, BackupName, ErrorText: string;
begin
  Original := 'Recoverable original:' + StringOfChar('o', 200);
  Prepare(Original, StringOfChar('n', 300));
  Publisher := TFaultingTextPublisher.Create;
  try
    Publisher.Failures[ecpReplacement] := cfAfterCompleteCopy;
    Publisher.Failures[ecpRestore] := cfAfterPartialCopy;
    ErrorText := PublishError(Publisher);
    BackupName := Publisher.BackupFileName;
    AssertTrue('A failed restore must retain a recovery path', BackupName <> '');
    AssertTrue('The reported error must identify the retained recovery file',
      Pos(BackupName, ErrorText) > 0);
    AssertTrue(Pos('replacement injected failure', ErrorText) > 0);
    AssertTrue(Pos('restore injected failure', ErrorText) > 0);
    AssertEquals('The recovery copy must contain the complete original',
      Original, ReadBytes(BackupName));
    AssertEquals(1, Publisher.Calls[ecpRestore]);
  finally
    Publisher.Free;
  end;
  AssertTrue('Recovery must survive helper destruction', FileExists(BackupName));
  AssertEquals(Original, ReadBytes(BackupName));
end;

procedure TExportInUseTests.MappedFileShrinkFailureRestoresOriginalWithoutResizing;
var
  Publisher: TExportTextPublisher;
  Reader, Mapping: THandle;
  View: Pointer;
  Original: string;
begin
  Original := 'Mapped original:' + StringOfChar('o', 200);
  Prepare(Original, 'short new value');
  Reader := OpenHandle(FFinalName, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE);
  try
    Mapping := CreateFileMappingW(Reader, nil, PAGE_READONLY, 0, 0, nil);
    if Mapping = 0 then RaiseLastOSError;
    try
      View := MapViewOfFile(Mapping, FILE_MAP_READ, 0, 0, 0);
      if View = nil then RaiseLastOSError;
      try
        Publisher := TExportTextPublisher.Create;
        try
          // Windows refuses the final shrink after the replacement bytes were written.
          PublishError(Publisher);
          AssertEquals('Rollback should restore bytes at the unchanged original size',
            Original, ReadHandle(Reader));
          AssertEquals('', Publisher.BackupFileName);
          AssertNoBackups;
        finally
          Publisher.Free;
        end;
      finally
        UnmapViewOfFile(View);
      end;
    finally
      CloseHandle(Mapping);
    end;
  finally
    CloseHandle(Reader);
  end;
end;

procedure TExportInUseTests.MappedFileAcceptsReplacementOfSameLength;
var
  Publisher: TExportTextPublisher;
  Reader, Mapping: THandle;
  View: Pointer;
  Replacement: string;
begin
  Replacement := StringOfChar('n', 200);
  Prepare(StringOfChar('o', 200), Replacement);
  Reader := OpenHandle(FFinalName, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE);
  try
    Mapping := CreateFileMappingW(Reader, nil, PAGE_READONLY, 0, 0, nil);
    if Mapping = 0 then RaiseLastOSError;
    try
      View := MapViewOfFile(Mapping, FILE_MAP_READ, 0, 0, 0);
      if View = nil then RaiseLastOSError;
      try
        Publisher := TExportTextPublisher.Create;
        try
          Publisher.Publish(FStagingName, FFinalName);
          AssertEquals('An unchanged size must not request an unnecessary mapped-file resize',
            Replacement, ReadHandle(Reader));
          AssertEquals('', Publisher.BackupFileName);
          AssertNoBackups;
        finally
          Publisher.Free;
        end;
      finally
        UnmapViewOfFile(View);
      end;
    finally
      CloseHandle(Mapping);
    end;
  finally
    CloseHandle(Reader);
  end;
end;
{$ENDIF}

initialization
  {$IFDEF WINDOWS}
  RegisterTest(TExportInUseTests);
  {$ENDIF}

end.
