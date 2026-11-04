unit export_file_staging;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TExportPublishMode = (epReplaceFile, epTextModule, epSQLiteModule);

  TExportFileStaging = class
  private
    FFinalFileName: string;
    FStagingFileName: string;
    FPublished: Boolean;
  public
    constructor Create(const AFinalFileName: string);
    destructor Destroy; override;
    procedure Publish(Mode: TExportPublishMode = epReplaceFile);
    property StagingFileName: string read FStagingFileName;
    property FinalFileName: string read FFinalFileName;
  end;

implementation

uses export_text_publisher, export_sqlite_publisher
{$IFDEF WINDOWS}
  , Windows
{$ENDIF}
  ;

{$IFDEF WINDOWS}
const
  ExportMoveFileWriteThrough = $00000008;
{$ENDIF}

constructor TExportFileStaging.Create(const AFinalFileName: string);
var
  Guid: TGUID;
  Extension, Stem, Suffix: string;
begin
  inherited Create;
  if Trim(AFinalFileName) = '' then
    raise Exception.Create('An export file name is required');
  FFinalFileName := ExpandFileName(AFinalFileName);
  if DirectoryExists(FFinalFileName) then
    raise Exception.CreateFmt('The export destination is a directory: %s',
      [FFinalFileName]);
  Extension := ExtractFileExt(FFinalFileName);
  Stem := Copy(FFinalFileName, 1, Length(FFinalFileName) - Length(Extension));
  repeat
    if CreateGUID(Guid) <> 0 then
      raise Exception.Create('Could not generate an export staging file name');
    Suffix := StringReplace(StringReplace(GUIDToString(Guid), '{', '',
      [rfReplaceAll]), '}', '', [rfReplaceAll]);
    FStagingFileName := Stem + '.exporting-' + Suffix + Extension;
  until not FileExists(FStagingFileName) and not DirectoryExists(FStagingFileName);
end;

destructor TExportFileStaging.Destroy;
const
  Suffixes: array[0..3] of string = ('', '-journal', '-wal', '-shm');
var
  Suffix: string;
begin
  if FStagingFileName <> '' then
    for Suffix in Suffixes do
      SysUtils.DeleteFile(FStagingFileName + Suffix);
  inherited Destroy;
end;

procedure TExportFileStaging.Publish(Mode: TExportPublishMode);
{$IFDEF WINDOWS}
var
  StagedName, FinalName: UnicodeString;
  Handle: THandle;
  ErrorCode: Integer;
  TextPublisher: TExportTextPublisher;
{$ENDIF}
begin
  if FPublished then
    raise Exception.Create('The export has already been published');
  if not FileExists(FStagingFileName) then
    raise Exception.CreateFmt('The staged export is missing: %s',
      [FStagingFileName]);
  if (Mode = epSQLiteModule) and FileExists(FFinalFileName) then
  begin
    try
      // SQLite coordinates with open connections and journals. Replacing the
      // file identity would bypass that coordination, especially with WAL.
      PublishExistingSQLiteExport(FStagingFileName, FFinalFileName);
      FPublished := True;
      Exit;
    except
      on E: ESQLiteExportPublishError do
        if not E.DestinationInvalid then raise;
    end;
    // An existing non-database can still be replaced by the completed module.
  end;
  {$IFDEF WINDOWS}
  StagedName := UTF8Decode(FStagingFileName);
  FinalName := UTF8Decode(FFinalFileName);
  Handle := CreateFileW(PWideChar(StagedName), GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  try
    if not FlushFileBuffers(Handle) then
      RaiseLastOSError;
  finally
    CloseHandle(Handle);
  end;
  // A same-volume replacement never truncates or deletes the old file first.
  if not MoveFileExW(PWideChar(StagedName), PWideChar(FinalName),
    MOVEFILE_REPLACE_EXISTING or ExportMoveFileWriteThrough) then
  begin
    ErrorCode := GetLastOSError;
    if (Mode = epTextModule) and FileExists(FFinalFileName) and
       (ErrorCode in [ERROR_ACCESS_DENIED, ERROR_SHARING_VIOLATION,
         ERROR_LOCK_VIOLATION]) then
    begin
      TextPublisher := TExportTextPublisher.Create;
      try
        TextPublisher.Publish(FStagingFileName, FFinalFileName);
        FPublished := True;
        Exit;
      finally
        TextPublisher.Free;
      end;
    end;
    raise Exception.CreateFmt('Could not publish the export to %s: %s',
      [FFinalFileName, SysErrorMessage(ErrorCode)]);
  end;
  {$ELSE}
  if not RenameFile(FStagingFileName, FFinalFileName) then
    raise Exception.CreateFmt('Could not publish the export to %s: %s',
      [FFinalFileName, SysErrorMessage(GetLastOSError)]);
  {$ENDIF}
  FPublished := True;
end;

end.
