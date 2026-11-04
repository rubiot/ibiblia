unit project_save_staging;

{$mode objfpc}{$H+}

interface

uses SysUtils;

type
  TProjectSaveStaging = class
  private
    FFinalFileName: string;
    FStagingFileName: string;
    FOverwriteExisting: Boolean;
    FPublished: Boolean;
  public
    constructor Create(const AFinalFileName: string; OverwriteExisting: Boolean);
    destructor Destroy; override;
    procedure Publish;
    property FinalFileName: string read FFinalFileName;
    property StagingFileName: string read FStagingFileName;
  end;

implementation

uses sqlite_database_copy
  {$IFDEF WINDOWS}, Windows{$ELSE}, BaseUnix{$ENDIF};

resourcestring
  SSaveProjectFileNameRequired = 'A project file name is required';
  SSaveProjectAlreadyExists = 'A file already exists at the project destination: %s';
  SSaveProjectIsDirectory = 'The project destination is a directory: %s';
  SSaveProjectStageMissing = 'The completed project copy is missing: %s';
  SSaveProjectStagePublished = 'The project copy has already been published';
  SSaveProjectPublishFailed = 'Could not save the project copy to %s: %s';

constructor TProjectSaveStaging.Create(const AFinalFileName: string;
  OverwriteExisting: Boolean);
var Guid: TGUID; Extension, Stem, Suffix: string;
begin
  inherited Create;
  if Trim(AFinalFileName) = '' then
    raise Exception.Create(SSaveProjectFileNameRequired);
  FFinalFileName := ExpandFileName(AFinalFileName);
  FOverwriteExisting := OverwriteExisting;
  if DirectoryExists(FFinalFileName) then
    raise Exception.CreateFmt(SSaveProjectIsDirectory, [FFinalFileName]);
  if FileExists(FFinalFileName) and not FOverwriteExisting then
    raise Exception.CreateFmt(SSaveProjectAlreadyExists, [FFinalFileName]);
  Extension := ExtractFileExt(FFinalFileName);
  Stem := Copy(FFinalFileName, 1, Length(FFinalFileName) - Length(Extension));
  repeat
    if CreateGUID(Guid) <> 0 then
      raise Exception.Create('Could not generate a project staging file name');
    Suffix := StringReplace(StringReplace(GUIDToString(Guid), '{', '',
      [rfReplaceAll]), '}', '', [rfReplaceAll]);
    FStagingFileName := Stem + '.saving-' + Suffix + Extension;
  until not FileExists(FStagingFileName) and
    not DirectoryExists(FStagingFileName) and
    not FileExists(FStagingFileName + '-journal') and
    not FileExists(FStagingFileName + '-wal') and
    not FileExists(FStagingFileName + '-shm');
end;

destructor TProjectSaveStaging.Destroy;
begin
  if FStagingFileName <> '' then DeleteSQLiteFiles(FStagingFileName);
  inherited Destroy;
end;

procedure TProjectSaveStaging.Publish;
var
  ReplaceInvalidFile: Boolean;
  ErrorCode: Integer;
  {$IFDEF WINDOWS}
  StageWide, FinalWide: UnicodeString;
  Handle: THandle;
  Flags: DWORD;
  {$ENDIF}
begin
  if FPublished then raise Exception.Create(SSaveProjectStagePublished);
  if not FileExists(FStagingFileName) then
    raise Exception.CreateFmt(SSaveProjectStageMissing, [FStagingFileName]);
  ValidateSQLiteDatabase(FStagingFileName);
  if DirectoryExists(FFinalFileName) then
    raise Exception.CreateFmt(SSaveProjectIsDirectory, [FFinalFileName]);
  ReplaceInvalidFile := False;
  if FileExists(FFinalFileName) then
  begin
    if not FOverwriteExisting then
      raise Exception.CreateFmt(SSaveProjectAlreadyExists, [FFinalFileName]);
    try
      CopySQLiteDatabase(FStagingFileName, FFinalFileName, True);
      FPublished := True;
      Exit;
    except
      on E: ESQLiteDatabaseCopyError do
        if E.DestinationInvalid then ReplaceInvalidFile := True
        else raise;
    end;
  end;
  // Do not rename over a database's leftover journal/WAL. An existing SQLite
  // destination above is always updated through SQLite's own locking protocol.
  if FileExists(FFinalFileName + '-journal') or
    FileExists(FFinalFileName + '-wal') or
    FileExists(FFinalFileName + '-shm') then
    raise Exception.CreateFmt(SSaveProjectAlreadyExists, [FFinalFileName]);
  {$IFDEF WINDOWS}
  StageWide := UTF8Decode(FStagingFileName);
  FinalWide := UTF8Decode(FFinalFileName);
  Handle := CreateFileW(PWideChar(StageWide), GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  try
    if not FlushFileBuffers(Handle) then RaiseLastOSError;
  finally
    CloseHandle(Handle);
  end;
  Flags := $00000008; // MOVEFILE_WRITE_THROUGH
  if ReplaceInvalidFile then Flags := Flags or MOVEFILE_REPLACE_EXISTING;
  // Without REPLACE_EXISTING, a file appearing after validation is preserved.
  if not MoveFileExW(PWideChar(StageWide), PWideChar(FinalWide), Flags) then
  begin
    ErrorCode := GetLastOSError;
    raise Exception.CreateFmt(SSaveProjectPublishFailed,
      [FFinalFileName, SysErrorMessage(ErrorCode)]);
  end;
  {$ELSE}
  if ReplaceInvalidFile then
  begin
    if not RenameFile(FStagingFileName, FFinalFileName) then RaiseLastOSError;
  end
  else
  begin
    // link is exclusive and both paths are on the same filesystem.
    if fpLink(FStagingFileName, FFinalFileName) <> 0 then
    begin
      ErrorCode := fpGetErrno;
      raise Exception.CreateFmt(SSaveProjectPublishFailed,
        [FFinalFileName, SysErrorMessage(ErrorCode)]);
    end;
    SysUtils.DeleteFile(FStagingFileName);
  end;
  {$ENDIF}
  FPublished := True;
end;

end.
