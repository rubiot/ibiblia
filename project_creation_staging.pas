unit project_creation_staging;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TProjectCreationStaging = class
  private
    FFinalFileName: string;
    FStagingFileName: string;
    FPublished: Boolean;
    FDiscarded: Boolean;
    class function NewStagingFileName(const AFinalFileName: string): string;
      static;
  public
    constructor Create(const AFinalFileName: string);
    destructor Destroy; override;
    procedure Publish;
    procedure Discard;
    property FinalFileName: string read FFinalFileName;
    property StagingFileName: string read FStagingFileName;
  end;

implementation

resourcestring
  SFinalProjectFileNameRequired = 'A final project file name is required';
  SProjectStagingAlreadyDiscarded = 'The staged project has already been discarded';
  SProjectStagingAlreadyPublished = 'The staged project has already been published';
  SProjectStagingFileMissing = 'The staged project file does not exist: %s';
  SFinalProjectAlreadyExists = 'A project already exists at the final path: %s';
  SProjectStagingPublishFailed = 'Could not publish the staged project from %s to %s: %s';
  SProjectStagingDiscardFailed = 'Could not remove the staged project file %s: %s';

class function TProjectCreationStaging.NewStagingFileName(
  const AFinalFileName: string): string;
var
  FileExtension: string;
  FileStem: string;
  Guid: TGUID;
  Suffix: string;
begin
  FileExtension := ExtractFileExt(AFinalFileName);
  FileStem := Copy(AFinalFileName, 1,
    Length(AFinalFileName) - Length(FileExtension));
  repeat
    if CreateGUID(Guid) <> 0 then
      raise Exception.Create('Could not generate a unique staging file name');
    Suffix := GUIDToString(Guid);
    Suffix := StringReplace(Suffix, '{', '', [rfReplaceAll]);
    Suffix := StringReplace(Suffix, '}', '', [rfReplaceAll]);
    Result := FileStem + '.creating-' + Suffix + FileExtension;
  until not FileExists(Result) and not DirectoryExists(Result);
end;

constructor TProjectCreationStaging.Create(const AFinalFileName: string);
begin
  inherited Create;
  if Trim(AFinalFileName) = '' then
    raise Exception.Create(SFinalProjectFileNameRequired);

  FFinalFileName := ExpandFileName(AFinalFileName);
  FStagingFileName := NewStagingFileName(FFinalFileName);
  FPublished := False;
  FDiscarded := False;
end;

destructor TProjectCreationStaging.Destroy;
begin
  if not FPublished then
  begin
    try
      Discard;
    except
      { Destructors must not mask the exception that caused project creation
        to abort. An explicit Discard still reports cleanup failures. }
    end;
  end;
  inherited Destroy;
end;

procedure TProjectCreationStaging.Publish;
var
  ErrorCode: Integer;
begin
  if FPublished then
    raise Exception.Create(SProjectStagingAlreadyPublished);
  if FDiscarded then
    raise Exception.Create(SProjectStagingAlreadyDiscarded);
  if not FileExists(FStagingFileName) then
    raise Exception.CreateFmt(SProjectStagingFileMissing, [FStagingFileName]);
  if FileExists(FFinalFileName) or DirectoryExists(FFinalFileName) then
    raise Exception.CreateFmt(SFinalProjectAlreadyExists, [FFinalFileName]);

  if not RenameFile(FStagingFileName, FFinalFileName) then
  begin
    ErrorCode := GetLastOSError;
    raise Exception.CreateFmt(SProjectStagingPublishFailed,
      [FStagingFileName, FFinalFileName, SysErrorMessage(ErrorCode)]);
  end;

  FPublished := True;
end;

procedure TProjectCreationStaging.Discard;
var
  ErrorCode: Integer;
begin
  if FPublished or FDiscarded then
    Exit;

  if FileExists(FStagingFileName) and not DeleteFile(FStagingFileName) then
  begin
    ErrorCode := GetLastOSError;
    raise Exception.CreateFmt(SProjectStagingDiscardFailed,
      [FStagingFileName, SysErrorMessage(ErrorCode)]);
  end;

  FDiscarded := True;
end;

end.
