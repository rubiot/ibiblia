unit project_creation_staging_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, project_creation_staging;

type
  TProjectCreationStagingTests = class(TTestCase)
  private
    FTempDir: string;
    function TempPath(const AFileName: string): string;
    procedure WriteBytes(const AFileName: string; const ABytes: RawByteString);
    function ReadBytes(const AFileName: string): RawByteString;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure DestructorDiscardsUnpublishedStagingFile;
    procedure DiscardRemovesStagingFileAndLeavesFinalAbsent;
    procedure PublishMovesStagingFileAndPreservesBytes;
    procedure PublishDoesNotOverwriteExistingFinalFile;
  end;

implementation

function TProjectCreationStagingTests.TempPath(
  const AFileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + AFileName;
end;

procedure TProjectCreationStagingTests.WriteBytes(const AFileName: string;
  const ABytes: RawByteString);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    if ABytes <> '' then
      Stream.WriteBuffer(ABytes[1], Length(ABytes));
  finally
    Stream.Free;
  end;
end;

function TProjectCreationStagingTests.ReadBytes(
  const AFileName: string): RawByteString;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, Stream.Size);
    if Result <> '' then
      Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

procedure TProjectCreationStagingTests.SetUp;
begin
  inherited SetUp;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia_project_staging_%x_%x',
      [PtrUInt(Self), GetTickCount64]);
  ForceDirectories(FTempDir);
end;

procedure TProjectCreationStagingTests.TearDown;
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

procedure TProjectCreationStagingTests.
  DestructorDiscardsUnpublishedStagingFile;
var
  FinalFile: string;
  Staging: TProjectCreationStaging;
  StagingFile: string;
begin
  FinalFile := TempPath('unpublished.bib');
  Staging := TProjectCreationStaging.Create(FinalFile);
  StagingFile := Staging.StagingFileName;
  WriteBytes(StagingFile, 'partial project');

  Staging.Free;

  AssertFalse('unpublished staging must be deleted', FileExists(StagingFile));
  AssertFalse('unpublished project must never appear at the final path',
    FileExists(FinalFile));
end;

procedure TProjectCreationStagingTests.
  DiscardRemovesStagingFileAndLeavesFinalAbsent;
var
  FinalFile: string;
  Staging: TProjectCreationStaging;
  StagingFile: string;
begin
  FinalFile := TempPath('discarded.bib');
  Staging := TProjectCreationStaging.Create(FinalFile);
  try
    StagingFile := Staging.StagingFileName;
    WriteBytes(StagingFile, 'invalid project');

    Staging.Discard;

    AssertFalse('Discard must remove the staging file',
      FileExists(StagingFile));
    AssertFalse('Discard must not create the final file',
      FileExists(FinalFile));
  finally
    Staging.Free;
  end;
end;

procedure TProjectCreationStagingTests.
  PublishMovesStagingFileAndPreservesBytes;
const
  PROJECT_BYTES: RawByteString = #$53#$51#$4C#$69#$74#$65#$00#$FF;
var
  FinalFile: string;
  Staging: TProjectCreationStaging;
  StagingFile: string;
begin
  FinalFile := TempPath('published.bib');
  Staging := TProjectCreationStaging.Create(FinalFile);
  try
    StagingFile := Staging.StagingFileName;
    WriteBytes(StagingFile, PROJECT_BYTES);

    Staging.Publish;

    AssertFalse('Publish must consume the staging path',
      FileExists(StagingFile));
    AssertTrue('Publish must create the final path', FileExists(FinalFile));
    AssertEquals('Publish must preserve the file byte for byte', PROJECT_BYTES,
      ReadBytes(FinalFile));
  finally
    Staging.Free;
  end;
end;

procedure TProjectCreationStagingTests.
  PublishDoesNotOverwriteExistingFinalFile;
const
  EXISTING_BYTES: RawByteString = 'existing project';
  STAGED_BYTES: RawByteString = 'replacement project';
var
  FinalFile: string;
  RaisedError: Boolean;
  Staging: TProjectCreationStaging;
  StagingFile: string;
begin
  FinalFile := TempPath('existing.bib');
  WriteBytes(FinalFile, EXISTING_BYTES);
  Staging := TProjectCreationStaging.Create(FinalFile);
  try
    StagingFile := Staging.StagingFileName;
    WriteBytes(StagingFile, STAGED_BYTES);
    RaisedError := False;

    try
      Staging.Publish;
    except
      on E: Exception do
        RaisedError := True;
    end;

    AssertTrue('publishing over an existing project must fail', RaisedError);
    AssertEquals('the existing project must remain byte-identical',
      EXISTING_BYTES, ReadBytes(FinalFile));
    AssertTrue('failed publication remains staged until discard/destruction',
      FileExists(StagingFile));
  finally
    Staging.Free;
  end;

  AssertFalse('destruction must clean a failed publication staging file',
    FileExists(StagingFile));
end;

initialization
  RegisterTest(TProjectCreationStagingTests);
end.
