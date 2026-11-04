unit export_destination_history_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, IniFiles, LazFileUtils, LazUTF8, fpcunit, testregistry,
  export_destination_history;

type
  TFaultingHistoryIni = class(TMemIniFile)
  public
    FailRead, FailWrite, FailUpdate: Boolean;
    WriteCount, UpdateCount: Integer;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: string); override;
    procedure UpdateFile; override;
  end;

  TCountingImmediateHistoryIni = class(TIniFile)
  public
    UpdateCount: Integer;
    procedure UpdateFile; override;
  end;

  TExportDestinationHistoryTests = class(TTestCase)
  private
    FTempDir, FIniFileName, FProjectFileName: string;
    FSettings: TFaultingHistoryIni;
    function TempPath(const Name: string): string;
    function OnlySection: string;
    function SectionCount: Integer;
    procedure Remember(const ProjectFileName: string; Kind: TExportDestinationKind;
      const FileName: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MissingHistoryAndLegacyGlobalValuesUseDefault;
    procedure EachExportKindKeepsItsOwnDestinationAndStableKey;
    procedure ProjectsWithSameNameInDifferentFoldersAreIndependent;
    procedure UnicodeAndBracketsRoundTripThroughSafeIniSection;
    procedure RelativeAndDottedProjectPathsUseTheSameHistory;
    {$IFDEF WINDOWS}
    procedure WindowsCaseAndPathSeparatorsUseTheSameHistory;
    procedure WindowsExtendedPathsCanBeRemembered;
    procedure WindowsDottedIAndPlainIProjectsStayIndependent;
    procedure WindowsAccentedCaseReusesExistingSection;
    {$ENDIF}
    procedure RelativeDestinationIsSavedAsAbsoluteBeforeIniCloses;
    procedure MissingDestinationFileIsStillRemembered;
    procedure MissingFolderUsesDefaultAndRecoversWhenFolderReturns;
    procedure MySwordDoesNotReplaceEitherTextDestination;
    procedure StoredMySwordValuesAreIgnoredWithoutErasingThem;
    procedure MySwordNoOpDoesNotRequireAvailableSettings;
    procedure NilAndInvalidInputsDoNotCreateGlobalHistory;
    procedure WriteFailureReturnsMessageAndKeepsPreviousDestination;
    procedure UpdateFailureReturnsMessageAndCanRetry;
    procedure ReadFailureUsesDefaultWithoutWriting;
    procedure MalformedProjectSectionsAreIgnored;
    procedure ImmediateIniWritesOnlyOnceBeforeReturning;
  end;

implementation

function TFaultingHistoryIni.ReadString(const Section, Ident,
  Default: string): string;
begin
  if FailRead then raise Exception.Create('Injected history read failure');
  Result := inherited ReadString(Section, Ident, Default);
end;

procedure TFaultingHistoryIni.WriteString(const Section, Ident, Value: string);
begin
  Inc(WriteCount);
  if FailWrite then raise Exception.Create('Injected history write failure');
  inherited WriteString(Section, Ident, Value);
end;

procedure TFaultingHistoryIni.UpdateFile;
begin
  Inc(UpdateCount);
  if FailUpdate then raise Exception.Create('Injected history flush failure');
  inherited UpdateFile;
end;

procedure TCountingImmediateHistoryIni.UpdateFile;
begin
  Inc(UpdateCount);
  inherited UpdateFile;
end;

procedure TExportDestinationHistoryTests.SetUp;
var
  Id: TGUID;
begin
  inherited SetUp;
  if CreateGUID(Id) <> 0 then
    raise Exception.Create('Could not create export history fixture name');
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'ibiblia_export_history_' + Copy(GUIDToString(Id), 2, 36);
  AssertTrue(ForceDirectories(FTempDir));
  FIniFileName := TempPath('histórico.ini');
  FProjectFileName := TempPath('Projeto [Á].bib');
  FSettings := TFaultingHistoryIni.Create(FIniFileName);
end;

procedure TExportDestinationHistoryTests.TearDown;
var
  Search: TSearchRec;
  FileName: string;
begin
  if Assigned(FSettings) then
  begin
    FSettings.FailRead := False;
    FSettings.FailWrite := False;
    FSettings.FailUpdate := False;
    FreeAndNil(FSettings);
  end;
  if FindFirst(TempPath('*'), faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then
      begin
        FileName := TempPath(Search.Name);
        if (Search.Attr and faDirectory) <> 0 then
          RemoveDir(FileName)
        else
          SysUtils.DeleteFile(FileName);
      end;
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(FTempDir);
  inherited TearDown;
end;

function TExportDestinationHistoryTests.TempPath(const Name: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + Name;
end;

function TExportDestinationHistoryTests.OnlySection: string;
var
  Sections: TStringList;
begin
  Sections := TStringList.Create;
  try
    FSettings.ReadSections(Sections);
    AssertEquals('Fixture must have exactly one project section', 1, Sections.Count);
    Result := Sections[0];
  finally
    Sections.Free;
  end;
end;

function TExportDestinationHistoryTests.SectionCount: Integer;
var
  Sections: TStringList;
begin
  Sections := TStringList.Create;
  try
    FSettings.ReadSections(Sections);
    Result := Sections.Count;
  finally
    Sections.Free;
  end;
end;

procedure TExportDestinationHistoryTests.Remember(const ProjectFileName: string;
  Kind: TExportDestinationKind; const FileName: string);
var
  ErrorMessage: string;
begin
  AssertTrue('Remember destination: ' + FileName,
    TryRememberExportDestination(FSettings, ProjectFileName, Kind,
      FileName, ErrorMessage));
  AssertEquals('', ErrorMessage);
end;

procedure TExportDestinationHistoryTests.MissingHistoryAndLegacyGlobalValuesUseDefault;
begin
  AssertEquals('default.ont', ReadExportDestination(FSettings,
    FProjectFileName, edTaggedTheWord, 'default.ont'));
  AssertEquals(0, SectionCount);
  FSettings.WriteString('export', 'last.tagged.destination.file',
    TempPath('another-project.ont'));
  FSettings.WriteCount := 0;
  AssertEquals('default.ont', ReadExportDestination(FSettings,
    FProjectFileName, edTaggedTheWord, 'default.ont'));
  AssertEquals('Legacy values cannot identify the project that owns them',
    1, SectionCount);
  AssertEquals(0, FSettings.WriteCount);
  AssertEquals(0, FSettings.UpdateCount);
end;

procedure TExportDestinationHistoryTests.EachExportKindKeepsItsOwnDestinationAndStableKey;
const
  Keys: array[TExportDestinationKind] of string = (
    'last.tagged.destination.file', 'last.interlinear.source.file',
    'last.strongs.analytical.concordance.file',
    'last.strongs.synthetic.concordance.file');
var
  Kind: TExportDestinationKind;
  Section, Expected: string;
begin
  for Kind := Low(TExportDestinationKind) to High(TExportDestinationKind) do
    Remember(FProjectFileName, Kind, TempPath(IntToStr(Ord(Kind)) + '.ont'));
  Section := OnlySection;
  for Kind := Low(TExportDestinationKind) to High(TExportDestinationKind) do
  begin
    Expected := TempPath(IntToStr(Ord(Kind)) + '.ont');
    AssertEquals(Expected, ReadExportDestination(FSettings, FProjectFileName,
      Kind, 'default.ont'));
    AssertEquals('The public preference key must remain stable', Expected,
      FSettings.ReadString(Section, Keys[Kind], ''));
  end;
end;

procedure TExportDestinationHistoryTests.ProjectsWithSameNameInDifferentFoldersAreIndependent;
var
  FirstProject, SecondProject: string;
begin
  FirstProject := TempPath('first' + PathDelim + 'same-name.bib');
  SecondProject := TempPath('second' + PathDelim + 'same-name.bib');
  Remember(FirstProject, edTaggedTheWord, TempPath('first.ont'));
  Remember(SecondProject, edTaggedTheWord, TempPath('second.ont'));
  AssertEquals(TempPath('first.ont'), ReadExportDestination(FSettings,
    FirstProject, edTaggedTheWord, 'default.ont'));
  AssertEquals(TempPath('second.ont'), ReadExportDestination(FSettings,
    SecondProject, edTaggedTheWord, 'default.ont'));
  AssertEquals('A copied or renamed project starts with independent history',
    'default.ont', ReadExportDestination(FSettings,
      TempPath('renamed.bib'), edTaggedTheWord, 'default.ont'));
  AssertEquals(2, SectionCount);
end;

procedure TExportDestinationHistoryTests.UnicodeAndBracketsRoundTripThroughSafeIniSection;
var
  Reopened: TMemIniFile;
  Section, Destination: string;
  I: Integer;
begin
  Destination := TempPath('Bíblia [final] λόγος.ont');
  Remember(FProjectFileName, edTaggedTheWord, Destination);
  Section := OnlySection;
  AssertEquals('export.project.v1.', Copy(Section, 1, Length('export.project.v1.')));
  for I := Length('export.project.v1.') + 1 to Length(Section) do
    AssertTrue('The project path encoding must not introduce INI delimiters',
      Section[I] in ['0'..'9', 'a'..'f']);
  Reopened := TMemIniFile.Create(FIniFileName);
  try
    AssertEquals(Destination, ReadExportDestination(Reopened,
      FProjectFileName, edTaggedTheWord, 'default.ont'));
  finally
    Reopened.Free;
  end;
end;

procedure TExportDestinationHistoryTests.RelativeAndDottedProjectPathsUseTheSameHistory;
var
  PreviousDirectory: string;
begin
  PreviousDirectory := GetCurrentDir;
  try
    AssertTrue(SetCurrentDir(FTempDir));
    Remember('.' + PathDelim + 'Projeto [Á].bib', edTaggedTheWord,
      TempPath('remembered.ont'));
    AssertEquals(TempPath('remembered.ont'), ReadExportDestination(FSettings,
      FProjectFileName, edTaggedTheWord, 'default.ont'));
    AssertEquals(TempPath('remembered.ont'), ReadExportDestination(FSettings,
      'other' + PathDelim + '..' + PathDelim + 'Projeto [Á].bib',
      edTaggedTheWord, 'default.ont'));
    AssertEquals(1, SectionCount);
  finally
    SetCurrentDir(PreviousDirectory);
  end;
end;

{$IFDEF WINDOWS}
procedure TExportDestinationHistoryTests.WindowsCaseAndPathSeparatorsUseTheSameHistory;
var
  EquivalentPath: string;
begin
  Remember(FProjectFileName, edInterlinearTheWord, TempPath('interlinear.ont'));
  EquivalentPath := UTF8UpperCase(StringReplace(FProjectFileName,
    '\', '/', [rfReplaceAll]));
  AssertEquals(TempPath('interlinear.ont'), ReadExportDestination(FSettings,
    EquivalentPath, edInterlinearTheWord, 'default.ont'));
  Remember(EquivalentPath, edInterlinearTheWord, TempPath('updated.ont'));
  AssertEquals(TempPath('updated.ont'), ReadExportDestination(FSettings,
    FProjectFileName, edInterlinearTheWord, 'default.ont'));
  AssertEquals(1, SectionCount);
end;

procedure TExportDestinationHistoryTests.WindowsExtendedPathsCanBeRemembered;
var
  ExtendedProject, ExtendedDestination, Expected: string;
begin
  ExtendedProject := '\\?\' + FProjectFileName;
  ExtendedDestination := '\\?\' + TempPath('extended.ont');
  Expected := CleanAndExpandFilename(ExtendedDestination);
  Remember(ExtendedProject, edTaggedTheWord, ExtendedDestination);
  AssertEquals(Expected, FSettings.ReadString(OnlySection,
    'last.tagged.destination.file', ''));
  AssertEquals(Expected, ReadExportDestination(FSettings, ExtendedProject,
    edTaggedTheWord, 'default.ont'));
end;

procedure TExportDestinationHistoryTests.WindowsDottedIAndPlainIProjectsStayIndependent;
var
  DottedProject, PlainProject: string;
begin
  DottedProject := TempPath('Projeto İ.bib');
  PlainProject := TempPath('Projeto i.bib');
  Remember(DottedProject, edTaggedTheWord, TempPath('dotted.ont'));
  Remember(PlainProject, edTaggedTheWord, TempPath('plain.ont'));
  AssertEquals('Ordinal Windows comparison must preserve distinct file names',
    2, SectionCount);
  AssertEquals(TempPath('dotted.ont'), ReadExportDestination(FSettings,
    DottedProject, edTaggedTheWord, 'default.ont'));
  AssertEquals(TempPath('plain.ont'), ReadExportDestination(FSettings,
    PlainProject, edTaggedTheWord, 'default.ont'));
end;

procedure TExportDestinationHistoryTests.WindowsAccentedCaseReusesExistingSection;
var
  UpperProject, LowerProject, OriginalSection: string;
begin
  UpperProject := TempPath('Projeto Á.bib');
  LowerProject := TempPath('projeto á.bib');
  Remember(UpperProject, edTaggedTheWord, TempPath('first.ont'));
  OriginalSection := OnlySection;
  Remember(LowerProject, edTaggedTheWord, TempPath('updated.ont'));
  AssertEquals('An equivalent name must reuse the section already on disk',
    OriginalSection, OnlySection);
  AssertEquals(TempPath('updated.ont'), ReadExportDestination(FSettings,
    UpperProject, edTaggedTheWord, 'default.ont'));
  AssertEquals(TempPath('updated.ont'), ReadExportDestination(FSettings,
    LowerProject, edTaggedTheWord, 'default.ont'));
end;
{$ENDIF}

procedure TExportDestinationHistoryTests.RelativeDestinationIsSavedAsAbsoluteBeforeIniCloses;
var
  PreviousDirectory, Expected: string;
  Reopened: TMemIniFile;
begin
  Expected := TempPath('selected.ont');
  PreviousDirectory := GetCurrentDir;
  try
    AssertTrue(SetCurrentDir(FTempDir));
    Remember(FProjectFileName, edTaggedTheWord, 'selected.ont');
  finally
    SetCurrentDir(PreviousDirectory);
  end;
  AssertEquals('The helper must flush cached settings before returning',
    1, FSettings.UpdateCount);
  Reopened := TMemIniFile.Create(FIniFileName);
  try
    AssertEquals(Expected, ReadExportDestination(Reopened, FProjectFileName,
      edTaggedTheWord, 'default.ont'));
  finally
    Reopened.Free;
  end;
end;

procedure TExportDestinationHistoryTests.MissingDestinationFileIsStillRemembered;
var
  Destination: string;
begin
  Destination := TempPath('not-created-yet.ont');
  AssertFalse(FileExists(Destination));
  Remember(FProjectFileName, edTaggedTheWord, Destination);
  AssertEquals(Destination, ReadExportDestination(FSettings, FProjectFileName,
    edTaggedTheWord, 'default.ont'));
  AssertFalse('Remembering a path must not create the exported module', FileExists(Destination));
end;

procedure TExportDestinationHistoryTests.MissingFolderUsesDefaultAndRecoversWhenFolderReturns;
var
  Directory, Destination, Section: string;
  BeforeWrites, BeforeUpdates: Integer;
begin
  Directory := TempPath('removable-output');
  Destination := IncludeTrailingPathDelimiter(Directory) + 'last.ont';
  AssertTrue(ForceDirectories(Directory));
  Remember(FProjectFileName, edTaggedTheWord, Destination);
  Section := OnlySection;
  AssertTrue(RemoveDir(Directory));
  BeforeWrites := FSettings.WriteCount;
  BeforeUpdates := FSettings.UpdateCount;
  AssertEquals('default.ont', ReadExportDestination(FSettings,
    FProjectFileName, edTaggedTheWord, 'default.ont'));
  AssertEquals(Destination,
    FSettings.ReadString(Section, 'last.tagged.destination.file', ''));
  AssertEquals(BeforeWrites, FSettings.WriteCount);
  AssertEquals(BeforeUpdates, FSettings.UpdateCount);
  AssertTrue(ForceDirectories(Directory));
  AssertEquals(Destination, ReadExportDestination(FSettings,
    FProjectFileName, edTaggedTheWord, 'default.ont'));
end;

procedure TExportDestinationHistoryTests.MySwordDoesNotReplaceEitherTextDestination;
var
  Kind: TExportDestinationKind;
  BeforeWrites, BeforeUpdates: Integer;
  Expected: string;
begin
  for Kind := edTaggedTheWord to edInterlinearTheWord do
  begin
    Expected := TempPath(IntToStr(Ord(Kind)) + '.ont');
    Remember(FProjectFileName, Kind, Expected);
    BeforeWrites := FSettings.WriteCount;
    BeforeUpdates := FSettings.UpdateCount;
    Remember(FProjectFileName, Kind, TempPath('unrelated.bbl.MyBiBlE'));
    AssertEquals(Expected, ReadExportDestination(FSettings,
      FProjectFileName, Kind, 'default.ont'));
    AssertEquals('MySword must not rewrite the remembered theWord destination',
      BeforeWrites, FSettings.WriteCount);
    AssertEquals(BeforeUpdates, FSettings.UpdateCount);
  end;
end;

procedure TExportDestinationHistoryTests.StoredMySwordValuesAreIgnoredWithoutErasingThem;
const
  Keys: array[edTaggedTheWord..edInterlinearTheWord] of string =
    ('last.tagged.destination.file', 'last.interlinear.source.file');
var
  Kind: TExportDestinationKind;
  Section, Stored: string;
  BeforeWrites: Integer;
begin
  Remember(FProjectFileName, edTaggedTheWord, TempPath('theword.ont'));
  Section := OnlySection;
  Stored := TempPath('old.bbl.MYBIBLE');
  for Kind := edTaggedTheWord to edInterlinearTheWord do
  begin
    FSettings.WriteString(Section, Keys[Kind], Stored);
    BeforeWrites := FSettings.WriteCount;
    AssertEquals('default.ont', ReadExportDestination(FSettings,
      FProjectFileName, Kind, 'default.ont'));
    AssertEquals(Stored, FSettings.ReadString(Section, Keys[Kind], ''));
    AssertEquals(BeforeWrites, FSettings.WriteCount);
  end;
end;

procedure TExportDestinationHistoryTests.MySwordNoOpDoesNotRequireAvailableSettings;
var
  ErrorMessage: string;
begin
  AssertTrue(TryRememberExportDestination(nil, '', edTaggedTheWord,
    TempPath('module.bbl.mybible'), ErrorMessage));
  AssertEquals('', ErrorMessage);
end;

procedure TExportDestinationHistoryTests.NilAndInvalidInputsDoNotCreateGlobalHistory;
var
  ErrorMessage: string;

  procedure Reject(const ProjectName, Destination: string);
  begin
    AssertFalse(TryRememberExportDestination(FSettings, ProjectName,
      edTaggedTheWord, Destination, ErrorMessage));
    AssertTrue(ErrorMessage <> '');
  end;

begin
  AssertEquals('default.ont', ReadExportDestination(nil, FProjectFileName,
    edTaggedTheWord, 'default.ont'));
  AssertFalse(TryRememberExportDestination(nil, FProjectFileName,
    edTaggedTheWord, TempPath('selected.ont'), ErrorMessage));
  AssertTrue(ErrorMessage <> '');
  Reject('', TempPath('selected.ont'));
  Reject('   ', TempPath('selected.ont'));
  Reject(FTempDir, TempPath('selected.ont'));
  Reject(FProjectFileName, '');
  Reject(FProjectFileName, FTempDir);
  Reject(FProjectFileName, 'bad' + #0 + '.ont');
  Reject(FProjectFileName, 'bad' + LineEnding + '[section].ont');
  Reject('bad' + #0 + '.bib', TempPath('selected.ont'));
  AssertEquals('default.ont', ReadExportDestination(FSettings, '',
    edTaggedTheWord, 'default.ont'));
  AssertEquals(0, SectionCount);
  AssertEquals(0, FSettings.WriteCount);
  AssertEquals(0, FSettings.UpdateCount);
end;

procedure TExportDestinationHistoryTests.WriteFailureReturnsMessageAndKeepsPreviousDestination;
var
  ErrorMessage: string;
  BeforeUpdates: Integer;
begin
  Remember(FProjectFileName, edTaggedTheWord, TempPath('previous.ont'));
  BeforeUpdates := FSettings.UpdateCount;
  FSettings.FailWrite := True;
  AssertFalse(TryRememberExportDestination(FSettings, FProjectFileName,
    edTaggedTheWord, TempPath('new.ont'), ErrorMessage));
  AssertTrue(Pos('Injected history write failure', ErrorMessage) > 0);
  AssertEquals(BeforeUpdates, FSettings.UpdateCount);
  AssertEquals(TempPath('previous.ont'), ReadExportDestination(FSettings,
    FProjectFileName, edTaggedTheWord, 'default.ont'));
end;

procedure TExportDestinationHistoryTests.UpdateFailureReturnsMessageAndCanRetry;
var
  ErrorMessage: string;
  Reopened: TMemIniFile;
begin
  Remember(FProjectFileName, edTaggedTheWord, TempPath('previous.ont'));
  FSettings.FailUpdate := True;
  AssertFalse(TryRememberExportDestination(FSettings, FProjectFileName,
    edTaggedTheWord, TempPath('new.ont'), ErrorMessage));
  AssertTrue(Pos('Injected history flush failure', ErrorMessage) > 0);
  Reopened := TMemIniFile.Create(FIniFileName);
  try
    AssertEquals('A failed flush is not reported as durable persistence',
      TempPath('previous.ont'), ReadExportDestination(Reopened,
        FProjectFileName, edTaggedTheWord, 'default.ont'));
  finally
    Reopened.Free;
  end;
  FSettings.FailUpdate := False;
  Remember(FProjectFileName, edTaggedTheWord, TempPath('new.ont'));
  Reopened := TMemIniFile.Create(FIniFileName);
  try
    AssertEquals(TempPath('new.ont'), ReadExportDestination(Reopened,
      FProjectFileName, edTaggedTheWord, 'default.ont'));
  finally
    Reopened.Free;
  end;
end;

procedure TExportDestinationHistoryTests.ReadFailureUsesDefaultWithoutWriting;
begin
  FSettings.FailRead := True;
  AssertEquals('default.ont', ReadExportDestination(FSettings,
    FProjectFileName, edTaggedTheWord, 'default.ont'));
  AssertEquals(0, FSettings.WriteCount);
  AssertEquals(0, FSettings.UpdateCount);
end;

procedure TExportDestinationHistoryTests.MalformedProjectSectionsAreIgnored;
begin
  FSettings.WriteString('export.project.v1.zz',
    'last.tagged.destination.file', TempPath('bad-hex.ont'));
  FSettings.WriteString('export.project.v1.6',
    'last.tagged.destination.file', TempPath('odd-hex.ont'));
  FSettings.WriteString('export.project.v1.00',
    'last.tagged.destination.file', TempPath('nul.ont'));
  FSettings.WriteString('export.project.v1.ff',
    'last.tagged.destination.file', TempPath('invalid-utf8.ont'));
  AssertEquals('default.ont', ReadExportDestination(FSettings,
    FProjectFileName, edTaggedTheWord, 'default.ont'));
  AssertEquals('Reading must not rewrite malformed sections', 4, SectionCount);
  Remember(FProjectFileName, edTaggedTheWord, TempPath('valid.ont'));
  AssertEquals(5, SectionCount);
  AssertEquals(TempPath('valid.ont'), ReadExportDestination(FSettings,
    FProjectFileName, edTaggedTheWord, 'default.ont'));
end;

procedure TExportDestinationHistoryTests.ImmediateIniWritesOnlyOnceBeforeReturning;
var
  Immediate: TCountingImmediateHistoryIni;
  Reopened: TMemIniFile;
  IniName, ErrorMessage: string;
begin
  IniName := TempPath('immediate.ini');
  Immediate := TCountingImmediateHistoryIni.Create(IniName);
  try
    AssertFalse('Fixture must use immediate writes, as the real opts does',
      Immediate.CacheUpdates);
    Immediate.UpdateCount := 0;
    AssertTrue(TryRememberExportDestination(Immediate, FProjectFileName,
      edTaggedTheWord, TempPath('selected.ont'), ErrorMessage));
    AssertEquals('', ErrorMessage);
    AssertEquals('WriteString already flushes a non-cached TIniFile',
      1, Immediate.UpdateCount);
    Reopened := TMemIniFile.Create(IniName);
    try
      AssertEquals('A single write must still be durable before helper return',
        TempPath('selected.ont'), ReadExportDestination(Reopened,
          FProjectFileName, edTaggedTheWord, 'default.ont'));
    finally
      Reopened.Free;
    end;
  finally
    Immediate.Free;
  end;
end;

initialization
  RegisterTest(TExportDestinationHistoryTests);

end.
