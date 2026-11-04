unit project_setup_validation_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, BibleScope, Project, ONTTokenizer,
  status_catalog, project_setup_model, project_setup_validation;

type
  TProjectSetupValidationTests = class(TTestCase)
  private
    FTempDir: string;
    FSourceModule: string;
    FDestinationModule: string;
    function CreateModule(const FileName: string; Scope: TEscopoTexto): string;
    function CreateDraft(Scenario: TProjectScenario): TProjectSetupDraft;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AssociationRequiresSourceAndDestination;
    procedure TranslationRequiresOnlySource;
    procedure TranslationAllowsEmptyDestination;
    procedure ScopeMismatchByExtensionIsInvalid;
    procedure InvalidStatusCatalogBlocksDraft;
    procedure MissingDictionaryBlocksDraft;
  end;

implementation

function TProjectSetupValidationTests.CreateModule(const FileName: string;
  Scope: TEscopoTexto): string;
var
  Lines: TStringList;
  I: Integer;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + FileName;
  Lines := TStringList.Create;
  try
    for I := 1 to QLinhas[Scope] do
      Lines.Add('In the beginning <WG1>.');
    Lines.SaveToFile(Result);
  finally
    Lines.Free;
  end;
end;

function TProjectSetupValidationTests.CreateDraft(
  Scenario: TProjectScenario): TProjectSetupDraft;
begin
  Result := DefaultProjectSetupDraft('Validation test');
  Result.Scenario := Scenario;
  Result.ProjectFileName := IncludeTrailingPathDelimiter(FTempDir) +
    Format('project-%d.bib', [Ord(Scenario)]);
  Result.Scope := etNT;
  Result.TokenizerVersion := TTokenizerFactory.LATEST_VERSION;
  Result.StatusDefinitions := BuildLegacyStatusDefinitions([
    'Not associated',
    'Associating',
    'Needs review',
    'Associated'
  ]);
  Result.Texts[tbOrigem].ModulePath := FSourceModule;
end;

procedure TProjectSetupValidationTests.SetUp;
begin
  FTempDir := IncludeTrailingPathDelimiter(GetCurrentDir) +
    'test_project_setup_validation';
  ForceDirectories(FTempDir);
  FSourceModule := CreateModule('source.nt', etNT);
  FDestinationModule := CreateModule('destination.nt', etNT);
end;

procedure TProjectSetupValidationTests.TearDown;
begin
  DeleteFile(FSourceModule);
  DeleteFile(FDestinationModule);
  DeleteFile(IncludeTrailingPathDelimiter(FTempDir) +
    'project-0.bib');
  DeleteFile(IncludeTrailingPathDelimiter(FTempDir) +
    'project-1.bib');
  DeleteFile(IncludeTrailingPathDelimiter(FTempDir) +
    'project-2.bib');
  RemoveDir(FTempDir);
end;

procedure TProjectSetupValidationTests.AssociationRequiresSourceAndDestination;
var
  Draft: TProjectSetupDraft;
  MessageText: string;
begin
  Draft := CreateDraft(psAssociationProject);

  AssertFalse(ProjectSetupValidateDraft(Draft, MessageText));
  AssertTrue(Pos('destination', LowerCase(MessageText)) > 0);
end;

procedure TProjectSetupValidationTests.TranslationRequiresOnlySource;
var
  Draft: TProjectSetupDraft;
  MessageText: string;
begin
  Draft := CreateDraft(psTranslationProject);

  AssertTrue(ProjectSetupValidateDraft(Draft, MessageText));
end;

procedure TProjectSetupValidationTests.TranslationAllowsEmptyDestination;
var
  Draft: TProjectSetupDraft;
  MessageText: string;
begin
  Draft := CreateDraft(psTranslationProject);
  Draft.Texts[tbDestino].ModulePath := '';

  AssertTrue(ProjectSetupValidateDraft(Draft, MessageText));
end;

procedure TProjectSetupValidationTests.ScopeMismatchByExtensionIsInvalid;
var
  Validation: TProjectSetupValidationResult;
begin
  AssertFalse(ProjectSetupValidateModule(FSourceModule, etOT,
    TTokenizerFactory.LATEST_VERSION, True, Validation));
  AssertTrue(Validation.Severity = psvsError);
end;

procedure TProjectSetupValidationTests.InvalidStatusCatalogBlocksDraft;
var
  Draft: TProjectSetupDraft;
  MessageText: string;
begin
  Draft := CreateDraft(psTranslationProject);
  SetLength(Draft.StatusDefinitions, 1);
  Draft.StatusDefinitions[0].Id := 0;
  Draft.StatusDefinitions[0].Caption := '';
  Draft.StatusDefinitions[0].DisplayOrder := 0;

  AssertFalse(ProjectSetupValidateDraft(Draft, MessageText));
  AssertTrue(Pos('status', LowerCase(MessageText)) > 0);
end;

procedure TProjectSetupValidationTests.MissingDictionaryBlocksDraft;
var
  Draft: TProjectSetupDraft;
  MessageText: string;
begin
  Draft := CreateDraft(psTranslationProject);
  Draft.Texts[tbOrigem].StrongDictionaryPath :=
    IncludeTrailingPathDelimiter(FTempDir) + 'missing.dct.twm';

  AssertFalse(ProjectSetupValidateDraft(Draft, MessageText));
  AssertTrue(Pos('dictionary', LowerCase(MessageText)) > 0);
end;

initialization
  RegisterTest(TProjectSetupValidationTests);
end.
