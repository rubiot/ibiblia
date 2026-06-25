unit project_setup_model;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BibleScope, Project, status_catalog, ONTTokenizer;

const
  PROJECT_SCENARIO_CONFIG_KEY = 'project.scenario';
  PROJECT_SCENARIO_ASSOCIATION = 'association';
  PROJECT_SCENARIO_TRANSLATION = 'translation';

type
  TProjectScenario = (psUnknown, psAssociationProject, psTranslationProject);

  TProjectTextSetup = record
    ModulePath: string;
    StrongDictionaryPath: string;
    MorphologyDictionaryPath: string;
    Description: string;
  end;

  TProjectTextSetupArray = array[tbOrigem..tbConsulta2] of TProjectTextSetup;

  TProjectSetupDraft = record
    Scenario: TProjectScenario;
    ProjectName: string;
    ProjectFileName: string;
    Scope: TEscopoTexto;
    TokenizerVersion: Integer;
    HasCustomStatusCatalog: Boolean;
    StatusDefinitions: TStatusDefinitions;
    Texts: TProjectTextSetupArray;
  end;

function DefaultProjectSetupDraft(const ProjectName: string): TProjectSetupDraft;
function ProjectScenarioToConfig(Scenario: TProjectScenario): string;
function ProjectScenarioFromConfig(const Value: string): TProjectScenario;
function ProjectScenarioCaption(Scenario: TProjectScenario): string;
function ProjectScenarioRequiresDestination(Scenario: TProjectScenario): Boolean;
function TextSetupConfigKey(const Prefix: string; TextType: TTipoTextoBiblico): string;

implementation

resourcestring
  SScenarioUnknown = 'Unknown';
  SScenarioAssociation = 'Translation association project';
  SScenarioTranslation = 'Translation project';

function DefaultProjectSetupDraft(const ProjectName: string): TProjectSetupDraft;
var
  TextType: TTipoTextoBiblico;
begin
  Result.Scenario := psAssociationProject;
  Result.ProjectName := ProjectName;
  Result.ProjectFileName := '';
  Result.Scope := etNT;
  Result.TokenizerVersion := TTokenizerFactory.LATEST_VERSION;
  Result.HasCustomStatusCatalog := False;
  SetLength(Result.StatusDefinitions, 0);
  for TextType := tbOrigem to tbConsulta2 do
  begin
    Result.Texts[TextType].ModulePath := '';
    Result.Texts[TextType].StrongDictionaryPath := '';
    Result.Texts[TextType].MorphologyDictionaryPath := '';
    Result.Texts[TextType].Description := '';
  end;
end;

function ProjectScenarioToConfig(Scenario: TProjectScenario): string;
begin
  case Scenario of
    psAssociationProject:
      Result := PROJECT_SCENARIO_ASSOCIATION;
    psTranslationProject:
      Result := PROJECT_SCENARIO_TRANSLATION;
  else
    Result := '';
  end;
end;

function ProjectScenarioFromConfig(const Value: string): TProjectScenario;
begin
  if SameText(Trim(Value), PROJECT_SCENARIO_ASSOCIATION) then
    Result := psAssociationProject
  else if SameText(Trim(Value), PROJECT_SCENARIO_TRANSLATION) then
    Result := psTranslationProject
  else
    Result := psUnknown;
end;

function ProjectScenarioCaption(Scenario: TProjectScenario): string;
begin
  case Scenario of
    psAssociationProject:
      Result := SScenarioAssociation;
    psTranslationProject:
      Result := SScenarioTranslation;
  else
    Result := SScenarioUnknown;
  end;
end;

function ProjectScenarioRequiresDestination(Scenario: TProjectScenario): Boolean;
begin
  Result := Scenario in [psUnknown, psAssociationProject];
end;

function TextSetupConfigKey(const Prefix: string; TextType: TTipoTextoBiblico): string;
begin
  Result := Format('%s%d', [Prefix, Ord(TextType)]);
end;

end.
