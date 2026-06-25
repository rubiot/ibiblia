unit project_setup_validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BibleScope, Project, ONTTokenizer, status_catalog,
  dictionary_manager, project_setup_model;

type
  TProjectSetupValidationSeverity = (psvsOk, psvsWarning, psvsError);

  TProjectSetupValidationResult = record
    Severity: TProjectSetupValidationSeverity;
    MessageText: string;
    PreviewText: string;
    ModuleScope: TEscopoTexto;
    VerseCount: Integer;
  end;

function ProjectSetupScopeCaption(Scope: TEscopoTexto): string;
function ProjectSetupTextCaption(TextType: TTipoTextoBiblico): string;
function ProjectSetupModuleScopeFromFileName(const FileName: string;
  out Scope: TEscopoTexto): Boolean;
function ProjectSetupModuleMatchesScope(ModuleScope, ProjectScope: TEscopoTexto): Boolean;
function ProjectSetupValidateModule(const FileName: string; ProjectScope: TEscopoTexto;
  TokenizerVersion: Integer; Required: Boolean;
  out Validation: TProjectSetupValidationResult): Boolean;
function ProjectSetupValidateDictionary(const FileName: string;
  ModuleType: TDictionaryModuleType; out MessageText: string): Boolean;
function ProjectSetupValidateDraft(const Draft: TProjectSetupDraft;
  out MessageText: string): Boolean;

implementation

resourcestring
  SScopeOldTestament = 'Old Testament';
  SScopeNewTestament = 'New Testament';
  SScopeWholeBible = 'Whole Bible';
  SScopeUnknown = 'Unknown scope';
  STextSource = 'Source';
  STextDestination = 'Destination';
  STextReference1 = 'Reference 1';
  STextReference2 = 'Reference 2';
  SProjectNameRequired = 'Please choose a name for the project.';
  SProjectFileRequired = 'Please choose where the .bib project file will be saved.';
  SProjectFileExtension = 'The project file must use the .bib extension.';
  SProjectFileExists = 'A project file already exists at this location.';
  SSourceRequired = 'Please choose the source Bible text.';
  SDestinationRequired = 'Please choose the destination Bible text for an association project.';
  SStatusCatalogInvalid = 'Status catalog is invalid: %s';
  SModuleOptional = 'Optional.';
  SModuleRequired = 'Required module was not selected.';
  SModuleMissing = 'The selected Bible module does not exist.';
  SModuleTypeInvalid = 'Only theWord Bible modules (.ont, .nt, .ot) are supported.';
  SModuleScopeMismatch = 'The selected %s module cannot be used in a %s project.';
  SModuleLineCountInvalid = 'The selected module has %d lines, but %d are required.';
  SModulePreviewEmpty = 'Module looks valid, but the first verse preview is empty.';
  SModuleReady = 'Ready: %s, %d verses.';
  SDictionaryOptional = 'Optional.';
  SDictionaryMissing = 'The selected dictionary does not exist.';
  SDictionaryStrongMissing = 'This dictionary does not advertise Strong support.';
  SDictionaryMorphMissing = 'This dictionary does not advertise morphology support.';
  SDictionaryEncrypted = 'Encrypted dictionaries are not supported.';
  SDictionaryCompressed = 'Compressed dictionaries are not supported.';
  SDictionaryNonRTF = 'Only RTF dictionary content is supported.';
  SDictionaryInvalid = 'The selected dictionary could not be validated.';
  SDictionaryReady = 'Dictionary ready.';

procedure SetValidation(out Validation: TProjectSetupValidationResult;
  Severity: TProjectSetupValidationSeverity; const MessageText: string;
  ModuleScope: TEscopoTexto = etNone; VerseCount: Integer = 0;
  const PreviewText: string = '');
begin
  Validation.Severity := Severity;
  Validation.MessageText := MessageText;
  Validation.ModuleScope := ModuleScope;
  Validation.VerseCount := VerseCount;
  Validation.PreviewText := PreviewText;
end;

function ProjectSetupScopeCaption(Scope: TEscopoTexto): string;
begin
  case Scope of
    etOT: Result := SScopeOldTestament;
    etNT: Result := SScopeNewTestament;
    etONT: Result := SScopeWholeBible;
  else
    Result := SScopeUnknown;
  end;
end;

function ProjectSetupTextCaption(TextType: TTipoTextoBiblico): string;
begin
  case TextType of
    tbOrigem: Result := STextSource;
    tbDestino: Result := STextDestination;
    tbConsulta1: Result := STextReference1;
    tbConsulta2: Result := STextReference2;
  else
    Result := '';
  end;
end;

function ProjectSetupModuleScopeFromFileName(const FileName: string;
  out Scope: TEscopoTexto): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := True;
  if Ext = '.ont' then
    Scope := etONT
  else if Ext = '.nt' then
    Scope := etNT
  else if Ext = '.ot' then
    Scope := etOT
  else
  begin
    Scope := etNone;
    Result := False;
  end;
end;

function ProjectSetupModuleMatchesScope(ModuleScope, ProjectScope: TEscopoTexto): Boolean;
begin
  case ModuleScope of
    etONT:
      Result := ProjectScope in [etOT, etNT, etONT];
    etNT:
      Result := ProjectScope in [etNT, etONT];
    etOT:
      Result := ProjectScope in [etOT, etONT];
  else
    Result := False;
  end;
end;

function ProjectStartLine(ModuleScope, ProjectScope: TEscopoTexto): Integer;
begin
  Result := 0;
  if (ModuleScope = etONT) and (ProjectScope = etNT) then
    Result := QLinhas[etOT];
end;

function PlainPreview(const XML: string; TokenizerVersion: Integer): string;
var
  Tokenizer: TONTTokenizer;
  Token: TTagSintagma;
begin
  Result := '';
  Tokenizer := TTokenizerFactory.CreateTokenizer(TokenizerVersion, XML);
  try
    while Tokenizer.LerSintagma(Token) <> tsNulo do
      if Token.tipo in [tsEspaco, tsSintagma, tsPontuacao] then
        Result := Result + Token.valor;
  finally
    Tokenizer.Free;
  end;
end;

function ProjectSetupValidateModule(const FileName: string; ProjectScope: TEscopoTexto;
  TokenizerVersion: Integer; Required: Boolean;
  out Validation: TProjectSetupValidationResult): Boolean;
var
  ModuleScope: TEscopoTexto;
  ModuleLines: TStringList;
  FirstLine: Integer;
  Preview: string;
begin
  Result := False;
  if Trim(FileName) = '' then
  begin
    if Required then
      SetValidation(Validation, psvsError, SModuleRequired)
    else
    begin
      SetValidation(Validation, psvsOk, SModuleOptional);
      Result := True;
    end;
    Exit;
  end;

  if not FileExists(FileName) then
  begin
    SetValidation(Validation, psvsError, SModuleMissing);
    Exit;
  end;

  if not ProjectSetupModuleScopeFromFileName(FileName, ModuleScope) then
  begin
    SetValidation(Validation, psvsError, SModuleTypeInvalid);
    Exit;
  end;

  if not ProjectSetupModuleMatchesScope(ModuleScope, ProjectScope) then
  begin
    SetValidation(Validation, psvsError, Format(SModuleScopeMismatch,
      [ProjectSetupScopeCaption(ModuleScope), ProjectSetupScopeCaption(ProjectScope)]),
      ModuleScope);
    Exit;
  end;

  ModuleLines := TStringList.Create;
  try
    try
      ModuleLines.LoadFromFile(FileName);
    except
      on E: Exception do
      begin
        SetValidation(Validation, psvsError, E.Message, ModuleScope);
        Exit;
      end;
    end;

    if ModuleLines.Count < QLinhas[ModuleScope] then
    begin
      SetValidation(Validation, psvsError, Format(SModuleLineCountInvalid,
        [ModuleLines.Count, QLinhas[ModuleScope]]), ModuleScope, ModuleLines.Count);
      Exit;
    end;

    FirstLine := ProjectStartLine(ModuleScope, ProjectScope);
    if FirstLine >= ModuleLines.Count then
    begin
      SetValidation(Validation, psvsError, Format(SModuleLineCountInvalid,
        [ModuleLines.Count, FirstLine + 1]), ModuleScope, ModuleLines.Count);
      Exit;
    end;

    Preview := PlainPreview(ModuleLines[FirstLine], TokenizerVersion);
    if Trim(Preview) = '' then
      SetValidation(Validation, psvsWarning, SModulePreviewEmpty,
        ModuleScope, QLinhas[ModuleScope], Preview)
    else
      SetValidation(Validation, psvsOk, Format(SModuleReady,
        [ProjectSetupScopeCaption(ModuleScope), QLinhas[ModuleScope]]),
        ModuleScope, QLinhas[ModuleScope], Preview);
    Result := True;
  finally
    ModuleLines.Free;
  end;
end;

function ProjectSetupValidateDictionary(const FileName: string;
  ModuleType: TDictionaryModuleType; out MessageText: string): Boolean;
var
  ValidationResult: TDictionaryValidationResult;
begin
  Result := False;
  MessageText := '';

  if Trim(FileName) = '' then
  begin
    MessageText := SDictionaryOptional;
    Exit(True);
  end;

  if not FileExists(FileName) then
  begin
    MessageText := SDictionaryMissing;
    Exit;
  end;

  try
    ValidationResult := TDictionaryManager.ValidateDictionaryModule(FileName, ModuleType);
  except
    MessageText := SDictionaryInvalid;
    Exit;
  end;

  case ValidationResult of
    dvrValid:
      begin
        MessageText := SDictionaryReady;
        Result := True;
      end;
    dvrMissingStrongSupport:
      MessageText := SDictionaryStrongMissing;
    dvrMissingMorphologySupport:
      MessageText := SDictionaryMorphMissing;
    dvrEncrypted:
      MessageText := SDictionaryEncrypted;
    dvrCompressed:
      MessageText := SDictionaryCompressed;
    dvrNonRTF:
      MessageText := SDictionaryNonRTF;
  else
    MessageText := SDictionaryInvalid;
  end;
end;

function ProjectSetupValidateDraft(const Draft: TProjectSetupDraft;
  out MessageText: string): Boolean;
var
  StatusError: string;
  TextType: TTipoTextoBiblico;
  ModuleValidation: TProjectSetupValidationResult;
  Required: Boolean;
  DictionaryMessage: string;
begin
  Result := False;
  MessageText := '';

  if Trim(Draft.ProjectName) = '' then
  begin
    MessageText := SProjectNameRequired;
    Exit;
  end;

  if Trim(Draft.ProjectFileName) = '' then
  begin
    MessageText := SProjectFileRequired;
    Exit;
  end;

  if not SameText(ExtractFileExt(Draft.ProjectFileName), '.bib') then
  begin
    MessageText := SProjectFileExtension;
    Exit;
  end;

  if FileExists(Draft.ProjectFileName) then
  begin
    MessageText := SProjectFileExists;
    Exit;
  end;

  if Trim(Draft.Texts[tbOrigem].ModulePath) = '' then
  begin
    MessageText := SSourceRequired;
    Exit;
  end;

  if ProjectScenarioRequiresDestination(Draft.Scenario) and
    (Trim(Draft.Texts[tbDestino].ModulePath) = '') then
  begin
    MessageText := SDestinationRequired;
    Exit;
  end;

  if not ValidateStatusDefinitions(Draft.StatusDefinitions, StatusError) then
  begin
    MessageText := Format(SStatusCatalogInvalid, [StatusError]);
    Exit;
  end;

  for TextType := tbOrigem to tbConsulta2 do
  begin
    Required := TextType = tbOrigem;
    if TextType = tbDestino then
      Required := ProjectScenarioRequiresDestination(Draft.Scenario);

    if not ProjectSetupValidateModule(Draft.Texts[TextType].ModulePath,
      Draft.Scope, Draft.TokenizerVersion, Required, ModuleValidation) then
    begin
      MessageText := Format('%s: %s',
        [ProjectSetupTextCaption(TextType), ModuleValidation.MessageText]);
      Exit;
    end;

    if not ProjectSetupValidateDictionary(
      Draft.Texts[TextType].StrongDictionaryPath, dmtStrong,
      DictionaryMessage) then
    begin
      MessageText := Format('%s Strong: %s',
        [ProjectSetupTextCaption(TextType), DictionaryMessage]);
      Exit;
    end;

    if not ProjectSetupValidateDictionary(
      Draft.Texts[TextType].MorphologyDictionaryPath, dmtMorphology,
      DictionaryMessage) then
    begin
      MessageText := Format('%s Morphology: %s',
        [ProjectSetupTextCaption(TextType), DictionaryMessage]);
      Exit;
    end;
  end;

  Result := True;
end;

end.
