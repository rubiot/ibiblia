unit llm_options_form_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, fpcunit, testregistry, Project, FormLlmOptions, ai_credential_store;

type
  TLLMOptionsFormTests = class(TTestCase)
  private
    FTempDir, FSettingsFile, FProjectFile: string;
    FStore: IAICredentialStore;
    FForm: TFrmLLMOptions;
    FHost: TForm;
    FSourcePanel, FDestinationPanel: TScrollBox;
    FStatusCombo: TComboBox;
    FProject: TProject;
    function TempPath(const Name: string): string;
    function ReadFileText(const FileName: string): string;
    procedure CreateOptionsForm;
    procedure CreateTemporaryProject;
    procedure SeedGlobalSettings;
    procedure CaptureIfRequested(const Name: string);
    function CountSecretEdits(Parent: TWinControl; const Secret: string): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure NoProjectShowsOnlyEditableMachineDefaults;
    procedure CancelDiscardsGlobalDraftWithoutCreatingFile;
    procedure OKSavesGlobalDraftWithoutAnOpenProject;
    procedure OpeningAndCancellingDoesNotChangeProjectMetadata;
    procedure OneProjectOverrideDoesNotCopyInheritedValues;
    procedure DetachingProjectClearsHiddenCredentialEdits;
    procedure MachineDefaultLabelsFollowSelectedLanguage;
    procedure ProjectInheritanceLabelsFollowSelectedLanguage;
    procedure ConnectionErrorsFollowSelectedLanguage;
  end;

implementation

uses
  LResources, LCLTranslator, BibleScope, project_database_bootstrap, sqlite3conn, sqldb,
  ai_global_settings, ai_settings_types, ai_effective_settings;

const
  SyntheticGlobalKey = 'synthetic-ui-global-key';
  SyntheticScopedKey = 'synthetic-ui-scoped-key';

function TLLMOptionsFormTests.TempPath(const Name: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + Name;
end;

function TLLMOptionsFormTests.ReadFileText(const FileName: string): string;
var Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TLLMOptionsFormTests.CreateOptionsForm;
var
  TextStream: TFileStream;
  BinaryStream: TMemoryStream;
  Reader: TReader;
  ResourceFile: string;
begin
  FreeAndNil(FForm);
  ResourceFile := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..' +
    PathDelim + 'formllmoptions.lfm');
  if not FileExists(ResourceFile) then
    ResourceFile := ExpandFileName('formllmoptions.lfm');
  AssertTrue('The real LLM options form resource must be available',
    FileExists(ResourceFile));
  RegisterClasses([TLabel, TLabeledEdit, TComboBox, TGroupBox, TCheckBox,
    TScrollBox, TPageControl, TTabSheet, TButton, TFrmLLMOptions]);
  FForm := TFrmLLMOptions.CreateNew(nil);
  TextStream := TFileStream.Create(ResourceFile, fmOpenRead or fmShareDenyNone);
  BinaryStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(TextStream, BinaryStream);
    BinaryStream.Position := 0;
    Reader := TReader.Create(BinaryStream, 4096);
    try
      Reader.ReadRootComponent(FForm);
    finally
      Reader.Free;
    end;
  finally
    BinaryStream.Free;
    TextStream.Free;
  end;
  // CreateNew has completed AfterConstruction before the resource is loaded.
  FForm.FormCreate(FForm);
  FForm.SetProject(nil, FSettingsFile, FStore);
end;

procedure TLLMOptionsFormTests.CreateTemporaryProject;
var Connection: TSQLite3Connection; Transaction: TSQLTransaction;
begin
  FProjectFile := TempPath('project.bib');
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(FProjectFile, etNT, 1);
  Connection := TSQLite3Connection.Create(nil);
  try
    Transaction := TSQLTransaction.Create(Connection);
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FProjectFile;
    Connection.Open;
    Connection.ExecuteDirect('update pares set pare_texto_origem = ''Source'', ' +
      'pare_texto_destino = ''Destination''');
    Transaction.Commit;
  finally
    Connection.Free;
  end;
  FHost := TForm.CreateNew(nil);
  FSourcePanel := TScrollBox.Create(FHost);
  FSourcePanel.Parent := FHost;
  FSourcePanel.SetBounds(0, 0, 800, 140);
  FDestinationPanel := TScrollBox.Create(FHost);
  FDestinationPanel.Parent := FHost;
  FDestinationPanel.SetBounds(0, 145, 800, 140);
  FStatusCombo := TComboBox.Create(FHost);
  FStatusCombo.Parent := FHost;
  FProject := TProject.Create([FSourcePanel, FDestinationPanel], nil,
    FStatusCombo, nil);
  FProject.Escopo := etNT;
  FProject.OpenProject(FProjectFile);
end;

procedure TLLMOptionsFormTests.SeedGlobalSettings;
var Settings: TAIGlobalSettings;
begin
  Settings := TAIGlobalSettings.Create(FSettingsFile, FStore);
  try
    Settings.SetValue(aisModel, 'gpt-4o-mini');
    Settings.SetValue(aisMaxParallel, '4');
    Settings.SetValue(aisMaxRetries, '5');
    Settings.SetValue(aisRetryDelay, '1250');
    Settings.SetValue(aisAutoSaveInterval, '12');
    Settings.StageCredential('OpenAI', SyntheticGlobalKey);
    Settings.Save;
  finally
    Settings.Free;
  end;
end;

procedure TLLMOptionsFormTests.CaptureIfRequested(const Name: string);
var Bitmap: TBitmap; Picture: TPortableNetworkGraphic;
begin
  if GetEnvironmentVariable('IBIBLIA_LLM_UI_CAPTURE') = '' then Exit;
  FForm.Show;
  FForm.Repaint;
  Application.ProcessMessages;
  Bitmap := FForm.GetFormImage;
  Picture := TPortableNetworkGraphic.Create;
  try
    Picture.Assign(Bitmap);
    Picture.SaveToFile(IncludeTrailingPathDelimiter(GetTempDir(False)) +
      'ibiblia-llm-' + Name + '.png');
  finally
    Picture.Free;
    Bitmap.Free;
    FForm.Hide;
  end;
end;

function TLLMOptionsFormTests.CountSecretEdits(Parent: TWinControl;
  const Secret: string): Integer;
var I: Integer; Child: TControl;
begin
  Result := 0;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Child := Parent.Controls[I];
    if (Child is TLabeledEdit) and (TLabeledEdit(Child).Text = Secret) then
      Inc(Result);
    if Child is TWinControl then
      Inc(Result, CountSecretEdits(TWinControl(Child), Secret));
  end;
end;

procedure TLLMOptionsFormTests.SetUp;
begin
  inherited SetUp;
  Forms.Application.Initialize;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia_llm_form_%x_%x', [PtrUInt(Self), GetTickCount64]);
  ForceDirectories(FTempDir);
  FSettingsFile := TempPath('llm.ini');
  FStore := TMemoryAICredentialStore.Create(True);
  CreateOptionsForm;
end;

procedure TLLMOptionsFormTests.TearDown;
var Search: TSearchRec;
begin
  FreeAndNil(FForm);
  if Assigned(FProject) then
  begin
    FProject.CloseProject(False);
    FreeAndNil(FProject);
  end;
  FreeAndNil(FHost);
  FStore := nil;
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

procedure TLLMOptionsFormTests.NoProjectShowsOnlyEditableMachineDefaults;
begin
  AssertTrue(FForm.tsGlobal.TabVisible);
  AssertFalse(FForm.tsProject.TabVisible);
  AssertFalse(FForm.tsAssociation.TabVisible);
  AssertFalse(FForm.tsTranslation.TabVisible);
  AssertTrue(FForm.pcFlows.ActivePage = FForm.tsGlobal);
  AssertTrue(FForm.cbGlobalModel.Enabled);
  AssertTrue(FForm.leGlobalOpenAI.Enabled);
  AssertEquals('*', string(FForm.leGlobalOpenAI.PasswordChar));
  AssertEquals('2', FForm.leGlobalMaxParallel.Text);
  AssertEquals('3', FForm.leGlobalMaxRetries.Text);
  AssertEquals('1000', FForm.leGlobalRetryDelay.Text);
  AssertEquals('10', FForm.leGlobalAutoSave.Text);
  FForm.cbGlobalModel.Text := 'gpt-4o-mini';
  FForm.leGlobalOpenAI.Text := SyntheticGlobalKey;
  CaptureIfRequested('global');
  AssertFalse(FileExists(FSettingsFile));
end;

procedure TLLMOptionsFormTests.CancelDiscardsGlobalDraftWithoutCreatingFile;
var Settings: TAIGlobalSettings;
begin
  FForm.cbGlobalModel.Text := 'gpt-4o-mini';
  FForm.leGlobalOpenAI.Text := SyntheticGlobalKey;
  FForm.leGlobalMaxParallel.Text := '7';
  FForm.btnCancel.Click;
  AssertEquals(Integer(mrCancel), Integer(FForm.ModalResult));
  FreeAndNil(FForm);
  AssertFalse(FileExists(FSettingsFile));
  Settings := TAIGlobalSettings.Create(FSettingsFile, FStore);
  try
    AssertFalse(Settings.HasCredential('OpenAI'));
    AssertEquals('', Settings.ReadValue(aisModel));
  finally
    Settings.Free;
  end;
end;

procedure TLLMOptionsFormTests.OKSavesGlobalDraftWithoutAnOpenProject;
var Settings: TAIGlobalSettings;
begin
  FForm.cbGlobalModel.Text := 'gpt-4o-mini';
  FForm.leGlobalOpenAI.Text := SyntheticGlobalKey;
  FForm.leGlobalMaxParallel.Text := '7';
  FForm.leGlobalRetryDelay.Text := '1200';
  FForm.leGlobalAutoSave.Text := '0';
  FForm.btnOKClick(FForm.btnOK);
  AssertEquals(Integer(mrOK), Integer(FForm.ModalResult));
  AssertTrue(FileExists(FSettingsFile));
  AssertEquals('Plaintext key must never enter the settings file', 0,
    Pos(SyntheticGlobalKey, ReadFileText(FSettingsFile)));
  Settings := TAIGlobalSettings.Create(FSettingsFile, FStore);
  try
    AssertEquals('gpt-4o-mini', Settings.ReadValue(aisModel));
    AssertEquals('7', Settings.ReadValue(aisMaxParallel));
    AssertEquals('1200', Settings.ReadValue(aisRetryDelay));
    AssertEquals('0', Settings.ReadValue(aisAutoSaveInterval));
    AssertEquals(SyntheticGlobalKey, Settings.ResolveCredential('OpenAI'));
  finally
    Settings.Free;
  end;
end;

procedure TLLMOptionsFormTests.OpeningAndCancellingDoesNotChangeProjectMetadata;
var Setting: TAICommonSetting; BeforeGlobal: string;
begin
  SeedGlobalSettings;
  BeforeGlobal := ReadFileText(FSettingsFile);
  CreateTemporaryProject;
  AssertFalse('Fixture starts clean', FProject.Modificado);
  FForm.SetProject(FProject, FSettingsFile, FStore);
  AssertFalse('Opening the dialog is read-only', FProject.Modificado);
  AssertEquals('gpt-4o-mini', FForm.cbModel.Text);
  AssertEquals('4', FForm.leMaxParallel.Text);
  AssertTrue(FForm.chkInheritModel.Checked);
  AssertTrue(FForm.chkInheritKey.Checked);
  AssertFalse(FForm.cbModel.Enabled);
  FForm.chkInheritParallel.Checked := False;
  FForm.leMaxParallel.Text := '9';
  FForm.leGlobalMaxRetries.Text := '11';
  FForm.btnCancel.Click;
  AssertFalse('Cancelling a project draft remains read-only', FProject.Modificado);
  for Setting in TAICommonSetting do
  begin
    AssertFalse(FProject.AIConfig.HasKey(AISettingConfigKey(Setting)));
    AssertFalse(FProject.AIConfig.HasKey(AISettingInheritanceKey(Setting)));
  end;
  AssertFalse(FProject.AIConfig.HasKey(AI_KEY_INHERIT_CONFIG));
  AssertEquals(BeforeGlobal, ReadFileText(FSettingsFile));
end;

procedure TLLMOptionsFormTests.OneProjectOverrideDoesNotCopyInheritedValues;
var Setting: TAICommonSetting;
begin
  SeedGlobalSettings;
  CreateTemporaryProject;
  FForm.SetProject(FProject, FSettingsFile, FStore);
  FForm.leGlobalMaxParallel.Text := '6';
  AssertEquals('Inherited display follows the staged global value', '6',
    FForm.leMaxParallel.Text);
  FForm.chkInheritParallel.Checked := False;
  AssertTrue(FForm.leMaxParallel.Enabled);
  FForm.leMaxParallel.Text := '9';
  FForm.leGlobalMaxParallel.Text := '8';
  AssertEquals('Own value stays independent of a global edit', '9',
    FForm.leMaxParallel.Text);
  CaptureIfRequested('project');
  FForm.btnOKClick(FForm.btnOK);
  AssertEquals(Integer(mrOK), Integer(FForm.ModalResult));
  AssertEquals('9', FProject.GetInfo(AISettingConfigKey(aisMaxParallel)));
  AssertEquals('false', LowerCase(FProject.GetInfo(
    AISettingInheritanceKey(aisMaxParallel))));
  for Setting in TAICommonSetting do
    if Setting <> aisMaxParallel then
      AssertFalse('Inherited values must not be materialized in the project',
        FProject.AIConfig.HasKey(AISettingConfigKey(Setting)));
  AssertFalse(FProject.HasAIKey(acsCommon));
  AssertTrue(FProject.Modificado);
end;

procedure TLLMOptionsFormTests.DetachingProjectClearsHiddenCredentialEdits;
begin
  CreateTemporaryProject;
  FProject.AIConfig.AICredentials.Stage(acsAssociation, SyntheticScopedKey);
  FForm.SetProject(FProject, FSettingsFile, FStore);
  AssertEquals('Fixture key is displayed only in its flow', 1,
    CountSecretEdits(FForm, SyntheticScopedKey));
  FForm.SetProject(nil, FSettingsFile, FStore);
  AssertEquals('Detached project credentials must leave hidden controls', 0,
    CountSecretEdits(FForm, SyntheticScopedKey));
  AssertFalse(FForm.tsProject.TabVisible);
  AssertFalse(FForm.tsAssociation.TabVisible);
  AssertFalse(FForm.tsTranslation.TabVisible);
  AssertEquals('', FForm.cbModel.Text);
  AssertEquals('', FForm.leApiKey.Text);
end;

procedure TLLMOptionsFormTests.MachineDefaultLabelsFollowSelectedLanguage;
var LanguagesDir: string;
begin
  LanguagesDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\languages');
  FreeAndNil(FForm);
  SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  try
    SetDefaultLang('pt', LanguagesDir, 'iBiblia', False);
    CreateOptionsForm;
    AssertEquals('Padrões deste computador', FForm.tsGlobal.Caption);
    AssertEquals('Configuração deste projeto', FForm.tsProject.Caption);
    AssertEquals('Testar padrões', FForm.btnTest.Caption);
    AssertEquals('OpenAI - Chave da API', FForm.leGlobalOpenAI.EditLabel.Caption);
    AssertEquals('DeepSeek - Chave da API', FForm.leGlobalDeepSeek.EditLabel.Caption);
    AssertEquals('Google - Chave da API', FForm.leGlobalGoogle.EditLabel.Caption);
    AssertEquals('Padrões para este usuário do Windows neste computador. ' +
      'As chaves da API são guardadas no cofre local de credenciais. ' +
      'As alterações são salvas ao clicar em OK.', FForm.lblGlobalDescription.Caption);
    AssertEquals('Salva para este usuário do Windows neste computador ao clicar em OK.',
      FForm.leGlobalOpenAI.Hint);
    AssertEquals('Avançado', FForm.gbGlobalAdvanced.Caption);
    AssertEquals('Intervalo entre tentativas (milissegundos)',
      FForm.leGlobalRetryDelay.EditLabel.Caption);
    AssertEquals('Salvar a cada N itens concluídos (0 = desativado)',
      FForm.leGlobalAutoSave.EditLabel.Caption);
    AssertTrue('Retry delay label must be translated',
      Pos('milissegundos', FForm.leGlobalRetryDelay.EditLabel.Caption) > 0);
    AssertTrue('Autosave label must be translated',
      Pos('conclu', FForm.leGlobalAutoSave.EditLabel.Caption) > 0);
    CaptureIfRequested('global-pt');
    SetDefaultLang('en', LanguagesDir, 'iBiblia', True);
    FForm.SetProject(nil, FSettingsFile, FStore);
    AssertEquals('Machine defaults', FForm.tsGlobal.Caption);
    AssertEquals('This project', FForm.tsProject.Caption);
    AssertEquals('Test defaults', FForm.btnTest.Caption);
    AssertEquals('OpenAI - API key', FForm.leGlobalOpenAI.EditLabel.Caption);
    AssertEquals('Advanced', FForm.gbGlobalAdvanced.Caption);
    AssertEquals('Retry delay (milliseconds)', FForm.leGlobalRetryDelay.EditLabel.Caption);
    AssertEquals('Autosave every N completed jobs (0 = off)',
      FForm.leGlobalAutoSave.EditLabel.Caption);
    AssertFalse(FileExists(FSettingsFile));
  finally
    FreeAndNil(FForm);
    SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  end;
end;

procedure TLLMOptionsFormTests.ProjectInheritanceLabelsFollowSelectedLanguage;
var LanguagesDir: string;
begin
  LanguagesDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\languages');
  CreateTemporaryProject;
  FreeAndNil(FForm);
  SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  try
    SetDefaultLang('pt', LanguagesDir, 'iBiblia', False);
    CreateOptionsForm;
    FForm.SetProject(FProject, FSettingsFile, FStore);
    AssertEquals('Usar padrão global', FForm.chkInheritModel.Caption);
    AssertEquals('Usar padrão global', FForm.chkInheritKey.Caption);
    AssertEquals('Usar padrão global', FForm.chkInheritParallel.Caption);
    AssertEquals('Usar padrão global', FForm.chkInheritRetries.Caption);
    AssertEquals('Usar padrão global', FForm.chkInheritRetryDelay.Caption);
    AssertEquals('Usar padrão global', FForm.chkInheritAutoSave.Caption);
    AssertEquals('Origem: padrão do aplicativo', FForm.lblParallelSource.Caption);
    AssertEquals('Usar os padrões globais neste projeto', FForm.btnUseGlobal.Caption);
    AssertEquals('Copiar configurações atuais para os padrões deste computador...',
      FForm.btnCopyGlobal.Caption);
    AssertEquals('Testar configurações comuns', FForm.btnTest.Caption);
    FForm.leGlobalMaxParallel.Text := '4';
    AssertEquals('Origem: padrões deste computador', FForm.lblParallelSource.Caption);
    FForm.chkInheritParallel.Checked := False;
    AssertEquals('Origem: este projeto', FForm.lblParallelSource.Caption);
    FForm.cbGlobalModel.Text := 'gpt-4o-mini';
    FForm.leGlobalOpenAI.Text := SyntheticGlobalKey;
    AssertEquals('Origem: padrões deste computador (OpenAI)', FForm.lblKeySource.Caption);
    CaptureIfRequested('project-pt');
    SetDefaultLang('en', LanguagesDir, 'iBiblia', True);
    FForm.SetProject(FProject, FSettingsFile, FStore);
    AssertEquals('Use global', FForm.chkInheritModel.Caption);
    AssertEquals('Source: application default', FForm.lblParallelSource.Caption);
    AssertEquals('Use global settings for this project', FForm.btnUseGlobal.Caption);
    AssertEquals('Copy current settings to machine defaults...', FForm.btnCopyGlobal.Caption);
    AssertEquals('Test common settings', FForm.btnTest.Caption);
    FForm.leGlobalMaxParallel.Text := '4';
    AssertEquals('Source: machine defaults', FForm.lblParallelSource.Caption);
    FForm.chkInheritParallel.Checked := False;
    AssertEquals('Source: this project', FForm.lblParallelSource.Caption);
    AssertFalse('Changing the display language must not save the project',
      FProject.Modificado);
    AssertFalse(FileExists(FSettingsFile));
  finally
    FreeAndNil(FForm);
    SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  end;
end;

procedure TLLMOptionsFormTests.ConnectionErrorsFollowSelectedLanguage;
const
  Languages: array[0..1] of string = ('pt', 'en');
  ExpectedConnectionErrors: array[0..1] of string = (
    'Configure o modelo e a chave da API em Opções de IA antes de iniciar o processamento.',
    'Configure the AI model and API key in LLM Options before processing.');
  ExpectedProviderErrors: array[0..1] of string = (
    'O provedor da credencial de IA não é compatível.',
    'The AI credential provider is not supported.');
var
  LanguagesDir, ErrorMessage: string;
  Settings: TAIGlobalSettings;
  Draft: TAIProjectAISettings;
  Setting: TAICommonSetting;
  LanguageIndex: Integer;
begin
  LanguagesDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\languages');
  FreeAndNil(FForm);
  Settings := TAIGlobalSettings.Create(FSettingsFile, FStore);
  Draft := Default(TAIProjectAISettings);
  for Setting in TAICommonSetting do Draft.Inherit[Setting] := True;
  Draft.InheritApiKey := True;
  SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  try
    for LanguageIndex := Low(Languages) to High(Languages) do
    begin
      SetDefaultLang(Languages[LanguageIndex], LanguagesDir, 'iBiblia', False);
      ErrorMessage := '';
      try
        ResolveAIConnection(Draft, Settings);
      except
        on E: EAICredentialError do ErrorMessage := E.Message;
      end;
      AssertEquals('Missing connection must report a localized error',
        ExpectedConnectionErrors[LanguageIndex], ErrorMessage);
      ErrorMessage := '';
      try
        Settings.ResolveCredential('unsupported-provider');
      except
        on E: EAICredentialError do ErrorMessage := E.Message;
      end;
      AssertEquals('Global settings must report a localized error',
        ExpectedProviderErrors[LanguageIndex], ErrorMessage);
    end;
    AssertFalse(FileExists(FSettingsFile));
  finally
    Settings.Free;
    SetDefaultLang('en', LanguagesDir, 'iBiblia', False);
  end;
end;

initialization
  RegisterTest(TLLMOptionsFormTests);

end.
