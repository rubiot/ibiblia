unit FormLlmOptions;

{$mode ObjFPC}{$H+}

interface

uses
  ui_style, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Project, llmclient, ai_flows, ai_flow_config,
  ai_credential_store, ai_settings_types, ai_global_settings, ai_effective_settings
  {$IFDEF WINDOWS}, Windows{$ENDIF};

type
  // Record to store controls for each flow tab
  TFlowTabControls = record
    TabSheet: TTabSheet;
    FlowId: string;
    ScrollBox: TScrollBox;
    LblDescription: TLabel;
    EditTargetLanguage: TLabeledEdit;
    LblInstructions: TLabel;
    MemoInstructions: TMemo;
    ChkIncludeComments: TCheckBox;
    ChkForceOverwrite: TCheckBox;
    ChkIncludeAssociations: TCheckBox;
    ChkMarkAddedWords: TCheckBox;
    chkUseOverride: TCheckBox;
    gbOverride: TGroupBox;
    CbProvider: TComboBox;
    CbModel: TComboBox;
    EditApiKey: TLabeledEdit;
    LblExamples: TLabel;
    LblExamplesSummary: TLabel;
    BtnManageExamples: TButton;
    BtnViewDefault: TButton;
    BtnReset: TButton;
    LblFormatInfo: TLabel;
    WorkingExamples: TAIFlowExampleArray;
  end;

  { TFrmLLMOptions }

  TFrmLLMOptions = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    btnTest: TButton;
    cbModel: TComboBox;
    gbCommonSettings: TGroupBox;
    LabelModel: TLabel;
    leApiKey: TLabeledEdit;
    leMaxParallel: TLabeledEdit;
    leMaxRetries: TLabeledEdit;
    pcFlows: TPageControl;
    tsGlobal: TTabSheet;
    tsProject: TTabSheet;
    sbGlobal: TScrollBox;
    sbProject: TScrollBox;
    lblGlobalDescription: TLabel;
    lblGlobalModel: TLabel;
    cbGlobalModel: TComboBox;
    leGlobalOpenAI: TLabeledEdit;
    leGlobalDeepSeek: TLabeledEdit;
    leGlobalGoogle: TLabeledEdit;
    leGlobalMaxParallel: TLabeledEdit;
    leGlobalMaxRetries: TLabeledEdit;
    gbGlobalAdvanced: TGroupBox;
    leGlobalRetryDelay: TLabeledEdit;
    leGlobalAutoSave: TLabeledEdit;
    lblProjectDescription: TLabel;
    leRetryDelay: TLabeledEdit;
    leAutoSave: TLabeledEdit;
    chkInheritModel: TCheckBox;
    chkInheritKey: TCheckBox;
    chkInheritParallel: TCheckBox;
    chkInheritRetries: TCheckBox;
    chkInheritRetryDelay: TCheckBox;
    chkInheritAutoSave: TCheckBox;
    lblModelSource: TLabel;
    lblKeySource: TLabel;
    lblParallelSource: TLabel;
    lblRetriesSource: TLabel;
    lblRetryDelaySource: TLabel;
    lblAutoSaveSource: TLabel;
    btnUseGlobal: TButton;
    btnCopyGlobal: TButton;
    tsAssociation: TTabSheet;
    tsTranslation: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    FProject: TProject;
    FGlobalSettings: TAIGlobalSettings;
    FProjectSettings: TAIProjectAISettings;
    FLoadingSettings: Boolean;
    FInheritControls: array[TAICommonSetting] of TCheckBox;
    FSourceLabels: array[TAICommonSetting] of TLabel;
    FGlobalKeyEdits: array[0..2] of TLabeledEdit;
    FLoadedGlobalKeys: array[0..2] of string;
    FFlowTabs: array of TFlowTabControls;
    FLoadedApiKeys: array[TAICredentialScope] of string;
    procedure LoadGlobalSettings;
    procedure CaptureGlobalSettings;
    procedure CaptureProjectSettings;
    procedure RefreshProjectSettings;
    procedure RefreshLocalizedCaptions;
    procedure OnGlobalFieldsChanged(Sender: TObject);
    procedure OnInheritChanged(Sender: TObject);
    procedure OnUseGlobalClick(Sender: TObject);
    procedure OnCopyGlobalClick(Sender: TObject);
    procedure OnSettingsPageChanged(Sender: TObject);
    function ReadSettingText(Setting: TAICommonSetting; Global: Boolean): string;
    procedure WriteSettingText(Setting: TAICommonSetting; Global: Boolean;
      const Value: string);
    function SettingControl(Setting: TAICommonSetting; Global: Boolean): TWinControl;
    function LoadCredential(Scope: TAICredentialScope; Edit: TLabeledEdit): string;
    procedure SaveCredential(Scope: TAICredentialScope; Edit: TLabeledEdit);
    procedure UpdateTestEnabled;
    procedure OnFieldsChanged(Sender: TObject);
    procedure LoadFromProject;
    procedure SaveToProject;
    procedure InitializeFlowTabs;
    procedure CreateAssociationTab(ATabSheet: TTabSheet; TabIdx: Integer);
    procedure CreateTranslationTab(ATabSheet: TTabSheet; TabIdx: Integer);
    function CreateFlowScrollBox(ATabSheet: TTabSheet; TabIdx: Integer): TScrollBox;
    procedure AddFlowDescription(AParent: TWinControl; var YPos: Integer;
      var LblDescription: TLabel; const ADescription: string);
    procedure AddInstructionsMemo(AParent: TWinControl; var YPos: Integer;
      var LblInstructions: TLabel; var MemoInstructions: TMemo;
      const ACaption, AHint: string);
    procedure AddProviderOverrideSection(AParent: TWinControl; var YPos: Integer;
      TabIdx: Integer; var TabControls: TFlowTabControls);
    procedure AddExamplesSection(AParent: TWinControl; var YPos: Integer;
      TabIdx: Integer; var TabControls: TFlowTabControls);
    procedure AddPromptButtonsAndFormatInfo(AParent: TWinControl; var YPos: Integer;
      TabIdx: Integer; var TabControls: TFlowTabControls;
      const AOutputFormatInfo: string);
    procedure SetFlowProviderValues(TabIdx: Integer; const AProvider, AModel,
      AApiKey: string);
    procedure OnFlowScrollBoxResize(Sender: TObject);
    procedure ResizeFlowTabControls(TabIdx: Integer);
    procedure LoadFlowConfig(const AFlowId: string);
    procedure SaveFlowConfig(const AFlowId: string);
    procedure OnViewDefaultClick(Sender: TObject);
    procedure OnResetClick(Sender: TObject);
    procedure OnManageExamplesClick(Sender: TObject);
    procedure OnFlowProviderChange(Sender: TObject);
    procedure OnFlowOverrideToggle(Sender: TObject);
    procedure RefreshFlowExamplesUI(const AFlowId: string);
    procedure UpdateFlowOverrideControls(TabIdx: Integer);
    function BuildCurrentPrompt(TabIdx: Integer): string;
    function GetFlowTabByFlowId(const AFlowId: string): TFlowTabControls;
    function GetFlowTabIndexByFlowId(const AFlowId: string): Integer;
  public
    procedure SetProject(AProject: TProject; const AGlobalFileName: string = '';
      const AStore: IAICredentialStore = nil);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

var
  FrmLLMOptions: TFrmLLMOptions;

resourcestring
  rsLlmOptionsCaption = 'LLM Options';
  rsLabelModel = 'Model';
  rsLabelApiKey = 'API key';
  rsLocalApiKey = 'API key (local to this user/computer)';
  rsMissingApiKey = 'API key unavailable here - configure again';
  rsLocalApiKeyHint = 'Saved in local credential storage when the project is saved. Copies opened on another computer require the key again.';
  rsBtnOK = 'OK';
  rsBtnCancel = 'Cancel';
  rsBtnClose = 'Close';
  rsBtnTest = 'Test';
  rsTestResultTitle = 'Test result';
  rsTestFailedTitle = 'Test failed';
  rsSystemPrompt = 'You are a helpful assistant ready to assist users with their questions on Bible translation.';
  rsFallbackTestPrompt = 'Tell me something about you. Answer in JSON format.';
  rsLabelMaxParallel = 'Max Parallel';
  rsLabelMaxRetries = 'Max Retries';
  rsCommonSettings = 'Common Settings (All Flows)';
  rsAdditionalInstructions = 'Additional Instructions (Optional):';
  rsBtnViewDefault = 'View Current Prompt';
  rsBtnReset = 'Reset to Default';
  rsSavedExamples = 'Saved Examples:';
  rsExamplesSummaryFmt = '%d saved example(s).';
  rsBtnManageExamples = 'Manage examples...';
  rsNoExamples = '(no saved examples)';
  rsDefaultPromptTitle = 'Current Prompt - %s';
  rsFormatInfoFmt = 'Info: %s';
  rsCustomInstructionsHint = 'Add specific guidelines for your project (e.g., translation philosophy, examples). Leave empty to use only the default prompt.';
  rsProviderOverride = 'Provider Override (Optional)';
  rsUseFlowOverride = 'Use flow-specific provider/model/API key';
  rsGlobalTab = 'Machine defaults';
  rsProjectTab = 'This project';
  rsGlobalDescription = 'Defaults for this Windows user on this computer. API keys are stored in the local credential store. Changes are saved when you click OK.';
  rsProjectDescription = 'Each setting can follow the machine defaults or use its own value. Project changes take effect now and are stored when you save the project.';
  rsUseGlobal = 'Use global';
  rsUsingGlobal = 'Source: machine defaults';
  rsUsingDefault = 'Source: application default';
  rsChooseModelForKey = 'Select a model to choose the provider API key';
  rsUsingProject = 'Source: this project';
  rsGlobalKeySource = 'Source: machine defaults (%s)';
  rsKeyNotConfigured = 'No API key configured for %s';
  rsKeyConfigured = 'API key configured for %s';
  rsGlobalKeyHint = 'Saved for this Windows user on this computer when you click OK.';
  rsAdvanced = 'Advanced';
  rsRetryDelay = 'Retry delay (milliseconds)';
  rsAutoSave = 'Autosave every N completed jobs (0 = off)';
  rsUseGlobalForProject = 'Use global settings for this project';
  rsCopyProjectToGlobal = 'Copy current settings to machine defaults...';
  rsCopyGlobalTitle = 'Update machine defaults';
  rsCopyGlobalPreview = 'The following common settings will become the defaults for this user on this computer:';
  rsCopyGlobalStaged = 'This will be saved only when you click OK. Existing project overrides are preserved.';
  rsMaskedKey = 'API key: ******** (%s)';
  rsNoKeyToCopy = 'API key: none';
  rsSettingsError = 'Could not apply AI settings';
  rsGlobalTest = 'Test defaults';
  rsProjectTest = 'Test common settings';

implementation

uses
  ai_association_flow, ai_translation_flow, FormFlowExamples;

{ TFrmLLMOptions }

const
  GlobalCredentialProviders: array[0..2] of string = ('OpenAI', 'DeepSeek', 'Google');

function TFrmLLMOptions.SettingControl(Setting: TAICommonSetting;
  Global: Boolean): TWinControl;
begin
  Result := nil;
  if Global then
    case Setting of
      aisModel: Result := cbGlobalModel;
      aisMaxParallel: Result := leGlobalMaxParallel;
      aisMaxRetries: Result := leGlobalMaxRetries;
      aisRetryDelay: Result := leGlobalRetryDelay;
      aisAutoSaveInterval: Result := leGlobalAutoSave;
    end
  else
    case Setting of
      aisModel: Result := cbModel;
      aisMaxParallel: Result := leMaxParallel;
      aisMaxRetries: Result := leMaxRetries;
      aisRetryDelay: Result := leRetryDelay;
      aisAutoSaveInterval: Result := leAutoSave;
    end;
end;

function TFrmLLMOptions.ReadSettingText(Setting: TAICommonSetting;
  Global: Boolean): string;
var Control: TWinControl;
begin
  Control := SettingControl(Setting, Global);
  if Control is TComboBox then Result := TComboBox(Control).Text
  else Result := TLabeledEdit(Control).Text;
end;

procedure TFrmLLMOptions.WriteSettingText(Setting: TAICommonSetting;
  Global: Boolean; const Value: string);
var Control: TWinControl;
begin
  Control := SettingControl(Setting, Global);
  if Control is TComboBox then
  begin
    if TComboBox(Control).Text <> Value then TComboBox(Control).Text := Value;
  end
  else if TLabeledEdit(Control).Text <> Value then TLabeledEdit(Control).Text := Value;
end;

procedure TFrmLLMOptions.LoadGlobalSettings;
var
  Setting: TAICommonSetting;
  I: Integer;
  Secret, ErrorMessage, Value: string;
begin
  for Setting in TAICommonSetting do
  begin
    Value := FGlobalSettings.ReadValue(Setting);
    if Value = '' then Value := AISettingDefault(Setting);
    WriteSettingText(Setting, True, Value);
  end;
  for I := 0 to High(FGlobalKeyEdits) do
  begin
    if FGlobalSettings.TryResolveCredential(GlobalCredentialProviders[I],
      Secret, ErrorMessage) then
    begin
      FGlobalKeyEdits[I].EditLabel.Caption := GlobalCredentialProviders[I] + ' - ' + rsLabelApiKey;
      FGlobalKeyEdits[I].Hint := rsGlobalKeyHint;
    end
    else
    begin
      FGlobalKeyEdits[I].EditLabel.Caption := GlobalCredentialProviders[I] + ' - ' + rsMissingApiKey;
      FGlobalKeyEdits[I].Hint := ErrorMessage;
    end;
    FLoadedGlobalKeys[I] := Secret;
    FGlobalKeyEdits[I].Text := Secret;
  end;
end;

procedure TFrmLLMOptions.CaptureGlobalSettings;
var Setting: TAICommonSetting; I: Integer;
begin
  if not Assigned(FGlobalSettings) then Exit;
  for Setting in TAICommonSetting do
    FGlobalSettings.SetValue(Setting, ReadSettingText(Setting, True));
  for I := 0 to High(FGlobalKeyEdits) do
    if Trim(FGlobalKeyEdits[I].Text) <> FLoadedGlobalKeys[I] then
    begin
      FGlobalSettings.StageCredential(GlobalCredentialProviders[I],
        FGlobalKeyEdits[I].Text);
      FLoadedGlobalKeys[I] := Trim(FGlobalKeyEdits[I].Text);
      FGlobalKeyEdits[I].EditLabel.Caption := GlobalCredentialProviders[I] + ' - ' + rsLabelApiKey;
      FGlobalKeyEdits[I].Hint := rsGlobalKeyHint;
    end;
end;

procedure TFrmLLMOptions.CaptureProjectSettings;
var Setting: TAICommonSetting; Model: string;
begin
  if not Assigned(FProject) then Exit;
  for Setting in TAICommonSetting do
    if not FProjectSettings.Inherit[Setting] then
      FProjectSettings.Values[Setting] := ReadSettingText(Setting, False);
  if not FProjectSettings.InheritApiKey and
    (Trim(leApiKey.Text) <> FProjectSettings.ApiKey) then
  begin
    FProjectSettings.ApiKey := Trim(leApiKey.Text);
    FProjectSettings.ApiKeyConfigured := FProjectSettings.ApiKey <> '';
    FProjectSettings.ApiKeyError := '';
    Model := FProjectSettings.Values[aisModel];
    if FProjectSettings.Inherit[aisModel] then
      Model := FGlobalSettings.ReadValue(aisModel);
    FProjectSettings.ApiKeyProvider := AIProviderForModel(Model);
  end;
end;

procedure TFrmLLMOptions.RefreshProjectSettings;
var
  Setting: TAICommonSetting;
  Provider, Model, Value: string;
  WasLoading: Boolean;
begin
  if not Assigned(FProject) or not Assigned(FGlobalSettings) then Exit;
  WasLoading := FLoadingSettings;
  FLoadingSettings := True;
  try
    for Setting in TAICommonSetting do
    begin
      FInheritControls[Setting].Checked := FProjectSettings.Inherit[Setting];
      SettingControl(Setting, False).Enabled := not FProjectSettings.Inherit[Setting];
      if FProjectSettings.Inherit[Setting] then
      begin
        Value := FGlobalSettings.ReadValue(Setting);
        if Value = '' then
        begin
          Value := AISettingDefault(Setting);
          FSourceLabels[Setting].Caption := rsUsingDefault;
        end
        else FSourceLabels[Setting].Caption := rsUsingGlobal;
        WriteSettingText(Setting, False, Value);
      end
      else
      begin
        WriteSettingText(Setting, False, FProjectSettings.Values[Setting]);
        FSourceLabels[Setting].Caption := rsUsingProject;
      end;
    end;
    chkInheritKey.Checked := FProjectSettings.InheritApiKey;
    leApiKey.Enabled := not FProjectSettings.InheritApiKey;
    Model := ReadSettingText(aisModel, False);
    Provider := AIProviderForModel(Model);
    if FProjectSettings.InheritApiKey then
    begin
      leApiKey.Text := '';
      leApiKey.EditLabel.Caption := rsLabelApiKey;
      lblKeySource.Caption := Format(rsGlobalKeySource, [Provider]);
      if Provider = '' then
      begin
        leApiKey.Hint := rsChooseModelForKey;
        lblKeySource.Caption := rsChooseModelForKey;
      end
      else if FGlobalSettings.HasCredential(Provider) then
        leApiKey.Hint := Format(rsKeyConfigured, [Provider])
      else
      begin
        leApiKey.Hint := Format(rsKeyNotConfigured, [Provider]);
        lblKeySource.Caption := leApiKey.Hint;
      end;
    end
    else
    begin
      if leApiKey.Text <> FProjectSettings.ApiKey then
        leApiKey.Text := FProjectSettings.ApiKey;
      lblKeySource.Caption := rsUsingProject;
      if FProjectSettings.ApiKeyError <> '' then
      begin
        leApiKey.EditLabel.Caption := rsMissingApiKey;
        leApiKey.Hint := FProjectSettings.ApiKeyError;
      end
      else
      begin
        leApiKey.EditLabel.Caption := rsLocalApiKey;
        leApiKey.Hint := rsLocalApiKeyHint;
      end;
    end;
    leApiKey.ShowHint := True;
  finally
    FLoadingSettings := WasLoading;
  end;
end;

procedure TFrmLLMOptions.OnGlobalFieldsChanged(Sender: TObject);
begin
  if FLoadingSettings then Exit;
  CaptureGlobalSettings;
  RefreshProjectSettings;
  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.OnInheritChanged(Sender: TObject);
var Setting: TAICommonSetting;
begin
  if FLoadingSettings or not Assigned(FProject) then Exit;
  CaptureProjectSettings;
  for Setting in TAICommonSetting do
  begin
    if FProjectSettings.Inherit[Setting] and not FInheritControls[Setting].Checked then
      FProjectSettings.Values[Setting] := ReadSettingText(Setting, False);
    FProjectSettings.Inherit[Setting] := FInheritControls[Setting].Checked;
  end;
  FProjectSettings.InheritApiKey := chkInheritKey.Checked;
  RefreshProjectSettings;
  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.OnUseGlobalClick(Sender: TObject);
var Setting: TAICommonSetting;
begin
  if not Assigned(FProject) then Exit;
  CaptureProjectSettings;
  for Setting in TAICommonSetting do FProjectSettings.Inherit[Setting] := True;
  FProjectSettings.InheritApiKey := True;
  RefreshProjectSettings;
  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.OnCopyGlobalClick(Sender: TObject);
var
  Common: TAICommonSettings;
  Connection: TAIResolvedConnection;
  Preview, CredentialPreview: string;
  WasLoading: Boolean;
begin
  if not Assigned(FProject) then Exit;
  try
    CaptureGlobalSettings;
    CaptureProjectSettings;
    Common := ResolveCommonAISettings(FProjectSettings, FGlobalSettings);
    Connection := Default(TAIResolvedConnection);
    Connection.Provider := Common.Provider;
    if (not FProjectSettings.InheritApiKey and FProjectSettings.ApiKeyConfigured) or
       (FProjectSettings.InheritApiKey and (Common.Provider <> '') and
        FGlobalSettings.HasCredential(Common.Provider)) then
      Connection := ResolveAIConnection(FProjectSettings, FGlobalSettings);
    if Connection.ApiKey = '' then CredentialPreview := rsNoKeyToCopy
    else CredentialPreview := Format(rsMaskedKey, [Connection.Provider]);
    Preview := rsCopyGlobalPreview + LineEnding + LineEnding +
      rsLabelModel + ': ' + Common.Model + LineEnding + CredentialPreview + LineEnding +
      rsLabelMaxParallel + ': ' + IntToStr(Common.MaxParallel) + LineEnding +
      rsLabelMaxRetries + ': ' + IntToStr(Common.MaxRetries) + LineEnding +
      rsRetryDelay + ': ' + IntToStr(Common.RetryDelay) + LineEnding +
      rsAutoSave + ': ' + IntToStr(Common.AutoSaveInterval) + LineEnding + LineEnding +
      rsCopyGlobalStaged;
    if MessageDlg(rsCopyGlobalTitle, Preview, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
    FGlobalSettings.SetValue(aisModel, Common.Model);
    FGlobalSettings.SetValue(aisMaxParallel, IntToStr(Common.MaxParallel));
    FGlobalSettings.SetValue(aisMaxRetries, IntToStr(Common.MaxRetries));
    FGlobalSettings.SetValue(aisRetryDelay, IntToStr(Common.RetryDelay));
    FGlobalSettings.SetValue(aisAutoSaveInterval, IntToStr(Common.AutoSaveInterval));
    if Connection.ApiKey <> '' then
      FGlobalSettings.StageCredential(Connection.Provider, Connection.ApiKey);
    WasLoading := FLoadingSettings;
    FLoadingSettings := True;
    try LoadGlobalSettings; finally FLoadingSettings := WasLoading; end;
    RefreshProjectSettings;
    pcFlows.ActivePage := tsGlobal;
    UpdateTestEnabled;
  except
    on E: Exception do MessageDlg(rsSettingsError, E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TFrmLLMOptions.RefreshLocalizedCaptions;
var Setting: TAICommonSetting; I: Integer; FlowConfig: IAIFlowConfig;
begin
  Caption := rsLlmOptionsCaption;
  tsGlobal.Caption := rsGlobalTab;
  tsProject.Caption := rsProjectTab;
  lblGlobalDescription.Caption := rsGlobalDescription;
  lblProjectDescription.Caption := rsProjectDescription;
  LabelModel.Caption := rsLabelModel;
  lblGlobalModel.Caption := rsLabelModel;
  leApiKey.EditLabel.Caption := rsLocalApiKey;
  gbCommonSettings.Caption := rsCommonSettings;
  leMaxParallel.EditLabel.Caption := rsLabelMaxParallel;
  leMaxRetries.EditLabel.Caption := rsLabelMaxRetries;
  leGlobalMaxParallel.EditLabel.Caption := rsLabelMaxParallel;
  leGlobalMaxRetries.EditLabel.Caption := rsLabelMaxRetries;
  leRetryDelay.EditLabel.Caption := rsRetryDelay;
  leAutoSave.EditLabel.Caption := rsAutoSave;
  leGlobalRetryDelay.EditLabel.Caption := rsRetryDelay;
  leGlobalAutoSave.EditLabel.Caption := rsAutoSave;
  gbGlobalAdvanced.Caption := rsAdvanced;
  btnUseGlobal.Caption := rsUseGlobalForProject;
  btnCopyGlobal.Caption := rsCopyProjectToGlobal;
  btnOK.Caption := rsBtnOK;
  btnCancel.Caption := rsBtnCancel;
  for Setting in TAICommonSetting do
    FInheritControls[Setting].Caption := rsUseGlobal;
  chkInheritKey.Caption := rsUseGlobal;
  for I := 0 to High(FFlowTabs) do
  begin
    FlowConfig := GetFlowConfigById(FFlowTabs[I].FlowId);
    FFlowTabs[I].TabSheet.Caption := FlowConfig.GetDisplayName;
    FFlowTabs[I].chkUseOverride.Caption := rsUseFlowOverride;
    FFlowTabs[I].gbOverride.Caption := rsProviderOverride;
    FFlowTabs[I].CbProvider.Hint := ai_association_flow.SConfigProviderOverrideDescription;
    FFlowTabs[I].CbModel.Hint := ai_association_flow.SConfigModelOverrideDescription;
    FFlowTabs[I].EditApiKey.Hint := ai_association_flow.SConfigApiKeyOverrideDescription;
  end;
end;

procedure TFrmLLMOptions.FormCreate(Sender: TObject);
var
  Setting: TAICommonSetting;
  I: Integer;
begin
  ApplyFormStyle(Self);
  FLoadingSettings := True;
  FInheritControls[aisModel] := chkInheritModel;
  FInheritControls[aisMaxParallel] := chkInheritParallel;
  FInheritControls[aisMaxRetries] := chkInheritRetries;
  FInheritControls[aisRetryDelay] := chkInheritRetryDelay;
  FInheritControls[aisAutoSaveInterval] := chkInheritAutoSave;
  FSourceLabels[aisModel] := lblModelSource;
  FSourceLabels[aisMaxParallel] := lblParallelSource;
  FSourceLabels[aisMaxRetries] := lblRetriesSource;
  FSourceLabels[aisRetryDelay] := lblRetryDelaySource;
  FSourceLabels[aisAutoSaveInterval] := lblAutoSaveSource;
  FGlobalKeyEdits[0] := leGlobalOpenAI;
  FGlobalKeyEdits[1] := leGlobalDeepSeek;
  FGlobalKeyEdits[2] := leGlobalGoogle;
  ReloadModelRegistry;
  FillSupportedModelNames(cbModel.Items);
  FillSupportedModelNames(cbGlobalModel.Items);
  leApiKey.PasswordChar := '*';
  btnUseGlobal.OnClick := @OnUseGlobalClick;
  btnCopyGlobal.OnClick := @OnCopyGlobalClick;
  for Setting in TAICommonSetting do
  begin
    FInheritControls[Setting].OnChange := @OnInheritChanged;
  end;
  chkInheritKey.OnChange := @OnInheritChanged;
  cbModel.OnChange := @OnFieldsChanged;
  leApiKey.OnChange := @OnFieldsChanged;
  leMaxParallel.OnChange := @OnFieldsChanged;
  leMaxRetries.OnChange := @OnFieldsChanged;
  leRetryDelay.OnChange := @OnFieldsChanged;
  leAutoSave.OnChange := @OnFieldsChanged;
  cbGlobalModel.OnChange := @OnGlobalFieldsChanged;
  leGlobalMaxParallel.OnChange := @OnGlobalFieldsChanged;
  leGlobalMaxRetries.OnChange := @OnGlobalFieldsChanged;
  leGlobalRetryDelay.OnChange := @OnGlobalFieldsChanged;
  leGlobalAutoSave.OnChange := @OnGlobalFieldsChanged;
  for I := 0 to High(FGlobalKeyEdits) do
  begin
    FGlobalKeyEdits[I].PasswordChar := '*';
    FGlobalKeyEdits[I].OnChange := @OnGlobalFieldsChanged;
    FGlobalKeyEdits[I].Hint := rsGlobalKeyHint;
    FGlobalKeyEdits[I].ShowHint := True;
  end;
  pcFlows.OnChange := @OnSettingsPageChanged;
  InitializeFlowTabs;
  RefreshLocalizedCaptions;
  FLoadingSettings := False;
end;

procedure TFrmLLMOptions.btnOKClick(Sender: TObject);
var Values: TAISettingValues; Setting: TAICommonSetting;
begin
  try
    CaptureGlobalSettings;
    CaptureProjectSettings;
    for Setting in TAICommonSetting do Values[Setting] := FGlobalSettings.ReadValue(Setting);
    ValidateAISettingValues(Values);
    if Assigned(FProject) then
      ResolveCommonAISettings(FProjectSettings, FGlobalSettings);
    FGlobalSettings.Save;
    SaveToProject;
    ModalResult := mrOK;
  except
    on E: Exception do
      MessageDlg(rsSettingsError, E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TFrmLLMOptions.btnTestClick(Sender: TObject);
var
  client: TBaseLLMClient;
  resp: TLLMResponse;
  testPrompt: string;
  msgs: array[0..1] of TLLMMessage;
  oldCursor: TCursor;
  Draft: TAIProjectAISettings;
  Connection: TAIResolvedConnection;
  Setting: TAICommonSetting;
begin
  testPrompt := rsFallbackTestPrompt;
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  btnTest.Enabled := False;
  try
    Application.ProcessMessages;
    CaptureGlobalSettings;
    CaptureProjectSettings;
    Draft := FProjectSettings;
    if not Assigned(FProject) or (pcFlows.ActivePage = tsGlobal) then
    begin
      Draft := Default(TAIProjectAISettings);
      for Setting in TAICommonSetting do Draft.Inherit[Setting] := True;
      Draft.InheritApiKey := True;
    end;
    Connection := ResolveAIConnection(Draft, FGlobalSettings);
    client := CreateBackend(Connection.Provider, Connection.Model, Connection.ApiKey);
    try
      msgs[0].Role := 'system';
      msgs[0].Content := rsSystemPrompt;
      msgs[1].Role := 'user';
      msgs[1].Content := testPrompt;
      resp := client.AskChatCompletions(msgs);
      if Pos('error', LowerCase(resp.Content)) > 0 then
        MessageDlg(rsTestResultTitle, Copy(resp.Content, 1, 1000), mtError, [mbOK], 0)
      else
        MessageDlg(rsTestResultTitle, Copy(resp.Content, 1, 1000), mtInformation, [mbOK], 0);
    finally
      client.Free;
    end;
  except
    on E: Exception do
      MessageDlg(rsTestFailedTitle, E.Message, mtError, [mbOK], 0);
  end;
  Screen.Cursor := oldCursor;
  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.UpdateTestEnabled;
begin
  if not Assigned(FGlobalSettings) then
  begin
    btnTest.Enabled := False;
    Exit;
  end;
  if not Assigned(FProject) or (pcFlows.ActivePage = tsGlobal) then
  begin
    btnTest.Caption := rsGlobalTest;
    btnTest.Enabled := Trim(cbGlobalModel.Text) <> '';
  end
  else
  begin
    btnTest.Caption := rsProjectTest;
    btnTest.Enabled := Trim(cbModel.Text) <> '';
  end;
end;

procedure TFrmLLMOptions.OnFieldsChanged(Sender: TObject);
begin
  if FLoadingSettings then Exit;
  CaptureProjectSettings;
  RefreshProjectSettings;
  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.OnSettingsPageChanged(Sender: TObject);
begin
  UpdateTestEnabled;
end;

function TFrmLLMOptions.LoadCredential(Scope: TAICredentialScope;
  Edit: TLabeledEdit): string;
var ErrorMessage: string;
begin
  if FProject.TryGetAIKey(Scope, Result, ErrorMessage) then
  begin
    Edit.EditLabel.Caption := rsLocalApiKey;
    Edit.Hint := rsLocalApiKeyHint;
  end
  else
  begin
    Edit.EditLabel.Caption := rsMissingApiKey;
    Edit.Hint := ErrorMessage;
  end;
  Edit.ShowHint := True;
  FLoadedApiKeys[Scope] := Result;
end;

procedure TFrmLLMOptions.SaveCredential(Scope: TAICredentialScope;
  Edit: TLabeledEdit);
begin
  { Opening options on another computer must not clear an unavailable reference. }
  if Trim(Edit.Text) <> FLoadedApiKeys[Scope] then
    FProject.SetAIKey(Scope, Edit.Text);
end;

procedure TFrmLLMOptions.LoadFromProject;
begin
  if not Assigned(FProject) then Exit;
  FProjectSettings := ReadProjectAISettings(FProject.AIConfig);
  RefreshProjectSettings;
  LoadFlowConfig('association');
  LoadFlowConfig('translation');
  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.SaveToProject;
begin
  if not Assigned(FProject) then Exit;
  FProject.ApplyAISettings(FProjectSettings);
  SaveFlowConfig('association');
  SaveFlowConfig('translation');
end;

procedure TFrmLLMOptions.InitializeFlowTabs;
begin
  SetLength(FFlowTabs, 2);
  CreateAssociationTab(tsAssociation, 0);
  CreateTranslationTab(tsTranslation, 1);
end;

function TFrmLLMOptions.CreateFlowScrollBox(ATabSheet: TTabSheet; TabIdx: Integer): TScrollBox;
begin
  Result := TScrollBox.Create(ATabSheet);
  with Result do
  begin
    Parent := ATabSheet;
    Align := alClient;
    BorderStyle := bsNone;
    Tag := TabIdx;
    HorzScrollBar.Visible := False;
    OnResize := @OnFlowScrollBoxResize;
  end;
end;

procedure TFrmLLMOptions.OnFlowScrollBoxResize(Sender: TObject);
begin
  if Sender is TScrollBox then
    ResizeFlowTabControls(TScrollBox(Sender).Tag);
end;

procedure TFrmLLMOptions.ResizeFlowTabControls(TabIdx: Integer);
var
  contentWidth, innerWidth: Integer;
  tabControls: TFlowTabControls;

  procedure SetWidthIfAssigned(AControl: TControl; AWidth: Integer);
  begin
    if Assigned(AControl) then
      AControl.Width := AWidth;
  end;

begin
  if (TabIdx < 0) or (TabIdx > High(FFlowTabs)) then Exit;
  tabControls := FFlowTabs[TabIdx];
  if not Assigned(tabControls.ScrollBox) then Exit;

  contentWidth := tabControls.ScrollBox.ClientWidth - UIScale(Self, 32);
  if contentWidth < 240 then
    contentWidth := 240;
  SetWidthIfAssigned(tabControls.LblDescription, contentWidth);
  SetWidthIfAssigned(tabControls.EditTargetLanguage, contentWidth);
  SetWidthIfAssigned(tabControls.LblInstructions, contentWidth);
  SetWidthIfAssigned(tabControls.MemoInstructions, contentWidth);
  SetWidthIfAssigned(tabControls.ChkIncludeComments, contentWidth);
  SetWidthIfAssigned(tabControls.ChkForceOverwrite, contentWidth);
  SetWidthIfAssigned(tabControls.ChkIncludeAssociations, contentWidth);
  SetWidthIfAssigned(tabControls.ChkMarkAddedWords, contentWidth);
  SetWidthIfAssigned(tabControls.chkUseOverride, contentWidth);
  SetWidthIfAssigned(tabControls.gbOverride, contentWidth);
  SetWidthIfAssigned(tabControls.LblExamplesSummary, contentWidth);
  SetWidthIfAssigned(tabControls.LblFormatInfo, contentWidth);

  if Assigned(tabControls.gbOverride) then
    innerWidth := tabControls.gbOverride.ClientWidth - UIScale(Self, 32)
  else
    innerWidth := contentWidth - UIScale(Self, 40);
  if innerWidth < UIScale(Self, 200) then
    innerWidth := UIScale(Self, 200);

  SetWidthIfAssigned(tabControls.CbProvider, innerWidth);
  SetWidthIfAssigned(tabControls.CbModel, innerWidth);
  SetWidthIfAssigned(tabControls.EditApiKey, innerWidth);
end;

procedure TFrmLLMOptions.AddFlowDescription(AParent: TWinControl;
  var YPos: Integer; var LblDescription: TLabel; const ADescription: string);
begin
  LblDescription := TLabel.Create(AParent);
  with LblDescription do
  begin
    Parent := AParent;
    Left := 16;
    Top := YPos;
    Width := 600;
    Anchors := [akTop, akLeft];
    Caption := ADescription;
    WordWrap := True;
    AutoSize := True;
    Font.Style := [fsBold];
  end;
  LblDescription.AdjustSize;
  Inc(YPos, LblDescription.Height + 16);
end;

procedure TFrmLLMOptions.AddInstructionsMemo(AParent: TWinControl;
  var YPos: Integer; var LblInstructions: TLabel;
  var MemoInstructions: TMemo; const ACaption, AHint: string);
begin
  LblInstructions := TLabel.Create(AParent);
  with LblInstructions do
  begin
    Parent := AParent;
    Left := 16;
    Top := YPos;
    Caption := ACaption + ':';
    AutoSize := True;
  end;
  Inc(YPos, 24);

  MemoInstructions := TMemo.Create(AParent);
  with MemoInstructions do
  begin
    Parent := AParent;
    Left := 16;
    Top := YPos;
    Width := 600;
    Height := 120;
    Anchors := [akTop, akLeft];
    ScrollBars := ssAutoBoth;
    Hint := AHint;
    ShowHint := True;
    WordWrap := True;
    BorderStyle := bsSingle;
    Color := clWindow;
  end;
  Inc(YPos, MemoInstructions.Height + 12);
end;

procedure TFrmLLMOptions.AddProviderOverrideSection(AParent: TWinControl;
  var YPos: Integer; TabIdx: Integer; var TabControls: TFlowTabControls);
var
  lbl: TLabel;
begin
  TabControls.chkUseOverride := TCheckBox.Create(AParent);
  with TabControls.chkUseOverride do
  begin
    Parent := AParent;
    Left := 16;
    Top := YPos;
    Width := 360;
    Caption := rsUseFlowOverride;
    Tag := TabIdx;
    OnChange := @OnFlowOverrideToggle;
  end;
  Inc(YPos, 32);

  TabControls.gbOverride := TGroupBox.Create(AParent);
  with TabControls.gbOverride do
  begin
    Parent := AParent;
    Left := 16;
    Top := YPos;
    Width := 600;
    Height := 224;
    Anchors := [akTop, akLeft];
    Caption := rsProviderOverride;
  end;

  lbl := TLabel.Create(TabControls.gbOverride);
  with lbl do
  begin
    Parent := TabControls.gbOverride;
    Left := 16;
    Top := 20;
    Caption := ai_association_flow.SConfigProviderOverrideDisplayName;
    AutoSize := True;
  end;

  TabControls.CbProvider := TComboBox.Create(TabControls.gbOverride);
  with TabControls.CbProvider do
  begin
    Parent := TabControls.gbOverride;
    Left := 16;
    Top := 44;
    Width := 584;
    Anchors := [akTop, akLeft];
    Style := csDropDownList;
    Hint := ai_association_flow.SConfigProviderOverrideDescription;
    ShowHint := True;
    Tag := TabIdx;
    OnChange := @OnFlowProviderChange;
    FillSupportedProviders(Items);
  end;

  lbl := TLabel.Create(TabControls.gbOverride);
  with lbl do
  begin
    Parent := TabControls.gbOverride;
    Left := 16;
    Top := 80;
    Caption := ai_association_flow.SConfigModelOverrideDisplayName;
    AutoSize := True;
  end;

  TabControls.CbModel := TComboBox.Create(TabControls.gbOverride);
  with TabControls.CbModel do
  begin
    Parent := TabControls.gbOverride;
    Left := 16;
    Top := 104;
    Width := 584;
    Anchors := [akTop, akLeft];
    Style := csDropDown;
    Hint := ai_association_flow.SConfigModelOverrideDescription;
    ShowHint := True;
    FillSupportedModelNames(Items);
  end;

  TabControls.EditApiKey := TLabeledEdit.Create(TabControls.gbOverride);
  with TabControls.EditApiKey do
  begin
    Parent := TabControls.gbOverride;
    Left := 16;
    Top := 160;
    Width := 584;
    Height := 32;
    Anchors := [akTop, akLeft];
    EditLabel.Caption := ai_association_flow.SConfigApiKeyOverrideDisplayName;
    LabelPosition := lpAbove;
    LabelSpacing := 3;
    PasswordChar := '*';
    Hint := ai_association_flow.SConfigApiKeyOverrideDescription;
    ShowHint := True;
  end;

  Inc(YPos, TabControls.gbOverride.Height + 18);
end;

procedure TFrmLLMOptions.AddExamplesSection(AParent: TWinControl;
  var YPos: Integer; TabIdx: Integer; var TabControls: TFlowTabControls);
begin
  TabControls.LblExamples := TLabel.Create(AParent);
  with TabControls.LblExamples do
  begin
    Parent := AParent;
    Left := 16;
    Top := YPos;
    Caption := rsSavedExamples;
    AutoSize := True;
  end;
  Inc(YPos, 24);

  TabControls.LblExamplesSummary := TLabel.Create(AParent);
  with TabControls.LblExamplesSummary do
  begin
    Parent := AParent;
    Left := 16;
    Top := YPos;
    Width := 600;
    Anchors := [akTop, akLeft];
    Caption := rsNoExamples;
    AutoSize := False;
  end;
  TabControls.LblExamplesSummary.Height := 24;
  Inc(YPos, 32);

  TabControls.BtnManageExamples := TButton.Create(AParent);
  with TabControls.BtnManageExamples do
  begin
    Parent := AParent;
    Left := 16;
    Top := YPos;
    Width := 192;
    Height := 32;
    Caption := rsBtnManageExamples;
    Tag := TabIdx;
    OnClick := @OnManageExamplesClick;
  end;

  Inc(YPos, 40);
end;

procedure TFrmLLMOptions.AddPromptButtonsAndFormatInfo(AParent: TWinControl;
  var YPos: Integer; TabIdx: Integer; var TabControls: TFlowTabControls;
  const AOutputFormatInfo: string);
begin
  TabControls.BtnViewDefault := TButton.Create(AParent);
  with TabControls.BtnViewDefault do
  begin
    Parent := AParent;
    Left := 16;
    Top := YPos;
    Width := 224;
    Height := 32;
    Caption := rsBtnViewDefault;
    Tag := TabIdx;
    OnClick := @OnViewDefaultClick;
  end;

  TabControls.BtnReset := TButton.Create(AParent);
  with TabControls.BtnReset do
  begin
    Parent := AParent;
    Left := 248;
    Top := YPos;
    Width := 176;
    Height := 32;
    Caption := rsBtnReset;
    Tag := TabIdx;
    OnClick := @OnResetClick;
  end;
  Inc(YPos, 40);

  TabControls.LblFormatInfo := TLabel.Create(AParent);
  with TabControls.LblFormatInfo do
  begin
    Parent := AParent;
    Left := 16;
    Top := YPos;
    Width := 600;
    Anchors := [akTop, akLeft];
    Caption := Format(rsFormatInfoFmt, [AOutputFormatInfo]);
    WordWrap := True;
    Font.Style := [fsItalic];
    AutoSize := True;
  end;
  Inc(YPos, TabControls.LblFormatInfo.Height + 16);
end;

procedure TFrmLLMOptions.CreateAssociationTab(ATabSheet: TTabSheet; TabIdx: Integer);
var
  flowConfig: IAIFlowConfig;
  tabControls: TFlowTabControls;
  yPos: Integer;
begin
  flowConfig := GetFlowConfigById('association');
  if not Assigned(flowConfig) then Exit;

  tabControls := Default(TFlowTabControls);
  tabControls.TabSheet := ATabSheet;
  tabControls.FlowId := 'association';
  tabControls.ScrollBox := CreateFlowScrollBox(ATabSheet, TabIdx);
  yPos := 16;

  AddFlowDescription(tabControls.ScrollBox, yPos, tabControls.LblDescription,
    flowConfig.GetDescription);
  AddInstructionsMemo(tabControls.ScrollBox, yPos, tabControls.LblInstructions,
    tabControls.MemoInstructions,
    ai_association_flow.SConfigAdditionalInstructionsDisplayName,
    ai_association_flow.SConfigAdditionalInstructionsDescription);

  tabControls.ChkIncludeComments := TCheckBox.Create(tabControls.ScrollBox);
  with tabControls.ChkIncludeComments do
  begin
    Parent := tabControls.ScrollBox;
    Left := 16;
    Top := yPos;
    Width := 600;
    Anchors := [akTop, akLeft];
    Caption := ai_association_flow.SConfigIncludeCommentsDisplayName;
    Hint := ai_association_flow.SConfigIncludeCommentsDescription;
    ShowHint := True;
  end;
  Inc(yPos, 32);

  AddProviderOverrideSection(tabControls.ScrollBox, yPos, TabIdx, tabControls);
  AddExamplesSection(tabControls.ScrollBox, yPos, TabIdx, tabControls);
  AddPromptButtonsAndFormatInfo(tabControls.ScrollBox, yPos, TabIdx, tabControls,
    flowConfig.GetOutputFormatInfo);

  FFlowTabs[TabIdx] := tabControls;
  UpdateFlowOverrideControls(TabIdx);
  ResizeFlowTabControls(TabIdx);
  OutputDebugString(PChar('DEBUG: CreateAssociationTab complete'));
end;

procedure TFrmLLMOptions.CreateTranslationTab(ATabSheet: TTabSheet; TabIdx: Integer);
var
  flowConfig: IAIFlowConfig;
  tabControls: TFlowTabControls;
  yPos: Integer;
begin
  flowConfig := GetFlowConfigById('translation');
  if not Assigned(flowConfig) then Exit;

  tabControls := Default(TFlowTabControls);
  tabControls.TabSheet := ATabSheet;
  tabControls.FlowId := 'translation';
  tabControls.ScrollBox := CreateFlowScrollBox(ATabSheet, TabIdx);
  yPos := 16;

  AddFlowDescription(tabControls.ScrollBox, yPos, tabControls.LblDescription,
    flowConfig.GetDescription);

  tabControls.EditTargetLanguage := TLabeledEdit.Create(tabControls.ScrollBox);
  with tabControls.EditTargetLanguage do
  begin
    Parent := tabControls.ScrollBox;
    Left := 16;
    Top := yPos + 20;
    Width := 600;
    Height := 32;
    Anchors := [akTop, akLeft];
    EditLabel.Caption := ai_translation_flow.SConfigTargetLanguageDisplayName;
    LabelPosition := lpAbove;
    LabelSpacing := 3;
    Hint := ai_translation_flow.SConfigTargetLanguageDescription;
    ShowHint := True;
  end;
  Inc(yPos, 58);

  AddInstructionsMemo(tabControls.ScrollBox, yPos, tabControls.LblInstructions,
    tabControls.MemoInstructions,
    ai_translation_flow.SConfigAdditionalInstructionsDisplayName,
    ai_translation_flow.SConfigAdditionalInstructionsDescription);

  tabControls.ChkForceOverwrite := TCheckBox.Create(tabControls.ScrollBox);
  with tabControls.ChkForceOverwrite do
  begin
    Parent := tabControls.ScrollBox;
    Left := 16;
    Top := yPos;
    Width := 600;
    Anchors := [akTop, akLeft];
    Caption := ai_translation_flow.SConfigForceOverwriteDisplayName;
    Hint := ai_translation_flow.SConfigForceOverwriteDescription;
    ShowHint := True;
  end;
  Inc(yPos, 32);

  tabControls.ChkIncludeAssociations := TCheckBox.Create(tabControls.ScrollBox);
  with tabControls.ChkIncludeAssociations do
  begin
    Parent := tabControls.ScrollBox;
    Left := 16;
    Top := yPos;
    Width := 600;
    Anchors := [akTop, akLeft];
    Caption := ai_translation_flow.SConfigIncludeAssociationsDisplayName;
    Hint := ai_translation_flow.SConfigIncludeAssociationsDescription;
    ShowHint := True;
  end;
  Inc(yPos, 32);

  tabControls.ChkMarkAddedWords := TCheckBox.Create(tabControls.ScrollBox);
  with tabControls.ChkMarkAddedWords do
  begin
    Parent := tabControls.ScrollBox;
    Left := 16;
    Top := yPos;
    Width := 600;
    Anchors := [akTop, akLeft];
    Caption := ai_translation_flow.SConfigMarkAddedWordsDisplayName;
    Hint := ai_translation_flow.SConfigMarkAddedWordsDescription;
    ShowHint := True;
  end;
  Inc(yPos, 32);

  AddProviderOverrideSection(tabControls.ScrollBox, yPos, TabIdx, tabControls);
  AddExamplesSection(tabControls.ScrollBox, yPos, TabIdx, tabControls);
  AddPromptButtonsAndFormatInfo(tabControls.ScrollBox, yPos, TabIdx, tabControls,
    flowConfig.GetOutputFormatInfo);

  FFlowTabs[TabIdx] := tabControls;
  UpdateFlowOverrideControls(TabIdx);
  ResizeFlowTabControls(TabIdx);
  OutputDebugString(PChar('DEBUG: CreateTranslationTab complete'));
end;

procedure TFrmLLMOptions.SetFlowProviderValues(TabIdx: Integer;
  const AProvider, AModel, AApiKey: string);
begin
  if (TabIdx < 0) or (TabIdx > High(FFlowTabs)) then Exit;

  if Assigned(FFlowTabs[TabIdx].CbProvider) then
    FFlowTabs[TabIdx].CbProvider.Text := AProvider;

  if Assigned(FFlowTabs[TabIdx].CbModel) then
  begin
    FFlowTabs[TabIdx].CbModel.Items.BeginUpdate;
    try
      FFlowTabs[TabIdx].CbModel.Items.Clear;
      FillSupportedModelNames(FFlowTabs[TabIdx].CbModel.Items, AProvider);
    finally
      FFlowTabs[TabIdx].CbModel.Items.EndUpdate;
    end;
    FFlowTabs[TabIdx].CbModel.Text := AModel;
  end;

  if Assigned(FFlowTabs[TabIdx].EditApiKey) then
    FFlowTabs[TabIdx].EditApiKey.Text := AApiKey;
end;

procedure TFrmLLMOptions.OnFlowProviderChange(Sender: TObject);
var
  cbProvider, LcbModel: TComboBox;
  tabIdx: Integer;
  provider: string;
begin
  if not (Sender is TComboBox) then Exit;
  cbProvider := TComboBox(Sender);
  tabIdx := cbProvider.Tag;
  if (tabIdx < 0) or (tabIdx > High(FFlowTabs)) then Exit;

  provider := cbProvider.Text;
  LcbModel := FFlowTabs[tabIdx].CbModel;
  if Assigned(LcbModel) then
  begin
    LcbModel.Items.BeginUpdate;
    try
      LcbModel.Items.Clear;
      FillSupportedModelNames(LcbModel.Items, provider);
      if LcbModel.Items.Count > 0 then
        LcbModel.ItemIndex := 0;
    finally
      LcbModel.Items.EndUpdate;
    end;
  end;
end;

procedure TFrmLLMOptions.LoadFlowConfig(const AFlowId: string);
var
  tabIdx: Integer;
  providerValue, modelValue, apiKeyValue, overrideEnabledValue: string;
  hasStoredOverride: Boolean;
  LegacyPrompt: string;
  FlowConfig: IAIFlowConfig;

  function StoredOrDefault(const AKey, ADefault: string): string;
  begin
    Result := FProject.GetInfo(AKey);
    if Trim(Result) = '' then
      Result := ADefault;
  end;

begin
  if not Assigned(FProject) then Exit;

  tabIdx := GetFlowTabIndexByFlowId(AFlowId);
  if tabIdx < 0 then Exit;

  providerValue := '';
  modelValue := '';
  apiKeyValue := '';

  if SameText(AFlowId, 'association') then
  begin
    if Assigned(FFlowTabs[tabIdx].MemoInstructions) then
      FFlowTabs[tabIdx].MemoInstructions.Lines.Text := StoredOrDefault(
        AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS,
        DEFAULT_ASSOCIATION_CUSTOM_INSTRUCTIONS);
    if FProject.GetInfo(AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS) = '' then
    begin
      LegacyPrompt := FProject.GetInfo(LEGACY_PROMPT_KEY);
      if LegacyPrompt <> '' then
      begin
        FlowConfig := GetFlowConfigById('association');
        LegacyPrompt := ExtractCustomInstructions(LegacyPrompt,
          FlowConfig.BuildPromptFromConfig([]));
        if LegacyPrompt <> '' then
          FFlowTabs[tabIdx].MemoInstructions.Lines.Text := LegacyPrompt;
      end;
    end;

    if Assigned(FFlowTabs[tabIdx].ChkIncludeComments) then
      FFlowTabs[tabIdx].ChkIncludeComments.Checked :=
        LowerCase(StoredOrDefault(AI_ASSOCIATION_CONFIG_KEY_INCLUDE_COMMENTS, 'false')) = 'true';

    providerValue := FProject.GetInfo(AI_ASSOCIATION_CONFIG_KEY_PROVIDER);
    modelValue := FProject.GetInfo(AI_ASSOCIATION_CONFIG_KEY_MODEL);
    apiKeyValue := LoadCredential(acsAssociation, FFlowTabs[tabIdx].EditApiKey);
  end
  else if SameText(AFlowId, 'translation') then
  begin
    if Assigned(FFlowTabs[tabIdx].EditTargetLanguage) then
      FFlowTabs[tabIdx].EditTargetLanguage.Text :=
        StoredOrDefault(AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE, '');

    if Assigned(FFlowTabs[tabIdx].MemoInstructions) then
      FFlowTabs[tabIdx].MemoInstructions.Lines.Text := StoredOrDefault(
        AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS,
        DEFAULT_TRANSLATION_CUSTOM_INSTRUCTIONS);

    if Assigned(FFlowTabs[tabIdx].ChkForceOverwrite) then
      FFlowTabs[tabIdx].ChkForceOverwrite.Checked :=
        LowerCase(StoredOrDefault(AI_TRANSLATION_CONFIG_KEY_FORCE_OVERWRITE, 'false')) = 'true';

    if Assigned(FFlowTabs[tabIdx].ChkIncludeAssociations) then
      FFlowTabs[tabIdx].ChkIncludeAssociations.Checked :=
        LowerCase(StoredOrDefault(AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS, 'false')) = 'true';

    if Assigned(FFlowTabs[tabIdx].ChkMarkAddedWords) then
      FFlowTabs[tabIdx].ChkMarkAddedWords.Checked :=
        LowerCase(StoredOrDefault(AI_TRANSLATION_CONFIG_KEY_MARK_ADDED_WORDS, 'false')) = 'true';

    providerValue := FProject.GetInfo(AI_TRANSLATION_CONFIG_KEY_PROVIDER);
    modelValue := FProject.GetInfo(AI_TRANSLATION_CONFIG_KEY_MODEL);
    apiKeyValue := LoadCredential(acsTranslation, FFlowTabs[tabIdx].EditApiKey);
  end
  else
    Exit;

  SetFlowProviderValues(tabIdx, providerValue, modelValue, apiKeyValue);

  hasStoredOverride := (Trim(providerValue) <> '') or
    (Trim(modelValue) <> '') or (Trim(apiKeyValue) <> '');
  if SameText(AFlowId, 'association') then
    hasStoredOverride := hasStoredOverride or FProject.HasAIKey(acsAssociation)
  else
    hasStoredOverride := hasStoredOverride or FProject.HasAIKey(acsTranslation);
  overrideEnabledValue := Trim(FProject.GetInfo(GetFlowOverrideEnabledConfigKey(AFlowId)));
  if Assigned(FFlowTabs[tabIdx].chkUseOverride) then
  begin
    if overrideEnabledValue = '' then
      FFlowTabs[tabIdx].chkUseOverride.Checked := hasStoredOverride
    else
      FFlowTabs[tabIdx].chkUseOverride.Checked := SameText(overrideEnabledValue, 'true') or
        (overrideEnabledValue = '1') or SameText(overrideEnabledValue, 'yes');
  end;
  UpdateFlowOverrideControls(tabIdx);

  FFlowTabs[tabIdx].WorkingExamples := FProject.GetFlowExamples(AFlowId);
  RefreshFlowExamplesUI(AFlowId);
end;

procedure TFrmLLMOptions.SaveFlowConfig(const AFlowId: string);
var
  tabControls: TFlowTabControls;
  tabIdx: Integer;
begin
  if not Assigned(FProject) then Exit;

  tabIdx := GetFlowTabIndexByFlowId(AFlowId);
  if tabIdx < 0 then Exit;
  tabControls := FFlowTabs[tabIdx];

  FProject.SetInfo(
    GetFlowOverrideEnabledConfigKey(AFlowId),
    BoolToStr(Assigned(tabControls.chkUseOverride) and tabControls.chkUseOverride.Checked, True)
  );

  if SameText(AFlowId, 'association') then
  begin
    if Assigned(tabControls.MemoInstructions) then
      FProject.SetInfo(AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS,
        tabControls.MemoInstructions.Lines.Text);
    if Assigned(tabControls.ChkIncludeComments) then
      FProject.SetInfo(AI_ASSOCIATION_CONFIG_KEY_INCLUDE_COMMENTS,
        BoolToStr(tabControls.ChkIncludeComments.Checked, True));
    if Assigned(tabControls.CbProvider) then
      FProject.SetInfo(AI_ASSOCIATION_CONFIG_KEY_PROVIDER, tabControls.CbProvider.Text);
    if Assigned(tabControls.CbModel) then
      FProject.SetInfo(AI_ASSOCIATION_CONFIG_KEY_MODEL, tabControls.CbModel.Text);
    if Assigned(tabControls.EditApiKey) then
      SaveCredential(acsAssociation, tabControls.EditApiKey);
  end
  else if SameText(AFlowId, 'translation') then
  begin
    if Assigned(tabControls.EditTargetLanguage) then
      FProject.SetInfo(AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE,
        tabControls.EditTargetLanguage.Text);
    if Assigned(tabControls.MemoInstructions) then
      FProject.SetInfo(AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS,
        tabControls.MemoInstructions.Lines.Text);
    if Assigned(tabControls.ChkForceOverwrite) then
      FProject.SetInfo(AI_TRANSLATION_CONFIG_KEY_FORCE_OVERWRITE,
        BoolToStr(tabControls.ChkForceOverwrite.Checked, True));
    if Assigned(tabControls.ChkIncludeAssociations) then
      FProject.SetInfo(AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS,
        BoolToStr(tabControls.ChkIncludeAssociations.Checked, True));
    if Assigned(tabControls.ChkMarkAddedWords) then
      FProject.SetInfo(AI_TRANSLATION_CONFIG_KEY_MARK_ADDED_WORDS,
        BoolToStr(tabControls.ChkMarkAddedWords.Checked, True));
    if Assigned(tabControls.CbProvider) then
      FProject.SetInfo(AI_TRANSLATION_CONFIG_KEY_PROVIDER, tabControls.CbProvider.Text);
    if Assigned(tabControls.CbModel) then
      FProject.SetInfo(AI_TRANSLATION_CONFIG_KEY_MODEL, tabControls.CbModel.Text);
    if Assigned(tabControls.EditApiKey) then
      SaveCredential(acsTranslation, tabControls.EditApiKey);
  end
  else
    Exit;

  FProject.SetFlowExamples(AFlowId, FFlowTabs[tabIdx].WorkingExamples);
end;

procedure TFrmLLMOptions.OnFlowOverrideToggle(Sender: TObject);
var
  chk: TCheckBox;
begin
  if not (Sender is TCheckBox) then Exit;
  chk := TCheckBox(Sender);
  UpdateFlowOverrideControls(chk.Tag);
end;

procedure TFrmLLMOptions.UpdateFlowOverrideControls(TabIdx: Integer);
begin
  if (TabIdx < 0) or (TabIdx > High(FFlowTabs)) then Exit;
  if not Assigned(FFlowTabs[TabIdx].chkUseOverride) then Exit;
  if not Assigned(FFlowTabs[TabIdx].gbOverride) then Exit;

  FFlowTabs[TabIdx].gbOverride.Enabled := FFlowTabs[TabIdx].chkUseOverride.Checked;
end;

procedure TFrmLLMOptions.OnManageExamplesClick(Sender: TObject);
var
  btn: TButton;
  idx: Integer;
begin
  if not (Sender is TButton) then Exit;
  if not Assigned(FProject) then Exit;

  btn := TButton(Sender);
  idx := btn.Tag;
  if (idx < 0) or (idx >= Length(FFlowTabs)) then Exit;

  if TFrmFlowExamples.Execute(Self, FProject, FFlowTabs[idx].FlowId,
      FFlowTabs[idx].WorkingExamples) then
    RefreshFlowExamplesUI(FFlowTabs[idx].FlowId);
end;

procedure TFrmLLMOptions.RefreshFlowExamplesUI(const AFlowId: string);
var
  idx, count: Integer;
begin
  idx := GetFlowTabIndexByFlowId(AFlowId);
  if idx < 0 then Exit;

  count := Length(FFlowTabs[idx].WorkingExamples);
  if Assigned(FFlowTabs[idx].LblExamplesSummary) then
  begin
    if count = 0 then
      FFlowTabs[idx].LblExamplesSummary.Caption := rsNoExamples
    else
      FFlowTabs[idx].LblExamplesSummary.Caption := Format(rsExamplesSummaryFmt, [count]);
  end;
  if Assigned(FFlowTabs[idx].BtnManageExamples) then
    FFlowTabs[idx].BtnManageExamples.Enabled := Assigned(FProject);
end;

procedure TFrmLLMOptions.OnViewDefaultClick(Sender: TObject);
var
  btn: TButton;
  flowConfig: IAIFlowConfig;
  frm: TForm;
  memo: TMemo;
  btnClose: TButton;
  currentPrompt: string;
begin
  if not (Sender is TButton) then Exit;

  btn := TButton(Sender);
  if (btn.Tag < 0) or (btn.Tag >= Length(FFlowTabs)) then Exit;

  flowConfig := GetFlowConfigById(FFlowTabs[btn.Tag].FlowId);
  if not Assigned(flowConfig) then Exit;

  currentPrompt := BuildCurrentPrompt(btn.Tag);

  // Create dialog
  frm := TForm.Create(nil);
  try
    ApplyFormStyle(frm);
    frm.Caption := Format(rsDefaultPromptTitle, [flowConfig.GetDisplayName]);
    frm.Width := 700;
    frm.Height := 500;
    frm.Position := poMainFormCenter;

    memo := TMemo.Create(frm);
    memo.Parent := frm;
    memo.Left := 16;
    memo.Top := 16;
    memo.Width := frm.ClientWidth - 32;
    memo.Height := frm.ClientHeight - 80;
    memo.Anchors := [akTop, akLeft, akRight, akBottom];
    memo.ReadOnly := True;
    memo.ScrollBars := ssAutoBoth;
    memo.WordWrap := True;
    memo.Lines.Text := currentPrompt;

    btnClose := TButton.Create(frm);
    btnClose.Parent := frm;
    btnClose.Caption := rsBtnClose;
    btnClose.Width := 112;
    btnClose.Height := 32;
    btnClose.Left := frm.ClientWidth - btnClose.Width - 16;
    btnClose.Top := frm.ClientHeight - btnClose.Height - 16;
    btnClose.Anchors := [akRight, akBottom];
    btnClose.ModalResult := mrOK;

    btnClose.Cancel := True;
    ApplyDialogStyle(frm);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TFrmLLMOptions.OnResetClick(Sender: TObject);
var
  btn: TButton;
  tabControls: TFlowTabControls;
begin
  if not (Sender is TButton) then Exit;

  btn := TButton(Sender);
  if (btn.Tag < 0) or (btn.Tag >= Length(FFlowTabs)) then Exit;

  tabControls := FFlowTabs[btn.Tag];

  if SameText(tabControls.FlowId, 'association') then
  begin
    if Assigned(tabControls.MemoInstructions) then
      tabControls.MemoInstructions.Lines.Text := DEFAULT_ASSOCIATION_CUSTOM_INSTRUCTIONS;
    if Assigned(tabControls.ChkIncludeComments) then
      tabControls.ChkIncludeComments.Checked := False;
  end
  else if SameText(tabControls.FlowId, 'translation') then
  begin
    if Assigned(tabControls.EditTargetLanguage) then
      tabControls.EditTargetLanguage.Text := '';
    if Assigned(tabControls.MemoInstructions) then
      tabControls.MemoInstructions.Lines.Text := DEFAULT_TRANSLATION_CUSTOM_INSTRUCTIONS;
    if Assigned(tabControls.ChkForceOverwrite) then
      tabControls.ChkForceOverwrite.Checked := False;
    if Assigned(tabControls.ChkIncludeAssociations) then
      tabControls.ChkIncludeAssociations.Checked := False;
    if Assigned(tabControls.ChkMarkAddedWords) then
      tabControls.ChkMarkAddedWords.Checked := False;
  end;

  SetFlowProviderValues(btn.Tag, '', '', '');

  if Assigned(tabControls.chkUseOverride) then
  begin
    tabControls.chkUseOverride.Checked := False;
    UpdateFlowOverrideControls(btn.Tag);
  end;
end;

function TFrmLLMOptions.BuildCurrentPrompt(TabIdx: Integer): string;
var
  tabControls: TFlowTabControls;
  associationTabControls: TFlowTabControls;
  flowConfig: IAIFlowConfig;
  customInstructions, serializedExamples: string;
  associationInstructions, serializedAssociationExamples: string;
  includeComments, includeAssociations, markAddedWords: Boolean;
begin
  Result := '';
  if (TabIdx < 0) or (TabIdx >= Length(FFlowTabs)) then Exit;

  tabControls := FFlowTabs[TabIdx];
  flowConfig := GetFlowConfigById(tabControls.FlowId);
  if not Assigned(flowConfig) then Exit;

  customInstructions := '';
  if Assigned(tabControls.MemoInstructions) then
    customInstructions := tabControls.MemoInstructions.Lines.Text;
  serializedExamples := SaveFlowExamples(tabControls.WorkingExamples);

  if SameText(tabControls.FlowId, 'association') then
  begin
    includeComments := Assigned(tabControls.ChkIncludeComments) and
      tabControls.ChkIncludeComments.Checked;
    Result := flowConfig.BuildPromptFromConfig([
      AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS, customInstructions,
      AI_ASSOCIATION_CONFIG_KEY_INCLUDE_COMMENTS, includeComments,
      AI_ASSOCIATION_EXAMPLES_KEY, serializedExamples
    ]);
  end
  else if SameText(tabControls.FlowId, 'translation') then
  begin
    includeAssociations := Assigned(tabControls.ChkIncludeAssociations) and
      tabControls.ChkIncludeAssociations.Checked;
    markAddedWords := Assigned(tabControls.ChkMarkAddedWords) and
      tabControls.ChkMarkAddedWords.Checked;

    associationTabControls := GetFlowTabByFlowId('association');
    associationInstructions := '';
    serializedAssociationExamples := '';
    if includeAssociations then
    begin
      if Assigned(associationTabControls.MemoInstructions) then
        associationInstructions := associationTabControls.MemoInstructions.Lines.Text;
      serializedAssociationExamples :=
        SaveFlowExamples(associationTabControls.WorkingExamples);
    end;

    Result := flowConfig.BuildPromptFromConfig([
      AI_TRANSLATION_CONFIG_KEY_TARGET_LANGUAGE, tabControls.EditTargetLanguage.Text,
      AI_TRANSLATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS, customInstructions,
      AI_TRANSLATION_CONFIG_KEY_INCLUDE_ASSOCIATIONS, includeAssociations,
      AI_TRANSLATION_CONFIG_KEY_MARK_ADDED_WORDS, markAddedWords,
      AI_TRANSLATION_EXAMPLES_KEY, serializedExamples,
      AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS, associationInstructions,
      AI_ASSOCIATION_EXAMPLES_KEY, serializedAssociationExamples
    ]);
  end;
end;

function TFrmLLMOptions.GetFlowTabByFlowId(const AFlowId: string): TFlowTabControls;
var
  i: Integer;
begin
  Result := Default(TFlowTabControls);
  for i := 0 to High(FFlowTabs) do
  begin
    if FFlowTabs[i].FlowId = AFlowId then
    begin
      Result := FFlowTabs[i];
      Exit;
    end;
  end;
end;

function TFrmLLMOptions.GetFlowTabIndexByFlowId(const AFlowId: string): Integer;
begin
  Result := -1;
  for Result := 0 to High(FFlowTabs) do
    if FFlowTabs[Result].FlowId = AFlowId then
      Exit;
  Result := -1;
end;

procedure TFrmLLMOptions.SetProject(AProject: TProject;
  const AGlobalFileName: string; const AStore: IAICredentialStore);
var Setting: TAICommonSetting; I: Integer;
begin
  FProject := AProject;
  FLoadingSettings := True;
  try
    FreeAndNil(FGlobalSettings);
    FGlobalSettings := TAIGlobalSettings.Create(AGlobalFileName, AStore);
    RefreshLocalizedCaptions;
    FProjectSettings := Default(TAIProjectAISettings);
    for Setting in TAICommonSetting do FProjectSettings.Inherit[Setting] := True;
    FProjectSettings.InheritApiKey := True;
    ReloadModelRegistry;
    FillSupportedModelNames(cbModel.Items);
    FillSupportedModelNames(cbGlobalModel.Items);
    LoadGlobalSettings;
    tsProject.TabVisible := Assigned(FProject);
    tsAssociation.TabVisible := Assigned(FProject);
    tsTranslation.TabVisible := Assigned(FProject);
    if Assigned(FProject) then
    begin
      LoadFromProject;
      pcFlows.ActivePage := tsProject;
    end
    else
    begin
      leApiKey.Clear;
      FLoadedApiKeys[acsCommon] := '';
      FLoadedApiKeys[acsAssociation] := '';
      FLoadedApiKeys[acsTranslation] := '';
      cbModel.Clear;
      for I := 0 to High(FFlowTabs) do
      begin
        FFlowTabs[I].WorkingExamples := nil;
        if Assigned(FFlowTabs[I].EditApiKey) then FFlowTabs[I].EditApiKey.Clear;
        if Assigned(FFlowTabs[I].MemoInstructions) then FFlowTabs[I].MemoInstructions.Clear;
      end;
      pcFlows.ActivePage := tsGlobal;
    end;
  finally
    FLoadingSettings := False;
  end;
  UpdateTestEnabled;
end;

destructor TFrmLLMOptions.Destroy;
begin
  FreeAndNil(FGlobalSettings);
  inherited Destroy;
end;

procedure TFrmLLMOptions.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
end;


initialization
  {$IFNDEF FPCUNIT}
    {$R formllmoptions.lfm}
  {$ENDIF}

end.

