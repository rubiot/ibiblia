unit FormLlmOptions;

{$mode ObjFPC}{$H+}

interface

uses
  ui_style, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Project, llmclient, ai_flows, ai_flow_config,
  ai_credential_store
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
    tsAssociation: TTabSheet;
    tsTranslation: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    FProject: TProject;
    FFlowTabs: array of TFlowTabControls;
    FLoadedApiKeys: array[TAICredentialScope] of string;
    function LoadCredential(Scope: TAICredentialScope; Edit: TLabeledEdit): string;
    procedure SaveCredential(Scope: TAICredentialScope; Edit: TLabeledEdit);
    procedure UpdateTestEnabled;
    procedure OnFieldsChanged(Sender: TObject);
    procedure LoadFromProject;
    procedure SaveToProject;
    procedure MigrateLegacyConfigIfNeeded;
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
    procedure SetProject(AProject: TProject);
  public
    procedure AfterConstruction; override;
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

implementation

uses
  ai_association_flow, ai_translation_flow, FormFlowExamples;

{ TFrmLLMOptions }

procedure TFrmLLMOptions.FormCreate(Sender: TObject);
begin
  ApplyFormStyle(Self);
  // Set password character for global API key
  leApiKey.PasswordChar := '*';

  // Populate supported model names
  ReloadModelRegistry;
  FillSupportedModelNames(cbModel.Items);

  // Localize captions
  Caption := rsLlmOptionsCaption;
  LabelModel.Caption := rsLabelModel;
  leApiKey.EditLabel.Caption := rsLabelApiKey;
  gbCommonSettings.Caption := rsCommonSettings;
  leMaxParallel.EditLabel.Caption := rsLabelMaxParallel;
  leMaxRetries.EditLabel.Caption := rsLabelMaxRetries;
  btnOK.Caption := rsBtnOK;
  btnCancel.Caption := rsBtnCancel;
  btnTest.Caption := rsBtnTest;

  // React to edits
  cbModel.OnChange := @OnFieldsChanged;
  leApiKey.OnChange := @OnFieldsChanged;
  leMaxParallel.OnChange := @OnFieldsChanged;
  leMaxRetries.OnChange := @OnFieldsChanged;

  // Initialize flow tabs
  InitializeFlowTabs;

  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.btnOKClick(Sender: TObject);
begin
  SaveToProject;
  ModalResult := mrOK;
end;

procedure TFrmLLMOptions.btnTestClick(Sender: TObject);
var
  client: TBaseLLMClient;
  resp: TLLMResponse;
  testPrompt: string;
  msgs: array[0..1] of TLLMMessage;
  oldCursor: TCursor;
begin
  testPrompt := rsFallbackTestPrompt;
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  btnTest.Enabled := False;
  try
    Application.ProcessMessages;
    client := CreateBackend(cbModel.Text, leApiKey.Text);
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
  btnTest.Enabled := True;
end;

procedure TFrmLLMOptions.UpdateTestEnabled;
begin
  // Prompt is optional for testing (we use a fallback when empty)
  btnTest.Enabled := (Trim(cbModel.Text) <> '') and (Trim(leApiKey.Text) <> '');
end;

procedure TFrmLLMOptions.OnFieldsChanged(Sender: TObject);
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

  // Try to migrate legacy config if present
  MigrateLegacyConfigIfNeeded;

  // Load common settings
  cbModel.Text := FProject.GetInfo(AI_COMMON_CONFIG.Model);
  leApiKey.Text := LoadCredential(acsCommon, leApiKey);

  leMaxParallel.Text := FProject.GetInfo(AI_COMMON_CONFIG.MaxParallel);
  if Trim(leMaxParallel.Text) = '' then leMaxParallel.Text := '2';

  leMaxRetries.Text := FProject.GetInfo(AI_COMMON_CONFIG.MaxRetries);
  if Trim(leMaxRetries.Text) = '' then leMaxRetries.Text := '3';

  // Load each flow configuration
  LoadFlowConfig('association');
  LoadFlowConfig('translation');

  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.SaveToProject;
begin
  if not Assigned(FProject) then Exit;

  // Save common settings
  FProject.SetInfo(AI_COMMON_CONFIG.Model, cbModel.Text);
  SaveCredential(acsCommon, leApiKey);

  if StrToIntDef(leMaxParallel.Text, 0) <= 0 then
    leMaxParallel.Text := '2';
  FProject.SetInfo(AI_COMMON_CONFIG.MaxParallel, leMaxParallel.Text);

  if StrToIntDef(leMaxRetries.Text, 0) < 0 then
    leMaxRetries.Text := '3';
  FProject.SetInfo(AI_COMMON_CONFIG.MaxRetries, leMaxRetries.Text);

  // Save each flow configuration
  SaveFlowConfig('association');
  SaveFlowConfig('translation');
end;

procedure TFrmLLMOptions.MigrateLegacyConfigIfNeeded;
var
  legacyPrompt, customInstructions: string;
  flowConfig: IAIFlowConfig;
  defaultPrompt: string;
begin
  if not Assigned(FProject) then Exit;

  // Check if legacy configuration exists and new one doesn't
  if (FProject.GetInfo(LEGACY_MODEL_KEY) <> '') and
     (FProject.GetInfo(AI_COMMON_CONFIG.Model) = '') then
  begin
    // Migrate common settings
    FProject.SetInfo(AI_COMMON_CONFIG.Model, FProject.GetInfo(LEGACY_MODEL_KEY));
    FProject.SetInfo(AI_COMMON_CONFIG.MaxParallel, FProject.GetInfo(LEGACY_MAX_PARALLEL_KEY));
    FProject.SetInfo(AI_COMMON_CONFIG.MaxRetries, FProject.GetInfo(LEGACY_MAX_RETRIES_KEY));
    FProject.SetInfo(AI_COMMON_CONFIG.RetryDelay, FProject.GetInfo(LEGACY_RETRY_DELAY_KEY));
    FProject.SetInfo(AI_COMMON_CONFIG.AutoSaveInterval, FProject.GetInfo(LEGACY_AUTOSAVE_INTERVAL_KEY));

    // Migrate prompt - extract custom instructions from old full prompt
    legacyPrompt := FProject.GetInfo(LEGACY_PROMPT_KEY);
    if legacyPrompt <> '' then
    begin
      // Get default prompt from association flow to extract custom part
      flowConfig := GetFlowConfigById('association');
      // Build default prompt to compare
      defaultPrompt := flowConfig.BuildPromptFromConfig([]);

      customInstructions := ExtractCustomInstructions(legacyPrompt, defaultPrompt);
      if customInstructions <> '' then
        FProject.SetInfo(AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS, customInstructions)
      else
        FProject.SetInfo(AI_ASSOCIATION_CONFIG_KEY_CUSTOM_INSTRUCTIONS, flowConfig.GetDefaultCustomInstructions);
    end;

    // Non-secret legacy settings remain for compatibility. Credentials migrate
    // separately, after a successful project save and vault verification.
  end;
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
      FFlowTabs[tabIdx].chkUseOverride.Checked := LowerCase(overrideEnabledValue) = 'true';
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

procedure TFrmLLMOptions.SetProject(AProject: TProject);
begin
  FProject := AProject;
  ReloadModelRegistry;
  FillSupportedModelNames(cbModel.Items);
  LoadFromProject;
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

