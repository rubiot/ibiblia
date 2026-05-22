unit FormLlmOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Projeto, llmclient, ai_flows, ai_flow_config
  {$IFDEF WINDOWS}, Windows{$ENDIF};

type
  // Record to store a single field control
  TFieldControl = record
    FieldKey: string;
    FieldType: string;
    Control: TControl;  // Can be TEdit, TMemo, TCheckBox, etc.
  end;

  // Record to store controls for each flow tab
  TFlowTabControls = record
    TabSheet: TTabSheet;
    FlowId: string;
    LblDescription: TLabel;
    FieldControls: array of TFieldControl;  // Dynamic array of field controls
    chkUseOverride: TCheckBox;
    gbOverride: TGroupBox;
    LblExamples: TLabel;
    lbExamples: TListBox;
    BtnRemoveExample: TButton;
    BtnClearExamples: TButton;
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
    FProjeto: TProjeto;
    FFlowTabs: array of TFlowTabControls;
    procedure UpdateTestEnabled;
    procedure OnFieldsChanged(Sender: TObject);
    procedure LoadFromProjeto;
    procedure SaveToProjeto;
    procedure MigrateLegacyConfigIfNeeded;
    procedure InitializeFlowTabs;
    procedure CreateFlowTab(ATabSheet: TTabSheet; const AFlowId: string);
    procedure LoadFlowConfig(const AFlowId: string);
    procedure SaveFlowConfig(const AFlowId: string);
    procedure OnViewDefaultClick(Sender: TObject);
    procedure OnResetClick(Sender: TObject);
    procedure OnRemoveExampleClick(Sender: TObject);
    procedure OnClearExamplesClick(Sender: TObject);
    procedure OnFlowProviderChange(Sender: TObject);
    procedure OnFlowOverrideToggle(Sender: TObject);
    procedure RefreshFlowExamplesUI(const AFlowId: string);
    procedure UpdateFlowOverrideControls(TabIdx: Integer);
    function GetFlowTabByFlowId(const AFlowId: string): TFlowTabControls;
    function GetFlowTabIndexByFlowId(const AFlowId: string): Integer;
  public
    procedure SetProjeto(AProjeto: TProjeto);
  end;

var
  FrmLLMOptions: TFrmLLMOptions;

resourcestring
  rsLlmOptionsCaption = 'LLM Options';
  rsLabelModel = 'Model';
  rsLabelApiKey = 'API key';
  rsBtnOK = 'OK';
  rsBtnCancel = 'Cancel';
  rsBtnTest = 'Test';
  rsTestResultTitle = 'Test result';
  rsTestFailedTitle = 'Test failed';
  rsSystemPrompt = 'You are a helpful assistant ready to assist users with their questions on Bible translation.';
  rsFallbackTestPrompt = 'Tell me something about you. Answer in JSON format.';
  rsLabelMaxParallel = 'Max Parallel';
  rsLabelMaxRetries = 'Max Retries';
  rsCommonSettings = 'Common Settings (All Flows)';
  rsAdditionalInstructions = 'Additional Instructions (Optional):';
  rsBtnViewDefault = 'View Default Prompt';
  rsBtnReset = 'Reset to Default';
  rsSavedExamples = 'Saved Examples:';
  rsBtnRemoveExample = 'Remove Selected';
  rsBtnClearExamples = 'Clear List';
  rsClearExamplesTitle = 'Clear examples';
  rsClearExamplesPrompt = 'Remove all saved examples for the %s flow?';
  rsNoExamples = '(no saved examples)';
  rsDefaultPromptTitle = 'Default Prompt - %s';
  rsCustomInstructionsHint = 'Add specific guidelines for your project (e.g., translation philosophy, examples). Leave empty to use only the default prompt.';
  rsProviderOverride = 'Provider Override (Optional)';
  rsUseFlowOverride = 'Use flow-specific provider/model/API key';

implementation

{ TFrmLLMOptions }

procedure TFrmLLMOptions.FormCreate(Sender: TObject);
begin
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
  SaveToProjeto;
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

procedure TFrmLLMOptions.LoadFromProjeto;
begin
  if not Assigned(FProjeto) then Exit;

  // Try to migrate legacy config if present
  MigrateLegacyConfigIfNeeded;

  // Load common settings
  cbModel.Text := FProjeto.GetInfo(AI_COMMON_CONFIG.Model);
  leApiKey.Text := FProjeto.GetInfo(AI_COMMON_CONFIG.ApiKey);

  leMaxParallel.Text := FProjeto.GetInfo(AI_COMMON_CONFIG.MaxParallel);
  if Trim(leMaxParallel.Text) = '' then leMaxParallel.Text := '2';

  leMaxRetries.Text := FProjeto.GetInfo(AI_COMMON_CONFIG.MaxRetries);
  if Trim(leMaxRetries.Text) = '' then leMaxRetries.Text := '3';

  // Load each flow configuration
  LoadFlowConfig('association');
  LoadFlowConfig('translation');

  UpdateTestEnabled;
end;

procedure TFrmLLMOptions.SaveToProjeto;
begin
  if not Assigned(FProjeto) then Exit;

  // Save common settings
  FProjeto.SetInfo(AI_COMMON_CONFIG.Model, cbModel.Text);
  FProjeto.SetInfo(AI_COMMON_CONFIG.ApiKey, leApiKey.Text);

  if StrToIntDef(leMaxParallel.Text, 0) <= 0 then
    leMaxParallel.Text := '2';
  FProjeto.SetInfo(AI_COMMON_CONFIG.MaxParallel, leMaxParallel.Text);

  if StrToIntDef(leMaxRetries.Text, 0) < 0 then
    leMaxRetries.Text := '3';
  FProjeto.SetInfo(AI_COMMON_CONFIG.MaxRetries, leMaxRetries.Text);

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
  if not Assigned(FProjeto) then Exit;

  // Check if legacy configuration exists and new one doesn't
  if (FProjeto.GetInfo(LEGACY_MODEL_KEY) <> '') and
     (FProjeto.GetInfo(AI_COMMON_CONFIG.Model) = '') then
  begin
    // Migrate common settings
    FProjeto.SetInfo(AI_COMMON_CONFIG.Model, FProjeto.GetInfo(LEGACY_MODEL_KEY));
    FProjeto.SetInfo(AI_COMMON_CONFIG.ApiKey, FProjeto.GetInfo(LEGACY_API_KEY));
    FProjeto.SetInfo(AI_COMMON_CONFIG.MaxParallel, FProjeto.GetInfo(LEGACY_MAX_PARALLEL_KEY));
    FProjeto.SetInfo(AI_COMMON_CONFIG.MaxRetries, FProjeto.GetInfo(LEGACY_MAX_RETRIES_KEY));
    FProjeto.SetInfo(AI_COMMON_CONFIG.RetryDelay, FProjeto.GetInfo(LEGACY_RETRY_DELAY_KEY));
    FProjeto.SetInfo(AI_COMMON_CONFIG.AutoSaveInterval, FProjeto.GetInfo(LEGACY_AUTOSAVE_INTERVAL_KEY));

    // Migrate prompt - extract custom instructions from old full prompt
    legacyPrompt := FProjeto.GetInfo(LEGACY_PROMPT_KEY);
    if legacyPrompt <> '' then
    begin
      // Get default prompt from association flow to extract custom part
      flowConfig := GetFlowConfigById('association');
      // Build default prompt to compare
      defaultPrompt := flowConfig.BuildPromptFromConfig([]);

      customInstructions := ExtractCustomInstructions(legacyPrompt, defaultPrompt);
      if customInstructions <> '' then
        FProjeto.SetInfo(flowConfig.GetConfigFields[0].Key, customInstructions)
      else
        FProjeto.SetInfo(flowConfig.GetConfigFields[0].Key, flowConfig.GetDefaultCustomInstructions);
    end;

    // Note: We don't delete legacy keys to maintain compatibility with older versions
    // They will simply be ignored by new code
  end;
end;

procedure TFrmLLMOptions.InitializeFlowTabs;
begin
  // Initialize each flow tab with controls
  SetLength(FFlowTabs, 2);

  // Association flow
  CreateFlowTab(tsAssociation, 'association');

  // Translation flow
  CreateFlowTab(tsTranslation, 'translation');
end;

procedure TFrmLLMOptions.CreateFlowTab(ATabSheet: TTabSheet; const AFlowId: string);
var
  flowConfig: IAIFlowConfig;
  tabControls: TFlowTabControls;
  yPos: Integer;
  idx: Integer;
  configFields: TAIFlowConfigFieldArray;
  i: Integer;
  fieldCtrl: TFieldControl;
  lbl: TLabel;
  edit: TLabeledEdit;
  memo: TMemo;
  checkbox: TCheckBox;
  cb: TComboBox;
  gbProvider: TGroupBox;
  currentParent: TWinControl;
  gbYPos: Integer;
begin
  flowConfig := GetFlowConfigById(AFlowId);
  if not Assigned(flowConfig) then Exit;

  // Find or add to array
  idx := Length(FFlowTabs);
  if AFlowId = 'association' then idx := 0
  else if AFlowId = 'translation' then idx := 1;

  tabControls.TabSheet := ATabSheet;
  tabControls.FlowId := AFlowId;

  yPos := 12;

  // Description label
  tabControls.LblDescription := TLabel.Create(ATabSheet);
  with tabControls.LblDescription do
  begin
    Parent := ATabSheet;
    Left := 8;
    Top := yPos;
    Width := 600;  // Use fixed width initially, Anchors will resize it
    Anchors := [akTop, akLeft, akRight];
    Caption := flowConfig.GetDescription;
    WordWrap := True;
    AutoSize := True;
    Font.Style := [fsBold];
  end;

  // Force layout calculation to get correct height for next controls
  tabControls.LblDescription.AdjustSize;
  Inc(yPos, tabControls.LblDescription.Height + 16);

  // Get configuration fields for this flow
  configFields := flowConfig.GetConfigFields;
  SetLength(tabControls.FieldControls, Length(configFields));

  gbProvider := nil;
  gbYPos := 20;

  // Create controls dynamically for each field
  for i := 0 to High(configFields) do
  begin
    fieldCtrl.FieldKey := configFields[i].Key;
    fieldCtrl.FieldType := configFields[i].FieldType;

    // Decide parent: provider fields go into a GroupBox
    if (configFields[i].FieldType = 'model') or (configFields[i].FieldType = 'password') or (configFields[i].FieldType = 'provider') then
    begin
      if gbProvider = nil then
      begin
        tabControls.chkUseOverride := TCheckBox.Create(ATabSheet);
        tabControls.chkUseOverride.Parent := ATabSheet;
        tabControls.chkUseOverride.Left := 8;
        tabControls.chkUseOverride.Top := yPos;
        tabControls.chkUseOverride.Width := 320;
        tabControls.chkUseOverride.Caption := rsUseFlowOverride;
        tabControls.chkUseOverride.Tag := idx;
        tabControls.chkUseOverride.OnChange := @OnFlowOverrideToggle;
        Inc(yPos, 26);

        gbProvider := TGroupBox.Create(ATabSheet);
        gbProvider.Parent := ATabSheet;
        gbProvider.Left := 8;
        gbProvider.Top := yPos;
        gbProvider.Width := 600;
        gbProvider.Height := 100; // Will adjust
        gbProvider.Anchors := [akTop, akLeft, akRight];
        gbProvider.Caption := rsProviderOverride;
        tabControls.gbOverride := gbProvider;
        Inc(yPos, gbProvider.Height + 10);
      end;
      currentParent := gbProvider;
    end
    else
      currentParent := ATabSheet;

    // Create appropriate control based on field type
    if (configFields[i].FieldType = 'string') or (configFields[i].FieldType = 'password') then
    begin
      // Create TLabeledEdit
      edit := TLabeledEdit.Create(currentParent);
      edit.Parent := currentParent;
      edit.Left := 8;
      
      if currentParent = gbProvider then
      begin
        edit.Top := gbYPos + 20;
        Inc(gbYPos, 50);
        if gbYPos + 15 > gbProvider.Height then gbProvider.Height := gbYPos + 15;
      end
      else
      begin
        Inc(yPos, 20);
        edit.Top := yPos;
        Inc(yPos, 28);
      end;

      edit.Width := currentParent.ClientWidth - 16;
      edit.Height := 21;
      edit.Anchors := [akTop, akLeft, akRight];
      edit.EditLabel.Caption := configFields[i].DisplayName;
      edit.LabelPosition := lpAbove;
      edit.LabelSpacing := 3;
      edit.Hint := configFields[i].Description;
      edit.ShowHint := True;
      if configFields[i].FieldType = 'password' then
        edit.PasswordChar := '*';
      fieldCtrl.Control := edit;
    end
    else if (configFields[i].FieldType = 'model') or (configFields[i].FieldType = 'provider') then
    begin
      // Only create label if it's not the provider field inside the gbProvider groupbox
      // (as the groupbox title 'Provider Override' already describes it)
      lbl := nil;
      if not ((currentParent = gbProvider) and (configFields[i].FieldType = 'provider')) then
      begin
        lbl := TLabel.Create(currentParent);
        lbl.Parent := currentParent;
        lbl.Left := 8;
        lbl.Caption := configFields[i].DisplayName;
      end;
      
      if currentParent = gbProvider then
      begin
        if Assigned(lbl) then
        begin
          lbl.Top := gbYPos;
          cb := TComboBox.Create(currentParent);
          cb.Parent := currentParent;
          cb.Left := 8;
          cb.Top := gbYPos + 16;
          Inc(gbYPos, 50);
        end
        else
        begin
          // If no label, position combo box at the top of current slot
          cb := TComboBox.Create(currentParent);
          cb.Parent := currentParent;
          cb.Left := 8;
          cb.Top := gbYPos;
          Inc(gbYPos, 36);
        end;
        if gbYPos + 15 > gbProvider.Height then gbProvider.Height := gbYPos + 15;
      end
      else
      begin
        lbl.Top := yPos;
        cb := TComboBox.Create(currentParent);
        cb.Parent := currentParent;
        cb.Left := 8;
        cb.Top := yPos + 16;
        Inc(yPos, 45);
      end;

      cb.Width := currentParent.ClientWidth - 16;
      cb.Anchors := [akTop, akLeft, akRight];
      if configFields[i].FieldType = 'provider' then
        cb.Style := csDropDownList
      else
        cb.Style := csDropDown;
      
      if configFields[i].FieldType = 'provider' then
      begin
        FillSupportedProviders(cb.Items);
        cb.OnChange := @OnFlowProviderChange;
        cb.Tag := idx; // Link to tab index
      end
      else
        FillSupportedModelNames(cb.Items);
        
      cb.Hint := configFields[i].Description;
      cb.ShowHint := True;
      if Assigned(lbl) then lbl.Caption := configFields[i].DisplayName;
      fieldCtrl.Control := cb;
    end
    else if configFields[i].FieldType = 'memo' then
    begin
      // Label for memo
      lbl := TLabel.Create(currentParent);
      lbl.Parent := currentParent;
      lbl.Left := 8;
      lbl.Top := yPos;
      lbl.Caption := configFields[i].DisplayName + ':';
      lbl.AutoSize := True;
      Inc(yPos, 20);

      // Create TMemo for memo fields
      memo := TMemo.Create(currentParent);
      memo.Parent := currentParent;
      memo.Left := 8;
      memo.Top := yPos;
      memo.Width := 600;  // Use fixed width, Anchors will resize it
      memo.Height := 100;
      memo.Anchors := [akTop, akLeft, akRight];
      memo.ScrollBars := ssAutoBoth;
      memo.Hint := configFields[i].Description;
      memo.ShowHint := True;
      memo.Visible := True;
      memo.TabStop := True;
      memo.WordWrap := True;
      memo.BorderStyle := bsSingle;  // Ensure border is visible
      memo.Color := clWindow;  // White background
      fieldCtrl.Control := memo;
      Inc(yPos, 108); // Memo (100) + Gap (8)
    end
    else if configFields[i].FieldType = 'boolean' then
    begin
      // Create TCheckBox for boolean fields
      checkbox := TCheckBox.Create(currentParent);
      checkbox.Parent := currentParent;
      checkbox.Left := 8;
      checkbox.Top := yPos;
      checkbox.Width := 600;  // Use fixed width, Anchors will resize it
      checkbox.Caption := configFields[i].DisplayName;
      checkbox.Hint := configFields[i].Description;
      checkbox.ShowHint := True;
      fieldCtrl.Control := checkbox;
      Inc(yPos, 28); // CheckBox (20) + Gap (8)
    end;

    tabControls.FieldControls[i] := fieldCtrl;
  end;

  // Final adjustment of yPos after GroupBox if it was resized
  if gbProvider <> nil then
    yPos := gbProvider.Top + gbProvider.Height + 10;

  Inc(yPos, 8);

  tabControls.LblExamples := TLabel.Create(ATabSheet);
  with tabControls.LblExamples do
  begin
    Parent := ATabSheet;
    Left := 8;
    Top := yPos;
    Caption := rsSavedExamples;
    AutoSize := True;
  end;
  Inc(yPos, 20);

  tabControls.lbExamples := TListBox.Create(ATabSheet);
  with tabControls.lbExamples do
  begin
    Parent := ATabSheet;
    Left := 8;
    Top := yPos;
    Width := 600;
    Height := 88;
    Anchors := [akTop, akLeft, akRight];
    IntegralHeight := False;
  end;
  Inc(yPos, 96);

  tabControls.BtnRemoveExample := TButton.Create(ATabSheet);
  with tabControls.BtnRemoveExample do
  begin
    Parent := ATabSheet;
    Left := 8;
    Top := yPos;
    Width := 120;
    Height := 25;
    Caption := rsBtnRemoveExample;
    Tag := idx;
    OnClick := @OnRemoveExampleClick;
  end;

  tabControls.BtnClearExamples := TButton.Create(ATabSheet);
  with tabControls.BtnClearExamples do
  begin
    Parent := ATabSheet;
    Left := 136;
    Top := yPos;
    Width := 90;
    Height := 25;
    Caption := rsBtnClearExamples;
    Tag := idx;
    OnClick := @OnClearExamplesClick;
  end;

  Inc(yPos, 33);

  // Buttons panel
  tabControls.BtnViewDefault := TButton.Create(ATabSheet);
  with tabControls.BtnViewDefault do
  begin
    Parent := ATabSheet;
    Left := 8;
    Top := yPos;
    Width := 140;
    Height := 25;
    Caption := rsBtnViewDefault;
    Tag := idx; // Store index for identification
    OnClick := @OnViewDefaultClick;
  end;

  tabControls.BtnReset := TButton.Create(ATabSheet);
  with tabControls.BtnReset do
  begin
    Parent := ATabSheet;
    Left := 156;
    Top := yPos;
    Width := 120;
    Height := 25;
    Caption := rsBtnReset;
    Tag := idx; // Store index for identification
    OnClick := @OnResetClick;
  end;

  Inc(yPos, 33);

  // Format info label
  tabControls.LblFormatInfo := TLabel.Create(ATabSheet);
  with tabControls.LblFormatInfo do
  begin
    Parent := ATabSheet;
    Left := 8;
    Top := yPos;
    Width := 600; // Use fixed width initially
    Anchors := [akTop, akLeft, akRight];
    Caption := 'ℹ️ ' + flowConfig.GetOutputFormatInfo;
    WordWrap := True;
    Font.Style := [fsItalic];
    AutoSize := True;
  end;

  FFlowTabs[idx] := tabControls;
  UpdateFlowOverrideControls(idx);

  OutputDebugString(PChar('DEBUG: CreateFlowTab complete for "' + AFlowId + '" - Created ' +
          IntToStr(Length(tabControls.FieldControls)) + ' field controls'));
end;

procedure TFrmLLMOptions.OnFlowProviderChange(Sender: TObject);
var
  cbProvider, LcbModel: TComboBox;
  tabIdx, i: Integer;
  provider: string;
begin
  if not (Sender is TComboBox) then Exit;
  cbProvider := TComboBox(Sender);
  tabIdx := cbProvider.Tag; // We set this in CreateFlowTab to 0 or 1
  provider := cbProvider.Text;

  // Find the model combo in the same tab
  LcbModel := nil;
  for i := 0 to High(FFlowTabs[tabIdx].FieldControls) do
  begin
    if FFlowTabs[tabIdx].FieldControls[i].FieldType = 'model' then
    begin
      LcbModel := TComboBox(FFlowTabs[tabIdx].FieldControls[i].Control);
      Break;
    end;
  end;

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
  flowConfig: IAIFlowConfig;
  tabControls: TFlowTabControls;
  configFields: TAIFlowConfigFieldArray;
  i, tabIdx: Integer;
  fieldValue, providerValue, overrideEnabledValue: string;
  hasStoredOverride: Boolean;
  ctrl: TControl;
  LcbModel: TComboBox;
begin
  if not Assigned(FProjeto) then Exit;

  flowConfig := GetFlowConfigById(AFlowId);
  if not Assigned(flowConfig) then Exit;

  tabControls := GetFlowTabByFlowId(AFlowId);
  if not Assigned(tabControls.TabSheet) then Exit;

  configFields := flowConfig.GetConfigFields;
  hasStoredOverride := False;

  // Find provider value first to filter models
  providerValue := '';
  for i := 0 to High(configFields) do
  begin
    if configFields[i].FieldType = 'provider' then
    begin
      providerValue := FProjeto.GetInfo(configFields[i].Key);
      if providerValue = '' then providerValue := configFields[i].DefaultValue;
      Break;
    end;
  end;

  for i := 0 to High(configFields) do
    if ((configFields[i].FieldType = 'provider') or
        (configFields[i].FieldType = 'model') or
        (configFields[i].FieldType = 'password')) and
      (Trim(FProjeto.GetInfo(configFields[i].Key)) <> '') then
    begin
      hasStoredOverride := True;
      Break;
    end;

  // Load each field value from project
  for i := 0 to High(tabControls.FieldControls) do
  begin
    if i > High(configFields) then Break;

    ctrl := tabControls.FieldControls[i].Control;
    fieldValue := FProjeto.GetInfo(configFields[i].Key);

    // If empty, use default value
    if Trim(fieldValue) = '' then
      fieldValue := configFields[i].DefaultValue;

    // Set value based on control type
    if ctrl is TLabeledEdit then
      TLabeledEdit(ctrl).Text := fieldValue
    else if ctrl is TMemo then
      TMemo(ctrl).Lines.Text := fieldValue
    else if ctrl is TCheckBox then
      TCheckBox(ctrl).Checked := LowerCase(fieldValue) = 'true'
    else if ctrl is TComboBox then
    begin
      LcbModel := TComboBox(ctrl);
      // If it's a model combo, populate it filtered by provider
      if tabControls.FieldControls[i].FieldType = 'model' then
      begin
        LcbModel.Items.BeginUpdate;
        try
          LcbModel.Items.Clear;
          FillSupportedModelNames(LcbModel.Items, providerValue);
        finally
          LcbModel.Items.EndUpdate;
        end;
      end;
      LcbModel.Text := fieldValue;
    end;
  end;

  tabIdx := GetFlowTabIndexByFlowId(AFlowId);
  if tabIdx >= 0 then
  begin
    overrideEnabledValue := Trim(FProjeto.GetInfo(GetFlowOverrideEnabledConfigKey(AFlowId)));
    if overrideEnabledValue = '' then
      FFlowTabs[tabIdx].chkUseOverride.Checked := hasStoredOverride
    else
      FFlowTabs[tabIdx].chkUseOverride.Checked := LowerCase(overrideEnabledValue) = 'true';
    UpdateFlowOverrideControls(tabIdx);

    FFlowTabs[tabIdx].WorkingExamples := FProjeto.GetFlowExamples(AFlowId);
    RefreshFlowExamplesUI(AFlowId);
  end;
end;

procedure TFrmLLMOptions.SaveFlowConfig(const AFlowId: string);
var
  flowConfig: IAIFlowConfig;
  tabControls: TFlowTabControls;
  configFields: TAIFlowConfigFieldArray;
  i, tabIdx: Integer;
  fieldValue: string;
  ctrl: TControl;
begin
  if not Assigned(FProjeto) then Exit;

  flowConfig := GetFlowConfigById(AFlowId);
  if not Assigned(flowConfig) then Exit;

  tabControls := GetFlowTabByFlowId(AFlowId);
  if not Assigned(tabControls.TabSheet) then Exit;

  FProjeto.SetInfo(
    GetFlowOverrideEnabledConfigKey(AFlowId),
    BoolToStr(Assigned(tabControls.chkUseOverride) and tabControls.chkUseOverride.Checked, True)
  );

  configFields := flowConfig.GetConfigFields;

  // Save each field value to project
  for i := 0 to High(tabControls.FieldControls) do
  begin
    if i > High(configFields) then Break;

    ctrl := tabControls.FieldControls[i].Control;
    fieldValue := '';

    // Get value based on control type
    if ctrl is TLabeledEdit then
      fieldValue := TLabeledEdit(ctrl).Text
    else if ctrl is TMemo then
      fieldValue := TMemo(ctrl).Lines.Text
    else if ctrl is TCheckBox then
      fieldValue := BoolToStr(TCheckBox(ctrl).Checked, True)
    else if ctrl is TComboBox then
      fieldValue := TComboBox(ctrl).Text;

    // Save to project
    FProjeto.SetInfo(configFields[i].Key, fieldValue);
  end;

  tabIdx := GetFlowTabIndexByFlowId(AFlowId);
  if tabIdx >= 0 then
    FProjeto.SetFlowExamples(AFlowId, FFlowTabs[tabIdx].WorkingExamples);
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

procedure TFrmLLMOptions.OnRemoveExampleClick(Sender: TObject);
var
  btn: TButton;
  idx, selectedIndex: Integer;
begin
  if not (Sender is TButton) then Exit;

  btn := TButton(Sender);
  idx := btn.Tag;
  if (idx < 0) or (idx >= Length(FFlowTabs)) then Exit;

  selectedIndex := FFlowTabs[idx].lbExamples.ItemIndex;
  if (selectedIndex < 0) or (selectedIndex > High(FFlowTabs[idx].WorkingExamples)) then Exit;

  FFlowTabs[idx].WorkingExamples := ai_flow_config.RemoveFlowExample(
    FFlowTabs[idx].WorkingExamples,
    FFlowTabs[idx].WorkingExamples[selectedIndex].ReferenceId
  );
  RefreshFlowExamplesUI(FFlowTabs[idx].FlowId);
end;

procedure TFrmLLMOptions.OnClearExamplesClick(Sender: TObject);
var
  btn: TButton;
  idx: Integer;
  flowConfig: IAIFlowConfig;
begin
  if not (Sender is TButton) then Exit;

  btn := TButton(Sender);
  idx := btn.Tag;
  if (idx < 0) or (idx >= Length(FFlowTabs)) then Exit;

  flowConfig := GetFlowConfigById(FFlowTabs[idx].FlowId);
  if not Assigned(flowConfig) then Exit;

  if MessageDlg(rsClearExamplesTitle,
                Format(rsClearExamplesPrompt, [flowConfig.GetDisplayName]),
                mtConfirmation,
                [mbYes, mbNo],
                0) <> mrYes then
    Exit;

  FFlowTabs[idx].WorkingExamples := nil;
  RefreshFlowExamplesUI(FFlowTabs[idx].FlowId);
end;

procedure TFrmLLMOptions.RefreshFlowExamplesUI(const AFlowId: string);
var
  idx, i: Integer;
begin
  idx := GetFlowTabIndexByFlowId(AFlowId);
  if idx < 0 then Exit;

  FFlowTabs[idx].lbExamples.Items.BeginUpdate;
  try
    FFlowTabs[idx].lbExamples.Items.Clear;
    if Length(FFlowTabs[idx].WorkingExamples) = 0 then
      FFlowTabs[idx].lbExamples.Items.Add(rsNoExamples)
    else
      for i := 0 to High(FFlowTabs[idx].WorkingExamples) do
        FFlowTabs[idx].lbExamples.Items.Add(FormatFlowExampleSummary(FFlowTabs[idx].WorkingExamples[i]));
  finally
    FFlowTabs[idx].lbExamples.Items.EndUpdate;
  end;

  if Length(FFlowTabs[idx].WorkingExamples) > 0 then
    FFlowTabs[idx].lbExamples.ItemIndex := 0
  else
    FFlowTabs[idx].lbExamples.ItemIndex := -1;
  FFlowTabs[idx].BtnRemoveExample.Enabled := Length(FFlowTabs[idx].WorkingExamples) > 0;
  FFlowTabs[idx].BtnClearExamples.Enabled := Length(FFlowTabs[idx].WorkingExamples) > 0;
end;

procedure TFrmLLMOptions.OnViewDefaultClick(Sender: TObject);
var
  btn: TButton;
  flowConfig: IAIFlowConfig;
  frm: TForm;
  memo: TMemo;
  btnClose: TButton;
  defaultPrompt: string;
begin
  if not (Sender is TButton) then Exit;

  btn := TButton(Sender);
  if (btn.Tag < 0) or (btn.Tag >= Length(FFlowTabs)) then Exit;

  flowConfig := GetFlowConfigById(FFlowTabs[btn.Tag].FlowId);
  if not Assigned(flowConfig) then Exit;

  // Build default prompt
  defaultPrompt := flowConfig.BuildPromptFromConfig([]);

  // Create dialog
  frm := TForm.Create(nil);
  try
    frm.Caption := Format(rsDefaultPromptTitle, [flowConfig.GetDisplayName]);
    frm.Width := 700;
    frm.Height := 500;
    frm.Position := poMainFormCenter;

    memo := TMemo.Create(frm);
    memo.Parent := frm;
    memo.Left := 8;
    memo.Top := 8;
    memo.Width := frm.ClientWidth - 16;
    memo.Height := frm.ClientHeight - 48;
    memo.Anchors := [akTop, akLeft, akRight, akBottom];
    memo.ReadOnly := True;
    memo.ScrollBars := ssAutoBoth;
    memo.WordWrap := True;
    memo.Lines.Text := defaultPrompt;

    btnClose := TButton.Create(frm);
    btnClose.Parent := frm;
    btnClose.Caption := 'Close';
    btnClose.Width := 75;
    btnClose.Height := 25;
    btnClose.Left := frm.ClientWidth - btnClose.Width - 8;
    btnClose.Top := frm.ClientHeight - btnClose.Height - 8;
    btnClose.Anchors := [akRight, akBottom];
    btnClose.ModalResult := mrOK;

    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TFrmLLMOptions.OnResetClick(Sender: TObject);
var
  btn: TButton;
  flowConfig: IAIFlowConfig;
  tabControls: TFlowTabControls;
  configFields: TAIFlowConfigFieldArray;
  i: Integer;
  ctrl: TControl;
begin
  if not (Sender is TButton) then Exit;

  btn := TButton(Sender);
  if (btn.Tag < 0) or (btn.Tag >= Length(FFlowTabs)) then Exit;

  tabControls := FFlowTabs[btn.Tag];
  flowConfig := GetFlowConfigById(tabControls.FlowId);
  if not Assigned(flowConfig) then Exit;

  configFields := flowConfig.GetConfigFields;

  // Reset all fields to their default values
  for i := 0 to High(tabControls.FieldControls) do
  begin
    if i > High(configFields) then Break;

    ctrl := tabControls.FieldControls[i].Control;

    // Reset based on control type
    if ctrl is TLabeledEdit then
      TLabeledEdit(ctrl).Text := configFields[i].DefaultValue
    else if ctrl is TMemo then
      TMemo(ctrl).Lines.Text := configFields[i].DefaultValue
    else if ctrl is TCheckBox then
      TCheckBox(ctrl).Checked := LowerCase(configFields[i].DefaultValue) = 'true'
    else if ctrl is TComboBox then
      TComboBox(ctrl).Text := configFields[i].DefaultValue;
  end;

  if Assigned(tabControls.chkUseOverride) then
  begin
    tabControls.chkUseOverride.Checked := False;
    UpdateFlowOverrideControls(btn.Tag);
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

procedure TFrmLLMOptions.SetProjeto(AProjeto: TProjeto);
begin
  FProjeto := AProjeto;
  ReloadModelRegistry;
  FillSupportedModelNames(cbModel.Items);
  LoadFromProjeto;
end;

initialization
  {$IFNDEF FPCUNIT}
    {$I formllmoptions.lrs}
  {$ENDIF}

end.

