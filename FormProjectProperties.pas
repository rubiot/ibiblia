unit FormProjectProperties;

{$mode objfpc}{$H+}

interface

uses
  ui_style, ui_icons, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ActnList, BibleScope, Project, LCLTranslator,
  StrUtils, ONTTokenizer, status_catalog, FormStatusCatalog, UnitPleaseWait,
  dictionary_manager, project_setup_model, project_setup_validation, RichMemo,
  RichMemoUtils, LazUTF8;

type

  TTokenizerVersionChangeAction = (tvcaKeepCurrentVersion, tvcaChangeAnyway, tvcaClearAssociations);
  TPendingTextAction = (ptaNone, ptaLoad, ptaClear);

  TFrmProjectProperties = class(TForm)
    ActionChangeTab: TAction;
    ActionClearText: TAction;
    ActionList1: TActionList;
    ActionLoadText: TAction;
    ActionSelectFont: TAction;
    ActionSelectMorphologyDictionary: TAction;
    ActionSelectStrongDictionary: TAction;
    BtnApplyTextDescription: TBitBtn;
    BtnCancel: TBitBtn;
    BtnClearText: TBitBtn;
    BtnLoadText: TBitBtn;
    BtnOk: TBitBtn;
    BtnSelectFont: TBitBtn;
    BtnSelectMorphologyDictionary: TBitBtn;
    BtnSelectStrongDictionary: TBitBtn;
    cbTokenizerVersion: TComboBox;
    EditMorphologyDictionary: TLabeledEdit;
    EditProjectName: TLabeledEdit;
    EditStrongDictionary: TLabeledEdit;
    EditTextDescription: TLabeledEdit;
    FontDialogSelect: TFontDialog;
    GroupBoxProperties: TGroupBox;
    LabelFont: TLabel;
    LabelTokenizerVersion: TLabel;
    MemoTextPreview: TMemo;
    OpenDialogSelectFile: TOpenDialog;
    ProgressBarOperation: TProgressBar;
    TabControlTextType: TTabControl;
    procedure ActionChangeTabExecute(Sender: TObject);
    procedure ActionClearTextExecute(Sender: TObject);
    procedure ActionLoadTextExecute(Sender: TObject);
    procedure ActionSelectFontExecute(Sender: TObject);
    procedure ActionSelectMorphologyDictionaryExecute(Sender: TObject);
    procedure ActionSelectStrongDictionaryExecute(Sender: TObject);
    procedure BtnApplyTextDescriptionClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure EditMorphologyDictionaryChange(Sender: TObject);
    procedure EditStrongDictionaryChange(Sender: TObject);
    procedure EditTextDescriptionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProject: TProject;
    FStatusDefinitions: TStatusDefinitions;
    FStatusRemaps: TStatusRemaps;
    FStatusCatalogChanged: Boolean;
    FLoadingControls: Boolean;
    BtnStatusCatalog: TButton;
    FPageControl: TPageControl;
    FTabProject: TTabSheet;
    FTabWorkflow: TTabSheet;
    FTabTexts: TTabSheet;
    FTabDictionaries: TTabSheet;
    FTabStatus: TTabSheet;
    FTabReview: TTabSheet;
    FWorkflowIntroLabel: TLabel;
    FWorkflowSuggestionGroup: TGroupBox;
    FWorkflowSuggestionMethodLabel: TLabel;
    FWorkflowSuggestionModeCombo: TComboBox;
    FWorkflowSuggestionExplanationLabel: TLabel;
    FWorkflowAIStatusLabel: TLabel;
    FWorkflowConfigureAIButton: TButton;
    FWorkflowAutomaticSuggestionCheck: TCheckBox;
    FWorkflowAutomaticSuggestionExplanationLabel: TLabel;
    FWorkflowVerseOpeningGroup: TGroupBox;
    FWorkflowDefaultVerseModeLabel: TLabel;
    FWorkflowDefaultVerseModeCombo: TComboBox;
    FWorkflowDefaultVerseModeExplanationLabel: TLabel;
    FDictionaryTextTabs: TTabControl;
    FScenarioLabel: TLabel;
    FStatusDescriptionLabel: TLabel;
    FStatusCatalogSummaryLabel: TLabel;
    FStrongDictionaryValidationLabel: TLabel;
    FMorphologyDictionaryValidationLabel: TLabel;
    FReviewMemo: TRichMemo;
    FPendingTextActions: array[tbOrigem..tbConsulta2] of TPendingTextAction;
    FPendingTextFiles: array[tbOrigem..tbConsulta2] of string;
    FPendingTextReplace: array[tbOrigem..tbConsulta2] of Boolean;
    FPendingDescriptions: array[tbOrigem..tbConsulta2] of string;
    FPendingDescriptionChanged: array[tbOrigem..tbConsulta2] of Boolean;
    FPendingStrongDictionaries: array[tbOrigem..tbConsulta2] of string;
    FPendingStrongDictionaryChanged: array[tbOrigem..tbConsulta2] of Boolean;
    FPendingMorphologyDictionaries: array[tbOrigem..tbConsulta2] of string;
    FPendingMorphologyDictionaryChanged: array[tbOrigem..tbConsulta2] of Boolean;
    FPendingFonts: array[tbOrigem..tbConsulta2] of TFont;
    FPendingFontChanged: array[tbOrigem..tbConsulta2] of Boolean;
    FPendingSuggestionMode: TSuggestionMode;
    FPendingAutomaticSuggestion: Boolean;
    FPendingDefaultVerseMode: TDefaultVerseMode;
    procedure BtnStatusCatalogClick(Sender: TObject);
    procedure BtnConfigureAIClick(Sender: TObject);
    procedure CreateTabbedLayout;
    procedure DictionaryTextTabsChange(Sender: TObject);
    function ConfirmTokenizerVersionChange: TTokenizerVersionChangeAction;
    procedure CreateStatusCatalogButton;
    function CurrentTextType: TTipoTextoBiblico;
    procedure InitializePendingState;
    function PendingPreviewText(TextType: TTipoTextoBiblico): string;
    procedure RefreshDictionaryControls(TextType: TTipoTextoBiblico);
    procedure RefreshTextControls(TextType: TTipoTextoBiblico);
    procedure UpdateDictionaryValidationLabels;
    procedure UpdateReview;
    procedure UpdateStatusCatalogSummary;
    procedure UpdateWorkflowControls;
    procedure WorkflowAutomaticSuggestionClick(Sender: TObject);
    function WorkflowAutomaticSuggestionAvailable: Boolean;
    procedure WorkflowDefaultVerseModeChange(Sender: TObject);
    procedure WorkflowSuggestionModeChange(Sender: TObject);
    function SuggestionModeCaption(Mode: TSuggestionMode): string;
    function DefaultVerseModeCaption(Mode: TDefaultVerseMode): string;
    function EnabledStateCaption(Value: Boolean): string;
    function ValidatePendingChanges(out MessageText: string): Boolean;
    function SelectedTextScopeName: string;
    procedure LoadTokenizerVersions(SelectedVersion: Integer);
    function GetSelectedTokenizerVersion: Integer;
  public
    procedure SetProject(Project: TProject);
    procedure ClearProject;
    procedure SelectWorkflowTab;
    procedure ApplyChanges(AProgressOwner: TCustomForm = nil);
    procedure Translate;
  public
    procedure AfterConstruction; override;
  end;

var
  FrmProjectProperties: TFrmProjectProperties;

resourcestring
  SProjectPropertiesTitle = 'Project settings';
  SPropertiesGroup = 'Properties';
  SProjectNameLabel = 'Name';
  SStrongDictionaryLabel = 'Strong''s dictionary';
  SMorphologyDictionaryLabel = 'Morphology dictionary';
  STextDescriptionLabel = 'Text description';
  SFontLabel = 'Font';
  SApply = 'Apply';
  SSetDictionary = 'Set';
  SChooseABibleModule = 'Please choose a theWord Bible module file...';
  SChooseAnotherText = 'Replace text in whole project';
  SChangeTextConfirmation = 'This will replace the %s text in every verse of the project with the selected Bible module.'#13#10#13#10 +
                            'Associations are not stored for this text, so source/destination associations will not be cleared.'#13#10#13#10 +
                            'Continue?';
  SChangeTextConfirmationAssociation = 'This will replace the %s text in every verse of the project with the selected Bible module.'#13#10#13#10 +
                                       'Because this text participates in source/destination associations, this affects all associations in the project.'#13#10#13#10 +
                                       'Choose Yes to try to keep all project associations by remapping them to the new text.'#13#10 +
                                       'Choose No to replace the text and clear all project associations.'#13#10 +
                                       'Choose Cancel to keep the current project text.';
  SClearText = 'Clear text in whole project';
  SClearTextConfirmation = 'This will clear the %s text in every verse of the project.'#13#10#13#10 +
                           'Associations are not stored for this text, so source/destination associations will not be cleared.'#13#10#13#10 +
                           'Continue?';
  SClearTextConfirmationAssociation = 'This will clear the %s text in every verse of the project.'#13#10#13#10 +
                                      'Because this text participates in source/destination associations, this will clear all associations in the project.'#13#10#13#10 +
                                      'Continue?';
  SChooseDictionary = 'Choose';
  SOpenDictionary = 'Please choose an unencrypted theWord dictionary module...';
  SSourceTab = 'Source';
  SDestinationTab = 'Destination';
  SReference1Tab = 'Reference 1';
  SReference2Tab = 'Reference 2';
  SSourceTextScope = 'source';
  SDestinationTextScope = 'destination';
  SReference1TextScope = 'reference 1';
  SReference2TextScope = 'reference 2';
  SReplaceTextInProjectAction = 'Replace %s text in whole project';
  SClearTextInProjectAction = 'Clear %s text in whole project';
  STokenizerVersion = 'Tokenizer version';
  SEditStatusCatalog = 'Edit status catalog...';
  STokenizerVersionChangeTitle = 'Change tokenizer version';
  STokenizerVersionChangeConfirmation = 'The tokenizer version is a project-wide setting.'#13#10#13#10 +
                                        'Changing it will retokenize the visible text and migrate all stored associations in the project. ' +
                                        'If the migration does not match the text, project associations can be lost or corrupted.'#13#10#13#10 +
                                        'Choose one of the options below:'#13#10 +
                                        'Yes: clear all project associations before changing the tokenizer.'#13#10 +
                                        'No: migrate all project associations and change the tokenizer.'#13#10 +
                                        'Cancel: keep the current tokenizer version.';
  SClearAssociationsAndChange = 'Clear all associations';
  SChangeTokenizerAnyway = 'Migrate associations';
  SKeepCurrentTokenizer = 'Keep current version';
  SChangingTokenizerVersionProgress = 'Changing tokenizer version...';
  SClearingTokenizerAssociationsProgress = 'Clearing project associations...';
  SMigratingTokenizerAssociationsProgress = 'Migrating project associations...';
  STokenizerVersionChangeDetail = '%s -> %s';
  SNewTestamentFilter = 'theWord Bible modules (*.ont, *.nt)|*.ont;*.nt';
  SOldTestamentFilter = 'theWord Bible modules (*.ont, *.ot)|*.ont;*.ot';
  SWholeBibleFilter = 'theWord Bible modules (*.ont, *.ot, *.nt)|*.ont;*.ot;*.nt';
  SDictionaryFilter = 'theWord dictionary modules (*.dct.twm)|*.dct.twm|All files|*.*';
  SNewTextImportedSuccessfully = 'New text imported successfully';
  STextImportTitle = 'Import Bible text';
  STextImportProgress = 'Importing Bible text...';
  STextImportCancelledFmt = 'The import of "%s" was cancelled.';
  STextImportFailedFmt = 'The text could not be imported from "%s".';
  STextImportPartialChanges = 'Changes completed before this import failure were kept.';
  STextImportRequiresSavedProject = 'Save or discard the current project changes before replacing a Bible text.';
  STextImportConflictingChanges = 'Apply tokenizer, status catalog, or text removal changes separately before replacing a Bible text.';
  SProjectTab = 'Project';
  SWorkflowTab = 'Workflow';
  STextsTab = 'Texts';
  SDictionariesTab = 'Dictionaries';
  SStatusTab = 'Status';
  SReviewTab = 'Review';
  SStatusCatalogDescription = 'The status catalog defines the labels, order, and colors used to track verse progress. iBiblia uses it in the project tree, filters, and review workflows. Changes remain pending until you confirm the project properties.';
  SStatusCatalogCurrent = 'Current catalog: %d statuses';
  SReviewSectionProject = 'Project';
  SReviewSectionPendingChanges = 'Pending changes';
  SProjectScenarioFmt = 'Scenario: %s';
  SPendingReady = 'Ready to apply changes.';
  SPendingBlocked = 'Cannot apply changes: %s';
  SPendingProjectName = 'Project name: %s';
  SPendingTokenizer = 'Tokenizer: %s -> %s';
  SPendingStatusCatalog = 'Status catalog changed.';
  SPendingLoadText = 'Load %s text: %s';
  SPendingClearText = 'Clear %s text.';
  SPendingDescription = 'Update %s description.';
  SPendingFont = 'Update %s font.';
  SPendingDictionary = 'Set %s %s dictionary: %s';
  SStrong = 'Strong';
  SMorphology = 'Morphology';
  SNoPendingChanges = 'No pending changes.';
  SProjectNameRequired = 'Please choose a name for the project.';
  STextPendingLoad = 'Pending import: %s';
  STextPendingClear = 'Pending clear on OK.';
  SDictionaryReady = 'Dictionary ready.';
  SDictionaryOptional = 'Optional.';
  SWorkflowIntro = 'These settings belong to this project. Changes remain pending until you confirm the project properties.';
  SWorkflowSuggestionGroup = 'Association suggestions';
  SWorkflowSuggestionMethod = '&Suggestion method';
  SWorkflowSuggestionStatistical = 'Statistical';
  SWorkflowSuggestionAI = 'AI';
  SWorkflowSuggestionStatisticalDescription = 'Uses the local statistical association engine.';
  SWorkflowSuggestionAIDescription = 'Uses the configured AI service. An internet connection is required.';
  SWorkflowAIConfigured = 'AI configuration: ready.';
  SWorkflowAIConfigurationPending = 'AI configuration: pending. Configure AI before using AI suggestions.';
  SWorkflowConfigureAI = 'Configure &AI...';
  SWorkflowAutomaticSuggestion = 'Suggest a&utomatically in the initial status';
  SWorkflowAutomaticSuggestionDescription = 'Runs one association suggestion after navigation enters a different verse that is still in the standard initial status.';
  SWorkflowAutomaticSuggestionUnavailable = 'Unavailable because this project uses a custom status catalog. Automatic suggestion depends on the standard initial status.';
  SWorkflowVerseOpeningGroup = 'When another verse opens';
  SWorkflowDefaultVerseMode = '&Open in';
  SWorkflowVerseModeAssociation = 'Association';
  SWorkflowVerseModeEditing = 'WYSIWYG editing';
  SWorkflowVerseModeAssociationDescription = 'Shows the source and destination texts ready for syntagm association.';
  SWorkflowVerseModeEditingDescription = 'Opens the destination text in the WYSIWYG editor after navigation.';
  SWorkflowEnabled = 'Enabled';
  SWorkflowDisabled = 'Disabled';
  SPendingSuggestionMode = 'Suggestion method: %s -> %s';
  SPendingAutomaticSuggestion = 'Automatic suggestion: %s -> %s';
  SPendingDefaultVerseMode = 'Verse opening: %s -> %s';

implementation

uses
  FormLlmOptions;

{ TFrmProjectProperties }

procedure TFrmProjectProperties.FormCreate(Sender: TObject);
var
  TextType: TTipoTextoBiblico;
begin
  ApplyFormStyle(Self);
  FProject := nil;
  FStatusCatalogChanged := False;
  FLoadingControls := False;
  for TextType := tbOrigem to tbConsulta2 do
    FPendingFonts[TextType] := TFont.Create;
  OnDestroy := @FormDestroy;
  CreateStatusCatalogButton;
  CreateTabbedLayout;
  BtnOk.OnClick := @BtnOkClick;
  Translate;
end;

procedure TFrmProjectProperties.FormDestroy(Sender: TObject);
var
  TextType: TTipoTextoBiblico;
begin
  for TextType := tbOrigem to tbConsulta2 do
    FreeAndNil(FPendingFonts[TextType]);
end;

procedure TFrmProjectProperties.CreateTabbedLayout;
const
  PageMargin = 16;
  ControlGap = 8;
  ProjectGroupHeight = 176;
var
  PageWidth: Integer;
  PageHeight: Integer;
begin
  if Assigned(FPageControl) then
    Exit;

  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := Self;
  FPageControl.SetBounds(16, 16, ClientWidth - 32, BtnOk.Top - 32);
  FPageControl.Anchors := [akLeft, akTop, akRight, akBottom];

  FTabProject := TTabSheet.Create(FPageControl);
  FTabProject.PageControl := FPageControl;
  FTabProject.Caption := SProjectTab;

  FTabWorkflow := TTabSheet.Create(FPageControl);
  FTabWorkflow.PageControl := FPageControl;
  FTabWorkflow.Caption := SWorkflowTab;

  FTabTexts := TTabSheet.Create(FPageControl);
  FTabTexts.PageControl := FPageControl;
  FTabTexts.Caption := STextsTab;

  FTabDictionaries := TTabSheet.Create(FPageControl);
  FTabDictionaries.PageControl := FPageControl;
  FTabDictionaries.Caption := SDictionariesTab;

  FTabStatus := TTabSheet.Create(FPageControl);
  FTabStatus.PageControl := FPageControl;
  FTabStatus.Caption := SStatusTab;

  FTabReview := TTabSheet.Create(FPageControl);
  FTabReview.PageControl := FPageControl;
  FTabReview.Caption := SReviewTab;

  PageWidth := FPageControl.Width - 32;
  PageHeight := FPageControl.Height - 48;

  GroupBoxProperties.Parent := FTabProject;
  GroupBoxProperties.SetBounds(16, 16, PageWidth - 12, ProjectGroupHeight);
  GroupBoxProperties.Anchors := [akLeft, akTop, akRight];
  EditProjectName.SetBounds(16, 24, PageWidth - 32, 25);
  EditProjectName.Anchors := [akLeft, akTop, akRight];
  LabelTokenizerVersion.SetBounds(16, 62, PageWidth - 32, 18);
  LabelTokenizerVersion.Anchors := [akLeft, akTop, akRight];
  cbTokenizerVersion.SetBounds(16, 82, PageWidth - 32, 25);
  cbTokenizerVersion.Anchors := [akLeft, akTop, akRight];

  FScenarioLabel := TLabel.Create(Self);
  FScenarioLabel.Parent := GroupBoxProperties;
  FScenarioLabel.SetBounds(16, 120, PageWidth - 32, 22);
  FScenarioLabel.Anchors := [akLeft, akTop, akRight];

  FWorkflowIntroLabel := TLabel.Create(Self);
  FWorkflowIntroLabel.Parent := FTabWorkflow;
  FWorkflowIntroLabel.SetBounds(16, 16, PageWidth - 32, 48);
  FWorkflowIntroLabel.Anchors := [akLeft, akTop, akRight];
  FWorkflowIntroLabel.AutoSize := False;
  FWorkflowIntroLabel.WordWrap := True;

  FWorkflowSuggestionGroup := TGroupBox.Create(Self);
  FWorkflowSuggestionGroup.Parent := FTabWorkflow;
  FWorkflowSuggestionGroup.SetBounds(16, 80, PageWidth - 12, 280);
  FWorkflowSuggestionGroup.Anchors := [akLeft, akTop, akRight];

  FWorkflowSuggestionMethodLabel := TLabel.Create(Self);
  FWorkflowSuggestionMethodLabel.Parent := FWorkflowSuggestionGroup;
  FWorkflowSuggestionMethodLabel.SetBounds(16, 26, PageWidth - 32, 18);
  FWorkflowSuggestionMethodLabel.Anchors := [akLeft, akTop, akRight];

  FWorkflowSuggestionModeCombo := TComboBox.Create(Self);
  FWorkflowSuggestionModeCombo.Parent := FWorkflowSuggestionGroup;
  FWorkflowSuggestionModeCombo.SetBounds(16, 46, PageWidth - 32, 25);
  FWorkflowSuggestionModeCombo.Anchors := [akLeft, akTop, akRight];
  FWorkflowSuggestionModeCombo.Style := csDropDownList;
  FWorkflowSuggestionModeCombo.OnChange := @WorkflowSuggestionModeChange;
  FWorkflowSuggestionMethodLabel.FocusControl :=
    FWorkflowSuggestionModeCombo;

  FWorkflowSuggestionExplanationLabel := TLabel.Create(Self);
  FWorkflowSuggestionExplanationLabel.Parent := FWorkflowSuggestionGroup;
  FWorkflowSuggestionExplanationLabel.SetBounds(16, 78, PageWidth - 32, 36);
  FWorkflowSuggestionExplanationLabel.Anchors := [akLeft, akTop, akRight];
  FWorkflowSuggestionExplanationLabel.AutoSize := False;
  FWorkflowSuggestionExplanationLabel.WordWrap := True;

  FWorkflowAIStatusLabel := TLabel.Create(Self);
  FWorkflowAIStatusLabel.Parent := FWorkflowSuggestionGroup;
  FWorkflowAIStatusLabel.SetBounds(16, 124, PageWidth - 236, 44);
  FWorkflowAIStatusLabel.Anchors := [akLeft, akTop, akRight];
  FWorkflowAIStatusLabel.AutoSize := False;
  FWorkflowAIStatusLabel.WordWrap := True;

  FWorkflowConfigureAIButton := TButton.Create(Self);
  FWorkflowConfigureAIButton.Parent := FWorkflowSuggestionGroup;
  FWorkflowConfigureAIButton.SetBounds(PageWidth - 204, 122, 176, 32);
  FWorkflowConfigureAIButton.Anchors := [akTop, akRight];
  FWorkflowConfigureAIButton.OnClick := @BtnConfigureAIClick;

  FWorkflowAutomaticSuggestionCheck := TCheckBox.Create(Self);
  FWorkflowAutomaticSuggestionCheck.Parent := FWorkflowSuggestionGroup;
  FWorkflowAutomaticSuggestionCheck.SetBounds(16, 174, PageWidth - 32, 26);
  FWorkflowAutomaticSuggestionCheck.Anchors := [akLeft, akTop, akRight];
  FWorkflowAutomaticSuggestionCheck.OnClick := @WorkflowAutomaticSuggestionClick;

  FWorkflowAutomaticSuggestionExplanationLabel := TLabel.Create(Self);
  FWorkflowAutomaticSuggestionExplanationLabel.Parent := FWorkflowSuggestionGroup;
  FWorkflowAutomaticSuggestionExplanationLabel.SetBounds(36, 202,
    PageWidth - 52, 52);
  FWorkflowAutomaticSuggestionExplanationLabel.Anchors := [akLeft, akTop, akRight];
  FWorkflowAutomaticSuggestionExplanationLabel.AutoSize := False;
  FWorkflowAutomaticSuggestionExplanationLabel.WordWrap := True;

  FWorkflowVerseOpeningGroup := TGroupBox.Create(Self);
  FWorkflowVerseOpeningGroup.Parent := FTabWorkflow;
  FWorkflowVerseOpeningGroup.SetBounds(16, 376, PageWidth - 12, 176);
  FWorkflowVerseOpeningGroup.Anchors := [akLeft, akTop, akRight];

  FWorkflowDefaultVerseModeLabel := TLabel.Create(Self);
  FWorkflowDefaultVerseModeLabel.Parent := FWorkflowVerseOpeningGroup;
  FWorkflowDefaultVerseModeLabel.SetBounds(16, 26, PageWidth - 32, 18);
  FWorkflowDefaultVerseModeLabel.Anchors := [akLeft, akTop, akRight];

  FWorkflowDefaultVerseModeCombo := TComboBox.Create(Self);
  FWorkflowDefaultVerseModeCombo.Parent := FWorkflowVerseOpeningGroup;
  FWorkflowDefaultVerseModeCombo.SetBounds(16, 46, PageWidth - 32, 25);
  FWorkflowDefaultVerseModeCombo.Anchors := [akLeft, akTop, akRight];
  FWorkflowDefaultVerseModeCombo.Style := csDropDownList;
  FWorkflowDefaultVerseModeCombo.OnChange := @WorkflowDefaultVerseModeChange;
  FWorkflowDefaultVerseModeLabel.FocusControl :=
    FWorkflowDefaultVerseModeCombo;

  FWorkflowDefaultVerseModeExplanationLabel := TLabel.Create(Self);
  FWorkflowDefaultVerseModeExplanationLabel.Parent := FWorkflowVerseOpeningGroup;
  FWorkflowDefaultVerseModeExplanationLabel.SetBounds(16, 80,
    PageWidth - 32, 58);
  FWorkflowDefaultVerseModeExplanationLabel.Anchors := [akLeft, akTop, akRight];
  FWorkflowDefaultVerseModeExplanationLabel.AutoSize := False;
  FWorkflowDefaultVerseModeExplanationLabel.WordWrap := True;

  TabControlTextType.Parent := FTabTexts;
  TabControlTextType.SetBounds(10, 10, PageWidth, 28);
  TabControlTextType.Anchors := [akLeft, akTop, akRight];

  BtnLoadText.Parent := FTabTexts;
  BtnLoadText.SetBounds(PageMargin, 46, PageWidth - (PageMargin * 2), 28);
  BtnLoadText.Anchors := [akLeft, akTop, akRight];
  BtnClearText.Parent := FTabTexts;
  BtnClearText.SetBounds(PageMargin, 46 + 28 + ControlGap,
    PageWidth - (PageMargin * 2), 28);
  BtnClearText.Anchors := [akLeft, akTop, akRight];
  EditTextDescription.Parent := FTabTexts;
  EditTextDescription.SetBounds(12, 132, PageWidth - 140, 24);
  EditTextDescription.Anchors := [akLeft, akTop, akRight];
  BtnApplyTextDescription.Parent := FTabTexts;
  BtnApplyTextDescription.SetBounds(PageWidth - 126, 130, 122, 28);
  BtnApplyTextDescription.Anchors := [akTop, akRight];
  LabelFont.Parent := FTabTexts;
  LabelFont.SetBounds(12, 172, 120, 18);
  BtnSelectFont.Parent := FTabTexts;
  BtnSelectFont.SetBounds(138, 168, 120, 28);
  MemoTextPreview.Parent := FTabTexts;
  MemoTextPreview.SetBounds(12, 210, PageWidth - 4, PageHeight - 222);
  MemoTextPreview.Anchors := [akLeft, akTop, akRight, akBottom];

  FDictionaryTextTabs := TTabControl.Create(Self);
  FDictionaryTextTabs.Parent := FTabDictionaries;
  FDictionaryTextTabs.SetBounds(10, 10, PageWidth, 28);
  FDictionaryTextTabs.Anchors := [akLeft, akTop, akRight];
  FDictionaryTextTabs.OnChange := @DictionaryTextTabsChange;

  EditStrongDictionary.Parent := FTabDictionaries;
  EditStrongDictionary.SetBounds(12, 66, PageWidth - 116, 24);
  EditStrongDictionary.Anchors := [akLeft, akTop, akRight];
  BtnSelectStrongDictionary.Parent := FTabDictionaries;
  BtnSelectStrongDictionary.SetBounds(PageWidth - 102, 64, 98, 28);
  BtnSelectStrongDictionary.Anchors := [akTop, akRight];
  FStrongDictionaryValidationLabel := TLabel.Create(Self);
  FStrongDictionaryValidationLabel.Parent := FTabDictionaries;
  FStrongDictionaryValidationLabel.SetBounds(12, 94, PageWidth - 4, 18);
  FStrongDictionaryValidationLabel.Anchors := [akLeft, akTop, akRight];

  EditMorphologyDictionary.Parent := FTabDictionaries;
  EditMorphologyDictionary.SetBounds(12, 146, PageWidth - 116, 24);
  EditMorphologyDictionary.Anchors := [akLeft, akTop, akRight];
  BtnSelectMorphologyDictionary.Parent := FTabDictionaries;
  BtnSelectMorphologyDictionary.SetBounds(PageWidth - 102, 144, 98, 28);
  BtnSelectMorphologyDictionary.Anchors := [akTop, akRight];
  FMorphologyDictionaryValidationLabel := TLabel.Create(Self);
  FMorphologyDictionaryValidationLabel.Parent := FTabDictionaries;
  FMorphologyDictionaryValidationLabel.SetBounds(12, 174, PageWidth - 4, 18);
  FMorphologyDictionaryValidationLabel.Anchors := [akLeft, akTop, akRight];

  FStatusDescriptionLabel := TLabel.Create(Self);
  FStatusDescriptionLabel.Parent := FTabStatus;
  FStatusDescriptionLabel.SetBounds(12, 24, PageWidth - 24, 84);
  FStatusDescriptionLabel.Anchors := [akLeft, akTop, akRight];
  FStatusDescriptionLabel.AutoSize := False;
  FStatusDescriptionLabel.WordWrap := True;
  FStatusDescriptionLabel.Caption := SStatusCatalogDescription;

  FStatusCatalogSummaryLabel := TLabel.Create(Self);
  FStatusCatalogSummaryLabel.Parent := FTabStatus;
  FStatusCatalogSummaryLabel.SetBounds(12, 126, PageWidth - 24, 18);
  FStatusCatalogSummaryLabel.Anchors := [akLeft, akTop, akRight];

  if Assigned(BtnStatusCatalog) then
  begin
    BtnStatusCatalog.Parent := FTabStatus;
    BtnStatusCatalog.SetBounds(12, 158, 180, 28);
    BtnStatusCatalog.Anchors := [akLeft, akTop];
  end;
  UpdateStatusCatalogSummary;

  FReviewMemo := TRichMemo.Create(Self);
  FReviewMemo.Parent := FTabReview;
  FReviewMemo.Align := alClient;
  FReviewMemo.ReadOnly := True;
  FReviewMemo.ScrollBars := ssVertical;
  FReviewMemo.WordWrap := True;
  FReviewMemo.Font.Assign(Font);
  if FReviewMemo.Font.Size <= 0 then
    FReviewMemo.Font.Size := 9;
end;

procedure TFrmProjectProperties.CreateStatusCatalogButton;
begin
  if Assigned(BtnStatusCatalog) then
    Exit;

  BtnStatusCatalog := TButton.Create(Self);
  BtnStatusCatalog.Caption := SEditStatusCatalog;
  BtnStatusCatalog.OnClick := @BtnStatusCatalogClick;
end;

procedure TFrmProjectProperties.BtnStatusCatalogClick(Sender: TObject);
var
  Counts: TStatusCounts;
  Definitions: TStatusDefinitions;
  Remaps: TStatusRemaps;
begin
  if not Assigned(FProject) then
    Exit;

  FProject.CountVersesByStatus(Counts);
  Definitions := Copy(FStatusDefinitions, 0, Length(FStatusDefinitions));
  if TFrmStatusCatalog.Execute(Definitions, Counts, Remaps) then
  begin
    FStatusDefinitions := Copy(Definitions, 0, Length(Definitions));
    FStatusRemaps := Copy(Remaps, 0, Length(Remaps));
    FStatusCatalogChanged := True;
    UpdateStatusCatalogSummary;
    UpdateWorkflowControls;
    UpdateReview;
  end;
end;

procedure TFrmProjectProperties.UpdateStatusCatalogSummary;
begin
  if Assigned(FStatusCatalogSummaryLabel) then
    FStatusCatalogSummaryLabel.Caption := Format(SStatusCatalogCurrent,
      [Length(FStatusDefinitions)]);
end;

function TFrmProjectProperties.WorkflowAutomaticSuggestionAvailable: Boolean;
begin
  Result := Assigned(FProject) and FProject.AutomaticSuggestionAvailable and
    (not FStatusCatalogChanged);
end;

function TFrmProjectProperties.SuggestionModeCaption(
  Mode: TSuggestionMode): string;
begin
  case Mode of
    smAI: Result := SWorkflowSuggestionAI;
  else
    Result := SWorkflowSuggestionStatistical;
  end;
end;

function TFrmProjectProperties.DefaultVerseModeCaption(
  Mode: TDefaultVerseMode): string;
begin
  case Mode of
    dvmEditing: Result := SWorkflowVerseModeEditing;
  else
    Result := SWorkflowVerseModeAssociation;
  end;
end;

function TFrmProjectProperties.EnabledStateCaption(Value: Boolean): string;
begin
  if Value then
    Result := SWorkflowEnabled
  else
    Result := SWorkflowDisabled;
end;

procedure TFrmProjectProperties.UpdateWorkflowControls;
var
  AutomaticSuggestionAvailable: Boolean;
  WasLoadingControls: Boolean;
begin
  if not Assigned(FWorkflowSuggestionModeCombo) then
    Exit;

  WasLoadingControls := FLoadingControls;
  FLoadingControls := True;
  try
    AutomaticSuggestionAvailable := WorkflowAutomaticSuggestionAvailable;
    if not AutomaticSuggestionAvailable then
      FPendingAutomaticSuggestion := False;

    FWorkflowSuggestionModeCombo.ItemIndex := Ord(FPendingSuggestionMode);
    FWorkflowAutomaticSuggestionCheck.Enabled :=
      AutomaticSuggestionAvailable;
    FWorkflowAutomaticSuggestionCheck.Checked :=
      FPendingAutomaticSuggestion and AutomaticSuggestionAvailable;
    FWorkflowDefaultVerseModeCombo.ItemIndex :=
      Ord(FPendingDefaultVerseMode);

    case FPendingSuggestionMode of
      smAI:
        FWorkflowSuggestionExplanationLabel.Caption :=
          SWorkflowSuggestionAIDescription;
    else
      FWorkflowSuggestionExplanationLabel.Caption :=
        SWorkflowSuggestionStatisticalDescription;
    end;

    if Assigned(FProject) and FProject.AIConfigurationAvailable then
    begin
      FWorkflowAIStatusLabel.Caption := SWorkflowAIConfigured;
      FWorkflowAIStatusLabel.Font.Color := UISuccessColor;
    end
    else
    begin
      FWorkflowAIStatusLabel.Caption := SWorkflowAIConfigurationPending;
      FWorkflowAIStatusLabel.Font.Color := UIWarningColor;
    end;

    if AutomaticSuggestionAvailable then
    begin
      FWorkflowAutomaticSuggestionExplanationLabel.Caption :=
        SWorkflowAutomaticSuggestionDescription;
      FWorkflowAutomaticSuggestionExplanationLabel.Font.Color := clWindowText;
    end
    else
    begin
      FWorkflowAutomaticSuggestionExplanationLabel.Caption :=
        SWorkflowAutomaticSuggestionUnavailable;
      FWorkflowAutomaticSuggestionExplanationLabel.Font.Color := clGrayText;
    end;

    case FPendingDefaultVerseMode of
      dvmEditing:
        FWorkflowDefaultVerseModeExplanationLabel.Caption :=
          SWorkflowVerseModeEditingDescription;
    else
      FWorkflowDefaultVerseModeExplanationLabel.Caption :=
        SWorkflowVerseModeAssociationDescription;
    end;
  finally
    FLoadingControls := WasLoadingControls;
  end;
end;

procedure TFrmProjectProperties.WorkflowSuggestionModeChange(Sender: TObject);
begin
  if FLoadingControls then
    Exit;

  if FWorkflowSuggestionModeCombo.ItemIndex = Ord(smAI) then
    FPendingSuggestionMode := smAI
  else
    FPendingSuggestionMode := smStatistical;
  UpdateWorkflowControls;
  UpdateReview;
end;

procedure TFrmProjectProperties.WorkflowAutomaticSuggestionClick(
  Sender: TObject);
begin
  if FLoadingControls then
    Exit;

  FPendingAutomaticSuggestion :=
    FWorkflowAutomaticSuggestionCheck.Checked and
    WorkflowAutomaticSuggestionAvailable;
  UpdateWorkflowControls;
  UpdateReview;
end;

procedure TFrmProjectProperties.WorkflowDefaultVerseModeChange(
  Sender: TObject);
begin
  if FLoadingControls then
    Exit;

  if FWorkflowDefaultVerseModeCombo.ItemIndex = Ord(dvmEditing) then
    FPendingDefaultVerseMode := dvmEditing
  else
    FPendingDefaultVerseMode := dvmAssociation;
  UpdateWorkflowControls;
  UpdateReview;
end;

procedure TFrmProjectProperties.BtnConfigureAIClick(Sender: TObject);
begin
  if not Assigned(FProject) then
    Exit;

  if not Assigned(FrmLLMOptions) then
    Application.CreateForm(TFrmLLMOptions, FrmLLMOptions);
  FrmLLMOptions.SetProject(FProject);
  FrmLLMOptions.ShowModal;
  UpdateWorkflowControls;
  UpdateReview;
end;

function TFrmProjectProperties.CurrentTextType: TTipoTextoBiblico;
begin
  if TabControlTextType.TabIndex < Ord(tbOrigem) then
    Result := tbOrigem
  else if TabControlTextType.TabIndex > Ord(tbConsulta2) then
    Result := tbConsulta2
  else
    Result := TTipoTextoBiblico(TabControlTextType.TabIndex);
end;

procedure TFrmProjectProperties.InitializePendingState;
var
  TextType: TTipoTextoBiblico;
begin
  if not Assigned(FProject) then
    Exit;

  FPendingSuggestionMode := FProject.SuggestionMode;
  FPendingAutomaticSuggestion := FProject.AutomaticAssociationSuggestion;
  FPendingDefaultVerseMode := FProject.DefaultVerseMode;
  if not WorkflowAutomaticSuggestionAvailable then
    FPendingAutomaticSuggestion := False;

  for TextType := tbOrigem to tbConsulta2 do
  begin
    FPendingTextActions[TextType] := ptaNone;
    FPendingTextFiles[TextType] := '';
    FPendingTextReplace[TextType] := False;

    FPendingDescriptions[TextType] := FProject.GetTextDescription(TextType);
    FPendingDescriptionChanged[TextType] := False;

    FPendingStrongDictionaries[TextType] :=
      FProject.GetInfo(Format('dicstrong%d', [Ord(TextType)]));
    FPendingStrongDictionaryChanged[TextType] := False;

    FPendingMorphologyDictionaries[TextType] :=
      FProject.GetInfo(Format('dicmorfo%d', [Ord(TextType)]));
    FPendingMorphologyDictionaryChanged[TextType] := False;

    if Assigned(FProject.GetTextFont(TextType)) then
      FPendingFonts[TextType].Assign(FProject.GetTextFont(TextType));
    FPendingFontChanged[TextType] := False;
  end;
end;

function TFrmProjectProperties.PendingPreviewText(TextType: TTipoTextoBiblico): string;
var
  Validation: TProjectSetupValidationResult;
begin
  case FPendingTextActions[TextType] of
    ptaLoad:
      begin
        if ProjectSetupValidateModule(FPendingTextFiles[TextType],
          FProject.Escopo, GetSelectedTokenizerVersion, True, Validation) then
          Result := Validation.PreviewText
        else
          Result := Validation.MessageText;
      end;
    ptaClear:
      Result := '';
  else
    Result := FProject.GetPlainVerseText(TextType);
  end;
end;

procedure TFrmProjectProperties.RefreshTextControls(TextType: TTipoTextoBiblico);
begin
  if not Assigned(FProject) then
    Exit;

  FLoadingControls := True;
  try
    ActionLoadText.Caption := Format(SReplaceTextInProjectAction, [SelectedTextScopeName]);
    ActionClearText.Caption := Format(SClearTextInProjectAction, [SelectedTextScopeName]);
    EditTextDescription.Text := FPendingDescriptions[TextType];
    BtnApplyTextDescription.Hide;
    MemoTextPreview.Text := PendingPreviewText(TextType);
    if FPendingFontChanged[TextType] then
      MemoTextPreview.Font := FPendingFonts[TextType]
    else
      MemoTextPreview.Font := FProject.GetTextFont(TextType);
  finally
    FLoadingControls := False;
  end;
end;

procedure TFrmProjectProperties.RefreshDictionaryControls(TextType: TTipoTextoBiblico);
begin
  if not Assigned(FProject) then
    Exit;

  FLoadingControls := True;
  try
    EditStrongDictionary.Text := FPendingStrongDictionaries[TextType];
    EditMorphologyDictionary.Text := FPendingMorphologyDictionaries[TextType];
    BtnSelectStrongDictionary.Caption := SChooseDictionary;
    BtnSelectMorphologyDictionary.Caption := SChooseDictionary;
  finally
    FLoadingControls := False;
  end;
  UpdateDictionaryValidationLabels;
end;

procedure TFrmProjectProperties.DictionaryTextTabsChange(Sender: TObject);
begin
  if FLoadingControls then
    Exit;

  if Assigned(FDictionaryTextTabs) and
    (TabControlTextType.TabIndex <> FDictionaryTextTabs.TabIndex) then
  begin
    TabControlTextType.TabIndex := FDictionaryTextTabs.TabIndex;
    ActionChangeTabExecute(nil);
  end
  else
    RefreshDictionaryControls(CurrentTextType);
end;

procedure TFrmProjectProperties.UpdateDictionaryValidationLabels;
var
  TextType: TTipoTextoBiblico;
  MessageText: string;
  Valid: Boolean;
begin
  TextType := CurrentTextType;

  Valid := ProjectSetupValidateDictionary(FPendingStrongDictionaries[TextType],
    dmtStrong, MessageText);
  FStrongDictionaryValidationLabel.Caption := MessageText;
  if Valid then
    FStrongDictionaryValidationLabel.Font.Color := UISuccessColor
  else
    FStrongDictionaryValidationLabel.Font.Color := UIErrorColor;

  Valid := ProjectSetupValidateDictionary(FPendingMorphologyDictionaries[TextType],
    dmtMorphology, MessageText);
  FMorphologyDictionaryValidationLabel.Caption := MessageText;
  if Valid then
    FMorphologyDictionaryValidationLabel.Font.Color := UISuccessColor
  else
    FMorphologyDictionaryValidationLabel.Font.Color := UIErrorColor;
end;

function TFrmProjectProperties.ValidatePendingChanges(out MessageText: string): Boolean;
var
  TextType: TTipoTextoBiblico;
  Validation: TProjectSetupValidationResult;
  StatusError: string;
begin
  Result := False;
  MessageText := '';

  if Trim(EditProjectName.Text) = '' then
  begin
    MessageText := SProjectNameRequired;
    Exit;
  end;

  if not ValidateStatusDefinitions(FStatusDefinitions, StatusError) then
  begin
    MessageText := StatusError;
    Exit;
  end;

  for TextType := tbOrigem to tbConsulta2 do
  begin
    if FPendingTextActions[TextType] = ptaLoad then
      if not ProjectSetupValidateModule(FPendingTextFiles[TextType],
        FProject.Escopo, GetSelectedTokenizerVersion, True, Validation) then
      begin
        MessageText := Format('%s: %s',
          [ProjectSetupTextCaption(TextType), Validation.MessageText]);
        Exit;
      end;

    if not ProjectSetupValidateDictionary(FPendingStrongDictionaries[TextType],
      dmtStrong, MessageText) then
    begin
      MessageText := Format('%s Strong: %s',
        [ProjectSetupTextCaption(TextType), MessageText]);
      Exit;
    end;

    if not ProjectSetupValidateDictionary(FPendingMorphologyDictionaries[TextType],
      dmtMorphology, MessageText) then
    begin
      MessageText := Format('%s Morphology: %s',
        [ProjectSetupTextCaption(TextType), MessageText]);
      Exit;
    end;
  end;

  Result := True;
end;

procedure TFrmProjectProperties.UpdateReview;
var
  MessageText: string;
  TextType: TTipoTextoBiblico;
  HasPending: Boolean;
  Scenario: TProjectScenario;

  procedure AppendReviewText(const Text: string; Styles: TFontStyles;
    Color: TColor);
  var
    Params: TFontParams;
  begin
    InitFontParams(Params);
    Params.Name := FReviewMemo.Font.Name;
    Params.Size := FReviewMemo.Font.Size;
    if Params.Size <= 0 then
      Params.Size := 9;
    Params.Color := Color;
    Params.Style := Styles;
    InsertFontText(FReviewMemo, Text, Params, UTF8Length(FReviewMemo.Text));
  end;

  procedure AppendReviewLine(const Text: string; Styles: TFontStyles;
    Color: TColor);
  begin
    AppendReviewText(Text + LineEnding, Styles, Color);
  end;

  procedure AppendBlankLine;
  begin
    AppendReviewText(LineEnding, [], clWindowText);
  end;

  procedure AppendSection(const Text: string);
  begin
    if FReviewMemo.Text <> '' then
      AppendBlankLine;
    AppendReviewLine(Text, [fsBold], clNavy);
  end;

  procedure AppendLabelValueLine(const Text: string);
  var
    SeparatorPos: Integer;
  begin
    SeparatorPos := Pos(':', Text);
    if SeparatorPos > 0 then
    begin
      AppendReviewText(Copy(Text, 1, SeparatorPos), [fsBold], clWindowText);
      AppendReviewText(Copy(Text, SeparatorPos + 1, MaxInt), [],
        clWindowText);
      AppendReviewText(LineEnding, [], clWindowText);
    end
    else
      AppendReviewLine(Text, [], clWindowText);
  end;

  procedure AddPending(const Text: string);
  begin
    HasPending := True;
    AppendLabelValueLine(Text);
  end;

begin
  if not Assigned(FReviewMemo) then
    Exit;
  if not Assigned(FProject) then
  begin
    FReviewMemo.Clear;
    Exit;
  end;

  FReviewMemo.Clear;
  FReviewMemo.Lines.BeginUpdate;
  try
    if ValidatePendingChanges(MessageText) then
      AppendReviewLine(SPendingReady, [fsBold], clGreen)
    else
      AppendReviewLine(Format(SPendingBlocked, [MessageText]), [fsBold],
        clRed);

    AppendSection(SReviewSectionProject);
    HasPending := False;

    if Assigned(FProject) then
    begin
      Scenario := ProjectScenarioFromConfig(
        FProject.GetInfo(PROJECT_SCENARIO_CONFIG_KEY));
      AppendLabelValueLine(Format(SProjectScenarioFmt,
        [ProjectScenarioCaption(Scenario)]));
    end;

    AppendSection(SReviewSectionPendingChanges);
    if Assigned(FProject) and (EditProjectName.Text <> FProject.GetInfo('descricao')) then
      AddPending(Format(SPendingProjectName, [EditProjectName.Text]));

    if Assigned(FProject) and (GetSelectedTokenizerVersion <> FProject.TokenizerVersion) then
      AddPending(Format(SPendingTokenizer, [
        TTokenizerFactory.GetVersionCaption(FProject.TokenizerVersion),
        TTokenizerFactory.GetVersionCaption(GetSelectedTokenizerVersion)]));

    if FStatusCatalogChanged then
      AddPending(SPendingStatusCatalog);

    if Assigned(FProject) and
      (FPendingSuggestionMode <> FProject.SuggestionMode) then
      AddPending(Format(SPendingSuggestionMode, [
        SuggestionModeCaption(FProject.SuggestionMode),
        SuggestionModeCaption(FPendingSuggestionMode)]));

    if Assigned(FProject) and
      (FPendingAutomaticSuggestion <>
       FProject.AutomaticAssociationSuggestion) then
      AddPending(Format(SPendingAutomaticSuggestion, [
        EnabledStateCaption(FProject.AutomaticAssociationSuggestion),
        EnabledStateCaption(FPendingAutomaticSuggestion)]));

    if Assigned(FProject) and
      (FPendingDefaultVerseMode <> FProject.DefaultVerseMode) then
      AddPending(Format(SPendingDefaultVerseMode, [
        DefaultVerseModeCaption(FProject.DefaultVerseMode),
        DefaultVerseModeCaption(FPendingDefaultVerseMode)]));

    for TextType := tbOrigem to tbConsulta2 do
    begin
      case FPendingTextActions[TextType] of
        ptaLoad:
          AddPending(Format(SPendingLoadText,
            [ProjectSetupTextCaption(TextType), FPendingTextFiles[TextType]]));
        ptaClear:
          AddPending(Format(SPendingClearText,
            [ProjectSetupTextCaption(TextType)]));
      end;

      if FPendingDescriptionChanged[TextType] then
        AddPending(Format(SPendingDescription,
          [ProjectSetupTextCaption(TextType)]));

      if FPendingFontChanged[TextType] then
        AddPending(Format(SPendingFont,
          [ProjectSetupTextCaption(TextType)]));

      if FPendingStrongDictionaryChanged[TextType] then
        AddPending(Format(SPendingDictionary,
          [ProjectSetupTextCaption(TextType), SStrong,
           FPendingStrongDictionaries[TextType]]));

      if FPendingMorphologyDictionaryChanged[TextType] then
        AddPending(Format(SPendingDictionary,
          [ProjectSetupTextCaption(TextType), SMorphology,
           FPendingMorphologyDictionaries[TextType]]));
    end;

    if not HasPending then
      AppendReviewLine(SNoPendingChanges, [fsItalic], clGrayText);

    FReviewMemo.SelStart := 0;
  finally
    FReviewMemo.Lines.EndUpdate;
  end;
end;

procedure TFrmProjectProperties.EditMorphologyDictionaryChange(Sender: TObject);
begin
  if FLoadingControls then
    Exit;

  FPendingMorphologyDictionaries[CurrentTextType] := EditMorphologyDictionary.Text;
  FPendingMorphologyDictionaryChanged[CurrentTextType] := True;
  BtnSelectMorphologyDictionary.Caption := SSetDictionary;
  UpdateDictionaryValidationLabels;
  UpdateReview;
end;

procedure TFrmProjectProperties.EditStrongDictionaryChange(Sender: TObject);
begin
  if FLoadingControls then
    Exit;

  FPendingStrongDictionaries[CurrentTextType] := EditStrongDictionary.Text;
  FPendingStrongDictionaryChanged[CurrentTextType] := True;
  BtnSelectStrongDictionary.Caption := SSetDictionary;
  UpdateDictionaryValidationLabels;
  UpdateReview;
end;

procedure TFrmProjectProperties.EditTextDescriptionChange(Sender: TObject);
begin
  if FLoadingControls then
    Exit;

  FPendingDescriptions[CurrentTextType] := EditTextDescription.Text;
  FPendingDescriptionChanged[CurrentTextType] := True;
  BtnApplyTextDescription.Show;
  UpdateReview;
end;

procedure TFrmProjectProperties.Translate;
var
  SelectedVersion: Integer;
  WasLoadingControls: Boolean;
begin
  SelectedVersion := GetSelectedTokenizerVersion;
  Caption := SProjectPropertiesTitle;
  if Assigned(BtnCancel) then
    BtnCancel.Caption := SUICancel;
  GroupBoxProperties.Caption := SPropertiesGroup;
  EditProjectName.EditLabel.Caption := SProjectNameLabel;
  EditStrongDictionary.EditLabel.Caption := SStrongDictionaryLabel;
  EditMorphologyDictionary.EditLabel.Caption := SMorphologyDictionaryLabel;
  EditTextDescription.EditLabel.Caption := STextDescriptionLabel;
  LabelFont.Caption := SFontLabel;
  BtnApplyTextDescription.Caption := SApply;
  TabControlTextType.Tabs[0] := SSourceTab;
  TabControlTextType.Tabs[1] := SDestinationTab;
  TabControlTextType.Tabs[2] := SReference1Tab;
  TabControlTextType.Tabs[3] := SReference2Tab;
  if Assigned(FDictionaryTextTabs) then
  begin
    FDictionaryTextTabs.Tabs.Clear;
    FDictionaryTextTabs.Tabs.Add(SSourceTab);
    FDictionaryTextTabs.Tabs.Add(SDestinationTab);
    FDictionaryTextTabs.Tabs.Add(SReference1Tab);
    FDictionaryTextTabs.Tabs.Add(SReference2Tab);
    if TabControlTextType.TabIndex >= 0 then
      FDictionaryTextTabs.TabIndex := TabControlTextType.TabIndex;
  end;
  if Assigned(FTabProject) then FTabProject.Caption := SProjectTab;
  if Assigned(FTabWorkflow) then FTabWorkflow.Caption := SWorkflowTab;
  if Assigned(FTabTexts) then FTabTexts.Caption := STextsTab;
  if Assigned(FTabDictionaries) then FTabDictionaries.Caption := SDictionariesTab;
  if Assigned(FTabStatus) then FTabStatus.Caption := SStatusTab;
  if Assigned(FTabReview) then FTabReview.Caption := SReviewTab;
  LabelTokenizerVersion.Caption := STokenizerVersion;
  if Assigned(BtnStatusCatalog) then
    BtnStatusCatalog.Caption := SEditStatusCatalog;
  if Assigned(FStatusDescriptionLabel) then
    FStatusDescriptionLabel.Caption := SStatusCatalogDescription;
  if Assigned(FScenarioLabel) and Assigned(FProject) then
    FScenarioLabel.Caption := Format(SProjectScenarioFmt,
      [ProjectScenarioCaption(ProjectScenarioFromConfig(
        FProject.GetInfo(PROJECT_SCENARIO_CONFIG_KEY)))]);
  if Assigned(FWorkflowIntroLabel) then
    FWorkflowIntroLabel.Caption := SWorkflowIntro;
  if Assigned(FWorkflowSuggestionGroup) then
    FWorkflowSuggestionGroup.Caption := SWorkflowSuggestionGroup;
  if Assigned(FWorkflowSuggestionMethodLabel) then
    FWorkflowSuggestionMethodLabel.Caption := SWorkflowSuggestionMethod;
  if Assigned(FWorkflowConfigureAIButton) then
    FWorkflowConfigureAIButton.Caption := SWorkflowConfigureAI;
  if Assigned(FWorkflowAutomaticSuggestionCheck) then
    FWorkflowAutomaticSuggestionCheck.Caption :=
      SWorkflowAutomaticSuggestion;
  if Assigned(FWorkflowVerseOpeningGroup) then
    FWorkflowVerseOpeningGroup.Caption := SWorkflowVerseOpeningGroup;
  if Assigned(FWorkflowDefaultVerseModeLabel) then
    FWorkflowDefaultVerseModeLabel.Caption := SWorkflowDefaultVerseMode;
  WasLoadingControls := FLoadingControls;
  FLoadingControls := True;
  try
    if Assigned(FWorkflowSuggestionModeCombo) then
    begin
      FWorkflowSuggestionModeCombo.Items.Clear;
      FWorkflowSuggestionModeCombo.Items.Add(
        SWorkflowSuggestionStatistical);
      FWorkflowSuggestionModeCombo.Items.Add(SWorkflowSuggestionAI);
    end;
    if Assigned(FWorkflowDefaultVerseModeCombo) then
    begin
      FWorkflowDefaultVerseModeCombo.Items.Clear;
      FWorkflowDefaultVerseModeCombo.Items.Add(
        SWorkflowVerseModeAssociation);
      FWorkflowDefaultVerseModeCombo.Items.Add(
        SWorkflowVerseModeEditing);
    end;
  finally
    FLoadingControls := WasLoadingControls;
  end;
  UpdateWorkflowControls;
  UpdateStatusCatalogSummary;
  LoadTokenizerVersions(SelectedVersion);
  ActionSelectFont.Caption := SChooseDictionary;
  ActionSelectStrongDictionary.Caption := SChooseDictionary;
  ActionSelectMorphologyDictionary.Caption := SChooseDictionary;
  ActionLoadText.Caption := Format(SReplaceTextInProjectAction, [SelectedTextScopeName]);
  ActionClearText.Caption := Format(SClearTextInProjectAction, [SelectedTextScopeName]);
end;

procedure TFrmProjectProperties.ActionLoadTextExecute(Sender: TObject);
var
  ReplaceExistingText: Boolean;
  TextScopeName: string;
  TextType: TTipoTextoBiblico;
  Validation: TProjectSetupValidationResult;
begin
  TextScopeName := SelectedTextScopeName;
  TextType := CurrentTextType;
  ReplaceExistingText := False;
  if TextTypeParticipatesInAssociations(TextType) then
  begin
    case MessageDlg(SChooseAnotherText, Format(SChangeTextConfirmationAssociation,
        [TextScopeName]), mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: ReplaceExistingText := True;
      mrNo: ReplaceExistingText := False;
      mrCancel: Exit;
    end;
  end else
  begin
    if MessageDlg(SChooseAnotherText, Format(SChangeTextConfirmation,
        [TextScopeName]), mtConfirmation, [mbYes, mbCancel], 0) = mrCancel then
      Exit;
    ReplaceExistingText := False;
  end;

  case FProject.Escopo of
    etOT: OpenDialogSelectFile.Filter := SOldTestamentFilter;
    etNT: OpenDialogSelectFile.Filter := SNewTestamentFilter;
    etONT: OpenDialogSelectFile.Filter := SWholeBibleFilter;
  end;

  OpenDialogSelectFile.Title := SChooseABibleModule;
  if OpenDialogSelectFile.Execute then
  begin
    if not ProjectSetupValidateModule(OpenDialogSelectFile.FileName,
      FProject.Escopo, GetSelectedTokenizerVersion, True, Validation) then
    begin
      MessageDlg(SChooseABibleModule, Validation.MessageText, mtWarning, [mbOK], 0);
      Exit;
    end;

    FPendingTextActions[TextType] := ptaLoad;
    FPendingTextFiles[TextType] := OpenDialogSelectFile.FileName;
    FPendingTextReplace[TextType] := ReplaceExistingText;
    MemoTextPreview.Text := Validation.PreviewText;
    UpdateReview;
  end;
end;

procedure TFrmProjectProperties.ActionClearTextExecute(Sender: TObject);
var
  Msg: string;
  TextScopeName: string;
  TextType: TTipoTextoBiblico;
begin
  TextType := CurrentTextType;
  TextScopeName := SelectedTextScopeName;
  Msg := IfThen(TextTypeParticipatesInAssociations(TextType),
                Format(SClearTextConfirmationAssociation, [TextScopeName]),
                Format(SClearTextConfirmation, [TextScopeName]));
  if MessageDlg(SClearText, Msg, mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    FPendingTextActions[TextType] := ptaClear;
    FPendingTextFiles[TextType] := '';
    MemoTextPreview.Clear;
    UpdateReview;
  end;
end;

procedure TFrmProjectProperties.ActionChangeTabExecute(Sender: TObject);
var
  TextType: TTipoTextoBiblico;
begin
  TextType := CurrentTextType;
  if Assigned(FDictionaryTextTabs) and (FDictionaryTextTabs.TabIndex <> Ord(TextType)) then
    FDictionaryTextTabs.TabIndex := Ord(TextType);

  RefreshTextControls(TextType);
  RefreshDictionaryControls(TextType);
  UpdateReview;
end;

procedure TFrmProjectProperties.ActionSelectFontExecute(Sender: TObject);
var
  TextType: TTipoTextoBiblico;
begin
  FontDialogSelect.Font := MemoTextPreview.Font;
  if FontDialogSelect.Execute then
  begin
    TextType := CurrentTextType;
    MemoTextPreview.Font := FontDialogSelect.Font;
    FPendingFonts[TextType].Assign(FontDialogSelect.Font);
    FPendingFontChanged[TextType] := True;
    UpdateReview;
  end;
end;

procedure TFrmProjectProperties.ActionSelectMorphologyDictionaryExecute(Sender: TObject);
var
  CurrentDirectory: string;
  SelectedPath: string;
  MessageText: string;
  TextType: TTipoTextoBiblico;
begin
  CurrentDirectory := '';
  TextType := CurrentTextType;
  OpenDialogSelectFile.Filter := SDictionaryFilter;
  OpenDialogSelectFile.FileName := EditMorphologyDictionary.Text;
  OpenDialogSelectFile.Title := SOpenDictionary;
  if (BtnSelectMorphologyDictionary.Caption = SSetDictionary) or OpenDialogSelectFile.Execute then
  begin
    { use a relative path whenever possible }
    GetDir(0, CurrentDirectory);
    if Pos(CurrentDirectory, OpenDialogSelectFile.FileName) = 1 then
      SelectedPath := Format('.%s', [RightStr(OpenDialogSelectFile.FileName,
        Length(OpenDialogSelectFile.FileName) - Length(CurrentDirectory))])
    else
      SelectedPath := OpenDialogSelectFile.FileName;
    if not ProjectSetupValidateDictionary(SelectedPath, dmtMorphology, MessageText) then
    begin
      MessageDlg(SOpenDictionary, MessageText, mtWarning, [mbOK], 0);
      Exit;
    end;
    FPendingMorphologyDictionaries[TextType] := SelectedPath;
    FPendingMorphologyDictionaryChanged[TextType] := True;
    EditMorphologyDictionary.Text := SelectedPath;
  end;
  BtnSelectMorphologyDictionary.Caption := SChooseDictionary;
  UpdateDictionaryValidationLabels;
  UpdateReview;
end;

procedure TFrmProjectProperties.ActionSelectStrongDictionaryExecute(Sender: TObject);
var
  CurrentDirectory: string;
  SelectedPath: string;
  MessageText: string;
  TextType: TTipoTextoBiblico;
begin
  CurrentDirectory := '';
  TextType := CurrentTextType;
  OpenDialogSelectFile.Filter := SDictionaryFilter;
  OpenDialogSelectFile.FileName := EditStrongDictionary.Text;
  OpenDialogSelectFile.Title := SOpenDictionary;
  if (BtnSelectStrongDictionary.Caption = SSetDictionary) or OpenDialogSelectFile.Execute then
  begin
    { use a relative path whenever possible }
    GetDir(0, CurrentDirectory);
    if Pos(CurrentDirectory, OpenDialogSelectFile.FileName) = 1 then
      SelectedPath := Format('.%s', [RightStr(OpenDialogSelectFile.FileName,
        Length(OpenDialogSelectFile.FileName) - Length(CurrentDirectory))])
    else
      SelectedPath := OpenDialogSelectFile.FileName;
    if not ProjectSetupValidateDictionary(SelectedPath, dmtStrong, MessageText) then
    begin
      MessageDlg(SOpenDictionary, MessageText, mtWarning, [mbOK], 0);
      Exit;
    end;
    FPendingStrongDictionaries[TextType] := SelectedPath;
    FPendingStrongDictionaryChanged[TextType] := True;
    EditStrongDictionary.Text := SelectedPath;
  end;
  BtnSelectStrongDictionary.Caption := SChooseDictionary;
  UpdateDictionaryValidationLabels;
  UpdateReview;
end;

procedure TFrmProjectProperties.BtnApplyTextDescriptionClick(Sender: TObject);
begin
  FPendingDescriptions[CurrentTextType] := EditTextDescription.Text;
  FPendingDescriptionChanged[CurrentTextType] := True;
  BtnApplyTextDescription.Hide;
  UpdateReview;
end;

procedure TFrmProjectProperties.BtnOkClick(Sender: TObject);
var
  MessageText: string;
begin
  if not ValidatePendingChanges(MessageText) then
  begin
    ModalResult := mrNone;
    if Assigned(FPageControl) then
      FPageControl.ActivePage := FTabReview;
    UpdateReview;
    MessageDlg(Caption, MessageText, mtWarning, [mbOK], 0);
  end;
end;

function TFrmProjectProperties.SelectedTextScopeName: string;
begin
  case TTipoTextoBiblico(TabControlTextType.TabIndex) of
    tbOrigem: Result := SSourceTextScope;
    tbDestino: Result := SDestinationTextScope;
    tbConsulta1: Result := SReference1TextScope;
    tbConsulta2: Result := SReference2TextScope;
  else
    Result := SSourceTextScope;
  end;
end;

function TFrmProjectProperties.ConfirmTokenizerVersionChange: TTokenizerVersionChangeAction;
var
  DialogForm: TForm;
  ButtonIndex: Integer;
begin
  Result := tvcaKeepCurrentVersion;
  DialogForm := CreateMessageDialog(STokenizerVersionChangeConfirmation, mtConfirmation, [mbYes, mbNo, mbCancel]);
  try
    DialogForm.Caption := STokenizerVersionChangeTitle;
    DialogForm.Position := poMainFormCenter;
    for ButtonIndex := 0 to DialogForm.ComponentCount - 1 do
      if DialogForm.Components[ButtonIndex] is TButton then
        case TButton(DialogForm.Components[ButtonIndex]).ModalResult of
          mrYes: TButton(DialogForm.Components[ButtonIndex]).Caption := SClearAssociationsAndChange;
          mrNo: TButton(DialogForm.Components[ButtonIndex]).Caption := SChangeTokenizerAnyway;
          mrCancel: TButton(DialogForm.Components[ButtonIndex]).Caption := SKeepCurrentTokenizer;
        end;

    case DialogForm.ShowModal of
      mrYes: Result := tvcaClearAssociations;
      mrNo: Result := tvcaChangeAnyway;
    else
      Result := tvcaKeepCurrentVersion;
    end;
  finally
    DialogForm.Free;
  end;
end;

procedure TFrmProjectProperties.LoadTokenizerVersions(SelectedVersion: Integer);
var
  VersionIndex: Integer;
begin
  TTokenizerFactory.PopulateVersionCaptions(cbTokenizerVersion.Items);
  cbTokenizerVersion.ItemIndex := -1;

  for VersionIndex := 0 to cbTokenizerVersion.Items.Count - 1 do
    if PtrInt(cbTokenizerVersion.Items.Objects[VersionIndex]) = SelectedVersion then
    begin
      cbTokenizerVersion.ItemIndex := VersionIndex;
      Exit;
    end;

  if cbTokenizerVersion.Items.Count > 0 then
    cbTokenizerVersion.ItemIndex := cbTokenizerVersion.Items.Count - 1;
end;

function TFrmProjectProperties.GetSelectedTokenizerVersion: Integer;
begin
  if cbTokenizerVersion.ItemIndex >= 0 then
    Result := PtrInt(cbTokenizerVersion.Items.Objects[cbTokenizerVersion.ItemIndex])
  else if Assigned(FProject) then
    Result := FProject.TokenizerVersion
  else
    Result := TTokenizerFactory.LATEST_VERSION;
end;

procedure TFrmProjectProperties.SetProject(Project: TProject);
var
  Scenario: TProjectScenario;
begin
  FProject := Project;
  FStatusDefinitions := Project.GetStatusDefinitions;
  FStatusRemaps := nil;
  FStatusCatalogChanged := False;
  UpdateStatusCatalogSummary;
  EditProjectName.Text := Project.GetInfo('descricao');
  LoadTokenizerVersions(Project.TokenizerVersion);
  InitializePendingState;
  UpdateWorkflowControls;
  Scenario := ProjectScenarioFromConfig(Project.GetInfo(PROJECT_SCENARIO_CONFIG_KEY));
  if Assigned(FScenarioLabel) then
    FScenarioLabel.Caption := Format(SProjectScenarioFmt,
      [ProjectScenarioCaption(Scenario)]);
  if TabControlTextType.TabIndex < 0 then
    TabControlTextType.TabIndex := 0;
  if Assigned(FDictionaryTextTabs) then
    FDictionaryTextTabs.TabIndex := TabControlTextType.TabIndex;
  ActionChangeTab.Execute;
  UpdateReview;
end;

procedure TFrmProjectProperties.ClearProject;
begin
  FProject := nil;
  FStatusDefinitions := nil;
  FStatusRemaps := nil;
  FStatusCatalogChanged := False;
  FPendingSuggestionMode := smStatistical;
  FPendingAutomaticSuggestion := False;
  FPendingDefaultVerseMode := dvmAssociation;
  UpdateWorkflowControls;
  UpdateStatusCatalogSummary;
end;

procedure TFrmProjectProperties.SelectWorkflowTab;
begin
  if Assigned(FPageControl) and Assigned(FTabWorkflow) then
    FPageControl.ActivePage := FTabWorkflow;
end;

procedure TFrmProjectProperties.ApplyChanges(
  AProgressOwner: TCustomForm);
var
  CurrentVersion: Integer;
  SelectedVersion: Integer;
  TokenizerChangeAction: TTokenizerVersionChangeAction;
  HasAssociations: Boolean;
  VersionChangeDetail: string;
  RemapIndex: Integer;
  TextType: TTipoTextoBiblico;
  MessageText: string;
  ImportResult: TTheWordModuleImportResult;
  ImportSucceeded: Boolean;
  HasTextImport: Boolean;
  ReplaceAssociations: Boolean;
begin
  if not Assigned(AProgressOwner) then
    AProgressOwner := Application.MainForm;
  if not Assigned(AProgressOwner) then
    AProgressOwner := Self;

  if Assigned(FProject) then
  begin
    if not ValidatePendingChanges(MessageText) then
      raise Exception.Create(MessageText);

    HasTextImport := False;
    for TextType := tbOrigem to tbConsulta2 do
      if FPendingTextActions[TextType] = ptaLoad then
        HasTextImport := True;

    if HasTextImport then
    begin
      if FProject.Modificado then
      begin
        MessageDlg(STextImportTitle, STextImportRequiresSavedProject,
          mtWarning, [mbOK], 0);
        Exit;
      end;
      if (GetSelectedTokenizerVersion <> FProject.TokenizerVersion) or
        FStatusCatalogChanged then
      begin
        MessageDlg(STextImportTitle, STextImportConflictingChanges,
          mtWarning, [mbOK], 0);
        Exit;
      end;
      for TextType := tbOrigem to tbConsulta2 do
        if FPendingTextActions[TextType] = ptaClear then
        begin
          MessageDlg(STextImportTitle, STextImportConflictingChanges,
            mtWarning, [mbOK], 0);
          Exit;
        end;
    end;

    FProject.SetInfo('descricao', EditProjectName.Text);

    CurrentVersion := FProject.TokenizerVersion;
    SelectedVersion := GetSelectedTokenizerVersion;

    if SelectedVersion <> CurrentVersion then
    begin
      HasAssociations := FProject.HasStoredAssociations;
      TokenizerChangeAction := tvcaChangeAnyway;
      if HasAssociations then
        TokenizerChangeAction := ConfirmTokenizerVersionChange;

      case TokenizerChangeAction of
          tvcaKeepCurrentVersion:
            SelectedVersion := CurrentVersion;
          tvcaClearAssociations:
            begin
              VersionChangeDetail := Format(STokenizerVersionChangeDetail, [
                TTokenizerFactory.GetVersionCaption(CurrentVersion),
                TTokenizerFactory.GetVersionCaption(SelectedVersion)
              ]);
              FormPleaseWait.BeginOperation(AProgressOwner, STokenizerVersionChangeTitle,
                SClearingTokenizerAssociationsProgress, 0,
                VersionChangeDetail);
              try
                FProject.ClearStoredAssociations(FormPleaseWait.ProgressBar1);
                FormPleaseWait.SetMessage(SChangingTokenizerVersionProgress);
                FProject.ChangeTokenizerVersion(SelectedVersion,
                  FormPleaseWait.ProgressBar1, False);
              finally
                FormPleaseWait.FinishOperation;
              end;
            end;
        end;

      if (SelectedVersion <> CurrentVersion) and
        (TokenizerChangeAction <> tvcaClearAssociations) then
      begin
        VersionChangeDetail := Format(STokenizerVersionChangeDetail, [
          TTokenizerFactory.GetVersionCaption(CurrentVersion),
          TTokenizerFactory.GetVersionCaption(SelectedVersion)
        ]);
        if HasAssociations then
          FormPleaseWait.BeginOperation(AProgressOwner, STokenizerVersionChangeTitle,
            SMigratingTokenizerAssociationsProgress, 0, VersionChangeDetail)
        else
          FormPleaseWait.BeginOperation(AProgressOwner, STokenizerVersionChangeTitle,
            SChangingTokenizerVersionProgress, 0, VersionChangeDetail);
        try
          FProject.ChangeTokenizerVersion(SelectedVersion,
            FormPleaseWait.ProgressBar1, HasAssociations);
        finally
          FormPleaseWait.FinishOperation;
        end;
      end;
    end;

    if FStatusCatalogChanged then
    begin
      for RemapIndex := 0 to High(FStatusRemaps) do
        FProject.RemapStatus(FStatusRemaps[RemapIndex].OldStatusId,
          FStatusRemaps[RemapIndex].NewStatusId);
      FProject.SetStatusDefinitions(FStatusDefinitions);
    end;

    FProject.SuggestionMode := FPendingSuggestionMode;
    FProject.AutomaticAssociationSuggestion :=
      FPendingAutomaticSuggestion;
    FProject.DefaultVerseMode := FPendingDefaultVerseMode;

    for TextType := tbOrigem to tbConsulta2 do
    begin
      case FPendingTextActions[TextType] of
        ptaLoad:
          begin
            FormPleaseWait.BeginOperation(AProgressOwner, STextImportTitle,
              STextImportProgress, 1, FPendingTextFiles[TextType]);
            try
              ReplaceAssociations := FPendingTextReplace[TextType] and
                TextTypeParticipatesInAssociations(TextType);
              ImportSucceeded := FProject.ImportTheWordModule(
                FPendingTextFiles[TextType], TextType,
                FormPleaseWait.ProgressBar1,
                ReplaceAssociations, ImportResult, True);
            finally
              FormPleaseWait.FinishOperation;
            end;

            if not ImportSucceeded then
            begin
              MessageText := Trim(ImportResult.ErrorMessage);
              if MessageText = '' then
                if ImportResult.Status = twmisCancelled then
                  MessageText := Format(STextImportCancelledFmt,
                    [FPendingTextFiles[TextType]])
                else
                  MessageText := Format(STextImportFailedFmt,
                    [FPendingTextFiles[TextType]]);
              MessageText := MessageText + LineEnding + LineEnding +
                STextImportPartialChanges;

              { A previous text or another property may already have been
                committed. Reload the current Verse and reset the pending UI
                before reporting the partial result. }
              FProject.RefreshUI;
              InitializePendingState;
              ActionChangeTabExecute(nil);
              UpdateReview;
              if ImportResult.Status = twmisCancelled then
                MessageDlg(STextImportTitle, MessageText, mtInformation,
                  [mbOK], 0)
              else
                MessageDlg(STextImportTitle, MessageText, mtError, [mbOK], 0);
              Exit;
            end;
          end;
        ptaClear:
          FProject.ClearText(TextType);
      end;

      if FPendingDescriptionChanged[TextType] then
        FProject.SetTextDescription(TextType, FPendingDescriptions[TextType]);

      if FPendingFontChanged[TextType] then
        FProject.AssignTextFont(FPendingFonts[TextType], [TextType]);

      if FPendingStrongDictionaryChanged[TextType] then
      begin
        if FPendingStrongDictionaries[TextType] = '' then
          FProject.SetInfo(Format('dicstrong%d', [Ord(TextType)]), '')
        else
          FProject.AssignStrongDictionary(FPendingStrongDictionaries[TextType],
            [TextType]);
      end;

      if FPendingMorphologyDictionaryChanged[TextType] then
      begin
        if FPendingMorphologyDictionaries[TextType] = '' then
          FProject.SetInfo(Format('dicmorfo%d', [Ord(TextType)]), '')
        else
          FProject.AssignMorphologyDictionary(
            FPendingMorphologyDictionaries[TextType], [TextType]);
      end;
    end;

    FProject.RefreshUI;
    InitializePendingState;
    ActionChangeTabExecute(nil);
    UpdateReview;
  end;
end;

procedure TFrmProjectProperties.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
  StyleControl(FWorkflowIntroLabel, uirHint);
  StyleControl(FStatusDescriptionLabel, uirHint);
  SetButtonIcon(BtnLoadText, aiTextLoad);
  SetButtonIcon(BtnClearText, aiTextClear);
  SetButtonIcon(BtnSelectFont, aiFontSelect);
  SetButtonIcon(BtnSelectStrongDictionary, aiDictionaryStrongSelect);
  SetButtonIcon(BtnSelectMorphologyDictionary, aiDictionaryMorphologySelect);
end;


initialization
{$IFNDEF FPCUNIT}
  {$R FormProjectProperties.lfm}
{$ENDIF}

end.
