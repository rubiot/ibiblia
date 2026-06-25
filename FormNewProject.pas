unit FormNewProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, LCLTranslator, ONTTokenizer,
  BibleScope, Project, status_catalog, FormStatusCatalog, dictionary_manager,
  project_setup_model, project_setup_validation, RichMemo, RichMemoUtils,
  LazUTF8;

type

  { TFrmNewProject }

  TFrmNewProject = class(TForm)
    BtnCancel: TBitBtn;
    BtnOk: TBitBtn;
    BtnSelectDestinationText: TBitBtn;
    BtnSelectReferenceText1: TBitBtn;
    BtnSelectReferenceText2: TBitBtn;
    BtnSelectSourceText: TBitBtn;
    cbTokenizerVersion: TComboBox;
    GroupBoxProperties: TGroupBox;
    GroupBoxTexts: TGroupBox;
    EditDestinationText: TLabeledEdit;
    EditProjectName: TLabeledEdit;
    EditReferenceText1: TLabeledEdit;
    EditReferenceText2: TLabeledEdit;
    EditSourceText: TLabeledEdit;
    LabelTokenizerVersion: TLabel;
    OpenDialogSelectText: TOpenDialog;
    RadioGroupScope: TRadioGroup;
    procedure BtnOkClick(Sender: TObject);
    procedure BtnSelectDestinationTextClick(Sender: TObject);
    procedure BtnSelectReferenceText1Click(Sender: TObject);
    procedure BtnSelectReferenceText2Click(Sender: TObject);
    procedure BtnSelectSourceTextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FStatusDefinitions: TStatusDefinitions;
    FStatusCatalogCustomized: Boolean;
    FSuppressEvents: Boolean;
    BtnStatusCatalog: TButton;
    FWizard: TPageControl;
    FTabScenario: TTabSheet;
    FTabProject: TTabSheet;
    FTabTexts: TTabSheet;
    FTabDictionaries: TTabSheet;
    FTabTokenizer: TTabSheet;
    FTabStatus: TTabSheet;
    FTabReview: TTabSheet;
    FNavPanel: TPanel;
    FBackButton: TButton;
    FNextButton: TButton;
    FFinishButton: TButton;
    FCancelButton: TButton;
    FScenarioRadio: TRadioGroup;
    FScenarioDescription: TMemo;
    FProjectFileEdit: TLabeledEdit;
    FProjectFileButton: TButton;
    FProjectSaveDialog: TSaveDialog;
    FTextEdits: array[tbOrigem..tbConsulta2] of TLabeledEdit;
    FTextButtons: array[tbOrigem..tbConsulta2] of TBitBtn;
    FTextValidationLabels: array[tbOrigem..tbConsulta2] of TLabel;
    FTextPreviews: array[tbOrigem..tbConsulta2] of TMemo;
    FStrongDictionaryEdits: array[tbOrigem..tbConsulta2] of TLabeledEdit;
    FMorphologyDictionaryEdits: array[tbOrigem..tbConsulta2] of TLabeledEdit;
    FStrongDictionaryButtons: array[tbOrigem..tbConsulta2] of TButton;
    FMorphologyDictionaryButtons: array[tbOrigem..tbConsulta2] of TButton;
    FStrongDictionaryLabels: array[tbOrigem..tbConsulta2] of TLabel;
    FMorphologyDictionaryLabels: array[tbOrigem..tbConsulta2] of TLabel;
    FStatusDescriptionLabel: TLabel;
    FStatusCatalogSummaryLabel: TLabel;
    FReviewMemo: TRichMemo;
    FLastSuggestedProjectName: string;
    procedure BackButtonClick(Sender: TObject);
    procedure BtnStatusCatalogClick(Sender: TObject);
    procedure CreateProjectPage;
    procedure CreateReviewPage;
    procedure CreateScenarioPage;
    procedure CreateStatusCatalogButton;
    procedure CreateStatusPage;
    procedure CreateTextPage;
    procedure CreateDictionaryPage;
    procedure CreateTokenizerPage;
    procedure CreateWizard;
    procedure DictionaryButtonClick(Sender: TObject);
    procedure FinishButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure ProjectFileButtonClick(Sender: TObject);
    procedure ScenarioChanged(Sender: TObject);
    procedure SetTextModuleFilter;
    procedure UpdateDictionaryValidation(TextType: TTipoTextoBiblico;
      StrongDictionary: Boolean);
    procedure UpdateNavigation;
    procedure UpdateReview;
    procedure UpdateScenarioUI;
    procedure UpdateStatusCatalogSummary;
    procedure UpdateTextValidation(TextType: TTipoTextoBiblico);
    function BuildDraft: TProjectSetupDraft;
    function DefaultStatusDefinitions(Scenario: TProjectScenario): TStatusDefinitions;
    function DictionaryEdit(TextType: TTipoTextoBiblico;
      StrongDictionary: Boolean): TLabeledEdit;
    function ScopeFromUI: TEscopoTexto;
    function ScenarioFromUI: TProjectScenario;
    function SuggestedProjectName(Scenario: TProjectScenario): string;
    function TextEdit(TextType: TTipoTextoBiblico): TLabeledEdit;
    function TextModuleRequired(TextType: TTipoTextoBiblico): Boolean;
    procedure UpdateSuggestedProjectName;
    function ValidateStep(StepIndex: Integer; ShowDialog: Boolean): Boolean;
    procedure LoadTokenizerVersions(SelectedVersion: Integer);
    procedure SelectText(Edit: TLabeledEdit);
    procedure SetValidationLabel(LabelControl: TLabel;
      Severity: TProjectSetupValidationSeverity; const MessageText: string);
  public
    function GetDraft: TProjectSetupDraft;
    function GetTokenizerVersion: Integer;
    function GetStatusDefinitions: TStatusDefinitions;
    function HasCustomStatusCatalog: Boolean;
    procedure ResetDraft(const DefaultProjectName: string);
    procedure Translate;
  end;

var
  FrmNewProject: TFrmNewProject;

resourcestring
  SNewProject = 'New project';
  SNewAssociationProjectName = 'New association project';
  SNewTranslationProjectName = 'New translation project';
  SChooseAName = 'Please choose a name for your project';
  SChooseAtLeastOneText = 'Please choose a source Bible text';
  SChooseDestinationText = 'Please choose a destination Bible text for an association project';
  SChooseProjectFile = 'Please choose a .bib project file';
  SProjectFileExtension = 'The project file must use the .bib extension';
  SProjectFileAlreadyExists = 'A project file already exists at this location';
  SOldTestament = 'Old Testament';
  SNewTestament = 'New Testament';
  SWholeBible = 'Whole Bible';
  STokenizerVersion = 'Tokenizer version';
  SEditStatusCatalog = 'Edit status catalog...';
  SStatusNotAssociated = 'Not associated';
  SStatusAssociating = 'Associating';
  SStatusNeedsReview = 'Needs review';
  SStatusAssociated = 'Associated!';
  SStatusAwaitingTranslation = 'Awaiting translation';
  SStatusTranslating = 'Translating';
  SStatusReadyForReview = 'Ready for review';
  SStatusHasPendingIssues = 'Has pending issues';
  SStatusReviewed = 'Reviewed';
  SNewTestamentFilter = 'theWord Bible modules (*.ont, *.nt)|*.ont;*.nt';
  SOldTestamentFilter = 'theWord Bible modules (*.ont, *.ot)|*.ont;*.ot';
  SWholeBibleFilter = 'theWord Bible modules (*.ont, *.ot, *.nt)|*.ont;*.ot;*.nt';
  SProjectFileFilter = 'iBiblia project (*.bib)|*.bib|All files|*.*';
  SDictionaryFilter = 'theWord dictionary modules (*.dct.twm)|*.dct.twm|All files|*.*';
  SChooseBibleModuleTitle = 'Choose a theWord Bible module';
  SChooseDictionaryModuleTitle = 'Choose a theWord dictionary module';
  SScenarioStep = 'Scenario';
  SProjectStep = 'Project';
  STextsStep = 'Texts';
  SDictionariesStep = 'Dictionaries';
  STokenizerStep = 'Tokenizer';
  SStatusStep = 'Status';
  SReviewStep = 'Review';
  SBack = '< Back';
  SNext = 'Next >';
  SFinish = 'Finish';
  SCancel = 'Cancel';
  SBrowse = 'Browse...';
  SScenarioAssociationTitle = 'Translation association project';
  SScenarioTranslationTitle = 'Translation project';
  SScenarioAssociationDescription = 'Use this when you already have a source text and a translated destination text. iBiblia will guide you through associating source syntagms with destination syntagms, so both source and destination texts are required.';
  SScenarioTranslationDescription = 'Use this when the destination text will be created inside iBiblia. Only the source text is required now; the destination text can start empty and be filled by the user.';
  SProjectNameLabel = 'Project name';
  SProjectFileLabel = 'Project file (.bib)';
  SScopeLabel = 'Scope';
  SSourceTextLabel = 'Source text';
  SDestinationTextLabelRequired = 'Destination text (required)';
  SDestinationTextLabelOptional = 'Destination text (optional)';
  SReference1TextLabel = 'Reference 1 text (optional)';
  SReference2TextLabel = 'Reference 2 text (optional)';
  SStrongDictionaryLabel = '%s Strong dictionary';
  SMorphologyDictionaryLabel = '%s morphology dictionary';
  SOptionalDictionaryHint = 'Optional; invalid filled dictionaries block Finish.';
  SStatusCatalogDescription = 'The status catalog defines the labels, order, and colors used to track verse progress. iBiblia uses it in the project tree, filters, and review workflows. You can keep the default catalog for the selected scenario or customize it before creating the project.';
  SStatusCatalogCurrent = 'Current catalog: %d statuses';
  SReviewReady = 'Ready to create the project.';
  SReviewBlocked = 'Cannot finish yet: %s';
  SReviewScenario = 'Scenario: %s';
  SReviewProject = 'Project: %s';
  SReviewProjectFile = 'File: %s';
  SReviewScope = 'Scope: %s';
  SReviewTokenizer = 'Tokenizer: %s';
  SReviewText = '%s text: %s';
  SReviewDictionary = '%s %s dictionary: %s';
  SReviewSectionProject = 'Project';
  SReviewSectionTexts = 'Texts';
  SReviewSectionDictionaries = 'Dictionaries';
  SNoDictionariesSelected = 'No dictionaries selected.';
  SNotSelected = '(not selected)';
  SStrong = 'Strong';
  SMorphology = 'Morphology';
  SStatusCatalogReady = 'Status catalog: %d statuses';

implementation

{ TFrmNewProject }

procedure TFrmNewProject.FormCreate(Sender: TObject);
begin
  FSuppressEvents := False;
  FStatusCatalogCustomized := False;
  FStatusDefinitions := DefaultStatusDefinitions(psAssociationProject);
  CreateWizard;
  Translate;
  ResetDraft('');
end;

procedure TFrmNewProject.CreateWizard;
begin
  Caption := SNewProject;
  Width := 760;
  Height := 620;
  Position := poMainFormCenter;

  GroupBoxProperties.Visible := False;
  GroupBoxTexts.Visible := False;
  BtnOk.Visible := False;
  BtnCancel.Visible := False;

  FProjectSaveDialog := TSaveDialog.Create(Self);
  FProjectSaveDialog.Filter := SProjectFileFilter;
  FProjectSaveDialog.DefaultExt := 'bib';

  FWizard := TPageControl.Create(Self);
  FWizard.Parent := Self;
  FWizard.Align := alClient;

  FTabScenario := TTabSheet.Create(FWizard);
  FTabScenario.PageControl := FWizard;
  FTabScenario.Caption := SScenarioStep;

  FTabProject := TTabSheet.Create(FWizard);
  FTabProject.PageControl := FWizard;
  FTabProject.Caption := SProjectStep;

  FTabTexts := TTabSheet.Create(FWizard);
  FTabTexts.PageControl := FWizard;
  FTabTexts.Caption := STextsStep;

  FTabDictionaries := TTabSheet.Create(FWizard);
  FTabDictionaries.PageControl := FWizard;
  FTabDictionaries.Caption := SDictionariesStep;

  FTabTokenizer := TTabSheet.Create(FWizard);
  FTabTokenizer.PageControl := FWizard;
  FTabTokenizer.Caption := STokenizerStep;

  FTabStatus := TTabSheet.Create(FWizard);
  FTabStatus.PageControl := FWizard;
  FTabStatus.Caption := SStatusStep;

  FTabReview := TTabSheet.Create(FWizard);
  FTabReview.PageControl := FWizard;
  FTabReview.Caption := SReviewStep;

  CreateScenarioPage;
  CreateProjectPage;
  CreateTextPage;
  CreateDictionaryPage;
  CreateTokenizerPage;
  CreateStatusPage;
  CreateReviewPage;

  FNavPanel := TPanel.Create(Self);
  FNavPanel.Parent := Self;
  FNavPanel.Align := alBottom;
  FNavPanel.Height := 46;
  FNavPanel.BevelOuter := bvNone;

  FCancelButton := TButton.Create(Self);
  FCancelButton.Parent := FNavPanel;
  FCancelButton.Caption := SCancel;
  FCancelButton.ModalResult := mrCancel;
  FCancelButton.SetBounds(10, 10, 90, 27);

  FBackButton := TButton.Create(Self);
  FBackButton.Parent := FNavPanel;
  FBackButton.Caption := SBack;
  FBackButton.SetBounds(455, 10, 90, 27);
  FBackButton.OnClick := @BackButtonClick;

  FNextButton := TButton.Create(Self);
  FNextButton.Parent := FNavPanel;
  FNextButton.Caption := SNext;
  FNextButton.SetBounds(550, 10, 90, 27);
  FNextButton.OnClick := @NextButtonClick;

  FFinishButton := TButton.Create(Self);
  FFinishButton.Parent := FNavPanel;
  FFinishButton.Caption := SFinish;
  FFinishButton.SetBounds(645, 10, 90, 27);
  FFinishButton.OnClick := @FinishButtonClick;

  UpdateNavigation;
end;

procedure TFrmNewProject.CreateScenarioPage;
begin
  FScenarioRadio := TRadioGroup.Create(Self);
  FScenarioRadio.Parent := FTabScenario;
  FScenarioRadio.SetBounds(20, 20, 690, 100);
  FScenarioRadio.Columns := 1;
  FScenarioRadio.OnClick := @ScenarioChanged;

  FScenarioDescription := TMemo.Create(Self);
  FScenarioDescription.Parent := FTabScenario;
  FScenarioDescription.SetBounds(20, 140, 690, 260);
  FScenarioDescription.ReadOnly := True;
  FScenarioDescription.ScrollBars := ssVertical;
  FScenarioDescription.WordWrap := True;
  FScenarioDescription.BorderStyle := bsSingle;
end;

procedure TFrmNewProject.CreateProjectPage;
begin
  EditProjectName.Parent := FTabProject;
  EditProjectName.SetBounds(24, 32, 520, 24);
  EditProjectName.EditLabel.Caption := SProjectNameLabel;

  FProjectFileEdit := TLabeledEdit.Create(Self);
  FProjectFileEdit.Parent := FTabProject;
  FProjectFileEdit.SetBounds(24, 96, 570, 24);
  FProjectFileEdit.EditLabel.Caption := SProjectFileLabel;

  FProjectFileButton := TButton.Create(Self);
  FProjectFileButton.Parent := FTabProject;
  FProjectFileButton.Caption := SBrowse;
  FProjectFileButton.SetBounds(606, 95, 100, 26);
  FProjectFileButton.OnClick := @ProjectFileButtonClick;

  RadioGroupScope.Parent := FTabProject;
  RadioGroupScope.SetBounds(24, 152, 570, 96);
  RadioGroupScope.Caption := SScopeLabel;
end;

procedure TFrmNewProject.CreateTextPage;

  procedure ConfigureText(TextType: TTipoTextoBiblico; Edit: TLabeledEdit;
    Button: TBitBtn; TopPos: Integer; const LabelText: string);
  begin
    FTextEdits[TextType] := Edit;
    FTextButtons[TextType] := Button;

    Edit.Parent := FTabTexts;
    Edit.SetBounds(24, TopPos, 570, 24);
    Edit.EditLabel.Caption := LabelText;

    Button.Parent := FTabTexts;
    Button.Caption := SBrowse;
    Button.SetBounds(606, TopPos - 1, 100, 26);

    FTextValidationLabels[TextType] := TLabel.Create(Self);
    FTextValidationLabels[TextType].Parent := FTabTexts;
    FTextValidationLabels[TextType].SetBounds(24, TopPos + 28, 680, 18);

    FTextPreviews[TextType] := TMemo.Create(Self);
    FTextPreviews[TextType].Parent := FTabTexts;
    FTextPreviews[TextType].SetBounds(24, TopPos + 48, 682, 42);
    FTextPreviews[TextType].ReadOnly := True;
    FTextPreviews[TextType].ScrollBars := ssVertical;
  end;

begin
  ConfigureText(tbOrigem, EditSourceText, BtnSelectSourceText, 28, SSourceTextLabel);
  ConfigureText(tbDestino, EditDestinationText, BtnSelectDestinationText, 138,
    SDestinationTextLabelRequired);
  ConfigureText(tbConsulta1, EditReferenceText1, BtnSelectReferenceText1, 248,
    SReference1TextLabel);
  ConfigureText(tbConsulta2, EditReferenceText2, BtnSelectReferenceText2, 358,
    SReference2TextLabel);
end;

procedure TFrmNewProject.CreateDictionaryPage;
var
  TextType: TTipoTextoBiblico;
  TopPos: Integer;
  TextCaption: string;
  InfoLabel: TLabel;
begin
  TopPos := 22;
  for TextType := tbOrigem to tbConsulta2 do
  begin
    TextCaption := ProjectSetupTextCaption(TextType);

    FStrongDictionaryEdits[TextType] := TLabeledEdit.Create(Self);
    FStrongDictionaryEdits[TextType].Parent := FTabDictionaries;
    FStrongDictionaryEdits[TextType].SetBounds(24, TopPos, 260, 24);
    FStrongDictionaryEdits[TextType].EditLabel.Caption :=
      Format(SStrongDictionaryLabel, [TextCaption]);

    FStrongDictionaryButtons[TextType] := TButton.Create(Self);
    FStrongDictionaryButtons[TextType].Parent := FTabDictionaries;
    FStrongDictionaryButtons[TextType].Caption := SBrowse;
    FStrongDictionaryButtons[TextType].SetBounds(294, TopPos - 1, 80, 26);
    FStrongDictionaryButtons[TextType].Tag := Ord(TextType) * 2;
    FStrongDictionaryButtons[TextType].OnClick := @DictionaryButtonClick;

    FStrongDictionaryLabels[TextType] := TLabel.Create(Self);
    FStrongDictionaryLabels[TextType].Parent := FTabDictionaries;
    FStrongDictionaryLabels[TextType].SetBounds(24, TopPos + 28, 320, 18);

    FMorphologyDictionaryEdits[TextType] := TLabeledEdit.Create(Self);
    FMorphologyDictionaryEdits[TextType].Parent := FTabDictionaries;
    FMorphologyDictionaryEdits[TextType].SetBounds(394, TopPos, 230, 24);
    FMorphologyDictionaryEdits[TextType].EditLabel.Caption :=
      Format(SMorphologyDictionaryLabel, [TextCaption]);

    FMorphologyDictionaryButtons[TextType] := TButton.Create(Self);
    FMorphologyDictionaryButtons[TextType].Parent := FTabDictionaries;
    FMorphologyDictionaryButtons[TextType].Caption := SBrowse;
    FMorphologyDictionaryButtons[TextType].SetBounds(634, TopPos - 1, 72, 26);
    FMorphologyDictionaryButtons[TextType].Tag := Ord(TextType) * 2 + 1;
    FMorphologyDictionaryButtons[TextType].OnClick := @DictionaryButtonClick;

    FMorphologyDictionaryLabels[TextType] := TLabel.Create(Self);
    FMorphologyDictionaryLabels[TextType].Parent := FTabDictionaries;
    FMorphologyDictionaryLabels[TextType].SetBounds(394, TopPos + 28, 310, 18);

    Inc(TopPos, 100);
  end;

  InfoLabel := TLabel.Create(Self);
  InfoLabel.Parent := FTabDictionaries;
  InfoLabel.SetBounds(24, TopPos, 650, 40);
  InfoLabel.Caption := SOptionalDictionaryHint;
end;

procedure TFrmNewProject.CreateTokenizerPage;
begin
  LabelTokenizerVersion.Parent := FTabTokenizer;
  LabelTokenizerVersion.SetBounds(24, 32, 200, 18);

  cbTokenizerVersion.Parent := FTabTokenizer;
  cbTokenizerVersion.SetBounds(24, 54, 260, 24);
end;

procedure TFrmNewProject.CreateStatusPage;
begin
  FStatusDescriptionLabel := TLabel.Create(Self);
  FStatusDescriptionLabel.Parent := FTabStatus;
  FStatusDescriptionLabel.SetBounds(24, 32, 680, 84);
  FStatusDescriptionLabel.AutoSize := False;
  FStatusDescriptionLabel.WordWrap := True;
  FStatusDescriptionLabel.Caption := SStatusCatalogDescription;

  FStatusCatalogSummaryLabel := TLabel.Create(Self);
  FStatusCatalogSummaryLabel.Parent := FTabStatus;
  FStatusCatalogSummaryLabel.SetBounds(24, 132, 360, 18);

  CreateStatusCatalogButton;
  BtnStatusCatalog.Parent := FTabStatus;
  BtnStatusCatalog.SetBounds(24, 162, 180, 27);

  UpdateStatusCatalogSummary;
end;

procedure TFrmNewProject.CreateReviewPage;
begin
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

procedure TFrmNewProject.CreateStatusCatalogButton;
begin
  if Assigned(BtnStatusCatalog) then
    Exit;

  BtnStatusCatalog := TButton.Create(Self);
  BtnStatusCatalog.Caption := SEditStatusCatalog;
  BtnStatusCatalog.OnClick := @BtnStatusCatalogClick;
end;

procedure TFrmNewProject.BtnStatusCatalogClick(Sender: TObject);
var
  Definitions: TStatusDefinitions;
  Counts: TStatusCounts;
  Remaps: TStatusRemaps;
begin
  Definitions := Copy(FStatusDefinitions, 0, Length(FStatusDefinitions));
  Counts := nil;
  if TFrmStatusCatalog.Execute(Definitions, Counts, Remaps) then
  begin
    NormalizeStatusDefinitions(Definitions, True);
    FStatusDefinitions := Definitions;
    FStatusCatalogCustomized := True;
    UpdateStatusCatalogSummary;
    UpdateReview;
  end;
end;

procedure TFrmNewProject.BtnOkClick(Sender: TObject);
begin
  FinishButtonClick(Sender);
end;

procedure TFrmNewProject.BackButtonClick(Sender: TObject);
begin
  if FWizard.ActivePageIndex > 0 then
    FWizard.ActivePageIndex := FWizard.ActivePageIndex - 1;
  UpdateNavigation;
end;

procedure TFrmNewProject.NextButtonClick(Sender: TObject);
begin
  if not ValidateStep(FWizard.ActivePageIndex, True) then
    Exit;

  if FWizard.ActivePageIndex < FWizard.PageCount - 1 then
    FWizard.ActivePageIndex := FWizard.ActivePageIndex + 1;
  UpdateNavigation;
end;

procedure TFrmNewProject.FinishButtonClick(Sender: TObject);
begin
  if ValidateStep(FWizard.PageCount - 1, True) then
    ModalResult := mrOK
  else
    ModalResult := mrNone;
end;

procedure TFrmNewProject.ProjectFileButtonClick(Sender: TObject);
begin
  FProjectSaveDialog.FileName := FProjectFileEdit.Text;
  if FProjectSaveDialog.Execute then
    FProjectFileEdit.Text := FProjectSaveDialog.FileName;
  UpdateReview;
end;

procedure TFrmNewProject.ScenarioChanged(Sender: TObject);
begin
  UpdateSuggestedProjectName;
  if not FStatusCatalogCustomized then
    FStatusDefinitions := DefaultStatusDefinitions(ScenarioFromUI);
  UpdateStatusCatalogSummary;
  UpdateScenarioUI;
  UpdateReview;
end;

procedure TFrmNewProject.DictionaryButtonClick(Sender: TObject);
var
  TextType: TTipoTextoBiblico;
  StrongDictionary: Boolean;
  TargetEdit: TLabeledEdit;
begin
  TextType := TTipoTextoBiblico(TButton(Sender).Tag div 2);
  StrongDictionary := (TButton(Sender).Tag mod 2) = 0;
  TargetEdit := DictionaryEdit(TextType, StrongDictionary);

  OpenDialogSelectText.Title := SChooseDictionaryModuleTitle;
  OpenDialogSelectText.Filter := SDictionaryFilter;
  OpenDialogSelectText.FileName := TargetEdit.Text;
  if OpenDialogSelectText.Execute then
    TargetEdit.Text := OpenDialogSelectText.FileName;

  UpdateDictionaryValidation(TextType, StrongDictionary);
  UpdateReview;
end;

procedure TFrmNewProject.BtnSelectSourceTextClick(Sender: TObject);
begin
  SelectText(EditSourceText);
end;

procedure TFrmNewProject.BtnSelectDestinationTextClick(Sender: TObject);
begin
  SelectText(EditDestinationText);
end;

procedure TFrmNewProject.BtnSelectReferenceText1Click(Sender: TObject);
begin
  SelectText(EditReferenceText1);
end;

procedure TFrmNewProject.BtnSelectReferenceText2Click(Sender: TObject);
begin
  SelectText(EditReferenceText2);
end;

procedure TFrmNewProject.SetTextModuleFilter;
begin
  case RadioGroupScope.ItemIndex of
    0: OpenDialogSelectText.Filter := SOldTestamentFilter;
    1: OpenDialogSelectText.Filter := SNewTestamentFilter;
    2: OpenDialogSelectText.Filter := SWholeBibleFilter;
  else
    OpenDialogSelectText.Filter := SWholeBibleFilter;
  end;
end;

procedure TFrmNewProject.SelectText(Edit: TLabeledEdit);
var
  TextType: TTipoTextoBiblico;
begin
  SetTextModuleFilter;
  OpenDialogSelectText.Title := SChooseBibleModuleTitle;
  OpenDialogSelectText.FileName := Edit.Text;
  if OpenDialogSelectText.Execute then
    Edit.Text := OpenDialogSelectText.FileName;

  TextType := tbOrigem;
  if Edit = EditDestinationText then
    TextType := tbDestino
  else if Edit = EditReferenceText1 then
    TextType := tbConsulta1
  else if Edit = EditReferenceText2 then
    TextType := tbConsulta2;

  UpdateTextValidation(TextType);
  UpdateReview;
end;

function TFrmNewProject.DefaultStatusDefinitions(
  Scenario: TProjectScenario): TStatusDefinitions;
begin
  case Scenario of
    psTranslationProject:
      begin
        SetLength(Result, 5);
        Result[0].Id := 0;
        Result[0].Caption := SStatusAwaitingTranslation;
        Result[0].Color := clRed;
        Result[0].DisplayOrder := 0;
        Result[1].Id := 1;
        Result[1].Caption := SStatusTranslating;
        Result[1].Color := clYellow;
        Result[1].DisplayOrder := 1;
        Result[2].Id := 2;
        Result[2].Caption := SStatusReadyForReview;
        Result[2].Color := $000080FF;
        Result[2].DisplayOrder := 2;
        Result[3].Id := 3;
        Result[3].Caption := SStatusHasPendingIssues;
        Result[3].Color := clFuchsia;
        Result[3].DisplayOrder := 3;
        Result[4].Id := 4;
        Result[4].Caption := SStatusReviewed;
        Result[4].Color := clGreen;
        Result[4].DisplayOrder := 4;
      end;
  else
    Result := BuildLegacyStatusDefinitions([
      SStatusNotAssociated,
      SStatusAssociating,
      SStatusNeedsReview,
      SStatusAssociated
    ]);
  end;
end;

function TFrmNewProject.ScenarioFromUI: TProjectScenario;
begin
  if Assigned(FScenarioRadio) and (FScenarioRadio.ItemIndex = 1) then
    Result := psTranslationProject
  else
    Result := psAssociationProject;
end;

function TFrmNewProject.SuggestedProjectName(Scenario: TProjectScenario): string;
begin
  case Scenario of
    psTranslationProject:
      Result := SNewTranslationProjectName;
  else
    Result := SNewAssociationProjectName;
  end;
end;

procedure TFrmNewProject.UpdateSuggestedProjectName;
var
  NewSuggestion: string;
begin
  NewSuggestion := SuggestedProjectName(ScenarioFromUI);
  if (Trim(EditProjectName.Text) = '') or
    SameText(EditProjectName.Text, FLastSuggestedProjectName) then
  begin
    EditProjectName.Text := NewSuggestion;
    FLastSuggestedProjectName := NewSuggestion;
  end;
end;

function TFrmNewProject.ScopeFromUI: TEscopoTexto;
begin
  case RadioGroupScope.ItemIndex of
    0: Result := etOT;
    1: Result := etNT;
    2: Result := etONT;
  else
    Result := etNT;
  end;
end;

function TFrmNewProject.TextModuleRequired(TextType: TTipoTextoBiblico): Boolean;
begin
  Result := TextType = tbOrigem;
  if TextType = tbDestino then
    Result := ProjectScenarioRequiresDestination(ScenarioFromUI);
end;

function TFrmNewProject.TextEdit(TextType: TTipoTextoBiblico): TLabeledEdit;
begin
  Result := FTextEdits[TextType];
end;

function TFrmNewProject.DictionaryEdit(TextType: TTipoTextoBiblico;
  StrongDictionary: Boolean): TLabeledEdit;
begin
  if StrongDictionary then
    Result := FStrongDictionaryEdits[TextType]
  else
    Result := FMorphologyDictionaryEdits[TextType];
end;

procedure TFrmNewProject.SetValidationLabel(LabelControl: TLabel;
  Severity: TProjectSetupValidationSeverity; const MessageText: string);
begin
  LabelControl.Caption := MessageText;
  case Severity of
    psvsOk:
      LabelControl.Font.Color := clGreen;
    psvsWarning:
      LabelControl.Font.Color := clOlive;
    psvsError:
      LabelControl.Font.Color := clRed;
  end;
end;

procedure TFrmNewProject.UpdateTextValidation(TextType: TTipoTextoBiblico);
var
  Validation: TProjectSetupValidationResult;
begin
  if FSuppressEvents then
    Exit;

  if ProjectSetupValidateModule(TextEdit(TextType).Text, ScopeFromUI,
    GetTokenizerVersion, TextModuleRequired(TextType), Validation) then
    SetValidationLabel(FTextValidationLabels[TextType], Validation.Severity,
      Validation.MessageText)
  else
    SetValidationLabel(FTextValidationLabels[TextType], psvsError,
      Validation.MessageText);

  FTextPreviews[TextType].Text := Validation.PreviewText;
end;

procedure TFrmNewProject.UpdateDictionaryValidation(TextType: TTipoTextoBiblico;
  StrongDictionary: Boolean);
var
  MessageText: string;
  Valid: Boolean;
  LabelControl: TLabel;
  ModuleType: TDictionaryModuleType;
begin
  if StrongDictionary then
  begin
    LabelControl := FStrongDictionaryLabels[TextType];
    ModuleType := dmtStrong;
  end
  else
  begin
    LabelControl := FMorphologyDictionaryLabels[TextType];
    ModuleType := dmtMorphology;
  end;

  Valid := ProjectSetupValidateDictionary(
    DictionaryEdit(TextType, StrongDictionary).Text, ModuleType, MessageText);

  if Valid then
    SetValidationLabel(LabelControl, psvsOk, MessageText)
  else
    SetValidationLabel(LabelControl, psvsError, MessageText);
end;

procedure TFrmNewProject.UpdateScenarioUI;
begin
  if not Assigned(FScenarioRadio) then
    Exit;

  if FScenarioRadio.ItemIndex = 1 then
    FScenarioDescription.Text := SScenarioTranslationDescription
  else
    FScenarioDescription.Text := SScenarioAssociationDescription;

  if Assigned(EditDestinationText) then
  begin
    if ProjectScenarioRequiresDestination(ScenarioFromUI) then
      EditDestinationText.EditLabel.Caption := SDestinationTextLabelRequired
    else
      EditDestinationText.EditLabel.Caption := SDestinationTextLabelOptional;
  end;

  UpdateTextValidation(tbDestino);
end;

procedure TFrmNewProject.UpdateStatusCatalogSummary;
begin
  if Assigned(FStatusCatalogSummaryLabel) then
    FStatusCatalogSummaryLabel.Caption := Format(SStatusCatalogCurrent,
      [Length(FStatusDefinitions)]);
end;

procedure TFrmNewProject.UpdateNavigation;
begin
  if not Assigned(FWizard) then
    Exit;

  FBackButton.Enabled := FWizard.ActivePageIndex > 0;
  FNextButton.Enabled := FWizard.ActivePageIndex < FWizard.PageCount - 1;
  FFinishButton.Enabled := FWizard.ActivePageIndex = FWizard.PageCount - 1;

  if FWizard.ActivePage = FTabReview then
    UpdateReview;
end;

procedure TFrmNewProject.UpdateReview;
var
  Draft: TProjectSetupDraft;
  MessageText: string;
  TextType: TTipoTextoBiblico;
  TextPath: string;
  HasDictionary: Boolean;

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

begin
  if not Assigned(FReviewMemo) then
    Exit;

  Draft := BuildDraft;
  FReviewMemo.Clear;
  FReviewMemo.Lines.BeginUpdate;
  try
    if ProjectSetupValidateDraft(Draft, MessageText) then
      AppendReviewLine(SReviewReady, [fsBold], clGreen)
    else
      AppendReviewLine(Format(SReviewBlocked, [MessageText]), [fsBold], clRed);

    AppendSection(SReviewSectionProject);
    AppendLabelValueLine(Format(SReviewScenario,
      [ProjectScenarioCaption(Draft.Scenario)]));
    AppendLabelValueLine(Format(SReviewProject, [Draft.ProjectName]));
    AppendLabelValueLine(Format(SReviewProjectFile, [Draft.ProjectFileName]));
    AppendLabelValueLine(Format(SReviewScope,
      [ProjectSetupScopeCaption(Draft.Scope)]));
    AppendLabelValueLine(Format(SReviewTokenizer, [
      TTokenizerFactory.GetVersionCaption(Draft.TokenizerVersion)]));
    AppendLabelValueLine(Format(SStatusCatalogReady,
      [Length(Draft.StatusDefinitions)]));

    AppendSection(SReviewSectionTexts);
    for TextType := tbOrigem to tbConsulta2 do
    begin
      TextPath := Draft.Texts[TextType].ModulePath;
      if TextPath = '' then
        TextPath := SNotSelected;
      AppendLabelValueLine(Format(SReviewText,
        [ProjectSetupTextCaption(TextType), TextPath]));
    end;

    AppendSection(SReviewSectionDictionaries);
    HasDictionary := False;
    for TextType := tbOrigem to tbConsulta2 do
    begin
      TextPath := Draft.Texts[TextType].StrongDictionaryPath;
      if TextPath <> '' then
      begin
        HasDictionary := True;
        AppendLabelValueLine(Format(SReviewDictionary,
          [ProjectSetupTextCaption(TextType), SStrong, TextPath]));
      end;

      TextPath := Draft.Texts[TextType].MorphologyDictionaryPath;
      if TextPath <> '' then
      begin
        HasDictionary := True;
        AppendLabelValueLine(Format(SReviewDictionary,
          [ProjectSetupTextCaption(TextType), SMorphology, TextPath]));
      end;
    end;

    if not HasDictionary then
      AppendReviewLine(SNoDictionariesSelected, [fsItalic], clGrayText);

    FReviewMemo.SelStart := 0;
  finally
    FReviewMemo.Lines.EndUpdate;
  end;
end;

function TFrmNewProject.ValidateStep(StepIndex: Integer; ShowDialog: Boolean): Boolean;
var
  TextType: TTipoTextoBiblico;
  Validation: TProjectSetupValidationResult;
  MessageText: string;
  StatusError: string;

  procedure Fail(const Msg: string);
  begin
    Result := False;
    if ShowDialog then
      MessageDlg(SNewProject, Msg, mtWarning, [mbOK], 0);
  end;

begin
  Result := True;
  case StepIndex of
    1:
      begin
        if Trim(EditProjectName.Text) = '' then
          Fail(SChooseAName)
        else if Trim(FProjectFileEdit.Text) = '' then
          Fail(SChooseProjectFile)
        else if not SameText(ExtractFileExt(FProjectFileEdit.Text), '.bib') then
          Fail(SProjectFileExtension)
        else if FileExists(FProjectFileEdit.Text) then
          Fail(SProjectFileAlreadyExists);
      end;
    2:
      begin
        for TextType := tbOrigem to tbConsulta2 do
        begin
          UpdateTextValidation(TextType);
          if not ProjectSetupValidateModule(TextEdit(TextType).Text, ScopeFromUI,
            GetTokenizerVersion, TextModuleRequired(TextType), Validation) then
          begin
            Fail(Format('%s: %s',
              [ProjectSetupTextCaption(TextType), Validation.MessageText]));
            Exit;
          end;
        end;
      end;
    3:
      begin
        for TextType := tbOrigem to tbConsulta2 do
        begin
          UpdateDictionaryValidation(TextType, True);
          if not ProjectSetupValidateDictionary(
            FStrongDictionaryEdits[TextType].Text, dmtStrong, MessageText) then
          begin
            Fail(Format('%s Strong: %s',
              [ProjectSetupTextCaption(TextType), MessageText]));
            Exit;
          end;

          UpdateDictionaryValidation(TextType, False);
          if not ProjectSetupValidateDictionary(
            FMorphologyDictionaryEdits[TextType].Text, dmtMorphology, MessageText) then
          begin
            Fail(Format('%s Morphology: %s',
              [ProjectSetupTextCaption(TextType), MessageText]));
            Exit;
          end;
        end;
      end;
    5:
      begin
        if not ValidateStatusDefinitions(GetStatusDefinitions, StatusError) then
          Fail(StatusError);
      end;
    6:
      begin
        UpdateReview;
        if not ProjectSetupValidateDraft(BuildDraft, MessageText) then
          Fail(MessageText);
      end;
  end;
end;

procedure TFrmNewProject.LoadTokenizerVersions(SelectedVersion: Integer);
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

function TFrmNewProject.GetTokenizerVersion: Integer;
begin
  if cbTokenizerVersion.ItemIndex >= 0 then
    Result := PtrInt(cbTokenizerVersion.Items.Objects[cbTokenizerVersion.ItemIndex])
  else
    Result := TTokenizerFactory.LATEST_VERSION;
end;

function TFrmNewProject.GetStatusDefinitions: TStatusDefinitions;
begin
  Result := Copy(FStatusDefinitions, 0, Length(FStatusDefinitions));
  NormalizeStatusDefinitions(Result, True);
end;

function TFrmNewProject.HasCustomStatusCatalog: Boolean;
begin
  Result := FStatusCatalogCustomized;
end;

function TFrmNewProject.BuildDraft: TProjectSetupDraft;
var
  TextType: TTipoTextoBiblico;
begin
  Result := DefaultProjectSetupDraft(EditProjectName.Text);
  Result.Scenario := ScenarioFromUI;
  Result.ProjectName := EditProjectName.Text;
  Result.ProjectFileName := FProjectFileEdit.Text;
  Result.Scope := ScopeFromUI;
  Result.TokenizerVersion := GetTokenizerVersion;
  Result.HasCustomStatusCatalog := FStatusCatalogCustomized or
    (Result.Scenario = psTranslationProject);
  Result.StatusDefinitions := GetStatusDefinitions;
  for TextType := tbOrigem to tbConsulta2 do
  begin
    Result.Texts[TextType].ModulePath := TextEdit(TextType).Text;
    Result.Texts[TextType].StrongDictionaryPath :=
      FStrongDictionaryEdits[TextType].Text;
    Result.Texts[TextType].MorphologyDictionaryPath :=
      FMorphologyDictionaryEdits[TextType].Text;
  end;
end;

function TFrmNewProject.GetDraft: TProjectSetupDraft;
begin
  Result := BuildDraft;
end;

procedure TFrmNewProject.ResetDraft(const DefaultProjectName: string);
var
  TextType: TTipoTextoBiblico;
begin
  FSuppressEvents := True;
  try
    FProjectFileEdit.Text := '';
    FScenarioRadio.ItemIndex := 0;
    if DefaultProjectName <> '' then
      FLastSuggestedProjectName := DefaultProjectName
    else
      FLastSuggestedProjectName := SuggestedProjectName(psAssociationProject);
    EditProjectName.Text := FLastSuggestedProjectName;
    RadioGroupScope.ItemIndex := 1;
    LoadTokenizerVersions(TTokenizerFactory.LATEST_VERSION);
    FStatusDefinitions := DefaultStatusDefinitions(psAssociationProject);
    FStatusCatalogCustomized := False;

    for TextType := tbOrigem to tbConsulta2 do
    begin
      TextEdit(TextType).Text := '';
      FTextPreviews[TextType].Clear;
      FStrongDictionaryEdits[TextType].Text := '';
      FMorphologyDictionaryEdits[TextType].Text := '';
    end;
  finally
    FSuppressEvents := False;
  end;

  FWizard.ActivePageIndex := 0;
  UpdateScenarioUI;
  UpdateStatusCatalogSummary;
  for TextType := tbOrigem to tbConsulta2 do
  begin
    UpdateTextValidation(TextType);
    UpdateDictionaryValidation(TextType, True);
    UpdateDictionaryValidation(TextType, False);
  end;
  UpdateReview;
  UpdateNavigation;
end;

procedure TFrmNewProject.Translate;
var
  SelectedVersion: Integer;
  SelectedScope: Integer;
  SelectedScenario: TProjectScenario;
  TextType: TTipoTextoBiblico;
  TextCaption: string;
begin
  SelectedVersion := GetTokenizerVersion;
  SelectedScope := RadioGroupScope.ItemIndex;
  SelectedScenario := ScenarioFromUI;
  if not FStatusCatalogCustomized then
    FStatusDefinitions := DefaultStatusDefinitions(SelectedScenario);

  if Assigned(FScenarioRadio) then
  begin
    FScenarioRadio.Items.Clear;
    FScenarioRadio.Items.Add(SScenarioAssociationTitle);
    FScenarioRadio.Items.Add(SScenarioTranslationTitle);
    if SelectedScenario = psTranslationProject then
      FScenarioRadio.ItemIndex := 1
    else
      FScenarioRadio.ItemIndex := 0;
  end;

  with RadioGroupScope do
  begin
    Items.Clear;
    Items.Add(SOldTestament);
    Items.Add(SNewTestament);
    Items.Add(SWholeBible);
    if SelectedScope >= 0 then
      ItemIndex := SelectedScope
    else
      ItemIndex := 1;
  end;

  Caption := SNewProject;
  if Assigned(FTabScenario) then FTabScenario.Caption := SScenarioStep;
  if Assigned(FTabProject) then FTabProject.Caption := SProjectStep;
  if Assigned(FTabTexts) then FTabTexts.Caption := STextsStep;
  if Assigned(FTabDictionaries) then FTabDictionaries.Caption := SDictionariesStep;
  if Assigned(FTabTokenizer) then FTabTokenizer.Caption := STokenizerStep;
  if Assigned(FTabStatus) then FTabStatus.Caption := SStatusStep;
  if Assigned(FTabReview) then FTabReview.Caption := SReviewStep;

  EditProjectName.EditLabel.Caption := SProjectNameLabel;
  if Assigned(FProjectFileEdit) then
    FProjectFileEdit.EditLabel.Caption := SProjectFileLabel;
  if Assigned(FProjectFileButton) then
    FProjectFileButton.Caption := SBrowse;
  if Assigned(FProjectSaveDialog) then
    FProjectSaveDialog.Filter := SProjectFileFilter;
  RadioGroupScope.Caption := SScopeLabel;
  LabelTokenizerVersion.Caption := STokenizerVersion;
  if Assigned(BtnStatusCatalog) then
    BtnStatusCatalog.Caption := SEditStatusCatalog;
  if Assigned(FStatusDescriptionLabel) then
    FStatusDescriptionLabel.Caption := SStatusCatalogDescription;
  UpdateStatusCatalogSummary;
  if Assigned(FBackButton) then FBackButton.Caption := SBack;
  if Assigned(FNextButton) then FNextButton.Caption := SNext;
  if Assigned(FFinishButton) then FFinishButton.Caption := SFinish;
  if Assigned(FCancelButton) then FCancelButton.Caption := SCancel;
  OpenDialogSelectText.Title := SChooseBibleModuleTitle;

  EditSourceText.EditLabel.Caption := SSourceTextLabel;
  EditReferenceText1.EditLabel.Caption := SReference1TextLabel;
  EditReferenceText2.EditLabel.Caption := SReference2TextLabel;
  for TextType := tbOrigem to tbConsulta2 do
  begin
    if Assigned(FTextButtons[TextType]) then
      FTextButtons[TextType].Caption := SBrowse;

    TextCaption := ProjectSetupTextCaption(TextType);
    if Assigned(FStrongDictionaryEdits[TextType]) then
      FStrongDictionaryEdits[TextType].EditLabel.Caption :=
        Format(SStrongDictionaryLabel, [TextCaption]);
    if Assigned(FMorphologyDictionaryEdits[TextType]) then
      FMorphologyDictionaryEdits[TextType].EditLabel.Caption :=
        Format(SMorphologyDictionaryLabel, [TextCaption]);
    if Assigned(FStrongDictionaryButtons[TextType]) then
      FStrongDictionaryButtons[TextType].Caption := SBrowse;
    if Assigned(FMorphologyDictionaryButtons[TextType]) then
      FMorphologyDictionaryButtons[TextType].Caption := SBrowse;
  end;

  LoadTokenizerVersions(SelectedVersion);
  UpdateScenarioUI;
end;

initialization
{$IFNDEF FPCUNIT}
  {$I FormNewProject.lrs}
{$ENDIF}

end.
