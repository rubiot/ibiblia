unit FormProjectExport;

{$mode objfpc}{$H+}

interface

uses
  ui_style, Classes, SysUtils, Math, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Spin, BibleScope, Project, LCLTranslator,
  LCLIntf, LazUTF8, UnitPleaseWait, project_exporter, ConcordanceExport,
  export_destination_history;

type

  TProjectExportForm = class(TForm)
    BtnExport: TButton;
    BtnCancel: TButton;
    CbExportUnassociatedAsItalics: TCheckBox;
    CbExportMorphology: TCheckBox;
    CbExportComments: TCheckBox;
    CbMarkReusedStrongs: TCheckBox;
    CbMarkUntranslatedStrongs: TCheckBox;
    LblTaggedDestinationProperties: TLabel;
    LblInterlinearSourceProperties: TLabel;
    lblConcordanceDescription: TLabel;
    lblConcordanceMaxCharacters: TLabel;
    lblConcordanceOutput: TLabel;
    EditAbbreviation: TLabeledEdit;
    MemoSourceFooter: TMemo;
    MemoDestinationFooter: TMemo;
    ExportPageControl: TPageControl;
    rgConcordanceContext: TRadioGroup;
    rgConcordanceType: TRadioGroup;
    SaveBibleModuleDialog: TSaveDialog;
    SaveConcordanceDialog: TSaveDialog;
    seConcordanceMaxCharacters: TSpinEdit;
    TaggedDestinationTab: TTabSheet;
    InterlinearSourceTab: TTabSheet;
    ConcordanceTab: TTabSheet;
    procedure ExportButtonClick(Sender: TObject);
    procedure ConcordanceContextClick(Sender: TObject);
    procedure ConcordanceTypeClick(Sender: TObject);
  private
    FProject: TProject;
    FAnalyticalAbbreviation: string;
    FSyntheticAbbreviation: string;
    FLastConcordanceType: Integer;
    function BuildTaggedDestinationOptions: TOpcoesExportacao;
    function BuildConcordanceOptions: TOpcoesExportacao;
    function DefaultConcordanceAbbreviation: string;
    function ConcordanceFileSuffix: string;
    function ConcordanceDestinationKind: TExportDestinationKind;
    function SelectedConcordanceContextMode: TConcordanceContextMode;
    function SuggestedBaseFileName(const Suffix: string): string;
    procedure ExportTaggedDestinationText;
    procedure ExportInterlinearSourceText;
    procedure ExportConcordance;
    procedure PersistTaggedDestinationOptions;
    procedure PersistInterlinearOptions;
    procedure PersistConcordanceOptions(const Abbreviation: string);
    procedure SetSelectedConcordanceContextMode(
      AValue: TConcordanceContextMode);
    procedure UpdateConcordanceAbbreviationDefault;
    procedure UpdateConcordanceContextControls;
    procedure UpdateConcordanceDescription;
    procedure ConcordanceExportProgress(Phase: TConcordanceExportPhase;
      Current, Total: Integer);
    procedure ConfigureBibleSaveDialog(const Suffix: string;
      Kind: TExportDestinationKind);
    procedure ConfigureConcordanceSaveDialog;
    function ConfirmExportDestination(Dialog: TSaveDialog;
      Kind: TExportDestinationKind; out PreferenceWarning: string): Boolean;
  protected
    function PerformTaggedDestinationExport(const FileName: string;
      Options: TOpcoesExportacao): TExportResult; virtual;
    function PerformInterlinearSourceExport(const FileName: string;
      Options: TOpcoesExportacao): TExportResult; virtual;
    function PerformConcordanceExport(const FileName: string;
      Options: TOpcoesExportacao; const Abbreviation: string;
      ContextMode: TConcordanceContextMode;
      ContextMaxCharacters: Integer): TExportResult; virtual;
    function BuildExportResultMessage(const ExportResult: TExportResult;
      const PreferenceWarning: string; out DialogTitle: string;
      out DialogType: TMsgDlgType): string;
  public
    procedure SetProject(Project: TProject);
    procedure ShowExportResult(const ExportResult: TExportResult;
      const PreferenceWarning: string = ''); virtual;
  public
    procedure AfterConstruction; override;
  end;

var
  ProjectExportForm: TProjectExportForm;

resourcestring
  STheWordOTModule = 'theWord Bible module (*.ot)';
  STheWordNTModule = 'theWord Bible module (*.nt)';
  STheWordONTModule = 'theWord Bible module (*.ont)';
  SMySwordModule   = 'MySword Bible module (*.bbl.mybible)';
  STheWordDictionaryModule = 'theWord dictionary module (*.dct.twm)';
  SSaveModuleAs = 'Save module as...';
  SAnalyticalConcordance = 'AnalyticalConcordance';
  SSyntheticConcordance = 'SyntheticConcordance';
  SExportTaggedDestinationTitle = 'Export project';
  SExportTaggedDestinationProgress = 'Exporting tagged destination text...';
  SExportInterlinearTitle = 'Export project';
  SExportInterlinearProgress = 'Exporting interlinear source text...';
  SExportConcordanceTitle = 'Export project';
  SExportConcordanceProgress = 'Exporting concordance...';
  SConcordanceReadingAssociations = 'Reading associations...';
  SConcordanceFormattingEntries = 'Formatting concordance entries...';
  SConcordanceWritingModule = 'Writing dictionary module...';
  SConcordanceFinalizing = 'Finalizing export...';
  SConcordanceAnalyticalDescription =
    'Shows each source form separately, with its translations and verse references.';
  SConcordanceSyntheticDescription =
    'Groups all source forms by Strong''s number for a compact translation overview.';
  SConcordanceTypeCaption = 'Concordance type';
  SConcordanceAnalyticalType = 'Analytical concordance';
  SConcordanceSyntheticType = 'Synthetic concordance';
  SConcordanceOutputDescription =
    'The exported dictionary is organized by Strong''s number, with compact frequency totals and clickable verse references.';
  SConcordanceReferenceTextCaption = 'Reference text';
  SConcordanceReferencesOnly = 'References only';
  SConcordanceRelevantExcerpt = 'Relevant excerpt';
  SConcordanceFullVerse = 'Full verse';
  SConcordanceMaxExcerptCharacters =
    'Maximum excerpt length (characters)';
  SExportCompletedTitle = 'Export completed';
  SExportCompletedPrompt = 'Export completed successfully.'#13#10#13#10'%s'#13#10#13#10'Open the exported file location?';
  SExportCompletedMessage = 'Export completed successfully.';
  SExportFailedTitle = 'Export failed';
  SExportWarningsTitle = 'Export completed with warnings';
  SExportWarningsMessage = 'The exported module contains %d warning(s).';
  SExportReportLocation = 'Warnings report: %s';
  SExportOpenLocation = 'Open the exported file location?';
  SExportDestinationPreferenceWarning = 'Could not remember the export destination locally: %s';

implementation

uses formPrincipal;

const
  SConcordanceContextModeKey =
    'opcoes.exportar.concordancia.contexto.modo';
  SConcordanceContextMaxCharactersKey =
    'opcoes.exportar.concordancia.contexto.max.caracteres';
  SConcordanceContextReferencesOnlyValue = 'references-only';
  SConcordanceContextRelevantExcerptValue = 'relevant-excerpt';
  SConcordanceContextFullVerseValue = 'full-verse';

function BoolToConfigValue(Value: Boolean): string;
begin
  if Value then
    Result := '1'
  else
    Result := '0';
end;

procedure TProjectExportForm.ExportButtonClick(Sender: TObject);
begin
  ModalResult := mrNone;
  case ExportPageControl.PageIndex of
    0: ExportTaggedDestinationText;
    1: ExportInterlinearSourceText;
    2: ExportConcordance;
  end;
end;

procedure TProjectExportForm.ConcordanceContextClick(Sender: TObject);
begin
  UpdateConcordanceContextControls;
end;

procedure TProjectExportForm.ConcordanceTypeClick(Sender: TObject);
begin
  if FLastConcordanceType = 0 then
    FAnalyticalAbbreviation := Trim(EditAbbreviation.Text)
  else if FLastConcordanceType = 1 then
    FSyntheticAbbreviation := Trim(EditAbbreviation.Text);

  if rgConcordanceType.ItemIndex = 1 then
    EditAbbreviation.Text := FSyntheticAbbreviation
  else
    EditAbbreviation.Text := FAnalyticalAbbreviation;
  FLastConcordanceType := rgConcordanceType.ItemIndex;
  UpdateConcordanceAbbreviationDefault;
  UpdateConcordanceDescription;
end;

procedure TProjectExportForm.UpdateConcordanceDescription;
begin
  if rgConcordanceType.ItemIndex = 1 then
    lblConcordanceDescription.Caption := SConcordanceSyntheticDescription
  else
    lblConcordanceDescription.Caption := SConcordanceAnalyticalDescription;
end;

procedure TProjectExportForm.ConcordanceExportProgress(
  Phase: TConcordanceExportPhase; Current, Total: Integer);
begin
  case Phase of
    cepReadingAssociations:
      FormPleaseWait.SetMessage(SConcordanceReadingAssociations);
    cepFormattingEntries:
      FormPleaseWait.SetMessage(SConcordanceFormattingEntries);
    cepWritingModule:
      FormPleaseWait.SetMessage(SConcordanceWritingModule);
    cepFinalizing:
      FormPleaseWait.SetMessage(SConcordanceFinalizing);
  end;

  if Total > 0 then
  begin
    FormPleaseWait.ProgressBar1.Style := pbstNormal;
    FormPleaseWait.ProgressBar1.Max := Total;
    FormPleaseWait.ProgressBar1.Position := Min(Current, Total);
    FormPleaseWait.SetDetail(Format('%d / %d', [Current, Total]));
  end
  else
  begin
    FormPleaseWait.ProgressBar1.Style := pbstMarquee;
    FormPleaseWait.SetDetail('');
  end;
  FormPleaseWait.ProgressBar1.Update;
  Application.ProcessMessages;
end;

function TProjectExportForm.BuildTaggedDestinationOptions: TOpcoesExportacao;
begin
  Result := [];
  if CbExportUnassociatedAsItalics.Checked then
    Include(Result, oeExportarNAComoItalicos);
  if CbExportComments.Checked then
    Include(Result, oeExportarComentarios);
  if CbExportMorphology.Checked then
    Include(Result, oeExportarMorfologia);
  if CbMarkReusedStrongs.Checked then
    Include(Result, oeStrongsReutilizados);
  if CbMarkUntranslatedStrongs.Checked then
    Include(Result, oeStrongsNaoTraduzidos);
end;

function TProjectExportForm.BuildConcordanceOptions: TOpcoesExportacao;
begin
  Result := [];
  if rgConcordanceType.ItemIndex < 0 then
    rgConcordanceType.ItemIndex := 0;
  if rgConcordanceType.ItemIndex = 0 then
    Include(Result, oeConcordDetalhada);
end;

function TProjectExportForm.DefaultConcordanceAbbreviation: string;
begin
  if rgConcordanceType.ItemIndex = 1 then
    Result := SSyntheticConcordance
  else
    Result := SAnalyticalConcordance;
end;

function TProjectExportForm.ConcordanceFileSuffix: string;
begin
  if rgConcordanceType.ItemIndex = 1 then
    Result := 'synthetic-concordance'
  else
    Result := 'analytical-concordance';
end;

function TProjectExportForm.ConcordanceDestinationKind: TExportDestinationKind;
begin
  if rgConcordanceType.ItemIndex = 1 then
    Result := edSyntheticConcordance
  else
    Result := edAnalyticalConcordance;
end;

function TProjectExportForm.SelectedConcordanceContextMode:
  TConcordanceContextMode;
begin
  case rgConcordanceContext.ItemIndex of
    1: Result := ccmRelevantExcerpt;
    2: Result := ccmFullVerse;
    else
      Result := ccmReferencesOnly;
  end;
end;

function TProjectExportForm.SuggestedBaseFileName(const Suffix: string): string;
var
  BaseName: string;
  Directory: string;
begin
  BaseName := '';
  Directory := '';
  if Assigned(FProject) then
  begin
    BaseName := ChangeFileExt(ExtractFileName(FProject.FileName), '');
    Directory := ExtractFilePath(FProject.FileName);
  end;

  if BaseName = '' then
    BaseName := 'iBiblia';

  Result := Directory + BaseName;
  if Suffix <> '' then
    Result := Result + '-' + Suffix;
end;

function TProjectExportForm.ConfirmExportDestination(Dialog: TSaveDialog;
  Kind: TExportDestinationKind; out PreferenceWarning: string): Boolean;
var
  ErrorMessage: string;
begin
  PreferenceWarning := '';
  Result := Dialog.Execute;
  if not Result then Exit;
  Dialog.FileName := ExpandFileName(Dialog.FileName);
  if not TryRememberExportDestination(opts, FProject.FileName, Kind,
    Dialog.FileName, ErrorMessage) then
    PreferenceWarning := Format(SExportDestinationPreferenceWarning, [ErrorMessage]);
end;

procedure TProjectExportForm.ConfigureBibleSaveDialog(const Suffix: string;
  Kind: TExportDestinationKind);
var
  SuggestedFileName: string;
begin
  case FProject.Escopo of
    etOT:
    begin
      SaveBibleModuleDialog.DefaultExt := '.ot';
      SaveBibleModuleDialog.Filter := STheWordOTModule + '|*.ot' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
    etNT:
    begin
      SaveBibleModuleDialog.DefaultExt := '.nt';
      SaveBibleModuleDialog.Filter := STheWordNTModule + '|*.nt' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
    etONT:
    begin
      SaveBibleModuleDialog.DefaultExt := '.ont';
      SaveBibleModuleDialog.Filter := STheWordONTModule + '|*.ont' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
  end;
  SaveBibleModuleDialog.Title := SSaveModuleAs;
  SaveBibleModuleDialog.FilterIndex := 1;
  SuggestedFileName := SuggestedBaseFileName(Suffix) + SaveBibleModuleDialog.DefaultExt;
  SaveBibleModuleDialog.FileName := ReadExportDestination(opts,
    FProject.FileName, Kind, SuggestedFileName);
  SaveBibleModuleDialog.InitialDir := ExtractFilePath(SaveBibleModuleDialog.FileName);
  SaveBibleModuleDialog.Options := SaveBibleModuleDialog.Options + [ofOverwritePrompt, ofPathMustExist];
end;

procedure TProjectExportForm.ConfigureConcordanceSaveDialog;
var
  SuggestedFileName: string;
begin
  SaveConcordanceDialog.Title := SSaveModuleAs;
  SaveConcordanceDialog.DefaultExt := '.dct.twm';
  SaveConcordanceDialog.Filter := STheWordDictionaryModule + '|*.dct.twm';
  SaveConcordanceDialog.FilterIndex := 1;
  SuggestedFileName := SuggestedBaseFileName(ConcordanceFileSuffix) + '.dct.twm';
  SaveConcordanceDialog.FileName := ReadExportDestination(opts,
    FProject.FileName, ConcordanceDestinationKind, SuggestedFileName);
  SaveConcordanceDialog.InitialDir := ExtractFilePath(SaveConcordanceDialog.FileName);
  SaveConcordanceDialog.Options := SaveConcordanceDialog.Options + [ofOverwritePrompt, ofPathMustExist];
end;

procedure TProjectExportForm.PersistTaggedDestinationOptions;
begin
  FProject.SetInfo('propriedades.destino', MemoDestinationFooter.Text);
  FProject.SetInfo('opcoes.exportar.comentarios', BoolToConfigValue(CbExportComments.Checked));
  FProject.SetInfo('opcoes.exportar.morfologia', BoolToConfigValue(CbExportMorphology.Checked));
  FProject.SetInfo('opcoes.exportar.na.como.italicos', BoolToConfigValue(CbExportUnassociatedAsItalics.Checked));
  FProject.SetInfo('opcoes.exportar.strongs.reutilizados', BoolToConfigValue(CbMarkReusedStrongs.Checked));
  FProject.SetInfo('opcoes.exportar.strongs.nao.traduzidos', BoolToConfigValue(CbMarkUntranslatedStrongs.Checked));
end;

procedure TProjectExportForm.PersistInterlinearOptions;
begin
  FProject.SetInfo('propriedades.origem', MemoSourceFooter.Text);
end;

procedure TProjectExportForm.PersistConcordanceOptions(const Abbreviation: string);
var
  ContextMode: TConcordanceContextMode;
  ContextModeValue: string;
begin
  if rgConcordanceType.ItemIndex = 1 then
    FSyntheticAbbreviation := Abbreviation
  else
    FAnalyticalAbbreviation := Abbreviation;
  FProject.SetInfo('propriedades.abbrev', Abbreviation);
  FProject.SetInfo('opcoes.exportar.concordancia.analitica.abbrev',
    FAnalyticalAbbreviation);
  FProject.SetInfo('opcoes.exportar.concordancia.sintetica.abbrev',
    FSyntheticAbbreviation);
  FProject.SetInfo('opcoes.exportar.concordancia.detalhada',
    BoolToConfigValue(oeConcordDetalhada in BuildConcordanceOptions));
  ContextMode := SelectedConcordanceContextMode;
  case ContextMode of
    ccmRelevantExcerpt:
      ContextModeValue := SConcordanceContextRelevantExcerptValue;
    ccmFullVerse:
      ContextModeValue := SConcordanceContextFullVerseValue;
    else
      ContextModeValue := SConcordanceContextReferencesOnlyValue;
  end;
  FProject.SetInfo(SConcordanceContextModeKey, ContextModeValue);
  FProject.SetInfo(SConcordanceContextMaxCharactersKey,
    IntToStr(seConcordanceMaxCharacters.Value));
  FProject.SetInfo('opcoes.exportar.concordancia.contexto',
    BoolToConfigValue(ContextMode <> ccmReferencesOnly));
end;

procedure TProjectExportForm.SetSelectedConcordanceContextMode(
  AValue: TConcordanceContextMode);
begin
  rgConcordanceContext.ItemIndex := Ord(AValue);
  UpdateConcordanceContextControls;
end;

procedure TProjectExportForm.UpdateConcordanceAbbreviationDefault;
var
  CurrentText: string;
begin
  CurrentText := Trim(EditAbbreviation.Text);
  if (CurrentText = '') or SameText(CurrentText, SAnalyticalConcordance) or
    SameText(CurrentText, SSyntheticConcordance) then
    EditAbbreviation.Text := DefaultConcordanceAbbreviation;
end;

procedure TProjectExportForm.UpdateConcordanceContextControls;
var
  RelevantExcerptSelected: Boolean;
begin
  RelevantExcerptSelected :=
    SelectedConcordanceContextMode = ccmRelevantExcerpt;
  lblConcordanceMaxCharacters.Enabled := RelevantExcerptSelected;
  seConcordanceMaxCharacters.Enabled := RelevantExcerptSelected;
end;

procedure TProjectExportForm.ExportTaggedDestinationText;
var
  Options: TOpcoesExportacao;
  ExportResult: TExportResult;
  PreferenceWarning: string;
begin
  Options := BuildTaggedDestinationOptions;
  ConfigureBibleSaveDialog('tagged-destination', edTaggedTheWord);

  if ConfirmExportDestination(SaveBibleModuleDialog, edTaggedTheWord,
    PreferenceWarning) then
  begin
    PersistTaggedDestinationOptions;
    ExportResult := PerformTaggedDestinationExport(
      SaveBibleModuleDialog.FileName, Options);
    ShowExportResult(ExportResult, PreferenceWarning);
  end;
end;

procedure TProjectExportForm.ExportInterlinearSourceText;
var
  ExportResult: TExportResult;
  PreferenceWarning: string;
begin
  ConfigureBibleSaveDialog('interlinear-source', edInterlinearTheWord);

  if ConfirmExportDestination(SaveBibleModuleDialog, edInterlinearTheWord,
    PreferenceWarning) then
  begin
    PersistInterlinearOptions;
    ExportResult := PerformInterlinearSourceExport(
      SaveBibleModuleDialog.FileName, []);
    ShowExportResult(ExportResult, PreferenceWarning);
  end;
end;

procedure TProjectExportForm.ExportConcordance;
var
  Options: TOpcoesExportacao;
  Abbreviation: string;
  ContextMode: TConcordanceContextMode;
  ExportResult: TExportResult;
  PreferenceWarning: string;
begin
  Options := BuildConcordanceOptions;
  ContextMode := SelectedConcordanceContextMode;
  UpdateConcordanceAbbreviationDefault;

  Abbreviation := Trim(EditAbbreviation.Text);
  if Abbreviation = '' then
    Abbreviation := DefaultConcordanceAbbreviation;

  ConfigureConcordanceSaveDialog;
  if ConfirmExportDestination(SaveConcordanceDialog, ConcordanceDestinationKind,
    PreferenceWarning) then
  begin
    EditAbbreviation.Text := Abbreviation;
    ExportResult := PerformConcordanceExport(SaveConcordanceDialog.FileName,
      Options, Abbreviation, ContextMode, seConcordanceMaxCharacters.Value);
    if ExportResult.Status <> esFailed then
      PersistConcordanceOptions(Abbreviation);
    ShowExportResult(ExportResult, PreferenceWarning);
  end;
end;

function TProjectExportForm.PerformTaggedDestinationExport(
  const FileName: string; Options: TOpcoesExportacao): TExportResult;
begin
  FormPleaseWait.BeginOperation(Self, SExportTaggedDestinationTitle,
    SExportTaggedDestinationProgress, 1, FileName);
  try
    Result := FProject.ExportDestinationTextWithStrongs(FileName,
      FormPleaseWait.ProgressBar1, Options);
  finally
    FormPleaseWait.FinishOperation;
  end;
end;

function TProjectExportForm.PerformInterlinearSourceExport(
  const FileName: string; Options: TOpcoesExportacao): TExportResult;
begin
  FormPleaseWait.BeginOperation(Self, SExportInterlinearTitle,
    SExportInterlinearProgress, 1, FileName);
  try
    Result := FProject.ExportInterlinearText(FileName,
      FormPleaseWait.ProgressBar1, Options);
  finally
    FormPleaseWait.FinishOperation;
  end;
end;

function TProjectExportForm.PerformConcordanceExport(const FileName: string;
  Options: TOpcoesExportacao; const Abbreviation: string;
  ContextMode: TConcordanceContextMode;
  ContextMaxCharacters: Integer): TExportResult;
var
  UseIndexedOccurrences: Boolean;
begin
  UseIndexedOccurrences := not (oeConcordDetalhada in Options) and
    (ContextMode = ccmReferencesOnly);
  if UseIndexedOccurrences then
    UseIndexedOccurrences := FProject.PrepareConcordanceExport(True);
  FormPleaseWait.BeginOperation(Self, SExportConcordanceTitle,
    SExportConcordanceProgress, 1, FileName);
  try
    Result := FProject.ExportConcordance(FileName,
      FormPleaseWait.ProgressBar1, Options, Abbreviation,
      ContextMode, ContextMaxCharacters, UseIndexedOccurrences,
      @ConcordanceExportProgress);
  finally
    FormPleaseWait.FinishOperation;
  end;
end;

function TProjectExportForm.BuildExportResultMessage(
  const ExportResult: TExportResult; const PreferenceWarning: string;
  out DialogTitle: string; out DialogType: TMsgDlgType): string;
begin
  if (ExportResult.Status = esSuccess) and (PreferenceWarning = '') then
  begin
    DialogTitle := SExportCompletedTitle;
    DialogType := mtConfirmation;
    Exit(Format(SExportCompletedPrompt, [ExportResult.OutputFileName]));
  end;

  if ExportResult.Status = esFailed then
  begin
    DialogTitle := SExportFailedTitle;
    DialogType := mtError;
    Result := ExportResult.ErrorMessage;
  end
  else
  begin
    DialogTitle := SExportCompletedTitle;
    DialogType := mtConfirmation;
    Result := SExportCompletedMessage + LineEnding + LineEnding +
      ExportResult.OutputFileName;
    if ExportResult.Status = esWarnings then
    begin
      DialogTitle := SExportWarningsTitle;
      DialogType := mtWarning;
      Result := Format(SExportWarningsMessage, [ExportResult.WarningCount]) +
        LineEnding + LineEnding + ExportResult.OutputFileName;
      if ExportResult.ErrorReportFileName <> '' then
        Result := Result + LineEnding + LineEnding +
          Format(SExportReportLocation, [ExportResult.ErrorReportFileName]);
      if ExportResult.ErrorMessage <> '' then
        Result := Result + LineEnding + LineEnding + ExportResult.ErrorMessage;
    end;
  end;

  if PreferenceWarning <> '' then
  begin
    Result := Result + LineEnding + LineEnding + PreferenceWarning;
    if ExportResult.Status <> esFailed then
    begin
      DialogTitle := SExportWarningsTitle;
      DialogType := mtWarning;
    end;
  end;
  if ExportResult.Status <> esFailed then
    Result := Result + LineEnding + LineEnding + SExportOpenLocation;
end;

procedure TProjectExportForm.ShowExportResult(const ExportResult: TExportResult;
  const PreferenceWarning: string);
var
  MessageText, DialogTitle: string;
  DialogType: TMsgDlgType;
begin
  MessageText := BuildExportResultMessage(ExportResult, PreferenceWarning,
    DialogTitle, DialogType);
  if ExportResult.Status = esFailed then
  begin
    MessageDlg(DialogTitle, MessageText, DialogType, [mbOK], 0);
    Exit;
  end;
  if MessageDlg(DialogTitle, MessageText, DialogType, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  {$IFDEF WINDOWS}
  SysUtils.ExecuteProcess(UTF8ToSys('explorer.exe'),
    '/select,"' + ExportResult.OutputFileName + '"', []);
  {$ELSE}
  OpenDocument(ExtractFilePath(ExportResult.OutputFileName));
  {$ENDIF}
end;

procedure TProjectExportForm.SetProject(Project: TProject);
var
  ContextMaxCharacters: Integer;
  ContextMode: TConcordanceContextMode;
  ContextModeValue: string;
  LegacyAbbreviation: string;
begin
  FProject := Project;
  Caption := SExportTaggedDestinationTitle;
  if Assigned(BtnCancel) then
    BtnCancel.Caption := SUICancel;
  rgConcordanceType.Caption := SConcordanceTypeCaption;
  rgConcordanceType.Items.BeginUpdate;
  try
    rgConcordanceType.Items.Clear;
    rgConcordanceType.Items.Add(SConcordanceAnalyticalType);
    rgConcordanceType.Items.Add(SConcordanceSyntheticType);
  finally
    rgConcordanceType.Items.EndUpdate;
  end;
  rgConcordanceContext.Caption := SConcordanceReferenceTextCaption;
  rgConcordanceContext.Items.BeginUpdate;
  try
    rgConcordanceContext.Items.Clear;
    rgConcordanceContext.Items.Add(SConcordanceReferencesOnly);
    rgConcordanceContext.Items.Add(SConcordanceRelevantExcerpt);
    rgConcordanceContext.Items.Add(SConcordanceFullVerse);
  finally
    rgConcordanceContext.Items.EndUpdate;
  end;
  lblConcordanceMaxCharacters.Caption :=
    SConcordanceMaxExcerptCharacters;
  lblConcordanceOutput.Caption := SConcordanceOutputDescription;
  MemoSourceFooter.Text := FProject.GetInfo('propriedades.origem');
  MemoDestinationFooter.Text := FProject.GetInfo('propriedades.destino');
  LegacyAbbreviation := FProject.GetInfo('propriedades.abbrev');
  FAnalyticalAbbreviation := FProject.GetInfo(
    'opcoes.exportar.concordancia.analitica.abbrev');
  FSyntheticAbbreviation := FProject.GetInfo(
    'opcoes.exportar.concordancia.sintetica.abbrev');
  if FAnalyticalAbbreviation = '' then
    FAnalyticalAbbreviation := LegacyAbbreviation;
  if FSyntheticAbbreviation = '' then
    FSyntheticAbbreviation := LegacyAbbreviation;
  if FProject.GetInfo('opcoes.exportar.concordancia.detalhada') = '0' then
    rgConcordanceType.ItemIndex := 1
  else
    rgConcordanceType.ItemIndex := 0;
  FLastConcordanceType := rgConcordanceType.ItemIndex;
  if FLastConcordanceType = 1 then
    EditAbbreviation.Text := FSyntheticAbbreviation
  else
    EditAbbreviation.Text := FAnalyticalAbbreviation;
  UpdateConcordanceAbbreviationDefault;
  UpdateConcordanceDescription;
  ContextModeValue := Trim(FProject.GetInfo(SConcordanceContextModeKey));
  if SameText(ContextModeValue,
      SConcordanceContextRelevantExcerptValue) then
    ContextMode := ccmRelevantExcerpt
  else if SameText(ContextModeValue,
      SConcordanceContextFullVerseValue) then
    ContextMode := ccmFullVerse
  else if SameText(ContextModeValue,
      SConcordanceContextReferencesOnlyValue) then
    ContextMode := ccmReferencesOnly
  else if FProject.GetInfo('opcoes.exportar.concordancia.contexto') = '1' then
    ContextMode := ccmFullVerse
  else
    ContextMode := ccmReferencesOnly;
  SetSelectedConcordanceContextMode(ContextMode);
  ContextMaxCharacters := StrToIntDef(
    FProject.GetInfo(SConcordanceContextMaxCharactersKey),
    CONCORDANCE_CONTEXT_DEFAULT_CHARACTERS);
  if ContextMaxCharacters < CONCORDANCE_CONTEXT_MIN_CHARACTERS then
    ContextMaxCharacters := CONCORDANCE_CONTEXT_MIN_CHARACTERS
  else if ContextMaxCharacters > CONCORDANCE_CONTEXT_MAX_CHARACTERS then
    ContextMaxCharacters := CONCORDANCE_CONTEXT_MAX_CHARACTERS;
  seConcordanceMaxCharacters.Value := ContextMaxCharacters;
  CbExportComments.Checked := FProject.GetInfo('opcoes.exportar.comentarios') = '1';
  CbExportMorphology.Checked := FProject.GetInfo('opcoes.exportar.morfologia') = '1';
  CbExportUnassociatedAsItalics.Checked := FProject.GetInfo('opcoes.exportar.na.como.italicos') = '1';
  CbMarkReusedStrongs.Checked := FProject.GetInfo('opcoes.exportar.strongs.reutilizados') = '1';
  CbMarkUntranslatedStrongs.Checked := FProject.GetInfo('opcoes.exportar.strongs.nao.traduzidos') = '1';
end;

procedure TProjectExportForm.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
end;


initialization
{$IFNDEF FPCUNIT}
  {$R FormProjectExport.lfm}
{$ENDIF}

end.
