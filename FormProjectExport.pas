unit FormProjectExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Spin, BibleScope, Project, LCLTranslator,
  LCLIntf, LazUTF8, UnitPleaseWait, project_exporter, ConcordanceExport;

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
    function ConcordanceLastFileKey: string;
    function SelectedConcordanceContextMode: TConcordanceContextMode;
    function SuggestedBaseFileName(const Suffix: string): string;
    function StoredExportFileName(const Key, DefaultFileName: string): string;
    procedure ConfigureBibleSaveDialog(const Suffix, LastFileKey: string);
    procedure ConfigureConcordanceSaveDialog;
    procedure ExportTaggedDestinationText;
    procedure ExportInterlinearSourceText;
    procedure ExportConcordance;
    procedure ConfirmOpenExportedFile(const FileName: string);
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
  public
    procedure SetProject(Project: TProject);
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

implementation

uses formPrincipal;

const
  SExportIniSection = 'export';
  SLastTaggedDestinationFileKey = 'last.tagged.destination.file';
  SLastInterlinearSourceFileKey = 'last.interlinear.source.file';
  SLastAnalyticalConcordanceFileKey = 'last.strongs.analytical.concordance.file';
  SLastSyntheticConcordanceFileKey = 'last.strongs.synthetic.concordance.file';
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

function TProjectExportForm.ConcordanceLastFileKey: string;
begin
  if rgConcordanceType.ItemIndex = 1 then
    Result := SLastSyntheticConcordanceFileKey
  else
    Result := SLastAnalyticalConcordanceFileKey;
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

function TProjectExportForm.StoredExportFileName(const Key,
  DefaultFileName: string): string;
var
  StoredFileName: string;
  StoredPath: string;
begin
  Result := DefaultFileName;
  if not Assigned(opts) then
    Exit;

  StoredFileName := Trim(opts.ReadString(SExportIniSection, Key, ''));
  if StoredFileName = '' then
    Exit;

  StoredPath := ExtractFilePath(StoredFileName);
  if (StoredPath = '') or DirectoryExists(StoredPath) then
    Result := StoredFileName;
end;

procedure SaveStoredExportFileName(const Key, FileName: string);
begin
  if Assigned(opts) then
    opts.WriteString(SExportIniSection, Key, FileName);
end;

procedure TProjectExportForm.ConfigureBibleSaveDialog(const Suffix,
  LastFileKey: string);
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
  SaveBibleModuleDialog.FileName := StoredExportFileName(LastFileKey, SuggestedFileName);
  if LowerCase(SaveBibleModuleDialog.FileName).EndsWith('.mybible') then
    SaveBibleModuleDialog.FilterIndex := 2;
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
  SaveConcordanceDialog.FileName := StoredExportFileName(
    ConcordanceLastFileKey, SuggestedFileName);
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
begin
  Options := BuildTaggedDestinationOptions;
  ConfigureBibleSaveDialog('tagged-destination', SLastTaggedDestinationFileKey);

  if SaveBibleModuleDialog.Execute then
  begin
    SaveStoredExportFileName(SLastTaggedDestinationFileKey, SaveBibleModuleDialog.FileName);
    PersistTaggedDestinationOptions;
    FormPleaseWait.BeginOperation(Self, SExportTaggedDestinationTitle,
      SExportTaggedDestinationProgress, 1, SaveBibleModuleDialog.FileName);
    try
      FProject.ExportDestinationTextWithStrongs(SaveBibleModuleDialog.FileName,
        FormPleaseWait.ProgressBar1, Options);
    finally
      FormPleaseWait.FinishOperation;
    end;
    ConfirmOpenExportedFile(SaveBibleModuleDialog.FileName);
  end;
end;

procedure TProjectExportForm.ExportInterlinearSourceText;
var
  Options: TOpcoesExportacao;
begin
  Options := [];
  ConfigureBibleSaveDialog('interlinear-source', SLastInterlinearSourceFileKey);

  if SaveBibleModuleDialog.Execute then
  begin
    SaveStoredExportFileName(SLastInterlinearSourceFileKey, SaveBibleModuleDialog.FileName);
    PersistInterlinearOptions;
    FormPleaseWait.BeginOperation(Self, SExportInterlinearTitle,
      SExportInterlinearProgress, 1, SaveBibleModuleDialog.FileName);
    try
      FProject.ExportInterlinearText(SaveBibleModuleDialog.FileName,
        FormPleaseWait.ProgressBar1, Options);
    finally
      FormPleaseWait.FinishOperation;
    end;
    ConfirmOpenExportedFile(SaveBibleModuleDialog.FileName);
  end;
end;

procedure TProjectExportForm.ExportConcordance;
var
  Options: TOpcoesExportacao;
  Abbreviation: string;
  ContextMode: TConcordanceContextMode;
  UseIndexedOccurrences: Boolean;
begin
  Options := BuildConcordanceOptions;
  ContextMode := SelectedConcordanceContextMode;
  UpdateConcordanceAbbreviationDefault;

  Abbreviation := Trim(EditAbbreviation.Text);
  if Abbreviation = '' then
    Abbreviation := DefaultConcordanceAbbreviation;

  ConfigureConcordanceSaveDialog;
  if SaveConcordanceDialog.Execute then
  begin
    EditAbbreviation.Text := Abbreviation;
    UseIndexedOccurrences := (rgConcordanceType.ItemIndex = 1) and
      (ContextMode = ccmReferencesOnly);
    if UseIndexedOccurrences then
      UseIndexedOccurrences := FProject.PrepareConcordanceExport(True);
    FormPleaseWait.BeginOperation(Self, SExportConcordanceTitle,
      SExportConcordanceProgress, 1, SaveConcordanceDialog.FileName);
    try
      FProject.ExportConcordance(SaveConcordanceDialog.FileName,
        FormPleaseWait.ProgressBar1, Options, Abbreviation,
        ContextMode, seConcordanceMaxCharacters.Value,
        UseIndexedOccurrences,
        @ConcordanceExportProgress);
    finally
      FormPleaseWait.FinishOperation;
    end;
    SaveStoredExportFileName(ConcordanceLastFileKey,
      SaveConcordanceDialog.FileName);
    PersistConcordanceOptions(Abbreviation);
    ConfirmOpenExportedFile(SaveConcordanceDialog.FileName);
  end;
end;

procedure TProjectExportForm.ConfirmOpenExportedFile(const FileName: string);
begin
  if not FileExists(FileName) then
    Exit;

  if MessageDlg(SExportCompletedTitle, Format(SExportCompletedPrompt, [FileName]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  {$IFDEF WINDOWS}
  SysUtils.ExecuteProcess(UTF8ToSys('explorer.exe'), '/select,' + FileName, []);
  {$ELSE}
  OpenDocument(ExtractFilePath(FileName));
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

initialization
{$IFNDEF FPCUNIT}
  {$I FormProjectExport.lrs}
{$ENDIF}

end.
