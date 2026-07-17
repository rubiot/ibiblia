unit FormProjectExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, BibleScope, Project, LCLTranslator, LCLIntf,
  LazUTF8, UnitPleaseWait, project_exporter;

type

  TFrmProjectExport = class(TForm)
    btExportar: TButton;
    Button2: TButton;
    cbExportarNAComoItalicos: TCheckBox;
    cbExportarMorfologia: TCheckBox;
    cbExportarComentarios: TCheckBox;
    cbConcordanceVerseContext: TCheckBox;
    cbStrongsReutilizados: TCheckBox;
    cbStrongsNaoTraduzidos: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    lblConcordanceDescription: TLabel;
    lblConcordanceOutput: TLabel;
    leAbreviacao: TLabeledEdit;
    MemoRodapeOrigem: TMemo;
    MemoRodapeDestino: TMemo;
    PageControl1: TPageControl;
    rgConcordanceType: TRadioGroup;
    SaveDialog1: TSaveDialog;
    SaveDialogConcordancia: TSaveDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure BtnExportClick(Sender: TObject);
    procedure ConcordanceTypeChange(Sender: TObject);
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
    procedure UpdateConcordanceAbbreviationDefault;
    procedure UpdateConcordanceDescription;
    procedure ConcordanceExportProgress(Phase: TConcordanceExportPhase;
      Current, Total: Integer);
  public
    procedure SetProject(Project: TProject);
  end;

var
  FrmProjectExport: TFrmProjectExport;

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
  SConcordanceIncludeVerseContext =
    'Include destination verse text beside each reference';
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

function BoolToConfigValue(Value: Boolean): string;
begin
  if Value then
    Result := '1'
  else
    Result := '0';
end;

procedure TFrmProjectExport.BtnExportClick(Sender: TObject);
begin
  ModalResult := mrNone;
  case PageControl1.PageIndex of
    0: ExportTaggedDestinationText;
    1: ExportInterlinearSourceText;
    2: ExportConcordance;
  end;
end;

procedure TFrmProjectExport.ConcordanceTypeChange(Sender: TObject);
begin
  if FLastConcordanceType = 0 then
    FAnalyticalAbbreviation := Trim(leAbreviacao.Text)
  else if FLastConcordanceType = 1 then
    FSyntheticAbbreviation := Trim(leAbreviacao.Text);

  if rgConcordanceType.ItemIndex = 1 then
    leAbreviacao.Text := FSyntheticAbbreviation
  else
    leAbreviacao.Text := FAnalyticalAbbreviation;
  FLastConcordanceType := rgConcordanceType.ItemIndex;
  UpdateConcordanceAbbreviationDefault;
  UpdateConcordanceDescription;
end;

procedure TFrmProjectExport.UpdateConcordanceDescription;
begin
  if rgConcordanceType.ItemIndex = 1 then
    lblConcordanceDescription.Caption := SConcordanceSyntheticDescription
  else
    lblConcordanceDescription.Caption := SConcordanceAnalyticalDescription;
end;

procedure TFrmProjectExport.ConcordanceExportProgress(
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

function TFrmProjectExport.BuildTaggedDestinationOptions: TOpcoesExportacao;
begin
  Result := [];
  if cbExportarNAComoItalicos.Checked then
    Include(Result, oeExportarNAComoItalicos);
  if cbExportarComentarios.Checked then
    Include(Result, oeExportarComentarios);
  if cbExportarMorfologia.Checked then
    Include(Result, oeExportarMorfologia);
  if cbStrongsReutilizados.Checked then
    Include(Result, oeStrongsReutilizados);
  if cbStrongsNaoTraduzidos.Checked then
    Include(Result, oeStrongsNaoTraduzidos);
end;

function TFrmProjectExport.BuildConcordanceOptions: TOpcoesExportacao;
begin
  Result := [];
  if rgConcordanceType.ItemIndex < 0 then
    rgConcordanceType.ItemIndex := 0;
  if rgConcordanceType.ItemIndex = 0 then
    Include(Result, oeConcordDetalhada);
end;

function TFrmProjectExport.DefaultConcordanceAbbreviation: string;
begin
  if rgConcordanceType.ItemIndex = 1 then
    Result := SSyntheticConcordance
  else
    Result := SAnalyticalConcordance;
end;

function TFrmProjectExport.ConcordanceFileSuffix: string;
begin
  if rgConcordanceType.ItemIndex = 1 then
    Result := 'synthetic-concordance'
  else
    Result := 'analytical-concordance';
end;

function TFrmProjectExport.ConcordanceLastFileKey: string;
begin
  if rgConcordanceType.ItemIndex = 1 then
    Result := SLastSyntheticConcordanceFileKey
  else
    Result := SLastAnalyticalConcordanceFileKey;
end;

function TFrmProjectExport.SuggestedBaseFileName(const Suffix: string): string;
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

function TFrmProjectExport.StoredExportFileName(const Key,
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

procedure TFrmProjectExport.ConfigureBibleSaveDialog(const Suffix,
  LastFileKey: string);
var
  SuggestedFileName: string;
begin
  case FProject.Escopo of
    etOT:
    begin
      SaveDialog1.DefaultExt := '.ot';
      SaveDialog1.Filter := STheWordOTModule + '|*.ot' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
    etNT:
    begin
      SaveDialog1.DefaultExt := '.nt';
      SaveDialog1.Filter := STheWordNTModule + '|*.nt' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
    etONT:
    begin
      SaveDialog1.DefaultExt := '.ont';
      SaveDialog1.Filter := STheWordONTModule + '|*.ont' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
  end;
  SaveDialog1.Title := SSaveModuleAs;
  SaveDialog1.FilterIndex := 1;
  SuggestedFileName := SuggestedBaseFileName(Suffix) + SaveDialog1.DefaultExt;
  SaveDialog1.FileName := StoredExportFileName(LastFileKey, SuggestedFileName);
  if LowerCase(SaveDialog1.FileName).EndsWith('.mybible') then
    SaveDialog1.FilterIndex := 2;
  SaveDialog1.Options := SaveDialog1.Options + [ofOverwritePrompt, ofPathMustExist];
end;

procedure TFrmProjectExport.ConfigureConcordanceSaveDialog;
var
  SuggestedFileName: string;
begin
  SaveDialogConcordancia.Title := SSaveModuleAs;
  SaveDialogConcordancia.DefaultExt := '.dct.twm';
  SaveDialogConcordancia.Filter := STheWordDictionaryModule + '|*.dct.twm';
  SaveDialogConcordancia.FilterIndex := 1;
  SuggestedFileName := SuggestedBaseFileName(ConcordanceFileSuffix) + '.dct.twm';
  SaveDialogConcordancia.FileName := StoredExportFileName(
    ConcordanceLastFileKey, SuggestedFileName);
  SaveDialogConcordancia.Options := SaveDialogConcordancia.Options + [ofOverwritePrompt, ofPathMustExist];
end;

procedure TFrmProjectExport.PersistTaggedDestinationOptions;
begin
  FProject.SetInfo('propriedades.destino', MemoRodapeDestino.Text);
  FProject.SetInfo('opcoes.exportar.comentarios', BoolToConfigValue(cbExportarComentarios.Checked));
  FProject.SetInfo('opcoes.exportar.morfologia', BoolToConfigValue(cbExportarMorfologia.Checked));
  FProject.SetInfo('opcoes.exportar.na.como.italicos', BoolToConfigValue(cbExportarNAComoItalicos.Checked));
  FProject.SetInfo('opcoes.exportar.strongs.reutilizados', BoolToConfigValue(cbStrongsReutilizados.Checked));
  FProject.SetInfo('opcoes.exportar.strongs.nao.traduzidos', BoolToConfigValue(cbStrongsNaoTraduzidos.Checked));
end;

procedure TFrmProjectExport.PersistInterlinearOptions;
begin
  FProject.SetInfo('propriedades.origem', MemoRodapeOrigem.Text);
end;

procedure TFrmProjectExport.PersistConcordanceOptions(const Abbreviation: string);
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
  FProject.SetInfo('opcoes.exportar.concordancia.contexto',
    BoolToConfigValue(cbConcordanceVerseContext.Checked));
end;

procedure TFrmProjectExport.UpdateConcordanceAbbreviationDefault;
var
  CurrentText: string;
begin
  CurrentText := Trim(leAbreviacao.Text);
  if (CurrentText = '') or SameText(CurrentText, SAnalyticalConcordance) or
    SameText(CurrentText, SSyntheticConcordance) then
    leAbreviacao.Text := DefaultConcordanceAbbreviation;
end;

procedure TFrmProjectExport.ExportTaggedDestinationText;
var
  Options: TOpcoesExportacao;
begin
  Options := BuildTaggedDestinationOptions;
  ConfigureBibleSaveDialog('tagged-destination', SLastTaggedDestinationFileKey);

  if SaveDialog1.Execute then
  begin
    SaveStoredExportFileName(SLastTaggedDestinationFileKey, SaveDialog1.FileName);
    PersistTaggedDestinationOptions;
    FormPleaseWait.BeginOperation(Self, SExportTaggedDestinationTitle,
      SExportTaggedDestinationProgress, 1, SaveDialog1.FileName);
    try
      FProject.ExportDestinationTextWithStrongs(SaveDialog1.FileName,
        FormPleaseWait.ProgressBar1, Options);
    finally
      FormPleaseWait.FinishOperation;
    end;
    ConfirmOpenExportedFile(SaveDialog1.FileName);
  end;
end;

procedure TFrmProjectExport.ExportInterlinearSourceText;
var
  Options: TOpcoesExportacao;
begin
  Options := [];
  ConfigureBibleSaveDialog('interlinear-source', SLastInterlinearSourceFileKey);

  if SaveDialog1.Execute then
  begin
    SaveStoredExportFileName(SLastInterlinearSourceFileKey, SaveDialog1.FileName);
    PersistInterlinearOptions;
    FormPleaseWait.BeginOperation(Self, SExportInterlinearTitle,
      SExportInterlinearProgress, 1, SaveDialog1.FileName);
    try
      FProject.ExportInterlinearText(SaveDialog1.FileName,
        FormPleaseWait.ProgressBar1, Options);
    finally
      FormPleaseWait.FinishOperation;
    end;
    ConfirmOpenExportedFile(SaveDialog1.FileName);
  end;
end;

procedure TFrmProjectExport.ExportConcordance;
var
  Options: TOpcoesExportacao;
  Abbreviation: string;
  UseIndexedOccurrences: Boolean;
begin
  Options := BuildConcordanceOptions;
  UpdateConcordanceAbbreviationDefault;

  Abbreviation := Trim(leAbreviacao.Text);
  if Abbreviation = '' then
    Abbreviation := DefaultConcordanceAbbreviation;

  ConfigureConcordanceSaveDialog;
  if SaveDialogConcordancia.Execute then
  begin
    leAbreviacao.Text := Abbreviation;
    UseIndexedOccurrences := rgConcordanceType.ItemIndex = 1;
    if UseIndexedOccurrences then
      UseIndexedOccurrences := FProject.PrepareConcordanceExport(True);
    FormPleaseWait.BeginOperation(Self, SExportConcordanceTitle,
      SExportConcordanceProgress, 1, SaveDialogConcordancia.FileName);
    try
      FProject.ExportConcordance(SaveDialogConcordancia.FileName,
        FormPleaseWait.ProgressBar1, Options, Abbreviation,
        cbConcordanceVerseContext.Checked, UseIndexedOccurrences,
        @ConcordanceExportProgress);
    finally
      FormPleaseWait.FinishOperation;
    end;
    SaveStoredExportFileName(ConcordanceLastFileKey,
      SaveDialogConcordancia.FileName);
    PersistConcordanceOptions(Abbreviation);
    ConfirmOpenExportedFile(SaveDialogConcordancia.FileName);
  end;
end;

procedure TFrmProjectExport.ConfirmOpenExportedFile(const FileName: string);
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

procedure TFrmProjectExport.SetProject(Project: TProject);
var
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
  lblConcordanceOutput.Caption := SConcordanceOutputDescription;
  cbConcordanceVerseContext.Caption := SConcordanceIncludeVerseContext;
  MemoRodapeOrigem.Text := FProject.GetInfo('propriedades.origem');
  MemoRodapeDestino.Text := FProject.GetInfo('propriedades.destino');
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
    leAbreviacao.Text := FSyntheticAbbreviation
  else
    leAbreviacao.Text := FAnalyticalAbbreviation;
  UpdateConcordanceAbbreviationDefault;
  UpdateConcordanceDescription;
  cbConcordanceVerseContext.Checked :=
    FProject.GetInfo('opcoes.exportar.concordancia.contexto') = '1';
  cbExportarComentarios.Checked := FProject.GetInfo('opcoes.exportar.comentarios') = '1';
  cbExportarMorfologia.Checked := FProject.GetInfo('opcoes.exportar.morfologia') = '1';
  cbExportarNAComoItalicos.Checked := FProject.GetInfo('opcoes.exportar.na.como.italicos') = '1';
  cbStrongsReutilizados.Checked := FProject.GetInfo('opcoes.exportar.strongs.reutilizados') = '1';
  cbStrongsNaoTraduzidos.Checked := FProject.GetInfo('opcoes.exportar.strongs.nao.traduzidos') = '1';
end;

initialization
{$IFNDEF FPCUNIT}
  {$I FormProjectExport.lrs}
{$ENDIF}

end.
