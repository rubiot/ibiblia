unit FormProjectExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, BibleScope, Project, LCLTranslator, LCLIntf,
  LazUTF8, UnitPleaseWait;

type

  TFrmProjectExport = class(TForm)
    btExportar: TButton;
    Button2: TButton;
    cbExportarNAComoItalicos: TCheckBox;
    cbExportarMorfologia: TCheckBox;
    cbExportarComentarios: TCheckBox;
    cbConcordanciaDetalhada: TCheckBox;
    cbStrongsReutilizados: TCheckBox;
    cbStrongsNaoTraduzidos: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    leAbreviacao: TLabeledEdit;
    MemoRodapeOrigem: TMemo;
    MemoRodapeDestino: TMemo;
    PageControl1: TPageControl;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    SaveDialogConcordancia: TSaveDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure BtnExportClick(Sender: TObject);
  private
    FProject: TProject;
    procedure ExportTaggedDestinationText;
    procedure ExportInterlinearSourceText;
    procedure ExportConcordance;
    procedure ShowExportedFile(const FileName: string);
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

implementation

function BoolToConfigValue(Value: Boolean): string;
begin
  if Value then
    Result := '1'
  else
    Result := '0';
end;

procedure TFrmProjectExport.BtnExportClick(Sender: TObject);
begin
  case PageControl1.PageIndex of
    0: ExportTaggedDestinationText;
    1: ExportInterlinearSourceText;
    2: ExportConcordance;
  end;
end;

procedure TFrmProjectExport.ExportTaggedDestinationText;
var
  Options: TOpcoesExportacao;
begin
  Options := [];
  if cbExportarNAComoItalicos.Checked then
    Options := Options + [oeExportarNAComoItalicos];
  if cbExportarComentarios.Checked then
    Options := Options + [oeExportarComentarios];
  if cbExportarMorfologia.Checked then
    Options := Options + [oeExportarMorfologia];
  if cbStrongsReutilizados.Checked then
    Options := Options + [oeStrongsReutilizados];
  if cbStrongsNaoTraduzidos.Checked then
    Options := Options + [oeStrongsNaoTraduzidos];

  FProject.SetInfo('propriedades.destino', MemoRodapeDestino.Text);
  FProject.SetInfo('opcoes.exportar.comentarios', BoolToConfigValue(cbExportarComentarios.Checked));
  FProject.SetInfo('opcoes.exportar.morfologia', BoolToConfigValue(cbExportarMorfologia.Checked));
  FProject.SetInfo('opcoes.exportar.na.como.italicos', BoolToConfigValue(cbExportarNAComoItalicos.Checked));
  FProject.SetInfo('opcoes.exportar.strongs.reutilizados', BoolToConfigValue(cbStrongsReutilizados.Checked));
  FProject.SetInfo('opcoes.exportar.strongs.nao.traduzidos', BoolToConfigValue(cbStrongsNaoTraduzidos.Checked));

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

  if SaveDialog1.Execute then
  begin
    FormPleaseWait.BeginOperation(Self, SExportTaggedDestinationTitle,
      SExportTaggedDestinationProgress, 1, SaveDialog1.FileName);
    try
      FProject.ExportDestinationTextWithStrongs(SaveDialog1.FileName,
        FormPleaseWait.ProgressBar1, Options);
    finally
      FormPleaseWait.FinishOperation;
    end;
    ShowExportedFile(SaveDialog1.FileName);
  end;
end;

procedure TFrmProjectExport.ExportInterlinearSourceText;
var
  Options: TOpcoesExportacao;
begin
  Options := [];
  FProject.SetInfo('propriedades.origem', MemoRodapeOrigem.Text);

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

  if SaveDialog1.Execute then
  begin
    FormPleaseWait.BeginOperation(Self, SExportInterlinearTitle,
      SExportInterlinearProgress, 1, SaveDialog1.FileName);
    try
      FProject.ExportInterlinearText(SaveDialog1.FileName,
        FormPleaseWait.ProgressBar1, Options);
    finally
      FormPleaseWait.FinishOperation;
    end;
    ShowExportedFile(SaveDialog1.FileName);
  end;
end;

procedure TFrmProjectExport.ExportConcordance;
var
  Options: TOpcoesExportacao;
  Abbreviation: string;
begin
  Options := [];
  if cbConcordanciaDetalhada.Checked then
    Options := Options + [oeConcordDetalhada];

  Abbreviation := leAbreviacao.Text;
  if Abbreviation = '' then
  begin
    if oeConcordDetalhada in Options then
      Abbreviation := SAnalyticalConcordance
    else
      Abbreviation := SSyntheticConcordance;
  end;

  {if oeConcordDetalhada in Options then
     FProject.SetInfo('opcoes.exportar.abrevconc.analitica', Abbreviation)
  else
     FProject.SetInfo('opcoes.exportar.abrevconc.sintetica', Abbreviation);}

  SaveDialogConcordancia.Title := SSaveModuleAs;
  SaveDialogConcordancia.Filter := STheWordDictionaryModule + '|*.dct.twm';
  if SaveDialogConcordancia.Execute then
  begin
    FormPleaseWait.BeginOperation(Self, SExportConcordanceTitle,
      SExportConcordanceProgress, 1, SaveDialogConcordancia.FileName);
    try
      FProject.ExportConcordance(SaveDialogConcordancia.FileName,
        FormPleaseWait.ProgressBar1, Options, leAbreviacao.Text);
    finally
      FormPleaseWait.FinishOperation;
    end;
    ShowExportedFile(SaveDialogConcordancia.FileName);
  end;

  FProject.SetInfo('propriedades.abbrev', leAbreviacao.Text);
end;

procedure TFrmProjectExport.ShowExportedFile(const FileName: string);
begin
  {$IFDEF WINDOWS}
  SysUtils.ExecuteProcess(UTF8ToSys('explorer.exe'), '/select,' + FileName, []);
  {$ELSE}
  OpenDocument(ExtractFilePath(FileName));
  {$ENDIF}
end;

procedure TFrmProjectExport.SetProject(Project: TProject);
begin
  FProject := Project;
  MemoRodapeOrigem.Text := FProject.GetInfo('propriedades.origem');
  MemoRodapeDestino.Text := FProject.GetInfo('propriedades.destino');
  leAbreviacao.Text := FProject.GetInfo('propriedades.abbrev');
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
