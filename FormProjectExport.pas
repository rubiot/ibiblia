unit FormProjectExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, BibleScope, Projeto, LCLTranslator, LCLIntf, LazUTF8;

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
    FProject: TProjeto;
    procedure ExportTaggedDestinationText;
    procedure ExportInterlinearSourceText;
    procedure ExportConcordance;
    procedure ShowExportedFile(const FileName: string);
  public
    procedure SetProject(Project: TProjeto);
  end;

var
  FrmProjectExport: TFrmProjectExport;

resourcestring
  STheWordOTModule = 'theWord Bible module (*.ot)';
  STheWordNTModule = 'theWord Bible module (*.nt)';
  STheWordONTModule = 'theWord Bible module (*.ont)';
  SMySwordModule   = 'MySword Bible module (*.bbl.mybible)';
  SAnalyticalConcordance = 'AnalyticalConcordance';
  SSyntheticConcordance = 'SyntheticConcordance';

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

  if SaveDialog1.Execute then
  begin
    try
      Self.Enabled := False;
      FProject.ExportDestinationTextWithStrongs(SaveDialog1.FileName, ProgressBar1, Options);
      ShowExportedFile(SaveDialog1.FileName);
    finally
      Self.Enabled := True;
    end;
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

  if SaveDialog1.Execute then
  begin
    try
      Self.Enabled := False;
      FProject.ExportInterlinearText(SaveDialog1.FileName, ProgressBar1, Options);
      ShowExportedFile(SaveDialog1.FileName);
    finally
      Self.Enabled := True;
    end;
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

  if SaveDialogConcordancia.Execute then
  begin
    try
      Self.Enabled := False;
      FProject.ExportConcordance(SaveDialogConcordancia.FileName, ProgressBar1, Options, leAbreviacao.Text);
      ShowExportedFile(SaveDialogConcordancia.FileName);
    finally
      Self.Enabled := True;
    end;
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

procedure TFrmProjectExport.SetProject(Project: TProjeto);
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