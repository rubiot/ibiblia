unit formexportar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Projeto, LCLTranslator, LCLIntf, LazUTF8;

type

  { TFrmExportarProjeto }

  TFrmExportarProjeto = class(TForm)
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
    procedure btExportarClick(Sender: TObject);
  private
    { private declarations }
    FProjeto: TProjeto;
    procedure ExportarDestinoEnriquecido;
    procedure ExportarOrigemInterlinear;
    procedure ExportarConcordancia;
    procedure ShowExportedFile(fileName: string);
  public
    { public declarations }
    procedure SetProjeto(projeto: TProjeto);
  end; 

var
  frmExportarProjeto: TFrmExportarProjeto;

resourcestring
  STheWordOTModule = 'theWord Bible module (*.ot)';
  STheWordNTModule = 'theWord Bible module (*.nt)';
  STheWordONTModule = 'theWord Bible module (*.ont)';
  SMySwordModule   = 'MySword Bible module (*.bbl.mybible)';
  SAnalyticalConcordance = 'AnalyticalConcordance';
  SSyntheticConcordance = 'SyntheticConcordance';

implementation

function bool2string(b: boolean): string;
begin
  if b then
    result := '1'
  else
    result := '0';
end;

{ TFrmExportarProjeto }

procedure TFrmExportarProjeto.btExportarClick(Sender: TObject);
begin
  case PageControl1.PageIndex of
    0: ExportarDestinoEnriquecido;
    1: ExportarOrigemInterlinear;
    2: ExportarConcordancia;
  end;
end;

procedure TFrmExportarProjeto.ExportarDestinoEnriquecido;
var
  opcoes: TOpcoesExportacao;
begin
  opcoes := [];
  if cbExportarNAComoItalicos.Checked then
    opcoes := opcoes + [oeExportarNAComoItalicos];
  if cbExportarComentarios.Checked then
    opcoes := opcoes + [oeExportarComentarios];
  if cbExportarMorfologia.Checked then
    opcoes := opcoes + [oeExportarMorfologia];
  if cbStrongsReutilizados.Checked then
    opcoes := opcoes + [oeStrongsReutilizados];
  if cbStrongsNaoTraduzidos.Checked then
    opcoes := opcoes + [oeStrongsNaoTraduzidos];

  FProjeto.AtribuirInfo('propriedades.destino', MemoRodapeDestino.Text);
  FProjeto.AtribuirInfo('opcoes.exportar.comentarios', bool2string(cbExportarComentarios.Checked));
  FProjeto.AtribuirInfo('opcoes.exportar.morfologia', bool2string(cbExportarMorfologia.Checked));
  FProjeto.AtribuirInfo('opcoes.exportar.na.como.italicos', bool2string(cbExportarNAComoItalicos.Checked));
  FProjeto.AtribuirInfo('opcoes.exportar.strongs.reutilizados', bool2string(cbStrongsReutilizados.Checked));
  FProjeto.AtribuirInfo('opcoes.exportar.strongs.nao.traduzidos', bool2string(cbStrongsNaoTraduzidos.Checked));

  case FProjeto.Escopo of
    etOT:
    begin
      SaveDialog1.DefaultExt:='.ot';
      SaveDialog1.Filter    := STheWordOTModule + '|*.ot' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
    etNT:
    begin
      SaveDialog1.DefaultExt:='.nt';
      SaveDialog1.Filter    := STheWordNTModule + '|*.nt' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
    etONT:
    begin
      SaveDialog1.DefaultExt:='.ont';
      SaveDialog1.Filter    := STheWordONTModule + '|*.ont' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
  end;

  if SaveDialog1.Execute then
  begin
    try
       self.Enabled := false;
       FProjeto.ExportarTextoDestinoComStrongs(SaveDialog1.FileName, ProgressBar1, opcoes);
       ShowExportedFile(SaveDialog1.FileName);
    finally
      self.Enabled := true;
    end;
  end;
end;

procedure TFrmExportarProjeto.ExportarOrigemInterlinear;
var
  opcoes: TOpcoesExportacao;
begin
  opcoes := [];
  FProjeto.AtribuirInfo('propriedades.origem', MemoRodapeOrigem.Text);

  case FProjeto.Escopo of
    etOT:
    begin
      SaveDialog1.DefaultExt:='.ot';
      SaveDialog1.Filter    := STheWordOTModule + '|*.ot' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
    etNT:
    begin
      SaveDialog1.DefaultExt:='.nt';
      SaveDialog1.Filter    := STheWordNTModule + '|*.nt' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
    etONT:
    begin
      SaveDialog1.DefaultExt:='.ont';
      SaveDialog1.Filter    := STheWordONTModule + '|*.ont' + '|' + SMySwordModule + '|*.bbl.mybible';
    end;
  end;

  if SaveDialog1.Execute then
  begin
    try
       self.Enabled:=false;
       FProjeto.ExportarTextoInterlinear(SaveDialog1.FileName, ProgressBar1, opcoes);
       ShowExportedFile(SaveDialog1.FileName);
    finally
      self.Enabled:=true;
    end;
  end;
end;

procedure TFrmExportarProjeto.ExportarConcordancia;
var
  opcoes: TOpcoesExportacao;
  abrev: string;
begin
  opcoes := [];
  if cbConcordanciaDetalhada.Checked then
    opcoes := opcoes + [oeConcordDetalhada];
  abrev := leAbreviacao.Text;
  if abrev = '' then
  begin
    if oeConcordDetalhada in opcoes then
      abrev := SAnalyticalConcordance
    else
      abrev := SSyntheticConcordance;
  end;

  {if oeConcordDetalhada in opcoes then
     FProjeto.AtribuirInfo('opcoes.exportar.abrevconc.analitica', abrev)
  else
     FProjeto.AtribuirInfo('opcoes.exportar.abrevconc.sintetica', abrev);}

  if SaveDialogConcordancia.Execute then
  begin
    try
       self.Enabled:=false;
       FProjeto.ExportarConcordancia(SaveDialogConcordancia.FileName, ProgressBar1, opcoes, leAbreviacao.Text);
       ShowExportedFile(SaveDialogConcordancia.FileName);
    finally
      self.Enabled:=true;
    end;
  end;

  FProjeto.AtribuirInfo('propriedades.abbrev', leAbreviacao.Text);
end;

procedure TFrmExportarProjeto.ShowExportedFile(fileName: string);
begin
  {$IFDEF WINDOWS}
  SysUtils.ExecuteProcess(UTF8ToSys('explorer.exe'), '/select,' + fileName, []);
  {$ELSE}
  OpenDocument(ExtractFilePath(fileName));
  {$ENDIF}
end;

procedure TFrmExportarProjeto.SetProjeto(projeto: TProjeto);
begin
  FProjeto := projeto;
  MemoRodapeOrigem.Text := FProjeto.ObterInfo('propriedades.origem');
  MemoRodapeDestino.Text := FProjeto.ObterInfo('propriedades.destino');
  leAbreviacao.Text := FProjeto.ObterInfo('propriedades.abbrev');
  cbExportarComentarios.Checked := FProjeto.ObterInfo('opcoes.exportar.comentarios') = '1';
  cbExportarMorfologia.Checked := FProjeto.ObterInfo('opcoes.exportar.morfologia') = '1';
  cbExportarNAComoItalicos.Checked := FProjeto.ObterInfo('opcoes.exportar.na.como.italicos') = '1';
  cbStrongsReutilizados.Checked := FProjeto.ObterInfo('opcoes.exportar.strongs.reutilizados') = '1';
  cbStrongsNaoTraduzidos.Checked := FProjeto.ObterInfo('opcoes.exportar.strongs.nao.traduzidos') = '1';
end;

initialization
  {$I formexportar.lrs}

end.

