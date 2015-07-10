unit formpropprojeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ActnList, Projeto;

type

  { TFrmPropProjeto }

  TFormPropProjeto = class(TForm)
    ActionSelecionarMorfo: TAction;
    ActionMudancaAba: TAction;
    ActionSelecionarFonte: TAction;
    ActionSelecionarStrong: TAction;
    ActionLimparTexto: TAction;
    ActionCarregarTexto: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    Button2: TButton;
    EditNomeProjeto: TLabeledEdit;
    FontDialog1: TFontDialog;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    leDicStrong: TLabeledEdit;
    leDicMorfo: TLabeledEdit;
    MemoFonte: TMemo;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    TabControl1: TTabControl;
    FProjeto: TProjeto;
    procedure ActionCarregarTextoExecute(Sender: TObject);
    procedure ActionLimparTextoExecute(Sender: TObject);
    procedure ActionMudancaAbaExecute(Sender: TObject);
    procedure ActionSelecionarFonteExecute(Sender: TObject);
    procedure ActionSelecionarMorfoExecute(Sender: TObject);
    procedure ActionSelecionarStrongExecute(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure leDicMorfoChange(Sender: TObject);
    procedure leDicStrongChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetProjeto(p: TProjeto);
    procedure AplicarAlteracoes;
  end; 

var
  FormPropProjeto1: TFormPropProjeto;

implementation

{ TFormPropProjeto }

procedure TFormPropProjeto.FormCreate(Sender: TObject);
begin
  FProjeto := nil;
end;

procedure TFormPropProjeto.leDicMorfoChange(Sender: TObject);
begin
  BitBtn8.Caption := 'Atribuir';
end;

procedure TFormPropProjeto.leDicStrongChange(Sender: TObject);
begin
  BitBtn4.Caption := 'Atribuir';
end;

procedure TFormPropProjeto.ActionCarregarTextoExecute(Sender: TObject);
begin
  OpenDialog1.Filter := '*.nt; *.ont';
  OpenDialog1.Title  := 'Selecione um módulo do theWord...';
  if OpenDialog1.Execute then
  begin
    //leTexto.Text := OpenDialog1.FileName;
    if MessageDlg('Substituir texto', 'Tem certeza de que deseja substituir o texto atual?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
    begin
      FProjeto.ImportarModuloTheWord(OpenDialog1.FileName, TTipoTextoBiblico(TabControl1.TabIndex), ProgressBar1);
    end;
  end;
end;

procedure TFormPropProjeto.ActionLimparTextoExecute(Sender: TObject);
begin
  if MessageDlg('Limpar texto', 'Tem certeza de que deseja limpar o texto atual?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    FProjeto.LimparTexto(TTipoTextoBiblico(TabControl1.TabIndex));
  end;
end;

procedure TFormPropProjeto.ActionMudancaAbaExecute(Sender: TObject);
begin
  leDicStrong.Text := FProjeto.ObterInfo(format('dicstrong%d', [TabControl1.TabIndex]));
  leDicMorfo.Text  := FProjeto.ObterInfo(format('dicmorfo%d', [TabControl1.TabIndex]));
  BitBtn4.Caption  := 'Selecionar'; // seleção Strong
  BitBtn8.Caption  := 'Selecionar'; // seleção Morfo
  MemoFonte.Text   := FProjeto.ObterTextoSimplesVersiculo(TTipoTextoBiblico(TabControl1.TabIndex));
  MemoFonte.Font   := FProjeto.ObterFonteTexto(TTipoTextoBiblico(TabControl1.TabIndex));
end;

procedure TFormPropProjeto.ActionSelecionarFonteExecute(Sender: TObject);
begin
  FontDialog1.Font := MemoFonte.Font;
  if FontDialog1.Execute then
  begin
    MemoFonte.Font := FontDialog1.Font;
    FProjeto.AtribuirFonteTexto(MemoFonte.Font, [TTipoTextoBiblico(TabControl1.TabIndex)]);
  end;
end;

procedure TFormPropProjeto.ActionSelecionarMorfoExecute(Sender: TObject);
var
  d: string;
begin
  //OpenDialog1.Filter := '*.nt; *.ont';
  OpenDialog1.FileName := leDicMorfo.Text;
  OpenDialog1.Title  := 'Selecione um módulo de dicionário do theWord...';
  if (BitBtn8.Caption = 'Atribuir') or OpenDialog1.Execute then
  begin
    { utilizando caminho relativo sempre que possível }
    GetDir(0, d);
    if Pos(d, OpenDialog1.FileName) = 1 then
    begin
      leDicMorfo.Text := format('.%s', [RightStr(OpenDialog1.Filename, length(OpenDialog1.Filename) - length(d))]);
    end else
      leDicMorfo.Text := OpenDialog1.FileName;
    FProjeto.AtribuirDicMorfo(leDicMorfo.Text, [TTipoTextoBiblico(TabControl1.TabIndex)]);
  end;
  BitBtn8.Caption := 'Selecionar';
end;

procedure TFormPropProjeto.ActionSelecionarStrongExecute(Sender: TObject);
var
  d: string;
begin
  //OpenDialog1.Filter := '*.nt; *.ont';
  OpenDialog1.FileName := leDicStrong.Text;
  OpenDialog1.Title  := 'Selecione um módulo de dicionário do theWord...';
  if (BitBtn4.Caption = 'Atribuir') or OpenDialog1.Execute then
  begin
    { utilizando caminho relativo sempre que possível }
    GetDir(0, d);
    if Pos(d, OpenDialog1.FileName) = 1 then
    begin
      leDicStrong.Text := format('.%s', [RightStr(OpenDialog1.Filename, length(OpenDialog1.Filename) - length(d))]);
    end else
      leDicStrong.Text := OpenDialog1.FileName;
    FProjeto.AtribuirDicStrong(leDicStrong.Text, [TTipoTextoBiblico(TabControl1.TabIndex)]);
  end;
  BitBtn4.Caption := 'Selecionar';
end;

procedure TFormPropProjeto.BitBtn1Click(Sender: TObject);
begin

end;

procedure TFormPropProjeto.SetProjeto(p: TProjeto);
begin
  FProjeto := p;
  EditNomeProjeto.Caption := p.ObterInfo('descricao');
  ActionMudancaAba.Execute;
end;

procedure TFormPropProjeto.AplicarAlteracoes;
begin
  if assigned(FProjeto) then
  begin
    FProjeto.AtribuirInfo('descricao', EditNomeProjeto.Caption);
  end;
end;

initialization
  {$I formpropprojeto.lrs}

end.

