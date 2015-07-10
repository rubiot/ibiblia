unit formnovoprojeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type

  { TFormNovoProjeto }

  TFormNovoProjeto = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    EditTextoOrigem: TLabeledEdit;
    EditTextoDestino: TLabeledEdit;
    EditTextoRef1: TLabeledEdit;
    EditTextoRef2: TLabeledEdit;
    EditNomeProjeto: TLabeledEdit;
    OpenDialog1: TOpenDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
    procedure SelecionarTexto(edit: TLabeledEdit);
  public

    { public declarations }
  end; 

var
  FormNovoProjeto1: TFormNovoProjeto;

implementation

{ TFormNovoProjeto }

procedure TFormNovoProjeto.BitBtn5Click(Sender: TObject);
begin
  //TBitBtn(Sender).ModalResult := mrNone;

  if EditNomeProjeto.Text = '' then
  begin
    ModalResult:=mrNone;
    MessageDlg('Novo projeto', 'Escolha um nome para o projeto', mtWarning, [mbOK], 0);
    exit;
  end;

  if (EditTextoOrigem.Text = '') or (EditTextoDestino.Text = '') then
  begin
    ModalResult:=mrNone;
    MessageDlg('Novo projeto', 'Escolha pelo menos um texto origem e um texto destino', mtWarning, [mbOK], 0);
    exit;
  end;

  //TBitBtn(Sender).ModalResult := mrOK;
  ModalResult:=mrOK;
  //Close;
end;

procedure TFormNovoProjeto.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin

end;

procedure TFormNovoProjeto.BitBtn1Click(Sender: TObject);
begin
  SelecionarTexto(EditTextoOrigem);
end;

procedure TFormNovoProjeto.BitBtn2Click(Sender: TObject);
begin
  SelecionarTexto(EditTextoDestino);
end;

procedure TFormNovoProjeto.BitBtn3Click(Sender: TObject);
begin
  SelecionarTexto(EditTextoRef1);
end;

procedure TFormNovoProjeto.BitBtn4Click(Sender: TObject);
begin
  SelecionarTexto(EditTextoRef2);
end;

procedure TFormNovoProjeto.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin

end;

procedure TFormNovoProjeto.SelecionarTexto(edit: TLabeledEdit);
begin
  if OpenDialog1.Execute then
    edit.Text := OpenDialog1.FileName;
end;

initialization
  {$I formnovoprojeto.lrs}

end.

