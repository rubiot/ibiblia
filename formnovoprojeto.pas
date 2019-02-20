unit formnovoprojeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LCLTranslator;

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
    RadioGroupScope: TRadioGroup;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure SelecionarTexto(edit: TLabeledEdit);
  public
    { public declarations }
    procedure Translate;
  end; 

var
  FormNovoProjeto1: TFormNovoProjeto;

resourcestring
  SNewProject = 'New project';
  SChooseAName = 'Please choose a name for your project';
  SChooseAtLeastTwoTexts = 'Please choose at least one source and one destination texts';
  SOldTestament = 'Old Testament';
  SNewTestament = 'New Testament';
  SNewTestamentFilter = 'theWord bible modules (*.ont, *.nt)|*.ont;*.nt';
  SOldTestamentFilter = 'theWord bible modules (*.ont, *.ot)|*.ont;*.ot';

implementation

{ TFormNovoProjeto }

procedure TFormNovoProjeto.BitBtn5Click(Sender: TObject);
begin
  //TBitBtn(Sender).ModalResult := mrNone;

  if EditNomeProjeto.Text = '' then
  begin
    ModalResult:=mrNone;
    MessageDlg(SNewProject, SChooseAName, mtWarning, [mbOK], 0);
    exit;
  end;

  if (EditTextoOrigem.Text = '') or (EditTextoDestino.Text = '') then
  begin
    ModalResult:=mrNone;
    MessageDlg(SNewProject, SChooseAtLeastTwoTexts, mtWarning, [mbOK], 0);
    exit;
  end;

  //TBitBtn(Sender).ModalResult := mrOK;
  ModalResult:=mrOK;
  //Close;
end;

procedure TFormNovoProjeto.FormCreate(Sender: TObject);
begin
  Translate;
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

procedure TFormNovoProjeto.SelecionarTexto(edit: TLabeledEdit);
begin
  case RadioGroupScope.ItemIndex of
    0: OpenDialog1.Filter := SOldTestamentFilter;
    1: OpenDialog1.Filter := SNewTestamentFilter;
  end;
  if OpenDialog1.Execute then
    edit.Text := OpenDialog1.FileName;
end;

procedure TFormNovoProjeto.Translate;
begin
  with RadioGroupScope do
  begin
    Items.Clear;
    Items.Add(SOldTestament);
    Items.Add(SNewTestament);
    ItemIndex := 1;
  end;
end;

initialization
  {$I formnovoprojeto.lrs}

end.

