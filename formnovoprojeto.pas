unit formnovoprojeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LCLTranslator, ONTTokenizer;

type

  { TFormNovoProjeto }

  TFormNovoProjeto = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    cbTokenizerVersion: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    EditTextoOrigem: TLabeledEdit;
    EditTextoDestino: TLabeledEdit;
    EditTextoRef1: TLabeledEdit;
    EditTextoRef2: TLabeledEdit;
    EditNomeProjeto: TLabeledEdit;
    LabelTokenizerVersion: TLabel;
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
    procedure LoadTokenizerVersions(selectedVersion: Integer);
    procedure SelecionarTexto(edit: TLabeledEdit);
  public
    { public declarations }
    function GetTokenizerVersion: Integer;
    procedure Translate;
  end; 

var
  FormNovoProjeto1: TFormNovoProjeto;

resourcestring
  SNewProject = 'New project';
  SChooseAName = 'Please choose a name for your project';
  SChooseAtLeastOneText = 'Please choose a source Bible text';
  SOldTestament = 'Old Testament';
  SNewTestament = 'New Testament';
  SWholeBible = 'Whole Bible';
  STokenizerVersion = 'Tokenizer version';
  SNewTestamentFilter = 'theWord Bible modules (*.ont, *.nt)|*.ont;*.nt';
  SOldTestamentFilter = 'theWord Bible modules (*.ont, *.ot)|*.ont;*.ot';
  SWholeBibleFilter = 'theWord Bible modules (*.ont, *.ot, *.nt)|*.ont;*.ot;*.nt';

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

  if EditTextoOrigem.Text = '' then
  begin
    ModalResult:=mrNone;
    MessageDlg(SNewProject, SChooseAtLeastOneText, mtWarning, [mbOK], 0);
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
    2: OpenDialog1.Filter := SWholeBibleFilter;
  end;
  if OpenDialog1.Execute then
    edit.Text := OpenDialog1.FileName;
end;

procedure TFormNovoProjeto.LoadTokenizerVersions(selectedVersion: Integer);
var
  i: Integer;
begin
  TTokenizerFactory.PopulateVersionCaptions(cbTokenizerVersion.Items);
  cbTokenizerVersion.ItemIndex := -1;

  for i := 0 to cbTokenizerVersion.Items.Count - 1 do
    if PtrInt(cbTokenizerVersion.Items.Objects[i]) = selectedVersion then
    begin
      cbTokenizerVersion.ItemIndex := i;
      Exit;
    end;

  if cbTokenizerVersion.Items.Count > 0 then
    cbTokenizerVersion.ItemIndex := cbTokenizerVersion.Items.Count - 1;
end;

function TFormNovoProjeto.GetTokenizerVersion: Integer;
begin
  if cbTokenizerVersion.ItemIndex >= 0 then
    Result := PtrInt(cbTokenizerVersion.Items.Objects[cbTokenizerVersion.ItemIndex])
  else
    Result := TTokenizerFactory.LATEST_VERSION;
end;

procedure TFormNovoProjeto.Translate;
var
  selectedVersion: Integer;
begin
  selectedVersion := GetTokenizerVersion;
  with RadioGroupScope do
  begin
    Items.Clear;
    Items.Add(SOldTestament);
    Items.Add(SNewTestament);
    Items.Add(SWholeBible);
    ItemIndex := 1;
  end;
  LabelTokenizerVersion.Caption := STokenizerVersion;
  LoadTokenizerVersions(selectedVersion);
end;

initialization
{$IFNDEF FPCUNIT}
  {$I formnovoprojeto.lrs}
{$ENDIF}

end.

