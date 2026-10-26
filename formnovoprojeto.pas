unit formnovoprojeto;

{$mode objfpc}{$H+}

interface

uses
  unitskins,
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
    procedure FormShow(Sender: TObject);
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
  SChooseAtLeastOneText = 'Please choose a source Bible text';
  SOldTestament = 'Old Testament';
  SNewTestament = 'New Testament';
  SWholeBible = 'Whole Bible';
  SNewTestamentFilter = 'theWord Bible modules (*.ont, *.nt)|*.ont;*.nt';
  SOldTestamentFilter = 'theWord Bible modules (*.ont, *.ot)|*.ont;*.ot';
  SWholeBibleFilter = 'theWord Bible modules (*.ont, *.ot, *.nt)|*.ont;*.ot;*.nt';

implementation

{ TFormNovoProjeto }

procedure TFormNovoProjeto.BitBtn5Click(Sender: TObject);
begin
  if EditNomeProjeto.Text = '' then
  begin
    ModalResult:=mrNone;
    ThemedMessageDlg(SNewProject, SChooseAName, mtWarning, [mbOK], 0);
    exit;
  end;

  if EditTextoOrigem.Text = '' then
  begin
    ModalResult:=mrNone;
    ThemedMessageDlg(SNewProject, SChooseAtLeastOneText, mtWarning, [mbOK], 0);
    exit;
  end;

  ModalResult:=mrOK;
end;

procedure TFormNovoProjeto.FormCreate(Sender: TObject);
begin
  OnShow := @FormShow;
  ApplySkinToForm(Self, CurrentSkin);
  Translate;
end;

procedure TFormNovoProjeto.FormShow(Sender: TObject);
begin
  HandleNeeded;
  GroupBox1.HandleNeeded;
  GroupBox2.HandleNeeded;
  RadioGroupScope.HandleNeeded;
  BitBtn1.HandleNeeded;
  BitBtn2.HandleNeeded;
  BitBtn3.HandleNeeded;
  BitBtn4.HandleNeeded;
  BitBtn5.HandleNeeded;
  BitBtn6.HandleNeeded;

  ApplySkinToForm(Self, CurrentSkin);
  {$IFDEF WINDOWS}
  HookGroupBoxControl(GroupBox1);
  HookGroupBoxControl(GroupBox2);
  HookGroupBoxControl(RadioGroupScope);
  HookButtonControl(BitBtn1);
  HookButtonControl(BitBtn2);
  HookButtonControl(BitBtn3);
  HookButtonControl(BitBtn4);
  HookButtonControl(BitBtn5);
  HookButtonControl(BitBtn6);
  {$ENDIF}
  if CurrentSkin.IsDark then
  begin
    GroupBox1.Color := CurrentSkin.PanelBg;
    GroupBox1.Font.Color := CurrentSkin.PanelText;
    GroupBox2.Color := CurrentSkin.PanelBg;
    GroupBox2.Font.Color := CurrentSkin.PanelText;
    RadioGroupScope.Color := CurrentSkin.PanelBg;
    RadioGroupScope.Font.Color := CurrentSkin.PanelText;
    EditNomeProjeto.Color := CurrentSkin.WindowBg;
    EditNomeProjeto.Font.Color := CurrentSkin.WindowText;
    EditNomeProjeto.EditLabel.Font.Color := CurrentSkin.PanelText;
    EditTextoOrigem.Color := CurrentSkin.WindowBg;
    EditTextoOrigem.Font.Color := CurrentSkin.WindowText;
    EditTextoOrigem.EditLabel.Font.Color := CurrentSkin.PanelText;
    EditTextoDestino.Color := CurrentSkin.WindowBg;
    EditTextoDestino.Font.Color := CurrentSkin.WindowText;
    EditTextoDestino.EditLabel.Font.Color := CurrentSkin.PanelText;
    EditTextoRef1.Color := CurrentSkin.WindowBg;
    EditTextoRef1.Font.Color := CurrentSkin.WindowText;
    EditTextoRef1.EditLabel.Font.Color := CurrentSkin.PanelText;
    EditTextoRef2.Color := CurrentSkin.WindowBg;
    EditTextoRef2.Font.Color := CurrentSkin.WindowText;
    EditTextoRef2.EditLabel.Font.Color := CurrentSkin.PanelText;
    BitBtn1.Font.Color := CurrentSkin.PanelText;
    BitBtn2.Font.Color := CurrentSkin.PanelText;
    BitBtn3.Font.Color := CurrentSkin.PanelText;
    BitBtn4.Font.Color := CurrentSkin.PanelText;
    BitBtn5.Font.Color := CurrentSkin.PanelText;
    BitBtn6.Font.Color := CurrentSkin.PanelText;
  end;
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

procedure TFormNovoProjeto.Translate;
begin
  with RadioGroupScope do
  begin
    Items.Clear;
    Items.Add(SOldTestament);
    Items.Add(SNewTestament);
    Items.Add(SWholeBible);
    ItemIndex := 1;
  end;
end;

initialization
  {$I formnovoprojeto.lrs}

end.
