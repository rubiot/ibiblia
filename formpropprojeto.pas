unit formpropprojeto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, ActnList, Projeto, LCLTranslator,
  StrUtils;

type

  { TFrmPropProjeto }

  { TFormPropProjeto }

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
    procedure FormCreate(Sender: TObject);
    procedure leDicMorfoChange(Sender: TObject);
    procedure leDicStrongChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetProjeto(p: TProjeto);
    procedure AplicarAlteracoes;
    procedure Translate;
  end;

var
  FormPropProjeto1: TFormPropProjeto;

resourcestring
  SSetDictionary = 'Set';
  SChooseABibleModule = 'Please choose a theWord Bible module file...';
  SChooseAnotherText = 'Replace text';
  SChangeTextConfirmation = 'Are you sure you want to replace this Bible text with a new one?';
  SChangeTextConfirmationAssociation = 'Loading a new Bible text will cause all your existing associations to be discarded. ' +
                                       'But, if you click ''Yes'', iBiblia can try to keep existing associations for you.'#13#10#13#10 +
                                       'Would you like iBiblia to do that?';
  SClearText = 'Clear Bible text';
  SClearTextConfirmation = 'Are you sure you want to clear this Bible text?';
  SClearTextConfirmationAssociation = 'Are you sure you want to clear this Bible text?'#13#10 +
                                      'All your associations will be lost!';
  SChooseDictionary = 'Choose';
  SOpenDictionary = 'Please choose a theWord dictionary module...';
  SSourceTab = 'Source';
  SDestinationTab = 'Destination';
  SReference1Tab = 'Reference 1';
  SReference2Tab = 'Reference 2';
  SNewTestamentFilter = 'theWord Bible modules (*.ont, *.nt)|*.ont;*.nt';
  SOldTestamentFilter = 'theWord Bible modules (*.ont, *.ot)|*.ont;*.ot';
  SWholeBibleFilter = 'theWord Bible modules (*.ont)|*.ont';

implementation

{ TFormPropProjeto }

procedure TFormPropProjeto.FormCreate(Sender: TObject);
begin
  FProjeto := nil;
  Translate;
end;

procedure TFormPropProjeto.leDicMorfoChange(Sender: TObject);
begin
  BitBtn8.Caption := SSetDictionary;
end;

procedure TFormPropProjeto.leDicStrongChange(Sender: TObject);
begin
  BitBtn4.Caption := SSetDictionary;
end;

procedure TFormPropProjeto.Translate;
begin
  TabControl1.Tabs[0] := SSourceTab;
  TabControl1.Tabs[1] := SDestinationTab;
  TabControl1.Tabs[2] := SReference1Tab;
  TabControl1.Tabs[3] := SReference2Tab;
end;

procedure TFormPropProjeto.ActionCarregarTextoExecute(Sender: TObject);
var
  replace: boolean;
begin
  if TTipoTextoBiblico(TabControl1.TabIndex) in [tbOrigem, tbDestino] then
  begin
    case MessageDlg(SChooseAnotherText, SChangeTextConfirmationAssociation, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: replace := true;
      mrNo: replace := false;
      mrCancel: exit;
    end;
  end else
  begin
    if MessageDlg(SChooseAnotherText, SChangeTextConfirmation, mtConfirmation, [mbYes, mbCancel], 0) = mrCancel then
      exit;
    replace := false;
  end;

  case FProjeto.Escopo of
    etOT: OpenDialog1.Filter := SOldTestamentFilter;
    etNT: OpenDialog1.Filter := SNewTestamentFilter;
    etONT: OpenDialog1.Filter := SWholeBibleFilter;
  end;

  OpenDialog1.Title  := SChooseABibleModule;
  if OpenDialog1.Execute then
    FProjeto.ImportarModuloTheWord(OpenDialog1.FileName, TTipoTextoBiblico(TabControl1.TabIndex), ProgressBar1, replace);
end;

procedure TFormPropProjeto.ActionLimparTextoExecute(Sender: TObject);
var
  msg: string;
begin
  msg := IfThen(TTipoTextoBiblico(TabControl1.TabIndex) in [tbOrigem, tbDestino],
                SClearTextConfirmationAssociation, SClearTextConfirmation);
  if MessageDlg(SClearText, msg, mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
    FProjeto.LimparTexto(TTipoTextoBiblico(TabControl1.TabIndex));
end;

procedure TFormPropProjeto.ActionMudancaAbaExecute(Sender: TObject);
begin
  leDicStrong.Text := FProjeto.ObterInfo(format('dicstrong%d', [TabControl1.TabIndex]));
  leDicMorfo.Text  := FProjeto.ObterInfo(format('dicmorfo%d', [TabControl1.TabIndex]));
  BitBtn4.Caption  := SChooseDictionary; // seleção Strong
  BitBtn8.Caption  := SChooseDictionary; // seleção Morfo
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
  OpenDialog1.Title  := SOpenDictionary;
  if (BitBtn8.Caption = SSetDictionary) or OpenDialog1.Execute then
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
  BitBtn8.Caption := SChooseDictionary;
end;

procedure TFormPropProjeto.ActionSelecionarStrongExecute(Sender: TObject);
var
  d: string;
begin
  //OpenDialog1.Filter := '*.nt; *.ont';
  OpenDialog1.FileName := leDicStrong.Text;
  OpenDialog1.Title  := SOpenDictionary;
  if (BitBtn4.Caption = SSetDictionary) or OpenDialog1.Execute then
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
  BitBtn4.Caption := SChooseDictionary;
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

