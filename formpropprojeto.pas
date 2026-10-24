unit formpropprojeto;

{$mode objfpc}{$H+}

interface

uses
  unitskins,
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
    BitBtnTextDescription: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    Button2: TButton;
    EditNomeProjeto: TLabeledEdit;
    FontDialog1: TFontDialog;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    leTextDescription: TLabeledEdit;
    leDicStrong: TLabeledEdit;
    leDicMorfo: TLabeledEdit;
    MemoFonte: TMemo;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    TabControl1: TTabControl;
    procedure ActionCarregarTextoExecute(Sender: TObject);
    procedure ActionLimparTextoExecute(Sender: TObject);
    procedure ActionMudancaAbaExecute(Sender: TObject);
    procedure ActionSelecionarFonteExecute(Sender: TObject);
    procedure ActionSelecionarMorfoExecute(Sender: TObject);
    procedure ActionSelecionarStrongExecute(Sender: TObject);
    procedure BitBtnTextDescriptionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure leDicMorfoChange(Sender: TObject);
    procedure leDicStrongChange(Sender: TObject);
    procedure leTextDescriptionChange(Sender: TObject);
  private
    { private declarations }
    FProjeto: TProjeto;
    FBtnDefaultDicts: TButton;
    FChkUseDefaultDict: TCheckBox;
    procedure AtualizarEstadoControlesDicionario;
    procedure ChkUseDefaultDictChange(Sender: TObject);
    procedure BtnDefaultDictsClick(Sender: TObject);
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
  SWholeBibleFilter = 'theWord Bible modules (*.ont, *.ot, *.nt)|*.ont;*.ot;*.nt';
  SDictionaryFilter = 'theWord dictionary modules (*.dct.twm)|*.dct.twm|All files|*.*';
  SNewTextImportedSuccessfully = 'New text imported successfully';

implementation

uses formglobaldicts, formPrincipal;

{ TFormPropProjeto }

procedure TFormPropProjeto.FormCreate(Sender: TObject);
begin
  OnShow := @FormShow;
  ApplySkinToForm(Self, CurrentSkin);
  {$IFDEF WINDOWS}
  HookTabControl(TabControl1);
  {$ENDIF}
  FProjeto := nil;

  Label2.Top := 248;
  Button2.Top := 266;
  MemoFonte.Top := 266;
  MemoFonte.Height := 190;

  FChkUseDefaultDict := TCheckBox.Create(Self);
  FChkUseDefaultDict.Parent := TabControl1;
  FChkUseDefaultDict.SetBounds(11, 224, 420, 22);
  FChkUseDefaultDict.Caption := 'Use Default Dictionaries (overrides above)';
  FChkUseDefaultDict.Font.Style := [fsBold];
  FChkUseDefaultDict.Color := CurrentSkin.PanelBg;
  FChkUseDefaultDict.Font.Color := CurrentSkin.PanelText;
  FChkUseDefaultDict.OnChange := @ChkUseDefaultDictChange;

  FBtnDefaultDicts := TButton.Create(Self);
  FBtnDefaultDicts.Parent := Self;
  FBtnDefaultDicts.Caption := 'Default Dictionaries...';
  FBtnDefaultDicts.Left := TabControl1.Left;
  FBtnDefaultDicts.Top := BitBtn1.Top;
  FBtnDefaultDicts.Width := 160;
  FBtnDefaultDicts.Height := BitBtn1.Height;
  FBtnDefaultDicts.OnClick := @BtnDefaultDictsClick;

  {$IFDEF WINDOWS}
  HookCheckBoxControl(FChkUseDefaultDict);
  HookButtonControl(FBtnDefaultDicts);
  {$ENDIF}

  Translate;
end;

procedure TFormPropProjeto.FormShow(Sender: TObject);
begin
  ApplySkinToForm(Self, CurrentSkin);
  {$IFDEF WINDOWS}
  HookTabControl(TabControl1);
  if Assigned(FChkUseDefaultDict) then
    HookCheckBoxControl(FChkUseDefaultDict);
  HookGroupBoxControl(GroupBox2);
  HookButtonControl(BitBtn1);
  HookButtonControl(BitBtn2);
  HookButtonControl(BitBtn3);
  HookButtonControl(BitBtn4);
  HookButtonControl(BitBtn8);
  HookButtonControl(BitBtn9);
  HookButtonControl(Button2);
  if Assigned(FBtnDefaultDicts) then
    HookButtonControl(FBtnDefaultDicts);
  {$ENDIF}
  if CurrentSkin.IsDark then
  begin
    GroupBox2.Color := CurrentSkin.PanelBg;
    GroupBox2.Font.Color := CurrentSkin.PanelText;
    GroupBox2.Invalidate;
    MemoFonte.Font.Color := CurrentSkin.WindowText;
    EditNomeProjeto.Color := CurrentSkin.WindowBg;
    EditNomeProjeto.Font.Color := CurrentSkin.WindowText;
    EditNomeProjeto.EditLabel.Font.Color := CurrentSkin.PanelText;
    leTextDescription.Color := CurrentSkin.WindowBg;
    leTextDescription.Font.Color := CurrentSkin.WindowText;
    leTextDescription.EditLabel.Font.Color := CurrentSkin.PanelText;
    Label2.Font.Color := CurrentSkin.PanelText;
    if Assigned(FChkUseDefaultDict) then
    begin
      FChkUseDefaultDict.Color := CurrentSkin.PanelBg;
      FChkUseDefaultDict.Font.Color := CurrentSkin.PanelText;
      FChkUseDefaultDict.Invalidate;
    end;
    BitBtn1.Font.Color := CurrentSkin.PanelText;
    BitBtn2.Font.Color := CurrentSkin.PanelText;
    BitBtn3.Font.Color := CurrentSkin.PanelText;
    BitBtn4.Font.Color := CurrentSkin.PanelText;
    BitBtn8.Font.Color := CurrentSkin.PanelText;
    BitBtn9.Font.Color := CurrentSkin.PanelText;
    Button2.Font.Color := CurrentSkin.PanelText;
  end;
  AtualizarEstadoControlesDicionario;
  TabControl1.Invalidate;
end;

procedure TFormPropProjeto.AtualizarEstadoControlesDicionario;
var
  isDef: Boolean;
begin
  isDef := FChkUseDefaultDict.Checked;
  leDicStrong.Enabled := not isDef;
  BitBtn4.Enabled     := not isDef;
  leDicMorfo.Enabled  := not isDef;
  BitBtn8.Enabled     := not isDef;

  if isDef then
  begin
    leDicStrong.EditLabel.Caption := 'Strong''s dictionary (Default active)';
    leDicMorfo.EditLabel.Caption := 'Morphology dictionary (Default active)';
  end
  else
  begin
    leDicStrong.EditLabel.Caption := 'Strong''s dictionary';
    leDicMorfo.EditLabel.Caption := 'Morphology dictionary';
  end;

  if CurrentSkin.IsDark then
  begin
    if isDef then
    begin
      leDicStrong.Color := CurrentSkin.PanelBg;
      leDicStrong.Font.Color := CurrentSkin.WindowTextDim;
      leDicMorfo.Color := CurrentSkin.PanelBg;
      leDicMorfo.Font.Color := CurrentSkin.WindowTextDim;
    end
    else
    begin
      leDicStrong.Color := CurrentSkin.WindowBg;
      leDicStrong.Font.Color := CurrentSkin.WindowText;
      leDicMorfo.Color := CurrentSkin.WindowBg;
      leDicMorfo.Font.Color := CurrentSkin.WindowText;
    end;
    BitBtn4.Invalidate;
    BitBtn8.Invalidate;
  end;
end;

procedure TFormPropProjeto.ChkUseDefaultDictChange(Sender: TObject);
begin
  if Assigned(FProjeto) then
  begin
    FProjeto.UseDefaultDict[TTipoTextoBiblico(TabControl1.TabIndex)] := FChkUseDefaultDict.Checked;
    AtualizarEstadoControlesDicionario;
  end;
  if Assigned(FChkUseDefaultDict) then
    FChkUseDefaultDict.Invalidate;
end;

procedure TFormPropProjeto.leDicMorfoChange(Sender: TObject);
begin
  BitBtn8.Caption := SSetDictionary;
end;

procedure TFormPropProjeto.leDicStrongChange(Sender: TObject);
begin
  BitBtn4.Caption := SSetDictionary;
end;

procedure TFormPropProjeto.leTextDescriptionChange(Sender: TObject);
begin
  BitBtnTextDescription.Show;
end;

procedure TFormPropProjeto.Translate;
begin
  TabControl1.Tabs[0] := SSourceTab;
  TabControl1.Tabs[1] := SDestinationTab;
  TabControl1.Tabs[2] := SReference1Tab;
  TabControl1.Tabs[3] := SReference2Tab;
  if Assigned(FBtnDefaultDicts) then
    FBtnDefaultDicts.Caption := 'Default Dictionaries...';
  if Assigned(FChkUseDefaultDict) then
    FChkUseDefaultDict.Caption := 'Use Default Dictionaries (overrides above)';
end;

procedure TFormPropProjeto.ActionCarregarTextoExecute(Sender: TObject);
var
  replace, importOK: boolean;
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
  begin
    Enabled := False;
    importOK := FProjeto.ImportarModuloTheWord(OpenDialog1.FileName, TTipoTextoBiblico(TabControl1.TabIndex), ProgressBar1, replace);
    Enabled := True;
    ActionMudancaAbaExecute(nil);
    FProjeto.Atualizar;
    if Assigned(FrmPrincipal) then
      FrmPrincipal.AtualizarItensFiltro;
    if importOK then
      ShowMessage(SNewTextImportedSuccessfully);
  end;
end;

procedure TFormPropProjeto.ActionLimparTextoExecute(Sender: TObject);
var
  msg: string;
begin
  msg := IfThen(TTipoTextoBiblico(TabControl1.TabIndex) in [tbOrigem, tbDestino],
                SClearTextConfirmationAssociation, SClearTextConfirmation);
  if MessageDlg(SClearText, msg, mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    FProjeto.LimparTexto(TTipoTextoBiblico(TabControl1.TabIndex));
    ActionMudancaAbaExecute(nil);
    FProjeto.Atualizar;
    if Assigned(FrmPrincipal) then
      FrmPrincipal.AtualizarItensFiltro;
  end;
end;

procedure TFormPropProjeto.ActionMudancaAbaExecute(Sender: TObject);
begin
  leTextDescription.Text := FProjeto.GetTextDescription(TTipoTextoBiblico(TabControl1.TabIndex));
  BitBtnTextDescription.Hide;
  leDicStrong.Text := FProjeto.ObterInfo(format('dicstrong%d', [TabControl1.TabIndex]));
  leDicMorfo.Text  := FProjeto.ObterInfo(format('dicmorfo%d', [TabControl1.TabIndex]));
  BitBtn4.Caption  := SChooseDictionary; // seleção Strong
  BitBtn8.Caption  := SChooseDictionary; // seleção Morfo
  MemoFonte.Text   := FProjeto.ObterTextoSimplesVersiculo(TTipoTextoBiblico(TabControl1.TabIndex));
  MemoFonte.Font   := FProjeto.ObterFonteTexto(TTipoTextoBiblico(TabControl1.TabIndex));
  if CurrentSkin.IsDark then
    MemoFonte.Font.Color := CurrentSkin.WindowText;
  TabControl1.Invalidate;

  FChkUseDefaultDict.OnChange := nil;
  try
    if Assigned(FProjeto) then
      FChkUseDefaultDict.Checked := FProjeto.UseDefaultDict[TTipoTextoBiblico(TabControl1.TabIndex)]
    else
      FChkUseDefaultDict.Checked := True;
  finally
    FChkUseDefaultDict.OnChange := @ChkUseDefaultDictChange;
  end;
  AtualizarEstadoControlesDicionario;
end;

procedure TFormPropProjeto.ActionSelecionarFonteExecute(Sender: TObject);
begin
  FontDialog1.Font := MemoFonte.Font;
  if FontDialog1.Execute then
  begin
    MemoFonte.Font := FontDialog1.Font;
    if CurrentSkin.IsDark then
      MemoFonte.Font.Color := CurrentSkin.WindowText;
    FProjeto.AtribuirFonteTexto(MemoFonte.Font, [TTipoTextoBiblico(TabControl1.TabIndex)]);
  end;
end;

procedure TFormPropProjeto.ActionSelecionarMorfoExecute(Sender: TObject);
var
  d, path: string;
begin
  d := '';
  OpenDialog1.Filter := SDictionaryFilter;
  OpenDialog1.FileName := leDicMorfo.Text;
  OpenDialog1.Title  := SOpenDictionary;
  if (BitBtn8.Caption = SSetDictionary) or OpenDialog1.Execute then
  begin
    { utilizando caminho relativo sempre que possível }
    GetDir(0, d);
    if Pos(d, OpenDialog1.FileName) = 1 then
      path := format('.%s', [RightStr(OpenDialog1.Filename, length(OpenDialog1.Filename) - length(d))])
    else
      path := OpenDialog1.FileName;
    if FProjeto.AtribuirDicMorfo(path, [TTipoTextoBiblico(TabControl1.TabIndex)]) then
      leDicMorfo.Text := path;
  end;
  BitBtn8.Caption := SChooseDictionary;
end;

procedure TFormPropProjeto.ActionSelecionarStrongExecute(Sender: TObject);
var
  d, path: string;
begin
  d := '';
  OpenDialog1.Filter := SDictionaryFilter;
  OpenDialog1.FileName := leDicStrong.Text;
  OpenDialog1.Title  := SOpenDictionary;
  if (BitBtn4.Caption = SSetDictionary) or OpenDialog1.Execute then
  begin
    { utilizando caminho relativo sempre que possível }
    GetDir(0, d);
    if Pos(d, OpenDialog1.FileName) = 1 then
      path := format('.%s', [RightStr(OpenDialog1.Filename, length(OpenDialog1.Filename) - length(d))])
    else
      path := OpenDialog1.FileName;
    if FProjeto.AtribuirDicStrong(path, [TTipoTextoBiblico(TabControl1.TabIndex)]) then
      leDicStrong.Text := path;
  end;
  BitBtn4.Caption := SChooseDictionary;
end;

procedure TFormPropProjeto.BtnDefaultDictsClick(Sender: TObject);
begin
  if Assigned(FrmPrincipal) then
  begin
    if ShowGlobalDictsDialog(Self, FrmPrincipal.IniOpts) then
    begin
      FrmPrincipal.UseGlobalDicts := FrmPrincipal.IniOpts.ReadBool('dicionarios', 'use_default', FrmPrincipal.IniOpts.ReadBool('dicionarios', 'use_global', True));
      FrmPrincipal.GlobalStrongGreek := FrmPrincipal.IniOpts.ReadString('dicionarios', 'default_strong_greek', FrmPrincipal.IniOpts.ReadString('dicionarios', 'global_strong_greek', ''));
      FrmPrincipal.GlobalStrongHebrew := FrmPrincipal.IniOpts.ReadString('dicionarios', 'default_strong_hebrew', FrmPrincipal.IniOpts.ReadString('dicionarios', 'global_strong_hebrew', ''));
      FrmPrincipal.GlobalMorphology := FrmPrincipal.IniOpts.ReadString('dicionarios', 'default_morphology', FrmPrincipal.IniOpts.ReadString('dicionarios', 'global_morphology', ''));
      if Assigned(FProjeto) then
        FrmPrincipal.AplicarDicionariosGlobais(FProjeto);
      ActionMudancaAbaExecute(nil);
    end;
  end
  else
    ShowGlobalDictsDialog(Self);
end;

procedure TFormPropProjeto.BitBtnTextDescriptionClick(Sender: TObject);
begin
  FProjeto.SetTextDescription(TTipoTextoBiblico(TabControl1.TabIndex), leTextDescription.Text);
  BitBtnTextDescription.Hide;
end;

procedure TFormPropProjeto.SetProjeto(p: TProjeto);
begin
  FProjeto := p;
  EditNomeProjeto.Caption := p.ObterInfo('descricao');
  ApplySkinToForm(Self, CurrentSkin);
  {$IFDEF WINDOWS}
  HookTabControl(TabControl1);
  {$ENDIF}
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

