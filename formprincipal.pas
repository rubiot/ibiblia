unit formPrincipal;

{$mode objfpc}{$H+}

{TODO -cDesejável: Confirmar se importa a ordem de associacao dos sintagmas }

interface

uses
  {$IFDEF WINDOWS}
  windows, twAutomate, TwSyncThread,
  {$ELSE}
  lclintf,
  {$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, ExtCtrls, StdCtrls, Projeto, IniFiles, Versiculo, Math;

type

  { TFrmPrincipal }

  TFrmPrincipal = class(TForm)
    ActionSyncTheWordVerse: TAction;
    ActionRecriarBaseSugestoes: TAction;
    ActionMesclarProjetos: TAction;
    ActionExportar: TAction;
    ActionPropProjeto: TAction;
    ActionSalvarProjetoComo: TAction;
    ActionExportarTextoInterlinear: TAction;
    ActionExportarDestinoComStrongs: TAction;
    ActionVersoUltimo: TAction;
    ActionVersoPrimeiro: TAction;
    ActionLimparAssociacoes: TAction;
    ActionReverterAssociacoes: TAction;
    ActionSalvarProjeto: TAction;
    ActionQuandoNovoVersiculo: TAction;
    ActionSugerirAssociacao: TAction;
    ActionVersoAnterior: TAction;
    ActionVersoSeguinte: TAction;
    ActionFecharProjeto: TAction;
    ActionSair: TAction;
    ActionAbrirProjeto: TAction;
    ActionNovoProjeto: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    ImageList64x64: TImageList;
    ImageListTreeView: TImageList;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItemRecent: TMenuItem;
    MenuItemSynciBiblia: TMenuItem;
    MenuItemSyncTheWord: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    RadioGroup1: TRadioGroup;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollBox3: TScrollBox;
    ScrollBox4: TScrollBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TreeView1: TTreeView;
    procedure ActionExportarExecute(Sender: TObject);
    procedure ActionAbrirProjetoExecute(Sender: TObject);
    procedure ActionExportarDestinoComStrongsExecute(Sender: TObject);
    procedure ActionExportarTextoInterlinearExecute(Sender: TObject);
    procedure ActionFecharProjetoExecute(Sender: TObject);
    procedure ActionLimparAssociacoesExecute(Sender: TObject);
    procedure ActionMesclarProjetosExecute(Sender: TObject);
    procedure ActionNovoProjetoExecute(Sender: TObject);
    procedure ActionPropProjetoExecute(Sender: TObject);
    procedure ActionQuandoNovoVersiculoExecute(Sender: TObject);
    procedure ActionRecriarBaseSugestoesExecute(Sender: TObject);
    procedure ActionReverterAssociacoesExecute(Sender: TObject);
    procedure ActionSairExecute(Sender: TObject);
    procedure ActionSalvarProjetoComoExecute(Sender: TObject);
    procedure ActionSalvarProjetoExecute(Sender: TObject);
    procedure ActionSugerirAssociacaoExecute(Sender: TObject);
    procedure ActionSyncTheWordVerseExecute(Sender: TObject);
    procedure ActionVersoAnteriorExecute(Sender: TObject);
    procedure ActionVersoPrimeiroExecute(Sender: TObject);
    procedure ActionVersoSeguinteExecute(Sender: TObject);
    procedure ActionVersoUltimoExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure AbrirRecenteClick(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItemSynciBibliaClick(Sender: TObject);
    procedure MenuItemSyncTheWordClick(Sender: TObject);
    procedure QuandoNovoVersiculo(Sender: TProjeto);
    procedure QuandoPalavraClicada(Sender: TSintagma);
    procedure QuandoAlterarVersiculo;
    procedure AtualizarMRU(m: TMenuItem);
    procedure CarregarMRU(m: TMenuItem);
    procedure DescarregarMRU(m: TMenuItem);
  private
    { private declarations }
    syncTw2iBiblia: boolean;
    synciBiblia2Tw: boolean;
    TwSyncThread: TTwSyncThread;
    procedure SyncToTwRef(Ref: string);
    procedure SetUpSyncThread;
  public
    { public declarations }
  end; 

type
  TParametroThread = record
    texto: TTipoTextoBiblico;
    pb: TProgressBar;
  end;

var
  FrmPrincipal: TFrmPrincipal;
  ProjetoAtual: TProjeto;
  opts: TIniFile;

const
  MAX_MRU = 10;

implementation

uses formnovoprojeto, formpropprojeto, formexportar, formmesclarprojetos;

{$R *.lfm}

{$IFDEF WINDOWS}
{------------------------------------------------------------------------------}
function RestoreTheWord(RestoreWindow: Boolean = True): THandle;
begin
  Result := twAutomate.TWAutomateUtils.IsTwRunning;
  if Result <> 0 then begin
    if RestoreWindow then begin
      if IsIconic(Result) then
        SendMessage(Result, WM_SYSCOMMAND, SC_RESTORE, 0);
      if Result <> GetForegroundWindow then begin
        BringWindowToTop(Result);
        SetForegroundWindow(Result);
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure SyncTheWordVerse(ref: string);
var
  twHWND: THandle;
  CData : TCopyDataStruct;
  bcv: TBCV_A;
  b, c, v: integer;
begin
  twHWND := RestoreTheWord(False);
  if twHWND <> 0 then begin
    SScanf(ref,'%d,%d,%d',[@b, @c, @v]);
    bcv.bi := b;
    bcv.ci := c;
    bcv.vi := v;
    bcv.span := 0;

    CData.dwData := twAutomate.COPYDATA_OP_GOTOVERSE;
    CData.lpData := @bcv;
    CData.cbData := SizeOf(bcv);
    SendMessage(twHWND, WM_COPYDATA, 0, DWORD(@CData));
  end;
end;

procedure SyncTheWordDict(ref: string);
var
  twHWND: THandle;
  CData : TCopyDataStruct;
  CD: TCopyData_Op_DctWordLookup;
  ws: WideString;
begin
  twHWND := RestoreTheWord(False);
  if twHWND <> 0 then begin
    ws := ref;

    CData.dwData := twAutomate.COPYDATA_OP_DCTWORDLOOKUP;
    FillChar(CD.phrase[1], 0, Length(CD.phrase)*2); //zero fill the array first, VERY IMPORTANT
    Move(ws[1], CD.phrase[1], 2*Math.max(Length(ws), Length(CD.phrase)));
    CData.lpData := @CD;
    CData.cbData := SizeOf(TCopyData_Op_DctWordLookup);
    SendMessage(twHWND, WM_COPYDATA, 0, DWORD(@CData) );
  end;
end;
{$ENDIF}

{ TFrmPrincipal }

procedure TFrmPrincipal.ActionAbrirProjetoExecute(Sender: TObject);
begin
  //OpenDialog1.Options := OpenDialog1.Options;
  if Sender is TMenuItem then
     OpenDialog1.FileName := TMenuItem(Sender).Caption
  else if not OpenDialog1.Execute then
    exit;

  if ProjetoAtual <> nil then
    ActionFecharProjetoExecute(nil);

  if (ProjetoAtual <> nil) or (OpenDialog1.FileName = '(-)') then
    exit;

  if not FileExists(OpenDialog1.FileName) then
  begin
    MessageDlg('Abrir projeto', format('O projeto selecionado não existe:'#13#10'%s', [OpenDialog1.FileName]), mtError, [mbOK], 0);
    exit;
  end;

  ProjetoAtual := TProjeto.Criar([ScrollBox1, ScrollBox2, ScrollBox3, ScrollBox4], TreeView1, RadioGroup1, Memo1);
  ProjetoAtual.OnNovoVersiculo := @QuandoNovoVersiculo;
  ProjetoAtual.OnAlterarVersiculo := @QuandoAlterarVersiculo;
  ProjetoAtual.OnSintagmaClick := @QuandoPalavraClicada;
  ProjetoAtual.Abrir(OpenDialog1.FileName);
  ProjetoAtual.ExibirDefinicoesSoComCtrl := MenuItem21.Checked;
  ProjetoAtual.SugerirAssociacaoAutomaticamente := MenuItem22.Checked;

  ActionSalvarProjeto.Enabled := true;
  ActionSalvarProjetoComo.Enabled := true;
  ActionFecharProjeto.Enabled := true;
  ActionPropProjeto.Enabled := true;
  ActionVersoPrimeiro.Enabled := true;
  ActionVersoUltimo.Enabled := true;
  ActionVersoSeguinte.Enabled := true;
  ActionVersoAnterior.Enabled := true;
  ActionSugerirAssociacao.Enabled := true;
  ActionReverterAssociacoes.Enabled := true;
  ActionLimparAssociacoes.Enabled := true;
  ActionExportar.Enabled := true;
  //ActionExportarDestinoComStrongs.Enabled := true;
  //ActionExportarTextoInterlinear.Enabled := true;
  StatusBar1.SimpleText := '';

  SetUpSyncThread;
end;

procedure TFrmPrincipal.ActionExportarExecute(Sender: TObject);
begin
  if not assigned(ProjetoAtual) then
    exit;
  frmExportarProjeto.SetProjeto(ProjetoAtual);
  frmExportarProjeto.ShowModal;
end;

procedure TFrmPrincipal.ActionExportarDestinoComStrongsExecute(Sender: TObject);
begin
  if (ProjetoAtual <> nil) and SaveDialog2.Execute then
    ProjetoAtual.ExportarTextoDestinoComStrongs(SaveDialog2.FileName, []{, ProgressBar1});
  //ProgressBar1.Visible := false;
end;

procedure TFrmPrincipal.ActionExportarTextoInterlinearExecute(Sender: TObject);
begin
  if (ProjetoAtual <> nil) and SaveDialog2.Execute then
    ProjetoAtual.ExportarTextoInterlinear(SaveDialog2.FileName, []{, ProgressBar1});
  //ProgressBar1.Visible := false;
end;

procedure TFrmPrincipal.ActionFecharProjetoExecute(Sender: TObject);
var
  salvar: boolean;
begin
  if ProjetoAtual <> nil then
  begin
    salvar := false;
    if ProjetoAtual.Modificado then
    begin
      case MessageDlg('Fechar projeto', 'Deseja salvar as alterações?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: salvar := true;
        mrNo: salvar := false;
        mrCancel: exit;
      end;
    end;

    AtualizarMRU(MenuItemRecent);

    ProjetoAtual.Fechar(salvar);
    ProjetoAtual.Destruir;
    ProjetoAtual := nil;
    Caption := 'iBiblia';

    ActionSalvarProjeto.Enabled := false;
    ActionSalvarProjetoComo.Enabled := false;
    ActionFecharProjeto.Enabled := false;
    ActionPropProjeto.Enabled := false;
    ActionVersoPrimeiro.Enabled := false;
    ActionVersoUltimo.Enabled := false;
    ActionVersoSeguinte.Enabled := false;
    ActionVersoAnterior.Enabled := false;
    ActionSugerirAssociacao.Enabled := false;
    ActionReverterAssociacoes.Enabled := false;
    ActionLimparAssociacoes.Enabled := false;
    ActionExportar.Enabled := false;
    //ActionExportarDestinoComStrongs.Enabled := false;
    //ActionExportarTextoInterlinear.Enabled := false;
    StatusBar1.SimpleText := '';
  end;
end;

procedure TFrmPrincipal.ActionLimparAssociacoesExecute(Sender: TObject);
begin
  if ProjetoAtual <> nil then
    ProjetoAtual.Limpar;
end;

procedure TFrmPrincipal.ActionMesclarProjetosExecute(Sender: TObject);
begin
  FrmMesclarProjetos.InicializarForm;
  FrmMesclarProjetos.ShowModal;
end;

function CarregarTexto(par: pointer): ptrint;
var
  edit: TLabeledEdit;
  p: TParametroThread;
begin
  p := TParametroThread(par^);
  case p.texto of
    tbOrigem: edit := FormNovoProjeto1.EditTextoOrigem;
    tbDestino: edit := FormNovoProjeto1.EditTextoDestino;
    tbConsulta1: edit := FormNovoProjeto1.EditTextoRef1;
    tbConsulta2: edit := FormNovoProjeto1.EditTextoRef2;
  end;

  if edit.Text <> '' then
    ProjetoAtual.ImportarModuloTheWord(edit.Text, p.texto, p.pb);

  result := 0;
end;

procedure TFrmPrincipal.ActionNovoProjetoExecute(Sender: TObject);
  function QualTexto(v: TTipoTextoBiblico): string;
  begin
    case v of
      tbOrigem: result := 'origem';
      tbDestino: result := 'destino';
      tbConsulta1: result := 'referência 1';
      tbConsulta2: result := 'referência 2';
    end;
  end;

var
  v: TTipoTextoBiblico;
  ParThread: TParametroThread;
begin
  ActionFecharProjetoExecute(nil);

  if ProjetoAtual <> nil then
    exit;

  FormNovoProjeto1.EditNomeProjeto.Text := 'Novo projeto de associação';

  if (FormNovoProjeto1.ShowModal = mrOK) and SaveDialog1.Execute then
  begin
    ProjetoAtual := TProjeto.Criar([ScrollBox1, ScrollBox2, ScrollBox3, ScrollBox4], TreeView1, RadioGroup1, Memo1);
    ProjetoAtual.OnNovoVersiculo := @QuandoNovoVersiculo;
    ProjetoAtual.OnAlterarVersiculo := @QuandoAlterarVersiculo;
    ProjetoAtual.Novo(SaveDialog1.FileName, FormNovoProjeto1.EditNomeProjeto.Text);
    ProjetoAtual.ExibirDefinicoesSoComCtrl := MenuItem21.Checked;
    ProjetoAtual.SugerirAssociacaoAutomaticamente := MenuItem22.Checked;

    //ParThread.pb := ProgressBar1;
    for v:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
    begin
      StatusBar1.Caption := Format('Carregando %s...', [QualTexto(v)]);
      ParThread.texto := v;
      ParThread.pb := ProgressBar1;
      Application.ProcessMessages;
      //WaitForThreadTerminate(BeginThread(@CarregarTexto, Pointer(@ParThread)), 0);
      CarregarTexto(Pointer(@ParThread));
    end;
    StatusBar1.Caption:='';

    ProjetoAtual.Commit;
    ProjetoAtual.Atualizar;

    //ProgressBar1.Visible := false;
    ActionSalvarProjeto.Enabled := true;
    ActionSalvarProjetoComo.Enabled := true;
    ActionFecharProjeto.Enabled := true;
    ActionPropProjeto.Enabled := true;
    ActionVersoPrimeiro.Enabled := true;
    ActionVersoUltimo.Enabled := true;
    ActionVersoSeguinte.Enabled := true;
    ActionVersoAnterior.Enabled := true;
    ActionSugerirAssociacao.Enabled := true;
    ActionReverterAssociacoes.Enabled := true;
    ActionLimparAssociacoes.Enabled := true;
    ActionExportar.Enabled := true;
    //ActionExportarDestinoComStrongs.Enabled := true;
    //ActionExportarTextoInterlinear.Enabled := true;

    StatusBar1.SimpleText := '';
  end;
end;

procedure TFrmPrincipal.ActionPropProjetoExecute(Sender: TObject);
begin
  FormPropProjeto1.SetProjeto(ProjetoAtual);
  if FormPropProjeto1.ShowModal = mrOK then
  begin
    FormPropProjeto1.AplicarAlteracoes;
    ActionQuandoNovoVersiculo.Execute;
  end;
end;

procedure TFrmPrincipal.ActionQuandoNovoVersiculoExecute(Sender: TObject);
begin
  if ProjetoAtual <> nil then
  begin
    Caption := 'iBiblia - [' + ProjetoAtual.ObterInfo('descricao') + '] - ' +  ProjetoAtual.Referencia;
    RadioGroup1.SetFocus;
    ActionSyncTheWordVerseExecute(Sender);
  end;
end;

procedure TFrmPrincipal.ActionRecriarBaseSugestoesExecute(Sender: TObject);
begin
  if ProjetoAtual <> nil then
    ProjetoAtual.RecriarBaseSugestoes(ProgressBar1);
end;

procedure TFrmPrincipal.ActionReverterAssociacoesExecute(Sender: TObject);
begin
  if ProjetoAtual <> nil then
    ProjetoAtual.Atualizar;
end;

procedure TFrmPrincipal.ActionSairExecute(Sender: TObject);
begin
  ActionFecharProjetoExecute(nil);
  Close;
end;

procedure TFrmPrincipal.ActionSalvarProjetoComoExecute(Sender: TObject);
begin
  {if ProjetoAtual <> nil then
  begin
    old := ProjetoAtual.Caminho;
    ProjetoAtual.Fechar(true);
    ProjetoAtual.Novo(old, );

  end;
  }
end;

procedure TFrmPrincipal.ActionSalvarProjetoExecute(Sender: TObject);
begin
  if ProjetoAtual <> nil then
  begin
    ProjetoAtual.Salvar;
    StatusBar1.SimpleText := '';
  end;
end;

procedure TFrmPrincipal.ActionSugerirAssociacaoExecute(Sender: TObject);
begin
  if ProjetoAtual <> nil then
    ProjetoAtual.SugerirAssociacao;
end;

procedure TFrmPrincipal.ActionSyncTheWordVerseExecute(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  if syncTw2iBiblia and (ProjetoAtual <> nil) then
    SyncTheWordVerse(ProjetoAtual.ID);
  {$ENDIF}
end;

procedure TFrmPrincipal.ActionVersoAnteriorExecute(Sender: TObject);
begin
  if ProjetoAtual <> nil then
    ProjetoAtual.VersiculoAnterior;
end;

procedure TFrmPrincipal.ActionVersoPrimeiroExecute(Sender: TObject);
begin
  if ProjetoAtual <> nil then
    ProjetoAtual.VersiculoInicial;
end;

procedure TFrmPrincipal.ActionVersoSeguinteExecute(Sender: TObject);
begin
  if ProjetoAtual <> nil then
    ProjetoAtual.VersiculoSeguinte;
end;

procedure TFrmPrincipal.ActionVersoUltimoExecute(Sender: TObject);
begin
  if ProjetoAtual <> nil then
    ProjetoAtual.VersiculoFinal;
end;

procedure TFrmPrincipal.FormActivate(Sender: TObject);
begin

end;

procedure TFrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ActionFecharProjetoExecute(Sender);
end;

procedure TFrmPrincipal.FormCreate(Sender: TObject);
begin
  ProjetoAtual := nil;
  opts := TIniFile.Create('iBiblia.ini');

  Width  := opts.ReadInteger('leiaute', 'principal.largura',  Width);
  Height := opts.ReadInteger('leiaute', 'principal.altura',   Height);
  MenuItem21.Checked := opts.ReadBool('opcoes', 'definicoes.com.ctrl', true);
  MenuItem22.Checked := opts.ReadBool('opcoes', 'sugestoes.automaticas', false);
  MenuItemSyncTheWord.Checked := opts.ReadBool('opcoes', 'synctheword', false);
  MenuItemSynciBiblia.Checked := opts.ReadBool('opcoes', 'syncibiblia', false);

  CarregarMRU(MenuItemRecent);
  TreeView1.Width := opts.ReadInteger('leiaute', 'principal.treeview.largura', TreeView1.Width);

  //opts.Free; ainda será usado em FormShow e FormDestroy

  //Application.AddOnActivateHandler(@FormActivate);
  //Application.AddOnDeactivateHandler(@FormDeactivate);

  syncTw2iBiblia := MenuItemSyncTheWord.Checked;
  synciBiblia2Tw := MenuItemSynciBiblia.Checked;
  SetUpSyncThread;
end;

procedure TFrmPrincipal.FormDeactivate(Sender: TObject);
begin

end;

procedure TFrmPrincipal.FormDestroy(Sender: TObject);
begin
  DescarregarMRU(MenuItemRecent);
  opts.WriteInteger('leiaute', 'principal.largura', Width);
  opts.WriteInteger('leiaute', 'principal.altura', Height);
  opts.WriteInteger('leiaute', 'principal.topo', Top);
  opts.WriteInteger('leiaute', 'principal.esquerda', Left);
  opts.WriteInteger('leiaute', 'principal.treeview.largura', TreeView1.Width);
  opts.WriteInteger('leiaute', 'principal.splitter2.topo', Splitter2.Top);
  opts.WriteInteger('leiaute', 'principal.splitter3.topo', Splitter3.Top);
  opts.WriteInteger('leiaute', 'principal.splitter4.topo', Splitter4.Top);
  opts.WriteBool('opcoes', 'definicoes.com.ctrl', MenuItem21.Checked);
  opts.WriteBool('opcoes', 'sugestoes.automaticas', MenuItem22.Checked);
  opts.WriteBool('opcoes', 'synctheword', MenuItemSyncTheWord.Checked);
  opts.WriteBool('opcoes', 'syncibiblia', MenuItemSynciBiblia.Checked);
  opts.Free;
end;

procedure TFrmPrincipal.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    '1', '2', '3', '4': if assigned(ProjetoAtual) then RadioGroup1.ItemIndex := Ord(Key) - Ord('0') - 1;
  end;
end;

procedure TFrmPrincipal.FormShow(Sender: TObject);
var
  x, y: Integer;
begin
  x := opts.ReadInteger('leiaute', 'principal.esquerda', Left);
  y := opts.ReadInteger('leiaute', 'principal.topo', Top);

  { garantindo que pelo menos parte do form esteja dentro da área visível da tela }
  if x > Screen.Width then
    x := Screen.Width - 200;
  if y > Screen.Height then
    y := Screen.Height - 200;

  //if (x + Width) > Screen.Width then
  //  x := x - ((x + Width) - Screen.Width);
  //if (y + Height) > Screen.Height then
  //  y := y - ((y + Height) - Screen.Height);

  Top           := y;
  Left          := x;
  Splitter2.Top := opts.ReadInteger('leiaute', 'principal.splitter2.topo', Splitter2.Top);
  Splitter3.Top := opts.ReadInteger('leiaute', 'principal.splitter3.topo', Splitter3.Top);
  Splitter4.Top := opts.ReadInteger('leiaute', 'principal.splitter4.topo', Splitter4.Top);
end;

procedure TFrmPrincipal.MenuItem21Click(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if assigned(ProjetoAtual) then
    ProjetoAtual.ExibirDefinicoesSoComCtrl := TMenuItem(Sender).Checked;
end;

procedure TFrmPrincipal.MenuItem22Click(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if assigned(ProjetoAtual) then
    ProjetoAtual.SugerirAssociacaoAutomaticamente := TMenuItem(Sender).Checked;
end;

procedure TFrmPrincipal.AbrirRecenteClick(Sender: TObject);
begin
  if TMenuItem(Sender).Caption <> '(-)' then
     ActionAbrirProjetoExecute(Sender);
end;

procedure TFrmPrincipal.MenuItem24Click(Sender: TObject);
begin
end;

procedure TFrmPrincipal.MenuItemSynciBibliaClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  synciBiblia2Tw := TMenuItem(Sender).Checked;
  SetUpSyncThread;
end;

procedure TFrmPrincipal.MenuItemSyncTheWordClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  syncTw2iBiblia := TMenuItem(Sender).Checked;
end;

procedure TFrmPrincipal.QuandoNovoVersiculo(Sender: TProjeto);
begin
  ActionQuandoNovoVersiculoExecute(nil);
end;

procedure TFrmPrincipal.QuandoPalavraClicada(Sender: TSintagma);
begin
  {$IFDEF WINDOWS}
  if not syncTw2iBiblia or (Sender.Strong.Count = 0) then
    exit;
  SyncTheWordDict(Sender.Strong.Strings[0]);
  {$ENDIF}
end;

procedure TFrmPrincipal.QuandoAlterarVersiculo;
begin
  StatusBar1.SimpleText := 'modificado';
end;

procedure TFrmPrincipal.AtualizarMRU(m: TMenuItem);
var
  f: TStringList;
  j, k: Integer;
  found: Boolean;
begin
  f := TStringList.Create;
  f.Sorted:=true;

  try
    f.Add(ProjetoAtual.Caminho);

    for j:=0 to m.Count-1 do
    begin
      // find stopped working on unsorted lists. Bug or implementation change...?
      found := false;
      for k:=0 to f.Count-1 do
        if f.Strings[k] = m.Items[j].Caption then
        begin
          found := true;
          break;
        end;
      if not found then
        f.Add(m.Items[j].Caption);
    end;

    m.Clear;

    for j:=0 to f.Count-1 do
    begin
      m.Add(TMenuItem.Create(m));
      m.Items[m.Count-1].Caption := f.Strings[j];
      m.Items[m.Count-1].OnClick := @AbrirRecenteClick;
    end;
  finally
    f.Free;
  end;
end;

procedure TFrmPrincipal.CarregarMRU(m: TMenuItem);
var
  j: Integer;
begin
  for j:=0 to MAX_MRU-1 do
  begin
    if opts.ReadString('projetos', format('recente.%d', [j]), '') <> '' then
    begin
      m.Add(TMenuItem.Create(m));
      m.Items[m.Count-1].Caption := opts.ReadString('projetos', format('recente.%d', [j]), '');
      m.Items[m.Count-1].OnClick := @AbrirRecenteClick;
    end;
  end;
end;

procedure TFrmPrincipal.DescarregarMRU(m: TMenuItem);
var
  j: Integer;
begin
  for j:=0 to m.Count-1 do
    opts.WriteString('projetos', format('recente.%d', [j]), m.Items[j].Caption);

  for j:=m.Count to MAX_MRU-1 do
    opts.DeleteKey('projetos', format('recente.%d', [j]));
end;

procedure TFrmPrincipal.SyncToTwRef(Ref: string);
begin
  if (ProjetoAtual <> nil) then
    ProjetoAtual.IrPara(Ref);
end;

procedure TFrmPrincipal.SetUpSyncThread;
begin
  if synciBiblia2Tw then
  begin
    if TwSyncThread = nil then
    begin
      TwSyncThread := TTwSyncThread.Create(true);
      TwSyncThread.OnRefChange := @SyncToTwRef;
      TwSyncThread.Start;
    end;
  end else
  begin
    if TwSyncThread <> nil then
    begin
      TwSyncThread.Terminate;
      TwSyncThread := nil;
    end;
  end;
end;

end.

