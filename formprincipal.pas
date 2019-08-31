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
  ActnList, ComCtrls, ExtCtrls, StdCtrls, Projeto, IniFiles, Math,
  Sintagma, LCLTranslator, unitabout, PCRE, Versiculo,
  LCLType, LazUTF8, KChapterView;

type

  { TFrmPrincipal }

  TFrmPrincipal = class(TForm)
    ActionMostrarTags: TAction;
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
    CommentsMemo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItemStrongsCountNone: TMenuItem;
    MenuItemStrongsCountWords: TMenuItem;
    MenuItemStrongsCountStrongs: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemMouseHover: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItemDictPopup: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItemAltMouseHover: TMenuItem;
    MenuItemCtrlMouseHover: TMenuItem;
    MenuItemAlwaysOnTop: TMenuItem;
    MenuItemLangEn: TMenuItem;
    MenuItemLangPt: TMenuItem;
    MenuItemStrongsCount: TMenuItem;
    MenuItemStrongNegrito: TMenuItem;
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
    CenterPanel: TPanel;
    BottomPanel: TPanel;
    ContextPanel: TPanel;
    ProgressBar1: TProgressBar;
    RadioGroupStatus: TRadioGroup;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    ScrollBoxSrcVerse: TScrollBox;
    ScrollBoxDstVerse: TScrollBox;
    ScrollBoxRef1Verse: TScrollBox;
    ScrollBoxRef2Verse: TScrollBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    Splitter6: TSplitter;
    StatusBar1: TStatusBar;
    ChapterViewTabCtrl: TTabControl;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButtonExit: TToolButton;
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
    procedure ActionMostrarTagsExecute(Sender: TObject);
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure AbrirRecenteClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemAlwaysOnTopClick(Sender: TObject);
    procedure MenuItemLangEnClick(Sender: TObject);
    procedure MenuItemLangPtClick(Sender: TObject);
    procedure MenuItemPopupTriggerClick(Sender: TObject);
    procedure MenuItemStrongNegritoClick(Sender: TObject);
    procedure MenuItemStrongsCountClick(Sender: TObject);
    procedure MenuItemSynciBibliaClick(Sender: TObject);
    procedure MenuItemSyncTheWordClick(Sender: TObject);
    procedure QuandoNovoVersiculo(Sender: TProjeto);
    procedure QuandoPalavraClicada(Sender: TSintagma);
    procedure QuandoAlterarVersiculo;
    procedure AtualizarMRU(m: TMenuItem);
    procedure CarregarMRU(m: TMenuItem);
    procedure DescarregarMRU(m: TMenuItem);
    procedure RadioGroupStatusSelectionChanged(Sender: TObject);
    procedure ChapterViewTabCtrlChange(Sender: TObject);
    procedure ToolButtonExitClick(Sender: TObject);
  private
    { private declarations }
    FPopupTrigger: TPopupTrigger;
    FCurrentRef: string;
    FChapterView: TKChapterView;
    {$IFDEF WINDOWS}
    FRxMorpho: IRegex;
    syncTw2iBiblia: boolean;
    synciBiblia2Tw: boolean;
    TwSyncThread: TTwSyncThread;
    procedure SyncToTwRef(Ref: string);
    procedure SetUpSyncThread;
    {$ENDIF}
    procedure Translate;
  public
    { public declarations }
    language: string;
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
  FStrongsCountMode: TStrongsCountMode;

const
  MAX_MRU = 10;

resourcestring
  SStatusNotAssociated = 'Not associated';
  SStatusAssociating = 'Associating';
  SStatusNeedsReview = 'Needs review';
  SStatusAssociated = 'Associated!';
  SRollbackChanges = 'Rollback changes';
  SRollbackChangesConfirmation = 'Are you sure you want to rollback all changes to this verse?';
  SSourceText = 'Source';
  SDestinationText = 'Destination';
  SReference1Text = 'Reference1';
  SReference2Text = 'Reference2';

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

resourcestring
  SOpenProject = 'Open project';
  SProjectFileDoesNotExist = 'The selected project file doesn''t exist:'#13#10'%s';
  SCloseProject = 'Close project';
  SSaveChanges = 'Would you like to save your changes?';
  SSource = 'source';
  SDestination = 'destination';
  SReference1 = 'reference 1';
  SReference2 = 'reference 2';
  SNewAssociationProject = 'New association project';
  SLoadingFile = 'Loading %s...';
  SChanged = 'changed';

{ TFrmPrincipal }

procedure TFrmPrincipal.ActionAbrirProjetoExecute(Sender: TObject);
begin
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
    MessageDlg(SOpenProject, format(SProjectFileDoesNotExist, [OpenDialog1.FileName]), mtError, [mbOK], 0);
    exit;
  end;

  ProjetoAtual := TProjeto.Criar([ScrollBoxSrcVerse, ScrollBoxDstVerse, ScrollBoxRef1Verse, ScrollBoxRef2Verse], TreeView1, RadioGroupStatus, CommentsMemo);
  ProjetoAtual.OnNewVerseSubscribe(@QuandoNovoVersiculo);
  ProjetoAtual.OnAlterarVersiculo := @QuandoAlterarVersiculo;
  ProjetoAtual.OnSintagmaClick := @QuandoPalavraClicada;
  ProjetoAtual.PalavrasComStrongEmNegrito := MenuItemStrongNegrito.Checked;
  ProjetoAtual.MostrarQtdStrongs := FStrongsCountMode;

  FChapterView.Enabled := true;
  FChapterView.Project := ProjetoAtual;

  ProjetoAtual.Abrir(OpenDialog1.FileName);
  ProjetoAtual.ExibirDefinicoesSoComCtrl := MenuItemDictPopup.Checked;
  ProjetoAtual.SugerirAssociacaoAutomaticamente := MenuItem22.Checked;
  ProjetoAtual.PopupTrigger := FPopupTrigger;

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

  {$IFDEF WINDOWS}
  SetUpSyncThread;
  {$ENDIF}
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
  if ProjetoAtual = nil then
    exit;

  salvar := false;
  if ProjetoAtual.Modificado then
  begin
    case MessageDlg(SCloseProject, SSaveChanges, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
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

  FCurrentRef := '';

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
  FChapterView.Enabled := false;
  FChapterView.Clear;
  //ActionExportarDestinoComStrongs.Enabled := false;
  //ActionExportarTextoInterlinear.Enabled := false;
  StatusBar1.SimpleText := '';
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

procedure TFrmPrincipal.ActionMostrarTagsExecute(Sender: TObject);
begin
  if assigned(ProjetoAtual) then
    ProjetoAtual.ToggleDisplayTags;
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
    ProjetoAtual.ImportarModuloTheWord(edit.Text, p.texto, p.pb, false);

  result := 0;
end;

procedure TFrmPrincipal.ActionNovoProjetoExecute(Sender: TObject);
  function QualTexto(v: TTipoTextoBiblico): string;
  begin
    case v of
      tbOrigem: result := SSource;
      tbDestino: result := SDestination;
      tbConsulta1: result := SReference1;
      tbConsulta2: result := SReference2;
    end;
  end;

  function QualEscopo: TEscopoTexto;
  begin
    case FormNovoProjeto1.RadioGroupScope.ItemIndex of
      0: result := etOT;
      1: result := etNT;
      2: result := etONT;
    end;
  end;

var
  v: TTipoTextoBiblico;
  ParThread: TParametroThread;
begin
  ActionFecharProjetoExecute(nil);

  if ProjetoAtual <> nil then
    exit;

  FormNovoProjeto1.EditNomeProjeto.Text := SNewAssociationProject;

  if (FormNovoProjeto1.ShowModal = mrOK) and SaveDialog1.Execute then
  begin
    ProjetoAtual := TProjeto.Criar([ScrollBoxSrcVerse, ScrollBoxDstVerse, ScrollBoxRef1Verse, ScrollBoxRef2Verse], TreeView1, RadioGroupStatus, CommentsMemo);
    ProjetoAtual.Escopo := QualEscopo;
    ProjetoAtual.PalavrasComStrongEmNegrito := MenuItemStrongNegrito.Checked;
    ProjetoAtual.MostrarQtdStrongs := FStrongsCountMode;
    ProjetoAtual.OnNewVerseSubscribe(@QuandoNovoVersiculo);
    ProjetoAtual.OnAlterarVersiculo := @QuandoAlterarVersiculo;
    ProjetoAtual.Novo(SaveDialog1.FileName, FormNovoProjeto1.EditNomeProjeto.Text);
    ProjetoAtual.ExibirDefinicoesSoComCtrl := MenuItemDictPopup.Checked;
    ProjetoAtual.SugerirAssociacaoAutomaticamente := MenuItem22.Checked;
    ProjetoAtual.PopupTrigger := FPopupTrigger;

    //ParThread.pb := ProgressBar1;
    for v:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
    begin
      StatusBar1.Caption := Format(SLoadingFile, [QualTexto(v)]);
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
    FChapterView.Enabled := true;
    FChapterView.Project := ProjetoAtual;

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
    Caption := format('iBiblia | %s | %s (%s)', [ProjetoAtual.FormattedReference, ProjetoAtual.ObterInfo('descricao'), ProjetoAtual.FileName]);
    RadioGroupStatus.SetFocus;
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
  if ProjetoAtual = nil then
    exit;
  if MessageDlg(SRollbackChanges, SRollbackChangesConfirmation, mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
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

procedure TFrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ActionFecharProjetoExecute(Sender);
  if ProjetoAtual <> nil then
    CloseAction := caNone;
end;

procedure TFrmPrincipal.FormCreate(Sender: TObject);
var
  m: integer;
begin
  ProjetoAtual := nil;
  opts := TIniFile.Create('iBiblia.ini');

  Width  := opts.ReadInteger('leiaute', 'principal.largura',  Width);
  Height := opts.ReadInteger('leiaute', 'principal.altura',   Height);
  MenuItem22.Checked := opts.ReadBool('opcoes', 'sugestoes.automaticas', false);
  MenuItemSyncTheWord.Checked := opts.ReadBool('opcoes', 'synctheword', false);
  MenuItemSynciBiblia.Checked := opts.ReadBool('opcoes', 'syncibiblia', false);
  MenuItemStrongNegrito.Checked := opts.ReadBool('opcoes', 'boldstrongs', false);

  MenuItemStrongsCountNone.Tag    := Integer(scNone);
  MenuItemStrongsCountWords.Tag   := Integer(scCountWords);
  MenuItemStrongsCountStrongs.Tag := Integer(scCountStrongs);
  FStrongsCountMode := TStrongsCountMode(opts.ReadInteger('opcoes', 'strongscountmode', Integer(scNone)));
  for m:=0 to MenuItemStrongsCount.Count-1 do
    if MenuItemStrongsCount.Items[m].Tag = Integer(FStrongsCountMode) then
      MenuItemStrongsCount.Items[m].Checked := true;

  FPopupTrigger := TPopupTrigger(opts.ReadInteger('opcoes', 'popuptrigger', LongInt(ptMouseHover)));
  MenuItemPopupTriggerClick(MenuItemDictPopup.Items[integer(FPopupTrigger)]);

  MenuItemAlwaysOnTop.Checked := opts.ReadBool('opcoes', 'alwaysontop', false);
  if MenuItemAlwaysOnTop.Checked then
    FormStyle := fsSystemStayOnTop;

  language := opts.ReadString('opcoes', 'language', 'pt');
  SetDefaultLang(language);
  if language = 'en' then
    MenuItemLangEn.Checked:=true
  else if language = 'pt' then
    MenuItemLangPt.Checked:=true;
  Translate;

  ChapterViewTabCtrl.TabIndex := opts.ReadInteger('opcoes', 'chapter.view.text', ord(tbDestino));
  FChapterView := TKChapterView.Create(ChapterViewTabCtrl);
  with FChapterView do
  begin
    ParentWindow := ChapterViewTabCtrl.Handle;
    BorderStyle  := bsNone;
    Align        := alClient;
    FontName     := opts.ReadString('leiaute', 'principal.chapterview.font.name', DefFontData.Name);
    FontSize     := opts.ReadInteger('leiaute', 'principal.chapterview.font.size', 0);
    VerseMode    := TViewMode(opts.ReadInteger('opcoes', 'chapterview.versemode', Ord(vmParagraph)));
    BibleText    := TTipoTextoBiblico(ChapterViewTabCtrl.TabIndex);
  end;

  CarregarMRU(MenuItemRecent);
  TreeView1.Width := opts.ReadInteger('leiaute', 'principal.treeview.largura', TreeView1.Width);

  {$IFDEF WINDOWS}
  FRxMorpho := RegexCreate('^(\S+)( l="(.*?)")?$', [rcoUTF8]);
  syncTw2iBiblia := MenuItemSyncTheWord.Checked;
  synciBiblia2Tw := MenuItemSynciBiblia.Checked;
  SetUpSyncThread;
  {$ENDIF}
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
  opts.WriteInteger('leiaute', 'principal.panel3.height', BottomPanel.Height);
  opts.WriteInteger('leiaute', 'principal.contextpanel.width', ContextPanel.Width);
  opts.WriteInteger('leiaute', 'principal.chapterview.font.size', FChapterView.FontSize);
  opts.WriteString('leiaute', 'principal.chapterview.font.name', FChapterView.FontName);
  opts.WriteInteger('opcoes', 'chapterview.versemode', Ord(FChapterView.VerseMode));
  opts.WriteBool('opcoes', 'sugestoes.automaticas', MenuItem22.Checked);
  opts.WriteBool('opcoes', 'synctheword', MenuItemSyncTheWord.Checked);
  opts.WriteBool('opcoes', 'syncibiblia', MenuItemSynciBiblia.Checked);
  opts.WriteBool('opcoes', 'boldstrongs', MenuItemStrongNegrito.Checked);
  opts.WriteInteger('opcoes', 'strongscountmode', Integer(FStrongsCountMode));
  opts.WriteString('opcoes', 'language', language);
  opts.WriteBool('opcoes', 'alwaysontop', MenuItemAlwaysOnTop.Checked);
  opts.WriteInteger('opcoes', 'popuptrigger', LongInt(FPopupTrigger));
  opts.WriteInteger('opcoes', 'chapter.view.text', ChapterViewTabCtrl.TabIndex);
  opts.Free;

  FChapterView.Free;
end;

procedure TFrmPrincipal.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    '1', '2', '3', '4':
      if assigned(ProjetoAtual) then
        RadioGroupStatus.ItemIndex := Ord(Key) - Ord('0') - 1;
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
  BottomPanel.Height := opts.ReadInteger('leiaute', 'principal.panel3.height', BottomPanel.Height);
  ContextPanel.Width := opts.ReadInteger('leiaute', 'principal.contextpanel.width', ContextPanel.Width);

  if (MenuItemRecent.Count > 0) and FileExists(MenuItemRecent.Items[0].Caption) then
    ActionAbrirProjetoExecute(MenuItemRecent.Items[0]);
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

procedure TFrmPrincipal.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.ShowModal;
end;

procedure TFrmPrincipal.MenuItemAlwaysOnTopClick(Sender: TObject);
var
  m: TMenuItem;
begin
  m := TMenuItem(Sender);
  m.Checked := not m.Checked;
  if m.Checked then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TFrmPrincipal.MenuItemLangEnClick(Sender: TObject);
begin
  if language = 'en' then
    exit;

  language := 'en';
  SetDefaultLang('en');
  TMenuItem(Sender).Checked:=true;
  MenuItemLangPt.Checked:=false;
  Translate;
end;

procedure TFrmPrincipal.MenuItemLangPtClick(Sender: TObject);
begin
  if language = 'pt' then
    exit;

  language := 'pt';
  SetDefaultLang('pt');
  TMenuItem(Sender).Checked:=true;
  MenuItemLangEn.Checked:=false;
  Translate;
end;

procedure TFrmPrincipal.MenuItemPopupTriggerClick(Sender: TObject);
var
  m: TPopupTrigger;
begin
  for m:=low(TPopupTrigger) to high(TPopupTrigger) do
  begin
    if Sender = MenuItemDictPopup.Items[integer(m)] then
    begin
      FPopupTrigger := m;
      TMenuItem(Sender).Checked := true;
      if assigned(ProjetoAtual) then
        ProjetoAtual.PopupTrigger := FPopupTrigger;
    end else
      MenuItemDictPopup.Items[integer(m)].Checked := false;
  end;
end;

procedure TFrmPrincipal.MenuItemStrongNegritoClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if assigned(ProjetoAtual) then
    ProjetoAtual.PalavrasComStrongEmNegrito := TMenuItem(Sender).Checked;
end;

procedure TFrmPrincipal.MenuItemStrongsCountClick(Sender: TObject);
var
  i: integer;
  m: TMenuItem;
begin
  for i:=0 to MenuItemStrongsCount.Count-1 do
  begin
    m := MenuItemStrongsCount.Items[i];
    if m = Sender then
    begin
      m.Checked := true;
      FStrongsCountMode := TStrongsCountMode(m.Tag);
      if assigned(ProjetoAtual) then
        ProjetoAtual.MostrarQtdStrongs := TStrongsCountMode(m.Tag);
    end else
      m.Checked := false;
  end;
end;

procedure TFrmPrincipal.MenuItemSynciBibliaClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  synciBiblia2Tw := TMenuItem(Sender).Checked;
  SetUpSyncThread;
  {$ENDIF}
end;

procedure TFrmPrincipal.MenuItemSyncTheWordClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  syncTw2iBiblia := TMenuItem(Sender).Checked;
  {$ENDIF}
end;

procedure TFrmPrincipal.QuandoNovoVersiculo(Sender: TProjeto);
begin
  ActionQuandoNovoVersiculoExecute(nil);
end;

procedure TFrmPrincipal.QuandoPalavraClicada(Sender: TSintagma);
{$IFDEF WINDOWS}
var
  match: IMatch;
  wordSyncd: boolean;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  if not syncTw2iBiblia then
    exit;

  if Sender.TemStrongs then
    SyncTheWordDict(Sender.Strong[0]); // strong#

  wordSyncd := false;
  if Sender.TemMorfs then
  begin
    match := FRxMorpho.Match(Sender.Morf[0]);
    if match.Success then
    begin
      SyncTheWordDict(match.Groups[1].Value); // morphology
      if not match.Groups[3].Value.IsEmpty then
      begin
        SyncTheWordDict(match.Groups[3].Value); // lemma
        wordSyncd := true;
      end;
    end;
  end;

  if not wordSyncd then
    SyncTheWordDict(Sender.Texto); // raw word
  {$ENDIF}
end;

procedure TFrmPrincipal.QuandoAlterarVersiculo;
begin
  StatusBar1.SimpleText := SChanged;
end;

procedure TFrmPrincipal.AtualizarMRU(m: TMenuItem);
var
  f: TStringList;
  j, k: Integer;
  found: Boolean;
begin
  f := TStringList.Create;

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

procedure TFrmPrincipal.RadioGroupStatusSelectionChanged(Sender: TObject);
begin
  if ProjetoAtual <> nil then
    ProjetoAtual.Situacao := RadioGroupStatus.ItemIndex;
end;

procedure TFrmPrincipal.ChapterViewTabCtrlChange(Sender: TObject);
begin
  FChapterView.BibleText := TTipoTextoBiblico(TTabControl(Sender).TabIndex);
end;

procedure TFrmPrincipal.ToolButtonExitClick(Sender: TObject);
begin
  Close;
end;

{$IFDEF WINDOWS}
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
{$ENDIF}

procedure TFrmPrincipal.Translate;
begin
  with RadioGroupStatus.Items do
  begin
    Clear;
    Add(SStatusNotAssociated);
    Add(SStatusAssociating);
    Add(SStatusNeedsReview);
    Add(SStatusAssociated);
  end;
  with ChapterViewTabCtrl do
  begin
    Tabs.Clear;
    Tabs.Add(SSourceText);
    Tabs.Add(SDestinationText);
    Tabs.Add(SReference1Text);
    Tabs.Add(SReference2Text);
  end;
  if assigned(FormNovoProjeto1) then
    FormNovoProjeto1.Translate;
  if assigned(FormPropProjeto1) then
    FormPropProjeto1.Translate;
  if assigned(ProjetoAtual) then
    ProjetoAtual.Translate;
end;

end.

