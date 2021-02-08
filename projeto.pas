unit Projeto;

{$mode objfpc}{$H+}{$M+}

{TODO -cDesejável: Remover dependência com formpopup, criá-lo dinamicamente }
{TODO -cDesejável: Usar descendente de THintWindow para exibição de definições }

interface

uses
  Classes,
  {$IFDEF UNIX}
  LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  SysUtils, Sqlite3DS, sqlite3conn, sqldb, db, StrUtils, math,
  ExtCtrls, Controls, ComCtrls, StdCtrls, Graphics, Forms, Versiculo, Sugestao,
  MemoVersiculo, ONTTokenizer, Dialogs, dos, PCRE, ExportarProjeto, LazLogger,
  Syntagm, MySwordModule, TheWordDictionary, fgl, PatchFile, LCLIntf;

type

  TProjeto = class;

  TEscopoTexto = (etOT, etNT, etONT, etNone);

  TReference = record
    Book: string;
    BookID: integer;
    Chapter: integer;
    Verse: integer;
  end;

  { TTipoTextoBiblico }

  TTipoTextoBiblico =
  (
    tbOrigem,
    tbDestino,
    tbConsulta1,
    tbConsulta2,
    tbInterlinear, // interlinear version of the destination text
    tbNone
  );

  TParagraphMode = (pmParagraph, pmNoParagraphs);
  TInterlinearMode = (imIntralinear, imInterlinear);

  TRegexList = specialize TFPGList<IRegex>;
  TIntegerList = specialize TFPGList<integer>;
  TPopupTrigger = (ptMouseHover, ptAltMouseHover, ptCtrlMouseHover);

  TOpcaoExportacao =
  (
    oeExportarMorfologia,      // incluir morfologia
    oeExportarNAComoItalicos,  // palavras não associadas como itálicos
    oeExportarComentarios,     // comentários como notas de rodapé
    oeConcordDetalhada,        // concordância aberta por declinação
    oeStrongsReutilizados,     // sinalizar Strongs reutilizados
    oeStrongsNaoTraduzidos     // sinalizar Strongs não traduzidos
  );

  TOpcoesExportacao = set of TOpcaoExportacao;

  TOnNovoVersiculoEvent = procedure (Sender: TProjeto) of object;
  TOnNovoVersiculoEvents = specialize TFPGList<TOnNovoVersiculoEvent>;
  TOnSintagmaClickEvent = procedure (Sender: TSyntagm) of object;
  //TOnAlterarVersiculoEvent = procedure (Sender: TProjeto) of object;

  AVersiculo = array[tbOrigem..tbConsulta2] of TVersiculo;
  ATmpVerse = array[tbOrigem..tbDestino] of TVersiculo;
  ACamposTexto = array[tbOrigem..tbConsulta2] of integer;
  ADicStrong = array[tbOrigem..tbConsulta2] of TSQLQuery;
  ADicMorfo = array[tbOrigem..tbConsulta2] of TSQLQuery;
  STextosBiblicos = set of TTipoTextoBiblico;

  { TProjeto }

  TProjeto = class
  private
    FReference: TReference;
    FFileName: string;
    FMemoVersiculo: TMemoVersiculo;
    FAtivo: boolean;
    FMarker: string;
    FScrollEventsEnabled: boolean;
    FExportando: boolean;
    FOnAlterarVersiculo: TOnAlterarVersiculoEvent;
    FDisplayTags: boolean;
    FSugerirAssociacaoAuto: boolean;
    FPalavrasComStrongEmNegrito: boolean;
    FExibirDefComCtrl: boolean;
    FTblPares: TSqlite3Dataset;
    FTblInfo: TSqlite3Dataset;
    FAVersiculo: AVersiculo;
    FATmpVerse: ATmpVerse;
    FACamposTexto: ACamposTexto;
    FADicStrong: ADicStrong;
    FADicMorfo: ADicMorfo;
    FSugeridor: TGerSugestoes;
    FParesAntigos: TStringList;
    FArvore: TTreeView;
    FMemoComentarios: TMemo;
    FRadioGroupSituacao: TRadioGroup;
    FTemporizador: TTimer;
    FAtrasoExibicao: Cardinal;
    FEscopo: TEscopoTexto;
    FPopupTrigger: TPopupTrigger;
    FVerseStrongsCountMode: TStrongsCountMode;
    FOnNovoVersiculo: TOnNovoVersiculoEvents;
    FOnSintagmaClick: TOnSintagmaClickEvent;
    FClosing: boolean;
    FAutoSave: boolean;
    function GetCaminho: string;
    function GetChapterViewText: TTipoTextoBiblico;
    function GetComentarios: string;
    function GetID: string;
    function GetInterlinearMode: TInterlinearMode;
    function GetModificado: boolean;
    function GetFormattedReference: string;
    function GetPairs: string;
    function GetParagraphMode: TParagraphMode;
    function GetSituacao: Integer;
    procedure PreencherArvore;
    procedure SetAtrasoExibicao(const AValue: Cardinal);
    procedure SetAutoSave(AValue: boolean);
    procedure SetChapterViewText(AValue: TTipoTextoBiblico);
    procedure SetComentarios(const AValue: string);
    procedure SetDisplayTags(AValue: boolean);
    procedure SetInterlinearMode(AValue: TInterlinearMode);
    procedure SetParagraphMode(AValue: TParagraphMode);
    procedure SetVerseStrongsCountMode(AValue: TStrongsCountMode);
    procedure SetOnAlterarVersiculo(const AValue: TOnAlterarVersiculoEvent);
    procedure SetOnSintagmaClick(const AValue: TOnSintagmaClickEvent);
    procedure SetPalavrasComStrongEmNegrito(AValue: boolean);
    procedure SetSituacao(const AValue: Integer);
    procedure ExportTheWordBible(verses: TStringList; filename: string; props: string);
    procedure ExportMySwordBible(verses: TStringList; filename: string; props: string);
    procedure HighlightStrongs(syntagm: TSyntagm);
  protected
    procedure CopiarArquivo(origem, destino: string);
    function CriarObjetoTabela(db, tabela, chave: string): TSqlite3Dataset;
    function CriarObjetoQuery(db: string): TSQLQuery;
    function InserirInfo(info, valor: string): boolean;
    function AtualizarInfo(info, valor: string): boolean;
    function AtualizarConfig(table: TSqlite3Dataset; name, value: string): boolean;
    function ResgatarInfo(info: string): string;
    function ObterDefinicaoStrong(strong: string; texto: TTipoTextoBiblico): string;
    function ObterDefinicaoMorfo(morfo: string; texto: TTipoTextoBiblico): string;
    function FormatRTF(rtf: string): string;
    procedure SetVerseText(versiculo: string; texto: TTipoTextoBiblico; replace: boolean);
    procedure PreRolagemVersiculo(DataSet: TDataSet);
    procedure PosRolagemVersiculo(DataSet: TDataSet);
    procedure SalvarPares;
    procedure SalvarTexto;
    procedure SalvarAssociacoes;
    procedure RemoverAssociacoes;
    procedure SintagmaOnMouseEnter(Sender: TSyntagm);
    procedure SintagmaOnMouseLeave(Sender: TSyntagm);
    procedure SintagmaOnClick(Sender: TSyntagm);
    procedure OnExportText(Sender: TVersiculo);
    procedure OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
    procedure OnAlterarTextoVersiculo(Sender: TMemoVersiculo);
    procedure OnExibirDefinicao(Sender: TObject);
    procedure OnDblClickVersiculo(Sender: TObject);
    procedure AtribuirDicStrong(dic: string; t: TTipoTextoBiblico);
    procedure AtribuirDicMorfo(dic: string; t: TTipoTextoBiblico);
    procedure AtualizarArvore;
    procedure AtualizarArvore(id: string);
    procedure SelectTreeNode(id: string);
  public
    constructor Criar;
    constructor Criar(paineis: array of TScrollbox; navegador: TTreeView; rsituacao: TRadioGroup; rcomentarios: TMemo);
    destructor Destruir;
    procedure Novo(nomedb, descricao: string);
    procedure Novo(nomedbvelho, nomedbnovo, descricao: string);
    procedure Abrir(Nome: string);
    procedure Fechar(_Commit: boolean);
    procedure Commit;
    procedure Rollback;
    procedure IrPara(Referencia: string);
    procedure VersiculoSeguinte;
    procedure VersiculoAnterior;
    procedure VersiculoInicial;
    procedure VersiculoFinal;
    procedure Atualizar;
    procedure Limpar;
    procedure Salvar;
    procedure SugerirAssociacao;
    procedure RecriarBaseSugestoes;
    procedure RecriarBaseSugestoes(pb: TProgressBar);
    function ImportarModuloTheWord(arquivo: string; texto: TTipoTextoBiblico; replace: boolean): boolean;
    function ImportarModuloTheWord(arquivo: string; texto: TTipoTextoBiblico; pb: TProgressBar; replace: boolean): boolean;
    procedure NovoObjetoVersiculo(owner: TScrollBox; texto: TTipoTextoBiblico);
    procedure ExportarTextoDestinoComStrongs(arquivo: string; opcoes: TOpcoesExportacao);
    procedure ExportarTextoDestinoComStrongs(arquivo: string; pb: TProgressBar; opcoes: TOpcoesExportacao);
    procedure ExportarTextoInterlinear(arquivo: string; opcoes: TOpcoesExportacao);
    procedure ExportarTextoInterlinear(arquivo: string; pb: TProgressBar; opcoes: TOpcoesExportacao);
    procedure ExportarConcordancia(arquivo: string; opcoes: TOpcoesExportacao; abbreviation: string);
    procedure ExportarConcordancia(arquivo: string; pb: TProgressBar; opcoes: TOpcoesExportacao; abbreviation: string);
    procedure AtribuirFonteTexto(fonte: TFont; textos: STextosBiblicos);
    procedure LimparTexto(texto: TTipoTextoBiblico);
    function ObterFonteTexto(texto: TTipoTextoBiblico): TFont;
    procedure AtribuirDicStrong(dic: string; textos: STextosBiblicos);
    procedure AtribuirDicMorfo(dic: string; textos: STextosBiblicos);
    procedure AtribuirInfo(info, valor: string);
    function ObterInfo(info: string): string;
    function ObterTextoVersiculo(texto: TTipoTextoBiblico): string;
    function ObterTextoVersiculo(Referencia: string; texto: TTipoTextoBiblico): string;
    function ObterTextoSimplesVersiculo(texto: TTipoTextoBiblico): string;
    function ObterTextoSimplesVersiculo(Referencia: string; texto: TTipoTextoBiblico): string;
    function GetTranslationSuggestions(syntagm: TSyntagm): string;
    function GetChapterText: TStringList;
    procedure CompileInterlinearVerseRules(var rulesRx: TRegexList; var replaceTo: TStringList; mode: TInterlinearMode; exporting: boolean = false);
    procedure Translate;
    procedure ToggleDisplayTags;
    procedure StartScrollingSession;
    procedure FinishScrollingSession;
    procedure RewindChapter;
    procedure OnNewVerseSubscribe(const AValue: TOnNovoVersiculoEvent);
    procedure OnNewVerseUnsubscribe(const AValue: TOnNovoVersiculoEvent);
    procedure ApplyPatch(patch: TPatchFile; indexes: TIntegerList);
    function GetPairs(ref: string): string;
    function GetComments(ref: string): string;
    procedure SetTextDescription(text: TTipoTextoBiblico; description: string);
    function GetTextDescription(text: TTipoTextoBiblico): string;
    procedure Vacuum;

    property FileName: string read FFileName;
    property FormattedReference: string read GetFormattedReference;
    property Reference: TReference read FReference;
    property ID: string read GetID;
    property Modificado: boolean read GetModificado;
    property Arvore: TTreeView read FArvore write FArvore;
    property OnAlterarVersiculo: TOnAlterarVersiculoEvent read FOnAlterarVersiculo write SetOnAlterarVersiculo;
    property OnSintagmaClick: TOnSintagmaClickEvent read FOnSintagmaClick write SetOnSintagmaClick;
    property AtrasoExibicaoDefinicao: Cardinal read FAtrasoExibicao write SetAtrasoExibicao;
    property Ativo: boolean read FAtivo;
    property Caminho: string read GetCaminho;
    property Situacao: Integer read GetSituacao write SetSituacao;
    property Comentarios: string read GetComentarios write SetComentarios;
    property MemoComentarios: TMemo read FMemoComentarios write FMemoComentarios;
    property RadioGroupSituacao: TRadioGroup read FRadioGroupSituacao write FRadioGroupSituacao;
    property ExibirDefinicoesSoComCtrl: boolean read FExibirDefComCtrl write FExibirDefComCtrl;
    property SugerirAssociacaoAutomaticamente: boolean read FSugerirAssociacaoAuto write FSugerirAssociacaoAuto;
    property PalavrasComStrongEmNegrito: boolean read FPalavrasComStrongEmNegrito write SetPalavrasComStrongEmNegrito;
    property Escopo: TEscopoTexto read FEscopo write FEscopo;
    property MostrarQtdStrongs: TStrongsCountMode read FVerseStrongsCountMode write SetVerseStrongsCountMode;
    property PopupTrigger: TPopupTrigger read FPopupTrigger write FPopupTrigger;
    property DisplayTags: boolean read FDisplayTags write SetDisplayTags;
    property BookID: integer read FReference.BookID;
    property Book: string read FReference.Book;
    property Chapter: integer read FReference.Chapter;
    property Verse: integer read FReference.Verse;
    property ScrollEventsEnabled: boolean read FScrollEventsEnabled write FScrollEventsEnabled;
    property ChapterViewText: TTipoTextoBiblico read GetChapterViewText write SetChapterViewText;
    property InterlinearMode: TInterlinearMode read GetInterlinearMode write SetInterlinearMode;
    property ParagraphMode: TParagraphMode read GetParagraphMode write SetParagraphMode;
    property AutoSave: boolean read FAutoSave write SetAutoSave;
    property Pairs: string read GetPairs;
  end;

{$INCLUDE include/bookdefs.inc}

resourcestring

  SAnalyticalConcordance = 'Anaytical Concordance';
  SSyntheticConcordance = 'Synthetic Concordance';
  SOTConcordanceNotImplementedYet = 'Old Testament concordance is not implemented yet';
  SLanguageId = 'en';
  SError = 'Error';
  SWarning = 'Warning';
  SInformation = 'Information';
  SCorruptedData = 'Corrupted data, some associations may be lost.' +
                   'This can happen for several reasons:'#13#10 +
                   ' 1. The project file was modified outside iBiblia'#13#10 +
                   ' 2. The project was created/edited on an incompatible iBiblia version'#13#10 +
                   ' 3. This can be a bug in iBiblia.'#13#10 +
                   '    If this is the case, please file a bug in the Github project page'#13#10 +
                   '    at https://github.com/rubiot/ibiblia.';
  SStrongDictionary = 'Strong''s Dictionary';
  SDictionaryDoesntExist = 'The selected dictionary doesn''t exist: %s';
  SMorphologyDictionary = 'Morphology Dictionary';
  SOpenProject = 'Open project';
  SProjectDoesntExist = 'The selected project doesn''t exist: %s';
  SInvalidLineCount = 'Invalid file, it must have at least %d lines';
  SPotentialNonUTF8ModuleWarning = 'This module has a ''charset'' property and this may be an indication that this is a non-UTF8 module. ' +
                                   'iBiblia was not designed to work with non-UTF8 modules. ' +
                                   'Please choose ''Ignore'' to ignore this warning and continue at your own risk or ''Abort'' ' +
                                   'to abort the operation and choose another module.';
  SFailedToCreateFile = 'Failed to create file: %s';
  SExportToFileError = 'iBiblia found an error while exporting the Bible module file.'#13#10 +
                       'Please check if theWord is using the file, then close it and try again.';
  STranslationMemory = 'Translation memory';
  SExportTextSaveDlgTitle = 'Please choose a destination file';
  SIBibliaPatchFiles = 'iBiblia patch files';
  SPatchSuccessfullyExported = 'Patch successfully exported';
  SReferenceNotFound = 'Reference not found';
  SReferenceNotFoundWarning = 'The reference %s is not a valid reference in this project and will be ignored.'#13#10'Press Ignore to ignore all subsequent warnings.';
  SVerseAbsentFromProject = '[This project does not contain this verse]';
  SLineBreaksInTextTitle = 'Edit error';
  SLineBreaksInText = 'A verse cannot contain line breaks. Your changes will be discarded.';
  SInvalidInterlinearVerseRule = 'There is something wrong with your interlinear verse rule.'#13#10'[%s]';
  SInterlinearVerseRuleReplaceError = 'There is something wrong with one of your interlinear verse rules.'#13#10'Please review them.';
  SModuleDoesNotIncludeScope = 'This module is incompatible with the scope of the project. Please choose a valid module.';
  SInvalidModuleType = 'Unknown module extension. Please choose a valid theWord Bible module (.ot, .ont or .nt)';
  SConfirmation = 'Confirmation';
  SVaccumProjectConfirmation = 'Vacuuming requires that your project be saved and reopened. Do you want to proceed?';
  SVacuumDone = 'Vacuum done!';

const
  QStrongs: array[etOT..etONT] of smallint = (8674, 5624, 14298);
  ProjetoModelo: array[etOT..etONT] of string = ('projeto-ot.modelo', 'projeto.modelo', 'projeto-ont.modelo');

implementation

uses formpopup, formverserules;

{ TProjeto }

procedure TProjeto.Novo(nomedb, descricao: string);
begin
  Novo(ProjetoModelo[FEscopo], nomedb, descricao);
end;

procedure TProjeto.Novo(nomedbvelho, nomedbnovo, descricao: string);
begin
  CopiarArquivo(nomedbvelho, nomedbnovo);

  Abrir(nomedbnovo);

  AtribuirInfo('descricao', descricao);
  AtribuirInfo('marcador', format('%d,1,1', [OffsetLivros[FEscopo] + 1]));
  FExibirDefComCtrl := false;  //FTblInfo.ExecuteDirect('COMMIT;');
  VersiculoInicial;
  Commit;
end;

function TProjeto.GetFormattedReference: string;
begin
  result := format('%s %d:%d', [Book, Chapter, Verse]);
end;

function TProjeto.GetPairs: string;
begin
  result := FTblPares.FieldByName('pare_pares').AsString;
end;

function TProjeto.GetParagraphMode: TParagraphMode;
var
  t: string;
begin
  t := ObterInfo('paragraph.mode');
  if t.IsEmpty then
    t := Ord(pmParagraph).ToString;
  result := TParagraphMode(t.ToInteger);
end;

function TProjeto.GetPairs(ref: string): string;
begin
  result := '';
  StartScrollingSession;
  try
    IrPara(ref);
    if ID = ref then
      result := GetPairs();
  finally
    FinishScrollingSession;
  end;
end;

function TProjeto.GetSituacao: Integer;
begin
  result := FTblPares.FieldByName('pare_situacao').AsInteger;
end;

procedure TProjeto.PreencherArvore;
var
  l, c, v: smallint;
  nl, nc, nv: TTreeNode;
begin
  if FArvore.Items.Count = 0 then // evitando preencher outra vez a mesma árvore
  begin // preenchendo árvore
    FArvore.BeginUpdate;
    for l:=0 to QLivros[FEscopo]-1 do
    begin
      nl := FArvore.Items.Add(nil, NLivros[FEscopo][l]);
      nl.ImageIndex := 0; nl.SelectedIndex := nl.ImageIndex + 4;
      for c:=0 to QCapitulos[FEscopo][l]-1 do
      begin
        nc := FArvore.Items.AddChild(nl, IntToStr(c+1));
        nc.ImageIndex := 0; nc.SelectedIndex := nc.ImageIndex + 4;
        for v:=0 to QVersiculos[FEscopo][QCapLivros[FEscopo][l] + c - 1]-1 do
        begin
          nv := FArvore.Items.AddChild(nc, IntToStr(v+1));
          nv.ImageIndex := 0; nv.SelectedIndex := nv.ImageIndex + 4;
        end;
      end;
    end;
    FArvore.EndUpdate;
  end;

  FArvore.OnChange := @OnMudancaVersiculo;
end;

procedure TProjeto.SetAtrasoExibicao(const AValue: Cardinal);
begin
  if FAtrasoExibicao = AValue then exit;
  FAtrasoExibicao := AValue;
  FTemporizador.Interval := AValue;
end;

procedure TProjeto.SetAutoSave(AValue: boolean);
begin
  if FAutoSave=AValue then Exit;
  FAutoSave:=AValue;
end;

procedure TProjeto.SetChapterViewText(AValue: TTipoTextoBiblico);
begin
  AtribuirInfo('chapterview.text', Ord(AValue).toString);
end;

procedure TProjeto.SetComentarios(const AValue: string);
begin
  if FTblPares.FieldByName('pare_comentarios').AsString = AValue then
    exit;
  FTblPares.Edit;
  FTblPares.FieldByName('pare_comentarios').AsString := AValue;
  FTblPares.Post;
end;

procedure TProjeto.SetDisplayTags(AValue: boolean);
var
  t: TTipoTextoBiblico;
begin
  if FDisplayTags = AValue then
    Exit;

  FDisplayTags := AValue;

  for t:=tbOrigem to tbConsulta2 do
  begin
    if FDisplayTags then
      FAVersiculo[t].MostrarTags
    else
      FAVersiculo[t].OcultarTags;
  end;
end;

procedure TProjeto.SetInterlinearMode(AValue: TInterlinearMode);
begin
  AtribuirInfo('interlinear.mode', Ord(AValue).toString);
end;

procedure TProjeto.SetParagraphMode(AValue: TParagraphMode);
begin
  AtribuirInfo('paragraph.mode', Ord(AValue).toString);
end;

procedure TProjeto.SetVerseStrongsCountMode(AValue: TStrongsCountMode);
var
  v: TTipoTextoBiblico;
begin
  if FVerseStrongsCountMode = AValue
     then Exit;
  FVerseStrongsCountMode := AValue;
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
      FAVersiculo[v].StrongsCountMode := AValue;
end;

procedure TProjeto.SetOnAlterarVersiculo(const AValue: TOnAlterarVersiculoEvent);
begin
  if FOnAlterarVersiculo = AValue then exit;
  FOnAlterarVersiculo := AValue;

  if assigned(FAVersiculo[tbOrigem]) then
    FAVersiculo[tbOrigem].OnAlterarVersiculo := OnAlterarVersiculo;
end;

procedure TProjeto.OnNewVerseSubscribe(const AValue: TOnNovoVersiculoEvent);
begin
  if FOnNovoVersiculo.IndexOf(AValue) = -1 then
    FOnNovoVersiculo.Add(AValue);
end;

procedure TProjeto.OnNewVerseUnsubscribe(const AValue: TOnNovoVersiculoEvent);
begin
  FOnNovoVersiculo.Remove(AValue);
end;

procedure TProjeto.ApplyPatch(patch: TPatchFile; indexes: TIntegerList);
var
  i: integer;
  ignoreWarnings: boolean;
begin
  ignoreWarnings := false;
  try
    StartScrollingSession;
    for i in indexes do
    begin
      IrPara(patch.Reference[i]);

      if ID <> patch.Reference[i] then
      begin
        if ignoreWarnings then
          continue;
        if MessageDlg(
             SReferenceNotFound,
             Format(SReferenceNotFoundWarning, [patch.Reference[i]]),
             mtWarning,
             [mbOK, mbIgnore],
             0
           ) = mrIgnore then
          ignoreWarnings := true;
        continue;
      end;

      FTblPares.Edit;
      FTblPares.FieldByName('pare_texto_origem' ).AsString := patch.SourceText[i];
      FTblPares.FieldByName('pare_texto_destino').AsString := patch.DestinationText[i];
      FTblPares.FieldByName('pare_pares'        ).AsString := patch.Pairs[i];
      FTblPares.FieldByName('pare_comentarios'  ).AsString := patch.Comments[i];
      FTblPares.FieldByName('pare_situacao'     ).AsString := patch.Status[i];
      FTblPares.Post;

      DebugLn('%s comments: "%s" situacao: %s', [ID, FTblPares.FieldByName('pare_comentarios').AsString, FTblPares.FieldByName('pare_situacao').AsString]);
    end;
  finally
    FinishScrollingSession;
  end;

  Atualizar;
end;

procedure TProjeto.SetOnSintagmaClick(const AValue: TOnSintagmaClickEvent);
begin
  if FOnSintagmaClick = AValue then exit;
  FOnSintagmaClick := AValue;
end;

procedure TProjeto.SetPalavrasComStrongEmNegrito(AValue: boolean);
var
  v: TTipoTextoBiblico;
begin
  if FPalavrasComStrongEmNegrito=AValue then Exit;
  FPalavrasComStrongEmNegrito:=AValue;
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
      FAVersiculo[v].PalavrasComStrongEmNegrito := AValue;
end;

procedure TProjeto.SetSituacao(const AValue: Integer);
begin
  if FClosing or (FTblPares.FieldByName('pare_situacao').AsInteger = AValue) then
    exit;
  FTblPares.Edit;
  FTblPares.FieldByName('pare_situacao').AsInteger := IfThen((AValue < 0) or (AValue > 3), 1, AValue);
  FTblPares.Post;
  AtualizarArvore(ID);
end;

procedure TProjeto.ExportTheWordBible(verses: TStringList; filename: string;
  props: string);
begin
  verses.Add('');
  verses.Add(props);
  verses[0] := #239#187#191 + verses.Strings[0]; // adicionando BOM
  verses.SaveToFile(filename);
end;

procedure TProjeto.ExportMySwordBible(verses: TStringList; filename: string; props: string);
var
  module: TMySwordModule;
  b, c, v, i: smallint;
begin
  module := TMySwordModule.Create(filename, FEscopo in [etOT, etONT], FEscopo in [etNT, etONT], true, props);
  i := 0;
  for b:=0 to QLivros[FEscopo]-1 do
    for c:=0 to QCapitulos[FEscopo][b]-1 do
      for v:=0 to QVersiculos[FEscopo][QCapLivros[FEscopo][b] + c - 1]-1 do
      begin
        module.AddVerse(verses[i], b+OffsetLivros[FEscopo]+1, c+1, v+1);
        Inc(i);
      end;
  module.Commit;
  module.Free;
end;

procedure TProjeto.HighlightStrongs(syntagm: TSyntagm);
var
  v: TTipoTextoBiblico;
  strongs: TStringList;
  strong: string;
begin
  strongs := nil;
  if syntagm.HasStrongs then
    strongs := syntagm.Strong
  else if syntagm.PairsHaveStrongs then
    strongs := syntagm.Pairs[0].Strong;

  if not assigned(strongs) then
    exit;

  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    if not assigned(FAVersiculo[v]) then
      continue;
    for strong in strongs do
      FAVersiculo[v].EnableStrongHighlight(strong);
  end;
end;

function TProjeto.GetModificado: boolean;
var
  pairsPending, infoPending, srcPending: boolean;
begin
  pairsPending := FTblPares.UpdatesPending;
  infoPending  := FTblInfo.UpdatesPending;
  srcPending   := (assigned(FAVersiculo[tbOrigem]) and FAVersiculo[tbOrigem].Modificado);

  result := pairsPending or infoPending or srcPending;
end;

function TProjeto.GetCaminho: string;
begin
  result := '';
  if FTblInfo.Active then
    result := FTblInfo.FileName;
end;

function TProjeto.GetChapterViewText: TTipoTextoBiblico;
var
  t: string;
begin
  t := ObterInfo('chapterview.text');
  if t.IsEmpty then
    t := Ord(tbDestino).ToString;
  result := TTipoTextoBiblico(t.ToInteger);
end;

function TProjeto.GetChapterText: TStringList;
  function GetText(rulesRx: TRegexList; replaceTo: TStringList): string;
  var
    i: integer;
  begin
    case ChapterViewText of
      tbOrigem, tbDestino, tbConsulta1, tbConsulta2:
        result := FTblPares.Fields[FACamposTexto[ChapterViewText]].AsString.Replace(#239#187#191, '');
      tbInterlinear:
      begin
        if length(FTblPares.FieldByName('pare_pares').AsString) = 0 then
          result := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString
        else
        begin
          FATmpVerse[tbOrigem ].Texto := FTblPares.Fields[FACamposTexto[tbOrigem] ].AsString;
          FATmpVerse[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
          try
            FATmpVerse[tbOrigem ].Pares := FTblPares.FieldByName('pare_pares').AsString;
          except
            on E: Exception do
              MessageDlg(SError, SCorruptedData + #13#10#13#10 + FormattedReference + #13#10 +
                   format('%s'#13#10'pares: %s'#13#10'%s'#13#10'%s',
                          [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                          FATmpVerse[tbOrigem].DebugTokens, FATmpVerse[tbDestino].DebugTokens]),
                   mtError, [mbOK], 0);
          end;

          if InterlinearMode = imInterlinear then
            result := FATmpVerse[tbOrigem].GetMySwordInterlinearLine
          else if InterlinearMode = imIntralinear then
            result := FATmpVerse[tbOrigem].GetTheWordInterlinearLine
          else
            raise Exception.Create(Format('Invalid interlinear mode [%d]', [Ord(InterlinearMode)]));

          // apply verse rules
          for i:=0 to rulesRx.Count-1 do
          begin
            try
              result := rulesRx[i].Replace(result, replaceTo[i]);
            except
              on E: Exception do ShowMessage(SInterlinearVerseRuleReplaceError + #13#10 + E.Message);
            end;
          end;

        end;
      end;
    end;
  end;

var
  bkch, comments: string;
  rulesRx: TRegexList;
  replaceTo: TStringList;
  //starttime: DWord;
begin
  //starttime := getTickCount;

  bkch := Format('%d,%d,', [BookID, Chapter]);
  result := TStringList.Create;

  rulesRx := nil;
  replaceTo := nil;
  if ChapterViewText = tbInterlinear then
    CompileInterlinearVerseRules(rulesRx, replaceTo, InterlinearMode);

  StartScrollingSession;
  RewindChapter;
  with FTblPares do
    while not FTblPares.EOF and GetID().StartsWith(bkch) do
    begin
      comments := IfThen(ChapterViewText = tbDestino, Comentarios.Replace(#13#10, '<br/>', [rfReplaceAll]), '');
      result.Add(Format('%s%s', [GetText(rulesRx, replaceTo), IfThen(comments.IsEmpty, '', Format('<RF>%s<Rf>', [comments]))]));
      VersiculoSeguinte;
    end;
  FinishScrollingSession;

  if ChapterViewText = tbInterlinear then
  begin
    rulesRx.Free;
    replaceTo.Free;
  end;
  //DebugLn('  TProjeto.GetChapterText: %d milliseconds', [getTickCount-starttime]);
end;

procedure TProjeto.CompileInterlinearVerseRules(var rulesRx: TRegexList;
  var replaceTo: TStringList; mode: TInterlinearMode; exporting: boolean);
var
  verseRules: string;
  keyValue: TStringArray;
  rule: string;
begin
  rulesRx := TRegexList.Create;
  replaceTo := TStringList.Create;
  if mode = imInterlinear then
    verseRules := ObterInfo('propriedades.interlinearview.verserules')
  else // intralinear (theWord mode)
    verseRules := ObterInfo('propriedades.intralinearview.verserules');

  for rule in verseRules.Split(#13) do
  begin
    try
      keyValue := rule.Split(#10);
      if (keyValue[0].IsEmpty) or ((length(keyValue) > 2) and (keyValue[2] <> '1')) then
        continue;
      if exporting and ((length(keyValue) > 2) and (keyValue[3] <> '1')) then
        continue;
      rulesRx.Add(RegexCreate(keyValue[0], [rcoUTF8]));
      replaceTo.Add(keyValue[1]);
    except
      on E: Exception do ShowMessage(Format(SInvalidInterlinearVerseRule, [rule]) + #13#10 + E.Message);
    end;
  end;
end;

function TProjeto.GetComentarios: string;
begin
  result := FTblPares.FieldByName('pare_comentarios').AsString;
end;

function TProjeto.GetComments(ref: string): string;
begin
  result := '';
  StartScrollingSession;
  try
    IrPara(ref);
    if ID = ref then
      result := GetComentarios;
  finally
    FinishScrollingSession;
  end;
end;

procedure TProjeto.SetTextDescription(text: TTipoTextoBiblico;
  description: string);
begin
  AtribuirInfo(Format('description.text%d', [Integer(text)]), description);
end;

function TProjeto.GetTextDescription(text: TTipoTextoBiblico): string;
begin
  result := ObterInfo(Format('description.text%d', [text]));
end;

procedure TProjeto.Vacuum;
begin
  if MessageDlg(SConfirmation, SVaccumProjectConfirmation, mtConfirmation, [mbYes, mbNo],0) = mrNo then
    exit;

  Fechar(true);
  with TSqlite3Dataset.Create(nil) do
  begin
    FileName:=FFileName;
    ExecSQL('VACUUM;');
    Free;
  end;
  Abrir(FFileName);
  ShowMessage(SVacuumDone);
end;

function TProjeto.GetID: string;
begin
  result := FTblPares.FieldByName('pare_id').AsString;
end;

function TProjeto.GetInterlinearMode: TInterlinearMode;
var
  t: string;
begin
  t := ObterInfo('interlinear.mode');
  if t.IsEmpty then
    t := Ord(imInterlinear).ToString;
  result := TInterlinearMode(t.ToInteger);
end;

procedure TProjeto.CopiarArquivo(origem, destino: string);
begin
  if FileExists(destino) then
    raise Exception.Create(SProjectOverrideError);

  with TMemoryStream.Create do
  try
    LoadFromFile(origem);
    SaveToFile(destino);
  finally
    Free;
  end;
end;

function TProjeto.CriarObjetoTabela(db, tabela, chave: string): TSqlite3Dataset;
begin
  result := TSqlite3Dataset.Create(nil);
  result.FileName := db;
  result.TableName := tabela;
  result.PrimaryKey := chave;
end;

function TProjeto.CriarObjetoQuery(db: string): TSQLQuery;
begin
  result := TSQLQuery.Create(nil);
  result.DataBase := TSQLite3Connection.Create(nil);
  result.DataBase.DatabaseName := db;
  result.DataBase.Open;
  TSQLite3Connection(result.DataBase).Transaction := TSQLTransaction.Create(nil);
  result.Transaction := TSQLite3Connection(result.DataBase).Transaction;
end;

function TProjeto.InserirInfo(info, valor: string): boolean;
begin
  if FTblInfo.Locate('id', info, []) then
  begin
     result := false;
     exit;
  end;

  FTblInfo.InsertRecord([info, valor]);

  result := true;
  {result := true;
  try
    FTblInfo.ExecuteDirect(format('insert into info (id, valor) values (''%s'', ''%s'')', [AnsiReplaceStr(info, '''', ''''''), AnsiReplaceStr(valor, '''', '''''')]));
  except
    result := false;
  end;}
end;

function TProjeto.AtualizarInfo(info, valor: string): boolean;
begin
  result := FTblInfo.Locate('id', info, []);

  if result then
  begin
    FTblInfo.Edit;
    FTblInfo.Fields[1].AsString := valor;
    FTblInfo.Post;
  end;

  {FTblInfo.ExecuteDirect(format('update info set valor = ''%s'' where id = ''%s''', [AnsiReplaceStr(valor, '''', ''''''), AnsiReplaceStr(info, '''', '''''')]));
  result := FTblInfo.RowsAffected = 1;}
end;

function TProjeto.AtualizarConfig(table: TSqlite3Dataset; name, value: string): boolean;
begin
  result := table.Locate('name', name, []);

  if result then
  begin
    table.Edit;
    table.Fields[1].AsString := value;
    table.Post;
  end;
end;

function TProjeto.ResgatarInfo(info: string): string;
begin
  result := '';
  if FTblInfo.Locate('id', info, []) then
    result := FTblInfo.Fields[1].AsString
  {result := FTblInfo.QuickQuery(format('select valor from info where id = ''%s''', [AnsiReplaceStr(info, '''', '''''')]));}
end;

function TProjeto.ObterDefinicaoStrong(strong: string; texto: TTipoTextoBiblico
  ): string;
begin
  if (FADicStrong[texto] = nil) or (strong = '') then
  begin
    result := ''; //'{\rtf1\ansi\ansicpg1252\deff0\deflang1046{\fonttbl{\f0\fswiss\fcharset0 Arial;}}\viewkind4\uc1\pard\f0\fs20\par}';
    exit;
  end;

  with FADicStrong[texto] do
  begin
    Close;
    Params.ParamByName('strong').AsString := strong;
    Open;
    result := FormatRTF(Fields[0].AsString);
  end;
end;

function TProjeto.ObterDefinicaoMorfo(morfo: string; texto: TTipoTextoBiblico
  ): string;
var
  m: string;
  //p, l: sizeint;
begin
  result := '';
  if (not assigned(FADicMorfo[texto])) or (morfo = '') then
    exit;

  // tratar morfologia com lema <WTXXXXX l="yyyyyy">
  m := morfo;
  //p := pos(' ', m);
  //l := length(m);
  if pos(' ', m) > 1 then
    m := copy(m, 1, pos(' ', m)-1);

  with FADicMorfo[texto] do
  begin
    Close;
    Params.ParamByName('morf').AsString := m;
    Open;

    if Fields[0].AsString = '' then
      exit;

    result := FormatRTF(Fields[0].AsString);
  end;
end;

function TProjeto.FormatRTF(rtf: string): string;
begin
  result := '';
  if rtf.IsEmpty then
    exit;

  if AnsiStartsStr('{\rtf1', rtf) then
    result := rtf
  else
    result := '{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Tahoma;}' +
      '{\f1\fswiss\fprq2\fcharset0 Tahoma;}{\f2\fswiss\fcharset0 Arial;}}' +
      '{\colortbl ;' +
        '\red0\green0\blue0;' +
        '\red0\green0\blue255;' +
        '\red150\green60\blue100;' + //maroon;
        '\red0\green255\blue0;' +
        '\red255\green0\blue255;' +
        '\red255\green0\blue0;' +
        '\red255\green255\blue0;' +
        '\red255\green255\blue255;' +
        '\red0\green0\blue128;' +
        '\red0\green128\blue128;' +
        '\red255\green0\blue0;' + //dark green
        '\red128\green0\blue128;' +
        '\red128\green0\blue0;' +
        '\red128\green128\blue0;' +
        '\red128\green128\blue128;' +
        '\red192\green192\blue192;}' +
      '\viewkind4\uc1\pard\lang1046\f0\fs20 ' + rtf + '}';
end;

procedure TProjeto.SetVerseText(versiculo: string;
  texto: TTipoTextoBiblico; replace: boolean);
begin
  FTblPares.Edit;

  // try to keep associations and update pairs
  if replace and (texto in [tbOrigem, tbDestino]) and not FTblPares.FieldByName('pare_pares').AsString.IsEmpty then
  begin
    FATmpVerse[tbOrigem ].Texto := FTblPares.Fields[FACamposTexto[tbOrigem ]].AsString;
    FATmpVerse[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;

    try
      FATmpVerse[tbOrigem ].Pares := FTblPares.FieldByName('pare_pares').AsString;
      FATmpVerse[texto].AlterarTexto(versiculo);
      FTblPares.FieldByName('pare_pares').AsString := FATmpVerse[tbOrigem].Pares;
    except
      on E: Exception do
        MessageDlg(SError, SCorruptedData + #13#10#13#10 + FormattedReference + #13#10 +
             format('%s'#13#10'pares: %s'#13#10'%s'#13#10'%s',
                    [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                    FATmpVerse[tbOrigem].DebugTokens, FATmpVerse[tbDestino].DebugTokens]),
             mtError, [mbOK], 0);
    end;
  end;

  FTblPares.Fields[FACamposTexto[texto]].AsString := versiculo;
  if not replace and (texto in [tbOrigem, tbDestino]) then // clearing associations to avoid inconsistencies
  begin
    FTblPares.FieldByName('pare_pares').AsString := '';
    FTblPares.FieldByName('pare_situacao').AsInteger := 0; // not associated
  end;

  FTblPares.Post;
end;

procedure TProjeto.PreRolagemVersiculo(DataSet: TDataSet);
//var starttime: DWord;
begin
  if not FScrollEventsEnabled or not Assigned(FAVersiculo[tbOrigem]) then
    exit;

  //starttime := getTickCount;

  FMemoVersiculo.Desativar; // hide and save verse edit if necessary

  if FAVersiculo[tbOrigem].Modificado then
  begin
    SalvarAssociacoes;
    SalvarPares;
  end;
  SalvarTexto; // salvar alterações nos textos dos versículos

  FreeAndNil(FParesAntigos); // liberando pares antigos do versículo

  if assigned(FMemoComentarios) then
    Comentarios := FMemoComentarios.Text;

  //DebugLn('  TProjeto.PreRolagemVersiculo: %d milliseconds', [getTickCount-starttime]);
end;

procedure TProjeto.PosRolagemVersiculo(DataSet: TDataSet);
var
  v: TTipoTextoBiblico;
  e: TOnNovoVersiculoEvent;
  //starttime: DWord;
begin
  // updating current reference
  Sscanf(GetID(), '%d,%d,%d', [@FReference.BookID, @FReference.Chapter, @FReference.Verse]);
  FReference.Book := NLivros[FEscopo][FReference.BookID-OffsetLivros[FEscopo]-1];

  if not FScrollEventsEnabled then
    exit;

  //starttime := getTickCount;

  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
    begin
      FAVersiculo[v].Ativo := false;
      FAVersiculo[v].Texto := FTblPares.Fields[FACamposTexto[v]].AsString;
    end;

  if assigned(FAVersiculo[tbOrigem]) and assigned(FAVersiculo[tbOrigem]) then
  begin
    try
      FAVersiculo[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;
    except
      on E: Exception do
      MessageDlg(SError, SCorruptedData + #13#10#13#10 + FormattedReference + #13#10#13#10 +
           format('%s'#13#10#13#10'pares: %s'#13#10'%s'#13#10'%s',
                  [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                  FAVersiculo[tbOrigem].DebugTokens, FAVersiculo[tbDestino].DebugTokens]),
           mtError, [mbOK], 0);

    end;
    FParesAntigos := FAVersiculo[tbOrigem].GetListaPares(plMetaData);
  end;

  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
    begin
      FAVersiculo[v].Modificado := false;
      FAVersiculo[v].XMLModificado := false;
      FAVersiculo[v].Ativo := true;
      FAVersiculo[v].Renderizar;
    end;

  SelectTreeNode(ID);

  if assigned(FMemoComentarios) then
    FMemoComentarios.Text := Comentarios;

  if assigned(FRadioGroupSituacao) then
    FRadioGroupSituacao.ItemIndex := Situacao;

  if FTblPares.Fields[FACamposTexto[tbDestino]].AsString.IsEmpty then // open verse to edition if is empty
    OnDblClickVersiculo(FAVersiculo[tbDestino].Painel);

  for e in FOnNovoVersiculo do
    e(self);

  if FAutoSave and not FExportando then
    Salvar;

  //DebugLn('  TProjeto.PosRolagemVersiculo: %d milliseconds', [getTickCount-starttime]);
end;

procedure TProjeto.SalvarPares;
begin
  FTblPares.Edit;
  FTblPares.FieldByName('pare_pares').AsString := FAVersiculo[tbOrigem].Pares;
  FTblPares.Post;
end;

procedure TProjeto.SalvarTexto;
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) and FAVersiculo[v].XMLModificado then
    begin
      FTblPares.Edit;
      FTblPares.Fields[FACamposTexto[v]].AsString := FAVersiculo[v].XML;
      FTblPares.Post;
      FAVersiculo[v].Modificado := false;
      FAVersiculo[v].XMLModificado := false;
    end;
end;

procedure TProjeto.SalvarAssociacoes;
var
  pares: TStringList;
  p: smallint;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  if Assigned(FParesAntigos) then
    RemoverAssociacoes;

  pares := FAVersiculo[tbOrigem].GetListaPares(plMetaData);

  for p:=0 to pares.Count-1 do
    if (p mod 2) = 0 then // pares estão alternados na lista
      FSugeridor.InserirPar(pares.Strings[p], pares.Strings[p+1]);

  pares.Free;
end;

procedure TProjeto.RemoverAssociacoes;
var
  p: smallint;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbOrigem] = nil) then
    exit;

  for p:=0 to FParesAntigos.Count-1 do
    if (p mod 2) = 0 then // pares estão alternados na lista
      FSugeridor.RemoverPar(FParesAntigos.Strings[p], FParesAntigos.Strings[p+1]);
end;

procedure TProjeto.SintagmaOnMouseEnter(Sender: TSyntagm);
var
  show: boolean;
begin
  case FPopupTrigger of
    ptMouseHover:     show := (GetKeyState(VK_CONTROL) and $8000 = 0) and
                              (GetKeyState(VK_SHIFT  ) and $8000 = 0);

    ptAltMouseHover:  show := (GetKeyState(VK_MENU   ) and $8000 <> 0);

    ptCtrlMouseHover: show := (GetKeyState(VK_CONTROL) and $8000 <> 0) and
                              (GetKeyState(VK_SHIFT  ) and $8000 = 0);
  end;

  if show then
  begin
    FTemporizador.Enabled := false;
    FTemporizador.Tag := PtrInt(Sender);
    FTemporizador.Enabled := true;
  end;

  HighlightStrongs(Sender);
  SetDisplayTags(false);
end;

procedure TProjeto.SintagmaOnMouseLeave(Sender: TSyntagm);
var
  hide: boolean;
  v: TTipoTextoBiblico;
begin
  FTemporizador.Enabled := false;
  hide := true;

  case FPopupTrigger of
    ptMouseHover:     hide := GetKeyState(VK_CONTROL) and $8000 = 0;
    ptAltMouseHover:  hide := GetKeyState(VK_MENU   ) and $8000 = 0;
    ptCtrlMouseHover: hide := GetKeyState(VK_CONTROL) and $8000 = 0;
  end;

  if hide then
    frmDictionaryPopup.Ocultar;

  { hide strong hightlights }
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
        FAVersiculo[v].DisableStrongHighlight;
end;

procedure TProjeto.SintagmaOnClick(Sender: TSyntagm);
begin
  FMemoVersiculo.Desativar; // saving eventual changes on verse edit

  if assigned(FOnSintagmaClick) then
    FOnSintagmaClick(Sender);

  if assigned(FAVersiculo[tbOrigem]) and
     ((Sender.VerseRef = FAVersiculo[tbOrigem]) or (Sender.VerseRef = FAVersiculo[tbDestino])) then
  begin
    if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AndamentoAssociacao > 0) then
      FRadioGroupSituacao.ItemIndex := 1 // associando
    else if (FRadioGroupSituacao.ItemIndex > 1) and (FAVersiculo[tbOrigem].AndamentoAssociacao = 0) then
      FRadioGroupSituacao.ItemIndex := 0 // sem associação
    else if (FRadioGroupSituacao.ItemIndex = 1) and ((FAVersiculo[tbOrigem].AndamentoAssociacao = 1) or (FAVersiculo[tbDestino].AndamentoAssociacao = 1)) then
      FRadioGroupSituacao.ItemIndex := 3; // associado
  end;
end;

procedure TProjeto.OnExportText(Sender: TVersiculo);
var
  lines: TStringList;
  v, text: TTipoTextoBiblico;
  SaveDlg: TSaveDialog;
  destination: string;
begin
  SaveDlg := TSaveDialog.Create(nil);
  SaveDlg.DefaultExt := '.txt';
  SaveDlg.Title := SExportTextSaveDlgTitle;
  destination := '';
  if SaveDlg.Execute then
    destination := SaveDlg.FileName;
  SaveDlg.Free;

  if destination.IsEmpty then
    exit;

  lines := TStringList.Create;

  text := tbNone;
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if Sender = FAVersiculo[v] then
    begin
      text := v;
      break;
    end;

  if text = tbNone then
     raise Exception.Create('Invalid text type in OnVersePopup');

  with FTblPares do
  begin
    StartScrollingSession;
    VersiculoInicial;
    while not FTblPares.EOF do
    begin
      lines.Add(FTblPares.Fields[FACamposTexto[text]].AsString);
      VersiculoSeguinte;
    end;
    FinishScrollingSession;
  end;
  lines.SaveToFile(destination);
  lines.Free;

  OpenDocument(ExtractFilePath(destination));
end;

procedure TProjeto.OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
var
  l, c, v: TTreeNode;
  i: smallint;
begin
  if not FScrollEventsEnabled or not assigned(FTblPares) or FClosing or not assigned(Node) then
    exit;

  if Node.HasChildren then // capítulo ou livro
  begin
    if assigned(Node.Parent) then // capitulo
    begin
      c := Node;
      l := c.Parent;
      v := c.GetFirstChild;
    end else begin // livro
      l := Node;
      c := l.GetFirstChild;
      v := c.GetFirstChild;
    end;
  end else begin // versículo
    v := Node;
    c := v.Parent;
    l := c.Parent;
  end;

  for i:=0 to QLivros[FEscopo]-1 do
    if NLivros[FEscopo][i] = l.Text then
      break;

  IrPara(format('%d,%s,%s', [i+1+OffsetLivros[FEscopo], c.Text, v.Text]));
end;

procedure TProjeto.OnAlterarTextoVersiculo(Sender: TMemoVersiculo);
var
  v: TTipoTextoBiblico;
  e: TOnNovoVersiculoEvent;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if FAVersiculo[v] = Sender.Versiculo then
    begin
      if ContainsText(Sender.Texto, #13) or ContainsText(Sender.Texto, #10) then
      begin
        MessageDlg(SLineBreaksInTextTitle, SLineBreaksInText, mtError, [mbOK], 0);
        exit;
      end;

      if Assigned(Sender.Versiculo.VersiculoPar) then
        Sender.Versiculo.AlterarTexto(Sender.Texto)
      else // reference text
        Sender.Versiculo.Texto := Sender.Texto;

      if v = tbDestino then
        for e in FOnNovoVersiculo do
          e(self);

      break;
    end;


  //MessageDlg('Versículo modificado', format('Versiculo modificado: %s', [Sender.Texto]), mtError, [mbOK], 0);
end;

procedure TProjeto.OnExibirDefinicao(Sender: TObject);
var
  v: TTipoTextoBiblico;
  point: TPoint;
  s, p: TSyntagm;
  m, rtf: string;
begin
  if frmDictionaryPopup.Visible then
    exit;

  s := TSyntagm(TTimer(Sender).Tag);
  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    if (FAVersiculo[v] = s.VerseRef) then
    begin
      if assigned(FADicMorfo[v]) then
      begin
        if s.Morph.Count > 0 then
          for m in s.Morph do
            FrmDictionaryPopup.AdicionarMorfo(m, ObterDefinicaoMorfo(m, v))
        else
        begin // não tem morfo, vejamos se os pares têm
          for p in s.Pairs do
            for m in p.Morph do
              FrmDictionaryPopup.AdicionarMorfo(m, ObterDefinicaoMorfo(m, v));
        end;
      end;

      if assigned(FADicStrong[v]) then
      begin
        if s.Strong.Count > 0 then
          for m in s.Strong do
            frmDictionaryPopup.AdicionarStrong(m, ObterDefinicaoStrong(m, v))
        else
        begin // não tem strongs, vejamos se os pares têm
          for p in s.Pairs do
            for m in p.Strong do
              FrmDictionaryPopup.AdicionarStrong(m, ObterDefinicaoStrong(m, v));
        end;
      end;

      if v = tbOrigem then
      begin
        rtf := FormatRTF(FSugeridor.GetTranslationAlternatives(s));
        if not rtf.IsEmpty then
          frmDictionaryPopup.AdicionarStrong(STranslationMemory, rtf);
      end;

      if (FrmDictionaryPopup.Strongs.Count > 0) or (FrmDictionaryPopup.Morfos.Count > 0) then
      begin
        point := s.LabelRef.ClientToScreen(s.LabelRef.ClientRect.BottomRight);
        frmDictionaryPopup.MostrarEm(point.x, point.y);
      end;
      break;
    end;
  end;
end;

procedure TProjeto.OnDblClickVersiculo(Sender: TObject);
begin
  if not Ativo then
    exit;

  FMemoVersiculo.Ativar(TScrollBox(Sender), TVersiculo(TScrollBox(Sender).Tag));
end;

procedure TProjeto.AtribuirDicStrong(dic: string; t: TTipoTextoBiblico);
//var
//  t1: TTipoTextoBiblico;
begin
  FAVersiculo[t].OnMouseEnter := @SintagmaOnMouseEnter;
  FAVersiculo[t].OnMouseLeave := @SintagmaOnMouseLeave;

  if dic = '' then
    exit;

  if not FileExists(dic) then
  begin
    MessageDlg(SStrongDictionary, format(SDictionaryDoesntExist, [FExpand(dic)]), mtError, [mbOK], 0);
    exit;
  end;

  if assigned(FADicStrong[t]) then // liberando query se não estiver mais sendo utilizada
  begin
    FADicStrong[t].Tag := FADicStrong[t].Tag - 1;
    if (FADicStrong[t].Tag = 0) then
    begin
      FADicStrong[t].Close;
      FADicStrong[t].DataBase.Close;
      TSQLite3Connection(FADicStrong[t].DataBase).Transaction.Free;
      FADicStrong[t].DataBase.Free;
      FADicStrong[t].Free;
      FADicStrong[t] := nil;
    end;
  end;
  { desabilitando reaproveitamento até corrigir erro de desalocação múltipla }
  {for t1:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
    if assigned(FADicStrong[t1]) and (FADicStrong[t1].DataBase.DatabaseName = dic) then
    begin // reaproveitando TQueries que usem o mesmo db
      FADicStrong[t] := FADicStrong[t1];
      FADicStrong[t].Tag := FADicStrong[t].Tag + 1;
      break;
    end;}

  if not assigned(FADicStrong[t]) then
  begin // primeira ocorrência deste db
    FADicStrong[t] := CriarObjetoQuery(dic);
    FADicStrong[t].SQL.Text := 'select data from topics, content where subject = :strong and id = topic_id';
    FADicStrong[t].Prepare;
    FADicStrong[t].Tag := 1;
  end;
end;

procedure TProjeto.AtribuirDicMorfo(dic: string; t: TTipoTextoBiblico);
//var
//  t1: TTipoTextoBiblico;
begin
  if dic = '' then
    exit;

  if not FileExists(dic) then
  begin
    MessageDlg(SMorphologyDictionary, format(SDictionaryDoesntExist, [FExpand(dic)]), mtError, [mbOK], 0);
    exit;
  end;

  if assigned(FADicMorfo[t]) then // liberando query se não estiver mais sendo utilizada
  begin
    FADicMorfo[t].Tag := FADicMorfo[t].Tag - 1;
    if (FADicMorfo[t].Tag = 0) then
    begin
      FADicMorfo[t].Close;
      FADicMorfo[t].DataBase.Close;
      TSQLite3Connection(FADicMorfo[t].DataBase).Transaction.Free;
      FADicMorfo[t].DataBase.Free;
      FADicMorfo[t].Free;
      FADicMorfo[t] := nil;
    end;
  end;
  // desabilitando reaproveitamento até corrigir erro de desalocação múltipla
  {for t1:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
    if assigned(FADicMorfo[t1]) and (FADicMorfo[t1].DataBase.DatabaseName = dic) then
    begin // reaproveitando TQueries que usem o mesmo db
      FADicMorfo[t] := FADicMorfo[t1];
      FADicMorfo[t].Tag := FADicStrong[t].Tag + 1;
      break;
    end;}

  if not assigned(FADicMorfo[t]) then
  begin // primeira ocorrência deste db
    FADicMorfo[t] := CriarObjetoQuery(dic);
    FADicMorfo[t].SQL.Text := 'select data from topics, content where subject = :morf and id = topic_id';
    FADicMorfo[t].Prepare;
    FADicMorfo[t].Tag := 1;
 end;
  FAVersiculo[t].OnMouseEnter := @SintagmaOnMouseEnter;
  FAVersiculo[t].OnMouseLeave := @SintagmaOnMouseLeave;
end;

procedure TProjeto.AtualizarArvore;
var
  n, ul, uc: TTreeNode;
  qv, qc, l, c, v, s: smallint;
  sl, sc: array[0..3] of smallint;
  b: boolean;
begin
  if not assigned(FArvore) then exit;
  // POSSIBLE BUG: TopItem returns the first VISIBLE node, not the top SELECTED node!!!
  n := FArvore.TopItem.GetFirstChild.GetFirstChild; // nó versículo atual
  l := OffsetLivros[FEscopo] + 1; // livro atual
  c := 1;  // capítulo atual
  v := 1;  // versículo atual
  ul := FArvore.TopItem; // nó do livro atual
  uc := FArvore.TopItem.GetFirstChild; // nó do capítulo atual
  qv := 0; // índice no vetor QVersiculos
  qc := 0; // índice no vetor QCapitulos

  sc[0] := 0;  // contagem de versículos por situação no capítulo
  sc[1] := 0;
  sc[2] := 0;
  sc[3] := 0;

  sl[0] := 0;  // contagem de versículos por situação no livro
  sl[1] := 0;
  sl[2] := 0;
  sl[3] := 0;

  VersiculoInicial;

  while not FTblPares.EOF do
  begin
    if v > QVersiculos[FEscopo][qv] then // novo capítulo
    begin
      { atualizando nó do capítulo }
      //if qv = 69 then
      //  MessageDlg('sc', format('[%d][%d][%d][%d] - QVersiculos[%d]=%d', [sc[0],sc[1],sc[2],sc[3], qv, QVersiculos[qv]]), mtInformation, [mbOK], 0);
      b := false;
      for s:=0 to 3 do // todos os versículos estão na mesma situação?
        if sc[s] = QVersiculos[FEscopo][qv] then
        begin
          uc.ImageIndex := s; uc.SelectedIndex := uc.ImageIndex + 4;
          b := true;
          break;
        end;
      if not b then // várias situacoes diferentes
      begin
        if sc[2] > 0 then //(sc[0] = 0) and (sc[1] = 0) then
          uc.ImageIndex := 2
        else
          uc.ImageIndex := 1;
        uc.SelectedIndex := uc.ImageIndex + 4;
      end;
      sc[0] := 0;
      sc[1] := 0;
      sc[2] := 0;
      sc[3] := 0;
      inc(sl[uc.ImageIndex]);

      v := 1;
      inc(c);
      if c > QCapitulos[FEscopo][qc] then // novo livro
      begin
        { atualizando nó do livro }
        //if qc = 27 then
        //  MessageDlg('sl', format('[%d][%d][%d][%d] - [%d]', [sl[0],sl[1],sl[2],sl[3], QCapitulos[qc]]), mtInformation, [mbOK], 0);
        b := false;
        for s:=0 to 3 do // todos os capítulos estão na mesma situação?
          if sl[s] = QCapitulos[FEscopo][qc] then
          begin
            ul.ImageIndex := s; ul.SelectedIndex := ul.ImageIndex + 4;
            b := true;
            break;
          end;
        if not b then // várias situações diferentes
        begin
          if sl[2] > 0 then //(sl[0] = 0) and (sl[1] = 0) then
            ul.ImageIndex := 2
          else
            ul.ImageIndex := 1;
          ul.SelectedIndex := ul.ImageIndex + 4;
        end;
        sl[0] := 0;
        sl[1] := 0;
        sl[2] := 0;
        sl[3] := 0;

        c := 1;
        inc(l);
        inc(qc);
        ul := ul.GetNextSibling;
        uc := ul.GetFirstChild;
      end else
        uc := uc.GetNextSibling;

      n := uc.GetFirstChild;
      inc(qv);
    end;
    inc(sc[Situacao]);
    n.ImageIndex := Situacao;
    n.SelectedIndex := n.ImageIndex + 4;

    VersiculoSeguinte;
    inc(v);
    n := n.GetNextSibling;
  end;

  { atualizando o último capítulo - Apocalipse 22 }
  b := false;
  for s:=0 to 3 do // todos os versículos estão na mesma situação?
    if sc[s] = QVersiculos[FEscopo][qv] then
    begin
      uc.ImageIndex := s; uc.SelectedIndex := uc.ImageIndex + 4;
      b := true;
      break;
    end;
  if not b then // várias situacoes diferentes
  begin
    if sc[2] > 0 then //(sc[0] = 0) and (sc[1] = 0) then
      uc.ImageIndex := 2
    else
      uc.ImageIndex := 1;

    uc.SelectedIndex := uc.ImageIndex + 4;
  end;

  { atualizando último livro - Apocalipse }
  b := false;
  inc(sl[Situacao]);
  //MessageDlg('sl', format('[%d][%d][%d][%d] - [%d]', [sl[0],sl[1],sl[2],sl[3], QCapitulos[qc]]), mtInformation, [mbOK], 0);
  for s:=0 to 3 do // todos os capítulos estão na mesma situação?
    if sl[s] = QCapitulos[FEscopo][qc] then
    begin
      ul.ImageIndex := s; ul.SelectedIndex := ul.ImageIndex + 4;
      b := true;
      break;
    end;
  if not b then // várias situações diferentes
  begin
    if sl[2] > 0 then //(sl[0] = 0) and (sl[1] = 0) then
      ul.ImageIndex := 2
    else
      ul.ImageIndex := 1;
    ul.SelectedIndex := ul.ImageIndex + 4;
  end;
end;

procedure TProjeto.AtualizarArvore(id: string);
var
  i: smallint;
  al, ac: array[0..3] of smallint;
  nl, nc, nv, n: TTreeNode;
  b: boolean;
begin
  if not assigned(FArvore) or FClosing then
    exit;

  al[0] := 0; al[1] := 0; al[2] := 0; al[3] := 0;
  ac[0] := 0; ac[1] := 0; ac[2] := 0; ac[3] := 0;

  nl := FArvore.Items[0];
  for i:=BookID-OffsetLivros[FEscopo]-1 downto 1 do
    nl := nl.GetNextSibling;

  nc := nl.GetFirstChild;
  for i:=Chapter-1 downto 1 do
  begin
    inc(al[nc.ImageIndex]);
    nc := nc.GetNextSibling;
  end;

  nv := nc.GetFirstChild;
  for i:=Verse-1 downto 1 do
  begin
    inc(ac[nv.ImageIndex]);
    nv := nv.GetNextSibling;
  end;

  if nv.ImageIndex <> Situacao then
  begin
    { atualizando versículo }
    nv.ImageIndex := Situacao; nv.SelectedIndex := nv.ImageIndex + 4;
    { atualizando capítulo }
    n := nv;
    while assigned(n) do // percorrendo últimos versículos do capítulo
    begin
      inc(ac[n.ImageIndex]);
      n := n.GetNextSibling;
    end;

    b := false;
    for i:=0 to 3 do // todos os versículos estão na mesma situação?
    begin
      if ac[i] = QVersiculos[FEscopo][ QCapLivros[FEscopo][BookID-OffsetLivros[FEscopo]]+Chapter-1 ] then
      begin
        nc.ImageIndex := i; nc.SelectedIndex := nc.ImageIndex + 4;
        b := true;
        break;
      end;
    end;
    if not b then // várias situações diferentes
    begin
      if ac[2] > 0 then
        nc.ImageIndex := 2
      else
        nc.ImageIndex := 1;
      nc.SelectedIndex := nc.ImageIndex + 4;
    end;
    { atualizando livro }
    n := nc;
    while assigned(n) do // percorrendo últimos capítulos do livro
    begin
      inc(al[n.ImageIndex]);
      n := n.GetNextSibling;
    end;

    b := false;
    for i:=0 to 3 do // todos os versículos estão na mesma situação?
    begin
      if al[i] = QCapitulos[FEscopo][BookID-OffsetLivros[FEscopo]] then
      begin
        nl.ImageIndex := i; nl.SelectedIndex := nl.ImageIndex + 4;
        b := true;
        break;
      end;
    end;
    if not b then // várias situações diferentes
    begin
      nl.ImageIndex := IfThen(al[2] > 0, 2, 1);
      nl.SelectedIndex := nl.ImageIndex + 4;
    end;
  end;
end;

procedure TProjeto.SelectTreeNode(id: string);
var
  b, c, v, i: smallint;
  node: TTreeNode;
begin
  if not assigned(FArvore) or FClosing then
    exit;

  SScanf(id, '%d,%d,%d', [@b, @c, @v]);

  node := FArvore.Items[0];
  for i:=b-OffsetLivros[FEscopo]-1 downto 1 do
    node := node.GetNextSibling;

  node := node.GetFirstChild;
  for i:=c-1 downto 1 do
    node := node.GetNextSibling;

  node := node.GetFirstChild;
  for i:=v-1 downto 1 do
    node := node.GetNextSibling;

  node.Selected := true;
end;

constructor TProjeto.Criar;
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    FAVersiculo[v]   := nil;
    FADicStrong[v]   := nil;
    FADicMorfo[v]    := nil;
    FACamposTexto[v] := 0;
  end;

  FParesAntigos       := nil;
  FOnNovoVersiculo    := TOnNovoVersiculoEvents.Create;
  FOnAlterarVersiculo := nil;
  FSugeridor          := nil;
  FTblPares           := nil;
  FTblInfo            := nil;
  FEscopo             := etNone;
  FClosing            := false;
  FVerseStrongsCountMode := scNone;

  FTemporizador          := TTimer.Create(nil);
  FTemporizador.Enabled  := false;
  FTemporizador.Interval := 300;
  FTemporizador.OnTimer  := @OnExibirDefinicao;

  FMemoVersiculo := TMemoVersiculo.Criar;
  FMemoVersiculo.QuandoModificarVersiculo := @OnAlterarTextoVersiculo;
end;

constructor TProjeto.Criar(paineis: array of TScrollbox; navegador: TTreeView;
  rsituacao: TRadioGroup; rcomentarios: TMemo);
var
  v: integer;
begin
  Criar;

  for v:=low(paineis) to high(paineis) do
    if (v <= Ord(tbConsulta2)) and assigned(paineis[v]) then
      NovoObjetoVersiculo(paineis[v], TTipoTextoBiblico(v));

  Arvore              := navegador;
  FRadioGroupSituacao := rsituacao;
  FMemoComentarios    := rcomentarios;
end;

destructor TProjeto.Destruir;
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    if assigned(FAVersiculo[v]) then
      FAVersiculo[v].Destruir;

    if assigned(FADicStrong[v]) then
    begin
      FADicStrong[v].Close;
      FADicStrong[v].DataBase.Close;
      TSQLite3Connection(FADicStrong[v].DataBase).Transaction.Free;
      FADicStrong[v].DataBase.Free;
      FADicStrong[v].Free;
      FADicStrong[v] := nil;
    end;

    if assigned(FADicMorfo[v]) then
    begin
      FADicMorfo[v].Close;
      FADicMorfo[v].DataBase.Close;
      TSQLite3Connection(FADicMorfo[v].DataBase).Transaction.Free;
      FADicMorfo[v].DataBase.Free;
      FADicMorfo[v].Free;
      FADicMorfo[v] := nil;
    end;
  end;

  FreeAndNil(FParesAntigos);

  FOnNovoVersiculo.Free;
  FSugeridor.Free;
  FTemporizador.Free;
  FMemoVersiculo.Destruir;
end;

procedure TProjeto.Abrir(Nome: string);
  function IdentificarEscopo: TEscopoTexto;
  begin
    FTblPares.First;
    if FTblPares.FieldByName('pare_id').AsString = '40,1,1' then
    begin
      result := etNT;
      exit;
    end;
    FTblPares.Last;
    if FTblPares.FieldByName('pare_id').AsString = '66,22,21' then
      result := etONT
    else
      result := etOT;
    FTblPares.First;
  end;
var
  t: TTipoTextoBiblico;
  s: string;
  f: TFont;
begin
  if not FileExists(Nome) then
  begin
      MessageDlg(SOpenProject, format(SProjectDoesntExist, [Nome]), mtError, [mbOK], 0);
    exit;
  end;

  FFileName := Nome;
  FTblPares := CriarObjetoTabela(Nome, 'pares', 'pare_id');

  FTblInfo  := CriarObjetoTabela(Nome, 'info', 'id');
  FSugeridor := TGerSugestoes.Criar(Nome);

  FTblPares.Open;
  FTblInfo.Open;

  f := TFont.Create;
  for t:=tbOrigem to tbConsulta2 do
  begin
    if not assigned(FAVersiculo[t]) then
       continue;

    { definindo fontes }
    f.Name := 'default';
    f.Size := 12;

    s := ObterInfo(format('fonte%d.tamanho', [t]));
    if not s.IsEmpty then
      f.Size := StrToInt(s);
    s := ObterInfo(format('fonte%d.nome', [t]));
    if not s.IsEmpty then
      f.Name := s;
    s := ObterInfo(format('fonte%d.bold', [t]));
    if not s.IsEmpty then
      f.Bold := Boolean(StrToInt(s));

    FAVersiculo[t].Fonte := f;
    FAVersiculo[t].PalavrasComStrongEmNegrito := FPalavrasComStrongEmNegrito;
    FAVersiculo[t].StrongsCountMode := FVerseStrongsCountMode;

    { definindo dicionários }
    AtribuirDicStrong(ObterInfo(format('dicstrong%d', [t])), t);
    AtribuirDicMorfo(ObterInfo(format('dicmorfo%d', [t])), t);
  end;
  f.free;

  FATmpVerse[tbOrigem ] := TVersiculo.Criar;
  FATmpVerse[tbDestino] := TVersiculo.Criar;
  FATmpVerse[tbOrigem ].VersiculoPar := FATmpVerse[tbDestino];

  FACamposTexto[tbOrigem]    := FTblPares.FindField('pare_texto_origem').Index;
  FACamposTexto[tbDestino]   := FTblPares.FindField('pare_texto_destino').Index;
  FACamposTexto[tbConsulta1] := FTblPares.FindField('pare_texto_consulta1').Index;
  FACamposTexto[tbConsulta2] := FTblPares.FindField('pare_texto_consulta2').Index;

  if FEscopo = etNone then
    FEscopo := IdentificarEscopo;

  if assigned(FArvore) then
  begin
    PreencherArvore;
    AtualizarArvore;
    //FArvore.FullCollapse;
    FArvore.Enabled := true;
  end;

  if assigned(FMemoComentarios) then
    FMemoComentarios.Enabled := true;
  if assigned(FRadioGroupSituacao) then
    FRadioGroupSituacao.Enabled := true;

  FTblPares.BeforeScroll := @PreRolagemVersiculo;
  FTblPares.AfterScroll  := @PosRolagemVersiculo;
  FArvore.OnChange       := @OnMudancaVersiculo;

  ScrollEventsEnabled := true;
  IrPara(ObterInfo('marcador'));

  FAtivo := true;
  FDisplayTags := false;
end;

procedure TProjeto.Fechar(_Commit: boolean);
var
  t: TTipoTextoBiblico;
begin
  FClosing := true;

  if assigned(FArvore) then
  begin
    //FArvore.FullCollapse;
    FArvore.Enabled := false;
    FArvore.Items.Clear;
  end;

  PreRolagemVersiculo(nil);

  for t:=low(FAVersiculo) to high(FAVersiculo) do // saving font sizes
    if assigned(FAVersiculo[t]) then
      AtribuirInfo(format('fonte%d.tamanho', [t]), IntToStr(FAVersiculo[t].Fonte.Size));

  if _Commit then
  begin
    AtribuirInfo('marcador', FTblPares.FieldByName('pare_id').AsString);
    Commit;
  end
  else
    Rollback;

  FTblPares.Close;
  FreeAndNil(FTblPares);

  FTblInfo.Close;
  FreeAndNil(FTblInfo);

  FMemoVersiculo.Desativar;

  FATmpVerse[tbOrigem ].Destruir;
  FATmpVerse[tbDestino].Destruir;

  if assigned(FMemoComentarios) then
  begin
    FMemoComentarios.Enabled := false;
    FMemoComentarios.Caption := '';
  end;
  if assigned(FRadioGroupSituacao) then
  begin
    FRadioGroupSituacao.Enabled := false;
    FRadioGroupSituacao.ItemIndex := 0;
  end;

  FClosing := false;
  FAtivo := false;
end;

procedure TProjeto.Commit;
begin
  if assigned(FSugeridor) then
    FSugeridor.Commit;
  if assigned(FTblPares) then
    FTblPares.ApplyUpdates;
  if assigned(FTblInfo) then
    FTblInfo.ApplyUpdates;
end;

procedure TProjeto.Rollback;
begin
  FTblPares.ClearUpdates;
  FTblInfo.ClearUpdates;
  FSugeridor.Rollback;
end;

procedure TProjeto.IrPara(Referencia: string);
begin
  if ID <> Referencia then
  begin
    FTblPares.Locate('pare_id', Referencia, []);
    if ID <> Referencia then
      raise Exception.Create(Format('Reference not found in project [%s]', [Referencia]));
  end;
end;

procedure TProjeto.VersiculoSeguinte;
begin
  if not FTblPares.EOF then
    FTblPares.Next;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoAnterior;
begin
  if not FTblPares.BOF then
    FTblPares.Prior;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoInicial;
begin
  if not FTblPares.BOF then
    FTblPares.First;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoFinal;
begin
  if not FTblPares.EOF then
    FTblPares.Last;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) then
    SugerirAssociacao;
end;

procedure TProjeto.Atualizar;
begin
  PosRolagemVersiculo(FTblPares);
end;

procedure TProjeto.Limpar;
begin
  if assigned(FAVersiculo[tbOrigem]) then
  begin
    FAVersiculo[tbOrigem].LimparAssociacoes;
    SalvarAssociacoes;
    SalvarPares;
    if assigned(FRadioGroupSituacao) then
      FRadioGroupSituacao.ItemIndex := 0; // not associated
  end;
end;

procedure TProjeto.Salvar;
var
  e: TOnNovoVersiculoEvent;
begin
  PreRolagemVersiculo(nil);
  AtribuirInfo('marcador', FTblPares.FieldByName('pare_id').AsString);
  Commit;

  if assigned(FAVersiculo[tbOrigem]) and assigned(FAVersiculo[tbDestino]) then
  begin
    FAVersiculo[tbOrigem].Modificado  := false;
    FAVersiculo[tbDestino].Modificado := false;
  end;
  //AtualizarArvore;
  for e in FOnNovoVersiculo do
    e(Self);
end;

procedure TProjeto.SugerirAssociacao;
begin
  FSugeridor.SugerirAssociacoes(FAVersiculo[tbOrigem]);

  if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AndamentoAssociacao > 0) then
    FRadioGroupSituacao.ItemIndex := 1; // associando

  FAVersiculo[tbOrigem].Renderizar;
  FAVersiculo[tbDestino].Renderizar;
end;

procedure TProjeto.RecriarBaseSugestoes;
begin
  RecriarBaseSugestoes(nil);
end;

procedure TProjeto.RecriarBaseSugestoes(pb: TProgressBar);
var
  pares: TStringList;
  p: smallint;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  FSugeridor.LimparBaseAssociacoes;

  try
    try
      if assigned(pb) then
      begin
        pb.Position := 0;
        pb.Min := 0;
        pb.Max := QLinhas[FEscopo];
        pb.Step := 1;
        pb.Visible := true;
      end;

      FExportando := true;

      StartScrollingSession;
      VersiculoInicial;
      while not FTblPares.EOF do
      begin
        if length(FTblPares.FieldByName('pare_pares').AsString) > 0 then
        begin

          FATmpVerse[tbOrigem ].Texto := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString;
          FATmpVerse[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
          FATmpVerse[tbOrigem ].Pares := FTblPares.FieldByName('pare_pares').AsString;

          pares := FATmpVerse[tbOrigem ].GetListaPares(plMetaData);

          for p:=0 to pares.Count-1 do
            if (p mod 2) = 0 then // pares estão alternados na lista
              FSugeridor.InserirPar(pares.Strings[p], pares.Strings[p+1]);

          pares.Free;
        end;
        if assigned(pb) then
        begin
           pb.StepIt;
           if (pb.Position mod 50) = 0 then
             Application.ProcessMessages;
        end;
        VersiculoSeguinte;
      end;
    except
      on E: Exception do
         ShowMessage( 'Error: '+ E.ClassName + #13#10 + E.Message + #13#10#13#10 +
                      'Ref: ' + GetID + #13#10 +
                      'Par1: ' + pares.Strings[p] + #13#10 +
                      'Par2: ' + pares.Strings[p+1] + #13#10);
    end;
  finally
    FinishScrollingSession;
    PosRolagemVersiculo(nil);
    FExportando := false;
    if assigned(pb) then
       pb.Visible := false;
  end;
end;

function TProjeto.ImportarModuloTheWord(arquivo: string;
  texto: TTipoTextoBiblico; replace: boolean): boolean;
begin
  result := ImportarModuloTheWord(arquivo, texto, nil, replace);
end;

function TProjeto.ImportarModuloTheWord(arquivo: string;
  texto: TTipoTextoBiblico; pb: TProgressBar; replace: boolean): boolean;
var
  modulo: TStringList;
  i, linesOffset, projectOffset: smallint;
  reVazio, reComments, reDescription, reCharset, reVerseRules, verseRule: IRegex;
  mtVerseRules, mtDescription: IMatch;
  rxVerseRulesFrom: TRegexList;
  rxVerseRulesTo: TStringList;
  propriedades: TStringStream;
  m: smallint;
  line: string;
  moduleScope: TEscopoTexto;
begin
  rxVerseRulesFrom := nil;
  rxVerseRulesTo := nil;
  result := false;
  linesOffset := 0;
  projectOffset := 0;
  if arquivo.EndsWith('.ont') then
  begin
    moduleScope := etONT;
    if FEscopo = etNT then
      linesOffset := QLinhas[etOT]; // saltando o velho testamento
  end
  else if arquivo.EndsWith('.nt') then
  begin
    moduleScope := etNT;
    if FEscopo = etOT then
    begin
      ShowMessage(SModuleDoesNotIncludeScope);
      exit;
    end;
    if FEscopo = etONT then
      projectOffset := QLinhas[etOT];
  end
  else if arquivo.EndsWith('.ot') then
  begin
    moduleScope := etOT;
    if FEscopo = etNT then
    begin
      ShowMessage(SModuleDoesNotIncludeScope);
      exit;
    end;
  end else
  begin
    ShowMessage(SInvalidModuleType);
    exit;
  end;

  try
    modulo := TStringList.Create;
    modulo.LoadFromFile(arquivo);

    if modulo.Count < QLinhas[moduleScope] then
    begin
      MessageDlg(SError, format(SInvalidLineCount, [QLinhas[moduleScope]]), mtError, [mbOK], 0);
      exit;
    end;

    reVazio        := RegexCreate('^\s*$', [rcoUTF8]);
    reComments     := RegexCreate('\s*#.*$', [rcoUTF8]);
    reDescription  := RegexCreate('^\s*description\s*=\s*(.*)$', [rcoUTF8]);
    reCharset      := RegexCreate('^\s*charset\s*=\s*(.*)$', [rcoUTF8]);
    reVerseRules   := RegexCreate('^\s*verse.rule\s*=\s*"(.*?)(?<!")"(?!")\s+"(.*?)"(?=\s*$|\s+"(.*?)(?<!")"(?!"))', [rcoUTF8]);

    FrmEscolherVerseRules.Reset;
    propriedades := TStringStream.Create('');

    for i:=QLinhas[moduleScope] to modulo.Count-1 do
    begin
      line := modulo[i];
      { eliminando comentários }
      line := reComments.Replace(line, '');

      if reVazio.IsMatch(line) then
        continue;

      if reCharset.IsMatch(line) then
      begin
        if MessageDlg(SWarning, SPotentialNonUTF8ModuleWarning, mtWarning, [mbIgnore, mbAbort], 0) = mrAbort then
          exit;
      end;

      propriedades.WriteString(modulo[i] + #13#10);

      mtDescription := reDescription.Match(line);
      if mtDescription.Success then
      begin
        SetTextDescription(texto, mtDescription.Groups[1].Value);
        continue;
      end;

      mtVerseRules := reVerseRules.Match(line);
      if mtVerseRules.Success then
         FrmEscolherVerseRules.AddVerseRule(mtVerseRules.Groups[1].Value, mtVerseRules.Groups[2].Value);
    end;

    if ((texto = tbOrigem)  and (ObterInfo('propriedades.origem')  = '')) or // não importar propriedades quando já existem
       ((texto = tbDestino) and (ObterInfo('propriedades.destino') = '')) then
    begin
      case texto of
        tbOrigem:  AtribuirInfo('propriedades.origem', propriedades.DataString);
        tbDestino: AtribuirInfo('propriedades.destino', propriedades.DataString);
      end;
    end;
    propriedades.Free;

    if FrmEscolherVerseRules.HasRules and (FrmEscolherVerseRules.ShowModal = mrOK) then
      FrmEscolherVerseRules.CompileVerseRules(rxVerseRulesFrom, rxVerseRulesTo);

    //modulo.SaveToFile(arquivo + '.txt');
    if assigned(pb) then
    begin
      pb.Position := 0;
      pb.Min := 0;
      pb.Max := QLinhas[FEscopo];
      pb.Step := 1;
      pb.Visible := true;
    end;

    FExportando := true;
    StartScrollingSession;
    VersiculoInicial;

    for i:=0 to projectOffset-1 do
      VersiculoSeguinte;

    for i:=linesOffset to QLinhas[moduleScope] - 1 do
    begin
      if FTblPares.EOF then break;
      //line := modulo[i];

      { aplicando verse.rules }
      if assigned(rxVerseRulesFrom) then
        for m:=0 to rxVerseRulesFrom.Count-1 do
          modulo[i] := rxVerseRulesFrom[m].Replace(modulo[i], rxVerseRulesTo[m]);

      SetVerseText(modulo[i], texto, replace);

      VersiculoSeguinte;

      if assigned(pb) then
      begin
        pb.StepIt;
        if (pb.Position mod 75) = 0 then
           Application.ProcessMessages;
      end;
    end;
    FinishScrollingSession;
    FExportando := false;
    AtualizarArvore(FMarker);
  finally
    modulo.Free;
    if assigned(pb) then
       pb.Visible := false;
    FreeAndNil(rxVerseRulesFrom);
    FreeAndNil(rxVerseRulesTo);
  end;
  //FTblPares.ApplyUpdates;
  result := true;
end;

procedure TProjeto.NovoObjetoVersiculo(owner: TScrollBox;
  texto: TTipoTextoBiblico);
begin
  FAVersiculo[texto] := TVersiculo.Criar(owner);
  owner.Tag := ptrint(FAVersiculo[texto]);
  owner.OnDblClick := @OnDblClickVersiculo;
  FAVersiculo[texto].OnClick := @SintagmaOnClick;
  FAVersiculo[texto].OnExportText := @OnExportText;

  //FAVersiculo[texto].MostrarDicas := true;
  if texto in [tbConsulta1, tbConsulta2] then
    FAVersiculo[texto].CorDesassociado := clWindowText;

  //if texto = tbOrigem then
  //  FAVersiculo[texto].StrongMorfoComoChave := true;

  if (texto = tbDestino) and assigned(FAVersiculo[tbOrigem]) then
    FAVersiculo[tbDestino].VersiculoPar := FAVersiculo[tbOrigem]
  else if (texto = tbOrigem) and assigned(FAVersiculo[tbDestino]) then
    FAVersiculo[tbOrigem].VersiculoPar := FAVersiculo[tbDestino];
end;

procedure TProjeto.ExportarTextoDestinoComStrongs(arquivo: string; opcoes: TOpcoesExportacao);
begin
  ExportarTextoDestinoComStrongs(arquivo, nil, opcoes);
end;

procedure TProjeto.ExportarTextoDestinoComStrongs(arquivo: string;
  pb: TProgressBar; opcoes: TOpcoesExportacao);
var
  lines, errors: TStringList;
begin
  if not assigned(FAVersiculo[tbOrigem]) or not assigned(FAVersiculo[tbDestino]) then
    exit;

  try
    if assigned(pb) then
    begin
      pb.Position := 0;
      pb.Min := 0;
      pb.Max := QLinhas[FEscopo];
      pb.Step := 1;
      pb.Visible := true;
    end;
    FExportando := true;
    lines := TStringList.Create;
    errors := TStringList.Create;

    StartScrollingSession;
    with FTblPares do
    begin
      VersiculoInicial;
      while not FTblPares.EOF do
      begin
        if length(FTblPares.FieldByName('pare_pares').AsString) = 0 then
        begin
          lines.Add(FTblPares.Fields[FACamposTexto[tbDestino]].AsString)
        end
        else
        begin
          FATmpVerse[tbOrigem ].Texto := FTblPares.Fields[FACamposTexto[tbOrigem ]].AsString;
          FATmpVerse[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
          try
            FATmpVerse[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;
          except
            on E: Exception do
              errors.Add(SCorruptedData + #13#10#13#10 + FormattedReference + #13#10 +
                   format('%s'#13#10'pares: %s'#13#10'%s'#13#10'%s'#13#10,
                          [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                          FATmpVerse[tbOrigem].DebugTokens, FATmpVerse[tbDestino].DebugTokens]));
          end;
          lines.Add(FATmpVerse[tbDestino].GetLinhaONT(
            (oeExportarMorfologia in opcoes),
            (oeExportarNAComoItalicos in opcoes),
            (oeStrongsReutilizados in opcoes),
            (oeStrongsNaoTraduzidos in opcoes))
          );
        end;

        if (oeExportarComentarios in opcoes) and not FTblPares.FieldByName('pare_comentarios').AsString.isEmpty then
          lines[lines.Count-1] := lines[lines.Count-1] + '<RF>' +
            AnsiReplaceStr(FTblPares.FieldByName('pare_comentarios').AsString, #13#10, '<CM>') + '<Rf>';

        if assigned(pb) then
        begin
          pb.StepIt;
          if (pb.Position mod 50) = 0 then
             Application.ProcessMessages;
        end;
        VersiculoSeguinte;
      end;
    end;
    FinishScrollingSession;

    try
      if arquivo.EndsWith('.mybible') then // MySword module?
        ExportMySwordBible(lines, arquivo, ResgatarInfo('propriedades.destino'))
      else
      begin
        if (pos(lines[0], '<WG') = 0) and     { no Greek tags in the first verse? }
           (pos(lines[0], '<WH') = 0) then    { nor Hebrew tags? }
          lines[0] := lines[0] + '<_MORPH_>'; { making sure theWord will know the module has tags }

        ExportTheWordBible(lines, arquivo, ResgatarInfo('propriedades.destino'));
      end;
    except
      on E: Exception do MessageDlg(SError, SExportToFileError, mtError, [mbOK], 0);
    end;
  finally
    lines.Free;
    FExportando := false;
    PosRolagemVersiculo(nil);
    if assigned(pb) then
      pb.Visible := false;
  end;

  if errors.Count > 0 then
  begin
    errors.SaveToFile('export-errors.txt');
    OpenDocument('export-errors.txt');
  end;
  errors.Free;

end;

procedure TProjeto.ExportarTextoInterlinear(arquivo: string; opcoes: TOpcoesExportacao);
begin
  ExportarTextoInterlinear(arquivo, nil, opcoes);
end;

{ TODO: Unify this code with ExportarTextoDestinoComStrongs() }
procedure TProjeto.ExportarTextoInterlinear(arquivo: string;
  pb: TProgressBar; opcoes: TOpcoesExportacao);
var
  i: integer;
  lines: TStringList;
  rulesRx: TRegexList;
  replaceTo: TStringList;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  rulesRx := nil;
  replaceTo := nil;
  if arquivo.EndsWith('.mybible') then
    CompileInterlinearVerseRules(rulesRx, replaceTo, imInterlinear, true)
  else // theWord mode
    CompileInterlinearVerseRules(rulesRx, replaceTo, imIntralinear, true);

  try
    if assigned(pb) then
    begin
      pb.Position := 0;
      pb.Min := 0;
      pb.Max := QLinhas[FEscopo];
      pb.Step := 1;
      pb.Visible := true;
    end;
    FExportando := true;
    lines := TStringList.Create;

    StartScrollingSession;
    with FTblPares do
    begin
      VersiculoInicial;
      while not FTblPares.EOF do
      begin
        if length(FTblPares.FieldByName('pare_pares').AsString) = 0 then
          lines.Add(FTblPares.Fields[FACamposTexto[tbOrigem]].AsString)
        else
        begin
          FATmpVerse[tbOrigem ].Texto := FTblPares.Fields[FACamposTexto[tbOrigem] ].AsString;
          FATmpVerse[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
          try
            FATmpVerse[tbOrigem ].Pares := FTblPares.FieldByName('pare_pares').AsString;
          except
            on E: Exception do
              MessageDlg(SError, SCorruptedData + #13#10#13#10 + FormattedReference + #13#10 +
                   format('%s'#13#10'pares: %s'#13#10'%s'#13#10'%s',
                          [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                          FATmpVerse[tbOrigem].DebugTokens, FATmpVerse[tbDestino].DebugTokens]),
                   mtError, [mbOK], 0);
          end;

          if arquivo.EndsWith('.mybible') then
            lines.Add(FATmpVerse[tbOrigem].GetMySwordInterlinearLine)
          else
            lines.Add(FATmpVerse[tbOrigem].GetTheWordInterlinearLine);
        end;

        // apply verse rules
        for i:=0 to rulesRx.Count-1 do
        begin
          try
            lines[lines.Count-1] := rulesRx[i].Replace(lines[lines.Count-1], replaceTo[i]);
          except
            on E: Exception do ShowMessage(SInterlinearVerseRuleReplaceError + #13#10 + E.Message);
          end;
        end;

        if pb <> nil then
        begin
          pb.StepIt;
          if (pb.Position mod 50) = 0 then
             Application.ProcessMessages;
        end;
        VersiculoSeguinte;
      end;
    end;
    FinishScrollingSession;

    //lines.Add('');
    //lines.Add(ResgatarInfo('propriedades.origem'));
    //lines[0] := #239#187#191 + lines[0]; // adicionando BOM

    try
      if arquivo.EndsWith('.mybible') then // MySword module?
        ExportMySwordBible(lines, arquivo, ResgatarInfo('propriedades.origem'))
      else
        ExportTheWordBible(lines, arquivo, ResgatarInfo('propriedades.origem'));
    except
      on E: Exception do MessageDlg(SError, SExportToFileError, mtError, [mbOK], 0);
    end;
  finally
    rulesRx.Free;
    replaceTo.Free;
    lines.Free;
    FExportando := false;
    PosRolagemVersiculo(nil);
    if assigned(pb) then
      pb.Visible := false;
  end;
end;

procedure TProjeto.ExportarConcordancia(arquivo: string;
  opcoes: TOpcoesExportacao; abbreviation: string);
begin
  ExportarConcordancia(arquivo, nil, opcoes, abbreviation);
end;

procedure TProjeto.ExportarConcordancia(arquivo: string;
  pb: TProgressBar; opcoes: TOpcoesExportacao; abbreviation: string);
var
  Concordancia: TConcordancia;
  entries: TStringList;
  dictionary: TTheWordDictionary;
  rxLemma: IRegex;
  match: IMatch;
  pares: TStringList;
  entry, strong, lemma: string;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  try
    if assigned(pb) then
    begin
      pb.Position := 0;
      pb.Min := 0;
      pb.Max := QLinhas[FEscopo] + QStrongs[etONT]; // versículos do NT + qtde de strongs
      pb.Step := 1;
      pb.Visible := true;
    end;

    Concordancia := TConcordancia.Create;
    Concordancia.Detalhada := oeConcordDetalhada in opcoes;

    dictionary := TTheWordDictionary.Create(arquivo, true);
    with dictionary do
    begin
      Title        := format('%s %s', [abbreviation, IfThen(oeConcordDetalhada in opcoes, SAnalyticalConcordance, SSyntheticConcordance)]);
      TitleEnglish := format('%s %s', [abbreviation, IfThen(oeConcordDetalhada in opcoes, 'Analytical Concordance', 'Synthetic Concordance')]);
      Language     := SLanguageId;
      Description  := format('%s %s', [abbreviation, IfThen(oeConcordDetalhada in opcoes, SAnalyticalConcordance, SSyntheticConcordance)]);
      EnglishDescription := format('%s %s', [abbreviation, IfThen(oeConcordDetalhada in opcoes, 'Analytical Concordance', 'Synthetic Concordance')]);
      VersionDate  := FormatDateTime('YYYY-MM-DD', Now);
      Abbrev       := abbreviation;
    end;

    FExportando := true;

    StartScrollingSession;
    VersiculoInicial;

    while not FTblPares.EOF do
    begin
      if length(FTblPares.FieldByName('pare_pares').AsString) > 0 then
      begin
        FATmpVerse[tbOrigem ].Texto := FTblPares.Fields[FACamposTexto[tbOrigem ]].AsString;
        FATmpVerse[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
        try
          FATmpVerse[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;
        except
          on E: Exception do
            MessageDlg(SError, SCorruptedData + #13#10#13#10 + FormattedReference + #13#10 +
                 format('%s'#13#10'pares: %s'#13#10'%s'#13#10'%s',
                        [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                        FATmpVerse[tbOrigem].DebugTokens, FATmpVerse[tbDestino].DebugTokens]),
                 mtError, [mbOK], 0);
        end;
        pares := FATmpVerse[tbOrigem].GetListaPares(plAll);
        Concordancia.AdicionarLocucao(pares, GetID);
        pares.Free;
      end;
      if assigned(pb) then
      begin
        pb.StepIt;
        if (pb.Position mod 20) = 0 then
          Application.ProcessMessages;
      end;
      VersiculoSeguinte;
    end;

    entries := TStringList.Create;
    entries.LoadFromFile('lemmas.dat');
    rxLemma := RegexCreate('^(.*?),(.*?)$', [rcoUTF8]);
    for entry in entries do
    begin
      match := rxLemma.Match(entry);

      if match.Success then
      begin
        strong := match.Groups[1].Value;
        lemma  := match.Groups[2].Value;
        dictionary.AddEntry(strong, lemma, Concordancia.GetStrongRTF(lemma, strong));
      end;

      if assigned(pb) then
      begin
        pb.StepIt;
        if (pb.Position mod 20) = 0 then
          Application.ProcessMessages;
      end;
    end;
    entries.Free;

    dictionary.Commit;
  finally
    //ShowMessage('Position: ' + IntToStr(pb.Position));
    Application.ProcessMessages;
    Concordancia.Free;
    dictionary.Free;
    FExportando := false;
    FinishScrollingSession;
    PosRolagemVersiculo(nil);
    if assigned(pb) then
      pb.Visible := false;
  end;
end;

procedure TProjeto.AtribuirFonteTexto(fonte: TFont; textos: STextosBiblicos);
var
  t: TTipoTextoBiblico;
begin
  for t:=tbOrigem to tbConsulta2 do
    if t in textos then
    begin
      FAVersiculo[t].Fonte := fonte;
      AtribuirInfo(format('fonte%d.nome', [t]), fonte.Name);
      AtribuirInfo(format('fonte%d.tamanho', [t]), IntToStr(fonte.Size));
      AtribuirInfo(format('fonte%d.bold', [t]), IntToStr(Ord(fonte.Bold)));
    end;
end;

procedure TProjeto.LimparTexto(texto: TTipoTextoBiblico);
begin
  with FTblPares do
  begin
    ScrollEventsEnabled := false;
    VersiculoInicial;
    while not FTblPares.EOF do
    begin
      SetVerseText('', texto, false);
      VersiculoSeguinte;
    end;
    ScrollEventsEnabled := true;
  end;
  SetTextDescription(texto, '');
end;

function TProjeto.ObterFonteTexto(texto: TTipoTextoBiblico): TFont;
begin
   if assigned(FAVersiculo[texto]) then
     result := FAVersiculo[texto].Fonte
   else
     result := nil;
end;

procedure TProjeto.AtribuirDicStrong(dic: string; textos: STextosBiblicos);
var
  t: TTipoTextoBiblico;
begin
  for t:=tbOrigem to tbConsulta2 do
  begin
    if t in textos then
    begin
      AtribuirDicStrong(dic, t);
      AtribuirInfo(format('dicstrong%d', [t]), dic);
    end;
  end;
end;

procedure TProjeto.AtribuirDicMorfo(dic: string; textos: STextosBiblicos);
var
  t: TTipoTextoBiblico;
begin
  for t:=tbOrigem to tbConsulta2 do
  begin
    if t in textos then
    begin
      AtribuirDicMorfo(dic, t);
      AtribuirInfo(format('dicmorfo%d', [t]), dic);
    end;
  end;
end;

procedure TProjeto.AtribuirInfo(info, valor: string);
begin
  if not AtualizarInfo(info, valor) then
    InserirInfo(info, valor);
end;

function TProjeto.ObterInfo(info: string): string;
begin
  result := ResgatarInfo(info);
end;

function TProjeto.ObterTextoVersiculo(texto: TTipoTextoBiblico): string;
begin
  result := FTblPares.Fields[FACamposTexto[texto]].AsString;
end;

function TProjeto.ObterTextoVersiculo(Referencia: string; texto: TTipoTextoBiblico): string;
begin
  result := SVerseAbsentFromProject;
  StartScrollingSession;
  try
    IrPara(Referencia);
    if ID = Referencia then
      result := ObterTextoVersiculo(texto);
  finally
    FinishScrollingSession;
  end;
end;

function TProjeto.ObterTextoSimplesVersiculo(texto: TTipoTextoBiblico): string;
begin
  result := ObterTextoSimplesVersiculo('', texto);
end;

function TProjeto.ObterTextoSimplesVersiculo(Referencia: string;
  texto: TTipoTextoBiblico): string;
var
  varredorXML: TONTTokenizer;
  s: TTagSintagma;
begin
  result := FAVersiculo[texto].TextoSimples;
  exit;
  result := '';
  varredorXML := TONTTokenizer.Criar(FTblPares.Fields[FACamposTexto[texto]].AsString);
  while varredorXML.LerSintagma(s) <> tsNulo do
  begin
    if s.tipo = tsTag then
    begin
      if s.valor = '<RF>' then
      begin
        varredorXML.LerAteTag(s, '<Rf>');
        s.tipo := tsMetaDado;
        continue;
      end else if AnsiStartsStr('<TS', s.valor) then
      begin
        varredorXML.LerAteTag(s, '<Ts>');
        s.tipo := tsMetaDado;
        continue;
      end;
    end;
    if s.tipo in [tsEspaco, tsSintagma, tsPontuacao] then
      result := result + s.valor;
  end;
  varredorXML.Destruir;
end;

function TProjeto.GetTranslationSuggestions(syntagm: TSyntagm): string;
begin
  result := FSugeridor.GetTranslationAlternatives(syntagm);
end;

procedure TProjeto.Translate;
var
  l: integer;
  node: TTreeNode;
begin
  if not assigned(FArvore) then
    exit;
  node := FArvore.Items[0];
  for l:=0 to QLivros[FEscopo]-1 do
  begin
    node.Text := NLivros[FEscopo][l];
    node := node.GetNextSibling;
  end;
end;

procedure TProjeto.ToggleDisplayTags;
begin
  SetDisplayTags(not DisplayTags);
end;

procedure TProjeto.StartScrollingSession;
begin
  FMarker := ID;
  PreRolagemVersiculo(nil);
  ScrollEventsEnabled := false;
end;

procedure TProjeto.FinishScrollingSession;
begin
  IrPara(FMarker);
  ScrollEventsEnabled := true;
end;

procedure TProjeto.RewindChapter;
begin
  if Verse <> 1 then
    IrPara(Format('%d,%d,%d', [BookID, Chapter, 1]));
end;

end.
