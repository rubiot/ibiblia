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
  MemoVersiculo, ONTTokenizer, unitskins, Dialogs, dos, PCRE, ExportarProjeto, LazLogger,
  Syntagm, MySwordModule, TheWordDictionary, fgl, PatchFile, LCLIntf, menus, zstream;

type

  TSearchScope = (ssDestination, ssSource, ssRef1, ssRef2, ssStrongs, ssComments, ssAll);

  TVerseSearchResult = record
    RefID: string;
    FormattedRef: string;
    PreviewText: string;
    Status: Integer;
  end;

  TVerseSearchResults = array of TVerseSearchResult;

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
    FLoadedVerseID: string;
    FLastSavedVerseID: string;
    FScrollingSessionDepth: Integer;
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
    FUseGlobalDicts: Boolean;
    FGlobalDicStrongGreekPath: string;
    FGlobalDicStrongHebrewPath: string;
    FGlobalDicMorfoPath: string;
    FGlobalDicEnglishPath: string;
    FGlobalDicStrongGreek: TSQLQuery;
    FGlobalDicStrongHebrew: TSQLQuery;
    FGlobalDicMorfo: TSQLQuery;
    FGlobalDicEnglish: TSQLQuery;
    FUseDefaultDict: array[tbOrigem..tbConsulta2] of Boolean;
    FSugeridor: TGerSugestoes;
    FParesAntigos: TStringList;
    FArvore: TTreeView;
    FBibleTreePointedNode: TTreeNode;
    FMemoComentarios: TMemo;
    FRadioGroupSituacao: TRadioGroup;
    FTemporizador: TTimer;
    FDestPopupTimer: TTimer;
    FDestPopupSyntagm: TSyntagm;
    FHoveredAssocDestSyntagm: TSyntagm;
    FDestPopupActive: Boolean;
    FAtrasoExibicao: Cardinal;
    FEscopo: TEscopoTexto;
    FPopupTrigger: TPopupTrigger;
    FVerseStrongsCountMode: TStrongsCountMode;
    FOnNovoVersiculo: TOnNovoVersiculoEvents;
    FOnSintagmaClick: TOnSintagmaClickEvent;
    FOnSintagmaMouseEnter: TOnSintagmaClickEvent;
    FClosing: boolean;
    FAutoSave: boolean;
    FFilterStatus: Integer;
    FTotaisSituacao: array[0..3] of Integer;
    procedure SetFilterStatus(AValue: Integer);
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
    function IsValidDicionaryModule(const path: string; const dicType: string): boolean;
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
    function CreateBibleTreeContextMenu: TPopupMenu;
    procedure UpdateReference(ref: string);
    function BibleTreeNodeToReference(const node: TTreeNode): TReference;
    procedure SetSituacaoDescricao(index: Integer; descricao: string);
    function HasSituacaoCustomizada: boolean;
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
    function ObterDefinicaoPalavraIngles(const wordText: string; texto: TTipoTextoBiblico; out matchedSubject: string): string;
    function FormatRTF(rtf: string): string;
    procedure SetVerseText(versiculo: string; texto: TTipoTextoBiblico; replace: boolean);
    procedure PreRolagemVersiculo(DataSet: TDataSet);
    procedure PosRolagemVersiculo(DataSet: TDataSet);
    procedure SalvarPares;
    procedure SalvarTexto;
    procedure SalvarAssociacoes;
    procedure HealMisplacedPairsInDatabase;
    procedure RemoverAssociacoes;
    procedure SintagmaOnMouseEnter(Sender: TSyntagm);
    procedure SintagmaOnMouseLeave(Sender: TSyntagm);
    procedure SintagmaOnClick(Sender: TSyntagm);
    procedure OnExportText(Sender: TVersiculo);
    procedure OnExportBibleModule(Sender: TVersiculo);
    procedure OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
    procedure OnAlterarTextoVersiculo(Sender: TMemoVersiculo);
    procedure OnLivePreviewVersiculo(Sender: TMemoVersiculo);
    procedure OnTrocarSecaoEdit(SectionIndex: Integer);
    procedure OnDestinoStatusChange(Sender: TVersiculo; NewStatus: Integer);
    procedure OnVerseNavigate(Sender: TVersiculo; Delta: Integer);
    function GetSelectedUnassociatedSourceSyntagm: TSyntagm;
    function IsMouseInDestinationPanel(out CursorPos: TPoint): Boolean;
    procedure PopulatePopupForSyntagm(s: TSyntagm; v: TTipoTextoBiblico);
    procedure UpdateDestinationPopup;
    procedure OnDestPopupTimer(Sender: TObject);
    procedure OnExibirDefinicao(Sender: TObject);
    procedure OnDblClickVersiculo(Sender: TObject);
    procedure OnVerseViewModeChange(Sender: TVersiculo; NewMode: TVerseViewMode);
    procedure OnBibleTreePopupItemClick(Sender: TObject);
    procedure OnBibleTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    function AtribuirDicStrong(dic: string; t: TTipoTextoBiblico; alertaSeNaoExistir: Boolean = True): boolean;
    function AtribuirDicMorfo(dic: string; t: TTipoTextoBiblico; alertaSeNaoExistir: Boolean = True): boolean;
    procedure AtualizarArvore;
    procedure AtualizarArvore(id: string);
    procedure LiberarObjetoQuery(var qry: TSQLQuery);
    function DescompactarDados(AStream: TStream): string;
    function ObterTextoDoCampo(AField: TField): string;
    function BuscarDefinicaoStrongNaQuery(qry: TSQLQuery; const strongCode: string; isHebrew: Boolean): string;
    function BuscarDefinicaoMorfoNaQuery(qry: TSQLQuery; const morphCode: string): string;
    function BuscarDefinicaoPalavraInglesNaQuery(qry: TSQLQuery; const wordText: string; out matchedSubject: string): string;
    function SanitizarFontesRTF(const rtf: string): string;
    function ObterPalavrasBiblia(v: TVersiculo): string;
    function GetUseDefaultDict(texto: TTipoTextoBiblico): Boolean;
    procedure SetUseDefaultDict(texto: TTipoTextoBiblico; AValue: Boolean);
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
    procedure GoToReference(reference: string);
    procedure GoToReference(reference: TReference);
    procedure VersiculoSeguinte;
    procedure VersiculoAnterior;
    procedure VersiculoInicial;
    procedure VersiculoFinal;
    procedure Atualizar;
    procedure ApplySkin(const ASkin: TSkinColors);
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
    function AtribuirDicStrong(dic: string; textos: STextosBiblicos): boolean;
    function AtribuirDicMorfo(dic: string; textos: STextosBiblicos): boolean;
    procedure ConfigurarDicionariosGlobais(useGlobal: Boolean; const greekPath, hebrewPath, morphPath: string; const englishPath: string = '');
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
    function GetStatus(ref: string): Integer;
    procedure SetTextDescription(text: TTipoTextoBiblico; description: string);
    function GetTextDescription(text: TTipoTextoBiblico): string;
    function GetTranslationName(texto: TTipoTextoBiblico): string;
    procedure AtualizarNomesTraducao;
    function GetPropriedadesKey(t: TTipoTextoBiblico): string;
    procedure Vacuum;
    function GetSituacaoDescricao(index: Integer): string;
    procedure SyncDestinoStatusBar;
    function HasPendingAutoSelectNext: Boolean;
    procedure CompletePendingAutoSelectNext;
    procedure UpdateVerseNavHoverStates;
    function HandleMouseWheelForSourceWords(Shift: TShiftState; WheelDelta: Integer): Boolean;
    procedure FiltrarArvore(AStatus: Integer);
    procedure SelectTreeNode(id: string);
    function IrParaVersiculoSeguinteComSituacao(AStatus: Integer): boolean;
    function IrParaVersiculoAnteriorComSituacao(AStatus: Integer): boolean;
    function ContarVersiculosPorSituacao(AStatus: Integer): Integer;
    function ObterTotalVersiculos: Integer;
    function Pesquisar(const Termo: string; EscopoBusca: TSearchScope = ssDestination): TVerseSearchResults;
    function IsSourceSyntagm(Sender: TSyntagm): boolean;

    property FilterStatus: Integer read FFilterStatus write SetFilterStatus;
    property TotalVersiculos: Integer read ObterTotalVersiculos;

    property FileName: string read FFileName;
    property FormattedReference: string read GetFormattedReference;
    property Reference: TReference read FReference;
    property ID: string read GetID;
    property Modificado: boolean read GetModificado;
    property Arvore: TTreeView read FArvore write FArvore;
    property OnAlterarVersiculo: TOnAlterarVersiculoEvent read FOnAlterarVersiculo write SetOnAlterarVersiculo;
    property OnSintagmaClick: TOnSintagmaClickEvent read FOnSintagmaClick write SetOnSintagmaClick;
    property OnSintagmaMouseEnter: TOnSintagmaClickEvent read FOnSintagmaMouseEnter write FOnSintagmaMouseEnter;
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
    property UseGlobalDicts: Boolean read FUseGlobalDicts write FUseGlobalDicts;
    property GlobalDicStrongGreekPath: string read FGlobalDicStrongGreekPath;
    property GlobalDicStrongHebrewPath: string read FGlobalDicStrongHebrewPath;
    property GlobalDicMorfoPath: string read FGlobalDicMorfoPath;
    property GlobalDicEnglishPath: string read FGlobalDicEnglishPath;
    property UseDefaultDict[texto: TTipoTextoBiblico]: Boolean read GetUseDefaultDict write SetUseDefaultDict;
  end;

{$INCLUDE include/bookdefs.inc}

resourcestring

  SStatusNotAssociated = 'Not associated';
  SStatusAssociating = 'Associating';
  SStatusNeedsReview = 'Needs review';
  SStatusAssociated = 'Associated';
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
  SMorphologyDictionary = 'Morphology Dictionary';
  SDictionaryDoesntExist = 'The selected dictionary doesn''t exist: %s';
  SInvalidMorphologyDictionary = 'This is not a morphology dictionary.'#13#10'Please choose a valid morphology dictionary.';
  SInvalidStrongsDictionary = 'This is not a Strong''s dictionary.'#13#10'Please choose a valid Strong''s dictionary.';
  SCompressedDictionary = 'Compressed dictionary';
  SInvalidCompressedDictionary = 'iBiblia does not suppport compressed dictionaries yet.'#13#10'Please choose an uncompressed module.';
  SEncryptedDictionary = 'Encrypted dictionary';
  SInvalidEncryptedDictionary = 'iBiblia does not and will not support encrypted modules.'#13#10'Please choose an unencrypted module.';
  SNonRTFDictionary = 'Non-RTF module';
  SInvalidNonRTFDictionary = 'iBiblia only suppports RTF modules.'#13#10'Please choose another module or convert this module to RTF in theWord.';
  SOpenProject = 'Open project';
  SProjectDoesntExist = 'The selected project doesn''t exist: %s';
  SInvalidLineCount = 'Invalid file, it must have at least %d lines';
  SPotentialNonUTF8ModuleWarning = 'This module has a ''charset'' property and this may be an indication that this is a non-UTF8 module. ' +
                                   'iBiblia was not designed to work with non-UTF8 modules. ' +
                                   'Please choose ''Ignore'' to ignore this warning and continue at your own risk or ''Abort'' ' +
                                   'to abort the operation and choose another module.';
  SFailedToCreateFile = 'Failed to create file: %s';
  SExportToFileError = 'iBiblia found an error while exporting the Bible module file.';
  STranslationMemory = 'Translation memory';
  SExportTextSaveDlgTitle = 'Please choose a destination file';
  SExportBibleModuleSaveDlgTitle = 'Export as Bible module';
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

procedure UpdateDefaultSituacaoDescricao;

implementation

uses formpopup, formverserules;

{ TProjeto }

const
  DefaultSituacaoDescricao: array[0..3] of string = (
    'Not associated',
    'Associating',
    'Needs review',
    'Associated'
  );

procedure UpdateDefaultSituacaoDescricao;
begin
  DefaultSituacaoDescricao[0] := SStatusNotAssociated;
  DefaultSituacaoDescricao[1] := SStatusAssociating;
  DefaultSituacaoDescricao[2] := SStatusNeedsReview;
  DefaultSituacaoDescricao[3] := SStatusAssociated;
end;

function TProjeto.GetSituacaoDescricao(index: Integer): string;
var
  custom: string;
begin
  custom := ObterInfo('status.description.' + IntToStr(index));
  if not custom.IsEmpty then
    Result := custom
  else if (index >= 0) and (index <= 3) then
    Result := DefaultSituacaoDescricao[index]
  else
    Result := '';
end;

procedure TProjeto.SetSituacaoDescricao(index: Integer; descricao: string);
begin
  AtribuirInfo('status.description.' + IntToStr(index), descricao);
  SyncDestinoStatusBar;
end;

procedure TProjeto.SyncDestinoStatusBar;
var
  s0, s1, s2, s3: string;
  curSit: Integer;
begin
  if not Assigned(FAVersiculo[tbDestino]) then Exit;
  if FAtivo and Assigned(FTblPares) and FTblPares.Active then
  begin
    s0 := GetSituacaoDescricao(0);
    s1 := GetSituacaoDescricao(1);
    s2 := GetSituacaoDescricao(2);
    s3 := GetSituacaoDescricao(3);
    curSit := Situacao;
  end
  else
  begin
    s0 := DefaultSituacaoDescricao[0];
    s1 := DefaultSituacaoDescricao[1];
    s2 := DefaultSituacaoDescricao[2];
    s3 := DefaultSituacaoDescricao[3];
    if Assigned(FRadioGroupSituacao) and (FRadioGroupSituacao.ItemIndex >= 0) then
      curSit := FRadioGroupSituacao.ItemIndex
    else
      curSit := 0;
  end;
  FAVersiculo[tbDestino].SetStatusLabels(s0, s1, s2, s3);
  FAVersiculo[tbDestino].StatusIndex := curSit;
end;

function TProjeto.HasSituacaoCustomizada: boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to 3 do
    if not ObterInfo('status.description.' + IntToStr(i)).IsEmpty then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TProjeto.Novo(nomedb, descricao: string);
begin
  Novo(ProjetoModelo[FEscopo], nomedb, descricao);
end;

procedure TProjeto.Novo(nomedbvelho, nomedbnovo, descricao: string);
begin
  CopiarArquivo(nomedbvelho, nomedbnovo);

  Abrir(nomedbnovo);

  AtribuirInfo('descricao', descricao);
  // new projects always use the latest tokenizer version
  AtribuirInfo('tokenizer_version', IntToStr(TTokenizerFactory.LATEST_VERSION));
  //AtribuirInfo('marcador', format('%d,1,1', [OffsetLivros[FEscopo] + 1]));
  FExibirDefComCtrl := false;  //FTblInfo.ExecuteDirect('COMMIT;');
  VersiculoInicial;
  Commit;
end;

function TProjeto.GetFormattedReference: string;
var
  curId: string;
  b, c, v, bIdx: smallint;
begin
  if Assigned(FTblPares) and FTblPares.Active and (not FScrollEventsEnabled) then
  begin
    curId := FTblPares.FieldByName('pare_id').AsString;
    if (curId <> '') and (SScanf(curId, '%d,%d,%d', [@b, @c, @v]) = 3) then
    begin
      bIdx := b - OffsetLivros[FEscopo] - 1;
      if (bIdx >= 0) and (bIdx < QLivros[FEscopo]) then
      begin
        Result := Format('%s %d:%d', [NLivros[FEscopo][bIdx], c, v]);
        Exit;
      end;
    end;
  end;
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
    GoToReference(ref);
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

function TProjeto.IsValidDicionaryModule(const path: string; const dicType: string): boolean;
var
  strong, morpho, encrypted, nonrtf: boolean;
begin
  result := false;
  strong := false;
  morpho := false;
  nonrtf := false;
  encrypted := false;

  with CriarObjetoQuery(path) do
  begin
    SQL.Text := 'select distinct * from config where name in (''strong'',''morph'',''secure'',''do.encrypt'',''content.type'')';
    Prepare;
    Open;
    try
      while not EOF do
      begin
        case FieldByName('name').AsString of
          'strong':
            strong := FieldByName('value').AsString = '1';
          'morph':
            morpho := FieldByName('value').AsString = '1';
          'secure','do.encrypt':
            encrypted := FieldByName('value').AsString = '1';
          'content.type':
            nonrtf := FieldByName('value').AsString.ToLower <> 'rtf';
        end;
        Next;
      end;
    finally
      Close;
      Free;
    end;
  end;

  if (dicType = 'morph') and not morpho then
  begin
    MessageDlg(SMorphologyDictionary, format(SInvalidMorphologyDictionary, [FExpand(path)]), mtError, [mbOK], 0);
    exit;
  end else if (dicType = 'strong') and not strong then
  begin
    MessageDlg(SStrongDictionary, format(SInvalidStrongsDictionary, [FExpand(path)]), mtError, [mbOK], 0);
    exit;
  end;

  if encrypted then
  begin
    MessageDlg(SEncryptedDictionary, format(SInvalidEncryptedDictionary, [FExpand(path)]), mtError, [mbOK], 0);
    exit;
  end;

  if nonrtf then
  begin
    MessageDlg(SNonRTFDictionary, format(SInvalidNonRTFDictionary, [FExpand(path)]), mtError, [mbOK], 0);
    exit;
  end;

  result := true;
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
      GoToReference(patch.Reference[i]);

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

      if (i mod 100) = 0 then
      begin
        if FTblPares.UpdatesPending then
        begin
          try
            FTblPares.ApplyUpdates;
          except
            on E: Exception do
              DebugLn('ApplyUpdates batch warning at ' + IntToStr(i) + ': ' + E.Message);
          end;
        end;
      end;

      DebugLn('%s comments: "%s" situacao: %s', [ID, FTblPares.FieldByName('pare_comentarios').AsString, FTblPares.FieldByName('pare_situacao').AsString]);
    end;
    if FTblPares.UpdatesPending then
    begin
      try
        FTblPares.ApplyUpdates;
      except
        on E: Exception do
          DebugLn('AplicarPatch final warning: ' + E.Message);
      end;
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
var
  oldVal, newVal: Integer;
begin
  if FClosing or (FTblPares.FieldByName('pare_situacao').AsInteger = AValue) then
    exit;

  oldVal := FTblPares.FieldByName('pare_situacao').AsInteger;
  newVal := IfThen((AValue < 0) or (AValue > 3), 1, AValue);

  FTblPares.Edit;
  FTblPares.FieldByName('pare_situacao').AsInteger := newVal;
  FTblPares.Post;

  if (oldVal >= 0) and (oldVal <= 3) then
    dec(FTotaisSituacao[oldVal]);
  if (newVal >= 0) and (newVal <= 3) then
    inc(FTotaisSituacao[newVal]);

  if Assigned(FAVersiculo[tbDestino]) then
    FAVersiculo[tbDestino].StatusIndex := newVal;
  if Assigned(FRadioGroupSituacao) and (FRadioGroupSituacao.ItemIndex <> newVal) then
    FRadioGroupSituacao.ItemIndex := newVal;

  AtualizarArvore(ID);
end;

procedure TProjeto.ExportTheWordBible(verses: TStringList; filename: string;
  props: string);
var
  stagedFile: string;
  stream: TFileStream;
  propLines: TStringList;
  i: Integer;
begin
  if (verses.Count > 0) and not verses[0].StartsWith(#239#187#191) then
    verses[0] := #239#187#191 + verses[0]; // adicionando BOM

  if props <> '' then
  begin
    propLines := TStringList.Create;
    try
      propLines.Text := props;
      for i := 0 to propLines.Count - 1 do
        verses.Add(propLines[i]);
    finally
      propLines.Free;
    end;
  end;

  stagedFile := filename + '.tmp_export';
  if FileExists(stagedFile) then
    SysUtils.DeleteFile(stagedFile);

  stream := TFileStream.Create(stagedFile, fmCreate or fmOpenWrite or fmShareDenyNone);
  try
    stream.Size := 0;
    verses.SaveToStream(stream);
  finally
    stream.Free;
  end;

  if not PublishStagedFile(stagedFile, filename) then
    raise Exception.CreateFmt('Unable to publish exported Bible module to "%s".', [filename]);
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

function TProjeto.CreateBibleTreeContextMenu: TPopupMenu;

  procedure AddSubItemForStatus(item: TMenuItem; status: Integer);
  var
    subitem: TMenuItem;
  begin
    subitem := TMenuItem.Create(item);
    with subitem do
    begin
      Caption := GetSituacaoDescricao(status);
      OnClick := @OnBibleTreePopupItemClick;
      ImageIndex := status;
      Tag := status;
    end;
    item.Add(subitem);
  end;

var
  item: TMenuItem;
  status: Integer;
begin
  result := TPopupMenu.Create(FArvore);
  result.Parent := FArvore;
  result.Images := FArvore.Images;

  item := TMenuItem.Create(result);
  item.Caption := 'Set status of all children to...';
  result.Items.Add(item);
  for status:=0 to 3 do
    AddSubItemForStatus(item, status);
end;

procedure TProjeto.UpdateReference(ref: string);
begin
  Sscanf(ref, '%d,%d,%d', [@FReference.BookID, @FReference.Chapter, @FReference.Verse]);
  FReference.Book := NLivros[FEscopo][FReference.BookID-OffsetLivros[FEscopo]-1];
end;

function TProjeto.BibleTreeNodeToReference(const node: TTreeNode): TReference;
var
  ch, bk, vs: TTreeNode;
  i: Integer;
begin
  if node.HasChildren then // node is either a chapter or a book
  begin
    if assigned(node.Parent) then // chapter
    begin
      ch := node;
      bk := ch.Parent;
      vs := ch.GetFirstChild;
    end else begin // book
      bk := node;
      ch := bk.GetFirstChild;
      vs := ch.GetFirstChild;
    end;
  end else begin // verse
    vs := node;
    ch := vs.Parent;
    bk := ch.Parent;
  end;

  for i:=0 to QLivros[FEscopo]-1 do
    if NLivros[FEscopo][i] = bk.Text then
      break;

  result.BookID  := i+1+OffsetLivros[FEscopo];
  result.Book    := NLivrosONT[result.BookID];
  result.Chapter := StrToInt(ch.Text);
  result.Verse   := StrToInt(vs.Text);
  //GoToReference(format('%d,%s,%s', [i+1+OffsetLivros[FEscopo], c.Text, v.Text]));
end;

function TProjeto.GetModificado: boolean;
var
  pairsPending, infoPending, srcPending, propsPending: boolean;
  t: TTipoTextoBiblico;
begin
  if not Ativo then
  begin
    result := false;
    exit;
  end;
  pairsPending := assigned(FTblPares) and FTblPares.Active and FTblPares.UpdatesPending;
  infoPending  := assigned(FTblInfo) and FTblInfo.Active and FTblInfo.UpdatesPending;
  srcPending   := (assigned(FAVersiculo[tbOrigem]) and FAVersiculo[tbOrigem].Modificado);
  propsPending := false;
  for t := low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[t]) and FAVersiculo[t].MemoPropsModified then
    begin
      propsPending := true;
      break;
    end;

  result := pairsPending or infoPending or srcPending or propsPending;
end;

function TProjeto.GetCaminho: string;
begin
  result := '';
  if assigned(FTblInfo) and FTblInfo.Active then
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
var
  savedMarker: string;

  function GetText(rulesRx: TRegexList; replaceTo: TStringList): string;
  var
    i: integer;
    srcTxt, dstTxt, rawPairs: string;
  begin
    case ChapterViewText of
      tbOrigem, tbConsulta1, tbConsulta2:
      begin
        if (GetID() = savedMarker) and Assigned(FMemoVersiculo) and FMemoVersiculo.Ativo and
           (FAVersiculo[ChapterViewText] = FMemoVersiculo.Versiculo) then
          result := string(FMemoVersiculo.Texto).Replace(#239#187#191, '')
        else
          result := FTblPares.Fields[FACamposTexto[ChapterViewText]].AsString.Replace(#239#187#191, '');
      end;
      tbDestino:
      begin
        if (GetID() = savedMarker) and Assigned(FMemoVersiculo) and FMemoVersiculo.Ativo and
           (FAVersiculo[tbDestino] = FMemoVersiculo.Versiculo) then
          result := string(FMemoVersiculo.Texto).Replace(#239#187#191, '')
        else
        begin
          srcTxt := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString.Replace(#239#187#191, '');
          dstTxt := FTblPares.Fields[FACamposTexto[tbDestino]].AsString.Replace(#239#187#191, '');
          rawPairs := FTblPares.FieldByName('pare_pares').AsString;
          if (Length(rawPairs) = 0) and (FTblPares.FieldByName('pare_situacao').AsInteger = 0) and
             ((FLastSavedVerseID = '') or (GetID() <> FLastSavedVerseID)) then
            result := dstTxt
          else
          begin
            FATmpVerse[tbOrigem ].Texto := srcTxt;
            FATmpVerse[tbDestino].Texto := dstTxt;
            if Length(rawPairs) > 0 then
            begin
              try
                if FATmpVerse[tbOrigem].AreParesValidForVerse(rawPairs) then
                  FATmpVerse[tbOrigem].Pares := rawPairs;
              except
              end;
            end;
            result := FATmpVerse[tbDestino].GetLinhaONT(true, true, false, false);
          end;
        end;
      end;
      tbInterlinear:
      begin
        srcTxt := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString;
        dstTxt := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
        if (GetID() = savedMarker) and Assigned(FMemoVersiculo) and FMemoVersiculo.Ativo then
        begin
          if FMemoVersiculo.Versiculo = FAVersiculo[tbOrigem] then
            srcTxt := string(FMemoVersiculo.Texto)
          else if FMemoVersiculo.Versiculo = FAVersiculo[tbDestino] then
            dstTxt := string(FMemoVersiculo.Texto);
        end;

        if length(FTblPares.FieldByName('pare_pares').AsString) = 0 then
          result := srcTxt
        else
        begin
          FATmpVerse[tbOrigem ].Texto := srcTxt;
          FATmpVerse[tbDestino].Texto := dstTxt;
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
  wasEditing: Boolean;
begin
  bkch := Format('%d,%d,', [BookID, Chapter]);
  result := TStringList.Create;
  savedMarker := ID;

  rulesRx := nil;
  replaceTo := nil;
  if ChapterViewText = tbInterlinear then
    CompileInterlinearVerseRules(rulesRx, replaceTo, InterlinearMode);

  wasEditing := Assigned(FMemoVersiculo) and FMemoVersiculo.Ativo;
  if wasEditing then
  begin
    FMarker := ID;
    ScrollEventsEnabled := false;
  end
  else
    StartScrollingSession;

  RewindChapter;
  with FTblPares do
    while not FTblPares.EOF and GetID().StartsWith(bkch) do
    begin
      comments := IfThen(
        ChapterViewText = tbDestino,
        Comentarios
          .Replace(#13#10, '<br/>', [rfReplaceAll])
          .Replace(#13, '<br/>', [rfReplaceAll])
          .Replace(#10, '<br/>', [rfReplaceAll]),
        ''
      );
      result.Add(Format('%s%s', [GetText(rulesRx, replaceTo), IfThen(comments.IsEmpty, '', Format('<RF>%s<Rf>', [comments]))]));
      VersiculoSeguinte;
    end;

  if wasEditing then
  begin
    FTblPares.Locate('pare_id', savedMarker, []);
    ScrollEventsEnabled := true;
  end
  else
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
    GoToReference(ref);
    if ID = ref then
      result := GetComentarios;
  finally
    FinishScrollingSession;
  end;
end;

function TProjeto.GetStatus(ref: string): Integer;
begin
  result := 0;
  StartScrollingSession;
  try
    GoToReference(ref);
    if ID = ref then
      result := GetSituacao;
  finally
    FinishScrollingSession;
  end;
end;

procedure TProjeto.SetTextDescription(text: TTipoTextoBiblico;
  description: string);
begin
  AtribuirInfo(Format('description.text%d', [Integer(text)]), description);
  AtualizarNomesTraducao;
end;

function TProjeto.GetTextDescription(text: TTipoTextoBiblico): string;
begin
  result := ObterInfo(Format('description.text%d', [text]));
end;

function TProjeto.GetPropriedadesKey(t: TTipoTextoBiblico): string;
begin
  case t of
    tbOrigem:    Result := 'propriedades.origem';
    tbDestino:   Result := 'propriedades.destino';
    tbConsulta1: Result := 'propriedades.consulta1';
    tbConsulta2: Result := 'propriedades.consulta2';
  else
    Result := Format('propriedades.%d', [Ord(t)]);
  end;
end;

function TProjeto.GetTranslationName(texto: TTipoTextoBiblico): string;
var
  s, props, line: string;
  lines: TStringList;
  i, eqPos: Integer;
  k, v: string;
begin
  Result := '';

  if (not Ativo) or (not assigned(FTblInfo)) or (not FTblInfo.Active) then
  begin
    case texto of
      tbOrigem:    Result := 'Source Bible';
      tbDestino:   Result := 'Destination Bible';
      tbConsulta1: Result := 'Reference 1';
      tbConsulta2: Result := 'Reference 2';
    else
      Result := '';
    end;
    exit;
  end;

  // 1. Check properties block for title= or short.title= or description=
  props := ObterInfo(GetPropriedadesKey(texto));
  if props = '' then
  begin
    case texto of
      tbOrigem:    props := ObterInfo('propriedades.origem');
      tbDestino:   props := ObterInfo('propriedades.destino');
      tbConsulta1: props := ObterInfo('propriedades.consulta1');
      tbConsulta2: props := ObterInfo('propriedades.consulta2');
    end;
  end;

  if props <> '' then
  begin
    lines := TStringList.Create;
    try
      lines.Text := props;
      // Look for title= first
      for i := 0 to lines.Count - 1 do
      begin
        line := Trim(lines[i]);
        eqPos := Pos('=', line);
        if eqPos > 0 then
        begin
          k := LowerCase(Trim(Copy(line, 1, eqPos - 1)));
          v := Trim(Copy(line, eqPos + 1, MaxInt));
          if (k = 'title') and (v <> '') then
          begin
            Result := v;
            exit;
          end;
        end;
      end;
      // Look for short.title=
      for i := 0 to lines.Count - 1 do
      begin
        line := Trim(lines[i]);
        eqPos := Pos('=', line);
        if eqPos > 0 then
        begin
          k := LowerCase(Trim(Copy(line, 1, eqPos - 1)));
          v := Trim(Copy(line, eqPos + 1, MaxInt));
          if (k = 'short.title') and (v <> '') then
          begin
            Result := v;
            exit;
          end;
        end;
      end;
      // Look for description=
      for i := 0 to lines.Count - 1 do
      begin
        line := Trim(lines[i]);
        eqPos := Pos('=', line);
        if eqPos > 0 then
        begin
          k := LowerCase(Trim(Copy(line, 1, eqPos - 1)));
          v := Trim(Copy(line, eqPos + 1, MaxInt));
          if (k = 'description') and (v <> '') then
          begin
            Result := v;
            exit;
          end;
        end;
      end;
    finally
      lines.Free;
    end;
  end;

  // 2. Check description.text%d
  s := Trim(GetTextDescription(texto));
  if s <> '' then
  begin
    Result := s;
    exit;
  end;

  // 3. Check module file name
  s := ObterInfo(Format('modulepath.%d', [Ord(texto)]));
  if s = '' then
  begin
    case texto of
      tbOrigem:    s := ObterInfo('modulepath.origem');
      tbDestino:   s := ObterInfo('modulepath.destino');
      tbConsulta1: s := ObterInfo('modulepath.consulta1');
      tbConsulta2: s := ObterInfo('modulepath.consulta2');
    end;
  end;
  if s <> '' then
  begin
    Result := ChangeFileExt(ExtractFileName(s), '');
    exit;
  end;

  // 4. Default section fallback
  case texto of
    tbOrigem:    Result := 'Source Bible';
    tbDestino:   Result := 'Destination Bible';
    tbConsulta1: Result := 'Reference 1';
    tbConsulta2: Result := 'Reference 2';
  else
    Result := '';
  end;
end;

procedure TProjeto.AtualizarNomesTraducao;
var
  t: TTipoTextoBiblico;
begin
  for t := low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[t]) then
      FAVersiculo[t].TranslationName := GetTranslationName(t);
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
  result := false;
  if not assigned(FTblInfo) or not FTblInfo.Active then
    exit;

  if FTblInfo.Locate('id', info, []) then
    exit;

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
  result := false;
  if not assigned(FTblInfo) or not FTblInfo.Active then
    exit;

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
  if assigned(FTblInfo) and FTblInfo.Active and FTblInfo.Locate('id', info, []) then
    result := FTblInfo.Fields[1].AsString;
  {result := FTblInfo.QuickQuery(format('select valor from info where id = ''%s''', [AnsiReplaceStr(info, '''', '''''')]));}
end;

procedure TProjeto.LiberarObjetoQuery(var qry: TSQLQuery);
var
  conn: TSQLite3Connection;
  trans: TSQLTransaction;
begin
  if Assigned(qry) then
  begin
    try
      qry.Close;
    except
    end;
    if Assigned(qry.DataBase) then
    begin
      conn := TSQLite3Connection(qry.DataBase);
      try
        conn.Close;
      except
      end;
      if Assigned(conn.Transaction) then
      begin
        trans := TSQLTransaction(conn.Transaction);
        conn.Transaction := nil;
        qry.Transaction := nil;
        trans.Free;
      end;
      qry.DataBase := nil;
      conn.Free;
    end;
    qry.Free;
    qry := nil;
  end;
end;

function TProjeto.DescompactarDados(AStream: TStream): string;
var
  decomp: TDecompressionStream;
  outStream: TStringStream;
  buf: array[0..8191] of Byte;
  hdr: array[0..31] of Byte;
  count, hdrLen, ofs: Integer;
begin
  Result := '';
  if (AStream = nil) or (AStream.Size = 0) then
    Exit;

  AStream.Position := 0;
  outStream := TStringStream.Create('');
  try
    try
      // 1. Standard zlib stream decompression (with header)
      decomp := TDecompressionStream.Create(AStream, False);
      try
        repeat
          count := decomp.Read(buf, SizeOf(buf));
          if count > 0 then
            outStream.WriteBuffer(buf, count);
        until count = 0;
        Result := outStream.DataString;
      finally
        decomp.Free;
      end;
    except
      // 2. If standard zlib fails, try raw deflate stream (skipHeader = True)
      outStream.Size := 0;
      AStream.Position := 0;
      try
        decomp := TDecompressionStream.Create(AStream, True);
        try
          repeat
            count := decomp.Read(buf, SizeOf(buf));
            if count > 0 then
              outStream.WriteBuffer(buf, count);
          until count = 0;
          Result := outStream.DataString;
        finally
          decomp.Free;
        end;
      except
        Result := '';
      end;
    end;

    // 3. If both failed, check if a small header precedes the zlib magic bytes ($78 $9C / $78 $DA / $78 $01 / $78 $5E)
    if (Result = '') and (AStream.Size > 6) then
    begin
      AStream.Position := 0;
      hdrLen := AStream.Read(hdr, SizeOf(hdr));
      for ofs := 1 to hdrLen - 2 do
      begin
        if (hdr[ofs] = $78) and (hdr[ofs + 1] in [$01, $5E, $9C, $DA]) then
        begin
          outStream.Size := 0;
          AStream.Position := ofs;
          try
            decomp := TDecompressionStream.Create(AStream, False);
            try
              repeat
                count := decomp.Read(buf, SizeOf(buf));
                if count > 0 then
                  outStream.WriteBuffer(buf, count);
              until count = 0;
              Result := outStream.DataString;
            finally
              decomp.Free;
            end;
          except
            Result := '';
          end;
          if Result <> '' then
            Break;
        end;
      end;
    end;
  finally
    outStream.Free;
  end;
end;

function TProjeto.ObterTextoDoCampo(AField: TField): string;
var
  memStream: TMemoryStream;
  rawStr: string;
  b: Byte;
begin
  Result := '';
  if (AField = nil) or AField.IsNull then
    Exit;

  memStream := TMemoryStream.Create;
  try
    if AField is TBlobField then
      TBlobField(AField).SaveToStream(memStream)
    else
    begin
      rawStr := AField.AsString;
      if rawStr <> '' then
        memStream.WriteBuffer(rawStr[1], Length(rawStr));
    end;

    if memStream.Size = 0 then
      Exit;

    memStream.Position := 0;
    memStream.ReadBuffer(b, 1);
    memStream.Position := 0;

    // Check if it already begins with '{' or '\' (standard uncompressed RTF)
    if (b = Ord('{')) or (b = Ord('\')) then
    begin
      SetLength(Result, memStream.Size);
      memStream.ReadBuffer(Result[1], memStream.Size);
      Exit;
    end;

    // Attempt decompression
    Result := DescompactarDados(memStream);

    // Fallback if decompression returned empty and data is not a failed zlib stream
    if (Result = '') and (b <> $78) then
    begin
      memStream.Position := 0;
      SetLength(Result, memStream.Size);
      memStream.ReadBuffer(Result[1], memStream.Size);
    end;
  finally
    memStream.Free;
  end;
end;

function TProjeto.BuscarDefinicaoStrongNaQuery(qry: TSQLQuery; const strongCode: string; isHebrew: Boolean): string;
var
  candidates: array of string;
  c, s, digitsOnly, suffix, langPrefix, u1, u2: string;
  i, numVal, slashPos, spacePos: Integer;
  auxQry: TSQLQuery;

  procedure AddCandidate(const cand: string);
  var
    k: Integer;
  begin
    if cand = '' then Exit;
    for k := 0 to High(candidates) do
      if candidates[k] = cand then Exit;
    SetLength(candidates, Length(candidates) + 1);
    candidates[High(candidates)] := cand;
  end;

  function LerCamposDeQry(q: TSQLQuery): string;
  var
    pId, endIdx, targetId: Integer;
    idStr, linkSubj: string;
    linkQry: TSQLQuery;
  begin
    Result := '';
    if (q = nil) or q.EOF then Exit;
    if (q.FieldCount > 0) and (not q.Fields[0].IsNull) then
      Result := ObterTextoDoCampo(q.Fields[0]);
    if (Result = '') and (q.FieldCount > 1) and (not q.Fields[1].IsNull) then
      Result := ObterTextoDoCampo(q.Fields[1]);

    // Follow theWord meta=_twmgc_ redirect/link topics (e.g. Strong number redirecting to lemma/BDB entry)
    if (Result <> '') and ((Pos('meta=_twmgc_', Result) > 0) or (Pos('type=link', Result) > 0)) and
       Assigned(q.DataBase) then
    begin
      pId := Pos('id=', Result);
      if pId > 0 then
      begin
        Inc(pId, 3);
        endIdx := pId;
        while (endIdx <= Length(Result)) and (Result[endIdx] in ['0'..'9']) do
          Inc(endIdx);
        idStr := Copy(Result, pId, endIdx - pId);
        targetId := StrToIntDef(idStr, -1);
        if targetId >= 0 then
        begin
          Result := '';
          linkQry := TSQLQuery.Create(nil);
          try
            linkQry.DataBase := q.DataBase;
            linkQry.Transaction := q.Transaction;
            linkQry.SQL.Text := 'SELECT data, data2 FROM content WHERE topic_id = :tid';
            linkQry.Params.ParamByName('tid').AsInteger := targetId;
            linkQry.Open;
            try
              if not linkQry.EOF then
              begin
                if not linkQry.Fields[0].IsNull then
                  Result := ObterTextoDoCampo(linkQry.Fields[0]);
                if (Result = '') and (linkQry.FieldCount > 1) and (not linkQry.Fields[1].IsNull) then
                  Result := ObterTextoDoCampo(linkQry.Fields[1]);
              end;
            finally
              linkQry.Close;
            end;
          except
            Result := '';
          end;
          linkQry.Free;
        end;
      end
      else
      begin
        pId := Pos('subject=', Result);
        if pId > 0 then
        begin
          Inc(pId, 8);
          endIdx := pId;
          while (endIdx <= Length(Result)) and not (Result[endIdx] in [#13, #10]) do
            Inc(endIdx);
          linkSubj := Trim(Copy(Result, pId, endIdx - pId));
          if linkSubj <> '' then
          begin
            Result := '';
            linkQry := TSQLQuery.Create(nil);
            try
              linkQry.DataBase := q.DataBase;
              linkQry.Transaction := q.Transaction;
              linkQry.SQL.Text := 'SELECT c.data, c.data2 FROM topics t JOIN content c ON t.id = c.topic_id WHERE t.subject = :subj';
              linkQry.Params.ParamByName('subj').AsString := linkSubj;
              linkQry.Open;
              try
                if not linkQry.EOF then
                begin
                  if not linkQry.Fields[0].IsNull then
                    Result := ObterTextoDoCampo(linkQry.Fields[0]);
                  if (Result = '') and (linkQry.FieldCount > 1) and (not linkQry.Fields[1].IsNull) then
                    Result := ObterTextoDoCampo(linkQry.Fields[1]);
                end;
              finally
                linkQry.Close;
              end;
            except
              Result := '';
            end;
            linkQry.Free;
          end;
        end;
      end;
    end;

    if Result <> '' then
      Result := FormatRTF(Result);
  end;

  function Executar(const term: string): string;
  begin
    Result := '';
    try
      qry.Close;
      qry.Params.ParamByName('strong').AsString := term;
      qry.Open;
      try
        if not qry.EOF then
          Result := LerCamposDeQry(qry);
      finally
        qry.Close;
      end;
    except
      Result := '';
    end;
  end;

begin
  Result := '';
  if (qry = nil) or (strongCode = '') then Exit;

  s := Trim(strongCode);
  while (s <> '') and (s[1] in ['<', ' ', '"', '''']) do
    Delete(s, 1, 1);
  while (s <> '') and (s[Length(s)] in ['>', ' ', '"', '''']) do
    Delete(s, Length(s), 1);

  spacePos := Pos(' ', s);
  if spacePos > 1 then
    s := Copy(s, 1, spacePos - 1);

  slashPos := LastDelimiter('/', s);
  if slashPos > 0 then
    s := Copy(s, slashPos + 1, MaxInt);

  if (Length(s) >= 2) and ((UpperCase(Copy(s, 1, 2)) = 'WH') or (UpperCase(Copy(s, 1, 2)) = 'WG')) then
    Delete(s, 1, 1);

  if s = '' then Exit;

  SetLength(candidates, 0);

  // Extract contiguous digits and optional sub-entry letter suffix
  digitsOnly := '';
  suffix := '';
  for i := 1 to Length(s) do
  begin
    if s[i] in ['0'..'9'] then
      digitsOnly := digitsOnly + s[i]
    else if (digitsOnly <> '') and (s[i] in ['a'..'z', 'A'..'Z']) then
      suffix := suffix + s[i];
  end;

  if isHebrew then
    langPrefix := 'H'
  else
    langPrefix := 'G';

  numVal := StrToIntDef(digitsOnly, -1);

  if numVal >= 0 then
  begin
    // 1. If there is a sub-entry letter suffix (e.g. H7225a), try with suffix first
    if suffix <> '' then
    begin
      AddCandidate(langPrefix + IntToStr(numVal) + suffix);
      AddCandidate(LowerCase(langPrefix) + IntToStr(numVal) + suffix);
      AddCandidate(langPrefix + Format('%.4d', [numVal]) + suffix);
      AddCandidate(LowerCase(langPrefix) + Format('%.4d', [numVal]) + suffix);
    end;

    // 2. Unpadded uppercase and lowercase (e.g. H430, h430, H7225, h7225)
    AddCandidate(langPrefix + IntToStr(numVal));
    AddCandidate(LowerCase(langPrefix) + IntToStr(numVal));

    // 3. As-is digits from tag and raw token
    AddCandidate(langPrefix + digitsOnly);
    AddCandidate(LowerCase(langPrefix) + digitsOnly);
    AddCandidate(s);

    // 4. 4-digit zero-padded (e.g. H0430, h0430)
    AddCandidate(langPrefix + Format('%.4d', [numVal]));
    AddCandidate(LowerCase(langPrefix) + Format('%.4d', [numVal]));

    // 5. 5-digit zero-padded (e.g. H00430, H07225)
    AddCandidate(langPrefix + Format('%.5d', [numVal]));
    AddCandidate(LowerCase(langPrefix) + Format('%.5d', [numVal]));

    // 6. Plain numeric (unpadded, raw digits, 4-digit, 5-digit)
    AddCandidate(IntToStr(numVal));
    AddCandidate(digitsOnly);
    AddCandidate(Format('%.4d', [numVal]));
    AddCandidate(Format('%.5d', [numVal]));
  end
  else
    AddCandidate(s);

  for c in candidates do
  begin
    if c <> '' then
    begin
      Result := Executar(c);
      if Result <> '' then Exit;
    end;
  end;

  // Fallback for theWord Lexicon dictionaries (e.g. uBDB, BDB, TWOT, Mickelson)
  // that index Strong numbers in topics_wordindex or prefix them in topics.subject
  if (Result = '') and Assigned(qry.DataBase) and (numVal >= 0) then
  begin
    u1 := langPrefix + IntToStr(numVal);
    u2 := langPrefix + Format('%.4d', [numVal]);

    auxQry := TSQLQuery.Create(nil);
    try
      auxQry.DataBase := qry.DataBase;
      auxQry.Transaction := qry.Transaction;

      // A. Check topics_wordindex
      try
        auxQry.SQL.Text :=
          'SELECT c.data, c.data2 FROM topics_wordindex wi ' +
          'JOIN content c ON c.topic_id = wi.id ' +
          'WHERE LOWER(wi.word) IN (:w1, :w2, :w3) ' +
          'ORDER BY wi.priority ASC, wi.id ASC LIMIT 1';
        auxQry.Params.ParamByName('w1').AsString := LowerCase(u1);
        auxQry.Params.ParamByName('w2').AsString := LowerCase(u2);
        auxQry.Params.ParamByName('w3').AsString := LowerCase(langPrefix + Format('%.5d', [numVal]));
        auxQry.Open;
        try
          if not auxQry.EOF then
            Result := LerCamposDeQry(auxQry);
        finally
          auxQry.Close;
        end;
      except
        Result := '';
      end;

      if Result <> '' then Exit;

      // B. Check topics.subject case-insensitively or with headword suffix (e.g. "H7225 ...", "H7225-...", "%(H7225)%")
      try
        auxQry.SQL.Text :=
          'SELECT c.data, c.data2 FROM topics t ' +
          'JOIN content c ON c.topic_id = t.id ' +
          'WHERE UPPER(t.subject) = :u1 OR UPPER(t.subject) = :u2 ' +
          '   OR UPPER(t.subject) LIKE :p1 OR UPPER(t.subject) LIKE :p2 ' +
          '   OR UPPER(t.subject) LIKE :p3 OR UPPER(t.subject) LIKE :p4 ' +
          'ORDER BY t.id ASC LIMIT 1';
        auxQry.Params.ParamByName('u1').AsString := u1;
        auxQry.Params.ParamByName('u2').AsString := u2;
        auxQry.Params.ParamByName('p1').AsString := u1 + ' %';
        auxQry.Params.ParamByName('p2').AsString := u1 + '-%';
        auxQry.Params.ParamByName('p3').AsString := u1 + ':%';
        auxQry.Params.ParamByName('p4').AsString := '%(' + u1 + ')%';
        auxQry.Open;
        try
          if not auxQry.EOF then
            Result := LerCamposDeQry(auxQry);
        finally
          auxQry.Close;
        end;
      except
        Result := '';
      end;
    finally
      auxQry.Free;
    end;
  end;
end;

function TProjeto.BuscarDefinicaoMorfoNaQuery(qry: TSQLQuery; const morphCode: string): string;
var
  candidates: array of string;
  c, m: string;

  function Executar(const term: string): string;
  begin
    Result := '';
    try
      qry.Close;
      qry.Params.ParamByName('morf').AsString := term;
      qry.Open;
      try
        if not qry.EOF then
        begin
          if not qry.Fields[0].IsNull then
            Result := ObterTextoDoCampo(qry.Fields[0]);
          if (Result = '') and (qry.FieldCount > 1) and (not qry.Fields[1].IsNull) then
            Result := ObterTextoDoCampo(qry.Fields[1]);
          if Result <> '' then
            Result := FormatRTF(Result);
        end;
      finally
        qry.Close;
      end;
    except
      Result := '';
    end;
  end;

begin
  Result := '';
  if (qry = nil) or (morphCode = '') then Exit;

  m := Trim(morphCode);
  if Pos(' ', m) > 1 then
    m := Copy(m, 1, Pos(' ', m) - 1);

  SetLength(candidates, 0);

  // 1. Raw code (e.g. V-AAI-3S or WTV-AAI-3S)
  SetLength(candidates, Length(candidates) + 1);
  candidates[High(candidates)] := m;

  // 2. If starts with 'WT', try without 'WT'
  if (Length(m) > 2) and (UpperCase(Copy(m, 1, 2)) = 'WT') then
  begin
    SetLength(candidates, Length(candidates) + 1);
    candidates[High(candidates)] := Copy(m, 3, Length(m) - 2);
  end
  // 3. If starts with 'T', try without 'T'
  else if (Length(m) > 1) and (UpperCase(Copy(m, 1, 1)) = 'T') and (m[2] in ['V','N','A','R','D','P','C','I','X','Q']) then
  begin
    SetLength(candidates, Length(candidates) + 1);
    candidates[High(candidates)] := Copy(m, 2, Length(m) - 1);
  end
  else
  begin
    // 4. If doesn't start with 'WT', try with 'WT'
    SetLength(candidates, Length(candidates) + 1);
    candidates[High(candidates)] := 'WT' + m;
  end;

  for c in candidates do
  begin
    if c <> '' then
    begin
      Result := Executar(c);
      if Result <> '' then Exit;
    end;
  end;
end;

function TProjeto.GetUseDefaultDict(texto: TTipoTextoBiblico): Boolean;
begin
  if texto in [tbOrigem..tbConsulta2] then
    Result := FUseDefaultDict[texto]
  else
    Result := False;
end;

procedure TProjeto.SetUseDefaultDict(texto: TTipoTextoBiblico; AValue: Boolean);
begin
  if texto in [tbOrigem..tbConsulta2] then
  begin
    FUseDefaultDict[texto] := AValue;
    if AValue then
      AtribuirInfo(format('usedefaultdict%d', [Ord(texto)]), '1')
    else
      AtribuirInfo(format('usedefaultdict%d', [Ord(texto)]), '0');
  end;
end;

function TProjeto.BuscarDefinicaoPalavraInglesNaQuery(qry: TSQLQuery; const wordText: string; out matchedSubject: string): string;
var
  candidates: array of string;
  w, baseW, lw, c: string;
  i: Integer;
  hasLatinLetter, hasNonAscii: Boolean;
  auxQry: TSQLQuery;

  procedure AddCandidate(const cand: string);
  var
    k: Integer;
  begin
    if Length(cand) < 2 then Exit;
    for k := 0 to High(candidates) do
      if SameText(candidates[k], cand) and (candidates[k] = cand) then Exit;
    SetLength(candidates, Length(candidates) + 1);
    candidates[High(candidates)] := cand;
  end;

  function LerCamposDeQryIngles(q: TSQLQuery; const fallbackSubj: string; out outSubj: string): string;
  var
    pId, endIdx, targetId: Integer;
    idStr, linkSubj: string;
    linkQry: TSQLQuery;
  begin
    Result := '';
    outSubj := fallbackSubj;
    if (q = nil) or q.EOF then Exit;

    if (q.FieldCount >= 3) then
    begin
      if not q.Fields[0].IsNull then
        outSubj := Trim(q.Fields[0].AsString);
      if outSubj = '' then
        outSubj := fallbackSubj;
      if not q.Fields[1].IsNull then
        Result := ObterTextoDoCampo(q.Fields[1]);
      if (Result = '') and (not q.Fields[2].IsNull) then
        Result := ObterTextoDoCampo(q.Fields[2]);
    end
    else
    begin
      if (q.FieldCount > 0) and (not q.Fields[0].IsNull) then
        Result := ObterTextoDoCampo(q.Fields[0]);
      if (Result = '') and (q.FieldCount > 1) and (not q.Fields[1].IsNull) then
        Result := ObterTextoDoCampo(q.Fields[1]);
    end;

    // Follow theWord meta=_twmgc_ redirect/link topics
    if (Result <> '') and ((Pos('meta=_twmgc_', Result) > 0) or (Pos('type=link', Result) > 0)) and
       Assigned(q.DataBase) then
    begin
      pId := Pos('id=', Result);
      if pId > 0 then
      begin
        Inc(pId, 3);
        endIdx := pId;
        while (endIdx <= Length(Result)) and (Result[endIdx] in ['0'..'9']) do
          Inc(endIdx);
        idStr := Copy(Result, pId, endIdx - pId);
        targetId := StrToIntDef(idStr, -1);
        if targetId >= 0 then
        begin
          Result := '';
          linkQry := TSQLQuery.Create(nil);
          try
            linkQry.DataBase := q.DataBase;
            linkQry.Transaction := q.Transaction;
            linkQry.SQL.Text := 'SELECT t.subject, c.data, c.data2 FROM topics t JOIN content c ON t.id = c.topic_id WHERE c.topic_id = :tid';
            linkQry.Params.ParamByName('tid').AsInteger := targetId;
            linkQry.Open;
            try
              if not linkQry.EOF then
              begin
                if not linkQry.Fields[0].IsNull then
                  outSubj := Trim(linkQry.Fields[0].AsString);
                if not linkQry.Fields[1].IsNull then
                  Result := ObterTextoDoCampo(linkQry.Fields[1]);
                if (Result = '') and (linkQry.FieldCount > 2) and (not linkQry.Fields[2].IsNull) then
                  Result := ObterTextoDoCampo(linkQry.Fields[2]);
              end;
            finally
              linkQry.Close;
            end;
          except
            Result := '';
          end;
          linkQry.Free;
        end;
      end
      else
      begin
        pId := Pos('subject=', Result);
        if pId > 0 then
        begin
          Inc(pId, 8);
          endIdx := pId;
          while (endIdx <= Length(Result)) and not (Result[endIdx] in [#13, #10]) do
            Inc(endIdx);
          linkSubj := Trim(Copy(Result, pId, endIdx - pId));
          if linkSubj <> '' then
          begin
            Result := '';
            linkQry := TSQLQuery.Create(nil);
            try
              linkQry.DataBase := q.DataBase;
              linkQry.Transaction := q.Transaction;
              linkQry.SQL.Text := 'SELECT t.subject, c.data, c.data2 FROM topics t JOIN content c ON t.id = c.topic_id WHERE LOWER(t.subject) = LOWER(:subj) LIMIT 1';
              linkQry.Params.ParamByName('subj').AsString := linkSubj;
              linkQry.Open;
              try
                if not linkQry.EOF then
                begin
                  if not linkQry.Fields[0].IsNull then
                    outSubj := Trim(linkQry.Fields[0].AsString);
                  if not linkQry.Fields[1].IsNull then
                    Result := ObterTextoDoCampo(linkQry.Fields[1]);
                  if (Result = '') and (linkQry.FieldCount > 2) and (not linkQry.Fields[2].IsNull) then
                    Result := ObterTextoDoCampo(linkQry.Fields[2]);
                end;
              finally
                linkQry.Close;
              end;
            except
              Result := '';
            end;
            linkQry.Free;
          end;
        end;
      end;
    end;

    if Result <> '' then
      Result := FormatRTF(Result);
  end;

  function Executar(const term: string; out outSubj: string): string;
  begin
    Result := '';
    outSubj := term;
    try
      qry.Close;
      qry.Params.ParamByName('word').AsString := term;
      qry.Open;
      try
        if not qry.EOF then
          Result := LerCamposDeQryIngles(qry, term, outSubj);
      finally
        qry.Close;
      end;
    except
      Result := '';
    end;
  end;

begin
  Result := '';
  matchedSubject := '';
  if (qry = nil) or (wordText = '') then Exit;

  // Normalize curly apostrophes/quotes and dashes
  w := wordText;
  w := StringReplace(w, #$E2#$80#$99, '''', [rfReplaceAll]);
  w := StringReplace(w, #$E2#$80#$98, '''', [rfReplaceAll]);
  w := StringReplace(w, #$E2#$80#$9C, '"', [rfReplaceAll]);
  w := StringReplace(w, #$E2#$80#$9D, '"', [rfReplaceAll]);
  w := StringReplace(w, #$E2#$80#$94, ' ', [rfReplaceAll]);
  w := StringReplace(w, #$E2#$80#$93, ' ', [rfReplaceAll]);
  w := Trim(w);

  while (w <> '') and (w[1] in [' ', #9, #10, #13, '.', ',', ';', ':', '!', '?', '(', ')', '[', ']', '{', '}', '"', '''', '-', '/', '\', '|', '<', '>']) do
    Delete(w, 1, 1);
  while (w <> '') and (w[Length(w)] in [' ', #9, #10, #13, '.', ',', ';', ':', '!', '?', '(', ')', '[', ']', '{', '}', '"', '-', '/', '\', '|', '<', '>']) do
    Delete(w, Length(w), 1);

  if (w <> '') and (w[Length(w)] = '''') and
     ((Length(w) < 2) or not (w[Length(w) - 1] in ['s', 'S'])) then
  begin
    while (w <> '') and (w[Length(w)] = '''') do
      Delete(w, Length(w), 1);
  end;

  if Length(w) < 2 then Exit;

  hasLatinLetter := False;
  hasNonAscii := False;
  for i := 1 to Length(w) do
  begin
    if w[i] in ['A'..'Z', 'a'..'z'] then
      hasLatinLetter := True
    else if Ord(w[i]) >= 128 then
      hasNonAscii := True;
  end;

  if (not hasLatinLetter) or hasNonAscii then Exit;

  SetLength(candidates, 0);
  AddCandidate(w);

  baseW := w;
  if (Length(baseW) > 3) and ((Copy(baseW, Length(baseW) - 1, 2) = '''s') or (Copy(baseW, Length(baseW) - 1, 2) = '''S')) then
  begin
    baseW := Copy(baseW, 1, Length(baseW) - 2);
    AddCandidate(baseW);
  end
  else if (Length(baseW) > 3) and ((Copy(baseW, Length(baseW) - 1, 2) = 's''') or (Copy(baseW, Length(baseW) - 1, 2) = 'S''')) then
  begin
    baseW := Copy(baseW, 1, Length(baseW) - 1);
    AddCandidate(baseW);
  end;

  lw := LowerCase(baseW);
  if (Length(lw) > 4) and (Copy(lw, Length(lw) - 2, 3) = 'ies') then
    AddCandidate(Copy(baseW, 1, Length(baseW) - 3) + 'y');
  if (Length(lw) > 4) and (Copy(lw, Length(lw) - 2, 3) = 'ves') then
  begin
    AddCandidate(Copy(baseW, 1, Length(baseW) - 3) + 'f');
    AddCandidate(Copy(baseW, 1, Length(baseW) - 3) + 'fe');
  end;
  if (Length(lw) > 4) and (Copy(lw, Length(lw) - 1, 2) = 'es') then
  begin
    AddCandidate(Copy(baseW, 1, Length(baseW) - 2));
    AddCandidate(Copy(baseW, 1, Length(baseW) - 1));
  end;
  if (Length(lw) > 3) and (lw[Length(lw)] = 's') and (lw[Length(lw) - 1] <> 's') then
    AddCandidate(Copy(baseW, 1, Length(baseW) - 1));
  if (Length(lw) > 4) and (Copy(lw, Length(lw) - 2, 3) = 'ied') then
    AddCandidate(Copy(baseW, 1, Length(baseW) - 3) + 'y');
  if (Length(lw) > 4) and (Copy(lw, Length(lw) - 1, 2) = 'ed') then
  begin
    AddCandidate(Copy(baseW, 1, Length(baseW) - 2));
    AddCandidate(Copy(baseW, 1, Length(baseW) - 1));
  end;
  if (Length(lw) > 5) and (Copy(lw, Length(lw) - 2, 3) = 'ing') then
  begin
    AddCandidate(Copy(baseW, 1, Length(baseW) - 3));
    AddCandidate(Copy(baseW, 1, Length(baseW) - 3) + 'e');
  end;
  if (Length(lw) > 5) and ((Copy(lw, Length(lw) - 2, 3) = 'eth') or (Copy(lw, Length(lw) - 2, 3) = 'est')) then
  begin
    AddCandidate(Copy(baseW, 1, Length(baseW) - 3));
    AddCandidate(Copy(baseW, 1, Length(baseW) - 3) + 'e');
  end;

  for c in candidates do
  begin
    if c <> '' then
    begin
      Result := Executar(c, matchedSubject);
      if Result <> '' then Exit;
    end;
  end;

  // Fallback for dictionaries with disambiguated headwords (e.g. "Aaron (1)", "Aaron - 1")
  // or topics_wordindex entries whose subject begins with the word
  if (Result = '') and Assigned(qry.DataBase) then
  begin
    auxQry := TSQLQuery.Create(nil);
    try
      auxQry.DataBase := qry.DataBase;
      auxQry.Transaction := qry.Transaction;
      for c in candidates do
      begin
        if c = '' then Continue;
        try
          auxQry.SQL.Text :=
            'SELECT t.subject, c.data, c.data2 FROM topics t ' +
            'JOIN content c ON c.topic_id = t.id ' +
            'WHERE UPPER(t.subject) LIKE :p1 OR UPPER(t.subject) LIKE :p2 ' +
            'ORDER BY t.id ASC LIMIT 1';
          auxQry.Params.ParamByName('p1').AsString := UpperCase(c) + ' (%';
          auxQry.Params.ParamByName('p2').AsString := UpperCase(c) + ' -%';
          auxQry.Open;
          try
            if not auxQry.EOF then
              Result := LerCamposDeQryIngles(auxQry, c, matchedSubject);
          finally
            auxQry.Close;
          end;
        except
          Result := '';
        end;
        if Result <> '' then Exit;
      end;
    finally
      auxQry.Free;
    end;
  end;
end;

procedure TProjeto.ConfigurarDicionariosGlobais(useGlobal: Boolean; const greekPath, hebrewPath, morphPath: string; const englishPath: string = '');
var
  v: TTipoTextoBiblico;
  pGreek, pHebrew, pMorph, pEnglish: string;
begin
  FUseGlobalDicts := useGlobal;

  pGreek := Trim(greekPath);
  pHebrew := Trim(hebrewPath);
  pMorph := Trim(morphPath);
  pEnglish := Trim(englishPath);

  if (pGreek <> '') and (not FileExists(pGreek)) then
  begin
    if FileExists(ExtractFilePath(ParamStr(0)) + pGreek) then
      pGreek := ExtractFilePath(ParamStr(0)) + pGreek
    else
      pGreek := ExpandFileName(pGreek);
  end;
  if (pHebrew <> '') and (not FileExists(pHebrew)) then
  begin
    if FileExists(ExtractFilePath(ParamStr(0)) + pHebrew) then
      pHebrew := ExtractFilePath(ParamStr(0)) + pHebrew
    else
      pHebrew := ExpandFileName(pHebrew);
  end;
  if (pMorph <> '') and (not FileExists(pMorph)) then
  begin
    if FileExists(ExtractFilePath(ParamStr(0)) + pMorph) then
      pMorph := ExtractFilePath(ParamStr(0)) + pMorph
    else
      pMorph := ExpandFileName(pMorph);
  end;
  if (pEnglish <> '') and (not FileExists(pEnglish)) then
  begin
    if FileExists(ExtractFilePath(ParamStr(0)) + pEnglish) then
      pEnglish := ExtractFilePath(ParamStr(0)) + pEnglish
    else
      pEnglish := ExpandFileName(pEnglish);
  end;

  // Greek / Default Strong
  if (FGlobalDicStrongGreekPath <> pGreek) or ((pGreek <> '') and (FGlobalDicStrongGreek = nil)) then
  begin
    LiberarObjetoQuery(FGlobalDicStrongGreek);
    FGlobalDicStrongGreekPath := pGreek;
    if (pGreek <> '') and FileExists(pGreek) then
    begin
      try
        FGlobalDicStrongGreek := CriarObjetoQuery(pGreek);
        FGlobalDicStrongGreek.SQL.Text := 'select data, data2 from topics, content where subject = :strong and id = topic_id';
        FGlobalDicStrongGreek.Prepare;
      except
        LiberarObjetoQuery(FGlobalDicStrongGreek);
      end;
    end;
  end;

  // Hebrew Strong
  if (FGlobalDicStrongHebrewPath <> pHebrew) or ((pHebrew <> '') and (FGlobalDicStrongHebrew = nil)) then
  begin
    LiberarObjetoQuery(FGlobalDicStrongHebrew);
    FGlobalDicStrongHebrewPath := pHebrew;
    if (pHebrew <> '') and FileExists(pHebrew) then
    begin
      try
        FGlobalDicStrongHebrew := CriarObjetoQuery(pHebrew);
        FGlobalDicStrongHebrew.SQL.Text := 'select data, data2 from topics, content where subject = :strong and id = topic_id';
        FGlobalDicStrongHebrew.Prepare;
      except
        LiberarObjetoQuery(FGlobalDicStrongHebrew);
      end;
    end;
  end;

  // Morphology
  if (FGlobalDicMorfoPath <> pMorph) or ((pMorph <> '') and (FGlobalDicMorfo = nil)) then
  begin
    LiberarObjetoQuery(FGlobalDicMorfo);
    FGlobalDicMorfoPath := pMorph;
    if (pMorph <> '') and FileExists(pMorph) then
    begin
      try
        FGlobalDicMorfo := CriarObjetoQuery(pMorph);
        FGlobalDicMorfo.SQL.Text := 'select data, data2 from topics, content where subject = :morf and id = topic_id';
        FGlobalDicMorfo.Prepare;
      except
        LiberarObjetoQuery(FGlobalDicMorfo);
      end;
    end;
  end;

  // English Word / Bible Dictionary
  if (FGlobalDicEnglishPath <> pEnglish) or ((pEnglish <> '') and (FGlobalDicEnglish = nil)) then
  begin
    LiberarObjetoQuery(FGlobalDicEnglish);
    FGlobalDicEnglishPath := pEnglish;
    if (pEnglish <> '') and FileExists(pEnglish) then
    begin
      try
        FGlobalDicEnglish := CriarObjetoQuery(pEnglish);
        FGlobalDicEnglish.SQL.Text :=
          'select t.subject, c.data, c.data2 from topics t, content c ' +
          'where t.subject = :word collate nocase and t.id = c.topic_id ' +
          'order by case when t.subject = :word then 0 else 1 end, t.id asc limit 1';
        FGlobalDicEnglish.Prepare;
      except
        LiberarObjetoQuery(FGlobalDicEnglish);
      end;
    end;
  end;

  // Wire hover handlers to all existing verse containers if global is active
  if FUseGlobalDicts then
  begin
    for v := low(FAVersiculo) to high(FAVersiculo) do
      if Assigned(FAVersiculo[v]) then
      begin
        FAVersiculo[v].OnMouseEnter := @SintagmaOnMouseEnter;
        FAVersiculo[v].OnMouseLeave := @SintagmaOnMouseLeave;
      end;
  end;
end;

function TProjeto.ObterDefinicaoStrong(strong: string; texto: TTipoTextoBiblico
  ): string;
var
  isHebrew: Boolean;
  s, cleanPart: string;
  slashPos, spacePos: Integer;
begin
  Result := '';
  s := Trim(strong);
  if s = '' then Exit;

  while (s <> '') and (s[1] in ['<', ' ', '"', '''']) do
    Delete(s, 1, 1);
  while (s <> '') and (s[Length(s)] in ['>', ' ', '"', '''']) do
    Delete(s, Length(s), 1);

  spacePos := Pos(' ', s);
  if spacePos > 1 then
    s := Copy(s, 1, spacePos - 1);

  if (Length(s) >= 2) and (UpperCase(Copy(s, 1, 2)) = 'WH') then
    Delete(s, 1, 1)
  else if (Length(s) >= 2) and (UpperCase(Copy(s, 1, 2)) = 'WG') then
    Delete(s, 1, 1);

  cleanPart := s;
  slashPos := LastDelimiter('/', cleanPart);
  if slashPos > 0 then
    cleanPart := Copy(cleanPart, slashPos + 1, MaxInt);
  if (Length(cleanPart) >= 2) and (UpperCase(Copy(cleanPart, 1, 2)) = 'WH') then
    Delete(cleanPart, 1, 1)
  else if (Length(cleanPart) >= 2) and (UpperCase(Copy(cleanPart, 1, 2)) = 'WG') then
    Delete(cleanPart, 1, 1);

  if cleanPart = '' then Exit;

  // Determine language strictly from Strong's prefix first, then verse/project scope
  if (cleanPart[1] = 'H') or (cleanPart[1] = 'h') or (s[1] = 'H') or (s[1] = 'h') then
    isHebrew := True
  else if (cleanPart[1] = 'G') or (cleanPart[1] = 'g') or (s[1] = 'G') or (s[1] = 'g') then
    isHebrew := False
  else
  begin
    if (FReference.BookID >= 1) and (FReference.BookID <= 39) then
      isHebrew := True
    else if FReference.BookID >= 40 then
      isHebrew := False
    else if FEscopo = etOT then
      isHebrew := True
    else
      isHebrew := False;
  end;

  // 1. Default Dictionaries routing:
  //    Hebrew Strong's numbers route strictly to FGlobalDicStrongHebrew;
  //    Greek Strong's numbers route strictly to FGlobalDicStrongGreek.
  //    Never convert a Hebrew H-code into a Greek G-code on the Greek dictionary.
  if FUseGlobalDicts and FUseDefaultDict[texto] then
  begin
    if isHebrew then
    begin
      if Assigned(FGlobalDicStrongHebrew) then
        Result := BuscarDefinicaoStrongNaQuery(FGlobalDicStrongHebrew, cleanPart, True);
      if (Result = '') and (not Assigned(FGlobalDicStrongHebrew)) and Assigned(FGlobalDicStrongGreek) then
        Result := BuscarDefinicaoStrongNaQuery(FGlobalDicStrongGreek, cleanPart, True);
    end
    else
    begin
      if Assigned(FGlobalDicStrongGreek) then
        Result := BuscarDefinicaoStrongNaQuery(FGlobalDicStrongGreek, cleanPart, False);
      if (Result = '') and (not Assigned(FGlobalDicStrongGreek)) and Assigned(FGlobalDicStrongHebrew) then
        Result := BuscarDefinicaoStrongNaQuery(FGlobalDicStrongHebrew, cleanPart, False);
    end;

    if Result <> '' then
      Exit;
  end;

  // 2. Fallback to per-module dictionary if assigned
  if assigned(FADicStrong[texto]) then
    Result := BuscarDefinicaoStrongNaQuery(FADicStrong[texto], cleanPart, isHebrew);

  // 3. If per-module dictionary was used (e.g. FUseDefaultDict[texto] was false) and had no match,
  //    fall back to the matching Default Dictionary for that language
  if (Result = '') and FUseGlobalDicts then
  begin
    if isHebrew and Assigned(FGlobalDicStrongHebrew) then
      Result := BuscarDefinicaoStrongNaQuery(FGlobalDicStrongHebrew, cleanPart, True)
    else if (not isHebrew) and Assigned(FGlobalDicStrongGreek) then
      Result := BuscarDefinicaoStrongNaQuery(FGlobalDicStrongGreek, cleanPart, False);
  end;
end;

function TProjeto.ObterDefinicaoMorfo(morfo: string; texto: TTipoTextoBiblico
  ): string;
var
  m: string;
begin
  Result := '';
  m := Trim(morfo);
  if m = '' then Exit;

  while (m <> '') and (m[1] in ['<', ' ', '"', '''']) do
    Delete(m, 1, 1);
  while (m <> '') and (m[Length(m)] in ['>', ' ', '"', '''']) do
    Delete(m, Length(m), 1);

  if pos(' ', m) > 1 then
    m := copy(m, 1, pos(' ', m)-1);

  // 1. If default morphology dictionary enabled for this module and assigned
  if FUseGlobalDicts and FUseDefaultDict[texto] and assigned(FGlobalDicMorfo) then
  begin
    Result := BuscarDefinicaoMorfoNaQuery(FGlobalDicMorfo, m);
    if Result <> '' then Exit;
  end;

  // 2. Fallback to per-module morphology dictionary
  if assigned(FADicMorfo[texto]) then
    Result := BuscarDefinicaoMorfoNaQuery(FADicMorfo[texto], m);
end;

function TProjeto.ObterDefinicaoPalavraIngles(const wordText: string; texto: TTipoTextoBiblico; out matchedSubject: string): string;
begin
  Result := '';
  matchedSubject := '';
  if Trim(wordText) = '' then Exit;

  if FUseGlobalDicts and Assigned(FGlobalDicEnglish) then
    Result := BuscarDefinicaoPalavraInglesNaQuery(FGlobalDicEnglish, wordText, matchedSubject);
end;

function TProjeto.SanitizarFontesRTF(const rtf: string): string;
const
  CustomGreekFonts: array[0..7] of string = (
    'Gentium', 'GentiumAlt', 'Galatia SIL', 'TITUS Cyberbit Basic',
    'SBL Greek', 'SPIonic', 'LaserGreek', 'Graeca'
  );
  CustomHebrewFonts: array[0..3] of string = (
    'Cardo', 'Ezra SIL', 'SBL Hebrew', 'SPTiberian'
  );
var
  pStart, pEnd, i, depth, len: Integer;
  fontTbl, prefix, suffix, fName: string;
begin
  Result := rtf;
  if Result = '' then Exit;

  pStart := Pos('{\fonttbl', Result);
  if pStart = 0 then Exit;

  len := Length(Result);
  depth := 0;
  pEnd := pStart;
  while pEnd <= len do
  begin
    if Result[pEnd] = '{' then
      Inc(depth)
    else if Result[pEnd] = '}' then
    begin
      Dec(depth);
      if depth = 0 then
        Break;
    end;
    Inc(pEnd);
  end;

  if (pEnd > len) or (depth <> 0) then Exit;

  prefix := Copy(Result, 1, pStart - 1);
  fontTbl := Copy(Result, pStart, pEnd - pStart + 1);
  suffix := Copy(Result, pEnd + 1, len - pEnd);

  // If a specialized font is not present in Screen.Fonts, substitute Segoe UI
  // strictly inside the {\fonttbl ...} header to avoid corrupting text in the body!
  for i := Low(CustomGreekFonts) to High(CustomGreekFonts) do
  begin
    fName := CustomGreekFonts[i];
    if (Screen.Fonts.IndexOf(fName) = -1) and (Pos(fName, fontTbl) > 0) then
    begin
      fontTbl := StringReplace(fontTbl, fName + ';', 'Segoe UI;', [rfReplaceAll, rfIgnoreCase]);
      fontTbl := StringReplace(fontTbl, fName + ' ;', 'Segoe UI;', [rfReplaceAll, rfIgnoreCase]);
    end;
  end;

  for i := Low(CustomHebrewFonts) to High(CustomHebrewFonts) do
  begin
    fName := CustomHebrewFonts[i];
    if (Screen.Fonts.IndexOf(fName) = -1) and (Pos(fName, fontTbl) > 0) then
    begin
      fontTbl := StringReplace(fontTbl, fName + ';', 'Segoe UI;', [rfReplaceAll, rfIgnoreCase]);
      fontTbl := StringReplace(fontTbl, fName + ' ;', 'Segoe UI;', [rfReplaceAll, rfIgnoreCase]);
    end;
  end;

  Result := prefix + fontTbl + suffix;
end;

function TProjeto.FormatRTF(rtf: string): string;
begin
  result := '';
  if rtf.IsEmpty then
    exit;

  if AnsiStartsStr('{\rtf1', rtf) then
    result := SanitizarFontesRTF(rtf)
  else
    result := SanitizarFontesRTF('{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Tahoma;}' +
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
      '\viewkind4\uc1\pard\lang1046\f0\fs20 ' + rtf + '}');
end;

function TProjeto.ObterPalavrasBiblia(v: TVersiculo): string;
var
  s: TSyntagm;
  str: TStringStream;
begin
  Result := '';
  if v = nil then Exit;
  str := TStringStream.Create('');
  try
    for s in v.Sintagmas do
      if s.Kind = tsSintagma then
      begin
        str.WriteString(s.Text);
        str.WriteString(' ');
      end;
    Result := str.DataString;
  finally
    str.Free;
  end;
end;

function SanitizeUTF8String(const s: string): string;
var
  i, len: Integer;
  c: Byte;
  valid: Boolean;
begin
  if s = '' then Exit('');
  valid := True;
  i := 1;
  len := Length(s);
  while i <= len do
  begin
    c := Byte(s[i]);
    if c <= 127 then
      Inc(i)
    else if (c in [194..223]) and (i + 1 <= len) and (Byte(s[i+1]) in [128..191]) then
      Inc(i, 2)
    else if (c in [224..239]) and (i + 2 <= len) and (Byte(s[i+1]) in [128..191]) and (Byte(s[i+2]) in [128..191]) then
      Inc(i, 3)
    else if (c in [240..244]) and (i + 3 <= len) and (Byte(s[i+1]) in [128..191]) and (Byte(s[i+2]) in [128..191]) and (Byte(s[i+3]) in [128..191]) then
      Inc(i, 4)
    else
    begin
      valid := False;
      Break;
    end;
  end;

  if valid then
    Result := s
  else
  begin
    Result := '';
    for i := 1 to len do
    begin
      c := Byte(s[i]);
      case c of
        145: Result := Result + #226#128#152; // left single quote
        146: Result := Result + #226#128#153; // right single quote / apostrophe
        147: Result := Result + #226#128#156; // left double quote
        148: Result := Result + #226#128#157; // right double quote
        149: Result := Result + #226#128#162; // bullet
        150: Result := Result + #226#128#147; // en dash
        151: Result := Result + #226#128#148; // em dash
        153: Result := Result + #226#132#162; // trademark
        169: Result := Result + #194#169;     // copyright
        174: Result := Result + #194#174;     // registered
        176: Result := Result + #194#176;     // degree
      else
        if c >= 128 then
          Result := Result + Chr(192 or (c shr 6)) + Chr(128 or (c and 63))
        else
          Result := Result + s[i];
      end;
    end;
  end;
end;

procedure TProjeto.SetVerseText(versiculo: string;
  texto: TTipoTextoBiblico; replace: boolean);
var
  oldPairs, newPairs, oldWords, newWords: string;
  oldStatus: Integer;
  wordsUpdated: Boolean;
begin
  versiculo := SanitizeUTF8String(versiculo);
  oldStatus := FTblPares.FieldByName('pare_situacao').AsInteger;
  oldPairs := FTblPares.FieldByName('pare_pares').AsString;

  FTblPares.Edit;

  // try to keep associations and update pairs
  if replace and (texto in [tbOrigem, tbDestino]) and not oldPairs.IsEmpty then
  begin
    FATmpVerse[tbOrigem ].Texto := FTblPares.Fields[FACamposTexto[tbOrigem ]].AsString;
    FATmpVerse[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;

    // Capture the plain Bible words before the update
    oldWords := ObterPalavrasBiblia(FATmpVerse[texto]);

    try
      FATmpVerse[tbOrigem ].Pares := oldPairs;
      FATmpVerse[texto].AlterarTexto(versiculo);
      newPairs := FATmpVerse[tbOrigem].Pares;
      FTblPares.FieldByName('pare_pares').AsString := newPairs;

      // Capture the plain Bible words after the update
      newWords := ObterPalavrasBiblia(FATmpVerse[texto]);

      // Status change to "Needs review" must ONLY be triggered if Bible words were updated within the verse.
      // This rule does NOT apply to updated internal taggings, codes, Strong's, or morphology that don't change the Bible words.
      wordsUpdated := (oldWords <> newWords);

      if (oldStatus = 3) and wordsUpdated then
      begin
        FTblPares.FieldByName('pare_situacao').AsInteger := 2; // Needs review
        if (oldStatus >= 0) and (oldStatus <= 3) then
          dec(FTotaisSituacao[oldStatus]);
        inc(FTotaisSituacao[2]);
      end;
    except
      on E: Exception do
      begin
        newWords := ObterPalavrasBiblia(FATmpVerse[texto]);
        if (oldStatus = 3) and (oldWords <> newWords) then
        begin
          FTblPares.FieldByName('pare_situacao').AsInteger := 2; // Needs review
          if (oldStatus >= 0) and (oldStatus <= 3) then
            dec(FTotaisSituacao[oldStatus]);
          inc(FTotaisSituacao[2]);
        end;
        MessageDlg(SError, SCorruptedData + #13#10#13#10 + FormattedReference + #13#10 +
             format('%s'#13#10'pares: %s'#13#10'%s'#13#10'%s',
                    [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                    FATmpVerse[tbOrigem].DebugTokens, FATmpVerse[tbDestino].DebugTokens]),
             mtError, [mbOK], 0);
      end;
    end;
  end;

  FTblPares.Fields[FACamposTexto[texto]].AsString := versiculo;
  if not replace and (texto in [tbOrigem, tbDestino]) then // clearing associations to avoid inconsistencies
  begin
    FTblPares.FieldByName('pare_pares').AsString := '';
    if not HasSituacaoCustomizada then
    begin
      if (oldStatus = 3) or (oldStatus = 1) then
      begin
        // If it was associated or in progress, change to Needs review so user knows where to fix it
        FTblPares.FieldByName('pare_situacao').AsInteger := 2; // Needs review
        if (oldStatus >= 0) and (oldStatus <= 3) then
          dec(FTotaisSituacao[oldStatus]);
        inc(FTotaisSituacao[2]);
      end
      else
        FTblPares.FieldByName('pare_situacao').AsInteger := 0; // not associated
    end;
  end;

  FTblPares.Post;
end;

procedure TProjeto.PreRolagemVersiculo(DataSet: TDataSet);
var
  wasScrollEnabled: Boolean;
begin
  if not FScrollEventsEnabled or not Assigned(FAVersiculo[tbOrigem]) then
    exit;

  if FLoadedVerseID = '' then
    exit;

  FMemoVersiculo.Desativar; // hide and save verse edit if necessary

  // Ensure FTblPares is positioned on the exact verse currently loaded in FAVersiculo before saving
  if Assigned(FTblPares) and FTblPares.Active and (GetID() <> FLoadedVerseID) then
  begin
    wasScrollEnabled := FScrollEventsEnabled;
    FScrollEventsEnabled := False;
    try
      FTblPares.Locate('pare_id', FLoadedVerseID, []);
    finally
      FScrollEventsEnabled := wasScrollEnabled;
    end;
  end;

  if (not Assigned(FTblPares)) or (not FTblPares.Active) or (GetID() <> FLoadedVerseID) then
    exit;

  if FAVersiculo[tbOrigem].Modificado then
  begin
    SalvarAssociacoes;
    SalvarPares;
    if Assigned(FTblPares) and FTblPares.Active then
      FLastSavedVerseID := FTblPares.FieldByName('pare_id').AsString;
  end;
  SalvarTexto; // salvar alterações nos textos dos versículos

  FreeAndNil(FParesAntigos); // liberando pares antigos do versículo

  if assigned(FMemoComentarios) then
    Comentarios := FMemoComentarios.Text;
end;

procedure TProjeto.PosRolagemVersiculo(DataSet: TDataSet);
var
  v: TTipoTextoBiblico;
  e: TOnNovoVersiculoEvent;
  rawPairs: string;
  oldRadioChange: TNotifyEvent;
begin
  if not FScrollEventsEnabled then
    exit;

  FLoadedVerseID := GetID();
  UpdateReference(FLoadedVerseID);

  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
    begin
      FAVersiculo[v].Ativo := false;
      FAVersiculo[v].Texto := FTblPares.Fields[FACamposTexto[v]].AsString;
    end;

  if assigned(FAVersiculo[tbOrigem]) and assigned(FAVersiculo[tbDestino]) then
  begin
    rawPairs := FTblPares.FieldByName('pare_pares').AsString;
    if (rawPairs <> '') and (not FAVersiculo[tbOrigem].AreParesValidForVerse(rawPairs)) then
    begin
      // Self-heal misplaced/corrupted pairs so this verse is not scrambled
      FTblPares.Edit;
      FTblPares.FieldByName('pare_pares').AsString := '';
      if FTblPares.FieldByName('pare_situacao').AsInteger > 0 then
        FTblPares.FieldByName('pare_situacao').AsInteger := 0;
      FTblPares.Post;
      rawPairs := '';
    end;
    try
      FAVersiculo[tbOrigem].Pares := rawPairs;
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
      if FAVersiculo[v].ViewMode = vvmEdit then
        FAVersiculo[v].ViewMode := vvmAssoc;
      FAVersiculo[v].Modificado := false;
      FAVersiculo[v].XMLModificado := false;
      FAVersiculo[v].Ativo := true;
      FAVersiculo[v].Renderizar;
    end;

  SelectTreeNode(ID);

  if assigned(FMemoComentarios) then
    FMemoComentarios.Text := Comentarios;

  if assigned(FRadioGroupSituacao) then
  begin
    oldRadioChange := FRadioGroupSituacao.OnSelectionChanged;
    FRadioGroupSituacao.OnSelectionChanged := nil;
    try
      FRadioGroupSituacao.ItemIndex := Situacao;
    finally
      FRadioGroupSituacao.OnSelectionChanged := oldRadioChange;
    end;
  end;
  if assigned(FAVersiculo[tbDestino]) then
    FAVersiculo[tbDestino].StatusIndex := Situacao;

  if FTblPares.Fields[FACamposTexto[tbDestino]].AsString.IsEmpty then // open verse to edition if is empty
    OnDblClickVersiculo(FAVersiculo[tbDestino].Painel);

  FHoveredAssocDestSyntagm := nil;
  UpdateDestinationPopup;

  for e in FOnNovoVersiculo do
    e(self);

  if FAutoSave and not FExportando then
    Salvar;
end;

procedure TProjeto.SalvarPares;
var
  newPairs: string;
begin
  if (not Assigned(FTblPares)) or (not FTblPares.Active) or (not Assigned(FAVersiculo[tbOrigem])) then
    Exit;
  if (FLoadedVerseID <> '') and (GetID() <> FLoadedVerseID) then
    Exit;
  newPairs := FAVersiculo[tbOrigem].Pares;
  if (newPairs <> '') and (not FAVersiculo[tbOrigem].AreParesValidForVerse(newPairs)) then
    Exit;
  FTblPares.Edit;
  FTblPares.FieldByName('pare_pares').AsString := newPairs;
  FTblPares.Post;
end;

procedure TProjeto.SalvarTexto;
var
  v: TTipoTextoBiblico;
begin
  if (not Assigned(FTblPares)) or (not FTblPares.Active) then
    Exit;
  if (FLoadedVerseID <> '') and (GetID() <> FLoadedVerseID) then
    Exit;
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

procedure TProjeto.HealMisplacedPairsInDatabase;
var
  wasScrollEnabled: Boolean;
  savedId, curPairs: string;
  badIds, badPairs, badStatuses: TStringList;
  i, restoredStatus: Integer;
  anyChanged: Boolean;
begin
  if (not Assigned(FTblPares)) or (not FTblPares.Active) or
     (not Assigned(FATmpVerse[tbOrigem])) or (not Assigned(FATmpVerse[tbDestino])) then
    Exit;

  wasScrollEnabled := FScrollEventsEnabled;
  savedId := GetID();
  FScrollEventsEnabled := False;
  badIds := TStringList.Create;
  badPairs := TStringList.Create;
  badStatuses := TStringList.Create;
  anyChanged := False;
  try
    FTblPares.First;
    while not FTblPares.EOF do
    begin
      curPairs := FTblPares.FieldByName('pare_pares').AsString;
      if curPairs <> '' then
      begin
        FATmpVerse[tbOrigem].Texto := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString;
        FATmpVerse[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
        if not FATmpVerse[tbOrigem].AreParesValidForVerse(curPairs) then
        begin
          badIds.Add(FTblPares.FieldByName('pare_id').AsString);
          badPairs.Add(curPairs);
          badStatuses.Add(IntToStr(FTblPares.FieldByName('pare_situacao').AsInteger));

          FTblPares.Edit;
          FTblPares.FieldByName('pare_pares').AsString := '';
          if FTblPares.FieldByName('pare_situacao').AsInteger > 0 then
            FTblPares.FieldByName('pare_situacao').AsInteger := 0;
          FTblPares.Post;
          anyChanged := True;
        end;
      end;
      FTblPares.Next;
    end;

    // Attempt to restore misplaced pairs to Genesis 41:32 ('1,41,32') or active marker if unassociated
    for i := 0 to badPairs.Count - 1 do
    begin
      if FTblPares.Locate('pare_id', '1,41,32', []) and
         (FTblPares.FieldByName('pare_pares').AsString = '') then
      begin
        FATmpVerse[tbOrigem].Texto := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString;
        FATmpVerse[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
        if FATmpVerse[tbOrigem].AreParesValidForVerse(badPairs[i]) then
        begin
          restoredStatus := StrToIntDef(badStatuses[i], 3);
          if (restoredStatus <= 0) or (restoredStatus > 3) then
            restoredStatus := 3;
          FTblPares.Edit;
          FTblPares.FieldByName('pare_pares').AsString := badPairs[i];
          FTblPares.FieldByName('pare_situacao').AsInteger := restoredStatus;
          FTblPares.Post;
          anyChanged := True;
        end;
      end;
    end;

    if anyChanged and FTblPares.UpdatesPending then
    begin
      try
        FTblPares.ApplyUpdates;
      except
        on E: Exception do
          DebugLn('HealMisplacedPairsInDatabase ApplyUpdates warning: ' + E.Message);
      end;
    end;

    if savedId <> '' then
      FTblPares.Locate('pare_id', savedId, []);
  finally
    badIds.Free;
    badPairs.Free;
    badStatuses.Free;
    FScrollEventsEnabled := wasScrollEnabled;
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

function TProjeto.IsSourceSyntagm(Sender: TSyntagm): boolean;
begin
  Result := Assigned(Sender) and Assigned(FAVersiculo[tbOrigem]) and (Sender.VerseRef = FAVersiculo[tbOrigem]);
end;

procedure TProjeto.SintagmaOnMouseEnter(Sender: TSyntagm);
var
  show: boolean;
  isAssocDestWord: boolean;
begin
  UpdateVerseNavHoverStates;
  if Assigned(FOnSintagmaMouseEnter) then
    FOnSintagmaMouseEnter(Sender);

  HighlightStrongs(Sender);
  SetDisplayTags(false);

  isAssocDestWord := Assigned(Sender) and Assigned(FAVersiculo[tbDestino]) and
                     (Sender.VerseRef = FAVersiculo[tbDestino]) and Sender.IsAssociated;

  if isAssocDestWord then
  begin
    // Rule: hovering an associated word within the Destination text disables the southeast
    // selected-Source popup and pops up the dictionary entry for the hovered word as usual.
    FHoveredAssocDestSyntagm := Sender;
    if FDestPopupActive then
    begin
      FDestPopupSyntagm := nil;
      FDestPopupActive := False;
      if Assigned(FrmDictionaryPopup) and FrmDictionaryPopup.Visible then
        FrmDictionaryPopup.Ocultar;
    end;
  end
  else
  begin
    FHoveredAssocDestSyntagm := nil;

    // Inside Destination panel on an unassociated word while a Source word is selected:
    // keep showing the southeast popup for the selected unassociated Source word.
    if Assigned(Sender) and Assigned(FAVersiculo[tbDestino]) and
       (Sender.VerseRef = FAVersiculo[tbDestino]) and (not Sender.IsAssociated) and
       Assigned(GetSelectedUnassociatedSourceSyntagm) then
    begin
      FTemporizador.Enabled := false;
      UpdateDestinationPopup;
      Exit;
    end;

    // Hovering any word in Source (highlighted, unassociated, or associated),
    // Reference 1 (tbConsulta1), Reference 2 (tbConsulta2), or Destination when no Source word is selected:
    // hide any active Destination-panel southeast popup and allow the normal hover popup for this word.
    if FDestPopupActive then
    begin
      FDestPopupSyntagm := nil;
      FDestPopupActive := False;
      if Assigned(FrmDictionaryPopup) and FrmDictionaryPopup.Visible then
        FrmDictionaryPopup.Ocultar;
    end;
  end;

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
end;

procedure TProjeto.SintagmaOnMouseLeave(Sender: TSyntagm);
var
  hide: boolean;
  v: TTipoTextoBiblico;
  wasHoveredAssocDest: boolean;
begin
  UpdateVerseNavHoverStates;
  FTemporizador.Enabled := false;

  wasHoveredAssocDest := Assigned(FHoveredAssocDestSyntagm) and
                         ((Sender = FHoveredAssocDestSyntagm) or (not FHoveredAssocDestSyntagm.IsAssociated));
  if wasHoveredAssocDest then
    FHoveredAssocDestSyntagm := nil;

  hide := true;
  case FPopupTrigger of
    ptMouseHover:     hide := GetKeyState(VK_CONTROL) and $8000 = 0;
    ptAltMouseHover:  hide := GetKeyState(VK_MENU   ) and $8000 = 0;
    ptCtrlMouseHover: hide := GetKeyState(VK_CONTROL) and $8000 = 0;
  end;

  if wasHoveredAssocDest then
  begin
    if hide and Assigned(FrmDictionaryPopup) then
      FrmDictionaryPopup.Ocultar;
    UpdateDestinationPopup;
  end
  else if FDestPopupActive then
  begin
    UpdateDestinationPopup;
  end
  else
  begin
    if hide then
      frmDictionaryPopup.Ocultar;
    if Assigned(GetSelectedUnassociatedSourceSyntagm) then
      UpdateDestinationPopup;
  end;

  { hide strong hightlights }
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
        FAVersiculo[v].DisableStrongHighlight;
end;

procedure TProjeto.SintagmaOnClick(Sender: TSyntagm);
var
  v: TTipoTextoBiblico;
  dummyPt: TPoint;
  showPopup: Boolean;
begin
  FMemoVersiculo.Desativar; // saving eventual changes on verse edit

  // Clear hovered-associated override when clicking/associating so next auto-selected Source word shows immediately
  FHoveredAssocDestSyntagm := nil;

  for v := low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
      FAVersiculo[v].DisableStrongHighlight;
  if Assigned(Sender) then
    HighlightStrongs(Sender);

  if assigned(FOnSintagmaClick) and Assigned(Sender) and
     (IsSourceSyntagm(Sender) or Sender.HasStrongs or Sender.HasMorph) then
    FOnSintagmaClick(Sender);

  if assigned(FAVersiculo[tbOrigem]) and Assigned(Sender) and
     ((Sender.VerseRef = FAVersiculo[tbOrigem]) or (Sender.VerseRef = FAVersiculo[tbDestino])) then
  begin
    if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AndamentoAssociacao > 0) then
      FRadioGroupSituacao.ItemIndex := 1 // associando
    else if (FRadioGroupSituacao.ItemIndex > 1) and (FAVersiculo[tbOrigem].AndamentoAssociacao = 0) then
      FRadioGroupSituacao.ItemIndex := 0 // sem associação
    else if (FRadioGroupSituacao.ItemIndex = 1) and ((FAVersiculo[tbOrigem].AndamentoAssociacao = 1) or (FAVersiculo[tbDestino].AndamentoAssociacao = 1)) then
      FRadioGroupSituacao.ItemIndex := 3; // associado
  end;

  UpdateDestinationPopup;

  if Assigned(Sender) and IsSourceSyntagm(Sender) and
     (not IsMouseInDestinationPanel(dummyPt)) and
     Assigned(FAVersiculo[tbOrigem]) and Assigned(FAVersiculo[tbOrigem].Painel) and
     IsMouseCursorInControl(FAVersiculo[tbOrigem].Painel) then
  begin
    showPopup := False;
    case FPopupTrigger of
      ptMouseHover:     showPopup := (GetKeyState(VK_CONTROL) and $8000 = 0) and
                                     (GetKeyState(VK_SHIFT  ) and $8000 = 0);
      ptAltMouseHover:  showPopup := (GetKeyState(VK_MENU   ) and $8000 <> 0);
      ptCtrlMouseHover: showPopup := (GetKeyState(VK_CONTROL) and $8000 <> 0) and
                                     (GetKeyState(VK_SHIFT  ) and $8000 = 0);
    end;
    if showPopup then
    begin
      if Assigned(FrmDictionaryPopup) and FrmDictionaryPopup.Visible and (not FDestPopupActive) then
        FrmDictionaryPopup.Ocultar;
      FTemporizador.Enabled := False;
      FTemporizador.Tag := PtrInt(Sender);
      FTemporizador.Enabled := True;
    end;
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

procedure TProjeto.OnExportBibleModule(Sender: TVersiculo);
var
  lines, projectVerses: TStringList;
  v, text: TTipoTextoBiblico;
  SaveDlg: TSaveDialog;
  destination, modExt, origPath, props, verseLine, ext, currentId, baseName, desc: string;
  exportScope: TEscopoTexto;
  totalExpectedLines, i: Integer;
begin
  text := tbNone;
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if Sender = FAVersiculo[v] then
    begin
      text := v;
      break;
    end;

  if text = tbNone then
     raise Exception.Create('Invalid text type in OnVersePopup');

  modExt := ObterInfo(Format('moduletype.%d', [Ord(text)]));
  if modExt = '' then
  begin
    case text of
      tbOrigem:    modExt := ObterInfo('moduletype.origem');
      tbDestino:   modExt := ObterInfo('moduletype.destino');
      tbConsulta1: modExt := ObterInfo('moduletype.consulta1');
      tbConsulta2: modExt := ObterInfo('moduletype.consulta2');
    end;
  end;

  origPath := ObterInfo(Format('modulepath.%d', [Ord(text)]));
  if origPath = '' then
  begin
    case text of
      tbOrigem:    origPath := ObterInfo('modulepath.origem');
      tbDestino:   origPath := ObterInfo('modulepath.destino');
      tbConsulta1: origPath := ObterInfo('modulepath.consulta1');
      tbConsulta2: origPath := ObterInfo('modulepath.consulta2');
    end;
  end;

  if (modExt = '') and (origPath <> '') then
    modExt := LowerCase(ExtractFileExt(origPath));

  if modExt = '' then
  begin
    case FEscopo of
      etOT:  modExt := '.ot';
      etNT:  modExt := '.nt';
      etONT: modExt := '.ont';
    else
      modExt := '.ont';
    end;
  end;

  if not modExt.StartsWith('.') then
    modExt := '.' + modExt;

  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.Title := SExportBibleModuleSaveDlgTitle;
    SaveDlg.DefaultExt := modExt;

    if ObterInfo(Format('export.dest.panel.%d', [Ord(text)])) <> '' then
    begin
      SaveDlg.InitialDir := ExtractFileDir(ObterInfo(Format('export.dest.panel.%d', [Ord(text)])));
      SaveDlg.FileName := ExtractFileName(ObterInfo(Format('export.dest.panel.%d', [Ord(text)])));
    end
    else if origPath <> '' then
    begin
      if DirectoryExists(ExtractFileDir(origPath)) then
        SaveDlg.InitialDir := ExtractFileDir(origPath);
      SaveDlg.FileName := ExtractFileName(origPath);
    end
    else
    begin
      baseName := ChangeFileExt(ExtractFileName(FFileName), '');
      if baseName = '' then
        baseName := 'bible';

      case text of
        tbOrigem:    SaveDlg.FileName := baseName + '_source' + modExt;
        tbDestino:   SaveDlg.FileName := baseName + '_destination' + modExt;
        tbConsulta1: SaveDlg.FileName := baseName + '_ref1' + modExt;
        tbConsulta2: SaveDlg.FileName := baseName + '_ref2' + modExt;
      else
        SaveDlg.FileName := baseName + modExt;
      end;
    end;

    if modExt = '.nt' then
    begin
      SaveDlg.Filter := 'theWord New Testament Bible module (*.nt)|*.nt|theWord Complete Bible module (*.ont)|*.ont|theWord Old Testament Bible module (*.ot)|*.ot|All files (*.*)|*.*';
      SaveDlg.FilterIndex := 1;
    end
    else if modExt = '.ot' then
    begin
      SaveDlg.Filter := 'theWord Old Testament Bible module (*.ot)|*.ot|theWord Complete Bible module (*.ont)|*.ont|theWord New Testament Bible module (*.nt)|*.nt|All files (*.*)|*.*';
      SaveDlg.FilterIndex := 1;
    end
    else
    begin
      SaveDlg.Filter := 'theWord Complete Bible module (*.ont)|*.ont|theWord New Testament Bible module (*.nt)|*.nt|theWord Old Testament Bible module (*.ot)|*.ot|All files (*.*)|*.*';
      SaveDlg.FilterIndex := 1;
    end;

    destination := '';
    if SaveDlg.Execute then
      destination := SaveDlg.FileName;
  finally
    SaveDlg.Free;
  end;

  if destination.IsEmpty then
    exit;

  ext := LowerCase(ExtractFileExt(destination));
  if ext = '' then
  begin
    ext := modExt;
    destination := destination + ext;
  end;

  if ext = '.nt' then
    exportScope := etNT
  else if ext = '.ot' then
    exportScope := etOT
  else
    exportScope := etONT;

  totalExpectedLines := QLinhas[exportScope];

  props := '';
  case text of
    tbOrigem:    props := ResgatarInfo('propriedades.origem');
    tbDestino:   props := ResgatarInfo('propriedades.destino');
    tbConsulta1: props := ResgatarInfo('propriedades.consulta1');
    tbConsulta2: props := ResgatarInfo('propriedades.consulta2');
  end;

  if props = '' then
    props := ResgatarInfo(Format('propriedades.%d', [Ord(text)]));

  if Trim(props) = '' then
  begin
    desc := GetTextDescription(text);
    if desc = '' then
    begin
      case text of
        tbOrigem:    desc := 'Source Bible';
        tbDestino:   desc := 'Destination Bible';
        tbConsulta1: desc := 'Reference 1';
        tbConsulta2: desc := 'Reference 2';
      else
        desc := 'Bible';
      end;
    end;

    props := 'title=' + desc + #13#10 +
             'short.title=' + desc + #13#10 +
             'description=Exported Bible module from iBiblia' + #13#10 +
             'version=1.0' + #13#10;
    if exportScope = etNT then
      props := props + 'ot=0' + #13#10
    else if exportScope = etOT then
      props := props + 'nt=0' + #13#10;
  end;

  if (FATmpVerse[tbOrigem] = nil) or (FATmpVerse[tbDestino] = nil) then
  begin
    FATmpVerse[tbOrigem] := TVersiculo.Criar;
    FATmpVerse[tbDestino] := TVersiculo.Criar;
    FATmpVerse[tbOrigem].VersiculoPar := FATmpVerse[tbDestino];
  end;

  currentId := '';
  projectVerses := TStringList.Create;
  lines := TStringList.Create;
  try
    StartScrollingSession;
    HealMisplacedPairsInDatabase;
    with FTblPares do
    begin
      currentId := FieldByName('pare_id').AsString;
      VersiculoInicial;
      while not EOF do
      begin
        if (text = tbDestino) and (Length(FieldByName('pare_pares').AsString) > 0) then
        begin
          FATmpVerse[tbOrigem ].Texto := Fields[FACamposTexto[tbOrigem ]].AsString;
          FATmpVerse[tbDestino].Texto := Fields[FACamposTexto[tbDestino]].AsString;
          try
            FATmpVerse[tbOrigem].Pares := FieldByName('pare_pares').AsString;
            verseLine := FATmpVerse[tbDestino].GetLinhaONT(True, False, False, False);
          except
            verseLine := Fields[FACamposTexto[tbDestino]].AsString;
          end;
        end
        else
          verseLine := Fields[FACamposTexto[text]].AsString;

        projectVerses.Add(verseLine);
        VersiculoSeguinte;
      end;

      if (currentId <> '') and Locate('pare_id', currentId, []) then
        PosRolagemVersiculo(nil);
    end;
    FinishScrollingSession;

    if exportScope = etONT then
    begin
      if FEscopo = etONT then
        lines.Assign(projectVerses)
      else if FEscopo = etNT then
      begin
        for i := 0 to QLinhas[etOT] - 1 do
          lines.Add('');
        for i := 0 to projectVerses.Count - 1 do
          lines.Add(projectVerses[i]);
      end
      else if FEscopo = etOT then
      begin
        for i := 0 to projectVerses.Count - 1 do
          lines.Add(projectVerses[i]);
        for i := 0 to QLinhas[etNT] - 1 do
          lines.Add('');
      end;
    end
    else if exportScope = etNT then
    begin
      if FEscopo = etNT then
        lines.Assign(projectVerses)
      else if FEscopo = etONT then
      begin
        for i := QLinhas[etOT] to projectVerses.Count - 1 do
          lines.Add(projectVerses[i]);
      end
      else if FEscopo = etOT then
      begin
        for i := 0 to QLinhas[etNT] - 1 do
          lines.Add('');
      end;
    end
    else if exportScope = etOT then
    begin
      if FEscopo = etOT then
        lines.Assign(projectVerses)
      else if FEscopo = etONT then
      begin
        for i := 0 to Min(QLinhas[etOT] - 1, projectVerses.Count - 1) do
          lines.Add(projectVerses[i]);
      end
      else if FEscopo = etNT then
      begin
        for i := 0 to QLinhas[etOT] - 1 do
          lines.Add('');
      end;
    end;

    while lines.Count < totalExpectedLines do
      lines.Add('');
    while lines.Count > totalExpectedLines do
      lines.Delete(lines.Count - 1);

    ExportTheWordBible(lines, destination, props);
    AtribuirInfo(Format('export.dest.panel.%d', [Ord(text)]), ExpandFileName(destination));
    Commit;
  finally
    projectVerses.Free;
    lines.Free;
  end;

  OpenDocument(ExtractFilePath(destination));
end;

procedure TProjeto.OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
begin
  if not FScrollEventsEnabled or not assigned(FTblPares) or FClosing or not assigned(Node) then
    exit;
  GoToReference(BibleTreeNodeToReference(Node));
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

      SalvarTexto;
      if Assigned(FAVersiculo[tbOrigem]) and FAVersiculo[tbOrigem].Modificado then
      begin
        SalvarAssociacoes;
        SalvarPares;
      end;
      if Assigned(FTblPares) and FTblPares.Active then
        FLastSavedVerseID := FTblPares.FieldByName('pare_id').AsString;

      for e in FOnNovoVersiculo do
        e(self);

      break;
    end;
end;

procedure TProjeto.OnLivePreviewVersiculo(Sender: TMemoVersiculo);
var
  e: TOnNovoVersiculoEvent;
begin
  for e in FOnNovoVersiculo do
    e(self);
end;

procedure TProjeto.OnTrocarSecaoEdit(SectionIndex: Integer);
var
  t: TTipoTextoBiblico;
begin
  if (SectionIndex < Ord(tbOrigem)) or (SectionIndex > Ord(tbConsulta2)) then
    Exit;
  t := TTipoTextoBiblico(SectionIndex);
  if Assigned(FAVersiculo[t]) then
    FAVersiculo[t].ViewMode := vvmEdit;
end;

procedure TProjeto.OnDestinoStatusChange(Sender: TVersiculo; NewStatus: Integer);
begin
  if not FAtivo or FClosing then Exit;
  if Assigned(FRadioGroupSituacao) and (FRadioGroupSituacao.ItemIndex <> NewStatus) then
    FRadioGroupSituacao.ItemIndex := NewStatus
  else
    SetSituacao(NewStatus);
  SyncDestinoStatusBar;
end;

procedure TProjeto.OnVerseNavigate(Sender: TVersiculo; Delta: Integer);
begin
  if not FAtivo or FClosing or not Assigned(FTblPares) then Exit;
  if Delta < 0 then
    VersiculoAnterior
  else if Delta > 0 then
    VersiculoSeguinte;
end;

function TProjeto.GetSelectedUnassociatedSourceSyntagm: TSyntagm;
var
  i: Integer;
  s: TSyntagm;
begin
  Result := nil;
  if (not FAtivo) or FClosing then Exit;
  if (not Assigned(FAVersiculo[tbOrigem])) or (not Assigned(FAVersiculo[tbDestino])) then Exit;
  if (FAVersiculo[tbOrigem].ViewMode <> vvmAssoc) or (FAVersiculo[tbDestino].ViewMode <> vvmAssoc) then Exit;
  if not Assigned(FAVersiculo[tbOrigem].Selecao) then Exit;

  for i := 0 to FAVersiculo[tbOrigem].Selecao.Count - 1 do
  begin
    s := FAVersiculo[tbOrigem].Selecao[i];
    if Assigned(s) and (s.Kind = tsSintagma) and (not s.IsAssociated) and (s.HasStrongs or s.HasMorph) then
    begin
      Result := s;
      Exit;
    end;
  end;
end;

function TProjeto.IsMouseInDestinationPanel(out CursorPos: TPoint): Boolean;
var
  dstPanel: TScrollBox;
  ptParent, ptClient: TPoint;
begin
  Result := False;
  CursorPos := Mouse.CursorPos;
  if (not FAtivo) or FClosing then Exit;
  if (not Assigned(FAVersiculo[tbDestino])) or (not Assigned(FAVersiculo[tbDestino].Painel)) then Exit;
  if Application.Terminated or (not Application.Active) then Exit;
  if Assigned(Screen.ActiveCustomForm) and (Screen.ActiveCustomForm <> Application.MainForm) and
     (Screen.ActiveCustomForm <> FrmDictionaryPopup) then Exit;

  dstPanel := FAVersiculo[tbDestino].Painel;
  if (not dstPanel.Showing) or (not dstPanel.Visible) or (not dstPanel.HandleAllocated) then Exit;

  if Assigned(dstPanel.Parent) and dstPanel.Parent.HandleAllocated then
  begin
    ptParent := dstPanel.Parent.ScreenToClient(CursorPos);
    Result := (ptParent.X >= dstPanel.Left) and (ptParent.X < dstPanel.Left + dstPanel.Width) and
              (ptParent.Y >= dstPanel.Top) and (ptParent.Y < dstPanel.Top + dstPanel.Height);
  end
  else
  begin
    ptClient := dstPanel.ScreenToClient(CursorPos);
    Result := (ptClient.X >= 0) and (ptClient.X < dstPanel.Width) and
              (ptClient.Y >= 0) and (ptClient.Y < dstPanel.Height);
  end;
end;

procedure TProjeto.PopulatePopupForSyntagm(s: TSyntagm; v: TTipoTextoBiblico);
var
  p: TSyntagm;
  m, tabLbl, rtf, def, engSubject: string;
  hasMorfo, hasStrong: Boolean;

  function CleanTagLabel(const rawTag: string): string;
  var
    spPos: Integer;
  begin
    Result := Trim(rawTag);
    spPos := Pos(' ', Result);
    if spPos > 1 then
      Result := Copy(Result, 1, spPos - 1);
  end;

begin
  if (s = nil) or (FrmDictionaryPopup = nil) then Exit;

  FrmDictionaryPopup.LimparConteudo;

  hasMorfo := (FUseGlobalDicts and assigned(FGlobalDicMorfo)) or assigned(FADicMorfo[v]);
  if hasMorfo then
  begin
    if Assigned(s.Morph) and (s.Morph.Count > 0) then
      for m in s.Morph do
      begin
        def := ObterDefinicaoMorfo(m, v);
        tabLbl := CleanTagLabel(m);
        if (def <> '') or (not FUseGlobalDicts) then
          FrmDictionaryPopup.AdicionarMorfo(tabLbl, def);
      end
    else if Assigned(s.Pairs) then
    begin // não tem morfo, vejamos se os pares têm
      for p in s.Pairs do
        if Assigned(p.Morph) then
          for m in p.Morph do
          begin
            def := ObterDefinicaoMorfo(m, v);
            tabLbl := CleanTagLabel(m);
            if (def <> '') or (not FUseGlobalDicts) then
              FrmDictionaryPopup.AdicionarMorfo(tabLbl, def);
          end;
    end;
  end;

  hasStrong := (FUseGlobalDicts and (assigned(FGlobalDicStrongGreek) or assigned(FGlobalDicStrongHebrew))) or assigned(FADicStrong[v]);
  if hasStrong then
  begin
    if Assigned(s.Strong) and (s.Strong.Count > 0) then
      for m in s.Strong do
      begin
        def := ObterDefinicaoStrong(m, v);
        tabLbl := CleanTagLabel(m);
        if (def <> '') or (not FUseGlobalDicts) then
          FrmDictionaryPopup.AdicionarStrong(tabLbl, def);
      end
    else if Assigned(s.Pairs) then
    begin // não tem strongs, vejamos se os pares têm
      for p in s.Pairs do
        if Assigned(p.Strong) then
          for m in p.Strong do
          begin
            def := ObterDefinicaoStrong(m, v);
            tabLbl := CleanTagLabel(m);
            if (def <> '') or (not FUseGlobalDicts) then
              FrmDictionaryPopup.AdicionarStrong(tabLbl, def);
          end;
    end;
  end;

  // English Word / Bible Dictionary lookup for any English word or name on any of the 4 Bible panels
  // that has no Strong's number tag (and no Strong's definition populated from an associated pair).
  if (not s.HasStrongs) and (FrmDictionaryPopup.Strongs.Count = 0) and
     FUseGlobalDicts and Assigned(FGlobalDicEnglish) then
  begin
    def := ObterDefinicaoPalavraIngles(s.Text, v, engSubject);
    if def <> '' then
    begin
      if engSubject = '' then
        engSubject := Trim(s.Text);
      FrmDictionaryPopup.AdicionarStrong(engSubject, def);
    end;
  end;

  if v = tbOrigem then
  begin
    rtf := FormatRTF(FSugeridor.GetTranslationAlternatives(s));
    if not rtf.IsEmpty then
      FrmDictionaryPopup.AdicionarStrong(STranslationMemory, rtf);
  end;
end;

procedure TProjeto.UpdateDestinationPopup;
var
  selSrc: TSyntagm;
  cursorPos: TPoint;
  inDest: Boolean;
begin
  if not Assigned(FrmDictionaryPopup) then Exit;

  selSrc := GetSelectedUnassociatedSourceSyntagm;
  inDest := IsMouseInDestinationPanel(cursorPos);

  if not inDest then
    FHoveredAssocDestSyntagm := nil;

  // When hovering an associated word inside the Destination text, disable the southeast popup
  // so the normal dictionary popup for that hovered Destination word works as usual.
  if inDest and Assigned(FHoveredAssocDestSyntagm) and FHoveredAssocDestSyntagm.IsAssociated then
  begin
    if FDestPopupActive then
    begin
      FDestPopupSyntagm := nil;
      FDestPopupActive := False;
      if FrmDictionaryPopup.Visible then
        FrmDictionaryPopup.Ocultar;
    end;
    Exit;
  end;

  if Assigned(selSrc) and inDest then
  begin
    FTemporizador.Enabled := False;

    if FDestPopupActive and FrmDictionaryPopup.Visible and (FDestPopupSyntagm = selSrc) then
    begin
      if (GetKeyState(VK_CONTROL) and $8000) = 0 then
        FrmDictionaryPopup.AtualizarPosicao(cursorPos.X + 18, cursorPos.Y + 18);
      Exit;
    end;

    PopulatePopupForSyntagm(selSrc, tbOrigem);
    if (FrmDictionaryPopup.Strongs.Count > 0) or (FrmDictionaryPopup.Morfos.Count > 0) then
    begin
      FDestPopupSyntagm := selSrc;
      FDestPopupActive := True;
      FrmDictionaryPopup.MostrarEm(cursorPos.X + 18, cursorPos.Y + 18);
    end
    else
    begin
      FDestPopupSyntagm := nil;
      FDestPopupActive := False;
      FrmDictionaryPopup.Ocultar;
    end;
  end
  else
  begin
    if FDestPopupActive then
    begin
      if FrmDictionaryPopup.Visible and
         ((GetKeyState(VK_CONTROL) and $8000) <> 0) and
         (cursorPos.X >= FrmDictionaryPopup.Left) and (cursorPos.X < FrmDictionaryPopup.Left + FrmDictionaryPopup.Width) and
         (cursorPos.Y >= FrmDictionaryPopup.Top) and (cursorPos.Y < FrmDictionaryPopup.Top + FrmDictionaryPopup.Height) then
        Exit;

      FDestPopupSyntagm := nil;
      FDestPopupActive := False;
      if FrmDictionaryPopup.Visible then
        FrmDictionaryPopup.Ocultar;
    end;
  end;
end;

procedure TProjeto.OnDestPopupTimer(Sender: TObject);
begin
  if (not FAtivo) or FClosing then
  begin
    if FDestPopupActive and Assigned(FrmDictionaryPopup) and FrmDictionaryPopup.Visible then
    begin
      FDestPopupSyntagm := nil;
      FDestPopupActive := False;
      FrmDictionaryPopup.Ocultar;
    end;
    Exit;
  end;
  UpdateDestinationPopup;
end;

procedure TProjeto.OnExibirDefinicao(Sender: TObject);
var
  v: TTipoTextoBiblico;
  point: TPoint;
  s: TSyntagm;
  isUnassocDestWord: Boolean;
begin
  if frmDictionaryPopup.Visible and (not FDestPopupActive) then
    exit;

  s := TSyntagm(TTimer(Sender).Tag);
  if s = nil then
    exit;

  isUnassocDestWord := Assigned(FAVersiculo[tbDestino]) and
                       (s.VerseRef = FAVersiculo[tbDestino]) and
                       (not s.IsAssociated);

  if isUnassocDestWord and Assigned(GetSelectedUnassociatedSourceSyntagm) then
  begin
    UpdateDestinationPopup;
    exit;
  end;

  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    if (FAVersiculo[v] = s.VerseRef) then
    begin
      PopulatePopupForSyntagm(s, v);

      if (FrmDictionaryPopup.Strongs.Count > 0) or (FrmDictionaryPopup.Morfos.Count > 0) then
      begin
        FDestPopupSyntagm := nil;
        FDestPopupActive := False;
        point := s.LabelRef.ClientToScreen(s.LabelRef.ClientRect.BottomRight);
        frmDictionaryPopup.MostrarEm(point.x, point.y);
      end;
      break;
    end;
  end;
end;

procedure TProjeto.OnDblClickVersiculo(Sender: TObject);
var
  v: TVersiculo;
begin
  if not Ativo or not assigned(Sender) then
    exit;

  v := TVersiculo(TScrollBox(Sender).Tag);
  if assigned(v) then
    v.ViewMode := vvmEdit
  else
    FMemoVersiculo.Ativar(TScrollBox(Sender), TVersiculo(TScrollBox(Sender).Tag));
end;

procedure TProjeto.OnVerseViewModeChange(Sender: TVersiculo; NewMode: TVerseViewMode);
var
  v, text: TTipoTextoBiblico;
  props, desc: string;
begin
  text := tbNone;
  for v := low(FAVersiculo) to high(FAVersiculo) do
    if Sender = FAVersiculo[v] then
    begin
      text := v;
      break;
    end;

  if text = tbNone then
    exit;

  case NewMode of
    vvmAssoc:
    begin
      if Sender.MemoPropsModified then
      begin
        AtribuirInfo(GetPropriedadesKey(text), Sender.MemoProps.Text);
        case text of
          tbOrigem:    AtribuirInfo('propriedades.origem', Sender.MemoProps.Text);
          tbDestino:   AtribuirInfo('propriedades.destino', Sender.MemoProps.Text);
          tbConsulta1: AtribuirInfo('propriedades.consulta1', Sender.MemoProps.Text);
          tbConsulta2: AtribuirInfo('propriedades.consulta2', Sender.MemoProps.Text);
        end;
        if Sender.TranslationName <> '' then
          AtribuirInfo(Format('description.text%d', [Integer(text)]), Sender.TranslationName);
        Sender.MemoPropsModified := False;
      end;

      if assigned(FMemoVersiculo) and (FMemoVersiculo.Versiculo = Sender) then
        FMemoVersiculo.Desativar;

      if assigned(Sender.MemoProps) then
        Sender.MemoProps.Visible := False;

      Sender.Renderizar;
      Sender.RepositionTabPanel;
    end;

    vvmEdit:
    begin
      if Sender.MemoPropsModified then
      begin
        AtribuirInfo(GetPropriedadesKey(text), Sender.MemoProps.Text);
        case text of
          tbOrigem:    AtribuirInfo('propriedades.origem', Sender.MemoProps.Text);
          tbDestino:   AtribuirInfo('propriedades.destino', Sender.MemoProps.Text);
          tbConsulta1: AtribuirInfo('propriedades.consulta1', Sender.MemoProps.Text);
          tbConsulta2: AtribuirInfo('propriedades.consulta2', Sender.MemoProps.Text);
        end;
        if Sender.TranslationName <> '' then
          AtribuirInfo(Format('description.text%d', [Integer(text)]), Sender.TranslationName);
        Sender.MemoPropsModified := False;
      end;

      if assigned(Sender.MemoProps) then
        Sender.MemoProps.Visible := False;

      if assigned(FMemoVersiculo) and assigned(Sender.Painel) then
      begin
        case text of
          tbOrigem:    desc := 'Source';
          tbDestino:   desc := 'Destination';
          tbConsulta1: desc := 'Reference 1';
          tbConsulta2: desc := 'Reference 2';
        else
          desc := 'Verse';
        end;
        FMemoVersiculo.SetActiveSectionInfo(Ord(text), desc);
        FMemoVersiculo.Ativar(Sender.Painel, Sender);
        Sender.RepositionTabPanel;
        if ChapterViewText <> text then
          ChapterViewText := text;
        OnLivePreviewVersiculo(FMemoVersiculo);
      end;
    end;

    vvmProps:
    begin
      if assigned(FMemoVersiculo) and (FMemoVersiculo.Versiculo = Sender) then
        FMemoVersiculo.Desativar;

      props := ObterInfo(GetPropriedadesKey(text));
      if props = '' then
      begin
        case text of
          tbOrigem:    props := ObterInfo('propriedades.origem');
          tbDestino:   props := ObterInfo('propriedades.destino');
          tbConsulta1: props := ObterInfo('propriedades.consulta1');
          tbConsulta2: props := ObterInfo('propriedades.consulta2');
        end;
      end;

      if Trim(props) = '' then
      begin
        desc := GetTextDescription(text);
        if desc = '' then
        begin
          case text of
            tbOrigem:    desc := 'Source Bible';
            tbDestino:   desc := 'Destination Bible';
            tbConsulta1: desc := 'Reference 1';
            tbConsulta2: desc := 'Reference 2';
          else
            desc := 'Bible';
          end;
        end;
        props := 'title=' + desc + LineEnding +
                 'short.title=' + desc + LineEnding +
                 'description=' + desc + LineEnding +
                 'version=1.0' + LineEnding;
        if FEscopo = etNT then
          props := props + 'ot=0' + LineEnding
        else if FEscopo = etOT then
          props := props + 'nt=0' + LineEnding;
      end;

      if assigned(Sender.MemoProps) then
      begin
        Sender.MemoProps.Text := props;
        Sender.MemoPropsModified := False;
        Sender.MemoProps.BringToFront;
        Sender.MemoProps.Visible := True;
        Sender.RepositionTabPanel;
      end;
    end;
  end;
end;

procedure TProjeto.OnBibleTreePopupItemClick(Sender: TObject);

  function SameReference(a, b: TReference): boolean;
  begin
    result := (a.Book = b.Book) and (a.BookID = b.BookID) and (a.Chapter = b.Chapter) and (a.Verse = b.Verse);
  end;

  procedure SetNodeStatus(node: TTreeNode; status: Integer);
  begin
    GoToReference(BibleTreeNodeToReference(node));
    SetSituacao(status);
    //node.ImageIndex := status;
  end;

  procedure SetSubTreeStatus(parent: TTreeNode; status: Integer);
  var
    node: TTreeNode;
  begin
    node := parent.GetFirstChild;
    while assigned(node) do
    begin
      if node.HasChildren then
      begin
        SetSubTreeStatus(node, status);
        if assigned(node.Parent) then // has children and parent, this is a chapter node
          node.ImageIndex := status;
      end
      else
        SetNodeStatus(node, status);
      node := node.GetNextSibling;
    end;
  end;

var
  item: TMenuItem;
begin
  item := TMenuItem(Sender);
  try
    StartScrollingSession;
    SetSubTreeStatus(FBibleTreePointedNode, item.Tag);
  finally
    FinishScrollingSession;
  end;

  Atualizar;
end;

procedure TProjeto.OnBibleTreeContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  FBibleTreePointedNode := (Sender as TTreeView).GetNodeAt(MousePos.X, MousePos.Y);
  if (not Ativo) or (FBibleTreePointedNode = nil) or (not FBibleTreePointedNode.HasChildren) then
    Handled := True;
end;

function TProjeto.AtribuirDicStrong(dic: string; t: TTipoTextoBiblico; alertaSeNaoExistir: Boolean = True): boolean;
//var
//  t1: TTipoTextoBiblico;
begin
  result := false;

  FAVersiculo[t].OnMouseEnter := @SintagmaOnMouseEnter;
  FAVersiculo[t].OnMouseLeave := @SintagmaOnMouseLeave;

  if dic = '' then
    exit;

  if not FileExists(dic) then
  begin
    if alertaSeNaoExistir then
      MessageDlg(SStrongDictionary, format(SDictionaryDoesntExist, [FExpand(dic)]), mtError, [mbOK], 0);
    exit;
  end;

  if not IsValidDicionaryModule(dic, 'strong') then
    exit;

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
    FADicStrong[t].SQL.Text := 'select data, data2 from topics, content where subject = :strong and id = topic_id';
    FADicStrong[t].Prepare;
    FADicStrong[t].Tag := 1;
  end;

  result := true;
end;

function TProjeto.AtribuirDicMorfo(dic: string; t: TTipoTextoBiblico; alertaSeNaoExistir: Boolean = True): boolean;
//var
//  t1: TTipoTextoBiblico;
begin
  result := false;

  if dic = '' then
    exit;

  if not FileExists(dic) then
  begin
    if alertaSeNaoExistir then
      MessageDlg(SMorphologyDictionary, format(SDictionaryDoesntExist, [FExpand(dic)]), mtError, [mbOK], 0);
    exit;
  end;

  if not IsValidDicionaryModule(dic, 'morph') then
    exit;

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
    FADicMorfo[t].SQL.Text := 'select data, data2 from topics, content where subject = :morf and id = topic_id';
    FADicMorfo[t].Prepare;
    FADicMorfo[t].Tag := 1;
 end;
  FAVersiculo[t].OnMouseEnter := @SintagmaOnMouseEnter;
  FAVersiculo[t].OnMouseLeave := @SintagmaOnMouseLeave;

  result := true;
end;

procedure TProjeto.AtualizarArvore;
var
  n, ul, uc: TTreeNode;
  qv, qc, l, c, v, s, curSit: smallint;
  sl, sc: array[0..3] of smallint;
  b: boolean;
  sitField: TField;
begin
  if not assigned(FArvore) or (FArvore.Items.Count = 0) then exit;

  ul := FArvore.Items[0];
  if not assigned(ul) then exit;
  uc := ul.GetFirstChild;
  if not assigned(uc) then exit;
  n := uc.GetFirstChild;
  if not assigned(n) then exit;

  l := OffsetLivros[FEscopo] + 1; // livro atual
  c := 1;  // capítulo atual
  v := 1;  // versículo atual
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

  FTotaisSituacao[0] := 0;
  FTotaisSituacao[1] := 0;
  FTotaisSituacao[2] := 0;
  FTotaisSituacao[3] := 0;

  sitField := FTblPares.FieldByName('pare_situacao');
  curSit := 0;

  FArvore.BeginUpdate;
  try
    StartScrollingSession;
    try
      FTblPares.First;

      while not FTblPares.EOF do
      begin
        if v > QVersiculos[FEscopo][qv] then // novo capítulo
        begin
          { atualizando nó do capítulo }
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
            if sc[2] > 0 then
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
              if sl[2] > 0 then
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

        curSit := sitField.AsInteger;
        if (curSit < 0) or (curSit > 3) then
          curSit := 0;
        inc(sc[curSit]);
        inc(FTotaisSituacao[curSit]);
        n.ImageIndex := curSit;
        n.SelectedIndex := curSit + 4;

        FTblPares.Next;
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
        if sc[2] > 0 then
          uc.ImageIndex := 2
        else
          uc.ImageIndex := 1;

        uc.SelectedIndex := uc.ImageIndex + 4;
      end;

      { atualizando último livro - Apocalipse }
      b := false;
      inc(sl[curSit]);
      for s:=0 to 3 do // todos os capítulos estão na mesma situação?
        if sl[s] = QCapitulos[FEscopo][qc] then
        begin
          ul.ImageIndex := s; ul.SelectedIndex := ul.ImageIndex + 4;
          b := true;
          break;
        end;
      if not b then // várias situações diferentes
      begin
        if sl[2] > 0 then
          ul.ImageIndex := 2
        else
          ul.ImageIndex := 1;
        ul.SelectedIndex := ul.ImageIndex + 4;
      end;
    finally
      FinishScrollingSession;
    end;
  finally
    FArvore.EndUpdate;
  end;
end;

procedure TProjeto.AtualizarArvore(id: string);
var
  i: smallint;
  al, ac: array[0..3] of smallint;
  nl, nc, nv, n: TTreeNode;
  b: boolean;
  targetBook: string;
begin
  if not assigned(FArvore) or FClosing or (FArvore.Items.Count = 0) then
    exit;

  if (FFilterStatus >= 0) and (FFilterStatus <= 3) then
  begin
    targetBook := NLivros[FEscopo][BookID-OffsetLivros[FEscopo]-1];
    nl := FArvore.Items[0];
    while assigned(nl) and (nl.Text <> targetBook) do
      nl := nl.GetNextSibling;
    if assigned(nl) then
    begin
      nc := nl.GetFirstChild;
      while assigned(nc) and (nc.Text <> IntToStr(Chapter)) do
        nc := nc.GetNextSibling;
      if assigned(nc) then
      begin
        nv := nc.GetFirstChild;
        while assigned(nv) and (nv.Text <> IntToStr(Verse)) do
          nv := nv.GetNextSibling;
        if assigned(nv) then
        begin
          nv.ImageIndex := Situacao;
          nv.SelectedIndex := nv.ImageIndex + 4;
        end;
      end;
    end;
    exit;
  end;

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
  b, c, v: smallint;
  targetBook: string;
  nl, nc, nv: TTreeNode;
  oldOnChange: TTVChangedEvent;
begin
  if not assigned(FArvore) or FClosing or (FArvore.Items.Count = 0) then
    exit;

  if SScanf(id, '%d,%d,%d', [@b, @c, @v]) <> 3 then
    exit;
  if (b - OffsetLivros[FEscopo] - 1 < 0) or (b - OffsetLivros[FEscopo] - 1 >= QLivros[FEscopo]) then
    exit;
  targetBook := NLivros[FEscopo][b-OffsetLivros[FEscopo]-1];

  nl := FArvore.Items[0];
  while assigned(nl) and (nl.Text <> targetBook) do
    nl := nl.GetNextSibling;

  if not assigned(nl) then exit;

  nc := nl.GetFirstChild;
  while assigned(nc) and (nc.Text <> IntToStr(c)) do
    nc := nc.GetNextSibling;

  if not assigned(nc) then exit;

  nv := nc.GetFirstChild;
  while assigned(nv) and (nv.Text <> IntToStr(v)) do
    nv := nv.GetNextSibling;

  oldOnChange := FArvore.OnChange;
  FArvore.OnChange := nil;
  try
    if assigned(nv) then
      nv.Selected := true;
  finally
    FArvore.OnChange := oldOnChange;
  end;
end;

procedure TProjeto.SetFilterStatus(AValue: Integer);
begin
  if FFilterStatus = AValue then exit;
  FiltrarArvore(AValue);
end;

procedure TProjeto.FiltrarArvore(AStatus: Integer);
var
  b, c, v: smallint;
  curBookID, curChapter: smallint;
  nl, nc, nv: TTreeNode;
  bName: string;
begin
  if not assigned(FArvore) or FClosing then exit;

  FFilterStatus := AStatus;
  FArvore.OnChange := nil;
  FArvore.BeginUpdate;
  try
    FArvore.Items.Clear;

    if (AStatus < 0) or (AStatus > 3) then
    begin
      PreencherArvore;
      FArvore.OnChange := nil;
      AtualizarArvore;
      AtualizarNomesTraducao;
    end
    else
    begin
      curBookID := -1;
      curChapter := -1;
      nl := nil;
      nc := nil;

      StartScrollingSession;
      try
        FTblPares.First;
        while not FTblPares.EOF do
        begin
          if FTblPares.FieldByName('pare_situacao').AsInteger = AStatus then
          begin
            SScanf(FTblPares.FieldByName('pare_id').AsString, '%d,%d,%d', [@b, @c, @v]);

            if b <> curBookID then
            begin
              curBookID := b;
              curChapter := -1;
              bName := NLivros[FEscopo][b - OffsetLivros[FEscopo] - 1];
              nl := FArvore.Items.Add(nil, bName);
              nl.ImageIndex := AStatus;
              nl.SelectedIndex := AStatus + 4;
            end;

            if c <> curChapter then
            begin
              curChapter := c;
              nc := FArvore.Items.AddChild(nl, IntToStr(c));
              nc.ImageIndex := AStatus;
              nc.SelectedIndex := AStatus + 4;
            end;

            nv := FArvore.Items.AddChild(nc, IntToStr(v));
            nv.ImageIndex := AStatus;
            nv.SelectedIndex := AStatus + 4;
          end;
          FTblPares.Next;
        end;
      finally
        FinishScrollingSession;
      end;

      FArvore.FullExpand;
    end;
  finally
    FArvore.EndUpdate;
    SelectTreeNode(ID);
    FArvore.OnChange := @OnMudancaVersiculo;
  end;

  {$IFDEF WINDOWS}
  HookTreeViewControl(FArvore);
  {$ENDIF}
  ApplyDarkModeToControl(FArvore, CurrentSkin.IsDark);
end;

function TProjeto.IrParaVersiculoSeguinteComSituacao(AStatus: Integer): boolean;
var
  originalMarker: string;
begin
  result := false;
  if not assigned(FTblPares) or FTblPares.EOF then exit;

  originalMarker := ID;
  while not FTblPares.EOF do
  begin
    VersiculoSeguinte;
    if (not FTblPares.EOF) and (Situacao = AStatus) then
    begin
      result := true;
      exit;
    end;
  end;

  GoToReference(originalMarker);
end;

function TProjeto.IrParaVersiculoAnteriorComSituacao(AStatus: Integer): boolean;
var
  originalMarker: string;
begin
  result := false;
  if not assigned(FTblPares) or FTblPares.BOF then exit;

  originalMarker := ID;
  while not FTblPares.BOF do
  begin
    VersiculoAnterior;
    if (not FTblPares.BOF) and (Situacao = AStatus) then
    begin
      result := true;
      exit;
    end;
  end;

  GoToReference(originalMarker);
end;

function TProjeto.ContarVersiculosPorSituacao(AStatus: Integer): Integer;
begin
  if (AStatus >= 0) and (AStatus <= 3) then
    result := FTotaisSituacao[AStatus]
  else
    result := 0;
end;

function TProjeto.ObterTotalVersiculos: Integer;
begin
  result := FTotaisSituacao[0] + FTotaisSituacao[1] + FTotaisSituacao[2] + FTotaisSituacao[3];
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
    if v in [tbOrigem..tbConsulta2] then
      FUseDefaultDict[v] := True;
  end;

  FUseGlobalDicts := True;
  FGlobalDicStrongGreekPath := '';
  FGlobalDicStrongHebrewPath := '';
  FGlobalDicMorfoPath := '';
  FGlobalDicEnglishPath := '';
  FGlobalDicStrongGreek := nil;
  FGlobalDicStrongHebrew := nil;
  FGlobalDicMorfo := nil;
  FGlobalDicEnglish := nil;

  FLoadedVerseID      := '';
  FLastSavedVerseID   := '';
  FScrollingSessionDepth := 0;
  FParesAntigos       := nil;
  FOnNovoVersiculo    := TOnNovoVersiculoEvents.Create;
  FOnAlterarVersiculo := nil;
  FSugeridor          := nil;
  FTblPares           := nil;
  FTblInfo            := nil;
  FEscopo             := etNone;
  FClosing            := false;
  FVerseStrongsCountMode := scNone;
  FFilterStatus := -1;
  FTotaisSituacao[0] := 0;
  FTotaisSituacao[1] := 0;
  FTotaisSituacao[2] := 0;
  FTotaisSituacao[3] := 0;

  FTemporizador          := TTimer.Create(nil);
  FTemporizador.Enabled  := false;
  FTemporizador.Interval := 300;
  FTemporizador.OnTimer  := @OnExibirDefinicao;

  FDestPopupSyntagm      := nil;
  FHoveredAssocDestSyntagm := nil;
  FDestPopupActive       := False;
  FDestPopupTimer        := TTimer.Create(nil);
  FDestPopupTimer.Enabled  := false;
  FDestPopupTimer.Interval := 40;
  FDestPopupTimer.OnTimer  := @OnDestPopupTimer;

  FMemoVersiculo := TMemoVersiculo.Criar;
  FMemoVersiculo.QuandoModificarVersiculo := @OnAlterarTextoVersiculo;
  FMemoVersiculo.QuandoLivePreview := @OnLivePreviewVersiculo;
  FMemoVersiculo.QuandoTrocarSecaoEdit := @OnTrocarSecaoEdit;
end;

constructor TProjeto.Criar(paineis: array of TScrollbox; navegador: TTreeView;
  rsituacao: TRadioGroup; rcomentarios: TMemo);
var
  v: integer;
begin
  Criar;

  if (Length(paineis) > 0) and Assigned(paineis[0]) and Assigned(paineis[0].Parent) then
  begin
    if Assigned(paineis[0].Parent.Parent) then
      FMemoVersiculo.AttachRibbonHost(paineis[0].Parent.Parent)
    else
      FMemoVersiculo.AttachRibbonHost(paineis[0].Parent);
  end;

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
  LiberarObjetoQuery(FGlobalDicStrongGreek);
  LiberarObjetoQuery(FGlobalDicStrongHebrew);
  LiberarObjetoQuery(FGlobalDicMorfo);
  LiberarObjetoQuery(FGlobalDicEnglish);

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
  FDestPopupTimer.Enabled := False;
  FDestPopupTimer.Free;
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
  i: Integer;
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
    s := ObterInfo(format('usedefaultdict%d', [t]));
    if s = '' then
      FUseDefaultDict[t] := True
    else
      FUseDefaultDict[t] := (s = '1') or (s = 'true') or (s = 'True');

    { se o módulo usa dicionários padrão, o dicionário por módulo está inativo; não carregar nem alertar }
    if not FUseDefaultDict[t] then
    begin
      AtribuirDicStrong(ObterInfo(format('dicstrong%d', [t])), t, false);
      AtribuirDicMorfo(ObterInfo(format('dicmorfo%d', [t])), t, false);
    end;
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

  HealMisplacedPairsInDatabase;

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
  begin
    // Repopulate RadioGroupStatus with the correct status descriptions for this project
    with FRadioGroupSituacao.Items do
    begin
      Clear;
      for i := 0 to 3 do
        Add(GetSituacaoDescricao(i));
    end;
    FRadioGroupSituacao.Enabled := true;
    FRadioGroupSituacao.Color := CurrentSkin.PanelBg;
    FRadioGroupSituacao.Font.Color := CurrentSkin.PanelText;
  end;
  SyncDestinoStatusBar;

  FTblPares.BeforeScroll := @PreRolagemVersiculo;
  FTblPares.AfterScroll  := @PosRolagemVersiculo;
  FArvore.OnChange       := @OnMudancaVersiculo;
  FArvore.OnContextPopup := @OnBibleTreeContextPopup;
  FArvore.PopupMenu := CreateBibleTreeContextMenu;

  // set tokenizer version
  TTokenizerFactory.PreferredVersion := StrToIntDef(ObterInfo('tokenizer_version'), 1);

  FLoadedVerseID := '';
  FLastSavedVerseID := '';
  FScrollingSessionDepth := 0;
  ScrollEventsEnabled := true;
  GoToReference(ObterInfo('marcador'));
  if FLoadedVerseID = '' then
    PosRolagemVersiculo(FTblPares);

  FAtivo := true;
  FDisplayTags := false;
  FDestPopupSyntagm := nil;
  FHoveredAssocDestSyntagm := nil;
  FDestPopupActive := False;
  FDestPopupTimer.Enabled := True;
  AtualizarNomesTraducao;
  ApplySkin(CurrentSkin);
end;

procedure TProjeto.Fechar(_Commit: boolean);
var
  t: TTipoTextoBiblico;
begin
  FClosing := true;
  if Assigned(FDestPopupTimer) then
    FDestPopupTimer.Enabled := False;
  FDestPopupSyntagm := nil;
  FHoveredAssocDestSyntagm := nil;
  FDestPopupActive := False;
  if Assigned(FrmDictionaryPopup) and FrmDictionaryPopup.Visible then
    FrmDictionaryPopup.Ocultar;

  if assigned(FArvore) then
  begin
    //FArvore.FullCollapse;
    FArvore.Enabled := false;
    FArvore.Items.Clear;
  end;

  PreRolagemVersiculo(nil);

  if _Commit then
  begin
    AtribuirInfo('marcador', FTblPares.FieldByName('pare_id').AsString);
    Commit;
  end
  else
    Rollback;

  // saving modified properties and font sizes
  for t:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[t]) then
    begin
      if FAVersiculo[t].MemoPropsModified then
      begin
        AtribuirInfo(GetPropriedadesKey(t), FAVersiculo[t].MemoProps.Text);
        case t of
          tbOrigem:    AtribuirInfo('propriedades.origem', FAVersiculo[t].MemoProps.Text);
          tbDestino:   AtribuirInfo('propriedades.destino', FAVersiculo[t].MemoProps.Text);
          tbConsulta1: AtribuirInfo('propriedades.consulta1', FAVersiculo[t].MemoProps.Text);
          tbConsulta2: AtribuirInfo('propriedades.consulta2', FAVersiculo[t].MemoProps.Text);
        end;
        if FAVersiculo[t].TranslationName <> '' then
          AtribuirInfo(Format('description.text%d', [Integer(t)]), FAVersiculo[t].TranslationName);
        FAVersiculo[t].MemoPropsModified := False;
      end;
      AtribuirInfo(format('fonte%d.tamanho', [t]), IntToStr(FAVersiculo[t].Fonte.Size));
    end;
  Commit;

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
    FRadioGroupSituacao.Items.Clear;
  end;

  FClosing := false;
  FAtivo := false;
end;

procedure TProjeto.Commit;
begin
  if assigned(FSugeridor) then
  begin
    try
      FSugeridor.Commit;
    except
      on E: Exception do ;
    end;
  end;

  if assigned(FTblPares) and FTblPares.Active and FTblPares.UpdatesPending then
  begin
    try
      FTblPares.ApplyUpdates;
    except
      on E: Exception do
        DebugLn('FTblPares.ApplyUpdates warning: ' + E.Message);
    end;
  end;

  if assigned(FTblInfo) and FTblInfo.Active and FTblInfo.UpdatesPending then
  begin
    try
      FTblInfo.ApplyUpdates;
    except
      on E: Exception do ;
    end;
  end;
end;

procedure TProjeto.Rollback;
begin
  if assigned(FTblPares) then
    FTblPares.ClearUpdates;
  if assigned(FTblInfo) then
    FTblInfo.ClearUpdates;
  if assigned(FSugeridor) then
    FSugeridor.Rollback;
end;

procedure TProjeto.GoToReference(reference: string);
begin
  if reference.IsEmpty then
    reference := format('%d,1,1', [OffsetLivros[FEscopo] + 1]);

  if ID = reference then
  begin // if FTblPares.Locate is not called, PosRolagemVersiculo won't either
    UpdateReference(reference);
    if FScrollEventsEnabled and (FLoadedVerseID <> reference) then
      PosRolagemVersiculo(FTblPares);
  end
  else
  begin
    FTblPares.Locate('pare_id', reference, []);
    if ID <> reference then
      raise Exception.Create(Format('Reference not found in project [%s]', [reference]));
    if not FScrollEventsEnabled then
      UpdateReference(reference)
    else if FLoadedVerseID <> reference then
      PosRolagemVersiculo(FTblPares);
  end;
end;

procedure TProjeto.GoToReference(reference: TReference);
begin
  GoToReference(format('%d,%d,%d', [reference.BookID, reference.Chapter, reference.Verse]));
end;

procedure TProjeto.VersiculoSeguinte;
begin
  if not FTblPares.EOF then
    FTblPares.Next;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) and (not HasSituacaoCustomizada) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoAnterior;
begin
  if not FTblPares.BOF then
    FTblPares.Prior;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) and (not HasSituacaoCustomizada) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoInicial;
begin
  if not FTblPares.BOF then
    FTblPares.First;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) and (not HasSituacaoCustomizada) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoFinal;
begin
  if not FTblPares.EOF then
    FTblPares.Last;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) and (not HasSituacaoCustomizada) then
    SugerirAssociacao;
end;

procedure TProjeto.Atualizar;
begin
  PosRolagemVersiculo(FTblPares);
end;

procedure TProjeto.ApplySkin(const ASkin: TSkinColors);
var
  t: TTipoTextoBiblico;
begin
  for t := low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[t]) then
    begin
      FAVersiculo[t].ApplySkin(ASkin);
      if t in [tbConsulta1, tbConsulta2] then
        FAVersiculo[t].CorDesassociado := ASkin.WindowText;
    end;
  if assigned(FMemoVersiculo) then
    FMemoVersiculo.ApplySkin(ASkin);
  if assigned(FArvore) then
  begin
    FArvore.ParentColor := False;
    FArvore.Color := ASkin.TreeBg;
    FArvore.Font.Color := ASkin.TreeText;
    FArvore.ExpandSignColor := ASkin.TreeText;
    FArvore.TreeLineColor := ASkin.PanelBorder;
    {$IFDEF WINDOWS}
    HookTreeViewControl(FArvore);
    {$ENDIF}
    ApplyDarkModeToControl(FArvore, ASkin.IsDark);
    FArvore.Invalidate;
  end;
  if assigned(FMemoComentarios) then
  begin
    FMemoComentarios.Color := ASkin.WindowBg;
    FMemoComentarios.Font.Color := ASkin.WindowText;
    ApplyDarkModeToControl(FMemoComentarios, ASkin.IsDark);
  end;
  if assigned(FRadioGroupSituacao) then
  begin
    FRadioGroupSituacao.Color := ASkin.PanelBg;
    FRadioGroupSituacao.Font.Color := ASkin.PanelText;
  end;
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
  if Assigned(FTblPares) and FTblPares.Active then
    FLastSavedVerseID := FTblPares.FieldByName('pare_id').AsString;
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
  FAVersiculo[tbOrigem].Renderizar;
  FAVersiculo[tbDestino].Renderizar;

  if HasSituacaoCustomizada then exit;

  if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AndamentoAssociacao > 0) then
    FRadioGroupSituacao.ItemIndex := 1; // associando
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
      HealMisplacedPairsInDatabase;
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
  i, linesOffset, projectOffset: Integer;
  reVazio, reComments, reDescription, reCharset, reVerseRules, verseRule: IRegex;
  mtVerseRules, mtDescription: IMatch;
  rxVerseRulesFrom: TRegexList;
  rxVerseRulesTo: TStringList;
  propriedades: TStringStream;
  m: Integer;
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
      MessageDlg(arquivo, SModuleDoesNotIncludeScope, mtError, [mbOK], 0);
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
      MessageDlg(arquivo, SModuleDoesNotIncludeScope, mtError, [mbOK], 0);
      exit;
    end;
  end else
  begin
    MessageDlg(arquivo, SInvalidModuleType, mtError, [mbOK], 0);
    exit;
  end;

  try
    modulo := TStringList.Create;
    modulo.LoadFromFile(arquivo);

    if modulo.Count < QLinhas[moduleScope] then
    begin
      MessageDlg(arquivo, format(SInvalidLineCount, [QLinhas[moduleScope]]), mtError, [mbOK], 0);
      exit;
    end;

    reVazio        := RegexCreate('^\s*$', [rcoUTF8]);
    reComments     := RegexCreate('\s*#.*$', [rcoUTF8]);
    reDescription  := RegexCreate('^\s*description\s*=\s*(.*)$', [rcoUTF8]);
    reCharset      := RegexCreate('^\s*charset\s*=\s*(.*)$', [rcoUTF8]);
    reVerseRules   := RegexCreate('^\s*verse.rule\s*=\s*"(.*?)(?<!")"(?!")\s+"(.*?)"(?=\s*$|\s+"(.*?)(?<!")"(?!"))', [rcoUTF8]);

    FrmEscolherVerseRules.Reset;
    propriedades := TStringStream.Create('');

    AtribuirInfo(Format('moduletype.%d', [Ord(texto)]), LowerCase(ExtractFileExt(arquivo)));
    AtribuirInfo(Format('modulepath.%d', [Ord(texto)]), arquivo);
    case texto of
      tbOrigem:
      begin
        AtribuirInfo('moduletype.origem', LowerCase(ExtractFileExt(arquivo)));
        AtribuirInfo('modulepath.origem', arquivo);
      end;
      tbDestino:
      begin
        AtribuirInfo('moduletype.destino', LowerCase(ExtractFileExt(arquivo)));
        AtribuirInfo('modulepath.destino', arquivo);
      end;
      tbConsulta1:
      begin
        AtribuirInfo('moduletype.consulta1', LowerCase(ExtractFileExt(arquivo)));
        AtribuirInfo('modulepath.consulta1', arquivo);
      end;
      tbConsulta2:
      begin
        AtribuirInfo('moduletype.consulta2', LowerCase(ExtractFileExt(arquivo)));
        AtribuirInfo('modulepath.consulta2', arquivo);
      end;
    end;

    for i:=QLinhas[moduleScope] to modulo.Count-1 do
    begin
      propriedades.WriteString(modulo[i] + #13#10);

      line := SanitizeUTF8String(modulo[i]);
      { eliminando comentários }
      line := reComments.Replace(line, '');

      if reVazio.IsMatch(line) then
        continue;

      if reCharset.IsMatch(line) then
      begin
        if MessageDlg(arquivo, SPotentialNonUTF8ModuleWarning, mtWarning, [mbIgnore, mbAbort], 0) = mrAbort then
          exit;
      end;

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

    AtribuirInfo(Format('propriedades.%d', [Ord(texto)]), propriedades.DataString);
    case texto of
      tbOrigem:  AtribuirInfo('propriedades.origem', propriedades.DataString);
      tbDestino: AtribuirInfo('propriedades.destino', propriedades.DataString);
      tbConsulta1: AtribuirInfo('propriedades.consulta1', propriedades.DataString);
      tbConsulta2: AtribuirInfo('propriedades.consulta2', propriedades.DataString);
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

      if (i mod 100) = 0 then
      begin
        if FTblPares.UpdatesPending then
        begin
          try
            FTblPares.ApplyUpdates;
          except
            on E: Exception do
              DebugLn('AplicarPatch batch warning at ' + IntToStr(i) + ': ' + E.Message);
          end;
        end;
      end;

      if assigned(pb) then
      begin
        pb.StepIt;
        if (pb.Position mod 75) = 0 then
           Application.ProcessMessages;
      end;
    end;
    if FTblPares.UpdatesPending then
    begin
      try
        FTblPares.ApplyUpdates;
      except
        on E: Exception do
          DebugLn('ApplyUpdates final warning: ' + E.Message);
      end;
    end;
    FinishScrollingSession;
    FExportando := false;
    AtualizarArvore;
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
  FAVersiculo[texto].OnExportBibleModule := @OnExportBibleModule;
  FAVersiculo[texto].OnViewModeChange := @OnVerseViewModeChange;
  FAVersiculo[texto].OnNavigateVerse := @OnVerseNavigate;
  FAVersiculo[texto].TranslationName := GetTranslationName(texto);
  FAVersiculo[texto].OnMouseEnter := @SintagmaOnMouseEnter;
  FAVersiculo[texto].OnMouseLeave := @SintagmaOnMouseLeave;

  //FAVersiculo[texto].MostrarDicas := true;
  if texto in [tbConsulta1, tbConsulta2] then
    FAVersiculo[texto].CorDesassociado := CurrentSkin.WindowText;

  if texto = tbDestino then
  begin
    FAVersiculo[tbDestino].ShowStatusBar := True;
    FAVersiculo[tbDestino].OnStatusChange := @OnDestinoStatusChange;
    SyncDestinoStatusBar;
  end;

  //if texto = tbOrigem then
  //  FAVersiculo[texto].StrongMorfoComoChave := true;

  FAVersiculo[texto].IsSourceVerse := (texto = tbOrigem);

  if (texto = tbDestino) and assigned(FAVersiculo[tbOrigem]) then
    FAVersiculo[tbDestino].VersiculoPar := FAVersiculo[tbOrigem]
  else if (texto = tbOrigem) and assigned(FAVersiculo[tbDestino]) then
    FAVersiculo[tbOrigem].VersiculoPar := FAVersiculo[tbDestino];
end;

function TProjeto.HasPendingAutoSelectNext: Boolean;
begin
  Result := (Assigned(FAVersiculo[tbOrigem]) and FAVersiculo[tbOrigem].PendingAutoSelectNext) or
            (Assigned(FAVersiculo[tbDestino]) and FAVersiculo[tbDestino].PendingAutoSelectNext);
end;

procedure TProjeto.CompletePendingAutoSelectNext;
begin
  if Assigned(FAVersiculo[tbOrigem]) and
     (FAVersiculo[tbOrigem].PendingAutoSelectNext or
      (Assigned(FAVersiculo[tbDestino]) and FAVersiculo[tbDestino].PendingAutoSelectNext)) then
    FAVersiculo[tbOrigem].CompletePendingAutoSelectNext
  else if Assigned(FAVersiculo[tbDestino]) and FAVersiculo[tbDestino].PendingAutoSelectNext then
    FAVersiculo[tbDestino].CompletePendingAutoSelectNext;
end;

procedure TProjeto.UpdateVerseNavHoverStates;
var
  t: TTipoTextoBiblico;
begin
  for t := low(FAVersiculo) to high(FAVersiculo) do
    if Assigned(FAVersiculo[t]) then
      FAVersiculo[t].UpdateNavHoverState;
end;

function TProjeto.HandleMouseWheelForSourceWords(Shift: TShiftState; WheelDelta: Integer): Boolean;
var
  inSrc, inDst: Boolean;
begin
  Result := False;
  if (not FAtivo) or FClosing or (WheelDelta = 0) then Exit;
  if (Shift * [ssCtrl, ssShift, ssAlt] <> []) or
     ((GetKeyState(VK_CONTROL) and $8000) <> 0) or
     ((GetKeyState(VK_SHIFT) and $8000) <> 0) then Exit;
  if (not Assigned(FAVersiculo[tbOrigem])) or (not Assigned(FAVersiculo[tbOrigem].Painel)) then Exit;

  inSrc := IsMouseCursorInControl(FAVersiculo[tbOrigem].Painel);
  inDst := Assigned(FAVersiculo[tbDestino]) and Assigned(FAVersiculo[tbDestino].Painel) and
           IsMouseCursorInControl(FAVersiculo[tbDestino].Painel);

  if (not inSrc) and (not inDst) then Exit;

  if inSrc and (FAVersiculo[tbOrigem].ViewMode <> vvmAssoc) then Exit;
  if inDst and (FAVersiculo[tbDestino].ViewMode <> vvmAssoc) then Exit;

  if WheelDelta > 0 then
    Result := FAVersiculo[tbOrigem].SelectSourceWordByDelta(-1)
  else
    Result := FAVersiculo[tbOrigem].SelectSourceWordByDelta(1);
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
    HealMisplacedPairsInDatabase;
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
            AnsiReplaceStr(
              AnsiReplaceStr(
              AnsiReplaceStr(FTblPares.FieldByName('pare_comentarios').AsString, #13#10, '<CM>'),
              #13, '<CM>'),
            #10, '<CM>') + '<Rf>';

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
        if (Pos('<WG', lines[0]) = 0) and     { no Greek tags in the first verse? }
           (Pos('<WH', lines[0]) = 0) and     { nor Hebrew tags? }
           (Pos('<WT', lines[0]) = 0) then    { nor Morphology tags? }
          lines[0] := lines[0] + '<_MORPH_>'; { making sure theWord will know the module has tags }

        ExportTheWordBible(lines, arquivo, ResgatarInfo('propriedades.destino'));
      end;
    except
      on E: Exception do MessageDlg(SError, SExportToFileError + #13#10 + E.Message, mtError, [mbOK], 0);
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
    HealMisplacedPairsInDatabase;
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
    HealMisplacedPairsInDatabase;
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
var
  clearCount: Integer;
begin
  StartScrollingSession;
  try
    with FTblPares do
    begin
      VersiculoInicial;
    clearCount := 0;
    while not FTblPares.EOF do
    begin
      SetVerseText('', texto, false);
      VersiculoSeguinte;
      inc(clearCount);
      if (clearCount mod 100) = 0 then
      begin
        if FTblPares.UpdatesPending then
        begin
          try
            FTblPares.ApplyUpdates;
          except
            on E: Exception do
              DebugLn('LimparTexto batch warning at ' + IntToStr(clearCount) + ': ' + E.Message);
          end;
        end;
      end;
    end;
    if FTblPares.UpdatesPending then
    begin
      try
        FTblPares.ApplyUpdates;
      except
        on E: Exception do
          DebugLn('LimparTexto final warning: ' + E.Message);
      end;
    end;
    end;
  finally
    FinishScrollingSession;
  end;
  SetTextDescription(texto, '');
  AtribuirInfo(Format('moduletype.%d', [Ord(texto)]), '');
  AtribuirInfo(Format('modulepath.%d', [Ord(texto)]), '');
  AtribuirInfo(Format('propriedades.%d', [Ord(texto)]), '');
  case texto of
    tbOrigem:
    begin
      AtribuirInfo('moduletype.origem', '');
      AtribuirInfo('modulepath.origem', '');
      AtribuirInfo('propriedades.origem', '');
    end;
    tbDestino:
    begin
      AtribuirInfo('moduletype.destino', '');
      AtribuirInfo('modulepath.destino', '');
      AtribuirInfo('propriedades.destino', '');
    end;
    tbConsulta1:
    begin
      AtribuirInfo('moduletype.consulta1', '');
      AtribuirInfo('modulepath.consulta1', '');
      AtribuirInfo('propriedades.consulta1', '');
    end;
    tbConsulta2:
    begin
      AtribuirInfo('moduletype.consulta2', '');
      AtribuirInfo('modulepath.consulta2', '');
      AtribuirInfo('propriedades.consulta2', '');
    end;
  end;
  AtualizarArvore;
  AtualizarNomesTraducao;
end;

function TProjeto.ObterFonteTexto(texto: TTipoTextoBiblico): TFont;
begin
   if assigned(FAVersiculo[texto]) then
     result := FAVersiculo[texto].Fonte
   else
     result := nil;
end;

function TProjeto.AtribuirDicStrong(dic: string; textos: STextosBiblicos
  ): boolean;
var
  t: TTipoTextoBiblico;
begin
  result := false;
  for t:=tbOrigem to tbConsulta2 do
  begin
    if t in textos then
    begin
      if not AtribuirDicStrong(dic, t) then
        exit;
      AtribuirInfo(format('dicstrong%d', [t]), dic);
    end;
  end;
  result := true;
end;

function TProjeto.AtribuirDicMorfo(dic: string; textos: STextosBiblicos
  ): boolean;
var
  t: TTipoTextoBiblico;
begin
  result := false;
  for t:=tbOrigem to tbConsulta2 do
  begin
    if t in textos then
    begin
      if not AtribuirDicMorfo(dic, t) then
        exit;
      AtribuirInfo(format('dicmorfo%d', [t]), dic);
    end;
  end;
  result := true;
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
    GoToReference(Referencia);
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
  varredorXML := TTokenizerFactory.CreatePreferredTokenizer(FTblPares.Fields[FACamposTexto[texto]].AsString);
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
  varredorXML.Destroy;
end;

function TProjeto.GetTranslationSuggestions(syntagm: TSyntagm): string;
begin
  result := FSugeridor.GetTranslationAlternatives(syntagm);
end;

procedure TProjeto.Translate;
var
  node: TTreeNode;
  bk: Integer;
  mnu: TPopupMenu;
begin
  UpdateDefaultSituacaoDescricao;
  SyncDestinoStatusBar;
  if not assigned(FArvore) or (FArvore.Items.Count = 0) or (FEscopo = etNone) then
    exit;

  // Safely translate whatever book nodes currently exist in the tree
  node := FArvore.Items[0];
  while assigned(node) do
  begin
    if node.Parent = nil then
    begin
      bk := PtrInt(node.Data);
      if (bk > 0) and (bk <= 66) then
      begin
        if (bk - OffsetLivros[FEscopo] - 1 >= 0) and (bk - OffsetLivros[FEscopo] - 1 < QLivros[FEscopo]) then
          node.Text := NLivros[FEscopo][bk - OffsetLivros[FEscopo] - 1];
      end
      else if (node.Index >= 0) and (node.Index < QLivros[FEscopo]) then
        node.Text := NLivros[FEscopo][node.Index];
    end;
    node := node.GetNextSibling;
  end;

  if assigned(FArvore.PopupMenu) then
  begin
    mnu := FArvore.PopupMenu;
    FArvore.PopupMenu := nil;
    mnu.Free;
  end;
  FArvore.PopupMenu := CreateBibleTreeContextMenu;
end;

procedure TProjeto.ToggleDisplayTags;
begin
  SetDisplayTags(not DisplayTags);
end;

procedure TProjeto.StartScrollingSession;
begin
  if FScrollingSessionDepth = 0 then
  begin
    if FLoadedVerseID <> '' then
      FMarker := FLoadedVerseID
    else if Assigned(FTblPares) and FTblPares.Active then
      FMarker := ID
    else
      FMarker := '';
    PreRolagemVersiculo(nil);
    ScrollEventsEnabled := false;
  end;
  Inc(FScrollingSessionDepth);
end;

procedure TProjeto.FinishScrollingSession;
var
  steps: Integer;
  idField: TField;
begin
  if FScrollingSessionDepth > 0 then
    Dec(FScrollingSessionDepth);
  if FScrollingSessionDepth > 0 then
    Exit;

  if (FMarker <> '') and Assigned(FTblPares) and FTblPares.Active then
  begin
    idField := FTblPares.FieldByName('pare_id');
    steps := 200;
    while (not FTblPares.BOF) and (idField.AsString <> FMarker) and (steps > 0) do
    begin
      FTblPares.Prior;
      Dec(steps);
    end;
    if idField.AsString <> FMarker then
      GoToReference(FMarker)
    else
      UpdateReference(FMarker);
  end;
  ScrollEventsEnabled := true;
end;

procedure TProjeto.RewindChapter;
var
  targetRef: string;
  steps: Integer;
  idField: TField;
begin
  if Verse <> 1 then
  begin
    targetRef := Format('%d,%d,1', [BookID, Chapter]);
    if Assigned(FTblPares) and FTblPares.Active then
    begin
      idField := FTblPares.FieldByName('pare_id');
      steps := Verse + 5;
      while (not FTblPares.BOF) and (idField.AsString <> targetRef) and (steps > 0) do
      begin
        FTblPares.Prior;
        Dec(steps);
      end;
      if idField.AsString <> targetRef then
        GoToReference(targetRef)
      else
        UpdateReference(targetRef);
    end
    else
      GoToReference(targetRef);
  end;
end;


function TProjeto.Pesquisar(const Termo: string; EscopoBusca: TSearchScope = ssDestination): TVerseSearchResults;
var
  qry: TSQLQuery;
  sqlWhere, termPattern, preview, bName: string;
  count, b, c, v: Integer;
begin
  SetLength(Result, 0);
  if (Trim(Termo) = '') or (FFileName = '') or not FileExists(FFileName) then
    exit;

  if Assigned(FTblPares) and (FTblPares.State in [dsEdit, dsInsert]) then
    FTblPares.Post;

  qry := CriarObjetoQuery(FFileName);
  try
    termPattern := '%' + Trim(Termo) + '%';

    case EscopoBusca of
      ssDestination:
        sqlWhere := 'pare_texto_destino LIKE :term';
      ssSource:
        sqlWhere := 'pare_texto_origem LIKE :term';
      ssRef1:
        sqlWhere := 'pare_texto_consulta1 LIKE :term';
      ssRef2:
        sqlWhere := 'pare_texto_consulta2 LIKE :term';
      ssStrongs:
        sqlWhere := '(pare_pares LIKE :term) OR (pare_texto_origem LIKE :term)';
      ssComments:
        sqlWhere := 'pare_comentarios LIKE :term';
    else
      sqlWhere := '(pare_texto_destino LIKE :term) OR (pare_texto_origem LIKE :term) OR ' +
                  '(pare_texto_consulta1 LIKE :term) OR (pare_texto_consulta2 LIKE :term) OR ' +
                  '(pare_pares LIKE :term) OR (pare_comentarios LIKE :term)';
    end;

    qry.SQL.Text := 'SELECT pare_id, pare_situacao, pare_texto_destino, pare_texto_origem, ' +
                    'pare_texto_consulta1, pare_texto_consulta2, pare_pares, pare_comentarios ' +
                    'FROM pares WHERE ' + sqlWhere + ' ORDER BY rowid';

    qry.Params.ParamByName('term').AsString := termPattern;
    try
      qry.Open;
    except
      on E: Exception do
      begin
        ShowMessage('Search error: ' + E.Message);
        exit;
      end;
    end;

    count := 0;
    while not qry.EOF do
    begin
      b := 0; c := 0; v := 0;
      SScanf(qry.FieldByName('pare_id').AsString, '%d,%d,%d', [@b, @c, @v]);

      bName := '';
      if (b > 0) and (b <= 66) then
      begin
        if (FEscopo <> etNone) and (b - OffsetLivros[FEscopo] - 1 >= 0) and (b - OffsetLivros[FEscopo] - 1 < QLivros[FEscopo]) then
          bName := NLivros[FEscopo][b - OffsetLivros[FEscopo] - 1]
        else
          bName := NLivrosONT[b];
      end;

      if bName = '' then
        bName := 'Book ' + IntToStr(b);

      case EscopoBusca of
        ssSource:      preview := qry.FieldByName('pare_texto_origem').AsString;
        ssRef1:        preview := qry.FieldByName('pare_texto_consulta1').AsString;
        ssRef2:        preview := qry.FieldByName('pare_texto_consulta2').AsString;
        ssComments:    preview := qry.FieldByName('pare_comentarios').AsString;
        ssStrongs:     preview := qry.FieldByName('pare_pares').AsString;
      else
        preview := qry.FieldByName('pare_texto_destino').AsString;
      end;

      if Trim(preview) = '' then
        preview := qry.FieldByName('pare_texto_origem').AsString;

      Inc(count);
      SetLength(Result, count);
      Result[count - 1].RefID := qry.FieldByName('pare_id').AsString;
      Result[count - 1].FormattedRef := Format('%s %d:%d', [bName, c, v]);
      Result[count - 1].PreviewText := Trim(preview);
      Result[count - 1].Status := qry.FieldByName('pare_situacao').AsInteger;

      qry.Next;
    end;

    qry.Close;
  finally
    if Assigned(qry.Transaction) then
      qry.Transaction.Free;
    if Assigned(qry.DataBase) then
      qry.DataBase.Free;
    qry.Free;
  end;
end;
end.
