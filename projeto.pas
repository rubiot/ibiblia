unit Projeto;

{$mode objfpc}{$H+}{$M+}

{TODO -cDesejável: Remover dependência com formpopup, criá-lo dinamicamente }
{TODO -cDesejável: Usar descendente de THintWindow para exibição de definições }

interface

uses
  Classes,
  {$IFDEF UNIX}
  LCLType, LCLIntf,
  {$ELSE}
  Windows,
  {$ENDIF}
  SysUtils, Sqlite3DS, sqlite3conn, sqldb, db, StrUtils, math,
  ExtCtrls, Controls, ComCtrls, StdCtrls, Graphics, Forms, Versiculo, Sugestao,
  MemoVersiculo, ONTTokenizer, Dialogs, dos, PCRE, ExportarProjeto, LazLogger,
  Sintagma, MySwordModule;

type

  TProjeto = class;

  { TTipoTextoBiblico }

  TTipoTextoBiblico =
  (
    tbOrigem,
    tbDestino,
    tbConsulta1,
    tbConsulta2
  );

  TEscopoTexto = (etOT, etNT, etNone);

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
  TOnSintagmaClickEvent = procedure (Sender: TSintagma) of object;
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
    FFileName: string;
    FMemoVersiculo: TMemoVersiculo;
    FAtivo: boolean;
    FMostrandoTags: boolean;
    FExportando: boolean;
    FOnAlterarVersiculo: TOnAlterarVersiculoEvent;
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
    FMostrarQtdStrongs: boolean;
    FOnNovoVersiculo: TOnNovoVersiculoEvent;
    FOnSintagmaClick: TOnSintagmaClickEvent;
    FClosing: boolean;
    function GetCaminho: string;
    function GetComentarios: string;
    function GetID: string;
    function GetModificado: boolean;
    function GetReferencia: string;
    function GetSituacao: Integer;
    procedure PreencherArvore;
    procedure SetAtrasoExibicao(const AValue: Cardinal);
    procedure SetComentarios(const AValue: string);
    procedure SetMostrarQtdStrongs(AValue: boolean);
    procedure SetOnAlterarVersiculo(const AValue: TOnAlterarVersiculoEvent);
    procedure SetOnNovoVersiculo(const AValue: TOnNovoVersiculoEvent);
    procedure SetOnSintagmaClick(const AValue: TOnSintagmaClickEvent);
    procedure SetPalavrasComStrongEmNegrito(AValue: boolean);
    procedure SetSituacao(const AValue: Integer);
    procedure ExportTheWordBible(verses: TStringList; filename: string; props: string);
    procedure ExportMySwordBible(verses: TStringList; filename: string; props: string);
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
    procedure HabilitarEventosRolagem;
    procedure DesabilitarEventosRolagem;
    procedure SintagmaOnMouseEnter(Sender: TSintagma);
    procedure SintagmaOnMouseLeave(Sender: TSintagma);
    procedure SintagmaOnClick(Sender: TSintagma);
    procedure OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
    procedure OnAlterarTextoVersiculo(Sender: TMemoVersiculo);
    procedure OnExibirDefinicao(Sender: TObject);
    procedure OnRedimensionarVersiculo(Sender: TObject);
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
    procedure ImportarModuloTheWord(arquivo: string; texto: TTipoTextoBiblico; replace: boolean);
    procedure ImportarModuloTheWord(arquivo: string; texto: TTipoTextoBiblico; pb: TProgressBar; replace: boolean);
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
    function GetTranslationSuggestions(syntagm: TSintagma): string;
    procedure Translate;
    procedure ToggleMostrarTags;
    property FileName: string read FFileName;
    property Referencia: string read GetReferencia;
    property ID: string read GetID;
    property Modificado: boolean read GetModificado;
    property Arvore: TTreeView read FArvore write FArvore;
    property OnAlterarVersiculo: TOnAlterarVersiculoEvent read FOnAlterarVersiculo write SetOnAlterarVersiculo;
    property OnNovoVersiculo: TOnNovoVersiculoEvent read FOnNovoVersiculo write SetOnNovoVersiculo;
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
    property MostrarQtdStrongs: boolean read FMostrarQtdStrongs write SetMostrarQtdStrongs;
    property PopupTrigger: TPopupTrigger read FPopupTrigger write FPopupTrigger;
  end;

resourcestring
  SGenesis = 'Genesis';
  SExodus = 'Exodus';
  SLeviticus = 'Leviticus';
  SNumbers = 'Numbers';
  SDeuteronomy = 'Deuteronomy';
  SJoshua = 'Joshua';
  SJudges = 'Judges';
  SRuth = 'Ruth';
  S1Samuel = '1 Samuel';
  S2Samuel = '2 Samuel';
  S1Kings = '1 Kings';
  S2Kings = '2 Kings';
  S1Chronicles = '1 Chronicles';
  S2Chronicles = '2 Chronicles';
  SEzra = 'Ezra';
  SNehemiah = 'Nehemiah';
  SEsther = 'Esther';
  SJob = 'Job';
  SPsalms = 'Psalms';
  SProverbs = 'Proverbs';
  SEcclesiastes = 'Ecclesiastes';
  SSongOfSolomon = 'Song of Solomon';
  SIsaiah = 'Isaiah';
  SJeremiah = 'Jeremiah';
  SLamentations = 'Lamentations';
  SEzekiel = 'Ezekiel';
  SDaniel = 'Daniel';
  SHosea = 'Hosea';
  SJoel = 'Joel';
  SAmos = 'Amos';
  SObadiah = 'Obadiah';
  SJohah = 'Jonah';
  SMicah = 'Micah';
  SNahum = 'Nahum';
  SHabakkuk = 'Habakkuk';
  SZephaniah = 'Zephaniah';
  SHaggai = 'Haggai';
  SZechariah = 'Zecariah';
  SMalachi = 'Malachi';
  SMatthew = 'Matthew';
  SMark = 'Mark';
  SLuke = 'Luke';
  SJohn = 'John';
  SActs = 'Acts';
  SRomans = 'Romans';
  S1Corinthians = '1 Corinthians';
  S2Corinthians = '2 Corinthians';
  SGalatians = 'Galatians';
  SEphesians = 'Ephesians';
  SPhilippians = 'Philippians';
  SColossians = 'Colossians';
  S1Thessalonians = '1 Thessalonians';
  S2Thessalonians = '2 Thessalonians';
  S1Timothy = '1 Timothy';
  S2Timothy = '2 Timothy';
  STitus = 'Titus';
  SPhilemon = 'Philemon';
  SHebrew = 'Hebrews';
  SJames = 'James';
  S1Peter = '1 Peter';
  S2Peter = '2 Peter';
  S1John = '1 John';
  S2John = '2 John';
  S3John = '3 John';
  SJude = 'Jude';
  SRevelation = 'Revelation';

  SAnalyticalConcordance = 'Anaytical Concordance';
  SSyntheticConcordance = 'Synthetic Concordance';
  SOTConcordanceNotImplementedYet = 'Old Testament concordance is not implemented yet';
  SLanguageId = 'en';
  SError = 'Error';
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
  SFailedToCreateFile = 'Failed to create file: %s';
  SExportToFileError = 'iBiblia found an error while exporting the Bible module file.'#13#10 +
                       'Please check if theWord is using the file, then close it and try again.';
  STranslationMemory = 'Translation memory';

const
  NLivrosVT: array[1..39] of string = (
    SGenesis, SExodus, SLeviticus, SNumbers, SDeuteronomy, SJoshua, SJudges, SRuth,
    S1Samuel, S2Samuel, S1Kings, S2Kings, S1Chronicles, S2Chronicles, SEzra, SNehemiah,
    SEsther, SJob, SPsalms, SProverbs, SEcclesiastes, SSongOfSolomon, SIsaiah,
    SJeremiah, SLamentations, SEzekiel, SDaniel, SHosea, SJoel, SAmos, SObadiah,
    SJohah, SMicah, SNahum, SHabakkuk, SZephaniah, SHaggai, SZechariah, SMalachi
  );
  NLivrosNT: array[1..27] of string = (
    SMatthew, SMark, SLuke, SJohn, SActs, SRomans, S1Corinthians, S2Corinthians,
    SGalatians, SEphesians, SPhilippians, SColossians, S1Thessalonians, S2Thessalonians,
    S1Timothy, S2Timothy, STitus, SPhilemon, SHebrew, SJames, S1Peter, S2Peter,
    S1John, S2John, S3John, SJude, SRevelation
  );
  NLivros: array[etOT..etNT] of ^string = (@NLivrosVT, @NLivrosNT);

  QCapitulosVT: array[1..39] of smallint = (
    50,40,27,36,34,24,21,4,31,24,22,25,29,36,10,13,10,42,150,31,12,8,66,
    52,5,48,12,14,3,9,1,4,7,3,3,3,2,14,4
  );
  QCapitulosNT: array[1..27] of smallint = (
    28,16,24,21,28,16,16,13,6,6,4,4,5,3,6,4,3,1,13,5,5,3,5,1,1,1,22
  );
  QCapitulos: array[etOT..etNT] of ^smallint = (@QCapitulosVT, @QCapitulosNT);

  QCapLivrosVT: array[1..39] of smallint = (
    1,51,91,118,154,188,212,233,237,268,292,314,339,368,404,414,427,437,479,629,
    660,672,680,746,798,803,851,863,877,880,889,890,894,901,904,907,910,912,926
  );
  QCapLivrosNT: array[1..27] of smallint = (
    1,29,45,69,90,118,134,150,163,169,175,179,183,188,191,197,201,204,205,218,
    223,228,231,236,237,238,239
  );
  QCapLivros: array[etOT..etNT] of ^smallint = (@QCapLivrosVT, @QCapLivrosNT);

  QVersiculosVT: array[1..929] of smallint = (
    31,25,24,26,32,22,24,22,29,32,32,20,18,24,21,16,27,33,38,18,34,24,20,67,34,35,
    46,22,35,43,55,32,20,31,29,43,36,30,23,23,57,38,34,34,28,34,31,22,33,26,22,25,
    22,31,23,30,25,32,35,29,10,51,22,31,27,36,16,27,25,26,36,31,33,18,40,37,21,43,
    46,38,18,35,23,35,35,38,29,31,43,38,17,16,17,35,19,30,38,36,24,20,47,8,59,57,
    33,34,16,30,37,27,24,33,44,23,55,46,34,54,34,51,49,31,27,89,26,23,36,35,16,33,
    45,41,50,13,32,22,29,35,41,30,25,18,65,23,31,40,16,54,42,56,29,34,13,46,37,29,
    49,33,25,26,20,29,22,32,32,18,29,23,22,20,22,21,20,23,30,25,22,19,19,26,68,29,
    20,30,52,29,12,18,24,17,24,15,27,26,35,27,43,23,24,33,15,63,10,18,28,51,9,45,
    34,16,33,36,23,31,24,31,40,25,35,57,18,40,15,25,20,20,31,13,31,30,48,25,22,23,
    18,22,28,36,21,22,12,21,17,22,27,27,15,25,23,52,35,23,58,30,24,42,15,23,29,22,
    44,25,12,25,11,31,13,27,32,39,12,25,23,29,18,13,19,27,31,39,33,37,23,29,33,43,
    26,22,51,39,25,53,46,28,34,18,38,51,66,28,29,43,33,34,31,34,34,24,46,21,43,29,
    53,18,25,27,44,27,33,20,29,37,36,21,21,25,29,38,20,41,37,37,21,26,20,37,20,30,
    54,55,24,43,26,81,40,40,44,14,47,40,14,17,29,43,27,17,19,8,30,19,32,31,31,32,
    34,21,30,17,18,17,22,14,42,22,18,31,19,23,16,22,15,19,14,19,34,11,37,20,12,21,
    27,28,23,9,27,36,27,21,33,25,33,27,23,11,70,13,24,17,22,28,36,15,44,11,20,32,
    23,19,19,73,18,38,39,36,47,31,22,23,15,17,14,14,10,17,32,3,22,13,26,21,27,30,
    21,22,35,22,20,25,28,22,35,22,16,21,29,29,34,30,17,25,6,14,23,28,25,31,40,22,
    33,37,16,33,24,41,30,24,34,17,6,12,8,8,12,10,17,9,20,18,7,8,6,7,5,11,15,50,14,
    9,13,31,6,10,22,12,14,9,11,12,24,11,22,22,28,12,40,22,13,17,13,11,5,26,17,11,
    9,14,20,23,19,9,6,7,23,13,11,11,17,12,8,12,11,10,13,20,7,35,36,5,24,20,28,23,
    10,12,20,72,13,19,16,8,18,12,13,17,7,18,52,17,16,15,5,23,11,13,12,9,9,5,8,28,
    22,35,45,48,43,13,31,7,10,10,9,8,18,19,2,29,176,7,8,9,4,8,5,6,5,6,8,8,3,18,3,
    3,21,26,9,8,24,13,10,7,12,15,21,10,20,14,9,6,33,22,35,27,23,35,27,36,18,32,31,
    28,25,35,33,33,28,24,29,30,31,29,35,34,28,28,27,28,27,33,31,18,26,22,16,20,12,
    29,17,18,20,10,14,17,17,11,16,16,13,13,14,31,22,26,6,30,13,25,22,21,34,16,6,
    22,32,9,14,14,7,25,6,17,25,18,23,12,21,13,29,24,33,9,20,24,17,10,22,38,22,8,
    31,29,25,28,28,25,13,15,22,26,11,23,15,12,17,13,12,21,14,21,22,11,12,19,12,25,
    24,19,37,25,31,31,30,34,22,26,25,23,17,27,22,21,21,27,23,15,18,14,30,40,10,38,
    24,22,17,32,24,40,44,26,22,19,32,21,28,18,16,18,22,13,30,5,28,7,47,39,46,64,
    34,22,22,66,22,22,28,10,27,17,17,14,27,18,11,22,25,28,23,23,8,63,24,32,14,49,
    32,31,49,27,17,21,36,26,21,26,18,32,33,31,15,38,28,23,29,49,26,20,27,31,25,24,
    23,35,21,49,30,37,31,28,28,27,27,21,45,13,11,23,5,19,15,11,16,14,17,15,12,14,
    16,9,20,32,21,15,16,15,13,27,14,17,14,15,21,17,10,10,11,16,13,12,13,15,16,20,
    15,13,19,17,20,19,18,15,20,15,23,21,13,10,14,11,15,14,23,17,12,17,14,9,21,14,
    17,18,6
  );
  QVersiculosNT: array[1..260] of smallint = (
    25,23,17,25,48,34,29,34,38,42,30,50,58,36,39,28,27,35,30,34,46,46,39,51,46,
    75,66,20,45,28,35,41,43,56,37,38,50,52,33,44,37,72,47,20,80,52,38,44,39,49,
    50,56,62,42,54,59,35,35,32,31,37,43,48,47,38,71,56,53,51,25,36,54,47,71,53,
    59,41,42,57,50,38,31,27,33,26,40,42,31,25,26,47,26,37,42,15,60,40,43,48,30,
    25,52,28,41,40,34,28,41,38,40,30,35,27,27,32,44,31,32,29,31,25,21,23,25,39,
    33,21,36,21,14,23,33,27,31,16,23,21,13,20,40,13,27,33,34,31,13,40,58,24,24,
    17,18,18,21,18,16,24,15,18,33,21,14,24,21,29,31,26,18,23,22,21,32,33,24,30,
    30,21,23,29,23,25,18,10,20,13,18,28,12,17,18,20,15,16,16,25,21,18,26,17,22,
    16,15,15,25,14,18,19,16,14,20,28,13,28,39,40,29,25,27,26,18,17,20,25,25,22,
    19,14,21,22,18,10,29,24,21,21,13,14,25,20,29,22,11,14,17,17,13,21,11,19,17,
    18,20,8,21,18,24,21,15,27,21
  );
  QVersiculos: array[etOT..etNT] of ^smallint = (@QVersiculosVT, @QVersiculosNT);

  QLivros: array[etOT..etNT] of smallint = (39, 27);
  OffsetLivros: array[etOT..etNT] of smallint = (0, 39);
  QLinhas: array[etOT..etNT] of smallint = (23145, 7957);
  QStrongs: array[etOT..etNT] of smallint = (8674, 5624);
  ProjetoModelo: array[etOT..etNT] of string = ( '.\projeto-ot.modelo', '.\projeto.modelo');
  ConcordanciaModelo: array[etOT..etNT] of string = ( '.\concordancia-ot.modelo', '.\concordancia.modelo');

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
  Commit;
end;

function TProjeto.GetReferencia: string;
var
  b, c, v: integer;
begin
  sscanf(GetID(), '%d,%d,%d', [@b, @c, @v]);
  result := format('%s %d:%d', [NLivros[FEscopo][b-OffsetLivros[FEscopo]-1], c, v]);
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
    FArvore.Visible := false;
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
    FArvore.Visible := true;
  end;

  FArvore.OnChange := @OnMudancaVersiculo;
end;

procedure TProjeto.SetAtrasoExibicao(const AValue: Cardinal);
begin
  if FAtrasoExibicao = AValue then exit;
  FAtrasoExibicao := AValue;
  FTemporizador.Interval := AValue;
end;

procedure TProjeto.SetComentarios(const AValue: string);
begin
  FTblPares.Edit;
  FTblPares.FieldByName('pare_comentarios').AsString := AValue;
  FTblPares.Post;
end;

procedure TProjeto.SetMostrarQtdStrongs(AValue: boolean);
var
  v: TTipoTextoBiblico;
begin
  if FMostrarQtdStrongs=AValue then Exit;
  FMostrarQtdStrongs:=AValue;
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
      FAVersiculo[v].MostrarQtdStrongs := AValue;
end;

procedure TProjeto.SetOnAlterarVersiculo(const AValue: TOnAlterarVersiculoEvent);
begin
  if FOnAlterarVersiculo = AValue then exit;
  FOnAlterarVersiculo := AValue;

  if assigned(FAVersiculo[tbOrigem]) then
    FAVersiculo[tbOrigem].OnAlterarVersiculo := OnAlterarVersiculo;
end;

procedure TProjeto.SetOnNovoVersiculo(const AValue: TOnNovoVersiculoEvent);
begin
  if FOnNovoVersiculo = AValue then exit;
  FOnNovoVersiculo := AValue;
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
  if FClosing then
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
  module := TMySwordModule.Create(filename, FEscopo = etOT, FEscopo = etNT, true, props);
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

function TProjeto.GetModificado: boolean;
begin
  result := FTblPares.UpdatesPending or FTblInfo.UpdatesPending or (assigned(FAVersiculo[tbOrigem]) and FAVersiculo[tbOrigem].Modificado);
end;

function TProjeto.GetCaminho: string;
begin
  result := '';
  if FTblInfo.Active then
    result := FTblInfo.FileName;
end;

function TProjeto.GetComentarios: string;
begin
  result := FTblPares.FieldByName('pare_comentarios').AsString;
end;

function TProjeto.GetID: string;
begin
  result := FTblPares.FieldByName('pare_id').AsString;
end;

procedure TProjeto.CopiarArquivo(origem, destino: string);
begin
  with TMemoryStream.Create do
  try
    LoadFromFile(origem);
    SaveToFile(destino);
  finally
    Free;
  end;
end;

function TProjeto.CriarObjetoTabela(db, tabela, chave: string
  ): TSqlite3Dataset;
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
    FATmpVerse[tbOrigem ].Pares := FTblPares.FieldByName('pare_pares').AsString;
    FATmpVerse[texto].AlterarTexto(versiculo);
    FTblPares.FieldByName('pare_pares').AsString := FATmpVerse[tbOrigem].Pares;
    FTblPares.FieldByName('pare_situacao').AsInteger := 2; // needs review
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
begin
  if FAVersiculo[tbOrigem] = nil then
    exit;

  FMemoVersiculo.Desativar; // hide and save verse edit if necessary

  if FAVersiculo[tbOrigem].Modificado then
  begin
    SalvarAssociacoes;
    SalvarPares;
  end;
  SalvarTexto; // salvar alterações nos textos dos versículos

  FParesAntigos.Free; // liberando pares antigos do versículo
  FParesAntigos := nil;

  if assigned(FMemoComentarios) then
    Comentarios := FMemoComentarios.Text;
end;

procedure TProjeto.PosRolagemVersiculo(DataSet: TDataSet);
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
      FAVersiculo[v].Texto := FTblPares.Fields[FACamposTexto[v]].AsString;

  if assigned(FAVersiculo[tbOrigem]) and assigned(FAVersiculo[tbOrigem]) then
  begin
    try
      FAVersiculo[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;
    except
      on E: Exception do
      MessageDlg(SError, SCorruptedData + #13#10#13#10 + Referencia + #13#10#13#10 +
           format('%s'#13#10#13#10'pares: %s'#13#10'%s'#13#10'%s',
                  [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                  FAVersiculo[tbOrigem].DebugTokens, FAVersiculo[tbDestino].DebugTokens]),
           mtError, [mbOK], 0);

    end;
    FParesAntigos := FAVersiculo[tbOrigem].GetListaPares(tlMetaDados);
  end;

  SelectTreeNode(ID);

  if assigned(FMemoComentarios) then
    FMemoComentarios.Text := Comentarios;

  if assigned(FRadioGroupSituacao) then
    FRadioGroupSituacao.ItemIndex := Situacao;

  if assigned(OnNovoVersiculo) then
    OnNovoVersiculo(self);

  if FTblPares.Fields[FACamposTexto[tbDestino]].AsString.IsEmpty then // open verse to edition if is empty
    OnDblClickVersiculo(FAVersiculo[tbDestino].Painel);
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

  pares := FAVersiculo[tbOrigem].GetListaPares(tlMetaDados);

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

procedure TProjeto.HabilitarEventosRolagem;
begin
  FTblPares.BeforeScroll := @PreRolagemVersiculo;
  FTblPares.AfterScroll  := @PosRolagemVersiculo;
  FArvore.OnChange       := @OnMudancaVersiculo;
end;

procedure TProjeto.DesabilitarEventosRolagem;
begin
  FTblPares.BeforeScroll := nil;
  FTblPares.AfterScroll  := nil;
  FArvore.OnChange       := nil;
end;

procedure TProjeto.SintagmaOnMouseEnter(Sender: TSintagma);
var
  show: boolean;
begin
  case FPopupTrigger of
    ptMouseHover:     show := true;
    ptAltMouseHover:  show := GetKeyState(VK_MENU   ) and $8000 <> 0;
    ptCtrlMouseHover: show := GetKeyState(VK_CONTROL) and $8000 <> 0;
  end;

  if show then
  begin
    FTemporizador.Enabled := false;
    FTemporizador.Tag := PtrInt(Sender);
    FTemporizador.Enabled := true;
  end;
end;

procedure TProjeto.SintagmaOnMouseLeave(Sender: TSintagma);
var
  hide: boolean;
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
end;

procedure TProjeto.SintagmaOnClick(Sender: TSintagma);
begin
  FMemoVersiculo.Desativar; // saving eventual changes on verse edit

  if assigned(FOnSintagmaClick) then
    FOnSintagmaClick(Sender);

  if assigned(FAVersiculo[tbOrigem]) and
     ((Sender.VersiculoRef = FAVersiculo[tbOrigem]) or (Sender.VersiculoRef = FAVersiculo[tbDestino])) then
  begin
    if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AndamentoAssociacao > 0) then
      FRadioGroupSituacao.ItemIndex := 1 // associando
    else if (FRadioGroupSituacao.ItemIndex > 1) and (FAVersiculo[tbOrigem].AndamentoAssociacao = 0) then
      FRadioGroupSituacao.ItemIndex := 0 // sem associação
    else if (FRadioGroupSituacao.ItemIndex = 1) and ((FAVersiculo[tbOrigem].AndamentoAssociacao = 1) or (FAVersiculo[tbDestino].AndamentoAssociacao = 1)) then
      FRadioGroupSituacao.ItemIndex := 3; // associado
  end;
end;

procedure TProjeto.OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
var
  l, c, v: TTreeNode;
  i: smallint;
begin
  if not assigned(FTblPares) or FClosing or not assigned(Node) then
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
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if FAVersiculo[v] = Sender.Versiculo then
    begin
      Sender.Versiculo.AlterarTexto(Sender.Texto);
      break;
    end;
  //MessageDlg('Versículo modificado', format('Versiculo modificado: %s', [Sender.Texto]), mtError, [mbOK], 0);
end;

procedure TProjeto.OnExibirDefinicao(Sender: TObject);
var
  v: TTipoTextoBiblico;
  point: TPoint;
  s, p: TSintagma;
  m, rtf: string;
begin
  if frmDictionaryPopup.Visible then
    exit;

  s := TSintagma(TTimer(Sender).Tag);
  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    if (FAVersiculo[v] = s.VersiculoRef) then
    begin
      if assigned(FADicMorfo[v]) then
      begin
        if s.Morf.Count > 0 then
          for m in s.Morf do
            FrmDictionaryPopup.AdicionarMorfo(m, ObterDefinicaoMorfo(m, v))
        else
        begin // não tem morfo, vejamos se os pares têm
          for p in s.Pares do
            for m in p.Morf do
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
          for p in s.Pares do
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
        frmDictionaryPopup.Caption := s.Texto;
        frmDictionaryPopup.MostrarEm(point.x, point.y);
      end;
      break;
    end;
  end;
end;

procedure TProjeto.OnRedimensionarVersiculo(Sender: TObject);
begin
  TVersiculo(TScrollBox(Sender).Tag).Renderizar;
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

  FAVersiculo[t].OnMouseEnter := @SintagmaOnMouseEnter;
  FAVersiculo[t].OnMouseLeave := @SintagmaOnMouseLeave;
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
  marcador: string;
  b: boolean;
begin
  if not assigned(FArvore) then exit;
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

  //DesabilitarEventosRolagem;
  marcador := GetID;
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

  IrPara(marcador);
  //HabilitarEventosRolagem;
end;

procedure TProjeto.AtualizarArvore(id: string);
var
  l, c, v, i: smallint;
  al, ac: array[0..3] of smallint;
  nl, nc, nv, n: TTreeNode;
  b: boolean;
begin
  if not assigned(FArvore) or FClosing then
    exit;

  SScanf(id, '%d,%d,%d', [@l, @c, @v]);

  al[0] := 0; al[1] := 0; al[2] := 0; al[3] := 0;
  ac[0] := 0; ac[1] := 0; ac[2] := 0; ac[3] := 0;

  nl := FArvore.Items[0];
  for i:=l-OffsetLivros[FEscopo]-1 downto 1 do
    nl := nl.GetNextSibling;

  nc := nl.GetFirstChild;
  for i:=c-1 downto 1 do
  begin
    inc(al[nc.ImageIndex]);
    nc := nc.GetNextSibling;
  end;

  nv := nc.GetFirstChild;
  for i:=v-1 downto 1 do
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
      if ac[i] = QVersiculos[FEscopo][ QCapLivros[FEscopo][l-OffsetLivros[FEscopo]]+c-1 ] then
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
      if al[i] = QCapitulos[FEscopo][l-OffsetLivros[FEscopo]] then
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
  FOnNovoVersiculo    := nil;
  FOnAlterarVersiculo := nil;
  FSugeridor          := nil;
  FTblPares           := nil;
  FTblInfo            := nil;
  FEscopo             := etNone;
  FClosing            := false;

  FTemporizador          := TTimer.Create(nil);
  FTemporizador.Enabled  := false;
  FTemporizador.Interval := 200;
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
    if (v <= integer(high(TTipoTextoBiblico))) and assigned(paineis[v]) then
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

  if assigned(FParesAntigos) then
    FParesAntigos.Free;

  FSugeridor.Free;
  FTemporizador.Free;
  FMemoVersiculo.Destruir;
end;

procedure TProjeto.Abrir(Nome: string);
  function IdentificarEscopo(marcador: string): TEscopoTexto;
  var
    livro: integer;
  begin
    livro := StrToInt(copy(marcador, 1, pos(',', marcador) - 1));
    if livro < 40 then
      result := etOT
    else
      result := etNT;
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
  DesabilitarEventosRolagem;
  FSugeridor := TGerSugestoes.Criar(Nome);

  FTblPares.Open;
  FTblInfo.Open;

  f := TFont.Create;
  for t:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
  begin
    if not assigned(FAVersiculo[t]) then
       continue;

    { definindo fontes }
    s := ObterInfo(format('fonte%d.tamanho', [t]));
    if s = '' then
    begin
      f.Name := 'default';
      f.Size := 12;
    end else begin
      f.Name := ObterInfo(format('fonte%d.nome', [t]));
      f.Size := StrToInt(s);
    end;
    FAVersiculo[t].Fonte := f;
    FAVersiculo[t].PalavrasComStrongEmNegrito := FPalavrasComStrongEmNegrito;
    FAVersiculo[t].MostrarQtdStrongs := FMostrarQtdStrongs;

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
    FEscopo := IdentificarEscopo(ObterInfo('marcador'));

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

  IrPara(ObterInfo('marcador'));
  PosRolagemVersiculo(nil);
  HabilitarEventosRolagem;

  FAtivo := true;
  FMostrandoTags := false;
end;

procedure TProjeto.Fechar(_Commit: boolean);
begin
  FClosing := true;

  if assigned(FArvore) then
  begin
    //FArvore.FullCollapse;
    FArvore.Enabled := false;
    FArvore.Items.Clear;
  end;

  PreRolagemVersiculo(nil);

  if _Commit then
    Commit
  else
    Rollback;

  AtribuirInfo('marcador', FTblPares.FieldByName('pare_id').AsString);
  Commit;

  FTblPares.Close;
  FTblPares.Free;
  FTblPares := nil;

  FTblInfo.Close;
  FTblInfo.Free;
  FTblInfo := nil;

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
    FTblPares.Locate('pare_id', Referencia, []);
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
end;

procedure TProjeto.SugerirAssociacao;
begin
  FSugeridor.SugerirAssociacoes(FAVersiculo[tbOrigem]);

  if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AndamentoAssociacao > 0) then
    FRadioGroupSituacao.ItemIndex := 1 // associando
end;

procedure TProjeto.RecriarBaseSugestoes;
begin
  RecriarBaseSugestoes(nil);
end;

procedure TProjeto.RecriarBaseSugestoes(pb: TProgressBar);
var
  marcador: string;
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

      FAVersiculo[tbOrigem].Ativo := false;
      FAVersiculo[tbDestino].Ativo := false;
      FExportando := true;
      PreRolagemVersiculo(nil);

      DesabilitarEventosRolagem;
      marcador := GetID;
      VersiculoInicial;
      while not FTblPares.EOF do
      begin
        if length(FTblPares.FieldByName('pare_pares').AsString) > 0 then
        begin
          FAVersiculo[tbOrigem].Texto := FTblPares.Fields[FACamposTexto[tbOrigem]].AsString;
          FAVersiculo[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
          FAVersiculo[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;

          pares := FAVersiculo[tbOrigem].GetListaPares(tlMetaDados);

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
    FAVersiculo[tbOrigem].Ativo := true;
    FAVersiculo[tbDestino].Ativo := true;
    IrPara(marcador);
    HabilitarEventosRolagem;
    PosRolagemVersiculo(nil);
    FExportando := false;
    if assigned(pb) then
       pb.Visible := false;
  end;
end;

procedure TProjeto.ImportarModuloTheWord(arquivo: string;
  texto: TTipoTextoBiblico; replace: boolean);
begin
  ImportarModuloTheWord(arquivo, texto, nil, replace);
end;

procedure TProjeto.ImportarModuloTheWord(arquivo: string;
  texto: TTipoTextoBiblico; pb: TProgressBar; replace: boolean);
var
  modulo: TStringList;
  i, offset: smallint;
  reVazio, reComments, {reShortTitle,} reVerseRules, verseRule: IRegex;
  mtVerseRules: IMatch;
  verseRulesDe, verseRulesPara: TStringList;
  propriedades: TStringStream;
  m: smallint;
  marker: string;
  //line: string;
begin
  offset := 0;
  if AnsiEndsText('.ont', arquivo) then
  begin
    if FEscopo = etNT then
      offset := QLinhas[etOT] // saltando o velho testamento
  end
  else if AnsiEndsText('.nt', arquivo) then
  begin
    if FEscopo = etOT then
      exit;
  end
  else if AnsiEndsText('.ot', arquivo) then
  begin
    if FEscopo = etNT then
      exit;
  end else
    exit; // não parece ser um módulo do theWord

  try
    modulo := TStringList.Create;
    modulo.LoadFromFile(arquivo);

    if modulo.Count < QLinhas[FEscopo] then
    begin
      MessageDlg(SError, format(SInvalidLineCount, [QLinhas[FEscopo]]), mtError, [mbOK], 0);
      exit;
    end;

    reVazio        := RegexCreate('^\s*$', [rcoUTF8]);
    reComments     := RegexCreate('#.*$', [rcoUTF8]);
    //reShortTitle   := RegexCreate('(short.title)\s*=\s*(.*)$', [rcoUTF8]);
    reVerseRules   := RegexCreate('^\s*verse.rule\s*=\s*"(.*?)(?<!")"(?!")\s+"(.*?)"(?=\s*$|\s+"(.*?)(?<!")"(?!"))', [rcoUTF8]);
    FrmEscolherVerseRules.Reset;
    verseRulesDe   := TStringList.Create;
    verseRulesPara := TStringList.Create;

    propriedades := TStringStream.Create('');
    for i:=offset+QLinhas[FEscopo] to modulo.Count-1 do
    begin
      //line := modulo[i];
      { eliminando comentários }
      modulo[i] := reComments.Replace(modulo[i], '');
      //modulo.Strings[i] := reShortTitle.Replace(modulo.Strings[i], '$1=$2i');

      if reVazio.IsMatch(modulo[i]) then
        continue;

      propriedades.WriteString(modulo[i] + #13#10);

      mtVerseRules := reVerseRules.Match(modulo[i]);
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

    if (FrmEscolherVerseRules.VerseRuleDe.Count > 0) and (FrmEscolherVerseRules.ShowModal = mrOK) then
    begin
      for m:=0 to FrmEscolherVerseRules.ckgVerseRules.Items.Count-1 do
        if FrmEscolherVerseRules.ckgVerseRules.Checked[m] then
        begin
          verseRulesDe.Add(FrmEscolherVerseRules.VerseRuleDe[m]);
          verseRulesPara.Add( FrmEscolherVerseRules.VerseRulePara[m] );
          DebugLn('regex criado (%s -------- %s)', [FrmEscolherVerseRules.VerseRuleDe[m], FrmEscolherVerseRules.VerseRulePara[m]]);
        end;
    end;
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
    DesabilitarEventosRolagem;
    marker := GetID;
    VersiculoInicial;
    for i:=offset to offset + QLinhas[FEscopo] - 1 do
    begin
      //line := modulo[i];
      { aplicando verse.rules }
      for m:=0 to verseRulesDe.Count-1 do
      begin
        verseRule := RegexCreate(verseRulesDe[m], [rcoUTF8]);
        modulo[i] := verseRule.Replace(modulo[i], verseRulesPara[m]);
      end;

      SetVerseText(modulo[i], texto, replace);

      VersiculoSeguinte;
      if assigned(pb) then
      begin
        pb.StepIt;
        if (pb.Position mod 75) = 0 then
           Application.ProcessMessages;
      end;
    end;
    HabilitarEventosRolagem;
    FExportando := false;
    IrPara(marker);
    AtualizarArvore(marker);
  finally
    modulo.Free;
    if assigned(pb) then
       pb.Visible := false;
    verseRulesDe.Free;
    verseRulesPara.Free;
  end;
  //FTblPares.ApplyUpdates;
end;

procedure TProjeto.NovoObjetoVersiculo(owner: TScrollBox;
  texto: TTipoTextoBiblico);
begin
  FAVersiculo[texto] := TVersiculo.Criar(owner);
  owner.Tag := ptrint(FAVersiculo[texto]);
  owner.OnResize := @OnRedimensionarVersiculo;
  owner.OnDblClick := @OnDblClickVersiculo;
  FAVersiculo[texto].OnClick := @SintagmaOnClick;

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
  lines: TStringList;
  marcador: string;
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
    PreRolagemVersiculo(nil);

    with FTblPares do
    begin
      DesabilitarEventosRolagem;
      marcador := GetID;
      VersiculoInicial;
      while not FTblPares.EOF do
      begin
        if length(FTblPares.FieldByName('pare_pares').AsString) = 0 then
          lines.Add(FTblPares.Fields[FACamposTexto[tbDestino]].AsString)
        else
        begin
          FATmpVerse[tbOrigem ].Texto := FTblPares.Fields[FACamposTexto[tbOrigem ]].AsString;
          FATmpVerse[tbDestino].Texto := FTblPares.Fields[FACamposTexto[tbDestino]].AsString;
          try
            FATmpVerse[tbOrigem].Pares := FTblPares.FieldByName('pare_pares').AsString;
          except
            on E: Exception do
              MessageDlg(SError, SCorruptedData + #13#10#13#10 + Referencia + #13#10 +
                   format('%s'#13#10'pares: %s'#13#10'%s'#13#10'%s',
                          [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                          FATmpVerse[tbOrigem].DebugTokens, FATmpVerse[tbDestino].DebugTokens]),
                   mtError, [mbOK], 0);
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
      IrPara(marcador);
      HabilitarEventosRolagem;
    end;

    if pos(lines[0], '<WG') = 0 then { adicionando tag inócua para que o theWord exiba definições mesmo sem associações no primeiro versículo }
      lines[0] := lines[0] + '<_MORPH_>';

    try
      if arquivo.EndsWith('.mybible') then // MySword module?
        ExportMySwordBible(lines, arquivo, ResgatarInfo('propriedades.destino'))
      else
        ExportTheWordBible(lines, arquivo, ResgatarInfo('propriedades.destino'));
    except
      on E: Exception do MessageDlg(SError, SExportToFileError, mtError, [mbOK], 0);
    end;
  finally
    lines.Destroy;
    FExportando := false;
    PosRolagemVersiculo(nil);
    if assigned(pb) then
      pb.Visible := false;
  end;
end;

procedure TProjeto.ExportarTextoInterlinear(arquivo: string; opcoes: TOpcoesExportacao);
begin
  ExportarTextoInterlinear(arquivo, nil, opcoes);
end;

{ TODO: Unify this code with ExportarTextoDestinoComStrongs() }
procedure TProjeto.ExportarTextoInterlinear(arquivo: string;
  pb: TProgressBar; opcoes: TOpcoesExportacao);
var
  lines: TStringList;
  marcador: string;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
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
    PreRolagemVersiculo(nil);

    with FTblPares do
    begin
      DesabilitarEventosRolagem;
      marcador := GetID;
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
              MessageDlg(SError, SCorruptedData + #13#10#13#10 + Referencia + #13#10 +
                   format('%s'#13#10'pares: %s'#13#10'%s'#13#10'%s',
                          [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                          FATmpVerse[tbOrigem].DebugTokens, FATmpVerse[tbDestino].DebugTokens]),
                   mtError, [mbOK], 0);
          end;
          lines.Add(IfThen(arquivo.EndsWith('.mybible'),
                           FATmpVerse[tbOrigem].GetMySwordInterlinearLine,
                           FATmpVerse[tbOrigem].GetTheWordInterlinearLine));
        end;
        if pb <> nil then
        begin
          pb.StepIt;
          if (pb.Position mod 50) = 0 then
             Application.ProcessMessages;
        end;
        VersiculoSeguinte;
      end;
      IrPara(marcador);
      HabilitarEventosRolagem;
    end;

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
    lines.Destroy;
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
  tblTopicos: TSqlite3Dataset;
  tblConteudo: TSqlite3Dataset;
  tblConfig: TSqlite3Dataset;
  marcador: string;
  pares: TStringList;
  s: string;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  if FEscopo = etOT then
  begin
    MessageDlg(SInformation, SOTConcordanceNotImplementedYet, mtInformation, [mbOK], 0);
    exit;
  end;

  if not FileExists(arquivo) then
  begin
    try
      CopiarArquivo(ConcordanciaModelo[FEscopo], arquivo);
    except
      MessageDlg(SError, format(SFailedToCreateFile, [arquivo]), mtError, [mbOK], 0);
      exit;
    end;
  end;

  tblConteudo := CriarObjetoTabela(arquivo, 'content', 'topic_id');
  tblConteudo.Open;
  tblTopicos := CriarObjetoTabela(arquivo, 'topics', 'id');
  tblTopicos.Open;
  tblConfig := CriarObjetoTabela(arquivo, 'config', 'name');
  tblConfig.Open;

  AtualizarConfig(tblConfig, 'title',               format('%s %s', [abbreviation, IfThen(oeConcordDetalhada in opcoes, SAnalyticalConcordance, SSyntheticConcordance)]));
  AtualizarConfig(tblConfig, 'lang',                SLanguageId);
  AtualizarConfig(tblConfig, 'description.english', format('%s %s', [abbreviation, IfThen(oeConcordDetalhada in opcoes, 'Analytical Concordance', 'Synthetic Concordance')]));
  AtualizarConfig(tblConfig, 'title.english',       format('%s %s', [abbreviation, IfThen(oeConcordDetalhada in opcoes, 'Analytical Concordance', 'Synthetic Concordance')]));
  AtualizarConfig(tblConfig, 'version.date',        FormatDateTime('YYYY-MM-DD', Now));
  AtualizarConfig(tblConfig, 'description',         format('%s %s', [abbreviation, IfThen(oeConcordDetalhada in opcoes, SAnalyticalConcordance, SSyntheticConcordance)]));
  AtualizarConfig(tblConfig, 'abbrev',              abbreviation);

  try
    if assigned(pb) then
    begin
      pb.Position := 0;
      pb.Min := 0;
      pb.Max := QLinhas[FEscopo] + QStrongs[FEscopo]; // versículos do NT + qtde de strongs
      pb.Step := 1;
      pb.Visible := true;
    end;
    Concordancia := TConcordancia.Create;
    Concordancia.Detalhada := oeConcordDetalhada in opcoes;

    FExportando := true;
    PreRolagemVersiculo(nil);

    DesabilitarEventosRolagem;
    marcador := GetID;
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
            MessageDlg(SError, SCorruptedData + #13#10#13#10 + Referencia + #13#10 +
                 format('%s'#13#10'pares: %s'#13#10'%s'#13#10'%s',
                        [E.Message, FTblPares.FieldByName('pare_pares').AsString,
                        FATmpVerse[tbOrigem].DebugTokens, FATmpVerse[tbDestino].DebugTokens]),
                 mtError, [mbOK], 0);
        end;
        pares := FATmpVerse[tbOrigem].GetListaPares(tlTudo);
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

    tblTopicos.First;
    while not tblTopicos.EOF do
    begin
      if not tblConteudo.Locate('topic_id', tblTopicos.FieldByName('id').AsInteger, []) then
        continue;

      s := Concordancia.GetStrongRTF(tblTopicos.FieldByName('strong_orig_word').AsString, tblTopicos.FieldByName('subject').AsString);
      if s <> '' then
      begin
        tblConteudo.Edit;
        tblConteudo.FieldByName('data').AsString := s;
        tblConteudo.Post;
      end;

      if assigned(pb) then
      begin
        pb.StepIt;
        if (pb.Position mod 20) = 0 then
          Application.ProcessMessages;
      end;

      tblTopicos.Next;
    end;
  finally
    //ShowMessage('Position: ' + IntToStr(pb.Position));
    Application.ProcessMessages;
    tblConteudo.ApplyUpdates;
    tblConteudo.Close;
    tblConfig.ApplyUpdates;
    tblConfig.Close;
    tblTopicos.Close;
    Concordancia.Free;
    FExportando := false;
    IrPara(marcador);
    HabilitarEventosRolagem;
    PosRolagemVersiculo(nil);
    if assigned(pb) then
      pb.Visible := false;
  end;
end;

procedure TProjeto.AtribuirFonteTexto(fonte: TFont; textos: STextosBiblicos);
var
  t: TTipoTextoBiblico;
begin
  for t:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
    if t in textos then
    begin
      FAVersiculo[t].Fonte := fonte;
      //PosRolagemVersiculo(nil);
      AtribuirInfo(format('fonte%d.nome', [t]), fonte.Name);
      AtribuirInfo(format('fonte%d.tamanho', [t]), IntToStr(fonte.Size));
    end;
end;

procedure TProjeto.LimparTexto(texto: TTipoTextoBiblico);
begin
  with FTblPares do
  begin
    DesabilitarEventosRolagem;
    VersiculoInicial;
    while not FTblPares.EOF do
    begin
      SetVerseText('', texto, false);
      VersiculoSeguinte;
    end;
    HabilitarEventosRolagem;
  end;
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
  for t:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
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
  for t:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
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
  result := ObterTextoVersiculo('', texto);
end;

function TProjeto.ObterTextoVersiculo(Referencia: string; texto: TTipoTextoBiblico): string;
var
  marcador: string;
begin
  if Referencia <> '' then
  begin
    DesabilitarEventosRolagem;
    marcador := FTblPares.FieldByName('pare_id').AsString;
    IrPara(Referencia);
  end;

  result := FTblPares.Fields[FACamposTexto[texto]].AsString;

  if Referencia <> '' then
  begin
    IrPara(marcador);
    HabilitarEventosRolagem;
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

function TProjeto.GetTranslationSuggestions(syntagm: TSintagma): string;
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

procedure TProjeto.ToggleMostrarTags;
var
  t: TTipoTextoBiblico;
begin
  FMostrandoTags := not FMostrandoTags;

  for t:=low(TTipoTextoBiblico) to high(TTipoTextoBiblico) do
  begin
    if FMostrandoTags then
      FAVersiculo[t].MostrarTags
    else
      FAVersiculo[t].OcultarTags;
  end;
end;

end.
