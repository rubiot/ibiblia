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
  ExtCtrls, Controls, ComCtrls, StdCtrls, Graphics, Forms, Verse, SuggestionManager,
  MemoVersiculo, ONTTokenizer, Dialogs, dos, PCRE, LazLogger, Clipbrd,
  Syntagm, BibleScope, MySwordModule, fgl, PatchFile, LCLIntf, menus,
  CommentsView, ai_processing_manager, ai_processing_batch, ai_batch_work_item,
  ai_flows, llmclient, ai_flow_config, ai_debug, project_persistence, project_config,
  project_verse_repository, current_verse_workspace, verse_render_buffer,
  project_patch_applier, bible_tree_coordinator, ai_batch_orchestrator,
  project_database_bootstrap,
  dictionary_manager, project_exporter;
const
  // AI processing icon indices for ImageListTreeView in formprincipal
  // ImageListTreeView has indices 0-7 for normal states (0-3) and selected states (4-7)
  // Indices 8-9 are reserved for AI processing states
  ImgIndexPending = 8;     // AI processing: queued/pending (to be added to ImageListTreeView)
  ImgIndexProcessing = 9;  // AI processing: in progress (to be added to ImageListTreeView)

type
  TProjeto = class;

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
  TSuggestionMode = (smStatistical, smAI);

  TRegexList = specialize TFPGList<IRegex>;
  TIntegerList = specialize TFPGList<integer>;
  TPopupTrigger = (ptMouseHover, ptAltMouseHover, ptCtrlMouseHover);

  { AI Node State Tracking }
  TAINodeState = (ansNormal, ansPending, ansProcessing);

  // OBSOLETE: TAINodeInfo no longer used - TFrmAIProcessing manages its own tree
  TAINodeInfo = class
  public
    Node: TTreeNode;
    Reference: string; // format: "book,chapter,verse"
    State: TAINodeState;
    OriginalStateIndex: Integer;
    constructor Create(ANode: TTreeNode; ARef: string);
  end;

  TAINodeInfoList = specialize TFPGObjectList<TAINodeInfo>;

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

  AVersiculo = array[tbOrigem..tbConsulta2] of TVerse;
  ACamposTexto = array[tbOrigem..tbConsulta2] of integer;
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
    FCurrentVerseWorkspace: TCurrentVerseWorkspace;
    FPersistence: TProjectPersistence;
    FConfig: TProjectConfig;
    FVerseRepository: TProjectVerseRepository;
    FVerseRenderBuffer: TVerseRenderBuffer;
    FProjectExporter: TProjectExporter;
    FDictionaryManager: TDictionaryManager;
    FPatchApplier: TProjectPatchApplier;
    FBibleTreeCoordinator: TBibleTreeCoordinator;
    FTblPares: TSqlite3Dataset;
    FTblInfo: TSqlite3Dataset;
    FAVersiculo: AVersiculo;
    FUseAsExampleMenuItems: array[tbOrigem..tbConsulta2] of TMenuItem;
    FUseAsAssociationExampleMenuItems: array[tbOrigem..tbConsulta2] of TMenuItem;
    FUseAsTranslationExampleMenuItems: array[tbOrigem..tbConsulta2] of TMenuItem;
    FCopyAssociationExampleMenuItems: array[tbOrigem..tbConsulta2] of TMenuItem;
    FSwitchDestinationTextMenuItems: array[tbOrigem..tbConsulta2] of TMenuItem;
    FACamposTexto: ACamposTexto;
    FSuggestionManager: TSuggestionManager;
    FAIManager: TAIProcessingManager; // batch manager IA
    FAIProcessingNodes: TAINodeInfoList; // OBSOLETE: No longer used - modal manages its own tree
    FAIBatchCompletionShown: Boolean; // flag to prevent showing completion dialog multiple times
    FArvore: TTreeView;
    FBibleTreePointedNode: TTreeNode;
    FMemoComentarios: TControl;
    FRadioGroupSituacao: TRadioGroup;
    FTemporizador: TTimer;
    FAtrasoExibicao: Cardinal;
    FInterlinearMode: TInterlinearMode; // It's not there, I should check valid fields.
    FEscopo: TEscopoTexto;
    FPopupTrigger: TPopupTrigger;
    FVerseStrongsCountMode: TStrongsCountMode;
    FSuggestionMode: TSuggestionMode;
    FLastLLMUsage: TLLMUsage;
    FLastLLMCallCompleted: Boolean;
    FOnNovoVersiculo: TOnNovoVersiculoEvents;
    FOnSintagmaClick: TOnSintagmaClickEvent;
    FClosing: boolean;
    FAutoSave: boolean;
    FIgnoreMissingPatchWarnings: Boolean;
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
    function GetTokenizerVersion: Integer;
    function BuildAssociationFlowExample: TAIFlowExample;
    function BuildTranslationFlowExample: TAIFlowExample;
    function CreateFlowExample(const FlowId: string): TAIFlowExample;
    function PromptReplaceExistingFlowExample(const FlowId: string; const ExistingExample: TAIFlowExample): Boolean;
    function ConfirmMissingPatchReference(const Reference: string): Boolean;
    procedure ResetAIBatchManager;
    procedure PreencherArvore;
    procedure SetAtrasoExibicao(const AValue: Cardinal);
    procedure SetAutoSave(AValue: boolean);
    procedure SetChapterViewText(AValue: TTipoTextoBiblico);
    procedure SetComentarios(const AValue: string);
    procedure SetDisplayTags(AValue: boolean);
    procedure SetInterlinearMode(AValue: TInterlinearMode);
    procedure SetParagraphMode(AValue: TParagraphMode);
    procedure SetTokenizerVersion(AValue: Integer);
    procedure SetVerseStrongsCountMode(AValue: TStrongsCountMode);
    procedure SetOnAlterarVersiculo(const AValue: TOnAlterarVersiculoEvent);
    procedure SetOnSintagmaClick(const AValue: TOnSintagmaClickEvent);
    procedure SetPalavrasComStrongEmNegrito(AValue: boolean);
    procedure SetSituacao(const AValue: Integer);
    procedure HighlightStrongs(syntagm: TSyntagm);
    function CreateBibleTreeContextMenu: TPopupMenu;
    procedure UpdateReference(ref: string);
    function BibleTreeNodeToReference(const node: TTreeNode): TReference;
    procedure SetSituacaoDescricao(index: Integer; descricao: string);
    function HasSituacaoCustomizada: boolean;
    procedure MigrateStoredAssociationsToTokenizerVersion(OldVersion, NewVersion: Integer);
  protected
    function CriarObjetoTabela(db, tabela, chave: string): TSqlite3Dataset;
    function CriarObjetoQuery(db: string): TSQLQuery;
    function InserirInfo(info, valor: string): boolean;
    function AtualizarInfo(info, valor: string): boolean;
    function AtualizarConfig(table: TSqlite3Dataset; name, value: string): boolean;
    function ResgatarInfo(info: string): string;
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
    procedure OnExportText(Sender: TVerse);
    procedure OnVersePopup(Sender: TObject);
    procedure OnUseAssociationAsExample(Sender: TObject);
    procedure OnUseTranslationAsExample(Sender: TObject);
    procedure OnCopyAssociationExample(Sender: TObject);
    procedure OnSwitchDestinationAndReference1Texts(Sender: TObject);
    procedure OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
    procedure OnAlterarTextoVersiculo(Sender: TMemoVersiculo);
    procedure OnExibirDefinicao(Sender: TObject);
    procedure OnDblClickVersiculo(Sender: TObject);
    procedure OnBibleTreePopupItemClick(Sender: TObject);
    procedure OnBibleTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure HandleAutoSave(Sender: TObject);
    function GetVerseReferenceString(Verse: TVerse): string;
    procedure AttachProjectVersePopupMenu(texto: TTipoTextoBiblico);
    function FindVerseType(Verse: TVerse; out VerseType: TTipoTextoBiblico): Boolean;
    { AI Visual Feedback }
    procedure MarkNodeAsAIPending(node: TTreeNode; const ref: string);
    procedure MarkNodeAsAIProcessing(const ref: string);
    procedure RestoreNodeStatus(const ref: string);
    procedure ClearAllAINodeStates;
    function FindTreeNodeByReference(const ref: string): TTreeNode;
    function AtribuirDicStrong(dic: string; t: TTipoTextoBiblico): boolean;
    function AtribuirDicMorfo(dic: string; t: TTipoTextoBiblico): boolean;
    procedure AtualizarArvore;
    procedure AtualizarArvore(id: string);
  public
    procedure OnAIBatchClick(Sender: TObject);
    procedure OnAICancelClick(Sender: TObject);
    constructor Create; overload;
    constructor Create(paineis: array of TScrollbox; navegador: TTreeView; rsituacao: TRadioGroup; rcomentarios: TControl); overload;
    destructor Destroy; override;
    procedure Novo(nomedb, descricao: string);
    procedure Novo(nomedbvelho, nomedbnovo, descricao: string);
    procedure Novo(nomedb, descricao: string; tokenizerVersion: Integer); overload;
    procedure Novo(nomedbvelho, nomedbnovo, descricao: string; tokenizerVersion: Integer); overload;
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
    procedure RefreshUI;
    procedure Limpar;
    procedure Salvar;
    procedure SalvarParesVersiculo(Verse: TVerse); // Save pairs from any verse object (public for AI flows)
    procedure ApplyVerseText(const newText: string; texto: TTipoTextoBiblico; replace: Boolean);
    procedure SugerirAssociacaoEstatistica;
    procedure SugerirAssociacao;
    procedure SugerirAssociacaoAI;
    procedure StartAIAssociationBatch(NodesVerses: TAIBatchWorkItemList; MaxParallel: Integer); // New: batch IA association
    procedure StartAITranslationBatch(NodesVerses: TAIBatchWorkItemList; MaxParallel: Integer); // batch tradução IA
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
    procedure AtribuirInfo(info, valor: string);
    function ObterInfo(info: string): string;
    function GetFlowExamples(const FlowId: string): TAIFlowExampleArray;
    procedure SetFlowExamples(const FlowId: string; const Examples: TAIFlowExampleArray);
    function IncludeCurrentVerseAsFlowExample(const FlowId: string): Boolean;
    procedure RemoveFlowExample(const FlowId, ReferenceId: string);
    procedure ClearFlowExamples(const FlowId: string);
    function ObterTextoVersiculo(texto: TTipoTextoBiblico): string;
    function ObterTextoVersiculo(Referencia: string; texto: TTipoTextoBiblico): string;
    function ObterTextoSimplesVersiculo(texto: TTipoTextoBiblico): string;
    function ObterTextoSimplesVersiculo(Referencia: string; texto: TTipoTextoBiblico): string;
    function GetTranslationSuggestions(syntagm: TSyntagm): string;
    function GetChapterText: TStringList;
    procedure CompileInterlinearVerseRules(var rulesRx: TRegexList; var replaceTo: TStringList; mode: TInterlinearMode; exporting: boolean);
    procedure Translate;
    procedure ToggleDisplayTags;
    procedure StartScrollingSession;
    procedure FinishScrollingSession;
    procedure RewindChapter;
    procedure OnNewVerseSubscribe(const AValue: TOnNovoVersiculoEvent);
    procedure OnNewVerseUnsubscribe(const AValue: TOnNovoVersiculoEvent);
    procedure ApplyPatch(patch: TPatchFile; indexes: TIntegerList = nil);
    function ApplyAcceptedAIBatch(Batch, OriginalData: TAIProcessingBatch;
      SelectedIndexes: TAIBatchIndexList = nil): Integer;
    function GetPairs(ref: string): string;
    function GetComments(ref: string): string;
    function GetStatus(ref: string): Integer;
    procedure SetTextDescription(text: TTipoTextoBiblico; description: string);
    function GetTextDescription(text: TTipoTextoBiblico): string;
    function HasStoredAssociations: Boolean;
    procedure ClearStoredAssociations;
    procedure Vacuum;
    function GetSituacaoDescricao(index: Integer): string;

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
    property MemoComentarios: TControl read FMemoComentarios write FMemoComentarios;
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
    property TokenizerVersion: Integer read GetTokenizerVersion write SetTokenizerVersion;
    property AutoSave: boolean read FAutoSave write SetAutoSave;
    property LastLLMUsage: TLLMUsage read FLastLLMUsage;
    property LastLLMCallCompleted: Boolean read FLastLLMCallCompleted;
    property Pairs: string read GetPairs;
    property VerseObjects: AVersiculo read FAVersiculo;
    property SuggestionMode: TSuggestionMode read FSuggestionMode write FSuggestionMode;
  end;

  { TProjectVerseStatusCursor — implements IVerseStatusCursor so that
    TBibleTreeCoordinator.RecomputeAllStatuses owns the iteration contract
    rather than receiving four callback closures over TProjeto. }
  TProjectVerseStatusCursor = class(TInterfacedObject, IVerseStatusCursor)
  private
    FOwner: TProjeto;
  public
    constructor Create(AOwner: TProjeto);
    procedure Reset;
    function EOF: Boolean;
    procedure Advance;
    function CurrentStatus: Integer;
  end;

{$INCLUDE include/bookdefs.inc}

resourcestring
  SSetStatusChildren = 'Set status of all children to...';
  SMarkAsFmt = 'Mark %s as...';
  SApplyAITo = 'Apply AI flow to...';
  SApplyAIToFmt = 'Apply AI flow to %s';
  SAssociation = 'Association';
  STranslation = 'Translation';
  SAssociate = 'Associate';
  STranslate = 'Translate';
  SUseVerseAsExample = 'Use verse as example...';
  SUseVerseAsAssociationExample = 'In association flow';
  SUseVerseAsTranslationExample = 'In translation flow';
  SCopyAssociationExample = 'Copy association example for AI instructions';
  SSwitchDestinationAndReference1Texts = 'Switch destination text with reference 1';
  SExampleSaved = 'Example saved.';
  SExampleAlreadyExistsTitle = 'Example already exists';
  SExampleAlreadyExistsPrompt = 'This verse is already saved as an example for the %s flow.' + #13#10 + #13#10 +
                                'Existing entry: %s' + #13#10 + #13#10 +
                                'Do you want to replace it with the current verse state?';
  SAssociationExampleRequiresPair = 'Association examples require an active source verse paired with a destination verse.';
  SAssociationExampleRequiresAssociations = 'Association examples require existing associations in the current verse.';
  STranslationExampleRequiresPair = 'Translation examples require an active source verse paired with a destination verse.';
  STranslationExampleRequiresText = 'Translation examples require translated text in the destination verse.';
  SCancelAIProcessing = 'Cancel AI processing';
  SConfirmApplyAIToBook = 'Are you sure you want to apply the AI flow to the entire book %s?';
  SConfirmApplyAIToChapter = 'Are you sure you want to apply the AI flow to chapter %s?';
  SAnalyzingVerse = 'Analyzing verse...';

  SStatusNotAssociated = 'Not associated';
  SStatusAssociating = 'Associating';
  SStatusNeedsReview = 'Needs review';
  SStatusAssociated = 'Associated!';
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

procedure UpdateDefaultSituacaoDescricao;

implementation

uses formpopup, formverserules, formaiprocessing;

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

{ TProjectVerseStatusCursor }

constructor TProjectVerseStatusCursor.Create(AOwner: TProjeto);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TProjectVerseStatusCursor.Reset;
begin
  FOwner.VersiculoInicial;
end;

function TProjectVerseStatusCursor.EOF: Boolean;
begin
  Result := not Assigned(FOwner.FPersistence) or FOwner.FPersistence.EOF;
end;

procedure TProjectVerseStatusCursor.Advance;
begin
  FOwner.VersiculoSeguinte;
end;

function TProjectVerseStatusCursor.CurrentStatus: Integer;
begin
  Result := FOwner.Situacao;
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

function TProjeto.BuildAssociationFlowExample: TAIFlowExample;
var
  SourceVerse, TargetVerse: TVerse;
begin
  SourceVerse := FAVersiculo[tbOrigem];
  TargetVerse := FAVersiculo[tbDestino];

  if (not Assigned(SourceVerse)) or (not Assigned(TargetVerse)) or (SourceVerse.VersiculoPar <> TargetVerse) then
    raise Exception.Create(SAssociationExampleRequiresPair);

  if not SourceVerse.HasAssociations then
    raise Exception.Create(SAssociationExampleRequiresAssociations);

  Result.ReferenceId := ID;
  Result.ReferenceCaption := FormattedReference;
  Result.SourceText := SourceVerse.TextoSimples;
  Result.TargetText := TargetVerse.TextoSimples;
  Result.ExpectedResponse := SourceVerse.GetAssociationsJSON;
end;

function TProjeto.BuildTranslationFlowExample: TAIFlowExample;
var
  SourceVerse, TargetVerse: TVerse;
begin
  SourceVerse := FAVersiculo[tbOrigem];
  TargetVerse := FAVersiculo[tbDestino];

  if (not Assigned(SourceVerse)) or (not Assigned(TargetVerse)) or (SourceVerse.VersiculoPar <> TargetVerse) then
    raise Exception.Create(STranslationExampleRequiresPair);

  if Trim(TargetVerse.TextoSimples) = '' then
    raise Exception.Create(STranslationExampleRequiresText);

  Result.ReferenceId := ID;
  Result.ReferenceCaption := FormattedReference;
  Result.SourceText := SourceVerse.TextoSimples;
  Result.TargetText := TargetVerse.TextoSimples;
  Result.ExpectedResponse := TargetVerse.TextoSimples;
end;

function TProjeto.CreateFlowExample(const FlowId: string): TAIFlowExample;
begin
  case LowerCase(Trim(FlowId)) of
    'association': Result := BuildAssociationFlowExample;
    'translation': Result := BuildTranslationFlowExample;
  else
    raise Exception.CreateFmt('Unknown flow ID for example creation: %s', [FlowId]);
  end;
end;

function TProjeto.PromptReplaceExistingFlowExample(const FlowId: string; const ExistingExample: TAIFlowExample): Boolean;
var
  FlowCaption: string;
begin
  if SameText(FlowId, 'association') then
    FlowCaption := SAssociation
  else if SameText(FlowId, 'translation') then
    FlowCaption := STranslation
  else
    FlowCaption := FlowId;

  Result := MessageDlg(
    SExampleAlreadyExistsTitle,
    Format(SExampleAlreadyExistsPrompt, [FlowCaption, FormatFlowExampleSummary(ExistingExample)]),
    mtConfirmation,
    [mbYes, mbNo],
    0
  ) = mrYes;
end;

procedure TProjeto.Novo(nomedb, descricao: string);
begin
  Novo(nomedb, descricao, TTokenizerFactory.LATEST_VERSION);
end;

procedure TProjeto.Novo(nomedbvelho, nomedbnovo, descricao: string);
begin
  Novo(nomedbnovo, descricao, TTokenizerFactory.LATEST_VERSION);
end;

procedure TProjeto.Novo(nomedb, descricao: string; tokenizerVersion: Integer);
begin
  TProjectDatabaseBootstrap.CreateProjectDatabase(nomedb, FEscopo);

  Abrir(nomedb);

  AtribuirInfo('descricao', descricao);
  TokenizerVersion := tokenizerVersion;
  FExibirDefComCtrl := false;  //FTblInfo.ExecuteDirect('COMMIT;');
  VersiculoInicial;
  Commit;
end;

procedure TProjeto.Novo(nomedbvelho, nomedbnovo, descricao: string; tokenizerVersion: Integer);
begin
  Novo(nomedbnovo, descricao, tokenizerVersion);
end;

function TProjeto.GetFormattedReference: string;
begin
  result := format('%s %d:%d', [Book, Chapter, Verse]);
end;

function TProjeto.GetPairs: string;
begin
  Result := FPersistence.CurrentPairs;
end;

function TProjeto.GetParagraphMode: TParagraphMode;
begin
  Result := TParagraphMode(FConfig.ReadParagraphMode(Ord(pmParagraph)));
end;

function TProjeto.GetTokenizerVersion: Integer;
begin
  Result := TTokenizerFactory.NormalizeVersion(
    FConfig.ReadTokenizerVersion(TTokenizerFactory.PreferredVersion));
end;

function TProjeto.GetPairs(ref: string): string;
var
  verseData: TVerseData;
begin
  result := '';
  StartScrollingSession;
  try
    if FVerseRepository.TryReadVerseData(ref, ref, verseData) then
      Result := verseData.Associations;
  finally
    FinishScrollingSession;
  end;
end;

function TProjeto.GetSituacao: Integer;
begin
  Result := FPersistence.CurrentStatus;
end;

procedure TProjeto.PreencherArvore;
begin
  FBibleTreeCoordinator.Populate(FEscopo, @OnMudancaVersiculo);
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
  FConfig.WriteChapterViewText(Ord(AValue));
end;

procedure TProjeto.SetComentarios(const AValue: string);
begin
  if FPersistence.CurrentComments = AValue then
    exit;
  FPersistence.WriteCurrentComments(AValue);
end;

procedure TProjeto.SetDisplayTags(AValue: boolean);
begin
  if FDisplayTags = AValue then
    Exit;

  FDisplayTags := AValue;
  FCurrentVerseWorkspace.ApplyDisplayTags(FAVersiculo, FDisplayTags);
end;

procedure TProjeto.SetInterlinearMode(AValue: TInterlinearMode);
begin
  FConfig.WriteInterlinearMode(Ord(AValue));
end;

procedure TProjeto.SetParagraphMode(AValue: TParagraphMode);
begin
  FConfig.WriteParagraphMode(Ord(AValue));
end;

procedure TProjeto.SetTokenizerVersion(AValue: Integer);
var
  currentVersion: Integer;
begin
  AValue := TTokenizerFactory.NormalizeVersion(AValue);
  currentVersion := TokenizerVersion;
  if currentVersion = AValue then
    Exit;

  MigrateStoredAssociationsToTokenizerVersion(currentVersion, AValue);

  FConfig.WriteTokenizerVersion(AValue);
  TTokenizerFactory.PreferredVersion := AValue;

  FCurrentVerseWorkspace.ApplyTokenizerVersion(FAVersiculo, AValue);

  if Assigned(FVerseRenderBuffer) then
    FVerseRenderBuffer.SetTokenizerVersion(AValue);

  if Assigned(FTblPares) and FAtivo then
    PosRolagemVersiculo(nil);
end;

procedure TProjeto.MigrateStoredAssociationsToTokenizerVersion(OldVersion,
  NewVersion: Integer);
var
  oldPairs: string;
begin
  if (OldVersion = NewVersion) or not Assigned(FPersistence) then
    Exit;

  if not Assigned(FVerseRenderBuffer) then
    Exit;

  StartScrollingSession;
  try
    VersiculoInicial;
    while not FPersistence.EOF do
    begin
      oldPairs := FPersistence.CurrentPairs;
      if not oldPairs.IsEmpty then
        FPersistence.WriteCurrentPairs(
          FVerseRenderBuffer.RetokenizeAssociations(
            FPersistence.CurrentText(FACamposTexto[tbOrigem]),
            FPersistence.CurrentText(FACamposTexto[tbDestino]),
            oldPairs,
            OldVersion,
            NewVersion));

      VersiculoSeguinte;
    end;
  finally
    FVerseRenderBuffer.SetTokenizerVersion(NewVersion);
    FinishScrollingSession;
  end;
end;

procedure TProjeto.SetVerseStrongsCountMode(AValue: TStrongsCountMode);
begin
  if FVerseStrongsCountMode = AValue then
    Exit;

  FVerseStrongsCountMode := AValue;
  FCurrentVerseWorkspace.ApplyStrongsCountMode(FAVersiculo, AValue);
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

function TProjeto.ConfirmMissingPatchReference(const Reference: string): Boolean;
begin
  Result := FIgnoreMissingPatchWarnings;
  if Result then
    Exit;

  if MessageDlg(
       SReferenceNotFound,
       Format(SReferenceNotFoundWarning, [Reference]),
       mtWarning,
       [mbOK, mbIgnore],
       0
     ) = mrIgnore then
  begin
    FIgnoreMissingPatchWarnings := True;
    Result := True;
  end;
end;

procedure TProjeto.ApplyPatch(patch: TPatchFile; indexes: TIntegerList = nil);
var
  oldAutoSaveValue: boolean;
  selectedIndexes: TPatchIndexArray;

  function BuildSelectedPatchIndexes: TPatchIndexArray;
  var
    PatchIndex: Integer;
  begin
    if not Assigned(indexes) then
      Exit(nil);

    SetLength(Result, indexes.Count);
    for PatchIndex := 0 to indexes.Count - 1 do
      Result[PatchIndex] := indexes[PatchIndex];
  end;
begin
  selectedIndexes := BuildSelectedPatchIndexes;
  FIgnoreMissingPatchWarnings := False;

  try
    StartScrollingSession;
    FPatchApplier.ApplyPatch(patch, selectedIndexes, @ConfirmMissingPatchReference);
  finally
    FinishScrollingSession;
  end;

  oldAutoSaveValue := FAutoSave;
  FAutoSave := false;
  RefreshUI;
  FAutoSave := oldAutoSaveValue;
end;

function TProjeto.ApplyAcceptedAIBatch(Batch, OriginalData: TAIProcessingBatch;
  SelectedIndexes: TAIBatchIndexList = nil): Integer;
var
  Patch: TPatchFile;
begin
  Result := 0;
  if not Assigned(Batch) then
    Exit;

  Patch := Batch.CreatePatch(OriginalData, SelectedIndexes, Result);
  try
    if Result > 0 then
      ApplyPatch(Patch, nil);
  finally
    Patch.Free;
  end;
end;

procedure TProjeto.SetOnSintagmaClick(const AValue: TOnSintagmaClickEvent);
begin
  if FOnSintagmaClick = AValue then exit;
  FOnSintagmaClick := AValue;
end;

procedure TProjeto.SetPalavrasComStrongEmNegrito(AValue: boolean);
begin
  if FPalavrasComStrongEmNegrito=AValue then Exit;
  FPalavrasComStrongEmNegrito:=AValue;
  FCurrentVerseWorkspace.ApplyBoldStrongs(FAVersiculo, AValue);
end;

procedure TProjeto.SetSituacao(const AValue: Integer);
var
  NormalizedStatus: Integer;
begin
  NormalizedStatus := IfThen((AValue < 0) or (AValue > 3), 1, AValue);
  if FClosing or (FPersistence.CurrentStatus = NormalizedStatus) then
    exit;
  FPersistence.WriteCurrentStatus(NormalizedStatus);
  AtualizarArvore(ID);
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

{ TAINodeInfo }

constructor TAINodeInfo.Create(ANode: TTreeNode; ARef: string);
begin
  inherited Create;
  Node := ANode;
  Reference := ARef;
  State := ansNormal;
  OriginalStateIndex := -1;
end;

{ AI Visual Feedback Methods }

// OBSOLETE: These methods are no longer used.
// Tree node visual updates during AI processing are now handled exclusively
// by TFrmAIProcessing modal. The main project tree (FArvore) is not affected
// during processing - it only updates when user clicks Apply in the modal.

procedure TProjeto.MarkNodeAsAIPending(node: TTreeNode; const ref: string);
begin
  // No-op: Modal handles its own tree updates
end;

procedure TProjeto.MarkNodeAsAIProcessing(const ref: string);
begin
  // No-op: Modal handles its own tree updates
end;

procedure TProjeto.RestoreNodeStatus(const ref: string);
begin
  // No-op: Modal handles its own tree updates
end;

procedure TProjeto.ClearAllAINodeStates;
begin
  // No-op: Modal handles its own tree updates
end;

function TProjeto.FindTreeNodeByReference(const ref: string): TTreeNode;
var
  refBookId, refChapter, refVerse: Integer;
  nb, nc, nv: TTreeNode;
begin
  Result := nil;
  if not Assigned(FArvore) then Exit;

  // Parse reference
  if Sscanf(ref, '%d,%d,%d', [@refBookId, @refChapter, @refVerse]) <> 3 then
    Exit;

  // Find book node
  nb := FArvore.Items.GetFirstNode;
  while Assigned(nb) do
  begin
    if PtrInt(nb.Data) = refBookId then
      Break;
    nb := nb.GetNextSibling;
  end;

  if not Assigned(nb) then Exit;

  // Find chapter node
  nc := nb.GetFirstChild;
  while Assigned(nc) do
  begin
    if PtrInt(nc.Data) = refChapter then
      Break;
    nc := nc.GetNextSibling;
  end;

  if not Assigned(nc) then Exit;

  // Find verse node
  nv := nc.GetFirstChild;
  while Assigned(nv) do
  begin
    if PtrInt(nv.Data) = refVerse then
    begin
      Result := nv;
      Exit;
    end;
    nv := nv.GetNextSibling;
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
  aiMenu: TMenuItem;
  aiChapterItem: TMenuItem;
  aiCancelItem: TMenuItem;
  aiTrChapterItem: TMenuItem;


  procedure CollectVersesFromNode(node: TTreeNode; dest: TList);
  var child: TTreeNode;
  begin
    if node = nil then Exit;
    if not node.HasChildren then
    begin
      GoToReference(FBibleTreeCoordinator.NodeToReferenceID(FEscopo, node));
      dest.Add(FAVersiculo[tbOrigem]);
      Exit;
    end;
    child := node.GetFirstChild;
    while child <> nil do
    begin
      CollectVersesFromNode(child, dest);
      child := child.GetNextSibling;
    end;
  end;

begin
  result := TPopupMenu.Create(FArvore);
  result.Parent := FArvore;
  result.Images := FArvore.Images;

  item := TMenuItem.Create(result);
  // Status submenu: 'Marcar <referência> como...'
  item.Caption := SSetStatusChildren;
  result.Items.Add(item);
  for status:=0 to 3 do
    AddSubItemForStatus(item, status);

  // 'Aplicar fluxo de IA a <referência>...' submenu (will be updated at popup time)
  aiMenu := TMenuItem.Create(result);
  aiMenu.Caption := SApplyAITo;
  result.Items.Add(aiMenu);
  // Associação
  aiChapterItem := TMenuItem.Create(aiMenu);
  aiChapterItem.Caption := SAssociation;
  aiChapterItem.OnClick := @Self.OnAIBatchClick;
  aiChapterItem.Tag := 1001; // assoc chapter/book/verse depending on popup (use non-colliding tag)
  aiMenu.Add(aiChapterItem);
  // Tradução
  aiTrChapterItem := TMenuItem.Create(aiMenu);
  aiTrChapterItem.Caption := STranslation;
  aiTrChapterItem.OnClick := @Self.OnAIBatchClick;
  aiTrChapterItem.Tag := 1011; // translate chapter/book/verse depending on popup (use non-colliding tag)
  aiMenu.Add(aiTrChapterItem);
  // Cancel item
  aiCancelItem := TMenuItem.Create(aiMenu);
  aiCancelItem.Caption := SCancelAIProcessing;
  aiCancelItem.Tag := 9001; // identificador para habilitação dinâmica
  aiCancelItem.OnClick := @Self.OnAICancelClick;
  if Assigned(FAIManager) and (not FAIManager.IsIdle) then
    aiCancelItem.Enabled := True
  else
    aiCancelItem.Enabled := False;
  aiMenu.Add(aiCancelItem);
end;



procedure TProjeto.OnAICancelClick(Sender: TObject);
begin
  if Assigned(FAIManager) then
  begin
    FAIManager.CancelAll;
    ClearAllAINodeStates; // Restore node icons
  end;
end;

procedure TProjeto.ResetAIBatchManager;
begin
  if Assigned(FAIManager) then
  begin
    FAIManager.CancelAll;
    ClearAllAINodeStates;
    FreeAndNil(FAIManager);
  end;
end;

procedure TProjeto.OnAIBatchClick(Sender: TObject);
var
  list: TAIBatchWorkItemList;
  maxPar: Integer;
  clickedNode: TTreeNode;
  flowId: string;
  flow: IAIVerseFlow;
  batchPrep: TAIBatchPreparation;
  orchestrator: TAIBatchOrchestrator;
  previewOptions: TAIProcessingPreviewOptions;

  procedure CollectVersesFromNode(node: TTreeNode; dest: TAIBatchWorkItemList);
  var
    child: TTreeNode;
    ref: TReference;
    verseData: TVerseData;
  begin
    if node = nil then Exit;

    // If leaf node (verse), collect this single verse as raw data
    if not node.HasChildren then
    begin
      ref := BibleTreeNodeToReference(node);
      verseData := FVerseRepository.ReadVerseData(
        Format('%d,%d,%d', [ref.BookID, ref.Chapter, ref.Verse]),
        ref.Book + ' ' + IntToStr(ref.Chapter) + ':' + IntToStr(ref.Verse)
      );

      dest.AddItem(TAIBatchWorkItem.Create(TokenizerVersion, verseData));
      Exit;
    end;

    // If not a leaf, recursively collect from children
    child := node.GetFirstChild;
    while child <> nil do
    begin
      CollectVersesFromNode(child, dest);
      child := child.GetNextSibling;
    end;
  end;

begin
  if FArvore = nil then Exit;
  clickedNode := FBibleTreePointedNode;  // Use the node where popup was triggered, not the selected node
  if clickedNode = nil then Exit;
  list := TAIBatchWorkItemList.Create;
  try
    // Determine action (association vs translation) by Tag and scope by pointed node level
    if not (Sender is TMenuItem) then Exit;

    // Confirm BEFORE collecting verses (expensive operation)
    if FBibleTreePointedNode.Level = 0 then // book
    begin
      if MessageDlg(SConfirmation, Format(SConfirmApplyAIToBook, [FBibleTreePointedNode.Text]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;
    end
    else if FBibleTreePointedNode.Level = 1 then // chapter
    begin
      if MessageDlg(SConfirmation, Format(SConfirmApplyAIToChapter, [Format('%s %s', [FBibleTreePointedNode.Parent.Text, FBibleTreePointedNode.Text])]), mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        Exit;
    end;

    // Now collect verses after confirmation
    StartScrollingSession;
    try
      case FBibleTreePointedNode.Level of
        0: // book
          CollectVersesFromNode(clickedNode, list);
        1: // chapter
          CollectVersesFromNode(clickedNode, list);
        2: // verse
          CollectVersesFromNode(clickedNode, list);
      else
        CollectVersesFromNode(clickedNode, list);
      end;
    finally
      FinishScrollingSession;
    end;
    if list.Count = 0 then Exit;

    maxPar := StrToIntDef(ObterInfo(AI_COMMON_CONFIG.MaxParallel), 2);

    if ObterInfo(AI_COMMON_CONFIG.Model).IsEmpty or ObterInfo(AI_COMMON_CONFIG.ApiKey).IsEmpty then
    begin
      ShowMessage('AI associator not configured.');
      Exit;
    end;

    if Assigned(FAIManager) then
    begin
      FAIManager.CancelAll;
      ClearAllAINodeStates;
      FreeAndNil(FAIManager);
    end;

    FAIBatchCompletionShown := False;

    // Note: Tree node visual updates are now handled by TFrmAIProcessing modal
    // The main tree (FArvore) is not affected until Apply is clicked

    if TMenuItem(Sender).Tag = 1001 then
      flowId := 'association'
    else if TMenuItem(Sender).Tag = 1011 then
      flowId := 'translation'
    else
      Exit;

    orchestrator := TAIBatchOrchestrator.Create(
      FConfig,
      TAIBatchContextAdapter.Create(
        @ResetAIBatchManager,
        @FindTreeNodeByReference,
        @MarkNodeAsAIPending,
        @HandleAutoSave,
        @GetVerseReferenceString
      )
    );
    try
      batchPrep := orchestrator.PrepareBatch(flowId, list, maxPar);
    finally
      orchestrator.Free;
    end;

    flow := batchPrep.Flow;
    FAIManager := batchPrep.Manager;
    if flow = nil then Exit;
    if not Assigned(FAIManager) then Exit;

    previewOptions.TokenizerVersion := TokenizerVersion;
    previewOptions.SourceFont := ObterFonteTexto(tbOrigem);
    previewOptions.DestinationFont := ObterFonteTexto(tbDestino);
    previewOptions.BoldStrongs := PalavrasComStrongEmNegrito;
    previewOptions.StrongsCountMode := MostrarQtdStrongs;

    // Use global form instance instead of creating new one
    if Assigned(FrmAIProcessing) then
    begin
      if FrmAIProcessing.StartProcessing(FAIManager, flow, list, previewOptions, @ApplyAcceptedAIBatch) then
      begin
        FAIManager.StartBatch(flow, list);
        if FrmAIProcessing.ShowModal = mrOK then
        begin
          FrmAIProcessing.Apply;
        end;
      end;
    end;
  finally
    list.Free;
  end;
end;

procedure TProjeto.UpdateReference(ref: string);
begin
  Sscanf(ref, '%d,%d,%d', [@FReference.BookID, @FReference.Chapter, @FReference.Verse]);
  FReference.Book := NLivros[FEscopo][FReference.BookID-OffsetLivros[FEscopo]-1];
end;

function TProjeto.BibleTreeNodeToReference(const node: TTreeNode): TReference;
var
  ReferenceID: string;
begin
  ReferenceID := FBibleTreeCoordinator.NodeToReferenceID(FEscopo, node);
  Sscanf(ReferenceID, '%d,%d,%d', [@Result.BookID, @Result.Chapter, @Result.Verse]);
  result.Book    := NLivrosONT[result.BookID];
end;

function TProjeto.GetModificado: boolean;
var
  srcPending: boolean;
begin
  srcPending   := (assigned(FAVersiculo[tbOrigem]) and FAVersiculo[tbOrigem].Modificado);

  result := FPersistence.UpdatesPending or srcPending;
end;

function TProjeto.GetCaminho: string;
begin
  if Assigned(FPersistence) then
    Result := FPersistence.FileName
  else
    Result := '';
end;

function TProjeto.GetChapterViewText: TTipoTextoBiblico;
var
  t: string;
begin
  t := ObterInfo('chapterview.text');
  if t.IsEmpty then
    t := Ord(tbDestino).ToString;
  Result := TTipoTextoBiblico(t.ToInteger);
end;

function TProjeto.GetChapterText: TStringList;
  function GetText(rulesRx: TRegexList; replaceTo: TStringList): string;
  var
    i: integer;
    corruptionDetails: string;
  begin
    case ChapterViewText of
      tbOrigem, tbDestino, tbConsulta1, tbConsulta2:
        result := FPersistence.CurrentText(FACamposTexto[ChapterViewText]).Replace(#239#187#191, '');
      tbInterlinear:
      begin
        if FPersistence.CurrentPairs.IsEmpty then
          result := FPersistence.CurrentText(FACamposTexto[tbOrigem])
        else
        begin
          if InterlinearMode = imInterlinear then
          begin
            if not FVerseRenderBuffer.TryBuildInterlinearLine(
                FPersistence.CurrentText(FACamposTexto[tbOrigem]),
                FPersistence.CurrentText(FACamposTexto[tbDestino]),
                FPersistence.CurrentPairs,
                rimMySword,
                result,
                corruptionDetails) then
              MessageDlg(SError, SCorruptedData + #13#10#13#10 + FormattedReference + #13#10 +
                   corruptionDetails,
                   mtError, [mbOK], 0);
          end
          else if InterlinearMode = imIntralinear then
          begin
            if not FVerseRenderBuffer.TryBuildInterlinearLine(
                FPersistence.CurrentText(FACamposTexto[tbOrigem]),
                FPersistence.CurrentText(FACamposTexto[tbDestino]),
                FPersistence.CurrentPairs,
                rimTheWord,
                result,
                corruptionDetails) then
              MessageDlg(SError, SCorruptedData + #13#10#13#10 + FormattedReference + #13#10 +
                   corruptionDetails,
                   mtError, [mbOK], 0);
          end
          else
            raise Exception.Create(Format('Invalid interlinear mode [%d]', [Ord(InterlinearMode)]));

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
begin
  bkch := Format('%d,%d,', [BookID, Chapter]);
  result := TStringList.Create;

  rulesRx := nil;
  replaceTo := nil;
  if ChapterViewText = tbInterlinear then
    CompileInterlinearVerseRules(rulesRx, replaceTo, InterlinearMode, false);

  StartScrollingSession;
  RewindChapter;
  while not FPersistence.EOF and GetID().StartsWith(bkch) do
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
  FinishScrollingSession;

  if ChapterViewText = tbInterlinear then
  begin
    rulesRx.Free;
    replaceTo.Free;
  end;
end;

procedure TProjeto.CompileInterlinearVerseRules(var rulesRx: TRegexList; var replaceTo: TStringList; mode: TInterlinearMode; exporting: boolean);
var
  verseRules: string;
  keyValue: TStringArray;
  rule: string;
begin
  rulesRx := TRegexList.Create;
  replaceTo := TStringList.Create;
  if mode = imInterlinear then
    verseRules := FConfig.ReadVerseRules(True)
  else
    verseRules := FConfig.ReadVerseRules(False);

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
  Result := FPersistence.CurrentComments;
end;

function TProjeto.GetComments(ref: string): string;
var
  verseData: TVerseData;
begin
  result := '';
  StartScrollingSession;
  try
    if FVerseRepository.TryReadVerseData(ref, ref, verseData) then
      Result := verseData.Comments;
  finally
    FinishScrollingSession;
  end;
end;

function TProjeto.GetStatus(ref: string): Integer;
var
  verseData: TVerseData;
begin
  result := 0;
  StartScrollingSession;
  try
    if FVerseRepository.TryReadVerseData(ref, ref, verseData) then
      Result := verseData.Status;
  finally
    FinishScrollingSession;
  end;
end;

procedure TProjeto.SetTextDescription(text: TTipoTextoBiblico;
  description: string);
begin
  FConfig.WriteTextDescription(Integer(text), description);
end;

function TProjeto.GetTextDescription(text: TTipoTextoBiblico): string;
begin
  Result := FConfig.ReadTextDescription(Integer(text));
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
  Result := FPersistence.CurrentPairId;
end;

function TProjeto.GetInterlinearMode: TInterlinearMode;
begin
  Result := TInterlinearMode(FConfig.ReadInterlinearMode(Ord(imInterlinear)));
end;

function TProjeto.CriarObjetoTabela(db, tabela, chave: string): TSqlite3Dataset;
begin
  Result := TProjectPersistence.CreateTableDataset(db, tabela, chave);
end;

function TProjeto.CriarObjetoQuery(db: string): TSQLQuery;
begin
  Result := TProjectPersistence.CreateQueryObject(db);
end;

function TProjeto.InserirInfo(info, valor: string): boolean;
begin
  Result := FPersistence.InsertInfo(info, valor);
end;

function TProjeto.AtualizarInfo(info, valor: string): boolean;
begin
  Result := FPersistence.UpdateInfo(info, valor);
end;

function TProjeto.AtualizarConfig(table: TSqlite3Dataset; name, value: string): boolean;
begin
  Result := TProjectPersistence.UpdateConfigTable(table, name, value);
end;

function TProjeto.ResgatarInfo(info: string): string;
begin
  if Assigned(FPersistence) then
    Result := FPersistence.ReadInfo(info)
  else
    Result := '';
end;

procedure TProjeto.SetVerseText(versiculo: string;
  texto: TTipoTextoBiblico; replace: boolean);
var
  corruptionDetails: string;
begin
  FCurrentVerseWorkspace.SetCurrentVerseText(FPersistence, FVerseRenderBuffer,
    FACamposTexto, versiculo, Ord(texto), Ord(tbOrigem), Ord(tbDestino),
    replace, not HasSituacaoCustomizada, corruptionDetails);

  if not corruptionDetails.IsEmpty then
    MessageDlg(SError, SCorruptedData + #13#10#13#10 + FormattedReference + #13#10 +
         corruptionDetails,
         mtError, [mbOK], 0);
end;

procedure TProjeto.ApplyVerseText(const newText: string; texto: TTipoTextoBiblico; replace: Boolean);
begin
  SetVerseText(newText, texto, replace);
end;

procedure TProjeto.PreRolagemVersiculo(DataSet: TDataSet);
//var starttime: DWord;
begin
  if not FScrollEventsEnabled or not Assigned(FAVersiculo[tbOrigem]) then
    exit;

  //starttime := getTickCount;

  FCurrentVerseWorkspace.SaveBeforeScroll(FPersistence, FSuggestionManager,
    FMemoVersiculo, FMemoComentarios, FAVersiculo, FACamposTexto,
    Ord(tbOrigem), Ord(tbDestino));

  //DebugLn('  TProjeto.PreRolagemVersiculo: %d milliseconds', [getTickCount-starttime]);
end;

procedure TProjeto.PosRolagemVersiculo(DataSet: TDataSet);
var
  e: TOnNovoVersiculoEvent;
  loadResult: TCurrentVerseLoadResult;
  //starttime: DWord;
begin
  UpdateReference(GetID());

  if not FScrollEventsEnabled then
    exit;

  //starttime := getTickCount;

  loadResult := FCurrentVerseWorkspace.LoadCurrentVerse(FPersistence,
    FAVersiculo, FACamposTexto, FormattedReference, GetID(),
    Ord(tbOrigem), Ord(tbDestino));

  if assigned(FAVersiculo[tbOrigem]) and assigned(FAVersiculo[tbDestino]) then
  begin
    try
      FCurrentVerseWorkspace.ApplyLoadedAssociations(FAVersiculo[tbOrigem],
        loadResult.Associations);
    except
      on E: Exception do
      MessageDlg(SError, SCorruptedData + #13#10#13#10 + FormattedReference + #13#10#13#10 +
           format('%s'#13#10#13#10'pares: %s'#13#10'%s'#13#10'%s',
                  [E.Message, loadResult.Associations,
                  FAVersiculo[tbOrigem].DebugTokens, FAVersiculo[tbDestino].DebugTokens]),
           mtError, [mbOK], 0);

    end;
  end;

  FCurrentVerseWorkspace.FinalizeLoadedVerse(FAVersiculo);

  FBibleTreeCoordinator.SelectReference(FEscopo, ID);

  if assigned(FMemoComentarios) then
  begin
    if FMemoComentarios is TCommentsView then
      TCommentsView(FMemoComentarios).Text := loadResult.Comments
    else if FMemoComentarios is TMemo then
      TMemo(FMemoComentarios).Text := loadResult.Comments;
  end;

  if assigned(FRadioGroupSituacao) then
    FRadioGroupSituacao.ItemIndex := loadResult.Status;

  if loadResult.DestinationTextEmpty then // open verse to edition if is empty
    OnDblClickVersiculo(FAVersiculo[tbDestino].Painel);

  for e in FOnNovoVersiculo do
    e(self);

  if fautosave and not fexportando then
    salvar;

  //DebugLn('  TProjeto.PosRolagemVersiculo: %d milliseconds', [getTickCount-starttime]);
end;

procedure TProjeto.SalvarPares;
begin
  FCurrentVerseWorkspace.SavePairs(FPersistence, FAVersiculo[tbOrigem]);
end;

procedure TProjeto.SalvarParesVersiculo(Verse: TVerse);
begin
  // Save pairs from a specific verse object (works for temp or visual verses)
  // Assumes database cursor is already positioned at the correct record
  if Verse = nil then Exit;
  FPersistence.WriteCurrentPairs(Verse.Pares);
end;

procedure TProjeto.SalvarTexto;
begin
  FCurrentVerseWorkspace.SaveTexts(FPersistence, FAVersiculo, FACamposTexto);
end;

procedure TProjeto.SalvarAssociacoes;
begin
  FCurrentVerseWorkspace.SaveAssociations(FSuggestionManager,
    FAVersiculo[tbOrigem], FAVersiculo[tbDestino]);
end;

procedure TProjeto.RemoverAssociacoes;
begin
  FCurrentVerseWorkspace.RemoveOldAssociations(FSuggestionManager,
    FAVersiculo[tbOrigem], FAVersiculo[tbDestino]);
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

procedure TProjeto.OnExportText(Sender: TVerse);
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
    FVerseRepository.First;
    while not FVerseRepository.EOF do
    begin
      lines.Add(FVerseRepository.CurrentText(FACamposTexto[text]));
      FVerseRepository.Next;
    end;
    FinishScrollingSession;
  end;
  lines.SaveToFile(destination);
  lines.Free;

  OpenDocument(ExtractFilePath(destination));
end;

function TProjeto.FindVerseType(Verse: TVerse; out VerseType: TTipoTextoBiblico): Boolean;
var
  t: TTipoTextoBiblico;
begin
  for t := Low(FAVersiculo) to High(FAVersiculo) do
    if FAVersiculo[t] = Verse then
    begin
      VerseType := t;
      Exit(True);
    end;

  Result := False;
end;

procedure TProjeto.OnVersePopup(Sender: TObject);
var
  PopupMenu: TPopupMenu;
  ClickedVerse: TVerse;
  VerseType: TTipoTextoBiblico;
  IsPrimaryVerse: Boolean;
  CanUseAssociationExample: Boolean;
  CanUseTranslationExample: Boolean;
begin
  if not (Sender is TPopupMenu) then
    Exit;

  PopupMenu := TPopupMenu(Sender);
  ClickedVerse := TVerse(PopupMenu.Tag);
  if not Assigned(ClickedVerse) or not FindVerseType(ClickedVerse, VerseType) then
    Exit;

  IsPrimaryVerse := VerseType = tbOrigem;
  CanUseAssociationExample := IsPrimaryVerse and Assigned(ClickedVerse.VersiculoPar) and ClickedVerse.HasAssociations;
  CanUseTranslationExample := IsPrimaryVerse and Assigned(ClickedVerse.VersiculoPar) and (Trim(ClickedVerse.VersiculoPar.TextoSimples) <> '');

  if Assigned(FUseAsExampleMenuItems[VerseType]) then
  begin
    FUseAsExampleMenuItems[VerseType].Visible := CanUseAssociationExample or CanUseTranslationExample;
    FUseAsAssociationExampleMenuItems[VerseType].Enabled := CanUseAssociationExample;
    FUseAsTranslationExampleMenuItems[VerseType].Enabled := CanUseTranslationExample;
  end;

  if Assigned(FCopyAssociationExampleMenuItems[VerseType]) then
    FCopyAssociationExampleMenuItems[VerseType].Visible := ClickedVerse.HasAssociations;
end;

procedure TProjeto.OnUseAssociationAsExample(Sender: TObject);
begin
  IncludeCurrentVerseAsFlowExample('association');
end;

procedure TProjeto.OnUseTranslationAsExample(Sender: TObject);
begin
  IncludeCurrentVerseAsFlowExample('translation');
end;

procedure TProjeto.OnCopyAssociationExample(Sender: TObject);
var
  example: string;
  originalVerse: TVerse;
  destinationVerse: TVerse;
begin
  originalVerse := FAVersiculo[tbOrigem];
  destinationVerse := FAVersiculo[tbDestino];

  if (not Assigned(originalVerse)) or (not Assigned(destinationVerse)) then
    Exit;

  example := Format('Original: "%s"'#13#10'Tradução: "%s"'#13#10'Resposta esperada: %s',
    [originalVerse.TextoSimples, destinationVerse.TextoSimples, originalVerse.GetAssociationsJSON]);
  Clipboard.AsText := example;
end;

procedure TProjeto.OnSwitchDestinationAndReference1Texts(Sender: TObject);
var
  destinationText: string;
begin
  if (not Assigned(FAVersiculo[tbDestino])) or (not Assigned(FAVersiculo[tbConsulta1])) then
    Exit;

  destinationText := FAVersiculo[tbDestino].XML;
  FAVersiculo[tbDestino].AlterarTexto(FAVersiculo[tbConsulta1].XML);
  FAVersiculo[tbConsulta1].Texto := '(KJF original) ' + destinationText;
end;

procedure TProjeto.AttachProjectVersePopupMenu(texto: TTipoTextoBiblico);
var
  PopupMenu: TPopupMenu;
begin
  if not Assigned(FAVersiculo[texto]) or not Assigned(FAVersiculo[texto].VersePopupMenu) then
    Exit;

  PopupMenu := FAVersiculo[texto].VersePopupMenu;
  PopupMenu.OnPopup := @OnVersePopup;

  FUseAsExampleMenuItems[texto] := TMenuItem.Create(PopupMenu);
  FUseAsExampleMenuItems[texto].Caption := SUseVerseAsExample;
  PopupMenu.Items.Add(FUseAsExampleMenuItems[texto]);

  FUseAsAssociationExampleMenuItems[texto] := TMenuItem.Create(PopupMenu);
  FUseAsAssociationExampleMenuItems[texto].Caption := SUseVerseAsAssociationExample;
  FUseAsAssociationExampleMenuItems[texto].OnClick := @OnUseAssociationAsExample;
  FUseAsExampleMenuItems[texto].Add(FUseAsAssociationExampleMenuItems[texto]);

  FUseAsTranslationExampleMenuItems[texto] := TMenuItem.Create(PopupMenu);
  FUseAsTranslationExampleMenuItems[texto].Caption := SUseVerseAsTranslationExample;
  FUseAsTranslationExampleMenuItems[texto].OnClick := @OnUseTranslationAsExample;
  FUseAsExampleMenuItems[texto].Add(FUseAsTranslationExampleMenuItems[texto]);

  {$IFDEF DEBUG}
  PopupMenu.Items.AddSeparator;

  FCopyAssociationExampleMenuItems[texto] := TMenuItem.Create(PopupMenu);
  FCopyAssociationExampleMenuItems[texto].Caption := SCopyAssociationExample;
  FCopyAssociationExampleMenuItems[texto].OnClick := @OnCopyAssociationExample;
  PopupMenu.Items.Add(FCopyAssociationExampleMenuItems[texto]);

  FSwitchDestinationTextMenuItems[texto] := TMenuItem.Create(PopupMenu);
  FSwitchDestinationTextMenuItems[texto].Caption := SSwitchDestinationAndReference1Texts;
  FSwitchDestinationTextMenuItems[texto].OnClick := @OnSwitchDestinationAndReference1Texts;
  PopupMenu.Items.Add(FSwitchDestinationTextMenuItems[texto]);
  {$ENDIF}
end;

procedure TProjeto.OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
begin
  if not FScrollEventsEnabled or not assigned(FTblPares) or FClosing or not assigned(Node) then
    exit;
  GoToReference(FBibleTreeCoordinator.NodeToReferenceID(FEscopo, Node));
end;

procedure TProjeto.OnAlterarTextoVersiculo(Sender: TMemoVersiculo);
var
  editedIndex: Integer;
  e: TOnNovoVersiculoEvent;
begin
  if ContainsText(Sender.Texto, #13) or ContainsText(Sender.Texto, #10) then
  begin
    MessageDlg(SLineBreaksInTextTitle, SLineBreaksInText, mtError, [mbOK], 0);
    exit;
  end;

  editedIndex := FCurrentVerseWorkspace.ApplyEditedVerseText(FAVersiculo,
    Sender.Versiculo, Sender.Texto);
  if editedIndex = Ord(tbDestino) then
    for e in FOnNovoVersiculo do
      e(self);

  //MessageDlg('Versículo modificado', format('Versiculo modificado: %s', [Sender.Texto]), mtError, [mbOK], 0);
end;

procedure TProjeto.OnExibirDefinicao(Sender: TObject);
var
  StrongEntries: TDictionaryEntryArray;
  MorphologyEntries: TDictionaryEntryArray;
  EntryIndex: Integer;
  v: TTipoTextoBiblico;
  point: TPoint;
  s: TSyntagm;
  rtf: string;
begin
  if frmDictionaryPopup.Visible then
    exit;

  s := TSyntagm(TTimer(Sender).Tag);
  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    if (FAVersiculo[v] = s.VerseRef) then
    begin
      FDictionaryManager.CollectPopupEntries(s, Ord(v), StrongEntries, MorphologyEntries);

      for EntryIndex := Low(MorphologyEntries) to High(MorphologyEntries) do
        FrmDictionaryPopup.AdicionarMorfo(MorphologyEntries[EntryIndex].Key,
          MorphologyEntries[EntryIndex].Definition);

      for EntryIndex := Low(StrongEntries) to High(StrongEntries) do
        FrmDictionaryPopup.AdicionarStrong(StrongEntries[EntryIndex].Key,
          StrongEntries[EntryIndex].Definition);

      if v = tbOrigem then
      begin
        rtf := TDictionaryManager.FormatRTF(FSuggestionManager.GetTranslationAlternatives(s));
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

  FMemoVersiculo.Ativar(TScrollBox(Sender), TVerse(TScrollBox(Sender).Tag));
end;

procedure TProjeto.OnBibleTreePopupItemClick(Sender: TObject);

  function SameReference(a, b: TReference): boolean;
  begin
    result := (a.Book = b.Book) and (a.BookID = b.BookID) and (a.Chapter = b.Chapter) and (a.Verse = b.Verse);
  end;

  procedure SetNodeStatus(node: TTreeNode; status: Integer);
  begin
    GoToReference(FBibleTreeCoordinator.NodeToReferenceID(FEscopo, node));
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

  RefreshUI;
end;

procedure TProjeto.OnBibleTreeContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  i, j: Integer;
  m, sub: TMenuItem;
  refStr: string;
  baseCaption: string;
begin
  FBibleTreePointedNode := (Sender as TTreeView).GetNodeAt(MousePos.X, MousePos.Y);
  // Allow context menu on leaf (verse) nodes as well — only block when project not active or no node
  if (not Ativo) or (FBibleTreePointedNode = nil) then
    Handled := True;
  // Atualiza habilitação do item 'Cancelar processamento IA' (Tag 9001)
  if Assigned(FArvore) and Assigned(FArvore.PopupMenu) then
  begin
    // update cancel enabled state
    for i := 0 to FArvore.PopupMenu.Items.Count-1 do
    begin
      m := FArvore.PopupMenu.Items[i];
      for j := 0 to m.Count-1 do
      begin
        sub := m.Items[j];
        if sub.Tag = 9001 then
        begin
          if Assigned(FAIManager) and (not FAIManager.IsIdle) then
            sub.Enabled := True
          else
            sub.Enabled := False;
        end;
      end;
    end;

    // Find IA submenu and update its children to include the reference and scope
    if FBibleTreePointedNode <> nil then
    begin
      // Build reference string depending on level
        if FBibleTreePointedNode.Level = 0 then
          refStr := FBibleTreePointedNode.Text
        else if FBibleTreePointedNode.Level = 1 then
          refStr := Format('%s %s', [FBibleTreePointedNode.Parent.Text, FBibleTreePointedNode.Text])
        else // level = 2
          refStr := Format('%s %s:%s', [FBibleTreePointedNode.Parent.Parent.Text, FBibleTreePointedNode.Parent.Text, FBibleTreePointedNode.Text]);

      // locate IA menu
      for i := 0 to FArvore.PopupMenu.Items.Count-1 do
      begin
        m := FArvore.PopupMenu.Items[i];
        for j := 0 to m.Count-1 do
        begin
          sub := m.Items[j];
          // Status items have Tag in 0..3
          if (sub.Tag >= 0) and (sub.Tag <= 3) then
          begin
            // update parent (status) caption to include reference
            m.Caption := Format(SMarkAsFmt, [refStr]);
          end;
          // Identify AI submenu children by their tags (association/translation starter)
          if (sub.Tag = 1001) or (sub.Tag = 1011) then
          begin
            // sub.Tag indicates action type (1=Association,11=Translation); scope is derived from node level
            // Use resource strings for AI child captions
            if sub.Tag = 1001 then
              baseCaption := SAssociate
            else
              baseCaption := STranslate;
            sub.Caption := Format('%s %s', [baseCaption, refStr]);
            // visibility: show for book/chapter/verse depending on node level
            sub.Visible := True;
            // set parent caption to include reference
            m.Caption := Format(SApplyAIToFmt, [refStr]);
          end;
        end;
      end;
    end;
  end;
end;

function TProjeto.AtribuirDicStrong(dic: string; t: TTipoTextoBiblico): boolean;
var
  ValidationResult: TDictionaryValidationResult;
begin
  result := false;

  FAVersiculo[t].OnMouseEnter := @SintagmaOnMouseEnter;
  FAVersiculo[t].OnMouseLeave := @SintagmaOnMouseLeave;

  if dic = '' then
    exit;

  if not FileExists(dic) then
  begin
    MessageDlg(SStrongDictionary, format(SDictionaryDoesntExist, [FExpand(dic)]), mtError, [mbOK], 0);
    exit;
  end;

  ValidationResult := TDictionaryManager.ValidateDictionaryModule(dic, dmtStrong);
  case ValidationResult of
    dvrMissingStrongSupport:
      begin
        MessageDlg(SStrongDictionary, format(SInvalidStrongsDictionary, [FExpand(dic)]), mtError, [mbOK], 0);
        Exit;
      end;
    dvrEncrypted:
      begin
        MessageDlg(SEncryptedDictionary, format(SInvalidEncryptedDictionary, [FExpand(dic)]), mtError, [mbOK], 0);
        Exit;
      end;
    dvrCompressed:
      begin
        MessageDlg(SCompressedDictionary, format(SInvalidCompressedDictionary, [FExpand(dic)]), mtError, [mbOK], 0);
        Exit;
      end;
    dvrNonRTF:
      begin
        MessageDlg(SNonRTFDictionary, format(SInvalidNonRTFDictionary, [FExpand(dic)]), mtError, [mbOK], 0);
        Exit;
      end;
  end;

  if ValidationResult <> dvrValid then
    exit;

  Result := FDictionaryManager.AssignStrongDictionary(dic, Ord(t));
end;

function TProjeto.AtribuirDicMorfo(dic: string; t: TTipoTextoBiblico): boolean;
var
  ValidationResult: TDictionaryValidationResult;
begin
  result := false;

  if dic = '' then
    exit;

  if not FileExists(dic) then
  begin
    MessageDlg(SMorphologyDictionary, format(SDictionaryDoesntExist, [FExpand(dic)]), mtError, [mbOK], 0);
    exit;
  end;

  ValidationResult := TDictionaryManager.ValidateDictionaryModule(dic, dmtMorphology);
  case ValidationResult of
    dvrMissingMorphologySupport:
      begin
        MessageDlg(SMorphologyDictionary, format(SInvalidMorphologyDictionary, [FExpand(dic)]), mtError, [mbOK], 0);
        Exit;
      end;
    dvrEncrypted:
      begin
        MessageDlg(SEncryptedDictionary, format(SInvalidEncryptedDictionary, [FExpand(dic)]), mtError, [mbOK], 0);
        Exit;
      end;
    dvrCompressed:
      begin
        MessageDlg(SCompressedDictionary, format(SInvalidCompressedDictionary, [FExpand(dic)]), mtError, [mbOK], 0);
        Exit;
      end;
    dvrNonRTF:
      begin
        MessageDlg(SNonRTFDictionary, format(SInvalidNonRTFDictionary, [FExpand(dic)]), mtError, [mbOK], 0);
        Exit;
      end;
  end;

  if ValidationResult <> dvrValid then
    exit;

  Result := FDictionaryManager.AssignMorphologyDictionary(dic, Ord(t));
  FAVersiculo[t].OnMouseEnter := @SintagmaOnMouseEnter;
  FAVersiculo[t].OnMouseLeave := @SintagmaOnMouseLeave;
end;

procedure TProjeto.AtualizarArvore;
begin
  if not assigned(FArvore) then exit;
  FBibleTreeCoordinator.RecomputeAllStatuses(FEscopo,
    TProjectVerseStatusCursor.Create(Self));
end;

procedure TProjeto.AtualizarArvore(id: string);
var
  i: smallint;
  al, ac: array[0..3] of smallint;
  nl, nc, nv, n: TTreeNode;
  b: boolean;
  totalChildren: integer;
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
    nc := nc.GetNextSibling;
  end;

  nv := nc.GetFirstChild;
  for i:=Verse-1 downto 1 do
  begin
    nv := nv.GetNextSibling;
  end;

  { atualizando versículo }
  if nv.ImageIndex <> Situacao then
  begin
    nv.ImageIndex := Situacao; nv.SelectedIndex := nv.ImageIndex + 4;
  end;
  
  { atualizando capítulo - sempre recontar todos os versículos }
  ac[0] := 0; ac[1] := 0; ac[2] := 0; ac[3] := 0;
  totalChildren := 0;
  n := nc.GetFirstChild;
  while assigned(n) do
  begin
    if (n.ImageIndex >= 0) and (n.ImageIndex <= 3) then
      inc(ac[n.ImageIndex]);
    inc(totalChildren);
    n := n.GetNextSibling;
  end;

  b := false;
  if totalChildren > 0 then
    for i:=0 to 3 do // todos os versículos estão na mesma situação?
    begin
      if ac[i] = totalChildren then
      begin
        nc.ImageIndex := i; nc.SelectedIndex := nc.ImageIndex + 4;
        b := true;
        break;
      end;
    end;
  if not b then // várias situações diferentes
  begin
    if ac[2] > 0 then
      nc.ImageIndex := 2  // vermelho: tem algo para rever
    else
      nc.ImageIndex := 1; // amarelo: status mistos mas nada para rever
    nc.SelectedIndex := nc.ImageIndex + 4;
  end;
  
  { atualizando livro - sempre recontar todos os capítulos }
  al[0] := 0; al[1] := 0; al[2] := 0; al[3] := 0;
  totalChildren := 0;
  n := nl.GetFirstChild;
  while assigned(n) do
  begin
    if (n.ImageIndex >= 0) and (n.ImageIndex <= 3) then
      inc(al[n.ImageIndex]);
    inc(totalChildren);
    n := n.GetNextSibling;
  end;

  b := false;
  if totalChildren > 0 then
    for i:=0 to 3 do // todos os capítulos estão na mesma situação?
    begin
      if al[i] = totalChildren then
      begin
        nl.ImageIndex := i; nl.SelectedIndex := nl.ImageIndex + 4;
        b := true;
        break;
      end;
    end;
  if not b then // várias situações diferentes
  begin
    if al[2] > 0 then
      nl.ImageIndex := 2  // vermelho: tem algo para rever
    else
      nl.ImageIndex := 1; // amarelo: status mistos mas nada para rever
    nl.SelectedIndex := nl.ImageIndex + 4;
  end;
end;

constructor TProjeto.Create;
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    FAVersiculo[v]   := nil;
    FACamposTexto[v] := 0;
  end;

  FOnNovoVersiculo    := TOnNovoVersiculoEvents.Create;
  FOnAlterarVersiculo := nil;
  FSuggestionManager  := nil;
  FAIManager          := nil;
  FAIProcessingNodes  := TAINodeInfoList.Create(True); // owns objects
  FAIBatchCompletionShown := False;
  FCurrentVerseWorkspace := TCurrentVerseWorkspace.Create;
  FCurrentVerseWorkspace.InitializeVerseRuntime(FAVersiculo, FACamposTexto);
  FPersistence        := nil;
  FConfig             := TProjectConfig.Create(nil);
  FVerseRepository    := TProjectVerseRepository.Create(nil);
  FVerseRenderBuffer  := nil;
  FProjectExporter    := TProjectExporter.Create(etNone, nil, nil);
  FDictionaryManager  := TDictionaryManager.Create;
  FPatchApplier       := TProjectPatchApplier.Create(nil);
  FBibleTreeCoordinator := TBibleTreeCoordinator.Create(nil);
  FTblPares           := nil;
  FTblInfo            := nil;
  FEscopo             := etNone;
  FClosing            := false;
  FVerseStrongsCountMode := scNone;
  FLastLLMUsage.PromptTokens := 0;
  FLastLLMUsage.CompletionTokens := 0;
  FLastLLMUsage.TotalTokens := 0;
  FLastLLMCallCompleted := False;

  FTemporizador          := TTimer.Create(nil);
  FTemporizador.Enabled  := false;
  FTemporizador.Interval := 300;
  FTemporizador.OnTimer  := @OnExibirDefinicao;

  FMemoVersiculo := TMemoVersiculo.Create;
  FMemoVersiculo.QuandoModificarVersiculo := @OnAlterarTextoVersiculo;
end;

constructor TProjeto.Create(paineis: array of TScrollbox; navegador: TTreeView;
  rsituacao: TRadioGroup; rcomentarios: TControl);
var
  v: integer;
begin
  Create;

  for v:=low(paineis) to high(paineis) do
    if (v <= Ord(tbConsulta2)) and assigned(paineis[v]) then
      NovoObjetoVersiculo(paineis[v], TTipoTextoBiblico(v));

  Arvore              := navegador;
  FBibleTreeCoordinator.Attach(FArvore);
  FRadioGroupSituacao := rsituacao;
  FMemoComentarios    := rcomentarios;
end;

destructor TProjeto.Destroy;
begin
  FCurrentVerseWorkspace.DestroyVerseViews(FAVersiculo);

  if Assigned(FAIProcessingNodes) then
    FreeAndNil(FAIProcessingNodes);

  FreeAndNil(FCurrentVerseWorkspace);
  FreeAndNil(FBibleTreeCoordinator);
  FreeAndNil(FPatchApplier);
  FreeAndNil(FProjectExporter);
  FreeAndNil(FDictionaryManager);
  FreeAndNil(FVerseRenderBuffer);
  FreeAndNil(FVerseRepository);
  FreeAndNil(FConfig);
  FreeAndNil(FPersistence);

  FOnNovoVersiculo.Free;
  FSuggestionManager.Free;
  FTemporizador.Free;
  FMemoVersiculo.Destroy;
  inherited Destroy;
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

  FreeAndNil(FPersistence);
  FPersistence := TProjectPersistence.Create;
  FPersistence.Open(Nome);
  if not Assigned(FConfig) then
    FConfig := TProjectConfig.Create(FPersistence)
  else
    FConfig.Attach(FPersistence);
  FTblInfo := FPersistence.InfoTable;

  FSuggestionManager := TSuggestionManager.Create(Nome);

  FTblPares := FPersistence.PairsTable;

  f := TFont.Create;
  for t:=tbOrigem to tbConsulta2 do
  begin
    if not assigned(FAVersiculo[t]) then
       continue;

    FAVersiculo[t].TokenizerVersion := TokenizerVersion;

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

    { definindo dicionários }
    AtribuirDicStrong(ObterInfo(format('dicstrong%d', [t])), t);
    AtribuirDicMorfo(ObterInfo(format('dicmorfo%d', [t])), t);
  end;
  f.free;

  FCurrentVerseWorkspace.ApplyTokenizerVersion(FAVersiculo, TokenizerVersion);
  FCurrentVerseWorkspace.ApplyBoldStrongs(FAVersiculo, FPalavrasComStrongEmNegrito);
  FCurrentVerseWorkspace.ApplyStrongsCountMode(FAVersiculo, FVerseStrongsCountMode);

  FreeAndNil(FVerseRenderBuffer);
  FVerseRenderBuffer := TVerseRenderBuffer.Create();
  FVerseRenderBuffer.SetTokenizerVersion(TokenizerVersion);
  if Assigned(FPatchApplier) then
    FPatchApplier.Attach(FPersistence);

  FACamposTexto[tbOrigem]    := FTblPares.FindField('pare_texto_origem').Index;
  FACamposTexto[tbDestino]   := FTblPares.FindField('pare_texto_destino').Index;
  FACamposTexto[tbConsulta1] := FTblPares.FindField('pare_texto_consulta1').Index;
  FACamposTexto[tbConsulta2] := FTblPares.FindField('pare_texto_consulta2').Index;
  FVerseRepository.Attach(FPersistence, FACamposTexto[tbOrigem], FACamposTexto[tbDestino]);

  if FEscopo = etNone then
    FEscopo := IdentificarEscopo;

  if Assigned(FProjectExporter) then
    FProjectExporter.Attach(FEscopo, FVerseRepository, FVerseRenderBuffer);

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
  end;

  FTblPares.BeforeScroll := @PreRolagemVersiculo;
  FTblPares.AfterScroll  := @PosRolagemVersiculo;
  FArvore.OnChange       := @OnMudancaVersiculo;
  FArvore.OnContextPopup := @OnBibleTreeContextPopup;
  FArvore.PopupMenu := CreateBibleTreeContextMenu;

  // set tokenizer version
  TTokenizerFactory.PreferredVersion := TokenizerVersion;

  ScrollEventsEnabled := true;
  GoToReference(FConfig.ReadMarker);

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

  if _Commit then
  begin
    FConfig.WriteMarker(ID);
    Commit;
  end
  else
    Rollback;

  // always saving font sizes
  for t:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[t]) then
      AtribuirInfo(format('fonte%d.tamanho', [t]), IntToStr(FAVersiculo[t].Fonte.Size));
  Commit;

  if Assigned(FConfig) then
    FConfig.Attach(nil);
  if Assigned(FVerseRepository) then
    FVerseRepository.Attach(nil);
  if Assigned(FPatchApplier) then
    FPatchApplier.Attach(nil);
  if Assigned(FCurrentVerseWorkspace) then
    FCurrentVerseWorkspace.Clear;
  FreeAndNil(FPersistence);
  FTblPares := nil;
  FTblInfo := nil;

  FMemoVersiculo.Desativar;

  FreeAndNil(FVerseRenderBuffer);

  if assigned(FMemoComentarios) then
  begin
    FMemoComentarios.Enabled := false;
    if FMemoComentarios is TCommentsView then
      TCommentsView(FMemoComentarios).Text := ''
    else if FMemoComentarios is TMemo then
      TMemo(FMemoComentarios).Text := '';
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
  if assigned(FSuggestionManager) then
    FSuggestionManager.Commit;

  if Assigned(FPersistence) then
    FPersistence.Commit;
end;

procedure TProjeto.Rollback;
begin
  if Assigned(FPersistence) then
    FPersistence.Rollback;
  FSuggestionManager.Rollback;
end;

procedure TProjeto.GoToReference(reference: string);
begin
  if reference.IsEmpty then
    reference := format('%d,1,1', [OffsetLivros[FEscopo] + 1]);

  if ID = reference then
  begin // if FTblPares.Locate is not called, PosRolagemVersiculo won't either
     UpdateReference(reference);
  end
  else
  begin
    FPersistence.LocateReference(reference);
    if ID <> reference then
      raise Exception.Create(Format('Reference not found in project [%s]', [reference]));
  end;
end;

procedure TProjeto.GoToReference(reference: TReference);
begin
  GoToReference(format('%d,%d,%d', [reference.BookID, reference.Chapter, reference.Verse]));
end;

procedure TProjeto.VersiculoSeguinte;
begin
  if not FPersistence.EOF then
    FPersistence.Next;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) and (not HasSituacaoCustomizada) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoAnterior;
begin
  if not FPersistence.BOF then
    FPersistence.Prior;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) and (not HasSituacaoCustomizada) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoInicial;
begin
  if not FPersistence.BOF then
    FPersistence.First;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) and (not HasSituacaoCustomizada) then
    SugerirAssociacao;
end;

procedure TProjeto.VersiculoFinal;
begin
  if not FPersistence.EOF then
    FPersistence.Last;
  if not FExportando and SugerirAssociacaoAutomaticamente and (FRadioGroupSituacao.ItemIndex = 0) and (not HasSituacaoCustomizada) then
    SugerirAssociacao;
end;

procedure TProjeto.RefreshUI;
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
  FConfig.WriteMarker(ID);
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

procedure TProjeto.SugerirAssociacaoEstatistica;
begin
  FSuggestionManager.SuggestAssociations(FAVersiculo[tbOrigem]);
  FAVersiculo[tbOrigem].Renderizar;
  FAVersiculo[tbDestino].Renderizar;

  if HasSituacaoCustomizada then exit;

  if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AndamentoAssociacao > 0) then
    FRadioGroupSituacao.ItemIndex := 1; // associando
end;

procedure TProjeto.SugerirAssociacao;
begin
  case FSuggestionMode of
    smStatistical: SugerirAssociacaoEstatistica;
    smAI:
      begin
        // Note: AI Association is blocking and should ideally show progress,
        // but for auto-suggestion we rely on blocking behavior.
        // It might be worth checking if AI is configured before trying.
        try
          SugerirAssociacaoAI;
        except
          on E: Exception do
            // Fallback or just log? User will see message box from SugerirAssociacaoAI catch block.
        end;
      end;
  end;
end;

procedure TProjeto.SugerirAssociacaoAI;
var
  flow: IAIVerseFlow;
  flowConfig: IAIFlowConfig;
  client: TBaseLLMClient;
  msgs: TLLMMessageArray;
  resp: TLLMResponse;
  parsed: TAIFlowResult;
  v: TVerse;
  OldCursor: TCursor;
begin
  FLastLLMUsage.PromptTokens := 0;
  FLastLLMUsage.CompletionTokens := 0;
  FLastLLMUsage.TotalTokens := 0;
  FLastLLMCallCompleted := False;

  if (ObterInfo(AI_COMMON_CONFIG.Model).IsEmpty) or
     (ObterInfo(AI_COMMON_CONFIG.ApiKey).IsEmpty) then
  begin
    ShowMessage('AI Associator not configured.');
    Exit;
  end;

  v := FAVersiculo[tbOrigem];
  if not Assigned(v) or not Assigned(v.VersiculoPar) then
  begin
    ShowMessage('No verse to associate.');
    Exit;
  end;

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    try
      flowConfig := GetFlowConfigById('association');
      flow := flowConfig.CreateFlowInstance(CreateAIFlowContext(FConfig));

      client := llmclient.CreateBackend(ObterInfo(AI_COMMON_CONFIG.Model), ObterInfo(AI_COMMON_CONFIG.ApiKey));
      try
        msgs := flow.BuildMessages(v);
        resp := client.AskChatCompletions(msgs);
        FLastLLMUsage := resp.Usage;
        FLastLLMCallCompleted := True;

        AIDebugLog('[AI Single] Response received, parsing...');
        parsed := flow.ParseResponse(resp.Content, v);
        try
          AIDebugLog('[AI Single] Applying result...');
          flow.ApplyResult(parsed, v);
          SalvarParesVersiculo(v);
          AIDebugLog('[AI Single] Result applied and saved.');
        finally
          parsed.Free;
        end;
      finally
        client.Free;
      end;
    except
      on E: Exception do
      begin
        AIDebugLog('[AI Single] Error: ' + E.Message);
        ShowMessage('AI association failed: ' + E.Message);
      end;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TProjeto.StartAIAssociationBatch(NodesVerses: TAIBatchWorkItemList; MaxParallel: Integer);
var
  flow: IAIVerseFlow;
  batchPrep: TAIBatchPreparation;
  orchestrator: TAIBatchOrchestrator;
begin
  if (NodesVerses = nil) or (NodesVerses.Count = 0) then Exit;
  if (ObterInfo(AI_COMMON_CONFIG.Model).IsEmpty) or (ObterInfo(AI_COMMON_CONFIG.ApiKey).IsEmpty) then
  begin
    ShowMessage('AI associator not configured.');
    Exit;
  end;

  // Reset completion flag
  FAIBatchCompletionShown := False;

  orchestrator := TAIBatchOrchestrator.Create(
    FConfig,
    TAIBatchContextAdapter.Create(
      @ResetAIBatchManager,
      @FindTreeNodeByReference,
      @MarkNodeAsAIPending,
      @HandleAutoSave,
      @GetVerseReferenceString
    )
  );
  try
    batchPrep := orchestrator.PrepareBatch('association', NodesVerses, MaxParallel);
  finally
    orchestrator.Free;
  end;

  flow := batchPrep.Flow;
  FAIManager := batchPrep.Manager;
  if (flow = nil) or not Assigned(FAIManager) then
    Exit;

  FAIManager.StartBatch(flow, NodesVerses);
end;

procedure TProjeto.StartAITranslationBatch(NodesVerses: TAIBatchWorkItemList; MaxParallel: Integer);
var
  flow: IAIVerseFlow;
  batchPrep: TAIBatchPreparation;
  orchestrator: TAIBatchOrchestrator;
begin
  if (NodesVerses = nil) or (NodesVerses.Count = 0) then Exit;

  // Reset completion flag
  FAIBatchCompletionShown := False;

  orchestrator := TAIBatchOrchestrator.Create(
    FConfig,
    TAIBatchContextAdapter.Create(
      @ResetAIBatchManager,
      @FindTreeNodeByReference,
      @MarkNodeAsAIPending,
      @HandleAutoSave,
      @GetVerseReferenceString
    )
  );
  try
    batchPrep := orchestrator.PrepareBatch('translation', NodesVerses, MaxParallel);
  finally
    orchestrator.Free;
  end;

  flow := batchPrep.Flow;
  FAIManager := batchPrep.Manager;
  if (flow = nil) or not Assigned(FAIManager) then
    Exit;

  FAIManager.StartBatch(flow, NodesVerses);
end;

procedure TProjeto.HandleAutoSave(Sender: TObject);
begin
  // Called from main thread when autosave interval is reached
  Salvar;
end;

function TProjeto.GetVerseReferenceString(Verse: TVerse): string;
begin
  // Return formatted reference for the current verse
  if Verse = nil then
  begin
    Result := 'Unknown';
    Exit;
  end;

  // Use current project reference (assumes verse is from this project)
  Result := Format('%s %d:%d', [Book, Chapter, Self.Verse]);
end;

// REMOVED: PumpAIAssociation - obsolete since AI processing moved to modal
// AI processing is now pumped by TFrmAIProcessing.PumpProcessing
// Modal handles completion messages and manager lifecycle

// REMOVED: GetAIBatchProgress - obsolete since AI processing moved to modal
// Progress is now tracked internally by TFrmAIProcessing

procedure TProjeto.RecriarBaseSugestoes;
begin
  RecriarBaseSugestoes(nil);
end;

procedure TProjeto.RecriarBaseSugestoes(pb: TProgressBar);
var
  verseData: TVerseData;
  pares: TStringList;
  p: smallint;
  corruptionDetails: string;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  FSuggestionManager.ClearAssociationDatabase;

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
      FVerseRepository.First;
      while not FVerseRepository.EOF do
      begin
        verseData := FVerseRepository.ReadCurrentVerseData;
        if verseData.Associations <> '' then
        begin
          if FVerseRenderBuffer.TryExtractPairsList(
              verseData.OriginalText,
              verseData.TranslatedText,
              verseData.Associations,
              plMetaData,
              pares,
              corruptionDetails) then
          begin
            for p:=0 to pares.Count-1 do
              if (p mod 2) = 0 then // pares estão alternados na lista
                FSuggestionManager.AddPair(pares.Strings[p], pares.Strings[p+1]);

            pares.Free;
          end
          else
            ShowMessage('Error: ' + SCorruptedData + #13#10#13#10 + GetID + #13#10 + corruptionDetails);
        end;
        if assigned(pb) then
        begin
           pb.StepIt;
           if (pb.Position mod 50) = 0 then
             Application.ProcessMessages;
        end;
        FVerseRepository.Next;
      end;
    except
      on E: Exception do
         ShowMessage('Error: ' + E.ClassName + #13#10 + E.Message);
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

    for i:=QLinhas[moduleScope] to modulo.Count-1 do
    begin
      line := modulo[i];
      { eliminando comentários }
      line := reComments.Replace(line, '');

      if reVazio.IsMatch(line) then
        continue;

      if reCharset.IsMatch(line) then
      begin
        if MessageDlg(arquivo, SPotentialNonUTF8ModuleWarning, mtWarning, [mbIgnore, mbAbort], 0) = mrAbort then
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
  FCurrentVerseWorkspace.CreateVerseView(owner, Ord(texto), Ord(tbOrigem),
    Ord(tbDestino), FAVersiculo, @OnDblClickVersiculo, @SintagmaOnClick,
    @OnExportText);
  if Assigned(FAVersiculo[texto]) then
  begin
    FAVersiculo[texto].TokenizerVersion := TokenizerVersion;
    AttachProjectVersePopupMenu(texto);
  end;
end;

procedure TProjeto.ExportarTextoDestinoComStrongs(arquivo: string; opcoes: TOpcoesExportacao);
begin
  ExportarTextoDestinoComStrongs(arquivo, nil, opcoes);
end;

procedure TProjeto.ExportarTextoDestinoComStrongs(arquivo: string;
  pb: TProgressBar; opcoes: TOpcoesExportacao);
var
  Options: TDestinationTextExportOptions;
begin
  if not assigned(FAVersiculo[tbOrigem]) or not assigned(FAVersiculo[tbDestino]) then
    exit;

  try
    FExportando := true;

    Options.ExportMorphology := oeExportarMorfologia in opcoes;
    Options.ExportNAAsItalics := oeExportarNAComoItalicos in opcoes;
    Options.MarkReusedStrongs := oeStrongsReutilizados in opcoes;
    Options.MarkUntranslatedStrongs := oeStrongsNaoTraduzidos in opcoes;
    Options.ExportComments := oeExportarComentarios in opcoes;
    Options.Properties := ResgatarInfo('propriedades.destino');
    Options.ErrorDialogTitle := SError;
    Options.CorruptedDataMessage := SCorruptedData;
    Options.ExportToFileErrorMessage := SExportToFileError;
    Options.ErrorReportFileName := 'export-errors.txt';

    FProjectExporter.ExportDestinationTextWithStrongs(arquivo, pb, Options,
      @StartScrollingSession, @FinishScrollingSession, @GetID);
  finally
    FExportando := false;
    PosRolagemVersiculo(nil);
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
  Options: TInterlinearTextExportOptions;
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
    FExportando := true;

    Options.Properties := ResgatarInfo('propriedades.origem');
    Options.ErrorDialogTitle := SError;
    Options.CorruptedDataMessage := SCorruptedData;
    Options.ExportToFileErrorMessage := SExportToFileError;
    Options.VerseRuleReplaceErrorMessage := SInterlinearVerseRuleReplaceError;

    FProjectExporter.ExportInterlinearText(arquivo, pb, rulesRx, replaceTo,
      Options, @StartScrollingSession, @FinishScrollingSession, @GetID);
  finally
    rulesRx.Free;
    replaceTo.Free;
    FExportando := false;
    PosRolagemVersiculo(nil);
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
  Options: TConcordanceExportOptions;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  try
    FExportando := true;

    Options.Detailed := oeConcordDetalhada in opcoes;
    Options.Abbreviation := abbreviation;
    Options.LanguageId := SLanguageId;
    Options.AnalyticalCaption := SAnalyticalConcordance;
    Options.SyntheticCaption := SSyntheticConcordance;
    Options.ErrorDialogTitle := SError;
    Options.CorruptedDataMessage := SCorruptedData;

    FProjectExporter.ExportConcordance(arquivo, pb, Options,
      @StartScrollingSession, @FinishScrollingSession, @GetID);
  finally
    FExportando := false;
    PosRolagemVersiculo(nil);
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
  StartScrollingSession;
  try
    FPatchApplier.ClearTextFieldForAll(FACamposTexto[texto], FACamposTexto[tbOrigem],
      FACamposTexto[tbDestino], not HasSituacaoCustomizada);
  finally
    FinishScrollingSession;
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

function TProjeto.AtribuirDicStrong(dic: string; textos: STextosBiblicos): boolean;
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

function TProjeto.AtribuirDicMorfo(dic: string; textos: STextosBiblicos): boolean;
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
  FConfig.WriteString(info, valor);
end;

function TProjeto.ObterInfo(info: string): string;
begin
  Result := FConfig.ReadString(info);
end;

function TProjeto.GetFlowExamples(const FlowId: string): TAIFlowExampleArray;
begin
  Result := LoadFlowExamples(ObterInfo(GetFlowExamplesConfigKey(FlowId)));
end;

procedure TProjeto.SetFlowExamples(const FlowId: string; const Examples: TAIFlowExampleArray);
begin
  AtribuirInfo(GetFlowExamplesConfigKey(FlowId), SaveFlowExamples(Examples));
end;

function TProjeto.IncludeCurrentVerseAsFlowExample(const FlowId: string): Boolean;
var
  Examples: TAIFlowExampleArray;
  Example: TAIFlowExample;
  ExistingIndex: Integer;
  ReplaceExisting: Boolean;
begin
  Result := False;
  try
    Example := CreateFlowExample(FlowId);
    Examples := GetFlowExamples(FlowId);
    ExistingIndex := IndexOfFlowExampleByReference(Examples, Example.ReferenceId);

    if ExistingIndex >= 0 then
    begin
      ReplaceExisting := PromptReplaceExistingFlowExample(FlowId, Examples[ExistingIndex]);
      if not ReplaceExisting then
        Exit;
    end
    else
      ReplaceExisting := False;

    Examples := UpsertFlowExample(Examples, Example, ReplaceExisting);
    SetFlowExamples(FlowId, Examples);
    ShowMessage(SExampleSaved);
    Result := True;
  except
    on E: Exception do
      MessageDlg(SWarning, E.Message, mtWarning, [mbOK], 0);
  end;
end;

procedure TProjeto.RemoveFlowExample(const FlowId, ReferenceId: string);
var
  Examples: TAIFlowExampleArray;
begin
  Examples := GetFlowExamples(FlowId);
  Examples := ai_flow_config.RemoveFlowExample(Examples, ReferenceId);
  SetFlowExamples(FlowId, Examples);
end;

procedure TProjeto.ClearFlowExamples(const FlowId: string);
var
  Examples: TAIFlowExampleArray;
begin
  Examples := nil;
  SetFlowExamples(FlowId, Examples);
end;

function TProjeto.ObterTextoVersiculo(texto: TTipoTextoBiblico): string;
begin
  Result := FPersistence.CurrentText(FACamposTexto[texto]);
end;

function TProjeto.ObterTextoVersiculo(Referencia: string; texto: TTipoTextoBiblico): string;
begin
  result := SVerseAbsentFromProject;
  StartScrollingSession;
  try
    FVerseRepository.TryReadText(Referencia, FACamposTexto[texto], Result);
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
  varredorXML := TTokenizerFactory.CreateTokenizer(TokenizerVersion, FTblPares.Fields[FACamposTexto[texto]].AsString);
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
  result := FSuggestionManager.GetTranslationAlternatives(syntagm);
end;

function TProjeto.HasStoredAssociations: Boolean;
begin
  Result := False;

  if not Assigned(FPersistence) then
    Exit;

  StartScrollingSession;
  try
    FVerseRepository.First;
    while not FVerseRepository.EOF do
    begin
      if FVerseRepository.HasCurrentAssociations then
      begin
        Result := True;
        Exit;
      end;
      FVerseRepository.Next;
    end;
  finally
    FinishScrollingSession;
  end;
end;

procedure TProjeto.ClearStoredAssociations;
var
  shouldResetStatus: Boolean;
begin
  if not Assigned(FPersistence) then
    Exit;

  shouldResetStatus := not HasSituacaoCustomizada;
  FSuggestionManager.ClearAssociationDatabase;

  StartScrollingSession;
  try
    FVerseRepository.First;
    while not FVerseRepository.EOF do
    begin
      if FVerseRepository.HasCurrentAssociations then
        FVerseRepository.ClearCurrentAssociations(shouldResetStatus);
      FVerseRepository.Next;
    end;
  finally
    FinishScrollingSession;
    PosRolagemVersiculo(nil);
  end;
end;

procedure TProjeto.Translate;
var
  bk: integer;
  node: TTreeNode;
begin
  UpdateDefaultSituacaoDescricao;
  if not assigned(FArvore) then
    exit;

  node := FArvore.Items[0];
  for bk:=0 to QLivros[FEscopo]-1 do
  begin
    node.Text := NLivros[FEscopo][bk];
    node := node.GetNextSibling;
  end;

  if assigned(FArvore.PopupMenu) then
  begin
    FArvore.PopupMenu.Free;
    FArvore.PopupMenu := CreateBibleTreeContextMenu;
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
  FExportando := True;
end;

procedure TProjeto.FinishScrollingSession;
begin
  GoToReference(FMarker);
  ScrollEventsEnabled := true;
  FExportando := False;
end;

procedure TProjeto.RewindChapter;
begin
  if Verse <> 1 then
    GoToReference(Format('%d,%d,%d', [BookID, Chapter, 1]));
end;

end.
