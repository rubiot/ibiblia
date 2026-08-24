unit Project;

{$mode objfpc}{$H+}{$M+}

{TODO -cDesejável: Remover dependência com FormDictionaryPopup, criá-lo dinamicamente }
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
  VerseMemo, ONTTokenizer, Dialogs, dos, PCRE, LazLogger, Clipbrd,
  Syntagm, BibleScope, MySwordModule, fgl, PatchFile, LCLIntf, menus,
  CommentsView, ai_processing_manager, ai_processing_batch, ai_batch_work_item,
  ai_flows, llmclient, ai_flow_config, ai_debug, project_persistence, project_config,
  project_verse_repository, current_verse_workspace, verse_render_buffer,
  project_patch_applier, bible_tree_coordinator, ai_batch_orchestrator,
  project_database_bootstrap, status_catalog,
  dictionary_manager, project_exporter, association_memory_service,
  UnitPleaseWait, bible_book_names, translation_memory_model, ConcordanceExport,
  navigation_history, text_direction, text_edit_provenance,
  rf_note_scanner, association_highlight_projection;
const
  // AI processing icon indices for ImageListTreeView in formprincipal
  // ImageListTreeView has indices 0-7 for normal states (0-3) and selected states (4-7)
  // Indices 8-9 are reserved for AI processing states
  ImgIndexPending = 8;     // AI processing: queued/pending (to be added to ImageListTreeView)
  ImgIndexProcessing = 9;  // AI processing: in progress (to be added to ImageListTreeView)

type
  TProject = class;

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

  TChapterViewMode = (cvmChapterText, cvmTranslationMemory);

  TParagraphMode = (pmParagraph, pmNoParagraphs);
  TInterlinearMode = (imIntralinear, imInterlinear);
  TSuggestionMode = (smStatistical, smAI);

  TRegexList = specialize TFPGList<IRegex>;
  TIntegerList = specialize TFPGList<integer>;
  TIntegerArray = array of Integer;
  TPopupTrigger = (ptMouseHover, ptAltMouseHover, ptCtrlMouseHover);

  TTheWordModuleImportStatus = (
    twmisNotStarted,
    twmisSucceeded,
    twmisCancelled,
    twmisFailed
  );

  TTheWordModuleImportResult = record
    Status: TTheWordModuleImportStatus;
    FileName: string;
    TextType: TTipoTextoBiblico;
    ExpectedVerseCount: Integer;
    AvailableVerseCount: Integer;
    ImportedVerseCount: Integer;
    LastReference: string;
    ErrorMessage: string;
  end;

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

  TOnNovoVersiculoEvent = procedure (Sender: TProject) of object;
  TOnNovoVersiculoEvents = specialize TFPGList<TOnNovoVersiculoEvent>;
  TOnSintagmaClickEvent = procedure (Sender: TSyntagm) of object;
  TOnProjectVerseChangedEvent = procedure () of object;
  AVersiculo = array[tbOrigem..tbConsulta2] of TVerse;
  ACamposTexto = array[tbOrigem..tbConsulta2] of integer;
  STextosBiblicos = set of TTipoTextoBiblico;

  { TProject }

  TProject = class
  private
    FReference: TReference;
    FFileName: string;
    FVerseMemo: TVerseMemo;
    FAtivo: boolean;
    FMarker: string;
    FScrollEventsEnabled: boolean;
    FExportando: boolean;
    FOnAlterarVersiculo: TOnProjectVerseChangedEvent;
    FDisplayTags: boolean;
    FSugerirAssociacaoAuto: boolean;
    FPalavrasComStrongEmNegrito: boolean;
    FExibirDefComCtrl: boolean;
    FCurrentVerseWorkspace: TCurrentVerseWorkspace;
    FPersistence: TProjectPersistence;
    FConfig: TProjectConfig;
    FVerseRepository: TProjectVerseRepository;
    FVerseRenderBuffer: TVerseRenderBuffer;
    FAssociationMemory: TAssociationMemoryService;
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
    FRadioGroupSituacao: TComboBox;
    FStatusImages: TImageList;
    FStatusAggregateImageCache: TStringList;
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
    FNewVerseNotificationQueued: Boolean;
    FOnSintagmaClick: TOnSintagmaClickEvent;
    FClosing: boolean;
    FAutoSave: boolean;
    FIgnoreMissingPatchWarnings: Boolean;
    FAssociationMemoryRebuildRunning: Boolean;
    FAssociationMemoryRebuildFailed: Boolean;
    FAssociationMemoryLiveEditPending: Boolean;
    FAssociationMemorySpeculativeEditPending: Boolean;
    FAssociationMemoryProgressBar: TProgressBar;
    FNavigationHistory: TNavigationHistory;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    function GetCaminho: string;
    function GetChapterViewText: TTipoTextoBiblico;
    function GetChapterViewMode: TChapterViewMode;
    function GetComments: string;
    function GetID: string;
    function GetInterlinearMode: TInterlinearMode;
    function GetModificado: boolean;
    function GetFormattedReference: string;
    function GetPairs: string;
    function GetParagraphMode: TParagraphMode;
    function GetStatus: Integer;
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
    procedure SetChapterViewMode(AValue: TChapterViewMode);
    procedure SetComments(const AValue: string);
    procedure SetDisplayTags(AValue: boolean);
    procedure SetInterlinearMode(AValue: TInterlinearMode);
    procedure SetParagraphMode(AValue: TParagraphMode);
    procedure SetTokenizerVersion(AValue: Integer);
    procedure SetVerseStrongsCountMode(AValue: TStrongsCountMode);
    procedure SetOnAlterarVersiculo(const AValue: TOnProjectVerseChangedEvent);
    procedure SetOnSintagmaClick(const AValue: TOnSintagmaClickEvent);
    procedure SetPalavrasComStrongEmNegrito(AValue: boolean);
    procedure SetStatus(const AValue: Integer);
    function GetLegacyStatusDefinitions: TStatusDefinitions;
    function HasLegacyStatusDescriptions: Boolean;
    procedure RefreshStatusImages;
    function GetAggregateStatusImageIndex(const Counts: TStatusCounts;
      Total: Integer; const StatusDefinitions: TStatusDefinitions): Integer;
    procedure HighlightStrongs(syntagm: TSyntagm);
    function CreateBibleTreeContextMenu: TPopupMenu;
    procedure UpdateReference(ref: string);
    function BibleTreeNodeToReference(const node: TTreeNode): TReference;
    procedure SetStatusDescription(index: Integer; description: string);
    function HasCustomStatus: boolean;
    procedure MigrateStoredAssociationsToTokenizerVersion(OldVersion,
      NewVersion: Integer; ProgressBar: TProgressBar = nil);
    function EnsureAssociationMemoryFresh: Boolean;
    procedure RebuildAssociationMemory(ProgressBar: TProgressBar);
    procedure AssociationMemoryRebuildProgress(
      Phase: TAssociationMemoryRebuildPhase; Current, Total: Integer);
    procedure ReconcileEditedVerseAssociationMemory;
    function ReconcileChangedVerseAssociations: Boolean;
    procedure PendingEditorChanged(Sender: TObject);
  protected
    procedure DispatchNewVerseObserversAfterEdit(Data: PtrInt);
    procedure NotifyNewVerseObserversAfterEdit;
    function CreateTableDataset(db, tableName, keyField: string): TSqlite3Dataset;
    function CreateQueryObject(db: string): TSQLQuery;
    function InsertInfo(info, value: string): boolean;
    function UpdateInfo(info, value: string): boolean;
    function UpdateConfigTable(table: TSqlite3Dataset; name, value: string): boolean;
    function ReadInfo(info: string): string;
    function TrySetVerseText(versiculo: string; texto: TTipoTextoBiblico;
      replace: boolean; out ErrorDetails: string): Boolean;
    procedure SetVerseText(versiculo: string; texto: TTipoTextoBiblico; replace: boolean);
    procedure BeforeVerseScroll(DataSet: TDataSet);
    procedure AfterVerseScroll(DataSet: TDataSet);
    procedure RefreshAutomaticNoteStarts;
    procedure SavePairs;
    procedure SaveText;
    procedure SaveAssociations;
    procedure RemoveAssociations;
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
    function OnChangeTextVersiculo(Sender: TVerseMemo;
      const ATrace: TVerseEditTrace;
      out AImpact: TAssociationEditImpact): Boolean;
    procedure OnExibirDefinicao(Sender: TObject);
    procedure OnDblClickVersiculo(Sender: TObject);
    procedure OnTranslatorNoteClick(Sender: TVerse; ANoteId,
      ARevision: QWord);
    procedure OnBibleTreePopupItemClick(Sender: TObject);
    procedure OnBibleTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure HandleAutoSave(Sender: TObject);
    function GetVerseReferenceString(Verse: TVerse): string;
    procedure AttachProjectVersePopupMenu(texto: TTipoTextoBiblico);
    function FindVerseType(Verse: TVerse; out VerseType: TTipoTextoBiblico): Boolean;
    function IsVerseObjectText(text: TTipoTextoBiblico): Boolean;
    procedure ApplyTextDirectionToVerse(text: TTipoTextoBiblico;
      Direction: TTextDirection);
    procedure OnTextDirectionChange(Sender: TVerse;
      Direction: TTextDirection);
    { AI Visual Feedback }
    procedure MarkNodeAsAIPending(node: TTreeNode; const ref: string);
    procedure MarkNodeAsAIProcessing(const ref: string);
    procedure RestoreNodeStatus(const ref: string);
    procedure ClearAllAINodeStates;
    function FindTreeNodeByReference(const ref: string): TTreeNode;
    function AssignStrongDictionary(dic: string; t: TTipoTextoBiblico): boolean;
    function AssignMorphologyDictionary(dic: string; t: TTipoTextoBiblico): boolean;
    procedure UpdateTree;
    procedure UpdateTree(id: string);
  public
    procedure OnAIBatchClick(Sender: TObject);
    procedure OnAICancelClick(Sender: TObject);
    constructor Create; overload;
    constructor Create(paineis: array of TScrollbox; navegador: TTreeView; rsituacao: TComboBox; rcomentarios: TControl); overload;
    destructor Destroy; override;
    procedure CreateProject(nomedb, descricao: string);
    procedure CreateProject(nomedbvelho, nomedbnovo, descricao: string);
    procedure CreateProject(nomedb, descricao: string; tokenizerVersion: Integer); overload;
    procedure CreateProject(nomedbvelho, nomedbnovo, descricao: string; tokenizerVersion: Integer); overload;
    procedure OpenProject(Nome: string);
    function CloseProject(_Commit: boolean): Boolean;
    procedure Commit;
    procedure Rollback;
    procedure GoToReference(reference: string);
    procedure GoToReference(reference: TReference);
    procedure GoBack;
    procedure GoForward;
    procedure NextVerse;
    procedure PreviousVerse;
    procedure FirstVerse;
    procedure LastVerse;
    procedure RefreshUI;
    procedure RollbackCurrentVerse;
    procedure ClearAssociations;
    procedure ChangeTokenizerVersion(AValue: Integer;
      ProgressBar: TProgressBar = nil; MigrateAssociations: Boolean = True);
    function SaveProject: Boolean;
    procedure SaveVersePairs(Verse: TVerse); // Save pairs from any verse object (public for AI flows)
    procedure ApplyVerseText(const newText: string; texto: TTipoTextoBiblico; replace: Boolean);
    procedure SuggestAssociationStatistical;
    procedure SuggestAssociation;
    procedure SuggestAssociationAI;
    procedure StartAIAssociationBatch(NodesVerses: TAIBatchWorkItemList; MaxParallel: Integer); // New: batch IA association
    procedure StartAITranslationBatch(NodesVerses: TAIBatchWorkItemList; MaxParallel: Integer); // batch tradução IA
    procedure RebuildSuggestionDatabase;
    procedure RebuildSuggestionDatabase(pb: TProgressBar);
    function ImportTheWordModule(arquivo: string; texto: TTipoTextoBiblico; replace: boolean): boolean;
    function ImportTheWordModule(arquivo: string; texto: TTipoTextoBiblico; pb: TProgressBar; replace: boolean): boolean;
    function ImportTheWordModule(arquivo: string; texto: TTipoTextoBiblico;
      pb: TProgressBar; replace: boolean;
      out AImportResult: TTheWordModuleImportResult;
      AEnableStagedBatching: Boolean = False): boolean;
    procedure CreateVerseObject(owner: TScrollBox; texto: TTipoTextoBiblico);
    procedure ExportDestinationTextWithStrongs(arquivo: string; opcoes: TOpcoesExportacao);
    procedure ExportDestinationTextWithStrongs(arquivo: string; pb: TProgressBar; opcoes: TOpcoesExportacao);
    procedure ExportInterlinearText(arquivo: string; opcoes: TOpcoesExportacao);
    procedure ExportInterlinearText(arquivo: string; pb: TProgressBar; opcoes: TOpcoesExportacao);
    procedure ExportConcordance(arquivo: string; opcoes: TOpcoesExportacao; abbreviation: string);
    procedure ExportConcordance(arquivo: string; pb: TProgressBar;
      opcoes: TOpcoesExportacao; abbreviation: string;
      IncludeVerseContext: Boolean = False;
      UseIndexedOccurrences: Boolean = False;
      ProgressEvent: TConcordanceExportProgressEvent = nil);
    procedure ExportConcordance(arquivo: string; pb: TProgressBar;
      opcoes: TOpcoesExportacao; abbreviation: string;
      ContextMode: TConcordanceContextMode; ContextMaxCharacters: Integer;
      UseIndexedOccurrences: Boolean;
      ProgressEvent: TConcordanceExportProgressEvent);
    function PrepareConcordanceExport(UseIndexedOccurrences: Boolean): Boolean;
    procedure AssignTextFont(fonte: TFont; textos: STextosBiblicos);
    procedure ClearText(texto: TTipoTextoBiblico);
    function GetTextFont(texto: TTipoTextoBiblico): TFont;
    function AssignStrongDictionary(dic: string; textos: STextosBiblicos): boolean;
    function AssignMorphologyDictionary(dic: string; textos: STextosBiblicos): boolean;
    procedure SetInfo(info, valor: string);
    function GetInfo(info: string): string;
    function GetFlowExamples(const FlowId: string): TAIFlowExampleArray;
    procedure SetFlowExamples(const FlowId: string; const Examples: TAIFlowExampleArray);
    function TryCreateCurrentVerseFlowExample(const FlowId: string;
      out Example: TAIFlowExample; out ErrorMessage: string): Boolean;
    function IncludeCurrentVerseAsFlowExample(const FlowId: string): Boolean;
    procedure RemoveFlowExample(const FlowId, ReferenceId: string);
    procedure ClearFlowExamples(const FlowId: string);
    function GetVerseText(texto: TTipoTextoBiblico): string;
    function GetVerseText(Referencia: string; texto: TTipoTextoBiblico): string;
    function GetPlainVerseText(texto: TTipoTextoBiblico): string;
    function GetPlainVerseText(Referencia: string; texto: TTipoTextoBiblico): string;
    function GetTranslationSuggestions(syntagm: TSyntagm): string;
    function TryGetCurrentVerseTranslationMemory(
      Order: TTranslationMemoryOrder;
      out Entries: TTranslationMemoryEntryArray;
      out ErrorMessage: string): Boolean;
    function TryGetTranslationMemoryOccurrences(
      const Matches: TTranslationPairMatchArray; Limit: Integer;
      out Occurrences: TTranslationOccurrencePage;
      out ErrorMessage: string): Boolean;
    function GetChapterText: TStringList;
    procedure CompileInterlinearVerseRules(var rulesRx: TRegexList; var replaceTo: TStringList; mode: TInterlinearMode; exporting: boolean);
    procedure Translate;
    procedure ToggleDisplayTags;
    procedure StartScrollingSession;
    procedure FinishScrollingSession;
    procedure RewindChapter;
    procedure OnNewVerseSubscribe(const AValue: TOnNovoVersiculoEvent);
    procedure OnNewVerseUnsubscribe(const AValue: TOnNovoVersiculoEvent);
    procedure ApplyProjectPatch(patch: TPatchFile; indexes: TIntegerList = nil);
    function ApplyAcceptedBatch(Batch, OriginalData: TAIProcessingBatch;
      SelectedIndexes: TAIBatchIndexList = nil): Integer;
    function GetPairs(ref: string): string;
    function GetComments(ref: string): string;
    function GetStatus(ref: string): Integer;
    procedure SetTextDescription(text: TTipoTextoBiblico; description: string);
    function GetTextDescription(text: TTipoTextoBiblico): string;
    procedure SetTextDirection(text: TTipoTextoBiblico; direction: TTextDirection);
    function GetTextDirection(text: TTipoTextoBiblico): TTextDirection;
    function HasStoredAssociations: Boolean;
    procedure ClearStoredAssociations(ProgressBar: TProgressBar = nil);
    procedure Vacuum;
    function GetStatusDescription(index: Integer): string;
    function GetTreeNodeStatusHint(Node: TTreeNode): string;
    function GetStatusDefinitions: TStatusDefinitions;
    procedure SetStatusDefinitions(const Definitions: TStatusDefinitions);
    procedure RefreshStatusControl;
    function StatusAutomationEnabled: Boolean;
    procedure CountVersesByStatus(out Counts: TStatusCounts);
    function CountVersesWithStatus(StatusId: Integer): Integer;
    procedure RemapStatus(OldStatusId, NewStatusId: Integer);

    property FileName: string read FFileName;
    property FormattedReference: string read GetFormattedReference;
    property Reference: TReference read FReference;
    property ID: string read GetID;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
    property Modificado: boolean read GetModificado;
    property Arvore: TTreeView read FArvore write FArvore;
    property OnAlterarVersiculo: TOnProjectVerseChangedEvent read FOnAlterarVersiculo write SetOnAlterarVersiculo;
    property OnSintagmaClick: TOnSintagmaClickEvent read FOnSintagmaClick write SetOnSintagmaClick;
    property AtrasoExibicaoDefinicao: Cardinal read FAtrasoExibicao write SetAtrasoExibicao;
    property Ativo: boolean read FAtivo;
    property Caminho: string read GetCaminho;
    property Status: Integer read GetStatus write SetStatus;
    property Comments: string read GetComments write SetComments;
    property MemoComentarios: TControl read FMemoComentarios write FMemoComentarios;
    property RadioGroupSituacao: TComboBox read FRadioGroupSituacao write FRadioGroupSituacao;
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
    property ChapterViewMode: TChapterViewMode read GetChapterViewMode write SetChapterViewMode;
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
    rather than receiving four callback closures over TProject. }
  TProjectVerseStatusCursor = class(TInterfacedObject, IVerseStatusCursor)
  private
    FOwner: TProject;
  public
    constructor Create(AOwner: TProject);
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
  SSwitchDestinationAndReference1Texts = 'Switch destination with reference 1 in this verse';
  SSwitchDestinationAndReference1TextsConfirmation = 'This will change text only in the current verse (%s).'#13#10#13#10 +
                                                     'The destination text will be replaced by reference 1 text. The previous destination text will be moved to reference 1 with the "(KJF original)" prefix.'#13#10#13#10 +
                                                     'Text and associations in other verses will not be changed.'#13#10#13#10 +
                                                     'Continue?';
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
  SButtonYes = 'Yes';
  SButtonNo = 'No';
  SAIConfigurationMissing = 'AI is not configured. Open LLM options and set a model and API key.';
  SNoEmptyTranslationsInScope = 'There are no empty translations in the selected scope.';
  SNoVerseToAssociate = 'No verse to associate.';
  SAIAssociationFailedFmt = 'AI association failed: %s';
  SAISingleResponseReceived = '[AI Single] Response received, parsing...';
  SAISingleApplyingResult = '[AI Single] Applying result...';
  SAISingleResultApplied = '[AI Single] Result applied and saved.';
  SAISingleErrorFmt = '[AI Single] Error: %s';
  SAnalyzingVerse = 'Analyzing verse...';
  SAssociationMemoryUnavailable =
    'Translation memory is unavailable because it could not be synchronized.';

  SStatusNotAssociated = 'Not associated';
  SStatusAssociating = 'Associating';
  SStatusNeedsReview = 'Needs review';
  SStatusAssociated = 'Associated!';
  SAnalyticalConcordance = 'Analytical Concordance';
  SSyntheticConcordance = 'Synthetic Concordance';
  SOTConcordanceNotImplementedYet = 'Old Testament concordance is not implemented yet';
  SLanguageId = 'en';
  SError = 'Error';
  SWarning = 'Warning';
  SInformation = 'Information';
  SButtonOK = 'OK';
  SButtonCancel = 'Cancel';
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
  SPotentialNonUTF8ModuleWarning = 'This module is not valid UTF-8 (invalid byte at offset %d). ' +
                                   'iBiblia was not designed to import legacy or malformed encodings. ' +
                                   'Choose ''Ignore'' to continue at your own risk, or ''Abort'' to choose another module.';
  SModuleImportCancelled = 'The module import was cancelled.';
  SModuleImportRulesCancelled = 'The module import was cancelled while choosing verse rules.';
  SModuleImportProgressCountFmt = '%d of %d verses';
  SModuleImportRestoring = 'Restoring project text after import failure...';
  SStagedImportRequiresCleanPairBaseline =
    'Staged module import requires a clean pair baseline.';
  SModuleImportCapacityError = 'The project has %d verse rows available for this module, but %d are required. No text was imported.';
  SModuleImportIncompleteError = 'The import stopped after %d of %d verses. Last imported reference: %s.';
  SModuleImportExceptionError = 'The module could not be imported after %d of %d verses. Last imported reference: %s.'#13#10'%s';
  SFailedToCreateFile = 'Failed to create file: %s';
  SExportToFileError = 'iBiblia found an error while exporting the Bible module file.';
  SExportErrorReportCreated = 'Some verses could not be exported correctly. Review the error report:'#13#10'%s';
  STranslationMemory = 'Translation memory';
  SExportTextSaveDlgTitle = 'Please choose a destination file';
  SIBibliaPatchFiles = 'iBiblia patch files';
  SPatchSuccessfullyExported = 'Patch successfully exported';
  SReferenceNotFound = 'Reference not found';
  SReferenceNotFoundWarning = 'The reference %s is not a valid reference in this project and will be ignored.'#13#10'Press Ignore to ignore all subsequent warnings.';
  SPatchStatusCatalogTitle = 'Patch status catalog';
  SPatchStatusCatalogDiverged = 'This patch uses a different status catalog. Map patch statuses to this project before applying.';
  SPatchStatusUnknownNoCatalog = 'This patch uses status IDs that do not exist in this project and does not include a status catalog.'#13#10#13#10 +
                                 'Apply the saved integer values anyway?';
  SPatchStatusLabelFmt = '%s (ID %d)';
  SVerseAbsentFromProject = '[This project does not contain this verse]';
  SInvalidInterlinearVerseRule = 'There is something wrong with your interlinear verse rule.'#13#10'[%s]';
  SInterlinearVerseRuleReplaceError = 'There is something wrong with one of your interlinear verse rules.'#13#10'Please review them.';
  SModuleDoesNotIncludeScope = 'This module is incompatible with the scope of the project. Please choose a valid module.';
  SInvalidModuleType = 'Unknown module extension. Please choose a valid theWord Bible module (.ot, .ont or .nt)';
  SConfirmation = 'Confirmation';
  SVaccumProjectConfirmation = 'Vacuuming requires that your project be saved and reopened. Do you want to proceed?';
  SVacuumDone = 'Vacuum done!';
  SVacuumProgressTitle = 'Optimize project';
  SVacuumProgressMessage = 'Optimizing project database...';
  SAssociationMemoryProgressTitle = 'Association memory';
  SAssociationMemoryProgressMessage =
    'Updating after an iBiblia or project change...';
  SAssociationMemoryProgressDetailFmt = '%s (%d of %d)';
  SAssociationMemoryWriting = 'Writing association memory...';
  SAssociationMemoryValidating = 'Validating association memory...';
  SAssociationMemoryFinalizing = 'Finalizing association memory...';
  SAssociationMemoryDatabaseProgressFmt = '%d of %d associations';
  SErrorWithDetailsFmt = '%s: %s';
  SErrorClassMessageFmt = '%s: %s'#13#10'%s';

const
  QStrongs: array[etOT..etONT] of smallint = (8674, 5624, 14298);

procedure UpdateDefaultStatusDescription;
function TextTypeParticipatesInAssociations(
  ATextType: TTipoTextoBiblico): Boolean;

implementation

uses FormDictionaryPopup, FormVerseRules, formaiprocessing,
  theword_module_encoding;

type
  { Executes only the blocking network request outside the LCL thread. Verse
    parsing and result application remain on the main thread. }
  TSingleLLMRequestThread = class(TThread)
  private
    FApiKey: string;
    FErrorMessage: string;
    FMessages: TLLMMessageArray;
    FModel: string;
    FResponse: TLLMResponse;
  protected
    procedure Execute; override;
  public
    constructor Create(const AModel, AApiKey: string;
      const AMessages: TLLMMessageArray);
    property ErrorMessage: string read FErrorMessage;
    property Response: TLLMResponse read FResponse;
  end;

function TextTypeParticipatesInAssociations(
  ATextType: TTipoTextoBiblico): Boolean;
begin
  Result := ATextType in [tbOrigem, tbDestino];
end;

{ TProject }

const
  DefaultStatusDescription: array[0..3] of string = (
    'Not associated',
    'Associating',
    'Needs review',
    'Associated'
  );

constructor TSingleLLMRequestThread.Create(const AModel, AApiKey: string;
  const AMessages: TLLMMessageArray);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FModel := AModel;
  FApiKey := AApiKey;
  FMessages := AMessages;
end;

procedure TSingleLLMRequestThread.Execute;
var
  Client: TBaseLLMClient;
begin
  Client := nil;
  try
    try
      // HTTP clients are created and used in the worker thread, matching the
      // batch-processing lifecycle.
      Client := llmclient.CreateBackend(FModel, FApiKey);
      FResponse := Client.AskChatCompletions(FMessages);
    except
      on E: Exception do
        FErrorMessage := E.Message;
    end;
  finally
    Client.Free;
  end;
end;

function AskChatCompletionsResponsive(const Model, ApiKey: string;
  const Messages: TLLMMessageArray): TLLMResponse;
var
  RequestThread: TSingleLLMRequestThread;
begin
  RequestThread := TSingleLLMRequestThread.Create(Model, ApiKey, Messages);
  try
    RequestThread.Start;
    while not RequestThread.Finished do
    begin
      Application.ProcessMessages;
      if not RequestThread.Finished then
        Sleep(15);
    end;
    RequestThread.WaitFor;

    if RequestThread.ErrorMessage <> '' then
      raise Exception.Create(RequestThread.ErrorMessage);
    Result := RequestThread.Response;
  finally
    RequestThread.Free;
  end;
end;

procedure ConfigureOperationProgressBar(ProgressBar: TProgressBar;
  MaxValue: Integer);
begin
  if not Assigned(ProgressBar) then
    Exit;

  if MaxValue <= 0 then
    MaxValue := 1;

  ProgressBar.Style := pbstNormal;
  ProgressBar.Position := 0;
  ProgressBar.Min := 0;
  ProgressBar.Max := MaxValue;
  ProgressBar.Step := 1;
  ProgressBar.Visible := True;
  ProgressBar.Update;
end;

procedure AdvanceOperationProgressBar(ProgressBar: TProgressBar;
  ProcessMessagesEvery: Integer = 50);
begin
  if not Assigned(ProgressBar) then
    Exit;

  ProgressBar.StepIt;
  if (ProcessMessagesEvery > 0) and
    ((ProgressBar.Position mod ProcessMessagesEvery) = 0) then
    Application.ProcessMessages;
end;

procedure UpdateDefaultStatusDescription;
begin
  DefaultStatusDescription[0] := SStatusNotAssociated;
  DefaultStatusDescription[1] := SStatusAssociating;
  DefaultStatusDescription[2] := SStatusNeedsReview;
  DefaultStatusDescription[3] := SStatusAssociated;
end;

{ TProjectVerseStatusCursor }

constructor TProjectVerseStatusCursor.Create(AOwner: TProject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TProjectVerseStatusCursor.Reset;
begin
  FOwner.FirstVerse;
end;

function TProjectVerseStatusCursor.EOF: Boolean;
begin
  Result := not Assigned(FOwner.FPersistence) or FOwner.FPersistence.EOF;
end;

procedure TProjectVerseStatusCursor.Advance;
begin
  FOwner.NextVerse;
end;

function TProjectVerseStatusCursor.CurrentStatus: Integer;
var
  Definitions: TStatusDefinitions;
begin
  Result := FOwner.Status;
  Definitions := FOwner.GetStatusDefinitions;
  if not IsValidStatusId(Definitions, Result) then
    Result := FirstStatusId(Definitions);
end;

function TProject.GetLegacyStatusDefinitions: TStatusDefinitions;
var
  Captions: array[0..3] of string;
  StatusIndex: Integer;
  CustomCaption: string;
begin
  for StatusIndex := 0 to 3 do
  begin
    CustomCaption := GetInfo('status.description.' + IntToStr(StatusIndex));
    if CustomCaption.IsEmpty then
      Captions[StatusIndex] := DefaultStatusDescription[StatusIndex]
    else
      Captions[StatusIndex] := CustomCaption;
  end;
  Result := BuildLegacyStatusDefinitions(Captions);
end;

function TProject.HasLegacyStatusDescriptions: Boolean;
var
  StatusIndex: Integer;
begin
  Result := False;
  for StatusIndex := 0 to 3 do
    if not GetInfo('status.description.' + IntToStr(StatusIndex)).IsEmpty then
      Exit(True);
end;

function TProject.GetStatusDefinitions: TStatusDefinitions;
var
  Legacy: TStatusDefinitions;
begin
  Legacy := GetLegacyStatusDefinitions;
  Result := LoadStatusDefinitions(GetInfo(STATUS_CATALOG_CONFIG_KEY),
    [
      Legacy[0].Caption,
      Legacy[1].Caption,
      Legacy[2].Caption,
      Legacy[3].Caption
    ]);
end;

procedure TProject.SetStatusDefinitions(const Definitions: TStatusDefinitions);
var
  Normalized: TStatusDefinitions;
  ErrorMessage: string;
begin
  Normalized := Copy(Definitions, 0, Length(Definitions));
  NormalizeStatusDefinitions(Normalized, False);
  if not ValidateStatusDefinitions(Normalized, ErrorMessage) then
    raise Exception.Create(ErrorMessage);

  FConfig.WriteString(STATUS_CATALOG_CONFIG_KEY,
    SaveStatusDefinitions(Normalized));
  RefreshStatusImages;
  RefreshStatusControl;
  if Assigned(FArvore) and Assigned(FArvore.PopupMenu) then
  begin
    FArvore.PopupMenu.Free;
    FArvore.PopupMenu := CreateBibleTreeContextMenu;
  end;
  UpdateTree;
end;

function TProject.GetStatusDescription(index: Integer): string;
begin
  Result := GetStatusCaption(GetStatusDefinitions, index);
end;

function TProject.GetTreeNodeStatusHint(Node: TTreeNode): string;
begin
  Result := FBibleTreeCoordinator.BuildStatusHint(FEscopo, Node,
    GetStatusDefinitions);
end;

procedure TProject.SetStatusDescription(index: Integer; description: string);
var
  Definitions: TStatusDefinitions;
  StatusIndex: Integer;
begin
  Definitions := GetStatusDefinitions;
  StatusIndex := IndexOfStatusId(Definitions, index);
  if StatusIndex >= 0 then
  begin
    Definitions[StatusIndex].Caption := description;
    SetStatusDefinitions(Definitions);
  end
  else
    FConfig.WriteString('status.description.' + IntToStr(index), description);
end;

function TProject.StatusAutomationEnabled: Boolean;
begin
  Result := GetInfo(STATUS_CATALOG_CONFIG_KEY).IsEmpty and
    (not HasLegacyStatusDescriptions);
end;

function TProject.HasCustomStatus: boolean;
begin
  Result := not StatusAutomationEnabled;
end;

function TProject.BuildAssociationFlowExample: TAIFlowExample;
var
  SourceVerse, TargetVerse: TVerse;
begin
  SourceVerse := FAVersiculo[tbOrigem];
  TargetVerse := FAVersiculo[tbDestino];

  if (not Assigned(SourceVerse)) or (not Assigned(TargetVerse)) or (SourceVerse.PairedVerse <> TargetVerse) then
    raise Exception.Create(SAssociationExampleRequiresPair);

  if not SourceVerse.HasAssociations then
    raise Exception.Create(SAssociationExampleRequiresAssociations);

  Result.ReferenceId := ID;
  Result.ReferenceCaption := FormattedReference;
  Result.SourceText := SourceVerse.PlainText;
  Result.TargetText := TargetVerse.PlainText;
  Result.SourceXml := SourceVerse.XML;
  Result.TargetXml := TargetVerse.XML;
  Result.ExpectedResponse := SourceVerse.GetAssociationsJSON;
  Result.PairsXml := SourceVerse.Pairs;
end;

function TProject.BuildTranslationFlowExample: TAIFlowExample;
var
  SourceVerse, TargetVerse: TVerse;
begin
  SourceVerse := FAVersiculo[tbOrigem];
  TargetVerse := FAVersiculo[tbDestino];

  if (not Assigned(SourceVerse)) or (not Assigned(TargetVerse)) or (SourceVerse.PairedVerse <> TargetVerse) then
    raise Exception.Create(STranslationExampleRequiresPair);

  if Trim(TargetVerse.PlainText) = '' then
    raise Exception.Create(STranslationExampleRequiresText);

  Result.ReferenceId := ID;
  Result.ReferenceCaption := FormattedReference;
  Result.SourceText := SourceVerse.PlainText;
  Result.TargetText := TargetVerse.PlainText;
  Result.SourceXml := SourceVerse.XML;
  Result.TargetXml := TargetVerse.XML;
  Result.ExpectedResponse := TargetVerse.PlainText;
  Result.PairsXml := '';
end;

function TProject.CreateFlowExample(const FlowId: string): TAIFlowExample;
begin
  case LowerCase(Trim(FlowId)) of
    'association': Result := BuildAssociationFlowExample;
    'translation': Result := BuildTranslationFlowExample;
  else
    raise Exception.CreateFmt('Unknown flow ID for example creation: %s', [FlowId]);
  end;
end;

function TProject.PromptReplaceExistingFlowExample(const FlowId: string; const ExistingExample: TAIFlowExample): Boolean;
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

procedure TProject.CreateProject(nomedb, descricao: string);
begin
  CreateProject(nomedb, descricao, TTokenizerFactory.LATEST_VERSION);
end;

procedure TProject.CreateProject(nomedbvelho, nomedbnovo, descricao: string);
begin
  CreateProject(nomedbnovo, descricao, TTokenizerFactory.LATEST_VERSION);
end;

procedure TProject.CreateProject(nomedb, descricao: string; tokenizerVersion: Integer);
begin
  TProjectDatabaseBootstrap.CreateProjectDatabase(nomedb, FEscopo);

  OpenProject(nomedb);

  SetInfo('descricao', descricao);
  ChangeTokenizerVersion(tokenizerVersion, nil, False);
  FExibirDefComCtrl := false;  //FTblInfo.ExecuteDirect('COMMIT;');
  FirstVerse;
  Commit;
end;

procedure TProject.CreateProject(nomedbvelho, nomedbnovo, descricao: string; tokenizerVersion: Integer);
begin
  CreateProject(nomedbnovo, descricao, tokenizerVersion);
end;

function TProject.GetFormattedReference: string;
begin
  result := format('%s %d:%d', [Book, Chapter, Verse]);
end;

function TProject.GetPairs: string;
begin
  Result := FPersistence.CurrentPairs;
end;

function TProject.GetParagraphMode: TParagraphMode;
begin
  Result := TParagraphMode(FConfig.ReadParagraphMode(Ord(pmParagraph)));
end;

function TProject.GetTokenizerVersion: Integer;
begin
  Result := TTokenizerFactory.NormalizeVersion(
    FConfig.ReadTokenizerVersion(TTokenizerFactory.LEGACY_VERSION));
end;

function TProject.GetPairs(ref: string): string;
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

function TProject.GetStatus: Integer;
begin
  Result := FPersistence.CurrentStatus;
end;

procedure TProject.PreencherArvore;
begin
  FBibleTreeCoordinator.Populate(FEscopo, @OnMudancaVersiculo,
    GetStatusDefinitions, @GetAggregateStatusImageIndex);
end;

procedure TProject.SetAtrasoExibicao(const AValue: Cardinal);
begin
  if FAtrasoExibicao = AValue then exit;
  FAtrasoExibicao := AValue;
  FTemporizador.Interval := AValue;
end;

procedure TProject.SetAutoSave(AValue: boolean);
begin
  if FAutoSave=AValue then Exit;
  FAutoSave:=AValue;
end;

procedure TProject.SetChapterViewText(AValue: TTipoTextoBiblico);
begin
  FConfig.WriteChapterViewText(Ord(AValue));
end;

procedure TProject.SetChapterViewMode(AValue: TChapterViewMode);
begin
  FConfig.WriteChapterViewMode(Ord(AValue));
end;

procedure TProject.SetComments(const AValue: string);
begin
  if FPersistence.CurrentComments = AValue then
    exit;
  FPersistence.WriteCurrentComments(AValue);
end;

procedure TProject.SetDisplayTags(AValue: boolean);
begin
  if FDisplayTags = AValue then
    Exit;

  FDisplayTags := AValue;
  FCurrentVerseWorkspace.ApplyDisplayTags(FAVersiculo, FDisplayTags);
end;

procedure TProject.SetInterlinearMode(AValue: TInterlinearMode);
begin
  FConfig.WriteInterlinearMode(Ord(AValue));
end;

procedure TProject.SetParagraphMode(AValue: TParagraphMode);
begin
  FConfig.WriteParagraphMode(Ord(AValue));
end;

procedure TProject.SetTokenizerVersion(AValue: Integer);
begin
  ChangeTokenizerVersion(AValue);
end;

procedure TProject.ChangeTokenizerVersion(AValue: Integer;
  ProgressBar: TProgressBar; MigrateAssociations: Boolean);
var
  currentVersion: Integer;
begin
  AValue := TTokenizerFactory.NormalizeVersion(AValue);
  currentVersion := TokenizerVersion;
  if currentVersion = AValue then
    Exit;

  if FAssociationMemorySpeculativeEditPending then
    BeforeVerseScroll(nil);

  if MigrateAssociations then
    MigrateStoredAssociationsToTokenizerVersion(currentVersion, AValue,
      ProgressBar);

  FConfig.WriteTokenizerVersion(AValue);
  TTokenizerFactory.PreferredVersion := AValue;

  FCurrentVerseWorkspace.ApplyTokenizerVersion(FAVersiculo, AValue);

  if Assigned(FVerseRenderBuffer) then
    FVerseRenderBuffer.SetTokenizerVersion(AValue);

  if Assigned(FTblPares) and FAtivo then
    AfterVerseScroll(nil);
end;

procedure TProject.MigrateStoredAssociationsToTokenizerVersion(OldVersion,
  NewVersion: Integer; ProgressBar: TProgressBar);
var
  oldPairs: string;
begin
  if (OldVersion = NewVersion) or not Assigned(FPersistence) then
    Exit;

  if not Assigned(FVerseRenderBuffer) then
    Exit;

  FAssociationMemory.MarkSourceDirty;
  FAssociationMemory.InvalidateReplay;
  StartScrollingSession;
  try
    ConfigureOperationProgressBar(ProgressBar, QLinhas[FEscopo]);
    FirstVerse;
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

      NextVerse;
      AdvanceOperationProgressBar(ProgressBar);
    end;
  finally
    FVerseRenderBuffer.SetTokenizerVersion(NewVersion);
    FinishScrollingSession;
  end;
  FCurrentVerseWorkspace.ApplyTokenizerVersion(FAVersiculo, NewVersion);
  RebuildAssociationMemory(ProgressBar);
end;

procedure TProject.SetVerseStrongsCountMode(AValue: TStrongsCountMode);
begin
  if FVerseStrongsCountMode = AValue then
    Exit;

  FVerseStrongsCountMode := AValue;
  FCurrentVerseWorkspace.ApplyStrongsCountMode(FAVersiculo, AValue);
end;

procedure TProject.SetOnAlterarVersiculo(const AValue: TOnProjectVerseChangedEvent);
var
  TextType: TTipoTextoBiblico;
begin
  if FOnAlterarVersiculo = AValue then exit;
  FOnAlterarVersiculo := AValue;

  for TextType := Low(FAVersiculo) to High(FAVersiculo) do
    if assigned(FAVersiculo[TextType]) then
      FAVersiculo[TextType].OnVerseChanged := FOnAlterarVersiculo;
end;

procedure TProject.OnNewVerseSubscribe(const AValue: TOnNovoVersiculoEvent);
begin
  if FOnNovoVersiculo.IndexOf(AValue) = -1 then
    FOnNovoVersiculo.Add(AValue);
end;

procedure TProject.OnNewVerseUnsubscribe(const AValue: TOnNovoVersiculoEvent);
begin
  FOnNovoVersiculo.Remove(AValue);
end;

function TProject.ConfirmMissingPatchReference(const Reference: string): Boolean;
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

procedure TProject.ApplyProjectPatch(patch: TPatchFile; indexes: TIntegerList = nil);
var
  oldAutoSaveValue: boolean;
  selectedIndexes: TPatchIndexArray;

  function PatchIndexIsSelected(PatchIndex: Integer): Boolean;
  var
    SelectedIndex: Integer;
  begin
    if Length(selectedIndexes) = 0 then
      Exit(True);

    for SelectedIndex in selectedIndexes do
      if SelectedIndex = PatchIndex then
        Exit(True);

    Result := False;
  end;

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

  function StatusListContains(const Values: array of Integer;
    StatusId: Integer): Boolean;
  var
    Value: Integer;
  begin
    for Value in Values do
      if Value = StatusId then
        Exit(True);
    Result := False;
  end;

  function BuildUsedPatchStatuses: TIntegerArray;
  var
    PatchIndex: Integer;
    StatusId: Integer;
  begin
    Result := nil;
    if not Assigned(patch) then
      Exit;

    for PatchIndex := 0 to patch.Status.Count - 1 do
    begin
      if not PatchIndexIsSelected(PatchIndex) then
        Continue;
      StatusId := StrToIntDef(patch.Status[PatchIndex], 0);
      if not StatusListContains(Result, StatusId) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := StatusId;
      end;
    end;
  end;

  function StatusDefinitionsDiffer(const PatchDefinitions,
    CurrentDefinitions: TStatusDefinitions; StatusId: Integer): Boolean;
  var
    PatchIndex: Integer;
    CurrentIndex: Integer;
  begin
    PatchIndex := IndexOfStatusId(PatchDefinitions, StatusId);
    CurrentIndex := IndexOfStatusId(CurrentDefinitions, StatusId);
    if (PatchIndex < 0) or (CurrentIndex < 0) then
      Exit(True);

    Result := (PatchDefinitions[PatchIndex].Caption <>
      CurrentDefinitions[CurrentIndex].Caption) or
      (ColorToRGB(PatchDefinitions[PatchIndex].Color) <>
        ColorToRGB(CurrentDefinitions[CurrentIndex].Color)) or
      (PatchDefinitions[PatchIndex].DisplayOrder <>
        CurrentDefinitions[CurrentIndex].DisplayOrder);
  end;

  function ComboIndexForStatus(Combo: TComboBox; StatusId: Integer): Integer;
  var
    ItemIndex: Integer;
  begin
    Result := -1;
    for ItemIndex := 0 to Combo.Items.Count - 1 do
      if PtrInt(Combo.Items.Objects[ItemIndex]) = StatusId then
        Exit(ItemIndex);
  end;

  function CaptionMatchStatusId(const CurrentDefinitions: TStatusDefinitions;
    const PatchDefinitions: TStatusDefinitions; PatchStatusId: Integer): Integer;
  var
    PatchIndex: Integer;
    CurrentIndex: Integer;
  begin
    Result := -1;
    PatchIndex := IndexOfStatusId(PatchDefinitions, PatchStatusId);
    if PatchIndex < 0 then
      Exit;

    for CurrentIndex := 0 to High(CurrentDefinitions) do
      if SameText(CurrentDefinitions[CurrentIndex].Caption,
        PatchDefinitions[PatchIndex].Caption) then
        Exit(CurrentDefinitions[CurrentIndex].Id);
  end;

  procedure RemapPatchStatusValues(const UsedStatusIds: array of Integer;
    const MappedStatusIds: array of Integer);
  var
    PatchIndex: Integer;
    MapIndex: Integer;
    StatusId: Integer;
  begin
    for PatchIndex := 0 to patch.Status.Count - 1 do
    begin
      if not PatchIndexIsSelected(PatchIndex) then
        Continue;

      StatusId := StrToIntDef(patch.Status[PatchIndex], 0);
      for MapIndex := Low(UsedStatusIds) to High(UsedStatusIds) do
        if UsedStatusIds[MapIndex] = StatusId then
        begin
          patch.Status[PatchIndex] := IntToStr(MappedStatusIds[MapIndex]);
          Break;
        end;
    end;
  end;

  function PromptPatchStatusMapping(const UsedStatusIds: array of Integer;
    const PatchDefinitions, CurrentDefinitions: TStatusDefinitions): Boolean;
  var
    DialogForm: TForm;
    InfoLabel: TLabel;
    LabelControl: TLabel;
    Combo: TComboBox;
    OkButton: TButton;
    CancelButton: TButton;
    Combos: array of TComboBox;
    SortedCurrent: TStatusDefinitions;
    StatusIndex: Integer;
    CurrentIndex: Integer;
    DefaultStatusId: Integer;
    DefaultComboIndex: Integer;
    MappedStatusIds: array of Integer;
  begin
    Result := False;
    DialogForm := TForm.CreateNew(nil, 1);
    try
      DialogForm.Caption := SPatchStatusCatalogTitle;
      DialogForm.Position := poMainFormCenter;
      DialogForm.Width := 560;
      DialogForm.Height := 116 + (Length(UsedStatusIds) * 34);

      InfoLabel := TLabel.Create(DialogForm);
      InfoLabel.Parent := DialogForm;
      InfoLabel.Left := 12;
      InfoLabel.Top := 12;
      InfoLabel.Width := 520;
      InfoLabel.Caption := SPatchStatusCatalogDiverged;

      SortedCurrent := SortStatusDefinitionsByDisplayOrder(CurrentDefinitions);
      SetLength(Combos, Length(UsedStatusIds));
      for StatusIndex := Low(UsedStatusIds) to High(UsedStatusIds) do
      begin
        LabelControl := TLabel.Create(DialogForm);
        LabelControl.Parent := DialogForm;
        LabelControl.Left := 12;
        LabelControl.Top := 44 + (StatusIndex * 34);
        LabelControl.Width := 230;
        LabelControl.Caption := Format(SPatchStatusLabelFmt, [
          GetStatusCaption(PatchDefinitions, UsedStatusIds[StatusIndex]),
          UsedStatusIds[StatusIndex]
        ]);

        Combo := TComboBox.Create(DialogForm);
        Combo.Parent := DialogForm;
        Combo.Left := 254;
        Combo.Top := 40 + (StatusIndex * 34);
        Combo.Width := 280;
        Combo.Style := csDropDownList;
        for CurrentIndex := 0 to High(SortedCurrent) do
          Combo.Items.AddObject(SortedCurrent[CurrentIndex].Caption,
            TObject(PtrUInt(SortedCurrent[CurrentIndex].Id)));

        DefaultStatusId := UsedStatusIds[StatusIndex];
        if not IsValidStatusId(CurrentDefinitions, DefaultStatusId) then
          DefaultStatusId := CaptionMatchStatusId(CurrentDefinitions,
            PatchDefinitions, UsedStatusIds[StatusIndex]);
        if not IsValidStatusId(CurrentDefinitions, DefaultStatusId) then
          DefaultStatusId := FirstStatusId(CurrentDefinitions);

        DefaultComboIndex := ComboIndexForStatus(Combo, DefaultStatusId);
        if DefaultComboIndex < 0 then
          DefaultComboIndex := 0;
        Combo.ItemIndex := DefaultComboIndex;
        Combos[StatusIndex] := Combo;
      end;

      OkButton := TButton.Create(DialogForm);
      OkButton.Parent := DialogForm;
      OkButton.Left := 350;
      OkButton.Top := DialogForm.ClientHeight - 40;
      OkButton.Width := 84;
      OkButton.Caption := SButtonOK;
      OkButton.ModalResult := mrOK;

      CancelButton := TButton.Create(DialogForm);
      CancelButton.Parent := DialogForm;
      CancelButton.Left := 450;
      CancelButton.Top := DialogForm.ClientHeight - 40;
      CancelButton.Width := 84;
      CancelButton.Caption := SButtonCancel;
      CancelButton.ModalResult := mrCancel;

      if DialogForm.ShowModal <> mrOK then
        Exit;

      SetLength(MappedStatusIds, Length(UsedStatusIds));
      for StatusIndex := Low(Combos) to High(Combos) do
        MappedStatusIds[StatusIndex] :=
          PtrInt(Combos[StatusIndex].Items.Objects[Combos[StatusIndex].ItemIndex]);

      RemapPatchStatusValues(UsedStatusIds, MappedStatusIds);
      Result := True;
    finally
      DialogForm.Free;
    end;
  end;

  function PreparePatchStatuses: Boolean;
  var
    UsedStatusIds: TIntegerArray;
    CurrentDefinitions: TStatusDefinitions;
    PatchDefinitions: TStatusDefinitions;
    LegacyDefinitions: TStatusDefinitions;
    StatusId: Integer;
    RequiresMapping: Boolean;
    HasUnknownStatus: Boolean;
  begin
    Result := True;
    UsedStatusIds := BuildUsedPatchStatuses;
    if Length(UsedStatusIds) = 0 then
      Exit;

    CurrentDefinitions := GetStatusDefinitions;
    HasUnknownStatus := False;
    for StatusId in UsedStatusIds do
      if not IsValidStatusId(CurrentDefinitions, StatusId) then
        HasUnknownStatus := True;

    if patch.StatusCatalog.IsEmpty then
    begin
      if HasUnknownStatus then
        Result := MessageDlg(SPatchStatusCatalogTitle,
          SPatchStatusUnknownNoCatalog, mtWarning, [mbYes, mbNo], 0) = mrYes;
      Exit;
    end;

    LegacyDefinitions := GetLegacyStatusDefinitions;
    PatchDefinitions := LoadStatusDefinitions(patch.StatusCatalog, [
      LegacyDefinitions[0].Caption,
      LegacyDefinitions[1].Caption,
      LegacyDefinitions[2].Caption,
      LegacyDefinitions[3].Caption
    ]);

    RequiresMapping := HasUnknownStatus;
    for StatusId in UsedStatusIds do
      if StatusDefinitionsDiffer(PatchDefinitions, CurrentDefinitions, StatusId) then
        RequiresMapping := True;

    if RequiresMapping then
      Result := PromptPatchStatusMapping(UsedStatusIds, PatchDefinitions,
        CurrentDefinitions);
  end;
begin
  selectedIndexes := BuildSelectedPatchIndexes;
  FIgnoreMissingPatchWarnings := False;
  if not PreparePatchStatuses then
    Exit;

  FAssociationMemory.MarkSourceDirty;
  FAssociationMemory.InvalidateReplay;
  try
    StartScrollingSession;
    FPatchApplier.ApplyPatch(patch, selectedIndexes, @ConfirmMissingPatchReference);
  finally
    FinishScrollingSession;
  end;

  RebuildAssociationMemory(nil);

  oldAutoSaveValue := FAutoSave;
  FAutoSave := false;
  RefreshUI;
  FAutoSave := oldAutoSaveValue;
end;

function TProject.ApplyAcceptedBatch(Batch, OriginalData: TAIProcessingBatch;
  SelectedIndexes: TAIBatchIndexList = nil): Integer;
var
  Patch: TPatchFile;
begin
  Result := 0;
  if not Assigned(Batch) then
    Exit;

  Patch := Batch.CreatePatch(OriginalData, SelectedIndexes, Result);
  Patch.StatusCatalog := SaveStatusDefinitions(GetStatusDefinitions);
  try
    if Result > 0 then
      ApplyProjectPatch(Patch, nil);
  finally
    Patch.Free;
  end;
end;

procedure TProject.SetOnSintagmaClick(const AValue: TOnSintagmaClickEvent);
begin
  if FOnSintagmaClick = AValue then exit;
  FOnSintagmaClick := AValue;
end;

procedure TProject.SetPalavrasComStrongEmNegrito(AValue: boolean);
begin
  if FPalavrasComStrongEmNegrito=AValue then Exit;
  FPalavrasComStrongEmNegrito:=AValue;
  FCurrentVerseWorkspace.ApplyBoldStrongs(FAVersiculo, AValue);
end;

procedure TProject.PendingEditorChanged(Sender: TObject);
begin
  if Assigned(FOnAlterarVersiculo) then
    FOnAlterarVersiculo;
end;

procedure TProject.RefreshStatusImages;
var
  Definitions: TStatusDefinitions;
  StatusId: Integer;
  MaxId: Integer;
  Bitmap: TBitmap;
  StatusColor: TColor;
begin
  if not Assigned(FArvore) then
    Exit;

  Definitions := GetStatusDefinitions;
  MaxId := MaxStatusId(Definitions);

  if Assigned(FStatusAggregateImageCache) then
    FStatusAggregateImageCache.Clear;
  FArvore.Images := nil;
  FreeAndNil(FStatusImages);
  FStatusImages := TImageList.Create(FArvore);
  FStatusImages.Width := 16;
  FStatusImages.Height := 16;

  for StatusId := 0 to MaxId do
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := 16;
      Bitmap.Height := 16;
      StatusColor := GetStatusColor(Definitions, StatusId);
      Bitmap.Canvas.Brush.Color := StatusColor;
      Bitmap.Canvas.Pen.Color := clBlack;
      Bitmap.Canvas.Rectangle(0, 0, 16, 16);
      FStatusImages.Add(Bitmap, nil);
    finally
      Bitmap.Free;
    end;
  end;

  FArvore.Images := FStatusImages;
end;

function TProject.GetAggregateStatusImageIndex(const Counts: TStatusCounts;
  Total: Integer; const StatusDefinitions: TStatusDefinitions): Integer;
const
  ImageSize = 16;
  BorderSize = 1;
  InnerSize = ImageSize - (BorderSize * 2);
var
  Segments: TStatusSegmentWidths;
  CacheKey: string;
  CacheIndex: Integer;
  SegmentIndex: Integer;
  Bitmap: TBitmap;
  X: Integer;
  SegmentRect: TRect;
begin
  Result := -1;
  if not Assigned(FStatusImages) then
    Exit;

  if not Assigned(FStatusAggregateImageCache) then
    FStatusAggregateImageCache := TStringList.Create;

  Segments := BuildStatusSegmentWidths(Counts, StatusDefinitions, Total, InnerSize);
  if Length(Segments) = 0 then
    Exit;

  CacheKey := '';
  for SegmentIndex := 0 to High(Segments) do
    CacheKey := CacheKey + Format('%d:%d;', [
      Segments[SegmentIndex].StatusId,
      Segments[SegmentIndex].Width
    ]);

  CacheIndex := FStatusAggregateImageCache.IndexOfName(CacheKey);
  if CacheIndex >= 0 then
    Exit(StrToIntDef(FStatusAggregateImageCache.ValueFromIndex[CacheIndex], -1));

  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := ImageSize;
    Bitmap.Height := ImageSize;
    Bitmap.Canvas.Brush.Color := clWhite;
    SegmentRect.Left := 0;
    SegmentRect.Top := 0;
    SegmentRect.Right := ImageSize;
    SegmentRect.Bottom := ImageSize;
    Bitmap.Canvas.FillRect(SegmentRect);

    X := BorderSize;
    for SegmentIndex := 0 to High(Segments) do
    begin
      if Segments[SegmentIndex].Width <= 0 then
        Continue;
      Bitmap.Canvas.Brush.Color := GetStatusColor(StatusDefinitions,
        Segments[SegmentIndex].StatusId);
      SegmentRect.Left := X;
      SegmentRect.Top := BorderSize;
      SegmentRect.Right := X + Segments[SegmentIndex].Width;
      SegmentRect.Bottom := BorderSize + InnerSize;
      Bitmap.Canvas.FillRect(SegmentRect);
      Inc(X, Segments[SegmentIndex].Width);
    end;

    Bitmap.Canvas.Brush.Style := bsClear;
    Bitmap.Canvas.Pen.Color := clBlack;
    Bitmap.Canvas.Rectangle(0, 0, ImageSize, ImageSize);
    Bitmap.Canvas.Brush.Style := bsSolid;

    Result := FStatusImages.Add(Bitmap, nil);
    FStatusAggregateImageCache.Add(CacheKey + '=' + IntToStr(Result));
  finally
    Bitmap.Free;
  end;
end;

procedure TProject.RefreshStatusControl;
var
  Definitions: TStatusDefinitions;
  Sorted: TStatusDefinitions;
  StatusIndex: Integer;
  CurrentStatus: Integer;
begin
  if not Assigned(FRadioGroupSituacao) then
    Exit;

  Definitions := GetStatusDefinitions;
  Sorted := SortStatusDefinitionsByDisplayOrder(Definitions);
  if Assigned(FPersistence) then
    CurrentStatus := Status
  else
    CurrentStatus := FirstStatusId(Definitions);

  FRadioGroupSituacao.Items.Clear;
  for StatusIndex := 0 to High(Sorted) do
    FRadioGroupSituacao.Items.AddObject(Sorted[StatusIndex].Caption,
      TObject(PtrUInt(Sorted[StatusIndex].Id)));

  FRadioGroupSituacao.ItemIndex := DisplayIndexOfStatusId(Definitions,
    CurrentStatus);
  if (FRadioGroupSituacao.ItemIndex < 0) and
    (FRadioGroupSituacao.Items.Count > 0) then
    FRadioGroupSituacao.ItemIndex := 0;
end;

procedure TProject.SetStatus(const AValue: Integer);
var
  NormalizedStatus: Integer;
  Definitions: TStatusDefinitions;
begin
  Definitions := GetStatusDefinitions;
  if IsValidStatusId(Definitions, AValue) then
    NormalizedStatus := AValue
  else
    NormalizedStatus := FirstStatusId(Definitions);
  if FClosing or (FPersistence.CurrentStatus = NormalizedStatus) then
    exit;
  FPersistence.WriteCurrentStatus(NormalizedStatus);
  UpdateTree(ID);
end;

procedure TProject.HighlightStrongs(syntagm: TSyntagm);
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

procedure TProject.MarkNodeAsAIPending(node: TTreeNode; const ref: string);
begin
  // No-op: Modal handles its own tree updates
end;

procedure TProject.MarkNodeAsAIProcessing(const ref: string);
begin
  // No-op: Modal handles its own tree updates
end;

procedure TProject.RestoreNodeStatus(const ref: string);
begin
  // No-op: Modal handles its own tree updates
end;

procedure TProject.ClearAllAINodeStates;
begin
  // No-op: Modal handles its own tree updates
end;

function TProject.FindTreeNodeByReference(const ref: string): TTreeNode;
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

function TProject.CreateBibleTreeContextMenu: TPopupMenu;

  procedure AddSubItemForStatus(item: TMenuItem; status: Integer);
  var
    subitem: TMenuItem;
  begin
    subitem := TMenuItem.Create(item);
    with subitem do
    begin
      Caption := GetStatusDescription(status);
      OnClick := @OnBibleTreePopupItemClick;
      ImageIndex := status;
      Tag := status;
    end;
    item.Add(subitem);
  end;

var
  item: TMenuItem;
  statusIndex: Integer;
  StatusDefinitions: TStatusDefinitions;
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
  StatusDefinitions := SortStatusDefinitionsByDisplayOrder(GetStatusDefinitions);
  for statusIndex := 0 to High(StatusDefinitions) do
    AddSubItemForStatus(item, StatusDefinitions[statusIndex].Id);

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



procedure TProject.OnAICancelClick(Sender: TObject);
begin
  if Assigned(FAIManager) then
  begin
    FAIManager.CancelAll;
    ClearAllAINodeStates; // Restore node icons
  end;
end;

procedure TProject.ResetAIBatchManager;
begin
  if Assigned(FAIManager) then
  begin
    FAIManager.CancelAll;
    ClearAllAINodeStates;
    FreeAndNil(FAIManager);
  end;
end;

procedure TProject.OnAIBatchClick(Sender: TObject);
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
      if QuestionDlg(SConfirmation, Format(SConfirmApplyAIToBook, [FBibleTreePointedNode.Text]),
        mtConfirmation, [mrYes, SButtonYes, 'IsDefault', mrNo, SButtonNo, 'IsCancel'], 0) = mrNo then
        Exit;
    end
    else if FBibleTreePointedNode.Level = 1 then // chapter
    begin
      if QuestionDlg(SConfirmation, Format(SConfirmApplyAIToChapter,
        [Format('%s %s', [FBibleTreePointedNode.Parent.Text, FBibleTreePointedNode.Text])]),
        mtConfirmation, [mrYes, SButtonYes, 'IsDefault', mrNo, SButtonNo, 'IsCancel'], 0) = mrNo then
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

    maxPar := StrToIntDef(GetInfo(AI_COMMON_CONFIG.MaxParallel), 2);

    if GetInfo(AI_COMMON_CONFIG.Model).IsEmpty or GetInfo(AI_COMMON_CONFIG.ApiKey).IsEmpty then
    begin
      ShowMessage(SAIConfigurationMissing);
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

    if SameText(flowId, 'translation') and (list.Count = 0) and
       (batchPrep.SkippedCount > 0) then
    begin
      ShowMessage(SNoEmptyTranslationsInScope);
      Exit;
    end;

    flow := batchPrep.Flow;
    FAIManager := batchPrep.Manager;
    if flow = nil then Exit;
    if not Assigned(FAIManager) then Exit;

    previewOptions.TokenizerVersion := TokenizerVersion;
    previewOptions.SourceFont := GetTextFont(tbOrigem);
    previewOptions.DestinationFont := GetTextFont(tbDestino);
    previewOptions.BoldStrongs := PalavrasComStrongEmNegrito;
    previewOptions.StrongsCountMode := MostrarQtdStrongs;

    // Use global form instance instead of creating new one
    if Assigned(FrmAIProcessing) then
    begin
      if FrmAIProcessing.StartProcessing(FAIManager, flow, list, previewOptions, @ApplyAcceptedBatch) then
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

procedure TProject.UpdateReference(ref: string);
begin
  Sscanf(ref, '%d,%d,%d', [@FReference.BookID, @FReference.Chapter, @FReference.Verse]);
  FReference.Book := LocalizedBookName(FEscopo,
    FReference.BookID - OffsetLivros[FEscopo] - 1);
end;

function TProject.BibleTreeNodeToReference(const node: TTreeNode): TReference;
var
  ReferenceID: string;
begin
  ReferenceID := FBibleTreeCoordinator.NodeToReferenceID(FEscopo, node);
  Sscanf(ReferenceID, '%d,%d,%d', [@Result.BookID, @Result.Chapter, @Result.Verse]);
  result.Book    := LocalizedBookNameByID(result.BookID);
end;

function TProject.GetModificado: boolean;
begin
  Result := Assigned(FCurrentVerseWorkspace) and
    FCurrentVerseWorkspace.HasPendingChanges(FPersistence, FVerseMemo,
      FMemoComentarios, FAVersiculo, FACamposTexto, Ord(tbOrigem));
end;

function TProject.GetCaminho: string;
begin
  if Assigned(FPersistence) then
    Result := FPersistence.FileName
  else
    Result := '';
end;

function TProject.GetChapterViewText: TTipoTextoBiblico;
var
  TextIndex: Integer;
begin
  TextIndex := FConfig.ReadChapterViewText(Ord(tbDestino));
  if (TextIndex < Ord(tbOrigem)) or (TextIndex > Ord(tbInterlinear)) then
    TextIndex := Ord(tbDestino);
  Result := TTipoTextoBiblico(TextIndex);
end;

function TProject.GetChapterViewMode: TChapterViewMode;
var
  ModeIndex: Integer;
begin
  ModeIndex := FConfig.ReadChapterViewMode(Ord(cvmChapterText));
  if (ModeIndex < Ord(Low(TChapterViewMode))) or
     (ModeIndex > Ord(High(TChapterViewMode))) then
    ModeIndex := Ord(cvmChapterText);
  Result := TChapterViewMode(ModeIndex);
end;

function TProject.GetChapterText: TStringList;
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
            begin
              // Chapter rendering is a secondary view and must never block
              // saving or correcting the selected Verse Pair. The rebuild
              // follows the same policy and skips corrupt canonical rows.
              result := FPersistence.CurrentText(FACamposTexto[tbOrigem]);
              DebugLn(Format('Ignoring invalid associations in chapter view ' +
                '[%s]: %s', [FPersistence.CurrentPairId,
                corruptionDetails]));
            end;
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
            begin
              result := FPersistence.CurrentText(FACamposTexto[tbOrigem]);
              DebugLn(Format('Ignoring invalid associations in chapter view ' +
                '[%s]: %s', [FPersistence.CurrentPairId,
                corruptionDetails]));
            end;
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
  bkch: string;
  rulesRx: TRegexList;
  replaceTo: TStringList;
begin
  bkch := Format('%d,%d,', [BookID, Chapter]);
  result := TStringList.Create;

  rulesRx := nil;
  replaceTo := nil;
  try
    if ChapterViewText = tbInterlinear then
      CompileInterlinearVerseRules(rulesRx, replaceTo, InterlinearMode, false);

    try
      StartScrollingSession;
      try
        RewindChapter;
        while not FPersistence.EOF and GetID().StartsWith(bkch) do
        begin
          result.Add(GetText(rulesRx, replaceTo));
          NextVerse;
        end;
      finally
        FinishScrollingSession;
      end;
    except
      FreeAndNil(result);
      raise;
    end;
  finally
    rulesRx.Free;
    replaceTo.Free;
  end;
end;

procedure TProject.CompileInterlinearVerseRules(var rulesRx: TRegexList; var replaceTo: TStringList; mode: TInterlinearMode; exporting: boolean);
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

function TProject.GetComments: string;
begin
  Result := FPersistence.CurrentComments;
end;

function TProject.GetComments(ref: string): string;
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

function TProject.GetStatus(ref: string): Integer;
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

procedure TProject.CountVersesByStatus(out Counts: TStatusCounts);

  procedure IncStatusCount(StatusId: Integer);
  var
    CountIndex: Integer;
  begin
    for CountIndex := 0 to High(Counts) do
      if Counts[CountIndex].StatusId = StatusId then
      begin
        Inc(Counts[CountIndex].Count);
        Exit;
      end;

    SetLength(Counts, Length(Counts) + 1);
    Counts[High(Counts)].StatusId := StatusId;
    Counts[High(Counts)].Count := 1;
  end;

var
  Definitions: TStatusDefinitions;
  VerseData: TVerseData;
  StatusIndex: Integer;
begin
  Counts := nil;
  Definitions := SortStatusDefinitionsByDisplayOrder(GetStatusDefinitions);
  SetLength(Counts, Length(Definitions));
  for StatusIndex := 0 to High(Definitions) do
  begin
    Counts[StatusIndex].StatusId := Definitions[StatusIndex].Id;
    Counts[StatusIndex].Count := 0;
  end;

  if not Assigned(FVerseRepository) then
    Exit;

  StartScrollingSession;
  try
    FVerseRepository.First;
    while not FVerseRepository.EOF do
    begin
      VerseData := FVerseRepository.ReadCurrentVerseData;
      IncStatusCount(VerseData.Status);
      FVerseRepository.Next;
    end;
  finally
    FinishScrollingSession;
  end;
end;

function TProject.CountVersesWithStatus(StatusId: Integer): Integer;
var
  Counts: TStatusCounts;
  CountIndex: Integer;
begin
  Result := 0;
  CountVersesByStatus(Counts);
  for CountIndex := 0 to High(Counts) do
    if Counts[CountIndex].StatusId = StatusId then
      Exit(Counts[CountIndex].Count);
end;

procedure TProject.RemapStatus(OldStatusId, NewStatusId: Integer);
begin
  if OldStatusId = NewStatusId then
    Exit;

  StartScrollingSession;
  try
    FVerseRepository.First;
    while not FVerseRepository.EOF do
    begin
      if FPersistence.CurrentStatus = OldStatusId then
        FPersistence.WriteCurrentStatus(NewStatusId);
      FVerseRepository.Next;
    end;
  finally
    FinishScrollingSession;
  end;

  RefreshStatusControl;
  UpdateTree;
end;

procedure TProject.SetTextDescription(text: TTipoTextoBiblico;
  description: string);
begin
  FConfig.WriteTextDescription(Integer(text), description);
end;

function TProject.GetTextDescription(text: TTipoTextoBiblico): string;
begin
  Result := FConfig.ReadTextDescription(Integer(text));
end;

procedure TProject.SetTextDirection(text: TTipoTextoBiblico;
  direction: TTextDirection);
begin
  if text = tbNone then
    Exit;
  if Assigned(FConfig) and Assigned(FPersistence) then
    FConfig.WriteTextDirection(Integer(text), direction);
  ApplyTextDirectionToVerse(text, direction);
end;

function TProject.IsVerseObjectText(text: TTipoTextoBiblico): Boolean;
begin
  Result := (Ord(text) >= Ord(Low(FAVersiculo))) and
    (Ord(text) <= Ord(High(FAVersiculo)));
end;

procedure TProject.ApplyTextDirectionToVerse(text: TTipoTextoBiblico;
  Direction: TTextDirection);
var
  SavedHandler: TOnTextDirectionChangeEvent;
  VerseObject: TVerse;
begin
  if not IsVerseObjectText(text) then
    Exit;
  VerseObject := FAVersiculo[text];
  if not Assigned(VerseObject) then
    Exit;

  SavedHandler := VerseObject.OnTextDirectionChange;
  VerseObject.OnTextDirectionChange := nil;
  try
    VerseObject.BaseDirection := Direction;
  finally
    VerseObject.OnTextDirectionChange := SavedHandler;
  end;
end;

function TProject.GetTextDirection(text: TTipoTextoBiblico): TTextDirection;
var
  ModuleProperties: string;
begin
  if not Assigned(FConfig) then
    Exit(tdLeftToRight);

  case text of
    tbOrigem:
      ModuleProperties := FConfig.ReadString('propriedades.origem');
    tbDestino:
      ModuleProperties := FConfig.ReadString('propriedades.destino');
  else
    ModuleProperties := '';
  end;
  Result := FConfig.ReadTextDirection(Integer(text),
    ParseTheWordR2LProperties(ModuleProperties));
end;

procedure TProject.Vacuum;
begin
  if MessageDlg(SConfirmation, SVaccumProjectConfirmation, mtConfirmation, [mbYes, mbNo],0) = mrNo then
    exit;

  FormPleaseWait.BeginOperation(Application.MainForm, SVacuumProgressTitle,
    SVacuumProgressMessage, 0, FFileName);
  try
    if not CloseProject(true) then
      Exit;
    with TSqlite3Dataset.Create(nil) do
    begin
      FileName:=FFileName;
      ExecSQL('VACUUM;');
      Free;
    end;
    OpenProject(FFileName);
  finally
    FormPleaseWait.FinishOperation;
  end;
  ShowMessage(SVacuumDone);
end;

function TProject.GetID: string;
begin
  Result := FPersistence.CurrentPairId;
end;

function TProject.GetCanGoBack: Boolean;
begin
  Result := Assigned(FNavigationHistory) and FNavigationHistory.CanGoBack;
end;

function TProject.GetCanGoForward: Boolean;
begin
  Result := Assigned(FNavigationHistory) and FNavigationHistory.CanGoForward;
end;

function TProject.GetInterlinearMode: TInterlinearMode;
begin
  Result := TInterlinearMode(FConfig.ReadInterlinearMode(Ord(imInterlinear)));
end;

function TProject.CreateTableDataset(db, tableName, keyField: string): TSqlite3Dataset;
begin
  Result := TProjectPersistence.CreateTableDataset(db, tableName, keyField);
end;

function TProject.CreateQueryObject(db: string): TSQLQuery;
begin
  Result := TProjectPersistence.CreateQueryObject(db);
end;

function TProject.InsertInfo(info, value: string): boolean;
begin
  Result := FPersistence.InsertInfo(info, value);
end;

function TProject.UpdateInfo(info, value: string): boolean;
begin
  Result := FPersistence.UpdateInfo(info, value);
end;

function TProject.UpdateConfigTable(table: TSqlite3Dataset; name, value: string): boolean;
begin
  Result := TProjectPersistence.UpdateConfigTable(table, name, value);
end;

function TProject.ReadInfo(info: string): string;
begin
  if Assigned(FPersistence) then
    Result := FPersistence.ReadInfo(info)
  else
    Result := '';
end;

function TProject.TrySetVerseText(versiculo: string;
  texto: TTipoTextoBiblico; replace: boolean;
  out ErrorDetails: string): Boolean;
var
  PreviousText: string;
  PreviousPairs: string;
  MemoryInputChanged: Boolean;
begin
  ErrorDetails := '';
  PreviousText := FPersistence.CurrentText(FACamposTexto[texto]);
  PreviousPairs := FPersistence.CurrentPairs;
  Result := FCurrentVerseWorkspace.SetCurrentVerseText(FPersistence,
    FVerseRenderBuffer,
    FACamposTexto, versiculo, Ord(texto), Ord(tbOrigem), Ord(tbDestino),
    replace, StatusAutomationEnabled, ErrorDetails);

  MemoryInputChanged := (PreviousText <>
    FPersistence.CurrentText(FACamposTexto[texto])) or
    (PreviousPairs <> FPersistence.CurrentPairs);
  if Result and MemoryInputChanged and TextTypeParticipatesInAssociations(
    texto) and Assigned(FAssociationMemory) then
  begin
    FAssociationMemory.MarkSourceDirty;
    FAssociationMemory.InvalidateReplay;
  end;
end;

procedure TProject.SetVerseText(versiculo: string;
  texto: TTipoTextoBiblico; replace: boolean);
var
  ErrorDetails: string;
begin
  ErrorDetails := '';
  if not TrySetVerseText(versiculo, texto, replace, ErrorDetails) and
    not ErrorDetails.IsEmpty then
    MessageDlg(SError, SCorruptedData + #13#10#13#10 +
      FormattedReference + #13#10 + ErrorDetails, mtError, [mbOK], 0);
end;

procedure TProject.ApplyVerseText(const newText: string; texto: TTipoTextoBiblico; replace: Boolean);
begin
  SetVerseText(newText, texto, replace);
end;

procedure TProject.BeforeVerseScroll(DataSet: TDataSet);
var
  CacheWasCurrent: Boolean;
  MemoryInputsChanged: Boolean;
begin
  if not FScrollEventsEnabled or not Assigned(FAVersiculo[tbOrigem]) then
    exit;

  //starttime := getTickCount;

  CacheWasCurrent := Assigned(FAssociationMemory) and
    not FAssociationMemory.NeedsRebuild;
  try
    MemoryInputsChanged := FCurrentVerseWorkspace.SaveBeforeScroll(
      FPersistence, FSuggestionManager,
      FVerseMemo, FMemoComentarios, FAVersiculo, FACamposTexto,
      Ord(tbOrigem), Ord(tbDestino), FAssociationMemory);
  except
    FAssociationMemoryLiveEditPending := True;
    if Assigned(FAssociationMemory) then
    begin
      FAssociationMemory.MarkCacheOutdated;
      FAssociationMemory.InvalidateReplay;
      try
        FAssociationMemory.MarkSourceDirty;
      except
        on RecoveryError: Exception do
          DebugLn('Association-memory recovery after autosave failed: ' +
            RecoveryError.Message);
      end;
    end;
    raise;
  end;
  if MemoryInputsChanged and Assigned(FAssociationMemory) then
  begin
    FAssociationMemory.MarkSourceDirty;
    if FAssociationMemoryRebuildFailed or not CacheWasCurrent then
      FAssociationMemory.MarkCacheOutdated
    else
      FAssociationMemory.MarkCacheSynchronized;
  end;
  FAssociationMemoryLiveEditPending := False;
  FAssociationMemorySpeculativeEditPending := False;

  //DebugLn('  TProject.BeforeVerseScroll: %d milliseconds', [getTickCount-starttime]);
end;

procedure TProject.RefreshAutomaticNoteStarts;
var
  Count, Index: Integer;
  NoteStarts: TAutomaticRFNoteCountArray;
  TextIndexes: TIntegerArray;
  TextType: TTipoTextoBiblico;
  TextTypes: array of TTipoTextoBiblico;
begin
  Count := 0;
  SetLength(TextIndexes, Length(FAVersiculo));
  SetLength(TextTypes, Length(FAVersiculo));
  for TextType := Low(FAVersiculo) to High(FAVersiculo) do
    if Assigned(FAVersiculo[TextType]) then
    begin
      TextIndexes[Count] := FACamposTexto[TextType];
      TextTypes[Count] := TextType;
      Inc(Count);
    end;
  SetLength(TextIndexes, Count);
  SetLength(TextTypes, Count);
  if Count = 0 then
    Exit;

  NoteStarts := FVerseRepository.CountAutomaticRFNotesBeforeMany(GetID(),
    TextIndexes);
  for Index := 0 to Count - 1 do
    FAVersiculo[TextTypes[Index]].AutomaticNoteStart := NoteStarts[Index];
end;

procedure TProject.AfterVerseScroll(DataSet: TDataSet);
var
  e: TOnNovoVersiculoEvent;
  loadResult: TCurrentVerseLoadResult;
  //starttime: DWord;
begin
  if not FScrollEventsEnabled then
    exit;

  UpdateReference(GetID());

  //starttime := getTickCount;

  loadResult := FCurrentVerseWorkspace.LoadCurrentVerse(FPersistence,
    FAVersiculo, FACamposTexto, FormattedReference, GetID(),
    Ord(tbOrigem), Ord(tbDestino));

  RefreshAutomaticNoteStarts;

  if assigned(FAVersiculo[tbOrigem]) and assigned(FAVersiculo[tbDestino]) then
  begin
    try
      FCurrentVerseWorkspace.ApplyLoadedAssociations(FAVersiculo[tbOrigem],
        loadResult.Associations, FVerseRenderBuffer);
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

  FNavigationHistory.Visit(ID);

  FBibleTreeCoordinator.SelectReference(FEscopo, ID);

  if assigned(FMemoComentarios) then
  begin
    if FMemoComentarios is TCommentsView then
      TCommentsView(FMemoComentarios).Text := loadResult.Comments
    else if FMemoComentarios is TMemo then
      TMemo(FMemoComentarios).Text := loadResult.Comments;
  end;

  if assigned(FRadioGroupSituacao) then
  begin
    FRadioGroupSituacao.ItemIndex := DisplayIndexOfStatusId(
      GetStatusDefinitions, loadResult.Status);
    if (FRadioGroupSituacao.ItemIndex < 0) and
      (FRadioGroupSituacao.Items.Count > 0) then
      FRadioGroupSituacao.ItemIndex := 0;
  end;

  if loadResult.DestinationTextEmpty and Assigned(FAVersiculo[tbDestino]) then
    // Open an empty destination verse for editing only when a visual verse exists.
    OnDblClickVersiculo(FAVersiculo[tbDestino].Panel);

  for e in FOnNovoVersiculo do
    e(self);

  if fautosave and not fexportando then
    SaveProject;

  //DebugLn('  TProject.PosRolagemVersiculo: %d milliseconds', [getTickCount-starttime]);
end;

procedure TProject.SavePairs;
begin
  FCurrentVerseWorkspace.SavePairs(FPersistence, FAVersiculo[tbOrigem]);
end;

procedure TProject.SaveVersePairs(Verse: TVerse);
begin
  // Save pairs from a specific verse object (works for temp or visual verses)
  // Assumes database cursor is already positioned at the correct record
  if Verse = nil then Exit;
  FPersistence.WriteCurrentPairs(Verse.Pairs);
end;

procedure TProject.SaveText;
begin
  FCurrentVerseWorkspace.SaveTexts(FPersistence, FAVersiculo, FACamposTexto);
end;

procedure TProject.SaveAssociations;
var
  CacheWasCurrent: Boolean;
begin
  CacheWasCurrent := Assigned(FAssociationMemory) and
    not FAssociationMemory.NeedsRebuild;
  if FCurrentVerseWorkspace.SaveAssociations(FSuggestionManager,
    FAVersiculo[tbOrigem], FAVersiculo[tbDestino], FAssociationMemory,
    FPersistence.CurrentPairId) and
    Assigned(FAssociationMemory) then
  begin
    FAssociationMemory.MarkSourceDirty;
    if FAssociationMemoryRebuildFailed or not CacheWasCurrent then
      FAssociationMemory.MarkCacheOutdated
    else
      FAssociationMemory.MarkCacheSynchronized;
  end;
end;

procedure TProject.ReconcileEditedVerseAssociationMemory;
begin
  FAssociationMemoryLiveEditPending := False;
  try
    SaveAssociations;
    FAssociationMemoryLiveEditPending :=
      Assigned(FAssociationMemory) and FAssociationMemory.NeedsRebuild;
  except
    on E: Exception do
    begin
      FAssociationMemoryLiveEditPending := True;
      if Assigned(FAssociationMemory) then
        try
          FAssociationMemory.MarkCacheOutdated;
          FAssociationMemory.InvalidateReplay;
          FAssociationMemory.MarkSourceDirty;
        except
          on RecoveryError: Exception do
            DebugLn('Association-memory recovery after a live edit failed: ' +
              RecoveryError.Message);
        end;
      DebugLn('Association-memory live edit reconciliation failed: ' +
        E.Message);
    end;
  end;
end;

function TProject.ReconcileChangedVerseAssociations: Boolean;
begin
  Result := False;
  if not Assigned(FAVersiculo[tbOrigem]) or
     not FCurrentVerseWorkspace.HasAssociationChanges(
       FAVersiculo[tbOrigem]) then
    Exit;

  Result := True;
  FAssociationMemorySpeculativeEditPending := True;
  ReconcileEditedVerseAssociationMemory;
end;

procedure TProject.RemoveAssociations;
var
  CacheWasCurrent: Boolean;
begin
  CacheWasCurrent := Assigned(FAssociationMemory) and
    not FAssociationMemory.NeedsRebuild;
  FSuggestionManager.BeginRebuild;
  try
    FCurrentVerseWorkspace.RemoveOldAssociations(FSuggestionManager,
      FAVersiculo[tbOrigem], FAVersiculo[tbDestino]);
    FSuggestionManager.CommitRebuild;
    if Assigned(FAssociationMemory) then
    begin
      FAssociationMemory.MarkSourceDirty;
      if FAssociationMemoryRebuildFailed or not CacheWasCurrent then
        FAssociationMemory.MarkCacheOutdated
      else
        FAssociationMemory.MarkCacheSynchronized;
      FAssociationMemory.InvalidateReplay;
    end;
  except
    FSuggestionManager.RollbackRebuild;
    raise;
  end;
end;

procedure TProject.SintagmaOnMouseEnter(Sender: TSyntagm);
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

procedure TProject.SintagmaOnMouseLeave(Sender: TSyntagm);
var
  hide: boolean;
  v: TTipoTextoBiblico;
begin
  FTemporizador.Enabled := false;
  FTemporizador.Tag := 0;
  hide := true;

  case FPopupTrigger of
    ptMouseHover:     hide := GetKeyState(VK_CONTROL) and $8000 = 0;
    ptAltMouseHover:  hide := GetKeyState(VK_MENU   ) and $8000 = 0;
    ptCtrlMouseHover: hide := GetKeyState(VK_CONTROL) and $8000 = 0;
  end;

  if hide then
    FrmDictionaryPopup.HidePopup;

  { hide strong hightlights }
  for v:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[v]) then
        FAVersiculo[v].DisableStrongHighlight;
end;

procedure TProject.SintagmaOnClick(Sender: TSyntagm);
begin
  if not FVerseMemo.TryDeactivate then
    Exit;

  ReconcileChangedVerseAssociations;

  if assigned(FOnSintagmaClick) then
    FOnSintagmaClick(Sender);

  if StatusAutomationEnabled and assigned(FRadioGroupSituacao) and
     assigned(FAVersiculo[tbOrigem]) and
     ((Sender.VerseRef = FAVersiculo[tbOrigem]) or (Sender.VerseRef = FAVersiculo[tbDestino])) then
  begin
    if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AssociationProgress > 0) then
      FRadioGroupSituacao.ItemIndex := 1 // associando
    else if (FRadioGroupSituacao.ItemIndex > 1) and (FAVersiculo[tbOrigem].AssociationProgress = 0) then
      FRadioGroupSituacao.ItemIndex := 0 // sem associação
    else if (FRadioGroupSituacao.ItemIndex = 1) and ((FAVersiculo[tbOrigem].AssociationProgress = 1) or (FAVersiculo[tbDestino].AssociationProgress = 1)) then
      FRadioGroupSituacao.ItemIndex := 3; // associado
  end;
end;

procedure TProject.OnExportText(Sender: TVerse);
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

function TProject.FindVerseType(Verse: TVerse; out VerseType: TTipoTextoBiblico): Boolean;
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

procedure TProject.OnTextDirectionChange(Sender: TVerse;
  Direction: TTextDirection);
var
  VerseType: TTipoTextoBiblico;
begin
  if FindVerseType(Sender, VerseType) then
    SetTextDirection(VerseType, Direction);
end;

procedure TProject.OnVersePopup(Sender: TObject);
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
  CanUseAssociationExample := IsPrimaryVerse and Assigned(ClickedVerse.PairedVerse) and ClickedVerse.HasAssociations;
  CanUseTranslationExample := IsPrimaryVerse and Assigned(ClickedVerse.PairedVerse) and (Trim(ClickedVerse.PairedVerse.PlainText) <> '');

  if Assigned(FUseAsExampleMenuItems[VerseType]) then
  begin
    FUseAsExampleMenuItems[VerseType].Visible := CanUseAssociationExample or CanUseTranslationExample;
    FUseAsAssociationExampleMenuItems[VerseType].Enabled := CanUseAssociationExample;
    FUseAsTranslationExampleMenuItems[VerseType].Enabled := CanUseTranslationExample;
  end;

  if Assigned(FCopyAssociationExampleMenuItems[VerseType]) then
    FCopyAssociationExampleMenuItems[VerseType].Visible := ClickedVerse.HasAssociations;
end;

procedure TProject.OnUseAssociationAsExample(Sender: TObject);
begin
  IncludeCurrentVerseAsFlowExample('association');
end;

procedure TProject.OnUseTranslationAsExample(Sender: TObject);
begin
  IncludeCurrentVerseAsFlowExample('translation');
end;

procedure TProject.OnCopyAssociationExample(Sender: TObject);
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
    [originalVerse.PlainText, destinationVerse.PlainText, originalVerse.GetAssociationsJSON]);
  Clipboard.AsText := example;
end;

procedure TProject.OnSwitchDestinationAndReference1Texts(Sender: TObject);
var
  destinationText: string;
begin
  if (not Assigned(FAVersiculo[tbDestino])) or (not Assigned(FAVersiculo[tbConsulta1])) then
    Exit;

  if MessageDlg(SSwitchDestinationAndReference1Texts,
      Format(SSwitchDestinationAndReference1TextsConfirmation,
        [FormattedReference]), mtConfirmation, [mbYes, mbCancel], 0) <> mrYes then
    Exit;

  destinationText := FAVersiculo[tbDestino].XML;
  FAVersiculo[tbDestino].ChangeText(FAVersiculo[tbConsulta1].XML);
  FAVersiculo[tbConsulta1].Text := '(KJF original) ' + destinationText;
end;

procedure TProject.AttachProjectVersePopupMenu(texto: TTipoTextoBiblico);
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

procedure TProject.OnMudancaVersiculo(Sender: TObject; Node: TTreeNode);
begin
  if not FScrollEventsEnabled or not assigned(FTblPares) or FClosing or not assigned(Node) then
    exit;
  GoToReference(FBibleTreeCoordinator.NodeToReferenceID(FEscopo, Node));
end;

function TProject.OnChangeTextVersiculo(Sender: TVerseMemo;
  const ATrace: TVerseEditTrace;
  out AImpact: TAssociationEditImpact): Boolean;
var
  editedIndex: Integer;
begin
  Result := False;
  AImpact.PreservedMembers := 0;
  AImpact.RemovedMembers := 0;
  AImpact.UsedLegacyFallback := False;
  AImpact.ErrorMessage := '';
  if ContainsText(Sender.Text, #13) or ContainsText(Sender.Text, #10) then
  begin
    AImpact.ErrorMessage := SLineBreaksInEditor;
    exit;
  end;

  Result := FCurrentVerseWorkspace.ApplyEditedVerseText(FAVersiculo,
    Sender.Verse, Sender.Text, ATrace, editedIndex, AImpact);
  if not Result then
    Exit;
  if editedIndex in [Ord(tbOrigem), Ord(tbDestino)] then
  begin
    FAssociationMemorySpeculativeEditPending := True;
    ReconcileEditedVerseAssociationMemory;
    NotifyNewVerseObserversAfterEdit;
  end;

  //MessageDlg('Versículo modificado', format('Versiculo modificado: %s', [Sender.Text]), mtError, [mbOK], 0);
end;

procedure TProject.NotifyNewVerseObserversAfterEdit;
begin
  if FNewVerseNotificationQueued then
    Exit;
  FNewVerseNotificationQueued := True;
  Application.QueueAsyncCall(@DispatchNewVerseObserversAfterEdit, 0);
end;

procedure TProject.DispatchNewVerseObserversAfterEdit(Data: PtrInt);
var
  Observer: TOnNovoVersiculoEvent;
begin
  FNewVerseNotificationQueued := False;
  { The editor has accepted and closed its session before this queued refresh
    runs. Observers may therefore save/navigate without re-entering the old
    edit baseline. }
  for Observer in FOnNovoVersiculo do
    try
      Observer(Self);
    except
      on E: Exception do
        DebugLn('Post-commit Verse observer failed: ' + E.Message);
    end;
end;

procedure TProject.OnExibirDefinicao(Sender: TObject);
var
  StrongEntries: TDictionaryEntryArray;
  MorphologyEntries: TDictionaryEntryArray;
  EntryIndex: Integer;
  Candidate: TSyntagm;
  Timer: TTimer;
  v: TTipoTextoBiblico;
  point: TPoint;
  s: TSyntagm;
  rtf: string;
begin
  if not (Sender is TTimer) then
    Exit;
  Timer := TTimer(Sender);
  Timer.Enabled := False;
  Candidate := TSyntagm(Timer.Tag);
  Timer.Tag := 0;

  if not Assigned(Candidate) then
    Exit;
  if frmDictionaryPopup.Visible then
    exit;

  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    if Assigned(FAVersiculo[v]) and Assigned(FAVersiculo[v].Syntagms) and
      (FAVersiculo[v].Syntagms.IndexOf(Candidate) >= 0) then
    begin
      s := Candidate;
      FDictionaryManager.CollectPopupEntries(s, Ord(v), StrongEntries, MorphologyEntries);

      for EntryIndex := Low(MorphologyEntries) to High(MorphologyEntries) do
        FrmDictionaryPopup.AddMorphology(MorphologyEntries[EntryIndex].Key,
          MorphologyEntries[EntryIndex].Definition);

      for EntryIndex := Low(StrongEntries) to High(StrongEntries) do
        FrmDictionaryPopup.AddStrong(StrongEntries[EntryIndex].Key,
          StrongEntries[EntryIndex].Definition);

      if v = tbOrigem then
      begin
        if EnsureAssociationMemoryFresh then
          Exit;
        rtf := TDictionaryManager.FormatRTF(FSuggestionManager.GetTranslationAlternatives(s));
        if not rtf.IsEmpty then
          FrmDictionaryPopup.AddStrong(STranslationMemory, rtf);
      end;

      if (FrmDictionaryPopup.Strongs.Count > 0) or (FrmDictionaryPopup.Morphologies.Count > 0) then
      begin
        if FAVersiculo[v].TryGetSyntagmScreenAnchor(s, point) then
          FrmDictionaryPopup.ShowAt(point.x, point.y);
      end;
      break;
    end;
  end;
end;

procedure TProject.OnDblClickVersiculo(Sender: TObject);
var
  Container: TScrollBox;
  TextType: TTipoTextoBiblico;
  VerseView: TVerse;
begin
  if not Ativo or not (Sender is TScrollBox) then
    exit;

  Container := TScrollBox(Sender);
  VerseView := nil;
  for TextType := Low(FAVersiculo) to High(FAVersiculo) do
    if Assigned(FAVersiculo[TextType]) and
      (FAVersiculo[TextType].Panel = Container) then
    begin
      VerseView := FAVersiculo[TextType];
      Break;
    end;
  if not Assigned(VerseView) then
    Exit;

  FVerseMemo.Activate(Container, VerseView, VerseView.AutomaticNoteStart);
end;

procedure TProject.OnTranslatorNoteClick(Sender: TVerse; ANoteId,
  ARevision: QWord);
var
  TextType: TTipoTextoBiblico;
begin
  if not Ativo or not Assigned(Sender) or (ANoteId = 0) or
     not FindVerseType(Sender, TextType) or not Assigned(Sender.Panel) then
    Exit;

  Sender.DismissTranslatorNotePreview;
  FVerseMemo.Activate(Sender.Panel, Sender, Sender.AutomaticNoteStart,
    ANoteId, ARevision);
end;

procedure TProject.OnBibleTreePopupItemClick(Sender: TObject);

  function SameReference(a, b: TReference): boolean;
  begin
    result := (a.Book = b.Book) and (a.BookID = b.BookID) and (a.Chapter = b.Chapter) and (a.Verse = b.Verse);
  end;

  procedure SetNodeStatus(node: TTreeNode; status: Integer);
  begin
    GoToReference(FBibleTreeCoordinator.NodeToReferenceID(FEscopo, node));
    SetStatus(status);
  end;

  procedure SetSubTreeStatus(parent: TTreeNode; status: Integer);
  var
    node: TTreeNode;
  begin
    node := parent.GetFirstChild;
    while assigned(node) do
    begin
      if node.HasChildren then
        SetSubTreeStatus(node, status)
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
    if FBibleTreePointedNode.HasChildren then
    begin
      SetSubTreeStatus(FBibleTreePointedNode, item.Tag)
    end
    else
    begin
      SetNodeStatus(FBibleTreePointedNode, item.Tag);
    end;
  finally
    FinishScrollingSession;
  end;

  RefreshUI;
end;

procedure TProject.OnBibleTreeContextPopup(Sender: TObject; MousePos: TPoint;
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
          if i = 0 then
            m.Caption := Format(SMarkAsFmt, [refStr]);
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

function TProject.AssignStrongDictionary(dic: string; t: TTipoTextoBiblico): boolean;
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

function TProject.AssignMorphologyDictionary(dic: string; t: TTipoTextoBiblico): boolean;
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

procedure TProject.UpdateTree;
var
  OldScrollEventsEnabled: Boolean;
  OldExporting: Boolean;
begin
  if not assigned(FArvore) then exit;
  OldScrollEventsEnabled := FScrollEventsEnabled;
  OldExporting := FExportando;
  StartScrollingSession;
  try
    FBibleTreeCoordinator.RecomputeAllStatuses(FEscopo,
      TProjectVerseStatusCursor.Create(Self), GetStatusDefinitions,
      @GetAggregateStatusImageIndex);
  finally
    try
      FinishScrollingSession;
    finally
      FScrollEventsEnabled := OldScrollEventsEnabled;
      FExportando := OldExporting;
    end;
  end;
end;

procedure TProject.UpdateTree(id: string);
var
  ReferenceStatus: Integer;
begin
  if not assigned(FArvore) or FClosing then
    exit;

  if id = ID then
    ReferenceStatus := Status
  else
    ReferenceStatus := GetStatus(id);
  FBibleTreeCoordinator.UpdateReferenceStatus(FEscopo, id, ReferenceStatus,
    GetStatusDefinitions, @GetAggregateStatusImageIndex);
end;

constructor TProject.Create;
var
  v: TTipoTextoBiblico;
begin
  for v:=low(FAVersiculo) to high(FAVersiculo) do
  begin
    FAVersiculo[v]   := nil;
    FACamposTexto[v] := 0;
  end;

  FOnNovoVersiculo    := TOnNovoVersiculoEvents.Create;
  FNewVerseNotificationQueued := False;
  FOnAlterarVersiculo := nil;
  FSuggestionManager  := nil;
  FAIManager          := nil;
  FAIProcessingNodes  := TAINodeInfoList.Create(True); // owns objects
  FAIBatchCompletionShown := False;
  FStatusAggregateImageCache := TStringList.Create;
  FCurrentVerseWorkspace := TCurrentVerseWorkspace.Create;
  FCurrentVerseWorkspace.InitializeVerseRuntime(FAVersiculo, FACamposTexto);
  FPersistence        := nil;
  FConfig             := TProjectConfig.Create(nil);
  FVerseRepository    := TProjectVerseRepository.Create(nil);
  FVerseRenderBuffer  := nil;
  FAssociationMemory  := TAssociationMemoryService.Create;
  FNavigationHistory  := TNavigationHistory.Create;
  FProjectExporter    := TProjectExporter.Create(etNone, nil, nil);
  FDictionaryManager  := TDictionaryManager.Create;
  FPatchApplier       := TProjectPatchApplier.Create(nil);
  FBibleTreeCoordinator := TBibleTreeCoordinator.Create(nil);
  FTblPares           := nil;
  FTblInfo            := nil;
  FEscopo             := etNone;
  FClosing            := false;
  FAssociationMemoryRebuildRunning := False;
  FAssociationMemoryRebuildFailed := False;
  FAssociationMemoryLiveEditPending := False;
  FAssociationMemorySpeculativeEditPending := False;
  FAssociationMemoryProgressBar := nil;
  FVerseStrongsCountMode := scNone;
  FLastLLMUsage.PromptTokens := 0;
  FLastLLMUsage.CompletionTokens := 0;
  FLastLLMUsage.TotalTokens := 0;
  FLastLLMCallCompleted := False;

  FTemporizador          := TTimer.Create(nil);
  FTemporizador.Enabled  := false;
  FTemporizador.Interval := 300;
  FTemporizador.OnTimer  := @OnExibirDefinicao;

  FVerseMemo := TVerseMemo.Create;
  FVerseMemo.OnVerseTextModified := @OnChangeTextVersiculo;
  FVerseMemo.OnPendingChange := @PendingEditorChanged;
end;

constructor TProject.Create(paineis: array of TScrollbox; navegador: TTreeView;
  rsituacao: TComboBox; rcomentarios: TControl);
var
  v: integer;
begin
  Create;

  for v:=low(paineis) to high(paineis) do
    if (v <= Ord(tbConsulta2)) and assigned(paineis[v]) then
      CreateVerseObject(paineis[v], TTipoTextoBiblico(v));

  Arvore              := navegador;
  FBibleTreeCoordinator.Attach(FArvore);
  FRadioGroupSituacao := rsituacao;
  FMemoComentarios    := rcomentarios;
  if FMemoComentarios is TCommentsView then
    TCommentsView(FMemoComentarios).OnTextChanged := @PendingEditorChanged;
end;

destructor TProject.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  FNewVerseNotificationQueued := False;
  FCurrentVerseWorkspace.DestroyVerseViews(FAVersiculo);

  if Assigned(FAIProcessingNodes) then
    FreeAndNil(FAIProcessingNodes);

  FreeAndNil(FCurrentVerseWorkspace);
  FreeAndNil(FBibleTreeCoordinator);
  FreeAndNil(FPatchApplier);
  FreeAndNil(FProjectExporter);
  FreeAndNil(FDictionaryManager);
  FreeAndNil(FStatusAggregateImageCache);
  FreeAndNil(FStatusImages);
  FreeAndNil(FNavigationHistory);
  FreeAndNil(FAssociationMemory);
  FreeAndNil(FVerseRenderBuffer);
  FreeAndNil(FVerseRepository);
  FreeAndNil(FConfig);
  FreeAndNil(FPersistence);

  FOnNovoVersiculo.Free;
  FSuggestionManager.Free;
  FTemporizador.Free;
  FVerseMemo.Destroy;
  inherited Destroy;
end;

procedure TProject.OpenProject(Nome: string);
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
  OldExporting: Boolean;
begin
  if not FileExists(Nome) then
  begin
      MessageDlg(SOpenProject, format(SProjectDoesntExist, [Nome]), mtError, [mbOK], 0);
    exit;
  end;

  FNavigationHistory.Clear;

  FAtivo := False;
  FAssociationMemoryRebuildRunning := False;
  FAssociationMemoryRebuildFailed := False;
  FAssociationMemoryLiveEditPending := False;
  FAssociationMemorySpeculativeEditPending := False;
  FScrollEventsEnabled := False;
  FCurrentVerseWorkspace.Clear;
  FVerseMemo.DiscardEdit;
  for t := Low(FAVersiculo) to High(FAVersiculo) do
    if Assigned(FAVersiculo[t]) then
    begin
      FAVersiculo[t].ReferenceID := '';
      FAVersiculo[t].Modified := False;
      FAVersiculo[t].XMLModified := False;
    end;

  FFileName := Nome;

  FreeAndNil(FPersistence);
  FPersistence := TProjectPersistence.Create;
  FPersistence.Open(Nome);
  FPersistence.OnContentPendingChanged := @PendingEditorChanged;
  if not Assigned(FConfig) then
    FConfig := TProjectConfig.Create(FPersistence)
  else
    FConfig.Attach(FPersistence);
  FTblInfo := FPersistence.InfoTable;

  FreeAndNil(FSuggestionManager);
  FSuggestionManager := TSuggestionManager.Create(Nome);

  FTblPares := FPersistence.PairsTable;

  f := TFont.Create;
  for t:=tbOrigem to tbConsulta2 do
  begin
    if not assigned(FAVersiculo[t]) then
       continue;

    FAVersiculo[t].TokenizerVersion := TokenizerVersion;
    ApplyTextDirectionToVerse(t, GetTextDirection(t));

    { definindo fontes }
    f.Name := 'default';
    f.Size := 12;

    s := GetInfo(format('fonte%d.tamanho', [t]));
    if not s.IsEmpty then
      f.Size := StrToInt(s);
    s := GetInfo(format('fonte%d.nome', [t]));
    if not s.IsEmpty then
      f.Name := s;
    s := GetInfo(format('fonte%d.bold', [t]));
    if not s.IsEmpty then
      f.Bold := Boolean(StrToInt(s));

    FAVersiculo[t].Font := f;

    { definindo dicionários }
    AssignStrongDictionary(GetInfo(format('dicstrong%d', [t])), t);
    AssignMorphologyDictionary(GetInfo(format('dicmorfo%d', [t])), t);
  end;
  f.free;

  FCurrentVerseWorkspace.ApplyTokenizerVersion(FAVersiculo, TokenizerVersion);
  FCurrentVerseWorkspace.ApplyBoldStrongs(FAVersiculo, FPalavrasComStrongEmNegrito);
  FCurrentVerseWorkspace.ApplyStrongsCountMode(FAVersiculo, FVerseStrongsCountMode);

  FreeAndNil(FVerseRenderBuffer);
  FVerseRenderBuffer := TVerseRenderBuffer.Create();
  FVerseRenderBuffer.SetTokenizerVersion(TokenizerVersion);
  FAssociationMemory.Attach(FConfig, FSuggestionManager, FVerseRenderBuffer);
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
    RefreshStatusImages;
    PreencherArvore;
    UpdateTree;
      UpdateTree;
    //FArvore.FullCollapse;
    FArvore.Enabled := true;
  end;

  if assigned(FMemoComentarios) then
    FMemoComentarios.Enabled := true;
  if assigned(FRadioGroupSituacao) then
  begin
    RefreshStatusControl;
    FRadioGroupSituacao.Enabled := true;
  end;

  FTblPares.BeforeScroll := @BeforeVerseScroll;
  FTblPares.AfterScroll  := @AfterVerseScroll;
  if Assigned(FArvore) then
  begin
    FArvore.OnChange       := @OnMudancaVersiculo;
    FArvore.OnContextPopup := @OnBibleTreeContextPopup;
    FArvore.PopupMenu := CreateBibleTreeContextMenu;
  end;

  // set tokenizer version
  TTokenizerFactory.PreferredVersion := TokenizerVersion;

  ScrollEventsEnabled := true;
  OldExporting := FExportando;
  FExportando := True;
  try
    GoToReference(FConfig.ReadMarker);
    if Assigned(FAVersiculo[tbOrigem]) and
      ((not Assigned(FAVersiculo[tbOrigem].Syntagms)) or
       (FAVersiculo[tbOrigem].ReferenceID <> ID)) then
      AfterVerseScroll(nil);
  finally
    FExportando := OldExporting;
  end;

  FAtivo := true;
  FDisplayTags := false;
  if FAssociationMemory.NeedsRebuild then
    try
      RebuildSuggestionDatabase(nil);
    except
      on E: Exception do
        if Assigned(FArvore) then
          MessageDlg(SError, E.Message, mtError, [mbOK], 0)
        else
          raise;
    end;
end;

function TProject.CloseProject(_Commit: boolean): Boolean;
var
  t: TTipoTextoBiblico;
  SavedChapterViewMode: TChapterViewMode;
  SavedChapterViewText: TTipoTextoBiblico;
  SavedMarker: string;
begin
  Result := False;
  if not Assigned(FPersistence) then
    Exit(True);
  if _Commit and (FAssociationMemoryRebuildRunning or
    (Assigned(FAssociationMemory) and FAssociationMemory.RebuildActive)) then
    Exit(False);

  SavedChapterViewMode := ChapterViewMode;
  SavedChapterViewText := ChapterViewText;
  SavedMarker := ID;

  if _Commit then
    try
      BeforeVerseScroll(nil);
    except
      on EAbort do
        Exit(False);
    end
  else
    FVerseMemo.DiscardEdit;

  if _Commit then
    Commit
  else
    Rollback;

  FConfig.WriteMarker(SavedMarker);
  FConfig.WriteChapterViewMode(Ord(SavedChapterViewMode));
  FConfig.WriteChapterViewText(Ord(SavedChapterViewText));

  for t:=low(FAVersiculo) to high(FAVersiculo) do
    if assigned(FAVersiculo[t]) then
      SetInfo(format('fonte%d.tamanho', [t]),
        IntToStr(FAVersiculo[t].Font.Size));

  FClosing := true;
  if assigned(FArvore) then
  begin
    //FArvore.FullCollapse;
    FArvore.Enabled := false;
    FArvore.Items.Clear;
    FArvore.Images := nil;
  end;
  if Assigned(FStatusAggregateImageCache) then
    FStatusAggregateImageCache.Clear;
  FreeAndNil(FStatusImages);

  if Assigned(FConfig) then
    FConfig.Attach(nil);
  if Assigned(FVerseRepository) then
    FVerseRepository.Attach(nil);
  if Assigned(FPatchApplier) then
    FPatchApplier.Attach(nil);
  if Assigned(FCurrentVerseWorkspace) then
    FCurrentVerseWorkspace.Clear;
  if Assigned(FAssociationMemory) then
    FAssociationMemory.Attach(nil, nil, nil);
  FreeAndNil(FPersistence);
  FTblPares := nil;
  FTblInfo := nil;

  FVerseMemo.DiscardEdit;

  FreeAndNil(FVerseRenderBuffer);

  if assigned(FMemoComentarios) then
  begin
    if FMemoComentarios is TCommentsView then
      TCommentsView(FMemoComentarios).DiscardEdit;
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
  FNavigationHistory.Clear;
  Result := True;
end;

function TProject.EnsureAssociationMemoryFresh: Boolean;
begin
  Result := False;
  if not Assigned(FAssociationMemory) then
    Exit;
  if FAssociationMemoryRebuildRunning or FAssociationMemory.RebuildActive then
    Exit(True);
  if FAssociationMemoryLiveEditPending then
    Exit(True);
  if FAssociationMemoryRebuildFailed and
    FAssociationMemory.NeedsRebuild then
    Exit(True);
  if FAssociationMemory.NeedsRebuild then
  begin
    RebuildAssociationMemory(nil);
    Commit;
    Result := True;
  end;
  FAssociationMemory.ValidateCurrentCache;
end;

procedure TProject.Commit;
var
  BuiltRevisionStaged: Boolean;
  MemoryWasDirty: Boolean;
begin
  if FAssociationMemoryRebuildRunning or
    (Assigned(FAssociationMemory) and FAssociationMemory.RebuildActive) then
    Exit;

  if Assigned(FAssociationMemory) and
    FAssociationMemoryRebuildFailed and
    FAssociationMemory.NeedsRebuild then
  begin
    if Assigned(FSuggestionManager) then
      FSuggestionManager.Rollback;
    if Assigned(FPersistence) then
      FPersistence.Commit;
    FAssociationMemory.CommitSourceOutdated;
    if Assigned(FPersistence) then
      FPersistence.FlushImmediateInfo;
    Exit;
  end;

  BuiltRevisionStaged := False;
  MemoryWasDirty := Assigned(FAssociationMemory) and
    FAssociationMemory.PendingDirty;
  if not MemoryWasDirty and Assigned(FAssociationMemory) then
  begin
    if FAssociationMemory.NeedsRebuild then
      RebuildAssociationMemory(nil);
    FAssociationMemory.ValidateCurrentCache;
    MemoryWasDirty := FAssociationMemory.PendingDirty;
  end;

  if MemoryWasDirty then
  begin
    if not FAssociationMemory.CanCommitCompletedRebuild then
    begin
      if Assigned(FSuggestionManager) then
        FSuggestionManager.Rollback;
      FAssociationMemory.MarkCacheOutdated;

      if Assigned(FPersistence) then
      begin
        // Persist the source revision first. If writing canonical pairs fails,
        // the durable revision mismatch prevents stale cache use after reopen.
        FPersistence.CommitInfo;
        FPersistence.CommitPairs;
        FAssociationMemory.MarkSourceRevisionPersisted;
      end;

      if FAssociationMemory.CanReplayPendingCache then
        FAssociationMemory.ReplayPendingCache
      else
        RebuildAssociationMemory(nil);
    end;
  end
  else if Assigned(FPersistence) then
  begin
    // Cache validation and suggestion lookups can leave a read transaction on
    // the independent Association-memory connection. Release that reader
    // before the canonical dataset tries to publish unrelated project edits.
    if Assigned(FSuggestionManager) then
      FSuggestionManager.Rollback;
    FPersistence.Commit;
  end;

  if assigned(FSuggestionManager) then
    try
      FSuggestionManager.Commit;
    except
      if Assigned(FAssociationMemory) then
      begin
        FAssociationMemory.MarkCacheOutdated;
        FAssociationMemory.InvalidateReplay;
      end;
      raise;
    end;

  if Assigned(FAssociationMemory) then
  begin
    BuiltRevisionStaged := FAssociationMemory.StageBuiltRevision;
    if BuiltRevisionStaged and Assigned(FPersistence) then
      try
        FPersistence.CommitInfo;
      except
        FAssociationMemory.MarkCacheOutdated;
        FAssociationMemory.InvalidateReplay;
        raise;
      end;
    FAssociationMemory.CommitCompleted;
  end;
  if Assigned(FPersistence) then
    FPersistence.FlushImmediateInfo;
  FAssociationMemoryLiveEditPending := False;
  FAssociationMemorySpeculativeEditPending := False;
end;

procedure TProject.Rollback;
var
  OldScrollEventsEnabled: Boolean;
  TextType: TTipoTextoBiblico;
begin
  // The Association-memory database uses an independent SQLite connection.
  // Release any reader or writer it owns before restoring canonical project
  // rows, otherwise the persistence rollback can fail with SQLITE_BUSY.
  if Assigned(FSuggestionManager) then
    FSuggestionManager.Rollback;
  OldScrollEventsEnabled := FScrollEventsEnabled;
  FScrollEventsEnabled := False;
  try
    if Assigned(FPersistence) then
      FPersistence.Rollback;
  finally
    FScrollEventsEnabled := OldScrollEventsEnabled;
  end;
  if Assigned(FPersistence) then
    FPersistence.FlushImmediateInfo;
  if Assigned(FAssociationMemory) then
    FAssociationMemory.ReloadRevisionState;
  FAssociationMemoryLiveEditPending := False;
  FAssociationMemorySpeculativeEditPending := False;
  if Assigned(FCurrentVerseWorkspace) then
    FCurrentVerseWorkspace.Clear;
  for TextType := Low(FAVersiculo) to High(FAVersiculo) do
    ApplyTextDirectionToVerse(TextType, GetTextDirection(TextType));
  if FAtivo and OldScrollEventsEnabled and not FClosing then
  begin
    FExportando := True;
    try
      AfterVerseScroll(nil);
    finally
      FExportando := False;
    end;
  end;
end;

procedure TProject.GoToReference(reference: string);
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

procedure TProject.GoToReference(reference: TReference);
begin
  GoToReference(format('%d,%d,%d', [reference.BookID, reference.Chapter, reference.Verse]));
end;

procedure TProject.GoBack;
var
  TargetReference: string;
  RestoredReference: string;
begin
  if not FNavigationHistory.TryGoBack(TargetReference) then
    Exit;

  try
    GoToReference(TargetReference);
  except
    FNavigationHistory.TryGoForward(RestoredReference);
    raise;
  end;
end;

procedure TProject.GoForward;
var
  TargetReference: string;
  RestoredReference: string;
begin
  if not FNavigationHistory.TryGoForward(TargetReference) then
    Exit;

  try
    GoToReference(TargetReference);
  except
    FNavigationHistory.TryGoBack(RestoredReference);
    raise;
  end;
end;

procedure TProject.NextVerse;
begin
  if not FPersistence.EOF then
    FPersistence.Next;
  if not FExportando and SugerirAssociacaoAutomaticamente and
    StatusAutomationEnabled and (Status = FirstStatusId(GetStatusDefinitions)) then
    SuggestAssociation;
end;

procedure TProject.PreviousVerse;
begin
  if not FPersistence.BOF then
    FPersistence.Prior;
  if not FExportando and SugerirAssociacaoAutomaticamente and
    StatusAutomationEnabled and (Status = FirstStatusId(GetStatusDefinitions)) then
    SuggestAssociation;
end;

procedure TProject.FirstVerse;
begin
  if not FPersistence.BOF then
    FPersistence.First;
  if not FExportando and SugerirAssociacaoAutomaticamente and
    StatusAutomationEnabled and (Status = FirstStatusId(GetStatusDefinitions)) then
    SuggestAssociation;
end;

procedure TProject.LastVerse;
begin
  if not FPersistence.EOF then
    FPersistence.Last;
  if not FExportando and SugerirAssociacaoAutomaticamente and
    StatusAutomationEnabled and (Status = FirstStatusId(GetStatusDefinitions)) then
    SuggestAssociation;
end;

procedure TProject.RefreshUI;
begin
  if FAssociationMemorySpeculativeEditPending then
    BeforeVerseScroll(nil);
  AfterVerseScroll(FTblPares);
end;

procedure TProject.RollbackCurrentVerse;
var
  DestinationVerse: TVerse;
  SourceVerse: TVerse;
begin
  if FAssociationMemorySpeculativeEditPending and
    Assigned(FPersistence) then
  begin
    SourceVerse := FAVersiculo[tbOrigem];
    DestinationVerse := FAVersiculo[tbDestino];
    if Assigned(SourceVerse) and Assigned(DestinationVerse) and
      not FAssociationMemoryLiveEditPending then
      try
        // Keep the live semantic baseline until the inverse cache delta has
        // been recorded; the reload below will replace that baseline.
        SourceVerse.ChangeText(FPersistence.CurrentText(
          FACamposTexto[tbOrigem]));
        DestinationVerse.ChangeText(FPersistence.CurrentText(
          FACamposTexto[tbDestino]));
        SourceVerse.Pairs := FPersistence.CurrentPairs;
        SaveAssociations;
      except
        on E: Exception do
        begin
          if Assigned(FAssociationMemory) then
          begin
            FAssociationMemory.MarkCacheOutdated;
            FAssociationMemory.InvalidateReplay;
          end;
          FAssociationMemoryRebuildFailed := True;
          FAssociationMemoryLiveEditPending := True;
          DebugLn('Association-memory current Verse rollback failed: ' +
            E.Message);
        end;
      end;
  end;
  FAssociationMemorySpeculativeEditPending := False;
  FAssociationMemoryLiveEditPending := False;
  AfterVerseScroll(FTblPares);
end;

procedure TProject.ClearAssociations;
var
  RecoveringInvalidAssociations: Boolean;
begin
  if assigned(FAVersiculo[tbOrigem]) then
  begin
    RecoveringInvalidAssociations :=
      FCurrentVerseWorkspace.LoadedAssociationsInvalid;
    FAVersiculo[tbOrigem].ClearAssociations;
    if RecoveringInvalidAssociations then
      // An invalid row is shown without its partially applicable prefix.
      // Treat the explicit Clear command as the user's decision to replace
      // that stored row with an empty, valid association set.
      FAVersiculo[tbOrigem].Modified := True;
    SaveAssociations;
    SavePairs;
    if StatusAutomationEnabled then
      SetStatus(FirstStatusId(GetStatusDefinitions));
    RefreshStatusControl;
  end;
end;

function TProject.SaveProject: Boolean;
var
  e: TOnNovoVersiculoEvent;
begin
  Result := False;
  if FAssociationMemoryRebuildRunning or
    (Assigned(FAssociationMemory) and FAssociationMemory.RebuildActive) then
    Exit;
  try
    BeforeVerseScroll(nil);
  except
    on EAbort do
      Exit(False);
  end;
  Commit;
  FConfig.WriteMarker(ID);

  //UpdateTree;
  for e in FOnNovoVersiculo do
    e(Self);
  Result := True;
end;

procedure TProject.SuggestAssociationStatistical;
begin
  if not Assigned(FAssociationMemory) then
    Exit;
  EnsureAssociationMemoryFresh;
  if FAssociationMemoryLiveEditPending or
     FAssociationMemoryRebuildRunning or
     FAssociationMemory.RebuildActive or
     FAssociationMemory.NeedsRebuild or
     not FAssociationMemory.CacheValidated then
    Exit;

  FSuggestionManager.SuggestAssociations(FAVersiculo[tbOrigem]);
  if ReconcileChangedVerseAssociations then
    NotifyNewVerseObserversAfterEdit;
  FAVersiculo[tbOrigem].Render;
  FAVersiculo[tbDestino].Render;

  if not StatusAutomationEnabled then exit;

  if (FRadioGroupSituacao.ItemIndex = 0) and (FAVersiculo[tbOrigem].AssociationProgress > 0) then
    FRadioGroupSituacao.ItemIndex := 1; // associando
end;

procedure TProject.SuggestAssociation;
begin
  case FSuggestionMode of
    smStatistical: SuggestAssociationStatistical;
    smAI:
      begin
        // Note: AI Association is blocking and should ideally show progress,
        // but for auto-suggestion we rely on blocking behavior.
        // It might be worth checking if AI is configured before trying.
        try
          SuggestAssociationAI;
        except
          on E: Exception do
            // Fallback or just log? User will see message box from SuggestAssociationAI catch block.
        end;
      end;
  end;
end;

procedure TProject.SuggestAssociationAI;
var
  flow: IAIVerseFlow;
  flowConfig: IAIFlowConfig;
  msgs: TLLMMessageArray;
  resp: TLLMResponse;
  parsed: TAIFlowResult;
  v: TVerse;
  ErrorMessage: string;
  WaitFormStarted: Boolean;
  AssociationChanged: Boolean;
begin
  AssociationChanged := False;
  FLastLLMUsage.PromptTokens := 0;
  FLastLLMUsage.CompletionTokens := 0;
  FLastLLMUsage.TotalTokens := 0;
  FLastLLMCallCompleted := False;

  if (GetInfo(AI_COMMON_CONFIG.Model).IsEmpty) or
    (GetInfo(AI_COMMON_CONFIG.ApiKey).IsEmpty) then
  begin
    ShowMessage(SAIConfigurationMissing);
    Exit;
  end;

  v := FAVersiculo[tbOrigem];
  if not Assigned(v) or not Assigned(v.PairedVerse) then
  begin
    ShowMessage(SNoVerseToAssociate);
    Exit;
  end;

  ErrorMessage := '';
  WaitFormStarted := False;
  if Assigned(FormPleaseWait) then
  begin
    FormPleaseWait.BeginOperation(Application.MainForm, '', SAnalyzingVerse,
      0, GetVerseReferenceString(v));
    WaitFormStarted := True;
  end;

  try
    try
      flowConfig := GetFlowConfigById('association');
      flow := flowConfig.CreateFlowInstance(CreateAIFlowContext(FConfig));

      msgs := flow.BuildMessages(v);
      resp := AskChatCompletionsResponsive(GetInfo(AI_COMMON_CONFIG.Model),
        GetInfo(AI_COMMON_CONFIG.ApiKey), msgs);
      FLastLLMUsage := resp.Usage;
      FLastLLMCallCompleted := True;

      AIDebugLog(SAISingleResponseReceived);
      parsed := flow.ParseResponse(resp.Content, v);
      try
        AIDebugLog(SAISingleApplyingResult);
        flow.ApplyResult(parsed, v);
        AssociationChanged :=
          FCurrentVerseWorkspace.HasAssociationChanges(v);
        if AssociationChanged then
          FAssociationMemorySpeculativeEditPending := True;
        try
          // Persist the canonical association before advancing the derived
          // memory baseline. A failed write must leave the old cache blocked.
          SaveVersePairs(v);
        except
          on SaveError: Exception do
          begin
            try
              if Assigned(FPersistence) then
                FPersistence.CancelCurrentEdit;
            except
              on CancelError: Exception do
              begin
                if Assigned(FAssociationMemory) then
                begin
                  FAssociationMemory.MarkCacheOutdated;
                  FAssociationMemory.InvalidateReplay;
                end;
                FAssociationMemoryRebuildFailed := True;
                DebugLn('Association persistence cancel failed after "' +
                  SaveError.Message + '": ' + CancelError.Message);
              end;
            end;
            if AssociationChanged then
            begin
              FAssociationMemoryLiveEditPending := True;
              NotifyNewVerseObserversAfterEdit;
            end;
            raise;
          end;
        end;
        if AssociationChanged then
        begin
          ReconcileEditedVerseAssociationMemory;
          NotifyNewVerseObserversAfterEdit;
        end;
        AIDebugLog(SAISingleResultApplied);
      finally
        parsed.Free;
      end;
    except
      on E: Exception do
      begin
        AIDebugLog(Format(SAISingleErrorFmt, [E.Message]));
        ErrorMessage := Format(SAIAssociationFailedFmt, [E.Message]);
      end;
    end;
  finally
    if WaitFormStarted then
      FormPleaseWait.FinishOperation;
  end;

  if ErrorMessage <> '' then
    ShowMessage(ErrorMessage);
end;

procedure TProject.StartAIAssociationBatch(NodesVerses: TAIBatchWorkItemList; MaxParallel: Integer);
var
  flow: IAIVerseFlow;
  batchPrep: TAIBatchPreparation;
  orchestrator: TAIBatchOrchestrator;
begin
  if (NodesVerses = nil) or (NodesVerses.Count = 0) then Exit;
  if (GetInfo(AI_COMMON_CONFIG.Model).IsEmpty) or (GetInfo(AI_COMMON_CONFIG.ApiKey).IsEmpty) then
  begin
    ShowMessage(SAIConfigurationMissing);
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

procedure TProject.StartAITranslationBatch(NodesVerses: TAIBatchWorkItemList; MaxParallel: Integer);
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

  if (NodesVerses.Count = 0) and (batchPrep.SkippedCount > 0) then
  begin
    ShowMessage(SNoEmptyTranslationsInScope);
    Exit;
  end;

  flow := batchPrep.Flow;
  FAIManager := batchPrep.Manager;
  if (flow = nil) or not Assigned(FAIManager) then
    Exit;

  FAIManager.StartBatch(flow, NodesVerses);
end;

procedure TProject.HandleAutoSave(Sender: TObject);
begin
  // Called from main thread when autosave interval is reached
  if FAssociationMemoryRebuildRunning or
    (Assigned(FAssociationMemory) and FAssociationMemory.RebuildActive) then
    Exit;
  SaveProject;
end;

function TProject.GetVerseReferenceString(Verse: TVerse): string;
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

procedure TProject.RebuildSuggestionDatabase;
begin
  RebuildSuggestionDatabase(nil);
end;

procedure TProject.RebuildSuggestionDatabase(pb: TProgressBar);
begin
  BeforeVerseScroll(nil);
  RebuildAssociationMemory(pb);
  Commit;
end;

procedure TProject.AssociationMemoryRebuildProgress(
  Phase: TAssociationMemoryRebuildPhase; Current, Total: Integer);
var
  ProgressMaximum: Integer;
  DisplayedPosition: Integer;
begin
  if Assigned(FAssociationMemoryProgressBar) then
  begin
    if Phase = amrpFinalizing then
      FAssociationMemoryProgressBar.Style := pbstMarquee
    else
    begin
      ProgressMaximum := Total;
      if ProgressMaximum <= 0 then
        ProgressMaximum := 1;
      FAssociationMemoryProgressBar.Style := pbstNormal;
      FAssociationMemoryProgressBar.Min := 0;
      FAssociationMemoryProgressBar.Max := ProgressMaximum;
      DisplayedPosition := Current;
      if DisplayedPosition < FAssociationMemoryProgressBar.Min then
        DisplayedPosition := FAssociationMemoryProgressBar.Min
      else if DisplayedPosition > ProgressMaximum then
        DisplayedPosition := ProgressMaximum;
      FAssociationMemoryProgressBar.Position := DisplayedPosition;
      // The native Windows progress control animates large forward jumps. A
      // one-step backward update makes phase changes visible immediately.
      if DisplayedPosition > FAssociationMemoryProgressBar.Min then
      begin
        FAssociationMemoryProgressBar.Position := DisplayedPosition - 1;
        FAssociationMemoryProgressBar.Position := DisplayedPosition;
      end;
    end;
    FAssociationMemoryProgressBar.Visible := True;
    FAssociationMemoryProgressBar.Update;
  end;

  if Assigned(FormPleaseWait) and
    (FAssociationMemoryProgressBar = FormPleaseWait.ProgressBar1) then
  begin
    case Phase of
      amrpWriting:
        FormPleaseWait.SetMessage(SAssociationMemoryWriting);
      amrpValidating:
        FormPleaseWait.SetMessage(SAssociationMemoryValidating);
      amrpFinalizing:
        FormPleaseWait.SetMessage(SAssociationMemoryFinalizing);
    end;
    if Phase = amrpFinalizing then
      FormPleaseWait.SetDetail('')
    else
      FormPleaseWait.SetDetail(Format(
        SAssociationMemoryDatabaseProgressFmt, [Current, Total]));
    TFormPleaseWait.PumpProgressPaintMessages(FormPleaseWait);
  end
  else if Assigned(FAssociationMemoryProgressBar) then
  begin
    TFormPleaseWait.PumpProgressPaintMessages(
      FAssociationMemoryProgressBar);
  end;
end;

procedure TProject.RebuildAssociationMemory(ProgressBar: TProgressBar);
var
  verseData: TVerseData;
  SavedMarker: string;
  OldScrollEventsEnabled: Boolean;
  OldExporting: Boolean;
  OwnProgressForm: Boolean;
  RebuildSucceeded: Boolean;
  UsesActiveProgressForm: Boolean;
  LastProgressPulse: QWord;
  ProgressPulse: QWord;
begin
  if not Assigned(FAssociationMemory) or not Assigned(FVerseRepository) then
    exit;
  if FAssociationMemoryRebuildRunning or FAssociationMemory.RebuildActive then
    Exit;

  FAssociationMemoryRebuildRunning := True;
  FAssociationMemoryRebuildFailed := False;
  RebuildSucceeded := False;
  OldScrollEventsEnabled := FScrollEventsEnabled;
  OldExporting := FExportando;
  OwnProgressForm := False;
  UsesActiveProgressForm := False;
  LastProgressPulse := 0;
  SavedMarker := '';
  try
    SavedMarker := ID;
    if not Assigned(ProgressBar) and Assigned(FormPleaseWait) then
    begin
      if not FormPleaseWait.OperationActive then
      begin
        FormPleaseWait.BeginOperation(Application.MainForm,
          SAssociationMemoryProgressTitle, SAssociationMemoryProgressMessage,
          QLinhas[FEscopo], ExtractFileName(FFileName));
        OwnProgressForm := True;
      end;
      ProgressBar := FormPleaseWait.ProgressBar1;
      UsesActiveProgressForm := True;
    end;
    if Assigned(ProgressBar) and Assigned(FormPleaseWait) and
      FormPleaseWait.OperationActive and
      (ProgressBar = FormPleaseWait.ProgressBar1) then
      UsesActiveProgressForm := True;

    if assigned(ProgressBar) then
    begin
      ProgressBar.Style := pbstNormal;
      ProgressBar.Position := 0;
      ProgressBar.Min := 0;
      ProgressBar.Max := QLinhas[FEscopo];
      ProgressBar.Step := 1;
      ProgressBar.Visible := true;
      ProgressBar.Update;
      if UsesActiveProgressForm then
        TFormPleaseWait.PumpProgressPaintMessages(FormPleaseWait)
      else
        TFormPleaseWait.PumpProgressPaintMessages(ProgressBar);
      LastProgressPulse := GetTickCount64;
    end;

    FScrollEventsEnabled := False;
    FExportando := True;
    FAssociationMemoryProgressBar := ProgressBar;
    if Assigned(FSuggestionManager) then
      FSuggestionManager.OnRebuildProgress :=
        @AssociationMemoryRebuildProgress;
    FAssociationMemory.MarkSourceDirty;
    if Assigned(FSuggestionManager) then
      FSuggestionManager.Rollback;
    FAssociationMemory.MarkCacheOutdated;
    if Assigned(FPersistence) then
    begin
      // Publish the canonical data and source revision before opening the
      // association-memory write transaction. A later failure therefore
      // leaves a durable revision mismatch instead of publishing stale data.
      FPersistence.CommitInfo;
      FPersistence.CommitPairs;
      FAssociationMemory.MarkSourceRevisionPersisted;
    end;
    FAssociationMemory.BeginRebuild;
    try
      FVerseRepository.First;
      while not FVerseRepository.EOF do
      begin
        verseData := FVerseRepository.ReadCurrentVerseData;
        FAssociationMemory.AddVerse(verseData.ReferenceID,
          verseData.ReferenceName, verseData.OriginalText,
          verseData.TranslatedText, verseData.Associations);
        if assigned(ProgressBar) then
        begin
           ProgressBar.StepIt;
           ProgressPulse := GetTickCount64;
           if (ProgressBar.Position >= ProgressBar.Max) or
             ((ProgressBar.Position mod 50) = 0) or
             (ProgressPulse - LastProgressPulse >= 150) then
           begin
             if UsesActiveProgressForm then
               FormPleaseWait.SetDetail(Format(
                 SAssociationMemoryProgressDetailFmt,
                 [verseData.ReferenceName, ProgressBar.Position,
                  ProgressBar.Max]));
             ProgressBar.Update;
             if UsesActiveProgressForm then
               TFormPleaseWait.PumpProgressPaintMessages(FormPleaseWait)
             else
               TFormPleaseWait.PumpProgressPaintMessages(ProgressBar);
             LastProgressPulse := ProgressPulse;
           end;
        end;
        FVerseRepository.Next;
      end;
      if Assigned(FormPleaseWait) and
        (ProgressBar = FormPleaseWait.ProgressBar1) then
      begin
        FormPleaseWait.SetMessage(SAssociationMemoryWriting);
        TFormPleaseWait.PumpProgressPaintMessages(FormPleaseWait);
      end;
      FAssociationMemory.FinishRebuild;
      RebuildSucceeded := True;
    except
      FAssociationMemory.AbortRebuild;
      raise;
    end;
  finally
    FAssociationMemoryRebuildFailed := not RebuildSucceeded;
    if RebuildSucceeded then
    begin
      FAssociationMemoryLiveEditPending := False;
      FAssociationMemorySpeculativeEditPending := False;
    end;
    if Assigned(FSuggestionManager) then
      FSuggestionManager.OnRebuildProgress := nil;
    FAssociationMemoryProgressBar := nil;
    FAssociationMemoryRebuildRunning := False;
    if Assigned(FPersistence) and not SavedMarker.IsEmpty then
      FPersistence.LocateReference(SavedMarker);
    FScrollEventsEnabled := OldScrollEventsEnabled;
    if OldScrollEventsEnabled then
    begin
      FExportando := True;
      try
        AfterVerseScroll(nil);
      finally
        FExportando := OldExporting;
      end;
    end
    else
      FExportando := OldExporting;
    if assigned(ProgressBar) and not UsesActiveProgressForm then
      ProgressBar.Visible := false;
    if OwnProgressForm then
      FormPleaseWait.FinishOperation;
  end;
end;

function TProject.ImportTheWordModule(arquivo: string;
  texto: TTipoTextoBiblico; replace: boolean): boolean;
begin
  result := ImportTheWordModule(arquivo, texto, nil, replace);
end;

function TProject.ImportTheWordModule(arquivo: string;
  texto: TTipoTextoBiblico; pb: TProgressBar; replace: boolean): boolean;
var
  ImportResult: TTheWordModuleImportResult;
begin
  Result := ImportTheWordModule(arquivo, texto, pb, replace, ImportResult);
  if (not Result) and (ImportResult.Status = twmisFailed) and
    not ImportResult.ErrorMessage.IsEmpty then
    MessageDlg(arquivo, ImportResult.ErrorMessage, mtError, [mbOK], 0);
end;

function TProject.ImportTheWordModule(arquivo: string;
  texto: TTipoTextoBiblico; pb: TProgressBar; replace: boolean;
  out AImportResult: TTheWordModuleImportResult;
  AEnableStagedBatching: Boolean): boolean;
const
  STAGED_IMPORT_BATCH_SIZE = 200;
type
  TImportVerseSnapshot = record
    ReferenceID: string;
    TextValue: string;
    PairsValue: string;
    StatusValue: Integer;
  end;
  TImportVerseSnapshots = array of TImportVerseSnapshot;
var
  AvailableCount: Integer;
  EncodingInfo: TTheWordModuleEncodingInfo;
  HasImportedDescription: Boolean;
  I, M: Integer;
  ImportedDescription: string;
  ImportedDirection: TTextDirection;
  ImportedProperties: string;
  ImportError: string;
  Line: string;
  LinesOffset: Integer;
  ModuleExtension: string;
  ModuleProperties: string;
  ModuleScope: TEscopoTexto;
  Modulo: TStringList;
  MtDescription, MtVerseRules: IMatch;
  OldCursor: TCursor;
  OwnVerseRulesForm: Boolean;
  ProjectOffset: Integer;
  Propriedades: TStringStream;
  ReComments, ReDescription, ReVazio, ReVerseRules: IRegex;
  RowsMayBeCommitted: Boolean;
  RxVerseRulesFrom: TRegexList;
  RxVerseRulesTo: TStringList;
  RulesForm: TVerseRulesForm;
  SessionStarted: Boolean;
  SnapshotCount: Integer;
  Snapshots: TImportVerseSnapshots;
  SnapshotsRestored: Boolean;
  StageImportedProperties: Boolean;
  VerseRuleSources: TStringList;
  VerseRuleTargets: TStringList;
  StagedBatchingEnabled: Boolean;
  LastProgressPulse: QWord;
  UsesActiveProgressForm: Boolean;
  WaitFormTemporarilyHidden: Boolean;

  procedure SetFailure(const AMessage: string);
  begin
    AImportResult.Status := twmisFailed;
    AImportResult.ErrorMessage := AMessage;
    Result := False;
  end;

  procedure PositionAtFirstProjectVerse;
  var
    OffsetIndex: Integer;
  begin
    FPersistence.First;
    for OffsetIndex := 1 to ProjectOffset do
      if not FPersistence.EOF then
        FPersistence.Next;
  end;

  procedure PulseImportProgress(ACurrent: Integer;
    const AReference: string; AForce: Boolean);
  var
    CurrentPulse: QWord;
    DetailText: string;
  begin
    if not Assigned(pb) then
      Exit;

    if ACurrent < pb.Min then
      pb.Position := pb.Min
    else if ACurrent > pb.Max then
      pb.Position := pb.Max
    else
      pb.Position := ACurrent;

    CurrentPulse := GetTickCount64;
    if not AForce and (LastProgressPulse <> 0) and
      (CurrentPulse - LastProgressPulse < 150) then
      Exit;

    pb.Update;
    if UsesActiveProgressForm then
    begin
      if Trim(AReference) = '' then
        DetailText := Format(SModuleImportProgressCountFmt,
          [pb.Position, pb.Max])
      else
        DetailText := Format(SAssociationMemoryProgressDetailFmt,
          [AReference, pb.Position, pb.Max]);
      FormPleaseWait.SetDetail(DetailText);
      TFormPleaseWait.PumpProgressPaintMessages(FormPleaseWait);
    end
    else
      TFormPleaseWait.PumpProgressPaintMessages(pb);
    LastProgressPulse := CurrentPulse;
  end;

  procedure CommitStagedPairChanges(ACurrent: Integer;
    const AReference: string);
  begin
    PulseImportProgress(ACurrent, AReference, True);
    // From this point onward rollback must also be made durable.
    RowsMayBeCommitted := True;
    FPersistence.CommitPairs;
    PulseImportProgress(ACurrent, AReference, True);
  end;

  function CountAvailableProjectVerses: Integer;
  var
    LastReference: string;
  begin
    Result := 0;
    LastReference := '';
    PositionAtFirstProjectVerse;
    while not FPersistence.EOF do
    begin
      Inc(Result);
      LastReference := FPersistence.CurrentPairId;
      PulseImportProgress(Result, LastReference,
        (Result mod 75) = 0);
      FPersistence.Next;
    end;
    PulseImportProgress(Result, LastReference, True);
  end;

  procedure RestoreSnapshots;
  var
    Index: Integer;
    OldExporting: Boolean;
    OldScrollEventsEnabled: Boolean;
    SavedReference: string;
  begin
    if SnapshotCount = 0 then
    begin
      SnapshotsRestored := True;
      Exit;
    end;

    SavedReference := FPersistence.CurrentPairId;
    OldScrollEventsEnabled := FScrollEventsEnabled;
    OldExporting := FExportando;
    FScrollEventsEnabled := False;
    FExportando := True;
    try
      if UsesActiveProgressForm then
        FormPleaseWait.SetMessage(SModuleImportRestoring);
      LastProgressPulse := 0;
      PulseImportProgress(0, '', True);
      for Index := 0 to SnapshotCount - 1 do
      begin
        if not FPersistence.LocateReference(Snapshots[Index].ReferenceID) then
          raise Exception.CreateFmt('Could not restore imported verse %s.',
            [Snapshots[Index].ReferenceID]);
        FPersistence.SetCurrentText(FACamposTexto[texto],
          Snapshots[Index].TextValue);
        FPersistence.SetCurrentFieldAsString('pare_pares',
          Snapshots[Index].PairsValue);
        FPersistence.SetCurrentFieldAsInteger('pare_situacao',
          Snapshots[Index].StatusValue);
        FPersistence.PostCurrentEdit;
        if StagedBatchingEnabled and RowsMayBeCommitted and
          (((Index + 1) mod STAGED_IMPORT_BATCH_SIZE) = 0) then
          CommitStagedPairChanges(Index + 1,
            Snapshots[Index].ReferenceID);
        PulseImportProgress(Index + 1, Snapshots[Index].ReferenceID,
          (((Index + 1) mod 75) = 0) or
          (Index = SnapshotCount - 1));
      end;
      if StagedBatchingEnabled and RowsMayBeCommitted then
        CommitStagedPairChanges(SnapshotCount,
          Snapshots[SnapshotCount - 1].ReferenceID);
      SnapshotsRestored := True;
      if Assigned(FAssociationMemory) and
        TextTypeParticipatesInAssociations(texto) then
      begin
        FAssociationMemory.MarkSourceDirty;
        FAssociationMemory.MarkCacheOutdated;
        FAssociationMemory.InvalidateReplay;
      end;
    finally
      if not SavedReference.IsEmpty then
        FPersistence.LocateReference(SavedReference);
      FScrollEventsEnabled := OldScrollEventsEnabled;
      FExportando := OldExporting;
    end;
  end;

begin
  Result := False;
  AImportResult := Default(TTheWordModuleImportResult);
  AImportResult.Status := twmisNotStarted;
  AImportResult.FileName := arquivo;
  AImportResult.TextType := texto;
  Modulo := nil;
  Propriedades := nil;
  RxVerseRulesFrom := nil;
  RxVerseRulesTo := nil;
  RulesForm := nil;
  VerseRuleSources := nil;
  VerseRuleTargets := nil;
  HasImportedDescription := False;
  LinesOffset := 0;
  OwnVerseRulesForm := False;
  ProjectOffset := 0;
  RowsMayBeCommitted := False;
  SessionStarted := False;
  SnapshotCount := 0;
  SnapshotsRestored := False;
  StageImportedProperties := False;
  StagedBatchingEnabled := AEnableStagedBatching;
  LastProgressPulse := 0;
  UsesActiveProgressForm := Assigned(pb) and Assigned(FormPleaseWait) and
    FormPleaseWait.OperationActive and (pb = FormPleaseWait.ProgressBar1);

  try
    try
      ModuleExtension := LowerCase(ExtractFileExt(arquivo));
      if ModuleExtension = '.ont' then
      begin
        ModuleScope := etONT;
        if FEscopo = etNT then
          LinesOffset := QLinhas[etOT];
      end
      else if ModuleExtension = '.nt' then
      begin
        ModuleScope := etNT;
        if FEscopo = etOT then
        begin
          SetFailure(SModuleDoesNotIncludeScope);
          Exit;
        end;
        if FEscopo = etONT then
          ProjectOffset := QLinhas[etOT];
      end
      else if ModuleExtension = '.ot' then
      begin
        ModuleScope := etOT;
        if FEscopo = etNT then
        begin
          SetFailure(SModuleDoesNotIncludeScope);
          Exit;
        end;
      end
      else
      begin
        SetFailure(SInvalidModuleType);
        Exit;
      end;

      if ModuleScope = etONT then
        case FEscopo of
          etOT:
            AImportResult.ExpectedVerseCount := QLinhas[etOT];
          etNT:
            AImportResult.ExpectedVerseCount := QLinhas[etNT];
        else
          AImportResult.ExpectedVerseCount := QLinhas[etONT];
        end
      else
        AImportResult.ExpectedVerseCount := QLinhas[ModuleScope];

      EncodingInfo := InspectTheWordModuleEncodingFile(arquivo);
      if EncodingInfo.RequiresConfirmation and
        (MessageDlg(arquivo, Format(SPotentialNonUTF8ModuleWarning,
          [EncodingInfo.InvalidByteOffset]), mtWarning,
          [mbIgnore, mbAbort], 0) = mrAbort) then
      begin
        AImportResult.Status := twmisCancelled;
        AImportResult.ErrorMessage := SModuleImportCancelled;
        Exit;
      end;

      Modulo := TStringList.Create;
      Modulo.LoadFromFile(arquivo);
      if Modulo.Count < QLinhas[ModuleScope] then
      begin
        SetFailure(Format(SInvalidLineCount, [QLinhas[ModuleScope]]));
        Exit;
      end;

      ReVazio := RegexCreate('^\s*$', [rcoUTF8]);
      ReComments := RegexCreate('\s*#.*$', [rcoUTF8]);
      ReDescription :=
        RegexCreate('^\s*description\s*=\s*(.*)$', [rcoUTF8]);
      ReVerseRules := RegexCreate(
        '^\s*verse.rule\s*=\s*"(.*?)(?<!")"(?!")\s+"(.*?)"(?=\s*$|\s+"(.*?)(?<!")"(?!"))',
        [rcoUTF8]);

      Propriedades := TStringStream.Create('');
      VerseRuleSources := TStringList.Create;
      VerseRuleTargets := TStringList.Create;
      for I := QLinhas[ModuleScope] to Modulo.Count - 1 do
      begin
        Line := ReComments.Replace(Modulo[I], '');
        if ReVazio.IsMatch(Line) then
          Continue;

        Propriedades.WriteString(Modulo[I] + #13#10);
        MtDescription := ReDescription.Match(Line);
        if MtDescription.Success then
        begin
          ImportedDescription := MtDescription.Groups[1].Value;
          HasImportedDescription := True;
          Continue;
        end;

        MtVerseRules := ReVerseRules.Match(Line);
        if MtVerseRules.Success then
        begin
          VerseRuleSources.Add(MtVerseRules.Groups[1].Value);
          VerseRuleTargets.Add(MtVerseRules.Groups[2].Value);
        end;
      end;

      ModuleProperties := Propriedades.DataString;
      ImportedDirection := ParseTheWordR2LProperties(ModuleProperties);
      if ((texto = tbOrigem) and
          (GetInfo('propriedades.origem') = '')) or
         ((texto = tbDestino) and
          (GetInfo('propriedades.destino') = '')) then
      begin
        StageImportedProperties := True;
        ImportedProperties := NormalizeTheWordR2LProperties(
          ModuleProperties, ImportedDirection);
      end;

      if VerseRuleSources.Count > 0 then
      begin
        RulesForm := VerseRulesForm;
        if not Assigned(RulesForm) then
        begin
          RulesForm := TVerseRulesForm.Create(nil);
          OwnVerseRulesForm := True;
        end;
        RulesForm.Reset(arquivo);
        for I := 0 to VerseRuleSources.Count - 1 do
          RulesForm.AddVerseRule(VerseRuleSources[I], VerseRuleTargets[I]);

        WaitFormTemporarilyHidden := Assigned(FormPleaseWait) and
          FormPleaseWait.OperationActive and FormPleaseWait.Visible;
        if WaitFormTemporarilyHidden then
          // Keep the stay-on-top progress form from covering the rule chooser.
          FormPleaseWait.Hide;

        OldCursor := Screen.Cursor;
        Screen.Cursor := crDefault;
        try
          if RulesForm.ShowModal <> mrOK then
          begin
            AImportResult.Status := twmisCancelled;
            AImportResult.ErrorMessage := SModuleImportRulesCancelled;
            Exit;
          end;
          RulesForm.CompileVerseRules(RxVerseRulesFrom,
            RxVerseRulesTo);
        finally
          Screen.Cursor := OldCursor;
          if WaitFormTemporarilyHidden and Assigned(FormPleaseWait) and
            FormPleaseWait.OperationActive then
          begin
            FormPleaseWait.Show;
            FormPleaseWait.BringToFront;
            TFormPleaseWait.PumpProgressPaintMessages(FormPleaseWait);
          end;
        end;
      end;

      StartScrollingSession;
      SessionStarted := True;
      if StagedBatchingEnabled and
        ((FPersistence.PairsTable.State in dsEditModes) or
         FPersistence.PairsTable.UpdatesPending) then
        raise Exception.Create(
          SStagedImportRequiresCleanPairBaseline);
      if StagedBatchingEnabled and Assigned(FSuggestionManager) then
        // Release a reader left by a previous suggestion lookup before the
        // first canonical batch tries to acquire the SQLite writer.
        FSuggestionManager.Rollback;
      try
        if Assigned(pb) then
        begin
          pb.Style := pbstNormal;
          pb.Position := 0;
          pb.Min := 0;
          pb.Max := AImportResult.ExpectedVerseCount;
          pb.Step := 1;
          pb.Visible := True;
        end;
        LastProgressPulse := 0;
        PulseImportProgress(0, '', True);

        AvailableCount := CountAvailableProjectVerses;
        AImportResult.AvailableVerseCount := AvailableCount;
        if AvailableCount < AImportResult.ExpectedVerseCount then
        begin
          SetFailure(Format(SModuleImportCapacityError,
            [AvailableCount, AImportResult.ExpectedVerseCount]));
          Exit;
        end;
        LastProgressPulse := 0;
        PulseImportProgress(0, '', True);

        SetLength(Snapshots, AImportResult.ExpectedVerseCount);
        PositionAtFirstProjectVerse;
        for I := LinesOffset to
          LinesOffset + AImportResult.ExpectedVerseCount - 1 do
        begin
          if FPersistence.EOF then
            Break;

          if Assigned(RxVerseRulesFrom) then
            for M := 0 to RxVerseRulesFrom.Count - 1 do
              Modulo[I] := RxVerseRulesFrom[M].Replace(Modulo[I],
                RxVerseRulesTo[M]);

          Snapshots[SnapshotCount].ReferenceID :=
            FPersistence.CurrentPairId;
          Snapshots[SnapshotCount].TextValue :=
            FPersistence.CurrentText(FACamposTexto[texto]);
          Snapshots[SnapshotCount].PairsValue :=
            FPersistence.CurrentPairs;
          Snapshots[SnapshotCount].StatusValue :=
            FPersistence.CurrentStatus;
          Inc(SnapshotCount);

          if not TrySetVerseText(Modulo[I], texto, replace,
            ImportError) then
          begin
            if ImportError.IsEmpty then
              ImportError := SCorruptedData;
            raise Exception.Create(ImportError);
          end;
          Inc(AImportResult.ImportedVerseCount);
          AImportResult.LastReference :=
            Snapshots[SnapshotCount - 1].ReferenceID;
          FPersistence.Next;

          if StagedBatchingEnabled and
            ((AImportResult.ImportedVerseCount mod
              STAGED_IMPORT_BATCH_SIZE) = 0) then
            CommitStagedPairChanges(AImportResult.ImportedVerseCount,
              AImportResult.LastReference);
          PulseImportProgress(AImportResult.ImportedVerseCount,
            AImportResult.LastReference,
            ((AImportResult.ImportedVerseCount mod 75) = 0) or
            (AImportResult.ImportedVerseCount =
             AImportResult.ExpectedVerseCount));
        end;

        if AImportResult.ImportedVerseCount <>
          AImportResult.ExpectedVerseCount then
        begin
          RestoreSnapshots;
          SetFailure(Format(SModuleImportIncompleteError,
            [AImportResult.ImportedVerseCount,
             AImportResult.ExpectedVerseCount,
             AImportResult.LastReference]));
          Exit;
        end;

        if StagedBatchingEnabled and
          (AImportResult.ImportedVerseCount > 0) then
          CommitStagedPairChanges(AImportResult.ImportedVerseCount,
            AImportResult.LastReference);
      finally
        if SessionStarted then
        begin
          try
            FinishScrollingSession;
          finally
            SessionStarted := False;
          end;
        end;
      end;

      if TextTypeParticipatesInAssociations(texto) then
      begin
        if UsesActiveProgressForm then
        begin
          FormPleaseWait.SetMessage(
            SAssociationMemoryProgressMessage);
          FormPleaseWait.SetDetail('');
          TFormPleaseWait.PumpProgressPaintMessages(
            FormPleaseWait);
        end;
        RowsMayBeCommitted := True;
        RebuildAssociationMemory(pb);
      end;
      if not FConfig.HasTextDirection(Integer(texto)) then
        SetTextDirection(texto, ImportedDirection);
      if HasImportedDescription then
        FConfig.StageTextDescription(Integer(texto), ImportedDescription);
      if StageImportedProperties then
        case texto of
          tbOrigem:
            FConfig.WriteString('propriedades.origem',
              ImportedProperties);
          tbDestino:
            FConfig.WriteString('propriedades.destino',
              ImportedProperties);
        end;

      // A completed Association-memory rebuild still owns a write
      // transaction until Commit publishes it. Staged project creation must
      // release that writer before importing the next module; otherwise the
      // canonical dataset receives SQLITE_BUSY and cannot safely be retried.
      if StagedBatchingEnabled then
        Self.Commit;

      UpdateTree(FMarker);
      AImportResult.Status := twmisSucceeded;
      AImportResult.ErrorMessage := '';
      Result := True;
    except
      on E: Exception do
      begin
        ImportError := E.Message;
        if RowsMayBeCommitted and Assigned(FSuggestionManager) then
          try
            // A completed or aborted rebuild can still own the independent
            // Association-memory transaction. Release it before restoring
            // canonical rows through the project persistence connection.
            FSuggestionManager.Rollback;
          except
            on MemoryRollbackError: Exception do
              ImportError := ImportError + #13#10 +
                'Association-memory rollback failed: ' +
                MemoryRollbackError.Message;
          end;
        if RowsMayBeCommitted and Assigned(FPersistence) then
          try
            // ApplyUpdates invalidates its internal change list when a write
            // fails. Reload the durable baseline before replaying the saved
            // snapshots so rollback never relies on that invalid state.
            FPersistence.Rollback;
          except
            on PersistenceRollbackError: Exception do
              ImportError := ImportError + #13#10 +
                'Project persistence rollback failed: ' +
                PersistenceRollbackError.Message;
          end;
        if (SnapshotCount > 0) and not SnapshotsRestored then
          try
            RestoreSnapshots;
          except
            on RestoreError: Exception do
              ImportError := ImportError + #13#10 +
                'Rollback failed: ' + RestoreError.Message;
          end;

        if RowsMayBeCommitted and SnapshotsRestored then
          try
            FPersistence.CommitPairs;
          except
            on RestoreCommitError: Exception do
              ImportError := ImportError + #13#10 +
                'Rollback commit failed: ' + RestoreCommitError.Message;
          end;

        AImportResult.Status := twmisFailed;
        AImportResult.ErrorMessage := Format(
          SModuleImportExceptionError,
          [AImportResult.ImportedVerseCount,
           AImportResult.ExpectedVerseCount,
           AImportResult.LastReference, ImportError]);
        Result := False;
      end;
    end;
  finally
    if SessionStarted then
    begin
      try
        FinishScrollingSession;
      except
        { Preserve the primary import result while restoring UI state. }
      end;
    end;
    if Assigned(pb) and not (Assigned(FormPleaseWait) and
      FormPleaseWait.OperationActive and
      (pb = FormPleaseWait.ProgressBar1)) then
      pb.Visible := False;
    Modulo.Free;
    Propriedades.Free;
    FreeAndNil(RxVerseRulesFrom);
    FreeAndNil(RxVerseRulesTo);
    VerseRuleSources.Free;
    VerseRuleTargets.Free;
    if OwnVerseRulesForm then
      RulesForm.Free;
  end;
end;

procedure TProject.CreateVerseObject(owner: TScrollBox;
  texto: TTipoTextoBiblico);
begin
  FCurrentVerseWorkspace.CreateVerseView(owner, Ord(texto), Ord(tbOrigem),
    Ord(tbDestino), FAVersiculo, @OnDblClickVersiculo, @SintagmaOnClick,
    @OnTranslatorNoteClick, @OnExportText);
  if Assigned(FAVersiculo[texto]) then
  begin
    FAVersiculo[texto].TokenizerVersion := TokenizerVersion;
    FAVersiculo[texto].BaseDirection := GetTextDirection(texto);
    FAVersiculo[texto].OnTextDirectionChange := @OnTextDirectionChange;
    FAVersiculo[texto].OnVerseChanged := FOnAlterarVersiculo;
    AttachProjectVersePopupMenu(texto);
  end;
end;

procedure TProject.ExportDestinationTextWithStrongs(arquivo: string; opcoes: TOpcoesExportacao);
begin
  ExportDestinationTextWithStrongs(arquivo, nil, opcoes);
end;

procedure TProject.ExportDestinationTextWithStrongs(arquivo: string;
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
    Options.Properties := ReadInfo('propriedades.destino');
    Options.Direction := GetTextDirection(tbDestino);
    Options.ErrorDialogTitle := SError;
    Options.CorruptedDataMessage := SCorruptedData;
    Options.ExportToFileErrorMessage := SExportToFileError;
    Options.ErrorReportFileName := ChangeFileExt(arquivo, '.errors.txt');
    Options.ErrorReportCreatedMessage := SExportErrorReportCreated;

    FProjectExporter.ExportDestinationTextWithStrongs(arquivo, pb, Options,
      @StartScrollingSession, @FinishScrollingSession, @GetID);
  finally
    FExportando := false;
    AfterVerseScroll(nil);
  end;
end;

procedure TProject.ExportInterlinearText(arquivo: string; opcoes: TOpcoesExportacao);
begin
  ExportInterlinearText(arquivo, nil, opcoes);
end;

{ TODO: Unify this code with ExportDestinationTextWithStrongs() }
procedure TProject.ExportInterlinearText(arquivo: string;
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

    Options.Properties := ReadInfo('propriedades.origem');
    Options.Direction := GetTextDirection(tbOrigem);
    Options.ErrorDialogTitle := SError;
    Options.CorruptedDataMessage := SCorruptedData;
    Options.ExportToFileErrorMessage := SExportToFileError;
    Options.VerseRuleReplaceErrorMessage := SInterlinearVerseRuleReplaceError;
    Options.ErrorReportFileName := ChangeFileExt(arquivo, '.errors.txt');
    Options.ErrorReportCreatedMessage := SExportErrorReportCreated;

    FProjectExporter.ExportInterlinearText(arquivo, pb, rulesRx, replaceTo,
      Options, @StartScrollingSession, @FinishScrollingSession, @GetID);
  finally
    rulesRx.Free;
    replaceTo.Free;
    FExportando := false;
    AfterVerseScroll(nil);
  end;
end;

procedure TProject.ExportConcordance(arquivo: string;
  opcoes: TOpcoesExportacao; abbreviation: string);
begin
  ExportConcordance(arquivo, nil, opcoes, abbreviation);
end;

procedure TProject.ExportConcordance(arquivo: string;
  pb: TProgressBar; opcoes: TOpcoesExportacao; abbreviation: string;
  IncludeVerseContext: Boolean; UseIndexedOccurrences: Boolean;
  ProgressEvent: TConcordanceExportProgressEvent);
var
  ContextMode: TConcordanceContextMode;
begin
  if IncludeVerseContext then
    ContextMode := ccmFullVerse
  else
    ContextMode := ccmReferencesOnly;
  ExportConcordance(arquivo, pb, opcoes, abbreviation, ContextMode,
    CONCORDANCE_CONTEXT_DEFAULT_CHARACTERS, UseIndexedOccurrences,
    ProgressEvent);
end;

procedure TProject.ExportConcordance(arquivo: string;
  pb: TProgressBar; opcoes: TOpcoesExportacao; abbreviation: string;
  ContextMode: TConcordanceContextMode; ContextMaxCharacters: Integer;
  UseIndexedOccurrences: Boolean;
  ProgressEvent: TConcordanceExportProgressEvent);
var
  Options: TConcordanceExportOptions;
begin
  if (FAVersiculo[tbOrigem] = nil) or (FAVersiculo[tbDestino] = nil) then
    exit;

  try
    FExportando := true;

    Options.Detailed := oeConcordDetalhada in opcoes;
    if Options.Detailed then
      Options.Mode := cemAnalytical
    else
      Options.Mode := cemSynthetic;
    Options.ContextMode := ContextMode;
    Options.ContextMaxCharacters := ContextMaxCharacters;
    Options.UseIndexedOccurrences := UseIndexedOccurrences;
    Options.Abbreviation := abbreviation;
    Options.LanguageId := SLanguageId;
    Options.AnalyticalCaption := SAnalyticalConcordance;
    Options.SyntheticCaption := SSyntheticConcordance;
    Options.ErrorDialogTitle := SError;
    Options.CorruptedDataMessage := SCorruptedData;
    Options.ErrorReportFileName := ChangeFileExt(arquivo, '.errors.txt');
    Options.ErrorReportCreatedMessage := SExportErrorReportCreated;

    FProjectExporter.ExportConcordance(arquivo, pb, Options,
      @StartScrollingSession, @FinishScrollingSession, @GetID,
      FSuggestionManager, ProgressEvent);
  finally
    FExportando := false;
    AfterVerseScroll(nil);
  end;
end;

procedure TProject.AssignTextFont(fonte: TFont; textos: STextosBiblicos);
var
  t: TTipoTextoBiblico;
begin
  for t:=tbOrigem to tbConsulta2 do
    if t in textos then
    begin
      FAVersiculo[t].Font := fonte;
      SetInfo(format('fonte%d.nome', [t]), fonte.Name);
      SetInfo(format('fonte%d.tamanho', [t]), IntToStr(fonte.Size));
      SetInfo(format('fonte%d.bold', [t]), IntToStr(Ord(fonte.Bold)));
    end;
end;

procedure TProject.ClearText(texto: TTipoTextoBiblico);
begin
  if texto in [tbOrigem, tbDestino] then
  begin
    FAssociationMemory.MarkSourceDirty;
    FAssociationMemory.InvalidateReplay;
  end;
  StartScrollingSession;
  try
      //UpdateTree;
    FPatchApplier.ClearTextFieldForAll(FACamposTexto[texto], FACamposTexto[tbOrigem],
      FACamposTexto[tbDestino], StatusAutomationEnabled);
  finally
    FinishScrollingSession;
  end;
  if texto in [tbOrigem, tbDestino] then
  begin
    RebuildAssociationMemory(nil);
  end;
  FConfig.StageTextDescription(Integer(texto), '');
end;

function TProject.GetTextFont(texto: TTipoTextoBiblico): TFont;
begin
   if assigned(FAVersiculo[texto]) then
     result := FAVersiculo[texto].Font
   else
     result := nil;
end;

function TProject.AssignStrongDictionary(dic: string; textos: STextosBiblicos): boolean;
var
  t: TTipoTextoBiblico;
begin
  result := false;
  for t:=tbOrigem to tbConsulta2 do
  begin
    if t in textos then
    begin
      if not AssignStrongDictionary(dic, t) then
        exit;
      SetInfo(format('dicstrong%d', [t]), dic);
    end;
  end;
  result := true;
end;

function TProject.AssignMorphologyDictionary(dic: string; textos: STextosBiblicos): boolean;
var
  t: TTipoTextoBiblico;
begin
  result := false;
  for t:=tbOrigem to tbConsulta2 do
  begin
    if t in textos then
    begin
      if not AssignMorphologyDictionary(dic, t) then
        exit;
      SetInfo(format('dicmorfo%d', [t]), dic);
    end;
  end;
  result := true;
end;

procedure TProject.SetInfo(info, valor: string);
begin
  FConfig.WriteImmediateString(info, valor);
end;

function TProject.PrepareConcordanceExport(
  UseIndexedOccurrences: Boolean): Boolean;
begin
  Result := False;
  if not UseIndexedOccurrences or not Assigned(FAssociationMemory) or
    not Assigned(FSuggestionManager) then
    Exit;

  try
    BeforeVerseScroll(nil);
    EnsureAssociationMemoryFresh;
    Result := not FAssociationMemory.NeedsRebuild;
  except
    // The canonical association data remains a safe export fallback. A cache
    // validation or rebuild failure must not prevent concordance generation.
    Result := False;
  end;
end;

function TProject.GetInfo(info: string): string;
begin
  Result := FConfig.ReadString(info);
end;

function TProject.GetFlowExamples(const FlowId: string): TAIFlowExampleArray;
begin
  Result := LoadFlowExamples(GetInfo(GetFlowExamplesConfigKey(FlowId)));
end;

procedure TProject.SetFlowExamples(const FlowId: string; const Examples: TAIFlowExampleArray);
begin
  SetInfo(GetFlowExamplesConfigKey(FlowId), SaveFlowExamples(Examples));
end;

function TProject.TryCreateCurrentVerseFlowExample(const FlowId: string;
  out Example: TAIFlowExample; out ErrorMessage: string): Boolean;
begin
  Result := False;
  ErrorMessage := '';
  Example := Default(TAIFlowExample);
  try
    Example := CreateFlowExample(FlowId);
    Result := True;
  except
    on E: Exception do
      ErrorMessage := E.Message;
  end;
end;

function TProject.IncludeCurrentVerseAsFlowExample(const FlowId: string): Boolean;
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

procedure TProject.RemoveFlowExample(const FlowId, ReferenceId: string);
var
  Examples: TAIFlowExampleArray;
begin
  Examples := GetFlowExamples(FlowId);
  Examples := ai_flow_config.RemoveFlowExample(Examples, ReferenceId);
  SetFlowExamples(FlowId, Examples);
end;

procedure TProject.ClearFlowExamples(const FlowId: string);
var
  Examples: TAIFlowExampleArray;
begin
  Examples := nil;
  SetFlowExamples(FlowId, Examples);
end;

function TProject.GetVerseText(texto: TTipoTextoBiblico): string;
begin
  Result := FPersistence.CurrentText(FACamposTexto[texto]);
end;

function TProject.GetVerseText(Referencia: string; texto: TTipoTextoBiblico): string;
begin
  result := SVerseAbsentFromProject;
  StartScrollingSession;
  try
    FVerseRepository.TryReadText(Referencia, FACamposTexto[texto], Result);
  finally
    FinishScrollingSession;
  end;
end;

function TProject.GetPlainVerseText(texto: TTipoTextoBiblico): string;
begin
  result := GetPlainVerseText('', texto);
end;

function TProject.GetPlainVerseText(Referencia: string;
  texto: TTipoTextoBiblico): string;
var
  varredorXML: TONTTokenizer;
  s: TSyntagmToken;
  SourceText: string;
  RFBody: string;
  RFClosingTag: string;
begin
  result := '';

  if Referencia = '' then
    SourceText := GetVerseText(texto)
  else
    SourceText := GetVerseText(Referencia, texto);

  varredorXML := TTokenizerFactory.CreateTokenizer(TokenizerVersion, SourceText);
  try
    while varredorXML.ReadSyntagm(s) <> skNull do
    begin
      if s.Kind = skTag then
      begin
        if IsRFOpeningTag(s.Value) then
        begin
          ReadRFNoteBody(varredorXML, RFBody, RFClosingTag);
          s.Kind := skMetadata;
          continue;
        end else if AnsiStartsStr('<TS', s.Value) then
        begin
          varredorXML.ReadToTag(s, '<Ts>');
          s.Kind := skMetadata;
          continue;
        end;
      end;
      if s.Kind in [skWhitespace, skSyntagm, skPunctuation] then
        result := result + s.Value;
    end;
  finally
    varredorXML.Destroy;
  end;
end;

function TProject.GetTranslationSuggestions(syntagm: TSyntagm): string;
begin
  if EnsureAssociationMemoryFresh then
    Exit('');
  result := FSuggestionManager.GetTranslationAlternatives(syntagm);
end;

function TProject.TryGetCurrentVerseTranslationMemory(
  Order: TTranslationMemoryOrder;
  out Entries: TTranslationMemoryEntryArray;
  out ErrorMessage: string): Boolean;
begin
  Result := False;
  ErrorMessage := '';
  SetLength(Entries, 0);
  if not Assigned(FSuggestionManager) or not Assigned(FAssociationMemory) or
     not Assigned(FAVersiculo[tbOrigem]) then
  begin
    ErrorMessage := SAssociationMemoryUnavailable;
    Exit;
  end;

  try
    EnsureAssociationMemoryFresh;
    if FAssociationMemoryRebuildRunning or FAssociationMemory.RebuildActive or
       FAssociationMemory.NeedsRebuild or
       not FAssociationMemory.CacheValidated then
    begin
      ErrorMessage := SAssociationMemoryUnavailable;
      Exit;
    end;
    Entries := TTranslationMemoryModel.Build(FAVersiculo[tbOrigem],
      FSuggestionManager, Order);
    Result := True;
  except
    on E: Exception do
    begin
      SetLength(Entries, 0);
      ErrorMessage := E.Message;
    end;
  end;
end;

function TProject.TryGetTranslationMemoryOccurrences(
  const Matches: TTranslationPairMatchArray; Limit: Integer;
  out Occurrences: TTranslationOccurrencePage;
  out ErrorMessage: string): Boolean;
var
  ProjectionBuffer: TVerseRenderBuffer;
  ItemIndex: Integer;

  function PlainDestinationText(const XML: string): string;
  var
    RFBody: string;
    RFClosingTag: string;
    Token: TSyntagmToken;
    Tokenizer: TONTTokenizer;
  begin
    Result := '';
    Tokenizer := TTokenizerFactory.CreateTokenizer(TokenizerVersion, XML);
    try
      while Tokenizer.ReadSyntagm(Token) <> skNull do
      begin
        if Token.Kind = skTag then
        begin
          if IsRFOpeningTag(Token.Value) then
          begin
            ReadRFNoteBody(Tokenizer, RFBody, RFClosingTag);
            Continue;
          end;
          if AnsiStartsStr('<TS', Token.Value) then
          begin
            Tokenizer.ReadToTag(Token, '<Ts>');
            Continue;
          end;
        end;
        if Token.Kind in [skWhitespace, skSyntagm, skPunctuation] then
          Result := Result + Token.Value;
      end;
    finally
      Tokenizer.Free;
    end;
    Result := Trim(Result);
  end;

  function FormatOccurrenceReference(const ReferenceID: string): string;
  var
    BookNumber: Integer;
    ChapterNumber: Integer;
    Parts: TStringList;
    VerseNumber: Integer;
  begin
    Result := ReferenceID;
    Parts := TStringList.Create;
    try
      Parts.StrictDelimiter := True;
      Parts.Delimiter := ',';
      Parts.DelimitedText := ReferenceID;
      if (Parts.Count = 3) and TryStrToInt(Parts[0], BookNumber) and
         TryStrToInt(Parts[1], ChapterNumber) and
         TryStrToInt(Parts[2], VerseNumber) then
        Result := Format('%s %d:%d',
          [LocalizedBookNameByID(BookNumber), ChapterNumber, VerseNumber]);
    finally
      Parts.Free;
    end;
  end;

  procedure BuildPlainOccurrenceText(RenderBuffer: TVerseRenderBuffer;
    const OriginalText, DestinationText: string; out PlainText: string);
  var
    IgnoredRanges: TConcordanceHighlightRangeArray;
  begin
    try
      RenderBuffer.LoadTexts(OriginalText, DestinationText);
      BuildVisibleDestinationProjection(RenderBuffer.DestinationVerse,
        PlainText, IgnoredRanges);
    except
      PlainText := PlainDestinationText(DestinationText);
    end;
  end;

  procedure ProjectOccurrence(var Occurrence: TTranslationOccurrenceData;
    RenderBuffer: TVerseRenderBuffer);
  var
    AssociationData: TAssociationPairDataArray;
    Associations: string;
    CorruptionDetails: string;
    DestinationText: string;
    HighlightRanges: TConcordanceHighlightRangeArray;
    OriginalText: string;
    SyntagmRanges: TConcordanceHighlightRangeArray;
    VisibleText: string;
  begin
    OriginalText := Occurrence.OriginalText;
    DestinationText := Occurrence.DestinationText;
    Associations := Occurrence.Associations;
    if (Occurrence.ReferenceID = ID) and
       Assigned(FAVersiculo[tbOrigem]) and
       Assigned(FAVersiculo[tbDestino]) then
    begin
      OriginalText := FAVersiculo[tbOrigem].XML;
      DestinationText := FAVersiculo[tbDestino].XML;
      Associations := FAVersiculo[tbOrigem].Pairs;
    end;

    Occurrence.HighlightRanges := nil;
    AssociationData := nil;
    if RenderBuffer.TryExtractAssociationPairData(OriginalText,
        DestinationText, Associations, plMetaData, AssociationData,
        CorruptionDetails) then
    begin
      BuildVisibleDestinationProjection(RenderBuffer.DestinationVerse,
        VisibleText, SyntagmRanges);
      HighlightRanges := nil;
      if TryProjectMatchingAssociationHighlightRanges(AssociationData,
          Matches, VisibleText, SyntagmRanges, HighlightRanges) then
        Occurrence.HighlightRanges := HighlightRanges;
    end
    else
      BuildPlainOccurrenceText(RenderBuffer, OriginalText,
        DestinationText, VisibleText);

    Occurrence.DestinationText := VisibleText;
  end;

begin
  Result := False;
  ErrorMessage := '';
  Occurrences.TotalAssociations := 0;
  Occurrences.TotalVerses := 0;
  Occurrences.HasMore := False;
  SetLength(Occurrences.Items, 0);
  if not Assigned(FSuggestionManager) or not Assigned(FAssociationMemory) then
  begin
    ErrorMessage := SAssociationMemoryUnavailable;
    Exit;
  end;

  try
    EnsureAssociationMemoryFresh;
    if FAssociationMemoryRebuildRunning or FAssociationMemory.RebuildActive or
       FAssociationMemory.NeedsRebuild or
       not FAssociationMemory.CacheValidated then
    begin
      ErrorMessage := SAssociationMemoryUnavailable;
      Exit;
    end;

    Occurrences := FSuggestionManager.GetTranslationOccurrences(Matches,
      Limit);
    ProjectionBuffer := TVerseRenderBuffer.Create;
    try
      ProjectionBuffer.SetTokenizerVersion(TokenizerVersion);
      for ItemIndex := 0 to Length(Occurrences.Items) - 1 do
      begin
        Occurrences.Items[ItemIndex].DisplayReference :=
          FormatOccurrenceReference(
            Occurrences.Items[ItemIndex].ReferenceID);
        ProjectOccurrence(Occurrences.Items[ItemIndex], ProjectionBuffer);
      end;
    finally
      ProjectionBuffer.Free;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      Occurrences.TotalAssociations := 0;
      Occurrences.TotalVerses := 0;
      Occurrences.HasMore := False;
      SetLength(Occurrences.Items, 0);
      ErrorMessage := E.Message;
    end;
  end;
end;

function TProject.HasStoredAssociations: Boolean;
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

procedure TProject.ClearStoredAssociations(ProgressBar: TProgressBar);
var
  shouldResetStatus: Boolean;
begin
  if not Assigned(FPersistence) then
    Exit;

  shouldResetStatus := StatusAutomationEnabled;
  FAssociationMemory.MarkSourceDirty;
  FAssociationMemory.InvalidateReplay;
  StartScrollingSession;
  try
    ConfigureOperationProgressBar(ProgressBar, QLinhas[FEscopo]);
    FVerseRepository.First;
    while not FVerseRepository.EOF do
    begin
      if FVerseRepository.HasCurrentAssociations then
        FVerseRepository.ClearCurrentAssociations(shouldResetStatus);
      FVerseRepository.Next;
      AdvanceOperationProgressBar(ProgressBar);
    end;
  finally
    FinishScrollingSession;
    AfterVerseScroll(nil);
  end;
  RebuildAssociationMemory(ProgressBar);
end;

procedure TProject.Translate;
begin
  UpdateDefaultStatusDescription;
  if Assigned(FPersistence) and (ID <> '') then
    UpdateReference(ID);
  if Assigned(FVerseMemo) then
    FVerseMemo.Translate;
  if not assigned(FArvore) then
    exit;

  RefreshStatusImages;
  RefreshStatusControl;
  if Assigned(FPersistence) then
    UpdateTree
  else
    FBibleTreeCoordinator.RefreshCaptions(FEscopo);

  if assigned(FArvore.PopupMenu) then
  begin
    FArvore.PopupMenu.Free;
    FArvore.PopupMenu := CreateBibleTreeContextMenu;
  end;
end;

procedure TProject.ToggleDisplayTags;
begin
  SetDisplayTags(not DisplayTags);
end;

procedure TProject.StartScrollingSession;
begin
  FMarker := ID;
  BeforeVerseScroll(nil);
  ScrollEventsEnabled := false;
  FExportando := True;
end;

procedure TProject.FinishScrollingSession;
begin
  GoToReference(FMarker);
  ScrollEventsEnabled := true;
  FExportando := False;
end;

procedure TProject.RewindChapter;
begin
  if Verse <> 1 then
    GoToReference(Format('%d,%d,%d', [BookID, Chapter, 1]));
end;

end.
