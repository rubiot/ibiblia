unit formPrincipal;

{$mode objfpc}{$H+}

{ TODO: Determine whether syntagm association order matters. }

interface

uses
  {$IFDEF WINDOWS}
  windows, twAutomate, TwSyncThread, fileassociation, ShellApi,
  {$ELSE}
  lclintf,
  {$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, ExtCtrls, StdCtrls, BibleScope, Project, IniFiles, Math,
  Syntagm, LCLTranslator, unitabout, PCRE, Verse, LCLProc, LazLogger,
  LCLType, LazUTF8, ChapterView, UnitPleaseWait, CommentsView, ONTTokenizer,
  status_catalog, SynEdit, project_setup_model, translation_memory_model,
  project_creation_staging;

type
  { TMainForm }

  TMainForm = class(TForm)
  ActionKeyboardShortcuts: TAction;
  ActionGuideMode: TAction;
  ActionHistoryBack: TAction;
  ActionHistoryForward: TAction;
  ActionLlmOptions: TAction;
  ActionResetLayout: TAction;
  ActionTheWordHighlighterColors: TAction;
    ActionUnregisterFileAssociation: TAction;
    ActionRegisterFileAssociation: TAction;
    ActionVaccumProject: TAction;
    ActionApplyPatch: TAction;
    ActionExportPatch: TAction;
    ActionShowTags: TAction;
    ActionSyncTheWordVerse: TAction;
    ActionRebuildSuggestionDatabase: TAction;
    ActionMergeProjects: TAction;
    ActionExportProject: TAction;
    ActionProjectProperties: TAction;
    ActionSaveProjectAs: TAction;
    ActionExportInterlinearText: TAction;
    ActionExportDestinationWithStrongs: TAction;
    ActionLastVerse: TAction;
    ActionFirstVerse: TAction;
    ActionClearAssociations: TAction;
    ActionRollbackCurrentVerse: TAction;
    ActionSaveProject: TAction;
    ActionNewVerse: TAction;
    ActionSuggestAssociation: TAction;
    ActionPreviousVerse: TAction;
    ActionNextVerse: TAction;
    ActionCloseProject: TAction;
    ActionExit: TAction;
    ActionOpenProject: TAction;
    ActionNewProject: TAction;
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
    MenuItem18: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItemUnregisterFileAssoc: TMenuItem;
    MenuItemRegisterFileAssoc: TMenuItem;
    MenuItemStatisticalSuggestion: TMenuItem;
    MenuItemAISuggestion: TMenuItem;
    MenuItemApplyPatch: TMenuItem;
    MenuItemExportPatch: TMenuItem;
    MenuItemAutoSave: TMenuItem;
    MenuItemStrongsCountNone: TMenuItem;
    MenuItemStrongsCountWords: TMenuItem;
    MenuItemStrongsCountStrongs: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemHistoryBack: TMenuItem;
    MenuItemHistoryForward: TMenuItem;
    MenuItemHistorySeparator: TMenuItem;
    MenuItemGuideMode: TMenuItem;
    MenuItemKeyboardShortcuts: TMenuItem;
    MenuItemResetLayout: TMenuItem;
    MenuItemTheWordHighlighterColors: TMenuItem;
    MenuItemMouseHover: TMenuItem;
    MenuItem19: TMenuItem;
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
    MenuItemBoldStrongs: TMenuItem;
    MenuItemRecent: TMenuItem;
  MenuItemLlmOptions: TMenuItem;
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
    NavigationPanel: TPanel;
    CenterPanel: TPanel;
    BottomPanel: TPanel;
    ContextPanel: TPanel;
    ProgressBar1: TProgressBar;
    RadioGroupStatus: TComboBox;
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
    StatusPanel: TPanel;
    ChapterViewTabCtrl: TTabControl;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButtonExit: TToolButton;
    ToolButtonHistoryBack: TToolButton;
    ToolButtonHistoryForward: TToolButton;
    ToolButtonHistorySeparator: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TreeLegendLabel: TLabel;
    TreeLegendPanel: TPanel;
    TreeView1: TTreeView;
    procedure ActionApplyPatchExecute(Sender: TObject);
    procedure ActionExportProjectExecute(Sender: TObject);
    procedure ActionOpenProjectExecute(Sender: TObject);
    procedure ActionExportDestinationWithStrongsExecute(Sender: TObject);
    procedure ActionExportInterlinearTextExecute(Sender: TObject);
    procedure ActionExportPatchExecute(Sender: TObject);
    procedure ActionCloseProjectExecute(Sender: TObject);
    procedure ActionHistoryBackExecute(Sender: TObject);
    procedure ActionHistoryForwardExecute(Sender: TObject);
    procedure ActionKeyboardShortcutsExecute(Sender: TObject);
    procedure ActionClearAssociationsExecute(Sender: TObject);
    procedure ActionMergeProjectsExecute(Sender: TObject);
    procedure ActionShowTagsExecute(Sender: TObject);
    procedure ActionNewProjectExecute(Sender: TObject);
  procedure ActionLlmOptionsExecute(Sender: TObject);
    procedure ActionTheWordHighlighterColorsExecute(Sender: TObject);
    procedure ActionProjectPropertiesExecute(Sender: TObject);
    procedure ActionNewVerseExecute(Sender: TObject);
    procedure ActionRebuildSuggestionDatabaseExecute(Sender: TObject);
    procedure ActionResetLayoutExecute(Sender: TObject);
    procedure ActionRegisterFileAssociationExecute(Sender: TObject);
    procedure ActionUnregisterFileAssociationExecute(Sender: TObject);
    procedure ActionRollbackCurrentVerseExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionSaveProjectAsExecute(Sender: TObject);
    procedure ActionSaveProjectExecute(Sender: TObject);
    procedure ActionSuggestAssociationExecute(Sender: TObject);
    procedure ActionSyncTheWordVerseExecute(Sender: TObject);
    procedure ActionVaccumProjectExecute(Sender: TObject);
    procedure ActionPreviousVerseExecute(Sender: TObject);
    procedure ActionFirstVerseExecute(Sender: TObject);
    procedure ActionNextVerseExecute(Sender: TObject);
    procedure ActionLastVerseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure RecentProjectClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemAISuggestionClick(Sender: TObject);
    procedure MenuItemAlwaysOnTopClick(Sender: TObject);
    procedure MenuItemAutoSaveClick(Sender: TObject);
    procedure MenuItemLangEnClick(Sender: TObject);
    procedure MenuItemLangPtClick(Sender: TObject);
    procedure MenuItemPopupTriggerClick(Sender: TObject);
    procedure MenuItemStatisticalSuggestionClick(Sender: TObject);
    procedure MenuItemBoldStrongsClick(Sender: TObject);
    procedure MenuItemStrongsCountClick(Sender: TObject);
    procedure MenuItemSynciBibliaClick(Sender: TObject);
    procedure MenuItemSyncTheWordClick(Sender: TObject);
    procedure ProjectNewVerse(Sender: TProject);
    procedure SyntagmClick(Sender: TSyntagm);
    procedure VerseChanged;
    procedure UpdateRecentProjects(m: TMenuItem);
    procedure LoadRecentProjects(m: TMenuItem);
    procedure SaveRecentProjects(m: TMenuItem);
    procedure RadioGroupStatusDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure RadioGroupStatusSelectionChanged(Sender: TObject);
    procedure StatusReviewListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ChapterViewTabCtrlChange(Sender: TObject);
    procedure ToolButtonExitClick(Sender: TObject);
    procedure UpdateFileAssociationMenuItems;
  private
    { private declarations }
    FActualCommentsMemo: TCommentsView; // The current comments view component
    FCommentsGroupBox: TGroupBox;
    FPopupTrigger: TPopupTrigger;
    FCurrentRef: string;
    FChapterView: TChapterView;
    FTranslationMemoryOrder: TTranslationMemoryOrder;
    FTranslationMemoryOrderMenu: TMenuItem;
    FTranslationMemoryVerseOrderItem: TMenuItem;
    FTranslationMemoryRareFirstItem: TMenuItem;
    FDefaultVerseMode: TDefaultVerseMode;
    FDefaultVerseModeMenu: TMenuItem;
    FDefaultVerseModeAssociationItem: TMenuItem;
    FDefaultVerseModeEditingItem: TMenuItem;
    FStateHeaderPanel: TPanel;
    FHeaderReferenceLabel: TLabel;
    FHeaderStatusLabel: TLabel;
    FHeaderAssociationsLabel: TLabel;
    FHeaderSelectionLabel: TLabel;
    FHeaderEngineLabel: TLabel;
    FHeaderModifiedLabel: TLabel;
    FGuidePanel: TPanel;
    FGuideTitleLabel: TLabel;
    FGuideTextLabel: TLabel;
    FGuideHideButton: TButton;
    FGuideModeEnabled: Boolean;
    FStatusReviewList: TListBox;
    FStatusReviewChecked: array of Boolean;
    FStatusReviewResetButton: TButton;
    FUpdatingStatusReview: Boolean;
    FVersePanelLabels: array[tbOrigem..tbConsulta2] of TPanel;
    FVersePanelTypeLabels: array[tbOrigem..tbConsulta2] of TLabel;
    FVersePanelDescriptionLabels: array[tbOrigem..tbConsulta2] of TLabel;
    {$IFDEF WINDOWS}
    FRxMorpho: IRegex;
    FSyncTheWordToIBiblia: boolean;
    FSyncIBibliaToTheWord: boolean;
    FTheWordSyncThread: TTwSyncThread;
    FSuggestionMode: TSuggestionMode;

    procedure SyncToTwRef(Ref: string);
    procedure SetUpSyncThread;
    {$ENDIF}
    procedure NormalizeActionHints;
    procedure UpdateNavigationHistoryActions;
    procedure ActionGuideModeExecute(Sender: TObject);
    function BuildKeyboardShortcutsText: string;
    procedure CreateGuidePanel;
    procedure CreateStatusReviewPanel;
    function CreateVersePanelLabel(AParent: TWinControl;
      TextType: TTipoTextoBiblico): TPanel;
    function CreateStateHeaderLabel(AParent: TWinControl; Bold: Boolean = False): TLabel;
    procedure AttachStatusComboToHeader;
    procedure CreateCommentsGroupBox;
    procedure CreateStateHeader;
    procedure CreateVersePanelLabels;
    procedure CountCurrentAssociations(out Associated, Total: Integer);
    function CurrentGuideScenario: TProjectScenario;
    function CurrentEngineCaption: string;
    function CurrentSelectionCaption: string;
    function GetVersePanelCaption(TextType: TTipoTextoBiblico): string;
    function GetVersePanelDescription(TextType: TTipoTextoBiblico): string;
    function GetVersePanelTypeCaption(TextType: TTipoTextoBiblico): string;
    function LocalizedProjectDescription(const Description: string): string;
    procedure GuideHideButtonClick(Sender: TObject);
    procedure GuidePanelResize(Sender: TObject);
    procedure LayoutGuidePanel;
    procedure LayoutStateHeader;
    procedure LayoutVersePanelLabels;
    procedure ResetMainLayout;
    procedure SetGuideModeEnabled(AEnabled: Boolean);
    procedure SplitterMoved(Sender: TObject);
    procedure StateHeaderResize(Sender: TObject);
    function StatusReviewItemChecked(Index: Integer): Boolean;
    procedure SetStatusReviewItemChecked(Index: Integer; AChecked: Boolean);
    procedure ToggleStatusReviewItem(Index: Integer);
    procedure StatusReviewListClickCheck(Sender: TObject);
    procedure StatusReviewListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StatusReviewResetButtonClick(Sender: TObject);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeViewShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure TranslationMemoryOrderClick(Sender: TObject);
    procedure DefaultVerseModeClick(Sender: TObject);
    procedure HandleTranslationMemoryPreviewFontSizeChanged(Sender: TObject);
    procedure Translate;
    procedure NormalizeActionHint(AAction: TCustomAction);
    procedure UpdateMainCaption;
    procedure UpdateGuidePanel;
    procedure UpdateStateHeader;
    procedure UpdateStatusReviewPanel;
    procedure UpdateTranslationMemoryOrderMenu;
    procedure UpdateDefaultVerseModeMenu;
    procedure SetDefaultVerseMode(AMode: TDefaultVerseMode);
    procedure ApplyStatusReviewFilter;
    procedure DoOpenProject(filename: string);
    procedure CheckFileAssociation;
  public
    { public declarations }
    language: string;
  end;

var
  MainForm: TMainForm;
  CurrentProject: TProject;
  opts: TIniFile;
  FStrongsCountMode: TStrongsCountMode;

const
  MAX_MRU = 10;
  CHAPTER_VIEW_MEMORY_TAB_INDEX = 5;

resourcestring
  SMainCaption = 'iBiblia';
  SMainCaptionProjectFmt = 'iBiblia | %s | %s (%s)';
  SProjectFileTypeName = 'iBiblia Project File';
  STheWordHighlightColorsMenu = 'theWord highlight colors...';
  SRollbackCurrentVerse = 'Rollback current verse';
  SRollbackCurrentVerseHint = 'Discard unsaved changes in the current verse';
  SRollbackCurrentVerseConfirmation = 'This will discard unsaved changes in the current verse only (%s) and reload it from the project file.'#13#10#13#10 +
                                      'Other verses and saved project data will not be changed.'#13#10#13#10 +
                                      'Continue?';
  SButtonYes = 'Yes';
  SButtonCancel = 'Cancel';
  SClearCurrentVerseAssociations = 'Clear associations in current verse';
  SClearCurrentVerseAssociationsHint = 'Remove all associations from the current verse only';
  SClearCurrentVerseAssociationsConfirmation = 'This will remove all associations from the current verse only (%s).'#13#10#13#10 +
                                               'Associations in other verses will not be changed.'#13#10#13#10 +
                                               'Continue?';
  SSourceText = 'Source';
  SDestinationText = 'Destination';
  SReference1Text = 'Reference1';
  SReference2Text = 'Reference2';
  SInterlinearText = 'Interlinear';
  SMemoryText = 'Memory';
  STranslationMemoryOrderMenu = 'Translation memory order';
  STranslationMemoryVerseOrder = 'Verse order';
  STranslationMemoryRareFirst = 'Least frequent first';
  SDefaultVerseModeMenu = 'Default verse mode';
  SDefaultVerseModeHint = 'Choose how verses open by default';
  SDefaultVerseModeAssociation = 'Association';
  SDefaultVerseModeAssociationHint = 'Open each verse in association mode';
  SDefaultVerseModeEditing = 'WYSIWYG editing';
  SDefaultVerseModeEditingHint = 'Open Destination in the WYSIWYG editor when navigating to a verse';
  SRegisterFileAssociationSuccess = 'Successfully registered file association for .bib files';
  SRegisterFileAssociationFailure = 'Failed to register file association for .bib files';
  SUnregisterFileAssociationSuccess = 'Successfully removed file association for .bib files';
  SUnregisterFileAssociationFailure = 'Failed to remove file association for .bib files';
  SRegisterFileAssociation = 'Register .bib file association';
  SUnregisterFileAssociation = 'Unregister .bib file association';
  SRegisterFileAssociationTakeOverPrompt = 'The .bib extension is currently associated with: %s.'#13#10 +
                                           'Do you want to take over the association for iBiblia?';
  SRegisterFileAssociationAdminRetry = 'Failed to register file association. This operation requires administrator rights.'#13#10 +
                                       'Do you want to retry as administrator?';
  SSuggestionMode = 'Suggestion mode';
  SStatisticalSuggestion = 'Statistical model';
  SAISuggestion = 'AI-assisted (experimental)';
  SStatisticalSuggestionHint = 'Use the traditional statistical suggestion engine';
  SAISuggestionHint = 'Use the new AI-assisted suggestion engine (requires internet)';
  SLLMOptionsMenu = 'LLM options...';
  SGuideModeMenu = 'Guide mode';
  SGuideModeHint = 'Show contextual guidance for the main workflow';
  SHideGuideButton = 'Hide guide';
  SGuideNoProjectTitle = 'Guide: start a project';
  SGuideNoProjectText = 'Create or open a project. Association projects copy Strong''s and morphology from Source to Destination; translation projects let you write Destination first.';
  SGuideAssociationTitle = 'Guide: associate words';
  SGuideAssociationNoSelectionText = 'Click a word in the source text to begin.';
  SGuideAssociationSelectedTitle = 'Guide: complete the association';
  SGuideAssociationSelectedAddText = 'To add words to the selection, hold Ctrl and click another word in the source text.';
  SGuideAssociationSelectedAssociateText = 'To associate this selection with the other text, hold Shift and select the equivalent word(s) in the destination text. Hold Ctrl too to add words to the selection.';
  SGuideAssociationSelectedQuickText = 'To associate this selection with one destination word and select the next word, right-click the equivalent word in the other text.';
  SGuideAssociationExistingTitle = 'Guide: adjust the association';
  SGuideAssociationExistingDisassociateText = 'To disassociate, press Ctrl+Shift and click each associated word.';
  SGuideAssociationExistingAddText = 'To add words to the association, hold Ctrl+Shift and select new words.';
  SGuideAssociationExistingRemoveText = 'To remove words from the association, press Ctrl+Shift and click the word.';
  SGuideTranslationTitle = 'Guide: translate the verse';
  SGuideTranslationText = 'Double-click an empty area in Destination to write the translation. Use Save to apply or Cancel to discard; Esc also saves and closes. Once Destination has text, the guide will switch to association.';
  SKeyboardShortcutsMenu = 'Keyboard shortcuts...';
  SKeyboardShortcutsHint = 'Show keyboard shortcuts';
  SKeyboardShortcutsTitle = 'Keyboard shortcuts';
  SShortcutProjectSection = 'Project';
  SShortcutNavigationSection = 'Navigation';
  SShortcutEditingSection = 'Editing and review';
  SShortcutHelpSection = 'Help';
  SShortcutNewProject = 'New project';
  SShortcutOpenProject = 'Open project';
  SShortcutSaveProject = 'Save project';
  SShortcutCloseProject = 'Close project';
  SShortcutProjectSettings = 'Project settings';
  SShortcutExportProject = 'Export project';
  SHistoryBack = 'Back';
  SHistoryBackHint = 'Go back in navigation history';
  SHistoryForward = 'Forward';
  SHistoryForwardHint = 'Go forward in navigation history';
  SShortcutHistoryBack = 'Back in navigation history';
  SShortcutHistoryForward = 'Forward in navigation history';
  SShortcutPreviousVerse = 'Previous verse';
  SShortcutNextVerse = 'Next verse';
  SShortcutAutoAssociate = 'Auto-associate current verse';
  SShortcutRollback = 'Rollback current verse';
  SShortcutClearAssociations = 'Clear associations in current verse';
  SShortcutToggleTags = 'Show or hide Strong''s and morphology tags';
  SShortcutStatusFmt = 'Set current verse status to %s';
  SShortcutFinishEdit = 'Finish verse text editing';
  SShortcutShowKeyboardShortcuts = 'Show this list';
  SStatusKeyboardHint = 'Use 1..9 to set the first visible statuses for the current verse';
  SStatusReviewTitle = 'Review by status';
  SStatusReviewFilterHint = 'Check statuses to show matching verses in the tree';
  SStatusReviewNoProject = 'Open a project to review statuses';
  SStatusReviewItemFmt = '%d  %s';
  SStatusReviewShowingFmt = 'Showing: %d/%d';
  SStatusReviewShowAll = 'All';
  SStatusReviewShowAllHint = 'Show all statuses in the tree';
  SHeaderNoProject = 'No project open';
  SHeaderReferenceFmt = 'Reference: %s';
  SHeaderStatusFmt = 'Status: %s';
  SHeaderStatusEmpty = 'Status: -';
  SHeaderAssociationsFmt = 'Associations: %d/%d (%d%%)';
  SHeaderAssociationsEmpty = 'Associations: -';
  SHeaderSelectionFmt = 'Selection: source %d, destination %d';
  SHeaderSelectionNone = 'Selection: none';
  SHeaderEngineFmt = 'Engine: %s';
  SHeaderEngineStatistical = 'Statistical';
  SHeaderEngineAI = 'AI-assisted';
  SHeaderSaved = 'Saved';
  SHeaderUnsavedChanges = 'Unsaved changes';
  SResetLayoutMenu = 'Reset layout';
  SResetLayoutHint = 'Restore the default panel layout';
  SPanelLabelSource = 'Source';
  SPanelLabelDestination = 'Destination';
  SPanelLabelReference1 = 'Reference 1';
  SPanelLabelReference2 = 'Reference 2';
  SPanelLabelComments = 'Editorial comments';
  STreeLegendTitle = 'Tree legend';
  STreeLegendStatusFmt = '%d: %s';
  STreeLegendCounts = 'Hover tree items for status counts';
  STreeLegendNotAssociated = 'Not associated';
  STreeLegendAssociating = 'Associating';
  STreeLegendNeedsReview = 'Needs review';
  STreeLegendAssociated = 'Associated';

implementation

uses FormNewProject, FormProjectProperties, FormProjectExport, FormMergeProjects,
  FormExportPatch, FormApplyPatch, FormLlmOptions, FormTheWordHighlighterColors,
  llmclient, theword_syntax_highlighter, translation_memory_preview;

{$R *.lfm}

{$IFDEF WINDOWS}
{------------------------------------------------------------------------------}
function SameExecutablePath(const LeftPath, RightPath: string): Boolean;
begin
  Result := (LeftPath <> '') and (RightPath <> '') and
    SameText(ExpandFileName(LeftPath), ExpandFileName(RightPath));
end;
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
  SNewTranslationProject = 'New translation project';
  SLoadingFile = 'Loading %s...';
  SNewProjectProgressTitle = 'Creating project';
  SCreatingProjectProgress = 'Creating project...';
  SLoadingSourceTextProgress = 'Loading source text...';
  SLoadingDestinationTextProgress = 'Loading destination text...';
  SLoadingReference1TextProgress = 'Loading reference 1 text...';
  SLoadingReference2TextProgress = 'Loading reference 2 text...';
  SNewProjectNotCreatedTitle = 'Project not created';
  SNewProjectOpenFailedTitle = 'Project created but not opened';
  SNewProjectImportCancelled = 'Project creation was cancelled while importing %s.';
  SNewProjectImportFailed = 'Could not import %s from:'#13#10'%s';
  SNewProjectCloseFailed = 'Could not safely close the newly created project before publishing it.';
  SNewProjectOpenFailed = 'The project was created but could not be opened:'#13#10'%s';
  SNewProjectCleanupFailed = 'Cleanup also failed:'#13#10'%s';
  SNewProjectCleanupCloseFailed = 'Could not close the incomplete project.';
  SNewProjectCleanupDeleteFailed = 'Could not remove the incomplete project file:'#13#10'%s';
  SNewProjectUnknownFailure = 'An unexpected error prevented the project from being created.';
  SRebuildAssociationMemoryTitle = 'Association memory';
  SRebuildAssociationMemoryProgress = 'Recreating association memory...';
  SChanged = 'changed';
  SAnalyzingVerse = 'Analyzing verse...';
  SLLMUsageStatus = 'Tokens: prompt %d, completion %d, total %d';
  SLLMUsageUnavailable = 'Tokens: unavailable';
  SInvalidProjectNameQuery = 'Do you want to choose a different name?';
  SInvalidProjectNameTitle = 'A project file with this name already exists';

function FormatLLMUsageStatus(const Usage: TLLMUsage): string;
begin
  if HasUsage(Usage) then
    Result := Format(SLLMUsageStatus, [Usage.PromptTokens, Usage.CompletionTokens, Usage.TotalTokens])
  else
    Result := SLLMUsageUnavailable;
end;

{ TMainForm }

procedure TMainForm.NormalizeActionHint(AAction: TCustomAction);
var
  BaseHint, ShortcutText, ShortcutSuffix: string;
begin
  if not Assigned(AAction) then
    Exit;

  ShortcutText := ShortCutToText(AAction.ShortCut);
  BaseHint := AAction.Hint;
  if BaseHint = '' then
    BaseHint := AAction.Caption;

  if ShortcutText <> '' then
  begin
    ShortcutSuffix := ' (' + ShortcutText + ')';
    while (Length(BaseHint) >= Length(ShortcutSuffix)) and
       (Copy(BaseHint, Length(BaseHint) - Length(ShortcutSuffix) + 1, Length(ShortcutSuffix)) = ShortcutSuffix) do
    begin
      Delete(BaseHint, Length(BaseHint) - Length(ShortcutSuffix) + 1, Length(ShortcutSuffix));
      BaseHint := TrimRight(BaseHint);
    end;

  end;

  AAction.Hint := BaseHint;
end;

procedure TMainForm.NormalizeActionHints;
begin
  NormalizeActionHint(ActionNewProject);
  NormalizeActionHint(ActionOpenProject);
  NormalizeActionHint(ActionCloseProject);
  NormalizeActionHint(ActionProjectProperties);
  NormalizeActionHint(ActionSaveProject);
  NormalizeActionHint(ActionExportProject);
  NormalizeActionHint(ActionHistoryBack);
  NormalizeActionHint(ActionHistoryForward);
  NormalizeActionHint(ActionPreviousVerse);
  NormalizeActionHint(ActionNextVerse);
  NormalizeActionHint(ActionSuggestAssociation);
  NormalizeActionHint(ActionRollbackCurrentVerse);
  NormalizeActionHint(ActionClearAssociations);

  ActionShowTags.Hint := SShortcutToggleTags;
  NormalizeActionHint(ActionShowTags);

  if Assigned(ActionKeyboardShortcuts) then
    NormalizeActionHint(ActionKeyboardShortcuts);

  RadioGroupStatus.Hint := SStatusKeyboardHint;
  RadioGroupStatus.ShowHint := True;
end;

procedure TMainForm.ActionGuideModeExecute(Sender: TObject);
begin
  SetGuideModeEnabled(not FGuideModeEnabled);
end;

function TMainForm.BuildKeyboardShortcutsText: string;
var
  Lines: TStringList;

  procedure AddSection(const ACaption: string);
  begin
    if Lines.Count > 0 then
      Lines.Add('');
    Lines.Add(ACaption);
  end;

  procedure AddShortcut(const AShortcut, ADescription: string);
  begin
    if AShortcut <> '' then
      Lines.Add(Format('  %s - %s', [AShortcut, ADescription]));
  end;

  procedure AddActionShortcut(AAction: TCustomAction; const ADescription: string);
  begin
    if Assigned(AAction) then
      AddShortcut(ShortCutToText(AAction.ShortCut), ADescription);
  end;

  procedure AddStatusShortcuts;
  var
    Definitions, Sorted: TStatusDefinitions;
    Index: Integer;
  begin
    if Assigned(CurrentProject) then
      Definitions := CurrentProject.GetStatusDefinitions
    else
      Definitions := BuildLegacyStatusDefinitions([
        STreeLegendNotAssociated,
        STreeLegendAssociating,
        STreeLegendNeedsReview,
        STreeLegendAssociated
      ]);

    Sorted := SortStatusDefinitionsByDisplayOrder(Definitions);
    for Index := 0 to Min(High(Sorted), 8) do
      AddShortcut(IntToStr(Index + 1), Format(SShortcutStatusFmt,
        [Sorted[Index].Caption]));
  end;

begin
  Lines := TStringList.Create;
  try
    AddSection(SShortcutProjectSection);
    AddActionShortcut(ActionNewProject, SShortcutNewProject);
    AddActionShortcut(ActionOpenProject, SShortcutOpenProject);
    AddActionShortcut(ActionSaveProject, SShortcutSaveProject);
    AddActionShortcut(ActionCloseProject, SShortcutCloseProject);
    AddActionShortcut(ActionProjectProperties, SShortcutProjectSettings);
    AddActionShortcut(ActionExportProject, SShortcutExportProject);

    AddSection(SShortcutNavigationSection);
    AddActionShortcut(ActionHistoryBack, SShortcutHistoryBack);
    AddActionShortcut(ActionHistoryForward, SShortcutHistoryForward);
    AddActionShortcut(ActionPreviousVerse, SShortcutPreviousVerse);
    AddActionShortcut(ActionNextVerse, SShortcutNextVerse);

    AddSection(SShortcutEditingSection);
    AddActionShortcut(ActionClearAssociations, SShortcutClearAssociations);
    AddActionShortcut(ActionSuggestAssociation, SShortcutAutoAssociate);
    AddActionShortcut(ActionRollbackCurrentVerse, SShortcutRollback);
    AddActionShortcut(ActionShowTags, SShortcutToggleTags);
    AddStatusShortcuts;
    AddShortcut('Esc', SShortcutFinishEdit);

    AddSection(SShortcutHelpSection);
    AddActionShortcut(ActionKeyboardShortcuts, SShortcutShowKeyboardShortcuts);

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TMainForm.CreateGuidePanel;
begin
  if Assigned(FGuidePanel) then
    Exit;

  FGuidePanel := TPanel.Create(Self);
  with FGuidePanel do
  begin
    Parent := Self;
    Align := alTop;
    Top := ToolBar1.Height + 34;
    Height := 88;
    BevelOuter := bvNone;
    ParentColor := False;
    Color := clInfoBk;
    ShowHint := True;
    Hint := SGuideModeHint;
    Visible := False;
    OnResize := @GuidePanelResize;
  end;

  FGuideTitleLabel := TLabel.Create(FGuidePanel);
  with FGuideTitleLabel do
  begin
    Parent := FGuidePanel;
    AutoSize := False;
    Transparent := True;
    Layout := tlCenter;
    Font.Style := Font.Style + [fsBold];
  end;

  FGuideTextLabel := TLabel.Create(FGuidePanel);
  with FGuideTextLabel do
  begin
    Parent := FGuidePanel;
    AutoSize := False;
    Transparent := True;
    Layout := tlTop;
    WordWrap := True;
  end;

  FGuideHideButton := TButton.Create(FGuidePanel);
  with FGuideHideButton do
  begin
    Parent := FGuidePanel;
    Caption := SHideGuideButton;
    OnClick := @GuideHideButtonClick;
  end;

  LayoutGuidePanel;
  UpdateGuidePanel;
end;

procedure TMainForm.CreateStatusReviewPanel;
begin
  if not Assigned(TreeLegendPanel) or Assigned(FStatusReviewList) then
    Exit;

  TreeLegendPanel.Height := Max(TreeLegendPanel.Height, 170);
  TreeLegendPanel.Constraints.MinHeight := 130;

  if Assigned(TreeLegendLabel) then
  begin
    TreeLegendLabel.Align := alTop;
    TreeLegendLabel.AutoSize := False;
    TreeLegendLabel.Height := 38;
    TreeLegendLabel.WordWrap := True;
    TreeLegendLabel.Font.Style := TreeLegendLabel.Font.Style + [fsBold];
    TreeLegendLabel.Caption := SStatusReviewTitle;
    TreeLegendLabel.Hint := SStatusReviewFilterHint;
    TreeLegendLabel.ShowHint := True;
  end;

  FStatusReviewResetButton := TButton.Create(TreeLegendPanel);
  with FStatusReviewResetButton do
  begin
    Parent := TreeLegendPanel;
    Align := alBottom;
    Height := 25;
    Caption := SStatusReviewShowAll;
    Hint := SStatusReviewShowAllHint;
    ShowHint := True;
    OnClick := @StatusReviewResetButtonClick;
  end;

  FStatusReviewList := TListBox.Create(TreeLegendPanel);
  with FStatusReviewList do
  begin
    Parent := TreeLegendPanel;
    Align := alClient;
    BorderStyle := bsNone;
    IntegralHeight := False;
    Style := lbOwnerDrawFixed;
    ItemHeight := Max(ItemHeight, 20);
    ShowHint := True;
    Hint := SStatusReviewFilterHint;
    OnClick := @StatusReviewListClickCheck;
    OnDrawItem := @StatusReviewListDrawItem;
    OnKeyDown := @StatusReviewListKeyDown;
  end;

  UpdateStatusReviewPanel;
end;

function TMainForm.CreateVersePanelLabel(AParent: TWinControl;
  TextType: TTipoTextoBiblico): TPanel;

  procedure SetupChildLabel(ALabel: TLabel; Bold: Boolean);
  begin
    ALabel.Parent := Result;
    ALabel.AutoSize := False;
    ALabel.Transparent := True;
    ALabel.Layout := tlCenter;
    ALabel.ShowHint := True;
    ALabel.Font.Size := Max(6, Font.Size - 1);
    if Bold then
      ALabel.Font.Style := ALabel.Font.Style + [fsBold]
    else
      ALabel.Font.Style := ALabel.Font.Style - [fsBold];
  end;

begin
  Result := TPanel.Create(AParent);
  with Result do
  begin
    Parent := AParent;
    Caption := '';
    BevelOuter := bvNone;
    ParentColor := False;
    Color := clBtnFace;
    ShowHint := True;
  end;

  FVersePanelTypeLabels[TextType] := TLabel.Create(Result);
  SetupChildLabel(FVersePanelTypeLabels[TextType], True);

  FVersePanelDescriptionLabels[TextType] := TLabel.Create(Result);
  SetupChildLabel(FVersePanelDescriptionLabels[TextType], False);
end;

function TMainForm.CreateStateHeaderLabel(AParent: TWinControl;
  Bold: Boolean): TLabel;
begin
  Result := TLabel.Create(AParent);
  with Result do
  begin
    Parent := AParent;
    AutoSize := False;
    Transparent := True;
    Layout := tlCenter;
    ShowHint := True;
    Font.Color := clWindowText;
    if Bold then
      Font.Style := Font.Style + [fsBold];
  end;
end;

procedure TMainForm.AttachStatusComboToHeader;
begin
  if Assigned(StatusPanel) then
  begin
    StatusPanel.Visible := False;
    StatusPanel.Width := 0;
  end;

  if not Assigned(RadioGroupStatus) or not Assigned(FStateHeaderPanel) then
    Exit;

  RadioGroupStatus.Parent := FStateHeaderPanel;
  RadioGroupStatus.Anchors := [akLeft, akTop];
  RadioGroupStatus.Visible := False;
  RadioGroupStatus.ShowHint := True;
  RadioGroupStatus.BringToFront;
end;

procedure TMainForm.CreateCommentsGroupBox;
begin
  if Assigned(FCommentsGroupBox) then
    Exit;

  BottomPanel.Caption := '';
  FCommentsGroupBox := TGroupBox.Create(Self);
  with FCommentsGroupBox do
  begin
    Parent := BottomPanel;
    Align := alClient;
    Caption := SPanelLabelComments;
    ShowHint := True;
    TabOrder := CommentsMemo.TabOrder;
  end;

  CommentsMemo.Parent := FCommentsGroupBox;
  CommentsMemo.Align := alClient;
  CommentsMemo.BorderSpacing.Around := 4;
end;

procedure TMainForm.CreateStateHeader;
begin
  if Assigned(FStateHeaderPanel) then
    Exit;

  ToolBar1.Align := alTop;
  ToolBar1.Top := 0;
  Panel1.Align := alClient;

  FStateHeaderPanel := TPanel.Create(Self);
  with FStateHeaderPanel do
  begin
    Parent := Self;
    Align := alTop;
    Top := ToolBar1.Height;
    Height := 34;
    BevelOuter := bvNone;
    ParentColor := False;
    Color := clBtnFace;
    OnResize := @StateHeaderResize;
  end;

  FHeaderReferenceLabel := CreateStateHeaderLabel(FStateHeaderPanel, True);
  FHeaderStatusLabel := CreateStateHeaderLabel(FStateHeaderPanel);
  FHeaderStatusLabel.Visible := False;
  FHeaderAssociationsLabel := CreateStateHeaderLabel(FStateHeaderPanel);
  FHeaderSelectionLabel := CreateStateHeaderLabel(FStateHeaderPanel);
  FHeaderEngineLabel := CreateStateHeaderLabel(FStateHeaderPanel);
  FHeaderModifiedLabel := CreateStateHeaderLabel(FStateHeaderPanel, True);
  AttachStatusComboToHeader;

  UpdateStateHeader;
end;

procedure TMainForm.CreateVersePanelLabels;
var
  TextType: TTipoTextoBiblico;
begin
  for TextType := tbOrigem to tbConsulta2 do
    if not Assigned(FVersePanelLabels[TextType]) then
      FVersePanelLabels[TextType] := CreateVersePanelLabel(CenterPanel,
        TextType);

  CenterPanel.OnResize := @SplitterMoved;
  Splitter2.OnMoved := @SplitterMoved;
  Splitter3.OnMoved := @SplitterMoved;
  Splitter4.OnMoved := @SplitterMoved;
  Splitter5.OnMoved := @SplitterMoved;

  LayoutVersePanelLabels;
end;

procedure TMainForm.CountCurrentAssociations(out Associated, Total: Integer);
var
  SourceVerse: TVerse;
  Syntagm: TSyntagm;
begin
  Associated := 0;
  Total := 0;

  if not Assigned(CurrentProject) then
    Exit;

  SourceVerse := CurrentProject.VerseObjects[tbOrigem];
  if not Assigned(SourceVerse) or not Assigned(SourceVerse.Syntagms) then
    Exit;

  for Syntagm in SourceVerse.Syntagms do
    if Syntagm.Kind = skSyntagm then
    begin
      Inc(Total);
      if Syntagm.IsAssociated then
        Inc(Associated);
    end;
end;

function TMainForm.CurrentEngineCaption: string;
begin
  case FSuggestionMode of
    smAI: Result := SHeaderEngineAI;
  else
    Result := SHeaderEngineStatistical;
  end;
end;

function TMainForm.CurrentSelectionCaption: string;
var
  SourceCount, DestinationCount: Integer;
begin
  SourceCount := 0;
  DestinationCount := 0;

  if Assigned(CurrentProject) then
  begin
    if Assigned(CurrentProject.VerseObjects[tbOrigem]) and
       Assigned(CurrentProject.VerseObjects[tbOrigem].Selection) then
      SourceCount := CurrentProject.VerseObjects[tbOrigem].Selection.Count;

    if Assigned(CurrentProject.VerseObjects[tbDestino]) and
       Assigned(CurrentProject.VerseObjects[tbDestino].Selection) then
      DestinationCount := CurrentProject.VerseObjects[tbDestino].Selection.Count;
  end;

  if (SourceCount = 0) and (DestinationCount = 0) then
    Result := SHeaderSelectionNone
  else
    Result := Format(SHeaderSelectionFmt, [SourceCount, DestinationCount]);
end;

function TMainForm.CurrentGuideScenario: TProjectScenario;
begin
  if Assigned(CurrentProject) then
    Result := ProjectScenarioFromConfig(
      CurrentProject.GetInfo(PROJECT_SCENARIO_CONFIG_KEY))
  else
    Result := psUnknown;
end;

function TMainForm.GetVersePanelCaption(TextType: TTipoTextoBiblico): string;
var
  TextDescription: string;
begin
  Result := GetVersePanelTypeCaption(TextType);
  TextDescription := GetVersePanelDescription(TextType);
  if TextDescription <> '' then
    Result := Format('%s: %s', [Result, TextDescription]);
end;

function TMainForm.GetVersePanelDescription(
  TextType: TTipoTextoBiblico): string;
begin
  Result := '';
  if Assigned(CurrentProject) then
    Result := Trim(CurrentProject.GetTextDescription(TextType));
end;

function TMainForm.LocalizedProjectDescription(
  const Description: string): string;
begin
  if SameText(Description, 'New association project') then
    Result := SNewAssociationProject
  else if SameText(Description, 'New translation project') then
    Result := SNewTranslationProject
  else
    Result := Description;
end;

function TMainForm.GetVersePanelTypeCaption(
  TextType: TTipoTextoBiblico): string;
begin
  case TextType of
    tbOrigem: Result := SPanelLabelSource;
    tbDestino: Result := SPanelLabelDestination;
    tbConsulta1: Result := SPanelLabelReference1;
    tbConsulta2: Result := SPanelLabelReference2;
  else
    Result := '';
  end;
end;

procedure TMainForm.GuideHideButtonClick(Sender: TObject);
begin
  SetGuideModeEnabled(False);
end;

procedure TMainForm.GuidePanelResize(Sender: TObject);
begin
  LayoutGuidePanel;
end;

procedure TMainForm.LayoutGuidePanel;
const
  Margin = 8;
  Gap = 10;
var
  Compact: Boolean;
  ButtonWidth, ButtonHeight, ButtonTop, TextWidth, TitleHeight,
    TextTop, TextHeight, DesiredHeight, LineHeight, WrappedLines: Integer;

  function CountWrappedLines(const AText: string; AMaxWidth: Integer): Integer;
  var
    Lines: TStringList;
    I, LineWidth: Integer;
  begin
    Result := 0;
    Lines := TStringList.Create;
    try
      Lines.Text := AText;
      for I := 0 to Lines.Count - 1 do
      begin
        LineWidth := FGuidePanel.Canvas.TextWidth(Lines[I]);
        Inc(Result, Max(1, Ceil(LineWidth / Max(1, AMaxWidth))));
      end;
    finally
      Lines.Free;
    end;
    Result := Max(1, Result);
  end;
begin
  if not Assigned(FGuidePanel) or (FGuidePanel.ClientWidth <= 0) then
    Exit;

  Compact := FGuidePanel.ClientWidth < 520;

  FGuidePanel.Canvas.Font.Assign(Font);
  ButtonWidth := Max(100,
    FGuidePanel.Canvas.TextWidth(FGuideHideButton.Caption) + 28);
  ButtonHeight := Max(28, FGuidePanel.Canvas.TextHeight('Hg') + 12);
  TitleHeight := FGuidePanel.Canvas.TextHeight('Hg') + 4;
  LineHeight := FGuidePanel.Canvas.TextHeight('Hg') + 2;

  if Compact then
    TextWidth := Max(80, FGuidePanel.ClientWidth - (Margin * 2))
  else
    TextWidth := Max(80,
      FGuidePanel.ClientWidth - ButtonWidth - (Margin * 2) - Gap);

  WrappedLines := CountWrappedLines(FGuideTextLabel.Caption, TextWidth);
  DesiredHeight := Margin + TitleHeight + 2 + (WrappedLines * LineHeight) +
    Margin;
  if Compact then
    DesiredHeight := DesiredHeight + ButtonHeight + Gap
  else
    DesiredHeight := Max(DesiredHeight, ButtonHeight + (Margin * 2));
  DesiredHeight := Max(72, DesiredHeight);

  if FGuidePanel.Height <> DesiredHeight then
    FGuidePanel.Height := DesiredHeight;

  if Compact then
  begin
    ButtonTop := FGuidePanel.ClientHeight - ButtonHeight - Margin;
  end
  else
  begin
    ButtonTop := Max(Margin,
      (FGuidePanel.ClientHeight - ButtonHeight) div 2);
  end;

  FGuideHideButton.SetBounds(
    Max(Margin, FGuidePanel.ClientWidth - ButtonWidth - Margin),
    ButtonTop,
    ButtonWidth,
    ButtonHeight);

  TextTop := Margin + TitleHeight + 2;
  if Compact then
    TextHeight := Max(24, ButtonTop - TextTop - 4)
  else
    TextHeight := Max(24, FGuidePanel.ClientHeight - TextTop - Margin);

  FGuideTitleLabel.SetBounds(Margin, Margin, TextWidth, TitleHeight);
  FGuideTextLabel.SetBounds(Margin, TextTop, TextWidth, TextHeight);
end;

procedure TMainForm.SetGuideModeEnabled(AEnabled: Boolean);
begin
  FGuideModeEnabled := AEnabled;

  if Assigned(ActionGuideMode) then
    ActionGuideMode.Checked := FGuideModeEnabled;
  if Assigned(MenuItemGuideMode) then
    MenuItemGuideMode.Checked := FGuideModeEnabled;

  if Assigned(opts) then
    opts.WriteBool('opcoes', 'guide.mode', FGuideModeEnabled);

  UpdateGuidePanel;
end;

procedure TMainForm.LayoutStateHeader;
const
  Margin = 8;
  Gap = 14;
  MinReferenceWidth = 120;
  StatusComboWidth = 178;
  MinStatusComboWidth = 112;
var
  X, Y, LineHeight, ModifiedWidth, MaxRight: Integer;

  function CaptionWidth(ALabel: TLabel): Integer;
  begin
    FStateHeaderPanel.Canvas.Font.Assign(ALabel.Font);
    Result := FStateHeaderPanel.Canvas.TextWidth(ALabel.Caption) + 16;
  end;

  procedure PlaceLabel(ALabel: TLabel; var ALeft: Integer; AMaxRight: Integer;
    AAlwaysShow: Boolean; AMaxWidth: Integer = 0);
  var
    W: Integer;
  begin
    W := CaptionWidth(ALabel);
    if AMaxWidth > 0 then
      W := Min(W, AMaxWidth);

    ALabel.Visible := AAlwaysShow or (ALeft + W <= AMaxRight);
    if not ALabel.Visible then
      Exit;

    if AAlwaysShow then
      W := Max(MinReferenceWidth, Min(W, Max(0, AMaxRight - ALeft)));

    ALabel.SetBounds(ALeft, Y, W, LineHeight);
    Inc(ALeft, W + Gap);
  end;

  procedure PlaceStatusCombo(var ALeft: Integer; AMaxRight: Integer);
  var
    Available, W, ComboTop: Integer;
  begin
    if not Assigned(RadioGroupStatus) then
      Exit;

    Available := Max(0, AMaxRight - ALeft);
    W := Min(StatusComboWidth, Available);
    RadioGroupStatus.Visible := Assigned(CurrentProject) and
      (RadioGroupStatus.Items.Count > 0) and (W >= MinStatusComboWidth);
    if not RadioGroupStatus.Visible then
      Exit;

    ComboTop := Max(0,
      (FStateHeaderPanel.ClientHeight - RadioGroupStatus.Height) div 2);
    RadioGroupStatus.SetBounds(ALeft, ComboTop, W, RadioGroupStatus.Height);
    Inc(ALeft, W + Gap);
  end;

begin
  if not Assigned(FStateHeaderPanel) then
    Exit;

  FStateHeaderPanel.Canvas.Font.Assign(Font);
  LineHeight := FStateHeaderPanel.Canvas.TextHeight('Hg') + 8;
  Y := Max(0, (FStateHeaderPanel.ClientHeight - LineHeight) div 2);

  ModifiedWidth := CaptionWidth(FHeaderModifiedLabel);
  FHeaderModifiedLabel.SetBounds(
    Max(Margin, FStateHeaderPanel.ClientWidth - ModifiedWidth - Margin),
    Y, ModifiedWidth, LineHeight);

  MaxRight := FHeaderModifiedLabel.Left - Gap;
  X := Margin;

  PlaceLabel(FHeaderReferenceLabel, X, MaxRight, True, 260);
  PlaceStatusCombo(X, MaxRight);
  PlaceLabel(FHeaderAssociationsLabel, X, MaxRight, False, 190);
  PlaceLabel(FHeaderSelectionLabel, X, MaxRight, False, 210);
  PlaceLabel(FHeaderEngineLabel, X, MaxRight, False, 180);
end;

procedure TMainForm.StateHeaderResize(Sender: TObject);
begin
  LayoutStateHeader;
end;

procedure TMainForm.UpdateStateHeader;
var
  Associated, Total, Percent: Integer;
begin
  if not Assigned(FStateHeaderPanel) then
    Exit;

  if not Assigned(CurrentProject) then
  begin
    FHeaderReferenceLabel.Caption := SHeaderNoProject;
    FHeaderStatusLabel.Caption := '';
    FHeaderAssociationsLabel.Caption := SHeaderAssociationsEmpty;
    FHeaderSelectionLabel.Caption := SHeaderSelectionNone;
    FHeaderEngineLabel.Caption := Format(SHeaderEngineFmt, [CurrentEngineCaption]);
    FHeaderModifiedLabel.Caption := SHeaderSaved;
    FHeaderModifiedLabel.Font.Color := clGrayText;
  end
  else
  begin
    CountCurrentAssociations(Associated, Total);
    if Total > 0 then
    begin
      Percent := Round((Associated / Total) * 100);
      FHeaderAssociationsLabel.Caption := Format(SHeaderAssociationsFmt,
        [Associated, Total, Percent]);
    end
    else
      FHeaderAssociationsLabel.Caption := SHeaderAssociationsEmpty;

    FHeaderReferenceLabel.Caption := Format(SHeaderReferenceFmt,
      [CurrentProject.FormattedReference]);
    FHeaderStatusLabel.Caption := '';
    FHeaderSelectionLabel.Caption := CurrentSelectionCaption;
    FHeaderEngineLabel.Caption := Format(SHeaderEngineFmt, [CurrentEngineCaption]);

    if CurrentProject.Modificado then
    begin
      FHeaderModifiedLabel.Caption := SHeaderUnsavedChanges;
      FHeaderModifiedLabel.Font.Color := clRed;
    end
    else
    begin
      FHeaderModifiedLabel.Caption := SHeaderSaved;
      FHeaderModifiedLabel.Font.Color := clGreen;
    end;
  end;

  FHeaderReferenceLabel.Hint := FHeaderReferenceLabel.Caption;
  FHeaderStatusLabel.Hint := '';
  if Assigned(RadioGroupStatus) then
  begin
    if Assigned(CurrentProject) then
      RadioGroupStatus.Hint := Format('%s. %s', [
        Format(SHeaderStatusFmt,
          [CurrentProject.GetStatusDescription(CurrentProject.Status)]),
        SStatusKeyboardHint
      ])
    else
      RadioGroupStatus.Hint := SHeaderStatusEmpty;
  end;
  FHeaderAssociationsLabel.Hint := FHeaderAssociationsLabel.Caption;
  FHeaderSelectionLabel.Hint := FHeaderSelectionLabel.Caption;
  FHeaderEngineLabel.Hint := FHeaderEngineLabel.Caption;
  FHeaderModifiedLabel.Hint := FHeaderModifiedLabel.Caption;
  LayoutStateHeader;
  UpdateGuidePanel;
end;

procedure TMainForm.UpdateMainCaption;
begin
  if Assigned(CurrentProject) then
    Caption := Format(SMainCaptionProjectFmt,
      [CurrentProject.FormattedReference,
       LocalizedProjectDescription(CurrentProject.GetInfo('descricao')),
       CurrentProject.FileName])
  else
    Caption := SMainCaption;
end;

procedure TMainForm.UpdateGuidePanel;
var
  SourceCount, DestinationCount: Integer;
  Scenario: TProjectScenario;
  DestinationHasText, SelectionHasAssociation, ShowTranslationGuide: Boolean;

  function JoinedGuideText(const FirstLine, SecondLine, ThirdLine: string): string;
  begin
    Result := FirstLine + LineEnding + SecondLine + LineEnding + ThirdLine;
  end;

  function SelectedAssociationIn(TextType: TTipoTextoBiblico): Boolean;
  var
    Syntagm: TSyntagm;
  begin
    Result := False;
    if not Assigned(CurrentProject.VerseObjects[TextType]) or
       not Assigned(CurrentProject.VerseObjects[TextType].Selection) then
      Exit;

    for Syntagm in CurrentProject.VerseObjects[TextType].Selection do
      if Syntagm.IsAssociated then
        Exit(True);
  end;
begin
  if not Assigned(FGuidePanel) then
    Exit;

  FGuidePanel.Visible := FGuideModeEnabled;
  if not FGuideModeEnabled then
    Exit;

  if not Assigned(CurrentProject) then
  begin
    FGuideTitleLabel.Caption := SGuideNoProjectTitle;
    FGuideTextLabel.Caption := SGuideNoProjectText;
  end
  else
  begin
    Scenario := CurrentGuideScenario;
    DestinationHasText := Assigned(CurrentProject.VerseObjects[tbDestino]) and
      (Trim(CurrentProject.VerseObjects[tbDestino].PlainText) <> '');
    ShowTranslationGuide := (Scenario = psTranslationProject) and
      not DestinationHasText;

    if ShowTranslationGuide then
    begin
      FGuideTitleLabel.Caption := SGuideTranslationTitle;
      FGuideTextLabel.Caption := SGuideTranslationText;
    end
    else
    begin
      SourceCount := 0;
      DestinationCount := 0;
      if Assigned(CurrentProject.VerseObjects[tbOrigem]) and
         Assigned(CurrentProject.VerseObjects[tbOrigem].Selection) then
        SourceCount := CurrentProject.VerseObjects[tbOrigem].Selection.Count;
      if Assigned(CurrentProject.VerseObjects[tbDestino]) and
         Assigned(CurrentProject.VerseObjects[tbDestino].Selection) then
        DestinationCount := CurrentProject.VerseObjects[tbDestino].Selection.Count;

      SelectionHasAssociation := SelectedAssociationIn(tbOrigem) or
        SelectedAssociationIn(tbDestino);

      if SelectionHasAssociation then
      begin
        FGuideTitleLabel.Caption := SGuideAssociationExistingTitle;
        FGuideTextLabel.Caption := JoinedGuideText(
          SGuideAssociationExistingDisassociateText,
          SGuideAssociationExistingAddText,
          SGuideAssociationExistingRemoveText);
      end
      else if (SourceCount > 0) or (DestinationCount > 0) then
      begin
        FGuideTitleLabel.Caption := SGuideAssociationSelectedTitle;
        FGuideTextLabel.Caption := JoinedGuideText(
          SGuideAssociationSelectedAddText,
          SGuideAssociationSelectedAssociateText,
          SGuideAssociationSelectedQuickText);
      end
      else
      begin
        FGuideTitleLabel.Caption := SGuideAssociationTitle;
        FGuideTextLabel.Caption := SGuideAssociationNoSelectionText;
      end;
    end;
  end;

  FGuidePanel.Hint := FGuideTextLabel.Caption;
  FGuideTitleLabel.Hint := FGuideTitleLabel.Caption;
  FGuideTextLabel.Hint := FGuideTextLabel.Caption;
  LayoutGuidePanel;
end;

procedure TMainForm.UpdateStatusReviewPanel;
type
  TStatusFilterState = record
    StatusId: Integer;
    Checked: Boolean;
  end;
var
  Definitions: TStatusDefinitions;
  Sorted: TStatusDefinitions;
  Counts: TStatusCounts;
  OldStates: array of TStatusFilterState;
  Index: Integer;
  CountIndex: Integer;
  ItemIndex: Integer;
  Total: Integer;
  VisibleTotal: Integer;
  Node: TTreeNode;

  procedure RememberCurrentFilter;
  var
    StatusIndex: Integer;
  begin
    OldStates := nil;
    if not Assigned(FStatusReviewList) then
      Exit;

    SetLength(OldStates, FStatusReviewList.Items.Count);
    for StatusIndex := 0 to FStatusReviewList.Items.Count - 1 do
    begin
      OldStates[StatusIndex].StatusId :=
        Integer(PtrUInt(FStatusReviewList.Items.Objects[StatusIndex]));
      OldStates[StatusIndex].Checked := StatusReviewItemChecked(StatusIndex);
    end;
  end;

  function WasChecked(StatusId: Integer): Boolean;
  var
    StatusIndex: Integer;
  begin
    Result := True;
    for StatusIndex := 0 to High(OldStates) do
      if OldStates[StatusIndex].StatusId = StatusId then
        Exit(OldStates[StatusIndex].Checked);
  end;

  procedure InitializeCounts;
  var
    StatusIndex: Integer;
  begin
    SetLength(Counts, Length(Sorted));
    for StatusIndex := 0 to High(Sorted) do
    begin
      Counts[StatusIndex].StatusId := Sorted[StatusIndex].Id;
      Counts[StatusIndex].Count := 0;
    end;
  end;

  procedure IncrementStatusCount(StatusId: Integer);
  var
    StatusIndex: Integer;
  begin
    for StatusIndex := 0 to High(Counts) do
      if Counts[StatusIndex].StatusId = StatusId then
      begin
        Inc(Counts[StatusIndex].Count);
        Exit;
      end;

    SetLength(Counts, Length(Counts) + 1);
    Counts[High(Counts)].StatusId := StatusId;
    Counts[High(Counts)].Count := 1;
  end;

  procedure CountNode(Node: TTreeNode);
  var
    ChildNode: TTreeNode;
  begin
    if not Assigned(Node) then
      Exit;

    if not Node.HasChildren then
    begin
      IncrementStatusCount(Node.ImageIndex);
      Inc(Total);
      Exit;
    end;

    ChildNode := Node.GetFirstChild;
    while Assigned(ChildNode) do
    begin
      CountNode(ChildNode);
      ChildNode := ChildNode.GetNextSibling;
    end;
  end;

  function CountValue(StatusId: Integer): Integer;
  var
    StatusIndex: Integer;
  begin
    Result := 0;
    for StatusIndex := 0 to High(Counts) do
      if Counts[StatusIndex].StatusId = StatusId then
        Exit(Counts[StatusIndex].Count);
  end;

  procedure AddStatusItem(StatusId: Integer; const Caption: string;
    Count: Integer);
  begin
    ItemIndex := FStatusReviewList.Items.AddObject(
      Format(SStatusReviewItemFmt, [Count, Caption]), TObject(PtrUInt(StatusId)));
    SetLength(FStatusReviewChecked, FStatusReviewList.Items.Count);
    FStatusReviewChecked[ItemIndex] := WasChecked(StatusId);
    if FStatusReviewChecked[ItemIndex] then
      Inc(VisibleTotal, Count);
  end;

begin
  if not Assigned(TreeLegendLabel) or not Assigned(FStatusReviewList) then
    Exit;

  RememberCurrentFilter;
  FUpdatingStatusReview := True;
  try
    FStatusReviewList.Items.Clear;
    FStatusReviewChecked := nil;
    FStatusReviewList.Enabled := Assigned(CurrentProject);
    if Assigned(FStatusReviewResetButton) then
    begin
      FStatusReviewResetButton.Caption := SStatusReviewShowAll;
      FStatusReviewResetButton.Hint := SStatusReviewShowAllHint;
      FStatusReviewResetButton.Enabled := Assigned(CurrentProject);
    end;

    if not Assigned(CurrentProject) then
    begin
      TreeLegendLabel.Caption := SStatusReviewTitle + LineEnding +
        SStatusReviewNoProject;
      TreeLegendPanel.Hint := SStatusReviewNoProject;
      Exit;
    end;

    Definitions := CurrentProject.GetStatusDefinitions;
    Sorted := SortStatusDefinitionsByDisplayOrder(Definitions);
    Counts := nil;
    Total := 0;
    VisibleTotal := 0;
    InitializeCounts;

    if Assigned(TreeView1) and (TreeView1.Items.Count > 0) then
    begin
      Node := TreeView1.Items[0];
      while Assigned(Node) do
      begin
        CountNode(Node);
        Node := Node.GetNextSibling;
      end;
    end;

    for Index := 0 to High(Sorted) do
      AddStatusItem(Sorted[Index].Id, Sorted[Index].Caption,
        CountValue(Sorted[Index].Id));

    for CountIndex := 0 to High(Counts) do
      if not IsValidStatusId(Definitions, Counts[CountIndex].StatusId) then
        AddStatusItem(Counts[CountIndex].StatusId,
          GetStatusCaption(Definitions, Counts[CountIndex].StatusId),
          Counts[CountIndex].Count);

    TreeLegendLabel.Caption := SStatusReviewTitle + LineEnding +
      Format(SStatusReviewShowingFmt, [VisibleTotal, Total]);
    TreeLegendPanel.Hint := SStatusReviewFilterHint;
    FStatusReviewList.Hint := SStatusReviewFilterHint;
  finally
    FUpdatingStatusReview := False;
  end;

  ApplyStatusReviewFilter;
end;

procedure TMainForm.ApplyStatusReviewFilter;
var
  SavedOnChange: TTVChangedEvent;
  Node: TTreeNode;
  FilterActive: Boolean;

  function StatusIsChecked(StatusId: Integer): Boolean;
  var
    StatusIndex: Integer;
  begin
    Result := True;
    if not Assigned(FStatusReviewList) or (FStatusReviewList.Items.Count = 0) then
      Exit;

    for StatusIndex := 0 to FStatusReviewList.Items.Count - 1 do
      if Integer(PtrUInt(FStatusReviewList.Items.Objects[StatusIndex])) = StatusId then
        Exit(StatusReviewItemChecked(StatusIndex));

    Result := False;
  end;

  function HasActiveFilter: Boolean;
  var
    StatusIndex: Integer;
  begin
    Result := False;
    if not Assigned(FStatusReviewList) then
      Exit;

    for StatusIndex := 0 to FStatusReviewList.Items.Count - 1 do
      if not StatusReviewItemChecked(StatusIndex) then
        Exit(True);
  end;

  function ApplyNodeFilter(ANode: TTreeNode): Boolean;
  var
    ChildNode: TTreeNode;
    ChildVisible: Boolean;
  begin
    if not Assigned(ANode) then
      Exit(False);

    if not FilterActive then
    begin
      Result := True;
      ChildNode := ANode.GetFirstChild;
      while Assigned(ChildNode) do
      begin
        ApplyNodeFilter(ChildNode);
        ChildNode := ChildNode.GetNextSibling;
      end;
    end
    else if not ANode.HasChildren then
      Result := StatusIsChecked(ANode.ImageIndex)
    else
    begin
      Result := False;
      ChildNode := ANode.GetFirstChild;
      while Assigned(ChildNode) do
      begin
        ChildVisible := ApplyNodeFilter(ChildNode);
        Result := Result or ChildVisible;
        ChildNode := ChildNode.GetNextSibling;
      end;
    end;

    ANode.Visible := Result;
  end;

begin
  if not Assigned(TreeView1) then
    Exit;

  FilterActive := HasActiveFilter;
  SavedOnChange := TreeView1.OnChange;
  TreeView1.OnChange := nil;
  TreeView1.BeginUpdate;
  try
    if TreeView1.Items.Count > 0 then
    begin
      Node := TreeView1.Items[0];
      while Assigned(Node) do
      begin
        ApplyNodeFilter(Node);
        Node := Node.GetNextSibling;
      end;
    end;
  finally
    TreeView1.EndUpdate;
    TreeView1.OnChange := SavedOnChange;
  end;
end;

function TMainForm.StatusReviewItemChecked(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index <= High(FStatusReviewChecked)) and
    FStatusReviewChecked[Index];
end;

procedure TMainForm.SetStatusReviewItemChecked(Index: Integer;
  AChecked: Boolean);
begin
  if (Index < 0) or (Index > High(FStatusReviewChecked)) then
    Exit;

  FStatusReviewChecked[Index] := AChecked;
  if Assigned(FStatusReviewList) then
    FStatusReviewList.Invalidate;
end;

procedure TMainForm.ToggleStatusReviewItem(Index: Integer);
begin
  if FUpdatingStatusReview or not Assigned(FStatusReviewList) or
    not FStatusReviewList.Enabled then
    Exit;

  if (Index < 0) or (Index >= FStatusReviewList.Items.Count) then
    Exit;

  SetStatusReviewItemChecked(Index, not StatusReviewItemChecked(Index));
  UpdateStatusReviewPanel;
end;

procedure TMainForm.StatusReviewListClickCheck(Sender: TObject);
begin
  if FUpdatingStatusReview then
    Exit;

  if Assigned(FStatusReviewList) then
    ToggleStatusReviewItem(FStatusReviewList.ItemIndex);
end;

procedure TMainForm.StatusReviewListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key <> VK_SPACE) or (Shift <> []) or not Assigned(FStatusReviewList) then
    Exit;

  ToggleStatusReviewItem(FStatusReviewList.ItemIndex);
  Key := 0;
end;

procedure TMainForm.StatusReviewResetButtonClick(Sender: TObject);
var
  Index: Integer;
begin
  if not Assigned(FStatusReviewList) then
    Exit;

  FUpdatingStatusReview := True;
  try
    for Index := 0 to FStatusReviewList.Items.Count - 1 do
      SetStatusReviewItemChecked(Index, True);
  finally
    FUpdatingStatusReview := False;
  end;

  UpdateStatusReviewPanel;
end;

procedure TMainForm.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift <> [] then
    Exit;

  case Key of
    VK_PRIOR:
      begin
        if ActionPreviousVerse.Enabled then
          ActionPreviousVerse.Execute;
        Key := 0;
      end;
    VK_NEXT:
      begin
        if ActionNextVerse.Enabled then
          ActionNextVerse.Execute;
        Key := 0;
      end;
  end;
end;

procedure TMainForm.TreeViewShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  Node: TTreeNode;
  HintText: string;
begin
  if (not Assigned(HintInfo)) or (not Assigned(CurrentProject)) then
    Exit;

  Node := TreeView1.GetNodeAt(HintInfo^.CursorPos.X, HintInfo^.CursorPos.Y);
  if not Assigned(Node) then
  begin
    HintInfo^.HintStr := '';
    Exit;
  end;

  HintText := CurrentProject.GetTreeNodeStatusHint(Node);
  HintInfo^.HintStr := HintText;
  HintInfo^.HintMaxWidth := 360;
end;

procedure TMainForm.LayoutVersePanelLabels;
const
  Margin = 5;
  LabelHeight = 17;
var
  TextType: TTipoTextoBiblico;
  LabelControl: TPanel;
  TypeLabel: TLabel;
  DescriptionLabel: TLabel;
  PanelControl: TControl;
  CaptionText: string;
  TypeCaption: string;
  DescriptionCaption: string;
  TypeWidth: Integer;
  DescriptionWidth: Integer;
  LabelWidth: Integer;
  AvailableWidth: Integer;
begin
  for TextType := tbOrigem to tbConsulta2 do
  begin
    LabelControl := FVersePanelLabels[TextType];
    TypeLabel := FVersePanelTypeLabels[TextType];
    DescriptionLabel := FVersePanelDescriptionLabels[TextType];
    if (not Assigned(LabelControl)) or (not Assigned(TypeLabel)) or
      (not Assigned(DescriptionLabel)) then
      Continue;

    case TextType of
      tbOrigem: PanelControl := ScrollBoxSrcVerse;
      tbDestino: PanelControl := ScrollBoxDstVerse;
      tbConsulta1: PanelControl := ScrollBoxRef1Verse;
      tbConsulta2: PanelControl := ScrollBoxRef2Verse;
    else
      PanelControl := nil;
    end;

    if not Assigned(PanelControl) then
      Continue;

    CaptionText := GetVersePanelCaption(TextType);
    LabelControl.Hint := CaptionText;
    TypeLabel.Hint := CaptionText;
    DescriptionLabel.Hint := CaptionText;

    TypeCaption := GetVersePanelTypeCaption(TextType);
    DescriptionCaption := GetVersePanelDescription(TextType);
    if DescriptionCaption <> '' then
      DescriptionCaption := ': ' + DescriptionCaption;

    TypeLabel.Caption := TypeCaption;
    DescriptionLabel.Caption := DescriptionCaption;
    DescriptionLabel.Visible := DescriptionCaption <> '';

    CenterPanel.Canvas.Font.Assign(TypeLabel.Font);
    TypeWidth := CenterPanel.Canvas.TextWidth(TypeCaption);
    CenterPanel.Canvas.Font.Assign(DescriptionLabel.Font);
    DescriptionWidth := CenterPanel.Canvas.TextWidth(DescriptionCaption);

    AvailableWidth := Max(1, PanelControl.Width - (Margin * 2));
    LabelWidth := Min(TypeWidth + DescriptionWidth + 16, AvailableWidth);
    LabelControl.SetBounds(
      PanelControl.Left + Max(Margin, PanelControl.Width - LabelWidth - Margin),
      PanelControl.Top + Max(Margin, PanelControl.Height - LabelHeight - Margin),
      LabelWidth,
      LabelHeight);

    TypeWidth := Min(TypeWidth, Max(0, LabelWidth - 8));
    DescriptionWidth := Max(0, LabelWidth - TypeWidth - 8);
    TypeLabel.SetBounds(6, 0, TypeWidth, LabelHeight);
    DescriptionLabel.SetBounds(6 + TypeWidth, 0, DescriptionWidth, LabelHeight);
    LabelControl.BringToFront;
  end;
end;

procedure TMainForm.ResetMainLayout;
const
  DefaultTreeWidth = 145;
  DefaultContextWidth = 321;
  DefaultCommentsHeight = 120;
  MinCommentsHeight = 80;
  MinVerseHeight = 42;
var
  TextTop, TextBottom, AvailableHeight, BaseHeight, ExtraHeight: Integer;
  TextWidth: Integer;
begin
  NavigationPanel.Width := DefaultTreeWidth;
  ContextPanel.Width := Min(DefaultContextWidth, Max(180, Panel1.ClientWidth div 2));
  BottomPanel.Height := Min(DefaultCommentsHeight,
    Max(MinCommentsHeight, CenterPanel.ClientHeight div 3));

  TextTop := 1;
  TextBottom := CenterPanel.ClientHeight - BottomPanel.Height - Splitter5.Height;
  AvailableHeight := TextBottom - TextTop - Splitter2.Height - Splitter3.Height - Splitter4.Height;
  BaseHeight := Max(MinVerseHeight, AvailableHeight div 4);
  ExtraHeight := Max(0, AvailableHeight - (BaseHeight * 4));
  TextWidth := CenterPanel.ClientWidth - 2;

  ScrollBoxSrcVerse.SetBounds(1, TextTop, TextWidth, BaseHeight + ExtraHeight);
  Splitter2.SetBounds(1, ScrollBoxSrcVerse.Top + ScrollBoxSrcVerse.Height,
    TextWidth, Splitter2.Height);
  ScrollBoxDstVerse.SetBounds(1, Splitter2.Top + Splitter2.Height,
    TextWidth, BaseHeight);
  Splitter3.SetBounds(1, ScrollBoxDstVerse.Top + ScrollBoxDstVerse.Height,
    TextWidth, Splitter3.Height);
  ScrollBoxRef1Verse.SetBounds(1, Splitter3.Top + Splitter3.Height,
    TextWidth, BaseHeight);
  Splitter4.SetBounds(1, ScrollBoxRef1Verse.Top + ScrollBoxRef1Verse.Height,
    TextWidth, Splitter4.Height);
  ScrollBoxRef2Verse.SetBounds(1, Splitter4.Top + Splitter4.Height,
    TextWidth, Max(MinVerseHeight, TextBottom - Splitter4.Top - Splitter4.Height));

  LayoutVersePanelLabels;
end;

procedure TMainForm.SplitterMoved(Sender: TObject);
begin
  LayoutVersePanelLabels;
end;

procedure TMainForm.ActionOpenProjectExecute(Sender: TObject);
begin
  if Sender is TMenuItem then
     OpenDialog1.FileName := TMenuItem(Sender).Caption
  else if not OpenDialog1.Execute then
    exit;

  if CurrentProject <> nil then
    ActionCloseProjectExecute(nil);

  if (CurrentProject <> nil) or (OpenDialog1.FileName = '(-)') then
    exit;

  if not FileExists(OpenDialog1.FileName) then
  begin
    MessageDlg(SOpenProject, format(SProjectFileDoesNotExist, [OpenDialog1.FileName]), mtError, [mbOK], 0);
    exit;
  end;

  DoOpenProject(OpenDialog1.FileName);
end;

procedure TMainForm.ActionExportProjectExecute(Sender: TObject);
begin
  if not assigned(CurrentProject) then
    exit;
  ProjectExportForm.SetProject(CurrentProject);
  ProjectExportForm.ShowModal;
end;

procedure TMainForm.ActionApplyPatchExecute(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  patch: string;
begin
  if not assigned(CurrentProject) then
    exit;

  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.DefaultExt := '.ibpatch';
    SaveDlg.Filter := SIBibliaPatchFiles + '|*.ibpatch';
    SaveDlg.Title := SExportTextSaveDlgTitle;
    if SaveDlg.Execute then
      patch := SaveDlg.FileName
    else
      patch := '';
  finally
    SaveDlg.Free;
  end;

  if patch.IsEmpty then
    exit;

  if FrmApplyPatch.LoadPatch(patch) and (FrmApplyPatch.ShowModal = mrOK) then
  begin
    Screen.Cursor := crHourGlass;
    FrmApplyPatch.Apply;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ActionExportDestinationWithStrongsExecute(Sender: TObject);
begin
  if (CurrentProject <> nil) and SaveDialog2.Execute then
    ProjectExportForm.ShowExportResult(
      CurrentProject.ExportDestinationTextWithStrongs(SaveDialog2.FileName, []));
  //ProgressBar1.Visible := false;
end;

procedure TMainForm.ActionExportInterlinearTextExecute(Sender: TObject);
begin
  if (CurrentProject <> nil) and SaveDialog2.Execute then
    ProjectExportForm.ShowExportResult(
      CurrentProject.ExportInterlinearText(SaveDialog2.FileName, []));
  //ProgressBar1.Visible := false;
end;

procedure TMainForm.ActionExportPatchExecute(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  destination: string;
begin
  if not assigned(CurrentProject) then exit;

  if FrmExportPatch.ShowModal <> mrOK then
    exit;

  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.DefaultExt := '.ibpatch';
    SaveDlg.Filter := SIBibliaPatchFiles + '|*.ibpatch';
    SaveDlg.Title := SExportTextSaveDlgTitle;
    if SaveDlg.Execute then
      destination := SaveDlg.FileName
    else
      destination := '';
  finally
    SaveDlg.Free;
  end;

  if destination.IsEmpty then
    exit;

  Screen.Cursor := crHourGlass;
  FrmExportPatch.SavePatch(destination);
  Screen.Cursor := crDefault;

  ShowMessage(SPatchSuccessfullyExported);
end;

procedure TMainForm.ActionCloseProjectExecute(Sender: TObject);
var
  SaveChanges: boolean;
begin
  if CurrentProject = nil then
    exit;

  SaveChanges := false;
  if CurrentProject.Modificado then
  begin
    case MessageDlg(SCloseProject, SSaveChanges, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: SaveChanges := true;
      mrNo: SaveChanges := false;
      mrCancel: exit;
    end;
  end;

  UpdateRecentProjects(MenuItemRecent);

  if (ChapterViewTabCtrl.TabIndex >= Ord(tbOrigem)) and
     (ChapterViewTabCtrl.TabIndex <= Ord(tbInterlinear)) then
    CurrentProject.ChapterViewText :=
      TTipoTextoBiblico(ChapterViewTabCtrl.TabIndex);
  if not CurrentProject.CloseProject(SaveChanges) then
    Exit;
  CurrentProject.Destroy;
  CurrentProject := nil;
  UpdateMainCaption;
  UpdateNavigationHistoryActions;

  FCurrentRef := '';

  ActionSaveProject.Enabled := false;
  ActionSaveProjectAs.Enabled := false;
  ActionCloseProject.Enabled := false;
  ActionProjectProperties.Enabled := false;
  ActionFirstVerse.Enabled := false;
  ActionLastVerse.Enabled := false;
  ActionNextVerse.Enabled := false;
  ActionPreviousVerse.Enabled := false;
  ActionSuggestAssociation.Enabled := false;
  ActionRollbackCurrentVerse.Enabled := false;
  ActionClearAssociations.Enabled := false;
  ActionExportProject.Enabled := false;
  ActionRebuildSuggestionDatabase.Enabled := false;
  ActionExportPatch.Enabled := false;
  ActionApplyPatch.Enabled := false;
  if Assigned(ActionLlmOptions) then ActionLlmOptions.Enabled := false;
  FChapterView.Enabled := false;
  FChapterView.Clear;
  //ActionExportDestinationWithStrongs.Enabled := false;
  //ActionExportInterlinearText.Enabled := false;
  StatusBar1.SimpleText := '';
  UpdateStateHeader;
  UpdateStatusReviewPanel;
  LayoutVersePanelLabels;
end;

procedure TMainForm.ActionHistoryBackExecute(Sender: TObject);
begin
  if CurrentProject = nil then
    Exit;

  CurrentProject.GoBack;
  UpdateNavigationHistoryActions;
end;

procedure TMainForm.ActionHistoryForwardExecute(Sender: TObject);
begin
  if CurrentProject = nil then
    Exit;

  CurrentProject.GoForward;
  UpdateNavigationHistoryActions;
end;

procedure TMainForm.ActionKeyboardShortcutsExecute(Sender: TObject);
begin
  MessageDlg(SKeyboardShortcutsTitle, BuildKeyboardShortcutsText,
    mtInformation, [mbOK], 0);
end;

procedure TMainForm.ActionClearAssociationsExecute(Sender: TObject);
begin
  if CurrentProject = nil then
    Exit;

  if MessageDlg(SClearCurrentVerseAssociations,
      Format(SClearCurrentVerseAssociationsConfirmation,
        [CurrentProject.FormattedReference]),
      mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  begin
    CurrentProject.ClearAssociations;
    FChapterView.RefreshTranslationMemory;
    UpdateStateHeader;
    UpdateStatusReviewPanel;
  end;
end;

procedure TMainForm.ActionMergeProjectsExecute(Sender: TObject);
begin
  FrmMergeProjects.InitializeForm;
  FrmMergeProjects.ShowModal;
end;

procedure TMainForm.ActionShowTagsExecute(Sender: TObject);
begin
  if assigned(CurrentProject) then
    CurrentProject.ToggleDisplayTags;
end;

procedure TMainForm.ActionNewProjectExecute(Sender: TObject);
  function GetTextTypeName(v: TTipoTextoBiblico): string;
  begin
    case v of
      tbOrigem: result := SSource;
      tbDestino: result := SDestination;
      tbConsulta1: result := SReference1;
      tbConsulta2: result := SReference2;
    else
      result := '';
    end;
  end;

  function GetLoadingMessage(v: TTipoTextoBiblico): string;
  begin
    case v of
      tbOrigem: result := SLoadingSourceTextProgress;
      tbDestino: result := SLoadingDestinationTextProgress;
      tbConsulta1: result := SLoadingReference1TextProgress;
      tbConsulta2: result := SLoadingReference2TextProgress;
    else
      result := Format(SLoadingFile, [GetTextTypeName(v)]);
    end;
  end;

  procedure AppendFailure(var AFailure: string; const ADetail: string);
  begin
    if Trim(ADetail) = '' then
      Exit;
    if AFailure <> '' then
      AFailure := AFailure + LineEnding + LineEnding;
    AFailure := AFailure + ADetail;
  end;

var
  v: TTipoTextoBiblico;
  Draft: TProjectSetupDraft;
  Staging: TProjectCreationStaging;
  ImportResult: TTheWordModuleImportResult;
  FinalProjectFileName: string;
  FailureMessage: string;
  CleanupMessage: string;
  BuildSucceeded: Boolean;
  ImportCancelled: Boolean;
begin
  ActionCloseProjectExecute(nil);

  if CurrentProject <> nil then
    exit;

  FrmNewProject.ResetDraft(SNewAssociationProject);

  if FrmNewProject.ShowModal <> mrOK then
    exit;

  Draft := FrmNewProject.GetDraft;
  FinalProjectFileName := Draft.ProjectFileName;
  Staging := nil;
  FailureMessage := '';
  BuildSucceeded := False;
  ImportCancelled := False;

  try
    FormPleaseWait.BeginOperation(Self, SNewProjectProgressTitle,
      SCreatingProjectProgress, 1, FinalProjectFileName);
    try
      try
        Staging := TProjectCreationStaging.Create(FinalProjectFileName);
        CurrentProject := TProject.Create([ScrollBoxSrcVerse,
          ScrollBoxDstVerse, ScrollBoxRef1Verse, ScrollBoxRef2Verse],
          TreeView1, RadioGroupStatus, FActualCommentsMemo);
        CurrentProject.Escopo := Draft.Scope;
        CurrentProject.PalavrasComStrongEmNegrito :=
          MenuItemBoldStrongs.Checked;
        CurrentProject.MostrarQtdStrongs := FStrongsCountMode;
        CurrentProject.OnNewVerseSubscribe(@ProjectNewVerse);
        CurrentProject.OnAlterarVersiculo := @VerseChanged;
        CurrentProject.OnSintagmaClick := @SyntagmClick;
        CurrentProject.CreateProject(Staging.StagingFileName,
          Draft.ProjectName, Draft.TokenizerVersion);
        CurrentProject.SetInfo(PROJECT_SCENARIO_CONFIG_KEY,
          ProjectScenarioToConfig(Draft.Scenario));
        if Draft.HasCustomStatusCatalog then
          CurrentProject.SetStatusDefinitions(Draft.StatusDefinitions);
        CurrentProject.ExibirDefinicoesSoComCtrl := MenuItemDictPopup.Checked;
        CurrentProject.SugerirAssociacaoAutomaticamente := MenuItem22.Checked;
        CurrentProject.SuggestionMode := FSuggestionMode;
        CurrentProject.PopupTrigger := FPopupTrigger;

        for v := tbOrigem to tbConsulta2 do
        begin
          if Draft.Texts[v].ModulePath = '' then
            Continue;
          StatusBar1.Caption := Format(SLoadingFile, [GetTextTypeName(v)]);
          FormPleaseWait.SetMessage(GetLoadingMessage(v));
          FormPleaseWait.SetDetail(Draft.Texts[v].ModulePath);
          if not CurrentProject.ImportTheWordModule(
              Draft.Texts[v].ModulePath, v, FormPleaseWait.ProgressBar1,
              False, ImportResult, True) then
          begin
            ImportCancelled := ImportResult.Status = twmisCancelled;
            if ImportCancelled then
              FailureMessage := Format(SNewProjectImportCancelled,
                [GetTextTypeName(v)])
            else
            begin
              FailureMessage := Format(SNewProjectImportFailed,
                [GetTextTypeName(v), Draft.Texts[v].ModulePath]);
              if Trim(ImportResult.ErrorMessage) <> '' then
                AppendFailure(FailureMessage, ImportResult.ErrorMessage);
            end;
            raise Exception.Create(FailureMessage);
          end;
        end;
        StatusBar1.Caption := '';

        for v := tbOrigem to tbConsulta2 do
        begin
          if Draft.Texts[v].StrongDictionaryPath <> '' then
            CurrentProject.AssignStrongDictionary(
              Draft.Texts[v].StrongDictionaryPath, [v]);
          if Draft.Texts[v].MorphologyDictionaryPath <> '' then
            CurrentProject.AssignMorphologyDictionary(
              Draft.Texts[v].MorphologyDictionaryPath, [v]);
        end;

        CurrentProject.Commit;
        if not CurrentProject.CloseProject(True) then
          raise Exception.Create(SNewProjectCloseFailed);
        FreeAndNil(CurrentProject);

        Staging.Publish;
        BuildSucceeded := True;
      except
        on E: Exception do
          if FailureMessage = '' then
          begin
            FailureMessage := E.Message;
            if FailureMessage = '' then
              FailureMessage := SNewProjectUnknownFailure;
          end;
      end;

      if not BuildSucceeded then
      begin
        CleanupMessage := '';
        if Assigned(CurrentProject) then
        begin
          try
            if not CurrentProject.CloseProject(False) then
              AppendFailure(CleanupMessage,
                SNewProjectCleanupCloseFailed);
          except
            on E: Exception do
              AppendFailure(CleanupMessage, E.Message);
          end;
          try
            FreeAndNil(CurrentProject);
          except
            on E: Exception do
              AppendFailure(CleanupMessage, E.Message);
          end;
        end;
        if Assigned(Staging) then
          try
            Staging.Discard;
          except
            on E: Exception do
              AppendFailure(CleanupMessage,
                Format(SNewProjectCleanupDeleteFailed, [E.Message]));
          end;
        if CleanupMessage <> '' then
          AppendFailure(FailureMessage,
            Format(SNewProjectCleanupFailed, [CleanupMessage]));
      end;
    finally
      StatusBar1.Caption := '';
      FormPleaseWait.FinishOperation;
    end;

    if BuildSucceeded then
    begin
      try
        DoOpenProject(FinalProjectFileName);
      except
        on E: Exception do
        begin
          BuildSucceeded := False;
          FailureMessage := Format(SNewProjectOpenFailed, [E.Message]);
          if Assigned(CurrentProject) then
          begin
            try
              CurrentProject.CloseProject(False);
            except
            end;
            FreeAndNil(CurrentProject);
          end;
          MessageDlg(SNewProjectOpenFailedTitle, FailureMessage,
            mtError, [mbOK], 0);
          Exit;
        end;
      end;
    end;

    if not BuildSucceeded then
    begin
      if ImportCancelled then
        MessageDlg(SNewProjectNotCreatedTitle, FailureMessage,
          mtInformation, [mbOK], 0)
      else
        MessageDlg(SNewProjectNotCreatedTitle, FailureMessage,
          mtError, [mbOK], 0);
    end;
  finally
    Staging.Free;
  end;
end;

procedure TMainForm.ActionProjectPropertiesExecute(Sender: TObject);
begin
  FrmProjectProperties.SetProject(CurrentProject);
  if FrmProjectProperties.ShowModal = mrOK then
  begin
    FrmProjectProperties.ApplyChanges(Self);
    ActionNewVerse.Execute;
    UpdateStatusReviewPanel;
    LayoutVersePanelLabels;
  end;
end;

procedure TMainForm.ActionLlmOptionsExecute(Sender: TObject);
begin
  if not assigned(CurrentProject) then Exit;
  if not assigned(FrmLLMOptions) then
    Application.CreateForm(TFrmLLMOptions, FrmLLMOptions);
  FrmLLMOptions.SetProject(CurrentProject);
  FrmLLMOptions.ShowModal;
end;

procedure TMainForm.ActionTheWordHighlighterColorsExecute(Sender: TObject);
begin
  TFrmTheWordHighlighterColors.Execute(Self, opts, Self);
end;

procedure TMainForm.ActionNewVerseExecute(Sender: TObject);
begin
  UpdateNavigationHistoryActions;
  if CurrentProject <> nil then
  begin
    UpdateMainCaption;
    ActionSyncTheWordVerseExecute(Sender);
    UpdateStateHeader;
  end;
end;

procedure TMainForm.ActionRebuildSuggestionDatabaseExecute(Sender: TObject);
begin
  if CurrentProject <> nil then
  begin
    FormPleaseWait.BeginOperation(Self, SRebuildAssociationMemoryTitle,
      SRebuildAssociationMemoryProgress, 1);
    try
      CurrentProject.RebuildSuggestionDatabase(FormPleaseWait.ProgressBar1);
    finally
      FormPleaseWait.FinishOperation;
    end;
  end;
end;

procedure TMainForm.ActionResetLayoutExecute(Sender: TObject);
begin
  ResetMainLayout;
end;

procedure TMainForm.ActionRollbackCurrentVerseExecute(Sender: TObject);
begin
  if CurrentProject = nil then
    exit;
  if QuestionDlg(SRollbackCurrentVerse,
      Format(SRollbackCurrentVerseConfirmation, [CurrentProject.FormattedReference]),
      mtConfirmation,
      [mrYes, SButtonYes, 'isdefault', mrCancel, SButtonCancel, 'iscancel'], 0) = mrYes then
  begin
    CurrentProject.RollbackCurrentVerse;
    UpdateStateHeader;
  end;
end;

procedure TMainForm.ActionExitExecute(Sender: TObject);
begin
  ActionCloseProjectExecute(nil);
  if CurrentProject = nil then
    Close;
end;

procedure TMainForm.ActionSaveProjectAsExecute(Sender: TObject);
begin
  {if CurrentProject <> nil then
  begin
    old := CurrentProject.Caminho;
    CurrentProject.CloseProject(true);
    CurrentProject.CreateProject(old, );

  end;
  }
end;

procedure TMainForm.ActionSaveProjectExecute(Sender: TObject);
begin
  if CurrentProject <> nil then
  begin
    if CurrentProject.SaveProject then
    begin
      StatusBar1.SimpleText := '';
      UpdateStateHeader;
    end;
  end;
end;

procedure TMainForm.ActionSuggestAssociationExecute(Sender: TObject);
begin
  if CurrentProject <> nil then
  begin
    if CurrentProject.SuggestionMode = smAI then
    begin
      StatusBar1.SimpleText := SAnalyzingVerse;
      try
        CurrentProject.SuggestAssociation;
      finally
        if CurrentProject.LastLLMCallCompleted then
          StatusBar1.SimpleText := FormatLLMUsageStatus(CurrentProject.LastLLMUsage)
        else
          StatusBar1.SimpleText := '';
      end;
    end
    else
      CurrentProject.SuggestAssociation;

    if (FChapterView.ViewMode = cvmChapterText) and
       (CurrentProject.ChapterViewText = tbInterlinear) then
      FChapterView.LoadChapter;
    FChapterView.RefreshTranslationMemory;
    UpdateStateHeader;
  end;
end;

procedure TMainForm.ActionSyncTheWordVerseExecute(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  if FSyncTheWordToIBiblia and (CurrentProject <> nil) then
    SyncTheWordVerse(CurrentProject.ID);
  {$ENDIF}
end;

procedure TMainForm.ActionVaccumProjectExecute(Sender: TObject);
begin
  if CurrentProject = nil then exit;

  CurrentProject.Vacuum;
end;

procedure TMainForm.ActionPreviousVerseExecute(Sender: TObject);
begin
  if CurrentProject = nil then exit;

  CurrentProject.PreviousVerse;
  UpdateStateHeader;
end;

procedure TMainForm.ActionFirstVerseExecute(Sender: TObject);
begin
  if CurrentProject = nil then exit;

  CurrentProject.FirstVerse;
  UpdateStateHeader;
end;

procedure TMainForm.ActionNextVerseExecute(Sender: TObject);
begin
  if CurrentProject = nil then exit;

  CurrentProject.NextVerse;
  UpdateStateHeader;
end;

procedure TMainForm.ActionLastVerseExecute(Sender: TObject);
begin
  if CurrentProject = nil then exit;

  CurrentProject.LastVerse;
  UpdateStateHeader;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ActionCloseProjectExecute(Sender);
  if CurrentProject <> nil then
    CloseAction := caNone;
end;

procedure TMainForm.FormCreate(Sender: TObject);

  function GetDefaultLang: string;
  var
    fallbackLang: string;
  begin
    result := '';
    fallbackLang := 'en';
    LazGetLanguageIDs(result, fallbackLang);
  end;

var
  m, modeInt: integer;
  ConfigFilePath: string;
begin
  DebugLn('=================================');

  CurrentProject := nil;

  ConfigFilePath := ExtractFilePath(ParamStr(0)) + 'iBiblia.ini';
  opts := TIniFile.Create(ConfigFilePath);
  LoadTheWordSyntaxColorScheme(opts);
  LoadTheWordSyntaxEditorFont(opts);

  if opts.ReadBool('leiaute', 'principal.maximized', false) then
    WindowState := wsMaximized
  else
  begin
    Width  := opts.ReadInteger('leiaute', 'principal.largura',  Width);
    Height := opts.ReadInteger('leiaute', 'principal.altura',   Height);
  end;

  MenuItem22.Checked := opts.ReadBool('opcoes', 'sugestoes.automaticas', false);
  MenuItemSyncTheWord.Checked := opts.ReadBool('opcoes', 'synctheword', false);
  MenuItemSynciBiblia.Checked := opts.ReadBool('opcoes', 'syncibiblia', false);
  MenuItemBoldStrongs.Checked := opts.ReadBool('opcoes', 'boldstrongs', false);
  MenuItemAutoSave.Checked := opts.ReadBool('opcoes', 'auto.save', false);
  FGuideModeEnabled := opts.ReadBool('opcoes', 'guide.mode', true);
  FDefaultVerseMode := DefaultVerseModeFromInteger(
    opts.ReadInteger('opcoes', 'verse.default.mode',
      Ord(dvmAssociation)));
  modeInt := opts.ReadInteger('opcoes', 'translation.memory.order',
    Ord(tmoVerseOccurrence));
  FTranslationMemoryOrder := TranslationMemoryOrderFromInteger(modeInt);

  // association engine
  modeInt := opts.ReadInteger('opcoes', 'suggestion.mode', 0);
  if modeInt = 1 then
    FSuggestionMode := smAI
  else
    FSuggestionMode := smStatistical;
  MenuItemStatisticalSuggestion.Checked := FSuggestionMode = smStatistical;
  MenuItemAISuggestion.Checked := FSuggestionMode = smAI;

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

  FChapterView := TChapterView.Create(ChapterViewTabCtrl);
  with FChapterView do
  begin
    ParentWindow := ChapterViewTabCtrl.Handle;
    BorderStyle  := bsNone;
    Align        := alClient;
    FontName     := opts.ReadString('leiaute', 'principal.chapterview.font.name', DefFontData.Name);
    FontSize     := opts.ReadInteger('leiaute', 'principal.chapterview.font.size', 0);
    TranslationMemoryPreviewFontSize :=
      ReadTranslationMemoryPreviewFontSize(opts, FontSize);
    OnTranslationMemoryPreviewFontSizeChanged :=
      @HandleTranslationMemoryPreviewFontSizeChanged;
    TranslationMemoryOrder := FTranslationMemoryOrder;
  end;

  language := opts.ReadString('opcoes', 'language', GetDefaultLang);
  SetDefaultLang(language);
  if language = 'en' then
    MenuItemLangEn.Checked:=true
  else if language = 'pt' then
    MenuItemLangPt.Checked:=true;

  CreateStateHeader;
  CreateGuidePanel;
  CreateStatusReviewPanel;
  CreateVersePanelLabels;
  CreateCommentsGroupBox;
  TreeView1.Hint := ' ';
  TreeView1.ShowHint := True;
  TreeView1.OnKeyDown := @TreeViewKeyDown;
  TreeView1.OnShowHint := @TreeViewShowHint;
  TreeView1.Options := TreeView1.Options - [tvoToolTips];

  ActionHistoryBack.ShortCut := ShortCut(VK_LEFT, [ssAlt]);
  ActionHistoryForward.ShortCut := ShortCut(VK_RIGHT, [ssAlt]);

  ActionKeyboardShortcuts := TAction.Create(ActionList1);
  ActionKeyboardShortcuts.Caption := SKeyboardShortcutsMenu;
  ActionKeyboardShortcuts.Hint := SKeyboardShortcutsHint;
  ActionKeyboardShortcuts.ShortCut := ShortCut(VK_F1, []);
  ActionKeyboardShortcuts.OnExecute := @ActionKeyboardShortcutsExecute;

  MenuItemKeyboardShortcuts := TMenuItem.Create(MainMenu1);
  MenuItemKeyboardShortcuts.Action := ActionKeyboardShortcuts;
  MenuItem17.Insert(0, MenuItemKeyboardShortcuts);

  ActionResetLayout := TAction.Create(ActionList1);
  ActionResetLayout.Caption := SResetLayoutMenu;
  ActionResetLayout.Hint := SResetLayoutHint;
  ActionResetLayout.OnExecute := @ActionResetLayoutExecute;

  MenuItemResetLayout := TMenuItem.Create(MainMenu1);
  MenuItemResetLayout.Action := ActionResetLayout;
  MenuItem20.Add(MenuItemResetLayout);

  ActionGuideMode := TAction.Create(ActionList1);
  ActionGuideMode.Caption := SGuideModeMenu;
  ActionGuideMode.Hint := SGuideModeHint;
  ActionGuideMode.Checked := FGuideModeEnabled;
  ActionGuideMode.OnExecute := @ActionGuideModeExecute;

  MenuItemGuideMode := TMenuItem.Create(MainMenu1);
  MenuItemGuideMode.Action := ActionGuideMode;
  MenuItemGuideMode.Checked := FGuideModeEnabled;
  MenuItem20.Add(MenuItemGuideMode);

  FDefaultVerseModeMenu := TMenuItem.Create(MainMenu1);
  FDefaultVerseModeAssociationItem := TMenuItem.Create(MainMenu1);
  FDefaultVerseModeAssociationItem.RadioItem := True;
  FDefaultVerseModeAssociationItem.OnClick := @DefaultVerseModeClick;
  FDefaultVerseModeMenu.Add(FDefaultVerseModeAssociationItem);
  FDefaultVerseModeEditingItem := TMenuItem.Create(MainMenu1);
  FDefaultVerseModeEditingItem.RadioItem := True;
  FDefaultVerseModeEditingItem.OnClick := @DefaultVerseModeClick;
  FDefaultVerseModeMenu.Add(FDefaultVerseModeEditingItem);
  MenuItem20.Add(FDefaultVerseModeMenu);
  UpdateDefaultVerseModeMenu;

  ActionTheWordHighlighterColors := TAction.Create(ActionList1);
  ActionTheWordHighlighterColors.Caption := STheWordHighlightColorsMenu;
  ActionTheWordHighlighterColors.OnExecute := @ActionTheWordHighlighterColorsExecute;

  MenuItemTheWordHighlighterColors := TMenuItem.Create(MainMenu1);
  MenuItemTheWordHighlighterColors.Action := ActionTheWordHighlighterColors;
  MenuItem20.Add(MenuItemTheWordHighlighterColors);

  FTranslationMemoryOrderMenu := TMenuItem.Create(MainMenu1);
  FTranslationMemoryVerseOrderItem := TMenuItem.Create(MainMenu1);
  FTranslationMemoryVerseOrderItem.RadioItem := True;
  FTranslationMemoryVerseOrderItem.OnClick := @TranslationMemoryOrderClick;
  FTranslationMemoryOrderMenu.Add(FTranslationMemoryVerseOrderItem);
  FTranslationMemoryRareFirstItem := TMenuItem.Create(MainMenu1);
  FTranslationMemoryRareFirstItem.RadioItem := True;
  FTranslationMemoryRareFirstItem.OnClick := @TranslationMemoryOrderClick;
  FTranslationMemoryOrderMenu.Add(FTranslationMemoryRareFirstItem);
  MenuItem20.Add(FTranslationMemoryOrderMenu);
  UpdateTranslationMemoryOrderMenu;

  ActionRegisterFileAssociation := TAction.Create(ActionList1);
  ActionRegisterFileAssociation.Caption := SRegisterFileAssociation;
  ActionRegisterFileAssociation.OnExecute := @ActionRegisterFileAssociationExecute;
  MenuItemRegisterFileAssoc.Action := ActionRegisterFileAssociation;

  ActionUnregisterFileAssociation := TAction.Create(ActionList1);
  ActionUnregisterFileAssociation.Caption := SUnregisterFileAssociation;
  ActionUnregisterFileAssociation.OnExecute := @ActionUnregisterFileAssociationExecute;
  MenuItemUnregisterFileAssoc.Action := ActionUnregisterFileAssociation;

  Translate;

  // Create runtime Action and MenuItem for LLM options
  ActionLlmOptions := TAction.Create(ActionList1);
  ActionLlmOptions.Caption := SLLMOptionsMenu;
  ActionLlmOptions.Enabled := False;
  ActionLlmOptions.OnExecute := @ActionLlmOptionsExecute;

  MenuItemLlmOptions := TMenuItem.Create(MainMenu1);
  MenuItemLlmOptions.Action := ActionLlmOptions;
  // Add under Options menu
  MenuItem20.Add(MenuItemLlmOptions);

  LoadRecentProjects(MenuItemRecent);
  NavigationPanel.Width := opts.ReadInteger('leiaute', 'principal.treeview.largura', NavigationPanel.Width);
  UpdateStatusReviewPanel;

  // Create the actual CommentsView and replace the TMemo
  FActualCommentsMemo := TCommentsView.Create(Self);
  FActualCommentsMemo.Parent := CommentsMemo.Parent;
  FActualCommentsMemo.Align := CommentsMemo.Align;
  FActualCommentsMemo.BorderSpacing.Around := CommentsMemo.BorderSpacing.Around;
  FActualCommentsMemo.Enabled := CommentsMemo.Enabled;
  FActualCommentsMemo.TabOrder := CommentsMemo.TabOrder;
  FActualCommentsMemo.FontName := opts.ReadString('leiaute', 'principal.commentsmemo.font.name', DefFontData.Name);
  FActualCommentsMemo.FontSize := opts.ReadInteger('leiaute', 'principal.commentsmemo.font.size', 10);
  FActualCommentsMemo.ScrollBars := ssVertical;
  CommentsMemo.Visible := False; // Hide the original TMemo

  {$IFDEF WINDOWS}
  FRxMorpho := RegexCreate('^(\S+)( l="(.*?)")?$', [rcoUTF8]);
  FSyncTheWordToIBiblia := MenuItemSyncTheWord.Checked;
  FSyncIBibliaToTheWord := MenuItemSynciBiblia.Checked;
  SetUpSyncThread;

  // Check and update file association menu items
  CheckFileAssociation;
  {$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveRecentProjects(MenuItemRecent);
  opts.WriteBool('leiaute', 'principal.maximized', WindowState = wsMaximized);
  if WindowState <> wsMaximized then
  begin
    opts.WriteInteger('leiaute', 'principal.largura', Width);
    opts.WriteInteger('leiaute', 'principal.altura', Height);
    opts.WriteInteger('leiaute', 'principal.topo', Top);
    opts.WriteInteger('leiaute', 'principal.esquerda', Left);
  end;
  opts.WriteInteger('leiaute', 'principal.treeview.largura', NavigationPanel.Width);
  opts.WriteInteger('leiaute', 'principal.splitter2.topo', Splitter2.Top);
  opts.WriteInteger('leiaute', 'principal.splitter3.topo', Splitter3.Top);
  opts.WriteInteger('leiaute', 'principal.splitter4.topo', Splitter4.Top);
  opts.WriteInteger('leiaute', 'principal.panel3.height', BottomPanel.Height);
  opts.WriteInteger('leiaute', 'principal.contextpanel.width', ContextPanel.Width);
  opts.WriteInteger('leiaute', 'principal.chapterview.font.size', FChapterView.FontSize);
  opts.WriteString('leiaute', 'principal.chapterview.font.name', FChapterView.FontName);
  WriteTranslationMemoryPreviewFontSize(opts,
    FChapterView.TranslationMemoryPreviewFontSize);
  opts.WriteInteger('leiaute', 'principal.commentsmemo.font.size', FActualCommentsMemo.FontSize);
  opts.WriteString('leiaute', 'principal.commentsmemo.font.name', FActualCommentsMemo.FontName);
  opts.WriteBool('opcoes', 'sugestoes.automaticas', MenuItem22.Checked);
  opts.WriteInteger('opcoes', 'suggestion.mode', Ord(FSuggestionMode));
  opts.WriteBool('opcoes', 'synctheword', MenuItemSyncTheWord.Checked);
  opts.WriteBool('opcoes', 'syncibiblia', MenuItemSynciBiblia.Checked);
  opts.WriteBool('opcoes', 'boldstrongs', MenuItemBoldStrongs.Checked);
  opts.WriteInteger('opcoes', 'strongscountmode', Integer(FStrongsCountMode));
  opts.WriteString('opcoes', 'language', language);
  opts.WriteBool('opcoes', 'alwaysontop', MenuItemAlwaysOnTop.Checked);
  opts.WriteInteger('opcoes', 'popuptrigger', LongInt(FPopupTrigger));
  opts.WriteBool('opcoes', 'auto.save', MenuItemAutoSave.Checked);
  opts.WriteBool('opcoes', 'guide.mode', FGuideModeEnabled);
  opts.WriteInteger('opcoes', 'verse.default.mode',
    Ord(FDefaultVerseMode));
  opts.WriteInteger('opcoes', 'translation.memory.order',
    Ord(FTranslationMemoryOrder));
  opts.Free;

  FChapterView.Free;
  FActualCommentsMemo.Free; // Free the dynamically created CommentsView
end;
// REMOVED: TimerAITimer - obsolete since AI processing moved to modal
// AI processing progress is now tracked by TFrmAIProcessing.PumpProcessing

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (ActiveControl is TCustomEdit) or (ActiveControl is TCustomSynEdit) or
    ((ActiveControl is TCustomComboBox) and (ActiveControl <> RadioGroupStatus)) then
    Exit;

  case Key of
    '1'..'9':
      if assigned(CurrentProject) and RadioGroupStatus.Enabled then
      begin
        if Ord(Key) - Ord('0') - 1 < RadioGroupStatus.Items.Count then
        begin
          RadioGroupStatus.ItemIndex := Ord(Key) - Ord('0') - 1;
          RadioGroupStatusSelectionChanged(RadioGroupStatus);
          Key := #0;
        end;
      end;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  x, y: Integer;
begin
  x := opts.ReadInteger('leiaute', 'principal.esquerda', Left);
  y := opts.ReadInteger('leiaute', 'principal.topo', Top);

  { Ensure that at least part of the form remains inside the visible screen area. }
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
  LayoutVersePanelLabels;

  if (ParamCount > 0) and SameText(ExtractFileExt(ParamStr(1)), '.bib') and FileExists(ParamStr(1)) then
    DoOpenProject(ParamStr(1))
  else if (MenuItemRecent.Count > 0) and FileExists(MenuItemRecent.Items[0].Caption) then
    DoOpenProject(MenuItemRecent.Items[0].Caption);
end;

procedure TMainForm.MenuItem22Click(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if assigned(CurrentProject) then
    CurrentProject.SugerirAssociacaoAutomaticamente := TMenuItem(Sender).Checked;
end;

procedure TMainForm.RecentProjectClick(Sender: TObject);
begin
  if TMenuItem(Sender).Caption <> '(-)' then
     ActionOpenProjectExecute(Sender);
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.ShowModal;
end;

procedure TMainForm.MenuItemAISuggestionClick(Sender: TObject);
begin
  FSuggestionMode := smAI;
  if Assigned(CurrentProject) then
    CurrentProject.SuggestionMode := smAI;
  MenuItemStatisticalSuggestion.Checked := False;
  MenuItemAISuggestion.Checked := True;
  UpdateStateHeader;
end;

procedure TMainForm.MenuItemAlwaysOnTopClick(Sender: TObject);
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

procedure TMainForm.MenuItemAutoSaveClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if assigned(CurrentProject) then
    CurrentProject.AutoSave := TMenuItem(Sender).Checked;
end;

procedure TMainForm.TranslationMemoryOrderClick(Sender: TObject);
begin
  if Sender = FTranslationMemoryVerseOrderItem then
    FTranslationMemoryOrder := tmoVerseOccurrence
  else if Sender = FTranslationMemoryRareFirstItem then
    FTranslationMemoryOrder := tmoMemoryFrequencyAscending
  else
    Exit;

  UpdateTranslationMemoryOrderMenu;
  if Assigned(FChapterView) then
    FChapterView.TranslationMemoryOrder := FTranslationMemoryOrder;
  if Assigned(opts) then
    opts.WriteInteger('opcoes', 'translation.memory.order',
      Ord(FTranslationMemoryOrder));
end;

procedure TMainForm.DefaultVerseModeClick(Sender: TObject);
begin
  if Sender = FDefaultVerseModeAssociationItem then
    SetDefaultVerseMode(dvmAssociation)
  else if Sender = FDefaultVerseModeEditingItem then
    SetDefaultVerseMode(dvmEditing);
end;

procedure TMainForm.SetDefaultVerseMode(AMode: TDefaultVerseMode);
begin
  FDefaultVerseMode := AMode;
  UpdateDefaultVerseModeMenu;

  if Assigned(CurrentProject) then
    CurrentProject.DefaultVerseMode := FDefaultVerseMode;
  if Assigned(opts) then
    opts.WriteInteger('opcoes', 'verse.default.mode',
      Ord(FDefaultVerseMode));
end;

procedure TMainForm.HandleTranslationMemoryPreviewFontSizeChanged(
  Sender: TObject);
begin
  if Assigned(opts) and Assigned(FChapterView) then
    WriteTranslationMemoryPreviewFontSize(opts,
      FChapterView.TranslationMemoryPreviewFontSize);
end;

procedure TMainForm.UpdateTranslationMemoryOrderMenu;
begin
  if Assigned(FTranslationMemoryVerseOrderItem) then
    FTranslationMemoryVerseOrderItem.Checked :=
      FTranslationMemoryOrder = tmoVerseOccurrence;
  if Assigned(FTranslationMemoryRareFirstItem) then
    FTranslationMemoryRareFirstItem.Checked :=
      FTranslationMemoryOrder = tmoMemoryFrequencyAscending;
end;

procedure TMainForm.UpdateDefaultVerseModeMenu;
begin
  if Assigned(FDefaultVerseModeAssociationItem) then
    FDefaultVerseModeAssociationItem.Checked :=
      FDefaultVerseMode = dvmAssociation;
  if Assigned(FDefaultVerseModeEditingItem) then
    FDefaultVerseModeEditingItem.Checked :=
      FDefaultVerseMode = dvmEditing;
end;

procedure TMainForm.MenuItemLangEnClick(Sender: TObject);
begin
  if language = 'en' then
    exit;

  language := 'en';
  SetDefaultLang('en');
  TMenuItem(Sender).Checked:=true;
  MenuItemLangPt.Checked:=false;
  Translate;
end;

procedure TMainForm.MenuItemLangPtClick(Sender: TObject);
begin
  if language = 'pt' then
    exit;

  language := 'pt';
  SetDefaultLang('pt');
  TMenuItem(Sender).Checked:=true;
  MenuItemLangEn.Checked:=false;
  Translate;
end;

procedure TMainForm.MenuItemPopupTriggerClick(Sender: TObject);
var
  m: TPopupTrigger;
begin
  for m:=low(TPopupTrigger) to high(TPopupTrigger) do
  begin
    if Sender = MenuItemDictPopup.Items[integer(m)] then
    begin
      FPopupTrigger := m;
      TMenuItem(Sender).Checked := true;
      if assigned(CurrentProject) then
        CurrentProject.PopupTrigger := FPopupTrigger;
    end else
      MenuItemDictPopup.Items[integer(m)].Checked := false;
  end;
end;

procedure TMainForm.MenuItemStatisticalSuggestionClick(Sender: TObject);
begin
  FSuggestionMode := smStatistical;
  if Assigned(CurrentProject) then
    CurrentProject.SuggestionMode := smStatistical;
  MenuItemStatisticalSuggestion.Checked := True;
  MenuItemAISuggestion.Checked := False;
  UpdateStateHeader;
end;

procedure TMainForm.MenuItemBoldStrongsClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if assigned(CurrentProject) then
    CurrentProject.PalavrasComStrongEmNegrito := TMenuItem(Sender).Checked;
end;

procedure TMainForm.MenuItemStrongsCountClick(Sender: TObject);
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
      if assigned(CurrentProject) then
        CurrentProject.MostrarQtdStrongs := TStrongsCountMode(m.Tag);
    end else
      m.Checked := false;
  end;
end;

procedure TMainForm.MenuItemSynciBibliaClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  FSyncIBibliaToTheWord := TMenuItem(Sender).Checked;
  SetUpSyncThread;
  {$ENDIF}
end;

procedure TMainForm.MenuItemSyncTheWordClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  FSyncTheWordToIBiblia := TMenuItem(Sender).Checked;
  if FSyncTheWordToIBiblia then
    ActionSyncTheWordVerseExecute(Sender);
  {$ENDIF}
end;

procedure TMainForm.ProjectNewVerse(Sender: TProject);
begin
  ActionNewVerseExecute(nil);
end;

procedure TMainForm.UpdateNavigationHistoryActions;
begin
  ActionHistoryBack.Enabled := Assigned(CurrentProject) and CurrentProject.CanGoBack;
  ActionHistoryForward.Enabled := Assigned(CurrentProject) and CurrentProject.CanGoForward;
end;

procedure TMainForm.SyntagmClick(Sender: TSyntagm);
{$IFDEF WINDOWS}
var
  match: IMatch;
  wordSyncd: boolean;
{$ENDIF}
begin
  UpdateStateHeader;
  FChapterView.FocusTranslationMemoryForSyntagm(Sender);
  {$IFDEF WINDOWS}
  if not FSyncTheWordToIBiblia then
    exit;

  if Sender.HasStrongs then
    SyncTheWordDict(Sender.Strong[0]); // strong#

  wordSyncd := false;
  if Sender.HasMorph then
  begin
    match := FRxMorpho.Match(Sender.Morph[0]);
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
    SyncTheWordDict(Sender.Text); // raw word
  {$ENDIF}
end;

procedure TMainForm.VerseChanged;
begin
  if Assigned(CurrentProject) and CurrentProject.Modificado then
    StatusBar1.SimpleText := SChanged
  else
    StatusBar1.SimpleText := '';
  UpdateStateHeader;
end;

procedure TMainForm.UpdateRecentProjects(m: TMenuItem);
var
  f: TStringList;
  j, k: Integer;
  found: Boolean;
begin
  f := TStringList.Create;

  try
    f.Add(CurrentProject.Caminho);

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
      m.Items[m.Count-1].OnClick := @RecentProjectClick;
    end;
  finally
    f.Free;
  end;
end;

procedure TMainForm.LoadRecentProjects(m: TMenuItem);
var
  j: Integer;
begin
  for j:=0 to MAX_MRU-1 do
  begin
    if opts.ReadString('projetos', format('recente.%d', [j]), '') <> '' then
    begin
      m.Add(TMenuItem.Create(m));
      m.Items[m.Count-1].Caption := opts.ReadString('projetos', format('recente.%d', [j]), '');
      m.Items[m.Count-1].OnClick := @RecentProjectClick;
    end;
  end;
end;

procedure TMainForm.SaveRecentProjects(m: TMenuItem);
var
  j: Integer;
begin
  for j:=0 to m.Count-1 do
    opts.WriteString('projetos', format('recente.%d', [j]), m.Items[j].Caption);

  for j:=m.Count to MAX_MRU-1 do
    opts.DeleteKey('projetos', format('recente.%d', [j]));
end;

procedure TMainForm.RadioGroupStatusDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Combo: TComboBox;
  Definitions: TStatusDefinitions;
  StatusId: Integer;
  SwatchRect: TRect;
  TextRect: TRect;
begin
  if not (Control is TComboBox) then
    Exit;

  Combo := TComboBox(Control);
  Combo.Canvas.FillRect(ARect);
  if (Index < 0) or (Index >= Combo.Items.Count) then
    Exit;

  if Assigned(CurrentProject) then
    Definitions := CurrentProject.GetStatusDefinitions
  else
    Definitions := BuildLegacyStatusDefinitions([
      STreeLegendNotAssociated,
      STreeLegendAssociating,
      STreeLegendNeedsReview,
      STreeLegendAssociated
    ]);

  StatusId := StatusIdAtDisplayIndex(Definitions, Index);
  SwatchRect := Rect(ARect.Left + 4, ARect.Top + 4, ARect.Left + 18,
    ARect.Bottom - 4);
  Combo.Canvas.Brush.Color := GetStatusColor(Definitions, StatusId);
  Combo.Canvas.Pen.Color := clBlack;
  Combo.Canvas.Rectangle(SwatchRect);

  TextRect := ARect;
  TextRect.Left := ARect.Left + 24;
  Combo.Canvas.Brush.Style := bsClear;
  Combo.Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top + 3,
    Combo.Items[Index]);
  Combo.Canvas.Brush.Style := bsSolid;
end;

procedure TMainForm.RadioGroupStatusSelectionChanged(Sender: TObject);
var
  StatusId: Integer;
begin
  if CurrentProject <> nil then
  begin
    StatusId := StatusIdAtDisplayIndex(CurrentProject.GetStatusDefinitions,
      RadioGroupStatus.ItemIndex);
    CurrentProject.Status := StatusId;
    UpdateStateHeader;
    UpdateStatusReviewPanel;
  end;
end;

procedure TMainForm.StatusReviewListDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  List: TListBox;
  Definitions: TStatusDefinitions;
  StatusId: Integer;
  SwatchRect: TRect;
  TextRect: TRect;
  SwatchColor: TColor;
  SwatchSize: Integer;
  CenterY: Integer;

  function CheckMarkColor(AColor: TColor): TColor;
  var
    RGB: LongInt;
    R, G, B: Integer;
  begin
    RGB := ColorToRGB(AColor);
    R := RGB and $FF;
    G := (RGB shr 8) and $FF;
    B := (RGB shr 16) and $FF;
    if ((R * 299 + G * 587 + B * 114) div 1000) < 128 then
      Result := clWhite
    else
      Result := clBlack;
  end;

begin
  if not (Control is TListBox) then
    Exit;

  List := TListBox(Control);
  if odSelected in State then
  begin
    List.Canvas.Brush.Color := clHighlight;
    List.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    List.Canvas.Brush.Color := List.Color;
    if List.Enabled then
      List.Canvas.Font.Color := List.Font.Color
    else
      List.Canvas.Font.Color := clGrayText;
  end;
  List.Canvas.FillRect(ARect);

  if (Index < 0) or (Index >= List.Items.Count) then
    Exit;

  if Assigned(CurrentProject) then
    Definitions := CurrentProject.GetStatusDefinitions
  else
    Definitions := BuildLegacyStatusDefinitions([
      STreeLegendNotAssociated,
      STreeLegendAssociating,
      STreeLegendNeedsReview,
      STreeLegendAssociated
    ]);

  StatusId := Integer(PtrUInt(List.Items.Objects[Index]));
  CenterY := ARect.Top + (ARect.Bottom - ARect.Top) div 2;
  SwatchSize := Max(11, Min(13, ARect.Bottom - ARect.Top - 5));
  SwatchRect := Rect(ARect.Left + 4, CenterY - SwatchSize div 2,
    ARect.Left + 4 + SwatchSize, CenterY + (SwatchSize + 1) div 2);

  SwatchColor := GetStatusColor(Definitions, StatusId);
  List.Canvas.Brush.Color := SwatchColor;
  List.Canvas.Pen.Color := clBlack;
  List.Canvas.Rectangle(SwatchRect);
  if StatusReviewItemChecked(Index) then
  begin
    List.Canvas.Pen.Color := CheckMarkColor(SwatchColor);
    List.Canvas.Pen.Width := 2;
    List.Canvas.MoveTo(SwatchRect.Left + 3, CenterY);
    List.Canvas.LineTo(SwatchRect.Left + SwatchSize div 2 - 1,
      SwatchRect.Bottom - 3);
    List.Canvas.LineTo(SwatchRect.Right - 3, SwatchRect.Top + 3);
    List.Canvas.Pen.Width := 1;
  end;

  TextRect := ARect;
  TextRect.Left := SwatchRect.Right + 6;
  if odSelected in State then
    List.Canvas.Brush.Color := clHighlight
  else
    List.Canvas.Brush.Color := List.Color;
  List.Canvas.TextRect(TextRect, TextRect.Left,
    TextRect.Top + Max(0, (TextRect.Bottom - TextRect.Top -
    List.Canvas.TextHeight(List.Items[Index])) div 2), List.Items[Index]);

  if odFocused in State then
    List.Canvas.DrawFocusRect(ARect);
end;

procedure TMainForm.ChapterViewTabCtrlChange(Sender: TObject);
var
  TabIndex: Integer;
begin
  if not Assigned(FChapterView) then
    Exit;
  TabIndex := TTabControl(Sender).TabIndex;
  if TabIndex = CHAPTER_VIEW_MEMORY_TAB_INDEX then
    FChapterView.ViewMode := cvmTranslationMemory
  else if (TabIndex >= Ord(tbOrigem)) and
          (TabIndex <= Ord(tbInterlinear)) then
    FChapterView.TextType := TTipoTextoBiblico(TabIndex);
end;

procedure TMainForm.ToolButtonExitClick(Sender: TObject);
begin
  Close;
end;

{$IFDEF WINDOWS}
procedure TMainForm.SyncToTwRef(Ref: string);
begin
  if (CurrentProject <> nil) then
    CurrentProject.NavigateToReference(Ref);
end;

procedure TMainForm.SetUpSyncThread;
begin
  if FSyncIBibliaToTheWord then
  begin
    if FTheWordSyncThread = nil then
    begin
      FTheWordSyncThread := TTwSyncThread.Create(true);
      FTheWordSyncThread.OnRefChange := @SyncToTwRef;
      FTheWordSyncThread.Start;
    end;
  end else
  begin
    if FTheWordSyncThread <> nil then
    begin
      FTheWordSyncThread.Terminate;
      FTheWordSyncThread.WaitFor;      // Wait for the thread to finish
      FTheWordSyncThread.Free;         // Free it explicitly
      FTheWordSyncThread := nil;
    end;
  end;
end;
{$ENDIF}

procedure TMainForm.Translate;
var
  SelectedChapterViewTab: Integer;
begin
  SelectedChapterViewTab := ChapterViewTabCtrl.TabIndex;
  if Assigned(ActionKeyboardShortcuts) then
  begin
    ActionKeyboardShortcuts.Caption := SKeyboardShortcutsMenu;
    ActionKeyboardShortcuts.Hint := SKeyboardShortcutsHint;
  end;
  if Assigned(ActionLlmOptions) then
    ActionLlmOptions.Caption := SLLMOptionsMenu;
  if Assigned(ActionGuideMode) then
  begin
    ActionGuideMode.Caption := SGuideModeMenu;
    ActionGuideMode.Hint := SGuideModeHint;
  end;
  if Assigned(ActionResetLayout) then
  begin
    ActionResetLayout.Caption := SResetLayoutMenu;
    ActionResetLayout.Hint := SResetLayoutHint;
  end;
  if Assigned(ActionTheWordHighlighterColors) then
    ActionTheWordHighlighterColors.Caption := STheWordHighlightColorsMenu;
  if Assigned(FTranslationMemoryOrderMenu) then
    FTranslationMemoryOrderMenu.Caption := STranslationMemoryOrderMenu;
  if Assigned(FTranslationMemoryVerseOrderItem) then
    FTranslationMemoryVerseOrderItem.Caption :=
      STranslationMemoryVerseOrder;
  if Assigned(FTranslationMemoryRareFirstItem) then
    FTranslationMemoryRareFirstItem.Caption :=
      STranslationMemoryRareFirst;
  UpdateTranslationMemoryOrderMenu;
  if Assigned(FDefaultVerseModeMenu) then
  begin
    FDefaultVerseModeMenu.Caption := SDefaultVerseModeMenu;
    FDefaultVerseModeMenu.Hint := SDefaultVerseModeHint;
  end;
  if Assigned(FDefaultVerseModeAssociationItem) then
  begin
    FDefaultVerseModeAssociationItem.Caption :=
      SDefaultVerseModeAssociation;
    FDefaultVerseModeAssociationItem.Hint :=
      SDefaultVerseModeAssociationHint;
  end;
  if Assigned(FDefaultVerseModeEditingItem) then
  begin
    FDefaultVerseModeEditingItem.Caption :=
      SDefaultVerseModeEditing;
    FDefaultVerseModeEditingItem.Hint :=
      SDefaultVerseModeEditingHint;
  end;
  UpdateDefaultVerseModeMenu;
  if Assigned(ActionRegisterFileAssociation) then
    ActionRegisterFileAssociation.Caption := SRegisterFileAssociation;
  if Assigned(ActionUnregisterFileAssociation) then
    ActionUnregisterFileAssociation.Caption := SUnregisterFileAssociation;
  ActionHistoryBack.Caption := SHistoryBack;
  ActionHistoryBack.Hint := SHistoryBackHint;
  ActionHistoryForward.Caption := SHistoryForward;
  ActionHistoryForward.Hint := SHistoryForwardHint;
  if Assigned(FGuideHideButton) then
    FGuideHideButton.Caption := SHideGuideButton;

  ActionRollbackCurrentVerse.Caption := SRollbackCurrentVerse;
  ActionRollbackCurrentVerse.Hint := SRollbackCurrentVerseHint;
  ActionClearAssociations.Caption := SClearCurrentVerseAssociations;
  ActionClearAssociations.Hint := SClearCurrentVerseAssociationsHint;
  if Assigned(FCommentsGroupBox) then
  begin
    FCommentsGroupBox.Caption := SPanelLabelComments;
    FCommentsGroupBox.Hint := SPanelLabelComments;
  end;
  NormalizeActionHints;

  UpdateDefaultStatusDescription;
  if Assigned(CurrentProject) then
    CurrentProject.RefreshStatusControl
  else
    RadioGroupStatus.Items.Clear;
  with ChapterViewTabCtrl do
  begin
    Tabs.Clear;
    Tabs.Add(SSourceText);
    Tabs.Add(SDestinationText);
    Tabs.Add(SReference1Text);
    Tabs.Add(SReference2Text);
    Tabs.Add(SInterlinearText);
    Tabs.Add(SMemoryText);
    if (SelectedChapterViewTab >= 0) and
       (SelectedChapterViewTab < Tabs.Count) then
      TabIndex := SelectedChapterViewTab;
  end;
  if assigned(FrmNewProject) then
    FrmNewProject.Translate;
  if assigned(FrmProjectProperties) then
    FrmProjectProperties.Translate;
  if assigned(CurrentProject) then
    CurrentProject.Translate;
  if assigned(FChapterView) then
    FChapterView.Translate;
  UpdateMainCaption;
  UpdateStateHeader;
  UpdateGuidePanel;
  UpdateStatusReviewPanel;
  LayoutVersePanelLabels;
end;

procedure TMainForm.DoOpenProject(filename: string);
begin
  CurrentProject := TProject.Create([ScrollBoxSrcVerse, ScrollBoxDstVerse, ScrollBoxRef1Verse, ScrollBoxRef2Verse], TreeView1, RadioGroupStatus, FActualCommentsMemo);
  CurrentProject.OnNewVerseSubscribe(@ProjectNewVerse);
  CurrentProject.OnAlterarVersiculo := @VerseChanged;
  CurrentProject.OnSintagmaClick := @SyntagmClick;
  CurrentProject.PalavrasComStrongEmNegrito := MenuItemBoldStrongs.Checked;
  CurrentProject.MostrarQtdStrongs := FStrongsCountMode;
  CurrentProject.AutoSave := MenuItemAutoSave.Checked;
  CurrentProject.SuggestionMode := FSuggestionMode;
  CurrentProject.DefaultVerseMode := FDefaultVerseMode;

  CurrentProject.OpenProject(filename);
  CurrentProject.ExibirDefinicoesSoComCtrl := MenuItemDictPopup.Checked;
  CurrentProject.SugerirAssociacaoAutomaticamente := MenuItem22.Checked;
  CurrentProject.PopupTrigger := FPopupTrigger;

  FChapterView.Project := CurrentProject;
  FChapterView.Enabled := true;
  if CurrentProject.ChapterViewMode = cvmTranslationMemory then
    ChapterViewTabCtrl.TabIndex := CHAPTER_VIEW_MEMORY_TAB_INDEX
  else
    ChapterViewTabCtrl.TabIndex := Ord(CurrentProject.ChapterViewText);
  ChapterViewTabCtrlChange(ChapterViewTabCtrl);

  ActionSaveProject.Enabled := true;
  ActionSaveProjectAs.Enabled := true;
  ActionCloseProject.Enabled := true;
  ActionProjectProperties.Enabled := true;
  ActionFirstVerse.Enabled := true;
  ActionLastVerse.Enabled := true;
  ActionNextVerse.Enabled := true;
  ActionPreviousVerse.Enabled := true;
  ActionSuggestAssociation.Enabled := true;
  ActionRollbackCurrentVerse.Enabled := true;
  ActionClearAssociations.Enabled := true;
  ActionExportProject.Enabled := true;
  ActionRebuildSuggestionDatabase.Enabled := true;
  ActionExportPatch.Enabled := true;
  ActionApplyPatch.Enabled := true;
  if Assigned(ActionLlmOptions) then ActionLlmOptions.Enabled := true;
  //ActionExportDestinationWithStrongs.Enabled := true;
  //ActionExportInterlinearText.Enabled := true;
  StatusBar1.SimpleText := '';
  UpdateNavigationHistoryActions;
  UpdateStateHeader;
  UpdateStatusReviewPanel;
  LayoutVersePanelLabels;

  {ScrollBoxRef1Verse.Visible := false;
  Splitter3.Visible := false;
  ScrollBoxRef2Verse.Visible := false;
  Splitter4.Visible := false;}

  {$IFDEF WINDOWS}
  SetUpSyncThread;
  {$ENDIF}
  CurrentProject.RequestDefaultVerseModeForCurrentVerse;
end;

procedure TMainForm.ActionRegisterFileAssociationExecute(Sender: TObject);
{$IFDEF WINDOWS}
var
  ExePath: string;
  IsAssociated: Boolean;
  AssociatedApp: string;
  IsOurs: Boolean;
  UserChoice: Integer;
begin
  ExePath := ParamStr(0);

  IsAssociated := IsFileAssociated('.bib');
  AssociatedApp := GetAssociatedApp('.bib');
  IsOurs := SameExecutablePath(AssociatedApp, ExePath);

  if IsAssociated and (not AssociatedApp.IsEmpty) and (not IsOurs) then
  begin
    UserChoice := MessageDlg(SRegisterFileAssociation, 
      Format(SRegisterFileAssociationTakeOverPrompt, [AssociatedApp]),
      mtConfirmation, [mbYes, mbNo], 0);
    if UserChoice <> mrYes then
      Exit;
  end;

  if ShellExecute(0, 'runas', PChar(ExePath), PChar('--register-file-assoc'), nil, SW_SHOWNORMAL) <= 32 then
    ShowMessage(SRegisterFileAssociationFailure);
end;
{$ELSE}
begin
  // Nothing to do on non-Windows platforms
end;
{$ENDIF}

procedure TMainForm.ActionUnregisterFileAssociationExecute(Sender: TObject);
{$IFDEF WINDOWS}
var
  ExePath: string;
begin
  ExePath := ParamStr(0);
  if ShellExecute(0, 'runas', PChar(ExePath), PChar('--unregister-file-assoc'), nil, SW_SHOWNORMAL) <= 32 then
    ShowMessage(SUnregisterFileAssociationFailure);
end;
{$ELSE}
begin
  // Nothing to do on non-Windows platforms
end;
{$ENDIF}

procedure TMainForm.UpdateFileAssociationMenuItems;
{$IFDEF WINDOWS}
var
  AssociatedApp: string;
  IsOurs: Boolean;
begin
  AssociatedApp := GetAssociatedApp('.bib');
  IsOurs := SameExecutablePath(AssociatedApp, ParamStr(0));

  if Assigned(ActionRegisterFileAssociation) then
    ActionRegisterFileAssociation.Enabled := not IsOurs
  else
    MenuItemRegisterFileAssoc.Enabled := not IsOurs;

  if Assigned(ActionUnregisterFileAssociation) then
    ActionUnregisterFileAssociation.Enabled := IsOurs
  else
    MenuItemUnregisterFileAssoc.Enabled := IsOurs;
end;
{$ELSE}
begin
  MenuItemRegisterFileAssoc.Visible := False;
  MenuItemUnregisterFileAssoc.Visible := False;
  MenuItemFileAssocSeparator.Visible := False;
end;
{$ENDIF}

procedure TMainForm.CheckFileAssociation;
{$IFDEF WINDOWS}
begin
  UpdateFileAssociationMenuItems;
end;
{$ELSE}
begin
  // Nothing to do on non-Windows platforms
end;
{$ENDIF}

end.

