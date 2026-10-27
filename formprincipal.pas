unit formPrincipal;

{$mode objfpc}{$H+}

{ TODO: Determine whether syntagm association order matters. }

interface

uses
  {$IFDEF WINDOWS}
  windows, twAutomate, TwSyncThread, fileassociation, ShellApi,
  {$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, LCLIntf,
  ActnList, ComCtrls, ExtCtrls, StdCtrls, BibleScope, Project, IniFiles, Math,
  Syntagm, LCLTranslator, unitabout, PCRE, Verse, LCLProc, LazLogger,
  LCLType, LazUTF8, ChapterView, UnitPleaseWait, CommentsView, ONTTokenizer,
  status_catalog, SynEdit, project_setup_model, translation_memory_model,
  project_creation_staging, FormApplicationPreferences, theword_sync_state,
  ui_icons, ui_style;

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
    MenuItemSaveProjectAs: TMenuItem;
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
    ToolButtonProjectSeparator: TToolButton;
    ToolButtonAssociationSeparator: TToolButton;
    ToolButtonSyncSeparator: TToolButton;
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
    procedure ActionSaveProjectAsUpdate(Sender: TObject);
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
    procedure UpdateRecentProjects(m: TMenuItem;
      const PreviousFileName: string = '');
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
    FAutoSaveEnabled: Boolean;
    FBoldStrongsEnabled: Boolean;
    FAlwaysOnTopEnabled: Boolean;
    FLegacyAutomaticSuggestion: Boolean;
    FCurrentRef: string;
    FChapterView: TChapterView;
    FChapterViewHostPanel: TPanel;
    FMemoryOrderPanel: TPanel;
    FMemoryOrderLabel: TLabel;
    FMemoryOrderCombo: TComboBox;
    FTranslationMemoryOrder: TTranslationMemoryOrder;
    FTranslationMemoryOrderMenu: TMenuItem;
    FTranslationMemoryVerseOrderItem: TMenuItem;
    FTranslationMemoryRareFirstItem: TMenuItem;
    FDefaultVerseMode: TDefaultVerseMode;
    FDefaultVerseModeMenu: TMenuItem;
    FDefaultVerseModeAssociationItem: TMenuItem;
    FDefaultVerseModeEditingItem: TMenuItem;
    FActionPreferences: TAction;
    FToolsMenu: TMenuItem;
    FVerseInformationMenu: TMenuItem;
    FTheWordSyncMenu: TMenuItem;
    FDisableTheWordSyncItem: TMenuItem;
    FViewMenuSeparator: TMenuItem;
    FToolsMenuSeparator: TMenuItem;
    FToolsOuterSeparator: TMenuItem;
    FHelpMenuSeparator: TMenuItem;
    FPreferencesMenuItem: TMenuItem;
    FSuggestionPopupMenu: TPopupMenu;
    FSuggestionStatisticalItem: TMenuItem;
    FSuggestionAIItem: TMenuItem;
    FSuggestionWorkflowItem: TMenuItem;
    FSuggestionAIConfigItem: TMenuItem;
    FTheWordPopupMenu: TPopupMenu;
    FTheWordToolButton: TToolButton;
    FMenuImages: TImageList;
    FLayoutStateHeaderBusy: Boolean;
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
    procedure StopTheWordSyncThread;
    {$ENDIF}
    procedure NormalizeActionHints;
    procedure ApplicationPreferencesChanged(
      const Preferences: TApplicationPreferences);
    procedure ActionPreferencesExecute(Sender: TObject);
    procedure BuildCommandUI;
    procedure InitializeVisualIdentity;
    procedure BuildSuggestionToolbarMenu;
    procedure BuildTheWordToolbarMenu;
    procedure CreateMemoryOrderControls;
    procedure DisableTheWordSyncClick(Sender: TObject);
    procedure OpenWorkflowSettingsExecute(Sender: TObject);
    procedure PersistGlobalPreference(const Key: string;
      const Value: Boolean); overload;
    procedure PersistGlobalPreference(const Key, Value: string); overload;
    procedure PersistGlobalPreference(const Key: string;
      Value: Integer); overload;
    procedure SetAlwaysOnTopEnabled(AEnabled: Boolean);
    procedure SetAutoSaveEnabled(AEnabled: Boolean);
    procedure SetBoldStrongsEnabled(AEnabled: Boolean);
    procedure SetPopupTrigger(APopupTrigger: TPopupTrigger);
    procedure SetStrongsCountMode(AMode: TStrongsCountMode);
    procedure SetSuggestionMode(AMode: TSuggestionMode);
    procedure SetTheWordSyncDirections(AIBibliaToTheWord,
      ATheWordToIBiblia: Boolean; PushCurrentReference: Boolean = False);
    procedure TheWordToolbarButtonClick(Sender: TObject);
    procedure TranslationMemoryOrderComboChange(Sender: TObject);
    procedure UpdateCommandUI;
    procedure UpdateMemoryOrderControls;
    procedure UpdateTheWordSyncUI;
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
  protected
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
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
  STranslationMemoryOrderMenu = '&Sort by:';
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
  SSuggestionMode = 'Suggestion method';
  SStatisticalSuggestion = 'Statistical model';
  SAISuggestion = 'AI-assisted (experimental)';
  SStatisticalSuggestionHint = 'Use the traditional statistical suggestion engine';
  SAISuggestionHint = 'Use the new AI-assisted suggestion engine (requires internet)';
  SLLMOptionsMenu = 'AI &configuration...';
  SGuideModeMenu = '&Contextual guide';
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
  SGuideTranslationText = 'Double-click an empty area in Destination to write the translation. Use Save to apply or Cancel to discard; Esc also discards and closes. Once Destination has text, the guide will switch to association.';
  SKeyboardShortcutsMenu = '&Keyboard shortcuts...';
  SKeyboardShortcutsHint = 'Show keyboard shortcuts';
  SKeyboardShortcutsTitle = 'Keyboard shortcuts';
  SShortcutProjectSection = 'Project';
  SShortcutNavigationSection = 'Navigation';
  SShortcutEditingSection = 'Editing and review';
  SShortcutHelpSection = 'Help';
  SShortcutNewProject = 'New project';
  SShortcutOpenProject = 'Open project';
  SShortcutSaveProject = 'Save project';
  SShortcutSaveProjectAs = 'Save the current project to another file';
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
  SHeaderEngineFmt = 'Method: %s';
  SHeaderEngineStatistical = 'Statistical';
  SHeaderEngineAI = 'AI';
  SHeaderEngineAutomatic = 'automatic';
  SHeaderEngineAIConfigurationPending = 'configuration pending';
  SHeaderSaved = 'Saved';
  SHeaderUnsavedChanges = 'Unsaved changes';
  SResetLayoutMenu = '&Restore panel arrangement';
  SResetLayoutHint = 'Restore the default panel layout';
  SProjectMenu = '&Project';
  SEditMenu = '&Edit';
  SNavigationMenu = '&Navigation';
  SHelpMenu = '&Help';
  SNewProjectMenu = '&New...';
  SOpenProjectMenu = '&Open...';
  SOpenRecentMenu = 'Open &recent';
  SSaveProjectMenu = '&Save';
  SSaveProjectAsMenu = 'Save &as...';
  SCloseProjectMenu = '&Close';
  SProjectSettingsMenu = 'Project &settings...';
  SExportProjectMenu = '&Export project...';
  SCreatePatchMenu = 'Create &patch...';
  SApplyPatchMenu = '&Apply patch...';
  SMaintenanceMenu = '&Maintenance';
  SCompactProjectMenu = '&Compact project database';
  SRebuildAssociationMemoryMenu = '&Recreate association memory';
  SExitMenu = 'E&xit';
  SPreviousVerseMenu = '&Previous verse';
  SNextVerseMenu = '&Next verse';
  SFirstVerseMenu = '&First verse';
  SLastVerseMenu = '&Last verse';
  SAboutMenu = '&About...';
  SViewMenu = '&View';
  SVerseInformationMenu = 'Verse &information';
  SStrongsCountMenu = 'Strong tag count';
  SStrongsCountNone = 'Hide';
  SStrongsCountWords = 'Count words';
  SStrongsCountTags = 'Count tags';
  SBoldStrongsMenu = 'Highlight words with Strong in bold';
  SAlwaysOnTopMenu = '&Keep window always on top';
  SToolsMenu = '&Tools';
  STheWordSynchronizationMenu = '&Synchronization with theWord';
  SSyncIBibliaToTheWord = 'iBiblia → theWord';
  SSyncTheWordToIBiblia = 'theWord → iBiblia';
  SDisableBothSyncDirections = 'Disable both';
  STheWordEditorAppearance = 'theWord editor appearance...';
  SPreferencesMenu = 'iBiblia &preferences...';
  STheWordStateOff = 'theWord synchronization: off';
  STheWordStateOutbound = 'theWord synchronization: iBiblia → theWord';
  STheWordStateInbound = 'theWord synchronization: theWord → iBiblia';
  STheWordStateBidirectional = 'theWord synchronization: bidirectional';
  SSuggestionToolbarStatistical = 'Statistical';
  SSuggestionToolbarAI = 'AI';
  SSuggestionToolbarWorkflow = 'Configure &workflow...';
  SSuggestAssociationCaption = 'Suggest association';
  SSuggestAssociationHint = 'Suggest associations for the current verse';
  SAIConfigurationPendingTitle = 'AI configuration pending';
  SAIConfigurationPendingText = 'Configure AI for this project before using AI suggestions.';
  SConfigureAIButton = 'Configure AI...';
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

type
  TOverflowLabel = class(TLabel)
  protected
    procedure Paint; override;
  end;

procedure TOverflowLabel.Paint;
var
  TextStyle: TTextStyle;
begin
  Canvas.Font.Assign(Font);
  if not Enabled then Canvas.Font.Color := clGrayText;
  if not Transparent then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
  end;
  Canvas.Brush.Style := bsClear;
  TextStyle := Canvas.TextStyle;
  TextStyle.Alignment := Alignment;
  TextStyle.Layout := Layout;
  TextStyle.SingleLine := True;
  TextStyle.Clipping := True;
  TextStyle.ShowPrefix := False;
  TextStyle.EndEllipsis := True;
  Canvas.TextRect(ClientRect, 0, 0, Caption, TextStyle);
end;

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
  NormalizeActionHint(ActionSaveProjectAs);
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

procedure TMainForm.PersistGlobalPreference(const Key: string;
  const Value: Boolean);
begin
  if Assigned(opts) then
  begin
    opts.WriteBool('opcoes', Key, Value);
    opts.UpdateFile;
  end;
end;

procedure TMainForm.PersistGlobalPreference(const Key, Value: string);
begin
  if Assigned(opts) then
  begin
    opts.WriteString('opcoes', Key, Value);
    opts.UpdateFile;
  end;
end;

procedure TMainForm.PersistGlobalPreference(const Key: string;
  Value: Integer);
begin
  if Assigned(opts) then
  begin
    opts.WriteInteger('opcoes', Key, Value);
    opts.UpdateFile;
  end;
end;

procedure TMainForm.SetAlwaysOnTopEnabled(AEnabled: Boolean);
begin
  FAlwaysOnTopEnabled := AEnabled;
  MenuItemAlwaysOnTop.Checked := FAlwaysOnTopEnabled;
  if FAlwaysOnTopEnabled then
    FormStyle := fsSystemStayOnTop
  else
    FormStyle := fsNormal;
  PersistGlobalPreference('alwaysontop', FAlwaysOnTopEnabled);
end;

procedure TMainForm.SetAutoSaveEnabled(AEnabled: Boolean);
begin
  FAutoSaveEnabled := AEnabled;
  MenuItemAutoSave.Checked := FAutoSaveEnabled;
  if Assigned(CurrentProject) then
    CurrentProject.AutoSave := FAutoSaveEnabled;
  PersistGlobalPreference('auto.save', FAutoSaveEnabled);
end;

procedure TMainForm.SetBoldStrongsEnabled(AEnabled: Boolean);
begin
  FBoldStrongsEnabled := AEnabled;
  MenuItemBoldStrongs.Checked := FBoldStrongsEnabled;
  if Assigned(CurrentProject) then
    CurrentProject.PalavrasComStrongEmNegrito := FBoldStrongsEnabled;
  PersistGlobalPreference('boldstrongs', FBoldStrongsEnabled);
end;

procedure TMainForm.SetPopupTrigger(APopupTrigger: TPopupTrigger);
var
  Index: Integer;
begin
  FPopupTrigger := APopupTrigger;
  for Index := 0 to MenuItemDictPopup.Count - 1 do
    MenuItemDictPopup.Items[Index].Checked :=
      Index = Ord(FPopupTrigger);
  if Assigned(CurrentProject) then
    CurrentProject.PopupTrigger := FPopupTrigger;
  PersistGlobalPreference('popuptrigger', Ord(FPopupTrigger));
end;

procedure TMainForm.SetStrongsCountMode(AMode: TStrongsCountMode);
var
  Index: Integer;
begin
  FStrongsCountMode := AMode;
  for Index := 0 to MenuItemStrongsCount.Count - 1 do
    MenuItemStrongsCount.Items[Index].Checked :=
      MenuItemStrongsCount.Items[Index].Tag = Ord(FStrongsCountMode);
  if Assigned(CurrentProject) then
    CurrentProject.MostrarQtdStrongs := FStrongsCountMode;
  PersistGlobalPreference('strongscountmode', Ord(FStrongsCountMode));
end;

procedure TMainForm.SetSuggestionMode(AMode: TSuggestionMode);
begin
  if Assigned(CurrentProject) then
    CurrentProject.SuggestionMode := AMode;
  UpdateCommandUI;
  UpdateStateHeader;
end;

procedure TMainForm.SetTheWordSyncDirections(AIBibliaToTheWord,
  ATheWordToIBiblia: Boolean; PushCurrentReference: Boolean);
begin
  {$IFDEF WINDOWS}
  FSyncTheWordToIBiblia := AIBibliaToTheWord;
  FSyncIBibliaToTheWord := ATheWordToIBiblia;
  if Assigned(opts) then
  begin
    WriteTheWordSyncState(opts, ResolveTheWordSyncState(
      FSyncTheWordToIBiblia, FSyncIBibliaToTheWord));
    opts.UpdateFile;
  end;
  SetUpSyncThread;
  if PushCurrentReference and FSyncTheWordToIBiblia and
     Assigned(CurrentProject) then
    ActionSyncTheWordVerseExecute(Self);
  {$ENDIF}
  UpdateTheWordSyncUI;
end;

procedure TMainForm.ApplicationPreferencesChanged(
  const Preferences: TApplicationPreferences);
begin
  if not SameText(language, Preferences.Language) then
  begin
    language := Preferences.Language;
    PersistGlobalPreference('language', language);
    SetDefaultLang(language);
    Translate;
  end;
  SetPopupTrigger(Preferences.PopupTrigger);
  SetAutoSaveEnabled(Preferences.AutoSave);
  if GetAppIconFamily <> Preferences.IconFamily then
  begin
    SetAppIconFamily(Preferences.IconFamily);
    PersistGlobalPreference('ui.icon.family',
      AppIconFamilyId(Preferences.IconFamily));
  end;
  UpdateCommandUI;
end;

procedure TMainForm.ActionPreferencesExecute(Sender: TObject);
var
  Preferences: TApplicationPreferences;
begin
  Preferences.Language := language;
  Preferences.PopupTrigger := FPopupTrigger;
  Preferences.AutoSave := FAutoSaveEnabled;
  Preferences.IconFamily := GetAppIconFamily;
  TFrmApplicationPreferences.Execute(Self, Preferences,
    @ApplicationPreferencesChanged);
end;

procedure TMainForm.DisableTheWordSyncClick(Sender: TObject);
begin
  SetTheWordSyncDirections(False, False);
end;

procedure TMainForm.TheWordToolbarButtonClick(Sender: TObject);
var
  PopupPoint: TPoint;
begin
  if not Assigned(FTheWordPopupMenu) or
     not Assigned(FTheWordToolButton) then
    Exit;
  PopupPoint := FTheWordToolButton.ClientToScreen(
    Point(0, FTheWordToolButton.Height));
  FTheWordPopupMenu.PopUp(PopupPoint.X, PopupPoint.Y);
end;

procedure TMainForm.OpenWorkflowSettingsExecute(Sender: TObject);
begin
  if not Assigned(CurrentProject) then
    Exit;
  FrmProjectProperties.SetProject(CurrentProject);
  FrmProjectProperties.SelectWorkflowTab;
  if FrmProjectProperties.ShowModal = mrOK then
  begin
    FrmProjectProperties.ApplyChanges(Self);
    ActionNewVerse.Execute;
    UpdateStatusReviewPanel;
    LayoutVersePanelLabels;
  end;
  UpdateCommandUI;
  UpdateStateHeader;
end;

procedure TMainForm.BuildSuggestionToolbarMenu;
var
  SeparatorItem: TMenuItem;
begin
  FSuggestionPopupMenu := TPopupMenu.Create(Self);

  FSuggestionStatisticalItem := TMenuItem.Create(FSuggestionPopupMenu);
  FSuggestionStatisticalItem.RadioItem := True;
  FSuggestionStatisticalItem.GroupIndex := 1;
  FSuggestionStatisticalItem.AutoCheck := False;
  FSuggestionStatisticalItem.OnClick := @MenuItemStatisticalSuggestionClick;
  FSuggestionPopupMenu.Items.Add(FSuggestionStatisticalItem);

  FSuggestionAIItem := TMenuItem.Create(FSuggestionPopupMenu);
  FSuggestionAIItem.RadioItem := True;
  FSuggestionAIItem.GroupIndex := 1;
  FSuggestionAIItem.AutoCheck := False;
  FSuggestionAIItem.OnClick := @MenuItemAISuggestionClick;
  FSuggestionPopupMenu.Items.Add(FSuggestionAIItem);

  SeparatorItem := TMenuItem.Create(FSuggestionPopupMenu);
  SeparatorItem.Caption := '-';
  FSuggestionPopupMenu.Items.Add(SeparatorItem);

  FSuggestionWorkflowItem := TMenuItem.Create(FSuggestionPopupMenu);
  FSuggestionWorkflowItem.OnClick := @OpenWorkflowSettingsExecute;
  FSuggestionPopupMenu.Items.Add(FSuggestionWorkflowItem);

  FSuggestionAIConfigItem := TMenuItem.Create(FSuggestionPopupMenu);
  FSuggestionAIConfigItem.Action := ActionLlmOptions;
  FSuggestionPopupMenu.Items.Add(FSuggestionAIConfigItem);

  ToolButton6.Style := tbsDropDown;
  ToolButton6.DropdownMenu := FSuggestionPopupMenu;
end;

procedure TMainForm.BuildTheWordToolbarMenu;
var
  Item: TMenuItem;
  ButtonIndex: Integer;
begin
  FTheWordPopupMenu := TPopupMenu.Create(Self);

  Item := TMenuItem.Create(FTheWordPopupMenu);
  Item.Tag := 1;
  Item.OnClick := @MenuItemSyncTheWordClick;
  FTheWordPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FTheWordPopupMenu);
  Item.Tag := 2;
  Item.OnClick := @MenuItemSynciBibliaClick;
  FTheWordPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FTheWordPopupMenu);
  Item.Caption := '-';
  FTheWordPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FTheWordPopupMenu);
  Item.Tag := 3;
  Item.OnClick := @DisableTheWordSyncClick;
  FTheWordPopupMenu.Items.Add(Item);

  FTheWordToolButton := TToolButton.Create(ToolBar1);
  FTheWordToolButton.Parent := ToolBar1;
  FTheWordToolButton.Left := ToolButtonSyncSeparator.Left + ToolButtonSyncSeparator.Width;
  FTheWordToolButton.Top := ToolButtonSyncSeparator.Top;
  FTheWordToolButton.Style := tbsDropDown;
  FTheWordToolButton.DropdownMenu := FTheWordPopupMenu;
  FTheWordToolButton.ImageIndex := AppIconIndex(aiSyncThewordVerse);
  FTheWordToolButton.ShowHint := True;
  FTheWordToolButton.OnClick := @TheWordToolbarButtonClick;

  { Keep the established icon-only toolbar compact while making this stateful
    integration button readable without relying on color or a tooltip. }
  ToolBar1.List := True;
  ToolBar1.ShowCaptions := True;
  for ButtonIndex := 0 to ToolBar1.ButtonCount - 1 do
    ToolBar1.Buttons[ButtonIndex].ShowCaption := False;
  FTheWordToolButton.ShowCaption := True;
  FTheWordToolButton.AutoSize := True;
end;

procedure TMainForm.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  if not Assigned(FMenuImages) then Exit;
  ToolBar1.ButtonHeight := UIScale(Self, 40);
  ToolBar1.ButtonWidth := UIScale(Self, 40);
  TreeView1.DefaultItemHeight := UIScale(Self, 24);
  FStatusReviewList.ItemHeight := UIScale(Self, 24);
  RadioGroupStatus.ItemHeight := UIScale(Self, 24);
  LayoutStateHeader;
  LayoutGuidePanel;
  LayoutVersePanelLabels;
end;

procedure TMainForm.InitializeVisualIdentity;
var
  TextType: TTipoTextoBiblico;

  procedure AssignIcon(AAction: TCustomAction; AIcon: TAppIcon);
  begin
    if Assigned(AAction) then
      AAction.ImageIndex := AppIconIndex(AIcon);
  end;

begin
  LoadApplicationIcon(Icon);
  Application.Icon.Assign(Icon);
  PopulateAppImageList(ImageList1, 32);
  FMenuImages := TImageList.Create(Self);
  PopulateAppImageList(FMenuImages, 16);
  ActionList1.Images := FMenuImages;
  MainMenu1.Images := FMenuImages;
  ToolBar1.Images := ImageList1;
  ToolBar1.ImagesWidth := 32;
  ToolBar1.ButtonHeight := UIScale(Self, 40);
  ToolBar1.ButtonWidth := UIScale(Self, 40);
  ToolBar1.Wrapable := True;
  ToolBar1.EdgeBorders := [];
  ToolBar1.BorderSpacing.Left := UIScale(Self, 8);
  ToolBar1.BorderSpacing.Right := UIScale(Self, 8);
  ToolBar1.BorderSpacing.Top := UIScale(Self, 4);
  ToolBar1.BorderSpacing.Bottom := UIScale(Self, 4);
  AssignIcon(ActionNewProject, aiProjectNew);
  AssignIcon(ActionOpenProject, aiProjectOpen);
  AssignIcon(ActionSaveProject, aiProjectSave);
  AssignIcon(ActionSaveProjectAs, aiProjectSaveAs);
  AssignIcon(ActionCloseProject, aiProjectClose);
  AssignIcon(ActionExit, aiAppExit);
  ToolButtonExit.ImageIndex := AppIconIndex(aiAppExit);
  AssignIcon(ActionProjectProperties, aiProjectSettings);
  AssignIcon(ActionExportProject, aiProjectExport);
  AssignIcon(ActionExportDestinationWithStrongs, aiExportDestinationStrongs);
  AssignIcon(ActionExportInterlinearText, aiExportInterlinear);
  AssignIcon(ActionSuggestAssociation, aiAssociationAuto);
  AssignIcon(ActionClearAssociations, aiAssociationClear);
  AssignIcon(ActionRollbackCurrentVerse, aiAssociationRollback);
  AssignIcon(ActionHistoryBack, aiHistoryBack);
  AssignIcon(ActionHistoryForward, aiHistoryForward);
  AssignIcon(ActionPreviousVerse, aiVersePrevious);
  AssignIcon(ActionNextVerse, aiVerseNext);
  AssignIcon(ActionFirstVerse, aiVerseFirst);
  AssignIcon(ActionLastVerse, aiVerseLast);
  AssignIcon(ActionMergeProjects, aiProjectsMerge);
  AssignIcon(ActionRebuildSuggestionDatabase, aiAssociationMemoryRebuild);
  AssignIcon(ActionSyncTheWordVerse, aiSyncThewordVerse);
  AssignIcon(ActionShowTags, aiTagsShow);
  AssignIcon(ActionNewVerse, aiTextLoad);
  AssignIcon(ActionExportPatch, aiPatchCreate);
  AssignIcon(ActionApplyPatch, aiPatchApply);
  AssignIcon(ActionVaccumProject, aiDatabaseVacuum);
  AssignIcon(ActionKeyboardShortcuts, aiKeyboardShortcuts);
  AssignIcon(ActionResetLayout, aiLayoutReset);
  AssignIcon(ActionGuideMode, aiGuideMode);
  AssignIcon(ActionLlmOptions, aiLlmOptions);
  AssignIcon(FActionPreferences, aiPreferences);
  AssignIcon(ActionTheWordHighlighterColors, aiThewordHighlighterColors);
  AssignIcon(ActionRegisterFileAssociation, aiFileAssociationRegister);
  AssignIcon(ActionUnregisterFileAssociation, aiFileAssociationUnregister);
  MenuItemAbout.ImageIndex := AppIconIndex(aiAbout);
  FSuggestionPopupMenu.Images := FMenuImages;
  FSuggestionStatisticalItem.ImageIndex := AppIconIndex(aiAssociationAuto);
  FSuggestionAIItem.ImageIndex := AppIconIndex(aiLlmOptions);
  FSuggestionWorkflowItem.ImageIndex := AppIconIndex(aiProjectSettings);
  FTheWordPopupMenu.Images := FMenuImages;
  FTheWordPopupMenu.Items[0].ImageIndex := AppIconIndex(aiSyncThewordVerse);
  FTheWordPopupMenu.Items[1].ImageIndex := AppIconIndex(aiSyncThewordVerse);
  FTheWordSyncMenu.ImageIndex := AppIconIndex(aiSyncThewordVerse);

  StyleControl(Panel1, uirSurface);
  StyleControl(NavigationPanel, uirSurface);
  StyleControl(CenterPanel, uirSurface);
  StyleControl(ContextPanel, uirSurface);
  StyleControl(BottomPanel, uirSurface);
  StyleControl(TreeLegendPanel, uirSurface);
  StyleControl(FStateHeaderPanel, uirSurface);
  StyleControl(FGuidePanel, uirSurface);
  StyleControl(FMemoryOrderPanel, uirSurface);
  StyleControl(FChapterViewHostPanel, uirContent);
  StyleControl(FGuideTitleLabel, uirSectionTitle);
  StyleControl(FGuideHideButton, uirButton);
  StyleControl(FStatusReviewResetButton, uirButton);
  StyleControl(TreeView1, uirContent);
  TreeView1.BorderStyle := bsNone;
  TreeView1.DefaultItemHeight := UIScale(Self, 24);
  TreeView1.BorderSpacing.Around := UIScale(Self, 4);
  TreeLegendPanel.BorderSpacing.Around := UIScale(Self, 8);
  StyleControl(FStatusReviewList, uirContent);
  FStatusReviewList.ParentColor := False;
  FStatusReviewList.Font.Color := UITextColor;
  FStatusReviewList.ItemHeight := UIScale(Self, 24);
  FStatusReviewResetButton.Height := UIScale(Self, 32);
  RadioGroupStatus.ItemHeight := UIScale(Self, 24);
  for TextType := tbOrigem to tbConsulta2 do
    StyleControl(FVersePanelLabels[TextType], uirSurface);
  LayoutStateHeader;
  LayoutGuidePanel;
  LayoutVersePanelLabels;
end;

procedure TMainForm.BuildCommandUI;
begin
  ActionProjectProperties.Caption := SProjectSettingsMenu;
  ActionSuggestAssociation.Caption := SSuggestAssociationCaption;
  ActionSuggestAssociation.Hint := SSuggestAssociationHint;

  while MenuItem20.Count > 0 do
    MenuItem20.Remove(MenuItem20.Items[0]);
  MenuItem20.Caption := SViewMenu;

  FVerseInformationMenu := TMenuItem.Create(MainMenu1);
  FVerseInformationMenu.Add(MenuItemStrongsCount);
  FVerseInformationMenu.Add(MenuItemBoldStrongs);
  MenuItem20.Add(FVerseInformationMenu);
  MenuItem20.Add(MenuItemAlwaysOnTop);
  FViewMenuSeparator := TMenuItem.Create(MainMenu1);
  FViewMenuSeparator.Caption := '-';
  MenuItem20.Add(FViewMenuSeparator);
  MenuItem20.Add(MenuItemResetLayout);

  if Assigned(MenuItemLlmOptions.Parent) then
    MenuItemLlmOptions.Parent.Remove(MenuItemLlmOptions);
  MenuItem1.Insert(MenuItem19.MenuIndex + 1, MenuItemLlmOptions);

  FToolsMenu := TMenuItem.Create(MainMenu1);
  MainMenu1.Items.Insert(MenuItem17.MenuIndex, FToolsMenu);
  FTheWordSyncMenu := TMenuItem.Create(MainMenu1);
  FTheWordSyncMenu.Add(MenuItemSyncTheWord);
  FTheWordSyncMenu.Add(MenuItemSynciBiblia);
  FDisableTheWordSyncItem := TMenuItem.Create(MainMenu1);
  FDisableTheWordSyncItem.OnClick := @DisableTheWordSyncClick;
  FTheWordSyncMenu.Add(FDisableTheWordSyncItem);
  FToolsMenuSeparator := TMenuItem.Create(MainMenu1);
  FToolsMenuSeparator.Caption := '-';
  FTheWordSyncMenu.Add(FToolsMenuSeparator);
  FTheWordSyncMenu.Add(MenuItemTheWordHighlighterColors);
  FToolsMenu.Add(FTheWordSyncMenu);

  FActionPreferences := TAction.Create(ActionList1);
  FActionPreferences.OnExecute := @ActionPreferencesExecute;
  FPreferencesMenuItem := TMenuItem.Create(MainMenu1);
  FPreferencesMenuItem.Action := FActionPreferences;
  FToolsOuterSeparator := TMenuItem.Create(MainMenu1);
  FToolsOuterSeparator.Caption := '-';
  FToolsMenu.Add(FToolsOuterSeparator);
  FToolsMenu.Add(FPreferencesMenuItem);

  while MenuItem17.Count > 0 do
    MenuItem17.Remove(MenuItem17.Items[0]);
  MenuItem17.Add(MenuItemGuideMode);
  MenuItem17.Add(MenuItemKeyboardShortcuts);
  FHelpMenuSeparator := TMenuItem.Create(MainMenu1);
  FHelpMenuSeparator.Caption := '-';
  MenuItem17.Add(FHelpMenuSeparator);
  MenuItem17.Add(MenuItemAbout);

  BuildSuggestionToolbarMenu;
  BuildTheWordToolbarMenu;
  UpdateCommandUI;
end;

procedure TMainForm.CreateMemoryOrderControls;
begin
  FMemoryOrderPanel := TPanel.Create(Self);
  FMemoryOrderPanel.Parent := FChapterViewHostPanel;
  FMemoryOrderPanel.Align := alTop;
  FMemoryOrderPanel.Height := 38;
  FMemoryOrderPanel.BevelOuter := bvNone;
  FMemoryOrderPanel.BorderSpacing.Bottom := 4;
  FMemoryOrderPanel.Visible := False;

  FMemoryOrderLabel := TLabel.Create(Self);
  FMemoryOrderLabel.Parent := FMemoryOrderPanel;
  FMemoryOrderLabel.SetBounds(8, 8, 82, 24);
  FMemoryOrderLabel.Layout := tlCenter;

  FMemoryOrderCombo := TComboBox.Create(Self);
  FMemoryOrderCombo.Parent := FMemoryOrderPanel;
  FMemoryOrderCombo.SetBounds(94, 6, 205, 27);
  FMemoryOrderCombo.Anchors := [akLeft, akTop, akRight];
  FMemoryOrderCombo.Style := csDropDownList;
  FMemoryOrderCombo.OnChange := @TranslationMemoryOrderComboChange;
  FMemoryOrderLabel.FocusControl := FMemoryOrderCombo;
  UpdateMemoryOrderControls;
end;

procedure TMainForm.TranslationMemoryOrderComboChange(Sender: TObject);
begin
  if not Assigned(FMemoryOrderCombo) then
    Exit;
  case FMemoryOrderCombo.ItemIndex of
    0: FTranslationMemoryOrder := tmoVerseOccurrence;
    1: FTranslationMemoryOrder := tmoMemoryFrequencyAscending;
  else
    Exit;
  end;
  if Assigned(FChapterView) then
    FChapterView.TranslationMemoryOrder := FTranslationMemoryOrder;
  PersistGlobalPreference('translation.memory.order',
    Ord(FTranslationMemoryOrder));
  UpdateMemoryOrderControls;
end;

procedure TMainForm.UpdateMemoryOrderControls;
begin
  if not Assigned(FMemoryOrderCombo) then
    Exit;
  FMemoryOrderLabel.Caption := STranslationMemoryOrderMenu;
  FMemoryOrderCombo.OnChange := nil;
  FMemoryOrderCombo.Items.BeginUpdate;
  try
    FMemoryOrderCombo.Items.Clear;
    FMemoryOrderCombo.Items.Add(STranslationMemoryVerseOrder);
    FMemoryOrderCombo.Items.Add(STranslationMemoryRareFirst);
    FMemoryOrderCombo.ItemIndex := Ord(FTranslationMemoryOrder);
  finally
    FMemoryOrderCombo.Items.EndUpdate;
    FMemoryOrderCombo.OnChange := @TranslationMemoryOrderComboChange;
  end;
  FMemoryOrderPanel.Visible :=
    ChapterViewTabCtrl.TabIndex = CHAPTER_VIEW_MEMORY_TAB_INDEX;
end;

procedure TMainForm.UpdateTheWordSyncUI;
var
  HintText: string;
begin
  {$IFDEF WINDOWS}
  MenuItemSyncTheWord.Checked := FSyncTheWordToIBiblia;
  MenuItemSynciBiblia.Checked := FSyncIBibliaToTheWord;
  if Assigned(FDisableTheWordSyncItem) then
    FDisableTheWordSyncItem.Checked :=
      not FSyncTheWordToIBiblia and not FSyncIBibliaToTheWord;
  if Assigned(FTheWordPopupMenu) then
  begin
    FTheWordPopupMenu.Items[0].Checked := FSyncTheWordToIBiblia;
    FTheWordPopupMenu.Items[1].Checked := FSyncIBibliaToTheWord;
    FTheWordPopupMenu.Items[3].Checked :=
      not FSyncTheWordToIBiblia and not FSyncIBibliaToTheWord;
  end;
  case ResolveTheWordSyncState(FSyncTheWordToIBiblia,
      FSyncIBibliaToTheWord) of
    twssIBibliaToTheWord:
      HintText := STheWordStateOutbound;
    twssTheWordToIBiblia:
      HintText := STheWordStateInbound;
    twssBidirectional:
      HintText := STheWordStateBidirectional;
    else
      HintText := STheWordStateOff;
  end;
  {$ELSE}
  HintText := STheWordStateOff;
  {$ENDIF}
  if Assigned(FTheWordToolButton) then
  begin
    FTheWordToolButton.Hint := HintText;
    FTheWordToolButton.Caption := HintText;
  end;
end;

procedure TMainForm.UpdateCommandUI;
var
  SuggestionMode: TSuggestionMode;
begin
  ActionSaveProjectAsUpdate(ActionSaveProjectAs);
  MenuItemAutoSave.Checked := FAutoSaveEnabled;
  MenuItemBoldStrongs.Checked := FBoldStrongsEnabled;
  MenuItemAlwaysOnTop.Checked := FAlwaysOnTopEnabled;

  if Assigned(CurrentProject) then
    SuggestionMode := CurrentProject.SuggestionMode
  else
    SuggestionMode := FSuggestionMode;
  MenuItemStatisticalSuggestion.Checked :=
    SuggestionMode = smStatistical;
  MenuItemAISuggestion.Checked := SuggestionMode = smAI;
  if Assigned(FSuggestionStatisticalItem) then
  begin
    FSuggestionStatisticalItem.Checked := SuggestionMode = smStatistical;
    FSuggestionAIItem.Checked := SuggestionMode = smAI;
    FSuggestionStatisticalItem.Enabled := Assigned(CurrentProject);
    FSuggestionAIItem.Enabled := Assigned(CurrentProject);
    FSuggestionWorkflowItem.Enabled := Assigned(CurrentProject);
    FSuggestionAIConfigItem.Enabled := Assigned(CurrentProject);
  end;
  UpdateTheWordSyncUI;
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
    AddActionShortcut(ActionSaveProjectAs, SShortcutSaveProjectAs);
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
    Color := UISurfaceColor;
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
    ALabel.Font.Assign(Font);
    ALabel.Font.Color := UITextColor;
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

  FVersePanelTypeLabels[TextType] := TOverflowLabel.Create(Result);
  SetupChildLabel(FVersePanelTypeLabels[TextType], True);

  FVersePanelDescriptionLabels[TextType] := TOverflowLabel.Create(Result);
  SetupChildLabel(FVersePanelDescriptionLabels[TextType], False);
end;

function TMainForm.CreateStateHeaderLabel(AParent: TWinControl;
  Bold: Boolean): TLabel;
begin
  Result := TOverflowLabel.Create(AParent);
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
var
  Mode: TSuggestionMode;
begin
  if Assigned(CurrentProject) then
    Mode := CurrentProject.SuggestionMode
  else
    Mode := FSuggestionMode;

  case Mode of
    smAI: Result := SHeaderEngineAI;
  else
    Result := SHeaderEngineStatistical;
  end;

  if Assigned(CurrentProject) and
     CurrentProject.AutomaticAssociationSuggestion then
    Result := Result + ' · ' + SHeaderEngineAutomatic;
  if Assigned(CurrentProject) and (Mode = smAI) and
     not CurrentProject.AIConfigurationAvailable then
    Result := Result + ' · ' + SHeaderEngineAIConfigurationPending;
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
var
  Margin, Gap: Integer;
  Compact: Boolean;
  ButtonWidth, ButtonHeight, ButtonTop, TextWidth, TitleHeight,
    TextTop, TextHeight, DesiredHeight, LineHeight, WrappedLines: Integer;

  function CountWrappedLines(const AText: string; AMaxWidth: Integer): Integer;
  var
    TextBounds: TRect;
  begin
    TextBounds := Rect(0, 0, Max(1, AMaxWidth), 0);
    LCLIntf.DrawText(FGuidePanel.Canvas.Handle, PChar(AText), Length(AText),
      TextBounds, DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);
    Result := Max(1, Ceil(TextBounds.Height /
      Max(1, FGuidePanel.Canvas.TextHeight('Hg'))));
  end;

begin
  if not Assigned(FGuidePanel) or (FGuidePanel.ClientWidth <= 0) then
    Exit;

  Margin := UIScale(Self, 8);
  Gap := UIScale(Self, 16);
  Compact := FGuidePanel.ClientWidth < UIScale(Self, 520);

  FGuidePanel.Canvas.Font.Assign(Font);
  ButtonWidth := Max(UIScale(Self, 100),
    FGuidePanel.Canvas.TextWidth(FGuideHideButton.Caption) + 28);
  ButtonHeight := Max(UIScale(Self, 32), FGuidePanel.Canvas.TextHeight('Hg') + 12);
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

  PersistGlobalPreference('guide.mode', FGuideModeEnabled);

  UpdateGuidePanel;
end;

procedure TMainForm.LayoutStateHeader;
var
  X, Y, Margin, Gap, LineHeight, AvailableWidth, DesiredHeight: Integer;

  procedure StartNextLine(AWidth: Integer);
  begin
    if (X > Margin) and (X + AWidth > FStateHeaderPanel.ClientWidth - Margin) then
    begin
      X := Margin;
      Inc(Y, LineHeight + UIScale(Self, 4));
    end;
  end;

  procedure PlaceLabel(ALabel: TLabel);
  var
    W: Integer;
  begin
    ALabel.Visible := ALabel.Caption <> '';
    if not ALabel.Visible then
      Exit;
    FStateHeaderPanel.Canvas.Font.Assign(ALabel.Font);
    W := Min(AvailableWidth,
      FStateHeaderPanel.Canvas.TextWidth(ALabel.Caption) + UIScale(Self, 8));
    StartNextLine(W);
    ALabel.SetBounds(X, Y, W, LineHeight);
    Inc(X, W + Gap);
  end;

  procedure PlaceStatusCombo;
  var
    W: Integer;
  begin
    RadioGroupStatus.Visible := Assigned(CurrentProject) and
      (RadioGroupStatus.Items.Count > 0);
    if not RadioGroupStatus.Visible then
      Exit;
    W := Min(AvailableWidth, UIScale(Self, 190));
    StartNextLine(W);
    RadioGroupStatus.SetBounds(X, Y + Max(0,
      (LineHeight - RadioGroupStatus.Height) div 2), W, RadioGroupStatus.Height);
    Inc(X, W + Gap);
  end;

begin
  if not Assigned(FHeaderModifiedLabel) or FLayoutStateHeaderBusy then
    Exit;
  FLayoutStateHeaderBusy := True;
  try
    Margin := UIScale(Self, 8);
    Gap := UIScale(Self, 16);
    AvailableWidth := Max(1, FStateHeaderPanel.ClientWidth - Margin * 2);
    FStateHeaderPanel.Canvas.Font.Assign(Font);
    LineHeight := Max(RadioGroupStatus.Height,
      FStateHeaderPanel.Canvas.TextHeight('Hg') + UIScale(Self, 8));
    X := Margin;
    Y := UIScale(Self, 4);
    PlaceLabel(FHeaderReferenceLabel);
    PlaceStatusCombo;
    PlaceLabel(FHeaderAssociationsLabel);
    PlaceLabel(FHeaderSelectionLabel);
    PlaceLabel(FHeaderEngineLabel);
    PlaceLabel(FHeaderModifiedLabel);
    DesiredHeight := Y + LineHeight + UIScale(Self, 4);
    if FStateHeaderPanel.Height <> DesiredHeight then
      FStateHeaderPanel.Height := DesiredHeight;
  finally
    FLayoutStateHeaderBusy := False;
  end;
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
      FHeaderModifiedLabel.Font.Color := UIErrorColor;
    end
    else
    begin
      FHeaderModifiedLabel.Caption := SHeaderSaved;
      FHeaderModifiedLabel.Font.Color := UISuccessColor;
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
  SelectedStatusId, PreviousTopIndex, RestoredItemIndex: Integer;
  HadSelectedStatus: Boolean;
  Node: TTreeNode;

  procedure RememberCurrentFilter;
  var
    StatusIndex: Integer;
  begin
    OldStates := nil;
    HadSelectedStatus := False;
    PreviousTopIndex := 0;
    if not Assigned(FStatusReviewList) then
      Exit;

    PreviousTopIndex := FStatusReviewList.TopIndex;
    StatusIndex := FStatusReviewList.ItemIndex;
    HadSelectedStatus := (StatusIndex >= 0) and
      (StatusIndex < FStatusReviewList.Items.Count);
    if HadSelectedStatus then
      SelectedStatusId := Integer(PtrUInt(FStatusReviewList.Items.Objects[StatusIndex]));
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
  FStatusReviewList.Items.BeginUpdate;
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
    try
      FStatusReviewList.Items.EndUpdate;
      RestoredItemIndex := -1;
      if HadSelectedStatus then
        for Index := 0 to FStatusReviewList.Items.Count - 1 do
          if Integer(PtrUInt(FStatusReviewList.Items.Objects[Index])) = SelectedStatusId then
          begin
            RestoredItemIndex := Index;
            Break;
          end;
      FStatusReviewList.ItemIndex := RestoredItemIndex;
      if FStatusReviewList.Items.Count > 0 then
        FStatusReviewList.TopIndex := EnsureRange(PreviousTopIndex, 0,
          FStatusReviewList.Items.Count - 1);
    finally
      FUpdatingStatusReview := False;
    end;
    FStatusReviewList.Invalidate;
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
var
  Margin, LabelHeight, Padding: Integer;
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
  Margin := UIScale(Self, 4);
  Padding := UIScale(Self, 8);
  CenterPanel.Canvas.Font.Assign(Font);
  LabelHeight := CenterPanel.Canvas.TextHeight('Hg') + UIScale(Self, 4);
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
    LabelWidth := Min(TypeWidth + DescriptionWidth + Padding * 2, AvailableWidth);
    LabelControl.SetBounds(
      PanelControl.Left + Max(Margin, PanelControl.Width - LabelWidth - Margin),
      PanelControl.Top + Max(Margin, PanelControl.Height - LabelHeight - Margin),
      LabelWidth,
      LabelHeight);

    TypeWidth := Min(TypeWidth, Max(0, LabelWidth - Padding * 2));
    DescriptionWidth := Max(0, LabelWidth - TypeWidth - Padding * 2);
    TypeLabel.SetBounds(Padding, 0, TypeWidth, LabelHeight);
    DescriptionLabel.SetBounds(Padding + TypeWidth, 0, DescriptionWidth, LabelHeight);
    LabelControl.BringToFront;
  end;
end;

procedure TMainForm.ResetMainLayout;
var
  DefaultTreeWidth, DefaultContextWidth, DefaultCommentsHeight,
    MinCommentsHeight, MinVerseHeight: Integer;
  TextTop, TextBottom, AvailableHeight, BaseHeight, ExtraHeight: Integer;
  TextWidth: Integer;
begin
  DefaultTreeWidth := UIScale(Self, 145);
  DefaultContextWidth := UIScale(Self, 321);
  DefaultCommentsHeight := UIScale(Self, 120);
  MinCommentsHeight := UIScale(Self, 80);
  MinVerseHeight := UIScale(Self, 42);
  NavigationPanel.Width := DefaultTreeWidth;
  ContextPanel.Width := Min(DefaultContextWidth, Max(UIScale(Self, 180), Panel1.ClientWidth div 2));
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
  if Assigned(FChapterView) then
    FChapterView.Project := nil;
  if Assigned(FrmProjectProperties) then
    FrmProjectProperties.ClearProject;
  if Assigned(FrmLLMOptions) then
    FrmLLMOptions.SetProject(nil);
  CurrentProject.Destroy;
  CurrentProject := nil;
  UpdateMainCaption;
  UpdateNavigationHistoryActions;
  UpdateCommandUI;

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
  if Assigned(ActionLlmOptions) then ActionLlmOptions.Enabled := true;
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
          FBoldStrongsEnabled;
        CurrentProject.MostrarQtdStrongs := FStrongsCountMode;
        CurrentProject.AutoSave := FAutoSaveEnabled;
        CurrentProject.OnNewVerseSubscribe(@ProjectNewVerse);
        CurrentProject.OnAlterarVersiculo := @VerseChanged;
        CurrentProject.OnSintagmaClick := @SyntagmClick;
        CurrentProject.CreateProject(Staging.StagingFileName,
          Draft.ProjectName, Draft.TokenizerVersion);
        CurrentProject.SetInfo(PROJECT_SCENARIO_CONFIG_KEY,
          ProjectScenarioToConfig(Draft.Scenario));
        if Draft.HasCustomStatusCatalog then
          CurrentProject.SetStatusDefinitions(Draft.StatusDefinitions);
        if Draft.Scenario = psTranslationProject then
          CurrentProject.InitializeWorkflowSettings(smStatistical, False,
            dvmEditing)
        else
          CurrentProject.InitializeWorkflowSettings(smStatistical, False,
            dvmAssociation);
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
  UpdateCommandUI;
  UpdateStateHeader;
end;

procedure TMainForm.ActionLlmOptionsExecute(Sender: TObject);
begin
  if not assigned(FrmLLMOptions) then
    Application.CreateForm(TFrmLLMOptions, FrmLLMOptions);
  FrmLLMOptions.SetProject(CurrentProject);
  FrmLLMOptions.ShowModal;
  UpdateCommandUI;
  UpdateStateHeader;
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
var
  SavingProject: TProject;
  PreviousFileName: string;
begin
  SavingProject := CurrentProject;
  if not Assigned(SavingProject) then
    Exit;
  if not SavingProject.BeginSaveAs then
    Exit;
  try
    ActionSaveProjectAsUpdate(ActionSaveProjectAs);
    PreviousFileName := SavingProject.Caminho;
    SaveDialog1.InitialDir := ExtractFileDir(PreviousFileName);
    SaveDialog1.FileName := ExtractFileName(PreviousFileName);
    SaveDialog1.DefaultExt := '.bib';
    SaveDialog1.Filter := OpenDialog1.Filter;
    SaveDialog1.FilterIndex := 1;
    SaveDialog1.Options := SaveDialog1.Options +
      [ofOverwritePrompt, ofPathMustExist, ofNoChangeDir];
    if not SaveDialog1.Execute then
      Exit;

    try
      if SavingProject.SaveProjectAs(SaveDialog1.FileName,
          FileExists(SaveDialog1.FileName)) then
      begin
        UpdateRecentProjects(MenuItemRecent, PreviousFileName);
        UpdateMainCaption;
        VerseChanged;
      end;
    except
      on E: Exception do
        MessageDlg(SaveDialog1.Title, E.Message, mtError, [mbOK], 0);
    end;
  finally
    SavingProject.EndSaveAs;
    UpdateCommandUI;
    UpdateStateHeader;
  end;
end;

procedure TMainForm.ActionSaveProjectAsUpdate(Sender: TObject);
begin
  ActionSaveProjectAs.Enabled := Assigned(CurrentProject) and
    CurrentProject.CanSaveAs;
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
      if not CurrentProject.AIConfigurationAvailable then
      begin
        if QuestionDlg(SAIConfigurationPendingTitle,
            SAIConfigurationPendingText, mtInformation,
            [mrYes, SConfigureAIButton, 'isdefault', mrCancel,
             SButtonCancel, 'iscancel'], 0) = mrYes then
          ActionLlmOptionsExecute(Self);
        Exit;
      end;
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
  ApplyFormStyle(Self);
  DebugLn('=================================');

  CurrentProject := nil;

  ConfigFilePath := ExtractFilePath(ParamStr(0)) + 'iBiblia.ini';
  opts := TIniFile.Create(ConfigFilePath);
  SetAppIconFamily(AppIconFamilyFromId(
    opts.ReadString('opcoes', 'ui.icon.family', 'connected')));
  LoadTheWordSyntaxColorScheme(opts);
  LoadTheWordSyntaxEditorFont(opts);

  if opts.ReadBool('leiaute', 'principal.maximized', false) then
    WindowState := wsMaximized
  else
  begin
    Width  := opts.ReadInteger('leiaute', 'principal.largura',  Width);
    Height := opts.ReadInteger('leiaute', 'principal.altura',   Height);
  end;

  FLegacyAutomaticSuggestion :=
    opts.ReadBool('opcoes', 'sugestoes.automaticas', false);
  MenuItem22.Checked := FLegacyAutomaticSuggestion;
  {$IFDEF WINDOWS}
  ExpandTheWordSyncState(ReadTheWordSyncState(opts),
    FSyncTheWordToIBiblia, FSyncIBibliaToTheWord);
  {$ENDIF}
  FBoldStrongsEnabled := opts.ReadBool('opcoes', 'boldstrongs', false);
  FAutoSaveEnabled := opts.ReadBool('opcoes', 'auto.save', false);
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

  modeInt := opts.ReadInteger('opcoes', 'popuptrigger', Ord(ptMouseHover));
  if (modeInt < Ord(Low(TPopupTrigger))) or
     (modeInt > Ord(High(TPopupTrigger))) then
    modeInt := Ord(ptMouseHover);
  FPopupTrigger := TPopupTrigger(modeInt);

  FAlwaysOnTopEnabled := opts.ReadBool('opcoes', 'alwaysontop', false);
  if FAlwaysOnTopEnabled then
    FormStyle := fsSystemStayOnTop;

  FChapterViewHostPanel := TPanel.Create(Self);
  FChapterViewHostPanel.Parent := ChapterViewTabCtrl;
  FChapterViewHostPanel.Align := alClient;
  FChapterViewHostPanel.BevelOuter := bvNone;
  CreateMemoryOrderControls;

  FChapterView := TChapterView.Create(FChapterViewHostPanel);
  with FChapterView do
  begin
    Parent := FChapterViewHostPanel;
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
  FMemoryOrderPanel.BringToFront;

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

  ActionResetLayout := TAction.Create(ActionList1);
  ActionResetLayout.Caption := SResetLayoutMenu;
  ActionResetLayout.Hint := SResetLayoutHint;
  ActionResetLayout.OnExecute := @ActionResetLayoutExecute;

  MenuItemResetLayout := TMenuItem.Create(MainMenu1);
  MenuItemResetLayout.Action := ActionResetLayout;

  ActionGuideMode := TAction.Create(ActionList1);
  ActionGuideMode.Caption := SGuideModeMenu;
  ActionGuideMode.Hint := SGuideModeHint;
  ActionGuideMode.Checked := FGuideModeEnabled;
  ActionGuideMode.OnExecute := @ActionGuideModeExecute;

  MenuItemGuideMode := TMenuItem.Create(MainMenu1);
  MenuItemGuideMode.Action := ActionGuideMode;
  MenuItemGuideMode.Checked := FGuideModeEnabled;

  ActionTheWordHighlighterColors := TAction.Create(ActionList1);
  ActionTheWordHighlighterColors.Caption := STheWordHighlightColorsMenu;
  ActionTheWordHighlighterColors.OnExecute := @ActionTheWordHighlighterColorsExecute;

  MenuItemTheWordHighlighterColors := TMenuItem.Create(MainMenu1);
  MenuItemTheWordHighlighterColors.Action := ActionTheWordHighlighterColors;

  ActionRegisterFileAssociation := TAction.Create(ActionList1);
  ActionRegisterFileAssociation.Caption := SRegisterFileAssociation;
  ActionRegisterFileAssociation.OnExecute := @ActionRegisterFileAssociationExecute;
  MenuItemRegisterFileAssoc.Action := ActionRegisterFileAssociation;

  ActionUnregisterFileAssociation := TAction.Create(ActionList1);
  ActionUnregisterFileAssociation.Caption := SUnregisterFileAssociation;
  ActionUnregisterFileAssociation.OnExecute := @ActionUnregisterFileAssociationExecute;
  MenuItemUnregisterFileAssoc.Action := ActionUnregisterFileAssociation;

  ActionLlmOptions := TAction.Create(ActionList1);
  ActionLlmOptions.Caption := SLLMOptionsMenu;
  ActionLlmOptions.Enabled := True;
  ActionLlmOptions.OnExecute := @ActionLlmOptionsExecute;

  MenuItemLlmOptions := TMenuItem.Create(MainMenu1);
  MenuItemLlmOptions.Action := ActionLlmOptions;

  BuildCommandUI;
  InitializeVisualIdentity;
  Translate;

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
  SetUpSyncThread;
  UpdateTheWordSyncUI;

  // Check and update file association menu items
  CheckFileAssociation;
  {$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  StopTheWordSyncThread;
  {$ENDIF}
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
  opts.WriteBool('opcoes', 'sugestoes.automaticas',
    FLegacyAutomaticSuggestion);
  opts.WriteInteger('opcoes', 'suggestion.mode', Ord(FSuggestionMode));
  {$IFDEF WINDOWS}
  WriteTheWordSyncState(opts, ResolveTheWordSyncState(
    FSyncTheWordToIBiblia, FSyncIBibliaToTheWord));
  {$ENDIF}
  opts.WriteBool('opcoes', 'boldstrongs', FBoldStrongsEnabled);
  opts.WriteInteger('opcoes', 'strongscountmode', Integer(FStrongsCountMode));
  opts.WriteString('opcoes', 'language', language);
  opts.WriteBool('opcoes', 'alwaysontop', FAlwaysOnTopEnabled);
  opts.WriteInteger('opcoes', 'popuptrigger', LongInt(FPopupTrigger));
  opts.WriteBool('opcoes', 'auto.save', FAutoSaveEnabled);
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
  if not Assigned(CurrentProject) then
    Exit;
  CurrentProject.AutomaticAssociationSuggestion :=
    not CurrentProject.AutomaticAssociationSuggestion;
  MenuItem22.Checked := CurrentProject.AutomaticAssociationSuggestion;
  UpdateCommandUI;
  UpdateStateHeader;
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
  SetSuggestionMode(smAI);
end;

procedure TMainForm.MenuItemAlwaysOnTopClick(Sender: TObject);
begin
  SetAlwaysOnTopEnabled(not FAlwaysOnTopEnabled);
end;

procedure TMainForm.MenuItemAutoSaveClick(Sender: TObject);
begin
  SetAutoSaveEnabled(not FAutoSaveEnabled);
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
  PersistGlobalPreference('language', language);
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
  PersistGlobalPreference('language', language);
  SetDefaultLang('pt');
  TMenuItem(Sender).Checked:=true;
  MenuItemLangEn.Checked:=false;
  Translate;
end;

procedure TMainForm.MenuItemPopupTriggerClick(Sender: TObject);
var
  Index: Integer;
begin
  for Index := 0 to MenuItemDictPopup.Count - 1 do
    if Sender = MenuItemDictPopup.Items[Index] then
    begin
      SetPopupTrigger(TPopupTrigger(Index));
      Exit;
    end;
end;

procedure TMainForm.MenuItemStatisticalSuggestionClick(Sender: TObject);
begin
  SetSuggestionMode(smStatistical);
end;

procedure TMainForm.MenuItemBoldStrongsClick(Sender: TObject);
begin
  SetBoldStrongsEnabled(not FBoldStrongsEnabled);
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
      SetStrongsCountMode(TStrongsCountMode(m.Tag));
      Break;
    end;
  end;
end;

procedure TMainForm.MenuItemSynciBibliaClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  SetTheWordSyncDirections(FSyncTheWordToIBiblia,
    not FSyncIBibliaToTheWord);
  {$ENDIF}
end;

procedure TMainForm.MenuItemSyncTheWordClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  SetTheWordSyncDirections(not FSyncTheWordToIBiblia,
    FSyncIBibliaToTheWord, True);
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

procedure TMainForm.UpdateRecentProjects(m: TMenuItem;
  const PreviousFileName: string);
var
  f: TStringList;
  j, k: Integer;
  found: Boolean;
begin
  f := TStringList.Create;

  try
    f.Add(CurrentProject.Caminho);
    if (PreviousFileName <> '') and
       (PreviousFileName <> CurrentProject.Caminho) then
      f.Add(PreviousFileName);

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
  TextStyle: TTextStyle;
begin
  if not (Control is TComboBox) then
    Exit;

  Combo := TComboBox(Control);
  Combo.Canvas.Font.Assign(Combo.Font);
  Combo.Canvas.Brush.Style := bsSolid;
  Combo.Canvas.Pen.Width := 1;
  if odSelected in State then
  begin
    Combo.Canvas.Brush.Color := clHighlight;
    Combo.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    Combo.Canvas.Brush.Color := UIContentColor;
    Combo.Canvas.Font.Color := UITextColor;
  end;
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
  SwatchRect := Rect(ARect.Left + UIScale(Combo, 4),
    ARect.Top + (ARect.Height - UIScale(Combo, 14)) div 2,
    ARect.Left + UIScale(Combo, 18),
    ARect.Top + (ARect.Height + UIScale(Combo, 14)) div 2);
  Combo.Canvas.Brush.Color := GetStatusColor(Definitions, StatusId);
  Combo.Canvas.Pen.Color := UIMutedTextColor;
  Combo.Canvas.Rectangle(SwatchRect);

  TextRect := ARect;
  TextRect.Left := ARect.Left + UIScale(Combo, 26);
  TextRect.Right := TextRect.Right - UIScale(Combo, 4);
  TextStyle := Combo.Canvas.TextStyle;
  TextStyle.Alignment := taLeftJustify;
  TextStyle.Layout := tlCenter;
  TextStyle.SingleLine := True;
  TextStyle.Wordbreak := False;
  TextStyle.Clipping := True;
  TextStyle.EndEllipsis := True;
  TextStyle.ShowPrefix := False;
  TextStyle.Opaque := False;
  Combo.Canvas.Brush.Style := bsClear;
  Combo.Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top,
    Combo.Items[Index], TextStyle);
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
  TextStyle: TTextStyle;
  SwatchColor: TColor;
  SwatchSize, MinimumSize, AvailableSize: Integer;
  CenterY, RowHeight, LeftInset, CheckInset: Integer;

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
  List.Canvas.Font.Assign(List.Font);
  List.Canvas.Brush.Style := bsSolid;
  List.Canvas.Pen.Style := psSolid;
  List.Canvas.Pen.Width := 1;
  if odSelected in State then
  begin
    List.Canvas.Brush.Color := clHighlight;
    List.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    List.Canvas.Brush.Color := UIContentColor;
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
  RowHeight := ARect.Bottom - ARect.Top;
  CenterY := ARect.Top + RowHeight div 2;
  MinimumSize := UIScale(List, 11);
  AvailableSize := RowHeight - UIScale(List, 6);
  SwatchSize := UIScale(List, 14);
  if SwatchSize > AvailableSize then
    SwatchSize := AvailableSize;
  if SwatchSize < MinimumSize then
    SwatchSize := MinimumSize;
  LeftInset := UIScale(List, 4);
  CheckInset := UIScale(List, 3);
  SwatchRect.Left := ARect.Left + LeftInset;
  SwatchRect.Top := CenterY - SwatchSize div 2;
  SwatchRect.Right := SwatchRect.Left + SwatchSize;
  SwatchRect.Bottom := SwatchRect.Top + SwatchSize;

  SwatchColor := GetStatusColor(Definitions, StatusId);
  List.Canvas.Brush.Color := SwatchColor;
  List.Canvas.Pen.Color := UIMutedTextColor;
  List.Canvas.Rectangle(SwatchRect);
  if StatusReviewItemChecked(Index) then
  begin
    List.Canvas.Pen.Color := CheckMarkColor(SwatchColor);
    List.Canvas.Pen.Width := UIScale(List, 2);
    List.Canvas.MoveTo(SwatchRect.Left + CheckInset, CenterY);
    List.Canvas.LineTo(SwatchRect.Left + SwatchSize div 2 - 1,
      SwatchRect.Bottom - CheckInset);
    List.Canvas.LineTo(SwatchRect.Right - CheckInset, SwatchRect.Top + CheckInset);
    List.Canvas.Pen.Width := 1;
  end;

  TextRect := ARect;
  TextRect.Left := SwatchRect.Right + UIScale(List, 8);
  TextRect.Right := TextRect.Right - LeftInset;
  TextStyle := List.Canvas.TextStyle;
  TextStyle.Alignment := taLeftJustify;
  TextStyle.Layout := tlCenter;
  TextStyle.SingleLine := True;
  TextStyle.Wordbreak := False;
  TextStyle.Clipping := True;
  TextStyle.EndEllipsis := True;
  TextStyle.ShowPrefix := False;
  TextStyle.Opaque := False;
  if odSelected in State then
    List.Canvas.Brush.Color := clHighlight
  else
    List.Canvas.Brush.Color := UIContentColor;
  List.Canvas.Brush.Style := bsClear;
  List.Canvas.TextRect(TextRect, TextRect.Left, TextRect.Top,
    List.Items[Index], TextStyle);
  List.Canvas.Brush.Style := bsSolid;

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
  UpdateMemoryOrderControls;
end;

procedure TMainForm.ToolButtonExitClick(Sender: TObject);
begin
  Close;
end;

{$IFDEF WINDOWS}
procedure TMainForm.SyncToTwRef(Ref: string);
begin
  if (CurrentProject <> nil) and not CurrentProject.SaveAsInProgress then
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
    StopTheWordSyncThread;
end;

procedure TMainForm.StopTheWordSyncThread;
begin
  if FTheWordSyncThread = nil then
    Exit;
  FTheWordSyncThread.OnRefChange := nil;
  FTheWordSyncThread.Stop;
  FTheWordSyncThread.WaitFor;
  FreeAndNil(FTheWordSyncThread);
end;
{$ENDIF}

procedure TMainForm.Translate;
var
  SelectedChapterViewTab: Integer;
begin
  SelectedChapterViewTab := ChapterViewTabCtrl.TabIndex;
  MenuItem1.Caption := SProjectMenu;
  MenuItem10.Caption := SEditMenu;
  MenuItem5.Caption := SNavigationMenu;
  MenuItem17.Caption := SHelpMenu;
  ActionNewProject.Caption := SNewProjectMenu;
  ActionNewProject.Hint := SShortcutNewProject;
  ActionOpenProject.Caption := SOpenProjectMenu;
  ActionOpenProject.Hint := SShortcutOpenProject;
  MenuItemRecent.Caption := SOpenRecentMenu;
  ActionSaveProject.Caption := SSaveProjectMenu;
  ActionSaveProject.Hint := SShortcutSaveProject;
  ActionSaveProjectAs.Caption := SSaveProjectAsMenu;
  ActionSaveProjectAs.Hint := SShortcutSaveProjectAs;
  ActionCloseProject.Caption := SCloseProjectMenu;
  ActionCloseProject.Hint := SShortcutCloseProject;
  ActionProjectProperties.Caption := SProjectSettingsMenu;
  ActionProjectProperties.Hint := SShortcutProjectSettings;
  ActionExportProject.Caption := SExportProjectMenu;
  ActionExportProject.Hint := SShortcutExportProject;
  MenuItem16.Caption := SExportProjectMenu;
  ActionExportPatch.Caption := SCreatePatchMenu;
  ActionApplyPatch.Caption := SApplyPatchMenu;
  MenuItem24.Caption := SMaintenanceMenu;
  ActionVaccumProject.Caption := SCompactProjectMenu;
  ActionRebuildSuggestionDatabase.Caption :=
    SRebuildAssociationMemoryMenu;
  ActionExit.Caption := SExitMenu;
  ActionExit.Hint := StripHotkey(SExitMenu);
  ToolButtonExit.Caption := SExitMenu;
  ToolButtonExit.Hint := ActionExit.Hint;
  ActionPreviousVerse.Caption := SPreviousVerseMenu;
  ActionPreviousVerse.Hint := SShortcutPreviousVerse;
  ActionNextVerse.Caption := SNextVerseMenu;
  ActionNextVerse.Hint := SShortcutNextVerse;
  ActionFirstVerse.Caption := SFirstVerseMenu;
  ActionLastVerse.Caption := SLastVerseMenu;
  MenuItemAbout.Caption := SAboutMenu;
  MenuItem20.Caption := SViewMenu;
  if Assigned(FVerseInformationMenu) then
    FVerseInformationMenu.Caption := SVerseInformationMenu;
  MenuItemStrongsCount.Caption := SStrongsCountMenu;
  MenuItemStrongsCountNone.Caption := SStrongsCountNone;
  MenuItemStrongsCountWords.Caption := SStrongsCountWords;
  MenuItemStrongsCountStrongs.Caption := SStrongsCountTags;
  MenuItemBoldStrongs.Caption := SBoldStrongsMenu;
  MenuItemAlwaysOnTop.Caption := SAlwaysOnTopMenu;
  if Assigned(FToolsMenu) then
    FToolsMenu.Caption := SToolsMenu;
  if Assigned(FTheWordSyncMenu) then
    FTheWordSyncMenu.Caption := STheWordSynchronizationMenu;
  MenuItemSyncTheWord.Caption := SSyncIBibliaToTheWord;
  MenuItemSynciBiblia.Caption := SSyncTheWordToIBiblia;
  if Assigned(FDisableTheWordSyncItem) then
    FDisableTheWordSyncItem.Caption := SDisableBothSyncDirections;
  if Assigned(FActionPreferences) then
    FActionPreferences.Caption := SPreferencesMenu;
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
    ActionTheWordHighlighterColors.Caption := STheWordEditorAppearance;
  if Assigned(FTranslationMemoryOrderMenu) then
    FTranslationMemoryOrderMenu.Caption := STranslationMemoryOrderMenu;
  if Assigned(FTranslationMemoryVerseOrderItem) then
    FTranslationMemoryVerseOrderItem.Caption :=
      STranslationMemoryVerseOrder;
  if Assigned(FTranslationMemoryRareFirstItem) then
    FTranslationMemoryRareFirstItem.Caption :=
      STranslationMemoryRareFirst;
  UpdateTranslationMemoryOrderMenu;
  UpdateMemoryOrderControls;
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
  if Assigned(FSuggestionStatisticalItem) then
  begin
    FSuggestionStatisticalItem.Caption := SSuggestionToolbarStatistical;
    FSuggestionAIItem.Caption := SSuggestionToolbarAI;
    FSuggestionWorkflowItem.Caption := SSuggestionToolbarWorkflow;
  end;
  if Assigned(FTheWordPopupMenu) then
  begin
    FTheWordPopupMenu.Items[0].Caption := SSyncIBibliaToTheWord;
    FTheWordPopupMenu.Items[1].Caption := SSyncTheWordToIBiblia;
    FTheWordPopupMenu.Items[3].Caption := SDisableBothSyncDirections;
  end;

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
  UpdateCommandUI;
end;

procedure TMainForm.DoOpenProject(filename: string);
begin
  CurrentProject := TProject.Create([ScrollBoxSrcVerse, ScrollBoxDstVerse, ScrollBoxRef1Verse, ScrollBoxRef2Verse], TreeView1, RadioGroupStatus, FActualCommentsMemo);
  CurrentProject.OnNewVerseSubscribe(@ProjectNewVerse);
  CurrentProject.OnAlterarVersiculo := @VerseChanged;
  CurrentProject.OnSintagmaClick := @SyntagmClick;
  CurrentProject.PalavrasComStrongEmNegrito := FBoldStrongsEnabled;
  CurrentProject.MostrarQtdStrongs := FStrongsCountMode;
  CurrentProject.AutoSave := FAutoSaveEnabled;

  CurrentProject.OpenProject(filename);
  CurrentProject.ApplyWorkflowFallbacks(FSuggestionMode,
    FLegacyAutomaticSuggestion, FDefaultVerseMode);
  CurrentProject.PopupTrigger := FPopupTrigger;
  MenuItem22.Checked := CurrentProject.AutomaticAssociationSuggestion;

  FChapterView.Project := CurrentProject;
  FChapterView.Enabled := true;
  if CurrentProject.ChapterViewMode = cvmTranslationMemory then
    ChapterViewTabCtrl.TabIndex := CHAPTER_VIEW_MEMORY_TAB_INDEX
  else
    ChapterViewTabCtrl.TabIndex := Ord(CurrentProject.ChapterViewText);
  ChapterViewTabCtrlChange(ChapterViewTabCtrl);

  ActionSaveProject.Enabled := true;
  ActionSaveProjectAsUpdate(ActionSaveProjectAs);
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
  UpdateCommandUI;
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

