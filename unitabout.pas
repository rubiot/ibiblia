unit unitabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Clipbrd, Math, fileinfo, LCLIntf, LCLType, unitskins;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    Label1: TLabel;
    LabelVersion: TLabel;
    Label2: TLabel;
    LabelRubioURL: TLabel;
    LabelContributor: TLabel;
    Label3: TLabel;
    ButtonUserGuide: TButton;
    ButtonChangelog: TButton;
    ButtonClose: TButton;
    procedure ButtonUserGuideClick(Sender: TObject);
    procedure ButtonChangelogClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure LabelContributorClick(Sender: TObject);
    procedure LabelRubioURLClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySkin(const ASkin: TSkinColors);
  end;

  { TFormChangelog }

  TFormChangelog = class(TForm)
    lblTitle: TLabel;
    memoChangelog: TMemo;
    btnOK: TButton;
    procedure FormShow(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySkin(const ASkin: TSkinColors);
  end;

  { TFormUserGuide }

  TFormUserGuide = class(TForm)
    pnlHeader: TPanel;
    lblHeaderTitle: TLabel;
    lblHeaderSub: TLabel;
    pnlBody: TPanel;
    pnlSidebar: TPanel;
    pbSidebar: TPaintBox;
    pnlContent: TPanel;
    pnlIllust: TPanel;
    pbIllust: TPaintBox;
    memoGuide: TMemo;
    pnlFooter: TPanel;
    btnPrev: TButton;
    btnNext: TButton;
    btnCopy: TButton;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PbSidebarPaint(Sender: TObject);
    procedure PbSidebarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PbSidebarMouseLeave(Sender: TObject);
    procedure PbSidebarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PbIllustPaint(Sender: TObject);
    procedure BtnPrevClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
  private
    FSelectedTopic: Integer;
    FHotTopic: Integer;
    FLang: string;
    function GetTopicNavTitle(Idx: Integer): string;
    function GetTopicVersionBadge(Idx: Integer): string;
    function GetTopicHeaderTitle(Idx: Integer): string;
    function GetTopicGuideText(Idx: Integer): string;
    procedure SelectTopic(Idx: Integer);
    procedure LayoutFooterButtons;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySkin(const ASkin: TSkinColors);
    procedure UpdateLanguage(const ALang: string);
  end;

procedure ShowChangelog(AOwner: TComponent);
procedure ShowUserGuide(AOwner: TComponent);

var
  FormAbout: TFormAbout;
  FormUserGuide: TFormUserGuide;

implementation

{$R *.lfm}

const
  GUIDE_TOPIC_COUNT = 12;

  CHANGELOG_TEXT =
    
  'Version 1.1.0 (64-bit Edition) - 2026-11-13' + LineEnding +
  '- Native 64-bit Windows (x86_64-win64) build with 64-bit SQLite3 and PCRE.' + LineEnding +
  '- Instant dictionary popup update on word hover move and mouse-wheel word scroll.' + LineEnding +
  '- Enabled mouse-wheel scrolling and Ctrl+Wheel zoom inside the dictionary popup.' + LineEnding +
  '- Underline-only feedback on equivalent Strong''s tags and Destination-to-Source highlight.' + LineEnding +
  '- Restored Ctrl+Shift multi-word association with auto-advance on key release.' + LineEnding +
  LineEnding +
'iBiblia Version 1.1.0.019 (64-bit Windows & Linux Edition)' + sLineBreak +
    'Release Date: 2026-11-10' + sLineBreak +
    '==========================================================' + sLineBreak +
    '' + sLineBreak +
    '1.1.0.019 (2026-11-10)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Fixed for-loop counter variable in TDirectWriteSyntagmView.HitTestTextOffset (directwrite_syntagm_view.pas line 2044) on Linux by iterating with local variable CharIdx instead of the out parameter UTF16Offset.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-10)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Fixed UIDefaultFontName and UIMonoFontName implementation placement in ui_style.pas and added ui_style to implementation uses of theword_translator_note_preview.pas.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-10)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Fixed Linux FPC 3.2.2 compilation in unitskins.pas (added LCLType to interface uses clause for LPARAM, LRESULT, COLORREF, and HBRUSH, and made GetPopupMenuBgColor available on Linux), resolved THandle deprecation warning in twAutomate.pas, and set "Release Linux" (OS=linux, CPU=x86_64) as the default active build mode in iBiblia.lpi.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-09)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Fixed Linux FPC 3.2.2 compilation in pcre_dll.pas (Delphi-mode procedural pointer assignments PPointer(@fn_pcre_...)^ := GetProcedureAddress and zero-arg function pointer nil checks), twAutomate.pas (PPointer(@fn...)^ := GetProcedureAddress), FormDictionaryPopup.pas (escaped RTF backslash literals in RenderRtfToRichMemoLinux), and ai_credential_store.pas (top-of-implementation uses IniFiles, BaseUnix), and bundled all required Windows (.dll) and Linux (.so) runtime libraries.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-06)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Completed 100% Linux feature audit and parity: added session-scoped Fontconfig (libfontconfig.so.1) custom font loading (LoadTheWordFonts), Wine theword.exe /proc & config.ini detection, and X11 window title verse synchronization in twAutomate.pas; AppImage/Electron /proc/cmdline detection in tanglaw_sync.pas; FindLCLWindow z-order occlusion checks for hover chevrons/strips and grid/SynEdit skinning in unitskins.pas; and UIDefaultFontName resolution in chapterview.pas.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-06)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Audited and hardened all Linux x86_64 features: automatic libsqlite3.so.0 runtime fallback in sqlite_database_copy.pas, native Linux RTF-to-TRichMemo renderer with Unicode/colortbl support and Ctrl+MouseWheel zoom in FormDictionaryPopup.pas, BiDi RTL line alignment + word-cluster wrapping + line-aware HitTestTextOffset in directwrite_syntagm_view.pas, cross-platform backslash/slash dictionary path resolution in Project.pas and dictionary_manager.pas, UIDefaultFontName/UIMonoFontName in ui_style.pas, TTS child process reaping in unittexttospeech.pas, and writable $XDG_CONFIG_HOME/iBiblia fallback for iBiblia.ini and iBiblia.models.json.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-06)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Added native Linux x86_64 port alongside 64-bit Windows: dynamic libpcre.so.3/libpcre.so.1 loader in pcre_dll.pas, LCL Canvas span layout/rendering and hit-testing in directwrite_syntagm_view.pas, Linux TTS support (espeak-ng/espeak/spd-say) in unittexttospeech.pas, native Linux Timotheos/Tanglaw synchronization in tanglaw_sync.pas, Linux OpenSSL 3/1.1 and CA bundle discovery in tls_runtime.pas, persistent XDG credential store in ai_credential_store.pas, fpfsync export publishing in export_text_publisher.pas, and Release Linux build mode in iBiblia.lpi + build-linux.sh.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-05)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Fixed recursive height overflow ("Size range overflow in .SendMoveSizeMessages: Height=99995") when clicking verse navigation chevrons or exiting iBiblia by preventing TDirectWriteSyntagmView.GetContentHeight from returning the outer control Height when FDocument is nil and guarding TVerse.UpdateVerseViewHeight against re-entrancy and null documents.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-05)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Fixed verse navigation chevron arrows (< >) disappearing or misaligning when vertical scrolling is active in a verse panel: parented TVerseChevrons directly to FVerseViewControl so it scrolls natively with the verse text, included wrapped chevron height and FBottomInset in FVerseViewControl.Height so the last line and chevrons can always scroll above the bottom strip, and used Box.Parent.ClientToScreen(Point(Box.Left, Box.Top)) in UpdateVersePanelStrips so scroll offsets never shift the hover hit-test rectangle.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-05)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Fixed dictionary popup splitter height persistence (FMorphologyPaneHeight in FormDictionaryPopup.pas) in both docked and floating modes by removing forced splitter centering and persisting user splitter adjustments immediately on resize/move/hide.' + sLineBreak +
    '- Clamped the right-edge docked dictionary popup vertical position and height so its top edge never rises above the top edge of the iBiblia main window and its height never exceeds the main window height.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-05)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Declared SDockPopupRightMenu resourcestring in formprincipal.pas for the View -> Dock Word Definition Popup to Right Edge menu toggle.' + sLineBreak +
    sLineBreak +
    '1.1.0.019 (2026-11-05)' + sLineBreak +
    '----------------------' + sLineBreak +
    '- Added a toggle option ("Dock word definition popup to the right edge of the main window" in View menu and Application Preferences, persisted in iBiblia.ini as popup.dock.right) that docks TFrmDictionaryPopup directly outside the right edge of the main window and smoothly tracks the vertical height of the mouse pointer.' + sLineBreak +
    '- Removed the redundant bold "Reference: Book ch:vs" label above the Bible tree in the state header bar and shifted the status selector and stats labels (Associations, Selection, and Method) to the left.' + sLineBreak +
    sLineBreak +
  '1.1.0.019 (2026-11-04)' + sLineBreak +
    '	* [C] Fixed Dictionary Popup size persistence across application restarts (FormDictionaryPopup.pas).' + sLineBreak +
    '	* [C] Restored Module Properties memo height (at least 5 lines high, overlaying the Bible panel instead of being compressed below DirectWriteView) when Properties is checked (formprincipal.pas).' + sLineBreak +
    '	* [C] Made individual Bible bottom toolbars (Name caption, status radio circles, Edit, and Properties) appear on mouse hover in sync with the per-panel chevron navigation arrows (formprincipal.pas).' + sLineBreak +
    '	* [N] Added 3 top bar menu icon size options (Small 16px, Medium 22px, Large 28px) in View > Toolbar icon size and Tools > Preferences (ui_icons.pas, FormApplicationPreferences.pas, formprincipal.pas).' + sLineBreak +
    '	* [C] Fixed skin theme rendering on the dropdown arrow sections of split toolbar buttons (ToolButton6 and FTheWordToolButton) across all themes (unitskins.pas).' + sLineBreak +
    '	* [C] Fixed Main Bible Tree status synchronization with the Destination panel 4 radio buttons status during word association, AI/statistical suggestions, and verse edits (Project.pas, formprincipal.pas).' + sLineBreak +
    '	* [C] Moved the realtime Saved / Unsaved changes status indicator to the bottom-left corner of the bottom status bar directly below the status summary box (formprincipal.pas, unitskins.pas).' + sLineBreak +
    '	* [N] Updated the New Features Guide (Shift+F1) with dedicated illustrated sections for all iBiblia 1.1.0.0 features (unitabout.pas, FormNewFeaturesGuide.pas).' + sLineBreak +
    '' + sLineBreak +
    '1.1.0.0 (2026-11-03)' + sLineBreak +
    '	* [N] Merged iBiblia 1.1.0 with the 1.0.1.055 64-bit edition features.' + sLineBreak +
    '	* [N] Visual WYSIWYG and Raw ONT verse editor with titles, added words, Words of Jesus, OT quotations, font colors, relative font sizes, and rich translator notes with hover previews.' + sLineBreak +
    '	* [N] Bidirectional rendering and Unicode selection for mixed Hebrew, Greek, and translated text.' + sLineBreak +
    '	* [N] Translation memory comparison with highlighted verse occurrences and hover previews.' + sLineBreak +
    '	* [N] Guided project creation wizard, customizable verse statuses, contextual guide, and Save As (.bib copy).' + sLineBreak +
    '	* [N] Experimental AI translation and association flows with batch review and Windows local secure credential storage.' + sLineBreak +
    '	* [C] Theme-aware verse editing interface, word selection/highlighting contrast, chapter scrollbar theming, multi-direction dictionary popup positioning, short.title panel captions, and Project Settings Default Dictionaries toggle.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.055 (2026-11-03)' + sLineBreak +
    '	* [C] Fixed Access Violation on application exit in TGerSugestoes.Destruir (sugestao.pas) by detaching FConnTmp.Transaction := nil and FConnVerbos.Transaction := nil before freeing the TSQLTransaction instances (preventing TDatabase.SetTransaction from dereferencing freed transaction objects) and safely closing/unpreparing/detaching all 15 TSQLQuery instances.' + sLineBreak +
    '	* [C] Fixed Access Violation on application exit in TFrmApplyPatch.FormDestroy (formapplypatch.pas) and TVersiculo.Destruir (versiculo.pas) by adding destructor Destroy; override; to TVersiculo, TMemoVersiculo, and TProjeto, and clearing FPanel.OnResize before freeing FSintagmas and child panels so destroying ScrollBoxSrcText/ScrollBoxDstText never triggers OnResize on freed TVersiculo instances.' + sLineBreak +
    '	* [C] Fixed use-after-free in TFrmPrincipal.ActionFecharProjetoExecute (formprincipal.pas) and TChapterView (chapterview.pas) by detaching FChapterView.Project := nil before destroying ProjetoAtual, removing premature project unsubscription from TChapterView.SetEnabled, and detaching FRawMemo.Parent := nil before freeing FRawMemo.' + sLineBreak +
    '	* [C] Hardened TProjeto.Fechar, TProjeto.Destruir, and TProjeto.LiberarObjetoQuery (projeto.pas), stopped TwSyncThread on exit, guarded HandleAllocated before reading .Handle in TFrmPrincipal.FormDestroy, TFrmExportarProjeto.FormDestroy, and Unhook*Control, and added WM_NCDESTROY/WM_DESTROY auto-unhooking and ApplicationShuttingDown guards across all Win32 SubclassProcs (unitskins.pas, formprincipal.pas, formexportar.pas).' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.054 (2026-10-31)' + sLineBreak +
    '	* [C] Fixed empty even-numbered verses when loading theWord (.ont/.ot/.nt) or USFM (.usfm/.sfm) modules with CR-CR-LF (\r\r\n / $0D$0D$0A) line endings (such as LXX Brenton+, LXX Rahlfs+, and SweteLXX+) by normalizing #13#13 to #13 in LoadModuleLinesNormalized (projeto.pas) and LoadUSFMFile/ImportUSFMBible (myswordmodule.pas), and sanitizing CR/LF in ExportTheWordBible.' + sLineBreak +
    '	* [C] Updated TFrmEscolherVerseRules.AddVerseRule (formverserules.pas) to leave display/hyperlink verse.rule entries (_ORIGWORD_, _NOLINK_, <NB, <ST, <sup>, <font) unchecked by default on import, and updated TONTParser.ParseLine (ontparser.pas) to treat <NB...<Nb> and <ST...<St> blocks as metadata.' + sLineBreak +
    '	* [C] Updated EnsureTanglawSyncBridge (twAutomate.pas) to auto-patch src/lib/theword.js and src/lib/modprops.js inside installed Tanglaw app.asar archives so CR-CR-LF (.ot/.ont/.nt) modules load with zero empty even verses.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.053 (2026-10-31)' + sLineBreak +
    '	* [C] Added LazUTF8 to the interface uses clause of projeto.pas for UTF8LowerCase resolution and converted TGerSugestoes.Destruir to a procedure invoked by destructor Destroy; override; in sugestao.pas.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.052 (2026-10-31)' + sLineBreak +
    '	* [C] Fixed association persistence across application restarts for Tokenizer V1 projects (such as Gospels-Ed.bib) by loading TTokenizerFactory.PreferredVersion immediately after FTblInfo.Open before HealMisplacedPairsInDatabase runs.' + sLineBreak +
    '	* [C] Restricted HealMisplacedPairsInDatabase so it never clears valid associations on startup, added lossless V1/V2 tokenizer pair remapping in PosRolagemVersiculo, and added automatic recovery of wiped verses from association_memory_occurrences.' + sLineBreak +
    '	* [C] Fixed SQLite multi-connection lock contention between TGerSugestoes and TSqlite3Dataset by closing all SELECT cursors after reads, flushing TGerSugestoes transactions prior to ApplyUpdates, overriding TGerSugestoes.Destroy, and executing SalvarPares before SalvarAssociacoes.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.051 (2026-10-31):' + sLineBreak +
    ' * Removed word/dictionary lookup synchronization between iBiblia and Tanglaw Bible while preserving full bidirectional verse synchronization ("Sync Tanglaw to iBiblia verse" and "Sync iBiblia to Tanglaw verse") and all Tanglaw UI size, window bounds, dock splitter, and panel zoom persistence fixes.' + sLineBreak +
    ' * Reverted QuandoPalavraClicada in formprincipal.pas to sync word/dictionary lookups exclusively with theWord, and removed LookupExternalWordOrStrong / SyncTanglawWordLookup across projeto.pas, formprincipal.pas, twsyncthread.pas, and twAutomate.pas.' + sLineBreak +
    ' * Updated EnsureTanglawSyncBridge in twAutomate.pas and Tanglaw-Bible-0.5.5-Windows-Installer.zip so Tanglaw syncs verses only and persists all UI/window sizes across restarts.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.050 (2026-10-31):' + sLineBreak +
    ' * Fixed Tanglaw Bible UI size, window bounds, dock layout splitters, and per-panel zoom persistence across application restarts.' + sLineBreak +
    ' * Added windowBounds & guideWindowBounds persistence (width, height, x, y, maximized, fullScreen with multi-monitor workArea clamping), atomic saveSettings() writes, and deep-merged settings:set in Tanglaw src/main.js.' + sLineBreak +
    ' * Updated Tanglaw options.js, app.js, dock.js, modules.js, workbench.css, style.css, and index.html so Menus & Panels UI size (--ui-size up to 22 px, including Ctrl+MouseWheel over toolbars/captions), Bible view zoom, all 10 docked panel zooms, and dock splitter drags persist across restarts.' + sLineBreak +
    ' * Updated EnsureTanglawSyncBridge and SyncTanglawVerse in twAutomate.pas to auto-upgrade installed Tanglaw app.asar archives with UI size persistence and eliminate concurrent settings.json file-write races while Tanglaw is running.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.049 (2026-10-31):' + sLineBreak +
    ' * Added bidirectional Tanglaw Bible verse and word/dictionary lookup synchronization under Options ("Sync Tanglaw to iBiblia verse" and "Sync iBiblia to Tanglaw verse") across twAutomate.pas, twsyncthread.pas, projeto.pas, formprincipal.pas, formprincipal.lfm, and languages/*.po.' + sLineBreak +
    ' * Added SyncTanglawVerse, SyncTanglawWordLookup, GetTanglawCurrentRef, MapIBibliaBookToTanglaw, TanglawBookIDToName, and EnsureTanglawSyncBridge in twAutomate.pas, supporting all 66 canonical books and 21 Apocrypha/Deuterocanonical books (including Daniel 13-14 and Psalms 151 mapping).' + sLineBreak +
    ' * Added ResolveTanglawRefToProjectRef and LookupExternalWordOrStrong in projeto.pas so clicking/hovering words in iBiblia opens their Strong''s, Morphology, Lemma, and Word Dictionary entries in Tanglaw''s Lookup panel, and clicking words/tags in Tanglaw highlights the matching word in iBiblia and opens iBiblia''s dictionary popup.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.048 (2026-10-30):' + sLineBreak +
    ' * Updated all changelogs (Histórico.txt, historico.txt, Help > About > Changelog, Help > New Features User Guide, and README.md) to document USFM (.usfm, .sfm) & USFX (.usfx, .xml) Bible import/export, e-Sword Apocrypha OT books & Deuterocanonical chapters in the Bible Books Tree, non-encrypted e-Sword (.bblx, .bbli, .bbl) import/export, auto-save on selecting "Associated", and Context Panel popup dictionary inheritance.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.047 (2026-10-30):' + sLineBreak +
    ' * Added full import and export support for USFM 2.x/3.0 (.usfm, .sfm) and USFX XML (.usfx, .xml) Bible files in myswordmodule.pas (LoadUSFMBibleWithApocrypha, LoadUSFXBibleWithApocrypha, ExportUSFMBible, ExportUSFXBible, ConvertONTToUSFM, ConvertONTToUSFX, USFMBookCodeToID, BookIDToUSFMCode), modeled on Simple Bible Reader (USFXBibleFormat / TheWordBibleFormat) and the eBible.org USFX specification.' + sLineBreak +
    ' * Added automatic multi-file USFM directory detection: selecting any single .usfm/.sfm book file in a folder automatically loads all sibling .usfm/.sfm book files into the project across all 66 canonical books (GEN-REV) and 21 Apocrypha / Deuterocanonical 3-letter book codes (1ES, 2ES, TOB, JDT, 1MA-4MA, MAN, WIS, SIR, BAR, LJE, S3Y, SUS, BEL, ESG, PS2, PSS, ODE, LAO, plus DAG Daniel 1-14).' + sLineBreak +
    ' * Preserves Strongs numbers, morphology tags, and lemmas (\w word|lemma="..." strong="..." x-morph="..."\w* and <w s="..." m="..." l="...">), Words of Jesus (\wj / <wj>), added/italic words (\add / <add>), bold (\bd / <bd>), section headings (\s1 / <s>), and footnotes (\f / <f>).' + sLineBreak +
    ' * Integrated USFM and USFX file filters and import/export handlers across formnovoprojeto.pas, formpropprojeto.pas, formexportar.pas, and projeto.pas.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.046 (2026-10-29):' + sLineBreak +
    ' * Fixed missing leading single quote on line 114 of CHANGELOG_TEXT in unitabout.pas and added Stage 17 Pascal string literal quote audit to audit_full_build.py.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.045 (2026-10-29):' + sLineBreak +
    ' * Added full support for Apocrypha / Deuterocanonical Old Testament books (Book >= 67: 1 Esdras, 2 Esdras, Tobit, Judith, 1-4 Maccabees, Prayer of Manasseh, Wisdom, Sirach, Baruch, Letter of Jeremiah, Prayer of Azariah, Susanna, Bel and the Dragon, Psalm 151, Psalms of Solomon, Odes, Laodiceans) and extra Deuterocanonical chapters in canonical OT books (Daniel 13-14, Psalms 151, Esther 11-16).' + sLineBreak +
    ' * Added LoadESwordBibleWithApocrypha and IdentifyApocryphaBookName in myswordmodule.pas to distinguish 12-book (67..78) and 16/19-book (67..86) e-Sword Apocrypha numbering schemes and backfill the 57 canonical KJV verses omitted by kjva.bbli.' + sLineBreak +
    ' * Added RebuildBookStructure, EnsureExtraVersesInDatabase, SyncESwordApocryphaIfNeeded, GetBookName, GetBookChapterCount, GetChapterVerseCount, and GetAdjacentChapter in projeto.pas so Apocrypha OT books appear right after Malachi at the end of the Old Testament in the Bible Books Tree (FArvore), Context Panel (chapterview.pas), and Project Search.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.044 (2026-10-29):' + sLineBreak +
    ' * Fixed TChapterView.GetWordInfoAt in chapterview.pas to use KControls 1.7.3 TKMemo.PointToIndex and Blocks.IndexToBlockIndex instead of PointToBlock/BlockToRect.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.043 (2026-10-29):' + sLineBreak +
    ' * Fixed FPC 3.2.2 SQLdb TSQLQuery preparation in TESwordModule.Create (myswordmodule.pas) by removing non-existent ParamsCheck property and initializing the RTF state stack.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.042 (2026-10-29):' + sLineBreak +
    ' * Added full import and export support for all non-encrypted e-Sword Bible files (.bblx RTF, .bbli HTML, and SQLite .bbl).' + sLineBreak +
    ' * Added TESwordModule in myswordmodule.pas with complete e-Sword RTF-to-ONT, HTML-to-ONT, ONT-to-RTF (.bblx Version 2), and ONT-to-HTML (.bbli Version 4) conversion covering Strong''s tags, TVM/Robinson morphology, Words of Christ in red, italics, bold, underline, headings, footnotes, and Unicode escapes.' + sLineBreak +
    ' * Integrated e-Sword Bible import into New Project (formnovoprojeto.pas) and Project Properties (formpropprojeto.pas) with automatic OT/NT/Whole-Bible scope detection and encrypted-module detection.' + sLineBreak +
    ' * Integrated e-Sword .bblx and .bbli export into File > Export (formexportar.pas) for both Tagged Destination and Interlinear Bibles, as well as each panel''s right-click "Export as Bible module..." menu (projeto.pas).' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.041 (2026-10-29):' + sLineBreak +
    ' * Added automatic project save trigger when selecting the "Associated" radio button (via RadioGroupStatus, the Destination verse status bar circle, or keyboard shortcut ''4'').' + sLineBreak +
    ' * Enabled the Context Panel (TChapterView) to inherit and display the popup dictionary (Strong''s, Morphology, Translation Memory, and English Dictionary fallback) for associated/linked words between the Source and Destination panels (including multi-word phrase associations and Interlinear/Intralinear views).' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.040 (64-bit Edition) - 2026-10-29' + sLineBreak +
    '- Fixed "Sync theWord to iBiblia verse" (`WM_COPYDATA` verse & dictionary lookup broadcast on 64-bit Windows):' + sLineBreak +
    '  * Resolved 64-bit struct packing collision where `LCLType.TCopyDataStruct` (`packed record`, 20 bytes on Win64 with `lpData` at offset 12) shadowed `Windows.TCopyDataStruct` (24 bytes on Win64 with 4-byte C-ABI alignment padding and `lpData` at offset 16), causing the 64-bit Windows kernel to reject `WM_COPYDATA` messages sent to theWord.' + sLineBreak +
    '  * Added explicit `TTWCopyDataStruct` with `{$IFDEF CPU64} ReservedPadding: DWORD; {$ENDIF}` and `TWAutomateUtils.BroadcastCopyDataToTheWord` in `twAutomate.pas` + `formprincipal.pas`.' + sLineBreak +
    '  * Prioritized `TW_MAINFORM_CLASSNAME` (`.UnicodeClass`) in `TWAutomateUtils.IsTwRunning` and added `FNavigatingVerse` re-entrancy guards in `ActionVersoPrimeiroExecute` and `ActionVersoAnteriorFiltradoExecute`.' + sLineBreak +
    '  * Added Stage 12 static verification check in `audit_full_build.py` to prevent `LCLType.TCopyDataStruct` shadowing.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.038 (64-bit Edition) - 2026-10-28' + sLineBreak +
    '- Integrated upstream v1.1.0 reliability & workflow improvements while preserving all v1.0.0.70–v1.0.1.034 features:' + sLineBreak +
    '  * Export Bible (.ont/.ot/.nt) and Concordance (.dct.twm) modules while open in theWord via staged publishing + shared-stream overwrite fallback (`PublishStagedFile`).' + sLineBreak +
    '  * Implemented safe Project "Save As..." (`File > Save as...` / `Ctrl+Shift+S` / Toolbar button) with transactional staging and MRU update.' + sLineBreak +
    '  * Added per-project remembered export paths for Tagged Destination, Interlinear, Analytical/Synthetic Concordance (`.dct.twm`), and all 4 Bible panels.' + sLineBreak +
    '  * Added apostrophe variant normalization (`NormalizeApostrophes`) across word associations, statistical suggestions, and smart reload word-diff checks.' + sLineBreak +
    '  * Highlighted the active verse''s current translation (`[current]`) inside the Translation Memory popup list.' + sLineBreak +
    '  * Added `TMemoVersiculo.PruneEmptyFormattingTags` to automatically remove empty wrapper tags after text deletion in the verse editor.' + sLineBreak +
    '  * Updated `TTheWordDictionary` (`.dct.twm`) to store uncompressed `BLOB` `content.data` and required 2026 Book Module Developer Guide metadata.' + sLineBreak +
    '  * Fixed reversed `Pos()` arguments in `TProjeto.ExportarTextoDestinoComStrongs` and fixed `FQryTranslationMemory` / `TMySwordModule` memory leaks.' + sLineBreak +
    '' + sLineBreak +
    'v1.0.1.034 (2026-10-28)' + sLineBreak +
    '  * Context Panel: Unassociated Destination text words automatically render in italics (<FI>...<Fi>) upon file save (Ctrl+S / File > Save).' + sLineBreak +
    '  * User Guide & README: Added visual illustration and documentation for Context Panel unassociated Destination words in italics on save.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.033 (2026-10-28):' + sLineBreak +
    ' * Fixed "Sync theWord to iBiblia verse": restored FArvore.OnChange in TProjeto.FiltrarArvore(-1), suppressed re-entrant filter/status events during verse loading, restored all 4 official theWord 6/7 window classes in TWAutomateUtils.IsTwRunning with strict EnumWindows filtering, and fixed 64-bit buffer bounds in SyncTheWordDict.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.032 (64-bit Edition) - 2026-10-27 20:47' + sLineBreak +
    '* Fixed TFontStyles set assignment in DrawBadge (unitabout.pas line 1130: replaced IfThen(BoldFont, [fsBold], []) with if BoldFont then C.Font.Style := [fsBold] else C.Font.Style := []).' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.031 (64-bit Edition) - 2026-10-27 20:26' + sLineBreak +
    '* Added dedicated floating "New Features User Guide" window with interactive visual illustrations under Help > New Features User Guide... (F1) above About... (unitabout.pas, formprincipal.pas, formprincipal.lfm):' + sLineBreak +
    '  - Collates all new features (excluding bug fixes) introduced since v1.0.0.71 into 7 structured feature topics plus a complete combined reference:' + sLineBreak +
    '    1. Mouse Wheel Source Word Selection & Ctrl+Shift Multi-Word Association' + sLineBreak +
    '    2. Southeast Cursor Dictionary Popup & 4-Slot Default Dictionaries (including English Word .dct.twm & compressed=1 support)' + sLineBreak +
    '    3. In-Verse Controls: Hover-Only Navigation Chevrons (< >), 4-Color Status Circles & Edit/Properties Checkboxes' + sLineBreak +
    '    4. Shared Master Formatting Ribbon, Floating Selection Mini-Toolbar & Live Context Panel Preview' + sLineBreak +
    '    5. Real-Time Text-to-Speech (TTS) Hover Reader (Greek, Hebrew & English OneCore/SAPI5 voices)' + sLineBreak +
    '    6. Dark Mode UI & 7 theWord Color Themes (Classic, theWord Dark, Night, Sepia, Slate, Solarized Dark, High Contrast)' + sLineBreak +
    '    7. Complete Bible Module Export (.ont/.ot/.nt), Smart Reload "Needs Review" Diff & Concurrent Bidirectional theWord 7 Sync' + sLineBreak +
    '    8. Complete Quick-Reference Summary of All New Features & Keyboard/Mouse Shortcuts' + sLineBreak +
    '  - Features a custom-drawn visual diagram canvas (TPaintBox) at the top of each topic illustrating how to use the feature, full theme synchronization across all 7 skins, Copy Guide button, and modeless floating behavior so users can keep the guide open while working in iBiblia.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.030 (64-bit Edition) - 2026-10-27 20:06' + sLineBreak +
    '* Fixed compilation error in versiculo.pas ("Identifier not found GetKeyState" at lines 2153-2154) by adding LCLIntf to the uses clause and using Shift * [ssCtrl, ssShift, ssAlt] in TVersiculo.OnMouseWheel.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.029 (64-bit Edition) - 2026-10-27 19:55' + sLineBreak +
    '* Restored Ctrl+Shift multi-word selection for association tasks alongside mouse wheel word selection (syntagm.pas, versiculo.pas, projeto.pas):' + sLineBreak +
    '  - Removed the selection-clearing check in TSyntagm.DoOnLeftClick that prevented selecting multiple words simultaneously while holding Ctrl+Shift.' + sLineBreak +
    '  - Added modifier key guards (Ctrl, Shift, Alt) in TVersiculo.OnMouseWheel and TProjeto.HandleMouseWheelForSourceWords so accidental scroll wheel movement while holding Ctrl+Shift never clears active multi-word selections.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.028 (64-bit Edition) - 2026-10-27 19:36' + sLineBreak +
    '* Added mouse wheel / scroll word selection in the Source text panel (versiculo.pas, syntagm.pas, projeto.pas, chapterview.pas, formprincipal.pas):' + sLineBreak +
    '  - Scrolling up (WheelDelta > 0) selects words in the Source text to the left one by one, and scrolling down (WheelDelta < 0) selects words in the Source text to the right one by one.' + sLineBreak +
    '  - Works seamlessly whenever the mouse cursor is hovering inside either the Source text panel (ScrollBoxSrcVerse) or the Destination text panel (ScrollBoxDstVerse), regardless of which control currently holds keyboard focus.' + sLineBreak +
    '  - Automatically scrolls the selected Source word into view, highlights any associated Destination word pairs, updates Strong''s highlights across all panels, and updates the active dictionary popup.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.027 (64-bit Edition) - 2026-10-27 16:38' + sLineBreak +
    '* Fixed export errors ("Invalid syntagm index: 18" on Genesis 1:1 reported under the last viewed reference "1 Kings 1:2") and added automatic database self-healing (versiculo.pas, projeto.pas, formprincipal.pas):' + sLineBreak +
    '  - Root Cause Identified: In export-errors.txt, row ''1,1,1'' (Genesis 1:1, 21 tokens) contained the pare_pares string of Genesis 41:32 (''1,41,32'', 50 tokens up to <par a="49" b="60">), which was written into ''1,1,1'' when FTblPares moved to the first row without updating FAVersiculo, and FormattedReference printed ''1 Kings 1:2'' because FReference was not updated while FScrollEventsEnabled = False during export.' + sLineBreak +
    '  - Added TVersiculo.AreParesValidForVerse (versiculo.pas) and TProjeto.HealMisplacedPairsInDatabase (projeto.pas), executed automatically on TProjeto.Abrir and before all Export operations, to detect any misplaced pare_pares on ''1,1,1'' (or any row), restore Genesis 41:32''s pairs to ''1,41,32'' if unassociated, and clean the invalid pare_pares from ''1,1,1'' so exports complete with zero errors.' + sLineBreak +
    '  - Added FLoadedVerseID verification in PreRolagemVersiculo, SalvarPares, and SalvarTexto, reference-counted FScrollingSessionDepth in StartScrollingSession/FinishScrollingSession, FArvore.OnChange suppression in SelectTreeNode/FiltrarArvore, and ScrollEventsEnabled guard in TFrmPrincipal.SyncToTwRef so associations can never be saved to a mismatched dataset row.' + sLineBreak +
    '  - Updated TProjeto.GetFormattedReference to read FTblPares.FieldByName(''pare_id'') when FScrollEventsEnabled is False so export diagnostics always report the true verse being processed.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.026 (64-bit Edition) - 2026-10-27 12:23' + sLineBreak +
    '* Made verse navigation chevron arrows (‹ ›) appear only while the mouse cursor is hovering inside the Bible panel where they belong (unitskins.pas, versiculo.pas, syntagm.pas, projeto.pas, formprincipal.pas):' + sLineBreak +
    '  - Added IsMouseCursorInControl in unitskins.pas using GetWindowRect, WindowFromPoint, IsChild, and THintWindow tooltip awareness to determine which of the 4 Bible panels (Source, Destination, Reference 1, Reference 2) currently contains the mouse cursor.' + sLineBreak +
    '  - Added FMouseInPanel, UpdateNavHoverState, and an on-demand 40ms FNavHoverTimer in TVersiculo (versiculo.pas), hooked across FPanel, word labels (TSyntagm), FNavPanel, FTranslationPanel, FStatusBarPanel, FTabPanel, and Application.OnIdle (OnAppIdleCheckAutoSelect) so hovering inside any Bible panel immediately shows its chevron arrows while hiding chevron arrows in all other Bible panels.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.025 (64-bit Edition) - 2026-10-27 11:57' + sLineBreak +
    '* Fixed OnKeyUp registration in TFrmPrincipal (formprincipal.pas: replaced Application.AddOnKeyUpHandler/RemoveOnKeyUpHandler with Self.OnKeyUp := @OnAppKeyUp alongside KeyPreview = True, Application.AddOnIdleHandler, and FAutoSelectTimer).' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.024 (64-bit Edition) - 2026-10-27 11:46' + sLineBreak +
    '* Added automatic selection of the next unassociated Source word when Ctrl+Shift keys are released after associating target words (syntagm.pas, versiculo.pas, projeto.pas, formprincipal.pas):' + sLineBreak +
    '  - When holding Ctrl+Shift (or Shift / Ctrl+Right-Click) and clicking/highlighting one or more target words in the Destination text to associate them with the selected Source word(s), PendingAutoSelectNext is armed on the paired verses.' + sLineBreak +
    '  - As soon as the Ctrl+Shift modifier keys are released (detected via OnAppKeyUp, OnAppIdleCheckAutoSelect, and a 30ms on-demand FAutoSelectTimer when the mouse is stationary), CompletePendingAutoSelectNext automatically clears the previous selection, highlights/selects the next unassociated word in the Source text, scrolls it into view if needed, and triggers OnClick for theWord synchronization.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.023 (64-bit Edition) - 2026-10-27 11:30' + sLineBreak +
    '* Unified Sepia (Parchment) skin consistency across the entire application (unitskins.pas, formprincipal.pas, formpopup.pas, bibletreeview.pas, and all secondary dialogs):' + sLineBreak +
    '  - Added IsThemedSkin (CurrentSkin.IsDark or CurrentSkin.Id = skSepia) so the Main Menu Bar, dropdown/popup menus, toolbar, comboboxes, checkboxes, buttons, groupboxes, tab controls, non-client borders, and secondary dialogs all render using Sepia''s warm parchment palette (#F4EEDB / #E2D7C3 / #C9BC9C / #2D241E).' + sLineBreak +
    '  - Added DrawThemedPopupMenuItem in unitskins.pas to custom-draw dropdown and context popup menu items in Sepia (warm #F4EEDB background, #E2D7C3 left gutter, #C9BC9C border & separators, #D9C289 hover bar, and #8A4E2C checkmarks/radio bullets), and updated ApplyDarkModeToWindow to call fnSetPreferredAppMode(pamForceLight) + fnFlushMenuThemes when switching out of dark skins.' + sLineBreak +
    '  - Added OnContextTabButtonPaint in formprincipal.pas so the Source | Destination | Ref 1 | Ref 2 context tab buttons and filter navigation buttons render with Sepia HighlightBg (#D9C289), VerseNumColor (#8A4E2C) top accent bar, and dark sepia text (#2D241E) instead of dark-mode charcoal.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.022 (64-bit Edition) - 2026-10-27 11:15' + sLineBreak +
    '* Fixed white border lines around the 4 Bible panels (ScrollBoxSrcVerse, ScrollBoxDstVerse, ScrollBoxRef1Verse, ScrollBoxRef2Verse), TreeView1, container panels, and splitters across all themed skins (unitskins.pas, formprincipal.pas, formprincipal.lfm, versiculo.pas):' + sLineBreak +
    '  - Added PaintControlNCBorder in unitskins.pas and hooked WM_NCPAINT, WM_PAINT, WM_SIZE, WM_WINDOWPOSCHANGED, and scroll messages in ScrollBoxSubclassProc and TreeViewSubclassProc to overpaint the 2px WS_EX_CLIENTEDGE non-client border (1px outer CurrentSkin.PanelBorder + 1px inner CurrentSkin.WindowBg / CurrentSkin.TreeBg) and the 17x17 scrollbar dead-space corner square.' + sLineBreak +
    '  - Removed bvRaised 3D white bevels (BevelOuter := bvNone) from Panel1, CenterPanel, and BottomPanel in formprincipal.lfm and formprincipal.pas, and aligned all 4 Bible panels flush inside CenterPanel.' + sLineBreak +
    '  - Added OnSplitterPaint in formprincipal.pas so Splitter1..Splitter6 render with CurrentSkin.PanelBorder and subtle CurrentSkin.WindowTextDim grip dots instead of white UxTheme gripper dots.' + sLineBreak +
    '  - Updated SetControlThemeMode to always invalidate RDW_FRAME when switching between dark skins so non-client borders immediately adopt the new skin''s PanelBorder and WindowBg colors.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.021 (64-bit Edition) - 2026-10-27 08:06' + sLineBreak +
    '* Fixed HPEN type identifier error in DrawDarkMenuSeparator (unitskins.pas line 905: replaced undeclared HPEN with THandle) and added explicit PtrUInt pointer casts for lParam struct pointers.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.020 (64-bit Edition) - 2026-10-27 07:58' + sLineBreak +
    '* Fixed white outer 3px borders and white separator rows in dropdown and context popup menus in dark mode (unitskins.pas, formprincipal.pas):' + sLineBreak +
    '  - Added thread-scoped WH_CALLWNDPROC / WH_CALLWNDPROCRET menu hooks and #32768 popup menu window subclassing (PopupMenuSubclassProc, PaintPopupMenuBorder) to overpaint the classic MNDrawFullNC 3px non-client border (1px COLOR_3DSHADOW + 2px COLOR_MENU) on WM_PRINT (with SaveDC/RestoreDC around offscreen animation bitmaps), WM_NCPAINT, WM_ERASEBKGND, and WM_PAINT using dark popup background RGB(43, 43, 43) and 1px CurrentSkin.PanelBorder.' + sLineBreak +
    '  - Added DrawDarkMenuSeparator and PreFillDarkMenuItem to fill separator item rows (Caption = ''-'') and submenu item backgrounds with RGB(43, 43, 43) and draw a 1px horizontal divider in CurrentSkin.PanelBorder, eliminating the white/light-grey COLOR_MENU background left un-erased by LCL''s VistaMenuDraw.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.018' + sLineBreak +
    '- Fixed Pascal character set literal in BuscarDefinicaoPalavraInglesNaQuery (projeto.pas lines 2217 and 2219) where backslash character literal was emitted as empty string '''' instead of ''\''.' + sLineBreak +
    '' + sLineBreak +
    '1.0.1.017' + sLineBreak +
    '- Added "English Word / Bible Dictionary (*.dct.twm)" option under Default Dictionaries Settings (saved as default_english_dict / global_english_dict in iBiblia.ini).' + sLineBreak +
    '- Added automatic mouse-hover English dictionary lookup (ObterDefinicaoPalavraIngles / BuscarDefinicaoPalavraInglesNaQuery) across all 4 Bible panels (Source, Destination, Reference 1, Reference 2) for any English word or proper name without a Strong''s number tag, including case-insensitive headword matching, possessive/inflectional stem fallbacks, and compressed/redirect topic support.' + sLineBreak +
    '' + sLineBreak +
    'Versão 1.0.1.016 (2026-10-26)' + sLineBreak +
    '- Fix (unitskins.pas, formpropprojeto.pas): Fixed TTabControl dark mode rendering in Project -> Settings (Properties, TFormPropProjeto) where the internal LCL FNoteBook (TCustomTabControl) and outer TTabControl container previously overlapped double-drawn tab headers and left the entire body area below y=30 painted stark white (#F9F9F9) with unreadable light-grey labels.' + sLineBreak +
    '- Fix (unitskins.pas): Added EditMemoSubclassProc (HookEditControl) to paint a 2px dark border (CurrentSkin.PanelBorder / WindowBg) over the white 3D WS_EX_CLIENTEDGE frame on TEdit, TLabeledEdit, and TMemo controls, and routed TCustomMemo through ''DarkMode_Explorer'' so vertical/horizontal scrollbars render in dark mode.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.015 (64-bit Edition) - 2026-10-26' + sLineBreak +
    '- Restored dictionary popup RTF font colors to match active skin settings (ASkin.NoteText) in AdaptRtfForSkin (formpopup.pas), fixing reddish text caused by mapping \plain / \pard / \par to \cf1 (maroon/red in dictionary color tables) instead of \cf0.' + sLineBreak +
    '- Restored full hover dictionary popup behavior in the Source text box (projeto.pas) so hovering any word in the Source text (highlighted, unassociated, or associated) displays its dictionary popup as usual while keeping the automatic southeast popup inside the Destination panel.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.014 (64-bit Edition) - 2026-10-26' + sLineBreak +
    '- Fixed Hebrew Strong''s dictionary lookup in TProjeto.ObterDefinicaoStrong and TProjeto.BuscarDefinicaoStrongNaQuery so Hebrew Strong''s numbers (H-codes) strictly query the selected Hebrew Lexicon dictionary under Default Dictionaries and never fall back to converting H-codes into Greek G-codes on the Greek EPV dictionary.' + sLineBreak +
    '- Added unpadded (H123), lowercase (h123), zero-padded (H0123, H00123), compound/prefixed (Hc/H1234), sub-entry (H1234a), topics_wordindex, subject headword prefix, and meta=_twmgc_ redirect link resolution for Hebrew and Greek Strong''s dictionary lookups.' + sLineBreak +
    '- Fixed hover popup handling so hovering words in Reference 1 / Reference 2 or associated words displays their dictionary entry normally without interference from the Destination-panel selected-Source popup timer.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.013 (64-bit Edition) - 2026-10-26 13:21' + sLineBreak +
    '- Added rule for hovering associated words inside the Destination text: when the mouse pointer hovers over an already-associated word in the Destination text, the southeast selected-Source popup is temporarily disabled and the normal dictionary popup for that hovered Destination word appears under it as usual; moving off the associated word back into the Destination panel automatically resumes the southeast popup for the selected unassociated Source word.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.012 (64-bit Edition) - 2026-10-26 13:08' + sLineBreak +
    '- Replaced mouse-hover dictionary popup on unassociated Source words with automatic Destination-panel cursor tracking: selecting (left-clicking) an unassociated tagged Source word automatically displays its Strong''s and Morphology dictionary popup at the southeast of the mouse pointer whenever the pointer is inside the Destination text panel, and hides it when the pointer leaves the Destination panel.' + sLineBreak +
    '- Automatic next-word popup progression: right-clicking a translated word in the Destination panel associates the current Source word, auto-selects the next unassociated Source word, and immediately updates the southeast dictionary popup in place with the newly highlighted word''s definitions and morphology codes.' + sLineBreak +
    '- Fixed per-module Strong''s dictionary fallback for Greek Strong''s tags (G-prefix) in Old Testament books (BookID 1..39).' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.008 (64-bit Edition) - 2026-10-25' + sLineBreak +
    '  * Fixed bitmask constant in UpdateMiniBarPosition (memoversiculo.pas lines 363-364) where $FFFF was stripped by shell expansion.' + sLineBreak +
    '  * Includes Layout 3 Shared Master Formatting Ribbon, Floating Selection Mini-Toolbar, Return (<CI>) / Double Return (<CM>) / Shift+Return (<CL>) line-wrapping and single-line flattening, PI/PF step buttons, and live theWord Context Panel preview.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.003 (64-bit Edition) - 2026-10-25' + sLineBreak +
    '  * Fixed Windows.POINT typecast shadowing of Classes.Point(x, y) and PtInRect in memoversiculo.pas on Win64 FPC 3.2.2.' + sLineBreak +
    '  * Includes Layout 3 Shared Master Formatting Ribbon, Floating Selection Mini-Toolbar, Return (<CI>) / Double Return (<CM>) / Shift+Return (<CL>) line-wrapping and single-line flattening, PI/PF step buttons, and live theWord Context Panel preview.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.002 (64-bit Edition) - 2026-10-25' + sLineBreak +
    '  * Added Layout 3 Shared Master Formatting Ribbon & Floating Selection Mini-Toolbar across all 4 verse sections in Edit Mode.' + sLineBreak +
    '  * Added custom vector icons for all theWord paragraph and inline tags (<CI>, <CM>, <CL>, <PI0..7>, <PF0..7>, <center>, <TS1..3>, <_CM>, <FI>, <FR>, <FO>, <FU>, <b>, <i>, <u>, <sup>, <sub>, <RF>, Clear Tags).' + sLineBreak +
    '  * Added Return (<CI> tight paragraph), Double Return (<CM> spaced paragraph), and Shift+Return (<CL> line break) keyboard shortcuts with visual line-wrapping in Edit mode and automatic single-line flattening on save.' + sLineBreak +
    '  * Upgraded right-side Context Panel (TChapterView) to render all theWord paragraph, indentation, heading, and inline formatting tags in real time.' + sLineBreak +
    '' + sLineBreak +
    'Version 1.0.1.000 (64-bit Edition) - 2026-10-24' + sLineBreak +
    '-------------------------------------------------------------------------------' + sLineBreak +
    '* Version Reset to 1.0.1.000 (64-bit Edition):' + sLineBreak +
    '  - Transitioned project versioning to the 1.0.1.xxx series starting at 64-bit Version 1.0.1.000.' + sLineBreak +
    '  - All subsequent builds follow the increment of .001 from here (1.0.1.001, 1.0.1.002, etc.).' + sLineBreak +
    '* Native Themed Tab Strips for Dictionary Popup (Strong''s & Morphology):' + sLineBreak +
    '  - Replaced Win32 SysTabControl32 native tabs in TFrmDictionaryPopup (formpopup.pas & formpopup.lfm) with native LCL owner-drawn tab panels (HeaderStrongs and HeaderMorfos).' + sLineBreak +
    '  - Eliminates dependency on ComCtl32 SysTabControl32, completely resolving Windows 10/11 DWM theme-engine caching that kept tab buttons white with dark font.' + sLineBreak +
    '  - Active tab paints with CurrentSkin.PointedBg, pure white text (CurrentSkin.PointedText), and top highlight accent (CurrentSkin.HighlightBg).' + sLineBreak +
    '  - Inactive tabs paint with CurrentSkin.PanelBg, readable light font (CurrentSkin.PanelText), and clean borders (CurrentSkin.PanelBorder).' + sLineBreak +
    '  - Retains 100% functional fidelity for tab selection (e.g. Strong''s vs Translation Memory), mouse clicks, RTF skinning, zoom, and splitter resizing.' + sLineBreak +
    '' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'CONTRIBUTORS:' + sLineBreak +
    '  * Edmund P. Valdez <malayangbiblia@gmail.com>: https://github.com/SalitaNgDiyos' + sLineBreak +
    '  * Rúbio R. C. Terra <rubio.terra@gmail.com>: https://github.com/rubiot/ibiblia' + sLineBreak +
    '------------------------------------------';

{ TFormAbout }

constructor TFormAbout.Create(AOwner: TComponent);
var
  FileVerInfo: TFileVersionInfo;
  ver: string;
begin
  inherited CreateNew(AOwner);

  Caption := 'About iBiblia';
  BorderStyle := bsToolWindow;
  BorderIcons := [biSystemMenu];
  Position := poMainFormCenter;
  ClientWidth := 480;
  ClientHeight := 240;
  OnShow := @FormShow;

  // Title: iBiblia
  Label1 := TLabel.Create(Self);
  Label1.Parent := Self;
  Label1.Left := 0;
  Label1.Top := 10;
  Label1.Width := 480;
  Label1.Height := 28;
  Label1.Alignment := taCenter;
  Label1.AutoSize := False;
  Label1.Caption := 'iBiblia';
  Label1.Font.Height := -21;
  Label1.Font.Style := [fsBold];

  // Version
  ver := '';
  try
    FileVerInfo := TFileVersionInfo.Create(nil);
    FileVerInfo.ReadFileInfo;
    ver := FileVerInfo.VersionStrings.Values['FileVersion'];
    FileVerInfo.Free;
  except
    ver := '';
  end;
  if (ver = '') or (ver = '1.1.0.0') or (ver = '1.1.0.1') then
    ver := '1.1.0.019';

  LabelVersion := TLabel.Create(Self);
  LabelVersion.Parent := Self;
  LabelVersion.Left := 0;
  LabelVersion.Top := 40;
  LabelVersion.Width := 480;
  LabelVersion.Height := 20;
  LabelVersion.Alignment := taCenter;
  LabelVersion.AutoSize := False;
  LabelVersion.Caption := 'Version ' + ver + ' (64-bit)';
  LabelVersion.Font.Height := -14;
  LabelVersion.Font.Style := [];

  // Copyright Rubio Terra
  Label2 := TLabel.Create(Self);
  Label2.Parent := Self;
  Label2.Left := 0;
  Label2.Top := 66;
  Label2.Width := 480;
  Label2.Height := 18;
  Label2.Alignment := taCenter;
  Label2.AutoSize := False;
  Label2.Caption := 'Copyright Rúbio R. C. Terra <rubio.terra@gmail.com>';
  Label2.Font.Height := -13;
  Label2.Font.Style := [];

  // Rubio GitHub
  LabelRubioURL := TLabel.Create(Self);
  LabelRubioURL.Parent := Self;
  LabelRubioURL.Left := 0;
  LabelRubioURL.Top := 86;
  LabelRubioURL.Width := 480;
  LabelRubioURL.Height := 18;
  LabelRubioURL.Alignment := taCenter;
  LabelRubioURL.AutoSize := False;
  LabelRubioURL.Caption := 'https://github.com/rubiot/ibiblia';
  LabelRubioURL.Cursor := crHandPoint;
  LabelRubioURL.Font.Height := -13;
  LabelRubioURL.Font.Style := [];
  LabelRubioURL.OnClick := @LabelRubioURLClick;

  // Contributor Edmund P. Valdez
  LabelContributor := TLabel.Create(Self);
  LabelContributor.Parent := Self;
  LabelContributor.Left := 0;
  LabelContributor.Top := 112;
  LabelContributor.Width := 480;
  LabelContributor.Height := 18;
  LabelContributor.Alignment := taCenter;
  LabelContributor.AutoSize := False;
  LabelContributor.Caption := 'Contributor: Edmund P. Valdez <malayangbiblia@gmail.com>';
  LabelContributor.Cursor := crHandPoint;
  LabelContributor.Hint := 'mailto:malayangbiblia@gmail.com';
  LabelContributor.ShowHint := True;
  LabelContributor.Font.Height := -13;
  LabelContributor.Font.Style := [];
  LabelContributor.OnClick := @LabelContributorClick;

  // Edmund GitHub
  Label3 := TLabel.Create(Self);
  Label3.Parent := Self;
  Label3.Left := 0;
  Label3.Top := 132;
  Label3.Width := 480;
  Label3.Height := 18;
  Label3.Alignment := taCenter;
  Label3.AutoSize := False;
  Label3.Caption := 'https://github.com/SalitaNgDiyos';
  Label3.Cursor := crHandPoint;
  Label3.Font.Height := -13;
  Label3.Font.Style := [];
  Label3.OnClick := @Label3Click;

  // Buttons
  ButtonUserGuide := TButton.Create(Self);
  ButtonUserGuide.Parent := Self;
  ButtonUserGuide.Left := 44;
  ButtonUserGuide.Top := 180;
  ButtonUserGuide.Width := 130;
  ButtonUserGuide.Height := 32;
  ButtonUserGuide.Caption := 'User Guide (Shift+F1)';
  ButtonUserGuide.OnClick := @ButtonUserGuideClick;

  ButtonChangelog := TButton.Create(Self);
  ButtonChangelog.Parent := Self;
  ButtonChangelog.Left := 184;
  ButtonChangelog.Top := 180;
  ButtonChangelog.Width := 140;
  ButtonChangelog.Height := 32;
  ButtonChangelog.Caption := 'View Changelog';
  ButtonChangelog.OnClick := @ButtonChangelogClick;

  ButtonClose := TButton.Create(Self);
  ButtonClose.Parent := Self;
  ButtonClose.Left := 334;
  ButtonClose.Top := 180;
  ButtonClose.Width := 100;
  ButtonClose.Height := 32;
  ButtonClose.Caption := 'Close';
  ButtonClose.Cancel := True;
  ButtonClose.Default := True;
  ButtonClose.ModalResult := mrOk;
  ButtonClose.OnClick := @ButtonCloseClick;

  ApplySkin(CurrentSkin);
end;

procedure TFormAbout.ApplySkin(const ASkin: TSkinColors);
begin
  Color := ASkin.PanelBg;
  ApplySkinToForm(Self, ASkin);
  {$IFDEF WINDOWS}
  if HandleAllocated then
    ApplyDarkModeToWindow(Self, ASkin.IsDark);
  if Assigned(ButtonUserGuide) and ButtonUserGuide.HandleAllocated then
    HookButtonControl(ButtonUserGuide);
  if Assigned(ButtonChangelog) and ButtonChangelog.HandleAllocated then
    HookButtonControl(ButtonChangelog);
  if Assigned(ButtonClose) and ButtonClose.HandleAllocated then
    HookButtonControl(ButtonClose);
  {$ENDIF}
  if ASkin.Id <> skClassic then
  begin
    Color := ASkin.PanelBg;
    if Assigned(Label1) then
      Label1.Font.Color := ASkin.HeaderColor;
    if Assigned(LabelVersion) then
      LabelVersion.Font.Color := ASkin.PanelText;
    if Assigned(Label2) then
      Label2.Font.Color := ASkin.PanelText;
    if Assigned(LabelRubioURL) then
      LabelRubioURL.Font.Color := ASkin.HeaderColor;
    if Assigned(LabelContributor) then
      LabelContributor.Font.Color := ASkin.PanelText;
    if Assigned(Label3) then
      Label3.Font.Color := ASkin.HeaderColor;
    if Assigned(ButtonUserGuide) then
      ButtonUserGuide.Font.Color := ASkin.PanelText;
    if Assigned(ButtonChangelog) then
      ButtonChangelog.Font.Color := ASkin.PanelText;
    if Assigned(ButtonClose) then
      ButtonClose.Font.Color := ASkin.PanelText;
  end
  else
  begin
    Color := clBtnFace;
    if Assigned(Label1) then
      Label1.Font.Color := clWindowText;
    if Assigned(LabelVersion) then
      LabelVersion.Font.Color := clWindowText;
    if Assigned(Label2) then
      Label2.Font.Color := clWindowText;
    if Assigned(LabelRubioURL) then
      LabelRubioURL.Font.Color := clBlue;
    if Assigned(LabelContributor) then
      LabelContributor.Font.Color := clWindowText;
    if Assigned(Label3) then
      Label3.Font.Color := clBlue;
    if Assigned(ButtonUserGuide) then
      ButtonUserGuide.Font.Color := clWindowText;
    if Assigned(ButtonChangelog) then
      ButtonChangelog.Font.Color := clWindowText;
    if Assigned(ButtonClose) then
      ButtonClose.Font.Color := clWindowText;
  end;
  if Assigned(ButtonUserGuide) then ButtonUserGuide.Invalidate;
  if Assigned(ButtonChangelog) then ButtonChangelog.Invalidate;
  if Assigned(ButtonClose) then ButtonClose.Invalidate;
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  ApplySkin(CurrentSkin);
  {$IFDEF WINDOWS}
  ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  if Assigned(ButtonUserGuide) then
    HookButtonControl(ButtonUserGuide);
  if Assigned(ButtonChangelog) then
    HookButtonControl(ButtonChangelog);
  if Assigned(ButtonClose) then
    HookButtonControl(ButtonClose);
  {$ENDIF}
  if Assigned(ButtonUserGuide) then ButtonUserGuide.Invalidate;
  if Assigned(ButtonChangelog) then ButtonChangelog.Invalidate;
  if Assigned(ButtonClose) then ButtonClose.Invalidate;
end;

procedure TFormAbout.ButtonUserGuideClick(Sender: TObject);
begin
  if Sender = nil then ;
  Close;
  ShowUserGuide(Application.MainForm);
end;

procedure TFormAbout.ButtonChangelogClick(Sender: TObject);
begin
  ShowChangelog(Self);
end;

procedure TFormAbout.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.LabelContributorClick(Sender: TObject);
begin
  if Sender = nil then ;
  OpenURL('mailto:malayangbiblia@gmail.com');
end;

procedure TFormAbout.Label3Click(Sender: TObject);
begin
  OpenURL('https://github.com/SalitaNgDiyos');
end;

procedure TFormAbout.LabelRubioURLClick(Sender: TObject);
begin
  OpenURL('https://github.com/rubiot/ibiblia');
end;

{ TFormChangelog }

constructor TFormChangelog.Create(AOwner: TComponent);
var
  histFile: string;
begin
  inherited CreateNew(AOwner);

  Caption := 'What''s New in iBiblia';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  ClientWidth := 560;
  ClientHeight := 430;
  Color := CurrentSkin.PanelBg;
  OnShow := @FormShow;

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := Self;
  lblTitle.Left := 16;
  lblTitle.Top := 12;
  lblTitle.Font.Height := -16;
  lblTitle.Font.Style := [fsBold];
  lblTitle.Caption := 'iBiblia v1.1.0.019 - Changelog';
  if IsThemedSkin then
    lblTitle.Font.Color := CurrentSkin.HeaderColor
  else
    lblTitle.Font.Color := clWindowText;

  memoChangelog := TMemo.Create(Self);
  memoChangelog.Parent := Self;
  memoChangelog.Left := 16;
  memoChangelog.Top := 38;
  memoChangelog.Width := 528;
  memoChangelog.Height := 335;
  memoChangelog.ReadOnly := True;
  memoChangelog.ScrollBars := ssAutoVertical;
  memoChangelog.Font.Name := {$IFDEF WINDOWS}'Segoe UI'{$ELSE}'Sans'{$ENDIF};
  memoChangelog.Font.Size := 9;
  if IsThemedSkin then
  begin
    memoChangelog.Color := CurrentSkin.WindowBg;
    memoChangelog.Font.Color := CurrentSkin.WindowText;
  end
  else
  begin
    memoChangelog.Color := clWindow;
    memoChangelog.Font.Color := clWindowText;
  end;

  histFile := ExtractFilePath(Application.ExeName) + 'historico.txt';
  if not FileExists(histFile) then
    histFile := ExtractFilePath(Application.ExeName) + 'Histórico.txt';

  if FileExists(histFile) then
  begin
    try
      memoChangelog.Lines.LoadFromFile(histFile);
    except
      memoChangelog.Text := CHANGELOG_TEXT;
    end;
  end
  else
    memoChangelog.Text := CHANGELOG_TEXT;

  btnOK := TButton.Create(Self);
  btnOK.Parent := Self;
  btnOK.Width := 100;
  btnOK.Height := 30;
  btnOK.Left := (ClientWidth - btnOK.Width) div 2;
  btnOK.Top := 383;
  btnOK.Caption := 'OK';
  btnOK.ModalResult := mrOk;
  btnOK.Default := True;
  btnOK.OnClick := @BtnOKClick;
  if IsThemedSkin then
    btnOK.Font.Color := CurrentSkin.PanelText
  else
    btnOK.Font.Color := clWindowText;

  ApplySkin(CurrentSkin);
end;

procedure TFormChangelog.BtnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

procedure TFormChangelog.ApplySkin(const ASkin: TSkinColors);
begin
  Color := ASkin.PanelBg;
  ApplySkinToForm(Self, ASkin);
  {$IFDEF WINDOWS}
  if HandleAllocated then
    ApplyDarkModeToWindow(Self, ASkin.IsDark);
  if Assigned(btnOK) and btnOK.HandleAllocated then
    HookButtonControl(btnOK);
  {$ENDIF}
  if ASkin.Id <> skClassic then
  begin
    Color := ASkin.PanelBg;
    if Assigned(lblTitle) then
      lblTitle.Font.Color := ASkin.HeaderColor;
    if Assigned(memoChangelog) then
    begin
      memoChangelog.Color := ASkin.WindowBg;
      memoChangelog.Font.Color := ASkin.WindowText;
    end;
    if Assigned(btnOK) then
      btnOK.Font.Color := ASkin.PanelText;
  end
  else
  begin
    Color := clBtnFace;
    if Assigned(lblTitle) then
      lblTitle.Font.Color := clWindowText;
    if Assigned(memoChangelog) then
    begin
      memoChangelog.Color := clWindow;
      memoChangelog.Font.Color := clWindowText;
    end;
    if Assigned(btnOK) then
      btnOK.Font.Color := clWindowText;
  end;
  if Assigned(btnOK) then btnOK.Invalidate;
end;

procedure TFormChangelog.FormShow(Sender: TObject);
begin
  ApplySkin(CurrentSkin);
  {$IFDEF WINDOWS}
  ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  if Assigned(btnOK) then
    HookButtonControl(btnOK);
  {$ENDIF}
  if IsThemedSkin then
  begin
    Color := CurrentSkin.PanelBg;
    if Assigned(lblTitle) then
      lblTitle.Font.Color := CurrentSkin.HeaderColor;
    if Assigned(btnOK) then
      btnOK.Font.Color := CurrentSkin.PanelText;
  end;
  if Assigned(btnOK) then btnOK.Invalidate;
end;

procedure ShowChangelog(AOwner: TComponent);
var
  dlg: TFormChangelog;
begin
  dlg := TFormChangelog.Create(AOwner);
  try
    dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;

{ TFormUserGuide }

constructor TFormUserGuide.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  Caption := 'iBiblia — New Features User Guide (Since v1.0.0.71)';
  BorderStyle := bsSizeable;
  BorderIcons := [biSystemMenu, biMinimize, biMaximize];
  FormStyle := fsStayOnTop;
  Position := poMainFormCenter;
  ClientWidth := 860;
  ClientHeight := 590;
  Constraints.MinWidth := 720;
  Constraints.MinHeight := 480;
  KeyPreview := True;
  OnShow := @FormShow;
  OnResize := @FormResize;
  OnKeyDown := @FormKeyDown;

  FSelectedTopic := 0;
  FHotTopic := -1;
  FLang := 'en';

  // 1. Header Panel
  pnlHeader := TPanel.Create(Self);
  pnlHeader.Parent := Self;
  pnlHeader.Align := alTop;
  pnlHeader.Height := 52;
  pnlHeader.BevelOuter := bvNone;
  pnlHeader.BorderStyle := bsNone;

  lblHeaderTitle := TLabel.Create(pnlHeader);
  lblHeaderTitle.Parent := pnlHeader;
  lblHeaderTitle.Left := 16;
  lblHeaderTitle.Top := 7;
  lblHeaderTitle.Font.Name := 'Segoe UI';
  lblHeaderTitle.Font.Height := -17;
  lblHeaderTitle.Font.Style := [fsBold];
  lblHeaderTitle.Caption := 'iBiblia New Features User Guide (v1.0.0.71 – v1.1.0.019)';

  lblHeaderSub := TLabel.Create(pnlHeader);
  lblHeaderSub.Parent := pnlHeader;
  lblHeaderSub.Left := 16;
  lblHeaderSub.Top := 30;
  lblHeaderSub.Font.Name := 'Segoe UI';
  lblHeaderSub.Font.Height := -12;
  lblHeaderSub.Caption := 'Floating Interactive Guide — Keep this window open while practicing features directly in iBiblia.';

  // 2. Footer Panel
  pnlFooter := TPanel.Create(Self);
  pnlFooter.Parent := Self;
  pnlFooter.Align := alBottom;
  pnlFooter.Height := 44;
  pnlFooter.BevelOuter := bvNone;
  pnlFooter.BorderStyle := bsNone;

  btnPrev := TButton.Create(pnlFooter);
  btnPrev.Parent := pnlFooter;
  btnPrev.Width := 130;
  btnPrev.Height := 30;
  btnPrev.Top := 7;
  btnPrev.Left := 14;
  btnPrev.Caption := '< Previous Topic';
  btnPrev.OnClick := @BtnPrevClick;

  btnNext := TButton.Create(pnlFooter);
  btnNext.Parent := pnlFooter;
  btnNext.Width := 130;
  btnNext.Height := 30;
  btnNext.Top := 7;
  btnNext.Left := 152;
  btnNext.Caption := 'Next Topic >';
  btnNext.OnClick := @BtnNextClick;

  btnCopy := TButton.Create(pnlFooter);
  btnCopy.Parent := pnlFooter;
  btnCopy.Width := 145;
  btnCopy.Height := 30;
  btnCopy.Top := 7;
  btnCopy.Left := 290;
  btnCopy.Caption := 'Copy Guide Text';
  btnCopy.OnClick := @BtnCopyClick;

  btnClose := TButton.Create(pnlFooter);
  btnClose.Parent := pnlFooter;
  btnClose.Width := 100;
  btnClose.Height := 30;
  btnClose.Top := 7;
  btnClose.Caption := 'Close';
  btnClose.Cancel := True;
  btnClose.OnClick := @BtnCloseClick;

  // 3. Body Container
  pnlBody := TPanel.Create(Self);
  pnlBody.Parent := Self;
  pnlBody.Align := alClient;
  pnlBody.BevelOuter := bvNone;
  pnlBody.BorderStyle := bsNone;

  // 4. Left Sidebar (Owner-Drawn Feature Topic Selector)
  pnlSidebar := TPanel.Create(pnlBody);
  pnlSidebar.Parent := pnlBody;
  pnlSidebar.Align := alLeft;
  pnlSidebar.Width := 250;
  pnlSidebar.BevelOuter := bvNone;
  pnlSidebar.BorderStyle := bsNone;

  pbSidebar := TPaintBox.Create(pnlSidebar);
  pbSidebar.Parent := pnlSidebar;
  pbSidebar.Align := alClient;
  pbSidebar.Cursor := crHandPoint;
  pbSidebar.OnPaint := @PbSidebarPaint;
  pbSidebar.OnMouseMove := @PbSidebarMouseMove;
  pbSidebar.OnMouseLeave := @PbSidebarMouseLeave;
  pbSidebar.OnMouseDown := @PbSidebarMouseDown;

  // 5. Right Content Area (Top Visual Illustration + Bottom Step-by-Step Guide)
  pnlContent := TPanel.Create(pnlBody);
  pnlContent.Parent := pnlBody;
  pnlContent.Align := alClient;
  pnlContent.BevelOuter := bvNone;
  pnlContent.BorderStyle := bsNone;
  pnlContent.BorderSpacing.Left := 8;
  pnlContent.BorderSpacing.Right := 10;
  pnlContent.BorderSpacing.Bottom := 4;

  pnlIllust := TPanel.Create(pnlContent);
  pnlIllust.Parent := pnlContent;
  pnlIllust.Align := alTop;
  pnlIllust.Height := 215;
  pnlIllust.BevelOuter := bvNone;
  pnlIllust.BorderStyle := bsNone;
  pnlIllust.BorderSpacing.Bottom := 6;

  pbIllust := TPaintBox.Create(pnlIllust);
  pbIllust.Parent := pnlIllust;
  pbIllust.Align := alClient;
  pbIllust.OnPaint := @PbIllustPaint;

  memoGuide := TMemo.Create(pnlContent);
  memoGuide.Parent := pnlContent;
  memoGuide.Align := alClient;
  memoGuide.ReadOnly := True;
  memoGuide.ScrollBars := ssAutoVertical;
  memoGuide.WordWrap := True;
  memoGuide.Font.Name := 'Segoe UI';
  memoGuide.Font.Size := 10;

  LayoutFooterButtons;
  SelectTopic(0);
  ApplySkin(CurrentSkin);
end;

procedure TFormUserGuide.LayoutFooterButtons;
begin
  if Assigned(btnClose) and Assigned(pnlFooter) then
    btnClose.Left := Max(445, pnlFooter.ClientWidth - btnClose.Width - 14);
end;

procedure TFormUserGuide.FormResize(Sender: TObject);
begin
  if Sender = nil then ;
  LayoutFooterButtons;
  if Assigned(pbSidebar) then pbSidebar.Invalidate;
  if Assigned(pbIllust) then pbIllust.Invalidate;
end;

procedure TFormUserGuide.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Sender = nil then ;
  if Shift = [] then ;
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end
  else if (Key = VK_PRIOR) and (FSelectedTopic > 0) then
  begin
    Key := 0;
    SelectTopic(FSelectedTopic - 1);
  end
  else if (Key = VK_NEXT) and (FSelectedTopic < GUIDE_TOPIC_COUNT - 1) then
  begin
    Key := 0;
    SelectTopic(FSelectedTopic + 1);
  end;
end;

function TFormUserGuide.GetTopicNavTitle(Idx: Integer): string;
begin
  case Idx of
    0: Result := '1. Wheel & Ctrl+Shift Assoc.';
    1: Result := '2. Southeast & English Dicts';
    2: Result := '3. Chevrons, Status & Tabs';
    3: Result := '4. Editor, Paragraphs & Italics';
    4: Result := '5. Text-to-Speech (TTS)';
    5: Result := '6. Dark Mode & 7 Skins';
    6: Result := '7. Import/Export & Sync';
    7: Result := '8. Status Filter & Find (Ctrl+F)';
    8: Result := '9. 1.1.0 Visual Editor & Notes';
    9: Result := '10. 1.1.0 BiDi & Trans. Memory';
    10: Result := '11. 1.1.0 Wizard, AI & Icons';
    11: Result := '12. Full Feature Summary';
  else
    Result := '';
  end;
end;

function TFormUserGuide.GetTopicVersionBadge(Idx: Integer): string;
begin
  case Idx of
    0: Result := 'v1.0.1.024 – v1.0.1.030';
    1: Result := 'v1.0.0.74 – v1.0.1.017';
    2: Result := 'v1.0.0.76 – v1.1.0.019';
    3: Result := 'v1.0.1.002 – v1.1.0.0';
    4: Result := 'v1.0.0.73 – v1.0.0.74';
    5: Result := 'v1.0.0.80 – v1.0.1.023';
    6: Result := 'v1.0.0.71 – v1.1.0.0';
    7: Result := 'v1.0.0.70 – v1.1.0.0';
    8: Result := 'v1.1.0.0 WYSIWYG & Notes';
    9: Result := 'v1.1.0.0 BiDi & Memory';
    10: Result := 'v1.1.0.0 – v1.1.0.019 AI & UI';
    11: Result := 'All Features (v1.0.0.70–v1.1.0.019)';
  else
    Result := '';
  end;
end;

function TFormUserGuide.GetTopicHeaderTitle(Idx: Integer): string;
begin
  case Idx of
    0: Result := '1. Mouse Wheel Source Word Selection & Ctrl+Shift Multi-Word Association';
    1: Result := '2. Southeast Cursor Dictionary Popup & 4-Slot Default Dictionaries';
    2: Result := '3. In-Verse Controls: Hover Navigation Chevrons, 4-Dot Status Bar & Mode Tabs';
    3: Result := '4. Visual Editor: Paragraph Keys, Italics on Save & Live Preview';
    4: Result := '5. Real-Time Text-to-Speech (TTS) Hover Reader (Greek, Hebrew & English)';
    5: Result := '6. Dark Mode UI & 7 theWord Color Themes (Classic, Dark, Night, Sepia, Slate...)';
    6: Result := '7. Multi-Format Import/Export (.ont, e-Sword, USFM, USFX), Apocrypha & Sync';
    7: Result := '8. Verse Status Filtering (F8 / Shift+F8) & Find in Project (Ctrl+F)';
    8: Result := '9. iBiblia 1.1.0: Visual WYSIWYG & Raw ONT Editor, Semantic Markup & Translator Notes';
    9: Result := '10. iBiblia 1.1.0: Bidirectional DirectWrite Rendering, Edit Safety & Translation Memory';
    10: Result := '11. iBiblia 1.1.0: Guided Project Wizard, Custom Statuses, AI Workflows & Toolbar Sizes';
    11: Result := '12. Complete Quick-Reference Summary of All New Features (v1.0.0.70 – v1.1.0.019)';
  else
    Result := '';
  end;
end;

function TFormUserGuide.GetTopicGuideText(Idx: Integer): string;
var
  i: Integer;
begin
  case Idx of
    0:
      Result :=
        'FEATURE OVERVIEW (v1.0.1.024 – v1.0.1.030)' + sLineBreak +
        'Select words in the Source text hands-free using the mouse scroll wheel from either the Source or Destination panel, associate single or multiple words with Ctrl+Shift, and let iBiblia automatically advance to the next unassociated Source word when you release Ctrl+Shift.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Mouse Wheel Word Selection (Left / Right):' + sLineBreak +
        '     • Place your mouse cursor inside EITHER the Source text panel OR the Destination text panel.' + sLineBreak +
        '     • Scroll UP (away from you): Selects the previous word to the LEFT in the Source text one by one.' + sLineBreak +
        '     • Scroll DOWN (toward you): Selects the next word to the RIGHT in the Source text one by one.' + sLineBreak +
        '     • If a word is already associated, its paired Destination word(s) highlight automatically so you can inspect existing associations.' + sLineBreak + sLineBreak +
        '  2. Multi-Word Association & Auto-Advance (Ctrl + Shift):' + sLineBreak +
        '     • Select one word (via Mouse Wheel or Left-Click) or multiple words (Hold Ctrl + Left-Click) in the Source text.' + sLineBreak +
        '     • Hold down CTRL + SHIFT and Left-Click one or more target words in the Destination text to associate them together as a phrase.' + sLineBreak +
        '     • Release the CTRL + SHIFT keys: iBiblia automatically clears the current selection and highlights the NEXT unassociated word in the Source text!' + sLineBreak + sLineBreak +
        '  3. Instant Single-Word Association (Right-Click):' + sLineBreak +
        '     • With a Source word selected, simply Right-Click a single target word in the Destination text to associate it and immediately jump to the next unassociated Source word.' + sLineBreak + sLineBreak +
        'SHORTCUT SUMMARY:' + sLineBreak +
        '  • Mouse Wheel Up (in Source or Dest)   -> Select Source word to the LEFT' + sLineBreak +
        '  • Mouse Wheel Down (in Source or Dest) -> Select Source word to the RIGHT' + sLineBreak +
        '  • Ctrl + Mouse Wheel                   -> Zoom panel font size in/out' + sLineBreak +
        '  • Hold Ctrl + Shift + Left-Click       -> Select/associate multiple words at once' + sLineBreak +
        '  • Release Ctrl + Shift                 -> Auto-select next unassociated Source word';

    1:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.74 – v1.0.1.017)' + sLineBreak +
        'Streamlined global dictionary management supporting compressed (.dct.twm) modules, automatic Greek/Hebrew font loading, English Word/Bible dictionary lookups on untagged words, and an automatic Southeast Cursor Dictionary Popup while translating inside the Destination panel.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Southeast Cursor Dictionary Popup in Destination Panel:' + sLineBreak +
        '     • Select an unassociated word in the Source text (or let auto-advance select it).' + sLineBreak +
        '     • Move your mouse cursor into the Destination text panel: the Strong''s & Morphology dictionary popup automatically floats 18px southeast of your cursor so you can read the Greek/Hebrew definition while choosing target words.' + sLineBreak +
        '     • Hover over any ALREADY-ASSOCIATED word in the Destination panel to temporarily inspect that word''s own dictionary entry; move off it to resume the selected Source word''s popup.' + sLineBreak +
        '     • Hold CTRL while the popup is visible to freeze its position so you can move the cursor inside the popup to scroll or zoom.' + sLineBreak + sLineBreak +
        '  2. Configure 4 Global Default Dictionaries (Tools > Default dictionaries...):' + sLineBreak +
        '     • Greek Strong''s Dictionary: Used for NT (Books 40–66) and LXX Greek G-codes.' + sLineBreak +
        '     • Hebrew Strong''s Dictionary: Used for OT (Books 1–39) and Hebrew H-codes (supports raw, zero-padded H0123, compound Hc/H7225, and redirect entries).' + sLineBreak +
        '     • Morphology Dictionary: Displays grammatical parsing for Robinson, Packard, and LXX morphology tags.' + sLineBreak +
        '     • English Word / Bible Dictionary: Hover over ANY untagged English word or proper name in any of the 4 Bible panels to view its English dictionary definition (with automatic plural/verb stem matching).' + sLineBreak + sLineBreak +
        '  3. Compressed Modules & Per-Module Override:' + sLineBreak +
        '     • Compressed theWord dictionaries (compressed=1) are decompressed on the fly.' + sLineBreak +
        '     • In Project settings, tick or untick "Use default dictionaries" per Bible module.';

    2:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.76 – v1.0.1.026)' + sLineBreak +
        'Every Bible panel features clean, context-sensitive in-verse controls for navigating verses, tracking association status, identifying translations, and switching between Association, Edit, and Properties modes.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Hover-Only Verse Navigation Chevrons (< >):' + sLineBreak +
        '     • Move your mouse pointer inside any of the 4 Bible panels (Source, Destination, Reference 1, or Reference 2).' + sLineBreak +
        '     • Subtle "<" (Previous Verse) and ">" (Next Verse) chevron arrows appear right after the end of the verse text ONLY in the Bible panel your mouse is currently inside, and disappear in all other panels.' + sLineBreak +
        '     • Click "<" to go to the previous verse or ">" to advance to the next verse.' + sLineBreak + sLineBreak +
        '  2. Interactive 4-Color Verse Status Circles (Destination Bottom Bar):' + sLineBreak +
        '     • Located in the center of the Destination panel''s bottom bar (and synced with keys 1–4):' + sLineBreak +
        '       - Circle 1 (Gray):   Not associated (Key: 1)' + sLineBreak +
        '       - Circle 2 (Yellow): Associating    (Key: 2)' + sLineBreak +
        '       - Circle 3 (Red):    Needs review   (Key: 3)' + sLineBreak +
        '       - Circle 4 (Green):  Associated!    (Key: 4 — also triggers automatic project Save/Commit)' + sLineBreak + sLineBreak +
        '  3. Translation Name Badge & Edit / Properties Checkboxes:' + sLineBreak +
        '     • Bottom-Left Badge: Displays the active module''s title, short.title, or description (updates live as you edit module properties).' + sLineBreak +
        '     • Bottom-Right Checkboxes:' + sLineBreak +
        '       - Both Unchecked: Normal Word Association & Alignment mode.' + sLineBreak +
        '       - [x] Edit:       Opens the in-place visual verse editor.' + sLineBreak +
        '       - [x] Properties: Shows the module properties (title, description, version... one name=value per line) in place of the verse text. Edit them there; the badge at the bottom-left follows the title, and the properties are saved with the project and used when you export the module.';

    3:
      Result :=
        'FEATURE OVERVIEW (v1.0.1.002 – v1.0.1.034, built on the 1.1.0 visual editor)' + sLineBreak +
        'When "Edit" is checked on a Bible panel (or you double-click the verse), the verse opens in the visual editor. The mod''s paragraph keys and its italics-on-save were kept; the separate Master Formatting Ribbon of the mod was replaced by the editor''s own formatting controls.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Activating Edit Mode:' + sLineBreak +
        '     • Check the "Edit" checkbox at the bottom-right of any Bible panel (or double-click the verse text).' + sLineBreak +
        '     • Uncheck "Edit" (or click another word/verse) to leave the editor; your changes are kept.' + sLineBreak + sLineBreak +
        '  2. Paragraph Keys:' + sLineBreak +
        '     • ENTER inserts <CI> (new line), DOUBLE ENTER inserts <CM> (new paragraph with blank line), SHIFT+ENTER inserts <CL> (line break inside a paragraph).' + sLineBreak + sLineBreak +
        '  3. Formatting:' + sLineBreak +
        '     • Use the visual editor''s own formatting controls for headings, indents, italics (added words), words of Jesus, quotations and footnotes.' + sLineBreak + sLineBreak +
        '  4. Live Preview & Unassociated Words in Italics on File Save:' + sLineBreak +
        '     • Edits show immediately in the right-hand Context Panel.' + sLineBreak +
        '     • When you save the file (Ctrl+S / File > Save), the Destination words that are not linked to a Greek/Hebrew source word are shown in italics (<FI>...<Fi>), so you can spot words added for readability or not yet linked.';

    4:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.73 – v1.0.0.74)' + sLineBreak +
        'Real-time Text-to-Speech (TTS) pronounces Greek, Hebrew, and English/vernacular words aloud when you hover over them, supporting both Windows 10/11 OneCore Speech Packs (e.g. Microsoft Stefanos for Greek, Microsoft Asaf for Hebrew) and classic SAPI5 voices.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Toggle Text-to-Speech On / Off:' + sLineBreak +
        '     • Press CTRL + T at any time, or click Tools > Read word on hover (text-to-speech).' + sLineBreak + sLineBreak +
        '  2. Configure Voices, Speed, Volume & Trigger (Tools > Voice settings...):' + sLineBreak +
        '     • Source Text Voice: Choose a Greek voice (such as Microsoft Stefanos) or Hebrew voice (such as Microsoft Asaf) for the Source Bible panel.' + sLineBreak +
        '     • Translation / Destination Voice: Choose an English or local language voice for the Destination, Reference 1, and Reference 2 panels.' + sLineBreak +
        '     • Speech Rate & Volume: Adjust speed (-10 to +10) and volume (0% to 100%) using the themed sliders.' + sLineBreak +
        '     • Hover Trigger Condition: Choose whether speech plays Always on Mouse Hover, only on Ctrl + Hover, or only on Alt + Hover.' + sLineBreak +
        '     • Instant Test Buttons: Click "Test" next to either voice dropdown to hear a live sample immediately.';

    5:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.80 – v1.0.1.023)' + sLineBreak +
        'Full OS-level and custom-drawn theming across the entire application—including the Main Menu Bar, dropdown/popup menus, 4 Bible panels, TreeView, Context Panel, Dictionary Popup, and all modal dialogs—with 7 built-in theWord color schemes.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Choose a Theme (View > Skins (theWord Themes)):' + sLineBreak +
        '     • Classic (Light):        Standard bright light workspace.' + sLineBreak +
        '     • theWord Dark (Charcoal): Signature #1E1E1E charcoal reading panes with #252526 shell.' + sLineBreak +
        '     • Night (Pitch Black):    Deep #121212 OLED pitch-black background with golden highlights.' + sLineBreak +
        '     • Sepia (Parchment):      Eye-strain relief warm #F4EEDB parchment with #2D241E ink.' + sLineBreak +
        '     • Slate (Steel Blue):     Cool #1C232B navy-slate background with ice-blue text.' + sLineBreak +
        '     • Solarized Dark:         Precision cyan-dark #002B36 palette.' + sLineBreak +
        '     • High Contrast Dark:     Pure black & stark white with vivid cyan/yellow accents.' + sLineBreak + sLineBreak +
        '  2. Instant Keyboard Toggle (Ctrl + Shift + D):' + sLineBreak +
        '     • Press CTRL + SHIFT + D at any time to toggle between Dark Mode (theWord Dark) and Classic (Light).' + sLineBreak +
        '     • Your active theme is automatically saved in iBiblia.ini and restored on startup.';

    6:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.71 – v1.0.1.051)' + sLineBreak +
        'Import and export Bibles across theWord (.ont, .ot, .nt), MySword (.bbl.mybible), non-encrypted e-Sword (.bblx RTF, .bbli HTML, .bbl SQLite), USFM 2.x/3.0 (.usfm, .sfm), and USFX XML (.usfx, .xml from eBible.org / Simple Bible Reader) with full Apocrypha / Deuterocanonical OT book & chapter support in the Bible Books Tree.' + sLineBreak +
        'Also includes safe Project Save As (Ctrl+Shift+S), exporting modules even while open in theWord, per-project remembered export destinations, smart word-diff detection on text reload, and bidirectional 64-bit theWord 7 synchronization.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Multi-Format Bible Import & Export (.ont/.ot/.nt, e-Sword, USFM, USFX, MySword):' + sLineBreak +
        '     • Import any supported format in File > New Project or Project > Settings across Source, Destination, Reference 1, or Reference 2 (selecting a single .usfm/.sfm book file automatically loads all sibling book files in that folder).' + sLineBreak +
        '     • Export Tagged Destination or Interlinear Bibles via File > Export... or right-click any of the 4 Bible panels and select "Export as Bible module..." to export directly to .ont/.ot/.nt, e-Sword (.bblx/.bbli), USFM (.usfm/.sfm), USFX (.usfx/.xml), or MySword (.bbl.mybible).' + sLineBreak + sLineBreak +
        '  2. Apocrypha / Deuterocanonical OT Books in the Bible Books Tree (v1.0.1.045–047):' + sLineBreak +
        '     • Standalone Apocrypha OT books (Book >= 67: 1–2 Esdras, Tobit, Judith, 1–4 Maccabees, Prayer of Manasseh, Wisdom, Sirach, Baruch, Letter of Jeremiah, Prayer of Azariah, Susanna, Bel and the Dragon, Psalm 151, Psalms of Solomon, Odes, Laodiceans) appear right after Malachi at the end of the Old Testament, and extra chapters (Daniel 13–14, Psalms 151, Esther 11–16) appear under their canonical books.' + sLineBreak + sLineBreak +
        '  3. Smart "Needs Review" Status on Text Reload & Concurrent theWord 7 + Tanglaw Bible Sync:' + sLineBreak +
        '     • Reloading a Bible text in Project > Settings compares actual words: tag-only updates stay Green ("Associated!"), while word edits become Red ("Needs review").' + sLineBreak +
        '     • Enable the theWord and Timotheos (Tanglaw) synchronization items in the Tools menu simultaneously for bidirectional verse synchronization.';

    7:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.70 – v1.0.0.76)' + sLineBreak +
        'Filter the left Bible TreeView by association status with live per-status verse counts, step directly through matching verses across chapters and books (F8 / Shift+F8), and search across the entire project using the modeless Find in Project dialog (Ctrl+F).' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Verse Status Filtering & Filtered Stepping (Above Left Bible TreeView):' + sLineBreak +
        '     • Open the Status Filter dropdown at the top of the left navigation panel to filter the Bible tree by status with live verse counts:' + sLineBreak +
        '       - All verses (Total count)' + sLineBreak +
        '       - Not associated (Count)' + sLineBreak +
        '       - Associating (Count)' + sLineBreak +
        '       - Needs review (Count)' + sLineBreak +
        '       - Associated! (Count)' + sLineBreak +
        '     • Click "<" (or press SHIFT + F8) to jump to the previous verse matching the selected status.' + sLineBreak +
        '     • Click ">" (or press F8) to jump to the next verse matching the selected status.' + sLineBreak +
        '     • The live counter beside the buttons displays "N found", and your active filter is persisted both per-project (.ibb) and in iBiblia.ini.' + sLineBreak + sLineBreak +
        '  2. Project-Wide Search — Find in Project (Edit > Find in Project... / Ctrl+F):' + sLineBreak +
        '     • Press CTRL + F at any time to open the floating Find in Project window.' + sLineBreak +
        '     • Type a word, phrase, or Strong''s code and choose one of the 7 "Look in:" scopes:' + sLineBreak +
        '       1) Destination translation (Target text)' + sLineBreak +
        '       2) Original source (Hebrew / Greek)' + sLineBreak +
        '       3) Reference text 1' + sLineBreak +
        '       4) Reference text 2' + sLineBreak +
        '       5) Strong tags / Pairs (e.g. G3056, H7225, or morphology tags)' + sLineBreak +
        '       6) Translator notes / Comments' + sLineBreak +
        '       7) All text fields' + sLineBreak +
        '     • Click or double-click any matching verse in the results list (or click "< Previous Match" / "Next Match >") to jump directly to that verse in the main workspace.';

    8:
      Result :=
        'FEATURE OVERVIEW (iBiblia 1.1.0.0)' + sLineBreak +
        'Write and format verses visually in WYSIWYG mode—including section titles, added words, Words of Jesus, Old Testament quotations, custom colors, relative font sizes, HTML/inline formatting, and rich translator notes with hover previews—or toggle Raw mode for direct theWord ONT markup editing.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Visual WYSIWYG & Raw ONT Modes:' + sLineBreak +
        '     • Click "Edit" on the hover bottom bar of any Bible panel (or double-click a verse) to open the in-place editor.' + sLineBreak +
        '     • Format selected text with semantic toolbar buttons: Added Words (I / <FI>...<Fi>), Words of Jesus (J / <FR>...<Fr>), OT Quotations (Q / <FO>...<Fo>), Titles (<TS1>..<TS3>), Underline, Strikethrough, Superscript, Subscript, Font Size (-/+), and Font Color.' + sLineBreak +
        '     • Click "Raw" / "Visual" on the editor toolbar to switch seamlessly between visual formatting and raw theWord ONT tags while protecting complex untouchable regions.' + sLineBreak + sLineBreak +
        '  2. Rich Translator Notes (<RF>...<Rf>):' + sLineBreak +
        '     • Click the Translator Note button (N) on the editor toolbar to insert or edit a footnote at the cursor with automatic sequential numbering (q=1, 2, 3...) or a custom marker.' + sLineBreak +
        '     • Hover over any footnote marker in the Bible panels to preview the formatted translator note in a themed popup card, or click the marker to edit that note directly.' + sLineBreak + sLineBreak +
        '  3. Association-Preserving Text Edits:' + sLineBreak +
        '     • When you edit Source or Destination text, unaffected word associations are automatically preserved, and iBiblia shows an impact confirmation dialog before any linked words are disconnected.';

    9:
      Result :=
        'FEATURE OVERVIEW (iBiblia 1.1.0.0)' + sLineBreak +
        'High-DPI DirectWrite bidirectional rendering for mixed Hebrew (RTL), Greek (LTR), and translated text, combined with an interactive Translation Memory tab in the Context Panel that compares past translation choices and previews occurrences without leaving the current verse.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Bidirectional (RTL / LTR) DirectWrite Rendering & Unicode Selection:' + sLineBreak +
        '     • Right-click any Bible panel and toggle "Right-to-left text" (or let imported module direction configure it automatically).' + sLineBreak +
        '     • Mixed Hebrew, Greek, punctuation, Strong''s counts, and translator note markers render with hardware-accelerated DirectWrite typography and accurate Unicode hit-testing.' + sLineBreak + sLineBreak +
        '  2. Translation Memory Comparison & Hover Previews (Context Panel > Memory Tab):' + sLineBreak +
        '     • Click the "Memory" tab in the right-hand Context Panel (or click any word in the current verse to focus its entry in Translation Memory).' + sLineBreak +
        '     • Sort entries by "Verse order" or "Rarest first" using the Order dropdown.' + sLineBreak +
        '     • Inspect how each Greek/Hebrew word or Strong''s number has been translated across your project, hover or expand occurrences to preview highlighted verses inline, or click an occurrence to navigate there (and use Alt+Left / Alt+Right to return).' + sLineBreak +
        '     • Use Rebuild Association Memory in Project > Maintenance whenever you want a fresh full-project index.';

    10:
      Result :=
        'FEATURE OVERVIEW (iBiblia 1.1.0.0 – 1.1.0.019)' + sLineBreak +
        'Create projects with the Guided New Project Wizard, customize project status catalogs, run experimental AI translation and association flows with batch review, save independent .bib copies via Save As, and customize interface language, icon families, and 3 toolbar icon sizes.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Guided Project Creation Wizard & Custom Statuses:' + sLineBreak +
        '     • File > New Project launches a step-by-step wizard supporting both "Association project" (existing Source + Destination) and "Translation project" (Source + initially empty Destination).' + sLineBreak +
        '     • Customize up to 12 verse statuses (names, colors, and display order) in New Project or Project > Settings; the Bible TreeView, Status Summary filter box, and Destination 4-dot status strip stay synchronized automatically.' + sLineBreak +
        '     • Use File > Save As (Ctrl+Shift+S) to save an independent .bib copy, and check the bottom-left corner of the status bar (just below the status summary box) for real-time "Saved" / "Unsaved changes" status.' + sLineBreak + sLineBreak +
        '  2. Experimental AI Translation & Association Flows:' + sLineBreak +
        '     • Configure machine-default or project-specific AI providers, models, custom prompts, and verse examples in Project > AI Options (with API keys stored securely in Windows local storage).' + sLineBreak +
        '     • Run single-verse AI suggestions from the toolbar split button or right-click a book/chapter in the Bible TreeView to run batch AI Association or Translation with side-by-side review before applying.' + sLineBreak + sLineBreak +
        '  3. Icon Families & 3 Toolbar Icon Sizes (View > Toolbar icon size / Tools > Preferences):' + sLineBreak +
        '     • Choose English or Portuguese UI, 4 icon families (Connected, Clareza, Mosaico, Folio), and 3 top-bar icon sizes: Small (16 px), Medium (22 px), or Large (28 px).';

    11:
      begin
        Result := '';
        for i := 0 to 10 do
        begin
          if Result <> '' then
            Result := Result + sLineBreak + sLineBreak +
              '========================================================================' + sLineBreak + sLineBreak;
          Result := Result + GetTopicHeaderTitle(i) + sLineBreak +
            '------------------------------------------------------------------------' + sLineBreak +
            GetTopicGuideText(i);
        end;
      end;
  else
    Result := '';
  end;
end;

procedure TFormUserGuide.SelectTopic(Idx: Integer);
begin
  FSelectedTopic := EnsureRange(Idx, 0, GUIDE_TOPIC_COUNT - 1);
  if Assigned(memoGuide) then
  begin
    memoGuide.Lines.BeginUpdate;
    try
      memoGuide.Text := GetTopicGuideText(FSelectedTopic);
      memoGuide.SelStart := 0;
      memoGuide.SelLength := 0;
    finally
      memoGuide.Lines.EndUpdate;
    end;
  end;
  if Assigned(btnPrev) then
    btnPrev.Enabled := (FSelectedTopic > 0);
  if Assigned(btnNext) then
    btnNext.Enabled := (FSelectedTopic < GUIDE_TOPIC_COUNT - 1);
  if Assigned(pbSidebar) then
    pbSidebar.Invalidate;
  if Assigned(pbIllust) then
    pbIllust.Invalidate;
end;

procedure TFormUserGuide.PbSidebarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  itemH, idx: Integer;
begin
  if Sender = nil then ;
  if Shift = [] then ;
  if X = 0 then ;
  itemH := Max(36, (pbSidebar.Height - 12) div GUIDE_TOPIC_COUNT);
  idx := (Y - 6) div itemH;
  if (Y < 6) or (idx < 0) or (idx >= GUIDE_TOPIC_COUNT) then
    idx := -1;
  if idx <> FHotTopic then
  begin
    FHotTopic := idx;
    pbSidebar.Invalidate;
  end;
end;

procedure TFormUserGuide.PbSidebarMouseLeave(Sender: TObject);
begin
  if Sender = nil then ;
  if FHotTopic <> -1 then
  begin
    FHotTopic := -1;
    pbSidebar.Invalidate;
  end;
end;

procedure TFormUserGuide.PbSidebarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  itemH, idx: Integer;
begin
  if Sender = nil then ;
  if Shift = [] then ;
  if X = 0 then ;
  if Button <> mbLeft then Exit;
  itemH := Max(36, (pbSidebar.Height - 12) div GUIDE_TOPIC_COUNT);
  idx := (Y - 6) div itemH;
  if (Y >= 6) and (idx >= 0) and (idx < GUIDE_TOPIC_COUNT) then
    SelectTopic(idx);
end;

procedure TFormUserGuide.PbSidebarPaint(Sender: TObject);
var
  bmp: TBitmap;
  i, itemH, yTop, yBot: Integer;
  rcItem, rcAccent: TRect;
  bgCol, itemBg, titleCol, subCol, borderCol: TColor;
begin
  if Sender = nil then ;
  if (pbSidebar.Width <= 0) or (pbSidebar.Height <= 0) then Exit;

  bmp := TBitmap.Create;
  try
    bmp.SetSize(pbSidebar.Width, pbSidebar.Height);
    bgCol := CurrentSkin.PanelBg;
    borderCol := CurrentSkin.PanelBorder;

    bmp.Canvas.Brush.Style := bsSolid;
    bmp.Canvas.Brush.Color := bgCol;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

    // Right border divider
    bmp.Canvas.Pen.Color := borderCol;
    bmp.Canvas.Line(bmp.Width - 1, 0, bmp.Width - 1, bmp.Height);

    itemH := Max(36, (bmp.Height - 12) div GUIDE_TOPIC_COUNT);
    bmp.Canvas.Font.Name := 'Segoe UI';

    for i := 0 to GUIDE_TOPIC_COUNT - 1 do
    begin
      yTop := 6 + i * itemH;
      yBot := yTop + itemH - 4;
      rcItem := Rect(8, yTop, bmp.Width - 8, yBot);

      if i = FSelectedTopic then
      begin
        if CurrentSkin.IsDark then
        begin
          itemBg := CurrentSkin.PointedBg;
          titleCol := CurrentSkin.PointedText;
          subCol := MakeRGB(220, 235, 250);
        end
        else if CurrentSkin.Id = skSepia then
        begin
          itemBg := CurrentSkin.HighlightBg;
          titleCol := CurrentSkin.HighlightText;
          subCol := CurrentSkin.WindowTextDim;
        end
        else
        begin
          itemBg := MakeRGB(215, 234, 252);
          titleCol := clBlack;
          subCol := MakeRGB(70, 90, 110);
        end;
      end
      else if i = FHotTopic then
      begin
        itemBg := CurrentSkin.VerseHighlightBg;
        titleCol := CurrentSkin.PanelText;
        subCol := CurrentSkin.WindowTextDim;
      end
      else
      begin
        itemBg := CurrentSkin.WindowBg;
        titleCol := CurrentSkin.WindowText;
        subCol := CurrentSkin.WindowTextDim;
      end;

      bmp.Canvas.Brush.Style := bsSolid;
      bmp.Canvas.Brush.Color := itemBg;
      bmp.Canvas.Pen.Color := borderCol;
      bmp.Canvas.RoundRect(rcItem.Left, rcItem.Top, rcItem.Right, rcItem.Bottom, 6, 6);

      if i = FSelectedTopic then
      begin
        rcAccent := Rect(rcItem.Left, rcItem.Top + 3, rcItem.Left + 4, rcItem.Bottom - 3);
        bmp.Canvas.Brush.Color := IfThen(CurrentSkin.IsDark, CurrentSkin.HighlightBg, CurrentSkin.HeaderColor);
        bmp.Canvas.FillRect(rcAccent);
      end;

      bmp.Canvas.Brush.Style := bsClear;
      bmp.Canvas.Font.Size := 9;
      bmp.Canvas.Font.Style := [fsBold];
      bmp.Canvas.Font.Color := titleCol;
      bmp.Canvas.TextOut(rcItem.Left + 10, rcItem.Top + 6, GetTopicNavTitle(i));

      bmp.Canvas.Font.Size := 8;
      bmp.Canvas.Font.Style := [];
      bmp.Canvas.Font.Color := subCol;
      bmp.Canvas.TextOut(rcItem.Left + 10, rcItem.Top + 23, GetTopicVersionBadge(i));
    end;

    pbSidebar.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TFormUserGuide.PbIllustPaint(Sender: TObject);
var
  bmp: TBitmap;
  W, H: Integer;
  winBg, pnlBg, borderCol, txtCol, dimCol, hdrCol, hiBg, hiTxt, ptBg, ptTxt: TColor;

  procedure DrawBox(C: TCanvas; X1, Y1, X2, Y2: Integer; FillC, BorderC: TColor; Radius: Integer = 6);
  begin
    C.Brush.Style := bsSolid;
    C.Brush.Color := FillC;
    C.Pen.Style := psSolid;
    C.Pen.Width := 1;
    C.Pen.Color := BorderC;
    if Radius > 0 then
      C.RoundRect(X1, Y1, X2, Y2, Radius, Radius)
    else
      C.Rectangle(X1, Y1, X2, Y2);
  end;

  procedure DrawBadge(C: TCanvas; X, Y: Integer; const Txt: string; BgC, FgC, BorderC: TColor; BoldFont: Boolean = True; PadX: Integer = 8; PadY: Integer = 3);
  var
    tw, th: Integer;
  begin
    if BoldFont then
      C.Font.Style := [fsBold]
    else
      C.Font.Style := [];
    tw := C.TextWidth(Txt);
    th := C.TextHeight(Txt);
    DrawBox(C, X, Y, X + tw + PadX * 2, Y + th + PadY * 2, BgC, BorderC, 5);
    C.Brush.Style := bsClear;
    C.Font.Color := FgC;
    C.TextOut(X + PadX, Y + PadY, Txt);
  end;

  procedure DrawArrow(C: TCanvas; X1, Y1, X2, Y2: Integer; Col: TColor);
  var
    dx, dy, len, ux, uy, px, py: Double;
    p1, p2: TPoint;
  begin
    C.Pen.Color := Col;
    C.Pen.Width := 2;
    C.Line(X1, Y1, X2, Y2);
    C.Pen.Width := 1;
    dx := X2 - X1;
    dy := Y2 - Y1;
    len := Sqrt(dx * dx + dy * dy);
    if len < 1 then Exit;
    ux := dx / len;
    uy := dy / len;
    px := -uy;
    py := ux;
    p1 := Point(Round(X2 - ux * 8 + px * 4), Round(Y2 - uy * 8 + py * 4));
    p2 := Point(Round(X2 - ux * 8 - px * 4), Round(Y2 - uy * 8 - py * 4));
    C.Brush.Style := bsSolid;
    C.Brush.Color := Col;
    C.Polygon([Point(X2, Y2), p1, p2]);
  end;

  procedure DrawTopic0(C: TCanvas);
  begin
    // Source Box
    DrawBox(C, 16, 36, W - 210, 106, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 41, 'SOURCE TEXT PANEL (Mouse Wheel Up = Left  |  Mouse Wheel Down = Right)');

    DrawBadge(C, 26, 62, 'ἘΝ', winBg, dimCol, borderCol, False);
    DrawBadge(C, 70, 62, '◄  ἀρχῇ  ►', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, 166, 62, 'ἐποίησεν (Next Unassoc.)', winBg, txtCol, ptBg, True);
    DrawBadge(C, 342, 62, 'ὁ Θεός', winBg, dimCol, borderCol, False);

    // Mouse wheel illustration on the right
    DrawBox(C, W - 198, 36, W - 16, 106, pnlBg, hdrCol, 8);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W - 188, 42, 'MOUSE WHEEL');
    C.Font.Style := [];
    C.Font.Color := txtCol;
    C.TextOut(W - 188, 60, '▲ Scroll Up   -> Word Left');
    C.TextOut(W - 188, 76, '▼ Scroll Down -> Word Right');
    C.Font.Color := dimCol;
    C.TextOut(W - 188, 91, '(Works in Source or Dest!)');

    // Destination Box
    DrawBox(C, 16, 118, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 123, 'DESTINATION TEXT PANEL (Hold Ctrl+Shift + Click multiple words -> Release to Auto-Advance)');

    DrawBadge(C, 26, 145, 'In', winBg, txtCol, borderCol, False);
    DrawBadge(C, 62, 145, 'the', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, 106, 145, 'beginning', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, 192, 145, 'God', winBg, dimCol, borderCol, False);
    DrawBadge(C, 238, 145, 'made', winBg, dimCol, borderCol, False);

    DrawArrow(C, 112, 90, 112, 142, hdrCol);
    DrawBadge(C, 295, 144, '1. Hold Ctrl+Shift + Click "the" & "beginning"', ptBg, ptTxt, borderCol, True);
    DrawBadge(C, 295, 171, '2. Release Ctrl+Shift -> Auto-selects "ἐποίησεν"!', hiBg, hiTxt, borderCol, True);
  end;

  procedure DrawTopic1(C: TCanvas);
  begin
    // Left: Source + Destination with Southeast Popup
    DrawBox(C, 16, 36, W div 2 + 15, 92, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(24, 41, '1. Select Unassociated Source Word:');
    DrawBadge(C, 24, 59, 'ἀρχῇ <WG746><WTN-DSF>', hiBg, hiTxt, hdrCol, True);

    DrawBox(C, 16, 98, W div 2 + 15, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(24, 103, '2. Move Cursor Inside Destination Panel:');
    DrawBadge(C, 24, 122, 'In   the   beginning', winBg, txtCol, borderCol, False);

    // Cursor + Southeast Popup
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(86, 146, '↖ Cursor');
    DrawBox(C, 142, 134, W div 2 + 8, 193, CurrentSkin.NoteBg, hdrCol, 5);
    DrawBadge(C, 146, 138, 'G746', ptBg, ptTxt, borderCol, True, 5, 1);
    DrawBadge(C, 192, 138, 'N-DSF', pnlBg, txtCol, borderCol, False, 5, 1);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Color := CurrentSkin.NoteText;
    C.TextOut(148, 160, 'ἀρχή (archē) — beginning, origin');
    C.Font.Color := dimCol;
    C.TextOut(148, 175, '(Floats +18px Southeast of cursor)');

    // Right: 4-Slot Default Dictionaries
    DrawBox(C, W div 2 + 25, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 35, 42, 'DEFAULT DICTIONARIES (Options Menu)');
    DrawBadge(C, W div 2 + 35, 62,  '1. Greek Strong''s (.dct.twm)  -> NT & LXX G-codes', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 35, 91,  '2. Hebrew Strong''s (.dct.twm) -> OT H-codes (H0123)', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 35, 120, '3. Morphology (.dct.twm)      -> Grammar & Parsing', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 35, 149, '4. English Word (.dct.twm)    -> Untagged English', hiBg, hiTxt, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 35, 178, 'Supports compressed=1 zlib & auto-loads .ttf fonts');
  end;

  procedure DrawTopic2(C: TCanvas);
  var
    barY, cx, i: Integer;
    dotCols: array[0..3] of TColor;
  begin
    // Annotated Bible Panel Box
    DrawBox(C, 16, 36, W - 16, 196, winBg, borderCol, 6);

    C.Brush.Style := bsClear;
    C.Font.Size := 10;
    C.Font.Style := [];
    C.Font.Color := txtCol;
    C.TextOut(28, 52, 'In the beginning God created the heaven and the earth.');

    // Hover-Only Chevron Arrows right after verse text
    DrawBox(C, 392, 48, 434, 72, pnlBg, hdrCol, 5);
    C.Brush.Style := bsClear;
    C.Font.Size := 10;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(399, 51, '‹   ›');

    DrawArrow(C, 470, 60, 438, 60, hdrCol);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(476, 46, 'Hover-Only Verse Chevrons (‹ Prev / › Next)');
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(476, 61, 'Appears ONLY in the Bible panel under the mouse cursor!');

    // Bottom Bar inside the Bible Panel
    barY := 158;
    DrawBox(C, 20, barY, W - 20, 190, pnlBg, borderCol, 4);

    // 1. Translation Name Badge (Bottom-Left)
    DrawBadge(C, 26, barY + 4, 'KJV Strong''s (title=)', winBg, txtCol, hdrCol, True);
    DrawArrow(C, 85, 130, 85, barY + 2, hdrCol);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(26, 112, 'Bottom-Left: Translation Name');

    // 2. 4-Color Status Circles (Bottom-Center)
    dotCols[0] := MakeRGB(205, 205, 205);
    dotCols[1] := MakeRGB(255, 207, 57);
    dotCols[2] := MakeRGB(243, 64, 93);
    dotCols[3] := MakeRGB(75, 200, 66);
    cx := (W div 2) - 45;
    for i := 0 to 3 do
    begin
      C.Brush.Style := bsSolid;
      C.Brush.Color := dotCols[i];
      C.Pen.Color := borderCol;
      C.Ellipse(cx + i * 28, barY + 8, cx + i * 28 + 16, barY + 24);
    end;
    DrawArrow(C, W div 2, 130, W div 2, barY + 4, hdrCol);
    C.Brush.Style := bsClear;
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(W div 2 - 110, 100, 'Bottom-Center: 4 Verse Status Circles');
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(W div 2 - 110, 114, '1: Gray  |  2: Yellow  |  3: Red  |  4: Green');

    // 3. Edit & Properties Checkboxes (Bottom-Right)
    DrawBadge(C, W - 165, barY + 4, '[✓] Edit   [ ] Properties', winBg, txtCol, hdrCol, True);
    DrawArrow(C, W - 95, 130, W - 95, barY + 2, hdrCol);
    C.Brush.Style := bsClear;
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(W - 210, 112, 'Bottom-Right: Mode Checkboxes');
  end;

  procedure DrawTopic3(C: TCanvas);
  begin
    DrawBox(C, 16, 36, W div 2 + 20, 198, winBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'VISUAL EDITOR  ([x] Edit is checked)');
    DrawBadge(C, 26, 66, 'Enter', winBg, txtCol, borderCol, False, 6, 2);
    C.Brush.Style := bsClear; C.Font.Style := []; C.Font.Color := dimCol;
    C.TextOut(92, 70, '-> <CI>  new line');
    DrawBadge(C, 26, 94, '2 x Enter', winBg, txtCol, borderCol, False, 6, 2);
    C.Brush.Style := bsClear; C.Font.Style := []; C.Font.Color := dimCol;
    C.TextOut(112, 98, '-> <CM>  new paragraph');
    DrawBadge(C, 26, 122, 'Shift+Enter', winBg, txtCol, borderCol, False, 6, 2);
    C.Brush.Style := bsClear; C.Font.Style := []; C.Font.Color := dimCol;
    C.TextOut(130, 126, '-> <CL>  line break');
    DrawBadge(C, 26, 156, 'Selected Verse Text', hiBg, hiTxt, hdrCol, True);

    DrawBox(C, W div 2 + 30, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 40, 42, 'LIVE CONTEXT PANEL PREVIEW');
    DrawBadge(C, W div 2 + 40, 66, 'Source | Dest | Ref 1 | Ref 2 | Interlinear', ptBg, ptTxt, borderCol, True, 6, 2);
    C.Brush.Style := bsClear;
    C.Font.Style := [fsItalic];
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 40, 110, 'Unlinked Dest words -> Italics on Save (Ctrl+S)');
    C.Font.Style := [];
    C.TextOut(W div 2 + 40, 134, 'Updates in real time while you edit.');
  end;

  procedure DrawTopic4(C: TCanvas);
  begin
    // Left: Hovered Words in Source & Destination
    DrawBox(C, 16, 36, W div 2 - 10, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'HOVER SPEECH ROUTING (Ctrl + T Toggle)');

    DrawBadge(C, 26, 66, 'Source Word:  λόγος  [🔊]', ptBg, ptTxt, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [];
    C.Font.Color := txtCol;
    C.TextOut(26, 94, '-> Routed to Source Voice (Greek / Hebrew)');
    C.Font.Color := dimCol;
    C.TextOut(26, 110, '   e.g. Microsoft Stefanos (Greek) / Asaf (Hebrew)');

    DrawBadge(C, 26, 134, 'Dest / Ref Word:  beginning  [🔊]', hiBg, hiTxt, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Color := txtCol;
    C.TextOut(26, 162, '-> Routed to Translation Voice (English / Local)');
    C.Font.Color := dimCol;
    C.TextOut(26, 178, '   Automatic punctuation stripping for clean speech');

    // Right: Voice / Speech Settings Dialog Schematic
    DrawBox(C, W div 2 + 4, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 16, 42, 'VOICE / SPEECH SETTINGS (Options Menu)');

    DrawBadge(C, W div 2 + 16, 64, 'Source Voice: [Microsoft Stefanos] [Test]', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 16, 92, 'Dest Voice:   [Microsoft Zira    ] [Test]', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 16, 120, 'Speed (-10..+10): ====O====   Volume: =======O=', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 16, 148, 'Trigger: (•) Hover   ( ) Ctrl+Hover   ( ) Alt+Hover', winBg, txtCol, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 16, 178, 'Auto-discovers Windows 10/11 OneCore & SAPI5 voices');
  end;

  procedure DrawTopic5(C: TCanvas);
  var
    sId: TSkinId;
    sk: TSkinColors;
    idx, cardW, xLeft: Integer;
  begin
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(16, 36, '7 BUILT-IN THEWORD SKINS (View > Skins (theWord Themes)  |  Shortcut: Ctrl + Shift + D)');

    cardW := (W - 32 - 6 * 6) div 7;
    idx := 0;
    for sId := Low(TSkinId) to High(TSkinId) do
    begin
      sk := GetSkin(sId);
      xLeft := 16 + idx * (cardW + 6);
      DrawBox(C, xLeft, 56, xLeft + cardW, 162, sk.WindowBg, sk.PanelBorder, 6);
      DrawBox(C, xLeft + 2, 58, xLeft + cardW - 2, 80, sk.PanelBg, sk.PanelBorder, 4);

      C.Brush.Style := bsClear;
      C.Font.Size := 7;
      C.Font.Style := [fsBold];
      C.Font.Color := sk.HeaderColor;
      C.TextOut(xLeft + 5, 63, sk.Name);

      C.Font.Style := [];
      C.Font.Color := sk.WindowText;
      C.TextOut(xLeft + 6, 88, 'Ἐν ἀρχῇ');
      C.Font.Color := sk.WindowTextDim;
      C.TextOut(xLeft + 6, 105, 'Unassoc.');

      DrawBadge(C, xLeft + 5, 126, 'Selected', sk.HighlightBg, sk.HighlightText, sk.PanelBorder, True, 4, 2);
      Inc(idx);
    end;

    DrawBadge(C, 16, 170, 'Ctrl + Shift + D : Instant Dark / Light Toggle', ptBg, ptTxt, borderCol, True);
    DrawBadge(C, 310, 170, 'Themes all Menus, Panels, Borders, Popups & Dialogs', hiBg, hiTxt, borderCol, True);
  end;

  procedure DrawTopic6(C: TCanvas);
  begin
    // Left: Complete Bible Module Export
    DrawBox(C, 16, 36, W div 2 - 6, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'RIGHT-CLICK -> EXPORT AS BIBLE MODULE');

    DrawBadge(C, 26, 64, 'Right-Click Any Bible Panel (Source/Dest/Ref)', winBg, txtCol, borderCol, False);
    DrawArrow(C, 130, 88, 130, 104, hdrCol);
    DrawBadge(C, 26, 106, '.ont/.ot/.nt | e-Sword (.bblx/.bbli) | USFM | USFX', hiBg, hiTxt, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [];
    C.Font.Color := txtCol;
    C.TextOut(26, 136, '• Full Import & Export: theWord, e-Sword, USFM, USFX, MySword');
    C.TextOut(26, 153, '• Apocrypha OT Books (Book >= 67) & Dan 13-14 / Ps 151 in Tree');
    C.TextOut(26, 170, '• Preserves Strong''s, Morphology, Red Letters, Italics & Notes');

    // Right: Smart Reload + Bidirectional theWord 7 Sync
    DrawBox(C, W div 2 + 6, 36, W - 16, 114, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 16, 42, 'SMART TEXT RELOAD DIFF');
    DrawBadge(C, W div 2 + 16, 61, 'Words Changed -> Red "Needs Review"', MakeRGB(243, 64, 93), clWhite, borderCol, True);
    DrawBadge(C, W div 2 + 16, 86, 'Tags Only Changed -> Keeps Green "Associated!"', MakeRGB(45, 150, 50), clWhite, borderCol, True);

    DrawBox(C, W div 2 + 6, 120, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 16, 126, 'BIDIRECTIONAL THEWORD 7 & TANGLAW BIBLE SYNC');
    DrawBadge(C, W div 2 + 16, 146, 'iBiblia  <==>  theWord 7 & Tanglaw Bible', ptBg, ptTxt, borderCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 16, 175, 'Both sync directions work simultaneously + Self-Healing DB');
  end;

  procedure DrawTopic7(C: TCanvas);
  begin
    // Left: Status Filter Dropdown & Prev/Next Filtered Buttons
    DrawBox(C, 16, 36, W div 2 - 8, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'VERSE STATUS FILTER (Top-Left Above TreeView)');

    DrawBadge(C, 26, 62, '[ Needs review (14)          ▼ ]', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, 26, 92, '[ < Prev (Shift+F8) ]', winBg, txtCol, borderCol, False);
    DrawBadge(C, 152, 92, '[ Next (F8) > ]', ptBg, ptTxt, borderCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(252, 96, '14 found');

    C.Font.Style := [];
    C.Font.Color := txtCol;
    C.TextOut(26, 124, '• Filters TreeView by: All / Not assoc. / Associating /');
    C.TextOut(26, 140, '  Needs review / Associated! with live counts.');
    C.Font.Color := dimCol;
    C.TextOut(26, 160, '• Automatically saved per-project (.ibb) & in iBiblia.ini.');

    // Right: Find in Project (Ctrl+F)
    DrawBox(C, W div 2 + 4, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 14, 42, 'FIND IN PROJECT (Ctrl + F  |  Edit > Find in Project...)');

    DrawBadge(C, W div 2 + 14, 62, 'Find what: [ G3056 / beginning      ]  [ Search ]', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 14, 90, 'Look in:   [ All text fields / Strong tags ▼ ]', ptBg, ptTxt, borderCol, True);
    DrawBadge(C, W div 2 + 14, 120, 'Gen 1:1   | In the beginning God created...', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, W div 2 + 14, 148, '[ Go to Verse ]  [ < Prev Match ]  [ Next Match > ]', winBg, txtCol, borderCol, False);
    C.Brush.Style := bsClear;
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 14, 176, 'Searches Dest, Source, Ref 1/2, Strong''s & Notes');
  end;

  procedure DrawTopic8(C: TCanvas);
  begin
    DrawBox(C, 16, 36, W div 2 + 10, 198, winBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, '1.1.0 VISUAL WYSIWYG & RAW ONT VERSE EDITOR');
    DrawBadge(C, 26, 62, '[I Added] [J Jesus] [Q Quote] [Title ▼] [N Note] [-A] [+A] [Color] [Raw]', pnlBg, txtCol, borderCol, True, 5, 2);
    DrawBadge(C, 26, 92, 'In the beginning [1] God created the heaven and the earth.', hiBg, hiTxt, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(26, 124, '• Preserves unaffected word associations when editing text');
    C.TextOut(26, 142, '• Switch instantly between WYSIWYG and Raw ONT markup');
    C.TextOut(26, 160, '• Properties checkbox opens 5+ line module properties editor');

    DrawBox(C, W div 2 + 20, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 30, 42, 'RICH TRANSLATOR NOTES (<RF>...<Rf>)');
    DrawBadge(C, W div 2 + 30, 66, 'Footnote Marker [1]  ->  Hover to Preview / Click to Edit', ptBg, ptTxt, borderCol, True);
    DrawBox(C, W div 2 + 30, 98, W - 28, 186, CurrentSkin.NoteBg, hdrCol, 5);
    C.Brush.Style := bsClear;
    C.Font.Color := CurrentSkin.NoteText;
    C.TextOut(W div 2 + 40, 108, 'Translator Note [1] (Gen 1:1):');
    C.Font.Style := [];
    C.TextOut(W div 2 + 40, 128, 'Heb. Bereshith — automatic or custom note markers');
    C.TextOut(W div 2 + 40, 146, 'with rich inline formatting and instant hover cards.');
  end;

  procedure DrawTopic9(C: TCanvas);
  begin
    DrawBox(C, 16, 36, W div 2 - 6, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'BIDIRECTIONAL (RTL / LTR) DIRECTWRITE RENDERING');
    DrawBadge(C, 26, 66, 'בְּרֵאשִׁית בָּרָא אֱלֹהִים  (Right-to-Left Hebrew)', winBg, txtCol, hdrCol, True);
    DrawBadge(C, 26, 98, 'Ἐν ἀρχῇ ἐποίησεν ὁ Θεός  (Left-to-Right Greek)', winBg, txtCol, borderCol, False);
    C.Brush.Style := bsClear;
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(26, 132, '• Hardware-accelerated DirectWrite text layout');
    C.TextOut(26, 150, '• Accurate Unicode grapheme & word selection');
    C.TextOut(26, 168, '• Per-panel Right-to-Left toggle & auto-detection');

    DrawBox(C, W div 2 + 6, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 16, 42, 'TRANSLATION MEMORY (Context Panel > Memory Tab)');
    DrawBadge(C, W div 2 + 16, 64, 'Order: [ Verse order / Rarest first ▼ ]', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 16, 94, 'ἀρχῇ (G746) -> "beginning" (42x), "first" (8x)', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, W div 2 + 16, 124, 'Hover occurrence -> Inline highlighted verse preview', ptBg, ptTxt, borderCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 16, 158, 'Click any occurrence to jump there; Alt+Left returns.');
  end;

  procedure DrawTopic10(C: TCanvas);
  begin
    DrawBox(C, 16, 36, W div 2 - 6, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'WIZARD, CUSTOM STATUSES & AI WORKFLOWS');
    DrawBadge(C, 26, 64, 'New Project Wizard: Association or Empty Translation', winBg, txtCol, borderCol, False);
    DrawBadge(C, 26, 94, 'AI Suggest & Batch AI Review (Association / Translation)', ptBg, ptTxt, hdrCol, True);
    DrawBadge(C, 26, 124, 'Custom Statuses (Up to 12) + Tree & 4-Dot Strip Sync', hiBg, hiTxt, borderCol, True);
    DrawBadge(C, 26, 154, 'Bottom-Left Status Bar: Realtime Saved / Unsaved Status', winBg, txtCol, borderCol, False);

    DrawBox(C, W div 2 + 6, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 16, 42, 'ICON FAMILIES & 3 TOOLBAR ICON SIZES');
    DrawBadge(C, W div 2 + 16, 64, 'Icon Families: Connected | Clareza | Mosaico | Folio', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 16, 94, 'Toolbar Icon Sizes: Small (16px) | Medium (22px) | Large (28px)', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, W div 2 + 16, 124, 'File > Save As (Ctrl+Shift+S) : Independent .bib Copy', ptBg, ptTxt, borderCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 16, 158, 'Configure in View > Toolbar icon size or Tools > Preferences.');
  end;

  procedure DrawTopic11(C: TCanvas);
  begin
    DrawBox(C, 16, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'COMPLETE NEW FEATURES MAP (v1.0.0.70 – v1.1.0.019) — QUICK SHORTCUT REFERENCE');

    DrawBadge(C, 26,  64, 'Mouse Wheel Up / Down : Select Source Word Left / Right', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, 26,  94, 'Hold Ctrl+Shift + Click : Associate Multiple Words Together', ptBg, ptTxt, borderCol, True);
    DrawBadge(C, 26, 124, 'Hover Bible Panel : Show Bottom Bar & < > Verse Chevrons', winBg, txtCol, hdrCol, True);
    DrawBadge(C, 26, 154, 'WYSIWYG Editor, Translator Notes, BiDi & Translation Memory', winBg, txtCol, borderCol, False);

    DrawBadge(C, W div 2 + 10,  64, 'Ctrl + Shift + D : Toggle Dark Mode / 7 theWord Skins', ptBg, ptTxt, borderCol, True);
    DrawBadge(C, W div 2 + 10,  94, 'Ctrl + T : Toggle Real-Time Hover Text-to-Speech (TTS)', winBg, txtCol, hdrCol, True);
    DrawBadge(C, W div 2 + 10, 124, 'Keys 1..9 / 4 Dots : Set Verse Status (Synced with Bible Tree)', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 10, 154, 'Ctrl + F : Find in Project   |   Shift+F1 : Open This User Guide', winBg, txtCol, borderCol, False);
  end;

begin
  if Sender = nil then ;
  W := pbIllust.Width;
  H := pbIllust.Height;
  if (W <= 0) or (H <= 0) then Exit;

  bmp := TBitmap.Create;
  try
    bmp.SetSize(W, H);
    winBg := CurrentSkin.WindowBg;
    pnlBg := CurrentSkin.PanelBg;
    borderCol := CurrentSkin.PanelBorder;
    txtCol := CurrentSkin.WindowText;
    dimCol := CurrentSkin.WindowTextDim;
    hdrCol := IfThen(CurrentSkin.Id = skClassic, MakeRGB(20, 95, 160), CurrentSkin.HeaderColor);
    hiBg := CurrentSkin.HighlightBg;
    hiTxt := CurrentSkin.HighlightText;
    ptBg := CurrentSkin.PointedBg;
    ptTxt := CurrentSkin.PointedText;

    bmp.Canvas.Font.Name := 'Segoe UI';

    // Outer frame
    DrawBox(bmp.Canvas, 0, 0, W, H, winBg, borderCol, 8);

    // Top banner inside illustration canvas
    DrawBox(bmp.Canvas, 1, 1, W - 1, 28, pnlBg, borderCol, 0);
    bmp.Canvas.Brush.Style := bsClear;
    bmp.Canvas.Font.Size := 9;
    bmp.Canvas.Font.Style := [fsBold];
    bmp.Canvas.Font.Color := hdrCol;
    bmp.Canvas.TextOut(14, 6, 'VISUAL ILLUSTRATION — ' + GetTopicHeaderTitle(FSelectedTopic));

    case FSelectedTopic of
      0: DrawTopic0(bmp.Canvas);
      1: DrawTopic1(bmp.Canvas);
      2: DrawTopic2(bmp.Canvas);
      3: DrawTopic3(bmp.Canvas);
      4: DrawTopic4(bmp.Canvas);
      5: DrawTopic5(bmp.Canvas);
      6: DrawTopic6(bmp.Canvas);
      7: DrawTopic7(bmp.Canvas);
      8: DrawTopic8(bmp.Canvas);
      9: DrawTopic9(bmp.Canvas);
      10: DrawTopic10(bmp.Canvas);
    else
      DrawTopic11(bmp.Canvas);
    end;

    pbIllust.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TFormUserGuide.BtnPrevClick(Sender: TObject);
begin
  if Sender = nil then ;
  if FSelectedTopic > 0 then
    SelectTopic(FSelectedTopic - 1);
end;

procedure TFormUserGuide.BtnNextClick(Sender: TObject);
begin
  if Sender = nil then ;
  if FSelectedTopic < GUIDE_TOPIC_COUNT - 1 then
    SelectTopic(FSelectedTopic + 1);
end;

procedure TFormUserGuide.BtnCopyClick(Sender: TObject);
begin
  if Sender = nil then ;
  if Assigned(memoGuide) then
    Clipboard.AsText := GetTopicHeaderTitle(FSelectedTopic) + sLineBreak +
                        '========================================================================' + sLineBreak +
                        memoGuide.Text;
end;

procedure TFormUserGuide.BtnCloseClick(Sender: TObject);
begin
  if Sender = nil then ;
  Close;
end;

procedure TFormUserGuide.UpdateLanguage(const ALang: string);
begin
  FLang := ALang;
  if FLang = 'pt' then
  begin
    Caption := 'iBiblia — Guia de Novos Recursos (Desde v1.0.0.71)';
    if Assigned(lblHeaderTitle) then
      lblHeaderTitle.Caption := 'Guia de Novos Recursos do iBiblia (v1.0.0.71 – v1.1.0.019)';
    if Assigned(lblHeaderSub) then
      lblHeaderSub.Caption := 'Janela Flutuante Interativa — Mantenha aberta enquanto pratica os recursos no iBiblia.';
    if Assigned(btnPrev) then btnPrev.Caption := '< Anterior';
    if Assigned(btnNext) then btnNext.Caption := 'Próximo >';
    if Assigned(btnCopy) then btnCopy.Caption := 'Copiar Guia';
    if Assigned(btnClose) then btnClose.Caption := 'Fechar';
  end
  else
  begin
    Caption := 'iBiblia — New Features User Guide (Since v1.0.0.71)';
    if Assigned(lblHeaderTitle) then
      lblHeaderTitle.Caption := 'iBiblia New Features User Guide (v1.0.0.71 – v1.1.0.019)';
    if Assigned(lblHeaderSub) then
      lblHeaderSub.Caption := 'Floating Interactive Guide — Keep this window open while practicing features directly in iBiblia.';
    if Assigned(btnPrev) then btnPrev.Caption := '< Previous Topic';
    if Assigned(btnNext) then btnNext.Caption := 'Next Topic >';
    if Assigned(btnCopy) then btnCopy.Caption := 'Copy Guide Text';
    if Assigned(btnClose) then btnClose.Caption := 'Close';
  end;
end;

procedure TFormUserGuide.ApplySkin(const ASkin: TSkinColors);
begin
  Color := ASkin.PanelBg;
  ApplySkinToForm(Self, ASkin);

  if Assigned(pnlHeader) then pnlHeader.Color := ASkin.PanelBg;
  if Assigned(pnlBody) then pnlBody.Color := ASkin.PanelBg;
  if Assigned(pnlSidebar) then pnlSidebar.Color := ASkin.PanelBg;
  if Assigned(pnlContent) then pnlContent.Color := ASkin.PanelBg;
  if Assigned(pnlIllust) then pnlIllust.Color := ASkin.PanelBg;
  if Assigned(pnlFooter) then pnlFooter.Color := ASkin.PanelBg;

  if Assigned(lblHeaderTitle) then
    lblHeaderTitle.Font.Color := IfThen(ASkin.Id = skClassic, MakeRGB(20, 95, 160), ASkin.HeaderColor);
  if Assigned(lblHeaderSub) then
    lblHeaderSub.Font.Color := ASkin.WindowTextDim;

  if Assigned(memoGuide) then
  begin
    memoGuide.Color := ASkin.WindowBg;
    memoGuide.Font.Color := ASkin.WindowText;
    ApplyDarkModeToControl(memoGuide, ASkin.IsDark);
    {$IFDEF WINDOWS}
    HookEditControl(memoGuide);
    {$ENDIF}
  end;

  {$IFDEF WINDOWS}
  if HandleAllocated then
    ApplyDarkModeToWindow(Self, ASkin.IsDark);
  if Assigned(btnPrev) and btnPrev.HandleAllocated then HookButtonControl(btnPrev);
  if Assigned(btnNext) and btnNext.HandleAllocated then HookButtonControl(btnNext);
  if Assigned(btnCopy) and btnCopy.HandleAllocated then HookButtonControl(btnCopy);
  if Assigned(btnClose) and btnClose.HandleAllocated then HookButtonControl(btnClose);
  {$ENDIF}

  if Assigned(btnPrev) then begin btnPrev.Font.Color := ASkin.PanelText; btnPrev.Invalidate; end;
  if Assigned(btnNext) then begin btnNext.Font.Color := ASkin.PanelText; btnNext.Invalidate; end;
  if Assigned(btnCopy) then begin btnCopy.Font.Color := ASkin.PanelText; btnCopy.Invalidate; end;
  if Assigned(btnClose) then begin btnClose.Font.Color := ASkin.PanelText; btnClose.Invalidate; end;
  if Assigned(pbSidebar) then pbSidebar.Invalidate;
  if Assigned(pbIllust) then pbIllust.Invalidate;
end;

procedure TFormUserGuide.FormShow(Sender: TObject);
begin
  if Sender = nil then ;
  LayoutFooterButtons;
  ApplySkin(CurrentSkin);
  {$IFDEF WINDOWS}
  ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  if Assigned(btnPrev) then HookButtonControl(btnPrev);
  if Assigned(btnNext) then HookButtonControl(btnNext);
  if Assigned(btnCopy) then HookButtonControl(btnCopy);
  if Assigned(btnClose) then HookButtonControl(btnClose);
  if Assigned(memoGuide) then HookEditControl(memoGuide);
  {$ENDIF}
end;

procedure ShowUserGuide(AOwner: TComponent);
begin
  if not Assigned(FormUserGuide) then
    FormUserGuide := TFormUserGuide.Create(AOwner);
  FormUserGuide.HandleNeeded;
  FormUserGuide.ApplySkin(CurrentSkin);
  FormUserGuide.Show;
  FormUserGuide.BringToFront;
end;

end.
