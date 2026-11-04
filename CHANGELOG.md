# Changelog

## 1.1.0

- Write and format verses visually, including titles, added words, words of Jesus, quotations, colors and relative font sizes; use Raw mode for theWord markup.
- Add and edit rich translator notes with automatic or custom markers and hover previews.
- Work with mixed Hebrew, Greek and translated text using bidirectional rendering and Unicode selection.
- Preserve unaffected Associations through text edits, with review before associated members are removed.
- Compare past translation choices in Translation memory, open exact highlighted occurrences and preview them without leaving the current verse.
- Create association or translation projects through a guided wizard, including an initially empty destination for new translations.
- Try experimental AI translation and Association, optional translation-plus-Association results and added-word marking, with batch review before accepted results are applied.
- Configure machine-default AI settings, project inheritance/overrides, flow-specific settings and reusable verse examples; keep Windows API secrets in local secure storage.
- Review progress with customizable statuses, status filters, a contextual guide, clearer workspace labels and a shortcut reference.
- Save the current project as an independent `.bib` copy with Save As.
- Export tagged and interlinear modules, richer concordances and selected-verse patches with clearer results and safer output publication, including supported updates to modules open in theWord.
- Use theWord 7 integration, faster navigation/rendering, improved imports, Association-memory consistency and cancellation handling.
- Choose English/Portuguese interface language and an icon family from application preferences.
- Install a complete portable Windows x64 package with pinned runtime dependencies and verified TLS support for optional AI connections.

Read the [illustrated release notes](docs/releases/1.1.0.md) and [installation/update guide](https://github.com/rubiot/ibiblia/wiki/Installation-and-updating-(English)) before upgrading. Keep a backup of older `.bib` projects; API credentials are local to a Windows user and do not travel with project copies.

This release follows the public `v1.0.0.68b` baseline. Older historical notes remain in `historico.txt` and `Histórico.txt`.

## 1.0.1.000 – 1.0.1.055 History (Restored from v1.0.1.055)

```text
1.1.0.001 (2026-11-04)
	* [C] Fixed Dictionary Popup size persistence across application restarts (FormDictionaryPopup.pas).
	* [C] Restored Module Properties memo height (at least 5 lines high, overlaying the Bible panel instead of being compressed below DirectWriteView) when Properties is checked (formprincipal.pas).
	* [C] Made individual Bible bottom toolbars (Name caption, status radio circles, Edit, and Properties) appear on mouse hover in sync with the per-panel chevron navigation arrows (formprincipal.pas).
	* [N] Added 3 top bar menu icon size options (Small 16px, Medium 22px, Large 28px) in View > Toolbar icon size and Tools > Preferences (ui_icons.pas, FormApplicationPreferences.pas, formprincipal.pas).
	* [C] Fixed skin theme rendering on the dropdown arrow sections of split toolbar buttons (ToolButton6 and FTheWordToolButton) across all themes (unitskins.pas).
	* [C] Fixed Main Bible Tree status synchronization with the Destination panel 4 radio buttons status during word association, AI/statistical suggestions, and verse edits (Project.pas, formprincipal.pas).
	* [C] Moved the realtime Saved / Unsaved changes status indicator to the bottom-left corner of the bottom status bar directly below the status summary box (formprincipal.pas, unitskins.pas).
	* [N] Updated the New Features Guide (Shift+F1) with dedicated illustrated sections for all iBiblia 1.1.0.0 features (unitabout.pas, FormNewFeaturesGuide.pas).

1.1.0.0 (2026-11-03)
	* [N] Merged iBiblia 1.1.0 with the 1.0.1.055 64-bit edition features.
	* [N] Visual WYSIWYG and Raw ONT verse editor with titles, added words, Words of Jesus, OT quotations, font colors, relative font sizes, and rich translator notes with hover previews.
	* [N] Bidirectional rendering and Unicode selection for mixed Hebrew, Greek, and translated text.
	* [N] Translation memory comparison with highlighted verse occurrences and hover previews.
	* [N] Guided project creation wizard, customizable verse statuses, contextual guide, and Save As (.bib copy).
	* [N] Experimental AI translation and association flows with batch review and Windows local secure credential storage.
	* [C] Theme-aware verse editing interface, word selection/highlighting contrast, chapter scrollbar theming, multi-direction dictionary popup positioning, short.title panel captions, and Project Settings Default Dictionaries toggle.

1.0.1.055 (2026-11-03)
	* [C] Fixed Access Violation on application exit in TGerSugestoes.Destruir (sugestao.pas) by detaching FConnTmp.Transaction := nil and FConnVerbos.Transaction := nil before freeing the TSQLTransaction instances (preventing TDatabase.SetTransaction from dereferencing freed transaction objects) and safely closing/unpreparing/detaching all 15 TSQLQuery instances.
	* [C] Fixed Access Violation on application exit in TFrmApplyPatch.FormDestroy (formapplypatch.pas) and TVersiculo.Destruir (versiculo.pas) by adding destructor Destroy; override; to TVersiculo, TMemoVersiculo, and TProjeto, and clearing FPanel.OnResize before freeing FSintagmas and child panels so destroying ScrollBoxSrcText/ScrollBoxDstText never triggers OnResize on freed TVersiculo instances.
	* [C] Fixed use-after-free in TFrmPrincipal.ActionFecharProjetoExecute (formprincipal.pas) and TChapterView (chapterview.pas) by detaching FChapterView.Project := nil before destroying ProjetoAtual, removing premature project unsubscription from TChapterView.SetEnabled, and detaching FRawMemo.Parent := nil before freeing FRawMemo.
	* [C] Hardened TProjeto.Fechar, TProjeto.Destruir, and TProjeto.LiberarObjetoQuery (projeto.pas), stopped TwSyncThread on exit, guarded HandleAllocated before reading .Handle in TFrmPrincipal.FormDestroy, TFrmExportarProjeto.FormDestroy, and Unhook*Control, and added WM_NCDESTROY/WM_DESTROY auto-unhooking and ApplicationShuttingDown guards across all Win32 SubclassProcs (unitskins.pas, formprincipal.pas, formexportar.pas).

1.0.1.054 (2026-10-31)
	* [C] Fixed empty even-numbered verses when loading theWord (.ont/.ot/.nt) or USFM (.usfm/.sfm) modules with CR-CR-LF (\r\r\n / $0D$0D$0A) line endings (such as LXX Brenton+, LXX Rahlfs+, and SweteLXX+) by normalizing #13#13 to #13 in LoadModuleLinesNormalized (projeto.pas) and LoadUSFMFile/ImportUSFMBible (myswordmodule.pas), and sanitizing CR/LF in ExportTheWordBible.
	* [C] Updated TFrmEscolherVerseRules.AddVerseRule (formverserules.pas) to leave display/hyperlink verse.rule entries (_ORIGWORD_, _NOLINK_, <NB, <ST, <sup>, <font) unchecked by default on import, and updated TONTParser.ParseLine (ontparser.pas) to treat <NB...<Nb> and <ST...<St> blocks as metadata.
	* [C] Updated EnsureTanglawSyncBridge (twAutomate.pas) to auto-patch src/lib/theword.js and src/lib/modprops.js inside installed Tanglaw app.asar archives so CR-CR-LF (.ot/.ont/.nt) modules load with zero empty even verses.

1.0.1.053 (2026-10-31)
	* [C] Added LazUTF8 to the interface uses clause of projeto.pas for UTF8LowerCase resolution and converted TGerSugestoes.Destruir to a procedure invoked by destructor Destroy; override; in sugestao.pas.

1.0.1.052 (2026-10-31)
	* [C] Fixed association persistence across application restarts for Tokenizer V1 projects (such as Gospels-Ed.bib) by loading TTokenizerFactory.PreferredVersion immediately after FTblInfo.Open before HealMisplacedPairsInDatabase runs.
	* [C] Restricted HealMisplacedPairsInDatabase so it never clears valid associations on startup, added lossless V1/V2 tokenizer pair remapping in PosRolagemVersiculo, and added automatic recovery of wiped verses from association_memory_occurrences.
	* [C] Fixed SQLite multi-connection lock contention between TGerSugestoes and TSqlite3Dataset by closing all SELECT cursors after reads, flushing TGerSugestoes transactions prior to ApplyUpdates, overriding TGerSugestoes.Destroy, and executing SalvarPares before SalvarAssociacoes.

1.0.1.051 (2026-10-31):
 * Removed word/dictionary lookup synchronization between iBiblia and Tanglaw Bible while preserving full bidirectional verse synchronization ("Sync Tanglaw to iBiblia verse" and "Sync iBiblia to Tanglaw verse") and all Tanglaw UI size, window bounds, dock splitter, and panel zoom persistence fixes.
 * Reverted QuandoPalavraClicada in formprincipal.pas to sync word/dictionary lookups exclusively with theWord, and removed LookupExternalWordOrStrong / SyncTanglawWordLookup across projeto.pas, formprincipal.pas, twsyncthread.pas, and twAutomate.pas.
 * Updated EnsureTanglawSyncBridge in twAutomate.pas and Tanglaw-Bible-0.5.5-Windows-Installer.zip so Tanglaw syncs verses only and persists all UI/window sizes across restarts.

1.0.1.050 (2026-10-31):
 * Fixed Tanglaw Bible UI size, window bounds, dock layout splitters, and per-panel zoom persistence across application restarts.
 * Added windowBounds & guideWindowBounds persistence (width, height, x, y, maximized, fullScreen with multi-monitor workArea clamping), atomic saveSettings() writes, and deep-merged settings:set in Tanglaw src/main.js.
 * Updated Tanglaw options.js, app.js, dock.js, modules.js, workbench.css, style.css, and index.html so Menus & Panels UI size (--ui-size up to 22 px, including Ctrl+MouseWheel over toolbars/captions), Bible view zoom, all 10 docked panel zooms, and dock splitter drags persist across restarts.
 * Updated EnsureTanglawSyncBridge and SyncTanglawVerse in twAutomate.pas to auto-upgrade installed Tanglaw app.asar archives with UI size persistence and eliminate concurrent settings.json file-write races while Tanglaw is running.

1.0.1.049 (2026-10-31):
 * Added bidirectional Tanglaw Bible verse and word/dictionary lookup synchronization under Options ("Sync Tanglaw to iBiblia verse" and "Sync iBiblia to Tanglaw verse") across twAutomate.pas, twsyncthread.pas, projeto.pas, formprincipal.pas, formprincipal.lfm, and languages/*.po.
 * Added SyncTanglawVerse, SyncTanglawWordLookup, GetTanglawCurrentRef, MapIBibliaBookToTanglaw, TanglawBookIDToName, and EnsureTanglawSyncBridge in twAutomate.pas, supporting all 66 canonical books and 21 Apocrypha/Deuterocanonical books (including Daniel 13-14 and Psalms 151 mapping).
 * Added ResolveTanglawRefToProjectRef and LookupExternalWordOrStrong in projeto.pas so clicking/hovering words in iBiblia opens their Strong's, Morphology, Lemma, and Word Dictionary entries in Tanglaw's Lookup panel, and clicking words/tags in Tanglaw highlights the matching word in iBiblia and opens iBiblia's dictionary popup.

1.0.1.048 (2026-10-30):
 * Updated all changelogs (Histórico.txt, historico.txt, Help > About > Changelog, Help > New Features User Guide, and README.md) to document USFM (.usfm, .sfm) & USFX (.usfx, .xml) Bible import/export, e-Sword Apocrypha OT books & Deuterocanonical chapters in the Bible Books Tree, non-encrypted e-Sword (.bblx, .bbli, .bbl) import/export, auto-save on selecting "Associated", and Context Panel popup dictionary inheritance.

1.0.1.047 (2026-10-30):
 * Added full import and export support for USFM 2.x/3.0 (.usfm, .sfm) and USFX XML (.usfx, .xml) Bible files in myswordmodule.pas (LoadUSFMBibleWithApocrypha, LoadUSFXBibleWithApocrypha, ExportUSFMBible, ExportUSFXBible, ConvertONTToUSFM, ConvertONTToUSFX, USFMBookCodeToID, BookIDToUSFMCode), modeled on Simple Bible Reader (USFXBibleFormat / TheWordBibleFormat) and the eBible.org USFX specification.
 * Added automatic multi-file USFM directory detection: selecting any single .usfm/.sfm book file in a folder automatically loads all sibling .usfm/.sfm book files into the project across all 66 canonical books (GEN-REV) and 21 Apocrypha / Deuterocanonical 3-letter book codes (1ES, 2ES, TOB, JDT, 1MA-4MA, MAN, WIS, SIR, BAR, LJE, S3Y, SUS, BEL, ESG, PS2, PSS, ODE, LAO, plus DAG Daniel 1-14).
 * Preserves Strongs numbers, morphology tags, and lemmas (\w word|lemma="..." strong="..." x-morph="..."\w* and <w s="..." m="..." l="...">), Words of Jesus (\wj / <wj>), added/italic words (\add / <add>), bold (\bd / <bd>), section headings (\s1 / <s>), and footnotes (\f / <f>).
 * Integrated USFM and USFX file filters and import/export handlers across formnovoprojeto.pas, formpropprojeto.pas, formexportar.pas, and projeto.pas.

1.0.1.046 (2026-10-29):
 * Fixed missing leading single quote on line 114 of CHANGELOG_TEXT in unitabout.pas and added Stage 17 Pascal string literal quote audit to audit_full_build.py.

1.0.1.045 (2026-10-29):
 * Added full support for Apocrypha / Deuterocanonical Old Testament books (Book >= 67: 1 Esdras, 2 Esdras, Tobit, Judith, 1-4 Maccabees, Prayer of Manasseh, Wisdom, Sirach, Baruch, Letter of Jeremiah, Prayer of Azariah, Susanna, Bel and the Dragon, Psalm 151, Psalms of Solomon, Odes, Laodiceans) and extra Deuterocanonical chapters in canonical OT books (Daniel 13-14, Psalms 151, Esther 11-16).
 * Added LoadESwordBibleWithApocrypha and IdentifyApocryphaBookName in myswordmodule.pas to distinguish 12-book (67..78) and 16/19-book (67..86) e-Sword Apocrypha numbering schemes and backfill the 57 canonical KJV verses omitted by kjva.bbli.
 * Added RebuildBookStructure, EnsureExtraVersesInDatabase, SyncESwordApocryphaIfNeeded, GetBookName, GetBookChapterCount, GetChapterVerseCount, and GetAdjacentChapter in projeto.pas so Apocrypha OT books appear right after Malachi at the end of the Old Testament in the Bible Books Tree (FArvore), Context Panel (chapterview.pas), and Project Search.

1.0.1.044 (2026-10-29):
 * Fixed TChapterView.GetWordInfoAt in chapterview.pas to use KControls 1.7.3 TKMemo.PointToIndex and Blocks.IndexToBlockIndex instead of PointToBlock/BlockToRect.

1.0.1.043 (2026-10-29):
 * Fixed FPC 3.2.2 SQLdb TSQLQuery preparation in TESwordModule.Create (myswordmodule.pas) by removing non-existent ParamsCheck property and initializing the RTF state stack.

1.0.1.042 (2026-10-29):
 * Added full import and export support for all non-encrypted e-Sword Bible files (.bblx RTF, .bbli HTML, and SQLite .bbl).
 * Added TESwordModule in myswordmodule.pas with complete e-Sword RTF-to-ONT, HTML-to-ONT, ONT-to-RTF (.bblx Version 2), and ONT-to-HTML (.bbli Version 4) conversion covering Strong's tags, TVM/Robinson morphology, Words of Christ in red, italics, bold, underline, headings, footnotes, and Unicode escapes.
 * Integrated e-Sword Bible import into New Project (formnovoprojeto.pas) and Project Properties (formpropprojeto.pas) with automatic OT/NT/Whole-Bible scope detection and encrypted-module detection.
 * Integrated e-Sword .bblx and .bbli export into File > Export (formexportar.pas) for both Tagged Destination and Interlinear Bibles, as well as each panel's right-click "Export as Bible module..." menu (projeto.pas).

1.0.1.041 (2026-10-29):
 * Added automatic project save trigger when selecting the "Associated" radio button (via RadioGroupStatus, the Destination verse status bar circle, or keyboard shortcut '4').
 * Enabled the Context Panel (TChapterView) to inherit and display the popup dictionary (Strong's, Morphology, Translation Memory, and English Dictionary fallback) for associated/linked words between the Source and Destination panels (including multi-word phrase associations and Interlinear/Intralinear views).

Version 1.0.1.040 (64-bit Edition) - 2026-10-29
- Fixed "Sync theWord to iBiblia verse" (`WM_COPYDATA` verse & dictionary lookup broadcast on 64-bit Windows):
  * Resolved 64-bit struct packing collision where `LCLType.TCopyDataStruct` (`packed record`, 20 bytes on Win64 with `lpData` at offset 12) shadowed `Windows.TCopyDataStruct` (24 bytes on Win64 with 4-byte C-ABI alignment padding and `lpData` at offset 16), causing the 64-bit Windows kernel to reject `WM_COPYDATA` messages sent to theWord.
  * Added explicit `TTWCopyDataStruct` with `{$IFDEF CPU64} ReservedPadding: DWORD; {$ENDIF}` and `TWAutomateUtils.BroadcastCopyDataToTheWord` in `twAutomate.pas` + `formprincipal.pas`.
  * Prioritized `TW_MAINFORM_CLASSNAME` (`.UnicodeClass`) in `TWAutomateUtils.IsTwRunning` and added `FNavigatingVerse` re-entrancy guards in `ActionVersoPrimeiroExecute` and `ActionVersoAnteriorFiltradoExecute`.
  * Added Stage 12 static verification check in `audit_full_build.py` to prevent `LCLType.TCopyDataStruct` shadowing.

Version 1.0.1.038 (64-bit Edition) - 2026-10-28
- Integrated upstream v1.1.0 reliability & workflow improvements while preserving all v1.0.0.70–v1.0.1.034 features:
  * Export Bible (.ont/.ot/.nt) and Concordance (.dct.twm) modules while open in theWord via staged publishing + shared-stream overwrite fallback (`PublishStagedFile`).
  * Implemented safe Project "Save As..." (`File > Save as...` / `Ctrl+Shift+S` / Toolbar button) with transactional staging and MRU update.
  * Added per-project remembered export paths for Tagged Destination, Interlinear, Analytical/Synthetic Concordance (`.dct.twm`), and all 4 Bible panels.
  * Added apostrophe variant normalization (`NormalizeApostrophes`) across word associations, statistical suggestions, and smart reload word-diff checks.
  * Highlighted the active verse's current translation (`[current]`) inside the Translation Memory popup list.
  * Added `TMemoVersiculo.PruneEmptyFormattingTags` to automatically remove empty wrapper tags after text deletion in the verse editor.
  * Updated `TTheWordDictionary` (`.dct.twm`) to store uncompressed `BLOB` `content.data` and required 2026 Book Module Developer Guide metadata.
  * Fixed reversed `Pos()` arguments in `TProjeto.ExportarTextoDestinoComStrongs` and fixed `FQryTranslationMemory` / `TMySwordModule` memory leaks.

v1.0.1.034 (2026-10-28)
  * Context Panel: Unassociated Destination text words automatically render in italics (<FI>...<Fi>) upon file save (Ctrl+S / File > Save).
  * User Guide & README: Added visual illustration and documentation for Context Panel unassociated Destination words in italics on save.

1.0.1.033 (2026-10-28):
 * Fixed "Sync theWord to iBiblia verse": restored FArvore.OnChange in TProjeto.FiltrarArvore(-1), suppressed re-entrant filter/status events during verse loading, restored all 4 official theWord 6/7 window classes in TWAutomateUtils.IsTwRunning with strict EnumWindows filtering, and fixed 64-bit buffer bounds in SyncTheWordDict.

Version 1.0.1.032 (64-bit Edition) - 2026-10-27 20:47
* Fixed TFontStyles set assignment in DrawBadge (unitabout.pas line 1130: replaced IfThen(BoldFont, [fsBold], []) with if BoldFont then C.Font.Style := [fsBold] else C.Font.Style := []).

Version 1.0.1.031 (64-bit Edition) - 2026-10-27 20:26
* Added dedicated floating "New Features User Guide" window with interactive visual illustrations under Help > New Features User Guide... (F1) above About... (unitabout.pas, formprincipal.pas, formprincipal.lfm):
  - Collates all new features (excluding bug fixes) introduced since v1.0.0.71 into 7 structured feature topics plus a complete combined reference:
    1. Mouse Wheel Source Word Selection & Ctrl+Shift Multi-Word Association
    2. Southeast Cursor Dictionary Popup & 4-Slot Default Dictionaries (including English Word .dct.twm & compressed=1 support)
    3. In-Verse Controls: Hover-Only Navigation Chevrons (< >), 4-Color Status Circles & Edit/Properties Checkboxes
    4. Shared Master Formatting Ribbon, Floating Selection Mini-Toolbar & Live Context Panel Preview
    5. Real-Time Text-to-Speech (TTS) Hover Reader (Greek, Hebrew & English OneCore/SAPI5 voices)
    6. Dark Mode UI & 7 theWord Color Themes (Classic, theWord Dark, Night, Sepia, Slate, Solarized Dark, High Contrast)
    7. Complete Bible Module Export (.ont/.ot/.nt), Smart Reload "Needs Review" Diff & Concurrent Bidirectional theWord 7 Sync
    8. Complete Quick-Reference Summary of All New Features & Keyboard/Mouse Shortcuts
  - Features a custom-drawn visual diagram canvas (TPaintBox) at the top of each topic illustrating how to use the feature, full theme synchronization across all 7 skins, Copy Guide button, and modeless floating behavior so users can keep the guide open while working in iBiblia.

Version 1.0.1.030 (64-bit Edition) - 2026-10-27 20:06
* Fixed compilation error in versiculo.pas ("Identifier not found GetKeyState" at lines 2153-2154) by adding LCLIntf to the uses clause and using Shift * [ssCtrl, ssShift, ssAlt] in TVersiculo.OnMouseWheel.

Version 1.0.1.029 (64-bit Edition) - 2026-10-27 19:55
* Restored Ctrl+Shift multi-word selection for association tasks alongside mouse wheel word selection (syntagm.pas, versiculo.pas, projeto.pas):
  - Removed the selection-clearing check in TSyntagm.DoOnLeftClick that prevented selecting multiple words simultaneously while holding Ctrl+Shift.
  - Added modifier key guards (Ctrl, Shift, Alt) in TVersiculo.OnMouseWheel and TProjeto.HandleMouseWheelForSourceWords so accidental scroll wheel movement while holding Ctrl+Shift never clears active multi-word selections.

Version 1.0.1.028 (64-bit Edition) - 2026-10-27 19:36
* Added mouse wheel / scroll word selection in the Source text panel (versiculo.pas, syntagm.pas, projeto.pas, chapterview.pas, formprincipal.pas):
  - Scrolling up (WheelDelta > 0) selects words in the Source text to the left one by one, and scrolling down (WheelDelta < 0) selects words in the Source text to the right one by one.
  - Works seamlessly whenever the mouse cursor is hovering inside either the Source text panel (ScrollBoxSrcVerse) or the Destination text panel (ScrollBoxDstVerse), regardless of which control currently holds keyboard focus.
  - Automatically scrolls the selected Source word into view, highlights any associated Destination word pairs, updates Strong's highlights across all panels, and updates the active dictionary popup.

Version 1.0.1.027 (64-bit Edition) - 2026-10-27 16:38
* Fixed export errors ("Invalid syntagm index: 18" on Genesis 1:1 reported under the last viewed reference "1 Kings 1:2") and added automatic database self-healing (versiculo.pas, projeto.pas, formprincipal.pas):
  - Root Cause Identified: In export-errors.txt, row '1,1,1' (Genesis 1:1, 21 tokens) contained the pare_pares string of Genesis 41:32 ('1,41,32', 50 tokens up to <par a="49" b="60">), which was written into '1,1,1' when FTblPares moved to the first row without updating FAVersiculo, and FormattedReference printed '1 Kings 1:2' because FReference was not updated while FScrollEventsEnabled = False during export.
  - Added TVersiculo.AreParesValidForVerse (versiculo.pas) and TProjeto.HealMisplacedPairsInDatabase (projeto.pas), executed automatically on TProjeto.Abrir and before all Export operations, to detect any misplaced pare_pares on '1,1,1' (or any row), restore Genesis 41:32's pairs to '1,41,32' if unassociated, and clean the invalid pare_pares from '1,1,1' so exports complete with zero errors.
  - Added FLoadedVerseID verification in PreRolagemVersiculo, SalvarPares, and SalvarTexto, reference-counted FScrollingSessionDepth in StartScrollingSession/FinishScrollingSession, FArvore.OnChange suppression in SelectTreeNode/FiltrarArvore, and ScrollEventsEnabled guard in TFrmPrincipal.SyncToTwRef so associations can never be saved to a mismatched dataset row.
  - Updated TProjeto.GetFormattedReference to read FTblPares.FieldByName('pare_id') when FScrollEventsEnabled is False so export diagnostics always report the true verse being processed.

Version 1.0.1.026 (64-bit Edition) - 2026-10-27 12:23
* Made verse navigation chevron arrows (‹ ›) appear only while the mouse cursor is hovering inside the Bible panel where they belong (unitskins.pas, versiculo.pas, syntagm.pas, projeto.pas, formprincipal.pas):
  - Added IsMouseCursorInControl in unitskins.pas using GetWindowRect, WindowFromPoint, IsChild, and THintWindow tooltip awareness to determine which of the 4 Bible panels (Source, Destination, Reference 1, Reference 2) currently contains the mouse cursor.
  - Added FMouseInPanel, UpdateNavHoverState, and an on-demand 40ms FNavHoverTimer in TVersiculo (versiculo.pas), hooked across FPanel, word labels (TSyntagm), FNavPanel, FTranslationPanel, FStatusBarPanel, FTabPanel, and Application.OnIdle (OnAppIdleCheckAutoSelect) so hovering inside any Bible panel immediately shows its chevron arrows while hiding chevron arrows in all other Bible panels.

Version 1.0.1.025 (64-bit Edition) - 2026-10-27 11:57
* Fixed OnKeyUp registration in TFrmPrincipal (formprincipal.pas: replaced Application.AddOnKeyUpHandler/RemoveOnKeyUpHandler with Self.OnKeyUp := @OnAppKeyUp alongside KeyPreview = True, Application.AddOnIdleHandler, and FAutoSelectTimer).

Version 1.0.1.024 (64-bit Edition) - 2026-10-27 11:46
* Added automatic selection of the next unassociated Source word when Ctrl+Shift keys are released after associating target words (syntagm.pas, versiculo.pas, projeto.pas, formprincipal.pas):
  - When holding Ctrl+Shift (or Shift / Ctrl+Right-Click) and clicking/highlighting one or more target words in the Destination text to associate them with the selected Source word(s), PendingAutoSelectNext is armed on the paired verses.
  - As soon as the Ctrl+Shift modifier keys are released (detected via OnAppKeyUp, OnAppIdleCheckAutoSelect, and a 30ms on-demand FAutoSelectTimer when the mouse is stationary), CompletePendingAutoSelectNext automatically clears the previous selection, highlights/selects the next unassociated word in the Source text, scrolls it into view if needed, and triggers OnClick for theWord synchronization.

Version 1.0.1.023 (64-bit Edition) - 2026-10-27 11:30
* Unified Sepia (Parchment) skin consistency across the entire application (unitskins.pas, formprincipal.pas, formpopup.pas, bibletreeview.pas, and all secondary dialogs):
  - Added IsThemedSkin (CurrentSkin.IsDark or CurrentSkin.Id = skSepia) so the Main Menu Bar, dropdown/popup menus, toolbar, comboboxes, checkboxes, buttons, groupboxes, tab controls, non-client borders, and secondary dialogs all render using Sepia's warm parchment palette (#F4EEDB / #E2D7C3 / #C9BC9C / #2D241E).
  - Added DrawThemedPopupMenuItem in unitskins.pas to custom-draw dropdown and context popup menu items in Sepia (warm #F4EEDB background, #E2D7C3 left gutter, #C9BC9C border & separators, #D9C289 hover bar, and #8A4E2C checkmarks/radio bullets), and updated ApplyDarkModeToWindow to call fnSetPreferredAppMode(pamForceLight) + fnFlushMenuThemes when switching out of dark skins.
  - Added OnContextTabButtonPaint in formprincipal.pas so the Source | Destination | Ref 1 | Ref 2 context tab buttons and filter navigation buttons render with Sepia HighlightBg (#D9C289), VerseNumColor (#8A4E2C) top accent bar, and dark sepia text (#2D241E) instead of dark-mode charcoal.

Version 1.0.1.022 (64-bit Edition) - 2026-10-27 11:15
* Fixed white border lines around the 4 Bible panels (ScrollBoxSrcVerse, ScrollBoxDstVerse, ScrollBoxRef1Verse, ScrollBoxRef2Verse), TreeView1, container panels, and splitters across all themed skins (unitskins.pas, formprincipal.pas, formprincipal.lfm, versiculo.pas):
  - Added PaintControlNCBorder in unitskins.pas and hooked WM_NCPAINT, WM_PAINT, WM_SIZE, WM_WINDOWPOSCHANGED, and scroll messages in ScrollBoxSubclassProc and TreeViewSubclassProc to overpaint the 2px WS_EX_CLIENTEDGE non-client border (1px outer CurrentSkin.PanelBorder + 1px inner CurrentSkin.WindowBg / CurrentSkin.TreeBg) and the 17x17 scrollbar dead-space corner square.
  - Removed bvRaised 3D white bevels (BevelOuter := bvNone) from Panel1, CenterPanel, and BottomPanel in formprincipal.lfm and formprincipal.pas, and aligned all 4 Bible panels flush inside CenterPanel.
  - Added OnSplitterPaint in formprincipal.pas so Splitter1..Splitter6 render with CurrentSkin.PanelBorder and subtle CurrentSkin.WindowTextDim grip dots instead of white UxTheme gripper dots.
  - Updated SetControlThemeMode to always invalidate RDW_FRAME when switching between dark skins so non-client borders immediately adopt the new skin's PanelBorder and WindowBg colors.

Version 1.0.1.021 (64-bit Edition) - 2026-10-27 08:06
* Fixed HPEN type identifier error in DrawDarkMenuSeparator (unitskins.pas line 905: replaced undeclared HPEN with THandle) and added explicit PtrUInt pointer casts for lParam struct pointers.

Version 1.0.1.020 (64-bit Edition) - 2026-10-27 07:58
* Fixed white outer 3px borders and white separator rows in dropdown and context popup menus in dark mode (unitskins.pas, formprincipal.pas):
  - Added thread-scoped WH_CALLWNDPROC / WH_CALLWNDPROCRET menu hooks and #32768 popup menu window subclassing (PopupMenuSubclassProc, PaintPopupMenuBorder) to overpaint the classic MNDrawFullNC 3px non-client border (1px COLOR_3DSHADOW + 2px COLOR_MENU) on WM_PRINT (with SaveDC/RestoreDC around offscreen animation bitmaps), WM_NCPAINT, WM_ERASEBKGND, and WM_PAINT using dark popup background RGB(43, 43, 43) and 1px CurrentSkin.PanelBorder.
  - Added DrawDarkMenuSeparator and PreFillDarkMenuItem to fill separator item rows (Caption = '-') and submenu item backgrounds with RGB(43, 43, 43) and draw a 1px horizontal divider in CurrentSkin.PanelBorder, eliminating the white/light-grey COLOR_MENU background left un-erased by LCL's VistaMenuDraw.

1.0.1.018
- Fixed Pascal character set literal in BuscarDefinicaoPalavraInglesNaQuery (projeto.pas lines 2217 and 2219) where backslash character literal was emitted as empty string '' instead of '\'.

1.0.1.017
- Added "English Word / Bible Dictionary (*.dct.twm)" option under Default Dictionaries Settings (saved as default_english_dict / global_english_dict in iBiblia.ini).
- Added automatic mouse-hover English dictionary lookup (ObterDefinicaoPalavraIngles / BuscarDefinicaoPalavraInglesNaQuery) across all 4 Bible panels (Source, Destination, Reference 1, Reference 2) for any English word or proper name without a Strong's number tag, including case-insensitive headword matching, possessive/inflectional stem fallbacks, and compressed/redirect topic support.

Versão 1.0.1.016 (2026-10-26)
- Fix (unitskins.pas, formpropprojeto.pas): Fixed TTabControl dark mode rendering in Project -> Settings (Properties, TFormPropProjeto) where the internal LCL FNoteBook (TCustomTabControl) and outer TTabControl container previously overlapped double-drawn tab headers and left the entire body area below y=30 painted stark white (#F9F9F9) with unreadable light-grey labels.
- Fix (unitskins.pas): Added EditMemoSubclassProc (HookEditControl) to paint a 2px dark border (CurrentSkin.PanelBorder / WindowBg) over the white 3D WS_EX_CLIENTEDGE frame on TEdit, TLabeledEdit, and TMemo controls, and routed TCustomMemo through 'DarkMode_Explorer' so vertical/horizontal scrollbars render in dark mode.

Version 1.0.1.015 (64-bit Edition) - 2026-10-26
- Restored dictionary popup RTF font colors to match active skin settings (ASkin.NoteText) in AdaptRtfForSkin (formpopup.pas), fixing reddish text caused by mapping \plain / \pard / \par to \cf1 (maroon/red in dictionary color tables) instead of \cf0.
- Restored full hover dictionary popup behavior in the Source text box (projeto.pas) so hovering any word in the Source text (highlighted, unassociated, or associated) displays its dictionary popup as usual while keeping the automatic southeast popup inside the Destination panel.

Version 1.0.1.014 (64-bit Edition) - 2026-10-26
- Fixed Hebrew Strong's dictionary lookup in TProjeto.ObterDefinicaoStrong and TProjeto.BuscarDefinicaoStrongNaQuery so Hebrew Strong's numbers (H-codes) strictly query the selected Hebrew Lexicon dictionary under Default Dictionaries and never fall back to converting H-codes into Greek G-codes on the Greek EPV dictionary.
- Added unpadded (H123), lowercase (h123), zero-padded (H0123, H00123), compound/prefixed (Hc/H1234), sub-entry (H1234a), topics_wordindex, subject headword prefix, and meta=_twmgc_ redirect link resolution for Hebrew and Greek Strong's dictionary lookups.
- Fixed hover popup handling so hovering words in Reference 1 / Reference 2 or associated words displays their dictionary entry normally without interference from the Destination-panel selected-Source popup timer.

Version 1.0.1.013 (64-bit Edition) - 2026-10-26 13:21
- Added rule for hovering associated words inside the Destination text: when the mouse pointer hovers over an already-associated word in the Destination text, the southeast selected-Source popup is temporarily disabled and the normal dictionary popup for that hovered Destination word appears under it as usual; moving off the associated word back into the Destination panel automatically resumes the southeast popup for the selected unassociated Source word.

Version 1.0.1.012 (64-bit Edition) - 2026-10-26 13:08
- Replaced mouse-hover dictionary popup on unassociated Source words with automatic Destination-panel cursor tracking: selecting (left-clicking) an unassociated tagged Source word automatically displays its Strong's and Morphology dictionary popup at the southeast of the mouse pointer whenever the pointer is inside the Destination text panel, and hides it when the pointer leaves the Destination panel.
- Automatic next-word popup progression: right-clicking a translated word in the Destination panel associates the current Source word, auto-selects the next unassociated Source word, and immediately updates the southeast dictionary popup in place with the newly highlighted word's definitions and morphology codes.
- Fixed per-module Strong's dictionary fallback for Greek Strong's tags (G-prefix) in Old Testament books (BookID 1..39).

Version 1.0.1.008 (64-bit Edition) - 2026-10-25
  * Fixed bitmask constant in UpdateMiniBarPosition (memoversiculo.pas lines 363-364) where $FFFF was stripped by shell expansion.
  * Includes Layout 3 Shared Master Formatting Ribbon, Floating Selection Mini-Toolbar, Return (<CI>) / Double Return (<CM>) / Shift+Return (<CL>) line-wrapping and single-line flattening, PI/PF step buttons, and live theWord Context Panel preview.

Version 1.0.1.003 (64-bit Edition) - 2026-10-25
  * Fixed Windows.POINT typecast shadowing of Classes.Point(x, y) and PtInRect in memoversiculo.pas on Win64 FPC 3.2.2.
  * Includes Layout 3 Shared Master Formatting Ribbon, Floating Selection Mini-Toolbar, Return (<CI>) / Double Return (<CM>) / Shift+Return (<CL>) line-wrapping and single-line flattening, PI/PF step buttons, and live theWord Context Panel preview.

Version 1.0.1.002 (64-bit Edition) - 2026-10-25
  * Added Layout 3 Shared Master Formatting Ribbon & Floating Selection Mini-Toolbar across all 4 verse sections in Edit Mode.
  * Added custom vector icons for all theWord paragraph and inline tags (<CI>, <CM>, <CL>, <PI0..7>, <PF0..7>, <center>, <TS1..3>, <_CM>, <FI>, <FR>, <FO>, <FU>, <b>, <i>, <u>, <sup>, <sub>, <RF>, Clear Tags).
  * Added Return (<CI> tight paragraph), Double Return (<CM> spaced paragraph), and Shift+Return (<CL> line break) keyboard shortcuts with visual line-wrapping in Edit mode and automatic single-line flattening on save.
  * Upgraded right-side Context Panel (TChapterView) to render all theWord paragraph, indentation, heading, and inline formatting tags in real time.

Version 1.0.1.000 (64-bit Edition) - 2026-10-24
-------------------------------------------------------------------------------
* Version Reset to 1.0.1.000 (64-bit Edition):
  - Transitioned project versioning to the 1.0.1.xxx series starting at 64-bit Version 1.0.1.000.
  - All subsequent builds follow the increment of .001 from here (1.0.1.001, 1.0.1.002, etc.).
* Native Themed Tab Strips for Dictionary Popup (Strong's & Morphology):
  - Replaced Win32 SysTabControl32 native tabs in TFrmDictionaryPopup (formpopup.pas & formpopup.lfm) with native LCL owner-drawn tab panels (HeaderStrongs and HeaderMorfos).
  - Eliminates dependency on ComCtl32 SysTabControl32, completely resolving Windows 10/11 DWM theme-engine caching that kept tab buttons white with dark font.
  - Active tab paints with CurrentSkin.PointedBg, pure white text (CurrentSkin.PointedText), and top highlight accent (CurrentSkin.HighlightBg).
  - Inactive tabs paint with CurrentSkin.PanelBg, readable light font (CurrentSkin.PanelText), and clean borders (CurrentSkin.PanelBorder).
  - Retains 100% functional fidelity for tab selection (e.g. Strong's vs Translation Memory), mouse clicks, RTF skinning, zoom, and splitter resizing.
```
