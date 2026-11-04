# iBiblia — 64-Bit Edition (v1.1.0.001)

**iBiblia** is a 64-bit visual alignment, translation, and interlinear tagging workstation for [theWord](https://www.theword.net/) Bible modules, combining all features of **iBiblia 1.1.0** and the **v1.0.1.055 64-Bit Edition**. It includes built-in import and export support for **theWord** (`.ont`, `.ot`, `.nt`), **MySword** (`.bbl.mybible`), **non-encrypted e-Sword** (`.bblx` RTF, `.bbli` HTML, `.bbl` SQLite), **USFM 2.x/3.0** (`.usfm`, `.sfm`), and **USFX XML** (`.usfx`, `.xml` from [eBible.org](https://ebible.org/) and Simple Bible Reader).

Associate words between a tagged **Source** Bible (Greek LXX/NT or Hebrew OT) and your **Destination** translation—or start with an initially empty Destination in a new translation project and write it directly inside the application. iBiblia automatically transfers Strong's numbers and morphological parsing tags, preserves unaffected associations across text edits, compares past translation choices in **Translation Memory**, formats verse markup in a **Visual WYSIWYG / Raw ONT Editor** with rich translator notes, and exports ready-to-use Bible and concordance modules.

For foundational workflow documentation, see:
* [User Manual (English)](https://github.com/rubiot/ibiblia/wiki/User-manual-(English)) · [Installation and Updating (English)](https://www.google.com/search?q=https://github.com/rubiot/ibiblia/wiki/Installation-and-updating-%28English%29)
* [Manual de Utilização (Português)](https://github.com/rubiot/ibiblia/wiki/Manual-de-utiliza%C3%A7%C3%A3o-(Portugu%C3%AAs)) · [Instalação e Atualização (Português)](https://www.google.com/search?q=https://github.com/rubiot/ibiblia/wiki/Instala%25C3%25A7%25C3%25A3o-e-atualiza%25C3%25A7%25C3%25A3o-%28Portugu%25C3%25AAs%29)
* [What's New in 1.1.0](https://www.google.com/search?q=docs/releases/1.1.0.md) · [Portable Package Instructions](https://www.google.com/search?q=docs/README-portable.md) · [Full Changelog](CHANGELOG.md)

## Changelog (Starting from the 1.1.0.0 Merge)

### v1.1.0.001 (2026-11-04) — UI Polish, Hover Panel Strips, 3 Toolbar Icon Sizes, Tree Status Sync & Updated Guide

* **Dictionary Popup Size Persistence (`FormDictionaryPopup.pas`)**:
  * Fixed Dictionary Popup dimensions (`popup.largura` and `popup.altura` in `iBiblia.ini`) not persisting across application restarts by storing explicit `FPopupWidth` and `FPopupHeight` fields initialized in `FormCreate` and `AfterConstruction` (preventing LCL's `GetControlClassDefaultSize` and DPI auto-adjust from resetting dimensions), saving user resizes immediately in `DoOnResize`, and preserving stored dimensions on exit even if the popup was not opened during the session.
* **Restored 5-Line Module Properties Editor Height (`formprincipal.pas`)**:
  * Parented `FPropsMemos[TextType]` to `CenterPanel` as a full-panel overlay over each Bible panel (instead of inside `VerseScrollBox` where `DirectWriteView.Align := alTop` compressed it into a tiny strip at the bottom) and enforced a minimum vertical height of 5 lines above the panel strip when `Properties` is checked.
* **Hover-Only Individual Bible Bottom Toolbars Synced with Chevron Arrows (`formprincipal.pas`)**:
  * Updated `UpdateVersePanelStrips` and `LayoutVersePanelLabels` (with a 50 ms hover timer) so each Bible panel's bottom toolbar (Module Name caption, Destination status radio circles, `Edit`, and `Properties`) appears only while the mouse pointer hovers over that individual Bible panel, matching the `< >` chevron navigation arrows.
* **3 Top Bar Menu Icon Size Options (`ui_icons.pas`, `FormApplicationPreferences.pas`, `formprincipal.pas`)**:
  * Added **Small (16 px)** (default, `24x24` buttons), **Medium (22 px)** (`31x31` buttons), and **Large (28 px)** (`38x38` buttons) selectable under both **View > Toolbar icon size** and **Tools > iBiblia preferences...**, persisted in `iBiblia.ini` (`ui.toolbar.icon.size`).
* **Skin-Themed Split-Button Dropdown Arrows on Top Bar (`unitskins.pas`)**:
  * Updated `ToolBarSubclassProc` (`WM_PAINT`) to paint the dropdown arrow halves of `tbsDropDown` split buttons (`ToolButton6` Suggest Association and `FTheWordToolButton` theWord/Timotheos Sync) using `CurrentSkin.PanelBg` (and `CurrentSkin.VerseHighlightBg` with a `CurrentSkin.PanelBorder` divider on hover) and a themed chevron arrow.
* **Main Bible Tree Status Synchronization with Destination 4 Radio Buttons (`Project.pas`, `formprincipal.pas`)**:
  * Fixed the case-insensitive identifier shadowing bug (`if id = ID`) in `TProject.UpdateTree(const AReferenceID: string)`, fired `FRadioGroupSituacao.OnChange` whenever `SintagmaOnClick` or statistical/AI suggestions advance verse status, and kept `FRadioGroupSituacao` and `TreeView1` synchronized in `SetStatus` and `TrySetVerseText`.
* **Realtime Save / Unsaved Status in Bottom-Left Status Bar (`formprincipal.pas`, `unitskins.pas`)**:
  * Moved `FHeaderModifiedLabel` (`Saved` / `Unsaved changes`) out of the top header bar and into `FSaveStatusPanel` at the bottom-left corner of `StatusBar1`, directly below the Status Summary box.
* **Expanded New Features User Guide (`Shift+F1`) & `+0.0.0.001` Versioning (`unitabout.pas`, `FormNewFeaturesGuide.pas`, `CHANGELOG.md`, `Histórico.txt`, `historico.txt`, `iBiblia.lpi`)**:
  * Expanded the interactive New Features User Guide (`Shift+F1`) from 9 to 12 illustrated topics covering all `1.1.0.0` features, and adopted `+0.0.0.001` version increments moving forward.

### v1.1.0.0 (2026-11-03) — Complete Merge of iBiblia 1.1.0 & v1.0.1.055 (64-Bit Edition)

* **Visual WYSIWYG & Raw ONT Verse Editor (`VerseMemo.pas`, `theword_wysiwyg_editor_v2.pas`, `theword_ont_codec_v2.pas`)**:
  * Write and format verses visually with section titles (`<TS1>`..`<TS3>`), added words (`<FI>...<Fi>`), Words of Jesus (`<FR>...<Fr>`), Old Testament quotations (`<FO>...<Fo>`), underline, strikethrough, superscript, subscript, custom font colors, and relative font sizes, or switch to **Raw mode** for direct theWord markup editing with protected-region safeguards.
* **Rich Translator Notes with Hover Previews (`theword_translator_note_body_v2.pas`, `translator_note_hover_v2.pas`, `rf_note_scanner.pas`)**:
  * Add and edit rich translator notes (`<RF>...<Rf>`) with automatic sequential numbering (`q=1, 2, 3...`) or custom markers, and preview formatted footnotes on hover without leaving the current verse.
* **Bidirectional DirectWrite Rendering & Unicode Selection (`directwrite_syntagm_view.pas`, `text_direction.pas`, `Verse.pas`)**:
  * Hardware-accelerated DirectWrite rendering and accurate Unicode selection for mixed Hebrew (`RTL`), Greek (`LTR`), and translated text, with per-panel right-to-left toggles and automatic direction detection.
* **Association-Preserving Text Edits & Translation Memory (`current_verse_workspace.pas`, `association_memory_service.pas`, `ChapterView.pas`)**:
  * Preserves unaffected word associations through verse text edits, prompting for review before any associated members are removed.
  * Compares past translation choices in the Context Panel's **Memory** tab (sortable by *Verse order* or *Rarest first*), highlights exact occurrences, and previews verses inline on hover without leaving the current verse.
* **Guided Project Creation Wizard, Customizable Status Catalog & Save As (`FormNewProject.pas`, `status_catalog.pas`, `status_visuals.pas`, `project_creation_staging.pas`)**:
  * Step-by-step wizard for creating **Association projects** (existing Source + Destination) or **Translation projects** (Source + initially empty Destination).
  * Customizable verse statuses (up to 12 statuses with custom names, colors, and display order), multi-select status filtering in the Status Summary box, contextual workflow guide banner, and transactional **Save As** (`Ctrl+Shift+S`).
* **Experimental AI Translation & Association Workflows (`ai_processing_manager.pas`, `FormAIProcessing.pas`, `FormLLMOptions.pas`, `ai_credentials.pas`)**:
  * Optional single-verse and batch AI translation and association (including combined translation-plus-association and automatic added-word marking) with side-by-side batch review before applying accepted results.
  * Machine-default and project-override AI settings, flow-specific prompts, reusable verse examples, and Windows DPAPI local secure storage for API keys.
* **Merged 1.0.1.055 64-Bit Edition Features (`unitskins.pas`, `myswordmodule.pas`, `tanglaw_sync.pas`, `twAutomate.pas`, `unittexttospeech.pas`, `FormDictionaryPopup.pas`)**:
  * Retains all 7 built-in theWord color skins (`Classic`, `theWord Dark`, `Night`, `Sepia`, `Slate`, `Solarized Dark`, `High Contrast Dark`, `Ctrl+Shift+D`), Mouse Wheel word selection & `Ctrl+Shift` multi-word association with auto-advance, 6-direction pointer-aware Dictionary Popup with instant hide & `Ctrl`-freeze, 4-slot global Default Dictionaries (including compressed `.dct.twm` and English Word stem lookup), multi-format Bible import/export (`.ont`/`.ot`/`.nt`, MySword `.bbl.mybible`, non-encrypted e-Sword `.bblx`/`.bbli`/`.bbl`, USFM `.usfm`/`.sfm`, USFX `.usfx`/`.xml`) with Apocrypha books (`67..87`), real-time Hover Text-to-Speech (`Ctrl+T`), Modeless Find in Project (`Ctrl+F`), and concurrent bidirectional verse synchronization with **theWord 7** and **Timotheos (Tanglaw)** (both Tauri 2 v0.6.5+ and Electron v0.5.5+ builds).

## Key Features Overview

### 1. Visual WYSIWYG / Raw ONT Verse Editor & Rich Translator Notes (`1.1.0.0`)

* **In-Place Visual & Raw Editing**: Click **Edit** on any Bible panel's bottom hover bar (or double-click the verse text) to open the visual editor. Format headings, added words (`I`), Words of Jesus (`J`), OT quotes (`Q`), colors, and relative font sizes, or toggle **Raw** mode to edit raw theWord ONT tags directly.
* **Paragraph Keys**: Press `Enter` for a tight paragraph break (`<CI>`), `Double Enter` for a spaced paragraph break (`<CM>`), or `Shift+Enter` for a line break within the same paragraph (`<CL>`).
* **Translator Notes (`<RF>...<Rf>`)**: Insert and edit footnotes with automatic or custom markers and hover over any footnote marker in the Bible panels to preview the formatted note in a themed popup card.
* **Association-Preserving Edits**: Editing a verse's text automatically keeps unaffected word associations intact and asks for confirmation before removing links from modified words.

### 2. Bidirectional DirectWrite Rendering & Translation Memory (`1.1.0.0`)

* **BiDi DirectWrite Text Engine**: Renders mixed Right-to-Left Hebrew, Left-to-Right Greek, and vernacular text with crisp DirectWrite typography, Strong's occurrence badges, and accurate Unicode word selection.
* **Translation Memory (`Context Panel > Memory`)**: Inspect how every source word or Strong's number in the current verse has been translated across your project, sort by *Verse order* or *Rarest first*, preview highlighted verse occurrences inline, and jump to any occurrence with `Alt+Left` / `Alt+Right` navigation history.

### 3. Guided Project Wizard, Customizable Statuses & Experimental AI Workflows (`1.1.0.0` – `1.1.0.001`)

* **Guided New Project Wizard (`File > New Project`)**: Choose between an **Association project** or a **Translation project** (starting with an empty Destination module), configure scope (`OT`, `NT`, `ONT`), and customize up to 12 verse statuses.
* **Experimental AI Translation & Association (`Project > AI Options`)**: Configure OpenAI, DeepSeek, or Google Gemini models (`iBiblia.models.json`), flow-specific system prompts, and few-shot verse examples from your project. Run single-verse AI suggestions or right-click a book/chapter in the Bible TreeView to launch batch AI processing with interactive review before applying changes.
* **Interface Language, Icon Families & 3 Toolbar Icon Sizes**: Choose **English** or **Português**, 4 icon families (**Connected texts**, **Clareza**, **Mosaico**, **Fólio**), and 3 top-bar icon sizes (**Small 16 px**, **Medium 22 px**, **Large 28 px**) in **View > Toolbar icon size** or **Tools > iBiblia preferences...**.

### 4. High-Speed Word Selection, Hover Strip & Multi-Direction Dictionary Popup (`1.0.1.055` + `1.1.0.001`)

* **Mouse Wheel Source Word Selection**: Roll the mouse wheel inside either the **Source** or **Destination** panel (`Scroll Up` = word left, `Scroll Down` = word right) to step through Source words hands-free.
* **`Ctrl+Shift` Multi-Word Phrase Association & Auto-Advance**: Hold `Ctrl+Shift` and click multiple words to associate a phrase; releasing `Ctrl+Shift` automatically selects the next unassociated Source word.
* **Hover-Synced Bottom Toolbar & Chevron Arrows**: Moving the mouse pointer into any Bible panel simultaneously reveals its `< >` verse navigation chevrons after the verse text and its bottom control strip (Translation Name badge, Destination status radio circles, `Edit`, and `Properties`).
* **Smart 6-Direction Dictionary Popup & 4 Default Dictionaries**: Opens adjacent to the mouse pointer without covering the hovered word, hides immediately when moving off a word, stays open if `Ctrl` is held while moving the pointer into the popup to scroll/zoom, remembers its resized dimensions across restarts, and supports 4 global fallback dictionaries (Greek Strong's, Hebrew Strong's, Morphology, and English Word dictionary with stem matching).

### 5. Multi-Format Import/Export, Apocrypha Books & External Bible Sync (`1.0.1.055` + `1.1.0.0`)

* **Multi-Format Bible Import & Export**: Import and export **theWord** (`.ont`, `.ot`, `.nt`), **MySword** (`.bbl.mybible`), **non-encrypted e-Sword** (`.bblx`, `.bbli`, `.bbl`), **USFM 2.x/3.0** (`.usfm`, `.sfm`), and **USFX XML** (`.usfx`, `.xml`), including all 21 standalone Apocrypha / Deuterocanonical OT books (`Book >= 67`).
* **Bidirectional theWord 7 & Timotheos (Tanglaw) Synchronization**: Synchronize verse navigation in both directions simultaneously with **64-bit theWord 7** (`WM_COPYDATA`) and **Timotheos / Tanglaw** (supporting both Tauri 2 `v0.6.5+` and Electron `v0.5.5+` installations via dual-bridge sync).

## Keyboard & Mouse Shortcuts Summary

| Shortcut / Action | Function |
| ----- | ----- |
| **`Mouse Wheel Up / Down`** *(in Source or Destination)* | Select previous (left) or next (right) word in the Source text one by one |
| **`Ctrl + Shift + Left-Click`** | Select and associate multiple words simultaneously across Source and Destination |
| **`Release Ctrl + Shift`** *(after associating)* | Automatically select and highlight the next unassociated word in the Source text |
| **`Right-Click`** *(on Destination word)* | Associate single word and immediately advance to the next unassociated Source word |
| **`Ctrl + Left-Click`** *(on associated word)* | Dissociate (unlink) an existing word association |
| **`Alt + Left` / `Alt + Right`** | Navigate back / forward through visited verse history |
| **`Ctrl + S` / `Ctrl + Shift + S`** | **Save Project** / **Save Project As...** (creates an independent `.bib` copy) |
| **`Ctrl + F`** | Open modeless **Find in Project** dialog (search Source, Destination, Refs, Strong's, or Notes) |
| **`F8` / `Shift + F8`** | Jump to **Next / Previous verse** matching the ticked statuses in the **Status Summary** filter |
| **`Page Down` / `Page Up`** | Navigate to the next / previous verse in canonical order |
| **`Keys 1..9`** | Set current verse status directly (synced between the Destination status circles and Bible Tree) |
| **`Ctrl + Mouse Wheel`** | Zoom Bible panel, Context Panel, or Dictionary Popup font size in / out |
| **`Hold Ctrl`** *(while Dictionary Popup is open)* | Keep Dictionary Popup open while moving the mouse pointer inside it to scroll or zoom |
| **`Enter` / `Double Enter` / `Shift+Enter`** *(in Editor)* | Insert `<CI>` tight paragraph / `<CM>` spaced paragraph / `<CL>` line break |
| **`Ctrl + T`** | Toggle real-time Text-to-Speech (TTS) word reading on hover |
| **`Ctrl + Shift + D`** | Toggle between Dark Mode (*theWord Dark*) and *Classic (Light)* |
| **`F1` / `Shift + F1`** | Show **Keyboard Shortcuts** reference / Open the interactive **New Features User Guide** |

## Built-In Color Themes (Skins)

| Theme Name | Background | Primary Ink | Best For |
| ----- | ----- | ----- | ----- |
| **Classic (Light)** | `#FFFFFF` | `#000000` | Daylight reading and traditional Windows appearance |
| **theWord Dark (Charcoal)** | `#1E1E1E` | `#D8D8D8` | Matching theWord 7 dark workspace for low-glare sessions |
| **Night (Pitch Black)** | `#121212` | `#E8E6E3` | OLED displays and late-night translation work |
| **Sepia (Parchment)** | `#F4EEDB` | `#2D241E` | Warm paper-like contrast for extended reading comfort |
| **Slate (Steel Blue)** | `#1C232B` | `#DCE4EC` | Cool navy-slate contrast with crisp cyan highlights |
| **Solarized Dark** | `#002B36` | `#93A1A1` | Precision low-contrast cyan/teal environment |
| **High Contrast Dark** | `#000000` | `#FFFFFF` | Maximum legibility with vivid yellow and cyan accents |

## AI Model Catalog (`iBiblia.models.json`)

`iBiblia.models.json`, placed beside `iBiblia.exe`, is the editable model catalog for the supported OpenAI, DeepSeek, and Google providers:

```json
{
  "name": "gpt-4.1-mini",
  "provider": "OpenAI",
  "supports_struct_output": true
}
```

`supports_struct_output` controls requests for structured JSON responses when supported. A missing or invalid catalog falls back to built-in defaults. API keys are configured inside the application (`Project > AI Options`) and stored in Windows local secure storage for the current user; never place API keys inside `iBiblia.models.json`.

## Credits & Contributors

* **Original Author & 1.1.0 Architecture**: [Rúbio R. C. Terra](https://github.com/rubiot/ibiblia) (`rubio.terra@gmail.com`)
* **64-Bit Edition & Feature Enhancements**: [Edmund P. Valdez](https://github.com/SalitaNgDiyos) (`malayangbiblia@gmail.com`)
