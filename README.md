# iBiblia (SalitaNgDiyos Fork — v1.1.0.086)

**iBiblia** (SalitaNgDiyos Fork) is a native **64-bit Windows (`x64`) and Linux (`x86_64`)** multi-format Bible alignment, translation, and interlinear tagging workstation built on [iBiblia 1.1.0](https://github.com/rubiot/ibiblia). It includes built-in import and export support for **theWord** (`.ont`, `.ot`, `.nt`, and `.dct.twm` concordances), **Timotheos** (compatible module/concordance exports with `.order` tree companions and USFM 3.0 exact Strong's metadata), **MySword** (`.bbl.mybible`), **non-encrypted e-Sword** (`.bblx` RTF, `.bbli` HTML, `.bbl` SQLite), **USFM 2.x/3.0** (`.usfm`, `.sfm`), and **USFX XML** (`.usfx`, `.xml` from [eBible.org](https://ebible.org/) and Simple Bible Reader), along with direct import from **SWORD / CrossWire** (`.conf` / `.zip` Bibles and dictionaries), compressed archives (`.zip`, `.tar*`, `.gz`, `.7z`, `.rar`), and text documents (`.txt`, `.rtf`, `.odt`, `.docx`) for adding or replacing Bible books.

Associate words between a tagged **Source** Bible (Greek LXX/NT or Hebrew OT) and your **Destination** translation—or start with an initially empty Destination in a new translation project and write it directly inside the application. iBiblia automatically transfers Strong's numbers and morphological parsing tags, preserves unaffected associations across text edits, compares past translation choices in **Translation Memory**, formats verse markup in a **Visual WYSIWYG / Raw ONT Editor** with paragraph tags and rich translator notes, provides interactive Bible tree reordering and selective diff-highlighted book replacement, maintains an embedded per-project **Change Log** with **Backup & Restore** plus automated **Crash & Error Logging**, synchronizes verses with **theWord** and **Timotheos**, delivers automatic SHA-256 verified updates, and exports ready-to-use Bible and concordance modules.

For downloads, manuals, and detailed history, see:

* [Download Latest Release (Windows x64 & Linux x86_64)](https://github.com/SalitaNgDiyos/ibiblia/releases)

* [User Manual (English)](https://github.com/rubiot/ibiblia/wiki/User-manual-%28English%29) · [Installation and Updating (English)](https://github.com/rubiot/ibiblia/wiki/Installation-and-updating-%28English%29)

* [Manual de Utilização (Português)](https://github.com/rubiot/ibiblia/wiki/Manual-de-utiliza%C3%A7%C3%A3o-%28Portugu%C3%AAs%29) · [Instalação e Atualização (Português)](https://github.com/rubiot/ibiblia/wiki/Instala%C3%A7%C3%A3o-e-atualiza%C3%A7%C3%A3o-%28Portugu%C3%AAs%29)

* [What's New in 1.1.0](docs/releases/1.1.0.md) · [Portable Package Instructions](docs/README-portable.md) · [Full Changelog (Including Bug Fixes)](CHANGELOG.md)

## Key Features Overview

### 1. Visual WYSIWYG / Raw ONT Verse Editor, Paragraph Formatting & Rich Translator Notes

* **In-Place Visual & Raw Editing**: Click **Edit** on any Bible panel's hover bar (or double-click the verse text) to open the visual editor. Format section titles (`<TS1>`..`<TS3>`), added words (`<FI>...<Fi>`), Words of Jesus (`<FR>...<Fr>`), Old Testament quotations (`<FO>...<Fo>`), underline, strikethrough, superscript, subscript, custom font colors, and relative font sizes, or toggle **Raw** mode to edit raw theWord ONT tags directly with protected-region safeguards.

* **Paragraph Shortcut Keys**: Press `Enter` for a tight paragraph break (`<CI>`), `Double Enter` for a spaced paragraph break (`<CM>`), or `Shift+Enter` for a line break within the same paragraph (`<CL>`).

* **theWord Paragraph Tag Rendering**: Full support for theWord paragraph tags (`<CM>`, `<CI>`, `<CL>`, `<PI>`/`<PI1>`–`<PI7>` block indents, `<PF>`/`<PF1>`–`<PF7>` first-line indents, and `<PI0>`/`<Pi>`/`<PF0>` resets) in both the **Chapter Panel** (Paragraph mode) and the **Source**, **Destination**, and **Reference** verse panels on Windows (per-paragraph DirectWrite layout) and Linux.

* **Rich Translator Notes (`<RF>...<Rf>`)**: Insert and edit footnotes with automatic sequential numbering (`q=1, 2, 3...`) or custom markers, and hover over any footnote marker in the Bible panels to preview the formatted note in a themed popup card without leaving the current verse.

* **Association-Preserving Edits**: Editing a verse's text automatically keeps unaffected word associations intact and prompts for impact review before removing links from modified words.

### 2. High-Speed Word Association, Navigation & Smart Dictionary Popup

* **Mouse Wheel Selection, `Ctrl+Shift` Phrase Linking & Hover TTS**: Roll the mouse wheel inside either the **Source** or **Destination** panel to step through tagged Source words hands-free (`Scroll Up` = word left, `Scroll Down` = word right). Hold `Ctrl+Shift` and click multiple words to associate a phrase (releasing `Ctrl+Shift` automatically selects the next unassociated Source word), **Right-Click** on a Destination word for single-click association and auto-advance, and toggle real-time Hover Text-to-Speech with `Ctrl+T` (system voices on Windows or `espeak-ng` on Linux via **Tools → Voice settings...**).

* **Go to Verse Lookup Box (`Tools → Go to verse...`, `Ctrl+G`)**: Fast theWord-style navigation supporting space, period, colon, or comma separators (`jn 3 16`, `jn 3.16`, `1co 13:4`, `ps 23`), standard abbreviations, English/Portuguese/Tagalog/custom Apocrypha book names, Roman numerals (`ii tim 3 16`), current-book or current-chapter shorthand (`3 16`, `:16`, `jude 5`), live reference preview as you type, and instant activation when typing a letter while the Bible tree has focus.

* **Transparent Hover-Only Verse Panel Strip, Status Roll-Up & Save Indicator**: Moving the mouse pointer into any Bible panel reveals its see-through hover bottom bar synced with `< >` verse navigation chevrons—showing the translation name, Destination status radio circles (with automatic status progression and real-time Chapter and Book roll-up in the Bible tree), **Edit**, and **Properties**. A streamlined state header bar pairs with the live `Saved` / `Unsaved changes` indicator in the bottom-left corner of the status bar.

* **Smart Multi-Direction & Edge-Docked Dictionary Popup + 4 Default Dictionaries**: Floating popup placement uses 6 directions with a 1-inch pointer clearance so the hovered word stays visible; mutually exclusive **View → Dock Word Definition Popup to Right Edge** and **Dock Word Definition Popup to Left Edge** modes (also in **Preferences**) follow the vertical height of your pointer while clamping to the main window; moving directly from word to word or stepping by mouse wheel refreshes entries immediately; holding `Ctrl` keeps the popup open while allowing direct mouse-wheel scrolling and `Ctrl`+wheel zooming inside Strong's and Morphology panes (persisting custom size and dock state across restarts); and **Tools → Default dictionaries...** configures 4 global fallback dictionaries (Greek Strong's, Hebrew Strong's, Morphology, and English Word with stem matching), supporting compressed `.dct.twm`, compressed archives, and SWORD dictionaries.

### 3. Bidirectional Rendering & Translation Memory

* **Bidirectional Text Engine**: DirectWrite rendering on Windows and native Unicode layout on Linux handle mixed Right-to-Left Hebrew, Left-to-Right Greek, and vernacular text with Strong's occurrence badges, per-panel right-to-left toggles, automatic direction detection, and accurate Unicode word selection.

* **Translation Memory (`Context Panel → Memory`)**: Inspect how every source word or Strong's number in the current verse has been translated across your project, sort by *Verse order* or *Rarest first*, preview highlighted verse occurrences inline on hover without leaving your verse, and move through visited verse history with `Alt+Left` / `Alt+Right`.

### 4. Interactive Bible Tree: Rearrange, Delete, Add & Replace Books

* **Drag-and-Drop Reordering & `.order` Companions**: Every book, chapter, and verse row in the Bible tree carries a right-edge grip handle for dragging rows into a custom arrangement (including moving verses or chapters across boundaries while preserving canonical chapter and verse numbers), saved inside the `.bib` project. Toggle grips anytime with **Tools → Rearrange books, chapters and verses**, restore standard order via right-click → **Reset the order of books, chapters and verses**, and automatically emit a companion `.order` file alongside exported Bible modules for Timotheos.

* **Delete Books, Chapters, or Verses**: Right-click any book, chapter, or verse in the Bible tree and choose **Delete this book / chapter / verse...** to remove its entries after confirmation, automatically saving a `_before-delete_<date>_<time>` safety copy beside the project.

* **Add Bible Books (`Project → Settings → Texts` & Blank-Panel `Add Bible` Link)**: Add missing canonical or Apocrypha books to Source, Destination, Reference 1, or Reference 2 from one or multiple selected files (for example, per-book USFM files), Bible modules (including SWORD), compressed archives, or text documents (`.txt`, `.rtf`, `.odt`, `.docx` with automatic detection of book titles, chapter/Greek-numeral headings, sub-verses, and double numbering). Blank or missing-verse panels also display an **Add Bible** link above the hover strip, and adding books from the other Testament prompts to expand project scope to whole Bible (`ONT`).

* **Selective Replace with Character-Level Diff Preview**: Choose to replace an entire text or selectively replace specific books, chapters, or verses (with *Try to keep the current associations*). Checking or selecting tree nodes updates a bold `Preview: <Book Chapter:Verse>` header with difference count; side-by-side **In the project** and **In the file** panes highlight exact differing characters, tags, and words (red on existing project text, green on incoming file text) with synchronized vertical scrolling and `Ctrl`+mouse wheel zooming.

* **Apocrypha / Deuterocanonical Placement (`67–87`)**: Deuterocanonical books (Tobit, Judith, Sirach, Maccabees, etc.) appear between Malachi and Matthew in imported source order across the Bible tree, verse navigation, Find in Project (`Ctrl+F`), `F8` status stepping, **Create patch**, Strong's concordance export, and Timotheos synchronization.

### 5. Multi-Format Import, Export, Module Properties Preview & Concordances

* **Broad Format, Archive & SWORD Support**: Import and export **theWord** (`.ont`, `.ot`, `.nt`), **MySword** (`.bbl.mybible`), **non-encrypted e-Sword** (`.bblx`, `.bbli`, `.bbl`), **USFM 2.x/3.0** (`.usfm`, `.sfm`), and **USFX XML** (`.usfx`, `.xml`); import **SWORD (CrossWire)** Bibles and dictionaries directly from `.conf` or `.zip` (preserving Strong's, morphology, footnotes, Words of Jesus, and Apocrypha, and converting SWORD dictionaries once into `.dct.twm`); and open compressed archives (`.zip`, `.tar`, `.tar.gz`/`.tgz`, `.tar.bz2`, `.tar.xz`, `.gz`, `.7z`, `.rar`) in Add/Replace Bible Books and dictionary pickers with combined multi-file trees and per-book conflict selection.

* **Full USFM 3.0 Import & Per-Book Export**: Imports headings, paragraphs, poetry (`\q1`–`\q4`), list/table markers, nested `+` character styles, `\w` word attributes (`lemma`, `strong`, `x-morph`), `\f`/`\fe` footnotes, `\x` cross-references, unfoldingWord `\zaln` alignments, and speaker red letters (`\qt-s who="Jesus"`). Exports combined USFM 3.0 with a per-book `<file>_usfm` folder or a dedicated **USFM 3.0 Bible, per-book files (`*.zip`)** option, writing `\rem` metadata marking exact Strong's numbers for Timotheos.

* **Selective Partial Export (`Choose books...`) & Panel Export Shortcut**: Use **Choose books...** on **Tagged destination text**, **Interlinear source text**, and **Strong's concordance** tabs to export only ticked books, chapters, or verses (preserving empty verse slots in theWord modules and omitting unticked verses in other formats). Right-click the Source or Destination panel → **Export as Bible module...** to open **Export** directly on the matching tab.

* **Module Properties Preview & Raw (Edit) Tabs (`Project → Export`)**: Both **Tagged destination text** and **Interlinear source text** tabs provide a formatted **Preview** tab (open by default) alongside a **Raw (Edit)** tab—rendering metadata keys, clickable URLs and emails, HTML/RTF `about=` formatting, and monospace `verse.rule=` regexes, with smooth mouse-wheel scrolling, `Ctrl`+mouse wheel zooming (7 pt–24 pt) in both tabs, double-click to edit, and full skin adaptation.

* **Tree-Aligned Strong's Concordance & Patches**: Strong's concordance export adheres to the exact books, custom order, and **Choose books...** selection of the main Bible tree—supporting Greek OT (LXX) Strong's, extended Strong's numbers beyond `G5624`, Apocrypha books (`67+`), and readable verse reference links (`Genesis 1:1`, `Tobit 1:1`) for both theWord and Timotheos. **Create patch** similarly mirrors the main Bible tree's exact books, Apocrypha placement, deletions, and custom arrangement.

### 6. Per-Project Change Log, Backup & Restore, and Crash / Error Tracking

* **Per-Project Change Log Tracker (`Project → Project change log...`)**: Stores an embedded `change_log` inside every `.bib` project file (so history travels automatically with backups and **Save As** copies), recording timestamps, categories, summaries, and details for project creation, verse/association/status/comment saves, dictionary and settings updates (with full **Before** and **After** paths and values), whole-text imports, Add/Replace Bible books, patches, AI batch applications, deletions, backups/restores, tree reordering, and manual notes. Includes text search, category filtering, manual notes, Copy, `.txt`/`.csv` export, and a resizable **Details** viewer with `Ctrl`+wheel zooming plus **inline red/green diff highlighting on text edits** (highlighting replaced text in red under `Before` and newly applied text in green under `After` while keeping non-text entries clean).

* **Project Backup & Restore (`.bib.zip`)**: Manage SQLite-verified `.bib.zip` snapshots with optional notes and a configurable backup folder in **Project → Settings → Project**. Automatic pre-operation backup prompts run before Replace, Add Books, Clear Text, Apply Patch, or Apply AI results (keeping the 2 newest automatic backups while never removing manual backups); **Restore project...** backs up the current project first so restores are undoable; and **File → Open** opens `.bib.zip` archives directly.

* **Automated Crash & Error Tracker (`Help → Crash & error logs...`)**: Automatically records unhandled exceptions (with stack trace, active window/control, open project, current verse, and recent activity breadcrumbs) into timestamped `.log` files under `<ConfigDir>/crash_logs/`, paired with a session watchdog that logs unexpected hard crashes on next launch. Displays a themed **iBiblia Error Report** popup (**Copy report**, **Save log as...**, **Open logs folder**, **Continue**, **Close iBiblia**) and offers **Help → Crash & error logs...** to review saved logs and create manual diagnostic reports.

### 7. Guided Project Wizard, Customizable Statuses, Themes & Experimental AI Workflows

* **Guided New Project Wizard, Status Catalog & Save As (`Ctrl+Shift+S`)**: Choose between an **Association project** or a **Translation project** (starting with an empty Destination module), configure `OT`/`NT`/`ONT` scope, customize up to 12 verse statuses (names, colors, and display order), filter navigation through multi-select checkboxes in **Status Summary** (`F8` / `Shift+F8`), search all texts including Apocrypha with modeless **Find in Project** (`Ctrl+F`), and save independent `.bib` copies.

* **Experimental AI Translation & Association (`Project → AI Options`)**: Configure OpenAI, DeepSeek, or Google Gemini models (`iBiblia.models.json`), machine-default or per-project settings, flow-specific system prompts, and reusable verse examples. Run single-verse AI suggestions or right-click a book/chapter in the Bible tree for batch processing with side-by-side review before applying, backed by Windows DPAPI local secure storage for API keys.

* **Languages, Skins, Icons & 14-Topic Interactive Guide (`Shift+F1`)**: Choose **English** or **Português** interface language, 7 built-in color skins (`Ctrl+Shift+D`), 4 icon families (**Connected texts**, **Clareza**, **Mosaico**, **Fólio**), 3 toolbar icon sizes (**Small 16 px**, **Medium 22 px**, **Large 28 px** in **View → Toolbar icon size** or **Preferences**), and explore the 14-topic illustrated **New Features User Guide** (`Shift+F1`).

### 8. Cross-Platform (Windows x64 & Linux x86_64), External Bible Sync & Automatic Updates

* **Native 64-Bit Windows & Linux Editions**: Runs natively on 64-bit Windows (`x64`) and 64-bit Debian/Ubuntu Linux (`x86_64`, GTK2) using interchangeable `.bib` project files. On Linux, user settings and logs live cleanly in `~/.config/iBiblia`, desktop integration registers iBiblia automatically in the Applications Menu (`IBIBLIA_NO_MENU=1` to opt out, alongside `install-menu-entry.sh`), all dialogs and KDE title bars respect dark/light/Sepia skins, and **About** displays clickable Program Folder and Working Folder paths.

* **Bidirectional theWord 7 & Timotheos (Tanglaw) Synchronization**: Synchronize verse navigation in both directions simultaneously with **64-bit theWord 7** (Windows `WM_COPYDATA`) and **Timotheos (Tanglaw)** (Windows and Linux, supporting both Tauri 2 `v0.6.5+` and Electron `v0.5.5+`, with Apocrypha books matched by name).

* **Automatic SHA-256 Verified Updates & Update Branch Selector**: Checks GitHub Releases automatically at startup or on demand via **Help → Check for updates...**, verifies archives against `SHA256SUMS.txt`, supports seamless background downloads (install on restart or on close) with rollback protection while leaving `.bib` projects and `iBiblia.ini` untouched (`IBIBLIA_NO_UPDATE=1` to opt out), and adds **Help → Update branch** to switch between **Stable (`linux-port`)** and **Neon (`Neon` branch)**.

## Keyboard & Mouse Shortcuts Summary

| Shortcut / Action | Function |
| ----- | ----- |
| **`Mouse Wheel Up / Down`** *(in Source or Destination)* | Select previous (left) or next (right) word in the Source text one by one |
| **`Ctrl + Shift + Left-Click`** | Select and associate multiple words simultaneously across Source and Destination |
| **`Release Ctrl + Shift`** *(after associating)* | Automatically select and highlight the next unassociated word in the Source text |
| **`Right-Click`** *(on Destination word)* | Associate single word and immediately advance to the next unassociated Source word |
| **`Ctrl + Left-Click`** *(on associated word)* | Dissociate (unlink) an existing word association |
| **`Ctrl + G`** *(or type a letter in the Bible tree)* | Open **Go to verse...** lookup box to jump directly to any book, chapter, or verse |
| **`Alt + Left` / `Alt + Right`** | Navigate back / forward through visited verse history |
| **`Ctrl + S` / `Ctrl + Shift + S`** | **Save Project** / **Save Project As...** (creates an independent `.bib` copy) |
| **`Ctrl + F`** | Open modeless **Find in Project** dialog (search Source, Destination, Refs, Strong's, or Notes) |
| **`F8` / `Shift + F8`** | Jump to **Next / Previous verse** matching the ticked statuses in the **Status Summary** filter |
| **`Page Down` / `Page Up`** | Navigate to the next / previous verse in canonical order |
| **`Keys 1..9`** | Set current verse status directly (synced between the Destination status circles and Bible Tree) |
| **`Ctrl + Mouse Wheel`** | Zoom Bible panels, Context Panel, Dictionary Popup, Export Properties Preview/Raw, Replace Books diff previews, or Project Change Log Details |
| **`Hold Ctrl`** *(while Dictionary Popup is open)* | Keep Dictionary Popup open while moving the mouse pointer inside it to scroll or zoom |
| **`Enter` / `Double Enter` / `Shift+Enter`** *(in Editor)* | Insert `<CI>` tight paragraph / `<CM>` spaced paragraph / `<CL>` line break |
| **`Ctrl + T`** | Toggle real-time Text-to-Speech (TTS) word reading on hover |
| **`Ctrl + Shift + D`** | Toggle between Dark Mode (*theWord Dark*) and *Classic (Light)* |
| **`F1` / `Shift + F1`** | Show **Keyboard Shortcuts** reference / Open the interactive **New Features User Guide** |

## Built-In Color Themes (Skins)

| Theme Name | Background | Primary Ink | Best For |
| ----- | ----- | ----- | ----- |
| **Classic (Light)** | `#FFFFFF` | `#000000` | Daylight reading and traditional desktop appearance |
| **theWord Dark (Charcoal)** | `#1E1E1E` | `#D8D8D8` | Matching theWord 7 dark workspace for low-glare sessions |
| **Night (Pitch Black)** | `#121212` | `#E8E6E3` | OLED displays and late-night translation work |
| **Sepia (Parchment)** | `#F4EEDB` | `#2D241E` | Warm paper-like contrast for extended reading comfort |
| **Slate (Steel Blue)** | `#1C232B` | `#DCE4EC` | Cool navy-slate contrast with crisp cyan highlights |
| **Solarized Dark** | `#002B36` | `#93A1A1` | Precision low-contrast cyan/teal environment |
| **High Contrast Dark** | `#000000` | `#FFFFFF` | Maximum legibility with vivid yellow and cyan accents |

## AI Model Catalog (`iBiblia.models.json`)

`iBiblia.models.json`, placed beside `iBiblia.exe` (Windows) or `iBiblia` (Linux), is the editable model catalog for the supported OpenAI, DeepSeek, and Google providers:

```json
{
  "name": "gpt-4.1-mini",
  "provider": "OpenAI",
  "supports_struct_output": true
}
```

`supports_struct_output` controls requests for structured JSON responses when supported. A missing or invalid catalog falls back to built-in defaults. API keys are configured inside the application (`Project > AI Options`) and stored locally for the current user; never place API keys inside `iBiblia.models.json`.

## Credits & Contributors

* **Original Author & 1.1.0 Architecture**: [Rúbio R. C. Terra](https://github.com/rubiot/ibiblia) (`rubio.terra@gmail.com`)

* **SalitaNgDiyos Fork, Linux Port & Feature Enhancements**: [Edmund P. Valdez](https://github.com/SalitaNgDiyos) (`malayangbiblia@gmail.com`)
