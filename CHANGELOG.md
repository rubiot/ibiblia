# Changelog — iBiblia v1.1.0 (64-bit Edition) (2026-11-14)

## Overview

**iBiblia v1.1.0 (64-bit Edition)** upgrades [iBiblia](https://github.com/rubiot/ibiblia) ([SalitaNgDiyos Fork](https://github.com/SalitaNgDiyos)) to a native 64-bit Windows (`x86_64-win64`) build with full **Dark Mode / Sepia theming**, enhanced **Strong's & Morphology Dictionary Popup** responsiveness and layout persistence, synchronized **Destination Status & Bible Tree Roll-Up**, and 64-bit IPC synchronization with [theWord](https://www.thewordbooks.com/index.php/download-tools-and-utilities/) and **Timotheos (Tanglaw)**.

## What's New & Fixed in v1.1.0

### 0. Version 1.1.0.089 (2026-11-22) — Full Notepad++-Style Find & Replace in Project (Ctrl+F / Ctrl+H)

* **Notepad++-Style Find & Replace (`FormFindInProject.pas`, `formprincipal.pas`):** Upgraded `Ctrl+F` (and added `Ctrl+H` / **Tools → Replace in project...**) to a tabbed **Find & Replace in Project** dialog supporting **Normal**, **Extended** (`\n`, `\r`, `\t`, `\0`, `\xHH`, `\uHHHH`), and **PCRE Regular Expression** modes (with `. matches newline`, capture groups `$1`/`\1`/`${name}`, and `\u`/`\l`/`\U`/`\L`/`\E` case transforms), **Match whole word only**, **Match case**, **Wrap around**, **Show raw tags in preview**, **Count**, **Find All**, **Replace**, and **Replace All**.
* **Association-Preserving In-Project Bulk Edits:** Replacements in Source, Destination, Reference 1, or Reference 2 text run directly through `TProject.ApplyVerseTexts(..., KeepAssociations)` so existing word associations stay intact wherever words remain unchanged, record a summary in **Project → Project change log...**, and save automatically.
* Files: FormFindInProject.pas, formprincipal.pas, iBiblia.lpi, iBiblia-linux.lpi, unitabout.pas, CHANGELOG.md, historico.txt, Histórico.txt.

### 0a. Version 1.1.0.088 (2026-11-22) — Update Help → About Dialog Name & GitHub URL to Dark-Skinned iBiblia

* **Updated Help → About Dialog (`unitabout.pas`, `unitabout.lfm`):** Updated the **Help → About** window title (`About Dark-Skinned iBiblia`), main heading (`Dark-Skinned iBiblia`), **View Changelog** window title (`What's New in Dark-Skinned iBiblia` / `Dark-Skinned iBiblia - Changelog`), and clickable GitHub repository link (`https://github.com/SalitaNgDiyos/Dark-Skinned-iBiblia`).
* Files: unitabout.pas, unitabout.lfm, README.md.

### 0a. Version 1.1.0.087 (2026-11-22) — Night (Pitch Black) as Default Skin & Dark-Skinned-iBiblia Feed

* **Night (Pitch Black) Default Skin (`unitskins.pas`, `formprincipal.pas`, `FormNewFeaturesGuide.pas`, `unitabout.pas`):** Made **Night (Pitch Black)** (`skNight`) the default skin on fresh installs and whenever `Theme` is unset or `Default` in `iBiblia.ini` (while preserving an explicit `Classic` choice when saved in `iBiblia.ini`), and updated `Ctrl+Shift+D` to toggle between **Night** and **Classic (Light)**.
* **Updated Repository Feed (`app_updater.pas`, `README.md`, `docs/releasing.md`):** Updated the built-in updater repository constant and release links to `SalitaNgDiyos/Dark-Skinned-iBiblia`.
* Files: unitskins.pas, formprincipal.pas, app_updater.pas, FormNewFeaturesGuide.pas, unitabout.pas, README.md, docs/releasing.md.

### 0a. Version 1.1.0.086 (2026-11-22) — Fix Splitter & Footer Layout in Project Change Log and Crash Logs Dialogs

* **Correct Splitter & Footer Stacking (`project_change_log_ui.pas`, `crash_tracker_ui.pas`):** Moved the list, horizontal splitter, and Details/Preview pane inside a dedicated `alClient` body panel (`FBodyPanel`) while pinning the bottom action bar (`FFooterPanel`) cleanly at the bottom of the dialog, and set panel heights and `Top` order before `Align := alBottom` with `ResizeAnchor := akBottom`. Dragging the splitter up/down in **Project → Project change log...** and **Help → Crash & error logs...** now properly expands the **Details** window while shrinking the log list above it instead of stretching the button bar's empty background strip.
* Files: project_change_log_ui.pas, crash_tracker_ui.pas, unitabout.pas.

### 0a. Version 1.1.0.085 (2026-11-22) — Refine Project Change Log Diff View (Text Edits Only & Cleaner Red/Green Highlights)

* **Text-Edit-Only Diff Highlighting (`project_change_log_ui.pas`):** Restricted the red/green diff view in **Project → Project change log...** strictly to actual text edits (`Source text`, `Destination text`, `Reference 1/2 text`, `Comments`, and `module properties`), leaving `Status`, `Associations`, dictionary paths, and other settings as clean plain lines.
* **Cleaner Inline Red/Green Highlights (`project_change_log_ui.pas`):** Removed the full-line red/green background tint so unchanged context stays clean and **only** the replaced text in `Before` is highlighted in **red** and **only** the newly applied text in `After` is highlighted in **green**, and stopped diffing literal `(empty)` placeholders against real text.
* **Cleaner Association & Internal Key Logging (`project_persistence.pas`, `project_change_log.pas`):** Logged association changes cleanly as `Associations updated` instead of dumping raw `<par ...>` XML, and suppressed internal `association.memory.*` revision keys from the Change Log summary.
* Files: project_change_log_ui.pas, project_persistence.pas, project_change_log.pas, unitabout.pas.

### 0a. Version 1.1.0.084 (2026-11-22) — GitHub-Style Diff Highlighting in Project Change Log

* **GitHub-Style Diff View (`project_change_log_ui.pas`):** Upgraded the **Project → Project change log...** details pane to a rich `TKMemo` diff viewer that renders every `Before:` / `After:` change with GitHub-style `- ` red removed lines and `+ ` green added lines, highlighting the exact changed words/characters with deeper inline red/green markers (adapted for both Dark and Light/Sepia skins) plus `Ctrl`+mouse wheel zooming.
* Files: project_change_log_ui.pas, unitabout.pas.

### 0a. Version 1.1.0.083 (2026-11-22) — Before/After Details in Project Change Log for Dictionaries & Raw Edits

* **Detailed Settings & Dictionary Change Log Entries (`FormProjectProperties.pas`):** Changing default or per-module dictionaries now logs the exact action (`Replaced`, `Set`, or `Cleared`), dictionary filename in the summary, and full **Before** and **After** file paths in the details pane (plus Before/After values for project name, short titles, workflow settings, and fonts).
* **Before/After Raw Data Diffs (`project_persistence.pas`):** Saving edits to raw verse texts (Source, Destination, Reference 1/2), associations, comments, status, or raw module properties now records both **Before** and **After** values in the Project Change Log details pane so every raw edit is auditable at a glance.
* Files: FormProjectProperties.pas, project_persistence.pas, unitabout.pas.

### 0a. Version 1.1.0.082 (2026-11-22) — Fix SQLite `database schema has changed` Crash on Older Projects

* **Pre-Open Change Log Table Migration (`project_change_log.pas`, `Project.pas`):** Ensured the `change_log` table is created via `EnsureProjectChangeLogTable` at the start of `TProject.OpenProject` **before** `TProject` opens its long-lived SQLite connection and prepares statements, and guarded `RecordProjectChange` with `ChangeLogTableExists` so it never runs `CREATE TABLE` DDL on a second connection while the project is open. This eliminates `ESQLDatabaseError: TSQLite3Connection : database schema has changed` and follow-on `not an error` crashes when first logging changes on pre-`1.1.0.078` `.bib` projects.
* Files: project_change_log.pas, Project.pas, unitabout.pas.

### 0a. Version 1.1.0.081 (2026-11-22) — Preserve Absolute Dictionary Paths & Auto-Heal Broken `./.config/...` Paths

* **Absolute Dictionary Paths (`FormProjectProperties.pas`):** Stopped rewriting selected dictionary paths that happen to live under the current working directory into relative `./...` paths (which turned `/home/<user>/.config/iBiblia/dictionaries/...` into `./.config/iBiblia/dictionaries/...` when launched from the home folder and broke dictionary loading whenever opening a new iBiblia build from a different folder).
* **Auto-Heal Saved Relative Dictionary Paths (`dictionary_manager.pas`, `FormProjectProperties.pas`, `formprincipal.pas`):** Exported `ResolveDictionaryFilePath` and added fallback resolution against the user home directory (`GetUserDir`) and `<ConfigDir>/dictionaries/` so existing projects that already saved `./.config/iBiblia/dictionaries/...` automatically resolve to the full path on open.
* Files: FormProjectProperties.pas, dictionary_manager.pas, formprincipal.pas, unitabout.pas.

### 0a. Version 1.1.0.080 (2026-11-22) — Fix Linux TLS Hostname Verification (Code 64) After System DNS Lookup

* **TLS Certificate Hostname Verification on Linux (`tls_runtime.pas`):** Passed the original resolved domain name (instead of the numeric IPv4 address returned by `getaddrinfo`) to OpenSSL's `SSL_set1_host` as well as SNI in `TVerifiedSocketHandler.Connect`. This fixes `The HTTPS certificate or server name could not be verified (code 64)` (`X509_V_ERR_IP_ADDRESS_MISMATCH`) when checking for updates or calling AI providers on Linux.
* Files: tls_runtime.pas, unitabout.pas.

### 0a. Version 1.1.0.079 (2026-11-22) — Fix Linux DNS Resolution for Update Checks & HTTPS

* **System Resolver on Linux (`tls_runtime.pas`):** `TIBibliaHTTPSClient` now resolves hostnames through Linux's native glibc `getaddrinfo` (via `cnetdb`) before connecting while preserving the original hostname for TLS SNI (`SSL_CTRL_SET_TLSEXT_HOSTNAME`) and certificate verification. This fixes `Host name resolution for "api.github.com" failed.` errors caused by Free Pascal's built-in `netdb` parser caching `/etc/resolv.conf` once at startup and ignoring IPv6 nameservers, `nsswitch.conf`, and `systemd-resolved` — fixing both built-in update checks (`app_updater.pas`) and AI provider HTTPS requests (`llmclient.pas`) on Linux.
* Files: tls_runtime.pas, unitabout.pas.

### 0a. Version 1.1.0.078 (2026-11-22) — Per-Project Change Log Tracker & Automated Crash / Error Logger

* **Per-Project Change Tracker (`project_change_log.pas`, `project_change_log_ui.pas`, `project_persistence.pas`, `Project.pas`, `FormProjectProperties.pas`, `project_backup_ui.pas`, `formprincipal.pas`):** Every `.bib` project now keeps an automatic `change_log` inside the project file itself (so history travels with backups and Save As copies), recording the date/time, category, summary, and details of project creation, verse/association/status/comment saves, settings edits, whole-text imports, **Add Bible books** / **Replace** operations, patches, AI batch applications, deletions, backups, restores, Bible tree reordering, and manual notes. View, search, filter by category, add notes, copy, or export (`.txt` / `.csv`) anytime from **Project → Project change log...**.
* **Automated Bug & Crash Tracker (`crash_tracker.pas`, `crash_tracker_ui.pas`, `iBiblia.lpr`, `formprincipal.pas`):** Automatically captures unhandled exceptions (with stack trace, active window/control, open project, current verse, and recent activity breadcrumbs) into timestamped `.log` files under `<ConfigDir>/crash_logs/` and maintains a `.session_active` watchdog to recover and log unexpected hard crashes on next launch. Displays a themed **iBiblia Error Report** popup with **Copy report**, **Save log as...**, **Open logs folder**, **Continue**, and **Close iBiblia**, plus **Help → Crash & error logs...** to browse past logs and generate manual diagnostic reports.
* Files: project_change_log.pas, project_change_log_ui.pas, crash_tracker.pas, crash_tracker_ui.pas, project_persistence.pas, project_backup_ui.pas, Project.pas, FormProjectProperties.pas, formprincipal.pas, iBiblia.lpr, iBiblia.lpi, iBiblia-linux.lpi, unitabout.pas.

### 0a. Version 1.1.0.077 (2026-11-21) — Character Diff Highlighting & Synchronized Scrolling in Replace Books Preview

* **Character-Level Diff Highlighting (`FormBookImport.pas`):** Upgraded the **In the project** and **In the file** preview panes in **Replace books, chapters or verses** (and **Add Bible books**) to rich diff views (`TBookDiffMemo` built on `TKMemo`) using a two-level Unicode LCS diff engine that pinpoints exact differing characters/tags/words (highlighted with skin-adapted red on the existing project side and green on the incoming file side) and displays the difference count (`Preview: Genesis 1:20 - 2 differences`) in the header.
* **Synchronized Vertical Scrolling & Zoom (`FormBookImport.pas`):** Synchronized vertical scrolling between the two preview panes across scrollbar drags, mouse-wheel scrolling (including hovering without clicking first), keyboard navigation, and `Ctrl`+mouse wheel zooming (matching font size and scroll position across both panes).
* Files: FormBookImport.pas, unitabout.pas.

### 0a. Version 1.1.0.076 (2026-11-21) — Unify Apocrypha Book IDs & Fix USFM/USFX Book Naming

* **Unified Apocrypha Book Numbering (`apocrypha_books.pas`, `esword_usfm.pas`, `bible_import_source.pas`, `sword_module.pas`):** Aligned Apocrypha/Deuterocanonical Book IDs (`67` 1 Esdras .. `87` Laodiceans) consistently across `apocrypha_books`, plain document importers, USFM/USFX importers/exporters, and SWORD OSIS mappings (also adding `PSS`/`PssSol` for Psalms of Solomon), eliminating the `76` (`Wisdom` vs. `Bel and the Dragon`) collision.
* **Accurate USFM/USFX Naming & Conflict-Safe Import (`esword_usfm.pas`, `FormBookImport.pas`, `bibletreeview.pas`):** USFM/USFX imports use the canonical book name for IDs `67–87` without English text guessing, `Add Bible books` / `Replace` matches existing project Apocrypha books by name (avoiding collisions with older projects and showing the file's real book name in the tree and preview header), and USFM/USFX export resolves book codes from project book names.
* Files: apocrypha_books.pas, esword_usfm.pas, bible_import_source.pas, sword_module.pas, bibletreeview.pas, FormBookImport.pas, unitabout.pas.

### 0a. Version 1.1.0.075 (2026-11-21) — Sync Preview Verse with Checked & Selected Bible Tree Nodes

* **Bible Tree Preview Synchronization (`bibletreeview.pas`, `FormBookImport.pas`, `FormApplyPatch.pas`):** Ticking or unticking checkboxes, selecting any Book/Chapter/Verse row, or navigating the tree with keyboard arrow keys now immediately updates the side-by-side preview (`In the project` / `In the file` and Apply Patch preview) to the selected node or first checked verse under it, instead of remaining stuck on the initial Genesis verse or only firing on direct verse-label clicks.
* **Preview Header & Unchecked State (`FormBookImport.pas`):** Added a bold `Preview: <Book Chapter:Verse>` (or `(not checked)` / `(no verse checked)`) reference header above the preview panes so it is always clear which verse is displayed, automatically moving away from unchecked verses when boxes are cleared and freeing project lookup string lists on dialog close.
* Files: bibletreeview.pas, FormBookImport.pas, FormApplyPatch.pas, unitabout.pas.

### 0a. Version 1.1.0.074 (2026-11-20) — Mouse Wheel Scrolling & Ctrl+Wheel Zoom in Export Properties Preview

* **Mouse Wheel Scrolling in Preview (`module_properties_preview.pas`, `FormProjectExport.pas`):** Enabled smooth mouse wheel scrolling directly inside the Export window's **Module properties** **Preview** tab (both **Tagged destination text** and **Interlinear source text**), including when hovering over the preview pane without clicking first and after switching tabs, matching the **Raw (Edit)** tab.
* **Ctrl+Mouse Wheel Zoom (`module_properties_preview.pas`, `FormProjectExport.pas`):** Fixed `Ctrl`+mouse wheel zooming in the **Preview** tab (preserving scroll position while adjusting font size from 7pt to 24pt) and also enabled `Ctrl`+mouse wheel font zooming in the **Raw (Edit)** tab.
* Files: module_properties_preview.pas, FormProjectExport.pas, unitabout.pas.

### 0a. Version 1.1.0.073 (2026-11-20) — Module Properties Preview & Raw HTML Edit Mode in Export Window

* **Preview & Raw (Edit) Tabs (`FormProjectExport.pas`, `module_properties_preview.pas`):** The **Module properties** area on both the **Tagged destination text** and **Interlinear source text** tabs of `Project > Export` now offers a **Preview** tab (opened by default) and a **Raw (Edit)** tab—matching the Bible panels' visual preview vs. raw edit workflow.
* **Actual Formatted Module Preview (`module_properties_preview.pas`):** Renders metadata keys, clickable `http://`/`https://`/`mailto:` links, HTML `about=` formatting (`h1`–`h6`, `center`, `p` with `text-indent`, `b`/`strong`, `i`/`em`, `u`, `s`, `sup`/`sub`, `font`, `span` CSS colors/styles, `ol`/`ul`/`li`, `br`, `hr`, `blockquote`, entities) as well as RTF `about=` bodies, while keeping `verse.rule=` regexes and `#` comments in monospace so literal tags are never mangled. Supports `Ctrl`+wheel zoom, double-click on Preview to jump straight into Raw (Edit), and full Classic/Dark/Sepia skin adaptation.
* Files: module_properties_preview.pas, FormProjectExport.pas, unitabout.pas.

### 0a. Version 1.1.0.072 (2026-11-20) — Readable Verse Links in the Concordance Export

* **Real Verse References (`ConcordanceExport.pas`):** Every verse link in the exported Strong's concordance now shows its reference (`Genesis 1:1`, `John 3:16`, `Tobit 1:1`) instead of the placeholder text `[vref]`, which only theWord itself expands and other readers such as Timotheos showed literally. The `tw://bible.*?id=b.c.v` target of each link is unchanged, so clicking still opens the verse.
* Files: ConcordanceExport.pas, unitabout.pas.

### 0a. Version 1.1.0.071 (2026-11-20) — Restore Hover-Underline for Identical Strong's Tags

* **Source & Destination Hover-Underline Restored (`syntagm.pas`, `Verse.pas`, `Project.pas`):** Hovering a tagged word in either the Source panel or the Destination panel now underlines not only the hovered word (which keeps both underline and hover highlight), but also every other word in the verse sharing the identical Strong's tag (including associated Destination words via their paired Source tags), and clears all underlines cleanly as soon as the pointer moves away.
* **Underline-Only Styling (`verse_visual_adapter.pas`):** Restored underline-only rendering (`vtsfUnderline`) for identical-tag occurrences without adding a background fill or overriding text color, and guarded programmatic click/auto-select paths so hover underlines never get stuck.
* Files: syntagm.pas, verse_visual_adapter.pas, Verse.pas, Project.pas, unitabout.pas.

### 0a. Version 1.1.0.070 (2026-11-20) — Strong's Concordance Follows the Main Bible Tree

* **Main Bible Tree Alignment (`project_exporter.pas`, `Project.pas`):** Strong's concordance export (`Export project → Strong's concordance`) now adheres to the exact books, order, and selection in the main Bible tree instead of assuming a fixed 66-book theWord layout (`QLinhas`).
* **All Tree Strong's Included (`ConcordanceExport.pas`, `project_exporter.pas`, `theworddictionary.pas`):** Removed the `etOT` Hebrew-only filter and `lemmas.dat`-only ceiling so Greek OT (LXX) Strong's and extended Strong's numbers beyond `G5624` (sorted naturally and using `<WT l="...">` lemma or surface word fallback) are exported accurately, with `config` `strong.g` / `strong.h` flags set to match the exported entries.
* **Apocrypha Verse Links (`ConcordanceExport.pas`):** Apocrypha references (`Book >= 67`) now display readable book names (`Tobit 1:1`, `Sirach 1:1`, etc.) while keeping `tw://bible.*?id=b.c.v` links, since `[vref]` only resolves books `1..66`.
* **Choose Books on Concordance Tab (`FormProjectExport.pas`):** Enabled the `Choose books...` check tree and verse summary on the **Strong's concordance** tab alongside Tagged destination text and Interlinear source text.
* **User Guide Updated (`unitabout.pas`, `FormNewFeaturesGuide.pas`):** Updated Shift+F1 Topic 7 and version badges through `v1.1.0.070`.
* Files: ConcordanceExport.pas, theworddictionary.pas, project_exporter.pas, Project.pas, FormProjectExport.pas, unitabout.pas, FormNewFeaturesGuide.pas.

### 0a. Version 1.1.0.069 (2026-11-19) — Updated Shift+F1 User Guide (Through v1.1.0.068)

* Updated the **Help → New features guide (`Shift+F1`)** (`unitabout.pas` `TFormUserGuide` and `FormNewFeaturesGuide.pas`) to cover all features through `v1.1.0.068`:
  * **Topic 7 (Import/Export & Sync):** added SWORD (CrossWire) modules, compressed archives (`.zip`, `.tar*`, `.gz`, `.7z`, `.rar`), `Choose books...` partial export, USFM 3.0 `\rem` exact Strong's marker for Timotheos, and Apocrypha order between Malachi and Matthew.
  * **Topic 12 (Bible Tree: Arrange, Delete, Add):** updated with the dedicated `Add Bible` row above the bottom bar on blank panels and exact `Create patch` tree parity.
  * **New Topic 13 ("Updates, Branch & Backups") + custom illustration:** covers automatic SHA-256 verified updates, **Help → Update branch** (`Stable (linux-port)` vs `Neon (Neon branch)`), **Project Backup & Restore** (`.bib.zip`), and the native 64-bit Linux edition.
  * **Topic 14:** shifted Full Feature Summary from 13 to 14 and adjusted sidebar height/row spacing so all 14 topics fit without clipping.
* Files: unitabout.pas, FormNewFeaturesGuide.pas.

### 0a. Version 1.1.0.068 (2026-11-19) — Update Branch Selection (Stable / Neon)

* Added **Help → Update branch** submenu to choose between **Stable (linux-port)** (`/releases/latest`) and **Neon (Neon branch)** (`/releases/tags/neon`), persisted in `iBiblia.ini` (`update.channel`).
* Switching update branches clears any skipped version and discards staged updates from the previous branch, and the updater shows the active branch name in update prompts.
* Updated `.github/workflows/release.yml` so pushes to `Neon` build Windows and Linux binaries and publish them under the rolling `neon` pre-release tag without affecting Stable releases.
* Files: app_updater.pas, unitupdate.pas, formprincipal.pas, unitabout.pas, .github/workflows/release.yml.

### 0a. Version 1.1.0.067 (2026-11-19) — USFM Export Marks Real Strong's Numbers

* Each book file of the USFM 3.0 export now carries a \rem line saying that the Strong's numbers are written exactly as tagged. Timotheos reads a 5-digit number ending in 0 (such as G13970) as an unfoldingWord number with a 0 added and showed G1397; with this line it keeps the real number. Timotheos 0.6.73 also checks the numbers in the file itself.
* Files: unitabout.pas, esword_usfm.pas.

### 0a. Version 1.1.0.066 (2026-11-19) — Export Part of a Project

* Export project now has a Choose books... button (Tagged destination text and Interlinear source text) that opens the same check tree as Create patch: tick the books, chapters or verses to export, with Select all and Deselect all. Everything is ticked at first. The tree lists what the main Bible tree lists, apocrypha books included.
* In a theWord module the verses you did not choose are written as empty lines (the verse positions stay right); e-Sword, MySword, USFM and USFX files simply leave them out, and a USFM book with no chosen verse gets no file.
* The button shows how many verses are chosen; choosing everything is the same as not choosing.
* Shift+F1 guide updated.
* The Name box of the Save module as... and Save concordance as... dialogs no longer has the file extension filled in; the extension of the chosen file type is added when you save (and the type that matches the remembered file is pre-selected).
* Files: unitabout.pas, FormExportSelection.pas, FormProjectExport.pas, FormProjectExport.lfm, FormExportPatch.pas, project_exporter.pas, Project.pas, bibletreeview.pas, FormNewFeaturesGuide.pas.

### 0b. Version 1.1.0.065 (2026-11-19) — Patch Dialog Follows the Bible Tree

* The Create patch dialog now lists exactly what the main Bible tree lists: books you added (including the apocrypha books, which sit after Malachi) appear, and books, chapters and verses you removed are gone. The order you arranged in the tree is kept.
* The "Add Bible" link now has its own row above the bottom bar of a blank Bible panel, so it no longer overlaps the status circles.
* Save module as... and Save concordance as... show only the file name in the Name box, not the whole folder path (the folder is the dialog's starting folder).
* Files: unitabout.pas, Project.pas, FormExportPatch.pas, bibletreeview.pas, FormProjectExport.pas, verse_panel_strip.pas.

### 0c. Version 1.1.0.064 (2026-11-19) — USFM 3.0 import and export

* Full USFM 3.0 import: headings, paragraph, poetry (\q1-\q4), list and table markers, \m, \b, character styles (\wj, \add, \nd, \qt, \bd, \it, \sup, nested ones with +), \w words with lemma, strong and x-morph, footnotes (\f, \fe), cross-references (\x), unfoldingWord alignment (\zaln-s/-e) and speaker milestones (\qt-s who="Jesus" becomes red letters); sidebars (\esb) and peripherals are skipped. Everything is turned into theWord tags with the paragraph rule (boundary at the end of the verse before, indent at the start of the next).
* Full USFM 3.0 export: one file with all the books and, beside it, a folder named <file>_usfm with one USFM 3.0 file per book (01-GEN.usfm ... 41-MAT.usfm ...). Each book has \id, \usfm 3.0, \ide, \h, \toc1-3 and \mt1; paragraphs, poetry indents, headings, red letters, added words, footnotes, cross-references and Strong's, lemma and morphology on each word (\w word|lemma strong x-morph\w*) are written, with nested styles using the + prefix.
* New save-as type "USFM 3.0 Bible, per-book files (*.zip)" in Export project: a zip holding only the per-book USFM 3.0 files.
* Importing two verses that share one line no longer puts a blank beside a paragraph tag (it broke theWord's no space beside CM/CL/CI rule).
* The "Add Bible" link now also shows on a Bible panel that has no text for the verse you are on (for example a module that lacks that book), not only on a panel with no text at all.
* Shift+F1 guide updated for USFM 3.0 import and export.
* Files: esword_usfm.pas, project_exporter.pas, FormProjectExport.pas, unitabout.pas.

### 0d. Version 1.1.0.062 (2026-11-19) — Add Bible Link on Blank Panels

* A Bible panel that has no text now shows a blue "Add Bible" link on its bottom bar while the pointer is over it. Click it to open Project settings on that panel's text with the Add Bible Books file dialog already open (several files can be chosen, for example one USFM file per book). Panels that already have text do not show the link.
* Help > New features guide (Shift+F1), topic 12: describes the Add Bible link.
* Files: `verse_panel_strip.pas`, `Project.pas`, `FormProjectProperties.pas`, `formprincipal.pas`, `unitabout.pas`.

### 0e. Version 1.1.0.061 (2026-11-19) — Add Books Names All Chosen Files

* Add / replace Bible books with several files chosen together: the note "Waiting to be applied (OK)" and the book dialog now name every file the chosen books come from (for example Tobit and Judith from their two USFM files), not only the first file.
* Files: `FormBookImport.pas`.

### 0f. Version 1.1.0.060 (2026-11-19) — New Features Guide Updated

* Help > New features guide (Shift+F1): new topic 12, "Tree: Arrange, Delete, Add", with its own illustration, explaining dragging rows by the grip and the Tools switch, the .order file for Timotheos, deleting a book, chapter or verse, adding books from one or several files, and the MySword export. The Import/Export topic now says that Add Bible Books reads only the files you choose. The old summary is now topic 13.
* Files: `unitabout.pas`.

### 0g. Version 1.1.0.059 (2026-11-19) — Several Files in Add Books

* Add / replace Bible books: you can now select several files at once in the file dialog (for example one USFM file per book, Ctrl- or Shift-click) and tick the books you want from all of them. The files are read together like the files of a compressed archive; a book found in more than one file asks which file to use.
* Files: `FormBookImport.pas`.

### 0g. Version 1.1.0.058 (2026-11-19) — Tree Grips and Scrolling Fixed

* Bible tree: the extra drag grips (several columns of dots, left behind when the tree scrolled sideways) and the garbled rows after scrolling are fixed. While the grips show, the tree no longer scrolls sideways, and any scrolling repaints the whole tree. Turn the Tools > Rearrange switch off to get the sideways scrollbar back.
* Files: `formprincipal.pas`.

### 0g. Version 1.1.0.057 (2026-11-19) — Delete Books, Chapters, Verses; USFM Book Selection

* Right-click a book, chapter or verse in the Bible tree and choose Delete this book / chapter / verse. iBiblia asks "Are you sure you want to delete this Book (Chapter, Verse)?" with Yes and No. On Yes the rows leave the project (texts, associations, comments, status), the tree is rebuilt without them, and a copy of the project made just before is kept next to it (name ending _before-delete_date_time).
* A project remembers whether it is Old Testament, New Testament or both after a deletion, so deleting the first or last verse does not change the project's scope. Deleting a whole apocrypha book also removes its name and place.
* Add / replace Bible books: choosing one USFM file now imports only that file. Before, when the file held one book, every .usfm/.sfm file in the same folder was read and imported (choosing Sirach brought in the whole folder). New projects and the project properties still read a whole folder of USFM books.
* Files: `project_book_import.pas`, `Project.pas`, `bible_tree_coordinator.pas`, `formprincipal.pas`, `esword_usfm.pas`, `FormBookImport.pas`.

### 0g. Version 1.1.0.056 (2026-11-19) — Blank Panels No Longer Erase a Verse

* Safeguard against a verse losing its texts: when a verse panel is blank and untouched while the project still holds text for that verse (the panel did not load), leaving the verse no longer writes the blank panel over the saved text or clears the verse's associations. This guards the Destination, Reference 1 and Reference 2 texts of Genesis 1:1 / Matthew 1:1 that went blank after reopening a project.
* Files: `current_verse_workspace.pas`.

### 0g. Version 1.1.0.055 (2026-11-19) — Switch for Rearranging

* New Tools > Rearrange books, chapters and verses switch. When it is checked, the grip icons show in the Bible tree and you can drag rows; when it is unchecked, the grips disappear and dragging is off. The choice is remembered. (It is checked by default.)
* Files: `formprincipal.pas`.

### 0g. Version 1.1.0.054 (2026-11-19) — MySword Export Fixed

* Exporting a Bible as MySword (.bbl.mybible) now really writes a MySword module. Before, the file carried the MySword name but was an e-Sword RTF module inside, so MySword could not read the text correctly. The module now has MySword's own Details and Bible tables, with the verse text in MySword's tag style (Strong's numbers, morphology, headings, footnotes, paragraphs, words of Jesus).
* Apocrypha / deuterocanonical books (67 and up) are no longer left out of a MySword export.
* The module's right-to-left setting, its About text (shown in MySword as Comments), the description and the short name are now written; the Strong's flag is set only when the text really has Strong's numbers. Cross-reference marks are written the MySword way (<RX1.2.3>).
* Files: `project_exporter.pas`, `myswordmodule.pas`, `tests/project_exporter_integration_test.pas`.

### 0g. Version 1.1.0.053 (2026-11-19) — Arrange Books, Chapters and Verses in the Tree

* Arrange the Bible tree your own way: every book, chapter and verse row now has a grip (six small dots) at its right end. Click it, hold, and drag the row up or down; a line shows where it will land, and the list scrolls when you hold the mouse near its top or bottom edge. A verse can be dropped into another chapter, a chapter into another book, and books can change places. Only the position changes: chapter and verse numbers stay as they are, so references keep their meaning. A chapter keeps at least one verse and a book at least one chapter.
* The arrangement is saved in the project and comes back when you open it again. Right-click the tree > Reset the order of books, chapters and verses puts everything back in the standard order.
* Exporting a module (theWord, e-Sword, USFM, MySword and the interlinear) now also writes a small .order file next to it (for example amb.nt.order) with your arrangement, so Timotheos shows the books, chapters and verses in the same order. theWord ignores the file. The module itself stays in the standard order.
* Fixed garbled rows in the left Bible tree while scrolling with the dark and Sepia skins (the tree background was being painted over rows that had just been scrolled into place).
* Files: `bible_tree_coordinator.pas`, `Project.pas`, `formprincipal.pas`, `unitskins.pas`.

### 0h. Version 1.1.0.052 (2026-11-18) — SWORD Modules

* SWORD (CrossWire) modules can now be used: Bible texts as the Source, Destination or reference text (New project and Project settings), in Add Bible Books and in Replace, and dictionaries (StrongsGreek, StrongsHebrew and others) in the popup dictionary lists. Choose the module's .conf file or its downloaded .zip. Strong's numbers, morphology, footnotes, words of Jesus and the apocrypha books are kept; a SWORD dictionary is converted once to a theWord dictionary. Locked modules are not opened; bzip2 / xz compressed modules need that program installed.
* Files: `sword_module.pas`, `sword_canons.pas` (new), `module_conversion.pas`, `bible_archive.pas`, `FormBookImport.pas`, `FormNewFeaturesGuide.pas`.

### 0zc. Version 1.1.0.051 (2026-11-18) — Export as Bible Module from the Panel Menu

* Right-click a Bible panel > Export as Bible module... now works: it opens the Export project window on the matching tab (Destination panel: Tagged Destination; Source panel: Interlinear Source). On a reference text it explains that reference texts cannot be exported as a module.
* Files: `Project.pas`, `formprincipal.pas`, `FormProjectExport.pas`.

### 0zd. Version 1.1.0.050 (2026-11-18) — Compressed Files for Bible Books and Dictionaries

* Add Bible Books and Replace now accept compressed files: a zip, tar, tar.gz / tgz, tar.bz2, tar.xz, single .gz, and 7z or rar when 7-Zip / unrar is installed. A compressed file can hold one book or a whole Bible, as theWord, e-Sword, MySword, USFM / USFX modules or text documents (.txt, .rtf, .odt, .docx), also inside folders of the archive.
* All the files found are combined into one book tree. Plain documents ask for the book matching as before. When the same book is in more than one file, iBiblia asks, book by book, which file to take it from. The order of the apocrypha books follows the files.
* Unpacking is safe: names that leave the temporary folder are skipped, links are removed, a size limit stops zip bombs, and the temporary files are deleted afterwards.
* Popup dictionaries (Strong's, morphology, word dictionaries in Project settings, New project and Default dictionaries) can also be chosen from a compressed file; the .dct.twm inside is unpacked into the iBiblia dictionaries folder of the user and used from there.
* Files: `bible_archive.pas` (new), `bible_import_source.pas`, `FormBookImport.pas`, `FormDefaultDictionaries.pas`, `FormNewProject.pas`, `FormProjectProperties.pas`.

### 0ze. Version 1.1.0.049 (2026-11-18) — Apocrypha Books Between Malachi and Matthew

* The apocrypha / deuterocanonical books (Tobit, Judith, Sirach, Maccabees, etc.) now sit between Malachi and Matthew in the Bible tree and in the project's verse order, so Next / Previous verse goes from Malachi to the first apocrypha book and from the last one to Matthew.
* The order of these books follows the imported source text (the order of the books in the file you add them from), not iBiblia's own book numbers. When you add books from a second file, the new books come after the ones already there.
* Projects that kept their apocrypha books after Revelation are arranged this way automatically when opened (one safe database change; nothing else is touched).
* Files: `apocrypha_books.pas`, `project_book_import.pas`, `bible_import_source.pas`, `FormBookImport.pas`, `bible_tree_coordinator.pas`, `Project.pas`, `formprincipal.pas`.

### 0zb. Version 1.1.0.048 (2026-11-18) — Project Backup and Restore

* Project settings, Project tab: new Backup section with "Back up this project" and "Restore project..." buttons and the backup folder (chosen once with "Change...", or asked on the first backup). A backup is a zipped copy of the project (.bib.zip, named with the project name, date and time), made with SQLite's own copy and checked before it is zipped; you can add a note to it.
* Before replacing, adding or clearing a Bible text, applying a patch or applying AI results, iBiblia asks: "Back up and continue", "Continue without backup" or Cancel. Of these automatic backups only the newest 2 of each project are kept; backups you make yourself are never deleted.
* Restore project lists the backups of the project (date, type, note, associated verses, size), or lets you choose another backup file. The current project is backed up first, so a restore can be undone.
* File > Open now also opens zipped projects (.bib.zip): iBiblia unpacks the project to a .bib file where you choose and opens it.
* Files: `project_backup.pas` (new), `project_backup_ui.pas` (new), `FormProjectProperties.pas`, `formprincipal.pas`, `formaiprocessing.pas`.

### 0za. Version 1.1.0.047 (2026-11-18) — Add Bible Books, Replace Chosen Books or Verses

* Project settings, Texts tab: new "Add Bible Books..." button beside "Replace ... text in whole project" for the Source, Destination, Reference 1 and Reference 2 texts. Choose a Bible file (theWord, e-Sword, USFM, USFX, MySword) or a text document (.txt, .rtf, .odt, .docx); iBiblia finds its books and shows them in a Bible tree with the project text and the file text side by side. Only books missing from the project can be ticked (books it already has are greyed out). Apocrypha / deuterocanonical books (Tobit, Judith, Sirach, 1-4 Maccabees, etc.) are added after Revelation and appear in the Bible tree. If the file has books of the other Testament (e.g. Old Testament books in a New Testament project), iBiblia asks first and then makes the project a whole-Bible project.
* "Replace ... text in whole project" now asks first: replace the whole text, or choose books, chapters or verses to replace. Choosing opens the same Bible tree (like Create/Apply patch), listing only the verses whose text differs, with "Try to keep the current associations".
* Text documents: book titles, chapter headings (Chapter 5, Κεφάλαιο 5, Greek numerals) and verse numbers (raised, lowered or at the start of a line) are recognised; you check or change each book in a list before importing. Sub-verses (1a, 1β) are joined to their verse, and double numbering such as 23(29) or 36:8(39:1) uses the number in brackets.
* The chosen books are listed on the Texts tab and are added or replaced when you click OK.
* Files: `FormProjectProperties.pas`, `FormBookImport.pas` (new), `bible_import_source.pas` (new), `bible_document_reader.pas` (new), `project_book_import.pas` (new), `Project.pas`, `bibletreeview.pas`, `verse_lookup.pas`, `formprincipal.pas`.

### 0{. Version 1.1.0.046 (2026-11-17) — Popup No Longer Slows Down Over Time (Linux)

* Linux: the dictionary popup no longer gets slower and slower the longer iBiblia runs. Each popup with indented definitions (e.g. Thayer 1a, 1b) left new hidden formatting entries behind that were never removed, so every later popup took longer. Indents now reuse the same entries. In a test of 300 popups the time per popup stayed the same instead of growing about six times.
* Files: `FormDictionaryPopup.pas`.

### 0{. Version 1.1.0.045 (2026-11-17) — See-Through Panel Bar (Linux)

* Linux: the bar at the bottom of each Bible panel is now really see-through. The bar's window is cut to the shape of its text, status circles and Edit/Properties boxes, so the verse text behind it shows everywhere else (earlier versions still drew a solid bar).
* Files: `verse_panel_strip.pas`, `formprincipal.pas`.

### 0{. Version 1.1.0.044 (2026-11-17) — Panel Bar Really Transparent (Linux)

* The bar at the bottom of each Bible panel is now truly transparent: it draws no background at all, only its text, status circles and Edit/Properties boxes, so verse text running behind it stays visible (Linux).
* Files: `verse_panel_strip.pas`, `formprincipal.pas`.

### 0{. Version 1.1.0.043 (2026-11-17) — Truly Transparent Panel Bar, Settings Kept Across Updates (Linux)

* The bar at the bottom of each Bible panel is now really see-through: only its text, status circles and Edit/Properties boxes are drawn, so verse text behind it can be seen (Linux).
* Linux: settings (default dictionaries, skin, etc.) are now kept in the .config/iBiblia folder of your user account instead of next to the program, so unpacking a new version in another folder no longer loses them. An existing iBiblia.ini next to the program is copied there the first time.
* Files: `verse_panel_strip.pas`, `formprincipal.pas`.

### 0z. Version 1.1.0.042 (2026-11-17) — Transparent Panel Bottom Bar

* The bar at the bottom of each Bible panel (Source, Destination, Reference 1, Reference 2) no longer has its own background colour or top line: it takes the panel's colour, so only the panel name, status circles, Edit and Properties show.
* Files: `verse_panel_strip.pas`.

### 0y. Version 1.1.0.041 (2026-11-17) — Classic Skin Always Light (Linux)

* Linux: the Classic skin is now always light, even when the desktop (KDE) theme is dark. Controls, menus, message boxes and the window title bar use light colours with Classic; other skins use their own colours as before.
* Files: `unitskins.pas`.

### 0x. Version 1.1.0.040 (2026-11-17) — Faster Word Association (Dictionaries)

* Faster word association with large dictionaries: the English word dictionary (e.g. Webster 1828) is searched through an in-memory name list instead of scanning the whole file for every word you point at, which made association feel slower and slower after updating to big dictionaries.
* Files: `dictionary_manager.pas`.

### 0w. Version 1.1.0.039 (2026-11-17) — Seamless Linux Updates, Faster Translation Memory

* Linux updates are now seamless: new versions download in the background by default (no installer step), then a small 'Restart now / Later' box appears. 'Restart now' installs the update, asks to save any unsaved changes, and reopens iBiblia by itself. 'Later' installs it when you next close iBiblia.
* Much less slowing down with use (Linux): the Translation memory panel rebuilt long lists with repeated text processing that grew with the project; it is now several times faster, shows the 12 most used translations per word with a '+N more' link to see all, and gives the same order as before. Dictionary look-ups are remembered for the session (and cleared when a dictionary is changed), and a dictionary file that was replaced with a newer copy is reopened when you press OK in the dictionary settings.
* Files: `unitupdate.pas`, `chapterview.pas`, `translation_memory_model.pas`, `dictionary_manager.pas`.

### 0v. Version 1.1.0.038 (2026-11-16) — Paste Without Line-Break Error

* Pasting text from another program into the verse editor no longer causes the 'line break' error. Line breaks in the pasted text (from documents, web pages or other Bible programs) become single spaces, as Rubio planned for issue #122. Text copied inside iBiblia is pasted unchanged. (The fix for the lost first verse, 'Prevent saving unloaded verse views', was already included.)
* Files: `theword_wysiwyg_editor_control.pas`.

### 0u. Version 1.1.0.037 (2026-11-16) — Dark Title Bar on Linux (KDE)

* Linux (KDE): the window title bar now takes the dark skin colours. KDE draws title bars itself; iBiblia now tells it which colours to use for each window (through the _KDE_NET_WM_COLOR_SCHEME hint). With a light skin the desktop's own colours are used again. Message boxes drawn by GTK itself may still use your KDE title colours.
* Files: `unitskins.pas`.

### 0t. Version 1.1.0.036 (2026-11-16) — Dark Message Boxes on Linux

* Linux: message boxes (such as 'Close project' and 'Clear associations') are dark in a dark skin. They are drawn by GTK and kept the system's light colours; their window, buttons and text now follow the skin.
* Files: `unitskins.pas`.

### 0s. Version 1.1.0.035 (2026-11-16) — Saved Indicator on Linux

* Linux: the Saved / Unsaved changes indicator at the bottom left of the window is back (green 'Saved', red 'Unsaved changes'). GTK draws the status bar itself and hid the indicator; it now sits over the left end of the status bar, and other status messages (such as 'Analyzing verse...') appear next to it.
* Files: `formprincipal.pas`.

### 0r. Version 1.1.0.034 (2026-11-16) — Dark Theme in All Linux Windows

* Linux: consistent dark theme in all windows. The scrollbars, buttons, tab strips, drop-down lists and window backgrounds that GTK draws itself stayed light inside a dark skin (most visible in the word popup, the verse tree and the status list); they now follow the skin colours, and go back to the system theme with a light skin. The window title bars are drawn by your desktop and follow its theme.
* Files: `unitskins.pas`, `FormDictionaryPopup.pas`.

### 0q. Version 1.1.0.033 (2026-11-16) — Timotheos Synchronization on Linux

* Linux: synchronization with Timotheos works again. iBiblia → Timotheos and Timotheos → iBiblia follow each other's verse through the same two small files Timotheos uses on Windows (ibiblia-sync-in.json / ibiblia-sync-out.json in its user-data folder, e.g. ~/.local/share/org.timotheos.bible or ~/.config/Timotheos), and a running Timotheos is detected through /proc. The sync button on the toolbar and Tools > Synchronization with Timotheos are back on Linux (theWord sync stays Windows-only).
* Files: `tanglaw_sync.pas`, `formprincipal.pas`.

### 0p. Version 1.1.0.032 (2026-11-16) — Broken Menu Entries on Linux

* Linux: iBiblia now removes broken applications-menu entries for itself (for example one left by an older copy of iBiblia that was moved or deleted, which showed 'Could not find the program ...' when clicked), and keeps one working entry for the copy you run.
* Files: `app_updater.pas`.

### 0o. Version 1.1.0.031 (2026-11-16) — Restart After Update on Linux

* Linux: after an automatic update the new iBiblia now starts by itself. It used to start while the old one was still closing, and failed with 'Unable to open file ... iBiblia.ini: Try again'. The restart now waits (up to 20 seconds) until the old copy has exited.
* Files: `app_updater.pas`.

### 0n. Version 1.1.0.030 (2026-11-16) — Applications Menu Entry on Linux

* Linux: iBiblia now adds itself to the applications menu the first time it runs from a folder (a menu entry for your user only, with the iBiblia icon), and updates the entry if you move the folder. Set IBIBLIA_NO_MENU=1 to turn this off. The install-menu-entry.sh script is still included.
* Files: `app_updater.pas`, `unitupdate.pas`.

### 0m. Version 1.1.0.029 (2026-11-16) — Dictionary Settings and Popup Paragraphs

* Per-module dictionaries: when the default dictionaries are enabled in Project > Properties, iBiblia no longer looks for (or complains about) the dictionary selected for each text. Dictionary paths saved on Windows (such as C:/Users/.../x.dct.twm) are no longer treated as folders inside the program folder on Linux: they are looked for next to the project or the program by file name, and any message shows the real saved path.
* Linux: dictionary entries in the word popup now keep their paragraph formatting: indented and hanging-indent lines (1), 1a), A., II.) and the space between paragraphs, as on Windows.
* Files: `Project.pas`, `FormDictionaryPopup.pas`.

### 0l. Version 1.1.0.028 (2026-11-16) — Linux Popup Speed and Wheel Selection

* Linux: the word definition popup no longer slows down and freezes iBiblia. Each time it showed a definition, the GTK text box kept every colour/bold/italic style it had ever used, so after a few dozen words the program spent all its time on the popup. Each style is now created once and reused (hovering 12 words: about 1 second of work instead of 10).
* Mouse-wheel word selection steps through the real source words only: it skipped onto the interlinear glosses (the small 'Shall Be', 'Great,' words) in a source text such as TRi. When the project's auto-advance only stops on words with Strong's or morphology, the wheel follows the same rule.
* Files: `FormDictionaryPopup.pas` (cached GTK text tags instead of RichMemo's per-call tags), `Verse.pas` (wheel word filter).

### 0k. Version 1.1.0.027 (2026-11-16) — Automatic Updates

* Automatic updates: iBiblia checks the SalitaNgDiyos/ibiblia GitHub releases a few seconds after it starts. When a newer version exists, a window shows the release notes and offers Update and restart, Skip this version or Later. The download is verified with SHA-256 before anything is installed, and your projects and iBiblia.ini are never touched.
* Tick 'Install future updates automatically' in that window to download new versions in the background and install them when you close iBiblia; the next start is already the new version.
* Help > Check for updates... checks on demand; Help > Check for updates when iBiblia starts switches the start-up check off or on. Set IBIBLIA_NO_UPDATE=1 to disable updating completely.
* Every new version is built by GitHub with Lazarus for Windows (x64) and Linux (x86_64) and published as a release, so the updater always has a matching package.

* **How it works (`app_updater.pas`, `unitupdate.pas`)**: the program asks the GitHub API for the latest release, picks `iBiblia-<version>-windows-x64.zip` or `iBiblia-<version>-linux-x86_64.zip`, checks it against `SHA256SUMS.txt`, unpacks it and swaps the files in. Each replaced file is first renamed to `<name>.old-update` (allowed while the program runs, on Windows and Linux) and deleted at the next start; if any step fails the old files are put back. It does nothing when the program folder is not writable.
* **Releases (`.github/workflows/release.yml`)**: pushing a tag such as `v1.1.0.027` builds both packages with Lazarus on GitHub Actions and publishes the release with the checksums and these notes. The tag must match the version in `iBiblia.lpi` and `iBiblia-linux.lpi`.

### 0j. Version 1.1.0.026 (2026-11-16) — Linux Audit of the New Features Guide

An audit of the 12 topics of Help > New Features User Guide on the Linux build found and fixed:

* **Mouse-wheel word selection on Linux (`Verse.pas`, `directwrite_syntagm_view.pas`)**: GTK delivers the wheel to the text view under the pointer and does not pass it on to the panel, so wheel selection of source words and Ctrl+wheel zoom did nothing. The view now has the wheel handler too (non-Windows only).
* **F8 / Shift+F8 with a status filter (`formprincipal.pas`)**: stepping now starts from the verse on screen, not from the tree selection, which the tree may move silently when the filter hides the current verse; when the target is already selected it navigates directly. After filtering, a selection moved that way is cleared so a click on the node navigates. The two commands are now also in the Navigation menu.
* **Word definition popup position (`FormDictionaryPopup.pas`)**: candidate positions are tried from south-east to north-west, so equal fits now favour south-east of the pointer (before: south-west).
* **Unreadable push buttons in dark skins on Linux (`VerseMemo.pas`, `unitabout.pas`)**: Raw/Visual, Save, Cancel and the About-unit OK buttons keep the theme text colour on GTK, where buttons are drawn light.
* **Properties editor colours on Linux (`formprincipal.pas`)**: the colours are set again after the memo is shown, so it is dark in dark skins the first time too.
* **Find in project previews (`FormFindInProject.pas`)**: markup is removed from the result previews, except for the Strong's tags scope.

Checked and working on Linux: wheel/click/right-click association, paired-word highlight and dictionary popup, hover chevrons, status circles and keys 1–4 with tree roll-up, Properties and Edit modes, Enter/Shift+Enter paragraph keys, Raw view, Cancel, skin toggle (Ctrl+Shift+D), Find in project (Ctrl+F) and double-click navigation, status filter and F8, USFM export (headings, footnotes, words of Jesus, added words), AI options dialog, toolbar icon sizes, right-to-left panel toggle, Translation memory tab and translator-note hover preview.

### 0i. Version 1.1.0.025 (2026-11-16) — Paragraph Indents in the Windows Verse Panels

* **`<PI>`/`<PF>` Indents and the `<CM>` Gap on Windows (`directwrite_paragraph_layout.pas`, `directwrite_syntagm_view.pas`)**: DirectWrite has no paragraph indent, so a verse that carries theWord paragraph formatting is now laid out with one DirectWrite layout per paragraph:
  * each paragraph is narrowed by its `<PI>` indent and drawn shifted by it, so wrapped lines stay indented;
  * the `<PF>` first-line indent is an invisible spacer in front of the paragraph's first line;
  * a paragraph after `<CM>` gets the gap above it.
  * The combined layout answers the same calls as a single DirectWrite layout (drawing, hit testing, caret and selection rectangles, line and cluster metrics), translating text positions between the verse and its paragraphs. Verses without `<PI>`, `<PF>` or `<CM>` keep the single native layout.
* Checked with a Windows (win64) cross build run under Wine: the Destination panel draws `<CM><PI2>` and `<PF>` paragraphs indented, wrapped lines included. Wine does not implement DirectWrite hit testing, so word clicks were not testable there.

### 0h. Version 1.1.0.024 (2026-11-16) — Paragraph Tags in the Verse Panels

* **Paragraph Tags in the Source, Destination, Reference 1 and Reference 2 Panels (`verse_visual_adapter.pas`, `verse_visual_document.pas`, `directwrite_syntagm_view.pas`)**:
  * `<CM>` and `<CI>` now break the line; `<CM>` also leaves a gap above the next paragraph. Both show a dim marker like `<CL>` does (pilcrow for `<CM>`, reversed pilcrow for `<CI>`).
  * `<PI>` / `<PI1>`..`<PI7>` indent the whole paragraph and `<PF>` / `<PF1>`..`<PF7>` its first line (`<PI0>`, `<Pi>` and `<PF0>` reset); level 1 is 40 px, +20 px per level. Indents end at the next line break.
  * The indents are drawn by the text layout used on Linux. The Windows DirectWrite view receives the same line breaks, but its indents are not drawn yet (drawn from 1.1.0.025).

### 0g. Version 1.1.0.023 (2026-11-16) — theWord Paragraph Tags in the Chapter Panel & Folder Info in About

* **theWord Paragraph Tags in the Chapter Panel (`chapterview.pas`)**: The chapter panel now renders the theWord paragraph formatting tags, in Paragraph mode:
  * `<CM>` starts a spaced paragraph, `<CI>` a tight paragraph and `<CL>` a new line inside the same paragraph.
  * `<PI>` / `<PI1>`..`<PI7>` set the left indent (level 1 = 40 px, +20 px per level); `<PI0>` and `<Pi>` reset it.
  * `<PF>` / `<PF1>`..`<PF7>` set the first-line indent (`<PF0>` = none). With no `<PF>` a paragraph keeps the usual first-line indent, unless it is indented with `<PI>` or follows `<CL>`.
  * `<PI>`/`<PF>` apply to the paragraph being built and reset at the next paragraph boundary, as the theWord rules say; the order boundary, `<PI>`, `<PF>`, text is honoured.
  * Tags at the very start of a verse are applied before the verse number, so the number starts the new paragraph.
  * Out-of-contract spellings (`<PI8>`, `<Pf>`) are ignored. With Paragraph mode off the tags are ignored, as before.
* **Program and Working Folder in About (`unitabout.pas`, `formprincipal.pas`)**: The About box lists the program folder and the working (settings) folder, the place `iBiblia.ini` is kept (the program folder, or `$XDG_CONFIG_HOME/iBiblia` / `~/.config/iBiblia` on Linux when the program folder is read-only). Long paths are shortened in the middle, the hint shows the full path, and a click opens the folder. When the settings are kept beside the program, a single "Program and working folder" line is shown instead of two.

### 0g. Version 1.1.0.021 (2026-11-15) — Dialog Skin Completed on First Open

* **Dialog Controls Skinned on First Open (`formprincipal.pas`)**: In 1.1.0.020 a dialog such as **Export project** got its dark background on first open, but its tab pages, check boxes and buttons stayed light. The dialog becomes the active form before its window handles exist, and the dark-mode subclass hooks (`HookButtonControl`, `HookCheckBoxControl`, `HookTabControl`, `HookFormControl`) need those handles. `SkinFormOnce` now sets the colours at once, queues `DeferredSkinForm` (`Application.QueueAsyncCall`) to apply the full skin once the dialog is visible, creates the handles of controls on tab pages not yet shown (`EnsureChildHandles`), and only then records the form as skinned.

### 0f. Version 1.1.0.020 (2026-11-15) — Go to Verse Lookup & Skin Fixes

* **Go to Verse Lookup Box (`FormGoToVerse.pas`, `verse_lookup.pas`, `bible_versification.pas`, `formprincipal.pas`)**: Added **Tools > Go to verse...** (`Ctrl+G`), a theWord-style lookup box. Type `jn 3 16`, `jn 3.16`, `1co 13:4` or `ps 23` and press `Enter`.
  * A space, period, colon or comma separates chapter and verse.
  * Books are recognised by standard abbreviations, full names, or the first letters of the name (English, the interface language, Tagalog, and the project's extra books), preferring books that exist in the open project; Roman numerals (`ii tim 3 16`) work too.
  * With no book typed, `3 16` goes to chapter 3 verse 16 of the current book, `3` to chapter 3, and `:16` / `.16` to verse 16 of the current chapter. One-chapter books take a single number as the verse (`jude 5`).
  * The box previews the resolved reference while typing and says why a reference cannot be reached (unknown book, book not in the project, no such chapter or verse).
  * Typing a letter while the Bible tree has the focus opens the box with that letter entered.
* **Skin Kept After Opening or Closing a Project (`formprincipal.pas`)**: Opening another `.bib` or closing a project now re-applies the active skin (any skin except Classic), so the state header strip, Bible tree and verse panels no longer revert to light colours until a restart.
* **Dialogs Skinned on First Open (`formprincipal.pas`)**: Dialogs such as **Export project** are now skinned the first time they open. The active-form handler skinned the form that was active *before* the change, so a dialog was only skinned after it had been closed once. Freed dialogs are also removed from the skinned-forms list so a new dialog at a reused address is not mistaken for one already skinned.

### 1. Dictionary Popup Enhancements & Layout Persistence

* **Instant Word-to-Word Hover Refresh**: Moving the mouse pointer directly from one tagged word to another in the **Source** or **Destination** panel immediately updates the Strong's and Morphology popup entry without requiring the cursor to exit the word area first.

* **Mouse-Wheel Word Selection Trigger**: Scrolling the mouse wheel over the Source or Destination panel to step through Source words automatically refreshes the dictionary popup for the newly selected word.

* **Destination-Only Paired Lookup**: Restricted Destination-to-Source paired word dictionary fallback strictly to the **Destination** panel (excluding Reference 1 and Reference 2).

* **Direct Mouse-Wheel Scrolling & Zoom Inside Popup**: Enabled smooth mouse-wheel scrolling (and `Ctrl` + Mouse Wheel font zooming) directly inside the Strong's and Morphology panes when hovering over the dictionary popup window.

* **Right-Edge & Left-Edge Docking + 1-Inch Pointer Clearance**: Added **Dock Word Definition Popup to Left Edge** alongside **Dock Word Definition Popup to Right Edge** in the **View** menu (mutually exclusive), and increased floating popup clearance to a full **1-inch gap** (`Screen.PixelsPerInch`, minimum `96 px`) from the mouse pointer when undocked so the popup never crowds the hovered word.

* **Persistent Popup Size & Dock State Across Restarts**: Fixed popup layout persistence in `iBiblia.ini` so user-resized width (`popup.largura`), height (`popup.altura`), right-edge and left-edge dock states (`popup.dock.right`, `popup.dock.left`) and offsets (`popup.dock.offset.x`/`y`, `popup.dock.left.offset.x`/`y`), and morphology splitter height (`popup.morfo.altura` / `popup.panel2.altura`) are preserved across application restarts.

### 2. Destination Status Strip & Bible Tree Roll-Up Synchronization

* **Synchronized 4-Status Radio Strip**: Kept the 4 status radio circles on the Destination panel (`TVersePanelStrip`), `CurrentProject.Status`, and the main Bible tree (`TreeView1`) continuously synchronized across word associations, disassociations, keyboard shortcuts (`1`–`9`), and verse navigation.

* **Automatic Verse Status Progression**:

  * Automatically advances verse status from `Not associated` to `Associating...` on partial word association.

  * Automatically advances verse status to `Associated!` (green) once all words in the verse are associated (deferring until `Ctrl`/`Shift` release during multi-word association, and optionally auto-saving when **Save on last status** is enabled).

  * Automatically reverts to `Not associated` when all associations in a verse are cleared.

* **Real-Time Chapter & Book Tree Status Roll-Up**: Added `RecalculateTreeParentStatuses` so parent Chapter and Book nodes in `TreeView1` (both normal and selected icons) immediately display green (`Associated!`) whenever all child verses and chapters under them are green.

### 3. Interactive Association & Tag Feedback

* **Clean Hover Tag Underlining**: Refined Strong's tag hover feedback to underline equivalent Greek/Hebrew Strong's number tags without background block clutter, while preserving Destination-to-Source paired word highlighting.

* **Multi-Word `Ctrl+Shift` Association & Auto-Advance**: Restored `Ctrl+Shift` multi-word selection and association with automatic advance to the next unassociated Source word upon key release (and right-click quick association on the final unassociated word).

* **Hover-Only Verse Navigation Chevrons (`‹ ›`)**: Added quick previous/next verse chevron buttons and an in-place module **Properties** editor and **Edit** toggle on the verse panel control strip.

### 4. 64-Bit Architecture, Dark Mode & External Synchronization

* **Native 64-Bit Windows Build (`x86_64-win64`)**: Compiled with Free Pascal (FPC 3.2.2) for 64-bit Windows using 64-bit `sqlite3.dll` and `pcre64.dll`, including support for compressed dictionary modules.

* **Full Dark Mode, Night & Sepia Skins**: Added system-wide Dark, Night, Sepia, and Classic skins (`Ctrl+Shift+D` quick toggle) with themed title bars, menus, scrollbars, splitters, and dialogs.

* **64-Bit C-ABI `WM_COPYDATA` Synchronization**: Added 24-byte Win64-aligned `WM_COPYDATA` bidirectional verse and dictionary lookup synchronization with [theWord](https://www.thewordbooks.com/index.php/download-tools-and-utilities/) and **Timotheos (Tanglaw)**.

* **Find in Project & Default Dictionaries**: Added full-project search (`Ctrl+F`) across Source, Destination, Reference 1/2, Strong's tags, and comments, plus configurable global and per-project Default Dictionaries (Greek Strong's, Hebrew Strong's, Morphology, and Word lookup).