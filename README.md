# iBiblia — 64-Bit Edition (v1.0.1.032)

**iBiblia** is a specialized visual alignment and interlinear tagging workstation for [theWord](https://www.theword.net/) Bible modules. Rather than manually typing raw `<WG...>`, `<WH...>`, and `<WT...>` tags into a text file, you simply associate words between a tagged **Source** Bible (Greek LXX/NT or Hebrew OT) and your **Destination** translation. iBiblia automatically transfers all Strong's numbers and morphological parsing tags, formats verse markup, and exports ready-to-use `.ont`, `.ot`, and `.nt` modules.

For foundational workflow documentation, see the [User Manual (English)](https://www.google.com/search?q=https://github.com/rubiot/ibiblia/wiki/User-manual-%28English%29) and [Manual de Utilização (Português)](https://www.google.com/search?q=https://github.com/rubiot/ibiblia/wiki/Manual-de-utiliza%25C3%25A7%25C3%25A3o-%28Portugu%25C3%25AAs%29).

## New Features & Benefits (v1.0.0.70 – v1.0.1.032)

The 64-bit `v1.0.1.xxx` series introduces a major modernization of iBiblia's word-alignment workflow, dictionary engine, status filtering, project-wide search, in-place verse editor, audio pronunciation, visual theming, and module export pipeline.

### 1. High-Speed Word Selection & Multi-Word Association (`v1.0.1.024` – `v1.0.1.030`)

* **Mouse Wheel Source Word Selection (`Scroll Up` / `Scroll Down`)**: Place your cursor inside either the **Source** text panel or the **Destination** text panel and roll the mouse wheel to step through words in the Source text one by one (`Scroll Up` selects to the left; `Scroll Down` selects to the right). Existing associations automatically highlight their paired Destination words as you scroll.
* **`Ctrl+Shift` Multi-Word Phrase Association**: Hold `Ctrl+Shift` and left-click multiple words in the Source or Destination panels to link entire idioms, compound verbs, or multi-word phrases in a single operation.
* **Auto-Advance on `Ctrl+Shift` Release**: As soon as you release the `Ctrl+Shift` keys after associating target words in the Destination panel, iBiblia automatically clears the completed pair and highlights the **next unassociated word** in the Source text.
* **Benefit**: Eliminates repetitive back-and-forth mouse travel between the Source and Destination panels. Your cursor can remain inside the Destination text while you scroll through Source words, associate phrases, and automatically advance through the verse.

### 2. Southeast Cursor Dictionary Popup & 4-Slot Global Dictionaries (`v1.0.0.74` – `v1.0.1.017`)

* **Southeast Cursor Dictionary Popup in Destination Panel**: Whenever a Source word is selected and your mouse moves inside the Destination panel, the Strong's & Morphology dictionary popup automatically follows `18px` southeast of your cursor. Hovering over an already-associated Destination word temporarily inspects that word's own dictionary entry, and holding `Ctrl` freezes the popup in place so you can scroll or zoom inside it.
* **4 Global Default Dictionary Slots (`Options > Default Dictionaries...`)**:
  * **Greek Strong's Dictionary**: Resolves New Testament (Books 40–66) and Septuagint (LXX) `G`-prefixed Strong's numbers.
  * **Hebrew Strong's Dictionary**: Resolves Old Testament (Books 1–39) `H`-prefixed Strong's numbers, including raw, zero-padded (`H0123`), compound (`Hc/H7225`), and redirect entries.
  * **Morphology Dictionary**: Displays grammatical parsing definitions for Robinson, Packard, and LXX morphology tags.
  * **English Word / Bible Dictionary (`*.dct.twm`)**: Hover over any untagged English word or proper name in any of the 4 Bible panels to view its dictionary definition, complete with automatic plural and verb-stem fallback matching.
* **Compressed Dictionary Support & Auto Font Loading**: Reads Zlib-compressed theWord dictionaries (`compressed=1`) directly from SQLite in memory and automatically registers custom Greek/Hebrew fonts from your theWord installation folder via `AddFontResourceEx`.
* **Benefit**: Keeps lexical definitions and morphological parsing directly in your line of sight while choosing target translation words, without requiring manual dictionary lookups or uncompressed module conversions.

### 3. Verse Status Filtering & Filtered Tree Navigation (`v1.0.0.71` – `v1.0.0.76`)

* **Live Status Filter Dropdown (Above Left Bible TreeView)**: Filter the entire Bible navigation tree by association status with real-time verse counts displayed next to each status option:
  * **All verses (`Total count`)**: Displays the full canonical tree.
  * **Not associated (`Count`)**: Isolates untouched verses that still need word alignment.
  * **Associating (`Count`)**: Shows partially aligned verses currently in progress.
  * **Needs review (`Count`)**: Highlights verses flagged for inspection (including verses automatically transitioned to *Needs review* after a text reload).
  * **Associated! (`Count`)**: Displays completed verses.
* **One-Click & Shortcut Filtered Verse Stepping (`<` / `>` Buttons, `F8` / `Shift+F8`)**: Use the `<` (*Previous verse with status*, `Shift+F8`) and `>` (*Next verse with status*, `F8`) buttons right below the filter dropdown—or under `Navigation > Next/Previous verse with status`—to jump directly to the next or previous matching verse across chapters and books, accompanied by a live match counter (`N found`).
* **Persistent Filter State**: Automatically saves your active status filter both per-project (inside the `.ibb` project database) and globally in `iBiblia.ini`, restoring your exact filtered view whenever you reopen the project.
* **Benefit**: Lets translators track project completion metrics at a glance and step directly through unassociated or *Needs review* verses without manually expanding books and chapters in the tree.

### 4. Project-Wide Search — Find in Project (`Ctrl+F`) (`v1.0.0.70`)

* **Modeless Multi-Scope Search Dialog (`Edit > Find in Project...` / `Ctrl+F`)**: A floating search window that stays open alongside your workspace so you can step through search results while editing or associating words.
* **7 Selectable Search Scopes (`Look in:`)**:
  * **Destination translation (Target text)**: Search words or phrases in your target Bible translation.
  * **Original source (Hebrew / Greek)**: Search original Greek or Hebrew words and lemmas.
  * **Reference text 1 & Reference text 2**: Search across either comparison Bible module.
  * **Strong tags / Pairs**: Search directly by Strong's number (e.g., `G3056`, `H7225`) or morphology tag to find every verse where a specific tag is used or paired.
  * **Translator notes / Comments**: Search inside your personal verse commentary and alignment notes.
  * **All text fields**: Search across all texts, tags, and comments simultaneously.
* **Instant Result Navigation**: Displays the total match count (`N verses found`) and a clean single-line preview for every matching verse. Clicking or double-clicking any result—or using the `< Previous Match` and `Next Match >` buttons—immediately navigates the main workspace to that verse.
* **Benefit**: Makes auditing translation consistency across all 31,102 verses effortless—allowing you to verify how a Greek/Hebrew term, Strong's number, or English phrase was translated elsewhere in the project in seconds.

### 5. Context-Sensitive In-Verse Controls & Status Bar (`v1.0.0.76` – `v1.0.1.026`)

* **Hover-Only Verse Navigation Chevrons (`‹ ›`)**: Subtle previous (`‹`) and next (`›`) verse navigation arrows appear right after the verse text **exclusively** inside whichever of the 4 Bible panels your mouse cursor is currently hovering over, keeping non-active panels free of visual clutter.
* **Interactive 4-Color Verse Status Circles**: Clickable status indicators centered in the Destination panel's bottom bar (also controlled via keys `1`–`4`):
  * **Gray (`1`)**: Not associated
  * **Yellow (`2`)**: Associating (in progress)
  * **Red (`3`)**: Needs review
  * **Green (`4`)**: Associated! (complete)
* **Live Translation Badge & Mode Checkboxes**: Bottom-left badge displays the active module's `short.title`, `title`, or `description`, while bottom-right checkboxes switch any Bible panel between **Association Mode**, **Edit Mode** (`[x] Edit`), and **Module Properties Mode** (`[x] Properties`).
* **Benefit**: Lets you navigate verses, update alignment status, and switch between word association, text editing, and module header configuration directly inside each Bible pane.

### 6. Shared Master Formatting Ribbon, Floating Mini-Toolbar & Live Preview (`v1.0.1.002` – `v1.0.1.008`)

* **Shared Master Formatting Ribbon**: Checking `Edit` on any of the 4 Bible panels reveals a unified formatting bar with one-click buttons for paragraph breaks (`<CM>`, `<CI>`, `<CL>`), paragraph indentation (`<PI0>`..`<PI7>`), first-line indentation (`<PF0>`..`<PF7>`), centered text (`<center>`), and section headings (`<TS1>`, `<TS2>`, `<TS3>`).
* **Intuitive Keyboard Line Breaks**: Press `Enter` to insert `<CI>`, `Double Enter` to upgrade `<CI>` to a `<CM>` paragraph break, or `Shift+Enter` for `<CL>`. All multi-line edits are automatically flattened into valid single-line theWord verse records on save.
* **Floating Selection Mini-Toolbar**: Highlighting any text in the verse editor pops up a contextual toolbar right above your selection for inline tags: `<FI>..<Fi>` (Translator's Added/Italic Words), `<FR>..<Fr>` (Words of Jesus in Red), `<FO>..<Fo>` (OT Quote), `<b>`, `<i>`, `<u>`, `<sup>`, `<sub>`, `<RF>..<Rf>` (Footnotes), plus a one-click **Clear Tags** action.
* **Real-Time Context Panel Preview**: Every edit and formatting tag renders live in the right-hand Context Panel (**Source | Destination | Ref 1 | Ref 2 | Interlinear**).
* **Benefit**: Turns iBiblia into a WYSIWYG-assisted theWord verse and typography editor, eliminating syntax typos and showing exactly how headings, red-letter text, footnotes, and poetry indents will look before exporting.

### 7. Real-Time Text-to-Speech (TTS) Hover Reader (`v1.0.0.73` – `v1.0.0.74`)

* **Unified OneCore & SAPI5 Speech Engine (`Options > Voice / Speech Settings...`)**: Enumerates both modern Windows 10/11 OneCore voices (`Speech_OneCore\Tokens`, such as *Microsoft Stefanos* for Greek and *Microsoft Asaf* for Hebrew) and desktop SAPI5 voices.
* **Dual Independent Voice Routing**: Assign a dedicated Greek or Hebrew voice to the **Source** Bible panel and a separate English/vernacular voice to the **Destination / Reference** panels, with independent speech rate (`-10` to `+10`), volume (`0%` to `100%`), and trigger mode (*Always on Hover*, *Ctrl + Hover*, or *Alt + Hover*). Toggle speech anytime with `Ctrl+T`.
* **Benefit**: Reinforces original-language reading fluency by pronouncing Greek, Hebrew, and translated words aloud as you move across the text.

### 8. Full Dark Mode UI & 7 Built-In theWord Skins (`v1.0.0.80` – `v1.0.1.023`)

* **7 Built-In Color Themes (`View > Skins (theWord Themes)`)**: Instant switching between **Classic (Light)**, **theWord Dark (Charcoal)**, **Night (Pitch Black)**, **Sepia (Parchment)**, **Slate (Steel Blue)**, **Solarized Dark**, and **High Contrast Dark**, or quick-toggle via `Ctrl+Shift+D`.
* **Complete Win32 / LCL Custom Theming**: Applies Windows DWM immersive dark title bars, custom-drawn main menu bars and popup menus, themed scrollbars, owner-drawn buttons and dropdowns, dark-mode RTF dictionary rendering, and themed confirmation dialogs (`ThemedMessageDlg`).
* **Benefit**: Dramatically reduces eye strain during long translation and tagging sessions with zero white flashes or unstyled OS dialogs.

### 9. Complete Bible Module Export, Smart Reload Review & theWord 7 Sync (`v1.0.0.71` – `v1.0.1.027`)

* **One-Click Complete Module Export (`Right-Click > Export as Bible module...`)**: Exports any of the 4 Bible panels directly to a complete `.ont` (31,102 lines), `.ot` (23,145 lines), or `.nt` (7,957 lines) file with UTF-8 BOM, preserving 100% of module header properties and embedding all associated Strong's and morphology tags.
* **Smart Word-Diff Detection on Reload**: When reloading an updated Bible file in `Project > Settings`, iBiblia compares the actual words of each verse—keeping verses **Green (`Associated!`)** if only underlying tags changed, and flagging verses as **Red (`Needs review`)** only when words were added, removed, or edited.
* **Self-Healing Association Integrity**: Automatically validates and repairs misplaced association records (`pare_pares`) on project open and prior to export.
* **Concurrent Bidirectional theWord 7 Synchronization**: Syncs verse navigation both ways (`theWord -> iBiblia` and `iBiblia -> theWord`) with 64-bit theWord 7 via `WM_COPYDATA` without feedback loops.
* **Benefit**: Guarantees clean, spec-compliant theWord module exports and protects hundreds of hours of alignment work when revising source or translation texts.

### 10. Interactive Floating "New Features User Guide" (`v1.0.1.031` – `v1.0.1.032`)

* **Modeless Visual Help Window (`Help > New Features User Guide...` / `F1`)**: A dedicated floating guide window with topic navigation, custom-drawn visual diagrams illustrating each feature's keyboard/mouse workflow, step-by-step instructions, and a one-click **Copy Guide** button.
* **Benefit**: Gives translators an interactive visual reference right alongside their workspace without interrupting their active project.

## Keyboard & Mouse Shortcuts Summary

| Shortcut / Action | Function |
| :--- | :--- |
| **`Mouse Wheel Up / Down`** *(in Source or Destination)* | Select previous (left) or next (right) word in the Source text one by one |
| **`Ctrl + Shift + Left-Click`** | Select and associate multiple words simultaneously across Source and Destination |
| **`Release Ctrl + Shift`** *(after associating)* | Automatically select and highlight the next unassociated word in the Source text |
| **`Right-Click`** *(on Destination word)* | Associate single word and immediately advance to the next unassociated Source word |
| **`Ctrl + Left-Click`** *(on associated word)* | Dissociate (unlink) an existing word association |
| **`Ctrl + F`** | Open **Find in Project** dialog (search Destination, Source, Refs, Strong's, or Notes) |
| **`F8` / `Shift + F8`** | Jump to **Next / Previous verse** matching the active **Status Filter** |
| **`Page Down` / `Page Up`** | Navigate to the next / previous verse in canonical order |
| **`Keys 1 / 2 / 3 / 4`** | Set current verse status: `1` Not associated, `2` Associating, `3` Needs review, `4` Associated! |
| **`Ctrl + Mouse Wheel`** | Zoom Bible panel or Dictionary Popup font size in / out |
| **`Hold Ctrl`** *(while Dictionary Popup is open)* | Freeze Dictionary Popup position so you can move the cursor inside it |
| **`Enter` / `Double Enter` / `Shift+Enter`** *(in Edit Mode)* | Insert `<CI>` line break / upgrade to `<CM>` paragraph break / insert `<CL>` break |
| **`Ctrl + T`** | Toggle real-time Text-to-Speech (TTS) word reading on hover |
| **`Ctrl + Shift + D`** | Toggle between Dark Mode (*theWord Dark*) and *Classic (Light)* |
| **`F1`** | Open the floating **New Features User Guide** window |

## Built-In Color Themes (Skins)

| Theme Name | Background | Primary Ink | Best For |
| :--- | :--- | :--- | :--- |
| **Classic (Light)** | `#FFFFFF` | `#000000` | Daylight reading and traditional Windows appearance |
| **theWord Dark (Charcoal)** | `#1E1E1E` | `#D8D8D8` | Matching theWord 7 dark workspace for low-glare sessions |
| **Night (Pitch Black)** | `#121212` | `#E8E6E3` | OLED displays and late-night translation work |
| **Sepia (Parchment)** | `#F4EEDB` | `#2D241E` | Warm paper-like contrast for extended reading comfort |
| **Slate (Steel Blue)** | `#1C232B` | `#DCE4EC` | Cool navy-slate contrast with crisp cyan highlights |
| **Solarized Dark** | `#002B36` | `#93A1A1` | Precision low-contrast cyan/teal environment |
| **High Contrast Dark** | `#000000` | `#FFFFFF` | Maximum legibility with vivid yellow and cyan accents |

## Credits & Contributors

* **Original Author**: [Rúbio R. C. Terra](https://github.com/rubiot/ibiblia) (`rubio.terra@gmail.com`)
* **64-Bit Edition & Feature Enhancements**: [Edmund Valdez](https://github.com/SalitaNgDiyos)

