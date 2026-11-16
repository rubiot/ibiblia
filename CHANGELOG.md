# Changelog — iBiblia v1.1.0 (64-bit Edition) (2026-11-14)

## Overview

**iBiblia v1.1.0 (64-bit Edition)** upgrades [iBiblia](https://github.com/rubiot/ibiblia) ([SalitaNgDiyos Fork](https://github.com/SalitaNgDiyos)) to a native 64-bit Windows (`x86_64-win64`) build with full **Dark Mode / Sepia theming**, enhanced **Strong's & Morphology Dictionary Popup** responsiveness and layout persistence, synchronized **Destination Status & Bible Tree Roll-Up**, and 64-bit IPC synchronization with [theWord](https://www.thewordbooks.com/index.php/download-tools-and-utilities/) and **Timotheos (Tanglaw)**.

## What's New & Fixed in v1.1.0

### 0. Version 1.1.0.024 (2026-11-16) — Paragraph Tags in the Verse Panels

* **Paragraph Tags in the Source, Destination, Reference 1 and Reference 2 Panels (`verse_visual_adapter.pas`, `verse_visual_document.pas`, `directwrite_syntagm_view.pas`)**:
  * `<CM>` and `<CI>` now break the line; `<CM>` also leaves a gap above the next paragraph. Both show a dim marker like `<CL>` does (pilcrow for `<CM>`, reversed pilcrow for `<CI>`).
  * `<PI>` / `<PI1>`..`<PI7>` indent the whole paragraph and `<PF>` / `<PF1>`..`<PF7>` its first line (`<PI0>`, `<Pi>` and `<PF0>` reset); level 1 is 40 px, +20 px per level. Indents end at the next line break.
  * The indents are drawn by the text layout used on Linux. The Windows DirectWrite view receives the same line breaks, but its indents are not drawn yet (not testable from the Linux build environment).

### 0c. Version 1.1.0.023 (2026-11-16) — theWord Paragraph Tags in the Chapter Panel & Folder Info in About

* **theWord Paragraph Tags in the Chapter Panel (`chapterview.pas`)**: The chapter panel now renders the theWord paragraph formatting tags, in Paragraph mode:
  * `<CM>` starts a spaced paragraph, `<CI>` a tight paragraph and `<CL>` a new line inside the same paragraph.
  * `<PI>` / `<PI1>`..`<PI7>` set the left indent (level 1 = 40 px, +20 px per level); `<PI0>` and `<Pi>` reset it.
  * `<PF>` / `<PF1>`..`<PF7>` set the first-line indent (`<PF0>` = none). With no `<PF>` a paragraph keeps the usual first-line indent, unless it is indented with `<PI>` or follows `<CL>`.
  * `<PI>`/`<PF>` apply to the paragraph being built and reset at the next paragraph boundary, as the theWord rules say; the order boundary, `<PI>`, `<PF>`, text is honoured.
  * Tags at the very start of a verse are applied before the verse number, so the number starts the new paragraph.
  * Out-of-contract spellings (`<PI8>`, `<Pf>`) are ignored. With Paragraph mode off the tags are ignored, as before.
* **Program and Working Folder in About (`unitabout.pas`, `formprincipal.pas`)**: The About box lists the program folder and the working (settings) folder, the place `iBiblia.ini` is kept (the program folder, or `$XDG_CONFIG_HOME/iBiblia` / `~/.config/iBiblia` on Linux when the program folder is read-only). Long paths are shortened in the middle, the hint shows the full path, and a click opens the folder. When the settings are kept beside the program, a single "Program and working folder" line is shown instead of two.

### 0b. Version 1.1.0.021 (2026-11-15) — Dialog Skin Completed on First Open

* **Dialog Controls Skinned on First Open (`formprincipal.pas`)**: In 1.1.0.020 a dialog such as **Export project** got its dark background on first open, but its tab pages, check boxes and buttons stayed light. The dialog becomes the active form before its window handles exist, and the dark-mode subclass hooks (`HookButtonControl`, `HookCheckBoxControl`, `HookTabControl`, `HookFormControl`) need those handles. `SkinFormOnce` now sets the colours at once, queues `DeferredSkinForm` (`Application.QueueAsyncCall`) to apply the full skin once the dialog is visible, creates the handles of controls on tab pages not yet shown (`EnsureChildHandles`), and only then records the form as skinned.

### 0a. Version 1.1.0.020 (2026-11-15) — Go to Verse Lookup & Skin Fixes

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