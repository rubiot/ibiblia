# Changelog — iBiblia v1.1.0 (64-bit Edition) (2026-11-14)

## Overview

**iBiblia v1.1.0 (64-bit Edition)** upgrades [iBiblia](https://github.com/rubiot/ibiblia) ([SalitaNgDiyos Fork](https://github.com/SalitaNgDiyos)) to a native 64-bit Windows (`x86_64-win64`) build with full **Dark Mode / Sepia theming**, enhanced **Strong's & Morphology Dictionary Popup** responsiveness and layout persistence, synchronized **Destination Status & Bible Tree Roll-Up**, and 64-bit IPC synchronization with [theWord](https://www.thewordbooks.com/index.php/download-tools-and-utilities/) and **Timotheos (Tanglaw)**.

## What's New & Fixed in v1.1.0

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