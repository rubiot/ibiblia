# iBiblia v1.1.0 (64-bit Edition) — Changelog

## Release Date: 2026-11-14

### What's New & Fixed in v1.1.0
- **Dictionary Popup Size Persistence Across Restarts (`FormDictionaryPopup.pas`, `formprincipal.pas`)**:
  - Restored full persistence of user-resized dictionary popup dimensions (`popup.largura`, `popup.altura`) and morphology splitter height (`popup.morfo.altura` / `popup.panel2.altura`) across application restarts in both floating and right-edge docked modes.
  - Added `LoadLayoutFromOpts` synchronization after form construction and in `TMainForm.FormShow` so DPI auto-scaling or form initialization order never overrides saved dimensions, and removed minimum-size reset overrides (`<= 196` / `<= 180`).
  - Removed the broken `FInSizeMove` / `IsPointerInteracting` gate on `DoOnResize`, `SaveLayoutToOpts`, and `TabControlMorphologiesResize`, and added layout saving in `HidePopup` prior to hiding.
- **Destination Status Radio Strip & Bible Tree Roll-Up Synchronization (`formprincipal.pas`, `Project.pas`)**:
  - Synchronized the 4 status radio circles on the Destination panel (`TVersePanelStrip`) with `CurrentProject.Status` and `RadioGroupStatus` across word associations, disassociations, keyboard shortcuts (`1`–`9`), and verse navigation.
  - Added automatic transition to `Associating...` on partial association, `Associated!` (green) once all words in a verse are associated (deferring until `Ctrl`/`Shift` release during multi-word association), and `Not associated` when all associations are cleared.
  - Added `RecalculateTreeParentStatuses` to keep the current verse node, parent chapter node, and parent book node in `TreeView1` (both `ImageIndex` and `SelectedIndex`) continuously synchronized, showing green (`Associated!`) whenever all child verses/chapters are green.
- **Instant Dictionary Popup Refresh on Hover & Scroll (`Project.pas`, `Verse.pas`, `FormDictionaryPopup.pas`)**:
  - Updated `TProject.SintagmaOnMouseEnter` and `OnShowPopupTimer` so moving the mouse directly from one tagged word to another in the Source (`tbOrigem`) or Destination (`tbDestino`) panel refreshes the popup dictionary immediately.
  - Added mouse-wheel word-selection popup trigger in `TVerse.OnMouseWheel` so scrolling through Source words updates the popup entry immediately.
  - Restricted Destination-to-Source paired word dictionary lookup strictly to `tbDestino` (excluding Reference 1 and Reference 2).
- **Smooth Mouse-Wheel Scrolling & Zoom Inside Dictionary Popup (`FormDictionaryPopup.pas`, `Verse.pas`)**:
  - Added `RouteMouseWheelAtCursor` and `WH_GETMESSAGE` mouse-wheel hook so scrolling the wheel while hovering inside `FrmDictionaryPopup` scrolls the Strong's or Morphology pane under the cursor (and `Ctrl`+Wheel zooms font size).
- **Clean Hover Feedback (`Syntagm.pas`)**:
  - Updated `TSyntagm.ToggleStrongHighlight` to underline equivalent Strong's number tags on mouse hover without background block highlighting, while preserving Destination-to-Source paired word highlight.
