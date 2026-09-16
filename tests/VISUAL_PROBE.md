# Native visual verification

Run `./tests/Run-VisualProbe.ps1` from the repository in PowerShell. It builds a
separate Lazarus executable with Release-level O3 optimization and captures Portuguese and English at 96, 144 and
192 PPI. For a quick pass, use `-Language pt -Dpi 96`. Use `-SkipBuild` only when
the probe has already been rebuilt after the latest Pascal, LFM or icon changes.

Each run creates a fresh directory below `.codex-visual-work/`, with its own
executable, INI file, libraries, language catalogs, synthetic Bible project and
synthetic patch. Existing output directories are preserved. Synchronization,
autosave and automatic association suggestions are disabled in the isolated INI;
no LLM request is made. Form errors are logged and terminate the probe without
an error dialog.

The probe uses real application constructors and form resources. It captures
the main window, narrow layout, dialog pages, AI tree states, custom status
catalog, patch remapping, note editors and previews. Modal operations are
cancelled after capture. The synthetic source, destination and reference texts
exercise Greek, Portuguese and Hebrew RTL with separately configured fonts.

`summary.csv` reports each language/PPI run. Each `captures/report.tsv` contains:

- `PASS`: assertions for twelve custom states, exact colors (including black
  and magenta), transparent marker padding, native image resolutions, preserved
  reading fonts and cancelled patch mapping. The status-filter regression checks
  click/Space toggling, selection by stable ID after reordering and scroll
  preservation. Reused dialogs are checked across Portuguese/English/Portuguese
  language changes.
- `CAPTURE`: the corresponding PNG and actual form dimensions/PPI.
- `REVIEW_BOUNDS` or `REVIEW_TEXT`: controls or fixed single-line labels needing
  visual review. Intentional scrolling containers are excluded.
- `DRAW_BEFORE`/`DRAW_AFTER`: a bounded trace for the owner-drawn status review
  list, useful for detecting canvas state leakage.
- `COMPLETE`: all requested surfaces were captured without a runtime error.

Scaling uses Lazarus's real `AutoAdjustLayout` path. Captures use native
`GetFormImage`/`PaintTo` on offscreen windows, so they complement live checks of
window frames, popup menus, keyboard focus and transitions between physical
monitors. Layout checks do not replace inspection of the PNGs.

The status review painter was also checked in the standalone Release executable.
An earlier version could paint the entire native list with a swatch color despite
correct offscreen captures. Explicit coordinate calculations and isolated canvas
state resolved that observed behavior; the exact underlying cause was not
established. Keep the live owner-draw check when changing that painter.
