# iBiblia Icon Styleguide

This guide defines the target visual identity for the next icon set. It is
documentation only; no Pascal code consumes this folder yet.

## Goals

- Make toolbar actions readable at 32x32 and menu/tree states readable at 16x16.
- Use domain-specific metaphors: Bible text, verse pairs, syntagms, association,
  review, export, patches, and AI-assisted processing.
- Replace generic folder/document metaphors where the action is more specific.
- Keep legacy ImageIndex information available during migration, but treat
  symbolic names as the future source of truth.

## Master Format

- Keep source artwork as SVG.
- Use a square SVG viewBox, preferably 24x24 for stroke icons or 32x32 for
  richer app/toolbar icons.
- Export PNG with alpha transparency for Lazarus consumption.
- Export ICO only for application icons.
- Do not put readable text inside icons. Avoid tiny labels such as "W", "AI",
  or ".bib" unless they remain recognizable at 16px.

## Export Sizes

| Context | Required sizes |
| --- | --- |
| App icon | 16, 32, 48, 64, 128, 256; packed as `.ico` |
| Toolbar | 32, plus 16 and 24 for menu/reuse |
| Main menu | 16 and 24 |
| Tree/status | 16 |
| Tab/panel | 16 and 24 |
| Documentation/preview | 64 |

## Visual Language

- Stroke: 2px at 24x24, scaled proportionally.
- Corners: small radius for documents, panels, and chips.
- Shapes: prefer simple silhouettes and one strong gesture per icon.
- Fill: mostly transparent/outline, with small filled accents for status.
- Contrast: icons must survive light and dark Windows themes.
- Disabled state: generate from runtime theme if possible; if pre-rendered, use
  neutral gray and lowered contrast.

## Palette

| Role | Color |
| --- | --- |
| Primary/action blue | `#2563EB` |
| Text/manuscript graphite | `#1F2937` |
| Association/success green | `#16A34A` |
| Warning/review amber | `#F59E0B` |
| Error red | `#DC2626` |
| Neutral gray | `#9CA3AF` |
| Surface/light fill | `#F8FAFC` |

Status icons should preserve the project status catalog color as the main cue,
but add shape cues so color is not the only signal.

## Naming

Use stable symbolic names in kebab case:

- `project-open`
- `association-auto`
- `association-rollback`
- `status-needs-review`
- `ai-processing`
- `tab-source`

Future generated files should use this layout:

```text
assets/icons/
  manifest/icons.json
  source/svg/<symbol>.svg
  png/16/<symbol>.png
  png/24/<symbol>.png
  png/32/<symbol>.png
  png/48/<symbol>.png
  png/64/<symbol>.png
  app/ico/iBiblia.ico
  legacy-map/legacy-image-index.md
```

## Category Guidance

### Project Actions

Use a Bible/document base with action overlays:

- Open: document or Bible with inward arrow.
- Close: document with x mark.
- Save: document/disk with check mark.
- Save as: duplicated document with pencil.
- Settings: document with small gear.
- Export: document with outward arrow.
- Patch create/apply: document plus puzzle-piece/patch shape.

### Association Actions

Represent source and destination text as two short columns or two word blocks.

- Auto-associate: two text columns connected with a spark or wand accent.
- Clear: broken link plus eraser.
- Rollback: connected text with circular back arrow.
- Rebuild memory: database/cylinder plus circular arrow.

### Navigation

Use simple arrows:

- Previous/next: left/right arrow.
- First/last: arrow plus vertical bar.

### Status And Tree Icons

At 16x16, keep status icons highly geometric:

- Not associated: empty circle or red outlined stop.
- Associating/translating: partial circle or spinner.
- Needs review/ready for review: warning diamond or eye/check.
- Associated/reviewed: check or double check.
- Mixed/aggregate: segmented square with a thin border.

### AI Processing

Use one consistent motif, preferably a small chip/spark plus state overlay:

- Pending: clock.
- Processing: spinner.
- Success: check.
- Error: x.

## Migration Notes

- The current toolbar uses `ImageList1` in `formprincipal.lfm`; symbolic names
  should eventually replace positional ImageIndex usage.
- The current main menu has no image list, so Action ImageIndex values do not
  reliably produce visible menu icons.
- The main project tree currently generates status bitmaps in code. A future
  migration can either keep runtime generation with better shapes or load
  symbolic 16px assets and recolor accents from the status catalog.
- `TBibleTreeView` uses StateImages, not node ImageIndex, for checkboxes and AI
  states. Keep this separate from the main project tree status ImageIndex map.
