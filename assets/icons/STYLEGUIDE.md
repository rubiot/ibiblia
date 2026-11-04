# iBiblia visual identity

The default command icon family is original SVG artwork built around **connected Bible
texts**. Filled silhouettes, gently rounded corners, pale page surfaces and
subtle two-stop shading give toolbar, menu and workflow icons one visual voice.
The Fluent Color collection is a visual reference, not an imported dependency;
no third-party icon files are bundled in this family.

The application offers four command icon families: **Textos conectados**
(default), **Clareza**, **Mosaico** and **Fólio**. The family choice changes
command artwork; the interface palette, typography and project status colors
remain consistent. Each alternative has the same 67 command symbols and seven
PNG resolutions as the default family.

The **application mark is unique**: the adopted `app-ibiblia` drawing from
**Clareza** is an open book with blue outlines, white pages and a gold link.
Its canonical source is `brand/app-ibiblia.svg`. This same mark is used in
every family, including the title bar, About window and Windows application
icon; changing the command family never changes the mark. The other
`app-ibiblia` drawings in the design galleries remain historical studies and
are excluded from selectable resources.

The command family choice is applied immediately through Preferences and persisted in the
application INI under `[opcoes] ui.icon.family`, using the stable IDs
`connected`, `clareza`, `mosaico` and `folio`. Missing or unknown IDs select
the default family. Only image slots registered through `AddAppIcon` are
refreshed; custom project status markers are not registered.

## Shape and meaning

- The application mark uses Clareza's rounded blue book outlines and a gold
  link joining the white pages. Keep these contours consistent across sizes.
- Project commands use familiar folders, documents and save disks. A compact
  action badge distinguishes new, close, settings and export.
- Association commands show source and destination syntagms as two sets of
  text blocks. Connecting strokes, a broken connection and a return arrow
  distinguish suggestion, removal and rollback.
- History uses slate curved arrows; previous/next Bible verses use blue pages
  and straight arrows. These are distinct operations with distinct artwork.
- The theWord integration uses a book and two gold synchronization arrows.
  This is an iBiblia integration symbol, not a replacement theWord logo.
- AI job states use a clock, progress ring, check and cross. These meanings
  belong to the processing workflow; they are not assigned to project status IDs.
- Standard confirmation, cancellation, editing and browsing controls reuse the
  same palette and geometry throughout the native dialogs.

## Palette and interface

| Role | Color |
| --- | --- |
| Primary action blue | `#2563EB` |
| Application mark link | `#B8811E` |
| Supporting gold (default command family) | `#D49A28` |
| Interface text graphite | `#272F37` |
| Neutral slate | `#64748B` |
| Reading surface | `#FFFFFF` |
| Interface surface | `#F3F6F8` |
| Success / destination teal | `#138878` |
| Destructive / processing error red | `#D94343` |
| AI configuration / patch purple | `#8056C7` |

Interface typography is Segoe UI, 10 points, scaled by Windows DPI. Bible text,
notes and specialized editors retain the user's configured content fonts.
Use 8-pixel spacing units and 16-pixel dialog margins at 96 DPI. Colored command
icons sit on quiet light surfaces; white reading areas remain visually primary.
Native focus, keyboard behavior and disabled-state rendering remain available.

`ui_style.pas` centralizes the interface palette, spacing and native control
roles. Use `ApplyFormStyle` for a window shell and `StyleControl` for explicit
surface, content, title, hint, footer and button roles. Dialogs call
`ApplyDialogStyle` after construction; it only visits native interface chrome,
leaving verse controls, RichMemo/KMemo, SynEdit and other content views intact.
`SetButtonIcon` assigns a small multi-resolution image list owned by each button,
so DPI changes select native source pixels. Form layouts load `.lfm` directly;
do not edit the old generated `.lrs` copies.

## Source and runtime resources

Each icon has a 32 by 32 SVG viewBox, alpha transparency, approximately two-pixel
strokes where needed and at least one pixel of breathing room. Avoid fine text,
hairlines, filters and detail that disappears at 16 pixels. The SVG sources and
reproducible generator are checked in alongside the rendered assets.

| Output | Sizes |
| --- | --- |
| All PNG symbols | 16, 20, 24, 32, 40, 48, 64 |
| Application PNG / ICO | All above, plus 128 and 256 |
| Toolbar logical size | 32 |
| Menu / tree logical size | 16 |

`ui_icons.pas` exposes `TAppIcon`, `AppIconIndex`, `AppIconSymbol`,
`LoadAppIcon`, `LoadApplicationIcon`, `AddAppIcon` and `PopulateAppImageList`.
Full image lists use `Ord(TAppIcon)`; subset lists use the index returned by
`AddAppIcon`. `PopulateAppImageList` registers all resolutions and enables LCL
scaling. `AddAppIcon` registers all resolutions but leaves the caller's logical
size and `Scaled` setting intact. `LoadAppIcon` chooses the smallest available
source at least as large as requested, or the largest available source.

The application mark is maintained in `brand/app-ibiblia.svg`; the generated
`source/svg/app-ibiblia.svg`, PNGs and ICO all derive from that canonical SVG.
Do not edit a generated copy or choose a family-specific mark at runtime.

The default PNG and ICO resources are embedded in `generated/ui_icons.res`;
the three additional command families are embedded in
`generated/ui_icon_families.res`, both through `ui_icons.pas`. Alternative
resource names use `IBIBLIA_<FAMILY>_<SYMBOL>_<SIZE>` with uppercase names and
underscores, for example `IBIBLIA_CLAREZA_PROJECT_OPEN_16`. Application mark
resources always come from `ui_icons.res` and have no family prefix.
The application runs without loose PNG or SVG files. The Windows
`MAINICON` resource also uses the new root `iBiblia.ico`. Existing `.lrs` files
are not regenerated or edited. ICO frames use uncompressed 32-bit BGRA DIBs
with AND masks at every size; PNG payloads in small ICO entries are not
compatible with Lazarus TIcon. Validate both LoadFromFile and the embedded
LoadApplicationIcon path after regeneration.

## Project status compatibility

Do not add a fixed `status-not-associated`, `status-reviewed` or similar symbol
based on a numeric status ID. Project catalogs own their names, colors and
number of entries. Runtime status images remain generated from those catalogs;
book and chapter indicators retain proportional colored segments. Selection
checkboxes and AI processing states use the separate `TBibleTreeView.StateImages`
channel, with stable indices 0 through 6.

## Regeneration

Requires Node.js, the `sharp` package and the Lazarus/FPC Windows resource
compiler. No network calls or image-generation service are used.

```powershell
node assets/icons/scripts/generate-icons.cjs --windres C:/Apps/lazarus/fpc/3.2.2/bin/x86_64-win64/windres.exe
```

If `sharp` is installed outside normal Node package resolution, append
`--sharp-module C:/path/to/node_modules/sharp`. The script explicitly uses
`cpp.exe` next to `windres.exe`, so it does not depend on a global compiler PATH.
It reads the application mark from `brand/app-ibiblia.svg` and regenerates the
source SVG copies, PNGs, application ICO, manifest, Pascal enum/name includes,
RC/RES and contact sheet. Normal application builds consume the
checked-in RES and do not require Node or sharp.

The selectable alternatives have their own SVG/PNG generator and resource
packaging step. Their resource builder requires Node.js and `windres`, but
does not require `sharp` when the rendered PNG files already exist:

```powershell
node assets/icons/alternatives/generate.cjs --sharp-module C:/path/to/node_modules/sharp
node assets/icons/scripts/build-icon-families.cjs --windres C:/Apps/lazarus/fpc/3.2.2/bin/x86_64-win64/windres.exe
```

`build-icon-families.cjs` validates the unique symbolic catalog and every
required PNG's signature, dimensions and alpha-channel format before writing
the RC file. It packages 1,407 resources (3 families × 67 commands × 7 sizes),
and replaces the compiled RES only after a successful resource compilation.
It does not rewrite the default icon family, the manifest or either application
ICO. Omitting `--windres` validates the PNGs and generates only the RC file.
Review `preview/contact-sheet.png` at its actual size and the live application
at 100%, 150% and 200% before accepting icon changes. The sheet shows every
symbol at 16, 24 and 48 pixels. Validate transparency, disabled rendering,
small-size recognizability and distinct adjacent commands.

The authoritative symbol/action inventory is `manifest/icons.json`; migration
history and the separate runtime image channels are described in
`legacy-map/legacy-image-index.md`.
