# Legacy Icon Inventory

This document records the current Lazarus icon wiring before any visual
identity migration. It is intentionally descriptive: no Pascal code has been
changed to consume the new manifest yet.

## Files And Mechanisms

| Resource | Location | Mechanism | Current size/count | Notes |
| --- | --- | --- | --- | --- |
| App icon | `iBiblia.ico` | Windows ICO referenced by `iBiblia.rc`; `iBiblia.lpr` links `*.res` | 16, 32, 48, 64, 128, 256 | Also mirrored in `iBiblia.lrs` as `MAINICON` |
| Toolbar icons | `formprincipal.lfm` / `ImageList1` | Lazarus `TImageList.Bitmap` serialized in LFM | 24 sprites, 32x32 | Toolbar uses only indices 0..10 today |
| Legacy 64px icons | `formprincipal.lfm` / `ImageList64x64` | Lazarus `TImageList.Bitmap` serialized in LFM | 7 sprites, 64x64 | No current binding found |
| Legacy tree icons | `formprincipal.lfm` / `ImageListTreeView` | Lazarus `TImageList.Bitmap` serialized in LFM | 10 sprites, 16x16 | Assigned at design time to `TreeView1.Images`, but replaced by runtime status images after a project opens |
| Project tree status icons | `Project.pas` / `TProject.RefreshStatusImages` | Runtime `TImageList` of generated `TBitmap` squares | 16x16 | `Node.ImageIndex` equals status id; aggregate icons are appended dynamically |
| Bible selection tree icons | `bibletreeview.lrs` and `bibletreeview.pas` | PNG resources loaded with `LoadFromLazarusResource`; additional AI states drawn as colored squares | 16x16 | Uses `StateImages` and `StateIndex`, not `ImageIndex` |
| Source PNG candidates | `imagens/` | Loose PNG files | 16, 32, 64 and screenshots | Mostly source/candidate artwork; not loaded directly by current UI code |

No SVG, Qt `.qrc`, loose BMP, XPM, or CUR resources were found in the active
project tree.

## Toolbar

`ToolBar1` is declared in `formprincipal.lfm`.

- `ToolBar1.Images = ImageList1`
- `ButtonHeight = 34`
- `ButtonWidth = 34`
- Most `TToolButton`s take their icon from the assigned `TAction.ImageIndex`
- `ToolButtonExit` sets `ImageIndex = 10` directly and calls `ToolButtonExitClick`

| Tool button | Action or handler | Current caption/hint source | ImageList1 index | Proposed symbol |
| --- | --- | --- | --- | --- |
| `ToolButton1` | `ActionAbrirProjeto` | Open project | 0 | `project-open` |
| `ToolButton2` | `ActionFecharProjeto` | Close project | 1 | `project-close` |
| `ToolButton3` | `ActionNovoProjeto` | New project | 6 | `project-new` |
| `ToolButton4` | `ActionSalvarProjeto` | Save | 2 | `project-save` |
| `ToolButton5` | `ActionSalvarProjetoComo` | Save as | 3 | `project-save-as` |
| `ToolButton6` | `ActionSugerirAssociacao` | Auto-associate | 4 | `association-auto` |
| `ToolButton7` | `ActionLimparAssociacoes` | Clear associations | 5 | `association-clear` |
| `ToolButton8` | `ActionVersoAnterior` | Previous verse | 7 | `verse-previous` |
| `ToolButton9` | `ActionVersoSeguinte` | Next verse | 8 | `verse-next` |
| `ToolButton10` | `ActionReverterAssociacoes` | Rollback | 9 | `association-rollback` |
| `ToolButtonExit` | `ToolButtonExitClick` / same semantic as `ActionSair` | Exit | 10 | `app-exit` |

## ImageList1, 32x32

`ImageList1` contains 24 sprites. The current UI uses 0..10. The loose PNG
match is based on comparing embedded sprites to files under `imagens/32x32`.

| Index | Used today | Pascal action | Closest loose PNG / current look | Proposed symbol |
| --- | --- | --- | --- | --- |
| 0 | Yes | `ActionAbrirProjeto` | `imagens/32x32/My-Documents-icon.png` | `project-open` |
| 1 | Yes | `ActionFecharProjeto` | `imagens/32x32/Folder-icon2.png` | `project-close` |
| 2 | Yes | `ActionSalvarProjeto` | `imagens/32x32/Edit-Document-icon.png` | `project-save` |
| 3 | Yes | `ActionSalvarProjetoComo` | `imagens/32x32/Document-Copy-icon.png` | `project-save-as` |
| 4 | Yes | `ActionSugerirAssociacao` | `imagens/32x32/Document-Help-icon.png` | `association-auto` |
| 5 | Yes | `ActionLimparAssociacoes` | `imagens/32x32/Eraser-icon.png` | `association-clear` |
| 6 | Yes | `ActionNovoProjeto` | `imagens/32x32/Document-Blank-icon2.png` | `project-new` |
| 7 | Yes | `ActionVersoAnterior` | `imagens/32x32/Actions-arrow-left-icon.png` | `verse-previous` |
| 8 | Yes | `ActionVersoSeguinte` | `imagens/32x32/Actions-arrow-right-icon.png` | `verse-next` |
| 9 | Yes | `ActionReverterAssociacoes` | `imagens/32x32/File-Open-icon.png` | `association-rollback` |
| 10 | Yes | `ActionSair` / `ToolButtonExit` | `imagens/32x32/Actions-application-exit-icon.png` | `app-exit` |
| 11 | No | none found | `imagens/32x32/Files-icon.png` | candidate spare |
| 12 | No | none found | `imagens/32x32/Actions-help-hint-icon.png` | candidate spare |
| 13 | No | none found | `imagens/32x32/Folder-Compressed-icon.png` | candidate spare |
| 14 | No | none found | `imagens/32x32/Document-Gant-Chart-icon.png` | candidate spare |
| 15 | No | none found | `imagens/32x32/Document-Preview-icon.png` | candidate spare |
| 16 | No | none found | `imagens/32x32/Document-icon2.png` | candidate spare |
| 17 | No | none found | `imagens/32x32/File-Delete-icon.png` | candidate spare |
| 18 | No | none found | `imagens/32x32/File-History-icon.png` | candidate spare |
| 19 | No | none found | `imagens/32x32/File-New-icon.png` | candidate spare |
| 20 | No | none found | `imagens/32x32/Folder-Add-icon.png` | candidate spare |
| 21 | No | none found | `imagens/32x32/Folder-Delete-icon.png` | candidate spare |
| 22 | No | none found | `imagens/32x32/Folder-Explorer-icon.png` | candidate spare |
| 23 | No | none found | `imagens/32x32/Folder-Open-icon.png` | candidate spare |

## Main Form Actions

`formprincipal.lfm` declares `ActionList1`. These are the current design-time
actions and their icon state.

| Pascal action | Current caption | Current ImageIndex | Visible contexts |
| --- | --- | --- | --- |
| `ActionNovoProjeto` | New... | 6 | toolbar, Project menu |
| `ActionSalvarProjetoComo` | Save as... | 3 | toolbar |
| `ActionAbrirProjeto` | Open... | 0 | toolbar, Project menu |
| `ActionFecharProjeto` | Close | 1 | toolbar, Project menu |
| `ActionPropProjeto` | Settings... | none | Project menu |
| `ActionSair` | Exit | 10 | Project menu; semantic match for exit toolbar button |
| `ActionVersoPrimeiro` | First | none | Navigation menu |
| `ActionVersoUltimo` | Last | none | Navigation menu |
| `ActionVersoSeguinte` | Next | 8 | toolbar, Navigation menu |
| `ActionVersoAnterior` | Previous | 7 | toolbar, Navigation menu |
| `ActionSugerirAssociacao` | Auto-associate | 4 | toolbar, Edit menu |
| `ActionQuandoNovoVersiculo` | ActionQuandoNovoVersiculo | none | internal |
| `ActionSalvarProjeto` | Save | 2 | toolbar, Project menu |
| `ActionReverterAssociacoes` | Rollback | 9 | toolbar, Edit menu |
| `ActionLimparAssociacoes` | Clear | 5 | toolbar, Edit menu |
| `ActionExportarDestinoComStrongs` | Export destination text | none | action exists; no visible menu binding found |
| `ActionExportarTextoInterlinear` | Export interlinear text | none | action exists; no visible menu binding found |
| `ActionExportar` | Export... | none | Project menu |
| `ActionMesclarProjetos` | Merge projects... | none | action exists; no visible menu binding found in LFM |
| `ActionRecriarBaseSugestoes` | Recreate association memory | none | Maintenance menu |
| `ActionSyncTheWordVerse` | ActionSyncTheWordVerse | none | action exists; no visible menu binding found |
| `ActionMostrarTags` | ActionMostrarTags | none | shortcut action |
| `ActionExportPatch` | Create patch... | none | Project menu |
| `ActionApplyPatch` | Apply patch... | none | Project menu |
| `ActionVaccumProject` | Vacuum project | none | Maintenance menu |

`formprincipal.pas` also creates these runtime actions:

| Pascal action | Current menu context | Current ImageIndex |
| --- | --- | --- |
| `ActionKeyboardShortcuts` | Help menu | none |
| `ActionResetLayout` | Options menu | none |
| `ActionGuideMode` | Options menu | none |
| `ActionTheWordHighlighterColors` | Options menu | none |
| `ActionLlmOptions` | Options menu | none |

The class declares `ActionRegisterFileAssociation` and
`ActionUnregisterFileAssociation`, but the current LFM menu items
`MenuItemRegisterFileAssoc` and `MenuItemUnregisterFileAssoc` are direct menu
items with captions and no `Action` binding.

## Project Properties Actions

`FormProjectProperties.lfm` declares a second `ActionList1`. None of these
actions has an ImageIndex or an ImageList.

| Pascal action | Current caption | Visible contexts |
| --- | --- | --- |
| `ActionLoadText` | Load new text | project properties button |
| `ActionClearText` | Clear text | project properties button |
| `ActionSelectStrongDictionary` | Choose | project properties button |
| `ActionSelectMorphologyDictionary` | Choose | project properties button |
| `ActionSelectFont` | Choose | project properties button |
| `ActionChangeTab` | ActionChangeTab | internal tab action |

## Menus

### MainMenu1

`MainMenu1` is assigned to `TFrmPrincipal.Menu`. It has these top-level menus:

- Project
- Edit
- Navigation
- Options
- Help

The main menu does not define an `Images` property, and the action list does
not provide a menu image list. Therefore action ImageIndex values are primarily
effective for the toolbar today, not for menu icons.

### Project Tree Context Menu

`TProject.CreateBibleTreeContextMenu` creates a `TPopupMenu` at runtime.

- `result.Images := FArvore.Images`
- Status submenu items use `ImageIndex := status`
- Status item `Tag` also stores the status id
- AI submenu items for association, translation, and cancel do not use ImageIndex

Because `FArvore.Images` is replaced by `FStatusImages`, the tree context menu
shows the same generated 16x16 status images as the project tree.

### Verse Popup Menus

`TVerse.InitSyntagmPopupMenu` creates menu items for:

- Copy tags
- Paste all tags
- Paste Strong tags
- Paste morphology tags

`TVerse.InitVersePopupMenu` creates menu items for:

- Right-to-left
- Save to file
- Debug-only copy/export helpers

`TProject.AttachProjectVersePopupMenu` appends AI flow example items to verse
popup menus:

- Use verse as example
- Use verse as association example
- Use verse as translation example
- Debug-only copy/switch helpers

These popup menus currently have no image list and no item icons.

### Chapter View Popup Menu

`TChapterView.InitPopupMenu` creates menu items for:

- Set font
- Paragraph mode
- Interlinear mode
- Interlinear
- Intralinear
- Verse rules
- Print chapter

No icon image list is assigned.

## Project Tree And Status Icons

The main side tree is `TreeView1` in `formprincipal.lfm`.

- Design time: `TreeView1.Images = ImageListTreeView`
- Runtime after opening a project: `TProject.RefreshStatusImages` sets
  `FArvore.Images := FStatusImages`
- `FStatusImages.Width := 16`
- `FStatusImages.Height := 16`
- For each status id from 0 to max id, the code draws a 16x16 colored square
- Verse nodes use `Node.ImageIndex := Status`
- Selected nodes use `Node.SelectedIndex := Status`
- Parent book/chapter nodes may receive aggregate indices appended after the
  base status images

Default legacy association statuses:

| Status id / ImageIndex | Default association caption | Default color source | Proposed symbol |
| --- | --- | --- | --- |
| 0 | Not associated | legacy red | `status-not-associated` |
| 1 | Associating | legacy yellow | `status-associating` |
| 2 | Needs review | legacy orange | `status-needs-review` |
| 3 | Associated! | legacy green | `status-associated` |

Translation projects can define five default statuses using the same status id
mechanism:

| Status id / ImageIndex | Default translation caption | Proposed symbol |
| --- | --- | --- |
| 0 | Awaiting translation | `status-awaiting-translation` |
| 1 | Translating | `status-translating` |
| 2 | Ready for review | `status-ready-for-review` |
| 3 | Has pending issues | `status-pending-issues` |
| 4 | Reviewed | `status-reviewed` |

## TBibleTreeView StateImages

`TBibleTreeView` is a custom tree view used for selectable verse lists in
export/patch/AI workflows. It uses `StateImages`, not the normal node
`Images/ImageIndex` channel.

| StateIndex | Symbol | Current implementation |
| --- | --- | --- |
| 0 | `checkbox-unchecked` | PNG resource `checkbox-unchecked` from `bibletreeview.lrs` |
| 1 | `checkbox-checked` | PNG resource `checkbox-checked` from `bibletreeview.lrs` |
| 2 | `checkbox-partial` | PNG resource `checkbox-partial` from `bibletreeview.lrs` |
| 3 | `ai-pending` | runtime colored square, `clYellow` |
| 4 | `ai-processing` | runtime colored square, `clAqua` |
| 5 | `ai-success` | runtime colored square, `clLime` |
| 6 | `ai-error` | runtime colored square, `clRed` |

`TFrmAIProcessing.OnJobStateChanged` maps job states to these AI StateIndex
values.

## Legacy ImageListTreeView, 16x16

This design-time image list remains in `formprincipal.lfm`, but normal project
operation replaces it with generated status images.

| Index | Current embedded image approximation |
| --- | --- |
| 0 | `imagens/bullet_grey.png` |
| 1 | `imagens/bullet_yellow.png` |
| 2 | `imagens/bullet_red.png` |
| 3 | `imagens/bullet_green.png` |
| 4 | `imagens/Button-Blank-Gray-icon.png` |
| 5 | `imagens/Button-Blank-Yellow-icon.png` |
| 6 | `imagens/Button-Blank-Red-icon.png` |
| 7 | `imagens/Button-Blank-Green-icon.png` |
| 8 | checkbox-like legacy slot |
| 9 | partial/checked checkbox-like legacy slot |

## Legacy ImageList64x64

This image list has no current UI binding found. It appears to contain larger
versions of older toolbar icons.

| Index | Closest loose PNG |
| --- | --- |
| 0 | `imagens/Abrir.png` |
| 1 | `imagens/Fechar.png` |
| 2 | `imagens/Salvar.png` |
| 3 | `imagens/Salvar Como.png` |
| 4 | `imagens/Sugerir.png` |
| 5 | `imagens/Limpar.png` |
| 6 | `imagens/Recarregar.png` |

## Tabs

Current tab controls use text only. No `Images` or tab `ImageIndex` use was
found.

| Form/control | Current tabs |
| --- | --- |
| `ChapterViewTabCtrl` in `formprincipal.lfm` | Source, Destination, Reference1, Reference2 |
| `FormProjectExport.PageControl1` | export option tabs |
| `FormInterlinearVerseRules.PageControlVerseRules` | interlinear/intralinear rule tabs |
| `formllmoptions.pcFlows` | Association, Translation |
| `FormDictionaryPopup.TabControlStrongs` / `TabControlMorphologies` | dictionary tabs |
| `FormApplyPatch.TabControlPreview` | patch preview tabs |
| `formaiprocessing.TabControlPreview` | AI processing preview tabs |
| `ONTMemo.PageControl1` | ONT, Preview |

## Recommended Migration Order

1. Generate SVG masters and PNG exports for the 11 currently visible toolbar
   symbols.
2. Generate 16px status and AI state icons, replacing color-only squares with
   shape-plus-color icons.
3. Add icons for visible menu gaps: settings, export, patch, LLM options,
   guide mode, reset layout, highlighter colors, maintenance actions.
4. Add project properties button icons.
5. Only after assets are ready, introduce a symbolic loader in Pascal and map
   symbols to actions without relying on raw positional ImageIndex values.
