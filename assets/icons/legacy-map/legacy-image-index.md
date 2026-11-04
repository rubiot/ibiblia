# Icon migration inventory

Updated for the active colored SVG family. The previous inventory used action
names from before the Portuguese-to-English API migration and described inactive
proposals. `../manifest/icons.json` now records the actual Pascal action names,
stable `TAppIcon` names, resource prefixes and source SVG paths.

## Active image channels

| Surface | Resource / API | Index contract |
| --- | --- | --- |
| Main toolbar | Full `ui_icons` image list, logical 24 px | `AppIconIndex(TAppIcon)` |
| Main menus | Full `ui_icons` image list, logical 16 px | Same symbolic index as toolbar |
| Native dialog buttons | `LoadAppIcon` into button glyph | No numeric index; LCL disabled rendering |
| Application / forms | New multi-resolution ICO, embedded as `IBIBLIA_APP_ICO` | Windows `MAINICON` also points at root ICO |
| Main project tree | Runtime status images from project catalog | Persisted status ID; aggregate indices appended |
| Selectable Bible tree | `AddAppIcon` in checkbox / job-state order | Stable `StateIndex` 0..6 |
| Content panels / tabs | Named document, book, notes and review symbols | `TAppIcon` when assigned |

## Legacy toolbar migration

The historical serialized `ImageList1` positions are recorded here only to
explain old LFM files. Active code must use symbolic indices.

| Old index | Current action | Current icon |
| --- | --- | --- |
| 0 | `ActionOpenProject` | `aiProjectOpen` |
| 1 | `ActionCloseProject` | `aiProjectClose` |
| 2 | `ActionSaveProject` | `aiProjectSave` |
| 3 | `ActionSaveProjectAs` | `aiProjectSaveAs` |
| 4 | `ActionSuggestAssociation` | `aiAssociationAuto` |
| 5 | `ActionClearAssociations` | `aiAssociationClear` |
| 6 | `ActionNewProject` | `aiProjectNew` |
| 7 | `ActionHistoryBack` / `ActionPreviousVerse` | `aiHistoryBack` / `aiVersePrevious` |
| 8 | `ActionHistoryForward` / `ActionNextVerse` | `aiHistoryForward` / `aiVerseNext` |
| 9 | `ActionRollbackCurrentVerse` | `aiAssociationRollback` |
| 10 | `ActionExit` | `aiAppExit` |

All visible main actions, including first/last verse, export, merge, patch,
preferences, AI options, maintenance, shortcuts, layout and file registration,
have entries in the current manifest. `ActionNewVerse` and the project properties
`ActionChangeTab` are internal hooks, so neither receives an icon.

## Stable Bible tree state indices

These states describe selection and AI jobs, independently of the project status
catalog. Never copy their meanings into persisted project status numbers.

| StateIndex | Symbol |
| --- | --- |
| 0 | `aiCheckboxUnchecked` |
| 1 | `aiCheckboxChecked` |
| 2 | `aiCheckboxPartial` |
| 3 | `aiAiPending` |
| 4 | `aiAiProcessing` |
| 5 | `aiAiSuccess` |
| 6 | `aiAiError` |

## Retained legacy material

The `imagens/` folder and existing generated `.lrs` resources may remain for
historical and unrelated uses. They are not sources for the active icon family.
The new family lives entirely under `assets/icons`; its embedded resources are
loaded by `ui_icons.pas`. Do not remove an old image merely because a similar
new symbol exists without checking other consumers.
