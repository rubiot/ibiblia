# WYSIWYG on the current project format

This branch deliberately keeps the existing `.bib` project schema and the
normal iBiblia workspace. Its scope is limited to validating DirectWrite-based
semantic WYSIWYG editing of verse text.

## Included

- The normal project tree, chapter navigation, interlinear tools, translation
  memory, comments, statuses, and Association workflow.
- DirectWrite rendering and character selection in the existing verse editor.
- Visual application/removal of theWord `FI/Fi`, `FR/Fr`, `FO/Fo`, and `FU/Fu`
  semantics.
- A raw ONT fallback backed by the same lossless codec.
- Unicode clipboard, undo/redo, and protected opaque/lexical regions.
- Existing `.bib` persistence and ONT import/export.

## Deliberately excluded

- The `.ibiblia` schema and repository.
- Legacy project conversion.
- UUID-based Syntagms, normalized Associations, and persistent revisions.
- The experimental multi-layer v2 form.

Textual or structural edits to a verse that already has Associations remain
blocked in this validation branch. Semantic-only edits are allowed and retain
the existing Association objects. This prevents silent Association loss while
the current-format WYSIWYG interaction is evaluated.

The preserved project-v2 work continues independently in
`C:\Code\ibiblia-project-v2` on branch `codex/project-v2`.
