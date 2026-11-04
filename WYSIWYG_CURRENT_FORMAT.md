# Visual editing with the current .bib project format

The normal iBiblia workspace includes a DirectWrite visual verse editor on Windows. It uses the existing `.bib` project persistence and theWord import/export workflow. The public user guides are maintained in the [English](https://github.com/rubiot/ibiblia/wiki/User-manual-(English)) and [Portuguese](https://github.com/rubiot/ibiblia/wiki/Manual-de-utiliza%C3%A7%C3%A3o-(Portugu%C3%AAs)) wiki manuals.

## Implemented behavior

- Visual verse text editing, Unicode selection/copy/paste, undo/redo and bidirectional rendering.
- theWord semantic marks, HTML inline formatting, relative font sizes and colors.
- Title insertion and selection-based title formatting, level changes and removal.
- Rich translator-note editing, automatic/custom markers and note previews in editing and Association mode.
- Paragraph and soft-line structures represented by theWord markup.
- Raw ONT editing with tag highlighting/completion and a lossless codec for recognized and opaque objects.
- Edit provenance and reconciliation that preserve identifiable unaffected Syntagms and Associations. Changes removing associated members require review/confirmation.
- Deliberate lexical corrections in Raw mode; protected visual objects remain guarded during a visual editing cycle.

Use Save/Ctrl+S to apply an active edit. Cancel/Esc discards it. Literal record newlines are invalid; visual paragraphs and soft-line breaks serialize as markup.

## Compatibility boundary

The release keeps `.bib` projects and the established Verse/Syntagm/Association workflow. New project formats or experimental alternative workspaces are outside this release. Association preservation does not make arbitrary text replacement lossless: review affected Verse Pairs after lexical/structural edits, tokenizer changes or imports. Save a project copy before a large replacement operation.

This document describes implemented source behavior; it is not a substitute for the exact release build, regression suite and packaged-application checks recorded by the release process.
