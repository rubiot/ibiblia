# iBiblia Agent Guide

## Project Overview

iBiblia is a Lazarus/Free Pascal desktop application for building annotated Bible modules for theWord. The central workflow links syntagms in a source Bible text to syntagms in a translated destination text, then uses those associations to export Strong's numbers, morphology, interlinear views, patches, and related project data.

The project also includes LLM-assisted flows for translation and association. These flows are configured in `formllmoptions.pas`, executed through batch-processing infrastructure, and reviewed/applied through the AI processing UI.

## Core Concepts

Use the terminology in `CONTEXT.md` when discussing or changing association behavior:

- **Verse**: a source or translated Bible verse represented by `TVerse`.
- **Verse Pair**: linked source/destination verses.
- **Syntagm**: selectable text unit represented by `TSyntagm`.
- **Syntagm Selection**: the current selected syntagms in a verse.
- **Association**: persisted link between selected syntagms in a verse pair.

Avoid replacing these terms with generic alternatives such as token, row, mapping, or fragment unless the code being edited already uses that word for a different technical concept.

## Important Units

- `Project.pas`: `TProject`, the main project/session coordinator. It owns navigation, SQLite-backed persistence, verse views, context menus, batch operations, export integration, and AI batch entry points.
- `Verse.pas`: `TVerse`, the verse model and UI-facing text object. It parses verse text into syntagms, manages selections, creates/removes associations, and serializes association data.
- `syntagm.pas`: `TSyntagm` and `TSyntagmList`, selectable text units with sibling/pair relationships.
- `current_verse_workspace.pas`: coordinates the currently visible source/destination/reference verse views.
- `project_persistence.pas`, `project_config.pas`, `project_verse_repository.pas`: persistence/configuration helpers around the project database.
- `bible_tree_coordinator.pas`: tree/reference/status coordination.
- `ai_flows.pas`: common AI flow interfaces and `TAIFlowResult`.
- `ai_association_flow.pas`: association flow prompt, parsing, and application.
- `ai_translation_flow.pas`: translation flow prompt, parsing, and application.
- `ai_processing_manager.pas`: worker-thread based AI processing manager.
- `ai_batch_orchestrator.pas`, `ai_processing_batch.pas`, `ai_batch_work_item.pas`: batch preparation and application support.
- `formllmoptions.pas`: LLM options dialog, with dedicated Association and Translation configuration tabs.
- `formaiprocessing.pas`: AI batch review/progress form.
- `llmclient.pas`: provider/model catalog and LLM client creation.

## Architecture Notes

- The app is an LCL desktop application. Forms are `.lfm` files paired with `.pas` units. Prefer editing form layout in Pascal only when the existing form already creates controls dynamically.
- Project data is SQLite-backed and accessed through Lazarus/FPC database components and project helper classes, not an ORM.
- `TProject` is broad by design. Before adding responsibilities there, check whether an existing helper unit already owns the behavior.
- `TVerse.Texto` parsing and syntagm construction can be expensive. Avoid unnecessary reparsing inside loops or UI callbacks.
- Associations are built through syntagm selection and `TVerse.AssociarSintagmas`; keep selection replacement semantics consistent with `CONTEXT.md`.

## AI Flow Notes

- Implement AI behavior through `IAIVerseFlow` and, where configuration is needed, `IAIFlowConfig`.
- `BuildMessages` prepares LLM messages, `ParseResponse` converts raw output to a `TAIFlowResult`, and `ApplyResult` applies accepted in-memory verse state.
- Worker threads perform LLM calls. UI and verse-state application must happen on the main thread via manager/batch plumbing.
- Association responses are JSON with `data` pairs and optional `comments`; translation responses are plain translated text.
- Flow-specific settings use stable project keys such as `ai.flow.association.*` and `ai.flow.translation.*`. Preserve those keys unless a migration is explicitly required.

## Coding Conventions

- New code should use English names, but preserve existing Portuguese API names and domain terms when touching legacy code.
- Classes use `T...`, interfaces use `I...`, private fields use `F...`.
- Keep changes scoped. This repo often has a dirty worktree, so do not revert unrelated files.
- Prefer structured parsers/helpers already in the codebase over ad hoc string manipulation.
- Add comments only when they clarify non-obvious behavior.
- Avoid editing generated Lazarus resource files (`.lrs`) unless the task explicitly requires regeneration.
- Do not manually rewrite translation catalog files unless the task is localization-specific.

## Build

Always verify Pascal changes with the project build command:

```powershell
& "C:\Apps\lazarus\lazbuild.exe" --lazarusdir="C:\Apps\lazarus" --os=win64 --cpu=x86_64 --build-mode=Release iBiblia.lpi
```

Exit code `0` means the application compiled successfully. Existing warnings and hints are common; investigate new errors first.

## Tests

FPCUnit tests live under `tests/`, especially:

- `tests/versiculotest.pas`
- `tests/syntagmselectiontest.pas`
- `tests/ibibliatest.pas`

When changing verse, syntagm, selection, association, or parsing behavior, compile and run the relevant FPCUnit target if the local Lazarus/FPC setup supports it. The main application build is still required for UI and integration changes.

## Debugging

AI debug logging is centralized in `ai_debug.pas`. Set `IBIBLIA_AI_DEBUG=1` before launching the app to inspect prompts, LLM responses, parsing, syntagm selection, and application behavior. See `AI_DEBUG_README.md` when present in the workspace.

## Files To Read First For Common Tasks

- LLM options UI: `formllmoptions.pas`, `ai_flow_config.pas`, `ai_association_flow.pas`, `ai_translation_flow.pas`.
- Association algorithm changes: `CONTEXT.md`, `Verse.pas`, `syntagm.pas`, `syntagmselection.pas`, `tests/versiculotest.pas`.
- Batch AI processing: `ai_processing_manager.pas`, `ai_batch_orchestrator.pas`, `formaiprocessing.pas`, `Project.pas`.
- Persistence/export changes: `project_persistence.pas`, `project_config.pas`, `project_exporter.pas`, `Project.pas`.
- Main UI/menu behavior: `formprincipal.pas`, `Project.pas`, relevant `.lfm` file.
