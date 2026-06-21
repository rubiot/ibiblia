# iBiblia AI Coding Agent Instructions

## Project Overview
iBiblia is a Lazarus/Free Pascal desktop application for creating annotated Bible modules with Strong's numbers and morphology. It uses LLM-powered AI to automate verse association between source and translated texts for theWord Bible software.

## Core Architecture

### Central Components
- **`TProjeto`** (`projeto.pas`) - Project manager coordinating SQLite database, UI tree navigation, and AI batch processing
- **`TVersiculo`** (`versiculo.pas`) - Verse representation parsing text into `TSyntagm` objects (words/phrases with Strong's/morphology)
- **`TAIProcessingManager`** (`ai_processing_manager.pas`) - Multi-threaded AI job orchestration with lazy verse creation optimization
- **`IAIVerseFlow`** (`ai_flows.pas`) - Interface for AI processing flows (association, translation)

### Data Flow Pattern
```
User action → TProjeto.CollectVersesFromNode (TVerseData records)
  → TAIProcessingManager.StartBatch (spawns worker threads)
  → Workers: lazy TVersiculo.Criar → LLM API call → parse response
  → Main thread: PumpApply → ApplyResult → TProjeto.SalvarParesVersiculo
```

### Key Architectural Insight
The lazy verse creation pattern (see `LAZY_VERSE_CREATION.md`) defers expensive syntagm parsing from main thread collection phase to background worker threads, enabling parallel processing while API calls are in flight.

## Critical Patterns

### Memory Management (CRITICAL)
```pascal
// Use Portuguese constructor/destructor names
verse := TVersiculo.Criar(project);
try
  // work with verse
finally
  verse.Destruir;  // REQUIRED - no automatic garbage collection
end;
```
- Always pair `.Criar` with `.Destruir` in try-finally blocks
- Check `OwnsVerse` flags to avoid double-free
- Worker threads create temporary verses with `OwnsVerse := True`

### Database Access Pattern
```pascal
// Use scrolling sessions for batch operations
TProjeto.StartScrollingSession;
try
  for verse in batch do
    GoToReference(verse.ref);
    // modify verse
    SalvarParesVersiculo(verse);
finally
  FinishScrollingSession;
end;
```
- SQLite via `TSqlite3Dataset` (no ORM)
- Direct SQL queries for verse data
- Table `pares` stores verse pairs and associations

### UI Thread Safety
```pascal
// AI work happens in TAIWorkerThread.Execute
// Results applied in main thread via Timer → PumpApply
TAIProcessingManager.PumpApply;  // Call from main thread only
```
- Never manipulate UI (TreeView, ScrollBoxes) from worker threads
- Use callbacks: `OnJobStateChange`, `OnJobCompleted`
- Visual feedback via tree node icon changes (indices 8=pending, 9=processing)

### AI Flow Implementation
```pascal
// Implement IAIVerseFlow for new AI features
function BuildMessages(Verse: TVersiculo): TLLMMessageArray;
  // Build LLM prompt from verse context
function ParseResponse(const RawContent: string; Verse: TVersiculo): TAIFlowResult;
  // Parse JSON/text response into structured result
procedure ApplyResult(ResultObj: TAIFlowResult; Verse: TVersiculo);
  // Apply parsed result (main thread only, uses TProjeto context)
```
See `ai_association_flow.pas` for reference implementation.

## File Conventions
- **`.pas`** - Pascal units (interface + implementation sections)
- **`.lfm`** - Lazarus forms (XML, edit in Form Designer)
- **`.lrs`** - Lazarus resources (regenerate with `lazres filename.lrs image1.png image2.png`)
- **Markdown docs** - Feature documentation (AI_DEBUG_README.md, AI_PROCESSING_MODAL_README.md)

## Debugging

### AI Debug System
```powershell
# Enable verbose AI flow logging
$env:IBIBLIA_AI_DEBUG = "1"
.\ibiblia.exe
# View with DebugView (Windows) or console output (Linux)
```
- Logs LLM prompts, responses, syntagm selection algorithm
- Implemented via `ai_debug.pas` with minimal performance impact when disabled
- See `AI_DEBUG_README.md` for detailed usage

### Common Issues
- **Memory leaks**: Search for `.Criar` without matching `.Destruir`
- **Thread safety**: Check `Application.ProcessMessages` in timer callbacks
- **Syntagm selection failures**: Enable AI debug, check `FindAndSelectSyntagms` logs
- **LFM corruption**: Reload from backup, reapply in Form Designer

## Build & Test

### Command Line Build
```powershell
# Always run this to verify compilation after changes
& "C:\Apps\lazarus4\lazbuild.exe" --lazarusdir="C:\Apps\lazarus4" --os=win64 --cpu=x86_64 --build-mode=Release iBiblia.lpi
```
- Use this command to ensure no compilation errors before committing
- Exit code 0 = success, non-zero = compilation failed

### Lazarus IDE
- Open `iBiblia.lpi`, press F9 to build and run
- Build modes: Debug (development), Release (production), Debug32/Release32 (32-bit)
- Post-build: `upx.bat` compresses executable (optional)

### Tests
```bash
cd tests
# Run FPCUnit test suite
fpcunitibiblia.exe
```
- Test files: `versiculotest.pas`, `ibibliatest.pas`
- Debug scripts: `debug_test.pas`, `debug_hebrew_structure.pas`

## Naming Conventions
- **Classes**: `TClassName` (e.g., `TVersiculo`, `TProjeto`)
- **Interfaces**: `IInterfaceName` (e.g., `IAIVerseFlow`)
- **Methods**: English for new code; legacy code uses Portuguese (`Criar`/`Destruir` remain for compatibility)
- **Language migration**: Write all new code in English; we're transitioning away from Portuguese
- **Enums**: Full words (`TAIJobState = (ajsPending, ajsFetching, ...)`)
- **Fields**: `F` prefix for private fields (`FVerse`, `FManager`)

## Key Methods to Know

### TProjeto
- `GoToReference(ref)` - Navigate to specific verse
- `CollectVersesFromNode(node, list)` - Gather verses for batch processing
- `StartScrollingSession` / `FinishScrollingSession` - Batch edit mode
- `SalvarParesVersiculo(verse)` - Save verse associations to database

### TVersiculo
- `Texto` property setter - Parses XML into syntagms (expensive operation)
- `FindAndSelectSyntagms(words, partial, joinConnected)` - Select syntagms by text
- `AssociarSintagmas` - Create associations from selection
- `VersiculoPar` - Reference to paired verse (source ↔ translation)

### TAIProcessingManager
- `StartBatch(flow, verses, project)` - Begin multi-threaded AI processing
- `PumpApply` - Process completed jobs (call from timer)
- `TotalJobs`, `CompletedJobs`, `IsIdle` - Progress tracking
- `EstimatedTimeRemaining`, `GetETAFormatted` - User feedback

## Current AI Features

### Association Flow
LLM identifies phrase pairs between source and translated verses, then:
1. Parses JSON response: `{"data": [["original phrase", "translated phrase"]], "comments": ""}`
2. Uses `FindAndSelectSyntagms` to locate syntagms for each phrase
3. Calls `AssociarSintagmas` to create associations
4. Saves via `TProjeto.SalvarParesVersiculo`

### Translation Flow
LLM translates source verse to target language, then:
1. Extracts translated text from response
2. Sets `verse.VersiculoPar.Texto` (triggers syntagm re-parse)
3. Optionally applies associations if included

### Visual Feedback
Tree nodes show AI processing state:
- Yellow icon (index 8): Pending
- Blue icon (index 9): Processing
- Restored original icon: Complete

## Extension Points

### Adding New AI Flows
1. Create unit implementing `IAIVerseFlow`
2. Define result type extending `TAIFlowResult`
3. Implement `BuildMessages` (prompt), `ParseResponse` (parse), `ApplyResult` (apply)
4. Register in UI menu (see `projeto.pas` context menu creation)

### Adding Configuration
1. Implement `IAIFlowConfig` interface
2. Define `GetConfigFields` with field metadata
3. Store in project database via `TProjeto.InserirInfo` / `AtualizarInfo`
4. Retrieve in `BuildMessages` for prompt customization

## Performance Considerations
- Syntagm parsing is CPU-intensive - avoid re-parsing unnecessarily
- Check `FModificado` flags before saving
- Use `TVerseData` records for batch collection (lazy creation pattern)
- LLM API calls dominate latency - maximize parallel workers (default 2)

## References
- `AI_PROCESSING_MODAL_README.md` - Modal UI design (in progress)
- `LAZY_VERSE_CREATION.md` - Batch optimization details
- `AI_VISUAL_FEEDBACK_README.md` - Tree node state management
- Wiki: https://github.com/rubiot/ibiblia/wiki
