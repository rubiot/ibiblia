unit performance_debug;

{$mode objfpc}{$H+}

interface

type
  TIBibliaPerfCounter = (
    ipcVerseLoads,
    ipcVerseChangeNotifications,
    ipcVerseVisualBuilds,
    ipcChapterSnapshotLoads,
    ipcChapterRenders,
    ipcRFPrefixBuilds,
    ipcWysiwygEncodes,
    ipcWysiwygDecodes,
    ipcWysiwygVisualBuilds,
    ipcCommandStateBuilds,
    ipcDirectWriteLayouts,
    ipcToolbarLayouts,
    ipcPendingChangeNotifications,
    ipcNotePreviewRenders,
    ipcNoteEditorDecodes,
    ipcNoteEditorEncodes,
    ipcTranslationMemoryViewRenders,
    ipcTranslationMemoryOccurrenceItemsProjected,
    ipcTranslationMemoryPreviewRenders,
    ipcTreeTraversals,
    ipcSaveBeforeScrollCalls,
    ipcPersistenceCommits,
    ipcAssociationProjectionBuilds,
    ipcCaretStopBuilds
  );

  TIBibliaPerfCounterValues = array[TIBibliaPerfCounter] of QWord;

function IBibliaPerfDebugEnabled: Boolean;
procedure IBibliaPerfIncrement(ACounter: TIBibliaPerfCounter;
  AAmount: QWord = 1);
procedure IBibliaPerfReset;
function IBibliaPerfSnapshot: TIBibliaPerfCounterValues;
procedure IBibliaPerfNavigationBegin;
procedure IBibliaPerfNavigationEnd(const AReference, AMode: string);

{ Tests use deterministic counters without changing the process environment. }
procedure IBibliaPerfSetEnabledForTests(AEnabled: Boolean);

implementation

uses
  SysUtils, LazLoggerBase;

var
  FEnabledInitialized: Boolean = False;
  FEnabled: Boolean = False;
  FCounters: TIBibliaPerfCounterValues;
  FNavigationDepth: Integer = 0;
  FNavigationStartedAt: QWord = 0;

const
  PERF_COUNTER_NAMES: array[TIBibliaPerfCounter] of string = (
    'verse_loads',
    'verse_change_notifications',
    'verse_visual_builds',
    'chapter_snapshot_loads',
    'chapter_renders',
    'rf_prefix_builds',
    'wysiwyg_encodes',
    'wysiwyg_decodes',
    'wysiwyg_visual_builds',
    'command_state_builds',
    'directwrite_layouts',
    'toolbar_layouts',
    'pending_change_notifications',
    'note_preview_renders',
    'note_editor_decodes',
    'note_editor_encodes',
    'translation_memory_view_renders',
    'translation_memory_occurrence_items_projected',
    'translation_memory_preview_renders',
    'tree_traversals',
    'save_before_scroll_calls',
    'persistence_commits',
    'association_projection_builds',
    'caret_stop_builds'
  );

function ReadEnabledFromEnvironment: Boolean;
var
  Value: string;
begin
  Value := Trim(GetEnvironmentVariable('IBIBLIA_PERF_DEBUG'));
  Result := SameText(Value, '1') or SameText(Value, 'true') or
    SameText(Value, 'yes') or SameText(Value, 'on');
end;

function IBibliaPerfDebugEnabled: Boolean;
begin
  if not FEnabledInitialized then
  begin
    FEnabled := ReadEnabledFromEnvironment;
    FEnabledInitialized := True;
  end;
  Result := FEnabled;
end;

procedure IBibliaPerfIncrement(ACounter: TIBibliaPerfCounter;
  AAmount: QWord);
begin
  if not IBibliaPerfDebugEnabled then
    Exit;
  Inc(FCounters[ACounter], AAmount);
end;

procedure IBibliaPerfReset;
begin
  FillChar(FCounters, SizeOf(FCounters), 0);
end;

function IBibliaPerfSnapshot: TIBibliaPerfCounterValues;
begin
  Result := FCounters;
end;

procedure IBibliaPerfNavigationBegin;
begin
  if not IBibliaPerfDebugEnabled then
    Exit;
  if FNavigationDepth = 0 then
  begin
    IBibliaPerfReset;
    FNavigationStartedAt := GetTickCount64;
  end;
  Inc(FNavigationDepth);
end;

procedure IBibliaPerfNavigationEnd(const AReference, AMode: string);
var
  Counter: TIBibliaPerfCounter;
  Details: string;
begin
  if not IBibliaPerfDebugEnabled or (FNavigationDepth <= 0) then
    Exit;
  Dec(FNavigationDepth);
  if FNavigationDepth <> 0 then
    Exit;

  Details := '';
  for Counter := Low(TIBibliaPerfCounter) to High(TIBibliaPerfCounter) do
  begin
    if Details <> '' then
      Details := Details + ' ';
    Details := Details + PERF_COUNTER_NAMES[Counter] + '=' +
      UIntToStr(FCounters[Counter]);
  end;
  DebugLn(Format('[PERF] navigation reference=%s mode=%s elapsed_ms=%d %s',
    [AReference, AMode, GetTickCount64 - FNavigationStartedAt, Details]));
end;

procedure IBibliaPerfSetEnabledForTests(AEnabled: Boolean);
begin
  FEnabledInitialized := True;
  FEnabled := AEnabled;
  FNavigationDepth := 0;
  FNavigationStartedAt := 0;
  IBibliaPerfReset;
end;

end.
