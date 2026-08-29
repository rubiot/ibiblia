unit wysiwyg_editor_performance_test;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Forms, Graphics,
  directwrite_verse_editor, theword_wysiwyg_editor_control;

type
  TWysiwygEditorPerformanceTests = class(TTestCase)
  private
    FContentChanges: Integer;
    FEditor: TTheWordWysiwygEditorControl;
    FHost: TForm;
    FSelectionChanges: Integer;
    procedure ContentChanged(Sender: TTheWordWysiwygEditorControl);
    function DirectEditor: TDirectWriteVerseEditor;
    procedure SelectionChanged(Sender: TTheWordWysiwygEditorControl);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LoadVerseContextDoesNotPublishIntermediateChanges;
    procedure SelectionMovementDoesNotPublishContent;
    procedure SemanticMutationPublishesContentOnce;
    procedure StructuralBreakMutationsUseSingleDecodeAndRebuild;
    procedure RawCacheTracksNoteUndoRedoAndAcceptance;
    procedure VerticalResizePreservesDirectWriteLayout;
  end;

implementation

uses
  Classes, Controls, StdCtrls, LCLType, SysUtils, text_direction,
  theword_ont_codec_v2,
  theword_wysiwyg_session_v2, performance_debug;

procedure TWysiwygEditorPerformanceTests.ContentChanged(
  Sender: TTheWordWysiwygEditorControl);
begin
  Inc(FContentChanges);
end;

procedure TWysiwygEditorPerformanceTests.SelectionChanged(
  Sender: TTheWordWysiwygEditorControl);
begin
  Inc(FSelectionChanges);
end;

function TWysiwygEditorPerformanceTests.DirectEditor:
  TDirectWriteVerseEditor;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to FEditor.ControlCount - 1 do
    if FEditor.Controls[Index] is TDirectWriteVerseEditor then
      Exit(TDirectWriteVerseEditor(FEditor.Controls[Index]));
end;

procedure TWysiwygEditorPerformanceTests.SetUp;
begin
  inherited SetUp;
  IBibliaPerfSetEnabledForTests(True);
  FHost := TForm.Create(nil);
  FHost.SetBounds(0, 0, 640, 240);
  FHost.HandleNeeded;
  FEditor := TTheWordWysiwygEditorControl.Create(FHost);
  FEditor.Parent := FHost;
  FEditor.Align := alClient;
  FEditor.OnChanged := @ContentChanged;
  FEditor.OnSelectionChanged := @SelectionChanged;
  FContentChanges := 0;
  FSelectionChanges := 0;
  Application.ProcessMessages;
end;

procedure TWysiwygEditorPerformanceTests.TearDown;
begin
  FHost.Free;
  IBibliaPerfSetEnabledForTests(False);
  inherited TearDown;
end;

procedure TWysiwygEditorPerformanceTests.
  LoadVerseContextDoesNotPublishIntermediateChanges;
var
  Counters: TIBibliaPerfCounterValues;
begin
  IBibliaPerfReset;
  FEditor.LoadVerseContext('alpha beta', tdLeftToRight, FHost.Font, 3, 7);
  AssertEquals('raw text', 'alpha beta', FEditor.Text);
  AssertEquals('content notifications', 0, FContentChanges);
  AssertEquals('selection notifications', 0, FSelectionChanges);
  Counters := IBibliaPerfSnapshot;
  AssertEquals('one WYSIWYG decode', 1,
    Int64(Counters[ipcWysiwygDecodes]));
  AssertEquals('load does not encode', 0,
    Int64(Counters[ipcWysiwygEncodes]));
  AssertEquals('one visual build', 1,
    Int64(Counters[ipcWysiwygVisualBuilds]));
end;

procedure TWysiwygEditorPerformanceTests.
  SelectionMovementDoesNotPublishContent;
var
  Editor: TDirectWriteVerseEditor;
  Counters: TIBibliaPerfCounterValues;
begin
  FEditor.LoadVerseContext('alpha beta', tdLeftToRight, FHost.Font, 3, 0);
  Editor := DirectEditor;
  AssertNotNull('direct editor', Editor);
  FContentChanges := 0;
  FSelectionChanges := 0;
  IBibliaPerfReset;
  Editor.SetSelection(3, 0);
  AssertEquals('content notifications', 0, FContentChanges);
  AssertEquals('selection notifications', 1, FSelectionChanges);
  Counters := IBibliaPerfSnapshot;
  AssertEquals('cursor movement does not encode', 0,
    Int64(Counters[ipcWysiwygEncodes]));
  AssertEquals('cursor movement does not decode', 0,
    Int64(Counters[ipcWysiwygDecodes]));
  AssertEquals('cursor movement does not rebuild', 0,
    Int64(Counters[ipcWysiwygVisualBuilds]));
  AssertEquals('cursor movement reuses caret stops', 0,
    Int64(Counters[ipcCaretStopBuilds]));
end;

procedure TWysiwygEditorPerformanceTests.
  SemanticMutationPublishesContentOnce;
var
  Editor: TDirectWriteVerseEditor;
  Counters: TIBibliaPerfCounterValues;
begin
  FEditor.LoadVerseContext('alpha beta', tdLeftToRight, FHost.Font, 3, 0);
  Editor := DirectEditor;
  AssertNotNull('direct editor', Editor);
  Editor.SetSelection(0, 5);
  FContentChanges := 0;
  FSelectionChanges := 0;
  IBibliaPerfReset;
  AssertTrue('semantic mutation', FEditor.ApplySemantic(twsmHtmlBold));
  AssertEquals('raw text', '<b>alpha</b> beta', FEditor.Text);
  AssertEquals('content notifications', 1, FContentChanges);
  AssertEquals('intermediate selection notifications', 0,
    FSelectionChanges);
  Counters := IBibliaPerfSnapshot;
  AssertEquals('one visual rebuild', 1,
    Int64(Counters[ipcWysiwygVisualBuilds]));
  AssertEquals('mutation encodes once', 1,
    Int64(Counters[ipcWysiwygEncodes]));
  AssertEquals('mutation decodes once', 1,
    Int64(Counters[ipcWysiwygDecodes]));
  AssertTrue('one undo entry', FEditor.Undo);
  AssertFalse('no second undo entry', FEditor.Undo);
end;

procedure TWysiwygEditorPerformanceTests.
  StructuralBreakMutationsUseSingleDecodeAndRebuild;
var
  Broker: TEdit;
  ComponentIndex: Integer;
  Counters: TIBibliaPerfCounterValues;
  Editor: TDirectWriteVerseEditor;
  Key: Word;

  procedure PressKey(AKey: Word; AShift: TShiftState);
  begin
    Key := AKey;
    AssertTrue('input broker key handler', Assigned(Broker.OnKeyDown));
    Broker.OnKeyDown(Broker, Key, AShift);
    AssertEquals('structural key is consumed', 0, Key);
  end;

  procedure AssertSinglePipeline(const AContext: string);
  begin
    Counters := IBibliaPerfSnapshot;
    AssertEquals(AContext + ': one encode', 1,
      Int64(Counters[ipcWysiwygEncodes]));
    AssertEquals(AContext + ': one decode', 1,
      Int64(Counters[ipcWysiwygDecodes]));
    AssertEquals(AContext + ': one visual rebuild', 1,
      Int64(Counters[ipcWysiwygVisualBuilds]));
    AssertEquals(AContext + ': one content notification', 1,
      FContentChanges);
    AssertEquals(AContext + ': one final selection notification', 1,
      FSelectionChanges);
    AssertTrue(AContext + ': one undo entry', FEditor.Undo);
    AssertFalse(AContext + ': no second undo entry', FEditor.Undo);
  end;

  procedure Prepare(const ARaw: string; ACaret: Integer);
  begin
    FEditor.LoadVerseContext(ARaw, tdLeftToRight, FHost.Font, 3, 0);
    Editor.SetSelection(ACaret, 0);
    FContentChanges := 0;
    FSelectionChanges := 0;
    IBibliaPerfReset;
  end;

begin
  FEditor.LoadVerseContext('alpha beta', tdLeftToRight, FHost.Font, 3, 0);
  Editor := DirectEditor;
  AssertNotNull('direct editor', Editor);
  Broker := nil;
  for ComponentIndex := 0 to Editor.ComponentCount - 1 do
    if Editor.Components[ComponentIndex] is TEdit then
    begin
      Broker := TEdit(Editor.Components[ComponentIndex]);
      Break;
    end;
  AssertNotNull('native input broker', Broker);

  Prepare('alpha beta', 5);
  PressKey(VK_RETURN, []);
  AssertEquals('Enter inserts CM', 'alpha<CM> beta', FEditor.Text);
  AssertSinglePipeline('Enter');

  Prepare('alpha beta', 5);
  PressKey(VK_RETURN, [ssShift]);
  AssertEquals('Shift+Enter inserts CL', 'alpha<CL> beta', FEditor.Text);
  AssertSinglePipeline('Shift+Enter');

  Prepare('alpha<CM> beta', 6);
  PressKey(VK_BACK, []);
  AssertEquals('Backspace removes CM', 'alpha beta', FEditor.Text);
  AssertSinglePipeline('Backspace');
end;

procedure TWysiwygEditorPerformanceTests.
  RawCacheTracksNoteUndoRedoAndAcceptance;
var
  Counters: TIBibliaPerfCounterValues;
  NoteId: QWord;
  Session: TTheWordWysiwygSessionV2;
begin
  Session := TTheWordWysiwygSessionV2.Create('a b', 3);
  try
    IBibliaPerfReset;
    AssertTrue('insert note', Session.InsertTranslatorNote(1, '',
      'note', NoteId));
    Counters := IBibliaPerfSnapshot;
    AssertEquals('note mutation encodes once', 1,
      Int64(Counters[ipcWysiwygEncodes]));
    IBibliaPerfReset;
    AssertEquals('cached note raw', 'a<RF>note<Rf> b', Session.RawText);
    AssertTrue('dirty after note', Session.Dirty);
    AssertEquals('repeat cached raw', 'a<RF>note<Rf> b', Session.RawText);
    AssertTrue('repeat dirty', Session.Dirty);
    Counters := IBibliaPerfSnapshot;
    AssertEquals('cache reads do not encode', 0,
      Int64(Counters[ipcWysiwygEncodes]));
    AssertTrue('undo note', Session.Undo);
    AssertEquals('cached undo raw', 'a b', Session.RawText);
    AssertTrue('redo note', Session.Redo);
    AssertEquals('cached redo raw', 'a<RF>note<Rf> b', Session.RawText);
    Session.AcceptChanges;
    AssertFalse('accepted cache is clean', Session.Dirty);
  finally
    Session.Free;
  end;
end;

procedure TWysiwygEditorPerformanceTests.
  VerticalResizePreservesDirectWriteLayout;
var
  Counters: TIBibliaPerfCounterValues;
  Editor: TDirectWriteVerseEditor;
  Generation: QWord;
begin
  FEditor.LoadVerseContext('alpha beta gamma delta', tdLeftToRight,
    FHost.Font, 3, 0);
  Editor := DirectEditor;
  AssertNotNull('direct editor', Editor);
  Editor.SetSelection(0, 0);
  Editor.Repaint;
  Generation := Editor.LayoutGeneration;
  IBibliaPerfReset;
  Editor.Height := Editor.Height + 20;
  Editor.Repaint;
  AssertEquals('vertical resize generation', Generation,
    Editor.LayoutGeneration);
  Counters := IBibliaPerfSnapshot;
  AssertEquals('vertical resize reuses the layout', 0,
    Int64(Counters[ipcDirectWriteLayouts]));
  Editor.Width := Editor.Width - 20;
  Editor.Repaint;
  AssertTrue('width resize invalidates layout',
    Editor.LayoutGeneration > Generation);
  Counters := IBibliaPerfSnapshot;
  AssertEquals('width resize builds one layout', 1,
    Int64(Counters[ipcDirectWriteLayouts]));
end;

initialization
  RegisterTest(TWysiwygEditorPerformanceTests);

end.
