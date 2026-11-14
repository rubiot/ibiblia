unit project_save_as_test;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, Forms, Controls, ExtCtrls,
  StdCtrls, Project, BibleScope;
type
  TSaveAsTestProject = class(TProject)
  protected
    procedure ActivateSavedProject(const NewFileName: string); override;
  public
    FailActivation: Boolean;
    procedure QueueObserver;
    procedure RunAutoSave;
  end;
  TProjectSaveAsTests = class(TTestCase)
  private
    FHost: TForm;
    FPanels: array[0..3] of TScrollBox;
    FComments: TMemo;
    FStatus: TComboBox;
    FProject: TSaveAsTestProject;
    FTempDir, FOriginal: string;
    FObserverCalls: Integer;
    procedure Observe(Sender: TProject);
    function Path(const Name: string): string;
    function ReadField(const FileName, Reference, Field: string): string;
    procedure CheckColdOpen(Scope: TEscopoTexto; const Marker: string; SwitchProject: Boolean);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ColdOpenNTAtFirst;
    procedure ColdOpenNTAtLater;
    procedure ColdOpenWholeBibleAtFirst;
    procedure ColdOpenWholeBibleAtLater;
    procedure SwitchingProjectDoesNotCopyOldViews;
    procedure SwitchingProjectAtSameReferenceDoesNotCopyOldViews;
    procedure MismatchedViewReferenceMustNotOverwriteCurrentRow;
    procedure IntentionallyEmptyTextIsSaved;
    procedure CopiesPendingContentAndContinuesInNewFile;
    procedure CleanProjectKeepsReferenceAndNavigationHistory;
    procedure GuardDefersEventsAndPreventsSourceWrites;
    procedure FailedDestinationPreservesPendingState;
    procedure FailedActivationKeepsOriginalAndPublishedCopy;
    procedure ConfirmedSameFileUsesNormalSave;
    procedure SaveAsCapturesOpenVisualEditor;
    procedure MismatchedEditorTokenizerBlocksPublication;
    procedure SaveAsCapturesRawLexicalEditor;
    procedure GuardDefersAutomaticSuggestionAndEditor;
  end;
implementation
uses sqlite3conn, sqldb, project_database_bootstrap, project_persistence,
  project_config, Verse, theword_wysiwyg_editor_control,
  directwrite_verse_editor, SynEdit;
procedure TSaveAsTestProject.ActivateSavedProject(const NewFileName: string);
begin
  if FailActivation then raise Exception.Create('Injected activation failure');
  inherited ActivateSavedProject(NewFileName);
end;
procedure TSaveAsTestProject.QueueObserver;
begin
  NotifyNewVerseObserversAfterEdit;
end;
procedure TSaveAsTestProject.RunAutoSave;
begin
  HandleAutoSave(nil);
end;
procedure TProjectSaveAsTests.Observe(Sender: TProject);
begin
  Inc(FObserverCalls);
end;
function TProjectSaveAsTests.Path(const Name: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + Name;
end;
function TProjectSaveAsTests.ReadField(const FileName, Reference,
  Field: string): string;
var Persistence: TProjectPersistence;
begin
  Persistence := TProjectPersistence.Create;
  try
    Persistence.Open(FileName);
    AssertTrue(Persistence.LocateReference(Reference));
    Result := Persistence.CurrentFieldAsString(Field);
  finally
    Persistence.Free;
  end;
end;
procedure TProjectSaveAsTests.SetUp;
var Connection: TSQLite3Connection; Transaction: TSQLTransaction; Index: Integer;
begin
  inherited SetUp;
  Forms.Application.Initialize;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia_save_as_%x_%x', [PtrUInt(Self), GetTickCount64]);
  ForceDirectories(FTempDir);
  FOriginal := Path('original.bib');
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(FOriginal, etNT, 3);
  Connection := TSQLite3Connection.Create(nil);
  try
    Transaction := TSQLTransaction.Create(Connection);
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FOriginal;
    Connection.Open;
    Connection.ExecuteDirect('update pares set pare_texto_origem = ''source<WG1>'', ' +
      'pare_texto_destino = ''destination'', pare_texto_consulta1 = ''reference one'', ' +
      'pare_texto_consulta2 = ''reference two'', pare_comentarios = ''old comment''');
    Transaction.Commit;
  finally
    Connection.Free;
  end;
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(0, 0, 900, 600);
  for Index := 0 to 3 do
  begin
    FPanels[Index] := TScrollBox.Create(FHost);
    FPanels[Index].Parent := FHost;
    FPanels[Index].SetBounds(0, Index * 110, 900, 105);
  end;
  FComments := TMemo.Create(FHost);
  FComments.Parent := FHost;
  FComments.SetBounds(0, 445, 900, 100);
  FStatus := TComboBox.Create(FHost);
  FStatus.Parent := FHost;
  FHost.Show;
  FProject := TSaveAsTestProject.Create(FPanels, nil, FStatus, FComments);
  FProject.Escopo := etNT;
  FProject.OpenProject(FOriginal);
  Application.ProcessMessages;
  FObserverCalls := 0;
end;
procedure TProjectSaveAsTests.TearDown;
var Search: TSearchRec;
begin
  FProject.Free;
  FHost.Free;
  if FindFirst(Path('*'), faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then DeleteFile(Path(Search.Name));
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(FTempDir);
  inherited TearDown;
end;
procedure TProjectSaveAsTests.CheckColdOpen(Scope: TEscopoTexto;
  const Marker: string; SwitchProject: Boolean);
var
  FileName, FirstRef, SecondRef: string;
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  if Scope = etNT then FirstRef := '40,1,1' else FirstRef := '1,1,1';
  if Scope = etNT then SecondRef := '40,1,2' else SecondRef := '1,1,2';
  FileName := Path('cold.bib');
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(FileName, Scope, 3);
  Connection := TSQLite3Connection.Create(nil);
  try
    Transaction := TSQLTransaction.Create(Connection);
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FileName;
    Connection.Open;
    Connection.ExecuteDirect('update pares set pare_texto_origem = ''origin'', ' +
      'pare_texto_destino = ''target'', pare_texto_consulta1 = ''reference1'', ' +
      'pare_texto_consulta2 = ''reference2'', ' +
      'pare_pares = ''<par a="0" b="0">'', pare_comentarios = ''comment''');
    if Scope = etONT then
      Connection.ExecuteDirect('insert into pares select ''40,1,1'', ''40,1,1'', ' +
        'pare_texto_origem, pare_texto_destino, pare_texto_consulta1, ' +
        'pare_texto_consulta2, pare_pares, pare_situacao, pare_comentarios ' +
        'from pares where pare_id = ''1,1,1''');
    Connection.ExecuteDirect('insert into info values (''marcador'', ''' + Marker + ''')');
    Transaction.Commit;
  finally
    Connection.Free;
  end;
  if not SwitchProject then
  begin
    FreeAndNil(FProject);
    FProject := TSaveAsTestProject.Create(FPanels, nil, FStatus, FComments);
  end
  else
  begin
    FProject.VerseObjects[tbOrigem].Text := 'old project source';
    FProject.VerseObjects[tbDestino].Text := 'old project destination';
    FComments.Text := 'old project comment';
  end;
  FProject.Escopo := Scope;
  FProject.OpenProject(FileName);
  AssertEquals('opened marker', Marker, FProject.ID);
  AssertTrue('save synthetic project', FProject.SaveProject);
  AssertEquals('first source', 'origin', ReadField(FileName, FirstRef, 'pare_texto_origem'));
  AssertEquals('first destination', 'target', ReadField(FileName, FirstRef, 'pare_texto_destino'));
  AssertEquals('first reference 1', 'reference1', ReadField(FileName, FirstRef, 'pare_texto_consulta1'));
  AssertEquals('first reference 2', 'reference2', ReadField(FileName, FirstRef, 'pare_texto_consulta2'));
  AssertEquals('first associations', '<par a="0" b="0">', ReadField(FileName, FirstRef, 'pare_pares'));
  AssertEquals('first comment', 'comment', ReadField(FileName, FirstRef, 'pare_comentarios'));
  AssertEquals('second source', 'origin', ReadField(FileName, SecondRef, 'pare_texto_origem'));
  AssertEquals('second destination', 'target', ReadField(FileName, SecondRef, 'pare_texto_destino'));
  AssertEquals('second associations', '<par a="0" b="0">', ReadField(FileName, SecondRef, 'pare_pares'));
  AssertEquals('second comment', 'comment', ReadField(FileName, SecondRef, 'pare_comentarios'));
  FProject.OpenProject(FileName);
  AssertEquals('reopened marker', Marker, FProject.ID);
  AssertEquals('reopened source', 'origin', FProject.VerseObjects[tbOrigem].Text);
  AssertEquals('reopened destination', 'target', FProject.VerseObjects[tbDestino].Text);
  AssertEquals('reopened comment', 'comment', FComments.Text);
end;
procedure TProjectSaveAsTests.ColdOpenNTAtFirst;
begin
  CheckColdOpen(etNT, '40,1,1', False);
end;
procedure TProjectSaveAsTests.ColdOpenNTAtLater;
begin
  CheckColdOpen(etNT, '40,1,2', False);
end;
procedure TProjectSaveAsTests.ColdOpenWholeBibleAtFirst;
begin
  CheckColdOpen(etONT, '1,1,1', False);
end;
procedure TProjectSaveAsTests.ColdOpenWholeBibleAtLater;
begin
  CheckColdOpen(etONT, '1,1,2', False);
end;
procedure TProjectSaveAsTests.SwitchingProjectDoesNotCopyOldViews;
begin
  CheckColdOpen(etNT, '40,1,2', True);
end;
procedure TProjectSaveAsTests.SwitchingProjectAtSameReferenceDoesNotCopyOldViews;
begin
  CheckColdOpen(etNT, '40,1,1', True);
end;
procedure TProjectSaveAsTests.MismatchedViewReferenceMustNotOverwriteCurrentRow;
var Rejected: Boolean;
begin
  FProject.VerseObjects[tbOrigem].ReferenceID := '40,1,2';
  FProject.VerseObjects[tbOrigem].Text := 'wrong source';
  FProject.VerseObjects[tbDestino].Text := 'wrong destination';
  FComments.Text := 'wrong comment';
  Rejected := False;
  try
    try
      FProject.SaveProject;
    except
      on E: Exception do
      begin
        if Pos('displayed views do not match', E.Message) = 0 then raise;
        Rejected := True;
      end;
    end;
  finally
    FProject.VerseObjects[tbOrigem].ReferenceID := '40,1,1';
  end;
  AssertEquals('source', 'source<WG1>', ReadField(FOriginal, '40,1,1', 'pare_texto_origem'));
  AssertEquals('destination', 'destination', ReadField(FOriginal, '40,1,1', 'pare_texto_destino'));
  AssertEquals('comment', 'old comment', ReadField(FOriginal, '40,1,1', 'pare_comentarios'));
  AssertTrue('mismatched views must reject save', Rejected);
end;
procedure TProjectSaveAsTests.IntentionallyEmptyTextIsSaved;
begin
  FProject.VerseObjects[tbDestino].Text := '';
  AssertTrue(FProject.SaveProject);
  AssertEquals('', ReadField(FOriginal, '40,1,1', 'pare_texto_destino'));
  FProject.OpenProject(FOriginal);
  AssertEquals('', FProject.VerseObjects[tbDestino].Text);
end;
procedure TProjectSaveAsTests.CopiesPendingContentAndContinuesInNewFile;
var Destination: string; Persistence: TProjectPersistence;
begin
  Destination := Path('copy.bib');
  FProject.VerseObjects[tbDestino].Text := 'edited first';
  FProject.VerseObjects[tbOrigem].Pairs := '<par a="0" b="0">';
  FComments.Text := 'new comment';
  FProject.Status := 2;
  FProject.SetInfo('description.text1', 'Pending description');
  FProject.NextVerse;
  FProject.VerseObjects[tbDestino].Text := 'edited second';
  FProject.VerseObjects[tbConsulta1].Text := '';
  FProject.VerseObjects[tbConsulta2].Text := 'edited reference';
  AssertTrue(FProject.Modificado);
  AssertTrue(FProject.SaveProjectAs(Destination, False));
  AssertEquals(Destination, FProject.FileName);
  AssertEquals(Destination, FProject.Caminho);
  AssertEquals('40,1,2', FProject.ID);
  AssertFalse('published contents are clean', FProject.Modificado);
  AssertEquals('destination', ReadField(FOriginal, '40,1,1', 'pare_texto_destino'));
  AssertEquals('destination', ReadField(FOriginal, '40,1,2', 'pare_texto_destino'));
  AssertEquals('old comment', ReadField(FOriginal, '40,1,1', 'pare_comentarios'));
  AssertEquals('', ReadField(FOriginal, '40,1,1', 'pare_pares'));
  AssertEquals('edited first', ReadField(Destination, '40,1,1', 'pare_texto_destino'));
  AssertEquals('new comment', ReadField(Destination, '40,1,1', 'pare_comentarios'));
  AssertEquals('2', ReadField(Destination, '40,1,1', 'pare_situacao'));
  AssertEquals('<par a="0" b="0">', ReadField(Destination, '40,1,1', 'pare_pares'));
  AssertEquals('', ReadField(Destination, '40,1,2', 'pare_texto_consulta1'));
  AssertEquals('edited reference', ReadField(Destination, '40,1,2', 'pare_texto_consulta2'));
  Persistence := TProjectPersistence.Create;
  try
    Persistence.Open(Destination);
    AssertEquals('Pending description', Persistence.ReadInfo('description.text1'));
    AssertEquals('40,1,2', Persistence.ReadInfo(PROJECT_CONFIG_KEY_MARKER));
  finally
    Persistence.Free;
  end;
  FProject.VerseObjects[tbDestino].Text := 'saved later';
  AssertTrue(FProject.SaveProject);
  AssertEquals('saved later', ReadField(Destination, '40,1,2', 'pare_texto_destino'));
  AssertEquals('destination', ReadField(FOriginal, '40,1,2', 'pare_texto_destino'));
end;
procedure TProjectSaveAsTests.CleanProjectKeepsReferenceAndNavigationHistory;
begin
  FProject.NextVerse;
  AssertTrue(FProject.CanGoBack);
  AssertTrue(FProject.SaveProjectAs(Path('clean.bib'), False));
  AssertEquals('40,1,2', FProject.ID);
  AssertTrue(FProject.CanGoBack);
  FProject.GoBack;
  AssertEquals('40,1,1', FProject.ID);
end;
procedure TProjectSaveAsTests.GuardDefersEventsAndPreventsSourceWrites;
begin
  FProject.AutoSave := True;
  FProject.VerseObjects[tbDestino].Text := 'unsaved';
  FProject.OnNewVerseSubscribe(@Observe);
  FProject.QueueObserver;
  AssertTrue(FProject.BeginSaveAs);
  try
    Application.ProcessMessages;
    AssertEquals('observer deferred', 0, FObserverCalls);
    AssertFalse(FProject.CanSaveAs);
    AssertFalse(FProject.SaveProject);
    FProject.RunAutoSave;
    FProject.Commit;
    FProject.NextVerse;
    FProject.GoToReference('40,1,3');
    AssertEquals('40,1,1', FProject.ID);
    AssertEquals('destination', ReadField(FOriginal, '40,1,1', 'pare_texto_destino'));
    AssertTrue(FProject.Modificado);
  finally
    FProject.EndSaveAs;
  end;
  Application.ProcessMessages;
  AssertEquals('observer resumed once', 1, FObserverCalls);
  AssertEquals(FOriginal, FProject.FileName);
  AssertTrue(FProject.Modificado);
end;
procedure TProjectSaveAsTests.FailedDestinationPreservesPendingState;
var Raised: Boolean;
begin
  FProject.VerseObjects[tbDestino].Text := 'unsaved';
  Raised := False;
  try FProject.SaveProjectAs(Path('missing/copy.bib'), False);
  except on E: Exception do Raised := True; end;
  AssertTrue(Raised);
  AssertFalse(FProject.SaveAsInProgress);
  AssertEquals(FOriginal, FProject.FileName);
  AssertTrue(FProject.Modificado);
  AssertEquals('unsaved', FProject.VerseObjects[tbDestino].XML);
  AssertEquals('destination', ReadField(FOriginal, '40,1,1', 'pare_texto_destino'));
  AssertTrue('can retry', FProject.SaveProjectAs(Path('retry.bib'), False));
end;
procedure TProjectSaveAsTests.FailedActivationKeepsOriginalAndPublishedCopy;
var Raised: Boolean;
begin
  FProject.VerseObjects[tbDestino].Text := 'unsaved';
  FProject.FailActivation := True;
  Raised := False;
  try FProject.SaveProjectAs(Path('published.bib'), False);
  except on E: Exception do begin
    Raised := True;
    AssertTrue(Pos(Path('published.bib'), E.Message) > 0);
  end; end;
  AssertTrue(Raised);
  AssertEquals(FOriginal, FProject.FileName);
  AssertTrue(FProject.Modificado);
  AssertEquals('unsaved', ReadField(Path('published.bib'), '40,1,1', 'pare_texto_destino'));
  AssertEquals('destination', ReadField(FOriginal, '40,1,1', 'pare_texto_destino'));
  FProject.FailActivation := False;
  AssertTrue('original remains saveable', FProject.SaveProject);
end;
procedure TProjectSaveAsTests.ConfirmedSameFileUsesNormalSave;
begin
  FProject.VerseObjects[tbDestino].Text := 'same file';
  AssertTrue(FProject.BeginSaveAs);
  try AssertTrue(FProject.SaveProjectAs(Path('./original.bib'), True));
  finally FProject.EndSaveAs; end;
  AssertEquals(FOriginal, FProject.FileName);
  AssertEquals('same file', ReadField(FOriginal, '40,1,1', 'pare_texto_destino'));
  AssertFalse(FProject.Modificado);
end;
function FindControl(Parent: TWinControl; ControlClass: TClass): TWinControl;
var Index: Integer;
begin
  Result := nil;
  for Index := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[Index] is TWinControl then begin
      if Parent.Controls[Index].InheritsFrom(ControlClass) then
        Exit(TWinControl(Parent.Controls[Index]));
      Result := FindControl(TWinControl(Parent.Controls[Index]), ControlClass);
      if Assigned(Result) then Exit;
    end;
end;
procedure TProjectSaveAsTests.SaveAsCapturesOpenVisualEditor;
var Editor: TTheWordWysiwygEditorControl; DirectEditor: TDirectWriteVerseEditor;
begin
  FProject.DefaultVerseMode := dvmEditing;
  FProject.RequestDefaultVerseModeForCurrentVerse;
  Application.ProcessMessages;
  Editor := TTheWordWysiwygEditorControl(FindControl(FHost, TTheWordWysiwygEditorControl));
  AssertNotNull(Editor);
  DirectEditor := TDirectWriteVerseEditor(FindControl(Editor, TDirectWriteVerseEditor));
  AssertNotNull(DirectEditor);
  DirectEditor.SetSelection(Length(DirectEditor.Document.Text), 0);
  DirectEditor.SimulateTextInputFixture(' edited');
  AssertTrue(FProject.Modificado);
  AssertTrue(FProject.SaveProjectAs(Path('editor.bib'), False));
  AssertEquals('destination edited', ReadField(Path('editor.bib'), '40,1,1', 'pare_texto_destino'));
  AssertEquals('destination', ReadField(FOriginal, '40,1,1', 'pare_texto_destino'));
  AssertFalse(FProject.Modificado);
end;
procedure TProjectSaveAsTests.MismatchedEditorTokenizerBlocksPublication;
var Editor: TTheWordWysiwygEditorControl; RawEditor: TSynEdit;
  MismatchedTokenizer: Integer;
begin
  FProject.VerseObjects[tbDestino].Text := 'destination<WG1>';
  AssertTrue(FProject.SaveProject);
  FProject.DefaultVerseMode := dvmEditing;
  FProject.RequestDefaultVerseModeForCurrentVerse;
  Application.ProcessMessages;
  Editor := TTheWordWysiwygEditorControl(FindControl(FHost, TTheWordWysiwygEditorControl));
  AssertNotNull(Editor);
  // Raw lexical changes are valid; a stale tokenizer context must still block publication.
  if FProject.VerseObjects[tbDestino].TokenizerVersion = 1 then
    MismatchedTokenizer := 2
  else
    MismatchedTokenizer := 1;
  Editor.LoadVerseContext(FProject.VerseObjects[tbDestino].XML,
    FProject.VerseObjects[tbDestino].BaseDirection,
    FProject.VerseObjects[tbDestino].Font, MismatchedTokenizer,
    FProject.VerseObjects[tbDestino].AutomaticNoteStart);
  Editor.RawMode := True;
  RawEditor := TSynEdit(FindControl(Editor, TSynEdit));
  AssertNotNull(RawEditor);
  RawEditor.Text := 'destination<WG2>';
  AssertFalse(FProject.SaveProjectAs(Path('invalid.bib'), False));
  AssertFalse(FileExists(Path('invalid.bib')));
  AssertEquals(FOriginal, FProject.FileName);
  AssertTrue(Editor.Visible);
  AssertEquals('destination<WG2>', RawEditor.Lines[0]);
  AssertEquals('destination<WG1>', ReadField(FOriginal, '40,1,1', 'pare_texto_destino'));
end;

procedure TProjectSaveAsTests.SaveAsCapturesRawLexicalEditor;
var Editor: TTheWordWysiwygEditorControl; RawEditor: TSynEdit;
begin
  FProject.VerseObjects[tbDestino].Text := 'destination<WG1>';
  AssertTrue(FProject.SaveProject);
  FProject.DefaultVerseMode := dvmEditing;
  FProject.RequestDefaultVerseModeForCurrentVerse;
  Application.ProcessMessages;
  Editor := TTheWordWysiwygEditorControl(FindControl(FHost, TTheWordWysiwygEditorControl));
  AssertNotNull(Editor);
  Editor.RawMode := True;
  RawEditor := TSynEdit(FindControl(Editor, TSynEdit));
  AssertNotNull(RawEditor);
  RawEditor.Text := 'destination<WG2>';

  AssertTrue('valid Raw lexical edits must be included in Save As',
    FProject.SaveProjectAs(Path('raw-editor.bib'), False));
  AssertEquals('destination<WG2>',
    ReadField(Path('raw-editor.bib'), '40,1,1', 'pare_texto_destino'));
  AssertEquals('Save As must preserve the original file', 'destination<WG1>',
    ReadField(FOriginal, '40,1,1', 'pare_texto_destino'));
  AssertFalse(FProject.Modificado);
end;
procedure TProjectSaveAsTests.GuardDefersAutomaticSuggestionAndEditor;
var Editor: TTheWordWysiwygEditorControl; Calls: QWord;
begin
  FProject.AutomaticAssociationSuggestion := True;
  FProject.DefaultVerseMode := dvmEditing;
  FProject.NextVerse;
  Calls := FProject.AutomaticSuggestionInvocationCount;
  AssertTrue(FProject.BeginSaveAs);
  try
    Application.ProcessMessages;
    AssertEquals(Int64(Calls), Int64(FProject.AutomaticSuggestionInvocationCount));
    Editor := TTheWordWysiwygEditorControl(FindControl(FHost, TTheWordWysiwygEditorControl));
    AssertTrue('queued editor is deferred', not Assigned(Editor) or not Editor.Visible);
  finally
    FProject.EndSaveAs;
  end;
  Application.ProcessMessages;
  AssertEquals(Int64(Calls + 1), Int64(FProject.AutomaticSuggestionInvocationCount));
  Editor := TTheWordWysiwygEditorControl(FindControl(FHost, TTheWordWysiwygEditorControl));
  AssertNotNull(Editor);
  AssertTrue(Editor.Visible);
end;

initialization
  RegisterTest(TProjectSaveAsTests);
end.
