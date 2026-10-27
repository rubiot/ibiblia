unit project_save_as_test;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fpcunit, testregistry, Forms, Controls, ExtCtrls,
  StdCtrls, Project;
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
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
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
  project_config, BibleScope, Verse, theword_wysiwyg_editor_control,
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