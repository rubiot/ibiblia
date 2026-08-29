unit default_verse_mode_test;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, Forms, ExtCtrls, StdCtrls, Project,
  theword_wysiwyg_editor_control;

type
  TDefaultVerseModeTests = class(TTestCase)
  private
    FDestinationPanel: TScrollBox;
    FHost: TForm;
    FProject: TProject;
    FSourcePanel: TScrollBox;
    FStatusCombo: TComboBox;
    FTempDir: string;
    function ActiveEditor: TTheWordWysiwygEditorControl;
    procedure CancelActiveEditor;
    procedure CreateAndOpenProject(AMode: TDefaultVerseMode;
      const ADestination1: string = 'Destination 1';
      const ADestination2: string = 'Destination 2';
      const ADestination3: string = 'Destination 3');
    function TempPath(const AFileName: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure IntegerValuesUseAssociationAsSafeFallback;
    procedure NewProjectDefaultsToAssociationMode;
    procedure AssociationModeLeavesPopulatedVerseReadyForAssociations;
    procedure EditingModeOpensDestinationOnInitialVerseAndNavigation;
    procedure AssociationModePreservesAutomaticEditingOfEmptyDestination;
    procedure ChangingModeAffectsOnlyNextRealNavigation;
    procedure RefreshOfSameVerseDoesNotReopenClosedEditor;
    procedure RapidNavigationActivatesOnlyFinalVerse;
    procedure AutomaticSuggestionDoesNotConsumePendingEditorRequest;
    procedure FormattedVerseNavigationKeepsEditingDefault;
    procedure FormattedNavigationRetargetsSameEditorWithoutHidingIt;
    procedure DirectAndHistoryNavigationReuseVisibleEditor;
    procedure EditingModeBoundsKeepCurrentEditorSession;
    procedure AutoSaveNavigationNotifiesSettledVerseOnce;
    procedure AutoSaveNavigationUsesSingleCommitAndChapterFastPath;
    procedure CurrentVerseSaveUpdatesExistingChapterSnapshot;
    procedure OneTypedCharacterUsesSingleMutationPipeline;
    procedure AssociationModeManualEditorStillClosesOnNavigation;
    procedure StaleFormattedRefreshCannotCancelEditorAfterRoundTripNavigation;
    procedure InvalidPendingEditBlocksNavigationAndKeepsCurrentEditor;
  end;

implementation

uses
  Classes, SysUtils, Controls, SynEdit, sqlite3conn, sqldb,
  BibleScope, project_database_bootstrap, directwrite_verse_editor, chapterview,
  theword_ont_codec_v2, performance_debug, project_verse_repository;

type
  TVisibleChangeProbe = class
  private
    FChangeCount: Integer;
  public
    procedure Changed(Sender: TObject);
    procedure Reset;
    property ChangeCount: Integer read FChangeCount;
  end;

  TNewVerseProbe = class
  private
    FCalls: Integer;
  public
    procedure Changed(Sender: TProject);
    procedure Reset;
    property Calls: Integer read FCalls;
  end;

procedure TVisibleChangeProbe.Changed(Sender: TObject);
begin
  Inc(FChangeCount);
end;

procedure TVisibleChangeProbe.Reset;
begin
  FChangeCount := 0;
end;

procedure TNewVerseProbe.Changed(Sender: TProject);
begin
  Inc(FCalls);
end;

procedure TNewVerseProbe.Reset;
begin
  FCalls := 0;
end;

function FindWysiwygEditor(AParent: TWinControl):
  TTheWordWysiwygEditorControl;
var
  ChildIndex: Integer;
begin
  Result := nil;
  if not Assigned(AParent) then
    Exit;
  for ChildIndex := 0 to AParent.ControlCount - 1 do
  begin
    if AParent.Controls[ChildIndex] is TTheWordWysiwygEditorControl then
      Exit(TTheWordWysiwygEditorControl(AParent.Controls[ChildIndex]));
    if AParent.Controls[ChildIndex] is TWinControl then
    begin
      Result := FindWysiwygEditor(TWinControl(AParent.Controls[ChildIndex]));
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function FindRawEditor(AParent: TWinControl): TSynEdit;
var
  ChildIndex: Integer;
begin
  Result := nil;
  if not Assigned(AParent) then
    Exit;
  for ChildIndex := 0 to AParent.ControlCount - 1 do
  begin
    if AParent.Controls[ChildIndex] is TSynEdit then
      Exit(TSynEdit(AParent.Controls[ChildIndex]));
    if AParent.Controls[ChildIndex] is TWinControl then
    begin
      Result := FindRawEditor(TWinControl(AParent.Controls[ChildIndex]));
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function FindDirectEditor(AParent: TWinControl): TDirectWriteVerseEditor;
var
  ChildIndex: Integer;
begin
  Result := nil;
  if not Assigned(AParent) then
    Exit;
  for ChildIndex := 0 to AParent.ControlCount - 1 do
  begin
    if AParent.Controls[ChildIndex] is TDirectWriteVerseEditor then
      Exit(TDirectWriteVerseEditor(AParent.Controls[ChildIndex]));
    if AParent.Controls[ChildIndex] is TWinControl then
    begin
      Result := FindDirectEditor(TWinControl(AParent.Controls[ChildIndex]));
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function FindCancelButton(AParent: TWinControl): TButton;
var
  ChildIndex: Integer;
  Candidate: TButton;
begin
  Result := nil;
  if not Assigned(AParent) then
    Exit;
  for ChildIndex := 0 to AParent.ControlCount - 1 do
  begin
    if AParent.Controls[ChildIndex] is TButton then
    begin
      Candidate := TButton(AParent.Controls[ChildIndex]);
      if SameText(Candidate.Caption, 'Cancel') or
         SameText(Candidate.Caption, 'Cancelar') then
        Exit(Candidate);
    end;
    if AParent.Controls[ChildIndex] is TWinControl then
    begin
      Result := FindCancelButton(TWinControl(AParent.Controls[ChildIndex]));
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

procedure SeedVerseTexts(const ADatabaseFile: string;
  const ADestination1, ADestination2, ADestination3: string);
var
  Connection: TSQLite3Connection;
  Destination: string;
  Query: TSQLQuery;
  RowNumber: Integer;
  Transaction: TSQLTransaction;
begin
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := ADatabaseFile;
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text :=
      'update pares set pare_texto_origem = :source, ' +
      'pare_texto_destino = :destination where rowid = :row_number';
    Connection.Open;
    Transaction.StartTransaction;
    for RowNumber := 1 to 3 do
    begin
      case RowNumber of
        1: Destination := ADestination1;
        2: Destination := ADestination2;
      else
        Destination := ADestination3;
      end;
      Query.ParamByName('source').AsString := Format('Source %d', [RowNumber]);
      Query.ParamByName('destination').AsString := Destination;
      Query.ParamByName('row_number').AsInteger := RowNumber;
      Query.ExecSQL;
    end;
    Transaction.Commit;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

function TDefaultVerseModeTests.TempPath(const AFileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + AFileName;
end;

procedure TDefaultVerseModeTests.SetUp;
begin
  inherited SetUp;
  Forms.Application.Initialize;
  FProject := nil;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia_default_verse_mode_%x_%x',
      [PtrUInt(Self), GetTickCount64]);
  ForceDirectories(FTempDir);

  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(0, 0, 900, 500);
  FSourcePanel := TScrollBox.Create(FHost);
  FSourcePanel.Parent := FHost;
  FSourcePanel.SetBounds(0, 0, 900, 240);
  FDestinationPanel := TScrollBox.Create(FHost);
  FDestinationPanel.Parent := FHost;
  FDestinationPanel.SetBounds(0, 250, 900, 240);
  FStatusCombo := TComboBox.Create(FHost);
  FStatusCombo.Parent := FHost;
  FHost.Show;
  Application.ProcessMessages;
end;

procedure TDefaultVerseModeTests.TearDown;
var
  Search: TSearchRec;
begin
  if Assigned(FProject) then
  begin
    FProject.CloseProject(False);
    FreeAndNil(FProject);
  end;
  FHost.Free;

  if FindFirst(TempPath('*'), faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then
        DeleteFile(TempPath(Search.Name));
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(FTempDir);
  inherited TearDown;
end;

procedure TDefaultVerseModeTests.CreateAndOpenProject(
  AMode: TDefaultVerseMode; const ADestination1, ADestination2,
  ADestination3: string);
var
  DatabaseFile: string;
begin
  DatabaseFile := TempPath('default-mode.bib');
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(DatabaseFile, etNT, 3);
  SeedVerseTexts(DatabaseFile, ADestination1, ADestination2, ADestination3);

  FProject := TProject.Create([FSourcePanel, FDestinationPanel], nil,
    FStatusCombo, nil);
  FProject.Escopo := etNT;
  FProject.DefaultVerseMode := AMode;
  FProject.OpenProject(DatabaseFile);
  { The main form applies the global preference after OpenProject has made
    the project active. Mirror that integration boundary here. }
  FProject.RequestDefaultVerseModeForCurrentVerse;
  Application.ProcessMessages;
end;

function TDefaultVerseModeTests.ActiveEditor:
  TTheWordWysiwygEditorControl;
begin
  Result := FindWysiwygEditor(FHost);
  if Assigned(Result) and not Result.Visible then
    Result := nil;
end;

procedure TDefaultVerseModeTests.CancelActiveEditor;
var
  CancelButton: TButton;
begin
  CancelButton := FindCancelButton(FHost);
  AssertTrue('the active editor must expose its Cancel button',
    Assigned(CancelButton));
  CancelButton.Click;
  Application.ProcessMessages;
end;

procedure TDefaultVerseModeTests.IntegerValuesUseAssociationAsSafeFallback;
begin
  AssertEquals('association value', Ord(dvmAssociation),
    Ord(DefaultVerseModeFromInteger(Ord(dvmAssociation))));
  AssertEquals('editing value', Ord(dvmEditing),
    Ord(DefaultVerseModeFromInteger(Ord(dvmEditing))));
  AssertEquals('negative values fall back to association', Ord(dvmAssociation),
    Ord(DefaultVerseModeFromInteger(-1)));
  AssertEquals('future values fall back to association', Ord(dvmAssociation),
    Ord(DefaultVerseModeFromInteger(High(Integer))));
end;

procedure TDefaultVerseModeTests.NewProjectDefaultsToAssociationMode;
begin
  FProject := TProject.Create;
  AssertEquals(Ord(dvmAssociation), Ord(FProject.DefaultVerseMode));
end;

procedure TDefaultVerseModeTests.
  AssociationModeLeavesPopulatedVerseReadyForAssociations;
begin
  CreateAndOpenProject(dvmAssociation);

  AssertEquals('40,1,1', FProject.ID);
  AssertNull('a populated destination must remain in Association mode',
    ActiveEditor);
end;

procedure TDefaultVerseModeTests.
  EditingModeOpensDestinationOnInitialVerseAndNavigation;
var
  Editor: TTheWordWysiwygEditorControl;
begin
  CreateAndOpenProject(dvmEditing);

  Editor := ActiveEditor;
  AssertTrue('the initial destination must open in the WYSIWYG',
    Assigned(Editor));
  AssertEquals('Destination 1', UTF8Encode(Editor.Text));
  AssertEquals(FProject.ID,
    FProject.VerseObjects[tbDestino].ReferenceID);

  FProject.NextVerse;
  Application.ProcessMessages;

  Editor := ActiveEditor;
  AssertEquals('40,1,2', FProject.ID);
  AssertTrue('the destination editor must follow real navigation',
    Assigned(Editor));
  AssertEquals('Destination 2', UTF8Encode(Editor.Text));
  AssertEquals(FProject.ID,
    FProject.VerseObjects[tbDestino].ReferenceID);
end;

procedure TDefaultVerseModeTests.
  AssociationModePreservesAutomaticEditingOfEmptyDestination;
var
  Editor: TTheWordWysiwygEditorControl;
begin
  CreateAndOpenProject(dvmAssociation, '', 'Destination 2', 'Destination 3');

  Editor := ActiveEditor;
  AssertTrue('the legacy empty-destination editor must remain available',
    Assigned(Editor));
  AssertEquals('', UTF8Encode(Editor.Text));
end;

procedure TDefaultVerseModeTests.ChangingModeAffectsOnlyNextRealNavigation;
begin
  CreateAndOpenProject(dvmAssociation);
  AssertNull(ActiveEditor);

  FProject.DefaultVerseMode := dvmEditing;
  Application.ProcessMessages;
  AssertNull('changing the preference must not interrupt the current verse',
    ActiveEditor);

  FProject.NextVerse;
  Application.ProcessMessages;
  AssertTrue('the changed preference applies on the next verse',
    Assigned(ActiveEditor));
  AssertEquals('Destination 2', UTF8Encode(ActiveEditor.Text));
end;

procedure TDefaultVerseModeTests.RefreshOfSameVerseDoesNotReopenClosedEditor;
begin
  CreateAndOpenProject(dvmEditing);
  AssertTrue(Assigned(ActiveEditor));

  CancelActiveEditor;
  AssertNull('Cancel must close the current editing session', ActiveEditor);

  FProject.RefreshUI;
  Application.ProcessMessages;
  AssertNull('refreshing the same reference must not reopen the editor',
    ActiveEditor);
end;

procedure TDefaultVerseModeTests.RapidNavigationActivatesOnlyFinalVerse;
begin
  CreateAndOpenProject(dvmEditing);

  FProject.NextVerse;
  FProject.NextVerse;
  Application.ProcessMessages;

  AssertEquals('40,1,3', FProject.ID);
  AssertTrue('one editor must be active for the settled reference',
    Assigned(ActiveEditor));
  AssertEquals('Destination 3', UTF8Encode(ActiveEditor.Text));
  AssertEquals(FProject.ID,
    FProject.VerseObjects[tbDestino].ReferenceID);
end;

procedure TDefaultVerseModeTests.
  AutomaticSuggestionDoesNotConsumePendingEditorRequest;
begin
  CreateAndOpenProject(dvmEditing);
  CancelActiveEditor;
  FProject.SugerirAssociacaoAutomaticamente := True;

  FProject.NextVerse;
  Application.ProcessMessages;

  AssertEquals('40,1,2', FProject.ID);
  AssertTrue('post-navigation suggestion work must not consume the pending editor request',
    Assigned(ActiveEditor));
  AssertEquals('Destination 2', UTF8Encode(ActiveEditor.Text));
end;

procedure TDefaultVerseModeTests.
  FormattedVerseNavigationKeepsEditingDefault;
var
  Chapter: TChapterView;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
begin
  CreateAndOpenProject(dvmEditing);
  FProject.AutoSave := True;
  Chapter := TChapterView.Create(FHost);
  Chapter.Parent := FHost;
  Chapter.Project := FProject;
  try
    Editor := ActiveEditor;
    AssertTrue(Assigned(Editor));
    DirectEditor := FindDirectEditor(Editor);
    AssertTrue(Assigned(DirectEditor));
    DirectEditor.SetSelection(0, Length('Destination'));
    AssertTrue('formatting must create a pending visual edit',
      Editor.ApplySemantic(twsmHtmlBold));

    FProject.NextVerse;
    Application.ProcessMessages;

    AssertEquals('40,1,2', FProject.ID);
    AssertTrue('saving formatting during navigation must still open the next Verse editor',
      Assigned(ActiveEditor));
    AssertEquals('Destination 2', UTF8Encode(ActiveEditor.Text));
  finally
    Chapter.Project := nil;
    Chapter.Free;
  end;
end;

procedure TDefaultVerseModeTests.
  FormattedNavigationRetargetsSameEditorWithoutHidingIt;
var
  DirectEditor: TDirectWriteVerseEditor;
  EditorAfter: TTheWordWysiwygEditorControl;
  EditorBefore: TTheWordWysiwygEditorControl;
  EditorPanel: TControl;
  Visibility: TVisibleChangeProbe;
begin
  CreateAndOpenProject(dvmEditing);
  FProject.AutoSave := True;
  EditorBefore := ActiveEditor;
  AssertTrue('precondition: the first destination editor is visible',
    Assigned(EditorBefore));
  EditorPanel := EditorBefore.Parent;
  AssertTrue('the editor must have a persistent overlay panel',
    Assigned(EditorPanel));
  DirectEditor := FindDirectEditor(EditorBefore);
  AssertTrue(Assigned(DirectEditor));
  DirectEditor.SetSelection(0, Length('Destination'));
  AssertTrue('formatting must create a pending edit',
    EditorBefore.ApplySemantic(twsmHtmlBold));

  Visibility := TVisibleChangeProbe.Create;
  try
    EditorPanel.AddHandlerOnVisibleChanged(@Visibility.Changed, True);
    EditorBefore.AddHandlerOnVisibleChanged(@Visibility.Changed, True);

    FProject.NextVerse;

    EditorAfter := ActiveEditor;
    AssertEquals('40,1,2', FProject.ID);
    AssertSame('retargeting must reuse the already visible editor',
      EditorBefore, EditorAfter);
    AssertEquals('the target text must be installed synchronously',
      'Destination 2', UTF8Encode(EditorAfter.Text));
    AssertEquals('the target Verse must already match the project reference',
      FProject.ID, FProject.VerseObjects[tbDestino].ReferenceID);
    AssertEquals('retargeting must emit no visibility changes', 0,
      Visibility.ChangeCount);
    AssertEquals('the target must not inherit the old selection', 0,
      FindDirectEditor(EditorAfter).SelectionLength);

    Application.ProcessMessages;

    AssertSame('queued callbacks must not replace the retargeted editor',
      EditorBefore, ActiveEditor);
    AssertEquals('queued callbacks must not change visibility later', 0,
      Visibility.ChangeCount);

    FProject.PreviousVerse;
    AssertSame('returning to the formatted Verse must reuse the editor',
      EditorBefore, ActiveEditor);
    AssertEquals('the formatted text must be committed before navigation',
      '<b>Destination</b> 1', UTF8Encode(ActiveEditor.Text));
    AssertEquals('round-trip navigation must emit no visibility changes',
      0, Visibility.ChangeCount);
  finally
    EditorBefore.RemoveHandlerOnVisibleChanged(@Visibility.Changed);
    EditorPanel.RemoveHandlerOnVisibleChanged(@Visibility.Changed);
    Visibility.Free;
  end;
end;

procedure TDefaultVerseModeTests.EditingModeBoundsKeepCurrentEditorSession;
var
  Editor: TTheWordWysiwygEditorControl;
  EditorPanel: TControl;
  Visibility: TVisibleChangeProbe;
begin
  CreateAndOpenProject(dvmEditing);
  Editor := ActiveEditor;
  AssertTrue(Assigned(Editor));
  EditorPanel := Editor.Parent;
  Visibility := TVisibleChangeProbe.Create;
  try
    EditorPanel.AddHandlerOnVisibleChanged(@Visibility.Changed, True);
    Editor.AddHandlerOnVisibleChanged(@Visibility.Changed, True);

    FProject.PreviousVerse;
    Application.ProcessMessages;
    AssertEquals('moving before the first Verse must keep its reference',
      '40,1,1', FProject.ID);
    AssertSame('BOF must keep the current editing session', Editor,
      ActiveEditor);
    AssertEquals('BOF must not change editing-surface visibility', 0,
      Visibility.ChangeCount);

    FProject.LastVerse;
    Application.ProcessMessages;
    AssertEquals('40,1,3', FProject.ID);
    AssertSame('real navigation must still reuse the editor', Editor,
      ActiveEditor);
    Visibility.Reset;

    FProject.NextVerse;
    Application.ProcessMessages;
    FProject.NextVerse;
    Application.ProcessMessages;
    AssertEquals('moving beyond the last Verse must keep its reference',
      '40,1,3', FProject.ID);
    AssertSame('EOF must keep the current editing session', Editor,
      ActiveEditor);
    AssertEquals('EOF must not change editing-surface visibility', 0,
      Visibility.ChangeCount);
  finally
    Editor.RemoveHandlerOnVisibleChanged(@Visibility.Changed);
    EditorPanel.RemoveHandlerOnVisibleChanged(@Visibility.Changed);
    Visibility.Free;
  end;
end;

procedure TDefaultVerseModeTests.
  DirectAndHistoryNavigationReuseVisibleEditor;
var
  Editor: TTheWordWysiwygEditorControl;
  EditorPanel: TControl;
  Visibility: TVisibleChangeProbe;
begin
  CreateAndOpenProject(dvmEditing);
  Editor := ActiveEditor;
  AssertTrue(Assigned(Editor));
  EditorPanel := Editor.Parent;
  Visibility := TVisibleChangeProbe.Create;
  try
    EditorPanel.AddHandlerOnVisibleChanged(@Visibility.Changed, True);
    Editor.AddHandlerOnVisibleChanged(@Visibility.Changed, True);

    FProject.NavigateToReference('40,1,3');
    AssertEquals('direct navigation must settle synchronously',
      '40,1,3', FProject.ID);
    AssertSame('tree and hyperlink navigation must reuse the editor',
      Editor, ActiveEditor);
    AssertEquals('Destination 3', UTF8Encode(ActiveEditor.Text));

    FProject.GoBack;
    AssertEquals('history Back must restore the previous reference',
      '40,1,1', FProject.ID);
    AssertSame('history Back must reuse the editor', Editor, ActiveEditor);
    AssertEquals('Destination 1', UTF8Encode(ActiveEditor.Text));

    FProject.GoForward;
    AssertEquals('history Forward must restore the later reference',
      '40,1,3', FProject.ID);
    AssertSame('history Forward must reuse the editor', Editor, ActiveEditor);
    AssertEquals('Destination 3', UTF8Encode(ActiveEditor.Text));
    AssertEquals('direct and history navigation must never change visibility',
      0, Visibility.ChangeCount);
  finally
    Editor.RemoveHandlerOnVisibleChanged(@Visibility.Changed);
    EditorPanel.RemoveHandlerOnVisibleChanged(@Visibility.Changed);
    Visibility.Free;
  end;
end;

procedure TDefaultVerseModeTests.AutoSaveNavigationNotifiesSettledVerseOnce;
var
  Probe: TNewVerseProbe;
begin
  CreateAndOpenProject(dvmEditing);
  FProject.AutoSave := True;
  Probe := TNewVerseProbe.Create;
  try
    FProject.OnNewVerseSubscribe(@Probe.Changed);
    Probe.Reset;

    FProject.NextVerse;
    Application.ProcessMessages;

    AssertEquals('autosave must not issue a duplicate Verse notification',
      1, Probe.Calls);
    AssertTrue('the settled destination editor must remain active',
      Assigned(ActiveEditor));
    AssertEquals('Destination 2', UTF8Encode(ActiveEditor.Text));
  finally
    FProject.OnNewVerseUnsubscribe(@Probe.Changed);
    Probe.Free;
  end;
end;

procedure TDefaultVerseModeTests.
  AutoSaveNavigationUsesSingleCommitAndChapterFastPath;
var
  Chapter: TChapterView;
  Counters: TIBibliaPerfCounterValues;
begin
  CreateAndOpenProject(dvmEditing);
  FProject.AutoSave := True;
  Chapter := TChapterView.Create(FHost);
  Chapter.Parent := FHost;
  Chapter.Project := FProject;
  Chapter.LoadChapter;
  IBibliaPerfSetEnabledForTests(True);
  try
    IBibliaPerfReset;
    FProject.NextVerse;
    Application.ProcessMessages;
    Counters := IBibliaPerfSnapshot;
    AssertEquals('one SaveBeforeScroll', 1,
      Int64(Counters[ipcSaveBeforeScrollCalls]));
    AssertEquals('one persistence commit', 1,
      Int64(Counters[ipcPersistenceCommits]));
    AssertEquals('same chapter does not rebuild', 0,
      Int64(Counters[ipcChapterRenders]));
    AssertEquals('navigation does not traverse an absent tree', 0,
      Int64(Counters[ipcTreeTraversals]));
  finally
    IBibliaPerfSetEnabledForTests(False);
    Chapter.Project := nil;
    Chapter.Free;
  end;
end;

procedure TDefaultVerseModeTests.
  CurrentVerseSaveUpdatesExistingChapterSnapshot;
var
  SnapshotAfter: TChapterSnapshot;
  SnapshotBefore: TChapterSnapshot;
  SnapshotEntry: TChapterSnapshotEntry;
begin
  CreateAndOpenProject(dvmAssociation);
  SnapshotBefore := FProject.GetChapterSnapshot;
  AssertNotNull('precondition: chapter snapshot', SnapshotBefore);

  FProject.ApplyVerseText('Changed<RF>note<Rf>', tbDestino, True);
  SnapshotAfter := FProject.GetChapterSnapshot;

  AssertSame('a normal Verse save patches the existing chapter object',
    SnapshotBefore, SnapshotAfter);
  SnapshotEntry := SnapshotAfter.Entry(0);
  AssertEquals('the snapshot immediately exposes the saved destination',
    'Changed<RF>note<Rf>', SnapshotEntry.Texts[1]);
  SnapshotEntry := SnapshotAfter.Entry(1);
  AssertEquals('later Verses immediately receive the rebuilt RF prefix',
    1, SnapshotEntry.AutomaticNoteStarts[1]);
end;

procedure TDefaultVerseModeTests.OneTypedCharacterUsesSingleMutationPipeline;
var
  Counters: TIBibliaPerfCounterValues;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
begin
  CreateAndOpenProject(dvmEditing);
  Editor := ActiveEditor;
  AssertNotNull('active WYSIWYG editor', Editor);
  DirectEditor := FindDirectEditor(Editor);
  AssertNotNull('direct editor', DirectEditor);
  DirectEditor.SetSelection(Length(DirectEditor.Document.Text), 0);
  IBibliaPerfSetEnabledForTests(True);
  try
    IBibliaPerfReset;
    DirectEditor.SimulateTextInputFixture('X');
    Counters := IBibliaPerfSnapshot;
    AssertEquals('typed text', 'Destination 1X', UTF8Encode(Editor.Text));
    AssertEquals('one encode', 1,
      Int64(Counters[ipcWysiwygEncodes]));
    AssertEquals('one decode', 1,
      Int64(Counters[ipcWysiwygDecodes]));
    AssertEquals('one visual rebuild', 1,
      Int64(Counters[ipcWysiwygVisualBuilds]));
    AssertEquals('command state is computed once with the final caret', 1,
      Int64(Counters[ipcCommandStateBuilds]));
    AssertTrue('one undo entry', Editor.Undo);
    AssertFalse('no second undo entry', Editor.Undo);
  finally
    IBibliaPerfSetEnabledForTests(False);
  end;
end;

procedure TDefaultVerseModeTests.
  AssociationModeManualEditorStillClosesOnNavigation;
var
  DestinationPanel: TScrollBox;
  Editor: TTheWordWysiwygEditorControl;
begin
  CreateAndOpenProject(dvmAssociation);
  DestinationPanel := FProject.VerseObjects[tbDestino].Panel;
  AssertTrue('the destination panel must expose manual WYSIWYG activation',
    Assigned(DestinationPanel) and Assigned(DestinationPanel.OnDblClick));
  DestinationPanel.OnDblClick(DestinationPanel);
  Application.ProcessMessages;
  Editor := ActiveEditor;
  AssertTrue('double click must open the manual editor', Assigned(Editor));

  FProject.NextVerse;
  Application.ProcessMessages;

  AssertEquals('40,1,2', FProject.ID);
  AssertNull('Association mode must not carry manual editing to the next Verse',
    ActiveEditor);
  AssertFalse('the previous manual editor must have been hidden',
    Editor.Visible);
end;

procedure TDefaultVerseModeTests.
  StaleFormattedRefreshCannotCancelEditorAfterRoundTripNavigation;
var
  Chapter: TChapterView;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
begin
  CreateAndOpenProject(dvmEditing);
  FProject.AutoSave := True;
  Chapter := TChapterView.Create(FHost);
  Chapter.Parent := FHost;
  Chapter.Project := FProject;
  try
    Editor := ActiveEditor;
    AssertTrue(Assigned(Editor));
    DirectEditor := FindDirectEditor(Editor);
    AssertTrue(Assigned(DirectEditor));
    DirectEditor.SetSelection(0, Length('Destination'));
    AssertTrue(Editor.ApplySemantic(twsmHtmlBold));

    { Keep both navigations ahead of the async observer dispatch. A notification
      from the first visit to verse 1 must not be accepted merely because the
      project has already returned to the same textual reference. }
    FProject.NextVerse;
    FProject.PreviousVerse;
    Application.ProcessMessages;

    AssertEquals('40,1,1', FProject.ID);
    AssertEquals('<b>Destination</b> 1',
      FProject.VerseObjects[tbDestino].XML);
    AssertTrue('an obsolete A-B-A refresh must not cancel the current editor request',
      Assigned(ActiveEditor));
    AssertEquals('<b>Destination</b> 1', UTF8Encode(ActiveEditor.Text));
  finally
    Chapter.Project := nil;
    Chapter.Free;
  end;
end;

procedure TDefaultVerseModeTests.
  InvalidPendingEditBlocksNavigationAndKeepsCurrentEditor;
var
  Editor: TTheWordWysiwygEditorControl;
  EditorPanel: TControl;
  RawEditor: TSynEdit;
  Visibility: TVisibleChangeProbe;
begin
  CreateAndOpenProject(dvmEditing, 'Destination 1<WG1>',
    'Destination 2', 'Destination 3');
  Editor := ActiveEditor;
  AssertTrue(Assigned(Editor));
  EditorPanel := Editor.Parent;
  Editor.RawMode := True;
  RawEditor := FindRawEditor(Editor);
  AssertTrue('the Raw editor must be available', Assigned(RawEditor));
  RawEditor.Text := 'Destination 1<WG2>';

  Visibility := TVisibleChangeProbe.Create;
  try
    EditorPanel.AddHandlerOnVisibleChanged(@Visibility.Changed, True);
    Editor.AddHandlerOnVisibleChanged(@Visibility.Changed, True);
    try
      FProject.NextVerse;
    except
      on EAbort do
        ;
    end;
    Application.ProcessMessages;

    AssertEquals('failed validation must keep the original reference',
      '40,1,1', FProject.ID);
    AssertSame('failed validation must keep the same editor open',
      Editor, ActiveEditor);
    AssertEquals('failed validation must not change editor visibility', 0,
      Visibility.ChangeCount);
    AssertEquals('the persisted Verse must remain unchanged',
      'Destination 1<WG1>', FProject.VerseObjects[tbDestino].XML);
    AssertEquals('the pending Raw edit must remain available',
      'Destination 1<WG2>', RawEditor.Lines[0]);
  finally
    Editor.RemoveHandlerOnVisibleChanged(@Visibility.Changed);
    EditorPanel.RemoveHandlerOnVisibleChanged(@Visibility.Changed);
    Visibility.Free;
  end;
end;

initialization
  RegisterTest(TDefaultVerseModeTests);

end.
