unit project_save_as_form_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Menus, ActnList, ComCtrls, StdCtrls,
  fpcunit, testregistry, Project, formPrincipal;

type
  TScriptedProjectSaveDialog = class(TSaveDialog)
  public
    Accepted, GuardActive, SaveBlocked, ProbeSave: Boolean;
    SelectedFileName, ConfiguredFileName, ConfiguredInitialDir: string;
    ExecuteCount: Integer;
    function Execute: Boolean; override;
  end;

  TProjectSaveAsFormTests = class(TTestCase)
  private
    FTempDir, FSourceFileName: string;
    FPreviousProject, FProject: TProject;
    FHost: TForm;
    FSourcePanel, FDestinationPanel: TScrollBox;
    FStatusCombo: TComboBox;
    FForm: TMainForm;
    FDialog: TScriptedProjectSaveDialog;
    function TempPath(const Name: string): string;
    function ReadDestination(const FileName: string): string;
    procedure CreateFormControls;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CancelProtectsPendingChangesBeforeDialog;
    procedure SuccessUpdatesTitleAndPreservesOriginalInRecentProjects;
    procedure SaveAsActionFollowsProjectAndOperationState;
    procedure EmbeddedFormWiresMenuToolbarShortcutAndHandlers;
  end;

implementation

uses
  BibleScope, project_database_bootstrap, sqlite3conn, sqldb, LResources;

function TScriptedProjectSaveDialog.Execute: Boolean;
begin
  Inc(ExecuteCount);
  GuardActive := Assigned(CurrentProject) and CurrentProject.SaveAsInProgress;
  ConfiguredFileName := FileName;
  ConfiguredInitialDir := InitialDir;
  if ProbeSave then
    SaveBlocked := not CurrentProject.SaveProject;
  if SelectedFileName <> '' then
    FileName := SelectedFileName;
  Result := Accepted;
end;

function TProjectSaveAsFormTests.TempPath(const Name: string): string;
begin
  Result := IncludeTrailingPathDelimiter(FTempDir) + Name;
end;

function TProjectSaveAsFormTests.ReadDestination(const FileName: string): string;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
  Query: TSQLQuery;
begin
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  Query := TSQLQuery.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FileName;
    Connection.Open;
    Query.Database := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text := 'select pare_texto_destino from pares order by pare_id limit 1';
    Query.Open;
    Result := Query.Fields[0].AsString;
  finally
    Query.Free;
    Transaction.Free;
    Connection.Free;
  end;
end;

procedure TProjectSaveAsFormTests.CreateFormControls;
begin
  FForm := TMainForm.CreateNew(nil);
  FForm.ActionSaveProjectAs := TAction.Create(FForm);
  FForm.MenuItemAutoSave := TMenuItem.Create(FForm);
  FForm.MenuItemBoldStrongs := TMenuItem.Create(FForm);
  FForm.MenuItemAlwaysOnTop := TMenuItem.Create(FForm);
  FForm.MenuItemStatisticalSuggestion := TMenuItem.Create(FForm);
  FForm.MenuItemAISuggestion := TMenuItem.Create(FForm);
  FForm.MenuItemSyncTheWord := TMenuItem.Create(FForm);
  FForm.MenuItemSynciBiblia := TMenuItem.Create(FForm);
  FForm.MenuItemRecent := TMenuItem.Create(FForm);
  FForm.StatusBar1 := TStatusBar.Create(FForm);
  FForm.OpenDialog1 := TOpenDialog.Create(FForm);
  FForm.OpenDialog1.Filter := 'Project files (*.bib)|*.bib';
  FDialog := TScriptedProjectSaveDialog.Create(FForm);
  FForm.SaveDialog1 := FDialog;
end;

procedure TProjectSaveAsFormTests.SetUp;
var
  Connection: TSQLite3Connection;
  Transaction: TSQLTransaction;
begin
  inherited SetUp;
  Forms.Application.Initialize;
  FPreviousProject := CurrentProject;
  FTempDir := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('ibiblia_save_as_form_%x_%x', [PtrUInt(Self), GetTickCount64]);
  ForceDirectories(FTempDir);
  FSourceFileName := TempPath('original.bib');
  TProjectDatabaseBootstrap.CreateProjectDatabaseFixture(FSourceFileName, etNT, 1);
  Connection := TSQLite3Connection.Create(nil);
  Transaction := TSQLTransaction.Create(nil);
  try
    Connection.Transaction := Transaction;
    Connection.DatabaseName := FSourceFileName;
    Connection.Open;
    Transaction.StartTransaction;
    Connection.ExecuteDirect('update pares set pare_texto_origem = ''Source'', ' +
      'pare_texto_destino = ''Destination''');
    Transaction.Commit;
  finally
    Transaction.Free;
    Connection.Free;
  end;
  FHost := TForm.CreateNew(nil);
  FSourcePanel := TScrollBox.Create(FHost);
  FSourcePanel.Parent := FHost;
  FDestinationPanel := TScrollBox.Create(FHost);
  FDestinationPanel.Parent := FHost;
  FStatusCombo := TComboBox.Create(FHost);
  FStatusCombo.Parent := FHost;
  FProject := TProject.Create([FSourcePanel, FDestinationPanel], nil,
    FStatusCombo, nil);
  FProject.Escopo := etNT;
  FProject.OpenProject(FSourceFileName);
  CurrentProject := FProject;
  CreateFormControls;
end;

procedure TProjectSaveAsFormTests.TearDown;
var
  Search: TSearchRec;
begin
  CurrentProject := FPreviousProject;
  if Assigned(FProject) then
  begin
    FProject.CloseProject(False);
    FreeAndNil(FProject);
  end;
  FForm.Free;
  FHost.Free;
  if FindFirst(TempPath('*'), faAnyFile, Search) = 0 then
  try
    repeat
      if (Search.Attr and faDirectory) = 0 then
        DeleteFile(TempPath(Search.Name));
    until FindNext(Search) <> 0;
  finally
    FindClose(Search);
  end;
  RemoveDir(FTempDir);
  inherited TearDown;
end;

procedure TProjectSaveAsFormTests.CancelProtectsPendingChangesBeforeDialog;
var
  Recent: TMenuItem;
begin
  Recent := TMenuItem.Create(FForm.MenuItemRecent);
  Recent.Caption := 'unchanged recent.bib';
  FForm.MenuItemRecent.Add(Recent);
  FProject.VerseObjects[tbDestino].Text := 'Pending destination';
  AssertTrue('Fixture must contain a pending edit', FProject.Modificado);
  FDialog.Accepted := False;
  FDialog.ProbeSave := True;
  FForm.ActionSaveProjectAsExecute(nil);
  AssertEquals(1, FDialog.ExecuteCount);
  AssertTrue('Save As guard must precede the dialog', FDialog.GuardActive);
  AssertTrue('A save callback cannot commit while the dialog is open', FDialog.SaveBlocked);
  AssertFalse('Cancel releases the guard', FProject.SaveAsInProgress);
  AssertEquals(FSourceFileName, FProject.Caminho);
  AssertEquals('original.bib', FDialog.ConfiguredFileName);
  AssertEquals(FTempDir, FDialog.ConfiguredInitialDir);
  AssertEquals('.bib', FDialog.DefaultExt);
  AssertTrue('Native overwrite confirmation is enabled', ofOverwritePrompt in FDialog.Options);
  AssertTrue('Cancel leaves the edit pending', FProject.Modificado);
  AssertEquals('Destination', ReadDestination(FSourceFileName));
  AssertEquals(1, FForm.MenuItemRecent.Count);
  AssertEquals('unchanged recent.bib', FForm.MenuItemRecent.Items[0].Caption);
end;

procedure TProjectSaveAsFormTests.SuccessUpdatesTitleAndPreservesOriginalInRecentProjects;
var
  Destination: string;
begin
  Destination := TempPath('cópia.bib');
  FProject.VerseObjects[tbDestino].Text := 'Pending destination';
  FDialog.Accepted := True;
  FDialog.SelectedFileName := Destination;
  FForm.ActionSaveProjectAsExecute(nil);
  AssertTrue(FDialog.GuardActive);
  AssertFalse(FProject.SaveAsInProgress);
  AssertEquals(Destination, FProject.Caminho);
  AssertEquals('Destination', ReadDestination(FSourceFileName));
  AssertEquals('Pending destination', ReadDestination(Destination));
  AssertTrue('Title identifies the new file', Pos(ExtractFileName(Destination),
    FForm.Caption) > 0);
  AssertEquals(2, FForm.MenuItemRecent.Count);
  AssertEquals(Destination, FForm.MenuItemRecent.Items[0].Caption);
  AssertEquals(FSourceFileName, FForm.MenuItemRecent.Items[1].Caption);
  AssertFalse('The copy includes the pending edit', FProject.Modificado);
  AssertTrue(FForm.ActionSaveProjectAs.Enabled);
end;

procedure TProjectSaveAsFormTests.SaveAsActionFollowsProjectAndOperationState;
begin
  FForm.ActionSaveProjectAsUpdate(nil);
  AssertTrue(FForm.ActionSaveProjectAs.Enabled);
  AssertTrue(FProject.BeginSaveAs);
  try
    FForm.ActionSaveProjectAsUpdate(nil);
    AssertFalse('Operation prevents a second Save As', FForm.ActionSaveProjectAs.Enabled);
  finally
    FProject.EndSaveAs;
  end;
  CurrentProject := nil;
  FForm.ActionSaveProjectAsUpdate(nil);
  AssertFalse('A project is required', FForm.ActionSaveProjectAs.Enabled);
  CurrentProject := FProject;
  FForm.ActionSaveProjectAsUpdate(nil);
  AssertTrue(FForm.ActionSaveProjectAs.Enabled);
end;

procedure TProjectSaveAsFormTests.EmbeddedFormWiresMenuToolbarShortcutAndHandlers;
var
  ResourceForm: TMainForm;
  ExpectedExecute, ExpectedUpdate: TNotifyEvent;
begin
  CurrentProject := nil;
  ResourceForm := TMainForm.CreateNew(nil);
  try
    // CreateNew has already completed AfterConstruction. Loading the resource
    // now binds the real controls and events without running FormCreate.
    AssertTrue('The compiled main-form resource loads',
      InitResourceComponent(ResourceForm, TForm));
    ResourceForm.OnCreate := nil;
    ResourceForm.OnDestroy := nil;
    ResourceForm.OnShow := nil;
    ResourceForm.OnClose := nil;

    AssertTrue('Project menu exposes the Save As action',
      ResourceForm.MenuItemSaveProjectAs.Action = ResourceForm.ActionSaveProjectAs);
    AssertTrue('Save As belongs to the Project menu',
      ResourceForm.MenuItemSaveProjectAs.Parent = ResourceForm.MenuItem1);
    AssertEquals('Save As follows Save', ResourceForm.MenuItem9.MenuIndex + 1,
      ResourceForm.MenuItemSaveProjectAs.MenuIndex);
    AssertTrue('The existing toolbar button uses the same action',
      ResourceForm.ToolButton5.Action = ResourceForm.ActionSaveProjectAs);
    AssertEquals('Ctrl+Shift+S is configured in the real form resource',
      Integer(ShortCut(Ord('S'), [ssCtrl, ssShift])),
      Integer(ResourceForm.ActionSaveProjectAs.ShortCut));

    ExpectedExecute := @ResourceForm.ActionSaveProjectAsExecute;
    ExpectedUpdate := @ResourceForm.ActionSaveProjectAsUpdate;
    AssertTrue('Execute handler is wired',
      (TMethod(ResourceForm.ActionSaveProjectAs.OnExecute).Code =
        TMethod(ExpectedExecute).Code) and
      (TMethod(ResourceForm.ActionSaveProjectAs.OnExecute).Data = Pointer(ResourceForm)));
    AssertTrue('Availability handler is wired',
      (TMethod(ResourceForm.ActionSaveProjectAs.OnUpdate).Code =
        TMethod(ExpectedUpdate).Code) and
      (TMethod(ResourceForm.ActionSaveProjectAs.OnUpdate).Data = Pointer(ResourceForm)));
    ResourceForm.ActionSaveProjectAs.Update;
    AssertFalse('No project disables the real action',
      ResourceForm.ActionSaveProjectAs.Enabled);

    CurrentProject := FProject;
    ResourceForm.ActionSaveProjectAs.Update;
    AssertTrue('Opening a project enables the real action',
      ResourceForm.ActionSaveProjectAs.Enabled);
    ResourceForm.SaveDialog1 := FDialog;
    FDialog.Accepted := False;
    ResourceForm.MenuItemSaveProjectAs.Click;
    AssertEquals('The real menu reaches the Save As dialog', 1, FDialog.ExecuteCount);
    AssertTrue('The menu also establishes the operation guard', FDialog.GuardActive);
    AssertFalse('Cancel through the real menu releases the guard',
      FProject.SaveAsInProgress);
    AssertEquals(FSourceFileName, FProject.Caminho);
  finally
    CurrentProject := FProject;
    // Always clear teardown handlers, including when resource loading fails.
    ResourceForm.OnCreate := nil;
    ResourceForm.OnDestroy := nil;
    ResourceForm.OnShow := nil;
    ResourceForm.OnClose := nil;
    ResourceForm.Free;
  end;
end;

initialization
  RegisterTest(TProjectSaveAsFormTests);

end.
