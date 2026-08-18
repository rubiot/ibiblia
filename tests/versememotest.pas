unit versememotest;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry;

type
  TVerseMemoTests = class(TTestCase)
  published
    procedure ActivateEmptyVerseKeepsEditorAttachedToWindow;
    procedure FailedSaveCallbackKeepsPendingEditorOpen;
    procedure FailedVisualSaveKeepsConcreteErrorVisible;
    procedure PostCommitObserverFailureStillClosesAppliedEdit;
    procedure RawSaveCannotChangeProtectedLexicalObjects;
  end;

implementation

uses
  SysUtils, Forms, Controls, ExtCtrls, StdCtrls, SynEdit, Verse, VerseMemo, Project,
  directwrite_verse_editor, theword_ont_codec_v2,
  theword_wysiwyg_editor_control, text_edit_provenance;

type
  TTestableProject = class;

  TTracedSaveProbe = class
  private
    FApply: Boolean;
    FCalls: Integer;
    FCurrentText: UnicodeString;
    FErrorMessage: string;
    FMode: TVerseEditTraceMode;
    FPostCommitProject: TTestableProject;
    FTokenizerVersion: Integer;
  public
    function Save(Sender: TVerseMemo;
      const ATrace: TVerseEditTrace;
      out AImpact: TAssociationEditImpact): Boolean;
    property Apply: Boolean read FApply write FApply;
    property Calls: Integer read FCalls;
    property CurrentText: UnicodeString read FCurrentText;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property Mode: TVerseEditTraceMode read FMode;
    property PostCommitProject: TTestableProject read FPostCommitProject
      write FPostCommitProject;
    property TokenizerVersion: Integer read FTokenizerVersion;
  end;

  TPostCommitObserverProbe = class
  private
    FFailedCalls: Integer;
    FSucceededCalls: Integer;
  public
    procedure Fail(Sender: TProject);
    procedure Succeed(Sender: TProject);
    property FailedCalls: Integer read FFailedCalls;
    property SucceededCalls: Integer read FSucceededCalls;
  end;

  TTestableProject = class(TProject)
  public
    procedure NotifyAfterEdit;
  end;

function TTracedSaveProbe.Save(Sender: TVerseMemo;
  const ATrace: TVerseEditTrace;
  out AImpact: TAssociationEditImpact): Boolean;
begin
  Inc(FCalls);
  FCurrentText := ATrace.CurrentText;
  FMode := ATrace.Mode;
  FTokenizerVersion := ATrace.TokenizerVersion;
  AImpact.PreservedMembers := 0;
  AImpact.RemovedMembers := 0;
  AImpact.UsedLegacyFallback := False;
  AImpact.ErrorMessage := FErrorMessage;
  Result := FApply and Sender.Verse.TryApplyTracedEdit(Sender.Text,
    ATrace, AImpact);
  if Result and Assigned(FPostCommitProject) then
    FPostCommitProject.NotifyAfterEdit;
end;

procedure TPostCommitObserverProbe.Fail(Sender: TProject);
begin
  Inc(FFailedCalls);
  raise Exception.Create('deliberate post-commit observer failure');
end;

procedure TPostCommitObserverProbe.Succeed(Sender: TProject);
begin
  Inc(FSucceededCalls);
end;

procedure TTestableProject.NotifyAfterEdit;
begin
  NotifyNewVerseObserversAfterEdit;
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

function FindLabelWithCaption(AParent: TWinControl;
  const ACaption: string): TLabel;
var
  ChildIndex: Integer;
begin
  Result := nil;
  if not Assigned(AParent) then
    Exit;
  for ChildIndex := 0 to AParent.ControlCount - 1 do
  begin
    if (AParent.Controls[ChildIndex] is TLabel) and
       (TLabel(AParent.Controls[ChildIndex]).Caption = ACaption) then
      Exit(TLabel(AParent.Controls[ChildIndex]));
    if AParent.Controls[ChildIndex] is TWinControl then
    begin
      Result := FindLabelWithCaption(
        TWinControl(AParent.Controls[ChildIndex]), ACaption);
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

procedure TVerseMemoTests.ActivateEmptyVerseKeepsEditorAttachedToWindow;
var
  Container: TScrollBox;
  Editor: TVerseMemo;
  Host: TForm;
  VerseView: TVerse;
begin
  Forms.Application.Initialize;
  Host := TForm.CreateNew(nil);
  Container := TScrollBox.Create(Host);
  Container.Parent := Host;
  Container.Align := alClient;
  VerseView := TVerse.Create(Container);
  Editor := TVerseMemo.Create;
  try
    Host.Show;
    Application.ProcessMessages;
    VerseView.Text := '';

    Editor.Activate(Container, VerseView);

    AssertSame(VerseView, Editor.Verse);
    Editor.DiscardEdit;
  finally
    Editor.Free;
    VerseView.Free;
    Host.Free;
  end;
end;

procedure TVerseMemoTests.FailedSaveCallbackKeepsPendingEditorOpen;
var
  Composite: TTheWordWysiwygEditorControl;
  Container: TScrollBox;
  Editor: TVerseMemo;
  Host: TForm;
  Probe: TTracedSaveProbe;
  RawEditor: TSynEdit;
  VerseView: TVerse;
begin
  Forms.Application.Initialize;
  Host := TForm.CreateNew(nil);
  Container := TScrollBox.Create(Host);
  Container.Parent := Host;
  Container.Align := alClient;
  VerseView := TVerse.Create(Container);
  Editor := TVerseMemo.Create;
  Probe := TTracedSaveProbe.Create;
  try
    Host.Show;
    Application.ProcessMessages;
    VerseView.TokenizerVersion := 3;
    VerseView.Text := 'alpha beta';
    Editor.OnVerseTextModified := @Probe.Save;
    Editor.Activate(Container, VerseView);
    Composite := FindWysiwygEditor(Host);
    AssertTrue('WYSIWYG control must be attached', Assigned(Composite));
    Composite.RawMode := True;
    RawEditor := FindRawEditor(Composite);
    AssertTrue('Raw editor must be available', Assigned(RawEditor));
    RawEditor.Text := 'alpha changed';

    Probe.Apply := False;
    AssertFalse('failed application must block closing',
      Editor.TryDeactivate);
    AssertSame('the active verse must remain available', VerseView,
      Editor.Verse);
    AssertTrue('the pending change must remain pending',
      Editor.HasPendingChange);
    AssertEquals(1, Probe.Calls);
    AssertEquals('alpha changed', UTF8Encode(Probe.CurrentText));
    AssertEquals(Ord(vetmRawConservative), Ord(Probe.Mode));
    AssertEquals(3, Probe.TokenizerVersion);

    Probe.Apply := True;
    AssertTrue('successful application may close the editor',
      Editor.TryDeactivate);
    AssertEquals(2, Probe.Calls);
    AssertEquals('alpha changed', VerseView.XML);
    AssertNull('closed editor must release the active verse', Editor.Verse);
  finally
    Probe.Free;
    Editor.Free;
    VerseView.Free;
    Host.Free;
  end;
end;

procedure TVerseMemoTests.FailedVisualSaveKeepsConcreteErrorVisible;
const
  ExpectedError = 'deliberate traced-save failure';
var
  Composite: TTheWordWysiwygEditorControl;
  Container: TScrollBox;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TVerseMemo;
  ErrorLabel: TLabel;
  Host: TForm;
  Probe: TTracedSaveProbe;
  VerseView: TVerse;
begin
  Forms.Application.Initialize;
  Host := TForm.CreateNew(nil);
  Host.SetBounds(0, 0, 800, 300);
  Container := TScrollBox.Create(Host);
  Container.Parent := Host;
  Container.Align := alClient;
  VerseView := TVerse.Create(Container);
  Editor := TVerseMemo.Create;
  Probe := TTracedSaveProbe.Create;
  try
    Host.Show;
    Application.ProcessMessages;
    VerseView.Text := 'alpha beta';
    Editor.OnVerseTextModified := @Probe.Save;
    Editor.Activate(Container, VerseView);
    Composite := FindWysiwygEditor(Host);
    DirectEditor := FindDirectEditor(Composite);
    AssertTrue(Assigned(Composite));
    AssertTrue(Assigned(DirectEditor));
    DirectEditor.SetSelection(0, 5);
    AssertTrue('semantic edit must make the session dirty',
      Composite.ApplySemantic(twsmItalic));

    Probe.Apply := False;
    Probe.ErrorMessage := ExpectedError;
    AssertFalse(Editor.TryDeactivate);

    ErrorLabel := FindLabelWithCaption(Host, ExpectedError);
    AssertTrue('the concrete save error must remain in the toolbar',
      Assigned(ErrorLabel));
    AssertTrue('the concrete save error must be visible', ErrorLabel.Visible);
    AssertSame(VerseView, Editor.Verse);
    Editor.DiscardEdit;
  finally
    Probe.Free;
    Editor.Free;
    VerseView.Free;
    Host.Free;
  end;
end;

procedure TVerseMemoTests.PostCommitObserverFailureStillClosesAppliedEdit;
var
  Composite: TTheWordWysiwygEditorControl;
  Container: TScrollBox;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TVerseMemo;
  Host: TForm;
  Observer: TPostCommitObserverProbe;
  Probe: TTracedSaveProbe;
  ProjectInstance: TTestableProject;
  VerseView: TVerse;
begin
  Forms.Application.Initialize;
  Host := TForm.CreateNew(nil);
  Host.SetBounds(0, 0, 800, 300);
  Container := TScrollBox.Create(Host);
  Container.Parent := Host;
  Container.Align := alClient;
  VerseView := TVerse.Create(Container);
  Editor := TVerseMemo.Create;
  ProjectInstance := TTestableProject.Create;
  Observer := TPostCommitObserverProbe.Create;
  Probe := TTracedSaveProbe.Create;
  try
    Host.Show;
    Application.ProcessMessages;
    ProjectInstance.OnNewVerseSubscribe(@Observer.Fail);
    ProjectInstance.OnNewVerseSubscribe(@Observer.Succeed);
    Probe.Apply := True;
    Probe.PostCommitProject := ProjectInstance;
    VerseView.Text := 'alpha beta';
    Editor.OnVerseTextModified := @Probe.Save;
    Editor.Activate(Container, VerseView);
    Composite := FindWysiwygEditor(Host);
    DirectEditor := FindDirectEditor(Composite);
    AssertTrue(Assigned(Composite));
    AssertTrue(Assigned(DirectEditor));
    DirectEditor.SetSelection(0, 5);
    AssertTrue(Composite.ApplySemantic(twsmItalic));

    AssertTrue('a committed edit must close despite a refresh observer error',
      Editor.TryDeactivate);

    AssertEquals('post-commit observers must wait until TryDeactivate returns',
      0, Observer.FailedCalls);
    AssertEquals('post-commit observers must wait until TryDeactivate returns',
      0, Observer.SucceededCalls);
    AssertEquals('<FI>alpha<Fi> beta', VerseView.XML);
    AssertNull('the applied editor session must be accepted and closed',
      Editor.Verse);

    ProjectInstance.NotifyAfterEdit;
    Application.ProcessMessages;

    AssertEquals(1, Observer.FailedCalls);
    AssertEquals('queued refreshes must coalesce and continue after failure', 1,
      Observer.SucceededCalls);
  finally
    Probe.Free;
    Observer.Free;
    ProjectInstance.Free;
    Editor.Free;
    VerseView.Free;
    Host.Free;
  end;
end;

procedure TVerseMemoTests.RawSaveCannotChangeProtectedLexicalObjects;
var
  Composite: TTheWordWysiwygEditorControl;
  Container: TScrollBox;
  Editor: TVerseMemo;
  Host: TForm;
  Probe: TTracedSaveProbe;
  RawEditor: TSynEdit;
  VerseView: TVerse;
begin
  Forms.Application.Initialize;
  Host := TForm.CreateNew(nil);
  Container := TScrollBox.Create(Host);
  Container.Parent := Host;
  Container.Align := alClient;
  VerseView := TVerse.Create(Container);
  Editor := TVerseMemo.Create;
  Probe := TTracedSaveProbe.Create;
  try
    Host.Show;
    Application.ProcessMessages;
    VerseView.Text := 'alpha<WG123> beta';
    Editor.OnVerseTextModified := @Probe.Save;
    Editor.Activate(Container, VerseView);
    Composite := FindWysiwygEditor(Host);
    AssertTrue(Assigned(Composite));
    Composite.RawMode := True;
    RawEditor := FindRawEditor(Composite);
    AssertTrue(Assigned(RawEditor));
    RawEditor.Text := 'alpha<WG124> beta';

    Probe.Apply := True;
    AssertFalse('lexical payload changes must remain protected',
      Editor.TryDeactivate);
    AssertEquals('the save callback must not see an unsafe edit', 0,
      Probe.Calls);
    AssertSame(VerseView, Editor.Verse);
    AssertEquals('alpha<WG123> beta', VerseView.XML);
    Editor.DiscardEdit;
  finally
    Probe.Free;
    Editor.Free;
    VerseView.Free;
    Host.Free;
  end;
end;

initialization
  RegisterTest(TVerseMemoTests);

end.
