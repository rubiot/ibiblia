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
    procedure NavigatingToAnotherVerseClearsVisualSelectionAndCaret;
    procedure TranslatorNoteHoverEndsOnSaveCancelHideAndReload;
    procedure PostCommitObserverFailureStillClosesAppliedEdit;
    procedure RawSaveCannotChangeProtectedLexicalObjects;
    procedure SemanticToolbarButtonsAreCompactTypographicAndNonOverlapping;
  end;

implementation

uses
  SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Graphics, SynEdit, Verse,
  VerseMemo, Project,
  directwrite_syntagm_view, directwrite_verse_editor, verse_view_intf,
  verse_visual_document, theword_ont_codec_v2,
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

function FindButtonWithCaption(AParent: TWinControl;
  const ACaption: string): TButton;
var
  ChildIndex: Integer;
begin
  Result := nil;
  if not Assigned(AParent) then
    Exit;
  for ChildIndex := 0 to AParent.ControlCount - 1 do
  begin
    if (AParent.Controls[ChildIndex] is TButton) and
       (TButton(AParent.Controls[ChildIndex]).Caption = ACaption) then
      Exit(TButton(AParent.Controls[ChildIndex]));
    if AParent.Controls[ChildIndex] is TWinControl then
    begin
      Result := FindButtonWithCaption(
        TWinControl(AParent.Controls[ChildIndex]), ACaption);
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function FindSpeedButtonWithCaption(AParent: TWinControl;
  const ACaption: string): TSpeedButton;
var
  ChildIndex: Integer;
begin
  Result := nil;
  if not Assigned(AParent) then
    Exit;
  for ChildIndex := 0 to AParent.ControlCount - 1 do
  begin
    if (AParent.Controls[ChildIndex] is TSpeedButton) and
       (TSpeedButton(AParent.Controls[ChildIndex]).Caption = ACaption) then
      Exit(TSpeedButton(AParent.Controls[ChildIndex]));
    if AParent.Controls[ChildIndex] is TWinControl then
    begin
      Result := FindSpeedButtonWithCaption(
        TWinControl(AParent.Controls[ChildIndex]), ACaption);
      if Assigned(Result) then
        Exit;
    end;
  end;
end;

function FindSpeedButtonWithStyle(AParent: TWinControl;
  AStyle: TFontStyle): TSpeedButton;
var
  ChildIndex: Integer;
begin
  Result := nil;
  if not Assigned(AParent) then
    Exit;
  for ChildIndex := 0 to AParent.ControlCount - 1 do
  begin
    if (AParent.Controls[ChildIndex] is TSpeedButton) and
       (AStyle in TSpeedButton(AParent.Controls[ChildIndex]).Font.Style) then
      Exit(TSpeedButton(AParent.Controls[ChildIndex]));
    if AParent.Controls[ChildIndex] is TWinControl then
    begin
      Result := FindSpeedButtonWithStyle(
        TWinControl(AParent.Controls[ChildIndex]), AStyle);
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

procedure TVerseMemoTests.NavigatingToAnotherVerseClearsVisualSelectionAndCaret;
var
  Container: TScrollBox;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TVerseMemo;
  Host: TForm;
  VerseA: TVerse;
  VerseB: TVerse;
begin
  Forms.Application.Initialize;
  Host := TForm.CreateNew(nil);
  Host.SetBounds(0, 0, 800, 300);
  Container := TScrollBox.Create(Host);
  Container.Parent := Host;
  Container.Align := alClient;
  VerseA := TVerse.Create(Container);
  VerseB := TVerse.Create(Container);
  Editor := TVerseMemo.Create;
  try
    Host.Show;
    Application.ProcessMessages;
    VerseA.Text := 'alpha beta gamma';
    VerseB.Text := 'one two three four';

    Editor.Activate(Container, VerseA);
    DirectEditor := FindDirectEditor(Host);
    AssertTrue('the DirectWrite editor must be attached',
      Assigned(DirectEditor));
    DirectEditor.SetSelection(6, 4);
    AssertEquals('precondition: verse A selection start', 6,
      DirectEditor.SelectionStart);
    AssertEquals('precondition: verse A selection length', 4,
      DirectEditor.SelectionLength);
    AssertTrue('leaving an unmodified verse must close its editor',
      Editor.TryDeactivate);

    Editor.Activate(Container, VerseB);
    DirectEditor := FindDirectEditor(Host);
    AssertTrue('the DirectWrite editor must remain attached',
      Assigned(DirectEditor));
    AssertEquals('a new verse must not inherit the previous selection', 0,
      DirectEditor.SelectionLength);
    AssertEquals('a new verse must start at its first editable caret', 0,
      DirectEditor.CaretOffset);
    Editor.DiscardEdit;
  finally
    Editor.Free;
    VerseB.Free;
    VerseA.Free;
    Host.Free;
  end;
end;

procedure TVerseMemoTests.TranslatorNoteHoverEndsOnSaveCancelHideAndReload;
var
  Composite: TTheWordWysiwygEditorControl;
  Container: TScrollBox;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TVerseMemo;
  Hit: TVerseHit;
  Host: TForm;
  Probe: TTracedSaveProbe;
  VerseView: TVerse;

  procedure BeginPendingHover;
  var
    HoverHandler: TVerseHitEvent;
  begin
    FillChar(Hit, SizeOf(Hit), 0);
    Hit.Revision := 1;
    Hit.Role := vsrTranslatorNote;
    Hit.ObjectId := 999;
    HoverHandler := DirectEditor.OnVisualObjectHover;
    AssertTrue('the composite editor must own the hover callback',
      Assigned(HoverHandler));
    HoverHandler(DirectEditor, Hit);
    AssertTrue('hover must arm the note preview lifecycle',
      Composite.TranslatorNotePreviewActive);
  end;

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
    VerseView.Text := 'alpha<RF>note<Rf> beta';
    Editor.OnVerseTextModified := @Probe.Save;
    Editor.Activate(Container, VerseView);
    Composite := FindWysiwygEditor(Host);
    DirectEditor := FindDirectEditor(Composite);
    AssertTrue(Assigned(Composite));
    AssertTrue(Assigned(DirectEditor));

    DirectEditor.SetSelection(0, 5);
    AssertTrue('save precondition: semantic change is pending',
      Composite.ApplySemantic(twsmItalic));
    BeginPendingHover;
    Probe.Apply := True;
    AssertTrue('save must close the active editor', Editor.TryDeactivate);
    AssertFalse('save must cancel timer and preview',
      Composite.TranslatorNotePreviewActive);

    Editor.Activate(Container, VerseView);
    Composite := FindWysiwygEditor(Host);
    DirectEditor := FindDirectEditor(Composite);
    BeginPendingHover;
    Editor.DiscardEdit;
    AssertFalse('cancel must cancel timer and preview',
      Composite.TranslatorNotePreviewActive);

    Editor.Activate(Container, VerseView);
    Composite := FindWysiwygEditor(Host);
    DirectEditor := FindDirectEditor(Composite);
    BeginPendingHover;
    Composite.Visible := False;
    AssertFalse('hiding the control must cancel timer and preview',
      Composite.TranslatorNotePreviewActive);
    Composite.Visible := True;

    BeginPendingHover;
    Composite.Text := 'replacement';
    AssertFalse('loading another verse must cancel timer and preview',
      Composite.TranslatorNotePreviewActive);

    BeginPendingHover;
    Composite.DismissTranslatorNotePreview;
    AssertFalse('the explicit lifecycle API must be idempotent',
      Composite.TranslatorNotePreviewActive);
    Composite.DismissTranslatorNotePreview;
    AssertFalse(Composite.TranslatorNotePreviewActive);
    Editor.DiscardEdit;
  finally
    Probe.Free;
    Editor.Free;
    VerseView.Free;
    Host.Free;
  end;
end;

procedure TVerseMemoTests.
  SemanticToolbarButtonsAreCompactTypographicAndNonOverlapping;
var
  AddedButton, JesusButton, OldTestamentButton, UnderlineButton,
    StrikethroughButton, SuperscriptButton, SubscriptButton: TSpeedButton;
  RawButton: TButton;
  Composite: TTheWordWysiwygEditorControl;
  Container: TScrollBox;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TVerseMemo;
  Host: TForm;
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
  try
    Host.Show;
    Application.ProcessMessages;
    VerseView.Text := 'alpha';
    Editor.Activate(Container, VerseView);

    AddedButton := FindSpeedButtonWithCaption(Host, 'I');
    JesusButton := FindSpeedButtonWithCaption(Host, 'J');
    OldTestamentButton := FindSpeedButtonWithCaption(Host, 'O');
    UnderlineButton := FindSpeedButtonWithCaption(Host, 'S');
    StrikethroughButton := FindSpeedButtonWithStyle(Host, fsStrikeOut);
    SuperscriptButton := FindSpeedButtonWithCaption(Host, 'x²');
    SubscriptButton := FindSpeedButtonWithCaption(Host, 'x₂');
    RawButton := FindButtonWithCaption(Host, 'Raw');
    AssertTrue('italic semantic button must exist', Assigned(AddedButton));
    AssertTrue('Jesus semantic button must exist', Assigned(JesusButton));
    AssertTrue('Old Testament semantic button must exist',
      Assigned(OldTestamentButton));
    AssertTrue('underline semantic button must exist',
      Assigned(UnderlineButton));
    AssertTrue('strikethrough semantic button must exist',
      Assigned(StrikethroughButton));
    AssertTrue('superscript semantic button must exist',
      Assigned(SuperscriptButton));
    AssertTrue('subscript semantic button must exist',
      Assigned(SubscriptButton));
    AssertTrue('raw-mode button must exist', Assigned(RawButton));

    AssertTrue('I must be italic', fsItalic in AddedButton.Font.Style);
    AssertEquals('J must be red', LongInt(clRed),
      LongInt(JesusButton.Font.Color));
    AssertTrue('O must be bold', fsBold in OldTestamentButton.Font.Style);
    AssertTrue('S must be underlined',
      fsUnderline in UnderlineButton.Font.Style);
    AssertTrue('strike button must be struck',
      fsStrikeOut in StrikethroughButton.Font.Style);
    AssertTrue('semantic buttons must be narrower than Raw',
      AddedButton.Width < RawButton.Width);
    AssertEquals('semantic buttons share the compact width',
      AddedButton.Width, JesusButton.Width);
    AssertEquals('semantic buttons share the compact width',
      AddedButton.Width, OldTestamentButton.Width);
    AssertEquals('semantic buttons share the compact width',
      AddedButton.Width, UnderlineButton.Width);
    AssertEquals('semantic buttons share the compact width',
      AddedButton.Width, StrikethroughButton.Width);
    AssertEquals('semantic buttons share the compact width',
      AddedButton.Width, SuperscriptButton.Width);
    AssertEquals('semantic buttons share the compact width',
      AddedButton.Width, SubscriptButton.Width);

    AssertTrue('I and J must not overlap',
      AddedButton.Left + AddedButton.Width <= JesusButton.Left);
    AssertTrue('J and O must not overlap',
      JesusButton.Left + JesusButton.Width <= OldTestamentButton.Left);
    AssertTrue('O and S must not overlap',
      OldTestamentButton.Left + OldTestamentButton.Width <=
        UnderlineButton.Left);
    AssertTrue('underline and strike must not overlap',
      UnderlineButton.Left + UnderlineButton.Width <=
        StrikethroughButton.Left);
    AssertTrue('strike and superscript must not overlap',
      StrikethroughButton.Left + StrikethroughButton.Width <=
        SuperscriptButton.Left);
    AssertTrue('superscript and subscript must not overlap',
      SuperscriptButton.Left + SuperscriptButton.Width <=
        SubscriptButton.Left);
    AssertTrue('subscript and Raw must not overlap',
      SubscriptButton.Left + SubscriptButton.Width <= RawButton.Left);
    AssertTrue('italic hint identifies added words',
      Pos('Added words', AddedButton.Hint) > 0);
    AssertTrue('italic hint exposes Ctrl+I',
      Pos('Ctrl+I', AddedButton.Hint) > 0);
    AssertTrue('red hint identifies words of Jesus',
      Pos('Words of Jesus', JesusButton.Hint) > 0);
    AssertTrue('red hint exposes Ctrl+J',
      Pos('Ctrl+J', JesusButton.Hint) > 0);
    AssertTrue('bold hint identifies the Old Testament quotation',
      Pos('Old Testament', OldTestamentButton.Hint) > 0);
    AssertTrue('bold hint exposes Ctrl+O',
      Pos('Ctrl+O', OldTestamentButton.Hint) > 0);
    AssertTrue('underline hint identifies its action',
      Pos('Underline', UnderlineButton.Hint) > 0);
    AssertTrue('strike hint exposes its shortcut',
      Pos('Ctrl+Shift+S', StrikethroughButton.Hint) > 0);
    AssertTrue('superscript hint exposes its shortcut',
      Pos('Ctrl+Shift+=', SuperscriptButton.Hint) > 0);
    AssertTrue('subscript hint exposes its shortcut',
      Pos('Ctrl+=', SubscriptButton.Hint) > 0);

    Composite := FindWysiwygEditor(Host);
    DirectEditor := FindDirectEditor(Host);
    AssertTrue(Assigned(Composite));
    AssertTrue(Assigned(DirectEditor));
    AssertFalse('semantic commands require a selection', AddedButton.Enabled);
    DirectEditor.SetSelection(0, 5);
    AssertTrue('semantic commands become enabled for selected text',
      AddedButton.Enabled and JesusButton.Enabled and
      OldTestamentButton.Enabled and UnderlineButton.Enabled and
      StrikethroughButton.Enabled and SuperscriptButton.Enabled and
      SubscriptButton.Enabled);

    AddedButton.Click;
    AssertEquals('I keeps the added-words callback', '<FI>alpha<Fi>',
      UTF8Encode(Composite.Text));
    AddedButton.Click;
    AssertEquals('I toggles the semantic back off', 'alpha',
      UTF8Encode(Composite.Text));
    JesusButton.Click;
    AssertEquals('J keeps the words-of-Jesus callback', '<FR>alpha<Fr>',
      UTF8Encode(Composite.Text));
    JesusButton.Click;
    OldTestamentButton.Click;
    AssertEquals('O keeps the Old Testament quotation callback',
      '<FO>alpha<Fo>', UTF8Encode(Composite.Text));
    OldTestamentButton.Click;
    UnderlineButton.Click;
    AssertEquals('S keeps the underline callback', '<FU>alpha<Fu>',
      UTF8Encode(Composite.Text));
    UnderlineButton.Click;
    StrikethroughButton.Click;
    AssertEquals('strike callback uses the canonical tag', '<s>alpha</s>',
      UTF8Encode(Composite.Text));
    StrikethroughButton.Click;
    SuperscriptButton.Click;
    AssertEquals('superscript callback uses the canonical tag',
      '<sup>alpha</sup>', UTF8Encode(Composite.Text));
    SuperscriptButton.Click;
    SubscriptButton.Click;
    AssertEquals('subscript callback uses the canonical tag',
      '<sub>alpha</sub>', UTF8Encode(Composite.Text));
    SubscriptButton.Click;
    AssertEquals('all semantic toggles restore the original text', 'alpha',
      UTF8Encode(Composite.Text));
    Editor.DiscardEdit;
  finally
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
