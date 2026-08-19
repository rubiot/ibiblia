unit VerseMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, LCLType, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Graphics, Verse, Math, directwrite_verse_editor, theword_ont_codec_v2,
  theword_wysiwyg_editor_control, text_edit_provenance;

type
  TVerseMemo = class;
  { ATrace remains owned by TVerseMemo and is valid only for the duration of
    the callback. Returning False leaves the editor and its pending change
    open. }
  TVerseTextModifiedEvent = function(Sender: TVerseMemo;
    const ATrace: TVerseEditTrace;
    out AImpact: TAssociationEditImpact): Boolean of object;

  TVerseMemo = class
  private
    FModified: Boolean;
    FActivating: Boolean;
    FEditorPanel: TPanel;
    FToolbarPanel: TPanel;
    FModeLabel: TLabel;
    FStatusLabel: TLabel;
    FSaveButton: TButton;
    FCancelButton: TButton;
    FAddedWordsButton: TSpeedButton;
    FJesusWordsButton: TSpeedButton;
    FOldTestamentQuoteButton: TSpeedButton;
    FUnderlineButton: TSpeedButton;
    FTranslatorNoteButton: TSpeedButton;
    FRawModeButton: TButton;
    FSaveErrorActive: Boolean;
    FEditor: TTheWordWysiwygEditorControl;
    FContainer: TScrollBox;
    FBoundsContainer: TScrollBox;
    FVerse: TVerse;
    FOnVerseTextModified: TVerseTextModifiedEvent;
    FOnPendingChange: TNotifyEvent;
    function GetText: TCaption;
    procedure ApplySemantic(AKind: TTheWordSemanticKind);
    procedure CancelButtonClick(Sender: TObject);
    procedure ContainerBoundsChanged(Sender: TObject);
    procedure DetachContainerBoundsHandler;
    procedure EditorChanged(Sender: TTheWordWysiwygEditorControl);
    procedure EditorCommand(Sender: TTheWordWysiwygEditorControl;
      ACommand: TVerseEditorCommand);
    procedure InsertAddedWords(Sender: TObject);
    procedure InsertJesusWords(Sender: TObject);
    procedure InsertOldTestamentQuote(Sender: TObject);
    procedure InsertUnderline(Sender: TObject);
    procedure InsertTranslatorNote(Sender: TObject);
    procedure LayoutEditorToolbar;
    procedure RefreshLocalizedCaptions;
    procedure RestoreFocusAfterClose;
    procedure SaveAndClose;
    procedure SaveButtonClick(Sender: TObject);
    procedure ShowSaveError(const AMessage: string);
    procedure ToggleRawMode(Sender: TObject);
    procedure ToolbarResize(Sender: TObject);
    procedure UpdateEditorBounds;
    procedure UpdateEditorState;
    function ValidateTextForSave: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Activate(Container: TScrollBox; Verse: TVerse;
      AAutomaticNoteStart: Integer = 0);
    procedure Deactivate;
    function HasPendingChange: Boolean;
    function TryDeactivate: Boolean;
    procedure DiscardEdit;
    property Text: TCaption read GetText;
    property Verse: TVerse read FVerse;
    property OnVerseTextModified: TVerseTextModifiedEvent
      read FOnVerseTextModified write FOnVerseTextModified;
    property OnPendingChange: TNotifyEvent read FOnPendingChange
      write FOnPendingChange;
  end;

resourcestring
  SEditModeTitle = 'Editing verse';
  SEditModeSaved = 'No changes';
  SEditModeModified = 'Unsaved changes';
  SProtectedRegions = 'Protected ONT objects';
  SSaveEditButton = 'Save';
  SSaveEditHint = 'Save verse text and close the editor';
  SCancelEditButton = 'Cancel';
  SCancelEditHint = 'Discard changes and close the editor';
  SAddedWordsButton = 'I';
  SJesusWordsButton = 'J';
  SOldTestamentQuoteButton = 'O';
  SUnderlineButton = 'S';
  STranslatorNoteButton = 'N¹';
  SRawModeButton = 'Raw';
  SVisualModeButton = 'Visual';
  SAddedWordsHint =
    'Added words: apply or remove italic formatting (Ctrl+I)';
  SJesusWordsHint =
    'Words of Jesus: apply or remove red formatting (Ctrl+J)';
  SOldTestamentQuoteHint =
    'Old Testament quotation: apply or remove bold formatting (Ctrl+O)';
  SUnderlineHint = 'Underline: apply or remove underlining';
  STranslatorNoteHint =
    'Insert translator note at the caret (Ctrl+Shift+N)';
  SRawModeHint = 'Switch between semantic editing and the raw ONT fallback';
  SLineBreaksInEditorTitle = 'Edit error';
  SLineBreaksInEditor = 'A verse cannot contain line breaks. Remove line breaks before saving.';
  SEditApplyFailed = 'The edit could not be applied';
  SProtectedLexicalObjectsChanged =
    'Strong and morphology data are protected in this editing cycle';

implementation

function TVerseMemo.GetText: TCaption;
begin
  Result := FEditor.Text;
end;

procedure TVerseMemo.ApplySemantic(AKind: TTheWordSemanticKind);
begin
  FEditor.ApplySemantic(AKind);
end;

procedure TVerseMemo.CancelButtonClick(Sender: TObject);
begin
  DiscardEdit;
end;

procedure TVerseMemo.ContainerBoundsChanged(Sender: TObject);
begin
  UpdateEditorBounds;
end;

procedure TVerseMemo.DetachContainerBoundsHandler;
begin
  if Assigned(FBoundsContainer) then
  begin
    FBoundsContainer.RemoveHandlerOnChangeBounds(@ContainerBoundsChanged);
    FBoundsContainer := nil;
  end;
end;

procedure TVerseMemo.EditorChanged(Sender: TTheWordWysiwygEditorControl);
begin
  if FActivating or not FEditor.Visible then
    Exit;
  FSaveErrorActive := False;
  FModified := FEditor.Modified;
  UpdateEditorState;
  if Assigned(FOnPendingChange) then
    FOnPendingChange(Self);
end;

procedure TVerseMemo.EditorCommand(Sender: TTheWordWysiwygEditorControl;
  ACommand: TVerseEditorCommand);
begin
  case ACommand of
    vecSave: SaveAndClose;
    vecCancel: DiscardEdit;
  end;
end;

procedure TVerseMemo.InsertAddedWords(Sender: TObject);
begin
  ApplySemantic(twsmItalic);
end;

procedure TVerseMemo.InsertJesusWords(Sender: TObject);
begin
  ApplySemantic(twsmWordsOfJesus);
end;

procedure TVerseMemo.InsertOldTestamentQuote(Sender: TObject);
begin
  ApplySemantic(twsmOldTestamentQuote);
end;

procedure TVerseMemo.InsertUnderline(Sender: TObject);
begin
  ApplySemantic(twsmUnderline);
end;

procedure TVerseMemo.InsertTranslatorNote(Sender: TObject);
begin
  FEditor.InsertTranslatorNote;
end;

procedure TVerseMemo.ToggleRawMode(Sender: TObject);
begin
  FEditor.RawMode := not FEditor.RawMode;
  UpdateEditorState;
end;

procedure TVerseMemo.LayoutEditorToolbar;
const
  Margin = 6;
  Gap = 5;
  SemanticButtonWidth = 34;
  RawModeButtonWidth = 64;
  ButtonRowHeight = 38;
  ErrorRowHeight = 24;
var
  X, RightEdge, ButtonHeight, ButtonTop, SaveWidth, CancelWidth, ModeWidth,
    StatusLeft, StatusWidth: Integer;
begin
  if not Assigned(FEditorPanel) or not Assigned(FToolbarPanel) or
     not Assigned(FEditorPanel.Parent) or
     (FToolbarPanel.ClientWidth <= 0) then
    Exit;

  if FSaveErrorActive then
    FToolbarPanel.Height := ButtonRowHeight + ErrorRowHeight
  else
    FToolbarPanel.Height := ButtonRowHeight;

  FToolbarPanel.Canvas.Font.Assign(FToolbarPanel.Font);
  ButtonHeight := Max(26, FToolbarPanel.Canvas.TextHeight('Hg') + 10);
  ButtonTop := (ButtonRowHeight - ButtonHeight) div 2;
  SaveWidth := Max(72, FToolbarPanel.Canvas.TextWidth(FSaveButton.Caption) + 26);
  CancelWidth := Max(72,
    FToolbarPanel.Canvas.TextWidth(FCancelButton.Caption) + 26);
  ModeWidth := Max(90,
    FToolbarPanel.Canvas.TextWidth(FModeLabel.Caption) + 10);

  RightEdge := FToolbarPanel.ClientWidth - Margin;
  FCancelButton.SetBounds(RightEdge - CancelWidth,
    ButtonTop,
    CancelWidth, ButtonHeight);
  Dec(RightEdge, CancelWidth + Gap);
  FSaveButton.SetBounds(RightEdge - SaveWidth,
    ButtonTop,
    SaveWidth, ButtonHeight);
  Dec(RightEdge, SaveWidth + Gap);

  X := Margin;
  FModeLabel.SetBounds(X, 0, ModeWidth, ButtonRowHeight);
  Inc(X, ModeWidth + Gap);
  FAddedWordsButton.SetBounds(X,
    ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);
  FJesusWordsButton.SetBounds(X,
    ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);
  FOldTestamentQuoteButton.SetBounds(X,
    ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);
  FUnderlineButton.SetBounds(X,
    ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);
  FTranslatorNoteButton.SetBounds(X,
    ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);
  FRawModeButton.SetBounds(X,
    ButtonTop,
    RawModeButtonWidth, ButtonHeight);
  Inc(X, RawModeButtonWidth + Gap);

  if FSaveErrorActive then
  begin
    FStatusLabel.Visible := True;
    FStatusLabel.SetBounds(Margin, ButtonRowHeight,
      Max(0, FToolbarPanel.ClientWidth - (2 * Margin)), ErrorRowHeight);
  end
  else
  begin
    StatusLeft := X;
    StatusWidth := RightEdge - StatusLeft;
    FStatusLabel.Visible := StatusWidth >= 80;
    if FStatusLabel.Visible then
      FStatusLabel.SetBounds(StatusLeft, 0, StatusWidth, ButtonRowHeight);
  end;
end;

procedure TVerseMemo.RefreshLocalizedCaptions;
begin
  FModeLabel.Caption := SEditModeTitle;
  FSaveButton.Caption := SSaveEditButton;
  FSaveButton.Hint := SSaveEditHint;
  FCancelButton.Caption := SCancelEditButton;
  FCancelButton.Hint := SCancelEditHint;
  FAddedWordsButton.Caption := SAddedWordsButton;
  FAddedWordsButton.Hint := SAddedWordsHint;
  FJesusWordsButton.Caption := SJesusWordsButton;
  FJesusWordsButton.Hint := SJesusWordsHint;
  FOldTestamentQuoteButton.Caption := SOldTestamentQuoteButton;
  FOldTestamentQuoteButton.Hint := SOldTestamentQuoteHint;
  FUnderlineButton.Caption := SUnderlineButton;
  FUnderlineButton.Hint := SUnderlineHint;
  FTranslatorNoteButton.Caption := STranslatorNoteButton;
  FTranslatorNoteButton.Hint := STranslatorNoteHint;
  FRawModeButton.Hint := SRawModeHint;
end;

procedure TVerseMemo.RestoreFocusAfterClose;
var
  FocusTarget: TWinControl;
begin
  if Assigned(FVerse) then
    FVerse.RefreshAfterEditorClose;
  FocusTarget := nil;
  if Assigned(FContainer) then
  begin
    if FContainer.CanSetFocus then
      FocusTarget := FContainer
    else if (FContainer.Parent is TWinControl) and
      TWinControl(FContainer.Parent).CanSetFocus then
      FocusTarget := TWinControl(FContainer.Parent);
  end;
  if Assigned(FocusTarget) then
    try
      FocusTarget.SetFocus;
    except
      on EInvalidOperation do ;
    end;
end;

procedure TVerseMemo.SaveAndClose;
begin
  TryDeactivate;
end;

procedure TVerseMemo.SaveButtonClick(Sender: TObject);
begin
  SaveAndClose;
end;

procedure TVerseMemo.ShowSaveError(const AMessage: string);
begin
  { Restore keyboard/IME focus before publishing the error. Focus changes can
    cause selection notifications, so doing this first prevents a generic
    editor-state refresh from replacing the concrete failure reason. }
  FEditor.FocusEditor;
  FSaveErrorActive := True;
  if AMessage <> '' then
    FStatusLabel.Caption := AMessage
  else
    FStatusLabel.Caption := SEditApplyFailed;
  FStatusLabel.Font.Color := clRed;
  FEditorPanel.Hint := FStatusLabel.Caption;
  LayoutEditorToolbar;
end;

procedure TVerseMemo.ToolbarResize(Sender: TObject);
begin
  LayoutEditorToolbar;
end;

procedure TVerseMemo.UpdateEditorBounds;
begin
  if not Assigned(FContainer) or not Assigned(FEditorPanel) or
     not Assigned(FEditorPanel.Parent) or (FEditorPanel.Align <> alNone) then
    Exit;
  FEditorPanel.SetBounds(FContainer.Left, FContainer.Top, FContainer.Width,
    FContainer.Height);
  FEditorPanel.BringToFront;
  LayoutEditorToolbar;
end;

procedure TVerseMemo.UpdateEditorState;
var
  CanMark: Boolean;
begin
  if not Assigned(FStatusLabel) then
    Exit;
  if FSaveErrorActive then
    FStatusLabel.Font.Color := clRed
  else if FModified then
  begin
    FStatusLabel.Caption := SEditModeModified;
    FStatusLabel.Font.Color := clRed;
  end
  else if FEditor.Capability = twlcProtectedRegions then
  begin
    FStatusLabel.Caption := SProtectedRegions;
    FStatusLabel.Font.Color := clGrayText;
  end
  else
  begin
    FStatusLabel.Caption := SEditModeSaved;
    FStatusLabel.Font.Color := clGrayText;
  end;

  CanMark := FEditor.CanApplySemantic;
  FAddedWordsButton.Enabled := CanMark;
  FJesusWordsButton.Enabled := CanMark;
  FOldTestamentQuoteButton.Enabled := CanMark;
  FUnderlineButton.Enabled := CanMark;
  FTranslatorNoteButton.Enabled := FEditor.CanInsertTranslatorNote;
  if FEditor.RawMode then
    FRawModeButton.Caption := SVisualModeButton
  else
    FRawModeButton.Caption := SRawModeButton;
  FEditorPanel.Hint := FStatusLabel.Caption;
  LayoutEditorToolbar;
end;

function TVerseMemo.ValidateTextForSave: Boolean;
begin
  Result := (Pos(#13, Text) = 0) and (Pos(#10, Text) = 0);
  if not Result then
    MessageDlg(SLineBreaksInEditorTitle, SLineBreaksInEditor, mtError,
      [mbOK], 0);
end;

constructor TVerseMemo.Create;

  function NewSemanticButton(const ACaption, AHint: string;
    AHandler: TNotifyEvent): TSpeedButton;
  begin
    Result := TSpeedButton.Create(FEditorPanel);
    Result.Parent := FToolbarPanel;
    Result.Caption := ACaption;
    Result.Flat := True;
    Result.Hint := AHint;
    Result.ShowHint := True;
    Result.OnClick := AHandler;
  end;

  function NewToolbarButton(const ACaption, AHint: string;
    AHandler: TNotifyEvent): TButton;
  begin
    Result := TButton.Create(FEditorPanel);
    Result.Parent := FToolbarPanel;
    Result.Caption := ACaption;
    Result.Hint := AHint;
    Result.ShowHint := True;
    Result.OnClick := AHandler;
  end;

begin
  FEditorPanel := TPanel.Create(nil);
  FEditorPanel.Visible := False;
  FEditorPanel.Align := alNone;
  FEditorPanel.BevelOuter := bvLowered;
  FEditorPanel.ParentColor := False;
  FEditorPanel.Color := clWindow;
  FEditorPanel.ShowHint := True;

  FToolbarPanel := TPanel.Create(FEditorPanel);
  FToolbarPanel.Parent := FEditorPanel;
  FToolbarPanel.Align := alTop;
  FToolbarPanel.Height := 38;
  FToolbarPanel.BevelOuter := bvNone;
  FToolbarPanel.ParentColor := False;
  FToolbarPanel.Color := clInfoBk;
  FToolbarPanel.OnResize := @ToolbarResize;

  FModeLabel := TLabel.Create(FEditorPanel);
  FModeLabel.Parent := FToolbarPanel;
  FModeLabel.AutoSize := False;
  FModeLabel.Layout := tlCenter;
  FModeLabel.Font.Style := FModeLabel.Font.Style + [fsBold];
  FModeLabel.Transparent := True;

  FStatusLabel := TLabel.Create(FEditorPanel);
  FStatusLabel.Parent := FToolbarPanel;
  FStatusLabel.AutoSize := False;
  FStatusLabel.Layout := tlCenter;
  FStatusLabel.Transparent := True;

  FAddedWordsButton := NewSemanticButton(SAddedWordsButton,
    SAddedWordsHint, @InsertAddedWords);
  FAddedWordsButton.Font.Style := [fsItalic];
  FJesusWordsButton := NewSemanticButton(SJesusWordsButton,
    SJesusWordsHint, @InsertJesusWords);
  FJesusWordsButton.Font.Color := clRed;
  FOldTestamentQuoteButton := NewSemanticButton(SOldTestamentQuoteButton,
    SOldTestamentQuoteHint, @InsertOldTestamentQuote);
  FOldTestamentQuoteButton.Font.Style := [fsBold];
  FUnderlineButton := NewSemanticButton(SUnderlineButton,
    SUnderlineHint, @InsertUnderline);
  FUnderlineButton.Font.Style := [fsUnderline];
  FTranslatorNoteButton := NewSemanticButton(STranslatorNoteButton,
    STranslatorNoteHint, @InsertTranslatorNote);
  FTranslatorNoteButton.Font.Color := clBlue;
  FTranslatorNoteButton.Font.Style := [fsItalic];
  FRawModeButton := NewToolbarButton(SRawModeButton,
    SRawModeHint, @ToggleRawMode);
  FSaveButton := NewToolbarButton(SSaveEditButton,
    SSaveEditHint, @SaveButtonClick);
  FCancelButton := NewToolbarButton(SCancelEditButton,
    SCancelEditHint, @CancelButtonClick);

  FEditor := TTheWordWysiwygEditorControl.Create(nil);
  FEditor.Visible := False;
  FEditor.Parent := FEditorPanel;
  FEditor.Align := alClient;
  FEditor.OnChanged := @EditorChanged;
  FEditor.OnCommand := @EditorCommand;

  FActivating := False;
  FModified := False;
  FSaveErrorActive := False;
  FContainer := nil;
  FBoundsContainer := nil;
  FVerse := nil;
  FOnVerseTextModified := nil;
  FOnPendingChange := nil;
  RefreshLocalizedCaptions;
  UpdateEditorState;
end;

destructor TVerseMemo.Destroy;
begin
  DetachContainerBoundsHandler;
  FEditor.DismissTranslatorNotePreview;
  FEditor.Parent := nil;
  FEditor.Free;
  FEditorPanel.Free;
  inherited Destroy;
end;

procedure TVerseMemo.Activate(Container: TScrollBox; Verse: TVerse;
  AAutomaticNoteStart: Integer);
var
  Activated: Boolean;
  OverlayParent: TWinControl;
begin
  if not Assigned(Container) or not Assigned(Verse) then
    Exit;

  { The editor is a sibling overlay. Without a parent for the verse container
    there is no window in which that overlay can safely be created. }
  OverlayParent := Container.Parent;
  if not Assigned(OverlayParent) then
    Exit;

  if FEditorPanel.Visible then
  begin
    if FContainer = Container then
    begin
      FEditor.FocusEditor;
      Exit;
    end;
    if not TryDeactivate then
      Exit;
  end;

  RefreshLocalizedCaptions;
  FActivating := True;
  Activated := False;
  try
    FModified := False;
    FSaveErrorActive := False;
    DetachContainerBoundsHandler;
    FContainer := Container;
    FVerse := Verse;
    FEditor.Visible := False;
    FEditorPanel.Visible := False;

    { Attach the complete control hierarchy before loading the document.
      Loading an empty document positions the hidden keyboard/IME broker at
      its first caret and may create its native window immediately. }
    FEditorPanel.Align := alNone;
    FEditorPanel.Parent := OverlayParent;
    FEditorPanel.SetBounds(Container.Left, Container.Top, Container.Width,
      Container.Height);
    FEditorPanel.Anchors := Container.Anchors;
    FBoundsContainer := Container;
    FBoundsContainer.AddHandlerOnChangeBounds(@ContainerBoundsChanged);

    FEditor.BaseDirection := Verse.BaseDirection;
    FEditor.Editable := not Verse.ReadOnly;
    FEditor.SetEditorFont(Verse.Font);
    FEditor.TokenizerVersion := Verse.TokenizerVersion;
    FEditor.AutomaticNoteStart := AAutomaticNoteStart;
    FEditor.Text := Verse.XML;
    if FEditor.Capability = twlcRawOnly then
      FEditor.RawMode := True;

    FEditor.Visible := True;
    UpdateEditorState;
    FEditorPanel.BringToFront;
    FEditorPanel.Visible := True;
    UpdateEditorBounds;
    Activated := True;
  finally
    FActivating := False;
    if not Activated then
    begin
      FEditor.Visible := False;
      FEditorPanel.Visible := False;
      DetachContainerBoundsHandler;
      FContainer := nil;
      FVerse := nil;
    end;
  end;
  FEditor.FocusEditor;
end;

procedure TVerseMemo.Deactivate;
begin
  TryDeactivate;
end;

function TVerseMemo.HasPendingChange: Boolean;
begin
  Result := Assigned(FEditorPanel) and FEditorPanel.Visible and FModified;
end;

function TVerseMemo.TryDeactivate: Boolean;
var
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  Result := True;
  Impact.PreservedMembers := 0;
  Impact.RemovedMembers := 0;
  Impact.UsedLegacyFallback := False;
  Impact.ErrorMessage := '';
  FEditor.DismissTranslatorNotePreview;
  if not FEditorPanel.Visible then
    Exit;
  try
    if not FEditor.CommitRaw then
    begin
      ShowSaveError(SEditApplyFailed);
      Exit(False);
    end;
  except
    on E: Exception do
    begin
      ShowSaveError(E.Message);
      Exit(False);
    end;
  end;
  FModified := FEditor.Modified;
  if not ValidateTextForSave then
    Exit(False);
  if not FEditor.ProtectedLexicalObjectsUnchanged then
  begin
    ShowSaveError(SProtectedLexicalObjectsChanged);
    Exit(False);
  end;
  Trace := nil;
  try
    if FModified then
    begin
      Trace := FEditor.CloneEditTrace;
      if not Assigned(Trace) or not Assigned(FOnVerseTextModified) then
      begin
        ShowSaveError(SEditApplyFailed);
        Exit(False);
      end;
      try
        if not FOnVerseTextModified(Self, Trace, Impact) then
        begin
          ShowSaveError(Impact.ErrorMessage);
          Exit(False);
        end;
      except
        on E: Exception do
        begin
          ShowSaveError(E.Message);
          Exit(False);
        end;
      end;
      FEditor.AcceptChanges;
    end;
  finally
    Trace.Free;
  end;

  FEditor.Visible := False;
  FEditorPanel.Visible := False;
  DetachContainerBoundsHandler;
  RestoreFocusAfterClose;
  FModified := False;
  FSaveErrorActive := False;
  FContainer := nil;
  FVerse := nil;
  if Assigned(FOnPendingChange) then
    FOnPendingChange(Self);
end;

procedure TVerseMemo.DiscardEdit;
begin
  FEditor.DismissTranslatorNotePreview;
  if not FEditorPanel.Visible then
    Exit;
  FEditor.Cancel;
  FModified := False;
  FSaveErrorActive := False;
  FEditor.Visible := False;
  FEditorPanel.Visible := False;
  RestoreFocusAfterClose;
  DetachContainerBoundsHandler;
  FContainer := nil;
  FVerse := nil;
  if Assigned(FOnPendingChange) then
    FOnPendingChange(Self);
end;

end.
