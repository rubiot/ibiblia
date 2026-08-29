unit VerseMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, LCLType, Dialogs, ExtCtrls, StdCtrls, ColorBox,
  Buttons, Graphics, Menus, Verse, Math, directwrite_verse_editor, theword_ont_codec_v2,
  theword_wysiwyg_editor_control, text_edit_provenance;

type
  TVerseMemo = class;
  TVerseMemoTransitionState = (
    vmtsNone,
    vmtsRequested,
    vmtsSealed,
    vmtsRetargeted
  );
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
    FStrikethroughButton: TSpeedButton;
    FSuperscriptButton: TSpeedButton;
    FSubscriptButton: TSpeedButton;
    FHtmlFormatButton: TSpeedButton;
    FFontSizeDecreaseButton: TSpeedButton;
    FFontSizeIncreaseButton: TSpeedButton;
    FFontColorBox: TColorBox;
    FHtmlFormatMenu: TPopupMenu;
    FTitleButton: TSpeedButton;
    FTitleMenu: TPopupMenu;
    FTranslatorNoteButton: TSpeedButton;
    FRawModeButton: TButton;
    FSaveErrorActive: Boolean;
    FTransitionState: TVerseMemoTransitionState;
    FTransitionSourceVerse: TVerse;
    FTransitionSourceReferenceID: string;
    FTransitionSourceVisualRevision: QWord;
    FTransitionTargetReferenceID: string;
    FTransitionTargetEditable: Boolean;
    FTransitionTargetRawMode: Boolean;
    FToolbarLayoutKey: string;
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
    procedure EditorSelectionChanged(Sender: TTheWordWysiwygEditorControl);
    procedure EditorCommand(Sender: TTheWordWysiwygEditorControl;
      ACommand: TVerseEditorCommand);
    procedure InsertAddedWords(Sender: TObject);
    procedure InsertJesusWords(Sender: TObject);
    procedure InsertOldTestamentQuote(Sender: TObject);
    procedure InsertUnderline(Sender: TObject);
    procedure InsertStrikethrough(Sender: TObject);
    procedure InsertSuperscript(Sender: TObject);
    procedure InsertSubscript(Sender: TObject);
    procedure InsertHtmlBold(Sender: TObject);
    procedure InsertHtmlItalic(Sender: TObject);
    procedure InsertHtmlUnderline(Sender: TObject);
    procedure ShowHtmlFormatMenu(Sender: TObject);
    procedure ShowTitleMenu(Sender: TObject);
    procedure TitleMenuClick(Sender: TObject);
    procedure UpdateTitleMenu;
    procedure DecreaseFontSize(Sender: TObject);
    procedure IncreaseFontSize(Sender: TObject);
    procedure SelectFontColor(Sender: TObject);
    procedure InsertTranslatorNote(Sender: TObject);
    procedure LayoutEditorToolbar;
    procedure CloseEditor(ARestoreFocus, ANotifyPendingChange: Boolean);
    procedure ClearTransitionState;
    procedure RefreshLocalizedCaptions;
    procedure RestoreFocusAfterClose;
    procedure SaveAndClose;
    procedure SaveButtonClick(Sender: TObject);
    procedure ShowSaveError(const AMessage: string);
    procedure ToggleRawMode(Sender: TObject);
    procedure ToolbarResize(Sender: TObject);
    procedure UpdateEditorBounds;
    procedure UpdateEditorStatus;
    procedure UpdateEditorState;
    procedure SetEditorEditableSilently(AEditable: Boolean);
    function TryCommitActiveEdit: Boolean;
    function ValidateTextForSave: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Activate(Container: TScrollBox; Verse: TVerse;
      AAutomaticNoteStart: Integer = 0;
      ATranslatorNoteId: QWord = 0;
      ATranslatorNoteRevision: QWord = 0);
    procedure Deactivate;
    procedure Translate;
    function BeginVerseTransition: Boolean;
    function RetargetVerseTransition(Container: TScrollBox; Verse: TVerse;
      AAutomaticNoteStart: Integer = 0): Boolean;
    procedure CompleteVerseTransition;
    procedure AbortVerseTransition;
    function HasPendingChange: Boolean;
    function IsActiveFor(Verse: TVerse): Boolean;
    function IsTransitioning: Boolean;
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
  SStrikethroughButton = 'S';
  SSuperscriptButton = 'x²';
  SSubscriptButton = 'x₂';
  SHtmlFormatButton = 'B/I/U';
  SHtmlBoldMenuItem = 'Bold (<b>)';
  SHtmlItalicMenuItem = 'Italic (<i>)';
  SHtmlUnderlineMenuItem = 'Underline (<u>)';
  SFontSizeDecreaseButton = 'A−';
  SFontSizeIncreaseButton = 'A+';
  STitleButton = 'T▾';
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
  SStrikethroughHint =
    'Strikethrough: apply or remove strikethrough (Ctrl+Shift+S)';
  SSuperscriptHint =
    'Superscript: apply or remove superscript (Ctrl+Shift+=)';
  SSubscriptHint = 'Subscript: apply or remove subscript (Ctrl+=)';
  SHtmlFormatHint = 'HTML bold, italic or underline';
  SFontSizeDecreaseHint = 'Decrease relative font size (Ctrl+Shift+,)';
  SFontSizeIncreaseHint = 'Increase relative font size (Ctrl+Shift+.)';
  SFontColorHint = 'Font color; choose Default to remove explicit color';
  STitleHint = 'Apply, change or remove title formatting';
  STitleLevel1 = 'Title level 1';
  STitleLevel2 = 'Title level 2';
  STitleLevel3 = 'Title level 3';
  SRemoveTitle = 'Remove title formatting';
  STitleEditErrorTitle = 'Title edit error';
  STitleEditError = 'The title could not be changed at this position.';
  STranslatorNoteHint =
    'Insert translator note at the caret (Ctrl+Shift+N)';
  SRawModeHint = 'Switch between semantic editing and the raw ONT fallback';
  SLineBreaksInEditorTitle = 'Edit error';
  SLineBreaksInEditor = 'A verse cannot contain line breaks. Remove line breaks before saving.';
  SEditApplyFailed = 'The edit could not be applied';
  SProtectedLexicalObjectsChanged =
    'Strong and morphology data are protected in this editing cycle';
  STranslatorNoteNotFound =
    'The translator note could not be located in the current verse.';

implementation

uses
  performance_debug;

function TVerseMemo.GetText: TCaption;
begin
  Result := FEditor.Text;
end;

procedure TVerseMemo.ApplySemantic(AKind: TTheWordSemanticKind);
begin
  if IsTransitioning then
    Exit;
  FEditor.ApplySemantic(AKind);
end;

procedure TVerseMemo.CancelButtonClick(Sender: TObject);
begin
  if IsTransitioning then
    Exit;
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
  if FActivating or IsTransitioning or not FEditor.Visible then
    Exit;
  FSaveErrorActive := False;
  FModified := FEditor.Modified;
  UpdateEditorState;
  if Assigned(FOnPendingChange) then
    FOnPendingChange(Self);
end;

procedure TVerseMemo.EditorSelectionChanged(
  Sender: TTheWordWysiwygEditorControl);
begin
  if FActivating or IsTransitioning or not FEditor.Visible then
    Exit;
  UpdateEditorState;
end;

procedure TVerseMemo.EditorCommand(Sender: TTheWordWysiwygEditorControl;
  ACommand: TVerseEditorCommand);
begin
  if IsTransitioning then
    Exit;
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

procedure TVerseMemo.InsertStrikethrough(Sender: TObject);
begin
  ApplySemantic(twsmStrikethrough);
end;

procedure TVerseMemo.InsertSuperscript(Sender: TObject);
begin
  ApplySemantic(twsmSuperscript);
end;

procedure TVerseMemo.InsertSubscript(Sender: TObject);
begin
  ApplySemantic(twsmSubscript);
end;

procedure TVerseMemo.InsertHtmlBold(Sender: TObject);
begin
  ApplySemantic(twsmHtmlBold);
end;

procedure TVerseMemo.InsertHtmlItalic(Sender: TObject);
begin
  ApplySemantic(twsmHtmlItalic);
end;

procedure TVerseMemo.InsertHtmlUnderline(Sender: TObject);
begin
  ApplySemantic(twsmHtmlUnderline);
end;

procedure TVerseMemo.ShowHtmlFormatMenu(Sender: TObject);
var P: TPoint;
begin
  if IsTransitioning then
    Exit;
  P := Mouse.CursorPos;
  FHtmlFormatMenu.PopUp(P.X, P.Y);
end;

procedure TVerseMemo.DecreaseFontSize(Sender: TObject);
begin
  if IsTransitioning then
    Exit;
  FEditor.AdjustRelativeFontSize(-1);
end;
procedure TVerseMemo.SelectFontColor(Sender: TObject);
begin
  if IsTransitioning then
    Exit;
  FEditor.ApplyFontColor(FFontColorBox.Selected <> clDefault,
    LongInt(FFontColorBox.Selected));
end;

procedure TVerseMemo.UpdateTitleMenu;
var
  HasTitle: Boolean;
  Index: Integer;
  Item: TMenuItem;
  Title: TTheWordTitleRange;
begin
  HasTitle := FEditor.TryGetCurrentTitle(Title);
  for Index := 0 to FTitleMenu.Items.Count - 1 do
  begin
    Item := FTitleMenu.Items[Index];
    case Item.Tag of
      1..3:
        begin
          Item.Enabled := FEditor.CanApplyTitle;
          Item.Checked := HasTitle and (Title.Level = Item.Tag);
        end;
      20:
        begin
          Item.Enabled := HasTitle;
          Item.Checked := False;
        end;
    end;
  end;
end;

procedure TVerseMemo.ShowTitleMenu(Sender: TObject);
var
  P: TPoint;
begin
  if IsTransitioning then
    Exit;
  UpdateTitleMenu;
  P := Mouse.CursorPos;
  FTitleMenu.PopUp(P.X, P.Y);
end;

procedure TVerseMemo.TitleMenuClick(Sender: TObject);
var
  Item: TMenuItem;
  Succeeded: Boolean;
begin
  if IsTransitioning then
    Exit;
  if not (Sender is TMenuItem) then
    Exit;
  Item := TMenuItem(Sender);
  Succeeded := False;
  case Item.Tag of
    1..3:
      Succeeded := FEditor.ApplyTitleLevel(Item.Tag);
    20:
      Succeeded := FEditor.RemoveCurrentTitle;
  end;
  if not Succeeded then
    MessageDlg(STitleEditErrorTitle, STitleEditError, mtError, [mbOK], 0);
  UpdateEditorState;
end;

procedure TVerseMemo.IncreaseFontSize(Sender: TObject);
begin
  if IsTransitioning then
    Exit;
  FEditor.AdjustRelativeFontSize(1);
end;

procedure TVerseMemo.InsertTranslatorNote(Sender: TObject);
begin
  if IsTransitioning then
    Exit;
  FEditor.InsertTranslatorNote;
end;

procedure TVerseMemo.ToggleRawMode(Sender: TObject);
begin
  if IsTransitioning then
    Exit;
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
    ActionButtonTop, ErrorTop, StatusLeft, StatusTop, StatusWidth: Integer;
  ActionsWrapped: Boolean;
  LayoutKey: string;
begin
  if not Assigned(FEditorPanel) or not Assigned(FToolbarPanel) or
     not Assigned(FEditorPanel.Parent) or
     (FToolbarPanel.ClientWidth <= 0) then
    Exit;
  LayoutKey := Format('%d|%d|%s|%s|%s|%s|%s|%d', [
    FToolbarPanel.ClientWidth, Ord(FSaveErrorActive), FModeLabel.Caption,
    FRawModeButton.Caption, FSaveButton.Caption, FCancelButton.Caption,
    FToolbarPanel.Font.Name, FToolbarPanel.Font.Size]);
  if LayoutKey = FToolbarLayoutKey then
    Exit;
  { Store the key before SetBounds: changing the toolbar height can re-enter
    resize handlers synchronously on Win32. }
  FToolbarLayoutKey := LayoutKey;
  IBibliaPerfIncrement(ipcToolbarLayouts);

  FToolbarPanel.Canvas.Font.Assign(FToolbarPanel.Font);
  ButtonHeight := Max(26, FToolbarPanel.Canvas.TextHeight('Hg') + 10);
  ButtonTop := (ButtonRowHeight - ButtonHeight) div 2;
  SaveWidth := Max(72, FToolbarPanel.Canvas.TextWidth(FSaveButton.Caption) + 26);
  CancelWidth := Max(72,
    FToolbarPanel.Canvas.TextWidth(FCancelButton.Caption) + 26);
  ModeWidth := Max(90,
    FToolbarPanel.Canvas.TextWidth(FModeLabel.Caption) + 10);

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
  FStrikethroughButton.SetBounds(X,
    ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);
  FSuperscriptButton.SetBounds(X,
    ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);
  FSubscriptButton.SetBounds(X,
    ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);
  FHtmlFormatButton.SetBounds(X, ButtonTop,
    SemanticButtonWidth + 14, ButtonHeight);
  Inc(X, SemanticButtonWidth + 14 + Gap);
  FFontSizeDecreaseButton.SetBounds(X, ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);
  FFontSizeIncreaseButton.SetBounds(X, ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);
  FFontColorBox.SetBounds(X, ButtonTop, 92, ButtonHeight);
  Inc(X, 92 + Gap);
  FTitleButton.SetBounds(X, ButtonTop,
    SemanticButtonWidth + 8, ButtonHeight);
  Inc(X, SemanticButtonWidth + 8 + Gap);
  FTranslatorNoteButton.SetBounds(X, ButtonTop,
    SemanticButtonWidth, ButtonHeight);
  Inc(X, SemanticButtonWidth + Gap);

  { Keep the mode/save actions together, but move them to a second row when
    the formatting controls consume their reserved area. This preserves every
    command at narrow widths instead of allowing Raw to slide under Save. }
  RightEdge := FToolbarPanel.ClientWidth - Margin - CancelWidth - Gap -
    SaveWidth - Gap;
  ActionsWrapped := X + RawModeButtonWidth > RightEdge;
  if ActionsWrapped then
  begin
    ActionButtonTop := ButtonTop + ButtonRowHeight;
    ErrorTop := 2 * ButtonRowHeight;
    StatusTop := ButtonRowHeight;
  end
  else
  begin
    ActionButtonTop := ButtonTop;
    ErrorTop := ButtonRowHeight;
    StatusTop := 0;
  end;
  if FSaveErrorActive then
    FToolbarPanel.Height := ErrorTop + ErrorRowHeight
  else
    FToolbarPanel.Height := ErrorTop;

  RightEdge := FToolbarPanel.ClientWidth - Margin;
  FCancelButton.SetBounds(RightEdge - CancelWidth, ActionButtonTop,
    CancelWidth, ButtonHeight);
  Dec(RightEdge, CancelWidth + Gap);
  FSaveButton.SetBounds(RightEdge - SaveWidth, ActionButtonTop,
    SaveWidth, ButtonHeight);
  Dec(RightEdge, SaveWidth + Gap);

  if ActionsWrapped then
    X := Margin;
  FRawModeButton.SetBounds(X, ActionButtonTop,
    RawModeButtonWidth, ButtonHeight);
  X := FRawModeButton.Left + FRawModeButton.Width + Gap;

  if FSaveErrorActive then
  begin
    FStatusLabel.Visible := True;
    FStatusLabel.SetBounds(Margin, ErrorTop,
      Max(0, FToolbarPanel.ClientWidth - (2 * Margin)), ErrorRowHeight);
  end
  else
  begin
    StatusLeft := X;
    StatusWidth := RightEdge - StatusLeft;
    FStatusLabel.Visible := StatusWidth >= 80;
    if FStatusLabel.Visible then
      FStatusLabel.SetBounds(StatusLeft, StatusTop,
        StatusWidth, ButtonRowHeight);
  end;
end;

procedure TVerseMemo.RefreshLocalizedCaptions;
var
  ColorBoxStyle: TColorBoxStyle;
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
  FStrikethroughButton.Caption := SStrikethroughButton;
  FStrikethroughButton.Hint := SStrikethroughHint;
  FSuperscriptButton.Caption := SSuperscriptButton;
  FSuperscriptButton.Hint := SSuperscriptHint;
  FSubscriptButton.Caption := SSubscriptButton;
  FSubscriptButton.Hint := SSubscriptHint;
  FHtmlFormatButton.Caption := SHtmlFormatButton;
  FHtmlFormatButton.Hint := SHtmlFormatHint;
  FFontSizeDecreaseButton.Caption := SFontSizeDecreaseButton;
  FFontSizeDecreaseButton.Hint := SFontSizeDecreaseHint;
  FFontSizeIncreaseButton.Caption := SFontSizeIncreaseButton;
  FFontSizeIncreaseButton.Hint := SFontSizeIncreaseHint;
  FTranslatorNoteButton.Caption := STranslatorNoteButton;
  FTitleButton.Caption := STitleButton;
  FTitleButton.Hint := STitleHint;
  FFontColorBox.Hint := SFontColorHint;
  FTranslatorNoteButton.Hint := STranslatorNoteHint;
  if FEditor.RawMode then
    FRawModeButton.Caption := SVisualModeButton
  else
    FRawModeButton.Caption := SRawModeButton;
  FRawModeButton.Hint := SRawModeHint;

  FHtmlFormatMenu.Items[0].Caption := SHtmlBoldMenuItem;
  FHtmlFormatMenu.Items[1].Caption := SHtmlItalicMenuItem;
  FHtmlFormatMenu.Items[2].Caption := SHtmlUnderlineMenuItem;
  FTitleMenu.Items[0].Caption := STitleLevel1;
  FTitleMenu.Items[1].Caption := STitleLevel2;
  FTitleMenu.Items[2].Caption := STitleLevel3;
  FTitleMenu.Items[4].Caption := SRemoveTitle;

  { TColorBox caches the LCL resource strings when its list is built. Rebuild
    that list after SetDefaultLang so an already open editor changes language
    along with the rest of the application. }
  ColorBoxStyle := FFontColorBox.Style;
  if cbPrettyNames in ColorBoxStyle then
  begin
    FFontColorBox.Style := ColorBoxStyle - [cbPrettyNames];
    FFontColorBox.Style := ColorBoxStyle;
  end;
end;

procedure TVerseMemo.ClearTransitionState;
begin
  FTransitionState := vmtsNone;
  FTransitionSourceVerse := nil;
  FTransitionSourceReferenceID := '';
  FTransitionSourceVisualRevision := 0;
  FTransitionTargetReferenceID := '';
  FTransitionTargetEditable := False;
  FTransitionTargetRawMode := False;
  FToolbarLayoutKey := '';
end;

procedure TVerseMemo.CloseEditor(ARestoreFocus,
  ANotifyPendingChange: Boolean);
begin
  FEditor.DismissTranslatorNotePreview;
  FEditor.Visible := False;
  FEditorPanel.Visible := False;
  DetachContainerBoundsHandler;
  if ARestoreFocus then
    RestoreFocusAfterClose;
  FModified := False;
  FSaveErrorActive := False;
  FContainer := nil;
  FVerse := nil;
  ClearTransitionState;
  if ANotifyPendingChange and Assigned(FOnPendingChange) then
    FOnPendingChange(Self);
end;

procedure TVerseMemo.SetEditorEditableSilently(AEditable: Boolean);
var
  WasActivating: Boolean;
begin
  WasActivating := FActivating;
  FActivating := True;
  try
    FEditor.Editable := AEditable;
  finally
    FActivating := WasActivating;
  end;
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

procedure TVerseMemo.UpdateEditorStatus;
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
  FEditorPanel.Hint := FStatusLabel.Caption;
end;

procedure TVerseMemo.UpdateEditorState;
var
  CanMark: Boolean;
  Title: TTheWordTitleRange;
begin
  if not Assigned(FStatusLabel) then
    Exit;
  { Keep the toolbar visually stable while the dataset and the editor session
    are being retargeted. CompleteVerseTransition refreshes it after editing
    has been enabled for the new verse. }
  if IsTransitioning then
  begin
    LayoutEditorToolbar;
    Exit;
  end;
  UpdateEditorStatus;

  CanMark := FEditor.CanApplySemantic;
  FAddedWordsButton.Enabled := CanMark;
  FJesusWordsButton.Enabled := CanMark;
  FOldTestamentQuoteButton.Enabled := CanMark;
  FUnderlineButton.Enabled := CanMark;
  FStrikethroughButton.Enabled := CanMark;
  FSuperscriptButton.Enabled := CanMark;
  FSubscriptButton.Enabled := CanMark;
  FHtmlFormatButton.Enabled := CanMark;
  FFontSizeDecreaseButton.Enabled := FEditor.Editable and not FEditor.RawMode;
  FFontSizeIncreaseButton.Enabled := FEditor.Editable and not FEditor.RawMode;
  FTranslatorNoteButton.Enabled := FEditor.CanInsertTranslatorNote;
  FTitleButton.Enabled := FEditor.CanApplyTitle or
    FEditor.TryGetCurrentTitle(Title);
  FFontColorBox.Enabled := FEditor.Editable and not FEditor.RawMode;
  if FEditor.RawMode then
    FRawModeButton.Caption := SVisualModeButton
  else
    FRawModeButton.Caption := SRawModeButton;
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

  procedure AddHtmlMenuItem(const ACaption: string; AHandler: TNotifyEvent);
  var Item: TMenuItem;
  begin
    Item := TMenuItem.Create(FHtmlFormatMenu);
    Item.Caption := ACaption;
    Item.OnClick := AHandler;
    FHtmlFormatMenu.Items.Add(Item);
  end;

  procedure AddTitleMenuItem(const ACaption: string; ATag: Integer);
  var Item: TMenuItem;
  begin
    Item := TMenuItem.Create(FTitleMenu);
    Item.Caption := ACaption;
    Item.Tag := ATag;
    Item.AutoCheck := False;
    Item.OnClick := @TitleMenuClick;
    FTitleMenu.Items.Add(Item);
  end;

  procedure AddTitleSeparator;
  var Item: TMenuItem;
  begin
    Item := TMenuItem.Create(FTitleMenu);
    Item.Caption := '-';
    Item.Tag := 0;
    FTitleMenu.Items.Add(Item);
  end;

begin
  FEditorPanel := TPanel.Create(nil);
  FEditorPanel.Visible := False;
  FEditorPanel.Align := alNone;
  FEditorPanel.BevelOuter := bvLowered;
  FEditorPanel.ParentColor := False;
  FEditorPanel.Color := clWindow;
  { The toolbar already displays the editor status.  Do not repeat it as a
    tooltip over the WYSIWYG surface, especially over translator-note
    previews.  Button hints remain enabled individually below. }
  FEditorPanel.ShowHint := False;

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
  FStrikethroughButton := NewSemanticButton(SStrikethroughButton,
    SStrikethroughHint, @InsertStrikethrough);
  FStrikethroughButton.Font.Style := [fsStrikeOut];
  FSuperscriptButton := NewSemanticButton(SSuperscriptButton,
    SSuperscriptHint, @InsertSuperscript);
  FSubscriptButton := NewSemanticButton(SSubscriptButton,
    SSubscriptHint, @InsertSubscript);
  FHtmlFormatMenu := TPopupMenu.Create(FEditorPanel);
  AddHtmlMenuItem(SHtmlBoldMenuItem, @InsertHtmlBold);
  AddHtmlMenuItem(SHtmlItalicMenuItem, @InsertHtmlItalic);
  AddHtmlMenuItem(SHtmlUnderlineMenuItem, @InsertHtmlUnderline);
  FHtmlFormatButton := NewSemanticButton(SHtmlFormatButton,
    SHtmlFormatHint, @ShowHtmlFormatMenu);
  FFontSizeDecreaseButton := NewSemanticButton(SFontSizeDecreaseButton,
    SFontSizeDecreaseHint, @DecreaseFontSize);
  FFontSizeIncreaseButton := NewSemanticButton(SFontSizeIncreaseButton,
    SFontSizeIncreaseHint, @IncreaseFontSize);
  FTranslatorNoteButton := NewSemanticButton(STranslatorNoteButton,
    STranslatorNoteHint, @InsertTranslatorNote);
  FFontColorBox := TColorBox.Create(FEditorPanel);
  FFontColorBox.Parent := FToolbarPanel;
  FFontColorBox.Style := [cbStandardColors, cbExtendedColors,
    cbIncludeDefault, cbCustomColor, cbPrettyNames];
  FFontColorBox.Selected := clDefault;
  FFontColorBox.Hint := SFontColorHint;
  FFontColorBox.ShowHint := True;
  FFontColorBox.OnChange := @SelectFontColor;
  FTitleMenu := TPopupMenu.Create(FEditorPanel);
  AddTitleMenuItem(STitleLevel1, 1);
  AddTitleMenuItem(STitleLevel2, 2);
  AddTitleMenuItem(STitleLevel3, 3);
  AddTitleSeparator;
  AddTitleMenuItem(SRemoveTitle, 20);
  FTitleButton := NewSemanticButton(STitleButton,
    STitleHint, @ShowTitleMenu);
  FTitleButton.Font.Style := [fsBold];

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
  FEditor.ShowHint := False;
  FEditor.Parent := FEditorPanel;
  FEditor.Align := alClient;
  FEditor.OnChanged := @EditorChanged;
  FEditor.OnSelectionChanged := @EditorSelectionChanged;
  FEditor.OnCommand := @EditorCommand;

  FActivating := False;
  FModified := False;
  FSaveErrorActive := False;
  ClearTransitionState;
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
  AAutomaticNoteStart: Integer; ATranslatorNoteId,
  ATranslatorNoteRevision: QWord);
var
  Activated: Boolean;
  OverlayParent: TWinControl;

  procedure EditDirectedTranslatorNote;
  begin
    try
      { Never resolve a stale ID in a newer document: IDs are only meaningful
        within the visual revision that produced the clicked marker. }
      if (ATranslatorNoteRevision <> 0) and
         (ATranslatorNoteRevision <> Verse.VisualRevision) then
      begin
        ShowSaveError(STranslatorNoteNotFound);
        Exit;
      end;
      if not FEditor.EditTranslatorNote(ATranslatorNoteId) then
        ShowSaveError(STranslatorNoteNotFound);
    except
      on E: Exception do
        ShowSaveError(E.Message);
    end;
  end;
begin
  if not Assigned(Container) or not Assigned(Verse) then
    Exit;
  { A queued activation from the old navigation path must not interrupt a
    sealed or already retargeted continuous transition. }
  if IsTransitioning then
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
      if ATranslatorNoteId <> 0 then
        EditDirectedTranslatorNote;
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

    FEditor.Editable := not Verse.ReadOnly;
    FEditor.LoadVerseContext(Verse.XML, Verse.BaseDirection, Verse.Font,
      Verse.TokenizerVersion, AAutomaticNoteStart);
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
  if Activated and (ATranslatorNoteId <> 0) then
    EditDirectedTranslatorNote;
  FEditor.FocusEditor;
end;

procedure TVerseMemo.Deactivate;
begin
  TryDeactivate;
end;

procedure TVerseMemo.Translate;
begin
  RefreshLocalizedCaptions;
  UpdateEditorState;
end;

function TVerseMemo.HasPendingChange: Boolean;
begin
  Result := Assigned(FEditorPanel) and FEditorPanel.Visible and FModified;
end;

function TVerseMemo.IsActiveFor(Verse: TVerse): Boolean;
begin
  Result := Assigned(Verse) and Assigned(FEditorPanel) and
    FEditorPanel.Visible and (FVerse = Verse);
end;

function TVerseMemo.IsTransitioning: Boolean;
begin
  Result := FTransitionState <> vmtsNone;
end;

function TVerseMemo.TryCommitActiveEdit: Boolean;
var
  Impact: TAssociationEditImpact;
  Trace: TVerseEditTrace;
begin
  Result := False;
  Impact.PreservedMembers := 0;
  Impact.RemovedMembers := 0;
  Impact.UsedLegacyFallback := False;
  Impact.Cancelled := False;
  Impact.ErrorMessage := '';
  FEditor.DismissTranslatorNotePreview;
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
          if Impact.Cancelled then
            FEditor.FocusEditor
          else
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

  FModified := False;
  FSaveErrorActive := False;
  Result := True;
end;

function TVerseMemo.BeginVerseTransition: Boolean;
var
  RestoreEditable: Boolean;
begin
  if IsTransitioning then
    Exit(True);
  if not Assigned(FEditorPanel) or not FEditorPanel.Visible or
     not Assigned(FVerse) then
    Exit(False);

  FTransitionState := vmtsRequested;
  FTransitionSourceVerse := FVerse;
  FTransitionSourceReferenceID := FVerse.ReferenceID;
  FTransitionSourceVisualRevision := 0;
  FTransitionTargetReferenceID := '';
  FTransitionTargetEditable := FEditor.Editable;
  FTransitionTargetRawMode := False;
  SetEditorEditableSilently(False);

  if not TryCommitActiveEdit then
  begin
    RestoreEditable := FTransitionTargetEditable;
    ClearTransitionState;
    SetEditorEditableSilently(RestoreEditable);
    Exit(False);
  end;

  { Capture the revision after applying the edit. A dataset load mutates the
    persistent TVerse instance in place, so pointer identity alone cannot tell
    whether an aborted navigation is still showing the source verse. }
  FTransitionSourceReferenceID := FVerse.ReferenceID;
  FTransitionSourceVisualRevision := FVerse.VisualRevision;
  FTransitionState := vmtsSealed;
  Result := True;
end;

function TVerseMemo.RetargetVerseTransition(Container: TScrollBox;
  Verse: TVerse; AAutomaticNoteStart: Integer): Boolean;
var
  OverlayParent: TWinControl;
  WasActivating: Boolean;
begin
  Result := False;
  if FTransitionState = vmtsRetargeted then
    Exit(Assigned(Container) and Assigned(Verse) and
      (FContainer = Container) and (FVerse = Verse) and
      (FTransitionTargetReferenceID = Verse.ReferenceID));
  if FTransitionState <> vmtsSealed then
    Exit;
  if not Assigned(Container) or not Assigned(Verse) or
     not Assigned(Container.Parent) then
  begin
    AbortVerseTransition;
    Exit;
  end;

  OverlayParent := Container.Parent;
  WasActivating := FActivating;
  FActivating := True;
  try
    try
      FEditor.DismissTranslatorNotePreview;
      FModified := False;
      FSaveErrorActive := False;

      if FBoundsContainer <> Container then
      begin
        DetachContainerBoundsHandler;
        FBoundsContainer := Container;
        FBoundsContainer.AddHandlerOnChangeBounds(@ContainerBoundsChanged);
      end;
      FContainer := Container;
      FEditorPanel.Align := alNone;
      if FEditorPanel.Parent <> OverlayParent then
        FEditorPanel.Parent := OverlayParent;
      FEditorPanel.SetBounds(Container.Left, Container.Top, Container.Width,
        Container.Height);
      FEditorPanel.Anchors := Container.Anchors;

      FTransitionTargetEditable := not Verse.ReadOnly;
      FVerse := Verse;
      FTransitionTargetReferenceID := Verse.ReferenceID;
      FEditor.LoadVerseContext(Verse.XML, Verse.BaseDirection, Verse.Font,
        Verse.TokenizerVersion, AAutomaticNoteStart);
      FTransitionTargetRawMode := FEditor.Capability = twlcRawOnly;

      FEditor.Visible := True;
      FEditorPanel.BringToFront;
      FEditorPanel.Visible := True;
      UpdateEditorBounds;
      FTransitionState := vmtsRetargeted;
      Result := True;
    except
      AbortVerseTransition;
      raise;
    end;
  finally
    FActivating := WasActivating;
  end;
end;

procedure TVerseMemo.CompleteVerseTransition;
var
  TargetEditable, TargetRawMode: Boolean;
begin
  if not IsTransitioning then
    Exit;
  if FTransitionState <> vmtsRetargeted then
  begin
    AbortVerseTransition;
    Exit;
  end;

  TargetEditable := FTransitionTargetEditable;
  TargetRawMode := FTransitionTargetRawMode;
  ClearTransitionState;
  SetEditorEditableSilently(TargetEditable);
  if TargetRawMode then
  begin
    FActivating := True;
    try
      FEditor.RawMode := True;
    finally
      FActivating := False;
    end;
  end;
  FModified := FEditor.Modified;
  FSaveErrorActive := False;
  UpdateEditorState;
  FEditor.FocusEditor;
end;

procedure TVerseMemo.AbortVerseTransition;
var
  KeepEditor, RestoreEditable: Boolean;
begin
  if not IsTransitioning then
    Exit;

  RestoreEditable := FTransitionTargetEditable;
  KeepEditor := Assigned(FEditorPanel) and FEditorPanel.Visible and
    Assigned(FVerse) and (FVerse = FTransitionSourceVerse) and
    (FVerse.ReferenceID = FTransitionSourceReferenceID) and
    (FVerse.VisualRevision = FTransitionSourceVisualRevision);
  ClearTransitionState;
  if KeepEditor then
  begin
    SetEditorEditableSilently(RestoreEditable);
    FModified := FEditor.Modified;
    FSaveErrorActive := False;
    UpdateEditorState;
    FEditor.FocusEditor;
  end
  else
    CloseEditor(True, True);
end;

function TVerseMemo.TryDeactivate: Boolean;
begin
  Result := True;
  { SaveProject, chapter-view refreshes and other observers can re-enter this
    method while navigation is in flight. The source session was already
    committed by BeginVerseTransition and must remain visible. }
  if IsTransitioning then
    Exit;
  if not FEditorPanel.Visible then
    Exit;
  if not TryCommitActiveEdit then
    Exit(False);

  CloseEditor(True, True);
end;

procedure TVerseMemo.DiscardEdit;
begin
  if IsTransitioning then
    Exit;
  FEditor.DismissTranslatorNotePreview;
  if not FEditorPanel.Visible then
    Exit;
  FEditor.Cancel;
  CloseEditor(True, True);
end;

end.
