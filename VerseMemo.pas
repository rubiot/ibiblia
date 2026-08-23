unit VerseMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, LCLType, Dialogs, ExtCtrls, StdCtrls, ColorBox,
  Buttons, Graphics, Menus, Verse, Math, directwrite_verse_editor, theword_ont_codec_v2,
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
      AAutomaticNoteStart: Integer = 0;
      ATranslatorNoteId: QWord = 0;
      ATranslatorNoteRevision: QWord = 0);
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
  SStrikethroughButton = 'S';
  SSuperscriptButton = 'x²';
  SSubscriptButton = 'x₂';
  SHtmlFormatButton = 'B/I/U';
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
  STitleHint = 'Insert, change or remove a title';
  SNewTitleLevel1 = 'New level 1 title...';
  SNewTitleLevel2 = 'New level 2 title...';
  SNewTitleLevel3 = 'New level 3 title...';
  SChangeTitleLevel1 = 'Change to level 1';
  SChangeTitleLevel2 = 'Change to level 2';
  SChangeTitleLevel3 = 'Change to level 3';
  SRemoveTitle = 'Remove title';
  SInsertTitleDialogTitle = 'Insert title';
  SInsertTitlePrompt = 'Title text:';
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
  P := Mouse.CursorPos;
  FHtmlFormatMenu.PopUp(P.X, P.Y);
end;

procedure TVerseMemo.DecreaseFontSize(Sender: TObject);
begin
  FEditor.AdjustRelativeFontSize(-1);
end;
procedure TVerseMemo.SelectFontColor(Sender: TObject);
begin
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
          Item.Enabled := FEditor.CanInsertTitle;
          Item.Checked := False;
        end;
      11..13:
        begin
          Item.Enabled := HasTitle;
          Item.Checked := HasTitle and (Title.Level = Item.Tag - 10);
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
  UpdateTitleMenu;
  P := Mouse.CursorPos;
  FTitleMenu.PopUp(P.X, P.Y);
end;

procedure TVerseMemo.TitleMenuClick(Sender: TObject);
var
  Item: TMenuItem;
  Succeeded: Boolean;
  TitleText: string;
begin
  if not (Sender is TMenuItem) then
    Exit;
  Item := TMenuItem(Sender);
  Succeeded := False;
  case Item.Tag of
    1..3:
      begin
        TitleText := '';
        if not InputQuery(SInsertTitleDialogTitle, SInsertTitlePrompt,
          TitleText) then
          Exit;
        if Trim(TitleText) = '' then
        begin
          MessageDlg(STitleEditErrorTitle, STitleEditError, mtError,
            [mbOK], 0);
          Exit;
        end;
        Succeeded := FEditor.InsertTitle(Item.Tag,
          UTF8Decode(TitleText));
      end;
    11..13:
      Succeeded := FEditor.ChangeCurrentTitleLevel(Item.Tag - 10);
    20:
      Succeeded := FEditor.RemoveCurrentTitle;
  end;
  if not Succeeded then
    MessageDlg(STitleEditErrorTitle, STitleEditError, mtError, [mbOK], 0);
  UpdateEditorState;
end;

procedure TVerseMemo.IncreaseFontSize(Sender: TObject);
begin
  FEditor.AdjustRelativeFontSize(1);
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
  FStrikethroughButton.Caption := SStrikethroughButton;
  FStrikethroughButton.Hint := SStrikethroughHint;
  FSuperscriptButton.Caption := SSuperscriptButton;
  FSuperscriptButton.Hint := SSuperscriptHint;
  FSubscriptButton.Caption := SSubscriptButton;
  FSubscriptButton.Hint := SSubscriptHint;
  FTranslatorNoteButton.Caption := STranslatorNoteButton;
  FTitleButton.Caption := STitleButton;
  FTitleButton.Hint := STitleHint;
  FFontColorBox.Hint := SFontColorHint;
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
  Title: TTheWordTitleRange;
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
  FStrikethroughButton.Enabled := CanMark;
  FSuperscriptButton.Enabled := CanMark;
  FSubscriptButton.Enabled := CanMark;
  FHtmlFormatButton.Enabled := CanMark;
  FFontSizeDecreaseButton.Enabled := FEditor.Editable and not FEditor.RawMode;
  FFontSizeIncreaseButton.Enabled := FEditor.Editable and not FEditor.RawMode;
  FTranslatorNoteButton.Enabled := FEditor.CanInsertTranslatorNote;
  FTitleButton.Enabled := FEditor.CanInsertTitle or
    FEditor.TryGetCurrentTitle(Title);
  FFontColorBox.Enabled := FEditor.Editable and not FEditor.RawMode;
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
  AddHtmlMenuItem('Bold (<b>)', @InsertHtmlBold);
  AddHtmlMenuItem('Italic (<i>)', @InsertHtmlItalic);
  AddHtmlMenuItem('Underline (<u>)', @InsertHtmlUnderline);
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
  AddTitleMenuItem(SNewTitleLevel1, 1);
  AddTitleMenuItem(SNewTitleLevel2, 2);
  AddTitleMenuItem(SNewTitleLevel3, 3);
  AddTitleSeparator;
  AddTitleMenuItem(SChangeTitleLevel1, 11);
  AddTitleMenuItem(SChangeTitleLevel2, 12);
  AddTitleMenuItem(SChangeTitleLevel3, 13);
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
  if Activated and (ATranslatorNoteId <> 0) then
    EditDirectedTranslatorNote;
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
