unit VerseMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, LCLType, SynEdit, SynEditKeyCmds,
  {$IFNDEF UNIX}
  Windows,
  {$ENDIF}
  Dialogs, ExtCtrls, StdCtrls, Graphics, Verse, Math,
  theword_syntax_highlighter;

type

  TVerseMemo = class;
  TVerseTextModifiedEvent = procedure (Sender: TVerseMemo) of object;

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
    FAddedWordsButton: TButton;
    FJesusWordsButton: TButton;
    FOldTestamentQuoteButton: TButton;
    FMemo: TSynEdit;
    FContainer: TScrollBox;
    FBoundsContainer: TScrollBox;
    FVerse: TVerse;
    FOnVerseTextModified: TVerseTextModifiedEvent;
    FOnPendingChange: TNotifyEvent;
    function GetText: TCaption;
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
             WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ApplyTagToSelection(const StartTag, EndTag: string);
    procedure CancelButtonClick(Sender: TObject);
    procedure CancelEdit;
    procedure ContainerBoundsChanged(Sender: TObject);
    procedure DetachContainerBoundsHandler;
    procedure InsertAddedWordsTag(Sender: TObject);
    procedure InsertJesusWordsTag(Sender: TObject);
    procedure InsertOldTestamentQuoteTag(Sender: TObject);
    function HandleEditorCommandKey(var Key: Word; Shift: TShiftState): Boolean;
    procedure LayoutEditorToolbar;
    procedure RefreshLocalizedCaptions;
    procedure RestoreFocusAfterClose;
    procedure SaveAndClose;
    procedure SaveButtonClick(Sender: TObject);
    procedure ToolbarResize(Sender: TObject);
    procedure UpdateEditorBounds;
    procedure UpdateEditorState;
    function ValidateTextForSave: Boolean;
  protected
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnChange(Sender: TObject);
    procedure ConfirmChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Activate(Container: TScrollBox; Verse: TVerse);
    procedure Deactivate;
    function HasPendingChange: Boolean;
    function TryDeactivate: Boolean;
    procedure DiscardEdit;
    property Text: TCaption read GetText;
    property Verse: TVerse read FVerse;
    property OnVerseTextModified: TVerseTextModifiedEvent read FOnVerseTextModified write FOnVerseTextModified;
    property OnPendingChange: TNotifyEvent read FOnPendingChange
      write FOnPendingChange;
  end;

resourcestring
  SChangeVerse = 'Change verse text?';
  SChangeVerseConfirmation = 'Are you sure you want to change the verse text? Some associations may be lost.';
  SEditModeTitle = 'Editing verse';
  SEditModeSaved = 'No changes';
  SEditModeModified = 'Unsaved changes';
  SSaveEditButton = 'Save';
  SSaveEditHint = 'Save verse text and close the editor';
  SCancelEditButton = 'Cancel';
  SCancelEditHint = 'Discard changes and close the editor';
  SAddedWordsTagHint = 'Wrap the selection with <FI>...<Fi> for added words';
  SJesusWordsTagHint = 'Wrap the selection with <FR>...<Fr> for words of Jesus';
  SOldTestamentQuoteTagHint = 'Wrap the selection with <FO>...<Fo> for Old Testament quotations';
  SLineBreaksInEditorTitle = 'Edit error';
  SLineBreaksInEditor = 'A verse cannot contain line breaks. Remove line breaks before saving.';

implementation

function RemoveFinalLineBreak(const Value: string): string;
begin
  Result := Value;
  if (Length(Result) >= 2) and
    (Copy(Result, Length(Result) - 1, 2) = #13#10) then
    SetLength(Result, Length(Result) - 2)
  else if (Length(Result) >= 1) and
    (Result[Length(Result)] in [#10, #13]) then
    SetLength(Result, Length(Result) - 1);
end;

function TVerseMemo.GetText: TCaption;
begin
  Result := RemoveFinalLineBreak(FMemo.Text);
end;

procedure TVerseMemo.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    FMemo.Font.Size := FMemo.Font.Size + Sign(WheelDelta);
end;

procedure TVerseMemo.ApplyTagToSelection(const StartTag, EndTag: string);
var
  Caret: TPoint;
  SelectedText: string;
begin
  SelectedText := FMemo.SelText;
  if SelectedText <> '' then
    FMemo.SelText := StartTag + SelectedText + EndTag
  else
  begin
    Caret := FMemo.LogicalCaretXY;
    FMemo.SelText := StartTag + EndTag;
    Inc(Caret.X, Length(StartTag));
    FMemo.LogicalCaretXY := Caret;
  end;
  FMemo.SetFocus;
end;

procedure TVerseMemo.CancelButtonClick(Sender: TObject);
begin
  DiscardEdit;
end;

procedure TVerseMemo.CancelEdit;
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

procedure TVerseMemo.InsertAddedWordsTag(Sender: TObject);
begin
  ApplyTagToSelection('<FI>', '<Fi>');
end;

procedure TVerseMemo.InsertJesusWordsTag(Sender: TObject);
begin
  ApplyTagToSelection('<FR>', '<Fr>');
end;

procedure TVerseMemo.InsertOldTestamentQuoteTag(Sender: TObject);
begin
  ApplyTagToSelection('<FO>', '<Fo>');
end;

function TVerseMemo.HandleEditorCommandKey(var Key: Word;
  Shift: TShiftState): Boolean;
var
  Command: TSynEditorCommand;
  BasicShift: TShiftState;
begin
  Result := False;

  if not Assigned(FMemo) or FMemo.ReadOnly then
    Exit;

  BasicShift := Shift * [ssShift, ssCtrl];
  if Shift <> BasicShift then
    Exit;

  Command := ecNone;
  case Key of
    VK_BACK:
      if (BasicShift = []) or (BasicShift = [ssShift]) then
        Command := ecDeleteLastChar
      else if BasicShift = [ssCtrl] then
        Command := ecDeleteLastWord;
    VK_DELETE:
      if BasicShift = [] then
        Command := ecDeleteChar
      else if BasicShift = [ssShift] then
        Command := ecCut;
    VK_C:
      if BasicShift = [ssCtrl] then
        Command := ecCopy;
    VK_V:
      if BasicShift = [ssCtrl] then
        Command := ecPaste;
    VK_X:
      if BasicShift = [ssCtrl] then
        Command := ecCut;
    VK_Y:
      if BasicShift = [ssCtrl] then
        Command := ecRedo;
    VK_Z:
      if BasicShift = [ssCtrl] then
        Command := ecUndo
      else if BasicShift = [ssShift, ssCtrl] then
        Command := ecRedo;
    VK_LEFT:
      if BasicShift = [] then
        Command := ecLeft
      else if BasicShift = [ssShift] then
        Command := ecSelLeft
      else if BasicShift = [ssCtrl] then
        Command := ecWordLeft
      else if BasicShift = [ssShift, ssCtrl] then
        Command := ecSelWordLeft;
    VK_RIGHT:
      if BasicShift = [] then
        Command := ecRight
      else if BasicShift = [ssShift] then
        Command := ecSelRight
      else if BasicShift = [ssCtrl] then
        Command := ecWordRight
      else if BasicShift = [ssShift, ssCtrl] then
        Command := ecSelWordRight;
    VK_UP:
      if BasicShift = [] then
        Command := ecUp
      else if BasicShift = [ssShift] then
        Command := ecSelUp;
    VK_DOWN:
      if BasicShift = [] then
        Command := ecDown
      else if BasicShift = [ssShift] then
        Command := ecSelDown;
    VK_HOME:
      if BasicShift = [] then
        Command := ecLineStart
      else if BasicShift = [ssShift] then
        Command := ecSelLineStart
      else if BasicShift = [ssCtrl] then
        Command := ecEditorTop
      else if BasicShift = [ssShift, ssCtrl] then
        Command := ecSelEditorTop;
    VK_END:
      if BasicShift = [] then
        Command := ecLineEnd
      else if BasicShift = [ssShift] then
        Command := ecSelLineEnd
      else if BasicShift = [ssCtrl] then
        Command := ecEditorBottom
      else if BasicShift = [ssShift, ssCtrl] then
        Command := ecSelEditorBottom;
  end;

  Result := Command <> ecNone;
  if Result then
  begin
    FMemo.CommandProcessor(Command, #0, nil);
    Key := 0;
  end;
end;

procedure TVerseMemo.LayoutEditorToolbar;
const
  Margin = 6;
  Gap = 6;
  TagButtonWidth = 46;
var
  X, RightEdge, ButtonHeight, SaveWidth, CancelWidth, ModeWidth,
    StatusLeft, StatusWidth: Integer;
begin
  if not Assigned(FEditorPanel) or not Assigned(FToolbarPanel) or
    not Assigned(FEditorPanel.Parent) or (FToolbarPanel.ClientWidth <= 0) then
    Exit;

  FToolbarPanel.Canvas.Font.Assign(FToolbarPanel.Font);
  ButtonHeight := Max(26, FToolbarPanel.Canvas.TextHeight('Hg') + 10);
  SaveWidth := Max(72, FToolbarPanel.Canvas.TextWidth(FSaveButton.Caption) + 26);
  CancelWidth := Max(72,
    FToolbarPanel.Canvas.TextWidth(FCancelButton.Caption) + 26);
  ModeWidth := Max(100,
    FToolbarPanel.Canvas.TextWidth(FModeLabel.Caption) + 10);

  RightEdge := FToolbarPanel.ClientWidth - Margin;
  FCancelButton.SetBounds(RightEdge - CancelWidth,
    (FToolbarPanel.ClientHeight - ButtonHeight) div 2,
    CancelWidth, ButtonHeight);
  Dec(RightEdge, CancelWidth + Gap);
  FSaveButton.SetBounds(RightEdge - SaveWidth,
    (FToolbarPanel.ClientHeight - ButtonHeight) div 2,
    SaveWidth, ButtonHeight);
  Dec(RightEdge, SaveWidth + Gap);

  X := Margin;
  FModeLabel.SetBounds(X, 0, ModeWidth, FToolbarPanel.ClientHeight);
  Inc(X, ModeWidth + Gap);

  FAddedWordsButton.SetBounds(X,
    (FToolbarPanel.ClientHeight - ButtonHeight) div 2,
    TagButtonWidth, ButtonHeight);
  Inc(X, TagButtonWidth + Gap);
  FJesusWordsButton.SetBounds(X,
    (FToolbarPanel.ClientHeight - ButtonHeight) div 2,
    TagButtonWidth, ButtonHeight);
  Inc(X, TagButtonWidth + Gap);
  FOldTestamentQuoteButton.SetBounds(X,
    (FToolbarPanel.ClientHeight - ButtonHeight) div 2,
    TagButtonWidth, ButtonHeight);
  Inc(X, TagButtonWidth + Gap);

  StatusLeft := X;
  StatusWidth := RightEdge - StatusLeft;
  FStatusLabel.Visible := StatusWidth >= 90;
  if FStatusLabel.Visible then
    FStatusLabel.SetBounds(StatusLeft, 0, StatusWidth,
      FToolbarPanel.ClientHeight);
end;

procedure TVerseMemo.RefreshLocalizedCaptions;
begin
  FModeLabel.Caption := SEditModeTitle;
  FSaveButton.Caption := SSaveEditButton;
  FSaveButton.Hint := SSaveEditHint;
  FCancelButton.Caption := SCancelEditButton;
  FCancelButton.Hint := SCancelEditHint;
  FAddedWordsButton.Hint := SAddedWordsTagHint;
  FJesusWordsButton.Hint := SJesusWordsTagHint;
  FOldTestamentQuoteButton.Hint := SOldTestamentQuoteTagHint;
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
begin
  if not Assigned(FStatusLabel) then
    Exit;

  if FModified then
  begin
    FStatusLabel.Caption := SEditModeModified;
    FStatusLabel.Font.Color := clRed;
  end
  else
  begin
    FStatusLabel.Caption := SEditModeSaved;
    FStatusLabel.Font.Color := clGrayText;
  end;

  FEditorPanel.Hint := FStatusLabel.Caption;
  LayoutEditorToolbar;
end;

function TVerseMemo.ValidateTextForSave: Boolean;
begin
  Result := (Pos(#13, Text) = 0) and (Pos(#10, Text) = 0);
  if not Result then
    MessageDlg(SLineBreaksInEditorTitle, SLineBreaksInEditor, mtError, [mbOK], 0);
end;

procedure TVerseMemo.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
  begin
    SaveAndClose;
    Key := 0;
  end
  else
  if (ssCtrl in Shift) and (ssShift in Shift) and
    (Key in [VK_I, VK_E, VK_O]) then
  begin
    case Key of
      VK_I:
        begin
          ApplyTagToSelection('<FI>', '<Fi>');
          Key := 0;
        end;
      VK_E:
        begin
          ApplyTagToSelection('<FR>', '<Fr>');
          Key := 0;
        end;
      VK_O:
        begin
          ApplyTagToSelection('<FO>', '<Fo>');
          Key := 0;
        end;
    end;
  end
  else if Key = VK_ESCAPE then
  begin
    ConfirmChange;
  end
  else if (Key = VK_RETURN) and (ssCtrl in Shift) then
  begin
    SaveAndClose;
    Key := 0;
  end
  else if Key = VK_RETURN then
    Key := 0
  else
    HandleEditorCommandKey(Key, Shift);
end;

procedure TVerseMemo.OnChange(Sender: TObject);
begin
  if (not FActivating) and Assigned(TSynEdit(Sender).Parent) and
    TSynEdit(Sender).Visible then
  begin
    FModified := not Assigned(FVerse) or (Text <> FVerse.XML);
    UpdateEditorState;
    if Assigned(FOnPendingChange) then
      FOnPendingChange(Self);
  end;
end;

procedure TVerseMemo.ConfirmChange;
begin
  {
  if FModified then
  begin
    case MessageDlg(SChangeVerse, SChangeVerseConfirmation, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        if Assigned(FOnVerseTextModified) then
          FOnVerseTextModified(Self);
      mrCancel: exit;
    end;
  end;
  }
  SaveAndClose;
end;

constructor TVerseMemo.Create;
begin
  FEditorPanel := TPanel.Create(nil);
  with FEditorPanel do
  begin
    Visible := False;
    Align := alNone;
    BevelOuter := bvLowered;
    ParentColor := False;
    Color := clWindow;
    ShowHint := True;
  end;

  FToolbarPanel := TPanel.Create(FEditorPanel);
  with FToolbarPanel do
  begin
    Parent := FEditorPanel;
    Align := alTop;
    Height := 38;
    BevelOuter := bvNone;
    ParentColor := False;
    Color := clInfoBk;
    OnResize := @ToolbarResize;
  end;

  FModeLabel := TLabel.Create(FEditorPanel);
  with FModeLabel do
  begin
    Parent := FToolbarPanel;
    Caption := SEditModeTitle;
    AutoSize := False;
    Layout := tlCenter;
    Font.Style := Font.Style + [fsBold];
    Transparent := True;
  end;

  FStatusLabel := TLabel.Create(FEditorPanel);
  with FStatusLabel do
  begin
    Parent := FToolbarPanel;
    AutoSize := False;
    Layout := tlCenter;
    Transparent := True;
  end;

  FAddedWordsButton := TButton.Create(FEditorPanel);
  with FAddedWordsButton do
  begin
    Parent := FToolbarPanel;
    Caption := '<FI>';
    Hint := SAddedWordsTagHint;
    ShowHint := True;
    OnClick := @InsertAddedWordsTag;
  end;

  FJesusWordsButton := TButton.Create(FEditorPanel);
  with FJesusWordsButton do
  begin
    Parent := FToolbarPanel;
    Caption := '<FR>';
    Hint := SJesusWordsTagHint;
    ShowHint := True;
    OnClick := @InsertJesusWordsTag;
  end;

  FOldTestamentQuoteButton := TButton.Create(FEditorPanel);
  with FOldTestamentQuoteButton do
  begin
    Parent := FToolbarPanel;
    Caption := '<FO>';
    Hint := SOldTestamentQuoteTagHint;
    ShowHint := True;
    OnClick := @InsertOldTestamentQuoteTag;
  end;

  FSaveButton := TButton.Create(FEditorPanel);
  with FSaveButton do
  begin
    Parent := FToolbarPanel;
    Caption := SSaveEditButton;
    Hint := SSaveEditHint;
    ShowHint := True;
    OnClick := @SaveButtonClick;
  end;

  FCancelButton := TButton.Create(FEditorPanel);
  with FCancelButton do
  begin
    Parent := FToolbarPanel;
    Caption := SCancelEditButton;
    Hint := SCancelEditHint;
    ShowHint := True;
    OnClick := @CancelButtonClick;
  end;

  FMemo := TSynEdit.Create(nil);
  FMemo.Visible := False;
  FMemo.Parent := FEditorPanel;
  FMemo.Align := alClient;
  ConfigureTheWordSyntaxEditor(FMemo, False);
  FMemo.OnKeyDown := @OnKeyDown;
  FMemo.OnChange := @OnChange;
  FMemo.OnMouseWheel := @OnMouseWheel;
  FActivating := False;
  FModified := False;
  FContainer := nil;
  FBoundsContainer := nil;
  FVerse := nil;
  FOnVerseTextModified := nil;
  FOnPendingChange := nil;
  UpdateEditorState;
end;

destructor TVerseMemo.Destroy;
begin
  DetachContainerBoundsHandler;
  FMemo.Parent := nil;
  FMemo.Free;
  FEditorPanel.Free;
  inherited Destroy;
end;

procedure TVerseMemo.Activate(Container: TScrollBox; Verse: TVerse);
begin
  if FEditorPanel.Visible and (FContainer <> Container) then
  begin
    if not TryDeactivate then
      Exit;
  end;

  ApplyTheWordSyntaxEditorFont(FMemo);
  RefreshLocalizedCaptions;
  FActivating := True;
  FModified := False;
  DetachContainerBoundsHandler;
  FContainer := Container;
  FVerse := Verse;
  FEditorPanel.Visible := False;
  FEditorPanel.Parent := nil;
  FMemo.Text := Verse.XML;

  if Assigned(Container.Parent) then
  begin
    FEditorPanel.Align := alNone;
    FEditorPanel.Parent := Container.Parent;
    FEditorPanel.SetBounds(Container.Left, Container.Top, Container.Width,
      Container.Height);
    FEditorPanel.Anchors := Container.Anchors;
    FBoundsContainer := Container;
    FBoundsContainer.AddHandlerOnChangeBounds(@ContainerBoundsChanged);
  end
  else
  begin
    FEditorPanel.Align := alClient;
    FEditorPanel.Parent := Container;
  end;

  FMemo.Visible := True;
  UpdateEditorState;
  FEditorPanel.BringToFront;
  FEditorPanel.Visible := True;
  UpdateEditorBounds;
  FActivating := False;
  if FMemo.CanSetFocus then
    FMemo.SetFocus;
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
begin
  Result := True;
  if not FEditorPanel.Visible then
    Exit;

  if not ValidateTextForSave then
    Exit(False);

  FMemo.Visible := False;
  FEditorPanel.Visible := False;
  DetachContainerBoundsHandler;
  if Assigned(FOnVerseTextModified) and FModified then
    FOnVerseTextModified(Self);
  RestoreFocusAfterClose;
  FModified := False;
  FContainer := nil;
  FVerse := nil;
  if Assigned(FOnPendingChange) then
    FOnPendingChange(Self);
end;

procedure TVerseMemo.DiscardEdit;
begin
  if not FEditorPanel.Visible then
    Exit;

  FModified := False;
  FMemo.Visible := False;
  FEditorPanel.Visible := False;
  RestoreFocusAfterClose;
  DetachContainerBoundsHandler;
  FContainer := nil;
  FVerse := nil;
  if Assigned(FOnPendingChange) then
    FOnPendingChange(Self);
end;

end.
