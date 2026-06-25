unit FormTheWordHighlighterColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IniFiles, SynEdit, theword_syntax_highlighter;

type

  { TFrmTheWordHighlighterColors }

  TFrmTheWordHighlighterColors = class(TForm)
  private
    FApplyRoot: TWinControl;
    FBackgroundSwatch: TPanel;
    FBtnApply: TButton;
    FBtnBackground: TButton;
    FBtnCancel: TButton;
    FBtnForeground: TButton;
    FBtnFont: TButton;
    FBtnNoBackground: TButton;
    FBtnOK: TButton;
    FBtnResetAll: TButton;
    FBtnResetFont: TButton;
    FBtnResetSelected: TButton;
    FChkBold: TCheckBox;
    FChkItalic: TCheckBox;
    FChkUnderline: TCheckBox;
    FColorDialog: TColorDialog;
    FElementsList: TListBox;
    FForegroundSwatch: TPanel;
    FFontDialog: TFontDialog;
    FFontLabel: TLabel;
    FIni: TCustomIniFile;
    FOriginalFont: TTheWordSyntaxEditorFont;
    FOriginalScheme: TTheWordSyntaxColorScheme;
    FPreview: TSynEdit;
    FUpdating: Boolean;
    FAccepted: Boolean;
    FApplied: Boolean;
    FWorkingFont: TTheWordSyntaxEditorFont;
    FWorkingScheme: TTheWordSyntaxColorScheme;
    procedure ApplyButtonClick(Sender: TObject);
    procedure BackgroundButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CloseQueryHandler(Sender: TObject; var CanClose: Boolean);
    procedure ControlsChanged(Sender: TObject);
    function CurrentElement: TTheWordSyntaxElement;
    procedure ElementsListClick(Sender: TObject);
    procedure FontButtonClick(Sender: TObject);
    procedure ForegroundButtonClick(Sender: TObject);
    procedure LoadFontControls;
    procedure LoadSelectedControls;
    procedure NoBackgroundButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure RefreshPreview;
    procedure ResetAllButtonClick(Sender: TObject);
    procedure ResetFontButtonClick(Sender: TObject);
    procedure ResetSelectedButtonClick(Sender: TObject);
    procedure RestoreOriginalScheme;
    procedure SaveAndApplyWorkingScheme;
    procedure SetSwatch(AControl: TPanel; AColor: TColor; HasColor: Boolean);
    procedure UpdateSelectedStyleFromControls;
  public
    constructor Create(AOwner: TComponent); override;
    class function Execute(AOwner: TComponent; Ini: TCustomIniFile;
      ApplyRoot: TWinControl): Boolean;
  end;

implementation

const
  SPreviewText =
    '<wt>kai<WG2532><WTCONJ l="kai"> phos<WG5457><WTN-NSN l="phos"> ' +
    '<RF q=a>note<Rf> <a href=tw://bible?b=43&c=1&v=5>John 1:5</a>' + LineEnding +
    'Comment with <i>"treva"</i>, <font color=red>HTML</font> and ' +
    '<rtf tag="{\b ">RTF</rtf>.' + LineEnding +
    '<CM > malformed tag and partial <WGabc>';

{ TFrmTheWordHighlighterColors }

constructor TFrmTheWordHighlighterColors.Create(AOwner: TComponent);
var
  ButtonsPanel: TPanel;
  ClientPanel: TPanel;
  Element: TTheWordSyntaxElement;
  MainPanel: TPanel;
  OptionsPanel: TPanel;
  PreviewLabel: TLabel;
  RightButtonsPanel: TPanel;
begin
  inherited CreateNew(AOwner);

  Caption := 'theWord highlight colors';
  Position := poMainFormCenter;
  BorderStyle := bsSizeable;
  Width := 820;
  Height := 560;
  Constraints.MinWidth := 700;
  Constraints.MinHeight := 440;
  OnCloseQuery := @CloseQueryHandler;

  FColorDialog := TColorDialog.Create(Self);
  FColorDialog.Options := [cdFullOpen];

  FFontDialog := TFontDialog.Create(Self);
  FFontDialog.Options := [fdNoStyleSel];

  ButtonsPanel := TPanel.Create(Self);
  ButtonsPanel.Parent := Self;
  ButtonsPanel.Align := alBottom;
  ButtonsPanel.Height := 46;
  ButtonsPanel.BevelOuter := bvNone;
  ButtonsPanel.BorderSpacing.Around := 8;

  RightButtonsPanel := TPanel.Create(Self);
  RightButtonsPanel.Parent := ButtonsPanel;
  RightButtonsPanel.Align := alRight;
  RightButtonsPanel.Width := 270;
  RightButtonsPanel.BevelOuter := bvNone;

  FBtnOK := TButton.Create(Self);
  FBtnOK.Parent := RightButtonsPanel;
  FBtnOK.Caption := 'OK';
  FBtnOK.SetBounds(0, 9, 82, 28);
  FBtnOK.Default := True;
  FBtnOK.OnClick := @OKButtonClick;

  FBtnCancel := TButton.Create(Self);
  FBtnCancel.Parent := RightButtonsPanel;
  FBtnCancel.Caption := 'Cancel';
  FBtnCancel.SetBounds(90, 9, 82, 28);
  FBtnCancel.Cancel := True;
  FBtnCancel.OnClick := @CancelButtonClick;

  FBtnApply := TButton.Create(Self);
  FBtnApply.Parent := RightButtonsPanel;
  FBtnApply.Caption := 'Apply';
  FBtnApply.SetBounds(180, 9, 82, 28);
  FBtnApply.OnClick := @ApplyButtonClick;

  ClientPanel := TPanel.Create(Self);
  ClientPanel.Parent := Self;
  ClientPanel.Align := alClient;
  ClientPanel.BevelOuter := bvNone;
  ClientPanel.BorderSpacing.Around := 8;

  FElementsList := TListBox.Create(Self);
  FElementsList.Parent := ClientPanel;
  FElementsList.Align := alLeft;
  FElementsList.Width := 190;
  FElementsList.OnClick := @ElementsListClick;
  for Element := Low(TTheWordSyntaxElement) to High(TTheWordSyntaxElement) do
    FElementsList.Items.Add(TheWordSyntaxElementCaption(Element));
  FElementsList.ItemIndex := 0;

  MainPanel := TPanel.Create(Self);
  MainPanel.Parent := ClientPanel;
  MainPanel.Align := alClient;
  MainPanel.BevelOuter := bvNone;
  MainPanel.BorderSpacing.Left := 8;

  OptionsPanel := TPanel.Create(Self);
  OptionsPanel.Parent := MainPanel;
  OptionsPanel.Align := alTop;
  OptionsPanel.Height := 150;
  OptionsPanel.BevelOuter := bvNone;

  FBtnForeground := TButton.Create(Self);
  FBtnForeground.Parent := OptionsPanel;
  FBtnForeground.Caption := 'Foreground...';
  FBtnForeground.SetBounds(0, 4, 112, 28);
  FBtnForeground.OnClick := @ForegroundButtonClick;

  FForegroundSwatch := TPanel.Create(Self);
  FForegroundSwatch.Parent := OptionsPanel;
  FForegroundSwatch.SetBounds(122, 4, 130, 28);
  FForegroundSwatch.BevelOuter := bvLowered;

  FBtnBackground := TButton.Create(Self);
  FBtnBackground.Parent := OptionsPanel;
  FBtnBackground.Caption := 'Background...';
  FBtnBackground.SetBounds(0, 40, 112, 28);
  FBtnBackground.OnClick := @BackgroundButtonClick;

  FBackgroundSwatch := TPanel.Create(Self);
  FBackgroundSwatch.Parent := OptionsPanel;
  FBackgroundSwatch.SetBounds(122, 40, 130, 28);
  FBackgroundSwatch.BevelOuter := bvLowered;

  FBtnNoBackground := TButton.Create(Self);
  FBtnNoBackground.Parent := OptionsPanel;
  FBtnNoBackground.Caption := 'No background';
  FBtnNoBackground.SetBounds(262, 40, 112, 28);
  FBtnNoBackground.OnClick := @NoBackgroundButtonClick;

  FChkBold := TCheckBox.Create(Self);
  FChkBold.Parent := OptionsPanel;
  FChkBold.Caption := 'Bold';
  FChkBold.SetBounds(0, 80, 70, 22);
  FChkBold.OnChange := @ControlsChanged;

  FChkItalic := TCheckBox.Create(Self);
  FChkItalic.Parent := OptionsPanel;
  FChkItalic.Caption := 'Italic';
  FChkItalic.SetBounds(78, 80, 70, 22);
  FChkItalic.OnChange := @ControlsChanged;

  FChkUnderline := TCheckBox.Create(Self);
  FChkUnderline.Parent := OptionsPanel;
  FChkUnderline.Caption := 'Underline';
  FChkUnderline.SetBounds(156, 80, 92, 22);
  FChkUnderline.OnChange := @ControlsChanged;

  FBtnResetSelected := TButton.Create(Self);
  FBtnResetSelected.Parent := OptionsPanel;
  FBtnResetSelected.Caption := 'Reset selected';
  FBtnResetSelected.SetBounds(262, 78, 112, 28);
  FBtnResetSelected.OnClick := @ResetSelectedButtonClick;

  FBtnResetAll := TButton.Create(Self);
  FBtnResetAll.Parent := OptionsPanel;
  FBtnResetAll.Caption := 'Reset all';
  FBtnResetAll.SetBounds(382, 78, 90, 28);
  FBtnResetAll.OnClick := @ResetAllButtonClick;

  FBtnFont := TButton.Create(Self);
  FBtnFont.Parent := OptionsPanel;
  FBtnFont.Caption := 'Font...';
  FBtnFont.SetBounds(0, 116, 112, 28);
  FBtnFont.OnClick := @FontButtonClick;

  FFontLabel := TLabel.Create(Self);
  FFontLabel.Parent := OptionsPanel;
  FFontLabel.AutoSize := False;
  FFontLabel.SetBounds(122, 121, 250, 22);

  FBtnResetFont := TButton.Create(Self);
  FBtnResetFont.Parent := OptionsPanel;
  FBtnResetFont.Caption := 'Reset font';
  FBtnResetFont.SetBounds(382, 116, 90, 28);
  FBtnResetFont.OnClick := @ResetFontButtonClick;

  PreviewLabel := TLabel.Create(Self);
  PreviewLabel.Parent := MainPanel;
  PreviewLabel.Align := alTop;
  PreviewLabel.Caption := 'Preview';
  PreviewLabel.Height := 20;

  FPreview := TSynEdit.Create(Self);
  FPreview.Parent := MainPanel;
  FPreview.Align := alClient;
  FPreview.BorderSpacing.Top := 4;
  FPreview.Lines.Text := SPreviewText;
  ConfigureTheWordSyntaxEditor(FPreview, True);
end;

class function TFrmTheWordHighlighterColors.Execute(AOwner: TComponent;
  Ini: TCustomIniFile; ApplyRoot: TWinControl): Boolean;
var
  Form: TFrmTheWordHighlighterColors;
begin
  Form := TFrmTheWordHighlighterColors.Create(AOwner);
  try
    Form.FIni := Ini;
    Form.FApplyRoot := ApplyRoot;
    Form.FOriginalScheme := GetTheWordSyntaxColorScheme;
    Form.FOriginalFont := GetTheWordSyntaxEditorFont;
    Form.FWorkingScheme := Form.FOriginalScheme;
    Form.FWorkingFont := Form.FOriginalFont;
    Form.LoadSelectedControls;
    Form.LoadFontControls;
    Form.RefreshPreview;
    Result := Form.ShowModal = mrOK;
  finally
    Form.Free;
  end;
end;

function TFrmTheWordHighlighterColors.CurrentElement: TTheWordSyntaxElement;
begin
  if FElementsList.ItemIndex < 0 then
    FElementsList.ItemIndex := 0;
  Result := TTheWordSyntaxElement(FElementsList.ItemIndex);
end;

procedure TFrmTheWordHighlighterColors.SetSwatch(AControl: TPanel;
  AColor: TColor; HasColor: Boolean);
begin
  AControl.ParentColor := False;
  if HasColor then
  begin
    AControl.Color := AColor;
    AControl.Caption := ColorToString(AColor);
  end
  else
  begin
    AControl.Color := clBtnFace;
    AControl.Caption := 'none';
  end;
end;

procedure TFrmTheWordHighlighterColors.LoadSelectedControls;
var
  Element: TTheWordSyntaxElement;
  Style: TTheWordSyntaxElementStyle;
begin
  FUpdating := True;
  try
    Element := CurrentElement;
    Style := FWorkingScheme[Element];
    SetSwatch(FForegroundSwatch, Style.Foreground, True);
    SetSwatch(FBackgroundSwatch, Style.Background, Style.HasBackground);
    FChkBold.Checked := fsBold in Style.FontStyle;
    FChkItalic.Checked := fsItalic in Style.FontStyle;
    FChkUnderline.Checked := fsUnderline in Style.FontStyle;
  finally
    FUpdating := False;
  end;
end;

procedure TFrmTheWordHighlighterColors.LoadFontControls;
begin
  FFontLabel.Caption := Format('%s, %d pt', [FWorkingFont.Name,
    FWorkingFont.Size]);
end;

procedure TFrmTheWordHighlighterColors.UpdateSelectedStyleFromControls;
var
  Element: TTheWordSyntaxElement;
begin
  if FUpdating then
    Exit;

  Element := CurrentElement;
  FWorkingScheme[Element].FontStyle := [];
  if FChkBold.Checked then
    Include(FWorkingScheme[Element].FontStyle, fsBold);
  if FChkItalic.Checked then
    Include(FWorkingScheme[Element].FontStyle, fsItalic);
  if FChkUnderline.Checked then
    Include(FWorkingScheme[Element].FontStyle, fsUnderline);
  RefreshPreview;
end;

procedure TFrmTheWordHighlighterColors.RefreshPreview;
begin
  ApplyTheWordSyntaxEditorFont(FPreview, FWorkingFont);
  if FPreview.Highlighter is TTheWordSyntaxHighlighter then
  begin
    ApplyTheWordSyntaxColorScheme(TTheWordSyntaxHighlighter(FPreview.Highlighter),
      FWorkingScheme);
    FPreview.Invalidate;
  end;
end;

procedure TFrmTheWordHighlighterColors.SaveAndApplyWorkingScheme;
begin
  SetTheWordSyntaxColorScheme(FWorkingScheme);
  SetTheWordSyntaxEditorFont(FWorkingFont);
  SaveTheWordSyntaxColorScheme(FIni);
  SaveTheWordSyntaxEditorFont(FIni);
  ApplyTheWordSyntaxEditorFontToEditors(FApplyRoot);
  ApplyTheWordSyntaxColorSchemeToEditors(FApplyRoot);
  FApplied := True;
end;

procedure TFrmTheWordHighlighterColors.RestoreOriginalScheme;
begin
  SetTheWordSyntaxColorScheme(FOriginalScheme);
  SetTheWordSyntaxEditorFont(FOriginalFont);
  SaveTheWordSyntaxColorScheme(FIni);
  SaveTheWordSyntaxEditorFont(FIni);
  ApplyTheWordSyntaxEditorFontToEditors(FApplyRoot);
  ApplyTheWordSyntaxColorSchemeToEditors(FApplyRoot);
end;

procedure TFrmTheWordHighlighterColors.ElementsListClick(Sender: TObject);
begin
  LoadSelectedControls;
end;

procedure TFrmTheWordHighlighterColors.ForegroundButtonClick(Sender: TObject);
var
  Element: TTheWordSyntaxElement;
begin
  Element := CurrentElement;
  FColorDialog.Color := FWorkingScheme[Element].Foreground;
  if FColorDialog.Execute then
  begin
    FWorkingScheme[Element].Foreground := FColorDialog.Color;
    LoadSelectedControls;
    RefreshPreview;
  end;
end;

procedure TFrmTheWordHighlighterColors.FontButtonClick(Sender: TObject);
begin
  FFontDialog.Font.Name := FWorkingFont.Name;
  FFontDialog.Font.Size := FWorkingFont.Size;
  if FFontDialog.Execute then
  begin
    FWorkingFont.Name := FFontDialog.Font.Name;
    FWorkingFont.Size := FFontDialog.Font.Size;
    LoadFontControls;
    RefreshPreview;
  end;
end;

procedure TFrmTheWordHighlighterColors.BackgroundButtonClick(Sender: TObject);
var
  Element: TTheWordSyntaxElement;
begin
  Element := CurrentElement;
  if FWorkingScheme[Element].HasBackground then
    FColorDialog.Color := FWorkingScheme[Element].Background
  else
    FColorDialog.Color := clWhite;
  if FColorDialog.Execute then
  begin
    FWorkingScheme[Element].Background := FColorDialog.Color;
    FWorkingScheme[Element].HasBackground := True;
    LoadSelectedControls;
    RefreshPreview;
  end;
end;

procedure TFrmTheWordHighlighterColors.NoBackgroundButtonClick(Sender: TObject);
var
  Element: TTheWordSyntaxElement;
begin
  Element := CurrentElement;
  FWorkingScheme[Element].Background := clNone;
  FWorkingScheme[Element].HasBackground := False;
  LoadSelectedControls;
  RefreshPreview;
end;

procedure TFrmTheWordHighlighterColors.ControlsChanged(Sender: TObject);
begin
  UpdateSelectedStyleFromControls;
end;

procedure TFrmTheWordHighlighterColors.ResetSelectedButtonClick(Sender: TObject);
var
  Defaults: TTheWordSyntaxColorScheme;
  Element: TTheWordSyntaxElement;
begin
  GetDefaultTheWordSyntaxColorScheme(Defaults);
  Element := CurrentElement;
  FWorkingScheme[Element] := Defaults[Element];
  LoadSelectedControls;
  RefreshPreview;
end;

procedure TFrmTheWordHighlighterColors.ResetAllButtonClick(Sender: TObject);
begin
  GetDefaultTheWordSyntaxColorScheme(FWorkingScheme);
  GetDefaultTheWordSyntaxEditorFont(FWorkingFont);
  LoadSelectedControls;
  LoadFontControls;
  RefreshPreview;
end;

procedure TFrmTheWordHighlighterColors.ResetFontButtonClick(Sender: TObject);
begin
  GetDefaultTheWordSyntaxEditorFont(FWorkingFont);
  LoadFontControls;
  RefreshPreview;
end;

procedure TFrmTheWordHighlighterColors.ApplyButtonClick(Sender: TObject);
begin
  SaveAndApplyWorkingScheme;
end;

procedure TFrmTheWordHighlighterColors.OKButtonClick(Sender: TObject);
begin
  FAccepted := True;
  SaveAndApplyWorkingScheme;
  ModalResult := mrOK;
end;

procedure TFrmTheWordHighlighterColors.CancelButtonClick(Sender: TObject);
begin
  if not FApplied then
    RestoreOriginalScheme;
  ModalResult := mrCancel;
end;

procedure TFrmTheWordHighlighterColors.CloseQueryHandler(Sender: TObject;
  var CanClose: Boolean);
begin
  if (not FAccepted) and (not FApplied) then
    RestoreOriginalScheme;
end;

end.
