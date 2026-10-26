unit formglobaldicts;

{$mode objfpc}{$H+}

interface

uses
  unitskins,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, IniFiles;

type

  { TFormGlobalDicts }

  TFormGlobalDicts = class(TForm)
  private
    FIni: TIniFile;
    FOwnsIni: Boolean;
    CheckUseGlobal: TCheckBox;
    GroupDicts: TGroupBox;

    LabelStrongGreek: TLabel;
    EditStrongGreek: TEdit;
    ButtonBrowseGreek: TButton;
    ButtonClearGreek: TButton;

    LabelStrongHebrew: TLabel;
    EditStrongHebrew: TEdit;
    ButtonBrowseHebrew: TButton;
    ButtonClearHebrew: TButton;

    LabelMorph: TLabel;
    EditMorph: TEdit;
    ButtonBrowseMorph: TButton;
    ButtonClearMorph: TButton;

    LabelEnglishDict: TLabel;
    EditEnglishDict: TEdit;
    ButtonBrowseEnglish: TButton;
    ButtonClearEnglish: TButton;

    LabelHelpInfo: TLabel;

    PanelButtons: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;

    OpenDialogDict: TOpenDialog;

    procedure CheckUseGlobalChange(Sender: TObject);
    procedure ButtonBrowseGreekClick(Sender: TObject);
    procedure ButtonClearGreekClick(Sender: TObject);
    procedure ButtonBrowseHebrewClick(Sender: TObject);
    procedure ButtonClearHebrewClick(Sender: TObject);
    procedure ButtonBrowseMorphClick(Sender: TObject);
    procedure ButtonClearMorphClick(Sender: TObject);
    procedure ButtonBrowseEnglishClick(Sender: TObject);
    procedure ButtonClearEnglishClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AIni: TIniFile = nil); reintroduce;
    destructor Destroy; override;
    procedure LoadSettings;
    procedure SaveSettings;
  end;

function ShowGlobalDictsDialog(AOwner: TComponent; AIni: TIniFile = nil): Boolean;

resourcestring
  SGlobalDictsTitle = 'Default Dictionaries Settings';
  SUseGlobalDictsCaption = 'Use default dictionaries for all Bible modules';
  SGlobalDictsGroup = 'Default theWord Dictionaries';
  SStrongGreekLabel = 'Greek Strong''s / Default Dictionary (*.dct.twm):';
  SStrongHebrewLabel = 'Hebrew Strong''s Dictionary (*.dct.twm):';
  SMorphologyLabel = 'Morphology Dictionary (*.dct.twm):';
  SEnglishDictLabel = 'English Word / Bible Dictionary (*.dct.twm):';
  SBrowseCaption = 'Browse...';
  SClearCaption = 'Clear';
  SDictDialogFilter = 'theWord dictionary modules (*.dct.twm)|*.dct.twm|All files (*.*)|*.*';
  SDictHelpInfo = 'When enabled, all Bible modules (Source, Destination, Ref 1, Ref 2) ' +
                  'use these unified dictionaries for hover definitions. ' +
                  'OT/H-codes route to Hebrew, NT/G-codes route to Greek, and English words ' +
                  'without Strong''s tags route to the English Word / Bible Dictionary.';

implementation

function ShowGlobalDictsDialog(AOwner: TComponent; AIni: TIniFile = nil): Boolean;
var
  dlg: TFormGlobalDicts;
begin
  dlg := TFormGlobalDicts.Create(AOwner, AIni);
  try
    dlg.LoadSettings;
    Result := (dlg.ShowModal = mrOk);
  finally
    dlg.Free;
  end;
end;

{ TFormGlobalDicts }

constructor TFormGlobalDicts.Create(AOwner: TComponent; AIni: TIniFile = nil);
begin
  inherited CreateNew(AOwner);
  if AIni <> nil then
  begin
    FIni := AIni;
    FOwnsIni := False;
  end
  else
  begin
    FIni := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'iBiblia.ini');
    FOwnsIni := True;
  end;

  Caption := SGlobalDictsTitle;
  Width := 600;
  Height := 440;
  Position := poMainFormCenter;
  BorderIcons := [biSystemMenu];
  BorderStyle := bsDialog;

  OpenDialogDict := TOpenDialog.Create(Self);
  OpenDialogDict.Filter := SDictDialogFilter;
  OpenDialogDict.Title := 'Select theWord Dictionary Module';

  CheckUseGlobal := TCheckBox.Create(Self);
  CheckUseGlobal.Parent := Self;
  CheckUseGlobal.SetBounds(20, 15, 560, 24);
  CheckUseGlobal.Caption := SUseGlobalDictsCaption;
  CheckUseGlobal.Font.Style := [fsBold];
  CheckUseGlobal.OnChange := @CheckUseGlobalChange;

  GroupDicts := TGroupBox.Create(Self);
  GroupDicts.Parent := Self;
  GroupDicts.SetBounds(15, 45, 570, 335);
  GroupDicts.Caption := SGlobalDictsGroup;

  // Greek / Default Strong
  LabelStrongGreek := TLabel.Create(GroupDicts);
  LabelStrongGreek.Parent := GroupDicts;
  LabelStrongGreek.SetBounds(15, 12, 540, 18);
  LabelStrongGreek.Caption := SStrongGreekLabel;

  EditStrongGreek := TEdit.Create(GroupDicts);
  EditStrongGreek.Parent := GroupDicts;
  EditStrongGreek.SetBounds(15, 32, 410, 24);

  ButtonBrowseGreek := TButton.Create(GroupDicts);
  ButtonBrowseGreek.Parent := GroupDicts;
  ButtonBrowseGreek.SetBounds(432, 31, 72, 26);
  ButtonBrowseGreek.Caption := SBrowseCaption;
  ButtonBrowseGreek.OnClick := @ButtonBrowseGreekClick;

  ButtonClearGreek := TButton.Create(GroupDicts);
  ButtonClearGreek.Parent := GroupDicts;
  ButtonClearGreek.SetBounds(508, 31, 48, 26);
  ButtonClearGreek.Caption := SClearCaption;
  ButtonClearGreek.OnClick := @ButtonClearGreekClick;

  // Hebrew Strong
  LabelStrongHebrew := TLabel.Create(GroupDicts);
  LabelStrongHebrew.Parent := GroupDicts;
  LabelStrongHebrew.SetBounds(15, 68, 540, 18);
  LabelStrongHebrew.Caption := SStrongHebrewLabel;

  EditStrongHebrew := TEdit.Create(GroupDicts);
  EditStrongHebrew.Parent := GroupDicts;
  EditStrongHebrew.SetBounds(15, 88, 410, 24);

  ButtonBrowseHebrew := TButton.Create(GroupDicts);
  ButtonBrowseHebrew.Parent := GroupDicts;
  ButtonBrowseHebrew.SetBounds(432, 87, 72, 26);
  ButtonBrowseHebrew.Caption := SBrowseCaption;
  ButtonBrowseHebrew.OnClick := @ButtonBrowseHebrewClick;

  ButtonClearHebrew := TButton.Create(GroupDicts);
  ButtonClearHebrew.Parent := GroupDicts;
  ButtonClearHebrew.SetBounds(508, 87, 48, 26);
  ButtonClearHebrew.Caption := SClearCaption;
  ButtonClearHebrew.OnClick := @ButtonClearHebrewClick;

  // Morphology
  LabelMorph := TLabel.Create(GroupDicts);
  LabelMorph.Parent := GroupDicts;
  LabelMorph.SetBounds(15, 124, 540, 18);
  LabelMorph.Caption := SMorphologyLabel;

  EditMorph := TEdit.Create(GroupDicts);
  EditMorph.Parent := GroupDicts;
  EditMorph.SetBounds(15, 144, 410, 24);

  ButtonBrowseMorph := TButton.Create(GroupDicts);
  ButtonBrowseMorph.Parent := GroupDicts;
  ButtonBrowseMorph.SetBounds(432, 143, 72, 26);
  ButtonBrowseMorph.Caption := SBrowseCaption;
  ButtonBrowseMorph.OnClick := @ButtonBrowseMorphClick;

  ButtonClearMorph := TButton.Create(GroupDicts);
  ButtonClearMorph.Parent := GroupDicts;
  ButtonClearMorph.SetBounds(508, 143, 48, 26);
  ButtonClearMorph.Caption := SClearCaption;
  ButtonClearMorph.OnClick := @ButtonClearMorphClick;

  // English Word / Bible Dictionary
  LabelEnglishDict := TLabel.Create(GroupDicts);
  LabelEnglishDict.Parent := GroupDicts;
  LabelEnglishDict.SetBounds(15, 180, 540, 18);
  LabelEnglishDict.Caption := SEnglishDictLabel;

  EditEnglishDict := TEdit.Create(GroupDicts);
  EditEnglishDict.Parent := GroupDicts;
  EditEnglishDict.SetBounds(15, 200, 410, 24);

  ButtonBrowseEnglish := TButton.Create(GroupDicts);
  ButtonBrowseEnglish.Parent := GroupDicts;
  ButtonBrowseEnglish.SetBounds(432, 199, 72, 26);
  ButtonBrowseEnglish.Caption := SBrowseCaption;
  ButtonBrowseEnglish.OnClick := @ButtonBrowseEnglishClick;

  ButtonClearEnglish := TButton.Create(GroupDicts);
  ButtonClearEnglish.Parent := GroupDicts;
  ButtonClearEnglish.SetBounds(508, 199, 48, 26);
  ButtonClearEnglish.Caption := SClearCaption;
  ButtonClearEnglish.OnClick := @ButtonClearEnglishClick;

  // Help info text
  LabelHelpInfo := TLabel.Create(GroupDicts);
  LabelHelpInfo.Parent := GroupDicts;
  LabelHelpInfo.SetBounds(15, 238, 540, 68);
  LabelHelpInfo.AutoSize := False;
  LabelHelpInfo.WordWrap := True;
  LabelHelpInfo.Font.Color := clGray;
  LabelHelpInfo.Caption := SDictHelpInfo;

  // Bottom buttons
  PanelButtons := TPanel.Create(Self);
  PanelButtons.Parent := Self;
  PanelButtons.Align := alBottom;
  PanelButtons.Height := 48;
  PanelButtons.BevelOuter := bvNone;

  ButtonOK := TButton.Create(PanelButtons);
  ButtonOK.Parent := PanelButtons;
  ButtonOK.SetBounds(415, 9, 80, 30);
  ButtonOK.Caption := 'OK';
  ButtonOK.Default := True;
  ButtonOK.OnClick := @ButtonOKClick;

  ButtonCancel := TButton.Create(PanelButtons);
  ButtonCancel.Parent := PanelButtons;
  ButtonCancel.SetBounds(505, 9, 80, 30);
  ButtonCancel.Caption := 'Cancel';
  ButtonCancel.Cancel := True;
  ButtonCancel.OnClick := @ButtonCancelClick;

  OnShow := @FormShow;
  Color := CurrentSkin.PanelBg;
  Font.Color := CurrentSkin.PanelText;
end;

destructor TFormGlobalDicts.Destroy;
begin
  if FOwnsIni and Assigned(FIni) then
  begin
    FIni.Free;
    FIni := nil;
  end;
  inherited Destroy;
end;

procedure TFormGlobalDicts.CheckUseGlobalChange(Sender: TObject);
begin
  GroupDicts.Enabled := CheckUseGlobal.Checked;
  if CurrentSkin.IsDark then
  begin
    if CheckUseGlobal.Checked then
      LabelHelpInfo.Font.Color := CurrentSkin.WindowTextDim
    else
      LabelHelpInfo.Font.Color := CurrentSkin.PanelBorder;
  end;
end;

procedure TFormGlobalDicts.ButtonBrowseGreekClick(Sender: TObject);
begin
  OpenDialogDict.Title := 'Select Greek / Default Strong''s Dictionary (*.dct.twm)';
  OpenDialogDict.FileName := EditStrongGreek.Text;
  if OpenDialogDict.Execute then
    EditStrongGreek.Text := OpenDialogDict.FileName;
end;

procedure TFormGlobalDicts.ButtonClearGreekClick(Sender: TObject);
begin
  EditStrongGreek.Text := '';
end;

procedure TFormGlobalDicts.ButtonBrowseHebrewClick(Sender: TObject);
begin
  OpenDialogDict.Title := 'Select Hebrew Strong''s Dictionary (*.dct.twm)';
  OpenDialogDict.FileName := EditStrongHebrew.Text;
  if OpenDialogDict.Execute then
    EditStrongHebrew.Text := OpenDialogDict.FileName;
end;

procedure TFormGlobalDicts.ButtonClearHebrewClick(Sender: TObject);
begin
  EditStrongHebrew.Text := '';
end;

procedure TFormGlobalDicts.ButtonBrowseMorphClick(Sender: TObject);
begin
  OpenDialogDict.Title := 'Select Morphology Dictionary (*.dct.twm)';
  OpenDialogDict.FileName := EditMorph.Text;
  if OpenDialogDict.Execute then
    EditMorph.Text := OpenDialogDict.FileName;
end;

procedure TFormGlobalDicts.ButtonClearMorphClick(Sender: TObject);
begin
  EditMorph.Text := '';
end;

procedure TFormGlobalDicts.ButtonBrowseEnglishClick(Sender: TObject);
begin
  OpenDialogDict.Title := 'Select English Word / Bible Dictionary (*.dct.twm)';
  OpenDialogDict.FileName := EditEnglishDict.Text;
  if OpenDialogDict.Execute then
    EditEnglishDict.Text := OpenDialogDict.FileName;
end;

procedure TFormGlobalDicts.ButtonClearEnglishClick(Sender: TObject);
begin
  EditEnglishDict.Text := '';
end;

procedure TFormGlobalDicts.LoadSettings;
begin
  if FIni = nil then Exit;
  CheckUseGlobal.Checked := FIni.ReadBool('dicionarios', 'use_default', FIni.ReadBool('dicionarios', 'use_global', True));
  EditStrongGreek.Text := FIni.ReadString('dicionarios', 'default_strong_greek', FIni.ReadString('dicionarios', 'global_strong_greek', ''));
  EditStrongHebrew.Text := FIni.ReadString('dicionarios', 'default_strong_hebrew', FIni.ReadString('dicionarios', 'global_strong_hebrew', ''));
  EditMorph.Text := FIni.ReadString('dicionarios', 'default_morphology', FIni.ReadString('dicionarios', 'global_morphology', ''));
  EditEnglishDict.Text := FIni.ReadString('dicionarios', 'default_english_dict', FIni.ReadString('dicionarios', 'global_english_dict', ''));
  CheckUseGlobalChange(nil);
end;

procedure TFormGlobalDicts.SaveSettings;
begin
  if FIni = nil then Exit;
  FIni.WriteBool('dicionarios', 'use_default', CheckUseGlobal.Checked);
  FIni.WriteBool('dicionarios', 'use_global', CheckUseGlobal.Checked);
  FIni.WriteString('dicionarios', 'default_strong_greek', Trim(EditStrongGreek.Text));
  FIni.WriteString('dicionarios', 'global_strong_greek', Trim(EditStrongGreek.Text));
  FIni.WriteString('dicionarios', 'default_strong_hebrew', Trim(EditStrongHebrew.Text));
  FIni.WriteString('dicionarios', 'global_strong_hebrew', Trim(EditStrongHebrew.Text));
  FIni.WriteString('dicionarios', 'default_morphology', Trim(EditMorph.Text));
  FIni.WriteString('dicionarios', 'global_morphology', Trim(EditMorph.Text));
  FIni.WriteString('dicionarios', 'default_english_dict', Trim(EditEnglishDict.Text));
  FIni.WriteString('dicionarios', 'global_english_dict', Trim(EditEnglishDict.Text));
  FIni.UpdateFile;
end;

procedure TFormGlobalDicts.ButtonOKClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOk;
end;

procedure TFormGlobalDicts.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormGlobalDicts.FormShow(Sender: TObject);
begin
  HandleNeeded;
  GroupDicts.HandleNeeded;
  CheckUseGlobal.HandleNeeded;
  EditStrongGreek.HandleNeeded;
  EditStrongHebrew.HandleNeeded;
  EditMorph.HandleNeeded;
  EditEnglishDict.HandleNeeded;
  ButtonBrowseGreek.HandleNeeded;
  ButtonClearGreek.HandleNeeded;
  ButtonBrowseHebrew.HandleNeeded;
  ButtonClearHebrew.HandleNeeded;
  ButtonBrowseMorph.HandleNeeded;
  ButtonClearMorph.HandleNeeded;
  ButtonBrowseEnglish.HandleNeeded;
  ButtonClearEnglish.HandleNeeded;
  ButtonOK.HandleNeeded;
  ButtonCancel.HandleNeeded;

  ApplySkinToForm(Self, CurrentSkin);
  {$IFDEF WINDOWS}
  HookGroupBoxControl(GroupDicts);
  HookCheckBoxControl(CheckUseGlobal);
  HookEditControl(EditStrongGreek);
  HookEditControl(EditStrongHebrew);
  HookEditControl(EditMorph);
  HookEditControl(EditEnglishDict);
  HookButtonControl(ButtonBrowseGreek);
  HookButtonControl(ButtonClearGreek);
  HookButtonControl(ButtonBrowseHebrew);
  HookButtonControl(ButtonClearHebrew);
  HookButtonControl(ButtonBrowseMorph);
  HookButtonControl(ButtonClearMorph);
  HookButtonControl(ButtonBrowseEnglish);
  HookButtonControl(ButtonClearEnglish);
  HookButtonControl(ButtonOK);
  HookButtonControl(ButtonCancel);
  {$ENDIF}
  if CurrentSkin.IsDark then
  begin
    GroupDicts.Color := CurrentSkin.PanelBg;
    GroupDicts.Font.Color := CurrentSkin.PanelText;
    GroupDicts.Invalidate;
    CheckUseGlobal.Color := CurrentSkin.PanelBg;
    CheckUseGlobal.Font.Color := CurrentSkin.PanelText;
    CheckUseGlobal.Invalidate;
    EditStrongGreek.Color := CurrentSkin.WindowBg;
    EditStrongGreek.Font.Color := CurrentSkin.WindowText;
    EditStrongHebrew.Color := CurrentSkin.WindowBg;
    EditStrongHebrew.Font.Color := CurrentSkin.WindowText;
    EditMorph.Color := CurrentSkin.WindowBg;
    EditMorph.Font.Color := CurrentSkin.WindowText;
    EditEnglishDict.Color := CurrentSkin.WindowBg;
    EditEnglishDict.Font.Color := CurrentSkin.WindowText;

    CheckUseGlobal.Font.Color := CurrentSkin.PanelText;
    LabelStrongGreek.Font.Color := CurrentSkin.PanelText;
    LabelStrongHebrew.Font.Color := CurrentSkin.PanelText;
    LabelMorph.Font.Color := CurrentSkin.PanelText;
    LabelEnglishDict.Font.Color := CurrentSkin.PanelText;
    LabelHelpInfo.Font.Color := CurrentSkin.WindowTextDim;

    ButtonBrowseGreek.Font.Color := CurrentSkin.PanelText;
    ButtonClearGreek.Font.Color := CurrentSkin.PanelText;
    ButtonBrowseHebrew.Font.Color := CurrentSkin.PanelText;
    ButtonClearHebrew.Font.Color := CurrentSkin.PanelText;
    ButtonBrowseMorph.Font.Color := CurrentSkin.PanelText;
    ButtonClearMorph.Font.Color := CurrentSkin.PanelText;
    ButtonBrowseEnglish.Font.Color := CurrentSkin.PanelText;
    ButtonClearEnglish.Font.Color := CurrentSkin.PanelText;
    ButtonOK.Font.Color := CurrentSkin.PanelText;
    ButtonCancel.Font.Color := CurrentSkin.PanelText;
  end;
end;

end.
