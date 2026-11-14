unit FormDefaultDictionaries;

{ Tools > Default dictionaries: the Strong's (Greek, Hebrew), morphology and
  word dictionaries used by every project when it has no dictionary of its
  own, or no entry for a key. Stored in iBiblia.ini, section [dicionarios],
  with the same keys as the 1.0.1 mod so existing settings carry over. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Dialogs, IniFiles,
  ui_style;

type
  TDefaultDictionarySettings = record
    Enabled: Boolean;
    GreekStrong: string;
    HebrewStrong: string;
    Morphology: string;
    Word: string;
  end;

function ReadDefaultDictionarySettings(Ini: TIniFile): TDefaultDictionarySettings;
procedure WriteDefaultDictionarySettings(Ini: TIniFile;
  const Settings: TDefaultDictionarySettings);
{ Applies the settings to dictionary_manager (empty paths when disabled). }
procedure ApplyDefaultDictionarySettings(const Settings: TDefaultDictionarySettings);
{ Shows the dialog; True when the user pressed OK. }
function EditDefaultDictionaries(AOwner: TComponent;
  var Settings: TDefaultDictionarySettings): Boolean;

implementation

uses
  dictionary_manager;

resourcestring
  SDefaultDictionariesTitle = 'Default dictionaries';
  SDefaultDictionariesIntro = 'Used by every project when it has no dictionary ' +
    'for a text, or when its dictionary has no entry for a word.';
  SUseDefaultDictionaries = '&Use default dictionaries';
  SGreekStrongDictionary = '&Greek Strong''s (G numbers):';
  SHebrewStrongDictionary = '&Hebrew Strong''s (H numbers):';
  SMorphologyDictionary = '&Morphology:';
  SWordDictionary = '&Word dictionary (untagged words):';
  SBrowse = 'Browse...';
  SClear = 'Clear';
  SOK = 'OK';
  SCancel = 'Cancel';
  SDictionaryFilter = 'theWord dictionaries (*.dct.twm;*.twm)|*.dct.twm;*.twm|All files (*.*)|*.*';

const
  INI_SECTION = 'dicionarios';

function ReadDefaultDictionarySettings(Ini: TIniFile): TDefaultDictionarySettings;
begin
  Result.Enabled := Ini.ReadBool(INI_SECTION, 'use_default',
    Ini.ReadBool(INI_SECTION, 'use_global', True));
  Result.GreekStrong := Ini.ReadString(INI_SECTION, 'default_strong_greek',
    Ini.ReadString(INI_SECTION, 'global_strong_greek', ''));
  Result.HebrewStrong := Ini.ReadString(INI_SECTION, 'default_strong_hebrew',
    Ini.ReadString(INI_SECTION, 'global_strong_hebrew', ''));
  Result.Morphology := Ini.ReadString(INI_SECTION, 'default_morphology',
    Ini.ReadString(INI_SECTION, 'global_morphology', ''));
  Result.Word := Ini.ReadString(INI_SECTION, 'default_english_dict',
    Ini.ReadString(INI_SECTION, 'global_english_dict', ''));
end;

procedure WriteDefaultDictionarySettings(Ini: TIniFile;
  const Settings: TDefaultDictionarySettings);
begin
  Ini.WriteBool(INI_SECTION, 'use_default', Settings.Enabled);
  Ini.WriteString(INI_SECTION, 'default_strong_greek', Settings.GreekStrong);
  Ini.WriteString(INI_SECTION, 'default_strong_hebrew', Settings.HebrewStrong);
  Ini.WriteString(INI_SECTION, 'default_morphology', Settings.Morphology);
  Ini.WriteString(INI_SECTION, 'default_english_dict', Settings.Word);
  Ini.UpdateFile;
end;

procedure ApplyDefaultDictionarySettings(const Settings: TDefaultDictionarySettings);
begin
  SetUseDefaultDictionaries(Settings.Enabled);
  if Settings.Enabled then
    SetDefaultDictionaries(Settings.GreekStrong, Settings.HebrewStrong,
      Settings.Morphology, Settings.Word)
  else
    SetDefaultDictionaries('', '', '', '');
end;

type

  { TFrmDefaultDictionaries }

  TFrmDefaultDictionaries = class(TForm)
  private
    FEnabledCheck: TCheckBox;
    FEdits: array[0..3] of TEdit;
    FBrowseButtons: array[0..3] of TButton;
    FClearButtons: array[0..3] of TButton;
    procedure BrowseClick(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure EnabledChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TFrmDefaultDictionaries.Create(AOwner: TComponent);
var
  Intro, RowLabel: TLabel;
  ButtonPanel: TPanel;
  OkButton, CancelButton: TButton;
  Row, RowTop: Integer;
  RowCaption: string;
begin
  inherited CreateNew(AOwner);
  ApplyFormStyle(Self);
  Caption := SDefaultDictionariesTitle;
  Position := poMainFormCenter;
  BorderStyle := bsDialog;
  ClientWidth := 640;
  ClientHeight := 400;

  Intro := TLabel.Create(Self);
  Intro.Parent := Self;
  Intro.SetBounds(16, 12, 608, 36);
  Intro.AutoSize := False;
  Intro.WordWrap := True;
  Intro.Caption := SDefaultDictionariesIntro;

  FEnabledCheck := TCheckBox.Create(Self);
  FEnabledCheck.Parent := Self;
  FEnabledCheck.SetBounds(16, 52, 400, 24);
  FEnabledCheck.Caption := SUseDefaultDictionaries;
  FEnabledCheck.OnChange := @EnabledChange;

  for Row := 0 to 3 do
  begin
    case Row of
      0: RowCaption := SGreekStrongDictionary;
      1: RowCaption := SHebrewStrongDictionary;
      2: RowCaption := SMorphologyDictionary;
    else
      RowCaption := SWordDictionary;
    end;
    RowTop := 88 + Row * 64;

    RowLabel := TLabel.Create(Self);
    RowLabel.Parent := Self;
    RowLabel.SetBounds(16, RowTop, 608, 20);
    RowLabel.Caption := RowCaption;

    FEdits[Row] := TEdit.Create(Self);
    FEdits[Row].Parent := Self;
    FEdits[Row].SetBounds(16, RowTop + 22, 424, 28);
    RowLabel.FocusControl := FEdits[Row];

    FBrowseButtons[Row] := TButton.Create(Self);
    FBrowseButtons[Row].Parent := Self;
    FBrowseButtons[Row].SetBounds(448, RowTop + 21, 96, 30);
    FBrowseButtons[Row].Caption := SBrowse;
    FBrowseButtons[Row].Tag := Row;
    FBrowseButtons[Row].OnClick := @BrowseClick;

    FClearButtons[Row] := TButton.Create(Self);
    FClearButtons[Row].Parent := Self;
    FClearButtons[Row].SetBounds(552, RowTop + 21, 72, 30);
    FClearButtons[Row].Caption := SClear;
    FClearButtons[Row].Tag := Row;
    FClearButtons[Row].OnClick := @ClearClick;
  end;

  ButtonPanel := TPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.Align := alBottom;
  ButtonPanel.Height := 48;
  ButtonPanel.BevelOuter := bvNone;

  CancelButton := TButton.Create(Self);
  CancelButton.Parent := ButtonPanel;
  CancelButton.SetBounds(528, 9, 96, 30);
  CancelButton.Caption := SCancel;
  CancelButton.Cancel := True;
  CancelButton.ModalResult := mrCancel;

  OkButton := TButton.Create(Self);
  OkButton.Parent := ButtonPanel;
  OkButton.SetBounds(424, 9, 96, 30);
  OkButton.Caption := SOK;
  OkButton.Default := True;
  OkButton.ModalResult := mrOK;
end;

procedure TFrmDefaultDictionaries.BrowseClick(Sender: TObject);
var
  Dialog: TOpenDialog;
  Row: Integer;
begin
  Row := TComponent(Sender).Tag;
  Dialog := TOpenDialog.Create(Self);
  try
    Dialog.Filter := SDictionaryFilter;
    Dialog.Options := Dialog.Options + [ofFileMustExist];
    if FEdits[Row].Text <> '' then
    begin
      Dialog.InitialDir := ExtractFilePath(FEdits[Row].Text);
      Dialog.FileName := ExtractFileName(FEdits[Row].Text);
    end;
    if Dialog.Execute then
      FEdits[Row].Text := Dialog.FileName;
  finally
    Dialog.Free;
  end;
end;

procedure TFrmDefaultDictionaries.ClearClick(Sender: TObject);
begin
  FEdits[TComponent(Sender).Tag].Text := '';
end;

procedure TFrmDefaultDictionaries.EnabledChange(Sender: TObject);
var
  Row: Integer;
begin
  for Row := 0 to 3 do
  begin
    FEdits[Row].Enabled := FEnabledCheck.Checked;
    FBrowseButtons[Row].Enabled := FEnabledCheck.Checked;
    FClearButtons[Row].Enabled := FEnabledCheck.Checked;
  end;
end;

function EditDefaultDictionaries(AOwner: TComponent;
  var Settings: TDefaultDictionarySettings): Boolean;
var
  Form: TFrmDefaultDictionaries;
begin
  Form := TFrmDefaultDictionaries.Create(AOwner);
  try
    Form.FEnabledCheck.Checked := Settings.Enabled;
    Form.FEdits[0].Text := Settings.GreekStrong;
    Form.FEdits[1].Text := Settings.HebrewStrong;
    Form.FEdits[2].Text := Settings.Morphology;
    Form.FEdits[3].Text := Settings.Word;
    Form.EnabledChange(nil);
    Result := Form.ShowModal = mrOK;
    if Result then
    begin
      Settings.Enabled := Form.FEnabledCheck.Checked;
      Settings.GreekStrong := Trim(Form.FEdits[0].Text);
      Settings.HebrewStrong := Trim(Form.FEdits[1].Text);
      Settings.Morphology := Trim(Form.FEdits[2].Text);
      Settings.Word := Trim(Form.FEdits[3].Text);
    end;
  finally
    Form.Free;
  end;
end;

end.
