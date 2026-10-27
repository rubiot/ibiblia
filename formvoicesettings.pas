unit formvoicesettings;

{$mode objfpc}{$H+}

interface

uses
  unitskins,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, unittexttospeech;

type

  { TFormVoiceSettings }

  TFormVoiceSettings = class(TForm)
  private
    CheckEnable: TCheckBox;
    LabelTrigger: TLabel;
    ComboTrigger: TComboBox;

    GroupVoices: TGroupBox;
    LabelSource: TLabel;
    ComboSourceVoice: TComboBox;
    ButtonTestSource: TButton;

    LabelDest: TLabel;
    ComboDestVoice: TComboBox;
    ButtonTestDest: TButton;

    GroupAudio: TGroupBox;
    LabelSpeed: TLabel;
    TrackSpeed: TTrackBar;
    LabelSpeedVal: TLabel;

    LabelVolume: TLabel;
    TrackVolume: TTrackBar;
    LabelVolumeVal: TLabel;

    PanelButtons: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;

    procedure FormShow(Sender: TObject);
    procedure TrackSpeedChange(Sender: TObject);
    procedure TrackVolumeChange(Sender: TObject);
    procedure ButtonTestSourceClick(Sender: TObject);
    procedure ButtonTestDestClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadSettings;
    procedure SaveSettings;
  end;

function ShowVoiceSettingsDialog(AOwner: TComponent): Boolean;

implementation

function ShowVoiceSettingsDialog(AOwner: TComponent): Boolean;
var
  dlg: TFormVoiceSettings;
begin
  dlg := TFormVoiceSettings.Create(AOwner);
  try
    dlg.LoadSettings;
    Result := (dlg.ShowModal = mrOk);
    if Result then
      dlg.SaveSettings;
  finally
    dlg.Free;
  end;
end;

{ TFormVoiceSettings }

constructor TFormVoiceSettings.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited CreateNew(AOwner);

  Caption := 'Voice & Text-to-Speech Settings';
  Width := 520;
  Height := 430;
  Position := poMainFormCenter;
  BorderStyle := bsDialog;
  BorderIcons := [biSystemMenu];
  OnShow := @FormShow;

  // 1. Activation & Trigger Settings
  CheckEnable := TCheckBox.Create(Self);
  CheckEnable.Parent := Self;
  CheckEnable.Left := 16;
  CheckEnable.Top := 12;
  CheckEnable.Width := 480;
  CheckEnable.Caption := 'Enable voice reading when hovering mouse over words';
  CheckEnable.Font.Style := [fsBold];

  LabelTrigger := TLabel.Create(Self);
  LabelTrigger.Parent := Self;
  LabelTrigger.Left := 36;
  LabelTrigger.Top := 40;
  LabelTrigger.Caption := 'Trigger condition:';

  ComboTrigger := TComboBox.Create(Self);
  ComboTrigger.Parent := Self;
  ComboTrigger.Left := 150;
  ComboTrigger.Top := 36;
  ComboTrigger.Width := 250;
  ComboTrigger.Style := csDropDownList;
  ComboTrigger.Items.Add('Always on mouse hover');
  ComboTrigger.Items.Add('Hover while holding Ctrl key');
  ComboTrigger.Items.Add('Hover while holding Alt key');

  // 2. GroupBox: Voice Selection
  GroupVoices := TGroupBox.Create(Self);
  GroupVoices.Parent := Self;
  GroupVoices.Left := 16;
  GroupVoices.Top := 70;
  GroupVoices.Width := 488;
  GroupVoices.Height := 160;
  GroupVoices.Caption := 'Voice Selection (Installed Windows Voices)';

  // Source text voice (Greek / Hebrew)
  LabelSource := TLabel.Create(GroupVoices);
  LabelSource.Parent := GroupVoices;
  LabelSource.Left := 12;
  LabelSource.Top := 16;
  LabelSource.Caption := 'Source Voice (Hebrew / Greek original text):';

  ComboSourceVoice := TComboBox.Create(GroupVoices);
  ComboSourceVoice.Parent := GroupVoices;
  ComboSourceVoice.Left := 12;
  ComboSourceVoice.Top := 36;
  ComboSourceVoice.Width := 340;
  ComboSourceVoice.Style := csDropDownList;

  ButtonTestSource := TButton.Create(GroupVoices);
  ButtonTestSource.Parent := GroupVoices;
  ButtonTestSource.Left := 362;
  ButtonTestSource.Top := 34;
  ButtonTestSource.Width := 110;
  ButtonTestSource.Height := 28;
  ButtonTestSource.Caption := 'Test Source';
  ButtonTestSource.OnClick := @ButtonTestSourceClick;

  // Destination translation voice (English, etc.)
  LabelDest := TLabel.Create(GroupVoices);
  LabelDest.Parent := GroupVoices;
  LabelDest.Left := 12;
  LabelDest.Top := 76;
  LabelDest.Caption := 'Translation Voice (English / Destination text):';

  ComboDestVoice := TComboBox.Create(GroupVoices);
  ComboDestVoice.Parent := GroupVoices;
  ComboDestVoice.Left := 12;
  ComboDestVoice.Top := 96;
  ComboDestVoice.Width := 340;
  ComboDestVoice.Style := csDropDownList;

  ButtonTestDest := TButton.Create(GroupVoices);
  ButtonTestDest.Parent := GroupVoices;
  ButtonTestDest.Left := 362;
  ButtonTestDest.Top := 94;
  ButtonTestDest.Width := 110;
  ButtonTestDest.Height := 28;
  ButtonTestDest.Caption := 'Test Translation';
  ButtonTestDest.OnClick := @ButtonTestDestClick;

  // Populate voice lists
  if Assigned(TTSManager) then
  begin
    for i := 0 to TTSManager.VoiceList.Count - 1 do
    begin
      ComboSourceVoice.Items.Add(TTSManager.VoiceList[i]);
      ComboDestVoice.Items.Add(TTSManager.VoiceList[i]);
    end;
  end;

  if ComboSourceVoice.Items.Count = 0 then
  begin
    ComboSourceVoice.Items.Add('Default Windows Voice');
    ComboDestVoice.Items.Add('Default Windows Voice');
  end;

  // 3. GroupBox: Audio Tuning
  GroupAudio := TGroupBox.Create(Self);
  GroupAudio.Parent := Self;
  GroupAudio.Left := 16;
  GroupAudio.Top := 240;
  GroupAudio.Width := 488;
  GroupAudio.Height := 125;
  GroupAudio.Caption := 'Speech Tuning';

  // Speed / Rate
  LabelSpeed := TLabel.Create(GroupAudio);
  LabelSpeed.Parent := GroupAudio;
  LabelSpeed.Left := 12;
  LabelSpeed.Top := 16;
  LabelSpeed.Caption := 'Speech Rate (Speed):';

  TrackSpeed := TTrackBar.Create(GroupAudio);
  TrackSpeed.Parent := GroupAudio;
  TrackSpeed.Left := 150;
  TrackSpeed.Top := 12;
  TrackSpeed.Width := 250;
  TrackSpeed.Min := -10;
  TrackSpeed.Max := 10;
  TrackSpeed.Position := 0;
  TrackSpeed.OnChange := @TrackSpeedChange;

  LabelSpeedVal := TLabel.Create(GroupAudio);
  LabelSpeedVal.Parent := GroupAudio;
  LabelSpeedVal.Left := 415;
  LabelSpeedVal.Top := 16;
  LabelSpeedVal.Caption := 'Normal (0)';

  // Volume
  LabelVolume := TLabel.Create(GroupAudio);
  LabelVolume.Parent := GroupAudio;
  LabelVolume.Left := 12;
  LabelVolume.Top := 56;
  LabelVolume.Caption := 'Volume:';

  TrackVolume := TTrackBar.Create(GroupAudio);
  TrackVolume.Parent := GroupAudio;
  TrackVolume.Left := 150;
  TrackVolume.Top := 52;
  TrackVolume.Width := 250;
  TrackVolume.Min := 0;
  TrackVolume.Max := 100;
  TrackVolume.Position := 100;
  TrackVolume.OnChange := @TrackVolumeChange;

  LabelVolumeVal := TLabel.Create(GroupAudio);
  LabelVolumeVal.Parent := GroupAudio;
  LabelVolumeVal.Left := 415;
  LabelVolumeVal.Top := 56;
  LabelVolumeVal.Caption := '100%';

  // 4. Buttons Panel
  PanelButtons := TPanel.Create(Self);
  PanelButtons.Parent := Self;
  PanelButtons.Align := alBottom;
  PanelButtons.Height := 48;
  PanelButtons.BevelOuter := bvNone;

  ButtonCancel := TButton.Create(PanelButtons);
  ButtonCancel.Parent := PanelButtons;
  ButtonCancel.Left := 404;
  ButtonCancel.Top := 10;
  ButtonCancel.Width := 100;
  ButtonCancel.Height := 30;
  ButtonCancel.Caption := 'Cancel';
  ButtonCancel.Cancel := True;
  ButtonCancel.ModalResult := mrCancel;
  ButtonCancel.OnClick := @ButtonCancelClick;

  ButtonOK := TButton.Create(PanelButtons);
  ButtonOK.Parent := PanelButtons;
  ButtonOK.Left := 296;
  ButtonOK.Top := 10;
  ButtonOK.Width := 100;
  ButtonOK.Height := 30;
  ButtonOK.Caption := 'OK';
  ButtonOK.Default := True;
  ButtonOK.ModalResult := mrOk;
  ButtonOK.OnClick := @ButtonOKClick;

  Color := CurrentSkin.PanelBg;
  Font.Color := CurrentSkin.PanelText;
end;

procedure TFormVoiceSettings.FormShow(Sender: TObject);
begin
  HandleNeeded;
  CheckEnable.HandleNeeded;
  ComboTrigger.HandleNeeded;
  GroupVoices.HandleNeeded;
  ComboSourceVoice.HandleNeeded;
  ButtonTestSource.HandleNeeded;
  ComboDestVoice.HandleNeeded;
  ButtonTestDest.HandleNeeded;
  GroupAudio.HandleNeeded;
  TrackSpeed.HandleNeeded;
  TrackVolume.HandleNeeded;
  PanelButtons.HandleNeeded;
  ButtonOK.HandleNeeded;
  ButtonCancel.HandleNeeded;

  ApplySkinToForm(Self, CurrentSkin);

  {$IFDEF WINDOWS}
  HookCheckBoxControl(CheckEnable);
  HookComboBoxControl(ComboTrigger);
  HookGroupBoxControl(GroupVoices);
  HookComboBoxControl(ComboSourceVoice);
  HookButtonControl(ButtonTestSource);
  HookComboBoxControl(ComboDestVoice);
  HookButtonControl(ButtonTestDest);
  HookGroupBoxControl(GroupAudio);
  HookTrackBarControl(TrackSpeed);
  HookTrackBarControl(TrackVolume);
  HookButtonControl(ButtonOK);
  HookButtonControl(ButtonCancel);
  {$ENDIF}

  Color := CurrentSkin.PanelBg;
  Font.Color := CurrentSkin.PanelText;
  PanelButtons.Color := CurrentSkin.PanelBg;
  PanelButtons.Font.Color := CurrentSkin.PanelText;

  CheckEnable.Color := CurrentSkin.PanelBg;
  CheckEnable.Font.Color := CurrentSkin.PanelText;
  LabelTrigger.Font.Color := CurrentSkin.PanelText;
  ComboTrigger.Color := CurrentSkin.WindowBg;
  ComboTrigger.Font.Color := CurrentSkin.WindowText;

  GroupVoices.Color := CurrentSkin.PanelBg;
  GroupVoices.Font.Color := CurrentSkin.PanelText;
  LabelSource.Font.Color := CurrentSkin.PanelText;
  ComboSourceVoice.Color := CurrentSkin.WindowBg;
  ComboSourceVoice.Font.Color := CurrentSkin.WindowText;
  ButtonTestSource.Font.Color := CurrentSkin.PanelText;

  LabelDest.Font.Color := CurrentSkin.PanelText;
  ComboDestVoice.Color := CurrentSkin.WindowBg;
  ComboDestVoice.Font.Color := CurrentSkin.WindowText;
  ButtonTestDest.Font.Color := CurrentSkin.PanelText;

  GroupAudio.Color := CurrentSkin.PanelBg;
  GroupAudio.Font.Color := CurrentSkin.PanelText;
  LabelSpeed.Font.Color := CurrentSkin.PanelText;
  LabelSpeedVal.Font.Color := CurrentSkin.PanelText;
  LabelVolume.Font.Color := CurrentSkin.PanelText;
  LabelVolumeVal.Font.Color := CurrentSkin.PanelText;

  ButtonOK.Font.Color := CurrentSkin.PanelText;
  ButtonCancel.Font.Color := CurrentSkin.PanelText;

  CheckEnable.Invalidate;
  ComboTrigger.Invalidate;
  GroupVoices.Invalidate;
  ComboSourceVoice.Invalidate;
  ButtonTestSource.Invalidate;
  ComboDestVoice.Invalidate;
  ButtonTestDest.Invalidate;
  GroupAudio.Invalidate;
  TrackSpeed.Invalidate;
  TrackVolume.Invalidate;
  ButtonOK.Invalidate;
  ButtonCancel.Invalidate;
  Invalidate;
end;

procedure TFormVoiceSettings.LoadSettings;
begin
  if TTSManager = nil then exit;

  CheckEnable.Checked := TTSManager.Enabled;
  ComboTrigger.ItemIndex := Integer(TTSManager.TriggerMode);

  if (TTSManager.SourceVoiceIndex >= 0) and (TTSManager.SourceVoiceIndex < ComboSourceVoice.Items.Count) then
    ComboSourceVoice.ItemIndex := TTSManager.SourceVoiceIndex
  else
    ComboSourceVoice.ItemIndex := 0;

  if (TTSManager.DestVoiceIndex >= 0) and (TTSManager.DestVoiceIndex < ComboDestVoice.Items.Count) then
    ComboDestVoice.ItemIndex := TTSManager.DestVoiceIndex
  else
    ComboDestVoice.ItemIndex := 0;

  TrackSpeed.Position := TTSManager.Rate;
  TrackSpeedChange(TrackSpeed);

  TrackVolume.Position := TTSManager.Volume;
  TrackVolumeChange(TrackVolume);
end;

procedure TFormVoiceSettings.SaveSettings;
begin
  if TTSManager = nil then exit;

  TTSManager.Enabled := CheckEnable.Checked;
  TTSManager.TriggerMode := TTTSTriggerMode(ComboTrigger.ItemIndex);

  TTSManager.SourceVoiceIndex := ComboSourceVoice.ItemIndex;
  if ComboSourceVoice.ItemIndex >= 0 then
    TTSManager.SourceVoiceName := ComboSourceVoice.Text;

  TTSManager.DestVoiceIndex := ComboDestVoice.ItemIndex;
  if ComboDestVoice.ItemIndex >= 0 then
    TTSManager.DestVoiceName := ComboDestVoice.Text;

  TTSManager.Rate := TrackSpeed.Position;
  TTSManager.Volume := TrackVolume.Position;
end;

procedure TFormVoiceSettings.TrackSpeedChange(Sender: TObject);
begin
  if TrackSpeed.Position = 0 then
    LabelSpeedVal.Caption := 'Normal (0)'
  else if TrackSpeed.Position > 0 then
    LabelSpeedVal.Caption := Format('+%d Fast', [TrackSpeed.Position])
  else
    LabelSpeedVal.Caption := Format('%d Slow', [TrackSpeed.Position]);
end;

procedure TFormVoiceSettings.TrackVolumeChange(Sender: TObject);
begin
  LabelVolumeVal.Caption := Format('%d%%', [TrackVolume.Position]);
end;

procedure TFormVoiceSettings.ButtonTestSourceClick(Sender: TObject);
begin
  if TTSManager = nil then exit;
  TTSManager.SpeakTest(ComboSourceVoice.ItemIndex, 'Ἐν ἀρχῇ ἦν ὁ λόγος', TrackSpeed.Position, TrackVolume.Position);
end;

procedure TFormVoiceSettings.ButtonTestDestClick(Sender: TObject);
begin
  if TTSManager = nil then exit;
  TTSManager.SpeakTest(ComboDestVoice.ItemIndex, 'In the beginning was the Word', TrackSpeed.Position, TrackVolume.Position);
end;

procedure TFormVoiceSettings.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormVoiceSettings.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
