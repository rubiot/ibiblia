unit FormApplicationPreferences;

{$mode objfpc}{$H+}

interface

uses
  ui_style, ui_icons, Classes, SysUtils, Forms, Controls, Graphics, StdCtrls,
  ExtCtrls, Project;

type
  TApplicationPreferences = record
    Language: string;
    PopupTrigger: TPopupTrigger;
    AutoSave: Boolean;
    SaveOnLastStatus: Boolean;
    IconFamily: TAppIconFamily;
    ToolbarIconSize: TToolbarIconSize;
  end;

  TApplicationPreferencesChangedEvent = procedure(
    const Preferences: TApplicationPreferences) of object;

  { TFrmApplicationPreferences }

  TFrmApplicationPreferences = class(TForm)
  private
    FAutoSaveCheckBox: TCheckBox;
    FSaveOnLastStatusCheckBox: TCheckBox;
    FCloseButton: TButton;
    FIconFamilyComboBox: TComboBox;
    FIconFamilyLabel: TLabel;
    FIconFamilyBrandLabel: TLabel;
    FToolbarIconSizeComboBox: TComboBox;
    FToolbarIconSizeLabel: TLabel;
    FIconPreview: array[0..5] of TImage;
    FLanguageComboBox: TComboBox;
    FLanguageLabel: TLabel;
    FOnPreferencesChanged: TApplicationPreferencesChangedEvent;
    FPopupTriggerComboBox: TComboBox;
    FPopupTriggerLabel: TLabel;
    FPreferences: TApplicationPreferences;
    FScopeLabel: TLabel;
    FUpdatingControls: Boolean;
    procedure AutoSaveCheckBoxChange(Sender: TObject);
    procedure SaveOnLastStatusCheckBoxChange(Sender: TObject);
    procedure BuildControls;
    procedure CloseButtonClick(Sender: TObject);
    procedure IconFamilyComboBoxChange(Sender: TObject);
    procedure ToolbarIconSizeComboBoxChange(Sender: TObject);
    procedure LanguageComboBoxChange(Sender: TObject);
    procedure NotifyPreferencesChanged;
    procedure PopupTriggerComboBoxChange(Sender: TObject);
    procedure RefreshCaptions;
    procedure RefreshIconFamilyItems;
    procedure RefreshToolbarIconSizeItems;
    procedure RefreshIconPreview;
    procedure RefreshLanguageItems;
    procedure RefreshPopupTriggerItems;
    procedure SelectLanguage(const Language: string);
    procedure SelectPopupTrigger(PopupTrigger: TPopupTrigger);
  protected
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
  public
    constructor Create(AOwner: TComponent); override;
    class procedure Execute(AOwner: TComponent;
      const Preferences: TApplicationPreferences;
      OnPreferencesChanged: TApplicationPreferencesChangedEvent);
  public
    procedure AfterConstruction; override;
  end;

implementation

resourcestring
  SPreferencesTitle = 'iBiblia preferences';
  SPreferencesScope = 'These preferences apply to all projects on this computer.';
  SInterfaceLanguage = '&Interface language';
  SIconFamily = 'Icon &family';
  SIconFamilyBrand = 'The iBiblia application icon is the same in every icon family.';
  SToolbarIconSize = '&Toolbar icon size';
  SToolbarIconSizeSmall = 'Small (16 px)';
  SToolbarIconSizeMedium = 'Medium (22 px)';
  SToolbarIconSizeLarge = 'Large (28 px)';
  SLanguageEnglish = 'English';
  SLanguagePortuguese = 'Português';
  SShowWordDefinition = 'Show &word definition';
  SPopupTriggerDisabled = 'Disabled';
  SPopupTriggerMouseHover = 'Mouse hover';
  SPopupTriggerAltMouseHover = 'Alt+Mouse hover';
  SPopupTriggerCtrlMouseHover = 'Ctrl+Mouse hover';
  SSaveAutomatically = 'Save &automatically when changing verse';
  SSaveOnLastStatus = 'Save the project when a verse is set to the &last status';
  SCloseButton = '&Close';

const
  LANGUAGE_ENGLISH = 'en';
  LANGUAGE_PORTUGUESE = 'pt';

{ TFrmApplicationPreferences }

constructor TFrmApplicationPreferences.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  ApplyFormStyle(Self);

  Position := poMainFormCenter;
  BorderStyle := bsDialog;
  ClientWidth := 600;
  ClientHeight := 536;
  Constraints.MinWidth := 600;
  Constraints.MinHeight := 536;

  BuildControls;
  RefreshCaptions;
end;

procedure TFrmApplicationPreferences.BuildControls;
var
  ButtonPanel: TPanel;
  ContentPanel: TPanel;
  I: Integer;
begin
  ButtonPanel := TPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.Align := alBottom;
  ButtonPanel.Height := 48;
  ButtonPanel.BevelOuter := bvNone;
  ButtonPanel.BorderSpacing.Around := 16;

  FCloseButton := TButton.Create(Self);
  FCloseButton.Parent := ButtonPanel;
  FCloseButton.Align := alRight;
  FCloseButton.Width := 112;
  FCloseButton.Default := True;
  FCloseButton.Cancel := True;
  FCloseButton.OnClick := @CloseButtonClick;

  ContentPanel := TPanel.Create(Self);
  ContentPanel.Parent := Self;
  ContentPanel.Align := alClient;
  ContentPanel.BevelOuter := bvNone;
  ContentPanel.BorderSpacing.Around := 16;
  ContentPanel.Width := ClientWidth - 32;

  FScopeLabel := TLabel.Create(Self);
  FScopeLabel.Parent := ContentPanel;
  FScopeLabel.SetBounds(0, 0, 568, 40);
  FScopeLabel.Anchors := [akLeft, akTop, akRight];
  FScopeLabel.AutoSize := False;
  FScopeLabel.WordWrap := True;

  FLanguageLabel := TLabel.Create(Self);
  FLanguageLabel.Parent := ContentPanel;
  FLanguageLabel.SetBounds(0, 56, 216, 24);
  FLanguageLabel.AutoSize := False;
  FLanguageLabel.Layout := tlCenter;

  FLanguageComboBox := TComboBox.Create(Self);
  FLanguageComboBox.Parent := ContentPanel;
  FLanguageComboBox.SetBounds(232, 56, 336, 28);
  FLanguageComboBox.Anchors := [akLeft, akTop, akRight];
  FLanguageComboBox.Style := csDropDownList;
  FLanguageComboBox.TabOrder := 0;
  FLanguageComboBox.OnChange := @LanguageComboBoxChange;
  FLanguageLabel.FocusControl := FLanguageComboBox;

  FIconFamilyLabel := TLabel.Create(Self);
  FIconFamilyLabel.Parent := ContentPanel;
  FIconFamilyLabel.SetBounds(0, 104, 216, 24);
  FIconFamilyLabel.AutoSize := False;
  FIconFamilyLabel.Layout := tlCenter;

  FIconFamilyComboBox := TComboBox.Create(Self);
  FIconFamilyComboBox.Name := 'IconFamilyComboBox';
  FIconFamilyComboBox.Parent := ContentPanel;
  FIconFamilyComboBox.SetBounds(232, 104, 336, 28);
  FIconFamilyComboBox.Anchors := [akLeft, akTop, akRight];
  FIconFamilyComboBox.Style := csDropDownList;
  FIconFamilyComboBox.TabOrder := 1;
  FIconFamilyComboBox.OnChange := @IconFamilyComboBoxChange;
  FIconFamilyLabel.FocusControl := FIconFamilyComboBox;

  for I := Low(FIconPreview) to High(FIconPreview) do
  begin
    FIconPreview[I] := TImage.Create(Self);
    FIconPreview[I].Parent := ContentPanel;
    FIconPreview[I].SetBounds(232 + I * 48, 152, 32, 32);
    FIconPreview[I].Center := True;
    FIconPreview[I].Proportional := True;
    FIconPreview[I].Stretch := True;
  end;

  FIconFamilyBrandLabel := TLabel.Create(Self);
  FIconFamilyBrandLabel.Parent := ContentPanel;
  FIconFamilyBrandLabel.SetBounds(0, 200, 568, 32);
  FIconFamilyBrandLabel.Anchors := [akLeft, akTop, akRight];
  FIconFamilyBrandLabel.AutoSize := False;
  FIconFamilyBrandLabel.WordWrap := True;

  FToolbarIconSizeLabel := TLabel.Create(Self);
  FToolbarIconSizeLabel.Parent := ContentPanel;
  FToolbarIconSizeLabel.SetBounds(0, 244, 216, 24);
  FToolbarIconSizeLabel.AutoSize := False;
  FToolbarIconSizeLabel.Layout := tlCenter;

  FToolbarIconSizeComboBox := TComboBox.Create(Self);
  FToolbarIconSizeComboBox.Parent := ContentPanel;
  FToolbarIconSizeComboBox.SetBounds(232, 244, 336, 28);
  FToolbarIconSizeComboBox.Anchors := [akLeft, akTop, akRight];
  FToolbarIconSizeComboBox.Style := csDropDownList;
  FToolbarIconSizeComboBox.TabOrder := 2;
  FToolbarIconSizeComboBox.OnChange := @ToolbarIconSizeComboBoxChange;
  FToolbarIconSizeLabel.FocusControl := FToolbarIconSizeComboBox;

  FPopupTriggerLabel := TLabel.Create(Self);
  FPopupTriggerLabel.Parent := ContentPanel;
  FPopupTriggerLabel.SetBounds(0, 292, 216, 24);
  FPopupTriggerLabel.AutoSize := False;
  FPopupTriggerLabel.Layout := tlCenter;

  FPopupTriggerComboBox := TComboBox.Create(Self);
  FPopupTriggerComboBox.Parent := ContentPanel;
  FPopupTriggerComboBox.SetBounds(232, 292, 336, 28);
  FPopupTriggerComboBox.Anchors := [akLeft, akTop, akRight];
  FPopupTriggerComboBox.Style := csDropDownList;
  FPopupTriggerComboBox.TabOrder := 3;
  FPopupTriggerComboBox.OnChange := @PopupTriggerComboBoxChange;
  FPopupTriggerLabel.FocusControl := FPopupTriggerComboBox;

  FAutoSaveCheckBox := TCheckBox.Create(Self);
  FAutoSaveCheckBox.Parent := ContentPanel;
  FAutoSaveCheckBox.SetBounds(0, 340, 568, 32);
  FAutoSaveCheckBox.Anchors := [akLeft, akTop, akRight];
  FAutoSaveCheckBox.TabOrder := 4;
  FAutoSaveCheckBox.OnChange := @AutoSaveCheckBoxChange;

  FSaveOnLastStatusCheckBox := TCheckBox.Create(Self);
  FSaveOnLastStatusCheckBox.Parent := ContentPanel;
  FSaveOnLastStatusCheckBox.SetBounds(0, 380, 568, 32);
  FSaveOnLastStatusCheckBox.Anchors := [akLeft, akTop, akRight];
  FSaveOnLastStatusCheckBox.TabOrder := 5;
  FSaveOnLastStatusCheckBox.OnChange := @SaveOnLastStatusCheckBoxChange;
end;

procedure TFrmApplicationPreferences.CloseButtonClick(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TFrmApplicationPreferences.NotifyPreferencesChanged;
begin
  if Assigned(FOnPreferencesChanged) then
    FOnPreferencesChanged(FPreferences);
end;

procedure TFrmApplicationPreferences.LanguageComboBoxChange(Sender: TObject);
begin
  if FUpdatingControls then
    Exit;

  case FLanguageComboBox.ItemIndex of
    0: FPreferences.Language := LANGUAGE_ENGLISH;
    1: FPreferences.Language := LANGUAGE_PORTUGUESE;
  else
    Exit;
  end;

  NotifyPreferencesChanged;
  RefreshCaptions;
end;

procedure TFrmApplicationPreferences.IconFamilyComboBoxChange(Sender: TObject);
begin
  if FUpdatingControls or (FIconFamilyComboBox.ItemIndex < 0) then
    Exit;

  FPreferences.IconFamily := TAppIconFamily(PtrInt(
    FIconFamilyComboBox.Items.Objects[FIconFamilyComboBox.ItemIndex]));
  NotifyPreferencesChanged;
  RefreshIconPreview;
end;

procedure TFrmApplicationPreferences.ToolbarIconSizeComboBoxChange(Sender: TObject);
begin
  if FUpdatingControls or (FToolbarIconSizeComboBox.ItemIndex < 0) then
    Exit;

  FPreferences.ToolbarIconSize := TToolbarIconSize(PtrInt(
    FToolbarIconSizeComboBox.Items.Objects[FToolbarIconSizeComboBox.ItemIndex]));
  NotifyPreferencesChanged;
end;

procedure TFrmApplicationPreferences.PopupTriggerComboBoxChange(Sender: TObject);
begin
  if FUpdatingControls or (FPopupTriggerComboBox.ItemIndex < 0) then
    Exit;

  FPreferences.PopupTrigger := TPopupTrigger(PtrInt(
    FPopupTriggerComboBox.Items.Objects[FPopupTriggerComboBox.ItemIndex]));
  NotifyPreferencesChanged;
end;

procedure TFrmApplicationPreferences.AutoSaveCheckBoxChange(Sender: TObject);
begin
  if FUpdatingControls then
    Exit;

  FPreferences.AutoSave := FAutoSaveCheckBox.Checked;
  NotifyPreferencesChanged;
end;

procedure TFrmApplicationPreferences.SaveOnLastStatusCheckBoxChange(
  Sender: TObject);
begin
  if FUpdatingControls then
    Exit;

  FPreferences.SaveOnLastStatus := FSaveOnLastStatusCheckBox.Checked;
  NotifyPreferencesChanged;
end;

procedure TFrmApplicationPreferences.SelectLanguage(const Language: string);
begin
  if SameText(Language, LANGUAGE_PORTUGUESE) or
    SameText(Copy(Language, 1, 3), LANGUAGE_PORTUGUESE + '_') or
    SameText(Copy(Language, 1, 3), LANGUAGE_PORTUGUESE + '-') then
    FLanguageComboBox.ItemIndex := 1
  else
    FLanguageComboBox.ItemIndex := 0;
end;

procedure TFrmApplicationPreferences.SelectPopupTrigger(
  PopupTrigger: TPopupTrigger);
var
  ItemIndex: Integer;
begin
  FPopupTriggerComboBox.ItemIndex := -1;
  for ItemIndex := 0 to FPopupTriggerComboBox.Items.Count - 1 do
    if PtrInt(FPopupTriggerComboBox.Items.Objects[ItemIndex]) = Ord(PopupTrigger) then
    begin
      FPopupTriggerComboBox.ItemIndex := ItemIndex;
      Break;
    end;
end;

procedure TFrmApplicationPreferences.RefreshLanguageItems;
begin
  FLanguageComboBox.Items.BeginUpdate;
  try
    FLanguageComboBox.Items.Clear;
    FLanguageComboBox.Items.Add(SLanguageEnglish);
    FLanguageComboBox.Items.Add(SLanguagePortuguese);
    SelectLanguage(FPreferences.Language);
  finally
    FLanguageComboBox.Items.EndUpdate;
  end;
end;

procedure TFrmApplicationPreferences.RefreshPopupTriggerItems;

  procedure AddTrigger(const Caption: string; PopupTrigger: TPopupTrigger);
  begin
    FPopupTriggerComboBox.Items.AddObject(Caption,
      TObject(PtrInt(Ord(PopupTrigger))));
  end;

begin
  FPopupTriggerComboBox.Items.BeginUpdate;
  try
    FPopupTriggerComboBox.Items.Clear;
    AddTrigger(SPopupTriggerDisabled, ptDisabled);
    AddTrigger(SPopupTriggerMouseHover, ptMouseHover);
    AddTrigger(SPopupTriggerAltMouseHover, ptAltMouseHover);
    AddTrigger(SPopupTriggerCtrlMouseHover, ptCtrlMouseHover);
    SelectPopupTrigger(FPreferences.PopupTrigger);
  finally
    FPopupTriggerComboBox.Items.EndUpdate;
  end;
end;

procedure TFrmApplicationPreferences.RefreshIconFamilyItems;
var
  Family: TAppIconFamily;
begin
  FIconFamilyComboBox.Items.BeginUpdate;
  try
    FIconFamilyComboBox.Items.Clear;
    for Family := Low(TAppIconFamily) to High(TAppIconFamily) do
      FIconFamilyComboBox.Items.AddObject(AppIconFamilyName(Family),
        TObject(PtrInt(Ord(Family))));
    FIconFamilyComboBox.ItemIndex := Ord(FPreferences.IconFamily);
  finally
    FIconFamilyComboBox.Items.EndUpdate;
  end;
end;

procedure TFrmApplicationPreferences.RefreshToolbarIconSizeItems;
begin
  FToolbarIconSizeComboBox.Items.BeginUpdate;
  try
    FToolbarIconSizeComboBox.Items.Clear;
    FToolbarIconSizeComboBox.Items.AddObject(SToolbarIconSizeSmall, TObject(PtrInt(Ord(tisSmall))));
    FToolbarIconSizeComboBox.Items.AddObject(SToolbarIconSizeMedium, TObject(PtrInt(Ord(tisMedium))));
    FToolbarIconSizeComboBox.Items.AddObject(SToolbarIconSizeLarge, TObject(PtrInt(Ord(tisLarge))));
    FToolbarIconSizeComboBox.ItemIndex := Ord(FPreferences.ToolbarIconSize);
  finally
    FToolbarIconSizeComboBox.Items.EndUpdate;
  end;
end;

procedure TFrmApplicationPreferences.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  RefreshIconPreview;
end;

procedure TFrmApplicationPreferences.RefreshIconPreview;
const
  PreviewIcons: array[0..5] of TAppIcon = (aiProjectOpen, aiProjectSave,
    aiAssociationAuto, aiHistoryBack, aiVerseNext, aiLlmOptions);
var
  I: Integer;
  PreviewImage: TPortableNetworkGraphic;
begin
  if not Assigned(FIconPreview[High(FIconPreview)]) then
    Exit;
  PreviewImage := TPortableNetworkGraphic.Create;
  try
    for I := Low(FIconPreview) to High(FIconPreview) do
    begin
      LoadAppIconForFamily(PreviewIcons[I], FIconPreview[I].Width, PreviewImage,
        FPreferences.IconFamily);
      FIconPreview[I].Picture.Assign(PreviewImage);
    end;
  finally
    PreviewImage.Free;
  end;
end;

procedure TFrmApplicationPreferences.RefreshCaptions;
begin
  FUpdatingControls := True;
  try
    Caption := SPreferencesTitle;
    FScopeLabel.Caption := SPreferencesScope;
    FLanguageLabel.Caption := SInterfaceLanguage;
    FIconFamilyLabel.Caption := SIconFamily;
    FIconFamilyBrandLabel.Caption := SIconFamilyBrand;
    FToolbarIconSizeLabel.Caption := SToolbarIconSize;
    FPopupTriggerLabel.Caption := SShowWordDefinition;
    FAutoSaveCheckBox.Caption := SSaveAutomatically;
    FSaveOnLastStatusCheckBox.Caption := SSaveOnLastStatus;
    FCloseButton.Caption := SCloseButton;
    RefreshLanguageItems;
    RefreshIconFamilyItems;
    RefreshToolbarIconSizeItems;
    RefreshIconPreview;
    RefreshPopupTriggerItems;
  finally
    FUpdatingControls := False;
  end;
end;

class procedure TFrmApplicationPreferences.Execute(AOwner: TComponent;
  const Preferences: TApplicationPreferences;
  OnPreferencesChanged: TApplicationPreferencesChangedEvent);
var
  PreferencesForm: TFrmApplicationPreferences;
begin
  PreferencesForm := TFrmApplicationPreferences.Create(AOwner);
  try
    PreferencesForm.FPreferences := Preferences;
    PreferencesForm.FOnPreferencesChanged := OnPreferencesChanged;
    PreferencesForm.FUpdatingControls := True;
    try
      PreferencesForm.FAutoSaveCheckBox.Checked := Preferences.AutoSave;
      PreferencesForm.FSaveOnLastStatusCheckBox.Checked :=
        Preferences.SaveOnLastStatus;
    finally
      PreferencesForm.FUpdatingControls := False;
    end;
    PreferencesForm.RefreshCaptions;
    PreferencesForm.ShowModal;
  finally
    PreferencesForm.Free;
  end;
end;

procedure TFrmApplicationPreferences.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
  StyleControl(FScopeLabel, uirHint);
  StyleControl(FIconFamilyBrandLabel, uirHint);
  RefreshIconPreview;
end;


end.
