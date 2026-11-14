unit ui_style;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Forms, Graphics, Buttons, ui_icons;

type
  TUIControlRole = (uirSurface, uirContent, uirSectionTitle, uirHint,
    uirFooter, uirButton);

const
  UIAccentColor = TColor($00EB6325);
  UIGoldColor = TColor($00289AD4);
  UISuccessColor = TColor($003D8015);
  UIWarningColor = TColor($000E6DA1);
  UIErrorColor = TColor($002626DC);
  UISpacing = 8;
  UIDialogMargin = 16;
  UIButtonHeight = 32;

resourcestring
  SUICancel = 'Cancel';

{ Surface, content, text and border colours follow the current skin
  (View > Skins). With the Classic skin they are 1.1.0's own palette. }
function UISurfaceColor: TColor;
function UIContentColor: TColor;
function UITextColor: TColor;
function UIMutedTextColor: TColor;
function UITextMutedColor: TColor;
function UIBorderColor: TColor;
function UISuccessTextColor: TColor;
function UIWarningTextColor: TColor;
function UIErrorTextColor: TColor;
function UIAccentTextColor: TColor;

function UIDefaultFontName: string;
function UIMonoFontName: string;

function UIScale(AControl: TControl; LogicalPixels: Integer): Integer;
procedure ApplyFormStyle(AForm: TCustomForm);
procedure ApplyDialogStyle(AForm: TCustomForm);
procedure StyleControl(AControl: TControl; ARole: TUIControlRole);
procedure SetButtonIcon(AButton: TBitBtn; AIcon: TAppIcon);

implementation

uses
  Math, StdCtrls, ExtCtrls, ComCtrls, Grids, CheckLst, ImgList, unitskins;

function UISurfaceColor: TColor;
begin
  if CurrentSkin.Id = skClassic then
    Result := TColor($00F8F6F3)
  else
    Result := CurrentSkin.PanelBg;
end;

function UIContentColor: TColor;
begin
  if CurrentSkin.Id = skClassic then
    Result := clWhite
  else
    Result := CurrentSkin.WindowBg;
end;

function UITextColor: TColor;
begin
  if CurrentSkin.Id = skClassic then
    Result := TColor($00372F27)
  else
    Result := CurrentSkin.PanelText;
end;

function UIMutedTextColor: TColor;
begin
  if CurrentSkin.Id = skClassic then
    Result := TColor($007C6D60)
  else
    Result := CurrentSkin.WindowTextDim;
end;

function UITextMutedColor: TColor;
begin
  Result := UIMutedTextColor;
end;

function UIBorderColor: TColor;
begin
  if CurrentSkin.Id = skClassic then
    Result := TColor($00E5DDD6)
  else
    Result := CurrentSkin.PanelBorder;
end;

function UISuccessTextColor: TColor;
begin
  if CurrentSkin.IsDark then
    Result := MakeRGB(110, 220, 130)
  else
    Result := UISuccessColor;
end;

function UIWarningTextColor: TColor;
begin
  if CurrentSkin.IsDark then
    Result := MakeRGB(245, 190, 90)
  else
    Result := UIWarningColor;
end;

function UIErrorTextColor: TColor;
begin
  if CurrentSkin.IsDark then
    Result := MakeRGB(245, 105, 105)
  else
    Result := UIErrorColor;
end;

function UIAccentTextColor: TColor;
begin
  if CurrentSkin.Id = skClassic then
    Result := UIAccentColor
  else
    Result := CurrentSkin.HeaderColor;
end;

type
  TControlAccess = class(TControl);

function UIDefaultFontName: string;
begin
  {$IFDEF WINDOWS}
  Result := 'Segoe UI';
  {$ELSE}
  Result := 'Sans';
  {$ENDIF}
end;

function UIMonoFontName: string;
begin
  {$IFDEF WINDOWS}
  Result := 'Consolas';
  {$ELSE}
  Result := 'Monospace';
  {$ENDIF}
end;

function UIScale(AControl: TControl; LogicalPixels: Integer): Integer;
begin
  if Assigned(AControl) then
    Result := AControl.Scale96ToForm(LogicalPixels)
  else
    Result := LogicalPixels;
end;

procedure ApplyFormStyle(AForm: TCustomForm);
begin
  if not Assigned(AForm) then Exit;
  AForm.Color := UISurfaceColor;
  AForm.Font.Name := UIDefaultFontName;
  AForm.Font.Size := 10;
  AForm.Font.Color := UITextColor;
end;

procedure SetButtonIcon(AButton: TBitBtn; AIcon: TAppIcon);
const
  ImagesName = 'IBibliaButtonImages';
var
  Images: TImageList;
begin
  if not Assigned(AButton) then Exit;
  Images := TImageList(AButton.FindComponent(ImagesName));
  if not Assigned(Images) then
  begin
    Images := TImageList.Create(AButton);
    Images.Name := ImagesName;
    Images.Width := 16;
    Images.Height := 16;
    Images.Scaled := True;
  end;
  Images.Clear;
  AButton.Glyph.Clear;
  AButton.Images := Images;
  AButton.ImageWidth := 16;
  AButton.ImageIndex := AddAppIcon(Images, AIcon);
  AButton.Spacing := UIScale(AButton, UISpacing);
end;

procedure StyleControl(AControl: TControl; ARole: TUIControlRole);
var
  Control: TControlAccess;
begin
  if not Assigned(AControl) then Exit;
  Control := TControlAccess(AControl);
  case ARole of
    uirSurface, uirFooter:
      begin
        Control.Color := UISurfaceColor;
        if AControl is TPanel then
          TPanel(AControl).BevelOuter := bvNone;
      end;
    uirContent:
      Control.Color := UIContentColor;
    uirSectionTitle:
      begin
        Control.Font.Name := UIDefaultFontName;
        Control.Font.Size := 10;
        Control.Font.Style := [fsBold];
        Control.Font.Color := UIAccentColor;
      end;
    uirHint:
      Control.Font.Color := UIMutedTextColor;
    uirButton:
      begin
        Control.Font.Name := UIDefaultFontName;
        Control.Font.Size := 10;
        Control.Constraints.MinHeight := UIScale(AControl, UIButtonHeight);
        Control.Height := Max(Control.Height, Control.Constraints.MinHeight);
      end;
  end;
end;

procedure StyleDialogChrome(AControl: TControl);
var
  I: Integer;
  NativeContainer: Boolean;
begin
  // Only native interface chrome participates. RichMemo, SynEdit, TVerse,
  // comments views and custom previews retain their own content typography.
  NativeContainer := (AControl is TCustomForm) or
    (AControl is TPanel) or (AControl is TCustomGroupBox) or
    (AControl is TPageControl) or (AControl is TTabSheet) or
    (AControl is TTabControl) or (AControl is TScrollBox);

  if (AControl is TCustomButton) or (AControl is TBitBtn) then
  begin
    StyleControl(AControl, uirButton);
    if AControl is TBitBtn then
      case TBitBtn(AControl).Kind of
        bkOK, bkYes, bkAll: SetButtonIcon(TBitBtn(AControl), aiConfirm);
        bkCancel, bkNo, bkClose: SetButtonIcon(TBitBtn(AControl), aiCancel);
        bkHelp: SetButtonIcon(TBitBtn(AControl), aiHelp);
        bkRetry: SetButtonIcon(TBitBtn(AControl), aiRefresh);
      end;
  end
  else if (AControl is TLabel) or (AControl is TCustomComboBox) or
    (AControl is TCustomCheckBox) or (AControl is TCustomListBox) or
    (AControl is TCustomGrid) or
    ((AControl is TCustomEdit) and not (AControl is TCustomMemo)) or
    NativeContainer then
  begin
    TControlAccess(AControl).Font.Name := UIDefaultFontName;
    TControlAccess(AControl).Font.Size := 10;
  end;

  if (AControl is TPanel) or (AControl is TCustomGroupBox) or
     (AControl is TPageControl) or (AControl is TTabSheet) or
     (AControl is TTabControl) then
  begin
    TControlAccess(AControl).ParentColor := False;
    StyleControl(AControl, uirSurface);
    TControlAccess(AControl).Font.Color := UITextColor;
  end;
  if (AControl is TLabel) or (AControl is TCustomCheckBox) or (AControl is TRadioButton) then
    TControlAccess(AControl).Font.Color := UITextColor;
  if (AControl is TScrollBox) or (AControl is TCustomListBox) or (AControl is TCustomEdit) or
     (AControl is TCustomComboBox) then
  begin
    StyleControl(AControl, uirContent);
    TControlAccess(AControl).Font.Color := UITextColor;
  end;
  if AControl is TCustomGrid then
  begin
    TStringGrid(AControl).DefaultRowHeight := UIScale(AControl, 28);
    TStringGrid(AControl).FixedColor := UISurfaceColor;
  end;

  if NativeContainer then
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
      StyleDialogChrome(TWinControl(AControl).Controls[I]);
end;

procedure ApplyDialogStyle(AForm: TCustomForm);
begin
  ApplyFormStyle(AForm);
  if Assigned(AForm) then
  begin
    LoadApplicationIcon(AForm.Icon);
    StyleDialogChrome(AForm);
    if CurrentSkin.Id <> skClassic then
      ApplySkinToForm(AForm, CurrentSkin);
  end;
end;

end.
