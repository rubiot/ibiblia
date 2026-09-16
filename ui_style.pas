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
  UISurfaceColor = TColor($00F8F6F3);
  UIContentColor = clWhite;
  UITextColor = TColor($00372F27);
  UIMutedTextColor = TColor($007C6D60);
  UIBorderColor = TColor($00E5DDD6);
  UISuccessColor = TColor($003D8015);
  UIWarningColor = TColor($000E6DA1);
  UIErrorColor = TColor($002626DC);
  UISpacing = 8;
  UIDialogMargin = 16;
  UIButtonHeight = 32;

resourcestring
  SUICancel = 'Cancel';

function UIScale(AControl: TControl; LogicalPixels: Integer): Integer;
procedure ApplyFormStyle(AForm: TCustomForm);
procedure ApplyDialogStyle(AForm: TCustomForm);
procedure StyleControl(AControl: TControl; ARole: TUIControlRole);
procedure SetButtonIcon(AButton: TBitBtn; AIcon: TAppIcon);

implementation

uses
  Math, StdCtrls, ExtCtrls, ComCtrls, Grids, CheckLst, ImgList;

type
  TControlAccess = class(TControl);

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
  AForm.Font.Name := 'Segoe UI';
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
        Control.Font.Name := 'Segoe UI';
        Control.Font.Size := 10;
        Control.Font.Style := [fsBold];
        Control.Font.Color := UIAccentColor;
      end;
    uirHint:
      Control.Font.Color := UIMutedTextColor;
    uirButton:
      begin
        Control.Font.Name := 'Segoe UI';
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
    TControlAccess(AControl).Font.Name := 'Segoe UI';
    TControlAccess(AControl).Font.Size := 10;
  end;

  if (AControl is TPanel) and TPanel(AControl).ParentColor then
    StyleControl(AControl, uirSurface);
  if (AControl is TScrollBox) or (AControl is TCustomListBox) then
    StyleControl(AControl, uirContent);
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
  end;
end;

end.
