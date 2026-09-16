unit unitabout;

{$mode objfpc}{$H+}

interface

uses
  ui_style, ui_icons, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, fileinfo;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    BrandImage: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadBrandImage;
  protected
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
  public
    procedure AfterConstruction; override;
  end;

var
  FormAbout: TFormAbout;

implementation

{ TFormAbout }

procedure TFormAbout.FormCreate(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
begin
  ApplyFormStyle(Self);
  FileVerInfo := TFileVersionInfo.Create(nil);
  FileVerInfo.ReadFileInfo;
  LabelVersion.Caption := FileVerInfo.VersionStrings.Values['FileVersion'];
  FileVerInfo.Free;
end;

procedure TFormAbout.LoadBrandImage;
var
  Image: TPortableNetworkGraphic;
begin
  if not Assigned(BrandImage) or (BrandImage.Width <= 0) then Exit;
  Image := TPortableNetworkGraphic.Create;
  try
    LoadAppIcon(aiAppIBiblia, BrandImage.Width, Image);
    BrandImage.Picture.Assign(Image);
  finally
    Image.Free;
  end;
end;

procedure TFormAbout.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  if (AMode = lapAutoAdjustForDPI) and not (csLoading in ComponentState) then
    LoadBrandImage;
end;

procedure TFormAbout.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
  LoadBrandImage;
  Label1.Font.Size := 20;
  Label1.Font.Color := UIAccentColor;
  StyleControl(LabelVersion, uirHint);
  StyleControl(Label3, uirHint);
end;

{$IFNDEF FPCUNIT}
initialization
  {$R unitabout.lfm}
{$ENDIF}

end.

