unit ui_icons;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Graphics, ImgList;

type
  TAppIconFamily = (aifConnected, aifClareza, aifMosaico, aifFolio);

  { Symbol order is shared by every full application image list. Subset lists,
    such as Bible tree StateImages, use the index returned by AddAppIcon. }
  TAppIcon = (
{$I assets/icons/generated/ui_icon_enum.inc}
  );

function AppIconSymbol(AIcon: TAppIcon): string;
function AppIconIndex(AIcon: TAppIcon): Integer; inline;
function GetAppIconFamily: TAppIconFamily;
procedure SetAppIconFamily(AFamily: TAppIconFamily);
function AppIconFamilyId(AFamily: TAppIconFamily): string;
function AppIconFamilyFromId(const AId: string): TAppIconFamily;
function AppIconFamilyName(AFamily: TAppIconFamily): string;
procedure LoadAppIconForFamily(AIcon: TAppIcon; ASize: Integer;
  AImage: TPortableNetworkGraphic; AFamily: TAppIconFamily);
procedure LoadAppIcon(AIcon: TAppIcon; ASize: Integer;
  AImage: TPortableNetworkGraphic);
procedure LoadApplicationIcon(AIcon: TIcon);
function AddAppIcon(AImages: TCustomImageList; AIcon: TAppIcon): Integer;
procedure PopulateAppImageList(AImages: TCustomImageList;
  ALogicalSize: Integer = 24);

implementation

uses
  LCLType;

{$R assets/icons/generated/ui_icons.res}
{$R assets/icons/generated/ui_icon_families.res}

type
  TImageListAccess = class(TCustomImageList);
  TIconBindingEntry = record
    Bound: Boolean;
    Icon: TAppIcon;
  end;

  { Owned by its image list, so dialogs unregister themselves when closed.
    Only explicitly bound command slots participate; project colors do not. }
  TAppIconBinding = class(TComponent)
  private
    FImages: TCustomImageList;
    FEntries: array of TIconBindingEntry;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Bind(AIndex: Integer; AIcon: TAppIcon);
    procedure Refresh;
  end;

resourcestring
  SConnectedIconFamily = 'Connected texts';

var
  CurrentIconFamily: TAppIconFamily = aifConnected;
  IconBindings: TFPList;

const
  IconSizes: array[0..6] of Integer = (16, 20, 24, 32, 40, 48, 64);
  IconSymbols: array[TAppIcon] of string = (
{$I assets/icons/generated/ui_icon_names.inc}
  );
  IconFamilyIds: array[TAppIconFamily] of string =
    ('connected', 'clareza', 'mosaico', 'folio');
  BindingName = 'IBibliaIconBinding';

function GetAppIconFamily: TAppIconFamily;
begin
  Result := CurrentIconFamily;
end;

function AppIconFamilyId(AFamily: TAppIconFamily): string;
begin
  Result := IconFamilyIds[AFamily];
end;

function AppIconFamilyFromId(const AId: string): TAppIconFamily;
var
  Family: TAppIconFamily;
begin
  for Family := Low(TAppIconFamily) to High(TAppIconFamily) do
    if SameText(Trim(AId), IconFamilyIds[Family]) then Exit(Family);
  Result := aifConnected;
end;

function AppIconFamilyName(AFamily: TAppIconFamily): string;
begin
  Result := SConnectedIconFamily;
  case AFamily of
    aifConnected: Result := SConnectedIconFamily;
    aifClareza: Result := 'Clareza';
    aifMosaico: Result := 'Mosaico';
    aifFolio: Result := 'Fólio';
  end;
end;

procedure SetAppIconFamily(AFamily: TAppIconFamily);
var
  I: Integer;
begin
  if CurrentIconFamily = AFamily then Exit;
  CurrentIconFamily := AFamily;
  if Assigned(IconBindings) then
    for I := 0 to IconBindings.Count - 1 do
      TAppIconBinding(IconBindings[I]).Refresh;
end;

constructor TAppIconBinding.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImages := TCustomImageList(AOwner);
  Name := BindingName;
  if not Assigned(IconBindings) then IconBindings := TFPList.Create;
  IconBindings.Add(Self);
end;

destructor TAppIconBinding.Destroy;
begin
  if Assigned(IconBindings) then IconBindings.Remove(Self);
  inherited Destroy;
end;

procedure TAppIconBinding.Bind(AIndex: Integer; AIcon: TAppIcon);
begin
  if AIndex = 0 then SetLength(FEntries, 0);
  SetLength(FEntries, FImages.Count);
  FEntries[AIndex].Bound := True;
  FEntries[AIndex].Icon := AIcon;
end;

procedure TAppIconBinding.Refresh;
var
  I, SizeIndex: Integer;
  Image: TPortableNetworkGraphic;
begin
  if csDestroying in FImages.ComponentState then Exit;
  Image := TPortableNetworkGraphic.Create;
  FImages.BeginUpdate;
  try
    for I := 0 to High(FEntries) do
      if (I < FImages.Count) and FEntries[I].Bound then
      begin
        // Refresh any extra resolution created by the widgetset, then replace
        // each supplied resolution with its original rasterization.
        LoadAppIcon(FEntries[I].Icon, IconSizes[High(IconSizes)], Image);
        FImages.Replace(I, Image, nil, True);
        for SizeIndex := Low(IconSizes) to High(IconSizes) - 1 do
        begin
          LoadAppIcon(FEntries[I].Icon, IconSizes[SizeIndex], Image);
          FImages.Replace(I, Image, nil, False);
        end;
      end;
    TImageListAccess(FImages).MarkAsChanged;
  finally
    FImages.EndUpdate;
    Image.Free;
  end;
end;

function AppIconSymbol(AIcon: TAppIcon): string;
begin
  Result := IconSymbols[AIcon];
end;

function AppIconIndex(AIcon: TAppIcon): Integer;
begin
  Result := Ord(AIcon);
end;

procedure LoadAppIcon(AIcon: TAppIcon; ASize: Integer;
  AImage: TPortableNetworkGraphic);
begin
  LoadAppIconForFamily(AIcon, ASize, AImage, CurrentIconFamily);
end;

procedure LoadAppIconForFamily(AIcon: TAppIcon; ASize: Integer;
  AImage: TPortableNetworkGraphic; AFamily: TAppIconFamily);
var
  I, ResourceSize: Integer;
  ResourceName: string;
  Stream: TResourceStream;
begin
  ResourceSize := IconSizes[High(IconSizes)];
  if (AIcon = aiAppIBiblia) and (ASize > 64) then
  begin
    if ASize <= 128 then ResourceSize := 128 else ResourceSize := 256;
  end
  else
    for I := Low(IconSizes) to High(IconSizes) do
      if IconSizes[I] >= ASize then
      begin
        ResourceSize := IconSizes[I];
        Break;
      end;
  ResourceName := 'IBIBLIA_';
  // The brand has one source in every context, including About and title bars.
  if (AIcon <> aiAppIBiblia) and (AFamily <> aifConnected) then
    ResourceName := ResourceName + UpperCase(AppIconFamilyId(AFamily)) + '_';
  ResourceName := ResourceName + UpperCase(StringReplace(AppIconSymbol(AIcon),
    '-', '_', [rfReplaceAll])) + '_' + IntToStr(ResourceSize);
  Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    AImage.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure LoadApplicationIcon(AIcon: TIcon);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, 'IBIBLIA_APP_ICO', RT_RCDATA);
  try
    AIcon.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function AddAppIcon(AImages: TCustomImageList; AIcon: TAppIcon): Integer;
var
  I: Integer;
  Images: array[0..6] of TRasterImage;
  Binding: TAppIconBinding;
begin
  AImages.RegisterResolutions(IconSizes);
  for I := Low(Images) to High(Images) do Images[I] := nil;
  try
    for I := Low(Images) to High(Images) do
    begin
      Images[I] := TPortableNetworkGraphic.Create;
      LoadAppIcon(AIcon, IconSizes[I], TPortableNetworkGraphic(Images[I]));
    end;
    Result := AImages.AddMultipleResolutions(Images);
    Binding := TAppIconBinding(AImages.FindComponent(BindingName));
    if not Assigned(Binding) then Binding := TAppIconBinding.Create(AImages);
    Binding.Bind(Result, AIcon);
  finally
    for I := Low(Images) to High(Images) do Images[I].Free;
  end;
end;

procedure PopulateAppImageList(AImages: TCustomImageList;
  ALogicalSize: Integer);
var
  Icon: TAppIcon;
begin
  AImages.BeginUpdate;
  try
    AImages.Clear;
    AImages.Width := ALogicalSize;
    AImages.Height := ALogicalSize;
    AImages.Scaled := True;
    AImages.RegisterResolutions(IconSizes);
    for Icon := Low(TAppIcon) to High(TAppIcon) do
      AddAppIcon(AImages, Icon);
  finally
    AImages.EndUpdate;
  end;
end;

finalization
  FreeAndNil(IconBindings);
end.
