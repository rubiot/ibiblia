unit ui_identity_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, ImgList,
  IntfGraphics, FPImage, md5, fpcunit, testregistry, ui_icons, ui_style;

type
  TUIIdentityTests = class(TTestCase)
  published
    procedure EveryEmbeddedIconLoadsAtEveryDisplayResolution;
    procedure MenuToolbarAndStateListsKeepSymbolBindings;
    procedure FamilyIdentifiersRoundTripAndUnknownValuesFallBack;
    procedure BrandStaysFixedWhileCommandArtworkChanges;
    procedure RegisteredImageListsFollowFamilyWithoutReindexing;
    procedure DestroyedImageListsDoNotSurviveFamilyChanges;
    procedure ClearedAndRepopulatedListsKeepCurrentSymbols;
    procedure StylingPreservesSpecializedTextAndUserColors;
  end;

implementation

const
  DisplaySizes: array[0..6] of Integer = (16, 20, 24, 32, 40, 48, 64);

function PixelFingerprint(AImage: TRasterImage): string;
var
  Pixels: TLazIntfImage;
  Color: TFPColor;
  Context: TMD5Context;
  Digest: TMD5Digest;
  Width, Height, X, Y: Integer;
begin
  Pixels := AImage.CreateIntfImage;
  try
    MD5Init(Context);
    Width := Pixels.Width;
    Height := Pixels.Height;
    MD5Update(Context, Width, SizeOf(Width));
    MD5Update(Context, Height, SizeOf(Height));
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
      begin
        Color := Pixels.Colors[X, Y];
        if Color.Alpha = 0 then
        begin
          Color.Red := 0;
          Color.Green := 0;
          Color.Blue := 0;
        end;
        MD5Update(Context, Color, SizeOf(Color));
      end;
    MD5Final(Context, Digest);
    Result := MD5Print(Digest);
  finally
    Pixels.Free;
  end;
end;

function ListIconFingerprint(AImages: TCustomImageList; ASize,
  AIndex: Integer): string;
var
  Resolution: TCustomImageListResolution;
  Bitmap: TBitmap;
begin
  if not AImages.FindResolution(ASize, Resolution) then
    raise Exception.CreateFmt('Missing image list resolution: %d', [ASize]);
  Bitmap := TBitmap.Create;
  try
    Resolution.GetBitmap(AIndex, Bitmap);
    Result := PixelFingerprint(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

function EmbeddedIconFingerprint(AIcon: TAppIcon; ASize: Integer;
  AFamily: TAppIconFamily): string;
var
  Image: TPortableNetworkGraphic;
  Reference: TImageList;
begin
  Image := TPortableNetworkGraphic.Create;
  Reference := TImageList.Create(nil);
  try
    LoadAppIconForFamily(AIcon, ASize, Image, AFamily);
    Reference.Width := ASize;
    Reference.Height := ASize;
    Reference.Add(Image, nil);
    Result := ListIconFingerprint(Reference, ASize, 0);
  finally
    Reference.Free;
    Image.Free;
  end;
end;

procedure TUIIdentityTests.EveryEmbeddedIconLoadsAtEveryDisplayResolution;
var
  Icon: TAppIcon;
  Family: TAppIconFamily;
  Size: Integer;
  Context: string;
  Png: TPortableNetworkGraphic;
  AppIcon: TIcon;
begin
  Png := TPortableNetworkGraphic.Create;
  AppIcon := TIcon.Create;
  try
    for Family := Low(TAppIconFamily) to High(TAppIconFamily) do
      for Icon := Low(TAppIcon) to High(TAppIcon) do
        for Size in DisplaySizes do
        begin
          Context := AppIconFamilyId(Family) + '/' + AppIconSymbol(Icon);
          LoadAppIconForFamily(Icon, Size, Png, Family);
          AssertEquals(Context + ' width', Size, Png.Width);
          AssertEquals(Context + ' height', Size, Png.Height);
          AssertFalse(Context + ' must contain artwork', Png.Empty);
        end;
    LoadApplicationIcon(AppIcon);
    AssertFalse('Application icon must be embedded', AppIcon.Empty);
  finally
    AppIcon.Free;
    Png.Free;
  end;
end;

procedure TUIIdentityTests.MenuToolbarAndStateListsKeepSymbolBindings;
var
  Toolbar, Menu, States: TImageList;
  Icon: TAppIcon;
  Width: Integer;
  Resolution: TCustomImageListResolution;
begin
  Toolbar := TImageList.Create(nil);
  Menu := TImageList.Create(nil);
  States := TImageList.Create(nil);
  try
    PopulateAppImageList(Toolbar, 32);
    PopulateAppImageList(Menu, 16);
    AssertEquals(Ord(High(TAppIcon)) + 1, Toolbar.Count);
    AssertEquals(Toolbar.Count, Menu.Count);
    for Icon := Low(TAppIcon) to High(TAppIcon) do
      AssertTrue('Shared symbol must exist in both lists',
        AppIconIndex(Icon) < Menu.Count);
    AssertTrue('History and verse arrows must differ',
      AppIconIndex(aiHistoryBack) <> AppIconIndex(aiVersePrevious));
    for Width in [16, 24, 32, 48, 64] do
    begin
      AssertTrue('Pre-rendered resolution ' + IntToStr(Width),
        Toolbar.FindResolution(Width, Resolution));
      AssertEquals(Toolbar.Count, Resolution.Count);
    end;
    AssertEquals(0, AddAppIcon(States, aiCheckboxUnchecked));
    AssertEquals(1, AddAppIcon(States, aiCheckboxChecked));
    AssertEquals(2, AddAppIcon(States, aiAiError));
    AssertEquals(3, States.Count);
  finally
    States.Free;
    Menu.Free;
    Toolbar.Free;
  end;
end;

procedure TUIIdentityTests.FamilyIdentifiersRoundTripAndUnknownValuesFallBack;
var
  Family, OtherFamily: TAppIconFamily;
begin
  AssertEquals('Four icon families are available', 4,
    Ord(High(TAppIconFamily)) - Ord(Low(TAppIconFamily)) + 1);
  for Family := Low(TAppIconFamily) to High(TAppIconFamily) do
  begin
    AssertTrue('Each family has a persistent identifier',
      AppIconFamilyId(Family) <> '');
    AssertTrue('Each family has a display name', AppIconFamilyName(Family) <> '');
    AssertEquals('Family identifier round trip', Ord(Family),
      Ord(AppIconFamilyFromId(AppIconFamilyId(Family))));
    for OtherFamily := Low(TAppIconFamily) to High(TAppIconFamily) do
      if OtherFamily <> Family then
        AssertTrue('Family identifiers are unique',
          AppIconFamilyId(Family) <> AppIconFamilyId(OtherFamily));
  end;
  AssertEquals('Missing preference uses the original family', Ord(aifConnected),
    Ord(AppIconFamilyFromId('')));
  AssertEquals('Unknown preference uses the original family', Ord(aifConnected),
    Ord(AppIconFamilyFromId('unavailable-family')));
end;

procedure TUIIdentityTests.BrandStaysFixedWhileCommandArtworkChanges;
const
  BrandSizes: array[0..8] of Integer = (16, 20, 24, 32, 40, 48, 64, 128, 256);
var
  SavedFamily, Family, OtherFamily: TAppIconFamily;
  Original, Selected: TPortableNetworkGraphic;
  ApplicationBrand: TIcon;
  Size: Integer;
  OriginalApplicationPixels: string;
  CommandPixels: array[TAppIconFamily] of string;
begin
  SavedFamily := GetAppIconFamily;
  Original := TPortableNetworkGraphic.Create;
  Selected := TPortableNetworkGraphic.Create;
  ApplicationBrand := TIcon.Create;
  try
    SetAppIconFamily(aifConnected);
    LoadApplicationIcon(ApplicationBrand);
    OriginalApplicationPixels := PixelFingerprint(ApplicationBrand);
    for Family := Low(TAppIconFamily) to High(TAppIconFamily) do
    begin
      SetAppIconFamily(Family);
      for Size in BrandSizes do
      begin
        LoadAppIconForFamily(aiAppIBiblia, Size, Original, aifConnected);
        LoadAppIconForFamily(aiAppIBiblia, Size, Selected, Family);
        AssertEquals('Explicit family preserves the application brand',
          PixelFingerprint(Original), PixelFingerprint(Selected));
        LoadAppIcon(aiAppIBiblia, Size, Selected);
        AssertEquals('Current family preserves the application brand',
          PixelFingerprint(Original), PixelFingerprint(Selected));
      end;
      LoadApplicationIcon(ApplicationBrand);
      AssertEquals('Window and application ICO remain the same',
        OriginalApplicationPixels, PixelFingerprint(ApplicationBrand));
      for Size in DisplaySizes do
      begin
        LoadAppIconForFamily(aiProjectOpen, Size, Original, Family);
        LoadAppIcon(aiProjectOpen, Size, Selected);
        AssertEquals('Current family is used for command artwork',
          PixelFingerprint(Original), PixelFingerprint(Selected));
      end;
      CommandPixels[Family] := PixelFingerprint(Selected);
    end;
    for Family := Low(TAppIconFamily) to High(TAppIconFamily) do
      for OtherFamily := Low(TAppIconFamily) to High(TAppIconFamily) do
        if Family <> OtherFamily then
          AssertTrue('Different families have different command pixels',
            CommandPixels[Family] <> CommandPixels[OtherFamily]);
  finally
    SetAppIconFamily(SavedFamily);
    ApplicationBrand.Free;
    Selected.Free;
    Original.Free;
  end;
end;

procedure TUIIdentityTests.RegisteredImageListsFollowFamilyWithoutReindexing;
const
  StateIcons: array[0..2] of TAppIcon =
    (aiCheckboxChecked, aiAiError, aiAiPending);
var
  SavedFamily, Family: TAppIconFamily;
  Toolbar, Menu, States: TImageList;
  Resolution: TCustomImageListResolution;
  Icon: TAppIcon;
  Size, Index: Integer;
  Context, Expected: string;
begin
  SavedFamily := GetAppIconFamily;
  Toolbar := TImageList.Create(nil);
  Menu := TImageList.Create(nil);
  States := TImageList.Create(nil);
  try
    SetAppIconFamily(aifConnected);
    PopulateAppImageList(Toolbar, 32);
    PopulateAppImageList(Menu, 16);
    States.Width := 20;
    States.Height := 20;
    for Index := Low(StateIcons) to High(StateIcons) do
      AssertEquals(Index, AddAppIcon(States, StateIcons[Index]));
    for Family := Low(TAppIconFamily) to High(TAppIconFamily) do
    begin
      SetAppIconFamily(Family);
      AssertEquals('Toolbar logical size stays unchanged', 32, Toolbar.Width);
      AssertEquals('Menu logical size stays unchanged', 16, Menu.Width);
      AssertEquals('Subset logical size stays unchanged', 20, States.Width);
      for Size in DisplaySizes do
      begin
        AssertTrue(Toolbar.FindResolution(Size, Resolution));
        AssertEquals('Toolbar count stays unchanged', Ord(High(TAppIcon)) + 1,
          Resolution.Count);
        AssertTrue(Menu.FindResolution(Size, Resolution));
        AssertEquals('Menu count stays unchanged', Toolbar.Count, Resolution.Count);
        AssertTrue(States.FindResolution(Size, Resolution));
        AssertEquals('Subset count stays unchanged', Length(StateIcons),
          Resolution.Count);
        for Icon := Low(TAppIcon) to High(TAppIcon) do
        begin
          Context := AppIconFamilyId(Family) + '/' + AppIconSymbol(Icon) +
            '/' + IntToStr(Size);
          Expected := EmbeddedIconFingerprint(Icon, Size, Family);
          AssertEquals('Live toolbar ' + Context, Expected,
            ListIconFingerprint(Toolbar, Size, AppIconIndex(Icon)));
          AssertEquals('Live menu ' + Context, Expected,
            ListIconFingerprint(Menu, Size, AppIconIndex(Icon)));
        end;
        for Index := Low(StateIcons) to High(StateIcons) do
          AssertEquals('Live subset ' + AppIconFamilyId(Family) + '/' +
            AppIconSymbol(StateIcons[Index]) + '/' + IntToStr(Size),
            EmbeddedIconFingerprint(StateIcons[Index], Size, Family),
            ListIconFingerprint(States, Size, Index));
      end;
    end;
  finally
    States.Free;
    Menu.Free;
    Toolbar.Free;
    SetAppIconFamily(SavedFamily);
  end;
end;

procedure TUIIdentityTests.DestroyedImageListsDoNotSurviveFamilyChanges;
var
  SavedFamily, Family: TAppIconFamily;
  Owner: TComponent;
  Transient, Surviving: TImageList;
begin
  SavedFamily := GetAppIconFamily;
  Surviving := TImageList.Create(nil);
  try
    AddAppIcon(Surviving, aiAiSuccess);
    for Family := Low(TAppIconFamily) to High(TAppIconFamily) do
    begin
      Owner := TComponent.Create(nil);
      try
        Transient := TImageList.Create(Owner);
        PopulateAppImageList(Transient, 24);
      finally
        Owner.Free;
      end;
      SetAppIconFamily(Family);
      AssertEquals('Surviving list still updates after another owner is freed',
        EmbeddedIconFingerprint(aiAiSuccess, 24, Family),
        ListIconFingerprint(Surviving, 24, 0));
    end;
  finally
    Surviving.Free;
    SetAppIconFamily(SavedFamily);
  end;
end;

procedure TUIIdentityTests.ClearedAndRepopulatedListsKeepCurrentSymbols;
var
  SavedFamily: TAppIconFamily;
  Images: TImageList;
  Size: Integer;
begin
  SavedFamily := GetAppIconFamily;
  Images := TImageList.Create(nil);
  try
    SetAppIconFamily(aifConnected);
    AddAppIcon(Images, aiAiError);
    AddAppIcon(Images, aiAiPending);
    Images.Clear;
    AssertEquals(0, AddAppIcon(Images, aiProjectOpen));
    AssertEquals(1, AddAppIcon(Images, aiComments));
    SetAppIconFamily(aifMosaico);
    AssertEquals('Reused subset keeps its new count', 2, Images.Count);
    for Size in DisplaySizes do
    begin
      AssertEquals('First reused index follows its new symbol',
        EmbeddedIconFingerprint(aiProjectOpen, Size, aifMosaico),
        ListIconFingerprint(Images, Size, 0));
      AssertEquals('Second reused index follows its new symbol',
        EmbeddedIconFingerprint(aiComments, Size, aifMosaico),
        ListIconFingerprint(Images, Size, 1));
    end;
    PopulateAppImageList(Images, 16);
    SetAppIconFamily(aifFolio);
    AssertEquals('Repopulated full list keeps every symbol',
      Ord(High(TAppIcon)) + 1, Images.Count);
    for Size in DisplaySizes do
      AssertEquals('Former subset index now uses the full catalog symbol',
        EmbeddedIconFingerprint(aiProjectOpen, Size, aifFolio),
        ListIconFingerprint(Images, Size, AppIconIndex(aiProjectOpen)));
    Images.Clear;
    SetAppIconFamily(aifClareza);
    AssertEquals('Switching families does not restore cleared entries', 0,
      Images.Count);
    AssertEquals(0, AddAppIcon(Images, aiAiSuccess));
    SetAppIconFamily(aifConnected);
    AssertEquals('Reused full list now keeps only its new subset', 1, Images.Count);
    for Size in DisplaySizes do
      AssertEquals('Final reused symbol follows the selected family',
        EmbeddedIconFingerprint(aiAiSuccess, Size, aifConnected),
        ListIconFingerprint(Images, Size, 0));
  finally
    Images.Free;
    SetAppIconFamily(SavedFamily);
  end;
end;

procedure TUIIdentityTests.StylingPreservesSpecializedTextAndUserColors;
var
  Host: TForm;
  RawText: TMemo;
  ContentPanel: TPanel;
  StatusLabel: TLabel;
begin
  Host := TForm.CreateNew(nil);
  try
    ContentPanel := TPanel.Create(Host);
    ContentPanel.Parent := Host;
    ContentPanel.Color := RGBToColor(83, 127, 191);
    ContentPanel.ParentColor := False;
    RawText := TMemo.Create(Host);
    RawText.Parent := ContentPanel;
    RawText.Font.Name := 'Consolas';
    RawText.Font.Size := 17;
    RawText.Text := 'source <WG1> / destination';
    StatusLabel := TLabel.Create(Host);
    StatusLabel.Parent := Host;
    StatusLabel.Font.Color := RGBToColor(35, 121, 87);
    ApplyDialogStyle(Host);
    AssertEquals('Segoe UI', Host.Font.Name);
    AssertEquals(10, Host.Font.Size);
    AssertEquals('Consolas', RawText.Font.Name);
    AssertEquals(17, RawText.Font.Size);
    AssertEquals('source <WG1> / destination', Trim(RawText.Text));
    AssertEquals(LongInt(RGBToColor(83, 127, 191)), LongInt(ContentPanel.Color));
    AssertEquals(LongInt(RGBToColor(35, 121, 87)), LongInt(StatusLabel.Font.Color));
  finally
    Host.Free;
  end;
end;

initialization
  RegisterTest(TUIIdentityTests);
end.
