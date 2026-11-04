unit status_visuals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ImgList, status_catalog;

const
  // Fourteen colored pixels remain at 100%, enough for all twelve states.
  StatusMarkerSize = 20;
  StatusMarkerResolutions: array[0..5] of Integer = (20, 25, 30, 40, 50, 60);

procedure ConfigureStatusImageList(Images: TCustomImageList);
function AddStatusSwatch(Images: TCustomImageList; Color: TColor): Integer;
function StatusAggregateImageKey(const Counts: TStatusCounts; Total: Integer;
  const Definitions: TStatusDefinitions): string;
function AddStatusAggregateImage(Images: TCustomImageList;
  const Counts: TStatusCounts; Total: Integer;
  const Definitions: TStatusDefinitions): Integer;

implementation

uses
  Math;

type
  TStatusBitmaps = array[0..High(StatusMarkerResolutions)] of TCustomBitmap;

const
  MarkerBorderColor = $00E1D5CB; // #CBD5E1

function MarkerPadding(Size: Integer): Integer;
begin
  Result := Max(1, Round(Size * 2 / StatusMarkerSize));
end;

function MarkerBorder(Size: Integer): Integer;
begin
  Result := Max(1, Round(Size / StatusMarkerSize));
end;

function MarkerInnerSize(Size: Integer): Integer;
begin
  Result := Size - 2 * (MarkerPadding(Size) + MarkerBorder(Size));
end;

function MaskColorFor(const Definitions: TStatusDefinitions): TColor;
var
  I: Integer;
  Used: Boolean;
begin
  // A user may choose any RGB color, including magenta or black. Never let
  // a color-key mask accidentally make a configured state transparent.
  Result := $00FF00FF;
  repeat
    Used := ColorToRGB(Result) = ColorToRGB(MarkerBorderColor);
    for I := 0 to High(Definitions) do
      Used := Used or (ColorToRGB(Definitions[I].Color) = ColorToRGB(Result));
    if not Used then
      Exit;
    Dec(Result);
  until False;
end;

function NewMarker(Size: Integer; MaskColor: TColor): TBitmap;
begin
  Result := TBitmap.Create;
  Result.SetSize(Size, Size);
  Result.Canvas.Brush.Color := MaskColor;
  Result.Canvas.FillRect(0, 0, Size, Size);
  Result.TransparentColor := MaskColor;
  Result.Transparent := True;
end;

procedure FreeBitmaps(var Bitmaps: TStatusBitmaps);
var
  I: Integer;
begin
  for I := Low(Bitmaps) to High(Bitmaps) do
    FreeAndNil(Bitmaps[I]);
end;

procedure ConfigureStatusImageList(Images: TCustomImageList);
begin
  Images.Width := StatusMarkerSize;
  Images.Height := StatusMarkerSize;
  Images.Scaled := True;
  Images.RegisterResolutions(StatusMarkerResolutions);
end;

function AddStatusSwatch(Images: TCustomImageList; Color: TColor): Integer;
var
  Bitmaps: TStatusBitmaps;
  Definitions: TStatusDefinitions;
  I, Size, Padding, Border: Integer;
  Bitmap: TBitmap;
  MaskColor: TColor;
begin
  Bitmaps := Default(TStatusBitmaps);
  SetLength(Definitions, 1);
  Definitions[0].Color := Color;
  MaskColor := MaskColorFor(Definitions);
  try
    for I := Low(Bitmaps) to High(Bitmaps) do
    begin
      Size := StatusMarkerResolutions[I];
      Padding := MarkerPadding(Size);
      Border := MarkerBorder(Size);
      Bitmap := NewMarker(Size, MaskColor);
      Bitmaps[I] := Bitmap;
      Bitmap.Canvas.Brush.Color := Color;
      Bitmap.Canvas.Pen.Color := MarkerBorderColor;
      Bitmap.Canvas.Pen.Width := Border;
      Bitmap.Canvas.RoundRect(Padding, Padding, Size - Padding,
        Size - Padding, Border * 4, Border * 4);
    end;
    Result := Images.AddMultipleResolutions(Bitmaps);
  finally
    FreeBitmaps(Bitmaps);
  end;
end;

function StatusAggregateImageKey(const Counts: TStatusCounts; Total: Integer;
  const Definitions: TStatusDefinitions): string;
var
  Segments: TStatusSegmentWidths;
  ResolutionIndex, SegmentIndex: Integer;
begin
  Result := '';
  // Two aggregates that round to the same 100% image may differ at 200%.
  // Include every native resolution in the key to preserve those ratios.
  for ResolutionIndex := Low(StatusMarkerResolutions) to High(StatusMarkerResolutions) do
  begin
    Segments := BuildStatusSegmentWidths(Counts, Definitions, Total,
      MarkerInnerSize(StatusMarkerResolutions[ResolutionIndex]));
    if Length(Segments) = 0 then
      Exit('');
    Result := Result + IntToStr(StatusMarkerResolutions[ResolutionIndex]) + '/';
    for SegmentIndex := 0 to High(Segments) do
      Result := Result + Format('%d:%d;', [Segments[SegmentIndex].StatusId,
        Segments[SegmentIndex].Width]);
  end;
end;

function AddStatusAggregateImage(Images: TCustomImageList;
  const Counts: TStatusCounts; Total: Integer;
  const Definitions: TStatusDefinitions): Integer;
var
  Bitmaps: TStatusBitmaps;
  Bitmap: TBitmap;
  Segments: TStatusSegmentWidths;
  I, SegmentIndex, Size, Padding, Border, X, Inset: Integer;
  MaskColor: TColor;
begin
  Result := -1;
  Bitmaps := Default(TStatusBitmaps);
  MaskColor := MaskColorFor(Definitions);
  try
    for I := Low(Bitmaps) to High(Bitmaps) do
    begin
      Size := StatusMarkerResolutions[I];
      Padding := MarkerPadding(Size);
      Border := MarkerBorder(Size);
      Inset := Padding + Border;
      Segments := BuildStatusSegmentWidths(Counts, Definitions, Total,
        MarkerInnerSize(Size));
      if Length(Segments) = 0 then
        Exit;
      Bitmap := NewMarker(Size, MaskColor);
      Bitmaps[I] := Bitmap;
      Bitmap.Canvas.Brush.Color := MarkerBorderColor;
      Bitmap.Canvas.FillRect(Padding, Padding, Size - Padding, Size - Padding);
      X := Inset;
      for SegmentIndex := 0 to High(Segments) do
      begin
        if Segments[SegmentIndex].Width <= 0 then
          Continue;
        Bitmap.Canvas.Brush.Color := GetStatusColor(Definitions,
          Segments[SegmentIndex].StatusId);
        Bitmap.Canvas.FillRect(X, Inset, X + Segments[SegmentIndex].Width,
          Size - Inset);
        Inc(X, Segments[SegmentIndex].Width);
      end;
    end;
    Result := Images.AddMultipleResolutions(Bitmaps);
  finally
    FreeBitmaps(Bitmaps);
  end;
end;

end.
