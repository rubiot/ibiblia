unit status_catalog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

const
  STATUS_CATALOG_CONFIG_KEY = 'status.catalog';
  STATUS_CATALOG_MAX_COUNT = 12;

type
  TStatusDefinition = record
    Id: Integer;
    Caption: string;
    Color: TColor;
    DisplayOrder: Integer;
    PriorityOrder: Integer;
  end;

  TStatusDefinitionComparer = function(const Left, Right: TStatusDefinition): Integer;

  TStatusDefinitions = array of TStatusDefinition;

  TStatusCount = record
    StatusId: Integer;
    Count: Integer;
  end;

  TStatusCounts = array of TStatusCount;

  TStatusRemap = record
    OldStatusId: Integer;
    NewStatusId: Integer;
  end;

  TStatusRemaps = array of TStatusRemap;

  TStatusSegmentWidth = record
    StatusId: Integer;
    Width: Integer;
  end;

  TStatusSegmentWidths = array of TStatusSegmentWidth;

function BuildLegacyStatusDefinitions(const Captions: array of string): TStatusDefinitions;
function LoadStatusDefinitions(const SerializedCatalog: string;
  const LegacyCaptions: array of string): TStatusDefinitions;
function SaveStatusDefinitions(const Definitions: TStatusDefinitions): string;
function ValidateStatusDefinitions(const Definitions: TStatusDefinitions;
  out ErrorMessage: string): Boolean;
procedure NormalizeStatusDefinitions(var Definitions: TStatusDefinitions;
  RenumberIds: Boolean);
function SortStatusDefinitionsByDisplayOrder(const Definitions: TStatusDefinitions): TStatusDefinitions;
function SortStatusDefinitionsByPriorityOrder(const Definitions: TStatusDefinitions): TStatusDefinitions;
function IndexOfStatusId(const Definitions: TStatusDefinitions; StatusId: Integer): Integer;
function IsValidStatusId(const Definitions: TStatusDefinitions; StatusId: Integer): Boolean;
function FirstStatusId(const Definitions: TStatusDefinitions): Integer;
function MaxStatusId(const Definitions: TStatusDefinitions): Integer;
function GetStatusCaption(const Definitions: TStatusDefinitions; StatusId: Integer): string;
function GetStatusColor(const Definitions: TStatusDefinitions; StatusId: Integer): TColor;
function StatusIdAtDisplayIndex(const Definitions: TStatusDefinitions; DisplayIndex: Integer): Integer;
function DisplayIndexOfStatusId(const Definitions: TStatusDefinitions; StatusId: Integer): Integer;
function StatusColorToString(Color: TColor): string;
function StringToStatusColor(const Value: string; DefaultColor: TColor): TColor;
function BuildStatusSegmentWidths(const Counts: TStatusCounts;
  const Definitions: TStatusDefinitions; Total, PixelWidth: Integer): TStatusSegmentWidths;

implementation

uses
  fpjson, jsonparser;

const
  LEGACY_STATUS_COLORS: array[0..3] of TColor = (
    clRed,
    clYellow,
    $000080FF, // orange
    clGreen
  );

function StatusColorToString(Color: TColor): string;
var
  RGB: LongInt;
  R, G, B: Integer;
begin
  RGB := ColorToRGB(Color);
  R := RGB and $FF;
  G := (RGB shr 8) and $FF;
  B := (RGB shr 16) and $FF;
  Result := UpperCase(Format('#%.2x%.2x%.2x', [R, G, B]));
end;

function StringToStatusColor(const Value: string; DefaultColor: TColor): TColor;
var
  Hex: string;
  R, G, B: Integer;
begin
  Result := DefaultColor;
  Hex := Trim(Value);
  if (Length(Hex) = 7) and (Hex[1] = '#') then
  begin
    R := StrToIntDef('$' + Copy(Hex, 2, 2), -1);
    G := StrToIntDef('$' + Copy(Hex, 4, 2), -1);
    B := StrToIntDef('$' + Copy(Hex, 6, 2), -1);
    if (R >= 0) and (G >= 0) and (B >= 0) then
      Result := RGBToColor(R, G, B);
  end;
end;

function StatusCountValue(const Counts: TStatusCounts; StatusId: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(Counts) do
    if Counts[I].StatusId = StatusId then
      Exit(Counts[I].Count);
end;

function SegmentIndexOfStatusId(const Segments: TStatusSegmentWidths;
  StatusId: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Segments) do
    if Segments[I].StatusId = StatusId then
      Exit(I);
end;

procedure AddStatusSegment(var Segments: TStatusSegmentWidths; StatusId,
  Width: Integer);
begin
  SetLength(Segments, Length(Segments) + 1);
  Segments[High(Segments)].StatusId := StatusId;
  Segments[High(Segments)].Width := Width;
end;

function BuildStatusSegmentWidths(const Counts: TStatusCounts;
  const Definitions: TStatusDefinitions; Total, PixelWidth: Integer): TStatusSegmentWidths;
var
  Sorted: TStatusDefinitions;
  CountedTotal: Integer;
  I: Integer;
  Count: Integer;
  SegmentCount: Integer;
  UsedWidth: Integer;
  BestIndex: Integer;
  Remainders: array of Int64;
  SegmentCounts: array of Integer;
  Numerator: Int64;
begin
  Result := nil;
  Remainders := nil;
  SegmentCounts := nil;
  if PixelWidth <= 0 then
    Exit;

  CountedTotal := 0;
  for I := 0 to High(Counts) do
    if Counts[I].Count > 0 then
      Inc(CountedTotal, Counts[I].Count);

  if Total <= 0 then
    Total := CountedTotal;
  if Total <= 0 then
    Exit;

  Sorted := SortStatusDefinitionsByDisplayOrder(Definitions);
  for I := 0 to High(Sorted) do
  begin
    Count := StatusCountValue(Counts, Sorted[I].Id);
    if Count > 0 then
      AddStatusSegment(Result, Sorted[I].Id, Count);
  end;

  for I := 0 to High(Counts) do
    if (Counts[I].Count > 0) and
      (SegmentIndexOfStatusId(Result, Counts[I].StatusId) < 0) then
      AddStatusSegment(Result, Counts[I].StatusId, Counts[I].Count);

  if Length(Result) = 0 then
    Exit;

  SegmentCount := Length(Result);
  if SegmentCount > PixelWidth then
  begin
    for I := 0 to High(Result) do
      Result[I].Width := 0;
    for I := 0 to PixelWidth - 1 do
      Result[I].Width := 1;
    Exit;
  end;

  SetLength(Remainders, SegmentCount);
  SetLength(SegmentCounts, SegmentCount);
  UsedWidth := 0;
  for I := 0 to High(Result) do
  begin
    Count := Result[I].Width;
    SegmentCounts[I] := Count;
    Numerator := Int64(Count) * PixelWidth;
    Result[I].Width := Numerator div Total;
    Remainders[I] := Numerator mod Total;
    if Result[I].Width < 1 then
      Result[I].Width := 1;
    Inc(UsedWidth, Result[I].Width);
  end;

  while UsedWidth < PixelWidth do
  begin
    BestIndex := 0;
    for I := 1 to High(Remainders) do
      if Remainders[I] > Remainders[BestIndex] then
        BestIndex := I;

    Inc(Result[BestIndex].Width);
    Remainders[BestIndex] := -1;
    Inc(UsedWidth);
  end;

  while UsedWidth > PixelWidth do
  begin
    BestIndex := -1;
    for I := 0 to High(Result) do
      if Result[I].Width > 1 then
      begin
        if (BestIndex < 0) or (Remainders[I] < Remainders[BestIndex]) or
          ((Remainders[I] = Remainders[BestIndex]) and
          (SegmentCounts[I] > SegmentCounts[BestIndex])) then
          BestIndex := I;
      end;

    if BestIndex < 0 then
      Break;

    Dec(Result[BestIndex].Width);
    Remainders[BestIndex] := High(Int64);
    Dec(UsedWidth);
  end;
end;

function BuildLegacyStatusDefinitions(const Captions: array of string): TStatusDefinitions;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, 4);
  for I := 0 to 3 do
  begin
    Result[I].Id := I;
    if (I >= Low(Captions)) and (I <= High(Captions)) and
      (Trim(Captions[I]) <> '') then
      Result[I].Caption := Captions[I]
    else
      Result[I].Caption := Format('Status %d', [I + 1]);
    Result[I].Color := LEGACY_STATUS_COLORS[I];
    Result[I].DisplayOrder := I;
    Result[I].PriorityOrder := I;
  end;
end;

function CompareByDisplayOrder(const Left, Right: TStatusDefinition): Integer;
begin
  Result := Left.DisplayOrder - Right.DisplayOrder;
  if Result = 0 then
    Result := Left.Id - Right.Id;
end;

function CompareByPriorityOrder(const Left, Right: TStatusDefinition): Integer;
begin
  Result := Left.PriorityOrder - Right.PriorityOrder;
  if Result = 0 then
    Result := Left.DisplayOrder - Right.DisplayOrder;
  if Result = 0 then
    Result := Left.Id - Right.Id;
end;

procedure SortDefinitions(var Definitions: TStatusDefinitions;
  Compare: TStatusDefinitionComparer);
var
  I, J: Integer;
  Tmp: TStatusDefinition;
begin
  for I := Low(Definitions) to High(Definitions) - 1 do
    for J := I + 1 to High(Definitions) do
      if Compare(Definitions[I], Definitions[J]) > 0 then
      begin
        Tmp := Definitions[I];
        Definitions[I] := Definitions[J];
        Definitions[J] := Tmp;
      end;
end;

function SortStatusDefinitionsByDisplayOrder(
  const Definitions: TStatusDefinitions): TStatusDefinitions;
begin
  Result := Copy(Definitions, 0, Length(Definitions));
  SortDefinitions(Result, @CompareByDisplayOrder);
end;

function SortStatusDefinitionsByPriorityOrder(
  const Definitions: TStatusDefinitions): TStatusDefinitions;
begin
  Result := Copy(Definitions, 0, Length(Definitions));
  SortDefinitions(Result, @CompareByPriorityOrder);
end;

procedure NormalizeStatusDefinitions(var Definitions: TStatusDefinitions;
  RenumberIds: Boolean);
var
  I: Integer;
  ByPriority: TStatusDefinitions;
begin
  Definitions := SortStatusDefinitionsByDisplayOrder(Definitions);
  for I := 0 to High(Definitions) do
  begin
    if RenumberIds then
      Definitions[I].Id := I;
    Definitions[I].DisplayOrder := I;
  end;

  ByPriority := SortStatusDefinitionsByPriorityOrder(Definitions);
  for I := 0 to High(ByPriority) do
    Definitions[IndexOfStatusId(Definitions, ByPriority[I].Id)].PriorityOrder := I;
end;

function LoadStatusDefinitions(const SerializedCatalog: string;
  const LegacyCaptions: array of string): TStatusDefinitions;
var
  Parser: TJSONParser;
  Data: TJSONData;
  Arr: TJSONArray;
  Obj: TJSONObject;
  I: Integer;
  ErrorMessage: string;
begin
  if Trim(SerializedCatalog) = '' then
    Exit(BuildLegacyStatusDefinitions(LegacyCaptions));

  Result := nil;
  try
    Parser := TJSONParser.Create(SerializedCatalog, []);
    try
      Data := Parser.Parse;
      try
        if not (Data is TJSONArray) then
          Exit(BuildLegacyStatusDefinitions(LegacyCaptions));

        Arr := TJSONArray(Data);
        SetLength(Result, Arr.Count);
        for I := 0 to Arr.Count - 1 do
        begin
          if not (Arr.Items[I] is TJSONObject) then
            Continue;

          Obj := TJSONObject(Arr.Items[I]);
          Result[I].Id := Obj.Get('id', I);
          Result[I].Caption := Obj.Get('caption', Format('Status %d', [I + 1]));
          Result[I].Color := StringToStatusColor(Obj.Get('color', ''),
            LEGACY_STATUS_COLORS[I mod Length(LEGACY_STATUS_COLORS)]);
          Result[I].DisplayOrder := Obj.Get('display_order', I);
          Result[I].PriorityOrder := Obj.Get('priority_order', I);
        end;
      finally
        Data.Free;
      end;
    finally
      Parser.Free;
    end;
  except
    Exit(BuildLegacyStatusDefinitions(LegacyCaptions));
  end;

  if not ValidateStatusDefinitions(Result, ErrorMessage) then
    Result := BuildLegacyStatusDefinitions(LegacyCaptions)
  else
    NormalizeStatusDefinitions(Result, False);
end;

function SaveStatusDefinitions(const Definitions: TStatusDefinitions): string;
var
  Arr: TJSONArray;
  Obj: TJSONObject;
  Sorted: TStatusDefinitions;
  I: Integer;
begin
  Sorted := SortStatusDefinitionsByDisplayOrder(Definitions);
  Arr := TJSONArray.Create;
  try
    for I := 0 to High(Sorted) do
    begin
      Obj := TJSONObject.Create;
      Obj.Add('id', Sorted[I].Id);
      Obj.Add('caption', Sorted[I].Caption);
      Obj.Add('color', StatusColorToString(Sorted[I].Color));
      Obj.Add('display_order', Sorted[I].DisplayOrder);
      Obj.Add('priority_order', Sorted[I].PriorityOrder);
      Arr.Add(Obj);
    end;
    Result := Arr.AsJSON;
  finally
    Arr.Free;
  end;
end;

function ValidateStatusDefinitions(const Definitions: TStatusDefinitions;
  out ErrorMessage: string): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  ErrorMessage := '';

  if Length(Definitions) < 1 then
  begin
    ErrorMessage := 'At least one status is required.';
    Exit;
  end;

  if Length(Definitions) > STATUS_CATALOG_MAX_COUNT then
  begin
    ErrorMessage := Format('At most %d statuses are allowed.',
      [STATUS_CATALOG_MAX_COUNT]);
    Exit;
  end;

  for I := 0 to High(Definitions) do
  begin
    if Definitions[I].Id < 0 then
    begin
      ErrorMessage := 'Status IDs must be non-negative.';
      Exit;
    end;

    if Trim(Definitions[I].Caption) = '' then
    begin
      ErrorMessage := 'Status names cannot be empty.';
      Exit;
    end;

    for J := I + 1 to High(Definitions) do
    begin
      if Definitions[I].Id = Definitions[J].Id then
      begin
        ErrorMessage := 'Status IDs must be unique.';
        Exit;
      end;

      if SameText(Trim(Definitions[I].Caption), Trim(Definitions[J].Caption)) then
      begin
        ErrorMessage := 'Status names must be unique.';
        Exit;
      end;
    end;
  end;

  Result := True;
end;

function IndexOfStatusId(const Definitions: TStatusDefinitions;
  StatusId: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Definitions) do
    if Definitions[I].Id = StatusId then
      Exit(I);
end;

function IsValidStatusId(const Definitions: TStatusDefinitions;
  StatusId: Integer): Boolean;
begin
  Result := IndexOfStatusId(Definitions, StatusId) >= 0;
end;

function FirstStatusId(const Definitions: TStatusDefinitions): Integer;
var
  Sorted: TStatusDefinitions;
begin
  Sorted := SortStatusDefinitionsByDisplayOrder(Definitions);
  if Length(Sorted) > 0 then
    Result := Sorted[0].Id
  else
    Result := 0;
end;

function MaxStatusId(const Definitions: TStatusDefinitions): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(Definitions) do
    if Definitions[I].Id > Result then
      Result := Definitions[I].Id;
end;

function GetStatusCaption(const Definitions: TStatusDefinitions;
  StatusId: Integer): string;
var
  Index: Integer;
begin
  Index := IndexOfStatusId(Definitions, StatusId);
  if Index >= 0 then
    Result := Definitions[Index].Caption
  else
    Result := Format('Status %d', [StatusId + 1]);
end;

function GetStatusColor(const Definitions: TStatusDefinitions;
  StatusId: Integer): TColor;
var
  Index: Integer;
begin
  Index := IndexOfStatusId(Definitions, StatusId);
  if Index >= 0 then
    Result := Definitions[Index].Color
  else
    Result := clGray;
end;

function StatusIdAtDisplayIndex(const Definitions: TStatusDefinitions;
  DisplayIndex: Integer): Integer;
var
  Sorted: TStatusDefinitions;
begin
  Sorted := SortStatusDefinitionsByDisplayOrder(Definitions);
  if (DisplayIndex >= 0) and (DisplayIndex <= High(Sorted)) then
    Result := Sorted[DisplayIndex].Id
  else
    Result := FirstStatusId(Definitions);
end;

function DisplayIndexOfStatusId(const Definitions: TStatusDefinitions;
  StatusId: Integer): Integer;
var
  Sorted: TStatusDefinitions;
  I: Integer;
begin
  Result := -1;
  Sorted := SortStatusDefinitionsByDisplayOrder(Definitions);
  for I := 0 to High(Sorted) do
    if Sorted[I].Id = StatusId then
      Exit(I);
end;

end.
