unit statuscatalogtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpcunit, testregistry, status_catalog;

type
  TStatusCatalogTests = class(TTestCase)
  published
    procedure LegacyFallbackUsesDefaultIdsAndCaptions;
    procedure JsonRoundTripPreservesStableIdsAndColors;
    procedure ValidationRejectsInvalidCatalogs;
    procedure NormalizeCanRenumberNewProjectCatalog;
    procedure PrioritySortUsesPriorityThenDisplayOrder;
    procedure SegmentWidthsSupportAggregateBars;
  end;

implementation

procedure TStatusCatalogTests.LegacyFallbackUsesDefaultIdsAndCaptions;
var
  Definitions: TStatusDefinitions;
begin
  Definitions := LoadStatusDefinitions('', ['Zero', 'One', 'Two', 'Three']);

  AssertEquals(4, Length(Definitions));
  AssertEquals(0, Definitions[0].Id);
  AssertEquals('Zero', Definitions[0].Caption);
  AssertEquals(3, Definitions[3].Id);
  AssertEquals('Three', Definitions[3].Caption);
  AssertEquals(0, FirstStatusId(Definitions));
end;

procedure TStatusCatalogTests.JsonRoundTripPreservesStableIdsAndColors;
var
  Definitions: TStatusDefinitions;
  Loaded: TStatusDefinitions;
  Serialized: string;
begin
  SetLength(Definitions, 3);
  Definitions[0].Id := 7;
  Definitions[0].Caption := 'Review';
  Definitions[0].Color := RGBToColor(10, 20, 30);
  Definitions[0].DisplayOrder := 2;
  Definitions[0].PriorityOrder := 1;

  Definitions[1].Id := 2;
  Definitions[1].Caption := 'Draft';
  Definitions[1].Color := RGBToColor(40, 50, 60);
  Definitions[1].DisplayOrder := 0;
  Definitions[1].PriorityOrder := 2;

  Definitions[2].Id := 4;
  Definitions[2].Caption := 'Done';
  Definitions[2].Color := RGBToColor(70, 80, 90);
  Definitions[2].DisplayOrder := 1;
  Definitions[2].PriorityOrder := 0;

  Serialized := SaveStatusDefinitions(Definitions);
  Loaded := LoadStatusDefinitions(Serialized, ['A', 'B', 'C', 'D']);

  AssertEquals(3, Length(Loaded));
  AssertEquals(2, Loaded[0].Id);
  AssertEquals('Draft', Loaded[0].Caption);
  AssertEquals(ColorToRGB(RGBToColor(40, 50, 60)), ColorToRGB(Loaded[0].Color));
  AssertEquals(4, Loaded[1].Id);
  AssertEquals(7, Loaded[2].Id);
  AssertEquals('Review', GetStatusCaption(Loaded, 7));
end;

procedure TStatusCatalogTests.ValidationRejectsInvalidCatalogs;
var
  Definitions: TStatusDefinitions;
  ErrorMessage: string;
  I: Integer;
begin
  SetLength(Definitions, 0);
  AssertFalse(ValidateStatusDefinitions(Definitions, ErrorMessage));

  SetLength(Definitions, 2);
  Definitions[0].Id := 0;
  Definitions[0].Caption := 'Same';
  Definitions[1].Id := 0;
  Definitions[1].Caption := 'Other';
  AssertFalse(ValidateStatusDefinitions(Definitions, ErrorMessage));

  Definitions[1].Id := 1;
  Definitions[1].Caption := 'same';
  AssertFalse(ValidateStatusDefinitions(Definitions, ErrorMessage));

  SetLength(Definitions, STATUS_CATALOG_MAX_COUNT + 1);
  for I := 0 to High(Definitions) do
  begin
    Definitions[I].Id := I;
    Definitions[I].Caption := Format('Status %d', [I]);
  end;
  AssertFalse(ValidateStatusDefinitions(Definitions, ErrorMessage));
end;

procedure TStatusCatalogTests.NormalizeCanRenumberNewProjectCatalog;
var
  Definitions: TStatusDefinitions;
begin
  SetLength(Definitions, 2);
  Definitions[0].Id := 42;
  Definitions[0].Caption := 'Second';
  Definitions[0].DisplayOrder := 1;
  Definitions[0].PriorityOrder := 0;
  Definitions[1].Id := 99;
  Definitions[1].Caption := 'First';
  Definitions[1].DisplayOrder := 0;
  Definitions[1].PriorityOrder := 1;

  NormalizeStatusDefinitions(Definitions, True);

  AssertEquals('First', Definitions[0].Caption);
  AssertEquals(0, Definitions[0].Id);
  AssertEquals(0, Definitions[0].DisplayOrder);
  AssertEquals('Second', Definitions[1].Caption);
  AssertEquals(1, Definitions[1].Id);
end;

procedure TStatusCatalogTests.PrioritySortUsesPriorityThenDisplayOrder;
var
  Definitions: TStatusDefinitions;
  Sorted: TStatusDefinitions;
begin
  SetLength(Definitions, 3);
  Definitions[0].Id := 0;
  Definitions[0].Caption := 'Low';
  Definitions[0].DisplayOrder := 0;
  Definitions[0].PriorityOrder := 2;
  Definitions[1].Id := 1;
  Definitions[1].Caption := 'High';
  Definitions[1].DisplayOrder := 1;
  Definitions[1].PriorityOrder := 0;
  Definitions[2].Id := 2;
  Definitions[2].Caption := 'Middle';
  Definitions[2].DisplayOrder := 2;
  Definitions[2].PriorityOrder := 1;

  Sorted := SortStatusDefinitionsByPriorityOrder(Definitions);

  AssertEquals(1, Sorted[0].Id);
  AssertEquals(2, Sorted[1].Id);
  AssertEquals(0, Sorted[2].Id);
end;

procedure TStatusCatalogTests.SegmentWidthsSupportAggregateBars;

  function SumWidths(const Segments: TStatusSegmentWidths): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to High(Segments) do
      Inc(Result, Segments[I].Width);
  end;

var
  Definitions: TStatusDefinitions;
  Counts: TStatusCounts;
  Segments: TStatusSegmentWidths;
  I: Integer;
begin
  SetLength(Definitions, 4);
  for I := 0 to High(Definitions) do
  begin
    Definitions[I].Id := (I + 1) * 10;
    Definitions[I].Caption := Format('Status %d', [I + 1]);
    Definitions[I].DisplayOrder := I;
    Definitions[I].PriorityOrder := I;
  end;

  SetLength(Counts, 3);
  Counts[0].StatusId := 20;
  Counts[0].Count := 3;
  Counts[1].StatusId := 10;
  Counts[1].Count := 1;
  Counts[2].StatusId := 30;
  Counts[2].Count := 0;

  Segments := BuildStatusSegmentWidths(Counts, Definitions, 4, 12);
  AssertEquals(2, Length(Segments));
  AssertEquals(10, Segments[0].StatusId);
  AssertEquals(3, Segments[0].Width);
  AssertEquals(20, Segments[1].StatusId);
  AssertEquals(9, Segments[1].Width);

  SetLength(Counts, 1);
  Counts[0].StatusId := 20;
  Counts[0].Count := 5;
  Segments := BuildStatusSegmentWidths(Counts, Definitions, 5, 14);
  AssertEquals(1, Length(Segments));
  AssertEquals(20, Segments[0].StatusId);
  AssertEquals(14, Segments[0].Width);

  SetLength(Counts, 2);
  Counts[0].StatusId := 10;
  Counts[0].Count := 1;
  Counts[1].StatusId := 40;
  Counts[1].Count := 24;
  Segments := BuildStatusSegmentWidths(Counts, Definitions, 25, 14);
  AssertEquals(2, Length(Segments));
  AssertEquals(10, Segments[0].StatusId);
  AssertEquals(1, Segments[0].Width);
  AssertEquals(40, Segments[1].StatusId);
  AssertEquals(13, Segments[1].Width);

  SetLength(Counts, 4);
  Counts[0].StatusId := 10;
  Counts[0].Count := 100;
  Counts[1].StatusId := 20;
  Counts[1].Count := 1;
  Counts[2].StatusId := 30;
  Counts[2].Count := 1;
  Counts[3].StatusId := 40;
  Counts[3].Count := 1;
  Segments := BuildStatusSegmentWidths(Counts, Definitions, 103, 14);
  AssertEquals(4, Length(Segments));
  AssertEquals(14, SumWidths(Segments));
  for I := 0 to High(Segments) do
    AssertTrue(Segments[I].Width >= 1);

  SetLength(Counts, 2);
  Counts[0].StatusId := 99;
  Counts[0].Count := 1;
  Counts[1].StatusId := 10;
  Counts[1].Count := 1;
  Segments := BuildStatusSegmentWidths(Counts, Definitions, 2, 10);
  AssertEquals(2, Length(Segments));
  AssertEquals(10, Segments[0].StatusId);
  AssertEquals(99, Segments[1].StatusId);
end;

initialization
  RegisterTest(TStatusCatalogTests);
end.
