unit translation_memory_focus_controller_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, KMemo, fpcunit,
  testregistry, translation_memory_focus_controller;

type
  { TTranslationMemoryFocusControllerTests }

  TTranslationMemoryFocusControllerTests = class(TTestCase)
  private
    FController: TTranslationMemoryFocusController;
    FEntries: TTranslationMemoryFocusEntryArray;
    FHost: TForm;
    FMemo: TKMemo;
    procedure AddEntry(AEntryIndex: Integer; const ATitle: string;
      const ASyntagmIndexes: array of Integer);
    procedure AssertEntryFocused(AEntryIndex: Integer; AFocused: Boolean);
    procedure AssertNoEntryFocused;
    procedure BuildDocument;
    function DocumentTopOfEntry(AEntryIndex: Integer): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure FocusesWholeEntriesWithoutChangingGeometry;
    procedure LastEntryScrollSaturatesAtMaximumReachablePosition;
    procedure MissingClearUnbindAndRebindAreSafe;
  end;

implementation

procedure TTranslationMemoryFocusControllerTests.AddEntry(
  AEntryIndex: Integer; const ATitle: string;
  const ASyntagmIndexes: array of Integer);
var
  Index: Integer;
  Paragraph: TKMemoParagraph;
begin
  FEntries[AEntryIndex].AnchorBlockIndex := FMemo.Blocks.Count;
  FMemo.Blocks.AddHyperlink(ATitle, IntToStr(AEntryIndex));
  Paragraph := FMemo.Blocks.AddParagraph;
  Paragraph.ParaStyle.TopPadding := 3;
  FEntries[AEntryIndex].FirstParagraphBlockIndex :=
    FMemo.Blocks.IndexOf(Paragraph);

  FMemo.Blocks.AddTextBlock('Forms: ' + ATitle + ', ' + ATitle + '2');
  Paragraph := FMemo.Blocks.AddParagraph;
  Paragraph.ParaStyle.FirstIndent := 0;

  FMemo.Blocks.AddTextBlock(
    'translation one (12), translation two (8), translation three (3)');
  Paragraph := FMemo.Blocks.AddParagraph;
  Paragraph.ParaStyle.FirstIndent := 8;
  Paragraph.ParaStyle.BottomPadding := 4;
  FEntries[AEntryIndex].LastParagraphBlockIndex :=
    FMemo.Blocks.IndexOf(Paragraph);

  SetLength(FEntries[AEntryIndex].SyntagmIndexes,
    Length(ASyntagmIndexes));
  for Index := 0 to High(ASyntagmIndexes) do
    FEntries[AEntryIndex].SyntagmIndexes[Index] :=
      ASyntagmIndexes[Index];
end;

procedure TTranslationMemoryFocusControllerTests.BuildDocument;
begin
  SetLength(FEntries, 5);
  FMemo.Blocks.LockUpdate;
  try
    FMemo.Blocks.Clear;
    AddEntry(0, 'alpha', [10]);
    AddEntry(1, 'beta', [20]);
    AddEntry(2, 'grouped gamma', [30, 31]);
    AddEntry(3, 'delta', [40]);
    AddEntry(4, 'last epsilon', [50]);
  finally
    FMemo.Blocks.UnLockUpdate;
  end;
  Application.ProcessMessages;
end;

procedure TTranslationMemoryFocusControllerTests.SetUp;
begin
  inherited SetUp;
  Application.Initialize;
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(40, 40, 280, 125);
  FMemo := TKMemo.Create(FHost);
  FMemo.Parent := FHost;
  FMemo.Align := alClient;
  FMemo.ReadOnly := True;
  FMemo.Color := clWindow;
  FMemo.TextStyle.Font.Name := 'Arial';
  FMemo.TextStyle.Font.Size := 10;
  FHost.Show;
  BuildDocument;
  FController := TTranslationMemoryFocusController.Create;
  FController.Bind(FMemo, FEntries);
  Application.ProcessMessages;
end;

procedure TTranslationMemoryFocusControllerTests.TearDown;
begin
  FController.Free;
  FHost.Free;
  inherited TearDown;
end;

procedure TTranslationMemoryFocusControllerTests.AssertEntryFocused(
  AEntryIndex: Integer; AFocused: Boolean);
var
  BlockIndex: Integer;
  Paragraph: TKMemoParagraph;
begin
  for BlockIndex := FEntries[AEntryIndex].FirstParagraphBlockIndex to
    FEntries[AEntryIndex].LastParagraphBlockIndex do
    if FMemo.Blocks[BlockIndex] is TKMemoParagraph then
    begin
      Paragraph := TKMemoParagraph(FMemo.Blocks[BlockIndex]);
      AssertEquals('left border remains reserved', 1,
        Paragraph.ParaStyle.BorderWidths.Left);
      AssertEquals('right border remains reserved', 1,
        Paragraph.ParaStyle.BorderWidths.Right);
      if BlockIndex = FEntries[AEntryIndex].FirstParagraphBlockIndex then
        AssertEquals('first paragraph has top border', 1,
          Paragraph.ParaStyle.BorderWidths.Top)
      else
        AssertEquals('inner paragraph has no top border', 0,
          Paragraph.ParaStyle.BorderWidths.Top);
      if BlockIndex = FEntries[AEntryIndex].LastParagraphBlockIndex then
        AssertEquals('last paragraph has bottom border', 1,
          Paragraph.ParaStyle.BorderWidths.Bottom)
      else
        AssertEquals('inner paragraph has no bottom border', 0,
          Paragraph.ParaStyle.BorderWidths.Bottom);
      if AFocused then
      begin
        AssertEquals('focused border uses highlight color',
          LongInt(clHighlight), LongInt(Paragraph.ParaStyle.BorderColor));
        AssertEquals('focused paragraph has solid background',
          Ord(bsSolid), Ord(Paragraph.ParaStyle.Brush.Style));
        AssertEquals('focused background uses info color',
          LongInt(clInfoBk), LongInt(Paragraph.ParaStyle.Brush.Color));
      end
      else
      begin
        AssertEquals('normal border is painted in memo background color',
          LongInt(clWindow), LongInt(Paragraph.ParaStyle.BorderColor));
        AssertEquals('normal paragraph keeps its clear background',
          Ord(bsClear), Ord(Paragraph.ParaStyle.Brush.Style));
      end;
    end;
end;

procedure TTranslationMemoryFocusControllerTests.AssertNoEntryFocused;
var
  EntryIndex: Integer;
begin
  for EntryIndex := 0 to High(FEntries) do
    AssertEntryFocused(EntryIndex, False);
end;

function TTranslationMemoryFocusControllerTests.DocumentTopOfEntry(
  AEntryIndex: Integer): Integer;
var
  AnchorRect: TRect;
begin
  FMemo.TopPos := 0;
  Application.ProcessMessages;
  AnchorRect := FMemo.BlockRect(
    FMemo.Blocks[FEntries[AEntryIndex].AnchorBlockIndex]);
  Result := AnchorRect.Top;
end;

procedure TTranslationMemoryFocusControllerTests.
  FocusesWholeEntriesWithoutChangingGeometry;
var
  InitialBlockCount: Integer;
  InitialContentHeight: Integer;
begin
  InitialBlockCount := FMemo.Blocks.Count;
  InitialContentHeight := FMemo.ContentHeight;
  AssertNoEntryFocused;

  AssertTrue('first entry resolves by its syntagm',
    FController.FocusSyntagmIndex(10));
  Application.ProcessMessages;
  AssertEntryFocused(0, True);
  AssertEntryFocused(1, False);
  AssertEquals('focus does not add blocks', InitialBlockCount,
    FMemo.Blocks.Count);
  AssertEquals('focus does not change content height',
    InitialContentHeight, FMemo.ContentHeight);

  AssertTrue('grouped entry resolves by its second syntagm',
    FController.FocusSyntagmIndex(31));
  Application.ProcessMessages;
  AssertEntryFocused(0, False);
  AssertEntryFocused(2, True);
  AssertEquals('changing focus does not add blocks', InitialBlockCount,
    FMemo.Blocks.Count);
  AssertEquals('changing focus keeps geometry',
    InitialContentHeight, FMemo.ContentHeight);

  AssertTrue('repeated focus is accepted', FController.FocusEntry(2));
  Application.ProcessMessages;
  AssertEntryFocused(2, True);
  AssertEquals('repeated focus keeps content height',
    InitialContentHeight, FMemo.ContentHeight);
end;

procedure TTranslationMemoryFocusControllerTests.
  LastEntryScrollSaturatesAtMaximumReachablePosition;
var
  LastEntryDocumentTop: Integer;
  MaximumTop: Integer;
begin
  LastEntryDocumentTop := DocumentTopOfEntry(High(FEntries));
  FMemo.TopPos := MaxInt;
  Application.ProcessMessages;
  MaximumTop := FMemo.TopPos;
  AssertTrue('test document is taller than the viewport', MaximumTop > 0);
  AssertTrue('last entry cannot naturally be placed at the top',
    LastEntryDocumentTop > MaximumTop);

  FMemo.TopPos := 0;
  AssertTrue(FController.FocusEntry(High(FEntries)));
  Application.ProcessMessages;
  AssertEquals('last entry scroll clamps at the maximum reachable top',
    MaximumTop, FMemo.TopPos);
  AssertEntryFocused(High(FEntries), True);

  AssertTrue(FController.FocusEntry(0));
  Application.ProcessMessages;
  AssertEquals('first entry returns to the natural top', 0, FMemo.TopPos);
  AssertEntryFocused(0, True);
  AssertEntryFocused(High(FEntries), False);
end;

procedure TTranslationMemoryFocusControllerTests.
  MissingClearUnbindAndRebindAreSafe;
var
  EntryCount: Integer;
  FirstParagraph: TKMemoParagraph;
begin
  AssertTrue(FController.FocusSyntagmIndex(30));
  AssertEntryFocused(2, True);
  AssertFalse('missing syntagm clears focus',
    FController.FocusSyntagmIndex(999));
  AssertNoEntryFocused;

  FController.FocusEntry(1);
  FController.Clear;
  FController.Clear;
  AssertNoEntryFocused;

  FController.Unbind;
  FirstParagraph := TKMemoParagraph(
    FMemo.Blocks[FEntries[0].FirstParagraphBlockIndex]);
  AssertEquals('unbind restores original left border', 0,
    FirstParagraph.ParaStyle.BorderWidths.Left);
  AssertEquals('unbind restores original right border', 0,
    FirstParagraph.ParaStyle.BorderWidths.Right);
  AssertEquals('unbind restores original clear background',
    Ord(bsClear), Ord(FirstParagraph.ParaStyle.Brush.Style));
  AssertFalse('unbound controller rejects focus', FController.FocusEntry(0));

  { A renderer unbinds before replacing its blocks, then binds the freshly
    recorded ranges. }
  BuildDocument;
  EntryCount := Length(FEntries);
  FController.Bind(FMemo, FEntries);
  AssertTrue('rebound controller resolves the rebuilt document',
    FController.FocusSyntagmIndex(50));
  AssertEquals(EntryCount, Length(FEntries));
  AssertEntryFocused(4, True);

  FController.Unbind;
  BuildDocument;
  FController.Bind(FMemo, FEntries);
  AssertTrue('bind after another rebuild has no stale references',
    FController.FocusSyntagmIndex(20));
  AssertEntryFocused(1, True);
end;

initialization
  RegisterTest(TTranslationMemoryFocusControllerTests);

end.
