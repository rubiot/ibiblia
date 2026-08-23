unit translation_memory_focus_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, KMemo;

type
  TTranslationMemoryFocusIntegerArray = array of Integer;

  TTranslationMemoryFocusEntry = record
    AnchorBlockIndex: Integer;
    FirstParagraphBlockIndex: Integer;
    LastParagraphBlockIndex: Integer;
    SyntagmIndexes: TTranslationMemoryFocusIntegerArray;
  end;

  TTranslationMemoryFocusEntryArray =
    array of TTranslationMemoryFocusEntry;

  { TTranslationMemoryFocusController }

  TTranslationMemoryFocusController = class
  private type
    TParagraphState = record
      BlockIndex: Integer;
      BlockIdentity: Pointer;
      EntryIndex: Integer;
      IsFirst: Boolean;
      IsLast: Boolean;
      OriginalStyle: TKMemoParaStyle;
    end;
    TParagraphStateArray = array of TParagraphState;
  private
    FMemo: TKCustomMemo;
    FEntries: TTranslationMemoryFocusEntryArray;
    FParagraphStates: TParagraphStateArray;
    FFocusedEntryIndex: Integer;
    procedure ApplyEntryStyle(AEntryIndex: Integer; AFocused: Boolean);
    procedure ApplyParagraphStyle(var AState: TParagraphState;
      AFocused: Boolean);
    procedure CaptureParagraphStates;
    procedure ClearParagraphStates;
    procedure CopyEntries(const AEntries: TTranslationMemoryFocusEntryArray);
    function NormalBorderColor: TColor;
    procedure RestoreParagraphStates;
    procedure ScrollEntryIntoFocus(AEntryIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Bind(AMemo: TKCustomMemo;
      const AEntries: TTranslationMemoryFocusEntryArray);
    function FocusSyntagmIndex(ASyntagmIndex: Integer): Boolean;
    function FocusEntry(AEntryIndex: Integer): Boolean;
    procedure Clear;
    procedure Unbind;
  end;

implementation

uses
  SysUtils;

constructor TTranslationMemoryFocusController.Create;
begin
  inherited Create;
  FMemo := nil;
  FFocusedEntryIndex := -1;
end;

destructor TTranslationMemoryFocusController.Destroy;
begin
  Unbind;
  inherited Destroy;
end;

procedure TTranslationMemoryFocusController.CopyEntries(
  const AEntries: TTranslationMemoryFocusEntryArray);
var
  EntryIndex: Integer;
  SyntagmIndex: Integer;
begin
  SetLength(FEntries, Length(AEntries));
  for EntryIndex := 0 to High(AEntries) do
  begin
    FEntries[EntryIndex].AnchorBlockIndex :=
      AEntries[EntryIndex].AnchorBlockIndex;
    FEntries[EntryIndex].FirstParagraphBlockIndex :=
      AEntries[EntryIndex].FirstParagraphBlockIndex;
    FEntries[EntryIndex].LastParagraphBlockIndex :=
      AEntries[EntryIndex].LastParagraphBlockIndex;
    SetLength(FEntries[EntryIndex].SyntagmIndexes,
      Length(AEntries[EntryIndex].SyntagmIndexes));
    for SyntagmIndex := 0 to
      High(AEntries[EntryIndex].SyntagmIndexes) do
      FEntries[EntryIndex].SyntagmIndexes[SyntagmIndex] :=
        AEntries[EntryIndex].SyntagmIndexes[SyntagmIndex];
  end;
end;

procedure TTranslationMemoryFocusController.ClearParagraphStates;
var
  StateIndex: Integer;
begin
  for StateIndex := 0 to High(FParagraphStates) do
    FParagraphStates[StateIndex].OriginalStyle.Free;
  SetLength(FParagraphStates, 0);
end;

procedure TTranslationMemoryFocusController.CaptureParagraphStates;
var
  BlockIndex: Integer;
  EntryIndex: Integer;
  Paragraph: TKMemoParagraph;
  StateCount: Integer;
begin
  ClearParagraphStates;
  if not Assigned(FMemo) then
    Exit;

  StateCount := 0;
  for EntryIndex := 0 to High(FEntries) do
  begin
    if FEntries[EntryIndex].FirstParagraphBlockIndex < 0 then
      Continue;
    for BlockIndex := FEntries[EntryIndex].FirstParagraphBlockIndex to
      FEntries[EntryIndex].LastParagraphBlockIndex do
    begin
      if (BlockIndex < 0) or (BlockIndex >= FMemo.Blocks.Count) or
         not (FMemo.Blocks[BlockIndex] is TKMemoParagraph) then
        Continue;
      Paragraph := TKMemoParagraph(FMemo.Blocks[BlockIndex]);
      SetLength(FParagraphStates, StateCount + 1);
      FParagraphStates[StateCount].BlockIndex := BlockIndex;
      FParagraphStates[StateCount].BlockIdentity := Pointer(Paragraph);
      FParagraphStates[StateCount].EntryIndex := EntryIndex;
      FParagraphStates[StateCount].IsFirst := BlockIndex =
        FEntries[EntryIndex].FirstParagraphBlockIndex;
      FParagraphStates[StateCount].IsLast := BlockIndex =
        FEntries[EntryIndex].LastParagraphBlockIndex;
      FParagraphStates[StateCount].OriginalStyle := TKMemoParaStyle.Create;
      FParagraphStates[StateCount].OriginalStyle.Assign(
        Paragraph.ParaStyle);
      Inc(StateCount);
    end;
  end;
end;

function TTranslationMemoryFocusController.NormalBorderColor: TColor;
begin
  Result := clWindow;
  if not Assigned(FMemo) then
    Exit;
  if FMemo.Background.Color <> clNone then
    Result := FMemo.Background.Color
  else if (FMemo.Color <> clNone) and (FMemo.Color <> clDefault) then
    Result := FMemo.Color;
end;

procedure TTranslationMemoryFocusController.ApplyParagraphStyle(
  var AState: TParagraphState; AFocused: Boolean);
var
  Paragraph: TKMemoParagraph;
begin
  if not Assigned(FMemo) or
     (AState.BlockIndex < 0) or
     (AState.BlockIndex >= FMemo.Blocks.Count) or
     (Pointer(FMemo.Blocks[AState.BlockIndex]) <> AState.BlockIdentity) or
     not (FMemo.Blocks[AState.BlockIndex] is TKMemoParagraph) then
    Exit;

  Paragraph := TKMemoParagraph(FMemo.Blocks[AState.BlockIndex]);
  Paragraph.ParaStyle.BorderWidth := 0;
  Paragraph.ParaStyle.BorderWidths.Left := 1;
  Paragraph.ParaStyle.BorderWidths.Right := 1;
  if AState.IsFirst then
    Paragraph.ParaStyle.BorderWidths.Top := 1
  else
    Paragraph.ParaStyle.BorderWidths.Top := 0;
  if AState.IsLast then
    Paragraph.ParaStyle.BorderWidths.Bottom := 1
  else
    Paragraph.ParaStyle.BorderWidths.Bottom := 0;

  if AFocused then
  begin
    Paragraph.ParaStyle.BorderColor := clHighlight;
    Paragraph.ParaStyle.Brush.Color := clInfoBk;
    Paragraph.ParaStyle.Brush.Style := bsSolid;
  end
  else
  begin
    Paragraph.ParaStyle.BorderColor := NormalBorderColor;
    Paragraph.ParaStyle.Brush.Assign(AState.OriginalStyle.Brush);
  end;
end;

procedure TTranslationMemoryFocusController.ApplyEntryStyle(
  AEntryIndex: Integer; AFocused: Boolean);
var
  StateIndex: Integer;
begin
  for StateIndex := 0 to High(FParagraphStates) do
    if FParagraphStates[StateIndex].EntryIndex = AEntryIndex then
      ApplyParagraphStyle(FParagraphStates[StateIndex], AFocused);
end;

procedure TTranslationMemoryFocusController.RestoreParagraphStates;
var
  Paragraph: TKMemoParagraph;
  StateIndex: Integer;
begin
  if not Assigned(FMemo) then
    Exit;
  FMemo.Blocks.LockUpdate;
  try
    for StateIndex := 0 to High(FParagraphStates) do
      with FParagraphStates[StateIndex] do
        if (BlockIndex >= 0) and (BlockIndex < FMemo.Blocks.Count) and
           (Pointer(FMemo.Blocks[BlockIndex]) = BlockIdentity) and
           (FMemo.Blocks[BlockIndex] is TKMemoParagraph) then
        begin
          Paragraph := TKMemoParagraph(FMemo.Blocks[BlockIndex]);
          Paragraph.ParaStyle.Assign(OriginalStyle);
        end;
  finally
    FMemo.Blocks.UnLockUpdate;
  end;
end;

procedure TTranslationMemoryFocusController.Bind(AMemo: TKCustomMemo;
  const AEntries: TTranslationMemoryFocusEntryArray);
var
  EntryIndex: Integer;
begin
  Unbind;
  FMemo := AMemo;
  CopyEntries(AEntries);
  FFocusedEntryIndex := -1;
  if not Assigned(FMemo) then
    Exit;

  CaptureParagraphStates;
  FMemo.Blocks.LockUpdate;
  try
    for EntryIndex := 0 to High(FEntries) do
      ApplyEntryStyle(EntryIndex, False);
  finally
    FMemo.Blocks.UnLockUpdate;
  end;
end;

procedure TTranslationMemoryFocusController.ScrollEntryIntoFocus(
  AEntryIndex: Integer);
var
  AnchorBlockIndex: Integer;
  AnchorRect: TRect;
begin
  if not Assigned(FMemo) or (AEntryIndex < 0) or
     (AEntryIndex >= Length(FEntries)) then
    Exit;
  AnchorBlockIndex := FEntries[AEntryIndex].AnchorBlockIndex;
  if (AnchorBlockIndex < 0) or
     (AnchorBlockIndex >= FMemo.Blocks.Count) then
    Exit;
  AnchorRect := FMemo.BlockRect(FMemo.Blocks[AnchorBlockIndex]);
  if (AnchorRect.Right <= AnchorRect.Left) or
     (AnchorRect.Bottom <= AnchorRect.Top) then
    Exit;
  FMemo.TopPos := FMemo.TopPos + AnchorRect.Top;
end;

function TTranslationMemoryFocusController.FocusEntry(
  AEntryIndex: Integer): Boolean;
begin
  Result := Assigned(FMemo) and (AEntryIndex >= 0) and
    (AEntryIndex < Length(FEntries));
  if not Result then
  begin
    Clear;
    Exit;
  end;

  if AEntryIndex <> FFocusedEntryIndex then
  begin
    FMemo.Blocks.LockUpdate;
    try
      if FFocusedEntryIndex >= 0 then
        ApplyEntryStyle(FFocusedEntryIndex, False);
      ApplyEntryStyle(AEntryIndex, True);
      FFocusedEntryIndex := AEntryIndex;
    finally
      FMemo.Blocks.UnLockUpdate;
    end;
  end;
  ScrollEntryIntoFocus(AEntryIndex);
end;

function TTranslationMemoryFocusController.FocusSyntagmIndex(
  ASyntagmIndex: Integer): Boolean;
var
  EntryIndex: Integer;
  IndexInEntry: Integer;
begin
  for EntryIndex := 0 to High(FEntries) do
    for IndexInEntry := 0 to High(FEntries[EntryIndex].SyntagmIndexes) do
      if FEntries[EntryIndex].SyntagmIndexes[IndexInEntry] =
         ASyntagmIndex then
        Exit(FocusEntry(EntryIndex));
  Clear;
  Result := False;
end;

procedure TTranslationMemoryFocusController.Clear;
begin
  if not Assigned(FMemo) or (FFocusedEntryIndex < 0) then
    Exit;
  FMemo.Blocks.LockUpdate;
  try
    ApplyEntryStyle(FFocusedEntryIndex, False);
    FFocusedEntryIndex := -1;
  finally
    FMemo.Blocks.UnLockUpdate;
  end;
end;

procedure TTranslationMemoryFocusController.Unbind;
begin
  RestoreParagraphStates;
  ClearParagraphStates;
  SetLength(FEntries, 0);
  FFocusedEntryIndex := -1;
  FMemo := nil;
end;

end.
