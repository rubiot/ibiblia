unit bible_tree_coordinator;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, ComCtrls, BibleScope, status_catalog;

type
  { IVerseStatusCursor — the coordinator iterates this instead of calling
    back into the owner via four separate procedure-of-object variables. }
  IVerseStatusCursor = interface
    procedure Reset;
    function EOF: Boolean;
    procedure Advance;
    function CurrentStatus: Integer;
  end;

  TStatusAggregateImageResolver = function(const Counts: TStatusCounts;
    Total: Integer; const StatusDefinitions: TStatusDefinitions): Integer of object;

  TBibleTreeCoordinator = class
  private
    FTree: TTreeView;
    function BaseNodeText(const Scope: TEscopoTexto; Node: TTreeNode): string;
    procedure CountVerseStatuses(Node: TTreeNode;
      const StatusDefinitions: TStatusDefinitions; out Counts: TStatusCounts;
      out Total: Integer);
    function FindVerseNode(const Scope: TEscopoTexto; const ReferenceID: string): TTreeNode;
    function ResolveNodePath(const Scope: TEscopoTexto; BookID, Chapter,
      Verse: Integer): TTreeNode;
    procedure ApplyAggregateStatus(const Scope: TEscopoTexto; ParentNode: TTreeNode;
      const StatusDefinitions: TStatusDefinitions;
      AggregateImageResolver: TStatusAggregateImageResolver);
    procedure RefreshNodeCaption(const Scope: TEscopoTexto; Node: TTreeNode);
    procedure SetNodeStatus(Node: TTreeNode; Status: Integer);
  public
    constructor Create(ATree: TTreeView = nil);
    procedure Attach(ATree: TTreeView);
    procedure Populate(const Scope: TEscopoTexto; OnChange: TTVChangedEvent;
      const StatusDefinitions: TStatusDefinitions;
      AggregateImageResolver: TStatusAggregateImageResolver = nil);
    function BuildStatusHint(const Scope: TEscopoTexto; Node: TTreeNode;
      const StatusDefinitions: TStatusDefinitions): string;
    function NodeToReferenceID(const Scope: TEscopoTexto; const Node: TTreeNode): string;
    procedure RecomputeAllStatuses(const Scope: TEscopoTexto;
      Cursor: IVerseStatusCursor; const StatusDefinitions: TStatusDefinitions;
      AggregateImageResolver: TStatusAggregateImageResolver = nil);
    procedure RefreshCaptions(const Scope: TEscopoTexto);
    procedure UpdateReferenceStatus(const Scope: TEscopoTexto;
      const ReferenceID: string; Status: Integer;
      const StatusDefinitions: TStatusDefinitions;
      AggregateImageResolver: TStatusAggregateImageResolver = nil);
    procedure SelectReference(const Scope: TEscopoTexto; const ReferenceID: string);
  end;

implementation

{$INCLUDE include/bookdefs.inc}

constructor TBibleTreeCoordinator.Create(ATree: TTreeView);
begin
  inherited Create;
  Attach(ATree);
end;

procedure TBibleTreeCoordinator.Attach(ATree: TTreeView);
begin
  FTree := ATree;
end;

function TBibleTreeCoordinator.BaseNodeText(const Scope: TEscopoTexto;
  Node: TTreeNode): string;
var
  BookIndex: Integer;
  NodeValue: PtrUInt;
begin
  Result := '';
  if not Assigned(Node) then
    Exit;

  NodeValue := PtrUInt(Node.Data);
  case Node.Level of
    0:
      begin
        BookIndex := Integer(NodeValue) - OffsetLivros[Scope] - 1;
        if (BookIndex >= 0) and (BookIndex < QLivros[Scope]) then
          Result := NLivros[Scope][BookIndex]
        else
          Result := Node.Text;
      end;
    1, 2:
      if NodeValue > 0 then
        Result := IntToStr(Integer(NodeValue))
      else
        Result := Node.Text;
  else
    Result := Node.Text;
  end;
end;

function IndexOfStatusCount(const Counts: TStatusCounts; StatusId: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(Counts) do
    if Counts[I].StatusId = StatusId then
      Exit(I);
end;

procedure EnsureStatusCount(var Counts: TStatusCounts; StatusId: Integer);
var
  Index: Integer;
begin
  Index := IndexOfStatusCount(Counts, StatusId);
  if Index >= 0 then
    Exit;

  SetLength(Counts, Length(Counts) + 1);
  Counts[High(Counts)].StatusId := StatusId;
  Counts[High(Counts)].Count := 0;
end;

procedure IncrementStatusCount(var Counts: TStatusCounts; StatusId: Integer;
  Amount: Integer = 1);
var
  Index: Integer;
begin
  EnsureStatusCount(Counts, StatusId);
  Index := IndexOfStatusCount(Counts, StatusId);
  Inc(Counts[Index].Count, Amount);
end;

function StatusCountValue(const Counts: TStatusCounts; StatusId: Integer): Integer;
var
  Index: Integer;
begin
  Index := IndexOfStatusCount(Counts, StatusId);
  if Index >= 0 then
    Result := Counts[Index].Count
  else
    Result := 0;
end;

procedure InitializeStatusCounts(var Counts: TStatusCounts;
  const StatusDefinitions: TStatusDefinitions);
var
  I: Integer;
begin
  SetLength(Counts, Length(StatusDefinitions));
  for I := 0 to High(StatusDefinitions) do
  begin
    Counts[I].StatusId := StatusDefinitions[I].Id;
    Counts[I].Count := 0;
  end;
end;

procedure TBibleTreeCoordinator.CountVerseStatuses(Node: TTreeNode;
  const StatusDefinitions: TStatusDefinitions; out Counts: TStatusCounts;
  out Total: Integer);
var
  ChildNode: TTreeNode;
  ChildCounts: TStatusCounts;
  ChildTotal: Integer;
  CountIndex: Integer;
begin
  InitializeStatusCounts(Counts, StatusDefinitions);
  Total := 0;

  if not Assigned(Node) then
    Exit;

  if not Node.HasChildren then
  begin
    IncrementStatusCount(Counts, Node.ImageIndex);
    Inc(Total);
    Exit;
  end;

  ChildNode := Node.GetFirstChild;
  while Assigned(ChildNode) do
  begin
    CountVerseStatuses(ChildNode, StatusDefinitions, ChildCounts, ChildTotal);
    for CountIndex := 0 to High(ChildCounts) do
      IncrementStatusCount(Counts, ChildCounts[CountIndex].StatusId,
        ChildCounts[CountIndex].Count);
    Inc(Total, ChildTotal);
    ChildNode := ChildNode.GetNextSibling;
  end;
end;

procedure TBibleTreeCoordinator.RefreshNodeCaption(const Scope: TEscopoTexto;
  Node: TTreeNode);
begin
  if not Assigned(Node) then
    Exit;

  Node.Text := BaseNodeText(Scope, Node);
end;

procedure TBibleTreeCoordinator.Populate(const Scope: TEscopoTexto;
  OnChange: TTVChangedEvent; const StatusDefinitions: TStatusDefinitions;
  AggregateImageResolver: TStatusAggregateImageResolver);
var
  BookIndex: SmallInt;
  ChapterIndex: SmallInt;
  VerseIndex: SmallInt;
  BookNode: TTreeNode;
  ChapterNode: TTreeNode;
  VerseNode: TTreeNode;
  InitialStatus: Integer;
begin
  if not Assigned(FTree) then
    Exit;

  InitialStatus := FirstStatusId(StatusDefinitions);

  if FTree.Items.Count = 0 then
  begin
    FTree.BeginUpdate;
    try
      for BookIndex := 0 to QLivros[Scope] - 1 do
      begin
        BookNode := FTree.Items.Add(nil, NLivros[Scope][BookIndex]);
        BookNode.Data := Pointer(PtrUInt(BookIndex + OffsetLivros[Scope] + 1));
        SetNodeStatus(BookNode, InitialStatus);
        for ChapterIndex := 0 to QCapitulos[Scope][BookIndex] - 1 do
        begin
          ChapterNode := FTree.Items.AddChild(BookNode, IntToStr(ChapterIndex + 1));
          ChapterNode.Data := Pointer(PtrUInt(ChapterIndex + 1));
          SetNodeStatus(ChapterNode, InitialStatus);
          for VerseIndex := 0 to QVersiculos[Scope][QCapLivros[Scope][BookIndex] + ChapterIndex - 1] - 1 do
          begin
            VerseNode := FTree.Items.AddChild(ChapterNode, IntToStr(VerseIndex + 1));
            VerseNode.Data := Pointer(PtrUInt(VerseIndex + 1));
            SetNodeStatus(VerseNode, InitialStatus);
          end;
          ApplyAggregateStatus(Scope, ChapterNode, StatusDefinitions,
            AggregateImageResolver);
        end;
        ApplyAggregateStatus(Scope, BookNode, StatusDefinitions,
          AggregateImageResolver);
      end;
    finally
      FTree.EndUpdate;
    end;
  end;

  FTree.OnChange := OnChange;
end;

function TBibleTreeCoordinator.BuildStatusHint(const Scope: TEscopoTexto;
  Node: TTreeNode; const StatusDefinitions: TStatusDefinitions): string;

  function StatusCaption(StatusId: Integer): string;
  begin
    Result := GetStatusCaption(StatusDefinitions, StatusId);
  end;

  function NodeTitle: string;
  var
    BookNode: TTreeNode;
    ChapterNode: TTreeNode;
  begin
    Result := '';
    if not Assigned(Node) then
      Exit;

    case Node.Level of
      0:
        Result := BaseNodeText(Scope, Node);
      1:
        Result := Format('%s %s', [
          BaseNodeText(Scope, Node.Parent),
          BaseNodeText(Scope, Node)
        ]);
      2:
        begin
          ChapterNode := Node.Parent;
          BookNode := ChapterNode.Parent;
          Result := Format('%s %s:%s', [
            BaseNodeText(Scope, BookNode),
            BaseNodeText(Scope, ChapterNode),
            BaseNodeText(Scope, Node)
          ]);
        end;
    else
      Result := Node.Text;
    end;
  end;

var
  Counts: TStatusCounts;
  Total: Integer;
  StatusIndex: Integer;
  Sorted: TStatusDefinitions;
  CountIndex: Integer;
begin
  Result := '';
  if not Assigned(Node) then
    Exit;

  if not Node.HasChildren then
  begin
    StatusIndex := Node.ImageIndex;
    Result := Format('%s%sStatus: %s', [
      NodeTitle,
      LineEnding,
      StatusCaption(StatusIndex)
    ]);
    Exit;
  end;

  CountVerseStatuses(Node, StatusDefinitions, Counts, Total);
  Result := Format('%s%sTotal: %d', [NodeTitle, LineEnding, Total]);
  Sorted := SortStatusDefinitionsByDisplayOrder(StatusDefinitions);
  for StatusIndex := 0 to High(Sorted) do
    Result := Result + LineEnding +
      Format('%s: %d', [StatusCaption(Sorted[StatusIndex].Id),
        StatusCountValue(Counts, Sorted[StatusIndex].Id)]);
  for CountIndex := 0 to High(Counts) do
    if not IsValidStatusId(StatusDefinitions, Counts[CountIndex].StatusId) then
      Result := Result + LineEnding +
        Format('%s: %d', [StatusCaption(Counts[CountIndex].StatusId),
          Counts[CountIndex].Count]);
end;

function TBibleTreeCoordinator.ResolveNodePath(const Scope: TEscopoTexto; BookID,
  Chapter, Verse: Integer): TTreeNode;
var
  Index: SmallInt;
begin
  Result := nil;
  if not Assigned(FTree) or (FTree.Items.Count = 0) then
    Exit;

  Result := FTree.Items[0];
  for Index := BookID - OffsetLivros[Scope] - 1 downto 1 do
    Result := Result.GetNextSibling;

  if not Assigned(Result) then
    Exit;

  Result := Result.GetFirstChild;
  for Index := Chapter - 1 downto 1 do
    Result := Result.GetNextSibling;

  if not Assigned(Result) then
    Exit;

  Result := Result.GetFirstChild;
  for Index := Verse - 1 downto 1 do
    Result := Result.GetNextSibling;
end;

function TBibleTreeCoordinator.FindVerseNode(const Scope: TEscopoTexto;
  const ReferenceID: string): TTreeNode;
var
  BookID: Integer;
  Chapter: Integer;
  Verse: Integer;
begin
  Result := nil;
  if ReferenceID.IsEmpty then
    Exit;

  Sscanf(ReferenceID, '%d,%d,%d', [@BookID, @Chapter, @Verse]);
  Result := ResolveNodePath(Scope, BookID, Chapter, Verse);
end;

function TBibleTreeCoordinator.NodeToReferenceID(const Scope: TEscopoTexto;
  const Node: TTreeNode): string;
var
  ChapterNode: TTreeNode;
  BookNode: TTreeNode;
  VerseNode: TTreeNode;
begin
  Result := '';
  if not Assigned(Node) then
    Exit;

  if Node.HasChildren then
  begin
    if Assigned(Node.Parent) then
    begin
      ChapterNode := Node;
      BookNode := ChapterNode.Parent;
      VerseNode := ChapterNode.GetFirstChild;
    end
    else
    begin
      BookNode := Node;
      ChapterNode := BookNode.GetFirstChild;
      VerseNode := ChapterNode.GetFirstChild;
    end;
  end
  else
  begin
    VerseNode := Node;
    ChapterNode := VerseNode.Parent;
    BookNode := ChapterNode.Parent;
  end;

  if Assigned(BookNode) and Assigned(ChapterNode) and Assigned(VerseNode) and
    (PtrUInt(BookNode.Data) > PtrUInt(OffsetLivros[Scope])) then
    Result := Format('%d,%d,%d', [
      PtrUInt(BookNode.Data),
      PtrUInt(ChapterNode.Data),
      PtrUInt(VerseNode.Data)
    ]);
end;

procedure TBibleTreeCoordinator.ApplyAggregateStatus(const Scope: TEscopoTexto;
  ParentNode: TTreeNode; const StatusDefinitions: TStatusDefinitions;
  AggregateImageResolver: TStatusAggregateImageResolver);
var
  Counts: TStatusCounts;
  TotalVerses: Integer;
  AggregateImageIndex: Integer;
  UniformStatus: Integer;
  HasUniformStatus: Boolean;
  SortedByDisplay: TStatusDefinitions;
  StatusIndex: Integer;
begin
  if not Assigned(ParentNode) then
    Exit;

  CountVerseStatuses(ParentNode, StatusDefinitions, Counts, TotalVerses);
  if TotalVerses <= 0 then
  begin
    SetNodeStatus(ParentNode, FirstStatusId(StatusDefinitions));
    RefreshNodeCaption(Scope, ParentNode);
    Exit;
  end;

  if Assigned(AggregateImageResolver) then
  begin
    AggregateImageIndex := AggregateImageResolver(Counts, TotalVerses,
      StatusDefinitions);
    if AggregateImageIndex >= 0 then
    begin
      SetNodeStatus(ParentNode, AggregateImageIndex);
      RefreshNodeCaption(Scope, ParentNode);
      Exit;
    end;
  end;

  UniformStatus := -1;
  HasUniformStatus := True;

  for StatusIndex := 0 to High(Counts) do
    if Counts[StatusIndex].Count > 0 then
    begin
      if UniformStatus < 0 then
        UniformStatus := Counts[StatusIndex].StatusId
      else if Counts[StatusIndex].StatusId <> UniformStatus then
        HasUniformStatus := False;
    end;

  if HasUniformStatus and (UniformStatus >= 0) then
  begin
    SetNodeStatus(ParentNode, UniformStatus);
    RefreshNodeCaption(Scope, ParentNode);
    Exit;
  end;

  SortedByDisplay := SortStatusDefinitionsByDisplayOrder(StatusDefinitions);
  for StatusIndex := 0 to High(SortedByDisplay) do
    if StatusCountValue(Counts, SortedByDisplay[StatusIndex].Id) > 0 then
    begin
      SetNodeStatus(ParentNode, SortedByDisplay[StatusIndex].Id);
      RefreshNodeCaption(Scope, ParentNode);
      Exit;
    end;

  for StatusIndex := 0 to High(Counts) do
    if Counts[StatusIndex].Count > 0 then
    begin
      SetNodeStatus(ParentNode, Counts[StatusIndex].StatusId);
      RefreshNodeCaption(Scope, ParentNode);
      Exit;
    end;

  SetNodeStatus(ParentNode, FirstStatusId(StatusDefinitions));
  RefreshNodeCaption(Scope, ParentNode);
end;

procedure TBibleTreeCoordinator.SetNodeStatus(Node: TTreeNode; Status: Integer);
begin
  if not Assigned(Node) then
    Exit;

  Node.ImageIndex := Status;
  Node.SelectedIndex := Status;
end;

procedure TBibleTreeCoordinator.RecomputeAllStatuses(const Scope: TEscopoTexto;
  Cursor: IVerseStatusCursor; const StatusDefinitions: TStatusDefinitions;
  AggregateImageResolver: TStatusAggregateImageResolver);
var
  BookNode: TTreeNode;
  ChapterNode: TTreeNode;
  VerseNode: TTreeNode;
begin
  if not Assigned(FTree) or (FTree.Items.Count = 0) or not Assigned(Cursor) then
    Exit;

  Cursor.Reset;

  FTree.BeginUpdate;
  try
    BookNode := FTree.Items[0];
    while Assigned(BookNode) and not Cursor.EOF do
    begin
      ChapterNode := BookNode.GetFirstChild;
      while Assigned(ChapterNode) and not Cursor.EOF do
      begin
        VerseNode := ChapterNode.GetFirstChild;
        while Assigned(VerseNode) and not Cursor.EOF do
        begin
          SetNodeStatus(VerseNode, Cursor.CurrentStatus);
          Cursor.Advance;
          VerseNode := VerseNode.GetNextSibling;
        end;

        ApplyAggregateStatus(Scope, ChapterNode, StatusDefinitions,
          AggregateImageResolver);
        ChapterNode := ChapterNode.GetNextSibling;
      end;

      ApplyAggregateStatus(Scope, BookNode, StatusDefinitions,
        AggregateImageResolver);
      BookNode := BookNode.GetNextSibling;
    end;
  finally
    FTree.EndUpdate;
  end;
end;

procedure TBibleTreeCoordinator.RefreshCaptions(const Scope: TEscopoTexto);
var
  BookNode: TTreeNode;
  ChapterNode: TTreeNode;
begin
  if not Assigned(FTree) or (FTree.Items.Count = 0) then
    Exit;

  FTree.BeginUpdate;
  try
    BookNode := FTree.Items[0];
    while Assigned(BookNode) do
    begin
      ChapterNode := BookNode.GetFirstChild;
      while Assigned(ChapterNode) do
      begin
        RefreshNodeCaption(Scope, ChapterNode);
        ChapterNode := ChapterNode.GetNextSibling;
      end;
      RefreshNodeCaption(Scope, BookNode);
      BookNode := BookNode.GetNextSibling;
    end;
  finally
    FTree.EndUpdate;
  end;
end;

procedure TBibleTreeCoordinator.UpdateReferenceStatus(const Scope: TEscopoTexto;
  const ReferenceID: string; Status: Integer;
  const StatusDefinitions: TStatusDefinitions;
  AggregateImageResolver: TStatusAggregateImageResolver);
var
  VerseNode: TTreeNode;
  ChapterNode: TTreeNode;
  BookNode: TTreeNode;
begin
  if not Assigned(FTree) then
    Exit;

  VerseNode := FindVerseNode(Scope, ReferenceID);
  if not Assigned(VerseNode) then
    Exit;

  ChapterNode := VerseNode.Parent;
  BookNode := ChapterNode.Parent;

  FTree.BeginUpdate;
  try
    if VerseNode.ImageIndex <> Status then
      SetNodeStatus(VerseNode, Status);

    ApplyAggregateStatus(Scope, ChapterNode, StatusDefinitions,
      AggregateImageResolver);
    ApplyAggregateStatus(Scope, BookNode, StatusDefinitions,
      AggregateImageResolver);
  finally
    FTree.EndUpdate;
  end;
  FTree.Invalidate;
end;

procedure TBibleTreeCoordinator.SelectReference(const Scope: TEscopoTexto;
  const ReferenceID: string);
var
  VerseNode: TTreeNode;
begin
  VerseNode := FindVerseNode(Scope, ReferenceID);
  if Assigned(VerseNode) then
    VerseNode.Selected := True;
end;

end.
