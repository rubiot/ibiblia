unit bible_tree_coordinator;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, ComCtrls, BibleScope;

type
  TTreeStatusCounts = array[0..3] of Integer;

  { IVerseStatusCursor — the coordinator iterates this instead of calling
    back into the owner via four separate procedure-of-object variables. }
  IVerseStatusCursor = interface
    procedure Reset;
    function EOF: Boolean;
    procedure Advance;
    function CurrentStatus: Integer;
  end;

  TBibleTreeCoordinator = class
  private
    FTree: TTreeView;
    function BaseNodeText(const Scope: TEscopoTexto; Node: TTreeNode): string;
    procedure CountVerseStatuses(Node: TTreeNode; out Counts: TTreeStatusCounts;
      out Total: Integer);
    function FindVerseNode(const Scope: TEscopoTexto; const ReferenceID: string): TTreeNode;
    function ResolveNodePath(const Scope: TEscopoTexto; BookID, Chapter,
      Verse: Integer): TTreeNode;
    procedure ApplyAggregateStatus(const Scope: TEscopoTexto; ParentNode: TTreeNode);
    procedure RefreshNodeCaption(const Scope: TEscopoTexto; Node: TTreeNode);
    procedure SetNodeStatus(Node: TTreeNode; Status: Integer);
  public
    constructor Create(ATree: TTreeView = nil);
    procedure Attach(ATree: TTreeView);
    procedure Populate(const Scope: TEscopoTexto; OnChange: TTVChangedEvent);
    function BuildStatusHint(const Scope: TEscopoTexto; Node: TTreeNode;
      const StatusCaptions: array of string): string;
    function NodeToReferenceID(const Scope: TEscopoTexto; const Node: TTreeNode): string;
    procedure RecomputeAllStatuses(const Scope: TEscopoTexto;
      Cursor: IVerseStatusCursor);
    procedure RefreshCaptions(const Scope: TEscopoTexto);
    procedure UpdateReferenceStatus(const Scope: TEscopoTexto;
      const ReferenceID: string; Status: Integer);
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

procedure TBibleTreeCoordinator.CountVerseStatuses(Node: TTreeNode;
  out Counts: TTreeStatusCounts; out Total: Integer);
var
  ChildNode: TTreeNode;
  ChildCounts: TTreeStatusCounts;
  ChildTotal: Integer;
  StatusIndex: Integer;
begin
  Counts[0] := 0;
  Counts[1] := 0;
  Counts[2] := 0;
  Counts[3] := 0;
  Total := 0;

  if not Assigned(Node) then
    Exit;

  if not Node.HasChildren then
  begin
    StatusIndex := Node.ImageIndex;
    if (StatusIndex >= 0) and (StatusIndex <= 3) then
      Inc(Counts[StatusIndex]);
    Inc(Total);
    Exit;
  end;

  ChildNode := Node.GetFirstChild;
  while Assigned(ChildNode) do
  begin
    CountVerseStatuses(ChildNode, ChildCounts, ChildTotal);
    for StatusIndex := 0 to 3 do
      Inc(Counts[StatusIndex], ChildCounts[StatusIndex]);
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
  OnChange: TTVChangedEvent);
var
  BookIndex: SmallInt;
  ChapterIndex: SmallInt;
  VerseIndex: SmallInt;
  BookNode: TTreeNode;
  ChapterNode: TTreeNode;
  VerseNode: TTreeNode;
begin
  if not Assigned(FTree) then
    Exit;

  if FTree.Items.Count = 0 then
  begin
    FTree.BeginUpdate;
    try
      for BookIndex := 0 to QLivros[Scope] - 1 do
      begin
        BookNode := FTree.Items.Add(nil, NLivros[Scope][BookIndex]);
        BookNode.Data := Pointer(PtrUInt(BookIndex + OffsetLivros[Scope] + 1));
        BookNode.ImageIndex := 0;
        BookNode.SelectedIndex := BookNode.ImageIndex + 4;
        for ChapterIndex := 0 to QCapitulos[Scope][BookIndex] - 1 do
        begin
          ChapterNode := FTree.Items.AddChild(BookNode, IntToStr(ChapterIndex + 1));
          ChapterNode.Data := Pointer(PtrUInt(ChapterIndex + 1));
          ChapterNode.ImageIndex := 0;
          ChapterNode.SelectedIndex := ChapterNode.ImageIndex + 4;
          for VerseIndex := 0 to QVersiculos[Scope][QCapLivros[Scope][BookIndex] + ChapterIndex - 1] - 1 do
          begin
            VerseNode := FTree.Items.AddChild(ChapterNode, IntToStr(VerseIndex + 1));
            VerseNode.Data := Pointer(PtrUInt(VerseIndex + 1));
            VerseNode.ImageIndex := 0;
            VerseNode.SelectedIndex := VerseNode.ImageIndex + 4;
          end;
          ApplyAggregateStatus(Scope, ChapterNode);
        end;
        ApplyAggregateStatus(Scope, BookNode);
      end;
    finally
      FTree.EndUpdate;
    end;
  end;

  FTree.OnChange := OnChange;
end;

function TBibleTreeCoordinator.BuildStatusHint(const Scope: TEscopoTexto;
  Node: TTreeNode; const StatusCaptions: array of string): string;

  function StatusCaption(StatusIndex: Integer): string;
  begin
    if (StatusIndex >= Low(StatusCaptions)) and (StatusIndex <= High(StatusCaptions)) then
      Result := StatusCaptions[StatusIndex]
    else
      Result := Format('Status %d', [StatusIndex + 1]);
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
  Counts: TTreeStatusCounts;
  Total: Integer;
  StatusIndex: Integer;
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

  CountVerseStatuses(Node, Counts, Total);
  Result := Format('%s%sTotal: %d', [NodeTitle, LineEnding, Total]);
  for StatusIndex := 0 to 3 do
    Result := Result + LineEnding +
      Format('%s: %d', [StatusCaption(StatusIndex), Counts[StatusIndex]]);
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
  ParentNode: TTreeNode);
var
  Counts: array[0..3] of SmallInt;
  ChildNode: TTreeNode;
  StatusIndex: SmallInt;
  TotalChildren: Integer;
  HasUniformStatus: Boolean;
begin
  if not Assigned(ParentNode) then
    Exit;

  Counts[0] := 0;
  Counts[1] := 0;
  Counts[2] := 0;
  Counts[3] := 0;
  TotalChildren := 0;
  ChildNode := ParentNode.GetFirstChild;
  while Assigned(ChildNode) do
  begin
    if (ChildNode.ImageIndex >= 0) and (ChildNode.ImageIndex <= 3) then
      Inc(Counts[ChildNode.ImageIndex]);
    Inc(TotalChildren);
    ChildNode := ChildNode.GetNextSibling;
  end;

  HasUniformStatus := False;
  if TotalChildren > 0 then
    for StatusIndex := 0 to 3 do
      if Counts[StatusIndex] = TotalChildren then
      begin
        ParentNode.ImageIndex := StatusIndex;
        ParentNode.SelectedIndex := ParentNode.ImageIndex + 4;
        HasUniformStatus := True;
        RefreshNodeCaption(Scope, ParentNode);
        Break;
      end;

  if HasUniformStatus then
    Exit;

  if Counts[2] > 0 then
    ParentNode.ImageIndex := 2
  else
    ParentNode.ImageIndex := 1;
  ParentNode.SelectedIndex := ParentNode.ImageIndex + 4;
  RefreshNodeCaption(Scope, ParentNode);
end;

procedure TBibleTreeCoordinator.SetNodeStatus(Node: TTreeNode; Status: Integer);
begin
  if not Assigned(Node) then
    Exit;

  Node.ImageIndex := Status;
  Node.SelectedIndex := Node.ImageIndex + 4;
end;

procedure TBibleTreeCoordinator.RecomputeAllStatuses(const Scope: TEscopoTexto;
  Cursor: IVerseStatusCursor);
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

        ApplyAggregateStatus(Scope, ChapterNode);
        ChapterNode := ChapterNode.GetNextSibling;
      end;

      ApplyAggregateStatus(Scope, BookNode);
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
  const ReferenceID: string; Status: Integer);
var
  VerseNode: TTreeNode;
  ChapterNode: TTreeNode;
  BookNode: TTreeNode;
begin
  VerseNode := FindVerseNode(Scope, ReferenceID);
  if not Assigned(VerseNode) then
    Exit;

  ChapterNode := VerseNode.Parent;
  BookNode := ChapterNode.Parent;

  if VerseNode.ImageIndex <> Status then
    SetNodeStatus(VerseNode, Status);

  ApplyAggregateStatus(Scope, ChapterNode);
  ApplyAggregateStatus(Scope, BookNode);
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
