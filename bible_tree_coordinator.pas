unit bible_tree_coordinator;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, ComCtrls, BibleScope;

type
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
    function FindVerseNode(const Scope: TEscopoTexto; const ReferenceID: string): TTreeNode;
    function ResolveNodePath(const Scope: TEscopoTexto; BookID, Chapter,
      Verse: Integer): TTreeNode;
    procedure ApplyAggregateStatus(ParentNode: TTreeNode);
    procedure SetNodeStatus(Node: TTreeNode; Status: Integer);
  public
    constructor Create(ATree: TTreeView = nil);
    procedure Attach(ATree: TTreeView);
    procedure Populate(const Scope: TEscopoTexto; OnChange: TTVChangedEvent);
    function NodeToReferenceID(const Scope: TEscopoTexto; const Node: TTreeNode): string;
    procedure RecomputeAllStatuses(const Scope: TEscopoTexto;
      Cursor: IVerseStatusCursor);
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
        BookNode.ImageIndex := 0;
        BookNode.SelectedIndex := BookNode.ImageIndex + 4;
        for ChapterIndex := 0 to QCapitulos[Scope][BookIndex] - 1 do
        begin
          ChapterNode := FTree.Items.AddChild(BookNode, IntToStr(ChapterIndex + 1));
          ChapterNode.ImageIndex := 0;
          ChapterNode.SelectedIndex := ChapterNode.ImageIndex + 4;
          for VerseIndex := 0 to QVersiculos[Scope][QCapLivros[Scope][BookIndex] + ChapterIndex - 1] - 1 do
          begin
            VerseNode := FTree.Items.AddChild(ChapterNode, IntToStr(VerseIndex + 1));
            VerseNode.ImageIndex := 0;
            VerseNode.SelectedIndex := VerseNode.ImageIndex + 4;
          end;
        end;
      end;
    finally
      FTree.EndUpdate;
    end;
  end;

  FTree.OnChange := OnChange;
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
  BookIndex: Integer;
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

  for BookIndex := 0 to QLivros[Scope] - 1 do
    if NLivros[Scope][BookIndex] = BookNode.Text then
      Exit(Format('%d,%s,%s', [
        BookIndex + 1 + OffsetLivros[Scope],
        ChapterNode.Text,
        VerseNode.Text
      ]));
end;

procedure TBibleTreeCoordinator.ApplyAggregateStatus(ParentNode: TTreeNode);
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
        Break;
      end;

  if HasUniformStatus then
    Exit;

  if Counts[2] > 0 then
    ParentNode.ImageIndex := 2
  else
    ParentNode.ImageIndex := 1;
  ParentNode.SelectedIndex := ParentNode.ImageIndex + 4;
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

      ApplyAggregateStatus(ChapterNode);
      ChapterNode := ChapterNode.GetNextSibling;
    end;

    ApplyAggregateStatus(BookNode);
    BookNode := BookNode.GetNextSibling;
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

  ApplyAggregateStatus(ChapterNode);
  ApplyAggregateStatus(BookNode);
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