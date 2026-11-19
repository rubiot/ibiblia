unit bible_tree_coordinator;

{$mode objfpc}{$H+}
{$WARN 4055 OFF}
{$WARN 4056 OFF}
{$WARN 4082 OFF}

interface

uses
  SysUtils, Classes, fgl, ComCtrls, BibleScope, status_catalog, bible_book_names,
  apocrypha_books;

{ A chapter or verse node keeps who it really is in Node.Data, so that it can
  be dragged to another place in the tree (even under another chapter or book)
  and still be the same chapter or verse:
    chapter node: book shl 40 or chapter shl 20
    verse node:   book shl 40 or chapter shl 20 or verse
  A book node keeps the plain book number. }
function TreeChapterKey(Book, Chapter: Integer): PtrUInt;
function TreeVerseKey(Book, Chapter, Verse: Integer): PtrUInt;
function TreeKeyBook(Key: PtrUInt): Integer;
function TreeKeyChapter(Key: PtrUInt): Integer;
function TreeKeyVerse(Key: PtrUInt): Integer;

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
    FKeyMap: specialize TFPGMap<PtrUInt, TTreeNode>;
    FCanon: array of PtrUInt;      // every verse key in the standard order
    FCanonText: string;            // the standard order as order text
    FExisting: TStringList;        // "b,c,v" of the rows; nil = show every verse
    function HasRow(Book, Chapter, Verse: Integer): Boolean;
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
    procedure RebuildKeyMap;
    function FindKeyNode(Key: PtrUInt): TTreeNode;
    function FindBookNode(BookID: Integer): TTreeNode;
    function BuildOrderText: string;
    function BuildCanonicalText: string;
    procedure RecomputeParents(const Scope: TEscopoTexto;
      const StatusDefinitions: TStatusDefinitions;
      AggregateImageResolver: TStatusAggregateImageResolver);
  public
    destructor Destroy; override;
    constructor Create(ATree: TTreeView = nil);
    { After books, chapters or verses were deleted the tree lists only what
      is still in the project. Nil: every verse of the Bible is listed. }
    procedure SetExistingReferences(Refs: TStrings);
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
    function FindNodeByReference(const ReferenceID: string): TTreeNode;
    { The arrangement of books, chapters and verses made by the user. }
    function CurrentOrderText: string;       // '' while everything is in the standard order
    function ApplyOrderText(const OrderText: string;
      const Scope: TEscopoTexto; const StatusDefinitions: TStatusDefinitions;
      AggregateImageResolver: TStatusAggregateImageResolver = nil): Boolean;
    procedure ResetOrder(const Scope: TEscopoTexto;
      const StatusDefinitions: TStatusDefinitions;
      AggregateImageResolver: TStatusAggregateImageResolver = nil);
    { Drag and drop: where would Node land if it were dropped at Y?  Returns
      False when that is not allowed. }
    function CanMoveNode(Node, Target: TTreeNode; Mode: TNodeAttachMode): Boolean;
    function MoveNode(Node, Target: TTreeNode; Mode: TNodeAttachMode;
      const Scope: TEscopoTexto; const StatusDefinitions: TStatusDefinitions;
      AggregateImageResolver: TStatusAggregateImageResolver = nil): Boolean;
  end;

implementation

const
  LAST_OT_BOOK_ID = 39;   // Malachi; the apocrypha books follow it

{$INCLUDE include/bookdefs.inc}

const
  KEY_BOOK_SHIFT = 40;
  KEY_CHAPTER_SHIFT = 20;
  KEY_PART_MASK = $FFFFF;

function TreeChapterKey(Book, Chapter: Integer): PtrUInt;
begin
  Result := (PtrUInt(Book) shl KEY_BOOK_SHIFT) or
    (PtrUInt(Chapter) shl KEY_CHAPTER_SHIFT);
end;

function TreeVerseKey(Book, Chapter, Verse: Integer): PtrUInt;
begin
  Result := TreeChapterKey(Book, Chapter) or PtrUInt(Verse);
end;

function TreeKeyBook(Key: PtrUInt): Integer;
begin
  Result := Integer(Key shr KEY_BOOK_SHIFT);
end;

function TreeKeyChapter(Key: PtrUInt): Integer;
begin
  Result := Integer((Key shr KEY_CHAPTER_SHIFT) and KEY_PART_MASK);
end;

function TreeKeyVerse(Key: PtrUInt): Integer;
begin
  Result := Integer(Key and KEY_PART_MASK);
end;

type
  TOrderChapter = record
    Key: PtrUInt;
    Verses: array of PtrUInt;
  end;
  TOrderBook = record
    Book: Integer;
    Chapters: array of TOrderChapter;
  end;
  TOrderBooks = array of TOrderBook;

destructor TBibleTreeCoordinator.Destroy;
begin
  FExisting.Free;
  FKeyMap.Free;
  inherited Destroy;
end;

constructor TBibleTreeCoordinator.Create(ATree: TTreeView);
begin
  inherited Create;
  Attach(ATree);
end;

procedure TBibleTreeCoordinator.Attach(ATree: TTreeView);
begin
  FTree := ATree;
end;

procedure TBibleTreeCoordinator.SetExistingReferences(Refs: TStrings);
begin
  FreeAndNil(FExisting);
  if Refs = nil then
    Exit;
  FExisting := TStringList.Create;
  FExisting.Sorted := True;
  FExisting.Duplicates := dupIgnore;
  FExisting.AddStrings(Refs);
end;

function TBibleTreeCoordinator.HasRow(Book, Chapter, Verse: Integer): Boolean;
begin
  Result := (FExisting = nil) or
    (FExisting.IndexOf(Format('%d,%d,%d', [Book, Chapter, Verse])) >= 0);
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
          Result := LocalizedBookName(Scope, BookIndex)
        else
          Result := Node.Text;
      end;
    1:
      if NodeValue > 0 then
        Result := IntToStr(TreeKeyChapter(NodeValue))
      else
        Result := Node.Text;
    2:
      if NodeValue > 0 then
        Result := IntToStr(TreeKeyVerse(NodeValue))
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

  { only a verse is a verse: a chapter or book that has been emptied by a drag
    counts as nothing }
  if Node.Level >= 2 then
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
  ApocryphaAdded: Boolean;
  { Apocrypha / deuterocanonical books (67+): after Malachi and before
    Matthew, in the order of the imported source text. }
  procedure AddApocryphaBooks;
  var
    ApocIndex: Integer;
    Apoc: TApocryphaBook;
    ChapterIndex, VerseIndex: SmallInt;
    BookNode, ChapterNode, VerseNode: TTreeNode;
  begin
    if Scope = etNT then
      Exit;
    for ApocIndex := 0 to ApocryphaBookCount - 1 do
    begin
      Apoc := ApocryphaBookAt(ApocIndex);
      BookNode := FTree.Items.Add(nil, Apoc.Name);
      BookNode.Data := Pointer(PtrUInt(Apoc.BookID));
      SetNodeStatus(BookNode, InitialStatus);
      for ChapterIndex := 0 to High(Apoc.VerseCounts) do
      begin
        ChapterNode := FTree.Items.AddChild(BookNode, IntToStr(ChapterIndex + 1));
        ChapterNode.Data := Pointer(TreeChapterKey(Apoc.BookID, ChapterIndex + 1));
        SetNodeStatus(ChapterNode, InitialStatus);
        for VerseIndex := 0 to Apoc.VerseCounts[ChapterIndex] - 1 do
        begin
          if not HasRow(Apoc.BookID, ChapterIndex + 1, VerseIndex + 1) then
            Continue;
          VerseNode := FTree.Items.AddChild(ChapterNode, IntToStr(VerseIndex + 1));
          VerseNode.Data := Pointer(TreeVerseKey(Apoc.BookID, ChapterIndex + 1,
            VerseIndex + 1));
          SetNodeStatus(VerseNode, InitialStatus);
        end;
        if ChapterNode.Count = 0 then
        begin
          ChapterNode.Delete;
          Continue;
        end;
        ApplyAggregateStatus(Scope, ChapterNode, StatusDefinitions,
          AggregateImageResolver);
      end;
      if BookNode.Count = 0 then
      begin
        BookNode.Delete;
        Continue;
      end;
      ApplyAggregateStatus(Scope, BookNode, StatusDefinitions,
        AggregateImageResolver);
    end;
  end;

begin
  if not Assigned(FTree) then
    Exit;

  InitialStatus := FirstStatusId(StatusDefinitions);
  ApocryphaAdded := False;

  if FTree.Items.Count = 0 then
  begin
    FTree.BeginUpdate;
    try
      for BookIndex := 0 to QLivros[Scope] - 1 do
      begin
        { the apocrypha books go between Malachi and Matthew }
        if (not ApocryphaAdded) and
           (BookIndex + OffsetLivros[Scope] + 1 > LAST_OT_BOOK_ID) then
        begin
          AddApocryphaBooks;
          ApocryphaAdded := True;
        end;
        BookNode := FTree.Items.Add(nil, LocalizedBookName(Scope, BookIndex));
        BookNode.Data := Pointer(PtrUInt(BookIndex + OffsetLivros[Scope] + 1));
        SetNodeStatus(BookNode, InitialStatus);
        for ChapterIndex := 0 to QCapitulos[Scope][BookIndex] - 1 do
        begin
          ChapterNode := FTree.Items.AddChild(BookNode, IntToStr(ChapterIndex + 1));
          ChapterNode.Data := Pointer(TreeChapterKey(
            BookIndex + OffsetLivros[Scope] + 1, ChapterIndex + 1));
          SetNodeStatus(ChapterNode, InitialStatus);
          for VerseIndex := 0 to QVersiculos[Scope][QCapLivros[Scope][BookIndex] + ChapterIndex - 1] - 1 do
          begin
            if not HasRow(BookIndex + OffsetLivros[Scope] + 1, ChapterIndex + 1,
              VerseIndex + 1) then
              Continue;
            VerseNode := FTree.Items.AddChild(ChapterNode, IntToStr(VerseIndex + 1));
            VerseNode.Data := Pointer(TreeVerseKey(
              BookIndex + OffsetLivros[Scope] + 1, ChapterIndex + 1, VerseIndex + 1));
            SetNodeStatus(VerseNode, InitialStatus);
          end;
          if ChapterNode.Count = 0 then
          begin
            ChapterNode.Delete;
            Continue;
          end;
          ApplyAggregateStatus(Scope, ChapterNode, StatusDefinitions,
            AggregateImageResolver);
        end;
        if BookNode.Count = 0 then
        begin
          BookNode.Delete;
          Continue;
        end;
        ApplyAggregateStatus(Scope, BookNode, StatusDefinitions,
          AggregateImageResolver);
      end;
      if not ApocryphaAdded then
        AddApocryphaBooks;
      RebuildKeyMap;
      FCanonText := BuildOrderText;
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
  begin
    Result := '';
    if not Assigned(Node) then
      Exit;

    case Node.Level of
      0:
        Result := BaseNodeText(Scope, Node);
      1:
        begin
          { the chapter's own book, even if it was dragged under another one }
          BookNode := FindBookNode(TreeKeyBook(PtrUInt(Node.Data)));
          if not Assigned(BookNode) then
            BookNode := Node.Parent;
          Result := Format('%s %s', [
            BaseNodeText(Scope, BookNode),
            BaseNodeText(Scope, Node)
          ]);
        end;
      2:
        begin
          BookNode := FindBookNode(TreeKeyBook(PtrUInt(Node.Data)));
          if not Assigned(BookNode) then
            BookNode := Node.Parent.Parent;
          Result := Format('%s %d:%s', [
            BaseNodeText(Scope, BookNode),
            TreeKeyChapter(PtrUInt(Node.Data)),
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

  if Node.Level >= 2 then
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
begin
  Result := nil;
  if not Assigned(FTree) or (FTree.Items.Count = 0) then
    Exit;
  Result := FindKeyNode(TreeVerseKey(BookID, Chapter, Verse));
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

  BookID := 0;
  Chapter := 0;
  Verse := 0;
  Sscanf(ReferenceID, '%d,%d,%d', [@BookID, @Chapter, @Verse]);
  Result := ResolveNodePath(Scope, BookID, Chapter, Verse);
end;

function TBibleTreeCoordinator.FindNodeByReference(
  const ReferenceID: string): TTreeNode;
begin
  Result := FindVerseNode(etONT, ReferenceID);
end;

procedure TBibleTreeCoordinator.RebuildKeyMap;
var
  Node: TTreeNode;
  Count: Integer;
begin
  if not Assigned(FKeyMap) then
  begin
    FKeyMap := specialize TFPGMap<PtrUInt, TTreeNode>.Create;
    FKeyMap.Sorted := False;
  end;
  FKeyMap.Clear;
  SetLength(FCanon, 0);
  if not Assigned(FTree) then
    Exit;

  Count := 0;
  Node := FTree.Items.GetFirstNode;
  while Assigned(Node) do
  begin
    if Node.Level > 0 then
      FKeyMap.Add(PtrUInt(Node.Data), Node);
    if Node.Level = 2 then
    begin
      if Count = Length(FCanon) then
        SetLength(FCanon, Count + 4096);
      FCanon[Count] := PtrUInt(Node.Data);
      Inc(Count);
    end;
    Node := Node.GetNext;
  end;
  SetLength(FCanon, Count);
  FKeyMap.Sort;
  FKeyMap.Sorted := True;
end;

function TBibleTreeCoordinator.FindKeyNode(Key: PtrUInt): TTreeNode;
var
  Index: Integer;
begin
  Result := nil;
  if not Assigned(FTree) then
    Exit;
  if (not Assigned(FKeyMap)) or (FKeyMap.Count = 0) or
     (FKeyMap.Count <> FTree.Items.Count - FTree.Items.TopLvlCount) then
    RebuildKeyMap;
  if FKeyMap.Find(Key, Index) then
    Result := FKeyMap.Data[Index];
end;

function TBibleTreeCoordinator.FindBookNode(BookID: Integer): TTreeNode;
begin
  Result := nil;
  if not Assigned(FTree) then
    Exit;
  Result := FTree.Items.GetFirstNode;
  while Assigned(Result) and (PtrUInt(Result.Data) <> PtrUInt(BookID)) do
    Result := Result.GetNextSibling;
end;

function TBibleTreeCoordinator.NodeToReferenceID(const Scope: TEscopoTexto;
  const Node: TTreeNode): string;
var
  ChapterNode: TTreeNode;
  VerseNode: TTreeNode;
  Key: PtrUInt;
begin
  Result := '';
  if not Assigned(Node) then
    Exit;

  { a book or a chapter stands for its first verse, wherever that is now }
  case Node.Level of
    0:
      begin
        ChapterNode := Node.GetFirstChild;
        if Assigned(ChapterNode) then
          VerseNode := ChapterNode.GetFirstChild
        else
          VerseNode := nil;
      end;
    1: VerseNode := Node.GetFirstChild;
  else
    VerseNode := Node;
  end;

  if not Assigned(VerseNode) then
    Exit;
  Key := PtrUInt(VerseNode.Data);
  if TreeKeyBook(Key) > OffsetLivros[Scope] then
    Result := Format('%d,%d,%d', [TreeKeyBook(Key), TreeKeyChapter(Key),
      TreeKeyVerse(Key)]);
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

procedure TBibleTreeCoordinator.RecomputeParents(const Scope: TEscopoTexto;
  const StatusDefinitions: TStatusDefinitions;
  AggregateImageResolver: TStatusAggregateImageResolver);
var
  BookNode: TTreeNode;
  ChapterNode: TTreeNode;
begin
  BookNode := FTree.Items.GetFirstNode;
  while Assigned(BookNode) do
  begin
    ChapterNode := BookNode.GetFirstChild;
    while Assigned(ChapterNode) do
    begin
      ApplyAggregateStatus(Scope, ChapterNode, StatusDefinitions,
        AggregateImageResolver);
      ChapterNode := ChapterNode.GetNextSibling;
    end;
    ApplyAggregateStatus(Scope, BookNode, StatusDefinitions,
      AggregateImageResolver);
    BookNode := BookNode.GetNextSibling;
  end;
end;

procedure TBibleTreeCoordinator.RecomputeAllStatuses(const Scope: TEscopoTexto;
  Cursor: IVerseStatusCursor; const StatusDefinitions: TStatusDefinitions;
  AggregateImageResolver: TStatusAggregateImageResolver);
var
  I: Integer;
  VerseNode: TTreeNode;
begin
  if not Assigned(FTree) or (FTree.Items.Count = 0) or not Assigned(Cursor) then
    Exit;

  Cursor.Reset;

  FTree.BeginUpdate;
  try
    if (Length(FCanon) = 0) or (not Assigned(FKeyMap)) then
      RebuildKeyMap;

    { the project hands the statuses over in the standard order of the verses;
      the tree may have been arranged differently, so each status goes to the
      verse it belongs to }
    I := 0;
    while (I < Length(FCanon)) and not Cursor.EOF do
    begin
      VerseNode := FindKeyNode(FCanon[I]);
      if Assigned(VerseNode) then
        SetNodeStatus(VerseNode, Cursor.CurrentStatus);
      Cursor.Advance;
      Inc(I);
    end;

    RecomputeParents(Scope, StatusDefinitions, AggregateImageResolver);
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
  if Assigned(VerseNode) and (FTree.Selected <> VerseNode) then
    VerseNode.Selected := True;
end;

{ ---------------- the user's own arrangement ---------------- }

function TBibleTreeCoordinator.BuildOrderText: string;
var
  Lines: TStringList;
  BookNode, ChapterNode, VerseNode: TTreeNode;
  RunBook, RunChapter, RunFrom, RunTo: Integer;
  RunOpen: Boolean;

  procedure FlushRun;
  begin
    if not RunOpen then
      Exit;
    if RunFrom = RunTo then
      Lines.Add(Format('V %d:%d:%d', [RunBook, RunChapter, RunFrom]))
    else
      Lines.Add(Format('V %d:%d:%d-%d', [RunBook, RunChapter, RunFrom, RunTo]));
    RunOpen := False;
  end;

var
  Key: PtrUInt;
begin
  Result := '';
  if not Assigned(FTree) then
    Exit;
  Lines := TStringList.Create;
  try
    RunOpen := False;
    RunBook := 0;
    RunChapter := 0;
    RunFrom := 0;
    RunTo := 0;
    BookNode := FTree.Items.GetFirstNode;
    while Assigned(BookNode) do
    begin
      Lines.Add(Format('B %d', [Integer(PtrUInt(BookNode.Data))]));
      ChapterNode := BookNode.GetFirstChild;
      while Assigned(ChapterNode) do
      begin
        Key := PtrUInt(ChapterNode.Data);
        Lines.Add(Format('C %d:%d', [TreeKeyBook(Key), TreeKeyChapter(Key)]));
        VerseNode := ChapterNode.GetFirstChild;
        while Assigned(VerseNode) do
        begin
          Key := PtrUInt(VerseNode.Data);
          if RunOpen and (RunBook = TreeKeyBook(Key)) and
             (RunChapter = TreeKeyChapter(Key)) and
             (RunTo + 1 = TreeKeyVerse(Key)) then
            RunTo := TreeKeyVerse(Key)
          else
          begin
            FlushRun;
            RunBook := TreeKeyBook(Key);
            RunChapter := TreeKeyChapter(Key);
            RunFrom := TreeKeyVerse(Key);
            RunTo := RunFrom;
            RunOpen := True;
          end;
          VerseNode := VerseNode.GetNextSibling;
        end;
        FlushRun;
        ChapterNode := ChapterNode.GetNextSibling;
      end;
      BookNode := BookNode.GetNextSibling;
    end;
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TBibleTreeCoordinator.BuildCanonicalText: string;
begin
  Result := FCanonText;
end;

function TBibleTreeCoordinator.CurrentOrderText: string;
begin
  Result := BuildOrderText;
  if (FCanonText <> '') and (Result = FCanonText) then
    Result := '';
end;

function TBibleTreeCoordinator.CanMoveNode(Node, Target: TTreeNode;
  Mode: TNodeAttachMode): Boolean;
var
  NewParent: TTreeNode;
begin
  Result := False;
  if (not Assigned(Node)) or (not Assigned(Target)) or (Node = Target) then
    Exit;

  if Mode in [naAddChild, naAddChildFirst] then
    NewParent := Target
  else
    NewParent := Target.Parent;

  if Node.Level = 0 then
  begin
    { a book only moves among the books }
    if (Target.Level <> 0) or (Mode in [naAddChild, naAddChildFirst]) then
      Exit;
  end
  else
  begin
    { a chapter goes under a book, a verse under a chapter }
    if (not Assigned(NewParent)) or (NewParent.Level <> Node.Level - 1) then
      Exit;
    if NewParent = Node then
      Exit;
    { the chapter or book it leaves must keep at least one child }
    if (Node.Parent <> NewParent) and (Node.Parent.Count <= 1) then
      Exit;
  end;
  Result := True;
end;

function TBibleTreeCoordinator.MoveNode(Node, Target: TTreeNode;
  Mode: TNodeAttachMode; const Scope: TEscopoTexto;
  const StatusDefinitions: TStatusDefinitions;
  AggregateImageResolver: TStatusAggregateImageResolver): Boolean;
var
  OldParent, NewParent: TTreeNode;
  SavedOnChange: TTVChangedEvent;
begin
  Result := False;
  if not CanMoveNode(Node, Target, Mode) then
    Exit;

  OldParent := Node.Parent;
  SavedOnChange := FTree.OnChange;
  FTree.OnChange := nil;
  FTree.BeginUpdate;
  try
    Node.MoveTo(Target, Mode);
    NewParent := Node.Parent;
    if Assigned(OldParent) then
    begin
      ApplyAggregateStatus(Scope, OldParent, StatusDefinitions,
        AggregateImageResolver);
      if Assigned(OldParent.Parent) then
        ApplyAggregateStatus(Scope, OldParent.Parent, StatusDefinitions,
          AggregateImageResolver);
    end;
    if Assigned(NewParent) and (NewParent <> OldParent) then
    begin
      ApplyAggregateStatus(Scope, NewParent, StatusDefinitions,
        AggregateImageResolver);
      if Assigned(NewParent.Parent) then
        ApplyAggregateStatus(Scope, NewParent.Parent, StatusDefinitions,
          AggregateImageResolver);
    end;
    Result := True;
  finally
    FTree.EndUpdate;
    FTree.OnChange := SavedOnChange;
  end;
  FTree.Invalidate;
end;

function TBibleTreeCoordinator.ApplyOrderText(const OrderText: string;
  const Scope: TEscopoTexto; const StatusDefinitions: TStatusDefinitions;
  AggregateImageResolver: TStatusAggregateImageResolver): Boolean;
var
  Lines: TStringList;
  Books: TOrderBooks;
  BookCount: Integer;
  I, P, Q, A, B, V, V1, V2: Integer;
  Line, Rest: string;
  SavedOnChange: TTVChangedEvent;

  procedure PutBook(ID: Integer);
  begin
    SetLength(Books, Length(Books) + 1);
    Books[High(Books)].Book := ID;
    SetLength(Books[High(Books)].Chapters, 0);
  end;

  procedure PutChapter(Key: PtrUInt);
  var
    BI: Integer;
  begin
    BI := High(Books);
    if BI < 0 then
      Exit;
    SetLength(Books[BI].Chapters, Length(Books[BI].Chapters) + 1);
    Books[BI].Chapters[High(Books[BI].Chapters)].Key := Key;
    SetLength(Books[BI].Chapters[High(Books[BI].Chapters)].Verses, 0);
  end;

  procedure PutVerse(Key: PtrUInt);
  var
    BI, CI, VI: Integer;
  begin
    BI := High(Books);
    if BI < 0 then
      Exit;
    CI := High(Books[BI].Chapters);
    if CI < 0 then
      Exit;
    VI := Length(Books[BI].Chapters[CI].Verses);
    SetLength(Books[BI].Chapters[CI].Verses, VI + 1);
    Books[BI].Chapters[CI].Verses[VI] := Key;
  end;

  { put Node as the child of Parent at position Prev + 1 (Prev = nil: first) }
  procedure Place(Node, Parent, Prev: TTreeNode);
  var
    Expected: TTreeNode;
  begin
    if not Assigned(Node) then
      Exit;
    if Assigned(Prev) then
      Expected := Prev.GetNextSibling
    else if Assigned(Parent) then
      Expected := Parent.GetFirstChild
    else
      Expected := FTree.Items.GetFirstNode;
    if Node = Expected then
      Exit;
    if Assigned(Prev) then
      Node.MoveTo(Prev, naInsertBehind)
    else if Assigned(Expected) then
      Node.MoveTo(Expected, naInsert)
    else
      Node.MoveTo(Parent, naAddChild);
  end;

var
  BI, CI, VI: Integer;
  BookNode, ChapterNode, PrevBook, PrevChapter, PrevVerse, VerseNode: TTreeNode;
begin
  Result := False;
  if (not Assigned(FTree)) or (FTree.Items.Count = 0) or (Trim(OrderText) = '') then
    Exit;

  SetLength(Books, 0);
  Lines := TStringList.Create;
  try
    Lines.Text := OrderText;
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if Length(Line) < 3 then
        Continue;
      Rest := Trim(Copy(Line, 2, MaxInt));
      case Line[1] of
        'B':
          begin
            A := StrToIntDef(Rest, 0);
            if A > 0 then
              PutBook(A);
          end;
        'C':
          begin
            P := Pos(':', Rest);
            if P > 0 then
            begin
              A := StrToIntDef(Copy(Rest, 1, P - 1), 0);
              B := StrToIntDef(Copy(Rest, P + 1, MaxInt), 0);
              if (A > 0) and (B > 0) then
                PutChapter(TreeChapterKey(A, B));
            end;
          end;
        'V':
          begin
            P := Pos(':', Rest);
            if P > 0 then
            begin
              A := StrToIntDef(Copy(Rest, 1, P - 1), 0);
              Rest := Copy(Rest, P + 1, MaxInt);
              P := Pos(':', Rest);
              if P > 0 then
              begin
                B := StrToIntDef(Copy(Rest, 1, P - 1), 0);
                Rest := Copy(Rest, P + 1, MaxInt);
                Q := Pos('-', Rest);
                if Q > 0 then
                begin
                  V1 := StrToIntDef(Copy(Rest, 1, Q - 1), 0);
                  V2 := StrToIntDef(Copy(Rest, Q + 1, MaxInt), 0);
                end
                else
                begin
                  V1 := StrToIntDef(Rest, 0);
                  V2 := V1;
                end;
                if (A > 0) and (B > 0) and (V1 > 0) then
                  for V := V1 to V2 do
                    PutVerse(TreeVerseKey(A, B, V));
              end;
            end;
          end;
      end;
    end;
  finally
    Lines.Free;
  end;
  BookCount := Length(Books);
  if BookCount = 0 then
    Exit;

  if (not Assigned(FKeyMap)) or (FKeyMap.Count = 0) then
    RebuildKeyMap;

  SavedOnChange := FTree.OnChange;
  FTree.OnChange := nil;
  FTree.BeginUpdate;
  try
    { 1. the books }
    PrevBook := nil;
    for BI := 0 to BookCount - 1 do
    begin
      BookNode := FindBookNode(Books[BI].Book);
      if not Assigned(BookNode) then
        Continue;
      Place(BookNode, nil, PrevBook);
      PrevBook := BookNode;
    end;

    { 2. the chapters, each under the book it was put in }
    for BI := 0 to BookCount - 1 do
    begin
      BookNode := FindBookNode(Books[BI].Book);
      if not Assigned(BookNode) then
        Continue;
      PrevChapter := nil;
      for CI := 0 to High(Books[BI].Chapters) do
      begin
        ChapterNode := FindKeyNode(Books[BI].Chapters[CI].Key);
        if not Assigned(ChapterNode) then
          Continue;
        Place(ChapterNode, BookNode, PrevChapter);
        PrevChapter := ChapterNode;
      end;
    end;

    { 3. the verses, each under the chapter it was put in }
    for BI := 0 to BookCount - 1 do
      for CI := 0 to High(Books[BI].Chapters) do
      begin
        ChapterNode := FindKeyNode(Books[BI].Chapters[CI].Key);
        if not Assigned(ChapterNode) then
          Continue;
        PrevVerse := nil;
        for VI := 0 to High(Books[BI].Chapters[CI].Verses) do
        begin
          VerseNode := FindKeyNode(Books[BI].Chapters[CI].Verses[VI]);
          if not Assigned(VerseNode) then
            Continue;
          Place(VerseNode, ChapterNode, PrevVerse);
          PrevVerse := VerseNode;
        end;
      end;

    RecomputeParents(Scope, StatusDefinitions, AggregateImageResolver);
    Result := True;
  finally
    FTree.EndUpdate;
    FTree.OnChange := SavedOnChange;
  end;
  FTree.Invalidate;
end;

procedure TBibleTreeCoordinator.ResetOrder(const Scope: TEscopoTexto;
  const StatusDefinitions: TStatusDefinitions;
  AggregateImageResolver: TStatusAggregateImageResolver);
begin
  if FCanonText <> '' then
    ApplyOrderText(FCanonText, Scope, StatusDefinitions, AggregateImageResolver);
end;

end.
