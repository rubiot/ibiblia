unit BibleTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, fgl, BibleScope, LResources;

const
  ImgIndexUnchecked = 0;
  ImgIndexChecked = 1;
  ImgIndexPartial = 2;
  ImgIndexAIPending = 3;     // AI processing pending (hourglass)
  ImgIndexAIProcessing = 4;  // AI processing in progress (spinner)
  ImgIndexAISuccess = 5;     // AI processing succeeded (green check)
  ImgIndexAIError = 6;       // AI processing failed (red x)


type

  TOnVerseChangeEvent = procedure (index: integer) of object;
  TIntegerList = specialize TFPGList<integer>;

  { TBibleTreeView }

  TBibleTreeView = class(TTreeView)
  private
    FScope: TEscopoTexto;
    FImages: TImageList;
    FOnVerseChange: TOnVerseChangeEvent;
    FAIStatesEnabled: Boolean; // Enable/disable AI state icons
    function GetVerseList: TStringList;
    function GetVerseIndexes: TIntegerList;
    procedure SetNodeStateIndex(node: TTreeNode; index: integer);
    procedure SetScope(AValue: TEscopoTexto);
    procedure ToggleTreeViewCheckBoxes(node: TTreeNode);
    procedure TreeViewClick(Sender: TObject);
    procedure CreateImageList;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectAll;
    procedure SelectNone;
    procedure SetVerseList(refs: TStringList);
    procedure SetVerseAIState(index: integer; state: integer); // Set AI state icon for verse by index
    procedure ClearAllAIStates; // Reset all verses to checked/unchecked state


    property Scope: TEscopoTexto read FScope write SetScope;
    property VerseList: TStringList read GetVerseList write SetVerseList;
    property IndexesList: TIntegerList read GetVerseIndexes;
    property OnVerseChange: TOnVerseChangeEvent read FOnVerseChange write FOnVerseChange;
    property AIStatesEnabled: Boolean read FAIStatesEnabled write FAIStatesEnabled;
  end;

{$INCLUDE include/bookdefs.inc}

resourcestring
  SChapterFmt = 'Chapter %d';

implementation

uses
  LCLIntf, ai_debug;

{ TBibleTreeView }

procedure TBibleTreeView.SetNodeStateIndex(node: TTreeNode; index: integer);
var
  child: TTreeNode;
begin
  node.StateIndex := index;
  child := node.GetFirstChild;
  while assigned(child) do
  begin
    SetNodeStateIndex(child, index);
    child := child.GetNextSibling;
  end;
end;

function TBibleTreeView.GetVerseList: TStringList;
var
  nb, nc, nv: TTreeNode;
begin
  //DebugLn('====== Generating verse list');
  result := TStringList.Create;
  nb := Items[0];
  while assigned(nb) do
  begin
    if nb.StateIndex <> ImgIndexUnchecked then
    begin
      nc := nb.GetFirstChild;
      while assigned(nc) do
      begin
        if nc.StateIndex <> ImgIndexUnchecked then
        begin
          nv := nc.GetFirstChild;
          while assigned(nv) do
          begin
            if nv.StateIndex = ImgIndexChecked then
            begin
              result.Add(Format('%d,%d,%d', [Integer(nb.Data), Integer(nc.Data), Integer(nv.Data)]));
              //DebugLn('Adding verse %s', [result[result.Count-1]]);
            end;
            nv := nv.GetNextSibling;
          end;
        end;
        nc := nc.GetNextSibling;
      end;
    end;
    nb := nb.GetNextSibling;
  end;
  //DebugLn('====== Verse list generated');
end;

function TBibleTreeView.GetVerseIndexes: TIntegerList;
var
  nb, nc, nv: TTreeNode;
  verseIndex: Integer;
begin
  //DebugLn('====== Generating verse indexes list');
  result := TIntegerList.Create;
  verseIndex := 0;
  nb := Items[0];
  while assigned(nb) do
  begin
    if nb.StateIndex <> ImgIndexUnchecked then
    begin
      nc := nb.GetFirstChild;
      while assigned(nc) do
      begin
        if nc.StateIndex <> ImgIndexUnchecked then
        begin
          nv := nc.GetFirstChild;
          while assigned(nv) do
          begin
            // Debug: Track all verse nodes and their state
            // Include verse if explicitly checked OR if it has an AI processing state
            // (ImgIndexAIPending=3, ImgIndexAIProcessing=4, ImgIndexAISuccess=5, ImgIndexAIError=6)
            if (nv.StateIndex = ImgIndexChecked) or 
               ((nv.StateIndex >= 3) and (nv.StateIndex <= 6)) then
            begin
              result.Add(Integer(nv.Data));
              //DebugLn('Adding verse %s', [result[result.Count-1]]);
            end;
            nv := nv.GetNextSibling;
          end;
        end;
        nc := nc.GetNextSibling;
      end;
    end;
    nb := nb.GetNextSibling;
  end;
  //DebugLn('====== Verse indexes list generated');
end;

procedure TBibleTreeView.SetScope(AValue: TEscopoTexto);
var
  b, c, v: smallint;
  nb, nc, nv: TTreeNode;
begin
  if FScope = AValue then Exit;
  FScope := AValue;

  BeginUpdate;
  Items.Clear;
  for b:=0 to QLivros[FScope]-1 do
  begin
    nb := Items.Add(nil, NLivros[FScope][b]);
    nb.StateIndex := 0;
    nb.Data := Pointer(b + OffsetLivros[FScope] + 1);
    for c:=0 to QCapitulos[FScope][b]-1 do
    begin
      nc := Items.AddChild(nb, Format(SChapterFmt, [c+1]));
      nc.StateIndex := 0;
      nc.Data := Pointer(c+1);
      for v:=0 to QVersiculos[FScope][QCapLivros[FScope][b] + c - 1]-1 do
      begin
        nv := Items.AddChild(nc, IntToStr(v+1));
        nv.StateIndex := 0;
        nv.Data := Pointer(v+1);
      end;
    end;
  end;
  EndUpdate;
end;

procedure TBibleTreeView.SetVerseList(refs: TStringList);
var
  ref: string;
  b, c, v, i: integer;
  nb, nc, nv: TTreeNode;
begin
  BeginUpdate;
  Items.Clear;
  nb := nil;
  nc := nil;
  i := 0;
  for ref in refs do
  begin
    Sscanf(ref, '%d,%d,%d', [@b, @c, @v]);

    if (nb = nil) or (nb.Data <> Pointer(b)) then
    begin
      nb := Items.Add(nil, NLivros[etONT][b-1]);
      nb.Data := Pointer(b);
      nb.StateIndex := ImgIndexUnchecked;  // Initialize book node state
      nc := nil;
    end;

    if (nc = nil) or (nc.Data <> Pointer(c)) then
    begin
      nc := Items.AddChild(nb, Format(SChapterFmt, [c]));
      nc.Data := Pointer(c);
      nc.StateIndex := ImgIndexUnchecked;  // Initialize chapter node state
    end;

    nv := Items.AddChild(nc, v.ToString);
    nv.Data := Pointer(i);
    nv.StateIndex := ImgIndexUnchecked;  // Initialize verse node state
    Inc(i);
  end;
  SelectAll;
  EndUpdate;
end;

procedure TBibleTreeView.ToggleTreeViewCheckBoxes(node: TTreeNode);
var
  nparent: TTreeNode;
begin
  if not assigned(node) then exit;

  if node.StateIndex = ImgIndexUnchecked then
  begin
    SetNodeStateIndex(node, ImgIndexChecked);
    nparent := node.Parent;
    while assigned(nparent) do
    begin
      nparent.StateIndex := ImgIndexPartial;
      nparent := nparent.Parent;
    end;
  end
  else if node.StateIndex = ImgIndexChecked then
  begin
    SetNodeStateIndex(node, ImgIndexUnchecked);
    nparent := node.Parent;
    while assigned(nparent) do
    begin
      nparent.StateIndex := ImgIndexPartial;
      nparent := nparent.Parent;
    end;
  end
  else if node.StateIndex = ImgIndexPartial then
  begin
    SetNodeStateIndex(node, ImgIndexUnchecked);
  end;
end;

procedure TBibleTreeView.TreeViewClick(Sender: TObject);
var
  P: TPoint;
  node: TTreeNode;
  ht: THitTests;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  ht := GetHitTestInfoAt(P.X, P.Y);
  if (htOnStateIcon in ht) then begin // checkbox
    node := GetNodeAt(P.X, P.Y);
    ToggleTreeViewCheckBoxes(node);
  end else if (htOnLabel in ht) and assigned(FOnVerseChange) then begin // node
    node := GetNodeAt(P.X, P.Y);
    if assigned(node.Parent) and assigned(node.Parent.Parent) then
      FOnVerseChange(Integer(node.Data));
  end;
end;

procedure TBibleTreeView.CreateImageList;
  procedure AddPngFromRes(res: string);
  var
    SrcBmp: TBitmap;
    Picture: TPicture;
  begin
    Picture := TPicture.Create;
    SrcBmp := TBitmap.Create;
    try
      Picture.LoadFromLazarusResource(res);
      SrcBmp.Assign(Picture.Graphic);
      FImages.Add(SrcBmp, nil);
    finally
      Picture.Free;
    end;
  end;

  procedure AddColoredSquare(color: TColor);
  var
    bmp: TBitmap;
  begin
    bmp := TBitmap.Create;
    try
      bmp.Width := 16;
      bmp.Height := 16;
      bmp.Canvas.Brush.Color := color;
      bmp.Canvas.FillRect(0, 0, 16, 16);
      FImages.Add(bmp, nil);
    finally
      bmp.Free;
    end;
  end;

begin
  FImages := TImageList.Create(self);
  AddPngFromRes('checkbox-unchecked');
  AddPngFromRes('checkbox-checked');
  AddPngFromRes('checkbox-partial');
  // AI state icons (using placeholder colors for now)
  AddColoredSquare(clYellow);  // ImgIndexAIPending - pending
  AddColoredSquare(clAqua);    // ImgIndexAIProcessing - processing
  AddColoredSquare(clLime);    // ImgIndexAISuccess - success
  AddColoredSquare(clRed);     // ImgIndexAIError - error
  StateImages := FImages;
end;

constructor TBibleTreeView.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);

  Parent := TWinControl(AnOwner);
  ReadOnly := true;
  CreateImageList;
  FScope := etNone;
  OnClick := @TreeViewClick;
  FOnVerseChange := nil;
  FAIStatesEnabled := False;
end;

destructor TBibleTreeView.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

procedure TBibleTreeView.SelectAll;
var
  node: TTreeNode;
begin
  node := Items[0];
  while assigned(node) do
  begin
    SetNodeStateIndex(node, ImgIndexChecked);
    node := node.GetNextSibling;
  end;
end;

procedure TBibleTreeView.SelectNone;
var
  node: TTreeNode;
begin
  node := Items[0];
  while assigned(node) do
  begin
    SetNodeStateIndex(node, ImgIndexUnchecked);
    node := node.GetNextSibling;
  end;
end;

procedure TBibleTreeView.SetVerseAIState(index: integer; state: integer);
var
  nb, nc, nv: TTreeNode;
  currentIndex: Integer;
begin
  AIDebugLogFmt('[SetVerseAIState] Called: index=%d, state=%d, AIStatesEnabled=%d', 
    [index, state, Ord(FAIStatesEnabled)]);
  
  if not FAIStatesEnabled then Exit;
  
  // Find verse node by index
  currentIndex := 0;
  nb := Items[0];
  while assigned(nb) do
  begin
    nc := nb.GetFirstChild;
    while assigned(nc) do
    begin
      nv := nc.GetFirstChild;
      while assigned(nv) do
      begin
        if currentIndex = index then
        begin
          AIDebugLogFmt('[SetVerseAIState] Found node at index %d, setting StateIndex to %d', [index, state]);
          nv.StateIndex := state;
          Exit;
        end;
        Inc(currentIndex);
        nv := nv.GetNextSibling;
      end;
      nc := nc.GetNextSibling;
    end;
    nb := nb.GetNextSibling;
  end;
  
  AIDebugLogFmt('[SetVerseAIState] WARNING: Node not found for index %d (total counted: %d)', [index, currentIndex]);
end;

procedure TBibleTreeView.ClearAllAIStates;
var
  nb, nc, nv: TTreeNode;
begin
  // Convert AI state icons back to checkboxes:
  // - Success (green) -> Checked (for user to review and optionally uncheck)
  // - Error (red) -> Unchecked (failed verses not applied by default)
  // - Pending/Processing -> Checked (incomplete jobs marked for retry)
  AIDebugLog('[ClearAllAIStates] Converting AI states to checkboxes');
  nb := Items[0];
  while assigned(nb) do
  begin
    nc := nb.GetFirstChild;
    while assigned(nc) do
    begin
      nv := nc.GetFirstChild;
      while assigned(nv) do
      begin
        if nv.StateIndex = ImgIndexAISuccess then
          nv.StateIndex := ImgIndexChecked // Success -> checked (user can uncheck)
        else if nv.StateIndex = ImgIndexAIError then
          nv.StateIndex := ImgIndexUnchecked // Error -> unchecked (user can check to retry)
        else if (nv.StateIndex = ImgIndexAIPending) or (nv.StateIndex = ImgIndexAIProcessing) then
          nv.StateIndex := ImgIndexChecked; // Incomplete -> checked
        nv := nv.GetNextSibling;
      end;
      nc := nc.GetNextSibling;
    end;
    nb := nb.GetNextSibling;
  end;
  FAIStatesEnabled := False; // Disable AI state mode
  AIDebugLog('[ClearAllAIStates] AI states cleared, checkboxes restored');
end;

initialization
{$IFNDEF FPCUNIT}
  {$I bibletreeview.lrs}
{$ENDIF}

end.

