unit BibleTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, fgl, Projeto, LResources;

const
  ImgIndexUnchecked = 0;
  ImgIndexChecked = 1;
  ImgIndexPartial = 2;

type

  TOnVerseChangeEvent = procedure (index: integer) of object;
  TIntegerList = specialize TFPGList<integer>;

  { TBibleTreeView }

  TBibleTreeView = class(TTreeView)
  private
    FScope: TEscopoTexto;
    FImages: TImageList;
    FOnVerseChange: TOnVerseChangeEvent;
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

    property Scope: TEscopoTexto read FScope write SetScope;
    property VerseList: TStringList read GetVerseList write SetVerseList;
    property IndexesList: TIntegerList read GetVerseIndexes;
    property OnVerseChange: TOnVerseChangeEvent read FOnVerseChange write FOnVerseChange;
  end;

{$INCLUDE include/bookdefs.inc}

resourcestring
  SChapterFmt = 'Chapter %d';

implementation

uses
  LCLIntf;

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
begin
  //DebugLn('====== Generating verse indexes list');
  result := TIntegerList.Create;
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
end;

procedure TBibleTreeView.SetVerseList(refs: TStringList);
var
  ref: string;
  b, c, v, i: integer;
  nb, nc: TTreeNode;
begin
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
      nc := nil;
    end;

    if (nc = nil) or (nc.Data <> Pointer(c)) then
    begin
      nc := Items.AddChild(nb, Format(SChapterFmt, [c]));
      nc.Data := Pointer(c);
    end;

    Items.AddChild(nc, v.ToString).Data := Pointer(i);
    Inc(i);
  end;
  SelectAll;
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

begin
  FImages := TImageList.Create(self);
  AddPngFromRes('checkbox-unchecked');
  AddPngFromRes('checkbox-checked');
  AddPngFromRes('checkbox-partial');
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

initialization
  {$I bibletreeview.lrs}

end.

