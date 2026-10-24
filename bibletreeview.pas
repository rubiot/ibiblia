unit BibleTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, fgl, Projeto, LResources, unitskins;

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
    procedure TreeViewAdvancedCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure TreeViewCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectAll;
    procedure SelectNone;
    procedure SetVerseList(refs: TStringList);
    procedure UpdateSkin(const ASkin: TSkinColors);

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

function CreateCheckboxBitmap(State: Integer; const ASkin: TSkinColors): TBitmap;
var
  rBox: TRect;
begin
  Result := TBitmap.Create;
  Result.SetSize(16, 16);
  Result.Canvas.Brush.Color := clFuchsia;
  Result.Canvas.FillRect(0, 0, 16, 16);

  rBox := Rect(1, 1, 14, 14);

  if ASkin.IsDark then
  begin
    Result.Canvas.Brush.Style := bsSolid;
    Result.Canvas.Brush.Color := ASkin.PanelBg;
    Result.Canvas.FillRect(rBox);

    Result.Canvas.Pen.Color := ASkin.TreeText; // Light/bright border conforming to theme
    Result.Canvas.Pen.Width := 1;
    Result.Canvas.Rectangle(rBox);

    if State = 1 then // Checked
    begin
      Result.Canvas.Pen.Color := MakeRGB($2A, $A1, $98); // Bright teal/cyan checkmark
      Result.Canvas.Pen.Width := 2;
      Result.Canvas.Polyline([Point(3, 7), Point(6, 11), Point(12, 4)]);
    end
    else if State = 2 then // Partial
    begin
      Result.Canvas.Brush.Color := ASkin.TreeText;
      Result.Canvas.FillRect(Rect(4, 4, 11, 11));
    end;
  end
  else
  begin
    Result.Canvas.Brush.Style := bsSolid;
    Result.Canvas.Brush.Color := clWhite;
    Result.Canvas.FillRect(rBox);

    Result.Canvas.Pen.Color := clGray;
    Result.Canvas.Rectangle(rBox);

    if State = 1 then
    begin
      Result.Canvas.Pen.Color := clGreen;
      Result.Canvas.Pen.Width := 2;
      Result.Canvas.Polyline([Point(3, 7), Point(6, 11), Point(12, 4)]);
    end
    else if State = 2 then
    begin
      Result.Canvas.Brush.Color := clGray;
      Result.Canvas.FillRect(Rect(4, 4, 11, 11));
    end;
  end;
end;


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
              result.Add(Format('%d,%d,%d', [PtrInt(nb.Data), PtrInt(nc.Data), PtrInt(nv.Data)]));
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
              result.Add(PtrInt(nv.Data));
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
    nb.Data := Pointer(PtrInt(b + OffsetLivros[FScope] + 1));
    for c:=0 to QCapitulos[FScope][b]-1 do
    begin
      nc := Items.AddChild(nb, Format(SChapterFmt, [c+1]));
      nc.StateIndex := 0;
      nc.Data := Pointer(PtrInt(c+1));
      for v:=0 to QVersiculos[FScope][QCapLivros[FScope][b] + c - 1]-1 do
      begin
        nv := Items.AddChild(nc, IntToStr(v+1));
        nv.StateIndex := 0;
        nv.Data := Pointer(PtrInt(v+1));
      end;
    end;
  end;
  EndUpdate;
end;

procedure TBibleTreeView.SetVerseList(refs: TStringList);
var
  ref: string;
  b, c, v, i: integer;
  nb, nc: TTreeNode;
begin
  BeginUpdate;
  Items.Clear;
  nb := nil;
  nc := nil;
  i := 0;
  for ref in refs do
  begin
    Sscanf(ref, '%d,%d,%d', [@b, @c, @v]);

    if (nb = nil) or (PtrInt(nb.Data) <> b) then
    begin
      nb := Items.Add(nil, NLivros[etONT][b-1]);
      nb.Data := Pointer(PtrInt(b));
      nc := nil;
    end;

    if (nc = nil) or (PtrInt(nc.Data) <> c) then
    begin
      nc := Items.AddChild(nb, Format(SChapterFmt, [c]));
      nc.Data := Pointer(PtrInt(c));
    end;

    Items.AddChild(nc, v.ToString).Data := Pointer(PtrInt(i));
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
      FOnVerseChange(PtrInt(node.Data));
  end;
end;

procedure TBibleTreeView.CreateImageList;
var
  bmp: TBitmap;
  iState: Integer;
begin
  if not Assigned(FImages) then
    FImages := TImageList.Create(Self);
  FImages.Clear;
  for iState := 0 to 2 do
  begin
    bmp := CreateCheckboxBitmap(iState, CurrentSkin);
    try
      FImages.AddMasked(bmp, clFuchsia);
    finally
      bmp.Free;
    end;
  end;
  StateImages := FImages;
end;

procedure TBibleTreeView.UpdateSkin(const ASkin: TSkinColors);
var
  bmp: TBitmap;
  iState: Integer;
begin
  ParentColor := False;
  Color := ASkin.TreeBg;
  Font.Color := ASkin.TreeText;
  ExpandSignColor := ASkin.TreeText;
  TreeLineColor := ASkin.PanelBorder;
  ExpandSignType := tvestArrowFill;

  if not Assigned(FImages) then
    FImages := TImageList.Create(Self);
  FImages.Clear;
  for iState := 0 to 2 do
  begin
    bmp := CreateCheckboxBitmap(iState, ASkin);
    try
      FImages.AddMasked(bmp, clFuchsia);
    finally
      bmp.Free;
    end;
  end;
  StateImages := FImages;
  Invalidate;
end;

procedure TBibleTreeView.TreeViewAdvancedCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if (Stage = cdPrePaint) and CurrentSkin.IsDark then
  begin
    Sender.Canvas.Brush.Color := CurrentSkin.TreeBg;
    Sender.Canvas.FillRect(ARect);
  end;
  DefaultDraw := True;
end;

procedure TBibleTreeView.TreeViewCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  rRow, rText: TRect;
  imgX, imgY, yPos, arrowLeft, arrowTop: Integer;
begin
  rRow := Node.DisplayRect(False);
  rText := Node.DisplayRect(True);

  // 1. Fill row background with dark TreeBg (or Highlight/Pointed)
  if (rRow.Right > rRow.Left) and (rRow.Bottom > rRow.Top) then
  begin
    Sender.Canvas.Brush.Style := bsSolid;
    if (cdsSelected in State) then
      Sender.Canvas.Brush.Color := CurrentSkin.HighlightBg
    else if (cdsHot in State) then
      Sender.Canvas.Brush.Color := CurrentSkin.PointedBg
    else
      Sender.Canvas.Brush.Color := CurrentSkin.TreeBg;
    Sender.Canvas.FillRect(rRow);
  end;

  // 2. Draw expand/collapse arrow icon before checkbox if node has children
  if Node.HasChildren then
  begin
    if Assigned(StateImages) then
      imgX := rText.Left - StateImages.Width - 2
    else
      imgX := rText.Left - 18;
    arrowLeft := imgX - 13;
    arrowTop := rRow.Top + (rRow.Height - 5) div 2;

    Sender.Canvas.Brush.Style := bsSolid;
    Sender.Canvas.Brush.Color := CurrentSkin.TreeText;
    Sender.Canvas.Pen.Color := CurrentSkin.TreeText;
    Sender.Canvas.Pen.Width := 1;

    if Node.Expanded then
    begin
      // Down arrow ▼
      Sender.Canvas.Polygon([
        Point(arrowLeft, arrowTop),
        Point(arrowLeft + 8, arrowTop),
        Point(arrowLeft + 4, arrowTop + 4)
      ]);
    end
    else
    begin
      // Right arrow ▶
      Sender.Canvas.Polygon([
        Point(arrowLeft + 1, arrowTop - 1),
        Point(arrowLeft + 6, arrowTop + 3),
        Point(arrowLeft + 1, arrowTop + 7)
      ]);
    end;
  end;

  // 3. Draw the light/themed checkbox at its proper location
  if Assigned(StateImages) and (Node.StateIndex >= 0) and (Node.StateIndex < StateImages.Count) then
  begin
    imgX := rText.Left - StateImages.Width - 2;
    imgY := rRow.Top + (rRow.Height - StateImages.Height) div 2;
    StateImages.Draw(Sender.Canvas, imgX, imgY, Node.StateIndex);
  end;

  // 3. Draw the node text in bright CurrentSkin.TreeText
  if (rText.Right > rText.Left) and (rText.Bottom > rText.Top) then
  begin
    Sender.Canvas.Brush.Style := bsClear;
    if (cdsSelected in State) then
      Sender.Canvas.Font.Color := CurrentSkin.HighlightText
    else if (cdsHot in State) then
      Sender.Canvas.Font.Color := CurrentSkin.PointedText
    else
    begin
      if CurrentSkin.IsDark then
        Sender.Canvas.Font.Color := CurrentSkin.TreeText
      else
        Sender.Canvas.Font.Color := clWindowText;
    end;

    yPos := rText.Top + (rText.Height - Sender.Canvas.TextHeight(Node.Text)) div 2;
    Sender.Canvas.TextOut(rText.Left + 2, yPos, Node.Text);
  end;

  DefaultDraw := False;
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
  OnCustomDrawItem := @TreeViewCustomDrawItem;
  OnAdvancedCustomDraw := @TreeViewAdvancedCustomDraw;
  ParentColor := False;
  Color := CurrentSkin.TreeBg;
  Font.Color := CurrentSkin.TreeText;
  ExpandSignColor := CurrentSkin.TreeText;
  TreeLineColor := CurrentSkin.PanelBorder;
  ExpandSignType := tvestArrowFill;
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

