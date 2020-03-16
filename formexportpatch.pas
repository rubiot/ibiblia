unit formexportpatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Projeto, LazLogger;

type

  { TFormExportPatch }

  TFormExportPatch = class(TForm)
    Button1: TButton;
    Button2: TButton;
    BtnSelectAll: TButton;
    BtnDeselectAll: TButton;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    TreeView1: TTreeView;
    procedure BtnDeselectAllClick(Sender: TObject);
    procedure BtnSelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    FScope: TEscopoTexto;
    procedure SetScope(AValue: TEscopoTexto);

  public
    function GetVerseList: TStringList;
    property Scope: TEscopoTexto read FScope write SetScope;
  end;

{$INCLUDE include/bookdefs.inc}

var
  FrmExportPatch: TFormExportPatch;

implementation

uses
  LCLIntf;

const
  ImgIndexUnchecked = 0;
  ImgIndexChecked = 1;
  ImgIndexPartial = 2;

procedure SetNodeStateIndex(node: TTreeNode; index: integer);
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

procedure ToggleTreeViewCheckBoxes(node: TTreeNode);
var
  parent: TTreeNode;
begin
  if not assigned(node) then exit;

  if node.StateIndex = ImgIndexUnchecked then
  begin
    SetNodeStateIndex(node, ImgIndexChecked);
    parent := node.Parent;
    while assigned(parent) do
    begin
      parent.StateIndex := ImgIndexPartial;
      parent := parent.Parent;
    end;
  end
  else if node.StateIndex = ImgIndexChecked then
  begin
    SetNodeStateIndex(node, ImgIndexUnchecked);
    parent := node.Parent;
    while assigned(parent) do
    begin
      parent.StateIndex := ImgIndexPartial;
      parent := parent.Parent;
    end;
  end
  else if node.StateIndex = ImgIndexPartial then
  begin
    SetNodeStateIndex(node, ImgIndexUnchecked);
  end;
end;

{ TFormExportPatch }

procedure TFormExportPatch.TreeView1Click(Sender: TObject);
var
  P: TPoint;
  node: TTreeNode;
  ht: THitTests;
begin
  GetCursorPos(P);
  P := TreeView1.ScreenToClient(P);
  ht := TreeView1.GetHitTestInfoAt(P.X, P.Y);
  if (htOnStateIcon in ht) then begin
    node := TreeView1.GetNodeAt(P.X, P.Y);
    ToggleTreeViewCheckBoxes(node);
  end;
end;

procedure TFormExportPatch.SetScope(AValue: TEscopoTexto);
var
  l, c, v: smallint;
  nl, nc, nv: TTreeNode;
begin
  if FScope = AValue then Exit;
  FScope := AValue;

  TreeView1.Items.Clear;
  for l:=0 to QLivros[FScope]-1 do
  begin
    nl := TreeView1.Items.Add(nil, NLivros[FScope][l]);
    nl.StateIndex := 0;
    nl.Data := Pointer(l + OffsetLivros[FScope] + 1);
    for c:=0 to QCapitulos[FScope][l]-1 do
    begin
      nc := TreeView1.Items.AddChild(nl, Format('Chapter %d', [c+1]));
      nc.StateIndex := 0;
      nc.Data := Pointer(c+1);
      for v:=0 to QVersiculos[FScope][QCapLivros[FScope][l] + c - 1]-1 do
      begin
        nv := TreeView1.Items.AddChild(nc, IntToStr(v+1));
        nv.StateIndex := 0;
        nv.Data := Pointer(v+1);
      end;
    end;
  end;
end;

function TFormExportPatch.GetVerseList: TStringList;
var
  nb, nc, nv: TTreeNode;
begin
  //DebugLn('====== Generating verse list');
  result := TStringList.Create;
  nb := TreeView1.Items[0];
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

procedure TFormExportPatch.FormCreate(Sender: TObject);
begin
  FScope := etNone;
end;

procedure TFormExportPatch.BtnSelectAllClick(Sender: TObject);
var
  node: TTreeNode;
begin
  node := TreeView1.Items[0];
  while assigned(node) do
  begin
    SetNodeStateIndex(node, ImgIndexChecked);
    node := node.GetNextSibling;
  end;
end;

procedure TFormExportPatch.BtnDeselectAllClick(Sender: TObject);
var
  node: TTreeNode;
begin
  node := TreeView1.Items[0];
  while assigned(node) do
  begin
    SetNodeStateIndex(node, ImgIndexUnchecked);
    node := node.GetNextSibling;
  end;
end;

initialization
  {$I formexportpatch.lrs}

end.

