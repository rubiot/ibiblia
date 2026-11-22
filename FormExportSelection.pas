{ SPDX-License-Identifier: AGPL-3.0-or-later
  Copyright (C) 2026 Edmund P. Valdez

  The "which books do you want?" dialog of Export project: the same check
  tree as Create patch (books, chapters, verses), listing what the main
  Bible tree lists, with Select all and Deselect all. }
unit FormExportSelection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Graphics, Dialogs,
  ComCtrls, BibleTreeView, Project, ui_style, unitskins;

{ Fills Tree with the books, chapters and verses of the open project, as the
  main Bible tree lists them (apocrypha books included, deleted ones left
  out, in the order the user arranged). }
procedure FillBibleTreeFromProject(Tree: TBibleTreeView; AProject: TProject);

{ Shows the dialog. Selected holds the chosen verses ("book,chapter,verse")
  and is Sorted; empty on entry means everything is ticked. Returns False
  when the user cancels (Selected is not changed). Total gets the number of
  verses the tree has. }
function ChooseExportVerses(AProject: TProject; Selected: TStringList;
  out Total: Integer): Boolean;

implementation

resourcestring
  SExportSelectionTitle = 'Books to export';
  SExportSelectionSelectAll = 'Select all';
  SExportSelectionDeselectAll = 'Deselect all';
  SExportSelectionOK = 'OK';
  SExportSelectionCancel = 'Cancel';
  SExportSelectionNothing = 'Tick at least one verse to export.';

procedure FillBibleTreeFromProject(Tree: TBibleTreeView; AProject: TProject);
var
  Refs, Parts: TStringList;
  I, Book, Chapter, Verse, LastBook, LastChapter: Integer;
  BookNode, ChapterNode: TTreeNode;
begin
  Refs := TStringList.Create;
  Parts := TStringList.Create;
  Parts.Delimiter := ',';
  Parts.StrictDelimiter := True;
  Tree.BeginUpdate;
  try
    AProject.GetTreeReferences(Refs);
    Tree.ClearStructure;
    LastBook := -1;
    LastChapter := -1;
    BookNode := nil;
    ChapterNode := nil;
    for I := 0 to Refs.Count - 1 do
    begin
      Parts.DelimitedText := Refs[I];
      if Parts.Count < 3 then
        Continue;
      Book := StrToIntDef(Parts[0], 0);
      Chapter := StrToIntDef(Parts[1], 0);
      Verse := StrToIntDef(Parts[2], 0);
      if (Book <= 0) or (Chapter <= 0) or (Verse <= 0) then
        Continue;
      if (Book <> LastBook) or (BookNode = nil) then
      begin
        BookNode := Tree.AddBookNode(AProject.TreeBookName(Book), Book);
        LastBook := Book;
        LastChapter := -1;
      end;
      if (Chapter <> LastChapter) or (ChapterNode = nil) then
      begin
        ChapterNode := Tree.AddChapterNode(BookNode, Chapter);
        LastChapter := Chapter;
      end;
      Tree.AddVerseNode(ChapterNode, Verse);
    end;
  finally
    Tree.EndUpdate;
    Parts.Free;
    Refs.Free;
  end;
end;

type
  { TExportSelectionForm }

  TExportSelectionForm = class(TForm)
  private
    FTree: TBibleTreeView;
    FBottom: TPanel;
    FSelectAll, FDeselectAll, FOK, FCancel: TButton;
    procedure SelectAllClick(Sender: TObject);
    procedure DeselectAllClick(Sender: TObject);
    procedure OKClick(Sender: TObject);
  public
    constructor Make(AOwner: TComponent);
    property Tree: TBibleTreeView read FTree;
  end;

constructor TExportSelectionForm.Make(AOwner: TComponent);
const
  Margin = 16;
  Gap = 8;
  ButtonHeight = 32;
begin
  inherited CreateNew(AOwner);
  Caption := SExportSelectionTitle;
  BorderStyle := bsSizeable;
  Position := poMainFormCenter;
  ClientWidth := UIScale(Self, 600);
  ClientHeight := Trunc(Screen.WorkAreaHeight * 0.75);
  Constraints.MinWidth := UIScale(Self, 540);
  Constraints.MinHeight := UIScale(Self, 320);

  FBottom := TPanel.Create(Self);
  FBottom.Parent := Self;
  FBottom.Align := alBottom;
  FBottom.BevelOuter := bvNone;
  FBottom.Height := UIScale(Self, ButtonHeight + Margin * 2);
  FBottom.Caption := '';

  FSelectAll := TButton.Create(Self);
  FSelectAll.Parent := FBottom;
  FSelectAll.SetBounds(UIScale(Self, Margin), UIScale(Self, Margin),
    UIScale(Self, 128), UIScale(Self, ButtonHeight));
  FSelectAll.Caption := SExportSelectionSelectAll;
  FSelectAll.OnClick := @SelectAllClick;

  FDeselectAll := TButton.Create(Self);
  FDeselectAll.Parent := FBottom;
  FDeselectAll.SetBounds(FSelectAll.Left + FSelectAll.Width + UIScale(Self, Gap),
    UIScale(Self, Margin), UIScale(Self, 152), UIScale(Self, ButtonHeight));
  FDeselectAll.Caption := SExportSelectionDeselectAll;
  FDeselectAll.OnClick := @DeselectAllClick;

  FCancel := TButton.Create(Self);
  FCancel.Parent := FBottom;
  FCancel.Anchors := [akTop, akRight];
  FCancel.SetBounds(FBottom.ClientWidth - UIScale(Self, Margin + 104),
    UIScale(Self, Margin), UIScale(Self, 104), UIScale(Self, ButtonHeight));
  FCancel.Caption := SExportSelectionCancel;
  FCancel.ModalResult := mrCancel;
  FCancel.Cancel := True;

  FOK := TButton.Create(Self);
  FOK.Parent := FBottom;
  FOK.Anchors := [akTop, akRight];
  FOK.SetBounds(FCancel.Left - UIScale(Self, Gap + 104), UIScale(Self, Margin),
    UIScale(Self, 104), UIScale(Self, ButtonHeight));
  FOK.Caption := SExportSelectionOK;
  FOK.Default := True;
  FOK.OnClick := @OKClick;

  FTree := TBibleTreeView.Create(Self);
  FTree.Parent := Self;
  FTree.Align := alClient;
  FTree.BorderSpacing.Around := UIScale(Self, Margin);
  ApplyDialogStyle(Self);
end;

procedure TExportSelectionForm.SelectAllClick(Sender: TObject);
begin
  FTree.SelectAll;
end;

procedure TExportSelectionForm.DeselectAllClick(Sender: TObject);
begin
  FTree.SelectNone;
end;

procedure TExportSelectionForm.OKClick(Sender: TObject);
var
  Chosen: TStringList;
begin
  Chosen := FTree.VerseList;
  try
    if Chosen.Count = 0 then
    begin
      MessageDlg(SExportSelectionTitle, SExportSelectionNothing, mtInformation, [mbOK], 0);
      Exit;
    end;
  finally
    Chosen.Free;
  end;
  ModalResult := mrOK;
end;

function CountVerses(Tree: TBibleTreeView): Integer;
var
  Node: TTreeNode;
begin
  Result := 0;
  for Node in Tree.Items do
    if Node.Level = 2 then
      Inc(Result);
end;

function ChooseExportVerses(AProject: TProject; Selected: TStringList;
  out Total: Integer): Boolean;
var
  Dialog: TExportSelectionForm;
  Chosen: TStringList;
begin
  Result := False;
  Total := 0;
  Dialog := TExportSelectionForm.Make(Application);
  try
    FillBibleTreeFromProject(Dialog.Tree, AProject);
    Total := CountVerses(Dialog.Tree);
    if Assigned(Selected) and (Selected.Count > 0) then
      Dialog.Tree.CheckReferences(Selected)
    else
      Dialog.Tree.SelectAll;
    ApplySkinToForm(Dialog, CurrentSkin);
    Dialog.Tree.ApplySkinColors;
    if Dialog.ShowModal <> mrOK then
      Exit;
    Chosen := Dialog.Tree.VerseList;
    try
      Selected.Clear;
      Selected.Sorted := True;
      Selected.Duplicates := dupIgnore;
      Selected.AddStrings(Chosen);
    finally
      Chosen.Free;
    end;
    Result := True;
  finally
    Dialog.Free;
  end;
end;

end.
