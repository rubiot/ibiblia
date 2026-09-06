unit FormApplyPatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, BibleTreeView, PatchFile, Verse, Project, CommentsView,
  ONTTokenizer;

type

  { TFrmApplyPatch }

  TFrmApplyPatch = class(TForm)
    BtnDeselectAll: TButton;
    BtnSelectAll: TButton;
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    LabelSrcText: TLabel;
    LabelDstText: TLabel;
    LabelAssociations: TLabel;
    LabelComments: TLabel;
    LabelStatus: TLabel;
    MemoComments: TMemo;
    ScrollBoxSrcText: TScrollBox;
    ScrollBoxDstText: TScrollBox;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TabControlPreview: TTabControl;
    procedure BtnDeselectAllClick(Sender: TObject);
    procedure BtnSelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabControlPreviewChange(Sender: TObject);
  private
    FBibleTreeView: TBibleTreeView;
    FPatch: TPatchFile;
    FIndex: integer; // index of current verse in patch
    FSrcVerse: TVerse;
    FDstVerse: TVerse;
    FActualCommentsMemo: TCommentsView; // Dynamic replacement for TMemo
    procedure OnVerseChange(index: integer);
    procedure LoadControlsFromPatch;
    procedure LoadControlsFromProject;
    procedure UpdateApplySelection(Sender: TObject);
  public
    function LoadPatch(filename: string): boolean;
    procedure Apply;
  end;

var
  FrmApplyPatch: TFrmApplyPatch;

resourcestring
  SPatchSuccessfullyApplied = 'Patch successfully applied';
  SIgnoredVersesOnPatch = '%d verses are identical to what you have in your project and were ignored.';
  SInfo = 'Information';
  SNoUpdatedVersesOnPatch = 'The content of this patch is identical to what you have in your project. The patch will not be applied.';

implementation

uses formPrincipal;

{ TFrmApplyPatch }

procedure TFrmApplyPatch.BtnSelectAllClick(Sender: TObject);
begin
  FBibleTreeView.SelectAll;
  UpdateApplySelection(Sender);
end;

procedure TFrmApplyPatch.BtnDeselectAllClick(Sender: TObject);
begin
  FBibleTreeView.SelectNone;
  UpdateApplySelection(Sender);
end;

procedure TFrmApplyPatch.UpdateApplySelection(Sender: TObject);
var
  Indexes: TIntegerList;
begin
  Indexes := FBibleTreeView.IndexesList;
  try
    Button1.Enabled := Indexes.Count > 0;
  finally
    Indexes.Free;
  end;
end;

procedure TFrmApplyPatch.FormCreate(Sender: TObject);
var
  tokenizerVersion: Integer;
begin
  FBibleTreeView := TBibleTreeView.Create(GroupBox1);
  FBibleTreeView.Align := alClient;
  FBibleTreeView.OnVerseChange := @OnVerseChange;
  FBibleTreeView.OnSelectionChanged := @UpdateApplySelection;
  FPatch := nil;

  FSrcVerse := TVerse.Create(ScrollBoxSrcText);
  FDstVerse := TVerse.Create(ScrollBoxDstText);
  FSrcVerse.PairedVerse := FDstVerse;
  FSrcVerse.ReadOnly := true;
  FDstVerse.ReadOnly := true;
  if Assigned(CurrentProject) then
    tokenizerVersion := CurrentProject.TokenizerVersion
  else
    tokenizerVersion := TTokenizerFactory.PreferredVersion;
  FSrcVerse.TokenizerVersion := tokenizerVersion;
  FDstVerse.TokenizerVersion := tokenizerVersion;

  // Replace the TMemo with TCommentsView
  FActualCommentsMemo := TCommentsView.Create(Self);
  FActualCommentsMemo.Parent := MemoComments.Parent;
  FActualCommentsMemo.Align := MemoComments.Align;
  FActualCommentsMemo.TabOrder := MemoComments.TabOrder;
  FActualCommentsMemo.ReadOnly := True; // Disable editing in patch preview
  MemoComments.Visible := False;
end;

procedure TFrmApplyPatch.FormDestroy(Sender: TObject);
begin
  FSrcVerse.Free;
  FDstVerse.Free;
  FBibleTreeView.Free;
  FActualCommentsMemo.Free; // Free the dynamically created TCommentsView
  if assigned(FPatch) then
    FPatch.Free;
end;

procedure TFrmApplyPatch.FormShow(Sender: TObject);
begin
  FSrcVerse.Font := CurrentProject.GetTextFont(tbOrigem);
  FDstVerse.Font := CurrentProject.GetTextFont(tbDestino);
  FSrcVerse.BoldWordsWithStrong := CurrentProject.PalavrasComStrongEmNegrito;
  FSrcVerse.StrongsCountMode := CurrentProject.MostrarQtdStrongs;
  FDstVerse.StrongsCountMode := CurrentProject.MostrarQtdStrongs;
  FSrcVerse.Active := true;
  FDstVerse.Active := true;

  // Configure FActualCommentsMemo with appropriate font settings
  FActualCommentsMemo.FontName := 'default';
  FActualCommentsMemo.FontSize := 10;
end;

procedure TFrmApplyPatch.TabControlPreviewChange(Sender: TObject);

  procedure SetLabelColor(lbl: TLabel; isEqual: boolean);
  begin
    if isEqual then
      lbl.Color := clGreen
    else
      lbl.Color := clRed;
  end;

begin
  case TabControlPreview.TabIndex of
    0: LoadControlsFromPatch;
    1: LoadControlsFromProject;
  end;
  SetLabelColor(LabelSrcText,      CurrentProject.GetVerseText(FPatch.Reference[FIndex], tbOrigem ) = FPatch.SourceText[FIndex]);
  SetLabelColor(LabelDstText,      CurrentProject.GetVerseText(FPatch.Reference[FIndex], tbDestino) = FPatch.DestinationText[FIndex]);
  SetLabelColor(LabelAssociations, CurrentProject.GetPairs           (FPatch.Reference[FIndex]           ) = FPatch.Pairs[FIndex]);
  SetLabelColor(LabelComments,     CurrentProject.GetComments        (FPatch.Reference[FIndex]           ) = FPatch.Comments[FIndex]);
  SetLabelColor(LabelStatus,       IntToStr(CurrentProject.GetStatus(FPatch.Reference[FIndex])           ) = FPatch.Status[FIndex]);
end;

procedure TFrmApplyPatch.OnVerseChange(index: integer);
begin
  if index = FIndex then
    exit;

  FIndex := index;

  TabControlPreviewChange(TabControlPreview);
end;

procedure TFrmApplyPatch.LoadControlsFromPatch;
begin
  FSrcVerse.Text := FPatch.SourceText[FIndex];
  FDstVerse.Text := FPatch.DestinationText[FIndex];
  FSrcVerse.Pairs := FPatch.Pairs[FIndex];
  FActualCommentsMemo.SetCommentsText(FPatch.Comments[FIndex]);
end;

procedure TFrmApplyPatch.LoadControlsFromProject;
begin
  FSrcVerse.Text := CurrentProject.GetVerseText(FPatch.Reference[FIndex], tbOrigem);
  FDstVerse.Text := CurrentProject.GetVerseText(FPatch.Reference[FIndex], tbDestino);
  FSrcVerse.Pairs := CurrentProject.GetPairs(FPatch.Reference[FIndex]);
  FActualCommentsMemo.SetCommentsText(CurrentProject.GetComments(FPatch.Reference[FIndex]));
end;

function TFrmApplyPatch.LoadPatch(filename: string): boolean;
begin
  if assigned(FPatch) then
    FPatch.Free;

  FPatch := TPatchFile.Create(filename, CurrentProject);
  if FPatch.SourceText.Count = 0 then
  begin
    MessageDlg(SInfo, SNoUpdatedVersesOnPatch, mtInformation, [mbOK], 0);
    result := false;
    Exit;
  end;

  if FPatch.IgnoredVerses > 0 then
    MessageDlg(SInfo, Format(SIgnoredVersesOnPatch, [FPatch.IgnoredVerses]), mtInformation, [mbOK], 0);

  FBibleTreeView.VerseList := FPatch.Reference;
  UpdateApplySelection(nil);
  FIndex := 0;
  TabControlPreviewChange(TabControlPreview);
  result := true;
end;

procedure TFrmApplyPatch.Apply;
var
  indexes: TIntegerList;
  AppliedCount: Integer;
begin
  AppliedCount := 0;
  indexes := FBibleTreeView.IndexesList;
  try
    if indexes.Count = 0 then
      Exit;
    AppliedCount := CurrentProject.ApplyProjectPatch(FPatch, indexes);
  finally
    indexes.Free;
    FreeAndNil(FPatch);
  end;
  if AppliedCount > 0 then
    ShowMessage(SPatchSuccessfullyApplied);
end;

initialization
{$IFNDEF FPCUNIT}
  {$I FormApplyPatch.lrs}
{$ENDIF}

end.

