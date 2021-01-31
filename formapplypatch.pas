unit formapplypatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, BibleTreeView, PatchFile, Versiculo, Projeto;

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
    FSrcVerse: TVersiculo;
    FDstVerse: TVersiculo;
    procedure OnVerseChange(index: integer);
    procedure LoadControlsFromPatch;
    procedure LoadControlsFromProject;
  public
    function LoadPatch(filename: string): boolean;
    procedure Apply;
  end;

var
  FrmApplyPatch: TFrmApplyPatch;

resourcestring
  SPatchSuccessfullyApplied = 'Patch successfully applied';
  SIgnoredVersesOnPatch = '%d verses are the same as those in the project and were ignored';
  SInfo = 'Information';
  SNoUpdatedVersesOnPatch = 'There are no updated verses in the patch. Ignoring it.';

implementation

uses formPrincipal;

{ TFrmApplyPatch }

procedure TFrmApplyPatch.BtnSelectAllClick(Sender: TObject);
begin
  FBibleTreeView.SelectAll;
end;

procedure TFrmApplyPatch.BtnDeselectAllClick(Sender: TObject);
begin
  FBibleTreeView.SelectNone;
end;

procedure TFrmApplyPatch.FormCreate(Sender: TObject);
begin
  FBibleTreeView := TBibleTreeView.Create(GroupBox1);
  FBibleTreeView.Align := alClient;
  FBibleTreeView.OnVerseChange := @OnVerseChange;
  FPatch := nil;

  FSrcVerse := TVersiculo.Criar(ScrollBoxSrcText);
  FDstVerse := TVersiculo.Criar(ScrollBoxDstText);
  FSrcVerse.VersiculoPar := FDstVerse;
  FSrcVerse.ReadOnly := true;
  FDstVerse.ReadOnly := true;
end;

procedure TFrmApplyPatch.FormDestroy(Sender: TObject);
begin
  FSrcVerse.Free;
  FDstVerse.Free;
  FBibleTreeView.Free;
  if assigned(FPatch) then
    FPatch.Free;
end;

procedure TFrmApplyPatch.FormShow(Sender: TObject);
begin
  FSrcVerse.Fonte := ProjetoAtual.ObterFonteTexto(tbOrigem);
  FDstVerse.Fonte := ProjetoAtual.ObterFonteTexto(tbDestino);
  FSrcVerse.PalavrasComStrongEmNegrito := ProjetoAtual.PalavrasComStrongEmNegrito;
  FSrcVerse.StrongsCountMode := ProjetoAtual.MostrarQtdStrongs;
  FDstVerse.StrongsCountMode := ProjetoAtual.MostrarQtdStrongs;
  MemoComments.Font := FrmPrincipal.CommentsMemo.Font;
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
  SetLabelColor(LabelSrcText,      ProjetoAtual.ObterTextoVersiculo(FPatch.Reference[FIndex], tbOrigem ) = FPatch.SourceText[FIndex]);
  SetLabelColor(LabelDstText,      ProjetoAtual.ObterTextoVersiculo(FPatch.Reference[FIndex], tbDestino) = FPatch.DestinationText[FIndex]);
  SetLabelColor(LabelAssociations, ProjetoAtual.GetPairs           (FPatch.Reference[FIndex]           ) = FPatch.Pairs[FIndex]);
  SetLabelColor(LabelComments,     ProjetoAtual.GetComments        (FPatch.Reference[FIndex]           ) = FPatch.Comments[FIndex]);
  //SetLabelColor(LabelStatus, ProjetoAtual.Situacao(FPatch.Reference[FIndex]                     ) = FPatch.Status[FIndex]);
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
  FSrcVerse.Texto := FPatch.SourceText[FIndex];
  FDstVerse.Texto := FPatch.DestinationText[FIndex];
  FSrcVerse.Pares := FPatch.Pairs[FIndex];
  MemoComments.Text := FPatch.Comments[FIndex];
end;

procedure TFrmApplyPatch.LoadControlsFromProject;
begin
  FSrcVerse.Texto := ProjetoAtual.ObterTextoVersiculo(FPatch.Reference[FIndex], tbOrigem);
  FDstVerse.Texto := ProjetoAtual.ObterTextoVersiculo(FPatch.Reference[FIndex], tbDestino);
  FSrcVerse.Pares := ProjetoAtual.GetPairs(FPatch.Reference[FIndex]);
  MemoComments.Text := ProjetoAtual.GetComments(FPatch.Reference[FIndex]);
end;

function TFrmApplyPatch.LoadPatch(filename: string): boolean;
begin
  if assigned(FPatch) then
    FPatch.Free;

  FPatch := TPatchFile.Create(filename, ProjetoAtual);
  if FPatch.SourceText.Count = 0 then
  begin
    MessageDlg(SInfo, SNoUpdatedVersesOnPatch, mtInformation, [mbOK], 0);
    result := false;
    Exit;
  end;

  if FPatch.IgnoredVerses > 0 then
    MessageDlg(SInfo, Format(SIgnoredVersesOnPatch, [FPatch.IgnoredVerses]), mtInformation, [mbOK], 0);

  FBibleTreeView.VerseList := FPatch.Reference;
  FIndex := 0;
  TabControlPreviewChange(TabControlPreview);
  result := true;
end;

procedure TFrmApplyPatch.Apply;
var
  indexes: TIntegerList;
begin
  indexes := FBibleTreeView.IndexesList;
  try
    ProjetoAtual.ApplyPatch(FPatch, indexes);
  finally
    indexes.Free;
    FreeAndNil(FPatch);
  end;
  ShowMessage(SPatchSuccessfullyApplied);
end;

initialization
  {$I formapplypatch.lrs}

end.

