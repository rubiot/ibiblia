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
    procedure LoadPatch(filename: string);
    procedure Apply;
  end;

var
  FrmApplyPatch: TFrmApplyPatch;

resourcestring
  SPatchSuccessfullyApplied = 'Patch successfully applied';

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
begin
  case TabControlPreview.TabIndex of
    0: LoadControlsFromPatch;
    1: LoadControlsFromProject;
  end;
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

procedure TFrmApplyPatch.LoadPatch(filename: string);
begin
  if assigned(FPatch) then
    FPatch.Free;

  FPatch := TPatchFile.Create(filename);
  FBibleTreeView.VerseList := FPatch.Reference;
  FIndex := 0;
  TabControlPreviewChange(TabControlPreview);
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

