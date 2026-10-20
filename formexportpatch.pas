unit formexportpatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Projeto, LazLogger, BibleTreeView, PatchFile, LCLIntf;

type

  { TFormExportPatch }

  TFormExportPatch = class(TForm)
    Button1: TButton;
    Button2: TButton;
    BtnSelectAll: TButton;
    BtnDeselectAll: TButton;
    GroupBox1: TGroupBox;
    procedure BtnDeselectAllClick(Sender: TObject);
    procedure BtnSelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBibleTreeView: TBibleTreeView;
    function CreatePatch: TPatchFile;
    function GetVerseList: TStringList;
    procedure SetScope(AValue: TEscopoTexto);
  public
    procedure SavePatch(filename: string);
    property VerseList: TStringList read GetVerseList;
    property Scope: TEscopoTexto write SetScope;
  end;

{$INCLUDE include/bookdefs.inc}

var
  FrmExportPatch: TFormExportPatch;

implementation

uses formPrincipal;

{ TFormExportPatch }

procedure TFormExportPatch.SetScope(AValue: TEscopoTexto);
begin
  FBibleTreeView.Scope := AValue;
end;

procedure TFormExportPatch.SavePatch(filename: string);
var
  patch: TPatchFile;
begin
  patch := CreatePatch;

  if patch.Reference.Count = 0 then
    exit;

  try
    patch.Save(filename);
  finally
    patch.Free;
  end;
  OpenDocument(ExtractFilePath(filename));
end;

function TFormExportPatch.GetVerseList: TStringList;
begin
  result := FBibleTreeView.VerseList;
end;

function TFormExportPatch.CreatePatch: TPatchFile;
var
  refs: TStringList;
  ref: string;
begin
  result := TPatchFile.Create;
  refs := FBibleTreeView.VerseList;
  try
    ProjetoAtual.StartScrollingSession;
    for ref in refs do
    begin
      ProjetoAtual.GoToReference(ref);
      DebugLn('exporting %s (%s)', [ref, ProjetoAtual.FormattedReference]);
      result.Add(ref,
                 ProjetoAtual.ObterTextoVersiculo(tbOrigem),
                 ProjetoAtual.ObterTextoVersiculo(tbDestino),
                 ProjetoAtual.Pairs,
                 ProjetoAtual.Comentarios,
                 ProjetoAtual.Situacao
                );
    end;
  finally
    ProjetoAtual.FinishScrollingSession;
    refs.Free;
  end;
end;

procedure TFormExportPatch.FormCreate(Sender: TObject);
begin
  FBibleTreeView := TBibleTreeView.Create(GroupBox1);
  FBibleTreeView.Align := alClient;
end;

procedure TFormExportPatch.FormDestroy(Sender: TObject);
begin
  FBibleTreeView.Free;
end;

procedure TFormExportPatch.FormShow(Sender: TObject);
begin
  Scope := ProjetoAtual.Escopo;
  Position := poScreenCenter; // necessary to force alignment after setting height
  Height := Trunc(FrmPrincipal.Height * 0.9);
  Position := poMainFormCenter;
end;

procedure TFormExportPatch.BtnSelectAllClick(Sender: TObject);
begin
  FBibleTreeView.SelectAll;
end;

procedure TFormExportPatch.BtnDeselectAllClick(Sender: TObject);
begin
  FBibleTreeView.SelectNone;
end;

initialization
  {$I formexportpatch.lrs}

end.

