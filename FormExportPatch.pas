unit FormExportPatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, BibleScope, Project, LazLogger, BibleTreeView, PatchFile, LCLIntf,
  status_catalog;

type

  { TFrmExportPatch }

  TFrmExportPatch = class(TForm)
    Button1: TButton;
    Button2: TButton;
    BtnSelectAll: TButton;
    BtnDeselectAll: TButton;
    GroupBox1: TGroupBox;
    procedure BtnDeselectAllClick(Sender: TObject);
    procedure BtnSelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBibleTreeView: TBibleTreeView;
    FLayouting: Boolean;
    function CreatePatch: TPatchFile;
    function GetVerseList: TStringList;
    procedure ApplyLocalizedCaptions;
    procedure LayoutControls;
    procedure SetScope(AValue: TEscopoTexto);
  public
    procedure SavePatch(filename: string);
    property VerseList: TStringList read GetVerseList;
    property Scope: TEscopoTexto write SetScope;
  end;

{$INCLUDE include/bookdefs.inc}

var
  FrmExportPatch: TFrmExportPatch;

implementation

uses formPrincipal;

resourcestring
  SExportPatchTitle = 'Export patch...';
  SSelectAll = 'Select all';
  SDeselectAll = 'Deselect all';
  SCancel = 'Cancel';
  SOK = 'OK';

{ TFrmExportPatch }

procedure TFrmExportPatch.SetScope(AValue: TEscopoTexto);
begin
  FBibleTreeView.Scope := AValue;
end;

procedure TFrmExportPatch.SavePatch(filename: string);
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

function TFrmExportPatch.GetVerseList: TStringList;
begin
  result := FBibleTreeView.VerseList;
end;

function TFrmExportPatch.CreatePatch: TPatchFile;
var
  refs: TStringList;
  ref: string;
begin
  result := TPatchFile.Create;
  result.StatusCatalog := SaveStatusDefinitions(CurrentProject.GetStatusDefinitions);
  refs := FBibleTreeView.VerseList;
  try
    CurrentProject.StartScrollingSession;
    for ref in refs do
    begin
      CurrentProject.GoToReference(ref);
      DebugLn('exporting %s (%s)', [ref, CurrentProject.FormattedReference]);
      result.Add(ref,
                 CurrentProject.GetVerseText(tbOrigem),
                 CurrentProject.GetVerseText(tbDestino),
                 CurrentProject.Pairs,
                 CurrentProject.Comments,
                 CurrentProject.Status
                );
    end;
  finally
    CurrentProject.FinishScrollingSession;
    refs.Free;
  end;
end;

procedure TFrmExportPatch.FormCreate(Sender: TObject);
begin
  FBibleTreeView := TBibleTreeView.Create(GroupBox1);
  FBibleTreeView.Parent := GroupBox1;
  FBibleTreeView.Align := alClient;
  OnResize := @FormResize;
end;

procedure TFrmExportPatch.FormDestroy(Sender: TObject);
begin
  FBibleTreeView.Free;
end;

procedure TFrmExportPatch.FormShow(Sender: TObject);
begin
  ApplyLocalizedCaptions;
  Scope := CurrentProject.Escopo;
  Position := poScreenCenter; // necessary to force alignment after setting height
  Height := Trunc(MainForm.Height * 0.9);
  LayoutControls;
  Position := poMainFormCenter;
end;

procedure TFrmExportPatch.FormResize(Sender: TObject);
begin
  LayoutControls;
end;

procedure TFrmExportPatch.BtnSelectAllClick(Sender: TObject);
begin
  FBibleTreeView.SelectAll;
end;

procedure TFrmExportPatch.BtnDeselectAllClick(Sender: TObject);
begin
  FBibleTreeView.SelectNone;
end;

procedure TFrmExportPatch.ApplyLocalizedCaptions;
begin
  Caption := SExportPatchTitle;
  BtnSelectAll.Caption := SSelectAll;
  BtnDeselectAll.Caption := SDeselectAll;
  Button2.Caption := SCancel;
  Button1.Caption := SOK;
end;

procedure TFrmExportPatch.LayoutControls;
const
  Margin = 12;
  Gap = 8;
  GroupGap = 18;
  SelectWidth = 118;
  DeselectWidth = 140;
  DialogButtonWidth = 88;
  ButtonHeight = 26;
var
  ButtonTop: Integer;
  RequiredClientWidth: Integer;
begin
  if FLayouting then
    Exit;

  FLayouting := True;
  try
    RequiredClientWidth := Margin + SelectWidth + Gap + DeselectWidth +
      GroupGap + DialogButtonWidth + Gap + DialogButtonWidth + Margin;
    if ClientWidth < RequiredClientWidth then
      ClientWidth := RequiredClientWidth;

    ButtonTop := ClientHeight - ButtonHeight - Margin;
    if ButtonTop < 0 then
      ButtonTop := 0;

    GroupBox1.Align := alNone;
    GroupBox1.SetBounds(0, 0, ClientWidth, ButtonTop - Gap);

    BtnSelectAll.SetBounds(Margin, ButtonTop, SelectWidth, ButtonHeight);
    BtnDeselectAll.SetBounds(BtnSelectAll.Left + BtnSelectAll.Width + Gap,
      ButtonTop, DeselectWidth, ButtonHeight);

    Button1.SetBounds(ClientWidth - Margin - DialogButtonWidth, ButtonTop,
      DialogButtonWidth, ButtonHeight);
    Button2.SetBounds(Button1.Left - Gap - DialogButtonWidth, ButtonTop,
      DialogButtonWidth, ButtonHeight);
  finally
    FLayouting := False;
  end;
end;

initialization
{$IFNDEF FPCUNIT}
  {$I FormExportPatch.lrs}
{$ENDIF}

end.

