unit FormMergeProjects;

{$mode objfpc}{$H+}

interface

uses
  ui_style, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, CheckLst;

type

  TFrmMergeProjects = class(TForm)
    BtnMerge: TButton;
    SourceCheckList: TCheckListBox;
    TargetCheckList: TCheckListBox;
    OpenDialogSelectProject: TOpenDialog;
    procedure SourceCheckListClickCheck(Sender: TObject);
    procedure TargetCheckListClickCheck(Sender: TObject);
  public
    procedure InitializeForm;
  public
    procedure AfterConstruction; override;
  end;

var
  FrmMergeProjects: TFrmMergeProjects;

implementation

{ TFrmMergeProjects }

procedure TFrmMergeProjects.SourceCheckListClickCheck(Sender: TObject);
begin
  TargetCheckList.Checked[SourceCheckList.ItemIndex] := not SourceCheckList.Checked[SourceCheckList.ItemIndex];
end;

procedure TFrmMergeProjects.TargetCheckListClickCheck(Sender: TObject);
begin
  SourceCheckList.Checked[TargetCheckList.ItemIndex] := not TargetCheckList.ItemEnabled[TargetCheckList.ItemIndex];
end;

procedure TFrmMergeProjects.InitializeForm;
var
  ItemIndex: Integer;
begin
  for ItemIndex := 0 to SourceCheckList.Count - 1 do
  begin
    SourceCheckList.Checked[ItemIndex] := True;
    TargetCheckList.Checked[ItemIndex] := False;
  end;
end;

procedure TFrmMergeProjects.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
end;


initialization
{$IFNDEF FPCUNIT}
  {$R FormMergeProjects.lfm}
{$ENDIF}

end.