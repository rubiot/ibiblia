unit formmesclarprojetos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, CheckLst;

type

  { TFrmMesclarProjetos }

  TFrmMesclarProjetos = class(TForm)
    Button1: TButton;
    CheckListBox1: TCheckListBox;
    CheckListBox2: TCheckListBox;
    OpenDialog1: TOpenDialog;
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure CheckListBox2ClickCheck(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure InicializarForm;
  end;

var
  FrmMesclarProjetos: TFrmMesclarProjetos;

implementation

{ TFrmMesclarProjetos }

procedure TFrmMesclarProjetos.CheckListBox1ClickCheck(Sender: TObject);
begin
  CheckListBox2.Checked[CheckListBox1.ItemIndex] := not CheckListBox1.Checked[CheckListBox1.ItemIndex];
end;

procedure TFrmMesclarProjetos.CheckListBox2ClickCheck(Sender: TObject);
begin
  CheckListBox1.Checked[CheckListBox2.ItemIndex] := not CheckListBox2.ItemEnabled[CheckListBox2.ItemIndex];
end;

procedure TFrmMesclarProjetos.InicializarForm;
var
  i: integer;
begin
  for i:=0 to CheckListBox1.Count-1 do
  begin
    CheckListBox1.Checked[i] := true;
    CheckListBox2.Checked[i] := false;
  end;
end;

initialization
  {$I formmesclarprojetos.lrs}

end.

