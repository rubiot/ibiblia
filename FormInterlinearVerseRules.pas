unit FormInterlinearVerseRules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ComCtrls, Clipbrd, StrUtils, Math, Project;

type

  { TFrmInterlinearVerseRules }

  TFrmInterlinearVerseRules = class(TForm)
    ButtonOK: TButton;
    Button2: TButton;
    PageControlVerseRules: TPageControl;
    StringGridIntralinear: TStringGrid;
    StringGridInterlinear: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ButtonOKClick(Sender: TObject);
  private
    FProject: TProject;
    procedure SetProject(AValue: TProject);
    procedure LoadGrid(grid: TStringGrid; values: string);
    function GetVerseRules(grid: TStringGrid): string;
  public
    property Project: TProject read FProject write SetProject;
  end;

var
  FrmInterlinearVerseRules: TFrmInterlinearVerseRules;

implementation

{ TFrmInterlinearVerseRules }

procedure TFrmInterlinearVerseRules.ButtonOKClick(Sender: TObject);
begin
  FProject.SetInfo('propriedades.interlinearview.verserules', GetVerseRules(StringGridInterlinear));
  FProject.SetInfo('propriedades.intralinearview.verserules', GetVerseRules(StringGridIntralinear));
end;

procedure TFrmInterlinearVerseRules.SetProject(AValue: TProject);
begin
  FProject := AValue;
  LoadGrid(StringGridInterlinear, FProject.GetInfo('propriedades.interlinearview.verserules'));
  LoadGrid(StringGridIntralinear, FProject.GetInfo('propriedades.intralinearview.verserules'));
  PageControlVerseRules.PageIndex := IfThen(FProject.InterlinearMode = imInterlinear, 0, 1);
end;

procedure TFrmInterlinearVerseRules.LoadGrid(grid: TStringGrid; values: string
  );
var
  rule: string;
  fromTo: TStringArray;
begin
  grid.RowCount := 1; // clearing grid

  if not values.IsEmpty then
    for rule in values.Split(#13) do
      if not rule.StartsWith(#10#10) then // not an empty rule
      begin
        fromTo := rule.Split(#10);
        grid.RowCount := grid.RowCount + 1;
        grid.Cells[0, grid.RowCount-1] := fromTo[0];
        grid.Cells[1, grid.RowCount-1] := fromTo[1];
        grid.Cells[2, grid.RowCount-1] := IfThen(length(fromTo) = 2, '1', fromTo[2]);
        grid.Cells[3, grid.RowCount-1] := IfThen(length(fromTo) = 2, '1', fromTo[3]);
      end;

  if grid.RowCount = 1 then
    grid.RowCount := 2; // add an empty row

  grid.AutoSizeColumns;
end;

function TFrmInterlinearVerseRules.GetVerseRules(grid: TStringGrid): string;
var
  i: integer;
begin
  result := '';
  for i:=1 to grid.RowCount-1 do
  begin
    if not result.IsEmpty then
      result := result + #13;
    result := Concat(result, grid.Cells[0, i], #10,
                             grid.Cells[1, i], #10,
                             grid.Cells[2, i], #10,
                             grid.Cells[3, i]);
  end;
end;

initialization
{$IFNDEF FPCUNIT}
  {$I FormInterlinearVerseRules.lrs}
{$ENDIF}

end.

