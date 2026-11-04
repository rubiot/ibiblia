unit FormInterlinearVerseRules;

{$mode objfpc}{$H+}

interface

uses
  ui_style, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ComCtrls, Clipbrd, StrUtils, Math, Project;

type

  { TInterlinearVerseRulesForm }

  TInterlinearVerseRulesForm = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    VerseRulesPageControl: TPageControl;
    StringGridIntralinear: TStringGrid;
    StringGridInterlinear: TStringGrid;
    InterlinearTab: TTabSheet;
    IntralinearTab: TTabSheet;
    procedure OkButtonClick(Sender: TObject);
  private
    FProject: TProject;
    procedure SetProject(AValue: TProject);
    procedure LoadGrid(grid: TStringGrid; values: string);
    function GetVerseRules(grid: TStringGrid): string;
  public
    property Project: TProject read FProject write SetProject;
  public
    procedure AfterConstruction; override;
  end;

var
  InterlinearVerseRulesForm: TInterlinearVerseRulesForm;

implementation

{ TInterlinearVerseRulesForm }

procedure TInterlinearVerseRulesForm.OkButtonClick(Sender: TObject);
begin
  FProject.SetInfo('propriedades.interlinearview.verserules', GetVerseRules(StringGridInterlinear));
  FProject.SetInfo('propriedades.intralinearview.verserules', GetVerseRules(StringGridIntralinear));
end;

procedure TInterlinearVerseRulesForm.SetProject(AValue: TProject);
begin
  FProject := AValue;
  if Assigned(CancelButton) then
    CancelButton.Caption := SUICancel;
  LoadGrid(StringGridInterlinear, FProject.GetInfo('propriedades.interlinearview.verserules'));
  LoadGrid(StringGridIntralinear, FProject.GetInfo('propriedades.intralinearview.verserules'));
  VerseRulesPageControl.PageIndex := IfThen(FProject.InterlinearMode = imInterlinear, 0, 1);
end;

procedure TInterlinearVerseRulesForm.LoadGrid(grid: TStringGrid; values: string
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

function TInterlinearVerseRulesForm.GetVerseRules(grid: TStringGrid): string;
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

procedure TInterlinearVerseRulesForm.AfterConstruction;
begin
  inherited AfterConstruction;
  ApplyDialogStyle(Self);
end;


initialization
{$IFNDEF FPCUNIT}
  {$R FormInterlinearVerseRules.lfm}
{$ENDIF}

end.

