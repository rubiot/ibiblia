unit formInterlinearVerseRules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Projeto;

type

  { TFormInterlinearVerseRules }

  TFormInterlinearVerseRules = class(TForm)
    ButtonOK: TButton;
    Button2: TButton;
    StringGridInterlinear: TStringGrid;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FProject: TProjeto;
    procedure SetProject(AValue: TProjeto);
  public
    property Project: TProjeto read FProject write SetProject;
  end;

var
  frmInterlinearVerseRules: TFormInterlinearVerseRules;

implementation

{ TFormInterlinearVerseRules }

procedure TFormInterlinearVerseRules.ButtonOKClick(Sender: TObject);
var
  i: integer;
  rules: String;
begin
  rules := '';
  for i:=1 to StringGridInterlinear.RowCount-1 do
  begin
    if not rules.IsEmpty then
      rules := rules + #13;
    rules := Concat(rules, StringGridInterlinear.Cells[0, i], #10, StringGridInterlinear.Cells[1, i]);
  end;
  FProject.AtribuirInfo('propriedades.interlinearview.verserules', rules);
end;

procedure TFormInterlinearVerseRules.FormCreate(Sender: TObject);
begin

end;

procedure TFormInterlinearVerseRules.SetProject(AValue: TProjeto);
var
  rule: string;
  keyValue: TStringArray;
begin
  FProject := AValue;

  StringGridInterlinear.RowCount := 1;
  for rule in FProject.ObterInfo('propriedades.interlinearview.verserules').Split(#13) do
  begin
    keyValue := rule.Split(#10);
    StringGridInterlinear.InsertRowWithValues(StringGridInterlinear.RowCount, [keyValue[0], keyValue[1]]);
  end;
  StringGridInterlinear.AutoSizeColumns;
end;

initialization
  {$I formInterlinearVerseRules.lrs}

end.

