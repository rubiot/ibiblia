unit formverserules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, PCRE, LCLTranslator, Grids, fgl;

type

  TRegexList = specialize TFPGList<IRegex>;

  { TFrmEscolherVerseRules }

  TFrmEscolherVerseRules = class(TForm)
    Button1: TButton;
    Button2: TButton;
    BtnSelectNone: TButton;
    BtnSelectAll: TButton;
    StringGridVerseRules: TStringGrid;
    procedure BtnSelectAllClick(Sender: TObject);
    procedure BtnSelectNoneClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetHasRules: boolean;
    { private declarations }
  public
    { public declarations }
    procedure AddVerseRule(de: String; para: String);
    procedure Reset;
    procedure CompileVerseRules(var rxFrom: TRegexList; var rxTo: TStringList);
    property HasRules: boolean read GetHasRules;
  end;

var
  FrmEscolherVerseRules: TFrmEscolherVerseRules;

implementation

{ TFrmEscolherVerseRules }

procedure TFrmEscolherVerseRules.FormShow(Sender: TObject);
begin
  StringGridVerseRules.AutoAdjustColumns;
end;

procedure TFrmEscolherVerseRules.BtnSelectAllClick(Sender: TObject);
var
  r: integer;
begin
  for r:=1 to StringGridVerseRules.RowCount-1 do
    StringGridVerseRules.Cells[0, r] := '1';
end;

procedure TFrmEscolherVerseRules.BtnSelectNoneClick(Sender: TObject);
var
  r: integer;
begin
  for r:=1 to StringGridVerseRules.RowCount-1 do
    StringGridVerseRules.Cells[0, r] := '0';
end;

function TFrmEscolherVerseRules.GetHasRules: boolean;
begin
  result := StringGridVerseRules.RowCount > 1;
end;

procedure TFrmEscolherVerseRules.AddVerseRule(de: String; para: String);
begin
  StringGridVerseRules.RowCount := StringGridVerseRules.RowCount + 1;
  StringGridVerseRules.Cells[0, StringGridVerseRules.RowCount-1] := '1';
  StringGridVerseRules.Cells[1, StringGridVerseRules.RowCount-1] := de.Replace('""', '"', [rfReplaceAll]);
  StringGridVerseRules.Cells[2, StringGridVerseRules.RowCount-1] := para.Replace('""', '"', [rfReplaceAll]);
end;

procedure TFrmEscolherVerseRules.Reset;
begin
  StringGridVerseRules.RowCount := 1;
end;

procedure TFrmEscolherVerseRules.CompileVerseRules(var rxFrom: TRegexList; var rxTo: TStringList);
var
  r: integer;
begin
  rxFrom := TRegexList.Create;
  rxTo   := TStringList.Create;

  for r:=1 to StringGridVerseRules.RowCount-1 do
    if StringGridVerseRules.Cells[0, r] = '1' then
    begin
      rxFrom.Add(RegexCreate(StringGridVerseRules.Cells[1, r], [rcoUTF8]));
      rxTo.Add(StringGridVerseRules.Cells[2, r]);
    end;
end;

initialization
  {$I formverserules.lrs}

end.

