unit FormVerseRules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, PCRE, LCLTranslator, Grids, fgl;

type

  TRegexList = specialize TFPGList<IRegex>;

  { TFrmVerseRules }

  TFrmVerseRules = class(TForm)
    Button1: TButton;
    Button2: TButton;
    BtnSelectNone: TButton;
    BtnSelectAll: TButton;
    Panel1: TPanel;
    StringGridVerseRules: TStringGrid;
    procedure BtnSelectAllClick(Sender: TObject);
    procedure BtnSelectNoneClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetHasRules: boolean;
  public
    procedure AddVerseRule(de: String; para: String);
    procedure Reset;
    procedure CompileVerseRules(var rxFrom: TRegexList; var rxTo: TStringList);
    property HasRules: boolean read GetHasRules;
  end;

var
  FrmVerseRules: TFrmVerseRules;

implementation

{ TFrmVerseRules }

procedure TFrmVerseRules.FormShow(Sender: TObject);
begin
  StringGridVerseRules.AutoAdjustColumns;
end;

procedure TFrmVerseRules.BtnSelectAllClick(Sender: TObject);
var
  r: integer;
begin
  for r:=1 to StringGridVerseRules.RowCount-1 do
    StringGridVerseRules.Cells[0, r] := '1';
end;

procedure TFrmVerseRules.BtnSelectNoneClick(Sender: TObject);
var
  r: integer;
begin
  for r:=1 to StringGridVerseRules.RowCount-1 do
    StringGridVerseRules.Cells[0, r] := '0';
end;

function TFrmVerseRules.GetHasRules: boolean;
begin
  result := StringGridVerseRules.RowCount > 1;
end;

procedure TFrmVerseRules.AddVerseRule(de: String; para: String);
begin
  StringGridVerseRules.RowCount := StringGridVerseRules.RowCount + 1;
  StringGridVerseRules.Cells[0, StringGridVerseRules.RowCount-1] := '1';
  StringGridVerseRules.Cells[1, StringGridVerseRules.RowCount-1] := de.Replace('""', '"', [rfReplaceAll]);
  StringGridVerseRules.Cells[2, StringGridVerseRules.RowCount-1] := para.Replace('""', '"', [rfReplaceAll]);
end;

procedure TFrmVerseRules.Reset;
begin
  StringGridVerseRules.RowCount := 1;
end;

procedure TFrmVerseRules.CompileVerseRules(var rxFrom: TRegexList; var rxTo: TStringList);
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
{$IFNDEF FPCUNIT}
  {$I FormVerseRules.lrs}
{$ENDIF}

end.