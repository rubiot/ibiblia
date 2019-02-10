unit formverserules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, PCRE, LCLTranslator;

type

  { TFrmEscolherVerseRules }

  TFrmEscolherVerseRules = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ckgVerseRules: TCheckGroup;
    lbDe: TListBox;
    lbPara: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbDeSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    FVerseRulesDe: TStringList;
    FVerseRulesPara: TStringList;
    FCorrigirAspas: IRegex;
  public
    { public declarations }
    procedure CarregarVerseRules(rulesDe: TStringList; rulesPara: TStringList);
    procedure AddVerseRule(de: String; para: String);
    procedure Reset;
    property VerseRuleDe: TStringList read FVerseRulesDe;
    property VerseRulePara: TStringList read FVerseRulesPara;
  end;

var
  FrmEscolherVerseRules: TFrmEscolherVerseRules;

implementation

{ TFrmEscolherVerseRules }

procedure TFrmEscolherVerseRules.FormCreate(Sender: TObject);
begin
  FVerseRulesDe := TStringList.Create;
  FVerseRulesPara := TStringList.Create;
  FCorrigirAspas:= RegexCreate('""');
end;

procedure TFrmEscolherVerseRules.CarregarVerseRules(rulesDe: TStringList;
  rulesPara: TStringList);
var
  r: integer;
begin
  Reset;
  for r:=0 to rulesDe.Count do
    AddVerseRule(rulesDe.Strings[r], rulesPara.Strings[r]);
end;

procedure TFrmEscolherVerseRules.AddVerseRule(de: String; para: String);
begin
  de := FCorrigirAspas.Replace(de, '"');
  para := FCorrigirAspas.Replace(para, '"');
  FVerseRulesDe.Add(de);
  FVerseRulesPara.Add(para);
  ckgVerseRules.Items.Add(Format('[%s] → [%s]', [de, para]));
  ckgVerseRules.Checked[ ckgVerseRules.Items.Count-1 ] := true;

  lbDe.Items.Add(de);
  lbPara.Items.Add(para);
  lbDe.Selected[lbDe.Items.Count-1] := true;
  lbPara.Selected[lbPara.Items.Count-1] := true;
end;

procedure TFrmEscolherVerseRules.Reset;
begin
  ckgVerseRules.Items.Clear;
  FVerseRulesDe.Clear;
  FVerseRulesPara.Clear;
end;

procedure TFrmEscolherVerseRules.FormDestroy(Sender: TObject);
begin
  FVerseRulesDe.Free;
  FVerseRulesPara.Free;
end;

procedure TFrmEscolherVerseRules.lbDeSelectionChange(Sender: TObject;
  User: boolean);
begin
  //lbPara.Selected[lbDe.ItemIndex] := lbDe.Selected[lbDe.ItemIndex];
end;

initialization
  {$I formverserules.lrs}

end.

