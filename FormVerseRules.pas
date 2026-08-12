unit FormVerseRules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, PCRE, LCLTranslator, Grids, fgl;

type

  TRegexList = specialize TFPGList<IRegex>;

  { TVerseRulesForm }

  TVerseRulesForm = class(TForm)
    CancelButton: TButton;
    OkButton: TButton;
    BtnSelectNone: TButton;
    BtnSelectAll: TButton;
    RulesPanel: TPanel;
    StringGridVerseRules: TStringGrid;
    procedure BtnSelectAllClick(Sender: TObject);
    procedure BtnSelectNoneClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FInfoPanel: TPanel;
    FInfoMemo: TMemo;
    FModuleFileName: string;
    procedure EnsureInfoControls;
    function GetHasRules: boolean;
    procedure LayoutControls;
    procedure UpdateInfoControls;
  public
    procedure AddVerseRule(SourcePattern: string; ReplacementText: string);
    procedure Reset(const ModuleFileName: string = '');
    procedure CompileVerseRules(var rxFrom: TRegexList; var rxTo: TStringList);
    property HasRules: boolean read GetHasRules;
  end;

var
  VerseRulesForm: TVerseRulesForm;

implementation

{ TVerseRulesForm }

resourcestring
  SVerseRulesTitle = 'theWord verse rules';
  SVerseRulesModule = 'Module: %s';
  SVerseRulesDescription = 'This module defines verse rules: regular-expression replacements that theWord applies while displaying verses. iBiblia imports fixed text, so selected rules are applied now. Keep them selected unless the module uses them only as optional display toggles.';
  SVerseRulesCount = '%d rules found. Uncheck any rule that should not be applied to the imported text.';
  SVerseRulesUnknownModule = '(module not available)';

procedure TVerseRulesForm.FormShow(Sender: TObject);
begin
  EnsureInfoControls;
  LayoutControls;
  UpdateInfoControls;
  FInfoPanel.Invalidate;
  StringGridVerseRules.AutoAdjustColumns;
end;

procedure TVerseRulesForm.EnsureInfoControls;
begin
  if Assigned(FInfoPanel) then
    Exit;

  FInfoPanel := TPanel.Create(Self);
  FInfoPanel.Parent := Self;
  FInfoPanel.BevelOuter := bvNone;
  FInfoPanel.Caption := '';
  FInfoPanel.Anchors := [akLeft, akTop, akRight];

  FInfoMemo := TMemo.Create(Self);
  FInfoMemo.Parent := FInfoPanel;
  FInfoMemo.BorderStyle := bsNone;
  FInfoMemo.Color := Color;
  FInfoMemo.ReadOnly := True;
  FInfoMemo.TabStop := False;
  FInfoMemo.WordWrap := True;
  FInfoMemo.ScrollBars := ssNone;
  FInfoMemo.Anchors := [akLeft, akTop, akRight, akBottom];
end;

procedure TVerseRulesForm.LayoutControls;
const
  Margin = 12;
  ButtonTopMargin = 12;
  ButtonBottomMargin = 14;
  ButtonHeight = 25;
  GridTopGap = 8;
  SelectButtonWidth = 110;
  DialogButtonWidth = 75;
  ButtonGap = 18;
var
  ButtonTop: Integer;
  GridTop: Integer;
  InfoHeight: Integer;
  LineHeight: Integer;
begin
  AutoScroll := False;
  Constraints.MinWidth := 760;
  Constraints.MinHeight := 430;
  if ClientHeight < Constraints.MinHeight then
    ClientHeight := Constraints.MinHeight;
  if ClientWidth < Constraints.MinWidth then
    ClientWidth := Constraints.MinWidth;

  EnsureInfoControls;
  LineHeight := Canvas.TextHeight('Tg') + 6;
  InfoHeight := LineHeight * 9 + 16;

  FInfoPanel.SetBounds(Margin, Margin, ClientWidth - (Margin * 2),
    InfoHeight);
  FInfoMemo.SetBounds(0, 0, FInfoPanel.Width, FInfoPanel.Height);

  ButtonTop := ClientHeight - ButtonBottomMargin - ButtonHeight;
  GridTop := FInfoPanel.Top + FInfoPanel.Height + GridTopGap;
  RulesPanel.SetBounds(Margin, GridTop, ClientWidth - (Margin * 2),
    ButtonTop - GridTop - ButtonTopMargin);
  RulesPanel.Caption := '';

  OkButton.SetBounds(ClientWidth - Margin - DialogButtonWidth, ButtonTop,
    DialogButtonWidth, ButtonHeight);
  CancelButton.SetBounds(OkButton.Left - ButtonGap - DialogButtonWidth, ButtonTop,
    DialogButtonWidth, ButtonHeight);
  BtnSelectNone.SetBounds(CancelButton.Left - ButtonGap - SelectButtonWidth,
    ButtonTop, SelectButtonWidth, ButtonHeight);
  BtnSelectAll.SetBounds(BtnSelectNone.Left - ButtonGap - SelectButtonWidth,
    ButtonTop, SelectButtonWidth, ButtonHeight);
end;

procedure TVerseRulesForm.UpdateInfoControls;
var
  ModuleText: string;
begin
  EnsureInfoControls;
  ModuleText := FModuleFileName;
  if Trim(ModuleText) = '' then
    ModuleText := SVerseRulesUnknownModule;
  FInfoMemo.Lines.BeginUpdate;
  try
    FInfoMemo.Clear;
    FInfoMemo.Lines.Add(SVerseRulesTitle);
    FInfoMemo.Lines.Add(Format(SVerseRulesModule, [ModuleText]));
    FInfoMemo.Lines.Add('');
    FInfoMemo.Lines.Add(SVerseRulesDescription);
    FInfoMemo.Lines.Add('');
    FInfoMemo.Lines.Add(Format(SVerseRulesCount,
      [StringGridVerseRules.RowCount - 1]));
  finally
    FInfoMemo.Lines.EndUpdate;
  end;
  FInfoMemo.Hint := Format(SVerseRulesModule, [ModuleText]);
  FInfoMemo.ShowHint := True;
end;

procedure TVerseRulesForm.BtnSelectAllClick(Sender: TObject);
var
  r: integer;
begin
  for r:=1 to StringGridVerseRules.RowCount-1 do
    StringGridVerseRules.Cells[0, r] := '1';
end;

procedure TVerseRulesForm.BtnSelectNoneClick(Sender: TObject);
var
  r: integer;
begin
  for r:=1 to StringGridVerseRules.RowCount-1 do
    StringGridVerseRules.Cells[0, r] := '0';
end;

function TVerseRulesForm.GetHasRules: boolean;
begin
  result := StringGridVerseRules.RowCount > 1;
end;

procedure TVerseRulesForm.AddVerseRule(SourcePattern: string; ReplacementText: string);
begin
  StringGridVerseRules.RowCount := StringGridVerseRules.RowCount + 1;
  StringGridVerseRules.Cells[0, StringGridVerseRules.RowCount-1] := '1';
  StringGridVerseRules.Cells[1, StringGridVerseRules.RowCount-1] := SourcePattern.Replace('""', '"', [rfReplaceAll]);
  StringGridVerseRules.Cells[2, StringGridVerseRules.RowCount-1] := ReplacementText.Replace('""', '"', [rfReplaceAll]);
end;

procedure TVerseRulesForm.Reset(const ModuleFileName: string);
begin
  FModuleFileName := ModuleFileName;
  StringGridVerseRules.RowCount := 1;
  if Assigned(FInfoPanel) and Visible then
  begin
    LayoutControls;
    UpdateInfoControls;
  end;
end;

procedure TVerseRulesForm.CompileVerseRules(var rxFrom: TRegexList; var rxTo: TStringList);
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
