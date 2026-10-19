unit unitabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, fileinfo, LCLIntf;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    ButtonChangelog: TButton;
    ButtonClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelContributor: TLabel;
    LabelVersion: TLabel;
    procedure ButtonChangelogClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private

  public

  end;

procedure ShowChangelog(AOwner: TComponent);

var
  FormAbout: TFormAbout;

implementation

const
  CHANGELOG_TEXT =
    'iBiblia Version 1.0.0.69 (64-bit Edition)' + sLineBreak +
    'Release Date: 2026-10-19' + sLineBreak +
    '==========================================' + sLineBreak + sLineBreak +
    'NEW FEATURES:' + sLineBreak +
    '  * Native 64-bit Windows support (Win64 / x86_64) compatible with theWord 7 64-bit.' + sLineBreak +
    '  * Dynamic runtime linkage with 64-bit SQLite 3 (sqlite3.dll) and PCRE (pcre64.dll).' + sLineBreak +
    '  * Verse Status Filtering (Issue #104): filter Bible tree by verse status.' + sLineBreak +
    '  * Fast navigation controls (< and > buttons) with global shortcuts:' + sLineBreak +
    '      - F8: Jump to Next filtered verse' + sLineBreak +
    '      - Shift + F8: Jump to Previous filtered verse' + sLineBreak +
    '  * Live dynamic verse match counters for each status code.' + sLineBreak +
    '  * First-time startup changelog popup and About dialog link.' + sLineBreak + sLineBreak +
    'BUG FIXES & IMPROVEMENTS:' + sLineBreak +
    '  * Fixed 64-bit pointer and ordinal conversion issues in PCRE and TreeView.' + sLineBreak +
    '  * Fixed Access Violation crash when switching back to "All verses".' + sLineBreak +
    '  * Removed hardcoded execution directory and debugging breakpoints.' + sLineBreak + sLineBreak +
    'CONTRIBUTORS:' + sLineBreak +
    '  * Edmund Valdez: https://github.com/SalitaNgDiyos' + sLineBreak +
    '  * Rúbio R. C. Terra: Original concept, design, and architecture.' + sLineBreak + sLineBreak +
    '------------------------------------------' + sLineBreak +
    'For full project history and source code, visit:' + sLineBreak +
    'https://github.com/rubiot/ibiblia';

procedure ShowChangelog(AOwner: TComponent);
var
  dlg: TForm;
  memo: TMemo;
  btn: TButton;
  lbl: TLabel;
  histFile: string;
begin
  dlg := TForm.Create(AOwner);
  try
    dlg.Position := poScreenCenter;
    dlg.Width := 560;
    dlg.Height := 430;
    dlg.Caption := 'What''s New in iBiblia';
    dlg.BorderStyle := bsDialog;

    lbl := TLabel.Create(dlg);
    lbl.Parent := dlg;
    lbl.Left := 16;
    lbl.Top := 12;
    lbl.Font.Height := -16;
    lbl.Font.Style := [fsBold];
    lbl.Caption := 'iBiblia v1.0.0.69 - Changelog';

    memo := TMemo.Create(dlg);
    memo.Parent := dlg;
    memo.Left := 16;
    memo.Top := 38;
    memo.Width := 528;
    memo.Height := 335;
    memo.ReadOnly := true;
    memo.ScrollBars := ssAutoVertical;
    memo.Font.Name := 'Segoe UI';
    memo.Font.Size := 9;

    histFile := ExtractFilePath(Application.ExeName) + 'historico.txt';
    if not FileExists(histFile) then
      histFile := ExtractFilePath(Application.ExeName) + 'Histórico.txt';

    if FileExists(histFile) then
    begin
      try
        memo.Lines.LoadFromFile(histFile);
      except
        memo.Text := CHANGELOG_TEXT;
      end;
    end
    else
      memo.Text := CHANGELOG_TEXT;

    btn := TButton.Create(dlg);
    btn.Parent := dlg;
    btn.Width := 100;
    btn.Height := 30;
    btn.Left := (dlg.Width - btn.Width) div 2;
    btn.Top := 383;
    btn.Caption := 'OK';
    btn.ModalResult := mrOk;
    btn.Default := true;

    dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;

{ TFormAbout }

procedure TFormAbout.ButtonChangelogClick(Sender: TObject);
begin
  ShowChangelog(Self);
end;

procedure TFormAbout.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
  ver: string;
begin
  ver := '';
  try
    FileVerInfo := TFileVersionInfo.Create(nil);
    FileVerInfo.ReadFileInfo;
    ver := FileVerInfo.VersionStrings.Values['FileVersion'];
    FileVerInfo.Free;
  except
    ver := '';
  end;
  if ver = '' then
    ver := '1.0.0.69';
  LabelVersion.Caption := 'Version ' + ver + ' (64-bit)';
  LabelContributor.Caption := 'Contributor: Edmund Valdez';
  Label3.Caption := 'https://github.com/SalitaNgDiyos';
end;

procedure TFormAbout.Label3Click(Sender: TObject);
begin
  OpenURL('https://github.com/SalitaNgDiyos');
end;


initialization
  {$I unitabout.lrs}

end.
