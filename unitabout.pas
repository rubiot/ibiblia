unit unitabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, fileinfo, LCLIntf;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    Label1: TLabel;
    LabelVersion: TLabel;
    Label2: TLabel;
    LabelRubioURL: TLabel;
    LabelContributor: TLabel;
    Label3: TLabel;
    ButtonChangelog: TButton;
    ButtonClose: TButton;
    procedure ButtonChangelogClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure LabelRubioURLClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure ShowChangelog(AOwner: TComponent);

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

const
  CHANGELOG_TEXT =
    'iBiblia Version 1.0.0.70 (64-bit Edition)' + sLineBreak +
    'Release Date: 2026-10-20' + sLineBreak +
    '==========================================' + sLineBreak + sLineBreak +
    'NEW FEATURES & UPDATES (v1.0.0.70):' + sLineBreak +
    '  * Find in Project (Ctrl + F) search engine bug fixes and stability improvements.' + sLineBreak +
    '  * Restored repository link (https://github.com/rubiot/ibiblia) under Rubio Terra.' + sLineBreak +
    '  * Direct link to contributor Edmund Valdez (https://github.com/SalitaNgDiyos).' + sLineBreak +
    '  * Clean regular font styling in About dialog.' + sLineBreak + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.0.69:' + sLineBreak +
    '  * Native 64-bit Windows support (Win64 / x86_64) compatible with theWord 7 64-bit.' + sLineBreak +
    '  * Dynamic runtime linkage with 64-bit SQLite 3 (sqlite3.dll) and PCRE (pcre64.dll).' + sLineBreak +
    '  * Verse Status Filtering (Issue #104): filter Bible tree by verse status.' + sLineBreak +
    '  * Fast navigation controls (< and > buttons) with global shortcuts:' + sLineBreak +
    '      - F8: Jump to Next filtered verse' + sLineBreak +
    '      - Shift + F8: Jump to Previous filtered verse' + sLineBreak +
    '  * Live dynamic verse match counters for each status code.' + sLineBreak +
    '  * First-time startup changelog popup and About dialog link.' + sLineBreak +
    '  * Restored word highlight on selection with auto-advance on word association.' + sLineBreak + sLineBreak +
    'CONTRIBUTORS:' + sLineBreak +
    '  * Edmund Valdez: https://github.com/SalitaNgDiyos' + sLineBreak +
    '  * Rúbio R. C. Terra: https://github.com/rubiot/ibiblia' + sLineBreak + sLineBreak +
    '------------------------------------------';

constructor TFormAbout.Create(AOwner: TComponent);
var
  FileVerInfo: TFileVersionInfo;
  ver: string;
begin
  inherited CreateNew(AOwner);

  Caption := 'About iBiblia';
  BorderStyle := bsToolWindow;
  BorderIcons := [biSystemMenu];
  Position := poMainFormCenter;
  ClientWidth := 480;
  ClientHeight := 240;

  // Title: iBiblia
  Label1 := TLabel.Create(Self);
  Label1.Parent := Self;
  Label1.Left := 0;
  Label1.Top := 10;
  Label1.Width := 480;
  Label1.Height := 28;
  Label1.Alignment := taCenter;
  Label1.AutoSize := False;
  Label1.Caption := 'iBiblia';
  Label1.Font.Height := -21;
  Label1.Font.Style := [];

  // Version
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
    ver := '1.0.0.70';

  LabelVersion := TLabel.Create(Self);
  LabelVersion.Parent := Self;
  LabelVersion.Left := 0;
  LabelVersion.Top := 40;
  LabelVersion.Width := 480;
  LabelVersion.Height := 20;
  LabelVersion.Alignment := taCenter;
  LabelVersion.AutoSize := False;
  LabelVersion.Caption := 'Version ' + ver + ' (64-bit)';
  LabelVersion.Font.Height := -14;
  LabelVersion.Font.Style := [];

  // Copyright Rubio Terra
  Label2 := TLabel.Create(Self);
  Label2.Parent := Self;
  Label2.Left := 0;
  Label2.Top := 66;
  Label2.Width := 480;
  Label2.Height := 18;
  Label2.Alignment := taCenter;
  Label2.AutoSize := False;
  Label2.Caption := 'Copyright Rúbio R. C. Terra <rubio.terra@gmail.com>';
  Label2.Font.Height := -13;
  Label2.Font.Style := [];

  // Rubio GitHub
  LabelRubioURL := TLabel.Create(Self);
  LabelRubioURL.Parent := Self;
  LabelRubioURL.Left := 0;
  LabelRubioURL.Top := 86;
  LabelRubioURL.Width := 480;
  LabelRubioURL.Height := 18;
  LabelRubioURL.Alignment := taCenter;
  LabelRubioURL.AutoSize := False;
  LabelRubioURL.Caption := 'https://github.com/rubiot/ibiblia';
  LabelRubioURL.Cursor := crHandPoint;
  LabelRubioURL.Font.Height := -13;
  LabelRubioURL.Font.Style := [];
  LabelRubioURL.OnClick := @LabelRubioURLClick;

  // Contributor Edmund Valdez
  LabelContributor := TLabel.Create(Self);
  LabelContributor.Parent := Self;
  LabelContributor.Left := 0;
  LabelContributor.Top := 112;
  LabelContributor.Width := 480;
  LabelContributor.Height := 18;
  LabelContributor.Alignment := taCenter;
  LabelContributor.AutoSize := False;
  LabelContributor.Caption := 'Contributor: Edmund Valdez';
  LabelContributor.Font.Height := -13;
  LabelContributor.Font.Style := [];

  // Edmund GitHub
  Label3 := TLabel.Create(Self);
  Label3.Parent := Self;
  Label3.Left := 0;
  Label3.Top := 132;
  Label3.Width := 480;
  Label3.Height := 18;
  Label3.Alignment := taCenter;
  Label3.AutoSize := False;
  Label3.Caption := 'https://github.com/SalitaNgDiyos';
  Label3.Cursor := crHandPoint;
  Label3.Font.Height := -13;
  Label3.Font.Style := [];
  Label3.OnClick := @Label3Click;

  // Buttons
  ButtonChangelog := TButton.Create(Self);
  ButtonChangelog.Parent := Self;
  ButtonChangelog.Left := 110;
  ButtonChangelog.Top := 180;
  ButtonChangelog.Width := 140;
  ButtonChangelog.Height := 32;
  ButtonChangelog.Caption := 'View Changelog';
  ButtonChangelog.OnClick := @ButtonChangelogClick;

  ButtonClose := TButton.Create(Self);
  ButtonClose.Parent := Self;
  ButtonClose.Left := 260;
  ButtonClose.Top := 180;
  ButtonClose.Width := 100;
  ButtonClose.Height := 32;
  ButtonClose.Caption := 'Close';
  ButtonClose.Cancel := True;
  ButtonClose.Default := True;
  ButtonClose.ModalResult := mrOk;
  ButtonClose.OnClick := @ButtonCloseClick;
end;

procedure TFormAbout.ButtonChangelogClick(Sender: TObject);
begin
  ShowChangelog(Self);
end;

procedure TFormAbout.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.Label3Click(Sender: TObject);
begin
  OpenURL('https://github.com/SalitaNgDiyos');
end;

procedure TFormAbout.LabelRubioURLClick(Sender: TObject);
begin
  OpenURL('https://github.com/rubiot/ibiblia');
end;

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
    lbl.Font.Style := [];
    lbl.Caption := 'iBiblia v1.0.0.70 - Changelog';

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

end.
