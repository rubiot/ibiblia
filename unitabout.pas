unit unitabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Clipbrd, Math, fileinfo, LCLIntf, LCLType, unitskins;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    Label1: TLabel;
    LabelVersion: TLabel;
    Label2: TLabel;
    LabelRubioURL: TLabel;
    LabelContributor: TLabel;
    Label3: TLabel;
    ButtonUserGuide: TButton;
    ButtonChangelog: TButton;
    ButtonClose: TButton;
    procedure ButtonUserGuideClick(Sender: TObject);
    procedure ButtonChangelogClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure LabelRubioURLClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySkin(const ASkin: TSkinColors);
  end;

  { TFormChangelog }

  TFormChangelog = class(TForm)
    lblTitle: TLabel;
    memoChangelog: TMemo;
    btnOK: TButton;
    procedure FormShow(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySkin(const ASkin: TSkinColors);
  end;

  { TFormUserGuide }

  TFormUserGuide = class(TForm)
    pnlHeader: TPanel;
    lblHeaderTitle: TLabel;
    lblHeaderSub: TLabel;
    pnlBody: TPanel;
    pnlSidebar: TPanel;
    pbSidebar: TPaintBox;
    pnlContent: TPanel;
    pnlIllust: TPanel;
    pbIllust: TPaintBox;
    memoGuide: TMemo;
    pnlFooter: TPanel;
    btnPrev: TButton;
    btnNext: TButton;
    btnCopy: TButton;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PbSidebarPaint(Sender: TObject);
    procedure PbSidebarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PbSidebarMouseLeave(Sender: TObject);
    procedure PbSidebarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PbIllustPaint(Sender: TObject);
    procedure BtnPrevClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
  private
    FSelectedTopic: Integer;
    FHotTopic: Integer;
    FLang: string;
    function GetTopicNavTitle(Idx: Integer): string;
    function GetTopicVersionBadge(Idx: Integer): string;
    function GetTopicHeaderTitle(Idx: Integer): string;
    function GetTopicGuideText(Idx: Integer): string;
    procedure SelectTopic(Idx: Integer);
    procedure LayoutFooterButtons;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySkin(const ASkin: TSkinColors);
    procedure UpdateLanguage(const ALang: string);
  end;

procedure ShowChangelog(AOwner: TComponent);
procedure ShowUserGuide(AOwner: TComponent);

var
  FormAbout: TFormAbout;
  FormUserGuide: TFormUserGuide;

implementation

{$R *.lfm}

const
  GUIDE_TOPIC_COUNT = 9;

  CHANGELOG_TEXT =
    'iBiblia Version 1.0.1.040 (64-bit Edition)' + sLineBreak +
    'Release Date: 2026-10-28' + sLineBreak +
    '==========================================' + sLineBreak + sLineBreak +
    'NEW FEATURES & UPDATES (v1.0.1.040):' + sLineBreak +
    '  * Upstream v1.1.0 Reliability & Workflow Improvements (Non-Destructive Integration):' + sLineBreak +
    '    - Export Modules While Open in theWord: Staged file publishing with automatic shared-stream overwrite fallback and byte-backup rollback allows exporting .ont/.ot/.nt and .dct.twm modules while open in theWord.' + sLineBreak +
    '    - Safe Project "Save As..." (File > Save as... / Ctrl+Shift+S): Commits active work, stages a transactionally clean .bib copy, updates Recent Projects (MRU), and switches to the new project.' + sLineBreak +
    '    - Per-Project Remembered Export Destinations: Automatically remembers and pre-populates separate export paths per project for Tagged Destination, Interlinear, Analytical/Synthetic Concordance (.dct.twm), and all 4 Bible panels.' + sLineBreak +
    '    - Smart Apostrophe Variant Matching: Normalizes curly/typographic apostrophes (U+2019, U+2018, U+02BC, etc.) to straight single quotes across word associations, suggestions, and smart reload diffs.' + sLineBreak +
    '    - Active Translation Highlight in Translation Memory: Highlights the current verse''s associated translation ([current]) inside the Translation Memory popup list.' + sLineBreak +
    '    - Automatic Pruning of Empty Formatting Tags: Strips empty wrapper tags (<FI><Fi>, <FR><Fr>, <FO><Fo>, <b></b>, <TS1><Ts>, etc.) left behind after deleting text in Edit Mode.' + sLineBreak +
    '    - 2026 Book Module Guide Uncompressed BLOB Concordance Export: Stores uncompressed RTF BLOB data and complete config metadata in exported .dct.twm concordances.' + sLineBreak +
    '    - Fixed reversed Pos() check in ExportarTextoDestinoComStrongs so <_MORPH_> is only appended when verse 1 has no existing tags.' + sLineBreak +
    '  * Context Panel Shows Unassociated Destination Words as Italics on File Save:' + sLineBreak +
    '    - Saving the project (Ctrl+S / File > Save) automatically renders unassociated words in the Destination tab of the Context Panel in italics (<FI>...<Fi>).' + sLineBreak +
    '  * Restored "Sync theWord to iBiblia verse" (v1.0.1.033):' + sLineBreak +
    '    - Fixed WM_COPYDATA verse broadcast and bidirectional loop guard so navigating in iBiblia immediately syncs theWord 7.' + sLineBreak +
    '  * Interactive Floating "New Features User Guide" Window (Help > New Features User Guide... / F1):' + sLineBreak +
    '    - Dedicated modeless floating window collating all new features introduced since v1.0.0.71.' + sLineBreak +
    '    - Includes 8 custom-drawn visual diagrams/illustrations and step-by-step instructions.' + sLineBreak +
    '  * Mouse Wheel Word Selection in Source Text & Multi-Word Ctrl+Shift Association:' + sLineBreak +
    '    - Scroll Up selects words to the left and Scroll Down selects words to the right one by one in the Source text panel whenever the mouse cursor is inside either the Source text panel or the Destination text panel.' + sLineBreak +
    '    - Full Ctrl+Shift multi-word selection across both Source and Destination panels for associating multiple words simultaneously.' + sLineBreak +
    '  * Export Error Remediation & Self-Healing Association Integrity:' + sLineBreak +
    '    - Automatically detects and repairs misplaced pare_pares records on project open and export.' + sLineBreak +
    '    - Added FLoadedVerseID tracking, reference-counted StartScrollingSession/FinishScrollingSession, and tree selection guards.' + sLineBreak +
    '  * Hover-Only Verse Navigation Chevron Arrows Across All 4 Bibles:' + sLineBreak +
    '    - Verse navigation chevron arrows (< >) appear exclusively inside the Bible panel currently hovered by the mouse cursor.' + sLineBreak +
    '  * Automatic Next Unassociated Source Word Selection on Ctrl+Shift Release:' + sLineBreak +
    '    - Releasing Ctrl+Shift after associating Destination words automatically highlights the next unassociated word in the Source text.' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.1.000 - v1.0.1.023:' + sLineBreak +
    '  * Dark Mode UI & 7 theWord Skins Integration (View > Skins / Ctrl+Shift+D).' + sLineBreak +
    '  * Layout 3 Shared Master Formatting Ribbon, Floating Selection Mini-Toolbar & Live Context Preview.' + sLineBreak +
    '  * Southeast Cursor Dictionary Popup in Destination Panel & English Word Dictionary (*.dct.twm) Slot.' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.0.71 - v1.0.0.79:' + sLineBreak +
    '  * Compressed Dictionary Modules (compressed=1) Support & Automatic theWord Font Loading.' + sLineBreak +
    '  * Per-Box Translation Name Badge, 4-Circle Status Bar & Edit/Properties Mode Checkboxes.' + sLineBreak +
    '  * Complete Bible Module Export (.ont, .ot, .nt) via Right-Click Context Menu.' + sLineBreak +
    '  * Smart "Needs Review" Status Transition on Text Reload & Persistent Verse Filter.' + sLineBreak +
    '  * Streamlined Default Dictionaries (Greek Strong''s, Hebrew Strong''s, Morphology, English).' + sLineBreak +
    '  * Real-Time Text-to-Speech (TTS) Voice Reading on Hover (Options > Voice Settings / Ctrl+T).' + sLineBreak +
    '  * Concurrent Bidirectional theWord 7 Synchronization.' + sLineBreak + sLineBreak +
    'CONTRIBUTORS:' + sLineBreak +
    '  * Edmund Valdez: https://github.com/SalitaNgDiyos' + sLineBreak +
    '  * Rúbio R. C. Terra: https://github.com/rubiot/ibiblia' + sLineBreak + sLineBreak +
    '------------------------------------------';

{ TFormAbout }

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
  OnShow := @FormShow;

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
  Label1.Font.Style := [fsBold];

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
    ver := '1.0.1.040';

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
  ButtonUserGuide := TButton.Create(Self);
  ButtonUserGuide.Parent := Self;
  ButtonUserGuide.Left := 44;
  ButtonUserGuide.Top := 180;
  ButtonUserGuide.Width := 130;
  ButtonUserGuide.Height := 32;
  ButtonUserGuide.Caption := 'User Guide (F1)';
  ButtonUserGuide.OnClick := @ButtonUserGuideClick;

  ButtonChangelog := TButton.Create(Self);
  ButtonChangelog.Parent := Self;
  ButtonChangelog.Left := 184;
  ButtonChangelog.Top := 180;
  ButtonChangelog.Width := 140;
  ButtonChangelog.Height := 32;
  ButtonChangelog.Caption := 'View Changelog';
  ButtonChangelog.OnClick := @ButtonChangelogClick;

  ButtonClose := TButton.Create(Self);
  ButtonClose.Parent := Self;
  ButtonClose.Left := 334;
  ButtonClose.Top := 180;
  ButtonClose.Width := 100;
  ButtonClose.Height := 32;
  ButtonClose.Caption := 'Close';
  ButtonClose.Cancel := True;
  ButtonClose.Default := True;
  ButtonClose.ModalResult := mrOk;
  ButtonClose.OnClick := @ButtonCloseClick;

  ApplySkin(CurrentSkin);
end;

procedure TFormAbout.ApplySkin(const ASkin: TSkinColors);
begin
  Color := ASkin.PanelBg;
  ApplySkinToForm(Self, ASkin);
  {$IFDEF WINDOWS}
  if HandleAllocated then
    ApplyDarkModeToWindow(Self, ASkin.IsDark);
  if Assigned(ButtonUserGuide) and ButtonUserGuide.HandleAllocated then
    HookButtonControl(ButtonUserGuide);
  if Assigned(ButtonChangelog) and ButtonChangelog.HandleAllocated then
    HookButtonControl(ButtonChangelog);
  if Assigned(ButtonClose) and ButtonClose.HandleAllocated then
    HookButtonControl(ButtonClose);
  {$ENDIF}
  if ASkin.Id <> skClassic then
  begin
    Color := ASkin.PanelBg;
    if Assigned(Label1) then
      Label1.Font.Color := ASkin.HeaderColor;
    if Assigned(LabelVersion) then
      LabelVersion.Font.Color := ASkin.PanelText;
    if Assigned(Label2) then
      Label2.Font.Color := ASkin.PanelText;
    if Assigned(LabelRubioURL) then
      LabelRubioURL.Font.Color := ASkin.HeaderColor;
    if Assigned(LabelContributor) then
      LabelContributor.Font.Color := ASkin.PanelText;
    if Assigned(Label3) then
      Label3.Font.Color := ASkin.HeaderColor;
    if Assigned(ButtonUserGuide) then
      ButtonUserGuide.Font.Color := ASkin.PanelText;
    if Assigned(ButtonChangelog) then
      ButtonChangelog.Font.Color := ASkin.PanelText;
    if Assigned(ButtonClose) then
      ButtonClose.Font.Color := ASkin.PanelText;
  end
  else
  begin
    Color := clBtnFace;
    if Assigned(Label1) then
      Label1.Font.Color := clWindowText;
    if Assigned(LabelVersion) then
      LabelVersion.Font.Color := clWindowText;
    if Assigned(Label2) then
      Label2.Font.Color := clWindowText;
    if Assigned(LabelRubioURL) then
      LabelRubioURL.Font.Color := clBlue;
    if Assigned(LabelContributor) then
      LabelContributor.Font.Color := clWindowText;
    if Assigned(Label3) then
      Label3.Font.Color := clBlue;
    if Assigned(ButtonUserGuide) then
      ButtonUserGuide.Font.Color := clWindowText;
    if Assigned(ButtonChangelog) then
      ButtonChangelog.Font.Color := clWindowText;
    if Assigned(ButtonClose) then
      ButtonClose.Font.Color := clWindowText;
  end;
  if Assigned(ButtonUserGuide) then ButtonUserGuide.Invalidate;
  if Assigned(ButtonChangelog) then ButtonChangelog.Invalidate;
  if Assigned(ButtonClose) then ButtonClose.Invalidate;
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  ApplySkin(CurrentSkin);
  {$IFDEF WINDOWS}
  ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  if Assigned(ButtonUserGuide) then
    HookButtonControl(ButtonUserGuide);
  if Assigned(ButtonChangelog) then
    HookButtonControl(ButtonChangelog);
  if Assigned(ButtonClose) then
    HookButtonControl(ButtonClose);
  {$ENDIF}
  if Assigned(ButtonUserGuide) then ButtonUserGuide.Invalidate;
  if Assigned(ButtonChangelog) then ButtonChangelog.Invalidate;
  if Assigned(ButtonClose) then ButtonClose.Invalidate;
end;

procedure TFormAbout.ButtonUserGuideClick(Sender: TObject);
begin
  if Sender = nil then ;
  Close;
  ShowUserGuide(Application.MainForm);
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

{ TFormChangelog }

constructor TFormChangelog.Create(AOwner: TComponent);
var
  histFile: string;
begin
  inherited CreateNew(AOwner);

  Caption := 'What''s New in iBiblia';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  ClientWidth := 560;
  ClientHeight := 430;
  Color := CurrentSkin.PanelBg;
  OnShow := @FormShow;

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := Self;
  lblTitle.Left := 16;
  lblTitle.Top := 12;
  lblTitle.Font.Height := -16;
  lblTitle.Font.Style := [fsBold];
  lblTitle.Caption := 'iBiblia v1.0.1.040 - Changelog';
  if IsThemedSkin then
    lblTitle.Font.Color := CurrentSkin.HeaderColor
  else
    lblTitle.Font.Color := clWindowText;

  memoChangelog := TMemo.Create(Self);
  memoChangelog.Parent := Self;
  memoChangelog.Left := 16;
  memoChangelog.Top := 38;
  memoChangelog.Width := 528;
  memoChangelog.Height := 335;
  memoChangelog.ReadOnly := True;
  memoChangelog.ScrollBars := ssAutoVertical;
  memoChangelog.Font.Name := 'Segoe UI';
  memoChangelog.Font.Size := 9;
  if IsThemedSkin then
  begin
    memoChangelog.Color := CurrentSkin.WindowBg;
    memoChangelog.Font.Color := CurrentSkin.WindowText;
  end
  else
  begin
    memoChangelog.Color := clWindow;
    memoChangelog.Font.Color := clWindowText;
  end;

  histFile := ExtractFilePath(Application.ExeName) + 'historico.txt';
  if not FileExists(histFile) then
    histFile := ExtractFilePath(Application.ExeName) + 'Histórico.txt';

  if FileExists(histFile) then
  begin
    try
      memoChangelog.Lines.LoadFromFile(histFile);
    except
      memoChangelog.Text := CHANGELOG_TEXT;
    end;
  end
  else
    memoChangelog.Text := CHANGELOG_TEXT;

  btnOK := TButton.Create(Self);
  btnOK.Parent := Self;
  btnOK.Width := 100;
  btnOK.Height := 30;
  btnOK.Left := (ClientWidth - btnOK.Width) div 2;
  btnOK.Top := 383;
  btnOK.Caption := 'OK';
  btnOK.ModalResult := mrOk;
  btnOK.Default := True;
  btnOK.OnClick := @BtnOKClick;
  if IsThemedSkin then
    btnOK.Font.Color := CurrentSkin.PanelText
  else
    btnOK.Font.Color := clWindowText;

  ApplySkin(CurrentSkin);
end;

procedure TFormChangelog.BtnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

procedure TFormChangelog.ApplySkin(const ASkin: TSkinColors);
begin
  Color := ASkin.PanelBg;
  ApplySkinToForm(Self, ASkin);
  {$IFDEF WINDOWS}
  if HandleAllocated then
    ApplyDarkModeToWindow(Self, ASkin.IsDark);
  if Assigned(btnOK) and btnOK.HandleAllocated then
    HookButtonControl(btnOK);
  {$ENDIF}
  if ASkin.Id <> skClassic then
  begin
    Color := ASkin.PanelBg;
    if Assigned(lblTitle) then
      lblTitle.Font.Color := ASkin.HeaderColor;
    if Assigned(memoChangelog) then
    begin
      memoChangelog.Color := ASkin.WindowBg;
      memoChangelog.Font.Color := ASkin.WindowText;
    end;
    if Assigned(btnOK) then
      btnOK.Font.Color := ASkin.PanelText;
  end
  else
  begin
    Color := clBtnFace;
    if Assigned(lblTitle) then
      lblTitle.Font.Color := clWindowText;
    if Assigned(memoChangelog) then
    begin
      memoChangelog.Color := clWindow;
      memoChangelog.Font.Color := clWindowText;
    end;
    if Assigned(btnOK) then
      btnOK.Font.Color := clWindowText;
  end;
  if Assigned(btnOK) then btnOK.Invalidate;
end;

procedure TFormChangelog.FormShow(Sender: TObject);
begin
  ApplySkin(CurrentSkin);
  {$IFDEF WINDOWS}
  ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  if Assigned(btnOK) then
    HookButtonControl(btnOK);
  {$ENDIF}
  if IsThemedSkin then
  begin
    Color := CurrentSkin.PanelBg;
    if Assigned(lblTitle) then
      lblTitle.Font.Color := CurrentSkin.HeaderColor;
    if Assigned(btnOK) then
      btnOK.Font.Color := CurrentSkin.PanelText;
  end;
  if Assigned(btnOK) then btnOK.Invalidate;
end;

procedure ShowChangelog(AOwner: TComponent);
var
  dlg: TFormChangelog;
begin
  dlg := TFormChangelog.Create(AOwner);
  try
    dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;

{ TFormUserGuide }

constructor TFormUserGuide.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  Caption := 'iBiblia — New Features User Guide (Since v1.0.0.71)';
  BorderStyle := bsSizeable;
  BorderIcons := [biSystemMenu, biMinimize, biMaximize];
  FormStyle := fsStayOnTop;
  Position := poMainFormCenter;
  ClientWidth := 860;
  ClientHeight := 590;
  Constraints.MinWidth := 720;
  Constraints.MinHeight := 480;
  KeyPreview := True;
  OnShow := @FormShow;
  OnResize := @FormResize;
  OnKeyDown := @FormKeyDown;

  FSelectedTopic := 0;
  FHotTopic := -1;
  FLang := 'en';

  // 1. Header Panel
  pnlHeader := TPanel.Create(Self);
  pnlHeader.Parent := Self;
  pnlHeader.Align := alTop;
  pnlHeader.Height := 52;
  pnlHeader.BevelOuter := bvNone;
  pnlHeader.BorderStyle := bsNone;

  lblHeaderTitle := TLabel.Create(pnlHeader);
  lblHeaderTitle.Parent := pnlHeader;
  lblHeaderTitle.Left := 16;
  lblHeaderTitle.Top := 7;
  lblHeaderTitle.Font.Name := 'Segoe UI';
  lblHeaderTitle.Font.Height := -17;
  lblHeaderTitle.Font.Style := [fsBold];
  lblHeaderTitle.Caption := 'iBiblia New Features User Guide (v1.0.0.71 – v1.0.1.xxx)';

  lblHeaderSub := TLabel.Create(pnlHeader);
  lblHeaderSub.Parent := pnlHeader;
  lblHeaderSub.Left := 16;
  lblHeaderSub.Top := 30;
  lblHeaderSub.Font.Name := 'Segoe UI';
  lblHeaderSub.Font.Height := -12;
  lblHeaderSub.Caption := 'Floating Interactive Guide — Keep this window open while practicing features directly in iBiblia.';

  // 2. Footer Panel
  pnlFooter := TPanel.Create(Self);
  pnlFooter.Parent := Self;
  pnlFooter.Align := alBottom;
  pnlFooter.Height := 44;
  pnlFooter.BevelOuter := bvNone;
  pnlFooter.BorderStyle := bsNone;

  btnPrev := TButton.Create(pnlFooter);
  btnPrev.Parent := pnlFooter;
  btnPrev.Width := 130;
  btnPrev.Height := 30;
  btnPrev.Top := 7;
  btnPrev.Left := 14;
  btnPrev.Caption := '< Previous Topic';
  btnPrev.OnClick := @BtnPrevClick;

  btnNext := TButton.Create(pnlFooter);
  btnNext.Parent := pnlFooter;
  btnNext.Width := 130;
  btnNext.Height := 30;
  btnNext.Top := 7;
  btnNext.Left := 152;
  btnNext.Caption := 'Next Topic >';
  btnNext.OnClick := @BtnNextClick;

  btnCopy := TButton.Create(pnlFooter);
  btnCopy.Parent := pnlFooter;
  btnCopy.Width := 145;
  btnCopy.Height := 30;
  btnCopy.Top := 7;
  btnCopy.Left := 290;
  btnCopy.Caption := 'Copy Guide Text';
  btnCopy.OnClick := @BtnCopyClick;

  btnClose := TButton.Create(pnlFooter);
  btnClose.Parent := pnlFooter;
  btnClose.Width := 100;
  btnClose.Height := 30;
  btnClose.Top := 7;
  btnClose.Caption := 'Close';
  btnClose.Cancel := True;
  btnClose.OnClick := @BtnCloseClick;

  // 3. Body Container
  pnlBody := TPanel.Create(Self);
  pnlBody.Parent := Self;
  pnlBody.Align := alClient;
  pnlBody.BevelOuter := bvNone;
  pnlBody.BorderStyle := bsNone;

  // 4. Left Sidebar (Owner-Drawn Feature Topic Selector)
  pnlSidebar := TPanel.Create(pnlBody);
  pnlSidebar.Parent := pnlBody;
  pnlSidebar.Align := alLeft;
  pnlSidebar.Width := 250;
  pnlSidebar.BevelOuter := bvNone;
  pnlSidebar.BorderStyle := bsNone;

  pbSidebar := TPaintBox.Create(pnlSidebar);
  pbSidebar.Parent := pnlSidebar;
  pbSidebar.Align := alClient;
  pbSidebar.Cursor := crHandPoint;
  pbSidebar.OnPaint := @PbSidebarPaint;
  pbSidebar.OnMouseMove := @PbSidebarMouseMove;
  pbSidebar.OnMouseLeave := @PbSidebarMouseLeave;
  pbSidebar.OnMouseDown := @PbSidebarMouseDown;

  // 5. Right Content Area (Top Visual Illustration + Bottom Step-by-Step Guide)
  pnlContent := TPanel.Create(pnlBody);
  pnlContent.Parent := pnlBody;
  pnlContent.Align := alClient;
  pnlContent.BevelOuter := bvNone;
  pnlContent.BorderStyle := bsNone;
  pnlContent.BorderSpacing.Left := 8;
  pnlContent.BorderSpacing.Right := 10;
  pnlContent.BorderSpacing.Bottom := 4;

  pnlIllust := TPanel.Create(pnlContent);
  pnlIllust.Parent := pnlContent;
  pnlIllust.Align := alTop;
  pnlIllust.Height := 215;
  pnlIllust.BevelOuter := bvNone;
  pnlIllust.BorderStyle := bsNone;
  pnlIllust.BorderSpacing.Bottom := 6;

  pbIllust := TPaintBox.Create(pnlIllust);
  pbIllust.Parent := pnlIllust;
  pbIllust.Align := alClient;
  pbIllust.OnPaint := @PbIllustPaint;

  memoGuide := TMemo.Create(pnlContent);
  memoGuide.Parent := pnlContent;
  memoGuide.Align := alClient;
  memoGuide.ReadOnly := True;
  memoGuide.ScrollBars := ssAutoVertical;
  memoGuide.WordWrap := True;
  memoGuide.Font.Name := 'Segoe UI';
  memoGuide.Font.Size := 10;

  LayoutFooterButtons;
  SelectTopic(0);
  ApplySkin(CurrentSkin);
end;

procedure TFormUserGuide.LayoutFooterButtons;
begin
  if Assigned(btnClose) and Assigned(pnlFooter) then
    btnClose.Left := Max(445, pnlFooter.ClientWidth - btnClose.Width - 14);
end;

procedure TFormUserGuide.FormResize(Sender: TObject);
begin
  if Sender = nil then ;
  LayoutFooterButtons;
  if Assigned(pbSidebar) then pbSidebar.Invalidate;
  if Assigned(pbIllust) then pbIllust.Invalidate;
end;

procedure TFormUserGuide.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Sender = nil then ;
  if Shift = [] then ;
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    Close;
  end
  else if (Key = VK_PRIOR) and (FSelectedTopic > 0) then
  begin
    Key := 0;
    SelectTopic(FSelectedTopic - 1);
  end
  else if (Key = VK_NEXT) and (FSelectedTopic < GUIDE_TOPIC_COUNT - 1) then
  begin
    Key := 0;
    SelectTopic(FSelectedTopic + 1);
  end;
end;

function TFormUserGuide.GetTopicNavTitle(Idx: Integer): string;
begin
  case Idx of
    0: Result := '1. Wheel & Ctrl+Shift Assoc.';
    1: Result := '2. Southeast & English Dicts';
    2: Result := '3. Chevrons, Status & Tabs';
    3: Result := '4. Formatting Ribbon & Edit';
    4: Result := '5. Text-to-Speech (TTS)';
    5: Result := '6. Dark Mode & 7 Skins';
    6: Result := '7. Module Export & Sync';
    7: Result := '8. Status Filter & Find (Ctrl+F)';
    8: Result := '9. Full Feature Summary';
  else
    Result := '';
  end;
end;

function TFormUserGuide.GetTopicVersionBadge(Idx: Integer): string;
begin
  case Idx of
    0: Result := 'v1.0.1.024 – v1.0.1.030';
    1: Result := 'v1.0.0.74 – v1.0.1.017';
    2: Result := 'v1.0.0.76 – v1.0.1.026';
    3: Result := 'v1.0.1.002 – v1.0.1.034';
    4: Result := 'v1.0.0.73 – v1.0.0.74';
    5: Result := 'v1.0.0.80 – v1.0.1.023';
    6: Result := 'v1.0.0.71 – v1.0.1.035';
    7: Result := 'v1.0.0.70 – v1.0.0.76';
    8: Result := 'All Features Since v1.0.0.70';
  else
    Result := '';
  end;
end;

function TFormUserGuide.GetTopicHeaderTitle(Idx: Integer): string;
begin
  case Idx of
    0: Result := '1. Mouse Wheel Source Word Selection & Ctrl+Shift Multi-Word Association';
    1: Result := '2. Southeast Cursor Dictionary Popup & 4-Slot Default Dictionaries';
    2: Result := '3. In-Verse Controls: Hover Navigation Chevrons, 4-Dot Status Bar & Mode Tabs';
    3: Result := '4. Shared Master Formatting Ribbon, Floating Mini-Toolbar & Live Preview';
    4: Result := '5. Real-Time Text-to-Speech (TTS) Hover Reader (Greek, Hebrew & English)';
    5: Result := '6. Dark Mode UI & 7 theWord Color Themes (Classic, Dark, Night, Sepia, Slate...)';
    6: Result := '7. Complete Bible Module Export, Smart Reload Review & theWord 7 Sync';
    7: Result := '8. Verse Status Filtering (F8 / Shift+F8) & Find in Project (Ctrl+F)';
    8: Result := '9. Complete Quick-Reference Summary of All New Features Since v1.0.0.70';
  else
    Result := '';
  end;
end;

function TFormUserGuide.GetTopicGuideText(Idx: Integer): string;
var
  i: Integer;
begin
  case Idx of
    0:
      Result :=
        'FEATURE OVERVIEW (v1.0.1.024 – v1.0.1.030)' + sLineBreak +
        'Select words in the Source text hands-free using the mouse scroll wheel from either the Source or Destination panel, associate single or multiple words with Ctrl+Shift, and let iBiblia automatically advance to the next unassociated Source word when you release Ctrl+Shift.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Mouse Wheel Word Selection (Left / Right):' + sLineBreak +
        '     • Place your mouse cursor inside EITHER the Source text panel OR the Destination text panel.' + sLineBreak +
        '     • Scroll UP (away from you): Selects the previous word to the LEFT in the Source text one by one.' + sLineBreak +
        '     • Scroll DOWN (toward you): Selects the next word to the RIGHT in the Source text one by one.' + sLineBreak +
        '     • If a word is already associated, its paired Destination word(s) highlight automatically so you can inspect existing associations.' + sLineBreak + sLineBreak +
        '  2. Multi-Word Association & Auto-Advance (Ctrl + Shift):' + sLineBreak +
        '     • Select one word (via Mouse Wheel or Left-Click) or multiple words (Hold Ctrl + Left-Click) in the Source text.' + sLineBreak +
        '     • Hold down CTRL + SHIFT and Left-Click one or more target words in the Destination text to associate them together as a phrase.' + sLineBreak +
        '     • Release the CTRL + SHIFT keys: iBiblia automatically clears the current selection and highlights the NEXT unassociated word in the Source text!' + sLineBreak + sLineBreak +
        '  3. Instant Single-Word Association (Right-Click):' + sLineBreak +
        '     • With a Source word selected, simply Right-Click a single target word in the Destination text to associate it and immediately jump to the next unassociated Source word.' + sLineBreak + sLineBreak +
        'SHORTCUT SUMMARY:' + sLineBreak +
        '  • Mouse Wheel Up (in Source or Dest)   -> Select Source word to the LEFT' + sLineBreak +
        '  • Mouse Wheel Down (in Source or Dest) -> Select Source word to the RIGHT' + sLineBreak +
        '  • Ctrl + Mouse Wheel                   -> Zoom panel font size in/out' + sLineBreak +
        '  • Hold Ctrl + Shift + Left-Click       -> Select/associate multiple words at once' + sLineBreak +
        '  • Release Ctrl + Shift                 -> Auto-select next unassociated Source word';

    1:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.74 – v1.0.1.017)' + sLineBreak +
        'Streamlined global dictionary management supporting compressed (.dct.twm) modules, automatic Greek/Hebrew font loading, English Word/Bible dictionary lookups on untagged words, and an automatic Southeast Cursor Dictionary Popup while translating inside the Destination panel.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Southeast Cursor Dictionary Popup in Destination Panel:' + sLineBreak +
        '     • Select an unassociated word in the Source text (or let auto-advance select it).' + sLineBreak +
        '     • Move your mouse cursor into the Destination text panel: the Strong''s & Morphology dictionary popup automatically floats 18px southeast of your cursor so you can read the Greek/Hebrew definition while choosing target words.' + sLineBreak +
        '     • Hover over any ALREADY-ASSOCIATED word in the Destination panel to temporarily inspect that word''s own dictionary entry; move off it to resume the selected Source word''s popup.' + sLineBreak +
        '     • Hold CTRL while the popup is visible to freeze its position so you can move the cursor inside the popup to scroll or zoom.' + sLineBreak + sLineBreak +
        '  2. Configure 4 Global Default Dictionaries (Options > Default Dictionaries...):' + sLineBreak +
        '     • Greek Strong''s Dictionary: Used for NT (Books 40–66) and LXX Greek G-codes.' + sLineBreak +
        '     • Hebrew Strong''s Dictionary: Used for OT (Books 1–39) and Hebrew H-codes (supports raw, zero-padded H0123, compound Hc/H7225, and redirect entries).' + sLineBreak +
        '     • Morphology Dictionary: Displays grammatical parsing for Robinson, Packard, and LXX morphology tags.' + sLineBreak +
        '     • English Word / Bible Dictionary: Hover over ANY untagged English word or proper name in any of the 4 Bible panels to view its English dictionary definition (with automatic plural/verb stem matching).' + sLineBreak + sLineBreak +
        '  3. Compressed Modules & Per-Module Override:' + sLineBreak +
        '     • Compressed theWord dictionaries (compressed=1) are decompressed on the fly.' + sLineBreak +
        '     • In Project > Settings, check or uncheck "Use Default Dictionaries (overrides above)" per Bible module.';

    2:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.76 – v1.0.1.026)' + sLineBreak +
        'Every Bible panel features clean, context-sensitive in-verse controls for navigating verses, tracking association status, identifying translations, and switching between Association, Edit, and Properties modes.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Hover-Only Verse Navigation Chevrons (< >):' + sLineBreak +
        '     • Move your mouse pointer inside any of the 4 Bible panels (Source, Destination, Reference 1, or Reference 2).' + sLineBreak +
        '     • Subtle "<" (Previous Verse) and ">" (Next Verse) chevron arrows appear right after the end of the verse text ONLY in the Bible panel your mouse is currently inside, and disappear in all other panels.' + sLineBreak +
        '     • Click "<" to go to the previous verse or ">" to advance to the next verse.' + sLineBreak + sLineBreak +
        '  2. Interactive 4-Color Verse Status Circles (Destination Bottom Bar):' + sLineBreak +
        '     • Located in the center of the Destination panel''s bottom bar (and synced with keys 1–4):' + sLineBreak +
        '       - Circle 1 (Gray):   Not associated (Key: 1)' + sLineBreak +
        '       - Circle 2 (Yellow): Associating    (Key: 2)' + sLineBreak +
        '       - Circle 3 (Red):    Needs review   (Key: 3)' + sLineBreak +
        '       - Circle 4 (Green):  Associated!    (Key: 4)' + sLineBreak + sLineBreak +
        '  3. Translation Name Badge & Edit / Properties Checkboxes:' + sLineBreak +
        '     • Bottom-Left Badge: Displays the active module''s title, short.title, or description (updates live as you edit module properties).' + sLineBreak +
        '     • Bottom-Right Checkboxes:' + sLineBreak +
        '       - Both Unchecked: Normal Word Association & Alignment mode.' + sLineBreak +
        '       - [x] Edit:       Opens the in-place verse editor and Master Formatting Ribbon.' + sLineBreak +
        '       - [x] Properties: Opens the full theWord module metadata/header editor.';

    3:
      Result :=
        'FEATURE OVERVIEW (v1.0.1.002 – v1.0.1.034)' + sLineBreak +
        'When "Edit" is checked on any Bible panel, iBiblia provides a Shared Master Formatting Ribbon, a Floating Selection Mini-Toolbar, intuitive keyboard shortcuts for paragraph breaks, and real-time rendering in the right-hand Context Panel.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Activating Edit Mode:' + sLineBreak +
        '     • Check the "Edit" checkbox at the bottom-right of any Bible panel (or double-click empty space in the verse panel).' + sLineBreak +
        '     • Uncheck "Edit" (or click another word/verse) to automatically flatten line breaks and save your changes.' + sLineBreak + sLineBreak +
        '  2. Paragraph, Indentation & Heading Controls (Master Ribbon):' + sLineBreak +
        '     • Paragraph Breaks: Press ENTER for <CI> (new line), DOUBLE ENTER for <CM> (new paragraph with blank line), or SHIFT+ENTER for <CL>.' + sLineBreak +
        '     • Indentation Step Buttons: Click PI- / PI+ (<PI0>..<PI7> paragraph indent) and PF- / PF+ (<PF0>..<PF7> first-line indent), or Center (<center>).' + sLineBreak +
        '     • Section Headings: Click TS1, TS2, or TS3 to wrap selected text in <TS1>..<Ts> section titles.' + sLineBreak + sLineBreak +
        '  3. Floating Selection Mini-Toolbar & Inline Tags:' + sLineBreak +
        '     • Highlight any word or phrase in the editor: a Floating Mini-Toolbar appears directly above your selection.' + sLineBreak +
        '     • Apply <FI>..<Fi> (Added/Italic words), <FR>..<Fr> (Words of Jesus in Red), <FO>..<Fo> (OT Quote), <b>, <i>, <u>, <sup>, <sub>, <RF>..<Rf> (Footnote), or click "Clear Tags" to strip markup from the selection.' + sLineBreak + sLineBreak +
        '  4. Live Context Panel Preview & Unassociated Words in Italics on File Save:' + sLineBreak +
        '     • Every tag and edit renders immediately in the right-hand Context Panel (Source | Destination | Ref 1 | Ref 2 | Interlinear).' + sLineBreak +
        '     • Upon saving the file (Ctrl+S / File > Save), the Context Panel''s Destination view automatically renders any unassociated Destination words in italics (<FI>...<Fi>) so you can immediately spot words added for English readability or not yet linked to a Greek/Hebrew source word.';

    4:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.73 – v1.0.0.74)' + sLineBreak +
        'Real-time Text-to-Speech (TTS) pronounces Greek, Hebrew, and English/vernacular words aloud when you hover over them, supporting both Windows 10/11 OneCore Speech Packs (e.g. Microsoft Stefanos for Greek, Microsoft Asaf for Hebrew) and classic SAPI5 voices.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Toggle Text-to-Speech On / Off:' + sLineBreak +
        '     • Press CTRL + T at any time, or click Options > Read word on hover (Text-to-Speech).' + sLineBreak + sLineBreak +
        '  2. Configure Voices, Speed, Volume & Trigger (Options > Voice / Speech Settings...):' + sLineBreak +
        '     • Source Text Voice: Choose a Greek voice (such as Microsoft Stefanos) or Hebrew voice (such as Microsoft Asaf) for the Source Bible panel.' + sLineBreak +
        '     • Translation / Destination Voice: Choose an English or local language voice for the Destination, Reference 1, and Reference 2 panels.' + sLineBreak +
        '     • Speech Rate & Volume: Adjust speed (-10 to +10) and volume (0% to 100%) using the themed sliders.' + sLineBreak +
        '     • Hover Trigger Condition: Choose whether speech plays Always on Mouse Hover, only on Ctrl + Hover, or only on Alt + Hover.' + sLineBreak +
        '     • Instant Test Buttons: Click "Test" next to either voice dropdown to hear a live sample immediately.';

    5:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.80 – v1.0.1.023)' + sLineBreak +
        'Full OS-level and custom-drawn theming across the entire application—including the Main Menu Bar, dropdown/popup menus, 4 Bible panels, TreeView, Context Panel, Dictionary Popup, and all modal dialogs—with 7 built-in theWord color schemes.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Choose a Theme (View > Skins (theWord Themes)):' + sLineBreak +
        '     • Classic (Light):        Standard bright light workspace.' + sLineBreak +
        '     • theWord Dark (Charcoal): Signature #1E1E1E charcoal reading panes with #252526 shell.' + sLineBreak +
        '     • Night (Pitch Black):    Deep #121212 OLED pitch-black background with golden highlights.' + sLineBreak +
        '     • Sepia (Parchment):      Eye-strain relief warm #F4EEDB parchment with #2D241E ink.' + sLineBreak +
        '     • Slate (Steel Blue):     Cool #1C232B navy-slate background with ice-blue text.' + sLineBreak +
        '     • Solarized Dark:         Precision cyan-dark #002B36 palette.' + sLineBreak +
        '     • High Contrast Dark:     Pure black & stark white with vivid cyan/yellow accents.' + sLineBreak + sLineBreak +
        '  2. Instant Keyboard Toggle (Ctrl + Shift + D):' + sLineBreak +
        '     • Press CTRL + SHIFT + D at any time to toggle between Dark Mode (theWord Dark) and Classic (Light).' + sLineBreak +
        '     • Your active theme is automatically saved in iBiblia.ini and restored on startup.';

    6:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.71 – v1.0.1.035)' + sLineBreak +
        'Includes safe Project Save As (Ctrl+Shift+S), exporting Bible and Concordance modules even while open in theWord, per-project remembered export destinations for every format, smart apostrophe variant matching, and uncompressed BLOB .dct.twm concordance generation.' + sLineBreak +
        'Export complete, ready-to-use theWord Bible modules (.ont, .ot, .nt) directly from any Bible panel''s right-click menu, rely on smart word-diff detection when reloading texts, and synchronize bidirectionally with 64-bit theWord 7.' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Complete Bible Module Export (Right-Click > Export as Bible module...):' + sLineBreak +
        '     • Right-click inside any of the 4 Bible panels (Source, Destination, Reference 1, or Reference 2) and click "Export as Bible module...".' + sLineBreak +
        '     • Automatically detects the module scope (.ont = 31,102 lines, .ot = 23,145 lines, .nt = 7,957 lines), writes the UTF-8 BOM, preserves 100% of module properties, and embeds all associated <WG...>/<WH...> and <WT...> tags when exporting the Destination Bible.' + sLineBreak + sLineBreak +
        '  2. Smart "Needs Review" Status on Text Reload & Self-Healing Integrity:' + sLineBreak +
        '     • When loading/replacing a Bible text in Project > Settings, iBiblia compares the actual words of each verse: verses where only Strong''s/morphology tags changed keep their "Associated!" (Green) status, while verses with modified words transition to "Needs review" (Red).' + sLineBreak +
        '     • Built-in self-healing database verification automatically validates and repairs association indices on project open and before every export.' + sLineBreak + sLineBreak +
        '  3. Concurrent Bidirectional theWord 7 Synchronization:' + sLineBreak +
        '     • Enable Options > "Sync theWord to iBiblia verse" and/or "Sync iBiblia to theWord verse" simultaneously without feedback loops.' + sLineBreak +
        '     • Active verse status filters in the left TreeView are also persisted across restarts.';

    7:
      Result :=
        'FEATURE OVERVIEW (v1.0.0.70 – v1.0.0.76)' + sLineBreak +
        'Filter the left Bible TreeView by association status with live per-status verse counts, step directly through matching verses across chapters and books (F8 / Shift+F8), and search across the entire project using the modeless Find in Project dialog (Ctrl+F).' + sLineBreak + sLineBreak +
        'HOW TO USE:' + sLineBreak +
        '  1. Verse Status Filtering & Filtered Stepping (Above Left Bible TreeView):' + sLineBreak +
        '     • Open the Status Filter dropdown at the top of the left navigation panel to filter the Bible tree by status with live verse counts:' + sLineBreak +
        '       - All verses (Total count)' + sLineBreak +
        '       - Not associated (Count)' + sLineBreak +
        '       - Associating (Count)' + sLineBreak +
        '       - Needs review (Count)' + sLineBreak +
        '       - Associated! (Count)' + sLineBreak +
        '     • Click "<" (or press SHIFT + F8) to jump to the previous verse matching the selected status.' + sLineBreak +
        '     • Click ">" (or press F8) to jump to the next verse matching the selected status.' + sLineBreak +
        '     • The live counter beside the buttons displays "N found", and your active filter is persisted both per-project (.ibb) and in iBiblia.ini.' + sLineBreak + sLineBreak +
        '  2. Project-Wide Search — Find in Project (Edit > Find in Project... / Ctrl+F):' + sLineBreak +
        '     • Press CTRL + F at any time to open the floating Find in Project window.' + sLineBreak +
        '     • Type a word, phrase, or Strong''s code and choose one of the 7 "Look in:" scopes:' + sLineBreak +
        '       1) Destination translation (Target text)' + sLineBreak +
        '       2) Original source (Hebrew / Greek)' + sLineBreak +
        '       3) Reference text 1' + sLineBreak +
        '       4) Reference text 2' + sLineBreak +
        '       5) Strong tags / Pairs (e.g. G3056, H7225, or morphology tags)' + sLineBreak +
        '       6) Translator notes / Comments' + sLineBreak +
        '       7) All text fields' + sLineBreak +
        '     • Click or double-click any matching verse in the results list (or click "< Previous Match" / "Next Match >") to jump directly to that verse in the main workspace.';

    8:
      begin
        Result := '';
        for i := 0 to 7 do
        begin
          if Result <> '' then
            Result := Result + sLineBreak + sLineBreak +
              '========================================================================' + sLineBreak + sLineBreak;
          Result := Result + GetTopicHeaderTitle(i) + sLineBreak +
            '------------------------------------------------------------------------' + sLineBreak +
            GetTopicGuideText(i);
        end;
      end;
  else
    Result := '';
  end;
end;

procedure TFormUserGuide.SelectTopic(Idx: Integer);
begin
  FSelectedTopic := EnsureRange(Idx, 0, GUIDE_TOPIC_COUNT - 1);
  if Assigned(memoGuide) then
  begin
    memoGuide.Lines.BeginUpdate;
    try
      memoGuide.Text := GetTopicGuideText(FSelectedTopic);
      memoGuide.SelStart := 0;
      memoGuide.SelLength := 0;
    finally
      memoGuide.Lines.EndUpdate;
    end;
  end;
  if Assigned(btnPrev) then
    btnPrev.Enabled := (FSelectedTopic > 0);
  if Assigned(btnNext) then
    btnNext.Enabled := (FSelectedTopic < GUIDE_TOPIC_COUNT - 1);
  if Assigned(pbSidebar) then
    pbSidebar.Invalidate;
  if Assigned(pbIllust) then
    pbIllust.Invalidate;
end;

procedure TFormUserGuide.PbSidebarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  itemH, idx: Integer;
begin
  if Sender = nil then ;
  if Shift = [] then ;
  if X = 0 then ;
  itemH := Max(36, (pbSidebar.Height - 12) div GUIDE_TOPIC_COUNT);
  idx := (Y - 6) div itemH;
  if (Y < 6) or (idx < 0) or (idx >= GUIDE_TOPIC_COUNT) then
    idx := -1;
  if idx <> FHotTopic then
  begin
    FHotTopic := idx;
    pbSidebar.Invalidate;
  end;
end;

procedure TFormUserGuide.PbSidebarMouseLeave(Sender: TObject);
begin
  if Sender = nil then ;
  if FHotTopic <> -1 then
  begin
    FHotTopic := -1;
    pbSidebar.Invalidate;
  end;
end;

procedure TFormUserGuide.PbSidebarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  itemH, idx: Integer;
begin
  if Sender = nil then ;
  if Shift = [] then ;
  if X = 0 then ;
  if Button <> mbLeft then Exit;
  itemH := Max(36, (pbSidebar.Height - 12) div GUIDE_TOPIC_COUNT);
  idx := (Y - 6) div itemH;
  if (Y >= 6) and (idx >= 0) and (idx < GUIDE_TOPIC_COUNT) then
    SelectTopic(idx);
end;

procedure TFormUserGuide.PbSidebarPaint(Sender: TObject);
var
  bmp: TBitmap;
  i, itemH, yTop, yBot: Integer;
  rcItem, rcAccent: TRect;
  bgCol, itemBg, titleCol, subCol, borderCol: TColor;
begin
  if Sender = nil then ;
  if (pbSidebar.Width <= 0) or (pbSidebar.Height <= 0) then Exit;

  bmp := TBitmap.Create;
  try
    bmp.SetSize(pbSidebar.Width, pbSidebar.Height);
    bgCol := CurrentSkin.PanelBg;
    borderCol := CurrentSkin.PanelBorder;

    bmp.Canvas.Brush.Style := bsSolid;
    bmp.Canvas.Brush.Color := bgCol;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

    // Right border divider
    bmp.Canvas.Pen.Color := borderCol;
    bmp.Canvas.Line(bmp.Width - 1, 0, bmp.Width - 1, bmp.Height);

    itemH := Max(36, (bmp.Height - 12) div GUIDE_TOPIC_COUNT);
    bmp.Canvas.Font.Name := 'Segoe UI';

    for i := 0 to GUIDE_TOPIC_COUNT - 1 do
    begin
      yTop := 6 + i * itemH;
      yBot := yTop + itemH - 4;
      rcItem := Rect(8, yTop, bmp.Width - 8, yBot);

      if i = FSelectedTopic then
      begin
        if CurrentSkin.IsDark then
        begin
          itemBg := CurrentSkin.PointedBg;
          titleCol := CurrentSkin.PointedText;
          subCol := MakeRGB(220, 235, 250);
        end
        else if CurrentSkin.Id = skSepia then
        begin
          itemBg := CurrentSkin.HighlightBg;
          titleCol := CurrentSkin.HighlightText;
          subCol := CurrentSkin.WindowTextDim;
        end
        else
        begin
          itemBg := MakeRGB(215, 234, 252);
          titleCol := clBlack;
          subCol := MakeRGB(70, 90, 110);
        end;
      end
      else if i = FHotTopic then
      begin
        itemBg := CurrentSkin.VerseHighlightBg;
        titleCol := CurrentSkin.PanelText;
        subCol := CurrentSkin.WindowTextDim;
      end
      else
      begin
        itemBg := CurrentSkin.WindowBg;
        titleCol := CurrentSkin.WindowText;
        subCol := CurrentSkin.WindowTextDim;
      end;

      bmp.Canvas.Brush.Style := bsSolid;
      bmp.Canvas.Brush.Color := itemBg;
      bmp.Canvas.Pen.Color := borderCol;
      bmp.Canvas.RoundRect(rcItem.Left, rcItem.Top, rcItem.Right, rcItem.Bottom, 6, 6);

      if i = FSelectedTopic then
      begin
        rcAccent := Rect(rcItem.Left, rcItem.Top + 3, rcItem.Left + 4, rcItem.Bottom - 3);
        bmp.Canvas.Brush.Color := IfThen(CurrentSkin.IsDark, CurrentSkin.HighlightBg, CurrentSkin.HeaderColor);
        bmp.Canvas.FillRect(rcAccent);
      end;

      bmp.Canvas.Brush.Style := bsClear;
      bmp.Canvas.Font.Size := 9;
      bmp.Canvas.Font.Style := [fsBold];
      bmp.Canvas.Font.Color := titleCol;
      bmp.Canvas.TextOut(rcItem.Left + 10, rcItem.Top + 6, GetTopicNavTitle(i));

      bmp.Canvas.Font.Size := 8;
      bmp.Canvas.Font.Style := [];
      bmp.Canvas.Font.Color := subCol;
      bmp.Canvas.TextOut(rcItem.Left + 10, rcItem.Top + 23, GetTopicVersionBadge(i));
    end;

    pbSidebar.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TFormUserGuide.PbIllustPaint(Sender: TObject);
var
  bmp: TBitmap;
  W, H: Integer;
  winBg, pnlBg, borderCol, txtCol, dimCol, hdrCol, hiBg, hiTxt, ptBg, ptTxt: TColor;

  procedure DrawBox(C: TCanvas; X1, Y1, X2, Y2: Integer; FillC, BorderC: TColor; Radius: Integer = 6);
  begin
    C.Brush.Style := bsSolid;
    C.Brush.Color := FillC;
    C.Pen.Style := psSolid;
    C.Pen.Width := 1;
    C.Pen.Color := BorderC;
    if Radius > 0 then
      C.RoundRect(X1, Y1, X2, Y2, Radius, Radius)
    else
      C.Rectangle(X1, Y1, X2, Y2);
  end;

  procedure DrawBadge(C: TCanvas; X, Y: Integer; const Txt: string; BgC, FgC, BorderC: TColor; BoldFont: Boolean = True; PadX: Integer = 8; PadY: Integer = 3);
  var
    tw, th: Integer;
  begin
    if BoldFont then
      C.Font.Style := [fsBold]
    else
      C.Font.Style := [];
    tw := C.TextWidth(Txt);
    th := C.TextHeight(Txt);
    DrawBox(C, X, Y, X + tw + PadX * 2, Y + th + PadY * 2, BgC, BorderC, 5);
    C.Brush.Style := bsClear;
    C.Font.Color := FgC;
    C.TextOut(X + PadX, Y + PadY, Txt);
  end;

  procedure DrawArrow(C: TCanvas; X1, Y1, X2, Y2: Integer; Col: TColor);
  var
    dx, dy, len, ux, uy, px, py: Double;
    p1, p2: TPoint;
  begin
    C.Pen.Color := Col;
    C.Pen.Width := 2;
    C.Line(X1, Y1, X2, Y2);
    C.Pen.Width := 1;
    dx := X2 - X1;
    dy := Y2 - Y1;
    len := Sqrt(dx * dx + dy * dy);
    if len < 1 then Exit;
    ux := dx / len;
    uy := dy / len;
    px := -uy;
    py := ux;
    p1 := Point(Round(X2 - ux * 8 + px * 4), Round(Y2 - uy * 8 + py * 4));
    p2 := Point(Round(X2 - ux * 8 - px * 4), Round(Y2 - uy * 8 - py * 4));
    C.Brush.Style := bsSolid;
    C.Brush.Color := Col;
    C.Polygon([Point(X2, Y2), p1, p2]);
  end;

  procedure DrawTopic0(C: TCanvas);
  begin
    // Source Box
    DrawBox(C, 16, 36, W - 210, 106, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 41, 'SOURCE TEXT PANEL (Mouse Wheel Up = Left  |  Mouse Wheel Down = Right)');

    DrawBadge(C, 26, 62, 'ἘΝ', winBg, dimCol, borderCol, False);
    DrawBadge(C, 70, 62, '◄  ἀρχῇ  ►', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, 166, 62, 'ἐποίησεν (Next Unassoc.)', winBg, txtCol, ptBg, True);
    DrawBadge(C, 342, 62, 'ὁ Θεός', winBg, dimCol, borderCol, False);

    // Mouse wheel illustration on the right
    DrawBox(C, W - 198, 36, W - 16, 106, pnlBg, hdrCol, 8);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W - 188, 42, 'MOUSE WHEEL');
    C.Font.Style := [];
    C.Font.Color := txtCol;
    C.TextOut(W - 188, 60, '▲ Scroll Up   -> Word Left');
    C.TextOut(W - 188, 76, '▼ Scroll Down -> Word Right');
    C.Font.Color := dimCol;
    C.TextOut(W - 188, 91, '(Works in Source or Dest!)');

    // Destination Box
    DrawBox(C, 16, 118, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 123, 'DESTINATION TEXT PANEL (Hold Ctrl+Shift + Click multiple words -> Release to Auto-Advance)');

    DrawBadge(C, 26, 145, 'In', winBg, txtCol, borderCol, False);
    DrawBadge(C, 62, 145, 'the', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, 106, 145, 'beginning', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, 192, 145, 'God', winBg, dimCol, borderCol, False);
    DrawBadge(C, 238, 145, 'made', winBg, dimCol, borderCol, False);

    DrawArrow(C, 112, 90, 112, 142, hdrCol);
    DrawBadge(C, 295, 144, '1. Hold Ctrl+Shift + Click "the" & "beginning"', ptBg, ptTxt, borderCol, True);
    DrawBadge(C, 295, 171, '2. Release Ctrl+Shift -> Auto-selects "ἐποίησεν"!', hiBg, hiTxt, borderCol, True);
  end;

  procedure DrawTopic1(C: TCanvas);
  begin
    // Left: Source + Destination with Southeast Popup
    DrawBox(C, 16, 36, W div 2 + 15, 92, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(24, 41, '1. Select Unassociated Source Word:');
    DrawBadge(C, 24, 59, 'ἀρχῇ <WG746><WTN-DSF>', hiBg, hiTxt, hdrCol, True);

    DrawBox(C, 16, 98, W div 2 + 15, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(24, 103, '2. Move Cursor Inside Destination Panel:');
    DrawBadge(C, 24, 122, 'In   the   beginning', winBg, txtCol, borderCol, False);

    // Cursor + Southeast Popup
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(86, 146, '↖ Cursor');
    DrawBox(C, 142, 134, W div 2 + 8, 193, CurrentSkin.NoteBg, hdrCol, 5);
    DrawBadge(C, 146, 138, 'G746', ptBg, ptTxt, borderCol, True, 5, 1);
    DrawBadge(C, 192, 138, 'N-DSF', pnlBg, txtCol, borderCol, False, 5, 1);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Color := CurrentSkin.NoteText;
    C.TextOut(148, 160, 'ἀρχή (archē) — beginning, origin');
    C.Font.Color := dimCol;
    C.TextOut(148, 175, '(Floats +18px Southeast of cursor)');

    // Right: 4-Slot Default Dictionaries
    DrawBox(C, W div 2 + 25, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 35, 42, 'DEFAULT DICTIONARIES (Options Menu)');
    DrawBadge(C, W div 2 + 35, 62,  '1. Greek Strong''s (.dct.twm)  -> NT & LXX G-codes', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 35, 91,  '2. Hebrew Strong''s (.dct.twm) -> OT H-codes (H0123)', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 35, 120, '3. Morphology (.dct.twm)      -> Grammar & Parsing', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 35, 149, '4. English Word (.dct.twm)    -> Untagged English', hiBg, hiTxt, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 35, 178, 'Supports compressed=1 zlib & auto-loads .ttf fonts');
  end;

  procedure DrawTopic2(C: TCanvas);
  var
    barY, cx, i: Integer;
    dotCols: array[0..3] of TColor;
  begin
    // Annotated Bible Panel Box
    DrawBox(C, 16, 36, W - 16, 196, winBg, borderCol, 6);

    C.Brush.Style := bsClear;
    C.Font.Size := 10;
    C.Font.Style := [];
    C.Font.Color := txtCol;
    C.TextOut(28, 52, 'In the beginning God created the heaven and the earth.');

    // Hover-Only Chevron Arrows right after verse text
    DrawBox(C, 392, 48, 434, 72, pnlBg, hdrCol, 5);
    C.Brush.Style := bsClear;
    C.Font.Size := 10;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(399, 51, '‹   ›');

    DrawArrow(C, 470, 60, 438, 60, hdrCol);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(476, 46, 'Hover-Only Verse Chevrons (‹ Prev / › Next)');
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(476, 61, 'Appears ONLY in the Bible panel under the mouse cursor!');

    // Bottom Bar inside the Bible Panel
    barY := 158;
    DrawBox(C, 20, barY, W - 20, 190, pnlBg, borderCol, 4);

    // 1. Translation Name Badge (Bottom-Left)
    DrawBadge(C, 26, barY + 4, 'KJV Strong''s (title=)', winBg, txtCol, hdrCol, True);
    DrawArrow(C, 85, 130, 85, barY + 2, hdrCol);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(26, 112, 'Bottom-Left: Translation Name');

    // 2. 4-Color Status Circles (Bottom-Center)
    dotCols[0] := MakeRGB(205, 205, 205);
    dotCols[1] := MakeRGB(255, 207, 57);
    dotCols[2] := MakeRGB(243, 64, 93);
    dotCols[3] := MakeRGB(75, 200, 66);
    cx := (W div 2) - 45;
    for i := 0 to 3 do
    begin
      C.Brush.Style := bsSolid;
      C.Brush.Color := dotCols[i];
      C.Pen.Color := borderCol;
      C.Ellipse(cx + i * 28, barY + 8, cx + i * 28 + 16, barY + 24);
    end;
    DrawArrow(C, W div 2, 130, W div 2, barY + 4, hdrCol);
    C.Brush.Style := bsClear;
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(W div 2 - 110, 100, 'Bottom-Center: 4 Verse Status Circles');
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(W div 2 - 110, 114, '1: Gray  |  2: Yellow  |  3: Red  |  4: Green');

    // 3. Edit & Properties Checkboxes (Bottom-Right)
    DrawBadge(C, W - 165, barY + 4, '[✓] Edit   [ ] Properties', winBg, txtCol, hdrCol, True);
    DrawArrow(C, W - 95, 130, W - 95, barY + 2, hdrCol);
    C.Brush.Style := bsClear;
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(W - 210, 112, 'Bottom-Right: Mode Checkboxes');
  end;

  procedure DrawTopic3(C: TCanvas);
  begin
    // Master Formatting Ribbon
    DrawBox(C, 16, 36, W - 16, 74, pnlBg, hdrCol, 5);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(24, 40, 'SHARED MASTER FORMATTING RIBBON (Active whenever [x] Edit is checked):');
    DrawBadge(C, 24,  53, 'CI (Enter)', winBg, txtCol, borderCol, False, 5, 1);
    DrawBadge(C, 90,  53, 'CM (2xEnter)', winBg, txtCol, borderCol, False, 5, 1);
    DrawBadge(C, 172, 53, 'CL (Shift+Enter)', winBg, txtCol, borderCol, False, 5, 1);
    DrawBadge(C, 272, 53, 'PI- / PI+', winBg, txtCol, borderCol, False, 5, 1);
    DrawBadge(C, 336, 53, 'TS1..3', winBg, txtCol, borderCol, False, 5, 1);
    DrawBadge(C, 386, 53, 'FI  FR  FO  RF', hiBg, hiTxt, hdrCol, True, 5, 1);
    DrawBadge(C, 482, 53, 'Clear Tags', winBg, txtCol, borderCol, False, 5, 1);

    // Left: In-place Editor + Floating Mini-Toolbar
    DrawBox(C, 16, 82, W div 2 + 20, 198, winBg, borderCol, 6);
    DrawBox(C, 68, 90, 298, 116, pnlBg, hdrCol, 5);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(76, 95, 'Floating Mini-Toolbar:  [FI] [FR] [B] [I] [RF] [X]');

    DrawBadge(C, 68, 124, 'Selected Verse Text', hiBg, hiTxt, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(28, 156, '• Visual line-wrapping in Edit mode; flattens to 1 line on save.');
    C.TextOut(28, 173, '• Select any text to pop up the Floating Mini-Toolbar above it.');

    // Right: Live Context Panel Preview
    DrawBox(C, W div 2 + 30, 82, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 40, 88, 'LIVE CONTEXT PANEL PREVIEW');
    DrawBadge(C, W div 2 + 40, 106, 'Source | Dest | Ref 1 | Ref 2 | Interlinear', ptBg, ptTxt, borderCol, True, 6, 2);
    C.Brush.Style := bsClear;
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(W div 2 + 40, 136, 'Heading <TS1> & Indented <PI1> Preview');
    C.Font.Style := [fsItalic];
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 52, 154, 'Unassociated Dest words -> Italics on Save (Ctrl+S)');
    C.Font.Style := [];
    C.TextOut(W div 2 + 40, 174, 'Updates in real time in Edit mode & upon File Save!');
  end;

  procedure DrawTopic4(C: TCanvas);
  begin
    // Left: Hovered Words in Source & Destination
    DrawBox(C, 16, 36, W div 2 - 10, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'HOVER SPEECH ROUTING (Ctrl + T Toggle)');

    DrawBadge(C, 26, 66, 'Source Word:  λόγος  [🔊]', ptBg, ptTxt, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [];
    C.Font.Color := txtCol;
    C.TextOut(26, 94, '-> Routed to Source Voice (Greek / Hebrew)');
    C.Font.Color := dimCol;
    C.TextOut(26, 110, '   e.g. Microsoft Stefanos (Greek) / Asaf (Hebrew)');

    DrawBadge(C, 26, 134, 'Dest / Ref Word:  beginning  [🔊]', hiBg, hiTxt, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Color := txtCol;
    C.TextOut(26, 162, '-> Routed to Translation Voice (English / Local)');
    C.Font.Color := dimCol;
    C.TextOut(26, 178, '   Automatic punctuation stripping for clean speech');

    // Right: Voice / Speech Settings Dialog Schematic
    DrawBox(C, W div 2 + 4, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 16, 42, 'VOICE / SPEECH SETTINGS (Options Menu)');

    DrawBadge(C, W div 2 + 16, 64, 'Source Voice: [Microsoft Stefanos] [Test]', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 16, 92, 'Dest Voice:   [Microsoft Zira    ] [Test]', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 16, 120, 'Speed (-10..+10): ====O====   Volume: =======O=', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 16, 148, 'Trigger: (•) Hover   ( ) Ctrl+Hover   ( ) Alt+Hover', winBg, txtCol, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 16, 178, 'Auto-discovers Windows 10/11 OneCore & SAPI5 voices');
  end;

  procedure DrawTopic5(C: TCanvas);
  var
    sId: TSkinId;
    sk: TSkinColors;
    idx, cardW, xLeft: Integer;
  begin
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(16, 36, '7 BUILT-IN THEWORD SKINS (View > Skins (theWord Themes)  |  Shortcut: Ctrl + Shift + D)');

    cardW := (W - 32 - 6 * 6) div 7;
    idx := 0;
    for sId := Low(TSkinId) to High(TSkinId) do
    begin
      sk := GetSkin(sId);
      xLeft := 16 + idx * (cardW + 6);
      DrawBox(C, xLeft, 56, xLeft + cardW, 162, sk.WindowBg, sk.PanelBorder, 6);
      DrawBox(C, xLeft + 2, 58, xLeft + cardW - 2, 80, sk.PanelBg, sk.PanelBorder, 4);

      C.Brush.Style := bsClear;
      C.Font.Size := 7;
      C.Font.Style := [fsBold];
      C.Font.Color := sk.HeaderColor;
      C.TextOut(xLeft + 5, 63, sk.Name);

      C.Font.Style := [];
      C.Font.Color := sk.WindowText;
      C.TextOut(xLeft + 6, 88, 'Ἐν ἀρχῇ');
      C.Font.Color := sk.WindowTextDim;
      C.TextOut(xLeft + 6, 105, 'Unassoc.');

      DrawBadge(C, xLeft + 5, 126, 'Selected', sk.HighlightBg, sk.HighlightText, sk.PanelBorder, True, 4, 2);
      Inc(idx);
    end;

    DrawBadge(C, 16, 170, 'Ctrl + Shift + D : Instant Dark / Light Toggle', ptBg, ptTxt, borderCol, True);
    DrawBadge(C, 310, 170, 'Themes all Menus, Panels, Borders, Popups & Dialogs', hiBg, hiTxt, borderCol, True);
  end;

  procedure DrawTopic6(C: TCanvas);
  begin
    // Left: Complete Bible Module Export
    DrawBox(C, 16, 36, W div 2 - 6, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'RIGHT-CLICK -> EXPORT AS BIBLE MODULE');

    DrawBadge(C, 26, 64, 'Right-Click Any Bible Panel (Source/Dest/Ref)', winBg, txtCol, borderCol, False);
    DrawArrow(C, 130, 88, 130, 104, hdrCol);
    DrawBadge(C, 26, 106, '"Export as Bible module..." (.ont / .ot / .nt)', hiBg, hiTxt, hdrCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [];
    C.Font.Color := txtCol;
    C.TextOut(26, 136, '• Exact line counts: 31,102 (.ont) / 23,145 (.ot) / 7,957 (.nt)');
    C.TextOut(26, 153, '• Preserves 100% of module properties & UTF-8 BOM');
    C.TextOut(26, 170, '• Embeds <WG...>/<WH...> & <WT...> tags on Destination');

    // Right: Smart Reload + Bidirectional theWord 7 Sync
    DrawBox(C, W div 2 + 6, 36, W - 16, 114, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 16, 42, 'SMART TEXT RELOAD DIFF');
    DrawBadge(C, W div 2 + 16, 61, 'Words Changed -> Red "Needs Review"', MakeRGB(243, 64, 93), clWhite, borderCol, True);
    DrawBadge(C, W div 2 + 16, 86, 'Tags Only Changed -> Keeps Green "Associated!"', MakeRGB(45, 150, 50), clWhite, borderCol, True);

    DrawBox(C, W div 2 + 6, 120, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 16, 126, 'CONCURRENT BIDIRECTIONAL THEWORD 7 SYNC');
    DrawBadge(C, W div 2 + 16, 146, 'iBiblia (64-bit)   <=== WM_COPYDATA ===>   theWord 7', ptBg, ptTxt, borderCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [];
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 16, 175, 'Both sync directions work simultaneously + Self-Healing DB');
  end;

  procedure DrawTopic7(C: TCanvas);
  begin
    // Left: Status Filter Dropdown & Prev/Next Filtered Buttons
    DrawBox(C, 16, 36, W div 2 - 8, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'VERSE STATUS FILTER (Top-Left Above TreeView)');

    DrawBadge(C, 26, 62, '[ Needs review (14)          ▼ ]', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, 26, 92, '[ < Prev (Shift+F8) ]', winBg, txtCol, borderCol, False);
    DrawBadge(C, 152, 92, '[ Next (F8) > ]', ptBg, ptTxt, borderCol, True);
    C.Brush.Style := bsClear;
    C.Font.Style := [fsBold];
    C.Font.Color := txtCol;
    C.TextOut(252, 96, '14 found');

    C.Font.Style := [];
    C.Font.Color := txtCol;
    C.TextOut(26, 124, '• Filters TreeView by: All / Not assoc. / Associating /');
    C.TextOut(26, 140, '  Needs review / Associated! with live counts.');
    C.Font.Color := dimCol;
    C.TextOut(26, 160, '• Automatically saved per-project (.ibb) & in iBiblia.ini.');

    // Right: Find in Project (Ctrl+F)
    DrawBox(C, W div 2 + 4, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(W div 2 + 14, 42, 'FIND IN PROJECT (Ctrl + F  |  Edit > Find in Project...)');

    DrawBadge(C, W div 2 + 14, 62, 'Find what: [ G3056 / beginning      ]  [ Search ]', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 14, 90, 'Look in:   [ All text fields / Strong tags ▼ ]', ptBg, ptTxt, borderCol, True);
    DrawBadge(C, W div 2 + 14, 120, 'Gen 1:1   | In the beginning God created...', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, W div 2 + 14, 148, '[ Go to Verse ]  [ < Prev Match ]  [ Next Match > ]', winBg, txtCol, borderCol, False);
    C.Brush.Style := bsClear;
    C.Font.Color := dimCol;
    C.TextOut(W div 2 + 14, 176, 'Searches Dest, Source, Ref 1/2, Strong''s & Notes');
  end;

  procedure DrawTopic8(C: TCanvas);
  begin
    DrawBox(C, 16, 36, W - 16, 198, pnlBg, borderCol, 6);
    C.Brush.Style := bsClear;
    C.Font.Size := 8;
    C.Font.Style := [fsBold];
    C.Font.Color := hdrCol;
    C.TextOut(26, 42, 'COMPLETE NEW FEATURES MAP (v1.0.0.70 – v1.0.1.035) — QUICK SHORTCUT REFERENCE');

    DrawBadge(C, 26,  64, 'Mouse Wheel Up / Down : Select Source Word Left / Right', hiBg, hiTxt, hdrCol, True);
    DrawBadge(C, 26,  94, 'Hold Ctrl+Shift + Click : Associate Multiple Words Together', ptBg, ptTxt, borderCol, True);
    DrawBadge(C, 26, 124, 'Release Ctrl+Shift : Auto-Select Next Unassociated Word', winBg, txtCol, hdrCol, True);
    DrawBadge(C, 26, 154, 'Hover in Panel : Show < > Verse Navigation Chevrons', winBg, txtCol, borderCol, False);

    DrawBadge(C, W div 2 + 10,  64, 'Ctrl + Shift + D : Toggle Dark Mode / 7 theWord Skins', ptBg, ptTxt, borderCol, True);
    DrawBadge(C, W div 2 + 10,  94, 'Ctrl + T : Toggle Real-Time Hover Text-to-Speech (TTS)', winBg, txtCol, hdrCol, True);
    DrawBadge(C, W div 2 + 10, 124, 'Keys 1, 2, 3, 4 : Set Verse Association Status Directly', winBg, txtCol, borderCol, False);
    DrawBadge(C, W div 2 + 10, 154, 'Ctrl + F : Find in Project   |   F1 : Open This User Guide', winBg, txtCol, borderCol, False);
  end;

begin
  if Sender = nil then ;
  W := pbIllust.Width;
  H := pbIllust.Height;
  if (W <= 0) or (H <= 0) then Exit;

  bmp := TBitmap.Create;
  try
    bmp.SetSize(W, H);
    winBg := CurrentSkin.WindowBg;
    pnlBg := CurrentSkin.PanelBg;
    borderCol := CurrentSkin.PanelBorder;
    txtCol := CurrentSkin.WindowText;
    dimCol := CurrentSkin.WindowTextDim;
    hdrCol := IfThen(CurrentSkin.Id = skClassic, MakeRGB(20, 95, 160), CurrentSkin.HeaderColor);
    hiBg := CurrentSkin.HighlightBg;
    hiTxt := CurrentSkin.HighlightText;
    ptBg := CurrentSkin.PointedBg;
    ptTxt := CurrentSkin.PointedText;

    bmp.Canvas.Font.Name := 'Segoe UI';

    // Outer frame
    DrawBox(bmp.Canvas, 0, 0, W, H, winBg, borderCol, 8);

    // Top banner inside illustration canvas
    DrawBox(bmp.Canvas, 1, 1, W - 1, 28, pnlBg, borderCol, 0);
    bmp.Canvas.Brush.Style := bsClear;
    bmp.Canvas.Font.Size := 9;
    bmp.Canvas.Font.Style := [fsBold];
    bmp.Canvas.Font.Color := hdrCol;
    bmp.Canvas.TextOut(14, 6, 'VISUAL ILLUSTRATION — ' + GetTopicHeaderTitle(FSelectedTopic));

    case FSelectedTopic of
      0: DrawTopic0(bmp.Canvas);
      1: DrawTopic1(bmp.Canvas);
      2: DrawTopic2(bmp.Canvas);
      3: DrawTopic3(bmp.Canvas);
      4: DrawTopic4(bmp.Canvas);
      5: DrawTopic5(bmp.Canvas);
      6: DrawTopic6(bmp.Canvas);
      7: DrawTopic7(bmp.Canvas);
    else
      DrawTopic8(bmp.Canvas);
    end;

    pbIllust.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TFormUserGuide.BtnPrevClick(Sender: TObject);
begin
  if Sender = nil then ;
  if FSelectedTopic > 0 then
    SelectTopic(FSelectedTopic - 1);
end;

procedure TFormUserGuide.BtnNextClick(Sender: TObject);
begin
  if Sender = nil then ;
  if FSelectedTopic < GUIDE_TOPIC_COUNT - 1 then
    SelectTopic(FSelectedTopic + 1);
end;

procedure TFormUserGuide.BtnCopyClick(Sender: TObject);
begin
  if Sender = nil then ;
  if Assigned(memoGuide) then
    Clipboard.AsText := GetTopicHeaderTitle(FSelectedTopic) + sLineBreak +
                        '========================================================================' + sLineBreak +
                        memoGuide.Text;
end;

procedure TFormUserGuide.BtnCloseClick(Sender: TObject);
begin
  if Sender = nil then ;
  Close;
end;

procedure TFormUserGuide.UpdateLanguage(const ALang: string);
begin
  FLang := ALang;
  if FLang = 'pt' then
  begin
    Caption := 'iBiblia — Guia de Novos Recursos (Desde v1.0.0.71)';
    if Assigned(lblHeaderTitle) then
      lblHeaderTitle.Caption := 'Guia de Novos Recursos do iBiblia (v1.0.0.71 – v1.0.1.xxx)';
    if Assigned(lblHeaderSub) then
      lblHeaderSub.Caption := 'Janela Flutuante Interativa — Mantenha aberta enquanto pratica os recursos no iBiblia.';
    if Assigned(btnPrev) then btnPrev.Caption := '< Anterior';
    if Assigned(btnNext) then btnNext.Caption := 'Próximo >';
    if Assigned(btnCopy) then btnCopy.Caption := 'Copiar Guia';
    if Assigned(btnClose) then btnClose.Caption := 'Fechar';
  end
  else
  begin
    Caption := 'iBiblia — New Features User Guide (Since v1.0.0.71)';
    if Assigned(lblHeaderTitle) then
      lblHeaderTitle.Caption := 'iBiblia New Features User Guide (v1.0.0.71 – v1.0.1.xxx)';
    if Assigned(lblHeaderSub) then
      lblHeaderSub.Caption := 'Floating Interactive Guide — Keep this window open while practicing features directly in iBiblia.';
    if Assigned(btnPrev) then btnPrev.Caption := '< Previous Topic';
    if Assigned(btnNext) then btnNext.Caption := 'Next Topic >';
    if Assigned(btnCopy) then btnCopy.Caption := 'Copy Guide Text';
    if Assigned(btnClose) then btnClose.Caption := 'Close';
  end;
end;

procedure TFormUserGuide.ApplySkin(const ASkin: TSkinColors);
begin
  Color := ASkin.PanelBg;
  ApplySkinToForm(Self, ASkin);

  if Assigned(pnlHeader) then pnlHeader.Color := ASkin.PanelBg;
  if Assigned(pnlBody) then pnlBody.Color := ASkin.PanelBg;
  if Assigned(pnlSidebar) then pnlSidebar.Color := ASkin.PanelBg;
  if Assigned(pnlContent) then pnlContent.Color := ASkin.PanelBg;
  if Assigned(pnlIllust) then pnlIllust.Color := ASkin.PanelBg;
  if Assigned(pnlFooter) then pnlFooter.Color := ASkin.PanelBg;

  if Assigned(lblHeaderTitle) then
    lblHeaderTitle.Font.Color := IfThen(ASkin.Id = skClassic, MakeRGB(20, 95, 160), ASkin.HeaderColor);
  if Assigned(lblHeaderSub) then
    lblHeaderSub.Font.Color := ASkin.WindowTextDim;

  if Assigned(memoGuide) then
  begin
    memoGuide.Color := ASkin.WindowBg;
    memoGuide.Font.Color := ASkin.WindowText;
    ApplyDarkModeToControl(memoGuide, ASkin.IsDark);
    {$IFDEF WINDOWS}
    HookEditControl(memoGuide);
    {$ENDIF}
  end;

  {$IFDEF WINDOWS}
  if HandleAllocated then
    ApplyDarkModeToWindow(Self, ASkin.IsDark);
  if Assigned(btnPrev) and btnPrev.HandleAllocated then HookButtonControl(btnPrev);
  if Assigned(btnNext) and btnNext.HandleAllocated then HookButtonControl(btnNext);
  if Assigned(btnCopy) and btnCopy.HandleAllocated then HookButtonControl(btnCopy);
  if Assigned(btnClose) and btnClose.HandleAllocated then HookButtonControl(btnClose);
  {$ENDIF}

  if Assigned(btnPrev) then begin btnPrev.Font.Color := ASkin.PanelText; btnPrev.Invalidate; end;
  if Assigned(btnNext) then begin btnNext.Font.Color := ASkin.PanelText; btnNext.Invalidate; end;
  if Assigned(btnCopy) then begin btnCopy.Font.Color := ASkin.PanelText; btnCopy.Invalidate; end;
  if Assigned(btnClose) then begin btnClose.Font.Color := ASkin.PanelText; btnClose.Invalidate; end;
  if Assigned(pbSidebar) then pbSidebar.Invalidate;
  if Assigned(pbIllust) then pbIllust.Invalidate;
end;

procedure TFormUserGuide.FormShow(Sender: TObject);
begin
  if Sender = nil then ;
  LayoutFooterButtons;
  ApplySkin(CurrentSkin);
  {$IFDEF WINDOWS}
  ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  if Assigned(btnPrev) then HookButtonControl(btnPrev);
  if Assigned(btnNext) then HookButtonControl(btnNext);
  if Assigned(btnCopy) then HookButtonControl(btnCopy);
  if Assigned(btnClose) then HookButtonControl(btnClose);
  if Assigned(memoGuide) then HookEditControl(memoGuide);
  {$ENDIF}
end;

procedure ShowUserGuide(AOwner: TComponent);
begin
  if not Assigned(FormUserGuide) then
    FormUserGuide := TFormUserGuide.Create(AOwner);
  FormUserGuide.HandleNeeded;
  FormUserGuide.ApplySkin(CurrentSkin);
  FormUserGuide.Show;
  FormUserGuide.BringToFront;
end;

end.
