unit unitabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, fileinfo, LCLIntf, unitskins;

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

procedure ShowChangelog(AOwner: TComponent);

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

const
  CHANGELOG_TEXT =
    'iBiblia Version 1.0.1.026 (64-bit Edition)' + sLineBreak +
    'Release Date: 2026-10-24' + sLineBreak +
    '==========================================' + sLineBreak + sLineBreak +
    'NEW FEATURES & UPDATES (v1.0.1.026):' + sLineBreak +
    '  * Hover-Only Verse Navigation Chevron Arrows Across All 4 Bibles:' + sLineBreak +
    '    - Verse navigation chevron arrows (< >) now appear exclusively inside the Bible panel currently hovered by the mouse cursor and disappear in all other Bible panels.' + sLineBreak +
    '  * Automatic Next Unassociated Source Word Selection on Ctrl+Shift Release:' + sLineBreak +
    '    - Releasing Ctrl+Shift after associating Destination words automatically highlights the next unassociated word in the Source text.' + sLineBreak +
    '  * Voice & Text-to-Speech Dialog & Modal Control Skinning Fix:' + sLineBreak +
    '    - Fixed modal handle recreation (ShowModal -> RecreateWnd) wiping Win32 subclasses on dialogs.' + sLineBreak +
    '    - Added custom dark-mode Win32 subclassing for TComboBox (ComboBoxSubclassProc) and TTrackBar (TrackBarSubclassProc).' + sLineBreak +
    '    - Enhanced GroupBoxSubclassProc to overpaint groupbox border frames and handle child WM_CTLCOLORSTATIC/BTN/EDIT/LISTBOX.' + sLineBreak +
    '    - Added OnShow skinning hooks to Voice Settings, Find in Project, New Project, Merge Projects, and Verse Rules dialogs.' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.1.000:' + sLineBreak +
    '  * Dark Mode UI & theWord Skins Integration:' + sLineBreak +
    '    - View menu with theWord skins and Dark Mode toggle (Ctrl+Shift+D).' + sLineBreak +
    '    - Palettes: Classic, theWord Dark (Charcoal), Night, Sepia, Slate, Solarized Dark, High Contrast.' + sLineBreak +
    '    - Complete control theming (Bible boxes, words, chapter view, treeview, statusbar, dialogs).' + sLineBreak +
    '    - Persistent theme saved to iBiblia.ini.' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.0.79:' + sLineBreak +
    '  * Support for Compressed Dictionary Modules (compressed=1):' + sLineBreak +
    '    - Enables full compatibility with compressed theWord dictionaries (.dct.twm).' + sLineBreak +
    '    - Transparent streaming zlib/deflate decompression for Strong numbers and Morphology codes.' + sLineBreak +
    '    - Removed restriction in module validation.' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.0.78:' + sLineBreak +
    '  * Checkboxes for Edit & Properties (Assoc. removed):' + sLineBreak +
    '    - Removed Assoc. button; default unckecked state is normal viewing / association mode.' + sLineBreak +
    '    - Checkmark boxes for Edit and Properties at bottom-right of each box.' + sLineBreak +
    '    - Check to activate, uncheck to return to normal viewing.' + sLineBreak +
    '  * Real-Time Translation Name Reflection:' + sLineBreak +
    '    - Changes to module name in Properties tab reflect in the name tab right away as you type.' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.0.77:' + sLineBreak +
    '  * Translation Name Display per Box:' + sLineBreak +
    '    - Shows the active Bible module Translation Name at the bottom-left corner of each window (Source, Destination, Ref 1, Ref 2).' + sLineBreak +
    '    - Automatically extracts title, short title, or description from module properties.' + sLineBreak +
    '    - Responsive width sharing the bottom status area with the section tab radio buttons.' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'NEW FEATURES INTRODUCED IN v1.0.0.76:' + sLineBreak +
    '  * Section Tabs via Radio Buttons (Assoc., Edit, Properties):' + sLineBreak +
    '    - 3 radio buttons anchored at the bottom-right corner of each Bible box (Source, Destination, Ref 1, Ref 2).' + sLineBreak +
    '    - "Assoc.": Standard word-level association and alignment mode.' + sLineBreak +
    '    - "Edit": Activates the in-place verse text editor directly (replacing double left-click).' + sLineBreak +
    '    - "Properties": Displays complete module metadata, headers, verse rules, and description.' + sLineBreak +
    '  * Complete Bible Module Export via Context Menu:' + sLineBreak +
    '    - Right-clicking any Bible module window (Source, Destination, Ref 1, or Ref 2) now provides' + sLineBreak +
    '      "Export as Bible module..." preserving 100% of the module metadata and properties.' + sLineBreak +
    '    - Intelligently detects and defaults to the module extension (.ont, .ot, or .nt).' + sLineBreak +
    '    - Enforces exact canonical line counts (31,102 for .ont, 23,145 for .ot, 7,957 for .nt) with UTF-8 BOM.' + sLineBreak +
    '    - Embeds associated Strong''s and morphology tags when exporting Destination module.' + sLineBreak +
    '  * Automatic Verse Status Transition on Text Reload/Replacement:' + sLineBreak +
    '    - Status transition to "Needs review" is strictly triggered only when actual Greek/Bible words' + sLineBreak +
    '      within a verse are modified, added, or deleted.' + sLineBreak +
    '    - Tag-only updates (Strong numbers, morphology codes, lemmas, or formatting) preserve "Associated" status.' + sLineBreak +
    '    - TreeView icons and filter statistics update immediately to highlight verses needing inspection.' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.0.75:' + sLineBreak +
    '  * Streamlined Default Dictionaries configuration with per-module override toggles.' + sLineBreak +
    '  * Global Greek Strong''s, Hebrew Strong''s, and Morphology dictionary slots.' + sLineBreak +
    '  * Intelligent routing: OT verses & H-codes route to Hebrew, NT verses & G-codes route to Greek.' + sLineBreak +
    '  * Automatic key normalization for prefixed (G/H) and non-prefixed topic schemas.' + sLineBreak +
    '  * Zero-padded fallback for Hebrew Strong''s numbering variations (e.g. 0123, H0123).' + sLineBreak +
    '  * Dedicated Default Dictionaries dialog under Options (Options > Default Dictionaries...).' + sLineBreak +
    '  * Per-module Default Dictionaries toggle in Project Properties (checked overrides module slots; unchecked preserves them).' + sLineBreak +    '  * Fixed Greek & Hebrew font rendering in dictionary popup when theWord is closed.' + sLineBreak +
    '  * Added automatic loading of theWord fonts (*.ttf, *.otf) on startup.' + sLineBreak +
    '  * Added dynamic font substitution (Segoe UI) in RTF font tables for missing specialized fonts.' + sLineBreak +    '  * Suppressed startup missing-file warnings for inactive per-module dictionaries when Default Dictionaries are enabled.' + sLineBreak +
    '  * Persistent dictionary configuration saved to iBiblia.ini across restarts.' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.0.74:' + sLineBreak +
    '  * Real-time Text-to-Speech (TTS) voice reading on hover.' + sLineBreak +
    '  * Multi-language support: Greek (Stefanos), Hebrew (Asaf), English.' + sLineBreak +
    '  * Full Windows 10/11 OneCore speech pack auto-discovery.' + sLineBreak +
    '  * Dedicated Voice & Audio Settings dialog (Options > Voice Settings).' + sLineBreak +
    '  * Independent Source vs Translation voice selection & instant test.' + sLineBreak +
    '  * Punctuation filtering for accurate Greek & English pronunciation.' + sLineBreak +
    '  * Global shortcut (Ctrl + T) to quickly toggle TTS on/off.' + sLineBreak +
    '--------------------------------------------------' + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.0.72:' + sLineBreak +
    '  * Concurrent Bidirectional theWord 7 Sync: resolved concurrency conflict when both sync directions are enabled simultaneously.' + sLineBreak +
    '  * Thread-safe navigation cooldown in TTwSyncThread (suppresses stale verse echo).' + sLineBreak +
    '  * Re-entrancy guard (FSyncingFromTw) to prevent circular feedback loops.' + sLineBreak + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.0.71:' + sLineBreak +
    '  * theWord 7 Bidirectional Sync: fixed window class GUID, dynamic window enumeration fallback, and 64-bit LPARAM pointer cast.' + sLineBreak +
    '  * Real-time TreeView selection and verse re-rendering on incoming verse events from theWord.' + sLineBreak +
    '  * Resilient built-in 66-book alias mapping table for instantaneous verse reference parsing.' + sLineBreak +
    '  * Suppressed intrusive debug popup when theWord is not running.' + sLineBreak +
    '  * Persistent verse filter status: remembers and restores active filter on restart.' + sLineBreak + sLineBreak +
    'FEATURES INTRODUCED IN v1.0.0.70:' + sLineBreak +
    '  * Find in Project (Ctrl + F) with multi-field search scope and fast match navigation.' + sLineBreak +
    '  * Restored repository link (https://github.com/rubiot/ibiblia) under Rubio Terra.' + sLineBreak +
    '  * Direct link to contributor Edmund Valdez (https://github.com/SalitaNgDiyos).' + sLineBreak +
    '  * Clean regular font styling in About dialog.' + sLineBreak + sLineBreak +
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
    ver := '1.0.1.026';

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

  ApplySkin(CurrentSkin);
end;

procedure TFormAbout.ApplySkin(const ASkin: TSkinColors);
begin
  Color := ASkin.PanelBg;
  ApplySkinToForm(Self, ASkin);
  {$IFDEF WINDOWS}
  if HandleAllocated then
    ApplyDarkModeToWindow(Self, ASkin.IsDark);
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
    if Assigned(ButtonChangelog) then
      ButtonChangelog.Font.Color := clWindowText;
    if Assigned(ButtonClose) then
      ButtonClose.Font.Color := clWindowText;
  end;
  if Assigned(ButtonChangelog) then ButtonChangelog.Invalidate;
  if Assigned(ButtonClose) then ButtonClose.Invalidate;
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  ApplySkin(CurrentSkin);
  {$IFDEF WINDOWS}
  ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  if Assigned(ButtonChangelog) then
    HookButtonControl(ButtonChangelog);
  if Assigned(ButtonClose) then
    HookButtonControl(ButtonClose);
  {$ENDIF}
  if Assigned(ButtonChangelog) then ButtonChangelog.Invalidate;
  if Assigned(ButtonClose) then ButtonClose.Invalidate;
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
  lblTitle.Caption := 'iBiblia v1.0.1.026 - Changelog';
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

end.
