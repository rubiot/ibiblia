unit unitupdate;

{ User-facing side of the self-updater: startup check, the "Update available"
  dialog, the Help menu actions and the apply-on-exit path. The download and
  install work itself lives in app_updater.pas. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Dialogs,
  Graphics, IniFiles, LCLType, app_updater;

type
  TUpdateCoordinator = class
  private
    FIni: TIniFile;
    FStartTimer: TTimer;
    FBusy: Boolean;
    FNoticeShown: Boolean;
    FPendingVersion: string;
    FStageDir: string;       // verified, unpacked update waiting for the next exit (auto mode)
    FRestartAfterClose: Boolean;
    function GetCheckAtStartup: Boolean;
    procedure SetCheckAtStartup(AValue: Boolean);
    function GetAutoInstall: Boolean;
    procedure SetAutoInstall(AValue: Boolean);
    procedure StartTimerFired(Sender: TObject);
    procedure ShowRestartNotice;
    procedure RunCheck(AManual: Boolean);
  public
    constructor Create(AIni: TIniFile);
    destructor Destroy; override;
    { Called once the main window is up; checks after a short delay. }
    procedure ScheduleStartupCheck;
    { Help > Check for updates. }
    procedure CheckNow;
    { Called when the main window is really closing (not when the user cancelled). }
    procedure MainFormClosing;
    procedure HandleResult(AManual: Boolean; const AInfo: TUpdateInfo;
      AOk: Boolean; const AErr: string);
    procedure BackgroundStageDone(AOk: Boolean; const AStageDir, AErr: string);
    { Installs the downloaded update and restarts iBiblia (asks to save first). }
    procedure RestartNow;
    property CheckAtStartup: Boolean read GetCheckAtStartup write SetCheckAtStartup;
    property AutoInstall: Boolean read GetAutoInstall write SetAutoInstall;
  end;

var
  UpdateCoordinator: TUpdateCoordinator = nil;

implementation

const
  IniSection = 'opcoes';

type
  { ----------------------------------------------------- worker threads -- }
  TCheckThread = class(TThread)
  private
    FManual: Boolean;
    FInfo: TUpdateInfo;
    FOk: Boolean;
    FErr: string;
    procedure Deliver;
  protected
    procedure Execute; override;
  public
    constructor Create(AManual: Boolean);
  end;

  TStageThread = class(TThread)
  private
    FInfo: TUpdateInfo;
    FOk: Boolean;
    FStageDir, FErr: string;
    FDone, FTotal: Int64;
    FFinished: Boolean;
    FNotifyCoordinator: Boolean;
    procedure Progress(ADone, ATotal: Int64);
    procedure Deliver;
  protected
    procedure Execute; override;
  public
    constructor Create(const AInfo: TUpdateInfo; ANotifyCoordinator: Boolean);
    property Done: Int64 read FDone;
    property Total: Int64 read FTotal;
    property Finished: Boolean read FFinished;
    property Ok: Boolean read FOk;
    property StageDir: string read FStageDir;
    property Err: string read FErr;
  end;

  { Small non-modal box that offers to restart once an update has been
    downloaded in the background. It never takes over the keyboard. }
  TRestartNotice = class(TForm)
  private
    procedure RestartClick(Sender: TObject);
    procedure LaterClick(Sender: TObject);
    procedure NoticeClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    constructor CreateNotice(AOwner: TComponent; const AVersion: string);
  end;

  { ---------------------------------------------------------- dialog -- }
  TUpdateForm = class(TForm)
  private
    FInfo: TUpdateInfo;
    FHead, FStatus: TLabel;
    FNotes: TMemo;
    FProgress: TProgressBar;
    FAutoCheck: TCheckBox;
    FBtnInstall, FBtnLater, FBtnSkip: TButton;
    FTimer: TTimer;
    FThread: TStageThread;
    FSkipRequested: Boolean;
    procedure InstallClick(Sender: TObject);
    procedure SkipClick(Sender: TObject);
    procedure PollTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    constructor CreateFor(AOwner: TComponent; const AInfo: TUpdateInfo);
    function AutoInstallChecked: Boolean;
    property SkipRequested: Boolean read FSkipRequested;
  end;

function FormatBytes(N: Int64): string;
begin
  if N <= 0 then
    Result := '?'
  else if N >= 1024 * 1024 then
    Result := Format('%.1f MB', [N / (1024 * 1024)])
  else
    Result := Format('%d KB', [N div 1024]);
end;

{ ------------------------------------------------------------ TCheckThread -- }

constructor TCheckThread.Create(AManual: Boolean);
begin
  inherited Create(False);
  // not FreeOnTerminate: a thread that frees itself also discards its queued callback
  FreeOnTerminate := False;
  FManual := AManual;
end;

procedure TCheckThread.Execute;
begin
  FOk := CheckForUpdate(FInfo, FErr);
  Queue(@Deliver);
end;

procedure TCheckThread.Deliver;
var
  Manual, Ok: Boolean;
  Info: TUpdateInfo;
  Err: string;
begin
  // runs in the main thread: take the result, release the thread, then act on it
  Manual := FManual;
  Ok := FOk;
  Info := FInfo;
  Err := FErr;
  WaitFor;
  Free;
  if Assigned(UpdateCoordinator) then
    UpdateCoordinator.HandleResult(Manual, Info, Ok, Err);
end;

{ ------------------------------------------------------------ TStageThread -- }

constructor TStageThread.Create(const AInfo: TUpdateInfo; ANotifyCoordinator: Boolean);
begin
  FInfo := AInfo;
  FNotifyCoordinator := ANotifyCoordinator;
  inherited Create(False);
  FreeOnTerminate := False;
end;

procedure TStageThread.Progress(ADone, ATotal: Int64);
begin
  FDone := ADone;
  FTotal := ATotal;
end;

procedure TStageThread.Execute;
begin
  FOk := DownloadAndStage(FInfo, @Progress, FStageDir, FErr);
  FFinished := True;
  if FNotifyCoordinator then
    Queue(@Deliver);
end;

procedure TStageThread.Deliver;
var
  GotOk: Boolean;
  GotDir, GotErr: string;
begin
  GotOk := FOk;
  GotDir := FStageDir;
  GotErr := FErr;
  WaitFor;
  Free;
  if Assigned(UpdateCoordinator) then
    UpdateCoordinator.BackgroundStageDone(GotOk, GotDir, GotErr);
end;

{ --------------------------------------------------------- TRestartNotice -- }

constructor TRestartNotice.CreateNotice(AOwner: TComponent; const AVersion: string);
var
  L: TLabel;
  B1, B2: TButton;
begin
  inherited CreateNew(AOwner);
  BorderStyle := bsDialog;
  FormStyle := fsStayOnTop;
  Caption := 'iBiblia update';
  ClientWidth := 400;
  ClientHeight := 110;
  Position := poDefault;
  OnClose := @NoticeClose;
  L := TLabel.Create(Self);
  L.Parent := Self;
  L.SetBounds(16, 14, 368, 50);
  L.AutoSize := False;
  L.WordWrap := True;
  L.Caption := 'Version ' + AVersion + ' has been downloaded. Restart iBiblia now ' +
    'to use it? (It will ask to save any unsaved changes first.)';
  B1 := TButton.Create(Self);
  B1.Parent := Self;
  B1.SetBounds(160, 72, 110, 28);
  B1.Caption := 'Restart now';
  B1.Default := False;
  B1.OnClick := @RestartClick;
  B2 := TButton.Create(Self);
  B2.Parent := Self;
  B2.SetBounds(280, 72, 104, 28);
  B2.Caption := 'Later';
  B2.OnClick := @LaterClick;
end;

procedure TRestartNotice.RestartClick(Sender: TObject);
begin
  Close;
  if Assigned(UpdateCoordinator) then
    UpdateCoordinator.RestartNow;
end;

procedure TRestartNotice.LaterClick(Sender: TObject);
begin
  Close;
end;

procedure TRestartNotice.NoticeClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

{ ------------------------------------------------------------ TUpdateForm -- }

constructor TUpdateForm.CreateFor(AOwner: TComponent; const AInfo: TUpdateInfo);
var
  Notes: string;
begin
  inherited CreateNew(AOwner);
  FInfo := AInfo;
  Caption := 'iBiblia update';
  Position := poMainFormCenter;
  BorderStyle := bsDialog;
  ClientWidth := 520;
  ClientHeight := 404;
  OnCloseQuery := @FormCloseQuery;

  FHead := TLabel.Create(Self);
  FHead.Parent := Self;
  FHead.SetBounds(16, 14, 488, 40);
  FHead.AutoSize := False;
  FHead.WordWrap := True;
  FHead.Font.Style := [fsBold];
  FHead.Caption := Format('iBiblia %s is available (you have %s).',
    [AInfo.Version, NormalizeVersion(CurrentAppVersion)]);

  FNotes := TMemo.Create(Self);
  FNotes.Parent := Self;
  FNotes.SetBounds(16, 58, 488, 212);
  FNotes.ReadOnly := True;
  FNotes.ScrollBars := ssAutoVertical;
  FNotes.WordWrap := True;
  Notes := Trim(AInfo.Notes);
  if Notes = '' then
    Notes := 'No release notes were published for this version.';
  // release notes are markdown; show them as plain text
  Notes := StringReplace(Notes, #13#10, #10, [rfReplaceAll]);
  Notes := StringReplace(Notes, '**', '', [rfReplaceAll]);
  Notes := StringReplace(Notes, '`', '', [rfReplaceAll]);
  while (Notes <> '') and (Notes[1] = '#') do
    Delete(Notes, 1, 1);
  FNotes.Text := Trim(Notes);

  FProgress := TProgressBar.Create(Self);
  FProgress.Parent := Self;
  FProgress.SetBounds(16, 280, 488, 14);
  FProgress.Min := 0;
  FProgress.Max := 1000;
  FProgress.Visible := False;

  FStatus := TLabel.Create(Self);
  FStatus.Parent := Self;
  FStatus.SetBounds(16, 280, 488, 40);
  FStatus.WordWrap := True;
  FStatus.AutoSize := False;
  FStatus.Caption := Format('Download size: %s. Your projects and settings are not touched.',
    [FormatBytes(AInfo.AssetSize)]);

  FAutoCheck := TCheckBox.Create(Self);
  FAutoCheck.Parent := Self;
  FAutoCheck.SetBounds(16, 330, 488, 22);
  FAutoCheck.Caption := 'Install future updates automatically in the background';
  FAutoCheck.Checked := Assigned(UpdateCoordinator) and UpdateCoordinator.AutoInstall;

  FBtnInstall := TButton.Create(Self);
  FBtnInstall.Parent := Self;
  FBtnInstall.SetBounds(16, 364, 170, 30);
  FBtnInstall.Caption := 'Update and restart';
  FBtnInstall.Default := True;
  FBtnInstall.OnClick := @InstallClick;

  FBtnSkip := TButton.Create(Self);
  FBtnSkip.Parent := Self;
  FBtnSkip.SetBounds(268, 364, 130, 30);
  FBtnSkip.Caption := 'Skip this version';
  FBtnSkip.OnClick := @SkipClick;

  FBtnLater := TButton.Create(Self);
  FBtnLater.Parent := Self;
  FBtnLater.SetBounds(404, 364, 100, 30);
  FBtnLater.Caption := 'Later';
  FBtnLater.Cancel := True;
  FBtnLater.ModalResult := mrCancel;

  {$IFNDEF WINDOWS}
  FBtnInstall.Font.Color := clDefault;
  FBtnSkip.Font.Color := clDefault;
  FBtnLater.Font.Color := clDefault;
  {$ENDIF}

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 150;
  FTimer.OnTimer := @PollTimer;
end;

function TUpdateForm.AutoInstallChecked: Boolean;
begin
  Result := FAutoCheck.Checked;
end;

procedure TUpdateForm.InstallClick(Sender: TObject);
begin
  FBtnInstall.Enabled := False;
  FBtnSkip.Enabled := False;
  FBtnLater.Enabled := False;
  FAutoCheck.Enabled := False;
  FStatus.Visible := False;
  FProgress.Position := 0;
  FProgress.Visible := True;
  FStatus.Top := FProgress.Top + FProgress.Height + 4;
  FStatus.Height := 22;
  FStatus.Caption := 'Downloading...';
  FStatus.Visible := True;
  FThread := TStageThread.Create(FInfo, False);
  FTimer.Enabled := True;
end;

procedure TUpdateForm.SkipClick(Sender: TObject);
begin
  FSkipRequested := True;
  ModalResult := mrCancel;
end;

procedure TUpdateForm.PollTimer(Sender: TObject);
var
  Err: string;
begin
  if not Assigned(FThread) then
    Exit;
  if not FThread.Finished then
  begin
    if FThread.Total > 0 then
    begin
      FProgress.Position := Integer((FThread.Done * 1000) div FThread.Total);
      FStatus.Caption := Format('Downloading... %s of %s',
        [FormatBytes(FThread.Done), FormatBytes(FThread.Total)]);
    end;
    Exit;
  end;
  FTimer.Enabled := False;
  FThread.WaitFor;
  if not FThread.Ok then
  begin
    FStatus.Caption := 'Update failed: ' + FThread.Err;
    FreeAndNil(FThread);
    FBtnInstall.Enabled := True;
    FBtnInstall.Caption := 'Try again';
    FBtnLater.Enabled := True;
    FBtnSkip.Enabled := True;
    FAutoCheck.Enabled := True;
    FProgress.Visible := False;
    FStatus.Top := 280;
    FStatus.Height := 40;
    Exit;
  end;
  FProgress.Position := 1000;
  FStatus.Caption := 'Installing...';
  Application.ProcessMessages;
  if ApplyStagedUpdate(FThread.StageDir, Err) then
  begin
    DiscardStagedUpdate(FThread.StageDir);
    FreeAndNil(FThread);
    ModalResult := mrOk;
  end
  else
  begin
    FStatus.Caption := 'Could not install the update: ' + Err;
    DiscardStagedUpdate(FThread.StageDir);
    FreeAndNil(FThread);
    FBtnInstall.Enabled := True;
    FBtnInstall.Caption := 'Try again';
    FBtnLater.Enabled := True;
    FBtnSkip.Enabled := True;
    FAutoCheck.Enabled := True;
    FProgress.Visible := False;
    FStatus.Top := 280;
    FStatus.Height := 40;
  end;
end;

procedure TUpdateForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // do not close the window while a download is running
  CanClose := not (Assigned(FThread) and not FThread.Finished);
end;

{ ------------------------------------------------------- TUpdateCoordinator -- }

constructor TUpdateCoordinator.Create(AIni: TIniFile);
begin
  inherited Create;
  FIni := AIni;
end;

destructor TUpdateCoordinator.Destroy;
begin
  FStartTimer.Free;
  inherited Destroy;
end;

function TUpdateCoordinator.GetCheckAtStartup: Boolean;
begin
  Result := FIni.ReadBool(IniSection, 'update.check', True);
end;

procedure TUpdateCoordinator.SetCheckAtStartup(AValue: Boolean);
begin
  FIni.WriteBool(IniSection, 'update.check', AValue);
  FIni.UpdateFile;
end;

function TUpdateCoordinator.GetAutoInstall: Boolean;
begin
  { Linux: download new versions in the background by default and offer to
    restart; there is no installer step, so this is what makes updating
    seamless. Windows keeps the explicit prompt. }
  Result := FIni.ReadBool(IniSection, 'update.auto', {$IFDEF WINDOWS}False{$ELSE}True{$ENDIF});
end;

procedure TUpdateCoordinator.SetAutoInstall(AValue: Boolean);
begin
  FIni.WriteBool(IniSection, 'update.auto', AValue);
  FIni.UpdateFile;
end;

procedure TUpdateCoordinator.ScheduleStartupCheck;
var
  Why: string;
begin
  CleanupOldUpdateFiles;
  EnsureLinuxMenuEntry;
  if not CheckAtStartup then
    Exit;
  if not UpdatesSupported(Why) then
    Exit;
  if not Assigned(FStartTimer) then
  begin
    FStartTimer := TTimer.Create(nil);
    FStartTimer.Interval := 4000;
    FStartTimer.OnTimer := @StartTimerFired;
  end;
  FStartTimer.Enabled := True;
end;

procedure TUpdateCoordinator.StartTimerFired(Sender: TObject);
begin
  FStartTimer.Enabled := False;
  RunCheck(False);
end;

procedure TUpdateCoordinator.CheckNow;
begin
  RunCheck(True);
end;

procedure TUpdateCoordinator.RunCheck(AManual: Boolean);
var
  Why: string;
begin
  if FBusy then
  begin
    if AManual then
      ShowMessage('An update check is already running.');
    Exit;
  end;
  if not UpdatesSupported(Why) then
  begin
    if AManual then
      ShowMessage(Why);
    Exit;
  end;
  FBusy := True;
  TCheckThread.Create(AManual);
end;

procedure TUpdateCoordinator.HandleResult(AManual: Boolean; const AInfo: TUpdateInfo;
  AOk: Boolean; const AErr: string);
var
  Dlg: TUpdateForm;
  Res: TModalResult;
begin
  FBusy := False;
  if not AOk then
  begin
    if AManual then
      MessageDlg('iBiblia update', 'Could not check for updates: ' + AErr, mtWarning, [mbOK], 0);
    Exit;
  end;
  if not AInfo.Available then
  begin
    if AManual then
      MessageDlg('iBiblia update', 'You have the latest version (' + NormalizeVersion(CurrentAppVersion) + ').',
        mtInformation, [mbOK], 0);
    Exit;
  end;
  if (not AManual) and SameText(FIni.ReadString(IniSection, 'update.skip', ''), AInfo.Version) then
    Exit;
  if FStageDir <> '' then
    Exit; // already downloaded, will be installed on exit

  if (not AManual) and AutoInstall then
  begin
    FBusy := True;
    FPendingVersion := NormalizeVersion(AInfo.Version);
    TStageThread.Create(AInfo, True);
    Exit;
  end;

  Dlg := TUpdateForm.CreateFor(Application.MainForm, AInfo);
  try
    Res := Dlg.ShowModal;
    if Dlg.AutoInstallChecked <> AutoInstall then
      AutoInstall := Dlg.AutoInstallChecked;
    if Dlg.SkipRequested then
    begin
      FIni.WriteString(IniSection, 'update.skip', AInfo.Version);
      FIni.UpdateFile;
    end;
  finally
    Dlg.Free;
  end;
  if Res = mrOk then
  begin
    FRestartAfterClose := True;
    if Assigned(Application.MainForm) then
      Application.MainForm.Close;
    // if closing was cancelled (unsaved project), do not restart on a later exit
    FRestartAfterClose := False;
  end;
end;

procedure TUpdateCoordinator.BackgroundStageDone(AOk: Boolean; const AStageDir, AErr: string);
begin
  FBusy := False;
  if AOk then
  begin
    FStageDir := AStageDir; // installed silently when iBiblia is closed
    ShowRestartNotice;
  end;
end;

procedure TUpdateCoordinator.ShowRestartNotice;
var
  N: TRestartNotice;
begin
  if FNoticeShown or (not Assigned(Application.MainForm)) then
    Exit;
  FNoticeShown := True;
  N := TRestartNotice.CreateNotice(Application, FPendingVersion);
  N.Left := Application.MainForm.Left + Application.MainForm.Width - N.Width - 24;
  N.Top := Application.MainForm.Top + 60;
  N.ShowOnTop;
end;

procedure TUpdateCoordinator.RestartNow;
begin
  if (FStageDir = '') or (not Assigned(Application.MainForm)) then
    Exit;
  FRestartAfterClose := True;
  Application.MainForm.Close;
  // if closing was cancelled (unsaved project), do not restart on a later exit
  FRestartAfterClose := False;
end;

procedure TUpdateCoordinator.MainFormClosing;
var
  Err: string;
begin
  if FStageDir <> '' then
  begin
    if ApplyStagedUpdate(FStageDir, Err) then
      DiscardStagedUpdate(FStageDir);
    FStageDir := '';
  end;
  if FRestartAfterClose then
  begin
    FRestartAfterClose := False;
    LaunchNewInstance;
  end;
end;

end.
