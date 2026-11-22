unit crash_tracker_ui;

{ Error report popup shown automatically when an unhandled crash occurs (or
  when a hard crash is recovered on next startup), plus Help > Crash & error logs
  to browse, copy, save, diagnose, or delete saved logs. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure ShowCrashReportDialog(const LogFile, LogContent: string;
  IsRecoveredHardCrash: Boolean; out ShouldTerminate: Boolean);
procedure ShowCrashLogsManagerDialog(AOwner: TComponent);

implementation

uses
  Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Dialogs, Graphics, Clipbrd,
  LCLIntf, ui_style, unitskins, crash_tracker;

resourcestring
  SCrashPopupTitle = 'iBiblia Error Report';
  SCrashPopupRecoveredTitle = 'iBiblia Recovered Error Report';
  SCrashPopupIntro = 'iBiblia ran into an unexpected error. An error log was saved automatically so you can save or share it for troubleshooting.';
  SCrashPopupRecoveredIntro = 'iBiblia closed unexpectedly during your previous session. A recovery crash log was saved automatically.';
  SCrashSavedAt = 'Saved log file: %s';
  SCrashCopyBtn = 'Copy report';
  SCrashSaveAsBtn = 'Save log as...';
  SCrashOpenFolderBtn = 'Open logs folder';
  SCrashContinueBtn = 'Continue';
  SCrashCloseAppBtn = 'Close iBiblia';
  SCrashCopiedStatus = 'Report copied to clipboard.';
  SCrashSaveFilter = 'Log files (*.log;*.txt)|*.log;*.txt|All files (*.*)|*.*';
  SCrashSaveSuccess = 'Error log saved to:'#13#10'%s';
  SCrashSaveFailed = 'Could not save the log file:'#13#10'%s';

  SLogsManagerTitle = 'Crash & error logs';
  SLogsManagerFolder = 'Logs folder: %s';
  SLogsManagerColDate = 'Date & Time';
  SLogsManagerColKind = 'Type';
  SLogsManagerColHeadline = 'Summary';
  SLogsManagerColSize = 'Size';
  SLogsManagerEmpty = 'No crash or diagnostic logs have been recorded yet.'#13#10#13#10'If iBiblia ever encounters an unexpected error or shuts down abruptly, a full error report will be saved here automatically. You can also create a manual diagnostic report using the button below.';
  SLogsManagerDiagBtn = 'Create diagnostic report';
  SLogsManagerDiagTitle = 'Create diagnostic report';
  SLogsManagerDiagPrompt = 'Optional note describing what you were doing (can be left blank):';
  SLogsManagerDiagFailed = 'Could not write diagnostic report to the logs folder.';
  SLogsManagerDeleteBtn = 'Delete selected';
  SLogsManagerDeleteConfirm = 'Delete this log file permanently?'#13#10'%s';
  SLogsManagerCloseBtn = 'Close';

procedure WriteLogCopyToFile(const FileName, Content: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Length(Content) > 0 then
      Stream.WriteBuffer(Content[1], Length(Content));
  finally
    Stream.Free;
  end;
end;

function FormatLogSize(SizeBytes: Int64): string;
begin
  if SizeBytes >= 1024 * 1024 then
    Result := FormatFloat('0.0', SizeBytes / (1024 * 1024)) + ' MB'
  else if SizeBytes >= 1024 then
    Result := FormatFloat('0.0', SizeBytes / 1024) + ' KB'
  else
    Result := IntToStr(SizeBytes) + ' B';
end;

type
  TCrashReportForm = class(TForm)
  private
    FLogFile: string;
    FLogContent: string;
    FIsRecovered: Boolean;
    FUserChoseTerminate: Boolean;
    FTopPanel: TPanel;
    FIntroLabel: TLabel;
    FPathLabel: TLabel;
    FStatusLabel: TLabel;
    FMemo: TMemo;
    FFooterPanel: TPanel;
    FCopyBtn: TButton;
    FSaveAsBtn: TButton;
    FOpenFolderBtn: TButton;
    FContinueBtn: TButton;
    FCloseAppBtn: TButton;
    procedure BuildUI;
    procedure ApplyTheme;
    procedure FormShowHandler(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure SaveAsBtnClick(Sender: TObject);
    procedure OpenFolderBtnClick(Sender: TObject);
    procedure CloseAppBtnClick(Sender: TObject);
  public
    constructor CreateReport(AOwner: TComponent; const ALogFile, ALogContent: string;
      AIsRecovered: Boolean);
    property UserChoseTerminate: Boolean read FUserChoseTerminate;
  end;

  TCrashLogsManagerForm = class(TForm)
  private
    FLogs: TCrashLogSummaryArray;
    FTopPanel: TPanel;
    FFolderLabel: TLabel;
    FStatusLabel: TLabel;
    FList: TListView;
    FBodyPanel: TPanel;
    FSplitter: TSplitter;
    FPreviewPanel: TPanel;
    FMemo: TMemo;
    FFooterPanel: TPanel;
    FCopyBtn: TButton;
    FSaveAsBtn: TButton;
    FOpenFolderBtn: TButton;
    FDiagBtn: TButton;
    FDeleteBtn: TButton;
    FCloseBtn: TButton;
    procedure BuildUI;
    procedure ApplyTheme;
    procedure FormShowHandler(Sender: TObject);
    procedure ReloadLogs(const SelectPath: string = '');
    procedure UpdateSelectionUI;
    function SelectedLogIndex: Integer;
    procedure ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure CopyBtnClick(Sender: TObject);
    procedure SaveAsBtnClick(Sender: TObject);
    procedure OpenFolderBtnClick(Sender: TObject);
    procedure DiagBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
  public
    constructor CreateManager(AOwner: TComponent);
  end;

constructor TCrashReportForm.CreateReport(AOwner: TComponent;
  const ALogFile, ALogContent: string; AIsRecovered: Boolean);
begin
  inherited CreateNew(AOwner);
  FLogFile := ALogFile;
  FLogContent := ALogContent;
  FIsRecovered := AIsRecovered;
  FUserChoseTerminate := False;

  if FIsRecovered then
    Caption := SCrashPopupRecoveredTitle
  else
    Caption := SCrashPopupTitle;
  Position := poScreenCenter;
  BorderStyle := bsSizeable;
  Width := 760;
  Height := 540;
  Constraints.MinWidth := 560;
  Constraints.MinHeight := 380;
  OnShow := @FormShowHandler;

  BuildUI;
  ApplyTheme;
end;

procedure TCrashReportForm.BuildUI;
var
  RightX: Integer;
begin
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Align := alTop;
  FTopPanel.Top := 0;
  FTopPanel.Width := ClientWidth - 24;
  FTopPanel.Height := 84;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.BorderSpacing.Left := 12;
  FTopPanel.BorderSpacing.Right := 12;
  FTopPanel.BorderSpacing.Top := 10;

  FIntroLabel := TLabel.Create(Self);
  FIntroLabel.Parent := FTopPanel;
  FIntroLabel.Left := 0;
  FIntroLabel.Top := 2;
  FIntroLabel.Width := FTopPanel.Width - 8;
  FIntroLabel.Anchors := [akTop, akLeft, akRight];
  FIntroLabel.WordWrap := True;
  FIntroLabel.Font.Style := [fsBold];
  if FIsRecovered then
    FIntroLabel.Caption := SCrashPopupRecoveredIntro
  else
    FIntroLabel.Caption := SCrashPopupIntro;

  FPathLabel := TLabel.Create(Self);
  FPathLabel.Parent := FTopPanel;
  FPathLabel.Left := 0;
  FPathLabel.Top := 46;
  FPathLabel.Width := FTopPanel.Width - 8;
  FPathLabel.Anchors := [akTop, akLeft, akRight];
  FPathLabel.WordWrap := False;
  FPathLabel.ShowAccelChar := False;
  if FLogFile <> '' then
    FPathLabel.Caption := Format(SCrashSavedAt, [FLogFile])
  else
    FPathLabel.Caption := Format(SCrashSavedAt, ['(could not write to disk)']);

  FFooterPanel := TPanel.Create(Self);
  FFooterPanel.Parent := Self;
  FFooterPanel.Width := ClientWidth - 24;
  FFooterPanel.Height := 54;
  FFooterPanel.BevelOuter := bvNone;
  FFooterPanel.BorderSpacing.Left := 12;
  FFooterPanel.BorderSpacing.Right := 12;
  FFooterPanel.BorderSpacing.Bottom := 8;
  FFooterPanel.Align := alBottom;

  FCopyBtn := TButton.Create(Self);
  FCopyBtn.Parent := FFooterPanel;
  FCopyBtn.Caption := SCrashCopyBtn;
  FCopyBtn.SetBounds(0, 11, 108, 32);
  FCopyBtn.OnClick := @CopyBtnClick;

  FSaveAsBtn := TButton.Create(Self);
  FSaveAsBtn.Parent := FFooterPanel;
  FSaveAsBtn.Caption := SCrashSaveAsBtn;
  FSaveAsBtn.SetBounds(116, 11, 118, 32);
  FSaveAsBtn.OnClick := @SaveAsBtnClick;

  FOpenFolderBtn := TButton.Create(Self);
  FOpenFolderBtn.Parent := FFooterPanel;
  FOpenFolderBtn.Caption := SCrashOpenFolderBtn;
  FOpenFolderBtn.SetBounds(242, 11, 134, 32);
  FOpenFolderBtn.OnClick := @OpenFolderBtnClick;

  RightX := FFooterPanel.Width;
  if not FIsRecovered then
  begin
    FCloseAppBtn := TButton.Create(Self);
    FCloseAppBtn.Parent := FFooterPanel;
    FCloseAppBtn.Caption := SCrashCloseAppBtn;
    RightX := RightX - 116;
    FCloseAppBtn.SetBounds(RightX, 11, 116, 32);
    FCloseAppBtn.Anchors := [akTop, akRight];
    FCloseAppBtn.OnClick := @CloseAppBtnClick;
    RightX := RightX - 8;
  end
  else
    FCloseAppBtn := nil;

  FContinueBtn := TButton.Create(Self);
  FContinueBtn.Parent := FFooterPanel;
  FContinueBtn.Caption := SCrashContinueBtn;
  RightX := RightX - 108;
  FContinueBtn.SetBounds(RightX, 11, 108, 32);
  FContinueBtn.Anchors := [akTop, akRight];
  FContinueBtn.ModalResult := mrOK;
  FContinueBtn.Default := True;
  FContinueBtn.Cancel := True;

  FStatusLabel := TLabel.Create(Self);
  FStatusLabel.Parent := Self;
  FStatusLabel.Top := 0;
  FStatusLabel.BorderSpacing.Left := 12;
  FStatusLabel.BorderSpacing.Right := 12;
  FStatusLabel.BorderSpacing.Top := 4;
  FStatusLabel.Height := 18;
  FStatusLabel.Caption := '';
  FStatusLabel.Align := alBottom;

  FMemo := TMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Align := alClient;
  FMemo.BorderSpacing.Left := 12;
  FMemo.BorderSpacing.Right := 12;
  FMemo.ReadOnly := True;
  FMemo.ScrollBars := ssAutoBoth;
  FMemo.WordWrap := False;
  FMemo.Font.Name := UIMonoFontName;
  FMemo.Font.Size := 9;
  FMemo.Text := FLogContent;
end;

procedure TCrashReportForm.ApplyTheme;
begin
  ApplyDialogStyle(Self);
  StyleControl(FTopPanel, uirSurface);
  StyleControl(FFooterPanel, uirFooter);
  StyleControl(FPathLabel, uirHint);
  StyleControl(FStatusLabel, uirHint);

  FMemo.Color := UIContentColor;
  FMemo.Font.Color := UITextColor;
  FMemo.Font.Name := UIMonoFontName;
  FMemo.Font.Size := 9;
end;

procedure TCrashReportForm.FormShowHandler(Sender: TObject);
begin
  HandleNeeded;
  ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  if CurrentSkin.Id <> skClassic then
    ApplySkinToForm(Self, CurrentSkin);
  ApplyTheme;
end;

procedure TCrashReportForm.CopyBtnClick(Sender: TObject);
begin
  Clipboard.AsText := FLogContent;
  FStatusLabel.Caption := SCrashCopiedStatus;
end;

procedure TCrashReportForm.SaveAsBtnClick(Sender: TObject);
var
  Dlg: TSaveDialog;
begin
  Dlg := TSaveDialog.Create(Self);
  try
    Dlg.Title := SCrashSaveAsBtn;
    Dlg.Filter := SCrashSaveFilter;
    Dlg.DefaultExt := 'log';
    if FLogFile <> '' then
      Dlg.FileName := ExtractFileName(FLogFile)
    else
      Dlg.FileName := 'ibiblia_crash_' + FormatDateTime('yyyy-mm-dd_hhnnss', Now) + '.log';
    if DirectoryExists(GetCrashLogsDir) then
      Dlg.InitialDir := GetCrashLogsDir;
    if not Dlg.Execute then
      Exit;
    try
      WriteLogCopyToFile(Dlg.FileName, FLogContent);
      FStatusLabel.Caption := Format(SCrashSavedAt, [Dlg.FileName]);
      MessageDlg(Caption, Format(SCrashSaveSuccess, [Dlg.FileName]),
        mtInformation, [mbOK], 0);
    except
      on E: Exception do
        MessageDlg(Caption, Format(SCrashSaveFailed, [E.Message]),
          mtError, [mbOK], 0);
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TCrashReportForm.OpenFolderBtnClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := GetCrashLogsDir;
  if DirectoryExists(Dir) then
    OpenDocument(Dir);
end;

procedure TCrashReportForm.CloseAppBtnClick(Sender: TObject);
begin
  FUserChoseTerminate := True;
  ModalResult := mrAbort;
end;

constructor TCrashLogsManagerForm.CreateManager(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption := SLogsManagerTitle;
  Position := poOwnerFormCenter;
  BorderStyle := bsSizeable;
  Width := 900;
  Height := 580;
  Constraints.MinWidth := 680;
  Constraints.MinHeight := 420;
  OnShow := @FormShowHandler;

  BuildUI;
  ApplyTheme;
  ReloadLogs('');
end;

procedure TCrashLogsManagerForm.BuildUI;
var
  Column: TListColumn;
begin
  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := Self;
  FTopPanel.Align := alTop;
  FTopPanel.Top := 0;
  FTopPanel.Width := ClientWidth - 24;
  FTopPanel.Height := 34;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.BorderSpacing.Left := 12;
  FTopPanel.BorderSpacing.Right := 12;
  FTopPanel.BorderSpacing.Top := 10;

  FFolderLabel := TLabel.Create(Self);
  FFolderLabel.Parent := FTopPanel;
  FFolderLabel.Left := 0;
  FFolderLabel.Top := 6;
  FFolderLabel.Width := FTopPanel.Width - 8;
  FFolderLabel.Anchors := [akTop, akLeft, akRight];
  FFolderLabel.WordWrap := False;
  FFolderLabel.ShowAccelChar := False;
  FFolderLabel.Caption := Format(SLogsManagerFolder, [GetCrashLogsDir]);

  FFooterPanel := TPanel.Create(Self);
  FFooterPanel.Parent := Self;
  FFooterPanel.Width := ClientWidth - 24;
  FFooterPanel.Height := 52;
  FFooterPanel.BevelOuter := bvNone;
  FFooterPanel.BorderSpacing.Left := 12;
  FFooterPanel.BorderSpacing.Right := 12;
  FFooterPanel.BorderSpacing.Bottom := 8;
  FFooterPanel.Align := alBottom;

  FCopyBtn := TButton.Create(Self);
  FCopyBtn.Parent := FFooterPanel;
  FCopyBtn.Caption := SCrashCopyBtn;
  FCopyBtn.SetBounds(0, 10, 104, 32);
  FCopyBtn.OnClick := @CopyBtnClick;

  FSaveAsBtn := TButton.Create(Self);
  FSaveAsBtn.Parent := FFooterPanel;
  FSaveAsBtn.Caption := SCrashSaveAsBtn;
  FSaveAsBtn.SetBounds(112, 10, 112, 32);
  FSaveAsBtn.OnClick := @SaveAsBtnClick;

  FOpenFolderBtn := TButton.Create(Self);
  FOpenFolderBtn.Parent := FFooterPanel;
  FOpenFolderBtn.Caption := SCrashOpenFolderBtn;
  FOpenFolderBtn.SetBounds(232, 10, 130, 32);
  FOpenFolderBtn.OnClick := @OpenFolderBtnClick;

  FDiagBtn := TButton.Create(Self);
  FDiagBtn.Parent := FFooterPanel;
  FDiagBtn.Caption := SLogsManagerDiagBtn;
  FDiagBtn.SetBounds(370, 10, 176, 32);
  FDiagBtn.OnClick := @DiagBtnClick;

  FDeleteBtn := TButton.Create(Self);
  FDeleteBtn.Parent := FFooterPanel;
  FDeleteBtn.Caption := SLogsManagerDeleteBtn;
  FDeleteBtn.SetBounds(554, 10, 118, 32);
  FDeleteBtn.OnClick := @DeleteBtnClick;

  FCloseBtn := TButton.Create(Self);
  FCloseBtn.Parent := FFooterPanel;
  FCloseBtn.Caption := SLogsManagerCloseBtn;
  FCloseBtn.SetBounds(FFooterPanel.Width - 104, 10, 104, 32);
  FCloseBtn.Anchors := [akTop, akRight];
  FCloseBtn.ModalResult := mrClose;
  FCloseBtn.Cancel := True;
  FCloseBtn.Default := True;

  FStatusLabel := TLabel.Create(Self);
  FStatusLabel.Parent := Self;
  FStatusLabel.Top := 0;
  FStatusLabel.BorderSpacing.Left := 12;
  FStatusLabel.BorderSpacing.Right := 12;
  FStatusLabel.BorderSpacing.Top := 4;
  FStatusLabel.Height := 18;
  FStatusLabel.Caption := '';
  FStatusLabel.Align := alBottom;

  FBodyPanel := TPanel.Create(Self);
  FBodyPanel.Parent := Self;
  FBodyPanel.Align := alClient;
  FBodyPanel.BevelOuter := bvNone;

  FPreviewPanel := TPanel.Create(Self);
  FPreviewPanel.Parent := FBodyPanel;
  FPreviewPanel.Height := 250;
  FPreviewPanel.BevelOuter := bvNone;
  FPreviewPanel.BorderSpacing.Left := 12;
  FPreviewPanel.BorderSpacing.Right := 12;
  FPreviewPanel.Align := alBottom;

  FMemo := TMemo.Create(Self);
  FMemo.Parent := FPreviewPanel;
  FMemo.Align := alClient;
  FMemo.ReadOnly := True;
  FMemo.ScrollBars := ssAutoBoth;
  FMemo.WordWrap := False;
  FMemo.Font.Name := UIMonoFontName;
  FMemo.Font.Size := 9;

  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := FBodyPanel;
  FSplitter.Top := 0;
  FSplitter.Height := 6;
  FSplitter.ResizeAnchor := akBottom;
  FSplitter.MinSize := 90;
  FSplitter.BorderSpacing.Left := 12;
  FSplitter.BorderSpacing.Right := 12;
  FSplitter.Align := alBottom;

  FList := TListView.Create(Self);
  FList.Parent := FBodyPanel;
  FList.Align := alClient;
  FList.BorderSpacing.Left := 12;
  FList.BorderSpacing.Right := 12;
  FList.ViewStyle := vsReport;
  FList.ReadOnly := True;
  FList.RowSelect := True;
  FList.HideSelection := False;
  FList.OnSelectItem := @ListSelectItem;

  Column := FList.Columns.Add;
  Column.Caption := SLogsManagerColDate;
  Column.Width := 155;
  Column := FList.Columns.Add;
  Column.Caption := SLogsManagerColKind;
  Column.Width := 125;
  Column := FList.Columns.Add;
  Column.Caption := SLogsManagerColHeadline;
  Column.Width := 490;
  Column := FList.Columns.Add;
  Column.Caption := SLogsManagerColSize;
  Column.Width := 80;
end;

procedure TCrashLogsManagerForm.ApplyTheme;
begin
  ApplyDialogStyle(Self);
  StyleControl(FTopPanel, uirSurface);
  StyleControl(FBodyPanel, uirSurface);
  StyleControl(FPreviewPanel, uirSurface);
  StyleControl(FFooterPanel, uirFooter);
  StyleControl(FFolderLabel, uirHint);
  StyleControl(FStatusLabel, uirHint);

  FList.Color := UIContentColor;
  if CurrentSkin.Id <> skClassic then
    FList.Font.Color := CurrentSkin.WindowText
  else
    FList.Font.Color := UITextColor;

  FMemo.Color := UIContentColor;
  FMemo.Font.Color := UITextColor;
  FMemo.Font.Name := UIMonoFontName;
  FMemo.Font.Size := 9;
end;

procedure TCrashLogsManagerForm.FormShowHandler(Sender: TObject);
begin
  HandleNeeded;
  ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  if CurrentSkin.Id <> skClassic then
    ApplySkinToForm(Self, CurrentSkin);
  ApplyTheme;
end;

procedure TCrashLogsManagerForm.ReloadLogs(const SelectPath: string);
var
  I, TargetRow: Integer;
  Item: TListItem;
begin
  FLogs := ListCrashLogs;
  TargetRow := -1;

  FList.Items.BeginUpdate;
  try
    FList.Items.Clear;
    for I := 0 to High(FLogs) do
    begin
      Item := FList.Items.Add;
      Item.Caption := FLogs[I].TimestampText;
      Item.SubItems.Add(FLogs[I].Kind);
      Item.SubItems.Add(FLogs[I].Headline);
      Item.SubItems.Add(FormatLogSize(FLogs[I].FileSize));
      Item.Data := Pointer(PtrInt(I));
      if (SelectPath <> '') and SameText(FLogs[I].FilePath, SelectPath) then
        TargetRow := I;
    end;
  finally
    FList.Items.EndUpdate;
  end;

  if Length(FLogs) > 0 then
  begin
    if TargetRow < 0 then
      TargetRow := 0;
    FList.Items[TargetRow].Selected := True;
    FList.ItemFocused := FList.Items[TargetRow];
    FList.Items[TargetRow].MakeVisible(False);
  end;
  UpdateSelectionUI;
end;

function TCrashLogsManagerForm.SelectedLogIndex: Integer;
begin
  Result := -1;
  if Assigned(FList.Selected) then
  begin
    Result := PtrInt(FList.Selected.Data);
    if (Result < 0) or (Result > High(FLogs)) then
      Result := -1;
  end;
end;

procedure TCrashLogsManagerForm.UpdateSelectionUI;
var
  Idx: Integer;
  HasSelection: Boolean;
begin
  Idx := SelectedLogIndex;
  HasSelection := Idx >= 0;
  FCopyBtn.Enabled := HasSelection;
  FSaveAsBtn.Enabled := HasSelection;
  FDeleteBtn.Enabled := HasSelection;

  if Length(FLogs) = 0 then
    FMemo.Text := SLogsManagerEmpty
  else if HasSelection then
    FMemo.Text := ReadCrashLogFile(FLogs[Idx].FilePath)
  else
    FMemo.Clear;
end;

procedure TCrashLogsManagerForm.ListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateSelectionUI;
end;

procedure TCrashLogsManagerForm.CopyBtnClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := SelectedLogIndex;
  if Idx < 0 then
    Exit;
  Clipboard.AsText := ReadCrashLogFile(FLogs[Idx].FilePath);
  FStatusLabel.Caption := SCrashCopiedStatus;
end;

procedure TCrashLogsManagerForm.SaveAsBtnClick(Sender: TObject);
var
  Idx: Integer;
  Dlg: TSaveDialog;
  Content: string;
begin
  Idx := SelectedLogIndex;
  if Idx < 0 then
    Exit;
  Content := ReadCrashLogFile(FLogs[Idx].FilePath);

  Dlg := TSaveDialog.Create(Self);
  try
    Dlg.Title := SCrashSaveAsBtn;
    Dlg.Filter := SCrashSaveFilter;
    Dlg.DefaultExt := 'log';
    Dlg.FileName := FLogs[Idx].FileName;
    if DirectoryExists(GetCrashLogsDir) then
      Dlg.InitialDir := GetCrashLogsDir;
    if not Dlg.Execute then
      Exit;
    try
      WriteLogCopyToFile(Dlg.FileName, Content);
      MessageDlg(SLogsManagerTitle, Format(SCrashSaveSuccess, [Dlg.FileName]),
        mtInformation, [mbOK], 0);
    except
      on E: Exception do
        MessageDlg(SLogsManagerTitle, Format(SCrashSaveFailed, [E.Message]),
          mtError, [mbOK], 0);
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TCrashLogsManagerForm.OpenFolderBtnClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := GetCrashLogsDir;
  if DirectoryExists(Dir) then
    OpenDocument(Dir);
end;

procedure TCrashLogsManagerForm.DiagBtnClick(Sender: TObject);
var
  UserNote, NewFile, NewContent: string;
begin
  UserNote := '';
  if not InputQuery(SLogsManagerDiagTitle, SLogsManagerDiagPrompt, UserNote) then
    Exit;
  if not WriteDiagnosticReport(UserNote, NewFile, NewContent) then
  begin
    MessageDlg(SLogsManagerTitle, SLogsManagerDiagFailed, mtError, [mbOK], 0);
    Exit;
  end;
  FStatusLabel.Caption := Format(SCrashSavedAt, [NewFile]);
  ReloadLogs(NewFile);
end;

procedure TCrashLogsManagerForm.DeleteBtnClick(Sender: TObject);
var
  Idx: Integer;
  FilePath: string;
begin
  Idx := SelectedLogIndex;
  if Idx < 0 then
    Exit;
  FilePath := FLogs[Idx].FilePath;
  if MessageDlg(SLogsManagerTitle, Format(SLogsManagerDeleteConfirm, [FilePath]),
     mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  SysUtils.DeleteFile(FilePath);
  FStatusLabel.Caption := '';
  ReloadLogs('');
end;

procedure ShowCrashReportDialog(const LogFile, LogContent: string;
  IsRecoveredHardCrash: Boolean; out ShouldTerminate: Boolean);
var
  Form: TCrashReportForm;
  OwnerForm: TComponent;
begin
  ShouldTerminate := False;
  if Assigned(Application) and Assigned(Application.MainForm) then
    OwnerForm := Application.MainForm
  else
    OwnerForm := Application;
  Form := TCrashReportForm.CreateReport(OwnerForm, LogFile, LogContent,
    IsRecoveredHardCrash);
  try
    Form.ShowModal;
    ShouldTerminate := Form.UserChoseTerminate;
  finally
    Form.Free;
  end;
end;

procedure ShowCrashLogsManagerDialog(AOwner: TComponent);
var
  Form: TCrashLogsManagerForm;
begin
  Form := TCrashLogsManagerForm.CreateManager(AOwner);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

initialization
  RegisterCrashDialogCallback(@ShowCrashReportDialog);

end.
