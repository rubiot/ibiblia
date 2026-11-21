unit project_backup_ui;

{ Dialogs for project backups: the backup folder, "Back up this project",
  the offer to back up before a big change, and the list of backups to
  restore from. The files themselves are made by project_backup. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, Project;

{ The backup folder chosen in the settings ('' if none). }
function GetBackupFolder: string;
procedure SetBackupFolder(const Folder: string);

{ Asks for the backup folder when none is set (or it no longer exists).
  '' when the user cancels. }
function EnsureBackupFolder: string;

{ Saves the project if needed and makes a backup. Shows a message on
  success unless Quiet; shows the error and returns False on failure. }
function BackUpProject(AProject: TProject; const Note: string;
  Automatic, Quiet: Boolean; out BackupFile: string): Boolean;

{ "Back up this project": asks for an optional note, then backs up. }
procedure BackUpProjectWithNote(AProject: TProject);

{ Before a big change: "Back up and continue" / "Continue without backup" /
  Cancel. Returns False when the user cancels (the change must not start). }
function OfferProjectBackup(AProject: TProject; const Reason: string): Boolean;

{ The list of backups of the project; returns the chosen zip file or ''. }
function ChooseBackupToRestore(AOwner: TComponent; AProject: TProject): string;

{ The date and note of a backup, for messages. }
function DescribeBackup(const ZipFile: string): string;

resourcestring
  SBackupTitle = 'Project backup';
  SBackupChooseFolder = 'Choose the folder for project backups';
  SBackupNoteCaption = 'Back up this project';
  SBackupNotePrompt = 'Note for this backup (optional):';
  SBackupDone = 'The project was backed up to:'#13#10'%s';
  SBackupFailed = 'The project could not be backed up:'#13#10'%s';
  SBackupSaveFailed = 'The project could not be saved before the backup.';
  SBackupOfferQuestion = 'Back up this project before continuing?'#13#10#13#10'%s';
  SBackupAndContinue = 'Back up and continue';
  SBackupContinueWithout = 'Continue without backup';
  SBackupContinueAfterFailure = 'The project could not be backed up:'#13#10'%s'#13#10#13#10 +
    'Continue without a backup?';
  SBackupRestoreTitle = 'Restore project';
  SBackupRestoreIntro = 'Backups of this project in %s (newest first):';
  SBackupRestoreNoFolder = 'No backup folder is set yet. Use "Other backup file..." to choose a backup.';
  SBackupRestoreNone = 'No backups of this project were found in this folder.';
  SBackupColumnDate = 'Date';
  SBackupColumnKind = 'Type';
  SBackupColumnNote = 'Note';
  SBackupColumnAssociated = 'Associated verses';
  SBackupColumnSize = 'Size';
  SBackupKindManual = 'Manual';
  SBackupKindAutomatic = 'Automatic';
  SBackupRestoreButton = 'Restore';
  SBackupOtherFile = 'Other backup file...';
  SBackupCancel = 'Cancel';
  SBackupZipFilter = 'Zipped iBiblia projects (*.bib.zip, *.zip)|*.bib.zip;*.zip';
  SBackupNotAProjectZip = 'This file is not a zipped iBiblia project:'#13#10'%s';
  SBackupDescription = 'backup of %s';
  SBackupDescriptionNote = 'backup of %s ("%s")';

implementation

uses
  Graphics, ComCtrls, StdCtrls, ExtCtrls, ui_style, unitskins, formPrincipal,
  project_backup, project_change_log;

const
  BACKUP_SECTION = 'backup';
  BACKUP_FOLDER_KEY = 'folder';

function GetBackupFolder: string;
begin
  Result := '';
  if Assigned(opts) then
    Result := opts.ReadString(BACKUP_SECTION, BACKUP_FOLDER_KEY, '');
end;

procedure SetBackupFolder(const Folder: string);
begin
  if Assigned(opts) then
  begin
    opts.WriteString(BACKUP_SECTION, BACKUP_FOLDER_KEY, Folder);
    opts.UpdateFile;
  end;
end;

function EnsureBackupFolder: string;
var
  Dialog: TSelectDirectoryDialog;
begin
  Result := GetBackupFolder;
  if (Result <> '') and DirectoryExists(Result) then
    Exit;
  Dialog := TSelectDirectoryDialog.Create(nil);
  try
    Dialog.Title := SBackupChooseFolder;
    if Result <> '' then
      Dialog.InitialDir := ExtractFileDir(ExcludeTrailingPathDelimiter(Result))
    else if Assigned(CurrentProject) and (CurrentProject.FileName <> '') then
      Dialog.InitialDir := ExtractFileDir(CurrentProject.FileName);
    Dialog.Options := Dialog.Options + [ofCreatePrompt];
    if Dialog.Execute then
    begin
      Result := Dialog.FileName;
      SetBackupFolder(Result);
    end
    else
      Result := '';
  finally
    Dialog.Free;
  end;
end;

function SaveFirst(AProject: TProject): Boolean;
begin
  Result := True;
  if AProject.Modificado then
    Result := AProject.SaveProject;
  if not Result then
    MessageDlg(SBackupTitle, SBackupSaveFailed, mtError, [mbOK], 0);
end;

function BackUpProject(AProject: TProject; const Note: string;
  Automatic, Quiet: Boolean; out BackupFile: string): Boolean;
var
  Folder: string;
  LogSummary: string;
begin
  Result := False;
  BackupFile := '';
  if not Assigned(AProject) or (AProject.FileName = '') then
    Exit;
  Folder := EnsureBackupFolder;
  if Folder = '' then
    Exit;
  if not SaveFirst(AProject) then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    try
      BackupFile := CreateProjectBackup(AProject.FileName, Folder, Note, Automatic);
      Result := True;
      if Automatic then
        LogSummary := 'Created automatic backup'
      else
        LogSummary := 'Created manual backup';
      if Trim(Note) <> '' then
        LogSummary := LogSummary + ': ' + Trim(Note);
      RecordProjectChange(AProject.FileName, 'Backup', LogSummary,
        'Backup file: ' + BackupFile);
    except
      on E: Exception do
      begin
        Screen.Cursor := crDefault;
        if not Quiet then
          MessageDlg(SBackupTitle, Format(SBackupFailed, [E.Message]), mtError,
            [mbOK], 0)
        else
          BackupFile := E.Message;
        Exit;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  if not Quiet then
    MessageDlg(SBackupTitle, Format(SBackupDone, [BackupFile]), mtInformation,
      [mbOK], 0);
end;

procedure BackUpProjectWithNote(AProject: TProject);
var
  Note, BackupFile: string;
begin
  if not Assigned(AProject) then
    Exit;
  Note := '';
  if not InputQuery(SBackupNoteCaption, SBackupNotePrompt, Note) then
    Exit;
  BackUpProject(AProject, Trim(Note), False, False, BackupFile);
end;

function OfferProjectBackup(AProject: TProject; const Reason: string): Boolean;
var
  BackupFile: string;
begin
  Result := True;
  if not Assigned(AProject) or (AProject.FileName = '') then
    Exit;
  case QuestionDlg(SBackupTitle, Format(SBackupOfferQuestion, [Reason]),
    mtConfirmation, [mrYes, SBackupAndContinue, mrNo, SBackupContinueWithout,
    mrCancel, SBackupCancel], 0) of
    mrYes:
      if not BackUpProject(AProject, Reason, True, True, BackupFile) then
      begin
        { BackupFile holds the error, or '' if the user cancelled }
        if BackupFile = '' then
          Exit(False);
        Result := MessageDlg(SBackupTitle, Format(SBackupContinueAfterFailure,
          [BackupFile]), mtWarning, [mbYes, mbNo], 0) = mrYes;
      end;
    mrNo:
      Result := True;
  else
    Result := False;
  end;
end;

function DescribeBackup(const ZipFile: string): string;
var
  Info: TProjectBackupInfo;
begin
  ReadBackupInfo(ZipFile, Info);
  if Info.Note <> '' then
    Result := Format(SBackupDescriptionNote,
      [FormatDateTime('yyyy-mm-dd hh:nn', Info.Created), Info.Note])
  else
    Result := Format(SBackupDescription,
      [FormatDateTime('yyyy-mm-dd hh:nn', Info.Created)]);
end;

{ the list dialog }

type
  TBackupListForm = class(TForm)
  private
    FList: TListView;
    FIntro: TLabel;
    FRestore, FOther, FCancel: TButton;
    FChosen: string;
    procedure ListDblClick(Sender: TObject);
    procedure ListSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure RestoreClick(Sender: TObject);
    procedure OtherClick(Sender: TObject);
  public
    constructor CreateList(AOwner: TComponent; AProject: TProject);
    property Chosen: string read FChosen;
  end;

function SizeText(Size: Int64): string;
begin
  if Size >= 1024 * 1024 then
    Result := FormatFloat('0.0', Size / (1024 * 1024)) + ' MB'
  else
    Result := FormatFloat('0', Size / 1024) + ' KB';
end;

constructor TBackupListForm.CreateList(AOwner: TComponent; AProject: TProject);
var
  Folder: string;
  Backups: TProjectBackupInfoArray;
  Index: Integer;
  Item: TListItem;
  Column: TListColumn;
  Bottom: TPanel;
begin
  inherited CreateNew(AOwner);
  Caption := SBackupRestoreTitle;
  Position := poOwnerFormCenter;
  Width := 820;
  Height := 420;
  BorderIcons := [biSystemMenu];

  FIntro := TLabel.Create(Self);
  FIntro.Parent := Self;
  FIntro.Align := alTop;
  FIntro.BorderSpacing.Around := 10;
  FIntro.WordWrap := True;

  Bottom := TPanel.Create(Self);
  Bottom.Parent := Self;
  Bottom.Align := alBottom;
  Bottom.Height := 48;
  Bottom.BevelOuter := bvNone;

  FCancel := TButton.Create(Self);
  FCancel.Parent := Bottom;
  FCancel.Caption := SBackupCancel;
  FCancel.SetBounds(Bottom.Width - 130, 10, 120, 30);
  FCancel.Anchors := [akTop, akRight];
  FCancel.ModalResult := mrCancel;
  FCancel.Cancel := True;

  FRestore := TButton.Create(Self);
  FRestore.Parent := Bottom;
  FRestore.Caption := SBackupRestoreButton;
  FRestore.SetBounds(Bottom.Width - 260, 10, 120, 30);
  FRestore.Anchors := [akTop, akRight];
  FRestore.OnClick := @RestoreClick;
  FRestore.Enabled := False;
  FRestore.Default := True;

  FOther := TButton.Create(Self);
  FOther.Parent := Bottom;
  FOther.Caption := SBackupOtherFile;
  FOther.SetBounds(10, 10, 200, 30);
  FOther.OnClick := @OtherClick;

  FList := TListView.Create(Self);
  FList.Parent := Self;
  FList.Align := alClient;
  FList.BorderSpacing.Left := 10;
  FList.BorderSpacing.Right := 10;
  FList.ViewStyle := vsReport;
  FList.ReadOnly := True;
  FList.RowSelect := True;
  FList.HideSelection := False;
  FList.OnDblClick := @ListDblClick;
  FList.OnSelectItem := @ListSelect;
  Column := FList.Columns.Add; Column.Caption := SBackupColumnDate; Column.Width := 140;
  Column := FList.Columns.Add; Column.Caption := SBackupColumnKind; Column.Width := 90;
  Column := FList.Columns.Add; Column.Caption := SBackupColumnNote; Column.Width := 330;
  Column := FList.Columns.Add; Column.Caption := SBackupColumnAssociated; Column.Width := 130;
  Column := FList.Columns.Add; Column.Caption := SBackupColumnSize; Column.Width := 80;

  ApplyDialogStyle(Self);
  if CurrentSkin.Id <> skClassic then
  begin
    FList.Color := CurrentSkin.WindowBg;
    FList.Font.Color := CurrentSkin.WindowText;
  end
  else
    FList.Font.Color := clWindowText;
  Folder := GetBackupFolder;
  if (Folder = '') or not DirectoryExists(Folder) then
  begin
    FIntro.Caption := SBackupRestoreNoFolder;
    Exit;
  end;
  Backups := ListProjectBackups(AProject.FileName, Folder);
  if Length(Backups) = 0 then
    FIntro.Caption := Format(SBackupRestoreIntro, [Folder]) + LineEnding +
      SBackupRestoreNone
  else
    FIntro.Caption := Format(SBackupRestoreIntro, [Folder]);
  for Index := 0 to High(Backups) do
  begin
    Item := FList.Items.Add;
    Item.Caption := FormatDateTime('yyyy-mm-dd hh:nn', Backups[Index].Created);
    if Backups[Index].Automatic then
      Item.SubItems.Add(SBackupKindAutomatic)
    else
      Item.SubItems.Add(SBackupKindManual);
    Item.SubItems.Add(Backups[Index].Note);
    if Backups[Index].AssociatedVerses >= 0 then
      Item.SubItems.Add(IntToStr(Backups[Index].AssociatedVerses))
    else
      Item.SubItems.Add('');
    Item.SubItems.Add(SizeText(Backups[Index].Size));
    Item.SubItems.Add(Backups[Index].FileName);   // hidden column: the file
  end;
  if FList.Items.Count > 0 then
  begin
    FList.Items[0].Selected := True;
    FList.ItemFocused := FList.Items[0];
    ActiveControl := FList;
  end;
end;

procedure TBackupListForm.ListSelect(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  FRestore.Enabled := Assigned(FList.Selected);
end;

procedure TBackupListForm.ListDblClick(Sender: TObject);
begin
  if Assigned(FList.Selected) then
    RestoreClick(Sender);
end;

procedure TBackupListForm.RestoreClick(Sender: TObject);
begin
  if not Assigned(FList.Selected) then
    Exit;
  FChosen := FList.Selected.SubItems[FList.Selected.SubItems.Count - 1];
  ModalResult := mrOK;
end;

procedure TBackupListForm.OtherClick(Sender: TObject);
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(Self);
  try
    Dialog.Title := SBackupRestoreTitle;
    Dialog.Filter := SBackupZipFilter;
    if GetBackupFolder <> '' then
      Dialog.InitialDir := GetBackupFolder;
    if not Dialog.Execute then
      Exit;
    if not IsZippedProject(Dialog.FileName) then
    begin
      MessageDlg(SBackupRestoreTitle, Format(SBackupNotAProjectZip,
        [Dialog.FileName]), mtError, [mbOK], 0);
      Exit;
    end;
    FChosen := Dialog.FileName;
    ModalResult := mrOK;
  finally
    Dialog.Free;
  end;
end;

function ChooseBackupToRestore(AOwner: TComponent; AProject: TProject): string;
var
  Form: TBackupListForm;
begin
  Result := '';
  if not Assigned(AProject) then
    Exit;
  Form := TBackupListForm.CreateList(AOwner, AProject);
  try
    if Form.ShowModal = mrOK then
      Result := Form.Chosen;
  finally
    Form.Free;
  end;
end;

end.
