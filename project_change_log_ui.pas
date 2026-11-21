unit project_change_log_ui;

{ Modal dialog to inspect, filter, annotate, copy, and export the per-project
  change history stored in the currently open .bib SQLite database. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

procedure ShowProjectChangeLogDialog(AOwner: TComponent;
  const ProjectFile, ProjectTitle: string);

implementation

uses
  Forms, StdCtrls, ExtCtrls, ComCtrls, Dialogs, Graphics, Clipbrd, StrUtils,
  ui_style, unitskins, project_change_log;

resourcestring
  SChangeLogTitle = 'Project change log';
  SChangeLogHeaderTitle = 'Project: %s';
  SChangeLogHeaderFile = 'File: %s';
  SChangeLogCount = '%d of %d change(s) shown';
  SChangeLogEmpty = 'No changes recorded for this project yet. Edits, saves, imports, patches, backups and notes will appear here with their date and time.';
  SChangeLogSearchLabel = 'Search:';
  SChangeLogCategoryLabel = 'Category:';
  SChangeLogAllCategories = 'All categories';
  SChangeLogColDate = 'Date & Time';
  SChangeLogColCategory = 'Category';
  SChangeLogColSummary = 'Description';
  SChangeLogDetailsLabel = 'Details:';
  SChangeLogAddNoteBtn = 'Add note...';
  SChangeLogCopyBtn = 'Copy';
  SChangeLogExportBtn = 'Export log...';
  SChangeLogRefreshBtn = 'Refresh';
  SChangeLogCloseBtn = 'Close';
  SChangeLogAddNoteTitle = 'Add note to change log';
  SChangeLogAddNotePrompt = 'Describe the change or milestone to record in this project:';
  SChangeLogAddNoteFailed = 'Could not save the note to the project change log:'#13#10'%s';
  SChangeLogExportTitle = 'Export project change log';
  SChangeLogExportFilter = 'Text report (*.txt)|*.txt|CSV spreadsheet (*.csv)|*.csv';
  SChangeLogExportDone = 'The change log was saved to:'#13#10'%s';
  SChangeLogExportFailed = 'Could not export the change log:'#13#10'%s';

type
  TProjectChangeLogForm = class(TForm)
  private
    FProjectFile: string;
    FProjectTitle: string;
    FEntries: TProjectChangeLogArray;
    FFilteredIndices: array of Integer;
    FHeaderPanel: TPanel;
    FProjectLabel: TLabel;
    FFileLabel: TLabel;
    FCountLabel: TLabel;
    FFilterPanel: TPanel;
    FSearchLabel: TLabel;
    FSearchEdit: TEdit;
    FCategoryLabel: TLabel;
    FCategoryCombo: TComboBox;
    FList: TListView;
    FSplitter: TSplitter;
    FDetailsPanel: TPanel;
    FDetailsLabel: TLabel;
    FDetailsMemo: TMemo;
    FFooterPanel: TPanel;
    FAddNoteBtn: TButton;
    FCopyBtn: TButton;
    FExportBtn: TButton;
    FRefreshBtn: TButton;
    FCloseBtn: TButton;
    procedure BuildUI;
    procedure ApplyTheme;
    procedure FormShowHandler(Sender: TObject);
    procedure ReloadEntries(SelectTop: Boolean = True);
    procedure PopulateCategoryFilter;
    procedure ApplyFilter(KeepEntryId: Int64 = -1);
    procedure ShowSelectedDetails;
    function GatherVisibleEntries: TProjectChangeLogArray;
    procedure FilterChanged(Sender: TObject);
    procedure ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure AddNoteClick(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure ExportClick(Sender: TObject);
    procedure RefreshClick(Sender: TObject);
  public
    constructor CreateDialog(AOwner: TComponent;
      const AProjectFile, AProjectTitle: string);
  end;

procedure WriteUtf8NoBom(const FileName, Content: string);
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

constructor TProjectChangeLogForm.CreateDialog(AOwner: TComponent;
  const AProjectFile, AProjectTitle: string);
begin
  inherited CreateNew(AOwner);
  FProjectFile := AProjectFile;
  if Trim(AProjectTitle) <> '' then
    FProjectTitle := Trim(AProjectTitle)
  else
    FProjectTitle := ChangeFileExt(ExtractFileName(AProjectFile), '');

  Caption := SChangeLogTitle + ' - ' + FProjectTitle;
  Position := poOwnerFormCenter;
  BorderStyle := bsSizeable;
  Width := 920;
  Height := 580;
  Constraints.MinWidth := 680;
  Constraints.MinHeight := 420;
  OnShow := @FormShowHandler;

  BuildUI;
  ApplyTheme;
  ReloadEntries(True);
end;

procedure TProjectChangeLogForm.BuildUI;
var
  Column: TListColumn;
begin
  FHeaderPanel := TPanel.Create(Self);
  FHeaderPanel.Parent := Self;
  FHeaderPanel.Align := alTop;
  FHeaderPanel.Width := ClientWidth - 24;
  FHeaderPanel.Height := 64;
  FHeaderPanel.BevelOuter := bvNone;
  FHeaderPanel.BorderSpacing.Left := 12;
  FHeaderPanel.BorderSpacing.Right := 12;
  FHeaderPanel.BorderSpacing.Top := 10;

  FProjectLabel := TLabel.Create(Self);
  FProjectLabel.Parent := FHeaderPanel;
  FProjectLabel.Left := 0;
  FProjectLabel.Top := 2;
  FProjectLabel.Font.Style := [fsBold];
  FProjectLabel.Caption := Format(SChangeLogHeaderTitle, [FProjectTitle]);

  FCountLabel := TLabel.Create(Self);
  FCountLabel.Parent := FHeaderPanel;
  FCountLabel.Top := 2;
  FCountLabel.Width := 220;
  FCountLabel.Left := FHeaderPanel.Width - 220;
  FCountLabel.Anchors := [akTop, akRight];
  FCountLabel.Alignment := taRightJustify;
  FCountLabel.Caption := '';

  FFileLabel := TLabel.Create(Self);
  FFileLabel.Parent := FHeaderPanel;
  FFileLabel.Left := 0;
  FFileLabel.Top := 24;
  FFileLabel.Width := FHeaderPanel.Width - 8;
  FFileLabel.Anchors := [akTop, akLeft, akRight];
  FFileLabel.WordWrap := False;
  FFileLabel.ShowAccelChar := False;
  FFileLabel.Caption := Format(SChangeLogHeaderFile, [FProjectFile]);

  FFilterPanel := TPanel.Create(Self);
  FFilterPanel.Parent := Self;
  FFilterPanel.Align := alTop;
  FFilterPanel.Width := ClientWidth - 24;
  FFilterPanel.Height := 38;
  FFilterPanel.BevelOuter := bvNone;
  FFilterPanel.BorderSpacing.Left := 12;
  FFilterPanel.BorderSpacing.Right := 12;
  FFilterPanel.BorderSpacing.Bottom := 6;

  FSearchLabel := TLabel.Create(Self);
  FSearchLabel.Parent := FFilterPanel;
  FSearchLabel.Left := 0;
  FSearchLabel.Top := 9;
  FSearchLabel.Caption := SChangeLogSearchLabel;

  FSearchEdit := TEdit.Create(Self);
  FSearchEdit.Parent := FFilterPanel;
  FSearchEdit.Left := 62;
  FSearchEdit.Top := 5;
  FSearchEdit.Width := FFilterPanel.Width - 340;
  FSearchEdit.Anchors := [akTop, akLeft, akRight];
  FSearchEdit.OnChange := @FilterChanged;

  FCategoryLabel := TLabel.Create(Self);
  FCategoryLabel.Parent := FFilterPanel;
  FCategoryLabel.Top := 9;
  FCategoryLabel.Left := FFilterPanel.Width - 266;
  FCategoryLabel.Anchors := [akTop, akRight];
  FCategoryLabel.Caption := SChangeLogCategoryLabel;

  FCategoryCombo := TComboBox.Create(Self);
  FCategoryCombo.Parent := FFilterPanel;
  FCategoryCombo.Style := csDropDownList;
  FCategoryCombo.Top := 5;
  FCategoryCombo.Left := FFilterPanel.Width - 196;
  FCategoryCombo.Width := 196;
  FCategoryCombo.Anchors := [akTop, akRight];
  FCategoryCombo.OnChange := @FilterChanged;

  FFooterPanel := TPanel.Create(Self);
  FFooterPanel.Parent := Self;
  FFooterPanel.Align := alBottom;
  FFooterPanel.Width := ClientWidth - 24;
  FFooterPanel.Height := 50;
  FFooterPanel.BevelOuter := bvNone;
  FFooterPanel.BorderSpacing.Left := 12;
  FFooterPanel.BorderSpacing.Right := 12;
  FFooterPanel.BorderSpacing.Bottom := 6;

  FAddNoteBtn := TButton.Create(Self);
  FAddNoteBtn.Parent := FFooterPanel;
  FAddNoteBtn.Caption := SChangeLogAddNoteBtn;
  FAddNoteBtn.SetBounds(0, 9, 120, 32);
  FAddNoteBtn.OnClick := @AddNoteClick;

  FCopyBtn := TButton.Create(Self);
  FCopyBtn.Parent := FFooterPanel;
  FCopyBtn.Caption := SChangeLogCopyBtn;
  FCopyBtn.SetBounds(128, 9, 96, 32);
  FCopyBtn.OnClick := @CopyClick;

  FExportBtn := TButton.Create(Self);
  FExportBtn.Parent := FFooterPanel;
  FExportBtn.Caption := SChangeLogExportBtn;
  FExportBtn.SetBounds(232, 9, 120, 32);
  FExportBtn.OnClick := @ExportClick;

  FRefreshBtn := TButton.Create(Self);
  FRefreshBtn.Parent := FFooterPanel;
  FRefreshBtn.Caption := SChangeLogRefreshBtn;
  FRefreshBtn.SetBounds(360, 9, 96, 32);
  FRefreshBtn.OnClick := @RefreshClick;

  FCloseBtn := TButton.Create(Self);
  FCloseBtn.Parent := FFooterPanel;
  FCloseBtn.Caption := SChangeLogCloseBtn;
  FCloseBtn.SetBounds(FFooterPanel.Width - 110, 9, 110, 32);
  FCloseBtn.Anchors := [akTop, akRight];
  FCloseBtn.ModalResult := mrClose;
  FCloseBtn.Cancel := True;
  FCloseBtn.Default := True;

  FDetailsPanel := TPanel.Create(Self);
  FDetailsPanel.Parent := Self;
  FDetailsPanel.Align := alBottom;
  FDetailsPanel.Height := 156;
  FDetailsPanel.BevelOuter := bvNone;
  FDetailsPanel.BorderSpacing.Left := 12;
  FDetailsPanel.BorderSpacing.Right := 12;
  FDetailsPanel.BorderSpacing.Bottom := 4;

  FDetailsLabel := TLabel.Create(Self);
  FDetailsLabel.Parent := FDetailsPanel;
  FDetailsLabel.Align := alTop;
  FDetailsLabel.BorderSpacing.Bottom := 4;
  FDetailsLabel.Caption := SChangeLogDetailsLabel;

  FDetailsMemo := TMemo.Create(Self);
  FDetailsMemo.Parent := FDetailsPanel;
  FDetailsMemo.Align := alClient;
  FDetailsMemo.ReadOnly := True;
  FDetailsMemo.ScrollBars := ssAutoBoth;
  FDetailsMemo.WordWrap := True;
  FDetailsMemo.Font.Name := UIMonoFontName;
  FDetailsMemo.Font.Size := 9;

  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Align := alBottom;
  FSplitter.Height := 6;

  FList := TListView.Create(Self);
  FList.Parent := Self;
  FList.Align := alClient;
  FList.BorderSpacing.Left := 12;
  FList.BorderSpacing.Right := 12;
  FList.ViewStyle := vsReport;
  FList.ReadOnly := True;
  FList.RowSelect := True;
  FList.HideSelection := False;
  FList.OnSelectItem := @ListSelectItem;

  Column := FList.Columns.Add;
  Column.Caption := SChangeLogColDate;
  Column.Width := 155;
  Column := FList.Columns.Add;
  Column.Caption := SChangeLogColCategory;
  Column.Width := 115;
  Column := FList.Columns.Add;
  Column.Caption := SChangeLogColSummary;
  Column.Width := 590;
end;

procedure TProjectChangeLogForm.ApplyTheme;
begin
  ApplyDialogStyle(Self);
  StyleControl(FHeaderPanel, uirSurface);
  StyleControl(FFilterPanel, uirSurface);
  StyleControl(FDetailsPanel, uirSurface);
  StyleControl(FFooterPanel, uirFooter);
  StyleControl(FFileLabel, uirHint);
  StyleControl(FCountLabel, uirHint);

  FList.Color := UIContentColor;
  if CurrentSkin.Id <> skClassic then
    FList.Font.Color := CurrentSkin.WindowText
  else
    FList.Font.Color := UITextColor;

  FDetailsMemo.Color := UIContentColor;
  FDetailsMemo.Font.Color := UITextColor;
  FDetailsMemo.Font.Name := UIMonoFontName;
  FDetailsMemo.Font.Size := 9;

  FSearchEdit.Color := UIContentColor;
  FSearchEdit.Font.Color := UITextColor;
  FCategoryCombo.Color := UIContentColor;
  FCategoryCombo.Font.Color := UITextColor;
end;

procedure TProjectChangeLogForm.FormShowHandler(Sender: TObject);
begin
  HandleNeeded;
  ApplyDarkModeToWindow(Self, CurrentSkin.IsDark);
  if CurrentSkin.Id <> skClassic then
    ApplySkinToForm(Self, CurrentSkin);
  ApplyTheme;
end;

procedure TProjectChangeLogForm.ReloadEntries(SelectTop: Boolean);
var
  SelectedId: Int64;
begin
  SelectedId := -1;
  if (not SelectTop) and Assigned(FList.Selected) then
    SelectedId := FEntries[PtrInt(FList.Selected.Data)].Id;

  FEntries := LoadProjectChangeLog(FProjectFile, 5000);
  PopulateCategoryFilter;
  ApplyFilter(SelectedId);
end;

procedure TProjectChangeLogForm.PopulateCategoryFilter;
var
  Categories: TStringList;
  PreviousCategory: string;
  I, NewIndex: Integer;
begin
  PreviousCategory := '';
  if FCategoryCombo.ItemIndex > 0 then
    PreviousCategory := FCategoryCombo.Items[FCategoryCombo.ItemIndex];

  Categories := TStringList.Create;
  try
    Categories.Sorted := True;
    Categories.Duplicates := dupIgnore;
    Categories.CaseSensitive := False;
    for I := 0 to High(FEntries) do
      if Trim(FEntries[I].Category) <> '' then
        Categories.Add(FEntries[I].Category);

    FCategoryCombo.Items.BeginUpdate;
    try
      FCategoryCombo.Items.Clear;
      FCategoryCombo.Items.Add(SChangeLogAllCategories);
      for I := 0 to Categories.Count - 1 do
        FCategoryCombo.Items.Add(Categories[I]);
      NewIndex := 0;
      if PreviousCategory <> '' then
      begin
        I := FCategoryCombo.Items.IndexOf(PreviousCategory);
        if I > 0 then
          NewIndex := I;
      end;
      FCategoryCombo.ItemIndex := NewIndex;
    finally
      FCategoryCombo.Items.EndUpdate;
    end;
  finally
    Categories.Free;
  end;
end;

procedure TProjectChangeLogForm.ApplyFilter(KeepEntryId: Int64);
var
  QueryLower, SelectedCategory, Haystack: string;
  I, VisibleCount, SelectedRow: Integer;
  Item: TListItem;
begin
  QueryLower := AnsiLowerCase(Trim(FSearchEdit.Text));
  if FCategoryCombo.ItemIndex > 0 then
    SelectedCategory := FCategoryCombo.Items[FCategoryCombo.ItemIndex]
  else
    SelectedCategory := '';

  SetLength(FFilteredIndices, Length(FEntries));
  VisibleCount := 0;
  SelectedRow := -1;

  FList.Items.BeginUpdate;
  try
    FList.Items.Clear;
    for I := 0 to High(FEntries) do
    begin
      if (SelectedCategory <> '') and
         (not SameText(FEntries[I].Category, SelectedCategory)) then
        Continue;

      if QueryLower <> '' then
      begin
        Haystack := AnsiLowerCase(
          FEntries[I].ChangedAt + ' ' +
          FEntries[I].Category + ' ' +
          FEntries[I].Summary + ' ' +
          FEntries[I].Details);
        if Pos(QueryLower, Haystack) = 0 then
          Continue;
      end;

      FFilteredIndices[VisibleCount] := I;
      Item := FList.Items.Add;
      Item.Caption := FEntries[I].ChangedAt;
      Item.SubItems.Add(FEntries[I].Category);
      Item.SubItems.Add(FEntries[I].Summary);
      Item.Data := Pointer(PtrInt(I));
      if (KeepEntryId >= 0) and (FEntries[I].Id = KeepEntryId) then
        SelectedRow := VisibleCount;
      Inc(VisibleCount);
    end;
  finally
    FList.Items.EndUpdate;
  end;

  SetLength(FFilteredIndices, VisibleCount);
  FCountLabel.Caption := Format(SChangeLogCount, [VisibleCount, Length(FEntries)]);

  if VisibleCount > 0 then
  begin
    if SelectedRow < 0 then
      SelectedRow := 0;
    FList.Items[SelectedRow].Selected := True;
    FList.ItemFocused := FList.Items[SelectedRow];
    FList.Items[SelectedRow].MakeVisible(False);
  end;
  ShowSelectedDetails;
end;

procedure TProjectChangeLogForm.ShowSelectedDetails;
var
  EntryIdx: Integer;
  Lines: TStringList;
begin
  if Length(FEntries) = 0 then
  begin
    FDetailsMemo.Text := SChangeLogEmpty;
    Exit;
  end;

  if not Assigned(FList.Selected) then
  begin
    FDetailsMemo.Clear;
    Exit;
  end;

  EntryIdx := PtrInt(FList.Selected.Data);
  if (EntryIdx < 0) or (EntryIdx > High(FEntries)) then
  begin
    FDetailsMemo.Clear;
    Exit;
  end;

  Lines := TStringList.Create;
  try
    Lines.Add('Date & Time: ' + FEntries[EntryIdx].ChangedAt);
    Lines.Add('Category:    ' + FEntries[EntryIdx].Category);
    Lines.Add('Description: ' + FEntries[EntryIdx].Summary);
    if Trim(FEntries[EntryIdx].Details) <> '' then
    begin
      Lines.Add('');
      Lines.Add(FEntries[EntryIdx].Details);
    end;
    FDetailsMemo.Text := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function TProjectChangeLogForm.GatherVisibleEntries: TProjectChangeLogArray;
var
  I, Idx: Integer;
begin
  SetLength(Result, Length(FFilteredIndices));
  for I := 0 to High(FFilteredIndices) do
  begin
    Idx := FFilteredIndices[I];
    Result[I] := FEntries[Idx];
  end;
end;

procedure TProjectChangeLogForm.FilterChanged(Sender: TObject);
begin
  ApplyFilter(-1);
end;

procedure TProjectChangeLogForm.ListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ShowSelectedDetails;
end;

procedure TProjectChangeLogForm.AddNoteClick(Sender: TObject);
var
  NoteText, ErrMsg: string;
begin
  NoteText := '';
  if not InputQuery(SChangeLogAddNoteTitle, SChangeLogAddNotePrompt, NoteText) then
    Exit;
  NoteText := Trim(NoteText);
  if NoteText = '' then
    Exit;

  if not RecordProjectChange(FProjectFile, 'Note', NoteText, '', ErrMsg) then
  begin
    MessageDlg(SChangeLogTitle, Format(SChangeLogAddNoteFailed, [ErrMsg]),
      mtError, [mbOK], 0);
    Exit;
  end;

  FSearchEdit.Clear;
  if FCategoryCombo.Items.Count > 0 then
    FCategoryCombo.ItemIndex := 0;
  ReloadEntries(True);
end;

procedure TProjectChangeLogForm.CopyClick(Sender: TObject);
var
  EntryIdx: Integer;
  SingleEntry: TProjectChangeLogArray;
begin
  if Assigned(FList.Selected) then
  begin
    EntryIdx := PtrInt(FList.Selected.Data);
    if (EntryIdx >= 0) and (EntryIdx <= High(FEntries)) then
    begin
      SetLength(SingleEntry, 1);
      SingleEntry[0] := FEntries[EntryIdx];
      Clipboard.AsText := FormatProjectChangeLogText(
        FProjectTitle, FProjectFile, SingleEntry);
      Exit;
    end;
  end;
  Clipboard.AsText := FormatProjectChangeLogText(
    FProjectTitle, FProjectFile, GatherVisibleEntries);
end;

procedure TProjectChangeLogForm.ExportClick(Sender: TObject);
var
  Dlg: TSaveDialog;
  VisibleEntries: TProjectChangeLogArray;
  OutputText, BaseName: string;
begin
  VisibleEntries := GatherVisibleEntries;
  Dlg := TSaveDialog.Create(Self);
  try
    Dlg.Title := SChangeLogExportTitle;
    Dlg.Filter := SChangeLogExportFilter;
    Dlg.DefaultExt := 'txt';
    BaseName := ChangeFileExt(ExtractFileName(FProjectFile), '');
    if BaseName = '' then
      BaseName := 'project';
    Dlg.FileName := BaseName + '_changelog_' +
      FormatDateTime('yyyy-mm-dd_hhnn', Now) + '.txt';
    if ExtractFileDir(FProjectFile) <> '' then
      Dlg.InitialDir := ExtractFileDir(FProjectFile);
    if not Dlg.Execute then
      Exit;

    if SameText(ExtractFileExt(Dlg.FileName), '.csv') or (Dlg.FilterIndex = 2) then
      OutputText := FormatProjectChangeLogCSV(VisibleEntries)
    else
      OutputText := FormatProjectChangeLogText(
        FProjectTitle, FProjectFile, VisibleEntries);

    try
      WriteUtf8NoBom(Dlg.FileName, OutputText);
      MessageDlg(SChangeLogTitle, Format(SChangeLogExportDone, [Dlg.FileName]),
        mtInformation, [mbOK], 0);
    except
      on E: Exception do
        MessageDlg(SChangeLogTitle, Format(SChangeLogExportFailed, [E.Message]),
          mtError, [mbOK], 0);
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TProjectChangeLogForm.RefreshClick(Sender: TObject);
begin
  ReloadEntries(False);
end;

procedure ShowProjectChangeLogDialog(AOwner: TComponent;
  const ProjectFile, ProjectTitle: string);
var
  Form: TProjectChangeLogForm;
begin
  Form := TProjectChangeLogForm.CreateDialog(AOwner, ProjectFile, ProjectTitle);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

end.
