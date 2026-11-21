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
  Math, Types, KMemo, KEditCommon, KGraphics,
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
  TChangeDiffSpan = record
    Text: UnicodeString;
    Changed: Boolean;
  end;
  TChangeDiffSpanArray = array of TChangeDiffSpan;
  TChangeDiffTokenArray = array of UnicodeString;

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
    FDetailsFrame: TPanel;
    FDetailsMemo: TKMemo;
    FDetailsFontSize: Integer;
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
    procedure RenderDetailsPlain(const AText: string);
    procedure RenderDetailsEntry(const Entry: TProjectChangeLogEntry);
    procedure DetailsMemoMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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

{ ---------- Unicode character-level diff helpers ---------- }

procedure AppendChangeDiffSpan(var Spans: TChangeDiffSpanArray;
  const S: UnicodeString; Changed: Boolean);
var
  LastIdx: Integer;
begin
  if S = '' then
    Exit;
  LastIdx := High(Spans);
  if (LastIdx >= 0) and (Spans[LastIdx].Changed = Changed) then
    Spans[LastIdx].Text := Spans[LastIdx].Text + S
  else
  begin
    SetLength(Spans, LastIdx + 2);
    Spans[LastIdx + 1].Text := S;
    Spans[LastIdx + 1].Changed := Changed;
  end;
end;

function SplitChangeDiffTokens(const S: UnicodeString): TChangeDiffTokenArray;
var
  I, Len, StartPos, Count: Integer;
  Ch: WideChar;

  procedure AddTok(const T: UnicodeString);
  begin
    if Count >= Length(Result) then
      SetLength(Result, Max(16, Length(Result) * 2));
    Result[Count] := T;
    Inc(Count);
  end;

  function IsWordChar(C: WideChar): Boolean;
  begin
    Result := (C > ' ') and (C <> '<') and (C <> '>') and (C <> '=') and
              (C <> '"') and (C <> '''') and (C <> '/') and (C <> '\');
  end;

begin
  Result := nil;
  Count := 0;
  Len := Length(S);
  I := 1;
  while I <= Len do
  begin
    Ch := S[I];
    if Ch = '<' then
    begin
      StartPos := I;
      Inc(I);
      while (I <= Len) and (S[I] <> '>') and (S[I] <> #10) do
        Inc(I);
      if (I <= Len) and (S[I] = '>') then
        Inc(I);
      AddTok(Copy(S, StartPos, I - StartPos));
    end
    else if (Ch = ' ') or (Ch = #9) then
    begin
      StartPos := I;
      while (I <= Len) and ((S[I] = ' ') or (S[I] = #9)) do
        Inc(I);
      AddTok(Copy(S, StartPos, I - StartPos));
    end
    else if IsWordChar(Ch) then
    begin
      StartPos := I;
      while (I <= Len) and IsWordChar(S[I]) do
        Inc(I);
      AddTok(Copy(S, StartPos, I - StartPos));
    end
    else
    begin
      AddTok(Ch);
      Inc(I);
    end;
  end;
  SetLength(Result, Count);
end;

procedure DiffChangeCharRange(const A, B: UnicodeString;
  var OldSpans, NewSpans: TChangeDiffSpanArray);
const
  MAX_CHAR_CELLS = 200000;
var
  LenA, LenB, PrefixLen, SuffixLen, MidA, MidB: Integer;
  AMid, BMid: UnicodeString;
  DP: array of Integer;
  Cols, I, J: Integer;
begin
  LenA := Length(A);
  LenB := Length(B);
  PrefixLen := 0;
  while (PrefixLen < LenA) and (PrefixLen < LenB) and
        (A[PrefixLen + 1] = B[PrefixLen + 1]) do
    Inc(PrefixLen);

  SuffixLen := 0;
  while (SuffixLen < LenA - PrefixLen) and (SuffixLen < LenB - PrefixLen) and
        (A[LenA - SuffixLen] = B[LenB - SuffixLen]) do
    Inc(SuffixLen);

  if PrefixLen > 0 then
  begin
    AppendChangeDiffSpan(OldSpans, Copy(A, 1, PrefixLen), False);
    AppendChangeDiffSpan(NewSpans, Copy(B, 1, PrefixLen), False);
  end;

  MidA := LenA - PrefixLen - SuffixLen;
  MidB := LenB - PrefixLen - SuffixLen;
  if (MidA > 0) or (MidB > 0) then
  begin
    AMid := Copy(A, PrefixLen + 1, MidA);
    BMid := Copy(B, PrefixLen + 1, MidB);
    if (MidA = 0) or (MidB = 0) or (Int64(MidA + 1) * Int64(MidB + 1) > MAX_CHAR_CELLS) then
    begin
      AppendChangeDiffSpan(OldSpans, AMid, True);
      AppendChangeDiffSpan(NewSpans, BMid, True);
    end
    else
    begin
      Cols := MidB + 1;
      SetLength(DP, (MidA + 1) * Cols);
      for I := 0 to High(DP) do
        DP[I] := 0;
      for I := MidA - 1 downto 0 do
        for J := MidB - 1 downto 0 do
          if AMid[I + 1] = BMid[J + 1] then
            DP[I * Cols + J] := DP[(I + 1) * Cols + (J + 1)] + 1
          else
            DP[I * Cols + J] := Max(DP[(I + 1) * Cols + J], DP[I * Cols + (J + 1)]);

      I := 0;
      J := 0;
      while (I < MidA) and (J < MidB) do
      begin
        if AMid[I + 1] = BMid[J + 1] then
        begin
          AppendChangeDiffSpan(OldSpans, AMid[I + 1], False);
          AppendChangeDiffSpan(NewSpans, BMid[J + 1], False);
          Inc(I);
          Inc(J);
        end
        else if DP[(I + 1) * Cols + J] >= DP[I * Cols + (J + 1)] then
        begin
          AppendChangeDiffSpan(OldSpans, AMid[I + 1], True);
          Inc(I);
        end
        else
        begin
          AppendChangeDiffSpan(NewSpans, BMid[J + 1], True);
          Inc(J);
        end;
      end;
      while I < MidA do
      begin
        AppendChangeDiffSpan(OldSpans, AMid[I + 1], True);
        Inc(I);
      end;
      while J < MidB do
      begin
        AppendChangeDiffSpan(NewSpans, BMid[J + 1], True);
        Inc(J);
      end;
    end;
  end;

  if SuffixLen > 0 then
  begin
    AppendChangeDiffSpan(OldSpans, Copy(A, LenA - SuffixLen + 1, SuffixLen), False);
    AppendChangeDiffSpan(NewSpans, Copy(B, LenB - SuffixLen + 1, SuffixLen), False);
  end;
end;

procedure ComputeChangeLogDiff(const AText, BText: string;
  out OldSpans, NewSpans: TChangeDiffSpanArray);
const
  MAX_TOK_CELLS = 200000;
var
  A, B, AMid, BMid, HunkA, HunkB: UnicodeString;
  LenA, LenB, PrefixLen, SuffixLen, MidA, MidB: Integer;
  TokA, TokB: TChangeDiffTokenArray;
  TA, TB, Cols, I, J: Integer;
  DP: array of Integer;
begin
  OldSpans := nil;
  NewSpans := nil;
  A := UTF8Decode(StringReplace(StringReplace(AText, #13#10, #10, [rfReplaceAll]),
    #13, #10, [rfReplaceAll]));
  B := UTF8Decode(StringReplace(StringReplace(BText, #13#10, #10, [rfReplaceAll]),
    #13, #10, [rfReplaceAll]));

  if A = B then
  begin
    AppendChangeDiffSpan(OldSpans, A, False);
    AppendChangeDiffSpan(NewSpans, B, False);
    Exit;
  end;

  LenA := Length(A);
  LenB := Length(B);
  PrefixLen := 0;
  while (PrefixLen < LenA) and (PrefixLen < LenB) and
        (A[PrefixLen + 1] = B[PrefixLen + 1]) do
    Inc(PrefixLen);

  SuffixLen := 0;
  while (SuffixLen < LenA - PrefixLen) and (SuffixLen < LenB - PrefixLen) and
        (A[LenA - SuffixLen] = B[LenB - SuffixLen]) do
    Inc(SuffixLen);

  if PrefixLen > 0 then
  begin
    AppendChangeDiffSpan(OldSpans, Copy(A, 1, PrefixLen), False);
    AppendChangeDiffSpan(NewSpans, Copy(B, 1, PrefixLen), False);
  end;

  MidA := LenA - PrefixLen - SuffixLen;
  MidB := LenB - PrefixLen - SuffixLen;
  if (MidA > 0) or (MidB > 0) then
  begin
    AMid := Copy(A, PrefixLen + 1, MidA);
    BMid := Copy(B, PrefixLen + 1, MidB);
    TokA := SplitChangeDiffTokens(AMid);
    TokB := SplitChangeDiffTokens(BMid);
    TA := Length(TokA);
    TB := Length(TokB);
    if (TA > 0) and (TB > 0) and (Int64(TA + 1) * Int64(TB + 1) <= MAX_TOK_CELLS) then
    begin
      Cols := TB + 1;
      SetLength(DP, (TA + 1) * Cols);
      for I := 0 to High(DP) do
        DP[I] := 0;
      for I := TA - 1 downto 0 do
        for J := TB - 1 downto 0 do
          if TokA[I] = TokB[J] then
            DP[I * Cols + J] := DP[(I + 1) * Cols + (J + 1)] + 1
          else
            DP[I * Cols + J] := Max(DP[(I + 1) * Cols + J], DP[I * Cols + (J + 1)]);

      I := 0;
      J := 0;
      HunkA := '';
      HunkB := '';
      while (I < TA) and (J < TB) do
      begin
        if TokA[I] = TokB[J] then
        begin
          if (HunkA <> '') or (HunkB <> '') then
          begin
            DiffChangeCharRange(HunkA, HunkB, OldSpans, NewSpans);
            HunkA := '';
            HunkB := '';
          end;
          AppendChangeDiffSpan(OldSpans, TokA[I], False);
          AppendChangeDiffSpan(NewSpans, TokB[J], False);
          Inc(I);
          Inc(J);
        end
        else if DP[(I + 1) * Cols + J] >= DP[I * Cols + (J + 1)] then
        begin
          HunkA := HunkA + TokA[I];
          Inc(I);
        end
        else
        begin
          HunkB := HunkB + TokB[J];
          Inc(J);
        end;
      end;
      while I < TA do
      begin
        HunkA := HunkA + TokA[I];
        Inc(I);
      end;
      while J < TB do
      begin
        HunkB := HunkB + TokB[J];
        Inc(J);
      end;
      if (HunkA <> '') or (HunkB <> '') then
        DiffChangeCharRange(HunkA, HunkB, OldSpans, NewSpans);
    end
    else
      DiffChangeCharRange(AMid, BMid, OldSpans, NewSpans);
  end;

  if SuffixLen > 0 then
  begin
    AppendChangeDiffSpan(OldSpans, Copy(A, LenA - SuffixLen + 1, SuffixLen), False);
    AppendChangeDiffSpan(NewSpans, Copy(B, LenB - SuffixLen + 1, SuffixLen), False);
  end;
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
  Width := 940;
  Height := 640;
  Constraints.MinWidth := 680;
  Constraints.MinHeight := 460;
  FDetailsFontSize := 9;
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
  FDetailsPanel.Height := 220;
  FDetailsPanel.BevelOuter := bvNone;
  FDetailsPanel.BorderSpacing.Left := 12;
  FDetailsPanel.BorderSpacing.Right := 12;
  FDetailsPanel.BorderSpacing.Bottom := 4;

  FDetailsLabel := TLabel.Create(Self);
  FDetailsLabel.Parent := FDetailsPanel;
  FDetailsLabel.Align := alTop;
  FDetailsLabel.BorderSpacing.Bottom := 4;
  FDetailsLabel.Caption := SChangeLogDetailsLabel;

  FDetailsFrame := TPanel.Create(Self);
  FDetailsFrame.Parent := FDetailsPanel;
  FDetailsFrame.Align := alClient;
  FDetailsFrame.BevelOuter := bvLowered;
  FDetailsFrame.BorderSpacing.Around := 0;

  FDetailsMemo := TKMemo.Create(Self);
  FDetailsMemo.Parent := FDetailsFrame;
  FDetailsMemo.Align := alClient;
  FDetailsMemo.ReadOnly := True;
  FDetailsMemo.TabStop := True;
  FDetailsMemo.BorderStyle := bsNone;
  FDetailsMemo.ScrollBars := ssVertical;
  FDetailsMemo.ParentFont := False;
  FDetailsMemo.TextStyle.Font.Name := UIMonoFontName;
  FDetailsMemo.TextStyle.Font.Size := FDetailsFontSize;
  FDetailsMemo.OnMouseWheel := @DetailsMemoMouseWheel;

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

  if CurrentSkin.Id = skClassic then
  begin
    FDetailsFrame.Color := clWindow;
    FDetailsMemo.Color := clWindow;
    FDetailsMemo.Background.Color := clWindow;
    FDetailsMemo.Colors.BkGnd := clWindow;
    FDetailsMemo.Colors.SelBkGnd := clHighlight;
    FDetailsMemo.Colors.SelText := clHighlightText;
    FDetailsMemo.Colors.SelBkGndFocused := clHighlight;
    FDetailsMemo.Colors.SelTextFocused := clHighlightText;
    FDetailsMemo.TextStyle.Font.Color := UITextColor;
  end
  else
  begin
    FDetailsFrame.Color := CurrentSkin.WindowBg;
    FDetailsMemo.Color := CurrentSkin.WindowBg;
    FDetailsMemo.Background.Color := CurrentSkin.WindowBg;
    FDetailsMemo.Colors.BkGnd := CurrentSkin.WindowBg;
    FDetailsMemo.Colors.SelBkGnd := CurrentSkin.SelectedBg;
    FDetailsMemo.Colors.SelText := CurrentSkin.SelectedText;
    FDetailsMemo.Colors.SelBkGndFocused := CurrentSkin.SelectedBg;
    FDetailsMemo.Colors.SelTextFocused := CurrentSkin.SelectedText;
    FDetailsMemo.TextStyle.Font.Color := CurrentSkin.WindowText;
  end;
  FDetailsMemo.TextStyle.Font.Name := UIMonoFontName;
  FDetailsMemo.TextStyle.Font.Size := FDetailsFontSize;
  if FDetailsMemo.HandleAllocated then
  begin
    ApplyDarkModeToControl(FDetailsMemo, CurrentSkin.IsDark);
    {$IFDEF WINDOWS}
    HookNCScrollBarControl(FDetailsMemo);
    {$ENDIF}
  end;

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
  ShowSelectedDetails;
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

procedure TProjectChangeLogForm.RenderDetailsPlain(const AText: string);
var
  NormalFg: TColor;
  TB: TKMemoTextBlock;
begin
  if CurrentSkin.Id = skClassic then
    NormalFg := UITextColor
  else
    NormalFg := CurrentSkin.WindowText;

  FDetailsMemo.Blocks.LockUpdate;
  try
    FDetailsMemo.Blocks.Clear;
    FDetailsMemo.Blocks.DefaultParaStyle.FirstIndent := 0;
    FDetailsMemo.Blocks.DefaultParaStyle.LeftPadding := 6;
    FDetailsMemo.Blocks.DefaultParaStyle.TopPadding := 2;
    FDetailsMemo.Blocks.DefaultParaStyle.BottomPadding := 2;
    FDetailsMemo.Blocks.DefaultParaStyle.HAlign := halLeft;
    if AText <> '' then
    begin
      TB := FDetailsMemo.Blocks.AddTextBlock(AText);
      TB.TextStyle.Font.Name := UIMonoFontName;
      TB.TextStyle.Font.Size := FDetailsFontSize;
      TB.TextStyle.Font.Color := NormalFg;
      TB.TextStyle.Font.Style := [];
      TB.TextStyle.AllowBrush := False;
      TB.TextStyle.Brush.Style := bsClear;
    end;
    FDetailsMemo.Modified := False;
    FDetailsMemo.TopPos := 0;
  finally
    FDetailsMemo.Blocks.UnLockUpdate;
    FDetailsMemo.Invalidate;
  end;
end;

procedure TProjectChangeLogForm.RenderDetailsEntry(const Entry: TProjectChangeLogEntry);
var
  NormalFg, MutedFg: TColor;
  DelHiBg, DelHiFg, DelSignFg: TColor;
  AddHiBg, AddHiFg, AddSignFg: TColor;
  DetailLines: TStringList;
  I, J, K, IndentLen, AfterIdx, EndIdx: Integer;
  Line, TrimmedLeft, CurrentHeader, BeforeText, AfterText, DiffOld, DiffNew: string;
  OldSpans, NewSpans: TChangeDiffSpanArray;

  procedure AddStyledRun(const S: string; Fg: TColor; Bold: Boolean;
    HasBg: Boolean; Bg: TColor);
  var
    TB: TKMemoTextBlock;
  begin
    if S = '' then
      Exit;
    TB := FDetailsMemo.Blocks.AddTextBlock(S);
    TB.TextStyle.Font.Name := UIMonoFontName;
    TB.TextStyle.Font.Size := FDetailsFontSize;
    TB.TextStyle.Font.Color := Fg;
    if Bold then
      TB.TextStyle.Font.Style := [fsBold]
    else
      TB.TextStyle.Font.Style := [];
    if HasBg then
    begin
      TB.TextStyle.AllowBrush := True;
      TB.TextStyle.Brush.Style := bsSolid;
      TB.TextStyle.Brush.Color := Bg;
    end
    else
    begin
      TB.TextStyle.AllowBrush := False;
      TB.TextStyle.Brush.Style := bsClear;
    end;
  end;

  procedure AddMetaLine(const LabelText, ValueText: string);
  begin
    AddStyledRun(LabelText, MutedFg, True, False, clNone);
    AddStyledRun(ValueText, NormalFg, False, False, clNone);
    FDetailsMemo.Blocks.AddParagraph;
  end;

  procedure RenderDiffSide(const Spans: TChangeDiffSpanArray; IsAdded: Boolean);
  var
    SIdx, SegStart, PosBreak: Integer;
    S, Seg: UnicodeString;
    HiBg, HiFg, SignFg: TColor;
    SignStr: string;
    AtLineStart: Boolean;

    procedure EmitGutter;
    begin
      AddStyledRun(SignStr, SignFg, True, False, clNone);
    end;

  begin
    if IsAdded then
    begin
      HiBg := AddHiBg;
      HiFg := AddHiFg;
      SignFg := AddSignFg;
      SignStr := '  + ';
    end
    else
    begin
      HiBg := DelHiBg;
      HiFg := DelHiFg;
      SignFg := DelSignFg;
      SignStr := '  - ';
    end;

    AtLineStart := True;
    if Length(Spans) = 0 then
    begin
      EmitGutter;
      AddStyledRun('(empty)', MutedFg, False, False, clNone);
      FDetailsMemo.Blocks.AddParagraph;
      Exit;
    end;

    for SIdx := 0 to High(Spans) do
    begin
      S := Spans[SIdx].Text;
      SegStart := 1;
      while SegStart <= Length(S) do
      begin
        if AtLineStart then
        begin
          EmitGutter;
          AtLineStart := False;
        end;
        PosBreak := SegStart;
        while (PosBreak <= Length(S)) and (S[PosBreak] <> #10) do
          Inc(PosBreak);
        Seg := Copy(S, SegStart, PosBreak - SegStart);
        if Seg <> '' then
        begin
          if Spans[SIdx].Changed then
            AddStyledRun(UTF8Encode(Seg), HiFg, True, True, HiBg)
          else
            AddStyledRun(UTF8Encode(Seg), NormalFg, False, False, clNone);
        end;
        if (PosBreak <= Length(S)) and (S[PosBreak] = #10) then
        begin
          if Spans[SIdx].Changed and (Seg = '') then
            AddStyledRun(' ', HiFg, True, True, HiBg);
          FDetailsMemo.Blocks.AddParagraph;
          AtLineStart := True;
          SegStart := PosBreak + 1;
        end
        else
          Break;
      end;
    end;
    if not AtLineStart then
      FDetailsMemo.Blocks.AddParagraph;
  end;

  function IsTextEditHeader(const HeaderLine: string): Boolean;
  var
    L: string;
  begin
    L := LowerCase(HeaderLine);
    Result :=
      (Pos('(source text)', L) > 0) or
      (Pos('(destination text)', L) > 0) or
      (Pos('(reference 1 text)', L) > 0) or
      (Pos('(reference 2 text)', L) > 0) or
      (Pos('(comments)', L) > 0) or
      (Pos('module properties', L) > 0);
  end;

  function CountLeadingSpaces(const S: string): Integer;
  begin
    Result := 0;
    while (Result < Length(S)) and (S[Result + 1] = ' ') do
      Inc(Result);
  end;

  function IsNextEntryBoundary(const S: string; BaseIndent: Integer): Boolean;
  var
    TrimL: string;
    Sp: Integer;
  begin
    TrimL := TrimLeft(S);
    if TrimL = '' then
      Exit(True);
    Sp := CountLeadingSpaces(S);
    if Sp < BaseIndent then
      Exit(True);
    if (Sp = BaseIndent) and
       ((Copy(TrimL, 1, 7) = 'Before:') or (Copy(TrimL, 1, 6) = 'After:')) then
      Exit(True);
    Result := False;
  end;

begin
  if CurrentSkin.Id = skClassic then
    NormalFg := UITextColor
  else
    NormalFg := CurrentSkin.WindowText;
  MutedFg := UIMutedTextColor;

  if CurrentSkin.IsDark then
  begin
    DelHiBg   := MakeRGB(148, 38, 46);
    DelHiFg   := MakeRGB(255, 232, 232);
    DelSignFg := MakeRGB(255, 130, 138);

    AddHiBg   := MakeRGB(30, 120, 62);
    AddHiFg   := MakeRGB(226, 255, 232);
    AddSignFg := MakeRGB(110, 235, 145);
  end
  else
  begin
    DelHiBg   := MakeRGB(255, 190, 196);
    DelHiFg   := MakeRGB(136, 12, 20);
    DelSignFg := MakeRGB(190, 30, 38);

    AddHiBg   := MakeRGB(172, 242, 189);
    AddHiFg   := MakeRGB(12, 98, 34);
    AddSignFg := MakeRGB(26, 127, 55);
  end;

  FDetailsMemo.Blocks.LockUpdate;
  try
    FDetailsMemo.Blocks.Clear;
    FDetailsMemo.Blocks.DefaultParaStyle.FirstIndent := 0;
    FDetailsMemo.Blocks.DefaultParaStyle.LeftPadding := 6;
    FDetailsMemo.Blocks.DefaultParaStyle.TopPadding := 1;
    FDetailsMemo.Blocks.DefaultParaStyle.BottomPadding := 1;
    FDetailsMemo.Blocks.DefaultParaStyle.HAlign := halLeft;

    AddMetaLine('Date & Time: ', Entry.ChangedAt);
    AddMetaLine('Category:    ', Entry.Category);
    AddMetaLine('Description: ', Entry.Summary);

    if Trim(Entry.Details) <> '' then
    begin
      FDetailsMemo.Blocks.AddParagraph;
      DetailLines := TStringList.Create;
      try
        DetailLines.Text := StringReplace(
          StringReplace(Entry.Details, #13#10, #10, [rfReplaceAll]),
          #13, #10, [rfReplaceAll]);
        CurrentHeader := '';
        I := 0;
        while I < DetailLines.Count do
        begin
          Line := DetailLines[I];
          TrimmedLeft := TrimLeft(Line);
          if (Copy(TrimmedLeft, 1, 7) = 'Before:') and
             IsTextEditHeader(CurrentHeader) then
          begin
            IndentLen := CountLeadingSpaces(Line);
            BeforeText := TrimLeft(Copy(TrimmedLeft, 8, MaxInt));
            AfterIdx := -1;
            for J := I + 1 to DetailLines.Count - 1 do
            begin
              if (CountLeadingSpaces(DetailLines[J]) = IndentLen) and
                 (Copy(TrimLeft(DetailLines[J]), 1, 6) = 'After:') then
              begin
                AfterIdx := J;
                Break;
              end;
              if (CountLeadingSpaces(DetailLines[J]) < IndentLen) and
                 (Trim(DetailLines[J]) <> '') then
                Break;
            end;

            if AfterIdx >= 0 then
            begin
              for K := I + 1 to AfterIdx - 1 do
                BeforeText := BeforeText + #10 + DetailLines[K];
              AfterText := TrimLeft(Copy(TrimLeft(DetailLines[AfterIdx]), 7, MaxInt));
              EndIdx := AfterIdx;
              for K := AfterIdx + 1 to DetailLines.Count - 1 do
              begin
                if IsNextEntryBoundary(DetailLines[K], IndentLen) then
                  Break;
                AfterText := AfterText + #10 + DetailLines[K];
                EndIdx := K;
              end;

              if BeforeText = '(empty)' then
                DiffOld := ''
              else
                DiffOld := BeforeText;
              if AfterText = '(empty)' then
                DiffNew := ''
              else
                DiffNew := AfterText;

              ComputeChangeLogDiff(DiffOld, DiffNew, OldSpans, NewSpans);
              RenderDiffSide(OldSpans, False);
              RenderDiffSide(NewSpans, True);
              I := EndIdx + 1;
              Continue;
            end;
          end;

          if (Trim(Line) <> '') and (Line[Length(Line)] = ':') then
          begin
            CurrentHeader := Line;
            AddStyledRun(Line, NormalFg, True, False, clNone);
          end
          else
            AddStyledRun(Line, NormalFg, False, False, clNone);
          FDetailsMemo.Blocks.AddParagraph;
          Inc(I);
        end;
      finally
        DetailLines.Free;
      end;
    end;

    FDetailsMemo.Modified := False;
    FDetailsMemo.TopPos := 0;
  finally
    FDetailsMemo.Blocks.UnLockUpdate;
    FDetailsMemo.Invalidate;
  end;
end;

procedure TProjectChangeLogForm.DetailsMemoMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  NewSize: Integer;
begin
  if ssCtrl in Shift then
  begin
    if WheelDelta > 0 then
      NewSize := Min(22, FDetailsFontSize + 1)
    else
      NewSize := Max(7, FDetailsFontSize - 1);
    if NewSize <> FDetailsFontSize then
    begin
      FDetailsFontSize := NewSize;
      FDetailsMemo.TextStyle.Font.Size := FDetailsFontSize;
      ShowSelectedDetails;
    end;
    Handled := True;
  end;
end;

procedure TProjectChangeLogForm.ShowSelectedDetails;
var
  EntryIdx: Integer;
begin
  if Length(FEntries) = 0 then
  begin
    RenderDetailsPlain(SChangeLogEmpty);
    Exit;
  end;

  if not Assigned(FList.Selected) then
  begin
    RenderDetailsPlain('');
    Exit;
  end;

  EntryIdx := PtrInt(FList.Selected.Data);
  if (EntryIdx < 0) or (EntryIdx > High(FEntries)) then
  begin
    RenderDetailsPlain('');
    Exit;
  end;

  RenderDetailsEntry(FEntries[EntryIdx]);
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
