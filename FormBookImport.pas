unit FormBookImport;

{ Dialogs of "Add Bible Books" and the partial "Replace text" (project
  properties, Texts tab):

  - TFrmBookMapping: for plain documents, the books found in the file and
    the Bible book each one is (guessed, the user confirms);
  - TFrmBookImport: a Bible tree of the file (book / chapter / verse, with
    tick boxes like the patch dialogs) and the project's text beside the
    file's text for the highlighted verse.

  RunBookImportDialog does the whole sequence and returns a
  TBookImportRequest; the project is changed only when the properties are
  applied. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Grids, Dialogs,
  Graphics, BibleScope, BibleTreeView, Project, project_book_import,
  bible_import_source;

{ Asks for a file, shows the dialogs and fills Request. False when the user
  cancels or nothing can be imported (a message has been shown). }
function RunBookImportDialog(AOwner: TComponent; AProject: TProject;
  TextType: TTipoTextoBiblico; const TextName: string; Kind: TBookImportKind;
  out Request: TBookImportRequest): Boolean;

resourcestring
  SBookImportAddTitle = 'Add Bible books to the %s text';
  SBookImportReplaceTitle = 'Replace books, chapters or verses of the %s text';
  SBookImportChooseFile = 'Choose the Bible file';
  SBookImportAllFiles = 'Bible files (theWord, e-Sword, USFM, USFX, MySword, text documents)|' +
    '*.ont;*.ot;*.nt;*.bblx;*.bbli;*.bbl;*.mybible;*.usfm;*.sfm;*.usfx;*.xml;*.txt;*.rtf;*.odt;*.docx' +
    '|theWord Bible modules (*.ont, *.ot, *.nt)|*.ont;*.ot;*.nt' +
    '|e-Sword Bibles (*.bblx, *.bbli, *.bbl)|*.bblx;*.bbli;*.bbl' +
    '|USFM / USFX Bibles (*.usfm, *.sfm, *.usfx, *.xml)|*.usfm;*.sfm;*.usfx;*.xml' +
    '|Text documents (*.txt, *.rtf, *.odt, *.docx)|*.txt;*.rtf;*.odt;*.docx' +
    '|All files|*';
  SBookImportReading = 'Reading the file...';
  SBookImportMappingTitle = 'Match the books of the document';
  SBookImportMappingHint = 'These books were found in the document. Check the Bible book of each one; choose "(do not import)" to leave a book out.';
  SBookImportMappingFound = 'Found in the document';
  SBookImportMappingChapters = 'Chapters';
  SBookImportMappingVerses = 'Verses';
  SBookImportMappingBook = 'Bible book';
  SBookImportSkipBook = '(do not import)';
  SBookImportOK = 'OK';
  SBookImportCancel = 'Cancel';
  SBookImportAddButton = 'Add selected';
  SBookImportReplaceButton = 'Replace selected';
  SBookImportSelectAll = 'Select all';
  SBookImportDeselectAll = 'Deselect all';
  SBookImportKeepAssociations = 'Try to keep the current associations';
  SBookImportInProject = 'In the project';
  SBookImportInFile = 'In the file';
  SBookImportAlreadyInProject = '  (already in the project)';
  SBookImportNotInProject = '(not in the project yet)';
  SBookImportFileInfo = 'File: %s';
  SBookImportAddHint = 'Tick the books (or chapters / verses) to add. Books the %s text already has are greyed out.';
  SBookImportReplaceHint = 'Only verses whose text differs from the project are listed. Tick what to replace.';
  SBookImportNothingToAdd = 'Every book of this file is already in the %s text of the project.';
  SBookImportNothingToReplace = 'The verses of this file that the project has are identical to the %s text of the project. There is nothing to replace.';
  SBookImportIdentical = '%d verses are identical to the project and are not listed.';
  SBookImportNoProjectRows = '%d verses of this file have no place in the project (different verse numbering) and are not listed.';
  SBookImportUnplaced = '%d passages of the document had no chapter or verse number and were left out.';
  SBookImportNothingSelected = 'Nothing is selected.';
  SBookImportDuplicates = '%d verse numbers came twice in the document; their texts were joined into one verse.';
  SBookImportScopeTitle = 'Change the project to the whole Bible?';
  SBookImportScopeOT = 'This project has the Old Testament only. Adding New Testament books makes it a whole-Bible project (Old and New Testament).'#13#10#13#10'Continue?';
  SBookImportScopeNT = 'This project has the New Testament only. Adding Old Testament books makes it a whole-Bible project (Old and New Testament).'#13#10#13#10'Continue?';
  SBookImportSaveFirst = 'Save the project first. Books are added to (or replaced in) the saved project.';
  SBookImportNoBooksMapped = 'No book of the document was matched to a Bible book.';
  SBookImportVerseCountFmt = '%s (%d verses)';

implementation

uses
  LazUTF8, ui_style, bible_versification, apocrypha_books;

type
  TRefEntry = record
    Book, Chapter, Verse: Integer;
    Ref, Text: string;
  end;
  TRefEntryArray = array of TRefEntry;

  { TFrmBookMapping }

  TFrmBookMapping = class(TForm)
  private
    FSource: TBibleImportSource;
    FGrid: TStringGrid;
    FNames: TStringList;   // picklist; Objects = book number
  public
    constructor CreateFor(AOwner: TComponent; ASource: TBibleImportSource);
    destructor Destroy; override;
    procedure StoreMapping;
  end;

  { TFrmBookImport }

  TFrmBookImport = class(TForm)
  private
    FTree: TBibleTreeView;
    FInfo: TLabel;
    FCurrentLabel, FNewLabel: TLabel;
    FCurrentMemo, FNewMemo: TMemo;
    FKeep: TCheckBox;
    FOkButton: TButton;
    FEntries: TRefEntryArray;
    FProjectTexts: TStringList;  // ref -> project text (Objects: index in FProjectValues)
    FProjectValues: TStringList;
    FKind: TBookImportKind;
    FScope: TEscopoTexto;
    procedure SelectAllClick(Sender: TObject);
    procedure DeselectAllClick(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure VerseChanged(Index: Integer);
    procedure LayoutPanes(Sender: TObject);
    procedure OkClick(Sender: TObject);
  public
    NewScope: TEscopoTexto;
    constructor CreateFor(AOwner: TComponent; AKind: TBookImportKind;
      AScope: TEscopoTexto; const ATitle, AInfo: string; AFont: TFont);
    function ProjectText(const Ref: string): string;
  end;

function ScopeOfBook(BookID: Integer): TEscopoTexto;
begin
  if (BookID >= 40) and (BookID <= 66) then
    Result := etNT
  else
    Result := etOT;
end;

function BookInProjectScope(Scope: TEscopoTexto; BookID: Integer): Boolean;
begin
  if BookID >= FIRST_APOCRYPHA_BOOK then
    Result := Scope <> etNT
  else
    Result := BibleBookInScope(Scope, BookID);
end;

{ ---------- TFrmBookMapping ---------- }

constructor TFrmBookMapping.CreateFor(AOwner: TComponent;
  ASource: TBibleImportSource);
var
  HintLabel: TLabel;
  Panel: TPanel;
  Btn: TButton;
  Row, Book, Index: Integer;
  Info: TDetectedBook;
begin
  inherited CreateNew(AOwner);
  FSource := ASource;
  Caption := SBookImportMappingTitle;
  Width := 720;
  Height := 560;
  Position := poMainFormCenter;
  BorderIcons := [biSystemMenu];

  FNames := TStringList.Create;
  FNames.AddObject(SBookImportSkipBook, TObject(PtrInt(0)));
  for Book := 1 to 66 do
    FNames.AddObject(AnyBibleBookName(Book), TObject(PtrInt(Book)));
  for Book := FIRST_APOCRYPHA_BOOK to 86 do
    FNames.AddObject(AnyBibleBookName(Book), TObject(PtrInt(Book)));

  HintLabel := TLabel.Create(Self);
  HintLabel.Parent := Self;
  HintLabel.Align := alTop;
  HintLabel.BorderSpacing.Around := 10;
  HintLabel.WordWrap := True;
  HintLabel.AutoSize := True;
  HintLabel.Caption := SBookImportMappingHint;

  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alBottom;
  Panel.Height := 48;
  Panel.BevelOuter := bvNone;

  Btn := TButton.Create(Self);
  Btn.Parent := Panel;
  Btn.Caption := SBookImportCancel;
  Btn.ModalResult := mrCancel;
  Btn.Cancel := True;
  Btn.SetBounds(Panel.Width - 110, 8, 100, 32);
  Btn.Anchors := [akRight, akTop];

  Btn := TButton.Create(Self);
  Btn.Parent := Panel;
  Btn.Caption := SBookImportOK;
  Btn.ModalResult := mrOK;
  Btn.Default := True;
  Btn.SetBounds(Panel.Width - 220, 8, 100, 32);
  Btn.Anchors := [akRight, akTop];

  FGrid := TStringGrid.Create(Self);
  FGrid.Parent := Self;
  FGrid.Align := alClient;
  FGrid.BorderSpacing.Around := 10;
  FGrid.FixedCols := 0;
  FGrid.Options := FGrid.Options + [goEditing, goColSizing] - [goRangeSelect];
  FGrid.RowCount := ASource.DetectedCount + 1;
  FGrid.Columns.Add.Title.Caption := SBookImportMappingFound;
  FGrid.Columns.Add.Title.Caption := SBookImportMappingChapters;
  FGrid.Columns.Add.Title.Caption := SBookImportMappingVerses;
  with FGrid.Columns.Add do
  begin
    Title.Caption := SBookImportMappingBook;
    ButtonStyle := cbsPickList;
    PickList.Assign(FNames);
    DropDownRows := 20;
  end;
  FGrid.Columns[0].ReadOnly := True;
  FGrid.Columns[1].ReadOnly := True;
  FGrid.Columns[2].ReadOnly := True;
  FGrid.Columns[0].Width := 260;
  FGrid.Columns[1].Width := 80;
  FGrid.Columns[2].Width := 80;
  FGrid.Columns[3].Width := 240;

  Row := 1;
  for Index := 0 to ASource.DetectedCount - 1 do
  begin
    Info := ASource.Detected[Index];
    FGrid.Cells[0, Row] := Info.Name;
    FGrid.Cells[1, Row] := IntToStr(Info.Chapters);
    FGrid.Cells[2, Row] := IntToStr(Info.Verses);
    if Info.BookID > 0 then
      FGrid.Cells[3, Row] := AnyBibleBookName(Info.BookID)
    else
      FGrid.Cells[3, Row] := SBookImportSkipBook;
    if Info.Verses = 0 then
      FGrid.Cells[3, Row] := SBookImportSkipBook;
    Inc(Row);
  end;
  ApplyDialogStyle(Self);
end;

destructor TFrmBookMapping.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

procedure TFrmBookMapping.StoreMapping;
var
  Row, NameIndex: Integer;
begin
  for Row := 1 to FGrid.RowCount - 1 do
  begin
    NameIndex := FNames.IndexOf(FGrid.Cells[3, Row]);
    if NameIndex < 0 then
      FSource.SetDetectedBookID(Row - 1, 0)
    else
      FSource.SetDetectedBookID(Row - 1, PtrInt(FNames.Objects[NameIndex]));
  end;
end;

{ ---------- TFrmBookImport ---------- }

constructor TFrmBookImport.CreateFor(AOwner: TComponent;
  AKind: TBookImportKind; AScope: TEscopoTexto; const ATitle,
  AInfo: string; AFont: TFont);
var
  BottomPanel, LeftPanel, RightPanel, Panes: TPanel;
  Btn: TButton;
  Split: TSplitter;
begin
  inherited CreateNew(AOwner);
  FKind := AKind;
  FScope := AScope;
  NewScope := etNone;
  Caption := ATitle;
  Width := 1000;
  Height := 680;
  Position := poMainFormCenter;
  Constraints.MinWidth := 760;
  Constraints.MinHeight := 480;

  FInfo := TLabel.Create(Self);
  FInfo.Parent := Self;
  FInfo.Align := alTop;
  FInfo.BorderSpacing.Around := 10;
  FInfo.WordWrap := True;
  FInfo.AutoSize := True;
  FInfo.Caption := AInfo;

  BottomPanel := TPanel.Create(Self);
  BottomPanel.Parent := Self;
  BottomPanel.Align := alBottom;
  BottomPanel.Height := 52;
  BottomPanel.BevelOuter := bvNone;

  Btn := TButton.Create(Self);
  Btn.Parent := BottomPanel;
  Btn.Caption := SBookImportSelectAll;
  Btn.SetBounds(10, 10, 120, 32);
  Btn.OnClick := @SelectAllClick;

  Btn := TButton.Create(Self);
  Btn.Parent := BottomPanel;
  Btn.Caption := SBookImportDeselectAll;
  Btn.SetBounds(138, 10, 130, 32);
  Btn.OnClick := @DeselectAllClick;

  FKeep := TCheckBox.Create(Self);
  FKeep.Parent := BottomPanel;
  FKeep.Caption := SBookImportKeepAssociations;
  FKeep.SetBounds(284, 16, 300, 24);
  FKeep.Checked := True;
  FKeep.Visible := AKind = bikReplace;

  Btn := TButton.Create(Self);
  Btn.Parent := BottomPanel;
  Btn.Caption := SBookImportCancel;
  Btn.ModalResult := mrCancel;
  Btn.Cancel := True;
  Btn.SetBounds(BottomPanel.Width - 112, 10, 102, 32);
  Btn.Anchors := [akRight, akTop];

  FOkButton := TButton.Create(Self);
  FOkButton.Parent := BottomPanel;
  if AKind = bikAdd then
    FOkButton.Caption := SBookImportAddButton
  else
    FOkButton.Caption := SBookImportReplaceButton;
  FOkButton.SetBounds(BottomPanel.Width - 270, 10, 150, 32);
  FOkButton.Anchors := [akRight, akTop];
  FOkButton.Default := True;
  FOkButton.OnClick := @OkClick;

  LeftPanel := TPanel.Create(Self);
  LeftPanel.Parent := Self;
  LeftPanel.Align := alLeft;
  LeftPanel.Width := 300;
  LeftPanel.BevelOuter := bvNone;
  LeftPanel.BorderSpacing.Left := 10;

  FTree := TBibleTreeView.Create(LeftPanel);
  FTree.Align := alClient;
  FTree.OnVerseChange := @VerseChanged;
  FTree.OnSelectionChanged := @SelectionChanged;
  FTree.ApplySkinColors;

  Split := TSplitter.Create(Self);
  Split.Parent := Self;
  Split.Align := alLeft;
  Split.Left := LeftPanel.Left + LeftPanel.Width + 1;

  RightPanel := TPanel.Create(Self);
  RightPanel.Parent := Self;
  RightPanel.Align := alClient;
  RightPanel.BevelOuter := bvNone;
  RightPanel.BorderSpacing.Right := 10;

  Panes := TPanel.Create(Self);
  Panes.Parent := RightPanel;
  Panes.Align := alClient;
  Panes.BevelOuter := bvNone;
  Panes.OnResize := @LayoutPanes;

  FCurrentLabel := TLabel.Create(Self);
  FCurrentLabel.Parent := Panes;
  FCurrentLabel.Caption := SBookImportInProject;
  FCurrentLabel.Font.Style := [fsBold];
  FNewLabel := TLabel.Create(Self);
  FNewLabel.Parent := Panes;
  FNewLabel.Caption := SBookImportInFile;
  FNewLabel.Font.Style := [fsBold];

  FCurrentMemo := TMemo.Create(Self);
  FCurrentMemo.Parent := Panes;
  FCurrentMemo.ReadOnly := True;
  FCurrentMemo.ScrollBars := ssAutoVertical;
  FCurrentMemo.WordWrap := True;
  FNewMemo := TMemo.Create(Self);
  FNewMemo.Parent := Panes;
  FNewMemo.ReadOnly := True;
  FNewMemo.ScrollBars := ssAutoVertical;
  FNewMemo.WordWrap := True;
  if Assigned(AFont) then
  begin
    FCurrentMemo.Font.Assign(AFont);
    FNewMemo.Font.Assign(AFont);
  end;

  FProjectTexts := TStringList.Create;
  FProjectValues := TStringList.Create;
  ApplyDialogStyle(Self);
end;

procedure TFrmBookImport.LayoutPanes(Sender: TObject);
var
  P: TPanel;
  W, H: Integer;
begin
  P := TPanel(Sender);
  W := (P.ClientWidth - 10) div 2;
  H := P.ClientHeight - 24;
  FCurrentLabel.SetBounds(0, 2, W, 20);
  FNewLabel.SetBounds(W + 10, 2, W, 20);
  FCurrentMemo.SetBounds(0, 24, W, H);
  FNewMemo.SetBounds(W + 10, 24, W, H);
end;

function TFrmBookImport.ProjectText(const Ref: string): string;
var
  Index: Integer;
begin
  Index := FProjectTexts.IndexOf(Ref);
  if Index < 0 then
    Result := SBookImportNotInProject
  else
    Result := FProjectValues[PtrInt(FProjectTexts.Objects[Index])];
end;

procedure TFrmBookImport.SelectAllClick(Sender: TObject);
begin
  FTree.SelectAll;
  FTree.Invalidate;
  SelectionChanged(nil);
end;

procedure TFrmBookImport.DeselectAllClick(Sender: TObject);
begin
  FTree.SelectNone;
  FTree.Invalidate;
  SelectionChanged(nil);
end;

procedure TFrmBookImport.SelectionChanged(Sender: TObject);
var
  Indexes: TIntegerList;
begin
  if FTree.Items.Count = 0 then
  begin
    FOkButton.Enabled := False;
    Exit;
  end;
  Indexes := FTree.IndexesList;
  try
    FOkButton.Enabled := Indexes.Count > 0;
  finally
    Indexes.Free;
  end;
end;

procedure TFrmBookImport.VerseChanged(Index: Integer);
begin
  if (Index < 0) or (Index > High(FEntries)) then
    Exit;
  FCurrentMemo.Text := ProjectText(FEntries[Index].Ref);
  FNewMemo.Text := FEntries[Index].Text;
end;

procedure TFrmBookImport.OkClick(Sender: TObject);
var
  Indexes: TIntegerList;
  I: Integer;
  NeedsNT, NeedsOT: Boolean;
  Msg: string;
begin
  if FTree.Items.Count = 0 then
    Exit;
  Indexes := FTree.IndexesList;
  try
    if Indexes.Count = 0 then
    begin
      MessageDlg(Caption, SBookImportNothingSelected, mtInformation, [mbOK], 0);
      Exit;
    end;
    NewScope := etNone;
    if FKind = bikAdd then
    begin
      NeedsNT := False;
      NeedsOT := False;
      for I := 0 to Indexes.Count - 1 do
        if not BookInProjectScope(FScope, FEntries[Indexes[I]].Book) then
        begin
          if ScopeOfBook(FEntries[Indexes[I]].Book) = etNT then
            NeedsNT := True
          else
            NeedsOT := True;
        end;
      if NeedsNT or NeedsOT then
      begin
        if FScope = etOT then
          Msg := SBookImportScopeOT
        else
          Msg := SBookImportScopeNT;
        if MessageDlg(SBookImportScopeTitle, Msg, mtConfirmation,
          [mbYes, mbNo], 0) <> mrYes then
          Exit;
        NewScope := etONT;
      end;
    end;
  finally
    Indexes.Free;
  end;
  ModalResult := mrOK;
end;

{ ---------- the whole sequence ---------- }

procedure SortEntries(var Entries: TRefEntryArray);

  function Less(const A, B: TRefEntry): Boolean;
  begin
    if A.Book <> B.Book then
      Exit(A.Book < B.Book);
    if A.Chapter <> B.Chapter then
      Exit(A.Chapter < B.Chapter);
    Result := A.Verse < B.Verse;
  end;

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, T: TRefEntry;
  begin
    repeat
      I := L;
      J := R;
      P := Entries[(L + R) shr 1];
      repeat
        while Less(Entries[I], P) do Inc(I);
        while Less(P, Entries[J]) do Dec(J);
        if I <= J then
        begin
          T := Entries[I];
          Entries[I] := Entries[J];
          Entries[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if Length(Entries) > 1 then
    QuickSort(0, High(Entries));
end;

function RunBookImportDialog(AOwner: TComponent; AProject: TProject;
  TextType: TTipoTextoBiblico; const TextName: string; Kind: TBookImportKind;
  out Request: TBookImportRequest): Boolean;
var
  OpenDlg: TOpenDialog;
  Source: TBibleImportSource;
  Mapping: TFrmBookMapping;
  Dlg: TFrmBookImport;
  ErrorMessage, Title, InfoText, ProjectValue, BookList: string;
  ProjectRefs, ProjectTexts: TStringList;
  BookHasText: array[1..LAST_IMPORT_BOOK] of Boolean;
  BookSelected: array[1..LAST_IMPORT_BOOK] of Integer;
  Entries: TRefEntryArray;
  Count, I, RowIndex, Identical, NoPlace, B, C, V: Integer;
  Verse: TImportVerse;
  InScope, HasRow: Boolean;
  Refs: TStringList;
  Indexes: TIntegerList;
  OldCursor: TCursor;
  Mapped: Boolean;
begin
  Result := False;
  Request := nil;
  if AProject.Modificado then
  begin
    MessageDlg(Format(SBookImportAddTitle, [TextName]), SBookImportSaveFirst,
      mtInformation, [mbOK], 0);
    Exit;
  end;

  OpenDlg := TOpenDialog.Create(nil);
  try
    OpenDlg.Title := SBookImportChooseFile;
    OpenDlg.Filter := SBookImportAllFiles;
    OpenDlg.Options := OpenDlg.Options + [ofFileMustExist];
    if not OpenDlg.Execute then
      Exit;
    Source := TBibleImportSource.Create;
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      if not Source.LoadFromFile(OpenDlg.FileName, ErrorMessage) then
      begin
        Screen.Cursor := OldCursor;
        MessageDlg(SBookImportChooseFile, ErrorMessage, mtError, [mbOK], 0);
        Source.Free;
        Exit;
      end;
    finally
      Screen.Cursor := OldCursor;
    end;
  finally
    OpenDlg.Free;
  end;

  ProjectRefs := TStringList.Create;
  ProjectTexts := TStringList.Create;
  Dlg := nil;
  try
    if Source.IsPlainDocument then
    begin
      Mapping := TFrmBookMapping.CreateFor(AOwner, Source);
      try
        if Mapping.ShowModal <> mrOK then
          Exit;
        Mapping.StoreMapping;
      finally
        Mapping.Free;
      end;
      Mapped := False;
      for I := 0 to Source.DetectedCount - 1 do
        if Source.Detected[I].BookID > 0 then
          Mapped := True;
      if not Mapped then
      begin
        MessageDlg(SBookImportMappingTitle, SBookImportNoBooksMapped,
          mtInformation, [mbOK], 0);
        Exit;
      end;
      Source.ApplyBookMapping;
    end;

    AProject.ReadTextColumn(TextType, ProjectRefs, ProjectTexts);
    FillChar(BookHasText, SizeOf(BookHasText), 0);
    for I := 0 to ProjectRefs.Count - 1 do
    begin
      B := 0;
      SScanf(ProjectRefs[I], '%d', [@B]);
      if (B >= 1) and (B <= LAST_IMPORT_BOOK) and (Trim(ProjectTexts[I]) <> '') then
        BookHasText[B] := True;
      ProjectRefs.Objects[I] := TObject(PtrInt(I));
    end;
    ProjectRefs.Sorted := True;

    SetLength(Entries, Source.VerseCount);
    Count := 0;
    Identical := 0;
    NoPlace := 0;
    for I := 0 to Source.VerseCount - 1 do
    begin
      Verse := Source.Verses[I];
      if (Verse.Book < 1) or (Verse.Book > LAST_IMPORT_BOOK) then
        Continue;
      RowIndex := ProjectRefs.IndexOf(Source.ReferenceID(I));
      HasRow := RowIndex >= 0;
      if Kind = bikReplace then
      begin
        if not HasRow then
        begin
          Inc(NoPlace);
          Continue;
        end;
        ProjectValue := ProjectTexts[PtrInt(ProjectRefs.Objects[RowIndex])];
        if ProjectValue = Verse.Text then
        begin
          Inc(Identical);
          Continue;
        end;
      end
      else
      begin
        InScope := BookInProjectScope(AProject.Escopo, Verse.Book);
        if Verse.Book < FIRST_APOCRYPHA_BOOK then
        begin
          { canonical verses must exist in the KJV numbering }
          if (InScope and not HasRow) or (not InScope and
             ((Verse.Chapter > BibleChapterCount(Verse.Book)) or
              (Verse.Verse > BibleVerseCount(Verse.Book, Verse.Chapter)))) then
          begin
            Inc(NoPlace);
            Continue;
          end;
        end;
      end;
      Entries[Count].Book := Verse.Book;
      Entries[Count].Chapter := Verse.Chapter;
      Entries[Count].Verse := Verse.Verse;
      Entries[Count].Ref := Source.ReferenceID(I);
      Entries[Count].Text := Verse.Text;
      Inc(Count);
    end;
    SetLength(Entries, Count);
    SortEntries(Entries);

    if Kind = bikAdd then
    begin
      I := 0;
      for RowIndex := 0 to High(Entries) do
        if not BookHasText[Entries[RowIndex].Book] then
          Inc(I);
      if I = 0 then
      begin
        MessageDlg(Format(SBookImportAddTitle, [TextName]),
          Format(SBookImportNothingToAdd, [TextName]), mtInformation, [mbOK], 0);
        Exit;
      end;
      Title := Format(SBookImportAddTitle, [TextName]);
      InfoText := Format(SBookImportFileInfo, [ExtractFileName(Source.FileName)]) +
        LineEnding + Format(SBookImportAddHint, [TextName]);
    end
    else
    begin
      if Count = 0 then
      begin
        MessageDlg(Format(SBookImportReplaceTitle, [TextName]),
          Format(SBookImportNothingToReplace, [TextName]), mtInformation,
          [mbOK], 0);
        Exit;
      end;
      Title := Format(SBookImportReplaceTitle, [TextName]);
      InfoText := Format(SBookImportFileInfo, [ExtractFileName(Source.FileName)]) +
        LineEnding + SBookImportReplaceHint;
      if Identical > 0 then
        InfoText := InfoText + LineEnding + Format(SBookImportIdentical, [Identical]);
    end;
    if NoPlace > 0 then
      InfoText := InfoText + LineEnding + Format(SBookImportNoProjectRows, [NoPlace]);
    if Source.UnplacedVerses > 0 then
      InfoText := InfoText + LineEnding +
        Format(SBookImportUnplaced, [Source.UnplacedVerses]);
    if Source.DuplicateVerses > 0 then
      InfoText := InfoText + LineEnding +
        Format(SBookImportDuplicates, [Source.DuplicateVerses]);

    Dlg := TFrmBookImport.CreateFor(AOwner, Kind, AProject.Escopo, Title,
      InfoText, AProject.GetTextFont(TextType));
    Dlg.FEntries := Entries;
    for I := 0 to ProjectRefs.Count - 1 do
      Dlg.FProjectTexts.AddObject(ProjectRefs[I], ProjectRefs.Objects[I]);
    Dlg.FProjectTexts.Sorted := True;
    Dlg.FProjectValues.Assign(ProjectTexts);

    Refs := TStringList.Create;
    try
      for I := 0 to High(Entries) do
        Refs.Add(Entries[I].Ref);
      Dlg.FTree.VerseList := Refs;
    finally
      Refs.Free;
    end;
    if Kind = bikAdd then
    begin
      Dlg.FTree.SelectNone;
      for B := 1 to LAST_IMPORT_BOOK do
        if BookHasText[B] then
          Dlg.FTree.DisableBook(B, SBookImportAlreadyInProject);
    end;
    Dlg.SelectionChanged(nil);
    if Length(Entries) > 0 then
      Dlg.VerseChanged(0);

    if Dlg.ShowModal <> mrOK then
      Exit;

    Request := TBookImportRequest.Create;
    Request.Kind := Kind;
    Request.TextIndex := Ord(TextType);
    Request.FileName := Source.FileName;
    Request.KeepAssociations := (Kind = bikReplace) and Dlg.FKeep.Checked;
    Request.NewScope := Dlg.NewScope;
    FillChar(BookSelected, SizeOf(BookSelected), 0);
    Indexes := Dlg.FTree.IndexesList;
    try
      for I := 0 to Indexes.Count - 1 do
      begin
        Request.Refs.Add(Entries[Indexes[I]].Ref);
        Request.Texts.Add(Entries[Indexes[I]].Text);
        Inc(BookSelected[Entries[Indexes[I]].Book]);
      end;
    finally
      Indexes.Free;
    end;
    BookList := '';
    for B := 1 to LAST_IMPORT_BOOK do
      if BookSelected[B] > 0 then
      begin
        if BookList <> '' then
          BookList := BookList + ', ';
        BookList := BookList + Format(SBookImportVerseCountFmt,
          [Source.BookName(B), BookSelected[B]]);
        if (Kind = bikAdd) and (B >= FIRST_APOCRYPHA_BOOK) then
        begin
          if Request.ApocryphaInfo <> '' then
            Request.ApocryphaInfo := Request.ApocryphaInfo + ';';
          Request.ApocryphaInfo := Request.ApocryphaInfo +
            IntToStr(B) + '=' + StringReplace(Source.BookName(B), ';', ',',
            [rfReplaceAll]);
        end;
      end;
    Request.Summary := BookList;
    Result := Request.Refs.Count > 0;
    if not Result then
      FreeAndNil(Request);
  finally
    Dlg.Free;
    ProjectRefs.Free;
    ProjectTexts.Free;
    Source.Free;
  end;
end;

end.
