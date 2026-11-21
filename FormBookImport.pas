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
  SBookImportChooseArchiveFile = 'Bible files and compressed files';
  SBookImportArchiveTitle = 'Compressed file';
  SBookImportArchiveFiles = 'Files in the compressed file: %d (%d with Bible text).';
  SBookImportArchiveSkipped = 'These files could not be read and were left out:';
  SBookImportArchiveNothing = 'The compressed file has no Bible book that can be imported.';
  SBookImportDuplicateTitle = 'Book found in more than one file';
  SBookImportDuplicateHint = '%s is in several files. Which file should it be taken from?';
  SBookImportDuplicateVerses = '%s  (%d verses)';
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
  SBookImportPreviewVerse = 'Preview: %s';
  SBookImportPreviewVerseUnchecked = 'Preview: %s (not checked)';
  SBookImportPreviewNone = 'Preview: (no verse checked)';
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
  LazUTF8, ui_style, bible_versification, apocrypha_books, bible_archive,
  bible_document_reader, module_conversion, esword_usfm;

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
    FVerseLabel: TLabel;
    FCurrentLabel, FNewLabel: TLabel;
    FCurrentMemo, FNewMemo: TMemo;
    FKeep: TCheckBox;
    FOkButton: TButton;
    FEntries: TRefEntryArray;
    FCurrentIndex: Integer;
    FProjectTexts: TStringList;  // ref -> project text (Objects: index in FProjectValues)
    FProjectValues: TStringList;
    FBookNames: TStringList;     // BookID string -> display name for this dialog
    FKind: TBookImportKind;
    FScope: TEscopoTexto;
    function FormatEntryReference(Index: Integer): string;
    procedure UpdatePreviewHeader;
    procedure ClearPreview;
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
    destructor Destroy; override;
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
  for Book := FIRST_APOCRYPHA_BOOK to 87 do
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
  FCurrentIndex := -1;
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

  FVerseLabel := TLabel.Create(Self);
  FVerseLabel.Parent := Panes;
  FVerseLabel.Caption := SBookImportPreviewNone;
  FVerseLabel.Font.Style := [fsBold];

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
  FBookNames := TStringList.Create;
  ApplyDialogStyle(Self);
end;

destructor TFrmBookImport.Destroy;
begin
  FreeAndNil(FProjectTexts);
  FreeAndNil(FProjectValues);
  FreeAndNil(FBookNames);
  inherited Destroy;
end;

procedure TFrmBookImport.LayoutPanes(Sender: TObject);
var
  P: TPanel;
  W, H, RefH, LblH, TopY: Integer;
begin
  P := TPanel(Sender);
  W := (P.ClientWidth - 10) div 2;
  RefH := UIScale(Self, 22);
  LblH := UIScale(Self, 20);
  TopY := RefH + LblH + UIScale(Self, 8);
  H := P.ClientHeight - TopY;
  if H < 40 then
    H := 40;
  FVerseLabel.SetBounds(0, UIScale(Self, 2), P.ClientWidth, RefH);
  FCurrentLabel.SetBounds(0, RefH + UIScale(Self, 4), W, LblH);
  FNewLabel.SetBounds(W + 10, RefH + UIScale(Self, 4), W, LblH);
  FCurrentMemo.SetBounds(0, TopY, W, H);
  FNewMemo.SetBounds(W + 10, TopY, W, H);
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

function TFrmBookImport.FormatEntryReference(Index: Integer): string;
var
  BName: string;
begin
  if (Index < 0) or (Index > High(FEntries)) then
    Exit('');
  BName := Trim(FBookNames.Values[IntToStr(FEntries[Index].Book)]);
  if BName = '' then
    BName := AnyBibleBookName(FEntries[Index].Book);
  if BName = '' then
    BName := '#' + IntToStr(FEntries[Index].Book);
  Result := Format('%s %d:%d',
    [BName, FEntries[Index].Chapter, FEntries[Index].Verse]);
end;

procedure TFrmBookImport.UpdatePreviewHeader;
var
  RefStr: string;
begin
  if (FCurrentIndex < 0) or (FCurrentIndex > High(FEntries)) then
  begin
    FVerseLabel.Caption := SBookImportPreviewNone;
    Exit;
  end;
  RefStr := FormatEntryReference(FCurrentIndex);
  if FTree.IsVerseChecked(FCurrentIndex) then
    FVerseLabel.Caption := Format(SBookImportPreviewVerse, [RefStr])
  else
    FVerseLabel.Caption := Format(SBookImportPreviewVerseUnchecked, [RefStr]);
end;

procedure TFrmBookImport.ClearPreview;
begin
  FCurrentIndex := -1;
  FVerseLabel.Caption := SBookImportPreviewNone;
  FCurrentMemo.Text := '';
  FNewMemo.Text := '';
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
  NextIdx: Integer;
begin
  if FTree.Items.Count = 0 then
  begin
    FOkButton.Enabled := False;
    ClearPreview;
    Exit;
  end;
  Indexes := FTree.IndexesList;
  try
    FOkButton.Enabled := Indexes.Count > 0;
    if Indexes.Count = 0 then
      ClearPreview
    else if (FCurrentIndex < 0) or (FCurrentIndex > High(FEntries)) or
            not FTree.IsVerseChecked(FCurrentIndex) then
    begin
      NextIdx := -1;
      if Assigned(FTree.Selected) then
        NextIdx := FTree.FirstVerseIndexInNode(FTree.Selected, True);
      if NextIdx < 0 then
        NextIdx := Indexes[0];
      VerseChanged(NextIdx);
    end
    else
      UpdatePreviewHeader;
  finally
    Indexes.Free;
  end;
end;

procedure TFrmBookImport.VerseChanged(Index: Integer);
begin
  if (Index < 0) or (Index > High(FEntries)) then
    Exit;
  if Index <> FCurrentIndex then
  begin
    FCurrentIndex := Index;
    FCurrentMemo.Text := ProjectText(FEntries[Index].Ref);
    FNewMemo.Text := FEntries[Index].Text;
  end;
  UpdatePreviewHeader;
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

{ ---------- compressed files ---------- }

type
  TArchiveBible = record
    Source: TBibleImportSource;
    DisplayName: string;
  end;

function AcceptBibleFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.ont') or (Ext = '.ot') or (Ext = '.nt') or
    IsPlainBibleDocument(FileName) or IsConvertibleBibleFile(FileName);
end;

{ Asks which candidate to use for one book; -1 = cancelled. }
function ChooseBookSource(AOwner: TComponent; const BookTitle: string;
  Labels: TStrings): Integer;
var
  Form: TForm;
  Hint: TLabel;
  Group: TRadioGroup;
  Panel: TPanel;
  Btn: TButton;
begin
  Form := TForm.CreateNew(AOwner);
  try
    Form.Caption := SBookImportDuplicateTitle;
    Form.Width := 640;
    Form.ClientHeight := 130 + 34 * Labels.Count;
    Form.Position := poMainFormCenter;
    Form.BorderIcons := [biSystemMenu];
    Hint := TLabel.Create(Form);
    Hint.Parent := Form;
    Hint.Align := alTop;
    Hint.BorderSpacing.Around := 10;
    Hint.WordWrap := True;
    Hint.AutoSize := True;
    Hint.Caption := Format(SBookImportDuplicateHint, [BookTitle]);
    Panel := TPanel.Create(Form);
    Panel.Parent := Form;
    Panel.Align := alBottom;
    Panel.Height := 48;
    Panel.BevelOuter := bvNone;
    Btn := TButton.Create(Form);
    Btn.Parent := Panel;
    Btn.Caption := SBookImportCancel;
    Btn.ModalResult := mrCancel;
    Btn.Cancel := True;
    Btn.Align := alRight;
    Btn.Width := 110;
    Btn.BorderSpacing.Around := 8;
    Btn := TButton.Create(Form);
    Btn.Parent := Panel;
    Btn.Caption := SBookImportOK;
    Btn.Default := True;
    Btn.ModalResult := mrOK;
    Btn.Align := alRight;
    Btn.Width := 110;
    Btn.BorderSpacing.Around := 8;
    Group := TRadioGroup.Create(Form);
    Group.Parent := Form;
    Group.Align := alClient;
    Group.BorderSpacing.Around := 10;
    Group.Items.Assign(Labels);
    Group.ItemIndex := 0;
    ApplyDialogStyle(Form);
    if Form.ShowModal = mrOK then
      Result := Group.ItemIndex
    else
      Result := -1;
  finally
    Form.Free;
  end;
end;

{ Unpacks a compressed file, reads every Bible file in it (folders too) and
  returns one combined source. Plain documents ask for the book matching;
  a book found in several files asks which file to use. }
function LoadArchiveSource(AOwner: TComponent; const ArchiveFile: string;
  out Combined: TBibleImportSource; PickedFiles: TStrings = nil): Boolean;
var
  Temp, ErrorMessage, Skipped, Title: string;
  Files, Labels, Seen: TStringList;
  Loaded: array of TArchiveBible;
  Src: TBibleImportSource;
  Mapping: TFrmBookMapping;
  I, J, B, Choice, Mapped: Integer;
  Candidates: array[1..LAST_IMPORT_BOOK] of TList;
  Chosen: array[1..LAST_IMPORT_BOOK] of Integer;
  Appended: array[1..LAST_IMPORT_BOOK] of Boolean;
  OldCursor: TCursor;
  Books: TStringList;
  Used: Boolean;
  UsedNames: string;
begin
  Result := False;
  Combined := nil;
  Skipped := '';
  Loaded := nil;
  for B := 1 to LAST_IMPORT_BOOK do
    Candidates[B] := nil;
  { PickedFiles: several Bible files chosen together (no archive to unpack) }
  if Assigned(PickedFiles) then
    Temp := ''
  else
    Temp := NewArchiveTempFolder('ibiblia_bible_');
  if (Temp = '') and not Assigned(PickedFiles) then
  begin
    MessageDlg(SBookImportArchiveTitle, SArchiveFailed, mtError, [mbOK], 0);
    Exit;
  end;
  Files := TStringList.Create;
  Labels := TStringList.Create;
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    try
      if Assigned(PickedFiles) then
      begin
        Files.AddStrings(PickedFiles);
        Files.Sort;
      end
      else
      begin
        if not ExtractArchive(ArchiveFile, Temp, ErrorMessage) then
        begin
          Screen.Cursor := OldCursor;
          MessageDlg(SBookImportArchiveTitle, ErrorMessage, mtError, [mbOK], 0);
          Exit;
        end;
        FindArchiveFiles(Temp, @AcceptBibleFile, Files);
      end;
      for I := 0 to Files.Count - 1 do
      begin
        Src := TBibleImportSource.Create;
        if Assigned(PickedFiles) then
          Title := ExtractFileName(Files[I])
        else
          Title := Copy(Files[I], Length(Temp) + 2, MaxInt);
        if not Src.LoadFromFile(Files[I], ErrorMessage) then
        begin
          Skipped := Skipped + LineEnding + '  ' + Title;
          Src.Free;
          Continue;
        end;
        if Src.IsPlainDocument then
        begin
          Screen.Cursor := OldCursor;
          Mapping := TFrmBookMapping.CreateFor(AOwner, Src);
          try
            Mapping.Caption := Mapping.Caption + ' - ' + Title;
            if Mapping.ShowModal <> mrOK then
            begin
              Src.Free;
              Exit;
            end;
            Mapping.StoreMapping;
          finally
            Mapping.Free;
          end;
          Screen.Cursor := crHourGlass;
          Mapped := 0;
          for J := 0 to Src.DetectedCount - 1 do
            if Src.Detected[J].BookID > 0 then
              Inc(Mapped);
          if Mapped = 0 then
          begin
            Src.Free;
            Continue;
          end;
          Src.ApplyBookMapping;
        end;
        if Src.VerseCount = 0 then
        begin
          Skipped := Skipped + LineEnding + '  ' + Title;
          Src.Free;
          Continue;
        end;
        SetLength(Loaded, Length(Loaded) + 1);
        Loaded[High(Loaded)].Source := Src;
        Loaded[High(Loaded)].DisplayName := Title;
      end;
    finally
      Screen.Cursor := OldCursor;
    end;

    if Length(Loaded) = 0 then
    begin
      MessageDlg(SBookImportArchiveTitle, SBookImportArchiveNothing +
        Skipped, mtInformation, [mbOK], 0);
      Exit;
    end;

    { which files hold which book }
    for I := 0 to High(Loaded) do
    begin
      Books := Loaded[I].Source.BookIDs;
      try
        for J := 0 to Books.Count - 1 do
        begin
          B := StrToIntDef(Books[J], 0);
          if (B < 1) or (B > LAST_IMPORT_BOOK) then
            Continue;
          if Candidates[B] = nil then
            Candidates[B] := TList.Create;
          Candidates[B].Add(Pointer(PtrInt(I)));
        end;
      finally
        Books.Free;
      end;
    end;
    for B := 1 to LAST_IMPORT_BOOK do
    begin
      Chosen[B] := -1;
      if (Candidates[B] <> nil) and (Candidates[B].Count > 0) then
      begin
        Chosen[B] := PtrInt(Candidates[B][0]);
        if Candidates[B].Count > 1 then
        begin
          Labels.Clear;
          for J := 0 to Candidates[B].Count - 1 do
            Labels.Add(Format(SBookImportDuplicateVerses,
              [Loaded[PtrInt(Candidates[B][J])].DisplayName,
               Loaded[PtrInt(Candidates[B][J])].Source.BookVerseCount(B)]));
          Choice := ChooseBookSource(AOwner, Loaded[Chosen[B]].Source.BookName(B), Labels);
          if Choice < 0 then
            Exit;
          Chosen[B] := PtrInt(Candidates[B][Choice]);
        end;
      end;
    end;

    { combine; the order of the files and of their books is kept }
    Combined := TBibleImportSource.Create;
    { several chosen files: name them all (not only the first one) }
    UsedNames := '';
    if Assigned(PickedFiles) then
      for I := 0 to High(Loaded) do
      begin
        Used := False;
        for B := 1 to LAST_IMPORT_BOOK do
          if Chosen[B] = I then
            Used := True;
        if Used then
        begin
          if UsedNames <> '' then
            UsedNames := UsedNames + ', ';
          UsedNames := UsedNames + Loaded[I].DisplayName;
        end;
      end;
    if UsedNames = '' then
      UsedNames := ArchiveFile;
    Combined.BeginCombined(UsedNames);
    FillChar(Appended, SizeOf(Appended), 0);
    for I := 0 to High(Loaded) do
    begin
      Books := Loaded[I].Source.BookIDs;
      try
        { books in the order they appear in this file }
        for J := 0 to Loaded[I].Source.VerseCount - 1 do
        begin
          B := Loaded[I].Source.Verses[J].Book;
          if (B < 1) or (B > LAST_IMPORT_BOOK) or Appended[B] or (Chosen[B] <> I) then
            Continue;
          Appended[B] := True;
          Combined.AppendBook(Loaded[I].Source, B);
        end;
      finally
        Books.Free;
      end;
    end;
    Combined.FinishCombined;
    Result := Combined.VerseCount > 0;
    if not Result then
      FreeAndNil(Combined);
    if Skipped <> '' then
      MessageDlg(SBookImportArchiveTitle, SBookImportArchiveSkipped + Skipped,
        mtInformation, [mbOK], 0);
  finally
    for B := 1 to LAST_IMPORT_BOOK do
      Candidates[B].Free;
    for I := 0 to High(Loaded) do
      Loaded[I].Source.Free;
    Files.Free;
    Labels.Free;
    if Temp <> '' then
      RemoveArchiveFolder(Temp);
  end;
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
  SrcName, MappedRef, UseName: string;
  ProjectRefs, ProjectTexts, SourceBooks: TStringList;
  BookHasText: array[1..LAST_IMPORT_BOOK] of Boolean;
  BookSelected: array[1..LAST_IMPORT_BOOK] of Integer;
  BookMap: array[1..LAST_IMPORT_BOOK] of Integer;
  UsedIDs: array[1..LAST_IMPORT_BOOK] of Boolean;
  MappedBookName: array[1..LAST_IMPORT_BOOK] of string;
  Entries: TRefEntryArray;
  Count, I, RowIndex, Identical, NoPlace, B, C, V: Integer;
  TargetB, FreeB, MappedB: Integer;
  Verse: TImportVerse;
  InScope, HasRow: Boolean;
  Refs: TStringList;
  Indexes: TIntegerList;
  OldCursor: TCursor;
  Mapped: Boolean;
  OrderItems: TStringList;
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
    OpenDlg.Filter := SBookImportChooseArchiveFile + '|*.ont;*.ot;*.nt;*.bblx;*.bbli;*.bbl;*.mybible;*.usfm;*.sfm;*.usfx;*.xml;*.conf;*.txt;*.rtf;*.odt;*.docx;' +
      ArchiveFilterMask + '|' + SBookImportAllFiles;
    OpenDlg.Options := OpenDlg.Options + [ofFileMustExist, ofAllowMultiSelect];
    if not OpenDlg.Execute then
      Exit;
    USFMSingleFileOnly := True;
    try
    if (OpenDlg.Files.Count > 1) and not IsArchiveFile(OpenDlg.Files[0]) then
    begin
      { several files chosen together (for example one USFM file per book) }
      if not LoadArchiveSource(AOwner, OpenDlg.Files[0], Source, OpenDlg.Files) then
        Exit;
    end
    else if IsArchiveFile(OpenDlg.FileName) then
    begin
      if not LoadArchiveSource(AOwner, OpenDlg.FileName, Source) then
        Exit;
    end
    else
    begin
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
    end;
    finally
      USFMSingleFileOnly := False;
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

    for B := 1 to LAST_IMPORT_BOOK do
    begin
      BookMap[B] := B;
      UsedIDs[B] := (B >= FIRST_APOCRYPHA_BOOK) and (BookHasText[B] or IsApocryphaBook(B));
      MappedBookName[B] := '';
    end;
    SourceBooks := Source.BookIDs;
    try
      for I := 0 to SourceBooks.Count - 1 do
      begin
        B := StrToIntDef(SourceBooks[I], 0);
        if (B >= FIRST_APOCRYPHA_BOOK) and (B <= LAST_IMPORT_BOOK) then
        begin
          SrcName := Source.BookName(B);
          TargetB := FindProjectApocryphaBookIDByName(SrcName);
          if (TargetB >= FIRST_APOCRYPHA_BOOK) and (TargetB <= LAST_IMPORT_BOOK) then
          begin
            BookMap[B] := TargetB;
            UsedIDs[TargetB] := True;
            MappedBookName[TargetB] := ApocryphaBookName(TargetB);
          end
          else
          begin
            if UsedIDs[B] then
            begin
              TargetB := B;
              for FreeB := FIRST_APOCRYPHA_BOOK to LAST_IMPORT_BOOK do
                if not UsedIDs[FreeB] then
                begin
                  TargetB := FreeB;
                  Break;
                end;
            end
            else
              TargetB := B;
            BookMap[B] := TargetB;
            UsedIDs[TargetB] := True;
            MappedBookName[TargetB] := SrcName;
          end;
        end;
      end;
    finally
      SourceBooks.Free;
    end;

    SetLength(Entries, Source.VerseCount);
    Count := 0;
    Identical := 0;
    NoPlace := 0;
    for I := 0 to Source.VerseCount - 1 do
    begin
      Verse := Source.Verses[I];
      if (Verse.Book < 1) or (Verse.Book > LAST_IMPORT_BOOK) then
        Continue;
      MappedB := BookMap[Verse.Book];
      MappedRef := Format('%d,%d,%d', [MappedB, Verse.Chapter, Verse.Verse]);
      RowIndex := ProjectRefs.IndexOf(MappedRef);
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
        InScope := BookInProjectScope(AProject.Escopo, MappedB);
        if MappedB < FIRST_APOCRYPHA_BOOK then
        begin
          { canonical verses must exist in the KJV numbering }
          if (InScope and not HasRow) or (not InScope and
             ((Verse.Chapter > BibleChapterCount(MappedB)) or
              (Verse.Verse > BibleVerseCount(MappedB, Verse.Chapter)))) then
          begin
            Inc(NoPlace);
            Continue;
          end;
        end;
      end;
      Entries[Count].Book := MappedB;
      Entries[Count].Chapter := Verse.Chapter;
      Entries[Count].Verse := Verse.Verse;
      Entries[Count].Ref := MappedRef;
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
    for B := FIRST_APOCRYPHA_BOOK to LAST_IMPORT_BOOK do
      if MappedBookName[B] <> '' then
      begin
        Dlg.FBookNames.Values[IntToStr(B)] := MappedBookName[B];
        Dlg.FTree.SetBookCaption(B, MappedBookName[B]);
      end;
    if Kind = bikAdd then
    begin
      Dlg.FTree.SelectNone;
      for B := 1 to LAST_IMPORT_BOOK do
        if BookHasText[B] then
          Dlg.FTree.DisableBook(B, SBookImportAlreadyInProject);
    end;
    Dlg.SelectionChanged(nil);

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
        UseName := MappedBookName[B];
        if UseName = '' then
          UseName := Source.BookName(B);
        if BookList <> '' then
          BookList := BookList + ', ';
        BookList := BookList + Format(SBookImportVerseCountFmt,
          [UseName, BookSelected[B]]);
        if (Kind = bikAdd) and (B >= FIRST_APOCRYPHA_BOOK) then
        begin
          if Request.ApocryphaInfo <> '' then
            Request.ApocryphaInfo := Request.ApocryphaInfo + ';';
          Request.ApocryphaInfo := Request.ApocryphaInfo +
            IntToStr(B) + '=' + StringReplace(UseName, ';', ',',
            [rfReplaceAll]);
        end;
      end;
    if Kind = bikAdd then
    begin
      { only the books ticked, in the order of the imported file }
      OrderItems := TStringList.Create;
      try
        OrderItems.Delimiter := ',';
        OrderItems.StrictDelimiter := True;
        OrderItems.DelimitedText := Source.ApocryphaOrder;
        for I := 0 to OrderItems.Count - 1 do
        begin
          B := StrToIntDef(OrderItems[I], 0);
          if (B >= FIRST_APOCRYPHA_BOOK) and (B <= LAST_IMPORT_BOOK) then
          begin
            MappedB := BookMap[B];
            if (MappedB >= FIRST_APOCRYPHA_BOOK) and (MappedB <= LAST_IMPORT_BOOK) and
               (BookSelected[MappedB] > 0) then
            begin
              if Request.ApocryphaOrder <> '' then
                Request.ApocryphaOrder := Request.ApocryphaOrder + ',';
              Request.ApocryphaOrder := Request.ApocryphaOrder + IntToStr(MappedB);
            end;
          end;
        end;
      finally
        OrderItems.Free;
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
