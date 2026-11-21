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
  SBookImportDiffOne = '%s - 1 difference';
  SBookImportDiffMany = '%s - %d differences';
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
  Math, Types, LazUTF8, LMessages, LCLType, LCLIntf, KMemo, KEditCommon,
  KFunctions, KGraphics, ui_style, unitskins, bible_versification,
  apocrypha_books, bible_archive, bible_document_reader, module_conversion,
  esword_usfm;

type
  TRefEntry = record
    Book, Chapter, Verse: Integer;
    Ref, Text: string;
  end;
  TRefEntryArray = array of TRefEntry;

  TDiffSpan = record
    Text: UnicodeString;
    Changed: Boolean;
  end;
  TDiffSpanArray = array of TDiffSpan;
  TDiffTokenArray = array of UnicodeString;

  TBookDiffSide = (bdsProject, bdsFile);
  TBookDiffZoomEvent = procedure(Sender: TObject; NewSize: Integer) of object;

  { TBookDiffMemo }

  TBookDiffMemo = class(TKMemo)
  private
    FSide: TBookDiffSide;
    FBaseFontName: string;
    FBaseFontSize: Integer;
    FLastSpans: TDiffSpanArray;
    FLastTopPos: Integer;
    FOnScrollSync: TNotifyEvent;
    FOnZoomChanged: TBookDiffZoomEvent;
    procedure NotifyScrollChanged;
    procedure RenderSpans;
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
    procedure WndProc(var TheMessage: TLMessage); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    constructor CreateForSide(AOwner: TComponent; ASide: TBookDiffSide);
    procedure ApplySkinColors;
    procedure SetBaseFont(AFont: TFont);
    procedure SetFontSizeSynced(NewSize: Integer);
    procedure SetPlainText(const AText: string);
    procedure SetDiffSpans(const ASpans: TDiffSpanArray);
    procedure HandleWheelInput(Shift: TShiftState; WheelDelta: Integer);
    procedure SyncTopPosFromPeer(ATopPos: Integer);
    property OnScrollSync: TNotifyEvent read FOnScrollSync write FOnScrollSync;
    property OnZoomChanged: TBookDiffZoomEvent read FOnZoomChanged write FOnZoomChanged;
  end;

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
    FCurrentFrame, FNewFrame: TPanel;
    FCurrentMemo, FNewMemo: TBookDiffMemo;
    FKeep: TCheckBox;
    FOkButton: TButton;
    FEntries: TRefEntryArray;
    FCurrentIndex: Integer;
    FCurrentDiffCount: Integer;
    FSyncingScroll: Boolean;
    FProjectTexts: TStringList;  // ref -> project text (Objects: index in FProjectValues)
    FProjectValues: TStringList;
    FBookNames: TStringList;     // BookID string -> display name for this dialog
    FKind: TBookImportKind;
    FScope: TEscopoTexto;
    function FormatEntryReference(Index: Integer): string;
    function HasProjectText(const Ref: string; out AText: string): Boolean;
    procedure UpdatePreviewHeader;
    procedure ClearPreview;
    procedure SyncPreviewScroll(Sender: TObject);
    procedure SyncPreviewZoom(Sender: TObject; NewSize: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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
  end;

{ ---------- character diff helpers ---------- }

procedure AppendDiffSpan(var Spans: TDiffSpanArray; const S: UnicodeString;
  Changed: Boolean);
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

function SplitDiffTokens(const S: UnicodeString): TDiffTokenArray;
var
  I, Len, StartPos, Count: Integer;
  Ch: WideChar;

  procedure AddToken(const T: UnicodeString);
  begin
    if T = '' then
      Exit;
    if Count >= Length(Result) then
      SetLength(Result, Max(16, Length(Result) * 2));
    Result[Count] := T;
    Inc(Count);
  end;

begin
  Result := nil;
  Count := 0;
  Len := Length(S);
  I := 1;
  while I <= Len do
  begin
    Ch := S[I];
    if Ch = #10 then
    begin
      AddToken(Ch);
      Inc(I);
    end
    else if (Ch = ' ') or (Ch = #9) then
    begin
      StartPos := I;
      while (I <= Len) and ((S[I] = ' ') or (S[I] = #9)) do
        Inc(I);
      AddToken(Copy(S, StartPos, I - StartPos));
    end
    else if (Ch = '<') or (Ch = '>') or (Ch = '=') or (Ch = '"') then
    begin
      AddToken(Ch);
      Inc(I);
    end
    else
    begin
      StartPos := I;
      while (I <= Len) and (S[I] <> #10) and (S[I] <> ' ') and (S[I] <> #9) and
            (S[I] <> '<') and (S[I] <> '>') and (S[I] <> '=') and (S[I] <> '"') do
        Inc(I);
      AddToken(Copy(S, StartPos, I - StartPos));
    end;
  end;
  SetLength(Result, Count);
end;

procedure DiffCharRange(const A, B: UnicodeString;
  var OldSpans, NewSpans: TDiffSpanArray; var DiffCount: Integer);
var
  LenA, LenB, PrefixLen, SuffixLen, MaxFix, MM, NN, Stride, I, J: Integer;
  Prefix, Suffix, AMid, BMid: UnicodeString;
  DP: array of Integer;
  InDiff: Boolean;
begin
  LenA := Length(A);
  LenB := Length(B);
  PrefixLen := 0;
  MaxFix := Min(LenA, LenB);
  while (PrefixLen < MaxFix) and (A[PrefixLen + 1] = B[PrefixLen + 1]) do
    Inc(PrefixLen);
  if PrefixLen > 0 then
  begin
    Prefix := Copy(A, 1, PrefixLen);
    AppendDiffSpan(OldSpans, Prefix, False);
    AppendDiffSpan(NewSpans, Prefix, False);
  end;

  SuffixLen := 0;
  while (SuffixLen < LenA - PrefixLen) and
        (SuffixLen < LenB - PrefixLen) and
        (A[LenA - SuffixLen] = B[LenB - SuffixLen]) do
    Inc(SuffixLen);

  MM := LenA - PrefixLen - SuffixLen;
  NN := LenB - PrefixLen - SuffixLen;
  AMid := Copy(A, PrefixLen + 1, MM);
  BMid := Copy(B, PrefixLen + 1, NN);

  if (MM = 0) and (NN = 0) then
  begin
    { identical after prefix/suffix }
  end
  else if (MM = 0) or (NN = 0) then
  begin
    AppendDiffSpan(OldSpans, AMid, True);
    AppendDiffSpan(NewSpans, BMid, True);
    Inc(DiffCount);
  end
  else if Int64(MM) * Int64(NN) <= 100000 then
  begin
    DP := nil;
    Stride := NN + 1;
    SetLength(DP, (MM + 1) * Stride);
    FillChar(DP[0], Length(DP) * SizeOf(Integer), 0);
    for I := MM - 1 downto 0 do
      for J := NN - 1 downto 0 do
        if AMid[I + 1] = BMid[J + 1] then
          DP[I * Stride + J] := DP[(I + 1) * Stride + (J + 1)] + 1
        else if DP[(I + 1) * Stride + J] >= DP[I * Stride + (J + 1)] then
          DP[I * Stride + J] := DP[(I + 1) * Stride + J]
        else
          DP[I * Stride + J] := DP[I * Stride + (J + 1)];

    I := 0;
    J := 0;
    InDiff := False;
    while (I < MM) and (J < NN) do
    begin
      if AMid[I + 1] = BMid[J + 1] then
      begin
        AppendDiffSpan(OldSpans, AMid[I + 1], False);
        AppendDiffSpan(NewSpans, BMid[J + 1], False);
        InDiff := False;
        Inc(I);
        Inc(J);
      end
      else if DP[(I + 1) * Stride + J] >= DP[I * Stride + (J + 1)] then
      begin
        AppendDiffSpan(OldSpans, AMid[I + 1], True);
        if not InDiff then
        begin
          Inc(DiffCount);
          InDiff := True;
        end;
        Inc(I);
      end
      else
      begin
        AppendDiffSpan(NewSpans, BMid[J + 1], True);
        if not InDiff then
        begin
          Inc(DiffCount);
          InDiff := True;
        end;
        Inc(J);
      end;
    end;
    if I < MM then
    begin
      AppendDiffSpan(OldSpans, Copy(AMid, I + 1, MM - I), True);
      if not InDiff then
      begin
        Inc(DiffCount);
        InDiff := True;
      end;
    end;
    if J < NN then
    begin
      AppendDiffSpan(NewSpans, Copy(BMid, J + 1, NN - J), True);
      if not InDiff then
        Inc(DiffCount);
    end;
  end
  else
  begin
    AppendDiffSpan(OldSpans, AMid, True);
    AppendDiffSpan(NewSpans, BMid, True);
    Inc(DiffCount);
  end;

  if SuffixLen > 0 then
  begin
    Suffix := Copy(A, LenA - SuffixLen + 1, SuffixLen);
    AppendDiffSpan(OldSpans, Suffix, False);
    AppendDiffSpan(NewSpans, Suffix, False);
  end;
end;

procedure BuildVerseDiffSpans(const OldUtf8, NewUtf8: string;
  out OldSpans, NewSpans: TDiffSpanArray; out DiffCount: Integer);
var
  NormOld, NormNew: string;
  A, B, Prefix, Suffix, AMid, BMid, HunkA, HunkB: UnicodeString;
  LenA, LenB, PrefixLen, SuffixLen, MaxFix, MM, NN, TA, TB, Stride, I, J: Integer;
  TokA, TokB: TDiffTokenArray;
  DP: array of Integer;
begin
  OldSpans := nil;
  NewSpans := nil;
  DiffCount := 0;

  NormOld := StringReplace(OldUtf8, #13#10, #10, [rfReplaceAll]);
  NormOld := StringReplace(NormOld, #13, #10, [rfReplaceAll]);
  NormNew := StringReplace(NewUtf8, #13#10, #10, [rfReplaceAll]);
  NormNew := StringReplace(NormNew, #13, #10, [rfReplaceAll]);

  if NormOld <> NormNew then
  begin
    A := UTF8Decode(NormOld);
    B := UTF8Decode(NormNew);
  end
  else if OldUtf8 <> NewUtf8 then
  begin
    A := UTF8Decode(StringReplace(OldUtf8, #13, '[CR]', [rfReplaceAll]));
    B := UTF8Decode(StringReplace(NewUtf8, #13, '[CR]', [rfReplaceAll]));
  end
  else
  begin
    AppendDiffSpan(OldSpans, UTF8Decode(NormOld), False);
    AppendDiffSpan(NewSpans, UTF8Decode(NormNew), False);
    Exit;
  end;

  LenA := Length(A);
  LenB := Length(B);
  PrefixLen := 0;
  MaxFix := Min(LenA, LenB);
  while (PrefixLen < MaxFix) and (A[PrefixLen + 1] = B[PrefixLen + 1]) do
    Inc(PrefixLen);
  if PrefixLen > 0 then
  begin
    Prefix := Copy(A, 1, PrefixLen);
    AppendDiffSpan(OldSpans, Prefix, False);
    AppendDiffSpan(NewSpans, Prefix, False);
  end;

  SuffixLen := 0;
  while (SuffixLen < LenA - PrefixLen) and
        (SuffixLen < LenB - PrefixLen) and
        (A[LenA - SuffixLen] = B[LenB - SuffixLen]) do
    Inc(SuffixLen);

  MM := LenA - PrefixLen - SuffixLen;
  NN := LenB - PrefixLen - SuffixLen;
  AMid := Copy(A, PrefixLen + 1, MM);
  BMid := Copy(B, PrefixLen + 1, NN);

  if (MM > 0) or (NN > 0) then
  begin
    TokA := SplitDiffTokens(AMid);
    TokB := SplitDiffTokens(BMid);
    TA := Length(TokA);
    TB := Length(TokB);
    if (TA > 0) and (TB > 0) and (Int64(TA) * Int64(TB) <= 250000) then
    begin
      DP := nil;
      Stride := TB + 1;
      SetLength(DP, (TA + 1) * Stride);
      FillChar(DP[0], Length(DP) * SizeOf(Integer), 0);
      for I := TA - 1 downto 0 do
        for J := TB - 1 downto 0 do
          if TokA[I] = TokB[J] then
            DP[I * Stride + J] := DP[(I + 1) * Stride + (J + 1)] + 1
          else if DP[(I + 1) * Stride + J] >= DP[I * Stride + (J + 1)] then
            DP[I * Stride + J] := DP[(I + 1) * Stride + J]
          else
            DP[I * Stride + J] := DP[I * Stride + (J + 1)];

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
            DiffCharRange(HunkA, HunkB, OldSpans, NewSpans, DiffCount);
            HunkA := '';
            HunkB := '';
          end;
          AppendDiffSpan(OldSpans, TokA[I], False);
          AppendDiffSpan(NewSpans, TokB[J], False);
          Inc(I);
          Inc(J);
        end
        else if DP[(I + 1) * Stride + J] >= DP[I * Stride + (J + 1)] then
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
        DiffCharRange(HunkA, HunkB, OldSpans, NewSpans, DiffCount);
    end
    else
      DiffCharRange(AMid, BMid, OldSpans, NewSpans, DiffCount);
  end;

  if SuffixLen > 0 then
  begin
    Suffix := Copy(A, LenA - SuffixLen + 1, SuffixLen);
    AppendDiffSpan(OldSpans, Suffix, False);
    AppendDiffSpan(NewSpans, Suffix, False);
  end;
end;

{ ---------- TBookDiffMemo ---------- }

constructor TBookDiffMemo.CreateForSide(AOwner: TComponent; ASide: TBookDiffSide);
begin
  inherited Create(AOwner);
  FSide := ASide;
  ReadOnly := True;
  TabStop := True;
  BorderStyle := bsNone;
  ScrollBars := ssVertical;
  ParentFont := False;
  FBaseFontName := UIDefaultFontName;
  FBaseFontSize := 10;
  FLastTopPos := 0;
  TextStyle.Font.Name := FBaseFontName;
  TextStyle.Font.Size := FBaseFontSize;
  OnMouseWheel := @HandleMouseWheel;
  ApplySkinColors;
end;

procedure TBookDiffMemo.ApplySkinColors;
begin
  if CurrentSkin.Id = skClassic then
  begin
    Color := clWindow;
    Background.Color := clWindow;
    Colors.BkGnd := clWindow;
    Colors.SelBkGnd := clHighlight;
    Colors.SelText := clHighlightText;
    Colors.SelBkGndFocused := clHighlight;
    Colors.SelTextFocused := clHighlightText;
    TextStyle.Font.Color := UITextColor;
  end
  else
  begin
    Color := CurrentSkin.WindowBg;
    Background.Color := CurrentSkin.WindowBg;
    Colors.BkGnd := CurrentSkin.WindowBg;
    Colors.SelBkGnd := CurrentSkin.SelectedBg;
    Colors.SelText := CurrentSkin.SelectedText;
    Colors.SelBkGndFocused := CurrentSkin.SelectedBg;
    Colors.SelTextFocused := CurrentSkin.SelectedText;
    TextStyle.Font.Color := CurrentSkin.WindowText;
  end;

  if HandleAllocated then
  begin
    ApplyDarkModeToControl(Self, CurrentSkin.IsDark);
    {$IFDEF WINDOWS}
    HookNCScrollBarControl(Self);
    {$ENDIF}
  end;
  if Length(FLastSpans) > 0 then
    RenderSpans;
end;

procedure TBookDiffMemo.SetBaseFont(AFont: TFont);
begin
  if not Assigned(AFont) then
    Exit;
  if (Trim(AFont.Name) <> '') and not SameText(Trim(AFont.Name), 'default') then
    FBaseFontName := AFont.Name
  else
    FBaseFontName := UIDefaultFontName;
  if AFont.Size > 0 then
    FBaseFontSize := AFont.Size
  else
    FBaseFontSize := 10;
  TextStyle.Font.Name := FBaseFontName;
  TextStyle.Font.Size := FBaseFontSize;
end;

procedure TBookDiffMemo.SetFontSizeSynced(NewSize: Integer);
var
  Clamped, SavedTop: Integer;
begin
  Clamped := Max(7, Min(28, NewSize));
  if Clamped = FBaseFontSize then
    Exit;
  SavedTop := TopPos;
  FBaseFontSize := Clamped;
  TextStyle.Font.Size := FBaseFontSize;
  RenderSpans;
  TopPos := SavedTop;
  FLastTopPos := TopPos;
end;

procedure TBookDiffMemo.RenderSpans;
var
  I, SegStart, PosBreak: Integer;
  S, Seg: UnicodeString;
  NormalFg, DiffFg, DiffBg: TColor;

  procedure EmitSegment(const ASeg: UnicodeString; AChanged: Boolean);
  var
    TB: TKMemoTextBlock;
  begin
    if ASeg = '' then
      Exit;
    TB := Blocks.AddTextBlock(UTF8Encode(ASeg));
    TB.TextStyle.Font.Name := FBaseFontName;
    TB.TextStyle.Font.Size := FBaseFontSize;
    if AChanged then
    begin
      TB.TextStyle.Font.Style := [fsBold];
      TB.TextStyle.Font.Color := DiffFg;
      TB.TextStyle.AllowBrush := True;
      TB.TextStyle.Brush.Style := bsSolid;
      TB.TextStyle.Brush.Color := DiffBg;
    end
    else
    begin
      TB.TextStyle.Font.Style := [];
      TB.TextStyle.Font.Color := NormalFg;
      TB.TextStyle.AllowBrush := False;
      TB.TextStyle.Brush.Style := bsClear;
    end;
  end;

begin
  if CurrentSkin.Id = skClassic then
    NormalFg := UITextColor
  else
    NormalFg := CurrentSkin.WindowText;

  if CurrentSkin.IsDark then
  begin
    if FSide = bdsProject then
    begin
      DiffBg := MakeRGB(130, 36, 42);
      DiffFg := MakeRGB(255, 224, 224);
    end
    else
    begin
      DiffBg := MakeRGB(28, 108, 58);
      DiffFg := MakeRGB(220, 255, 228);
    end;
  end
  else
  begin
    if FSide = bdsProject then
    begin
      DiffBg := MakeRGB(255, 205, 210);
      DiffFg := MakeRGB(150, 16, 24);
    end
    else
    begin
      DiffBg := MakeRGB(200, 242, 208);
      DiffFg := MakeRGB(16, 110, 42);
    end;
  end;

  Blocks.LockUpdate;
  try
    Blocks.Clear;
    Blocks.DefaultParaStyle.FirstIndent := 0;
    Blocks.DefaultParaStyle.LeftPadding := 4;
    Blocks.DefaultParaStyle.TopPadding := 2;
    Blocks.DefaultParaStyle.BottomPadding := 2;
    Blocks.DefaultParaStyle.HAlign := halLeft;

    for I := 0 to High(FLastSpans) do
    begin
      S := FLastSpans[I].Text;
      SegStart := 1;
      while SegStart <= Length(S) do
      begin
        PosBreak := SegStart;
        while (PosBreak <= Length(S)) and (S[PosBreak] <> #10) do
          Inc(PosBreak);
        Seg := Copy(S, SegStart, PosBreak - SegStart);
        if Seg <> '' then
          EmitSegment(Seg, FLastSpans[I].Changed);
        if (PosBreak <= Length(S)) and (S[PosBreak] = #10) then
        begin
          if FLastSpans[I].Changed and (Seg = '') then
            EmitSegment(' ', True);
          Blocks.AddParagraph;
          SegStart := PosBreak + 1;
        end
        else
          Break;
      end;
    end;
    Modified := False;
  finally
    Blocks.UnLockUpdate;
    Invalidate;
  end;
end;

procedure TBookDiffMemo.SetPlainText(const AText: string);
var
  Normalized: string;
begin
  if AText = '' then
    FLastSpans := nil
  else
  begin
    Normalized := StringReplace(AText, #13#10, #10, [rfReplaceAll]);
    Normalized := StringReplace(Normalized, #13, #10, [rfReplaceAll]);
    SetLength(FLastSpans, 1);
    FLastSpans[0].Text := UTF8Decode(Normalized);
    FLastSpans[0].Changed := False;
  end;
  RenderSpans;
  TopPos := 0;
  FLastTopPos := 0;
end;

procedure TBookDiffMemo.SetDiffSpans(const ASpans: TDiffSpanArray);
begin
  FLastSpans := Copy(ASpans, 0, Length(ASpans));
  RenderSpans;
  TopPos := 0;
  FLastTopPos := 0;
end;

procedure TBookDiffMemo.NotifyScrollChanged;
begin
  if TopPos = FLastTopPos then
    Exit;
  FLastTopPos := TopPos;
  {$IFDEF WINDOWS}
  if HandleAllocated then
    PaintThemedNCScrollBars(Handle);
  {$ENDIF}
  if Assigned(FOnScrollSync) then
    FOnScrollSync(Self);
end;

procedure TBookDiffMemo.WndProc(var TheMessage: TLMessage);
var
  OldTop: Integer;
begin
  OldTop := TopPos;
  inherited WndProc(TheMessage);
  if TopPos <> OldTop then
    NotifyScrollChanged;
end;

function TBookDiffMemo.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if (not Result) and (WheelDelta <> 0) then
  begin
    HandleWheelInput(Shift, WheelDelta);
    Result := True;
  end;
end;

procedure TBookDiffMemo.HandleWheelInput(Shift: TShiftState;
  WheelDelta: Integer);
var
  NewSize, Step: Integer;
begin
  if WheelDelta = 0 then
    Exit;
  if ssCtrl in Shift then
  begin
    if WheelDelta > 0 then
      NewSize := Min(28, FBaseFontSize + 1)
    else
      NewSize := Max(7, FBaseFontSize - 1);
    if Assigned(FOnZoomChanged) then
      FOnZoomChanged(Self, NewSize)
    else
      SetFontSizeSynced(NewSize);
    Exit;
  end;

  Step := Max(UIScale(Self, 24),
    (Abs(WheelDelta) * UIScale(Self, 48) + 60) div 120);
  if WheelDelta > 0 then
    TopPos := Max(0, TopPos - Step)
  else
    TopPos := TopPos + Step;
  NotifyScrollChanged;
end;

procedure TBookDiffMemo.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta = 0 then
    Exit;
  HandleWheelInput(Shift, WheelDelta);
  Handled := True;
end;

procedure TBookDiffMemo.SyncTopPosFromPeer(ATopPos: Integer);
begin
  TopPos := ATopPos;
  FLastTopPos := TopPos;
  {$IFDEF WINDOWS}
  if HandleAllocated then
    PaintThemedNCScrollBars(Handle);
  {$ENDIF}
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
  FCurrentDiffCount := 0;
  FSyncingScroll := False;
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

  FCurrentFrame := TPanel.Create(Self);
  FCurrentFrame.Parent := Panes;
  FCurrentFrame.BevelOuter := bvNone;
  FCurrentFrame.BorderWidth := 1;
  FCurrentFrame.ParentColor := False;

  FNewFrame := TPanel.Create(Self);
  FNewFrame.Parent := Panes;
  FNewFrame.BevelOuter := bvNone;
  FNewFrame.BorderWidth := 1;
  FNewFrame.ParentColor := False;

  FCurrentMemo := TBookDiffMemo.CreateForSide(Self, bdsProject);
  FCurrentMemo.Parent := FCurrentFrame;
  FCurrentMemo.Align := alClient;
  FCurrentMemo.OnScrollSync := @SyncPreviewScroll;
  FCurrentMemo.OnZoomChanged := @SyncPreviewZoom;

  FNewMemo := TBookDiffMemo.CreateForSide(Self, bdsFile);
  FNewMemo.Parent := FNewFrame;
  FNewMemo.Align := alClient;
  FNewMemo.OnScrollSync := @SyncPreviewScroll;
  FNewMemo.OnZoomChanged := @SyncPreviewZoom;

  if Assigned(AFont) then
  begin
    FCurrentMemo.SetBaseFont(AFont);
    FNewMemo.SetBaseFont(AFont);
  end;

  FProjectTexts := TStringList.Create;
  FProjectValues := TStringList.Create;
  FBookNames := TStringList.Create;
  OnShow := @FormShow;
  OnMouseWheel := @FormMouseWheel;
  ApplyDialogStyle(Self);
  FCurrentFrame.Color := UIBorderColor;
  FNewFrame.Color := UIBorderColor;
  FCurrentMemo.ApplySkinColors;
  FNewMemo.ApplySkinColors;
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
  FCurrentFrame.SetBounds(0, TopY, W, H);
  FNewFrame.SetBounds(W + 10, TopY, W, H);
end;

procedure TFrmBookImport.FormShow(Sender: TObject);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Color := UIBorderColor;
  if Assigned(FNewFrame) then
    FNewFrame.Color := UIBorderColor;
  if Assigned(FCurrentMemo) then
    FCurrentMemo.ApplySkinColors;
  if Assigned(FNewMemo) then
    FNewMemo.ApplySkinColors;
end;

procedure TFrmBookImport.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  CurPos, LocalPos: TPoint;
begin
  if (WheelDelta = 0) or not Visible then
    Exit;
  CurPos := Point(0, 0);
  GetCursorPos(CurPos);
  if Assigned(FCurrentMemo) and FCurrentMemo.Visible then
  begin
    LocalPos := FCurrentMemo.ScreenToClient(CurPos);
    if (LocalPos.X >= 0) and (LocalPos.Y >= 0) and
       (LocalPos.X < FCurrentMemo.ClientWidth) and
       (LocalPos.Y < FCurrentMemo.ClientHeight) then
    begin
      FCurrentMemo.HandleWheelInput(Shift, WheelDelta);
      Handled := True;
      Exit;
    end;
  end;
  if Assigned(FNewMemo) and FNewMemo.Visible then
  begin
    LocalPos := FNewMemo.ScreenToClient(CurPos);
    if (LocalPos.X >= 0) and (LocalPos.Y >= 0) and
       (LocalPos.X < FNewMemo.ClientWidth) and
       (LocalPos.Y < FNewMemo.ClientHeight) then
    begin
      FNewMemo.HandleWheelInput(Shift, WheelDelta);
      Handled := True;
      Exit;
    end;
  end;
end;

procedure TFrmBookImport.SyncPreviewScroll(Sender: TObject);
var
  Src, Dst: TBookDiffMemo;
begin
  if FSyncingScroll or not Assigned(FCurrentMemo) or not Assigned(FNewMemo) then
    Exit;
  if Sender = FCurrentMemo then
  begin
    Src := FCurrentMemo;
    Dst := FNewMemo;
  end
  else if Sender = FNewMemo then
  begin
    Src := FNewMemo;
    Dst := FCurrentMemo;
  end
  else
    Exit;
  FSyncingScroll := True;
  try
    Dst.SyncTopPosFromPeer(Src.TopPos);
  finally
    FSyncingScroll := False;
  end;
end;

procedure TFrmBookImport.SyncPreviewZoom(Sender: TObject; NewSize: Integer);
var
  Src, Dst: TBookDiffMemo;
begin
  if FSyncingScroll or not Assigned(FCurrentMemo) or not Assigned(FNewMemo) then
    Exit;
  if Sender = FCurrentMemo then
  begin
    Src := FCurrentMemo;
    Dst := FNewMemo;
  end
  else
  begin
    Src := FNewMemo;
    Dst := FCurrentMemo;
  end;
  FSyncingScroll := True;
  try
    FCurrentMemo.SetFontSizeSynced(NewSize);
    FNewMemo.SetFontSizeSynced(NewSize);
    Dst.SyncTopPosFromPeer(Src.TopPos);
  finally
    FSyncingScroll := False;
  end;
end;

function TFrmBookImport.HasProjectText(const Ref: string;
  out AText: string): Boolean;
var
  Index: Integer;
begin
  Index := FProjectTexts.IndexOf(Ref);
  Result := Index >= 0;
  if Result then
    AText := FProjectValues[PtrInt(FProjectTexts.Objects[Index])]
  else
    AText := '';
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
  RefStr, HeaderText: string;
begin
  if (FCurrentIndex < 0) or (FCurrentIndex > High(FEntries)) then
  begin
    FVerseLabel.Caption := SBookImportPreviewNone;
    Exit;
  end;
  RefStr := FormatEntryReference(FCurrentIndex);
  if FTree.IsVerseChecked(FCurrentIndex) then
    HeaderText := Format(SBookImportPreviewVerse, [RefStr])
  else
    HeaderText := Format(SBookImportPreviewVerseUnchecked, [RefStr]);
  if FCurrentDiffCount = 1 then
    HeaderText := Format(SBookImportDiffOne, [HeaderText])
  else if FCurrentDiffCount > 1 then
    HeaderText := Format(SBookImportDiffMany, [HeaderText, FCurrentDiffCount]);
  FVerseLabel.Caption := HeaderText;
end;

procedure TFrmBookImport.ClearPreview;
begin
  FCurrentIndex := -1;
  FCurrentDiffCount := 0;
  FVerseLabel.Caption := SBookImportPreviewNone;
  FSyncingScroll := True;
  try
    FCurrentMemo.SetPlainText('');
    FNewMemo.SetPlainText('');
  finally
    FSyncingScroll := False;
  end;
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
var
  ProjText: string;
  OldSpans, NewSpans: TDiffSpanArray;
begin
  if (Index < 0) or (Index > High(FEntries)) then
    Exit;
  if Index <> FCurrentIndex then
  begin
    FCurrentIndex := Index;
    if HasProjectText(FEntries[Index].Ref, ProjText) then
    begin
      BuildVerseDiffSpans(ProjText, FEntries[Index].Text,
        OldSpans, NewSpans, FCurrentDiffCount);
      FSyncingScroll := True;
      try
        FCurrentMemo.SetDiffSpans(OldSpans);
        FNewMemo.SetDiffSpans(NewSpans);
      finally
        FSyncingScroll := False;
      end;
    end
    else
    begin
      FCurrentDiffCount := 0;
      FSyncingScroll := True;
      try
        FCurrentMemo.SetPlainText(SBookImportNotInProject);
        FNewMemo.SetPlainText(FEntries[Index].Text);
      finally
        FSyncingScroll := False;
      end;
    end;
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
