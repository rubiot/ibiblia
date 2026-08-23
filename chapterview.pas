unit ChapterView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, ONTTokenizer, LCLType, HTMLColors,
  LazUTF8, BibleScope, Project, Verse, Syntagm, PCRE, Controls, Menus, fgl, Dialogs, Math, Contnrs, KMemo,
  KEditCommon, KGraphics, KDialogs, FormInterlinearVerseRules,
  LCLTranslator, LazLogger, HTMLRenderer, SynEdit,
  theword_syntax_highlighter, bible_book_names, translation_memory_model,
  translation_memory_focus_controller,
  SuggestionManager, rf_note_scanner,
  theword_translator_note_preview, theword_effective_inline_style_v2, theword_translator_note_style;

type

  TBlockRange = record
    first, last: integer;
  end;

  TChapterView = class;

  { TRawChapterMemo }

  TRawChapterMemo = class(TSynEdit)
  private
    FChapterView: TChapterView;
    procedure HandleOnKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure HandleOnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent; ChapterView: TChapterView); overload;
    destructor Destroy; override;

    procedure Show(aText: string);
    procedure Hide;
  end;

  { TNoteInfo }

  TNoteInfo = class
  private
    FId: integer;
    FLinkText: string;
    FVerse: integer;
    FText: string;
    FRange: TBlockRange;
  public
    constructor Create(id: integer; verse: integer; text: string; linktext: string);
    //destructor Destroy; override;
    property Id: integer read FId write FId;
    property LinkText: string read FLinkText write FLinkText;
    property Verse: integer read FVerse write FVerse;
    property Text: string read FText write FText;
    property Range: TBlockRange read FRange write FRange;
    property FirstBlock: integer read FRange.first write FRange.first;
    property LastBlock: integer read FRange.last write FRange.last;
  end;

  { TNoteWindow }

  TNoteWindow = class(THintWindow)
  private
    FNoteView: TTranslatorNoteBodyView;
    function GetScrollBarsVisible: boolean;
    procedure InitNoteView;
    procedure SetNoteText(const AText: string; const AStyle: TKMemoTextStyle);
    procedure HandlePopupMouseEnter(Sender: TObject);
    procedure HandlePopupMouseLeave(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(const AHint: string; AStyle: TKMemoTextStyle); overload;
    property ScrollBarsVisible: boolean read GetScrollBarsVisible;
  end;

  TNoteList = specialize TFPGList<TNoteInfo>;

  { TChapterView }

  TChapterView = class(THTMLRenderer)
  private
    FFontName: string;
    FFontSize: integer;
    FPrintMode: boolean;
    FProject: TProject;
    FRxVerseHeading: IRegex;
    FNoteID: integer;
    FNoteNumberer: TRFNoteNumberer;
    FHint: TNoteWindow;
    FSetFontItem: TMenuItem;
    FParagraphModeItem: TMenuItem;
    FVerseRulesItem: TMenuItem;
    FInterlinearModeItem: TMenuItem;
    FInterlinearModeInterItem: TMenuItem;
    FInterlinearModeIntraItem: TMenuItem;
    FPrintChapterItem: TMenuItem;
    FVerseRanges: array of TBlockRange;
    FCurrentVerse: integer;
    FNotes: TNoteList;
    FHideVerseNumber: boolean;
    FRawMemo: TRawChapterMemo;
    FViewMode: TChapterViewMode;
    FTranslationMemoryOrder: TTranslationMemoryOrder;
    FTranslationMemoryEntries: TTranslationMemoryEntryArray;
    FTranslationMemoryFocusEntries: TTranslationMemoryFocusEntryArray;
    FTranslationMemoryFocus: TTranslationMemoryFocusController;
    FTranslationMemoryPairsSnapshot: string;
    FTranslationMemoryReferenceSnapshot: string;
    FTranslationMemoryShowingOccurrences: Boolean;
    FTranslationMemoryOccurrenceMatches: TTranslationPairMatchArray;
    FTranslationMemoryOccurrenceSource: string;
    FTranslationMemoryOccurrenceTranslation: string;
    FTranslationMemoryOccurrenceLimit: Integer;
    FTranslationMemoryRefreshPending: Boolean;
    FViewSelectionActive: Boolean;

    function GetInterlinearMode: TInterlinearMode;
    function GetParagraphMode: TParagraphMode;
    function GetTextType: TTipoTextoBiblico;
    function IsTranslationMemoryVisible: Boolean;
    procedure RenderChapter(verses: TStringList; current: integer);
    procedure RenderChapterHeader;
    procedure RenderChapterFooter;
    procedure RenderVerse(txt: string);
    procedure RenderVerseNumber(number: integer);
    procedure RenderSpan(cont: TKMemoBlocks; txt: string); override;
    procedure RenderNote(const OpeningTag, Note: string);
    procedure RenderNotes;
    procedure RenderInterlinearBlock(cont: TKMemoBlocks; blockText: string);
    procedure RenderInterlinearUnit(cont: TKMemoBlocks; translation: string);
    procedure HandleReferenceClick(sender: TObject);
    procedure HandleNoteLinkClick(sender: TObject);
    procedure HandleNoteClick(sender: TObject);
    procedure HandleTranslationMemoryEntryClick(Sender: TObject);
    procedure HandleTranslationMemoryAlternativeClick(Sender: TObject);
    procedure HandleTranslationMemoryBackClick(Sender: TObject);
    procedure HandleTranslationMemoryOccurrenceClick(Sender: TObject);
    procedure HandleTranslationMemoryShowMoreClick(Sender: TObject);
    procedure HandleVerseChange(Sender: TProject);
    procedure HandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandlePopupKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HandleDoubleClick(Sender: TObject);
    procedure HandleContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure HandleSetFont(Sender: TObject);
    procedure HandlePrintChapter(Sender: TObject);
    procedure HandleParagraphMode(Sender: TObject);
    procedure HandleVerseRules(Sender: TObject);
    procedure HandleInterlinearMode(Sender: TObject);
    procedure HandleIntralinearMode(Sender: TObject);
    procedure SetInterlinearMode(AValue: TInterlinearMode);
    procedure SetPrintMode(AValue: boolean);
    procedure SetProject(AValue: TProject);
    procedure SetViewMode(AValue: TChapterViewMode);
    procedure SetTranslationMemoryOrder(AValue: TTranslationMemoryOrder);
    procedure InitPopupMenu;
    procedure SetParagraphMode(AValue: TParagraphMode);
    procedure SetFontSize(AValue: integer);
    procedure SetFontName(AValue: string);
    procedure HighlightRange(range: TBlockRange; bgcolor: TColor);
    function GetPreviousParagraphBlock: TKMemoParagraph;
    procedure SetTextType(AValue: TTipoTextoBiblico);
    procedure RenderTranslationMemory;
    procedure RenderTranslationMemoryOccurrences;
    procedure ResetTranslationMemoryOccurrences;
    procedure ClearTranslationMemorySourceHighlight;
    procedure ClearTranslationMemoryEntryFocus;
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure SetupDefaultStyle; override;
    procedure ApplyProjectedInlineStyle(ABlock: TKMemoTextBlock;
      ABaseStyle: TKMemoTextStyle;
      const AEffective: TTheWordEffectiveInlineStyleV2); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Translate;
    procedure LoadChapter;
    procedure RefreshTranslationMemory;
    procedure FocusTranslationMemoryForSyntagm(ASyntagm: TSyntagm);
    property Project: TProject read FProject write SetProject;
    property TextType: TTipoTextoBiblico read GetTextType write SetTextType;
    property ViewMode: TChapterViewMode read FViewMode write SetViewMode;
    property TranslationMemoryOrder: TTranslationMemoryOrder
      read FTranslationMemoryOrder write SetTranslationMemoryOrder;
    property ParagraphMode: TParagraphMode read GetParagraphMode write SetParagraphMode;
    property InterlinearMode: TInterlinearMode read GetInterlinearMode write SetInterlinearMode;
    property FontSize: integer read FFontSize write SetFontSize;
    property FontName: string read FFontName write SetFontName;
    property CurrentStyle: TKMemoTextStyle read GetCurrentStyle;
    property HideVerseNumber: boolean read FHideVerseNumber write FHideVerseNumber default false;
    property PrintMode: boolean read FPrintMode write SetPrintMode;
  end;

const
  NonBibleTextColor = $be7c41;
  NoteLinkColor = TheWordTranslatorNoteMarkerColor;
  ChapterNumberColor = $2cba7d;
  TranslationMemoryCurrentColor = clBlue;
  DefaultFirstIndent = 20;

resourcestring
  SSetFont = 'Choose &font...';
  SParagraphMode = '&Paragraph mode';
  SPrintChapter = 'Print chapter';
  SVerseRules = 'Verse &rules...';
  SInterlinearMode = 'Interlinear layout...';
  SInterlinearModeInter = 'Interlinear';
  SInterlinearModeIntra = 'Intralinear';
  SPreviousChapter = 'Previous chapter';
  SNextChapter = 'Next chapter';
  SChapterBeginning = 'Beginning of chapter';
  STranslationMemoryTitle = 'Translation memory';
  STranslationMemoryFormsFmt = 'Forms: %s';
  STranslationMemoryStrongsFmt = 'Strong''s: %s';
  STranslationMemoryCurrentLegend =
    'Blue translations are used in the current verse.';
  STranslationMemoryNoTranslations = 'No translation recorded';
  STranslationMemoryNoSourceWords =
    'No source words are available for this verse.';
  STranslationMemoryErrorFmt = 'Translation memory is unavailable: %s';
  STranslationMemoryBack = 'Back to translation memory';
  STranslationMemoryOccurrencesTitleFmt = '%s → %s';
  STranslationMemoryManyAssociationsOneVerseFmt =
    '%d associations in 1 verse';
  STranslationMemoryOneAssociationManyVersesFmt =
    '1 association in %d verses';
  STranslationMemoryOneAssociationOneVerse =
    '1 association in 1 verse';
  STranslationMemoryOccurrenceTotalsFmt =
    '%d associations in %d verses';
  STranslationMemoryOccurrenceCountFmt = '%d occurrences';
  STranslationMemoryNoOccurrences =
    'No verse currently uses this translation.';
  STranslationMemoryShowMoreFmt = 'Show 100 more (%d remaining)';

implementation

function TranslationMemoryOccurrenceTotalsText(AssociationCount,
  VerseCount: Integer): string;
begin
  if AssociationCount = 1 then
  begin
    if VerseCount = 1 then
      Result := STranslationMemoryOneAssociationOneVerse
    else
      Result := Format(STranslationMemoryOneAssociationManyVersesFmt,
        [VerseCount]);
  end
  else if VerseCount = 1 then
    Result := Format(STranslationMemoryManyAssociationsOneVerseFmt,
      [AssociationCount])
  else
    Result := Format(STranslationMemoryOccurrenceTotalsFmt,
      [AssociationCount, VerseCount]);
end;

function JoinMemoryValues(const Values: TTranslationMemoryStringArray;
  const Separator: string): string;
var
  Value: string;
begin
  Result := '';
  for Value in Values do
  begin
    if Result <> '' then
      Result := Result + Separator;
    Result := Result + Value;
  end;
end;

{ TRawChapterMemo }

procedure TRawChapterMemo.HandleOnKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Hide;
end;

procedure TRawChapterMemo.HandleOnMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if ssCtrl in Shift then
    Font.Size := Font.Size + Sign(WheelDelta) * 1;
end;

constructor TRawChapterMemo.Create(AOwner: TComponent; ChapterView: TChapterView
  );
begin
  inherited Create(nil);

  Visible    := False;
  Parent     := TWinControl(AOwner);
  FChapterView := ChapterView;
  Align      := alClient;
  ConfigureTheWordSyntaxEditor(Self, True);
  OnKeyDown  := @HandleOnKeyDown;
  OnMouseWheel := @HandleOnMouseWheel;
end;

destructor TRawChapterMemo.Destroy;
begin
  inherited Destroy;
end;

procedure TRawChapterMemo.Show(aText: string);
begin
  ApplyTheWordSyntaxEditorFont(Self);
  Text := aText;
  BringToFront;
  Visible := True;
  SetFocus;
end;

procedure TRawChapterMemo.Hide;
begin
  Visible := false;
  //FChapterView.FontSize := Font.Size;
end;

{ TNoteWindow }

procedure TNoteWindow.InitNoteView;
begin
  InsertControl(TTranslatorNoteBodyView.Create(nil));
  FNoteView := Controls[0] as TTranslatorNoteBodyView;
  with FNoteView do
  begin
    OnMouseEnter:= @HandlePopupMouseEnter;
    OnMouseLeave:= @HandlePopupMouseLeave;
    Align       := alClient;
    BorderStyle := bsNone;
  end;
end;

function TNoteWindow.GetScrollBarsVisible: boolean;
begin
  result := ((FNoteView.Height - FNoteView.ClientRect.Bottom) > 10) or
            ((FNoteView.Width - FNoteView.ClientRect.Right) > 10) or
            (FNoteView.ClientRect.Height = 10);
end;

procedure TNoteWindow.SetNoteText(const AText: string;
  const AStyle: TKMemoTextStyle);
begin
  FNoteView.Blocks.LockUpdate;
  Width := 500;
  FNoteView.Clear(false);
  FNoteView.TextStyle.Font.Name := AStyle.Font.Name;
  FNoteView.TextStyle.Font.Size := AStyle.Font.Size - 1;
  FNoteView.SetNoteBody(UTF8Decode(AText));
  FNoteView.Background.Color := $00CCFBFB;
  FNoteView.Blocks.UnLockUpdate;
end;

procedure TNoteWindow.HandlePopupMouseEnter(Sender: TObject);
begin
  AutoHide := false;
end;

procedure TNoteWindow.HandlePopupMouseLeave(Sender: TObject);
begin
  Hide;
end;

constructor TNoteWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Color := clWindow;
  FNoteView:= nil;
end;

destructor TNoteWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TNoteWindow.ActivateHint(const AHint: string; AStyle: TKMemoTextStyle);
var
  Rect: TRect;
  Pos: TPoint;
begin
  if not Assigned(FNoteView) then
    InitNoteView;

  AutoHide := true;

  SetNoteText(AHint, AStyle);

  Rect := FNoteView.ContentRect;

  Pos := Mouse.CursorPos;
  Rect.Left := Pos.X+10;
  Rect.Top := Pos.Y+5;
  Rect.Right := Rect.Left + Rect.Right + 5;
  Rect.Bottom := Rect.Top + Rect.Bottom + 5;
  ActivateHint(Rect, '');
end;

{ TNoteInfo }

constructor TNoteInfo.Create(id: integer; verse: integer; text: string;
  linktext: string);
begin
  FId    := id;
  FVerse := verse;
  FText  := text;
  FLinkText := linktext;
end;

{ TChapterView }

procedure TChapterView.RenderVerse(txt: string);
var
  mtHeading: IMatch;
  heading: string;
begin
  ResetStyleStack;
  mtHeading := FRxVerseHeading.Match(txt, [rmoNoUTF8Check]);
  if not mtHeading.Success then
    raise Exception.Create(SysUtils.Format('There''s something wrong with this verse: "%s"'#13#10 +
                                           'Please check for invalid tags or line breaks', [txt]));
  heading := mtHeading.Groups[1].Value;
  txt     := mtHeading.Groups[2].Value;

  { rendering titles before the verse number }
  if not heading.IsEmpty then
    RenderSpan(Blocks, heading);

  if Blocks.LastBlock.ClassName <> 'TKMemoParagraph' then
    Blocks.AddTextBlock(' '); // space between verses

  { rendering verse number }
  RenderVerseNumber(FCurrentVerse);

  { rendering the verse text }
  FVerseRanges[FCurrentVerse].first := Blocks.Count;
  RenderSpan(Blocks, txt);
  FVerseRanges[FCurrentVerse].last := Blocks.Count-1;

  if FProject.ParagraphMode = pmNoParagraphs then
    Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
end;

procedure TChapterView.RenderVerseNumber(number: integer);
var
  b: TKMemoBlocks;
begin
  if FHideVerseNumber then exit;

  if (FProject.ChapterViewText = tbInterlinear) and (FProject.InterlinearMode = imInterlinear) then
    with Blocks.AddContainer do
    begin
      Position := mbpText;
      FixedWidth := true;
      //BlockStyle.BottomPadding := 5;
      BlockStyle.RightPadding := 5;
      b := Blocks;
    end
  else
    b := Blocks;

  with b.AddHyperlink(Format('%d ', [number]), Format('%d,%d,%d', [FProject.BookID, FProject.Chapter, number])) do
  begin
    OnClick := @HandleReferenceClick;
    TextStyle.Font.Name  := 'default';
    TextStyle.Font.Size  := FFontSize-2;
    TextStyle.Font.Style := [fsBold];
    TextStyle.Font.Color := NonBibleTextColor;
  end;
end;

function TChapterView.GetInterlinearMode: TInterlinearMode;
begin
  if Assigned(FProject) then
    result := FProject.InterlinearMode
  else
    result := imIntralinear;
end;

function TChapterView.GetParagraphMode: TParagraphMode;
begin
  if Assigned(FProject) then
    result := FProject.ParagraphMode
  else
    result := pmNoParagraphs;
end;

function TChapterView.GetTextType: TTipoTextoBiblico;
begin
  result := FProject.ChapterViewText;
end;

function TChapterView.IsTranslationMemoryVisible: Boolean;
begin
  Result := FViewSelectionActive and Enabled and Assigned(FProject) and
    (FViewMode = cvmTranslationMemory);
end;

procedure TChapterView.ApplyProjectedInlineStyle(ABlock: TKMemoTextBlock;
  ABaseStyle: TKMemoTextStyle;
  const AEffective: TTheWordEffectiveInlineStyleV2);
begin
  inherited ApplyProjectedInlineStyle(ABlock, ABaseStyle, AEffective);
  if (tweifv2AddedWords in AEffective.Flags) and
    (not AEffective.HasExplicitColor) then
    ABlock.TextStyle.Font.Color := clGray;
end;

procedure TChapterView.RenderSpan(cont: TKMemoBlocks; txt: string);
var
  FTokenizer: TONTTokenizer;
  token: TSyntagmToken;
  chunk: string;
  OpeningTag, ConsumedBody, ConsumedClose: string;
  NoteBody: string;
  NoteClosingTag: string;
  padding: longint;
  lastpar: TKMemoParagraph;
  Projection: TTheWordEffectiveInlineProjectionV2;
  VisibleOffsetUTF16, RunIndex: Integer;
begin
  chunk := '';
  FTokenizer := nil;
  Projection := nil;
  VisibleOffsetUTF16 := 0;
  RunIndex := 0;
  if Assigned(FProject) then
    TokenizerVersion := FProject.TokenizerVersion;
  try
    FTokenizer := TTokenizerFactory.CreateTokenizer(TokenizerVersion, txt);
    TryResolveInlineProjection(UTF8Decode(txt), Projection);

    cont.LockUpdate;
    try
      while FTokenizer.ReadSyntagm(token) <> skNull do
    begin
      case token.Kind of
        skMetadata:
          ;
        skWhitespace:
          if token.Value <> '|' then
            chunk := chunk + token.Value;
        skPunctuation, skSyntagm:
          chunk := chunk + token.Value;
        skTag:
        begin
          if chunk.Length > 0 then
          begin
            RenderProjectedChunk(cont, chunk, Projection,
              VisibleOffsetUTF16, RunIndex);
            chunk := '';
          end;

          OpeningTag := token.Value;
          if OpeningTag.StartsWith('<TS') then { title beginning }
          begin
            if assigned(cont.LastBlock) and
              (cont.LastBlock.ClassName <> 'TKMemoParagraph') then
              cont.AddParagraph().ParaStyle.FirstIndent :=
                IfThen(GetParagraphMode = pmParagraph, DefaultFirstIndent);
            with PushNewStyle do
              case OpeningTag[4] of
                '>','0':
                begin
                  Font.Size := Font.Size + 2;
                  Font.Style := [fsBold];
                  Font.Color := $424242;
                end;
                '1':
                begin
                  Font.Size := Font.Size + 1;
                  Font.Style := [fsBold, fsItalic];
                  Font.Color := clDefault;
                end;
                '2':
                begin
                  Font.Style := [fsBold, fsItalic];
                  Font.Color := clDefault;
                end;
                '3':
                begin
                  Font.Style := [fsItalic];
                  Font.Color := clDefault;
                end;
              end;
          end
          else if OpeningTag = '<Ts>' then { title end }
          begin
            ResetStyleStack;
            lastpar := GetPreviousParagraphBlock;
            padding := IfThen(assigned(lastpar) and
              (lastpar.ParaStyle.BottomPadding = 0), 5);
            cont.AddParagraph().ParaStyle.TopPadding := padding;
          end
          else if IsRFOpeningTag(OpeningTag) then { translator note }
          begin
            ReadRFNoteBody(FTokenizer, NoteBody, NoteClosingTag);
            RenderNote(OpeningTag, NoteBody);
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag + NoteBody +
              NoteClosingTag), Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<Q>' then
          begin
            ConsumedBody := FTokenizer.ReadUntilExclusive('<q>');
            RenderInterlinearBlock(cont, ConsumedBody);
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag +
              ConsumedBody + '<q>'), Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<E>' then
          begin
            ConsumedBody := FTokenizer.ReadUntilExclusive('<e>');
            RenderInterlinearUnit(cont, ConsumedBody);
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag +
              ConsumedBody + '<e>'), Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<T>' then
          begin
            ConsumedBody := FTokenizer.ReadUntilExclusive('<t>');
            RenderInterlinearUnit(cont, ConsumedBody);
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag +
              ConsumedBody + '<t>'), Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<X>' then
          begin
            ConsumedBody := FTokenizer.ReadUntilExclusive('<x>');
            RenderInterlinearUnit(cont, ConsumedBody);
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag +
              ConsumedBody + '<x>'), Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<H>' then
          begin
            ConsumedBody := FTokenizer.ReadUntilExclusive('<h>');
            RenderInterlinearUnit(cont, ConsumedBody);
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag +
              ConsumedBody + '<h>'), Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<G>' then
          begin
            ConsumedBody := FTokenizer.ReadUntilExclusive('<g>');
            RenderInterlinearUnit(cont, ConsumedBody);
            AdvanceProjectionForConsumedRaw(UTF8Decode(OpeningTag +
              ConsumedBody + '<g>'), Projection, VisibleOffsetUTF16, RunIndex);
          end
          else if OpeningTag = '<CM>' then
          begin
            if GetParagraphMode = pmParagraph then
              cont.AddParagraph().ParaStyle.FirstIndent := DefaultFirstIndent;
          end
          else if OpeningTag = '<CL>' then
          begin
            if GetParagraphMode = pmParagraph then
              cont.AddParagraph().ParaStyle.FirstIndent := 0;
          end
          else if LowerCase(OpeningTag) = '<br/>' then
            cont.AddParagraph().ParaStyle.FirstIndent := 0
          else
            ConsumedClose := ''; { inline/metadata tags are projected or hidden }
        end;
      end;
    end;

    if chunk.Length > 0 then
    begin
      if assigned(cont.LastBlock) and
        (cont.LastBlock.ClassName = 'TKMemoParagraph') then
        cont.LastBlock.ParaStyle.FirstIndent := DefaultFirstIndent;
      RenderProjectedChunk(cont, chunk, Projection,
        VisibleOffsetUTF16, RunIndex);
    end;
    finally
      cont.UnLockUpdate;
    end;
  finally
    FreeAndNil(FTokenizer);
    FreeAndNil(Projection);
  end;
end;

procedure TChapterView.RenderNote(const OpeningTag, Note: string);
var
  Link: UnicodeString;
begin
  if not FNoteNumberer.ResolveMarker(UnicodeString(OpeningTag), Link) then
    Exit;
  if Link = '' then
    Exit;

  FNotes.Add(TNoteInfo.Create(FNoteID, FCurrentVerse, Note, string(Link)));
  if assigned(Blocks.LastBlock) and not Blocks.LastBlock.Text.EndsWith(' ') then
    Link := ' ' + Link;

  with Blocks.AddHyperlink(string(Link), (FNoteID-1).ToString) do
  begin
    TextStyle.Assign(TextStyle);
    TextStyle.Font.Color := NoteLinkColor;
    TextStyle.Font.Style := [fsItalic];
    TextStyle.Font.Size := TextStyle.Font.Size + 1;
    TextStyle.ScriptPosition := tpoSuperscript;
    OnClick := @HandleNoteLinkClick;
  end;
  Inc(FNoteID);
end;

procedure TChapterView.RenderNotes;
var
  note: TNoteInfo;
begin
  if FNotes.Count = 0 then
    exit;

  ResetStyleStack;
  CurrentStyle.Font.Size := max(FFontSize-2, 2); // using smaller font

  Blocks.AddParagraph();
  Blocks.AddParagraph();

  for note in FNotes do
  begin
    with Blocks.AddHyperlink(SysUtils.Format('[%s] ', [note.LinkText]), (note.Id-1).ToString) do
    begin
      TextStyle.Font.Color := NoteLinkColor;
      TextStyle.Font.Style := [fsBold];
      TextStyle.Font.Size  := Max(FFontSize - 3, 2);
      OnClick := @HandleNoteClick;
    end;
    note.FirstBlock := Blocks.Count-1;
    RenderSpan(Blocks, note.Text);
    note.LastBlock := Blocks.Count-1;
    Blocks.AddParagraph();
  end;
end;

procedure TChapterView.RenderInterlinearBlock(cont: TKMemoBlocks;
  blockText: string);
var
  b: TKMemoContainer;
begin
  b := cont.AddContainer();
  b.Position := mbpText;
  b.FixedWidth := true;
  //b.BlockStyle.BottomPadding := 5;
  b.BlockStyle.RightPadding := 5;

  // using non-breakable spaces inside interlinear blocks
  RenderSpan(b.Blocks, stringReplace(blockText, ' ', #8239, [rfReplaceAll]));
end;

procedure TChapterView.RenderInterlinearUnit(cont: TKMemoBlocks;
  translation: string);
begin
  if cont.Count > 0 then
  begin
    //cont.LastBlock.ParaStyle.HAlign := halCenter;
    cont.AddParagraph();
  end;
  RenderSpan(cont, '<font color="#417cbe">' + translation + '</font>');
  //cont.AddTextBlock(translation).TextStyle.Font.Color := $be7c41;
end;

procedure TChapterView.HandleReferenceClick(sender: TObject);
begin
  FProject.GoToReference(TKMemoHyperlink(Sender).URL);
end;

procedure TChapterView.HandleNoteLinkClick(sender: TObject);
var
  note: TNoteInfo;
  noteIndex: integer;
  c: integer = 0;
begin
  if not (sender is TKMemoHyperlink) then
    Exit;
  noteIndex := StrToIntDef(string(TKMemoHyperlink(sender).URL), -1);
  if (noteIndex < 0) or (noteIndex >= FNotes.Count) then
    Exit;
  note := FNotes[noteIndex];
  repeat { working around TKMemo.ClientRect, which always gives wrong results the first time }
    Inc(c);
    FHint.ActivateHint(note.Text, TextStyle);
  until not FHint.ScrollBarsVisible or (c = 2);
end;

procedure TChapterView.HandleNoteClick(sender: TObject);
var
  note: TNoteInfo;
  noteIndex: integer;
begin
  if not (sender is TKMemoHyperlink) then
    Exit;
  noteIndex := StrToIntDef(string(TKMemoHyperlink(sender).URL), -1);
  if (noteIndex < 0) or (noteIndex >= FNotes.Count) then
    Exit;
  note := FNotes[noteIndex];

  HighlightRange(note.Range, clDefault); // un-highlight current note text
  HighlightRange(FVerseRanges[note.Verse], clSilver); // un-highlight current note text
end;

procedure TChapterView.HandleTranslationMemoryEntryClick(Sender: TObject);
var
  EntryIndex: Integer;
  SyntagmIndex: Integer;
  SourceVerse: TVerse;
begin
  if not Assigned(FProject) or
     not (Sender is TKMemoHyperlink) then
    Exit;
  EntryIndex := StrToIntDef(TKMemoHyperlink(Sender).URL, -1);
  if (EntryIndex < 0) or
     (EntryIndex >= Length(FTranslationMemoryEntries)) then
    Exit;

  ClearTranslationMemorySourceHighlight;
  FTranslationMemoryFocus.FocusEntry(EntryIndex);
  SourceVerse := FProject.VerseObjects[tbOrigem];
  if not Assigned(SourceVerse) or not Assigned(SourceVerse.Syntagms) then
    Exit;
  for SyntagmIndex in
    FTranslationMemoryEntries[EntryIndex].SyntagmIndexes do
    if (SyntagmIndex >= 0) and
       (SyntagmIndex < SourceVerse.Syntagms.Count) then
      SourceVerse.Syntagms[SyntagmIndex].ToggleStrongHighlight(True);
end;

procedure TChapterView.HandleTranslationMemoryAlternativeClick(
  Sender: TObject);
var
  AlternativeIndex: Integer;
  EntryIndex: Integer;
  SeparatorIndex: Integer;
  URL: string;
begin
  if not (Sender is TKMemoHyperlink) then
    Exit;
  URL := TKMemoHyperlink(Sender).URL;
  SeparatorIndex := Pos(':', URL);
  if SeparatorIndex <= 1 then
    Exit;
  EntryIndex := StrToIntDef(Copy(URL, 1, SeparatorIndex - 1), -1);
  AlternativeIndex := StrToIntDef(Copy(URL, SeparatorIndex + 1, MaxInt), -1);
  if (EntryIndex < 0) or
     (EntryIndex >= Length(FTranslationMemoryEntries)) or
     (AlternativeIndex < 0) or
     (AlternativeIndex >= Length(
       FTranslationMemoryEntries[EntryIndex].Alternatives)) then
    Exit;
  with FTranslationMemoryEntries[EntryIndex].Alternatives[AlternativeIndex] do
  begin
    if (Occurrences <= 0) or (Length(Matches) = 0) then
      Exit;
    FTranslationMemoryOccurrenceTranslation := Translation;
    FTranslationMemoryOccurrenceMatches := Copy(Matches, 0, Length(Matches));
  end;
  FTranslationMemoryOccurrenceSource :=
    FTranslationMemoryEntries[EntryIndex].DisplayTerm;
  FTranslationMemoryOccurrenceLimit := 100;
  FTranslationMemoryShowingOccurrences := True;
  ClearTranslationMemorySourceHighlight;
  RenderTranslationMemoryOccurrences;
  TopPos := 0;
end;

procedure TChapterView.HandleTranslationMemoryBackClick(Sender: TObject);
begin
  ResetTranslationMemoryOccurrences;
  RenderTranslationMemory;
  TopPos := 0;
end;

procedure TChapterView.HandleTranslationMemoryOccurrenceClick(
  Sender: TObject);
var
  ReferenceID: string;
begin
  if not Assigned(FProject) or not (Sender is TKMemoHyperlink) then
    Exit;
  ReferenceID := TKMemoHyperlink(Sender).URL;
  FProject.GoToReference(ReferenceID);
end;

procedure TChapterView.HandleTranslationMemoryShowMoreClick(Sender: TObject);
var
  PreviousTop: Integer;
begin
  PreviousTop := TopPos;
  Inc(FTranslationMemoryOccurrenceLimit, 100);
  RenderTranslationMemoryOccurrences;
  TopPos := PreviousTop;
end;

procedure TChapterView.ResetTranslationMemoryOccurrences;
begin
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  FTranslationMemoryShowingOccurrences := False;
  SetLength(FTranslationMemoryOccurrenceMatches, 0);
  FTranslationMemoryOccurrenceSource := '';
  FTranslationMemoryOccurrenceTranslation := '';
  FTranslationMemoryOccurrenceLimit := 100;
end;

procedure TChapterView.HandleVerseChange(Sender: TProject);
var
  verses: TStringList;
  ReferenceChanged: Boolean;
begin
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  if not assigned(FProject) or (Width < 2) then
    exit;

  ClearTranslationMemorySourceHighlight;
  // The same notification also fires after saving or editing the current
  // verse. Leave occurrence drill-down only when navigation really occurred.
  ReferenceChanged := FTranslationMemoryReferenceSnapshot <> FProject.ID;
  FTranslationMemoryReferenceSnapshot := FProject.ID;
  if ReferenceChanged then
    ResetTranslationMemoryOccurrences;
  if FViewMode = cvmTranslationMemory then
  begin
    FTranslationMemoryRefreshPending := True;
    if IsTranslationMemoryVisible then
      if FTranslationMemoryShowingOccurrences then
        RenderTranslationMemoryOccurrences
      else
      begin
        RenderTranslationMemory;
        TopPos := 0;
      end;
    Exit;
  end;

  FTranslationMemoryRefreshPending := True;
  verses := FProject.GetChapterText;
  try
    RenderChapter(verses, Sender.Verse);
  finally
    FreeAndNil(verses);
  end;
end;

procedure TChapterView.HandleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key in [VK_SUBTRACT, VK_OEM_MINUS]) and (FFontSize > 1) then
  begin
    FFontSize := FFontSize-1;
    LoadChapter;
  end
  else if (ssCtrl in Shift) and (Key in [VK_ADD, VK_OEM_PLUS]) then
  begin
    FFontSize := FFontSize+1;
    LoadChapter;
  end;
end;

procedure TChapterView.HandlePopupKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    (Parent as THintWindow).Hide;
end;

procedure TChapterView.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  // Mouse move handling can be implemented later if needed
end;

procedure TChapterView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    Blocks.LockUpdate;
    FontSize := FFontSize + Sign(WheelDelta) * 1;
    LoadChapter;
    Blocks.UnlockUpdate;
    Handled := true;
  end;
end;

procedure TChapterView.HandleDoubleClick(Sender: TObject);
var
  verses: TStringList;
begin
  if not assigned(FProject) or
     (FViewMode = cvmTranslationMemory) then
    Exit;

  verses := FProject.GetChapterText;
  try
    FRawMemo.Show(verses.Text);
  finally
    verses.Free;
  end;
end;

procedure TChapterView.HandleContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  if not assigned(FProject) then
  begin
    Handled := true;
    exit;
  end;

  FParagraphModeItem.Visible := FViewMode = cvmChapterText;
  FPrintChapterItem.Visible := FViewMode = cvmChapterText;
  FVerseRulesItem.Visible := (FViewMode = cvmChapterText) and
    (GetTextType = tbInterlinear);
  FInterlinearModeItem.Visible := (FViewMode = cvmChapterText) and
    (GetTextType = tbInterlinear);
end;

procedure TChapterView.HandleSetFont(Sender: TObject);
var
  dialog: TFontDialog;
begin
  dialog := TFontDialog.Create(self);
  dialog.Options := [fdNoStyleSel];
  dialog.Font.Name := FFontName;
  dialog.Font.Size := FFontSize;
  if dialog.Execute then
  begin
    Blocks.LockUpdate;
    FontName := dialog.Font.Name;
    FontSize := dialog.Font.Size;
    LoadChapter;
    Blocks.UnlockUpdate;
  end;
  dialog.Free;
end;

procedure TChapterView.HandlePrintChapter(Sender: TObject);
var
  setup: TKPrintSetupDialog;
begin
  setup := TKPrintSetupDialog.Create(nil);
  try
    PrintMode := true;
    setup.Control := self;
    setup.Execute;
  finally
    setup.Free;
    PrintMode := false;
  end;
end;

procedure TChapterView.HandleParagraphMode(Sender: TObject);
begin
  if ParagraphMode = pmParagraph then
    ParagraphMode := pmNoParagraphs
  else
    ParagraphMode := pmParagraph;
end;

procedure TChapterView.HandleVerseRules(Sender: TObject);
begin
  InterlinearVerseRulesForm.Project := FProject;
  if InterlinearVerseRulesForm.ShowModal = mrOK then
    LoadChapter;
end;

procedure TChapterView.HandleInterlinearMode(Sender: TObject);
begin
  InterlinearMode := imInterlinear;
end;

procedure TChapterView.HandleIntralinearMode(Sender: TObject);
begin
  InterlinearMode := imIntralinear;
end;

procedure TChapterView.SetInterlinearMode(AValue: TInterlinearMode);
begin
  FInterlinearModeInterItem.Checked := AValue = imInterlinear;
  FInterlinearModeIntraItem.Checked := AValue = imIntralinear;

  if FProject.InterlinearMode = AValue then Exit;
  FProject.InterlinearMode := AValue;

  LoadChapter;
end;

procedure TChapterView.SetPrintMode(AValue: boolean);
begin
  if FPrintMode=AValue then Exit;
  FPrintMode:=AValue;
  HandleVerseChange(FProject);
end;

procedure TChapterView.SetProject(AValue: TProject);
begin
  if FProject = AValue then Exit;
  ClearTranslationMemorySourceHighlight;
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  ResetTranslationMemoryOccurrences;
  if Assigned(FProject) then
    FProject.OnNewVerseUnsubscribe(@HandleVerseChange);
  FProject := AValue;
  FTranslationMemoryRefreshPending := True;
  FTranslationMemoryReferenceSnapshot := '';
  FViewSelectionActive := False;

  if assigned(FProject) then
  begin
    TokenizerVersion := FProject.TokenizerVersion;
    FViewMode := FProject.ChapterViewMode;
    SetParagraphMode(FProject.ParagraphMode);
    SetInterlinearMode(FProject.InterlinearMode);
    FProject.OnNewVerseSubscribe(@HandleVerseChange);
  end;
end;

procedure TChapterView.SetViewMode(AValue: TChapterViewMode);
begin
  FViewSelectionActive := True;
  if FViewMode = AValue then
  begin
    if (AValue = cvmTranslationMemory) and
       FTranslationMemoryRefreshPending and
       IsTranslationMemoryVisible then
      LoadChapter;
    Exit;
  end;
  ClearTranslationMemorySourceHighlight;
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  FViewMode := AValue;
  if AValue = cvmTranslationMemory then
    FTranslationMemoryRefreshPending := True;
  if Assigned(FProject) then
  begin
    FProject.ChapterViewMode := AValue;
    LoadChapter;
  end;
end;

procedure TChapterView.SetTranslationMemoryOrder(
  AValue: TTranslationMemoryOrder);
begin
  if FTranslationMemoryOrder = AValue then
    Exit;
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  FTranslationMemoryOrder := AValue;
  FTranslationMemoryRefreshPending := True;
  if IsTranslationMemoryVisible then
  begin
    if FTranslationMemoryShowingOccurrences then
      RenderTranslationMemoryOccurrences
    else
    begin
      RenderTranslationMemory;
      TopPos := 0;
    end;
  end;
end;

procedure TChapterView.InitPopupMenu;
begin
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Parent := Self;

  { Set font }
  FSetFontItem := TMenuItem.Create(PopupMenu);
  with FSetFontItem do
  begin
    Caption := SSetFont;
    OnClick := @HandleSetFont;
  end;
  PopupMenu.Items.Add(FSetFontItem);

  { Paragraph mode }
  FParagraphModeItem := TMenuItem.Create(PopupMenu);
  with FParagraphModeItem do
  begin
    Caption := SParagraphMode;
    OnClick := @HandleParagraphMode;
  end;
  PopupMenu.Items.Add(FParagraphModeItem);

  { Interlinear mode }
  FInterlinearModeItem := TMenuItem.Create(PopupMenu);
  FInterlinearModeItem.Caption := SInterlinearMode;
  PopupMenu.Items.Add(FInterlinearModeItem);

  { Interlinear mode -> Interlinear }
  FInterlinearModeInterItem := TMenuItem.Create(PopupMenu);
  with FInterlinearModeInterItem do
  begin
    Caption := SInterlinearModeInter;
    OnClick := @HandleInterlinearMode;
  end;
  FInterlinearModeItem.Add(FInterlinearModeInterItem);

  { Interlinear mode -> Intralinear }
  FInterlinearModeIntraItem := TMenuItem.Create(PopupMenu);
  with FInterlinearModeIntraItem do
  begin
    Caption := SInterlinearModeIntra;
    OnClick := @HandleIntralinearMode;
  end;
  FInterlinearModeItem.Add(FInterlinearModeIntraItem);

  { Verse rules }
  FVerseRulesItem := TMenuItem.Create(PopupMenu);
  with FVerseRulesItem do
  begin
    Caption := SVerseRules;
    OnClick := @HandleVerseRules;
  end;
  PopupMenu.Items.Add(FVerseRulesItem);

  { Print }
  FPrintChapterItem := TMenuItem.Create(PopupMenu);
  with FPrintChapterItem do
  begin
    Caption := SPrintChapter;
    OnClick := @HandlePrintChapter;
  end;
  PopupMenu.Items.Add(FPrintChapterItem);
end;

procedure TChapterView.SetParagraphMode(AValue: TParagraphMode);
begin
  FParagraphModeItem.Checked := AValue = pmParagraph;

  if FProject.ParagraphMode = AValue then exit;
  FProject.ParagraphMode := AValue;

  LoadChapter;
end;

procedure TChapterView.SetFontSize(AValue: integer);
begin
  if (FFontSize = AValue) or (AValue < 1) then Exit;

  Blocks.LockUpdate;
  FFontSize := AValue;
  TextStyle.Font.Size := FFontSize;
  FHint.Font.Assign(TextStyle.Font);
  Blocks.UnlockUpdate;
end;

procedure TChapterView.SetFontName(AValue: string);
begin
  if (FFontName = AValue) then Exit;

  Blocks.LockUpdate;
  FFontName := AValue;
  TextStyle.Font.Name := FFontName;
  FHint.Font.Assign(TextStyle.Font);
  Blocks.UnlockUpdate;
end;

procedure TChapterView.SetupDefaultStyle;
begin
  inherited SetupDefaultStyle;
  TextStyle.Font.Size := FFontSize;
  TextStyle.Font.Name := FFontName;
  TextStyle.ScriptPosition := tpoNormal;
  ParaStyle.FirstIndent := 0;
  ParaStyle.LineSpacingFactor := 1.15;
end;

procedure TChapterView.HighlightRange(range: TBlockRange; bgcolor: TColor);
var
  i: integer;
begin
  for i:=range.first to range.last do
  begin
    if Blocks[i].ClassName = 'TKMemoTextBlock' then
      TKMemoTextBlock(Blocks[i]).TextStyle.Brush.Color := bgcolor
    else if Blocks[i].ClassName = 'TKMemoContainer' then
      TKMemoContainer(Blocks[i]).BlockStyle.Brush.Color:= bgcolor;;
  end;
end;

function TChapterView.GetPreviousParagraphBlock: TKMemoParagraph;
var
  b: TKMemoBlockIndex;
begin
  Result := nil;
  for b := Blocks.Count-1 downto 0 do
    if Blocks[b] is TKMemoParagraph then
    begin
      Result := Blocks[b] as TKMemoParagraph;
      break;
    end;
end;

procedure TChapterView.SetTextType(AValue: TTipoTextoBiblico);
begin
  if not Assigned(FProject) then
    Exit;
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  FViewSelectionActive := True;
  if FViewMode <> cvmChapterText then
  begin
    ClearTranslationMemorySourceHighlight;
    FViewMode := cvmChapterText;
    FProject.ChapterViewMode := cvmChapterText;
  end;
  if GetTextType <> AValue then
    FProject.ChapterViewText := AValue;
  LoadChapter;
end;

procedure TChapterView.RenderChapterHeader;
var
  c: integer;
begin
  { current chapter }
  with Blocks.AddTextBlock(SysUtils.Format('%s %d', [FProject.Book, FProject.Chapter])).TextStyle.Font do
  begin
    Name := 'default';
    Size := Self.TextStyle.Font.Size+2;
    Bold := true;
    Color := NonBibleTextColor;
  end;

  if not FPrintMode then
  begin
    Blocks.AddParagraph(); //.ParaStyle.HAlign := halLeft;

    { chapter list }
    for c:=1 to QCapitulosONT[FProject.BookID] do
    begin
      if c > 1 then
        Blocks.AddTextBlock(' ');

      if c = FProject.Chapter then
        with Blocks.AddTextBlock(c.ToString).TextStyle.Font do
        begin
          Name := 'default';
          Size := Self.TextStyle.Font.Size-2;
          Bold := true;
          Color := NonBibleTextColor;
        end
      else
        with Blocks.AddHyperlink(c.ToString, Format('%d,%d,1', [FProject.BookID, c])) do
        begin
          OnClick := @HandleReferenceClick;
          TextStyle.Font.Name  := 'default';
          TextStyle.Font.Size  := Self.TextStyle.Font.Size-2;
          TextStyle.Font.Bold  := false;
          TextStyle.Font.Color := ChapterNumberColor;
        end;
    end;
  end;

  with Blocks.AddParagraph().ParaStyle do
  begin
    BottomPadding := 10;
    FirstIndent := 0;
  end;
end;

procedure TChapterView.RenderChapterFooter;
  function GetFirstBook: integer;
  begin
    if FProject.Escopo = etNT then
      Result := 40
    else
      Result := 1;
  end;

  procedure GetPreviousChapter(var book: integer; var chapter: integer);
  begin
    if (FProject.BookID = GetFirstBook) and (FProject.Chapter = 1) then
       exit;

    if FProject.Chapter = 1 then
    begin
      book := FProject.BookID - 1;
      chapter := QCapitulosONT[book];
    end else
    begin
      book := FProject.BookID;
      chapter := FProject.Chapter - 1;
    end;
  end;

  function GetLastBook: integer;
  begin
    if FProject.Escopo = etOT then
      Result := 39
    else
      Result := 66;
  end;

  procedure GetNextChapter(var book: integer; var chapter: integer);
  begin
    if (FProject.BookID = GetLastBook) and (FProject.Chapter = QCapitulosONT[FProject.BookID]) then
       exit;

    if FProject.Chapter = QCapitulosONT[FProject.BookID] then
    begin
      book := FProject.BookID + 1;
      chapter := 1;
    end else
    begin
      book := FProject.BookID;
      chapter := FProject.Chapter + 1;
    end;
  end;

var
  pbook, nbook, pchapter, nchapter: integer;
begin
  if FPrintMode then exit;

  pbook := 0;
  pchapter := 0;
  GetPreviousChapter(pbook, pchapter);

  if pbook <> 0 then
    with Blocks.AddHyperlink(Format('<< %s %d', [LocalizedBookNameByID(pbook), pchapter]), Format('%d,%d,1', [pbook, pchapter])) do
    begin
      OnClick := @HandleReferenceClick;
      TextStyle.Font.Name  := 'default';
      TextStyle.Font.Size  := Self.TextStyle.Font.Size-2;
      TextStyle.Font.Bold  := false;
      TextStyle.Font.Color := ChapterNumberColor;
    end;

  nbook := 0;
  nchapter := 0;
  GetNextChapter(nbook, nchapter);

  if pbook <> 0 then
    Blocks.AddTextBlock(' | ');

  with Blocks.AddHyperlink(Format('%s', [SChapterBeginning]), Format('%d,%d,1', [FProject.BookID, FProject.Chapter])) do
  begin
    OnClick := @HandleReferenceClick;
    TextStyle.Font.Name  := 'default';
    TextStyle.Font.Size  := Self.TextStyle.Font.Size-2;
    TextStyle.Font.Bold  := false;
    TextStyle.Font.Color := ChapterNumberColor;
  end;

  if nbook <> 0 then
  begin
    Blocks.AddTextBlock(' | ');
    with Blocks.AddHyperlink(Format('%s %d >>', [LocalizedBookNameByID(nbook), nchapter]), Format('%d,%d,1', [nbook, nchapter])) do
    begin
      OnClick := @HandleReferenceClick;
      TextStyle.Font.Name  := 'default';
      TextStyle.Font.Size  := Self.TextStyle.Font.Size-2;
      TextStyle.Font.Bold  := false;
      TextStyle.Font.Color := ChapterNumberColor;
    end;
  end;

  with Blocks.AddParagraph().ParaStyle do
  begin
    TopPadding := 10;
    HAlign := halCenter;
  end;
end;

procedure TChapterView.SetEnabled(Value: Boolean);
begin
  if not Value and Enabled and Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  if assigned(FProject) and not Value and Enabled then
  begin
    ClearTranslationMemorySourceHighlight;
    ResetTranslationMemoryOccurrences;
    FProject.OnNewVerseUnsubscribe(@HandleVerseChange);
    FProject := nil;
    FViewSelectionActive := False;
    FTranslationMemoryRefreshPending := True;
    FTranslationMemoryReferenceSnapshot := '';
  end;

  inherited SetEnabled(Value);
end;

constructor TChapterView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent       := TWinControl(AOwner);
  ReadOnly     := true;
  OnKeyDown    := @HandleKeyDown;
  OnMouseWheel := @HandleMouseWheel;
  OnDblClick   := @HandleDoubleClick;
  //OnMouseMove  := @HandleMouseMove;
  OnContextPopup := @HandleContextPopup;
  FFontName       := 'default';
  FFontSize       := 10;
  FProject        := nil;
  FRxVerseHeading := RegexCreate('^((?:<TS[0-3]?>.*?<Ts>)*)(.*?)$', [rcoUTF8]);
  FNotes          := TNoteList.Create;
  FNoteNumberer   := TRFNoteNumberer.Create;
  FHint           := TNoteWindow.Create(Self);
  FHideVerseNumber:= false;
  FPrintMode      := false;
  FViewMode       := cvmChapterText;
  FTranslationMemoryOrder := tmoVerseOccurrence;
  SetLength(FTranslationMemoryEntries, 0);
  SetLength(FTranslationMemoryFocusEntries, 0);
  FTranslationMemoryFocus := TTranslationMemoryFocusController.Create;
  FTranslationMemoryPairsSnapshot := '';
  FTranslationMemoryReferenceSnapshot := '';
  FTranslationMemoryShowingOccurrences := False;
  SetLength(FTranslationMemoryOccurrenceMatches, 0);
  FTranslationMemoryOccurrenceLimit := 100;
  FTranslationMemoryRefreshPending := True;
  FViewSelectionActive := False;
  InitPopupMenu;

  FRawMemo    := TRawChapterMemo.Create(Parent, Self);
end;

destructor TChapterView.Destroy;
begin
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  FreeAndNil(FTranslationMemoryFocus);
  FRawMemo.Free;
  FNoteNumberer.Free;
  FNotes.Free;
  FHint.Free;
  if assigned(FProject) then
    FProject.OnNewVerseUnsubscribe(@HandleVerseChange);

  inherited Destroy;
end;

procedure TChapterView.Translate;
begin
  FSetFontItem.Caption              := SSetFont;
  FParagraphModeItem.Caption        := SParagraphMode;
  FInterlinearModeItem.Caption      := SInterlinearMode;
  FInterlinearModeInterItem.Caption := SInterlinearModeInter;
  FInterlinearModeIntraItem.Caption := SInterlinearModeIntra;
  FVerseRulesItem.Caption           := SVerseRules;
  FPrintChapterItem.Caption         := SPrintChapter;
  LoadChapter;
end;

procedure TChapterView.LoadChapter;
begin
  if Assigned(FProject) then
    HandleVerseChange(FProject);
end;

procedure TChapterView.RefreshTranslationMemory;
begin
  FTranslationMemoryRefreshPending := True;
  if not IsTranslationMemoryVisible then
  begin
    if Assigned(FTranslationMemoryFocus) then
      FTranslationMemoryFocus.Unbind;
    Exit;
  end;
  if FTranslationMemoryShowingOccurrences then
    RenderTranslationMemoryOccurrences
  else
    RenderTranslationMemory;
end;

procedure TChapterView.FocusTranslationMemoryForSyntagm(
  ASyntagm: TSyntagm);
var
  SyntagmIndex: Integer;
  SourceVerse: TVerse;
begin
  if not Enabled or not Assigned(FProject) or
     (FViewMode <> cvmTranslationMemory) or
     FTranslationMemoryShowingOccurrences then
  begin
    ClearTranslationMemoryEntryFocus;
    Exit;
  end;

  SourceVerse := FProject.VerseObjects[tbOrigem];
  if not Assigned(SourceVerse) or not Assigned(SourceVerse.Syntagms) then
  begin
    ClearTranslationMemoryEntryFocus;
    Exit;
  end;

  // A normal selection does not change the memory data. Avoid rebuilding the
  // entire view unless this click actually changed an association.
  if SourceVerse.Pairs <> FTranslationMemoryPairsSnapshot then
    RenderTranslationMemory;

  if not Assigned(ASyntagm) or
     (ASyntagm.VerseRef <> SourceVerse) then
    Exit;

  SyntagmIndex := SourceVerse.Syntagms.IndexOf(ASyntagm);
  if SyntagmIndex < 0 then
  begin
    ClearTranslationMemoryEntryFocus;
    Exit;
  end;

  if not FTranslationMemoryFocus.FocusSyntagmIndex(SyntagmIndex) then
    ClearTranslationMemoryEntryFocus;
end;

procedure TChapterView.ClearTranslationMemorySourceHighlight;
var
  SourceVerse: TVerse;
begin
  if not Assigned(FProject) then
    Exit;
  SourceVerse := FProject.VerseObjects[tbOrigem];
  if Assigned(SourceVerse) then
    SourceVerse.DisableStrongHighlight;
end;

procedure TChapterView.ClearTranslationMemoryEntryFocus;
begin
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Clear;
end;

procedure TChapterView.RenderTranslationMemoryOccurrences;
var
  BlockBounds: TRect;
  CurrentBlockIndex: Integer;
  ErrorMessage: string;
  ItemIndex: Integer;
  LinkBlock: TKMemoHyperlink;
  OccurrencePage: TTranslationOccurrencePage;
  Remaining: Integer;
  TextBlock: TKMemoTextBlock;
begin
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  if not IsTranslationMemoryVisible then
  begin
    FTranslationMemoryRefreshPending := True;
    Exit;
  end;

  FTranslationMemoryRefreshPending := False;
  FRawMemo.Hide;
  if not FProject.TryGetTranslationMemoryOccurrences(
    FTranslationMemoryOccurrenceMatches,
    FTranslationMemoryOccurrenceLimit, OccurrencePage, ErrorMessage) then
  begin
    OccurrencePage.TotalAssociations := 0;
    OccurrencePage.TotalVerses := 0;
    OccurrencePage.HasMore := False;
    SetLength(OccurrencePage.Items, 0);
  end;

  CurrentBlockIndex := -1;
  Blocks.LockUpdate;
  try
    Blocks.Clear;
    ResetStyleStack;

    LinkBlock := Blocks.AddHyperlink(STranslationMemoryBack, 'back');
    LinkBlock.OnClick := @HandleTranslationMemoryBackClick;
    LinkBlock.TextStyle.Font.Color := NoteLinkColor;
    LinkBlock.TextStyle.Font.Style := [fsUnderline];
    with Blocks.AddParagraph().ParaStyle do
    begin
      FirstIndent := 0;
      BottomPadding := 8;
    end;

    TextBlock := Blocks.AddTextBlock(Format(
      STranslationMemoryOccurrencesTitleFmt,
      [FTranslationMemoryOccurrenceSource,
       FTranslationMemoryOccurrenceTranslation]));
    TextBlock.TextStyle.Font.Name := 'default';
    TextBlock.TextStyle.Font.Size := TextStyle.Font.Size + 2;
    TextBlock.TextStyle.Font.Bold := True;
    TextBlock.TextStyle.Font.Color := NonBibleTextColor;
    Blocks.AddParagraph().ParaStyle.FirstIndent := 0;

    if ErrorMessage <> '' then
    begin
      TextBlock := Blocks.AddTextBlock(Format(
        STranslationMemoryErrorFmt, [ErrorMessage]));
      TextBlock.TextStyle.Font.Color := clRed;
      Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
      Exit;
    end;

    TextBlock := Blocks.AddTextBlock(TranslationMemoryOccurrenceTotalsText(
      OccurrencePage.TotalAssociations, OccurrencePage.TotalVerses));
    TextBlock.TextStyle.Font.Color := clGray;
    with Blocks.AddParagraph().ParaStyle do
    begin
      FirstIndent := 0;
      BottomPadding := 10;
    end;

    if Length(OccurrencePage.Items) = 0 then
    begin
      TextBlock := Blocks.AddTextBlock(STranslationMemoryNoOccurrences);
      TextBlock.TextStyle.Font.Italic := True;
      TextBlock.TextStyle.Font.Color := clGray;
      Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
      Exit;
    end;

    for ItemIndex := 0 to Length(OccurrencePage.Items) - 1 do
      with OccurrencePage.Items[ItemIndex] do
      begin
        if ReferenceID = FProject.ID then
          CurrentBlockIndex := Blocks.Count;
        LinkBlock := Blocks.AddHyperlink(DisplayReference, ReferenceID);
        LinkBlock.OnClick := @HandleTranslationMemoryOccurrenceClick;
        LinkBlock.TextStyle.Font.Color := NoteLinkColor;
        LinkBlock.TextStyle.Font.Style := [fsUnderline];
        if ReferenceID = FProject.ID then
        begin
          LinkBlock.TextStyle.Font.Style := [fsBold, fsUnderline];
          LinkBlock.TextStyle.Font.Color := ChapterNumberColor;
        end;
        if Occurrences > 1 then
        begin
          TextBlock := Blocks.AddTextBlock(' (' + Format(
            STranslationMemoryOccurrenceCountFmt, [Occurrences]) + ')');
          TextBlock.TextStyle.Font.Color := clGray;
        end;
        if DestinationText <> '' then
          Blocks.AddTextBlock(' — ' + DestinationText);
        with Blocks.AddParagraph().ParaStyle do
        begin
          FirstIndent := 0;
          BottomPadding := 5;
        end;
      end;

    if OccurrencePage.HasMore then
    begin
      Remaining := OccurrencePage.TotalVerses -
        Length(OccurrencePage.Items);
      LinkBlock := Blocks.AddHyperlink(Format(
        STranslationMemoryShowMoreFmt, [Remaining]), 'more');
      LinkBlock.OnClick := @HandleTranslationMemoryShowMoreClick;
      LinkBlock.TextStyle.Font.Color := NoteLinkColor;
      LinkBlock.TextStyle.Font.Style := [fsUnderline];
      with Blocks.AddParagraph().ParaStyle do
      begin
        FirstIndent := 0;
        TopPadding := 8;
      end;
    end;
  finally
    Blocks.UnLockUpdate;
  end;

  if (CurrentBlockIndex >= 0) and
     (CurrentBlockIndex < Blocks.Count) then
  begin
    TopPos := 0;
    BlockBounds := BlockRect(Blocks[CurrentBlockIndex]);
    TopPos := BlockBounds.Top;
  end;
end;

procedure TChapterView.RenderTranslationMemory;
var
  ErrorMessage: string;
  EntryIndex: Integer;
  AlternativeIndex: Integer;
  MetadataText: string;
  TextBlock: TKMemoTextBlock;
  LinkBlock: TKMemoHyperlink;
  SourceVerse: TVerse;
  HasCurrentAlternative: Boolean;
  SyntagmIndex: Integer;
begin
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  if not IsTranslationMemoryVisible then
  begin
    FTranslationMemoryRefreshPending := True;
    Exit;
  end;

  FTranslationMemoryRefreshPending := False;
  FRawMemo.Hide;
  if not FProject.TryGetCurrentVerseTranslationMemory(
    FTranslationMemoryOrder,
    FTranslationMemoryEntries, ErrorMessage) then
    SetLength(FTranslationMemoryEntries, 0);
  SourceVerse := FProject.VerseObjects[tbOrigem];
  if Assigned(SourceVerse) then
    FTranslationMemoryPairsSnapshot := SourceVerse.Pairs
  else
    FTranslationMemoryPairsSnapshot := '';
  SetLength(FTranslationMemoryFocusEntries,
    Length(FTranslationMemoryEntries));
  for EntryIndex := 0 to High(FTranslationMemoryFocusEntries) do
  begin
    FTranslationMemoryFocusEntries[EntryIndex].AnchorBlockIndex := -1;
    FTranslationMemoryFocusEntries[EntryIndex].FirstParagraphBlockIndex := -1;
    FTranslationMemoryFocusEntries[EntryIndex].LastParagraphBlockIndex := -1;
    SetLength(FTranslationMemoryFocusEntries[EntryIndex].SyntagmIndexes,
      Length(FTranslationMemoryEntries[EntryIndex].SyntagmIndexes));
    for SyntagmIndex := 0 to
      High(FTranslationMemoryEntries[EntryIndex].SyntagmIndexes) do
      FTranslationMemoryFocusEntries[EntryIndex].SyntagmIndexes[
        SyntagmIndex] := FTranslationMemoryEntries[EntryIndex].SyntagmIndexes[
        SyntagmIndex];
  end;

  Blocks.LockUpdate;
  try
    Blocks.Clear;
    ResetStyleStack;

    TextBlock := Blocks.AddTextBlock(STranslationMemoryTitle);
    TextBlock.TextStyle.Font.Name := 'default';
    TextBlock.TextStyle.Font.Size := TextStyle.Font.Size + 2;
    TextBlock.TextStyle.Font.Bold := True;
    TextBlock.TextStyle.Font.Color := NonBibleTextColor;
    Blocks.AddTextBlock(Format(' — %s %d:%d',
      [FProject.Book, FProject.Chapter, FProject.Verse]));
    with Blocks.AddParagraph().ParaStyle do
    begin
      FirstIndent := 0;
      BottomPadding := 10;
    end;

    if ErrorMessage <> '' then
    begin
      TextBlock := Blocks.AddTextBlock(Format(
        STranslationMemoryErrorFmt, [ErrorMessage]));
      TextBlock.TextStyle.Font.Color := clRed;
      Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
      Exit;
    end;

    if Length(FTranslationMemoryEntries) = 0 then
    begin
      TextBlock := Blocks.AddTextBlock(STranslationMemoryNoSourceWords);
      TextBlock.TextStyle.Font.Italic := True;
      Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
      Exit;
    end;

    HasCurrentAlternative := False;
    for EntryIndex := 0 to Length(FTranslationMemoryEntries) - 1 do
    begin
      FTranslationMemoryFocusEntries[EntryIndex].AnchorBlockIndex :=
        Blocks.Count;
      LinkBlock := Blocks.AddHyperlink(
        FTranslationMemoryEntries[EntryIndex].DisplayTerm,
        IntToStr(EntryIndex));
      LinkBlock.OnClick := @HandleTranslationMemoryEntryClick;
      LinkBlock.TextStyle.Font.Name := 'default';
      LinkBlock.TextStyle.Font.Size := TextStyle.Font.Size;
      LinkBlock.TextStyle.Font.Bold := True;
      LinkBlock.TextStyle.Font.Color := ChapterNumberColor;
      with Blocks.AddParagraph().ParaStyle do
      begin
        FirstIndent := 0;
        TopPadding := IfThen(EntryIndex = 0, 0, 8);
      end;
      FTranslationMemoryFocusEntries[EntryIndex].FirstParagraphBlockIndex :=
        Blocks.Count - 1;

      MetadataText := JoinMemoryValues(
        FTranslationMemoryEntries[EntryIndex].Forms, ', ');
      if MetadataText <> '' then
      begin
        TextBlock := Blocks.AddTextBlock(Format(
          STranslationMemoryFormsFmt, [MetadataText]));
        TextBlock.TextStyle.Font.Size := Max(1, TextStyle.Font.Size - 1);
        TextBlock.TextStyle.Font.Color := clGray;
        Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
      end;

      MetadataText := JoinMemoryValues(
        FTranslationMemoryEntries[EntryIndex].Strongs, ', ');
      if MetadataText <> '' then
      begin
        TextBlock := Blocks.AddTextBlock(Format(
          STranslationMemoryStrongsFmt, [MetadataText]));
        TextBlock.TextStyle.Font.Size := Max(1, TextStyle.Font.Size - 1);
        TextBlock.TextStyle.Font.Color := clGray;
        Blocks.AddParagraph().ParaStyle.FirstIndent := 0;
      end;

      if Length(FTranslationMemoryEntries[EntryIndex].Alternatives) = 0 then
      begin
        TextBlock := Blocks.AddTextBlock(
          STranslationMemoryNoTranslations);
        TextBlock.TextStyle.Font.Italic := True;
        TextBlock.TextStyle.Font.Color := clGray;
        Blocks.AddParagraph().ParaStyle.FirstIndent := 8;
      end
      else
      begin
        for AlternativeIndex := 0 to
          Length(FTranslationMemoryEntries[EntryIndex].Alternatives) - 1 do
        begin
          if AlternativeIndex > 0 then
            Blocks.AddTextBlock(', ');
          with FTranslationMemoryEntries[EntryIndex].Alternatives[
            AlternativeIndex] do
          begin
            TextBlock := Blocks.AddTextBlock(Translation);
            if IsCurrent then
            begin
              HasCurrentAlternative := True;
              TextBlock.TextStyle.Font.Bold := True;
              TextBlock.TextStyle.Font.Color :=
                TranslationMemoryCurrentColor;
            end;
            Blocks.AddTextBlock(' (');

            if (Occurrences > 0) and (Length(Matches) > 0) then
            begin
              LinkBlock := Blocks.AddHyperlink(IntToStr(Occurrences),
                IntToStr(EntryIndex) + ':' + IntToStr(AlternativeIndex));
              LinkBlock.OnClick := @HandleTranslationMemoryAlternativeClick;
              LinkBlock.TextStyle.Font.Color := NoteLinkColor;
              LinkBlock.TextStyle.Font.Style := [fsUnderline];
            end
            else
              Blocks.AddTextBlock(IntToStr(Occurrences));
            Blocks.AddTextBlock(')');
          end;
        end;
        Blocks.AddParagraph().ParaStyle.FirstIndent := 8;
      end;
      FTranslationMemoryFocusEntries[EntryIndex].LastParagraphBlockIndex :=
        Blocks.Count - 1;
    end;

    if HasCurrentAlternative then
    begin
      TextBlock := Blocks.AddTextBlock(
        STranslationMemoryCurrentLegend);
      TextBlock.TextStyle.Font.Size := Max(1, TextStyle.Font.Size - 1);
      TextBlock.TextStyle.Font.Italic := True;
      TextBlock.TextStyle.Font.Color := TranslationMemoryCurrentColor;
      with Blocks.AddParagraph().ParaStyle do
      begin
        FirstIndent := 0;
        TopPadding := 14;
      end;
    end;
  finally
    Blocks.UnLockUpdate;
  end;
  FTranslationMemoryFocus.Bind(Self, FTranslationMemoryFocusEntries);
end;

procedure TChapterView.RenderChapter(verses: TStringList; current: integer);
var
  verse: string;
  //starttime: DWord;
begin
  //starttime := getTickCount;
  SetLength(FVerseRanges, verses.Count + 1);
  FRawMemo.Hide;

  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  Blocks.LockUpdate;
  try
    Blocks.Clear;
    ResetStyleStack;

    FNoteID := 1;
    FNoteNumberer.Reset;
    FNotes.Clear;

    RenderChapterHeader;

    FCurrentVerse := 0;
    for verse in verses do
    begin
      Inc(FCurrentVerse);
      RenderVerse(verse);
    end;

    if Blocks.LastBlock.ClassName <> 'TKMemoParagraph' then
      Blocks.AddParagraph().ParaStyle.FirstIndent := IfThen(GetParagraphMode = pmParagraph, DefaultFirstIndent);

    RenderChapterFooter;
    if not FPrintMode then
      HighlightRange(FVerseRanges[current], clSilver);
    //RenderNotes;
  finally
    Blocks.UnLockUpdate;
  end;

  if not FPrintMode then
  begin
    { scroll current verse to center }
    self.SelectBlock(Blocks[FVerseRanges[current].first], sgpTopLeft);
    self.SelLength:=0;
    ExecuteCommand(ecScrollCenter);
  end;

  //DebugLn('  TChapterView.RenderChapter: %d milliseconds', [getTickCount-starttime]);
end;

end.

