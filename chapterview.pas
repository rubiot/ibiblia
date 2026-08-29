unit ChapterView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, Graphics, ONTTokenizer, LCLType, LMessages,
  HTMLColors,
  LazUTF8, BibleScope, Project, Verse, Syntagm, PCRE, Controls, Menus, fgl, Dialogs, Math, Contnrs, KMemo,
  KEditCommon, KGraphics, KDialogs, FormInterlinearVerseRules,
  LCLTranslator, LazLogger, HTMLRenderer, SynEdit,
  theword_syntax_highlighter, bible_book_names, translation_memory_model,
  translation_memory_focus_controller, translation_memory_preview,
  SuggestionManager, rf_note_scanner,
  theword_translator_note_preview, theword_effective_inline_style_v2,
  theword_translator_note_style, translator_note_hover_controller,
  project_verse_repository;

type

  TBlockRange = record
    first, last: integer;
  end;

  TTranslationMemoryInputSnapshot = record
    Initialized: Boolean;
    ReferenceID: string;
    SourceXML: string;
    DestinationXML: string;
    Associations: string;
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
    FMarkerLink: TKMemoHyperlink;
    FRenderRevision: QWord;
  public
    constructor Create(id: integer; verse: integer; text: string; linktext: string);
    destructor Destroy; override;
    property Id: integer read FId write FId;
    property LinkText: string read FLinkText write FLinkText;
    property Verse: integer read FVerse write FVerse;
    property Text: string read FText write FText;
    property Range: TBlockRange read FRange write FRange;
    property FirstBlock: integer read FRange.first write FRange.first;
    property LastBlock: integer read FRange.last write FRange.last;
    property MarkerLink: TKMemoHyperlink read FMarkerLink write FMarkerLink;
    property RenderRevision: QWord read FRenderRevision write FRenderRevision;
  end;

  { TNoteWindow }

  TNoteWindow = class(THintWindow)
  private
    FHasRenderedBody: Boolean;
    FLastBody: string;
    FLastFontName: string;
    FLastFontSize: Integer;
    FNoteView: TTranslatorNoteBodyView;
    function GetScrollBarsVisible: boolean;
    procedure InitNoteView;
    procedure SetNoteText(const AText: string; const AStyle: TKMemoTextStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(const AHint: string; AStyle: TKMemoTextStyle); overload;
    property ScrollBarsVisible: boolean read GetScrollBarsVisible;
  end;

  { TChapterTranslatorNoteHoverController }

  TChapterTranslatorNoteHoverController = class(TTranslatorNoteHoverController)
  private
    FChapterView: TChapterView;
  protected
    function CreatePreviewWindow: TCustomForm; override;
    procedure ShowPreviewWindow(APreview: TCustomForm;
      const ABodyRaw: UnicodeString;
      const AAnchorScreenRect: TRect); override;
  public
    constructor Create(AOwner: TComponent;
      AChapterView: TChapterView); reintroduce;
  end;

  TNoteList = specialize TFPGObjectList<TNoteInfo>;
  TTranslationMemoryCounterLinkArray = array of TKMemoHyperlink;
  TTranslationMemoryCounterLinkMatrix =
    array of TTranslationMemoryCounterLinkArray;

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
    FTranslatorNoteHoverController: TChapterTranslatorNoteHoverController;
    FTranslatorNoteRevision: QWord;
    FSetFontItem: TMenuItem;
    FParagraphModeItem: TMenuItem;
    FVerseRulesItem: TMenuItem;
    FInterlinearModeItem: TMenuItem;
    FInterlinearModeInterItem: TMenuItem;
    FInterlinearModeIntraItem: TMenuItem;
    FPrintChapterItem: TMenuItem;
    FVerseRanges: array of TBlockRange;
    FCurrentVerse: integer;
    FRenderedChapterBookID: Integer;
    FRenderedChapterChapter: Integer;
    FRenderedChapterContentRevision: QWord;
    FRenderedChapterCurrentVerse: Integer;
    FRenderedChapterFontName: string;
    FRenderedChapterFontSize: Integer;
    FRenderedChapterFullRenderCount: QWord;
    FRenderedChapterHideVerseNumber: Boolean;
    FRenderedChapterInterlinearMode: TInterlinearMode;
    FRenderedChapterParagraphMode: TParagraphMode;
    FRenderedChapterPrintMode: Boolean;
    FRenderedChapterProject: TProject;
    FRenderedChapterTextType: TTipoTextoBiblico;
    FRenderedChapterTokenizerVersion: Integer;
    FRenderedChapterValid: Boolean;
    FRenderedChapterVerses: TStringList;
    FNotes: TNoteList;
    FNotesById: TStringList;
    FNotesByLink: TStringList;
    FHideVerseNumber: boolean;
    FRawMemo: TRawChapterMemo;
    FViewMode: TChapterViewMode;
    FTranslationMemoryOrder: TTranslationMemoryOrder;
    FTranslationMemoryEntries: TTranslationMemoryEntryArray;
    FTranslationMemoryFocusEntries: TTranslationMemoryFocusEntryArray;
    FTranslationMemoryFocus: TTranslationMemoryFocusController;
    FTranslationMemoryInputSnapshot: TTranslationMemoryInputSnapshot;
    FTranslationMemoryShowingOccurrences: Boolean;
    FTranslationMemoryOccurrenceMatches: TTranslationPairMatchArray;
    FTranslationMemoryOccurrenceSource: string;
    FTranslationMemoryOccurrenceTranslation: string;
    FTranslationMemoryOccurrenceLimit: Integer;
    FTranslationMemoryOccurrenceCache: TTranslationOccurrencePage;
    FTranslationMemoryOccurrenceCacheLoaded: Boolean;
    FTranslationMemoryShowMoreBlockIndex: Integer;
    FTranslationMemoryRefreshPending: Boolean;
    FTranslationMemoryPreviewController: TTranslationMemoryHoverController;
    FTranslationMemoryPreviewFontSize: Integer;
    FOnTranslationMemoryPreviewFontSizeChanged: TNotifyEvent;
    FTranslationMemoryPreviewGeneration: QWord;
    FTranslationMemoryCounterLinks: TTranslationMemoryCounterLinkMatrix;
    FViewSelectionActive: Boolean;

    function GetInterlinearMode: TInterlinearMode;
    function GetParagraphMode: TParagraphMode;
    function GetTextType: TTipoTextoBiblico;
    function IsTranslationMemoryVisible: Boolean;
    function CurrentTranslationMemoryInputSnapshot:
      TTranslationMemoryInputSnapshot;
    procedure CaptureTranslationMemoryInputSnapshot;
    procedure RenderChapter(verses: TStringList; current: integer);
    procedure CaptureChapterRenderIdentity(AVerses: TStrings);
    function ChapterRenderKeyMatchesCurrentProject: Boolean;
    function ChapterRenderIdentityMatches(AVerses: TStrings): Boolean;
    procedure FocusRenderedChapterVerse(ACurrentVerse: Integer);
    procedure InvalidateChapterRenderCache;
    procedure RenderChapterHeader;
    procedure RenderChapterFooter;
    procedure RenderVerse(txt: string);
    procedure RenderVerseNumber(number: integer);
    procedure RenderSpan(cont: TKMemoBlocks; txt: string); override;
    procedure RenderNote(const OpeningTag, Note: string);
    procedure RenderNotes;
    procedure ClearTranslatorNotes;
    function FindTranslatorNoteById(ANoteId: QWord): TNoteInfo;
    function FindTranslatorNoteByLink(
      ALinkBlock: TKMemoHyperlink): TNoteInfo;
    procedure IndexTranslatorNote(ANoteInfo: TNoteInfo);
    procedure RenderInterlinearBlock(cont: TKMemoBlocks; blockText: string);
    procedure RenderInterlinearUnit(cont: TKMemoBlocks; translation: string);
    procedure HandleReferenceClick(sender: TObject);
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
    procedure HandleMouseLeave(Sender: TObject);
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HandleTranslationMemoryPreviewFontSizeChanged(Sender: TObject;
      AFontSize: Integer);
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
    procedure SetTranslationMemoryPreviewFontSize(AValue: Integer);
    procedure HighlightRange(range: TBlockRange; bgcolor: TColor);
    function GetPreviousParagraphBlock: TKMemoParagraph;
    procedure SetTextType(AValue: TTipoTextoBiblico);
    procedure RenderTranslationMemory;
    procedure RenderTranslationMemoryOccurrences;
    procedure ClearTranslationMemoryOccurrenceCache;
    procedure AppendTranslationMemoryOccurrencePage(
      const APage: TTranslationOccurrencePage);
    procedure AppendTranslationMemoryOccurrenceItems(
      const AItems: TTranslationOccurrenceDataArray;
      var ACurrentBlockIndex: Integer);
    procedure RemoveTranslationMemoryShowMoreBlock;
    procedure AppendTranslationMemoryShowMoreBlock;
    procedure ResetTranslationMemoryOccurrences(
      AWillReplaceDocument: Boolean = False);
    procedure ClearTranslationMemorySourceHighlight;
    procedure ClearTranslationMemoryEntryFocus;
    procedure DismissTranslatorNotePreview;
    procedure InvalidateTranslatorNotePreview;
    procedure DismissTranslationMemoryPreview;
    procedure InvalidateTranslationMemoryPreview;
    function TryResolveTranslatorNoteHoverTarget(
      ALinkBlock: TKMemoHyperlink; out ARevision, ANoteId: QWord): Boolean;
    function ResolveTranslatorNoteHover(Sender: TObject; ARevision,
      ANoteId: QWord; out ABodyRaw: UnicodeString;
      out AAnchorScreenRect: TRect): Boolean;
    procedure ShowTranslatorNotePreview(APreview: TNoteWindow;
      const ABodyRaw: UnicodeString);
    function TryResolveTranslationMemoryHoverTarget(
      ALinkBlock: TKMemoHyperlink;
      out ATarget: TTranslationMemoryHoverTarget): Boolean;
    function ValidateTranslationMemoryPreviewTarget(
      const ATarget: TTranslationMemoryHoverTarget;
      out AAnchorScreenRect: TRect): Boolean;
    function LoadTranslationMemoryPreviewPayload(
      const ATarget: TTranslationMemoryHoverTarget;
      out APayload: TTranslationMemoryPreviewPayload): Boolean;
  protected
    procedure Resize; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetupDefaultStyle; override;
    procedure WndProc(var TheMessage: TLMessage); override;
    procedure ApplyProjectedInlineStyle(ABlock: TKMemoTextBlock;
      ABaseStyle: TKMemoTextStyle;
      const AEffective: TTheWordEffectiveInlineStyleV2); override;
    procedure AppendTranslationMemoryOccurrenceText(
      const Occurrence: TTranslationOccurrenceData);
    function TryLoadTranslationMemoryOccurrenceRange(
      const Matches: TTranslationPairMatchArray; Offset, Limit: Integer;
      out Occurrences: TTranslationOccurrencePage;
      out ErrorMessage: string): Boolean; virtual;
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
    property TranslationMemoryPreviewFontSize: Integer
      read FTranslationMemoryPreviewFontSize
      write SetTranslationMemoryPreviewFontSize;
    property OnTranslationMemoryPreviewFontSizeChanged: TNotifyEvent
      read FOnTranslationMemoryPreviewFontSizeChanged
      write FOnTranslationMemoryPreviewFontSizeChanged;
    property CurrentStyle: TKMemoTextStyle read GetCurrentStyle;
    property HideVerseNumber: boolean read FHideVerseNumber write FHideVerseNumber default false;
    property PrintMode: boolean read FPrintMode write SetPrintMode;
    {$IFDEF FPCUNIT}
    procedure ConfigureTranslationMemoryInteractionFixture(
      const AEntries: TTranslationMemoryEntryArray;
      const ACounterLinks: TTranslationMemoryCounterLinkMatrix);
    function ResolveTranslationMemoryHoverTargetFixture(
      ALinkBlock: TKMemoHyperlink;
      out ATarget: TTranslationMemoryHoverTarget): Boolean;
    procedure BeginTranslationMemoryHoverFixture(
      const ATarget: TTranslationMemoryHoverTarget);
    function TranslationMemoryPreviewActiveFixture: Boolean;
    function ResolveTranslatorNoteHoverTargetFixture(
      ALinkBlock: TKMemoHyperlink; out ARevision, ANoteId: QWord): Boolean;
    procedure RenderTranslatorNotesFixture;
    procedure BeginTranslatorNoteHoverFixture(ANoteId: QWord);
    procedure InvalidateTranslatorNotePreviewFixture;
    function TranslatorNotePreviewActiveFixture: Boolean;
    function TranslatorNoteRevisionFixture: QWord;
    procedure ClearTranslatorNotesFixture;
    class function TranslatorNoteLiveCountFixture: Integer; static;
    function ChapterFullRenderCountFixture: QWord;
    procedure ReadTranslationMemoryOccurrenceStateFixture(
      out AMatches: TTranslationPairMatchArray; out ALimit: Integer;
      out AShowingOccurrences: Boolean);
    procedure ConfigureTranslationMemoryOccurrencePageFixture(
      const APage: TTranslationOccurrencePage);
    procedure AppendTranslationMemoryOccurrencePageFixture(
      const APage: TTranslationOccurrencePage);
    procedure ClearTranslationMemoryOccurrenceCacheFixture;
    function TranslationMemoryCachedOccurrenceCountFixture: Integer;
    function TranslationMemoryShowMoreBlockIndexFixture: Integer;
    {$ENDIF}
  end;

function TranslationMemoryInputSnapshotsEqual(
  const ALeft, ARight: TTranslationMemoryInputSnapshot): Boolean;
function ShouldResetTranslationMemoryOccurrences(AShowingOccurrences: Boolean;
  const APrevious, ACurrent: TTranslationMemoryInputSnapshot): Boolean;

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
  STranslationMemoryPreviewRemainingFmt =
    '%d more verses — click the count to see all.';
  STranslationMemoryPreviewUnavailable =
    'Preview unavailable — click to open the occurrences.';

implementation

uses
  performance_debug;

var
  TranslatorNoteInfoLiveCount: Integer = 0;

function TranslationMemoryInputSnapshotsEqual(
  const ALeft, ARight: TTranslationMemoryInputSnapshot): Boolean;
begin
  Result := (ALeft.Initialized = ARight.Initialized) and
    (not ALeft.Initialized or
      ((ALeft.ReferenceID = ARight.ReferenceID) and
       (ALeft.SourceXML = ARight.SourceXML) and
       (ALeft.DestinationXML = ARight.DestinationXML) and
       (ALeft.Associations = ARight.Associations)));
end;

function ShouldResetTranslationMemoryOccurrences(AShowingOccurrences: Boolean;
  const APrevious, ACurrent: TTranslationMemoryInputSnapshot): Boolean;
begin
  Result := AShowingOccurrences and
    not TranslationMemoryInputSnapshotsEqual(APrevious, ACurrent);
end;

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

procedure TChapterView.AppendTranslationMemoryOccurrenceText(
  const Occurrence: TTranslationOccurrenceData);
begin
  translation_memory_preview.AppendTranslationMemoryOccurrenceText(
    Blocks, Occurrence);
end;

function TChapterView.TryLoadTranslationMemoryOccurrenceRange(
  const Matches: TTranslationPairMatchArray; Offset, Limit: Integer;
  out Occurrences: TTranslationOccurrencePage;
  out ErrorMessage: string): Boolean;
begin
  Occurrences := Default(TTranslationOccurrencePage);
  ErrorMessage := '';
  if not Assigned(FProject) then
  begin
    ErrorMessage := 'Translation-memory project is not available';
    Exit(False);
  end;
  Result := FProject.TryGetTranslationMemoryOccurrenceRange(Matches,
    Offset, Limit, Occurrences, ErrorMessage);
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
  InsertControl(TTranslatorNoteBodyView.Create(Self));
  FNoteView := Controls[0] as TTranslatorNoteBodyView;
  with FNoteView do
  begin
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
  if FHasRenderedBody and (FLastBody = AText) and
     (FLastFontName = AStyle.Font.Name) and
     (FLastFontSize = AStyle.Font.Size) then
    Exit;
  FNoteView.Blocks.LockUpdate;
  try
    IBibliaPerfIncrement(ipcNotePreviewRenders);
    Width := 500;
    FNoteView.TextStyle.Font.Name := AStyle.Font.Name;
    FNoteView.TextStyle.Font.Size := AStyle.Font.Size - 1;
    FNoteView.SetNoteBody(UTF8Decode(AText));
    FNoteView.Background.Color := $00CCFBFB;
    FLastBody := AText;
    FLastFontName := AStyle.Font.Name;
    FLastFontSize := AStyle.Font.Size;
    FHasRenderedBody := True;
  finally
    FNoteView.Blocks.UnLockUpdate;
  end;
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

  AutoHide := false;

  SetNoteText(AHint, AStyle);

  Rect := FNoteView.ContentRect;

  Pos := Mouse.CursorPos;
  Rect.Left := Pos.X+10;
  Rect.Top := Pos.Y+5;
  Rect.Right := Rect.Left + Rect.Right + 5;
  Rect.Bottom := Rect.Top + Rect.Bottom + 5;
  ActivateHint(Rect, '');
end;

{ TChapterTranslatorNoteHoverController }

constructor TChapterTranslatorNoteHoverController.Create(
  AOwner: TComponent; AChapterView: TChapterView);
begin
  inherited Create(AOwner);
  FChapterView := AChapterView;
end;

function TChapterTranslatorNoteHoverController.CreatePreviewWindow:
  TCustomForm;
begin
  Result := TNoteWindow.Create(nil);
end;

procedure TChapterTranslatorNoteHoverController.ShowPreviewWindow(
  APreview: TCustomForm; const ABodyRaw: UnicodeString;
  const AAnchorScreenRect: TRect);
begin
  if Assigned(FChapterView) and (APreview is TNoteWindow) then
    FChapterView.ShowTranslatorNotePreview(TNoteWindow(APreview), ABodyRaw);
end;

{ TNoteInfo }

constructor TNoteInfo.Create(id: integer; verse: integer; text: string;
  linktext: string);
begin
  Inc(TranslatorNoteInfoLiveCount);
  FId    := id;
  FVerse := verse;
  FText  := text;
  FLinkText := linktext;
  FMarkerLink := nil;
  FRenderRevision := 0;
end;

destructor TNoteInfo.Destroy;
begin
  Dec(TranslatorNoteInfoLiveCount);
  inherited Destroy;
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

function TChapterView.CurrentTranslationMemoryInputSnapshot:
  TTranslationMemoryInputSnapshot;
var
  DestinationVerse: TVerse;
  SourceVerse: TVerse;
begin
  Result.Initialized := Assigned(FProject);
  Result.ReferenceID := '';
  Result.SourceXML := '';
  Result.DestinationXML := '';
  Result.Associations := '';
  if not Result.Initialized then
    Exit;

  Result.ReferenceID := FProject.ID;
  SourceVerse := FProject.VerseObjects[tbOrigem];
  if Assigned(SourceVerse) then
  begin
    Result.SourceXML := SourceVerse.XML;
    Result.Associations := SourceVerse.Pairs;
  end;
  DestinationVerse := FProject.VerseObjects[tbDestino];
  if Assigned(DestinationVerse) then
    Result.DestinationXML := DestinationVerse.XML;
end;

procedure TChapterView.CaptureTranslationMemoryInputSnapshot;
begin
  FTranslationMemoryInputSnapshot :=
    CurrentTranslationMemoryInputSnapshot;
end;

procedure TChapterView.DismissTranslatorNotePreview;
begin
  if Assigned(FTranslatorNoteHoverController) then
    FTranslatorNoteHoverController.Dismiss;
end;

procedure TChapterView.InvalidateTranslatorNotePreview;
begin
  Inc(FTranslatorNoteRevision);
  DismissTranslatorNotePreview;
end;

function TranslatorNoteLinkKey(ALinkBlock: TKMemoHyperlink): string;
begin
  Result := IntToHex(PtrUInt(ALinkBlock), SizeOf(Pointer) * 2);
end;

procedure TChapterView.ClearTranslatorNotes;
begin
  FNotesByLink.Clear;
  FNotesById.Clear;
  FNotes.Clear;
end;

function TChapterView.FindTranslatorNoteById(ANoteId: QWord): TNoteInfo;
var
  NoteIndex: Integer;
begin
  Result := nil;
  if ANoteId = 0 then
    Exit;
  NoteIndex := FNotesById.IndexOf(IntToStr(Int64(ANoteId)));
  if NoteIndex >= 0 then
    Result := TNoteInfo(FNotesById.Objects[NoteIndex]);
end;

function TChapterView.FindTranslatorNoteByLink(
  ALinkBlock: TKMemoHyperlink): TNoteInfo;
var
  NoteIndex: Integer;
begin
  Result := nil;
  if not Assigned(ALinkBlock) then
    Exit;
  NoteIndex := FNotesByLink.IndexOf(TranslatorNoteLinkKey(ALinkBlock));
  if NoteIndex >= 0 then
  begin
    Result := TNoteInfo(FNotesByLink.Objects[NoteIndex]);
    if not Assigned(Result) or (Result.MarkerLink <> ALinkBlock) then
      Result := nil;
  end;
end;

procedure TChapterView.IndexTranslatorNote(ANoteInfo: TNoteInfo);
begin
  if not Assigned(ANoteInfo) or not Assigned(ANoteInfo.MarkerLink) then
    Exit;
  FNotesById.AddObject(IntToStr(ANoteInfo.Id), ANoteInfo);
  FNotesByLink.AddObject(TranslatorNoteLinkKey(ANoteInfo.MarkerLink),
    ANoteInfo);
end;

function TChapterView.TryResolveTranslatorNoteHoverTarget(
  ALinkBlock: TKMemoHyperlink; out ARevision, ANoteId: QWord): Boolean;
var
  NoteInfo: TNoteInfo;
begin
  Result := False;
  ARevision := 0;
  ANoteId := 0;
  if not Assigned(ALinkBlock) or (FViewMode <> cvmChapterText) or
     FPrintMode then
    Exit;

  NoteInfo := FindTranslatorNoteByLink(ALinkBlock);
  if not Assigned(NoteInfo) or
     (NoteInfo.RenderRevision <> FTranslatorNoteRevision) then
    Exit;
  ARevision := NoteInfo.RenderRevision;
  ANoteId := QWord(NoteInfo.Id);
  Result := ANoteId <> 0;
end;

function TChapterView.ResolveTranslatorNoteHover(Sender: TObject;
  ARevision, ANoteId: QWord; out ABodyRaw: UnicodeString;
  out AAnchorScreenRect: TRect): Boolean;
var
  AnchorClientRect: TRect;
  AnchorScreenBottomRight: TPoint;
  AnchorScreenTopLeft: TPoint;
  MarkerRevision: QWord;
  NoteId: QWord;
  NoteInfo: TNoteInfo;
  ViewRect: TRect;
begin
  Result := False;
  ABodyRaw := '';
  AAnchorScreenRect := Rect(0, 0, 0, 0);
  if (ARevision <> FTranslatorNoteRevision) or
     (ANoteId = 0) or not Enabled or not Assigned(FProject) or
     (FViewMode <> cvmChapterText) or FPrintMode then
    Exit;

  NoteInfo := FindTranslatorNoteById(ANoteId);
  if not Assigned(NoteInfo) or
     (NoteInfo.RenderRevision <> ARevision) or
     not TryResolveTranslatorNoteHoverTarget(NoteInfo.MarkerLink,
       MarkerRevision, NoteId) or
     (MarkerRevision <> ARevision) or
     (NoteId <> ANoteId) then
    Exit;

  AnchorClientRect := BlockRect(NoteInfo.MarkerLink);
  ViewRect := ClientRect;
  AnchorClientRect.Left := Max(AnchorClientRect.Left, ViewRect.Left);
  AnchorClientRect.Top := Max(AnchorClientRect.Top, ViewRect.Top);
  AnchorClientRect.Right := Min(AnchorClientRect.Right, ViewRect.Right);
  AnchorClientRect.Bottom := Min(AnchorClientRect.Bottom, ViewRect.Bottom);
  if (AnchorClientRect.Right <= AnchorClientRect.Left) or
     (AnchorClientRect.Bottom <= AnchorClientRect.Top) then
    Exit;

  AnchorScreenTopLeft := ClientToScreen(
    Point(AnchorClientRect.Left, AnchorClientRect.Top));
  AnchorScreenBottomRight := ClientToScreen(
    Point(AnchorClientRect.Right, AnchorClientRect.Bottom));
  AAnchorScreenRect := Rect(AnchorScreenTopLeft.X, AnchorScreenTopLeft.Y,
    AnchorScreenBottomRight.X, AnchorScreenBottomRight.Y);
  ABodyRaw := UTF8Decode(NoteInfo.Text);
  Result := True;
end;

procedure TChapterView.ShowTranslatorNotePreview(APreview: TNoteWindow;
  const ABodyRaw: UnicodeString);
begin
  if not Assigned(APreview) then
    Exit;
  APreview.ActivateHint(UTF8Encode(ABodyRaw), TextStyle);
end;

procedure TChapterView.DismissTranslationMemoryPreview;
begin
  if Assigned(FTranslationMemoryPreviewController) then
    FTranslationMemoryPreviewController.Dismiss;
end;

procedure TChapterView.InvalidateTranslationMemoryPreview;
begin
  Inc(FTranslationMemoryPreviewGeneration);
  SetLength(FTranslationMemoryCounterLinks, 0);
  if Assigned(FTranslationMemoryPreviewController) then
    FTranslationMemoryPreviewController.Invalidate;
end;

function TChapterView.TryResolveTranslationMemoryHoverTarget(
  ALinkBlock: TKMemoHyperlink;
  out ATarget: TTranslationMemoryHoverTarget): Boolean;
var
  AlternativeIndex: Integer;
  EntryIndex: Integer;
begin
  Result := False;
  ATarget := Default(TTranslationMemoryHoverTarget);
  if not Assigned(ALinkBlock) or
     not TryParseTranslationMemoryHoverURL(
       ALinkBlock.URL, EntryIndex, AlternativeIndex) or
     (EntryIndex < 0) or
     (EntryIndex >= Length(FTranslationMemoryEntries)) or
     (AlternativeIndex < 0) or
     (AlternativeIndex >= Length(
       FTranslationMemoryEntries[EntryIndex].Alternatives)) or
     (EntryIndex >= Length(FTranslationMemoryCounterLinks)) or
     (AlternativeIndex >= Length(
       FTranslationMemoryCounterLinks[EntryIndex])) or
     (FTranslationMemoryCounterLinks[EntryIndex][AlternativeIndex] <>
       ALinkBlock) then
    Exit;

  with FTranslationMemoryEntries[EntryIndex].Alternatives[
    AlternativeIndex] do
    if (Occurrences <= 0) or (Length(Matches) = 0) then
      Exit;

  ATarget.Generation := FTranslationMemoryPreviewGeneration;
  ATarget.EntryIndex := EntryIndex;
  ATarget.AlternativeIndex := AlternativeIndex;
  Result := True;
end;

function TChapterView.ValidateTranslationMemoryPreviewTarget(
  const ATarget: TTranslationMemoryHoverTarget;
  out AAnchorScreenRect: TRect): Boolean;
var
  AnchorClientRect: TRect;
  AnchorScreenBottomRight: TPoint;
  AnchorScreenTopLeft: TPoint;
  AlternativeIndex: Integer;
  CurrentSnapshot: TTranslationMemoryInputSnapshot;
  EntryIndex: Integer;
  LinkBlock: TKMemoHyperlink;
  ViewRect: TRect;
begin
  Result := False;
  AAnchorScreenRect := Rect(0, 0, 0, 0);
  if not Assigned(FProject) or not IsTranslationMemoryVisible or
     FTranslationMemoryShowingOccurrences or FTranslationMemoryRefreshPending or
     (ATarget.Generation <> FTranslationMemoryPreviewGeneration) then
    Exit;

  CurrentSnapshot := CurrentTranslationMemoryInputSnapshot;
  if not TranslationMemoryInputSnapshotsEqual(
    FTranslationMemoryInputSnapshot, CurrentSnapshot) then
    Exit;

  EntryIndex := ATarget.EntryIndex;
  AlternativeIndex := ATarget.AlternativeIndex;
  if (EntryIndex < 0) or
     (EntryIndex >= Length(FTranslationMemoryEntries)) or
     (AlternativeIndex < 0) or
     (AlternativeIndex >= Length(
       FTranslationMemoryEntries[EntryIndex].Alternatives)) or
     (EntryIndex >= Length(FTranslationMemoryCounterLinks)) or
     (AlternativeIndex >= Length(
       FTranslationMemoryCounterLinks[EntryIndex])) then
    Exit;
  with FTranslationMemoryEntries[EntryIndex].Alternatives[
    AlternativeIndex] do
    if (Occurrences <= 0) or (Length(Matches) = 0) then
      Exit;

  LinkBlock := FTranslationMemoryCounterLinks[EntryIndex][AlternativeIndex];
  if not Assigned(LinkBlock) or
     not TryParseTranslationMemoryHoverURL(LinkBlock.URL, EntryIndex,
       AlternativeIndex) or
     (EntryIndex <> ATarget.EntryIndex) or
     (AlternativeIndex <> ATarget.AlternativeIndex) then
    Exit;

  AnchorClientRect := BlockRect(LinkBlock);
  ViewRect := ClientRect;
  AnchorClientRect.Left := Max(AnchorClientRect.Left, ViewRect.Left);
  AnchorClientRect.Top := Max(AnchorClientRect.Top, ViewRect.Top);
  AnchorClientRect.Right := Min(AnchorClientRect.Right, ViewRect.Right);
  AnchorClientRect.Bottom := Min(AnchorClientRect.Bottom, ViewRect.Bottom);
  if (AnchorClientRect.Right <= AnchorClientRect.Left) or
     (AnchorClientRect.Bottom <= AnchorClientRect.Top) then
    Exit;

  AnchorScreenTopLeft := ClientToScreen(
    Point(AnchorClientRect.Left, AnchorClientRect.Top));
  AnchorScreenBottomRight := ClientToScreen(
    Point(AnchorClientRect.Right, AnchorClientRect.Bottom));
  AAnchorScreenRect := Rect(AnchorScreenTopLeft.X, AnchorScreenTopLeft.Y,
    AnchorScreenBottomRight.X, AnchorScreenBottomRight.Y);
  Result := True;
end;

{$IFDEF FPCUNIT}
procedure TChapterView.ConfigureTranslationMemoryInteractionFixture(
  const AEntries: TTranslationMemoryEntryArray;
  const ACounterLinks: TTranslationMemoryCounterLinkMatrix);
var
  AlternativeIndex: Integer;
  EntryIndex: Integer;
begin
  InvalidateTranslationMemoryPreview;
  ResetTranslationMemoryOccurrences;
  FTranslationMemoryEntries := Copy(AEntries, 0, Length(AEntries));
  FTranslationMemoryCounterLinks := Copy(
    ACounterLinks, 0, Length(ACounterLinks));
  FTranslationMemoryInputSnapshot := CurrentTranslationMemoryInputSnapshot;
  FTranslationMemoryRefreshPending := False;
  for EntryIndex := 0 to High(FTranslationMemoryCounterLinks) do
    for AlternativeIndex := 0 to
      High(FTranslationMemoryCounterLinks[EntryIndex]) do
      if Assigned(
        FTranslationMemoryCounterLinks[EntryIndex][AlternativeIndex]) then
        FTranslationMemoryCounterLinks[EntryIndex][AlternativeIndex].OnClick :=
          @HandleTranslationMemoryAlternativeClick;
end;

function TChapterView.ResolveTranslationMemoryHoverTargetFixture(
  ALinkBlock: TKMemoHyperlink;
  out ATarget: TTranslationMemoryHoverTarget): Boolean;
begin
  Result := TryResolveTranslationMemoryHoverTarget(ALinkBlock, ATarget);
end;

procedure TChapterView.BeginTranslationMemoryHoverFixture(
  const ATarget: TTranslationMemoryHoverTarget);
begin
  FTranslationMemoryPreviewController.Hover(ATarget);
end;

function TChapterView.TranslationMemoryPreviewActiveFixture: Boolean;
begin
  Result := Assigned(FTranslationMemoryPreviewController) and
    FTranslationMemoryPreviewController.Active;
end;

function TChapterView.ResolveTranslatorNoteHoverTargetFixture(
  ALinkBlock: TKMemoHyperlink; out ARevision, ANoteId: QWord): Boolean;
begin
  Result := TryResolveTranslatorNoteHoverTarget(ALinkBlock,
    ARevision, ANoteId);
  if not Result then
  begin
    ARevision := 0;
    ANoteId := 0;
  end;
end;

procedure TChapterView.RenderTranslatorNotesFixture;
begin
  RenderNotes;
end;

procedure TChapterView.BeginTranslatorNoteHoverFixture(ANoteId: QWord);
begin
  FTranslatorNoteHoverController.Hover(FTranslatorNoteRevision, ANoteId);
end;

procedure TChapterView.InvalidateTranslatorNotePreviewFixture;
begin
  InvalidateTranslatorNotePreview;
end;

function TChapterView.TranslatorNotePreviewActiveFixture: Boolean;
begin
  Result := Assigned(FTranslatorNoteHoverController) and
    FTranslatorNoteHoverController.Active;
end;

function TChapterView.TranslatorNoteRevisionFixture: QWord;
begin
  Result := FTranslatorNoteRevision;
end;

procedure TChapterView.ClearTranslatorNotesFixture;
begin
  ClearTranslatorNotes;
end;

{$IFDEF FPCUNIT}
class function TChapterView.TranslatorNoteLiveCountFixture: Integer;
begin
  Result := TranslatorNoteInfoLiveCount;
end;
{$ENDIF}

function TChapterView.ChapterFullRenderCountFixture: QWord;
begin
  Result := FRenderedChapterFullRenderCount;
end;

procedure TChapterView.ReadTranslationMemoryOccurrenceStateFixture(
  out AMatches: TTranslationPairMatchArray; out ALimit: Integer;
  out AShowingOccurrences: Boolean);
begin
  AMatches := Copy(FTranslationMemoryOccurrenceMatches, 0,
    Length(FTranslationMemoryOccurrenceMatches));
  ALimit := FTranslationMemoryOccurrenceLimit;
  AShowingOccurrences := FTranslationMemoryShowingOccurrences;
end;

procedure TChapterView.ConfigureTranslationMemoryOccurrencePageFixture(
  const APage: TTranslationOccurrencePage);
var
  CurrentBlockIndex: Integer;
begin
  CurrentBlockIndex := -1;
  FTranslationMemoryShowingOccurrences := True;
  FTranslationMemoryOccurrenceLimit := 100;
  FTranslationMemoryInputSnapshot := CurrentTranslationMemoryInputSnapshot;
  Blocks.LockUpdate;
  try
    Blocks.Clear;
    ClearTranslationMemoryOccurrenceCache;
    AppendTranslationMemoryOccurrencePage(APage);
    AppendTranslationMemoryOccurrenceItems(APage.Items, CurrentBlockIndex);
    AppendTranslationMemoryShowMoreBlock;
  finally
    Blocks.UnLockUpdate;
  end;
end;

procedure TChapterView.AppendTranslationMemoryOccurrencePageFixture(
  const APage: TTranslationOccurrencePage);
var
  CurrentBlockIndex: Integer;
begin
  CurrentBlockIndex := -1;
  Blocks.LockUpdate;
  try
    RemoveTranslationMemoryShowMoreBlock;
    AppendTranslationMemoryOccurrenceItems(APage.Items, CurrentBlockIndex);
    AppendTranslationMemoryOccurrencePage(APage);
    AppendTranslationMemoryShowMoreBlock;
  finally
    Blocks.UnLockUpdate;
  end;
end;

procedure TChapterView.ClearTranslationMemoryOccurrenceCacheFixture;
begin
  ClearTranslationMemoryOccurrenceCache;
end;

function TChapterView.TranslationMemoryCachedOccurrenceCountFixture: Integer;
begin
  Result := Length(FTranslationMemoryOccurrenceCache.Items);
end;

function TChapterView.TranslationMemoryShowMoreBlockIndexFixture: Integer;
begin
  Result := FTranslationMemoryShowMoreBlockIndex;
end;
{$ENDIF}

function TChapterView.LoadTranslationMemoryPreviewPayload(
  const ATarget: TTranslationMemoryHoverTarget;
  out APayload: TTranslationMemoryPreviewPayload): Boolean;
var
  Alternative: TTranslationMemoryAlternative;
  AnchorScreenRect: TRect;
  ErrorMessage: string;
  Remaining: Integer;
begin
  Result := False;
  APayload := Default(TTranslationMemoryPreviewPayload);
  if not ValidateTranslationMemoryPreviewTarget(ATarget, AnchorScreenRect) then
    Exit;

  Alternative := FTranslationMemoryEntries[ATarget.EntryIndex].Alternatives[
    ATarget.AlternativeIndex];
  APayload.SourceTerm :=
    FTranslationMemoryEntries[ATarget.EntryIndex].DisplayTerm;
  APayload.Translation := Alternative.Translation;
  APayload.TitleText := Format(STranslationMemoryOccurrencesTitleFmt,
    [APayload.SourceTerm, APayload.Translation]);
  APayload.NoOccurrencesText := STranslationMemoryNoOccurrences;
  APayload.OccurrenceCountFormat := STranslationMemoryOccurrenceCountFmt;
  APayload.CurrentReferenceID := FProject.ID;
  APayload.FontName := TextStyle.Font.Name;
  APayload.FontSize := FTranslationMemoryPreviewFontSize;

  if not FProject.TryGetReadyTranslationMemoryOccurrences(
    Alternative.Matches, TRANSLATION_MEMORY_PREVIEW_LIMIT, APayload.Page,
    ErrorMessage) then
  begin
    APayload.ErrorMessage := STranslationMemoryPreviewUnavailable;
    Exit(True);
  end;

  APayload.TotalsText := TranslationMemoryOccurrenceTotalsText(
    APayload.Page.TotalAssociations, APayload.Page.TotalVerses);
  Remaining := APayload.Page.TotalVerses - Length(APayload.Page.Items);
  if Remaining > 0 then
    APayload.FooterText := Format(
      STranslationMemoryPreviewRemainingFmt, [Remaining]);
  Result := True;
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
                '>','0','1':
                begin
                  Font.Size := Font.Size + 2;
                  Font.Style := [fsBold];
                  Font.Color := $424242;
                end;
                '2':
                begin
                  Font.Size := Font.Size + 1;
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
            PopStyle;
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
  LinkBlock: TKMemoHyperlink;
  NoteInfo: TNoteInfo;
begin
  if not FNoteNumberer.ResolveMarker(UnicodeString(OpeningTag), Link) then
    Exit;
  if Link = '' then
    Exit;

  NoteInfo := TNoteInfo.Create(FNoteID, FCurrentVerse, Note, string(Link));
  NoteInfo.RenderRevision := FTranslatorNoteRevision;
  FNotes.Add(NoteInfo);
  if assigned(Blocks.LastBlock) and not Blocks.LastBlock.Text.EndsWith(' ') then
    Link := ' ' + Link;

  LinkBlock := Blocks.AddHyperlink(string(Link), (FNoteID-1).ToString);
  NoteInfo.MarkerLink := LinkBlock;
  IndexTranslatorNote(NoteInfo);
  with LinkBlock do
  begin
    TextStyle.Assign(TextStyle);
    TextStyle.Font.Color := NoteLinkColor;
    TextStyle.Font.Style := [fsItalic];
    TextStyle.Font.Size := TextStyle.Font.Size + 1;
    TextStyle.ScriptPosition := tpoSuperscript;
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
  DismissTranslatorNotePreview;
  FProject.NavigateToReference(TKMemoHyperlink(Sender).URL);
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
  DismissTranslationMemoryPreview;
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
  CurrentSnapshot: TTranslationMemoryInputSnapshot;
  EntryIndex: Integer;
begin
  DismissTranslationMemoryPreview;
  if not (Sender is TKMemoHyperlink) then
    Exit;
  CurrentSnapshot := CurrentTranslationMemoryInputSnapshot;
  if not TranslationMemoryInputSnapshotsEqual(
    FTranslationMemoryInputSnapshot, CurrentSnapshot) then
  begin
    FTranslationMemoryInputSnapshot := CurrentSnapshot;
    ResetTranslationMemoryOccurrences(True);
    RenderTranslationMemory;
    TopPos := 0;
    Exit;
  end;
  if not TryParseTranslationMemoryHoverURL(
    TKMemoHyperlink(Sender).URL, EntryIndex, AlternativeIndex) then
    Exit;
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
  ClearTranslationMemoryOccurrenceCache;
  FTranslationMemoryOccurrenceLimit := 100;
  FTranslationMemoryShowingOccurrences := True;
  ClearTranslationMemorySourceHighlight;
  RenderTranslationMemoryOccurrences;
  TopPos := 0;
end;

procedure TChapterView.HandleTranslationMemoryBackClick(Sender: TObject);
begin
  ResetTranslationMemoryOccurrences(True);
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
  FProject.NavigateToReference(ReferenceID);
end;

procedure TChapterView.HandleTranslationMemoryShowMoreClick(Sender: TObject);
var
  CurrentBlockIndex: Integer;
  CurrentSnapshot: TTranslationMemoryInputSnapshot;
  ErrorMessage: string;
  NextPage: TTranslationOccurrencePage;
  Offset: Integer;
  PreviousTop: Integer;
begin
  if not FTranslationMemoryShowingOccurrences or
     not FTranslationMemoryOccurrenceCacheLoaded or
     not FTranslationMemoryOccurrenceCache.HasMore then
    Exit;
  CurrentSnapshot := CurrentTranslationMemoryInputSnapshot;
  if not TranslationMemoryInputSnapshotsEqual(
    FTranslationMemoryInputSnapshot, CurrentSnapshot) then
  begin
    FTranslationMemoryInputSnapshot := CurrentSnapshot;
    ResetTranslationMemoryOccurrences(True);
    RenderTranslationMemory;
    TopPos := 0;
    Exit;
  end;
  PreviousTop := TopPos;
  Offset := Length(FTranslationMemoryOccurrenceCache.Items);
  if not TryLoadTranslationMemoryOccurrenceRange(
    FTranslationMemoryOccurrenceMatches, Offset, 100, NextPage,
    ErrorMessage) then
    Exit;
  if (NextPage.TotalAssociations <>
      FTranslationMemoryOccurrenceCache.TotalAssociations) or
     (NextPage.TotalVerses <>
      FTranslationMemoryOccurrenceCache.TotalVerses) then
  begin
    ClearTranslationMemoryOccurrenceCache;
    RenderTranslationMemoryOccurrences;
    TopPos := PreviousTop;
    Exit;
  end;

  CurrentBlockIndex := -1;
  Blocks.LockUpdate;
  try
    RemoveTranslationMemoryShowMoreBlock;
    AppendTranslationMemoryOccurrenceItems(NextPage.Items,
      CurrentBlockIndex);
    AppendTranslationMemoryOccurrencePage(NextPage);
    AppendTranslationMemoryShowMoreBlock;
  finally
    Blocks.UnLockUpdate;
  end;
  Inc(FTranslationMemoryOccurrenceLimit, 100);
  TopPos := PreviousTop;
end;

procedure TChapterView.ResetTranslationMemoryOccurrences(
  AWillReplaceDocument: Boolean);
begin
  if Assigned(FTranslationMemoryFocus) then
    if AWillReplaceDocument then
      FTranslationMemoryFocus.Discard
    else
      FTranslationMemoryFocus.Unbind;
  FTranslationMemoryShowingOccurrences := False;
  SetLength(FTranslationMemoryOccurrenceMatches, 0);
  FTranslationMemoryOccurrenceSource := '';
  FTranslationMemoryOccurrenceTranslation := '';
  FTranslationMemoryOccurrenceLimit := 100;
  ClearTranslationMemoryOccurrenceCache;
end;

procedure TChapterView.HandleVerseChange(Sender: TProject);
var
  verses: TStringList;
begin
  if not assigned(FProject) or (Width < 2) then
  begin
    InvalidateTranslatorNotePreview;
    InvalidateTranslationMemoryPreview;
    if Assigned(FTranslationMemoryFocus) then
      FTranslationMemoryFocus.Unbind;
    exit;
  end;

  ClearTranslationMemorySourceHighlight;
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
  if ChapterRenderKeyMatchesCurrentProject then
  begin
    FRawMemo.Hide;
    DismissTranslatorNotePreview;
    DismissTranslationMemoryPreview;
    FocusRenderedChapterVerse(Sender.Verse);
    Exit;
  end;
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
  if Key in [VK_ESCAPE, VK_PRIOR, VK_NEXT, VK_HOME, VK_END, VK_UP, VK_DOWN,
     VK_LEFT, VK_RIGHT] then
  begin
    DismissTranslatorNotePreview;
    DismissTranslationMemoryPreview;
  end;
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
var
  HitBlock: TKMemoBlock;
  HitRect: TRect;
  LinkBlock: TKMemoHyperlink;
  NoteId: QWord;
  NoteRevision: QWord;
  Target: TTranslationMemoryHoverTarget;
begin
  LinkBlock := nil;
  HitBlock := BlockAt(Point(X, Y));
  if HitBlock is TKMemoHyperlink then
  begin
    HitRect := BlockRect(HitBlock);
    if (X >= HitRect.Left) and (X < HitRect.Right) and
       (Y >= HitRect.Top) and (Y < HitRect.Bottom) then
      LinkBlock := TKMemoHyperlink(HitBlock);
  end;

  if Assigned(FTranslatorNoteHoverController) then
    if TryResolveTranslatorNoteHoverTarget(LinkBlock,
      NoteRevision, NoteId) then
      FTranslatorNoteHoverController.Hover(
        NoteRevision, NoteId)
    else
      FTranslatorNoteHoverController.Leave;

  if not Assigned(FTranslationMemoryPreviewController) or
     not IsTranslationMemoryVisible or FTranslationMemoryShowingOccurrences then
    Exit;

  if not TryResolveTranslationMemoryHoverTarget(LinkBlock, Target) then
    FTranslationMemoryPreviewController.Leave
  else
    FTranslationMemoryPreviewController.Hover(Target);
end;

procedure TChapterView.HandleMouseLeave(Sender: TObject);
begin
  if Assigned(FTranslatorNoteHoverController) then
    FTranslatorNoteHoverController.Leave;
  if Assigned(FTranslationMemoryPreviewController) then
    FTranslationMemoryPreviewController.Leave;
end;

procedure TChapterView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  DismissTranslatorNotePreview;
  DismissTranslationMemoryPreview;
  if ssCtrl in Shift then
  begin
    Blocks.LockUpdate;
    FontSize := FFontSize + Sign(WheelDelta) * 1;
    LoadChapter;
    Blocks.UnlockUpdate;
    Handled := true;
  end;
end;

procedure TChapterView.HandleTranslationMemoryPreviewFontSizeChanged(
  Sender: TObject; AFontSize: Integer);
begin
  TranslationMemoryPreviewFontSize := AFontSize;
end;

procedure TChapterView.HandleDoubleClick(Sender: TObject);
var
  verses: TStringList;
begin
  DismissTranslatorNotePreview;
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
  DismissTranslatorNotePreview;
  DismissTranslationMemoryPreview;
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
  DismissTranslatorNotePreview;
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
  DismissTranslatorNotePreview;
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
  DismissTranslatorNotePreview;
  InterlinearVerseRulesForm.Project := FProject;
  if InterlinearVerseRulesForm.ShowModal = mrOK then
  begin
    InvalidateChapterRenderCache;
    LoadChapter;
  end;
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
  InvalidateChapterRenderCache;
  InvalidateTranslatorNotePreview;
  InvalidateTranslationMemoryPreview;
  ClearTranslationMemorySourceHighlight;
  ResetTranslationMemoryOccurrences;
  if Assigned(FProject) then
    FProject.OnNewVerseUnsubscribe(@HandleVerseChange);
  FProject := AValue;
  FTranslationMemoryRefreshPending := True;
  FTranslationMemoryInputSnapshot :=
    Default(TTranslationMemoryInputSnapshot);
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
  InvalidateTranslationMemoryPreview;
  if FViewMode = AValue then
  begin
    DismissTranslatorNotePreview;
    if (AValue = cvmTranslationMemory) and
       FTranslationMemoryRefreshPending and
       IsTranslationMemoryVisible then
      LoadChapter;
    Exit;
  end;
  InvalidateTranslatorNotePreview;
  ClearTranslationMemorySourceHighlight;
  if Assigned(FTranslationMemoryFocus) then
    if Assigned(FProject) then
      FTranslationMemoryFocus.Discard
    else
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
  InvalidateTranslationMemoryPreview;
  if Assigned(FTranslationMemoryFocus) and
     not IsTranslationMemoryVisible then
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

  DismissTranslatorNotePreview;
  Blocks.LockUpdate;
  FFontSize := AValue;
  TextStyle.Font.Size := FFontSize;
  Blocks.UnlockUpdate;
end;

procedure TChapterView.SetFontName(AValue: string);
begin
  if (FFontName = AValue) then Exit;

  DismissTranslatorNotePreview;
  Blocks.LockUpdate;
  FFontName := AValue;
  TextStyle.Font.Name := FFontName;
  Blocks.UnlockUpdate;
end;

procedure TChapterView.SetTranslationMemoryPreviewFontSize(AValue: Integer);
var
  NormalizedSize: Integer;
begin
  NormalizedSize := NormalizeTranslationMemoryPreviewFontSize(AValue);
  if FTranslationMemoryPreviewFontSize = NormalizedSize then
    Exit;

  FTranslationMemoryPreviewFontSize := NormalizedSize;
  if Assigned(FOnTranslationMemoryPreviewFontSizeChanged) then
    FOnTranslationMemoryPreviewFontSizeChanged(Self);
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
  InvalidateTranslatorNotePreview;
  InvalidateTranslationMemoryPreview;
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

procedure TChapterView.Resize;
begin
  DismissTranslatorNotePreview;
  inherited Resize;
end;

procedure TChapterView.WndProc(var TheMessage: TLMessage);
begin
  if (TheMessage.msg = LM_HSCROLL) or
     (TheMessage.msg = LM_VSCROLL) then
    DismissTranslatorNotePreview;
  inherited WndProc(TheMessage);
end;

procedure TChapterView.SetEnabled(Value: Boolean);
begin
  if not Value then
  begin
    InvalidateTranslatorNotePreview;
    InvalidateTranslationMemoryPreview;
  end;
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
    FTranslationMemoryInputSnapshot :=
      Default(TTranslationMemoryInputSnapshot);
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
  OnMouseMove  := @HandleMouseMove;
  OnMouseLeave := @HandleMouseLeave;
  OnContextPopup := @HandleContextPopup;
  FFontName       := 'default';
  FFontSize       := 10;
  FTranslationMemoryPreviewFontSize :=
    NormalizeTranslationMemoryPreviewFontSize(FFontSize);
  FProject        := nil;
  FRenderedChapterVerses := TStringList.Create;
  FRenderedChapterCurrentVerse := 0;
  FRenderedChapterValid := False;
  FRxVerseHeading := RegexCreate('^((?:<TS[0-3]?>.*?<Ts>)*)(.*?)$', [rcoUTF8]);
  FNotes          := TNoteList.Create(True);
  FNotesById      := TStringList.Create;
  FNotesById.Sorted := True;
  FNotesById.Duplicates := dupError;
  FNotesByLink    := TStringList.Create;
  FNotesByLink.Sorted := True;
  FNotesByLink.Duplicates := dupError;
  FNoteID         := 1;
  FNoteNumberer   := TRFNoteNumberer.Create;
  FTranslatorNoteRevision := 1;
  FTranslatorNoteHoverController :=
    TChapterTranslatorNoteHoverController.Create(Self, Self);
  FTranslatorNoteHoverController.OnResolve :=
    @ResolveTranslatorNoteHover;
  FHideVerseNumber:= false;
  FPrintMode      := false;
  FViewMode       := cvmChapterText;
  FTranslationMemoryOrder := tmoVerseOccurrence;
  SetLength(FTranslationMemoryEntries, 0);
  SetLength(FTranslationMemoryFocusEntries, 0);
  FTranslationMemoryFocus := TTranslationMemoryFocusController.Create;
  FTranslationMemoryInputSnapshot :=
    Default(TTranslationMemoryInputSnapshot);
  FTranslationMemoryShowingOccurrences := False;
  SetLength(FTranslationMemoryOccurrenceMatches, 0);
  FTranslationMemoryOccurrenceLimit := 100;
  ClearTranslationMemoryOccurrenceCache;
  FTranslationMemoryRefreshPending := True;
  FTranslationMemoryPreviewGeneration := 0;
  SetLength(FTranslationMemoryCounterLinks, 0);
  FTranslationMemoryPreviewController :=
    TTranslationMemoryHoverController.Create(Self);
  FTranslationMemoryPreviewController.OnValidateTarget :=
    @ValidateTranslationMemoryPreviewTarget;
  FTranslationMemoryPreviewController.OnLoadPayload :=
    @LoadTranslationMemoryPreviewPayload;
  FTranslationMemoryPreviewController.OnFontSizeChanged :=
    @HandleTranslationMemoryPreviewFontSizeChanged;
  FViewSelectionActive := False;
  InitPopupMenu;

  FRawMemo    := TRawChapterMemo.Create(Parent, Self);
end;

destructor TChapterView.Destroy;
begin
  InvalidateTranslatorNotePreview;
  FreeAndNil(FTranslatorNoteHoverController);
  InvalidateTranslationMemoryPreview;
  FreeAndNil(FTranslationMemoryPreviewController);
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Unbind;
  FreeAndNil(FTranslationMemoryFocus);
  FRawMemo.Free;
  FRenderedChapterVerses.Free;
  FNoteNumberer.Free;
  FNotesByLink.Free;
  FNotesById.Free;
  FNotes.Free;
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
  InvalidateChapterRenderCache;
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
  if FTranslationMemoryShowingOccurrences then
    ClearTranslationMemoryOccurrenceCache;
  if not IsTranslationMemoryVisible then
  begin
    InvalidateTranslationMemoryPreview;
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
  CurrentSnapshot: TTranslationMemoryInputSnapshot;
  SyntagmIndex: Integer;
  SourceVerse: TVerse;
begin
  DismissTranslationMemoryPreview;
  if not Enabled or not Assigned(FProject) or
     (FViewMode <> cvmTranslationMemory) then
  begin
    ClearTranslationMemoryEntryFocus;
    Exit;
  end;

  // A normal selection does not change the memory data. Avoid rebuilding the
  // entire view unless this click actually changed one of its canonical inputs.
  CurrentSnapshot := CurrentTranslationMemoryInputSnapshot;
  if not TranslationMemoryInputSnapshotsEqual(
    FTranslationMemoryInputSnapshot, CurrentSnapshot) then
  begin
    if FTranslationMemoryShowingOccurrences then
    begin
      ResetTranslationMemoryOccurrences(True);
      RenderTranslationMemory;
      TopPos := 0;
    end
    else
      RenderTranslationMemory;
  end;

  if FTranslationMemoryShowingOccurrences then
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

procedure TChapterView.ClearTranslationMemoryOccurrenceCache;
begin
  FTranslationMemoryOccurrenceCache := Default(TTranslationOccurrencePage);
  FTranslationMemoryOccurrenceCacheLoaded := False;
  FTranslationMemoryShowMoreBlockIndex := -1;
end;

procedure TChapterView.AppendTranslationMemoryOccurrencePage(
  const APage: TTranslationOccurrencePage);
var
  ItemIndex: Integer;
  OldCount: Integer;
begin
  OldCount := Length(FTranslationMemoryOccurrenceCache.Items);
  SetLength(FTranslationMemoryOccurrenceCache.Items,
    OldCount + Length(APage.Items));
  for ItemIndex := 0 to High(APage.Items) do
    FTranslationMemoryOccurrenceCache.Items[OldCount + ItemIndex] :=
      APage.Items[ItemIndex];
  FTranslationMemoryOccurrenceCache.TotalAssociations :=
    APage.TotalAssociations;
  FTranslationMemoryOccurrenceCache.TotalVerses := APage.TotalVerses;
  FTranslationMemoryOccurrenceCache.HasMore := APage.HasMore;
  FTranslationMemoryOccurrenceCacheLoaded := True;
end;

procedure TChapterView.AppendTranslationMemoryOccurrenceItems(
  const AItems: TTranslationOccurrenceDataArray;
  var ACurrentBlockIndex: Integer);
var
  ItemIndex: Integer;
  LinkBlock: TKMemoHyperlink;
  TextBlock: TKMemoTextBlock;
begin
  IBibliaPerfIncrement(ipcTranslationMemoryOccurrenceItemsProjected,
    Length(AItems));
  for ItemIndex := 0 to High(AItems) do
    with AItems[ItemIndex] do
    begin
      if Assigned(FProject) and (ReferenceID = FProject.ID) then
        ACurrentBlockIndex := Blocks.Count;
      LinkBlock := Blocks.AddHyperlink(DisplayReference, ReferenceID);
      LinkBlock.OnClick := @HandleTranslationMemoryOccurrenceClick;
      LinkBlock.TextStyle.Font.Color := NoteLinkColor;
      LinkBlock.TextStyle.Font.Style := [fsUnderline];
      if Assigned(FProject) and (ReferenceID = FProject.ID) then
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
      AppendTranslationMemoryOccurrenceText(AItems[ItemIndex]);
      with Blocks.AddParagraph().ParaStyle do
      begin
        FirstIndent := 0;
        BottomPadding := 5;
      end;
    end;
end;

procedure TChapterView.RemoveTranslationMemoryShowMoreBlock;
begin
  if (FTranslationMemoryShowMoreBlockIndex < 0) or
     (FTranslationMemoryShowMoreBlockIndex >= Blocks.Count) then
  begin
    FTranslationMemoryShowMoreBlockIndex := -1;
    Exit;
  end;
  while Blocks.Count > FTranslationMemoryShowMoreBlockIndex do
    Blocks.Delete(Blocks.Count - 1);
  FTranslationMemoryShowMoreBlockIndex := -1;
end;

procedure TChapterView.AppendTranslationMemoryShowMoreBlock;
var
  LinkBlock: TKMemoHyperlink;
  Remaining: Integer;
begin
  FTranslationMemoryShowMoreBlockIndex := -1;
  if not FTranslationMemoryOccurrenceCache.HasMore then
    Exit;
  Remaining := FTranslationMemoryOccurrenceCache.TotalVerses -
    Length(FTranslationMemoryOccurrenceCache.Items);
  if Remaining <= 0 then
    Exit;
  FTranslationMemoryShowMoreBlockIndex := Blocks.Count;
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

procedure TChapterView.RenderTranslationMemoryOccurrences;
var
  BlockBounds: TRect;
  CurrentBlockIndex: Integer;
  CurrentSnapshot: TTranslationMemoryInputSnapshot;
  ErrorMessage: string;
  LinkBlock: TKMemoHyperlink;
  OccurrencePage: TTranslationOccurrencePage;
  TextBlock: TKMemoTextBlock;
begin
  if not IsTranslationMemoryVisible then
  begin
    InvalidateTranslatorNotePreview;
    InvalidateTranslationMemoryPreview;
    if Assigned(FTranslationMemoryFocus) then
      FTranslationMemoryFocus.Unbind;
    FTranslationMemoryRefreshPending := True;
    Exit;
  end;
  CurrentSnapshot := CurrentTranslationMemoryInputSnapshot;
  if ShouldResetTranslationMemoryOccurrences(
    FTranslationMemoryShowingOccurrences,
    FTranslationMemoryInputSnapshot, CurrentSnapshot) then
  begin
    FTranslationMemoryInputSnapshot := CurrentSnapshot;
    ResetTranslationMemoryOccurrences(True);
    RenderTranslationMemory;
    TopPos := 0;
    Exit;
  end;
  IBibliaPerfIncrement(ipcTranslationMemoryViewRenders);
  InvalidateTranslatorNotePreview;
  InvalidateTranslationMemoryPreview;
  InvalidateChapterRenderCache;
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Discard;
  FTranslationMemoryInputSnapshot := CurrentSnapshot;
  FTranslationMemoryRefreshPending := False;
  FRawMemo.Hide;
  ErrorMessage := '';
  if not FTranslationMemoryOccurrenceCacheLoaded and
     TryLoadTranslationMemoryOccurrenceRange(
       FTranslationMemoryOccurrenceMatches, 0,
       FTranslationMemoryOccurrenceLimit, OccurrencePage,
       ErrorMessage) then
    AppendTranslationMemoryOccurrencePage(OccurrencePage)
  else if not FTranslationMemoryOccurrenceCacheLoaded then
  begin
    FTranslationMemoryOccurrenceCache :=
      Default(TTranslationOccurrencePage);
  end;
  OccurrencePage := FTranslationMemoryOccurrenceCache;

  CurrentBlockIndex := -1;
  Blocks.LockUpdate;
  try
    ClearTranslatorNotes;
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

    AppendTranslationMemoryOccurrenceItems(OccurrencePage.Items,
      CurrentBlockIndex);
    AppendTranslationMemoryShowMoreBlock;
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
  HasCurrentAlternative: Boolean;
  SyntagmIndex: Integer;
begin
  if not IsTranslationMemoryVisible then
  begin
    InvalidateTranslatorNotePreview;
    InvalidateTranslationMemoryPreview;
    if Assigned(FTranslationMemoryFocus) then
      FTranslationMemoryFocus.Unbind;
    FTranslationMemoryRefreshPending := True;
    Exit;
  end;
  IBibliaPerfIncrement(ipcTranslationMemoryViewRenders);

  InvalidateTranslatorNotePreview;
  InvalidateTranslationMemoryPreview;
  InvalidateChapterRenderCache;
  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Discard;
  FTranslationMemoryRefreshPending := False;
  FRawMemo.Hide;
  if not FProject.TryGetCurrentVerseTranslationMemory(
    FTranslationMemoryOrder,
    FTranslationMemoryEntries, ErrorMessage) then
    SetLength(FTranslationMemoryEntries, 0);
  CaptureTranslationMemoryInputSnapshot;
  SetLength(FTranslationMemoryFocusEntries,
    Length(FTranslationMemoryEntries));
  SetLength(FTranslationMemoryCounterLinks,
    Length(FTranslationMemoryEntries));
  for EntryIndex := 0 to High(FTranslationMemoryFocusEntries) do
  begin
    SetLength(FTranslationMemoryCounterLinks[EntryIndex],
      Length(FTranslationMemoryEntries[EntryIndex].Alternatives));
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
    ClearTranslatorNotes;
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
              FTranslationMemoryCounterLinks[EntryIndex][AlternativeIndex] :=
                LinkBlock;
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

procedure TChapterView.InvalidateChapterRenderCache;
begin
  FRenderedChapterValid := False;
  FRenderedChapterCurrentVerse := 0;
  if Assigned(FRenderedChapterVerses) then
    FRenderedChapterVerses.Clear;
end;

function TChapterView.ChapterRenderKeyMatchesCurrentProject: Boolean;
var
  Snapshot: TChapterSnapshot;
begin
  Result := FRenderedChapterValid and Assigned(FProject) and
    (FRenderedChapterProject = FProject) and
    (FRenderedChapterBookID = FProject.BookID) and
    (FRenderedChapterChapter = FProject.Chapter) and
    (FRenderedChapterTextType = GetTextType) and
    (FRenderedChapterParagraphMode = GetParagraphMode) and
    (FRenderedChapterInterlinearMode = GetInterlinearMode) and
    (FRenderedChapterFontName = FFontName) and
    (FRenderedChapterFontSize = FFontSize) and
    (FRenderedChapterPrintMode = FPrintMode) and
    (FRenderedChapterHideVerseNumber = FHideVerseNumber) and
    (FRenderedChapterTokenizerVersion = FProject.TokenizerVersion);
  if not Result then
    Exit;

  Snapshot := FProject.GetChapterSnapshot;
  Result := Assigned(Snapshot) and
    (FRenderedChapterContentRevision = Snapshot.ContentRevision);
end;

function TChapterView.ChapterRenderIdentityMatches(
  AVerses: TStrings): Boolean;
var
  Index: Integer;
begin
  Result := ChapterRenderKeyMatchesCurrentProject and Assigned(AVerses) and
    Assigned(FRenderedChapterVerses) and
    (FRenderedChapterVerses.Count = AVerses.Count);
  if not Result then
    Exit;
  for Index := 0 to AVerses.Count - 1 do
    if FRenderedChapterVerses[Index] <> AVerses[Index] then
      Exit(False);
end;

procedure TChapterView.CaptureChapterRenderIdentity(AVerses: TStrings);
var
  Snapshot: TChapterSnapshot;
begin
  FRenderedChapterValid := False;
  if not Assigned(FProject) or not Assigned(AVerses) then
    Exit;
  Snapshot := FProject.GetChapterSnapshot;
  if not Assigned(Snapshot) then
    Exit;

  FRenderedChapterProject := FProject;
  FRenderedChapterBookID := FProject.BookID;
  FRenderedChapterChapter := FProject.Chapter;
  FRenderedChapterContentRevision := Snapshot.ContentRevision;
  FRenderedChapterTextType := GetTextType;
  FRenderedChapterParagraphMode := GetParagraphMode;
  FRenderedChapterInterlinearMode := GetInterlinearMode;
  FRenderedChapterFontName := FFontName;
  FRenderedChapterFontSize := FFontSize;
  FRenderedChapterPrintMode := FPrintMode;
  FRenderedChapterHideVerseNumber := FHideVerseNumber;
  FRenderedChapterTokenizerVersion := FProject.TokenizerVersion;
  FRenderedChapterVerses.Assign(AVerses);
  FRenderedChapterValid := True;
end;

procedure TChapterView.FocusRenderedChapterVerse(ACurrentVerse: Integer);
begin
  if FPrintMode or (ACurrentVerse <= 0) or
     (ACurrentVerse >= Length(FVerseRanges)) or
     (FVerseRanges[ACurrentVerse].first < 0) or
     (FVerseRanges[ACurrentVerse].first >= Blocks.Count) then
    Exit;

  Blocks.LockUpdate;
  try
    if (FRenderedChapterCurrentVerse > 0) and
       (FRenderedChapterCurrentVerse < Length(FVerseRanges)) then
      HighlightRange(FVerseRanges[FRenderedChapterCurrentVerse], clDefault);
    HighlightRange(FVerseRanges[ACurrentVerse], clSilver);
    FRenderedChapterCurrentVerse := ACurrentVerse;
  finally
    Blocks.UnLockUpdate;
  end;

  SelectBlock(Blocks[FVerseRanges[ACurrentVerse].first], sgpTopLeft);
  SelLength := 0;
  ExecuteCommand(ecScrollCenter);
end;

procedure TChapterView.RenderChapter(verses: TStringList; current: integer);
var
  verse: string;
  //starttime: DWord;
begin
  FRawMemo.Hide;
  if ChapterRenderIdentityMatches(verses) then
  begin
    DismissTranslatorNotePreview;
    DismissTranslationMemoryPreview;
    FocusRenderedChapterVerse(current);
    Exit;
  end;

  InvalidateTranslatorNotePreview;
  InvalidateTranslationMemoryPreview;
  InvalidateChapterRenderCache;
  Inc(FRenderedChapterFullRenderCount);
  IBibliaPerfIncrement(ipcChapterRenders);
  //starttime := getTickCount;
  SetLength(FVerseRanges, verses.Count + 1);

  if Assigned(FTranslationMemoryFocus) then
    FTranslationMemoryFocus.Discard;
  ClearTranslatorNotes;
  Blocks.LockUpdate;
  try
    Blocks.Clear;
    ResetStyleStack;

    FNoteID := 1;
    FNoteNumberer.Reset;

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
    //RenderNotes;
  finally
    Blocks.UnLockUpdate;
  end;

  CaptureChapterRenderIdentity(verses);
  FocusRenderedChapterVerse(current);

  //DebugLn('  TChapterView.RenderChapter: %d milliseconds', [getTickCount-starttime]);
end;

end.

