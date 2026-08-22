unit Verse;

{$mode objfpc}{$H+}

{TODO -cObrigatório: Rever algoritmo de associação/desassociação }

{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, StrUtils, ExtCtrls, StdCtrls, Controls, Graphics,
  ONTTokenizer, Syntagm, Forms, LCLType, Math, LCLProc, Dialogs, LazUTF8,
  ONTParser, dbugintf, Menus, Clipbrd, PCRE, ai_debug, text_direction,
  verse_view_intf, text_edit_provenance, verse_visual_adapter,
  translator_note_hover_controller;

type

  TVerse = class;

  TOnSyntagmEvent = procedure (Sender: TSyntagm) of object;
  TOnVerseChangedEvent = procedure () of object;
  TOnTextDirectionChangeEvent = procedure(Sender: TVerse;
    Direction: TTextDirection) of object;
  TOnExportTextEvent = procedure (Sender: TVerse) of object;
  TOnTranslatorNoteClickEvent = procedure(Sender: TVerse; ANoteId,
    ARevision: QWord) of object;

  TStrongsCountMode = (scNone, scCountWords, scCountStrongs);

  TSyntagmIndexArray = array of Integer;

  TAssociationPairData = record
    SourcePhrase: string;
    TargetPhrase: string;
    SourceSyntagmIndexes: TSyntagmIndexArray;
    TargetSyntagmIndexes: TSyntagmIndexArray;
  end;

  TAssociationPairDataArray = array of TAssociationPairData;

  TAssociationEditImpact = record
    PreservedMembers: Integer;
    RemovedMembers: Integer;
    UsedLegacyFallback: Boolean;
    ErrorMessage: string;
  end;

  { TVerse }

  TVerse = class
  private
    FReference: string;
    FReferenceID: string;
    FBaseDirection: TTextDirection;
    FVerseView: IVerseView;
    FVerseViewControl: TCustomControl;
    FVisualRevision: QWord;
    FVisualUpdatesSuspended: Integer;
    FVisualRefreshQueued: Boolean;
    FLastHoverLogicalIndex: Integer;
    FAutomaticNoteStart: Integer;
    FNoteHover: TTranslatorNoteHoverController;
    FRenderedTranslatorNotes: TVerseRenderedTranslatorNoteArray;
    FDisplayTags: Boolean;
    FOnTextDirectionChange: TOnTextDirectionChangeEvent;
    { Private declarations }
    FShowHints: boolean;
    FOnVerseChanged: TOnVerseChangedEvent;
    //FStrongMorfoComoChave: boolean;
    FPairedVerse: TVerse;
    FSyntagms: TSyntagmList;
    FStrongCount: TSyntagm;
    FSelection: TSyntagmList;
    FModified: boolean;
    FChangeNotificationsSuspended: Integer;
    FXMLModified: boolean;
    FPanel: TScrollBox;
    FEdit: TEdit;
    FActive: boolean;
    FDestroying: boolean;
    FRendering: Boolean;
    FShowError: boolean;
    FBoldWordsWithStrong: boolean;
    FStrongsCountMode: TStrongsCountMode;
    //FFontePadrao: TFont;
    FAssociatedColor: TColor;
    FUnassociatedColor: TColor;
    FOnSyntagmClick: TOnSyntagmEvent;
    FOnSyntagmMouseEnter: TOnSyntagmEvent;
    FOnSyntagmMouseLeave: TOnSyntagmEvent;
    FOnExportText: TOnExportTextEvent;
    FOnTranslatorNoteClick: TOnTranslatorNoteClickEvent;
    FXML: string;
    FONTParser: TONTParser;
    FTokenizerVersion: Integer;
    FSyntagmPopupMenu: TPopupMenu;
    FVersePopupMenu: TPopupMenu;
    FRTLMenuItem: TMenuItem;
    FFixWrongFiTag: IRegex;
    FReadOnly: Boolean;
    { determines the auto select behaviour on right-click association }
    FNextAutoSelectWordMustHaveStrongMorpho: Boolean;
    // function CompareSyntagmsInFSyntagms(Item1, Item2: Pointer): Integer; // Removed
    //FOnNovaAssociacao: TOnAssociacaoEvent;
    //FOnRemoverAssociacao: TOnAssociacaoEvent;
    function GetAssociationProgress: Single;
    function GetActive: boolean;
    function GetFont: TFont;
    function GetPairs: string;
    function GetPlainText: string;
    procedure SetActive(const AValue: boolean);
    procedure SetAutomaticNoteStart(AValue: Integer);
    function GetRightToLeft: Boolean;
    procedure SetFont(const AValue: TFont);
    procedure SetModified(const AValue: boolean);
    procedure NotifyVerseChanged;
    procedure SetRightToLeft(AValue: Boolean);
    procedure SetBaseDirection(AValue: TTextDirection);
    procedure RefreshVisualDocument(ContentChanged: Boolean = False);
    procedure FlushVisualRefresh(Data: PtrInt);
    procedure RenderCommittedState;
    procedure RetryRenderAfterCommit(Data: PtrInt);
    procedure OnVisualMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; const Hit: TVerseHit);
    procedure OnVisualDoubleClick(Sender: TObject);
    procedure OnVisualHover(Sender: TObject; const Hit: TVerseHit);
    function ResolveTranslatorNoteHover(Sender: TObject; ARevision,
      ANoteId: QWord; out ABodyRaw: UnicodeString;
      out AAnchorScreenRect: TRect): Boolean;
    function TryGetRenderedTranslatorNote(ANoteId: QWord;
      out ANote: TVerseRenderedTranslatorNote): Boolean;
    procedure OnVisualContentHeightChanged(Sender: TObject);
    procedure SetStrongsCountMode(AValue: TStrongsCountMode);
    procedure SetBoldWordsWithStrong(AValue: boolean);
    procedure SetPairs(const AValue: string);
    procedure SetTokenizerVersion(AValue: Integer);
    procedure SetText(_XML: string);
    function GetText: string;
    procedure SetPairedVerse(PairVerse: TVerse);
    procedure SelectSyntagmList(lst: string);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditExit(Sender: TObject);
    procedure EditConfirm;
    procedure UpdateInternalXML;
    procedure UpdateStrongCount;
    function GetTokens: string;
    procedure OnCopySyntagmTags(Sender: TObject);
    procedure OnPasteSyntagmTags(Sender: TObject);
    procedure OnSaveTextToFile(Sender: TObject);
    procedure OnCopyTokensToClipboard(Sender: TObject);
    procedure OnCopyInterlinearVerseToClipboard(Sender: TObject);
    procedure OnCopyXMLToClipboard(Sender: TObject);
    procedure OnRightToLeft(Sender: TObject);
    procedure OnVerseMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnResize(Sender: TObject);
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
             WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure InitSyntagmPopupMenu;
    procedure InitVersePopupMenu;
    procedure OnCopyAssociationsJSONToClipboard(Sender: TObject);
    procedure OnCopySyntagmsAsJSON(Sender: TObject);
    function CreateTokenizer(const XML: string): TONTTokenizer;
    procedure ReplaceTextPreservingAssociations(const _XML: string; ForceRetokenize: Boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(const AReference: string = ''); overload;
    constructor Create(TheOwner: TScrollBox; const AReference: string = ''); overload;
    destructor Destroy; override;
    procedure MarkDestroying;
    procedure ClearSyntagms;
    procedure DismissTranslatorNotePreview;
    function TranslatorNotePreviewActive: Boolean;
    procedure InvalidateSyntagmVisual(Syntagm: TSyntagm);
    procedure RefreshAfterEditorClose;
    procedure AssociateSyntagms;
    procedure ClearSelection;
    procedure DisassociatePairs;
    procedure ClearAssociations;
    procedure Render; virtual;
    procedure RenderStrongCount;
    procedure OrganizeSyntagms;
    procedure SelectSyntagms(list: TSyntagmList);
    procedure ChangeText(_XML: string);
    function GetPairsList(ListType: TPairsListType): TStringList;
    function GetAssociationPairData(
      ListType: TPairsListType): TAssociationPairDataArray;
    procedure OnSyntagmPopupMenu(s: TSyntagm);
    procedure ShowTags;
    function TryGetSyntagmScreenAnchor(Syntagm: TSyntagm;
      out Anchor: TPoint): Boolean;
    procedure HideTags;
    function GetONTLine: string; overload;
    function GetONTLine(Morphology: boolean; AutoItalics: boolean; ReusedStrongs: boolean;
      UntranslatedStrongs: boolean): string; overload;
    procedure ClearStrongCount;
    procedure RetokenizePreservingAssociations;
    procedure EnableStrongHighlight(strong: string);
    procedure DisableStrongHighlight;
    function GetAssociationsJSON: string;
    function HasAssociations: boolean;
    function CanChangeTextPreservingAssociationIdentity(
      const AXML: string): Boolean;
    function TryApplyTracedEdit(const ANewXML: string;
      const ATrace: TVerseEditTrace;
      out AImpact: TAssociationEditImpact): Boolean;

    procedure FindAndSelectSyntagms(Words: TStringList; Partial: Boolean = False; JoinConnected: Boolean = True);

    function GetTheWordInterlinearLine: string;
    function GetMySwordInterlinearLine: string;

    property XML: string read FXML;
    property VersePopupMenu: TPopupMenu read FVersePopupMenu;
    property Edit: TEdit read FEdit write FEdit;
    property PairedVerse: TVerse read FPairedVerse write SetPairedVerse;
    property Reference: string read FReference write FReference;
    property ReferenceID: string read FReferenceID write FReferenceID;
    property Text: string read GetText write SetText;
    property Pairs: string read GetPairs write SetPairs;
    property Panel: TScrollBox read FPanel write FPanel;
    property Syntagms: TSyntagmList read FSyntagms;
    property Selection: TSyntagmList read FSelection write FSelection;
    property Modified: boolean read FModified write SetModified;
    property XMLModified: boolean read FXMLModified write FXMLModified;
    property OnClick: TOnSyntagmEvent read FOnSyntagmClick write FOnSyntagmClick;
    property OnMouseEnter: TOnSyntagmEvent read FOnSyntagmMouseEnter write FOnSyntagmMouseEnter;
    property OnMouseLeave: TOnSyntagmEvent read FOnSyntagmMouseLeave write FOnSyntagmMouseLeave;
    property OnVerseChanged: TOnVerseChangedEvent read FOnVerseChanged write FOnVerseChanged;
    property OnExportText: TOnExportTextEvent read FOnExportText write FOnExportText;
    property OnTranslatorNoteClick: TOnTranslatorNoteClickEvent
      read FOnTranslatorNoteClick write FOnTranslatorNoteClick;
    property ShowHints: boolean read FShowHints write FShowHints;
    property OnTextDirectionChange: TOnTextDirectionChangeEvent
      read FOnTextDirectionChange write FOnTextDirectionChange;
    property Active: boolean read GetActive write SetActive;
    property PlainText: string read GetPlainText;
    property AssociatedColor: TColor read FAssociatedColor write FAssociatedColor;
    property UnassociatedColor: TColor read FUnassociatedColor write FUnassociatedColor;
    property AssociationProgress: Single read GetAssociationProgress;
    property Font: TFont read GetFont write SetFont;
    property BoldWordsWithStrong: boolean read FBoldWordsWithStrong write SetBoldWordsWithStrong;
    property StrongsCountMode: TStrongsCountMode read FStrongsCountMode write SetStrongsCountMode;
    property DebugTokens: string read GetTokens;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property BaseDirection: TTextDirection read FBaseDirection write SetBaseDirection;
    property RightToLeft: Boolean read GetRightToLeft write SetRightToLeft default False;
    property NextAutoSelectWordMustHaveStrongMorpho: boolean read FNextAutoSelectWordMustHaveStrongMorpho write FNextAutoSelectWordMustHaveStrongMorpho;
    property TokenizerVersion: Integer read FTokenizerVersion write SetTokenizerVersion;
    property AutomaticNoteStart: Integer read FAutomaticNoteStart
      write SetAutomaticNoteStart;
    property VisualRevision: QWord read FVisualRevision;
    property Destroying: boolean read FDestroying;
 published{ Published declarations }
  end;

resourcestring
  SCopyTags ='&Copy tags';
  SPasteAllTags = 'Paste &all tags';
  SPasteStrongTags = '&Paste &Strong''s tags';
  SPasteMorphoTags = 'Paste &morphology tags';
  SSaveToFile = '&Save text to file...';
  SRightToLeft = 'Right to left text';
  SCopyTokensToClipboard = 'Copy &tokens to clipboard';
  SCopySyntagmsJSONToClipboard = 'Copy syntagms as &JSON to clipboard';
  SCopyInterlinearVerseToClipboard = 'Copy &interlinear verse to clipboard';
  SCopyXMLToClipboard = 'Copy association &XML to clipboard';
  SCopyAssociationsJSONToClipboard = 'Copy association &JSON to clipboard';

implementation

uses
  TypInfo, LazLoggerBase, SyntagmSelection, verse_visual_document,
  directwrite_syntagm_view, theword_ont_codec_v2;

const
  MarginWidth = 10; // text's distance from the margin

var
  SyntagmClipboard: TSyntagm;

function CompareIndexes(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := List[Index1].ToInteger - List[Index2].ToInteger;
end;

function CopySelectionInLogicalOrder(AVerse: TVerse;
  ASelection: TSyntagmList): TSyntagmList;
var
  Syntagm: TSyntagm;
begin
  Result := TSyntagmList.Create;
  if not Assigned(AVerse) or not Assigned(ASelection) or
     not Assigned(AVerse.Syntagms) then
    Exit;
  for Syntagm in AVerse.Syntagms do
    if ASelection.IndexOf(Syntagm) >= 0 then
      Result.Add(Syntagm);
end;

{ TVerse }

constructor TVerse.Create(TheOwner : TScrollBox; const AReference: string = '');
var
  DirectWriteView: TDirectWriteSyntagmView;
begin
  AIDebugLogFmt('[TVerse] Create called. Self=%s, Owner=%s, Reference=%s',
    [IntToHex(PtrUInt(Pointer(Self)), SizeOf(Pointer)*2),
     IntToHex(PtrUInt(Pointer(TheOwner)), SizeOf(Pointer)*2),
     AReference]);
  FReference := AReference;
  FPanel := TheOwner;

  FBaseDirection := tdLeftToRight;
  FVisualRevision := 1;
  FVisualUpdatesSuspended := 0;
  FVisualRefreshQueued := False;
  FLastHoverLogicalIndex := -1;
  FAutomaticNoteStart := 0;
  FRenderedTranslatorNotes := nil;
  FOnTranslatorNoteClick := nil;
  FNoteHover := TTranslatorNoteHoverController.Create(nil);
  FNoteHover.OnResolve := @ResolveTranslatorNoteHover;
  FDisplayTags := False;
  FVerseView := nil;
  FVerseViewControl := nil;
  FActive := assigned(FPanel);

  if (FActive) then
  begin
    FPanel.Color   := clWindow;
    FPanel.Caption := '';

    InitSyntagmPopupMenu;
    DirectWriteView := TDirectWriteSyntagmView.Create(FPanel);
    if DirectWriteView.BackendAvailable then
    begin
      DirectWriteView.Parent := FPanel;
      DirectWriteView.Align := alTop;
      DirectWriteView.Font.Assign(FPanel.Font);
      DirectWriteView.Color := FPanel.Color;
      DirectWriteView.OnVerseMouseDown := @OnVisualMouseDown;
      DirectWriteView.OnVerseDoubleClick := @OnVisualDoubleClick;
      DirectWriteView.OnVerseHover := @OnVisualHover;
      DirectWriteView.OnContentHeightChanged :=
        @OnVisualContentHeightChanged;
      FVerseViewControl := DirectWriteView;
      FVerseView := DirectWriteView;
    end
    else
      DirectWriteView.Free;

    InitVersePopupMenu;

    FEdit            := TEdit.Create(FPanel);
    FEdit.Visible    := false;
    FEdit.AutoSize   := false;
    FEdit.OnKeyDown  := @EditKeyDown;
    FEdit.OnExit     := @EditExit;
    FPanel.InsertControl(FEdit);
    FPanel.OnMouseDown  := @OnVerseMouseDown;
    FPanel.OnResize     := @OnResize;
    FPanel.OnMouseWheel := @OnMouseWheel;
  end;

  FStrongCount          := nil;
  FStrongsCountMode     := scNone;
  FPairedVerse         := nil;
  FOnSyntagmClick      := nil;
  FOnSyntagmMouseEnter := nil;
  FOnSyntagmMouseLeave := nil;
  FModified           := false;
  FChangeNotificationsSuspended := 0;
  FXMLModified        := false;
  FSelection              := TSyntagmList.Create;
  FShowHints         := false;
  FNextAutoSelectWordMustHaveStrongMorpho := true;
  FAssociatedColor         := clWindowText;
  FUnassociatedColor      := $A3A3A3;
  FDestroying           := false;
  FRendering            := false;
  FTokenizerVersion := TTokenizerFactory.PreferredVersion;
  FONTParser := TONTParser.Create(FTokenizerVersion);
  FSyntagms := TSyntagmList.Create;

  { regex to fix cases in which a <Fi> tag ends up misplaced }
  FFixWrongFiTag := RegexCreate('((?:<W[THG][^>]+>)+)(<Fi>)', [rcoUTF8]);
end;

destructor TVerse.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  FVisualRefreshQueued := False;
  AIDebugLogFmt('[TVerse] Destroy called. Self=%s, Panel=%s, Reference=%s',
    [IntToHex(PtrUInt(Pointer(Self)), SizeOf(Pointer)*2),
     IntToHex(PtrUInt(Pointer(FPanel)), SizeOf(Pointer)*2),
     FReference]);
  FDestroying := true;
  DismissTranslatorNotePreview;
  ClearSyntagms;
  FSyntagms.Free;
  FSelection.Destroy;
  if assigned(FPanel) then
  begin
    FEdit.Free;
    FSyntagmPopupMenu.Free;
    FVerseView := nil;
    FreeAndNil(FVerseViewControl);
    FVersePopupMenu.Free;
    FPanel.OnMouseDown  := nil;
    FPanel.OnResize     := nil;
    FPanel.OnMouseWheel := nil;
  end;
  FONTParser.Destroy;
  FreeAndNil(FNoteHover);
  inherited Destroy;
end;

procedure TVerse.MarkDestroying;
begin
  FDestroying := true;
  DismissTranslatorNotePreview;
end;

procedure TVerse.RefreshAfterEditorClose;
begin
  if FDestroying or not FActive then
    Exit;

  { The editor is a sibling overlay. Hiding it does not reliably generate a
    paint for the DirectWrite child on Win32. Hit testing deliberately rejects
    stale geometry, so force one synchronous paint before accepting input. }
  if Assigned(FVerseViewControl) then
  begin
    FVerseViewControl.Invalidate;
    FVerseViewControl.Repaint;
  end
  else if Assigned(FPanel) then
    FPanel.Repaint;
end;

procedure TVerse.ClearSyntagms;
var
  s: TSyntagm;
begin
  DismissTranslatorNotePreview;
  FRenderedTranslatorNotes := nil;
  if assigned(FSyntagms) then
  begin
    Inc(FVisualUpdatesSuspended);
    try
      Application.RemoveAsyncCalls(Self);
      FVisualRefreshQueued := False;

      if FActive and assigned(FPanel) then
        FPanel.DisableAutoSizing;
      Inc(FVisualRevision);
      if Assigned(FVerseView) then
        FVerseView.SetDocument(nil);
      FLastHoverLogicalIndex := -1;

      for s in FSyntagms do
        s.Destroy;

      ClearStrongCount;
      FSyntagms.Clear;
    finally
      if FActive and assigned(FPanel) then
        FPanel.EnableAutoSizing;
      Dec(FVisualUpdatesSuspended);
    end;
  end;
  FSelection.Clear;
end;

procedure TVerse.DismissTranslatorNotePreview;
begin
  if Assigned(FNoteHover) then
    FNoteHover.Dismiss;
end;

function TVerse.TranslatorNotePreviewActive: Boolean;
begin
  Result := Assigned(FNoteHover) and FNoteHover.Active;
end;

procedure TVerse.InvalidateSyntagmVisual(Syntagm: TSyntagm);
begin
  if FDestroying or (FVisualUpdatesSuspended > 0) then
    Exit;
  if Assigned(FVerseView) then
  begin
    if not FVisualRefreshQueued then
    begin
      FVisualRefreshQueued := True;
      Application.QueueAsyncCall(@FlushVisualRefresh, 0);
    end;
  end
  else if FActive and Assigned(FPanel) then
    FPanel.Invalidate;
end;

procedure TVerse.FlushVisualRefresh(Data: PtrInt);
begin
  FVisualRefreshQueued := False;
  if FDestroying or (FVisualUpdatesSuspended > 0) or
     not Assigned(FVerseView) then
    Exit;
  RefreshVisualDocument;
end;

procedure TVerse.RenderCommittedState;
begin
  try
    Render;
  except
    on E: Exception do
    begin
      { The model is already committed. Reporting a presentation exception as
        a rejected save would leave the editor with a stale provenance trace. }
      DebugLogger.DebugLn('Post-commit Verse render failed: ' + E.Message);
      try
        Application.QueueAsyncCall(@RetryRenderAfterCommit, 0);
      except
        on QueueError: Exception do
          DebugLogger.DebugLn(
            'Could not queue post-commit Verse render retry: ' +
            QueueError.Message);
      end;
    end;
  end;
end;

procedure TVerse.RetryRenderAfterCommit(Data: PtrInt);
begin
  if FDestroying then
    Exit;
  try
    Render;
  except
    on E: Exception do
      DebugLogger.DebugLn(
        'Deferred post-commit Verse render failed: ' + E.Message);
  end;
end;

procedure TVerse.RefreshVisualDocument(ContentChanged: Boolean);
begin
  if not Assigned(FVerseView) or not Assigned(FSyntagms) then
    Exit;
  if ContentChanged then
  begin
    DismissTranslatorNotePreview;
    Inc(FVisualRevision);
  end;
  FVerseView.SetDocument(BuildVerseVisualDocument(FSyntagms, FStrongCount,
    FBaseDirection, FVisualRevision, FAssociatedColor, FUnassociatedColor,
    FBoldWordsWithStrong, FXML, FAutomaticNoteStart,
    FRenderedTranslatorNotes));
  FVerseView.DisplayTags := FDisplayTags;
end;

procedure TVerse.OnVisualMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; const Hit: TVerseHit);
var
  Syntagm: TSyntagm;
begin
  if FDestroying then
    Exit;
  if Hit.Role = vsrTranslatorNote then
  begin
    DismissTranslatorNotePreview;
    { Route stale hits as well so the editor can report that the note is no
      longer present, instead of silently ignoring the user's click. }
    if (Button = mbLeft) and (Hit.ObjectId <> 0) and
       Assigned(FOnTranslatorNoteClick) then
      FOnTranslatorNoteClick(Self, Hit.ObjectId, Hit.Revision);
    Exit;
  end;
  if Hit.Revision <> FVisualRevision then
  begin
    OnVerseMouseDown(Sender, Button, Shift, 0, 0);
    Exit;
  end;
  if (Hit.LogicalIndex < 0) or not Assigned(FSyntagms) or
     (Hit.LogicalIndex >= FSyntagms.Count) then
  begin
    OnVerseMouseDown(Sender, Button, Shift, 0, 0);
    Exit;
  end;
  Syntagm := FSyntagms[Hit.LogicalIndex];
  if Assigned(Syntagm) then
    Syntagm.HandleMouseDown(Sender, Button, Shift, 0, 0);
end;

procedure TVerse.OnVisualDoubleClick(Sender: TObject);
begin
  if Assigned(FPanel) and Assigned(FPanel.OnDblClick) then
    FPanel.OnDblClick(FPanel);
end;

procedure TVerse.OnVisualHover(Sender: TObject; const Hit: TVerseHit);
var
  NewIndex: Integer;
  OldIndex: Integer;
begin
  if FDestroying or not Assigned(FSyntagms) then
    Exit;
  OldIndex := FLastHoverLogicalIndex;
  if Hit.Role = vsrTranslatorNote then
  begin
    if OldIndex >= 0 then
    begin
      FLastHoverLogicalIndex := -1;
      if OldIndex < FSyntagms.Count then
        FSyntagms[OldIndex].HandleMouseLeave(Sender);
    end;
    if (Hit.Revision = FVisualRevision) and (Hit.ObjectId <> 0) then
      FNoteHover.Hover(Hit.Revision, Hit.ObjectId)
    else
      FNoteHover.Leave;
    Exit;
  end;

  FNoteHover.Leave;
  NewIndex := Hit.LogicalIndex;
  if (Hit.Revision <> FVisualRevision) or
     (NewIndex < 0) or (NewIndex >= FSyntagms.Count) then
    NewIndex := -1;
  if NewIndex = OldIndex then
    Exit;
  FLastHoverLogicalIndex := NewIndex;
  if (OldIndex >= 0) and (OldIndex < FSyntagms.Count) then
    FSyntagms[OldIndex].HandleMouseLeave(Sender);
  if NewIndex >= 0 then
    FSyntagms[NewIndex].HandleMouseEnter(Sender);
end;

function TVerse.TryGetRenderedTranslatorNote(ANoteId: QWord;
  out ANote: TVerseRenderedTranslatorNote): Boolean;
var
  Index: Integer;
begin
  ANote := Default(TVerseRenderedTranslatorNote);
  for Index := 0 to High(FRenderedTranslatorNotes) do
    if FRenderedTranslatorNotes[Index].Id = ANoteId then
    begin
      ANote := FRenderedTranslatorNotes[Index];
      Exit(True);
    end;
  Result := False;
end;

function TVerse.ResolveTranslatorNoteHover(Sender: TObject; ARevision,
  ANoteId: QWord; out ABodyRaw: UnicodeString;
  out AAnchorScreenRect: TRect): Boolean;
var
  AnchorRect: TRect;
  Note: TVerseRenderedTranslatorNote;
  P1, P2: TPoint;
begin
  ABodyRaw := '';
  AAnchorScreenRect := Rect(0, 0, 0, 0);
  Result := not FDestroying and (ARevision = FVisualRevision) and
    TryGetRenderedTranslatorNote(ANoteId, Note) and
    (FVerseViewControl is TDirectWriteSyntagmView) and
    TDirectWriteSyntagmView(FVerseViewControl).GetVisualObjectClientRect(
      vsrTranslatorNote, ANoteId, AnchorRect);
  if not Result then
    Exit;
  P1 := FVerseViewControl.ClientToScreen(
    Point(AnchorRect.Left, AnchorRect.Top));
  P2 := FVerseViewControl.ClientToScreen(
    Point(AnchorRect.Right, AnchorRect.Bottom));
  AAnchorScreenRect := Rect(P1.X, P1.Y, P2.X, P2.Y);
  ABodyRaw := Note.BodyRaw;
end;

procedure TVerse.OnVisualContentHeightChanged(Sender: TObject);
var
  NewHeight: Integer;
begin
  if FDestroying or not Assigned(FVerseViewControl) or
     not (Sender is TDirectWriteSyntagmView) then
    Exit;
  NewHeight := Max(1, TDirectWriteSyntagmView(Sender).ContentHeight);
  if FVerseViewControl.Height <> NewHeight then
    FVerseViewControl.Height := NewHeight;
end;

function TVerse.TryGetSyntagmScreenAnchor(Syntagm: TSyntagm;
  out Anchor: TPoint): Boolean;
var
  LogicalIndex: Integer;
begin
  Anchor := Point(0, 0);
  Result := False;
  if not Assigned(Syntagm) or not Assigned(FSyntagms) then
    Exit;
  LogicalIndex := FSyntagms.IndexOf(Syntagm);
  if LogicalIndex < 0 then
    Exit;
  if Assigned(FVerseView) then
    Exit(FVerseView.TryGetLogicalScreenAnchor(LogicalIndex, Anchor));
  if Assigned(Syntagm.LabelRef) then
  begin
    Anchor := Syntagm.LabelRef.ClientToScreen(
      Syntagm.LabelRef.ClientRect.BottomRight);
    Result := True;
  end;
end;

procedure TVerse.AssociateSyntagms;
var
  s: TSyntagm;
  SourceSelection: TSyntagmList;
  TargetSelection: TSyntagmList;
begin
  if not Assigned(PairedVerse) then
    exit;

  SourceSelection := CopySelectionInLogicalOrder(Self, Selection);
  TargetSelection := CopySelectionInLogicalOrder(PairedVerse,
    PairedVerse.Selection);
  try
    for s in SourceSelection do
    begin
      with s do
      begin
        Pairs.Clear;
        Pairs.AddList(TargetSelection);
        IsAssociated:=true;
        Siblings.Clear;
        Siblings.AddList(SourceSelection);
        Siblings.Remove(s);
      end;
    end;
    for s in TargetSelection do
    begin
      with s do
      begin
        Pairs.Clear;
        Pairs.AddList(SourceSelection);
        IsAssociated:=true;
        Siblings.Clear;
        Siblings.AddList(TargetSelection);
        Siblings.Remove(s);
      end;
    end;
  finally
    TargetSelection.Free;
    SourceSelection.Free;
  end;

  Modified := true;
  PairedVerse.Modified := true;
end;

procedure TVerse.ClearSelection;
begin
  while not Selection.Empty do
    TSyntagm(Selection.First).RemoveFromSelection;
end;

procedure TVerse.DisassociatePairs;
var
  s: TSyntagm;
begin
  ClearSelection;
  PairedVerse.ClearSelection;

  for s in FSyntagms do
    s.Disassociate;

  for s in PairedVerse.FSyntagms do
    s.Disassociate;

  Render;
  PairedVerse.Render;
end;

procedure TVerse.ClearAssociations;
var
  s: TSyntagm;
begin
  for s in FSyntagms do
    if not s.Pairs.Empty then
    begin // disassociate only when associations exist, avoiding false 'modified = true' notifications
      DisassociatePairs;
      Modified := true;
      break;
    end;
end;

procedure TVerse.SelectSyntagms(list: TSyntagmList);
var
  s: TSyntagm;
begin
  for s in list do
    s.AddToSelection;
end;

function TVerse.GetPairsList(ListType: TPairsListType): TStringList;
var
  PairData: TAssociationPairData;
begin
  result := TStringList.Create;
  for PairData in GetAssociationPairData(ListType) do
  begin
    result.Add(PairData.SourcePhrase);
    result.Add(PairData.TargetPhrase);
  end;
end;

function TVerse.GetAssociationPairData(
  ListType: TPairsListType): TAssociationPairDataArray;
var
  DataIndex: Integer;
  SourceIndex: Integer;
  TargetIndex: Integer;
  SourcePhrase: string;
  TargetPhrase: string;
  Syntagm: TSyntagm;
  PairedSyntagm: TSyntagm;
  Processed: TSyntagmList;
begin
  Result := nil;
  if not Assigned(FSyntagms) then
    Exit;

  Processed := TSyntagmList.Create;
  try
    for Syntagm in FSyntagms do
    begin
      if not Assigned(Syntagm.Pairs) or
         (Syntagm.Kind <> skSyntagm) or Syntagm.IsItalic or
         (Processed.IndexOf(Syntagm) >= 0) then
        continue;

      SourcePhrase := Syntagm.GetSuggestionKey(ListType);
      DataIndex := Length(Result);
      SetLength(Result, DataIndex + 1);
      SourceIndex := FSyntagms.IndexOf(Syntagm);
      SetLength(Result[DataIndex].SourceSyntagmIndexes, 1);
      Result[DataIndex].SourceSyntagmIndexes[0] := SourceIndex;
      Processed.Add(Syntagm);

      for PairedSyntagm in Syntagm.Siblings do
      begin
        if PairedSyntagm.IsItalic or
           (PairedSyntagm.Kind <> skSyntagm) then
          continue;
        SourcePhrase := SourcePhrase + ';' +
          PairedSyntagm.GetSuggestionKey(ListType);
        SourceIndex := Length(Result[DataIndex].SourceSyntagmIndexes);
        SetLength(Result[DataIndex].SourceSyntagmIndexes, SourceIndex + 1);
        Result[DataIndex].SourceSyntagmIndexes[SourceIndex] :=
          FSyntagms.IndexOf(PairedSyntagm);
        Processed.Add(PairedSyntagm);
      end;

      TargetPhrase := '';
      for PairedSyntagm in Syntagm.Pairs do
      begin
        if PairedSyntagm.IsItalic or
           (PairedSyntagm.Kind <> skSyntagm) then
          continue;
        if not TargetPhrase.IsEmpty then
          TargetPhrase := TargetPhrase + ';';
        TargetPhrase := TargetPhrase +
          PairedSyntagm.GetSuggestionKey(ListType);
        TargetIndex := Length(Result[DataIndex].TargetSyntagmIndexes);
        SetLength(Result[DataIndex].TargetSyntagmIndexes, TargetIndex + 1);
        Result[DataIndex].TargetSyntagmIndexes[TargetIndex] :=
          FPairedVerse.Syntagms.IndexOf(PairedSyntagm);
      end;

      if SourcePhrase.IsEmpty or TargetPhrase.IsEmpty then
      begin
        SetLength(Result, DataIndex);
        Continue;
      end;
      Result[DataIndex].SourcePhrase := SourcePhrase;
      Result[DataIndex].TargetPhrase := TargetPhrase;
    end;
  finally
    Processed.Free;
  end;
end;

procedure TVerse.OnSyntagmPopupMenu(s: TSyntagm);
var
  p: TPoint;
begin
  FSyntagmPopupMenu.Tag := PtrInt(s);
  if not TryGetSyntagmScreenAnchor(s, p) then
    p := Mouse.CursorPos;
  FSyntagmPopupMenu.PopUp(p.x, p.y);
end;

procedure TVerse.ShowTags;
var
  s: TSyntagm;
  p: TPoint;
  tags: string;
begin
  FDisplayTags := True;
  if not FActive then
    Exit;
  if Assigned(FVerseView) then
  begin
    FVerseView.DisplayTags := True;
    Exit;
  end;

  for s in FSyntagms do
  begin
    if not assigned(s.LabelRef) or not s.HasStrongs then
      continue;
    p := s.LabelRef.ClientToParent(s.LabelRef.ClientRect.TopLeft, FPanel);
    tags := s.Strong.CommaText;
    with FPanel.Canvas do
    begin
      // Link to the Paint override solution: http://forum.lazarus.freepascal.org/index.php?topic=23894.0
      Brush.Color := clYellow;
      Font.Name   := DefFontData.Name;
      Font.Height := round(s.LabelRef.Font.Height * 0.6);
      Font.Color  := clBlack;
      Rectangle(p.x, p.y, p.x + TextWidth(tags) + 4, p.y + TextHeight(tags));
      Brush.Style := bsClear;
      TextOut(p.x+2, p.y, tags);
    end;
  end;
end;

procedure TVerse.HideTags;
begin
  FDisplayTags := False;
  if Assigned(FVerseView) then
  begin
    FVerseView.DisplayTags := False;
    Exit;
  end;
  if FActive and Assigned(FPanel) then
    FPanel.Refresh;
end;

procedure TVerse.EnableStrongHighlight(strong: string);
var
  s: TSyntagm;
begin
  for s in FSyntagms do
    s.HighlightStrong(strong);
end;

procedure TVerse.DisableStrongHighlight;
var
  s: TSyntagm;
begin
  for s in FSyntagms do
    s.ToggleStrongHighlight(false);
end;

procedure TVerse.SetText(_XML: string);
begin
  FXML := _XML;
  ClearSyntagms;
  FreeAndNil(FSyntagms);
  FSyntagms := FONTParser.ParseLine(FXML, self);
  Render;
  Modified := true;
  FXMLModified := true;
  SyntagmClipboard := nil;
end;

{ Replaces the verse text trying to keep existing associations }
procedure TVerse.ReplaceTextPreservingAssociations(const _XML: string;
  ForceRetokenize: Boolean);

  function HasSameAssociationIdentity(const Existing,
    Candidate: TSyntagm): Boolean;
  begin
    { Association identity is the structural text unit, not its presentation.
      Semantic wrappers such as FI/FR/FO/FU can change style fields or insert
      metadata syntagms without changing the associated word itself. Strong
      and morphology payloads are deliberately refreshed from Candidate below. }
    Result := Assigned(Existing) and Assigned(Candidate) and
      (Existing.Kind = Candidate.Kind) and
      (Existing.Text = Candidate.Text);
  end;

  function IsAssociationUnit(ASyntagm: TSyntagm): Boolean;
  begin
    Result := Assigned(ASyntagm) and
      (ASyntagm.Kind in [skSyntagm, skPunctuation]);
  end;

  procedure DeleteSyntagm(i: integer; newlist: TSyntagmList);
  var
    s: TSyntagm;
  begin
    s := FSyntagms[i];
    s.Disassociate;
    s.Destroy;
    FSyntagms.Delete(i);
  end;

var
  new, result: TSyntagmList;
  s, old: TSyntagm;
  found, oldActive: boolean;
begin
  if (not ForceRetokenize) and (FXML = _XML) then
    exit;

  oldActive := FActive;
  Inc(FVisualUpdatesSuspended);
  try
    { End hover and cancel queued paints while the original logical-index map
      is still intact. Reconciliation below destroys and moves syntagms. }
    DismissTranslatorNotePreview;
    FRenderedTranslatorNotes := nil;
    Application.RemoveAsyncCalls(Self);
    FVisualRefreshQueued := False;
    Inc(FVisualRevision);
    if Assigned(FVerseView) then
      FVerseView.SetDocument(nil);
    FLastHoverLogicalIndex := -1;

    FActive := false; // we're potentially deleting labels

    new := FONTParser.ParseLine(_XML, self);

  ClearSelection;

  if (not Assigned(FSyntagms)) or FSyntagms.Empty then
  begin
    FreeAndNil(FSyntagms);
    FSyntagms := new;
    new := nil;
  end
  else
  begin
    result := TSyntagmList.Create;

    while not (new.Empty and FSyntagms.Empty) do
    begin
      if new.Empty then
        DeleteSyntagm(0, result)
      else if FSyntagms.Empty then
      begin
       result.Add(new[0]);
       new.Delete(0);
      end
      else if HasSameAssociationIdentity(FSyntagms.First, new.First) then
      begin
        FSyntagms.First.AdoptParsedState(new.First);
        result.Add(FSyntagms.First);
        FSyntagms.Delete(0);
        new.First.Destroy;
        new.Delete(0);
      end
      else if not IsAssociationUnit(FSyntagms.First) then
        DeleteSyntagm(0, result)
      else if not IsAssociationUnit(new.First) then
      begin
        result.Add(new[0]);
        new.Delete(0);
      end
      else
      begin
        { checking if the old syntagm will be used in the future }
        found := false;
        old := FSyntagms.First;
        if not old.Pairs.Empty then // does it have associations?
        begin
          for s in new do
            if HasSameAssociationIdentity(old, s) then
            begin
              result.Add(new[0]);
              new.Delete(0);
              found := true;
              break;
            end;
        end;
        if not found then { nope, wasting it }
          DeleteSyntagm(0, result);
      end;
    end;

    if not new.Empty then
      raise Exception.Create('Ainda há sintagmas novos que não foram liberados!');

    if not FSyntagms.Empty then
      raise Exception.Create('Ainda há sintagmas antigos que não foram liberados!');

    FreeAndNil(new);
    ClearSyntagms;
    FreeAndNil(FSyntagms);
    FSyntagms := result;
  end;

  FXML := _XML;
  FXMLModified := true;
  Modified := true;
  if assigned(PairedVerse) then
    PairedVerse.Modified := true;
  SyntagmClipboard := nil;

  finally
    FActive := oldActive;
    Dec(FVisualUpdatesSuspended);
  end;
  RenderCommittedState;
end;

procedure TVerse.ChangeText(_XML: string);
begin
  ReplaceTextPreservingAssociations(_XML, False);
end;

procedure TVerse.RetokenizePreservingAssociations;
begin
  ReplaceTextPreservingAssociations(FXML, True);
end;

function TVerse.GetText: string;
var
  lineStream: TStringStream;
  s: TSyntagm;
begin
  result := '';
  try
    lineStream := TStringStream.Create('');
    for s in FSyntagms do
      lineStream.WriteString(s.Text);
  finally
    result := lineStream.DataString;
    lineStream.Destroy;
  end;
end;

function TVerse.CreateTokenizer(const XML: string): TONTTokenizer;
begin
  Result := TTokenizerFactory.CreateTokenizer(FTokenizerVersion, XML);
end;

procedure TVerse.SetPairs(const AValue: string);
var
  varredorXML: TONTTokenizer;
  s: TSyntagmToken;
begin
  if not assigned(PairedVerse) then
    exit;

  // Applying serialized associations is a load operation. Avoid publishing
  // a transient dirty state for every individual <par> element.
  Inc(FChangeNotificationsSuspended);
  Inc(PairedVerse.FChangeNotificationsSuspended);
  try
    FShowError := true;
    PairedVerse.FShowError := true;

    ClearAssociations;
    ClearSelection;
    PairedVerse.ClearSelection;
    if AValue.IsEmpty then
    begin
      Render;
      PairedVerse.Render;
      Exit;
    end;

    varredorXML := CreateTokenizer(AValue);
    try
      try
        while varredorXML.ReadSyntagm(s) <> skNull do
        begin
          if not AnsiStartsStr('<par ', s.Value) then
            Continue;

          //DebugLn('selecionando par: %s', [s.Value]);
          SelectSyntagmList(varredorXML.ReadTagProperty('a', s));
          PairedVerse.SelectSyntagmList(
            varredorXML.ReadTagProperty('b', s));
          AssociateSyntagms;

          ClearSelection;
          PairedVerse.ClearSelection;
        end;
      except
        // Applying persisted associations is atomic from the Verse Pair point
        // of view. Never expose the successfully parsed prefix of corrupt data.
        ClearSelection;
        PairedVerse.ClearSelection;
        ClearAssociations;
        raise;
      end;
    finally
      varredorXML.Free;
    end;

    Render;
    PairedVerse.Render;
  finally
    Dec(PairedVerse.FChangeNotificationsSuspended);
    Dec(FChangeNotificationsSuspended);
  end;
end;

procedure TVerse.SetTokenizerVersion(AValue: Integer);
begin
  AValue := TTokenizerFactory.NormalizeVersion(AValue);
  if FTokenizerVersion = AValue then
    Exit;

  FTokenizerVersion := AValue;
  if Assigned(FONTParser) then
    FONTParser.TokenizerVersion := AValue;
end;

function TVerse.GetPairs: string;
var
  b: integer;
  s, p: TSyntagm;
  _xml: TStringStream;
  tmp: TSyntagmList;
begin
  _xml := TStringStream.Create('');

  // gerar tags de Texto <p a="1,2,3" b="1,2,3">
  tmp := TSyntagmList.Create;
  for s in FSyntagms do
  begin
    if not s.Pairs.Empty and (tmp.IndexOf(s) < 0) then
    begin
      _xml.WriteString(Format('<par a="%d', [FSyntagms.IndexOf(s)]));
      tmp.Add(s);
      for p in s.Siblings do
      begin
        _xml.WriteString(Format(',%d', [FSyntagms.IndexOf(p)]));
        tmp.Add(p);
      end;
      _xml.WriteString('" b="');
      for p in s.Pairs do
      begin
        b := FPairedVerse.Syntagms.IndexOf(p);
        if b < 0 then
           raise exception.Create('Pair not found in other verse');
        _xml.WriteString(Format('%d', [b]));
        if s.Pairs.IndexOf(p) <> s.Pairs.Count-1 then
          _xml.WriteString(',');
      end;
      _xml.WriteString('">');
    end;
  end;
  tmp.Destroy;

  result := _xml.DataString;
  _xml.Destroy;
end;

function TVerse.GetPlainText: string;
var
  lineStream: TStringStream;
  s: TSyntagm;
begin
  result := '';
  if not Assigned(FSyntagms) then
    Exit;

  lineStream := TStringStream.Create('');
  try
    for s in FSyntagms do
      if s.Kind in [skWhitespace, skSyntagm, skPunctuation] then
        lineStream.WriteString(s.Text);
    result := lineStream.DataString;
  finally
    lineStream.Destroy;
  end;
end;

function TVerse.GetActive: boolean;
begin
  result := FActive;
end;

function TVerse.GetFont: TFont;
begin
  if FActive then
    result := FPanel.Font
  else
    result := nil;
end;

function TVerse.GetAssociationProgress: Single;
var
  s: TSyntagm;
  t, p: smallint;
begin
  t := 0;
  p := 0;
  result := 0;

  for s in FSyntagms do
    if s.Kind = skSyntagm then
    begin
      inc(t);
      if s.IsAssociated then
        inc(p);
    end;

  if t > 0 then
    result := p/t;
end;

function TVerse.GetTheWordInterlinearLine: string;

  function GetSpacingBefore(syn, pair: TSyntagm): string;
  var
    i: integer;
  begin
    result := '';
    if pair = syn.Pairs[0] then exit;
    for i:=FPairedVerse.Syntagms.IndexOf(pair)-1 downto 0 do
    begin
      if FPairedVerse.Syntagms[i].Kind = skSyntagm then
        break;
      if FPairedVerse.Syntagms[i].Kind = skWhitespace then
      begin
        result := ' ';
        break;
      end;
    end;
  end;

var
  lineStream: TStringStream;
  s, p, prox: TSyntagm;
  m: string;
  n: integer;
begin
  result := '';

  try
    lineStream := TStringStream.Create('');

    for s in FSyntagms do
    begin
      lineStream.WriteString(s.Text);
      if (assigned(s.Strong) and (s.Strong.Count > 0)) or (assigned(s.Morph) and (s.Morph.Count > 0)) then
      begin // if this text contains Strongs
        for m in s.Strong do // strongs
          lineStream.WriteString(format('<W%s>', [m]));
        for m in s.Morph do // Morphologylogia
          lineStream.WriteString(format('<WT%s>', [m]));
      end else
      begin // otherwise, try the related text
        for p in s.Pairs do
        begin
          for m in p.Strong do // strongs
            lineStream.WriteString(format('<W%s>', [m]));
          for m in p.Morph do   // Morphologylogia
            lineStream.WriteString(format('<WT%s>', [m]));
        end;
      end;

      if (FSyntagms.IndexOf(s) < (FSyntagms.Count-1)) and // is not the last syntagm and
         (s.Siblings.Count > 0) then   // tem irmãos
      begin
        prox := nil;
        for n:=FSyntagms.IndexOf(s)+1 to Syntagms.Count-1 do
        begin // looking for the next syntagm (skipping spaces, punctuation, etc.)
          if Syntagms[n].Kind = skSyntagm then
          begin
            prox := Syntagms[n];
            break;
          end;
        end;
        if (prox <> nil) and (s.Siblings.IndexOf(prox) >= 0) then // o próximo syntagm é irmão deste?
          continue;
      end;

      for p in s.Pairs do // pairs
        lineStream.WriteString(GetSpacingBefore(s, p) + '<sup>' + IfThen(p.IsItalic, format('<FI>%s<Fi>', [p.Text]), p.Text) + '</sup>');
    end;

  finally
    result := lineStream.DataString.Replace('</sup> <sup>', ' ')
                              .Replace('  ', ' ')
                              .Replace('<sup>', '<font size=+1 color="CC3300"><sup>')
                              .Replace('</sup>', '</sup></font>')
                              .Replace('<Fi><FI>', '')
                              .Replace(' -', '-');
    lineStream.Destroy;
  end;
end;

function TVerse.GetMySwordInterlinearLine: string;

  function GetSpacingBefore(syn, pair: TSyntagm): string;
  var
    i: integer;
  begin
    result := '';
    if pair = syn.Pairs[0] then exit;
    for i:=FPairedVerse.Syntagms.IndexOf(pair)-1 downto 0 do
    begin
      if FPairedVerse.Syntagms[i].Kind = skSyntagm then
        break;
      if FPairedVerse.Syntagms[i].Kind = skWhitespace then
      begin
        result := ' ';
        break;
      end;
    end;
  end;

  function TestAndUnset(out flag: boolean): Boolean;
  begin
    result := flag;
    if flag then
      flag := false;
  end;

  function CollectConsecutiveSiblings(startSyntagm: TSyntagm; var skipIndices: TList): string;
  var
    currentIdx, nextIdx, lookAheadIdx: Integer;
    currentSyn, nextSyn, lookAheadSyn: TSyntagm;
    combinedText: string;
    foundSpace: Boolean;
  begin
    combinedText := startSyntagm.Text;
    currentIdx := FSyntagms.IndexOf(startSyntagm);

    // If we have siblings, check if any are consecutive
    if startSyntagm.Siblings.Count = 0 then
    begin
      Result := combinedText;
      Exit;
    end;

    // Check next syntagms to find consecutive siblings
    nextIdx := currentIdx + 1;
    while nextIdx < FSyntagms.Count do
    begin
      nextSyn := FSyntagms[nextIdx];

      // Add any spaces between siblings
      if nextSyn.Kind = skWhitespace then
      begin
        combinedText := combinedText + nextSyn.Text;
        skipIndices.Add(Pointer(nextIdx));
        Inc(nextIdx);
        continue;
      end
      // Handle '<wt>' tags between siblings - look ahead to see if followed by a sibling
      else if (nextSyn.Kind = skMetadata) and (nextSyn.Text = '<wt>') then
      begin
        // Look ahead to find next syntagm after the tag
        lookAheadIdx := nextIdx + 1;
        lookAheadSyn := nil;

        while (lookAheadIdx < FSyntagms.Count) do
        begin
          if FSyntagms[lookAheadIdx].Kind = skSyntagm then
          begin
            lookAheadSyn := FSyntagms[lookAheadIdx];
            break;
          end;
          Inc(lookAheadIdx);
        end;

        // Only add the '<wt>' tag if followed by a sibling
        if (lookAheadSyn <> nil) and (startSyntagm.Siblings.IndexOf(lookAheadSyn) >= 0) then
        begin
          combinedText := combinedText + nextSyn.Text;
          skipIndices.Add(Pointer(nextIdx));
        end;

        Inc(nextIdx);
        continue;
      end
      // If next syntagm is not a sibling or not a syntagm, we're done
      else if (nextSyn.Kind <> skSyntagm) or
              (startSyntagm.Siblings.IndexOf(nextSyn) < 0) then
        break;

      // Found consecutive sibling, add it to result
      combinedText := combinedText + nextSyn.Text;
      skipIndices.Add(Pointer(nextIdx));
      Inc(nextIdx);
    end;

    Result := combinedText;
  end;

var
  line: TStringStream;
  s, p, nextSyntagm: TSyntagm;
  m: string;
  skip: boolean;
  skipIndices: TList;
  i, currentIndex: Integer;
  combinedText: string;
begin
  result := '';

  {Sample: <TS>The Creation<Ts><Q><wg>εν<WG1722><E> In<e><q> <Q><wg>αρχή<WG746><E> <FI>the<Fi> beginning<e><q>}
  skipIndices := TList.Create;
  try
    line := TStringStream.Create('');

    skip := false;
    i := 0;
    while i < FSyntagms.Count do
    begin
      if skipIndices.IndexOf(Pointer(i)) >= 0 then
      begin
        Inc(i);
        continue;
      end;

      s := FSyntagms[i];

      if TestAndUnset(skip) then
      begin
        Inc(i);
        continue;
      end;

      case s.Kind of
        skSyntagm:
        begin
          // Collect text from consecutive siblings
          combinedText := CollectConsecutiveSiblings(s, skipIndices);

          line.WriteString('<Q>');
          // Use non-breakable spaces in the text
          line.WriteString(combinedText);

          // Output Strong's numbers and morphology for this syntagm
          for m in s.Strong do
            line.WriteString(format('<W%s>', [m]));
          for m in s.Morph do
            line.WriteString(format('<WT%s>', [m]));

          if assigned(s.GetNext) and (s.GetNext.Kind = skPunctuation) then
          begin
            line.WriteString(s.GetNext.Text);
            skip := true;
          end;

          if s.Pairs.Count > 0 then
          begin
            line.WriteString('<T>');
            if (s.Siblings.Count = 0) or (i < FSyntagms.IndexOf(s.Siblings[0])) then
            begin // no siblings or is the first sibling
              for p in s.Pairs do
              begin
                line.WriteString(GetSpacingBefore(s, p));
                // Also use non-breakable spaces in the translated text
                if p.IsItalic then
                  line.WriteString(Format('<FI>%s<Fi>', [p.Text]))
                else
                  line.WriteString(p.Text);
              end;
            end else // not a first sibling
              line.WriteString('←');
            line.WriteString('<t>');
          end
          else
            line.WriteString('<T>•<t>');

          line.WriteString('<q>');
        end;
        skTag:       line.WriteString(s.RawText);
        skWhitespace:    line.WriteString(s.RawText);
        skPunctuation: line.WriteString(s.RawText);
        skMetadata:  if s.Text <> '<wt>' then line.WriteString(s.RawText);
      end;

      Inc(i);
    end;

    result := line.DataString;
  finally
    skipIndices.Free;
    line.Free;
  end;
end;

function TVerse.GetONTLine: string;
begin
  Result := GetONTLine(True, False, False, False);
end;

function TVerse.GetONTLine(Morphology: boolean; AutoItalics: boolean; ReusedStrongs: boolean;
      UntranslatedStrongs: boolean): string;
var
  lineStream: TStringStream;
  stg, prox: TSyntagm;
  s, p, m: smallint;
  UsedStrongs: TList;
  temp: string;
  label NextSyntagm;
begin
  result := '';
  UsedStrongs := TList.Create; // used to store the Strongs already used in the verse
  prox := nil;

  try
    lineStream := TStringStream.Create('');

    s := -1;
    for stg in FSyntagms do
    begin // for each syntagm, compute its output in a temporary variable
      Inc(s);
      temp := '';

      if AutoItalics and (stg.Pairs.Count = 0) and (stg.Kind = skSyntagm) then
        temp := temp + '<FI>';

      if AutoItalics and (stg.Kind = skTag) and ((stg.Text = '<FI>') or (stg.Text = '<Fi>')) then
        goto NextSyntagm; // when auto-italicizing, skip the original italics

      temp := temp + stg.Text;

      if AutoItalics and (stg.Pairs.Count = 0) and (stg.Kind = skSyntagm) then
        temp := temp + '<Fi>';

      if (stg.Pairs.Count > 0) and (stg.Kind = skSyntagm) then
      begin
        if (s < (Syntagms.Count-1)) and // is not the last syntagm and
           (stg.Siblings.Count > 0) then // tem irmãos
        begin
          prox := nil;
          for m := s+1 to Syntagms.Count-1 do
          begin // looking for the next syntagm (skipping spaces, punctuation, etc.)
            if Syntagms[m].Kind = skSyntagm then
            begin
              prox := Syntagms[m];
              break;
            end;
          end;
          if (prox <> nil) and (stg.Siblings.IndexOf(prox) >= 0) then // o próximo syntagm é irmão deste?
            goto NextSyntagm;
        end;

        prox := nil;
        for p := 0 to stg.Pairs.Count-1 do
        begin // pairs
          for m := 0 to stg.Pairs[p].Strong.Count-1 do // strongs
          begin
            if (not ReusedStrongs) or (UsedStrongs.IndexOf(stg.Pairs[p].Strong) = -1) then // has this Strong already been used?
            begin // não
              temp := temp + format('<W%s>', [stg.Pairs[p].Strong[m]]);
              UsedStrongs.Add(stg.Pairs[p].Strong)
            end else // strong reutilizado
              temp := temp + format('<W%ss>', [stg.Pairs[p].Strong[m]]);
          end;

          if not Morphology then
            continue;

          for m := 0 to stg.Pairs[p].Morph.Count-1 do // Morphologylogia
            temp := temp + format('<WT%s>', [stg.Pairs[p].Morph[m]]);
        end;
      end;

      NextSyntagm:
        // write the syntagm result to 'lineStream' after computing it completely
        if temp.Contains('<W') then // se houver strongs ou Morphologylogia, então é um syntagm
          lineStream.WriteString('<wt>');
        lineStream.WriteString(temp);
    end;

    if UntranslatedStrongs then
    begin
      // checking for untranslated words from the source
      for s := 0 to PairedVerse.Syntagms.Count-1 do
      begin
      stg := PairedVerse.Syntagms[s];
      if assigned(stg.Strong) and (UsedStrongs.IndexOf(stg.Strong) = -1) then
        for m := 0 to stg.Strong.Count-1 do // strongs
        lineStream.WriteString(format('<W%sx>', [stg.Strong[m]]));
      end;
    end;

    // handling special cases for italics
    result := FFixWrongFiTag.Replace(lineStream.DataString, '$2$1'); { fix '<wt>bla <FI>ble<WG1><Fi>' case }
    result := result.Replace('<FI><wt>', '<wt><FI>', [rfReplaceAll]);
  finally
    lineStream.Destroy;
    UsedStrongs.Destroy;
  end;
end;

procedure TVerse.SetActive(const AValue: boolean);
begin
  if (AValue <> FActive) then
  begin
    if not AValue then
      DismissTranslatorNotePreview;
    FActive := AValue;
    if (AValue) then
    begin // ativar

    end
    else
    begin // desativar

    end;
  end;
end;

procedure TVerse.SetAutomaticNoteStart(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FAutomaticNoteStart = AValue then
    Exit;
  DismissTranslatorNotePreview;
  FAutomaticNoteStart := AValue;
  Inc(FVisualRevision);
  if FActive and Assigned(FVerseView) then
    RefreshVisualDocument;
end;

procedure TVerse.SetFont(const AValue: TFont);
begin
  DismissTranslatorNotePreview;
  if Assigned(FVerseViewControl) then
    FVerseViewControl.Font.Assign(AValue);
  if not FActive then
    exit;
  FPanel.Font := AValue;
  Render;
end;

procedure TVerse.SetModified(const AValue: boolean);
begin
  FModified := AValue;
  if FModified then
    NotifyVerseChanged;
end;

procedure TVerse.NotifyVerseChanged;
begin
  if (FChangeNotificationsSuspended = 0) and
    assigned(FOnVerseChanged) then
    FOnVerseChanged;
end;

function TVerse.GetRightToLeft: Boolean;
begin
  Result := FBaseDirection = tdRightToLeft;
end;

procedure TVerse.SetBaseDirection(AValue: TTextDirection);
begin
  if FBaseDirection = AValue then
    Exit;
  DismissTranslatorNotePreview;
  FBaseDirection := AValue;
  if Assigned(FRTLMenuItem) then
    FRTLMenuItem.Checked := GetRightToLeft;
  if Assigned(FVerseView) then
    RefreshVisualDocument
  else
    Render;
  if Assigned(FOnTextDirectionChange) then
    FOnTextDirectionChange(Self, FBaseDirection);
end;

procedure TVerse.SetRightToLeft(AValue: Boolean);
begin
  if AValue then
    SetBaseDirection(tdRightToLeft)
  else
    SetBaseDirection(tdLeftToRight);
end;

procedure TVerse.SetStrongsCountMode(AValue: TStrongsCountMode);
begin
  if FStrongsCountMode = AValue
     then Exit;
  FStrongsCountMode := AValue;
  Render;
end;

procedure TVerse.SetBoldWordsWithStrong(AValue: boolean);
begin
  if FBoldWordsWithStrong=AValue then Exit;
  FBoldWordsWithStrong:=AValue;
  Render;
end;

procedure TVerse.OrganizeSyntagms;

  function StartingXPos: smallint;
  begin
    if GetRightToLeft then
      result := Panel.Width-MarginWidth
    else
      result := MarginWidth;
  end;

  function WrapLine(s: TSyntagm; x: smallint): boolean;
  var
    next, prev: TSyntagm;
    punctWidth: smallint;
  begin
    result := false;
    prev := s.GetPrevVisible;
    if (s.Kind = skPunctuation) and
       (not assigned(prev) or (prev.Kind <> skPunctuation)) then // and not following another punctiation
       exit;

    next := s.GetNextVisible;
    if assigned(next) and (next.Kind = skPunctuation) then
      punctWidth := next.LabelRef.Width
    else
      punctWidth := 0;

    if GetRightToLeft then
      result := (x - punctWidth) < 15
    else
      result := (x + s.LabelRef.Width + punctWidth) > (Panel.Width-15);
  end;

  procedure PositionSyntagm(s: TSyntagm; var x, y, a: smallint);
  var
    firstLabel: boolean;
  begin
    a := max(a, s.LabelRef.Height);

    firstLabel := x = StartingXPos;
    if GetRightToLeft then
      dec(x, s.LabelRef.Width);

    if WrapLine(s, x) then
    begin
      inc(y, a);
      a := 0;
      x := IfThen(GetRightToLeft, StartingXPos-s.LabelRef.Width, StartingXPos);
      firstLabel := true;
    end;

    s.LabelRef.SetBounds(x, y, s.LabelRef.Width, s.LabelRef.Height);

    if firstLabel and (s.Kind = skWhitespace) then
      s.LabelRef.Visible := false // hiding spaces at the beginning of the line
    else begin
      s.LabelRef.Show;
      if not GetRightToLeft then
        inc(x, s.LabelRef.Width);
    end;
  end;

var
  syntagm: TSyntagm;
  x, y, h: smallint;
begin
  if Assigned(FVerseView) then
    Exit;

  if not Active or FDestroying or not assigned(FSyntagms) then
    exit;

  x := StartingXPos; // starting x position
  y := 0;            // starting y position
  h := 0;            // line height
  for syntagm in FSyntagms do
    if assigned(syntagm.LabelRef) then
      PositionSyntagm(syntagm, x, y, h);

  if (FStrongsCountMode <> scNone) and assigned(FStrongCount) then
    PositionSyntagm(FStrongCount, x, y, h);
end;

procedure TVerse.SetPairedVerse(PairVerse: TVerse);
begin
  FPairedVerse := PairVerse;
  if Assigned(PairVerse) and (PairVerse.PairedVerse <> self) then
    PairVerse.PairedVerse := self;
end;

procedure TVerse.SelectSyntagmList(lst: string);
var
  tokens: TStringList;
  i: Integer;
  idx: Integer;
  ok: Boolean;
begin
  tokens := TStringList.Create;
  try
    tokens.DelimitedText := lst;
    tokens.CustomSort(@CompareIndexes);
    for i := 0 to tokens.Count - 1 do
    begin
      ok := TryStrToInt(tokens[i], idx);
      if not ok then
      begin
        //AIDebugLogFmt('[SelectSyntagmList] Invalid token (not integer): %s', [tokens[i]]);
        Continue;
      end;
      if (idx < 0) or (idx >= Syntagms.Count) then
      begin
        //AIDebugLogFmt('[SelectSyntagmList] Index out of range: %d (Count=%d)', [idx, Syntagms.Count]);
        Continue;
      end;
      if not (Syntagms[idx].Kind in [skSyntagm, skPunctuation]) then
      begin
        //AIDebugLogFmt('[SelectSyntagmList] Index %d is not selectable (Kind=%d)', [idx, Ord(Syntagms[idx].Kind)]);
        Continue;
      end;
      Syntagms[idx].AddToSelection;
    end;
  finally
    tokens.Free;
  end;
end;

procedure TVerse.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    FEdit.Visible := False
  else if Key = VK_RETURN then
    EditConfirm;
end;

procedure TVerse.EditExit(Sender: TObject);
begin
  FEdit.Visible := False;
end;

procedure TVerse.EditConfirm;
{var
  s: TSyntagmToken;
  varredorXML: TONTTokenizer;
  syntagm, newSyntagm: TSyntagm;
  i, offset, j: integer;}
begin
  {
  syntagm := TSyntagm(FEdit.Tag);
  i := FSyntagms.IndexOf(syntagm);
  FSyntagms.Remove(syntagm);
  FSelection.Remove(syntagm);
  offset := 0;
  newSyntagm := nil;

  varredorXML := TTokenizerFactory.CreateTokenizer(FTokenizerVersion, FEdit.Caption);
  while varredorXML.ReadSyntagm(s) <> skNull do
  begin
    if (s.Kind = skWhitespace) and (s.Value = '|') then
    begin
      if assigned(newSyntagm) then
        newSyntagm.TextoBruto := newSyntagm.TextoBruto + s.Value;
      continue;
    end;

    newSyntagm := TSyntagm.Create(self, s);
    FSyntagms.Insert(i, newSyntagm);
    Inc(i); Inc(Offset);
  end;
  varredorXML.Destruir;

  // copying data from the clicked syntagm to the first parsed syntagm
  for j:=(i-Offset) to i+Offset do
  begin
    newSyntagm := FSyntagms[j];
    if newSyntagm.Tipo = skSyntagm then
    begin
      for i:=0 to syntagm.Irmaos.Count-1 do
      begin
        newSyntagm.Irmaos.Add(syntagm.Irmaos[i]);
        syntagm.Irmaos[i].Irmaos.Remove(syntagm);
        syntagm.Irmaos[i].Irmaos.Add(newSyntagm);
      end;
      for i:=0 to syntagm.Pares.Count-1 do
      begin
        newSyntagm.Correlacionado := true;
        newSyntagm.Pares.Add(syntagm.Pares[i]);
        syntagm.Pares[i].Pares.Remove(syntagm);
        syntagm.Pares[i].Pares.Add(newSyntagm);
      end;
      break;
    end;
  end;

  syntagm.Destruir;
  UpdateInternalXML;
  EditExit(Self);
  OrganizeSyntagms;
  }
end;

{ Updates the internal XML from syntagm text }
procedure TVerse.UpdateInternalXML;
var
  s: TSyntagm;
begin
  FXML := '';
  for s in FSyntagms do
    FXML := FXML + s.RawText;

  FXMLModified := true;
  Modified := true;
end;

procedure TVerse.Render;
var
  s: TSyntagm;
  //starttime: DWord;
begin
  if not FActive or not assigned(FSyntagms) or FRendering then
    exit;

  //starttime := getTickCount;
  if Assigned(FVerseView) then
  begin
    RenderStrongCount;
    RefreshVisualDocument;
    Exit;
  end;

  FRendering := True;
  try
    if assigned(FPanel) then
      FPanel.DisableAutoSizing;
    for s in FSyntagms do
      s.Render;
    RenderStrongCount;
    if assigned(FPanel) then
      FPanel.EnableAutoSizing;
    OrganizeSyntagms;
  finally
    FRendering := False;
  end;
  //DebugLn('  TVerse.Render: %d milliseconds', [getTickCount-starttime]);
end;

procedure TVerse.RenderStrongCount;
begin
  ClearStrongCount;
  if FStrongsCountMode = scNone then
    exit;
  if FSyntagms.Empty then
    exit;

  UpdateStrongCount;
  if not Assigned(FVerseView) then
    FStrongCount.Render;
end;

procedure TVerse.ClearStrongCount;
begin
  if not assigned(FStrongCount) then
    exit;

  FStrongCount.Destroy;
  FStrongCount := nil;
end;

procedure TVerse.UpdateStrongCount;

  function GetStrongsCount(s: TSyntagm): integer;
  begin
    case FStrongsCountMode of
      scCountStrongs:
        result := s.StrongsCount;
      scCountWords:
        result := s.StrongsWordsCount;
    else
      result := 0;
    end;
  end;

var
  token: TSyntagmToken;
  s, p: TSyntagm;
  count: integer;
  unique: TSyntagmList;
begin
  count := 0;
  unique := TSyntagmList.Create;
  for s in FSyntagms do
  begin
    if s.Kind <> skSyntagm then
      continue;
    if unique.IndexOf(s) <> -1 then
      continue;
    inc(count, GetStrongsCount(s));
    if not s.HasStrongs and assigned(s.Siblings) then
      for p in s.Siblings do
        if not p.HasStrongs then
          unique.Add(p);
  end;
  unique.Destroy;

  with token do
  begin
    Kind        := skStrongCount;
    Value       := Format(' (%d strongs)', [count]);
    Color       := clRed;
    Superscript := True;
    Italic      := False;
  end;
  FStrongCount := TSyntagm.Create(token, self);
end;

function TVerse.GetTokens: string;
var
  s: TSyntagm;
  tokens: TStringStream;
begin
  tokens := TStringStream.Create('');
  for s in FSyntagms do
    tokens.WriteString(s.Gist);
  result:= tokens.DataString;
  tokens.Destroy;
end;

procedure TVerse.FindAndSelectSyntagms(Words: TStringList; Partial: Boolean = False; JoinConnected: Boolean = True);
var
  I: Integer;
  SelectedIndexes: TSyntagmIndexArray;

begin
  ClearSelection;

  if not Assigned(Words) or (Words.Count = 0) then
    Exit;

  AIDebugLogFmt('[FindAndSelectSyntagms] Verse: %s - Searching for %d words, Partial=%s, JoinConnected=%s',
                [Reference, Words.Count, BoolToStr(Partial, True), BoolToStr(JoinConnected, True)]);
  for I := 0 to Words.Count - 1 do
    AIDebugLogFmt('[FindAndSelectSyntagms]   Word[%d]: "%s"', [I, Words[I]]);
  AIDebugLogFmt('[FindAndSelectSyntagms] Total syntagms in verse: %d', [FSyntagms.Count]);

  SelectedIndexes := ResolveSyntagmSelectionIndexes(FSyntagms, Words, Partial, JoinConnected);
  AIDebugLogVerboseFmt('[FindAndSelectSyntagms] Resolved %d syntagm indexes', [Length(SelectedIndexes)]);

  for I := 0 to High(SelectedIndexes) do
  begin
    if (SelectedIndexes[I] < 0) or (SelectedIndexes[I] >= FSyntagms.Count) then
      Continue;

    AIDebugLogVerboseFmt('[FindAndSelectSyntagms]   Adding index %d to selection: "%s"',
      [SelectedIndexes[I], FSyntagms[SelectedIndexes[I]].Text]);
    FSyntagms[SelectedIndexes[I]].AddToSelection;
  end;

  AIDebugLogVerbose('[FindAndSelectSyntagms] Completed successfully');
end;

procedure TVerse.OnCopySyntagmTags(Sender: TObject);
begin
  SyntagmClipboard := TSyntagm(FSyntagmPopupMenu.Tag);
end;

procedure TVerse.OnPasteSyntagmTags(Sender: TObject);
var
  s: TSyntagm;
  t: string;
begin
  if not assigned(SyntagmClipboard) then
    exit;

  s := TSyntagm(FSyntagmPopupMenu.Tag);

  s.RawText := s.Text;
  if TMenuItem(Sender).Caption <> SPasteMorphoTags then
  begin
    s.Strong.Clear;
    for t in SyntagmClipboard.Strong do
    begin
      s.Strong.Add(t);
      s.RawText := Format('%s<W%s>', [s.RawText, t]);
    end;
  end;

  if TMenuItem(Sender).Caption <> SPasteStrongTags then
  begin
    s.Morph.Clear;
    for t in SyntagmClipboard.Morph do
    begin
      s.Morph.Add(t);
      s.RawText := Format('%s<W%s>', [s.RawText, t]);
    end;
  end;

  UpdateInternalXML;
  PairedVerse.Modified := true;

  ClearSelection;
  PairedVerse.ClearSelection;
  s.AddToSelection;

  Render;
end;

procedure TVerse.OnSaveTextToFile(Sender: TObject);
begin
  if assigned(FOnExportText) then
     FOnExportText(Self);
end;

procedure TVerse.OnCopyTokensToClipboard(Sender: TObject);
begin
  clipboard.AsText := GetTokens();
end;

procedure TVerse.OnCopyInterlinearVerseToClipboard(Sender: TObject);
begin
  clipboard.AsText := GetMySwordInterlinearLine;
end;

procedure TVerse.OnCopyXMLToClipboard(Sender: TObject);
begin
  clipboard.AsText := GetPairs;
end;

procedure TVerse.OnRightToLeft(Sender: TObject);
begin
  RightToLeft := not GetRightToLeft;
end;

procedure TVerse.OnVerseMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    if Assigned(FVersePopupMenu) then
      FVersePopupMenu.PopUp;
end;

procedure TVerse.OnResize(Sender: TObject);
begin
  if FRendering then
    Exit;
  if Assigned(FVerseView) then
  begin
    FVerseView.NotifyChanges([vvcViewport], FVisualRevision);
    OnVisualContentHeightChanged(FVerseViewControl);
  end
  else
    OrganizeSyntagms;
end;

procedure TVerse.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    FPanel.Font.Size := FPanel.Font.Size + Sign(WheelDelta) * 1;
    if Assigned(FVerseViewControl) then
      FVerseViewControl.Font.Assign(FPanel.Font);
    Render;
    Handled := True;
  end;
end;

procedure TVerse.InitSyntagmPopupMenu;
var
  Item: TMenuItem;
begin
  FSyntagmPopupMenu := TPopupMenu.Create(FPanel);
  FSyntagmPopupMenu.Parent := FPanel;

  Item := TMenuItem.Create(FSyntagmPopupMenu);
  Item.Caption := SCopyTags;
  Item.OnClick := @OnCopySyntagmTags;
  FSyntagmPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FSyntagmPopupMenu);
  Item.Caption := SPasteAllTags;
  Item.OnClick := @OnPasteSyntagmTags;
  FSyntagmPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FSyntagmPopupMenu);
  Item.Caption := SPasteStrongTags;
  Item.OnClick := @OnPasteSyntagmTags;
  FSyntagmPopupMenu.Items.Add(Item);

  Item := TMenuItem.Create(FSyntagmPopupMenu);
  Item.Caption := SPasteMorphoTags;
  Item.OnClick := @OnPasteSyntagmTags;
  FSyntagmPopupMenu.Items.Add(Item);
end;

procedure TVerse.InitVersePopupMenu;
var
  Item: TMenuItem;
begin
  FVersePopupMenu := TPopupMenu.Create(FPanel);
  FVersePopupMenu.Parent := FPanel;
  FVersePopupMenu.Tag := PtrInt(Self);

  FRTLMenuItem := TMenuItem.Create(FVersePopupMenu);
  FRTLMenuItem.Caption := SRightToLeft;
  FRTLMenuItem.OnClick := @OnRightToLeft;
  FVersePopupMenu.Items.Add(FRTLMenuItem);

  Item := TMenuItem.Create(FVersePopupMenu);
  Item.Caption := SSaveToFile;
  Item.OnClick := @OnSaveTextToFile;
  FVersePopupMenu.Items.Add(Item);

  {$IFDEF DEBUG}
    FVersePopupMenu.Items.AddSeparator;

    Item := TMenuItem.Create(FVersePopupMenu);
    Item.Caption := SCopyTokensToClipboard;
    Item.OnClick := @OnCopyTokensToClipboard;
    FVersePopupMenu.Items.Add(Item);

    Item := TMenuItem.Create(FVersePopupMenu);
    Item.Caption := SCopySyntagmsJSONToClipboard;
    Item.OnClick := @OnCopySyntagmsAsJSON;
    FVersePopupMenu.Items.Add(Item);

    Item := TMenuItem.Create(FVersePopupMenu);
    Item.Caption := SCopyInterlinearVerseToClipboard;
    Item.OnClick := @OnCopyInterlinearVerseToClipboard;
    FVersePopupMenu.Items.Add(Item);

    Item := TMenuItem.Create(FVersePopupMenu);
    Item.Caption := SCopyXMLToClipboard;
    Item.OnClick := @OnCopyXMLToClipboard;
    FVersePopupMenu.Items.Add(Item);

    Item := TMenuItem.Create(FVersePopupMenu);
    Item.Caption := SCopyAssociationsJSONToClipboard;
    Item.OnClick := @OnCopyAssociationsJSONToClipboard;
    FVersePopupMenu.Items.Add(Item);

  {$ENDIF}
end;

constructor TVerse.Create(const AReference: string = '');
begin
  Create(nil, AReference);
end;

procedure TVerse.OnCopySyntagmsAsJSON(Sender: TObject);
  // local helper to convert a TStringList to a JSON array
  function StringListToJSONArray(lst: TStringList): string;
  var
    i: Integer;
    arr: string;
  begin
    if lst = nil then
    begin
      Result := '[]';
      Exit;
    end;
    arr := '[';
    for i := 0 to lst.Count - 1 do
    begin
      if i > 0 then
        arr := arr + ',';
      arr := arr + QuotedStr(lst[i]);
    end;
    arr := arr + ']';
    Result := arr;
  end;
var
  i: Integer;
  s: TSyntagm;
  jsonItems: TStringList;
  jsonStr: string;
  strongsJSON, morphJSON, kindStr: string;
begin
  jsonItems := TStringList.Create;
  try
    for i := 0 to FSyntagms.Count - 1 do
    begin
      s := FSyntagms[i];
      strongsJSON := StringListToJSONArray(s.Strong);
      morphJSON := StringListToJSONArray(s.Morph);
      kindStr := GetEnumName(TypeInfo(TSyntagmKind), Ord(s.Kind)); // convert kind to text
      jsonItems.Add(Format('{"text": "%s", "index": %d, "kind": "%s", "strongs": %s, "morph": %s}'#13#10,
        [s.Text, i, kindStr, strongsJSON, morphJSON]));
    end;
    // Prevent TStringList from adding extra quotes by disabling quoting
    jsonItems.Delimiter := ',';
    jsonItems.StrictDelimiter := True;
    jsonItems.QuoteChar := #0;
    jsonStr := '{"syntagmas": [' + jsonItems.DelimitedText + ']}';
    Clipboard.AsText := jsonStr;
  finally
    jsonItems.Free;
  end;
end;

function TVerse.GetAssociationsJSON: string;
var
  associations: TStringList;
  processed: TList; // list to avoid duplicate processing
  s, p: TSyntagm;
  i: integer;
  wordA, wordB: string;
  jsonList: string;
begin
  associations := TStringList.Create;
  processed := TList.Create;
  try
    for i := 0 to FSyntagms.Count - 1 do
    begin
      s := FSyntagms[i];
      if (s.Pairs.Count = 0) or (processed.IndexOf(s) <> -1) then
        Continue;
      // Mark the group as processed
      processed.Add(s);
      for p in s.Siblings do
        processed.Add(p);
      // Build side A by concatenating s.Text and its siblings
      wordA := s.Text;
      for p in s.Siblings do
        wordA := wordA + ' ' + p.Text;
      // Build side B by concatenating texts of each paired syntagm
      wordB := '';
      for p in s.Pairs do
      begin
        if wordB = '' then
          wordB := p.Text
        else
          wordB := wordB + ' ' + p.Text;
      end;
      // Add association in desired format
      associations.Add(Format('["%s", "%s"]', [wordA, wordB]));
    end;
    // Manually join the association elements without extra quotes
    jsonList := '';
    for i := 0 to associations.Count - 1 do
    begin
      if i > 0 then
        jsonList := jsonList + ',';
      jsonList := jsonList + associations[i];
    end;
    Result := '{"data": [' + jsonList + ']}';
  finally
    processed.Free;
    associations.Free;
  end;
end;

function TVerse.HasAssociations: boolean;
var
  s: TSyntagm;
begin
  Result := False;
  if not Assigned(FSyntagms) then Exit;
  for s in FSyntagms do
    if s.Pairs.Count > 0 then
    begin
      Result := True;
      Break;
    end;
end;

function TVerse.CanChangeTextPreservingAssociationIdentity(
  const AXML: string): Boolean;

  function NextContentUnit(AList: TSyntagmList; var AIndex: Integer;
    out AUnit: TSyntagm): Boolean;
  begin
    AUnit := nil;
    while Assigned(AList) and (AIndex < AList.Count) do
    begin
      AUnit := AList[AIndex];
      Inc(AIndex);
      if AUnit.Kind in [skSyntagm, skWhitespace, skPunctuation] then
        Exit(True);
    end;
    AUnit := nil;
    Result := False;
  end;

  procedure FreeParsedSyntagms(var AList: TSyntagmList);
  var
    Index: Integer;
  begin
    if not Assigned(AList) then
      Exit;
    for Index := AList.Count - 1 downto 0 do
      AList[Index].Free;
    FreeAndNil(AList);
  end;

var
  ExistingIndex, CandidateIndex: Integer;
  ExistingUnit, CandidateUnit: TSyntagm;
  HasExistingUnit, HasCandidateUnit: Boolean;
  CandidateSyntagms: TSyntagmList;
begin
  { Formatting is unrestricted when there is no persisted identity to keep.
    With Associations, tags may be added only at existing content-unit
    boundaries. Parsing the candidate catches a tag inserted in the middle of
    a word or whitespace because that unit would be split in two. }
  if not HasAssociations then
    Exit(True);

  Result := False;
  CandidateSyntagms := nil;
  try
    try
      CandidateSyntagms := FONTParser.ParseLine(AXML, nil);
      ExistingIndex := 0;
      CandidateIndex := 0;
      Result := True;
      repeat
        HasExistingUnit := NextContentUnit(FSyntagms, ExistingIndex,
          ExistingUnit);
        HasCandidateUnit := NextContentUnit(CandidateSyntagms, CandidateIndex,
          CandidateUnit);
        if HasExistingUnit <> HasCandidateUnit then
        begin
          Result := False;
          Break;
        end;
        if not HasExistingUnit then
          Break;
        if (ExistingUnit.Kind <> CandidateUnit.Kind) or
           (ExistingUnit.Text <> CandidateUnit.Text) or
           (ExistingUnit.RawText <> CandidateUnit.RawText) then
        begin
          Result := False;
          Break;
        end;
      until False;
    except
      on E: Exception do
        Result := False;
    end;
  finally
    FreeParsedSyntagms(CandidateSyntagms);
  end;
end;

function TVerse.TryApplyTracedEdit(const ANewXML: string;
  const ATrace: TVerseEditTrace;
  out AImpact: TAssociationEditImpact): Boolean;
type
  TContentSpan = record
    Syntagm: TSyntagm;
    ListIndex: Integer;
    StartOffset: Integer;
    EndOffset: Integer;
  end;
  TContentSpanArray = array of TContentSpan;
  TIntegerArray = array of Integer;

  function IsContentUnit(ASyntagm: TSyntagm): Boolean;
  begin
    Result := Assigned(ASyntagm) and
      (ASyntagm.Kind in [skSyntagm, skWhitespace, skPunctuation]);
  end;

  function IsAssociationUnit(ASyntagm: TSyntagm): Boolean;
  begin
    Result := Assigned(ASyntagm) and
      (ASyntagm.Kind in [skSyntagm, skPunctuation]);
  end;

  function HasExactIdentity(AExisting, ACandidate: TSyntagm): Boolean;
  begin
    Result := Assigned(AExisting) and Assigned(ACandidate) and
      (AExisting.Kind = ACandidate.Kind) and
      (AExisting.Text = ACandidate.Text) and
      (AExisting.RawText = ACandidate.RawText);
  end;

  function HasLegacyIdentity(AExisting, ACandidate: TSyntagm): Boolean;
  begin
    Result := Assigned(AExisting) and Assigned(ACandidate) and
      (AExisting.Kind = ACandidate.Kind) and
      (AExisting.Text = ACandidate.Text);
  end;

  procedure FreeParsedSyntagms(var AList: TSyntagmList);
  var
    Index: Integer;
  begin
    if not Assigned(AList) then
      Exit;
    for Index := AList.Count - 1 downto 0 do
      AList[Index].Free;
    FreeAndNil(AList);
  end;

  function DecodeVisibleFragment(const ARaw: string;
    const ACodec: ITheWordOntCodecV2;
    out AVisible: UnicodeString): Boolean;
  var
    MetadataDocument: TTheWordVerseDocument;
  begin
    AVisible := '';
    MetadataDocument := nil;
    try
      MetadataDocument := ACodec.DecodeVerse(UTF8Decode(ARaw));
      AVisible := MetadataDocument.VisibleText;
      Result := True;
    except
      Result := False;
    end;
    MetadataDocument.Free;
  end;

  function BuildContentSpans(AList: TSyntagmList;
    const AVisibleText: UnicodeString; const ACodec: ITheWordOntCodecV2;
    out ASpans: TContentSpanArray): Boolean;
  var
    Content, ProtectedText: UnicodeString;
    Cursor, Index, SpanIndex: Integer;
    Syntagm: TSyntagm;
  begin
    ASpans := nil;
    Cursor := 0;
    Result := Assigned(AList);
    if not Result then
      Exit;

    for Index := 0 to AList.Count - 1 do
    begin
      Syntagm := AList[Index];
      if IsContentUnit(Syntagm) then
      begin
        { Text is already the visible value for words and punctuation. The
          legacy tokenizer can keep a structural `|` inside whitespace, even
          though the editor correctly gives it zero visual width. Avoid a
          full codec document allocation for every ordinary Syntagm. }
        if Syntagm.Kind = skWhitespace then
          Content := UTF8Decode(StringReplace(Syntagm.Text, '|', '',
            [rfReplaceAll]))
        else
          Content := UTF8Decode(Syntagm.Text);
        if (Length(Content) > Length(AVisibleText) - Cursor) or
           (Copy(AVisibleText, Cursor + 1, Length(Content)) <> Content) then
          Exit(False);
        SpanIndex := Length(ASpans);
        SetLength(ASpans, SpanIndex + 1);
        ASpans[SpanIndex].Syntagm := Syntagm;
        ASpans[SpanIndex].ListIndex := Index;
        ASpans[SpanIndex].StartOffset := Cursor;
        Inc(Cursor, Length(Content));
        ASpans[SpanIndex].EndOffset := Cursor;
      end
      else
      begin
        if not DecodeVisibleFragment(Syntagm.Text, ACodec,
          ProtectedText) then
          Exit(False);
        if (Length(ProtectedText) > Length(AVisibleText) - Cursor) or
           (Copy(AVisibleText, Cursor + 1, Length(ProtectedText)) <>
            ProtectedText) then
          Exit(False);
        Inc(Cursor, Length(ProtectedText));
      end;
    end;
    Result := Cursor = Length(AVisibleText);
  end;

  procedure InitializeMapping(var AValues: TIntegerArray; ALength: Integer);
  var
    Index: Integer;
  begin
    SetLength(AValues, ALength);
    for Index := 0 to High(AValues) do
      AValues[Index] := -1;
  end;

  function ReserveExactMatches(const AOldSpans,
    ANewSpans: TContentSpanArray; const AOriginMap: TTextOriginMap;
    var AOldToNew, ANewToOld: TIntegerArray): Boolean;
  var
    CandidateCount, CandidateIndex, CurrentStart, HighIndex, Index,
      LowIndex, MatchIndex, MiddleIndex, OriginalLength: Integer;
  begin
    Result := Assigned(AOriginMap);
    if not Result then
      Exit;
    for Index := 0 to High(AOldSpans) do
    begin
      OriginalLength := AOldSpans[Index].EndOffset -
        AOldSpans[Index].StartOffset;
      if (OriginalLength <= 0) or
         not AOriginMap.TryProjectOriginalRange(
           AOldSpans[Index].StartOffset, OriginalLength,
           CurrentStart) then
        Continue;

      { Spans are ordered by their visible start. Locate only the candidates
        at this projected position instead of scanning the whole Verse for
        every original Syntagm. }
      LowIndex := 0;
      HighIndex := Length(ANewSpans);
      while LowIndex < HighIndex do
      begin
        MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
        if ANewSpans[MiddleIndex].StartOffset < CurrentStart then
          LowIndex := MiddleIndex + 1
        else
          HighIndex := MiddleIndex;
      end;

      CandidateCount := 0;
      MatchIndex := -1;
      CandidateIndex := LowIndex;
      while (CandidateIndex <= High(ANewSpans)) and
            (ANewSpans[CandidateIndex].StartOffset = CurrentStart) do
      begin
        if (ANewSpans[CandidateIndex].EndOffset =
             CurrentStart + OriginalLength) and
           HasExactIdentity(AOldSpans[Index].Syntagm,
             ANewSpans[CandidateIndex].Syntagm) then
        begin
          Inc(CandidateCount);
          MatchIndex := CandidateIndex;
        end;
        Inc(CandidateIndex);
      end;
      if CandidateCount = 1 then
      begin
        if ANewToOld[MatchIndex] >= 0 then
          Exit(False);
        AOldToNew[Index] := MatchIndex;
        ANewToOld[MatchIndex] := Index;
      end;
    end;
  end;

  procedure ReconcileLegacyGap(const AOldSpans,
    ANewSpans: TContentSpanArray; AOldStart, AOldFinish,
    ANewStart, ANewFinish: Integer;
    var AOldToNew, ANewToOld: TIntegerArray);
  var
    CandidateIndex, OldIndex, SearchIndex: Integer;
    Found: Boolean;
  begin
    OldIndex := AOldStart;
    CandidateIndex := ANewStart;
    while (OldIndex <= AOldFinish) and
          (CandidateIndex <= ANewFinish) do
    begin
      if HasLegacyIdentity(AOldSpans[OldIndex].Syntagm,
        ANewSpans[CandidateIndex].Syntagm) then
      begin
        AOldToNew[OldIndex] := CandidateIndex;
        ANewToOld[CandidateIndex] := OldIndex;
        Inc(OldIndex);
        Inc(CandidateIndex);
      end
      else if not IsAssociationUnit(AOldSpans[OldIndex].Syntagm) then
        Inc(OldIndex)
      else if not IsAssociationUnit(
        ANewSpans[CandidateIndex].Syntagm) then
        Inc(CandidateIndex)
      else
      begin
        Found := False;
        if AOldSpans[OldIndex].Syntagm.Pairs.Count > 0 then
          for SearchIndex := CandidateIndex to ANewFinish do
            if HasLegacyIdentity(AOldSpans[OldIndex].Syntagm,
              ANewSpans[SearchIndex].Syntagm) then
            begin
              Found := True;
              CandidateIndex := SearchIndex;
              Break;
            end;
        if Found then
        begin
          AOldToNew[OldIndex] := CandidateIndex;
          ANewToOld[CandidateIndex] := OldIndex;
          Inc(OldIndex);
          Inc(CandidateIndex);
        end
        else
          Inc(OldIndex);
      end;
    end;
  end;

  function ReconcileRawGaps(const AOldSpans,
    ANewSpans: TContentSpanArray; var AOldToNew,
    ANewToOld: TIntegerArray): Boolean;
  var
    AnchorNew, Index, PreviousNew, PreviousOld: Integer;
  begin
    PreviousOld := 0;
    PreviousNew := 0;
    for Index := 0 to High(AOldSpans) do
      if AOldToNew[Index] >= 0 then
      begin
        AnchorNew := AOldToNew[Index];
        if AnchorNew < PreviousNew then
          Exit(False);
        ReconcileLegacyGap(AOldSpans, ANewSpans,
          PreviousOld, Index - 1, PreviousNew, AnchorNew - 1,
          AOldToNew, ANewToOld);
        PreviousOld := Index + 1;
        PreviousNew := AnchorNew + 1;
      end;
    ReconcileLegacyGap(AOldSpans, ANewSpans,
      PreviousOld, High(AOldSpans), PreviousNew, High(ANewSpans),
      AOldToNew, ANewToOld);
    Result := True;
  end;

  procedure ClearOrphanedSiblingGroups(AList: TSyntagmList);
  var
    Index: Integer;
    Syntagm: TSyntagm;
  begin
    if not Assigned(AList) then
      Exit;
    for Index := 0 to AList.Count - 1 do
    begin
      Syntagm := AList[Index];
      if IsAssociationUnit(Syntagm) and
         (Syntagm.Pairs.Count = 0) and
         (Syntagm.Siblings.Count > 0) then
        Syntagm.Disassociate;
    end;
  end;

  function ApplyLegacyFallback: Boolean;
  var
    AssociatedBefore: TList;
    Candidate: TSyntagmList;
    Index: Integer;
    Syntagm: TSyntagm;
  begin
    Result := False;
    Candidate := nil;
    AssociatedBefore := TList.Create;
    try
      { Parse once before entering the legacy mutating path. Structural input
        failures therefore leave the live Verse untouched. }
      Candidate := FONTParser.ParseLine(ANewXML, nil);
      FreeParsedSyntagms(Candidate);
      for Syntagm in FSyntagms do
        if Syntagm.Pairs.Count > 0 then
          AssociatedBefore.Add(Syntagm);

      ReplaceTextPreservingAssociations(ANewXML, False);
      AImpact.UsedLegacyFallback := True;
      for Index := 0 to AssociatedBefore.Count - 1 do
        if FSyntagms.IndexOf(AssociatedBefore[Index]) >= 0 then
          Inc(AImpact.PreservedMembers)
        else
          Inc(AImpact.RemovedMembers);
      Result := True;
    finally
      FreeParsedSyntagms(Candidate);
      AssociatedBefore.Free;
    end;
  end;

var
  CandidateSyntagms, ResultSyntagms: TSyntagmList;
  Codec: ITheWordOntCodecV2;
  OldDocument, NewDocument: TTheWordVerseDocument;
  OldSpans, NewSpans: TContentSpanArray;
  OldToNew, NewToOld: TIntegerArray;
  OldReused: array of Boolean;
  CandidateReused: array of Boolean;
  CandidateIndex, Index, OldIndex: Integer;
  OldActive, RawFallbackAllowed, TraceValidated: Boolean;
  OldSyntagm, NewSyntagm: TSyntagm;
begin
  AImpact.PreservedMembers := 0;
  AImpact.RemovedMembers := 0;
  AImpact.UsedLegacyFallback := False;
  AImpact.ErrorMessage := '';
  Result := False;

  if not Assigned(ATrace) then
  begin
    AImpact.ErrorMessage := 'The edit provenance trace is missing.';
    Exit;
  end;
  if (ATrace.TokenizerVersion <> 0) and
     (TTokenizerFactory.NormalizeVersion(ATrace.TokenizerVersion) <>
      FTokenizerVersion) then
  begin
    AImpact.ErrorMessage := 'The edit trace uses a different tokenizer version.';
    Exit;
  end;

  Codec := TTheWordOntCodecV2.Create;
  OldDocument := nil;
  NewDocument := nil;
  CandidateSyntagms := nil;
  ResultSyntagms := nil;
  TraceValidated := False;
  RawFallbackAllowed := False;
  try
    try
      OldDocument := Codec.DecodeVerse(UTF8Decode(FXML));
      NewDocument := Codec.DecodeVerse(UTF8Decode(ANewXML));
      if ATrace.OriginalText <> OldDocument.VisibleText then
        raise Exception.Create(
          'The edit trace baseline does not match the current Verse text.');
      if ATrace.CurrentText <> NewDocument.VisibleText then
        raise Exception.Create(
          'The edit trace result does not match the edited Verse text.');
      if (ATrace.OriginMap.OriginalLength <> Length(ATrace.OriginalText)) or
         (ATrace.OriginMap.CurrentLength <> Length(ATrace.CurrentText)) then
        raise Exception.Create('The edit provenance map is inconsistent.');
      TraceValidated := True;
      RawFallbackAllowed := True;

      if ATrace.Mode = vetmLegacyFallback then
      begin
        for Index := 0 to FSyntagms.Count - 1 do
          if FSyntagms[Index].HasStrongs or FSyntagms[Index].HasMorph then
            raise Exception.Create(
              'A legacy fallback cannot prove a Strong or morphology word intact.');
        Result := ApplyLegacyFallback;
        Exit;
      end;

      CandidateSyntagms := FONTParser.ParseLine(ANewXML, Self);
      for Index := 0 to FSyntagms.Count - 1 do
        if FSyntagms[Index].HasStrongs or FSyntagms[Index].HasMorph then
        begin
          { Until spans prove every lexical word intact, an imprecise Raw
            fallback would be able to edit protected annotations. }
          RawFallbackAllowed := False;
          Break;
        end;
      if not BuildContentSpans(FSyntagms, OldDocument.VisibleText,
        Codec, OldSpans) then
        raise Exception.Create(
          'Could not project the existing Syntagms into visible text.');
      if not BuildContentSpans(CandidateSyntagms,
        NewDocument.VisibleText, Codec, NewSpans) then
        raise Exception.Create(
          'Could not project the edited Syntagms into visible text.');

      InitializeMapping(OldToNew, Length(OldSpans));
      InitializeMapping(NewToOld, Length(NewSpans));
      if not ReserveExactMatches(OldSpans, NewSpans,
        ATrace.OriginMap, OldToNew, NewToOld) then
        raise Exception.Create('The exact Syntagm projection is ambiguous.');
      for Index := 0 to High(OldSpans) do
        if (OldSpans[Index].Syntagm.HasStrongs or
            OldSpans[Index].Syntagm.HasMorph) and
           (OldToNew[Index] < 0) then
        begin
          { Lexically annotated words remain protected in this milestone.
            They must be proven byte-for-byte intact by provenance; the Raw
            heuristic is deliberately not allowed to guess their identity. }
          RawFallbackAllowed := False;
          raise Exception.Create(
            'A word with Strong or morphology data was edited.');
        end;
      RawFallbackAllowed := True;
      if (ATrace.Mode = vetmRawConservative) and
         not ReconcileRawGaps(OldSpans, NewSpans,
           OldToNew, NewToOld) then
        raise Exception.Create('The conservative Raw projection is ambiguous.');

      SetLength(OldReused, FSyntagms.Count);
      SetLength(CandidateReused, CandidateSyntagms.Count);
      for Index := 0 to High(OldSpans) do
        if OldToNew[Index] >= 0 then
        begin
          OldReused[OldSpans[Index].ListIndex] := True;
          CandidateReused[NewSpans[OldToNew[Index]].ListIndex] := True;
        end;

      { Allocate and populate the replacement list before changing any live
        object or Association. From this point onward the commit consists only
        of deterministic pointer/list operations. }
      ResultSyntagms := TSyntagmList.Create;
      for CandidateIndex := 0 to CandidateSyntagms.Count - 1 do
      begin
        OldIndex := -1;
        for Index := 0 to High(NewSpans) do
          if NewSpans[Index].ListIndex = CandidateIndex then
          begin
            if NewToOld[Index] >= 0 then
              OldIndex := OldSpans[NewToOld[Index]].ListIndex;
            Break;
          end;
        if OldIndex >= 0 then
          ResultSyntagms.Add(FSyntagms[OldIndex])
        else
          ResultSyntagms.Add(CandidateSyntagms[CandidateIndex]);
      end;

      for Index := 0 to FSyntagms.Count - 1 do
        if FSyntagms[Index].Pairs.Count > 0 then
        begin
          if OldReused[Index] then
            Inc(AImpact.PreservedMembers)
          else
            Inc(AImpact.RemovedMembers);
        end;
    except
      on E: Exception do
      begin
        AImpact.ErrorMessage := E.Message;
        FreeParsedSyntagms(CandidateSyntagms);
        FreeAndNil(ResultSyntagms);
        if TraceValidated and RawFallbackAllowed and
           (ATrace.Mode = vetmRawConservative) then
        begin
          AImpact.PreservedMembers := 0;
          AImpact.RemovedMembers := 0;
          AImpact.ErrorMessage := '';
          try
            Result := ApplyLegacyFallback;
          except
            on FallbackError: Exception do
            begin
              AImpact.ErrorMessage := FallbackError.Message;
              Result := False;
            end;
          end;
          Exit;
        end;
        Exit(False);
      end;
    end;

    OldActive := FActive;
    Inc(FVisualUpdatesSuspended);
    try
      DismissTranslatorNotePreview;
      FRenderedTranslatorNotes := nil;
      Application.RemoveAsyncCalls(Self);
      FVisualRefreshQueued := False;
      Inc(FVisualRevision);
      if Assigned(FVerseView) then
        FVerseView.SetDocument(nil);
      FLastHoverLogicalIndex := -1;
      FActive := False;
      ClearSelection;

      { First detach only the members that provenance could not preserve.
        TSyntagm.Disassociate shrinks an N:M group without disturbing its
        surviving members. }
      for Index := 0 to FSyntagms.Count - 1 do
        if not OldReused[Index] then
          FSyntagms[Index].Disassociate;
      ClearOrphanedSiblingGroups(FSyntagms);
      if Assigned(PairedVerse) then
        ClearOrphanedSiblingGroups(PairedVerse.Syntagms);

      for Index := 0 to CandidateSyntagms.Count - 1 do
      begin
        NewSyntagm := CandidateSyntagms[Index];
        if CandidateReused[Index] then
        begin
          OldSyntagm := TSyntagm(ResultSyntagms[Index]);
          OldSyntagm.AdoptParsedState(NewSyntagm);
          NewSyntagm.Free;
        end;
      end;
      for Index := 0 to FSyntagms.Count - 1 do
        if not OldReused[Index] then
          FSyntagms[Index].Free;

      CandidateSyntagms.Clear;
      FreeAndNil(CandidateSyntagms);
      FSyntagms.Clear;
      FreeAndNil(FSyntagms);
      FSyntagms := ResultSyntagms;
      ResultSyntagms := nil;
      ClearStrongCount;

      FXML := ANewXML;
      FXMLModified := True;
      Modified := True;
      if Assigned(PairedVerse) then
        PairedVerse.Modified := True;
      SyntagmClipboard := nil;
      Result := True;
    finally
      FActive := OldActive;
      Dec(FVisualUpdatesSuspended);
    end;
    RenderCommittedState;
  finally
    FreeParsedSyntagms(CandidateSyntagms);
    ResultSyntagms.Free;
    OldDocument.Free;
    NewDocument.Free;
    Codec := nil;
  end;
end;

procedure TVerse.OnCopyAssociationsJSONToClipboard(Sender: TObject);
begin
  clipboard.AsText := GetAssociationsJSON;
end;

end.


