unit theword_wysiwyg_editor_control;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Controls, Graphics, SynEdit,
  directwrite_verse_editor, verse_view_intf,
  verse_visual_document, theword_ont_codec_v2, theword_wysiwyg_session_v2,
  theword_break_v2, text_direction, text_edit_provenance, theword_translator_note_editor,
  translator_note_hover_controller;

type
  TTheWordWysiwygEditorControl = class;
  TTheWordEditorChangedEvent = procedure(Sender:
    TTheWordWysiwygEditorControl) of object;
  TTheWordEditorCommandEvent = procedure(Sender:
    TTheWordWysiwygEditorControl; ACommand: TVerseEditorCommand) of object;

  { Composite surface. Only the visible child is authoritative: leaving raw
    mode decodes through the same codec session before DirectWrite is shown. }
  TTheWordWysiwygEditorControl = class(TCustomControl)
  private
    FAutomaticNoteStart: Integer;
    FBaseDirection: TTextDirection;
    FCommandStateCanApplySemantic: Boolean;
    FCommandStateCanApplyTitle: Boolean;
    FCommandStateCanInsertNote: Boolean;
    FCommandStateCanInsertTitle: Boolean;
    FCommandStateEditable: Boolean;
    FCommandStateHasTitle: Boolean;
    FCommandStateRawMode: Boolean;
    FCommandStateRevision: QWord;
    FCommandStateSelectionLength: Integer;
    FCommandStateSelectionStart: Integer;
    FCommandStateTitle: TTheWordTitleRange;
    FCommandStateValid: Boolean;
    FCommandStateVisualGeneration: QWord;
    FDirectMutationPending: Boolean;
    FDirectEditor: TDirectWriteVerseEditor;
    FDirectZoomSize: Integer;
    FEditable: Boolean;
    FEditingNoteId: QWord;
    FEditingNoteOffset: Integer;
    FLoadedRaw: UnicodeString;
    FNoteHover: TTranslatorNoteHoverController;
    FOnChanged: TTheWordEditorChangedEvent;
    FOnSelectionChanged: TTheWordEditorChangedEvent;
    FOnCommand: TTheWordEditorCommandEvent;
    FPendingFontSizeActive: Boolean;
    FPendingFontSizeLevel: Integer;
    FPendingFontSizeAnchor: Integer;
    FPendingFontSizeTitleOrdinal: Integer;
    FPendingFontColorActive: Boolean;
    FPendingFontColorHasColor: Boolean;
    FPendingFontColor: LongInt;
    FPendingFontColorAnchor: Integer;
    FPendingFontColorTitleOrdinal: Integer;
    FRawEditor: TSynEdit;
    FRawMode: Boolean;
    FSession: TTheWordWysiwygSessionV2;
    FTokenizerVersion: Integer;
    FUpdating: Boolean;
    FVisualGeneration: QWord;
    FVisualDocument: IVerseVisualDocument;
    function GetCanInsertTranslatorNote: Boolean;
    function ApplyTranslatorNoteFromDialog(Sender: TObject;
      AAction: TTranslatorNoteEditorAction; const AQ,
      ABodyRaw: UnicodeString; out AErrorMessage: string): Boolean;
    function GetCapability: TTheWordLayerCapability;
    function GetChangesAreSemanticOnly: Boolean;
    function GetModified: Boolean;
    function GetRawEditorText: UnicodeString;
    function GetSelectionLength: Integer;
    function GetSelectionStart: Integer;
    function GetText: string;
    procedure ClearPendingFormatting;
    procedure EnsureCommandState;
    procedure DirectClipboardCopy(Sender: TObject; StartUTF16,
      LengthUTF16: Integer; ACut: Boolean; var Handled: Boolean);
    procedure DirectClipboardPaste(Sender: TObject; StartUTF16,
      LengthUTF16: Integer; var Handled: Boolean);
    procedure DirectCommand(Sender: TObject; Command: TVerseEditorCommand);
    procedure DirectVisualObjectClick(Sender: TObject; const Hit: TVerseHit);
    procedure DirectVisualObjectHover(Sender: TObject; const Hit: TVerseHit);
    procedure DirectParagraphEdit(Sender: TObject;
      Kind: TVerseParagraphEditKind; StartUTF16, LengthUTF16: Integer;
      var Accepted: Boolean; var NewCaretUTF16: Integer);
    procedure DirectBreakEdit(Sender: TObject;
      Kind: TVerseParagraphEditKind; InsertKind: TTheWordBreakKind;
      StartUTF16, LengthUTF16: Integer;
      const Breaks: TTheWordBreakIdentityArray;
      var Accepted: Boolean; var NewCaretUTF16: Integer);
    procedure DirectReplaceText(Sender: TObject; StartUTF16,
      LengthUTF16: Integer; const Replacement: UnicodeString;
      var Accepted: Boolean; var NewCaretUTF16: Integer);
    function DirectResolveInsertionStyle(Sender: TObject;
      LayoutOffsetUTF16: Integer; out Style: TVisualTextStyle): Boolean;
    procedure DirectSelectionChanged(Sender: TObject; StartUTF16,
      LengthUTF16: Integer);
    procedure DirectMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure DoContentChanged;
    procedure DoSelectionChanged;
    function GetAnchorScreenRect(ARole: TVisualSpanRole;
      AObjectId: QWord; out ARect: TRect): Boolean;
    procedure HideNotePreview;
    function ResolveNoteHover(Sender: TObject; ARevision,
      ANoteId: QWord; out ABodyRaw: UnicodeString;
      out AAnchorScreenRect: TRect): Boolean;
    procedure OpenTranslatorNote(ANoteId: QWord);
    procedure RawChanged(Sender: TObject);
    procedure RawKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RawMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure RefreshDirectDocument;
    procedure SetAutomaticNoteStart(AValue: Integer);
    procedure SetBaseDirection(AValue: TTextDirection);
    procedure SetRawMode(AValue: Boolean);
    procedure SetEditable(AValue: Boolean);
    procedure SetText(const AValue: string);
    function SyncRawEditor: Boolean;
    function TryGetDocumentSelection(out AStartUTF16,
      ALengthUTF16: Integer): Boolean;
    function TryGetTitleLayoutSelection(ALayoutStart, ALayoutLength: Integer;
      out ATitleId: QWord; out ARelativeStartUTF16,
      ADocumentLength: Integer; out ATitle: TTheWordTitleRange): Boolean;
    function LayoutSelectionTouchesTitle(ALayoutStart,
      ALayoutLength: Integer): Boolean;
    function TrySetDocumentSelection(AStartUTF16,
      ALengthUTF16: Integer): Boolean;
    function TrySetTitleDocumentSelection(AOpenOrdinal,
      ARelativeStartUTF16, ALengthUTF16: Integer): Boolean;
  protected
    procedure Resize; override;
    procedure VisibleChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ApplySemantic(AKind: TTheWordSemanticKind): Boolean;
    function AdjustRelativeFontSize(ADelta: Integer): Boolean;
    function ApplyFontColor(AHasColor: Boolean; AColor: LongInt): Boolean;
    function CanApplyTitle: Boolean;
    function ApplyTitleLevel(ALevel: Integer): Boolean;
    function CanInsertTitle: Boolean;
    function InsertTitle(ALevel: Integer; const AText: UnicodeString): Boolean;
    function ChangeCurrentTitleLevel(ALevel: Integer): Boolean;
    function RemoveCurrentTitle: Boolean;
    function TryGetCurrentTitle(out ATitle: TTheWordTitleRange): Boolean;
    function CanApplySemantic: Boolean;
    function CommitRaw: Boolean;
    function CloneEditTrace: TVerseEditTrace;
    function ProtectedLexicalObjectsUnchanged: Boolean;
    function InsertTranslatorNote: Boolean;
    function EditTranslatorNote(ANoteId: QWord): Boolean;
    function Undo: Boolean;
    function Redo: Boolean;
    function RejectLastChange: Boolean;
    procedure Cancel;
    procedure AcceptChanges;
    procedure FocusEditor;
    procedure DismissTranslatorNotePreview;
    function TranslatorNotePreviewActive: Boolean;
    procedure LoadVerseContext(const ARaw: string;
      ADirection: TTextDirection; AFont: TFont;
      ATokenizerVersion, AAutomaticNoteStart: Integer);
    procedure SetEditorFont(AFont: TFont);
    property AutomaticNoteStart: Integer read FAutomaticNoteStart
      write SetAutomaticNoteStart;
    property CanInsertTranslatorNote: Boolean read GetCanInsertTranslatorNote;
    property BaseDirection: TTextDirection read FBaseDirection
      write SetBaseDirection;
    property Capability: TTheWordLayerCapability read GetCapability;
    property ChangesAreSemanticOnly: Boolean
      read GetChangesAreSemanticOnly;
    property Editable: Boolean read FEditable write SetEditable;
    property Modified: Boolean read GetModified;
    property RawMode: Boolean read FRawMode write SetRawMode;
    property SelectionLength: Integer read GetSelectionLength;
    property SelectionStart: Integer read GetSelectionStart;
    property Text: string read GetText write SetText;
    property TokenizerVersion: Integer read FTokenizerVersion
      write FTokenizerVersion;
    property OnChanged: TTheWordEditorChangedEvent read FOnChanged
      write FOnChanged;
    property OnSelectionChanged: TTheWordEditorChangedEvent
      read FOnSelectionChanged write FOnSelectionChanged;
    property OnCommand: TTheWordEditorCommandEvent read FOnCommand
      write FOnCommand;
  end;

{$IFDEF FPCUNIT}
procedure EnableTheWordClipboardFixture(const APlainText: UnicodeString;
  const APayload: string = '');
procedure DisableTheWordClipboardFixture;
function TheWordClipboardPlainTextFixture: UnicodeString;
{$ENDIF}

implementation

uses
  LCLType, Clipbrd, Math, theword_clipboard_v2, theword_verse_visual_adapter_v2,
  theword_syntax_highlighter, performance_debug;

constructor TTheWordWysiwygEditorControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := clWindow;
  ParentColor := False;
  TabStop := True;
  FBaseDirection := tdLeftToRight;
  FEditable := True;
  FEditingNoteId := 0;
  FEditingNoteOffset := 0;
  FRawMode := False;
  FUpdating := False;
  FVisualGeneration := 0;
  FCommandStateValid := False;
  FDirectMutationPending := False;
  FTokenizerVersion := 0;
  FDirectZoomSize := 0;
  FAutomaticNoteStart := 0;
  FPendingFontSizeAnchor := -1;
  FPendingFontSizeTitleOrdinal := -1;
  FPendingFontColorAnchor := -1;
  FPendingFontColorTitleOrdinal := -1;
  FPendingFontColorHasColor := False;
  FPendingFontColor := 0;

  FDirectEditor := TDirectWriteVerseEditor.Create(Self);
  FDirectEditor.Parent := Self;
  FDirectEditor.Align := alClient;
  FDirectEditor.Color := clWindow;
  FDirectEditor.InteractionMode := vimEdit;
  FDirectEditor.OnClipboardCopy := @DirectClipboardCopy;
  FDirectEditor.OnClipboardPaste := @DirectClipboardPaste;
  FDirectEditor.OnBreakEdit := @DirectBreakEdit;
  FDirectEditor.OnReplaceText := @DirectReplaceText;
  FDirectEditor.OnResolveInsertionStyle := @DirectResolveInsertionStyle;
  FDirectEditor.OnSelectionChanged := @DirectSelectionChanged;
  FDirectEditor.OnCommand := @DirectCommand;
  FDirectEditor.OnMouseWheel := @DirectMouseWheel;
  FDirectEditor.OnVisualObjectClick := @DirectVisualObjectClick;
  FDirectEditor.OnVisualObjectHover := @DirectVisualObjectHover;

  FRawEditor := TSynEdit.Create(Self);
  FRawEditor.Parent := Self;
  FRawEditor.Align := alClient;
  FRawEditor.Visible := False;
  ConfigureTheWordSyntaxEditor(FRawEditor, False);
  FRawEditor.OnChange := @RawChanged;
  FRawEditor.OnKeyDown := @RawKeyDown;
  FRawEditor.OnMouseWheel := @RawMouseWheel;

  FNoteHover := TTranslatorNoteHoverController.Create(Self);
  FNoteHover.OnResolve := @ResolveNoteHover;
end;

destructor TTheWordWysiwygEditorControl.Destroy;
begin
  HideNotePreview;
  FSession.Free;
  inherited Destroy;
end;

procedure TTheWordWysiwygEditorControl.Resize;
begin
  inherited Resize;
  if Assigned(FDirectEditor) then
    FDirectEditor.Invalidate;
end;

procedure TTheWordWysiwygEditorControl.VisibleChanged;
begin
  if not Visible then
    DismissTranslatorNotePreview;
  inherited VisibleChanged;
end;

procedure TTheWordWysiwygEditorControl.SetText(const AValue: string);
begin
  LoadVerseContext(AValue, FBaseDirection, FDirectEditor.Font,
    FTokenizerVersion, FAutomaticNoteStart);
end;

procedure TTheWordWysiwygEditorControl.LoadVerseContext(
  const ARaw: string; ADirection: TTextDirection; AFont: TFont;
  ATokenizerVersion, AAutomaticNoteStart: Integer);
begin
  DismissTranslatorNotePreview;
  ClearPendingFormatting;
  FUpdating := True;
  try
    if AAutomaticNoteStart < 0 then
      AAutomaticNoteStart := 0;
    FBaseDirection := ADirection;
    FTokenizerVersion := ATokenizerVersion;
    FAutomaticNoteStart := AAutomaticNoteStart;
    if Assigned(AFont) and (AFont <> FDirectEditor.Font) then
    begin
      FDirectEditor.Font.Assign(AFont);
      if FDirectZoomSize > 0 then
        FDirectEditor.Font.Size := FDirectZoomSize
      else if FDirectEditor.Font.Size < 1 then
        FDirectEditor.Font.Size := 1;
    end;
    FVisualDocument := nil;
    FreeAndNil(FSession);
    FLoadedRaw := UTF8Decode(ARaw);
    FSession := TTheWordWysiwygSessionV2.Create(FLoadedRaw,
      FTokenizerVersion);
    FRawEditor.Text := ARaw;
    FRawMode := False;
    FRawEditor.Visible := False;
    FDirectEditor.Visible := True;
    RefreshDirectDocument;
    FDirectEditor.SetSelection(0, 0);
  finally
    FUpdating := False;
  end;
end;

function TTheWordWysiwygEditorControl.GetText: string;
begin
  if not Assigned(FSession) then
    Exit('');
  if FRawMode then
    SyncRawEditor;
  Result := UTF8Encode(FSession.RawText);
end;

function TTheWordWysiwygEditorControl.GetModified: Boolean;
begin
  if not Assigned(FSession) then
    Exit(False);
  if FRawMode then
    Result := GetRawEditorText <> FLoadedRaw
  else
    Result := FSession.Dirty;
end;

function TTheWordWysiwygEditorControl.GetRawEditorText: UnicodeString;
begin
  Result := UTF8Decode(FRawEditor.Text);
  { TSynEdit exposes one final platform line ending even for a single logical
    line. Verse text does not contain that editor-owned terminator. }
  if (Length(Result) >= 2) and
    (Copy(Result, Length(Result) - 1, 2) = #13#10) then
    SetLength(Result, Length(Result) - 2)
  else if (Length(Result) >= 1) and
    (Result[Length(Result)] in [#10, #13]) then
    SetLength(Result, Length(Result) - 1);
end;

function TTheWordWysiwygEditorControl.GetCapability:
  TTheWordLayerCapability;
begin
  if Assigned(FSession) then
    Result := FSession.Capability
  else
    Result := twlcRawOnly;
end;

function TTheWordWysiwygEditorControl.GetChangesAreSemanticOnly: Boolean;
begin
  Result := Assigned(FSession);
  if not Result then Exit;
  try
    if FRawMode and not SyncRawEditor then
      Exit(False);
    Result := FSession.ChangesAreSemanticOnly;
  except
    on E: ETheWordOntCodec do
      Result := False;
  end;
end;

function TTheWordWysiwygEditorControl.GetSelectionStart: Integer;
var
  MappedLength: Integer;
begin
  Result := 0;
  if FRawMode or not TryGetDocumentSelection(Result, MappedLength) then
    Result := 0;
end;

function TTheWordWysiwygEditorControl.GetSelectionLength: Integer;
var
  StartUTF16: Integer;
begin
  Result := 0;
  if FRawMode or not TryGetDocumentSelection(StartUTF16, Result) then
    Result := 0;
end;

procedure TTheWordWysiwygEditorControl.EnsureCommandState;
var
  DocumentLength, DocumentStart, RelativeStart: Integer;
  HasDocumentSelection: Boolean;
  Revision, TitleId: QWord;
  LayoutSelectionLength, LayoutSelectionStart: Integer;
  Title: TTheWordTitleRange;
begin
  Revision := 0;
  if Assigned(FSession) then
    Revision := FSession.Revision;
  LayoutSelectionStart := -1;
  LayoutSelectionLength := -1;
  if Assigned(FDirectEditor) and not FRawMode then
  begin
    LayoutSelectionStart := FDirectEditor.SelectionStart;
    LayoutSelectionLength := FDirectEditor.SelectionLength;
  end;
  if FCommandStateValid and
     (FCommandStateRevision = Revision) and
     (FCommandStateVisualGeneration = FVisualGeneration) and
     (FCommandStateSelectionStart = LayoutSelectionStart) and
     (FCommandStateSelectionLength = LayoutSelectionLength) and
     (FCommandStateRawMode = FRawMode) and
     (FCommandStateEditable = FEditable) then
    Exit;

  IBibliaPerfIncrement(ipcCommandStateBuilds);
  FCommandStateValid := True;
  FCommandStateRevision := Revision;
  FCommandStateVisualGeneration := FVisualGeneration;
  FCommandStateSelectionStart := LayoutSelectionStart;
  FCommandStateSelectionLength := LayoutSelectionLength;
  FCommandStateRawMode := FRawMode;
  FCommandStateEditable := FEditable;
  FCommandStateCanApplySemantic := False;
  FCommandStateCanApplyTitle := False;
  FCommandStateCanInsertNote := False;
  FCommandStateCanInsertTitle := False;
  FCommandStateHasTitle := False;
  FCommandStateTitle.OpenLexeme := '';
  FCommandStateTitle.CloseLexeme := '';
  FCommandStateTitle.StartOffset := 0;
  FCommandStateTitle.EndOffset := 0;
  FCommandStateTitle.Level := 0;
  FCommandStateTitle.OpenOrdinal := 0;
  FCommandStateTitle.CloseOrdinal := 0;
  FCommandStateTitle.Editable := False;
  if not FEditable or FRawMode or not Assigned(FSession) or
     not Assigned(FDirectEditor) then
    Exit;

  FCommandStateHasTitle := TryGetTitleLayoutSelection(LayoutSelectionStart,
    LayoutSelectionLength, TitleId, RelativeStart, DocumentLength, Title);
  if FCommandStateHasTitle then
    FCommandStateTitle := Title;
  HasDocumentSelection := TryGetDocumentSelection(DocumentStart,
    DocumentLength);
  if not HasDocumentSelection then
    Exit;
  FCommandStateCanApplySemantic := FSession.CanApplySemantic(DocumentStart,
    DocumentLength);
  FCommandStateCanInsertNote := (DocumentLength = 0) and
    FSession.CanInsertTranslatorNote(DocumentStart);
  FCommandStateCanInsertTitle := (DocumentLength = 0) and
    FSession.CanInsertTitle(DocumentStart);
  FCommandStateCanApplyTitle := FCommandStateHasTitle or
    ((DocumentLength > 0) and
     FSession.CanWrapTitle(DocumentStart, DocumentLength));
end;

function TTheWordWysiwygEditorControl.GetCanInsertTranslatorNote: Boolean;
begin
  EnsureCommandState;
  Result := FCommandStateCanInsertNote;
end;

procedure TTheWordWysiwygEditorControl.HideNotePreview;
begin
  if Assigned(FNoteHover) then
    FNoteHover.Dismiss;
end;

procedure TTheWordWysiwygEditorControl.DismissTranslatorNotePreview;
begin
  HideNotePreview;
end;

function TTheWordWysiwygEditorControl.TranslatorNotePreviewActive: Boolean;
begin
  Result := Assigned(FNoteHover) and FNoteHover.Active;
end;

function TTheWordWysiwygEditorControl.GetAnchorScreenRect(
  ARole: TVisualSpanRole; AObjectId: QWord; out ARect: TRect): Boolean;
var
  P1, P2: TPoint;
begin
  if ARole = vsrTranslatorNote then
    Result := FDirectEditor.GetVisualObjectClientRect(ARole,
      AObjectId, ARect)
  else
    Result := FDirectEditor.GetTextPositionClientRect(
      FDirectEditor.CaretOffset, ARect);
  if not Result then Exit;
  P1 := FDirectEditor.ClientToScreen(Point(ARect.Left, ARect.Top));
  P2 := FDirectEditor.ClientToScreen(Point(ARect.Right, ARect.Bottom));
  ARect := Rect(P1.X, P1.Y, P2.X, P2.Y);
end;

procedure TTheWordWysiwygEditorControl.DirectVisualObjectHover(
  Sender: TObject; const Hit: TVerseHit);
begin
  if (Hit.Role <> vsrTranslatorNote) or (Hit.ObjectId = 0) then
    FNoteHover.Leave
  else
    FNoteHover.Hover(Hit.Revision, Hit.ObjectId);
end;

function TTheWordWysiwygEditorControl.ResolveNoteHover(Sender: TObject;
  ARevision, ANoteId: QWord; out ABodyRaw: UnicodeString;
  out AAnchorScreenRect: TRect): Boolean;
var
  Note: TTheWordTranslatorNote;
begin
  ABodyRaw := '';
  AAnchorScreenRect := Rect(0, 0, 0, 0);
  Result := Assigned(FSession) and (ARevision = FSession.Revision) and
    FSession.TryGetTranslatorNote(ANoteId, Note) and
    GetAnchorScreenRect(vsrTranslatorNote, ANoteId,
      AAnchorScreenRect);
  if Result then
    ABodyRaw := Note.BodyRaw;
end;

function TTheWordWysiwygEditorControl.ApplyTranslatorNoteFromDialog(
  Sender: TObject; AAction: TTranslatorNoteEditorAction; const AQ,
  ABodyRaw: UnicodeString; out AErrorMessage: string): Boolean;
var
  NewNoteId: QWord;
begin
  Result := False;
  AErrorMessage := '';
  if not Assigned(FSession) then Exit;
  try
    case AAction of
      tneaSave:
        if FEditingNoteId = 0 then
        begin
          NewNoteId := 0;
          Result := FSession.InsertTranslatorNote(FEditingNoteOffset,
            AQ, ABodyRaw, NewNoteId);
          if Result then FEditingNoteId := NewNoteId;
        end
        else
          Result := FSession.UpdateTranslatorNote(FEditingNoteId,
            AQ, ABodyRaw);
      tneaDelete:
        Result := (FEditingNoteId <> 0) and
          FSession.RemoveTranslatorNote(FEditingNoteId);
    end;
  except
    on E: Exception do
    begin
      AErrorMessage := E.Message;
      Result := False;
    end;
  end;
end;

procedure TTheWordWysiwygEditorControl.DirectVisualObjectClick(
  Sender: TObject; const Hit: TVerseHit);
begin
  if (Hit.Role = vsrTranslatorNote) and (Hit.ObjectId <> 0) then
    OpenTranslatorNote(Hit.ObjectId);
end;

procedure TTheWordWysiwygEditorControl.OpenTranslatorNote(ANoteId: QWord);
begin
  EditTranslatorNote(ANoteId);
end;

function TTheWordWysiwygEditorControl.EditTranslatorNote(
  ANoteId: QWord): Boolean;
var
  EditorAction: TTranslatorNoteEditorAction;
  AnchorRect: TRect;
  InitialBody, InitialQ: UnicodeString;
  Note: TTheWordTranslatorNote;
  NoteOffset: Integer;
begin
  Result := False;
  HideNotePreview;
  if not FEditable or not Assigned(FSession) or
     not FSession.TryGetTranslatorNote(ANoteId, Note) or
     not GetAnchorScreenRect(vsrTranslatorNote, ANoteId,
       AnchorRect) then Exit;
  InitialQ := Note.Q;
  InitialBody := Note.BodyRaw;
  NoteOffset := Note.OffsetUTF16;
  FEditingNoteId := ANoteId;
  FEditingNoteOffset := NoteOffset;
  Result := True;
  try
    EditorAction := ExecuteTranslatorNoteEditor(Self, AnchorRect, True,
      Note.Capability, InitialQ, InitialBody,
      @ApplyTranslatorNoteFromDialog);
  finally
    FEditingNoteId := 0;
    FEditingNoteOffset := 0;
  end;
  if EditorAction = tneaCancel then Exit;
  RefreshDirectDocument;
  TrySetDocumentSelection(NoteOffset, 0);
  DoContentChanged;
  FocusEditor;
end;

function TTheWordWysiwygEditorControl.InsertTranslatorNote: Boolean;
var
  EditorAction: TTranslatorNoteEditorAction;
  AnchorRect: TRect;
  InitialBody, InitialQ: UnicodeString;
  DocumentLength, DocumentStart: Integer;
begin
  Result := False;
  if not GetCanInsertTranslatorNote or
     not TryGetDocumentSelection(DocumentStart, DocumentLength) then
  begin Beep; Exit; end;
  if not GetAnchorScreenRect(vsrContent, 0, AnchorRect) then
  begin
    AnchorRect.TopLeft := ClientToScreen(Point(0, 0));
    AnchorRect.BottomRight := AnchorRect.TopLeft;
  end;
  InitialQ := '';
  InitialBody := '';
  FEditingNoteId := 0;
  FEditingNoteOffset := DocumentStart;
  try
    EditorAction := ExecuteTranslatorNoteEditor(Self, AnchorRect, False,
      twlcFullWysiwyg, InitialQ, InitialBody,
      @ApplyTranslatorNoteFromDialog);
    Result := EditorAction = tneaSave;
  finally
    FEditingNoteId := 0;
    FEditingNoteOffset := 0;
  end;
  if not Result then Exit;
  RefreshDirectDocument;
  TrySetDocumentSelection(DocumentStart, 0);
  DoContentChanged;
  FocusEditor;
end;

function TTheWordWysiwygEditorControl.TryGetDocumentSelection(
  out AStartUTF16, ALengthUTF16: Integer): Boolean;
begin
  AStartUTF16 := 0;
  ALengthUTF16 := 0;
  Result := Assigned(FDirectEditor) and Assigned(FVisualDocument) and
    TryTheWordLayoutRangeToDocument(FVisualDocument,
      FDirectEditor.SelectionStart, FDirectEditor.SelectionLength,
      AStartUTF16, ALengthUTF16);
end;

function TTheWordWysiwygEditorControl.TryGetTitleLayoutSelection(
  ALayoutStart, ALayoutLength: Integer; out ATitleId: QWord;
  out ARelativeStartUTF16, ADocumentLength: Integer;
  out ATitle: TTheWordTitleRange): Boolean;
var
  CoveredLength, DocumentStart, EmptyTitleCount, Index, IntersectFinish,
    IntersectStart, LayoutFinish, SpanFinish, TitleLayoutStart: Integer;
  FoundTitle, SpanTouchesSelection: Boolean;
  Span: TVisualSpan;
  Titles: TTheWordTitleRangeArray;
begin
  Result := False;
  ATitleId := 0;
  FoundTitle := False;
  ARelativeStartUTF16 := 0;
  ADocumentLength := 0;
  CoveredLength := 0;
  FillChar(ATitle, SizeOf(ATitle), 0);
  if not Assigned(FSession) or not Assigned(FVisualDocument) or
     (ALayoutStart < 0) or (ALayoutLength < 0) or
     (ALayoutStart > Length(FVisualDocument.Text)) or
     (ALayoutLength > Length(FVisualDocument.Text) - ALayoutStart) then
    Exit;
  LayoutFinish := ALayoutStart + ALayoutLength;
  for Index := 0 to FVisualDocument.SpanCount - 1 do
  begin
    Span := FVisualDocument.Spans[Index];
    if not Span.Interactive or (Span.Role <> vsrTitleContent) or
       (Span.LengthUTF16 <= 0) then
      Continue;
    SpanFinish := Span.StartUTF16 + Span.LengthUTF16;
    if ALayoutLength = 0 then
      SpanTouchesSelection := (ALayoutStart >= Span.StartUTF16) and
        (ALayoutStart <= SpanFinish)
    else
      SpanTouchesSelection := (ALayoutStart < SpanFinish) and
        (LayoutFinish > Span.StartUTF16);
    if SpanTouchesSelection then
    begin
      if FoundTitle and (ATitleId <> Span.ObjectId) then
        Exit(False);
      ATitleId := Span.ObjectId;
      FoundTitle := True;
      if ALayoutLength > 0 then
      begin
        IntersectStart := Max(ALayoutStart, Span.StartUTF16);
        IntersectFinish := Min(LayoutFinish, SpanFinish);
        Inc(CoveredLength, IntersectFinish - IntersectStart);
      end;
    end;
  end;
  if FoundTitle and (ALayoutLength > 0) and
     (CoveredLength <> ALayoutLength) then
    Exit(False);
  if not TryTheWordLayoutRangeToDocument(FVisualDocument,
    ALayoutStart, ALayoutLength, DocumentStart, ADocumentLength) then
    Exit(False);
  if FoundTitle then
  begin
    if (ATitleId > QWord(High(Integer))) or
       not FSession.TryGetTitle(Integer(ATitleId), ATitle) or
       not ATitle.Editable then
      Exit(False);
  end
  else
  begin
    { An empty title has no content span. Its caret lives on the title side of
      the synthetic title break, so resolve it through the document offset
      without adding a visible placeholder. Refuse ambiguous anchors instead
      of choosing one of multiple zero-length titles. }
    if (ALayoutLength <> 0) or
       not TryTheWordDocumentToLayoutOffset(FVisualDocument, DocumentStart,
         twoaBefore, TitleLayoutStart) or
       (TitleLayoutStart <> ALayoutStart) then
      Exit(False);
    CollectTheWordTitleRanges(FSession.Document, Titles);
    EmptyTitleCount := 0;
    for Index := 0 to High(Titles) do
      if (Titles[Index].StartOffset = DocumentStart) and
         (Titles[Index].EndOffset = DocumentStart) then
      begin
        Inc(EmptyTitleCount);
        ATitle := Titles[Index];
      end;
    if (EmptyTitleCount <> 1) or not ATitle.Editable or
       (ATitle.OpenOrdinal < 0) then
      Exit(False);
    ATitleId := QWord(ATitle.OpenOrdinal);
  end;
  ARelativeStartUTF16 := DocumentStart - ATitle.StartOffset;
  Result := (ARelativeStartUTF16 >= 0) and
    (ARelativeStartUTF16 <= ATitle.EndOffset - ATitle.StartOffset) and
    (ADocumentLength <= ATitle.EndOffset - ATitle.StartOffset -
      ARelativeStartUTF16);
end;

function TTheWordWysiwygEditorControl.LayoutSelectionTouchesTitle(
  ALayoutStart, ALayoutLength: Integer): Boolean;
var
  Index, LayoutFinish, SpanFinish: Integer;
  Span: TVisualSpan;
begin
  Result := False;
  if not Assigned(FVisualDocument) or (ALayoutLength <= 0) or
     (ALayoutStart < 0) or
     (ALayoutStart > Length(FVisualDocument.Text)) or
     (ALayoutLength > Length(FVisualDocument.Text) - ALayoutStart) then
    Exit;
  LayoutFinish := ALayoutStart + ALayoutLength;
  for Index := 0 to FVisualDocument.SpanCount - 1 do
  begin
    Span := FVisualDocument.Spans[Index];
    if (Span.Role <> vsrTitleContent) or (Span.LengthUTF16 <= 0) then
      Continue;
    SpanFinish := Span.StartUTF16 + Span.LengthUTF16;
    if (ALayoutStart < SpanFinish) and
       (LayoutFinish > Span.StartUTF16) then
      Exit(True);
  end;
end;

function TTheWordWysiwygEditorControl.TrySetDocumentSelection(
  AStartUTF16, ALengthUTF16: Integer): Boolean;
var
  LayoutStart, LayoutFinish: Integer;
begin
  Result := Assigned(FDirectEditor) and Assigned(FVisualDocument) and
    (AStartUTF16 >= 0) and (ALengthUTF16 >= 0);
  if not Result then
    Exit;
  if ALengthUTF16 = 0 then
  begin
    Result := TryTheWordDocumentToLayoutOffset(FVisualDocument,
      AStartUTF16, twoaAfter, LayoutStart);
    LayoutFinish := LayoutStart;
  end
  else
    Result := TryTheWordDocumentToLayoutOffset(FVisualDocument,
      AStartUTF16, twoaAfter, LayoutStart) and
      TryTheWordDocumentToLayoutOffset(FVisualDocument,
        AStartUTF16 + ALengthUTF16, twoaBefore, LayoutFinish) and
      (LayoutFinish >= LayoutStart);
  if Result then
    FDirectEditor.SetSelection(LayoutStart, LayoutFinish - LayoutStart);
end;

function TTheWordWysiwygEditorControl.TrySetTitleDocumentSelection(
  AOpenOrdinal, ARelativeStartUTF16, ALengthUTF16: Integer): Boolean;
var
  LayoutFinish, LayoutStart: Integer;
  Title: TTheWordTitleRange;
begin
  Result := Assigned(FDirectEditor) and Assigned(FVisualDocument) and
    Assigned(FSession) and FSession.TryGetTitle(AOpenOrdinal, Title) and
    Title.Editable and (ARelativeStartUTF16 >= 0) and
    (ALengthUTF16 >= 0) and
    (ARelativeStartUTF16 <= Title.EndOffset - Title.StartOffset) and
    (ALengthUTF16 <= Title.EndOffset - Title.StartOffset -
      ARelativeStartUTF16);
  if not Result then
    Exit;
  if ALengthUTF16 = 0 then
  begin
    Result := TryTheWordDocumentToLayoutOffset(FVisualDocument,
      Title.StartOffset + ARelativeStartUTF16, twoaBefore, LayoutStart);
    LayoutFinish := LayoutStart;
  end
  else
    Result := TryTheWordDocumentToLayoutOffset(FVisualDocument,
      Title.StartOffset + ARelativeStartUTF16, twoaAfter, LayoutStart) and
      TryTheWordDocumentToLayoutOffset(FVisualDocument,
        Title.StartOffset + ARelativeStartUTF16 + ALengthUTF16,
        twoaBefore, LayoutFinish) and (LayoutFinish >= LayoutStart);
  if Result then
    FDirectEditor.SetSelection(LayoutStart, LayoutFinish - LayoutStart);
end;

procedure TTheWordWysiwygEditorControl.DoContentChanged;
begin
  if not FUpdating and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TTheWordWysiwygEditorControl.DoSelectionChanged;
begin
  if not FUpdating and Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TTheWordWysiwygEditorControl.RawChanged(Sender: TObject);
begin
  DoContentChanged;
end;

procedure TTheWordWysiwygEditorControl.RawKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
  begin
    if Assigned(FOnCommand) then
      FOnCommand(Self, vecSave);
    Key := 0;
  end
  else if Key = VK_ESCAPE then
  begin
    if Assigned(FOnCommand) then
      FOnCommand(Self, vecCancel);
    Key := 0;
  end
  else if (Key = VK_RETURN) and
          (Shift * [ssCtrl, ssAlt] = []) then
  begin
    if ssShift in Shift then
      FRawEditor.SelText := '<CL>'
    else
      FRawEditor.SelText := '<CM>';
    Key := 0;
  end;
end;

function TTheWordWysiwygEditorControl.SyncRawEditor: Boolean;
var
  Raw: UnicodeString;
begin
  Result := Assigned(FSession);
  if not Result then
    Exit;
  Raw := GetRawEditorText;
  if Raw <> FSession.RawText then
    FSession.ReplaceRawText(Raw);
end;

function TTheWordWysiwygEditorControl.CommitRaw: Boolean;
begin
  ClearPendingFormatting;
  Result := (not FRawMode) or SyncRawEditor;
end;

function TTheWordWysiwygEditorControl.CloneEditTrace: TVerseEditTrace;
begin
  if Assigned(FSession) then
    Result := FSession.CloneEditTrace
  else
    Result := nil;
end;

function TTheWordWysiwygEditorControl.ProtectedLexicalObjectsUnchanged:
  Boolean;
begin
  Result := Assigned(FSession) and
    FSession.ProtectedLexicalObjectsUnchanged;
end;

procedure TTheWordWysiwygEditorControl.SetRawMode(AValue: Boolean);
begin
  ClearPendingFormatting;
  if (FRawMode = AValue) or not Assigned(FSession) then
    Exit;
  DismissTranslatorNotePreview;
  FUpdating := True;
  try
    if AValue then
      FRawEditor.Text := UTF8Encode(FSession.RawText)
    else
    begin
      SyncRawEditor;
      RefreshDirectDocument;
    end;
    FRawMode := AValue;
    FRawEditor.Visible := FRawMode;
    FDirectEditor.Visible := not FRawMode;
  finally
    FUpdating := False;
  end;
  FocusEditor;
  DoSelectionChanged;
end;

procedure TTheWordWysiwygEditorControl.SetEditable(AValue: Boolean);
begin
  if not AValue then
    ClearPendingFormatting;
  if FEditable = AValue then
    Exit;
  FEditable := AValue;
  if not FEditable then
    DismissTranslatorNotePreview;
  FRawEditor.ReadOnly := not FEditable;
  DoSelectionChanged;
end;

procedure TTheWordWysiwygEditorControl.SetAutomaticNoteStart(
  AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FAutomaticNoteStart = AValue then Exit;
  FAutomaticNoteStart := AValue;
  RefreshDirectDocument;
end;

procedure TTheWordWysiwygEditorControl.SetBaseDirection(
  AValue: TTextDirection);
begin
  if FBaseDirection = AValue then
    Exit;
  FBaseDirection := AValue;
  RefreshDirectDocument;
end;

procedure TTheWordWysiwygEditorControl.RefreshDirectDocument;
var
  DocumentLength, DocumentStart, RelativeStart: Integer;
  HadSelection, HadTitleSelection: Boolean;
  Title: TTheWordTitleRange;
  TitleId: QWord;
  Visual: IVerseVisualDocument;
  WasUpdating: Boolean;
begin
  if not Assigned(FSession) then
    Exit;
  IBibliaPerfIncrement(ipcWysiwygVisualBuilds);
  HideNotePreview;
  HadTitleSelection := Assigned(FDirectEditor) and
    TryGetTitleLayoutSelection(FDirectEditor.SelectionStart,
      FDirectEditor.SelectionLength, TitleId, RelativeStart,
      DocumentLength, Title);
  HadSelection := False;
  if not HadTitleSelection then
    HadSelection := TryGetDocumentSelection(DocumentStart, DocumentLength);
  Visual := BuildTheWordVerseVisualDocument(FSession.Document,
    FBaseDirection, FSession.Revision, FAutomaticNoteStart);
  Inc(FVisualGeneration);
  WasUpdating := FUpdating;
  FUpdating := True;
  try
    FVisualDocument := Visual;
    FDirectEditor.SetEditorDocument(Visual);
    if HadTitleSelection and (TitleId <= QWord(High(Integer))) then
      TrySetTitleDocumentSelection(Integer(TitleId), RelativeStart,
        DocumentLength)
    else if HadSelection then
      TrySetDocumentSelection(DocumentStart, DocumentLength);
  finally
    FUpdating := WasUpdating;
  end;
end;

procedure TTheWordWysiwygEditorControl.RawMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  NewSize: Integer;
begin
  if not (ssCtrl in Shift) or (WheelDelta = 0) then
    Exit;

  NewSize := FRawEditor.Font.Size;
  if WheelDelta > 0 then
    Inc(NewSize)
  else if NewSize > 1 then
    Dec(NewSize);
  if NewSize <> FRawEditor.Font.Size then
    FRawEditor.Font.Size := NewSize;
  Handled := True;
end;

procedure TTheWordWysiwygEditorControl.DirectMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  NewSize: Integer;
begin
  if not (ssCtrl in Shift) or (WheelDelta = 0) then
    Exit;

  NewSize := FDirectEditor.Font.Size;
  if WheelDelta > 0 then
    Inc(NewSize)
  else if NewSize > 1 then
    Dec(NewSize);
  if NewSize <> FDirectEditor.Font.Size then
  begin
    FDirectEditor.Font.Size := NewSize;
    FDirectZoomSize := NewSize;
    FDirectEditor.Invalidate;
  end;
  Handled := True;
end;

procedure TTheWordWysiwygEditorControl.DirectReplaceText(Sender: TObject;
  StartUTF16, LengthUTF16: Integer; const Replacement: UnicodeString;
  var Accepted: Boolean; var NewCaretUTF16: Integer);
var
  CurrentLevel, DocumentLength, DocumentStart, DocumentCaret,
    UpdatedTitleOrdinal: Integer;
  RelativeStart: Integer;
  Affinity: TTheWordOffsetAffinity;
  InsertionStyle: TVisualTextStyle;
  ApplyPendingFontColor, ApplyPendingFontSize, IsTitle,
    TitleRemoved: Boolean;
  Title: TTheWordTitleRange;
  TitleId: QWord;
begin
  Accepted := False;
  DocumentStart := 0;
  DocumentLength := 0;
  UpdatedTitleOrdinal := -1;
  Affinity := twoaBefore;
  IsTitle := TryGetTitleLayoutSelection(StartUTF16, LengthUTF16,
    TitleId, RelativeStart, DocumentLength, Title);
  ApplyPendingFontSize := FPendingFontSizeActive and
    (((IsTitle) and (TitleId <= QWord(High(Integer))) and
      (FPendingFontSizeTitleOrdinal = Integer(TitleId))) or
     ((not IsTitle) and (FPendingFontSizeTitleOrdinal < 0)));
  ApplyPendingFontColor := FPendingFontColorActive and
    (((IsTitle) and (TitleId <= QWord(High(Integer))) and
      (FPendingFontColorTitleOrdinal = Integer(TitleId))) or
     ((not IsTitle) and (FPendingFontColorTitleOrdinal < 0)));
  try
    Accepted := FEditable and Assigned(FSession);
    if Accepted and IsTitle then
    begin
      DocumentStart := Title.StartOffset + RelativeStart;
      Accepted := TitleId <= QWord(High(Integer));
      if Accepted and (Replacement <> '') and
         (ApplyPendingFontSize or ApplyPendingFontColor) then
      begin
        InsertionStyle := DefaultVisualTextStyle;
        if TryResolveTheWordInsertionVisualStyle(FSession.Document,
          DocumentStart, Integer(TitleId), InsertionStyle) then
          CurrentLevel := (InsertionStyle.FontSizeDelta -
            TheWordTitleFontSizeDelta(Title.Level)) div 2
        else
          CurrentLevel := 0;
        Accepted := FSession.ReplaceTitleTextWithPendingFormatting(
          Integer(TitleId), RelativeStart, DocumentLength, Replacement,
          ApplyPendingFontSize, CurrentLevel, FPendingFontSizeLevel,
          ApplyPendingFontColor, FPendingFontColorHasColor,
          FPendingFontColor, UpdatedTitleOrdinal);
        if Accepted then
        begin
          TitleId := QWord(UpdatedTitleOrdinal);
          if ApplyPendingFontSize then
            FPendingFontSizeTitleOrdinal := UpdatedTitleOrdinal;
          if ApplyPendingFontColor then
            FPendingFontColorTitleOrdinal := UpdatedTitleOrdinal;
        end;
      end
      else if Accepted then
        Accepted := FSession.ReplaceTitleText(Integer(TitleId),
          RelativeStart, DocumentLength, Replacement);
    end
    else if Accepted and (LengthUTF16 = 0) then
    begin
      DocumentLength := 0;
      Accepted := TryTheWordLayoutToDocumentPosition(FVisualDocument,
        StartUTF16, DocumentStart, Affinity);
    end
    else if Accepted then
      Accepted := TryTheWordLayoutRangeToDocument(FVisualDocument,
        StartUTF16, LengthUTF16, DocumentStart, DocumentLength);

    if Accepted and (not IsTitle) and (DocumentLength = 0) and
       (Replacement <> '') then
    begin
      InsertionStyle := DefaultVisualTextStyle;
      if not TryResolveTheWordInsertionVisualStyle(FSession.Document,
        DocumentStart, InsertionStyle) then
        InsertionStyle := DefaultVisualTextStyle;
      CurrentLevel := InsertionStyle.FontSizeDelta div 2;
      Accepted := FSession.ReplaceTextWithPendingFormatting(DocumentStart,
        0, Replacement, Affinity = twoaAfter,
        ApplyPendingFontSize, CurrentLevel, FPendingFontSizeLevel,
        ApplyPendingFontColor, FPendingFontColorHasColor,
        FPendingFontColor);
    end
    else if Accepted and (not IsTitle) then
      Accepted := FSession.ReplaceText(DocumentStart, DocumentLength,
        Replacement);
  except
    on E: ETheWordOntProtectedEdit do Accepted := False;
    on E: ETheWordOntCodec do Accepted := False;
  end;
  if Accepted then
  begin
    TitleRemoved := IsTitle and (Replacement = '') and
      (RelativeStart = 0) and
      (DocumentLength = Title.EndOffset - Title.StartOffset);
    DocumentCaret := DocumentStart + Length(Replacement);
    RefreshDirectDocument;
    if ApplyPendingFontSize and (Replacement <> '') then
      FPendingFontSizeAnchor := DocumentCaret;
    if ApplyPendingFontColor and (Replacement <> '') then
      FPendingFontColorAnchor := DocumentCaret;
    if IsTitle and not TitleRemoved then
      Affinity := twoaBefore
    else if DocumentLength <> 0 then
      Affinity := twoaAfter;
    if not TryTheWordDocumentToLayoutOffset(FVisualDocument,
      DocumentCaret, Affinity, NewCaretUTF16) then
      NewCaretUTF16 := StartUTF16 + Length(Replacement);
    FDirectMutationPending := True;
  end
  else
    Beep;
end;

procedure TTheWordWysiwygEditorControl.DirectBreakEdit(Sender: TObject;
  Kind: TVerseParagraphEditKind; InsertKind: TTheWordBreakKind;
  StartUTF16, LengthUTF16: Integer;
  const Breaks: TTheWordBreakIdentityArray;
  var Accepted: Boolean; var NewCaretUTF16: Integer);
var
  Affinity: TTheWordOffsetAffinity;
  BreakLayoutLength, BreakLayoutStart, DocumentLength, DocumentStart: Integer;
  InsertBefore: TTheWordBreakIdentity;
  HasInsertBefore: Boolean;
begin
  Accepted := False;
  DocumentStart := 0;
  DocumentLength := 0;
  Affinity := twoaBefore;
  HasInsertBefore := False;
  try
    if not FEditable or not Assigned(FSession) then
      Exit;
    case Kind of
      vpekInsert:
        begin
          if LengthUTF16 = 0 then
            Accepted := TryTheWordLayoutToDocumentPosition(FVisualDocument,
              StartUTF16, DocumentStart, Affinity)
          else
            Accepted := TryTheWordLayoutRangeToDocument(FVisualDocument,
              StartUTF16, LengthUTF16, DocumentStart, DocumentLength);
          if Accepted and (LengthUTF16 = 0) then
            HasInsertBefore := TryGetTheWordBreakIdentityAtLayoutOffset(
              FVisualDocument, StartUTF16, InsertBefore, BreakLayoutStart,
              BreakLayoutLength) and (BreakLayoutStart = StartUTF16);
          if Accepted then
            if HasInsertBefore then
              Accepted := FSession.ReplaceSelectionWithBreak(DocumentStart,
                DocumentLength, Breaks, InsertKind, InsertBefore)
            else
              Accepted := FSession.ReplaceSelectionWithBreak(DocumentStart,
                DocumentLength, Breaks, InsertKind,
                Affinity = twoaAfter);
          Affinity := twoaAfter;
        end;
      vpekDeleteSelection:
        begin
          Accepted := (Length(Breaks) > 0) and
            TryTheWordLayoutRangeToDocument(FVisualDocument, StartUTF16,
              LengthUTF16, DocumentStart, DocumentLength) and
            FSession.DeleteSelectionWithBreaks(DocumentStart,
              DocumentLength, Breaks);
          Affinity := twoaBefore;
        end;
      vpekDeleteBackward, vpekDeleteForward:
        begin
          Accepted := (Length(Breaks) = 1) and
            FSession.RemoveBreak(Breaks[0]);
          if Accepted then
            DocumentStart := Breaks[0].OffsetUTF16;
          if Kind = vpekDeleteBackward then
            Affinity := twoaAfter
          else
            Affinity := twoaBefore;
        end;
    end;
  except
    on E: ETheWordOntProtectedEdit do Accepted := False;
    on E: ETheWordOntCodec do Accepted := False;
  end;
  if Accepted then
  begin
    RefreshDirectDocument;
    if (Kind = vpekInsert) and FPendingFontSizeActive then
      FPendingFontSizeAnchor := DocumentStart;
    if (Kind = vpekInsert) and FPendingFontColorActive then
      FPendingFontColorAnchor := DocumentStart;
    if (Kind = vpekInsert) and HasInsertBefore then
      NewCaretUTF16 := StartUTF16 +
        Length(TheWordBreakVisualText(InsertKind))
    else if Kind in [vpekDeleteSelection, vpekDeleteBackward,
      vpekDeleteForward] then
      NewCaretUTF16 := Min(StartUTF16, Length(FVisualDocument.Text))
    else if not TryTheWordDocumentToLayoutOffset(FVisualDocument,
      DocumentStart, Affinity, NewCaretUTF16) then
      NewCaretUTF16 := StartUTF16;
    FDirectMutationPending := True;
  end
  else
    Beep;
end;


procedure TTheWordWysiwygEditorControl.DirectParagraphEdit(Sender: TObject;
  Kind: TVerseParagraphEditKind; StartUTF16, LengthUTF16: Integer;
  var Accepted: Boolean; var NewCaretUTF16: Integer);
var
  Affinity: TTheWordOffsetAffinity;
  DocumentLength, DocumentStart: Integer;
  ParagraphMarkOffsets: TTheWordVisualOffsetArray;
begin
  Accepted := False;
  DocumentStart := 0;
  DocumentLength := 0;
  Affinity := twoaBefore;
  try
    if not FEditable or not Assigned(FSession) then
      Exit;
    case Kind of
      vpekInsert:
        begin
          Accepted := TryTheWordLayoutRangeToDocument(FVisualDocument,
            StartUTF16, LengthUTF16, DocumentStart, DocumentLength) and
            FSession.InsertParagraphMark(DocumentStart, DocumentLength);
          Affinity := twoaAfter;
        end;
      vpekDeleteSelection:
        begin
          Accepted := TryTheWordLayoutRangeToDocument(FVisualDocument,
            StartUTF16, LengthUTF16, DocumentStart, DocumentLength) and
            TryCollectTheWordParagraphMarkOffsets(FVisualDocument,
              StartUTF16, LengthUTF16, ParagraphMarkOffsets) and
            (Length(ParagraphMarkOffsets) > 0) and
            FSession.DeleteSelectionWithParagraphMarks(DocumentStart,
              DocumentLength, ParagraphMarkOffsets);
        end;
      vpekDeleteBackward, vpekDeleteForward:
        begin
          Accepted := TryTheWordLayoutToDocumentOffset(FVisualDocument,
            StartUTF16, DocumentStart) and
            FSession.RemoveParagraphMark(DocumentStart,
              Kind = vpekDeleteBackward);
          if Kind = vpekDeleteBackward then
            Affinity := twoaAfter
          else
            Affinity := twoaBefore;
        end;
    end;
  except
    on E: ETheWordOntProtectedEdit do Accepted := False;
    on E: ETheWordOntCodec do Accepted := False;
  end;
  if Accepted then
  begin
    RefreshDirectDocument;
    if Kind = vpekDeleteSelection then
      NewCaretUTF16 := StartUTF16
    else if not TryTheWordDocumentToLayoutOffset(FVisualDocument,
      DocumentStart, Affinity, NewCaretUTF16) then
        NewCaretUTF16 := StartUTF16;
    FDirectMutationPending := True;
  end
  else
    Beep;
end;

function PrivateClipboardFormatV2: TClipboardFormat;
begin
  Result := RegisterClipboardFormat(THEWORD_CLIPBOARD_FORMAT_V2);
end;

{$IFDEF FPCUNIT}
var
  FClipboardFixtureActive: Boolean = False;
  FClipboardFixturePlainText: UnicodeString = '';
  FClipboardFixturePayload: string = '';

procedure EnableTheWordClipboardFixture(const APlainText: UnicodeString;
  const APayload: string);
begin
  FClipboardFixtureActive := True;
  FClipboardFixturePlainText := APlainText;
  FClipboardFixturePayload := APayload;
end;

procedure DisableTheWordClipboardFixture;
begin
  FClipboardFixtureActive := False;
  FClipboardFixturePlainText := '';
  FClipboardFixturePayload := '';
end;

function TheWordClipboardPlainTextFixture: UnicodeString;
begin
  Result := FClipboardFixturePlainText;
end;
{$ENDIF}

procedure WriteTheWordClipboardV2(const APlainText: UnicodeString;
  const APayload: string);
var
  Stream: TMemoryStream;
begin
{$IFDEF FPCUNIT}
  if FClipboardFixtureActive then
  begin
    FClipboardFixturePlainText := APlainText;
    FClipboardFixturePayload := APayload;
    Exit;
  end;
{$ENDIF}
  Clipboard.AsText := UTF8Encode(APlainText);
  if APayload = '' then Exit;
  Stream := TMemoryStream.Create;
  try
    if Length(APayload) > 0 then
      Stream.WriteBuffer(APayload[1], Length(APayload));
    Stream.Position := 0;
    Clipboard.AddFormat(PrivateClipboardFormatV2, Stream);
  finally
    Stream.Free;
  end;
end;

function ReadTheWordClipboardV2: string;
var
  Stream: TMemoryStream;
  FormatId: TClipboardFormat;
begin
  Result := '';
{$IFDEF FPCUNIT}
  if FClipboardFixtureActive then
    Exit(FClipboardFixturePayload);
{$ENDIF}
  FormatId := PrivateClipboardFormatV2;
  if not Clipboard.HasFormat(FormatId) then Exit;
  Stream := TMemoryStream.Create;
  try
    if not Clipboard.GetFormat(FormatId, Stream) or
       (Stream.Size <= 0) or (Stream.Size > High(Integer)) then Exit;
    SetLength(Result, Integer(Stream.Size));
    Stream.Position := 0;
    Stream.ReadBuffer(Result[1], Length(Result));
  finally
    Stream.Free;
  end;
end;

function ReadTheWordClipboardPlainText: UnicodeString;
begin
{$IFDEF FPCUNIT}
  if FClipboardFixtureActive then
    Exit(FClipboardFixturePlainText);
{$ENDIF}
  Result := UTF8Decode(Clipboard.AsText);
end;

procedure TTheWordWysiwygEditorControl.DirectClipboardCopy(Sender: TObject;
  StartUTF16, LengthUTF16: Integer; ACut: Boolean; var Handled: Boolean);
var
  DocumentLength, DocumentStart, RelativeStart: Integer;
  Breaks: TTheWordBreakIdentityArray;
  Payload: string;
  PlainText: UnicodeString;
  IsTitle, RemovedTitle, Succeeded: Boolean;
  Title: TTheWordTitleRange;
  TitleId: QWord;
begin
  Handled := True;
  Succeeded := False;
  IsTitle := TryGetTitleLayoutSelection(StartUTF16, LengthUTF16,
    TitleId, RelativeStart, DocumentLength, Title);
  try
    if Assigned(FSession) and ((not ACut) or FEditable) then
    begin
      if IsTitle and (DocumentLength > 0) and
         (TitleId <= QWord(High(Integer))) then
      begin
        DocumentStart := Title.StartOffset + RelativeStart;
        Succeeded := FSession.CopySelection(DocumentStart,
          DocumentLength, PlainText, Payload);
        if Succeeded and ACut then
          Succeeded := FSession.ReplaceTitleText(Integer(TitleId),
            RelativeStart, DocumentLength, '');
      end
      else if LayoutSelectionTouchesTitle(StartUTF16, LengthUTF16) then
        Succeeded := False
      else if TryTheWordLayoutRangeToDocument(FVisualDocument, StartUTF16,
        LengthUTF16, DocumentStart, DocumentLength) and
        TryCollectTheWordBreakIdentities(FVisualDocument, StartUTF16,
          LengthUTF16, Breaks) then
        if ACut then
          Succeeded := FSession.CutSelection(DocumentStart,
            DocumentLength, Breaks, PlainText, Payload)
        else
          Succeeded := FSession.CopySelection(DocumentStart,
            DocumentLength, Breaks, PlainText, Payload);
    end;
    if not Succeeded then
    begin
      Beep;
      Exit;
    end;
    WriteTheWordClipboardV2(PlainText, Payload);
    if ACut then
    begin
      RemovedTitle := IsTitle and (RelativeStart = 0) and
        (DocumentLength = Title.EndOffset - Title.StartOffset);
      RefreshDirectDocument;
      if IsTitle and not RemovedTitle then
        TrySetTitleDocumentSelection(Integer(TitleId), RelativeStart, 0)
      else
        TrySetDocumentSelection(DocumentStart, 0);
      DoContentChanged;
    end;
  except
    on E: Exception do Beep;
  end;
end;

procedure TTheWordWysiwygEditorControl.DirectClipboardPaste(Sender: TObject;
  StartUTF16, LengthUTF16: Integer; var Handled: Boolean);
var
  BreakLayoutLength, BreakLayoutStart, DocumentLength, DocumentStart,
    NewLayoutCaret, RelativeStart: Integer;
  Breaks: TTheWordBreakIdentityArray;
  InsertBefore: TTheWordBreakIdentity;
  Affinity: TTheWordOffsetAffinity;
  InsertedLength: Integer;
  UsedPrivate: Boolean;
  Payload: string;
  PlainText: UnicodeString;
  HasInsertBefore, IsTitle, RemovedTitle, Succeeded: Boolean;
  Title: TTheWordTitleRange;
  TitleId: QWord;
begin
  Handled := True;
  HasInsertBefore := False;
  ClearPendingFormatting;
  if not FEditable or not Assigned(FSession) then
  begin
    Beep;
    Exit;
  end;
  try
    IsTitle := TryGetTitleLayoutSelection(StartUTF16, LengthUTF16,
      TitleId, RelativeStart, DocumentLength, Title);
    PlainText := ReadTheWordClipboardPlainText;
    if IsTitle and (TitleId <= QWord(High(Integer))) then
    begin
      DocumentStart := Title.StartOffset + RelativeStart;
      Payload := ReadTheWordClipboardV2;
      Succeeded := FSession.PasteTitle(Integer(TitleId), RelativeStart,
        DocumentLength, Payload, PlainText, InsertedLength, UsedPrivate);
    end
    else
    begin
      if not TryTheWordLayoutRangeToDocument(FVisualDocument, StartUTF16,
        LengthUTF16, DocumentStart, DocumentLength) or
         not TryTheWordLayoutToDocumentPosition(FVisualDocument,
           StartUTF16, DocumentStart, Affinity) or
         not TryCollectTheWordBreakIdentities(FVisualDocument,
           StartUTF16, LengthUTF16, Breaks) then
      begin
        Beep;
        Exit;
      end;
      HasInsertBefore := TryGetTheWordBreakIdentityAtLayoutOffset(
        FVisualDocument, StartUTF16 + LengthUTF16, InsertBefore,
        BreakLayoutStart, BreakLayoutLength) and
        (BreakLayoutStart = StartUTF16 + LengthUTF16);
      Payload := ReadTheWordClipboardV2;
      Succeeded := FSession.Paste(DocumentStart, DocumentLength, Breaks,
        Affinity = twoaAfter, HasInsertBefore, InsertBefore, Payload,
        PlainText, InsertedLength, UsedPrivate);
    end;
    if not Succeeded then
    begin
      Beep;
      Exit;
    end;
    RemovedTitle := IsTitle and (PlainText = '') and (RelativeStart = 0) and
      (DocumentLength = Title.EndOffset - Title.StartOffset);
    RefreshDirectDocument;
    if IsTitle and not RemovedTitle then
      TrySetTitleDocumentSelection(Integer(TitleId),
        RelativeStart + InsertedLength, 0)
    else if HasInsertBefore and
      TryTheWordDocumentToLayoutOffset(FVisualDocument,
        DocumentStart + InsertedLength, twoaBefore, NewLayoutCaret) then
      FDirectEditor.SetSelection(NewLayoutCaret, 0)
    else
      TrySetDocumentSelection(DocumentStart + InsertedLength, 0);
    DoContentChanged;
  except
    on E: Exception do Beep;
  end;
end;

procedure TTheWordWysiwygEditorControl.DirectSelectionChanged(
  Sender: TObject; StartUTF16, LengthUTF16: Integer);
var
  DocumentLength, DocumentOffset, RelativeStart,
    TitleOrdinal: Integer;
  Title: TTheWordTitleRange;
  TitleId: QWord;
begin
  TitleOrdinal := -1;
  if LengthUTF16 = 0 then
    if TryGetTitleLayoutSelection(StartUTF16, 0, TitleId,
      RelativeStart, DocumentLength, Title) and
      (TitleId <= QWord(High(Integer))) then
      TitleOrdinal := Integer(TitleId);
  if (FPendingFontSizeActive or FPendingFontColorActive) and
     ((LengthUTF16 <> 0) or
      (not TryTheWordLayoutToDocumentOffset(FVisualDocument, StartUTF16,
        DocumentOffset)) or
      (FPendingFontSizeActive and
        ((DocumentOffset <> FPendingFontSizeAnchor) or
         (TitleOrdinal <> FPendingFontSizeTitleOrdinal))) or
      (FPendingFontColorActive and
        ((DocumentOffset <> FPendingFontColorAnchor) or
         (TitleOrdinal <> FPendingFontColorTitleOrdinal)))) then
    ClearPendingFormatting;
  if FDirectMutationPending then
  begin
    FDirectMutationPending := False;
    DoContentChanged;
  end;
  DoSelectionChanged;
end;

function TTheWordWysiwygEditorControl.DirectResolveInsertionStyle(
  Sender: TObject; LayoutOffsetUTF16: Integer;
  out Style: TVisualTextStyle): Boolean;
var
  ActiveTitleOrdinal, DocumentLength, DocumentOffset, Index,
    RelativeStart: Integer;
  Affinity: TTheWordOffsetAffinity;
  ApplyPendingFontColor, ApplyPendingFontSize, HasActiveTitle,
    HasOldTestamentQuote, HasWordsOfJesus: Boolean;
  Mark: TTheWordSemanticMark;
  Title: TTheWordTitleRange;
  TitleId: QWord;
begin
  Style := DefaultVisualTextStyle;
  ActiveTitleOrdinal := -1;
  HasActiveTitle := TryGetTitleLayoutSelection(LayoutOffsetUTF16, 0,
    TitleId, RelativeStart, DocumentLength, Title) and
    (TitleId <= QWord(High(Integer)));
  if HasActiveTitle then
    ActiveTitleOrdinal := Integer(TitleId);
  ApplyPendingFontSize := FPendingFontSizeActive and
    (FPendingFontSizeTitleOrdinal = ActiveTitleOrdinal);
  ApplyPendingFontColor := FPendingFontColorActive and
    (FPendingFontColorTitleOrdinal = ActiveTitleOrdinal);
  Result := Assigned(FSession) and Assigned(FVisualDocument) and
    TryTheWordLayoutToDocumentPosition(FVisualDocument,
      LayoutOffsetUTF16, DocumentOffset, Affinity) and
    TryResolveTheWordInsertionVisualStyle(FSession.Document,
      DocumentOffset, ActiveTitleOrdinal, Style);
  if not Result then
    Exit;
  if ApplyPendingFontSize then
  begin
    Style.FontSizeDelta := 2 * FPendingFontSizeLevel;
    if HasActiveTitle then
      Inc(Style.FontSizeDelta,
        TheWordTitleFontSizeDelta(Title.Level));
  end;
  if ApplyPendingFontColor then
  begin
    if FPendingFontColorHasColor then
    begin
      Style.HasForegroundColor := True;
      Style.ForegroundColor := FPendingFontColor;
    end
    else if HasActiveTitle and (Title.Level in [0, 1]) then
    begin
      Style.HasForegroundColor := True;
      Style.ForegroundColor := THEWORD_TITLE_LEVEL1_COLOR;
    end
    else
      Style.HasForegroundColor := False;
  end;

  { Explicit font color is presentation. Semantic color keeps theWord's
    precedence even while the caret previews pending formatting. }
  HasWordsOfJesus := False;
  HasOldTestamentQuote := False;
  for Index := 0 to FSession.Document.MarkCount - 1 do
  begin
    Mark := FSession.Document.Marks[Index];
    if (Mark.StartOffset < DocumentOffset) and
       (Mark.EndOffset > DocumentOffset) then
      case Mark.Kind of
        twsmWordsOfJesus: HasWordsOfJesus := True;
        twsmOldTestamentQuote: HasOldTestamentQuote := True;
      end;
  end;
  if HasWordsOfJesus then
  begin
    Style.HasForegroundColor := True;
    Style.ForegroundColor := LongInt(clRed);
  end
  else if HasOldTestamentQuote then
  begin
    Style.HasForegroundColor := True;
    Style.ForegroundColor := LongInt(clBlack);
  end;
end;

procedure TTheWordWysiwygEditorControl.DirectCommand(Sender: TObject;
  Command: TVerseEditorCommand);
begin
  case Command of
    vecUndo: Undo;
    vecRedo: Redo;
    vecItalic: ApplySemantic(twsmItalic);
    vecHtmlBold: ApplySemantic(twsmHtmlBold);
    vecHtmlItalic: ApplySemantic(twsmHtmlItalic);
    vecHtmlUnderline: ApplySemantic(twsmHtmlUnderline);
    vecFontSizeDecrease: AdjustRelativeFontSize(-1);
    vecFontSizeIncrease: AdjustRelativeFontSize(1);
    vecClearPendingFormatting: ClearPendingFormatting;
    vecWordsOfJesus: ApplySemantic(twsmWordsOfJesus);
    vecOldTestamentQuote: ApplySemantic(twsmOldTestamentQuote);
    vecStrikethrough: ApplySemantic(twsmStrikethrough);
    vecSuperscript: ApplySemantic(twsmSuperscript);
    vecSubscript: ApplySemantic(twsmSubscript);
    vecInsertTranslatorNote: InsertTranslatorNote;
    vecSave:
      begin
        { A failed save must not leave a latent insertion style behind. }
        ClearPendingFormatting;
        if Assigned(FOnCommand) then
          FOnCommand(Self, Command);
      end;
    vecCancel:
      if Assigned(FOnCommand) then
        FOnCommand(Self, Command);
  end;
end;

function TTheWordWysiwygEditorControl.TryGetCurrentTitle(
  out ATitle: TTheWordTitleRange): Boolean;
begin
  EnsureCommandState;
  ATitle := FCommandStateTitle;
  Result := FCommandStateHasTitle;
end;

function TTheWordWysiwygEditorControl.CanInsertTitle: Boolean;
begin
  EnsureCommandState;
  Result := FCommandStateCanInsertTitle;
end;

function TTheWordWysiwygEditorControl.CanApplyTitle: Boolean;
begin
  EnsureCommandState;
  Result := FCommandStateCanApplyTitle;
end;

function TTheWordWysiwygEditorControl.ApplyTitleLevel(
  ALevel: Integer): Boolean;
var
  DocumentLength, DocumentStart, OpenOrdinal: Integer;
  Title: TTheWordTitleRange;
begin
  Result := False;
  if (ALevel < 1) or (ALevel > 3) or not CanApplyTitle then
    Exit;
  if TryGetCurrentTitle(Title) then
    Exit(ChangeCurrentTitleLevel(ALevel));
  if not TryGetDocumentSelection(DocumentStart, DocumentLength) or
     (DocumentLength <= 0) then
    Exit;
  ClearPendingFormatting;
  Result := FSession.WrapTitle(DocumentStart, DocumentLength, ALevel,
    OpenOrdinal);
  if not Result then
    Exit;
  RefreshDirectDocument;
  TrySetTitleDocumentSelection(OpenOrdinal, 0, DocumentLength);
  DoContentChanged;
  FocusEditor;
end;

function TTheWordWysiwygEditorControl.InsertTitle(ALevel: Integer;
  const AText: UnicodeString): Boolean;
var
  DocumentLength, DocumentStart, Index: Integer;
  InsertedTitle: TTheWordTitleRange;
  Titles: TTheWordTitleRangeArray;
begin
  Result := False;
  if (ALevel < 1) or (ALevel > 3) or (AText = '') or
     not FEditable or FRawMode or not Assigned(FSession) or
     not TryGetDocumentSelection(DocumentStart, DocumentLength) or
     (DocumentLength <> 0) or not FSession.CanInsertTitle(DocumentStart) then
    Exit;
  ClearPendingFormatting;
  Result := FSession.InsertTitle(DocumentStart, ALevel, AText);
  if not Result then
    Exit;
  InsertedTitle.OpenOrdinal := -1;
  CollectTheWordTitleRanges(FSession.Document, Titles);
  for Index := High(Titles) downto 0 do
    if Titles[Index].Editable and
       (Titles[Index].StartOffset = DocumentStart) and
       (Titles[Index].EndOffset = DocumentStart + Length(AText)) then
    begin
      InsertedTitle := Titles[Index];
      Break;
    end;
  RefreshDirectDocument;
  if InsertedTitle.OpenOrdinal >= 0 then
    TrySetTitleDocumentSelection(InsertedTitle.OpenOrdinal, 0,
      Length(AText))
  else
    TrySetDocumentSelection(DocumentStart, Length(AText));
  DoContentChanged;
  FocusEditor;
end;

function TTheWordWysiwygEditorControl.ChangeCurrentTitleLevel(
  ALevel: Integer): Boolean;
var
  DocumentLength, RelativeStart: Integer;
  Title: TTheWordTitleRange;
  TitleId: QWord;
begin
  Result := False;
  if (ALevel < 1) or (ALevel > 3) or not FEditable or FRawMode or
     not Assigned(FDirectEditor) or
     not TryGetTitleLayoutSelection(FDirectEditor.SelectionStart,
       FDirectEditor.SelectionLength, TitleId, RelativeStart,
       DocumentLength, Title) or (TitleId > QWord(High(Integer))) then
    Exit;
  ClearPendingFormatting;
  Result := FSession.ChangeTitleLevel(Integer(TitleId), ALevel);
  if not Result then
    Exit;
  RefreshDirectDocument;
  TrySetTitleDocumentSelection(Integer(TitleId), RelativeStart,
    DocumentLength);
  DoContentChanged;
  FocusEditor;
end;

function TTheWordWysiwygEditorControl.RemoveCurrentTitle: Boolean;
var
  DocumentLength, RelativeStart: Integer;
  Title: TTheWordTitleRange;
  TitleId: QWord;
begin
  Result := False;
  if not FEditable or FRawMode or not Assigned(FDirectEditor) or
     not TryGetTitleLayoutSelection(FDirectEditor.SelectionStart,
       FDirectEditor.SelectionLength, TitleId, RelativeStart,
       DocumentLength, Title) or (TitleId > QWord(High(Integer))) then
    Exit;
  ClearPendingFormatting;
  Result := FSession.UnwrapTitle(Integer(TitleId));
  if not Result then
    Exit;
  RefreshDirectDocument;
  TrySetDocumentSelection(Title.StartOffset + RelativeStart,
    DocumentLength);
  DoContentChanged;
  FocusEditor;
end;

function TTheWordWysiwygEditorControl.CanApplySemantic: Boolean;
begin
  EnsureCommandState;
  Result := FCommandStateCanApplySemantic;
end;

function TTheWordWysiwygEditorControl.ApplySemantic(
  AKind: TTheWordSemanticKind): Boolean;
begin
  Result := CanApplySemantic and FSession.ApplySemantic(AKind,
    SelectionStart, SelectionLength);
  if Result then
  begin
    RefreshDirectDocument;
    DoContentChanged;
    FocusEditor;
  end;
end;
function TTheWordWysiwygEditorControl.ApplyFontColor(
  AHasColor: Boolean; AColor: LongInt): Boolean;
var
  DocumentLength, DocumentStart: Integer;
  HasTitleSelection: Boolean;
  Title: TTheWordTitleRange;
begin
  Result := False;
  if not FEditable or FRawMode or not Assigned(FSession) or
     not TryGetDocumentSelection(DocumentStart, DocumentLength) then
    Exit;
  HasTitleSelection := TryGetCurrentTitle(Title);
  if AHasColor then
    AColor := LongInt(ColorToRGB(TColor(AColor))) and $00FFFFFF
  else
    AColor := 0;

  if DocumentLength > 0 then
  begin
    ClearPendingFormatting;
    Result := FSession.ApplyFontColor(DocumentStart, DocumentLength,
      AHasColor, AColor);
    if Result then
    begin
      RefreshDirectDocument;
      if HasTitleSelection then
        TrySetTitleDocumentSelection(Title.OpenOrdinal,
          DocumentStart - Title.StartOffset, DocumentLength)
      else
        TrySetDocumentSelection(DocumentStart, DocumentLength);
      DoContentChanged;
      FocusEditor;
    end;
    Exit;
  end;

  FPendingFontColorActive := True;
  FPendingFontColorHasColor := AHasColor;
  FPendingFontColor := AColor;
  FPendingFontColorAnchor := DocumentStart;
  if HasTitleSelection then
    FPendingFontColorTitleOrdinal := Title.OpenOrdinal
  else
    FPendingFontColorTitleOrdinal := -1;
  FDirectEditor.Invalidate;
  FocusEditor;
  Result := True;
end;



procedure TTheWordWysiwygEditorControl.ClearPendingFormatting;
var
  HadPending: Boolean;
begin
  HadPending := FPendingFontSizeActive or FPendingFontColorActive;
  FPendingFontSizeActive := False;
  FPendingFontSizeAnchor := -1;
  FPendingFontSizeTitleOrdinal := -1;
  FPendingFontColorActive := False;
  FPendingFontColorHasColor := False;
  FPendingFontColor := 0;
  FPendingFontColorAnchor := -1;
  FPendingFontColorTitleOrdinal := -1;
  if HadPending and Assigned(FDirectEditor) then
    FDirectEditor.Invalidate;
end;

function TTheWordWysiwygEditorControl.AdjustRelativeFontSize(
  ADelta: Integer): Boolean;
var
  DocumentLength, DocumentStart, Level: Integer;
  HasTitleSelection: Boolean;
  Style: TVisualTextStyle;
  Title: TTheWordTitleRange;
begin
  Result := False;
  if not FEditable or FRawMode or not Assigned(FSession) or
     ((ADelta <> -1) and (ADelta <> 1)) or
     not TryGetDocumentSelection(DocumentStart, DocumentLength) then Exit;
  HasTitleSelection := TryGetCurrentTitle(Title);
  if DocumentLength > 0 then
  begin
    ClearPendingFormatting;
    Result := FSession.AdjustRelativeFontSize(DocumentStart,
      DocumentLength, ADelta);
    if Result then
    begin
      RefreshDirectDocument;
      if HasTitleSelection then
        TrySetTitleDocumentSelection(Title.OpenOrdinal,
          DocumentStart - Title.StartOffset, DocumentLength)
      else
        TrySetDocumentSelection(DocumentStart, DocumentLength);
      DoContentChanged;
      FocusEditor;
    end;
    Exit;
  end;
  Style := DefaultVisualTextStyle;
  if HasTitleSelection then
  begin
    if not TryResolveTheWordInsertionVisualStyle(FSession.Document,
      DocumentStart, Title.OpenOrdinal, Style) then
      Exit;
    Dec(Style.FontSizeDelta,
      TheWordTitleFontSizeDelta(Title.Level));
  end
  else if not TryResolveTheWordInsertionVisualStyle(FSession.Document,
    DocumentStart, Style) then
    Exit;
  if FPendingFontSizeActive then Level := FPendingFontSizeLevel
  else Level := Style.FontSizeDelta div 2;
  Level := EnsureRange(Level + ADelta, -4, 10);
  FPendingFontSizeLevel := Level;
  FPendingFontSizeAnchor := DocumentStart;
  if HasTitleSelection then
    FPendingFontSizeTitleOrdinal := Title.OpenOrdinal
  else
    FPendingFontSizeTitleOrdinal := -1;
  FPendingFontSizeActive := True;
  FDirectEditor.Invalidate;
  FocusEditor;
  Result := True;
end;

function TTheWordWysiwygEditorControl.Undo: Boolean;
begin
  ClearPendingFormatting;
  if FRawMode then
  begin
    FRawEditor.Undo;
    Exit(True);
  end;
  Result := Assigned(FSession) and FSession.Undo;
  if Result then
  begin
    RefreshDirectDocument;
    DoContentChanged;
  end;
end;

function TTheWordWysiwygEditorControl.Redo: Boolean;
begin
  ClearPendingFormatting;
  if FRawMode then
  begin
    FRawEditor.Redo;
    Exit(True);
  end;
  Result := Assigned(FSession) and FSession.Redo;
  if Result then
  begin
    RefreshDirectDocument;
    DoContentChanged;
  end;
end;

function TTheWordWysiwygEditorControl.RejectLastChange: Boolean;
begin
  Result := (not FRawMode) and Assigned(FSession) and
    FSession.RejectLastChange;
  if Result then
  begin
    RefreshDirectDocument;
    DoContentChanged;
  end;
end;

procedure TTheWordWysiwygEditorControl.Cancel;
begin
  ClearPendingFormatting;
  DismissTranslatorNotePreview;
  if not Assigned(FSession) then
    Exit;
  FSession.Cancel;
  FRawEditor.Text := UTF8Encode(FSession.RawText);
  FRawMode := False;
  FRawEditor.Visible := False;
  FDirectEditor.Visible := True;
  RefreshDirectDocument;
  DoContentChanged;
end;

procedure TTheWordWysiwygEditorControl.FocusEditor;
begin
  if FRawMode then
  begin
    if FRawEditor.CanSetFocus then
      FRawEditor.SetFocus;
  end
  else
  begin
    FDirectEditor.InteractionMode := vimEdit;
    FDirectEditor.FocusEditor;
  end;
end;

procedure TTheWordWysiwygEditorControl.SetEditorFont(AFont: TFont);
begin
  if not Assigned(AFont) then
    Exit;
  FDirectEditor.Font.Assign(AFont);
  if FDirectZoomSize > 0 then
    FDirectEditor.Font.Size := FDirectZoomSize
  else if FDirectEditor.Font.Size < 1 then
    FDirectEditor.Font.Size := 1;
  FDirectEditor.Invalidate;
end;

procedure TTheWordWysiwygEditorControl.AcceptChanges;
begin
  ClearPendingFormatting;
  if not Assigned(FSession) then
    Exit;
  FSession.AcceptChanges;
  FLoadedRaw := FSession.RawText;
  FRawEditor.Modified := False;
end;

end.
