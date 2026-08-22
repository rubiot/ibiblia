unit theword_wysiwyg_editor_control;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Controls, Graphics, SynEdit,
  directwrite_verse_editor, verse_view_intf,
  verse_visual_document, theword_ont_codec_v2, theword_wysiwyg_session_v2,
  text_direction, text_edit_provenance, theword_translator_note_editor,
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
    FDirectEditor: TDirectWriteVerseEditor;
    FDirectZoomSize: Integer;
    FEditable: Boolean;
    FEditingNoteId: QWord;
    FEditingNoteOffset: Integer;
    FLoadedRaw: UnicodeString;
    FNoteHover: TTranslatorNoteHoverController;
    FOnChanged: TTheWordEditorChangedEvent;
    FOnCommand: TTheWordEditorCommandEvent;
    FPendingFontSizeActive: Boolean;
    FPendingFontSizeLevel: Integer;
    FPendingFontSizeAnchor: Integer;
    FRawEditor: TSynEdit;
    FRawMode: Boolean;
    FSession: TTheWordWysiwygSessionV2;
    FTokenizerVersion: Integer;
    FUpdating: Boolean;
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
    procedure ClearPendingFontSize;
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
    procedure DirectReplaceText(Sender: TObject; StartUTF16,
      LengthUTF16: Integer; const Replacement: UnicodeString;
      var Accepted: Boolean; var NewCaretUTF16: Integer);
    function DirectResolveInsertionStyle(Sender: TObject;
      LayoutOffsetUTF16: Integer; out Style: TVisualTextStyle): Boolean;
    procedure DirectSelectionChanged(Sender: TObject; StartUTF16,
      LengthUTF16: Integer);
    procedure DirectMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure DoChanged;
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
    function TrySetDocumentSelection(AStartUTF16,
      ALengthUTF16: Integer): Boolean;
  protected
    procedure Resize; override;
    procedure VisibleChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ApplySemantic(AKind: TTheWordSemanticKind): Boolean;
    function AdjustRelativeFontSize(ADelta: Integer): Boolean;
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
    property OnCommand: TTheWordEditorCommandEvent read FOnCommand
      write FOnCommand;
  end;

implementation

uses
  LCLType, Clipbrd, Math, theword_clipboard_v2, theword_verse_visual_adapter_v2,
  theword_syntax_highlighter;

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
  FTokenizerVersion := 0;
  FDirectZoomSize := 0;
  FAutomaticNoteStart := 0;

  FDirectEditor := TDirectWriteVerseEditor.Create(Self);
  FDirectEditor.Parent := Self;
  FDirectEditor.Align := alClient;
  FDirectEditor.Color := clWindow;
  FDirectEditor.InteractionMode := vimEdit;
  FDirectEditor.OnClipboardCopy := @DirectClipboardCopy;
  FDirectEditor.OnClipboardPaste := @DirectClipboardPaste;
  FDirectEditor.OnParagraphEdit := @DirectParagraphEdit;
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
  DismissTranslatorNotePreview;
  ClearPendingFontSize;
  FUpdating := True;
  try
    FVisualDocument := nil;
    FreeAndNil(FSession);
    FLoadedRaw := UTF8Decode(AValue);
    FSession := TTheWordWysiwygSessionV2.Create(FLoadedRaw,
      FTokenizerVersion);
    FRawEditor.Text := AValue;
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

function TTheWordWysiwygEditorControl.GetCanInsertTranslatorNote: Boolean;
var
  DocumentLength, DocumentStart: Integer;
begin
  Result := FEditable and (not FRawMode) and Assigned(FSession) and
    TryGetDocumentSelection(DocumentStart, DocumentLength) and
    (DocumentLength = 0) and
    FSession.CanInsertTranslatorNote(DocumentStart);
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
  DoChanged;
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
  DoChanged;
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

procedure TTheWordWysiwygEditorControl.DoChanged;
begin
  if not FUpdating and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TTheWordWysiwygEditorControl.RawChanged(Sender: TObject);
begin
  DoChanged;
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
  else if Key = VK_RETURN then
    Key := 0;
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
  ClearPendingFontSize;
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
  DoChanged;
end;

procedure TTheWordWysiwygEditorControl.SetEditable(AValue: Boolean);
begin
  if FEditable = AValue then
    Exit;
  FEditable := AValue;
  if not FEditable then
    DismissTranslatorNotePreview;
  FRawEditor.ReadOnly := not FEditable;
  DoChanged;
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
  DocumentLength, DocumentStart: Integer;
  HadSelection: Boolean;
  Visual: IVerseVisualDocument;
begin
  if not Assigned(FSession) then
    Exit;
  HideNotePreview;
  HadSelection := TryGetDocumentSelection(DocumentStart, DocumentLength);
  Visual := BuildTheWordVerseVisualDocument(FSession.Document,
    FBaseDirection, FSession.Revision, FAutomaticNoteStart);
  FVisualDocument := Visual;
  FDirectEditor.SetEditorDocument(Visual);
  if HadSelection then
    TrySetDocumentSelection(DocumentStart, DocumentLength);
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
  CurrentLevel, DocumentLength, DocumentStart, DocumentCaret: Integer;
  InsertionStyle: TVisualTextStyle;
begin
  Accepted := False;
  try
    Accepted := FEditable and Assigned(FSession) and
      TryTheWordLayoutRangeToDocument(FVisualDocument, StartUTF16,
        LengthUTF16, DocumentStart, DocumentLength);
    if Accepted and FPendingFontSizeActive and (DocumentLength = 0) and
       (Replacement <> '') then
    begin
      InsertionStyle := DefaultVisualTextStyle;
      if not TryResolveTheWordInsertionVisualStyle(FSession.Document,
        DocumentStart, InsertionStyle) then
        InsertionStyle := DefaultVisualTextStyle;
      CurrentLevel := InsertionStyle.FontSizeDelta div 2;
      Accepted := FSession.ReplaceTextWithRelativeFontSize(DocumentStart,
        DocumentLength, Replacement, CurrentLevel, FPendingFontSizeLevel);
    end
    else if Accepted then
      Accepted := FSession.ReplaceText(DocumentStart, DocumentLength,
        Replacement);
  except
    on E: ETheWordOntProtectedEdit do Accepted := False;
    on E: ETheWordOntCodec do Accepted := False;
  end;
  if Accepted then
  begin
    DocumentCaret := DocumentStart + Length(Replacement);
    if FPendingFontSizeActive then
      FPendingFontSizeAnchor := DocumentCaret;
    RefreshDirectDocument;
    if not TryTheWordDocumentToLayoutOffset(FVisualDocument,
      DocumentCaret, twoaAfter, NewCaretUTF16) then
      NewCaretUTF16 := StartUTF16 + Length(Replacement);
    DoChanged;
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
    DoChanged;
  end
  else
    Beep;
end;

function PrivateClipboardFormatV2: TClipboardFormat;
begin
  Result := RegisterClipboardFormat(THEWORD_CLIPBOARD_FORMAT_V2);
end;

procedure WriteTheWordClipboardV2(const APlainText: UnicodeString;
  const APayload: string);
var
  Stream: TMemoryStream;
begin
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

procedure TTheWordWysiwygEditorControl.DirectClipboardCopy(Sender: TObject;
  StartUTF16, LengthUTF16: Integer; ACut: Boolean; var Handled: Boolean);
var
  DocumentLength, DocumentStart: Integer;
  Payload: string;
  PlainText: UnicodeString;
  Succeeded: Boolean;
begin
  Handled := True;
  Succeeded := False;
  try
    if Assigned(FSession) and ((not ACut) or FEditable) and
      TryTheWordLayoutRangeToDocument(FVisualDocument, StartUTF16,
        LengthUTF16, DocumentStart, DocumentLength) then
      if ACut then
        Succeeded := FSession.CutSelection(DocumentStart, DocumentLength,
          PlainText, Payload)
      else
        Succeeded := FSession.CopySelection(DocumentStart, DocumentLength,
          PlainText, Payload);
    if not Succeeded then
    begin
      Beep;
      Exit;
    end;
    WriteTheWordClipboardV2(PlainText, Payload);
    if ACut then
    begin
      RefreshDirectDocument;
      TrySetDocumentSelection(DocumentStart, 0);
      DoChanged;
    end;
  except
    on E: Exception do Beep;
  end;
end;

procedure TTheWordWysiwygEditorControl.DirectClipboardPaste(Sender: TObject;
  StartUTF16, LengthUTF16: Integer; var Handled: Boolean);
var
  DocumentLength, DocumentStart: Integer;
  InsertedLength: Integer;
  UsedPrivate: Boolean;
  Payload: string;
  PlainText: UnicodeString;
begin
  Handled := True;
  ClearPendingFontSize;
  if not FEditable or not Assigned(FSession) then
  begin
    Beep;
    Exit;
  end;
  try
    if not TryTheWordLayoutRangeToDocument(FVisualDocument, StartUTF16,
      LengthUTF16, DocumentStart, DocumentLength) then
    begin
      Beep;
      Exit;
    end;
    Payload := ReadTheWordClipboardV2;
    PlainText := UTF8Decode(Clipboard.AsText);
    if not FSession.Paste(DocumentStart, DocumentLength, Payload, PlainText,
      InsertedLength, UsedPrivate) then
    begin
      Beep;
      Exit;
    end;
    RefreshDirectDocument;
    TrySetDocumentSelection(DocumentStart + InsertedLength, 0);
    DoChanged;
  except
    on E: Exception do Beep;
  end;
end;

procedure TTheWordWysiwygEditorControl.DirectSelectionChanged(
  Sender: TObject; StartUTF16, LengthUTF16: Integer);
var
  DocumentOffset: Integer;
begin
  if FPendingFontSizeActive and
     ((LengthUTF16 <> 0) or
      (not TryTheWordLayoutToDocumentOffset(FVisualDocument, StartUTF16,
        DocumentOffset)) or (DocumentOffset <> FPendingFontSizeAnchor)) then
    ClearPendingFontSize;
  DoChanged;
end;

function TTheWordWysiwygEditorControl.DirectResolveInsertionStyle(
  Sender: TObject; LayoutOffsetUTF16: Integer;
  out Style: TVisualTextStyle): Boolean;
var
  DocumentOffset: Integer;
begin
  Style := DefaultVisualTextStyle;
  Result := Assigned(FSession) and Assigned(FVisualDocument) and
    TryTheWordLayoutToDocumentOffset(FVisualDocument, LayoutOffsetUTF16,
      DocumentOffset) and
    TryResolveTheWordInsertionVisualStyle(FSession.Document,
      DocumentOffset, Style);
  if Result and FPendingFontSizeActive then
    Style.FontSizeDelta := 2 * FPendingFontSizeLevel;
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
    vecClearPendingFormatting: ClearPendingFontSize;
    vecWordsOfJesus: ApplySemantic(twsmWordsOfJesus);
    vecOldTestamentQuote: ApplySemantic(twsmOldTestamentQuote);
    vecStrikethrough: ApplySemantic(twsmStrikethrough);
    vecSuperscript: ApplySemantic(twsmSuperscript);
    vecSubscript: ApplySemantic(twsmSubscript);
    vecInsertTranslatorNote: InsertTranslatorNote;
    vecSave, vecCancel:
      if Assigned(FOnCommand) then
        FOnCommand(Self, Command);
  end;
end;

function TTheWordWysiwygEditorControl.CanApplySemantic: Boolean;
begin
  Result := FEditable and (not FRawMode) and Assigned(FSession) and
    FSession.CanApplySemantic(SelectionStart, SelectionLength);
end;

function TTheWordWysiwygEditorControl.ApplySemantic(
  AKind: TTheWordSemanticKind): Boolean;
begin
  Result := CanApplySemantic and FSession.ApplySemantic(AKind,
    SelectionStart, SelectionLength);
  if Result then
  begin
    RefreshDirectDocument;
    DoChanged;
    FocusEditor;
  end;
end;

procedure TTheWordWysiwygEditorControl.ClearPendingFontSize;
begin
  FPendingFontSizeActive := False;
  FPendingFontSizeAnchor := -1;
end;

function TTheWordWysiwygEditorControl.AdjustRelativeFontSize(
  ADelta: Integer): Boolean;
var
  DocumentLength, DocumentStart, Level: Integer;
  Style: TVisualTextStyle;
begin
  Result := False;
  if not FEditable or FRawMode or not Assigned(FSession) or
     ((ADelta <> -1) and (ADelta <> 1)) or
     not TryGetDocumentSelection(DocumentStart, DocumentLength) then Exit;
  if DocumentLength > 0 then
  begin
    ClearPendingFontSize;
    Result := FSession.AdjustRelativeFontSize(DocumentStart,
      DocumentLength, ADelta);
    if Result then
    begin
      RefreshDirectDocument;
      TrySetDocumentSelection(DocumentStart, DocumentLength);
      DoChanged;
      FocusEditor;
    end;
    Exit;
  end;
  Style := DefaultVisualTextStyle;
  if not TryResolveTheWordInsertionVisualStyle(FSession.Document,
    DocumentStart, Style) then Exit;
  if FPendingFontSizeActive then Level := FPendingFontSizeLevel
  else Level := Style.FontSizeDelta div 2;
  Level := EnsureRange(Level + ADelta, -4, 10);
  FPendingFontSizeLevel := Level;
  FPendingFontSizeAnchor := DocumentStart;
  FPendingFontSizeActive := True;
  FDirectEditor.Invalidate;
  FocusEditor;
  Result := True;
end;

function TTheWordWysiwygEditorControl.Undo: Boolean;
begin
  ClearPendingFontSize;
  if FRawMode then
  begin
    FRawEditor.Undo;
    Exit(True);
  end;
  Result := Assigned(FSession) and FSession.Undo;
  if Result then
  begin
    RefreshDirectDocument;
    DoChanged;
  end;
end;

function TTheWordWysiwygEditorControl.Redo: Boolean;
begin
  ClearPendingFontSize;
  if FRawMode then
  begin
    FRawEditor.Redo;
    Exit(True);
  end;
  Result := Assigned(FSession) and FSession.Redo;
  if Result then
  begin
    RefreshDirectDocument;
    DoChanged;
  end;
end;

function TTheWordWysiwygEditorControl.RejectLastChange: Boolean;
begin
  Result := (not FRawMode) and Assigned(FSession) and
    FSession.RejectLastChange;
  if Result then
  begin
    RefreshDirectDocument;
    DoChanged;
  end;
end;

procedure TTheWordWysiwygEditorControl.Cancel;
begin
  ClearPendingFontSize;
  DismissTranslatorNotePreview;
  if not Assigned(FSession) then
    Exit;
  FSession.Cancel;
  FRawEditor.Text := UTF8Encode(FSession.RawText);
  FRawMode := False;
  FRawEditor.Visible := False;
  FDirectEditor.Visible := True;
  RefreshDirectDocument;
  DoChanged;
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
  ClearPendingFontSize;
  if not Assigned(FSession) then
    Exit;
  FSession.AcceptChanges;
  FLoadedRaw := FSession.RawText;
  FRawEditor.Modified := False;
end;

end.
