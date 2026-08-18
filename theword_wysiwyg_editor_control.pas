unit theword_wysiwyg_editor_control;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Controls, Graphics, SynEdit, directwrite_verse_editor,
  verse_visual_document, theword_ont_codec_v2, theword_wysiwyg_session_v2,
  text_direction;

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
    FBaseDirection: TTextDirection;
    FDirectEditor: TDirectWriteVerseEditor;
    FEditable: Boolean;
    FLoadedRaw: UnicodeString;
    FOnChanged: TTheWordEditorChangedEvent;
    FOnCommand: TTheWordEditorCommandEvent;
    FRawEditor: TSynEdit;
    FRawMode: Boolean;
    FSession: TTheWordWysiwygSessionV2;
    FUpdating: Boolean;
    FVisualDocument: IVerseVisualDocument;
    function GetCapability: TTheWordLayerCapability;
    function GetChangesAreSemanticOnly: Boolean;
    function GetModified: Boolean;
    function GetSelectionLength: Integer;
    function GetSelectionStart: Integer;
    function GetText: string;
    procedure DirectClipboardCopy(Sender: TObject; StartUTF16,
      LengthUTF16: Integer; ACut: Boolean; var Handled: Boolean);
    procedure DirectClipboardPaste(Sender: TObject; StartUTF16,
      LengthUTF16: Integer; var Handled: Boolean);
    procedure DirectCommand(Sender: TObject; Command: TVerseEditorCommand);
    procedure DirectReplaceText(Sender: TObject; StartUTF16,
      LengthUTF16: Integer; const Replacement: UnicodeString;
      var Accepted: Boolean; var NewCaretUTF16: Integer);
    procedure DirectSelectionChanged(Sender: TObject; StartUTF16,
      LengthUTF16: Integer);
    procedure DoChanged;
    procedure RawChanged(Sender: TObject);
    procedure RawKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RefreshDirectDocument;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ApplySemantic(AKind: TTheWordSemanticKind): Boolean;
    function CanApplySemantic: Boolean;
    function CommitRaw: Boolean;
    function Undo: Boolean;
    function Redo: Boolean;
    procedure Cancel;
    procedure FocusEditor;
    procedure SetEditorFont(AFont: TFont);
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
    property OnChanged: TTheWordEditorChangedEvent read FOnChanged
      write FOnChanged;
    property OnCommand: TTheWordEditorCommandEvent read FOnCommand
      write FOnCommand;
  end;

implementation

uses
  LCLType, Clipbrd, theword_clipboard_v2, theword_verse_visual_adapter_v2,
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
  FRawMode := False;
  FUpdating := False;

  FDirectEditor := TDirectWriteVerseEditor.Create(Self);
  FDirectEditor.Parent := Self;
  FDirectEditor.Align := alClient;
  FDirectEditor.Color := clWindow;
  FDirectEditor.InteractionMode := vimEdit;
  FDirectEditor.OnClipboardCopy := @DirectClipboardCopy;
  FDirectEditor.OnClipboardPaste := @DirectClipboardPaste;
  FDirectEditor.OnReplaceText := @DirectReplaceText;
  FDirectEditor.OnSelectionChanged := @DirectSelectionChanged;
  FDirectEditor.OnCommand := @DirectCommand;

  FRawEditor := TSynEdit.Create(Self);
  FRawEditor.Parent := Self;
  FRawEditor.Align := alClient;
  FRawEditor.Visible := False;
  ConfigureTheWordSyntaxEditor(FRawEditor, False);
  FRawEditor.OnChange := @RawChanged;
  FRawEditor.OnKeyDown := @RawKeyDown;
end;

destructor TTheWordWysiwygEditorControl.Destroy;
begin
  FSession.Free;
  inherited Destroy;
end;

procedure TTheWordWysiwygEditorControl.Resize;
begin
  inherited Resize;
  if Assigned(FDirectEditor) then
    FDirectEditor.Invalidate;
end;

procedure TTheWordWysiwygEditorControl.SetText(const AValue: string);
begin
  FUpdating := True;
  try
    FVisualDocument := nil;
    FreeAndNil(FSession);
    FLoadedRaw := UTF8Decode(AValue);
    FSession := TTheWordWysiwygSessionV2.Create(FLoadedRaw);
    FRawEditor.Text := AValue;
    FRawMode := False;
    FRawEditor.Visible := False;
    FDirectEditor.Visible := True;
    RefreshDirectDocument;
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
    Result := UTF8Decode(FRawEditor.Text) <> FLoadedRaw
  else
    Result := FSession.Dirty;
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
  Raw := UTF8Decode(FRawEditor.Text);
  if Raw <> FSession.RawText then
    FSession.ReplaceRawText(Raw);
end;

function TTheWordWysiwygEditorControl.CommitRaw: Boolean;
begin
  Result := (not FRawMode) or SyncRawEditor;
end;

procedure TTheWordWysiwygEditorControl.SetRawMode(AValue: Boolean);
begin
  if (FRawMode = AValue) or not Assigned(FSession) then
    Exit;
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
  FRawEditor.ReadOnly := not FEditable;
  DoChanged;
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
  HadSelection := TryGetDocumentSelection(DocumentStart, DocumentLength);
  Visual := BuildTheWordVerseVisualDocument(FSession.Document,
    FBaseDirection, FSession.Revision);
  FVisualDocument := Visual;
  FDirectEditor.SetEditorDocument(Visual);
  if HadSelection then
    TrySetDocumentSelection(DocumentStart, DocumentLength);
end;

procedure TTheWordWysiwygEditorControl.DirectReplaceText(Sender: TObject;
  StartUTF16, LengthUTF16: Integer; const Replacement: UnicodeString;
  var Accepted: Boolean; var NewCaretUTF16: Integer);
var
  DocumentLength, DocumentStart, DocumentCaret: Integer;
begin
  Accepted := False;
  try
    Accepted := FEditable and Assigned(FSession) and
      TryTheWordLayoutRangeToDocument(FVisualDocument, StartUTF16,
        LengthUTF16, DocumentStart, DocumentLength) and
      FSession.ReplaceText(DocumentStart, DocumentLength, Replacement);
  except
    on E: ETheWordOntProtectedEdit do Accepted := False;
    on E: ETheWordOntCodec do Accepted := False;
  end;
  if Accepted then
  begin
    DocumentCaret := DocumentStart + Length(Replacement);
    RefreshDirectDocument;
    if not TryTheWordDocumentToLayoutOffset(FVisualDocument,
      DocumentCaret, twoaAfter, NewCaretUTF16) then
      NewCaretUTF16 := StartUTF16 + Length(Replacement);
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
begin
  DoChanged;
end;

procedure TTheWordWysiwygEditorControl.DirectCommand(Sender: TObject;
  Command: TVerseEditorCommand);
begin
  case Command of
    vecUndo: Undo;
    vecRedo: Redo;
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

function TTheWordWysiwygEditorControl.Undo: Boolean;
begin
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

procedure TTheWordWysiwygEditorControl.Cancel;
begin
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
    FDirectEditor.InteractionMode := vimAssociation;
    FDirectEditor.InteractionMode := vimEdit;
    if FDirectEditor.CanSetFocus then
      FDirectEditor.SetFocus;
  end;
end;

procedure TTheWordWysiwygEditorControl.SetEditorFont(AFont: TFont);
begin
  if not Assigned(AFont) then
    Exit;
  FRawEditor.Font.Assign(AFont);
  FDirectEditor.Font.Assign(AFont);
  FDirectEditor.Invalidate;
end;

end.
