unit directwrite_verse_editor;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Character, Types, Controls, StdCtrls, Graphics, LCLType, LCLProc,
  Clipbrd, verse_visual_document, verse_view_intf,
  directwrite_syntagm_view;

type
  TVerseInteractionMode = (vimAssociation, vimEdit);
  TVerseEditorCommand = (
    vecUndo,
    vecRedo,
    vecSave,
    vecCancel,
    vecItalic,
    vecHtmlBold,
    vecHtmlItalic,
    vecHtmlUnderline,
    vecFontSizeDecrease,
    vecFontSizeIncrease,
    vecClearPendingFormatting,
    vecWordsOfJesus,
    vecOldTestamentQuote,
    vecStrikethrough,
    vecSuperscript,
    vecSubscript,
    vecInsertTranslatorNote
  );

  TVerseTextReplaceEvent = procedure(Sender: TObject; StartUTF16,
    LengthUTF16: Integer; const Replacement: UnicodeString;
    var Accepted: Boolean; var NewCaretUTF16: Integer) of object;
  TVerseEditorCommandEvent = procedure(Sender: TObject;
    Command: TVerseEditorCommand) of object;
  TVerseSelectionChangedEvent = procedure(Sender: TObject;
    StartUTF16, LengthUTF16: Integer) of object;
  TVerseClipboardCopyEvent = procedure(Sender: TObject;
    StartUTF16, LengthUTF16: Integer; ACut: Boolean;
    var Handled: Boolean) of object;
  TVerseClipboardPasteEvent = procedure(Sender: TObject;
    StartUTF16, LengthUTF16: Integer; var Handled: Boolean) of object;
  TVerseParagraphEditKind = (
    vpekInsert,
    vpekDeleteSelection,
    vpekDeleteBackward,
    vpekDeleteForward
  );
  TVerseParagraphEditEvent = procedure(Sender: TObject;
    Kind: TVerseParagraphEditKind; StartUTF16, LengthUTF16: Integer;
    var Accepted: Boolean; var NewCaretUTF16: Integer) of object;

  { DirectWrite remains responsible for shaping and visual navigation. A tiny
    native edit control owns keyboard/IME focus at the painted caret; committed
    UTF-8 input is forwarded as document commands and is never a second text
    buffer. }
  TDirectWriteVerseEditor = class(TDirectWriteSyntagmView)
  private
    FActiveOffset: Integer;
    FAnchorOffset: Integer;
    FCaretStops: TVerseTextOffsetArray;
    FDragging: Boolean;
    FInputBroker: TEdit;
    FBrokerChanging: Boolean;
    FMode: TVerseInteractionMode;
    FOnCommand: TVerseEditorCommandEvent;
    FOnClipboardCopy: TVerseClipboardCopyEvent;
    FOnClipboardPaste: TVerseClipboardPasteEvent;
    FOnParagraphEdit: TVerseParagraphEditEvent;
    FOnReplaceText: TVerseTextReplaceEvent;
    FOnSelectionChanged: TVerseSelectionChangedEvent;
    FOnVisualObjectClick: TVerseHitEvent;
    FOnVisualObjectHover: TVerseHitEvent;
    FLastVisualObjectId: QWord;
    FLastVisualObjectRole: TVisualSpanRole;
    procedure BrokerChange(Sender: TObject);
    procedure BrokerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BrokerUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure CopySelection;
    procedure CutSelection;
    procedure DeleteBackward;
    procedure DeleteForward;
    procedure DoSelectionChanged;
    procedure ClearVisualObjectHover;
    procedure UpdateVisualObjectHover(X, Y: Integer);
    procedure FocusInputBroker;
    function GetCaretOffset: Integer;
    function GetSelectionLength: Integer;
    function GetSelectionStart: Integer;
    function GetTextLength: Integer;
    function IsInteractiveCaretStop(Value: Integer): Boolean;
    function IsInteractiveHitOffset(Value: Integer;
      IsTrailing: Boolean): Boolean;
    function IsParagraphMarkAt(Value: Integer): Boolean;
    function SelectionContainsParagraphMark: Boolean;
    function IsTraversableNonInteractiveSpan(
      const Span: TVisualSpan): Boolean;
    function IsWordCodeUnit(const AText: UnicodeString;
      AIndex: Integer): Boolean;
    procedure MoveCaretHorizontal(Direction: Integer; ExtendSelection,
      ByWord: Boolean);
    procedure MoveCaretVertical(Direction: Integer; ExtendSelection: Boolean);
    function NearestCaretStop(Value: Integer): Integer;
    function NearestCaretStopInRegion(Value, RegionStart,
      RegionFinish: Integer): Integer;
    function NextCaretStop(Value: Integer): Integer;
    procedure PasteText;
    function PreviousCaretStop(Value: Integer): Integer;
    procedure RefreshCaretStops;
    procedure ReplaceSelection(const Replacement: UnicodeString);
    procedure RequestReplace(StartUTF16, LengthUTF16: Integer;
      const Replacement: UnicodeString);
    procedure RequestParagraphEdit(Kind: TVerseParagraphEditKind;
      StartUTF16, LengthUTF16: Integer);
    procedure SelectWordAt(UTF16Offset: Integer);
    procedure SetCaret(Value: Integer; ExtendSelection: Boolean);
    procedure SetInteractionMode(Value: TVerseInteractionMode);
    function TryGetCaretRegion(Value: Integer; out RegionStart,
      RegionFinish: Integer): Boolean;
    procedure UpdateInputBrokerPosition;
  protected
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure FocusEditor;
    procedure SetEditorDocument(const ADocument: IVerseVisualDocument);
    procedure SetSelection(StartUTF16, LengthUTF16: Integer);
    procedure SelectAll;
    property CaretOffset: Integer read GetCaretOffset;
    property InteractionMode: TVerseInteractionMode read FMode
      write SetInteractionMode;
    property SelectionLength: Integer read GetSelectionLength;
    property SelectionStart: Integer read GetSelectionStart;
    property OnClipboardCopy: TVerseClipboardCopyEvent
      read FOnClipboardCopy write FOnClipboardCopy;
    property OnClipboardPaste: TVerseClipboardPasteEvent
      read FOnClipboardPaste write FOnClipboardPaste;
    property OnCommand: TVerseEditorCommandEvent read FOnCommand
      write FOnCommand;
    property OnParagraphEdit: TVerseParagraphEditEvent
      read FOnParagraphEdit write FOnParagraphEdit;
    property OnReplaceText: TVerseTextReplaceEvent read FOnReplaceText
      write FOnReplaceText;
    property OnSelectionChanged: TVerseSelectionChangedEvent
      read FOnSelectionChanged write FOnSelectionChanged;
    property OnVisualObjectClick: TVerseHitEvent
      read FOnVisualObjectClick write FOnVisualObjectClick;
    property OnVisualObjectHover: TVerseHitEvent
      read FOnVisualObjectHover write FOnVisualObjectHover;
  published
    property OnMouseWheel;
  end;

implementation

uses
  Math;

function NormalizeInsertedText(const Value: UnicodeString): UnicodeString;
var
  Index: Integer;
begin
  Result := '';
  Index := 1;
  while Index <= Length(Value) do
  begin
    if Value[Index] = #13 then
    begin
      Result := Result + ' ';
      if (Index < Length(Value)) and (Value[Index + 1] = #10) then
        Inc(Index);
    end
    else if Value[Index] = #10 then
      Result := Result + ' '
    else
      Result := Result + Value[Index];
    Inc(Index);
  end;
end;

constructor TDirectWriteVerseEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  FMode := vimAssociation;
  FAnchorOffset := 0;
  FActiveOffset := 0;
  FDragging := False;
  FBrokerChanging := False;
  FLastVisualObjectId := 0;
  FLastVisualObjectRole := vsrContent;

  FInputBroker := TEdit.Create(Self);
  FInputBroker.Parent := Self;
  FInputBroker.AutoSize := False;
  FInputBroker.BorderStyle := bsNone;
  FInputBroker.TabStop := False;
  FInputBroker.Text := '';
  FInputBroker.Visible := False;
  FInputBroker.OnChange := @BrokerChange;
  FInputBroker.OnKeyDown := @BrokerKeyDown;
  FInputBroker.OnUTF8KeyPress := @BrokerUTF8KeyPress;
end;

function TDirectWriteVerseEditor.GetTextLength: Integer;
begin
  if Assigned(Document) then
    Result := Length(Document.Text)
  else
    Result := 0;
end;

function TDirectWriteVerseEditor.GetSelectionStart: Integer;
begin
  Result := Min(FAnchorOffset, FActiveOffset);
end;

function TDirectWriteVerseEditor.GetSelectionLength: Integer;
begin
  Result := Abs(FActiveOffset - FAnchorOffset);
end;

function TDirectWriteVerseEditor.GetCaretOffset: Integer;
begin
  Result := FActiveOffset;
end;

procedure TDirectWriteVerseEditor.RefreshCaretStops;
var
  CandidateStops: TVerseTextOffsetArray;
  DocText: UnicodeString;
  Index: Integer;
  StopCount: Integer;
begin
  DocText := '';
  if Assigned(Document) then
    DocText := Document.Text;

  if not GetCaretStops(CandidateStops) then
  begin
    SetLength(CandidateStops, Length(DocText) + 1);
    StopCount := 1;
    CandidateStops[0] := 0;
    Index := 1;
    while Index <= Length(DocText) do
    begin
      if (Ord(DocText[Index]) >= $D800) and
         (Ord(DocText[Index]) <= $DBFF) and
         (Index < Length(DocText)) and
         (Ord(DocText[Index + 1]) >= $DC00) and
         (Ord(DocText[Index + 1]) <= $DFFF) then
        Inc(Index, 2)
      else
        Inc(Index);
      CandidateStops[StopCount] := Index - 1;
      Inc(StopCount);
    end;
    SetLength(CandidateStops, StopCount);
  end;

  SetLength(FCaretStops, Length(CandidateStops));
  StopCount := 0;
  for Index := 0 to High(CandidateStops) do
    if IsInteractiveCaretStop(CandidateStops[Index]) and
       ((StopCount = 0) or
        (FCaretStops[StopCount - 1] <> CandidateStops[Index])) then
    begin
      FCaretStops[StopCount] := CandidateStops[Index];
      Inc(StopCount);
    end;
  SetLength(FCaretStops, StopCount);

  { An empty document still needs its sole insertion position. }
  if (Length(FCaretStops) = 0) and (Length(DocText) = 0) then
  begin
    SetLength(FCaretStops, 1);
    FCaretStops[0] := 0;
  end;
end;

procedure TDirectWriteVerseEditor.FocusEditor;
begin
  if FMode = vimEdit then
    FocusInputBroker
  else if CanSetFocus then
    SetFocus;
end;

function TDirectWriteVerseEditor.IsInteractiveCaretStop(
  Value: Integer): Boolean;
var
  Index: Integer;
  Span: TVisualSpan;
begin
  Result := False;
  if not Assigned(Document) then
    Exit;
  for Index := 0 to Document.SpanCount - 1 do
  begin
    Span := Document.Spans[Index];
    if Span.Interactive and (Span.Role = vsrContent) and
       (Span.LengthUTF16 > 0) and
       (Value >= Span.StartUTF16) and
       (Value <= Span.StartUTF16 + Span.LengthUTF16) then
      Exit(True);
  end;
end;

function TDirectWriteVerseEditor.IsInteractiveHitOffset(Value: Integer;
  IsTrailing: Boolean): Boolean;
var
  Span: TVisualSpan;
begin
  Result := False;
  if not Assigned(Document) then
    Exit;
  if IsTrailing and (Value > 0) then
    Dec(Value);
  Result := Document.TryGetInteractiveSpanAt(Value, Span) and
    (Span.Role = vsrContent);
end;

function TDirectWriteVerseEditor.IsParagraphMarkAt(Value: Integer): Boolean;
var
  Index: Integer;
  Span: TVisualSpan;
begin
  Result := False;
  if not Assigned(Document) or (Value < 0) or
     (Value >= Length(Document.Text)) then
    Exit;
  for Index := 0 to Document.SpanCount - 1 do
  begin
    Span := Document.Spans[Index];
    if (Span.Role = vsrParagraphMark) and
       (Value >= Span.StartUTF16) and
       (Value < Span.StartUTF16 + Span.LengthUTF16) then
      Exit(True);
  end;
end;

function TDirectWriteVerseEditor.SelectionContainsParagraphMark: Boolean;
var
  Offset: Integer;
begin
  Result := False;
  for Offset := SelectionStart to SelectionStart + SelectionLength - 1 do
    if IsParagraphMarkAt(Offset) then
      Exit(True);
end;

function TDirectWriteVerseEditor.IsTraversableNonInteractiveSpan(
  const Span: TVisualSpan): Boolean;
begin
  Result := (Span.LengthUTF16 = 0) or
    (Span.Kind = vskDirectionControl) or
    (Span.Role in [vsrProtectedAnchor, vsrParagraphMark,
      vsrTitleBreak, vsrTranslatorNote]);
end;

function TDirectWriteVerseEditor.TryGetCaretRegion(Value: Integer;
  out RegionStart, RegionFinish: Integer): Boolean;
var
  Index: Integer;
  Span: TVisualSpan;
begin
  Result := False;
  RegionStart := -1;
  RegionFinish := -1;
  if not Assigned(Document) then
    Exit;

  for Index := 0 to Document.SpanCount - 1 do
  begin
    Span := Document.Spans[Index];
    if Span.Interactive and (Span.Role = vsrContent) and
       (Span.LengthUTF16 > 0) then
    begin
      if RegionStart < 0 then
        RegionStart := Span.StartUTF16;
      RegionFinish := Max(RegionFinish,
        Span.StartUTF16 + Span.LengthUTF16);
    end
    else if not IsTraversableNonInteractiveSpan(Span) then
    begin
      if (RegionStart >= 0) and (Value >= RegionStart) and
         (Value <= RegionFinish) then
        Exit(True);
      RegionStart := -1;
      RegionFinish := -1;
    end;
  end;

  Result := (RegionStart >= 0) and (Value >= RegionStart) and
    (Value <= RegionFinish);
end;

function TDirectWriteVerseEditor.NearestCaretStop(Value: Integer): Integer;
var
  BestDistance: Integer;
  Distance: Integer;
  Index: Integer;
begin
  if Value < 0 then
    Value := 0
  else if Value > GetTextLength then
    Value := GetTextLength;
  if Length(FCaretStops) = 0 then
    RefreshCaretStops;
  if Length(FCaretStops) = 0 then
    Exit(0);
  Result := Value;
  BestDistance := MaxInt;
  for Index := 0 to High(FCaretStops) do
  begin
    Distance := Abs(FCaretStops[Index] - Value);
    if Distance < BestDistance then
    begin
      Result := FCaretStops[Index];
      BestDistance := Distance;
    end;
  end;
end;

function TDirectWriteVerseEditor.NearestCaretStopInRegion(Value,
  RegionStart, RegionFinish: Integer): Integer;
var
  BestDistance: Integer;
  Distance: Integer;
  Index: Integer;
begin
  if Length(FCaretStops) = 0 then
    RefreshCaretStops;
  if Length(FCaretStops) = 0 then
    Exit(0);
  if Value < RegionStart then
    Value := RegionStart
  else if Value > RegionFinish then
    Value := RegionFinish;
  Result := RegionStart;
  BestDistance := MaxInt;
  for Index := 0 to High(FCaretStops) do
    if (FCaretStops[Index] >= RegionStart) and
       (FCaretStops[Index] <= RegionFinish) then
    begin
      Distance := Abs(FCaretStops[Index] - Value);
      if Distance < BestDistance then
      begin
        Result := FCaretStops[Index];
        BestDistance := Distance;
      end;
    end;
end;

function TDirectWriteVerseEditor.PreviousCaretStop(Value: Integer): Integer;
var
  Index: Integer;
  RegionFinish: Integer;
  RegionStart: Integer;
begin
  if Length(FCaretStops) = 0 then
    RefreshCaretStops;
  if Length(FCaretStops) = 0 then
    Exit(0);
  if not TryGetCaretRegion(Value, RegionStart, RegionFinish) then
    Exit(NearestCaretStop(Value));
  Result := RegionStart;
  for Index := High(FCaretStops) downto 0 do
    if (FCaretStops[Index] >= RegionStart) and
       (FCaretStops[Index] < Value) then
      Exit(FCaretStops[Index]);
end;

function TDirectWriteVerseEditor.NextCaretStop(Value: Integer): Integer;
var
  Index: Integer;
  RegionFinish: Integer;
  RegionStart: Integer;
begin
  if Length(FCaretStops) = 0 then
    RefreshCaretStops;
  if Length(FCaretStops) = 0 then
    Exit(0);
  if not TryGetCaretRegion(Value, RegionStart, RegionFinish) then
    Exit(NearestCaretStop(Value));
  Result := RegionFinish;
  for Index := 0 to High(FCaretStops) do
    if (FCaretStops[Index] <= RegionFinish) and
       (FCaretStops[Index] > Value) then
      Exit(FCaretStops[Index]);
end;

procedure TDirectWriteVerseEditor.DoSelectionChanged;
begin
  Invalidate;
  UpdateInputBrokerPosition;
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self, SelectionStart, SelectionLength);
end;

procedure TDirectWriteVerseEditor.SetSelection(StartUTF16,
  LengthUTF16: Integer);
var
  EndOffset: Integer;
  RegionFinish: Integer;
  RegionStart: Integer;
begin
  RefreshCaretStops;
  EndOffset := StartUTF16 + LengthUTF16;
  FAnchorOffset := NearestCaretStop(StartUTF16);
  if TryGetCaretRegion(FAnchorOffset, RegionStart, RegionFinish) then
    FActiveOffset := NearestCaretStopInRegion(EndOffset, RegionStart,
      RegionFinish)
  else
    FActiveOffset := FAnchorOffset;
  DoSelectionChanged;
end;

procedure TDirectWriteVerseEditor.SelectAll;
var
  RegionFinish: Integer;
  RegionStart: Integer;
begin
  RefreshCaretStops;
  if Length(FCaretStops) = 0 then
  begin
    FAnchorOffset := 0;
    FActiveOffset := 0;
  end
  else
  begin
    if not TryGetCaretRegion(FActiveOffset, RegionStart, RegionFinish) then
      TryGetCaretRegion(FCaretStops[0], RegionStart, RegionFinish);
    FAnchorOffset := RegionStart;
    FActiveOffset := RegionFinish;
  end;
  DoSelectionChanged;
end;

procedure TDirectWriteVerseEditor.SetCaret(Value: Integer;
  ExtendSelection: Boolean);
var
  RegionFinish: Integer;
  RegionStart: Integer;
begin
  if ExtendSelection and
     TryGetCaretRegion(FAnchorOffset, RegionStart, RegionFinish) then
    Value := NearestCaretStopInRegion(Value, RegionStart, RegionFinish)
  else
    Value := NearestCaretStop(Value);
  if not ExtendSelection then
    FAnchorOffset := Value;
  FActiveOffset := Value;
  DoSelectionChanged;
end;

procedure TDirectWriteVerseEditor.SetEditorDocument(
  const ADocument: IVerseVisualDocument);
var
  RegionFinish: Integer;
  RegionStart: Integer;
begin
  ClearVisualObjectHover;
  SetDocument(ADocument);
  SetLength(FCaretStops, 0);
  RefreshCaretStops;
  FAnchorOffset := NearestCaretStop(FAnchorOffset);
  if TryGetCaretRegion(FAnchorOffset, RegionStart, RegionFinish) then
    FActiveOffset := NearestCaretStopInRegion(FActiveOffset, RegionStart,
      RegionFinish)
  else
    FActiveOffset := FAnchorOffset;
  UpdateInputBrokerPosition;
  Invalidate;
end;

procedure TDirectWriteVerseEditor.SetInteractionMode(
  Value: TVerseInteractionMode);
var
  RegionFinish: Integer;
  RegionStart: Integer;
begin
  if FMode = Value then
    Exit;
  FMode := Value;
  FDragging := False;
  if FMode = vimEdit then
  begin
    RefreshCaretStops;
    FAnchorOffset := NearestCaretStop(FAnchorOffset);
    if TryGetCaretRegion(FAnchorOffset, RegionStart, RegionFinish) then
      FActiveOffset := NearestCaretStopInRegion(FActiveOffset, RegionStart,
        RegionFinish)
    else
      FActiveOffset := FAnchorOffset;
    FocusInputBroker
  end
  else
  begin
    FInputBroker.Visible := False;
    FAnchorOffset := 0;
    FActiveOffset := 0;
    DoSelectionChanged;
  end;
  Invalidate;
end;

procedure TDirectWriteVerseEditor.FocusInputBroker;
begin
  if FMode <> vimEdit then
    Exit;
  FInputBroker.Visible := True;
  UpdateInputBrokerPosition;
  if FInputBroker.CanSetFocus then
    try
      FInputBroker.SetFocus;
    except
      on EInvalidOperation do ;
    end;
end;

procedure TDirectWriteVerseEditor.UpdateInputBrokerPosition;
var
  CaretRect: TRect;
  BrokerHeight: Integer;
begin
  if not Assigned(FInputBroker) or (FMode <> vimEdit) then
    Exit;
  if not GetTextPositionClientRect(FActiveOffset, CaretRect) then
    Exit;
  BrokerHeight := Max(1, CaretRect.Bottom - CaretRect.Top);
  FInputBroker.Color := Color;
  FInputBroker.Font.Color := Color;
  FInputBroker.SetBounds(CaretRect.Left, CaretRect.Top, 1, BrokerHeight);
  FInputBroker.BringToFront;
end;

procedure TDirectWriteVerseEditor.RequestReplace(StartUTF16,
  LengthUTF16: Integer; const Replacement: UnicodeString);
var
  NewCaret: Integer;
  SafeReplacement: UnicodeString;
  Accepted: Boolean;
begin
  if not Assigned(FOnReplaceText) then
    Exit;
  SafeReplacement := NormalizeInsertedText(Replacement);
  NewCaret := StartUTF16 + Length(SafeReplacement);
  Accepted := False;
  FOnReplaceText(Self, StartUTF16, LengthUTF16, SafeReplacement,
    Accepted, NewCaret);
  if not Accepted then
    Exit;
  SetLength(FCaretStops, 0);
  RefreshCaretStops;
  FAnchorOffset := NearestCaretStop(NewCaret);
  FActiveOffset := FAnchorOffset;
  DoSelectionChanged;
end;

procedure TDirectWriteVerseEditor.RequestParagraphEdit(
  Kind: TVerseParagraphEditKind; StartUTF16, LengthUTF16: Integer);
var
  Accepted: Boolean;
  NewCaret: Integer;
begin
  if not Assigned(FOnParagraphEdit) then
    Exit;
  Accepted := False;
  NewCaret := FActiveOffset;
  FOnParagraphEdit(Self, Kind, StartUTF16, LengthUTF16, Accepted,
    NewCaret);
  if not Accepted then
    Exit;
  SetLength(FCaretStops, 0);
  RefreshCaretStops;
  FAnchorOffset := NearestCaretStop(NewCaret);
  FActiveOffset := FAnchorOffset;
  DoSelectionChanged;
end;

procedure TDirectWriteVerseEditor.ReplaceSelection(
  const Replacement: UnicodeString);
begin
  RequestReplace(SelectionStart, SelectionLength, Replacement);
end;

procedure TDirectWriteVerseEditor.DeleteBackward;
var
  Previous: Integer;
begin
  if SelectionLength > 0 then
  begin
    if SelectionContainsParagraphMark then
      RequestParagraphEdit(vpekDeleteSelection, SelectionStart,
        SelectionLength)
    else
      ReplaceSelection('');
    Exit;
  end;
  if IsParagraphMarkAt(FActiveOffset - 1) then
  begin
    RequestParagraphEdit(vpekDeleteBackward, FActiveOffset - 1, 1);
    Exit;
  end;
  Previous := PreviousCaretStop(FActiveOffset);
  if Previous < FActiveOffset then
    RequestReplace(Previous, FActiveOffset - Previous, '');
end;

procedure TDirectWriteVerseEditor.DeleteForward;
var
  Following: Integer;
begin
  if SelectionLength > 0 then
  begin
    if SelectionContainsParagraphMark then
      RequestParagraphEdit(vpekDeleteSelection, SelectionStart,
        SelectionLength)
    else
      ReplaceSelection('');
    Exit;
  end;
  if IsParagraphMarkAt(FActiveOffset) then
  begin
    RequestParagraphEdit(vpekDeleteForward, FActiveOffset, 1);
    Exit;
  end;
  Following := NextCaretStop(FActiveOffset);
  if Following > FActiveOffset then
    RequestReplace(FActiveOffset, Following - FActiveOffset, '');
end;

procedure TDirectWriteVerseEditor.CopySelection;
var
  SelectedText: UnicodeString;
  Handled: Boolean;
begin
  if (SelectionLength <= 0) or not Assigned(Document) then
    Exit;
  Handled := False;
  if Assigned(FOnClipboardCopy) then
    FOnClipboardCopy(Self, SelectionStart, SelectionLength, False, Handled);
  if Handled then Exit;
  SelectedText := Copy(Document.Text, SelectionStart + 1, SelectionLength);
  Clipboard.AsText := UTF8Encode(SelectedText);
end;

procedure TDirectWriteVerseEditor.CutSelection;
var
  Handled: Boolean;
begin
  if SelectionLength <= 0 then
    Exit;
  Handled := False;
  if Assigned(FOnClipboardCopy) then
    FOnClipboardCopy(Self, SelectionStart, SelectionLength, True, Handled);
  if Handled then Exit;
  CopySelection;
  ReplaceSelection('');
end;

procedure TDirectWriteVerseEditor.PasteText;
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnClipboardPaste) then
    FOnClipboardPaste(Self, SelectionStart, SelectionLength, Handled);
  if Handled then Exit;
  ReplaceSelection(UTF8Decode(Clipboard.AsText));
end;

function TDirectWriteVerseEditor.IsWordCodeUnit(
  const AText: UnicodeString; AIndex: Integer): Boolean;
var
  Category: TUnicodeCategory;
  Value: WideChar;
begin
  Result := False;
  if (AIndex < 1) or (AIndex > Length(AText)) then
    Exit;
  Value := AText[AIndex];
  if Ord(Value) < 128 then
  begin
    { Keep the editor's established ASCII boundaries without converting a
      Unicode code unit through the process code page. }
    case Ord(Value) of
      0..32, $21, $22, $27..$29, $2C..$2F, $3A, $3B, $3F,
      $5B..$5D, $7B, $7D:
        Exit(False);
    end;
    Exit(True);
  end;

  { TCharacter needs the high-surrogate index. DirectWrite caret stops keep
    the whole pair or combining cluster atomic, but scans can approach it
    from either side. }
  if TCharacter.IsLowSurrogate(Value) then
  begin
    if (AIndex <= 1) or
       not TCharacter.IsHighSurrogate(AText[AIndex - 1]) then
      Exit(False);
    Dec(AIndex);
  end
  else if TCharacter.IsHighSurrogate(Value) and
          ((AIndex >= Length(AText)) or
           not TCharacter.IsLowSurrogate(AText[AIndex + 1])) then
    Exit(False);

  Category := TCharacter.GetUnicodeCategory(AText, AIndex);
  Result := Category in [
    TUnicodeCategory.ucUppercaseLetter,
    TUnicodeCategory.ucLowercaseLetter,
    TUnicodeCategory.ucTitlecaseLetter,
    TUnicodeCategory.ucModifierLetter,
    TUnicodeCategory.ucOtherLetter,
    TUnicodeCategory.ucNonSpacingMark,
    TUnicodeCategory.ucCombiningMark,
    TUnicodeCategory.ucEnclosingMark,
    TUnicodeCategory.ucDecimalNumber,
    TUnicodeCategory.ucLetterNumber,
    TUnicodeCategory.ucOtherNumber,
    TUnicodeCategory.ucConnectPunctuation];
end;

procedure TDirectWriteVerseEditor.MoveCaretHorizontal(Direction: Integer;
  ExtendSelection, ByWord: Boolean);
var
  DocText: UnicodeString;
  FirstOffset: Integer;
  LastOffset: Integer;
  NewOffset: Integer;
begin
  if not ExtendSelection and (SelectionLength > 0) then
  begin
    if Direction < 0 then
      NewOffset := SelectionStart
    else
      NewOffset := SelectionStart + SelectionLength;
    SetCaret(NewOffset, False);
    Exit;
  end;

  NewOffset := FActiveOffset;
  if not ByWord then
  begin
    if Direction < 0 then
      NewOffset := PreviousCaretStop(NewOffset)
    else
      NewOffset := NextCaretStop(NewOffset);
  end
  else
  begin
    if Length(FCaretStops) = 0 then
      RefreshCaretStops;
    if (Length(FCaretStops) = 0) or
       not TryGetCaretRegion(NewOffset, FirstOffset, LastOffset) then
      Exit;
    DocText := '';
    if Assigned(Document) then
      DocText := Document.Text;
    if Direction < 0 then
    begin
      while (NewOffset > FirstOffset) and
        not IsWordCodeUnit(DocText, NewOffset) do
        Dec(NewOffset);
      while (NewOffset > FirstOffset) and
        IsWordCodeUnit(DocText, NewOffset) do
        Dec(NewOffset);
    end
    else
    begin
      while (NewOffset < LastOffset) and
        not IsWordCodeUnit(DocText, NewOffset + 1) do
        Inc(NewOffset);
      while (NewOffset < LastOffset) and
        IsWordCodeUnit(DocText, NewOffset + 1) do
        Inc(NewOffset);
    end;
  end;
  SetCaret(NewOffset, ExtendSelection);
end;

procedure TDirectWriteVerseEditor.MoveCaretVertical(Direction: Integer;
  ExtendSelection: Boolean);
var
  CaretRect: TRect;
  Inside: Boolean;
  LineRect: TRect;
  Offset: Integer;
  Point: TPoint;
  Trailing: Boolean;
begin
  if not GetTextPositionClientRect(FActiveOffset, CaretRect) then
    Exit;
  if not GetTextLineClientRect(FActiveOffset, LineRect) then
    LineRect := CaretRect;
  Point.X := CaretRect.Left;
  if Direction < 0 then
    Point.Y := LineRect.Top - Max(1, LineRect.Bottom - LineRect.Top) div 2
  else
    Point.Y := LineRect.Bottom +
      Max(1, LineRect.Bottom - LineRect.Top) div 2;
  if HitTestTextOffset(Point, Offset, Trailing, Inside) then
    SetCaret(Offset, ExtendSelection);
end;

procedure TDirectWriteVerseEditor.BrokerChange(Sender: TObject);
var
  InputText: UnicodeString;
begin
  if FBrokerChanging or (FMode <> vimEdit) or
     (FInputBroker.Text = '') then
    Exit;
  InputText := UTF8Decode(FInputBroker.Text);
  FBrokerChanging := True;
  try
    FInputBroker.Text := '';
  finally
    FBrokerChanging := False;
  end;
  if InputText <> '' then
    ReplaceSelection(InputText);
end;

procedure TDirectWriteVerseEditor.BrokerKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  BasicShift: TShiftState;
  RegionFinish: Integer;
  RegionStart: Integer;
begin
  if FMode <> vimEdit then
    Exit;
  BasicShift := Shift * [ssShift, ssCtrl, ssAlt];
  if (Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END]) and
     Assigned(FOnCommand) then
    FOnCommand(Self, vecClearPendingFormatting);
  case Key of
    VK_BACK:
      if not (ssCtrl in BasicShift) then
        DeleteBackward;
    VK_DELETE:
      if not (ssCtrl in BasicShift) then
        DeleteForward;
    VK_LEFT:
      MoveCaretHorizontal(-1, ssShift in BasicShift, ssCtrl in BasicShift);
    VK_RIGHT:
      MoveCaretHorizontal(1, ssShift in BasicShift, ssCtrl in BasicShift);
    VK_UP:
      MoveCaretVertical(-1, ssShift in BasicShift);
    VK_DOWN:
      MoveCaretVertical(1, ssShift in BasicShift);
    VK_HOME:
      begin
        RefreshCaretStops;
        if TryGetCaretRegion(FActiveOffset, RegionStart, RegionFinish) then
          SetCaret(RegionStart, ssShift in BasicShift);
      end;
    VK_END:
      begin
        RefreshCaretStops;
        if TryGetCaretRegion(FActiveOffset, RegionStart, RegionFinish) then
          SetCaret(RegionFinish, ssShift in BasicShift);
      end;
    VK_A:
      if ssCtrl in BasicShift then
        SelectAll
      else
        Exit;
    VK_C:
      if ssCtrl in BasicShift then
        CopySelection
      else
        Exit;
    VK_X:
      if ssCtrl in BasicShift then
        CutSelection
      else
        Exit;
    VK_V:
      if ssCtrl in BasicShift then
        PasteText
      else
        Exit;
    VK_Z:
      if ssCtrl in BasicShift then
      begin
        if Assigned(FOnCommand) then
          if ssShift in BasicShift then
            FOnCommand(Self, vecRedo)
          else
            FOnCommand(Self, vecUndo);
      end
      else
        Exit;
    VK_Y:
      if ssCtrl in BasicShift then
      begin
        if Assigned(FOnCommand) then
          FOnCommand(Self, vecRedo);
      end
      else
        Exit;
    VK_S:
      if BasicShift = [ssCtrl, ssShift] then
      begin
        if Assigned(FOnCommand) then
          FOnCommand(Self, vecStrikethrough);
      end
      else if BasicShift = [ssCtrl] then
      begin
        if Assigned(FOnCommand) then
          FOnCommand(Self, vecSave);
      end
      else
        Exit;
    VK_ESCAPE:
      begin
        if Assigned(FOnCommand) then
          FOnCommand(Self, vecCancel);
      end;
    VK_B:
      if BasicShift = [ssCtrl] then
      begin
        if Assigned(FOnCommand) then FOnCommand(Self, vecHtmlBold);
      end
      else Exit;
    VK_I:
      if BasicShift = [ssCtrl, ssShift] then
      begin
        if Assigned(FOnCommand) then FOnCommand(Self, vecHtmlItalic);
      end
      else if BasicShift = [ssCtrl] then
      begin
        if Assigned(FOnCommand) then
          FOnCommand(Self, vecItalic);
      end
      else
        Exit;
    VK_U:
      if BasicShift = [ssCtrl] then
      begin
        if Assigned(FOnCommand) then FOnCommand(Self, vecHtmlUnderline);
      end
      else Exit;
    VK_OEM_COMMA:
      if BasicShift = [ssCtrl, ssShift] then
      begin
        if Assigned(FOnCommand) then FOnCommand(Self, vecFontSizeDecrease);
      end
      else Exit;
    VK_OEM_PERIOD:
      if BasicShift = [ssCtrl, ssShift] then
      begin
        if Assigned(FOnCommand) then FOnCommand(Self, vecFontSizeIncrease);
      end
      else Exit;
    VK_J:
      if BasicShift = [ssCtrl] then
      begin
        if Assigned(FOnCommand) then
          FOnCommand(Self, vecWordsOfJesus);
      end
      else
        Exit;
    VK_O:
      if BasicShift = [ssCtrl] then
      begin
        if Assigned(FOnCommand) then
          FOnCommand(Self, vecOldTestamentQuote);
      end
      else
        Exit;
    VK_OEM_PLUS:
      if BasicShift = [ssCtrl, ssShift] then
      begin
        if Assigned(FOnCommand) then
          FOnCommand(Self, vecSuperscript);
      end
      else if BasicShift = [ssCtrl] then
      begin
        if Assigned(FOnCommand) then
          FOnCommand(Self, vecSubscript);
      end
      else
        Exit;
    VK_N:
      if BasicShift = [ssCtrl, ssShift] then
      begin
        if Assigned(FOnCommand) then
          FOnCommand(Self, vecInsertTranslatorNote);
      end
      else
        Exit;
    VK_RETURN:
      if BasicShift = [] then
        RequestParagraphEdit(vpekInsert, SelectionStart, SelectionLength)
      else
        Exit;
  else
    Exit;
  end;
  Key := 0;
end;

procedure TDirectWriteVerseEditor.BrokerUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
var
  InputText: UnicodeString;
begin
  if (FMode <> vimEdit) or (UTF8Key = '') then
    Exit;
  InputText := UTF8Decode(UTF8Key);
  if (InputText <> '') and (InputText[1] >= #32) then
    ReplaceSelection(InputText);
  UTF8Key := '';
end;

procedure TDirectWriteVerseEditor.ClearVisualObjectHover;
var
  LeaveHit: TVerseHit;
begin
  if FLastVisualObjectRole = vsrContent then
    Exit;
  FillChar(LeaveHit, SizeOf(LeaveHit), 0);
  LeaveHit.LogicalIndex := -1;
  LeaveHit.UTF16Offset := -1;
  if Assigned(Document) then
    LeaveHit.Revision := Document.Revision;
  if Assigned(FOnVisualObjectHover) then
    FOnVisualObjectHover(Self, LeaveHit);
  FLastVisualObjectRole := vsrContent;
  FLastVisualObjectId := 0;
end;

procedure TDirectWriteVerseEditor.UpdateVisualObjectHover(X, Y: Integer);
var
  Hit: TVerseHit;
begin
  if HitTest(Point(X, Y), Hit) and
     (Hit.Role = vsrTranslatorNote) then
  begin
    Cursor := crHandPoint;
    if (Hit.Role <> FLastVisualObjectRole) or
       (Hit.ObjectId <> FLastVisualObjectId) then
    begin
      ClearVisualObjectHover;
      FLastVisualObjectRole := Hit.Role;
      FLastVisualObjectId := Hit.ObjectId;
      if Assigned(FOnVisualObjectHover) then
        FOnVisualObjectHover(Self, Hit);
    end;
  end
  else
  begin
    Cursor := crDefault;
    ClearVisualObjectHover;
  end;
end;

procedure TDirectWriteVerseEditor.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Inside: Boolean;
  Offset: Integer;
  Trailing: Boolean;
  Hit: TVerseHit;
begin
  if FMode <> vimEdit then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;
  if Assigned(FOnCommand) then
    FOnCommand(Self, vecClearPendingFormatting);
  if (Button = mbLeft) and HitTest(Point(X, Y), Hit) and
     (Hit.Role = vsrTranslatorNote) then
  begin
    if Assigned(FOnVisualObjectClick) then
      FOnVisualObjectClick(Self, Hit);
    FocusInputBroker;
    Exit;
  end;
  if (Button <> mbLeft) or
     not HitTestTextOffset(Point(X, Y), Offset, Trailing, Inside) then
    Exit;
  RefreshCaretStops;
  if not IsInteractiveHitOffset(Offset, Trailing) then
    Exit;
  FDragging := True;
  SetCaret(Offset, ssShift in Shift);
  FocusInputBroker;
end;

procedure TDirectWriteVerseEditor.MouseLeave;
begin
  ClearVisualObjectHover;
  inherited MouseLeave;
end;

procedure TDirectWriteVerseEditor.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  Inside: Boolean;
  Offset: Integer;
  RegionFinish: Integer;
  RegionStart: Integer;
  Trailing: Boolean;
begin
  if FMode <> vimEdit then
  begin
    inherited MouseMove(Shift, X, Y);
    Exit;
  end;
  if FDragging and HitTestTextOffset(Point(X, Y), Offset, Trailing,
    Inside) then
  begin
    if not TryGetCaretRegion(FAnchorOffset, RegionStart, RegionFinish) then
      Exit;
    FActiveOffset := NearestCaretStopInRegion(Offset, RegionStart,
      RegionFinish);
    DoSelectionChanged;
  end
  else
    UpdateVisualObjectHover(X, Y);
end;

procedure TDirectWriteVerseEditor.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FMode <> vimEdit then
  begin
    inherited MouseUp(Button, Shift, X, Y);
    Exit;
  end;
  if Button = mbLeft then
    FDragging := False;
end;

procedure TDirectWriteVerseEditor.SelectWordAt(UTF16Offset: Integer);
var
  DocText: UnicodeString;
  Finish: Integer;
  FirstOffset: Integer;
  LastOffset: Integer;
  Start: Integer;
begin
  if not Assigned(Document) or (GetTextLength = 0) then
    Exit;
  RefreshCaretStops;
  if Length(FCaretStops) = 0 then
    Exit;
  UTF16Offset := NearestCaretStop(UTF16Offset);
  if not TryGetCaretRegion(UTF16Offset, FirstOffset, LastOffset) then
    Exit;
  DocText := Document.Text;
  UTF16Offset := Min(UTF16Offset, LastOffset - 1);
  UTF16Offset := Max(UTF16Offset, FirstOffset);
  Start := UTF16Offset;
  Finish := UTF16Offset;
  while (Start > FirstOffset) and IsWordCodeUnit(DocText, Start) do
    Dec(Start);
  while (Finish < LastOffset) and
    IsWordCodeUnit(DocText, Finish + 1) do
    Inc(Finish);
  SetSelection(Start, Finish - Start);
end;

procedure TDirectWriteVerseEditor.DblClick;
var
  CursorPoint: TPoint;
  Inside: Boolean;
  Offset: Integer;
  Trailing: Boolean;
begin
  if FMode <> vimEdit then
  begin
    inherited DblClick;
    Exit;
  end;
  CursorPoint := ScreenToClient(Mouse.CursorPos);
  if HitTestTextOffset(CursorPoint, Offset, Trailing, Inside) and
     IsInteractiveHitOffset(Offset, Trailing) then
    SelectWordAt(Offset);
end;

procedure TDirectWriteVerseEditor.Paint;
var
  CaretRect: TRect;
  Index: Integer;
  Rects: TRectArray;
begin
  inherited Paint;
  if FMode <> vimEdit then
    Exit;

  if (SelectionLength > 0) and
     GetTextRangeClientRects(SelectionStart, SelectionLength, Rects) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := clHighlight;
    Canvas.Pen.Style := psDot;
    for Index := 0 to High(Rects) do
      Canvas.Rectangle(Rects[Index].Left, Rects[Index].Top,
        Rects[Index].Right, Rects[Index].Bottom);
    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Style := bsSolid;
  end;

  if (SelectionLength = 0) and
     GetTextPositionClientRect(FActiveOffset, CaretRect) then
  begin
    Canvas.Pen.Color := clWindowText;
    Canvas.Line(CaretRect.Left, CaretRect.Top,
      CaretRect.Left, CaretRect.Bottom);
  end;
  UpdateInputBrokerPosition;
end;

procedure TDirectWriteVerseEditor.Resize;
begin
  inherited Resize;
  UpdateInputBrokerPosition;
end;

end.
