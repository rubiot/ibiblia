unit directwrite_verse_editor;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Types, Controls, StdCtrls, Graphics, LCLType, LCLProc,
  Clipbrd, verse_visual_document, verse_view_intf,
  directwrite_syntagm_view;

type
  TVerseInteractionMode = (vimAssociation, vimEdit);
  TVerseEditorCommand = (vecUndo, vecRedo, vecSave, vecCancel);

  TVerseTextReplaceEvent = procedure(Sender: TObject; StartUTF16,
    LengthUTF16: Integer; const Replacement: UnicodeString;
    var Accepted: Boolean) of object;
  TVerseEditorCommandEvent = procedure(Sender: TObject;
    Command: TVerseEditorCommand) of object;
  TVerseSelectionChangedEvent = procedure(Sender: TObject;
    StartUTF16, LengthUTF16: Integer) of object;
  TVerseClipboardCopyEvent = procedure(Sender: TObject;
    StartUTF16, LengthUTF16: Integer; ACut: Boolean;
    var Handled: Boolean) of object;
  TVerseClipboardPasteEvent = procedure(Sender: TObject;
    StartUTF16, LengthUTF16: Integer; var Handled: Boolean) of object;

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
    FOnReplaceText: TVerseTextReplaceEvent;
    FOnSelectionChanged: TVerseSelectionChangedEvent;
    procedure BrokerChange(Sender: TObject);
    procedure BrokerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BrokerUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure CopySelection;
    procedure CutSelection;
    procedure DeleteBackward;
    procedure DeleteForward;
    procedure DoSelectionChanged;
    procedure FocusInputBroker;
    function GetCaretOffset: Integer;
    function GetSelectionLength: Integer;
    function GetSelectionStart: Integer;
    function GetTextLength: Integer;
    function IsWordCodeUnit(Value: WideChar): Boolean;
    procedure MoveCaretHorizontal(Direction: Integer; ExtendSelection,
      ByWord: Boolean);
    procedure MoveCaretVertical(Direction: Integer; ExtendSelection: Boolean);
    function NearestCaretStop(Value: Integer): Integer;
    function NextCaretStop(Value: Integer): Integer;
    procedure PasteText;
    function PreviousCaretStop(Value: Integer): Integer;
    procedure RefreshCaretStops;
    procedure ReplaceSelection(const Replacement: UnicodeString);
    procedure RequestReplace(StartUTF16, LengthUTF16: Integer;
      const Replacement: UnicodeString);
    procedure SelectWordAt(UTF16Offset: Integer);
    procedure SetCaret(Value: Integer; ExtendSelection: Boolean);
    procedure SetInteractionMode(Value: TVerseInteractionMode);
    procedure UpdateInputBrokerPosition;
  protected
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
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
    property OnReplaceText: TVerseTextReplaceEvent read FOnReplaceText
      write FOnReplaceText;
    property OnSelectionChanged: TVerseSelectionChangedEvent
      read FOnSelectionChanged write FOnSelectionChanged;
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
  DocText: UnicodeString;
  Index: Integer;
  StopCount: Integer;
begin
  if GetCaretStops(FCaretStops) then
    Exit;

  DocText := '';
  if Assigned(Document) then
    DocText := Document.Text;
  SetLength(FCaretStops, Length(DocText) + 1);
  StopCount := 1;
  FCaretStops[0] := 0;
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
    FCaretStops[StopCount] := Index - 1;
    Inc(StopCount);
  end;
  SetLength(FCaretStops, StopCount);
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

function TDirectWriteVerseEditor.PreviousCaretStop(Value: Integer): Integer;
var
  Index: Integer;
begin
  Result := 0;
  if Length(FCaretStops) = 0 then
    RefreshCaretStops;
  for Index := High(FCaretStops) downto 0 do
    if FCaretStops[Index] < Value then
      Exit(FCaretStops[Index]);
end;

function TDirectWriteVerseEditor.NextCaretStop(Value: Integer): Integer;
var
  Index: Integer;
begin
  Result := GetTextLength;
  if Length(FCaretStops) = 0 then
    RefreshCaretStops;
  for Index := 0 to High(FCaretStops) do
    if FCaretStops[Index] > Value then
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
begin
  RefreshCaretStops;
  EndOffset := StartUTF16 + LengthUTF16;
  FAnchorOffset := NearestCaretStop(StartUTF16);
  FActiveOffset := NearestCaretStop(EndOffset);
  DoSelectionChanged;
end;

procedure TDirectWriteVerseEditor.SelectAll;
begin
  FAnchorOffset := 0;
  FActiveOffset := GetTextLength;
  DoSelectionChanged;
end;

procedure TDirectWriteVerseEditor.SetCaret(Value: Integer;
  ExtendSelection: Boolean);
begin
  Value := NearestCaretStop(Value);
  if not ExtendSelection then
    FAnchorOffset := Value;
  FActiveOffset := Value;
  DoSelectionChanged;
end;

procedure TDirectWriteVerseEditor.SetEditorDocument(
  const ADocument: IVerseVisualDocument);
begin
  SetDocument(ADocument);
  SetLength(FCaretStops, 0);
  RefreshCaretStops;
  FAnchorOffset := NearestCaretStop(FAnchorOffset);
  FActiveOffset := NearestCaretStop(FActiveOffset);
  UpdateInputBrokerPosition;
  Invalidate;
end;

procedure TDirectWriteVerseEditor.SetInteractionMode(
  Value: TVerseInteractionMode);
begin
  if FMode = Value then
    Exit;
  FMode := Value;
  FDragging := False;
  if FMode = vimEdit then
    FocusInputBroker
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
    Accepted);
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
    ReplaceSelection('');
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
    ReplaceSelection('');
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

function TDirectWriteVerseEditor.IsWordCodeUnit(Value: WideChar): Boolean;
begin
  Result := (Value > ' ') and
    not (Value in ['.', ',', ';', ':', '!', '?', '(', ')', '[', ']',
      '{', '}', '"', '''', '/', '\', '-']);
end;

procedure TDirectWriteVerseEditor.MoveCaretHorizontal(Direction: Integer;
  ExtendSelection, ByWord: Boolean);
var
  DocText: UnicodeString;
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
    DocText := '';
    if Assigned(Document) then
      DocText := Document.Text;
    if Direction < 0 then
    begin
      while (NewOffset > 0) and
        not IsWordCodeUnit(DocText[NewOffset]) do
        Dec(NewOffset);
      while (NewOffset > 0) and
        IsWordCodeUnit(DocText[NewOffset]) do
        Dec(NewOffset);
    end
    else
    begin
      while (NewOffset < Length(DocText)) and
        not IsWordCodeUnit(DocText[NewOffset + 1]) do
        Inc(NewOffset);
      while (NewOffset < Length(DocText)) and
        IsWordCodeUnit(DocText[NewOffset + 1]) do
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
  Offset: Integer;
  Point: TPoint;
  Trailing: Boolean;
begin
  if not GetTextPositionClientRect(FActiveOffset, CaretRect) then
    Exit;
  Point.X := CaretRect.Left;
  if Direction < 0 then
    Point.Y := CaretRect.Top - Max(1, CaretRect.Bottom - CaretRect.Top) div 2
  else
    Point.Y := CaretRect.Bottom +
      Max(1, CaretRect.Bottom - CaretRect.Top) div 2;
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
begin
  if FMode <> vimEdit then
    Exit;
  BasicShift := Shift * [ssShift, ssCtrl];
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
      SetCaret(0, ssShift in BasicShift);
    VK_END:
      SetCaret(GetTextLength, ssShift in BasicShift);
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
      if ssCtrl in BasicShift then
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
    VK_RETURN:
      ;
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

procedure TDirectWriteVerseEditor.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Inside: Boolean;
  Offset: Integer;
  Trailing: Boolean;
begin
  if FMode <> vimEdit then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;
  if (Button <> mbLeft) or
     not HitTestTextOffset(Point(X, Y), Offset, Trailing, Inside) then
    Exit;
  FDragging := True;
  RefreshCaretStops;
  Offset := NearestCaretStop(Offset);
  if ssShift in Shift then
    FActiveOffset := Offset
  else
  begin
    FAnchorOffset := Offset;
    FActiveOffset := Offset;
  end;
  DoSelectionChanged;
  FocusInputBroker;
end;

procedure TDirectWriteVerseEditor.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  Inside: Boolean;
  Offset: Integer;
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
    FActiveOffset := NearestCaretStop(Offset);
    DoSelectionChanged;
  end;
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
  Start: Integer;
begin
  if not Assigned(Document) or (GetTextLength = 0) then
    Exit;
  DocText := Document.Text;
  UTF16Offset := Min(UTF16Offset, Length(DocText) - 1);
  Start := UTF16Offset;
  Finish := UTF16Offset;
  while (Start > 0) and IsWordCodeUnit(DocText[Start]) do
    Dec(Start);
  while (Finish < Length(DocText)) and
    IsWordCodeUnit(DocText[Finish + 1]) do
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
  if HitTestTextOffset(CursorPoint, Offset, Trailing, Inside) then
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
