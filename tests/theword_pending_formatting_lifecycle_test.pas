unit theword_pending_formatting_lifecycle_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, LCLType,
  fpcunit, testregistry,
  directwrite_verse_editor, theword_ont_codec_v2,
  theword_wysiwyg_editor_control;

type

  { TTheWordPendingFormattingLifecycleTests }

  TTheWordPendingFormattingLifecycleTests = class(TTestCase)
  private
    FCommandCount: Integer;
    FLastCommand: TVerseEditorCommand;
    procedure AssertFormattingAt(const AMessage, ARaw: string;
      AOffsetUTF16, ASizeLevel: Integer; AHasColor: Boolean;
      AColor: LongInt);
    procedure ConfigureEditor(AEditor: TTheWordWysiwygEditorControl;
      out ADirectEditor: TDirectWriteVerseEditor; out ABroker: TEdit);
    procedure EditorCommand(Sender: TTheWordWysiwygEditorControl;
      ACommand: TVerseEditorCommand);
    function PressKey(ABroker: TEdit; AKey: Word;
      AShift: TShiftState): Word;
    procedure TypeAscii(ABroker: TEdit; AValue: AnsiChar);
  published
    procedure CombinedSizeAndColorSurviveContiguousTypingWithAtomicUndo;
    procedure PendingFormattingSurvivesParagraphAndSoftBreak;
    procedure PendingFormattingClearsOnModeEditableCommitAndFailedSave;
    procedure PendingFormattingClearsOnUndoRedoNavigationAndPaste;
  end;

implementation

uses
  Clipbrd;

procedure TTheWordPendingFormattingLifecycleTests.AssertFormattingAt(
  const AMessage, ARaw: string; AOffsetUTF16, ASizeLevel: Integer;
  AHasColor: Boolean; AColor: LongInt);
var
  AnyColor: Boolean;
  AnySize: Boolean;
  Codec: TTheWordOntCodecV2;
  ColorFound: Boolean;
  Document: TTheWordVerseDocument;
  Index: Integer;
  Mark: TTheWordFontMark;
  SizeFound: Boolean;
begin
  Codec := TTheWordOntCodecV2.Create;
  Document := nil;
  try
    Document := Codec.DecodeVerse(UTF8Decode(ARaw));
    AssertTrue(AMessage + '.offset',
      (AOffsetUTF16 >= 0) and
      (AOffsetUTF16 < Length(Document.VisibleText)));
    SizeFound := False;
    ColorFound := False;
    AnySize := False;
    AnyColor := False;
    for Index := 0 to Document.FontMarkCount - 1 do
    begin
      Mark := Document.FontMarks[Index];
      if (Mark.StartOffset > AOffsetUTF16) or
         (Mark.EndOffset <= AOffsetUTF16) then
        Continue;
      if Mark.HasSize then
      begin
        AnySize := True;
        if Mark.RelativeSize = ASizeLevel then
          SizeFound := True;
      end;
      if Mark.HasColor then
      begin
        AnyColor := True;
        if Mark.ResolvedColor = AColor then
          ColorFound := True;
      end;
    end;
    if ASizeLevel = 0 then
      AssertFalse(AMessage + '.size', AnySize)
    else
      AssertTrue(AMessage + '.size', SizeFound);
    if AHasColor then
      AssertTrue(AMessage + '.color', ColorFound)
    else
      AssertFalse(AMessage + '.color', AnyColor);
  finally
    Document.Free;
    Codec.Free;
  end;
end;

procedure TTheWordPendingFormattingLifecycleTests.ConfigureEditor(
  AEditor: TTheWordWysiwygEditorControl;
  out ADirectEditor: TDirectWriteVerseEditor; out ABroker: TEdit);
var
  ComponentIndex: Integer;
begin
  ADirectEditor := nil;
  ABroker := nil;
  for ComponentIndex := 0 to AEditor.ComponentCount - 1 do
    if AEditor.Components[ComponentIndex] is TDirectWriteVerseEditor then
    begin
      ADirectEditor := TDirectWriteVerseEditor(
        AEditor.Components[ComponentIndex]);
      Break;
    end;
  AssertTrue('DirectWrite editor exists', Assigned(ADirectEditor));
  for ComponentIndex := 0 to ADirectEditor.ComponentCount - 1 do
    if ADirectEditor.Components[ComponentIndex] is TEdit then
    begin
      ABroker := TEdit(ADirectEditor.Components[ComponentIndex]);
      Break;
    end;
  AssertTrue('native input broker exists', Assigned(ABroker));
  AssertTrue('native UTF-8 input handler exists',
    Assigned(ABroker.OnUTF8KeyPress));
  AssertTrue('native key handler exists', Assigned(ABroker.OnKeyDown));
end;

procedure TTheWordPendingFormattingLifecycleTests.EditorCommand(
  Sender: TTheWordWysiwygEditorControl; ACommand: TVerseEditorCommand);
begin
  Inc(FCommandCount);
  FLastCommand := ACommand;
  { Deliberately do not commit. This models a rejected save attempt: pending
    presentation must already have been cleared before control returns here. }
end;

function TTheWordPendingFormattingLifecycleTests.PressKey(ABroker: TEdit;
  AKey: Word; AShift: TShiftState): Word;
begin
  Result := AKey;
  ABroker.OnKeyDown(ABroker, Result, AShift);
end;

procedure TTheWordPendingFormattingLifecycleTests.TypeAscii(ABroker: TEdit;
  AValue: AnsiChar);
var
  Key: TUTF8Char;
begin
  Key := AValue;
  ABroker.OnUTF8KeyPress(ABroker, Key);
  AssertEquals('typed input is consumed', '', Key);
end;

procedure TTheWordPendingFormattingLifecycleTests.
  CombinedSizeAndColorSurviveContiguousTypingWithAtomicUndo;
var
  Broker: TEdit;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
  RawAfterFirstInput: string;
  Red: LongInt;
begin
  Red := LongInt(ColorToRGB(clRed)) and $00FFFFFF;
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  try
    ConfigureEditor(Editor, DirectEditor, Broker);
    Editor.Text := 'x';
    DirectEditor.SetSelection(1, 0);
    AssertTrue('A+ activates pending relative size',
      Editor.AdjustRelativeFontSize(1));
    AssertTrue('color activates pending explicit color',
      Editor.ApplyFontColor(True, clRed));

    TypeAscii(Broker, 'a');
    RawAfterFirstInput := Editor.Text;
    AssertFormattingAt('first input', RawAfterFirstInput, 1, 1, True, Red);
    TypeAscii(Broker, 'b');
    AssertFormattingAt('first contiguous input', Editor.Text,
      1, 1, True, Red);
    AssertFormattingAt('second contiguous input', Editor.Text,
      2, 1, True, Red);

    AssertTrue('second input has one undo record', Editor.Undo);
    AssertEquals('one undo removes text, size and color atomically',
      RawAfterFirstInput, Editor.Text);
    AssertTrue('first input has one undo record', Editor.Undo);
    AssertEquals('second undo restores the original text', 'x', Editor.Text);
    AssertFalse('pending commands created no separate undo records',
      Editor.Undo);
  finally
    Editor.Free;
  end;
end;

procedure TTheWordPendingFormattingLifecycleTests.
  PendingFormattingSurvivesParagraphAndSoftBreak;
var
  Broker: TEdit;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
  Red: LongInt;
begin
  Red := LongInt(ColorToRGB(clRed)) and $00FFFFFF;
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  try
    ConfigureEditor(Editor, DirectEditor, Broker);
    Editor.Text := 'x';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.AdjustRelativeFontSize(1));
    AssertTrue(Editor.ApplyFontColor(True, clRed));
    TypeAscii(Broker, 'a');
    AssertEquals('Enter is consumed', 0, PressKey(Broker, VK_RETURN, []));
    TypeAscii(Broker, 'b');
    AssertEquals('Shift+Enter is consumed', 0,
      PressKey(Broker, VK_RETURN, [ssShift]));
    TypeAscii(Broker, 'c');

    AssertTrue('paragraph break is serialized', Pos('<CM>', Editor.Text) > 0);
    AssertTrue('soft break is serialized', Pos('<CL>', Editor.Text) > 0);
    AssertFormattingAt('before breaks', Editor.Text, 1, 1, True, Red);
    AssertFormattingAt('after paragraph break', Editor.Text,
      2, 1, True, Red);
    AssertFormattingAt('after soft break', Editor.Text,
      3, 1, True, Red);
  finally
    Editor.Free;
  end;
end;

procedure TTheWordPendingFormattingLifecycleTests.
  PendingFormattingClearsOnModeEditableCommitAndFailedSave;
var
  Broker: TEdit;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
begin
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  try
    ConfigureEditor(Editor, DirectEditor, Broker);

    Editor.Text := 'x';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.AdjustRelativeFontSize(1));
    AssertTrue(Editor.ApplyFontColor(True, clRed));
    Editor.RawMode := True;
    Editor.RawMode := False;
    DirectEditor.SetSelection(1, 0);
    TypeAscii(Broker, 'a');
    AssertFormattingAt('Raw mode clears pending formatting', Editor.Text,
      1, 0, False, 0);

    Editor.Text := 'x';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.AdjustRelativeFontSize(1));
    AssertTrue(Editor.ApplyFontColor(True, clRed));
    Editor.Editable := False;
    Editor.Editable := True;
    TypeAscii(Broker, 'a');
    AssertFormattingAt('disabling clears pending formatting', Editor.Text,
      1, 0, False, 0);

    Editor.Text := 'x';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.AdjustRelativeFontSize(1));
    AssertTrue(Editor.ApplyFontColor(True, clRed));
    AssertTrue('visual CommitRaw succeeds', Editor.CommitRaw);
    TypeAscii(Broker, 'a');
    AssertFormattingAt('CommitRaw clears pending formatting', Editor.Text,
      1, 0, False, 0);

    Editor.Text := 'x';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.AdjustRelativeFontSize(1));
    AssertTrue(Editor.ApplyFontColor(True, clRed));
    FCommandCount := 0;
    FLastCommand := vecUndo;
    Editor.OnCommand := @EditorCommand;
    AssertEquals('Ctrl+S is consumed', 0,
      PressKey(Broker, VK_S, [ssCtrl]));
    AssertEquals('one save attempt is forwarded', 1, FCommandCount);
    AssertEquals('forwarded command is save', Ord(vecSave),
      Ord(FLastCommand));
    TypeAscii(Broker, 'a');
    AssertFormattingAt('failed save attempt clears pending formatting',
      Editor.Text, 1, 0, False, 0);
  finally
    Editor.Free;
  end;
end;

procedure TTheWordPendingFormattingLifecycleTests.
  PendingFormattingClearsOnUndoRedoNavigationAndPaste;
var
  Broker: TEdit;
  DirectEditor: TDirectWriteVerseEditor;
  Editor: TTheWordWysiwygEditorControl;
begin
  Editor := TTheWordWysiwygEditorControl.Create(nil);
  try
    ConfigureEditor(Editor, DirectEditor, Broker);

    Editor.Text := 'x';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.AdjustRelativeFontSize(1));
    AssertTrue(Editor.ApplyFontColor(True, clRed));
    AssertFalse('there is no edit to undo', Editor.Undo);
    TypeAscii(Broker, 'a');
    AssertFormattingAt('Undo clears pending formatting', Editor.Text,
      1, 0, False, 0);

    Editor.Text := 'x';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.AdjustRelativeFontSize(1));
    AssertTrue(Editor.ApplyFontColor(True, clRed));
    AssertFalse('there is no edit to redo', Editor.Redo);
    TypeAscii(Broker, 'a');
    AssertFormattingAt('Redo clears pending formatting', Editor.Text,
      1, 0, False, 0);

    Editor.Text := 'x';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.AdjustRelativeFontSize(1));
    AssertTrue(Editor.ApplyFontColor(True, clRed));
    AssertEquals('Left is consumed', 0, PressKey(Broker, VK_LEFT, []));
    TypeAscii(Broker, 'a');
    AssertFormattingAt('navigation clears pending formatting', Editor.Text,
      0, 0, False, 0);

    Editor.Text := 'x';
    DirectEditor.SetSelection(1, 0);
    AssertTrue(Editor.AdjustRelativeFontSize(1));
    AssertTrue(Editor.ApplyFontColor(True, clRed));
    Clipboard.Clear;
    Clipboard.AsText := 'a';
    AssertEquals('Ctrl+V is consumed', 0,
      PressKey(Broker, VK_V, [ssCtrl]));
    AssertFormattingAt('paste clears pending formatting', Editor.Text,
      1, 0, False, 0);
  finally
    Clipboard.Clear;
    Editor.Free;
  end;
end;

initialization
  RegisterTest(TTheWordPendingFormattingLifecycleTests);

end.
