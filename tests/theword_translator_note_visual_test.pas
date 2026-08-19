unit theword_translator_note_visual_test;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  fpcunit, testregistry;

type
  TTheWordTranslatorNoteVisualTests = class(TTestCase)
  published
    procedure ApplyGateKeepsDraftOnFailureAndRoutesSaveDeleteCancel;
    procedure ValidationRejectsEmptyMarkupUnsafeMarkerBreaksAndLinks;
    procedure VisualBodyRoundTripsUnicodeRelativeSizeAndBreaks;
    procedure SharedBodyPreviewRendersRichAndProtectedContent;
    procedure BidiRunsRenderWithIsolatesWithoutChangingLogicalText;
    procedure InitialFormattingIsSafeBeforeBodyReceivesFocus;
    procedure EmptyVisualBodyTypingKeepsFirstCharacterOrder;
    procedure PlainQuotedHyphenatedNoteSerializesVisually;
    procedure LogicalFontSizeSurvivesPhysicalDpiDrift;
    procedure ImplicitWhiteBackgroundIsOmittedButExplicitWhiteRoundTrips;
    procedure EditorBreakKeysSerializeMarkupWithoutPhysicalNewlines;
    procedure AdapterCreatesTypedNonTextMarkersWithSharedNumbering;
    procedure DirectEditorRoutesShortcutHoverAndClickForNoteMarker;
  end;

implementation

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, LCLType, KGraphics,
  Types, KMemo, text_direction, verse_view_intf, verse_visual_document,
  directwrite_syntagm_view, directwrite_verse_editor,
  theword_ont_codec_v2, theword_verse_visual_adapter_v2,
  theword_translator_note_editor, theword_translator_note_body_v2,
  theword_translator_note_preview;

type
  TApplyProbe = class
  private
    FCalls: Integer;
    FLastAction: TTranslatorNoteEditorAction;
    FLastBody: UnicodeString;
    FLastQ: UnicodeString;
    FNextError: string;
    FNextResult: Boolean;
  public
    function Apply(Sender: TObject; AAction: TTranslatorNoteEditorAction;
      const AQ, ABodyRaw: UnicodeString;
      out AErrorMessage: string): Boolean;
  end;

  TVisualEventProbe = class
  private
    FClickCount: Integer;
    FCommand: TVerseEditorCommand;
    FCommandCount: Integer;
    FHoverCount: Integer;
    FLastHit: TVerseHit;
  public
    procedure Command(Sender: TObject; ACommand: TVerseEditorCommand);
    procedure Click(Sender: TObject; const Hit: TVerseHit);
    procedure Hover(Sender: TObject; const Hit: TVerseHit);
  end;

  TDirectWriteVerseEditorAccess = class(TDirectWriteVerseEditor)
  public
    procedure SimulateClick(X, Y: Integer);
    procedure SimulateHover(X, Y: Integer);
  end;

  TEditAccess = class(TEdit)
  public
    procedure SimulateKey(var Key: Word; Shift: TShiftState);
  end;

function TApplyProbe.Apply(Sender: TObject;
  AAction: TTranslatorNoteEditorAction; const AQ,
  ABodyRaw: UnicodeString; out AErrorMessage: string): Boolean;
begin
  Inc(FCalls);
  FLastAction := AAction;
  FLastQ := AQ;
  FLastBody := ABodyRaw;
  AErrorMessage := FNextError;
  Result := FNextResult;
end;

procedure TVisualEventProbe.Command(Sender: TObject;
  ACommand: TVerseEditorCommand);
begin
  Inc(FCommandCount);
  FCommand := ACommand;
end;

procedure TVisualEventProbe.Click(Sender: TObject; const Hit: TVerseHit);
begin
  Inc(FClickCount);
  FLastHit := Hit;
end;

procedure TVisualEventProbe.Hover(Sender: TObject; const Hit: TVerseHit);
begin
  if Hit.Role = vsrTranslatorNote then
    Inc(FHoverCount);
  FLastHit := Hit;
end;

procedure TDirectWriteVerseEditorAccess.SimulateClick(X, Y: Integer);
begin
  MouseDown(mbLeft, [], X, Y);
end;

procedure TDirectWriteVerseEditorAccess.SimulateHover(X, Y: Integer);
begin
  MouseMove([], X, Y);
end;

procedure TEditAccess.SimulateKey(var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

procedure TTheWordTranslatorNoteVisualTests.
  ApplyGateKeepsDraftOnFailureAndRoutesSaveDeleteCancel;
var
  Body, Q: UnicodeString;
  ErrorMessage: string;
  Probe: TApplyProbe;
begin
  Probe := TApplyProbe.Create;
  try
    Q := '*';
    Body := UnicodeString('rascunho ação');
    Probe.FNextResult := False;
    Probe.FNextError := 'rejected atomically';
    AssertFalse(TryApplyTranslatorNoteEditorDraft(nil, @Probe.Apply,
      tneaSave, False, Q, Body, ErrorMessage));
    AssertEquals('failure calls apply exactly once', 1, Probe.FCalls);
    AssertEquals('failure is reported without changing q', '*',
      UTF8Encode(Q));
    AssertEquals('failure is reported without changing the body',
      'rascunho ação', UTF8Encode(Body));
    AssertEquals('domain error is retained', 'rejected atomically',
      ErrorMessage);

    Probe.FNextResult := True;
    Probe.FNextError := '';
    AssertTrue(TryApplyTranslatorNoteEditorDraft(nil, @Probe.Apply,
      tneaSave, False, Q, Body, ErrorMessage));
    AssertEquals(2, Probe.FCalls);
    AssertEquals(Ord(tneaSave), Ord(Probe.FLastAction));
    AssertEquals('*', UTF8Encode(Probe.FLastQ));
    AssertEquals('rascunho ação', UTF8Encode(Probe.FLastBody));

    AssertTrue(TryApplyTranslatorNoteEditorDraft(nil, @Probe.Apply,
      tneaDelete, True, '', '', ErrorMessage));
    AssertEquals(3, Probe.FCalls);
    AssertEquals(Ord(tneaDelete), Ord(Probe.FLastAction));

    AssertFalse(TryApplyTranslatorNoteEditorDraft(nil, @Probe.Apply,
      tneaCancel, True, '', '', ErrorMessage));
    AssertEquals('Cancel never reaches the outer session', 3,
      Probe.FCalls);
  finally
    Probe.Free;
  end;
end;

procedure TTheWordTranslatorNoteVisualTests.
  ValidationRejectsEmptyMarkupUnsafeMarkerBreaksAndLinks;
var
  ErrorMessage: string;
  Probe: TApplyProbe;
begin
  Probe := TApplyProbe.Create;
  try
    Probe.FNextResult := True;
    AssertFalse(TryApplyTranslatorNoteEditorDraft(nil, @Probe.Apply,
      tneaSave, True, '', '<b></b><p><CL>', ErrorMessage));
    AssertEquals('empty formatted body never reaches the session', 0,
      Probe.FCalls);
    AssertTrue(ErrorMessage <> '');

    AssertFalse(TryApplyTranslatorNoteEditorDraft(nil, @Probe.Apply,
      tneaSave, False, '"bad"', 'body', ErrorMessage));
    AssertEquals('unsafe q never reaches the session', 0, Probe.FCalls);

    AssertFalse(TryApplyTranslatorNoteEditorDraft(nil, @Probe.Apply,
      tneaSave, True, '', UnicodeString('line') + #13#10 + 'two',
      ErrorMessage));
    AssertEquals('physical newlines never reach the session', 0,
      Probe.FCalls);

    AssertTrue(IsAllowedTranslatorNoteHref(
      'https://example.test/?a=1&b=2'));
    AssertTrue(IsAllowedTranslatorNoteHref('http://example.test'));
    AssertTrue(IsAllowedTranslatorNoteHref('tw://bible.*?id=1'));
    AssertFalse(IsAllowedTranslatorNoteHref('ftp://example.test'));
    AssertFalse(IsAllowedTranslatorNoteHref('https://example.test/a b'));
    AssertFalse(IsAllowedTranslatorNoteHref('https://example.test/"x"'));
    AssertFalse(IsAllowedTranslatorNoteHref('https://example.test/<x>'));

    AssertTrue(IsTranslatorNoteVisualEditingEnabled(twlcFullWysiwyg));
    AssertFalse(IsTranslatorNoteVisualEditingEnabled(twlcProtectedRegions));
    AssertFalse(IsTranslatorNoteVisualEditingEnabled(twlcRawOnly));
  finally
    Probe.Free;
  end;
end;

procedure TTheWordTranslatorNoteVisualTests.
  VisualBodyRoundTripsUnicodeRelativeSizeAndBreaks;
var
  Body, Canonical: UnicodeString;
  ErrorMessage: string;
begin
  Forms.Application.Initialize;
  Body := UnicodeString('ação ') + UnicodeString(WideChar($03B1)) +
    UnicodeString(WideChar($0301)) + ' ' +
    UnicodeString(WideChar($05E9)) + UnicodeString(WideChar($05DC)) +
    UnicodeString(WideChar($05D5)) + UnicodeString(WideChar($05DD)) + ' ' +
    UnicodeString(WideChar($D83D)) + UnicodeString(WideChar($DE00)) +
    '<font size="+2">grande</font><p>fim<CL>linha';
  AssertTrue(ErrorMessage,
    TryRoundTripTranslatorNoteVisualBody(Body, Canonical, ErrorMessage));
  AssertTrue('Portuguese survives UTF-8 TKMemo conversion',
    Pos(UnicodeString('ação'), Canonical) > 0);
  AssertTrue('combining Greek survives TKMemo conversion',
    Pos(UnicodeString(WideChar($03B1)) + UnicodeString(WideChar($0301)),
      Canonical) > 0);
  AssertTrue('Hebrew survives TKMemo conversion',
    Pos(UnicodeString(WideChar($05E9)) + UnicodeString(WideChar($05DC)) +
      UnicodeString(WideChar($05D5)) + UnicodeString(WideChar($05DD)),
      Canonical) > 0);
  AssertTrue('emoji surrogate pair survives TKMemo conversion',
    Pos(UnicodeString(WideChar($D83D)) + UnicodeString(WideChar($DE00)),
      Canonical) > 0);
  AssertTrue('font size remains relative',
    Pos(UnicodeString('size="+2"'), Canonical) > 0);
  AssertTrue('paragraph survives', Pos(UnicodeString('<p>'), Canonical) > 0);
  AssertTrue('soft break survives', Pos(UnicodeString('<CL>'), Canonical) > 0);
end;

procedure TTheWordTranslatorNoteVisualTests.
  SharedBodyPreviewRendersRichAndProtectedContent;
var
  NoteRun: TTranslatorNoteRun;
  View: TTranslatorNoteBodyView;
begin
  Forms.Application.Initialize;
  View := TTranslatorNoteBodyView.Create(nil);
  try
    View.TextStyle.Font.Name := 'Arial';
    View.TextStyle.Font.Size := 10;
    View.SetNoteBody('<a href="tw://bible.*?id=1"><rtl>' +
      '<span style="background:#112233"><b>rich</b></span>' +
      '</rtl></a><CL>end');
    AssertEquals('shared view reports the codec capability',
      Ord(twnbcFullWysiwyg), Ord(View.Capability));
    AssertEquals('shared view retains the raw body for inspection',
      '<a href="tw://bible.*?id=1"><rtl>' +
      '<span style="background:#112233"><b>rich</b></span>' +
      '</rtl></a><CL>end', UTF8Encode(View.BodyRaw));
    AssertTrue('rich text is rendered as a typed run',
      View.Blocks[0] is TTranslatorNoteRun);
    NoteRun := TTranslatorNoteRun(View.Blocks[0]);
    AssertEquals('link metadata is available to both previews',
      'tw://bible.*?id=1', UTF8Encode(NoteRun.Href));
    AssertEquals('inline direction is retained', Ord(ndRtl),
      Ord(NoteRun.InlineDirection));
    AssertTrue('bold is rendered', fsBold in NoteRun.TextStyle.Font.Style);
    AssertTrue('background is rendered', NoteRun.TextStyle.AllowBrush);
    AssertEquals('background color is rendered',
      LongInt(ColorToRGB(RGBToColor($11, $22, $33))),
      LongInt(ColorToRGB(NoteRun.TextStyle.Brush.Color)));
    AssertTrue('soft break uses the shared non-text block',
      View.Blocks[1] is TTranslatorNoteSoftBreak);

    View.SetNoteBody('<FI>protected<Fi>');
    AssertEquals('protected markup stays visibly protected',
      Ord(twnbcProtectedRegions), Ord(View.Capability));
    AssertEquals('protected markup is not normalized',
      '<FI>protected<Fi>', UTF8Encode(View.BodyRaw));
    AssertTrue('opaque markup is rendered as a typed run',
      View.Blocks[0] is TTranslatorNoteRun);
    NoteRun := TTranslatorNoteRun(View.Blocks[0]);
    AssertEquals('opaque markup remains visible', '<FI>', NoteRun.Text);
    AssertEquals('opaque markup uses a monospaced face', 'Consolas',
      NoteRun.TextStyle.Font.Name);
  finally
    View.Free;
  end;
end;

procedure TTheWordTranslatorNoteVisualTests.
  BidiRunsRenderWithIsolatesWithoutChangingLogicalText;
var
  Body, Canonical, DisplayText, LogicalText: UnicodeString;
  ErrorMessage: string;
  NoteRun: TTranslatorNoteRun;
  View: TTranslatorNoteBodyView;
begin
  Forms.Application.Initialize;
  LogicalText := UnicodeString(WideChar($05E9)) +
    UnicodeString(WideChar($05DC)) + UnicodeString(WideChar($05D5)) +
    UnicodeString(WideChar($05DD)) + ' 123 ' +
    UnicodeString(WideChar($D83D)) + UnicodeString(WideChar($DE00));
  Body := '<rtlpar><rtl>' + LogicalText + '</rtl></rtlpar>';
  View := TTranslatorNoteBodyView.Create(nil);
  try
    View.SetNoteBody(Body);
    AssertTrue(View.Blocks[0] is TTranslatorNoteRun);
    NoteRun := TTranslatorNoteRun(View.Blocks[0]);
    AssertEquals('logical run excludes synthetic bidi controls',
      UTF8Encode(LogicalText), NoteRun.Text);
    DisplayText := UTF8Decode(NoteRun.DirectionalDisplayText(NoteRun.Text));
    AssertEquals('paragraph direction opens an RTL isolate',
      Ord(RTL_ISOLATE), Ord(DisplayText[1]));
    AssertEquals('inline direction opens a nested RTL isolate',
      Ord(RTL_ISOLATE), Ord(DisplayText[2]));
    AssertEquals('inline isolate is closed', Ord(POP_DIRECTIONAL_ISOLATE),
      Ord(DisplayText[Length(DisplayText) - 1]));
    AssertEquals('paragraph isolate is closed',
      Ord(POP_DIRECTIONAL_ISOLATE), Ord(DisplayText[Length(DisplayText)]));
    AssertTrue('Unicode text remains inside the visual isolates',
      Pos(LogicalText, DisplayText) > 0);
    AssertEquals('RTL paragraph is visibly right aligned', Ord(halRight),
      Ord(View.Blocks.DefaultParaStyle.HAlign));
    AssertEquals('raw RF body remains byte-for-byte untouched',
      UTF8Encode(Body), UTF8Encode(View.BodyRaw));
  finally
    View.Free;
  end;

  AssertTrue(ErrorMessage,
    TryRoundTripTranslatorNoteVisualBody(Body, Canonical, ErrorMessage));
  AssertEquals('visual controls never enter serialized ONT',
    UTF8Encode(Body), UTF8Encode(Canonical));

  View := TTranslatorNoteBodyView.Create(nil);
  try
    View.SetNoteBody('<rtlpar>a<CL>b</rtlpar><p>' +
      '<ltrpar>c</ltrpar>');
    AssertTrue(View.Blocks[1] is TTranslatorNoteSoftBreak);
    AssertEquals('soft break retains RTL paragraph alignment',
      Ord(halRight),
      Ord(TKMemoParagraph(View.Blocks[1]).ParaStyle.HAlign));
    AssertTrue(View.Blocks[3] is TKMemoParagraph);
    AssertEquals('completed RTL paragraph is right aligned',
      Ord(halRight),
      Ord(TKMemoParagraph(View.Blocks[3]).ParaStyle.HAlign));
    AssertEquals('following LTR paragraph restores left alignment',
      Ord(halLeft), Ord(View.Blocks.DefaultParaStyle.HAlign));
  finally
    View.Free;
  end;
end;

procedure TTheWordTranslatorNoteVisualTests.
  InitialFormattingIsSafeBeforeBodyReceivesFocus;
var
  ErrorMessage: string;
begin
  Forms.Application.Initialize;
  AssertTrue(ErrorMessage,
    TryExerciseTranslatorNoteEditorInitialFocus(ErrorMessage));
end;

procedure TTheWordTranslatorNoteVisualTests.
  EmptyVisualBodyTypingKeepsFirstCharacterOrder;
var
  Body: UnicodeString;
  CaretPosition: Integer;
  ErrorMessage: string;
begin
  Forms.Application.Initialize;
  AssertTrue(ErrorMessage, TryExerciseTranslatorNoteEditorTyping(
    'abcdef', Body, CaretPosition, ErrorMessage));
  AssertEquals('typing order', 'abcdef', UTF8Encode(Body));
  AssertEquals('caret follows the last typed character', 6, CaretPosition);
end;

procedure TTheWordTranslatorNoteVisualTests.
  PlainQuotedHyphenatedNoteSerializesVisually;
var
  Body, Canonical, Input: UnicodeString;
  Capability: TTheWordNoteBodyCapability;
  CaretPosition: Integer;
  ErrorMessage: string;
begin
  Forms.Application.Initialize;
  Input :=
    'O verbo traduzido por "compreender" também significa ' +
    '"apropriar-se, tomar para si".';
  AssertTrue(ErrorMessage, TryExerciseTranslatorNoteEditorTyping(
    Input, Body, CaretPosition, ErrorMessage));
  AssertEquals('ResolvedVisualBodyRaw preserves quoted punctuation',
    UTF8Encode(Input), UTF8Encode(Body));
  AssertTrue('the body codec canonicalizes the resolved visual body',
    TTheWordTranslatorNoteBodyCodec.TryCanonicalize(
      Body, Canonical, Capability));
  AssertEquals('the simple note remains fully WYSIWYG',
    Ord(twnbcFullWysiwyg), Ord(Capability));
  AssertEquals('canonicalization preserves the exact note',
    UTF8Encode(Input), UTF8Encode(Canonical));
end;

procedure TTheWordTranslatorNoteVisualTests.
  LogicalFontSizeSurvivesPhysicalDpiDrift;
var
  ErrorMessage: string;
  Probe: TTranslatorNoteDpiFontProbe;
begin
  Forms.Application.Initialize;
  AssertTrue(ErrorMessage,
    TryExerciseTranslatorNoteEditorDpiFontDrift(Probe, ErrorMessage));
  AssertEquals('physical one-point drift is not semantic font markup',
    'O verbo traduzido por "compreender" também significa ' +
    '"apropriar-se, tomar para si".', UTF8Encode(Probe.PlainBody));
  AssertEquals('positive relative size survives physical drift',
    '<font size="+2">grande</font>', UTF8Encode(Probe.PositiveBody));
  AssertEquals('negative relative size survives physical drift',
    '<font size="-1">menor</font>', UTF8Encode(Probe.NegativeBody));
  AssertEquals('font-size button writes logical relative size',
    '<font size="+1">botão</font>', UTF8Encode(Probe.ButtonBody));
  AssertEquals('font-size button affects only the selected text',
    'a<font size="+1">bcd</font>e', UTF8Encode(Probe.PartialBody));
  AssertEquals('moving the caret clears the pending logical size',
    'a<font size="+1">bcd</font>ez',
    UTF8Encode(Probe.MovedCaretBody));
  AssertEquals('collapsed toolbar size applies to following input',
    '<font size="+1">novo</font>', UTF8Encode(Probe.CaretBody));
  AssertEquals('typing at an existing relative run inherits logical size',
    '<font size="+1">herdadox</font>',
    UTF8Encode(Probe.InheritedCaretBody));
  AssertEquals('Ctrl+wheel zoom does not change logical relative size',
    '<font size="+1">zoom</font>', UTF8Encode(Probe.ZoomBody));
end;

procedure TTheWordTranslatorNoteVisualTests.
  ImplicitWhiteBackgroundIsOmittedButExplicitWhiteRoundTrips;
var
  Body, Canonical: UnicodeString;
  CaretPosition: Integer;
  ErrorMessage: string;
begin
  Forms.Application.Initialize;
  AssertTrue(ErrorMessage, TryExerciseTranslatorNoteEditorTyping(
    'plain', Body, CaretPosition, ErrorMessage));
  AssertEquals('default visual input has no background markup', 'plain',
    UTF8Encode(Body));
  AssertTrue(ErrorMessage, TryRoundTripTranslatorNoteVisualBody(
    '<span style="background:#FFFFFF">white</span>', Canonical,
    ErrorMessage));
  AssertEquals('an explicit white background remains explicit',
    '<span style="background:#FFFFFF">white</span>',
    UTF8Encode(Canonical));
end;

procedure TTheWordTranslatorNoteVisualTests.
  EditorBreakKeysSerializeMarkupWithoutPhysicalNewlines;
var
  Probe: TTranslatorNoteEditorBreakProbe;
  ErrorMessage: string;
begin
  Forms.Application.Initialize;
  AssertTrue(ErrorMessage,
    TryExerciseTranslatorNoteEditorBreakEditing(Probe, ErrorMessage));

  AssertEquals('Enter and Shift+Enter use ONT markup in Visual mode',
    'one<p>two<CL>three<p>four', UTF8Encode(Probe.VisualBody));
  AssertEquals('Visual body contains no physical CR', 0,
    Pos(#13, Probe.VisualBody));
  AssertEquals('Visual body contains no physical LF', 0,
    Pos(#10, Probe.VisualBody));

  AssertEquals('terminal Shift+Enter appends the soft break',
    'one<CL>two', UTF8Encode(Probe.TerminalSoftBreakBody));
  AssertEquals('Enter before the last character preserves following text',
    'one<p>twoX', UTF8Encode(Probe.BeforeLastCharacterBody));

  AssertEquals('Raw Enter and Shift+Enter use ONT markup',
    'raw<p><CL>', UTF8Encode(Probe.RawBody));
  AssertEquals('Raw body omits the TSynEdit terminal CR', 0,
    Pos(#13, Probe.RawBody));
  AssertEquals('Raw body omits the TSynEdit terminal LF', 0,
    Pos(#10, Probe.RawBody));

  AssertTrue('an intentional Raw line break remains visible to validation',
    (Pos(#13, Probe.RawInteriorNewlineBody) > 0) or
    (Pos(#10, Probe.RawInteriorNewlineBody) > 0));
  AssertTrue('text before the intentional Raw line break is retained',
    Pos(UnicodeString('alpha'), Probe.RawInteriorNewlineBody) = 1);
  AssertTrue('text after the intentional Raw line break is retained',
    Pos(UnicodeString('beta'), Probe.RawInteriorNewlineBody) > 1);
end;

procedure TTheWordTranslatorNoteVisualTests.
  AdapterCreatesTypedNonTextMarkersWithSharedNumbering;
var
  Codec: ITheWordOntCodecV2;
  DocumentOffset, Index, MarkerCount, MarkerLength: Integer;
  Source: TTheWordVerseDocument;
  Span: TVisualSpan;
  Visual: IVerseVisualDocument;
begin
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse(
    'A<RF>automatic<Rf>B<RF q="*">custom<Rf>C');
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight,
      73, 4);
    MarkerCount := 0;
    for Index := 0 to Visual.SpanCount - 1 do
    begin
      Span := Visual.Spans[Index];
      if Span.Role <> vsrTranslatorNote then Continue;
      Inc(MarkerCount);
      AssertFalse('marker is not editable verse text', Span.Interactive);
      AssertTrue('marker has a stable note identity', Span.ObjectId <> 0);
      AssertTrue('marker is italic', vtsfItalic in Span.Style.Flags);
      AssertTrue('marker is superscript',
        vtsfSuperscript in Span.Style.Flags);
      AssertEquals('marker is one point smaller', -1,
        Span.Style.FontSizeDelta);
      AssertEquals('marker is blue', LongInt(clBlue),
        Span.Style.ForegroundColor);
      if MarkerCount = 1 then
        AssertEquals('automatic numbering continues from chapter base',
          '5', UTF8Encode(Copy(Visual.Text, Span.StartUTF16 + 1,
          Span.LengthUTF16)))
      else
        AssertEquals('q is rendered literally without consuming a number',
          '*', UTF8Encode(Copy(Visual.Text, Span.StartUTF16 + 1,
          Span.LengthUTF16)));
      AssertTrue(TryTheWordLayoutRangeToDocument(Visual,
        Span.StartUTF16, Span.LengthUTF16, DocumentOffset, MarkerLength));
      AssertEquals('inline marker has no Bible-text width', 0,
        MarkerLength);
      AssertFalse('marker is excluded from text hit-testing',
        Visual.TryGetInteractiveSpanAt(Span.StartUTF16, Span));
    end;
    AssertEquals(2, MarkerCount);
  finally
    Source.Free;
  end;
end;

procedure TTheWordTranslatorNoteVisualTests.
  DirectEditorRoutesShortcutHoverAndClickForNoteMarker;
var
  Anchor: TRect;
  Broker: TEditAccess;
  Builder: TVerseVisualDocumentBuilder;
  Codec: ITheWordOntCodecV2;
  Editor: TDirectWriteVerseEditorAccess;
  Host: TForm;
  Index: Integer;
  Key: Word;
  MarkerId: QWord;
  Probe: TVisualEventProbe;
  Style: TVisualTextStyle;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;
begin
  Forms.Application.Initialize;
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse('A<RF>note<Rf>B');
  Host := TForm.CreateNew(nil);
  Probe := TVisualEventProbe.Create;
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 81);
    MarkerId := Source.TranslatorNotes[0].Id;
    Editor := TDirectWriteVerseEditorAccess.Create(Host);
    Editor.Parent := Host;
    Editor.Align := alClient;
    Editor.InteractionMode := vimEdit;
    Editor.OnCommand := @Probe.Command;
    Editor.OnVisualObjectHover := @Probe.Hover;
    Editor.OnVisualObjectClick := @Probe.Click;
    Host.SetBounds(50, 50, 640, 220);
    Host.Show;
    Editor.SetEditorDocument(Visual);
    Application.ProcessMessages;

    Broker := nil;
    for Index := 0 to Editor.ControlCount - 1 do
      if Editor.Controls[Index] is TEdit then
        Broker := TEditAccess(Editor.Controls[Index]);
    AssertTrue('native input broker is present', Assigned(Broker));
    Key := VK_N;
    Broker.SimulateKey(Key, [ssCtrl, ssShift]);
    AssertEquals('shortcut emits one command', 1, Probe.FCommandCount);
    AssertEquals(Ord(vecInsertTranslatorNote), Ord(Probe.FCommand));
    AssertEquals('shortcut is consumed', 0, Key);

    AssertTrue('marker geometry is available',
      Editor.GetVisualObjectClientRect(vsrTranslatorNote, MarkerId, Anchor));
    Editor.SimulateHover((Anchor.Left + Anchor.Right) div 2,
      (Anchor.Top + Anchor.Bottom) div 2);
    AssertEquals(1, Probe.FHoverCount);
    AssertEquals(Ord(vsrTranslatorNote), Ord(Probe.FLastHit.Role));
    AssertTrue(Probe.FLastHit.ObjectId = MarkerId);

    Editor.SimulateClick((Anchor.Left + Anchor.Right) div 2,
      (Anchor.Top + Anchor.Bottom) div 2);
    AssertEquals(1, Probe.FClickCount);
    AssertEquals(Ord(vsrTranslatorNote), Ord(Probe.FLastHit.Role));
    AssertTrue(Probe.FLastHit.ObjectId = MarkerId);

    Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
    try
      Style := DefaultVisualTextStyle;
      Builder.AddRoleText(0, 'T', Style, False, vsrTitleBreak, 999);
      Builder.AddText(1, 'A', Style, True);
      Visual := Builder.Build(82);
    finally
      Builder.Free;
    end;
    Editor.SetEditorDocument(Visual);
    Application.ProcessMessages;
    AssertTrue('title geometry remains available for layout',
      Editor.GetVisualObjectClientRect(vsrTitleBreak, 999, Anchor));
    Editor.SimulateHover((Anchor.Left + Anchor.Right) div 2,
      (Anchor.Top + Anchor.Bottom) div 2);
    Editor.SimulateClick((Anchor.Left + Anchor.Right) div 2,
      (Anchor.Top + Anchor.Bottom) div 2);
    AssertEquals('title is not routed as object hover', 1, Probe.FHoverCount);
    AssertEquals('title is not routed as object click', 1, Probe.FClickCount);
  finally
    Probe.Free;
    Host.Free;
    Source.Free;
  end;
end;

initialization
  RegisterTest(TTheWordTranslatorNoteVisualTests);

end.
