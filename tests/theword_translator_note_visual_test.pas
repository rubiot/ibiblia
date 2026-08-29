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
    procedure ChapterPopupReusesPreviewForRomansElevenSevenNote;
    procedure BidiRunsRenderWithIsolatesWithoutChangingLogicalText;
    procedure InitialFormattingIsSafeBeforeBodyReceivesFocus;
    procedure EmptyVisualBodyTypingKeepsFirstCharacterOrder;
    procedure PlainQuotedHyphenatedNoteSerializesVisually;
    procedure NamedTheWordColorPreservesNoOpAndCanonicalizesRealEdit;
    procedure LogicalFontSizeSurvivesPhysicalDpiDrift;
    procedure ImplicitWhiteBackgroundIsOmittedButExplicitWhiteRoundTrips;
    procedure EditorBreakKeysSerializeMarkupWithoutPhysicalNewlines;
    procedure BodyOnlyEditorHidesMarkerAndAcceptsEmptyAndRawBreaks;
    procedure AdapterCreatesTypedNonTextMarkersWithSharedNumbering;
    procedure DirectEditorRoutesShortcutHoverAndClickForNoteMarker;
    procedure DirectEditorKeepsMixedMarkersAtomicAcrossParagraphDirections;
    procedure DirectEditorKeepsScriptTextEditableWrappedAndCaretLineSized;
    procedure DirectEditorReservesScriptInkAcrossConsecutiveLines;
    procedure DirectEditorPlacesTerminalSoftBreakCaretAtNextLineStart;
    procedure DirectEditorDoubleClickSelectsUnicodeWords;
  end;

implementation

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, LCLType, KGraphics,
  Types, KMemo, text_direction, verse_view_intf, verse_visual_document,
  directwrite_syntagm_view, directwrite_verse_editor,
  theword_ont_codec_v2, theword_verse_visual_adapter_v2,
  theword_wysiwyg_editor_control, theword_translator_note_editor, theword_translator_note_body_v2,
  theword_translator_note_preview, theword_translator_note_style, chapterview,
  performance_debug;

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
    procedure SimulateDoubleClick(X, Y: Integer);
    procedure SimulateHover(X, Y: Integer);
  end;

  TEditAccess = class(TEdit)
  public
    procedure SimulateKey(var Key: Word; Shift: TShiftState);
    procedure SimulateUTF8Key(var UTF8Key: TUTF8Char);
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

procedure TDirectWriteVerseEditorAccess.SimulateDoubleClick(X, Y: Integer);
begin
  Mouse.CursorPos := ClientToScreen(Point(X, Y));
  DblClick;
end;

procedure TDirectWriteVerseEditorAccess.SimulateHover(X, Y: Integer);
begin
  MouseMove([], X, Y);
end;

procedure TEditAccess.SimulateKey(var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

procedure TEditAccess.SimulateUTF8Key(var UTF8Key: TUTF8Char);
begin
  UTF8KeyPress(UTF8Key);
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

    View.SetNoteBody('<font color="red">Gn 1</font>');
    AssertEquals('theWord named colors remain visually editable',
      Ord(twnbcFullWysiwyg), Ord(View.Capability));
    AssertTrue('named-color text is rendered as a typed run',
      View.Blocks[0] is TTranslatorNoteRun);
    NoteRun := TTranslatorNoteRun(View.Blocks[0]);
    AssertEquals('named red is rendered in the shared preview',
      LongInt(ColorToRGB(clRed)),
      LongInt(ColorToRGB(NoteRun.TextStyle.Font.Color)));

    View.SetNoteBody('<span style="background:red">named bg</span>');
    AssertEquals('named backgrounds remain visually editable',
      Ord(twnbcFullWysiwyg), Ord(View.Capability));
    NoteRun := TTranslatorNoteRun(View.Blocks[0]);
    AssertTrue('named background is rendered',
      NoteRun.TextStyle.AllowBrush);
    AssertEquals('named red background uses theWord color semantics',
      LongInt(ColorToRGB(clRed)),
      LongInt(ColorToRGB(NoteRun.TextStyle.Brush.Color)));

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
  ChapterPopupReusesPreviewForRomansElevenSevenNote;
const
  PREVIOUS_BODY: UnicodeString =
    '<b>a</b><i>b</i><u>c</u><s>d</s><sup>e</sup>' +
    '<sub>f</sub><font color="red">g</font><b>h</b><i>i</i><u>j</u>';
  ROMANS_BODY: UnicodeString =
    '<font color="red">Rm 11:7 "eleição"</font> refere-se ao ' +
    'remanescente fiel, aos israelitas que creram.';
var
  Counters: TIBibliaPerfCounterValues;
  Hint: TNoteWindow;
  Memo: TKMemo;
begin
  Forms.Application.Initialize;
  Memo := TKMemo.Create(nil);
  Hint := TNoteWindow.Create(nil);
  IBibliaPerfSetEnabledForTests(True);
  try
    IBibliaPerfReset;
    Memo.TextStyle.Font.Name := 'Arial';
    Memo.TextStyle.Font.Size := 10;
    Hint.ActivateHint(UTF8Encode(PREVIOUS_BODY), Memo.TextStyle);
    Hint.Hide;
    Hint.ActivateHint(UTF8Encode(ROMANS_BODY), Memo.TextStyle);
    Hint.ActivateHint(UTF8Encode(ROMANS_BODY), Memo.TextStyle);
    Counters := IBibliaPerfSnapshot;
    AssertEquals('two distinct bodies render once each', 2,
      Int64(Counters[ipcNotePreviewRenders]));
    AssertTrue('chapter popup remains visible after reusing its note view',
      Hint.Visible);
    Hint.Hide;
  finally
    IBibliaPerfSetEnabledForTests(False);
    Hint.Free;
    Memo.Free;
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
  NamedTheWordColorPreservesNoOpAndCanonicalizesRealEdit;
var
  Body: UnicodeString;
  EditedBody, NoOpBody: UnicodeString;
  ErrorMessage: string;
  Perf: TIBibliaPerfCounterValues;
begin
  Forms.Application.Initialize;
  ErrorMessage := '';
  Body := '<font color="ReD">Gn 1:</font> rest';
  IBibliaPerfSetEnabledForTests(True);
  try
    AssertTrue(UTF8Encode(ErrorMessage),
      TryExerciseTranslatorNoteEditorNamedColor(Body, 3, 1, '2',
        NoOpBody, EditedBody, ErrorMessage));
    Perf := IBibliaPerfSnapshot;
    AssertEquals('opening the structured note decodes it once', 1,
      Perf[ipcNoteEditorDecodes]);
    AssertEquals('the no-op read is cached and the edit encodes once', 1,
      Perf[ipcNoteEditorEncodes]);
    AssertEquals('opening and saving without edits keeps the literal color',
      '<font color="ReD">Gn 1:</font> rest', UTF8Encode(NoOpBody));
    AssertEquals('a real Visual edit serializes the named color canonically',
      '<font color="#FF0000">Gn 2:</font> rest', UTF8Encode(EditedBody));
  finally
    IBibliaPerfSetEnabledForTests(False);
  end;
end;

procedure TTheWordTranslatorNoteVisualTests.
  LogicalFontSizeSurvivesPhysicalDpiDrift;
var
  ErrorMessage: string;
  PlainText: UnicodeString;
  Probe: TTranslatorNoteDpiFontProbe;
begin
  Forms.Application.Initialize;
  PlainText :=
    'O verbo traduzido por "compreender" também significa ' +
    '"apropriar-se, tomar para si".';
  AssertTrue(ErrorMessage,
    TryExerciseTranslatorNoteEditorDpiFontDrift(PlainText, Probe,
      ErrorMessage));
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

  AssertEquals('body-only Raw Enter and Shift+Enter keep ONT semantics',
    'raw-only<CM>opaque<CI><p><CL>',
    UTF8Encode(Probe.BodyOnlyRawBody));
  AssertEquals('body-only Raw output contains no physical CR', 0,
    Pos(#13, Probe.BodyOnlyRawBody));
  AssertEquals('body-only Raw output contains no physical LF', 0,
    Pos(#10, Probe.BodyOnlyRawBody));
end;

procedure TTheWordTranslatorNoteVisualTests.
  BodyOnlyEditorHidesMarkerAndAcceptsEmptyAndRawBreaks;
var
  Body: UnicodeString;
  DeleteVisible, MarkerControlsVisible, VisualEditing: Boolean;
  ErrorMessage: string;
begin
  Forms.Application.Initialize;
  AssertTrue(ErrorMessage, TryExerciseTheWordBodyEditorMode(
    'editorial', Body, MarkerControlsVisible, DeleteVisible,
    VisualEditing, ErrorMessage));
  AssertFalse('body-only mode hides RF marker controls',
    MarkerControlsVisible);
  AssertFalse('body-only mode does not offer note deletion',
    DeleteVisible);
  AssertTrue('supported comments open in Visual mode', VisualEditing);
  AssertEquals('body-only keeps Enter and Shift+Enter parity with RF notes',
    'editorial<p><CL>', UTF8Encode(Body));

  AssertTrue('empty editorial comments can be saved',
    TryExerciseTheWordBodyEditorSave('', Body, VisualEditing,
      ErrorMessage));
  AssertEquals('empty save remains empty', '', UTF8Encode(Body));
  AssertTrue('empty supported body stays visual', VisualEditing);

  AssertTrue('unknown comments can still be saved in Raw mode',
    TryExerciseTheWordBodyEditorSave(
      'alpha<CM>opaque<CI>' + LineEnding + 'beta', Body,
      VisualEditing, ErrorMessage));
  AssertFalse('unknown comment markup falls back to Raw mode',
    VisualEditing);
  AssertEquals('Raw physical line breaks normalize before comment save',
    'alpha<CM>opaque<CI><p>beta', UTF8Encode(Body));
  AssertEquals('normalized Raw body contains no physical CR', 0,
    Pos(#13, Body));
  AssertEquals('normalized Raw body contains no physical LF', 0,
    Pos(#10, Body));
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
      AssertEquals('marker uses theWord orange',
        LongInt(TheWordTranslatorNoteMarkerColor),
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

procedure TTheWordTranslatorNoteVisualTests.
  DirectEditorKeepsMixedMarkersAtomicAcrossParagraphDirections;
var
  Anchor: TRect;
  Codec: ITheWordOntCodecV2;
  Editor: TDirectWriteVerseEditorAccess;
  Host: TForm;
  Index: Integer;
  MarkerId: QWord;
  MarkerSpan: TVisualSpan;
  Probe: TVisualEventProbe;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;

  function ClickMarkerInside(const ARect: TRect;
    AObjectId: QWord): Boolean;
  var
    Hit: TVerseHit;
    X: Integer;
    Y: Integer;
  begin
    Result := False;
    for Y := ARect.Top to ARect.Bottom - 1 do
      for X := ARect.Left to ARect.Right - 1 do
        if Editor.HitTest(Point(X, Y), Hit) and
           (Hit.Role = vsrTranslatorNote) and
           (Hit.ObjectId = AObjectId) then
        begin
          Editor.SimulateClick(X, Y);
          Exit(True);
        end;
  end;

begin
  Forms.Application.Initialize;
  Codec := TTheWordOntCodecV2.Create;
  Host := TForm.CreateNew(nil);
  Probe := TVisualEventProbe.Create;
  Source := nil;
  try
    Editor := TDirectWriteVerseEditorAccess.Create(Host);
    Editor.Parent := Host;
    Editor.Align := alClient;
    Editor.InteractionMode := vimEdit;
    Editor.OnVisualObjectClick := @Probe.Click;
    Host.SetBounds(50, 50, 640, 220);
    Host.Show;

    Source := Codec.DecodeVerse(
      UnicodeString('left <RF q="אב-A">note<Rf> right'));
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 93);
    MarkerId := Source.TranslatorNotes[0].Id;
    for Index := 0 to Visual.SpanCount - 1 do
      if Visual.Spans[Index].Role = vsrTranslatorNote then
      begin
        MarkerSpan := Visual.Spans[Index];
        Break;
      end;
    AssertEquals('LTR paragraph keeps its metadata direction',
      Ord(tdLeftToRight), Ord(MarkerSpan.Direction));
    AssertEquals('Hebrew-leading mixed q is retained literally', 'אב-A',
      UTF8Encode(Copy(Visual.Text, MarkerSpan.StartUTF16 + 1,
      MarkerSpan.LengthUTF16)));
    Editor.SetEditorDocument(Visual);
    Application.ProcessMessages;
    AssertTrue('Hebrew-leading marker geometry is available in LTR text',
      Editor.GetVisualObjectClientRect(vsrTranslatorNote, MarkerId, Anchor));
    AssertTrue('Hebrew-leading marker has positive width',
      Anchor.Right > Anchor.Left);
    AssertTrue('Hebrew-leading marker has a hit inside its geometry',
      ClickMarkerInside(Anchor, MarkerId));
    AssertEquals('Hebrew-leading marker remains one clickable object', 1,
      Probe.FClickCount);
    AssertTrue(Probe.FLastHit.ObjectId = MarkerId);
    Visual := nil;
    FreeAndNil(Source);

    Source := Codec.DecodeVerse(
      UnicodeString('אב <RF q="A-אב">note<Rf> גד'));
    Visual := BuildTheWordVerseVisualDocument(Source, tdRightToLeft, 94);
    MarkerId := Source.TranslatorNotes[0].Id;
    for Index := 0 to Visual.SpanCount - 1 do
      if Visual.Spans[Index].Role = vsrTranslatorNote then
      begin
        MarkerSpan := Visual.Spans[Index];
        Break;
      end;
    AssertEquals('RTL paragraph keeps its metadata direction',
      Ord(tdRightToLeft), Ord(MarkerSpan.Direction));
    AssertEquals('Latin-leading mixed q is retained literally', 'A-אב',
      UTF8Encode(Copy(Visual.Text, MarkerSpan.StartUTF16 + 1,
      MarkerSpan.LengthUTF16)));
    Editor.SetEditorDocument(Visual);
    Application.ProcessMessages;
    AssertTrue('Latin-leading marker geometry is available in RTL text',
      Editor.GetVisualObjectClientRect(vsrTranslatorNote, MarkerId, Anchor));
    AssertTrue('Latin-leading marker has positive width',
      Anchor.Right > Anchor.Left);
    AssertTrue('Latin-leading marker has a hit inside its geometry',
      ClickMarkerInside(Anchor, MarkerId));
    AssertEquals('Latin-leading marker remains one clickable object', 2,
      Probe.FClickCount);
    AssertTrue(Probe.FLastHit.ObjectId = MarkerId);
  finally
    Source.Free;
    Probe.Free;
    Host.Free;
  end;
end;

procedure TTheWordTranslatorNoteVisualTests.
  DirectEditorKeepsScriptTextEditableWrappedAndCaretLineSized;
const
  LONG_SCRIPT = 'long script words that must wrap naturally across lines';
var
  Broker: TEditAccess;
  CaretNormal: TRect;
  CaretSubEnd: TRect;
  CaretSubInside: TRect;
  CaretSubStart: TRect;
  CaretSupEnd: TRect;
  CaretSupInside: TRect;
  CaretSupStart: TRect;
  Composite: TTheWordWysiwygEditorControl;
  Editor: TDirectWriteVerseEditor;
  Host: TForm;
  HasWrappedLine: Boolean;
  Index: Integer;
  Key: Word;
  KeyText: TUTF8Char;
  Rects: TRectArray;
  Stops: TVerseTextOffsetArray;
  UnicodeScript: UnicodeString;
begin
  Forms.Application.Initialize;
  Host := TForm.CreateNew(nil);
  try
    Host.SetBounds(50, 50, 420, 220);
    Composite := TTheWordWysiwygEditorControl.Create(Host);
    Composite.Parent := Host;
    Composite.Align := alClient;
    Composite.Editable := True;
    Composite.Text := 'A<sup>bc</sup>D<sub>ef</sub>G';
    Host.Show;
    Application.ProcessMessages;

    Editor := nil;
    for Index := 0 to Composite.ControlCount - 1 do
      if Composite.Controls[Index] is TDirectWriteVerseEditor then
        Editor := TDirectWriteVerseEditor(Composite.Controls[Index]);
    AssertTrue('the composite owns its DirectWrite editor', Assigned(Editor));

    Broker := nil;
    for Index := 0 to Editor.ControlCount - 1 do
      if Editor.Controls[Index] is TEdit then
        Broker := TEditAccess(Editor.Controls[Index]);
    AssertTrue('the native input broker is present', Assigned(Broker));

    AssertTrue('script layout exposes caret stops', Editor.GetCaretStops(Stops));
    AssertEquals('every ASCII cluster remains editable', 8, Length(Stops));
    for Index := 0 to High(Stops) do
      AssertEquals('caret stop ' + IntToStr(Index), Index, Stops[Index]);

    AssertTrue('normal caret rectangle is available',
      Editor.GetTextPositionClientRect(0, CaretNormal));
    AssertTrue('superscript start caret rectangle is available',
      Editor.GetTextPositionClientRect(1, CaretSupStart));
    AssertTrue('superscript interior caret rectangle is available',
      Editor.GetTextPositionClientRect(2, CaretSupInside));
    AssertTrue('superscript end caret rectangle is available',
      Editor.GetTextPositionClientRect(3, CaretSupEnd));
    AssertTrue('subscript start caret rectangle is available',
      Editor.GetTextPositionClientRect(4, CaretSubStart));
    AssertTrue('subscript interior caret rectangle is available',
      Editor.GetTextPositionClientRect(5, CaretSubInside));
    AssertTrue('subscript end caret rectangle is available',
      Editor.GetTextPositionClientRect(6, CaretSubEnd));

    AssertTrue('superscript caret is shorter than normal text',
      CaretSupInside.Bottom - CaretSupInside.Top <
      CaretNormal.Bottom - CaretNormal.Top);
    AssertTrue('superscript caret is raised above normal text',
      CaretSupInside.Top <= CaretNormal.Top);
    AssertTrue('superscript caret ends above the normal caret',
      CaretSupInside.Bottom < CaretNormal.Bottom);
    AssertTrue('subscript caret is shorter than normal text',
      CaretSubInside.Bottom - CaretSubInside.Top <
      CaretNormal.Bottom - CaretNormal.Top);
    AssertTrue('subscript caret starts below the normal caret',
      CaretSubInside.Top > CaretNormal.Top);
    AssertTrue('subscript caret extends below the normal caret',
      CaretSubInside.Bottom > CaretNormal.Bottom);
    AssertTrue('superscript opening boundary uses normal caret top',
      Abs(CaretSupStart.Top - CaretNormal.Top) <= 1);
    AssertEquals('superscript opening boundary uses normal caret height',
      CaretNormal.Bottom - CaretNormal.Top,
      CaretSupStart.Bottom - CaretSupStart.Top);
    AssertTrue('superscript closing boundary uses normal caret top',
      Abs(CaretSupEnd.Top - CaretNormal.Top) <= 1);
    AssertEquals('superscript closing boundary uses normal caret height',
      CaretNormal.Bottom - CaretNormal.Top,
      CaretSupEnd.Bottom - CaretSupEnd.Top);
    AssertTrue('subscript opening boundary uses normal caret top',
      Abs(CaretSubStart.Top - CaretNormal.Top) <= 1);
    AssertEquals('subscript opening boundary uses normal caret height',
      CaretNormal.Bottom - CaretNormal.Top,
      CaretSubStart.Bottom - CaretSubStart.Top);
    AssertTrue('subscript closing boundary uses normal caret top',
      Abs(CaretSubEnd.Top - CaretNormal.Top) <= 1);
    AssertEquals('subscript closing boundary uses normal caret height',
      CaretNormal.Bottom - CaretNormal.Top,
      CaretSubEnd.Bottom - CaretSubEnd.Top);

    Editor.SetSelection(2, 0);
    AssertEquals('caret enters the middle of superscript text', 2,
      Editor.CaretOffset);
    AssertTrue('input broker follows raised superscript caret',
      Abs(Broker.Top - CaretSupInside.Top) <= 1);
    AssertEquals('input broker uses superscript caret height',
      CaretSupInside.Bottom - CaretSupInside.Top, Broker.Height);
    KeyText := 'X';
    Broker.SimulateUTF8Key(KeyText);
    AssertEquals('typing edits only the selected superscript position',
      'A<sup>bXc</sup>D<sub>ef</sub>G', Composite.Text);
    Editor.SetSelection(3, 0);
    Key := VK_BACK;
    Broker.SimulateKey(Key, []);
    AssertEquals('backspace removes one cluster inside superscript text',
      'A<sup>bc</sup>D<sub>ef</sub>G', Composite.Text);

    Composite.Text := 'A<sub>bc</sub>D';
    Application.ProcessMessages;
    Editor.SetSelection(2, 0);
    AssertEquals('caret enters the middle of subscript text', 2,
      Editor.CaretOffset);
    KeyText := 'X';
    Broker.SimulateUTF8Key(KeyText);
    AssertEquals('typing edits only the selected subscript position',
      'A<sub>bXc</sub>D', Composite.Text);

    UnicodeScript := UnicodeString(WideChar($D83D)) + WideChar($DE00) +
      'e' + WideChar($0301);
    Composite.Text := 'A<sup>' + UTF8Encode(UnicodeScript) + '</sup>Z';
    Application.ProcessMessages;
    AssertTrue('Unicode script layout exposes caret stops',
      Editor.GetCaretStops(Stops));
    AssertEquals('surrogate and combining clusters remain indivisible',
      5, Length(Stops));
    AssertEquals(0, Stops[0]);
    AssertEquals(1, Stops[1]);
    AssertEquals(3, Stops[2]);
    AssertEquals(5, Stops[3]);
    AssertEquals(6, Stops[4]);

    Host.ClientWidth := 190;
    Composite.Text := 'A<sup>' + LONG_SCRIPT + '</sup>Z';
    Application.ProcessMessages;
    AssertTrue('long script range exposes visual rectangles',
      Editor.GetTextRangeClientRects(1, Length(LONG_SCRIPT), Rects));
    HasWrappedLine := False;
    for Index := 1 to High(Rects) do
      if Rects[Index].Top <> Rects[0].Top then
        HasWrappedLine := True;
    AssertTrue('long script text wraps instead of becoming one atomic object',
      HasWrappedLine);
  finally
    Host.Free;
  end;
end;

procedure TTheWordTranslatorNoteVisualTests.
  DirectEditorReservesScriptInkAcrossConsecutiveLines;
var
  Builder: TVerseVisualDocumentBuilder;
  Editor: TDirectWriteVerseEditorAccess;
  FirstCaret: TRect;
  FirstLineHeight: Integer;
  FirstLineRect: TRect;
  Host: TForm;
  NormalCaret: TRect;
  NormalContentHeight: Integer;
  NormalLineHeight: Integer;
  NormalLineRect: TRect;
  NormalStyle: TVisualTextStyle;
  NormalVisual: IVerseVisualDocument;
  ScriptContentHeight: Integer;
  ScriptVisual: IVerseVisualDocument;
  SecondCaret: TRect;
  SecondLineRect: TRect;
  SubOnlyCaret: TRect;
  SubOnlyLineRect: TRect;
  SubOnlyVisual: IVerseVisualDocument;
  SubscriptStyle: TVisualTextStyle;
  SupOnlyCaret: TRect;
  SupOnlyLineRect: TRect;
  SupOnlyVisual: IVerseVisualDocument;
  SuperscriptStyle: TVisualTextStyle;
  ThirdCaret: TRect;
  ThirdLineRect: TRect;
begin
  Forms.Application.Initialize;
  NormalStyle := DefaultVisualTextStyle;
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    { Five/six UTF-16 units combine CJK fallback with Hebrew marks whose ink
      can extend beyond ordinary Latin ascent/descent. }
    Builder.AddText(0, '漢שָׁa', NormalStyle, True);
    Builder.AddRoleText(1, #10, NormalStyle, False, vsrTitleBreak,
      901, 'line-break');
    Builder.AddText(2, '漢שָׁab', NormalStyle, True);
    Builder.AddRoleText(3, #10, NormalStyle, False, vsrTitleBreak,
      902, 'line-break');
    Builder.AddText(4, '漢שָׁa', NormalStyle, True);
    NormalVisual := Builder.Build(91);
  finally
    Builder.Free;
  end;

  SuperscriptStyle := DefaultVisualTextStyle;
  Include(SuperscriptStyle.Flags, vtsfSuperscript);
  SubscriptStyle := DefaultVisualTextStyle;
  Include(SubscriptStyle.Flags, vtsfSubscript);
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddText(0, '漢שָׁa', SuperscriptStyle, True);
    SupOnlyVisual := Builder.Build(93);
  finally
    Builder.Free;
  end;
  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddText(0, '漢שָׁa', SubscriptStyle, True);
    SubOnlyVisual := Builder.Build(94);
  finally
    Builder.Free;
  end;

  Builder := TVerseVisualDocumentBuilder.Create(tdLeftToRight);
  try
    Builder.AddText(0, '漢שָׁa', SuperscriptStyle, True);
    Builder.AddRoleText(1, #10, NormalStyle, False, vsrTitleBreak,
      903, 'line-break');
    Builder.AddText(2, '漢שָׁab', SubscriptStyle, True);
    Builder.AddRoleText(3, #10, NormalStyle, False, vsrTitleBreak,
      904, 'line-break');
    Builder.AddText(4, '漢שָׁa', SuperscriptStyle, True);
    ScriptVisual := Builder.Build(92);
  finally
    Builder.Free;
  end;

  Host := TForm.CreateNew(nil);
  try
    Host.SetBounds(50, 50, 420, 260);
    Editor := TDirectWriteVerseEditorAccess.Create(Host);
    Editor.Parent := Host;
    Editor.Align := alClient;
    Editor.InteractionMode := vimEdit;
    Host.Show;

    Editor.SetEditorDocument(NormalVisual);
    Application.ProcessMessages;
    AssertTrue('normal first-line caret is available',
      Editor.GetTextPositionClientRect(0, NormalCaret));
    AssertTrue('normal first-line box is available',
      Editor.GetTextLineClientRect(0, NormalLineRect));
    NormalContentHeight := Editor.ContentHeight;
    NormalLineHeight := NormalLineRect.Bottom - NormalLineRect.Top;

    Editor.SetEditorDocument(ScriptVisual);
    Application.ProcessMessages;
    AssertTrue('script-only first-line caret is available',
      Editor.GetTextPositionClientRect(0, FirstCaret));
    AssertTrue('script-only middle-line caret is available',
      Editor.GetTextPositionClientRect(6, SecondCaret));
    AssertTrue('script-only last-line caret is available',
      Editor.GetTextPositionClientRect(13, ThirdCaret));
    AssertTrue('script-only first-line box is available',
      Editor.GetTextLineClientRect(0, FirstLineRect));
    AssertTrue('script-only middle-line box is available',
      Editor.GetTextLineClientRect(6, SecondLineRect));
    AssertTrue('script-only last-line box is available',
      Editor.GetTextLineClientRect(13, ThirdLineRect));
    ScriptContentHeight := Editor.ContentHeight;
    FirstLineHeight := FirstLineRect.Bottom - FirstLineRect.Top;

    AssertTrue('fallback-font script keeps at least the natural line height',
      FirstLineHeight >= NormalLineHeight);
    AssertTrue('script line reserves only its net overhang',
      FirstLineHeight <= NormalLineHeight + NormalLineHeight div 3 + 1);
    AssertEquals('all consecutive script lines share reserved height',
      FirstLineHeight, SecondLineRect.Bottom - SecondLineRect.Top);
    AssertEquals('last script line shares reserved height',
      FirstLineHeight, ThirdLineRect.Bottom - ThirdLineRect.Top);
    AssertTrue('first and middle script lines are consecutive',
      Abs((SecondLineRect.Top - FirstLineRect.Top) - FirstLineHeight) <= 1);
    AssertTrue('middle and last script lines are consecutive',
      Abs((ThirdLineRect.Top - SecondLineRect.Top) - FirstLineHeight) <= 1);
    AssertTrue('superscript caret is smaller than a normal caret',
      FirstCaret.Bottom - FirstCaret.Top <
      NormalCaret.Bottom - NormalCaret.Top);
    AssertTrue('subscript caret is smaller than a normal caret',
      SecondCaret.Bottom - SecondCaret.Top <
      NormalCaret.Bottom - NormalCaret.Top);
    AssertTrue('subscript caret sits lower than superscript within the line box',
      SecondCaret.Top - SecondLineRect.Top >
      FirstCaret.Top - FirstLineRect.Top);
    AssertTrue('script caret remains inside first line box',
      (FirstCaret.Top >= FirstLineRect.Top) and
      (FirstCaret.Bottom <= FirstLineRect.Bottom));
    AssertTrue('script caret remains inside middle line box',
      (SecondCaret.Top >= SecondLineRect.Top) and
      (SecondCaret.Bottom <= SecondLineRect.Bottom));
    AssertTrue('reserved lines do not shrink reported content height',
      ScriptContentHeight >= NormalContentHeight);
    AssertTrue('the final script line remains inside reported content',
      ThirdLineRect.Bottom + Editor.LayoutPadding <= ScriptContentHeight);

    Editor.SetEditorDocument(SupOnlyVisual);
    Application.ProcessMessages;
    AssertTrue('sup-only caret is available',
      Editor.GetTextPositionClientRect(2, SupOnlyCaret));
    AssertTrue('sup-only line box is available',
      Editor.GetTextLineClientRect(2, SupOnlyLineRect));
    AssertTrue('sup-only line retains a normal text strut',
      SupOnlyLineRect.Bottom - SupOnlyLineRect.Top >= NormalLineHeight);
    AssertTrue('sup-only caret stays inside its line',
      (SupOnlyCaret.Top >= SupOnlyLineRect.Top) and
      (SupOnlyCaret.Bottom <= SupOnlyLineRect.Bottom));

    Editor.SetEditorDocument(SubOnlyVisual);
    Application.ProcessMessages;
    AssertTrue('sub-only caret is available',
      Editor.GetTextPositionClientRect(2, SubOnlyCaret));
    AssertTrue('sub-only line box is available',
      Editor.GetTextLineClientRect(2, SubOnlyLineRect));
    AssertTrue('sub-only line retains a normal text strut',
      SubOnlyLineRect.Bottom - SubOnlyLineRect.Top >= NormalLineHeight);
    AssertTrue('sub-only caret stays inside its line',
      (SubOnlyCaret.Top >= SubOnlyLineRect.Top) and
      (SubOnlyCaret.Bottom <= SubOnlyLineRect.Bottom));
  finally
    Host.Free;
  end;
end;

procedure TTheWordTranslatorNoteVisualTests.
  DirectEditorPlacesTerminalSoftBreakCaretAtNextLineStart;
var
  Codec: ITheWordOntCodecV2;
  Editor: TDirectWriteVerseEditorAccess;
  Host: TForm;
  LineRect: TRect;
  PopulatedCaret: TRect;
  Source: TTheWordVerseDocument;
  TerminalCaret: TRect;
  Visual: IVerseVisualDocument;

  procedure CheckDirection(ADirection: TTextDirection;
    const ALabel: string);
  var
    ExpectedLeadingEdge: Integer;
    OffsetAfterBreak: Integer;
    PopulatedRaw: UnicodeString;
    TerminalRaw: UnicodeString;
  begin
    if ADirection = tdRightToLeft then
    begin
      TerminalRaw := 'אבג<CL>';
      PopulatedRaw := 'אבג<CL>ד';
    end
    else
    begin
      TerminalRaw := 'abc<CL>';
      PopulatedRaw := 'abc<CL>x';
    end;
    Source := Codec.DecodeVerse(TerminalRaw);
    Visual := BuildTheWordVerseVisualDocument(Source, ADirection, 121);
    Editor.SetEditorDocument(Visual);
    Application.ProcessMessages;
    OffsetAfterBreak := Length(Visual.Text);
    AssertTrue(ALabel + ' terminal soft-break caret is available',
      Editor.GetTextPositionClientRect(OffsetAfterBreak, TerminalCaret));
    AssertTrue(ALabel + ' empty line box is available',
      Editor.GetTextLineClientRect(OffsetAfterBreak, LineRect));
    if ADirection = tdRightToLeft then
      ExpectedLeadingEdge := LineRect.Right
    else
      ExpectedLeadingEdge := LineRect.Left;
    AssertTrue(ALabel + ' terminal caret uses the empty-line leading edge',
      Abs(TerminalCaret.Left - ExpectedLeadingEdge) <= 1);
    Visual := nil;
    FreeAndNil(Source);

    Source := Codec.DecodeVerse(PopulatedRaw);
    Visual := BuildTheWordVerseVisualDocument(Source, ADirection, 122);
    Editor.SetEditorDocument(Visual);
    Application.ProcessMessages;
    OffsetAfterBreak := Length(Visual.Text) - 1;
    AssertTrue(ALabel + ' populated-line caret is available',
      Editor.GetTextPositionClientRect(OffsetAfterBreak, PopulatedCaret));
    AssertTrue(ALabel + ' terminal and populated caret share the line start',
      Abs(TerminalCaret.Left - PopulatedCaret.Left) <= 1);
    AssertTrue(ALabel + ' terminal and populated caret share the line top',
      Abs(TerminalCaret.Top - PopulatedCaret.Top) <= 1);
    Visual := nil;
    FreeAndNil(Source);
  end;

begin
  Forms.Application.Initialize;
  Codec := TTheWordOntCodecV2.Create;
  Source := nil;
  Host := TForm.CreateNew(nil);
  try
    Host.SetBounds(50, 50, 420, 220);
    Editor := TDirectWriteVerseEditorAccess.Create(Host);
    Editor.Parent := Host;
    Editor.Align := alClient;
    Editor.InteractionMode := vimEdit;
    Host.Show;
    CheckDirection(tdLeftToRight, 'LTR');
    CheckDirection(tdRightToLeft, 'RTL');
  finally
    Visual := nil;
    Source.Free;
    Host.Free;
  end;
end;

procedure TTheWordTranslatorNoteVisualTests.
  DirectEditorDoubleClickSelectsUnicodeWords;
const
  RAW: UnicodeString =
    '<font color=green>sofrerão punição olá—mundo</font>';
var
  AlternateRaw: UnicodeString;
  Codec: ITheWordOntCodecV2;
  Editor: TDirectWriteVerseEditorAccess;
  Host: TForm;
  Rects: TRectArray;
  Source: TTheWordVerseDocument;
  Visual: IVerseVisualDocument;

  procedure DoubleClickAt(AOffset, AExpectedStart,
    AExpectedLength: Integer; const ALabel: string);
  var
    Rect: TRect;
  begin
    AssertTrue(ALabel + ' has text geometry',
      Editor.GetTextRangeClientRects(AOffset, 1, Rects));
    AssertTrue(ALabel + ' exposes a rectangle', Length(Rects) > 0);
    Rect := Rects[0];
    Editor.SimulateDoubleClick((Rect.Left + Rect.Right) div 2,
      (Rect.Top + Rect.Bottom) div 2);
    AssertEquals(ALabel + ' selection start', AExpectedStart,
      Editor.SelectionStart);
    AssertEquals(ALabel + ' selection length', AExpectedLength,
      Editor.SelectionLength);
  end;

begin
  Forms.Application.Initialize;
  Codec := TTheWordOntCodecV2.Create;
  Source := Codec.DecodeVerse(RAW);
  Host := TForm.CreateNew(nil);
  try
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 119);
    Editor := TDirectWriteVerseEditorAccess.Create(Host);
    Editor.Parent := Host;
    Editor.Align := alClient;
    Editor.InteractionMode := vimEdit;
    Host.SetBounds(50, 50, 640, 220);
    Host.Show;
    Editor.SetEditorDocument(Visual);
    Application.ProcessMessages;

    DoubleClickAt(2, 0, 8, 'precomposed sofrerão');
    DoubleClickAt(11, 9, 7, 'precomposed punição');
    DoubleClickAt(18, 17, 3, 'word before Unicode dash');
    DoubleClickAt(22, 21, 5, 'word after Unicode dash');

    Visual := nil;
    FreeAndNil(Source);
    AlternateRaw := '<font color=green>olá' + WideChar($00A0) +
      'mundo cafe' + WideChar($0301) + '</font>';
    Source := Codec.DecodeVerse(AlternateRaw);
    Visual := BuildTheWordVerseVisualDocument(Source, tdLeftToRight, 120);
    Editor.SetEditorDocument(Visual);
    Application.ProcessMessages;

    DoubleClickAt(1, 0, 3, 'word before nonbreaking space');
    DoubleClickAt(5, 4, 5, 'word after nonbreaking space');
    DoubleClickAt(11, 10, 5, 'decomposed accented word');
  finally
    Host.Free;
    Source.Free;
  end;
end;

initialization
  RegisterTest(TTheWordTranslatorNoteVisualTests);

end.
